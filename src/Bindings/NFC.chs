{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Bindings.NFC 
  ( NFCTarget(..)
  , NFCModulation(..)
  , NFCModulationType(..)
  , NFCBaudRate(..)
  , NFCISO14443aInfo(..)
  , initialize
  , open
  , initiatorInit
  , initiatorSelectPassiveTarget
  , initiatorPollTarget
  ) where

import Control.Monad         ((>=>))
import Data.ByteString       (ByteString, packCStringLen)
import Data.Word             (Word8, Word16)
import Foreign.C.String      (withCStringLen)
import Foreign.C.Types       (CUChar(..))
import Foreign.ForeignPtr    (newForeignPtr)
import Foreign.Ptr           (Ptr, castPtr, nullPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, pokeArray)
import Foreign.Marshal.Utils (maybePeek, with)
import Foreign.Storable      (Storable(..))

data NFCModulation =
  NFCModulation { nfcModType     :: NFCModulationType
                , nfcModBaudRate :: NFCBaudRate
                }

data NFCISO14443aInfo =
  NFCISO14443aInfo { iso14443aAbtAtqa :: Word16
                   , iso14443aBtSak   :: Word8
                   , iso14443aAbtUid  :: ByteString
                   , iso14443aAbtAts  :: ByteString
                   }

-- Add additional protocols here
data NFCTarget = NFCTargetISO14443a NFCISO14443aInfo

outNfcCtx :: Ptr (Ptr ()) -> IO NFCContextPtr
outNfcCtx = peek >=> newForeignPtr nfc_exit

outNfcDev :: Ptr () -> IO (Maybe NFCDevicePtr)
outNfcDev = maybePeek $ newForeignPtr nfc_close

inMaybeStrLen :: Num a => Maybe String -> ((Ptr CUChar, a) -> IO b) -> IO b
inMaybeStrLen Nothing    f = f (nullPtr, 0)
inMaybeStrLen (Just str) f = withCStringLen str $ \(ptr, len) -> f (castPtr ptr, fromIntegral len)

inNFCMod :: Num a => [NFCModulation] -> ((Ptr NFCModulation, a) -> IO b) -> IO b
inNFCMod nfcMod f = do
  let len = length nfcMod

  allocaArray len $ \ptr -> do
    pokeArray ptr nfcMod
    f (ptr, fromIntegral len)

#include "nfc/nfc.h"
#include "nfc/nfc-types.h"

{#enum nfc_modulation_type as NFCModulationType {underscoreToCase} deriving (Eq,Show)#}

{#enum nfc_baud_rate as NFCBaudRate {underscoreToCase} deriving (Eq, Show)#}

{#pointer *nfc_context as NFCContextPtr foreign finalizer nfc_exit#}

{#pointer *nfc_device as NFCDevicePtr foreign finalizer nfc_close#}

{#pointer *nfc_modulation as NFCModulationPtr foreign -> NFCModulation#}

{#pointer *nfc_target as NFCTargetPtr foreign -> NFCTarget#}

{#fun nfc_init as initialize {alloca- `NFCContextPtr' outNfcCtx*} -> `()'#}

{#fun nfc_open as open {`NFCContextPtr', `String'} -> `Maybe NFCDevicePtr' outNfcDev*#}

{#fun nfc_initiator_init as initiatorInit {`NFCDevicePtr'} -> `Int'#}

{#fun nfc_initiator_select_passive_target as initiatorSelectPassiveTarget
{`NFCDevicePtr', with* %`NFCModulation', inMaybeStrLen* `Maybe String'&, alloca- `NFCTarget' peek*} -> `Int'#}

{#fun nfc_initiator_poll_target as initiatorPollTarget
{`NFCDevicePtr', inNFCMod* `[NFCModulation]'&, `Word8', `Word8', alloca- `NFCTarget' peek*} -> `Int'#}

#c

/* These functions are needed because the libnfc author used #pragma pack(1).
 * c2hs does not recognize this compiler directive.
 */
nfc_modulation *hs_nfc_get_nm(nfc_target *t) { return &t->nm; }

size_t hs_nfc_target_size() { return sizeof(nfc_target); }

uint8_t *hs_nfc_get_nai_abtAtqa(nfc_target *t)  { return t->nti.nai.abtAtqa;  }
uint8_t  hs_nfc_get_nai_btSak(nfc_target *t)    { return t->nti.nai.btSak;    }
size_t   hs_nfc_get_nai_szUidLen(nfc_target *t) { return t->nti.nai.szUidLen; }
uint8_t *hs_nfc_get_nai_abtUid(nfc_target *t)   { return t->nti.nai.abtUid;   }
size_t   hs_nfc_get_nai_szAtsLen(nfc_target *t) { return t->nti.nai.szAtsLen; }
uint8_t *hs_nfc_get_nai_abtAts(nfc_target *t)   { return t->nti.nai.abtAts;   }

#endc

decodeIso14443a :: Ptr NFCTarget -> IO NFCTarget
decodeIso14443a p = do
  abtAtqa        <- (peek . castPtr) =<< {#call hs_nfc_get_nai_abtAtqa#} p

  (CUChar btSak) <- {#call hs_nfc_get_nai_btSak#} p

  szUidLen       <- fromIntegral <$> {#call hs_nfc_get_nai_szUidLen#} p
  abtUidPtr      <- castPtr      <$> {#call hs_nfc_get_nai_abtUid#} p
  abtUid         <- packCStringLen (abtUidPtr, szUidLen)

  szAtsLen       <- fromIntegral <$> {#call hs_nfc_get_nai_szAtsLen#} p
  abtAtsPtr      <- castPtr      <$> {#call hs_nfc_get_nai_abtAts#} p
  abtAts         <- packCStringLen (abtAtsPtr, szAtsLen)

  return $ NFCTargetISO14443a $ NFCISO14443aInfo abtAtqa btSak abtUid abtAts

instance Storable NFCModulation where
  alignment _ = {#alignof nfc_modulation#}
  sizeOf    _ = {#sizeof nfc_modulation#}
  peek p      = do
    modType <- (toEnum . fromIntegral) <$> {#get nfc_modulation.nmt#} p
    baud    <- (toEnum . fromIntegral) <$> {#get nfc_modulation.nbr#} p
    return $ NFCModulation modType baud
  poke p (NFCModulation modType baud) = do
    {#set nfc_modulation.nmt#} p $ (fromIntegral . fromEnum) modType
    {#set nfc_modulation.nbr#} p $ (fromIntegral . fromEnum) baud

instance Storable NFCTarget where
  alignment _ = {#alignof nfc_target#}
  sizeOf    _ = fromIntegral {#call pure hs_nfc_target_size#}
  peek p      = do
    nfcMod <- peek =<< {#call hs_nfc_get_nm#} p
    case nfcModType nfcMod of
      NmtIso14443a -> decodeIso14443a p
      _            -> error "not implemented"
  poke        = error "not implemented"

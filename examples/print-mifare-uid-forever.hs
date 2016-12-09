module Main where

import Control.Monad          (forever, void)
import qualified Data.ByteString.Char8 as C8 (putStrLn)
import Data.ByteString.Base16 (encode)

import Bindings.NFC

main :: IO ()
main = do
  ctx <- initialize
  dev <- open ctx "pn532_uart:/dev/ttyUSB0"

  case dev of
    Nothing -> putStrLn "error opening device"
    Just d  -> do
      void $ initiatorInit d
      forever $ do
        let nfcMod = NFCModulation NmtIso14443a Nbr106 
        (_, NFCTargetISO14443a info) <- initiatorSelectPassiveTarget d nfcMod Nothing
        C8.putStrLn $ encode $ iso14443aAbtUid info

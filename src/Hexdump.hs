{-# LANGUAGE BangPatterns #-}

module Hexdump (
    hexDump
  , hexDumpHandle
  , hexDumpFile
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder
import           Data.Monoid
import           Data.Word
import           Data.Char
import           Control.Monad.RWS.Strict
import           System.IO (stdin, stdout, openFile, hClose, IOMode(..), Handle)

data DumpState = DumpState {
    previousBS :: !(Maybe ByteString)
  , repeated   :: !Bool
  , lineNumber :: {-# UNPACK #-} !Int
  , byteCount  :: {-# UNPACK #-} !Int
  }

data AppConfig = AppConfig

defaultConfig = AppConfig
defaultState  = DumpState Nothing False 0 0

-- not using Data.Char.isPrint as some of them ain't really
-- considered as printable in ASCII terminal.
isAPrint :: Word8 -> Bool
isAPrint x = x >= 0x20 && x <= 0x7e
{-# INLINE isAPrint #-}

dumpEntry :: ByteString -> RWST AppConfig Builder DumpState IO ()
dumpEntry bs = when ((not . B.null) bs) $ do
  (DumpState prev starPrinted ln cnt) <- get
  if (Just bs) == prev then do
    when (not starPrinted) (tell (string7 "*\n"))
    put (DumpState prev True (1+ln) (16+cnt))
    else do
    let !p1 = word32HexFixed ((fromIntegral ln) * 16) <> string7 "  "
        !p2 = snd . B.foldl acc1 (1, p1) $ bs
        !p3 = string7 " |" <> B.foldl acc2 mempty bs <> string7 "|\n"
        acc1 (!k, !w) !c
          | k /= 8 = (1+k, w <> word8HexFixed c <> char7 ' ')
          | k == 8 = (1+k, w <> word8HexFixed c <> string7 "  ")
        acc1 :: (Int, Builder) -> Word8 -> (Int, Builder)
        {-# INLINE acc1 #-}
        acc2 !w !c
          | isAPrint c = w <> word8 c
          | otherwise  = w <> char7 '.'
        {-# INLINE acc2 #-}
        !len = B.length bs
        !pp = if len < 8 then p2 <> string7 ( replicate ((16 - len)*3+1) ' ') else
               if len < 16 then p2 <> string7 (replicate ((16-len)*3) ' ') else p2
    put (DumpState (Just bs) False (1+ln) (cnt + (fromIntegral len)))
    tell $! (pp  <> p3)

dumpBS_ bs = when ( (not . B.null) bs ) $ do
  let (!ent, !bs') = B.splitAt 16 bs
  dumpEntry ent >> dumpBS_ bs'

dumpBSAll :: AppConfig -> DumpState -> [ByteString] -> IO ()
dumpBSAll cfg st@(DumpState _ _ _ cnt) [] = hPutBuilder stdout (word32HexFixed (fromIntegral cnt) <> string7 "  \n")
dumpBSAll cfg st@(DumpState prev repeated ln cnt) (s:ss) = do
  (!st', !w) <- execRWST (dumpBS_ s) cfg st
  hPutBuilder stdout w
  dumpBSAll cfg st' ss

hexDumpLBS cfg st = dumpBSAll cfg st . LBS.toChunks

hexDumpFile :: FilePath -> IO ()
hexDumpFile fp =  openFile fp ReadMode >>= hexDumpHandle

hexDumpHandle :: Handle -> IO ()
hexDumpHandle hdl = do
  contents <- LBS.hGetContents hdl
  hexDumpLBS defaultConfig defaultState contents
  return ()

hexDump = hexDumpHandle stdin

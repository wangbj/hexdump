{-# LANGUAGE BangPatterns #-}

module Hexdump (
    hexPrint
  , hexPrintHandle
  , hexPrintFile
  , hexDump
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder
import           Data.Monoid
import           Data.Word
import           Data.Char
import           Control.Monad.RWS.Strict
import           Control.Monad.Identity
import           System.IO (withFile, stdin, stdout, openFile, hClose, IOMode(..), Handle)

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

dump16B :: (Monad m) => ByteString -> RWST AppConfig Builder DumpState m ()
dump16B bs = when ((not . B.null) bs) $ do
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

dumpByteString bs = when ( (not . B.null) bs ) $ do
  let (!ent, !bs') = B.splitAt 16 bs
  dump16B ent >> dumpByteString bs'

dumpBSAll :: AppConfig -> DumpState -> [ByteString] -> IO ()
dumpBSAll cfg st@(DumpState _ _ _ cnt) [] = hPutBuilder stdout (word32HexFixed (fromIntegral cnt) <> char7 '\n')
dumpBSAll cfg st@(DumpState prev repeated ln cnt) (s:ss) = do
  (!st', !w) <- execRWST (dumpByteString s) cfg st
  hPutBuilder stdout w
  dumpBSAll cfg st' ss

dumpBSHelperWith_ :: (Monad m) => AppConfig -> DumpState -> (Builder -> m ()) -> [ByteString] -> m ()
dumpBSHelperWith_ cfg st@(DumpState _ _ _ cnt) f [] = f (word32HexFixed (fromIntegral cnt) <> char7 '\n')
dumpBSHelperWith_ cfg st@(DumpState prev repeated ln cnt) f (s:ss) = do
  (!st', !w) <- execRWST (dumpByteString s) cfg st
  f w >> dumpBSHelperWith_ cfg st' f ss

dumpBSHelperWith :: (Monad m) => AppConfig -> DumpState -> (Builder -> m a) -> [ByteString] -> m [a]
dumpBSHelperWith cfg st@(DumpState _ _ _ cnt) f [] = f (word32HexFixed (fromIntegral cnt) <> char7 '\n') >>= \x -> return [x]
dumpBSHelperWith cfg st@(DumpState prev repeated ln cnt) f (s:ss) = do
  (!st', !w) <- execRWST (dumpByteString s) cfg st
  liftM2 (:) (f w) (dumpBSHelperWith cfg st' f ss)

hexDump :: LBS.ByteString -> LBS.ByteString
hexDump = LBS.concat . runIdentity . dumpBSHelperWith defaultConfig defaultState (Identity .  toLazyByteString) . LBS.toChunks

-- convert lazy byte string <-> strict byte string then print the strict byte string
-- is about 50-80% faster than call ``hPutBuilder`` for each strict bytestring
-- even though conversion between lazy <-> strict bytestring is costly
hexPrintHandle :: Handle -> IO ()
hexPrintHandle hdl = LBS.hGetContents hdl >>= mapM_ B.putStr . LBS.toChunks . hexDump

hexPrintFile fp = withFile fp ReadMode hexPrintHandle
{-# INLINE hexPrintFile #-}

hexPrint = hexPrintHandle stdin
{-# INLINE hexPrint #-}

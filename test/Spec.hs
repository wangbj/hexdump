{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck hiding ( (.&.) )

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Control.Monad
import           Control.Applicative
import           Data.Word
import           Data.Bits

import           Hexdump

newtype LargeLBS = LargeLBS LBS.ByteString deriving (Show)

instance Arbitrary LargeLBS where
    arbitrary = fmap (LargeLBS . LBS.pack) . liftA2 take (choose (0, 64*1024)) $ (infiniteListOf arbitrary)

prop_sanity_lazy_bytestring_to_chunks_is_aligned :: LargeLBS -> Bool
prop_sanity_lazy_bytestring_to_chunks_is_aligned (LargeLBS lbs) = aligned (init (LBS.toChunks lbs))
  where aligned xs = all (== True) (map (\x -> ((B.length x) .&. 0xf) == 0) xs)

return []
runTests = $quickCheckAll

main :: IO ()
main = runTests >> return ()

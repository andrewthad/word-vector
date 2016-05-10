{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.ByteString.Coerce 
  ( downcast
  , upcast
  , upcast'
  , toVectorWord8
  , fromVectorWord8
  , CmpWord
  ) where

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.Vector.Storable     as V
import qualified Data.Vector.Storable.Internal as V
import           Data.Vector.Storable     (Vector)
import           Data.Word

import Foreign.Storable
import Foreign.ForeignPtr
import GHC.ForeignPtr (ForeignPtr(..))
import GHC.Prim
import GHC.Types

toVectorWord8 :: ByteString -> Vector Word8
toVectorWord8 (BS.PS fptr off len) = V.unsafeFromForeignPtr fptr off len

fromVectorWord8 :: Vector Word8 -> ByteString
fromVectorWord8 v = BS.PS fp s l
  where (fp, s, l) = V.unsafeToForeignPtr v

downcast :: (CmpWord a b ~ 'GT, Storable a, Storable b) => Vector a -> Vector b
downcast = V.unsafeCast

upcast :: (CmpWord a b ~ 'LT, Storable a, Storable b) => Vector a -> Vector b
upcast = V.unsafeCast 

upcast' :: forall a b. (CmpWord a b ~ 'LT, Storable a, Storable b) => Vector a -> (Vector b, Vector a)
upcast' v = 
  if leftoverElems == 0
    then (upcast v, V.empty)
    else (upcast v, V.drop takenElems v)
  where
  n = V.length v
  newToOldRatio = bSize `quot` aSize
  leftoverElems = n `rem` newToOldRatio
  takenElems = n - leftoverElems
  aSize = sizeOf (undefined :: a)
  bSize = sizeOf (undefined :: b)

type family CmpWord (a :: *) (b :: *) :: Ordering where
  CmpWord Word8  Word8  = 'EQ
  CmpWord Word8  Word16 = 'LT
  CmpWord Word8  Word32 = 'LT
  CmpWord Word8  Word64 = 'LT
  CmpWord Word16 Word8  = 'GT
  CmpWord Word16 Word16 = 'EQ
  CmpWord Word16 Word32 = 'LT
  CmpWord Word16 Word64 = 'LT
  CmpWord Word32 Word8  = 'GT
  CmpWord Word32 Word16 = 'GT
  CmpWord Word32 Word32 = 'EQ
  CmpWord Word32 Word64 = 'LT
  CmpWord Word64 Word8  = 'GT
  CmpWord Word64 Word16 = 'GT
  CmpWord Word64 Word32 = 'GT
  CmpWord Word64 Word64 = 'EQ

-- | This operation is often O(1), but it can also be O(n). It depends
--   on the alignment of the bytestring that you are copying from.
-- toVectorRemainder :: Storable a => BS.ByteString -> (V.Vector a, BS.ByteString)
-- toVectorRemainder bs = if (offRem == 0) && (isAddrAligned unliftedSize unliftedAddr)
--   then (vec,remainingBs)
--   else (
--   where
--   vec = V.unsafeFromForeignPtr (castForeignPtr fptr) (scale off) (scale len)
--   BS.PS fptr off len = bs
--   !(ForeignPtr unliftedAddr _) = fptr
--   scale = (`quot` size)
--   lenRem = len `rem` size
--   offRem = off `rem` size
--   size = sizeOfElem vec
--   !(I# unliftedSize) = size
--   remainingBs = BS.drop (len - lenRem) bs

-- isAddrAligned :: Int# -> Addr# -> Bool
-- isAddrAligned i a = case (remAddr# a i) of
--   0# -> True
--   _  -> False

-- enforceAlignment :: Int -> ByteString -> ByteString
-- enforceAlignment i bs@(BS.PS fptr off len) = 
--   if (off `rem` i == 0) && 
--     then bs
--     else BS.copy bs

-- | If a bytestring does not have enough 
-- bytes to fill out the new type, then this will drop bytes off 
-- of the end. For example, a 'ByteString' with 23 bytes would
-- become a @Vector Word64@ of length 2. The 7 bytes at the end
-- would be lost.
-- toVector :: (Storable a) => BS.ByteString -> V.Vector a
-- toVector bs = error "ah" 
-- 
-- toVector' :: (Storable a) => BS.ByteString -> Maybe (V.Vector a)
-- toVector' bs = error "ah"


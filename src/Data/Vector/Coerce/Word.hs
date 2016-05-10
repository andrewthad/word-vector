{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vector.Coerce.Word
  ( downcast
  , upcast
  , upcast'
  , CmpWord
  ) where

import qualified Data.Vector.Storable     as V
import           Data.Vector.Storable     (Vector)
import           Data.Word

import Foreign.Storable
import Foreign.ForeignPtr

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


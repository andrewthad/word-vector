{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Vector.Coerce.ByteString where

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.Vector.Storable     as V
import qualified Data.Vector.Storable.Internal as V
import           Data.Vector.Storable     (Vector)
import           Data.Word

fromByteString :: ByteString -> Vector Word8
fromByteString (BS.PS fptr off len) = V.unsafeFromForeignPtr fptr off len

toByteString :: Vector Word8 -> ByteString
toByteString v = BS.PS fp s l
  where (fp, s, l) = V.unsafeToForeignPtr v


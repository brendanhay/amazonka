{-# LANGUAGE OverloadedStrings #-}

-- Module      : Text.Hastache.Aeson
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               Berkeley Software Distribution License, v. 3.0.
--               You can obtain it at
--               http://http://opensource.org/licenses/BSD-3-Clause.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adjusts the Mustache spec to correct reflect Haskell's use
-- of maybe for 'falsey' values.
module Text.Hastache.Aeson (jsonContext) where

import           Data.Aeson
import           Data.Attoparsec.Number
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as BS
import           Data.HashMap.Strict    (foldlWithKey')
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Vector            as V
import           Text.Hastache

import System.IO.Unsafe

jsonContext :: Monad m => Value -> MuContext m
jsonContext = buildMapContext . buildMap "" Map.empty

buildMapContext :: Monad m => Map ByteString (MuType m) -> ByteString -> m (MuType m)
buildMapContext m a = return $ fromMaybe
    (if a == "." then maybe MuNothing id $ Map.lookup BS.empty m else MuNothing)
    (Map.lookup a m)

buildMap :: Monad m
         => String
         -> Map ByteString (MuType m)
         -> Value
         -> Map ByteString (MuType m)
buildMap name m (Object obj) = Map.insert (encodeStr name)
    (MuList [buildMapContext $ foldlWithKey' (foldObject "") Map.empty obj])
    (foldlWithKey' (foldObject name) m obj)
buildMap name m value = Map.insert (encodeStr name) muValue m
    where
        muValue = case value of
            Array arr        -> MuList . V.toList $ fmap jsonContext arr
            Number (D float) -> MuVariable $ toLByteString float
            Number (I int)   -> MuVariable $ toLByteString int
            String s         -> MuVariable s
            Bool b           -> MuVariable $ show b
            Null             -> MuNothing
            t                -> MuVariable $ unsafePerformIO (print t >> return (show t))

foldObject :: Monad m
           => String
           -> Map ByteString (MuType m)
           -> Text
           -> Value
           -> Map ByteString (MuType m)
foldObject name m k v = buildMap (buildName $ Text.unpack k) m v
  where
    buildName n
        | null name = n
        | otherwise = name ++ "." ++ n

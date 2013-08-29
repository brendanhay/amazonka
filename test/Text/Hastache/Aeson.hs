{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
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

-- | Adjusts the Mustache spec to correct reflect Haskell's use of maybe
-- for 'falsey' values and add {{n}} for getting the current list index.
module Text.Hastache.Aeson (render) where

import           Control.Applicative
import           Data.Aeson
import           Data.Attoparsec.Number
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char                  (toLower)
import           Data.HashMap.Strict        (foldlWithKey')
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Vector                as V
import           Text.Hastache

render :: ByteString -> Value -> IO ByteString
render tmpl val = LBS.toStrict
    <$> hastacheStr defaultConfig tmpl (valueContext val)

valueContext :: Monad m => Value -> MuContext m
valueContext = mapContext . buildMap "" Map.empty

arrayContext :: Monad m => (Integer, Value) -> MuContext m
arrayContext (idx, val) = mapContext
    . Map.insert "n" (MuVariable $ toLByteString idx)
    $ buildMap "" Map.empty val

mapContext :: Monad m => Map ByteString (MuType m) -> ByteString -> m (MuType m)
mapContext m a = return $ ctx
  where
    ctx = fromMaybe
        (if a == "." then maybe MuNothing id $ Map.lookup BS.empty m else MuNothing)
        (Map.lookup a m)

buildMap :: Monad m
         => String
         -> Map ByteString (MuType m)
         -> Value
         -> Map ByteString (MuType m)
buildMap name m (Object obj) = Map.insert (encodeStr name)
    (MuList [mapContext $ foldlWithKey' (foldObject "") Map.empty obj])
    (foldlWithKey' (foldObject name) m obj)
buildMap name m value = Map.insert (encodeStr name) muValue m
    where
        muValue = case value of
            Array arr        -> MuList . map arrayContext . zip [1..] $ V.toList arr
            Number (D float) -> MuVariable $ toLByteString float
            Number (I int)   -> MuVariable $ toLByteString int
            String s         -> MuVariable s
            Bool b           -> MuVariable . map toLower $ show b
            Null             -> MuNothing
            t                -> MuVariable $ show t

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

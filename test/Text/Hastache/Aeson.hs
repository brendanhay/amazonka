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

-- | Adjusts the Mustache spec to correct reflect Haskell's use
-- of maybe for 'falsey' values.
module Text.Hastache.Aeson (render) where

import           Control.Applicative
import           Control.Monad.State
import           Data.Aeson
import           Data.Attoparsec.Number
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.HashMap.Strict        (foldlWithKey')
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Vector                as V
import           Text.Hastache

render :: ByteString -> Value -> IO ByteString
render tmpl val = LBS.toStrict <$> evalStateT run 0
  where
    run :: StateT Integer IO LBS.ByteString
    run = hastacheStr defaultConfig tmpl (jsonContext val)

-- jsonContext :: Monad m => Value -> MuContext m
jsonContext = buildMapContext
    . Map.insert "ordinals" (MuLambdaM $ const ordinals)
    . Map.insert "n" (MuLambdaM $ const n)
    . buildMap "" Map.empty
  where
    ordinals :: MonadState Integer m => m String
    ordinals = do
        x <- get
        let y = x + 1
        put y
        return ""

    n :: MonadState Integer m => m Integer
    n = get

--buildMapContext :: Monad m => Map ByteString (MuType m) -> ByteString -> m (MuType m)
buildMapContext m a = return $ ctx
  where
    ctx = fromMaybe
        (if a == "." then maybe MuNothing id $ Map.lookup BS.empty m else MuNothing)
        (Map.lookup a m)

-- buildMap :: Monad m
--          => String
--          -> Map ByteString (MuType m)
--          -> Value
--          -> Map ByteString (MuType m)
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
            t                -> MuVariable $ show t

-- foldObject :: Monad m
--            => String
--            -> Map ByteString (MuType m)
--            -> Text
--            -> Value
--            -> Map ByteString (MuType m)
foldObject name m k v = buildMap (buildName $ Text.unpack k) m v
  where
    buildName n
        | null name = n
        | otherwise = name ++ "." ++ n

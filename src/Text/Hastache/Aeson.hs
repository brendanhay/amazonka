{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Text.Hastache.Aeson
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               Berkeley Software Distribution License, v. 3.0.
--               You can obtain it at
--               http://http://opensource.org/licenses/BSD-3-Clause.
-- Author      : Vladimir Kirillov <proger@hackndev.com>
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.Hastache.Aeson
    ( jsonContext
    ) where

import           Data.Aeson.Types
import           Data.Attoparsec.Number (Number(I, D))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as BS
import qualified Data.HashMap.Strict    as HM
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Text.Hastache

jsonContext :: Monad m => Value -> MuContext m
jsonContext = buildContext . buildMap "" Map.empty

--
-- Internal
--

buildMap :: Monad m
         => [Char]
         -> Map ByteString (MuType m)
         -> Value
         -> Map ByteString (MuType m)
buildMap name m val
    | Object obj <- val = insertObject obj
    | otherwise      = insertValue
  where
    insertObject obj = Map.insert
        (encodeStr name)
        (MuList [buildContext $ HM.foldlWithKey' (foldObject "") Map.empty obj])
        (HM.foldlWithKey' (foldObject name) m obj)

    foldObject name' m' k = buildMap (buildName name' $ T.unpack k) m'

    buildName old new
        | not (null old) = old ++ "." ++ new
        | otherwise    = new

    insertValue = flip (Map.insert $ encodeStr name) m $
        case val of
            Array arr        -> MuList . V.toList $ fmap jsonContext arr
            Number (D float) -> MuVariable float
            Number (I int)   -> MuVariable int
            String s         -> MuVariable s
            Bool b           -> MuBool b
            Null             -> MuNothing
            t                -> MuVariable $ show t


buildContext :: Monad m
             => Map ByteString (MuType m)
             -> ByteString
             -> m (MuType m)
buildContext m a = return $ fromMaybe
    (if a == "." then maybe MuNothing id $ Map.lookup BS.empty m else MuNothing)
    (Map.lookup a m)

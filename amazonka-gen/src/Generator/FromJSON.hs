{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Generator.FromJSON
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generator.FromJSON where

import           Control.Applicative
import           Control.Arrow
import           Control.Error
import           Control.Monad
import qualified Data.Aeson           as Aeson
import           Data.Aeson           hiding (String)
import           Data.Aeson.Types     hiding (String)
import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.Monoid
import           Data.Ord
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Unsafe     as Text
import           GHC.Generics
import           Generator.AST
import           Text.EDE.Filters

instance FromJSON [PathPart] where
    parseJSON = withObject "uri" $ \o -> path <$> o .: "uri"
       where
        path = filter (/= PConst "") . go . Text.takeWhile (/= '?')

        go x | Text.null s = [PConst l]
             | otherwise   = PConst l : PVar m : go (Text.unsafeTail t)
          where
            (m, t) = Text.span (/= '}') $ Text.unsafeTail s
            (l, s) = Text.span (/= '{') x

instance FromJSON [QueryPart] where
    parseJSON = withObject "uri" $ \o -> query <$> o .: "uri"
      where
        query = map (uncurry QueryPart) . go . Text.dropWhile (/= '?')

        go x | Text.null s
             , Text.null l = []
             | Text.null s = [(Text.tail l, Nothing)]
             | otherwise   = brk : go (Text.unsafeTail t)
          where
            (m, t) = Text.span (/= '}') $ Text.unsafeTail s
            (l, s) = Text.span (/= '{') x

            brk | '=' <- Text.last l = (Text.init $ Text.tail l, Just m)
                | otherwise          = (Text.tail l, Nothing)

fromField :: (Generic a, GFromJSON (Rep a))
          => (String -> String)
          -> Value
          -> Parser a
fromField f = genericParseJSON $ defaultOptions { fieldLabelModifier = f }

fromCtor :: (Generic a, GFromJSON (Rep a))
         => (String -> String)
         -> Value
         -> Parser a
fromCtor f = genericParseJSON $ defaultOptions
    { constructorTagModifier = f
    , allNullaryToStringTag  = True
    }

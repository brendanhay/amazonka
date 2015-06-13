{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Data.JSON
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.JSON
    (
    -- * FromJSON
      FromJSON (..)
    , parseJSONText
    , eitherDecode'

    -- ** Parser a
    , withObject
    , (.:)
    , (.:?)
    , (.!=)

    -- ** Either String a
    , (.:>)
    , (.?>)

    -- * ToJSON
    , ToJSON   (..)
    , toJSONText
    , object
    , (.=)
    ) where

import           Data.Aeson            (eitherDecode')
import           Data.Aeson.Types
import           Data.ByteString.Lazy  (ByteString)
import qualified Data.HashMap.Strict   as Map
import           Data.Text             (Text)
import           Network.AWS.Data.Text

parseJSONText :: FromText a => String -> Value -> Parser a
parseJSONText n = withText n (either fail return . fromText)

toJSONText :: ToText a => a -> Value
toJSONText = String . toText

eitherParseJSON :: FromJSON a => Object -> Either String a
eitherParseJSON = parseEither parseJSON . Object

-- parseJSON   :: Value -> Parser a
-- parseEither :: (a -> Parser b) -> a -> Either String b

(.:>) :: FromJSON a => Object -> Text -> Either String a
(.:>) o k =
    case Map.lookup k o of
        Nothing -> Left $ "key " ++ show k ++ " not present"
        Just v  -> parseEither parseJSON v

(.?>) :: FromJSON a => Object -> Text -> Either String (Maybe a)
(.?>) o k =
    case Map.lookup k o of
        Nothing -> Right Nothing
        Just v  -> parseEither parseJSON v

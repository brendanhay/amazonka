-- |
-- Module      : Network.AWS.Data.JSON
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Data.JSON
  ( -- * FromJSON
    FromJSON (..),
    FromJSONKey (..),
    parseJSONText,
    Aeson.eitherDecode,
    Aeson.eitherDecode',

    -- ** Parser a
    Aeson.withObject,
    (Aeson..:),
    (Aeson..:?),
    (Aeson..!=),

    -- ** Either String a
    eitherParseJSON,
    (.:>),
    (.?>),

    -- * ToJSON
    ToJSON (..),
    ToJSONKey (..),
    toJSONText,
    Aeson.Value (Object),
    Aeson.object,
    (Aeson..=),
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.HashMap.Strict as HashMap
import Network.AWS.Data.Text
import Network.AWS.Internal.Prelude

parseJSONText :: FromText a => String -> Aeson.Value -> Aeson.Types.Parser a
parseJSONText n = Aeson.withText n (either fail pure . fromText)

toJSONText :: ToText a => a -> Aeson.Value
toJSONText = Aeson.String . toText

eitherParseJSON :: FromJSON a => Aeson.Object -> Either String a
eitherParseJSON = Aeson.Types.parseEither Aeson.parseJSON . Aeson.Object

(.:>) :: FromJSON a => Aeson.Object -> Text -> Either String a
(.:>) o k =
  case HashMap.lookup k o of
    Nothing -> Left $ "key " ++ show k ++ " not present"
    Just v -> Aeson.Types.parseEither Aeson.parseJSON v

(.?>) :: FromJSON a => Aeson.Object -> Text -> Either String (Maybe a)
(.?>) o k =
  case HashMap.lookup k o of
    Nothing -> Right Nothing
    Just v -> Aeson.Types.parseEither Aeson.parseJSON v

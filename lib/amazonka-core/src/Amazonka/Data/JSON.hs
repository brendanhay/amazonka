{-# LANGUAGE CPP #-}

-- |
-- Module      : Amazonka.Data.JSON
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Data.JSON
  ( -- * FromJSON
    Aeson.FromJSON (..),
    Aeson.FromJSONKey (..),
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
    Aeson.ToJSON (..),
    Aeson.ToJSONKey (..),
    toJSONText,
    Aeson.Value (Object),
    Aeson.object,
    (Aeson..=),
  )
where

import Amazonka.Data.Text
import Amazonka.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (Key)
#else
import qualified Data.HashMap.Strict as KM
type Key = Text
#endif

parseJSONText :: FromText a => String -> Aeson.Value -> Aeson.Types.Parser a
parseJSONText n = Aeson.withText n (either fail pure . fromText)

toJSONText :: ToText a => a -> Aeson.Value
toJSONText = Aeson.String . toText

eitherParseJSON :: Aeson.FromJSON a => Aeson.Object -> Either String a
eitherParseJSON = Aeson.Types.parseEither Aeson.parseJSON . Aeson.Object

(.:>) :: Aeson.FromJSON a => Aeson.Object -> Key -> Either String a
(.:>) o k =
  case KM.lookup k o of
    Nothing -> Left $ "key " ++ show k ++ " not present"
    Just v -> Aeson.Types.parseEither Aeson.parseJSON v

(.?>) :: Aeson.FromJSON a => Aeson.Object -> Key -> Either String (Maybe a)
(.?>) o k =
  case KM.lookup k o of
    Nothing -> Right Nothing
    Just v -> Aeson.Types.parseEither Aeson.parseJSON v

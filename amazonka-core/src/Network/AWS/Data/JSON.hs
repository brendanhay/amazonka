-- |
-- Module      : Network.AWS.Data.JSON
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Data.JSON
  ( -- * Serialisation
    Aeson.ToJSONKey,
    Aeson.ToJSON (..),
    Aeson.Value (Object),
    Aeson.object,
    (Aeson..=),

    -- * Deserialisation
    Aeson.FromJSONKey,
    Aeson.FromJSON (..),
    Aeson.eitherDecode,
    Aeson.eitherDecode',
    eitherParseJSON,
    Aeson.withObject,
    withJSONText,
    (Aeson..:),
    (Aeson..:?),
    (Aeson..!=),
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Text as Text
import qualified Network.AWS.Data.Text as AWS.Text
import Network.AWS.Prelude

eitherParseJSON ::
  Aeson.FromJSON a =>
  Aeson.Object ->
  Either String a
eitherParseJSON =
  Aeson.Types.parseEither Aeson.parseJSON
    . Aeson.Object

withJSONText ::
  AWS.Text.FromText a =>
  String ->
  Aeson.Value ->
  Aeson.Types.Parser a
withJSONText name =
  Aeson.withText ("withJSONText." ++ name) $ \text ->
    either (fail . Text.unpack) pure (AWS.Text.parseText text)

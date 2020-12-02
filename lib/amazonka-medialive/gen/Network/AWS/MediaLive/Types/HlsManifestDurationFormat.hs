{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsManifestDurationFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsManifestDurationFormat where

import Network.AWS.Prelude

-- | Hls Manifest Duration Format
data HlsManifestDurationFormat
  = FloatingPoint
  | Integer
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText HlsManifestDurationFormat where
  parser =
    takeLowerText >>= \case
      "floating_point" -> pure FloatingPoint
      "integer" -> pure Integer
      e ->
        fromTextError $
          "Failure parsing HlsManifestDurationFormat from value: '" <> e
            <> "'. Accepted values: floating_point, integer"

instance ToText HlsManifestDurationFormat where
  toText = \case
    FloatingPoint -> "FLOATING_POINT"
    Integer -> "INTEGER"

instance Hashable HlsManifestDurationFormat

instance NFData HlsManifestDurationFormat

instance ToByteString HlsManifestDurationFormat

instance ToQuery HlsManifestDurationFormat

instance ToHeader HlsManifestDurationFormat

instance ToJSON HlsManifestDurationFormat where
  toJSON = toJSONText

instance FromJSON HlsManifestDurationFormat where
  parseJSON = parseJSONText "HlsManifestDurationFormat"

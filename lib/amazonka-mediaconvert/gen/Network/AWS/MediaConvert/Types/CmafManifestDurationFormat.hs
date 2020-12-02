{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafManifestDurationFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafManifestDurationFormat where

import Network.AWS.Prelude

-- | Indicates whether the output manifest should use floating point values for segment duration.
data CmafManifestDurationFormat
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

instance FromText CmafManifestDurationFormat where
  parser =
    takeLowerText >>= \case
      "floating_point" -> pure FloatingPoint
      "integer" -> pure Integer
      e ->
        fromTextError $
          "Failure parsing CmafManifestDurationFormat from value: '" <> e
            <> "'. Accepted values: floating_point, integer"

instance ToText CmafManifestDurationFormat where
  toText = \case
    FloatingPoint -> "FLOATING_POINT"
    Integer -> "INTEGER"

instance Hashable CmafManifestDurationFormat

instance NFData CmafManifestDurationFormat

instance ToByteString CmafManifestDurationFormat

instance ToQuery CmafManifestDurationFormat

instance ToHeader CmafManifestDurationFormat

instance ToJSON CmafManifestDurationFormat where
  toJSON = toJSONText

instance FromJSON CmafManifestDurationFormat where
  parseJSON = parseJSONText "CmafManifestDurationFormat"

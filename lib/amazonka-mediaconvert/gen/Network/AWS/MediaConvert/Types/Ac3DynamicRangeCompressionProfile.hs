{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Ac3DynamicRangeCompressionProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Ac3DynamicRangeCompressionProfile where

import Network.AWS.Prelude

-- | If set to FILM_STANDARD, adds dynamic range compression signaling to the output bitstream as defined in the Dolby Digital specification.
data Ac3DynamicRangeCompressionProfile
  = ADRCPFilmStandard
  | ADRCPNone
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

instance FromText Ac3DynamicRangeCompressionProfile where
  parser =
    takeLowerText >>= \case
      "film_standard" -> pure ADRCPFilmStandard
      "none" -> pure ADRCPNone
      e ->
        fromTextError $
          "Failure parsing Ac3DynamicRangeCompressionProfile from value: '" <> e
            <> "'. Accepted values: film_standard, none"

instance ToText Ac3DynamicRangeCompressionProfile where
  toText = \case
    ADRCPFilmStandard -> "FILM_STANDARD"
    ADRCPNone -> "NONE"

instance Hashable Ac3DynamicRangeCompressionProfile

instance NFData Ac3DynamicRangeCompressionProfile

instance ToByteString Ac3DynamicRangeCompressionProfile

instance ToQuery Ac3DynamicRangeCompressionProfile

instance ToHeader Ac3DynamicRangeCompressionProfile

instance ToJSON Ac3DynamicRangeCompressionProfile where
  toJSON = toJSONText

instance FromJSON Ac3DynamicRangeCompressionProfile where
  parseJSON = parseJSONText "Ac3DynamicRangeCompressionProfile"

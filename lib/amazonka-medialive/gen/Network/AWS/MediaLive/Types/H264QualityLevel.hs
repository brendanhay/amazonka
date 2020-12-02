{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264QualityLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264QualityLevel where

import Network.AWS.Prelude

-- | H264 Quality Level
data H264QualityLevel
  = EnhancedQuality
  | StandardQuality
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

instance FromText H264QualityLevel where
  parser =
    takeLowerText >>= \case
      "enhanced_quality" -> pure EnhancedQuality
      "standard_quality" -> pure StandardQuality
      e ->
        fromTextError $
          "Failure parsing H264QualityLevel from value: '" <> e
            <> "'. Accepted values: enhanced_quality, standard_quality"

instance ToText H264QualityLevel where
  toText = \case
    EnhancedQuality -> "ENHANCED_QUALITY"
    StandardQuality -> "STANDARD_QUALITY"

instance Hashable H264QualityLevel

instance NFData H264QualityLevel

instance ToByteString H264QualityLevel

instance ToQuery H264QualityLevel

instance ToHeader H264QualityLevel

instance ToJSON H264QualityLevel where
  toJSON = toJSONText

instance FromJSON H264QualityLevel where
  parseJSON = parseJSONText "H264QualityLevel"

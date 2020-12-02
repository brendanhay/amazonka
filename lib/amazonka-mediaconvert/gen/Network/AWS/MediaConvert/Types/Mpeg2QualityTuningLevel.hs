{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2QualityTuningLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2QualityTuningLevel where

import Network.AWS.Prelude

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
data Mpeg2QualityTuningLevel
  = MQTLMultiPass
  | MQTLSinglePass
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

instance FromText Mpeg2QualityTuningLevel where
  parser =
    takeLowerText >>= \case
      "multi_pass" -> pure MQTLMultiPass
      "single_pass" -> pure MQTLSinglePass
      e ->
        fromTextError $
          "Failure parsing Mpeg2QualityTuningLevel from value: '" <> e
            <> "'. Accepted values: multi_pass, single_pass"

instance ToText Mpeg2QualityTuningLevel where
  toText = \case
    MQTLMultiPass -> "MULTI_PASS"
    MQTLSinglePass -> "SINGLE_PASS"

instance Hashable Mpeg2QualityTuningLevel

instance NFData Mpeg2QualityTuningLevel

instance ToByteString Mpeg2QualityTuningLevel

instance ToQuery Mpeg2QualityTuningLevel

instance ToHeader Mpeg2QualityTuningLevel

instance ToJSON Mpeg2QualityTuningLevel where
  toJSON = toJSONText

instance FromJSON Mpeg2QualityTuningLevel where
  parseJSON = parseJSONText "Mpeg2QualityTuningLevel"

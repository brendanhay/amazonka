{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265QualityTuningLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265QualityTuningLevel where

import Network.AWS.Prelude

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
data H265QualityTuningLevel
  = MultiPassHq
  | SinglePass
  | SinglePassHq
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

instance FromText H265QualityTuningLevel where
  parser =
    takeLowerText >>= \case
      "multi_pass_hq" -> pure MultiPassHq
      "single_pass" -> pure SinglePass
      "single_pass_hq" -> pure SinglePassHq
      e ->
        fromTextError $
          "Failure parsing H265QualityTuningLevel from value: '" <> e
            <> "'. Accepted values: multi_pass_hq, single_pass, single_pass_hq"

instance ToText H265QualityTuningLevel where
  toText = \case
    MultiPassHq -> "MULTI_PASS_HQ"
    SinglePass -> "SINGLE_PASS"
    SinglePassHq -> "SINGLE_PASS_HQ"

instance Hashable H265QualityTuningLevel

instance NFData H265QualityTuningLevel

instance ToByteString H265QualityTuningLevel

instance ToQuery H265QualityTuningLevel

instance ToHeader H265QualityTuningLevel

instance ToJSON H265QualityTuningLevel where
  toJSON = toJSONText

instance FromJSON H265QualityTuningLevel where
  parseJSON = parseJSONText "H265QualityTuningLevel"

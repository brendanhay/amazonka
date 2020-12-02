{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264QualityTuningLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264QualityTuningLevel where

import Network.AWS.Prelude

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, single-pass encoding.
data H264QualityTuningLevel
  = HQTLMultiPassHq
  | HQTLSinglePass
  | HQTLSinglePassHq
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

instance FromText H264QualityTuningLevel where
  parser =
    takeLowerText >>= \case
      "multi_pass_hq" -> pure HQTLMultiPassHq
      "single_pass" -> pure HQTLSinglePass
      "single_pass_hq" -> pure HQTLSinglePassHq
      e ->
        fromTextError $
          "Failure parsing H264QualityTuningLevel from value: '" <> e
            <> "'. Accepted values: multi_pass_hq, single_pass, single_pass_hq"

instance ToText H264QualityTuningLevel where
  toText = \case
    HQTLMultiPassHq -> "MULTI_PASS_HQ"
    HQTLSinglePass -> "SINGLE_PASS"
    HQTLSinglePassHq -> "SINGLE_PASS_HQ"

instance Hashable H264QualityTuningLevel

instance NFData H264QualityTuningLevel

instance ToByteString H264QualityTuningLevel

instance ToQuery H264QualityTuningLevel

instance ToHeader H264QualityTuningLevel

instance ToJSON H264QualityTuningLevel where
  toJSON = toJSONText

instance FromJSON H264QualityTuningLevel where
  parseJSON = parseJSONText "H264QualityTuningLevel"

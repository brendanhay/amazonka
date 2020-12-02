{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264AdaptiveQuantization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264AdaptiveQuantization where

import Network.AWS.Prelude

-- | Keep the default value, Auto (AUTO), for this setting to have MediaConvert automatically apply the best types of quantization for your video content. When you want to apply your quantization settings manually, you must set H264AdaptiveQuantization to a value other than Auto (AUTO). Use this setting to specify the strength of any adaptive quantization filters that you enable. If you don't want MediaConvert to do any adaptive quantization in this transcode, set Adaptive quantization (H264AdaptiveQuantization) to Off (OFF). Related settings: The value that you choose here applies to the following settings: H264FlickerAdaptiveQuantization, H264SpatialAdaptiveQuantization, and H264TemporalAdaptiveQuantization.
data H264AdaptiveQuantization
  = H26Auto
  | H26High
  | H26Higher
  | H26Low
  | H26Max
  | H26Medium
  | H26Off
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

instance FromText H264AdaptiveQuantization where
  parser =
    takeLowerText >>= \case
      "auto" -> pure H26Auto
      "high" -> pure H26High
      "higher" -> pure H26Higher
      "low" -> pure H26Low
      "max" -> pure H26Max
      "medium" -> pure H26Medium
      "off" -> pure H26Off
      e ->
        fromTextError $
          "Failure parsing H264AdaptiveQuantization from value: '" <> e
            <> "'. Accepted values: auto, high, higher, low, max, medium, off"

instance ToText H264AdaptiveQuantization where
  toText = \case
    H26Auto -> "AUTO"
    H26High -> "HIGH"
    H26Higher -> "HIGHER"
    H26Low -> "LOW"
    H26Max -> "MAX"
    H26Medium -> "MEDIUM"
    H26Off -> "OFF"

instance Hashable H264AdaptiveQuantization

instance NFData H264AdaptiveQuantization

instance ToByteString H264AdaptiveQuantization

instance ToQuery H264AdaptiveQuantization

instance ToHeader H264AdaptiveQuantization

instance ToJSON H264AdaptiveQuantization where
  toJSON = toJSONText

instance FromJSON H264AdaptiveQuantization where
  parseJSON = parseJSONText "H264AdaptiveQuantization"

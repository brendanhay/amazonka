{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Av1AdaptiveQuantization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Av1AdaptiveQuantization where

import Network.AWS.Prelude

-- | Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to Spatial adaptive quantization (spatialAdaptiveQuantization).
data Av1AdaptiveQuantization
  = High
  | Higher
  | Low
  | Max
  | Medium
  | Off
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

instance FromText Av1AdaptiveQuantization where
  parser =
    takeLowerText >>= \case
      "high" -> pure High
      "higher" -> pure Higher
      "low" -> pure Low
      "max" -> pure Max
      "medium" -> pure Medium
      "off" -> pure Off
      e ->
        fromTextError $
          "Failure parsing Av1AdaptiveQuantization from value: '" <> e
            <> "'. Accepted values: high, higher, low, max, medium, off"

instance ToText Av1AdaptiveQuantization where
  toText = \case
    High -> "HIGH"
    Higher -> "HIGHER"
    Low -> "LOW"
    Max -> "MAX"
    Medium -> "MEDIUM"
    Off -> "OFF"

instance Hashable Av1AdaptiveQuantization

instance NFData Av1AdaptiveQuantization

instance ToByteString Av1AdaptiveQuantization

instance ToQuery Av1AdaptiveQuantization

instance ToHeader Av1AdaptiveQuantization

instance ToJSON Av1AdaptiveQuantization where
  toJSON = toJSONText

instance FromJSON Av1AdaptiveQuantization where
  parseJSON = parseJSONText "Av1AdaptiveQuantization"

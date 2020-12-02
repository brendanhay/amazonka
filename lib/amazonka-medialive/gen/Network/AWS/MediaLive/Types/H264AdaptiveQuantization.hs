{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264AdaptiveQuantization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264AdaptiveQuantization where

import Network.AWS.Prelude

-- | H264 Adaptive Quantization
data H264AdaptiveQuantization
  = HHigh
  | HHigher
  | HLow
  | HMax
  | HMedium
  | HOff
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
      "high" -> pure HHigh
      "higher" -> pure HHigher
      "low" -> pure HLow
      "max" -> pure HMax
      "medium" -> pure HMedium
      "off" -> pure HOff
      e ->
        fromTextError $
          "Failure parsing H264AdaptiveQuantization from value: '" <> e
            <> "'. Accepted values: high, higher, low, max, medium, off"

instance ToText H264AdaptiveQuantization where
  toText = \case
    HHigh -> "HIGH"
    HHigher -> "HIGHER"
    HLow -> "LOW"
    HMax -> "MAX"
    HMedium -> "MEDIUM"
    HOff -> "OFF"

instance Hashable H264AdaptiveQuantization

instance NFData H264AdaptiveQuantization

instance ToByteString H264AdaptiveQuantization

instance ToQuery H264AdaptiveQuantization

instance ToHeader H264AdaptiveQuantization

instance ToJSON H264AdaptiveQuantization where
  toJSON = toJSONText

instance FromJSON H264AdaptiveQuantization where
  parseJSON = parseJSONText "H264AdaptiveQuantization"

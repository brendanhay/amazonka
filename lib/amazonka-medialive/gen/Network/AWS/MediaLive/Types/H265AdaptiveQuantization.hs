{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265AdaptiveQuantization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265AdaptiveQuantization where

import Network.AWS.Prelude

-- | H265 Adaptive Quantization
data H265AdaptiveQuantization
  = HAQHigh
  | HAQHigher
  | HAQLow
  | HAQMax
  | HAQMedium
  | HAQOff
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

instance FromText H265AdaptiveQuantization where
  parser =
    takeLowerText >>= \case
      "high" -> pure HAQHigh
      "higher" -> pure HAQHigher
      "low" -> pure HAQLow
      "max" -> pure HAQMax
      "medium" -> pure HAQMedium
      "off" -> pure HAQOff
      e ->
        fromTextError $
          "Failure parsing H265AdaptiveQuantization from value: '" <> e
            <> "'. Accepted values: high, higher, low, max, medium, off"

instance ToText H265AdaptiveQuantization where
  toText = \case
    HAQHigh -> "HIGH"
    HAQHigher -> "HIGHER"
    HAQLow -> "LOW"
    HAQMax -> "MAX"
    HAQMedium -> "MEDIUM"
    HAQOff -> "OFF"

instance Hashable H265AdaptiveQuantization

instance NFData H265AdaptiveQuantization

instance ToByteString H265AdaptiveQuantization

instance ToQuery H265AdaptiveQuantization

instance ToHeader H265AdaptiveQuantization

instance ToJSON H265AdaptiveQuantization where
  toJSON = toJSONText

instance FromJSON H265AdaptiveQuantization where
  parseJSON = parseJSONText "H265AdaptiveQuantization"

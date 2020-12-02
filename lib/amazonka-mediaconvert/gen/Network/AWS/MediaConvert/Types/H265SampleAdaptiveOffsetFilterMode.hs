{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265SampleAdaptiveOffsetFilterMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265SampleAdaptiveOffsetFilterMode where

import Network.AWS.Prelude

-- | Specify Sample Adaptive Offset (SAO) filter strength.  Adaptive mode dynamically selects best strength based on content
data H265SampleAdaptiveOffsetFilterMode
  = HSAOFMAdaptive
  | HSAOFMDefault
  | HSAOFMOff
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

instance FromText H265SampleAdaptiveOffsetFilterMode where
  parser =
    takeLowerText >>= \case
      "adaptive" -> pure HSAOFMAdaptive
      "default" -> pure HSAOFMDefault
      "off" -> pure HSAOFMOff
      e ->
        fromTextError $
          "Failure parsing H265SampleAdaptiveOffsetFilterMode from value: '" <> e
            <> "'. Accepted values: adaptive, default, off"

instance ToText H265SampleAdaptiveOffsetFilterMode where
  toText = \case
    HSAOFMAdaptive -> "ADAPTIVE"
    HSAOFMDefault -> "DEFAULT"
    HSAOFMOff -> "OFF"

instance Hashable H265SampleAdaptiveOffsetFilterMode

instance NFData H265SampleAdaptiveOffsetFilterMode

instance ToByteString H265SampleAdaptiveOffsetFilterMode

instance ToQuery H265SampleAdaptiveOffsetFilterMode

instance ToHeader H265SampleAdaptiveOffsetFilterMode

instance ToJSON H265SampleAdaptiveOffsetFilterMode where
  toJSON = toJSONText

instance FromJSON H265SampleAdaptiveOffsetFilterMode where
  parseJSON = parseJSONText "H265SampleAdaptiveOffsetFilterMode"

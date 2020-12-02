{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265SceneChangeDetect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265SceneChangeDetect where

import Network.AWS.Prelude

-- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
data H265SceneChangeDetect
  = HSCDDisabled
  | HSCDEnabled
  | HSCDTransitionDetection
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

instance FromText H265SceneChangeDetect where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure HSCDDisabled
      "enabled" -> pure HSCDEnabled
      "transition_detection" -> pure HSCDTransitionDetection
      e ->
        fromTextError $
          "Failure parsing H265SceneChangeDetect from value: '" <> e
            <> "'. Accepted values: disabled, enabled, transition_detection"

instance ToText H265SceneChangeDetect where
  toText = \case
    HSCDDisabled -> "DISABLED"
    HSCDEnabled -> "ENABLED"
    HSCDTransitionDetection -> "TRANSITION_DETECTION"

instance Hashable H265SceneChangeDetect

instance NFData H265SceneChangeDetect

instance ToByteString H265SceneChangeDetect

instance ToQuery H265SceneChangeDetect

instance ToHeader H265SceneChangeDetect

instance ToJSON H265SceneChangeDetect where
  toJSON = toJSONText

instance FromJSON H265SceneChangeDetect where
  parseJSON = parseJSONText "H265SceneChangeDetect"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264SceneChangeDetect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264SceneChangeDetect where

import Network.AWS.Prelude

-- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
data H264SceneChangeDetect
  = HSCDSDisabled
  | HSCDSEnabled
  | HSCDSTransitionDetection
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

instance FromText H264SceneChangeDetect where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure HSCDSDisabled
      "enabled" -> pure HSCDSEnabled
      "transition_detection" -> pure HSCDSTransitionDetection
      e ->
        fromTextError $
          "Failure parsing H264SceneChangeDetect from value: '" <> e
            <> "'. Accepted values: disabled, enabled, transition_detection"

instance ToText H264SceneChangeDetect where
  toText = \case
    HSCDSDisabled -> "DISABLED"
    HSCDSEnabled -> "ENABLED"
    HSCDSTransitionDetection -> "TRANSITION_DETECTION"

instance Hashable H264SceneChangeDetect

instance NFData H264SceneChangeDetect

instance ToByteString H264SceneChangeDetect

instance ToQuery H264SceneChangeDetect

instance ToHeader H264SceneChangeDetect

instance ToJSON H264SceneChangeDetect where
  toJSON = toJSONText

instance FromJSON H264SceneChangeDetect where
  parseJSON = parseJSONText "H264SceneChangeDetect"

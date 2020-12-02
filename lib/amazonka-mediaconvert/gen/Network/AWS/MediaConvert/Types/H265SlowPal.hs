{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265SlowPal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265SlowPal where

import Network.AWS.Prelude

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
data H265SlowPal
  = HSPDisabled
  | HSPEnabled
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

instance FromText H265SlowPal where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure HSPDisabled
      "enabled" -> pure HSPEnabled
      e ->
        fromTextError $
          "Failure parsing H265SlowPal from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText H265SlowPal where
  toText = \case
    HSPDisabled -> "DISABLED"
    HSPEnabled -> "ENABLED"

instance Hashable H265SlowPal

instance NFData H265SlowPal

instance ToByteString H265SlowPal

instance ToQuery H265SlowPal

instance ToHeader H265SlowPal

instance ToJSON H265SlowPal where
  toJSON = toJSONText

instance FromJSON H265SlowPal where
  parseJSON = parseJSONText "H265SlowPal"

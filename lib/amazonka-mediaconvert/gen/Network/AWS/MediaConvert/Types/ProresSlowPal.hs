{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ProresSlowPal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ProresSlowPal where

import Network.AWS.Prelude

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
data ProresSlowPal
  = PSPDisabled
  | PSPEnabled
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

instance FromText ProresSlowPal where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure PSPDisabled
      "enabled" -> pure PSPEnabled
      e ->
        fromTextError $
          "Failure parsing ProresSlowPal from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText ProresSlowPal where
  toText = \case
    PSPDisabled -> "DISABLED"
    PSPEnabled -> "ENABLED"

instance Hashable ProresSlowPal

instance NFData ProresSlowPal

instance ToByteString ProresSlowPal

instance ToQuery ProresSlowPal

instance ToHeader ProresSlowPal

instance ToJSON ProresSlowPal where
  toJSON = toJSONText

instance FromJSON ProresSlowPal where
  parseJSON = parseJSONText "ProresSlowPal"

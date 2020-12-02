{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vc3SlowPal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vc3SlowPal where

import Network.AWS.Prelude

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output by relabeling the video frames and resampling your audio. Note that enabling this setting will slightly reduce the duration of your video. Related settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
data Vc3SlowPal
  = VSPDisabled
  | VSPEnabled
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

instance FromText Vc3SlowPal where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure VSPDisabled
      "enabled" -> pure VSPEnabled
      e ->
        fromTextError $
          "Failure parsing Vc3SlowPal from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText Vc3SlowPal where
  toText = \case
    VSPDisabled -> "DISABLED"
    VSPEnabled -> "ENABLED"

instance Hashable Vc3SlowPal

instance NFData Vc3SlowPal

instance ToByteString Vc3SlowPal

instance ToQuery Vc3SlowPal

instance ToHeader Vc3SlowPal

instance ToJSON Vc3SlowPal where
  toJSON = toJSONText

instance FromJSON Vc3SlowPal where
  parseJSON = parseJSONText "Vc3SlowPal"

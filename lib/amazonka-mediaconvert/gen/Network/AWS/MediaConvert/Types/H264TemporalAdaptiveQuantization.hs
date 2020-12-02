{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264TemporalAdaptiveQuantization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264TemporalAdaptiveQuantization where

import Network.AWS.Prelude

-- | Only use this setting when you change the default value, AUTO, for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264TemporalAdaptiveQuantization is Enabled (ENABLED). Keep this default value to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to set H264TemporalAdaptiveQuantization to Disabled (DISABLED). Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization). To manually enable or disable H264TemporalAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
data H264TemporalAdaptiveQuantization
  = H26Disabled
  | H26Enabled
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

instance FromText H264TemporalAdaptiveQuantization where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure H26Disabled
      "enabled" -> pure H26Enabled
      e ->
        fromTextError $
          "Failure parsing H264TemporalAdaptiveQuantization from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText H264TemporalAdaptiveQuantization where
  toText = \case
    H26Disabled -> "DISABLED"
    H26Enabled -> "ENABLED"

instance Hashable H264TemporalAdaptiveQuantization

instance NFData H264TemporalAdaptiveQuantization

instance ToByteString H264TemporalAdaptiveQuantization

instance ToQuery H264TemporalAdaptiveQuantization

instance ToHeader H264TemporalAdaptiveQuantization

instance ToJSON H264TemporalAdaptiveQuantization where
  toJSON = toJSONText

instance FromJSON H264TemporalAdaptiveQuantization where
  parseJSON = parseJSONText "H264TemporalAdaptiveQuantization"

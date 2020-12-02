{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2TemporalAdaptiveQuantization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2TemporalAdaptiveQuantization where

import Network.AWS.Prelude

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to disable this feature. Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization).
data Mpeg2TemporalAdaptiveQuantization
  = MTAQDisabled
  | MTAQEnabled
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

instance FromText Mpeg2TemporalAdaptiveQuantization where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure MTAQDisabled
      "enabled" -> pure MTAQEnabled
      e ->
        fromTextError $
          "Failure parsing Mpeg2TemporalAdaptiveQuantization from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText Mpeg2TemporalAdaptiveQuantization where
  toText = \case
    MTAQDisabled -> "DISABLED"
    MTAQEnabled -> "ENABLED"

instance Hashable Mpeg2TemporalAdaptiveQuantization

instance NFData Mpeg2TemporalAdaptiveQuantization

instance ToByteString Mpeg2TemporalAdaptiveQuantization

instance ToQuery Mpeg2TemporalAdaptiveQuantization

instance ToHeader Mpeg2TemporalAdaptiveQuantization

instance ToJSON Mpeg2TemporalAdaptiveQuantization where
  toJSON = toJSONText

instance FromJSON Mpeg2TemporalAdaptiveQuantization where
  parseJSON = parseJSONText "Mpeg2TemporalAdaptiveQuantization"

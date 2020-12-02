{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2SpatialAdaptiveQuantization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2SpatialAdaptiveQuantization where

import Network.AWS.Prelude

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on spatial variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas that can sustain more distortion with no noticeable visual degradation and uses more bits on areas where any small distortion will be noticeable. For example, complex textured blocks are encoded with fewer bits and smooth textured blocks are encoded with more bits. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen with a lot of complex texture, you might choose to disable this feature. Related setting: When you enable spatial adaptive quantization, set the value for Adaptive quantization (adaptiveQuantization) depending on your content. For homogeneous content, such as cartoons and video games, set it to Low. For content with a wider variety of textures, set it to High or Higher.
data Mpeg2SpatialAdaptiveQuantization
  = MSAQDisabled
  | MSAQEnabled
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

instance FromText Mpeg2SpatialAdaptiveQuantization where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure MSAQDisabled
      "enabled" -> pure MSAQEnabled
      e ->
        fromTextError $
          "Failure parsing Mpeg2SpatialAdaptiveQuantization from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText Mpeg2SpatialAdaptiveQuantization where
  toText = \case
    MSAQDisabled -> "DISABLED"
    MSAQEnabled -> "ENABLED"

instance Hashable Mpeg2SpatialAdaptiveQuantization

instance NFData Mpeg2SpatialAdaptiveQuantization

instance ToByteString Mpeg2SpatialAdaptiveQuantization

instance ToQuery Mpeg2SpatialAdaptiveQuantization

instance ToHeader Mpeg2SpatialAdaptiveQuantization

instance ToJSON Mpeg2SpatialAdaptiveQuantization where
  toJSON = toJSONText

instance FromJSON Mpeg2SpatialAdaptiveQuantization where
  parseJSON = parseJSONText "Mpeg2SpatialAdaptiveQuantization"

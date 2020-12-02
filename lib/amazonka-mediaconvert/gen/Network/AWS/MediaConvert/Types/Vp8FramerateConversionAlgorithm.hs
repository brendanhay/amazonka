{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vp8FramerateConversionAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vp8FramerateConversionAlgorithm where

import Network.AWS.Prelude

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
data Vp8FramerateConversionAlgorithm
  = VDuplicateDrop
  | VFrameformer
  | VInterpolate
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

instance FromText Vp8FramerateConversionAlgorithm where
  parser =
    takeLowerText >>= \case
      "duplicate_drop" -> pure VDuplicateDrop
      "frameformer" -> pure VFrameformer
      "interpolate" -> pure VInterpolate
      e ->
        fromTextError $
          "Failure parsing Vp8FramerateConversionAlgorithm from value: '" <> e
            <> "'. Accepted values: duplicate_drop, frameformer, interpolate"

instance ToText Vp8FramerateConversionAlgorithm where
  toText = \case
    VDuplicateDrop -> "DUPLICATE_DROP"
    VFrameformer -> "FRAMEFORMER"
    VInterpolate -> "INTERPOLATE"

instance Hashable Vp8FramerateConversionAlgorithm

instance NFData Vp8FramerateConversionAlgorithm

instance ToByteString Vp8FramerateConversionAlgorithm

instance ToQuery Vp8FramerateConversionAlgorithm

instance ToHeader Vp8FramerateConversionAlgorithm

instance ToJSON Vp8FramerateConversionAlgorithm where
  toJSON = toJSONText

instance FromJSON Vp8FramerateConversionAlgorithm where
  parseJSON = parseJSONText "Vp8FramerateConversionAlgorithm"

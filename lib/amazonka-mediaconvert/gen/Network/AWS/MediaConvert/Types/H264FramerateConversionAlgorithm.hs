{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264FramerateConversionAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264FramerateConversionAlgorithm where

import Network.AWS.Prelude

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
data H264FramerateConversionAlgorithm
  = HDuplicateDrop
  | HFrameformer
  | HInterpolate
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

instance FromText H264FramerateConversionAlgorithm where
  parser =
    takeLowerText >>= \case
      "duplicate_drop" -> pure HDuplicateDrop
      "frameformer" -> pure HFrameformer
      "interpolate" -> pure HInterpolate
      e ->
        fromTextError $
          "Failure parsing H264FramerateConversionAlgorithm from value: '" <> e
            <> "'. Accepted values: duplicate_drop, frameformer, interpolate"

instance ToText H264FramerateConversionAlgorithm where
  toText = \case
    HDuplicateDrop -> "DUPLICATE_DROP"
    HFrameformer -> "FRAMEFORMER"
    HInterpolate -> "INTERPOLATE"

instance Hashable H264FramerateConversionAlgorithm

instance NFData H264FramerateConversionAlgorithm

instance ToByteString H264FramerateConversionAlgorithm

instance ToQuery H264FramerateConversionAlgorithm

instance ToHeader H264FramerateConversionAlgorithm

instance ToJSON H264FramerateConversionAlgorithm where
  toJSON = toJSONText

instance FromJSON H264FramerateConversionAlgorithm where
  parseJSON = parseJSONText "H264FramerateConversionAlgorithm"

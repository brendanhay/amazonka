{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2FramerateConversionAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2FramerateConversionAlgorithm where

import Network.AWS.Prelude

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
data Mpeg2FramerateConversionAlgorithm
  = MFCADuplicateDrop
  | MFCAFrameformer
  | MFCAInterpolate
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

instance FromText Mpeg2FramerateConversionAlgorithm where
  parser =
    takeLowerText >>= \case
      "duplicate_drop" -> pure MFCADuplicateDrop
      "frameformer" -> pure MFCAFrameformer
      "interpolate" -> pure MFCAInterpolate
      e ->
        fromTextError $
          "Failure parsing Mpeg2FramerateConversionAlgorithm from value: '" <> e
            <> "'. Accepted values: duplicate_drop, frameformer, interpolate"

instance ToText Mpeg2FramerateConversionAlgorithm where
  toText = \case
    MFCADuplicateDrop -> "DUPLICATE_DROP"
    MFCAFrameformer -> "FRAMEFORMER"
    MFCAInterpolate -> "INTERPOLATE"

instance Hashable Mpeg2FramerateConversionAlgorithm

instance NFData Mpeg2FramerateConversionAlgorithm

instance ToByteString Mpeg2FramerateConversionAlgorithm

instance ToQuery Mpeg2FramerateConversionAlgorithm

instance ToHeader Mpeg2FramerateConversionAlgorithm

instance ToJSON Mpeg2FramerateConversionAlgorithm where
  toJSON = toJSONText

instance FromJSON Mpeg2FramerateConversionAlgorithm where
  parseJSON = parseJSONText "Mpeg2FramerateConversionAlgorithm"

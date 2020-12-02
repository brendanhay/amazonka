{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AvcIntraFramerateConversionAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AvcIntraFramerateConversionAlgorithm where

import Network.AWS.Prelude

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
data AvcIntraFramerateConversionAlgorithm
  = AIFCADuplicateDrop
  | AIFCAFrameformer
  | AIFCAInterpolate
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

instance FromText AvcIntraFramerateConversionAlgorithm where
  parser =
    takeLowerText >>= \case
      "duplicate_drop" -> pure AIFCADuplicateDrop
      "frameformer" -> pure AIFCAFrameformer
      "interpolate" -> pure AIFCAInterpolate
      e ->
        fromTextError $
          "Failure parsing AvcIntraFramerateConversionAlgorithm from value: '" <> e
            <> "'. Accepted values: duplicate_drop, frameformer, interpolate"

instance ToText AvcIntraFramerateConversionAlgorithm where
  toText = \case
    AIFCADuplicateDrop -> "DUPLICATE_DROP"
    AIFCAFrameformer -> "FRAMEFORMER"
    AIFCAInterpolate -> "INTERPOLATE"

instance Hashable AvcIntraFramerateConversionAlgorithm

instance NFData AvcIntraFramerateConversionAlgorithm

instance ToByteString AvcIntraFramerateConversionAlgorithm

instance ToQuery AvcIntraFramerateConversionAlgorithm

instance ToHeader AvcIntraFramerateConversionAlgorithm

instance ToJSON AvcIntraFramerateConversionAlgorithm where
  toJSON = toJSONText

instance FromJSON AvcIntraFramerateConversionAlgorithm where
  parseJSON = parseJSONText "AvcIntraFramerateConversionAlgorithm"

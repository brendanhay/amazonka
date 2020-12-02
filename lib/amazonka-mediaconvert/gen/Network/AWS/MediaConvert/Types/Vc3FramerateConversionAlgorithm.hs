{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vc3FramerateConversionAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vc3FramerateConversionAlgorithm where

import Network.AWS.Prelude

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
data Vc3FramerateConversionAlgorithm
  = Vc3DuplicateDrop
  | Vc3Frameformer
  | Vc3Interpolate
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

instance FromText Vc3FramerateConversionAlgorithm where
  parser =
    takeLowerText >>= \case
      "duplicate_drop" -> pure Vc3DuplicateDrop
      "frameformer" -> pure Vc3Frameformer
      "interpolate" -> pure Vc3Interpolate
      e ->
        fromTextError $
          "Failure parsing Vc3FramerateConversionAlgorithm from value: '" <> e
            <> "'. Accepted values: duplicate_drop, frameformer, interpolate"

instance ToText Vc3FramerateConversionAlgorithm where
  toText = \case
    Vc3DuplicateDrop -> "DUPLICATE_DROP"
    Vc3Frameformer -> "FRAMEFORMER"
    Vc3Interpolate -> "INTERPOLATE"

instance Hashable Vc3FramerateConversionAlgorithm

instance NFData Vc3FramerateConversionAlgorithm

instance ToByteString Vc3FramerateConversionAlgorithm

instance ToQuery Vc3FramerateConversionAlgorithm

instance ToHeader Vc3FramerateConversionAlgorithm

instance ToJSON Vc3FramerateConversionAlgorithm where
  toJSON = toJSONText

instance FromJSON Vc3FramerateConversionAlgorithm where
  parseJSON = parseJSONText "Vc3FramerateConversionAlgorithm"

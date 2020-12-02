{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ProresFramerateConversionAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ProresFramerateConversionAlgorithm where

import Network.AWS.Prelude

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
data ProresFramerateConversionAlgorithm
  = PFCADuplicateDrop
  | PFCAFrameformer
  | PFCAInterpolate
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

instance FromText ProresFramerateConversionAlgorithm where
  parser =
    takeLowerText >>= \case
      "duplicate_drop" -> pure PFCADuplicateDrop
      "frameformer" -> pure PFCAFrameformer
      "interpolate" -> pure PFCAInterpolate
      e ->
        fromTextError $
          "Failure parsing ProresFramerateConversionAlgorithm from value: '" <> e
            <> "'. Accepted values: duplicate_drop, frameformer, interpolate"

instance ToText ProresFramerateConversionAlgorithm where
  toText = \case
    PFCADuplicateDrop -> "DUPLICATE_DROP"
    PFCAFrameformer -> "FRAMEFORMER"
    PFCAInterpolate -> "INTERPOLATE"

instance Hashable ProresFramerateConversionAlgorithm

instance NFData ProresFramerateConversionAlgorithm

instance ToByteString ProresFramerateConversionAlgorithm

instance ToQuery ProresFramerateConversionAlgorithm

instance ToHeader ProresFramerateConversionAlgorithm

instance ToJSON ProresFramerateConversionAlgorithm where
  toJSON = toJSONText

instance FromJSON ProresFramerateConversionAlgorithm where
  parseJSON = parseJSONText "ProresFramerateConversionAlgorithm"

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ProresFramerateConversionAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ProresFramerateConversionAlgorithm
  ( ProresFramerateConversionAlgorithm
      ( ProresFramerateConversionAlgorithm',
        ProresFramerateConversionAlgorithmDuplicateDrop,
        ProresFramerateConversionAlgorithmInterpolate,
        ProresFramerateConversionAlgorithmFrameformer,
        fromProresFramerateConversionAlgorithm
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
newtype ProresFramerateConversionAlgorithm = ProresFramerateConversionAlgorithm'
  { fromProresFramerateConversionAlgorithm ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ProresFramerateConversionAlgorithmDuplicateDrop :: ProresFramerateConversionAlgorithm
pattern ProresFramerateConversionAlgorithmDuplicateDrop = ProresFramerateConversionAlgorithm' "DUPLICATE_DROP"

pattern ProresFramerateConversionAlgorithmInterpolate :: ProresFramerateConversionAlgorithm
pattern ProresFramerateConversionAlgorithmInterpolate = ProresFramerateConversionAlgorithm' "INTERPOLATE"

pattern ProresFramerateConversionAlgorithmFrameformer :: ProresFramerateConversionAlgorithm
pattern ProresFramerateConversionAlgorithmFrameformer = ProresFramerateConversionAlgorithm' "FRAMEFORMER"

{-# COMPLETE
  ProresFramerateConversionAlgorithmDuplicateDrop,
  ProresFramerateConversionAlgorithmInterpolate,
  ProresFramerateConversionAlgorithmFrameformer,
  ProresFramerateConversionAlgorithm'
  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264FramerateConversionAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264FramerateConversionAlgorithm
  ( H264FramerateConversionAlgorithm
      ( H264FramerateConversionAlgorithm',
        HDuplicateDrop,
        HFrameformer,
        HInterpolate
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Choose the method that you want MediaConvert to use when increasing or decreasing the frame rate. We recommend using drop duplicate (DUPLICATE_DROP) for numerically simple conversions, such as 60 fps to 30 fps. For numerically complex conversions, you can use interpolate (INTERPOLATE) to avoid stutter. This results in a smooth picture, but might introduce undesirable video artifacts. For complex frame rate conversions, especially if your source video has already been converted from its original cadence, use FrameFormer (FRAMEFORMER) to do motion-compensated interpolation. FrameFormer chooses the best conversion method frame by frame. Note that using FrameFormer increases the transcoding time and incurs a significant add-on cost.
newtype H264FramerateConversionAlgorithm = H264FramerateConversionAlgorithm' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern HDuplicateDrop :: H264FramerateConversionAlgorithm
pattern HDuplicateDrop = H264FramerateConversionAlgorithm' "DUPLICATE_DROP"

pattern HFrameformer :: H264FramerateConversionAlgorithm
pattern HFrameformer = H264FramerateConversionAlgorithm' "FRAMEFORMER"

pattern HInterpolate :: H264FramerateConversionAlgorithm
pattern HInterpolate = H264FramerateConversionAlgorithm' "INTERPOLATE"

{-# COMPLETE
  HDuplicateDrop,
  HFrameformer,
  HInterpolate,
  H264FramerateConversionAlgorithm'
  #-}

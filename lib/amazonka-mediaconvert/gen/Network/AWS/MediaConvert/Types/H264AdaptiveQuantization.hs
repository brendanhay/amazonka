{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264AdaptiveQuantization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264AdaptiveQuantization
  ( H264AdaptiveQuantization
    ( H264AdaptiveQuantization'
    , H264AdaptiveQuantizationOff
    , H264AdaptiveQuantizationAuto
    , H264AdaptiveQuantizationLow
    , H264AdaptiveQuantizationMedium
    , H264AdaptiveQuantizationHigh
    , H264AdaptiveQuantizationHigher
    , H264AdaptiveQuantizationMax
    , fromH264AdaptiveQuantization
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Keep the default value, Auto (AUTO), for this setting to have MediaConvert automatically apply the best types of quantization for your video content. When you want to apply your quantization settings manually, you must set H264AdaptiveQuantization to a value other than Auto (AUTO). Use this setting to specify the strength of any adaptive quantization filters that you enable. If you don't want MediaConvert to do any adaptive quantization in this transcode, set Adaptive quantization (H264AdaptiveQuantization) to Off (OFF). Related settings: The value that you choose here applies to the following settings: H264FlickerAdaptiveQuantization, H264SpatialAdaptiveQuantization, and H264TemporalAdaptiveQuantization.
newtype H264AdaptiveQuantization = H264AdaptiveQuantization'{fromH264AdaptiveQuantization
                                                             :: Core.Text}
                                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                     Core.Generic)
                                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                       Core.ToJSONKey, Core.FromJSONKey,
                                                       Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                       Core.FromXML, Core.ToText, Core.FromText,
                                                       Core.ToByteString, Core.ToQuery,
                                                       Core.ToHeader)

pattern H264AdaptiveQuantizationOff :: H264AdaptiveQuantization
pattern H264AdaptiveQuantizationOff = H264AdaptiveQuantization' "OFF"

pattern H264AdaptiveQuantizationAuto :: H264AdaptiveQuantization
pattern H264AdaptiveQuantizationAuto = H264AdaptiveQuantization' "AUTO"

pattern H264AdaptiveQuantizationLow :: H264AdaptiveQuantization
pattern H264AdaptiveQuantizationLow = H264AdaptiveQuantization' "LOW"

pattern H264AdaptiveQuantizationMedium :: H264AdaptiveQuantization
pattern H264AdaptiveQuantizationMedium = H264AdaptiveQuantization' "MEDIUM"

pattern H264AdaptiveQuantizationHigh :: H264AdaptiveQuantization
pattern H264AdaptiveQuantizationHigh = H264AdaptiveQuantization' "HIGH"

pattern H264AdaptiveQuantizationHigher :: H264AdaptiveQuantization
pattern H264AdaptiveQuantizationHigher = H264AdaptiveQuantization' "HIGHER"

pattern H264AdaptiveQuantizationMax :: H264AdaptiveQuantization
pattern H264AdaptiveQuantizationMax = H264AdaptiveQuantization' "MAX"

{-# COMPLETE 
  H264AdaptiveQuantizationOff,

  H264AdaptiveQuantizationAuto,

  H264AdaptiveQuantizationLow,

  H264AdaptiveQuantizationMedium,

  H264AdaptiveQuantizationHigh,

  H264AdaptiveQuantizationHigher,

  H264AdaptiveQuantizationMax,
  H264AdaptiveQuantization'
  #-}

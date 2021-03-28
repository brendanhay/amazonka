{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265AdaptiveQuantization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265AdaptiveQuantization
  ( H265AdaptiveQuantization
    ( H265AdaptiveQuantization'
    , H265AdaptiveQuantizationOff
    , H265AdaptiveQuantizationLow
    , H265AdaptiveQuantizationMedium
    , H265AdaptiveQuantizationHigh
    , H265AdaptiveQuantizationHigher
    , H265AdaptiveQuantizationMax
    , fromH265AdaptiveQuantization
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to the following settings: Flicker adaptive quantization (flickerAdaptiveQuantization), Spatial adaptive quantization (spatialAdaptiveQuantization), and Temporal adaptive quantization (temporalAdaptiveQuantization).
newtype H265AdaptiveQuantization = H265AdaptiveQuantization'{fromH265AdaptiveQuantization
                                                             :: Core.Text}
                                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                     Core.Generic)
                                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                       Core.ToJSONKey, Core.FromJSONKey,
                                                       Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                       Core.FromXML, Core.ToText, Core.FromText,
                                                       Core.ToByteString, Core.ToQuery,
                                                       Core.ToHeader)

pattern H265AdaptiveQuantizationOff :: H265AdaptiveQuantization
pattern H265AdaptiveQuantizationOff = H265AdaptiveQuantization' "OFF"

pattern H265AdaptiveQuantizationLow :: H265AdaptiveQuantization
pattern H265AdaptiveQuantizationLow = H265AdaptiveQuantization' "LOW"

pattern H265AdaptiveQuantizationMedium :: H265AdaptiveQuantization
pattern H265AdaptiveQuantizationMedium = H265AdaptiveQuantization' "MEDIUM"

pattern H265AdaptiveQuantizationHigh :: H265AdaptiveQuantization
pattern H265AdaptiveQuantizationHigh = H265AdaptiveQuantization' "HIGH"

pattern H265AdaptiveQuantizationHigher :: H265AdaptiveQuantization
pattern H265AdaptiveQuantizationHigher = H265AdaptiveQuantization' "HIGHER"

pattern H265AdaptiveQuantizationMax :: H265AdaptiveQuantization
pattern H265AdaptiveQuantizationMax = H265AdaptiveQuantization' "MAX"

{-# COMPLETE 
  H265AdaptiveQuantizationOff,

  H265AdaptiveQuantizationLow,

  H265AdaptiveQuantizationMedium,

  H265AdaptiveQuantizationHigh,

  H265AdaptiveQuantizationHigher,

  H265AdaptiveQuantizationMax,
  H265AdaptiveQuantization'
  #-}

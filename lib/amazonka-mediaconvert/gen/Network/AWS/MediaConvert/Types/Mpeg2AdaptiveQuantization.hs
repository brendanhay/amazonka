{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2AdaptiveQuantization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2AdaptiveQuantization
  ( Mpeg2AdaptiveQuantization
      ( Mpeg2AdaptiveQuantization',
        Mpeg2AdaptiveQuantizationOff,
        Mpeg2AdaptiveQuantizationLow,
        Mpeg2AdaptiveQuantizationMedium,
        Mpeg2AdaptiveQuantizationHigh,
        fromMpeg2AdaptiveQuantization
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to the following settings: Spatial adaptive quantization (spatialAdaptiveQuantization), and Temporal adaptive quantization (temporalAdaptiveQuantization).
newtype Mpeg2AdaptiveQuantization = Mpeg2AdaptiveQuantization'
  { fromMpeg2AdaptiveQuantization ::
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

pattern Mpeg2AdaptiveQuantizationOff :: Mpeg2AdaptiveQuantization
pattern Mpeg2AdaptiveQuantizationOff = Mpeg2AdaptiveQuantization' "OFF"

pattern Mpeg2AdaptiveQuantizationLow :: Mpeg2AdaptiveQuantization
pattern Mpeg2AdaptiveQuantizationLow = Mpeg2AdaptiveQuantization' "LOW"

pattern Mpeg2AdaptiveQuantizationMedium :: Mpeg2AdaptiveQuantization
pattern Mpeg2AdaptiveQuantizationMedium = Mpeg2AdaptiveQuantization' "MEDIUM"

pattern Mpeg2AdaptiveQuantizationHigh :: Mpeg2AdaptiveQuantization
pattern Mpeg2AdaptiveQuantizationHigh = Mpeg2AdaptiveQuantization' "HIGH"

{-# COMPLETE
  Mpeg2AdaptiveQuantizationOff,
  Mpeg2AdaptiveQuantizationLow,
  Mpeg2AdaptiveQuantizationMedium,
  Mpeg2AdaptiveQuantizationHigh,
  Mpeg2AdaptiveQuantization'
  #-}

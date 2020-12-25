{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Av1AdaptiveQuantization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Av1AdaptiveQuantization
  ( Av1AdaptiveQuantization
      ( Av1AdaptiveQuantization',
        Av1AdaptiveQuantizationOff,
        Av1AdaptiveQuantizationLow,
        Av1AdaptiveQuantizationMedium,
        Av1AdaptiveQuantizationHigh,
        Av1AdaptiveQuantizationHigher,
        Av1AdaptiveQuantizationMax,
        fromAv1AdaptiveQuantization
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to Spatial adaptive quantization (spatialAdaptiveQuantization).
newtype Av1AdaptiveQuantization = Av1AdaptiveQuantization'
  { fromAv1AdaptiveQuantization ::
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

pattern Av1AdaptiveQuantizationOff :: Av1AdaptiveQuantization
pattern Av1AdaptiveQuantizationOff = Av1AdaptiveQuantization' "OFF"

pattern Av1AdaptiveQuantizationLow :: Av1AdaptiveQuantization
pattern Av1AdaptiveQuantizationLow = Av1AdaptiveQuantization' "LOW"

pattern Av1AdaptiveQuantizationMedium :: Av1AdaptiveQuantization
pattern Av1AdaptiveQuantizationMedium = Av1AdaptiveQuantization' "MEDIUM"

pattern Av1AdaptiveQuantizationHigh :: Av1AdaptiveQuantization
pattern Av1AdaptiveQuantizationHigh = Av1AdaptiveQuantization' "HIGH"

pattern Av1AdaptiveQuantizationHigher :: Av1AdaptiveQuantization
pattern Av1AdaptiveQuantizationHigher = Av1AdaptiveQuantization' "HIGHER"

pattern Av1AdaptiveQuantizationMax :: Av1AdaptiveQuantization
pattern Av1AdaptiveQuantizationMax = Av1AdaptiveQuantization' "MAX"

{-# COMPLETE
  Av1AdaptiveQuantizationOff,
  Av1AdaptiveQuantizationLow,
  Av1AdaptiveQuantizationMedium,
  Av1AdaptiveQuantizationHigh,
  Av1AdaptiveQuantizationHigher,
  Av1AdaptiveQuantizationMax,
  Av1AdaptiveQuantization'
  #-}

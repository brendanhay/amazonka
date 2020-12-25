{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264AdaptiveQuantization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264AdaptiveQuantization
  ( H264AdaptiveQuantization
      ( H264AdaptiveQuantization',
        H264AdaptiveQuantizationHigh,
        H264AdaptiveQuantizationHigher,
        H264AdaptiveQuantizationLow,
        H264AdaptiveQuantizationMax,
        H264AdaptiveQuantizationMedium,
        H264AdaptiveQuantizationOff,
        fromH264AdaptiveQuantization
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | H264 Adaptive Quantization
newtype H264AdaptiveQuantization = H264AdaptiveQuantization'
  { fromH264AdaptiveQuantization ::
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

pattern H264AdaptiveQuantizationHigh :: H264AdaptiveQuantization
pattern H264AdaptiveQuantizationHigh = H264AdaptiveQuantization' "HIGH"

pattern H264AdaptiveQuantizationHigher :: H264AdaptiveQuantization
pattern H264AdaptiveQuantizationHigher = H264AdaptiveQuantization' "HIGHER"

pattern H264AdaptiveQuantizationLow :: H264AdaptiveQuantization
pattern H264AdaptiveQuantizationLow = H264AdaptiveQuantization' "LOW"

pattern H264AdaptiveQuantizationMax :: H264AdaptiveQuantization
pattern H264AdaptiveQuantizationMax = H264AdaptiveQuantization' "MAX"

pattern H264AdaptiveQuantizationMedium :: H264AdaptiveQuantization
pattern H264AdaptiveQuantizationMedium = H264AdaptiveQuantization' "MEDIUM"

pattern H264AdaptiveQuantizationOff :: H264AdaptiveQuantization
pattern H264AdaptiveQuantizationOff = H264AdaptiveQuantization' "OFF"

{-# COMPLETE
  H264AdaptiveQuantizationHigh,
  H264AdaptiveQuantizationHigher,
  H264AdaptiveQuantizationLow,
  H264AdaptiveQuantizationMax,
  H264AdaptiveQuantizationMedium,
  H264AdaptiveQuantizationOff,
  H264AdaptiveQuantization'
  #-}

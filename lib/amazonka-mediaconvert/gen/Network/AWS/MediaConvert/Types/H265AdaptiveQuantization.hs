{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265AdaptiveQuantization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265AdaptiveQuantization
  ( H265AdaptiveQuantization
      ( H265AdaptiveQuantization',
        HAQOff,
        HAQLow,
        HAQMedium,
        HAQHigh,
        HAQHigher,
        HAQMax
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to the following settings: Flicker adaptive quantization (flickerAdaptiveQuantization), Spatial adaptive quantization (spatialAdaptiveQuantization), and Temporal adaptive quantization (temporalAdaptiveQuantization).
newtype H265AdaptiveQuantization = H265AdaptiveQuantization' Lude.Text
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

pattern HAQOff :: H265AdaptiveQuantization
pattern HAQOff = H265AdaptiveQuantization' "OFF"

pattern HAQLow :: H265AdaptiveQuantization
pattern HAQLow = H265AdaptiveQuantization' "LOW"

pattern HAQMedium :: H265AdaptiveQuantization
pattern HAQMedium = H265AdaptiveQuantization' "MEDIUM"

pattern HAQHigh :: H265AdaptiveQuantization
pattern HAQHigh = H265AdaptiveQuantization' "HIGH"

pattern HAQHigher :: H265AdaptiveQuantization
pattern HAQHigher = H265AdaptiveQuantization' "HIGHER"

pattern HAQMax :: H265AdaptiveQuantization
pattern HAQMax = H265AdaptiveQuantization' "MAX"

{-# COMPLETE
  HAQOff,
  HAQLow,
  HAQMedium,
  HAQHigh,
  HAQHigher,
  HAQMax,
  H265AdaptiveQuantization'
  #-}

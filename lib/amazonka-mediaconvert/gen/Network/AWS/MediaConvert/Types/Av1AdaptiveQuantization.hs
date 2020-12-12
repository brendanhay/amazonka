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
        High,
        Higher,
        Low,
        Max,
        Medium,
        Off
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to Spatial adaptive quantization (spatialAdaptiveQuantization).
newtype Av1AdaptiveQuantization = Av1AdaptiveQuantization' Lude.Text
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

pattern High :: Av1AdaptiveQuantization
pattern High = Av1AdaptiveQuantization' "HIGH"

pattern Higher :: Av1AdaptiveQuantization
pattern Higher = Av1AdaptiveQuantization' "HIGHER"

pattern Low :: Av1AdaptiveQuantization
pattern Low = Av1AdaptiveQuantization' "LOW"

pattern Max :: Av1AdaptiveQuantization
pattern Max = Av1AdaptiveQuantization' "MAX"

pattern Medium :: Av1AdaptiveQuantization
pattern Medium = Av1AdaptiveQuantization' "MEDIUM"

pattern Off :: Av1AdaptiveQuantization
pattern Off = Av1AdaptiveQuantization' "OFF"

{-# COMPLETE
  High,
  Higher,
  Low,
  Max,
  Medium,
  Off,
  Av1AdaptiveQuantization'
  #-}

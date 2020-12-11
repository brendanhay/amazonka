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
        MAQHigh,
        MAQLow,
        MAQMedium,
        MAQOff
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify the strength of any adaptive quantization filters that you enable. The value that you choose here applies to the following settings: Spatial adaptive quantization (spatialAdaptiveQuantization), and Temporal adaptive quantization (temporalAdaptiveQuantization).
newtype Mpeg2AdaptiveQuantization = Mpeg2AdaptiveQuantization' Lude.Text
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

pattern MAQHigh :: Mpeg2AdaptiveQuantization
pattern MAQHigh = Mpeg2AdaptiveQuantization' "HIGH"

pattern MAQLow :: Mpeg2AdaptiveQuantization
pattern MAQLow = Mpeg2AdaptiveQuantization' "LOW"

pattern MAQMedium :: Mpeg2AdaptiveQuantization
pattern MAQMedium = Mpeg2AdaptiveQuantization' "MEDIUM"

pattern MAQOff :: Mpeg2AdaptiveQuantization
pattern MAQOff = Mpeg2AdaptiveQuantization' "OFF"

{-# COMPLETE
  MAQHigh,
  MAQLow,
  MAQMedium,
  MAQOff,
  Mpeg2AdaptiveQuantization'
  #-}

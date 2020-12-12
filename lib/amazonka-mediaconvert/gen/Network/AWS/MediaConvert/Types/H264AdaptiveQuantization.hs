{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264AdaptiveQuantization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264AdaptiveQuantization
  ( H264AdaptiveQuantization
      ( H264AdaptiveQuantization',
        H26Auto,
        H26High,
        H26Higher,
        H26Low,
        H26Max,
        H26Medium,
        H26Off
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Keep the default value, Auto (AUTO), for this setting to have MediaConvert automatically apply the best types of quantization for your video content. When you want to apply your quantization settings manually, you must set H264AdaptiveQuantization to a value other than Auto (AUTO). Use this setting to specify the strength of any adaptive quantization filters that you enable. If you don't want MediaConvert to do any adaptive quantization in this transcode, set Adaptive quantization (H264AdaptiveQuantization) to Off (OFF). Related settings: The value that you choose here applies to the following settings: H264FlickerAdaptiveQuantization, H264SpatialAdaptiveQuantization, and H264TemporalAdaptiveQuantization.
newtype H264AdaptiveQuantization = H264AdaptiveQuantization' Lude.Text
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

pattern H26Auto :: H264AdaptiveQuantization
pattern H26Auto = H264AdaptiveQuantization' "AUTO"

pattern H26High :: H264AdaptiveQuantization
pattern H26High = H264AdaptiveQuantization' "HIGH"

pattern H26Higher :: H264AdaptiveQuantization
pattern H26Higher = H264AdaptiveQuantization' "HIGHER"

pattern H26Low :: H264AdaptiveQuantization
pattern H26Low = H264AdaptiveQuantization' "LOW"

pattern H26Max :: H264AdaptiveQuantization
pattern H26Max = H264AdaptiveQuantization' "MAX"

pattern H26Medium :: H264AdaptiveQuantization
pattern H26Medium = H264AdaptiveQuantization' "MEDIUM"

pattern H26Off :: H264AdaptiveQuantization
pattern H26Off = H264AdaptiveQuantization' "OFF"

{-# COMPLETE
  H26Auto,
  H26High,
  H26Higher,
  H26Low,
  H26Max,
  H26Medium,
  H26Off,
  H264AdaptiveQuantization'
  #-}

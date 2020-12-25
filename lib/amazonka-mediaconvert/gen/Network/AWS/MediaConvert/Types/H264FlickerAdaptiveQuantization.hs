{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264FlickerAdaptiveQuantization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264FlickerAdaptiveQuantization
  ( H264FlickerAdaptiveQuantization
      ( H264FlickerAdaptiveQuantization',
        H264FlickerAdaptiveQuantizationDisabled,
        H264FlickerAdaptiveQuantizationEnabled,
        fromH264FlickerAdaptiveQuantization
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Only use this setting when you change the default value, AUTO, for the setting H264AdaptiveQuantization. When you keep all defaults, excluding H264AdaptiveQuantization and all other adaptive quantization from your JSON job specification, MediaConvert automatically applies the best types of quantization for your video content. When you set H264AdaptiveQuantization to a value other than AUTO, the default value for H264FlickerAdaptiveQuantization is Disabled (DISABLED). Change this value to Enabled (ENABLED) to reduce I-frame pop. I-frame pop appears as a visual flicker that can arise when the encoder saves bits by copying some macroblocks many times from frame to frame, and then refreshes them at the I-frame. When you enable this setting, the encoder updates these macroblocks slightly more often to smooth out the flicker. To manually enable or disable H264FlickerAdaptiveQuantization, you must set Adaptive quantization (H264AdaptiveQuantization) to a value other than AUTO.
newtype H264FlickerAdaptiveQuantization = H264FlickerAdaptiveQuantization'
  { fromH264FlickerAdaptiveQuantization ::
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

pattern H264FlickerAdaptiveQuantizationDisabled :: H264FlickerAdaptiveQuantization
pattern H264FlickerAdaptiveQuantizationDisabled = H264FlickerAdaptiveQuantization' "DISABLED"

pattern H264FlickerAdaptiveQuantizationEnabled :: H264FlickerAdaptiveQuantization
pattern H264FlickerAdaptiveQuantizationEnabled = H264FlickerAdaptiveQuantization' "ENABLED"

{-# COMPLETE
  H264FlickerAdaptiveQuantizationDisabled,
  H264FlickerAdaptiveQuantizationEnabled,
  H264FlickerAdaptiveQuantization'
  #-}

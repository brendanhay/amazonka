{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265FlickerAdaptiveQuantization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265FlickerAdaptiveQuantization
  ( H265FlickerAdaptiveQuantization
      ( H265FlickerAdaptiveQuantization',
        H265FlickerAdaptiveQuantizationDisabled,
        H265FlickerAdaptiveQuantizationEnabled,
        fromH265FlickerAdaptiveQuantization
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Enable this setting to have the encoder reduce I-frame pop. I-frame pop appears as a visual flicker that can arise when the encoder saves bits by copying some macroblocks many times from frame to frame, and then refreshes them at the I-frame. When you enable this setting, the encoder updates these macroblocks slightly more often to smooth out the flicker. This setting is disabled by default. Related setting: In addition to enabling this setting, you must also set adaptiveQuantization to a value other than Off (OFF).
newtype H265FlickerAdaptiveQuantization = H265FlickerAdaptiveQuantization'
  { fromH265FlickerAdaptiveQuantization ::
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

pattern H265FlickerAdaptiveQuantizationDisabled :: H265FlickerAdaptiveQuantization
pattern H265FlickerAdaptiveQuantizationDisabled = H265FlickerAdaptiveQuantization' "DISABLED"

pattern H265FlickerAdaptiveQuantizationEnabled :: H265FlickerAdaptiveQuantization
pattern H265FlickerAdaptiveQuantizationEnabled = H265FlickerAdaptiveQuantization' "ENABLED"

{-# COMPLETE
  H265FlickerAdaptiveQuantizationDisabled,
  H265FlickerAdaptiveQuantizationEnabled,
  H265FlickerAdaptiveQuantization'
  #-}

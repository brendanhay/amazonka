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
        HFAQDisabled,
        HFAQEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Enable this setting to have the encoder reduce I-frame pop. I-frame pop appears as a visual flicker that can arise when the encoder saves bits by copying some macroblocks many times from frame to frame, and then refreshes them at the I-frame. When you enable this setting, the encoder updates these macroblocks slightly more often to smooth out the flicker. This setting is disabled by default. Related setting: In addition to enabling this setting, you must also set adaptiveQuantization to a value other than Off (OFF).
newtype H265FlickerAdaptiveQuantization = H265FlickerAdaptiveQuantization' Lude.Text
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

pattern HFAQDisabled :: H265FlickerAdaptiveQuantization
pattern HFAQDisabled = H265FlickerAdaptiveQuantization' "DISABLED"

pattern HFAQEnabled :: H265FlickerAdaptiveQuantization
pattern HFAQEnabled = H265FlickerAdaptiveQuantization' "ENABLED"

{-# COMPLETE
  HFAQDisabled,
  HFAQEnabled,
  H265FlickerAdaptiveQuantization'
  #-}

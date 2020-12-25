{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265TemporalAdaptiveQuantization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265TemporalAdaptiveQuantization
  ( H265TemporalAdaptiveQuantization
      ( H265TemporalAdaptiveQuantization',
        H265TemporalAdaptiveQuantizationDisabled,
        H265TemporalAdaptiveQuantizationEnabled,
        fromH265TemporalAdaptiveQuantization
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Keep the default value, Enabled (ENABLED), to adjust quantization within each frame based on temporal variation of content complexity. When you enable this feature, the encoder uses fewer bits on areas of the frame that aren't moving and uses more bits on complex objects with sharp edges that move a lot. For example, this feature improves the readability of text tickers on newscasts and scoreboards on sports matches. Enabling this feature will almost always improve your video quality. Note, though, that this feature doesn't take into account where the viewer's attention is likely to be. If viewers are likely to be focusing their attention on a part of the screen that doesn't have moving objects with sharp edges, such as sports athletes' faces, you might choose to disable this feature. Related setting: When you enable temporal quantization, adjust the strength of the filter with the setting Adaptive quantization (adaptiveQuantization).
newtype H265TemporalAdaptiveQuantization = H265TemporalAdaptiveQuantization'
  { fromH265TemporalAdaptiveQuantization ::
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

pattern H265TemporalAdaptiveQuantizationDisabled :: H265TemporalAdaptiveQuantization
pattern H265TemporalAdaptiveQuantizationDisabled = H265TemporalAdaptiveQuantization' "DISABLED"

pattern H265TemporalAdaptiveQuantizationEnabled :: H265TemporalAdaptiveQuantization
pattern H265TemporalAdaptiveQuantizationEnabled = H265TemporalAdaptiveQuantization' "ENABLED"

{-# COMPLETE
  H265TemporalAdaptiveQuantizationDisabled,
  H265TemporalAdaptiveQuantizationEnabled,
  H265TemporalAdaptiveQuantization'
  #-}

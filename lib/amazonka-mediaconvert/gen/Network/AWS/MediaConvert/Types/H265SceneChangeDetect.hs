{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265SceneChangeDetect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265SceneChangeDetect
  ( H265SceneChangeDetect
      ( H265SceneChangeDetect',
        H265SceneChangeDetectDisabled,
        H265SceneChangeDetectEnabled,
        H265SceneChangeDetectTransitionDetection,
        fromH265SceneChangeDetect
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
newtype H265SceneChangeDetect = H265SceneChangeDetect'
  { fromH265SceneChangeDetect ::
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

pattern H265SceneChangeDetectDisabled :: H265SceneChangeDetect
pattern H265SceneChangeDetectDisabled = H265SceneChangeDetect' "DISABLED"

pattern H265SceneChangeDetectEnabled :: H265SceneChangeDetect
pattern H265SceneChangeDetectEnabled = H265SceneChangeDetect' "ENABLED"

pattern H265SceneChangeDetectTransitionDetection :: H265SceneChangeDetect
pattern H265SceneChangeDetectTransitionDetection = H265SceneChangeDetect' "TRANSITION_DETECTION"

{-# COMPLETE
  H265SceneChangeDetectDisabled,
  H265SceneChangeDetectEnabled,
  H265SceneChangeDetectTransitionDetection,
  H265SceneChangeDetect'
  #-}

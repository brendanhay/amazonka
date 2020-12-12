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
        HSCDDisabled,
        HSCDEnabled,
        HSCDTransitionDetection
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
newtype H265SceneChangeDetect = H265SceneChangeDetect' Lude.Text
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

pattern HSCDDisabled :: H265SceneChangeDetect
pattern HSCDDisabled = H265SceneChangeDetect' "DISABLED"

pattern HSCDEnabled :: H265SceneChangeDetect
pattern HSCDEnabled = H265SceneChangeDetect' "ENABLED"

pattern HSCDTransitionDetection :: H265SceneChangeDetect
pattern HSCDTransitionDetection = H265SceneChangeDetect' "TRANSITION_DETECTION"

{-# COMPLETE
  HSCDDisabled,
  HSCDEnabled,
  HSCDTransitionDetection,
  H265SceneChangeDetect'
  #-}

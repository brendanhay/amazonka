{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264SceneChangeDetect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264SceneChangeDetect
  ( H264SceneChangeDetect
      ( H264SceneChangeDetect',
        HSCDSDisabled,
        HSCDSEnabled,
        HSCDSTransitionDetection
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default. If this output uses QVBR, choose Transition detection (TRANSITION_DETECTION) for further video quality improvement. For more information about QVBR, see https://docs.aws.amazon.com/console/mediaconvert/cbr-vbr-qvbr.
newtype H264SceneChangeDetect = H264SceneChangeDetect' Lude.Text
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

pattern HSCDSDisabled :: H264SceneChangeDetect
pattern HSCDSDisabled = H264SceneChangeDetect' "DISABLED"

pattern HSCDSEnabled :: H264SceneChangeDetect
pattern HSCDSEnabled = H264SceneChangeDetect' "ENABLED"

pattern HSCDSTransitionDetection :: H264SceneChangeDetect
pattern HSCDSTransitionDetection = H264SceneChangeDetect' "TRANSITION_DETECTION"

{-# COMPLETE
  HSCDSDisabled,
  HSCDSEnabled,
  HSCDSTransitionDetection,
  H264SceneChangeDetect'
  #-}

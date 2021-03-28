{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2SlowPal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2SlowPal
  ( Mpeg2SlowPal
    ( Mpeg2SlowPal'
    , Mpeg2SlowPalDisabled
    , Mpeg2SlowPalEnabled
    , fromMpeg2SlowPal
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames per second (fps). Enable slow PAL to create a 25 fps output. When you enable slow PAL, MediaConvert relabels the video frames to 25 fps and resamples your audio to keep it synchronized with the video. Note that enabling this setting will slightly reduce the duration of your video. Required settings: You must also set Framerate to 25. In your JSON job specification, set (framerateControl) to (SPECIFIED), (framerateNumerator) to 25 and (framerateDenominator) to 1.
newtype Mpeg2SlowPal = Mpeg2SlowPal'{fromMpeg2SlowPal :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern Mpeg2SlowPalDisabled :: Mpeg2SlowPal
pattern Mpeg2SlowPalDisabled = Mpeg2SlowPal' "DISABLED"

pattern Mpeg2SlowPalEnabled :: Mpeg2SlowPal
pattern Mpeg2SlowPalEnabled = Mpeg2SlowPal' "ENABLED"

{-# COMPLETE 
  Mpeg2SlowPalDisabled,

  Mpeg2SlowPalEnabled,
  Mpeg2SlowPal'
  #-}

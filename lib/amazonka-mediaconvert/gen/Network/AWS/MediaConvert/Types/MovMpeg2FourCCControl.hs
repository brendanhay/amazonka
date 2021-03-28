{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MovMpeg2FourCCControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.MovMpeg2FourCCControl
  ( MovMpeg2FourCCControl
    ( MovMpeg2FourCCControl'
    , MovMpeg2FourCCControlXdcam
    , MovMpeg2FourCCControlMpeg
    , fromMovMpeg2FourCCControl
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | When set to XDCAM, writes MPEG2 video streams into the QuickTime file using XDCAM fourcc codes. This increases compatibility with Apple editors and players, but may decrease compatibility with other players. Only applicable when the video codec is MPEG2.
newtype MovMpeg2FourCCControl = MovMpeg2FourCCControl'{fromMovMpeg2FourCCControl
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern MovMpeg2FourCCControlXdcam :: MovMpeg2FourCCControl
pattern MovMpeg2FourCCControlXdcam = MovMpeg2FourCCControl' "XDCAM"

pattern MovMpeg2FourCCControlMpeg :: MovMpeg2FourCCControl
pattern MovMpeg2FourCCControlMpeg = MovMpeg2FourCCControl' "MPEG"

{-# COMPLETE 
  MovMpeg2FourCCControlXdcam,

  MovMpeg2FourCCControlMpeg,
  MovMpeg2FourCCControl'
  #-}

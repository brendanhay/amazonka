{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.UdpGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.UdpGroupSettings
  ( UdpGroupSettings (..),

    -- * Smart constructor
    mkUdpGroupSettings,

    -- * Lenses
    ugsTimedMetadataId3Period,
    ugsInputLossAction,
    ugsTimedMetadataId3Frame,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputLossActionForUdpOut
import Network.AWS.MediaLive.Types.UdpTimedMetadataId3Frame
import qualified Network.AWS.Prelude as Lude

-- | Udp Group Settings
--
-- /See:/ 'mkUdpGroupSettings' smart constructor.
data UdpGroupSettings = UdpGroupSettings'
  { timedMetadataId3Period ::
      Lude.Maybe Lude.Natural,
    inputLossAction :: Lude.Maybe InputLossActionForUdpOut,
    timedMetadataId3Frame ::
      Lude.Maybe UdpTimedMetadataId3Frame
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UdpGroupSettings' with the minimum fields required to make a request.
--
-- * 'inputLossAction' - Specifies behavior of last resort when input video is lost, and no more backup inputs are available. When dropTs is selected the entire transport stream will stop being emitted.  When dropProgram is selected the program can be dropped from the transport stream (and replaced with null packets to meet the TS bitrate requirement).  Or, when emitProgram is chosen the transport stream will continue to be produced normally with repeat frames, black frames, or slate frames substituted for the absent input video.
-- * 'timedMetadataId3Frame' - Indicates ID3 frame that has the timecode.
-- * 'timedMetadataId3Period' - Timed Metadata interval in seconds.
mkUdpGroupSettings ::
  UdpGroupSettings
mkUdpGroupSettings =
  UdpGroupSettings'
    { timedMetadataId3Period = Lude.Nothing,
      inputLossAction = Lude.Nothing,
      timedMetadataId3Frame = Lude.Nothing
    }

-- | Timed Metadata interval in seconds.
--
-- /Note:/ Consider using 'timedMetadataId3Period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsTimedMetadataId3Period :: Lens.Lens' UdpGroupSettings (Lude.Maybe Lude.Natural)
ugsTimedMetadataId3Period = Lens.lens (timedMetadataId3Period :: UdpGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {timedMetadataId3Period = a} :: UdpGroupSettings)
{-# DEPRECATED ugsTimedMetadataId3Period "Use generic-lens or generic-optics with 'timedMetadataId3Period' instead." #-}

-- | Specifies behavior of last resort when input video is lost, and no more backup inputs are available. When dropTs is selected the entire transport stream will stop being emitted.  When dropProgram is selected the program can be dropped from the transport stream (and replaced with null packets to meet the TS bitrate requirement).  Or, when emitProgram is chosen the transport stream will continue to be produced normally with repeat frames, black frames, or slate frames substituted for the absent input video.
--
-- /Note:/ Consider using 'inputLossAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsInputLossAction :: Lens.Lens' UdpGroupSettings (Lude.Maybe InputLossActionForUdpOut)
ugsInputLossAction = Lens.lens (inputLossAction :: UdpGroupSettings -> Lude.Maybe InputLossActionForUdpOut) (\s a -> s {inputLossAction = a} :: UdpGroupSettings)
{-# DEPRECATED ugsInputLossAction "Use generic-lens or generic-optics with 'inputLossAction' instead." #-}

-- | Indicates ID3 frame that has the timecode.
--
-- /Note:/ Consider using 'timedMetadataId3Frame' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsTimedMetadataId3Frame :: Lens.Lens' UdpGroupSettings (Lude.Maybe UdpTimedMetadataId3Frame)
ugsTimedMetadataId3Frame = Lens.lens (timedMetadataId3Frame :: UdpGroupSettings -> Lude.Maybe UdpTimedMetadataId3Frame) (\s a -> s {timedMetadataId3Frame = a} :: UdpGroupSettings)
{-# DEPRECATED ugsTimedMetadataId3Frame "Use generic-lens or generic-optics with 'timedMetadataId3Frame' instead." #-}

instance Lude.FromJSON UdpGroupSettings where
  parseJSON =
    Lude.withObject
      "UdpGroupSettings"
      ( \x ->
          UdpGroupSettings'
            Lude.<$> (x Lude..:? "timedMetadataId3Period")
            Lude.<*> (x Lude..:? "inputLossAction")
            Lude.<*> (x Lude..:? "timedMetadataId3Frame")
      )

instance Lude.ToJSON UdpGroupSettings where
  toJSON UdpGroupSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("timedMetadataId3Period" Lude..=)
              Lude.<$> timedMetadataId3Period,
            ("inputLossAction" Lude..=) Lude.<$> inputLossAction,
            ("timedMetadataId3Frame" Lude..=) Lude.<$> timedMetadataId3Frame
          ]
      )

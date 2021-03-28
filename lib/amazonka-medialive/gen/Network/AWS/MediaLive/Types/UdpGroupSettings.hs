{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.UdpGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.UdpGroupSettings
  ( UdpGroupSettings (..)
  -- * Smart constructor
  , mkUdpGroupSettings
  -- * Lenses
  , ugsInputLossAction
  , ugsTimedMetadataId3Frame
  , ugsTimedMetadataId3Period
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.InputLossActionForUdpOut as Types
import qualified Network.AWS.MediaLive.Types.UdpTimedMetadataId3Frame as Types
import qualified Network.AWS.Prelude as Core

-- | Udp Group Settings
--
-- /See:/ 'mkUdpGroupSettings' smart constructor.
data UdpGroupSettings = UdpGroupSettings'
  { inputLossAction :: Core.Maybe Types.InputLossActionForUdpOut
    -- ^ Specifies behavior of last resort when input video is lost, and no more backup inputs are available. When dropTs is selected the entire transport stream will stop being emitted.  When dropProgram is selected the program can be dropped from the transport stream (and replaced with null packets to meet the TS bitrate requirement).  Or, when emitProgram is chosen the transport stream will continue to be produced normally with repeat frames, black frames, or slate frames substituted for the absent input video.
  , timedMetadataId3Frame :: Core.Maybe Types.UdpTimedMetadataId3Frame
    -- ^ Indicates ID3 frame that has the timecode.
  , timedMetadataId3Period :: Core.Maybe Core.Natural
    -- ^ Timed Metadata interval in seconds.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UdpGroupSettings' value with any optional fields omitted.
mkUdpGroupSettings
    :: UdpGroupSettings
mkUdpGroupSettings
  = UdpGroupSettings'{inputLossAction = Core.Nothing,
                      timedMetadataId3Frame = Core.Nothing,
                      timedMetadataId3Period = Core.Nothing}

-- | Specifies behavior of last resort when input video is lost, and no more backup inputs are available. When dropTs is selected the entire transport stream will stop being emitted.  When dropProgram is selected the program can be dropped from the transport stream (and replaced with null packets to meet the TS bitrate requirement).  Or, when emitProgram is chosen the transport stream will continue to be produced normally with repeat frames, black frames, or slate frames substituted for the absent input video.
--
-- /Note:/ Consider using 'inputLossAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsInputLossAction :: Lens.Lens' UdpGroupSettings (Core.Maybe Types.InputLossActionForUdpOut)
ugsInputLossAction = Lens.field @"inputLossAction"
{-# INLINEABLE ugsInputLossAction #-}
{-# DEPRECATED inputLossAction "Use generic-lens or generic-optics with 'inputLossAction' instead"  #-}

-- | Indicates ID3 frame that has the timecode.
--
-- /Note:/ Consider using 'timedMetadataId3Frame' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsTimedMetadataId3Frame :: Lens.Lens' UdpGroupSettings (Core.Maybe Types.UdpTimedMetadataId3Frame)
ugsTimedMetadataId3Frame = Lens.field @"timedMetadataId3Frame"
{-# INLINEABLE ugsTimedMetadataId3Frame #-}
{-# DEPRECATED timedMetadataId3Frame "Use generic-lens or generic-optics with 'timedMetadataId3Frame' instead"  #-}

-- | Timed Metadata interval in seconds.
--
-- /Note:/ Consider using 'timedMetadataId3Period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsTimedMetadataId3Period :: Lens.Lens' UdpGroupSettings (Core.Maybe Core.Natural)
ugsTimedMetadataId3Period = Lens.field @"timedMetadataId3Period"
{-# INLINEABLE ugsTimedMetadataId3Period #-}
{-# DEPRECATED timedMetadataId3Period "Use generic-lens or generic-optics with 'timedMetadataId3Period' instead"  #-}

instance Core.FromJSON UdpGroupSettings where
        toJSON UdpGroupSettings{..}
          = Core.object
              (Core.catMaybes
                 [("inputLossAction" Core..=) Core.<$> inputLossAction,
                  ("timedMetadataId3Frame" Core..=) Core.<$> timedMetadataId3Frame,
                  ("timedMetadataId3Period" Core..=) Core.<$>
                    timedMetadataId3Period])

instance Core.FromJSON UdpGroupSettings where
        parseJSON
          = Core.withObject "UdpGroupSettings" Core.$
              \ x ->
                UdpGroupSettings' Core.<$>
                  (x Core..:? "inputLossAction") Core.<*>
                    x Core..:? "timedMetadataId3Frame"
                    Core.<*> x Core..:? "timedMetadataId3Period"

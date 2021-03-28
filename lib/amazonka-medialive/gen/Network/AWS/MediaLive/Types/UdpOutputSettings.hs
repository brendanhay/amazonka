{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.UdpOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.UdpOutputSettings
  ( UdpOutputSettings (..)
  -- * Smart constructor
  , mkUdpOutputSettings
  -- * Lenses
  , uosDestination
  , uosContainerSettings
  , uosBufferMsec
  , uosFecOutputSettings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.FecOutputSettings as Types
import qualified Network.AWS.MediaLive.Types.OutputLocationRef as Types
import qualified Network.AWS.MediaLive.Types.UdpContainerSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Udp Output Settings
--
-- /See:/ 'mkUdpOutputSettings' smart constructor.
data UdpOutputSettings = UdpOutputSettings'
  { destination :: Types.OutputLocationRef
    -- ^ Destination address and port number for RTP or UDP packets. Can be unicast or multicast RTP or UDP (eg. rtp://239.10.10.10:5001 or udp://10.100.100.100:5002).
  , containerSettings :: Types.UdpContainerSettings
  , bufferMsec :: Core.Maybe Core.Natural
    -- ^ UDP output buffering in milliseconds. Larger values increase latency through the transcoder but simultaneously assist the transcoder in maintaining a constant, low-jitter UDP/RTP output while accommodating clock recovery, input switching, input disruptions, picture reordering, etc.
  , fecOutputSettings :: Core.Maybe Types.FecOutputSettings
    -- ^ Settings for enabling and adjusting Forward Error Correction on UDP outputs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UdpOutputSettings' value with any optional fields omitted.
mkUdpOutputSettings
    :: Types.OutputLocationRef -- ^ 'destination'
    -> Types.UdpContainerSettings -- ^ 'containerSettings'
    -> UdpOutputSettings
mkUdpOutputSettings destination containerSettings
  = UdpOutputSettings'{destination, containerSettings,
                       bufferMsec = Core.Nothing, fecOutputSettings = Core.Nothing}

-- | Destination address and port number for RTP or UDP packets. Can be unicast or multicast RTP or UDP (eg. rtp://239.10.10.10:5001 or udp://10.100.100.100:5002).
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uosDestination :: Lens.Lens' UdpOutputSettings Types.OutputLocationRef
uosDestination = Lens.field @"destination"
{-# INLINEABLE uosDestination #-}
{-# DEPRECATED destination "Use generic-lens or generic-optics with 'destination' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'containerSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uosContainerSettings :: Lens.Lens' UdpOutputSettings Types.UdpContainerSettings
uosContainerSettings = Lens.field @"containerSettings"
{-# INLINEABLE uosContainerSettings #-}
{-# DEPRECATED containerSettings "Use generic-lens or generic-optics with 'containerSettings' instead"  #-}

-- | UDP output buffering in milliseconds. Larger values increase latency through the transcoder but simultaneously assist the transcoder in maintaining a constant, low-jitter UDP/RTP output while accommodating clock recovery, input switching, input disruptions, picture reordering, etc.
--
-- /Note:/ Consider using 'bufferMsec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uosBufferMsec :: Lens.Lens' UdpOutputSettings (Core.Maybe Core.Natural)
uosBufferMsec = Lens.field @"bufferMsec"
{-# INLINEABLE uosBufferMsec #-}
{-# DEPRECATED bufferMsec "Use generic-lens or generic-optics with 'bufferMsec' instead"  #-}

-- | Settings for enabling and adjusting Forward Error Correction on UDP outputs.
--
-- /Note:/ Consider using 'fecOutputSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uosFecOutputSettings :: Lens.Lens' UdpOutputSettings (Core.Maybe Types.FecOutputSettings)
uosFecOutputSettings = Lens.field @"fecOutputSettings"
{-# INLINEABLE uosFecOutputSettings #-}
{-# DEPRECATED fecOutputSettings "Use generic-lens or generic-optics with 'fecOutputSettings' instead"  #-}

instance Core.FromJSON UdpOutputSettings where
        toJSON UdpOutputSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("destination" Core..= destination),
                  Core.Just ("containerSettings" Core..= containerSettings),
                  ("bufferMsec" Core..=) Core.<$> bufferMsec,
                  ("fecOutputSettings" Core..=) Core.<$> fecOutputSettings])

instance Core.FromJSON UdpOutputSettings where
        parseJSON
          = Core.withObject "UdpOutputSettings" Core.$
              \ x ->
                UdpOutputSettings' Core.<$>
                  (x Core..: "destination") Core.<*> x Core..: "containerSettings"
                    Core.<*> x Core..:? "bufferMsec"
                    Core.<*> x Core..:? "fecOutputSettings"

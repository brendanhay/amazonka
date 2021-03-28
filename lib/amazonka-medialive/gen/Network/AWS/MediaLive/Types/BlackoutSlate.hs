{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BlackoutSlate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.BlackoutSlate
  ( BlackoutSlate (..)
  -- * Smart constructor
  , mkBlackoutSlate
  -- * Lenses
  , bsBlackoutSlateImage
  , bsNetworkEndBlackout
  , bsNetworkEndBlackoutImage
  , bsNetworkId
  , bsState
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.BlackoutSlateNetworkEndBlackout as Types
import qualified Network.AWS.MediaLive.Types.BlackoutSlateState as Types
import qualified Network.AWS.MediaLive.Types.InputLocation as Types
import qualified Network.AWS.Prelude as Core

-- | Blackout Slate
--
-- /See:/ 'mkBlackoutSlate' smart constructor.
data BlackoutSlate = BlackoutSlate'
  { blackoutSlateImage :: Core.Maybe Types.InputLocation
    -- ^ Blackout slate image to be used. Leave empty for solid black. Only bmp and png images are supported.
  , networkEndBlackout :: Core.Maybe Types.BlackoutSlateNetworkEndBlackout
    -- ^ Setting to enabled causes the encoder to blackout the video, audio, and captions, and raise the "Network Blackout Image" slate when an SCTE104/35 Network End Segmentation Descriptor is encountered. The blackout will be lifted when the Network Start Segmentation Descriptor is encountered. The Network End and Network Start descriptors must contain a network ID that matches the value entered in "Network ID".
  , networkEndBlackoutImage :: Core.Maybe Types.InputLocation
    -- ^ Path to local file to use as Network End Blackout image. Image will be scaled to fill the entire output raster.
  , networkId :: Core.Maybe Core.Text
    -- ^ Provides Network ID that matches EIDR ID format (e.g., "10.XXXX/XXXX-XXXX-XXXX-XXXX-XXXX-C").
  , state :: Core.Maybe Types.BlackoutSlateState
    -- ^ When set to enabled, causes video, audio and captions to be blanked when indicated by program metadata.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BlackoutSlate' value with any optional fields omitted.
mkBlackoutSlate
    :: BlackoutSlate
mkBlackoutSlate
  = BlackoutSlate'{blackoutSlateImage = Core.Nothing,
                   networkEndBlackout = Core.Nothing,
                   networkEndBlackoutImage = Core.Nothing, networkId = Core.Nothing,
                   state = Core.Nothing}

-- | Blackout slate image to be used. Leave empty for solid black. Only bmp and png images are supported.
--
-- /Note:/ Consider using 'blackoutSlateImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsBlackoutSlateImage :: Lens.Lens' BlackoutSlate (Core.Maybe Types.InputLocation)
bsBlackoutSlateImage = Lens.field @"blackoutSlateImage"
{-# INLINEABLE bsBlackoutSlateImage #-}
{-# DEPRECATED blackoutSlateImage "Use generic-lens or generic-optics with 'blackoutSlateImage' instead"  #-}

-- | Setting to enabled causes the encoder to blackout the video, audio, and captions, and raise the "Network Blackout Image" slate when an SCTE104/35 Network End Segmentation Descriptor is encountered. The blackout will be lifted when the Network Start Segmentation Descriptor is encountered. The Network End and Network Start descriptors must contain a network ID that matches the value entered in "Network ID".
--
-- /Note:/ Consider using 'networkEndBlackout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsNetworkEndBlackout :: Lens.Lens' BlackoutSlate (Core.Maybe Types.BlackoutSlateNetworkEndBlackout)
bsNetworkEndBlackout = Lens.field @"networkEndBlackout"
{-# INLINEABLE bsNetworkEndBlackout #-}
{-# DEPRECATED networkEndBlackout "Use generic-lens or generic-optics with 'networkEndBlackout' instead"  #-}

-- | Path to local file to use as Network End Blackout image. Image will be scaled to fill the entire output raster.
--
-- /Note:/ Consider using 'networkEndBlackoutImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsNetworkEndBlackoutImage :: Lens.Lens' BlackoutSlate (Core.Maybe Types.InputLocation)
bsNetworkEndBlackoutImage = Lens.field @"networkEndBlackoutImage"
{-# INLINEABLE bsNetworkEndBlackoutImage #-}
{-# DEPRECATED networkEndBlackoutImage "Use generic-lens or generic-optics with 'networkEndBlackoutImage' instead"  #-}

-- | Provides Network ID that matches EIDR ID format (e.g., "10.XXXX/XXXX-XXXX-XXXX-XXXX-XXXX-C").
--
-- /Note:/ Consider using 'networkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsNetworkId :: Lens.Lens' BlackoutSlate (Core.Maybe Core.Text)
bsNetworkId = Lens.field @"networkId"
{-# INLINEABLE bsNetworkId #-}
{-# DEPRECATED networkId "Use generic-lens or generic-optics with 'networkId' instead"  #-}

-- | When set to enabled, causes video, audio and captions to be blanked when indicated by program metadata.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsState :: Lens.Lens' BlackoutSlate (Core.Maybe Types.BlackoutSlateState)
bsState = Lens.field @"state"
{-# INLINEABLE bsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.FromJSON BlackoutSlate where
        toJSON BlackoutSlate{..}
          = Core.object
              (Core.catMaybes
                 [("blackoutSlateImage" Core..=) Core.<$> blackoutSlateImage,
                  ("networkEndBlackout" Core..=) Core.<$> networkEndBlackout,
                  ("networkEndBlackoutImage" Core..=) Core.<$>
                    networkEndBlackoutImage,
                  ("networkId" Core..=) Core.<$> networkId,
                  ("state" Core..=) Core.<$> state])

instance Core.FromJSON BlackoutSlate where
        parseJSON
          = Core.withObject "BlackoutSlate" Core.$
              \ x ->
                BlackoutSlate' Core.<$>
                  (x Core..:? "blackoutSlateImage") Core.<*>
                    x Core..:? "networkEndBlackout"
                    Core.<*> x Core..:? "networkEndBlackoutImage"
                    Core.<*> x Core..:? "networkId"
                    Core.<*> x Core..:? "state"

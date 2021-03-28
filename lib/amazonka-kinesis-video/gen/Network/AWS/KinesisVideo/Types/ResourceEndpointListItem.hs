{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.ResourceEndpointListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideo.Types.ResourceEndpointListItem
  ( ResourceEndpointListItem (..)
  -- * Smart constructor
  , mkResourceEndpointListItem
  -- * Lenses
  , reliProtocol
  , reliResourceEndpoint
  ) where

import qualified Network.AWS.KinesisVideo.Types.ChannelProtocol as Types
import qualified Network.AWS.KinesisVideo.Types.ResourceEndpoint as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that describes the endpoint of the signaling channel returned by the @GetSignalingChannelEndpoint@ API.
--
-- /See:/ 'mkResourceEndpointListItem' smart constructor.
data ResourceEndpointListItem = ResourceEndpointListItem'
  { protocol :: Core.Maybe Types.ChannelProtocol
    -- ^ The protocol of the signaling channel returned by the @GetSignalingChannelEndpoint@ API.
  , resourceEndpoint :: Core.Maybe Types.ResourceEndpoint
    -- ^ The endpoint of the signaling channel returned by the @GetSignalingChannelEndpoint@ API.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceEndpointListItem' value with any optional fields omitted.
mkResourceEndpointListItem
    :: ResourceEndpointListItem
mkResourceEndpointListItem
  = ResourceEndpointListItem'{protocol = Core.Nothing,
                              resourceEndpoint = Core.Nothing}

-- | The protocol of the signaling channel returned by the @GetSignalingChannelEndpoint@ API.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reliProtocol :: Lens.Lens' ResourceEndpointListItem (Core.Maybe Types.ChannelProtocol)
reliProtocol = Lens.field @"protocol"
{-# INLINEABLE reliProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

-- | The endpoint of the signaling channel returned by the @GetSignalingChannelEndpoint@ API.
--
-- /Note:/ Consider using 'resourceEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reliResourceEndpoint :: Lens.Lens' ResourceEndpointListItem (Core.Maybe Types.ResourceEndpoint)
reliResourceEndpoint = Lens.field @"resourceEndpoint"
{-# INLINEABLE reliResourceEndpoint #-}
{-# DEPRECATED resourceEndpoint "Use generic-lens or generic-optics with 'resourceEndpoint' instead"  #-}

instance Core.FromJSON ResourceEndpointListItem where
        parseJSON
          = Core.withObject "ResourceEndpointListItem" Core.$
              \ x ->
                ResourceEndpointListItem' Core.<$>
                  (x Core..:? "Protocol") Core.<*> x Core..:? "ResourceEndpoint"

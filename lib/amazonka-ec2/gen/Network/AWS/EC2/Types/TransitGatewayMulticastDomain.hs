{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastDomain
  ( TransitGatewayMulticastDomain (..),

    -- * Smart constructor
    mkTransitGatewayMulticastDomain,

    -- * Lenses
    tgmdCreationTime,
    tgmdState,
    tgmdTags,
    tgmdTransitGatewayId,
    tgmdTransitGatewayMulticastDomainId,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.TransitGatewayMulticastDomainState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the transit gateway multicast domain.
--
-- /See:/ 'mkTransitGatewayMulticastDomain' smart constructor.
data TransitGatewayMulticastDomain = TransitGatewayMulticastDomain'
  { -- | The time the transit gateway multicast domain was created.
    creationTime :: Core.Maybe Core.UTCTime,
    -- | The state of the transit gateway multicast domain.
    state :: Core.Maybe Types.TransitGatewayMulticastDomainState,
    -- | The tags for the transit gateway multicast domain.
    tags :: Core.Maybe [Types.Tag],
    -- | The ID of the transit gateway.
    transitGatewayId :: Core.Maybe Types.String,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TransitGatewayMulticastDomain' value with any optional fields omitted.
mkTransitGatewayMulticastDomain ::
  TransitGatewayMulticastDomain
mkTransitGatewayMulticastDomain =
  TransitGatewayMulticastDomain'
    { creationTime = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      transitGatewayId = Core.Nothing,
      transitGatewayMulticastDomainId = Core.Nothing
    }

-- | The time the transit gateway multicast domain was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdCreationTime :: Lens.Lens' TransitGatewayMulticastDomain (Core.Maybe Core.UTCTime)
tgmdCreationTime = Lens.field @"creationTime"
{-# DEPRECATED tgmdCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The state of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdState :: Lens.Lens' TransitGatewayMulticastDomain (Core.Maybe Types.TransitGatewayMulticastDomainState)
tgmdState = Lens.field @"state"
{-# DEPRECATED tgmdState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The tags for the transit gateway multicast domain.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdTags :: Lens.Lens' TransitGatewayMulticastDomain (Core.Maybe [Types.Tag])
tgmdTags = Lens.field @"tags"
{-# DEPRECATED tgmdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdTransitGatewayId :: Lens.Lens' TransitGatewayMulticastDomain (Core.Maybe Types.String)
tgmdTransitGatewayId = Lens.field @"transitGatewayId"
{-# DEPRECATED tgmdTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdTransitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastDomain (Core.Maybe Types.String)
tgmdTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# DEPRECATED tgmdTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

instance Core.FromXML TransitGatewayMulticastDomain where
  parseXML x =
    TransitGatewayMulticastDomain'
      Core.<$> (x Core..@? "creationTime")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "transitGatewayId")
      Core.<*> (x Core..@? "transitGatewayMulticastDomainId")

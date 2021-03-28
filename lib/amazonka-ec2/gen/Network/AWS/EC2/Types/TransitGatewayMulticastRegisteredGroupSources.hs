{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastRegisteredGroupSources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayMulticastRegisteredGroupSources
  ( TransitGatewayMulticastRegisteredGroupSources (..)
  -- * Smart constructor
  , mkTransitGatewayMulticastRegisteredGroupSources
  -- * Lenses
  , tgmrgsGroupIpAddress
  , tgmrgsRegisteredNetworkInterfaceIds
  , tgmrgsTransitGatewayMulticastDomainId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the members registered with the transit gateway multicast group.
--
-- /See:/ 'mkTransitGatewayMulticastRegisteredGroupSources' smart constructor.
data TransitGatewayMulticastRegisteredGroupSources = TransitGatewayMulticastRegisteredGroupSources'
  { groupIpAddress :: Core.Maybe Core.Text
    -- ^ The IP address assigned to the transit gateway multicast group.
  , registeredNetworkInterfaceIds :: Core.Maybe [Core.Text]
    -- ^ The IDs of the network interfaces members registered with the transit gateway multicast group.
  , transitGatewayMulticastDomainId :: Core.Maybe Core.Text
    -- ^ The ID of the transit gateway multicast domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayMulticastRegisteredGroupSources' value with any optional fields omitted.
mkTransitGatewayMulticastRegisteredGroupSources
    :: TransitGatewayMulticastRegisteredGroupSources
mkTransitGatewayMulticastRegisteredGroupSources
  = TransitGatewayMulticastRegisteredGroupSources'{groupIpAddress =
                                                     Core.Nothing,
                                                   registeredNetworkInterfaceIds = Core.Nothing,
                                                   transitGatewayMulticastDomainId = Core.Nothing}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmrgsGroupIpAddress :: Lens.Lens' TransitGatewayMulticastRegisteredGroupSources (Core.Maybe Core.Text)
tgmrgsGroupIpAddress = Lens.field @"groupIpAddress"
{-# INLINEABLE tgmrgsGroupIpAddress #-}
{-# DEPRECATED groupIpAddress "Use generic-lens or generic-optics with 'groupIpAddress' instead"  #-}

-- | The IDs of the network interfaces members registered with the transit gateway multicast group.
--
-- /Note:/ Consider using 'registeredNetworkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmrgsRegisteredNetworkInterfaceIds :: Lens.Lens' TransitGatewayMulticastRegisteredGroupSources (Core.Maybe [Core.Text])
tgmrgsRegisteredNetworkInterfaceIds = Lens.field @"registeredNetworkInterfaceIds"
{-# INLINEABLE tgmrgsRegisteredNetworkInterfaceIds #-}
{-# DEPRECATED registeredNetworkInterfaceIds "Use generic-lens or generic-optics with 'registeredNetworkInterfaceIds' instead"  #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmrgsTransitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastRegisteredGroupSources (Core.Maybe Core.Text)
tgmrgsTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# INLINEABLE tgmrgsTransitGatewayMulticastDomainId #-}
{-# DEPRECATED transitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead"  #-}

instance Core.FromXML TransitGatewayMulticastRegisteredGroupSources
         where
        parseXML x
          = TransitGatewayMulticastRegisteredGroupSources' Core.<$>
              (x Core..@? "groupIpAddress") Core.<*>
                x Core..@? "registeredNetworkInterfaceIds" Core..<@>
                  Core.parseXMLList "item"
                Core.<*> x Core..@? "transitGatewayMulticastDomainId"

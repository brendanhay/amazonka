{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupSources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupSources
  ( TransitGatewayMulticastDeregisteredGroupSources (..)
  -- * Smart constructor
  , mkTransitGatewayMulticastDeregisteredGroupSources
  -- * Lenses
  , tgmdgsDeregisteredNetworkInterfaceIds
  , tgmdgsGroupIpAddress
  , tgmdgsTransitGatewayMulticastDomainId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the deregistered transit gateway multicast group sources.
--
-- /See:/ 'mkTransitGatewayMulticastDeregisteredGroupSources' smart constructor.
data TransitGatewayMulticastDeregisteredGroupSources = TransitGatewayMulticastDeregisteredGroupSources'
  { deregisteredNetworkInterfaceIds :: Core.Maybe [Core.Text]
    -- ^ The network interface IDs of the non-registered members.
  , groupIpAddress :: Core.Maybe Core.Text
    -- ^ The IP address assigned to the transit gateway multicast group.
  , transitGatewayMulticastDomainId :: Core.Maybe Core.Text
    -- ^ The ID of the transit gateway multicast domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayMulticastDeregisteredGroupSources' value with any optional fields omitted.
mkTransitGatewayMulticastDeregisteredGroupSources
    :: TransitGatewayMulticastDeregisteredGroupSources
mkTransitGatewayMulticastDeregisteredGroupSources
  = TransitGatewayMulticastDeregisteredGroupSources'{deregisteredNetworkInterfaceIds
                                                       = Core.Nothing,
                                                     groupIpAddress = Core.Nothing,
                                                     transitGatewayMulticastDomainId = Core.Nothing}

-- | The network interface IDs of the non-registered members.
--
-- /Note:/ Consider using 'deregisteredNetworkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdgsDeregisteredNetworkInterfaceIds :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupSources (Core.Maybe [Core.Text])
tgmdgsDeregisteredNetworkInterfaceIds = Lens.field @"deregisteredNetworkInterfaceIds"
{-# INLINEABLE tgmdgsDeregisteredNetworkInterfaceIds #-}
{-# DEPRECATED deregisteredNetworkInterfaceIds "Use generic-lens or generic-optics with 'deregisteredNetworkInterfaceIds' instead"  #-}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdgsGroupIpAddress :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupSources (Core.Maybe Core.Text)
tgmdgsGroupIpAddress = Lens.field @"groupIpAddress"
{-# INLINEABLE tgmdgsGroupIpAddress #-}
{-# DEPRECATED groupIpAddress "Use generic-lens or generic-optics with 'groupIpAddress' instead"  #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdgsTransitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupSources (Core.Maybe Core.Text)
tgmdgsTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# INLINEABLE tgmdgsTransitGatewayMulticastDomainId #-}
{-# DEPRECATED transitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead"  #-}

instance Core.FromXML
           TransitGatewayMulticastDeregisteredGroupSources
         where
        parseXML x
          = TransitGatewayMulticastDeregisteredGroupSources' Core.<$>
              (x Core..@? "deregisteredNetworkInterfaceIds" Core..<@>
                 Core.parseXMLList "item")
                Core.<*> x Core..@? "groupIpAddress"
                Core.<*> x Core..@? "transitGatewayMulticastDomainId"

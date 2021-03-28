{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastRegisteredGroupMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayMulticastRegisteredGroupMembers
  ( TransitGatewayMulticastRegisteredGroupMembers (..)
  -- * Smart constructor
  , mkTransitGatewayMulticastRegisteredGroupMembers
  -- * Lenses
  , tgmrgmGroupIpAddress
  , tgmrgmRegisteredNetworkInterfaceIds
  , tgmrgmTransitGatewayMulticastDomainId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the registered transit gateway multicast group members.
--
-- /See:/ 'mkTransitGatewayMulticastRegisteredGroupMembers' smart constructor.
data TransitGatewayMulticastRegisteredGroupMembers = TransitGatewayMulticastRegisteredGroupMembers'
  { groupIpAddress :: Core.Maybe Core.Text
    -- ^ The IP address assigned to the transit gateway multicast group.
  , registeredNetworkInterfaceIds :: Core.Maybe [Core.Text]
    -- ^ The ID of the registered network interfaces.
  , transitGatewayMulticastDomainId :: Core.Maybe Core.Text
    -- ^ The ID of the transit gateway multicast domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayMulticastRegisteredGroupMembers' value with any optional fields omitted.
mkTransitGatewayMulticastRegisteredGroupMembers
    :: TransitGatewayMulticastRegisteredGroupMembers
mkTransitGatewayMulticastRegisteredGroupMembers
  = TransitGatewayMulticastRegisteredGroupMembers'{groupIpAddress =
                                                     Core.Nothing,
                                                   registeredNetworkInterfaceIds = Core.Nothing,
                                                   transitGatewayMulticastDomainId = Core.Nothing}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmrgmGroupIpAddress :: Lens.Lens' TransitGatewayMulticastRegisteredGroupMembers (Core.Maybe Core.Text)
tgmrgmGroupIpAddress = Lens.field @"groupIpAddress"
{-# INLINEABLE tgmrgmGroupIpAddress #-}
{-# DEPRECATED groupIpAddress "Use generic-lens or generic-optics with 'groupIpAddress' instead"  #-}

-- | The ID of the registered network interfaces.
--
-- /Note:/ Consider using 'registeredNetworkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmrgmRegisteredNetworkInterfaceIds :: Lens.Lens' TransitGatewayMulticastRegisteredGroupMembers (Core.Maybe [Core.Text])
tgmrgmRegisteredNetworkInterfaceIds = Lens.field @"registeredNetworkInterfaceIds"
{-# INLINEABLE tgmrgmRegisteredNetworkInterfaceIds #-}
{-# DEPRECATED registeredNetworkInterfaceIds "Use generic-lens or generic-optics with 'registeredNetworkInterfaceIds' instead"  #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmrgmTransitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastRegisteredGroupMembers (Core.Maybe Core.Text)
tgmrgmTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# INLINEABLE tgmrgmTransitGatewayMulticastDomainId #-}
{-# DEPRECATED transitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead"  #-}

instance Core.FromXML TransitGatewayMulticastRegisteredGroupMembers
         where
        parseXML x
          = TransitGatewayMulticastRegisteredGroupMembers' Core.<$>
              (x Core..@? "groupIpAddress") Core.<*>
                x Core..@? "registeredNetworkInterfaceIds" Core..<@>
                  Core.parseXMLList "item"
                Core.<*> x Core..@? "transitGatewayMulticastDomainId"

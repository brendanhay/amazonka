{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupMembers
  ( TransitGatewayMulticastDeregisteredGroupMembers (..)
  -- * Smart constructor
  , mkTransitGatewayMulticastDeregisteredGroupMembers
  -- * Lenses
  , tgmdgmDeregisteredNetworkInterfaceIds
  , tgmdgmGroupIpAddress
  , tgmdgmTransitGatewayMulticastDomainId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the deregistered transit gateway multicast group members.
--
-- /See:/ 'mkTransitGatewayMulticastDeregisteredGroupMembers' smart constructor.
data TransitGatewayMulticastDeregisteredGroupMembers = TransitGatewayMulticastDeregisteredGroupMembers'
  { deregisteredNetworkInterfaceIds :: Core.Maybe [Core.Text]
    -- ^ The network interface IDs of the deregistered members.
  , groupIpAddress :: Core.Maybe Core.Text
    -- ^ The IP address assigned to the transit gateway multicast group.
  , transitGatewayMulticastDomainId :: Core.Maybe Core.Text
    -- ^ The ID of the transit gateway multicast domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayMulticastDeregisteredGroupMembers' value with any optional fields omitted.
mkTransitGatewayMulticastDeregisteredGroupMembers
    :: TransitGatewayMulticastDeregisteredGroupMembers
mkTransitGatewayMulticastDeregisteredGroupMembers
  = TransitGatewayMulticastDeregisteredGroupMembers'{deregisteredNetworkInterfaceIds
                                                       = Core.Nothing,
                                                     groupIpAddress = Core.Nothing,
                                                     transitGatewayMulticastDomainId = Core.Nothing}

-- | The network interface IDs of the deregistered members.
--
-- /Note:/ Consider using 'deregisteredNetworkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdgmDeregisteredNetworkInterfaceIds :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupMembers (Core.Maybe [Core.Text])
tgmdgmDeregisteredNetworkInterfaceIds = Lens.field @"deregisteredNetworkInterfaceIds"
{-# INLINEABLE tgmdgmDeregisteredNetworkInterfaceIds #-}
{-# DEPRECATED deregisteredNetworkInterfaceIds "Use generic-lens or generic-optics with 'deregisteredNetworkInterfaceIds' instead"  #-}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdgmGroupIpAddress :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupMembers (Core.Maybe Core.Text)
tgmdgmGroupIpAddress = Lens.field @"groupIpAddress"
{-# INLINEABLE tgmdgmGroupIpAddress #-}
{-# DEPRECATED groupIpAddress "Use generic-lens or generic-optics with 'groupIpAddress' instead"  #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdgmTransitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupMembers (Core.Maybe Core.Text)
tgmdgmTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# INLINEABLE tgmdgmTransitGatewayMulticastDomainId #-}
{-# DEPRECATED transitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead"  #-}

instance Core.FromXML
           TransitGatewayMulticastDeregisteredGroupMembers
         where
        parseXML x
          = TransitGatewayMulticastDeregisteredGroupMembers' Core.<$>
              (x Core..@? "deregisteredNetworkInterfaceIds" Core..<@>
                 Core.parseXMLList "item")
                Core.<*> x Core..@? "groupIpAddress"
                Core.<*> x Core..@? "transitGatewayMulticastDomainId"

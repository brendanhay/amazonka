{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastDeregisteredGroupMembers
  ( TransitGatewayMulticastDeregisteredGroupMembers (..),

    -- * Smart constructor
    mkTransitGatewayMulticastDeregisteredGroupMembers,

    -- * Lenses
    tgmdgmDeregisteredNetworkInterfaceIds,
    tgmdgmGroupIpAddress,
    tgmdgmTransitGatewayMulticastDomainId,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the deregistered transit gateway multicast group members.
--
-- /See:/ 'mkTransitGatewayMulticastDeregisteredGroupMembers' smart constructor.
data TransitGatewayMulticastDeregisteredGroupMembers = TransitGatewayMulticastDeregisteredGroupMembers'
  { -- | The network interface IDs of the deregistered members.
    deregisteredNetworkInterfaceIds :: Core.Maybe [Types.String],
    -- | The IP address assigned to the transit gateway multicast group.
    groupIpAddress :: Core.Maybe Types.String,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TransitGatewayMulticastDeregisteredGroupMembers' value with any optional fields omitted.
mkTransitGatewayMulticastDeregisteredGroupMembers ::
  TransitGatewayMulticastDeregisteredGroupMembers
mkTransitGatewayMulticastDeregisteredGroupMembers =
  TransitGatewayMulticastDeregisteredGroupMembers'
    { deregisteredNetworkInterfaceIds =
        Core.Nothing,
      groupIpAddress = Core.Nothing,
      transitGatewayMulticastDomainId = Core.Nothing
    }

-- | The network interface IDs of the deregistered members.
--
-- /Note:/ Consider using 'deregisteredNetworkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdgmDeregisteredNetworkInterfaceIds :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupMembers (Core.Maybe [Types.String])
tgmdgmDeregisteredNetworkInterfaceIds = Lens.field @"deregisteredNetworkInterfaceIds"
{-# DEPRECATED tgmdgmDeregisteredNetworkInterfaceIds "Use generic-lens or generic-optics with 'deregisteredNetworkInterfaceIds' instead." #-}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdgmGroupIpAddress :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupMembers (Core.Maybe Types.String)
tgmdgmGroupIpAddress = Lens.field @"groupIpAddress"
{-# DEPRECATED tgmdgmGroupIpAddress "Use generic-lens or generic-optics with 'groupIpAddress' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdgmTransitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupMembers (Core.Maybe Types.String)
tgmdgmTransitGatewayMulticastDomainId = Lens.field @"transitGatewayMulticastDomainId"
{-# DEPRECATED tgmdgmTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

instance
  Core.FromXML
    TransitGatewayMulticastDeregisteredGroupMembers
  where
  parseXML x =
    TransitGatewayMulticastDeregisteredGroupMembers'
      Core.<$> ( x Core..@? "deregisteredNetworkInterfaceIds"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "groupIpAddress")
      Core.<*> (x Core..@? "transitGatewayMulticastDomainId")

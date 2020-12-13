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
    tgmdgmTransitGatewayMulticastDomainId,
    tgmdgmGroupIPAddress,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the deregistered transit gateway multicast group members.
--
-- /See:/ 'mkTransitGatewayMulticastDeregisteredGroupMembers' smart constructor.
data TransitGatewayMulticastDeregisteredGroupMembers = TransitGatewayMulticastDeregisteredGroupMembers'
  { -- | The network interface IDs of the deregistered members.
    deregisteredNetworkInterfaceIds :: Lude.Maybe [Lude.Text],
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Lude.Maybe Lude.Text,
    -- | The IP address assigned to the transit gateway multicast group.
    groupIPAddress :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransitGatewayMulticastDeregisteredGroupMembers' with the minimum fields required to make a request.
--
-- * 'deregisteredNetworkInterfaceIds' - The network interface IDs of the deregistered members.
-- * 'transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
-- * 'groupIPAddress' - The IP address assigned to the transit gateway multicast group.
mkTransitGatewayMulticastDeregisteredGroupMembers ::
  TransitGatewayMulticastDeregisteredGroupMembers
mkTransitGatewayMulticastDeregisteredGroupMembers =
  TransitGatewayMulticastDeregisteredGroupMembers'
    { deregisteredNetworkInterfaceIds =
        Lude.Nothing,
      transitGatewayMulticastDomainId = Lude.Nothing,
      groupIPAddress = Lude.Nothing
    }

-- | The network interface IDs of the deregistered members.
--
-- /Note:/ Consider using 'deregisteredNetworkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdgmDeregisteredNetworkInterfaceIds :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupMembers (Lude.Maybe [Lude.Text])
tgmdgmDeregisteredNetworkInterfaceIds = Lens.lens (deregisteredNetworkInterfaceIds :: TransitGatewayMulticastDeregisteredGroupMembers -> Lude.Maybe [Lude.Text]) (\s a -> s {deregisteredNetworkInterfaceIds = a} :: TransitGatewayMulticastDeregisteredGroupMembers)
{-# DEPRECATED tgmdgmDeregisteredNetworkInterfaceIds "Use generic-lens or generic-optics with 'deregisteredNetworkInterfaceIds' instead." #-}

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdgmTransitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupMembers (Lude.Maybe Lude.Text)
tgmdgmTransitGatewayMulticastDomainId = Lens.lens (transitGatewayMulticastDomainId :: TransitGatewayMulticastDeregisteredGroupMembers -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayMulticastDomainId = a} :: TransitGatewayMulticastDeregisteredGroupMembers)
{-# DEPRECATED tgmdgmTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmdgmGroupIPAddress :: Lens.Lens' TransitGatewayMulticastDeregisteredGroupMembers (Lude.Maybe Lude.Text)
tgmdgmGroupIPAddress = Lens.lens (groupIPAddress :: TransitGatewayMulticastDeregisteredGroupMembers -> Lude.Maybe Lude.Text) (\s a -> s {groupIPAddress = a} :: TransitGatewayMulticastDeregisteredGroupMembers)
{-# DEPRECATED tgmdgmGroupIPAddress "Use generic-lens or generic-optics with 'groupIPAddress' instead." #-}

instance
  Lude.FromXML
    TransitGatewayMulticastDeregisteredGroupMembers
  where
  parseXML x =
    TransitGatewayMulticastDeregisteredGroupMembers'
      Lude.<$> ( x Lude..@? "deregisteredNetworkInterfaceIds" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "transitGatewayMulticastDomainId")
      Lude.<*> (x Lude..@? "groupIpAddress")

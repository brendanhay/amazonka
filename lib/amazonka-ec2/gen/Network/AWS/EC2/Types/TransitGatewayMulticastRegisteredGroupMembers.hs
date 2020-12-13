{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastRegisteredGroupMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TransitGatewayMulticastRegisteredGroupMembers
  ( TransitGatewayMulticastRegisteredGroupMembers (..),

    -- * Smart constructor
    mkTransitGatewayMulticastRegisteredGroupMembers,

    -- * Lenses
    tgmrgmTransitGatewayMulticastDomainId,
    tgmrgmRegisteredNetworkInterfaceIds,
    tgmrgmGroupIPAddress,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the registered transit gateway multicast group members.
--
-- /See:/ 'mkTransitGatewayMulticastRegisteredGroupMembers' smart constructor.
data TransitGatewayMulticastRegisteredGroupMembers = TransitGatewayMulticastRegisteredGroupMembers'
  { -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Lude.Maybe Lude.Text,
    -- | The ID of the registered network interfaces.
    registeredNetworkInterfaceIds :: Lude.Maybe [Lude.Text],
    -- | The IP address assigned to the transit gateway multicast group.
    groupIPAddress :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransitGatewayMulticastRegisteredGroupMembers' with the minimum fields required to make a request.
--
-- * 'transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
-- * 'registeredNetworkInterfaceIds' - The ID of the registered network interfaces.
-- * 'groupIPAddress' - The IP address assigned to the transit gateway multicast group.
mkTransitGatewayMulticastRegisteredGroupMembers ::
  TransitGatewayMulticastRegisteredGroupMembers
mkTransitGatewayMulticastRegisteredGroupMembers =
  TransitGatewayMulticastRegisteredGroupMembers'
    { transitGatewayMulticastDomainId =
        Lude.Nothing,
      registeredNetworkInterfaceIds = Lude.Nothing,
      groupIPAddress = Lude.Nothing
    }

-- | The ID of the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmrgmTransitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastRegisteredGroupMembers (Lude.Maybe Lude.Text)
tgmrgmTransitGatewayMulticastDomainId = Lens.lens (transitGatewayMulticastDomainId :: TransitGatewayMulticastRegisteredGroupMembers -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayMulticastDomainId = a} :: TransitGatewayMulticastRegisteredGroupMembers)
{-# DEPRECATED tgmrgmTransitGatewayMulticastDomainId "Use generic-lens or generic-optics with 'transitGatewayMulticastDomainId' instead." #-}

-- | The ID of the registered network interfaces.
--
-- /Note:/ Consider using 'registeredNetworkInterfaceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmrgmRegisteredNetworkInterfaceIds :: Lens.Lens' TransitGatewayMulticastRegisteredGroupMembers (Lude.Maybe [Lude.Text])
tgmrgmRegisteredNetworkInterfaceIds = Lens.lens (registeredNetworkInterfaceIds :: TransitGatewayMulticastRegisteredGroupMembers -> Lude.Maybe [Lude.Text]) (\s a -> s {registeredNetworkInterfaceIds = a} :: TransitGatewayMulticastRegisteredGroupMembers)
{-# DEPRECATED tgmrgmRegisteredNetworkInterfaceIds "Use generic-lens or generic-optics with 'registeredNetworkInterfaceIds' instead." #-}

-- | The IP address assigned to the transit gateway multicast group.
--
-- /Note:/ Consider using 'groupIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgmrgmGroupIPAddress :: Lens.Lens' TransitGatewayMulticastRegisteredGroupMembers (Lude.Maybe Lude.Text)
tgmrgmGroupIPAddress = Lens.lens (groupIPAddress :: TransitGatewayMulticastRegisteredGroupMembers -> Lude.Maybe Lude.Text) (\s a -> s {groupIPAddress = a} :: TransitGatewayMulticastRegisteredGroupMembers)
{-# DEPRECATED tgmrgmGroupIPAddress "Use generic-lens or generic-optics with 'groupIPAddress' instead." #-}

instance Lude.FromXML TransitGatewayMulticastRegisteredGroupMembers where
  parseXML x =
    TransitGatewayMulticastRegisteredGroupMembers'
      Lude.<$> (x Lude..@? "transitGatewayMulticastDomainId")
      Lude.<*> ( x Lude..@? "registeredNetworkInterfaceIds" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "groupIpAddress")

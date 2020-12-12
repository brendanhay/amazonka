{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVPNRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVPNRoute
  ( ClientVPNRoute (..),

    -- * Smart constructor
    mkClientVPNRoute,

    -- * Lenses
    cvrStatus,
    cvrOrigin,
    cvrClientVPNEndpointId,
    cvrTargetSubnet,
    cvrDestinationCidr,
    cvrType,
    cvrDescription,
  )
where

import Network.AWS.EC2.Types.ClientVPNRouteStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a Client VPN endpoint route.
--
-- /See:/ 'mkClientVPNRoute' smart constructor.
data ClientVPNRoute = ClientVPNRoute'
  { status ::
      Lude.Maybe ClientVPNRouteStatus,
    origin :: Lude.Maybe Lude.Text,
    clientVPNEndpointId :: Lude.Maybe Lude.Text,
    targetSubnet :: Lude.Maybe Lude.Text,
    destinationCidr :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClientVPNRoute' with the minimum fields required to make a request.
--
-- * 'clientVPNEndpointId' - The ID of the Client VPN endpoint with which the route is associated.
-- * 'description' - A brief description of the route.
-- * 'destinationCidr' - The IPv4 address range, in CIDR notation, of the route destination.
-- * 'origin' - Indicates how the route was associated with the Client VPN endpoint. @associate@ indicates that the route was automatically added when the target network was associated with the Client VPN endpoint. @add-route@ indicates that the route was manually added using the __CreateClientVpnRoute__ action.
-- * 'status' - The current state of the route.
-- * 'targetSubnet' - The ID of the subnet through which traffic is routed.
-- * 'type'' - The route type.
mkClientVPNRoute ::
  ClientVPNRoute
mkClientVPNRoute =
  ClientVPNRoute'
    { status = Lude.Nothing,
      origin = Lude.Nothing,
      clientVPNEndpointId = Lude.Nothing,
      targetSubnet = Lude.Nothing,
      destinationCidr = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The current state of the route.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrStatus :: Lens.Lens' ClientVPNRoute (Lude.Maybe ClientVPNRouteStatus)
cvrStatus = Lens.lens (status :: ClientVPNRoute -> Lude.Maybe ClientVPNRouteStatus) (\s a -> s {status = a} :: ClientVPNRoute)
{-# DEPRECATED cvrStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Indicates how the route was associated with the Client VPN endpoint. @associate@ indicates that the route was automatically added when the target network was associated with the Client VPN endpoint. @add-route@ indicates that the route was manually added using the __CreateClientVpnRoute__ action.
--
-- /Note:/ Consider using 'origin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrOrigin :: Lens.Lens' ClientVPNRoute (Lude.Maybe Lude.Text)
cvrOrigin = Lens.lens (origin :: ClientVPNRoute -> Lude.Maybe Lude.Text) (\s a -> s {origin = a} :: ClientVPNRoute)
{-# DEPRECATED cvrOrigin "Use generic-lens or generic-optics with 'origin' instead." #-}

-- | The ID of the Client VPN endpoint with which the route is associated.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrClientVPNEndpointId :: Lens.Lens' ClientVPNRoute (Lude.Maybe Lude.Text)
cvrClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: ClientVPNRoute -> Lude.Maybe Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: ClientVPNRoute)
{-# DEPRECATED cvrClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

-- | The ID of the subnet through which traffic is routed.
--
-- /Note:/ Consider using 'targetSubnet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrTargetSubnet :: Lens.Lens' ClientVPNRoute (Lude.Maybe Lude.Text)
cvrTargetSubnet = Lens.lens (targetSubnet :: ClientVPNRoute -> Lude.Maybe Lude.Text) (\s a -> s {targetSubnet = a} :: ClientVPNRoute)
{-# DEPRECATED cvrTargetSubnet "Use generic-lens or generic-optics with 'targetSubnet' instead." #-}

-- | The IPv4 address range, in CIDR notation, of the route destination.
--
-- /Note:/ Consider using 'destinationCidr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrDestinationCidr :: Lens.Lens' ClientVPNRoute (Lude.Maybe Lude.Text)
cvrDestinationCidr = Lens.lens (destinationCidr :: ClientVPNRoute -> Lude.Maybe Lude.Text) (\s a -> s {destinationCidr = a} :: ClientVPNRoute)
{-# DEPRECATED cvrDestinationCidr "Use generic-lens or generic-optics with 'destinationCidr' instead." #-}

-- | The route type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrType :: Lens.Lens' ClientVPNRoute (Lude.Maybe Lude.Text)
cvrType = Lens.lens (type' :: ClientVPNRoute -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: ClientVPNRoute)
{-# DEPRECATED cvrType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A brief description of the route.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvrDescription :: Lens.Lens' ClientVPNRoute (Lude.Maybe Lude.Text)
cvrDescription = Lens.lens (description :: ClientVPNRoute -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ClientVPNRoute)
{-# DEPRECATED cvrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML ClientVPNRoute where
  parseXML x =
    ClientVPNRoute'
      Lude.<$> (x Lude..@? "status")
      Lude.<*> (x Lude..@? "origin")
      Lude.<*> (x Lude..@? "clientVpnEndpointId")
      Lude.<*> (x Lude..@? "targetSubnet")
      Lude.<*> (x Lude..@? "destinationCidr")
      Lude.<*> (x Lude..@? "type")
      Lude.<*> (x Lude..@? "description")

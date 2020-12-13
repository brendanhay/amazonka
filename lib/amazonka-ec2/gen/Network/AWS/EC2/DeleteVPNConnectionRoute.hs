{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVPNConnectionRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified static route associated with a VPN connection between an existing virtual private gateway and a VPN customer gateway. The static route allows traffic to be routed from the virtual private gateway to the VPN customer gateway.
module Network.AWS.EC2.DeleteVPNConnectionRoute
  ( -- * Creating a request
    DeleteVPNConnectionRoute (..),
    mkDeleteVPNConnectionRoute,

    -- ** Request lenses
    dvcrVPNConnectionId,
    dvcrDestinationCidrBlock,

    -- * Destructuring the response
    DeleteVPNConnectionRouteResponse (..),
    mkDeleteVPNConnectionRouteResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DeleteVpnConnectionRoute.
--
-- /See:/ 'mkDeleteVPNConnectionRoute' smart constructor.
data DeleteVPNConnectionRoute = DeleteVPNConnectionRoute'
  { -- | The ID of the VPN connection.
    vpnConnectionId :: Lude.Text,
    -- | The CIDR block associated with the local subnet of the customer network.
    destinationCidrBlock :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPNConnectionRoute' with the minimum fields required to make a request.
--
-- * 'vpnConnectionId' - The ID of the VPN connection.
-- * 'destinationCidrBlock' - The CIDR block associated with the local subnet of the customer network.
mkDeleteVPNConnectionRoute ::
  -- | 'vpnConnectionId'
  Lude.Text ->
  -- | 'destinationCidrBlock'
  Lude.Text ->
  DeleteVPNConnectionRoute
mkDeleteVPNConnectionRoute pVPNConnectionId_ pDestinationCidrBlock_ =
  DeleteVPNConnectionRoute'
    { vpnConnectionId = pVPNConnectionId_,
      destinationCidrBlock = pDestinationCidrBlock_
    }

-- | The ID of the VPN connection.
--
-- /Note:/ Consider using 'vpnConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcrVPNConnectionId :: Lens.Lens' DeleteVPNConnectionRoute Lude.Text
dvcrVPNConnectionId = Lens.lens (vpnConnectionId :: DeleteVPNConnectionRoute -> Lude.Text) (\s a -> s {vpnConnectionId = a} :: DeleteVPNConnectionRoute)
{-# DEPRECATED dvcrVPNConnectionId "Use generic-lens or generic-optics with 'vpnConnectionId' instead." #-}

-- | The CIDR block associated with the local subnet of the customer network.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcrDestinationCidrBlock :: Lens.Lens' DeleteVPNConnectionRoute Lude.Text
dvcrDestinationCidrBlock = Lens.lens (destinationCidrBlock :: DeleteVPNConnectionRoute -> Lude.Text) (\s a -> s {destinationCidrBlock = a} :: DeleteVPNConnectionRoute)
{-# DEPRECATED dvcrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

instance Lude.AWSRequest DeleteVPNConnectionRoute where
  type Rs DeleteVPNConnectionRoute = DeleteVPNConnectionRouteResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DeleteVPNConnectionRouteResponse'

instance Lude.ToHeaders DeleteVPNConnectionRoute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteVPNConnectionRoute where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteVPNConnectionRoute where
  toQuery DeleteVPNConnectionRoute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteVpnConnectionRoute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VpnConnectionId" Lude.=: vpnConnectionId,
        "DestinationCidrBlock" Lude.=: destinationCidrBlock
      ]

-- | /See:/ 'mkDeleteVPNConnectionRouteResponse' smart constructor.
data DeleteVPNConnectionRouteResponse = DeleteVPNConnectionRouteResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPNConnectionRouteResponse' with the minimum fields required to make a request.
mkDeleteVPNConnectionRouteResponse ::
  DeleteVPNConnectionRouteResponse
mkDeleteVPNConnectionRouteResponse =
  DeleteVPNConnectionRouteResponse'

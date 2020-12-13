{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVPNConnectionRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a static route associated with a VPN connection between an existing virtual private gateway and a VPN customer gateway. The static route allows traffic to be routed from the virtual private gateway to the VPN customer gateway.
--
-- For more information, see <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html AWS Site-to-Site VPN> in the /AWS Site-to-Site VPN User Guide/ .
module Network.AWS.EC2.CreateVPNConnectionRoute
  ( -- * Creating a request
    CreateVPNConnectionRoute (..),
    mkCreateVPNConnectionRoute,

    -- ** Request lenses
    cvcrVPNConnectionId,
    cvcrDestinationCidrBlock,

    -- * Destructuring the response
    CreateVPNConnectionRouteResponse (..),
    mkCreateVPNConnectionRouteResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for CreateVpnConnectionRoute.
--
-- /See:/ 'mkCreateVPNConnectionRoute' smart constructor.
data CreateVPNConnectionRoute = CreateVPNConnectionRoute'
  { -- | The ID of the VPN connection.
    vpnConnectionId :: Lude.Text,
    -- | The CIDR block associated with the local subnet of the customer network.
    destinationCidrBlock :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVPNConnectionRoute' with the minimum fields required to make a request.
--
-- * 'vpnConnectionId' - The ID of the VPN connection.
-- * 'destinationCidrBlock' - The CIDR block associated with the local subnet of the customer network.
mkCreateVPNConnectionRoute ::
  -- | 'vpnConnectionId'
  Lude.Text ->
  -- | 'destinationCidrBlock'
  Lude.Text ->
  CreateVPNConnectionRoute
mkCreateVPNConnectionRoute pVPNConnectionId_ pDestinationCidrBlock_ =
  CreateVPNConnectionRoute'
    { vpnConnectionId = pVPNConnectionId_,
      destinationCidrBlock = pDestinationCidrBlock_
    }

-- | The ID of the VPN connection.
--
-- /Note:/ Consider using 'vpnConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcrVPNConnectionId :: Lens.Lens' CreateVPNConnectionRoute Lude.Text
cvcrVPNConnectionId = Lens.lens (vpnConnectionId :: CreateVPNConnectionRoute -> Lude.Text) (\s a -> s {vpnConnectionId = a} :: CreateVPNConnectionRoute)
{-# DEPRECATED cvcrVPNConnectionId "Use generic-lens or generic-optics with 'vpnConnectionId' instead." #-}

-- | The CIDR block associated with the local subnet of the customer network.
--
-- /Note:/ Consider using 'destinationCidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvcrDestinationCidrBlock :: Lens.Lens' CreateVPNConnectionRoute Lude.Text
cvcrDestinationCidrBlock = Lens.lens (destinationCidrBlock :: CreateVPNConnectionRoute -> Lude.Text) (\s a -> s {destinationCidrBlock = a} :: CreateVPNConnectionRoute)
{-# DEPRECATED cvcrDestinationCidrBlock "Use generic-lens or generic-optics with 'destinationCidrBlock' instead." #-}

instance Lude.AWSRequest CreateVPNConnectionRoute where
  type Rs CreateVPNConnectionRoute = CreateVPNConnectionRouteResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull CreateVPNConnectionRouteResponse'

instance Lude.ToHeaders CreateVPNConnectionRoute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateVPNConnectionRoute where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateVPNConnectionRoute where
  toQuery CreateVPNConnectionRoute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateVpnConnectionRoute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VpnConnectionId" Lude.=: vpnConnectionId,
        "DestinationCidrBlock" Lude.=: destinationCidrBlock
      ]

-- | /See:/ 'mkCreateVPNConnectionRouteResponse' smart constructor.
data CreateVPNConnectionRouteResponse = CreateVPNConnectionRouteResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVPNConnectionRouteResponse' with the minimum fields required to make a request.
mkCreateVPNConnectionRouteResponse ::
  CreateVPNConnectionRouteResponse
mkCreateVPNConnectionRouteResponse =
  CreateVPNConnectionRouteResponse'

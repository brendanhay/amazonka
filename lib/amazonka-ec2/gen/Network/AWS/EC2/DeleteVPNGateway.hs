{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVPNGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified virtual private gateway. You must first detach the virtual private gateway from the VPC. Note that you don't need to delete the virtual private gateway if you plan to delete and recreate the VPN connection between your VPC and your network.
module Network.AWS.EC2.DeleteVPNGateway
  ( -- * Creating a request
    DeleteVPNGateway (..),
    mkDeleteVPNGateway,

    -- ** Request lenses
    dvgDryRun,
    dvgVPNGatewayId,

    -- * Destructuring the response
    DeleteVPNGatewayResponse (..),
    mkDeleteVPNGatewayResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DeleteVpnGateway.
--
-- /See:/ 'mkDeleteVPNGateway' smart constructor.
data DeleteVPNGateway = DeleteVPNGateway'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    vpnGatewayId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPNGateway' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'vpnGatewayId' - The ID of the virtual private gateway.
mkDeleteVPNGateway ::
  -- | 'vpnGatewayId'
  Lude.Text ->
  DeleteVPNGateway
mkDeleteVPNGateway pVPNGatewayId_ =
  DeleteVPNGateway'
    { dryRun = Lude.Nothing,
      vpnGatewayId = pVPNGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvgDryRun :: Lens.Lens' DeleteVPNGateway (Lude.Maybe Lude.Bool)
dvgDryRun = Lens.lens (dryRun :: DeleteVPNGateway -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteVPNGateway)
{-# DEPRECATED dvgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'vpnGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvgVPNGatewayId :: Lens.Lens' DeleteVPNGateway Lude.Text
dvgVPNGatewayId = Lens.lens (vpnGatewayId :: DeleteVPNGateway -> Lude.Text) (\s a -> s {vpnGatewayId = a} :: DeleteVPNGateway)
{-# DEPRECATED dvgVPNGatewayId "Use generic-lens or generic-optics with 'vpnGatewayId' instead." #-}

instance Lude.AWSRequest DeleteVPNGateway where
  type Rs DeleteVPNGateway = DeleteVPNGatewayResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DeleteVPNGatewayResponse'

instance Lude.ToHeaders DeleteVPNGateway where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteVPNGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteVPNGateway where
  toQuery DeleteVPNGateway' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteVpnGateway" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "VpnGatewayId" Lude.=: vpnGatewayId
      ]

-- | /See:/ 'mkDeleteVPNGatewayResponse' smart constructor.
data DeleteVPNGatewayResponse = DeleteVPNGatewayResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPNGatewayResponse' with the minimum fields required to make a request.
mkDeleteVPNGatewayResponse ::
  DeleteVPNGatewayResponse
mkDeleteVPNGatewayResponse = DeleteVPNGatewayResponse'

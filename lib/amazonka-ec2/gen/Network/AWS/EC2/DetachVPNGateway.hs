{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DetachVPNGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a virtual private gateway from a VPC. You do this if you're planning to turn off the VPC and not use it anymore. You can confirm a virtual private gateway has been completely detached from a VPC by describing the virtual private gateway (any attachments to the virtual private gateway are also described).
--
-- You must wait for the attachment's state to switch to @detached@ before you can delete the VPC or attach a different VPC to the virtual private gateway.
module Network.AWS.EC2.DetachVPNGateway
  ( -- * Creating a request
    DetachVPNGateway (..),
    mkDetachVPNGateway,

    -- ** Request lenses
    dvpngVPNGatewayId,
    dvpngVPCId,
    dvpngDryRun,

    -- * Destructuring the response
    DetachVPNGatewayResponse (..),
    mkDetachVPNGatewayResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DetachVpnGateway.
--
-- /See:/ 'mkDetachVPNGateway' smart constructor.
data DetachVPNGateway = DetachVPNGateway'
  { -- | The ID of the virtual private gateway.
    vpnGatewayId :: Lude.Text,
    -- | The ID of the VPC.
    vpcId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachVPNGateway' with the minimum fields required to make a request.
--
-- * 'vpnGatewayId' - The ID of the virtual private gateway.
-- * 'vpcId' - The ID of the VPC.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDetachVPNGateway ::
  -- | 'vpnGatewayId'
  Lude.Text ->
  -- | 'vpcId'
  Lude.Text ->
  DetachVPNGateway
mkDetachVPNGateway pVPNGatewayId_ pVPCId_ =
  DetachVPNGateway'
    { vpnGatewayId = pVPNGatewayId_,
      vpcId = pVPCId_,
      dryRun = Lude.Nothing
    }

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'vpnGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpngVPNGatewayId :: Lens.Lens' DetachVPNGateway Lude.Text
dvpngVPNGatewayId = Lens.lens (vpnGatewayId :: DetachVPNGateway -> Lude.Text) (\s a -> s {vpnGatewayId = a} :: DetachVPNGateway)
{-# DEPRECATED dvpngVPNGatewayId "Use generic-lens or generic-optics with 'vpnGatewayId' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpngVPCId :: Lens.Lens' DetachVPNGateway Lude.Text
dvpngVPCId = Lens.lens (vpcId :: DetachVPNGateway -> Lude.Text) (\s a -> s {vpcId = a} :: DetachVPNGateway)
{-# DEPRECATED dvpngVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpngDryRun :: Lens.Lens' DetachVPNGateway (Lude.Maybe Lude.Bool)
dvpngDryRun = Lens.lens (dryRun :: DetachVPNGateway -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DetachVPNGateway)
{-# DEPRECATED dvpngDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DetachVPNGateway where
  type Rs DetachVPNGateway = DetachVPNGatewayResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DetachVPNGatewayResponse'

instance Lude.ToHeaders DetachVPNGateway where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DetachVPNGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery DetachVPNGateway where
  toQuery DetachVPNGateway' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DetachVpnGateway" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VpnGatewayId" Lude.=: vpnGatewayId,
        "VpcId" Lude.=: vpcId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDetachVPNGatewayResponse' smart constructor.
data DetachVPNGatewayResponse = DetachVPNGatewayResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachVPNGatewayResponse' with the minimum fields required to make a request.
mkDetachVPNGatewayResponse ::
  DetachVPNGatewayResponse
mkDetachVPNGatewayResponse = DetachVPNGatewayResponse'

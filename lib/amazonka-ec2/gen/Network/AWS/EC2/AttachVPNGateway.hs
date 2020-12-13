{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AttachVPNGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a virtual private gateway to a VPC. You can attach one virtual private gateway to one VPC at a time.
--
-- For more information, see <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html AWS Site-to-Site VPN> in the /AWS Site-to-Site VPN User Guide/ .
module Network.AWS.EC2.AttachVPNGateway
  ( -- * Creating a request
    AttachVPNGateway (..),
    mkAttachVPNGateway,

    -- ** Request lenses
    avgVPNGatewayId,
    avgVPCId,
    avgDryRun,

    -- * Destructuring the response
    AttachVPNGatewayResponse (..),
    mkAttachVPNGatewayResponse,

    -- ** Response lenses
    avgrsVPCAttachment,
    avgrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for AttachVpnGateway.
--
-- /See:/ 'mkAttachVPNGateway' smart constructor.
data AttachVPNGateway = AttachVPNGateway'
  { -- | The ID of the virtual private gateway.
    vpnGatewayId :: Lude.Text,
    -- | The ID of the VPC.
    vpcId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachVPNGateway' with the minimum fields required to make a request.
--
-- * 'vpnGatewayId' - The ID of the virtual private gateway.
-- * 'vpcId' - The ID of the VPC.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkAttachVPNGateway ::
  -- | 'vpnGatewayId'
  Lude.Text ->
  -- | 'vpcId'
  Lude.Text ->
  AttachVPNGateway
mkAttachVPNGateway pVPNGatewayId_ pVPCId_ =
  AttachVPNGateway'
    { vpnGatewayId = pVPNGatewayId_,
      vpcId = pVPCId_,
      dryRun = Lude.Nothing
    }

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'vpnGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avgVPNGatewayId :: Lens.Lens' AttachVPNGateway Lude.Text
avgVPNGatewayId = Lens.lens (vpnGatewayId :: AttachVPNGateway -> Lude.Text) (\s a -> s {vpnGatewayId = a} :: AttachVPNGateway)
{-# DEPRECATED avgVPNGatewayId "Use generic-lens or generic-optics with 'vpnGatewayId' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avgVPCId :: Lens.Lens' AttachVPNGateway Lude.Text
avgVPCId = Lens.lens (vpcId :: AttachVPNGateway -> Lude.Text) (\s a -> s {vpcId = a} :: AttachVPNGateway)
{-# DEPRECATED avgVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avgDryRun :: Lens.Lens' AttachVPNGateway (Lude.Maybe Lude.Bool)
avgDryRun = Lens.lens (dryRun :: AttachVPNGateway -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AttachVPNGateway)
{-# DEPRECATED avgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest AttachVPNGateway where
  type Rs AttachVPNGateway = AttachVPNGatewayResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          AttachVPNGatewayResponse'
            Lude.<$> (x Lude..@? "attachment") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AttachVPNGateway where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AttachVPNGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery AttachVPNGateway where
  toQuery AttachVPNGateway' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AttachVpnGateway" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VpnGatewayId" Lude.=: vpnGatewayId,
        "VpcId" Lude.=: vpcId,
        "DryRun" Lude.=: dryRun
      ]

-- | Contains the output of AttachVpnGateway.
--
-- /See:/ 'mkAttachVPNGatewayResponse' smart constructor.
data AttachVPNGatewayResponse = AttachVPNGatewayResponse'
  { -- | Information about the attachment.
    vpcAttachment :: Lude.Maybe VPCAttachment,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachVPNGatewayResponse' with the minimum fields required to make a request.
--
-- * 'vpcAttachment' - Information about the attachment.
-- * 'responseStatus' - The response status code.
mkAttachVPNGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AttachVPNGatewayResponse
mkAttachVPNGatewayResponse pResponseStatus_ =
  AttachVPNGatewayResponse'
    { vpcAttachment = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the attachment.
--
-- /Note:/ Consider using 'vpcAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avgrsVPCAttachment :: Lens.Lens' AttachVPNGatewayResponse (Lude.Maybe VPCAttachment)
avgrsVPCAttachment = Lens.lens (vpcAttachment :: AttachVPNGatewayResponse -> Lude.Maybe VPCAttachment) (\s a -> s {vpcAttachment = a} :: AttachVPNGatewayResponse)
{-# DEPRECATED avgrsVPCAttachment "Use generic-lens or generic-optics with 'vpcAttachment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avgrsResponseStatus :: Lens.Lens' AttachVPNGatewayResponse Lude.Int
avgrsResponseStatus = Lens.lens (responseStatus :: AttachVPNGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AttachVPNGatewayResponse)
{-# DEPRECATED avgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

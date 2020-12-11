{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVPNGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a virtual private gateway. A virtual private gateway is the endpoint on the VPC side of your VPN connection. You can create a virtual private gateway before creating the VPC itself.
--
-- For more information, see <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html AWS Site-to-Site VPN> in the /AWS Site-to-Site VPN User Guide/ .
module Network.AWS.EC2.CreateVPNGateway
  ( -- * Creating a request
    CreateVPNGateway (..),
    mkCreateVPNGateway,

    -- ** Request lenses
    cvgAmazonSideASN,
    cvgTagSpecifications,
    cvgAvailabilityZone,
    cvgDryRun,
    cvgType,

    -- * Destructuring the response
    CreateVPNGatewayResponse (..),
    mkCreateVPNGatewayResponse,

    -- ** Response lenses
    cvgrsVPNGateway,
    cvgrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for CreateVpnGateway.
--
-- /See:/ 'mkCreateVPNGateway' smart constructor.
data CreateVPNGateway = CreateVPNGateway'
  { amazonSideASN ::
      Lude.Maybe Lude.Integer,
    tagSpecifications :: Lude.Maybe [TagSpecification],
    availabilityZone :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    type' :: GatewayType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVPNGateway' with the minimum fields required to make a request.
--
-- * 'amazonSideASN' - A private Autonomous System Number (ASN) for the Amazon side of a BGP session. If you're using a 16-bit ASN, it must be in the 64512 to 65534 range. If you're using a 32-bit ASN, it must be in the 4200000000 to 4294967294 range.
--
-- Default: 64512
-- * 'availabilityZone' - The Availability Zone for the virtual private gateway.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'tagSpecifications' - The tags to apply to the virtual private gateway.
-- * 'type'' - The type of VPN connection this virtual private gateway supports.
mkCreateVPNGateway ::
  -- | 'type''
  GatewayType ->
  CreateVPNGateway
mkCreateVPNGateway pType_ =
  CreateVPNGateway'
    { amazonSideASN = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      dryRun = Lude.Nothing,
      type' = pType_
    }

-- | A private Autonomous System Number (ASN) for the Amazon side of a BGP session. If you're using a 16-bit ASN, it must be in the 64512 to 65534 range. If you're using a 32-bit ASN, it must be in the 4200000000 to 4294967294 range.
--
-- Default: 64512
--
-- /Note:/ Consider using 'amazonSideASN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvgAmazonSideASN :: Lens.Lens' CreateVPNGateway (Lude.Maybe Lude.Integer)
cvgAmazonSideASN = Lens.lens (amazonSideASN :: CreateVPNGateway -> Lude.Maybe Lude.Integer) (\s a -> s {amazonSideASN = a} :: CreateVPNGateway)
{-# DEPRECATED cvgAmazonSideASN "Use generic-lens or generic-optics with 'amazonSideASN' instead." #-}

-- | The tags to apply to the virtual private gateway.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvgTagSpecifications :: Lens.Lens' CreateVPNGateway (Lude.Maybe [TagSpecification])
cvgTagSpecifications = Lens.lens (tagSpecifications :: CreateVPNGateway -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateVPNGateway)
{-# DEPRECATED cvgTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The Availability Zone for the virtual private gateway.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvgAvailabilityZone :: Lens.Lens' CreateVPNGateway (Lude.Maybe Lude.Text)
cvgAvailabilityZone = Lens.lens (availabilityZone :: CreateVPNGateway -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: CreateVPNGateway)
{-# DEPRECATED cvgAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvgDryRun :: Lens.Lens' CreateVPNGateway (Lude.Maybe Lude.Bool)
cvgDryRun = Lens.lens (dryRun :: CreateVPNGateway -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateVPNGateway)
{-# DEPRECATED cvgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The type of VPN connection this virtual private gateway supports.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvgType :: Lens.Lens' CreateVPNGateway GatewayType
cvgType = Lens.lens (type' :: CreateVPNGateway -> GatewayType) (\s a -> s {type' = a} :: CreateVPNGateway)
{-# DEPRECATED cvgType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.AWSRequest CreateVPNGateway where
  type Rs CreateVPNGateway = CreateVPNGatewayResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateVPNGatewayResponse'
            Lude.<$> (x Lude..@? "vpnGateway") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateVPNGateway where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateVPNGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateVPNGateway where
  toQuery CreateVPNGateway' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateVpnGateway" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "AmazonSideAsn" Lude.=: amazonSideASN,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "AvailabilityZone" Lude.=: availabilityZone,
        "DryRun" Lude.=: dryRun,
        "Type" Lude.=: type'
      ]

-- | Contains the output of CreateVpnGateway.
--
-- /See:/ 'mkCreateVPNGatewayResponse' smart constructor.
data CreateVPNGatewayResponse = CreateVPNGatewayResponse'
  { vpnGateway ::
      Lude.Maybe VPNGateway,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVPNGatewayResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'vpnGateway' - Information about the virtual private gateway.
mkCreateVPNGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateVPNGatewayResponse
mkCreateVPNGatewayResponse pResponseStatus_ =
  CreateVPNGatewayResponse'
    { vpnGateway = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the virtual private gateway.
--
-- /Note:/ Consider using 'vpnGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvgrsVPNGateway :: Lens.Lens' CreateVPNGatewayResponse (Lude.Maybe VPNGateway)
cvgrsVPNGateway = Lens.lens (vpnGateway :: CreateVPNGatewayResponse -> Lude.Maybe VPNGateway) (\s a -> s {vpnGateway = a} :: CreateVPNGatewayResponse)
{-# DEPRECATED cvgrsVPNGateway "Use generic-lens or generic-optics with 'vpnGateway' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvgrsResponseStatus :: Lens.Lens' CreateVPNGatewayResponse Lude.Int
cvgrsResponseStatus = Lens.lens (responseStatus :: CreateVPNGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateVPNGatewayResponse)
{-# DEPRECATED cvgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

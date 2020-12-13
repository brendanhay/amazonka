{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateCustomerGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information to AWS about your VPN customer gateway device. The customer gateway is the appliance at your end of the VPN connection. (The device on the AWS side of the VPN connection is the virtual private gateway.) You must provide the internet-routable IP address of the customer gateway's external interface. The IP address must be static and can be behind a device performing network address translation (NAT).
--
-- For devices that use Border Gateway Protocol (BGP), you can also provide the device's BGP Autonomous System Number (ASN). You can use an existing ASN assigned to your network. If you don't have an ASN already, you can use a private ASN (in the 64512 - 65534 range).
-- For more information, see <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html AWS Site-to-Site VPN> in the /AWS Site-to-Site VPN User Guide/ .
-- /Important:/ To create more than one customer gateway with the same VPN type, IP address, and BGP ASN, specify a unique device name for each customer gateway. Identical requests return information about the existing customer gateway and do not create new customer gateways.
module Network.AWS.EC2.CreateCustomerGateway
  ( -- * Creating a request
    CreateCustomerGateway (..),
    mkCreateCustomerGateway,

    -- ** Request lenses
    ccgfCertificateARN,
    ccgfBGPASN,
    ccgfTagSpecifications,
    ccgfDeviceName,
    ccgfType,
    ccgfPublicIP,
    ccgfDryRun,

    -- * Destructuring the response
    CreateCustomerGatewayResponse (..),
    mkCreateCustomerGatewayResponse,

    -- ** Response lenses
    ccgrsCustomerGateway,
    ccgrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for CreateCustomerGateway.
--
-- /See:/ 'mkCreateCustomerGateway' smart constructor.
data CreateCustomerGateway = CreateCustomerGateway'
  { -- | The Amazon Resource Name (ARN) for the customer gateway certificate.
    certificateARN :: Lude.Maybe Lude.Text,
    -- | For devices that support BGP, the customer gateway's BGP ASN.
    --
    -- Default: 65000
    bgpASN :: Lude.Int,
    -- | The tags to apply to the customer gateway.
    tagSpecifications :: Lude.Maybe [TagSpecification],
    -- | A name for the customer gateway device.
    --
    -- Length Constraints: Up to 255 characters.
    deviceName :: Lude.Maybe Lude.Text,
    -- | The type of VPN connection that this customer gateway supports (@ipsec.1@ ).
    type' :: GatewayType,
    -- | The Internet-routable IP address for the customer gateway's outside interface. The address must be static.
    publicIP :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCustomerGateway' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The Amazon Resource Name (ARN) for the customer gateway certificate.
-- * 'bgpASN' - For devices that support BGP, the customer gateway's BGP ASN.
--
-- Default: 65000
-- * 'tagSpecifications' - The tags to apply to the customer gateway.
-- * 'deviceName' - A name for the customer gateway device.
--
-- Length Constraints: Up to 255 characters.
-- * 'type'' - The type of VPN connection that this customer gateway supports (@ipsec.1@ ).
-- * 'publicIP' - The Internet-routable IP address for the customer gateway's outside interface. The address must be static.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkCreateCustomerGateway ::
  -- | 'bgpASN'
  Lude.Int ->
  -- | 'type''
  GatewayType ->
  CreateCustomerGateway
mkCreateCustomerGateway pBGPASN_ pType_ =
  CreateCustomerGateway'
    { certificateARN = Lude.Nothing,
      bgpASN = pBGPASN_,
      tagSpecifications = Lude.Nothing,
      deviceName = Lude.Nothing,
      type' = pType_,
      publicIP = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the customer gateway certificate.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgfCertificateARN :: Lens.Lens' CreateCustomerGateway (Lude.Maybe Lude.Text)
ccgfCertificateARN = Lens.lens (certificateARN :: CreateCustomerGateway -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: CreateCustomerGateway)
{-# DEPRECATED ccgfCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | For devices that support BGP, the customer gateway's BGP ASN.
--
-- Default: 65000
--
-- /Note:/ Consider using 'bgpASN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgfBGPASN :: Lens.Lens' CreateCustomerGateway Lude.Int
ccgfBGPASN = Lens.lens (bgpASN :: CreateCustomerGateway -> Lude.Int) (\s a -> s {bgpASN = a} :: CreateCustomerGateway)
{-# DEPRECATED ccgfBGPASN "Use generic-lens or generic-optics with 'bgpASN' instead." #-}

-- | The tags to apply to the customer gateway.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgfTagSpecifications :: Lens.Lens' CreateCustomerGateway (Lude.Maybe [TagSpecification])
ccgfTagSpecifications = Lens.lens (tagSpecifications :: CreateCustomerGateway -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateCustomerGateway)
{-# DEPRECATED ccgfTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | A name for the customer gateway device.
--
-- Length Constraints: Up to 255 characters.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgfDeviceName :: Lens.Lens' CreateCustomerGateway (Lude.Maybe Lude.Text)
ccgfDeviceName = Lens.lens (deviceName :: CreateCustomerGateway -> Lude.Maybe Lude.Text) (\s a -> s {deviceName = a} :: CreateCustomerGateway)
{-# DEPRECATED ccgfDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

-- | The type of VPN connection that this customer gateway supports (@ipsec.1@ ).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgfType :: Lens.Lens' CreateCustomerGateway GatewayType
ccgfType = Lens.lens (type' :: CreateCustomerGateway -> GatewayType) (\s a -> s {type' = a} :: CreateCustomerGateway)
{-# DEPRECATED ccgfType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The Internet-routable IP address for the customer gateway's outside interface. The address must be static.
--
-- /Note:/ Consider using 'publicIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgfPublicIP :: Lens.Lens' CreateCustomerGateway (Lude.Maybe Lude.Text)
ccgfPublicIP = Lens.lens (publicIP :: CreateCustomerGateway -> Lude.Maybe Lude.Text) (\s a -> s {publicIP = a} :: CreateCustomerGateway)
{-# DEPRECATED ccgfPublicIP "Use generic-lens or generic-optics with 'publicIP' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgfDryRun :: Lens.Lens' CreateCustomerGateway (Lude.Maybe Lude.Bool)
ccgfDryRun = Lens.lens (dryRun :: CreateCustomerGateway -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateCustomerGateway)
{-# DEPRECATED ccgfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest CreateCustomerGateway where
  type Rs CreateCustomerGateway = CreateCustomerGatewayResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateCustomerGatewayResponse'
            Lude.<$> (x Lude..@? "customerGateway")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCustomerGateway where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateCustomerGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCustomerGateway where
  toQuery CreateCustomerGateway' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateCustomerGateway" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "CertificateArn" Lude.=: certificateARN,
        "BgpAsn" Lude.=: bgpASN,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "DeviceName" Lude.=: deviceName,
        "Type" Lude.=: type',
        "IpAddress" Lude.=: publicIP,
        "DryRun" Lude.=: dryRun
      ]

-- | Contains the output of CreateCustomerGateway.
--
-- /See:/ 'mkCreateCustomerGatewayResponse' smart constructor.
data CreateCustomerGatewayResponse = CreateCustomerGatewayResponse'
  { -- | Information about the customer gateway.
    customerGateway :: Lude.Maybe CustomerGateway,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCustomerGatewayResponse' with the minimum fields required to make a request.
--
-- * 'customerGateway' - Information about the customer gateway.
-- * 'responseStatus' - The response status code.
mkCreateCustomerGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCustomerGatewayResponse
mkCreateCustomerGatewayResponse pResponseStatus_ =
  CreateCustomerGatewayResponse'
    { customerGateway = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the customer gateway.
--
-- /Note:/ Consider using 'customerGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgrsCustomerGateway :: Lens.Lens' CreateCustomerGatewayResponse (Lude.Maybe CustomerGateway)
ccgrsCustomerGateway = Lens.lens (customerGateway :: CreateCustomerGatewayResponse -> Lude.Maybe CustomerGateway) (\s a -> s {customerGateway = a} :: CreateCustomerGatewayResponse)
{-# DEPRECATED ccgrsCustomerGateway "Use generic-lens or generic-optics with 'customerGateway' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgrsResponseStatus :: Lens.Lens' CreateCustomerGatewayResponse Lude.Int
ccgrsResponseStatus = Lens.lens (responseStatus :: CreateCustomerGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCustomerGatewayResponse)
{-# DEPRECATED ccgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

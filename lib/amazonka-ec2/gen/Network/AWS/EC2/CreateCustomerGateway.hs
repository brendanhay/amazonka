{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateCustomerGateway (..)
    , mkCreateCustomerGateway
    -- ** Request lenses
    , ccgBgpAsn
    , ccgType
    , ccgCertificateArn
    , ccgDeviceName
    , ccgDryRun
    , ccgPublicIp
    , ccgTagSpecifications

    -- * Destructuring the response
    , CreateCustomerGatewayResponse (..)
    , mkCreateCustomerGatewayResponse
    -- ** Response lenses
    , ccgrrsCustomerGateway
    , ccgrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateCustomerGateway.
--
-- /See:/ 'mkCreateCustomerGateway' smart constructor.
data CreateCustomerGateway = CreateCustomerGateway'
  { bgpAsn :: Core.Int
    -- ^ For devices that support BGP, the customer gateway's BGP ASN.
--
-- Default: 65000
  , type' :: Types.GatewayType
    -- ^ The type of VPN connection that this customer gateway supports (@ipsec.1@ ).
  , certificateArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for the customer gateway certificate.
  , deviceName :: Core.Maybe Core.Text
    -- ^ A name for the customer gateway device.
--
-- Length Constraints: Up to 255 characters.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , publicIp :: Core.Maybe Core.Text
    -- ^ The Internet-routable IP address for the customer gateway's outside interface. The address must be static.
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the customer gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCustomerGateway' value with any optional fields omitted.
mkCreateCustomerGateway
    :: Core.Int -- ^ 'bgpAsn'
    -> Types.GatewayType -- ^ 'type\''
    -> CreateCustomerGateway
mkCreateCustomerGateway bgpAsn type'
  = CreateCustomerGateway'{bgpAsn, type',
                           certificateArn = Core.Nothing, deviceName = Core.Nothing,
                           dryRun = Core.Nothing, publicIp = Core.Nothing,
                           tagSpecifications = Core.Nothing}

-- | For devices that support BGP, the customer gateway's BGP ASN.
--
-- Default: 65000
--
-- /Note:/ Consider using 'bgpAsn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgBgpAsn :: Lens.Lens' CreateCustomerGateway Core.Int
ccgBgpAsn = Lens.field @"bgpAsn"
{-# INLINEABLE ccgBgpAsn #-}
{-# DEPRECATED bgpAsn "Use generic-lens or generic-optics with 'bgpAsn' instead"  #-}

-- | The type of VPN connection that this customer gateway supports (@ipsec.1@ ).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgType :: Lens.Lens' CreateCustomerGateway Types.GatewayType
ccgType = Lens.field @"type'"
{-# INLINEABLE ccgType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The Amazon Resource Name (ARN) for the customer gateway certificate.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgCertificateArn :: Lens.Lens' CreateCustomerGateway (Core.Maybe Core.Text)
ccgCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE ccgCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | A name for the customer gateway device.
--
-- Length Constraints: Up to 255 characters.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgDeviceName :: Lens.Lens' CreateCustomerGateway (Core.Maybe Core.Text)
ccgDeviceName = Lens.field @"deviceName"
{-# INLINEABLE ccgDeviceName #-}
{-# DEPRECATED deviceName "Use generic-lens or generic-optics with 'deviceName' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgDryRun :: Lens.Lens' CreateCustomerGateway (Core.Maybe Core.Bool)
ccgDryRun = Lens.field @"dryRun"
{-# INLINEABLE ccgDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The Internet-routable IP address for the customer gateway's outside interface. The address must be static.
--
-- /Note:/ Consider using 'publicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgPublicIp :: Lens.Lens' CreateCustomerGateway (Core.Maybe Core.Text)
ccgPublicIp = Lens.field @"publicIp"
{-# INLINEABLE ccgPublicIp #-}
{-# DEPRECATED publicIp "Use generic-lens or generic-optics with 'publicIp' instead"  #-}

-- | The tags to apply to the customer gateway.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgTagSpecifications :: Lens.Lens' CreateCustomerGateway (Core.Maybe [Types.TagSpecification])
ccgTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE ccgTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateCustomerGateway where
        toQuery CreateCustomerGateway{..}
          = Core.toQueryPair "Action" ("CreateCustomerGateway" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "BgpAsn" bgpAsn
              Core.<> Core.toQueryPair "Type" type'
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CertificateArn")
                certificateArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DeviceName") deviceName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "IpAddress") publicIp
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateCustomerGateway where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateCustomerGateway where
        type Rs CreateCustomerGateway = CreateCustomerGatewayResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateCustomerGatewayResponse' Core.<$>
                   (x Core..@? "customerGateway") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of CreateCustomerGateway.
--
-- /See:/ 'mkCreateCustomerGatewayResponse' smart constructor.
data CreateCustomerGatewayResponse = CreateCustomerGatewayResponse'
  { customerGateway :: Core.Maybe Types.CustomerGateway
    -- ^ Information about the customer gateway.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCustomerGatewayResponse' value with any optional fields omitted.
mkCreateCustomerGatewayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCustomerGatewayResponse
mkCreateCustomerGatewayResponse responseStatus
  = CreateCustomerGatewayResponse'{customerGateway = Core.Nothing,
                                   responseStatus}

-- | Information about the customer gateway.
--
-- /Note:/ Consider using 'customerGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgrrsCustomerGateway :: Lens.Lens' CreateCustomerGatewayResponse (Core.Maybe Types.CustomerGateway)
ccgrrsCustomerGateway = Lens.field @"customerGateway"
{-# INLINEABLE ccgrrsCustomerGateway #-}
{-# DEPRECATED customerGateway "Use generic-lens or generic-optics with 'customerGateway' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgrrsResponseStatus :: Lens.Lens' CreateCustomerGatewayResponse Core.Int
ccgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

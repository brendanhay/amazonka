{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVpnGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a virtual private gateway. A virtual private gateway is the endpoint on the VPC side of your VPN connection. You can create a virtual private gateway before creating the VPC itself.
--
-- For more information, see <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html AWS Site-to-Site VPN> in the /AWS Site-to-Site VPN User Guide/ .
module Network.AWS.EC2.CreateVpnGateway
    (
    -- * Creating a request
      CreateVpnGateway (..)
    , mkCreateVpnGateway
    -- ** Request lenses
    , cvgType
    , cvgAmazonSideAsn
    , cvgAvailabilityZone
    , cvgDryRun
    , cvgTagSpecifications

    -- * Destructuring the response
    , CreateVpnGatewayResponse (..)
    , mkCreateVpnGatewayResponse
    -- ** Response lenses
    , cvgrrsVpnGateway
    , cvgrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateVpnGateway.
--
-- /See:/ 'mkCreateVpnGateway' smart constructor.
data CreateVpnGateway = CreateVpnGateway'
  { type' :: Types.GatewayType
    -- ^ The type of VPN connection this virtual private gateway supports.
  , amazonSideAsn :: Core.Maybe Core.Integer
    -- ^ A private Autonomous System Number (ASN) for the Amazon side of a BGP session. If you're using a 16-bit ASN, it must be in the 64512 to 65534 range. If you're using a 32-bit ASN, it must be in the 4200000000 to 4294967294 range.
--
-- Default: 64512
  , availabilityZone :: Core.Maybe Core.Text
    -- ^ The Availability Zone for the virtual private gateway.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the virtual private gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVpnGateway' value with any optional fields omitted.
mkCreateVpnGateway
    :: Types.GatewayType -- ^ 'type\''
    -> CreateVpnGateway
mkCreateVpnGateway type'
  = CreateVpnGateway'{type', amazonSideAsn = Core.Nothing,
                      availabilityZone = Core.Nothing, dryRun = Core.Nothing,
                      tagSpecifications = Core.Nothing}

-- | The type of VPN connection this virtual private gateway supports.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvgType :: Lens.Lens' CreateVpnGateway Types.GatewayType
cvgType = Lens.field @"type'"
{-# INLINEABLE cvgType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | A private Autonomous System Number (ASN) for the Amazon side of a BGP session. If you're using a 16-bit ASN, it must be in the 64512 to 65534 range. If you're using a 32-bit ASN, it must be in the 4200000000 to 4294967294 range.
--
-- Default: 64512
--
-- /Note:/ Consider using 'amazonSideAsn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvgAmazonSideAsn :: Lens.Lens' CreateVpnGateway (Core.Maybe Core.Integer)
cvgAmazonSideAsn = Lens.field @"amazonSideAsn"
{-# INLINEABLE cvgAmazonSideAsn #-}
{-# DEPRECATED amazonSideAsn "Use generic-lens or generic-optics with 'amazonSideAsn' instead"  #-}

-- | The Availability Zone for the virtual private gateway.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvgAvailabilityZone :: Lens.Lens' CreateVpnGateway (Core.Maybe Core.Text)
cvgAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE cvgAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvgDryRun :: Lens.Lens' CreateVpnGateway (Core.Maybe Core.Bool)
cvgDryRun = Lens.field @"dryRun"
{-# INLINEABLE cvgDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The tags to apply to the virtual private gateway.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvgTagSpecifications :: Lens.Lens' CreateVpnGateway (Core.Maybe [Types.TagSpecification])
cvgTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE cvgTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateVpnGateway where
        toQuery CreateVpnGateway{..}
          = Core.toQueryPair "Action" ("CreateVpnGateway" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "Type" type'
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AmazonSideAsn")
                amazonSideAsn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AvailabilityZone")
                availabilityZone
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateVpnGateway where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateVpnGateway where
        type Rs CreateVpnGateway = CreateVpnGatewayResponse
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
                 CreateVpnGatewayResponse' Core.<$>
                   (x Core..@? "vpnGateway") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of CreateVpnGateway.
--
-- /See:/ 'mkCreateVpnGatewayResponse' smart constructor.
data CreateVpnGatewayResponse = CreateVpnGatewayResponse'
  { vpnGateway :: Core.Maybe Types.VpnGateway
    -- ^ Information about the virtual private gateway.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVpnGatewayResponse' value with any optional fields omitted.
mkCreateVpnGatewayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateVpnGatewayResponse
mkCreateVpnGatewayResponse responseStatus
  = CreateVpnGatewayResponse'{vpnGateway = Core.Nothing,
                              responseStatus}

-- | Information about the virtual private gateway.
--
-- /Note:/ Consider using 'vpnGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvgrrsVpnGateway :: Lens.Lens' CreateVpnGatewayResponse (Core.Maybe Types.VpnGateway)
cvgrrsVpnGateway = Lens.field @"vpnGateway"
{-# INLINEABLE cvgrrsVpnGateway #-}
{-# DEPRECATED vpnGateway "Use generic-lens or generic-optics with 'vpnGateway' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvgrrsResponseStatus :: Lens.Lens' CreateVpnGatewayResponse Core.Int
cvgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cvgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

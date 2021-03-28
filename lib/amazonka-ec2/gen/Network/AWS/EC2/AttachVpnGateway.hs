{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AttachVpnGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a virtual private gateway to a VPC. You can attach one virtual private gateway to one VPC at a time.
--
-- For more information, see <https://docs.aws.amazon.com/vpn/latest/s2svpn/VPC_VPN.html AWS Site-to-Site VPN> in the /AWS Site-to-Site VPN User Guide/ .
module Network.AWS.EC2.AttachVpnGateway
    (
    -- * Creating a request
      AttachVpnGateway (..)
    , mkAttachVpnGateway
    -- ** Request lenses
    , avgVpcId
    , avgVpnGatewayId
    , avgDryRun

    -- * Destructuring the response
    , AttachVpnGatewayResponse (..)
    , mkAttachVpnGatewayResponse
    -- ** Response lenses
    , avgrrsVpcAttachment
    , avgrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for AttachVpnGateway.
--
-- /See:/ 'mkAttachVpnGateway' smart constructor.
data AttachVpnGateway = AttachVpnGateway'
  { vpcId :: Types.VpcId
    -- ^ The ID of the VPC.
  , vpnGatewayId :: Types.VpnGatewayId
    -- ^ The ID of the virtual private gateway.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachVpnGateway' value with any optional fields omitted.
mkAttachVpnGateway
    :: Types.VpcId -- ^ 'vpcId'
    -> Types.VpnGatewayId -- ^ 'vpnGatewayId'
    -> AttachVpnGateway
mkAttachVpnGateway vpcId vpnGatewayId
  = AttachVpnGateway'{vpcId, vpnGatewayId, dryRun = Core.Nothing}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avgVpcId :: Lens.Lens' AttachVpnGateway Types.VpcId
avgVpcId = Lens.field @"vpcId"
{-# INLINEABLE avgVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'vpnGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avgVpnGatewayId :: Lens.Lens' AttachVpnGateway Types.VpnGatewayId
avgVpnGatewayId = Lens.field @"vpnGatewayId"
{-# INLINEABLE avgVpnGatewayId #-}
{-# DEPRECATED vpnGatewayId "Use generic-lens or generic-optics with 'vpnGatewayId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avgDryRun :: Lens.Lens' AttachVpnGateway (Core.Maybe Core.Bool)
avgDryRun = Lens.field @"dryRun"
{-# INLINEABLE avgDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery AttachVpnGateway where
        toQuery AttachVpnGateway{..}
          = Core.toQueryPair "Action" ("AttachVpnGateway" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VpcId" vpcId
              Core.<> Core.toQueryPair "VpnGatewayId" vpnGatewayId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders AttachVpnGateway where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AttachVpnGateway where
        type Rs AttachVpnGateway = AttachVpnGatewayResponse
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
                 AttachVpnGatewayResponse' Core.<$>
                   (x Core..@? "attachment") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of AttachVpnGateway.
--
-- /See:/ 'mkAttachVpnGatewayResponse' smart constructor.
data AttachVpnGatewayResponse = AttachVpnGatewayResponse'
  { vpcAttachment :: Core.Maybe Types.VpcAttachment
    -- ^ Information about the attachment.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachVpnGatewayResponse' value with any optional fields omitted.
mkAttachVpnGatewayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AttachVpnGatewayResponse
mkAttachVpnGatewayResponse responseStatus
  = AttachVpnGatewayResponse'{vpcAttachment = Core.Nothing,
                              responseStatus}

-- | Information about the attachment.
--
-- /Note:/ Consider using 'vpcAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avgrrsVpcAttachment :: Lens.Lens' AttachVpnGatewayResponse (Core.Maybe Types.VpcAttachment)
avgrrsVpcAttachment = Lens.field @"vpcAttachment"
{-# INLINEABLE avgrrsVpcAttachment #-}
{-# DEPRECATED vpcAttachment "Use generic-lens or generic-optics with 'vpcAttachment' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avgrrsResponseStatus :: Lens.Lens' AttachVpnGatewayResponse Core.Int
avgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE avgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

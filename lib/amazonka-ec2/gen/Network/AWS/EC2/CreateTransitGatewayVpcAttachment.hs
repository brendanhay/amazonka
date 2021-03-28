{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTransitGatewayVpcAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified VPC to the specified transit gateway.
--
-- If you attach a VPC with a CIDR range that overlaps the CIDR range of a VPC that is already attached, the new VPC CIDR range is not propagated to the default propagation route table.
-- To send VPC traffic to an attached transit gateway, add a route to the VPC route table using 'CreateRoute' .
module Network.AWS.EC2.CreateTransitGatewayVpcAttachment
    (
    -- * Creating a request
      CreateTransitGatewayVpcAttachment (..)
    , mkCreateTransitGatewayVpcAttachment
    -- ** Request lenses
    , ctgvaTransitGatewayId
    , ctgvaVpcId
    , ctgvaSubnetIds
    , ctgvaDryRun
    , ctgvaOptions
    , ctgvaTagSpecifications

    -- * Destructuring the response
    , CreateTransitGatewayVpcAttachmentResponse (..)
    , mkCreateTransitGatewayVpcAttachmentResponse
    -- ** Response lenses
    , ctgvarrsTransitGatewayVpcAttachment
    , ctgvarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTransitGatewayVpcAttachment' smart constructor.
data CreateTransitGatewayVpcAttachment = CreateTransitGatewayVpcAttachment'
  { transitGatewayId :: Types.TransitGatewayId
    -- ^ The ID of the transit gateway.
  , vpcId :: Types.VpcId
    -- ^ The ID of the VPC.
  , subnetIds :: [Types.SubnetId]
    -- ^ The IDs of one or more subnets. You can specify only one subnet per Availability Zone. You must specify at least one subnet, but we recommend that you specify two subnets for better availability. The transit gateway uses one IP address from each specified subnet.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , options :: Core.Maybe Types.CreateTransitGatewayVpcAttachmentRequestOptions
    -- ^ The VPC attachment options.
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the VPC attachment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTransitGatewayVpcAttachment' value with any optional fields omitted.
mkCreateTransitGatewayVpcAttachment
    :: Types.TransitGatewayId -- ^ 'transitGatewayId'
    -> Types.VpcId -- ^ 'vpcId'
    -> CreateTransitGatewayVpcAttachment
mkCreateTransitGatewayVpcAttachment transitGatewayId vpcId
  = CreateTransitGatewayVpcAttachment'{transitGatewayId, vpcId,
                                       subnetIds = Core.mempty, dryRun = Core.Nothing,
                                       options = Core.Nothing, tagSpecifications = Core.Nothing}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgvaTransitGatewayId :: Lens.Lens' CreateTransitGatewayVpcAttachment Types.TransitGatewayId
ctgvaTransitGatewayId = Lens.field @"transitGatewayId"
{-# INLINEABLE ctgvaTransitGatewayId #-}
{-# DEPRECATED transitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead"  #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgvaVpcId :: Lens.Lens' CreateTransitGatewayVpcAttachment Types.VpcId
ctgvaVpcId = Lens.field @"vpcId"
{-# INLINEABLE ctgvaVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | The IDs of one or more subnets. You can specify only one subnet per Availability Zone. You must specify at least one subnet, but we recommend that you specify two subnets for better availability. The transit gateway uses one IP address from each specified subnet.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgvaSubnetIds :: Lens.Lens' CreateTransitGatewayVpcAttachment [Types.SubnetId]
ctgvaSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE ctgvaSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgvaDryRun :: Lens.Lens' CreateTransitGatewayVpcAttachment (Core.Maybe Core.Bool)
ctgvaDryRun = Lens.field @"dryRun"
{-# INLINEABLE ctgvaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The VPC attachment options.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgvaOptions :: Lens.Lens' CreateTransitGatewayVpcAttachment (Core.Maybe Types.CreateTransitGatewayVpcAttachmentRequestOptions)
ctgvaOptions = Lens.field @"options"
{-# INLINEABLE ctgvaOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | The tags to apply to the VPC attachment.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgvaTagSpecifications :: Lens.Lens' CreateTransitGatewayVpcAttachment (Core.Maybe [Types.TagSpecification])
ctgvaTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE ctgvaTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateTransitGatewayVpcAttachment where
        toQuery CreateTransitGatewayVpcAttachment{..}
          = Core.toQueryPair "Action"
              ("CreateTransitGatewayVpcAttachment" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "TransitGatewayId" transitGatewayId
              Core.<> Core.toQueryPair "VpcId" vpcId
              Core.<> Core.toQueryList "SubnetIds" subnetIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Options") options
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecifications")
                tagSpecifications

instance Core.ToHeaders CreateTransitGatewayVpcAttachment where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateTransitGatewayVpcAttachment where
        type Rs CreateTransitGatewayVpcAttachment =
             CreateTransitGatewayVpcAttachmentResponse
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
                 CreateTransitGatewayVpcAttachmentResponse' Core.<$>
                   (x Core..@? "transitGatewayVpcAttachment") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTransitGatewayVpcAttachmentResponse' smart constructor.
data CreateTransitGatewayVpcAttachmentResponse = CreateTransitGatewayVpcAttachmentResponse'
  { transitGatewayVpcAttachment :: Core.Maybe Types.TransitGatewayVpcAttachment
    -- ^ Information about the VPC attachment.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateTransitGatewayVpcAttachmentResponse' value with any optional fields omitted.
mkCreateTransitGatewayVpcAttachmentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTransitGatewayVpcAttachmentResponse
mkCreateTransitGatewayVpcAttachmentResponse responseStatus
  = CreateTransitGatewayVpcAttachmentResponse'{transitGatewayVpcAttachment
                                                 = Core.Nothing,
                                               responseStatus}

-- | Information about the VPC attachment.
--
-- /Note:/ Consider using 'transitGatewayVpcAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgvarrsTransitGatewayVpcAttachment :: Lens.Lens' CreateTransitGatewayVpcAttachmentResponse (Core.Maybe Types.TransitGatewayVpcAttachment)
ctgvarrsTransitGatewayVpcAttachment = Lens.field @"transitGatewayVpcAttachment"
{-# INLINEABLE ctgvarrsTransitGatewayVpcAttachment #-}
{-# DEPRECATED transitGatewayVpcAttachment "Use generic-lens or generic-optics with 'transitGatewayVpcAttachment' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgvarrsResponseStatus :: Lens.Lens' CreateTransitGatewayVpcAttachmentResponse Core.Int
ctgvarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctgvarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

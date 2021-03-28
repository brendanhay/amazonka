{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyTransitGatewayVpcAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified VPC attachment.
module Network.AWS.EC2.ModifyTransitGatewayVpcAttachment
    (
    -- * Creating a request
      ModifyTransitGatewayVpcAttachment (..)
    , mkModifyTransitGatewayVpcAttachment
    -- ** Request lenses
    , mtgvaTransitGatewayAttachmentId
    , mtgvaAddSubnetIds
    , mtgvaDryRun
    , mtgvaOptions
    , mtgvaRemoveSubnetIds

    -- * Destructuring the response
    , ModifyTransitGatewayVpcAttachmentResponse (..)
    , mkModifyTransitGatewayVpcAttachmentResponse
    -- ** Response lenses
    , mtgvarrsTransitGatewayVpcAttachment
    , mtgvarrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyTransitGatewayVpcAttachment' smart constructor.
data ModifyTransitGatewayVpcAttachment = ModifyTransitGatewayVpcAttachment'
  { transitGatewayAttachmentId :: Types.TransitGatewayAttachmentId
    -- ^ The ID of the attachment.
  , addSubnetIds :: Core.Maybe [Types.SubnetId]
    -- ^ The IDs of one or more subnets to add. You can specify at most one subnet per Availability Zone.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , options :: Core.Maybe Types.ModifyTransitGatewayVpcAttachmentRequestOptions
    -- ^ The new VPC attachment options.
  , removeSubnetIds :: Core.Maybe [Types.SubnetId]
    -- ^ The IDs of one or more subnets to remove.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTransitGatewayVpcAttachment' value with any optional fields omitted.
mkModifyTransitGatewayVpcAttachment
    :: Types.TransitGatewayAttachmentId -- ^ 'transitGatewayAttachmentId'
    -> ModifyTransitGatewayVpcAttachment
mkModifyTransitGatewayVpcAttachment transitGatewayAttachmentId
  = ModifyTransitGatewayVpcAttachment'{transitGatewayAttachmentId,
                                       addSubnetIds = Core.Nothing, dryRun = Core.Nothing,
                                       options = Core.Nothing, removeSubnetIds = Core.Nothing}

-- | The ID of the attachment.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgvaTransitGatewayAttachmentId :: Lens.Lens' ModifyTransitGatewayVpcAttachment Types.TransitGatewayAttachmentId
mtgvaTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE mtgvaTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

-- | The IDs of one or more subnets to add. You can specify at most one subnet per Availability Zone.
--
-- /Note:/ Consider using 'addSubnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgvaAddSubnetIds :: Lens.Lens' ModifyTransitGatewayVpcAttachment (Core.Maybe [Types.SubnetId])
mtgvaAddSubnetIds = Lens.field @"addSubnetIds"
{-# INLINEABLE mtgvaAddSubnetIds #-}
{-# DEPRECATED addSubnetIds "Use generic-lens or generic-optics with 'addSubnetIds' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgvaDryRun :: Lens.Lens' ModifyTransitGatewayVpcAttachment (Core.Maybe Core.Bool)
mtgvaDryRun = Lens.field @"dryRun"
{-# INLINEABLE mtgvaDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The new VPC attachment options.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgvaOptions :: Lens.Lens' ModifyTransitGatewayVpcAttachment (Core.Maybe Types.ModifyTransitGatewayVpcAttachmentRequestOptions)
mtgvaOptions = Lens.field @"options"
{-# INLINEABLE mtgvaOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | The IDs of one or more subnets to remove.
--
-- /Note:/ Consider using 'removeSubnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgvaRemoveSubnetIds :: Lens.Lens' ModifyTransitGatewayVpcAttachment (Core.Maybe [Types.SubnetId])
mtgvaRemoveSubnetIds = Lens.field @"removeSubnetIds"
{-# INLINEABLE mtgvaRemoveSubnetIds #-}
{-# DEPRECATED removeSubnetIds "Use generic-lens or generic-optics with 'removeSubnetIds' instead"  #-}

instance Core.ToQuery ModifyTransitGatewayVpcAttachment where
        toQuery ModifyTransitGatewayVpcAttachment{..}
          = Core.toQueryPair "Action"
              ("ModifyTransitGatewayVpcAttachment" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TransitGatewayAttachmentId"
                transitGatewayAttachmentId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "AddSubnetIds")
                addSubnetIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Options") options
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "RemoveSubnetIds")
                removeSubnetIds

instance Core.ToHeaders ModifyTransitGatewayVpcAttachment where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyTransitGatewayVpcAttachment where
        type Rs ModifyTransitGatewayVpcAttachment =
             ModifyTransitGatewayVpcAttachmentResponse
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
                 ModifyTransitGatewayVpcAttachmentResponse' Core.<$>
                   (x Core..@? "transitGatewayVpcAttachment") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyTransitGatewayVpcAttachmentResponse' smart constructor.
data ModifyTransitGatewayVpcAttachmentResponse = ModifyTransitGatewayVpcAttachmentResponse'
  { transitGatewayVpcAttachment :: Core.Maybe Types.TransitGatewayVpcAttachment
    -- ^ Information about the modified attachment.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyTransitGatewayVpcAttachmentResponse' value with any optional fields omitted.
mkModifyTransitGatewayVpcAttachmentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyTransitGatewayVpcAttachmentResponse
mkModifyTransitGatewayVpcAttachmentResponse responseStatus
  = ModifyTransitGatewayVpcAttachmentResponse'{transitGatewayVpcAttachment
                                                 = Core.Nothing,
                                               responseStatus}

-- | Information about the modified attachment.
--
-- /Note:/ Consider using 'transitGatewayVpcAttachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgvarrsTransitGatewayVpcAttachment :: Lens.Lens' ModifyTransitGatewayVpcAttachmentResponse (Core.Maybe Types.TransitGatewayVpcAttachment)
mtgvarrsTransitGatewayVpcAttachment = Lens.field @"transitGatewayVpcAttachment"
{-# INLINEABLE mtgvarrsTransitGatewayVpcAttachment #-}
{-# DEPRECATED transitGatewayVpcAttachment "Use generic-lens or generic-optics with 'transitGatewayVpcAttachment' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgvarrsResponseStatus :: Lens.Lens' ModifyTransitGatewayVpcAttachmentResponse Core.Int
mtgvarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mtgvarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

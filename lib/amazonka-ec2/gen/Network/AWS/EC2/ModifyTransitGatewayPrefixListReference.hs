{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyTransitGatewayPrefixListReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a reference (route) to a prefix list in a specified transit gateway route table.
module Network.AWS.EC2.ModifyTransitGatewayPrefixListReference
    (
    -- * Creating a request
      ModifyTransitGatewayPrefixListReference (..)
    , mkModifyTransitGatewayPrefixListReference
    -- ** Request lenses
    , mtgplrTransitGatewayRouteTableId
    , mtgplrPrefixListId
    , mtgplrBlackhole
    , mtgplrDryRun
    , mtgplrTransitGatewayAttachmentId

    -- * Destructuring the response
    , ModifyTransitGatewayPrefixListReferenceResponse (..)
    , mkModifyTransitGatewayPrefixListReferenceResponse
    -- ** Response lenses
    , mtgplrrrsTransitGatewayPrefixListReference
    , mtgplrrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyTransitGatewayPrefixListReference' smart constructor.
data ModifyTransitGatewayPrefixListReference = ModifyTransitGatewayPrefixListReference'
  { transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId
    -- ^ The ID of the transit gateway route table.
  , prefixListId :: Types.PrefixListResourceId
    -- ^ The ID of the prefix list.
  , blackhole :: Core.Maybe Core.Bool
    -- ^ Indicates whether to drop traffic that matches this route.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , transitGatewayAttachmentId :: Core.Maybe Types.TransitGatewayAttachmentId
    -- ^ The ID of the attachment to which traffic is routed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTransitGatewayPrefixListReference' value with any optional fields omitted.
mkModifyTransitGatewayPrefixListReference
    :: Types.TransitGatewayRouteTableId -- ^ 'transitGatewayRouteTableId'
    -> Types.PrefixListResourceId -- ^ 'prefixListId'
    -> ModifyTransitGatewayPrefixListReference
mkModifyTransitGatewayPrefixListReference
  transitGatewayRouteTableId prefixListId
  = ModifyTransitGatewayPrefixListReference'{transitGatewayRouteTableId,
                                             prefixListId, blackhole = Core.Nothing,
                                             dryRun = Core.Nothing,
                                             transitGatewayAttachmentId = Core.Nothing}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgplrTransitGatewayRouteTableId :: Lens.Lens' ModifyTransitGatewayPrefixListReference Types.TransitGatewayRouteTableId
mtgplrTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# INLINEABLE mtgplrTransitGatewayRouteTableId #-}
{-# DEPRECATED transitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead"  #-}

-- | The ID of the prefix list.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgplrPrefixListId :: Lens.Lens' ModifyTransitGatewayPrefixListReference Types.PrefixListResourceId
mtgplrPrefixListId = Lens.field @"prefixListId"
{-# INLINEABLE mtgplrPrefixListId #-}
{-# DEPRECATED prefixListId "Use generic-lens or generic-optics with 'prefixListId' instead"  #-}

-- | Indicates whether to drop traffic that matches this route.
--
-- /Note:/ Consider using 'blackhole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgplrBlackhole :: Lens.Lens' ModifyTransitGatewayPrefixListReference (Core.Maybe Core.Bool)
mtgplrBlackhole = Lens.field @"blackhole"
{-# INLINEABLE mtgplrBlackhole #-}
{-# DEPRECATED blackhole "Use generic-lens or generic-optics with 'blackhole' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgplrDryRun :: Lens.Lens' ModifyTransitGatewayPrefixListReference (Core.Maybe Core.Bool)
mtgplrDryRun = Lens.field @"dryRun"
{-# INLINEABLE mtgplrDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The ID of the attachment to which traffic is routed.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgplrTransitGatewayAttachmentId :: Lens.Lens' ModifyTransitGatewayPrefixListReference (Core.Maybe Types.TransitGatewayAttachmentId)
mtgplrTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE mtgplrTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

instance Core.ToQuery ModifyTransitGatewayPrefixListReference where
        toQuery ModifyTransitGatewayPrefixListReference{..}
          = Core.toQueryPair "Action"
              ("ModifyTransitGatewayPrefixListReference" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "TransitGatewayRouteTableId"
                transitGatewayRouteTableId
              Core.<> Core.toQueryPair "PrefixListId" prefixListId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Blackhole") blackhole
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "TransitGatewayAttachmentId")
                transitGatewayAttachmentId

instance Core.ToHeaders ModifyTransitGatewayPrefixListReference
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyTransitGatewayPrefixListReference
         where
        type Rs ModifyTransitGatewayPrefixListReference =
             ModifyTransitGatewayPrefixListReferenceResponse
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
                 ModifyTransitGatewayPrefixListReferenceResponse' Core.<$>
                   (x Core..@? "transitGatewayPrefixListReference") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyTransitGatewayPrefixListReferenceResponse' smart constructor.
data ModifyTransitGatewayPrefixListReferenceResponse = ModifyTransitGatewayPrefixListReferenceResponse'
  { transitGatewayPrefixListReference :: Core.Maybe Types.TransitGatewayPrefixListReference
    -- ^ Information about the prefix list reference.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyTransitGatewayPrefixListReferenceResponse' value with any optional fields omitted.
mkModifyTransitGatewayPrefixListReferenceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyTransitGatewayPrefixListReferenceResponse
mkModifyTransitGatewayPrefixListReferenceResponse responseStatus
  = ModifyTransitGatewayPrefixListReferenceResponse'{transitGatewayPrefixListReference
                                                       = Core.Nothing,
                                                     responseStatus}

-- | Information about the prefix list reference.
--
-- /Note:/ Consider using 'transitGatewayPrefixListReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgplrrrsTransitGatewayPrefixListReference :: Lens.Lens' ModifyTransitGatewayPrefixListReferenceResponse (Core.Maybe Types.TransitGatewayPrefixListReference)
mtgplrrrsTransitGatewayPrefixListReference = Lens.field @"transitGatewayPrefixListReference"
{-# INLINEABLE mtgplrrrsTransitGatewayPrefixListReference #-}
{-# DEPRECATED transitGatewayPrefixListReference "Use generic-lens or generic-optics with 'transitGatewayPrefixListReference' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtgplrrrsResponseStatus :: Lens.Lens' ModifyTransitGatewayPrefixListReferenceResponse Core.Int
mtgplrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mtgplrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

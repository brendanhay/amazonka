{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTransitGatewayPrefixListReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a reference (route) to a prefix list in a specified transit gateway route table.
module Network.AWS.EC2.CreateTransitGatewayPrefixListReference
    (
    -- * Creating a request
      CreateTransitGatewayPrefixListReference (..)
    , mkCreateTransitGatewayPrefixListReference
    -- ** Request lenses
    , ctgplrTransitGatewayRouteTableId
    , ctgplrPrefixListId
    , ctgplrBlackhole
    , ctgplrDryRun
    , ctgplrTransitGatewayAttachmentId

    -- * Destructuring the response
    , CreateTransitGatewayPrefixListReferenceResponse (..)
    , mkCreateTransitGatewayPrefixListReferenceResponse
    -- ** Response lenses
    , ctgplrrrsTransitGatewayPrefixListReference
    , ctgplrrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTransitGatewayPrefixListReference' smart constructor.
data CreateTransitGatewayPrefixListReference = CreateTransitGatewayPrefixListReference'
  { transitGatewayRouteTableId :: Types.TransitGatewayRouteTableId
    -- ^ The ID of the transit gateway route table.
  , prefixListId :: Types.PrefixListResourceId
    -- ^ The ID of the prefix list that is used for destination matches.
  , blackhole :: Core.Maybe Core.Bool
    -- ^ Indicates whether to drop traffic that matches this route.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , transitGatewayAttachmentId :: Core.Maybe Types.TransitGatewayAttachmentId
    -- ^ The ID of the attachment to which traffic is routed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTransitGatewayPrefixListReference' value with any optional fields omitted.
mkCreateTransitGatewayPrefixListReference
    :: Types.TransitGatewayRouteTableId -- ^ 'transitGatewayRouteTableId'
    -> Types.PrefixListResourceId -- ^ 'prefixListId'
    -> CreateTransitGatewayPrefixListReference
mkCreateTransitGatewayPrefixListReference
  transitGatewayRouteTableId prefixListId
  = CreateTransitGatewayPrefixListReference'{transitGatewayRouteTableId,
                                             prefixListId, blackhole = Core.Nothing,
                                             dryRun = Core.Nothing,
                                             transitGatewayAttachmentId = Core.Nothing}

-- | The ID of the transit gateway route table.
--
-- /Note:/ Consider using 'transitGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgplrTransitGatewayRouteTableId :: Lens.Lens' CreateTransitGatewayPrefixListReference Types.TransitGatewayRouteTableId
ctgplrTransitGatewayRouteTableId = Lens.field @"transitGatewayRouteTableId"
{-# INLINEABLE ctgplrTransitGatewayRouteTableId #-}
{-# DEPRECATED transitGatewayRouteTableId "Use generic-lens or generic-optics with 'transitGatewayRouteTableId' instead"  #-}

-- | The ID of the prefix list that is used for destination matches.
--
-- /Note:/ Consider using 'prefixListId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgplrPrefixListId :: Lens.Lens' CreateTransitGatewayPrefixListReference Types.PrefixListResourceId
ctgplrPrefixListId = Lens.field @"prefixListId"
{-# INLINEABLE ctgplrPrefixListId #-}
{-# DEPRECATED prefixListId "Use generic-lens or generic-optics with 'prefixListId' instead"  #-}

-- | Indicates whether to drop traffic that matches this route.
--
-- /Note:/ Consider using 'blackhole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgplrBlackhole :: Lens.Lens' CreateTransitGatewayPrefixListReference (Core.Maybe Core.Bool)
ctgplrBlackhole = Lens.field @"blackhole"
{-# INLINEABLE ctgplrBlackhole #-}
{-# DEPRECATED blackhole "Use generic-lens or generic-optics with 'blackhole' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgplrDryRun :: Lens.Lens' CreateTransitGatewayPrefixListReference (Core.Maybe Core.Bool)
ctgplrDryRun = Lens.field @"dryRun"
{-# INLINEABLE ctgplrDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The ID of the attachment to which traffic is routed.
--
-- /Note:/ Consider using 'transitGatewayAttachmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgplrTransitGatewayAttachmentId :: Lens.Lens' CreateTransitGatewayPrefixListReference (Core.Maybe Types.TransitGatewayAttachmentId)
ctgplrTransitGatewayAttachmentId = Lens.field @"transitGatewayAttachmentId"
{-# INLINEABLE ctgplrTransitGatewayAttachmentId #-}
{-# DEPRECATED transitGatewayAttachmentId "Use generic-lens or generic-optics with 'transitGatewayAttachmentId' instead"  #-}

instance Core.ToQuery CreateTransitGatewayPrefixListReference where
        toQuery CreateTransitGatewayPrefixListReference{..}
          = Core.toQueryPair "Action"
              ("CreateTransitGatewayPrefixListReference" :: Core.Text)
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

instance Core.ToHeaders CreateTransitGatewayPrefixListReference
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateTransitGatewayPrefixListReference
         where
        type Rs CreateTransitGatewayPrefixListReference =
             CreateTransitGatewayPrefixListReferenceResponse
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
                 CreateTransitGatewayPrefixListReferenceResponse' Core.<$>
                   (x Core..@? "transitGatewayPrefixListReference") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTransitGatewayPrefixListReferenceResponse' smart constructor.
data CreateTransitGatewayPrefixListReferenceResponse = CreateTransitGatewayPrefixListReferenceResponse'
  { transitGatewayPrefixListReference :: Core.Maybe Types.TransitGatewayPrefixListReference
    -- ^ Information about the prefix list reference.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTransitGatewayPrefixListReferenceResponse' value with any optional fields omitted.
mkCreateTransitGatewayPrefixListReferenceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTransitGatewayPrefixListReferenceResponse
mkCreateTransitGatewayPrefixListReferenceResponse responseStatus
  = CreateTransitGatewayPrefixListReferenceResponse'{transitGatewayPrefixListReference
                                                       = Core.Nothing,
                                                     responseStatus}

-- | Information about the prefix list reference.
--
-- /Note:/ Consider using 'transitGatewayPrefixListReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgplrrrsTransitGatewayPrefixListReference :: Lens.Lens' CreateTransitGatewayPrefixListReferenceResponse (Core.Maybe Types.TransitGatewayPrefixListReference)
ctgplrrrsTransitGatewayPrefixListReference = Lens.field @"transitGatewayPrefixListReference"
{-# INLINEABLE ctgplrrrsTransitGatewayPrefixListReference #-}
{-# DEPRECATED transitGatewayPrefixListReference "Use generic-lens or generic-optics with 'transitGatewayPrefixListReference' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgplrrrsResponseStatus :: Lens.Lens' CreateTransitGatewayPrefixListReferenceResponse Core.Int
ctgplrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctgplrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

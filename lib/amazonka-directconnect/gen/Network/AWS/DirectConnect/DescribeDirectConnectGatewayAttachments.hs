{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeDirectConnectGatewayAttachments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the attachments between your Direct Connect gateways and virtual interfaces. You must specify a Direct Connect gateway, a virtual interface, or both. If you specify a Direct Connect gateway, the response contains all virtual interfaces attached to the Direct Connect gateway. If you specify a virtual interface, the response contains all Direct Connect gateways attached to the virtual interface. If you specify both, the response contains the attachment between the Direct Connect gateway and the virtual interface.
--
-- This operation returns paginated results.
module Network.AWS.DirectConnect.DescribeDirectConnectGatewayAttachments
    (
    -- * Creating a request
      DescribeDirectConnectGatewayAttachments (..)
    , mkDescribeDirectConnectGatewayAttachments
    -- ** Request lenses
    , ddcgasDirectConnectGatewayId
    , ddcgasMaxResults
    , ddcgasNextToken
    , ddcgasVirtualInterfaceId

    -- * Destructuring the response
    , DescribeDirectConnectGatewayAttachmentsResponse (..)
    , mkDescribeDirectConnectGatewayAttachmentsResponse
    -- ** Response lenses
    , ddcgargrsDirectConnectGatewayAttachments
    , ddcgargrsNextToken
    , ddcgargrsResponseStatus
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDirectConnectGatewayAttachments' smart constructor.
data DescribeDirectConnectGatewayAttachments = DescribeDirectConnectGatewayAttachments'
  { directConnectGatewayId :: Core.Maybe Types.DirectConnectGatewayId
    -- ^ The ID of the Direct Connect gateway.
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The token provided in the previous call to retrieve the next page.
  , virtualInterfaceId :: Core.Maybe Types.VirtualInterfaceId
    -- ^ The ID of the virtual interface.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDirectConnectGatewayAttachments' value with any optional fields omitted.
mkDescribeDirectConnectGatewayAttachments
    :: DescribeDirectConnectGatewayAttachments
mkDescribeDirectConnectGatewayAttachments
  = DescribeDirectConnectGatewayAttachments'{directConnectGatewayId =
                                               Core.Nothing,
                                             maxResults = Core.Nothing, nextToken = Core.Nothing,
                                             virtualInterfaceId = Core.Nothing}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgasDirectConnectGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAttachments (Core.Maybe Types.DirectConnectGatewayId)
ddcgasDirectConnectGatewayId = Lens.field @"directConnectGatewayId"
{-# INLINEABLE ddcgasDirectConnectGatewayId #-}
{-# DEPRECATED directConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgasMaxResults :: Lens.Lens' DescribeDirectConnectGatewayAttachments (Core.Maybe Core.Int)
ddcgasMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ddcgasMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token provided in the previous call to retrieve the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgasNextToken :: Lens.Lens' DescribeDirectConnectGatewayAttachments (Core.Maybe Types.PaginationToken)
ddcgasNextToken = Lens.field @"nextToken"
{-# INLINEABLE ddcgasNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The ID of the virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgasVirtualInterfaceId :: Lens.Lens' DescribeDirectConnectGatewayAttachments (Core.Maybe Types.VirtualInterfaceId)
ddcgasVirtualInterfaceId = Lens.field @"virtualInterfaceId"
{-# INLINEABLE ddcgasVirtualInterfaceId #-}
{-# DEPRECATED virtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead"  #-}

instance Core.ToQuery DescribeDirectConnectGatewayAttachments where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeDirectConnectGatewayAttachments
         where
        toHeaders DescribeDirectConnectGatewayAttachments{..}
          = Core.pure
              ("X-Amz-Target",
               "OvertureService.DescribeDirectConnectGatewayAttachments")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeDirectConnectGatewayAttachments
         where
        toJSON DescribeDirectConnectGatewayAttachments{..}
          = Core.object
              (Core.catMaybes
                 [("directConnectGatewayId" Core..=) Core.<$>
                    directConnectGatewayId,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("virtualInterfaceId" Core..=) Core.<$> virtualInterfaceId])

instance Core.AWSRequest DescribeDirectConnectGatewayAttachments
         where
        type Rs DescribeDirectConnectGatewayAttachments =
             DescribeDirectConnectGatewayAttachmentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeDirectConnectGatewayAttachmentsResponse' Core.<$>
                   (x Core..:? "directConnectGatewayAttachments") Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeDirectConnectGatewayAttachments
         where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"directConnectGatewayAttachments" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeDirectConnectGatewayAttachmentsResponse' smart constructor.
data DescribeDirectConnectGatewayAttachmentsResponse = DescribeDirectConnectGatewayAttachmentsResponse'
  { directConnectGatewayAttachments :: Core.Maybe [Types.DirectConnectGatewayAttachment]
    -- ^ The attachments.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The token to retrieve the next page.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDirectConnectGatewayAttachmentsResponse' value with any optional fields omitted.
mkDescribeDirectConnectGatewayAttachmentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDirectConnectGatewayAttachmentsResponse
mkDescribeDirectConnectGatewayAttachmentsResponse responseStatus
  = DescribeDirectConnectGatewayAttachmentsResponse'{directConnectGatewayAttachments
                                                       = Core.Nothing,
                                                     nextToken = Core.Nothing, responseStatus}

-- | The attachments.
--
-- /Note:/ Consider using 'directConnectGatewayAttachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgargrsDirectConnectGatewayAttachments :: Lens.Lens' DescribeDirectConnectGatewayAttachmentsResponse (Core.Maybe [Types.DirectConnectGatewayAttachment])
ddcgargrsDirectConnectGatewayAttachments = Lens.field @"directConnectGatewayAttachments"
{-# INLINEABLE ddcgargrsDirectConnectGatewayAttachments #-}
{-# DEPRECATED directConnectGatewayAttachments "Use generic-lens or generic-optics with 'directConnectGatewayAttachments' instead"  #-}

-- | The token to retrieve the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgargrsNextToken :: Lens.Lens' DescribeDirectConnectGatewayAttachmentsResponse (Core.Maybe Types.PaginationToken)
ddcgargrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ddcgargrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgargrsResponseStatus :: Lens.Lens' DescribeDirectConnectGatewayAttachmentsResponse Core.Int
ddcgargrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddcgargrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

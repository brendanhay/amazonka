{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeDirectConnectGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all your Direct Connect gateways or only the specified Direct Connect gateway. Deleted Direct Connect gateways are not returned.
--
-- This operation returns paginated results.
module Network.AWS.DirectConnect.DescribeDirectConnectGateways
    (
    -- * Creating a request
      DescribeDirectConnectGateways (..)
    , mkDescribeDirectConnectGateways
    -- ** Request lenses
    , ddcgDirectConnectGatewayId
    , ddcgMaxResults
    , ddcgNextToken

    -- * Destructuring the response
    , DescribeDirectConnectGatewaysResponse (..)
    , mkDescribeDirectConnectGatewaysResponse
    -- ** Response lenses
    , ddcgrrsDirectConnectGateways
    , ddcgrrsNextToken
    , ddcgrrsResponseStatus
    ) where

import qualified Network.AWS.DirectConnect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDirectConnectGateways' smart constructor.
data DescribeDirectConnectGateways = DescribeDirectConnectGateways'
  { directConnectGatewayId :: Core.Maybe Types.DirectConnectGatewayId
    -- ^ The ID of the Direct Connect gateway.
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token provided in the previous call to retrieve the next page.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDirectConnectGateways' value with any optional fields omitted.
mkDescribeDirectConnectGateways
    :: DescribeDirectConnectGateways
mkDescribeDirectConnectGateways
  = DescribeDirectConnectGateways'{directConnectGatewayId =
                                     Core.Nothing,
                                   maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgDirectConnectGatewayId :: Lens.Lens' DescribeDirectConnectGateways (Core.Maybe Types.DirectConnectGatewayId)
ddcgDirectConnectGatewayId = Lens.field @"directConnectGatewayId"
{-# INLINEABLE ddcgDirectConnectGatewayId #-}
{-# DEPRECATED directConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead"  #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgMaxResults :: Lens.Lens' DescribeDirectConnectGateways (Core.Maybe Core.Int)
ddcgMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ddcgMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token provided in the previous call to retrieve the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgNextToken :: Lens.Lens' DescribeDirectConnectGateways (Core.Maybe Types.NextToken)
ddcgNextToken = Lens.field @"nextToken"
{-# INLINEABLE ddcgNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeDirectConnectGateways where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeDirectConnectGateways where
        toHeaders DescribeDirectConnectGateways{..}
          = Core.pure
              ("X-Amz-Target", "OvertureService.DescribeDirectConnectGateways")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeDirectConnectGateways where
        toJSON DescribeDirectConnectGateways{..}
          = Core.object
              (Core.catMaybes
                 [("directConnectGatewayId" Core..=) Core.<$>
                    directConnectGatewayId,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeDirectConnectGateways where
        type Rs DescribeDirectConnectGateways =
             DescribeDirectConnectGatewaysResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeDirectConnectGatewaysResponse' Core.<$>
                   (x Core..:? "directConnectGateways") Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeDirectConnectGateways where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"directConnectGateways" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeDirectConnectGatewaysResponse' smart constructor.
data DescribeDirectConnectGatewaysResponse = DescribeDirectConnectGatewaysResponse'
  { directConnectGateways :: Core.Maybe [Types.DirectConnectGateway]
    -- ^ The Direct Connect gateways.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to retrieve the next page.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDirectConnectGatewaysResponse' value with any optional fields omitted.
mkDescribeDirectConnectGatewaysResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeDirectConnectGatewaysResponse
mkDescribeDirectConnectGatewaysResponse responseStatus
  = DescribeDirectConnectGatewaysResponse'{directConnectGateways =
                                             Core.Nothing,
                                           nextToken = Core.Nothing, responseStatus}

-- | The Direct Connect gateways.
--
-- /Note:/ Consider using 'directConnectGateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgrrsDirectConnectGateways :: Lens.Lens' DescribeDirectConnectGatewaysResponse (Core.Maybe [Types.DirectConnectGateway])
ddcgrrsDirectConnectGateways = Lens.field @"directConnectGateways"
{-# INLINEABLE ddcgrrsDirectConnectGateways #-}
{-# DEPRECATED directConnectGateways "Use generic-lens or generic-optics with 'directConnectGateways' instead"  #-}

-- | The token to retrieve the next page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgrrsNextToken :: Lens.Lens' DescribeDirectConnectGatewaysResponse (Core.Maybe Types.NextToken)
ddcgrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ddcgrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgrrsResponseStatus :: Lens.Lens' DescribeDirectConnectGatewaysResponse Core.Int
ddcgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ddcgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

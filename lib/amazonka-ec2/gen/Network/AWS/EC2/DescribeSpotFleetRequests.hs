{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSpotFleetRequests
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your Spot Fleet requests.
--
-- Spot Fleet requests are deleted 48 hours after they are canceled and their instances are terminated.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeSpotFleetRequests
    (
    -- * Creating a request
      DescribeSpotFleetRequests (..)
    , mkDescribeSpotFleetRequests
    -- ** Request lenses
    , dsfrDryRun
    , dsfrMaxResults
    , dsfrNextToken
    , dsfrSpotFleetRequestIds

    -- * Destructuring the response
    , DescribeSpotFleetRequestsResponse (..)
    , mkDescribeSpotFleetRequestsResponse
    -- ** Response lenses
    , dsfrrrsNextToken
    , dsfrrrsSpotFleetRequestConfigs
    , dsfrrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeSpotFleetRequests.
--
-- /See:/ 'mkDescribeSpotFleetRequests' smart constructor.
data DescribeSpotFleetRequests = DescribeSpotFleetRequests'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results.
  , spotFleetRequestIds :: Core.Maybe [Types.SpotFleetRequestId]
    -- ^ The IDs of the Spot Fleet requests.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSpotFleetRequests' value with any optional fields omitted.
mkDescribeSpotFleetRequests
    :: DescribeSpotFleetRequests
mkDescribeSpotFleetRequests
  = DescribeSpotFleetRequests'{dryRun = Core.Nothing,
                               maxResults = Core.Nothing, nextToken = Core.Nothing,
                               spotFleetRequestIds = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrDryRun :: Lens.Lens' DescribeSpotFleetRequests (Core.Maybe Core.Bool)
dsfrDryRun = Lens.field @"dryRun"
{-# INLINEABLE dsfrDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrMaxResults :: Lens.Lens' DescribeSpotFleetRequests (Core.Maybe Core.Int)
dsfrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dsfrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrNextToken :: Lens.Lens' DescribeSpotFleetRequests (Core.Maybe Core.Text)
dsfrNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsfrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The IDs of the Spot Fleet requests.
--
-- /Note:/ Consider using 'spotFleetRequestIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrSpotFleetRequestIds :: Lens.Lens' DescribeSpotFleetRequests (Core.Maybe [Types.SpotFleetRequestId])
dsfrSpotFleetRequestIds = Lens.field @"spotFleetRequestIds"
{-# INLINEABLE dsfrSpotFleetRequestIds #-}
{-# DEPRECATED spotFleetRequestIds "Use generic-lens or generic-optics with 'spotFleetRequestIds' instead"  #-}

instance Core.ToQuery DescribeSpotFleetRequests where
        toQuery DescribeSpotFleetRequests{..}
          = Core.toQueryPair "Action"
              ("DescribeSpotFleetRequests" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "SpotFleetRequestId")
                spotFleetRequestIds

instance Core.ToHeaders DescribeSpotFleetRequests where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeSpotFleetRequests where
        type Rs DescribeSpotFleetRequests =
             DescribeSpotFleetRequestsResponse
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
                 DescribeSpotFleetRequestsResponse' Core.<$>
                   (x Core..@? "nextToken") Core.<*>
                     x Core..@? "spotFleetRequestConfigSet" Core..<@>
                       Core.parseXMLList "item"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeSpotFleetRequests where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"spotFleetRequestConfigs" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Contains the output of DescribeSpotFleetRequests.
--
-- /See:/ 'mkDescribeSpotFleetRequestsResponse' smart constructor.
data DescribeSpotFleetRequestsResponse = DescribeSpotFleetRequestsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
  , spotFleetRequestConfigs :: Core.Maybe [Types.SpotFleetRequestConfig]
    -- ^ Information about the configuration of your Spot Fleet.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeSpotFleetRequestsResponse' value with any optional fields omitted.
mkDescribeSpotFleetRequestsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSpotFleetRequestsResponse
mkDescribeSpotFleetRequestsResponse responseStatus
  = DescribeSpotFleetRequestsResponse'{nextToken = Core.Nothing,
                                       spotFleetRequestConfigs = Core.Nothing, responseStatus}

-- | The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrrrsNextToken :: Lens.Lens' DescribeSpotFleetRequestsResponse (Core.Maybe Core.Text)
dsfrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsfrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the configuration of your Spot Fleet.
--
-- /Note:/ Consider using 'spotFleetRequestConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrrrsSpotFleetRequestConfigs :: Lens.Lens' DescribeSpotFleetRequestsResponse (Core.Maybe [Types.SpotFleetRequestConfig])
dsfrrrsSpotFleetRequestConfigs = Lens.field @"spotFleetRequestConfigs"
{-# INLINEABLE dsfrrrsSpotFleetRequestConfigs #-}
{-# DEPRECATED spotFleetRequestConfigs "Use generic-lens or generic-optics with 'spotFleetRequestConfigs' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfrrrsResponseStatus :: Lens.Lens' DescribeSpotFleetRequestsResponse Core.Int
dsfrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsfrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

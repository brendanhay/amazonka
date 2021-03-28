{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSpotFleetInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the running instances for the specified Spot Fleet.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeSpotFleetInstances
    (
    -- * Creating a request
      DescribeSpotFleetInstances (..)
    , mkDescribeSpotFleetInstances
    -- ** Request lenses
    , dsfiSpotFleetRequestId
    , dsfiDryRun
    , dsfiMaxResults
    , dsfiNextToken

    -- * Destructuring the response
    , DescribeSpotFleetInstancesResponse (..)
    , mkDescribeSpotFleetInstancesResponse
    -- ** Response lenses
    , dsfirrsActiveInstances
    , dsfirrsNextToken
    , dsfirrsSpotFleetRequestId
    , dsfirrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeSpotFleetInstances.
--
-- /See:/ 'mkDescribeSpotFleetInstances' smart constructor.
data DescribeSpotFleetInstances = DescribeSpotFleetInstances'
  { spotFleetRequestId :: Types.SpotFleetRequestId
    -- ^ The ID of the Spot Fleet request.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSpotFleetInstances' value with any optional fields omitted.
mkDescribeSpotFleetInstances
    :: Types.SpotFleetRequestId -- ^ 'spotFleetRequestId'
    -> DescribeSpotFleetInstances
mkDescribeSpotFleetInstances spotFleetRequestId
  = DescribeSpotFleetInstances'{spotFleetRequestId,
                                dryRun = Core.Nothing, maxResults = Core.Nothing,
                                nextToken = Core.Nothing}

-- | The ID of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfiSpotFleetRequestId :: Lens.Lens' DescribeSpotFleetInstances Types.SpotFleetRequestId
dsfiSpotFleetRequestId = Lens.field @"spotFleetRequestId"
{-# INLINEABLE dsfiSpotFleetRequestId #-}
{-# DEPRECATED spotFleetRequestId "Use generic-lens or generic-optics with 'spotFleetRequestId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfiDryRun :: Lens.Lens' DescribeSpotFleetInstances (Core.Maybe Core.Bool)
dsfiDryRun = Lens.field @"dryRun"
{-# INLINEABLE dsfiDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfiMaxResults :: Lens.Lens' DescribeSpotFleetInstances (Core.Maybe Core.Natural)
dsfiMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dsfiMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfiNextToken :: Lens.Lens' DescribeSpotFleetInstances (Core.Maybe Core.Text)
dsfiNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsfiNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeSpotFleetInstances where
        toQuery DescribeSpotFleetInstances{..}
          = Core.toQueryPair "Action"
              ("DescribeSpotFleetInstances" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "SpotFleetRequestId" spotFleetRequestId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders DescribeSpotFleetInstances where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeSpotFleetInstances where
        type Rs DescribeSpotFleetInstances =
             DescribeSpotFleetInstancesResponse
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
                 DescribeSpotFleetInstancesResponse' Core.<$>
                   (x Core..@? "activeInstanceSet" Core..<@> Core.parseXMLList "item")
                     Core.<*> x Core..@? "nextToken"
                     Core.<*> x Core..@? "spotFleetRequestId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeSpotFleetInstances where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"activeInstances" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Contains the output of DescribeSpotFleetInstances.
--
-- /See:/ 'mkDescribeSpotFleetInstancesResponse' smart constructor.
data DescribeSpotFleetInstancesResponse = DescribeSpotFleetInstancesResponse'
  { activeInstances :: Core.Maybe [Types.ActiveInstance]
    -- ^ The running instances. This list is refreshed periodically and might be out of date.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
  , spotFleetRequestId :: Core.Maybe Core.Text
    -- ^ The ID of the Spot Fleet request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSpotFleetInstancesResponse' value with any optional fields omitted.
mkDescribeSpotFleetInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSpotFleetInstancesResponse
mkDescribeSpotFleetInstancesResponse responseStatus
  = DescribeSpotFleetInstancesResponse'{activeInstances =
                                          Core.Nothing,
                                        nextToken = Core.Nothing, spotFleetRequestId = Core.Nothing,
                                        responseStatus}

-- | The running instances. This list is refreshed periodically and might be out of date.
--
-- /Note:/ Consider using 'activeInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfirrsActiveInstances :: Lens.Lens' DescribeSpotFleetInstancesResponse (Core.Maybe [Types.ActiveInstance])
dsfirrsActiveInstances = Lens.field @"activeInstances"
{-# INLINEABLE dsfirrsActiveInstances #-}
{-# DEPRECATED activeInstances "Use generic-lens or generic-optics with 'activeInstances' instead"  #-}

-- | The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfirrsNextToken :: Lens.Lens' DescribeSpotFleetInstancesResponse (Core.Maybe Core.Text)
dsfirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsfirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The ID of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfirrsSpotFleetRequestId :: Lens.Lens' DescribeSpotFleetInstancesResponse (Core.Maybe Core.Text)
dsfirrsSpotFleetRequestId = Lens.field @"spotFleetRequestId"
{-# INLINEABLE dsfirrsSpotFleetRequestId #-}
{-# DEPRECATED spotFleetRequestId "Use generic-lens or generic-optics with 'spotFleetRequestId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfirrsResponseStatus :: Lens.Lens' DescribeSpotFleetInstancesResponse Core.Int
dsfirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsfirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

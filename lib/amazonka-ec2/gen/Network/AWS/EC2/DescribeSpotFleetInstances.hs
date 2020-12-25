{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeSpotFleetInstances (..),
    mkDescribeSpotFleetInstances,

    -- ** Request lenses
    dsfiSpotFleetRequestId,
    dsfiDryRun,
    dsfiMaxResults,
    dsfiNextToken,

    -- * Destructuring the response
    DescribeSpotFleetInstancesResponse (..),
    mkDescribeSpotFleetInstancesResponse,

    -- ** Response lenses
    dsfirrsActiveInstances,
    dsfirrsNextToken,
    dsfirrsSpotFleetRequestId,
    dsfirrsResponseStatus,
  )
where

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
  { -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Types.SpotFleetRequestId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSpotFleetInstances' value with any optional fields omitted.
mkDescribeSpotFleetInstances ::
  -- | 'spotFleetRequestId'
  Types.SpotFleetRequestId ->
  DescribeSpotFleetInstances
mkDescribeSpotFleetInstances spotFleetRequestId =
  DescribeSpotFleetInstances'
    { spotFleetRequestId,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfiSpotFleetRequestId :: Lens.Lens' DescribeSpotFleetInstances Types.SpotFleetRequestId
dsfiSpotFleetRequestId = Lens.field @"spotFleetRequestId"
{-# DEPRECATED dsfiSpotFleetRequestId "Use generic-lens or generic-optics with 'spotFleetRequestId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfiDryRun :: Lens.Lens' DescribeSpotFleetInstances (Core.Maybe Core.Bool)
dsfiDryRun = Lens.field @"dryRun"
{-# DEPRECATED dsfiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfiMaxResults :: Lens.Lens' DescribeSpotFleetInstances (Core.Maybe Core.Natural)
dsfiMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dsfiMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfiNextToken :: Lens.Lens' DescribeSpotFleetInstances (Core.Maybe Types.String)
dsfiNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsfiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeSpotFleetInstances where
  type
    Rs DescribeSpotFleetInstances =
      DescribeSpotFleetInstancesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeSpotFleetInstances")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "SpotFleetRequestId" spotFleetRequestId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSpotFleetInstancesResponse'
            Core.<$> (x Core..@? "activeInstanceSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (x Core..@? "spotFleetRequestId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeSpotFleetInstances where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"activeInstances" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Contains the output of DescribeSpotFleetInstances.
--
-- /See:/ 'mkDescribeSpotFleetInstancesResponse' smart constructor.
data DescribeSpotFleetInstancesResponse = DescribeSpotFleetInstancesResponse'
  { -- | The running instances. This list is refreshed periodically and might be out of date.
    activeInstances :: Core.Maybe [Types.ActiveInstance],
    -- | The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSpotFleetInstancesResponse' value with any optional fields omitted.
mkDescribeSpotFleetInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeSpotFleetInstancesResponse
mkDescribeSpotFleetInstancesResponse responseStatus =
  DescribeSpotFleetInstancesResponse'
    { activeInstances =
        Core.Nothing,
      nextToken = Core.Nothing,
      spotFleetRequestId = Core.Nothing,
      responseStatus
    }

-- | The running instances. This list is refreshed periodically and might be out of date.
--
-- /Note:/ Consider using 'activeInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfirrsActiveInstances :: Lens.Lens' DescribeSpotFleetInstancesResponse (Core.Maybe [Types.ActiveInstance])
dsfirrsActiveInstances = Lens.field @"activeInstances"
{-# DEPRECATED dsfirrsActiveInstances "Use generic-lens or generic-optics with 'activeInstances' instead." #-}

-- | The token required to retrieve the next set of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfirrsNextToken :: Lens.Lens' DescribeSpotFleetInstancesResponse (Core.Maybe Types.String)
dsfirrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsfirrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ID of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfirrsSpotFleetRequestId :: Lens.Lens' DescribeSpotFleetInstancesResponse (Core.Maybe Types.String)
dsfirrsSpotFleetRequestId = Lens.field @"spotFleetRequestId"
{-# DEPRECATED dsfirrsSpotFleetRequestId "Use generic-lens or generic-optics with 'spotFleetRequestId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfirrsResponseStatus :: Lens.Lens' DescribeSpotFleetInstancesResponse Core.Int
dsfirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsfirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeClassicLinkInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your linked EC2-Classic instances. This request only returns information about EC2-Classic instances linked to a VPC through ClassicLink. You cannot use this request to return information about other instances.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClassicLinkInstances
  ( -- * Creating a request
    DescribeClassicLinkInstances (..),
    mkDescribeClassicLinkInstances,

    -- ** Request lenses
    dcliDryRun,
    dcliFilters,
    dcliInstanceIds,
    dcliMaxResults,
    dcliNextToken,

    -- * Destructuring the response
    DescribeClassicLinkInstancesResponse (..),
    mkDescribeClassicLinkInstancesResponse,

    -- ** Response lenses
    dclirrsInstances,
    dclirrsNextToken,
    dclirrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeClassicLinkInstances' smart constructor.
data DescribeClassicLinkInstances = DescribeClassicLinkInstances'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters.
    --
    --
    --     * @group-id@ - The ID of a VPC security group that's associated with the instance.
    --
    --
    --     * @instance-id@ - The ID of the instance.
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    --
    --
    --     * @vpc-id@ - The ID of the VPC to which the instance is linked.
    -- @vpc-id@ - The ID of the VPC that the instance is linked to.
    filters :: Core.Maybe [Types.Filter],
    -- | One or more instance IDs. Must be instances linked to a VPC through ClassicLink.
    instanceIds :: Core.Maybe [Types.InstanceId],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    --
    -- Constraint: If the value is greater than 1000, we return only 1000 items.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClassicLinkInstances' value with any optional fields omitted.
mkDescribeClassicLinkInstances ::
  DescribeClassicLinkInstances
mkDescribeClassicLinkInstances =
  DescribeClassicLinkInstances'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      instanceIds = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcliDryRun :: Lens.Lens' DescribeClassicLinkInstances (Core.Maybe Core.Bool)
dcliDryRun = Lens.field @"dryRun"
{-# DEPRECATED dcliDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more filters.
--
--
--     * @group-id@ - The ID of a VPC security group that's associated with the instance.
--
--
--     * @instance-id@ - The ID of the instance.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC to which the instance is linked.
-- @vpc-id@ - The ID of the VPC that the instance is linked to.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcliFilters :: Lens.Lens' DescribeClassicLinkInstances (Core.Maybe [Types.Filter])
dcliFilters = Lens.field @"filters"
{-# DEPRECATED dcliFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | One or more instance IDs. Must be instances linked to a VPC through ClassicLink.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcliInstanceIds :: Lens.Lens' DescribeClassicLinkInstances (Core.Maybe [Types.InstanceId])
dcliInstanceIds = Lens.field @"instanceIds"
{-# DEPRECATED dcliInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- Constraint: If the value is greater than 1000, we return only 1000 items.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcliMaxResults :: Lens.Lens' DescribeClassicLinkInstances (Core.Maybe Core.Natural)
dcliMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dcliMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcliNextToken :: Lens.Lens' DescribeClassicLinkInstances (Core.Maybe Types.String)
dcliNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcliNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeClassicLinkInstances where
  type
    Rs DescribeClassicLinkInstances =
      DescribeClassicLinkInstancesResponse
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
            ( Core.pure ("Action", "DescribeClassicLinkInstances")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryList "InstanceId" Core.<$> instanceIds)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeClassicLinkInstancesResponse'
            Core.<$> (x Core..@? "instancesSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeClassicLinkInstances where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"instances" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeClassicLinkInstancesResponse' smart constructor.
data DescribeClassicLinkInstancesResponse = DescribeClassicLinkInstancesResponse'
  { -- | Information about one or more linked EC2-Classic instances.
    instances :: Core.Maybe [Types.ClassicLinkInstance],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClassicLinkInstancesResponse' value with any optional fields omitted.
mkDescribeClassicLinkInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeClassicLinkInstancesResponse
mkDescribeClassicLinkInstancesResponse responseStatus =
  DescribeClassicLinkInstancesResponse'
    { instances = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about one or more linked EC2-Classic instances.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclirrsInstances :: Lens.Lens' DescribeClassicLinkInstancesResponse (Core.Maybe [Types.ClassicLinkInstance])
dclirrsInstances = Lens.field @"instances"
{-# DEPRECATED dclirrsInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclirrsNextToken :: Lens.Lens' DescribeClassicLinkInstancesResponse (Core.Maybe Types.String)
dclirrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dclirrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dclirrsResponseStatus :: Lens.Lens' DescribeClassicLinkInstancesResponse Core.Int
dclirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dclirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

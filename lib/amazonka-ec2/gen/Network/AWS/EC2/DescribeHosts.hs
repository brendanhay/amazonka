{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeHosts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Dedicated Hosts or all your Dedicated Hosts.
--
-- The results describe only the Dedicated Hosts in the Region you're currently using. All listed instances consume capacity on your Dedicated Host. Dedicated Hosts that have recently been released are listed with the state @released@ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeHosts
  ( -- * Creating a request
    DescribeHosts (..),
    mkDescribeHosts,

    -- ** Request lenses
    dhFilter,
    dhHostIds,
    dhMaxResults,
    dhNextToken,

    -- * Destructuring the response
    DescribeHostsResponse (..),
    mkDescribeHostsResponse,

    -- ** Response lenses
    dhrrsHosts,
    dhrrsNextToken,
    dhrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeHosts' smart constructor.
data DescribeHosts = DescribeHosts'
  { -- | The filters.
    --
    --
    --     * @auto-placement@ - Whether auto-placement is enabled or disabled (@on@ | @off@ ).
    --
    --
    --     * @availability-zone@ - The Availability Zone of the host.
    --
    --
    --     * @client-token@ - The idempotency token that you provided when you allocated the host.
    --
    --
    --     * @host-reservation-id@ - The ID of the reservation assigned to this host.
    --
    --
    --     * @instance-type@ - The instance type size that the Dedicated Host is configured to support.
    --
    --
    --     * @state@ - The allocation state of the Dedicated Host (@available@ | @under-assessment@ | @permanent-failure@ | @released@ | @released-permanent-failure@ ).
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    filter :: Core.Maybe [Types.Filter],
    -- | The IDs of the Dedicated Hosts. The IDs are used for targeted instance launches.
    hostIds :: Core.Maybe [Types.DedicatedHostId],
    -- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
    --
    -- You cannot specify this parameter and the host IDs parameter in the same request.
    maxResults :: Core.Maybe Core.Int,
    -- | The token to use to retrieve the next page of results.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeHosts' value with any optional fields omitted.
mkDescribeHosts ::
  DescribeHosts
mkDescribeHosts =
  DescribeHosts'
    { filter = Core.Nothing,
      hostIds = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The filters.
--
--
--     * @auto-placement@ - Whether auto-placement is enabled or disabled (@on@ | @off@ ).
--
--
--     * @availability-zone@ - The Availability Zone of the host.
--
--
--     * @client-token@ - The idempotency token that you provided when you allocated the host.
--
--
--     * @host-reservation-id@ - The ID of the reservation assigned to this host.
--
--
--     * @instance-type@ - The instance type size that the Dedicated Host is configured to support.
--
--
--     * @state@ - The allocation state of the Dedicated Host (@available@ | @under-assessment@ | @permanent-failure@ | @released@ | @released-permanent-failure@ ).
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhFilter :: Lens.Lens' DescribeHosts (Core.Maybe [Types.Filter])
dhFilter = Lens.field @"filter"
{-# DEPRECATED dhFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The IDs of the Dedicated Hosts. The IDs are used for targeted instance launches.
--
-- /Note:/ Consider using 'hostIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhHostIds :: Lens.Lens' DescribeHosts (Core.Maybe [Types.DedicatedHostId])
dhHostIds = Lens.field @"hostIds"
{-# DEPRECATED dhHostIds "Use generic-lens or generic-optics with 'hostIds' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
--
-- You cannot specify this parameter and the host IDs parameter in the same request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhMaxResults :: Lens.Lens' DescribeHosts (Core.Maybe Core.Int)
dhMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dhMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token to use to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhNextToken :: Lens.Lens' DescribeHosts (Core.Maybe Types.String)
dhNextToken = Lens.field @"nextToken"
{-# DEPRECATED dhNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeHosts where
  type Rs DescribeHosts = DescribeHostsResponse
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
            ( Core.pure ("Action", "DescribeHosts")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "Filter" Core.<$> filter)
                Core.<> (Core.toQueryList "HostId" Core.<$> hostIds)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeHostsResponse'
            Core.<$> (x Core..@? "hostSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeHosts where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"hosts" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeHostsResponse' smart constructor.
data DescribeHostsResponse = DescribeHostsResponse'
  { -- | Information about the Dedicated Hosts.
    hosts :: Core.Maybe [Types.Host],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeHostsResponse' value with any optional fields omitted.
mkDescribeHostsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeHostsResponse
mkDescribeHostsResponse responseStatus =
  DescribeHostsResponse'
    { hosts = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the Dedicated Hosts.
--
-- /Note:/ Consider using 'hosts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrrsHosts :: Lens.Lens' DescribeHostsResponse (Core.Maybe [Types.Host])
dhrrsHosts = Lens.field @"hosts"
{-# DEPRECATED dhrrsHosts "Use generic-lens or generic-optics with 'hosts' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrrsNextToken :: Lens.Lens' DescribeHostsResponse (Core.Maybe Types.NextToken)
dhrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dhrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrrsResponseStatus :: Lens.Lens' DescribeHostsResponse Core.Int
dhrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dhrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

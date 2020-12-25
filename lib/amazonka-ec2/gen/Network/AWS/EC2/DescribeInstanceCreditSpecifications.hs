{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeInstanceCreditSpecifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the credit option for CPU usage of the specified burstable performance instances. The credit options are @standard@ and @unlimited@ .
--
-- If you do not specify an instance ID, Amazon EC2 returns burstable performance instances with the @unlimited@ credit option, as well as instances that were previously configured as T2, T3, and T3a with the @unlimited@ credit option. For example, if you resize a T2 instance, while it is configured as @unlimited@ , to an M4 instance, Amazon EC2 returns the M4 instance.
-- If you specify one or more instance IDs, Amazon EC2 returns the credit option (@standard@ or @unlimited@ ) of those instances. If you specify an instance ID that is not valid, such as an instance that is not a burstable performance instance, an error is returned.
-- Recently terminated instances might appear in the returned results. This interval is usually less than one hour.
-- If an Availability Zone is experiencing a service disruption and you specify instance IDs in the affected zone, or do not specify any instance IDs at all, the call fails. If you specify only instance IDs in an unaffected zone, the call works normally.
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/burstable-performance-instances.html Burstable performance instances> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeInstanceCreditSpecifications
  ( -- * Creating a request
    DescribeInstanceCreditSpecifications (..),
    mkDescribeInstanceCreditSpecifications,

    -- ** Request lenses
    dicsDryRun,
    dicsFilters,
    dicsInstanceIds,
    dicsMaxResults,
    dicsNextToken,

    -- * Destructuring the response
    DescribeInstanceCreditSpecificationsResponse (..),
    mkDescribeInstanceCreditSpecificationsResponse,

    -- ** Response lenses
    dicsrrsInstanceCreditSpecifications,
    dicsrrsNextToken,
    dicsrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeInstanceCreditSpecifications' smart constructor.
data DescribeInstanceCreditSpecifications = DescribeInstanceCreditSpecifications'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The filters.
    --
    --
    --     * @instance-id@ - The ID of the instance.
    filters :: Core.Maybe [Types.Filter],
    -- | The instance IDs.
    --
    -- Default: Describes all your instances.
    -- Constraints: Maximum 1000 explicitly specified instance IDs.
    instanceIds :: Core.Maybe [Types.InstanceId],
    -- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 5 and 1000. You cannot specify this parameter and the instance IDs parameter in the same call.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token to retrieve the next page of results.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceCreditSpecifications' value with any optional fields omitted.
mkDescribeInstanceCreditSpecifications ::
  DescribeInstanceCreditSpecifications
mkDescribeInstanceCreditSpecifications =
  DescribeInstanceCreditSpecifications'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      instanceIds = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dicsDryRun :: Lens.Lens' DescribeInstanceCreditSpecifications (Core.Maybe Core.Bool)
dicsDryRun = Lens.field @"dryRun"
{-# DEPRECATED dicsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The filters.
--
--
--     * @instance-id@ - The ID of the instance.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dicsFilters :: Lens.Lens' DescribeInstanceCreditSpecifications (Core.Maybe [Types.Filter])
dicsFilters = Lens.field @"filters"
{-# DEPRECATED dicsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The instance IDs.
--
-- Default: Describes all your instances.
-- Constraints: Maximum 1000 explicitly specified instance IDs.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dicsInstanceIds :: Lens.Lens' DescribeInstanceCreditSpecifications (Core.Maybe [Types.InstanceId])
dicsInstanceIds = Lens.field @"instanceIds"
{-# DEPRECATED dicsInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 5 and 1000. You cannot specify this parameter and the instance IDs parameter in the same call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dicsMaxResults :: Lens.Lens' DescribeInstanceCreditSpecifications (Core.Maybe Core.Natural)
dicsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dicsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dicsNextToken :: Lens.Lens' DescribeInstanceCreditSpecifications (Core.Maybe Types.String)
dicsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dicsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeInstanceCreditSpecifications where
  type
    Rs DescribeInstanceCreditSpecifications =
      DescribeInstanceCreditSpecificationsResponse
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
            ( Core.pure ("Action", "DescribeInstanceCreditSpecifications")
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
          DescribeInstanceCreditSpecificationsResponse'
            Core.<$> ( x Core..@? "instanceCreditSpecificationSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeInstanceCreditSpecifications where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"instanceCreditSpecifications" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeInstanceCreditSpecificationsResponse' smart constructor.
data DescribeInstanceCreditSpecificationsResponse = DescribeInstanceCreditSpecificationsResponse'
  { -- | Information about the credit option for CPU usage of an instance.
    instanceCreditSpecifications :: Core.Maybe [Types.InstanceCreditSpecification],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceCreditSpecificationsResponse' value with any optional fields omitted.
mkDescribeInstanceCreditSpecificationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeInstanceCreditSpecificationsResponse
mkDescribeInstanceCreditSpecificationsResponse responseStatus =
  DescribeInstanceCreditSpecificationsResponse'
    { instanceCreditSpecifications =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the credit option for CPU usage of an instance.
--
-- /Note:/ Consider using 'instanceCreditSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dicsrrsInstanceCreditSpecifications :: Lens.Lens' DescribeInstanceCreditSpecificationsResponse (Core.Maybe [Types.InstanceCreditSpecification])
dicsrrsInstanceCreditSpecifications = Lens.field @"instanceCreditSpecifications"
{-# DEPRECATED dicsrrsInstanceCreditSpecifications "Use generic-lens or generic-optics with 'instanceCreditSpecifications' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dicsrrsNextToken :: Lens.Lens' DescribeInstanceCreditSpecificationsResponse (Core.Maybe Types.String)
dicsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dicsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dicsrrsResponseStatus :: Lens.Lens' DescribeInstanceCreditSpecificationsResponse Core.Int
dicsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dicsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

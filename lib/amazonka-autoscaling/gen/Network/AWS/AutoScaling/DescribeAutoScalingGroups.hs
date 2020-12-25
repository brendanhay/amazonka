{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Auto Scaling groups.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeAutoScalingGroups
  ( -- * Creating a request
    DescribeAutoScalingGroups (..),
    mkDescribeAutoScalingGroups,

    -- ** Request lenses
    dasgAutoScalingGroupNames,
    dasgMaxRecords,
    dasgNextToken,

    -- * Destructuring the response
    DescribeAutoScalingGroupsResponse (..),
    mkDescribeAutoScalingGroupsResponse,

    -- ** Response lenses
    dasgrrsAutoScalingGroups,
    dasgrrsNextToken,
    dasgrrsResponseStatus,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAutoScalingGroups' smart constructor.
data DescribeAutoScalingGroups = DescribeAutoScalingGroups'
  { -- | The names of the Auto Scaling groups. By default, you can only specify up to 50 names. You can optionally increase this limit using the @MaxRecords@ parameter.
    --
    -- If you omit this parameter, all Auto Scaling groups are described.
    autoScalingGroupNames :: Core.Maybe [Types.ResourceName],
    -- | The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
    maxRecords :: Core.Maybe Core.Int,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.XmlString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAutoScalingGroups' value with any optional fields omitted.
mkDescribeAutoScalingGroups ::
  DescribeAutoScalingGroups
mkDescribeAutoScalingGroups =
  DescribeAutoScalingGroups'
    { autoScalingGroupNames = Core.Nothing,
      maxRecords = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The names of the Auto Scaling groups. By default, you can only specify up to 50 names. You can optionally increase this limit using the @MaxRecords@ parameter.
--
-- If you omit this parameter, all Auto Scaling groups are described.
--
-- /Note:/ Consider using 'autoScalingGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasgAutoScalingGroupNames :: Lens.Lens' DescribeAutoScalingGroups (Core.Maybe [Types.ResourceName])
dasgAutoScalingGroupNames = Lens.field @"autoScalingGroupNames"
{-# DEPRECATED dasgAutoScalingGroupNames "Use generic-lens or generic-optics with 'autoScalingGroupNames' instead." #-}

-- | The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasgMaxRecords :: Lens.Lens' DescribeAutoScalingGroups (Core.Maybe Core.Int)
dasgMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dasgMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasgNextToken :: Lens.Lens' DescribeAutoScalingGroups (Core.Maybe Types.XmlString)
dasgNextToken = Lens.field @"nextToken"
{-# DEPRECATED dasgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeAutoScalingGroups where
  type
    Rs DescribeAutoScalingGroups =
      DescribeAutoScalingGroupsResponse
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
            ( Core.pure ("Action", "DescribeAutoScalingGroups")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> ( Core.toQueryValue
                            "AutoScalingGroupNames"
                            (Core.toQueryList "member" Core.<$> autoScalingGroupNames)
                        )
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeAutoScalingGroupsResult"
      ( \s h x ->
          DescribeAutoScalingGroupsResponse'
            Core.<$> ( x Core..@? "AutoScalingGroups" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeAutoScalingGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"autoScalingGroups") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeAutoScalingGroupsResponse' smart constructor.
data DescribeAutoScalingGroupsResponse = DescribeAutoScalingGroupsResponse'
  { -- | The groups.
    autoScalingGroups :: [Types.AutoScalingGroup],
    -- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
    nextToken :: Core.Maybe Types.XmlString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAutoScalingGroupsResponse' value with any optional fields omitted.
mkDescribeAutoScalingGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAutoScalingGroupsResponse
mkDescribeAutoScalingGroupsResponse responseStatus =
  DescribeAutoScalingGroupsResponse'
    { autoScalingGroups =
        Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The groups.
--
-- /Note:/ Consider using 'autoScalingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasgrrsAutoScalingGroups :: Lens.Lens' DescribeAutoScalingGroupsResponse [Types.AutoScalingGroup]
dasgrrsAutoScalingGroups = Lens.field @"autoScalingGroups"
{-# DEPRECATED dasgrrsAutoScalingGroups "Use generic-lens or generic-optics with 'autoScalingGroups' instead." #-}

-- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasgrrsNextToken :: Lens.Lens' DescribeAutoScalingGroupsResponse (Core.Maybe Types.XmlString)
dasgrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dasgrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasgrrsResponseStatus :: Lens.Lens' DescribeAutoScalingGroupsResponse Core.Int
dasgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dasgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

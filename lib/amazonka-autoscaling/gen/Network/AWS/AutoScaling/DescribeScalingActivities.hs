{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeScalingActivities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more scaling activities for the specified Auto Scaling group.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeScalingActivities
  ( -- * Creating a request
    DescribeScalingActivities (..),
    mkDescribeScalingActivities,

    -- ** Request lenses
    dsafActivityIds,
    dsafAutoScalingGroupName,
    dsafMaxRecords,
    dsafNextToken,

    -- * Destructuring the response
    DescribeScalingActivitiesResponse (..),
    mkDescribeScalingActivitiesResponse,

    -- ** Response lenses
    dsarfrsActivities,
    dsarfrsNextToken,
    dsarfrsResponseStatus,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeScalingActivities' smart constructor.
data DescribeScalingActivities = DescribeScalingActivities'
  { -- | The activity IDs of the desired scaling activities. You can specify up to 50 IDs. If you omit this parameter, all activities for the past six weeks are described. If unknown activities are requested, they are ignored with no error. If you specify an Auto Scaling group, the results are limited to that group.
    activityIds :: Core.Maybe [Types.XmlString],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Core.Maybe Types.ResourceName,
    -- | The maximum number of items to return with this call. The default value is @100@ and the maximum value is @100@ .
    maxRecords :: Core.Maybe Core.Int,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.XmlString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeScalingActivities' value with any optional fields omitted.
mkDescribeScalingActivities ::
  DescribeScalingActivities
mkDescribeScalingActivities =
  DescribeScalingActivities'
    { activityIds = Core.Nothing,
      autoScalingGroupName = Core.Nothing,
      maxRecords = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The activity IDs of the desired scaling activities. You can specify up to 50 IDs. If you omit this parameter, all activities for the past six weeks are described. If unknown activities are requested, they are ignored with no error. If you specify an Auto Scaling group, the results are limited to that group.
--
-- /Note:/ Consider using 'activityIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsafActivityIds :: Lens.Lens' DescribeScalingActivities (Core.Maybe [Types.XmlString])
dsafActivityIds = Lens.field @"activityIds"
{-# DEPRECATED dsafActivityIds "Use generic-lens or generic-optics with 'activityIds' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsafAutoScalingGroupName :: Lens.Lens' DescribeScalingActivities (Core.Maybe Types.ResourceName)
dsafAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED dsafAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The maximum number of items to return with this call. The default value is @100@ and the maximum value is @100@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsafMaxRecords :: Lens.Lens' DescribeScalingActivities (Core.Maybe Core.Int)
dsafMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dsafMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsafNextToken :: Lens.Lens' DescribeScalingActivities (Core.Maybe Types.XmlString)
dsafNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsafNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeScalingActivities where
  type
    Rs DescribeScalingActivities =
      DescribeScalingActivitiesResponse
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
            ( Core.pure ("Action", "DescribeScalingActivities")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> ( Core.toQueryValue
                            "ActivityIds"
                            (Core.toQueryList "member" Core.<$> activityIds)
                        )
                Core.<> ( Core.toQueryValue "AutoScalingGroupName"
                            Core.<$> autoScalingGroupName
                        )
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeScalingActivitiesResult"
      ( \s h x ->
          DescribeScalingActivitiesResponse'
            Core.<$> ( x Core..@? "Activities" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeScalingActivities where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"activities") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeScalingActivitiesResponse' smart constructor.
data DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse'
  { -- | The scaling activities. Activities are sorted by start time. Activities still in progress are described first.
    activities :: [Types.Activity],
    -- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
    nextToken :: Core.Maybe Types.XmlString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeScalingActivitiesResponse' value with any optional fields omitted.
mkDescribeScalingActivitiesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeScalingActivitiesResponse
mkDescribeScalingActivitiesResponse responseStatus =
  DescribeScalingActivitiesResponse'
    { activities = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The scaling activities. Activities are sorted by start time. Activities still in progress are described first.
--
-- /Note:/ Consider using 'activities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarfrsActivities :: Lens.Lens' DescribeScalingActivitiesResponse [Types.Activity]
dsarfrsActivities = Lens.field @"activities"
{-# DEPRECATED dsarfrsActivities "Use generic-lens or generic-optics with 'activities' instead." #-}

-- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarfrsNextToken :: Lens.Lens' DescribeScalingActivitiesResponse (Core.Maybe Types.XmlString)
dsarfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsarfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarfrsResponseStatus :: Lens.Lens' DescribeScalingActivitiesResponse Core.Int
dsarfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsarfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

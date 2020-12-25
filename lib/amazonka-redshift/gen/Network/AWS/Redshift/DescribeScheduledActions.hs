{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeScheduledActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes properties of scheduled actions.
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeScheduledActions
  ( -- * Creating a request
    DescribeScheduledActions (..),
    mkDescribeScheduledActions,

    -- ** Request lenses
    dsasActive,
    dsasEndTime,
    dsasFilters,
    dsasMarker,
    dsasMaxRecords,
    dsasScheduledActionName,
    dsasStartTime,
    dsasTargetActionType,

    -- * Destructuring the response
    DescribeScheduledActionsResponse (..),
    mkDescribeScheduledActionsResponse,

    -- ** Response lenses
    dsarrsMarker,
    dsarrsScheduledActions,
    dsarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeScheduledActions' smart constructor.
data DescribeScheduledActions = DescribeScheduledActions'
  { -- | If true, retrieve only active scheduled actions. If false, retrieve only disabled scheduled actions.
    active :: Core.Maybe Core.Bool,
    -- | The end time in UTC of the scheduled action to retrieve. Only active scheduled actions that have invocations before this time are retrieved.
    endTime :: Core.Maybe Core.UTCTime,
    -- | List of scheduled action filters.
    filters :: Core.Maybe [Types.ScheduledActionFilter],
    -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeScheduledActions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    marker :: Core.Maybe Types.Marker,
    -- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
    --
    -- Default: @100@
    -- Constraints: minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | The name of the scheduled action to retrieve.
    scheduledActionName :: Core.Maybe Types.ScheduledActionName,
    -- | The start time in UTC of the scheduled actions to retrieve. Only active scheduled actions that have invocations after this time are retrieved.
    startTime :: Core.Maybe Core.UTCTime,
    -- | The type of the scheduled actions to retrieve.
    targetActionType :: Core.Maybe Types.ScheduledActionTypeValues
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeScheduledActions' value with any optional fields omitted.
mkDescribeScheduledActions ::
  DescribeScheduledActions
mkDescribeScheduledActions =
  DescribeScheduledActions'
    { active = Core.Nothing,
      endTime = Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      scheduledActionName = Core.Nothing,
      startTime = Core.Nothing,
      targetActionType = Core.Nothing
    }

-- | If true, retrieve only active scheduled actions. If false, retrieve only disabled scheduled actions.
--
-- /Note:/ Consider using 'active' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasActive :: Lens.Lens' DescribeScheduledActions (Core.Maybe Core.Bool)
dsasActive = Lens.field @"active"
{-# DEPRECATED dsasActive "Use generic-lens or generic-optics with 'active' instead." #-}

-- | The end time in UTC of the scheduled action to retrieve. Only active scheduled actions that have invocations before this time are retrieved.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasEndTime :: Lens.Lens' DescribeScheduledActions (Core.Maybe Core.UTCTime)
dsasEndTime = Lens.field @"endTime"
{-# DEPRECATED dsasEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | List of scheduled action filters.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasFilters :: Lens.Lens' DescribeScheduledActions (Core.Maybe [Types.ScheduledActionFilter])
dsasFilters = Lens.field @"filters"
{-# DEPRECATED dsasFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeScheduledActions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasMarker :: Lens.Lens' DescribeScheduledActions (Core.Maybe Types.Marker)
dsasMarker = Lens.field @"marker"
{-# DEPRECATED dsasMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasMaxRecords :: Lens.Lens' DescribeScheduledActions (Core.Maybe Core.Int)
dsasMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dsasMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The name of the scheduled action to retrieve.
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasScheduledActionName :: Lens.Lens' DescribeScheduledActions (Core.Maybe Types.ScheduledActionName)
dsasScheduledActionName = Lens.field @"scheduledActionName"
{-# DEPRECATED dsasScheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead." #-}

-- | The start time in UTC of the scheduled actions to retrieve. Only active scheduled actions that have invocations after this time are retrieved.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasStartTime :: Lens.Lens' DescribeScheduledActions (Core.Maybe Core.UTCTime)
dsasStartTime = Lens.field @"startTime"
{-# DEPRECATED dsasStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The type of the scheduled actions to retrieve.
--
-- /Note:/ Consider using 'targetActionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasTargetActionType :: Lens.Lens' DescribeScheduledActions (Core.Maybe Types.ScheduledActionTypeValues)
dsasTargetActionType = Lens.field @"targetActionType"
{-# DEPRECATED dsasTargetActionType "Use generic-lens or generic-optics with 'targetActionType' instead." #-}

instance Core.AWSRequest DescribeScheduledActions where
  type Rs DescribeScheduledActions = DescribeScheduledActionsResponse
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
            ( Core.pure ("Action", "DescribeScheduledActions")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "Active" Core.<$> active)
                Core.<> (Core.toQueryValue "EndTime" Core.<$> endTime)
                Core.<> ( Core.toQueryValue
                            "Filters"
                            (Core.toQueryList "ScheduledActionFilter" Core.<$> filters)
                        )
                Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> ( Core.toQueryValue "ScheduledActionName"
                            Core.<$> scheduledActionName
                        )
                Core.<> (Core.toQueryValue "StartTime" Core.<$> startTime)
                Core.<> (Core.toQueryValue "TargetActionType" Core.<$> targetActionType)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeScheduledActionsResult"
      ( \s h x ->
          DescribeScheduledActionsResponse'
            Core.<$> (x Core..@? "Marker")
            Core.<*> ( x Core..@? "ScheduledActions"
                         Core..<@> Core.parseXMLList "ScheduledAction"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeScheduledActions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"scheduledActions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | /See:/ 'mkDescribeScheduledActionsResponse' smart constructor.
data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse'
  { -- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeScheduledActions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
    marker :: Core.Maybe Types.Marker,
    -- | List of retrieved scheduled actions.
    scheduledActions :: Core.Maybe [Types.ScheduledAction],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeScheduledActionsResponse' value with any optional fields omitted.
mkDescribeScheduledActionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeScheduledActionsResponse
mkDescribeScheduledActionsResponse responseStatus =
  DescribeScheduledActionsResponse'
    { marker = Core.Nothing,
      scheduledActions = Core.Nothing,
      responseStatus
    }

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeScheduledActions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarrsMarker :: Lens.Lens' DescribeScheduledActionsResponse (Core.Maybe Types.Marker)
dsarrsMarker = Lens.field @"marker"
{-# DEPRECATED dsarrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | List of retrieved scheduled actions.
--
-- /Note:/ Consider using 'scheduledActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarrsScheduledActions :: Lens.Lens' DescribeScheduledActionsResponse (Core.Maybe [Types.ScheduledAction])
dsarrsScheduledActions = Lens.field @"scheduledActions"
{-# DEPRECATED dsarrsScheduledActions "Use generic-lens or generic-optics with 'scheduledActions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarrsResponseStatus :: Lens.Lens' DescribeScheduledActionsResponse Core.Int
dsarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

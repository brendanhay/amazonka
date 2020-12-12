{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dsasStartTime,
    dsasScheduledActionName,
    dsasFilters,
    dsasActive,
    dsasTargetActionType,
    dsasMarker,
    dsasMaxRecords,
    dsasEndTime,

    -- * Destructuring the response
    DescribeScheduledActionsResponse (..),
    mkDescribeScheduledActionsResponse,

    -- ** Response lenses
    dsarsScheduledActions,
    dsarsMarker,
    dsarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeScheduledActions' smart constructor.
data DescribeScheduledActions = DescribeScheduledActions'
  { startTime ::
      Lude.Maybe Lude.DateTime,
    scheduledActionName ::
      Lude.Maybe Lude.Text,
    filters ::
      Lude.Maybe [ScheduledActionFilter],
    active :: Lude.Maybe Lude.Bool,
    targetActionType ::
      Lude.Maybe ScheduledActionTypeValues,
    marker :: Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int,
    endTime :: Lude.Maybe Lude.DateTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScheduledActions' with the minimum fields required to make a request.
--
-- * 'active' - If true, retrieve only active scheduled actions. If false, retrieve only disabled scheduled actions.
-- * 'endTime' - The end time in UTC of the scheduled action to retrieve. Only active scheduled actions that have invocations before this time are retrieved.
-- * 'filters' - List of scheduled action filters.
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeScheduledActions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
-- * 'maxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
-- * 'scheduledActionName' - The name of the scheduled action to retrieve.
-- * 'startTime' - The start time in UTC of the scheduled actions to retrieve. Only active scheduled actions that have invocations after this time are retrieved.
-- * 'targetActionType' - The type of the scheduled actions to retrieve.
mkDescribeScheduledActions ::
  DescribeScheduledActions
mkDescribeScheduledActions =
  DescribeScheduledActions'
    { startTime = Lude.Nothing,
      scheduledActionName = Lude.Nothing,
      filters = Lude.Nothing,
      active = Lude.Nothing,
      targetActionType = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      endTime = Lude.Nothing
    }

-- | The start time in UTC of the scheduled actions to retrieve. Only active scheduled actions that have invocations after this time are retrieved.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasStartTime :: Lens.Lens' DescribeScheduledActions (Lude.Maybe Lude.DateTime)
dsasStartTime = Lens.lens (startTime :: DescribeScheduledActions -> Lude.Maybe Lude.DateTime) (\s a -> s {startTime = a} :: DescribeScheduledActions)
{-# DEPRECATED dsasStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The name of the scheduled action to retrieve.
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasScheduledActionName :: Lens.Lens' DescribeScheduledActions (Lude.Maybe Lude.Text)
dsasScheduledActionName = Lens.lens (scheduledActionName :: DescribeScheduledActions -> Lude.Maybe Lude.Text) (\s a -> s {scheduledActionName = a} :: DescribeScheduledActions)
{-# DEPRECATED dsasScheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead." #-}

-- | List of scheduled action filters.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasFilters :: Lens.Lens' DescribeScheduledActions (Lude.Maybe [ScheduledActionFilter])
dsasFilters = Lens.lens (filters :: DescribeScheduledActions -> Lude.Maybe [ScheduledActionFilter]) (\s a -> s {filters = a} :: DescribeScheduledActions)
{-# DEPRECATED dsasFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | If true, retrieve only active scheduled actions. If false, retrieve only disabled scheduled actions.
--
-- /Note:/ Consider using 'active' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasActive :: Lens.Lens' DescribeScheduledActions (Lude.Maybe Lude.Bool)
dsasActive = Lens.lens (active :: DescribeScheduledActions -> Lude.Maybe Lude.Bool) (\s a -> s {active = a} :: DescribeScheduledActions)
{-# DEPRECATED dsasActive "Use generic-lens or generic-optics with 'active' instead." #-}

-- | The type of the scheduled actions to retrieve.
--
-- /Note:/ Consider using 'targetActionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasTargetActionType :: Lens.Lens' DescribeScheduledActions (Lude.Maybe ScheduledActionTypeValues)
dsasTargetActionType = Lens.lens (targetActionType :: DescribeScheduledActions -> Lude.Maybe ScheduledActionTypeValues) (\s a -> s {targetActionType = a} :: DescribeScheduledActions)
{-# DEPRECATED dsasTargetActionType "Use generic-lens or generic-optics with 'targetActionType' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeScheduledActions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasMarker :: Lens.Lens' DescribeScheduledActions (Lude.Maybe Lude.Text)
dsasMarker = Lens.lens (marker :: DescribeScheduledActions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeScheduledActions)
{-# DEPRECATED dsasMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.
--
-- Default: @100@
-- Constraints: minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasMaxRecords :: Lens.Lens' DescribeScheduledActions (Lude.Maybe Lude.Int)
dsasMaxRecords = Lens.lens (maxRecords :: DescribeScheduledActions -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeScheduledActions)
{-# DEPRECATED dsasMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The end time in UTC of the scheduled action to retrieve. Only active scheduled actions that have invocations before this time are retrieved.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasEndTime :: Lens.Lens' DescribeScheduledActions (Lude.Maybe Lude.DateTime)
dsasEndTime = Lens.lens (endTime :: DescribeScheduledActions -> Lude.Maybe Lude.DateTime) (\s a -> s {endTime = a} :: DescribeScheduledActions)
{-# DEPRECATED dsasEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

instance Page.AWSPager DescribeScheduledActions where
  page rq rs
    | Page.stop (rs Lens.^. dsarsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dsarsScheduledActions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsasMarker Lens..~ rs Lens.^. dsarsMarker

instance Lude.AWSRequest DescribeScheduledActions where
  type Rs DescribeScheduledActions = DescribeScheduledActionsResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeScheduledActionsResult"
      ( \s h x ->
          DescribeScheduledActionsResponse'
            Lude.<$> ( x Lude..@? "ScheduledActions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "ScheduledAction")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeScheduledActions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeScheduledActions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeScheduledActions where
  toQuery DescribeScheduledActions' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeScheduledActions" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "StartTime" Lude.=: startTime,
        "ScheduledActionName" Lude.=: scheduledActionName,
        "Filters"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "ScheduledActionFilter" Lude.<$> filters),
        "Active" Lude.=: active,
        "TargetActionType" Lude.=: targetActionType,
        "Marker" Lude.=: marker,
        "MaxRecords" Lude.=: maxRecords,
        "EndTime" Lude.=: endTime
      ]

-- | /See:/ 'mkDescribeScheduledActionsResponse' smart constructor.
data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse'
  { scheduledActions ::
      Lude.Maybe
        [ScheduledAction],
    marker ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeScheduledActionsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeScheduledActions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
-- * 'responseStatus' - The response status code.
-- * 'scheduledActions' - List of retrieved scheduled actions.
mkDescribeScheduledActionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeScheduledActionsResponse
mkDescribeScheduledActionsResponse pResponseStatus_ =
  DescribeScheduledActionsResponse'
    { scheduledActions =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of retrieved scheduled actions.
--
-- /Note:/ Consider using 'scheduledActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarsScheduledActions :: Lens.Lens' DescribeScheduledActionsResponse (Lude.Maybe [ScheduledAction])
dsarsScheduledActions = Lens.lens (scheduledActions :: DescribeScheduledActionsResponse -> Lude.Maybe [ScheduledAction]) (\s a -> s {scheduledActions = a} :: DescribeScheduledActionsResponse)
{-# DEPRECATED dsarsScheduledActions "Use generic-lens or generic-optics with 'scheduledActions' instead." #-}

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeScheduledActions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarsMarker :: Lens.Lens' DescribeScheduledActionsResponse (Lude.Maybe Lude.Text)
dsarsMarker = Lens.lens (marker :: DescribeScheduledActionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeScheduledActionsResponse)
{-# DEPRECATED dsarsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarsResponseStatus :: Lens.Lens' DescribeScheduledActionsResponse Lude.Int
dsarsResponseStatus = Lens.lens (responseStatus :: DescribeScheduledActionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeScheduledActionsResponse)
{-# DEPRECATED dsarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

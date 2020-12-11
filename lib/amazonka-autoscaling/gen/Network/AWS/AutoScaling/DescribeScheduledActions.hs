{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeScheduledActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the actions scheduled for your Auto Scaling group that haven't run or that have not reached their end time. To describe the actions that have already run, call the 'DescribeScalingActivities' API.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeScheduledActions
  ( -- * Creating a request
    DescribeScheduledActions (..),
    mkDescribeScheduledActions,

    -- ** Request lenses
    dsasStartTime,
    dsasNextToken,
    dsasAutoScalingGroupName,
    dsasMaxRecords,
    dsasEndTime,
    dsasScheduledActionNames,

    -- * Destructuring the response
    DescribeScheduledActionsResponse (..),
    mkDescribeScheduledActionsResponse,

    -- ** Response lenses
    dsarsScheduledUpdateGroupActions,
    dsarsNextToken,
    dsarsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeScheduledActions' smart constructor.
data DescribeScheduledActions = DescribeScheduledActions'
  { startTime ::
      Lude.Maybe Lude.ISO8601,
    nextToken :: Lude.Maybe Lude.Text,
    autoScalingGroupName ::
      Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int,
    endTime :: Lude.Maybe Lude.ISO8601,
    scheduledActionNames ::
      Lude.Maybe [Lude.Text]
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
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'endTime' - The latest scheduled start time to return. If scheduled action names are provided, this parameter is ignored.
-- * 'maxRecords' - The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'scheduledActionNames' - The names of one or more scheduled actions. You can specify up to 50 actions. If you omit this parameter, all scheduled actions are described. If you specify an unknown scheduled action, it is ignored with no error.
-- * 'startTime' - The earliest scheduled start time to return. If scheduled action names are provided, this parameter is ignored.
mkDescribeScheduledActions ::
  DescribeScheduledActions
mkDescribeScheduledActions =
  DescribeScheduledActions'
    { startTime = Lude.Nothing,
      nextToken = Lude.Nothing,
      autoScalingGroupName = Lude.Nothing,
      maxRecords = Lude.Nothing,
      endTime = Lude.Nothing,
      scheduledActionNames = Lude.Nothing
    }

-- | The earliest scheduled start time to return. If scheduled action names are provided, this parameter is ignored.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasStartTime :: Lens.Lens' DescribeScheduledActions (Lude.Maybe Lude.ISO8601)
dsasStartTime = Lens.lens (startTime :: DescribeScheduledActions -> Lude.Maybe Lude.ISO8601) (\s a -> s {startTime = a} :: DescribeScheduledActions)
{-# DEPRECATED dsasStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasNextToken :: Lens.Lens' DescribeScheduledActions (Lude.Maybe Lude.Text)
dsasNextToken = Lens.lens (nextToken :: DescribeScheduledActions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeScheduledActions)
{-# DEPRECATED dsasNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasAutoScalingGroupName :: Lens.Lens' DescribeScheduledActions (Lude.Maybe Lude.Text)
dsasAutoScalingGroupName = Lens.lens (autoScalingGroupName :: DescribeScheduledActions -> Lude.Maybe Lude.Text) (\s a -> s {autoScalingGroupName = a} :: DescribeScheduledActions)
{-# DEPRECATED dsasAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasMaxRecords :: Lens.Lens' DescribeScheduledActions (Lude.Maybe Lude.Int)
dsasMaxRecords = Lens.lens (maxRecords :: DescribeScheduledActions -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeScheduledActions)
{-# DEPRECATED dsasMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The latest scheduled start time to return. If scheduled action names are provided, this parameter is ignored.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasEndTime :: Lens.Lens' DescribeScheduledActions (Lude.Maybe Lude.ISO8601)
dsasEndTime = Lens.lens (endTime :: DescribeScheduledActions -> Lude.Maybe Lude.ISO8601) (\s a -> s {endTime = a} :: DescribeScheduledActions)
{-# DEPRECATED dsasEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The names of one or more scheduled actions. You can specify up to 50 actions. If you omit this parameter, all scheduled actions are described. If you specify an unknown scheduled action, it is ignored with no error.
--
-- /Note:/ Consider using 'scheduledActionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsasScheduledActionNames :: Lens.Lens' DescribeScheduledActions (Lude.Maybe [Lude.Text])
dsasScheduledActionNames = Lens.lens (scheduledActionNames :: DescribeScheduledActions -> Lude.Maybe [Lude.Text]) (\s a -> s {scheduledActionNames = a} :: DescribeScheduledActions)
{-# DEPRECATED dsasScheduledActionNames "Use generic-lens or generic-optics with 'scheduledActionNames' instead." #-}

instance Page.AWSPager DescribeScheduledActions where
  page rq rs
    | Page.stop (rs Lens.^. dsarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dsarsScheduledUpdateGroupActions) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsasNextToken Lens..~ rs Lens.^. dsarsNextToken

instance Lude.AWSRequest DescribeScheduledActions where
  type Rs DescribeScheduledActions = DescribeScheduledActionsResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DescribeScheduledActionsResult"
      ( \s h x ->
          DescribeScheduledActionsResponse'
            Lude.<$> ( x Lude..@? "ScheduledUpdateGroupActions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextToken")
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
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "StartTime" Lude.=: startTime,
        "NextToken" Lude.=: nextToken,
        "AutoScalingGroupName" Lude.=: autoScalingGroupName,
        "MaxRecords" Lude.=: maxRecords,
        "EndTime" Lude.=: endTime,
        "ScheduledActionNames"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> scheduledActionNames)
      ]

-- | /See:/ 'mkDescribeScheduledActionsResponse' smart constructor.
data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse'
  { scheduledUpdateGroupActions ::
      Lude.Maybe
        [ScheduledUpdateGroupAction],
    nextToken ::
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
-- * 'nextToken' - A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
-- * 'responseStatus' - The response status code.
-- * 'scheduledUpdateGroupActions' - The scheduled actions.
mkDescribeScheduledActionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeScheduledActionsResponse
mkDescribeScheduledActionsResponse pResponseStatus_ =
  DescribeScheduledActionsResponse'
    { scheduledUpdateGroupActions =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The scheduled actions.
--
-- /Note:/ Consider using 'scheduledUpdateGroupActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarsScheduledUpdateGroupActions :: Lens.Lens' DescribeScheduledActionsResponse (Lude.Maybe [ScheduledUpdateGroupAction])
dsarsScheduledUpdateGroupActions = Lens.lens (scheduledUpdateGroupActions :: DescribeScheduledActionsResponse -> Lude.Maybe [ScheduledUpdateGroupAction]) (\s a -> s {scheduledUpdateGroupActions = a} :: DescribeScheduledActionsResponse)
{-# DEPRECATED dsarsScheduledUpdateGroupActions "Use generic-lens or generic-optics with 'scheduledUpdateGroupActions' instead." #-}

-- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarsNextToken :: Lens.Lens' DescribeScheduledActionsResponse (Lude.Maybe Lude.Text)
dsarsNextToken = Lens.lens (nextToken :: DescribeScheduledActionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeScheduledActionsResponse)
{-# DEPRECATED dsarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarsResponseStatus :: Lens.Lens' DescribeScheduledActionsResponse Lude.Int
dsarsResponseStatus = Lens.lens (responseStatus :: DescribeScheduledActionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeScheduledActionsResponse)
{-# DEPRECATED dsarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

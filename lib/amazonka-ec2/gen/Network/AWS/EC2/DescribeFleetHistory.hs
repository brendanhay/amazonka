{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeFleetHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the events for the specified EC2 Fleet during the specified time.
--
-- EC2 Fleet events are delayed by up to 30 seconds before they can be described. This ensures that you can query by the last evaluated time and not miss a recorded event. EC2 Fleet events are available for 48 hours.
module Network.AWS.EC2.DescribeFleetHistory
  ( -- * Creating a request
    DescribeFleetHistory (..),
    mkDescribeFleetHistory,

    -- ** Request lenses
    dfhNextToken,
    dfhEventType,
    dfhDryRun,
    dfhMaxResults,
    dfhFleetId,
    dfhStartTime,

    -- * Destructuring the response
    DescribeFleetHistoryResponse (..),
    mkDescribeFleetHistoryResponse,

    -- ** Response lenses
    dfhrsStartTime,
    dfhrsLastEvaluatedTime,
    dfhrsNextToken,
    dfhrsHistoryRecords,
    dfhrsFleetId,
    dfhrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeFleetHistory' smart constructor.
data DescribeFleetHistory = DescribeFleetHistory'
  { nextToken ::
      Lude.Maybe Lude.Text,
    eventType :: Lude.Maybe FleetEventType,
    dryRun :: Lude.Maybe Lude.Bool,
    maxResults :: Lude.Maybe Lude.Int,
    fleetId :: Lude.Text,
    startTime :: Lude.ISO8601
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFleetHistory' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'eventType' - The type of events to describe. By default, all events are described.
-- * 'fleetId' - The ID of the EC2 Fleet.
-- * 'maxResults' - The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
-- * 'nextToken' - The token for the next set of results.
-- * 'startTime' - The start date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
mkDescribeFleetHistory ::
  -- | 'fleetId'
  Lude.Text ->
  -- | 'startTime'
  Lude.ISO8601 ->
  DescribeFleetHistory
mkDescribeFleetHistory pFleetId_ pStartTime_ =
  DescribeFleetHistory'
    { nextToken = Lude.Nothing,
      eventType = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing,
      fleetId = pFleetId_,
      startTime = pStartTime_
    }

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhNextToken :: Lens.Lens' DescribeFleetHistory (Lude.Maybe Lude.Text)
dfhNextToken = Lens.lens (nextToken :: DescribeFleetHistory -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFleetHistory)
{-# DEPRECATED dfhNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The type of events to describe. By default, all events are described.
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhEventType :: Lens.Lens' DescribeFleetHistory (Lude.Maybe FleetEventType)
dfhEventType = Lens.lens (eventType :: DescribeFleetHistory -> Lude.Maybe FleetEventType) (\s a -> s {eventType = a} :: DescribeFleetHistory)
{-# DEPRECATED dfhEventType "Use generic-lens or generic-optics with 'eventType' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhDryRun :: Lens.Lens' DescribeFleetHistory (Lude.Maybe Lude.Bool)
dfhDryRun = Lens.lens (dryRun :: DescribeFleetHistory -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeFleetHistory)
{-# DEPRECATED dfhDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhMaxResults :: Lens.Lens' DescribeFleetHistory (Lude.Maybe Lude.Int)
dfhMaxResults = Lens.lens (maxResults :: DescribeFleetHistory -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeFleetHistory)
{-# DEPRECATED dfhMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the EC2 Fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhFleetId :: Lens.Lens' DescribeFleetHistory Lude.Text
dfhFleetId = Lens.lens (fleetId :: DescribeFleetHistory -> Lude.Text) (\s a -> s {fleetId = a} :: DescribeFleetHistory)
{-# DEPRECATED dfhFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | The start date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhStartTime :: Lens.Lens' DescribeFleetHistory Lude.ISO8601
dfhStartTime = Lens.lens (startTime :: DescribeFleetHistory -> Lude.ISO8601) (\s a -> s {startTime = a} :: DescribeFleetHistory)
{-# DEPRECATED dfhStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

instance Lude.AWSRequest DescribeFleetHistory where
  type Rs DescribeFleetHistory = DescribeFleetHistoryResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeFleetHistoryResponse'
            Lude.<$> (x Lude..@? "startTime")
            Lude.<*> (x Lude..@? "lastEvaluatedTime")
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> ( x Lude..@? "historyRecordSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "fleetId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeFleetHistory where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeFleetHistory where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeFleetHistory where
  toQuery DescribeFleetHistory' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeFleetHistory" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "EventType" Lude.=: eventType,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults,
        "FleetId" Lude.=: fleetId,
        "StartTime" Lude.=: startTime
      ]

-- | /See:/ 'mkDescribeFleetHistoryResponse' smart constructor.
data DescribeFleetHistoryResponse = DescribeFleetHistoryResponse'
  { startTime ::
      Lude.Maybe Lude.ISO8601,
    lastEvaluatedTime ::
      Lude.Maybe Lude.ISO8601,
    nextToken :: Lude.Maybe Lude.Text,
    historyRecords ::
      Lude.Maybe [HistoryRecordEntry],
    fleetId :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFleetHistoryResponse' with the minimum fields required to make a request.
--
-- * 'fleetId' - The ID of the EC Fleet.
-- * 'historyRecords' - Information about the events in the history of the EC2 Fleet.
-- * 'lastEvaluatedTime' - The last date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). All records up to this time were retrieved.
--
-- If @nextToken@ indicates that there are more results, this value is not present.
-- * 'nextToken' - The token for the next set of results.
-- * 'responseStatus' - The response status code.
-- * 'startTime' - The start date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
mkDescribeFleetHistoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeFleetHistoryResponse
mkDescribeFleetHistoryResponse pResponseStatus_ =
  DescribeFleetHistoryResponse'
    { startTime = Lude.Nothing,
      lastEvaluatedTime = Lude.Nothing,
      nextToken = Lude.Nothing,
      historyRecords = Lude.Nothing,
      fleetId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The start date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhrsStartTime :: Lens.Lens' DescribeFleetHistoryResponse (Lude.Maybe Lude.ISO8601)
dfhrsStartTime = Lens.lens (startTime :: DescribeFleetHistoryResponse -> Lude.Maybe Lude.ISO8601) (\s a -> s {startTime = a} :: DescribeFleetHistoryResponse)
{-# DEPRECATED dfhrsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The last date and time for the events, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z). All records up to this time were retrieved.
--
-- If @nextToken@ indicates that there are more results, this value is not present.
--
-- /Note:/ Consider using 'lastEvaluatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhrsLastEvaluatedTime :: Lens.Lens' DescribeFleetHistoryResponse (Lude.Maybe Lude.ISO8601)
dfhrsLastEvaluatedTime = Lens.lens (lastEvaluatedTime :: DescribeFleetHistoryResponse -> Lude.Maybe Lude.ISO8601) (\s a -> s {lastEvaluatedTime = a} :: DescribeFleetHistoryResponse)
{-# DEPRECATED dfhrsLastEvaluatedTime "Use generic-lens or generic-optics with 'lastEvaluatedTime' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhrsNextToken :: Lens.Lens' DescribeFleetHistoryResponse (Lude.Maybe Lude.Text)
dfhrsNextToken = Lens.lens (nextToken :: DescribeFleetHistoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFleetHistoryResponse)
{-# DEPRECATED dfhrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the events in the history of the EC2 Fleet.
--
-- /Note:/ Consider using 'historyRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhrsHistoryRecords :: Lens.Lens' DescribeFleetHistoryResponse (Lude.Maybe [HistoryRecordEntry])
dfhrsHistoryRecords = Lens.lens (historyRecords :: DescribeFleetHistoryResponse -> Lude.Maybe [HistoryRecordEntry]) (\s a -> s {historyRecords = a} :: DescribeFleetHistoryResponse)
{-# DEPRECATED dfhrsHistoryRecords "Use generic-lens or generic-optics with 'historyRecords' instead." #-}

-- | The ID of the EC Fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhrsFleetId :: Lens.Lens' DescribeFleetHistoryResponse (Lude.Maybe Lude.Text)
dfhrsFleetId = Lens.lens (fleetId :: DescribeFleetHistoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: DescribeFleetHistoryResponse)
{-# DEPRECATED dfhrsFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfhrsResponseStatus :: Lens.Lens' DescribeFleetHistoryResponse Lude.Int
dfhrsResponseStatus = Lens.lens (responseStatus :: DescribeFleetHistoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeFleetHistoryResponse)
{-# DEPRECATED dfhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

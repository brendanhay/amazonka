{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSpotFleetRequestHistory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the events for the specified Spot Fleet request during the
-- specified time.
--
-- Spot Fleet events are delayed by up to 30 seconds before they can be
-- described. This ensures that you can query by the last evaluated time
-- and not miss a recorded event. Spot Fleet events are available for 48
-- hours.
module Network.AWS.EC2.DescribeSpotFleetRequestHistory
  ( -- * Creating a Request
    DescribeSpotFleetRequestHistory (..),
    newDescribeSpotFleetRequestHistory,

    -- * Request Lenses
    describeSpotFleetRequestHistory_nextToken,
    describeSpotFleetRequestHistory_eventType,
    describeSpotFleetRequestHistory_dryRun,
    describeSpotFleetRequestHistory_maxResults,
    describeSpotFleetRequestHistory_spotFleetRequestId,
    describeSpotFleetRequestHistory_startTime,

    -- * Destructuring the Response
    DescribeSpotFleetRequestHistoryResponse (..),
    newDescribeSpotFleetRequestHistoryResponse,

    -- * Response Lenses
    describeSpotFleetRequestHistoryResponse_nextToken,
    describeSpotFleetRequestHistoryResponse_startTime,
    describeSpotFleetRequestHistoryResponse_historyRecords,
    describeSpotFleetRequestHistoryResponse_lastEvaluatedTime,
    describeSpotFleetRequestHistoryResponse_spotFleetRequestId,
    describeSpotFleetRequestHistoryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeSpotFleetRequestHistory.
--
-- /See:/ 'newDescribeSpotFleetRequestHistory' smart constructor.
data DescribeSpotFleetRequestHistory = DescribeSpotFleetRequestHistory'
  { -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The type of events to describe. By default, all events are described.
    eventType :: Core.Maybe EventType,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return in a single call. Specify a
    -- value between 1 and 1000. The default value is 1000. To retrieve the
    -- remaining results, make another call with the returned @NextToken@
    -- value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Core.Text,
    -- | The starting date and time for the events, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    startTime :: Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSpotFleetRequestHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSpotFleetRequestHistory_nextToken' - The token for the next set of results.
--
-- 'eventType', 'describeSpotFleetRequestHistory_eventType' - The type of events to describe. By default, all events are described.
--
-- 'dryRun', 'describeSpotFleetRequestHistory_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeSpotFleetRequestHistory_maxResults' - The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
--
-- 'spotFleetRequestId', 'describeSpotFleetRequestHistory_spotFleetRequestId' - The ID of the Spot Fleet request.
--
-- 'startTime', 'describeSpotFleetRequestHistory_startTime' - The starting date and time for the events, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
newDescribeSpotFleetRequestHistory ::
  -- | 'spotFleetRequestId'
  Core.Text ->
  -- | 'startTime'
  Core.UTCTime ->
  DescribeSpotFleetRequestHistory
newDescribeSpotFleetRequestHistory
  pSpotFleetRequestId_
  pStartTime_ =
    DescribeSpotFleetRequestHistory'
      { nextToken =
          Core.Nothing,
        eventType = Core.Nothing,
        dryRun = Core.Nothing,
        maxResults = Core.Nothing,
        spotFleetRequestId = pSpotFleetRequestId_,
        startTime = Core._Time Lens.# pStartTime_
      }

-- | The token for the next set of results.
describeSpotFleetRequestHistory_nextToken :: Lens.Lens' DescribeSpotFleetRequestHistory (Core.Maybe Core.Text)
describeSpotFleetRequestHistory_nextToken = Lens.lens (\DescribeSpotFleetRequestHistory' {nextToken} -> nextToken) (\s@DescribeSpotFleetRequestHistory' {} a -> s {nextToken = a} :: DescribeSpotFleetRequestHistory)

-- | The type of events to describe. By default, all events are described.
describeSpotFleetRequestHistory_eventType :: Lens.Lens' DescribeSpotFleetRequestHistory (Core.Maybe EventType)
describeSpotFleetRequestHistory_eventType = Lens.lens (\DescribeSpotFleetRequestHistory' {eventType} -> eventType) (\s@DescribeSpotFleetRequestHistory' {} a -> s {eventType = a} :: DescribeSpotFleetRequestHistory)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeSpotFleetRequestHistory_dryRun :: Lens.Lens' DescribeSpotFleetRequestHistory (Core.Maybe Core.Bool)
describeSpotFleetRequestHistory_dryRun = Lens.lens (\DescribeSpotFleetRequestHistory' {dryRun} -> dryRun) (\s@DescribeSpotFleetRequestHistory' {} a -> s {dryRun = a} :: DescribeSpotFleetRequestHistory)

-- | The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
describeSpotFleetRequestHistory_maxResults :: Lens.Lens' DescribeSpotFleetRequestHistory (Core.Maybe Core.Natural)
describeSpotFleetRequestHistory_maxResults = Lens.lens (\DescribeSpotFleetRequestHistory' {maxResults} -> maxResults) (\s@DescribeSpotFleetRequestHistory' {} a -> s {maxResults = a} :: DescribeSpotFleetRequestHistory)

-- | The ID of the Spot Fleet request.
describeSpotFleetRequestHistory_spotFleetRequestId :: Lens.Lens' DescribeSpotFleetRequestHistory Core.Text
describeSpotFleetRequestHistory_spotFleetRequestId = Lens.lens (\DescribeSpotFleetRequestHistory' {spotFleetRequestId} -> spotFleetRequestId) (\s@DescribeSpotFleetRequestHistory' {} a -> s {spotFleetRequestId = a} :: DescribeSpotFleetRequestHistory)

-- | The starting date and time for the events, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
describeSpotFleetRequestHistory_startTime :: Lens.Lens' DescribeSpotFleetRequestHistory Core.UTCTime
describeSpotFleetRequestHistory_startTime = Lens.lens (\DescribeSpotFleetRequestHistory' {startTime} -> startTime) (\s@DescribeSpotFleetRequestHistory' {} a -> s {startTime = a} :: DescribeSpotFleetRequestHistory) Core.. Core._Time

instance
  Core.AWSRequest
    DescribeSpotFleetRequestHistory
  where
  type
    AWSResponse DescribeSpotFleetRequestHistory =
      DescribeSpotFleetRequestHistoryResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSpotFleetRequestHistoryResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> (x Core..@? "startTime")
            Core.<*> ( x Core..@? "historyRecordSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "lastEvaluatedTime")
            Core.<*> (x Core..@? "spotFleetRequestId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeSpotFleetRequestHistory

instance Core.NFData DescribeSpotFleetRequestHistory

instance
  Core.ToHeaders
    DescribeSpotFleetRequestHistory
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeSpotFleetRequestHistory where
  toPath = Core.const "/"

instance Core.ToQuery DescribeSpotFleetRequestHistory where
  toQuery DescribeSpotFleetRequestHistory' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeSpotFleetRequestHistory" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "EventType" Core.=: eventType,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        "SpotFleetRequestId" Core.=: spotFleetRequestId,
        "StartTime" Core.=: startTime
      ]

-- | Contains the output of DescribeSpotFleetRequestHistory.
--
-- /See:/ 'newDescribeSpotFleetRequestHistoryResponse' smart constructor.
data DescribeSpotFleetRequestHistoryResponse = DescribeSpotFleetRequestHistoryResponse'
  { -- | The token required to retrieve the next set of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The starting date and time for the events, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    startTime :: Core.Maybe Core.ISO8601,
    -- | Information about the events in the history of the Spot Fleet request.
    historyRecords :: Core.Maybe [HistoryRecord],
    -- | The last date and time for the events, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). All records up to this time were
    -- retrieved.
    --
    -- If @nextToken@ indicates that there are more results, this value is not
    -- present.
    lastEvaluatedTime :: Core.Maybe Core.ISO8601,
    -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSpotFleetRequestHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSpotFleetRequestHistoryResponse_nextToken' - The token required to retrieve the next set of results. This value is
-- @null@ when there are no more results to return.
--
-- 'startTime', 'describeSpotFleetRequestHistoryResponse_startTime' - The starting date and time for the events, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
--
-- 'historyRecords', 'describeSpotFleetRequestHistoryResponse_historyRecords' - Information about the events in the history of the Spot Fleet request.
--
-- 'lastEvaluatedTime', 'describeSpotFleetRequestHistoryResponse_lastEvaluatedTime' - The last date and time for the events, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). All records up to this time were
-- retrieved.
--
-- If @nextToken@ indicates that there are more results, this value is not
-- present.
--
-- 'spotFleetRequestId', 'describeSpotFleetRequestHistoryResponse_spotFleetRequestId' - The ID of the Spot Fleet request.
--
-- 'httpStatus', 'describeSpotFleetRequestHistoryResponse_httpStatus' - The response's http status code.
newDescribeSpotFleetRequestHistoryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeSpotFleetRequestHistoryResponse
newDescribeSpotFleetRequestHistoryResponse
  pHttpStatus_ =
    DescribeSpotFleetRequestHistoryResponse'
      { nextToken =
          Core.Nothing,
        startTime = Core.Nothing,
        historyRecords = Core.Nothing,
        lastEvaluatedTime = Core.Nothing,
        spotFleetRequestId = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token required to retrieve the next set of results. This value is
-- @null@ when there are no more results to return.
describeSpotFleetRequestHistoryResponse_nextToken :: Lens.Lens' DescribeSpotFleetRequestHistoryResponse (Core.Maybe Core.Text)
describeSpotFleetRequestHistoryResponse_nextToken = Lens.lens (\DescribeSpotFleetRequestHistoryResponse' {nextToken} -> nextToken) (\s@DescribeSpotFleetRequestHistoryResponse' {} a -> s {nextToken = a} :: DescribeSpotFleetRequestHistoryResponse)

-- | The starting date and time for the events, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
describeSpotFleetRequestHistoryResponse_startTime :: Lens.Lens' DescribeSpotFleetRequestHistoryResponse (Core.Maybe Core.UTCTime)
describeSpotFleetRequestHistoryResponse_startTime = Lens.lens (\DescribeSpotFleetRequestHistoryResponse' {startTime} -> startTime) (\s@DescribeSpotFleetRequestHistoryResponse' {} a -> s {startTime = a} :: DescribeSpotFleetRequestHistoryResponse) Core.. Lens.mapping Core._Time

-- | Information about the events in the history of the Spot Fleet request.
describeSpotFleetRequestHistoryResponse_historyRecords :: Lens.Lens' DescribeSpotFleetRequestHistoryResponse (Core.Maybe [HistoryRecord])
describeSpotFleetRequestHistoryResponse_historyRecords = Lens.lens (\DescribeSpotFleetRequestHistoryResponse' {historyRecords} -> historyRecords) (\s@DescribeSpotFleetRequestHistoryResponse' {} a -> s {historyRecords = a} :: DescribeSpotFleetRequestHistoryResponse) Core.. Lens.mapping Lens._Coerce

-- | The last date and time for the events, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). All records up to this time were
-- retrieved.
--
-- If @nextToken@ indicates that there are more results, this value is not
-- present.
describeSpotFleetRequestHistoryResponse_lastEvaluatedTime :: Lens.Lens' DescribeSpotFleetRequestHistoryResponse (Core.Maybe Core.UTCTime)
describeSpotFleetRequestHistoryResponse_lastEvaluatedTime = Lens.lens (\DescribeSpotFleetRequestHistoryResponse' {lastEvaluatedTime} -> lastEvaluatedTime) (\s@DescribeSpotFleetRequestHistoryResponse' {} a -> s {lastEvaluatedTime = a} :: DescribeSpotFleetRequestHistoryResponse) Core.. Lens.mapping Core._Time

-- | The ID of the Spot Fleet request.
describeSpotFleetRequestHistoryResponse_spotFleetRequestId :: Lens.Lens' DescribeSpotFleetRequestHistoryResponse (Core.Maybe Core.Text)
describeSpotFleetRequestHistoryResponse_spotFleetRequestId = Lens.lens (\DescribeSpotFleetRequestHistoryResponse' {spotFleetRequestId} -> spotFleetRequestId) (\s@DescribeSpotFleetRequestHistoryResponse' {} a -> s {spotFleetRequestId = a} :: DescribeSpotFleetRequestHistoryResponse)

-- | The response's http status code.
describeSpotFleetRequestHistoryResponse_httpStatus :: Lens.Lens' DescribeSpotFleetRequestHistoryResponse Core.Int
describeSpotFleetRequestHistoryResponse_httpStatus = Lens.lens (\DescribeSpotFleetRequestHistoryResponse' {httpStatus} -> httpStatus) (\s@DescribeSpotFleetRequestHistoryResponse' {} a -> s {httpStatus = a} :: DescribeSpotFleetRequestHistoryResponse)

instance
  Core.NFData
    DescribeSpotFleetRequestHistoryResponse

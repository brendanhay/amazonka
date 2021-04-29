{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.DescribeFleetHistory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the events for the specified EC2 Fleet during the specified
-- time.
--
-- EC2 Fleet events are delayed by up to 30 seconds before they can be
-- described. This ensures that you can query by the last evaluated time
-- and not miss a recorded event. EC2 Fleet events are available for 48
-- hours.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet.html#monitor-ec2-fleet Monitoring your EC2 Fleet>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.EC2.DescribeFleetHistory
  ( -- * Creating a Request
    DescribeFleetHistory (..),
    newDescribeFleetHistory,

    -- * Request Lenses
    describeFleetHistory_nextToken,
    describeFleetHistory_eventType,
    describeFleetHistory_dryRun,
    describeFleetHistory_maxResults,
    describeFleetHistory_fleetId,
    describeFleetHistory_startTime,

    -- * Destructuring the Response
    DescribeFleetHistoryResponse (..),
    newDescribeFleetHistoryResponse,

    -- * Response Lenses
    describeFleetHistoryResponse_nextToken,
    describeFleetHistoryResponse_fleetId,
    describeFleetHistoryResponse_startTime,
    describeFleetHistoryResponse_historyRecords,
    describeFleetHistoryResponse_lastEvaluatedTime,
    describeFleetHistoryResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeFleetHistory' smart constructor.
data DescribeFleetHistory = DescribeFleetHistory'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of events to describe. By default, all events are described.
    eventType :: Prelude.Maybe FleetEventType,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return in a single call. Specify a
    -- value between 1 and 1000. The default value is 1000. To retrieve the
    -- remaining results, make another call with the returned @NextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The ID of the EC2 Fleet.
    fleetId :: Prelude.Text,
    -- | The start date and time for the events, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    startTime :: Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFleetHistory_nextToken' - The token for the next set of results.
--
-- 'eventType', 'describeFleetHistory_eventType' - The type of events to describe. By default, all events are described.
--
-- 'dryRun', 'describeFleetHistory_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeFleetHistory_maxResults' - The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
--
-- 'fleetId', 'describeFleetHistory_fleetId' - The ID of the EC2 Fleet.
--
-- 'startTime', 'describeFleetHistory_startTime' - The start date and time for the events, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
newDescribeFleetHistory ::
  -- | 'fleetId'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  DescribeFleetHistory
newDescribeFleetHistory pFleetId_ pStartTime_ =
  DescribeFleetHistory'
    { nextToken = Prelude.Nothing,
      eventType = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      fleetId = pFleetId_,
      startTime = Prelude._Time Lens.# pStartTime_
    }

-- | The token for the next set of results.
describeFleetHistory_nextToken :: Lens.Lens' DescribeFleetHistory (Prelude.Maybe Prelude.Text)
describeFleetHistory_nextToken = Lens.lens (\DescribeFleetHistory' {nextToken} -> nextToken) (\s@DescribeFleetHistory' {} a -> s {nextToken = a} :: DescribeFleetHistory)

-- | The type of events to describe. By default, all events are described.
describeFleetHistory_eventType :: Lens.Lens' DescribeFleetHistory (Prelude.Maybe FleetEventType)
describeFleetHistory_eventType = Lens.lens (\DescribeFleetHistory' {eventType} -> eventType) (\s@DescribeFleetHistory' {} a -> s {eventType = a} :: DescribeFleetHistory)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeFleetHistory_dryRun :: Lens.Lens' DescribeFleetHistory (Prelude.Maybe Prelude.Bool)
describeFleetHistory_dryRun = Lens.lens (\DescribeFleetHistory' {dryRun} -> dryRun) (\s@DescribeFleetHistory' {} a -> s {dryRun = a} :: DescribeFleetHistory)

-- | The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
describeFleetHistory_maxResults :: Lens.Lens' DescribeFleetHistory (Prelude.Maybe Prelude.Int)
describeFleetHistory_maxResults = Lens.lens (\DescribeFleetHistory' {maxResults} -> maxResults) (\s@DescribeFleetHistory' {} a -> s {maxResults = a} :: DescribeFleetHistory)

-- | The ID of the EC2 Fleet.
describeFleetHistory_fleetId :: Lens.Lens' DescribeFleetHistory Prelude.Text
describeFleetHistory_fleetId = Lens.lens (\DescribeFleetHistory' {fleetId} -> fleetId) (\s@DescribeFleetHistory' {} a -> s {fleetId = a} :: DescribeFleetHistory)

-- | The start date and time for the events, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
describeFleetHistory_startTime :: Lens.Lens' DescribeFleetHistory Prelude.UTCTime
describeFleetHistory_startTime = Lens.lens (\DescribeFleetHistory' {startTime} -> startTime) (\s@DescribeFleetHistory' {} a -> s {startTime = a} :: DescribeFleetHistory) Prelude.. Prelude._Time

instance Prelude.AWSRequest DescribeFleetHistory where
  type
    Rs DescribeFleetHistory =
      DescribeFleetHistoryResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeFleetHistoryResponse'
            Prelude.<$> (x Prelude..@? "nextToken")
            Prelude.<*> (x Prelude..@? "fleetId")
            Prelude.<*> (x Prelude..@? "startTime")
            Prelude.<*> ( x Prelude..@? "historyRecordSet"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (x Prelude..@? "lastEvaluatedTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFleetHistory

instance Prelude.NFData DescribeFleetHistory

instance Prelude.ToHeaders DescribeFleetHistory where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeFleetHistory where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeFleetHistory where
  toQuery DescribeFleetHistory' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeFleetHistory" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Prelude.=: nextToken,
        "EventType" Prelude.=: eventType,
        "DryRun" Prelude.=: dryRun,
        "MaxResults" Prelude.=: maxResults,
        "FleetId" Prelude.=: fleetId,
        "StartTime" Prelude.=: startTime
      ]

-- | /See:/ 'newDescribeFleetHistoryResponse' smart constructor.
data DescribeFleetHistoryResponse = DescribeFleetHistoryResponse'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the EC Fleet.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The start date and time for the events, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    startTime :: Prelude.Maybe Prelude.ISO8601,
    -- | Information about the events in the history of the EC2 Fleet.
    historyRecords :: Prelude.Maybe [HistoryRecordEntry],
    -- | The last date and time for the events, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). All records up to this time were
    -- retrieved.
    --
    -- If @nextToken@ indicates that there are more results, this value is not
    -- present.
    lastEvaluatedTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeFleetHistoryResponse_nextToken' - The token for the next set of results.
--
-- 'fleetId', 'describeFleetHistoryResponse_fleetId' - The ID of the EC Fleet.
--
-- 'startTime', 'describeFleetHistoryResponse_startTime' - The start date and time for the events, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
--
-- 'historyRecords', 'describeFleetHistoryResponse_historyRecords' - Information about the events in the history of the EC2 Fleet.
--
-- 'lastEvaluatedTime', 'describeFleetHistoryResponse_lastEvaluatedTime' - The last date and time for the events, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). All records up to this time were
-- retrieved.
--
-- If @nextToken@ indicates that there are more results, this value is not
-- present.
--
-- 'httpStatus', 'describeFleetHistoryResponse_httpStatus' - The response's http status code.
newDescribeFleetHistoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFleetHistoryResponse
newDescribeFleetHistoryResponse pHttpStatus_ =
  DescribeFleetHistoryResponse'
    { nextToken =
        Prelude.Nothing,
      fleetId = Prelude.Nothing,
      startTime = Prelude.Nothing,
      historyRecords = Prelude.Nothing,
      lastEvaluatedTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results.
describeFleetHistoryResponse_nextToken :: Lens.Lens' DescribeFleetHistoryResponse (Prelude.Maybe Prelude.Text)
describeFleetHistoryResponse_nextToken = Lens.lens (\DescribeFleetHistoryResponse' {nextToken} -> nextToken) (\s@DescribeFleetHistoryResponse' {} a -> s {nextToken = a} :: DescribeFleetHistoryResponse)

-- | The ID of the EC Fleet.
describeFleetHistoryResponse_fleetId :: Lens.Lens' DescribeFleetHistoryResponse (Prelude.Maybe Prelude.Text)
describeFleetHistoryResponse_fleetId = Lens.lens (\DescribeFleetHistoryResponse' {fleetId} -> fleetId) (\s@DescribeFleetHistoryResponse' {} a -> s {fleetId = a} :: DescribeFleetHistoryResponse)

-- | The start date and time for the events, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
describeFleetHistoryResponse_startTime :: Lens.Lens' DescribeFleetHistoryResponse (Prelude.Maybe Prelude.UTCTime)
describeFleetHistoryResponse_startTime = Lens.lens (\DescribeFleetHistoryResponse' {startTime} -> startTime) (\s@DescribeFleetHistoryResponse' {} a -> s {startTime = a} :: DescribeFleetHistoryResponse) Prelude.. Lens.mapping Prelude._Time

-- | Information about the events in the history of the EC2 Fleet.
describeFleetHistoryResponse_historyRecords :: Lens.Lens' DescribeFleetHistoryResponse (Prelude.Maybe [HistoryRecordEntry])
describeFleetHistoryResponse_historyRecords = Lens.lens (\DescribeFleetHistoryResponse' {historyRecords} -> historyRecords) (\s@DescribeFleetHistoryResponse' {} a -> s {historyRecords = a} :: DescribeFleetHistoryResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The last date and time for the events, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). All records up to this time were
-- retrieved.
--
-- If @nextToken@ indicates that there are more results, this value is not
-- present.
describeFleetHistoryResponse_lastEvaluatedTime :: Lens.Lens' DescribeFleetHistoryResponse (Prelude.Maybe Prelude.UTCTime)
describeFleetHistoryResponse_lastEvaluatedTime = Lens.lens (\DescribeFleetHistoryResponse' {lastEvaluatedTime} -> lastEvaluatedTime) (\s@DescribeFleetHistoryResponse' {} a -> s {lastEvaluatedTime = a} :: DescribeFleetHistoryResponse) Prelude.. Lens.mapping Prelude._Time

-- | The response's http status code.
describeFleetHistoryResponse_httpStatus :: Lens.Lens' DescribeFleetHistoryResponse Prelude.Int
describeFleetHistoryResponse_httpStatus = Lens.lens (\DescribeFleetHistoryResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetHistoryResponse' {} a -> s {httpStatus = a} :: DescribeFleetHistoryResponse)

instance Prelude.NFData DescribeFleetHistoryResponse

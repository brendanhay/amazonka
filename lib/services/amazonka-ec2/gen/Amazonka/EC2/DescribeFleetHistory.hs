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
-- Module      : Amazonka.EC2.DescribeFleetHistory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/fleet-monitor.html Monitor fleet events using Amazon EventBridge>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.DescribeFleetHistory
  ( -- * Creating a Request
    DescribeFleetHistory (..),
    newDescribeFleetHistory,

    -- * Request Lenses
    describeFleetHistory_dryRun,
    describeFleetHistory_eventType,
    describeFleetHistory_maxResults,
    describeFleetHistory_nextToken,
    describeFleetHistory_fleetId,
    describeFleetHistory_startTime,

    -- * Destructuring the Response
    DescribeFleetHistoryResponse (..),
    newDescribeFleetHistoryResponse,

    -- * Response Lenses
    describeFleetHistoryResponse_fleetId,
    describeFleetHistoryResponse_historyRecords,
    describeFleetHistoryResponse_lastEvaluatedTime,
    describeFleetHistoryResponse_nextToken,
    describeFleetHistoryResponse_startTime,
    describeFleetHistoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFleetHistory' smart constructor.
data DescribeFleetHistory = DescribeFleetHistory'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The type of events to describe. By default, all events are described.
    eventType :: Prelude.Maybe FleetEventType,
    -- | The maximum number of results to return in a single call. Specify a
    -- value between 1 and 1000. The default value is 1000. To retrieve the
    -- remaining results, make another call with the returned @NextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the EC2 Fleet.
    fleetId :: Prelude.Text,
    -- | The start date and time for the events, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    startTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeFleetHistory_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'eventType', 'describeFleetHistory_eventType' - The type of events to describe. By default, all events are described.
--
-- 'maxResults', 'describeFleetHistory_maxResults' - The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
--
-- 'nextToken', 'describeFleetHistory_nextToken' - The token for the next set of results.
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
    { dryRun = Prelude.Nothing,
      eventType = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      fleetId = pFleetId_,
      startTime = Data._Time Lens.# pStartTime_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeFleetHistory_dryRun :: Lens.Lens' DescribeFleetHistory (Prelude.Maybe Prelude.Bool)
describeFleetHistory_dryRun = Lens.lens (\DescribeFleetHistory' {dryRun} -> dryRun) (\s@DescribeFleetHistory' {} a -> s {dryRun = a} :: DescribeFleetHistory)

-- | The type of events to describe. By default, all events are described.
describeFleetHistory_eventType :: Lens.Lens' DescribeFleetHistory (Prelude.Maybe FleetEventType)
describeFleetHistory_eventType = Lens.lens (\DescribeFleetHistory' {eventType} -> eventType) (\s@DescribeFleetHistory' {} a -> s {eventType = a} :: DescribeFleetHistory)

-- | The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
describeFleetHistory_maxResults :: Lens.Lens' DescribeFleetHistory (Prelude.Maybe Prelude.Int)
describeFleetHistory_maxResults = Lens.lens (\DescribeFleetHistory' {maxResults} -> maxResults) (\s@DescribeFleetHistory' {} a -> s {maxResults = a} :: DescribeFleetHistory)

-- | The token for the next set of results.
describeFleetHistory_nextToken :: Lens.Lens' DescribeFleetHistory (Prelude.Maybe Prelude.Text)
describeFleetHistory_nextToken = Lens.lens (\DescribeFleetHistory' {nextToken} -> nextToken) (\s@DescribeFleetHistory' {} a -> s {nextToken = a} :: DescribeFleetHistory)

-- | The ID of the EC2 Fleet.
describeFleetHistory_fleetId :: Lens.Lens' DescribeFleetHistory Prelude.Text
describeFleetHistory_fleetId = Lens.lens (\DescribeFleetHistory' {fleetId} -> fleetId) (\s@DescribeFleetHistory' {} a -> s {fleetId = a} :: DescribeFleetHistory)

-- | The start date and time for the events, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
describeFleetHistory_startTime :: Lens.Lens' DescribeFleetHistory Prelude.UTCTime
describeFleetHistory_startTime = Lens.lens (\DescribeFleetHistory' {startTime} -> startTime) (\s@DescribeFleetHistory' {} a -> s {startTime = a} :: DescribeFleetHistory) Prelude.. Data._Time

instance Core.AWSRequest DescribeFleetHistory where
  type
    AWSResponse DescribeFleetHistory =
      DescribeFleetHistoryResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeFleetHistoryResponse'
            Prelude.<$> (x Data..@? "fleetId")
            Prelude.<*> ( x
                            Data..@? "historyRecordSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "lastEvaluatedTime")
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (x Data..@? "startTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFleetHistory where
  hashWithSalt _salt DescribeFleetHistory' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` eventType
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData DescribeFleetHistory where
  rnf DescribeFleetHistory' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf eventType
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf startTime

instance Data.ToHeaders DescribeFleetHistory where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeFleetHistory where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeFleetHistory where
  toQuery DescribeFleetHistory' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeFleetHistory" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "EventType" Data.=: eventType,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "FleetId" Data.=: fleetId,
        "StartTime" Data.=: startTime
      ]

-- | /See:/ 'newDescribeFleetHistoryResponse' smart constructor.
data DescribeFleetHistoryResponse = DescribeFleetHistoryResponse'
  { -- | The ID of the EC Fleet.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | Information about the events in the history of the EC2 Fleet.
    historyRecords :: Prelude.Maybe [HistoryRecordEntry],
    -- | The last date and time for the events, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). All records up to this time were
    -- retrieved.
    --
    -- If @nextToken@ indicates that there are more results, this value is not
    -- present.
    lastEvaluatedTime :: Prelude.Maybe Data.ISO8601,
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The start date and time for the events, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    startTime :: Prelude.Maybe Data.ISO8601,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'describeFleetHistoryResponse_fleetId' - The ID of the EC Fleet.
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
-- 'nextToken', 'describeFleetHistoryResponse_nextToken' - The token for the next set of results.
--
-- 'startTime', 'describeFleetHistoryResponse_startTime' - The start date and time for the events, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
--
-- 'httpStatus', 'describeFleetHistoryResponse_httpStatus' - The response's http status code.
newDescribeFleetHistoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFleetHistoryResponse
newDescribeFleetHistoryResponse pHttpStatus_ =
  DescribeFleetHistoryResponse'
    { fleetId =
        Prelude.Nothing,
      historyRecords = Prelude.Nothing,
      lastEvaluatedTime = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      startTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the EC Fleet.
describeFleetHistoryResponse_fleetId :: Lens.Lens' DescribeFleetHistoryResponse (Prelude.Maybe Prelude.Text)
describeFleetHistoryResponse_fleetId = Lens.lens (\DescribeFleetHistoryResponse' {fleetId} -> fleetId) (\s@DescribeFleetHistoryResponse' {} a -> s {fleetId = a} :: DescribeFleetHistoryResponse)

-- | Information about the events in the history of the EC2 Fleet.
describeFleetHistoryResponse_historyRecords :: Lens.Lens' DescribeFleetHistoryResponse (Prelude.Maybe [HistoryRecordEntry])
describeFleetHistoryResponse_historyRecords = Lens.lens (\DescribeFleetHistoryResponse' {historyRecords} -> historyRecords) (\s@DescribeFleetHistoryResponse' {} a -> s {historyRecords = a} :: DescribeFleetHistoryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The last date and time for the events, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). All records up to this time were
-- retrieved.
--
-- If @nextToken@ indicates that there are more results, this value is not
-- present.
describeFleetHistoryResponse_lastEvaluatedTime :: Lens.Lens' DescribeFleetHistoryResponse (Prelude.Maybe Prelude.UTCTime)
describeFleetHistoryResponse_lastEvaluatedTime = Lens.lens (\DescribeFleetHistoryResponse' {lastEvaluatedTime} -> lastEvaluatedTime) (\s@DescribeFleetHistoryResponse' {} a -> s {lastEvaluatedTime = a} :: DescribeFleetHistoryResponse) Prelude.. Lens.mapping Data._Time

-- | The token for the next set of results.
describeFleetHistoryResponse_nextToken :: Lens.Lens' DescribeFleetHistoryResponse (Prelude.Maybe Prelude.Text)
describeFleetHistoryResponse_nextToken = Lens.lens (\DescribeFleetHistoryResponse' {nextToken} -> nextToken) (\s@DescribeFleetHistoryResponse' {} a -> s {nextToken = a} :: DescribeFleetHistoryResponse)

-- | The start date and time for the events, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
describeFleetHistoryResponse_startTime :: Lens.Lens' DescribeFleetHistoryResponse (Prelude.Maybe Prelude.UTCTime)
describeFleetHistoryResponse_startTime = Lens.lens (\DescribeFleetHistoryResponse' {startTime} -> startTime) (\s@DescribeFleetHistoryResponse' {} a -> s {startTime = a} :: DescribeFleetHistoryResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describeFleetHistoryResponse_httpStatus :: Lens.Lens' DescribeFleetHistoryResponse Prelude.Int
describeFleetHistoryResponse_httpStatus = Lens.lens (\DescribeFleetHistoryResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetHistoryResponse' {} a -> s {httpStatus = a} :: DescribeFleetHistoryResponse)

instance Prelude.NFData DescribeFleetHistoryResponse where
  rnf DescribeFleetHistoryResponse' {..} =
    Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf historyRecords
      `Prelude.seq` Prelude.rnf lastEvaluatedTime
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf httpStatus

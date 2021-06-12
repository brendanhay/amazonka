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
-- Module      : Network.AWS.CloudWatch.DescribeAlarmHistory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the history for the specified alarm. You can filter the
-- results by date range or item type. If an alarm name is not specified,
-- the histories for either all metric alarms or all composite alarms are
-- returned.
--
-- CloudWatch retains the history of an alarm even if you delete the alarm.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatch.DescribeAlarmHistory
  ( -- * Creating a Request
    DescribeAlarmHistory (..),
    newDescribeAlarmHistory,

    -- * Request Lenses
    describeAlarmHistory_nextToken,
    describeAlarmHistory_startDate,
    describeAlarmHistory_alarmTypes,
    describeAlarmHistory_historyItemType,
    describeAlarmHistory_scanBy,
    describeAlarmHistory_alarmName,
    describeAlarmHistory_endDate,
    describeAlarmHistory_maxRecords,

    -- * Destructuring the Response
    DescribeAlarmHistoryResponse (..),
    newDescribeAlarmHistoryResponse,

    -- * Response Lenses
    describeAlarmHistoryResponse_nextToken,
    describeAlarmHistoryResponse_alarmHistoryItems,
    describeAlarmHistoryResponse_httpStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAlarmHistory' smart constructor.
data DescribeAlarmHistory = DescribeAlarmHistory'
  { -- | The token returned by a previous call to indicate that there is more
    -- data available.
    nextToken :: Core.Maybe Core.Text,
    -- | The starting date to retrieve alarm history.
    startDate :: Core.Maybe Core.ISO8601,
    -- | Use this parameter to specify whether you want the operation to return
    -- metric alarms or composite alarms. If you omit this parameter, only
    -- metric alarms are returned.
    alarmTypes :: Core.Maybe [AlarmType],
    -- | The type of alarm histories to retrieve.
    historyItemType :: Core.Maybe HistoryItemType,
    -- | Specified whether to return the newest or oldest alarm history first.
    -- Specify @TimestampDescending@ to have the newest event history returned
    -- first, and specify @TimestampAscending@ to have the oldest history
    -- returned first.
    scanBy :: Core.Maybe ScanBy,
    -- | The name of the alarm.
    alarmName :: Core.Maybe Core.Text,
    -- | The ending date to retrieve alarm history.
    endDate :: Core.Maybe Core.ISO8601,
    -- | The maximum number of alarm history records to retrieve.
    maxRecords :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAlarmHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAlarmHistory_nextToken' - The token returned by a previous call to indicate that there is more
-- data available.
--
-- 'startDate', 'describeAlarmHistory_startDate' - The starting date to retrieve alarm history.
--
-- 'alarmTypes', 'describeAlarmHistory_alarmTypes' - Use this parameter to specify whether you want the operation to return
-- metric alarms or composite alarms. If you omit this parameter, only
-- metric alarms are returned.
--
-- 'historyItemType', 'describeAlarmHistory_historyItemType' - The type of alarm histories to retrieve.
--
-- 'scanBy', 'describeAlarmHistory_scanBy' - Specified whether to return the newest or oldest alarm history first.
-- Specify @TimestampDescending@ to have the newest event history returned
-- first, and specify @TimestampAscending@ to have the oldest history
-- returned first.
--
-- 'alarmName', 'describeAlarmHistory_alarmName' - The name of the alarm.
--
-- 'endDate', 'describeAlarmHistory_endDate' - The ending date to retrieve alarm history.
--
-- 'maxRecords', 'describeAlarmHistory_maxRecords' - The maximum number of alarm history records to retrieve.
newDescribeAlarmHistory ::
  DescribeAlarmHistory
newDescribeAlarmHistory =
  DescribeAlarmHistory'
    { nextToken = Core.Nothing,
      startDate = Core.Nothing,
      alarmTypes = Core.Nothing,
      historyItemType = Core.Nothing,
      scanBy = Core.Nothing,
      alarmName = Core.Nothing,
      endDate = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The token returned by a previous call to indicate that there is more
-- data available.
describeAlarmHistory_nextToken :: Lens.Lens' DescribeAlarmHistory (Core.Maybe Core.Text)
describeAlarmHistory_nextToken = Lens.lens (\DescribeAlarmHistory' {nextToken} -> nextToken) (\s@DescribeAlarmHistory' {} a -> s {nextToken = a} :: DescribeAlarmHistory)

-- | The starting date to retrieve alarm history.
describeAlarmHistory_startDate :: Lens.Lens' DescribeAlarmHistory (Core.Maybe Core.UTCTime)
describeAlarmHistory_startDate = Lens.lens (\DescribeAlarmHistory' {startDate} -> startDate) (\s@DescribeAlarmHistory' {} a -> s {startDate = a} :: DescribeAlarmHistory) Core.. Lens.mapping Core._Time

-- | Use this parameter to specify whether you want the operation to return
-- metric alarms or composite alarms. If you omit this parameter, only
-- metric alarms are returned.
describeAlarmHistory_alarmTypes :: Lens.Lens' DescribeAlarmHistory (Core.Maybe [AlarmType])
describeAlarmHistory_alarmTypes = Lens.lens (\DescribeAlarmHistory' {alarmTypes} -> alarmTypes) (\s@DescribeAlarmHistory' {} a -> s {alarmTypes = a} :: DescribeAlarmHistory) Core.. Lens.mapping Lens._Coerce

-- | The type of alarm histories to retrieve.
describeAlarmHistory_historyItemType :: Lens.Lens' DescribeAlarmHistory (Core.Maybe HistoryItemType)
describeAlarmHistory_historyItemType = Lens.lens (\DescribeAlarmHistory' {historyItemType} -> historyItemType) (\s@DescribeAlarmHistory' {} a -> s {historyItemType = a} :: DescribeAlarmHistory)

-- | Specified whether to return the newest or oldest alarm history first.
-- Specify @TimestampDescending@ to have the newest event history returned
-- first, and specify @TimestampAscending@ to have the oldest history
-- returned first.
describeAlarmHistory_scanBy :: Lens.Lens' DescribeAlarmHistory (Core.Maybe ScanBy)
describeAlarmHistory_scanBy = Lens.lens (\DescribeAlarmHistory' {scanBy} -> scanBy) (\s@DescribeAlarmHistory' {} a -> s {scanBy = a} :: DescribeAlarmHistory)

-- | The name of the alarm.
describeAlarmHistory_alarmName :: Lens.Lens' DescribeAlarmHistory (Core.Maybe Core.Text)
describeAlarmHistory_alarmName = Lens.lens (\DescribeAlarmHistory' {alarmName} -> alarmName) (\s@DescribeAlarmHistory' {} a -> s {alarmName = a} :: DescribeAlarmHistory)

-- | The ending date to retrieve alarm history.
describeAlarmHistory_endDate :: Lens.Lens' DescribeAlarmHistory (Core.Maybe Core.UTCTime)
describeAlarmHistory_endDate = Lens.lens (\DescribeAlarmHistory' {endDate} -> endDate) (\s@DescribeAlarmHistory' {} a -> s {endDate = a} :: DescribeAlarmHistory) Core.. Lens.mapping Core._Time

-- | The maximum number of alarm history records to retrieve.
describeAlarmHistory_maxRecords :: Lens.Lens' DescribeAlarmHistory (Core.Maybe Core.Natural)
describeAlarmHistory_maxRecords = Lens.lens (\DescribeAlarmHistory' {maxRecords} -> maxRecords) (\s@DescribeAlarmHistory' {} a -> s {maxRecords = a} :: DescribeAlarmHistory)

instance Core.AWSPager DescribeAlarmHistory where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeAlarmHistoryResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeAlarmHistoryResponse_alarmHistoryItems
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeAlarmHistory_nextToken
          Lens..~ rs
          Lens.^? describeAlarmHistoryResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeAlarmHistory where
  type
    AWSResponse DescribeAlarmHistory =
      DescribeAlarmHistoryResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeAlarmHistoryResult"
      ( \s h x ->
          DescribeAlarmHistoryResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "AlarmHistoryItems" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeAlarmHistory

instance Core.NFData DescribeAlarmHistory

instance Core.ToHeaders DescribeAlarmHistory where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeAlarmHistory where
  toPath = Core.const "/"

instance Core.ToQuery DescribeAlarmHistory where
  toQuery DescribeAlarmHistory' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeAlarmHistory" :: Core.ByteString),
        "Version" Core.=: ("2010-08-01" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "StartDate" Core.=: startDate,
        "AlarmTypes"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> alarmTypes),
        "HistoryItemType" Core.=: historyItemType,
        "ScanBy" Core.=: scanBy,
        "AlarmName" Core.=: alarmName,
        "EndDate" Core.=: endDate,
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeAlarmHistoryResponse' smart constructor.
data DescribeAlarmHistoryResponse = DescribeAlarmHistoryResponse'
  { -- | The token that marks the start of the next batch of returned results.
    nextToken :: Core.Maybe Core.Text,
    -- | The alarm histories, in JSON format.
    alarmHistoryItems :: Core.Maybe [AlarmHistoryItem],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAlarmHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeAlarmHistoryResponse_nextToken' - The token that marks the start of the next batch of returned results.
--
-- 'alarmHistoryItems', 'describeAlarmHistoryResponse_alarmHistoryItems' - The alarm histories, in JSON format.
--
-- 'httpStatus', 'describeAlarmHistoryResponse_httpStatus' - The response's http status code.
newDescribeAlarmHistoryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAlarmHistoryResponse
newDescribeAlarmHistoryResponse pHttpStatus_ =
  DescribeAlarmHistoryResponse'
    { nextToken =
        Core.Nothing,
      alarmHistoryItems = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that marks the start of the next batch of returned results.
describeAlarmHistoryResponse_nextToken :: Lens.Lens' DescribeAlarmHistoryResponse (Core.Maybe Core.Text)
describeAlarmHistoryResponse_nextToken = Lens.lens (\DescribeAlarmHistoryResponse' {nextToken} -> nextToken) (\s@DescribeAlarmHistoryResponse' {} a -> s {nextToken = a} :: DescribeAlarmHistoryResponse)

-- | The alarm histories, in JSON format.
describeAlarmHistoryResponse_alarmHistoryItems :: Lens.Lens' DescribeAlarmHistoryResponse (Core.Maybe [AlarmHistoryItem])
describeAlarmHistoryResponse_alarmHistoryItems = Lens.lens (\DescribeAlarmHistoryResponse' {alarmHistoryItems} -> alarmHistoryItems) (\s@DescribeAlarmHistoryResponse' {} a -> s {alarmHistoryItems = a} :: DescribeAlarmHistoryResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAlarmHistoryResponse_httpStatus :: Lens.Lens' DescribeAlarmHistoryResponse Core.Int
describeAlarmHistoryResponse_httpStatus = Lens.lens (\DescribeAlarmHistoryResponse' {httpStatus} -> httpStatus) (\s@DescribeAlarmHistoryResponse' {} a -> s {httpStatus = a} :: DescribeAlarmHistoryResponse)

instance Core.NFData DescribeAlarmHistoryResponse

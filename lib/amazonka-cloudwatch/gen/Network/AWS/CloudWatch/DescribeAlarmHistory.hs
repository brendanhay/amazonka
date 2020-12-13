{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DescribeAlarmHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the history for the specified alarm. You can filter the results by date range or item type. If an alarm name is not specified, the histories for either all metric alarms or all composite alarms are returned.
--
-- CloudWatch retains the history of an alarm even if you delete the alarm.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatch.DescribeAlarmHistory
  ( -- * Creating a request
    DescribeAlarmHistory (..),
    mkDescribeAlarmHistory,

    -- ** Request lenses
    dahAlarmName,
    dahHistoryItemType,
    dahAlarmTypes,
    dahEndDate,
    dahStartDate,
    dahNextToken,
    dahScanBy,
    dahMaxRecords,

    -- * Destructuring the response
    DescribeAlarmHistoryResponse (..),
    mkDescribeAlarmHistoryResponse,

    -- ** Response lenses
    dahrsAlarmHistoryItems,
    dahrsNextToken,
    dahrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAlarmHistory' smart constructor.
data DescribeAlarmHistory = DescribeAlarmHistory'
  { -- | The name of the alarm.
    alarmName :: Lude.Maybe Lude.Text,
    -- | The type of alarm histories to retrieve.
    historyItemType :: Lude.Maybe HistoryItemType,
    -- | Use this parameter to specify whether you want the operation to return metric alarms or composite alarms. If you omit this parameter, only metric alarms are returned.
    alarmTypes :: Lude.Maybe [AlarmType],
    -- | The ending date to retrieve alarm history.
    endDate :: Lude.Maybe Lude.DateTime,
    -- | The starting date to retrieve alarm history.
    startDate :: Lude.Maybe Lude.DateTime,
    -- | The token returned by a previous call to indicate that there is more data available.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Specified whether to return the newest or oldest alarm history first. Specify @TimestampDescending@ to have the newest event history returned first, and specify @TimestampAscending@ to have the oldest history returned first.
    scanBy :: Lude.Maybe ScanBy,
    -- | The maximum number of alarm history records to retrieve.
    maxRecords :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAlarmHistory' with the minimum fields required to make a request.
--
-- * 'alarmName' - The name of the alarm.
-- * 'historyItemType' - The type of alarm histories to retrieve.
-- * 'alarmTypes' - Use this parameter to specify whether you want the operation to return metric alarms or composite alarms. If you omit this parameter, only metric alarms are returned.
-- * 'endDate' - The ending date to retrieve alarm history.
-- * 'startDate' - The starting date to retrieve alarm history.
-- * 'nextToken' - The token returned by a previous call to indicate that there is more data available.
-- * 'scanBy' - Specified whether to return the newest or oldest alarm history first. Specify @TimestampDescending@ to have the newest event history returned first, and specify @TimestampAscending@ to have the oldest history returned first.
-- * 'maxRecords' - The maximum number of alarm history records to retrieve.
mkDescribeAlarmHistory ::
  DescribeAlarmHistory
mkDescribeAlarmHistory =
  DescribeAlarmHistory'
    { alarmName = Lude.Nothing,
      historyItemType = Lude.Nothing,
      alarmTypes = Lude.Nothing,
      endDate = Lude.Nothing,
      startDate = Lude.Nothing,
      nextToken = Lude.Nothing,
      scanBy = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The name of the alarm.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahAlarmName :: Lens.Lens' DescribeAlarmHistory (Lude.Maybe Lude.Text)
dahAlarmName = Lens.lens (alarmName :: DescribeAlarmHistory -> Lude.Maybe Lude.Text) (\s a -> s {alarmName = a} :: DescribeAlarmHistory)
{-# DEPRECATED dahAlarmName "Use generic-lens or generic-optics with 'alarmName' instead." #-}

-- | The type of alarm histories to retrieve.
--
-- /Note:/ Consider using 'historyItemType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahHistoryItemType :: Lens.Lens' DescribeAlarmHistory (Lude.Maybe HistoryItemType)
dahHistoryItemType = Lens.lens (historyItemType :: DescribeAlarmHistory -> Lude.Maybe HistoryItemType) (\s a -> s {historyItemType = a} :: DescribeAlarmHistory)
{-# DEPRECATED dahHistoryItemType "Use generic-lens or generic-optics with 'historyItemType' instead." #-}

-- | Use this parameter to specify whether you want the operation to return metric alarms or composite alarms. If you omit this parameter, only metric alarms are returned.
--
-- /Note:/ Consider using 'alarmTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahAlarmTypes :: Lens.Lens' DescribeAlarmHistory (Lude.Maybe [AlarmType])
dahAlarmTypes = Lens.lens (alarmTypes :: DescribeAlarmHistory -> Lude.Maybe [AlarmType]) (\s a -> s {alarmTypes = a} :: DescribeAlarmHistory)
{-# DEPRECATED dahAlarmTypes "Use generic-lens or generic-optics with 'alarmTypes' instead." #-}

-- | The ending date to retrieve alarm history.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahEndDate :: Lens.Lens' DescribeAlarmHistory (Lude.Maybe Lude.DateTime)
dahEndDate = Lens.lens (endDate :: DescribeAlarmHistory -> Lude.Maybe Lude.DateTime) (\s a -> s {endDate = a} :: DescribeAlarmHistory)
{-# DEPRECATED dahEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

-- | The starting date to retrieve alarm history.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahStartDate :: Lens.Lens' DescribeAlarmHistory (Lude.Maybe Lude.DateTime)
dahStartDate = Lens.lens (startDate :: DescribeAlarmHistory -> Lude.Maybe Lude.DateTime) (\s a -> s {startDate = a} :: DescribeAlarmHistory)
{-# DEPRECATED dahStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

-- | The token returned by a previous call to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahNextToken :: Lens.Lens' DescribeAlarmHistory (Lude.Maybe Lude.Text)
dahNextToken = Lens.lens (nextToken :: DescribeAlarmHistory -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAlarmHistory)
{-# DEPRECATED dahNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specified whether to return the newest or oldest alarm history first. Specify @TimestampDescending@ to have the newest event history returned first, and specify @TimestampAscending@ to have the oldest history returned first.
--
-- /Note:/ Consider using 'scanBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahScanBy :: Lens.Lens' DescribeAlarmHistory (Lude.Maybe ScanBy)
dahScanBy = Lens.lens (scanBy :: DescribeAlarmHistory -> Lude.Maybe ScanBy) (\s a -> s {scanBy = a} :: DescribeAlarmHistory)
{-# DEPRECATED dahScanBy "Use generic-lens or generic-optics with 'scanBy' instead." #-}

-- | The maximum number of alarm history records to retrieve.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahMaxRecords :: Lens.Lens' DescribeAlarmHistory (Lude.Maybe Lude.Natural)
dahMaxRecords = Lens.lens (maxRecords :: DescribeAlarmHistory -> Lude.Maybe Lude.Natural) (\s a -> s {maxRecords = a} :: DescribeAlarmHistory)
{-# DEPRECATED dahMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeAlarmHistory where
  page rq rs
    | Page.stop (rs Lens.^. dahrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dahrsAlarmHistoryItems) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dahNextToken Lens..~ rs Lens.^. dahrsNextToken

instance Lude.AWSRequest DescribeAlarmHistory where
  type Rs DescribeAlarmHistory = DescribeAlarmHistoryResponse
  request = Req.postQuery cloudWatchService
  response =
    Res.receiveXMLWrapper
      "DescribeAlarmHistoryResult"
      ( \s h x ->
          DescribeAlarmHistoryResponse'
            Lude.<$> ( x Lude..@? "AlarmHistoryItems" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAlarmHistory where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAlarmHistory where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAlarmHistory where
  toQuery DescribeAlarmHistory' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeAlarmHistory" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "AlarmName" Lude.=: alarmName,
        "HistoryItemType" Lude.=: historyItemType,
        "AlarmTypes"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> alarmTypes),
        "EndDate" Lude.=: endDate,
        "StartDate" Lude.=: startDate,
        "NextToken" Lude.=: nextToken,
        "ScanBy" Lude.=: scanBy,
        "MaxRecords" Lude.=: maxRecords
      ]

-- | /See:/ 'mkDescribeAlarmHistoryResponse' smart constructor.
data DescribeAlarmHistoryResponse = DescribeAlarmHistoryResponse'
  { -- | The alarm histories, in JSON format.
    alarmHistoryItems :: Lude.Maybe [AlarmHistoryItem],
    -- | The token that marks the start of the next batch of returned results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAlarmHistoryResponse' with the minimum fields required to make a request.
--
-- * 'alarmHistoryItems' - The alarm histories, in JSON format.
-- * 'nextToken' - The token that marks the start of the next batch of returned results.
-- * 'responseStatus' - The response status code.
mkDescribeAlarmHistoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAlarmHistoryResponse
mkDescribeAlarmHistoryResponse pResponseStatus_ =
  DescribeAlarmHistoryResponse'
    { alarmHistoryItems = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The alarm histories, in JSON format.
--
-- /Note:/ Consider using 'alarmHistoryItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahrsAlarmHistoryItems :: Lens.Lens' DescribeAlarmHistoryResponse (Lude.Maybe [AlarmHistoryItem])
dahrsAlarmHistoryItems = Lens.lens (alarmHistoryItems :: DescribeAlarmHistoryResponse -> Lude.Maybe [AlarmHistoryItem]) (\s a -> s {alarmHistoryItems = a} :: DescribeAlarmHistoryResponse)
{-# DEPRECATED dahrsAlarmHistoryItems "Use generic-lens or generic-optics with 'alarmHistoryItems' instead." #-}

-- | The token that marks the start of the next batch of returned results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahrsNextToken :: Lens.Lens' DescribeAlarmHistoryResponse (Lude.Maybe Lude.Text)
dahrsNextToken = Lens.lens (nextToken :: DescribeAlarmHistoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAlarmHistoryResponse)
{-# DEPRECATED dahrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahrsResponseStatus :: Lens.Lens' DescribeAlarmHistoryResponse Lude.Int
dahrsResponseStatus = Lens.lens (responseStatus :: DescribeAlarmHistoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAlarmHistoryResponse)
{-# DEPRECATED dahrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

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
    dahAlarmTypes,
    dahEndDate,
    dahHistoryItemType,
    dahMaxRecords,
    dahNextToken,
    dahScanBy,
    dahStartDate,

    -- * Destructuring the response
    DescribeAlarmHistoryResponse (..),
    mkDescribeAlarmHistoryResponse,

    -- ** Response lenses
    dahrrsAlarmHistoryItems,
    dahrrsNextToken,
    dahrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAlarmHistory' smart constructor.
data DescribeAlarmHistory = DescribeAlarmHistory'
  { -- | The name of the alarm.
    alarmName :: Core.Maybe Types.AlarmName,
    -- | Use this parameter to specify whether you want the operation to return metric alarms or composite alarms. If you omit this parameter, only metric alarms are returned.
    alarmTypes :: Core.Maybe [Types.AlarmType],
    -- | The ending date to retrieve alarm history.
    endDate :: Core.Maybe Core.UTCTime,
    -- | The type of alarm histories to retrieve.
    historyItemType :: Core.Maybe Types.HistoryItemType,
    -- | The maximum number of alarm history records to retrieve.
    maxRecords :: Core.Maybe Core.Natural,
    -- | The token returned by a previous call to indicate that there is more data available.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Specified whether to return the newest or oldest alarm history first. Specify @TimestampDescending@ to have the newest event history returned first, and specify @TimestampAscending@ to have the oldest history returned first.
    scanBy :: Core.Maybe Types.ScanBy,
    -- | The starting date to retrieve alarm history.
    startDate :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAlarmHistory' value with any optional fields omitted.
mkDescribeAlarmHistory ::
  DescribeAlarmHistory
mkDescribeAlarmHistory =
  DescribeAlarmHistory'
    { alarmName = Core.Nothing,
      alarmTypes = Core.Nothing,
      endDate = Core.Nothing,
      historyItemType = Core.Nothing,
      maxRecords = Core.Nothing,
      nextToken = Core.Nothing,
      scanBy = Core.Nothing,
      startDate = Core.Nothing
    }

-- | The name of the alarm.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahAlarmName :: Lens.Lens' DescribeAlarmHistory (Core.Maybe Types.AlarmName)
dahAlarmName = Lens.field @"alarmName"
{-# DEPRECATED dahAlarmName "Use generic-lens or generic-optics with 'alarmName' instead." #-}

-- | Use this parameter to specify whether you want the operation to return metric alarms or composite alarms. If you omit this parameter, only metric alarms are returned.
--
-- /Note:/ Consider using 'alarmTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahAlarmTypes :: Lens.Lens' DescribeAlarmHistory (Core.Maybe [Types.AlarmType])
dahAlarmTypes = Lens.field @"alarmTypes"
{-# DEPRECATED dahAlarmTypes "Use generic-lens or generic-optics with 'alarmTypes' instead." #-}

-- | The ending date to retrieve alarm history.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahEndDate :: Lens.Lens' DescribeAlarmHistory (Core.Maybe Core.UTCTime)
dahEndDate = Lens.field @"endDate"
{-# DEPRECATED dahEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

-- | The type of alarm histories to retrieve.
--
-- /Note:/ Consider using 'historyItemType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahHistoryItemType :: Lens.Lens' DescribeAlarmHistory (Core.Maybe Types.HistoryItemType)
dahHistoryItemType = Lens.field @"historyItemType"
{-# DEPRECATED dahHistoryItemType "Use generic-lens or generic-optics with 'historyItemType' instead." #-}

-- | The maximum number of alarm history records to retrieve.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahMaxRecords :: Lens.Lens' DescribeAlarmHistory (Core.Maybe Core.Natural)
dahMaxRecords = Lens.field @"maxRecords"
{-# DEPRECATED dahMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The token returned by a previous call to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahNextToken :: Lens.Lens' DescribeAlarmHistory (Core.Maybe Types.NextToken)
dahNextToken = Lens.field @"nextToken"
{-# DEPRECATED dahNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specified whether to return the newest or oldest alarm history first. Specify @TimestampDescending@ to have the newest event history returned first, and specify @TimestampAscending@ to have the oldest history returned first.
--
-- /Note:/ Consider using 'scanBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahScanBy :: Lens.Lens' DescribeAlarmHistory (Core.Maybe Types.ScanBy)
dahScanBy = Lens.field @"scanBy"
{-# DEPRECATED dahScanBy "Use generic-lens or generic-optics with 'scanBy' instead." #-}

-- | The starting date to retrieve alarm history.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahStartDate :: Lens.Lens' DescribeAlarmHistory (Core.Maybe Core.UTCTime)
dahStartDate = Lens.field @"startDate"
{-# DEPRECATED dahStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

instance Core.AWSRequest DescribeAlarmHistory where
  type Rs DescribeAlarmHistory = DescribeAlarmHistoryResponse
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
            ( Core.pure ("Action", "DescribeAlarmHistory")
                Core.<> (Core.pure ("Version", "2010-08-01"))
                Core.<> (Core.toQueryValue "AlarmName" Core.<$> alarmName)
                Core.<> ( Core.toQueryValue
                            "AlarmTypes"
                            (Core.toQueryList "member" Core.<$> alarmTypes)
                        )
                Core.<> (Core.toQueryValue "EndDate" Core.<$> endDate)
                Core.<> (Core.toQueryValue "HistoryItemType" Core.<$> historyItemType)
                Core.<> (Core.toQueryValue "MaxRecords" Core.<$> maxRecords)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> (Core.toQueryValue "ScanBy" Core.<$> scanBy)
                Core.<> (Core.toQueryValue "StartDate" Core.<$> startDate)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeAlarmHistoryResult"
      ( \s h x ->
          DescribeAlarmHistoryResponse'
            Core.<$> ( x Core..@? "AlarmHistoryItems"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeAlarmHistory where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"alarmHistoryItems" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeAlarmHistoryResponse' smart constructor.
data DescribeAlarmHistoryResponse = DescribeAlarmHistoryResponse'
  { -- | The alarm histories, in JSON format.
    alarmHistoryItems :: Core.Maybe [Types.AlarmHistoryItem],
    -- | The token that marks the start of the next batch of returned results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAlarmHistoryResponse' value with any optional fields omitted.
mkDescribeAlarmHistoryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAlarmHistoryResponse
mkDescribeAlarmHistoryResponse responseStatus =
  DescribeAlarmHistoryResponse'
    { alarmHistoryItems = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The alarm histories, in JSON format.
--
-- /Note:/ Consider using 'alarmHistoryItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahrrsAlarmHistoryItems :: Lens.Lens' DescribeAlarmHistoryResponse (Core.Maybe [Types.AlarmHistoryItem])
dahrrsAlarmHistoryItems = Lens.field @"alarmHistoryItems"
{-# DEPRECATED dahrrsAlarmHistoryItems "Use generic-lens or generic-optics with 'alarmHistoryItems' instead." #-}

-- | The token that marks the start of the next batch of returned results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahrrsNextToken :: Lens.Lens' DescribeAlarmHistoryResponse (Core.Maybe Types.NextToken)
dahrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dahrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahrrsResponseStatus :: Lens.Lens' DescribeAlarmHistoryResponse Core.Int
dahrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dahrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

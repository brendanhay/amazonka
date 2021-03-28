{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeAlarmHistory (..)
    , mkDescribeAlarmHistory
    -- ** Request lenses
    , dahAlarmName
    , dahAlarmTypes
    , dahEndDate
    , dahHistoryItemType
    , dahMaxRecords
    , dahNextToken
    , dahScanBy
    , dahStartDate

    -- * Destructuring the response
    , DescribeAlarmHistoryResponse (..)
    , mkDescribeAlarmHistoryResponse
    -- ** Response lenses
    , dahrrsAlarmHistoryItems
    , dahrrsNextToken
    , dahrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAlarmHistory' smart constructor.
data DescribeAlarmHistory = DescribeAlarmHistory'
  { alarmName :: Core.Maybe Types.AlarmName
    -- ^ The name of the alarm.
  , alarmTypes :: Core.Maybe [Types.AlarmType]
    -- ^ Use this parameter to specify whether you want the operation to return metric alarms or composite alarms. If you omit this parameter, only metric alarms are returned.
  , endDate :: Core.Maybe Core.UTCTime
    -- ^ The ending date to retrieve alarm history.
  , historyItemType :: Core.Maybe Types.HistoryItemType
    -- ^ The type of alarm histories to retrieve.
  , maxRecords :: Core.Maybe Core.Natural
    -- ^ The maximum number of alarm history records to retrieve.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token returned by a previous call to indicate that there is more data available.
  , scanBy :: Core.Maybe Types.ScanBy
    -- ^ Specified whether to return the newest or oldest alarm history first. Specify @TimestampDescending@ to have the newest event history returned first, and specify @TimestampAscending@ to have the oldest history returned first.
  , startDate :: Core.Maybe Core.UTCTime
    -- ^ The starting date to retrieve alarm history.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeAlarmHistory' value with any optional fields omitted.
mkDescribeAlarmHistory
    :: DescribeAlarmHistory
mkDescribeAlarmHistory
  = DescribeAlarmHistory'{alarmName = Core.Nothing,
                          alarmTypes = Core.Nothing, endDate = Core.Nothing,
                          historyItemType = Core.Nothing, maxRecords = Core.Nothing,
                          nextToken = Core.Nothing, scanBy = Core.Nothing,
                          startDate = Core.Nothing}

-- | The name of the alarm.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahAlarmName :: Lens.Lens' DescribeAlarmHistory (Core.Maybe Types.AlarmName)
dahAlarmName = Lens.field @"alarmName"
{-# INLINEABLE dahAlarmName #-}
{-# DEPRECATED alarmName "Use generic-lens or generic-optics with 'alarmName' instead"  #-}

-- | Use this parameter to specify whether you want the operation to return metric alarms or composite alarms. If you omit this parameter, only metric alarms are returned.
--
-- /Note:/ Consider using 'alarmTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahAlarmTypes :: Lens.Lens' DescribeAlarmHistory (Core.Maybe [Types.AlarmType])
dahAlarmTypes = Lens.field @"alarmTypes"
{-# INLINEABLE dahAlarmTypes #-}
{-# DEPRECATED alarmTypes "Use generic-lens or generic-optics with 'alarmTypes' instead"  #-}

-- | The ending date to retrieve alarm history.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahEndDate :: Lens.Lens' DescribeAlarmHistory (Core.Maybe Core.UTCTime)
dahEndDate = Lens.field @"endDate"
{-# INLINEABLE dahEndDate #-}
{-# DEPRECATED endDate "Use generic-lens or generic-optics with 'endDate' instead"  #-}

-- | The type of alarm histories to retrieve.
--
-- /Note:/ Consider using 'historyItemType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahHistoryItemType :: Lens.Lens' DescribeAlarmHistory (Core.Maybe Types.HistoryItemType)
dahHistoryItemType = Lens.field @"historyItemType"
{-# INLINEABLE dahHistoryItemType #-}
{-# DEPRECATED historyItemType "Use generic-lens or generic-optics with 'historyItemType' instead"  #-}

-- | The maximum number of alarm history records to retrieve.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahMaxRecords :: Lens.Lens' DescribeAlarmHistory (Core.Maybe Core.Natural)
dahMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dahMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The token returned by a previous call to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahNextToken :: Lens.Lens' DescribeAlarmHistory (Core.Maybe Types.NextToken)
dahNextToken = Lens.field @"nextToken"
{-# INLINEABLE dahNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Specified whether to return the newest or oldest alarm history first. Specify @TimestampDescending@ to have the newest event history returned first, and specify @TimestampAscending@ to have the oldest history returned first.
--
-- /Note:/ Consider using 'scanBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahScanBy :: Lens.Lens' DescribeAlarmHistory (Core.Maybe Types.ScanBy)
dahScanBy = Lens.field @"scanBy"
{-# INLINEABLE dahScanBy #-}
{-# DEPRECATED scanBy "Use generic-lens or generic-optics with 'scanBy' instead"  #-}

-- | The starting date to retrieve alarm history.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahStartDate :: Lens.Lens' DescribeAlarmHistory (Core.Maybe Core.UTCTime)
dahStartDate = Lens.field @"startDate"
{-# INLINEABLE dahStartDate #-}
{-# DEPRECATED startDate "Use generic-lens or generic-optics with 'startDate' instead"  #-}

instance Core.ToQuery DescribeAlarmHistory where
        toQuery DescribeAlarmHistory{..}
          = Core.toQueryPair "Action" ("DescribeAlarmHistory" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-08-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AlarmName") alarmName
              Core.<>
              Core.toQueryPair "AlarmTypes"
                (Core.maybe Core.mempty (Core.toQueryList "member") alarmTypes)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "EndDate") endDate
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HistoryItemType")
                historyItemType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "ScanBy") scanBy
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StartDate") startDate

instance Core.ToHeaders DescribeAlarmHistory where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeAlarmHistory where
        type Rs DescribeAlarmHistory = DescribeAlarmHistoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeAlarmHistoryResult"
              (\ s h x ->
                 DescribeAlarmHistoryResponse' Core.<$>
                   (x Core..@? "AlarmHistoryItems" Core..<@>
                      Core.parseXMLList "member")
                     Core.<*> x Core..@? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeAlarmHistory where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"alarmHistoryItems" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeAlarmHistoryResponse' smart constructor.
data DescribeAlarmHistoryResponse = DescribeAlarmHistoryResponse'
  { alarmHistoryItems :: Core.Maybe [Types.AlarmHistoryItem]
    -- ^ The alarm histories, in JSON format.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token that marks the start of the next batch of returned results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeAlarmHistoryResponse' value with any optional fields omitted.
mkDescribeAlarmHistoryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAlarmHistoryResponse
mkDescribeAlarmHistoryResponse responseStatus
  = DescribeAlarmHistoryResponse'{alarmHistoryItems = Core.Nothing,
                                  nextToken = Core.Nothing, responseStatus}

-- | The alarm histories, in JSON format.
--
-- /Note:/ Consider using 'alarmHistoryItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahrrsAlarmHistoryItems :: Lens.Lens' DescribeAlarmHistoryResponse (Core.Maybe [Types.AlarmHistoryItem])
dahrrsAlarmHistoryItems = Lens.field @"alarmHistoryItems"
{-# INLINEABLE dahrrsAlarmHistoryItems #-}
{-# DEPRECATED alarmHistoryItems "Use generic-lens or generic-optics with 'alarmHistoryItems' instead"  #-}

-- | The token that marks the start of the next batch of returned results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahrrsNextToken :: Lens.Lens' DescribeAlarmHistoryResponse (Core.Maybe Types.NextToken)
dahrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dahrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dahrrsResponseStatus :: Lens.Lens' DescribeAlarmHistoryResponse Core.Int
dahrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dahrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

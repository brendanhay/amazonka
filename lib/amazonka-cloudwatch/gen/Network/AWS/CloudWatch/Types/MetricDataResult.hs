{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.MetricDataResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.MetricDataResult
  ( MetricDataResult (..),

    -- * Smart constructor
    mkMetricDataResult,

    -- * Lenses
    mdrId,
    mdrLabel,
    mdrMessages,
    mdrStatusCode,
    mdrTimestamps,
    mdrValues,
  )
where

import qualified Network.AWS.CloudWatch.Types.MessageData as Types
import qualified Network.AWS.CloudWatch.Types.MetricId as Types
import qualified Network.AWS.CloudWatch.Types.MetricLabel as Types
import qualified Network.AWS.CloudWatch.Types.StatusCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A @GetMetricData@ call returns an array of @MetricDataResult@ structures. Each of these structures includes the data points for that metric, along with the timestamps of those data points and other identifying information.
--
-- /See:/ 'mkMetricDataResult' smart constructor.
data MetricDataResult = MetricDataResult'
  { -- | The short name you specified to represent this metric.
    id :: Core.Maybe Types.MetricId,
    -- | The human-readable label associated with the data.
    label :: Core.Maybe Types.MetricLabel,
    -- | A list of messages with additional information about the data returned.
    messages :: Core.Maybe [Types.MessageData],
    -- | The status of the returned data. @Complete@ indicates that all data points in the requested time range were returned. @PartialData@ means that an incomplete set of data points were returned. You can use the @NextToken@ value that was returned and repeat your request to get more data points. @NextToken@ is not returned if you are performing a math expression. @InternalError@ indicates that an error occurred. Retry your request using @NextToken@ , if present.
    statusCode :: Core.Maybe Types.StatusCode,
    -- | The timestamps for the data points, formatted in Unix timestamp format. The number of timestamps always matches the number of values and the value for Timestamps[x] is Values[x].
    timestamps :: Core.Maybe [Core.UTCTime],
    -- | The data points for the metric corresponding to @Timestamps@ . The number of values always matches the number of timestamps and the timestamp for Values[x] is Timestamps[x].
    values :: Core.Maybe [Core.Double]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'MetricDataResult' value with any optional fields omitted.
mkMetricDataResult ::
  MetricDataResult
mkMetricDataResult =
  MetricDataResult'
    { id = Core.Nothing,
      label = Core.Nothing,
      messages = Core.Nothing,
      statusCode = Core.Nothing,
      timestamps = Core.Nothing,
      values = Core.Nothing
    }

-- | The short name you specified to represent this metric.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdrId :: Lens.Lens' MetricDataResult (Core.Maybe Types.MetricId)
mdrId = Lens.field @"id"
{-# DEPRECATED mdrId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The human-readable label associated with the data.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdrLabel :: Lens.Lens' MetricDataResult (Core.Maybe Types.MetricLabel)
mdrLabel = Lens.field @"label"
{-# DEPRECATED mdrLabel "Use generic-lens or generic-optics with 'label' instead." #-}

-- | A list of messages with additional information about the data returned.
--
-- /Note:/ Consider using 'messages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdrMessages :: Lens.Lens' MetricDataResult (Core.Maybe [Types.MessageData])
mdrMessages = Lens.field @"messages"
{-# DEPRECATED mdrMessages "Use generic-lens or generic-optics with 'messages' instead." #-}

-- | The status of the returned data. @Complete@ indicates that all data points in the requested time range were returned. @PartialData@ means that an incomplete set of data points were returned. You can use the @NextToken@ value that was returned and repeat your request to get more data points. @NextToken@ is not returned if you are performing a math expression. @InternalError@ indicates that an error occurred. Retry your request using @NextToken@ , if present.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdrStatusCode :: Lens.Lens' MetricDataResult (Core.Maybe Types.StatusCode)
mdrStatusCode = Lens.field @"statusCode"
{-# DEPRECATED mdrStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

-- | The timestamps for the data points, formatted in Unix timestamp format. The number of timestamps always matches the number of values and the value for Timestamps[x] is Values[x].
--
-- /Note:/ Consider using 'timestamps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdrTimestamps :: Lens.Lens' MetricDataResult (Core.Maybe [Core.UTCTime])
mdrTimestamps = Lens.field @"timestamps"
{-# DEPRECATED mdrTimestamps "Use generic-lens or generic-optics with 'timestamps' instead." #-}

-- | The data points for the metric corresponding to @Timestamps@ . The number of values always matches the number of timestamps and the timestamp for Values[x] is Timestamps[x].
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdrValues :: Lens.Lens' MetricDataResult (Core.Maybe [Core.Double])
mdrValues = Lens.field @"values"
{-# DEPRECATED mdrValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromXML MetricDataResult where
  parseXML x =
    MetricDataResult'
      Core.<$> (x Core..@? "Id")
      Core.<*> (x Core..@? "Label")
      Core.<*> (x Core..@? "Messages" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "StatusCode")
      Core.<*> (x Core..@? "Timestamps" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "Values" Core..<@> Core.parseXMLList "member")

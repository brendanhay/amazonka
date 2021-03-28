{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.MetricDataQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatch.Types.MetricDataQuery
  ( MetricDataQuery (..)
  -- * Smart constructor
  , mkMetricDataQuery
  -- * Lenses
  , mdqId
  , mdqExpression
  , mdqLabel
  , mdqMetricStat
  , mdqPeriod
  , mdqReturnData
  ) where

import qualified Network.AWS.CloudWatch.Types.Expression as Types
import qualified Network.AWS.CloudWatch.Types.MetricId as Types
import qualified Network.AWS.CloudWatch.Types.MetricLabel as Types
import qualified Network.AWS.CloudWatch.Types.MetricStat as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This structure is used in both @GetMetricData@ and @PutMetricAlarm@ . The supported use of this structure is different for those two operations.
--
-- When used in @GetMetricData@ , it indicates the metric data to return, and whether this call is just retrieving a batch set of data for one metric, or is performing a math expression on metric data. A single @GetMetricData@ call can include up to 500 @MetricDataQuery@ structures.
-- When used in @PutMetricAlarm@ , it enables you to create an alarm based on a metric math expression. Each @MetricDataQuery@ in the array specifies either a metric to retrieve, or a math expression to be performed on retrieved metrics. A single @PutMetricAlarm@ call can include up to 20 @MetricDataQuery@ structures in the array. The 20 structures can include as many as 10 structures that contain a @MetricStat@ parameter to retrieve a metric, and as many as 10 structures that contain the @Expression@ parameter to perform a math expression. Of those @Expression@ structures, one must have @True@ as the value for @ReturnData@ . The result of this expression is the value the alarm watches.
-- Any expression used in a @PutMetricAlarm@ operation must return a single time series. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/using-metric-math.html#metric-math-syntax Metric Math Syntax and Functions> in the /Amazon CloudWatch User Guide/ .
-- Some of the parameters of this structure also have different uses whether you are using this structure in a @GetMetricData@ operation or a @PutMetricAlarm@ operation. These differences are explained in the following parameter list.
--
-- /See:/ 'mkMetricDataQuery' smart constructor.
data MetricDataQuery = MetricDataQuery'
  { id :: Types.MetricId
    -- ^ A short name used to tie this object to the results in the response. This name must be unique within a single call to @GetMetricData@ . If you are performing math expressions on this set of data, this name represents that data and can serve as a variable in the mathematical expression. The valid characters are letters, numbers, and underscore. The first character must be a lowercase letter.
  , expression :: Core.Maybe Types.Expression
    -- ^ The math expression to be performed on the returned data, if this object is performing a math expression. This expression can use the @Id@ of the other metrics to refer to those metrics, and can also use the @Id@ of other expressions to use the result of those expressions. For more information about metric math expressions, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/using-metric-math.html#metric-math-syntax Metric Math Syntax and Functions> in the /Amazon CloudWatch User Guide/ .
--
-- Within each MetricDataQuery object, you must specify either @Expression@ or @MetricStat@ but not both.
  , label :: Core.Maybe Types.MetricLabel
    -- ^ A human-readable label for this metric or expression. This is especially useful if this is an expression, so that you know what the value represents. If the metric or expression is shown in a CloudWatch dashboard widget, the label is shown. If Label is omitted, CloudWatch generates a default.
  , metricStat :: Core.Maybe Types.MetricStat
    -- ^ The metric to be returned, along with statistics, period, and units. Use this parameter only if this object is retrieving a metric and not performing a math expression on returned data.
--
-- Within one MetricDataQuery object, you must specify either @Expression@ or @MetricStat@ but not both.
  , period :: Core.Maybe Core.Natural
    -- ^ The granularity, in seconds, of the returned data points. For metrics with regular resolution, a period can be as short as one minute (60 seconds) and must be a multiple of 60. For high-resolution metrics that are collected at intervals of less than one minute, the period can be 1, 5, 10, 30, 60, or any multiple of 60. High-resolution metrics are those metrics stored by a @PutMetricData@ operation that includes a @StorageResolution of 1 second@ .
  , returnData :: Core.Maybe Core.Bool
    -- ^ When used in @GetMetricData@ , this option indicates whether to return the timestamps and raw data values of this metric. If you are performing this call just to do math expressions and do not also need the raw data returned, you can specify @False@ . If you omit this, the default of @True@ is used.
--
-- When used in @PutMetricAlarm@ , specify @True@ for the one expression result to use as the alarm. For all other metrics and expressions in the same @PutMetricAlarm@ operation, specify @ReturnData@ as False.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetricDataQuery' value with any optional fields omitted.
mkMetricDataQuery
    :: Types.MetricId -- ^ 'id'
    -> MetricDataQuery
mkMetricDataQuery id
  = MetricDataQuery'{id, expression = Core.Nothing,
                     label = Core.Nothing, metricStat = Core.Nothing,
                     period = Core.Nothing, returnData = Core.Nothing}

-- | A short name used to tie this object to the results in the response. This name must be unique within a single call to @GetMetricData@ . If you are performing math expressions on this set of data, this name represents that data and can serve as a variable in the mathematical expression. The valid characters are letters, numbers, and underscore. The first character must be a lowercase letter.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdqId :: Lens.Lens' MetricDataQuery Types.MetricId
mdqId = Lens.field @"id"
{-# INLINEABLE mdqId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The math expression to be performed on the returned data, if this object is performing a math expression. This expression can use the @Id@ of the other metrics to refer to those metrics, and can also use the @Id@ of other expressions to use the result of those expressions. For more information about metric math expressions, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/using-metric-math.html#metric-math-syntax Metric Math Syntax and Functions> in the /Amazon CloudWatch User Guide/ .
--
-- Within each MetricDataQuery object, you must specify either @Expression@ or @MetricStat@ but not both.
--
-- /Note:/ Consider using 'expression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdqExpression :: Lens.Lens' MetricDataQuery (Core.Maybe Types.Expression)
mdqExpression = Lens.field @"expression"
{-# INLINEABLE mdqExpression #-}
{-# DEPRECATED expression "Use generic-lens or generic-optics with 'expression' instead"  #-}

-- | A human-readable label for this metric or expression. This is especially useful if this is an expression, so that you know what the value represents. If the metric or expression is shown in a CloudWatch dashboard widget, the label is shown. If Label is omitted, CloudWatch generates a default.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdqLabel :: Lens.Lens' MetricDataQuery (Core.Maybe Types.MetricLabel)
mdqLabel = Lens.field @"label"
{-# INLINEABLE mdqLabel #-}
{-# DEPRECATED label "Use generic-lens or generic-optics with 'label' instead"  #-}

-- | The metric to be returned, along with statistics, period, and units. Use this parameter only if this object is retrieving a metric and not performing a math expression on returned data.
--
-- Within one MetricDataQuery object, you must specify either @Expression@ or @MetricStat@ but not both.
--
-- /Note:/ Consider using 'metricStat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdqMetricStat :: Lens.Lens' MetricDataQuery (Core.Maybe Types.MetricStat)
mdqMetricStat = Lens.field @"metricStat"
{-# INLINEABLE mdqMetricStat #-}
{-# DEPRECATED metricStat "Use generic-lens or generic-optics with 'metricStat' instead"  #-}

-- | The granularity, in seconds, of the returned data points. For metrics with regular resolution, a period can be as short as one minute (60 seconds) and must be a multiple of 60. For high-resolution metrics that are collected at intervals of less than one minute, the period can be 1, 5, 10, 30, 60, or any multiple of 60. High-resolution metrics are those metrics stored by a @PutMetricData@ operation that includes a @StorageResolution of 1 second@ .
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdqPeriod :: Lens.Lens' MetricDataQuery (Core.Maybe Core.Natural)
mdqPeriod = Lens.field @"period"
{-# INLINEABLE mdqPeriod #-}
{-# DEPRECATED period "Use generic-lens or generic-optics with 'period' instead"  #-}

-- | When used in @GetMetricData@ , this option indicates whether to return the timestamps and raw data values of this metric. If you are performing this call just to do math expressions and do not also need the raw data returned, you can specify @False@ . If you omit this, the default of @True@ is used.
--
-- When used in @PutMetricAlarm@ , specify @True@ for the one expression result to use as the alarm. For all other metrics and expressions in the same @PutMetricAlarm@ operation, specify @ReturnData@ as False.
--
-- /Note:/ Consider using 'returnData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdqReturnData :: Lens.Lens' MetricDataQuery (Core.Maybe Core.Bool)
mdqReturnData = Lens.field @"returnData"
{-# INLINEABLE mdqReturnData #-}
{-# DEPRECATED returnData "Use generic-lens or generic-optics with 'returnData' instead"  #-}

instance Core.ToQuery MetricDataQuery where
        toQuery MetricDataQuery{..}
          = Core.toQueryPair "Id" id Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Expression") expression
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Label") label
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MetricStat") metricStat
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Period") period
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ReturnData") returnData

instance Core.FromXML MetricDataQuery where
        parseXML x
          = MetricDataQuery' Core.<$>
              (x Core..@ "Id") Core.<*> x Core..@? "Expression" Core.<*>
                x Core..@? "Label"
                Core.<*> x Core..@? "MetricStat"
                Core.<*> x Core..@? "Period"
                Core.<*> x Core..@? "ReturnData"

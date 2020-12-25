{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.PutMetricData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Publishes metric data points to Amazon CloudWatch. CloudWatch associates the data points with the specified metric. If the specified metric does not exist, CloudWatch creates the metric. When CloudWatch creates a metric, it can take up to fifteen minutes for the metric to appear in calls to <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_ListMetrics.html ListMetrics> .
--
-- You can publish either individual data points in the @Value@ field, or arrays of values and the number of times each value occurred during the period by using the @Values@ and @Counts@ fields in the @MetricDatum@ structure. Using the @Values@ and @Counts@ method enables you to publish up to 150 values per metric with one @PutMetricData@ request, and supports retrieving percentile statistics on this data.
-- Each @PutMetricData@ request is limited to 40 KB in size for HTTP POST requests. You can send a payload compressed by gzip. Each request is also limited to no more than 20 different metrics.
-- Although the @Value@ parameter accepts numbers of type @Double@ , CloudWatch rejects values that are either too small or too large. Values must be in the range of -2^360 to 2^360. In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.
-- You can use up to 10 dimensions per metric to further clarify what data the metric collects. Each dimension consists of a Name and Value pair. For more information about specifying dimensions, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html Publishing Metrics> in the /Amazon CloudWatch User Guide/ .
-- You specify the time stamp to be associated with each data point. You can specify time stamps that are as much as two weeks before the current date, and as much as 2 hours after the current day and time.
-- Data points with time stamps from 24 hours ago or longer can take at least 48 hours to become available for <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricData.html GetMetricData> or <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricStatistics.html GetMetricStatistics> from the time they are submitted. Data points with time stamps between 3 and 24 hours ago can take as much as 2 hours to become available for for <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricData.html GetMetricData> or <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_GetMetricStatistics.html GetMetricStatistics> .
-- CloudWatch needs raw data points to calculate percentile statistics. If you publish data using a statistic set instead, you can only retrieve percentile statistics for this data if one of the following conditions is true:
--
--     * The @SampleCount@ value of the statistic set is 1 and @Min@ , @Max@ , and @Sum@ are all equal.
--
--
--     * The @Min@ and @Max@ are equal, and @Sum@ is equal to @Min@ multiplied by @SampleCount@ .
module Network.AWS.CloudWatch.PutMetricData
  ( -- * Creating a request
    PutMetricData (..),
    mkPutMetricData,

    -- ** Request lenses
    pmdNamespace,
    pmdMetricData,

    -- * Destructuring the response
    PutMetricDataResponse (..),
    mkPutMetricDataResponse,
  )
where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutMetricData' smart constructor.
data PutMetricData = PutMetricData'
  { -- | The namespace for the metric data.
    --
    -- To avoid conflicts with AWS service namespaces, you should not specify a namespace that begins with @AWS/@
    namespace :: Types.Namespace,
    -- | The data for the metric. The array can include no more than 20 metrics per call.
    metricData :: [Types.MetricDatum]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PutMetricData' value with any optional fields omitted.
mkPutMetricData ::
  -- | 'namespace'
  Types.Namespace ->
  PutMetricData
mkPutMetricData namespace =
  PutMetricData' {namespace, metricData = Core.mempty}

-- | The namespace for the metric data.
--
-- To avoid conflicts with AWS service namespaces, you should not specify a namespace that begins with @AWS/@
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmdNamespace :: Lens.Lens' PutMetricData Types.Namespace
pmdNamespace = Lens.field @"namespace"
{-# DEPRECATED pmdNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The data for the metric. The array can include no more than 20 metrics per call.
--
-- /Note:/ Consider using 'metricData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmdMetricData :: Lens.Lens' PutMetricData [Types.MetricDatum]
pmdMetricData = Lens.field @"metricData"
{-# DEPRECATED pmdMetricData "Use generic-lens or generic-optics with 'metricData' instead." #-}

instance Core.AWSRequest PutMetricData where
  type Rs PutMetricData = PutMetricDataResponse
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
            ( Core.pure ("Action", "PutMetricData")
                Core.<> (Core.pure ("Version", "2010-08-01"))
                Core.<> (Core.toQueryValue "Namespace" namespace)
                Core.<> ( Core.toQueryValue
                            "MetricData"
                            (Core.toQueryList "member" metricData)
                        )
            )
      }
  response = Response.receiveNull PutMetricDataResponse'

-- | /See:/ 'mkPutMetricDataResponse' smart constructor.
data PutMetricDataResponse = PutMetricDataResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutMetricDataResponse' value with any optional fields omitted.
mkPutMetricDataResponse ::
  PutMetricDataResponse
mkPutMetricDataResponse = PutMetricDataResponse'

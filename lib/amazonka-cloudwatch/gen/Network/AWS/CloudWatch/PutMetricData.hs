{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutMetricData' smart constructor.
data PutMetricData = PutMetricData'
  { namespace :: Lude.Text,
    metricData :: [MetricDatum]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutMetricData' with the minimum fields required to make a request.
--
-- * 'metricData' - The data for the metric. The array can include no more than 20 metrics per call.
-- * 'namespace' - The namespace for the metric data.
--
-- To avoid conflicts with AWS service namespaces, you should not specify a namespace that begins with @AWS/@
mkPutMetricData ::
  -- | 'namespace'
  Lude.Text ->
  PutMetricData
mkPutMetricData pNamespace_ =
  PutMetricData' {namespace = pNamespace_, metricData = Lude.mempty}

-- | The namespace for the metric data.
--
-- To avoid conflicts with AWS service namespaces, you should not specify a namespace that begins with @AWS/@
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmdNamespace :: Lens.Lens' PutMetricData Lude.Text
pmdNamespace = Lens.lens (namespace :: PutMetricData -> Lude.Text) (\s a -> s {namespace = a} :: PutMetricData)
{-# DEPRECATED pmdNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The data for the metric. The array can include no more than 20 metrics per call.
--
-- /Note:/ Consider using 'metricData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmdMetricData :: Lens.Lens' PutMetricData [MetricDatum]
pmdMetricData = Lens.lens (metricData :: PutMetricData -> [MetricDatum]) (\s a -> s {metricData = a} :: PutMetricData)
{-# DEPRECATED pmdMetricData "Use generic-lens or generic-optics with 'metricData' instead." #-}

instance Lude.AWSRequest PutMetricData where
  type Rs PutMetricData = PutMetricDataResponse
  request = Req.postQuery cloudWatchService
  response = Res.receiveNull PutMetricDataResponse'

instance Lude.ToHeaders PutMetricData where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PutMetricData where
  toPath = Lude.const "/"

instance Lude.ToQuery PutMetricData where
  toQuery PutMetricData' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("PutMetricData" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "Namespace" Lude.=: namespace,
        "MetricData" Lude.=: Lude.toQueryList "member" metricData
      ]

-- | /See:/ 'mkPutMetricDataResponse' smart constructor.
data PutMetricDataResponse = PutMetricDataResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutMetricDataResponse' with the minimum fields required to make a request.
mkPutMetricDataResponse ::
  PutMetricDataResponse
mkPutMetricDataResponse = PutMetricDataResponse'

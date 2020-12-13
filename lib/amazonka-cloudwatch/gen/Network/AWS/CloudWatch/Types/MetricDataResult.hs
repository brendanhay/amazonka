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
    mdrValues,
    mdrId,
    mdrTimestamps,
    mdrMessages,
    mdrLabel,
    mdrStatusCode,
  )
where

import Network.AWS.CloudWatch.Types.MessageData
import Network.AWS.CloudWatch.Types.StatusCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A @GetMetricData@ call returns an array of @MetricDataResult@ structures. Each of these structures includes the data points for that metric, along with the timestamps of those data points and other identifying information.
--
-- /See:/ 'mkMetricDataResult' smart constructor.
data MetricDataResult = MetricDataResult'
  { -- | The data points for the metric corresponding to @Timestamps@ . The number of values always matches the number of timestamps and the timestamp for Values[x] is Timestamps[x].
    values :: Lude.Maybe [Lude.Double],
    -- | The short name you specified to represent this metric.
    id :: Lude.Maybe Lude.Text,
    -- | The timestamps for the data points, formatted in Unix timestamp format. The number of timestamps always matches the number of values and the value for Timestamps[x] is Values[x].
    timestamps :: Lude.Maybe [Lude.DateTime],
    -- | A list of messages with additional information about the data returned.
    messages :: Lude.Maybe [MessageData],
    -- | The human-readable label associated with the data.
    label :: Lude.Maybe Lude.Text,
    -- | The status of the returned data. @Complete@ indicates that all data points in the requested time range were returned. @PartialData@ means that an incomplete set of data points were returned. You can use the @NextToken@ value that was returned and repeat your request to get more data points. @NextToken@ is not returned if you are performing a math expression. @InternalError@ indicates that an error occurred. Retry your request using @NextToken@ , if present.
    statusCode :: Lude.Maybe StatusCode
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricDataResult' with the minimum fields required to make a request.
--
-- * 'values' - The data points for the metric corresponding to @Timestamps@ . The number of values always matches the number of timestamps and the timestamp for Values[x] is Timestamps[x].
-- * 'id' - The short name you specified to represent this metric.
-- * 'timestamps' - The timestamps for the data points, formatted in Unix timestamp format. The number of timestamps always matches the number of values and the value for Timestamps[x] is Values[x].
-- * 'messages' - A list of messages with additional information about the data returned.
-- * 'label' - The human-readable label associated with the data.
-- * 'statusCode' - The status of the returned data. @Complete@ indicates that all data points in the requested time range were returned. @PartialData@ means that an incomplete set of data points were returned. You can use the @NextToken@ value that was returned and repeat your request to get more data points. @NextToken@ is not returned if you are performing a math expression. @InternalError@ indicates that an error occurred. Retry your request using @NextToken@ , if present.
mkMetricDataResult ::
  MetricDataResult
mkMetricDataResult =
  MetricDataResult'
    { values = Lude.Nothing,
      id = Lude.Nothing,
      timestamps = Lude.Nothing,
      messages = Lude.Nothing,
      label = Lude.Nothing,
      statusCode = Lude.Nothing
    }

-- | The data points for the metric corresponding to @Timestamps@ . The number of values always matches the number of timestamps and the timestamp for Values[x] is Timestamps[x].
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdrValues :: Lens.Lens' MetricDataResult (Lude.Maybe [Lude.Double])
mdrValues = Lens.lens (values :: MetricDataResult -> Lude.Maybe [Lude.Double]) (\s a -> s {values = a} :: MetricDataResult)
{-# DEPRECATED mdrValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The short name you specified to represent this metric.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdrId :: Lens.Lens' MetricDataResult (Lude.Maybe Lude.Text)
mdrId = Lens.lens (id :: MetricDataResult -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: MetricDataResult)
{-# DEPRECATED mdrId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The timestamps for the data points, formatted in Unix timestamp format. The number of timestamps always matches the number of values and the value for Timestamps[x] is Values[x].
--
-- /Note:/ Consider using 'timestamps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdrTimestamps :: Lens.Lens' MetricDataResult (Lude.Maybe [Lude.DateTime])
mdrTimestamps = Lens.lens (timestamps :: MetricDataResult -> Lude.Maybe [Lude.DateTime]) (\s a -> s {timestamps = a} :: MetricDataResult)
{-# DEPRECATED mdrTimestamps "Use generic-lens or generic-optics with 'timestamps' instead." #-}

-- | A list of messages with additional information about the data returned.
--
-- /Note:/ Consider using 'messages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdrMessages :: Lens.Lens' MetricDataResult (Lude.Maybe [MessageData])
mdrMessages = Lens.lens (messages :: MetricDataResult -> Lude.Maybe [MessageData]) (\s a -> s {messages = a} :: MetricDataResult)
{-# DEPRECATED mdrMessages "Use generic-lens or generic-optics with 'messages' instead." #-}

-- | The human-readable label associated with the data.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdrLabel :: Lens.Lens' MetricDataResult (Lude.Maybe Lude.Text)
mdrLabel = Lens.lens (label :: MetricDataResult -> Lude.Maybe Lude.Text) (\s a -> s {label = a} :: MetricDataResult)
{-# DEPRECATED mdrLabel "Use generic-lens or generic-optics with 'label' instead." #-}

-- | The status of the returned data. @Complete@ indicates that all data points in the requested time range were returned. @PartialData@ means that an incomplete set of data points were returned. You can use the @NextToken@ value that was returned and repeat your request to get more data points. @NextToken@ is not returned if you are performing a math expression. @InternalError@ indicates that an error occurred. Retry your request using @NextToken@ , if present.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdrStatusCode :: Lens.Lens' MetricDataResult (Lude.Maybe StatusCode)
mdrStatusCode = Lens.lens (statusCode :: MetricDataResult -> Lude.Maybe StatusCode) (\s a -> s {statusCode = a} :: MetricDataResult)
{-# DEPRECATED mdrStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.FromXML MetricDataResult where
  parseXML x =
    MetricDataResult'
      Lude.<$> ( x Lude..@? "Values" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "Id")
      Lude.<*> ( x Lude..@? "Timestamps" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "Messages" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "Label")
      Lude.<*> (x Lude..@? "StatusCode")

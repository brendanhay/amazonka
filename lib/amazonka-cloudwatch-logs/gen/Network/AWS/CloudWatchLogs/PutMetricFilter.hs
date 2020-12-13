{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.PutMetricFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a metric filter and associates it with the specified log group. Metric filters allow you to configure rules to extract metric data from log events ingested through <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutLogEvents.html PutLogEvents> .
--
-- The maximum number of metric filters that can be associated with a log group is 100.
module Network.AWS.CloudWatchLogs.PutMetricFilter
  ( -- * Creating a request
    PutMetricFilter (..),
    mkPutMetricFilter,

    -- ** Request lenses
    pmfFilterName,
    pmfLogGroupName,
    pmfFilterPattern,
    pmfMetricTransformations,

    -- * Destructuring the response
    PutMetricFilterResponse (..),
    mkPutMetricFilterResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutMetricFilter' smart constructor.
data PutMetricFilter = PutMetricFilter'
  { -- | A name for the metric filter.
    filterName :: Lude.Text,
    -- | The name of the log group.
    logGroupName :: Lude.Text,
    -- | A filter pattern for extracting metric data out of ingested log events.
    filterPattern :: Lude.Text,
    -- | A collection of information that defines how metric data gets emitted.
    metricTransformations :: Lude.NonEmpty MetricTransformation
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutMetricFilter' with the minimum fields required to make a request.
--
-- * 'filterName' - A name for the metric filter.
-- * 'logGroupName' - The name of the log group.
-- * 'filterPattern' - A filter pattern for extracting metric data out of ingested log events.
-- * 'metricTransformations' - A collection of information that defines how metric data gets emitted.
mkPutMetricFilter ::
  -- | 'filterName'
  Lude.Text ->
  -- | 'logGroupName'
  Lude.Text ->
  -- | 'filterPattern'
  Lude.Text ->
  -- | 'metricTransformations'
  Lude.NonEmpty MetricTransformation ->
  PutMetricFilter
mkPutMetricFilter
  pFilterName_
  pLogGroupName_
  pFilterPattern_
  pMetricTransformations_ =
    PutMetricFilter'
      { filterName = pFilterName_,
        logGroupName = pLogGroupName_,
        filterPattern = pFilterPattern_,
        metricTransformations = pMetricTransformations_
      }

-- | A name for the metric filter.
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmfFilterName :: Lens.Lens' PutMetricFilter Lude.Text
pmfFilterName = Lens.lens (filterName :: PutMetricFilter -> Lude.Text) (\s a -> s {filterName = a} :: PutMetricFilter)
{-# DEPRECATED pmfFilterName "Use generic-lens or generic-optics with 'filterName' instead." #-}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmfLogGroupName :: Lens.Lens' PutMetricFilter Lude.Text
pmfLogGroupName = Lens.lens (logGroupName :: PutMetricFilter -> Lude.Text) (\s a -> s {logGroupName = a} :: PutMetricFilter)
{-# DEPRECATED pmfLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | A filter pattern for extracting metric data out of ingested log events.
--
-- /Note:/ Consider using 'filterPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmfFilterPattern :: Lens.Lens' PutMetricFilter Lude.Text
pmfFilterPattern = Lens.lens (filterPattern :: PutMetricFilter -> Lude.Text) (\s a -> s {filterPattern = a} :: PutMetricFilter)
{-# DEPRECATED pmfFilterPattern "Use generic-lens or generic-optics with 'filterPattern' instead." #-}

-- | A collection of information that defines how metric data gets emitted.
--
-- /Note:/ Consider using 'metricTransformations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmfMetricTransformations :: Lens.Lens' PutMetricFilter (Lude.NonEmpty MetricTransformation)
pmfMetricTransformations = Lens.lens (metricTransformations :: PutMetricFilter -> Lude.NonEmpty MetricTransformation) (\s a -> s {metricTransformations = a} :: PutMetricFilter)
{-# DEPRECATED pmfMetricTransformations "Use generic-lens or generic-optics with 'metricTransformations' instead." #-}

instance Lude.AWSRequest PutMetricFilter where
  type Rs PutMetricFilter = PutMetricFilterResponse
  request = Req.postJSON cloudWatchLogsService
  response = Res.receiveNull PutMetricFilterResponse'

instance Lude.ToHeaders PutMetricFilter where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.PutMetricFilter" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutMetricFilter where
  toJSON PutMetricFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("filterName" Lude..= filterName),
            Lude.Just ("logGroupName" Lude..= logGroupName),
            Lude.Just ("filterPattern" Lude..= filterPattern),
            Lude.Just ("metricTransformations" Lude..= metricTransformations)
          ]
      )

instance Lude.ToPath PutMetricFilter where
  toPath = Lude.const "/"

instance Lude.ToQuery PutMetricFilter where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutMetricFilterResponse' smart constructor.
data PutMetricFilterResponse = PutMetricFilterResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutMetricFilterResponse' with the minimum fields required to make a request.
mkPutMetricFilterResponse ::
  PutMetricFilterResponse
mkPutMetricFilterResponse = PutMetricFilterResponse'

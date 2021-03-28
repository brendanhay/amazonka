{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      PutMetricFilter (..)
    , mkPutMetricFilter
    -- ** Request lenses
    , pmfLogGroupName
    , pmfFilterName
    , pmfFilterPattern
    , pmfMetricTransformations

    -- * Destructuring the response
    , PutMetricFilterResponse (..)
    , mkPutMetricFilterResponse
    ) where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutMetricFilter' smart constructor.
data PutMetricFilter = PutMetricFilter'
  { logGroupName :: Types.LogGroupName
    -- ^ The name of the log group.
  , filterName :: Types.FilterName
    -- ^ A name for the metric filter.
  , filterPattern :: Types.FilterPattern
    -- ^ A filter pattern for extracting metric data out of ingested log events.
  , metricTransformations :: Core.NonEmpty Types.MetricTransformation
    -- ^ A collection of information that defines how metric data gets emitted.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutMetricFilter' value with any optional fields omitted.
mkPutMetricFilter
    :: Types.LogGroupName -- ^ 'logGroupName'
    -> Types.FilterName -- ^ 'filterName'
    -> Types.FilterPattern -- ^ 'filterPattern'
    -> Core.NonEmpty Types.MetricTransformation -- ^ 'metricTransformations'
    -> PutMetricFilter
mkPutMetricFilter logGroupName filterName filterPattern
  metricTransformations
  = PutMetricFilter'{logGroupName, filterName, filterPattern,
                     metricTransformations}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmfLogGroupName :: Lens.Lens' PutMetricFilter Types.LogGroupName
pmfLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE pmfLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

-- | A name for the metric filter.
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmfFilterName :: Lens.Lens' PutMetricFilter Types.FilterName
pmfFilterName = Lens.field @"filterName"
{-# INLINEABLE pmfFilterName #-}
{-# DEPRECATED filterName "Use generic-lens or generic-optics with 'filterName' instead"  #-}

-- | A filter pattern for extracting metric data out of ingested log events.
--
-- /Note:/ Consider using 'filterPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmfFilterPattern :: Lens.Lens' PutMetricFilter Types.FilterPattern
pmfFilterPattern = Lens.field @"filterPattern"
{-# INLINEABLE pmfFilterPattern #-}
{-# DEPRECATED filterPattern "Use generic-lens or generic-optics with 'filterPattern' instead"  #-}

-- | A collection of information that defines how metric data gets emitted.
--
-- /Note:/ Consider using 'metricTransformations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmfMetricTransformations :: Lens.Lens' PutMetricFilter (Core.NonEmpty Types.MetricTransformation)
pmfMetricTransformations = Lens.field @"metricTransformations"
{-# INLINEABLE pmfMetricTransformations #-}
{-# DEPRECATED metricTransformations "Use generic-lens or generic-optics with 'metricTransformations' instead"  #-}

instance Core.ToQuery PutMetricFilter where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutMetricFilter where
        toHeaders PutMetricFilter{..}
          = Core.pure ("X-Amz-Target", "Logs_20140328.PutMetricFilter")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutMetricFilter where
        toJSON PutMetricFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("logGroupName" Core..= logGroupName),
                  Core.Just ("filterName" Core..= filterName),
                  Core.Just ("filterPattern" Core..= filterPattern),
                  Core.Just ("metricTransformations" Core..= metricTransformations)])

instance Core.AWSRequest PutMetricFilter where
        type Rs PutMetricFilter = PutMetricFilterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull PutMetricFilterResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutMetricFilterResponse' smart constructor.
data PutMetricFilterResponse = PutMetricFilterResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutMetricFilterResponse' value with any optional fields omitted.
mkPutMetricFilterResponse
    :: PutMetricFilterResponse
mkPutMetricFilterResponse = PutMetricFilterResponse'

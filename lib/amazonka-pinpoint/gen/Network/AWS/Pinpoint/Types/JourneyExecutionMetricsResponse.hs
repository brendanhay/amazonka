{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyExecutionMetricsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.JourneyExecutionMetricsResponse
  ( JourneyExecutionMetricsResponse (..)
  -- * Smart constructor
  , mkJourneyExecutionMetricsResponse
  -- * Lenses
  , jemrMetrics
  , jemrJourneyId
  , jemrLastEvaluatedTime
  , jemrApplicationId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides the results of a query that retrieved the data for a standard execution metric that applies to a journey, and provides information about that query.
--
-- /See:/ 'mkJourneyExecutionMetricsResponse' smart constructor.
data JourneyExecutionMetricsResponse = JourneyExecutionMetricsResponse'
  { metrics :: Core.HashMap Core.Text Core.Text
    -- ^ A JSON object that contains the results of the query. For information about the structure and contents of the results, see the <https://docs.aws.amazon.com//pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
  , journeyId :: Core.Text
    -- ^ The unique identifier for the journey that the metric applies to.
  , lastEvaluatedTime :: Core.Text
    -- ^ The date and time, in ISO 8601 format, when Amazon Pinpoint last evaluated the journey and updated the data for the metric.
  , applicationId :: Core.Text
    -- ^ The unique identifier for the application that the metric applies to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JourneyExecutionMetricsResponse' value with any optional fields omitted.
mkJourneyExecutionMetricsResponse
    :: Core.Text -- ^ 'journeyId'
    -> Core.Text -- ^ 'lastEvaluatedTime'
    -> Core.Text -- ^ 'applicationId'
    -> JourneyExecutionMetricsResponse
mkJourneyExecutionMetricsResponse journeyId lastEvaluatedTime
  applicationId
  = JourneyExecutionMetricsResponse'{metrics = Core.mempty,
                                     journeyId, lastEvaluatedTime, applicationId}

-- | A JSON object that contains the results of the query. For information about the structure and contents of the results, see the <https://docs.aws.amazon.com//pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jemrMetrics :: Lens.Lens' JourneyExecutionMetricsResponse (Core.HashMap Core.Text Core.Text)
jemrMetrics = Lens.field @"metrics"
{-# INLINEABLE jemrMetrics #-}
{-# DEPRECATED metrics "Use generic-lens or generic-optics with 'metrics' instead"  #-}

-- | The unique identifier for the journey that the metric applies to.
--
-- /Note:/ Consider using 'journeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jemrJourneyId :: Lens.Lens' JourneyExecutionMetricsResponse Core.Text
jemrJourneyId = Lens.field @"journeyId"
{-# INLINEABLE jemrJourneyId #-}
{-# DEPRECATED journeyId "Use generic-lens or generic-optics with 'journeyId' instead"  #-}

-- | The date and time, in ISO 8601 format, when Amazon Pinpoint last evaluated the journey and updated the data for the metric.
--
-- /Note:/ Consider using 'lastEvaluatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jemrLastEvaluatedTime :: Lens.Lens' JourneyExecutionMetricsResponse Core.Text
jemrLastEvaluatedTime = Lens.field @"lastEvaluatedTime"
{-# INLINEABLE jemrLastEvaluatedTime #-}
{-# DEPRECATED lastEvaluatedTime "Use generic-lens or generic-optics with 'lastEvaluatedTime' instead"  #-}

-- | The unique identifier for the application that the metric applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jemrApplicationId :: Lens.Lens' JourneyExecutionMetricsResponse Core.Text
jemrApplicationId = Lens.field @"applicationId"
{-# INLINEABLE jemrApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.FromJSON JourneyExecutionMetricsResponse where
        parseJSON
          = Core.withObject "JourneyExecutionMetricsResponse" Core.$
              \ x ->
                JourneyExecutionMetricsResponse' Core.<$>
                  (x Core..:? "Metrics" Core..!= Core.mempty) Core.<*>
                    x Core..: "JourneyId"
                    Core.<*> x Core..: "LastEvaluatedTime"
                    Core.<*> x Core..: "ApplicationId"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyExecutionActivityMetricsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.JourneyExecutionActivityMetricsResponse
  ( JourneyExecutionActivityMetricsResponse (..)
  -- * Smart constructor
  , mkJourneyExecutionActivityMetricsResponse
  -- * Lenses
  , jeamrMetrics
  , jeamrJourneyId
  , jeamrLastEvaluatedTime
  , jeamrJourneyActivityId
  , jeamrActivityType
  , jeamrApplicationId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides the results of a query that retrieved the data for a standard execution metric that applies to a journey activity, and provides information about that query.
--
-- /See:/ 'mkJourneyExecutionActivityMetricsResponse' smart constructor.
data JourneyExecutionActivityMetricsResponse = JourneyExecutionActivityMetricsResponse'
  { metrics :: Core.HashMap Core.Text Core.Text
    -- ^ A JSON object that contains the results of the query. The results vary depending on the type of activity (ActivityType). For information about the structure and contents of the results, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
  , journeyId :: Core.Text
    -- ^ The unique identifier for the journey that the metric applies to.
  , lastEvaluatedTime :: Core.Text
    -- ^ The date and time, in ISO 8601 format, when Amazon Pinpoint last evaluated the execution status of the activity and updated the data for the metric.
  , journeyActivityId :: Core.Text
    -- ^ The unique identifier for the activity that the metric applies to.
  , activityType :: Core.Text
    -- ^ The type of activity that the metric applies to. Possible values are:
--
--
--     * CONDITIONAL_SPLIT - For a yes/no split activity, which is an activity that sends participants down one of two paths in a journey.
--
--
--     * HOLDOUT - For a holdout activity, which is an activity that stops a journey for a specified percentage of participants.
--
--
--     * MESSAGE - For an email activity, which is an activity that sends an email message to participants.
--
--
--     * MULTI_CONDITIONAL_SPLIT - For a multivariate split activity, which is an activity that sends participants down one of as many as five paths in a journey.
--
--
--     * RANDOM_SPLIT - For a random split activity, which is an activity that sends specified percentages of participants down one of as many as five paths in a journey.
--
--
--     * WAIT - For a wait activity, which is an activity that waits for a certain amount of time or until a specific date and time before moving participants to the next activity in a journey.
--
--
  , applicationId :: Core.Text
    -- ^ The unique identifier for the application that the metric applies to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JourneyExecutionActivityMetricsResponse' value with any optional fields omitted.
mkJourneyExecutionActivityMetricsResponse
    :: Core.Text -- ^ 'journeyId'
    -> Core.Text -- ^ 'lastEvaluatedTime'
    -> Core.Text -- ^ 'journeyActivityId'
    -> Core.Text -- ^ 'activityType'
    -> Core.Text -- ^ 'applicationId'
    -> JourneyExecutionActivityMetricsResponse
mkJourneyExecutionActivityMetricsResponse journeyId
  lastEvaluatedTime journeyActivityId activityType applicationId
  = JourneyExecutionActivityMetricsResponse'{metrics = Core.mempty,
                                             journeyId, lastEvaluatedTime, journeyActivityId,
                                             activityType, applicationId}

-- | A JSON object that contains the results of the query. The results vary depending on the type of activity (ActivityType). For information about the structure and contents of the results, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeamrMetrics :: Lens.Lens' JourneyExecutionActivityMetricsResponse (Core.HashMap Core.Text Core.Text)
jeamrMetrics = Lens.field @"metrics"
{-# INLINEABLE jeamrMetrics #-}
{-# DEPRECATED metrics "Use generic-lens or generic-optics with 'metrics' instead"  #-}

-- | The unique identifier for the journey that the metric applies to.
--
-- /Note:/ Consider using 'journeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeamrJourneyId :: Lens.Lens' JourneyExecutionActivityMetricsResponse Core.Text
jeamrJourneyId = Lens.field @"journeyId"
{-# INLINEABLE jeamrJourneyId #-}
{-# DEPRECATED journeyId "Use generic-lens or generic-optics with 'journeyId' instead"  #-}

-- | The date and time, in ISO 8601 format, when Amazon Pinpoint last evaluated the execution status of the activity and updated the data for the metric.
--
-- /Note:/ Consider using 'lastEvaluatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeamrLastEvaluatedTime :: Lens.Lens' JourneyExecutionActivityMetricsResponse Core.Text
jeamrLastEvaluatedTime = Lens.field @"lastEvaluatedTime"
{-# INLINEABLE jeamrLastEvaluatedTime #-}
{-# DEPRECATED lastEvaluatedTime "Use generic-lens or generic-optics with 'lastEvaluatedTime' instead"  #-}

-- | The unique identifier for the activity that the metric applies to.
--
-- /Note:/ Consider using 'journeyActivityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeamrJourneyActivityId :: Lens.Lens' JourneyExecutionActivityMetricsResponse Core.Text
jeamrJourneyActivityId = Lens.field @"journeyActivityId"
{-# INLINEABLE jeamrJourneyActivityId #-}
{-# DEPRECATED journeyActivityId "Use generic-lens or generic-optics with 'journeyActivityId' instead"  #-}

-- | The type of activity that the metric applies to. Possible values are:
--
--
--     * CONDITIONAL_SPLIT - For a yes/no split activity, which is an activity that sends participants down one of two paths in a journey.
--
--
--     * HOLDOUT - For a holdout activity, which is an activity that stops a journey for a specified percentage of participants.
--
--
--     * MESSAGE - For an email activity, which is an activity that sends an email message to participants.
--
--
--     * MULTI_CONDITIONAL_SPLIT - For a multivariate split activity, which is an activity that sends participants down one of as many as five paths in a journey.
--
--
--     * RANDOM_SPLIT - For a random split activity, which is an activity that sends specified percentages of participants down one of as many as five paths in a journey.
--
--
--     * WAIT - For a wait activity, which is an activity that waits for a certain amount of time or until a specific date and time before moving participants to the next activity in a journey.
--
--
--
-- /Note:/ Consider using 'activityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeamrActivityType :: Lens.Lens' JourneyExecutionActivityMetricsResponse Core.Text
jeamrActivityType = Lens.field @"activityType"
{-# INLINEABLE jeamrActivityType #-}
{-# DEPRECATED activityType "Use generic-lens or generic-optics with 'activityType' instead"  #-}

-- | The unique identifier for the application that the metric applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeamrApplicationId :: Lens.Lens' JourneyExecutionActivityMetricsResponse Core.Text
jeamrApplicationId = Lens.field @"applicationId"
{-# INLINEABLE jeamrApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

instance Core.FromJSON JourneyExecutionActivityMetricsResponse
         where
        parseJSON
          = Core.withObject "JourneyExecutionActivityMetricsResponse" Core.$
              \ x ->
                JourneyExecutionActivityMetricsResponse' Core.<$>
                  (x Core..:? "Metrics" Core..!= Core.mempty) Core.<*>
                    x Core..: "JourneyId"
                    Core.<*> x Core..: "LastEvaluatedTime"
                    Core.<*> x Core..: "JourneyActivityId"
                    Core.<*> x Core..: "ActivityType"
                    Core.<*> x Core..: "ApplicationId"

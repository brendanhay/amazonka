{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyExecutionActivityMetricsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyExecutionActivityMetricsResponse
  ( JourneyExecutionActivityMetricsResponse (..),

    -- * Smart constructor
    mkJourneyExecutionActivityMetricsResponse,

    -- * Lenses
    jeamMetrics,
    jeamActivityType,
    jeamLastEvaluatedTime,
    jeamJourneyActivityId,
    jeamApplicationId,
    jeamJourneyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the results of a query that retrieved the data for a standard execution metric that applies to a journey activity, and provides information about that query.
--
-- /See:/ 'mkJourneyExecutionActivityMetricsResponse' smart constructor.
data JourneyExecutionActivityMetricsResponse = JourneyExecutionActivityMetricsResponse'
  { -- | A JSON object that contains the results of the query. The results vary depending on the type of activity (ActivityType). For information about the structure and contents of the results, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
    metrics :: Lude.HashMap Lude.Text (Lude.Text),
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
    activityType :: Lude.Text,
    -- | The date and time, in ISO 8601 format, when Amazon Pinpoint last evaluated the execution status of the activity and updated the data for the metric.
    lastEvaluatedTime :: Lude.Text,
    -- | The unique identifier for the activity that the metric applies to.
    journeyActivityId :: Lude.Text,
    -- | The unique identifier for the application that the metric applies to.
    applicationId :: Lude.Text,
    -- | The unique identifier for the journey that the metric applies to.
    journeyId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JourneyExecutionActivityMetricsResponse' with the minimum fields required to make a request.
--
-- * 'metrics' - A JSON object that contains the results of the query. The results vary depending on the type of activity (ActivityType). For information about the structure and contents of the results, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
-- * 'activityType' - The type of activity that the metric applies to. Possible values are:
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
-- * 'lastEvaluatedTime' - The date and time, in ISO 8601 format, when Amazon Pinpoint last evaluated the execution status of the activity and updated the data for the metric.
-- * 'journeyActivityId' - The unique identifier for the activity that the metric applies to.
-- * 'applicationId' - The unique identifier for the application that the metric applies to.
-- * 'journeyId' - The unique identifier for the journey that the metric applies to.
mkJourneyExecutionActivityMetricsResponse ::
  -- | 'activityType'
  Lude.Text ->
  -- | 'lastEvaluatedTime'
  Lude.Text ->
  -- | 'journeyActivityId'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  -- | 'journeyId'
  Lude.Text ->
  JourneyExecutionActivityMetricsResponse
mkJourneyExecutionActivityMetricsResponse
  pActivityType_
  pLastEvaluatedTime_
  pJourneyActivityId_
  pApplicationId_
  pJourneyId_ =
    JourneyExecutionActivityMetricsResponse'
      { metrics = Lude.mempty,
        activityType = pActivityType_,
        lastEvaluatedTime = pLastEvaluatedTime_,
        journeyActivityId = pJourneyActivityId_,
        applicationId = pApplicationId_,
        journeyId = pJourneyId_
      }

-- | A JSON object that contains the results of the query. The results vary depending on the type of activity (ActivityType). For information about the structure and contents of the results, see the <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeamMetrics :: Lens.Lens' JourneyExecutionActivityMetricsResponse (Lude.HashMap Lude.Text (Lude.Text))
jeamMetrics = Lens.lens (metrics :: JourneyExecutionActivityMetricsResponse -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {metrics = a} :: JourneyExecutionActivityMetricsResponse)
{-# DEPRECATED jeamMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

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
jeamActivityType :: Lens.Lens' JourneyExecutionActivityMetricsResponse Lude.Text
jeamActivityType = Lens.lens (activityType :: JourneyExecutionActivityMetricsResponse -> Lude.Text) (\s a -> s {activityType = a} :: JourneyExecutionActivityMetricsResponse)
{-# DEPRECATED jeamActivityType "Use generic-lens or generic-optics with 'activityType' instead." #-}

-- | The date and time, in ISO 8601 format, when Amazon Pinpoint last evaluated the execution status of the activity and updated the data for the metric.
--
-- /Note:/ Consider using 'lastEvaluatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeamLastEvaluatedTime :: Lens.Lens' JourneyExecutionActivityMetricsResponse Lude.Text
jeamLastEvaluatedTime = Lens.lens (lastEvaluatedTime :: JourneyExecutionActivityMetricsResponse -> Lude.Text) (\s a -> s {lastEvaluatedTime = a} :: JourneyExecutionActivityMetricsResponse)
{-# DEPRECATED jeamLastEvaluatedTime "Use generic-lens or generic-optics with 'lastEvaluatedTime' instead." #-}

-- | The unique identifier for the activity that the metric applies to.
--
-- /Note:/ Consider using 'journeyActivityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeamJourneyActivityId :: Lens.Lens' JourneyExecutionActivityMetricsResponse Lude.Text
jeamJourneyActivityId = Lens.lens (journeyActivityId :: JourneyExecutionActivityMetricsResponse -> Lude.Text) (\s a -> s {journeyActivityId = a} :: JourneyExecutionActivityMetricsResponse)
{-# DEPRECATED jeamJourneyActivityId "Use generic-lens or generic-optics with 'journeyActivityId' instead." #-}

-- | The unique identifier for the application that the metric applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeamApplicationId :: Lens.Lens' JourneyExecutionActivityMetricsResponse Lude.Text
jeamApplicationId = Lens.lens (applicationId :: JourneyExecutionActivityMetricsResponse -> Lude.Text) (\s a -> s {applicationId = a} :: JourneyExecutionActivityMetricsResponse)
{-# DEPRECATED jeamApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique identifier for the journey that the metric applies to.
--
-- /Note:/ Consider using 'journeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeamJourneyId :: Lens.Lens' JourneyExecutionActivityMetricsResponse Lude.Text
jeamJourneyId = Lens.lens (journeyId :: JourneyExecutionActivityMetricsResponse -> Lude.Text) (\s a -> s {journeyId = a} :: JourneyExecutionActivityMetricsResponse)
{-# DEPRECATED jeamJourneyId "Use generic-lens or generic-optics with 'journeyId' instead." #-}

instance Lude.FromJSON JourneyExecutionActivityMetricsResponse where
  parseJSON =
    Lude.withObject
      "JourneyExecutionActivityMetricsResponse"
      ( \x ->
          JourneyExecutionActivityMetricsResponse'
            Lude.<$> (x Lude..:? "Metrics" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "ActivityType")
            Lude.<*> (x Lude..: "LastEvaluatedTime")
            Lude.<*> (x Lude..: "JourneyActivityId")
            Lude.<*> (x Lude..: "ApplicationId")
            Lude.<*> (x Lude..: "JourneyId")
      )

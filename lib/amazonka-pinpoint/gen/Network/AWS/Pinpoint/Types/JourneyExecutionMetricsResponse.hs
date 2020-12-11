-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyExecutionMetricsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyExecutionMetricsResponse
  ( JourneyExecutionMetricsResponse (..),

    -- * Smart constructor
    mkJourneyExecutionMetricsResponse,

    -- * Lenses
    jemMetrics,
    jemJourneyId,
    jemLastEvaluatedTime,
    jemApplicationId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the results of a query that retrieved the data for a standard execution metric that applies to a journey, and provides information about that query.
--
-- /See:/ 'mkJourneyExecutionMetricsResponse' smart constructor.
data JourneyExecutionMetricsResponse = JourneyExecutionMetricsResponse'
  { metrics ::
      Lude.HashMap
        Lude.Text
        (Lude.Text),
    journeyId :: Lude.Text,
    lastEvaluatedTime ::
      Lude.Text,
    applicationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JourneyExecutionMetricsResponse' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application that the metric applies to.
-- * 'journeyId' - The unique identifier for the journey that the metric applies to.
-- * 'lastEvaluatedTime' - The date and time, in ISO 8601 format, when Amazon Pinpoint last evaluated the journey and updated the data for the metric.
-- * 'metrics' - A JSON object that contains the results of the query. For information about the structure and contents of the results, see the <https://docs.aws.amazon.com//pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
mkJourneyExecutionMetricsResponse ::
  -- | 'journeyId'
  Lude.Text ->
  -- | 'lastEvaluatedTime'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  JourneyExecutionMetricsResponse
mkJourneyExecutionMetricsResponse
  pJourneyId_
  pLastEvaluatedTime_
  pApplicationId_ =
    JourneyExecutionMetricsResponse'
      { metrics = Lude.mempty,
        journeyId = pJourneyId_,
        lastEvaluatedTime = pLastEvaluatedTime_,
        applicationId = pApplicationId_
      }

-- | A JSON object that contains the results of the query. For information about the structure and contents of the results, see the <https://docs.aws.amazon.com//pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide> .
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jemMetrics :: Lens.Lens' JourneyExecutionMetricsResponse (Lude.HashMap Lude.Text (Lude.Text))
jemMetrics = Lens.lens (metrics :: JourneyExecutionMetricsResponse -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {metrics = a} :: JourneyExecutionMetricsResponse)
{-# DEPRECATED jemMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The unique identifier for the journey that the metric applies to.
--
-- /Note:/ Consider using 'journeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jemJourneyId :: Lens.Lens' JourneyExecutionMetricsResponse Lude.Text
jemJourneyId = Lens.lens (journeyId :: JourneyExecutionMetricsResponse -> Lude.Text) (\s a -> s {journeyId = a} :: JourneyExecutionMetricsResponse)
{-# DEPRECATED jemJourneyId "Use generic-lens or generic-optics with 'journeyId' instead." #-}

-- | The date and time, in ISO 8601 format, when Amazon Pinpoint last evaluated the journey and updated the data for the metric.
--
-- /Note:/ Consider using 'lastEvaluatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jemLastEvaluatedTime :: Lens.Lens' JourneyExecutionMetricsResponse Lude.Text
jemLastEvaluatedTime = Lens.lens (lastEvaluatedTime :: JourneyExecutionMetricsResponse -> Lude.Text) (\s a -> s {lastEvaluatedTime = a} :: JourneyExecutionMetricsResponse)
{-# DEPRECATED jemLastEvaluatedTime "Use generic-lens or generic-optics with 'lastEvaluatedTime' instead." #-}

-- | The unique identifier for the application that the metric applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jemApplicationId :: Lens.Lens' JourneyExecutionMetricsResponse Lude.Text
jemApplicationId = Lens.lens (applicationId :: JourneyExecutionMetricsResponse -> Lude.Text) (\s a -> s {applicationId = a} :: JourneyExecutionMetricsResponse)
{-# DEPRECATED jemApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.FromJSON JourneyExecutionMetricsResponse where
  parseJSON =
    Lude.withObject
      "JourneyExecutionMetricsResponse"
      ( \x ->
          JourneyExecutionMetricsResponse'
            Lude.<$> (x Lude..:? "Metrics" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "JourneyId")
            Lude.<*> (x Lude..: "LastEvaluatedTime")
            Lude.<*> (x Lude..: "ApplicationId")
      )

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyExecutionActivityMetricsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyExecutionActivityMetricsResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides the results of a query that retrieved the data for a standard
-- execution metric that applies to a journey activity, and provides
-- information about that query.
--
-- /See:/ 'newJourneyExecutionActivityMetricsResponse' smart constructor.
data JourneyExecutionActivityMetricsResponse = JourneyExecutionActivityMetricsResponse'
  { -- | A JSON object that contains the results of the query. The results vary
    -- depending on the type of activity (ActivityType). For information about
    -- the structure and contents of the results, see the
    -- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
    metrics :: Core.HashMap Core.Text Core.Text,
    -- | The unique identifier for the journey that the metric applies to.
    journeyId :: Core.Text,
    -- | The date and time, in ISO 8601 format, when Amazon Pinpoint last
    -- evaluated the execution status of the activity and updated the data for
    -- the metric.
    lastEvaluatedTime :: Core.Text,
    -- | The unique identifier for the activity that the metric applies to.
    journeyActivityId :: Core.Text,
    -- | The type of activity that the metric applies to. Possible values are:
    --
    -- -   CONDITIONAL_SPLIT - For a yes\/no split activity, which is an
    --     activity that sends participants down one of two paths in a journey.
    --
    -- -   HOLDOUT - For a holdout activity, which is an activity that stops a
    --     journey for a specified percentage of participants.
    --
    -- -   MESSAGE - For an email activity, which is an activity that sends an
    --     email message to participants.
    --
    -- -   MULTI_CONDITIONAL_SPLIT - For a multivariate split activity, which
    --     is an activity that sends participants down one of as many as five
    --     paths in a journey.
    --
    -- -   RANDOM_SPLIT - For a random split activity, which is an activity
    --     that sends specified percentages of participants down one of as many
    --     as five paths in a journey.
    --
    -- -   WAIT - For a wait activity, which is an activity that waits for a
    --     certain amount of time or until a specific date and time before
    --     moving participants to the next activity in a journey.
    activityType :: Core.Text,
    -- | The unique identifier for the application that the metric applies to.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'JourneyExecutionActivityMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metrics', 'journeyExecutionActivityMetricsResponse_metrics' - A JSON object that contains the results of the query. The results vary
-- depending on the type of activity (ActivityType). For information about
-- the structure and contents of the results, see the
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
--
-- 'journeyId', 'journeyExecutionActivityMetricsResponse_journeyId' - The unique identifier for the journey that the metric applies to.
--
-- 'lastEvaluatedTime', 'journeyExecutionActivityMetricsResponse_lastEvaluatedTime' - The date and time, in ISO 8601 format, when Amazon Pinpoint last
-- evaluated the execution status of the activity and updated the data for
-- the metric.
--
-- 'journeyActivityId', 'journeyExecutionActivityMetricsResponse_journeyActivityId' - The unique identifier for the activity that the metric applies to.
--
-- 'activityType', 'journeyExecutionActivityMetricsResponse_activityType' - The type of activity that the metric applies to. Possible values are:
--
-- -   CONDITIONAL_SPLIT - For a yes\/no split activity, which is an
--     activity that sends participants down one of two paths in a journey.
--
-- -   HOLDOUT - For a holdout activity, which is an activity that stops a
--     journey for a specified percentage of participants.
--
-- -   MESSAGE - For an email activity, which is an activity that sends an
--     email message to participants.
--
-- -   MULTI_CONDITIONAL_SPLIT - For a multivariate split activity, which
--     is an activity that sends participants down one of as many as five
--     paths in a journey.
--
-- -   RANDOM_SPLIT - For a random split activity, which is an activity
--     that sends specified percentages of participants down one of as many
--     as five paths in a journey.
--
-- -   WAIT - For a wait activity, which is an activity that waits for a
--     certain amount of time or until a specific date and time before
--     moving participants to the next activity in a journey.
--
-- 'applicationId', 'journeyExecutionActivityMetricsResponse_applicationId' - The unique identifier for the application that the metric applies to.
newJourneyExecutionActivityMetricsResponse ::
  -- | 'journeyId'
  Core.Text ->
  -- | 'lastEvaluatedTime'
  Core.Text ->
  -- | 'journeyActivityId'
  Core.Text ->
  -- | 'activityType'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  JourneyExecutionActivityMetricsResponse
newJourneyExecutionActivityMetricsResponse
  pJourneyId_
  pLastEvaluatedTime_
  pJourneyActivityId_
  pActivityType_
  pApplicationId_ =
    JourneyExecutionActivityMetricsResponse'
      { metrics =
          Core.mempty,
        journeyId = pJourneyId_,
        lastEvaluatedTime =
          pLastEvaluatedTime_,
        journeyActivityId =
          pJourneyActivityId_,
        activityType = pActivityType_,
        applicationId = pApplicationId_
      }

-- | A JSON object that contains the results of the query. The results vary
-- depending on the type of activity (ActivityType). For information about
-- the structure and contents of the results, see the
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
journeyExecutionActivityMetricsResponse_metrics :: Lens.Lens' JourneyExecutionActivityMetricsResponse (Core.HashMap Core.Text Core.Text)
journeyExecutionActivityMetricsResponse_metrics = Lens.lens (\JourneyExecutionActivityMetricsResponse' {metrics} -> metrics) (\s@JourneyExecutionActivityMetricsResponse' {} a -> s {metrics = a} :: JourneyExecutionActivityMetricsResponse) Core.. Lens._Coerce

-- | The unique identifier for the journey that the metric applies to.
journeyExecutionActivityMetricsResponse_journeyId :: Lens.Lens' JourneyExecutionActivityMetricsResponse Core.Text
journeyExecutionActivityMetricsResponse_journeyId = Lens.lens (\JourneyExecutionActivityMetricsResponse' {journeyId} -> journeyId) (\s@JourneyExecutionActivityMetricsResponse' {} a -> s {journeyId = a} :: JourneyExecutionActivityMetricsResponse)

-- | The date and time, in ISO 8601 format, when Amazon Pinpoint last
-- evaluated the execution status of the activity and updated the data for
-- the metric.
journeyExecutionActivityMetricsResponse_lastEvaluatedTime :: Lens.Lens' JourneyExecutionActivityMetricsResponse Core.Text
journeyExecutionActivityMetricsResponse_lastEvaluatedTime = Lens.lens (\JourneyExecutionActivityMetricsResponse' {lastEvaluatedTime} -> lastEvaluatedTime) (\s@JourneyExecutionActivityMetricsResponse' {} a -> s {lastEvaluatedTime = a} :: JourneyExecutionActivityMetricsResponse)

-- | The unique identifier for the activity that the metric applies to.
journeyExecutionActivityMetricsResponse_journeyActivityId :: Lens.Lens' JourneyExecutionActivityMetricsResponse Core.Text
journeyExecutionActivityMetricsResponse_journeyActivityId = Lens.lens (\JourneyExecutionActivityMetricsResponse' {journeyActivityId} -> journeyActivityId) (\s@JourneyExecutionActivityMetricsResponse' {} a -> s {journeyActivityId = a} :: JourneyExecutionActivityMetricsResponse)

-- | The type of activity that the metric applies to. Possible values are:
--
-- -   CONDITIONAL_SPLIT - For a yes\/no split activity, which is an
--     activity that sends participants down one of two paths in a journey.
--
-- -   HOLDOUT - For a holdout activity, which is an activity that stops a
--     journey for a specified percentage of participants.
--
-- -   MESSAGE - For an email activity, which is an activity that sends an
--     email message to participants.
--
-- -   MULTI_CONDITIONAL_SPLIT - For a multivariate split activity, which
--     is an activity that sends participants down one of as many as five
--     paths in a journey.
--
-- -   RANDOM_SPLIT - For a random split activity, which is an activity
--     that sends specified percentages of participants down one of as many
--     as five paths in a journey.
--
-- -   WAIT - For a wait activity, which is an activity that waits for a
--     certain amount of time or until a specific date and time before
--     moving participants to the next activity in a journey.
journeyExecutionActivityMetricsResponse_activityType :: Lens.Lens' JourneyExecutionActivityMetricsResponse Core.Text
journeyExecutionActivityMetricsResponse_activityType = Lens.lens (\JourneyExecutionActivityMetricsResponse' {activityType} -> activityType) (\s@JourneyExecutionActivityMetricsResponse' {} a -> s {activityType = a} :: JourneyExecutionActivityMetricsResponse)

-- | The unique identifier for the application that the metric applies to.
journeyExecutionActivityMetricsResponse_applicationId :: Lens.Lens' JourneyExecutionActivityMetricsResponse Core.Text
journeyExecutionActivityMetricsResponse_applicationId = Lens.lens (\JourneyExecutionActivityMetricsResponse' {applicationId} -> applicationId) (\s@JourneyExecutionActivityMetricsResponse' {} a -> s {applicationId = a} :: JourneyExecutionActivityMetricsResponse)

instance
  Core.FromJSON
    JourneyExecutionActivityMetricsResponse
  where
  parseJSON =
    Core.withObject
      "JourneyExecutionActivityMetricsResponse"
      ( \x ->
          JourneyExecutionActivityMetricsResponse'
            Core.<$> (x Core..:? "Metrics" Core..!= Core.mempty)
            Core.<*> (x Core..: "JourneyId")
            Core.<*> (x Core..: "LastEvaluatedTime")
            Core.<*> (x Core..: "JourneyActivityId")
            Core.<*> (x Core..: "ActivityType")
            Core.<*> (x Core..: "ApplicationId")
      )

instance
  Core.Hashable
    JourneyExecutionActivityMetricsResponse

instance
  Core.NFData
    JourneyExecutionActivityMetricsResponse

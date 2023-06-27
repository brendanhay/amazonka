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
-- Module      : Amazonka.Pinpoint.Types.JourneyExecutionActivityMetricsResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.JourneyExecutionActivityMetricsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
    metrics :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | The unique identifier for the journey that the metric applies to.
    journeyId :: Prelude.Text,
    -- | The date and time, in ISO 8601 format, when Amazon Pinpoint last
    -- evaluated the execution status of the activity and updated the data for
    -- the metric.
    lastEvaluatedTime :: Prelude.Text,
    -- | The unique identifier for the activity that the metric applies to.
    journeyActivityId :: Prelude.Text,
    -- | The type of activity that the metric applies to. Possible values are:
    --
    -- -   CONDITIONAL_SPLIT – For a yes\/no split activity, which is an
    --     activity that sends participants down one of two paths in a journey.
    --
    -- -   HOLDOUT – For a holdout activity, which is an activity that stops a
    --     journey for a specified percentage of participants.
    --
    -- -   MESSAGE – For an email activity, which is an activity that sends an
    --     email message to participants.
    --
    -- -   MULTI_CONDITIONAL_SPLIT – For a multivariate split activity, which
    --     is an activity that sends participants down one of as many as five
    --     paths in a journey.
    --
    -- -   RANDOM_SPLIT – For a random split activity, which is an activity
    --     that sends specified percentages of participants down one of as many
    --     as five paths in a journey.
    --
    -- -   WAIT – For a wait activity, which is an activity that waits for a
    --     certain amount of time or until a specific date and time before
    --     moving participants to the next activity in a journey.
    activityType :: Prelude.Text,
    -- | The unique identifier for the application that the metric applies to.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- -   CONDITIONAL_SPLIT – For a yes\/no split activity, which is an
--     activity that sends participants down one of two paths in a journey.
--
-- -   HOLDOUT – For a holdout activity, which is an activity that stops a
--     journey for a specified percentage of participants.
--
-- -   MESSAGE – For an email activity, which is an activity that sends an
--     email message to participants.
--
-- -   MULTI_CONDITIONAL_SPLIT – For a multivariate split activity, which
--     is an activity that sends participants down one of as many as five
--     paths in a journey.
--
-- -   RANDOM_SPLIT – For a random split activity, which is an activity
--     that sends specified percentages of participants down one of as many
--     as five paths in a journey.
--
-- -   WAIT – For a wait activity, which is an activity that waits for a
--     certain amount of time or until a specific date and time before
--     moving participants to the next activity in a journey.
--
-- 'applicationId', 'journeyExecutionActivityMetricsResponse_applicationId' - The unique identifier for the application that the metric applies to.
newJourneyExecutionActivityMetricsResponse ::
  -- | 'journeyId'
  Prelude.Text ->
  -- | 'lastEvaluatedTime'
  Prelude.Text ->
  -- | 'journeyActivityId'
  Prelude.Text ->
  -- | 'activityType'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  JourneyExecutionActivityMetricsResponse
newJourneyExecutionActivityMetricsResponse
  pJourneyId_
  pLastEvaluatedTime_
  pJourneyActivityId_
  pActivityType_
  pApplicationId_ =
    JourneyExecutionActivityMetricsResponse'
      { metrics =
          Prelude.mempty,
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
journeyExecutionActivityMetricsResponse_metrics :: Lens.Lens' JourneyExecutionActivityMetricsResponse (Prelude.HashMap Prelude.Text Prelude.Text)
journeyExecutionActivityMetricsResponse_metrics = Lens.lens (\JourneyExecutionActivityMetricsResponse' {metrics} -> metrics) (\s@JourneyExecutionActivityMetricsResponse' {} a -> s {metrics = a} :: JourneyExecutionActivityMetricsResponse) Prelude.. Lens.coerced

-- | The unique identifier for the journey that the metric applies to.
journeyExecutionActivityMetricsResponse_journeyId :: Lens.Lens' JourneyExecutionActivityMetricsResponse Prelude.Text
journeyExecutionActivityMetricsResponse_journeyId = Lens.lens (\JourneyExecutionActivityMetricsResponse' {journeyId} -> journeyId) (\s@JourneyExecutionActivityMetricsResponse' {} a -> s {journeyId = a} :: JourneyExecutionActivityMetricsResponse)

-- | The date and time, in ISO 8601 format, when Amazon Pinpoint last
-- evaluated the execution status of the activity and updated the data for
-- the metric.
journeyExecutionActivityMetricsResponse_lastEvaluatedTime :: Lens.Lens' JourneyExecutionActivityMetricsResponse Prelude.Text
journeyExecutionActivityMetricsResponse_lastEvaluatedTime = Lens.lens (\JourneyExecutionActivityMetricsResponse' {lastEvaluatedTime} -> lastEvaluatedTime) (\s@JourneyExecutionActivityMetricsResponse' {} a -> s {lastEvaluatedTime = a} :: JourneyExecutionActivityMetricsResponse)

-- | The unique identifier for the activity that the metric applies to.
journeyExecutionActivityMetricsResponse_journeyActivityId :: Lens.Lens' JourneyExecutionActivityMetricsResponse Prelude.Text
journeyExecutionActivityMetricsResponse_journeyActivityId = Lens.lens (\JourneyExecutionActivityMetricsResponse' {journeyActivityId} -> journeyActivityId) (\s@JourneyExecutionActivityMetricsResponse' {} a -> s {journeyActivityId = a} :: JourneyExecutionActivityMetricsResponse)

-- | The type of activity that the metric applies to. Possible values are:
--
-- -   CONDITIONAL_SPLIT – For a yes\/no split activity, which is an
--     activity that sends participants down one of two paths in a journey.
--
-- -   HOLDOUT – For a holdout activity, which is an activity that stops a
--     journey for a specified percentage of participants.
--
-- -   MESSAGE – For an email activity, which is an activity that sends an
--     email message to participants.
--
-- -   MULTI_CONDITIONAL_SPLIT – For a multivariate split activity, which
--     is an activity that sends participants down one of as many as five
--     paths in a journey.
--
-- -   RANDOM_SPLIT – For a random split activity, which is an activity
--     that sends specified percentages of participants down one of as many
--     as five paths in a journey.
--
-- -   WAIT – For a wait activity, which is an activity that waits for a
--     certain amount of time or until a specific date and time before
--     moving participants to the next activity in a journey.
journeyExecutionActivityMetricsResponse_activityType :: Lens.Lens' JourneyExecutionActivityMetricsResponse Prelude.Text
journeyExecutionActivityMetricsResponse_activityType = Lens.lens (\JourneyExecutionActivityMetricsResponse' {activityType} -> activityType) (\s@JourneyExecutionActivityMetricsResponse' {} a -> s {activityType = a} :: JourneyExecutionActivityMetricsResponse)

-- | The unique identifier for the application that the metric applies to.
journeyExecutionActivityMetricsResponse_applicationId :: Lens.Lens' JourneyExecutionActivityMetricsResponse Prelude.Text
journeyExecutionActivityMetricsResponse_applicationId = Lens.lens (\JourneyExecutionActivityMetricsResponse' {applicationId} -> applicationId) (\s@JourneyExecutionActivityMetricsResponse' {} a -> s {applicationId = a} :: JourneyExecutionActivityMetricsResponse)

instance
  Data.FromJSON
    JourneyExecutionActivityMetricsResponse
  where
  parseJSON =
    Data.withObject
      "JourneyExecutionActivityMetricsResponse"
      ( \x ->
          JourneyExecutionActivityMetricsResponse'
            Prelude.<$> (x Data..:? "Metrics" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "JourneyId")
            Prelude.<*> (x Data..: "LastEvaluatedTime")
            Prelude.<*> (x Data..: "JourneyActivityId")
            Prelude.<*> (x Data..: "ActivityType")
            Prelude.<*> (x Data..: "ApplicationId")
      )

instance
  Prelude.Hashable
    JourneyExecutionActivityMetricsResponse
  where
  hashWithSalt
    _salt
    JourneyExecutionActivityMetricsResponse' {..} =
      _salt
        `Prelude.hashWithSalt` metrics
        `Prelude.hashWithSalt` journeyId
        `Prelude.hashWithSalt` lastEvaluatedTime
        `Prelude.hashWithSalt` journeyActivityId
        `Prelude.hashWithSalt` activityType
        `Prelude.hashWithSalt` applicationId

instance
  Prelude.NFData
    JourneyExecutionActivityMetricsResponse
  where
  rnf JourneyExecutionActivityMetricsResponse' {..} =
    Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf journeyId
      `Prelude.seq` Prelude.rnf lastEvaluatedTime
      `Prelude.seq` Prelude.rnf journeyActivityId
      `Prelude.seq` Prelude.rnf activityType
      `Prelude.seq` Prelude.rnf applicationId

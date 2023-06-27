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
-- Module      : Amazonka.Pinpoint.Types.JourneyRunExecutionActivityMetricsResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.JourneyRunExecutionActivityMetricsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the results of a query that retrieved the data for a standard
-- execution metric that applies to a journey activity for a particular
-- journey run, and provides information about that query.
--
-- /See:/ 'newJourneyRunExecutionActivityMetricsResponse' smart constructor.
data JourneyRunExecutionActivityMetricsResponse = JourneyRunExecutionActivityMetricsResponse'
  { -- | A JSON object that contains the results of the query. For information
    -- about the structure and contents of the results, see see
    -- <https://docs.aws.amazon.com//pinpoint/latest/developerguide/analytics-standard-metrics.html Standard Amazon Pinpoint analytics metrics>
    -- in the /Amazon Pinpoint Developer Guide/.
    metrics :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | The unique identifier for the journey that the metric applies to.
    journeyId :: Prelude.Text,
    -- | The date and time, in ISO 8601 format, when Amazon Pinpoint last
    -- evaluated the execution status of the activity for this journey run and
    -- updated the data for the metric.
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
    -- | The unique identifier for the journey run that the metric applies to.
    runId :: Prelude.Text,
    -- | The unique identifier for the application that the metric applies to.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JourneyRunExecutionActivityMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metrics', 'journeyRunExecutionActivityMetricsResponse_metrics' - A JSON object that contains the results of the query. For information
-- about the structure and contents of the results, see see
-- <https://docs.aws.amazon.com//pinpoint/latest/developerguide/analytics-standard-metrics.html Standard Amazon Pinpoint analytics metrics>
-- in the /Amazon Pinpoint Developer Guide/.
--
-- 'journeyId', 'journeyRunExecutionActivityMetricsResponse_journeyId' - The unique identifier for the journey that the metric applies to.
--
-- 'lastEvaluatedTime', 'journeyRunExecutionActivityMetricsResponse_lastEvaluatedTime' - The date and time, in ISO 8601 format, when Amazon Pinpoint last
-- evaluated the execution status of the activity for this journey run and
-- updated the data for the metric.
--
-- 'journeyActivityId', 'journeyRunExecutionActivityMetricsResponse_journeyActivityId' - The unique identifier for the activity that the metric applies to.
--
-- 'activityType', 'journeyRunExecutionActivityMetricsResponse_activityType' - The type of activity that the metric applies to. Possible values are:
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
-- 'runId', 'journeyRunExecutionActivityMetricsResponse_runId' - The unique identifier for the journey run that the metric applies to.
--
-- 'applicationId', 'journeyRunExecutionActivityMetricsResponse_applicationId' - The unique identifier for the application that the metric applies to.
newJourneyRunExecutionActivityMetricsResponse ::
  -- | 'journeyId'
  Prelude.Text ->
  -- | 'lastEvaluatedTime'
  Prelude.Text ->
  -- | 'journeyActivityId'
  Prelude.Text ->
  -- | 'activityType'
  Prelude.Text ->
  -- | 'runId'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  JourneyRunExecutionActivityMetricsResponse
newJourneyRunExecutionActivityMetricsResponse
  pJourneyId_
  pLastEvaluatedTime_
  pJourneyActivityId_
  pActivityType_
  pRunId_
  pApplicationId_ =
    JourneyRunExecutionActivityMetricsResponse'
      { metrics =
          Prelude.mempty,
        journeyId = pJourneyId_,
        lastEvaluatedTime =
          pLastEvaluatedTime_,
        journeyActivityId =
          pJourneyActivityId_,
        activityType = pActivityType_,
        runId = pRunId_,
        applicationId = pApplicationId_
      }

-- | A JSON object that contains the results of the query. For information
-- about the structure and contents of the results, see see
-- <https://docs.aws.amazon.com//pinpoint/latest/developerguide/analytics-standard-metrics.html Standard Amazon Pinpoint analytics metrics>
-- in the /Amazon Pinpoint Developer Guide/.
journeyRunExecutionActivityMetricsResponse_metrics :: Lens.Lens' JourneyRunExecutionActivityMetricsResponse (Prelude.HashMap Prelude.Text Prelude.Text)
journeyRunExecutionActivityMetricsResponse_metrics = Lens.lens (\JourneyRunExecutionActivityMetricsResponse' {metrics} -> metrics) (\s@JourneyRunExecutionActivityMetricsResponse' {} a -> s {metrics = a} :: JourneyRunExecutionActivityMetricsResponse) Prelude.. Lens.coerced

-- | The unique identifier for the journey that the metric applies to.
journeyRunExecutionActivityMetricsResponse_journeyId :: Lens.Lens' JourneyRunExecutionActivityMetricsResponse Prelude.Text
journeyRunExecutionActivityMetricsResponse_journeyId = Lens.lens (\JourneyRunExecutionActivityMetricsResponse' {journeyId} -> journeyId) (\s@JourneyRunExecutionActivityMetricsResponse' {} a -> s {journeyId = a} :: JourneyRunExecutionActivityMetricsResponse)

-- | The date and time, in ISO 8601 format, when Amazon Pinpoint last
-- evaluated the execution status of the activity for this journey run and
-- updated the data for the metric.
journeyRunExecutionActivityMetricsResponse_lastEvaluatedTime :: Lens.Lens' JourneyRunExecutionActivityMetricsResponse Prelude.Text
journeyRunExecutionActivityMetricsResponse_lastEvaluatedTime = Lens.lens (\JourneyRunExecutionActivityMetricsResponse' {lastEvaluatedTime} -> lastEvaluatedTime) (\s@JourneyRunExecutionActivityMetricsResponse' {} a -> s {lastEvaluatedTime = a} :: JourneyRunExecutionActivityMetricsResponse)

-- | The unique identifier for the activity that the metric applies to.
journeyRunExecutionActivityMetricsResponse_journeyActivityId :: Lens.Lens' JourneyRunExecutionActivityMetricsResponse Prelude.Text
journeyRunExecutionActivityMetricsResponse_journeyActivityId = Lens.lens (\JourneyRunExecutionActivityMetricsResponse' {journeyActivityId} -> journeyActivityId) (\s@JourneyRunExecutionActivityMetricsResponse' {} a -> s {journeyActivityId = a} :: JourneyRunExecutionActivityMetricsResponse)

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
journeyRunExecutionActivityMetricsResponse_activityType :: Lens.Lens' JourneyRunExecutionActivityMetricsResponse Prelude.Text
journeyRunExecutionActivityMetricsResponse_activityType = Lens.lens (\JourneyRunExecutionActivityMetricsResponse' {activityType} -> activityType) (\s@JourneyRunExecutionActivityMetricsResponse' {} a -> s {activityType = a} :: JourneyRunExecutionActivityMetricsResponse)

-- | The unique identifier for the journey run that the metric applies to.
journeyRunExecutionActivityMetricsResponse_runId :: Lens.Lens' JourneyRunExecutionActivityMetricsResponse Prelude.Text
journeyRunExecutionActivityMetricsResponse_runId = Lens.lens (\JourneyRunExecutionActivityMetricsResponse' {runId} -> runId) (\s@JourneyRunExecutionActivityMetricsResponse' {} a -> s {runId = a} :: JourneyRunExecutionActivityMetricsResponse)

-- | The unique identifier for the application that the metric applies to.
journeyRunExecutionActivityMetricsResponse_applicationId :: Lens.Lens' JourneyRunExecutionActivityMetricsResponse Prelude.Text
journeyRunExecutionActivityMetricsResponse_applicationId = Lens.lens (\JourneyRunExecutionActivityMetricsResponse' {applicationId} -> applicationId) (\s@JourneyRunExecutionActivityMetricsResponse' {} a -> s {applicationId = a} :: JourneyRunExecutionActivityMetricsResponse)

instance
  Data.FromJSON
    JourneyRunExecutionActivityMetricsResponse
  where
  parseJSON =
    Data.withObject
      "JourneyRunExecutionActivityMetricsResponse"
      ( \x ->
          JourneyRunExecutionActivityMetricsResponse'
            Prelude.<$> (x Data..:? "Metrics" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "JourneyId")
            Prelude.<*> (x Data..: "LastEvaluatedTime")
            Prelude.<*> (x Data..: "JourneyActivityId")
            Prelude.<*> (x Data..: "ActivityType")
            Prelude.<*> (x Data..: "RunId")
            Prelude.<*> (x Data..: "ApplicationId")
      )

instance
  Prelude.Hashable
    JourneyRunExecutionActivityMetricsResponse
  where
  hashWithSalt
    _salt
    JourneyRunExecutionActivityMetricsResponse' {..} =
      _salt
        `Prelude.hashWithSalt` metrics
        `Prelude.hashWithSalt` journeyId
        `Prelude.hashWithSalt` lastEvaluatedTime
        `Prelude.hashWithSalt` journeyActivityId
        `Prelude.hashWithSalt` activityType
        `Prelude.hashWithSalt` runId
        `Prelude.hashWithSalt` applicationId

instance
  Prelude.NFData
    JourneyRunExecutionActivityMetricsResponse
  where
  rnf JourneyRunExecutionActivityMetricsResponse' {..} =
    Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf journeyId
      `Prelude.seq` Prelude.rnf lastEvaluatedTime
      `Prelude.seq` Prelude.rnf journeyActivityId
      `Prelude.seq` Prelude.rnf activityType
      `Prelude.seq` Prelude.rnf runId
      `Prelude.seq` Prelude.rnf applicationId

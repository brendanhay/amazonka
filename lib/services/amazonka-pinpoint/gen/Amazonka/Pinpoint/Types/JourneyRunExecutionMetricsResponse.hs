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
-- Module      : Amazonka.Pinpoint.Types.JourneyRunExecutionMetricsResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.JourneyRunExecutionMetricsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the results of a query that retrieved the data for a standard
-- execution metric that applies to a journey run, and provides information
-- about that query.
--
-- /See:/ 'newJourneyRunExecutionMetricsResponse' smart constructor.
data JourneyRunExecutionMetricsResponse = JourneyRunExecutionMetricsResponse'
  { -- | A JSON object that contains the results of the query. For information
    -- about the structure and contents of the results, see the
    -- <https://docs.aws.amazon.com//pinpoint/latest/developerguide/analytics-standard-metrics.html Standard Amazon Pinpoint analytics metrics>
    -- in the /Amazon Pinpoint Developer Guide/.
    metrics :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | The unique identifier for the journey that the metric applies to.
    journeyId :: Prelude.Text,
    -- | The date and time, in ISO 8601 format, when Amazon Pinpoint last
    -- evaluated the journey run and updated the data for the metric.
    lastEvaluatedTime :: Prelude.Text,
    -- | The unique identifier for the journey run that the metric applies to.
    runId :: Prelude.Text,
    -- | The unique identifier for the application that the metric applies to.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JourneyRunExecutionMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metrics', 'journeyRunExecutionMetricsResponse_metrics' - A JSON object that contains the results of the query. For information
-- about the structure and contents of the results, see the
-- <https://docs.aws.amazon.com//pinpoint/latest/developerguide/analytics-standard-metrics.html Standard Amazon Pinpoint analytics metrics>
-- in the /Amazon Pinpoint Developer Guide/.
--
-- 'journeyId', 'journeyRunExecutionMetricsResponse_journeyId' - The unique identifier for the journey that the metric applies to.
--
-- 'lastEvaluatedTime', 'journeyRunExecutionMetricsResponse_lastEvaluatedTime' - The date and time, in ISO 8601 format, when Amazon Pinpoint last
-- evaluated the journey run and updated the data for the metric.
--
-- 'runId', 'journeyRunExecutionMetricsResponse_runId' - The unique identifier for the journey run that the metric applies to.
--
-- 'applicationId', 'journeyRunExecutionMetricsResponse_applicationId' - The unique identifier for the application that the metric applies to.
newJourneyRunExecutionMetricsResponse ::
  -- | 'journeyId'
  Prelude.Text ->
  -- | 'lastEvaluatedTime'
  Prelude.Text ->
  -- | 'runId'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  JourneyRunExecutionMetricsResponse
newJourneyRunExecutionMetricsResponse
  pJourneyId_
  pLastEvaluatedTime_
  pRunId_
  pApplicationId_ =
    JourneyRunExecutionMetricsResponse'
      { metrics =
          Prelude.mempty,
        journeyId = pJourneyId_,
        lastEvaluatedTime = pLastEvaluatedTime_,
        runId = pRunId_,
        applicationId = pApplicationId_
      }

-- | A JSON object that contains the results of the query. For information
-- about the structure and contents of the results, see the
-- <https://docs.aws.amazon.com//pinpoint/latest/developerguide/analytics-standard-metrics.html Standard Amazon Pinpoint analytics metrics>
-- in the /Amazon Pinpoint Developer Guide/.
journeyRunExecutionMetricsResponse_metrics :: Lens.Lens' JourneyRunExecutionMetricsResponse (Prelude.HashMap Prelude.Text Prelude.Text)
journeyRunExecutionMetricsResponse_metrics = Lens.lens (\JourneyRunExecutionMetricsResponse' {metrics} -> metrics) (\s@JourneyRunExecutionMetricsResponse' {} a -> s {metrics = a} :: JourneyRunExecutionMetricsResponse) Prelude.. Lens.coerced

-- | The unique identifier for the journey that the metric applies to.
journeyRunExecutionMetricsResponse_journeyId :: Lens.Lens' JourneyRunExecutionMetricsResponse Prelude.Text
journeyRunExecutionMetricsResponse_journeyId = Lens.lens (\JourneyRunExecutionMetricsResponse' {journeyId} -> journeyId) (\s@JourneyRunExecutionMetricsResponse' {} a -> s {journeyId = a} :: JourneyRunExecutionMetricsResponse)

-- | The date and time, in ISO 8601 format, when Amazon Pinpoint last
-- evaluated the journey run and updated the data for the metric.
journeyRunExecutionMetricsResponse_lastEvaluatedTime :: Lens.Lens' JourneyRunExecutionMetricsResponse Prelude.Text
journeyRunExecutionMetricsResponse_lastEvaluatedTime = Lens.lens (\JourneyRunExecutionMetricsResponse' {lastEvaluatedTime} -> lastEvaluatedTime) (\s@JourneyRunExecutionMetricsResponse' {} a -> s {lastEvaluatedTime = a} :: JourneyRunExecutionMetricsResponse)

-- | The unique identifier for the journey run that the metric applies to.
journeyRunExecutionMetricsResponse_runId :: Lens.Lens' JourneyRunExecutionMetricsResponse Prelude.Text
journeyRunExecutionMetricsResponse_runId = Lens.lens (\JourneyRunExecutionMetricsResponse' {runId} -> runId) (\s@JourneyRunExecutionMetricsResponse' {} a -> s {runId = a} :: JourneyRunExecutionMetricsResponse)

-- | The unique identifier for the application that the metric applies to.
journeyRunExecutionMetricsResponse_applicationId :: Lens.Lens' JourneyRunExecutionMetricsResponse Prelude.Text
journeyRunExecutionMetricsResponse_applicationId = Lens.lens (\JourneyRunExecutionMetricsResponse' {applicationId} -> applicationId) (\s@JourneyRunExecutionMetricsResponse' {} a -> s {applicationId = a} :: JourneyRunExecutionMetricsResponse)

instance
  Data.FromJSON
    JourneyRunExecutionMetricsResponse
  where
  parseJSON =
    Data.withObject
      "JourneyRunExecutionMetricsResponse"
      ( \x ->
          JourneyRunExecutionMetricsResponse'
            Prelude.<$> (x Data..:? "Metrics" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "JourneyId")
            Prelude.<*> (x Data..: "LastEvaluatedTime")
            Prelude.<*> (x Data..: "RunId")
            Prelude.<*> (x Data..: "ApplicationId")
      )

instance
  Prelude.Hashable
    JourneyRunExecutionMetricsResponse
  where
  hashWithSalt
    _salt
    JourneyRunExecutionMetricsResponse' {..} =
      _salt
        `Prelude.hashWithSalt` metrics
        `Prelude.hashWithSalt` journeyId
        `Prelude.hashWithSalt` lastEvaluatedTime
        `Prelude.hashWithSalt` runId
        `Prelude.hashWithSalt` applicationId

instance
  Prelude.NFData
    JourneyRunExecutionMetricsResponse
  where
  rnf JourneyRunExecutionMetricsResponse' {..} =
    Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf journeyId
      `Prelude.seq` Prelude.rnf lastEvaluatedTime
      `Prelude.seq` Prelude.rnf runId
      `Prelude.seq` Prelude.rnf applicationId

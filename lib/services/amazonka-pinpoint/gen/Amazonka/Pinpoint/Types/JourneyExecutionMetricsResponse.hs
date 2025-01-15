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
-- Module      : Amazonka.Pinpoint.Types.JourneyExecutionMetricsResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.JourneyExecutionMetricsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the results of a query that retrieved the data for a standard
-- execution metric that applies to a journey, and provides information
-- about that query.
--
-- /See:/ 'newJourneyExecutionMetricsResponse' smart constructor.
data JourneyExecutionMetricsResponse = JourneyExecutionMetricsResponse'
  { -- | A JSON object that contains the results of the query. For information
    -- about the structure and contents of the results, see the
    -- <https://docs.aws.amazon.com//pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
    metrics :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | The unique identifier for the journey that the metric applies to.
    journeyId :: Prelude.Text,
    -- | The date and time, in ISO 8601 format, when Amazon Pinpoint last
    -- evaluated the journey and updated the data for the metric.
    lastEvaluatedTime :: Prelude.Text,
    -- | The unique identifier for the application that the metric applies to.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JourneyExecutionMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metrics', 'journeyExecutionMetricsResponse_metrics' - A JSON object that contains the results of the query. For information
-- about the structure and contents of the results, see the
-- <https://docs.aws.amazon.com//pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
--
-- 'journeyId', 'journeyExecutionMetricsResponse_journeyId' - The unique identifier for the journey that the metric applies to.
--
-- 'lastEvaluatedTime', 'journeyExecutionMetricsResponse_lastEvaluatedTime' - The date and time, in ISO 8601 format, when Amazon Pinpoint last
-- evaluated the journey and updated the data for the metric.
--
-- 'applicationId', 'journeyExecutionMetricsResponse_applicationId' - The unique identifier for the application that the metric applies to.
newJourneyExecutionMetricsResponse ::
  -- | 'journeyId'
  Prelude.Text ->
  -- | 'lastEvaluatedTime'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  JourneyExecutionMetricsResponse
newJourneyExecutionMetricsResponse
  pJourneyId_
  pLastEvaluatedTime_
  pApplicationId_ =
    JourneyExecutionMetricsResponse'
      { metrics =
          Prelude.mempty,
        journeyId = pJourneyId_,
        lastEvaluatedTime = pLastEvaluatedTime_,
        applicationId = pApplicationId_
      }

-- | A JSON object that contains the results of the query. For information
-- about the structure and contents of the results, see the
-- <https://docs.aws.amazon.com//pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
journeyExecutionMetricsResponse_metrics :: Lens.Lens' JourneyExecutionMetricsResponse (Prelude.HashMap Prelude.Text Prelude.Text)
journeyExecutionMetricsResponse_metrics = Lens.lens (\JourneyExecutionMetricsResponse' {metrics} -> metrics) (\s@JourneyExecutionMetricsResponse' {} a -> s {metrics = a} :: JourneyExecutionMetricsResponse) Prelude.. Lens.coerced

-- | The unique identifier for the journey that the metric applies to.
journeyExecutionMetricsResponse_journeyId :: Lens.Lens' JourneyExecutionMetricsResponse Prelude.Text
journeyExecutionMetricsResponse_journeyId = Lens.lens (\JourneyExecutionMetricsResponse' {journeyId} -> journeyId) (\s@JourneyExecutionMetricsResponse' {} a -> s {journeyId = a} :: JourneyExecutionMetricsResponse)

-- | The date and time, in ISO 8601 format, when Amazon Pinpoint last
-- evaluated the journey and updated the data for the metric.
journeyExecutionMetricsResponse_lastEvaluatedTime :: Lens.Lens' JourneyExecutionMetricsResponse Prelude.Text
journeyExecutionMetricsResponse_lastEvaluatedTime = Lens.lens (\JourneyExecutionMetricsResponse' {lastEvaluatedTime} -> lastEvaluatedTime) (\s@JourneyExecutionMetricsResponse' {} a -> s {lastEvaluatedTime = a} :: JourneyExecutionMetricsResponse)

-- | The unique identifier for the application that the metric applies to.
journeyExecutionMetricsResponse_applicationId :: Lens.Lens' JourneyExecutionMetricsResponse Prelude.Text
journeyExecutionMetricsResponse_applicationId = Lens.lens (\JourneyExecutionMetricsResponse' {applicationId} -> applicationId) (\s@JourneyExecutionMetricsResponse' {} a -> s {applicationId = a} :: JourneyExecutionMetricsResponse)

instance
  Data.FromJSON
    JourneyExecutionMetricsResponse
  where
  parseJSON =
    Data.withObject
      "JourneyExecutionMetricsResponse"
      ( \x ->
          JourneyExecutionMetricsResponse'
            Prelude.<$> (x Data..:? "Metrics" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "JourneyId")
            Prelude.<*> (x Data..: "LastEvaluatedTime")
            Prelude.<*> (x Data..: "ApplicationId")
      )

instance
  Prelude.Hashable
    JourneyExecutionMetricsResponse
  where
  hashWithSalt
    _salt
    JourneyExecutionMetricsResponse' {..} =
      _salt
        `Prelude.hashWithSalt` metrics
        `Prelude.hashWithSalt` journeyId
        `Prelude.hashWithSalt` lastEvaluatedTime
        `Prelude.hashWithSalt` applicationId

instance
  Prelude.NFData
    JourneyExecutionMetricsResponse
  where
  rnf JourneyExecutionMetricsResponse' {..} =
    Prelude.rnf metrics `Prelude.seq`
      Prelude.rnf journeyId `Prelude.seq`
        Prelude.rnf lastEvaluatedTime `Prelude.seq`
          Prelude.rnf applicationId

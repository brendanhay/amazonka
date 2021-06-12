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
-- Module      : Network.AWS.Pinpoint.Types.JourneyExecutionMetricsResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyExecutionMetricsResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides the results of a query that retrieved the data for a standard
-- execution metric that applies to a journey, and provides information
-- about that query.
--
-- /See:/ 'newJourneyExecutionMetricsResponse' smart constructor.
data JourneyExecutionMetricsResponse = JourneyExecutionMetricsResponse'
  { -- | A JSON object that contains the results of the query. For information
    -- about the structure and contents of the results, see the
    -- <https://docs.aws.amazon.com//pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
    metrics :: Core.HashMap Core.Text Core.Text,
    -- | The unique identifier for the journey that the metric applies to.
    journeyId :: Core.Text,
    -- | The date and time, in ISO 8601 format, when Amazon Pinpoint last
    -- evaluated the journey and updated the data for the metric.
    lastEvaluatedTime :: Core.Text,
    -- | The unique identifier for the application that the metric applies to.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'lastEvaluatedTime'
  Core.Text ->
  -- | 'applicationId'
  Core.Text ->
  JourneyExecutionMetricsResponse
newJourneyExecutionMetricsResponse
  pJourneyId_
  pLastEvaluatedTime_
  pApplicationId_ =
    JourneyExecutionMetricsResponse'
      { metrics =
          Core.mempty,
        journeyId = pJourneyId_,
        lastEvaluatedTime = pLastEvaluatedTime_,
        applicationId = pApplicationId_
      }

-- | A JSON object that contains the results of the query. For information
-- about the structure and contents of the results, see the
-- <https://docs.aws.amazon.com//pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
journeyExecutionMetricsResponse_metrics :: Lens.Lens' JourneyExecutionMetricsResponse (Core.HashMap Core.Text Core.Text)
journeyExecutionMetricsResponse_metrics = Lens.lens (\JourneyExecutionMetricsResponse' {metrics} -> metrics) (\s@JourneyExecutionMetricsResponse' {} a -> s {metrics = a} :: JourneyExecutionMetricsResponse) Core.. Lens._Coerce

-- | The unique identifier for the journey that the metric applies to.
journeyExecutionMetricsResponse_journeyId :: Lens.Lens' JourneyExecutionMetricsResponse Core.Text
journeyExecutionMetricsResponse_journeyId = Lens.lens (\JourneyExecutionMetricsResponse' {journeyId} -> journeyId) (\s@JourneyExecutionMetricsResponse' {} a -> s {journeyId = a} :: JourneyExecutionMetricsResponse)

-- | The date and time, in ISO 8601 format, when Amazon Pinpoint last
-- evaluated the journey and updated the data for the metric.
journeyExecutionMetricsResponse_lastEvaluatedTime :: Lens.Lens' JourneyExecutionMetricsResponse Core.Text
journeyExecutionMetricsResponse_lastEvaluatedTime = Lens.lens (\JourneyExecutionMetricsResponse' {lastEvaluatedTime} -> lastEvaluatedTime) (\s@JourneyExecutionMetricsResponse' {} a -> s {lastEvaluatedTime = a} :: JourneyExecutionMetricsResponse)

-- | The unique identifier for the application that the metric applies to.
journeyExecutionMetricsResponse_applicationId :: Lens.Lens' JourneyExecutionMetricsResponse Core.Text
journeyExecutionMetricsResponse_applicationId = Lens.lens (\JourneyExecutionMetricsResponse' {applicationId} -> applicationId) (\s@JourneyExecutionMetricsResponse' {} a -> s {applicationId = a} :: JourneyExecutionMetricsResponse)

instance
  Core.FromJSON
    JourneyExecutionMetricsResponse
  where
  parseJSON =
    Core.withObject
      "JourneyExecutionMetricsResponse"
      ( \x ->
          JourneyExecutionMetricsResponse'
            Core.<$> (x Core..:? "Metrics" Core..!= Core.mempty)
            Core.<*> (x Core..: "JourneyId")
            Core.<*> (x Core..: "LastEvaluatedTime")
            Core.<*> (x Core..: "ApplicationId")
      )

instance
  Core.Hashable
    JourneyExecutionMetricsResponse

instance Core.NFData JourneyExecutionMetricsResponse

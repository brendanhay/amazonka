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
-- Module      : Amazonka.Pinpoint.Types.JourneyDateRangeKpiResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.JourneyDateRangeKpiResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.BaseKpiResult
import qualified Amazonka.Prelude as Prelude

-- | Provides the results of a query that retrieved the data for a standard
-- engagement metric that applies to a journey, and provides information
-- about that query.
--
-- /See:/ 'newJourneyDateRangeKpiResponse' smart constructor.
data JourneyDateRangeKpiResponse = JourneyDateRangeKpiResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null for the Journey
    -- Engagement Metrics resource because the resource returns all results in
    -- a single page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that contains the results of the query. Each object
    -- contains the value for the metric and metadata about that value.
    kpiResult :: BaseKpiResult,
    -- | The name of the metric, also referred to as a /key performance indicator
    -- (KPI)/, that the data was retrieved for. This value describes the
    -- associated metric and consists of two or more terms, which are comprised
    -- of lowercase alphanumeric characters, separated by a hyphen. For a list
    -- of possible values, see the
    -- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
    kpiName :: Prelude.Text,
    -- | The unique identifier for the journey that the metric applies to.
    journeyId :: Prelude.Text,
    -- | The last date and time of the date range that was used to filter the
    -- query results, in extended ISO 8601 format. The date range is inclusive.
    endTime :: Data.ISO8601,
    -- | The first date and time of the date range that was used to filter the
    -- query results, in extended ISO 8601 format. The date range is inclusive.
    startTime :: Data.ISO8601,
    -- | The unique identifier for the application that the metric applies to.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JourneyDateRangeKpiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'journeyDateRangeKpiResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null for the Journey
-- Engagement Metrics resource because the resource returns all results in
-- a single page.
--
-- 'kpiResult', 'journeyDateRangeKpiResponse_kpiResult' - An array of objects that contains the results of the query. Each object
-- contains the value for the metric and metadata about that value.
--
-- 'kpiName', 'journeyDateRangeKpiResponse_kpiName' - The name of the metric, also referred to as a /key performance indicator
-- (KPI)/, that the data was retrieved for. This value describes the
-- associated metric and consists of two or more terms, which are comprised
-- of lowercase alphanumeric characters, separated by a hyphen. For a list
-- of possible values, see the
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
--
-- 'journeyId', 'journeyDateRangeKpiResponse_journeyId' - The unique identifier for the journey that the metric applies to.
--
-- 'endTime', 'journeyDateRangeKpiResponse_endTime' - The last date and time of the date range that was used to filter the
-- query results, in extended ISO 8601 format. The date range is inclusive.
--
-- 'startTime', 'journeyDateRangeKpiResponse_startTime' - The first date and time of the date range that was used to filter the
-- query results, in extended ISO 8601 format. The date range is inclusive.
--
-- 'applicationId', 'journeyDateRangeKpiResponse_applicationId' - The unique identifier for the application that the metric applies to.
newJourneyDateRangeKpiResponse ::
  -- | 'kpiResult'
  BaseKpiResult ->
  -- | 'kpiName'
  Prelude.Text ->
  -- | 'journeyId'
  Prelude.Text ->
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'applicationId'
  Prelude.Text ->
  JourneyDateRangeKpiResponse
newJourneyDateRangeKpiResponse
  pKpiResult_
  pKpiName_
  pJourneyId_
  pEndTime_
  pStartTime_
  pApplicationId_ =
    JourneyDateRangeKpiResponse'
      { nextToken =
          Prelude.Nothing,
        kpiResult = pKpiResult_,
        kpiName = pKpiName_,
        journeyId = pJourneyId_,
        endTime = Data._Time Lens.# pEndTime_,
        startTime = Data._Time Lens.# pStartTime_,
        applicationId = pApplicationId_
      }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null for the Journey
-- Engagement Metrics resource because the resource returns all results in
-- a single page.
journeyDateRangeKpiResponse_nextToken :: Lens.Lens' JourneyDateRangeKpiResponse (Prelude.Maybe Prelude.Text)
journeyDateRangeKpiResponse_nextToken = Lens.lens (\JourneyDateRangeKpiResponse' {nextToken} -> nextToken) (\s@JourneyDateRangeKpiResponse' {} a -> s {nextToken = a} :: JourneyDateRangeKpiResponse)

-- | An array of objects that contains the results of the query. Each object
-- contains the value for the metric and metadata about that value.
journeyDateRangeKpiResponse_kpiResult :: Lens.Lens' JourneyDateRangeKpiResponse BaseKpiResult
journeyDateRangeKpiResponse_kpiResult = Lens.lens (\JourneyDateRangeKpiResponse' {kpiResult} -> kpiResult) (\s@JourneyDateRangeKpiResponse' {} a -> s {kpiResult = a} :: JourneyDateRangeKpiResponse)

-- | The name of the metric, also referred to as a /key performance indicator
-- (KPI)/, that the data was retrieved for. This value describes the
-- associated metric and consists of two or more terms, which are comprised
-- of lowercase alphanumeric characters, separated by a hyphen. For a list
-- of possible values, see the
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
journeyDateRangeKpiResponse_kpiName :: Lens.Lens' JourneyDateRangeKpiResponse Prelude.Text
journeyDateRangeKpiResponse_kpiName = Lens.lens (\JourneyDateRangeKpiResponse' {kpiName} -> kpiName) (\s@JourneyDateRangeKpiResponse' {} a -> s {kpiName = a} :: JourneyDateRangeKpiResponse)

-- | The unique identifier for the journey that the metric applies to.
journeyDateRangeKpiResponse_journeyId :: Lens.Lens' JourneyDateRangeKpiResponse Prelude.Text
journeyDateRangeKpiResponse_journeyId = Lens.lens (\JourneyDateRangeKpiResponse' {journeyId} -> journeyId) (\s@JourneyDateRangeKpiResponse' {} a -> s {journeyId = a} :: JourneyDateRangeKpiResponse)

-- | The last date and time of the date range that was used to filter the
-- query results, in extended ISO 8601 format. The date range is inclusive.
journeyDateRangeKpiResponse_endTime :: Lens.Lens' JourneyDateRangeKpiResponse Prelude.UTCTime
journeyDateRangeKpiResponse_endTime = Lens.lens (\JourneyDateRangeKpiResponse' {endTime} -> endTime) (\s@JourneyDateRangeKpiResponse' {} a -> s {endTime = a} :: JourneyDateRangeKpiResponse) Prelude.. Data._Time

-- | The first date and time of the date range that was used to filter the
-- query results, in extended ISO 8601 format. The date range is inclusive.
journeyDateRangeKpiResponse_startTime :: Lens.Lens' JourneyDateRangeKpiResponse Prelude.UTCTime
journeyDateRangeKpiResponse_startTime = Lens.lens (\JourneyDateRangeKpiResponse' {startTime} -> startTime) (\s@JourneyDateRangeKpiResponse' {} a -> s {startTime = a} :: JourneyDateRangeKpiResponse) Prelude.. Data._Time

-- | The unique identifier for the application that the metric applies to.
journeyDateRangeKpiResponse_applicationId :: Lens.Lens' JourneyDateRangeKpiResponse Prelude.Text
journeyDateRangeKpiResponse_applicationId = Lens.lens (\JourneyDateRangeKpiResponse' {applicationId} -> applicationId) (\s@JourneyDateRangeKpiResponse' {} a -> s {applicationId = a} :: JourneyDateRangeKpiResponse)

instance Data.FromJSON JourneyDateRangeKpiResponse where
  parseJSON =
    Data.withObject
      "JourneyDateRangeKpiResponse"
      ( \x ->
          JourneyDateRangeKpiResponse'
            Prelude.<$> (x Data..:? "NextToken")
            Prelude.<*> (x Data..: "KpiResult")
            Prelude.<*> (x Data..: "KpiName")
            Prelude.<*> (x Data..: "JourneyId")
            Prelude.<*> (x Data..: "EndTime")
            Prelude.<*> (x Data..: "StartTime")
            Prelude.<*> (x Data..: "ApplicationId")
      )

instance Prelude.Hashable JourneyDateRangeKpiResponse where
  hashWithSalt _salt JourneyDateRangeKpiResponse' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` kpiResult
      `Prelude.hashWithSalt` kpiName
      `Prelude.hashWithSalt` journeyId
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData JourneyDateRangeKpiResponse where
  rnf JourneyDateRangeKpiResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf kpiResult
      `Prelude.seq` Prelude.rnf kpiName
      `Prelude.seq` Prelude.rnf journeyId
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf applicationId

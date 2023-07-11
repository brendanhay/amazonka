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
-- Module      : Amazonka.Pinpoint.Types.ApplicationDateRangeKpiResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ApplicationDateRangeKpiResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.BaseKpiResult
import qualified Amazonka.Prelude as Prelude

-- | Provides the results of a query that retrieved the data for a standard
-- metric that applies to an application, and provides information about
-- that query.
--
-- /See:/ 'newApplicationDateRangeKpiResponse' smart constructor.
data ApplicationDateRangeKpiResponse = ApplicationDateRangeKpiResponse'
  { -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null for the Application
    -- Metrics resource because the resource returns all results in a single
    -- page.
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
-- Create a value of 'ApplicationDateRangeKpiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'applicationDateRangeKpiResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null for the Application
-- Metrics resource because the resource returns all results in a single
-- page.
--
-- 'kpiResult', 'applicationDateRangeKpiResponse_kpiResult' - An array of objects that contains the results of the query. Each object
-- contains the value for the metric and metadata about that value.
--
-- 'kpiName', 'applicationDateRangeKpiResponse_kpiName' - The name of the metric, also referred to as a /key performance indicator
-- (KPI)/, that the data was retrieved for. This value describes the
-- associated metric and consists of two or more terms, which are comprised
-- of lowercase alphanumeric characters, separated by a hyphen. For a list
-- of possible values, see the
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
--
-- 'endTime', 'applicationDateRangeKpiResponse_endTime' - The last date and time of the date range that was used to filter the
-- query results, in extended ISO 8601 format. The date range is inclusive.
--
-- 'startTime', 'applicationDateRangeKpiResponse_startTime' - The first date and time of the date range that was used to filter the
-- query results, in extended ISO 8601 format. The date range is inclusive.
--
-- 'applicationId', 'applicationDateRangeKpiResponse_applicationId' - The unique identifier for the application that the metric applies to.
newApplicationDateRangeKpiResponse ::
  -- | 'kpiResult'
  BaseKpiResult ->
  -- | 'kpiName'
  Prelude.Text ->
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'applicationId'
  Prelude.Text ->
  ApplicationDateRangeKpiResponse
newApplicationDateRangeKpiResponse
  pKpiResult_
  pKpiName_
  pEndTime_
  pStartTime_
  pApplicationId_ =
    ApplicationDateRangeKpiResponse'
      { nextToken =
          Prelude.Nothing,
        kpiResult = pKpiResult_,
        kpiName = pKpiName_,
        endTime = Data._Time Lens.# pEndTime_,
        startTime = Data._Time Lens.# pStartTime_,
        applicationId = pApplicationId_
      }

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null for the Application
-- Metrics resource because the resource returns all results in a single
-- page.
applicationDateRangeKpiResponse_nextToken :: Lens.Lens' ApplicationDateRangeKpiResponse (Prelude.Maybe Prelude.Text)
applicationDateRangeKpiResponse_nextToken = Lens.lens (\ApplicationDateRangeKpiResponse' {nextToken} -> nextToken) (\s@ApplicationDateRangeKpiResponse' {} a -> s {nextToken = a} :: ApplicationDateRangeKpiResponse)

-- | An array of objects that contains the results of the query. Each object
-- contains the value for the metric and metadata about that value.
applicationDateRangeKpiResponse_kpiResult :: Lens.Lens' ApplicationDateRangeKpiResponse BaseKpiResult
applicationDateRangeKpiResponse_kpiResult = Lens.lens (\ApplicationDateRangeKpiResponse' {kpiResult} -> kpiResult) (\s@ApplicationDateRangeKpiResponse' {} a -> s {kpiResult = a} :: ApplicationDateRangeKpiResponse)

-- | The name of the metric, also referred to as a /key performance indicator
-- (KPI)/, that the data was retrieved for. This value describes the
-- associated metric and consists of two or more terms, which are comprised
-- of lowercase alphanumeric characters, separated by a hyphen. For a list
-- of possible values, see the
-- <https://docs.aws.amazon.com/pinpoint/latest/developerguide/analytics-standard-metrics.html Amazon Pinpoint Developer Guide>.
applicationDateRangeKpiResponse_kpiName :: Lens.Lens' ApplicationDateRangeKpiResponse Prelude.Text
applicationDateRangeKpiResponse_kpiName = Lens.lens (\ApplicationDateRangeKpiResponse' {kpiName} -> kpiName) (\s@ApplicationDateRangeKpiResponse' {} a -> s {kpiName = a} :: ApplicationDateRangeKpiResponse)

-- | The last date and time of the date range that was used to filter the
-- query results, in extended ISO 8601 format. The date range is inclusive.
applicationDateRangeKpiResponse_endTime :: Lens.Lens' ApplicationDateRangeKpiResponse Prelude.UTCTime
applicationDateRangeKpiResponse_endTime = Lens.lens (\ApplicationDateRangeKpiResponse' {endTime} -> endTime) (\s@ApplicationDateRangeKpiResponse' {} a -> s {endTime = a} :: ApplicationDateRangeKpiResponse) Prelude.. Data._Time

-- | The first date and time of the date range that was used to filter the
-- query results, in extended ISO 8601 format. The date range is inclusive.
applicationDateRangeKpiResponse_startTime :: Lens.Lens' ApplicationDateRangeKpiResponse Prelude.UTCTime
applicationDateRangeKpiResponse_startTime = Lens.lens (\ApplicationDateRangeKpiResponse' {startTime} -> startTime) (\s@ApplicationDateRangeKpiResponse' {} a -> s {startTime = a} :: ApplicationDateRangeKpiResponse) Prelude.. Data._Time

-- | The unique identifier for the application that the metric applies to.
applicationDateRangeKpiResponse_applicationId :: Lens.Lens' ApplicationDateRangeKpiResponse Prelude.Text
applicationDateRangeKpiResponse_applicationId = Lens.lens (\ApplicationDateRangeKpiResponse' {applicationId} -> applicationId) (\s@ApplicationDateRangeKpiResponse' {} a -> s {applicationId = a} :: ApplicationDateRangeKpiResponse)

instance
  Data.FromJSON
    ApplicationDateRangeKpiResponse
  where
  parseJSON =
    Data.withObject
      "ApplicationDateRangeKpiResponse"
      ( \x ->
          ApplicationDateRangeKpiResponse'
            Prelude.<$> (x Data..:? "NextToken")
            Prelude.<*> (x Data..: "KpiResult")
            Prelude.<*> (x Data..: "KpiName")
            Prelude.<*> (x Data..: "EndTime")
            Prelude.<*> (x Data..: "StartTime")
            Prelude.<*> (x Data..: "ApplicationId")
      )

instance
  Prelude.Hashable
    ApplicationDateRangeKpiResponse
  where
  hashWithSalt
    _salt
    ApplicationDateRangeKpiResponse' {..} =
      _salt
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` kpiResult
        `Prelude.hashWithSalt` kpiName
        `Prelude.hashWithSalt` endTime
        `Prelude.hashWithSalt` startTime
        `Prelude.hashWithSalt` applicationId

instance
  Prelude.NFData
    ApplicationDateRangeKpiResponse
  where
  rnf ApplicationDateRangeKpiResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf kpiResult
      `Prelude.seq` Prelude.rnf kpiName
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf applicationId

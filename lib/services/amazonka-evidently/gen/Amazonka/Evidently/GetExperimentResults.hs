{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Evidently.GetExperimentResults
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the results of a running or completed experiment. No results
-- are available until there have been 100 events for each variation and at
-- least 10 minutes have passed since the start of the experiment. To
-- increase the statistical power, Evidently performs an additional offline
-- p-value analysis at the end of the experiment. Offline p-value analysis
-- can detect statistical significance in some cases where the anytime
-- p-values used during the experiment do not find statistical
-- significance.
--
-- Experiment results are available up to 63 days after the start of the
-- experiment. They are not available after that because of CloudWatch data
-- retention policies.
module Amazonka.Evidently.GetExperimentResults
  ( -- * Creating a Request
    GetExperimentResults (..),
    newGetExperimentResults,

    -- * Request Lenses
    getExperimentResults_baseStat,
    getExperimentResults_period,
    getExperimentResults_resultStats,
    getExperimentResults_endTime,
    getExperimentResults_reportNames,
    getExperimentResults_startTime,
    getExperimentResults_experiment,
    getExperimentResults_metricNames,
    getExperimentResults_project,
    getExperimentResults_treatmentNames,

    -- * Destructuring the Response
    GetExperimentResultsResponse (..),
    newGetExperimentResultsResponse,

    -- * Response Lenses
    getExperimentResultsResponse_timestamps,
    getExperimentResultsResponse_details,
    getExperimentResultsResponse_reports,
    getExperimentResultsResponse_resultsData,
    getExperimentResultsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetExperimentResults' smart constructor.
data GetExperimentResults = GetExperimentResults'
  { -- | The statistic used to calculate experiment results. Currently the only
    -- valid value is @mean@, which uses the mean of the collected values as
    -- the statistic.
    baseStat :: Prelude.Maybe ExperimentBaseStat,
    -- | In seconds, the amount of time to aggregate results together.
    period :: Prelude.Maybe Prelude.Natural,
    -- | The statistics that you want to see in the returned results.
    --
    -- -   @PValue@ specifies to use p-values for the results. A p-value is
    --     used in hypothesis testing to measure how often you are willing to
    --     make a mistake in rejecting the null hypothesis. A general practice
    --     is to reject the null hypothesis and declare that the results are
    --     statistically significant when the p-value is less than 0.05.
    --
    -- -   @ConfidenceInterval@ specifies a confidence interval for the
    --     results. The confidence interval represents the range of values for
    --     the chosen metric that is likely to contain the true difference
    --     between the @baseStat@ of a variation and the baseline. Evidently
    --     returns the 95% confidence interval.
    --
    -- -   @TreatmentEffect@ is the difference in the statistic specified by
    --     the @baseStat@ parameter between each variation and the default
    --     variation.
    --
    -- -   @BaseStat@ returns the statistical values collected for the metric
    --     for each variation. The statistic uses the same statistic specified
    --     in the @baseStat@ parameter. Therefore, if @baseStat@ is @mean@,
    --     this returns the mean of the values collected for each variation.
    resultStats :: Prelude.Maybe [ExperimentResultRequestType],
    -- | The date and time that the experiment ended, if it is completed. This
    -- must be no longer than 30 days after the experiment start time.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The names of the report types that you want to see. Currently,
    -- @BayesianInference@ is the only valid value.
    reportNames :: Prelude.Maybe [ExperimentReportName],
    -- | The date and time that the experiment started.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the experiment to retrieve the results of.
    experiment :: Prelude.Text,
    -- | The names of the experiment metrics that you want to see the results of.
    metricNames :: Prelude.NonEmpty Prelude.Text,
    -- | The name or ARN of the project that contains the experiment that you
    -- want to see the results of.
    project :: Prelude.Text,
    -- | The names of the experiment treatments that you want to see the results
    -- for.
    treatmentNames :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExperimentResults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseStat', 'getExperimentResults_baseStat' - The statistic used to calculate experiment results. Currently the only
-- valid value is @mean@, which uses the mean of the collected values as
-- the statistic.
--
-- 'period', 'getExperimentResults_period' - In seconds, the amount of time to aggregate results together.
--
-- 'resultStats', 'getExperimentResults_resultStats' - The statistics that you want to see in the returned results.
--
-- -   @PValue@ specifies to use p-values for the results. A p-value is
--     used in hypothesis testing to measure how often you are willing to
--     make a mistake in rejecting the null hypothesis. A general practice
--     is to reject the null hypothesis and declare that the results are
--     statistically significant when the p-value is less than 0.05.
--
-- -   @ConfidenceInterval@ specifies a confidence interval for the
--     results. The confidence interval represents the range of values for
--     the chosen metric that is likely to contain the true difference
--     between the @baseStat@ of a variation and the baseline. Evidently
--     returns the 95% confidence interval.
--
-- -   @TreatmentEffect@ is the difference in the statistic specified by
--     the @baseStat@ parameter between each variation and the default
--     variation.
--
-- -   @BaseStat@ returns the statistical values collected for the metric
--     for each variation. The statistic uses the same statistic specified
--     in the @baseStat@ parameter. Therefore, if @baseStat@ is @mean@,
--     this returns the mean of the values collected for each variation.
--
-- 'endTime', 'getExperimentResults_endTime' - The date and time that the experiment ended, if it is completed. This
-- must be no longer than 30 days after the experiment start time.
--
-- 'reportNames', 'getExperimentResults_reportNames' - The names of the report types that you want to see. Currently,
-- @BayesianInference@ is the only valid value.
--
-- 'startTime', 'getExperimentResults_startTime' - The date and time that the experiment started.
--
-- 'experiment', 'getExperimentResults_experiment' - The name of the experiment to retrieve the results of.
--
-- 'metricNames', 'getExperimentResults_metricNames' - The names of the experiment metrics that you want to see the results of.
--
-- 'project', 'getExperimentResults_project' - The name or ARN of the project that contains the experiment that you
-- want to see the results of.
--
-- 'treatmentNames', 'getExperimentResults_treatmentNames' - The names of the experiment treatments that you want to see the results
-- for.
newGetExperimentResults ::
  -- | 'experiment'
  Prelude.Text ->
  -- | 'metricNames'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'project'
  Prelude.Text ->
  -- | 'treatmentNames'
  Prelude.NonEmpty Prelude.Text ->
  GetExperimentResults
newGetExperimentResults
  pExperiment_
  pMetricNames_
  pProject_
  pTreatmentNames_ =
    GetExperimentResults'
      { baseStat = Prelude.Nothing,
        period = Prelude.Nothing,
        resultStats = Prelude.Nothing,
        endTime = Prelude.Nothing,
        reportNames = Prelude.Nothing,
        startTime = Prelude.Nothing,
        experiment = pExperiment_,
        metricNames = Lens.coerced Lens.# pMetricNames_,
        project = pProject_,
        treatmentNames =
          Lens.coerced Lens.# pTreatmentNames_
      }

-- | The statistic used to calculate experiment results. Currently the only
-- valid value is @mean@, which uses the mean of the collected values as
-- the statistic.
getExperimentResults_baseStat :: Lens.Lens' GetExperimentResults (Prelude.Maybe ExperimentBaseStat)
getExperimentResults_baseStat = Lens.lens (\GetExperimentResults' {baseStat} -> baseStat) (\s@GetExperimentResults' {} a -> s {baseStat = a} :: GetExperimentResults)

-- | In seconds, the amount of time to aggregate results together.
getExperimentResults_period :: Lens.Lens' GetExperimentResults (Prelude.Maybe Prelude.Natural)
getExperimentResults_period = Lens.lens (\GetExperimentResults' {period} -> period) (\s@GetExperimentResults' {} a -> s {period = a} :: GetExperimentResults)

-- | The statistics that you want to see in the returned results.
--
-- -   @PValue@ specifies to use p-values for the results. A p-value is
--     used in hypothesis testing to measure how often you are willing to
--     make a mistake in rejecting the null hypothesis. A general practice
--     is to reject the null hypothesis and declare that the results are
--     statistically significant when the p-value is less than 0.05.
--
-- -   @ConfidenceInterval@ specifies a confidence interval for the
--     results. The confidence interval represents the range of values for
--     the chosen metric that is likely to contain the true difference
--     between the @baseStat@ of a variation and the baseline. Evidently
--     returns the 95% confidence interval.
--
-- -   @TreatmentEffect@ is the difference in the statistic specified by
--     the @baseStat@ parameter between each variation and the default
--     variation.
--
-- -   @BaseStat@ returns the statistical values collected for the metric
--     for each variation. The statistic uses the same statistic specified
--     in the @baseStat@ parameter. Therefore, if @baseStat@ is @mean@,
--     this returns the mean of the values collected for each variation.
getExperimentResults_resultStats :: Lens.Lens' GetExperimentResults (Prelude.Maybe [ExperimentResultRequestType])
getExperimentResults_resultStats = Lens.lens (\GetExperimentResults' {resultStats} -> resultStats) (\s@GetExperimentResults' {} a -> s {resultStats = a} :: GetExperimentResults) Prelude.. Lens.mapping Lens.coerced

-- | The date and time that the experiment ended, if it is completed. This
-- must be no longer than 30 days after the experiment start time.
getExperimentResults_endTime :: Lens.Lens' GetExperimentResults (Prelude.Maybe Prelude.UTCTime)
getExperimentResults_endTime = Lens.lens (\GetExperimentResults' {endTime} -> endTime) (\s@GetExperimentResults' {} a -> s {endTime = a} :: GetExperimentResults) Prelude.. Lens.mapping Data._Time

-- | The names of the report types that you want to see. Currently,
-- @BayesianInference@ is the only valid value.
getExperimentResults_reportNames :: Lens.Lens' GetExperimentResults (Prelude.Maybe [ExperimentReportName])
getExperimentResults_reportNames = Lens.lens (\GetExperimentResults' {reportNames} -> reportNames) (\s@GetExperimentResults' {} a -> s {reportNames = a} :: GetExperimentResults) Prelude.. Lens.mapping Lens.coerced

-- | The date and time that the experiment started.
getExperimentResults_startTime :: Lens.Lens' GetExperimentResults (Prelude.Maybe Prelude.UTCTime)
getExperimentResults_startTime = Lens.lens (\GetExperimentResults' {startTime} -> startTime) (\s@GetExperimentResults' {} a -> s {startTime = a} :: GetExperimentResults) Prelude.. Lens.mapping Data._Time

-- | The name of the experiment to retrieve the results of.
getExperimentResults_experiment :: Lens.Lens' GetExperimentResults Prelude.Text
getExperimentResults_experiment = Lens.lens (\GetExperimentResults' {experiment} -> experiment) (\s@GetExperimentResults' {} a -> s {experiment = a} :: GetExperimentResults)

-- | The names of the experiment metrics that you want to see the results of.
getExperimentResults_metricNames :: Lens.Lens' GetExperimentResults (Prelude.NonEmpty Prelude.Text)
getExperimentResults_metricNames = Lens.lens (\GetExperimentResults' {metricNames} -> metricNames) (\s@GetExperimentResults' {} a -> s {metricNames = a} :: GetExperimentResults) Prelude.. Lens.coerced

-- | The name or ARN of the project that contains the experiment that you
-- want to see the results of.
getExperimentResults_project :: Lens.Lens' GetExperimentResults Prelude.Text
getExperimentResults_project = Lens.lens (\GetExperimentResults' {project} -> project) (\s@GetExperimentResults' {} a -> s {project = a} :: GetExperimentResults)

-- | The names of the experiment treatments that you want to see the results
-- for.
getExperimentResults_treatmentNames :: Lens.Lens' GetExperimentResults (Prelude.NonEmpty Prelude.Text)
getExperimentResults_treatmentNames = Lens.lens (\GetExperimentResults' {treatmentNames} -> treatmentNames) (\s@GetExperimentResults' {} a -> s {treatmentNames = a} :: GetExperimentResults) Prelude.. Lens.coerced

instance Core.AWSRequest GetExperimentResults where
  type
    AWSResponse GetExperimentResults =
      GetExperimentResultsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExperimentResultsResponse'
            Prelude.<$> (x Data..?> "timestamps" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "details")
            Prelude.<*> (x Data..?> "reports" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "resultsData" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetExperimentResults where
  hashWithSalt _salt GetExperimentResults' {..} =
    _salt `Prelude.hashWithSalt` baseStat
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` resultStats
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` reportNames
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` experiment
      `Prelude.hashWithSalt` metricNames
      `Prelude.hashWithSalt` project
      `Prelude.hashWithSalt` treatmentNames

instance Prelude.NFData GetExperimentResults where
  rnf GetExperimentResults' {..} =
    Prelude.rnf baseStat
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf resultStats
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf reportNames
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf experiment
      `Prelude.seq` Prelude.rnf metricNames
      `Prelude.seq` Prelude.rnf project
      `Prelude.seq` Prelude.rnf treatmentNames

instance Data.ToHeaders GetExperimentResults where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetExperimentResults where
  toJSON GetExperimentResults' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("baseStat" Data..=) Prelude.<$> baseStat,
            ("period" Data..=) Prelude.<$> period,
            ("resultStats" Data..=) Prelude.<$> resultStats,
            ("endTime" Data..=) Prelude.<$> endTime,
            ("reportNames" Data..=) Prelude.<$> reportNames,
            ("startTime" Data..=) Prelude.<$> startTime,
            Prelude.Just ("metricNames" Data..= metricNames),
            Prelude.Just
              ("treatmentNames" Data..= treatmentNames)
          ]
      )

instance Data.ToPath GetExperimentResults where
  toPath GetExperimentResults' {..} =
    Prelude.mconcat
      [ "/projects/",
        Data.toBS project,
        "/experiments/",
        Data.toBS experiment,
        "/results"
      ]

instance Data.ToQuery GetExperimentResults where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetExperimentResultsResponse' smart constructor.
data GetExperimentResultsResponse = GetExperimentResultsResponse'
  { -- | The timestamps of each result returned.
    timestamps :: Prelude.Maybe [Data.POSIX],
    -- | If the experiment doesn\'t yet have enough events to provide valid
    -- results, this field is returned with the message
    -- @Not enough events to generate results@. If there are enough events to
    -- provide valid results, this field is not returned.
    details :: Prelude.Maybe Prelude.Text,
    -- | An array of structures that include the reports that you requested.
    reports :: Prelude.Maybe [ExperimentReport],
    -- | An array of structures that include experiment results including metric
    -- names and values.
    resultsData :: Prelude.Maybe [ExperimentResultsData],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExperimentResultsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestamps', 'getExperimentResultsResponse_timestamps' - The timestamps of each result returned.
--
-- 'details', 'getExperimentResultsResponse_details' - If the experiment doesn\'t yet have enough events to provide valid
-- results, this field is returned with the message
-- @Not enough events to generate results@. If there are enough events to
-- provide valid results, this field is not returned.
--
-- 'reports', 'getExperimentResultsResponse_reports' - An array of structures that include the reports that you requested.
--
-- 'resultsData', 'getExperimentResultsResponse_resultsData' - An array of structures that include experiment results including metric
-- names and values.
--
-- 'httpStatus', 'getExperimentResultsResponse_httpStatus' - The response's http status code.
newGetExperimentResultsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetExperimentResultsResponse
newGetExperimentResultsResponse pHttpStatus_ =
  GetExperimentResultsResponse'
    { timestamps =
        Prelude.Nothing,
      details = Prelude.Nothing,
      reports = Prelude.Nothing,
      resultsData = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The timestamps of each result returned.
getExperimentResultsResponse_timestamps :: Lens.Lens' GetExperimentResultsResponse (Prelude.Maybe [Prelude.UTCTime])
getExperimentResultsResponse_timestamps = Lens.lens (\GetExperimentResultsResponse' {timestamps} -> timestamps) (\s@GetExperimentResultsResponse' {} a -> s {timestamps = a} :: GetExperimentResultsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the experiment doesn\'t yet have enough events to provide valid
-- results, this field is returned with the message
-- @Not enough events to generate results@. If there are enough events to
-- provide valid results, this field is not returned.
getExperimentResultsResponse_details :: Lens.Lens' GetExperimentResultsResponse (Prelude.Maybe Prelude.Text)
getExperimentResultsResponse_details = Lens.lens (\GetExperimentResultsResponse' {details} -> details) (\s@GetExperimentResultsResponse' {} a -> s {details = a} :: GetExperimentResultsResponse)

-- | An array of structures that include the reports that you requested.
getExperimentResultsResponse_reports :: Lens.Lens' GetExperimentResultsResponse (Prelude.Maybe [ExperimentReport])
getExperimentResultsResponse_reports = Lens.lens (\GetExperimentResultsResponse' {reports} -> reports) (\s@GetExperimentResultsResponse' {} a -> s {reports = a} :: GetExperimentResultsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array of structures that include experiment results including metric
-- names and values.
getExperimentResultsResponse_resultsData :: Lens.Lens' GetExperimentResultsResponse (Prelude.Maybe [ExperimentResultsData])
getExperimentResultsResponse_resultsData = Lens.lens (\GetExperimentResultsResponse' {resultsData} -> resultsData) (\s@GetExperimentResultsResponse' {} a -> s {resultsData = a} :: GetExperimentResultsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getExperimentResultsResponse_httpStatus :: Lens.Lens' GetExperimentResultsResponse Prelude.Int
getExperimentResultsResponse_httpStatus = Lens.lens (\GetExperimentResultsResponse' {httpStatus} -> httpStatus) (\s@GetExperimentResultsResponse' {} a -> s {httpStatus = a} :: GetExperimentResultsResponse)

instance Prelude.NFData GetExperimentResultsResponse where
  rnf GetExperimentResultsResponse' {..} =
    Prelude.rnf timestamps
      `Prelude.seq` Prelude.rnf details
      `Prelude.seq` Prelude.rnf reports
      `Prelude.seq` Prelude.rnf resultsData
      `Prelude.seq` Prelude.rnf httpStatus

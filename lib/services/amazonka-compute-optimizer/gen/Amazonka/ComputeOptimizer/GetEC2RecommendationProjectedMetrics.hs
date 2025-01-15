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
-- Module      : Amazonka.ComputeOptimizer.GetEC2RecommendationProjectedMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the projected utilization metrics of Amazon EC2 instance
-- recommendations.
--
-- The @Cpu@ and @Memory@ metrics are the only projected utilization
-- metrics returned when you run this action. Additionally, the @Memory@
-- metric is returned only for resources that have the unified CloudWatch
-- agent installed on them. For more information, see
-- <https://docs.aws.amazon.com/compute-optimizer/latest/ug/metrics.html#cw-agent Enabling Memory Utilization with the CloudWatch Agent>.
module Amazonka.ComputeOptimizer.GetEC2RecommendationProjectedMetrics
  ( -- * Creating a Request
    GetEC2RecommendationProjectedMetrics (..),
    newGetEC2RecommendationProjectedMetrics,

    -- * Request Lenses
    getEC2RecommendationProjectedMetrics_recommendationPreferences,
    getEC2RecommendationProjectedMetrics_instanceArn,
    getEC2RecommendationProjectedMetrics_stat,
    getEC2RecommendationProjectedMetrics_period,
    getEC2RecommendationProjectedMetrics_startTime,
    getEC2RecommendationProjectedMetrics_endTime,

    -- * Destructuring the Response
    GetEC2RecommendationProjectedMetricsResponse (..),
    newGetEC2RecommendationProjectedMetricsResponse,

    -- * Response Lenses
    getEC2RecommendationProjectedMetricsResponse_recommendedOptionProjectedMetrics,
    getEC2RecommendationProjectedMetricsResponse_httpStatus,
  )
where

import Amazonka.ComputeOptimizer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEC2RecommendationProjectedMetrics' smart constructor.
data GetEC2RecommendationProjectedMetrics = GetEC2RecommendationProjectedMetrics'
  { -- | An object to specify the preferences for the Amazon EC2 recommendation
    -- projected metrics to return in the response.
    recommendationPreferences :: Prelude.Maybe RecommendationPreferences,
    -- | The Amazon Resource Name (ARN) of the instances for which to return
    -- recommendation projected metrics.
    instanceArn :: Prelude.Text,
    -- | The statistic of the projected metrics.
    stat :: MetricStatistic,
    -- | The granularity, in seconds, of the projected metrics data points.
    period :: Prelude.Int,
    -- | The timestamp of the first projected metrics data point to return.
    startTime :: Data.POSIX,
    -- | The timestamp of the last projected metrics data point to return.
    endTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEC2RecommendationProjectedMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommendationPreferences', 'getEC2RecommendationProjectedMetrics_recommendationPreferences' - An object to specify the preferences for the Amazon EC2 recommendation
-- projected metrics to return in the response.
--
-- 'instanceArn', 'getEC2RecommendationProjectedMetrics_instanceArn' - The Amazon Resource Name (ARN) of the instances for which to return
-- recommendation projected metrics.
--
-- 'stat', 'getEC2RecommendationProjectedMetrics_stat' - The statistic of the projected metrics.
--
-- 'period', 'getEC2RecommendationProjectedMetrics_period' - The granularity, in seconds, of the projected metrics data points.
--
-- 'startTime', 'getEC2RecommendationProjectedMetrics_startTime' - The timestamp of the first projected metrics data point to return.
--
-- 'endTime', 'getEC2RecommendationProjectedMetrics_endTime' - The timestamp of the last projected metrics data point to return.
newGetEC2RecommendationProjectedMetrics ::
  -- | 'instanceArn'
  Prelude.Text ->
  -- | 'stat'
  MetricStatistic ->
  -- | 'period'
  Prelude.Int ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  GetEC2RecommendationProjectedMetrics
newGetEC2RecommendationProjectedMetrics
  pInstanceArn_
  pStat_
  pPeriod_
  pStartTime_
  pEndTime_ =
    GetEC2RecommendationProjectedMetrics'
      { recommendationPreferences =
          Prelude.Nothing,
        instanceArn = pInstanceArn_,
        stat = pStat_,
        period = pPeriod_,
        startTime =
          Data._Time Lens.# pStartTime_,
        endTime = Data._Time Lens.# pEndTime_
      }

-- | An object to specify the preferences for the Amazon EC2 recommendation
-- projected metrics to return in the response.
getEC2RecommendationProjectedMetrics_recommendationPreferences :: Lens.Lens' GetEC2RecommendationProjectedMetrics (Prelude.Maybe RecommendationPreferences)
getEC2RecommendationProjectedMetrics_recommendationPreferences = Lens.lens (\GetEC2RecommendationProjectedMetrics' {recommendationPreferences} -> recommendationPreferences) (\s@GetEC2RecommendationProjectedMetrics' {} a -> s {recommendationPreferences = a} :: GetEC2RecommendationProjectedMetrics)

-- | The Amazon Resource Name (ARN) of the instances for which to return
-- recommendation projected metrics.
getEC2RecommendationProjectedMetrics_instanceArn :: Lens.Lens' GetEC2RecommendationProjectedMetrics Prelude.Text
getEC2RecommendationProjectedMetrics_instanceArn = Lens.lens (\GetEC2RecommendationProjectedMetrics' {instanceArn} -> instanceArn) (\s@GetEC2RecommendationProjectedMetrics' {} a -> s {instanceArn = a} :: GetEC2RecommendationProjectedMetrics)

-- | The statistic of the projected metrics.
getEC2RecommendationProjectedMetrics_stat :: Lens.Lens' GetEC2RecommendationProjectedMetrics MetricStatistic
getEC2RecommendationProjectedMetrics_stat = Lens.lens (\GetEC2RecommendationProjectedMetrics' {stat} -> stat) (\s@GetEC2RecommendationProjectedMetrics' {} a -> s {stat = a} :: GetEC2RecommendationProjectedMetrics)

-- | The granularity, in seconds, of the projected metrics data points.
getEC2RecommendationProjectedMetrics_period :: Lens.Lens' GetEC2RecommendationProjectedMetrics Prelude.Int
getEC2RecommendationProjectedMetrics_period = Lens.lens (\GetEC2RecommendationProjectedMetrics' {period} -> period) (\s@GetEC2RecommendationProjectedMetrics' {} a -> s {period = a} :: GetEC2RecommendationProjectedMetrics)

-- | The timestamp of the first projected metrics data point to return.
getEC2RecommendationProjectedMetrics_startTime :: Lens.Lens' GetEC2RecommendationProjectedMetrics Prelude.UTCTime
getEC2RecommendationProjectedMetrics_startTime = Lens.lens (\GetEC2RecommendationProjectedMetrics' {startTime} -> startTime) (\s@GetEC2RecommendationProjectedMetrics' {} a -> s {startTime = a} :: GetEC2RecommendationProjectedMetrics) Prelude.. Data._Time

-- | The timestamp of the last projected metrics data point to return.
getEC2RecommendationProjectedMetrics_endTime :: Lens.Lens' GetEC2RecommendationProjectedMetrics Prelude.UTCTime
getEC2RecommendationProjectedMetrics_endTime = Lens.lens (\GetEC2RecommendationProjectedMetrics' {endTime} -> endTime) (\s@GetEC2RecommendationProjectedMetrics' {} a -> s {endTime = a} :: GetEC2RecommendationProjectedMetrics) Prelude.. Data._Time

instance
  Core.AWSRequest
    GetEC2RecommendationProjectedMetrics
  where
  type
    AWSResponse GetEC2RecommendationProjectedMetrics =
      GetEC2RecommendationProjectedMetricsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEC2RecommendationProjectedMetricsResponse'
            Prelude.<$> ( x
                            Data..?> "recommendedOptionProjectedMetrics"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetEC2RecommendationProjectedMetrics
  where
  hashWithSalt
    _salt
    GetEC2RecommendationProjectedMetrics' {..} =
      _salt
        `Prelude.hashWithSalt` recommendationPreferences
        `Prelude.hashWithSalt` instanceArn
        `Prelude.hashWithSalt` stat
        `Prelude.hashWithSalt` period
        `Prelude.hashWithSalt` startTime
        `Prelude.hashWithSalt` endTime

instance
  Prelude.NFData
    GetEC2RecommendationProjectedMetrics
  where
  rnf GetEC2RecommendationProjectedMetrics' {..} =
    Prelude.rnf recommendationPreferences `Prelude.seq`
      Prelude.rnf instanceArn `Prelude.seq`
        Prelude.rnf stat `Prelude.seq`
          Prelude.rnf period `Prelude.seq`
            Prelude.rnf startTime `Prelude.seq`
              Prelude.rnf endTime

instance
  Data.ToHeaders
    GetEC2RecommendationProjectedMetrics
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComputeOptimizerService.GetEC2RecommendationProjectedMetrics" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    GetEC2RecommendationProjectedMetrics
  where
  toJSON GetEC2RecommendationProjectedMetrics' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("recommendationPreferences" Data..=)
              Prelude.<$> recommendationPreferences,
            Prelude.Just ("instanceArn" Data..= instanceArn),
            Prelude.Just ("stat" Data..= stat),
            Prelude.Just ("period" Data..= period),
            Prelude.Just ("startTime" Data..= startTime),
            Prelude.Just ("endTime" Data..= endTime)
          ]
      )

instance
  Data.ToPath
    GetEC2RecommendationProjectedMetrics
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetEC2RecommendationProjectedMetrics
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEC2RecommendationProjectedMetricsResponse' smart constructor.
data GetEC2RecommendationProjectedMetricsResponse = GetEC2RecommendationProjectedMetricsResponse'
  { -- | An array of objects that describes projected metrics.
    recommendedOptionProjectedMetrics :: Prelude.Maybe [RecommendedOptionProjectedMetric],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEC2RecommendationProjectedMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommendedOptionProjectedMetrics', 'getEC2RecommendationProjectedMetricsResponse_recommendedOptionProjectedMetrics' - An array of objects that describes projected metrics.
--
-- 'httpStatus', 'getEC2RecommendationProjectedMetricsResponse_httpStatus' - The response's http status code.
newGetEC2RecommendationProjectedMetricsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEC2RecommendationProjectedMetricsResponse
newGetEC2RecommendationProjectedMetricsResponse
  pHttpStatus_ =
    GetEC2RecommendationProjectedMetricsResponse'
      { recommendedOptionProjectedMetrics =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of objects that describes projected metrics.
getEC2RecommendationProjectedMetricsResponse_recommendedOptionProjectedMetrics :: Lens.Lens' GetEC2RecommendationProjectedMetricsResponse (Prelude.Maybe [RecommendedOptionProjectedMetric])
getEC2RecommendationProjectedMetricsResponse_recommendedOptionProjectedMetrics = Lens.lens (\GetEC2RecommendationProjectedMetricsResponse' {recommendedOptionProjectedMetrics} -> recommendedOptionProjectedMetrics) (\s@GetEC2RecommendationProjectedMetricsResponse' {} a -> s {recommendedOptionProjectedMetrics = a} :: GetEC2RecommendationProjectedMetricsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getEC2RecommendationProjectedMetricsResponse_httpStatus :: Lens.Lens' GetEC2RecommendationProjectedMetricsResponse Prelude.Int
getEC2RecommendationProjectedMetricsResponse_httpStatus = Lens.lens (\GetEC2RecommendationProjectedMetricsResponse' {httpStatus} -> httpStatus) (\s@GetEC2RecommendationProjectedMetricsResponse' {} a -> s {httpStatus = a} :: GetEC2RecommendationProjectedMetricsResponse)

instance
  Prelude.NFData
    GetEC2RecommendationProjectedMetricsResponse
  where
  rnf GetEC2RecommendationProjectedMetricsResponse' {..} =
    Prelude.rnf recommendedOptionProjectedMetrics `Prelude.seq`
      Prelude.rnf httpStatus

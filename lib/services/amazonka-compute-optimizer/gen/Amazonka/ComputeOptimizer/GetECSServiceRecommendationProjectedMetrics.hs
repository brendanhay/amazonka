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
-- Module      : Amazonka.ComputeOptimizer.GetECSServiceRecommendationProjectedMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the projected metrics of Amazon ECS service recommendations.
module Amazonka.ComputeOptimizer.GetECSServiceRecommendationProjectedMetrics
  ( -- * Creating a Request
    GetECSServiceRecommendationProjectedMetrics (..),
    newGetECSServiceRecommendationProjectedMetrics,

    -- * Request Lenses
    getECSServiceRecommendationProjectedMetrics_serviceArn,
    getECSServiceRecommendationProjectedMetrics_stat,
    getECSServiceRecommendationProjectedMetrics_period,
    getECSServiceRecommendationProjectedMetrics_startTime,
    getECSServiceRecommendationProjectedMetrics_endTime,

    -- * Destructuring the Response
    GetECSServiceRecommendationProjectedMetricsResponse (..),
    newGetECSServiceRecommendationProjectedMetricsResponse,

    -- * Response Lenses
    getECSServiceRecommendationProjectedMetricsResponse_recommendedOptionProjectedMetrics,
    getECSServiceRecommendationProjectedMetricsResponse_httpStatus,
  )
where

import Amazonka.ComputeOptimizer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetECSServiceRecommendationProjectedMetrics' smart constructor.
data GetECSServiceRecommendationProjectedMetrics = GetECSServiceRecommendationProjectedMetrics'
  { -- | The ARN that identifies the ECS service.
    --
    -- The following is the format of the ARN:
    --
    -- @arn:aws:ecs:region:aws_account_id:service\/cluster-name\/service-name@
    serviceArn :: Prelude.Text,
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
-- Create a value of 'GetECSServiceRecommendationProjectedMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceArn', 'getECSServiceRecommendationProjectedMetrics_serviceArn' - The ARN that identifies the ECS service.
--
-- The following is the format of the ARN:
--
-- @arn:aws:ecs:region:aws_account_id:service\/cluster-name\/service-name@
--
-- 'stat', 'getECSServiceRecommendationProjectedMetrics_stat' - The statistic of the projected metrics.
--
-- 'period', 'getECSServiceRecommendationProjectedMetrics_period' - The granularity, in seconds, of the projected metrics data points.
--
-- 'startTime', 'getECSServiceRecommendationProjectedMetrics_startTime' - The timestamp of the first projected metrics data point to return.
--
-- 'endTime', 'getECSServiceRecommendationProjectedMetrics_endTime' - The timestamp of the last projected metrics data point to return.
newGetECSServiceRecommendationProjectedMetrics ::
  -- | 'serviceArn'
  Prelude.Text ->
  -- | 'stat'
  MetricStatistic ->
  -- | 'period'
  Prelude.Int ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  GetECSServiceRecommendationProjectedMetrics
newGetECSServiceRecommendationProjectedMetrics
  pServiceArn_
  pStat_
  pPeriod_
  pStartTime_
  pEndTime_ =
    GetECSServiceRecommendationProjectedMetrics'
      { serviceArn =
          pServiceArn_,
        stat = pStat_,
        period = pPeriod_,
        startTime =
          Data._Time
            Lens.# pStartTime_,
        endTime =
          Data._Time Lens.# pEndTime_
      }

-- | The ARN that identifies the ECS service.
--
-- The following is the format of the ARN:
--
-- @arn:aws:ecs:region:aws_account_id:service\/cluster-name\/service-name@
getECSServiceRecommendationProjectedMetrics_serviceArn :: Lens.Lens' GetECSServiceRecommendationProjectedMetrics Prelude.Text
getECSServiceRecommendationProjectedMetrics_serviceArn = Lens.lens (\GetECSServiceRecommendationProjectedMetrics' {serviceArn} -> serviceArn) (\s@GetECSServiceRecommendationProjectedMetrics' {} a -> s {serviceArn = a} :: GetECSServiceRecommendationProjectedMetrics)

-- | The statistic of the projected metrics.
getECSServiceRecommendationProjectedMetrics_stat :: Lens.Lens' GetECSServiceRecommendationProjectedMetrics MetricStatistic
getECSServiceRecommendationProjectedMetrics_stat = Lens.lens (\GetECSServiceRecommendationProjectedMetrics' {stat} -> stat) (\s@GetECSServiceRecommendationProjectedMetrics' {} a -> s {stat = a} :: GetECSServiceRecommendationProjectedMetrics)

-- | The granularity, in seconds, of the projected metrics data points.
getECSServiceRecommendationProjectedMetrics_period :: Lens.Lens' GetECSServiceRecommendationProjectedMetrics Prelude.Int
getECSServiceRecommendationProjectedMetrics_period = Lens.lens (\GetECSServiceRecommendationProjectedMetrics' {period} -> period) (\s@GetECSServiceRecommendationProjectedMetrics' {} a -> s {period = a} :: GetECSServiceRecommendationProjectedMetrics)

-- | The timestamp of the first projected metrics data point to return.
getECSServiceRecommendationProjectedMetrics_startTime :: Lens.Lens' GetECSServiceRecommendationProjectedMetrics Prelude.UTCTime
getECSServiceRecommendationProjectedMetrics_startTime = Lens.lens (\GetECSServiceRecommendationProjectedMetrics' {startTime} -> startTime) (\s@GetECSServiceRecommendationProjectedMetrics' {} a -> s {startTime = a} :: GetECSServiceRecommendationProjectedMetrics) Prelude.. Data._Time

-- | The timestamp of the last projected metrics data point to return.
getECSServiceRecommendationProjectedMetrics_endTime :: Lens.Lens' GetECSServiceRecommendationProjectedMetrics Prelude.UTCTime
getECSServiceRecommendationProjectedMetrics_endTime = Lens.lens (\GetECSServiceRecommendationProjectedMetrics' {endTime} -> endTime) (\s@GetECSServiceRecommendationProjectedMetrics' {} a -> s {endTime = a} :: GetECSServiceRecommendationProjectedMetrics) Prelude.. Data._Time

instance
  Core.AWSRequest
    GetECSServiceRecommendationProjectedMetrics
  where
  type
    AWSResponse
      GetECSServiceRecommendationProjectedMetrics =
      GetECSServiceRecommendationProjectedMetricsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetECSServiceRecommendationProjectedMetricsResponse'
            Prelude.<$> ( x
                            Data..?> "recommendedOptionProjectedMetrics"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetECSServiceRecommendationProjectedMetrics
  where
  hashWithSalt
    _salt
    GetECSServiceRecommendationProjectedMetrics' {..} =
      _salt
        `Prelude.hashWithSalt` serviceArn
        `Prelude.hashWithSalt` stat
        `Prelude.hashWithSalt` period
        `Prelude.hashWithSalt` startTime
        `Prelude.hashWithSalt` endTime

instance
  Prelude.NFData
    GetECSServiceRecommendationProjectedMetrics
  where
  rnf GetECSServiceRecommendationProjectedMetrics' {..} =
    Prelude.rnf serviceArn
      `Prelude.seq` Prelude.rnf stat
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime

instance
  Data.ToHeaders
    GetECSServiceRecommendationProjectedMetrics
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ComputeOptimizerService.GetECSServiceRecommendationProjectedMetrics" ::
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
    GetECSServiceRecommendationProjectedMetrics
  where
  toJSON
    GetECSServiceRecommendationProjectedMetrics' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just ("serviceArn" Data..= serviceArn),
              Prelude.Just ("stat" Data..= stat),
              Prelude.Just ("period" Data..= period),
              Prelude.Just ("startTime" Data..= startTime),
              Prelude.Just ("endTime" Data..= endTime)
            ]
        )

instance
  Data.ToPath
    GetECSServiceRecommendationProjectedMetrics
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetECSServiceRecommendationProjectedMetrics
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetECSServiceRecommendationProjectedMetricsResponse' smart constructor.
data GetECSServiceRecommendationProjectedMetricsResponse = GetECSServiceRecommendationProjectedMetricsResponse'
  { -- | An array of objects that describes the projected metrics.
    recommendedOptionProjectedMetrics :: Prelude.Maybe [ECSServiceRecommendedOptionProjectedMetric],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetECSServiceRecommendationProjectedMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommendedOptionProjectedMetrics', 'getECSServiceRecommendationProjectedMetricsResponse_recommendedOptionProjectedMetrics' - An array of objects that describes the projected metrics.
--
-- 'httpStatus', 'getECSServiceRecommendationProjectedMetricsResponse_httpStatus' - The response's http status code.
newGetECSServiceRecommendationProjectedMetricsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetECSServiceRecommendationProjectedMetricsResponse
newGetECSServiceRecommendationProjectedMetricsResponse
  pHttpStatus_ =
    GetECSServiceRecommendationProjectedMetricsResponse'
      { recommendedOptionProjectedMetrics =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | An array of objects that describes the projected metrics.
getECSServiceRecommendationProjectedMetricsResponse_recommendedOptionProjectedMetrics :: Lens.Lens' GetECSServiceRecommendationProjectedMetricsResponse (Prelude.Maybe [ECSServiceRecommendedOptionProjectedMetric])
getECSServiceRecommendationProjectedMetricsResponse_recommendedOptionProjectedMetrics = Lens.lens (\GetECSServiceRecommendationProjectedMetricsResponse' {recommendedOptionProjectedMetrics} -> recommendedOptionProjectedMetrics) (\s@GetECSServiceRecommendationProjectedMetricsResponse' {} a -> s {recommendedOptionProjectedMetrics = a} :: GetECSServiceRecommendationProjectedMetricsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getECSServiceRecommendationProjectedMetricsResponse_httpStatus :: Lens.Lens' GetECSServiceRecommendationProjectedMetricsResponse Prelude.Int
getECSServiceRecommendationProjectedMetricsResponse_httpStatus = Lens.lens (\GetECSServiceRecommendationProjectedMetricsResponse' {httpStatus} -> httpStatus) (\s@GetECSServiceRecommendationProjectedMetricsResponse' {} a -> s {httpStatus = a} :: GetECSServiceRecommendationProjectedMetricsResponse)

instance
  Prelude.NFData
    GetECSServiceRecommendationProjectedMetricsResponse
  where
  rnf
    GetECSServiceRecommendationProjectedMetricsResponse' {..} =
      Prelude.rnf recommendedOptionProjectedMetrics
        `Prelude.seq` Prelude.rnf httpStatus

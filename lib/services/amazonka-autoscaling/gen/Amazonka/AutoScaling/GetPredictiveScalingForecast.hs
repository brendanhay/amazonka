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
-- Module      : Amazonka.AutoScaling.GetPredictiveScalingForecast
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the forecast data for a predictive scaling policy.
--
-- Load forecasts are predictions of the hourly load values using
-- historical load data from CloudWatch and an analysis of historical
-- trends. Capacity forecasts are represented as predicted values for the
-- minimum capacity that is needed on an hourly basis, based on the hourly
-- load forecast.
--
-- A minimum of 24 hours of data is required to create the initial
-- forecasts. However, having a full 14 days of historical data results in
-- more accurate forecasts.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-predictive-scaling.html Predictive scaling for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Amazonka.AutoScaling.GetPredictiveScalingForecast
  ( -- * Creating a Request
    GetPredictiveScalingForecast (..),
    newGetPredictiveScalingForecast,

    -- * Request Lenses
    getPredictiveScalingForecast_autoScalingGroupName,
    getPredictiveScalingForecast_policyName,
    getPredictiveScalingForecast_startTime,
    getPredictiveScalingForecast_endTime,

    -- * Destructuring the Response
    GetPredictiveScalingForecastResponse (..),
    newGetPredictiveScalingForecastResponse,

    -- * Response Lenses
    getPredictiveScalingForecastResponse_httpStatus,
    getPredictiveScalingForecastResponse_loadForecast,
    getPredictiveScalingForecastResponse_capacityForecast,
    getPredictiveScalingForecastResponse_updateTime,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPredictiveScalingForecast' smart constructor.
data GetPredictiveScalingForecast = GetPredictiveScalingForecast'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | The name of the policy.
    policyName :: Prelude.Text,
    -- | The inclusive start time of the time range for the forecast data to get.
    -- At most, the date and time can be one year before the current date and
    -- time.
    startTime :: Core.ISO8601,
    -- | The exclusive end time of the time range for the forecast data to get.
    -- The maximum time duration between the start and end time is 30 days.
    --
    -- Although this parameter can accept a date and time that is more than two
    -- days in the future, the availability of forecast data has limits. Amazon
    -- EC2 Auto Scaling only issues forecasts for periods of two days in
    -- advance.
    endTime :: Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPredictiveScalingForecast' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroupName', 'getPredictiveScalingForecast_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'policyName', 'getPredictiveScalingForecast_policyName' - The name of the policy.
--
-- 'startTime', 'getPredictiveScalingForecast_startTime' - The inclusive start time of the time range for the forecast data to get.
-- At most, the date and time can be one year before the current date and
-- time.
--
-- 'endTime', 'getPredictiveScalingForecast_endTime' - The exclusive end time of the time range for the forecast data to get.
-- The maximum time duration between the start and end time is 30 days.
--
-- Although this parameter can accept a date and time that is more than two
-- days in the future, the availability of forecast data has limits. Amazon
-- EC2 Auto Scaling only issues forecasts for periods of two days in
-- advance.
newGetPredictiveScalingForecast ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  GetPredictiveScalingForecast
newGetPredictiveScalingForecast
  pAutoScalingGroupName_
  pPolicyName_
  pStartTime_
  pEndTime_ =
    GetPredictiveScalingForecast'
      { autoScalingGroupName =
          pAutoScalingGroupName_,
        policyName = pPolicyName_,
        startTime = Core._Time Lens.# pStartTime_,
        endTime = Core._Time Lens.# pEndTime_
      }

-- | The name of the Auto Scaling group.
getPredictiveScalingForecast_autoScalingGroupName :: Lens.Lens' GetPredictiveScalingForecast Prelude.Text
getPredictiveScalingForecast_autoScalingGroupName = Lens.lens (\GetPredictiveScalingForecast' {autoScalingGroupName} -> autoScalingGroupName) (\s@GetPredictiveScalingForecast' {} a -> s {autoScalingGroupName = a} :: GetPredictiveScalingForecast)

-- | The name of the policy.
getPredictiveScalingForecast_policyName :: Lens.Lens' GetPredictiveScalingForecast Prelude.Text
getPredictiveScalingForecast_policyName = Lens.lens (\GetPredictiveScalingForecast' {policyName} -> policyName) (\s@GetPredictiveScalingForecast' {} a -> s {policyName = a} :: GetPredictiveScalingForecast)

-- | The inclusive start time of the time range for the forecast data to get.
-- At most, the date and time can be one year before the current date and
-- time.
getPredictiveScalingForecast_startTime :: Lens.Lens' GetPredictiveScalingForecast Prelude.UTCTime
getPredictiveScalingForecast_startTime = Lens.lens (\GetPredictiveScalingForecast' {startTime} -> startTime) (\s@GetPredictiveScalingForecast' {} a -> s {startTime = a} :: GetPredictiveScalingForecast) Prelude.. Core._Time

-- | The exclusive end time of the time range for the forecast data to get.
-- The maximum time duration between the start and end time is 30 days.
--
-- Although this parameter can accept a date and time that is more than two
-- days in the future, the availability of forecast data has limits. Amazon
-- EC2 Auto Scaling only issues forecasts for periods of two days in
-- advance.
getPredictiveScalingForecast_endTime :: Lens.Lens' GetPredictiveScalingForecast Prelude.UTCTime
getPredictiveScalingForecast_endTime = Lens.lens (\GetPredictiveScalingForecast' {endTime} -> endTime) (\s@GetPredictiveScalingForecast' {} a -> s {endTime = a} :: GetPredictiveScalingForecast) Prelude.. Core._Time

instance Core.AWSRequest GetPredictiveScalingForecast where
  type
    AWSResponse GetPredictiveScalingForecast =
      GetPredictiveScalingForecastResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetPredictiveScalingForecastResult"
      ( \s h x ->
          GetPredictiveScalingForecastResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "LoadForecast" Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "member"
                        )
            Prelude.<*> (x Core..@ "CapacityForecast")
            Prelude.<*> (x Core..@ "UpdateTime")
      )

instance
  Prelude.Hashable
    GetPredictiveScalingForecast
  where
  hashWithSalt _salt GetPredictiveScalingForecast' {..} =
    _salt `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData GetPredictiveScalingForecast where
  rnf GetPredictiveScalingForecast' {..} =
    Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime

instance Core.ToHeaders GetPredictiveScalingForecast where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetPredictiveScalingForecast where
  toPath = Prelude.const "/"

instance Core.ToQuery GetPredictiveScalingForecast where
  toQuery GetPredictiveScalingForecast' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "GetPredictiveScalingForecast" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2011-01-01" :: Prelude.ByteString),
        "AutoScalingGroupName" Core.=: autoScalingGroupName,
        "PolicyName" Core.=: policyName,
        "StartTime" Core.=: startTime,
        "EndTime" Core.=: endTime
      ]

-- | /See:/ 'newGetPredictiveScalingForecastResponse' smart constructor.
data GetPredictiveScalingForecastResponse = GetPredictiveScalingForecastResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The load forecast.
    loadForecast :: [LoadForecast],
    -- | The capacity forecast.
    capacityForecast :: CapacityForecast,
    -- | The time the forecast was made.
    updateTime :: Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPredictiveScalingForecastResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getPredictiveScalingForecastResponse_httpStatus' - The response's http status code.
--
-- 'loadForecast', 'getPredictiveScalingForecastResponse_loadForecast' - The load forecast.
--
-- 'capacityForecast', 'getPredictiveScalingForecastResponse_capacityForecast' - The capacity forecast.
--
-- 'updateTime', 'getPredictiveScalingForecastResponse_updateTime' - The time the forecast was made.
newGetPredictiveScalingForecastResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'capacityForecast'
  CapacityForecast ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  GetPredictiveScalingForecastResponse
newGetPredictiveScalingForecastResponse
  pHttpStatus_
  pCapacityForecast_
  pUpdateTime_ =
    GetPredictiveScalingForecastResponse'
      { httpStatus =
          pHttpStatus_,
        loadForecast = Prelude.mempty,
        capacityForecast = pCapacityForecast_,
        updateTime =
          Core._Time Lens.# pUpdateTime_
      }

-- | The response's http status code.
getPredictiveScalingForecastResponse_httpStatus :: Lens.Lens' GetPredictiveScalingForecastResponse Prelude.Int
getPredictiveScalingForecastResponse_httpStatus = Lens.lens (\GetPredictiveScalingForecastResponse' {httpStatus} -> httpStatus) (\s@GetPredictiveScalingForecastResponse' {} a -> s {httpStatus = a} :: GetPredictiveScalingForecastResponse)

-- | The load forecast.
getPredictiveScalingForecastResponse_loadForecast :: Lens.Lens' GetPredictiveScalingForecastResponse [LoadForecast]
getPredictiveScalingForecastResponse_loadForecast = Lens.lens (\GetPredictiveScalingForecastResponse' {loadForecast} -> loadForecast) (\s@GetPredictiveScalingForecastResponse' {} a -> s {loadForecast = a} :: GetPredictiveScalingForecastResponse) Prelude.. Lens.coerced

-- | The capacity forecast.
getPredictiveScalingForecastResponse_capacityForecast :: Lens.Lens' GetPredictiveScalingForecastResponse CapacityForecast
getPredictiveScalingForecastResponse_capacityForecast = Lens.lens (\GetPredictiveScalingForecastResponse' {capacityForecast} -> capacityForecast) (\s@GetPredictiveScalingForecastResponse' {} a -> s {capacityForecast = a} :: GetPredictiveScalingForecastResponse)

-- | The time the forecast was made.
getPredictiveScalingForecastResponse_updateTime :: Lens.Lens' GetPredictiveScalingForecastResponse Prelude.UTCTime
getPredictiveScalingForecastResponse_updateTime = Lens.lens (\GetPredictiveScalingForecastResponse' {updateTime} -> updateTime) (\s@GetPredictiveScalingForecastResponse' {} a -> s {updateTime = a} :: GetPredictiveScalingForecastResponse) Prelude.. Core._Time

instance
  Prelude.NFData
    GetPredictiveScalingForecastResponse
  where
  rnf GetPredictiveScalingForecastResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf loadForecast
      `Prelude.seq` Prelude.rnf capacityForecast
      `Prelude.seq` Prelude.rnf updateTime

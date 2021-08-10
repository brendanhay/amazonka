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
-- Module      : Network.AWS.AutoScalingPlans.GetScalingPlanResourceForecastData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the forecast data for a scalable resource.
--
-- Capacity forecasts are represented as predicted values, or data points,
-- that are calculated using historical data points from a specified
-- CloudWatch load metric. Data points are available for up to 56 days.
module Network.AWS.AutoScalingPlans.GetScalingPlanResourceForecastData
  ( -- * Creating a Request
    GetScalingPlanResourceForecastData (..),
    newGetScalingPlanResourceForecastData,

    -- * Request Lenses
    getScalingPlanResourceForecastData_scalingPlanName,
    getScalingPlanResourceForecastData_scalingPlanVersion,
    getScalingPlanResourceForecastData_serviceNamespace,
    getScalingPlanResourceForecastData_resourceId,
    getScalingPlanResourceForecastData_scalableDimension,
    getScalingPlanResourceForecastData_forecastDataType,
    getScalingPlanResourceForecastData_startTime,
    getScalingPlanResourceForecastData_endTime,

    -- * Destructuring the Response
    GetScalingPlanResourceForecastDataResponse (..),
    newGetScalingPlanResourceForecastDataResponse,

    -- * Response Lenses
    getScalingPlanResourceForecastDataResponse_httpStatus,
    getScalingPlanResourceForecastDataResponse_datapoints,
  )
where

import Network.AWS.AutoScalingPlans.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetScalingPlanResourceForecastData' smart constructor.
data GetScalingPlanResourceForecastData = GetScalingPlanResourceForecastData'
  { -- | The name of the scaling plan.
    scalingPlanName :: Prelude.Text,
    -- | The version number of the scaling plan. Currently, the only valid value
    -- is @1@.
    scalingPlanVersion :: Prelude.Integer,
    -- | The namespace of the AWS service. The only valid value is @autoscaling@.
    serviceNamespace :: ServiceNamespace,
    -- | The ID of the resource. This string consists of a prefix
    -- (@autoScalingGroup@) followed by the name of a specified Auto Scaling
    -- group (@my-asg@). Example: @autoScalingGroup\/my-asg@.
    resourceId :: Prelude.Text,
    -- | The scalable dimension for the resource. The only valid value is
    -- @autoscaling:autoScalingGroup:DesiredCapacity@.
    scalableDimension :: ScalableDimension,
    -- | The type of forecast data to get.
    --
    -- -   @LoadForecast@: The load metric forecast.
    --
    -- -   @CapacityForecast@: The capacity forecast.
    --
    -- -   @ScheduledActionMinCapacity@: The minimum capacity for each
    --     scheduled scaling action. This data is calculated as the larger of
    --     two values: the capacity forecast or the minimum capacity in the
    --     scaling instruction.
    --
    -- -   @ScheduledActionMaxCapacity@: The maximum capacity for each
    --     scheduled scaling action. The calculation used is determined by the
    --     predictive scaling maximum capacity behavior setting in the scaling
    --     instruction.
    forecastDataType :: ForecastDataType,
    -- | The inclusive start time of the time range for the forecast data to get.
    -- The date and time can be at most 56 days before the current date and
    -- time.
    startTime :: Core.POSIX,
    -- | The exclusive end time of the time range for the forecast data to get.
    -- The maximum time duration between the start and end time is seven days.
    --
    -- Although this parameter can accept a date and time that is more than two
    -- days in the future, the availability of forecast data has limits. AWS
    -- Auto Scaling only issues forecasts for periods of two days in advance.
    endTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetScalingPlanResourceForecastData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scalingPlanName', 'getScalingPlanResourceForecastData_scalingPlanName' - The name of the scaling plan.
--
-- 'scalingPlanVersion', 'getScalingPlanResourceForecastData_scalingPlanVersion' - The version number of the scaling plan. Currently, the only valid value
-- is @1@.
--
-- 'serviceNamespace', 'getScalingPlanResourceForecastData_serviceNamespace' - The namespace of the AWS service. The only valid value is @autoscaling@.
--
-- 'resourceId', 'getScalingPlanResourceForecastData_resourceId' - The ID of the resource. This string consists of a prefix
-- (@autoScalingGroup@) followed by the name of a specified Auto Scaling
-- group (@my-asg@). Example: @autoScalingGroup\/my-asg@.
--
-- 'scalableDimension', 'getScalingPlanResourceForecastData_scalableDimension' - The scalable dimension for the resource. The only valid value is
-- @autoscaling:autoScalingGroup:DesiredCapacity@.
--
-- 'forecastDataType', 'getScalingPlanResourceForecastData_forecastDataType' - The type of forecast data to get.
--
-- -   @LoadForecast@: The load metric forecast.
--
-- -   @CapacityForecast@: The capacity forecast.
--
-- -   @ScheduledActionMinCapacity@: The minimum capacity for each
--     scheduled scaling action. This data is calculated as the larger of
--     two values: the capacity forecast or the minimum capacity in the
--     scaling instruction.
--
-- -   @ScheduledActionMaxCapacity@: The maximum capacity for each
--     scheduled scaling action. The calculation used is determined by the
--     predictive scaling maximum capacity behavior setting in the scaling
--     instruction.
--
-- 'startTime', 'getScalingPlanResourceForecastData_startTime' - The inclusive start time of the time range for the forecast data to get.
-- The date and time can be at most 56 days before the current date and
-- time.
--
-- 'endTime', 'getScalingPlanResourceForecastData_endTime' - The exclusive end time of the time range for the forecast data to get.
-- The maximum time duration between the start and end time is seven days.
--
-- Although this parameter can accept a date and time that is more than two
-- days in the future, the availability of forecast data has limits. AWS
-- Auto Scaling only issues forecasts for periods of two days in advance.
newGetScalingPlanResourceForecastData ::
  -- | 'scalingPlanName'
  Prelude.Text ->
  -- | 'scalingPlanVersion'
  Prelude.Integer ->
  -- | 'serviceNamespace'
  ServiceNamespace ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'scalableDimension'
  ScalableDimension ->
  -- | 'forecastDataType'
  ForecastDataType ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  GetScalingPlanResourceForecastData
newGetScalingPlanResourceForecastData
  pScalingPlanName_
  pScalingPlanVersion_
  pServiceNamespace_
  pResourceId_
  pScalableDimension_
  pForecastDataType_
  pStartTime_
  pEndTime_ =
    GetScalingPlanResourceForecastData'
      { scalingPlanName =
          pScalingPlanName_,
        scalingPlanVersion =
          pScalingPlanVersion_,
        serviceNamespace = pServiceNamespace_,
        resourceId = pResourceId_,
        scalableDimension = pScalableDimension_,
        forecastDataType = pForecastDataType_,
        startTime =
          Core._Time Lens.# pStartTime_,
        endTime = Core._Time Lens.# pEndTime_
      }

-- | The name of the scaling plan.
getScalingPlanResourceForecastData_scalingPlanName :: Lens.Lens' GetScalingPlanResourceForecastData Prelude.Text
getScalingPlanResourceForecastData_scalingPlanName = Lens.lens (\GetScalingPlanResourceForecastData' {scalingPlanName} -> scalingPlanName) (\s@GetScalingPlanResourceForecastData' {} a -> s {scalingPlanName = a} :: GetScalingPlanResourceForecastData)

-- | The version number of the scaling plan. Currently, the only valid value
-- is @1@.
getScalingPlanResourceForecastData_scalingPlanVersion :: Lens.Lens' GetScalingPlanResourceForecastData Prelude.Integer
getScalingPlanResourceForecastData_scalingPlanVersion = Lens.lens (\GetScalingPlanResourceForecastData' {scalingPlanVersion} -> scalingPlanVersion) (\s@GetScalingPlanResourceForecastData' {} a -> s {scalingPlanVersion = a} :: GetScalingPlanResourceForecastData)

-- | The namespace of the AWS service. The only valid value is @autoscaling@.
getScalingPlanResourceForecastData_serviceNamespace :: Lens.Lens' GetScalingPlanResourceForecastData ServiceNamespace
getScalingPlanResourceForecastData_serviceNamespace = Lens.lens (\GetScalingPlanResourceForecastData' {serviceNamespace} -> serviceNamespace) (\s@GetScalingPlanResourceForecastData' {} a -> s {serviceNamespace = a} :: GetScalingPlanResourceForecastData)

-- | The ID of the resource. This string consists of a prefix
-- (@autoScalingGroup@) followed by the name of a specified Auto Scaling
-- group (@my-asg@). Example: @autoScalingGroup\/my-asg@.
getScalingPlanResourceForecastData_resourceId :: Lens.Lens' GetScalingPlanResourceForecastData Prelude.Text
getScalingPlanResourceForecastData_resourceId = Lens.lens (\GetScalingPlanResourceForecastData' {resourceId} -> resourceId) (\s@GetScalingPlanResourceForecastData' {} a -> s {resourceId = a} :: GetScalingPlanResourceForecastData)

-- | The scalable dimension for the resource. The only valid value is
-- @autoscaling:autoScalingGroup:DesiredCapacity@.
getScalingPlanResourceForecastData_scalableDimension :: Lens.Lens' GetScalingPlanResourceForecastData ScalableDimension
getScalingPlanResourceForecastData_scalableDimension = Lens.lens (\GetScalingPlanResourceForecastData' {scalableDimension} -> scalableDimension) (\s@GetScalingPlanResourceForecastData' {} a -> s {scalableDimension = a} :: GetScalingPlanResourceForecastData)

-- | The type of forecast data to get.
--
-- -   @LoadForecast@: The load metric forecast.
--
-- -   @CapacityForecast@: The capacity forecast.
--
-- -   @ScheduledActionMinCapacity@: The minimum capacity for each
--     scheduled scaling action. This data is calculated as the larger of
--     two values: the capacity forecast or the minimum capacity in the
--     scaling instruction.
--
-- -   @ScheduledActionMaxCapacity@: The maximum capacity for each
--     scheduled scaling action. The calculation used is determined by the
--     predictive scaling maximum capacity behavior setting in the scaling
--     instruction.
getScalingPlanResourceForecastData_forecastDataType :: Lens.Lens' GetScalingPlanResourceForecastData ForecastDataType
getScalingPlanResourceForecastData_forecastDataType = Lens.lens (\GetScalingPlanResourceForecastData' {forecastDataType} -> forecastDataType) (\s@GetScalingPlanResourceForecastData' {} a -> s {forecastDataType = a} :: GetScalingPlanResourceForecastData)

-- | The inclusive start time of the time range for the forecast data to get.
-- The date and time can be at most 56 days before the current date and
-- time.
getScalingPlanResourceForecastData_startTime :: Lens.Lens' GetScalingPlanResourceForecastData Prelude.UTCTime
getScalingPlanResourceForecastData_startTime = Lens.lens (\GetScalingPlanResourceForecastData' {startTime} -> startTime) (\s@GetScalingPlanResourceForecastData' {} a -> s {startTime = a} :: GetScalingPlanResourceForecastData) Prelude.. Core._Time

-- | The exclusive end time of the time range for the forecast data to get.
-- The maximum time duration between the start and end time is seven days.
--
-- Although this parameter can accept a date and time that is more than two
-- days in the future, the availability of forecast data has limits. AWS
-- Auto Scaling only issues forecasts for periods of two days in advance.
getScalingPlanResourceForecastData_endTime :: Lens.Lens' GetScalingPlanResourceForecastData Prelude.UTCTime
getScalingPlanResourceForecastData_endTime = Lens.lens (\GetScalingPlanResourceForecastData' {endTime} -> endTime) (\s@GetScalingPlanResourceForecastData' {} a -> s {endTime = a} :: GetScalingPlanResourceForecastData) Prelude.. Core._Time

instance
  Core.AWSRequest
    GetScalingPlanResourceForecastData
  where
  type
    AWSResponse GetScalingPlanResourceForecastData =
      GetScalingPlanResourceForecastDataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetScalingPlanResourceForecastDataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Core..?> "Datapoints" Core..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    GetScalingPlanResourceForecastData

instance
  Prelude.NFData
    GetScalingPlanResourceForecastData

instance
  Core.ToHeaders
    GetScalingPlanResourceForecastData
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AnyScaleScalingPlannerFrontendService.GetScalingPlanResourceForecastData" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    GetScalingPlanResourceForecastData
  where
  toJSON GetScalingPlanResourceForecastData' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ScalingPlanName" Core..= scalingPlanName),
            Prelude.Just
              ("ScalingPlanVersion" Core..= scalingPlanVersion),
            Prelude.Just
              ("ServiceNamespace" Core..= serviceNamespace),
            Prelude.Just ("ResourceId" Core..= resourceId),
            Prelude.Just
              ("ScalableDimension" Core..= scalableDimension),
            Prelude.Just
              ("ForecastDataType" Core..= forecastDataType),
            Prelude.Just ("StartTime" Core..= startTime),
            Prelude.Just ("EndTime" Core..= endTime)
          ]
      )

instance
  Core.ToPath
    GetScalingPlanResourceForecastData
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    GetScalingPlanResourceForecastData
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetScalingPlanResourceForecastDataResponse' smart constructor.
data GetScalingPlanResourceForecastDataResponse = GetScalingPlanResourceForecastDataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The data points to return.
    datapoints :: [Datapoint]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetScalingPlanResourceForecastDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getScalingPlanResourceForecastDataResponse_httpStatus' - The response's http status code.
--
-- 'datapoints', 'getScalingPlanResourceForecastDataResponse_datapoints' - The data points to return.
newGetScalingPlanResourceForecastDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetScalingPlanResourceForecastDataResponse
newGetScalingPlanResourceForecastDataResponse
  pHttpStatus_ =
    GetScalingPlanResourceForecastDataResponse'
      { httpStatus =
          pHttpStatus_,
        datapoints = Prelude.mempty
      }

-- | The response's http status code.
getScalingPlanResourceForecastDataResponse_httpStatus :: Lens.Lens' GetScalingPlanResourceForecastDataResponse Prelude.Int
getScalingPlanResourceForecastDataResponse_httpStatus = Lens.lens (\GetScalingPlanResourceForecastDataResponse' {httpStatus} -> httpStatus) (\s@GetScalingPlanResourceForecastDataResponse' {} a -> s {httpStatus = a} :: GetScalingPlanResourceForecastDataResponse)

-- | The data points to return.
getScalingPlanResourceForecastDataResponse_datapoints :: Lens.Lens' GetScalingPlanResourceForecastDataResponse [Datapoint]
getScalingPlanResourceForecastDataResponse_datapoints = Lens.lens (\GetScalingPlanResourceForecastDataResponse' {datapoints} -> datapoints) (\s@GetScalingPlanResourceForecastDataResponse' {} a -> s {datapoints = a} :: GetScalingPlanResourceForecastDataResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    GetScalingPlanResourceForecastDataResponse

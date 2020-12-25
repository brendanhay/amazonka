{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.GetScalingPlanResourceForecastData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the forecast data for a scalable resource.
--
-- Capacity forecasts are represented as predicted values, or data points, that are calculated using historical data points from a specified CloudWatch load metric. Data points are available for up to 56 days.
module Network.AWS.AutoScalingPlans.GetScalingPlanResourceForecastData
  ( -- * Creating a request
    GetScalingPlanResourceForecastData (..),
    mkGetScalingPlanResourceForecastData,

    -- ** Request lenses
    gsprfdScalingPlanName,
    gsprfdScalingPlanVersion,
    gsprfdServiceNamespace,
    gsprfdResourceId,
    gsprfdScalableDimension,
    gsprfdForecastDataType,
    gsprfdStartTime,
    gsprfdEndTime,

    -- * Destructuring the response
    GetScalingPlanResourceForecastDataResponse (..),
    mkGetScalingPlanResourceForecastDataResponse,

    -- ** Response lenses
    gsprfdrrsDatapoints,
    gsprfdrrsResponseStatus,
  )
where

import qualified Network.AWS.AutoScalingPlans.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetScalingPlanResourceForecastData' smart constructor.
data GetScalingPlanResourceForecastData = GetScalingPlanResourceForecastData'
  { -- | The name of the scaling plan.
    scalingPlanName :: Types.ScalingPlanName,
    -- | The version number of the scaling plan.
    scalingPlanVersion :: Core.Integer,
    -- | The namespace of the AWS service.
    serviceNamespace :: Types.ServiceNamespace,
    -- | The ID of the resource. This string consists of the resource type and unique identifier.
    --
    --
    --     * Auto Scaling group - The resource type is @autoScalingGroup@ and the unique identifier is the name of the Auto Scaling group. Example: @autoScalingGroup/my-asg@ .
    --
    --
    --     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .
    --
    --
    --     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .
    --
    --
    --     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .
    --
    --
    --     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .
    --
    --
    --     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
    resourceId :: Types.XmlString,
    -- | The scalable dimension for the resource.
    scalableDimension :: Types.ScalableDimension,
    -- | The type of forecast data to get.
    --
    --
    --     * @LoadForecast@ : The load metric forecast.
    --
    --
    --     * @CapacityForecast@ : The capacity forecast.
    --
    --
    --     * @ScheduledActionMinCapacity@ : The minimum capacity for each scheduled scaling action. This data is calculated as the larger of two values: the capacity forecast or the minimum capacity in the scaling instruction.
    --
    --
    --     * @ScheduledActionMaxCapacity@ : The maximum capacity for each scheduled scaling action. The calculation used is determined by the predictive scaling maximum capacity behavior setting in the scaling instruction.
    forecastDataType :: Types.ForecastDataType,
    -- | The inclusive start time of the time range for the forecast data to get. The date and time can be at most 56 days before the current date and time.
    startTime :: Core.NominalDiffTime,
    -- | The exclusive end time of the time range for the forecast data to get. The maximum time duration between the start and end time is seven days.
    --
    -- Although this parameter can accept a date and time that is more than two days in the future, the availability of forecast data has limits. AWS Auto Scaling only issues forecasts for periods of two days in advance.
    endTime :: Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetScalingPlanResourceForecastData' value with any optional fields omitted.
mkGetScalingPlanResourceForecastData ::
  -- | 'scalingPlanName'
  Types.ScalingPlanName ->
  -- | 'scalingPlanVersion'
  Core.Integer ->
  -- | 'serviceNamespace'
  Types.ServiceNamespace ->
  -- | 'resourceId'
  Types.XmlString ->
  -- | 'scalableDimension'
  Types.ScalableDimension ->
  -- | 'forecastDataType'
  Types.ForecastDataType ->
  -- | 'startTime'
  Core.NominalDiffTime ->
  -- | 'endTime'
  Core.NominalDiffTime ->
  GetScalingPlanResourceForecastData
mkGetScalingPlanResourceForecastData
  scalingPlanName
  scalingPlanVersion
  serviceNamespace
  resourceId
  scalableDimension
  forecastDataType
  startTime
  endTime =
    GetScalingPlanResourceForecastData'
      { scalingPlanName,
        scalingPlanVersion,
        serviceNamespace,
        resourceId,
        scalableDimension,
        forecastDataType,
        startTime,
        endTime
      }

-- | The name of the scaling plan.
--
-- /Note:/ Consider using 'scalingPlanName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsprfdScalingPlanName :: Lens.Lens' GetScalingPlanResourceForecastData Types.ScalingPlanName
gsprfdScalingPlanName = Lens.field @"scalingPlanName"
{-# DEPRECATED gsprfdScalingPlanName "Use generic-lens or generic-optics with 'scalingPlanName' instead." #-}

-- | The version number of the scaling plan.
--
-- /Note:/ Consider using 'scalingPlanVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsprfdScalingPlanVersion :: Lens.Lens' GetScalingPlanResourceForecastData Core.Integer
gsprfdScalingPlanVersion = Lens.field @"scalingPlanVersion"
{-# DEPRECATED gsprfdScalingPlanVersion "Use generic-lens or generic-optics with 'scalingPlanVersion' instead." #-}

-- | The namespace of the AWS service.
--
-- /Note:/ Consider using 'serviceNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsprfdServiceNamespace :: Lens.Lens' GetScalingPlanResourceForecastData Types.ServiceNamespace
gsprfdServiceNamespace = Lens.field @"serviceNamespace"
{-# DEPRECATED gsprfdServiceNamespace "Use generic-lens or generic-optics with 'serviceNamespace' instead." #-}

-- | The ID of the resource. This string consists of the resource type and unique identifier.
--
--
--     * Auto Scaling group - The resource type is @autoScalingGroup@ and the unique identifier is the name of the Auto Scaling group. Example: @autoScalingGroup/my-asg@ .
--
--
--     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .
--
--
--     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .
--
--
--     * DynamoDB table - The resource type is @table@ and the unique identifier is the resource ID. Example: @table/my-table@ .
--
--
--     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the resource ID. Example: @table/my-table/index/my-table-index@ .
--
--
--     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .
--
--
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsprfdResourceId :: Lens.Lens' GetScalingPlanResourceForecastData Types.XmlString
gsprfdResourceId = Lens.field @"resourceId"
{-# DEPRECATED gsprfdResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The scalable dimension for the resource.
--
-- /Note:/ Consider using 'scalableDimension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsprfdScalableDimension :: Lens.Lens' GetScalingPlanResourceForecastData Types.ScalableDimension
gsprfdScalableDimension = Lens.field @"scalableDimension"
{-# DEPRECATED gsprfdScalableDimension "Use generic-lens or generic-optics with 'scalableDimension' instead." #-}

-- | The type of forecast data to get.
--
--
--     * @LoadForecast@ : The load metric forecast.
--
--
--     * @CapacityForecast@ : The capacity forecast.
--
--
--     * @ScheduledActionMinCapacity@ : The minimum capacity for each scheduled scaling action. This data is calculated as the larger of two values: the capacity forecast or the minimum capacity in the scaling instruction.
--
--
--     * @ScheduledActionMaxCapacity@ : The maximum capacity for each scheduled scaling action. The calculation used is determined by the predictive scaling maximum capacity behavior setting in the scaling instruction.
--
--
--
-- /Note:/ Consider using 'forecastDataType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsprfdForecastDataType :: Lens.Lens' GetScalingPlanResourceForecastData Types.ForecastDataType
gsprfdForecastDataType = Lens.field @"forecastDataType"
{-# DEPRECATED gsprfdForecastDataType "Use generic-lens or generic-optics with 'forecastDataType' instead." #-}

-- | The inclusive start time of the time range for the forecast data to get. The date and time can be at most 56 days before the current date and time.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsprfdStartTime :: Lens.Lens' GetScalingPlanResourceForecastData Core.NominalDiffTime
gsprfdStartTime = Lens.field @"startTime"
{-# DEPRECATED gsprfdStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The exclusive end time of the time range for the forecast data to get. The maximum time duration between the start and end time is seven days.
--
-- Although this parameter can accept a date and time that is more than two days in the future, the availability of forecast data has limits. AWS Auto Scaling only issues forecasts for periods of two days in advance.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsprfdEndTime :: Lens.Lens' GetScalingPlanResourceForecastData Core.NominalDiffTime
gsprfdEndTime = Lens.field @"endTime"
{-# DEPRECATED gsprfdEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

instance Core.FromJSON GetScalingPlanResourceForecastData where
  toJSON GetScalingPlanResourceForecastData {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ScalingPlanName" Core..= scalingPlanName),
            Core.Just ("ScalingPlanVersion" Core..= scalingPlanVersion),
            Core.Just ("ServiceNamespace" Core..= serviceNamespace),
            Core.Just ("ResourceId" Core..= resourceId),
            Core.Just ("ScalableDimension" Core..= scalableDimension),
            Core.Just ("ForecastDataType" Core..= forecastDataType),
            Core.Just ("StartTime" Core..= startTime),
            Core.Just ("EndTime" Core..= endTime)
          ]
      )

instance Core.AWSRequest GetScalingPlanResourceForecastData where
  type
    Rs GetScalingPlanResourceForecastData =
      GetScalingPlanResourceForecastDataResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AnyScaleScalingPlannerFrontendService.GetScalingPlanResourceForecastData"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetScalingPlanResourceForecastDataResponse'
            Core.<$> (x Core..:? "Datapoints" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetScalingPlanResourceForecastDataResponse' smart constructor.
data GetScalingPlanResourceForecastDataResponse = GetScalingPlanResourceForecastDataResponse'
  { -- | The data points to return.
    datapoints :: [Types.Datapoint],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetScalingPlanResourceForecastDataResponse' value with any optional fields omitted.
mkGetScalingPlanResourceForecastDataResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetScalingPlanResourceForecastDataResponse
mkGetScalingPlanResourceForecastDataResponse responseStatus =
  GetScalingPlanResourceForecastDataResponse'
    { datapoints =
        Core.mempty,
      responseStatus
    }

-- | The data points to return.
--
-- /Note:/ Consider using 'datapoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsprfdrrsDatapoints :: Lens.Lens' GetScalingPlanResourceForecastDataResponse [Types.Datapoint]
gsprfdrrsDatapoints = Lens.field @"datapoints"
{-# DEPRECATED gsprfdrrsDatapoints "Use generic-lens or generic-optics with 'datapoints' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsprfdrrsResponseStatus :: Lens.Lens' GetScalingPlanResourceForecastDataResponse Core.Int
gsprfdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsprfdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

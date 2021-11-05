{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Location.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Location.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * BatchItemErrorCode
    BatchItemErrorCode (..),

    -- * DimensionUnit
    DimensionUnit (..),

    -- * DistanceUnit
    DistanceUnit (..),

    -- * IntendedUse
    IntendedUse (..),

    -- * PositionFiltering
    PositionFiltering (..),

    -- * PricingPlan
    PricingPlan (..),

    -- * TravelMode
    TravelMode (..),

    -- * VehicleWeightUnit
    VehicleWeightUnit (..),

    -- * BatchDeleteDevicePositionHistoryError
    BatchDeleteDevicePositionHistoryError (..),
    newBatchDeleteDevicePositionHistoryError,
    batchDeleteDevicePositionHistoryError_deviceId,
    batchDeleteDevicePositionHistoryError_error,

    -- * BatchDeleteGeofenceError
    BatchDeleteGeofenceError (..),
    newBatchDeleteGeofenceError,
    batchDeleteGeofenceError_error,
    batchDeleteGeofenceError_geofenceId,

    -- * BatchEvaluateGeofencesError
    BatchEvaluateGeofencesError (..),
    newBatchEvaluateGeofencesError,
    batchEvaluateGeofencesError_deviceId,
    batchEvaluateGeofencesError_error,
    batchEvaluateGeofencesError_sampleTime,

    -- * BatchGetDevicePositionError
    BatchGetDevicePositionError (..),
    newBatchGetDevicePositionError,
    batchGetDevicePositionError_deviceId,
    batchGetDevicePositionError_error,

    -- * BatchItemError
    BatchItemError (..),
    newBatchItemError,
    batchItemError_code,
    batchItemError_message,

    -- * BatchPutGeofenceError
    BatchPutGeofenceError (..),
    newBatchPutGeofenceError,
    batchPutGeofenceError_error,
    batchPutGeofenceError_geofenceId,

    -- * BatchPutGeofenceRequestEntry
    BatchPutGeofenceRequestEntry (..),
    newBatchPutGeofenceRequestEntry,
    batchPutGeofenceRequestEntry_geofenceId,
    batchPutGeofenceRequestEntry_geometry,

    -- * BatchPutGeofenceSuccess
    BatchPutGeofenceSuccess (..),
    newBatchPutGeofenceSuccess,
    batchPutGeofenceSuccess_createTime,
    batchPutGeofenceSuccess_geofenceId,
    batchPutGeofenceSuccess_updateTime,

    -- * BatchUpdateDevicePositionError
    BatchUpdateDevicePositionError (..),
    newBatchUpdateDevicePositionError,
    batchUpdateDevicePositionError_deviceId,
    batchUpdateDevicePositionError_error,
    batchUpdateDevicePositionError_sampleTime,

    -- * CalculateRouteCarModeOptions
    CalculateRouteCarModeOptions (..),
    newCalculateRouteCarModeOptions,
    calculateRouteCarModeOptions_avoidTolls,
    calculateRouteCarModeOptions_avoidFerries,

    -- * CalculateRouteSummary
    CalculateRouteSummary (..),
    newCalculateRouteSummary,
    calculateRouteSummary_dataSource,
    calculateRouteSummary_distance,
    calculateRouteSummary_distanceUnit,
    calculateRouteSummary_durationSeconds,
    calculateRouteSummary_routeBBox,

    -- * CalculateRouteTruckModeOptions
    CalculateRouteTruckModeOptions (..),
    newCalculateRouteTruckModeOptions,
    calculateRouteTruckModeOptions_weight,
    calculateRouteTruckModeOptions_avoidTolls,
    calculateRouteTruckModeOptions_dimensions,
    calculateRouteTruckModeOptions_avoidFerries,

    -- * DataSourceConfiguration
    DataSourceConfiguration (..),
    newDataSourceConfiguration,
    dataSourceConfiguration_intendedUse,

    -- * DevicePosition
    DevicePosition (..),
    newDevicePosition,
    devicePosition_deviceId,
    devicePosition_position,
    devicePosition_receivedTime,
    devicePosition_sampleTime,

    -- * DevicePositionUpdate
    DevicePositionUpdate (..),
    newDevicePositionUpdate,
    devicePositionUpdate_deviceId,
    devicePositionUpdate_position,
    devicePositionUpdate_sampleTime,

    -- * GeofenceGeometry
    GeofenceGeometry (..),
    newGeofenceGeometry,
    geofenceGeometry_polygon,

    -- * Leg
    Leg (..),
    newLeg,
    leg_geometry,
    leg_distance,
    leg_durationSeconds,
    leg_endPosition,
    leg_startPosition,
    leg_steps,

    -- * LegGeometry
    LegGeometry (..),
    newLegGeometry,
    legGeometry_lineString,

    -- * ListDevicePositionsResponseEntry
    ListDevicePositionsResponseEntry (..),
    newListDevicePositionsResponseEntry,
    listDevicePositionsResponseEntry_deviceId,
    listDevicePositionsResponseEntry_position,
    listDevicePositionsResponseEntry_sampleTime,

    -- * ListGeofenceCollectionsResponseEntry
    ListGeofenceCollectionsResponseEntry (..),
    newListGeofenceCollectionsResponseEntry,
    listGeofenceCollectionsResponseEntry_pricingPlanDataSource,
    listGeofenceCollectionsResponseEntry_collectionName,
    listGeofenceCollectionsResponseEntry_createTime,
    listGeofenceCollectionsResponseEntry_description,
    listGeofenceCollectionsResponseEntry_pricingPlan,
    listGeofenceCollectionsResponseEntry_updateTime,

    -- * ListGeofenceResponseEntry
    ListGeofenceResponseEntry (..),
    newListGeofenceResponseEntry,
    listGeofenceResponseEntry_createTime,
    listGeofenceResponseEntry_geofenceId,
    listGeofenceResponseEntry_geometry,
    listGeofenceResponseEntry_status,
    listGeofenceResponseEntry_updateTime,

    -- * ListMapsResponseEntry
    ListMapsResponseEntry (..),
    newListMapsResponseEntry,
    listMapsResponseEntry_createTime,
    listMapsResponseEntry_dataSource,
    listMapsResponseEntry_description,
    listMapsResponseEntry_mapName,
    listMapsResponseEntry_pricingPlan,
    listMapsResponseEntry_updateTime,

    -- * ListPlaceIndexesResponseEntry
    ListPlaceIndexesResponseEntry (..),
    newListPlaceIndexesResponseEntry,
    listPlaceIndexesResponseEntry_createTime,
    listPlaceIndexesResponseEntry_dataSource,
    listPlaceIndexesResponseEntry_description,
    listPlaceIndexesResponseEntry_indexName,
    listPlaceIndexesResponseEntry_pricingPlan,
    listPlaceIndexesResponseEntry_updateTime,

    -- * ListRouteCalculatorsResponseEntry
    ListRouteCalculatorsResponseEntry (..),
    newListRouteCalculatorsResponseEntry,
    listRouteCalculatorsResponseEntry_calculatorName,
    listRouteCalculatorsResponseEntry_createTime,
    listRouteCalculatorsResponseEntry_dataSource,
    listRouteCalculatorsResponseEntry_description,
    listRouteCalculatorsResponseEntry_pricingPlan,
    listRouteCalculatorsResponseEntry_updateTime,

    -- * ListTrackersResponseEntry
    ListTrackersResponseEntry (..),
    newListTrackersResponseEntry,
    listTrackersResponseEntry_pricingPlanDataSource,
    listTrackersResponseEntry_createTime,
    listTrackersResponseEntry_description,
    listTrackersResponseEntry_pricingPlan,
    listTrackersResponseEntry_trackerName,
    listTrackersResponseEntry_updateTime,

    -- * MapConfiguration
    MapConfiguration (..),
    newMapConfiguration,
    mapConfiguration_style,

    -- * Place
    Place (..),
    newPlace,
    place_municipality,
    place_addressNumber,
    place_postalCode,
    place_country,
    place_street,
    place_subRegion,
    place_region,
    place_label,
    place_neighborhood,
    place_geometry,

    -- * PlaceGeometry
    PlaceGeometry (..),
    newPlaceGeometry,
    placeGeometry_point,

    -- * SearchForPositionResult
    SearchForPositionResult (..),
    newSearchForPositionResult,
    searchForPositionResult_place,

    -- * SearchForTextResult
    SearchForTextResult (..),
    newSearchForTextResult,
    searchForTextResult_place,

    -- * SearchPlaceIndexForPositionSummary
    SearchPlaceIndexForPositionSummary (..),
    newSearchPlaceIndexForPositionSummary,
    searchPlaceIndexForPositionSummary_maxResults,
    searchPlaceIndexForPositionSummary_dataSource,
    searchPlaceIndexForPositionSummary_position,

    -- * SearchPlaceIndexForTextSummary
    SearchPlaceIndexForTextSummary (..),
    newSearchPlaceIndexForTextSummary,
    searchPlaceIndexForTextSummary_filterBBox,
    searchPlaceIndexForTextSummary_resultBBox,
    searchPlaceIndexForTextSummary_biasPosition,
    searchPlaceIndexForTextSummary_filterCountries,
    searchPlaceIndexForTextSummary_maxResults,
    searchPlaceIndexForTextSummary_dataSource,
    searchPlaceIndexForTextSummary_text,

    -- * Step
    Step (..),
    newStep,
    step_geometryOffset,
    step_distance,
    step_durationSeconds,
    step_endPosition,
    step_startPosition,

    -- * TruckDimensions
    TruckDimensions (..),
    newTruckDimensions,
    truckDimensions_length,
    truckDimensions_height,
    truckDimensions_width,
    truckDimensions_unit,

    -- * TruckWeight
    TruckWeight (..),
    newTruckWeight,
    truckWeight_total,
    truckWeight_unit,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Location.Types.BatchDeleteDevicePositionHistoryError
import Network.AWS.Location.Types.BatchDeleteGeofenceError
import Network.AWS.Location.Types.BatchEvaluateGeofencesError
import Network.AWS.Location.Types.BatchGetDevicePositionError
import Network.AWS.Location.Types.BatchItemError
import Network.AWS.Location.Types.BatchItemErrorCode
import Network.AWS.Location.Types.BatchPutGeofenceError
import Network.AWS.Location.Types.BatchPutGeofenceRequestEntry
import Network.AWS.Location.Types.BatchPutGeofenceSuccess
import Network.AWS.Location.Types.BatchUpdateDevicePositionError
import Network.AWS.Location.Types.CalculateRouteCarModeOptions
import Network.AWS.Location.Types.CalculateRouteSummary
import Network.AWS.Location.Types.CalculateRouteTruckModeOptions
import Network.AWS.Location.Types.DataSourceConfiguration
import Network.AWS.Location.Types.DevicePosition
import Network.AWS.Location.Types.DevicePositionUpdate
import Network.AWS.Location.Types.DimensionUnit
import Network.AWS.Location.Types.DistanceUnit
import Network.AWS.Location.Types.GeofenceGeometry
import Network.AWS.Location.Types.IntendedUse
import Network.AWS.Location.Types.Leg
import Network.AWS.Location.Types.LegGeometry
import Network.AWS.Location.Types.ListDevicePositionsResponseEntry
import Network.AWS.Location.Types.ListGeofenceCollectionsResponseEntry
import Network.AWS.Location.Types.ListGeofenceResponseEntry
import Network.AWS.Location.Types.ListMapsResponseEntry
import Network.AWS.Location.Types.ListPlaceIndexesResponseEntry
import Network.AWS.Location.Types.ListRouteCalculatorsResponseEntry
import Network.AWS.Location.Types.ListTrackersResponseEntry
import Network.AWS.Location.Types.MapConfiguration
import Network.AWS.Location.Types.Place
import Network.AWS.Location.Types.PlaceGeometry
import Network.AWS.Location.Types.PositionFiltering
import Network.AWS.Location.Types.PricingPlan
import Network.AWS.Location.Types.SearchForPositionResult
import Network.AWS.Location.Types.SearchForTextResult
import Network.AWS.Location.Types.SearchPlaceIndexForPositionSummary
import Network.AWS.Location.Types.SearchPlaceIndexForTextSummary
import Network.AWS.Location.Types.Step
import Network.AWS.Location.Types.TravelMode
import Network.AWS.Location.Types.TruckDimensions
import Network.AWS.Location.Types.TruckWeight
import Network.AWS.Location.Types.VehicleWeightUnit
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2020-11-19@ of the Amazon Location Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Location",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "geo",
      Core._serviceSigningName = "geo",
      Core._serviceVersion = "2020-11-19",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Location",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The input failed to meet the constraints specified by the AWS service.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | The request was denied because of insufficient access or permissions.
-- Check with an administrator to verify your permissions.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The request was unsuccessful because of a conflict.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The operation was denied because the request would exceed the maximum
-- <https://docs.aws.amazon.com/location/latest/developerguide/location-quotas.html quota>
-- set for Amazon Location Service.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was denied because of request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The request has failed to process because of an unknown server error,
-- exception, or failure.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The resource that you\'ve entered was not found in your AWS account.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

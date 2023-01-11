{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Location.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

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

    -- * RouteMatrixErrorCode
    RouteMatrixErrorCode (..),

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
    calculateRouteCarModeOptions_avoidFerries,
    calculateRouteCarModeOptions_avoidTolls,

    -- * CalculateRouteMatrixSummary
    CalculateRouteMatrixSummary (..),
    newCalculateRouteMatrixSummary,
    calculateRouteMatrixSummary_dataSource,
    calculateRouteMatrixSummary_distanceUnit,
    calculateRouteMatrixSummary_errorCount,
    calculateRouteMatrixSummary_routeCount,

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
    calculateRouteTruckModeOptions_avoidFerries,
    calculateRouteTruckModeOptions_avoidTolls,
    calculateRouteTruckModeOptions_dimensions,
    calculateRouteTruckModeOptions_weight,

    -- * Circle
    Circle (..),
    newCircle,
    circle_center,
    circle_radius,

    -- * DataSourceConfiguration
    DataSourceConfiguration (..),
    newDataSourceConfiguration,
    dataSourceConfiguration_intendedUse,

    -- * DevicePosition
    DevicePosition (..),
    newDevicePosition,
    devicePosition_accuracy,
    devicePosition_deviceId,
    devicePosition_positionProperties,
    devicePosition_position,
    devicePosition_receivedTime,
    devicePosition_sampleTime,

    -- * DevicePositionUpdate
    DevicePositionUpdate (..),
    newDevicePositionUpdate,
    devicePositionUpdate_accuracy,
    devicePositionUpdate_positionProperties,
    devicePositionUpdate_deviceId,
    devicePositionUpdate_position,
    devicePositionUpdate_sampleTime,

    -- * GeofenceGeometry
    GeofenceGeometry (..),
    newGeofenceGeometry,
    geofenceGeometry_circle,
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
    listDevicePositionsResponseEntry_accuracy,
    listDevicePositionsResponseEntry_positionProperties,
    listDevicePositionsResponseEntry_deviceId,
    listDevicePositionsResponseEntry_position,
    listDevicePositionsResponseEntry_sampleTime,

    -- * ListGeofenceCollectionsResponseEntry
    ListGeofenceCollectionsResponseEntry (..),
    newListGeofenceCollectionsResponseEntry,
    listGeofenceCollectionsResponseEntry_pricingPlan,
    listGeofenceCollectionsResponseEntry_pricingPlanDataSource,
    listGeofenceCollectionsResponseEntry_collectionName,
    listGeofenceCollectionsResponseEntry_createTime,
    listGeofenceCollectionsResponseEntry_description,
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
    listMapsResponseEntry_pricingPlan,
    listMapsResponseEntry_createTime,
    listMapsResponseEntry_dataSource,
    listMapsResponseEntry_description,
    listMapsResponseEntry_mapName,
    listMapsResponseEntry_updateTime,

    -- * ListPlaceIndexesResponseEntry
    ListPlaceIndexesResponseEntry (..),
    newListPlaceIndexesResponseEntry,
    listPlaceIndexesResponseEntry_pricingPlan,
    listPlaceIndexesResponseEntry_createTime,
    listPlaceIndexesResponseEntry_dataSource,
    listPlaceIndexesResponseEntry_description,
    listPlaceIndexesResponseEntry_indexName,
    listPlaceIndexesResponseEntry_updateTime,

    -- * ListRouteCalculatorsResponseEntry
    ListRouteCalculatorsResponseEntry (..),
    newListRouteCalculatorsResponseEntry,
    listRouteCalculatorsResponseEntry_pricingPlan,
    listRouteCalculatorsResponseEntry_calculatorName,
    listRouteCalculatorsResponseEntry_createTime,
    listRouteCalculatorsResponseEntry_dataSource,
    listRouteCalculatorsResponseEntry_description,
    listRouteCalculatorsResponseEntry_updateTime,

    -- * ListTrackersResponseEntry
    ListTrackersResponseEntry (..),
    newListTrackersResponseEntry,
    listTrackersResponseEntry_pricingPlan,
    listTrackersResponseEntry_pricingPlanDataSource,
    listTrackersResponseEntry_createTime,
    listTrackersResponseEntry_description,
    listTrackersResponseEntry_trackerName,
    listTrackersResponseEntry_updateTime,

    -- * MapConfiguration
    MapConfiguration (..),
    newMapConfiguration,
    mapConfiguration_style,

    -- * Place
    Place (..),
    newPlace,
    place_addressNumber,
    place_country,
    place_interpolated,
    place_label,
    place_municipality,
    place_neighborhood,
    place_postalCode,
    place_region,
    place_street,
    place_subRegion,
    place_timeZone,
    place_unitNumber,
    place_unitType,
    place_geometry,

    -- * PlaceGeometry
    PlaceGeometry (..),
    newPlaceGeometry,
    placeGeometry_point,

    -- * PositionalAccuracy
    PositionalAccuracy (..),
    newPositionalAccuracy,
    positionalAccuracy_horizontal,

    -- * RouteMatrixEntry
    RouteMatrixEntry (..),
    newRouteMatrixEntry,
    routeMatrixEntry_distance,
    routeMatrixEntry_durationSeconds,
    routeMatrixEntry_error,

    -- * RouteMatrixEntryError
    RouteMatrixEntryError (..),
    newRouteMatrixEntryError,
    routeMatrixEntryError_message,
    routeMatrixEntryError_code,

    -- * SearchForPositionResult
    SearchForPositionResult (..),
    newSearchForPositionResult,
    searchForPositionResult_placeId,
    searchForPositionResult_distance,
    searchForPositionResult_place,

    -- * SearchForSuggestionsResult
    SearchForSuggestionsResult (..),
    newSearchForSuggestionsResult,
    searchForSuggestionsResult_placeId,
    searchForSuggestionsResult_text,

    -- * SearchForTextResult
    SearchForTextResult (..),
    newSearchForTextResult,
    searchForTextResult_distance,
    searchForTextResult_placeId,
    searchForTextResult_relevance,
    searchForTextResult_place,

    -- * SearchPlaceIndexForPositionSummary
    SearchPlaceIndexForPositionSummary (..),
    newSearchPlaceIndexForPositionSummary,
    searchPlaceIndexForPositionSummary_language,
    searchPlaceIndexForPositionSummary_maxResults,
    searchPlaceIndexForPositionSummary_dataSource,
    searchPlaceIndexForPositionSummary_position,

    -- * SearchPlaceIndexForSuggestionsSummary
    SearchPlaceIndexForSuggestionsSummary (..),
    newSearchPlaceIndexForSuggestionsSummary,
    searchPlaceIndexForSuggestionsSummary_biasPosition,
    searchPlaceIndexForSuggestionsSummary_filterBBox,
    searchPlaceIndexForSuggestionsSummary_filterCountries,
    searchPlaceIndexForSuggestionsSummary_language,
    searchPlaceIndexForSuggestionsSummary_maxResults,
    searchPlaceIndexForSuggestionsSummary_dataSource,
    searchPlaceIndexForSuggestionsSummary_text,

    -- * SearchPlaceIndexForTextSummary
    SearchPlaceIndexForTextSummary (..),
    newSearchPlaceIndexForTextSummary,
    searchPlaceIndexForTextSummary_biasPosition,
    searchPlaceIndexForTextSummary_filterBBox,
    searchPlaceIndexForTextSummary_filterCountries,
    searchPlaceIndexForTextSummary_language,
    searchPlaceIndexForTextSummary_maxResults,
    searchPlaceIndexForTextSummary_resultBBox,
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

    -- * TimeZone
    TimeZone (..),
    newTimeZone,
    timeZone_offset,
    timeZone_name,

    -- * TruckDimensions
    TruckDimensions (..),
    newTruckDimensions,
    truckDimensions_height,
    truckDimensions_length,
    truckDimensions_unit,
    truckDimensions_width,

    -- * TruckWeight
    TruckWeight (..),
    newTruckWeight,
    truckWeight_total,
    truckWeight_unit,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Location.Types.BatchDeleteDevicePositionHistoryError
import Amazonka.Location.Types.BatchDeleteGeofenceError
import Amazonka.Location.Types.BatchEvaluateGeofencesError
import Amazonka.Location.Types.BatchGetDevicePositionError
import Amazonka.Location.Types.BatchItemError
import Amazonka.Location.Types.BatchItemErrorCode
import Amazonka.Location.Types.BatchPutGeofenceError
import Amazonka.Location.Types.BatchPutGeofenceRequestEntry
import Amazonka.Location.Types.BatchPutGeofenceSuccess
import Amazonka.Location.Types.BatchUpdateDevicePositionError
import Amazonka.Location.Types.CalculateRouteCarModeOptions
import Amazonka.Location.Types.CalculateRouteMatrixSummary
import Amazonka.Location.Types.CalculateRouteSummary
import Amazonka.Location.Types.CalculateRouteTruckModeOptions
import Amazonka.Location.Types.Circle
import Amazonka.Location.Types.DataSourceConfiguration
import Amazonka.Location.Types.DevicePosition
import Amazonka.Location.Types.DevicePositionUpdate
import Amazonka.Location.Types.DimensionUnit
import Amazonka.Location.Types.DistanceUnit
import Amazonka.Location.Types.GeofenceGeometry
import Amazonka.Location.Types.IntendedUse
import Amazonka.Location.Types.Leg
import Amazonka.Location.Types.LegGeometry
import Amazonka.Location.Types.ListDevicePositionsResponseEntry
import Amazonka.Location.Types.ListGeofenceCollectionsResponseEntry
import Amazonka.Location.Types.ListGeofenceResponseEntry
import Amazonka.Location.Types.ListMapsResponseEntry
import Amazonka.Location.Types.ListPlaceIndexesResponseEntry
import Amazonka.Location.Types.ListRouteCalculatorsResponseEntry
import Amazonka.Location.Types.ListTrackersResponseEntry
import Amazonka.Location.Types.MapConfiguration
import Amazonka.Location.Types.Place
import Amazonka.Location.Types.PlaceGeometry
import Amazonka.Location.Types.PositionFiltering
import Amazonka.Location.Types.PositionalAccuracy
import Amazonka.Location.Types.PricingPlan
import Amazonka.Location.Types.RouteMatrixEntry
import Amazonka.Location.Types.RouteMatrixEntryError
import Amazonka.Location.Types.RouteMatrixErrorCode
import Amazonka.Location.Types.SearchForPositionResult
import Amazonka.Location.Types.SearchForSuggestionsResult
import Amazonka.Location.Types.SearchForTextResult
import Amazonka.Location.Types.SearchPlaceIndexForPositionSummary
import Amazonka.Location.Types.SearchPlaceIndexForSuggestionsSummary
import Amazonka.Location.Types.SearchPlaceIndexForTextSummary
import Amazonka.Location.Types.Step
import Amazonka.Location.Types.TimeZone
import Amazonka.Location.Types.TravelMode
import Amazonka.Location.Types.TruckDimensions
import Amazonka.Location.Types.TruckWeight
import Amazonka.Location.Types.VehicleWeightUnit
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-11-19@ of the Amazon Location Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Location",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "geo",
      Core.signingName = "geo",
      Core.version = "2020-11-19",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Location",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The request was denied because of insufficient access or permissions.
-- Check with an administrator to verify your permissions.
_AccessDeniedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The request was unsuccessful because of a conflict.
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request has failed to process because of an unknown server error,
-- exception, or failure.
_InternalServerException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The resource that you\'ve entered was not found in your AWS account.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The operation was denied because the request would exceed the maximum
-- <https://docs.aws.amazon.com/location/latest/developerguide/location-quotas.html quota>
-- set for Amazon Location Service.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was denied because of request throttling.
_ThrottlingException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input failed to meet the constraints specified by the AWS service.
_ValidationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Location.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Lens
  ( -- * Operations

    -- ** AssociateTrackerConsumer
    associateTrackerConsumer_consumerArn,
    associateTrackerConsumer_trackerName,
    associateTrackerConsumerResponse_httpStatus,

    -- ** BatchDeleteDevicePositionHistory
    batchDeleteDevicePositionHistory_deviceIds,
    batchDeleteDevicePositionHistory_trackerName,
    batchDeleteDevicePositionHistoryResponse_httpStatus,
    batchDeleteDevicePositionHistoryResponse_errors,

    -- ** BatchDeleteGeofence
    batchDeleteGeofence_collectionName,
    batchDeleteGeofence_geofenceIds,
    batchDeleteGeofenceResponse_httpStatus,
    batchDeleteGeofenceResponse_errors,

    -- ** BatchEvaluateGeofences
    batchEvaluateGeofences_collectionName,
    batchEvaluateGeofences_devicePositionUpdates,
    batchEvaluateGeofencesResponse_httpStatus,
    batchEvaluateGeofencesResponse_errors,

    -- ** BatchGetDevicePosition
    batchGetDevicePosition_deviceIds,
    batchGetDevicePosition_trackerName,
    batchGetDevicePositionResponse_httpStatus,
    batchGetDevicePositionResponse_devicePositions,
    batchGetDevicePositionResponse_errors,

    -- ** BatchPutGeofence
    batchPutGeofence_collectionName,
    batchPutGeofence_entries,
    batchPutGeofenceResponse_httpStatus,
    batchPutGeofenceResponse_errors,
    batchPutGeofenceResponse_successes,

    -- ** BatchUpdateDevicePosition
    batchUpdateDevicePosition_trackerName,
    batchUpdateDevicePosition_updates,
    batchUpdateDevicePositionResponse_httpStatus,
    batchUpdateDevicePositionResponse_errors,

    -- ** CalculateRoute
    calculateRoute_carModeOptions,
    calculateRoute_departNow,
    calculateRoute_departureTime,
    calculateRoute_distanceUnit,
    calculateRoute_includeLegGeometry,
    calculateRoute_travelMode,
    calculateRoute_truckModeOptions,
    calculateRoute_waypointPositions,
    calculateRoute_calculatorName,
    calculateRoute_departurePosition,
    calculateRoute_destinationPosition,
    calculateRouteResponse_httpStatus,
    calculateRouteResponse_legs,
    calculateRouteResponse_summary,

    -- ** CalculateRouteMatrix
    calculateRouteMatrix_carModeOptions,
    calculateRouteMatrix_departNow,
    calculateRouteMatrix_departureTime,
    calculateRouteMatrix_distanceUnit,
    calculateRouteMatrix_travelMode,
    calculateRouteMatrix_truckModeOptions,
    calculateRouteMatrix_calculatorName,
    calculateRouteMatrix_departurePositions,
    calculateRouteMatrix_destinationPositions,
    calculateRouteMatrixResponse_snappedDeparturePositions,
    calculateRouteMatrixResponse_snappedDestinationPositions,
    calculateRouteMatrixResponse_httpStatus,
    calculateRouteMatrixResponse_routeMatrix,
    calculateRouteMatrixResponse_summary,

    -- ** CreateGeofenceCollection
    createGeofenceCollection_description,
    createGeofenceCollection_kmsKeyId,
    createGeofenceCollection_pricingPlan,
    createGeofenceCollection_pricingPlanDataSource,
    createGeofenceCollection_tags,
    createGeofenceCollection_collectionName,
    createGeofenceCollectionResponse_httpStatus,
    createGeofenceCollectionResponse_collectionArn,
    createGeofenceCollectionResponse_collectionName,
    createGeofenceCollectionResponse_createTime,

    -- ** CreateMap
    createMap_description,
    createMap_pricingPlan,
    createMap_tags,
    createMap_configuration,
    createMap_mapName,
    createMapResponse_httpStatus,
    createMapResponse_createTime,
    createMapResponse_mapArn,
    createMapResponse_mapName,

    -- ** CreatePlaceIndex
    createPlaceIndex_dataSourceConfiguration,
    createPlaceIndex_description,
    createPlaceIndex_pricingPlan,
    createPlaceIndex_tags,
    createPlaceIndex_dataSource,
    createPlaceIndex_indexName,
    createPlaceIndexResponse_httpStatus,
    createPlaceIndexResponse_createTime,
    createPlaceIndexResponse_indexArn,
    createPlaceIndexResponse_indexName,

    -- ** CreateRouteCalculator
    createRouteCalculator_description,
    createRouteCalculator_pricingPlan,
    createRouteCalculator_tags,
    createRouteCalculator_calculatorName,
    createRouteCalculator_dataSource,
    createRouteCalculatorResponse_httpStatus,
    createRouteCalculatorResponse_calculatorArn,
    createRouteCalculatorResponse_calculatorName,
    createRouteCalculatorResponse_createTime,

    -- ** CreateTracker
    createTracker_description,
    createTracker_kmsKeyId,
    createTracker_positionFiltering,
    createTracker_pricingPlan,
    createTracker_pricingPlanDataSource,
    createTracker_tags,
    createTracker_trackerName,
    createTrackerResponse_httpStatus,
    createTrackerResponse_createTime,
    createTrackerResponse_trackerArn,
    createTrackerResponse_trackerName,

    -- ** DeleteGeofenceCollection
    deleteGeofenceCollection_collectionName,
    deleteGeofenceCollectionResponse_httpStatus,

    -- ** DeleteMap
    deleteMap_mapName,
    deleteMapResponse_httpStatus,

    -- ** DeletePlaceIndex
    deletePlaceIndex_indexName,
    deletePlaceIndexResponse_httpStatus,

    -- ** DeleteRouteCalculator
    deleteRouteCalculator_calculatorName,
    deleteRouteCalculatorResponse_httpStatus,

    -- ** DeleteTracker
    deleteTracker_trackerName,
    deleteTrackerResponse_httpStatus,

    -- ** DescribeGeofenceCollection
    describeGeofenceCollection_collectionName,
    describeGeofenceCollectionResponse_kmsKeyId,
    describeGeofenceCollectionResponse_pricingPlan,
    describeGeofenceCollectionResponse_pricingPlanDataSource,
    describeGeofenceCollectionResponse_tags,
    describeGeofenceCollectionResponse_httpStatus,
    describeGeofenceCollectionResponse_collectionArn,
    describeGeofenceCollectionResponse_collectionName,
    describeGeofenceCollectionResponse_createTime,
    describeGeofenceCollectionResponse_description,
    describeGeofenceCollectionResponse_updateTime,

    -- ** DescribeMap
    describeMap_mapName,
    describeMapResponse_pricingPlan,
    describeMapResponse_tags,
    describeMapResponse_httpStatus,
    describeMapResponse_configuration,
    describeMapResponse_createTime,
    describeMapResponse_dataSource,
    describeMapResponse_description,
    describeMapResponse_mapArn,
    describeMapResponse_mapName,
    describeMapResponse_updateTime,

    -- ** DescribePlaceIndex
    describePlaceIndex_indexName,
    describePlaceIndexResponse_pricingPlan,
    describePlaceIndexResponse_tags,
    describePlaceIndexResponse_httpStatus,
    describePlaceIndexResponse_createTime,
    describePlaceIndexResponse_dataSource,
    describePlaceIndexResponse_dataSourceConfiguration,
    describePlaceIndexResponse_description,
    describePlaceIndexResponse_indexArn,
    describePlaceIndexResponse_indexName,
    describePlaceIndexResponse_updateTime,

    -- ** DescribeRouteCalculator
    describeRouteCalculator_calculatorName,
    describeRouteCalculatorResponse_pricingPlan,
    describeRouteCalculatorResponse_tags,
    describeRouteCalculatorResponse_httpStatus,
    describeRouteCalculatorResponse_calculatorArn,
    describeRouteCalculatorResponse_calculatorName,
    describeRouteCalculatorResponse_createTime,
    describeRouteCalculatorResponse_dataSource,
    describeRouteCalculatorResponse_description,
    describeRouteCalculatorResponse_updateTime,

    -- ** DescribeTracker
    describeTracker_trackerName,
    describeTrackerResponse_kmsKeyId,
    describeTrackerResponse_positionFiltering,
    describeTrackerResponse_pricingPlan,
    describeTrackerResponse_pricingPlanDataSource,
    describeTrackerResponse_tags,
    describeTrackerResponse_httpStatus,
    describeTrackerResponse_createTime,
    describeTrackerResponse_description,
    describeTrackerResponse_trackerArn,
    describeTrackerResponse_trackerName,
    describeTrackerResponse_updateTime,

    -- ** DisassociateTrackerConsumer
    disassociateTrackerConsumer_consumerArn,
    disassociateTrackerConsumer_trackerName,
    disassociateTrackerConsumerResponse_httpStatus,

    -- ** GetDevicePosition
    getDevicePosition_deviceId,
    getDevicePosition_trackerName,
    getDevicePositionResponse_accuracy,
    getDevicePositionResponse_deviceId,
    getDevicePositionResponse_positionProperties,
    getDevicePositionResponse_httpStatus,
    getDevicePositionResponse_position,
    getDevicePositionResponse_receivedTime,
    getDevicePositionResponse_sampleTime,

    -- ** GetDevicePositionHistory
    getDevicePositionHistory_endTimeExclusive,
    getDevicePositionHistory_maxResults,
    getDevicePositionHistory_nextToken,
    getDevicePositionHistory_startTimeInclusive,
    getDevicePositionHistory_deviceId,
    getDevicePositionHistory_trackerName,
    getDevicePositionHistoryResponse_nextToken,
    getDevicePositionHistoryResponse_httpStatus,
    getDevicePositionHistoryResponse_devicePositions,

    -- ** GetGeofence
    getGeofence_collectionName,
    getGeofence_geofenceId,
    getGeofenceResponse_httpStatus,
    getGeofenceResponse_createTime,
    getGeofenceResponse_geofenceId,
    getGeofenceResponse_geometry,
    getGeofenceResponse_status,
    getGeofenceResponse_updateTime,

    -- ** GetMapGlyphs
    getMapGlyphs_fontStack,
    getMapGlyphs_fontUnicodeRange,
    getMapGlyphs_mapName,
    getMapGlyphsResponse_blob,
    getMapGlyphsResponse_contentType,
    getMapGlyphsResponse_httpStatus,

    -- ** GetMapSprites
    getMapSprites_fileName,
    getMapSprites_mapName,
    getMapSpritesResponse_blob,
    getMapSpritesResponse_contentType,
    getMapSpritesResponse_httpStatus,

    -- ** GetMapStyleDescriptor
    getMapStyleDescriptor_mapName,
    getMapStyleDescriptorResponse_blob,
    getMapStyleDescriptorResponse_contentType,
    getMapStyleDescriptorResponse_httpStatus,

    -- ** GetMapTile
    getMapTile_mapName,
    getMapTile_x,
    getMapTile_y,
    getMapTile_z,
    getMapTileResponse_blob,
    getMapTileResponse_contentType,
    getMapTileResponse_httpStatus,

    -- ** GetPlace
    getPlace_language,
    getPlace_indexName,
    getPlace_placeId,
    getPlaceResponse_httpStatus,
    getPlaceResponse_place,

    -- ** ListDevicePositions
    listDevicePositions_maxResults,
    listDevicePositions_nextToken,
    listDevicePositions_trackerName,
    listDevicePositionsResponse_nextToken,
    listDevicePositionsResponse_httpStatus,
    listDevicePositionsResponse_entries,

    -- ** ListGeofenceCollections
    listGeofenceCollections_maxResults,
    listGeofenceCollections_nextToken,
    listGeofenceCollectionsResponse_nextToken,
    listGeofenceCollectionsResponse_httpStatus,
    listGeofenceCollectionsResponse_entries,

    -- ** ListGeofences
    listGeofences_maxResults,
    listGeofences_nextToken,
    listGeofences_collectionName,
    listGeofencesResponse_nextToken,
    listGeofencesResponse_httpStatus,
    listGeofencesResponse_entries,

    -- ** ListMaps
    listMaps_maxResults,
    listMaps_nextToken,
    listMapsResponse_nextToken,
    listMapsResponse_httpStatus,
    listMapsResponse_entries,

    -- ** ListPlaceIndexes
    listPlaceIndexes_maxResults,
    listPlaceIndexes_nextToken,
    listPlaceIndexesResponse_nextToken,
    listPlaceIndexesResponse_httpStatus,
    listPlaceIndexesResponse_entries,

    -- ** ListRouteCalculators
    listRouteCalculators_maxResults,
    listRouteCalculators_nextToken,
    listRouteCalculatorsResponse_nextToken,
    listRouteCalculatorsResponse_httpStatus,
    listRouteCalculatorsResponse_entries,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTrackerConsumers
    listTrackerConsumers_maxResults,
    listTrackerConsumers_nextToken,
    listTrackerConsumers_trackerName,
    listTrackerConsumersResponse_nextToken,
    listTrackerConsumersResponse_httpStatus,
    listTrackerConsumersResponse_consumerArns,

    -- ** ListTrackers
    listTrackers_maxResults,
    listTrackers_nextToken,
    listTrackersResponse_nextToken,
    listTrackersResponse_httpStatus,
    listTrackersResponse_entries,

    -- ** PutGeofence
    putGeofence_collectionName,
    putGeofence_geofenceId,
    putGeofence_geometry,
    putGeofenceResponse_httpStatus,
    putGeofenceResponse_createTime,
    putGeofenceResponse_geofenceId,
    putGeofenceResponse_updateTime,

    -- ** SearchPlaceIndexForPosition
    searchPlaceIndexForPosition_language,
    searchPlaceIndexForPosition_maxResults,
    searchPlaceIndexForPosition_indexName,
    searchPlaceIndexForPosition_position,
    searchPlaceIndexForPositionResponse_httpStatus,
    searchPlaceIndexForPositionResponse_results,
    searchPlaceIndexForPositionResponse_summary,

    -- ** SearchPlaceIndexForSuggestions
    searchPlaceIndexForSuggestions_biasPosition,
    searchPlaceIndexForSuggestions_filterBBox,
    searchPlaceIndexForSuggestions_filterCountries,
    searchPlaceIndexForSuggestions_language,
    searchPlaceIndexForSuggestions_maxResults,
    searchPlaceIndexForSuggestions_indexName,
    searchPlaceIndexForSuggestions_text,
    searchPlaceIndexForSuggestionsResponse_httpStatus,
    searchPlaceIndexForSuggestionsResponse_results,
    searchPlaceIndexForSuggestionsResponse_summary,

    -- ** SearchPlaceIndexForText
    searchPlaceIndexForText_biasPosition,
    searchPlaceIndexForText_filterBBox,
    searchPlaceIndexForText_filterCountries,
    searchPlaceIndexForText_language,
    searchPlaceIndexForText_maxResults,
    searchPlaceIndexForText_indexName,
    searchPlaceIndexForText_text,
    searchPlaceIndexForTextResponse_httpStatus,
    searchPlaceIndexForTextResponse_results,
    searchPlaceIndexForTextResponse_summary,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateGeofenceCollection
    updateGeofenceCollection_description,
    updateGeofenceCollection_pricingPlan,
    updateGeofenceCollection_pricingPlanDataSource,
    updateGeofenceCollection_collectionName,
    updateGeofenceCollectionResponse_httpStatus,
    updateGeofenceCollectionResponse_collectionArn,
    updateGeofenceCollectionResponse_collectionName,
    updateGeofenceCollectionResponse_updateTime,

    -- ** UpdateMap
    updateMap_description,
    updateMap_pricingPlan,
    updateMap_mapName,
    updateMapResponse_httpStatus,
    updateMapResponse_mapArn,
    updateMapResponse_mapName,
    updateMapResponse_updateTime,

    -- ** UpdatePlaceIndex
    updatePlaceIndex_dataSourceConfiguration,
    updatePlaceIndex_description,
    updatePlaceIndex_pricingPlan,
    updatePlaceIndex_indexName,
    updatePlaceIndexResponse_httpStatus,
    updatePlaceIndexResponse_indexArn,
    updatePlaceIndexResponse_indexName,
    updatePlaceIndexResponse_updateTime,

    -- ** UpdateRouteCalculator
    updateRouteCalculator_description,
    updateRouteCalculator_pricingPlan,
    updateRouteCalculator_calculatorName,
    updateRouteCalculatorResponse_httpStatus,
    updateRouteCalculatorResponse_calculatorArn,
    updateRouteCalculatorResponse_calculatorName,
    updateRouteCalculatorResponse_updateTime,

    -- ** UpdateTracker
    updateTracker_description,
    updateTracker_positionFiltering,
    updateTracker_pricingPlan,
    updateTracker_pricingPlanDataSource,
    updateTracker_trackerName,
    updateTrackerResponse_httpStatus,
    updateTrackerResponse_trackerArn,
    updateTrackerResponse_trackerName,
    updateTrackerResponse_updateTime,

    -- * Types

    -- ** BatchDeleteDevicePositionHistoryError
    batchDeleteDevicePositionHistoryError_deviceId,
    batchDeleteDevicePositionHistoryError_error,

    -- ** BatchDeleteGeofenceError
    batchDeleteGeofenceError_error,
    batchDeleteGeofenceError_geofenceId,

    -- ** BatchEvaluateGeofencesError
    batchEvaluateGeofencesError_deviceId,
    batchEvaluateGeofencesError_error,
    batchEvaluateGeofencesError_sampleTime,

    -- ** BatchGetDevicePositionError
    batchGetDevicePositionError_deviceId,
    batchGetDevicePositionError_error,

    -- ** BatchItemError
    batchItemError_code,
    batchItemError_message,

    -- ** BatchPutGeofenceError
    batchPutGeofenceError_error,
    batchPutGeofenceError_geofenceId,

    -- ** BatchPutGeofenceRequestEntry
    batchPutGeofenceRequestEntry_geofenceId,
    batchPutGeofenceRequestEntry_geometry,

    -- ** BatchPutGeofenceSuccess
    batchPutGeofenceSuccess_createTime,
    batchPutGeofenceSuccess_geofenceId,
    batchPutGeofenceSuccess_updateTime,

    -- ** BatchUpdateDevicePositionError
    batchUpdateDevicePositionError_deviceId,
    batchUpdateDevicePositionError_error,
    batchUpdateDevicePositionError_sampleTime,

    -- ** CalculateRouteCarModeOptions
    calculateRouteCarModeOptions_avoidFerries,
    calculateRouteCarModeOptions_avoidTolls,

    -- ** CalculateRouteMatrixSummary
    calculateRouteMatrixSummary_dataSource,
    calculateRouteMatrixSummary_distanceUnit,
    calculateRouteMatrixSummary_errorCount,
    calculateRouteMatrixSummary_routeCount,

    -- ** CalculateRouteSummary
    calculateRouteSummary_dataSource,
    calculateRouteSummary_distance,
    calculateRouteSummary_distanceUnit,
    calculateRouteSummary_durationSeconds,
    calculateRouteSummary_routeBBox,

    -- ** CalculateRouteTruckModeOptions
    calculateRouteTruckModeOptions_avoidFerries,
    calculateRouteTruckModeOptions_avoidTolls,
    calculateRouteTruckModeOptions_dimensions,
    calculateRouteTruckModeOptions_weight,

    -- ** Circle
    circle_center,
    circle_radius,

    -- ** DataSourceConfiguration
    dataSourceConfiguration_intendedUse,

    -- ** DevicePosition
    devicePosition_accuracy,
    devicePosition_deviceId,
    devicePosition_positionProperties,
    devicePosition_position,
    devicePosition_receivedTime,
    devicePosition_sampleTime,

    -- ** DevicePositionUpdate
    devicePositionUpdate_accuracy,
    devicePositionUpdate_positionProperties,
    devicePositionUpdate_deviceId,
    devicePositionUpdate_position,
    devicePositionUpdate_sampleTime,

    -- ** GeofenceGeometry
    geofenceGeometry_circle,
    geofenceGeometry_polygon,

    -- ** Leg
    leg_geometry,
    leg_distance,
    leg_durationSeconds,
    leg_endPosition,
    leg_startPosition,
    leg_steps,

    -- ** LegGeometry
    legGeometry_lineString,

    -- ** ListDevicePositionsResponseEntry
    listDevicePositionsResponseEntry_accuracy,
    listDevicePositionsResponseEntry_positionProperties,
    listDevicePositionsResponseEntry_deviceId,
    listDevicePositionsResponseEntry_position,
    listDevicePositionsResponseEntry_sampleTime,

    -- ** ListGeofenceCollectionsResponseEntry
    listGeofenceCollectionsResponseEntry_pricingPlan,
    listGeofenceCollectionsResponseEntry_pricingPlanDataSource,
    listGeofenceCollectionsResponseEntry_collectionName,
    listGeofenceCollectionsResponseEntry_createTime,
    listGeofenceCollectionsResponseEntry_description,
    listGeofenceCollectionsResponseEntry_updateTime,

    -- ** ListGeofenceResponseEntry
    listGeofenceResponseEntry_createTime,
    listGeofenceResponseEntry_geofenceId,
    listGeofenceResponseEntry_geometry,
    listGeofenceResponseEntry_status,
    listGeofenceResponseEntry_updateTime,

    -- ** ListMapsResponseEntry
    listMapsResponseEntry_pricingPlan,
    listMapsResponseEntry_createTime,
    listMapsResponseEntry_dataSource,
    listMapsResponseEntry_description,
    listMapsResponseEntry_mapName,
    listMapsResponseEntry_updateTime,

    -- ** ListPlaceIndexesResponseEntry
    listPlaceIndexesResponseEntry_pricingPlan,
    listPlaceIndexesResponseEntry_createTime,
    listPlaceIndexesResponseEntry_dataSource,
    listPlaceIndexesResponseEntry_description,
    listPlaceIndexesResponseEntry_indexName,
    listPlaceIndexesResponseEntry_updateTime,

    -- ** ListRouteCalculatorsResponseEntry
    listRouteCalculatorsResponseEntry_pricingPlan,
    listRouteCalculatorsResponseEntry_calculatorName,
    listRouteCalculatorsResponseEntry_createTime,
    listRouteCalculatorsResponseEntry_dataSource,
    listRouteCalculatorsResponseEntry_description,
    listRouteCalculatorsResponseEntry_updateTime,

    -- ** ListTrackersResponseEntry
    listTrackersResponseEntry_pricingPlan,
    listTrackersResponseEntry_pricingPlanDataSource,
    listTrackersResponseEntry_createTime,
    listTrackersResponseEntry_description,
    listTrackersResponseEntry_trackerName,
    listTrackersResponseEntry_updateTime,

    -- ** MapConfiguration
    mapConfiguration_style,

    -- ** Place
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

    -- ** PlaceGeometry
    placeGeometry_point,

    -- ** PositionalAccuracy
    positionalAccuracy_horizontal,

    -- ** RouteMatrixEntry
    routeMatrixEntry_distance,
    routeMatrixEntry_durationSeconds,
    routeMatrixEntry_error,

    -- ** RouteMatrixEntryError
    routeMatrixEntryError_message,
    routeMatrixEntryError_code,

    -- ** SearchForPositionResult
    searchForPositionResult_placeId,
    searchForPositionResult_distance,
    searchForPositionResult_place,

    -- ** SearchForSuggestionsResult
    searchForSuggestionsResult_placeId,
    searchForSuggestionsResult_text,

    -- ** SearchForTextResult
    searchForTextResult_distance,
    searchForTextResult_placeId,
    searchForTextResult_relevance,
    searchForTextResult_place,

    -- ** SearchPlaceIndexForPositionSummary
    searchPlaceIndexForPositionSummary_language,
    searchPlaceIndexForPositionSummary_maxResults,
    searchPlaceIndexForPositionSummary_dataSource,
    searchPlaceIndexForPositionSummary_position,

    -- ** SearchPlaceIndexForSuggestionsSummary
    searchPlaceIndexForSuggestionsSummary_biasPosition,
    searchPlaceIndexForSuggestionsSummary_filterBBox,
    searchPlaceIndexForSuggestionsSummary_filterCountries,
    searchPlaceIndexForSuggestionsSummary_language,
    searchPlaceIndexForSuggestionsSummary_maxResults,
    searchPlaceIndexForSuggestionsSummary_dataSource,
    searchPlaceIndexForSuggestionsSummary_text,

    -- ** SearchPlaceIndexForTextSummary
    searchPlaceIndexForTextSummary_biasPosition,
    searchPlaceIndexForTextSummary_filterBBox,
    searchPlaceIndexForTextSummary_filterCountries,
    searchPlaceIndexForTextSummary_language,
    searchPlaceIndexForTextSummary_maxResults,
    searchPlaceIndexForTextSummary_resultBBox,
    searchPlaceIndexForTextSummary_dataSource,
    searchPlaceIndexForTextSummary_text,

    -- ** Step
    step_geometryOffset,
    step_distance,
    step_durationSeconds,
    step_endPosition,
    step_startPosition,

    -- ** TimeZone
    timeZone_offset,
    timeZone_name,

    -- ** TruckDimensions
    truckDimensions_height,
    truckDimensions_length,
    truckDimensions_unit,
    truckDimensions_width,

    -- ** TruckWeight
    truckWeight_total,
    truckWeight_unit,
  )
where

import Amazonka.Location.AssociateTrackerConsumer
import Amazonka.Location.BatchDeleteDevicePositionHistory
import Amazonka.Location.BatchDeleteGeofence
import Amazonka.Location.BatchEvaluateGeofences
import Amazonka.Location.BatchGetDevicePosition
import Amazonka.Location.BatchPutGeofence
import Amazonka.Location.BatchUpdateDevicePosition
import Amazonka.Location.CalculateRoute
import Amazonka.Location.CalculateRouteMatrix
import Amazonka.Location.CreateGeofenceCollection
import Amazonka.Location.CreateMap
import Amazonka.Location.CreatePlaceIndex
import Amazonka.Location.CreateRouteCalculator
import Amazonka.Location.CreateTracker
import Amazonka.Location.DeleteGeofenceCollection
import Amazonka.Location.DeleteMap
import Amazonka.Location.DeletePlaceIndex
import Amazonka.Location.DeleteRouteCalculator
import Amazonka.Location.DeleteTracker
import Amazonka.Location.DescribeGeofenceCollection
import Amazonka.Location.DescribeMap
import Amazonka.Location.DescribePlaceIndex
import Amazonka.Location.DescribeRouteCalculator
import Amazonka.Location.DescribeTracker
import Amazonka.Location.DisassociateTrackerConsumer
import Amazonka.Location.GetDevicePosition
import Amazonka.Location.GetDevicePositionHistory
import Amazonka.Location.GetGeofence
import Amazonka.Location.GetMapGlyphs
import Amazonka.Location.GetMapSprites
import Amazonka.Location.GetMapStyleDescriptor
import Amazonka.Location.GetMapTile
import Amazonka.Location.GetPlace
import Amazonka.Location.ListDevicePositions
import Amazonka.Location.ListGeofenceCollections
import Amazonka.Location.ListGeofences
import Amazonka.Location.ListMaps
import Amazonka.Location.ListPlaceIndexes
import Amazonka.Location.ListRouteCalculators
import Amazonka.Location.ListTagsForResource
import Amazonka.Location.ListTrackerConsumers
import Amazonka.Location.ListTrackers
import Amazonka.Location.PutGeofence
import Amazonka.Location.SearchPlaceIndexForPosition
import Amazonka.Location.SearchPlaceIndexForSuggestions
import Amazonka.Location.SearchPlaceIndexForText
import Amazonka.Location.TagResource
import Amazonka.Location.Types.BatchDeleteDevicePositionHistoryError
import Amazonka.Location.Types.BatchDeleteGeofenceError
import Amazonka.Location.Types.BatchEvaluateGeofencesError
import Amazonka.Location.Types.BatchGetDevicePositionError
import Amazonka.Location.Types.BatchItemError
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
import Amazonka.Location.Types.GeofenceGeometry
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
import Amazonka.Location.Types.PositionalAccuracy
import Amazonka.Location.Types.RouteMatrixEntry
import Amazonka.Location.Types.RouteMatrixEntryError
import Amazonka.Location.Types.SearchForPositionResult
import Amazonka.Location.Types.SearchForSuggestionsResult
import Amazonka.Location.Types.SearchForTextResult
import Amazonka.Location.Types.SearchPlaceIndexForPositionSummary
import Amazonka.Location.Types.SearchPlaceIndexForSuggestionsSummary
import Amazonka.Location.Types.SearchPlaceIndexForTextSummary
import Amazonka.Location.Types.Step
import Amazonka.Location.Types.TimeZone
import Amazonka.Location.Types.TruckDimensions
import Amazonka.Location.Types.TruckWeight
import Amazonka.Location.UntagResource
import Amazonka.Location.UpdateGeofenceCollection
import Amazonka.Location.UpdateMap
import Amazonka.Location.UpdatePlaceIndex
import Amazonka.Location.UpdateRouteCalculator
import Amazonka.Location.UpdateTracker

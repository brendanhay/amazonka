{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Location
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-11-19@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- \"Suite of geospatial services including Maps, Places, Routes, Tracking,
-- and Geofencing\"
module Amazonka.Location
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateTrackerConsumer
    AssociateTrackerConsumer (AssociateTrackerConsumer'),
    newAssociateTrackerConsumer,
    AssociateTrackerConsumerResponse (AssociateTrackerConsumerResponse'),
    newAssociateTrackerConsumerResponse,

    -- ** BatchDeleteDevicePositionHistory
    BatchDeleteDevicePositionHistory (BatchDeleteDevicePositionHistory'),
    newBatchDeleteDevicePositionHistory,
    BatchDeleteDevicePositionHistoryResponse (BatchDeleteDevicePositionHistoryResponse'),
    newBatchDeleteDevicePositionHistoryResponse,

    -- ** BatchDeleteGeofence
    BatchDeleteGeofence (BatchDeleteGeofence'),
    newBatchDeleteGeofence,
    BatchDeleteGeofenceResponse (BatchDeleteGeofenceResponse'),
    newBatchDeleteGeofenceResponse,

    -- ** BatchEvaluateGeofences
    BatchEvaluateGeofences (BatchEvaluateGeofences'),
    newBatchEvaluateGeofences,
    BatchEvaluateGeofencesResponse (BatchEvaluateGeofencesResponse'),
    newBatchEvaluateGeofencesResponse,

    -- ** BatchGetDevicePosition
    BatchGetDevicePosition (BatchGetDevicePosition'),
    newBatchGetDevicePosition,
    BatchGetDevicePositionResponse (BatchGetDevicePositionResponse'),
    newBatchGetDevicePositionResponse,

    -- ** BatchPutGeofence
    BatchPutGeofence (BatchPutGeofence'),
    newBatchPutGeofence,
    BatchPutGeofenceResponse (BatchPutGeofenceResponse'),
    newBatchPutGeofenceResponse,

    -- ** BatchUpdateDevicePosition
    BatchUpdateDevicePosition (BatchUpdateDevicePosition'),
    newBatchUpdateDevicePosition,
    BatchUpdateDevicePositionResponse (BatchUpdateDevicePositionResponse'),
    newBatchUpdateDevicePositionResponse,

    -- ** CalculateRoute
    CalculateRoute (CalculateRoute'),
    newCalculateRoute,
    CalculateRouteResponse (CalculateRouteResponse'),
    newCalculateRouteResponse,

    -- ** CalculateRouteMatrix
    CalculateRouteMatrix (CalculateRouteMatrix'),
    newCalculateRouteMatrix,
    CalculateRouteMatrixResponse (CalculateRouteMatrixResponse'),
    newCalculateRouteMatrixResponse,

    -- ** CreateGeofenceCollection
    CreateGeofenceCollection (CreateGeofenceCollection'),
    newCreateGeofenceCollection,
    CreateGeofenceCollectionResponse (CreateGeofenceCollectionResponse'),
    newCreateGeofenceCollectionResponse,

    -- ** CreateMap
    CreateMap (CreateMap'),
    newCreateMap,
    CreateMapResponse (CreateMapResponse'),
    newCreateMapResponse,

    -- ** CreatePlaceIndex
    CreatePlaceIndex (CreatePlaceIndex'),
    newCreatePlaceIndex,
    CreatePlaceIndexResponse (CreatePlaceIndexResponse'),
    newCreatePlaceIndexResponse,

    -- ** CreateRouteCalculator
    CreateRouteCalculator (CreateRouteCalculator'),
    newCreateRouteCalculator,
    CreateRouteCalculatorResponse (CreateRouteCalculatorResponse'),
    newCreateRouteCalculatorResponse,

    -- ** CreateTracker
    CreateTracker (CreateTracker'),
    newCreateTracker,
    CreateTrackerResponse (CreateTrackerResponse'),
    newCreateTrackerResponse,

    -- ** DeleteGeofenceCollection
    DeleteGeofenceCollection (DeleteGeofenceCollection'),
    newDeleteGeofenceCollection,
    DeleteGeofenceCollectionResponse (DeleteGeofenceCollectionResponse'),
    newDeleteGeofenceCollectionResponse,

    -- ** DeleteMap
    DeleteMap (DeleteMap'),
    newDeleteMap,
    DeleteMapResponse (DeleteMapResponse'),
    newDeleteMapResponse,

    -- ** DeletePlaceIndex
    DeletePlaceIndex (DeletePlaceIndex'),
    newDeletePlaceIndex,
    DeletePlaceIndexResponse (DeletePlaceIndexResponse'),
    newDeletePlaceIndexResponse,

    -- ** DeleteRouteCalculator
    DeleteRouteCalculator (DeleteRouteCalculator'),
    newDeleteRouteCalculator,
    DeleteRouteCalculatorResponse (DeleteRouteCalculatorResponse'),
    newDeleteRouteCalculatorResponse,

    -- ** DeleteTracker
    DeleteTracker (DeleteTracker'),
    newDeleteTracker,
    DeleteTrackerResponse (DeleteTrackerResponse'),
    newDeleteTrackerResponse,

    -- ** DescribeGeofenceCollection
    DescribeGeofenceCollection (DescribeGeofenceCollection'),
    newDescribeGeofenceCollection,
    DescribeGeofenceCollectionResponse (DescribeGeofenceCollectionResponse'),
    newDescribeGeofenceCollectionResponse,

    -- ** DescribeMap
    DescribeMap (DescribeMap'),
    newDescribeMap,
    DescribeMapResponse (DescribeMapResponse'),
    newDescribeMapResponse,

    -- ** DescribePlaceIndex
    DescribePlaceIndex (DescribePlaceIndex'),
    newDescribePlaceIndex,
    DescribePlaceIndexResponse (DescribePlaceIndexResponse'),
    newDescribePlaceIndexResponse,

    -- ** DescribeRouteCalculator
    DescribeRouteCalculator (DescribeRouteCalculator'),
    newDescribeRouteCalculator,
    DescribeRouteCalculatorResponse (DescribeRouteCalculatorResponse'),
    newDescribeRouteCalculatorResponse,

    -- ** DescribeTracker
    DescribeTracker (DescribeTracker'),
    newDescribeTracker,
    DescribeTrackerResponse (DescribeTrackerResponse'),
    newDescribeTrackerResponse,

    -- ** DisassociateTrackerConsumer
    DisassociateTrackerConsumer (DisassociateTrackerConsumer'),
    newDisassociateTrackerConsumer,
    DisassociateTrackerConsumerResponse (DisassociateTrackerConsumerResponse'),
    newDisassociateTrackerConsumerResponse,

    -- ** GetDevicePosition
    GetDevicePosition (GetDevicePosition'),
    newGetDevicePosition,
    GetDevicePositionResponse (GetDevicePositionResponse'),
    newGetDevicePositionResponse,

    -- ** GetDevicePositionHistory (Paginated)
    GetDevicePositionHistory (GetDevicePositionHistory'),
    newGetDevicePositionHistory,
    GetDevicePositionHistoryResponse (GetDevicePositionHistoryResponse'),
    newGetDevicePositionHistoryResponse,

    -- ** GetGeofence
    GetGeofence (GetGeofence'),
    newGetGeofence,
    GetGeofenceResponse (GetGeofenceResponse'),
    newGetGeofenceResponse,

    -- ** GetMapGlyphs
    GetMapGlyphs (GetMapGlyphs'),
    newGetMapGlyphs,
    GetMapGlyphsResponse (GetMapGlyphsResponse'),
    newGetMapGlyphsResponse,

    -- ** GetMapSprites
    GetMapSprites (GetMapSprites'),
    newGetMapSprites,
    GetMapSpritesResponse (GetMapSpritesResponse'),
    newGetMapSpritesResponse,

    -- ** GetMapStyleDescriptor
    GetMapStyleDescriptor (GetMapStyleDescriptor'),
    newGetMapStyleDescriptor,
    GetMapStyleDescriptorResponse (GetMapStyleDescriptorResponse'),
    newGetMapStyleDescriptorResponse,

    -- ** GetMapTile
    GetMapTile (GetMapTile'),
    newGetMapTile,
    GetMapTileResponse (GetMapTileResponse'),
    newGetMapTileResponse,

    -- ** GetPlace
    GetPlace (GetPlace'),
    newGetPlace,
    GetPlaceResponse (GetPlaceResponse'),
    newGetPlaceResponse,

    -- ** ListDevicePositions (Paginated)
    ListDevicePositions (ListDevicePositions'),
    newListDevicePositions,
    ListDevicePositionsResponse (ListDevicePositionsResponse'),
    newListDevicePositionsResponse,

    -- ** ListGeofenceCollections (Paginated)
    ListGeofenceCollections (ListGeofenceCollections'),
    newListGeofenceCollections,
    ListGeofenceCollectionsResponse (ListGeofenceCollectionsResponse'),
    newListGeofenceCollectionsResponse,

    -- ** ListGeofences (Paginated)
    ListGeofences (ListGeofences'),
    newListGeofences,
    ListGeofencesResponse (ListGeofencesResponse'),
    newListGeofencesResponse,

    -- ** ListMaps (Paginated)
    ListMaps (ListMaps'),
    newListMaps,
    ListMapsResponse (ListMapsResponse'),
    newListMapsResponse,

    -- ** ListPlaceIndexes (Paginated)
    ListPlaceIndexes (ListPlaceIndexes'),
    newListPlaceIndexes,
    ListPlaceIndexesResponse (ListPlaceIndexesResponse'),
    newListPlaceIndexesResponse,

    -- ** ListRouteCalculators (Paginated)
    ListRouteCalculators (ListRouteCalculators'),
    newListRouteCalculators,
    ListRouteCalculatorsResponse (ListRouteCalculatorsResponse'),
    newListRouteCalculatorsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTrackerConsumers (Paginated)
    ListTrackerConsumers (ListTrackerConsumers'),
    newListTrackerConsumers,
    ListTrackerConsumersResponse (ListTrackerConsumersResponse'),
    newListTrackerConsumersResponse,

    -- ** ListTrackers (Paginated)
    ListTrackers (ListTrackers'),
    newListTrackers,
    ListTrackersResponse (ListTrackersResponse'),
    newListTrackersResponse,

    -- ** PutGeofence
    PutGeofence (PutGeofence'),
    newPutGeofence,
    PutGeofenceResponse (PutGeofenceResponse'),
    newPutGeofenceResponse,

    -- ** SearchPlaceIndexForPosition
    SearchPlaceIndexForPosition (SearchPlaceIndexForPosition'),
    newSearchPlaceIndexForPosition,
    SearchPlaceIndexForPositionResponse (SearchPlaceIndexForPositionResponse'),
    newSearchPlaceIndexForPositionResponse,

    -- ** SearchPlaceIndexForSuggestions
    SearchPlaceIndexForSuggestions (SearchPlaceIndexForSuggestions'),
    newSearchPlaceIndexForSuggestions,
    SearchPlaceIndexForSuggestionsResponse (SearchPlaceIndexForSuggestionsResponse'),
    newSearchPlaceIndexForSuggestionsResponse,

    -- ** SearchPlaceIndexForText
    SearchPlaceIndexForText (SearchPlaceIndexForText'),
    newSearchPlaceIndexForText,
    SearchPlaceIndexForTextResponse (SearchPlaceIndexForTextResponse'),
    newSearchPlaceIndexForTextResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateGeofenceCollection
    UpdateGeofenceCollection (UpdateGeofenceCollection'),
    newUpdateGeofenceCollection,
    UpdateGeofenceCollectionResponse (UpdateGeofenceCollectionResponse'),
    newUpdateGeofenceCollectionResponse,

    -- ** UpdateMap
    UpdateMap (UpdateMap'),
    newUpdateMap,
    UpdateMapResponse (UpdateMapResponse'),
    newUpdateMapResponse,

    -- ** UpdatePlaceIndex
    UpdatePlaceIndex (UpdatePlaceIndex'),
    newUpdatePlaceIndex,
    UpdatePlaceIndexResponse (UpdatePlaceIndexResponse'),
    newUpdatePlaceIndexResponse,

    -- ** UpdateRouteCalculator
    UpdateRouteCalculator (UpdateRouteCalculator'),
    newUpdateRouteCalculator,
    UpdateRouteCalculatorResponse (UpdateRouteCalculatorResponse'),
    newUpdateRouteCalculatorResponse,

    -- ** UpdateTracker
    UpdateTracker (UpdateTracker'),
    newUpdateTracker,
    UpdateTrackerResponse (UpdateTrackerResponse'),
    newUpdateTrackerResponse,

    -- * Types

    -- ** BatchItemErrorCode
    BatchItemErrorCode (..),

    -- ** DimensionUnit
    DimensionUnit (..),

    -- ** DistanceUnit
    DistanceUnit (..),

    -- ** IntendedUse
    IntendedUse (..),

    -- ** PositionFiltering
    PositionFiltering (..),

    -- ** PricingPlan
    PricingPlan (..),

    -- ** RouteMatrixErrorCode
    RouteMatrixErrorCode (..),

    -- ** TravelMode
    TravelMode (..),

    -- ** VehicleWeightUnit
    VehicleWeightUnit (..),

    -- ** BatchDeleteDevicePositionHistoryError
    BatchDeleteDevicePositionHistoryError (BatchDeleteDevicePositionHistoryError'),
    newBatchDeleteDevicePositionHistoryError,

    -- ** BatchDeleteGeofenceError
    BatchDeleteGeofenceError (BatchDeleteGeofenceError'),
    newBatchDeleteGeofenceError,

    -- ** BatchEvaluateGeofencesError
    BatchEvaluateGeofencesError (BatchEvaluateGeofencesError'),
    newBatchEvaluateGeofencesError,

    -- ** BatchGetDevicePositionError
    BatchGetDevicePositionError (BatchGetDevicePositionError'),
    newBatchGetDevicePositionError,

    -- ** BatchItemError
    BatchItemError (BatchItemError'),
    newBatchItemError,

    -- ** BatchPutGeofenceError
    BatchPutGeofenceError (BatchPutGeofenceError'),
    newBatchPutGeofenceError,

    -- ** BatchPutGeofenceRequestEntry
    BatchPutGeofenceRequestEntry (BatchPutGeofenceRequestEntry'),
    newBatchPutGeofenceRequestEntry,

    -- ** BatchPutGeofenceSuccess
    BatchPutGeofenceSuccess (BatchPutGeofenceSuccess'),
    newBatchPutGeofenceSuccess,

    -- ** BatchUpdateDevicePositionError
    BatchUpdateDevicePositionError (BatchUpdateDevicePositionError'),
    newBatchUpdateDevicePositionError,

    -- ** CalculateRouteCarModeOptions
    CalculateRouteCarModeOptions (CalculateRouteCarModeOptions'),
    newCalculateRouteCarModeOptions,

    -- ** CalculateRouteMatrixSummary
    CalculateRouteMatrixSummary (CalculateRouteMatrixSummary'),
    newCalculateRouteMatrixSummary,

    -- ** CalculateRouteSummary
    CalculateRouteSummary (CalculateRouteSummary'),
    newCalculateRouteSummary,

    -- ** CalculateRouteTruckModeOptions
    CalculateRouteTruckModeOptions (CalculateRouteTruckModeOptions'),
    newCalculateRouteTruckModeOptions,

    -- ** Circle
    Circle (Circle'),
    newCircle,

    -- ** DataSourceConfiguration
    DataSourceConfiguration (DataSourceConfiguration'),
    newDataSourceConfiguration,

    -- ** DevicePosition
    DevicePosition (DevicePosition'),
    newDevicePosition,

    -- ** DevicePositionUpdate
    DevicePositionUpdate (DevicePositionUpdate'),
    newDevicePositionUpdate,

    -- ** GeofenceGeometry
    GeofenceGeometry (GeofenceGeometry'),
    newGeofenceGeometry,

    -- ** Leg
    Leg (Leg'),
    newLeg,

    -- ** LegGeometry
    LegGeometry (LegGeometry'),
    newLegGeometry,

    -- ** ListDevicePositionsResponseEntry
    ListDevicePositionsResponseEntry (ListDevicePositionsResponseEntry'),
    newListDevicePositionsResponseEntry,

    -- ** ListGeofenceCollectionsResponseEntry
    ListGeofenceCollectionsResponseEntry (ListGeofenceCollectionsResponseEntry'),
    newListGeofenceCollectionsResponseEntry,

    -- ** ListGeofenceResponseEntry
    ListGeofenceResponseEntry (ListGeofenceResponseEntry'),
    newListGeofenceResponseEntry,

    -- ** ListMapsResponseEntry
    ListMapsResponseEntry (ListMapsResponseEntry'),
    newListMapsResponseEntry,

    -- ** ListPlaceIndexesResponseEntry
    ListPlaceIndexesResponseEntry (ListPlaceIndexesResponseEntry'),
    newListPlaceIndexesResponseEntry,

    -- ** ListRouteCalculatorsResponseEntry
    ListRouteCalculatorsResponseEntry (ListRouteCalculatorsResponseEntry'),
    newListRouteCalculatorsResponseEntry,

    -- ** ListTrackersResponseEntry
    ListTrackersResponseEntry (ListTrackersResponseEntry'),
    newListTrackersResponseEntry,

    -- ** MapConfiguration
    MapConfiguration (MapConfiguration'),
    newMapConfiguration,

    -- ** Place
    Place (Place'),
    newPlace,

    -- ** PlaceGeometry
    PlaceGeometry (PlaceGeometry'),
    newPlaceGeometry,

    -- ** PositionalAccuracy
    PositionalAccuracy (PositionalAccuracy'),
    newPositionalAccuracy,

    -- ** RouteMatrixEntry
    RouteMatrixEntry (RouteMatrixEntry'),
    newRouteMatrixEntry,

    -- ** RouteMatrixEntryError
    RouteMatrixEntryError (RouteMatrixEntryError'),
    newRouteMatrixEntryError,

    -- ** SearchForPositionResult
    SearchForPositionResult (SearchForPositionResult'),
    newSearchForPositionResult,

    -- ** SearchForSuggestionsResult
    SearchForSuggestionsResult (SearchForSuggestionsResult'),
    newSearchForSuggestionsResult,

    -- ** SearchForTextResult
    SearchForTextResult (SearchForTextResult'),
    newSearchForTextResult,

    -- ** SearchPlaceIndexForPositionSummary
    SearchPlaceIndexForPositionSummary (SearchPlaceIndexForPositionSummary'),
    newSearchPlaceIndexForPositionSummary,

    -- ** SearchPlaceIndexForSuggestionsSummary
    SearchPlaceIndexForSuggestionsSummary (SearchPlaceIndexForSuggestionsSummary'),
    newSearchPlaceIndexForSuggestionsSummary,

    -- ** SearchPlaceIndexForTextSummary
    SearchPlaceIndexForTextSummary (SearchPlaceIndexForTextSummary'),
    newSearchPlaceIndexForTextSummary,

    -- ** Step
    Step (Step'),
    newStep,

    -- ** TimeZone
    TimeZone (TimeZone'),
    newTimeZone,

    -- ** TruckDimensions
    TruckDimensions (TruckDimensions'),
    newTruckDimensions,

    -- ** TruckWeight
    TruckWeight (TruckWeight'),
    newTruckWeight,
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
import Amazonka.Location.Lens
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
import Amazonka.Location.Types
import Amazonka.Location.UntagResource
import Amazonka.Location.UpdateGeofenceCollection
import Amazonka.Location.UpdateMap
import Amazonka.Location.UpdatePlaceIndex
import Amazonka.Location.UpdateRouteCalculator
import Amazonka.Location.UpdateTracker
import Amazonka.Location.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Location'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

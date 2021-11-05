{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Location
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-11-19@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Suite of geospatial services including Maps, Places, Routes, Tracking,
-- and Geofencing
module Network.AWS.Location
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchUpdateDevicePosition
    BatchUpdateDevicePosition (BatchUpdateDevicePosition'),
    newBatchUpdateDevicePosition,
    BatchUpdateDevicePositionResponse (BatchUpdateDevicePositionResponse'),
    newBatchUpdateDevicePositionResponse,

    -- ** DeleteRouteCalculator
    DeleteRouteCalculator (DeleteRouteCalculator'),
    newDeleteRouteCalculator,
    DeleteRouteCalculatorResponse (DeleteRouteCalculatorResponse'),
    newDeleteRouteCalculatorResponse,

    -- ** UpdateRouteCalculator
    UpdateRouteCalculator (UpdateRouteCalculator'),
    newUpdateRouteCalculator,
    UpdateRouteCalculatorResponse (UpdateRouteCalculatorResponse'),
    newUpdateRouteCalculatorResponse,

    -- ** CreateGeofenceCollection
    CreateGeofenceCollection (CreateGeofenceCollection'),
    newCreateGeofenceCollection,
    CreateGeofenceCollectionResponse (CreateGeofenceCollectionResponse'),
    newCreateGeofenceCollectionResponse,

    -- ** ListRouteCalculators (Paginated)
    ListRouteCalculators (ListRouteCalculators'),
    newListRouteCalculators,
    ListRouteCalculatorsResponse (ListRouteCalculatorsResponse'),
    newListRouteCalculatorsResponse,

    -- ** CreateTracker
    CreateTracker (CreateTracker'),
    newCreateTracker,
    CreateTrackerResponse (CreateTrackerResponse'),
    newCreateTrackerResponse,

    -- ** ListTrackerConsumers (Paginated)
    ListTrackerConsumers (ListTrackerConsumers'),
    newListTrackerConsumers,
    ListTrackerConsumersResponse (ListTrackerConsumersResponse'),
    newListTrackerConsumersResponse,

    -- ** GetDevicePosition
    GetDevicePosition (GetDevicePosition'),
    newGetDevicePosition,
    GetDevicePositionResponse (GetDevicePositionResponse'),
    newGetDevicePositionResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** SearchPlaceIndexForText
    SearchPlaceIndexForText (SearchPlaceIndexForText'),
    newSearchPlaceIndexForText,
    SearchPlaceIndexForTextResponse (SearchPlaceIndexForTextResponse'),
    newSearchPlaceIndexForTextResponse,

    -- ** DescribeGeofenceCollection
    DescribeGeofenceCollection (DescribeGeofenceCollection'),
    newDescribeGeofenceCollection,
    DescribeGeofenceCollectionResponse (DescribeGeofenceCollectionResponse'),
    newDescribeGeofenceCollectionResponse,

    -- ** ListMaps (Paginated)
    ListMaps (ListMaps'),
    newListMaps,
    ListMapsResponse (ListMapsResponse'),
    newListMapsResponse,

    -- ** GetMapSprites
    GetMapSprites (GetMapSprites'),
    newGetMapSprites,
    GetMapSpritesResponse (GetMapSpritesResponse'),
    newGetMapSpritesResponse,

    -- ** CreateMap
    CreateMap (CreateMap'),
    newCreateMap,
    CreateMapResponse (CreateMapResponse'),
    newCreateMapResponse,

    -- ** DescribeRouteCalculator
    DescribeRouteCalculator (DescribeRouteCalculator'),
    newDescribeRouteCalculator,
    DescribeRouteCalculatorResponse (DescribeRouteCalculatorResponse'),
    newDescribeRouteCalculatorResponse,

    -- ** BatchGetDevicePosition
    BatchGetDevicePosition (BatchGetDevicePosition'),
    newBatchGetDevicePosition,
    BatchGetDevicePositionResponse (BatchGetDevicePositionResponse'),
    newBatchGetDevicePositionResponse,

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

    -- ** ListGeofenceCollections (Paginated)
    ListGeofenceCollections (ListGeofenceCollections'),
    newListGeofenceCollections,
    ListGeofenceCollectionsResponse (ListGeofenceCollectionsResponse'),
    newListGeofenceCollectionsResponse,

    -- ** DeleteGeofenceCollection
    DeleteGeofenceCollection (DeleteGeofenceCollection'),
    newDeleteGeofenceCollection,
    DeleteGeofenceCollectionResponse (DeleteGeofenceCollectionResponse'),
    newDeleteGeofenceCollectionResponse,

    -- ** UpdateGeofenceCollection
    UpdateGeofenceCollection (UpdateGeofenceCollection'),
    newUpdateGeofenceCollection,
    UpdateGeofenceCollectionResponse (UpdateGeofenceCollectionResponse'),
    newUpdateGeofenceCollectionResponse,

    -- ** ListTrackers (Paginated)
    ListTrackers (ListTrackers'),
    newListTrackers,
    ListTrackersResponse (ListTrackersResponse'),
    newListTrackersResponse,

    -- ** DeletePlaceIndex
    DeletePlaceIndex (DeletePlaceIndex'),
    newDeletePlaceIndex,
    DeletePlaceIndexResponse (DeletePlaceIndexResponse'),
    newDeletePlaceIndexResponse,

    -- ** UpdatePlaceIndex
    UpdatePlaceIndex (UpdatePlaceIndex'),
    newUpdatePlaceIndex,
    UpdatePlaceIndexResponse (UpdatePlaceIndexResponse'),
    newUpdatePlaceIndexResponse,

    -- ** DisassociateTrackerConsumer
    DisassociateTrackerConsumer (DisassociateTrackerConsumer'),
    newDisassociateTrackerConsumer,
    DisassociateTrackerConsumerResponse (DisassociateTrackerConsumerResponse'),
    newDisassociateTrackerConsumerResponse,

    -- ** PutGeofence
    PutGeofence (PutGeofence'),
    newPutGeofence,
    PutGeofenceResponse (PutGeofenceResponse'),
    newPutGeofenceResponse,

    -- ** ListGeofences (Paginated)
    ListGeofences (ListGeofences'),
    newListGeofences,
    ListGeofencesResponse (ListGeofencesResponse'),
    newListGeofencesResponse,

    -- ** AssociateTrackerConsumer
    AssociateTrackerConsumer (AssociateTrackerConsumer'),
    newAssociateTrackerConsumer,
    AssociateTrackerConsumerResponse (AssociateTrackerConsumerResponse'),
    newAssociateTrackerConsumerResponse,

    -- ** BatchEvaluateGeofences
    BatchEvaluateGeofences (BatchEvaluateGeofences'),
    newBatchEvaluateGeofences,
    BatchEvaluateGeofencesResponse (BatchEvaluateGeofencesResponse'),
    newBatchEvaluateGeofencesResponse,

    -- ** CalculateRoute
    CalculateRoute (CalculateRoute'),
    newCalculateRoute,
    CalculateRouteResponse (CalculateRouteResponse'),
    newCalculateRouteResponse,

    -- ** DeleteMap
    DeleteMap (DeleteMap'),
    newDeleteMap,
    DeleteMapResponse (DeleteMapResponse'),
    newDeleteMapResponse,

    -- ** UpdateMap
    UpdateMap (UpdateMap'),
    newUpdateMap,
    UpdateMapResponse (UpdateMapResponse'),
    newUpdateMapResponse,

    -- ** GetDevicePositionHistory (Paginated)
    GetDevicePositionHistory (GetDevicePositionHistory'),
    newGetDevicePositionHistory,
    GetDevicePositionHistoryResponse (GetDevicePositionHistoryResponse'),
    newGetDevicePositionHistoryResponse,

    -- ** DescribeTracker
    DescribeTracker (DescribeTracker'),
    newDescribeTracker,
    DescribeTrackerResponse (DescribeTrackerResponse'),
    newDescribeTrackerResponse,

    -- ** DescribePlaceIndex
    DescribePlaceIndex (DescribePlaceIndex'),
    newDescribePlaceIndex,
    DescribePlaceIndexResponse (DescribePlaceIndexResponse'),
    newDescribePlaceIndexResponse,

    -- ** GetGeofence
    GetGeofence (GetGeofence'),
    newGetGeofence,
    GetGeofenceResponse (GetGeofenceResponse'),
    newGetGeofenceResponse,

    -- ** ListDevicePositions (Paginated)
    ListDevicePositions (ListDevicePositions'),
    newListDevicePositions,
    ListDevicePositionsResponse (ListDevicePositionsResponse'),
    newListDevicePositionsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetMapGlyphs
    GetMapGlyphs (GetMapGlyphs'),
    newGetMapGlyphs,
    GetMapGlyphsResponse (GetMapGlyphsResponse'),
    newGetMapGlyphsResponse,

    -- ** BatchPutGeofence
    BatchPutGeofence (BatchPutGeofence'),
    newBatchPutGeofence,
    BatchPutGeofenceResponse (BatchPutGeofenceResponse'),
    newBatchPutGeofenceResponse,

    -- ** BatchDeleteGeofence
    BatchDeleteGeofence (BatchDeleteGeofence'),
    newBatchDeleteGeofence,
    BatchDeleteGeofenceResponse (BatchDeleteGeofenceResponse'),
    newBatchDeleteGeofenceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** BatchDeleteDevicePositionHistory
    BatchDeleteDevicePositionHistory (BatchDeleteDevicePositionHistory'),
    newBatchDeleteDevicePositionHistory,
    BatchDeleteDevicePositionHistoryResponse (BatchDeleteDevicePositionHistoryResponse'),
    newBatchDeleteDevicePositionHistoryResponse,

    -- ** ListPlaceIndexes (Paginated)
    ListPlaceIndexes (ListPlaceIndexes'),
    newListPlaceIndexes,
    ListPlaceIndexesResponse (ListPlaceIndexesResponse'),
    newListPlaceIndexesResponse,

    -- ** SearchPlaceIndexForPosition
    SearchPlaceIndexForPosition (SearchPlaceIndexForPosition'),
    newSearchPlaceIndexForPosition,
    SearchPlaceIndexForPositionResponse (SearchPlaceIndexForPositionResponse'),
    newSearchPlaceIndexForPositionResponse,

    -- ** DeleteTracker
    DeleteTracker (DeleteTracker'),
    newDeleteTracker,
    DeleteTrackerResponse (DeleteTrackerResponse'),
    newDeleteTrackerResponse,

    -- ** CreatePlaceIndex
    CreatePlaceIndex (CreatePlaceIndex'),
    newCreatePlaceIndex,
    CreatePlaceIndexResponse (CreatePlaceIndexResponse'),
    newCreatePlaceIndexResponse,

    -- ** UpdateTracker
    UpdateTracker (UpdateTracker'),
    newUpdateTracker,
    UpdateTrackerResponse (UpdateTrackerResponse'),
    newUpdateTrackerResponse,

    -- ** CreateRouteCalculator
    CreateRouteCalculator (CreateRouteCalculator'),
    newCreateRouteCalculator,
    CreateRouteCalculatorResponse (CreateRouteCalculatorResponse'),
    newCreateRouteCalculatorResponse,

    -- ** DescribeMap
    DescribeMap (DescribeMap'),
    newDescribeMap,
    DescribeMapResponse (DescribeMapResponse'),
    newDescribeMapResponse,

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

    -- ** CalculateRouteSummary
    CalculateRouteSummary (CalculateRouteSummary'),
    newCalculateRouteSummary,

    -- ** CalculateRouteTruckModeOptions
    CalculateRouteTruckModeOptions (CalculateRouteTruckModeOptions'),
    newCalculateRouteTruckModeOptions,

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

    -- ** SearchForPositionResult
    SearchForPositionResult (SearchForPositionResult'),
    newSearchForPositionResult,

    -- ** SearchForTextResult
    SearchForTextResult (SearchForTextResult'),
    newSearchForTextResult,

    -- ** SearchPlaceIndexForPositionSummary
    SearchPlaceIndexForPositionSummary (SearchPlaceIndexForPositionSummary'),
    newSearchPlaceIndexForPositionSummary,

    -- ** SearchPlaceIndexForTextSummary
    SearchPlaceIndexForTextSummary (SearchPlaceIndexForTextSummary'),
    newSearchPlaceIndexForTextSummary,

    -- ** Step
    Step (Step'),
    newStep,

    -- ** TruckDimensions
    TruckDimensions (TruckDimensions'),
    newTruckDimensions,

    -- ** TruckWeight
    TruckWeight (TruckWeight'),
    newTruckWeight,
  )
where

import Network.AWS.Location.AssociateTrackerConsumer
import Network.AWS.Location.BatchDeleteDevicePositionHistory
import Network.AWS.Location.BatchDeleteGeofence
import Network.AWS.Location.BatchEvaluateGeofences
import Network.AWS.Location.BatchGetDevicePosition
import Network.AWS.Location.BatchPutGeofence
import Network.AWS.Location.BatchUpdateDevicePosition
import Network.AWS.Location.CalculateRoute
import Network.AWS.Location.CreateGeofenceCollection
import Network.AWS.Location.CreateMap
import Network.AWS.Location.CreatePlaceIndex
import Network.AWS.Location.CreateRouteCalculator
import Network.AWS.Location.CreateTracker
import Network.AWS.Location.DeleteGeofenceCollection
import Network.AWS.Location.DeleteMap
import Network.AWS.Location.DeletePlaceIndex
import Network.AWS.Location.DeleteRouteCalculator
import Network.AWS.Location.DeleteTracker
import Network.AWS.Location.DescribeGeofenceCollection
import Network.AWS.Location.DescribeMap
import Network.AWS.Location.DescribePlaceIndex
import Network.AWS.Location.DescribeRouteCalculator
import Network.AWS.Location.DescribeTracker
import Network.AWS.Location.DisassociateTrackerConsumer
import Network.AWS.Location.GetDevicePosition
import Network.AWS.Location.GetDevicePositionHistory
import Network.AWS.Location.GetGeofence
import Network.AWS.Location.GetMapGlyphs
import Network.AWS.Location.GetMapSprites
import Network.AWS.Location.GetMapStyleDescriptor
import Network.AWS.Location.GetMapTile
import Network.AWS.Location.Lens
import Network.AWS.Location.ListDevicePositions
import Network.AWS.Location.ListGeofenceCollections
import Network.AWS.Location.ListGeofences
import Network.AWS.Location.ListMaps
import Network.AWS.Location.ListPlaceIndexes
import Network.AWS.Location.ListRouteCalculators
import Network.AWS.Location.ListTagsForResource
import Network.AWS.Location.ListTrackerConsumers
import Network.AWS.Location.ListTrackers
import Network.AWS.Location.PutGeofence
import Network.AWS.Location.SearchPlaceIndexForPosition
import Network.AWS.Location.SearchPlaceIndexForText
import Network.AWS.Location.TagResource
import Network.AWS.Location.Types
import Network.AWS.Location.UntagResource
import Network.AWS.Location.UpdateGeofenceCollection
import Network.AWS.Location.UpdateMap
import Network.AWS.Location.UpdatePlaceIndex
import Network.AWS.Location.UpdateRouteCalculator
import Network.AWS.Location.UpdateTracker
import Network.AWS.Location.Waiters

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

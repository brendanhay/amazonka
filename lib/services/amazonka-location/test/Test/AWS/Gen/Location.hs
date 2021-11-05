{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Location
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Location where

import Amazonka.Location
import qualified Data.Proxy as Proxy
import Test.AWS.Fixture
import Test.AWS.Location.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchUpdateDevicePosition $
--             newBatchUpdateDevicePosition
--
--         , requestDeleteRouteCalculator $
--             newDeleteRouteCalculator
--
--         , requestUpdateRouteCalculator $
--             newUpdateRouteCalculator
--
--         , requestCreateGeofenceCollection $
--             newCreateGeofenceCollection
--
--         , requestListRouteCalculators $
--             newListRouteCalculators
--
--         , requestCreateTracker $
--             newCreateTracker
--
--         , requestListTrackerConsumers $
--             newListTrackerConsumers
--
--         , requestGetDevicePosition $
--             newGetDevicePosition
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestSearchPlaceIndexForText $
--             newSearchPlaceIndexForText
--
--         , requestDescribeGeofenceCollection $
--             newDescribeGeofenceCollection
--
--         , requestListMaps $
--             newListMaps
--
--         , requestGetMapSprites $
--             newGetMapSprites
--
--         , requestCreateMap $
--             newCreateMap
--
--         , requestDescribeRouteCalculator $
--             newDescribeRouteCalculator
--
--         , requestBatchGetDevicePosition $
--             newBatchGetDevicePosition
--
--         , requestGetMapStyleDescriptor $
--             newGetMapStyleDescriptor
--
--         , requestGetMapTile $
--             newGetMapTile
--
--         , requestListGeofenceCollections $
--             newListGeofenceCollections
--
--         , requestDeleteGeofenceCollection $
--             newDeleteGeofenceCollection
--
--         , requestUpdateGeofenceCollection $
--             newUpdateGeofenceCollection
--
--         , requestListTrackers $
--             newListTrackers
--
--         , requestDeletePlaceIndex $
--             newDeletePlaceIndex
--
--         , requestUpdatePlaceIndex $
--             newUpdatePlaceIndex
--
--         , requestDisassociateTrackerConsumer $
--             newDisassociateTrackerConsumer
--
--         , requestPutGeofence $
--             newPutGeofence
--
--         , requestListGeofences $
--             newListGeofences
--
--         , requestAssociateTrackerConsumer $
--             newAssociateTrackerConsumer
--
--         , requestBatchEvaluateGeofences $
--             newBatchEvaluateGeofences
--
--         , requestCalculateRoute $
--             newCalculateRoute
--
--         , requestDeleteMap $
--             newDeleteMap
--
--         , requestUpdateMap $
--             newUpdateMap
--
--         , requestGetDevicePositionHistory $
--             newGetDevicePositionHistory
--
--         , requestDescribeTracker $
--             newDescribeTracker
--
--         , requestDescribePlaceIndex $
--             newDescribePlaceIndex
--
--         , requestGetGeofence $
--             newGetGeofence
--
--         , requestListDevicePositions $
--             newListDevicePositions
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetMapGlyphs $
--             newGetMapGlyphs
--
--         , requestBatchPutGeofence $
--             newBatchPutGeofence
--
--         , requestBatchDeleteGeofence $
--             newBatchDeleteGeofence
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestBatchDeleteDevicePositionHistory $
--             newBatchDeleteDevicePositionHistory
--
--         , requestListPlaceIndexes $
--             newListPlaceIndexes
--
--         , requestSearchPlaceIndexForPosition $
--             newSearchPlaceIndexForPosition
--
--         , requestDeleteTracker $
--             newDeleteTracker
--
--         , requestCreatePlaceIndex $
--             newCreatePlaceIndex
--
--         , requestUpdateTracker $
--             newUpdateTracker
--
--         , requestCreateRouteCalculator $
--             newCreateRouteCalculator
--
--         , requestDescribeMap $
--             newDescribeMap
--
--           ]

--     , testGroup "response"
--         [ responseBatchUpdateDevicePosition $
--             newBatchUpdateDevicePositionResponse
--
--         , responseDeleteRouteCalculator $
--             newDeleteRouteCalculatorResponse
--
--         , responseUpdateRouteCalculator $
--             newUpdateRouteCalculatorResponse
--
--         , responseCreateGeofenceCollection $
--             newCreateGeofenceCollectionResponse
--
--         , responseListRouteCalculators $
--             newListRouteCalculatorsResponse
--
--         , responseCreateTracker $
--             newCreateTrackerResponse
--
--         , responseListTrackerConsumers $
--             newListTrackerConsumersResponse
--
--         , responseGetDevicePosition $
--             newGetDevicePositionResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseSearchPlaceIndexForText $
--             newSearchPlaceIndexForTextResponse
--
--         , responseDescribeGeofenceCollection $
--             newDescribeGeofenceCollectionResponse
--
--         , responseListMaps $
--             newListMapsResponse
--
--         , responseGetMapSprites $
--             newGetMapSpritesResponse
--
--         , responseCreateMap $
--             newCreateMapResponse
--
--         , responseDescribeRouteCalculator $
--             newDescribeRouteCalculatorResponse
--
--         , responseBatchGetDevicePosition $
--             newBatchGetDevicePositionResponse
--
--         , responseGetMapStyleDescriptor $
--             newGetMapStyleDescriptorResponse
--
--         , responseGetMapTile $
--             newGetMapTileResponse
--
--         , responseListGeofenceCollections $
--             newListGeofenceCollectionsResponse
--
--         , responseDeleteGeofenceCollection $
--             newDeleteGeofenceCollectionResponse
--
--         , responseUpdateGeofenceCollection $
--             newUpdateGeofenceCollectionResponse
--
--         , responseListTrackers $
--             newListTrackersResponse
--
--         , responseDeletePlaceIndex $
--             newDeletePlaceIndexResponse
--
--         , responseUpdatePlaceIndex $
--             newUpdatePlaceIndexResponse
--
--         , responseDisassociateTrackerConsumer $
--             newDisassociateTrackerConsumerResponse
--
--         , responsePutGeofence $
--             newPutGeofenceResponse
--
--         , responseListGeofences $
--             newListGeofencesResponse
--
--         , responseAssociateTrackerConsumer $
--             newAssociateTrackerConsumerResponse
--
--         , responseBatchEvaluateGeofences $
--             newBatchEvaluateGeofencesResponse
--
--         , responseCalculateRoute $
--             newCalculateRouteResponse
--
--         , responseDeleteMap $
--             newDeleteMapResponse
--
--         , responseUpdateMap $
--             newUpdateMapResponse
--
--         , responseGetDevicePositionHistory $
--             newGetDevicePositionHistoryResponse
--
--         , responseDescribeTracker $
--             newDescribeTrackerResponse
--
--         , responseDescribePlaceIndex $
--             newDescribePlaceIndexResponse
--
--         , responseGetGeofence $
--             newGetGeofenceResponse
--
--         , responseListDevicePositions $
--             newListDevicePositionsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetMapGlyphs $
--             newGetMapGlyphsResponse
--
--         , responseBatchPutGeofence $
--             newBatchPutGeofenceResponse
--
--         , responseBatchDeleteGeofence $
--             newBatchDeleteGeofenceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseBatchDeleteDevicePositionHistory $
--             newBatchDeleteDevicePositionHistoryResponse
--
--         , responseListPlaceIndexes $
--             newListPlaceIndexesResponse
--
--         , responseSearchPlaceIndexForPosition $
--             newSearchPlaceIndexForPositionResponse
--
--         , responseDeleteTracker $
--             newDeleteTrackerResponse
--
--         , responseCreatePlaceIndex $
--             newCreatePlaceIndexResponse
--
--         , responseUpdateTracker $
--             newUpdateTrackerResponse
--
--         , responseCreateRouteCalculator $
--             newCreateRouteCalculatorResponse
--
--         , responseDescribeMap $
--             newDescribeMapResponse
--
--           ]
--     ]

-- Requests

requestBatchUpdateDevicePosition :: BatchUpdateDevicePosition -> TestTree
requestBatchUpdateDevicePosition =
  req
    "BatchUpdateDevicePosition"
    "fixture/BatchUpdateDevicePosition.yaml"

requestDeleteRouteCalculator :: DeleteRouteCalculator -> TestTree
requestDeleteRouteCalculator =
  req
    "DeleteRouteCalculator"
    "fixture/DeleteRouteCalculator.yaml"

requestUpdateRouteCalculator :: UpdateRouteCalculator -> TestTree
requestUpdateRouteCalculator =
  req
    "UpdateRouteCalculator"
    "fixture/UpdateRouteCalculator.yaml"

requestCreateGeofenceCollection :: CreateGeofenceCollection -> TestTree
requestCreateGeofenceCollection =
  req
    "CreateGeofenceCollection"
    "fixture/CreateGeofenceCollection.yaml"

requestListRouteCalculators :: ListRouteCalculators -> TestTree
requestListRouteCalculators =
  req
    "ListRouteCalculators"
    "fixture/ListRouteCalculators.yaml"

requestCreateTracker :: CreateTracker -> TestTree
requestCreateTracker =
  req
    "CreateTracker"
    "fixture/CreateTracker.yaml"

requestListTrackerConsumers :: ListTrackerConsumers -> TestTree
requestListTrackerConsumers =
  req
    "ListTrackerConsumers"
    "fixture/ListTrackerConsumers.yaml"

requestGetDevicePosition :: GetDevicePosition -> TestTree
requestGetDevicePosition =
  req
    "GetDevicePosition"
    "fixture/GetDevicePosition.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestSearchPlaceIndexForText :: SearchPlaceIndexForText -> TestTree
requestSearchPlaceIndexForText =
  req
    "SearchPlaceIndexForText"
    "fixture/SearchPlaceIndexForText.yaml"

requestDescribeGeofenceCollection :: DescribeGeofenceCollection -> TestTree
requestDescribeGeofenceCollection =
  req
    "DescribeGeofenceCollection"
    "fixture/DescribeGeofenceCollection.yaml"

requestListMaps :: ListMaps -> TestTree
requestListMaps =
  req
    "ListMaps"
    "fixture/ListMaps.yaml"

requestGetMapSprites :: GetMapSprites -> TestTree
requestGetMapSprites =
  req
    "GetMapSprites"
    "fixture/GetMapSprites.yaml"

requestCreateMap :: CreateMap -> TestTree
requestCreateMap =
  req
    "CreateMap"
    "fixture/CreateMap.yaml"

requestDescribeRouteCalculator :: DescribeRouteCalculator -> TestTree
requestDescribeRouteCalculator =
  req
    "DescribeRouteCalculator"
    "fixture/DescribeRouteCalculator.yaml"

requestBatchGetDevicePosition :: BatchGetDevicePosition -> TestTree
requestBatchGetDevicePosition =
  req
    "BatchGetDevicePosition"
    "fixture/BatchGetDevicePosition.yaml"

requestGetMapStyleDescriptor :: GetMapStyleDescriptor -> TestTree
requestGetMapStyleDescriptor =
  req
    "GetMapStyleDescriptor"
    "fixture/GetMapStyleDescriptor.yaml"

requestGetMapTile :: GetMapTile -> TestTree
requestGetMapTile =
  req
    "GetMapTile"
    "fixture/GetMapTile.yaml"

requestListGeofenceCollections :: ListGeofenceCollections -> TestTree
requestListGeofenceCollections =
  req
    "ListGeofenceCollections"
    "fixture/ListGeofenceCollections.yaml"

requestDeleteGeofenceCollection :: DeleteGeofenceCollection -> TestTree
requestDeleteGeofenceCollection =
  req
    "DeleteGeofenceCollection"
    "fixture/DeleteGeofenceCollection.yaml"

requestUpdateGeofenceCollection :: UpdateGeofenceCollection -> TestTree
requestUpdateGeofenceCollection =
  req
    "UpdateGeofenceCollection"
    "fixture/UpdateGeofenceCollection.yaml"

requestListTrackers :: ListTrackers -> TestTree
requestListTrackers =
  req
    "ListTrackers"
    "fixture/ListTrackers.yaml"

requestDeletePlaceIndex :: DeletePlaceIndex -> TestTree
requestDeletePlaceIndex =
  req
    "DeletePlaceIndex"
    "fixture/DeletePlaceIndex.yaml"

requestUpdatePlaceIndex :: UpdatePlaceIndex -> TestTree
requestUpdatePlaceIndex =
  req
    "UpdatePlaceIndex"
    "fixture/UpdatePlaceIndex.yaml"

requestDisassociateTrackerConsumer :: DisassociateTrackerConsumer -> TestTree
requestDisassociateTrackerConsumer =
  req
    "DisassociateTrackerConsumer"
    "fixture/DisassociateTrackerConsumer.yaml"

requestPutGeofence :: PutGeofence -> TestTree
requestPutGeofence =
  req
    "PutGeofence"
    "fixture/PutGeofence.yaml"

requestListGeofences :: ListGeofences -> TestTree
requestListGeofences =
  req
    "ListGeofences"
    "fixture/ListGeofences.yaml"

requestAssociateTrackerConsumer :: AssociateTrackerConsumer -> TestTree
requestAssociateTrackerConsumer =
  req
    "AssociateTrackerConsumer"
    "fixture/AssociateTrackerConsumer.yaml"

requestBatchEvaluateGeofences :: BatchEvaluateGeofences -> TestTree
requestBatchEvaluateGeofences =
  req
    "BatchEvaluateGeofences"
    "fixture/BatchEvaluateGeofences.yaml"

requestCalculateRoute :: CalculateRoute -> TestTree
requestCalculateRoute =
  req
    "CalculateRoute"
    "fixture/CalculateRoute.yaml"

requestDeleteMap :: DeleteMap -> TestTree
requestDeleteMap =
  req
    "DeleteMap"
    "fixture/DeleteMap.yaml"

requestUpdateMap :: UpdateMap -> TestTree
requestUpdateMap =
  req
    "UpdateMap"
    "fixture/UpdateMap.yaml"

requestGetDevicePositionHistory :: GetDevicePositionHistory -> TestTree
requestGetDevicePositionHistory =
  req
    "GetDevicePositionHistory"
    "fixture/GetDevicePositionHistory.yaml"

requestDescribeTracker :: DescribeTracker -> TestTree
requestDescribeTracker =
  req
    "DescribeTracker"
    "fixture/DescribeTracker.yaml"

requestDescribePlaceIndex :: DescribePlaceIndex -> TestTree
requestDescribePlaceIndex =
  req
    "DescribePlaceIndex"
    "fixture/DescribePlaceIndex.yaml"

requestGetGeofence :: GetGeofence -> TestTree
requestGetGeofence =
  req
    "GetGeofence"
    "fixture/GetGeofence.yaml"

requestListDevicePositions :: ListDevicePositions -> TestTree
requestListDevicePositions =
  req
    "ListDevicePositions"
    "fixture/ListDevicePositions.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetMapGlyphs :: GetMapGlyphs -> TestTree
requestGetMapGlyphs =
  req
    "GetMapGlyphs"
    "fixture/GetMapGlyphs.yaml"

requestBatchPutGeofence :: BatchPutGeofence -> TestTree
requestBatchPutGeofence =
  req
    "BatchPutGeofence"
    "fixture/BatchPutGeofence.yaml"

requestBatchDeleteGeofence :: BatchDeleteGeofence -> TestTree
requestBatchDeleteGeofence =
  req
    "BatchDeleteGeofence"
    "fixture/BatchDeleteGeofence.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestBatchDeleteDevicePositionHistory :: BatchDeleteDevicePositionHistory -> TestTree
requestBatchDeleteDevicePositionHistory =
  req
    "BatchDeleteDevicePositionHistory"
    "fixture/BatchDeleteDevicePositionHistory.yaml"

requestListPlaceIndexes :: ListPlaceIndexes -> TestTree
requestListPlaceIndexes =
  req
    "ListPlaceIndexes"
    "fixture/ListPlaceIndexes.yaml"

requestSearchPlaceIndexForPosition :: SearchPlaceIndexForPosition -> TestTree
requestSearchPlaceIndexForPosition =
  req
    "SearchPlaceIndexForPosition"
    "fixture/SearchPlaceIndexForPosition.yaml"

requestDeleteTracker :: DeleteTracker -> TestTree
requestDeleteTracker =
  req
    "DeleteTracker"
    "fixture/DeleteTracker.yaml"

requestCreatePlaceIndex :: CreatePlaceIndex -> TestTree
requestCreatePlaceIndex =
  req
    "CreatePlaceIndex"
    "fixture/CreatePlaceIndex.yaml"

requestUpdateTracker :: UpdateTracker -> TestTree
requestUpdateTracker =
  req
    "UpdateTracker"
    "fixture/UpdateTracker.yaml"

requestCreateRouteCalculator :: CreateRouteCalculator -> TestTree
requestCreateRouteCalculator =
  req
    "CreateRouteCalculator"
    "fixture/CreateRouteCalculator.yaml"

requestDescribeMap :: DescribeMap -> TestTree
requestDescribeMap =
  req
    "DescribeMap"
    "fixture/DescribeMap.yaml"

-- Responses

responseBatchUpdateDevicePosition :: BatchUpdateDevicePositionResponse -> TestTree
responseBatchUpdateDevicePosition =
  res
    "BatchUpdateDevicePositionResponse"
    "fixture/BatchUpdateDevicePositionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpdateDevicePosition)

responseDeleteRouteCalculator :: DeleteRouteCalculatorResponse -> TestTree
responseDeleteRouteCalculator =
  res
    "DeleteRouteCalculatorResponse"
    "fixture/DeleteRouteCalculatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRouteCalculator)

responseUpdateRouteCalculator :: UpdateRouteCalculatorResponse -> TestTree
responseUpdateRouteCalculator =
  res
    "UpdateRouteCalculatorResponse"
    "fixture/UpdateRouteCalculatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRouteCalculator)

responseCreateGeofenceCollection :: CreateGeofenceCollectionResponse -> TestTree
responseCreateGeofenceCollection =
  res
    "CreateGeofenceCollectionResponse"
    "fixture/CreateGeofenceCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGeofenceCollection)

responseListRouteCalculators :: ListRouteCalculatorsResponse -> TestTree
responseListRouteCalculators =
  res
    "ListRouteCalculatorsResponse"
    "fixture/ListRouteCalculatorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRouteCalculators)

responseCreateTracker :: CreateTrackerResponse -> TestTree
responseCreateTracker =
  res
    "CreateTrackerResponse"
    "fixture/CreateTrackerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTracker)

responseListTrackerConsumers :: ListTrackerConsumersResponse -> TestTree
responseListTrackerConsumers =
  res
    "ListTrackerConsumersResponse"
    "fixture/ListTrackerConsumersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrackerConsumers)

responseGetDevicePosition :: GetDevicePositionResponse -> TestTree
responseGetDevicePosition =
  res
    "GetDevicePositionResponse"
    "fixture/GetDevicePositionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevicePosition)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseSearchPlaceIndexForText :: SearchPlaceIndexForTextResponse -> TestTree
responseSearchPlaceIndexForText =
  res
    "SearchPlaceIndexForTextResponse"
    "fixture/SearchPlaceIndexForTextResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchPlaceIndexForText)

responseDescribeGeofenceCollection :: DescribeGeofenceCollectionResponse -> TestTree
responseDescribeGeofenceCollection =
  res
    "DescribeGeofenceCollectionResponse"
    "fixture/DescribeGeofenceCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGeofenceCollection)

responseListMaps :: ListMapsResponse -> TestTree
responseListMaps =
  res
    "ListMapsResponse"
    "fixture/ListMapsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMaps)

responseGetMapSprites :: GetMapSpritesResponse -> TestTree
responseGetMapSprites =
  res
    "GetMapSpritesResponse"
    "fixture/GetMapSpritesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMapSprites)

responseCreateMap :: CreateMapResponse -> TestTree
responseCreateMap =
  res
    "CreateMapResponse"
    "fixture/CreateMapResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMap)

responseDescribeRouteCalculator :: DescribeRouteCalculatorResponse -> TestTree
responseDescribeRouteCalculator =
  res
    "DescribeRouteCalculatorResponse"
    "fixture/DescribeRouteCalculatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRouteCalculator)

responseBatchGetDevicePosition :: BatchGetDevicePositionResponse -> TestTree
responseBatchGetDevicePosition =
  res
    "BatchGetDevicePositionResponse"
    "fixture/BatchGetDevicePositionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetDevicePosition)

responseGetMapStyleDescriptor :: GetMapStyleDescriptorResponse -> TestTree
responseGetMapStyleDescriptor =
  res
    "GetMapStyleDescriptorResponse"
    "fixture/GetMapStyleDescriptorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMapStyleDescriptor)

responseGetMapTile :: GetMapTileResponse -> TestTree
responseGetMapTile =
  res
    "GetMapTileResponse"
    "fixture/GetMapTileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMapTile)

responseListGeofenceCollections :: ListGeofenceCollectionsResponse -> TestTree
responseListGeofenceCollections =
  res
    "ListGeofenceCollectionsResponse"
    "fixture/ListGeofenceCollectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGeofenceCollections)

responseDeleteGeofenceCollection :: DeleteGeofenceCollectionResponse -> TestTree
responseDeleteGeofenceCollection =
  res
    "DeleteGeofenceCollectionResponse"
    "fixture/DeleteGeofenceCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGeofenceCollection)

responseUpdateGeofenceCollection :: UpdateGeofenceCollectionResponse -> TestTree
responseUpdateGeofenceCollection =
  res
    "UpdateGeofenceCollectionResponse"
    "fixture/UpdateGeofenceCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGeofenceCollection)

responseListTrackers :: ListTrackersResponse -> TestTree
responseListTrackers =
  res
    "ListTrackersResponse"
    "fixture/ListTrackersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrackers)

responseDeletePlaceIndex :: DeletePlaceIndexResponse -> TestTree
responseDeletePlaceIndex =
  res
    "DeletePlaceIndexResponse"
    "fixture/DeletePlaceIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePlaceIndex)

responseUpdatePlaceIndex :: UpdatePlaceIndexResponse -> TestTree
responseUpdatePlaceIndex =
  res
    "UpdatePlaceIndexResponse"
    "fixture/UpdatePlaceIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePlaceIndex)

responseDisassociateTrackerConsumer :: DisassociateTrackerConsumerResponse -> TestTree
responseDisassociateTrackerConsumer =
  res
    "DisassociateTrackerConsumerResponse"
    "fixture/DisassociateTrackerConsumerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateTrackerConsumer)

responsePutGeofence :: PutGeofenceResponse -> TestTree
responsePutGeofence =
  res
    "PutGeofenceResponse"
    "fixture/PutGeofenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutGeofence)

responseListGeofences :: ListGeofencesResponse -> TestTree
responseListGeofences =
  res
    "ListGeofencesResponse"
    "fixture/ListGeofencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGeofences)

responseAssociateTrackerConsumer :: AssociateTrackerConsumerResponse -> TestTree
responseAssociateTrackerConsumer =
  res
    "AssociateTrackerConsumerResponse"
    "fixture/AssociateTrackerConsumerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateTrackerConsumer)

responseBatchEvaluateGeofences :: BatchEvaluateGeofencesResponse -> TestTree
responseBatchEvaluateGeofences =
  res
    "BatchEvaluateGeofencesResponse"
    "fixture/BatchEvaluateGeofencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchEvaluateGeofences)

responseCalculateRoute :: CalculateRouteResponse -> TestTree
responseCalculateRoute =
  res
    "CalculateRouteResponse"
    "fixture/CalculateRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CalculateRoute)

responseDeleteMap :: DeleteMapResponse -> TestTree
responseDeleteMap =
  res
    "DeleteMapResponse"
    "fixture/DeleteMapResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMap)

responseUpdateMap :: UpdateMapResponse -> TestTree
responseUpdateMap =
  res
    "UpdateMapResponse"
    "fixture/UpdateMapResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMap)

responseGetDevicePositionHistory :: GetDevicePositionHistoryResponse -> TestTree
responseGetDevicePositionHistory =
  res
    "GetDevicePositionHistoryResponse"
    "fixture/GetDevicePositionHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevicePositionHistory)

responseDescribeTracker :: DescribeTrackerResponse -> TestTree
responseDescribeTracker =
  res
    "DescribeTrackerResponse"
    "fixture/DescribeTrackerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTracker)

responseDescribePlaceIndex :: DescribePlaceIndexResponse -> TestTree
responseDescribePlaceIndex =
  res
    "DescribePlaceIndexResponse"
    "fixture/DescribePlaceIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePlaceIndex)

responseGetGeofence :: GetGeofenceResponse -> TestTree
responseGetGeofence =
  res
    "GetGeofenceResponse"
    "fixture/GetGeofenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGeofence)

responseListDevicePositions :: ListDevicePositionsResponse -> TestTree
responseListDevicePositions =
  res
    "ListDevicePositionsResponse"
    "fixture/ListDevicePositionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevicePositions)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseGetMapGlyphs :: GetMapGlyphsResponse -> TestTree
responseGetMapGlyphs =
  res
    "GetMapGlyphsResponse"
    "fixture/GetMapGlyphsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMapGlyphs)

responseBatchPutGeofence :: BatchPutGeofenceResponse -> TestTree
responseBatchPutGeofence =
  res
    "BatchPutGeofenceResponse"
    "fixture/BatchPutGeofenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchPutGeofence)

responseBatchDeleteGeofence :: BatchDeleteGeofenceResponse -> TestTree
responseBatchDeleteGeofence =
  res
    "BatchDeleteGeofenceResponse"
    "fixture/BatchDeleteGeofenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteGeofence)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseBatchDeleteDevicePositionHistory :: BatchDeleteDevicePositionHistoryResponse -> TestTree
responseBatchDeleteDevicePositionHistory =
  res
    "BatchDeleteDevicePositionHistoryResponse"
    "fixture/BatchDeleteDevicePositionHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteDevicePositionHistory)

responseListPlaceIndexes :: ListPlaceIndexesResponse -> TestTree
responseListPlaceIndexes =
  res
    "ListPlaceIndexesResponse"
    "fixture/ListPlaceIndexesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPlaceIndexes)

responseSearchPlaceIndexForPosition :: SearchPlaceIndexForPositionResponse -> TestTree
responseSearchPlaceIndexForPosition =
  res
    "SearchPlaceIndexForPositionResponse"
    "fixture/SearchPlaceIndexForPositionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchPlaceIndexForPosition)

responseDeleteTracker :: DeleteTrackerResponse -> TestTree
responseDeleteTracker =
  res
    "DeleteTrackerResponse"
    "fixture/DeleteTrackerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTracker)

responseCreatePlaceIndex :: CreatePlaceIndexResponse -> TestTree
responseCreatePlaceIndex =
  res
    "CreatePlaceIndexResponse"
    "fixture/CreatePlaceIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePlaceIndex)

responseUpdateTracker :: UpdateTrackerResponse -> TestTree
responseUpdateTracker =
  res
    "UpdateTrackerResponse"
    "fixture/UpdateTrackerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTracker)

responseCreateRouteCalculator :: CreateRouteCalculatorResponse -> TestTree
responseCreateRouteCalculator =
  res
    "CreateRouteCalculatorResponse"
    "fixture/CreateRouteCalculatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRouteCalculator)

responseDescribeMap :: DescribeMapResponse -> TestTree
responseDescribeMap =
  res
    "DescribeMapResponse"
    "fixture/DescribeMapResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMap)

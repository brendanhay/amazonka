{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Location
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Location where

import Amazonka.Location
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Location.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateTrackerConsumer $
--             newAssociateTrackerConsumer
--
--         , requestBatchDeleteDevicePositionHistory $
--             newBatchDeleteDevicePositionHistory
--
--         , requestBatchDeleteGeofence $
--             newBatchDeleteGeofence
--
--         , requestBatchEvaluateGeofences $
--             newBatchEvaluateGeofences
--
--         , requestBatchGetDevicePosition $
--             newBatchGetDevicePosition
--
--         , requestBatchPutGeofence $
--             newBatchPutGeofence
--
--         , requestBatchUpdateDevicePosition $
--             newBatchUpdateDevicePosition
--
--         , requestCalculateRoute $
--             newCalculateRoute
--
--         , requestCalculateRouteMatrix $
--             newCalculateRouteMatrix
--
--         , requestCreateGeofenceCollection $
--             newCreateGeofenceCollection
--
--         , requestCreateMap $
--             newCreateMap
--
--         , requestCreatePlaceIndex $
--             newCreatePlaceIndex
--
--         , requestCreateRouteCalculator $
--             newCreateRouteCalculator
--
--         , requestCreateTracker $
--             newCreateTracker
--
--         , requestDeleteGeofenceCollection $
--             newDeleteGeofenceCollection
--
--         , requestDeleteMap $
--             newDeleteMap
--
--         , requestDeletePlaceIndex $
--             newDeletePlaceIndex
--
--         , requestDeleteRouteCalculator $
--             newDeleteRouteCalculator
--
--         , requestDeleteTracker $
--             newDeleteTracker
--
--         , requestDescribeGeofenceCollection $
--             newDescribeGeofenceCollection
--
--         , requestDescribeMap $
--             newDescribeMap
--
--         , requestDescribePlaceIndex $
--             newDescribePlaceIndex
--
--         , requestDescribeRouteCalculator $
--             newDescribeRouteCalculator
--
--         , requestDescribeTracker $
--             newDescribeTracker
--
--         , requestDisassociateTrackerConsumer $
--             newDisassociateTrackerConsumer
--
--         , requestGetDevicePosition $
--             newGetDevicePosition
--
--         , requestGetDevicePositionHistory $
--             newGetDevicePositionHistory
--
--         , requestGetGeofence $
--             newGetGeofence
--
--         , requestGetMapGlyphs $
--             newGetMapGlyphs
--
--         , requestGetMapSprites $
--             newGetMapSprites
--
--         , requestGetMapStyleDescriptor $
--             newGetMapStyleDescriptor
--
--         , requestGetMapTile $
--             newGetMapTile
--
--         , requestGetPlace $
--             newGetPlace
--
--         , requestListDevicePositions $
--             newListDevicePositions
--
--         , requestListGeofenceCollections $
--             newListGeofenceCollections
--
--         , requestListGeofences $
--             newListGeofences
--
--         , requestListMaps $
--             newListMaps
--
--         , requestListPlaceIndexes $
--             newListPlaceIndexes
--
--         , requestListRouteCalculators $
--             newListRouteCalculators
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTrackerConsumers $
--             newListTrackerConsumers
--
--         , requestListTrackers $
--             newListTrackers
--
--         , requestPutGeofence $
--             newPutGeofence
--
--         , requestSearchPlaceIndexForPosition $
--             newSearchPlaceIndexForPosition
--
--         , requestSearchPlaceIndexForSuggestions $
--             newSearchPlaceIndexForSuggestions
--
--         , requestSearchPlaceIndexForText $
--             newSearchPlaceIndexForText
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateGeofenceCollection $
--             newUpdateGeofenceCollection
--
--         , requestUpdateMap $
--             newUpdateMap
--
--         , requestUpdatePlaceIndex $
--             newUpdatePlaceIndex
--
--         , requestUpdateRouteCalculator $
--             newUpdateRouteCalculator
--
--         , requestUpdateTracker $
--             newUpdateTracker
--
--           ]

--     , testGroup "response"
--         [ responseAssociateTrackerConsumer $
--             newAssociateTrackerConsumerResponse
--
--         , responseBatchDeleteDevicePositionHistory $
--             newBatchDeleteDevicePositionHistoryResponse
--
--         , responseBatchDeleteGeofence $
--             newBatchDeleteGeofenceResponse
--
--         , responseBatchEvaluateGeofences $
--             newBatchEvaluateGeofencesResponse
--
--         , responseBatchGetDevicePosition $
--             newBatchGetDevicePositionResponse
--
--         , responseBatchPutGeofence $
--             newBatchPutGeofenceResponse
--
--         , responseBatchUpdateDevicePosition $
--             newBatchUpdateDevicePositionResponse
--
--         , responseCalculateRoute $
--             newCalculateRouteResponse
--
--         , responseCalculateRouteMatrix $
--             newCalculateRouteMatrixResponse
--
--         , responseCreateGeofenceCollection $
--             newCreateGeofenceCollectionResponse
--
--         , responseCreateMap $
--             newCreateMapResponse
--
--         , responseCreatePlaceIndex $
--             newCreatePlaceIndexResponse
--
--         , responseCreateRouteCalculator $
--             newCreateRouteCalculatorResponse
--
--         , responseCreateTracker $
--             newCreateTrackerResponse
--
--         , responseDeleteGeofenceCollection $
--             newDeleteGeofenceCollectionResponse
--
--         , responseDeleteMap $
--             newDeleteMapResponse
--
--         , responseDeletePlaceIndex $
--             newDeletePlaceIndexResponse
--
--         , responseDeleteRouteCalculator $
--             newDeleteRouteCalculatorResponse
--
--         , responseDeleteTracker $
--             newDeleteTrackerResponse
--
--         , responseDescribeGeofenceCollection $
--             newDescribeGeofenceCollectionResponse
--
--         , responseDescribeMap $
--             newDescribeMapResponse
--
--         , responseDescribePlaceIndex $
--             newDescribePlaceIndexResponse
--
--         , responseDescribeRouteCalculator $
--             newDescribeRouteCalculatorResponse
--
--         , responseDescribeTracker $
--             newDescribeTrackerResponse
--
--         , responseDisassociateTrackerConsumer $
--             newDisassociateTrackerConsumerResponse
--
--         , responseGetDevicePosition $
--             newGetDevicePositionResponse
--
--         , responseGetDevicePositionHistory $
--             newGetDevicePositionHistoryResponse
--
--         , responseGetGeofence $
--             newGetGeofenceResponse
--
--         , responseGetMapGlyphs $
--             newGetMapGlyphsResponse
--
--         , responseGetMapSprites $
--             newGetMapSpritesResponse
--
--         , responseGetMapStyleDescriptor $
--             newGetMapStyleDescriptorResponse
--
--         , responseGetMapTile $
--             newGetMapTileResponse
--
--         , responseGetPlace $
--             newGetPlaceResponse
--
--         , responseListDevicePositions $
--             newListDevicePositionsResponse
--
--         , responseListGeofenceCollections $
--             newListGeofenceCollectionsResponse
--
--         , responseListGeofences $
--             newListGeofencesResponse
--
--         , responseListMaps $
--             newListMapsResponse
--
--         , responseListPlaceIndexes $
--             newListPlaceIndexesResponse
--
--         , responseListRouteCalculators $
--             newListRouteCalculatorsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTrackerConsumers $
--             newListTrackerConsumersResponse
--
--         , responseListTrackers $
--             newListTrackersResponse
--
--         , responsePutGeofence $
--             newPutGeofenceResponse
--
--         , responseSearchPlaceIndexForPosition $
--             newSearchPlaceIndexForPositionResponse
--
--         , responseSearchPlaceIndexForSuggestions $
--             newSearchPlaceIndexForSuggestionsResponse
--
--         , responseSearchPlaceIndexForText $
--             newSearchPlaceIndexForTextResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateGeofenceCollection $
--             newUpdateGeofenceCollectionResponse
--
--         , responseUpdateMap $
--             newUpdateMapResponse
--
--         , responseUpdatePlaceIndex $
--             newUpdatePlaceIndexResponse
--
--         , responseUpdateRouteCalculator $
--             newUpdateRouteCalculatorResponse
--
--         , responseUpdateTracker $
--             newUpdateTrackerResponse
--
--           ]
--     ]

-- Requests

requestAssociateTrackerConsumer :: AssociateTrackerConsumer -> TestTree
requestAssociateTrackerConsumer =
  req
    "AssociateTrackerConsumer"
    "fixture/AssociateTrackerConsumer.yaml"

requestBatchDeleteDevicePositionHistory :: BatchDeleteDevicePositionHistory -> TestTree
requestBatchDeleteDevicePositionHistory =
  req
    "BatchDeleteDevicePositionHistory"
    "fixture/BatchDeleteDevicePositionHistory.yaml"

requestBatchDeleteGeofence :: BatchDeleteGeofence -> TestTree
requestBatchDeleteGeofence =
  req
    "BatchDeleteGeofence"
    "fixture/BatchDeleteGeofence.yaml"

requestBatchEvaluateGeofences :: BatchEvaluateGeofences -> TestTree
requestBatchEvaluateGeofences =
  req
    "BatchEvaluateGeofences"
    "fixture/BatchEvaluateGeofences.yaml"

requestBatchGetDevicePosition :: BatchGetDevicePosition -> TestTree
requestBatchGetDevicePosition =
  req
    "BatchGetDevicePosition"
    "fixture/BatchGetDevicePosition.yaml"

requestBatchPutGeofence :: BatchPutGeofence -> TestTree
requestBatchPutGeofence =
  req
    "BatchPutGeofence"
    "fixture/BatchPutGeofence.yaml"

requestBatchUpdateDevicePosition :: BatchUpdateDevicePosition -> TestTree
requestBatchUpdateDevicePosition =
  req
    "BatchUpdateDevicePosition"
    "fixture/BatchUpdateDevicePosition.yaml"

requestCalculateRoute :: CalculateRoute -> TestTree
requestCalculateRoute =
  req
    "CalculateRoute"
    "fixture/CalculateRoute.yaml"

requestCalculateRouteMatrix :: CalculateRouteMatrix -> TestTree
requestCalculateRouteMatrix =
  req
    "CalculateRouteMatrix"
    "fixture/CalculateRouteMatrix.yaml"

requestCreateGeofenceCollection :: CreateGeofenceCollection -> TestTree
requestCreateGeofenceCollection =
  req
    "CreateGeofenceCollection"
    "fixture/CreateGeofenceCollection.yaml"

requestCreateMap :: CreateMap -> TestTree
requestCreateMap =
  req
    "CreateMap"
    "fixture/CreateMap.yaml"

requestCreatePlaceIndex :: CreatePlaceIndex -> TestTree
requestCreatePlaceIndex =
  req
    "CreatePlaceIndex"
    "fixture/CreatePlaceIndex.yaml"

requestCreateRouteCalculator :: CreateRouteCalculator -> TestTree
requestCreateRouteCalculator =
  req
    "CreateRouteCalculator"
    "fixture/CreateRouteCalculator.yaml"

requestCreateTracker :: CreateTracker -> TestTree
requestCreateTracker =
  req
    "CreateTracker"
    "fixture/CreateTracker.yaml"

requestDeleteGeofenceCollection :: DeleteGeofenceCollection -> TestTree
requestDeleteGeofenceCollection =
  req
    "DeleteGeofenceCollection"
    "fixture/DeleteGeofenceCollection.yaml"

requestDeleteMap :: DeleteMap -> TestTree
requestDeleteMap =
  req
    "DeleteMap"
    "fixture/DeleteMap.yaml"

requestDeletePlaceIndex :: DeletePlaceIndex -> TestTree
requestDeletePlaceIndex =
  req
    "DeletePlaceIndex"
    "fixture/DeletePlaceIndex.yaml"

requestDeleteRouteCalculator :: DeleteRouteCalculator -> TestTree
requestDeleteRouteCalculator =
  req
    "DeleteRouteCalculator"
    "fixture/DeleteRouteCalculator.yaml"

requestDeleteTracker :: DeleteTracker -> TestTree
requestDeleteTracker =
  req
    "DeleteTracker"
    "fixture/DeleteTracker.yaml"

requestDescribeGeofenceCollection :: DescribeGeofenceCollection -> TestTree
requestDescribeGeofenceCollection =
  req
    "DescribeGeofenceCollection"
    "fixture/DescribeGeofenceCollection.yaml"

requestDescribeMap :: DescribeMap -> TestTree
requestDescribeMap =
  req
    "DescribeMap"
    "fixture/DescribeMap.yaml"

requestDescribePlaceIndex :: DescribePlaceIndex -> TestTree
requestDescribePlaceIndex =
  req
    "DescribePlaceIndex"
    "fixture/DescribePlaceIndex.yaml"

requestDescribeRouteCalculator :: DescribeRouteCalculator -> TestTree
requestDescribeRouteCalculator =
  req
    "DescribeRouteCalculator"
    "fixture/DescribeRouteCalculator.yaml"

requestDescribeTracker :: DescribeTracker -> TestTree
requestDescribeTracker =
  req
    "DescribeTracker"
    "fixture/DescribeTracker.yaml"

requestDisassociateTrackerConsumer :: DisassociateTrackerConsumer -> TestTree
requestDisassociateTrackerConsumer =
  req
    "DisassociateTrackerConsumer"
    "fixture/DisassociateTrackerConsumer.yaml"

requestGetDevicePosition :: GetDevicePosition -> TestTree
requestGetDevicePosition =
  req
    "GetDevicePosition"
    "fixture/GetDevicePosition.yaml"

requestGetDevicePositionHistory :: GetDevicePositionHistory -> TestTree
requestGetDevicePositionHistory =
  req
    "GetDevicePositionHistory"
    "fixture/GetDevicePositionHistory.yaml"

requestGetGeofence :: GetGeofence -> TestTree
requestGetGeofence =
  req
    "GetGeofence"
    "fixture/GetGeofence.yaml"

requestGetMapGlyphs :: GetMapGlyphs -> TestTree
requestGetMapGlyphs =
  req
    "GetMapGlyphs"
    "fixture/GetMapGlyphs.yaml"

requestGetMapSprites :: GetMapSprites -> TestTree
requestGetMapSprites =
  req
    "GetMapSprites"
    "fixture/GetMapSprites.yaml"

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

requestGetPlace :: GetPlace -> TestTree
requestGetPlace =
  req
    "GetPlace"
    "fixture/GetPlace.yaml"

requestListDevicePositions :: ListDevicePositions -> TestTree
requestListDevicePositions =
  req
    "ListDevicePositions"
    "fixture/ListDevicePositions.yaml"

requestListGeofenceCollections :: ListGeofenceCollections -> TestTree
requestListGeofenceCollections =
  req
    "ListGeofenceCollections"
    "fixture/ListGeofenceCollections.yaml"

requestListGeofences :: ListGeofences -> TestTree
requestListGeofences =
  req
    "ListGeofences"
    "fixture/ListGeofences.yaml"

requestListMaps :: ListMaps -> TestTree
requestListMaps =
  req
    "ListMaps"
    "fixture/ListMaps.yaml"

requestListPlaceIndexes :: ListPlaceIndexes -> TestTree
requestListPlaceIndexes =
  req
    "ListPlaceIndexes"
    "fixture/ListPlaceIndexes.yaml"

requestListRouteCalculators :: ListRouteCalculators -> TestTree
requestListRouteCalculators =
  req
    "ListRouteCalculators"
    "fixture/ListRouteCalculators.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTrackerConsumers :: ListTrackerConsumers -> TestTree
requestListTrackerConsumers =
  req
    "ListTrackerConsumers"
    "fixture/ListTrackerConsumers.yaml"

requestListTrackers :: ListTrackers -> TestTree
requestListTrackers =
  req
    "ListTrackers"
    "fixture/ListTrackers.yaml"

requestPutGeofence :: PutGeofence -> TestTree
requestPutGeofence =
  req
    "PutGeofence"
    "fixture/PutGeofence.yaml"

requestSearchPlaceIndexForPosition :: SearchPlaceIndexForPosition -> TestTree
requestSearchPlaceIndexForPosition =
  req
    "SearchPlaceIndexForPosition"
    "fixture/SearchPlaceIndexForPosition.yaml"

requestSearchPlaceIndexForSuggestions :: SearchPlaceIndexForSuggestions -> TestTree
requestSearchPlaceIndexForSuggestions =
  req
    "SearchPlaceIndexForSuggestions"
    "fixture/SearchPlaceIndexForSuggestions.yaml"

requestSearchPlaceIndexForText :: SearchPlaceIndexForText -> TestTree
requestSearchPlaceIndexForText =
  req
    "SearchPlaceIndexForText"
    "fixture/SearchPlaceIndexForText.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateGeofenceCollection :: UpdateGeofenceCollection -> TestTree
requestUpdateGeofenceCollection =
  req
    "UpdateGeofenceCollection"
    "fixture/UpdateGeofenceCollection.yaml"

requestUpdateMap :: UpdateMap -> TestTree
requestUpdateMap =
  req
    "UpdateMap"
    "fixture/UpdateMap.yaml"

requestUpdatePlaceIndex :: UpdatePlaceIndex -> TestTree
requestUpdatePlaceIndex =
  req
    "UpdatePlaceIndex"
    "fixture/UpdatePlaceIndex.yaml"

requestUpdateRouteCalculator :: UpdateRouteCalculator -> TestTree
requestUpdateRouteCalculator =
  req
    "UpdateRouteCalculator"
    "fixture/UpdateRouteCalculator.yaml"

requestUpdateTracker :: UpdateTracker -> TestTree
requestUpdateTracker =
  req
    "UpdateTracker"
    "fixture/UpdateTracker.yaml"

-- Responses

responseAssociateTrackerConsumer :: AssociateTrackerConsumerResponse -> TestTree
responseAssociateTrackerConsumer =
  res
    "AssociateTrackerConsumerResponse"
    "fixture/AssociateTrackerConsumerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateTrackerConsumer)

responseBatchDeleteDevicePositionHistory :: BatchDeleteDevicePositionHistoryResponse -> TestTree
responseBatchDeleteDevicePositionHistory =
  res
    "BatchDeleteDevicePositionHistoryResponse"
    "fixture/BatchDeleteDevicePositionHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteDevicePositionHistory)

responseBatchDeleteGeofence :: BatchDeleteGeofenceResponse -> TestTree
responseBatchDeleteGeofence =
  res
    "BatchDeleteGeofenceResponse"
    "fixture/BatchDeleteGeofenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteGeofence)

responseBatchEvaluateGeofences :: BatchEvaluateGeofencesResponse -> TestTree
responseBatchEvaluateGeofences =
  res
    "BatchEvaluateGeofencesResponse"
    "fixture/BatchEvaluateGeofencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchEvaluateGeofences)

responseBatchGetDevicePosition :: BatchGetDevicePositionResponse -> TestTree
responseBatchGetDevicePosition =
  res
    "BatchGetDevicePositionResponse"
    "fixture/BatchGetDevicePositionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetDevicePosition)

responseBatchPutGeofence :: BatchPutGeofenceResponse -> TestTree
responseBatchPutGeofence =
  res
    "BatchPutGeofenceResponse"
    "fixture/BatchPutGeofenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchPutGeofence)

responseBatchUpdateDevicePosition :: BatchUpdateDevicePositionResponse -> TestTree
responseBatchUpdateDevicePosition =
  res
    "BatchUpdateDevicePositionResponse"
    "fixture/BatchUpdateDevicePositionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpdateDevicePosition)

responseCalculateRoute :: CalculateRouteResponse -> TestTree
responseCalculateRoute =
  res
    "CalculateRouteResponse"
    "fixture/CalculateRouteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CalculateRoute)

responseCalculateRouteMatrix :: CalculateRouteMatrixResponse -> TestTree
responseCalculateRouteMatrix =
  res
    "CalculateRouteMatrixResponse"
    "fixture/CalculateRouteMatrixResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CalculateRouteMatrix)

responseCreateGeofenceCollection :: CreateGeofenceCollectionResponse -> TestTree
responseCreateGeofenceCollection =
  res
    "CreateGeofenceCollectionResponse"
    "fixture/CreateGeofenceCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGeofenceCollection)

responseCreateMap :: CreateMapResponse -> TestTree
responseCreateMap =
  res
    "CreateMapResponse"
    "fixture/CreateMapResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMap)

responseCreatePlaceIndex :: CreatePlaceIndexResponse -> TestTree
responseCreatePlaceIndex =
  res
    "CreatePlaceIndexResponse"
    "fixture/CreatePlaceIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePlaceIndex)

responseCreateRouteCalculator :: CreateRouteCalculatorResponse -> TestTree
responseCreateRouteCalculator =
  res
    "CreateRouteCalculatorResponse"
    "fixture/CreateRouteCalculatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRouteCalculator)

responseCreateTracker :: CreateTrackerResponse -> TestTree
responseCreateTracker =
  res
    "CreateTrackerResponse"
    "fixture/CreateTrackerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTracker)

responseDeleteGeofenceCollection :: DeleteGeofenceCollectionResponse -> TestTree
responseDeleteGeofenceCollection =
  res
    "DeleteGeofenceCollectionResponse"
    "fixture/DeleteGeofenceCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGeofenceCollection)

responseDeleteMap :: DeleteMapResponse -> TestTree
responseDeleteMap =
  res
    "DeleteMapResponse"
    "fixture/DeleteMapResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMap)

responseDeletePlaceIndex :: DeletePlaceIndexResponse -> TestTree
responseDeletePlaceIndex =
  res
    "DeletePlaceIndexResponse"
    "fixture/DeletePlaceIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePlaceIndex)

responseDeleteRouteCalculator :: DeleteRouteCalculatorResponse -> TestTree
responseDeleteRouteCalculator =
  res
    "DeleteRouteCalculatorResponse"
    "fixture/DeleteRouteCalculatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRouteCalculator)

responseDeleteTracker :: DeleteTrackerResponse -> TestTree
responseDeleteTracker =
  res
    "DeleteTrackerResponse"
    "fixture/DeleteTrackerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTracker)

responseDescribeGeofenceCollection :: DescribeGeofenceCollectionResponse -> TestTree
responseDescribeGeofenceCollection =
  res
    "DescribeGeofenceCollectionResponse"
    "fixture/DescribeGeofenceCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGeofenceCollection)

responseDescribeMap :: DescribeMapResponse -> TestTree
responseDescribeMap =
  res
    "DescribeMapResponse"
    "fixture/DescribeMapResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMap)

responseDescribePlaceIndex :: DescribePlaceIndexResponse -> TestTree
responseDescribePlaceIndex =
  res
    "DescribePlaceIndexResponse"
    "fixture/DescribePlaceIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePlaceIndex)

responseDescribeRouteCalculator :: DescribeRouteCalculatorResponse -> TestTree
responseDescribeRouteCalculator =
  res
    "DescribeRouteCalculatorResponse"
    "fixture/DescribeRouteCalculatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRouteCalculator)

responseDescribeTracker :: DescribeTrackerResponse -> TestTree
responseDescribeTracker =
  res
    "DescribeTrackerResponse"
    "fixture/DescribeTrackerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTracker)

responseDisassociateTrackerConsumer :: DisassociateTrackerConsumerResponse -> TestTree
responseDisassociateTrackerConsumer =
  res
    "DisassociateTrackerConsumerResponse"
    "fixture/DisassociateTrackerConsumerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateTrackerConsumer)

responseGetDevicePosition :: GetDevicePositionResponse -> TestTree
responseGetDevicePosition =
  res
    "GetDevicePositionResponse"
    "fixture/GetDevicePositionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevicePosition)

responseGetDevicePositionHistory :: GetDevicePositionHistoryResponse -> TestTree
responseGetDevicePositionHistory =
  res
    "GetDevicePositionHistoryResponse"
    "fixture/GetDevicePositionHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevicePositionHistory)

responseGetGeofence :: GetGeofenceResponse -> TestTree
responseGetGeofence =
  res
    "GetGeofenceResponse"
    "fixture/GetGeofenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGeofence)

responseGetMapGlyphs :: GetMapGlyphsResponse -> TestTree
responseGetMapGlyphs =
  res
    "GetMapGlyphsResponse"
    "fixture/GetMapGlyphsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMapGlyphs)

responseGetMapSprites :: GetMapSpritesResponse -> TestTree
responseGetMapSprites =
  res
    "GetMapSpritesResponse"
    "fixture/GetMapSpritesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMapSprites)

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

responseGetPlace :: GetPlaceResponse -> TestTree
responseGetPlace =
  res
    "GetPlaceResponse"
    "fixture/GetPlaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPlace)

responseListDevicePositions :: ListDevicePositionsResponse -> TestTree
responseListDevicePositions =
  res
    "ListDevicePositionsResponse"
    "fixture/ListDevicePositionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevicePositions)

responseListGeofenceCollections :: ListGeofenceCollectionsResponse -> TestTree
responseListGeofenceCollections =
  res
    "ListGeofenceCollectionsResponse"
    "fixture/ListGeofenceCollectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGeofenceCollections)

responseListGeofences :: ListGeofencesResponse -> TestTree
responseListGeofences =
  res
    "ListGeofencesResponse"
    "fixture/ListGeofencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGeofences)

responseListMaps :: ListMapsResponse -> TestTree
responseListMaps =
  res
    "ListMapsResponse"
    "fixture/ListMapsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMaps)

responseListPlaceIndexes :: ListPlaceIndexesResponse -> TestTree
responseListPlaceIndexes =
  res
    "ListPlaceIndexesResponse"
    "fixture/ListPlaceIndexesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPlaceIndexes)

responseListRouteCalculators :: ListRouteCalculatorsResponse -> TestTree
responseListRouteCalculators =
  res
    "ListRouteCalculatorsResponse"
    "fixture/ListRouteCalculatorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRouteCalculators)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTrackerConsumers :: ListTrackerConsumersResponse -> TestTree
responseListTrackerConsumers =
  res
    "ListTrackerConsumersResponse"
    "fixture/ListTrackerConsumersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrackerConsumers)

responseListTrackers :: ListTrackersResponse -> TestTree
responseListTrackers =
  res
    "ListTrackersResponse"
    "fixture/ListTrackersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrackers)

responsePutGeofence :: PutGeofenceResponse -> TestTree
responsePutGeofence =
  res
    "PutGeofenceResponse"
    "fixture/PutGeofenceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutGeofence)

responseSearchPlaceIndexForPosition :: SearchPlaceIndexForPositionResponse -> TestTree
responseSearchPlaceIndexForPosition =
  res
    "SearchPlaceIndexForPositionResponse"
    "fixture/SearchPlaceIndexForPositionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchPlaceIndexForPosition)

responseSearchPlaceIndexForSuggestions :: SearchPlaceIndexForSuggestionsResponse -> TestTree
responseSearchPlaceIndexForSuggestions =
  res
    "SearchPlaceIndexForSuggestionsResponse"
    "fixture/SearchPlaceIndexForSuggestionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchPlaceIndexForSuggestions)

responseSearchPlaceIndexForText :: SearchPlaceIndexForTextResponse -> TestTree
responseSearchPlaceIndexForText =
  res
    "SearchPlaceIndexForTextResponse"
    "fixture/SearchPlaceIndexForTextResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchPlaceIndexForText)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateGeofenceCollection :: UpdateGeofenceCollectionResponse -> TestTree
responseUpdateGeofenceCollection =
  res
    "UpdateGeofenceCollectionResponse"
    "fixture/UpdateGeofenceCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGeofenceCollection)

responseUpdateMap :: UpdateMapResponse -> TestTree
responseUpdateMap =
  res
    "UpdateMapResponse"
    "fixture/UpdateMapResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMap)

responseUpdatePlaceIndex :: UpdatePlaceIndexResponse -> TestTree
responseUpdatePlaceIndex =
  res
    "UpdatePlaceIndexResponse"
    "fixture/UpdatePlaceIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePlaceIndex)

responseUpdateRouteCalculator :: UpdateRouteCalculatorResponse -> TestTree
responseUpdateRouteCalculator =
  res
    "UpdateRouteCalculatorResponse"
    "fixture/UpdateRouteCalculatorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRouteCalculator)

responseUpdateTracker :: UpdateTrackerResponse -> TestTree
responseUpdateTracker =
  res
    "UpdateTrackerResponse"
    "fixture/UpdateTrackerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTracker)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Rekognition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Rekognition where

import Data.Proxy
import Network.AWS.Rekognition
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Rekognition.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestStartCelebrityRecognition $
--             startCelebrityRecognition
--
--         , requestGetPersonTracking $
--             getPersonTracking
--
--         , requestListCollections $
--             listCollections
--
--         , requestDeleteCollection $
--             deleteCollection
--
--         , requestCreateCollection $
--             createCollection
--
--         , requestStopStreamProcessor $
--             stopStreamProcessor
--
--         , requestDetectLabels $
--             detectLabels
--
--         , requestStartContentModeration $
--             startContentModeration
--
--         , requestSearchFacesByImage $
--             searchFacesByImage
--
--         , requestListStreamProcessors $
--             listStreamProcessors
--
--         , requestRecognizeCelebrities $
--             recognizeCelebrities
--
--         , requestGetFaceSearch $
--             getFaceSearch
--
--         , requestStartLabelDetection $
--             startLabelDetection
--
--         , requestSearchFaces $
--             searchFaces
--
--         , requestIndexFaces $
--             indexFaces
--
--         , requestGetLabelDetection $
--             getLabelDetection
--
--         , requestDescribeStreamProcessor $
--             describeStreamProcessor
--
--         , requestStartFaceSearch $
--             startFaceSearch
--
--         , requestStartPersonTracking $
--             startPersonTracking
--
--         , requestGetCelebrityRecognition $
--             getCelebrityRecognition
--
--         , requestStartStreamProcessor $
--             startStreamProcessor
--
--         , requestDetectText $
--             detectText
--
--         , requestCompareFaces $
--             compareFaces
--
--         , requestDetectFaces $
--             detectFaces
--
--         , requestGetFaceDetection $
--             getFaceDetection
--
--         , requestListFaces $
--             listFaces
--
--         , requestGetContentModeration $
--             getContentModeration
--
--         , requestDeleteFaces $
--             deleteFaces
--
--         , requestGetCelebrityInfo $
--             getCelebrityInfo
--
--         , requestDeleteStreamProcessor $
--             deleteStreamProcessor
--
--         , requestDetectModerationLabels $
--             detectModerationLabels
--
--         , requestCreateStreamProcessor $
--             createStreamProcessor
--
--         , requestStartFaceDetection $
--             startFaceDetection
--
--           ]

--     , testGroup "response"
--         [ responseStartCelebrityRecognition $
--             startCelebrityRecognitionResponse
--
--         , responseGetPersonTracking $
--             getPersonTrackingResponse
--
--         , responseListCollections $
--             listCollectionsResponse
--
--         , responseDeleteCollection $
--             deleteCollectionResponse
--
--         , responseCreateCollection $
--             createCollectionResponse
--
--         , responseStopStreamProcessor $
--             stopStreamProcessorResponse
--
--         , responseDetectLabels $
--             detectLabelsResponse
--
--         , responseStartContentModeration $
--             startContentModerationResponse
--
--         , responseSearchFacesByImage $
--             searchFacesByImageResponse
--
--         , responseListStreamProcessors $
--             listStreamProcessorsResponse
--
--         , responseRecognizeCelebrities $
--             recognizeCelebritiesResponse
--
--         , responseGetFaceSearch $
--             getFaceSearchResponse
--
--         , responseStartLabelDetection $
--             startLabelDetectionResponse
--
--         , responseSearchFaces $
--             searchFacesResponse
--
--         , responseIndexFaces $
--             indexFacesResponse
--
--         , responseGetLabelDetection $
--             getLabelDetectionResponse
--
--         , responseDescribeStreamProcessor $
--             describeStreamProcessorResponse
--
--         , responseStartFaceSearch $
--             startFaceSearchResponse
--
--         , responseStartPersonTracking $
--             startPersonTrackingResponse
--
--         , responseGetCelebrityRecognition $
--             getCelebrityRecognitionResponse
--
--         , responseStartStreamProcessor $
--             startStreamProcessorResponse
--
--         , responseDetectText $
--             detectTextResponse
--
--         , responseCompareFaces $
--             compareFacesResponse
--
--         , responseDetectFaces $
--             detectFacesResponse
--
--         , responseGetFaceDetection $
--             getFaceDetectionResponse
--
--         , responseListFaces $
--             listFacesResponse
--
--         , responseGetContentModeration $
--             getContentModerationResponse
--
--         , responseDeleteFaces $
--             deleteFacesResponse
--
--         , responseGetCelebrityInfo $
--             getCelebrityInfoResponse
--
--         , responseDeleteStreamProcessor $
--             deleteStreamProcessorResponse
--
--         , responseDetectModerationLabels $
--             detectModerationLabelsResponse
--
--         , responseCreateStreamProcessor $
--             createStreamProcessorResponse
--
--         , responseStartFaceDetection $
--             startFaceDetectionResponse
--
--           ]
--     ]

-- Requests

requestStartCelebrityRecognition :: StartCelebrityRecognition -> TestTree
requestStartCelebrityRecognition = req
    "StartCelebrityRecognition"
    "fixture/StartCelebrityRecognition.yaml"

requestGetPersonTracking :: GetPersonTracking -> TestTree
requestGetPersonTracking = req
    "GetPersonTracking"
    "fixture/GetPersonTracking.yaml"

requestListCollections :: ListCollections -> TestTree
requestListCollections = req
    "ListCollections"
    "fixture/ListCollections.yaml"

requestDeleteCollection :: DeleteCollection -> TestTree
requestDeleteCollection = req
    "DeleteCollection"
    "fixture/DeleteCollection.yaml"

requestCreateCollection :: CreateCollection -> TestTree
requestCreateCollection = req
    "CreateCollection"
    "fixture/CreateCollection.yaml"

requestStopStreamProcessor :: StopStreamProcessor -> TestTree
requestStopStreamProcessor = req
    "StopStreamProcessor"
    "fixture/StopStreamProcessor.yaml"

requestDetectLabels :: DetectLabels -> TestTree
requestDetectLabels = req
    "DetectLabels"
    "fixture/DetectLabels.yaml"

requestStartContentModeration :: StartContentModeration -> TestTree
requestStartContentModeration = req
    "StartContentModeration"
    "fixture/StartContentModeration.yaml"

requestSearchFacesByImage :: SearchFacesByImage -> TestTree
requestSearchFacesByImage = req
    "SearchFacesByImage"
    "fixture/SearchFacesByImage.yaml"

requestListStreamProcessors :: ListStreamProcessors -> TestTree
requestListStreamProcessors = req
    "ListStreamProcessors"
    "fixture/ListStreamProcessors.yaml"

requestRecognizeCelebrities :: RecognizeCelebrities -> TestTree
requestRecognizeCelebrities = req
    "RecognizeCelebrities"
    "fixture/RecognizeCelebrities.yaml"

requestGetFaceSearch :: GetFaceSearch -> TestTree
requestGetFaceSearch = req
    "GetFaceSearch"
    "fixture/GetFaceSearch.yaml"

requestStartLabelDetection :: StartLabelDetection -> TestTree
requestStartLabelDetection = req
    "StartLabelDetection"
    "fixture/StartLabelDetection.yaml"

requestSearchFaces :: SearchFaces -> TestTree
requestSearchFaces = req
    "SearchFaces"
    "fixture/SearchFaces.yaml"

requestIndexFaces :: IndexFaces -> TestTree
requestIndexFaces = req
    "IndexFaces"
    "fixture/IndexFaces.yaml"

requestGetLabelDetection :: GetLabelDetection -> TestTree
requestGetLabelDetection = req
    "GetLabelDetection"
    "fixture/GetLabelDetection.yaml"

requestDescribeStreamProcessor :: DescribeStreamProcessor -> TestTree
requestDescribeStreamProcessor = req
    "DescribeStreamProcessor"
    "fixture/DescribeStreamProcessor.yaml"

requestStartFaceSearch :: StartFaceSearch -> TestTree
requestStartFaceSearch = req
    "StartFaceSearch"
    "fixture/StartFaceSearch.yaml"

requestStartPersonTracking :: StartPersonTracking -> TestTree
requestStartPersonTracking = req
    "StartPersonTracking"
    "fixture/StartPersonTracking.yaml"

requestGetCelebrityRecognition :: GetCelebrityRecognition -> TestTree
requestGetCelebrityRecognition = req
    "GetCelebrityRecognition"
    "fixture/GetCelebrityRecognition.yaml"

requestStartStreamProcessor :: StartStreamProcessor -> TestTree
requestStartStreamProcessor = req
    "StartStreamProcessor"
    "fixture/StartStreamProcessor.yaml"

requestDetectText :: DetectText -> TestTree
requestDetectText = req
    "DetectText"
    "fixture/DetectText.yaml"

requestCompareFaces :: CompareFaces -> TestTree
requestCompareFaces = req
    "CompareFaces"
    "fixture/CompareFaces.yaml"

requestDetectFaces :: DetectFaces -> TestTree
requestDetectFaces = req
    "DetectFaces"
    "fixture/DetectFaces.yaml"

requestGetFaceDetection :: GetFaceDetection -> TestTree
requestGetFaceDetection = req
    "GetFaceDetection"
    "fixture/GetFaceDetection.yaml"

requestListFaces :: ListFaces -> TestTree
requestListFaces = req
    "ListFaces"
    "fixture/ListFaces.yaml"

requestGetContentModeration :: GetContentModeration -> TestTree
requestGetContentModeration = req
    "GetContentModeration"
    "fixture/GetContentModeration.yaml"

requestDeleteFaces :: DeleteFaces -> TestTree
requestDeleteFaces = req
    "DeleteFaces"
    "fixture/DeleteFaces.yaml"

requestGetCelebrityInfo :: GetCelebrityInfo -> TestTree
requestGetCelebrityInfo = req
    "GetCelebrityInfo"
    "fixture/GetCelebrityInfo.yaml"

requestDeleteStreamProcessor :: DeleteStreamProcessor -> TestTree
requestDeleteStreamProcessor = req
    "DeleteStreamProcessor"
    "fixture/DeleteStreamProcessor.yaml"

requestDetectModerationLabels :: DetectModerationLabels -> TestTree
requestDetectModerationLabels = req
    "DetectModerationLabels"
    "fixture/DetectModerationLabels.yaml"

requestCreateStreamProcessor :: CreateStreamProcessor -> TestTree
requestCreateStreamProcessor = req
    "CreateStreamProcessor"
    "fixture/CreateStreamProcessor.yaml"

requestStartFaceDetection :: StartFaceDetection -> TestTree
requestStartFaceDetection = req
    "StartFaceDetection"
    "fixture/StartFaceDetection.yaml"

-- Responses

responseStartCelebrityRecognition :: StartCelebrityRecognitionResponse -> TestTree
responseStartCelebrityRecognition = res
    "StartCelebrityRecognitionResponse"
    "fixture/StartCelebrityRecognitionResponse.proto"
    rekognition
    (Proxy :: Proxy StartCelebrityRecognition)

responseGetPersonTracking :: GetPersonTrackingResponse -> TestTree
responseGetPersonTracking = res
    "GetPersonTrackingResponse"
    "fixture/GetPersonTrackingResponse.proto"
    rekognition
    (Proxy :: Proxy GetPersonTracking)

responseListCollections :: ListCollectionsResponse -> TestTree
responseListCollections = res
    "ListCollectionsResponse"
    "fixture/ListCollectionsResponse.proto"
    rekognition
    (Proxy :: Proxy ListCollections)

responseDeleteCollection :: DeleteCollectionResponse -> TestTree
responseDeleteCollection = res
    "DeleteCollectionResponse"
    "fixture/DeleteCollectionResponse.proto"
    rekognition
    (Proxy :: Proxy DeleteCollection)

responseCreateCollection :: CreateCollectionResponse -> TestTree
responseCreateCollection = res
    "CreateCollectionResponse"
    "fixture/CreateCollectionResponse.proto"
    rekognition
    (Proxy :: Proxy CreateCollection)

responseStopStreamProcessor :: StopStreamProcessorResponse -> TestTree
responseStopStreamProcessor = res
    "StopStreamProcessorResponse"
    "fixture/StopStreamProcessorResponse.proto"
    rekognition
    (Proxy :: Proxy StopStreamProcessor)

responseDetectLabels :: DetectLabelsResponse -> TestTree
responseDetectLabels = res
    "DetectLabelsResponse"
    "fixture/DetectLabelsResponse.proto"
    rekognition
    (Proxy :: Proxy DetectLabels)

responseStartContentModeration :: StartContentModerationResponse -> TestTree
responseStartContentModeration = res
    "StartContentModerationResponse"
    "fixture/StartContentModerationResponse.proto"
    rekognition
    (Proxy :: Proxy StartContentModeration)

responseSearchFacesByImage :: SearchFacesByImageResponse -> TestTree
responseSearchFacesByImage = res
    "SearchFacesByImageResponse"
    "fixture/SearchFacesByImageResponse.proto"
    rekognition
    (Proxy :: Proxy SearchFacesByImage)

responseListStreamProcessors :: ListStreamProcessorsResponse -> TestTree
responseListStreamProcessors = res
    "ListStreamProcessorsResponse"
    "fixture/ListStreamProcessorsResponse.proto"
    rekognition
    (Proxy :: Proxy ListStreamProcessors)

responseRecognizeCelebrities :: RecognizeCelebritiesResponse -> TestTree
responseRecognizeCelebrities = res
    "RecognizeCelebritiesResponse"
    "fixture/RecognizeCelebritiesResponse.proto"
    rekognition
    (Proxy :: Proxy RecognizeCelebrities)

responseGetFaceSearch :: GetFaceSearchResponse -> TestTree
responseGetFaceSearch = res
    "GetFaceSearchResponse"
    "fixture/GetFaceSearchResponse.proto"
    rekognition
    (Proxy :: Proxy GetFaceSearch)

responseStartLabelDetection :: StartLabelDetectionResponse -> TestTree
responseStartLabelDetection = res
    "StartLabelDetectionResponse"
    "fixture/StartLabelDetectionResponse.proto"
    rekognition
    (Proxy :: Proxy StartLabelDetection)

responseSearchFaces :: SearchFacesResponse -> TestTree
responseSearchFaces = res
    "SearchFacesResponse"
    "fixture/SearchFacesResponse.proto"
    rekognition
    (Proxy :: Proxy SearchFaces)

responseIndexFaces :: IndexFacesResponse -> TestTree
responseIndexFaces = res
    "IndexFacesResponse"
    "fixture/IndexFacesResponse.proto"
    rekognition
    (Proxy :: Proxy IndexFaces)

responseGetLabelDetection :: GetLabelDetectionResponse -> TestTree
responseGetLabelDetection = res
    "GetLabelDetectionResponse"
    "fixture/GetLabelDetectionResponse.proto"
    rekognition
    (Proxy :: Proxy GetLabelDetection)

responseDescribeStreamProcessor :: DescribeStreamProcessorResponse -> TestTree
responseDescribeStreamProcessor = res
    "DescribeStreamProcessorResponse"
    "fixture/DescribeStreamProcessorResponse.proto"
    rekognition
    (Proxy :: Proxy DescribeStreamProcessor)

responseStartFaceSearch :: StartFaceSearchResponse -> TestTree
responseStartFaceSearch = res
    "StartFaceSearchResponse"
    "fixture/StartFaceSearchResponse.proto"
    rekognition
    (Proxy :: Proxy StartFaceSearch)

responseStartPersonTracking :: StartPersonTrackingResponse -> TestTree
responseStartPersonTracking = res
    "StartPersonTrackingResponse"
    "fixture/StartPersonTrackingResponse.proto"
    rekognition
    (Proxy :: Proxy StartPersonTracking)

responseGetCelebrityRecognition :: GetCelebrityRecognitionResponse -> TestTree
responseGetCelebrityRecognition = res
    "GetCelebrityRecognitionResponse"
    "fixture/GetCelebrityRecognitionResponse.proto"
    rekognition
    (Proxy :: Proxy GetCelebrityRecognition)

responseStartStreamProcessor :: StartStreamProcessorResponse -> TestTree
responseStartStreamProcessor = res
    "StartStreamProcessorResponse"
    "fixture/StartStreamProcessorResponse.proto"
    rekognition
    (Proxy :: Proxy StartStreamProcessor)

responseDetectText :: DetectTextResponse -> TestTree
responseDetectText = res
    "DetectTextResponse"
    "fixture/DetectTextResponse.proto"
    rekognition
    (Proxy :: Proxy DetectText)

responseCompareFaces :: CompareFacesResponse -> TestTree
responseCompareFaces = res
    "CompareFacesResponse"
    "fixture/CompareFacesResponse.proto"
    rekognition
    (Proxy :: Proxy CompareFaces)

responseDetectFaces :: DetectFacesResponse -> TestTree
responseDetectFaces = res
    "DetectFacesResponse"
    "fixture/DetectFacesResponse.proto"
    rekognition
    (Proxy :: Proxy DetectFaces)

responseGetFaceDetection :: GetFaceDetectionResponse -> TestTree
responseGetFaceDetection = res
    "GetFaceDetectionResponse"
    "fixture/GetFaceDetectionResponse.proto"
    rekognition
    (Proxy :: Proxy GetFaceDetection)

responseListFaces :: ListFacesResponse -> TestTree
responseListFaces = res
    "ListFacesResponse"
    "fixture/ListFacesResponse.proto"
    rekognition
    (Proxy :: Proxy ListFaces)

responseGetContentModeration :: GetContentModerationResponse -> TestTree
responseGetContentModeration = res
    "GetContentModerationResponse"
    "fixture/GetContentModerationResponse.proto"
    rekognition
    (Proxy :: Proxy GetContentModeration)

responseDeleteFaces :: DeleteFacesResponse -> TestTree
responseDeleteFaces = res
    "DeleteFacesResponse"
    "fixture/DeleteFacesResponse.proto"
    rekognition
    (Proxy :: Proxy DeleteFaces)

responseGetCelebrityInfo :: GetCelebrityInfoResponse -> TestTree
responseGetCelebrityInfo = res
    "GetCelebrityInfoResponse"
    "fixture/GetCelebrityInfoResponse.proto"
    rekognition
    (Proxy :: Proxy GetCelebrityInfo)

responseDeleteStreamProcessor :: DeleteStreamProcessorResponse -> TestTree
responseDeleteStreamProcessor = res
    "DeleteStreamProcessorResponse"
    "fixture/DeleteStreamProcessorResponse.proto"
    rekognition
    (Proxy :: Proxy DeleteStreamProcessor)

responseDetectModerationLabels :: DetectModerationLabelsResponse -> TestTree
responseDetectModerationLabels = res
    "DetectModerationLabelsResponse"
    "fixture/DetectModerationLabelsResponse.proto"
    rekognition
    (Proxy :: Proxy DetectModerationLabels)

responseCreateStreamProcessor :: CreateStreamProcessorResponse -> TestTree
responseCreateStreamProcessor = res
    "CreateStreamProcessorResponse"
    "fixture/CreateStreamProcessorResponse.proto"
    rekognition
    (Proxy :: Proxy CreateStreamProcessor)

responseStartFaceDetection :: StartFaceDetectionResponse -> TestTree
responseStartFaceDetection = res
    "StartFaceDetectionResponse"
    "fixture/StartFaceDetectionResponse.proto"
    rekognition
    (Proxy :: Proxy StartFaceDetection)

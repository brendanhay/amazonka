{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Rekognition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         [ requestStartFaceSearch $
--             newStartFaceSearch
--
--         , requestDescribeStreamProcessor $
--             newDescribeStreamProcessor
--
--         , requestDeleteCollection $
--             newDeleteCollection
--
--         , requestGetLabelDetection $
--             newGetLabelDetection
--
--         , requestSearchFaces $
--             newSearchFaces
--
--         , requestGetTextDetection $
--             newGetTextDetection
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestDetectCustomLabels $
--             newDetectCustomLabels
--
--         , requestRecognizeCelebrities $
--             newRecognizeCelebrities
--
--         , requestStartFaceDetection $
--             newStartFaceDetection
--
--         , requestDetectModerationLabels $
--             newDetectModerationLabels
--
--         , requestDeleteFaces $
--             newDeleteFaces
--
--         , requestListStreamProcessors $
--             newListStreamProcessors
--
--         , requestDescribeCollection $
--             newDescribeCollection
--
--         , requestDeleteStreamProcessor $
--             newDeleteStreamProcessor
--
--         , requestListFaces $
--             newListFaces
--
--         , requestSearchFacesByImage $
--             newSearchFacesByImage
--
--         , requestCompareFaces $
--             newCompareFaces
--
--         , requestDetectLabels $
--             newDetectLabels
--
--         , requestGetSegmentDetection $
--             newGetSegmentDetection
--
--         , requestGetCelebrityRecognition $
--             newGetCelebrityRecognition
--
--         , requestStartPersonTracking $
--             newStartPersonTracking
--
--         , requestCreateCollection $
--             newCreateCollection
--
--         , requestStopProjectVersion $
--             newStopProjectVersion
--
--         , requestStartProjectVersion $
--             newStartProjectVersion
--
--         , requestListCollections $
--             newListCollections
--
--         , requestDetectProtectiveEquipment $
--             newDetectProtectiveEquipment
--
--         , requestGetPersonTracking $
--             newGetPersonTracking
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestIndexFaces $
--             newIndexFaces
--
--         , requestStartSegmentDetection $
--             newStartSegmentDetection
--
--         , requestStartCelebrityRecognition $
--             newStartCelebrityRecognition
--
--         , requestGetFaceSearch $
--             newGetFaceSearch
--
--         , requestStartLabelDetection $
--             newStartLabelDetection
--
--         , requestDescribeProjectVersions $
--             newDescribeProjectVersions
--
--         , requestDeleteProjectVersion $
--             newDeleteProjectVersion
--
--         , requestCreateStreamProcessor $
--             newCreateStreamProcessor
--
--         , requestGetContentModeration $
--             newGetContentModeration
--
--         , requestGetCelebrityInfo $
--             newGetCelebrityInfo
--
--         , requestDescribeProjects $
--             newDescribeProjects
--
--         , requestCreateProjectVersion $
--             newCreateProjectVersion
--
--         , requestGetFaceDetection $
--             newGetFaceDetection
--
--         , requestStartContentModeration $
--             newStartContentModeration
--
--         , requestDetectFaces $
--             newDetectFaces
--
--         , requestDetectText $
--             newDetectText
--
--         , requestStartTextDetection $
--             newStartTextDetection
--
--         , requestStopStreamProcessor $
--             newStopStreamProcessor
--
--         , requestStartStreamProcessor $
--             newStartStreamProcessor
--
--           ]

--     , testGroup "response"
--         [ responseStartFaceSearch $
--             newStartFaceSearchResponse
--
--         , responseDescribeStreamProcessor $
--             newDescribeStreamProcessorResponse
--
--         , responseDeleteCollection $
--             newDeleteCollectionResponse
--
--         , responseGetLabelDetection $
--             newGetLabelDetectionResponse
--
--         , responseSearchFaces $
--             newSearchFacesResponse
--
--         , responseGetTextDetection $
--             newGetTextDetectionResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseDetectCustomLabels $
--             newDetectCustomLabelsResponse
--
--         , responseRecognizeCelebrities $
--             newRecognizeCelebritiesResponse
--
--         , responseStartFaceDetection $
--             newStartFaceDetectionResponse
--
--         , responseDetectModerationLabels $
--             newDetectModerationLabelsResponse
--
--         , responseDeleteFaces $
--             newDeleteFacesResponse
--
--         , responseListStreamProcessors $
--             newListStreamProcessorsResponse
--
--         , responseDescribeCollection $
--             newDescribeCollectionResponse
--
--         , responseDeleteStreamProcessor $
--             newDeleteStreamProcessorResponse
--
--         , responseListFaces $
--             newListFacesResponse
--
--         , responseSearchFacesByImage $
--             newSearchFacesByImageResponse
--
--         , responseCompareFaces $
--             newCompareFacesResponse
--
--         , responseDetectLabels $
--             newDetectLabelsResponse
--
--         , responseGetSegmentDetection $
--             newGetSegmentDetectionResponse
--
--         , responseGetCelebrityRecognition $
--             newGetCelebrityRecognitionResponse
--
--         , responseStartPersonTracking $
--             newStartPersonTrackingResponse
--
--         , responseCreateCollection $
--             newCreateCollectionResponse
--
--         , responseStopProjectVersion $
--             newStopProjectVersionResponse
--
--         , responseStartProjectVersion $
--             newStartProjectVersionResponse
--
--         , responseListCollections $
--             newListCollectionsResponse
--
--         , responseDetectProtectiveEquipment $
--             newDetectProtectiveEquipmentResponse
--
--         , responseGetPersonTracking $
--             newGetPersonTrackingResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseIndexFaces $
--             newIndexFacesResponse
--
--         , responseStartSegmentDetection $
--             newStartSegmentDetectionResponse
--
--         , responseStartCelebrityRecognition $
--             newStartCelebrityRecognitionResponse
--
--         , responseGetFaceSearch $
--             newGetFaceSearchResponse
--
--         , responseStartLabelDetection $
--             newStartLabelDetectionResponse
--
--         , responseDescribeProjectVersions $
--             newDescribeProjectVersionsResponse
--
--         , responseDeleteProjectVersion $
--             newDeleteProjectVersionResponse
--
--         , responseCreateStreamProcessor $
--             newCreateStreamProcessorResponse
--
--         , responseGetContentModeration $
--             newGetContentModerationResponse
--
--         , responseGetCelebrityInfo $
--             newGetCelebrityInfoResponse
--
--         , responseDescribeProjects $
--             newDescribeProjectsResponse
--
--         , responseCreateProjectVersion $
--             newCreateProjectVersionResponse
--
--         , responseGetFaceDetection $
--             newGetFaceDetectionResponse
--
--         , responseStartContentModeration $
--             newStartContentModerationResponse
--
--         , responseDetectFaces $
--             newDetectFacesResponse
--
--         , responseDetectText $
--             newDetectTextResponse
--
--         , responseStartTextDetection $
--             newStartTextDetectionResponse
--
--         , responseStopStreamProcessor $
--             newStopStreamProcessorResponse
--
--         , responseStartStreamProcessor $
--             newStartStreamProcessorResponse
--
--           ]
--     ]

-- Requests

requestStartFaceSearch :: StartFaceSearch -> TestTree
requestStartFaceSearch =
  req
    "StartFaceSearch"
    "fixture/StartFaceSearch.yaml"

requestDescribeStreamProcessor :: DescribeStreamProcessor -> TestTree
requestDescribeStreamProcessor =
  req
    "DescribeStreamProcessor"
    "fixture/DescribeStreamProcessor.yaml"

requestDeleteCollection :: DeleteCollection -> TestTree
requestDeleteCollection =
  req
    "DeleteCollection"
    "fixture/DeleteCollection.yaml"

requestGetLabelDetection :: GetLabelDetection -> TestTree
requestGetLabelDetection =
  req
    "GetLabelDetection"
    "fixture/GetLabelDetection.yaml"

requestSearchFaces :: SearchFaces -> TestTree
requestSearchFaces =
  req
    "SearchFaces"
    "fixture/SearchFaces.yaml"

requestGetTextDetection :: GetTextDetection -> TestTree
requestGetTextDetection =
  req
    "GetTextDetection"
    "fixture/GetTextDetection.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestDetectCustomLabels :: DetectCustomLabels -> TestTree
requestDetectCustomLabels =
  req
    "DetectCustomLabels"
    "fixture/DetectCustomLabels.yaml"

requestRecognizeCelebrities :: RecognizeCelebrities -> TestTree
requestRecognizeCelebrities =
  req
    "RecognizeCelebrities"
    "fixture/RecognizeCelebrities.yaml"

requestStartFaceDetection :: StartFaceDetection -> TestTree
requestStartFaceDetection =
  req
    "StartFaceDetection"
    "fixture/StartFaceDetection.yaml"

requestDetectModerationLabels :: DetectModerationLabels -> TestTree
requestDetectModerationLabels =
  req
    "DetectModerationLabels"
    "fixture/DetectModerationLabels.yaml"

requestDeleteFaces :: DeleteFaces -> TestTree
requestDeleteFaces =
  req
    "DeleteFaces"
    "fixture/DeleteFaces.yaml"

requestListStreamProcessors :: ListStreamProcessors -> TestTree
requestListStreamProcessors =
  req
    "ListStreamProcessors"
    "fixture/ListStreamProcessors.yaml"

requestDescribeCollection :: DescribeCollection -> TestTree
requestDescribeCollection =
  req
    "DescribeCollection"
    "fixture/DescribeCollection.yaml"

requestDeleteStreamProcessor :: DeleteStreamProcessor -> TestTree
requestDeleteStreamProcessor =
  req
    "DeleteStreamProcessor"
    "fixture/DeleteStreamProcessor.yaml"

requestListFaces :: ListFaces -> TestTree
requestListFaces =
  req
    "ListFaces"
    "fixture/ListFaces.yaml"

requestSearchFacesByImage :: SearchFacesByImage -> TestTree
requestSearchFacesByImage =
  req
    "SearchFacesByImage"
    "fixture/SearchFacesByImage.yaml"

requestCompareFaces :: CompareFaces -> TestTree
requestCompareFaces =
  req
    "CompareFaces"
    "fixture/CompareFaces.yaml"

requestDetectLabels :: DetectLabels -> TestTree
requestDetectLabels =
  req
    "DetectLabels"
    "fixture/DetectLabels.yaml"

requestGetSegmentDetection :: GetSegmentDetection -> TestTree
requestGetSegmentDetection =
  req
    "GetSegmentDetection"
    "fixture/GetSegmentDetection.yaml"

requestGetCelebrityRecognition :: GetCelebrityRecognition -> TestTree
requestGetCelebrityRecognition =
  req
    "GetCelebrityRecognition"
    "fixture/GetCelebrityRecognition.yaml"

requestStartPersonTracking :: StartPersonTracking -> TestTree
requestStartPersonTracking =
  req
    "StartPersonTracking"
    "fixture/StartPersonTracking.yaml"

requestCreateCollection :: CreateCollection -> TestTree
requestCreateCollection =
  req
    "CreateCollection"
    "fixture/CreateCollection.yaml"

requestStopProjectVersion :: StopProjectVersion -> TestTree
requestStopProjectVersion =
  req
    "StopProjectVersion"
    "fixture/StopProjectVersion.yaml"

requestStartProjectVersion :: StartProjectVersion -> TestTree
requestStartProjectVersion =
  req
    "StartProjectVersion"
    "fixture/StartProjectVersion.yaml"

requestListCollections :: ListCollections -> TestTree
requestListCollections =
  req
    "ListCollections"
    "fixture/ListCollections.yaml"

requestDetectProtectiveEquipment :: DetectProtectiveEquipment -> TestTree
requestDetectProtectiveEquipment =
  req
    "DetectProtectiveEquipment"
    "fixture/DetectProtectiveEquipment.yaml"

requestGetPersonTracking :: GetPersonTracking -> TestTree
requestGetPersonTracking =
  req
    "GetPersonTracking"
    "fixture/GetPersonTracking.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject =
  req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestIndexFaces :: IndexFaces -> TestTree
requestIndexFaces =
  req
    "IndexFaces"
    "fixture/IndexFaces.yaml"

requestStartSegmentDetection :: StartSegmentDetection -> TestTree
requestStartSegmentDetection =
  req
    "StartSegmentDetection"
    "fixture/StartSegmentDetection.yaml"

requestStartCelebrityRecognition :: StartCelebrityRecognition -> TestTree
requestStartCelebrityRecognition =
  req
    "StartCelebrityRecognition"
    "fixture/StartCelebrityRecognition.yaml"

requestGetFaceSearch :: GetFaceSearch -> TestTree
requestGetFaceSearch =
  req
    "GetFaceSearch"
    "fixture/GetFaceSearch.yaml"

requestStartLabelDetection :: StartLabelDetection -> TestTree
requestStartLabelDetection =
  req
    "StartLabelDetection"
    "fixture/StartLabelDetection.yaml"

requestDescribeProjectVersions :: DescribeProjectVersions -> TestTree
requestDescribeProjectVersions =
  req
    "DescribeProjectVersions"
    "fixture/DescribeProjectVersions.yaml"

requestDeleteProjectVersion :: DeleteProjectVersion -> TestTree
requestDeleteProjectVersion =
  req
    "DeleteProjectVersion"
    "fixture/DeleteProjectVersion.yaml"

requestCreateStreamProcessor :: CreateStreamProcessor -> TestTree
requestCreateStreamProcessor =
  req
    "CreateStreamProcessor"
    "fixture/CreateStreamProcessor.yaml"

requestGetContentModeration :: GetContentModeration -> TestTree
requestGetContentModeration =
  req
    "GetContentModeration"
    "fixture/GetContentModeration.yaml"

requestGetCelebrityInfo :: GetCelebrityInfo -> TestTree
requestGetCelebrityInfo =
  req
    "GetCelebrityInfo"
    "fixture/GetCelebrityInfo.yaml"

requestDescribeProjects :: DescribeProjects -> TestTree
requestDescribeProjects =
  req
    "DescribeProjects"
    "fixture/DescribeProjects.yaml"

requestCreateProjectVersion :: CreateProjectVersion -> TestTree
requestCreateProjectVersion =
  req
    "CreateProjectVersion"
    "fixture/CreateProjectVersion.yaml"

requestGetFaceDetection :: GetFaceDetection -> TestTree
requestGetFaceDetection =
  req
    "GetFaceDetection"
    "fixture/GetFaceDetection.yaml"

requestStartContentModeration :: StartContentModeration -> TestTree
requestStartContentModeration =
  req
    "StartContentModeration"
    "fixture/StartContentModeration.yaml"

requestDetectFaces :: DetectFaces -> TestTree
requestDetectFaces =
  req
    "DetectFaces"
    "fixture/DetectFaces.yaml"

requestDetectText :: DetectText -> TestTree
requestDetectText =
  req
    "DetectText"
    "fixture/DetectText.yaml"

requestStartTextDetection :: StartTextDetection -> TestTree
requestStartTextDetection =
  req
    "StartTextDetection"
    "fixture/StartTextDetection.yaml"

requestStopStreamProcessor :: StopStreamProcessor -> TestTree
requestStopStreamProcessor =
  req
    "StopStreamProcessor"
    "fixture/StopStreamProcessor.yaml"

requestStartStreamProcessor :: StartStreamProcessor -> TestTree
requestStartStreamProcessor =
  req
    "StartStreamProcessor"
    "fixture/StartStreamProcessor.yaml"

-- Responses

responseStartFaceSearch :: StartFaceSearchResponse -> TestTree
responseStartFaceSearch =
  res
    "StartFaceSearchResponse"
    "fixture/StartFaceSearchResponse.proto"
    defaultService
    (Proxy :: Proxy StartFaceSearch)

responseDescribeStreamProcessor :: DescribeStreamProcessorResponse -> TestTree
responseDescribeStreamProcessor =
  res
    "DescribeStreamProcessorResponse"
    "fixture/DescribeStreamProcessorResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStreamProcessor)

responseDeleteCollection :: DeleteCollectionResponse -> TestTree
responseDeleteCollection =
  res
    "DeleteCollectionResponse"
    "fixture/DeleteCollectionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCollection)

responseGetLabelDetection :: GetLabelDetectionResponse -> TestTree
responseGetLabelDetection =
  res
    "GetLabelDetectionResponse"
    "fixture/GetLabelDetectionResponse.proto"
    defaultService
    (Proxy :: Proxy GetLabelDetection)

responseSearchFaces :: SearchFacesResponse -> TestTree
responseSearchFaces =
  res
    "SearchFacesResponse"
    "fixture/SearchFacesResponse.proto"
    defaultService
    (Proxy :: Proxy SearchFaces)

responseGetTextDetection :: GetTextDetectionResponse -> TestTree
responseGetTextDetection =
  res
    "GetTextDetectionResponse"
    "fixture/GetTextDetectionResponse.proto"
    defaultService
    (Proxy :: Proxy GetTextDetection)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProject)

responseDetectCustomLabels :: DetectCustomLabelsResponse -> TestTree
responseDetectCustomLabels =
  res
    "DetectCustomLabelsResponse"
    "fixture/DetectCustomLabelsResponse.proto"
    defaultService
    (Proxy :: Proxy DetectCustomLabels)

responseRecognizeCelebrities :: RecognizeCelebritiesResponse -> TestTree
responseRecognizeCelebrities =
  res
    "RecognizeCelebritiesResponse"
    "fixture/RecognizeCelebritiesResponse.proto"
    defaultService
    (Proxy :: Proxy RecognizeCelebrities)

responseStartFaceDetection :: StartFaceDetectionResponse -> TestTree
responseStartFaceDetection =
  res
    "StartFaceDetectionResponse"
    "fixture/StartFaceDetectionResponse.proto"
    defaultService
    (Proxy :: Proxy StartFaceDetection)

responseDetectModerationLabels :: DetectModerationLabelsResponse -> TestTree
responseDetectModerationLabels =
  res
    "DetectModerationLabelsResponse"
    "fixture/DetectModerationLabelsResponse.proto"
    defaultService
    (Proxy :: Proxy DetectModerationLabels)

responseDeleteFaces :: DeleteFacesResponse -> TestTree
responseDeleteFaces =
  res
    "DeleteFacesResponse"
    "fixture/DeleteFacesResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFaces)

responseListStreamProcessors :: ListStreamProcessorsResponse -> TestTree
responseListStreamProcessors =
  res
    "ListStreamProcessorsResponse"
    "fixture/ListStreamProcessorsResponse.proto"
    defaultService
    (Proxy :: Proxy ListStreamProcessors)

responseDescribeCollection :: DescribeCollectionResponse -> TestTree
responseDescribeCollection =
  res
    "DescribeCollectionResponse"
    "fixture/DescribeCollectionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCollection)

responseDeleteStreamProcessor :: DeleteStreamProcessorResponse -> TestTree
responseDeleteStreamProcessor =
  res
    "DeleteStreamProcessorResponse"
    "fixture/DeleteStreamProcessorResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStreamProcessor)

responseListFaces :: ListFacesResponse -> TestTree
responseListFaces =
  res
    "ListFacesResponse"
    "fixture/ListFacesResponse.proto"
    defaultService
    (Proxy :: Proxy ListFaces)

responseSearchFacesByImage :: SearchFacesByImageResponse -> TestTree
responseSearchFacesByImage =
  res
    "SearchFacesByImageResponse"
    "fixture/SearchFacesByImageResponse.proto"
    defaultService
    (Proxy :: Proxy SearchFacesByImage)

responseCompareFaces :: CompareFacesResponse -> TestTree
responseCompareFaces =
  res
    "CompareFacesResponse"
    "fixture/CompareFacesResponse.proto"
    defaultService
    (Proxy :: Proxy CompareFaces)

responseDetectLabels :: DetectLabelsResponse -> TestTree
responseDetectLabels =
  res
    "DetectLabelsResponse"
    "fixture/DetectLabelsResponse.proto"
    defaultService
    (Proxy :: Proxy DetectLabels)

responseGetSegmentDetection :: GetSegmentDetectionResponse -> TestTree
responseGetSegmentDetection =
  res
    "GetSegmentDetectionResponse"
    "fixture/GetSegmentDetectionResponse.proto"
    defaultService
    (Proxy :: Proxy GetSegmentDetection)

responseGetCelebrityRecognition :: GetCelebrityRecognitionResponse -> TestTree
responseGetCelebrityRecognition =
  res
    "GetCelebrityRecognitionResponse"
    "fixture/GetCelebrityRecognitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetCelebrityRecognition)

responseStartPersonTracking :: StartPersonTrackingResponse -> TestTree
responseStartPersonTracking =
  res
    "StartPersonTrackingResponse"
    "fixture/StartPersonTrackingResponse.proto"
    defaultService
    (Proxy :: Proxy StartPersonTracking)

responseCreateCollection :: CreateCollectionResponse -> TestTree
responseCreateCollection =
  res
    "CreateCollectionResponse"
    "fixture/CreateCollectionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCollection)

responseStopProjectVersion :: StopProjectVersionResponse -> TestTree
responseStopProjectVersion =
  res
    "StopProjectVersionResponse"
    "fixture/StopProjectVersionResponse.proto"
    defaultService
    (Proxy :: Proxy StopProjectVersion)

responseStartProjectVersion :: StartProjectVersionResponse -> TestTree
responseStartProjectVersion =
  res
    "StartProjectVersionResponse"
    "fixture/StartProjectVersionResponse.proto"
    defaultService
    (Proxy :: Proxy StartProjectVersion)

responseListCollections :: ListCollectionsResponse -> TestTree
responseListCollections =
  res
    "ListCollectionsResponse"
    "fixture/ListCollectionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListCollections)

responseDetectProtectiveEquipment :: DetectProtectiveEquipmentResponse -> TestTree
responseDetectProtectiveEquipment =
  res
    "DetectProtectiveEquipmentResponse"
    "fixture/DetectProtectiveEquipmentResponse.proto"
    defaultService
    (Proxy :: Proxy DetectProtectiveEquipment)

responseGetPersonTracking :: GetPersonTrackingResponse -> TestTree
responseGetPersonTracking =
  res
    "GetPersonTrackingResponse"
    "fixture/GetPersonTrackingResponse.proto"
    defaultService
    (Proxy :: Proxy GetPersonTracking)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProject)

responseIndexFaces :: IndexFacesResponse -> TestTree
responseIndexFaces =
  res
    "IndexFacesResponse"
    "fixture/IndexFacesResponse.proto"
    defaultService
    (Proxy :: Proxy IndexFaces)

responseStartSegmentDetection :: StartSegmentDetectionResponse -> TestTree
responseStartSegmentDetection =
  res
    "StartSegmentDetectionResponse"
    "fixture/StartSegmentDetectionResponse.proto"
    defaultService
    (Proxy :: Proxy StartSegmentDetection)

responseStartCelebrityRecognition :: StartCelebrityRecognitionResponse -> TestTree
responseStartCelebrityRecognition =
  res
    "StartCelebrityRecognitionResponse"
    "fixture/StartCelebrityRecognitionResponse.proto"
    defaultService
    (Proxy :: Proxy StartCelebrityRecognition)

responseGetFaceSearch :: GetFaceSearchResponse -> TestTree
responseGetFaceSearch =
  res
    "GetFaceSearchResponse"
    "fixture/GetFaceSearchResponse.proto"
    defaultService
    (Proxy :: Proxy GetFaceSearch)

responseStartLabelDetection :: StartLabelDetectionResponse -> TestTree
responseStartLabelDetection =
  res
    "StartLabelDetectionResponse"
    "fixture/StartLabelDetectionResponse.proto"
    defaultService
    (Proxy :: Proxy StartLabelDetection)

responseDescribeProjectVersions :: DescribeProjectVersionsResponse -> TestTree
responseDescribeProjectVersions =
  res
    "DescribeProjectVersionsResponse"
    "fixture/DescribeProjectVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProjectVersions)

responseDeleteProjectVersion :: DeleteProjectVersionResponse -> TestTree
responseDeleteProjectVersion =
  res
    "DeleteProjectVersionResponse"
    "fixture/DeleteProjectVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProjectVersion)

responseCreateStreamProcessor :: CreateStreamProcessorResponse -> TestTree
responseCreateStreamProcessor =
  res
    "CreateStreamProcessorResponse"
    "fixture/CreateStreamProcessorResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStreamProcessor)

responseGetContentModeration :: GetContentModerationResponse -> TestTree
responseGetContentModeration =
  res
    "GetContentModerationResponse"
    "fixture/GetContentModerationResponse.proto"
    defaultService
    (Proxy :: Proxy GetContentModeration)

responseGetCelebrityInfo :: GetCelebrityInfoResponse -> TestTree
responseGetCelebrityInfo =
  res
    "GetCelebrityInfoResponse"
    "fixture/GetCelebrityInfoResponse.proto"
    defaultService
    (Proxy :: Proxy GetCelebrityInfo)

responseDescribeProjects :: DescribeProjectsResponse -> TestTree
responseDescribeProjects =
  res
    "DescribeProjectsResponse"
    "fixture/DescribeProjectsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProjects)

responseCreateProjectVersion :: CreateProjectVersionResponse -> TestTree
responseCreateProjectVersion =
  res
    "CreateProjectVersionResponse"
    "fixture/CreateProjectVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProjectVersion)

responseGetFaceDetection :: GetFaceDetectionResponse -> TestTree
responseGetFaceDetection =
  res
    "GetFaceDetectionResponse"
    "fixture/GetFaceDetectionResponse.proto"
    defaultService
    (Proxy :: Proxy GetFaceDetection)

responseStartContentModeration :: StartContentModerationResponse -> TestTree
responseStartContentModeration =
  res
    "StartContentModerationResponse"
    "fixture/StartContentModerationResponse.proto"
    defaultService
    (Proxy :: Proxy StartContentModeration)

responseDetectFaces :: DetectFacesResponse -> TestTree
responseDetectFaces =
  res
    "DetectFacesResponse"
    "fixture/DetectFacesResponse.proto"
    defaultService
    (Proxy :: Proxy DetectFaces)

responseDetectText :: DetectTextResponse -> TestTree
responseDetectText =
  res
    "DetectTextResponse"
    "fixture/DetectTextResponse.proto"
    defaultService
    (Proxy :: Proxy DetectText)

responseStartTextDetection :: StartTextDetectionResponse -> TestTree
responseStartTextDetection =
  res
    "StartTextDetectionResponse"
    "fixture/StartTextDetectionResponse.proto"
    defaultService
    (Proxy :: Proxy StartTextDetection)

responseStopStreamProcessor :: StopStreamProcessorResponse -> TestTree
responseStopStreamProcessor =
  res
    "StopStreamProcessorResponse"
    "fixture/StopStreamProcessorResponse.proto"
    defaultService
    (Proxy :: Proxy StopStreamProcessor)

responseStartStreamProcessor :: StartStreamProcessorResponse -> TestTree
responseStartStreamProcessor =
  res
    "StartStreamProcessorResponse"
    "fixture/StartStreamProcessorResponse.proto"
    defaultService
    (Proxy :: Proxy StartStreamProcessor)

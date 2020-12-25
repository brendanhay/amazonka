{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Rekognition
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestDetectProtectiveEquipment $
--             mkDetectProtectiveEquipment
--
--         , requestDeleteProject $
--             mkDeleteProject
--
--         , requestStartCelebrityRecognition $
--             mkStartCelebrityRecognition
--
--         , requestGetPersonTracking $
--             mkGetPersonTracking
--
--         , requestGetTextDetection $
--             mkGetTextDetection
--
--         , requestStartSegmentDetection $
--             mkStartSegmentDetection
--
--         , requestListCollections $
--             mkListCollections
--
--         , requestStartProjectVersion $
--             mkStartProjectVersion
--
--         , requestDeleteCollection $
--             mkDeleteCollection
--
--         , requestCreateCollection $
--             mkCreateCollection
--
--         , requestStopStreamProcessor $
--             mkStopStreamProcessor
--
--         , requestDetectLabels $
--             mkDetectLabels
--
--         , requestStartContentModeration $
--             mkStartContentModeration
--
--         , requestSearchFacesByImage $
--             mkSearchFacesByImage
--
--         , requestListStreamProcessors $
--             mkListStreamProcessors
--
--         , requestDescribeCollection $
--             mkDescribeCollection
--
--         , requestDeleteProjectVersion $
--             mkDeleteProjectVersion
--
--         , requestDescribeProjectVersions $
--             mkDescribeProjectVersions
--
--         , requestRecognizeCelebrities $
--             mkRecognizeCelebrities
--
--         , requestDetectCustomLabels $
--             mkDetectCustomLabels
--
--         , requestGetFaceSearch $
--             mkGetFaceSearch
--
--         , requestStartLabelDetection $
--             mkStartLabelDetection
--
--         , requestSearchFaces $
--             mkSearchFaces
--
--         , requestIndexFaces $
--             mkIndexFaces
--
--         , requestGetLabelDetection $
--             mkGetLabelDetection
--
--         , requestStopProjectVersion $
--             mkStopProjectVersion
--
--         , requestDescribeStreamProcessor $
--             mkDescribeStreamProcessor
--
--         , requestStartFaceSearch $
--             mkStartFaceSearch
--
--         , requestStartTextDetection $
--             mkStartTextDetection
--
--         , requestStartPersonTracking $
--             mkStartPersonTracking
--
--         , requestGetCelebrityRecognition $
--             mkGetCelebrityRecognition
--
--         , requestStartStreamProcessor $
--             mkStartStreamProcessor
--
--         , requestDetectText $
--             mkDetectText
--
--         , requestGetSegmentDetection $
--             mkGetSegmentDetection
--
--         , requestCompareFaces $
--             mkCompareFaces
--
--         , requestDetectFaces $
--             mkDetectFaces
--
--         , requestGetFaceDetection $
--             mkGetFaceDetection
--
--         , requestListFaces $
--             mkListFaces
--
--         , requestCreateProjectVersion $
--             mkCreateProjectVersion
--
--         , requestDescribeProjects $
--             mkDescribeProjects
--
--         , requestGetContentModeration $
--             mkGetContentModeration
--
--         , requestDeleteFaces $
--             mkDeleteFaces
--
--         , requestGetCelebrityInfo $
--             mkGetCelebrityInfo
--
--         , requestDeleteStreamProcessor $
--             mkDeleteStreamProcessor
--
--         , requestDetectModerationLabels $
--             mkDetectModerationLabels
--
--         , requestCreateStreamProcessor $
--             mkCreateStreamProcessor
--
--         , requestStartFaceDetection $
--             mkStartFaceDetection
--
--         , requestCreateProject $
--             mkCreateProject
--
--           ]

--     , testGroup "response"
--         [ responseDetectProtectiveEquipment $
--             mkDetectProtectiveEquipmentResponse
--
--         , responseDeleteProject $
--             mkDeleteProjectResponse
--
--         , responseStartCelebrityRecognition $
--             mkStartCelebrityRecognitionResponse
--
--         , responseGetPersonTracking $
--             mkGetPersonTrackingResponse
--
--         , responseGetTextDetection $
--             mkGetTextDetectionResponse
--
--         , responseStartSegmentDetection $
--             mkStartSegmentDetectionResponse
--
--         , responseListCollections $
--             mkListCollectionsResponse
--
--         , responseStartProjectVersion $
--             mkStartProjectVersionResponse
--
--         , responseDeleteCollection $
--             mkDeleteCollectionResponse
--
--         , responseCreateCollection $
--             mkCreateCollectionResponse
--
--         , responseStopStreamProcessor $
--             mkStopStreamProcessorResponse
--
--         , responseDetectLabels $
--             mkDetectLabelsResponse
--
--         , responseStartContentModeration $
--             mkStartContentModerationResponse
--
--         , responseSearchFacesByImage $
--             mkSearchFacesByImageResponse
--
--         , responseListStreamProcessors $
--             mkListStreamProcessorsResponse
--
--         , responseDescribeCollection $
--             mkDescribeCollectionResponse
--
--         , responseDeleteProjectVersion $
--             mkDeleteProjectVersionResponse
--
--         , responseDescribeProjectVersions $
--             mkDescribeProjectVersionsResponse
--
--         , responseRecognizeCelebrities $
--             mkRecognizeCelebritiesResponse
--
--         , responseDetectCustomLabels $
--             mkDetectCustomLabelsResponse
--
--         , responseGetFaceSearch $
--             mkGetFaceSearchResponse
--
--         , responseStartLabelDetection $
--             mkStartLabelDetectionResponse
--
--         , responseSearchFaces $
--             mkSearchFacesResponse
--
--         , responseIndexFaces $
--             mkIndexFacesResponse
--
--         , responseGetLabelDetection $
--             mkGetLabelDetectionResponse
--
--         , responseStopProjectVersion $
--             mkStopProjectVersionResponse
--
--         , responseDescribeStreamProcessor $
--             mkDescribeStreamProcessorResponse
--
--         , responseStartFaceSearch $
--             mkStartFaceSearchResponse
--
--         , responseStartTextDetection $
--             mkStartTextDetectionResponse
--
--         , responseStartPersonTracking $
--             mkStartPersonTrackingResponse
--
--         , responseGetCelebrityRecognition $
--             mkGetCelebrityRecognitionResponse
--
--         , responseStartStreamProcessor $
--             mkStartStreamProcessorResponse
--
--         , responseDetectText $
--             mkDetectTextResponse
--
--         , responseGetSegmentDetection $
--             mkGetSegmentDetectionResponse
--
--         , responseCompareFaces $
--             mkCompareFacesResponse
--
--         , responseDetectFaces $
--             mkDetectFacesResponse
--
--         , responseGetFaceDetection $
--             mkGetFaceDetectionResponse
--
--         , responseListFaces $
--             mkListFacesResponse
--
--         , responseCreateProjectVersion $
--             mkCreateProjectVersionResponse
--
--         , responseDescribeProjects $
--             mkDescribeProjectsResponse
--
--         , responseGetContentModeration $
--             mkGetContentModerationResponse
--
--         , responseDeleteFaces $
--             mkDeleteFacesResponse
--
--         , responseGetCelebrityInfo $
--             mkGetCelebrityInfoResponse
--
--         , responseDeleteStreamProcessor $
--             mkDeleteStreamProcessorResponse
--
--         , responseDetectModerationLabels $
--             mkDetectModerationLabelsResponse
--
--         , responseCreateStreamProcessor $
--             mkCreateStreamProcessorResponse
--
--         , responseStartFaceDetection $
--             mkStartFaceDetectionResponse
--
--         , responseCreateProject $
--             mkCreateProjectResponse
--
--           ]
--     ]

-- Requests

requestDetectProtectiveEquipment :: DetectProtectiveEquipment -> TestTree
requestDetectProtectiveEquipment =
  req
    "DetectProtectiveEquipment"
    "fixture/DetectProtectiveEquipment.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject =
  req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestStartCelebrityRecognition :: StartCelebrityRecognition -> TestTree
requestStartCelebrityRecognition =
  req
    "StartCelebrityRecognition"
    "fixture/StartCelebrityRecognition.yaml"

requestGetPersonTracking :: GetPersonTracking -> TestTree
requestGetPersonTracking =
  req
    "GetPersonTracking"
    "fixture/GetPersonTracking.yaml"

requestGetTextDetection :: GetTextDetection -> TestTree
requestGetTextDetection =
  req
    "GetTextDetection"
    "fixture/GetTextDetection.yaml"

requestStartSegmentDetection :: StartSegmentDetection -> TestTree
requestStartSegmentDetection =
  req
    "StartSegmentDetection"
    "fixture/StartSegmentDetection.yaml"

requestListCollections :: ListCollections -> TestTree
requestListCollections =
  req
    "ListCollections"
    "fixture/ListCollections.yaml"

requestStartProjectVersion :: StartProjectVersion -> TestTree
requestStartProjectVersion =
  req
    "StartProjectVersion"
    "fixture/StartProjectVersion.yaml"

requestDeleteCollection :: DeleteCollection -> TestTree
requestDeleteCollection =
  req
    "DeleteCollection"
    "fixture/DeleteCollection.yaml"

requestCreateCollection :: CreateCollection -> TestTree
requestCreateCollection =
  req
    "CreateCollection"
    "fixture/CreateCollection.yaml"

requestStopStreamProcessor :: StopStreamProcessor -> TestTree
requestStopStreamProcessor =
  req
    "StopStreamProcessor"
    "fixture/StopStreamProcessor.yaml"

requestDetectLabels :: DetectLabels -> TestTree
requestDetectLabels =
  req
    "DetectLabels"
    "fixture/DetectLabels.yaml"

requestStartContentModeration :: StartContentModeration -> TestTree
requestStartContentModeration =
  req
    "StartContentModeration"
    "fixture/StartContentModeration.yaml"

requestSearchFacesByImage :: SearchFacesByImage -> TestTree
requestSearchFacesByImage =
  req
    "SearchFacesByImage"
    "fixture/SearchFacesByImage.yaml"

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

requestDeleteProjectVersion :: DeleteProjectVersion -> TestTree
requestDeleteProjectVersion =
  req
    "DeleteProjectVersion"
    "fixture/DeleteProjectVersion.yaml"

requestDescribeProjectVersions :: DescribeProjectVersions -> TestTree
requestDescribeProjectVersions =
  req
    "DescribeProjectVersions"
    "fixture/DescribeProjectVersions.yaml"

requestRecognizeCelebrities :: RecognizeCelebrities -> TestTree
requestRecognizeCelebrities =
  req
    "RecognizeCelebrities"
    "fixture/RecognizeCelebrities.yaml"

requestDetectCustomLabels :: DetectCustomLabels -> TestTree
requestDetectCustomLabels =
  req
    "DetectCustomLabels"
    "fixture/DetectCustomLabels.yaml"

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

requestSearchFaces :: SearchFaces -> TestTree
requestSearchFaces =
  req
    "SearchFaces"
    "fixture/SearchFaces.yaml"

requestIndexFaces :: IndexFaces -> TestTree
requestIndexFaces =
  req
    "IndexFaces"
    "fixture/IndexFaces.yaml"

requestGetLabelDetection :: GetLabelDetection -> TestTree
requestGetLabelDetection =
  req
    "GetLabelDetection"
    "fixture/GetLabelDetection.yaml"

requestStopProjectVersion :: StopProjectVersion -> TestTree
requestStopProjectVersion =
  req
    "StopProjectVersion"
    "fixture/StopProjectVersion.yaml"

requestDescribeStreamProcessor :: DescribeStreamProcessor -> TestTree
requestDescribeStreamProcessor =
  req
    "DescribeStreamProcessor"
    "fixture/DescribeStreamProcessor.yaml"

requestStartFaceSearch :: StartFaceSearch -> TestTree
requestStartFaceSearch =
  req
    "StartFaceSearch"
    "fixture/StartFaceSearch.yaml"

requestStartTextDetection :: StartTextDetection -> TestTree
requestStartTextDetection =
  req
    "StartTextDetection"
    "fixture/StartTextDetection.yaml"

requestStartPersonTracking :: StartPersonTracking -> TestTree
requestStartPersonTracking =
  req
    "StartPersonTracking"
    "fixture/StartPersonTracking.yaml"

requestGetCelebrityRecognition :: GetCelebrityRecognition -> TestTree
requestGetCelebrityRecognition =
  req
    "GetCelebrityRecognition"
    "fixture/GetCelebrityRecognition.yaml"

requestStartStreamProcessor :: StartStreamProcessor -> TestTree
requestStartStreamProcessor =
  req
    "StartStreamProcessor"
    "fixture/StartStreamProcessor.yaml"

requestDetectText :: DetectText -> TestTree
requestDetectText =
  req
    "DetectText"
    "fixture/DetectText.yaml"

requestGetSegmentDetection :: GetSegmentDetection -> TestTree
requestGetSegmentDetection =
  req
    "GetSegmentDetection"
    "fixture/GetSegmentDetection.yaml"

requestCompareFaces :: CompareFaces -> TestTree
requestCompareFaces =
  req
    "CompareFaces"
    "fixture/CompareFaces.yaml"

requestDetectFaces :: DetectFaces -> TestTree
requestDetectFaces =
  req
    "DetectFaces"
    "fixture/DetectFaces.yaml"

requestGetFaceDetection :: GetFaceDetection -> TestTree
requestGetFaceDetection =
  req
    "GetFaceDetection"
    "fixture/GetFaceDetection.yaml"

requestListFaces :: ListFaces -> TestTree
requestListFaces =
  req
    "ListFaces"
    "fixture/ListFaces.yaml"

requestCreateProjectVersion :: CreateProjectVersion -> TestTree
requestCreateProjectVersion =
  req
    "CreateProjectVersion"
    "fixture/CreateProjectVersion.yaml"

requestDescribeProjects :: DescribeProjects -> TestTree
requestDescribeProjects =
  req
    "DescribeProjects"
    "fixture/DescribeProjects.yaml"

requestGetContentModeration :: GetContentModeration -> TestTree
requestGetContentModeration =
  req
    "GetContentModeration"
    "fixture/GetContentModeration.yaml"

requestDeleteFaces :: DeleteFaces -> TestTree
requestDeleteFaces =
  req
    "DeleteFaces"
    "fixture/DeleteFaces.yaml"

requestGetCelebrityInfo :: GetCelebrityInfo -> TestTree
requestGetCelebrityInfo =
  req
    "GetCelebrityInfo"
    "fixture/GetCelebrityInfo.yaml"

requestDeleteStreamProcessor :: DeleteStreamProcessor -> TestTree
requestDeleteStreamProcessor =
  req
    "DeleteStreamProcessor"
    "fixture/DeleteStreamProcessor.yaml"

requestDetectModerationLabels :: DetectModerationLabels -> TestTree
requestDetectModerationLabels =
  req
    "DetectModerationLabels"
    "fixture/DetectModerationLabels.yaml"

requestCreateStreamProcessor :: CreateStreamProcessor -> TestTree
requestCreateStreamProcessor =
  req
    "CreateStreamProcessor"
    "fixture/CreateStreamProcessor.yaml"

requestStartFaceDetection :: StartFaceDetection -> TestTree
requestStartFaceDetection =
  req
    "StartFaceDetection"
    "fixture/StartFaceDetection.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

-- Responses

responseDetectProtectiveEquipment :: DetectProtectiveEquipmentResponse -> TestTree
responseDetectProtectiveEquipment =
  res
    "DetectProtectiveEquipmentResponse"
    "fixture/DetectProtectiveEquipmentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetectProtectiveEquipment)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteProject)

responseStartCelebrityRecognition :: StartCelebrityRecognitionResponse -> TestTree
responseStartCelebrityRecognition =
  res
    "StartCelebrityRecognitionResponse"
    "fixture/StartCelebrityRecognitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartCelebrityRecognition)

responseGetPersonTracking :: GetPersonTrackingResponse -> TestTree
responseGetPersonTracking =
  res
    "GetPersonTrackingResponse"
    "fixture/GetPersonTrackingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetPersonTracking)

responseGetTextDetection :: GetTextDetectionResponse -> TestTree
responseGetTextDetection =
  res
    "GetTextDetectionResponse"
    "fixture/GetTextDetectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetTextDetection)

responseStartSegmentDetection :: StartSegmentDetectionResponse -> TestTree
responseStartSegmentDetection =
  res
    "StartSegmentDetectionResponse"
    "fixture/StartSegmentDetectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartSegmentDetection)

responseListCollections :: ListCollectionsResponse -> TestTree
responseListCollections =
  res
    "ListCollectionsResponse"
    "fixture/ListCollectionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListCollections)

responseStartProjectVersion :: StartProjectVersionResponse -> TestTree
responseStartProjectVersion =
  res
    "StartProjectVersionResponse"
    "fixture/StartProjectVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartProjectVersion)

responseDeleteCollection :: DeleteCollectionResponse -> TestTree
responseDeleteCollection =
  res
    "DeleteCollectionResponse"
    "fixture/DeleteCollectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteCollection)

responseCreateCollection :: CreateCollectionResponse -> TestTree
responseCreateCollection =
  res
    "CreateCollectionResponse"
    "fixture/CreateCollectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCollection)

responseStopStreamProcessor :: StopStreamProcessorResponse -> TestTree
responseStopStreamProcessor =
  res
    "StopStreamProcessorResponse"
    "fixture/StopStreamProcessorResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopStreamProcessor)

responseDetectLabels :: DetectLabelsResponse -> TestTree
responseDetectLabels =
  res
    "DetectLabelsResponse"
    "fixture/DetectLabelsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetectLabels)

responseStartContentModeration :: StartContentModerationResponse -> TestTree
responseStartContentModeration =
  res
    "StartContentModerationResponse"
    "fixture/StartContentModerationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartContentModeration)

responseSearchFacesByImage :: SearchFacesByImageResponse -> TestTree
responseSearchFacesByImage =
  res
    "SearchFacesByImageResponse"
    "fixture/SearchFacesByImageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SearchFacesByImage)

responseListStreamProcessors :: ListStreamProcessorsResponse -> TestTree
responseListStreamProcessors =
  res
    "ListStreamProcessorsResponse"
    "fixture/ListStreamProcessorsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListStreamProcessors)

responseDescribeCollection :: DescribeCollectionResponse -> TestTree
responseDescribeCollection =
  res
    "DescribeCollectionResponse"
    "fixture/DescribeCollectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCollection)

responseDeleteProjectVersion :: DeleteProjectVersionResponse -> TestTree
responseDeleteProjectVersion =
  res
    "DeleteProjectVersionResponse"
    "fixture/DeleteProjectVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteProjectVersion)

responseDescribeProjectVersions :: DescribeProjectVersionsResponse -> TestTree
responseDescribeProjectVersions =
  res
    "DescribeProjectVersionsResponse"
    "fixture/DescribeProjectVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeProjectVersions)

responseRecognizeCelebrities :: RecognizeCelebritiesResponse -> TestTree
responseRecognizeCelebrities =
  res
    "RecognizeCelebritiesResponse"
    "fixture/RecognizeCelebritiesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RecognizeCelebrities)

responseDetectCustomLabels :: DetectCustomLabelsResponse -> TestTree
responseDetectCustomLabels =
  res
    "DetectCustomLabelsResponse"
    "fixture/DetectCustomLabelsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetectCustomLabels)

responseGetFaceSearch :: GetFaceSearchResponse -> TestTree
responseGetFaceSearch =
  res
    "GetFaceSearchResponse"
    "fixture/GetFaceSearchResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetFaceSearch)

responseStartLabelDetection :: StartLabelDetectionResponse -> TestTree
responseStartLabelDetection =
  res
    "StartLabelDetectionResponse"
    "fixture/StartLabelDetectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartLabelDetection)

responseSearchFaces :: SearchFacesResponse -> TestTree
responseSearchFaces =
  res
    "SearchFacesResponse"
    "fixture/SearchFacesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SearchFaces)

responseIndexFaces :: IndexFacesResponse -> TestTree
responseIndexFaces =
  res
    "IndexFacesResponse"
    "fixture/IndexFacesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy IndexFaces)

responseGetLabelDetection :: GetLabelDetectionResponse -> TestTree
responseGetLabelDetection =
  res
    "GetLabelDetectionResponse"
    "fixture/GetLabelDetectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetLabelDetection)

responseStopProjectVersion :: StopProjectVersionResponse -> TestTree
responseStopProjectVersion =
  res
    "StopProjectVersionResponse"
    "fixture/StopProjectVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopProjectVersion)

responseDescribeStreamProcessor :: DescribeStreamProcessorResponse -> TestTree
responseDescribeStreamProcessor =
  res
    "DescribeStreamProcessorResponse"
    "fixture/DescribeStreamProcessorResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeStreamProcessor)

responseStartFaceSearch :: StartFaceSearchResponse -> TestTree
responseStartFaceSearch =
  res
    "StartFaceSearchResponse"
    "fixture/StartFaceSearchResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartFaceSearch)

responseStartTextDetection :: StartTextDetectionResponse -> TestTree
responseStartTextDetection =
  res
    "StartTextDetectionResponse"
    "fixture/StartTextDetectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartTextDetection)

responseStartPersonTracking :: StartPersonTrackingResponse -> TestTree
responseStartPersonTracking =
  res
    "StartPersonTrackingResponse"
    "fixture/StartPersonTrackingResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartPersonTracking)

responseGetCelebrityRecognition :: GetCelebrityRecognitionResponse -> TestTree
responseGetCelebrityRecognition =
  res
    "GetCelebrityRecognitionResponse"
    "fixture/GetCelebrityRecognitionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCelebrityRecognition)

responseStartStreamProcessor :: StartStreamProcessorResponse -> TestTree
responseStartStreamProcessor =
  res
    "StartStreamProcessorResponse"
    "fixture/StartStreamProcessorResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartStreamProcessor)

responseDetectText :: DetectTextResponse -> TestTree
responseDetectText =
  res
    "DetectTextResponse"
    "fixture/DetectTextResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetectText)

responseGetSegmentDetection :: GetSegmentDetectionResponse -> TestTree
responseGetSegmentDetection =
  res
    "GetSegmentDetectionResponse"
    "fixture/GetSegmentDetectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSegmentDetection)

responseCompareFaces :: CompareFacesResponse -> TestTree
responseCompareFaces =
  res
    "CompareFacesResponse"
    "fixture/CompareFacesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CompareFaces)

responseDetectFaces :: DetectFacesResponse -> TestTree
responseDetectFaces =
  res
    "DetectFacesResponse"
    "fixture/DetectFacesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetectFaces)

responseGetFaceDetection :: GetFaceDetectionResponse -> TestTree
responseGetFaceDetection =
  res
    "GetFaceDetectionResponse"
    "fixture/GetFaceDetectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetFaceDetection)

responseListFaces :: ListFacesResponse -> TestTree
responseListFaces =
  res
    "ListFacesResponse"
    "fixture/ListFacesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListFaces)

responseCreateProjectVersion :: CreateProjectVersionResponse -> TestTree
responseCreateProjectVersion =
  res
    "CreateProjectVersionResponse"
    "fixture/CreateProjectVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateProjectVersion)

responseDescribeProjects :: DescribeProjectsResponse -> TestTree
responseDescribeProjects =
  res
    "DescribeProjectsResponse"
    "fixture/DescribeProjectsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeProjects)

responseGetContentModeration :: GetContentModerationResponse -> TestTree
responseGetContentModeration =
  res
    "GetContentModerationResponse"
    "fixture/GetContentModerationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetContentModeration)

responseDeleteFaces :: DeleteFacesResponse -> TestTree
responseDeleteFaces =
  res
    "DeleteFacesResponse"
    "fixture/DeleteFacesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteFaces)

responseGetCelebrityInfo :: GetCelebrityInfoResponse -> TestTree
responseGetCelebrityInfo =
  res
    "GetCelebrityInfoResponse"
    "fixture/GetCelebrityInfoResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCelebrityInfo)

responseDeleteStreamProcessor :: DeleteStreamProcessorResponse -> TestTree
responseDeleteStreamProcessor =
  res
    "DeleteStreamProcessorResponse"
    "fixture/DeleteStreamProcessorResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteStreamProcessor)

responseDetectModerationLabels :: DetectModerationLabelsResponse -> TestTree
responseDetectModerationLabels =
  res
    "DetectModerationLabelsResponse"
    "fixture/DetectModerationLabelsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetectModerationLabels)

responseCreateStreamProcessor :: CreateStreamProcessorResponse -> TestTree
responseCreateStreamProcessor =
  res
    "CreateStreamProcessorResponse"
    "fixture/CreateStreamProcessorResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateStreamProcessor)

responseStartFaceDetection :: StartFaceDetectionResponse -> TestTree
responseStartFaceDetection =
  res
    "StartFaceDetectionResponse"
    "fixture/StartFaceDetectionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartFaceDetection)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateProject)

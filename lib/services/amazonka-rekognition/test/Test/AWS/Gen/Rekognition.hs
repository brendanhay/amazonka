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
--         , requestGetLabelDetection $
--             newGetLabelDetection
--
--         , requestDeleteCollection $
--             newDeleteCollection
--
--         , requestDescribeStreamProcessor $
--             newDescribeStreamProcessor
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
--         , requestListStreamProcessors $
--             newListStreamProcessors
--
--         , requestDeleteStreamProcessor $
--             newDeleteStreamProcessor
--
--         , requestDescribeCollection $
--             newDescribeCollection
--
--         , requestDeleteFaces $
--             newDeleteFaces
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListFaces $
--             newListFaces
--
--         , requestTagResource $
--             newTagResource
--
--         , requestSearchFacesByImage $
--             newSearchFacesByImage
--
--         , requestCompareFaces $
--             newCompareFaces
--
--         , requestCreateCollection $
--             newCreateCollection
--
--         , requestDetectLabels $
--             newDetectLabels
--
--         , requestGetSegmentDetection $
--             newGetSegmentDetection
--
--         , requestStartPersonTracking $
--             newStartPersonTracking
--
--         , requestGetCelebrityRecognition $
--             newGetCelebrityRecognition
--
--         , requestStartProjectVersion $
--             newStartProjectVersion
--
--         , requestListCollections $
--             newListCollections
--
--         , requestStopProjectVersion $
--             newStopProjectVersion
--
--         , requestIndexFaces $
--             newIndexFaces
--
--         , requestStartSegmentDetection $
--             newStartSegmentDetection
--
--         , requestDetectProtectiveEquipment $
--             newDetectProtectiveEquipment
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestStartCelebrityRecognition $
--             newStartCelebrityRecognition
--
--         , requestGetPersonTracking $
--             newGetPersonTracking
--
--         , requestStartLabelDetection $
--             newStartLabelDetection
--
--         , requestDescribeProjectVersions $
--             newDescribeProjectVersions
--
--         , requestGetFaceSearch $
--             newGetFaceSearch
--
--         , requestDeleteProjectVersion $
--             newDeleteProjectVersion
--
--         , requestCreateStreamProcessor $
--             newCreateStreamProcessor
--
--         , requestGetCelebrityInfo $
--             newGetCelebrityInfo
--
--         , requestGetContentModeration $
--             newGetContentModeration
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
--         , requestDetectFaces $
--             newDetectFaces
--
--         , requestStartContentModeration $
--             newStartContentModeration
--
--         , requestStartStreamProcessor $
--             newStartStreamProcessor
--
--         , requestDetectText $
--             newDetectText
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStopStreamProcessor $
--             newStopStreamProcessor
--
--         , requestStartTextDetection $
--             newStartTextDetection
--
--           ]

--     , testGroup "response"
--         [ responseStartFaceSearch $
--             newStartFaceSearchResponse
--
--         , responseGetLabelDetection $
--             newGetLabelDetectionResponse
--
--         , responseDeleteCollection $
--             newDeleteCollectionResponse
--
--         , responseDescribeStreamProcessor $
--             newDescribeStreamProcessorResponse
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
--         , responseListStreamProcessors $
--             newListStreamProcessorsResponse
--
--         , responseDeleteStreamProcessor $
--             newDeleteStreamProcessorResponse
--
--         , responseDescribeCollection $
--             newDescribeCollectionResponse
--
--         , responseDeleteFaces $
--             newDeleteFacesResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListFaces $
--             newListFacesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseSearchFacesByImage $
--             newSearchFacesByImageResponse
--
--         , responseCompareFaces $
--             newCompareFacesResponse
--
--         , responseCreateCollection $
--             newCreateCollectionResponse
--
--         , responseDetectLabels $
--             newDetectLabelsResponse
--
--         , responseGetSegmentDetection $
--             newGetSegmentDetectionResponse
--
--         , responseStartPersonTracking $
--             newStartPersonTrackingResponse
--
--         , responseGetCelebrityRecognition $
--             newGetCelebrityRecognitionResponse
--
--         , responseStartProjectVersion $
--             newStartProjectVersionResponse
--
--         , responseListCollections $
--             newListCollectionsResponse
--
--         , responseStopProjectVersion $
--             newStopProjectVersionResponse
--
--         , responseIndexFaces $
--             newIndexFacesResponse
--
--         , responseStartSegmentDetection $
--             newStartSegmentDetectionResponse
--
--         , responseDetectProtectiveEquipment $
--             newDetectProtectiveEquipmentResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseStartCelebrityRecognition $
--             newStartCelebrityRecognitionResponse
--
--         , responseGetPersonTracking $
--             newGetPersonTrackingResponse
--
--         , responseStartLabelDetection $
--             newStartLabelDetectionResponse
--
--         , responseDescribeProjectVersions $
--             newDescribeProjectVersionsResponse
--
--         , responseGetFaceSearch $
--             newGetFaceSearchResponse
--
--         , responseDeleteProjectVersion $
--             newDeleteProjectVersionResponse
--
--         , responseCreateStreamProcessor $
--             newCreateStreamProcessorResponse
--
--         , responseGetCelebrityInfo $
--             newGetCelebrityInfoResponse
--
--         , responseGetContentModeration $
--             newGetContentModerationResponse
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
--         , responseDetectFaces $
--             newDetectFacesResponse
--
--         , responseStartContentModeration $
--             newStartContentModerationResponse
--
--         , responseStartStreamProcessor $
--             newStartStreamProcessorResponse
--
--         , responseDetectText $
--             newDetectTextResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStopStreamProcessor $
--             newStopStreamProcessorResponse
--
--         , responseStartTextDetection $
--             newStartTextDetectionResponse
--
--           ]
--     ]

-- Requests

requestStartFaceSearch :: StartFaceSearch -> TestTree
requestStartFaceSearch =
  req
    "StartFaceSearch"
    "fixture/StartFaceSearch.yaml"

requestGetLabelDetection :: GetLabelDetection -> TestTree
requestGetLabelDetection =
  req
    "GetLabelDetection"
    "fixture/GetLabelDetection.yaml"

requestDeleteCollection :: DeleteCollection -> TestTree
requestDeleteCollection =
  req
    "DeleteCollection"
    "fixture/DeleteCollection.yaml"

requestDescribeStreamProcessor :: DescribeStreamProcessor -> TestTree
requestDescribeStreamProcessor =
  req
    "DescribeStreamProcessor"
    "fixture/DescribeStreamProcessor.yaml"

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

requestListStreamProcessors :: ListStreamProcessors -> TestTree
requestListStreamProcessors =
  req
    "ListStreamProcessors"
    "fixture/ListStreamProcessors.yaml"

requestDeleteStreamProcessor :: DeleteStreamProcessor -> TestTree
requestDeleteStreamProcessor =
  req
    "DeleteStreamProcessor"
    "fixture/DeleteStreamProcessor.yaml"

requestDescribeCollection :: DescribeCollection -> TestTree
requestDescribeCollection =
  req
    "DescribeCollection"
    "fixture/DescribeCollection.yaml"

requestDeleteFaces :: DeleteFaces -> TestTree
requestDeleteFaces =
  req
    "DeleteFaces"
    "fixture/DeleteFaces.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListFaces :: ListFaces -> TestTree
requestListFaces =
  req
    "ListFaces"
    "fixture/ListFaces.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

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

requestCreateCollection :: CreateCollection -> TestTree
requestCreateCollection =
  req
    "CreateCollection"
    "fixture/CreateCollection.yaml"

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

requestStopProjectVersion :: StopProjectVersion -> TestTree
requestStopProjectVersion =
  req
    "StopProjectVersion"
    "fixture/StopProjectVersion.yaml"

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

requestGetFaceSearch :: GetFaceSearch -> TestTree
requestGetFaceSearch =
  req
    "GetFaceSearch"
    "fixture/GetFaceSearch.yaml"

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

requestGetCelebrityInfo :: GetCelebrityInfo -> TestTree
requestGetCelebrityInfo =
  req
    "GetCelebrityInfo"
    "fixture/GetCelebrityInfo.yaml"

requestGetContentModeration :: GetContentModeration -> TestTree
requestGetContentModeration =
  req
    "GetContentModeration"
    "fixture/GetContentModeration.yaml"

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

requestDetectFaces :: DetectFaces -> TestTree
requestDetectFaces =
  req
    "DetectFaces"
    "fixture/DetectFaces.yaml"

requestStartContentModeration :: StartContentModeration -> TestTree
requestStartContentModeration =
  req
    "StartContentModeration"
    "fixture/StartContentModeration.yaml"

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

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStopStreamProcessor :: StopStreamProcessor -> TestTree
requestStopStreamProcessor =
  req
    "StopStreamProcessor"
    "fixture/StopStreamProcessor.yaml"

requestStartTextDetection :: StartTextDetection -> TestTree
requestStartTextDetection =
  req
    "StartTextDetection"
    "fixture/StartTextDetection.yaml"

-- Responses

responseStartFaceSearch :: StartFaceSearchResponse -> TestTree
responseStartFaceSearch =
  res
    "StartFaceSearchResponse"
    "fixture/StartFaceSearchResponse.proto"
    defaultService
    (Proxy :: Proxy StartFaceSearch)

responseGetLabelDetection :: GetLabelDetectionResponse -> TestTree
responseGetLabelDetection =
  res
    "GetLabelDetectionResponse"
    "fixture/GetLabelDetectionResponse.proto"
    defaultService
    (Proxy :: Proxy GetLabelDetection)

responseDeleteCollection :: DeleteCollectionResponse -> TestTree
responseDeleteCollection =
  res
    "DeleteCollectionResponse"
    "fixture/DeleteCollectionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCollection)

responseDescribeStreamProcessor :: DescribeStreamProcessorResponse -> TestTree
responseDescribeStreamProcessor =
  res
    "DescribeStreamProcessorResponse"
    "fixture/DescribeStreamProcessorResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStreamProcessor)

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

responseListStreamProcessors :: ListStreamProcessorsResponse -> TestTree
responseListStreamProcessors =
  res
    "ListStreamProcessorsResponse"
    "fixture/ListStreamProcessorsResponse.proto"
    defaultService
    (Proxy :: Proxy ListStreamProcessors)

responseDeleteStreamProcessor :: DeleteStreamProcessorResponse -> TestTree
responseDeleteStreamProcessor =
  res
    "DeleteStreamProcessorResponse"
    "fixture/DeleteStreamProcessorResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStreamProcessor)

responseDescribeCollection :: DescribeCollectionResponse -> TestTree
responseDescribeCollection =
  res
    "DescribeCollectionResponse"
    "fixture/DescribeCollectionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCollection)

responseDeleteFaces :: DeleteFacesResponse -> TestTree
responseDeleteFaces =
  res
    "DeleteFacesResponse"
    "fixture/DeleteFacesResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFaces)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseListFaces :: ListFacesResponse -> TestTree
responseListFaces =
  res
    "ListFacesResponse"
    "fixture/ListFacesResponse.proto"
    defaultService
    (Proxy :: Proxy ListFaces)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

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

responseCreateCollection :: CreateCollectionResponse -> TestTree
responseCreateCollection =
  res
    "CreateCollectionResponse"
    "fixture/CreateCollectionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCollection)

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

responseStartPersonTracking :: StartPersonTrackingResponse -> TestTree
responseStartPersonTracking =
  res
    "StartPersonTrackingResponse"
    "fixture/StartPersonTrackingResponse.proto"
    defaultService
    (Proxy :: Proxy StartPersonTracking)

responseGetCelebrityRecognition :: GetCelebrityRecognitionResponse -> TestTree
responseGetCelebrityRecognition =
  res
    "GetCelebrityRecognitionResponse"
    "fixture/GetCelebrityRecognitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetCelebrityRecognition)

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

responseStopProjectVersion :: StopProjectVersionResponse -> TestTree
responseStopProjectVersion =
  res
    "StopProjectVersionResponse"
    "fixture/StopProjectVersionResponse.proto"
    defaultService
    (Proxy :: Proxy StopProjectVersion)

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

responseDetectProtectiveEquipment :: DetectProtectiveEquipmentResponse -> TestTree
responseDetectProtectiveEquipment =
  res
    "DetectProtectiveEquipmentResponse"
    "fixture/DetectProtectiveEquipmentResponse.proto"
    defaultService
    (Proxy :: Proxy DetectProtectiveEquipment)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProject)

responseStartCelebrityRecognition :: StartCelebrityRecognitionResponse -> TestTree
responseStartCelebrityRecognition =
  res
    "StartCelebrityRecognitionResponse"
    "fixture/StartCelebrityRecognitionResponse.proto"
    defaultService
    (Proxy :: Proxy StartCelebrityRecognition)

responseGetPersonTracking :: GetPersonTrackingResponse -> TestTree
responseGetPersonTracking =
  res
    "GetPersonTrackingResponse"
    "fixture/GetPersonTrackingResponse.proto"
    defaultService
    (Proxy :: Proxy GetPersonTracking)

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

responseGetFaceSearch :: GetFaceSearchResponse -> TestTree
responseGetFaceSearch =
  res
    "GetFaceSearchResponse"
    "fixture/GetFaceSearchResponse.proto"
    defaultService
    (Proxy :: Proxy GetFaceSearch)

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

responseGetCelebrityInfo :: GetCelebrityInfoResponse -> TestTree
responseGetCelebrityInfo =
  res
    "GetCelebrityInfoResponse"
    "fixture/GetCelebrityInfoResponse.proto"
    defaultService
    (Proxy :: Proxy GetCelebrityInfo)

responseGetContentModeration :: GetContentModerationResponse -> TestTree
responseGetContentModeration =
  res
    "GetContentModerationResponse"
    "fixture/GetContentModerationResponse.proto"
    defaultService
    (Proxy :: Proxy GetContentModeration)

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

responseDetectFaces :: DetectFacesResponse -> TestTree
responseDetectFaces =
  res
    "DetectFacesResponse"
    "fixture/DetectFacesResponse.proto"
    defaultService
    (Proxy :: Proxy DetectFaces)

responseStartContentModeration :: StartContentModerationResponse -> TestTree
responseStartContentModeration =
  res
    "StartContentModerationResponse"
    "fixture/StartContentModerationResponse.proto"
    defaultService
    (Proxy :: Proxy StartContentModeration)

responseStartStreamProcessor :: StartStreamProcessorResponse -> TestTree
responseStartStreamProcessor =
  res
    "StartStreamProcessorResponse"
    "fixture/StartStreamProcessorResponse.proto"
    defaultService
    (Proxy :: Proxy StartStreamProcessor)

responseDetectText :: DetectTextResponse -> TestTree
responseDetectText =
  res
    "DetectTextResponse"
    "fixture/DetectTextResponse.proto"
    defaultService
    (Proxy :: Proxy DetectText)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseStopStreamProcessor :: StopStreamProcessorResponse -> TestTree
responseStopStreamProcessor =
  res
    "StopStreamProcessorResponse"
    "fixture/StopStreamProcessorResponse.proto"
    defaultService
    (Proxy :: Proxy StopStreamProcessor)

responseStartTextDetection :: StartTextDetectionResponse -> TestTree
responseStartTextDetection =
  res
    "StartTextDetectionResponse"
    "fixture/StartTextDetectionResponse.proto"
    defaultService
    (Proxy :: Proxy StartTextDetection)

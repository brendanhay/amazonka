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

import Amazonka.Rekognition
import qualified Data.Proxy as Proxy
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
--         , requestGetTextDetection $
--             newGetTextDetection
--
--         , requestStartSegmentDetection $
--             newStartSegmentDetection
--
--         , requestListCollections $
--             newListCollections
--
--         , requestStartProjectVersion $
--             newStartProjectVersion
--
--         , requestDeleteCollection $
--             newDeleteCollection
--
--         , requestCreateCollection $
--             newCreateCollection
--
--         , requestStopStreamProcessor $
--             newStopStreamProcessor
--
--         , requestDetectLabels $
--             newDetectLabels
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartContentModeration $
--             newStartContentModeration
--
--         , requestSearchFacesByImage $
--             newSearchFacesByImage
--
--         , requestListStreamProcessors $
--             newListStreamProcessors
--
--         , requestDescribeCollection $
--             newDescribeCollection
--
--         , requestDeleteProjectVersion $
--             newDeleteProjectVersion
--
--         , requestDescribeProjectVersions $
--             newDescribeProjectVersions
--
--         , requestRecognizeCelebrities $
--             newRecognizeCelebrities
--
--         , requestDetectCustomLabels $
--             newDetectCustomLabels
--
--         , requestGetFaceSearch $
--             newGetFaceSearch
--
--         , requestStartLabelDetection $
--             newStartLabelDetection
--
--         , requestSearchFaces $
--             newSearchFaces
--
--         , requestIndexFaces $
--             newIndexFaces
--
--         , requestGetLabelDetection $
--             newGetLabelDetection
--
--         , requestStopProjectVersion $
--             newStopProjectVersion
--
--         , requestDescribeStreamProcessor $
--             newDescribeStreamProcessor
--
--         , requestStartFaceSearch $
--             newStartFaceSearch
--
--         , requestStartTextDetection $
--             newStartTextDetection
--
--         , requestStartPersonTracking $
--             newStartPersonTracking
--
--         , requestGetCelebrityRecognition $
--             newGetCelebrityRecognition
--
--         , requestStartStreamProcessor $
--             newStartStreamProcessor
--
--         , requestDetectText $
--             newDetectText
--
--         , requestGetSegmentDetection $
--             newGetSegmentDetection
--
--         , requestCompareFaces $
--             newCompareFaces
--
--         , requestDetectFaces $
--             newDetectFaces
--
--         , requestGetFaceDetection $
--             newGetFaceDetection
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListFaces $
--             newListFaces
--
--         , requestCreateProjectVersion $
--             newCreateProjectVersion
--
--         , requestDescribeProjects $
--             newDescribeProjects
--
--         , requestGetContentModeration $
--             newGetContentModeration
--
--         , requestDeleteFaces $
--             newDeleteFaces
--
--         , requestGetCelebrityInfo $
--             newGetCelebrityInfo
--
--         , requestDeleteStreamProcessor $
--             newDeleteStreamProcessor
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDetectModerationLabels $
--             newDetectModerationLabels
--
--         , requestCreateStreamProcessor $
--             newCreateStreamProcessor
--
--         , requestStartFaceDetection $
--             newStartFaceDetection
--
--         , requestCreateProject $
--             newCreateProject
--
--           ]

--     , testGroup "response"
--         [ responseDetectProtectiveEquipment $
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
--         , responseGetTextDetection $
--             newGetTextDetectionResponse
--
--         , responseStartSegmentDetection $
--             newStartSegmentDetectionResponse
--
--         , responseListCollections $
--             newListCollectionsResponse
--
--         , responseStartProjectVersion $
--             newStartProjectVersionResponse
--
--         , responseDeleteCollection $
--             newDeleteCollectionResponse
--
--         , responseCreateCollection $
--             newCreateCollectionResponse
--
--         , responseStopStreamProcessor $
--             newStopStreamProcessorResponse
--
--         , responseDetectLabels $
--             newDetectLabelsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartContentModeration $
--             newStartContentModerationResponse
--
--         , responseSearchFacesByImage $
--             newSearchFacesByImageResponse
--
--         , responseListStreamProcessors $
--             newListStreamProcessorsResponse
--
--         , responseDescribeCollection $
--             newDescribeCollectionResponse
--
--         , responseDeleteProjectVersion $
--             newDeleteProjectVersionResponse
--
--         , responseDescribeProjectVersions $
--             newDescribeProjectVersionsResponse
--
--         , responseRecognizeCelebrities $
--             newRecognizeCelebritiesResponse
--
--         , responseDetectCustomLabels $
--             newDetectCustomLabelsResponse
--
--         , responseGetFaceSearch $
--             newGetFaceSearchResponse
--
--         , responseStartLabelDetection $
--             newStartLabelDetectionResponse
--
--         , responseSearchFaces $
--             newSearchFacesResponse
--
--         , responseIndexFaces $
--             newIndexFacesResponse
--
--         , responseGetLabelDetection $
--             newGetLabelDetectionResponse
--
--         , responseStopProjectVersion $
--             newStopProjectVersionResponse
--
--         , responseDescribeStreamProcessor $
--             newDescribeStreamProcessorResponse
--
--         , responseStartFaceSearch $
--             newStartFaceSearchResponse
--
--         , responseStartTextDetection $
--             newStartTextDetectionResponse
--
--         , responseStartPersonTracking $
--             newStartPersonTrackingResponse
--
--         , responseGetCelebrityRecognition $
--             newGetCelebrityRecognitionResponse
--
--         , responseStartStreamProcessor $
--             newStartStreamProcessorResponse
--
--         , responseDetectText $
--             newDetectTextResponse
--
--         , responseGetSegmentDetection $
--             newGetSegmentDetectionResponse
--
--         , responseCompareFaces $
--             newCompareFacesResponse
--
--         , responseDetectFaces $
--             newDetectFacesResponse
--
--         , responseGetFaceDetection $
--             newGetFaceDetectionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListFaces $
--             newListFacesResponse
--
--         , responseCreateProjectVersion $
--             newCreateProjectVersionResponse
--
--         , responseDescribeProjects $
--             newDescribeProjectsResponse
--
--         , responseGetContentModeration $
--             newGetContentModerationResponse
--
--         , responseDeleteFaces $
--             newDeleteFacesResponse
--
--         , responseGetCelebrityInfo $
--             newGetCelebrityInfoResponse
--
--         , responseDeleteStreamProcessor $
--             newDeleteStreamProcessorResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDetectModerationLabels $
--             newDetectModerationLabelsResponse
--
--         , responseCreateStreamProcessor $
--             newCreateStreamProcessorResponse
--
--         , responseStartFaceDetection $
--             newStartFaceDetectionResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
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

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

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

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectProtectiveEquipment)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProject)

responseStartCelebrityRecognition :: StartCelebrityRecognitionResponse -> TestTree
responseStartCelebrityRecognition =
  res
    "StartCelebrityRecognitionResponse"
    "fixture/StartCelebrityRecognitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartCelebrityRecognition)

responseGetPersonTracking :: GetPersonTrackingResponse -> TestTree
responseGetPersonTracking =
  res
    "GetPersonTrackingResponse"
    "fixture/GetPersonTrackingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPersonTracking)

responseGetTextDetection :: GetTextDetectionResponse -> TestTree
responseGetTextDetection =
  res
    "GetTextDetectionResponse"
    "fixture/GetTextDetectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTextDetection)

responseStartSegmentDetection :: StartSegmentDetectionResponse -> TestTree
responseStartSegmentDetection =
  res
    "StartSegmentDetectionResponse"
    "fixture/StartSegmentDetectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSegmentDetection)

responseListCollections :: ListCollectionsResponse -> TestTree
responseListCollections =
  res
    "ListCollectionsResponse"
    "fixture/ListCollectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCollections)

responseStartProjectVersion :: StartProjectVersionResponse -> TestTree
responseStartProjectVersion =
  res
    "StartProjectVersionResponse"
    "fixture/StartProjectVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartProjectVersion)

responseDeleteCollection :: DeleteCollectionResponse -> TestTree
responseDeleteCollection =
  res
    "DeleteCollectionResponse"
    "fixture/DeleteCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCollection)

responseCreateCollection :: CreateCollectionResponse -> TestTree
responseCreateCollection =
  res
    "CreateCollectionResponse"
    "fixture/CreateCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCollection)

responseStopStreamProcessor :: StopStreamProcessorResponse -> TestTree
responseStopStreamProcessor =
  res
    "StopStreamProcessorResponse"
    "fixture/StopStreamProcessorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopStreamProcessor)

responseDetectLabels :: DetectLabelsResponse -> TestTree
responseDetectLabels =
  res
    "DetectLabelsResponse"
    "fixture/DetectLabelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectLabels)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartContentModeration :: StartContentModerationResponse -> TestTree
responseStartContentModeration =
  res
    "StartContentModerationResponse"
    "fixture/StartContentModerationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartContentModeration)

responseSearchFacesByImage :: SearchFacesByImageResponse -> TestTree
responseSearchFacesByImage =
  res
    "SearchFacesByImageResponse"
    "fixture/SearchFacesByImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchFacesByImage)

responseListStreamProcessors :: ListStreamProcessorsResponse -> TestTree
responseListStreamProcessors =
  res
    "ListStreamProcessorsResponse"
    "fixture/ListStreamProcessorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreamProcessors)

responseDescribeCollection :: DescribeCollectionResponse -> TestTree
responseDescribeCollection =
  res
    "DescribeCollectionResponse"
    "fixture/DescribeCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCollection)

responseDeleteProjectVersion :: DeleteProjectVersionResponse -> TestTree
responseDeleteProjectVersion =
  res
    "DeleteProjectVersionResponse"
    "fixture/DeleteProjectVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProjectVersion)

responseDescribeProjectVersions :: DescribeProjectVersionsResponse -> TestTree
responseDescribeProjectVersions =
  res
    "DescribeProjectVersionsResponse"
    "fixture/DescribeProjectVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProjectVersions)

responseRecognizeCelebrities :: RecognizeCelebritiesResponse -> TestTree
responseRecognizeCelebrities =
  res
    "RecognizeCelebritiesResponse"
    "fixture/RecognizeCelebritiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RecognizeCelebrities)

responseDetectCustomLabels :: DetectCustomLabelsResponse -> TestTree
responseDetectCustomLabels =
  res
    "DetectCustomLabelsResponse"
    "fixture/DetectCustomLabelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectCustomLabels)

responseGetFaceSearch :: GetFaceSearchResponse -> TestTree
responseGetFaceSearch =
  res
    "GetFaceSearchResponse"
    "fixture/GetFaceSearchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFaceSearch)

responseStartLabelDetection :: StartLabelDetectionResponse -> TestTree
responseStartLabelDetection =
  res
    "StartLabelDetectionResponse"
    "fixture/StartLabelDetectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartLabelDetection)

responseSearchFaces :: SearchFacesResponse -> TestTree
responseSearchFaces =
  res
    "SearchFacesResponse"
    "fixture/SearchFacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchFaces)

responseIndexFaces :: IndexFacesResponse -> TestTree
responseIndexFaces =
  res
    "IndexFacesResponse"
    "fixture/IndexFacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy IndexFaces)

responseGetLabelDetection :: GetLabelDetectionResponse -> TestTree
responseGetLabelDetection =
  res
    "GetLabelDetectionResponse"
    "fixture/GetLabelDetectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLabelDetection)

responseStopProjectVersion :: StopProjectVersionResponse -> TestTree
responseStopProjectVersion =
  res
    "StopProjectVersionResponse"
    "fixture/StopProjectVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopProjectVersion)

responseDescribeStreamProcessor :: DescribeStreamProcessorResponse -> TestTree
responseDescribeStreamProcessor =
  res
    "DescribeStreamProcessorResponse"
    "fixture/DescribeStreamProcessorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStreamProcessor)

responseStartFaceSearch :: StartFaceSearchResponse -> TestTree
responseStartFaceSearch =
  res
    "StartFaceSearchResponse"
    "fixture/StartFaceSearchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartFaceSearch)

responseStartTextDetection :: StartTextDetectionResponse -> TestTree
responseStartTextDetection =
  res
    "StartTextDetectionResponse"
    "fixture/StartTextDetectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartTextDetection)

responseStartPersonTracking :: StartPersonTrackingResponse -> TestTree
responseStartPersonTracking =
  res
    "StartPersonTrackingResponse"
    "fixture/StartPersonTrackingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartPersonTracking)

responseGetCelebrityRecognition :: GetCelebrityRecognitionResponse -> TestTree
responseGetCelebrityRecognition =
  res
    "GetCelebrityRecognitionResponse"
    "fixture/GetCelebrityRecognitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCelebrityRecognition)

responseStartStreamProcessor :: StartStreamProcessorResponse -> TestTree
responseStartStreamProcessor =
  res
    "StartStreamProcessorResponse"
    "fixture/StartStreamProcessorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartStreamProcessor)

responseDetectText :: DetectTextResponse -> TestTree
responseDetectText =
  res
    "DetectTextResponse"
    "fixture/DetectTextResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectText)

responseGetSegmentDetection :: GetSegmentDetectionResponse -> TestTree
responseGetSegmentDetection =
  res
    "GetSegmentDetectionResponse"
    "fixture/GetSegmentDetectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSegmentDetection)

responseCompareFaces :: CompareFacesResponse -> TestTree
responseCompareFaces =
  res
    "CompareFacesResponse"
    "fixture/CompareFacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CompareFaces)

responseDetectFaces :: DetectFacesResponse -> TestTree
responseDetectFaces =
  res
    "DetectFacesResponse"
    "fixture/DetectFacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectFaces)

responseGetFaceDetection :: GetFaceDetectionResponse -> TestTree
responseGetFaceDetection =
  res
    "GetFaceDetectionResponse"
    "fixture/GetFaceDetectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFaceDetection)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListFaces :: ListFacesResponse -> TestTree
responseListFaces =
  res
    "ListFacesResponse"
    "fixture/ListFacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFaces)

responseCreateProjectVersion :: CreateProjectVersionResponse -> TestTree
responseCreateProjectVersion =
  res
    "CreateProjectVersionResponse"
    "fixture/CreateProjectVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProjectVersion)

responseDescribeProjects :: DescribeProjectsResponse -> TestTree
responseDescribeProjects =
  res
    "DescribeProjectsResponse"
    "fixture/DescribeProjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProjects)

responseGetContentModeration :: GetContentModerationResponse -> TestTree
responseGetContentModeration =
  res
    "GetContentModerationResponse"
    "fixture/GetContentModerationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContentModeration)

responseDeleteFaces :: DeleteFacesResponse -> TestTree
responseDeleteFaces =
  res
    "DeleteFacesResponse"
    "fixture/DeleteFacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFaces)

responseGetCelebrityInfo :: GetCelebrityInfoResponse -> TestTree
responseGetCelebrityInfo =
  res
    "GetCelebrityInfoResponse"
    "fixture/GetCelebrityInfoResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCelebrityInfo)

responseDeleteStreamProcessor :: DeleteStreamProcessorResponse -> TestTree
responseDeleteStreamProcessor =
  res
    "DeleteStreamProcessorResponse"
    "fixture/DeleteStreamProcessorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStreamProcessor)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDetectModerationLabels :: DetectModerationLabelsResponse -> TestTree
responseDetectModerationLabels =
  res
    "DetectModerationLabelsResponse"
    "fixture/DetectModerationLabelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectModerationLabels)

responseCreateStreamProcessor :: CreateStreamProcessorResponse -> TestTree
responseCreateStreamProcessor =
  res
    "CreateStreamProcessorResponse"
    "fixture/CreateStreamProcessorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStreamProcessor)

responseStartFaceDetection :: StartFaceDetectionResponse -> TestTree
responseStartFaceDetection =
  res
    "StartFaceDetectionResponse"
    "fixture/StartFaceDetectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartFaceDetection)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProject)

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Rekognition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Rekognition where

import Amazonka.Rekognition
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Rekognition.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCompareFaces $
--             newCompareFaces
--
--         , requestCopyProjectVersion $
--             newCopyProjectVersion
--
--         , requestCreateCollection $
--             newCreateCollection
--
--         , requestCreateDataset $
--             newCreateDataset
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestCreateProjectVersion $
--             newCreateProjectVersion
--
--         , requestCreateStreamProcessor $
--             newCreateStreamProcessor
--
--         , requestDeleteCollection $
--             newDeleteCollection
--
--         , requestDeleteDataset $
--             newDeleteDataset
--
--         , requestDeleteFaces $
--             newDeleteFaces
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestDeleteProjectPolicy $
--             newDeleteProjectPolicy
--
--         , requestDeleteProjectVersion $
--             newDeleteProjectVersion
--
--         , requestDeleteStreamProcessor $
--             newDeleteStreamProcessor
--
--         , requestDescribeCollection $
--             newDescribeCollection
--
--         , requestDescribeDataset $
--             newDescribeDataset
--
--         , requestDescribeProjectVersions $
--             newDescribeProjectVersions
--
--         , requestDescribeProjects $
--             newDescribeProjects
--
--         , requestDescribeStreamProcessor $
--             newDescribeStreamProcessor
--
--         , requestDetectCustomLabels $
--             newDetectCustomLabels
--
--         , requestDetectFaces $
--             newDetectFaces
--
--         , requestDetectLabels $
--             newDetectLabels
--
--         , requestDetectModerationLabels $
--             newDetectModerationLabels
--
--         , requestDetectProtectiveEquipment $
--             newDetectProtectiveEquipment
--
--         , requestDetectText $
--             newDetectText
--
--         , requestDistributeDatasetEntries $
--             newDistributeDatasetEntries
--
--         , requestGetCelebrityInfo $
--             newGetCelebrityInfo
--
--         , requestGetCelebrityRecognition $
--             newGetCelebrityRecognition
--
--         , requestGetContentModeration $
--             newGetContentModeration
--
--         , requestGetFaceDetection $
--             newGetFaceDetection
--
--         , requestGetFaceSearch $
--             newGetFaceSearch
--
--         , requestGetLabelDetection $
--             newGetLabelDetection
--
--         , requestGetPersonTracking $
--             newGetPersonTracking
--
--         , requestGetSegmentDetection $
--             newGetSegmentDetection
--
--         , requestGetTextDetection $
--             newGetTextDetection
--
--         , requestIndexFaces $
--             newIndexFaces
--
--         , requestListCollections $
--             newListCollections
--
--         , requestListDatasetEntries $
--             newListDatasetEntries
--
--         , requestListDatasetLabels $
--             newListDatasetLabels
--
--         , requestListFaces $
--             newListFaces
--
--         , requestListProjectPolicies $
--             newListProjectPolicies
--
--         , requestListStreamProcessors $
--             newListStreamProcessors
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutProjectPolicy $
--             newPutProjectPolicy
--
--         , requestRecognizeCelebrities $
--             newRecognizeCelebrities
--
--         , requestSearchFaces $
--             newSearchFaces
--
--         , requestSearchFacesByImage $
--             newSearchFacesByImage
--
--         , requestStartCelebrityRecognition $
--             newStartCelebrityRecognition
--
--         , requestStartContentModeration $
--             newStartContentModeration
--
--         , requestStartFaceDetection $
--             newStartFaceDetection
--
--         , requestStartFaceSearch $
--             newStartFaceSearch
--
--         , requestStartLabelDetection $
--             newStartLabelDetection
--
--         , requestStartPersonTracking $
--             newStartPersonTracking
--
--         , requestStartProjectVersion $
--             newStartProjectVersion
--
--         , requestStartSegmentDetection $
--             newStartSegmentDetection
--
--         , requestStartStreamProcessor $
--             newStartStreamProcessor
--
--         , requestStartTextDetection $
--             newStartTextDetection
--
--         , requestStopProjectVersion $
--             newStopProjectVersion
--
--         , requestStopStreamProcessor $
--             newStopStreamProcessor
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDatasetEntries $
--             newUpdateDatasetEntries
--
--         , requestUpdateStreamProcessor $
--             newUpdateStreamProcessor
--
--           ]

--     , testGroup "response"
--         [ responseCompareFaces $
--             newCompareFacesResponse
--
--         , responseCopyProjectVersion $
--             newCopyProjectVersionResponse
--
--         , responseCreateCollection $
--             newCreateCollectionResponse
--
--         , responseCreateDataset $
--             newCreateDatasetResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseCreateProjectVersion $
--             newCreateProjectVersionResponse
--
--         , responseCreateStreamProcessor $
--             newCreateStreamProcessorResponse
--
--         , responseDeleteCollection $
--             newDeleteCollectionResponse
--
--         , responseDeleteDataset $
--             newDeleteDatasetResponse
--
--         , responseDeleteFaces $
--             newDeleteFacesResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseDeleteProjectPolicy $
--             newDeleteProjectPolicyResponse
--
--         , responseDeleteProjectVersion $
--             newDeleteProjectVersionResponse
--
--         , responseDeleteStreamProcessor $
--             newDeleteStreamProcessorResponse
--
--         , responseDescribeCollection $
--             newDescribeCollectionResponse
--
--         , responseDescribeDataset $
--             newDescribeDatasetResponse
--
--         , responseDescribeProjectVersions $
--             newDescribeProjectVersionsResponse
--
--         , responseDescribeProjects $
--             newDescribeProjectsResponse
--
--         , responseDescribeStreamProcessor $
--             newDescribeStreamProcessorResponse
--
--         , responseDetectCustomLabels $
--             newDetectCustomLabelsResponse
--
--         , responseDetectFaces $
--             newDetectFacesResponse
--
--         , responseDetectLabels $
--             newDetectLabelsResponse
--
--         , responseDetectModerationLabels $
--             newDetectModerationLabelsResponse
--
--         , responseDetectProtectiveEquipment $
--             newDetectProtectiveEquipmentResponse
--
--         , responseDetectText $
--             newDetectTextResponse
--
--         , responseDistributeDatasetEntries $
--             newDistributeDatasetEntriesResponse
--
--         , responseGetCelebrityInfo $
--             newGetCelebrityInfoResponse
--
--         , responseGetCelebrityRecognition $
--             newGetCelebrityRecognitionResponse
--
--         , responseGetContentModeration $
--             newGetContentModerationResponse
--
--         , responseGetFaceDetection $
--             newGetFaceDetectionResponse
--
--         , responseGetFaceSearch $
--             newGetFaceSearchResponse
--
--         , responseGetLabelDetection $
--             newGetLabelDetectionResponse
--
--         , responseGetPersonTracking $
--             newGetPersonTrackingResponse
--
--         , responseGetSegmentDetection $
--             newGetSegmentDetectionResponse
--
--         , responseGetTextDetection $
--             newGetTextDetectionResponse
--
--         , responseIndexFaces $
--             newIndexFacesResponse
--
--         , responseListCollections $
--             newListCollectionsResponse
--
--         , responseListDatasetEntries $
--             newListDatasetEntriesResponse
--
--         , responseListDatasetLabels $
--             newListDatasetLabelsResponse
--
--         , responseListFaces $
--             newListFacesResponse
--
--         , responseListProjectPolicies $
--             newListProjectPoliciesResponse
--
--         , responseListStreamProcessors $
--             newListStreamProcessorsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutProjectPolicy $
--             newPutProjectPolicyResponse
--
--         , responseRecognizeCelebrities $
--             newRecognizeCelebritiesResponse
--
--         , responseSearchFaces $
--             newSearchFacesResponse
--
--         , responseSearchFacesByImage $
--             newSearchFacesByImageResponse
--
--         , responseStartCelebrityRecognition $
--             newStartCelebrityRecognitionResponse
--
--         , responseStartContentModeration $
--             newStartContentModerationResponse
--
--         , responseStartFaceDetection $
--             newStartFaceDetectionResponse
--
--         , responseStartFaceSearch $
--             newStartFaceSearchResponse
--
--         , responseStartLabelDetection $
--             newStartLabelDetectionResponse
--
--         , responseStartPersonTracking $
--             newStartPersonTrackingResponse
--
--         , responseStartProjectVersion $
--             newStartProjectVersionResponse
--
--         , responseStartSegmentDetection $
--             newStartSegmentDetectionResponse
--
--         , responseStartStreamProcessor $
--             newStartStreamProcessorResponse
--
--         , responseStartTextDetection $
--             newStartTextDetectionResponse
--
--         , responseStopProjectVersion $
--             newStopProjectVersionResponse
--
--         , responseStopStreamProcessor $
--             newStopStreamProcessorResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDatasetEntries $
--             newUpdateDatasetEntriesResponse
--
--         , responseUpdateStreamProcessor $
--             newUpdateStreamProcessorResponse
--
--           ]
--     ]

-- Requests

requestCompareFaces :: CompareFaces -> TestTree
requestCompareFaces =
  req
    "CompareFaces"
    "fixture/CompareFaces.yaml"

requestCopyProjectVersion :: CopyProjectVersion -> TestTree
requestCopyProjectVersion =
  req
    "CopyProjectVersion"
    "fixture/CopyProjectVersion.yaml"

requestCreateCollection :: CreateCollection -> TestTree
requestCreateCollection =
  req
    "CreateCollection"
    "fixture/CreateCollection.yaml"

requestCreateDataset :: CreateDataset -> TestTree
requestCreateDataset =
  req
    "CreateDataset"
    "fixture/CreateDataset.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestCreateProjectVersion :: CreateProjectVersion -> TestTree
requestCreateProjectVersion =
  req
    "CreateProjectVersion"
    "fixture/CreateProjectVersion.yaml"

requestCreateStreamProcessor :: CreateStreamProcessor -> TestTree
requestCreateStreamProcessor =
  req
    "CreateStreamProcessor"
    "fixture/CreateStreamProcessor.yaml"

requestDeleteCollection :: DeleteCollection -> TestTree
requestDeleteCollection =
  req
    "DeleteCollection"
    "fixture/DeleteCollection.yaml"

requestDeleteDataset :: DeleteDataset -> TestTree
requestDeleteDataset =
  req
    "DeleteDataset"
    "fixture/DeleteDataset.yaml"

requestDeleteFaces :: DeleteFaces -> TestTree
requestDeleteFaces =
  req
    "DeleteFaces"
    "fixture/DeleteFaces.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject =
  req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestDeleteProjectPolicy :: DeleteProjectPolicy -> TestTree
requestDeleteProjectPolicy =
  req
    "DeleteProjectPolicy"
    "fixture/DeleteProjectPolicy.yaml"

requestDeleteProjectVersion :: DeleteProjectVersion -> TestTree
requestDeleteProjectVersion =
  req
    "DeleteProjectVersion"
    "fixture/DeleteProjectVersion.yaml"

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

requestDescribeDataset :: DescribeDataset -> TestTree
requestDescribeDataset =
  req
    "DescribeDataset"
    "fixture/DescribeDataset.yaml"

requestDescribeProjectVersions :: DescribeProjectVersions -> TestTree
requestDescribeProjectVersions =
  req
    "DescribeProjectVersions"
    "fixture/DescribeProjectVersions.yaml"

requestDescribeProjects :: DescribeProjects -> TestTree
requestDescribeProjects =
  req
    "DescribeProjects"
    "fixture/DescribeProjects.yaml"

requestDescribeStreamProcessor :: DescribeStreamProcessor -> TestTree
requestDescribeStreamProcessor =
  req
    "DescribeStreamProcessor"
    "fixture/DescribeStreamProcessor.yaml"

requestDetectCustomLabels :: DetectCustomLabels -> TestTree
requestDetectCustomLabels =
  req
    "DetectCustomLabels"
    "fixture/DetectCustomLabels.yaml"

requestDetectFaces :: DetectFaces -> TestTree
requestDetectFaces =
  req
    "DetectFaces"
    "fixture/DetectFaces.yaml"

requestDetectLabels :: DetectLabels -> TestTree
requestDetectLabels =
  req
    "DetectLabels"
    "fixture/DetectLabels.yaml"

requestDetectModerationLabels :: DetectModerationLabels -> TestTree
requestDetectModerationLabels =
  req
    "DetectModerationLabels"
    "fixture/DetectModerationLabels.yaml"

requestDetectProtectiveEquipment :: DetectProtectiveEquipment -> TestTree
requestDetectProtectiveEquipment =
  req
    "DetectProtectiveEquipment"
    "fixture/DetectProtectiveEquipment.yaml"

requestDetectText :: DetectText -> TestTree
requestDetectText =
  req
    "DetectText"
    "fixture/DetectText.yaml"

requestDistributeDatasetEntries :: DistributeDatasetEntries -> TestTree
requestDistributeDatasetEntries =
  req
    "DistributeDatasetEntries"
    "fixture/DistributeDatasetEntries.yaml"

requestGetCelebrityInfo :: GetCelebrityInfo -> TestTree
requestGetCelebrityInfo =
  req
    "GetCelebrityInfo"
    "fixture/GetCelebrityInfo.yaml"

requestGetCelebrityRecognition :: GetCelebrityRecognition -> TestTree
requestGetCelebrityRecognition =
  req
    "GetCelebrityRecognition"
    "fixture/GetCelebrityRecognition.yaml"

requestGetContentModeration :: GetContentModeration -> TestTree
requestGetContentModeration =
  req
    "GetContentModeration"
    "fixture/GetContentModeration.yaml"

requestGetFaceDetection :: GetFaceDetection -> TestTree
requestGetFaceDetection =
  req
    "GetFaceDetection"
    "fixture/GetFaceDetection.yaml"

requestGetFaceSearch :: GetFaceSearch -> TestTree
requestGetFaceSearch =
  req
    "GetFaceSearch"
    "fixture/GetFaceSearch.yaml"

requestGetLabelDetection :: GetLabelDetection -> TestTree
requestGetLabelDetection =
  req
    "GetLabelDetection"
    "fixture/GetLabelDetection.yaml"

requestGetPersonTracking :: GetPersonTracking -> TestTree
requestGetPersonTracking =
  req
    "GetPersonTracking"
    "fixture/GetPersonTracking.yaml"

requestGetSegmentDetection :: GetSegmentDetection -> TestTree
requestGetSegmentDetection =
  req
    "GetSegmentDetection"
    "fixture/GetSegmentDetection.yaml"

requestGetTextDetection :: GetTextDetection -> TestTree
requestGetTextDetection =
  req
    "GetTextDetection"
    "fixture/GetTextDetection.yaml"

requestIndexFaces :: IndexFaces -> TestTree
requestIndexFaces =
  req
    "IndexFaces"
    "fixture/IndexFaces.yaml"

requestListCollections :: ListCollections -> TestTree
requestListCollections =
  req
    "ListCollections"
    "fixture/ListCollections.yaml"

requestListDatasetEntries :: ListDatasetEntries -> TestTree
requestListDatasetEntries =
  req
    "ListDatasetEntries"
    "fixture/ListDatasetEntries.yaml"

requestListDatasetLabels :: ListDatasetLabels -> TestTree
requestListDatasetLabels =
  req
    "ListDatasetLabels"
    "fixture/ListDatasetLabels.yaml"

requestListFaces :: ListFaces -> TestTree
requestListFaces =
  req
    "ListFaces"
    "fixture/ListFaces.yaml"

requestListProjectPolicies :: ListProjectPolicies -> TestTree
requestListProjectPolicies =
  req
    "ListProjectPolicies"
    "fixture/ListProjectPolicies.yaml"

requestListStreamProcessors :: ListStreamProcessors -> TestTree
requestListStreamProcessors =
  req
    "ListStreamProcessors"
    "fixture/ListStreamProcessors.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutProjectPolicy :: PutProjectPolicy -> TestTree
requestPutProjectPolicy =
  req
    "PutProjectPolicy"
    "fixture/PutProjectPolicy.yaml"

requestRecognizeCelebrities :: RecognizeCelebrities -> TestTree
requestRecognizeCelebrities =
  req
    "RecognizeCelebrities"
    "fixture/RecognizeCelebrities.yaml"

requestSearchFaces :: SearchFaces -> TestTree
requestSearchFaces =
  req
    "SearchFaces"
    "fixture/SearchFaces.yaml"

requestSearchFacesByImage :: SearchFacesByImage -> TestTree
requestSearchFacesByImage =
  req
    "SearchFacesByImage"
    "fixture/SearchFacesByImage.yaml"

requestStartCelebrityRecognition :: StartCelebrityRecognition -> TestTree
requestStartCelebrityRecognition =
  req
    "StartCelebrityRecognition"
    "fixture/StartCelebrityRecognition.yaml"

requestStartContentModeration :: StartContentModeration -> TestTree
requestStartContentModeration =
  req
    "StartContentModeration"
    "fixture/StartContentModeration.yaml"

requestStartFaceDetection :: StartFaceDetection -> TestTree
requestStartFaceDetection =
  req
    "StartFaceDetection"
    "fixture/StartFaceDetection.yaml"

requestStartFaceSearch :: StartFaceSearch -> TestTree
requestStartFaceSearch =
  req
    "StartFaceSearch"
    "fixture/StartFaceSearch.yaml"

requestStartLabelDetection :: StartLabelDetection -> TestTree
requestStartLabelDetection =
  req
    "StartLabelDetection"
    "fixture/StartLabelDetection.yaml"

requestStartPersonTracking :: StartPersonTracking -> TestTree
requestStartPersonTracking =
  req
    "StartPersonTracking"
    "fixture/StartPersonTracking.yaml"

requestStartProjectVersion :: StartProjectVersion -> TestTree
requestStartProjectVersion =
  req
    "StartProjectVersion"
    "fixture/StartProjectVersion.yaml"

requestStartSegmentDetection :: StartSegmentDetection -> TestTree
requestStartSegmentDetection =
  req
    "StartSegmentDetection"
    "fixture/StartSegmentDetection.yaml"

requestStartStreamProcessor :: StartStreamProcessor -> TestTree
requestStartStreamProcessor =
  req
    "StartStreamProcessor"
    "fixture/StartStreamProcessor.yaml"

requestStartTextDetection :: StartTextDetection -> TestTree
requestStartTextDetection =
  req
    "StartTextDetection"
    "fixture/StartTextDetection.yaml"

requestStopProjectVersion :: StopProjectVersion -> TestTree
requestStopProjectVersion =
  req
    "StopProjectVersion"
    "fixture/StopProjectVersion.yaml"

requestStopStreamProcessor :: StopStreamProcessor -> TestTree
requestStopStreamProcessor =
  req
    "StopStreamProcessor"
    "fixture/StopStreamProcessor.yaml"

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

requestUpdateDatasetEntries :: UpdateDatasetEntries -> TestTree
requestUpdateDatasetEntries =
  req
    "UpdateDatasetEntries"
    "fixture/UpdateDatasetEntries.yaml"

requestUpdateStreamProcessor :: UpdateStreamProcessor -> TestTree
requestUpdateStreamProcessor =
  req
    "UpdateStreamProcessor"
    "fixture/UpdateStreamProcessor.yaml"

-- Responses

responseCompareFaces :: CompareFacesResponse -> TestTree
responseCompareFaces =
  res
    "CompareFacesResponse"
    "fixture/CompareFacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CompareFaces)

responseCopyProjectVersion :: CopyProjectVersionResponse -> TestTree
responseCopyProjectVersion =
  res
    "CopyProjectVersionResponse"
    "fixture/CopyProjectVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyProjectVersion)

responseCreateCollection :: CreateCollectionResponse -> TestTree
responseCreateCollection =
  res
    "CreateCollectionResponse"
    "fixture/CreateCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCollection)

responseCreateDataset :: CreateDatasetResponse -> TestTree
responseCreateDataset =
  res
    "CreateDatasetResponse"
    "fixture/CreateDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataset)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProject)

responseCreateProjectVersion :: CreateProjectVersionResponse -> TestTree
responseCreateProjectVersion =
  res
    "CreateProjectVersionResponse"
    "fixture/CreateProjectVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProjectVersion)

responseCreateStreamProcessor :: CreateStreamProcessorResponse -> TestTree
responseCreateStreamProcessor =
  res
    "CreateStreamProcessorResponse"
    "fixture/CreateStreamProcessorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStreamProcessor)

responseDeleteCollection :: DeleteCollectionResponse -> TestTree
responseDeleteCollection =
  res
    "DeleteCollectionResponse"
    "fixture/DeleteCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCollection)

responseDeleteDataset :: DeleteDatasetResponse -> TestTree
responseDeleteDataset =
  res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataset)

responseDeleteFaces :: DeleteFacesResponse -> TestTree
responseDeleteFaces =
  res
    "DeleteFacesResponse"
    "fixture/DeleteFacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFaces)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProject)

responseDeleteProjectPolicy :: DeleteProjectPolicyResponse -> TestTree
responseDeleteProjectPolicy =
  res
    "DeleteProjectPolicyResponse"
    "fixture/DeleteProjectPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProjectPolicy)

responseDeleteProjectVersion :: DeleteProjectVersionResponse -> TestTree
responseDeleteProjectVersion =
  res
    "DeleteProjectVersionResponse"
    "fixture/DeleteProjectVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProjectVersion)

responseDeleteStreamProcessor :: DeleteStreamProcessorResponse -> TestTree
responseDeleteStreamProcessor =
  res
    "DeleteStreamProcessorResponse"
    "fixture/DeleteStreamProcessorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStreamProcessor)

responseDescribeCollection :: DescribeCollectionResponse -> TestTree
responseDescribeCollection =
  res
    "DescribeCollectionResponse"
    "fixture/DescribeCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCollection)

responseDescribeDataset :: DescribeDatasetResponse -> TestTree
responseDescribeDataset =
  res
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataset)

responseDescribeProjectVersions :: DescribeProjectVersionsResponse -> TestTree
responseDescribeProjectVersions =
  res
    "DescribeProjectVersionsResponse"
    "fixture/DescribeProjectVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProjectVersions)

responseDescribeProjects :: DescribeProjectsResponse -> TestTree
responseDescribeProjects =
  res
    "DescribeProjectsResponse"
    "fixture/DescribeProjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProjects)

responseDescribeStreamProcessor :: DescribeStreamProcessorResponse -> TestTree
responseDescribeStreamProcessor =
  res
    "DescribeStreamProcessorResponse"
    "fixture/DescribeStreamProcessorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStreamProcessor)

responseDetectCustomLabels :: DetectCustomLabelsResponse -> TestTree
responseDetectCustomLabels =
  res
    "DetectCustomLabelsResponse"
    "fixture/DetectCustomLabelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectCustomLabels)

responseDetectFaces :: DetectFacesResponse -> TestTree
responseDetectFaces =
  res
    "DetectFacesResponse"
    "fixture/DetectFacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectFaces)

responseDetectLabels :: DetectLabelsResponse -> TestTree
responseDetectLabels =
  res
    "DetectLabelsResponse"
    "fixture/DetectLabelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectLabels)

responseDetectModerationLabels :: DetectModerationLabelsResponse -> TestTree
responseDetectModerationLabels =
  res
    "DetectModerationLabelsResponse"
    "fixture/DetectModerationLabelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectModerationLabels)

responseDetectProtectiveEquipment :: DetectProtectiveEquipmentResponse -> TestTree
responseDetectProtectiveEquipment =
  res
    "DetectProtectiveEquipmentResponse"
    "fixture/DetectProtectiveEquipmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectProtectiveEquipment)

responseDetectText :: DetectTextResponse -> TestTree
responseDetectText =
  res
    "DetectTextResponse"
    "fixture/DetectTextResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetectText)

responseDistributeDatasetEntries :: DistributeDatasetEntriesResponse -> TestTree
responseDistributeDatasetEntries =
  res
    "DistributeDatasetEntriesResponse"
    "fixture/DistributeDatasetEntriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DistributeDatasetEntries)

responseGetCelebrityInfo :: GetCelebrityInfoResponse -> TestTree
responseGetCelebrityInfo =
  res
    "GetCelebrityInfoResponse"
    "fixture/GetCelebrityInfoResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCelebrityInfo)

responseGetCelebrityRecognition :: GetCelebrityRecognitionResponse -> TestTree
responseGetCelebrityRecognition =
  res
    "GetCelebrityRecognitionResponse"
    "fixture/GetCelebrityRecognitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCelebrityRecognition)

responseGetContentModeration :: GetContentModerationResponse -> TestTree
responseGetContentModeration =
  res
    "GetContentModerationResponse"
    "fixture/GetContentModerationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContentModeration)

responseGetFaceDetection :: GetFaceDetectionResponse -> TestTree
responseGetFaceDetection =
  res
    "GetFaceDetectionResponse"
    "fixture/GetFaceDetectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFaceDetection)

responseGetFaceSearch :: GetFaceSearchResponse -> TestTree
responseGetFaceSearch =
  res
    "GetFaceSearchResponse"
    "fixture/GetFaceSearchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFaceSearch)

responseGetLabelDetection :: GetLabelDetectionResponse -> TestTree
responseGetLabelDetection =
  res
    "GetLabelDetectionResponse"
    "fixture/GetLabelDetectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLabelDetection)

responseGetPersonTracking :: GetPersonTrackingResponse -> TestTree
responseGetPersonTracking =
  res
    "GetPersonTrackingResponse"
    "fixture/GetPersonTrackingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPersonTracking)

responseGetSegmentDetection :: GetSegmentDetectionResponse -> TestTree
responseGetSegmentDetection =
  res
    "GetSegmentDetectionResponse"
    "fixture/GetSegmentDetectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSegmentDetection)

responseGetTextDetection :: GetTextDetectionResponse -> TestTree
responseGetTextDetection =
  res
    "GetTextDetectionResponse"
    "fixture/GetTextDetectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTextDetection)

responseIndexFaces :: IndexFacesResponse -> TestTree
responseIndexFaces =
  res
    "IndexFacesResponse"
    "fixture/IndexFacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy IndexFaces)

responseListCollections :: ListCollectionsResponse -> TestTree
responseListCollections =
  res
    "ListCollectionsResponse"
    "fixture/ListCollectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCollections)

responseListDatasetEntries :: ListDatasetEntriesResponse -> TestTree
responseListDatasetEntries =
  res
    "ListDatasetEntriesResponse"
    "fixture/ListDatasetEntriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasetEntries)

responseListDatasetLabels :: ListDatasetLabelsResponse -> TestTree
responseListDatasetLabels =
  res
    "ListDatasetLabelsResponse"
    "fixture/ListDatasetLabelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasetLabels)

responseListFaces :: ListFacesResponse -> TestTree
responseListFaces =
  res
    "ListFacesResponse"
    "fixture/ListFacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFaces)

responseListProjectPolicies :: ListProjectPoliciesResponse -> TestTree
responseListProjectPolicies =
  res
    "ListProjectPoliciesResponse"
    "fixture/ListProjectPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProjectPolicies)

responseListStreamProcessors :: ListStreamProcessorsResponse -> TestTree
responseListStreamProcessors =
  res
    "ListStreamProcessorsResponse"
    "fixture/ListStreamProcessorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreamProcessors)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutProjectPolicy :: PutProjectPolicyResponse -> TestTree
responsePutProjectPolicy =
  res
    "PutProjectPolicyResponse"
    "fixture/PutProjectPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutProjectPolicy)

responseRecognizeCelebrities :: RecognizeCelebritiesResponse -> TestTree
responseRecognizeCelebrities =
  res
    "RecognizeCelebritiesResponse"
    "fixture/RecognizeCelebritiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RecognizeCelebrities)

responseSearchFaces :: SearchFacesResponse -> TestTree
responseSearchFaces =
  res
    "SearchFacesResponse"
    "fixture/SearchFacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchFaces)

responseSearchFacesByImage :: SearchFacesByImageResponse -> TestTree
responseSearchFacesByImage =
  res
    "SearchFacesByImageResponse"
    "fixture/SearchFacesByImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchFacesByImage)

responseStartCelebrityRecognition :: StartCelebrityRecognitionResponse -> TestTree
responseStartCelebrityRecognition =
  res
    "StartCelebrityRecognitionResponse"
    "fixture/StartCelebrityRecognitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartCelebrityRecognition)

responseStartContentModeration :: StartContentModerationResponse -> TestTree
responseStartContentModeration =
  res
    "StartContentModerationResponse"
    "fixture/StartContentModerationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartContentModeration)

responseStartFaceDetection :: StartFaceDetectionResponse -> TestTree
responseStartFaceDetection =
  res
    "StartFaceDetectionResponse"
    "fixture/StartFaceDetectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartFaceDetection)

responseStartFaceSearch :: StartFaceSearchResponse -> TestTree
responseStartFaceSearch =
  res
    "StartFaceSearchResponse"
    "fixture/StartFaceSearchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartFaceSearch)

responseStartLabelDetection :: StartLabelDetectionResponse -> TestTree
responseStartLabelDetection =
  res
    "StartLabelDetectionResponse"
    "fixture/StartLabelDetectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartLabelDetection)

responseStartPersonTracking :: StartPersonTrackingResponse -> TestTree
responseStartPersonTracking =
  res
    "StartPersonTrackingResponse"
    "fixture/StartPersonTrackingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartPersonTracking)

responseStartProjectVersion :: StartProjectVersionResponse -> TestTree
responseStartProjectVersion =
  res
    "StartProjectVersionResponse"
    "fixture/StartProjectVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartProjectVersion)

responseStartSegmentDetection :: StartSegmentDetectionResponse -> TestTree
responseStartSegmentDetection =
  res
    "StartSegmentDetectionResponse"
    "fixture/StartSegmentDetectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSegmentDetection)

responseStartStreamProcessor :: StartStreamProcessorResponse -> TestTree
responseStartStreamProcessor =
  res
    "StartStreamProcessorResponse"
    "fixture/StartStreamProcessorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartStreamProcessor)

responseStartTextDetection :: StartTextDetectionResponse -> TestTree
responseStartTextDetection =
  res
    "StartTextDetectionResponse"
    "fixture/StartTextDetectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartTextDetection)

responseStopProjectVersion :: StopProjectVersionResponse -> TestTree
responseStopProjectVersion =
  res
    "StopProjectVersionResponse"
    "fixture/StopProjectVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopProjectVersion)

responseStopStreamProcessor :: StopStreamProcessorResponse -> TestTree
responseStopStreamProcessor =
  res
    "StopStreamProcessorResponse"
    "fixture/StopStreamProcessorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopStreamProcessor)

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

responseUpdateDatasetEntries :: UpdateDatasetEntriesResponse -> TestTree
responseUpdateDatasetEntries =
  res
    "UpdateDatasetEntriesResponse"
    "fixture/UpdateDatasetEntriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDatasetEntries)

responseUpdateStreamProcessor :: UpdateStreamProcessorResponse -> TestTree
responseUpdateStreamProcessor =
  res
    "UpdateStreamProcessorResponse"
    "fixture/UpdateStreamProcessorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStreamProcessor)

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Evidently
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Evidently where

import Amazonka.Evidently
import qualified Data.Proxy as Proxy
import Test.Amazonka.Evidently.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchEvaluateFeature $
--             newBatchEvaluateFeature
--
--         , requestCreateExperiment $
--             newCreateExperiment
--
--         , requestCreateFeature $
--             newCreateFeature
--
--         , requestCreateLaunch $
--             newCreateLaunch
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestCreateSegment $
--             newCreateSegment
--
--         , requestDeleteExperiment $
--             newDeleteExperiment
--
--         , requestDeleteFeature $
--             newDeleteFeature
--
--         , requestDeleteLaunch $
--             newDeleteLaunch
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestDeleteSegment $
--             newDeleteSegment
--
--         , requestEvaluateFeature $
--             newEvaluateFeature
--
--         , requestGetExperiment $
--             newGetExperiment
--
--         , requestGetExperimentResults $
--             newGetExperimentResults
--
--         , requestGetFeature $
--             newGetFeature
--
--         , requestGetLaunch $
--             newGetLaunch
--
--         , requestGetProject $
--             newGetProject
--
--         , requestGetSegment $
--             newGetSegment
--
--         , requestListExperiments $
--             newListExperiments
--
--         , requestListFeatures $
--             newListFeatures
--
--         , requestListLaunches $
--             newListLaunches
--
--         , requestListProjects $
--             newListProjects
--
--         , requestListSegmentReferences $
--             newListSegmentReferences
--
--         , requestListSegments $
--             newListSegments
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutProjectEvents $
--             newPutProjectEvents
--
--         , requestStartExperiment $
--             newStartExperiment
--
--         , requestStartLaunch $
--             newStartLaunch
--
--         , requestStopExperiment $
--             newStopExperiment
--
--         , requestStopLaunch $
--             newStopLaunch
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTestSegmentPattern $
--             newTestSegmentPattern
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateExperiment $
--             newUpdateExperiment
--
--         , requestUpdateFeature $
--             newUpdateFeature
--
--         , requestUpdateLaunch $
--             newUpdateLaunch
--
--         , requestUpdateProject $
--             newUpdateProject
--
--         , requestUpdateProjectDataDelivery $
--             newUpdateProjectDataDelivery
--
--           ]

--     , testGroup "response"
--         [ responseBatchEvaluateFeature $
--             newBatchEvaluateFeatureResponse
--
--         , responseCreateExperiment $
--             newCreateExperimentResponse
--
--         , responseCreateFeature $
--             newCreateFeatureResponse
--
--         , responseCreateLaunch $
--             newCreateLaunchResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseCreateSegment $
--             newCreateSegmentResponse
--
--         , responseDeleteExperiment $
--             newDeleteExperimentResponse
--
--         , responseDeleteFeature $
--             newDeleteFeatureResponse
--
--         , responseDeleteLaunch $
--             newDeleteLaunchResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseDeleteSegment $
--             newDeleteSegmentResponse
--
--         , responseEvaluateFeature $
--             newEvaluateFeatureResponse
--
--         , responseGetExperiment $
--             newGetExperimentResponse
--
--         , responseGetExperimentResults $
--             newGetExperimentResultsResponse
--
--         , responseGetFeature $
--             newGetFeatureResponse
--
--         , responseGetLaunch $
--             newGetLaunchResponse
--
--         , responseGetProject $
--             newGetProjectResponse
--
--         , responseGetSegment $
--             newGetSegmentResponse
--
--         , responseListExperiments $
--             newListExperimentsResponse
--
--         , responseListFeatures $
--             newListFeaturesResponse
--
--         , responseListLaunches $
--             newListLaunchesResponse
--
--         , responseListProjects $
--             newListProjectsResponse
--
--         , responseListSegmentReferences $
--             newListSegmentReferencesResponse
--
--         , responseListSegments $
--             newListSegmentsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutProjectEvents $
--             newPutProjectEventsResponse
--
--         , responseStartExperiment $
--             newStartExperimentResponse
--
--         , responseStartLaunch $
--             newStartLaunchResponse
--
--         , responseStopExperiment $
--             newStopExperimentResponse
--
--         , responseStopLaunch $
--             newStopLaunchResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTestSegmentPattern $
--             newTestSegmentPatternResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateExperiment $
--             newUpdateExperimentResponse
--
--         , responseUpdateFeature $
--             newUpdateFeatureResponse
--
--         , responseUpdateLaunch $
--             newUpdateLaunchResponse
--
--         , responseUpdateProject $
--             newUpdateProjectResponse
--
--         , responseUpdateProjectDataDelivery $
--             newUpdateProjectDataDeliveryResponse
--
--           ]
--     ]

-- Requests

requestBatchEvaluateFeature :: BatchEvaluateFeature -> TestTree
requestBatchEvaluateFeature =
  req
    "BatchEvaluateFeature"
    "fixture/BatchEvaluateFeature.yaml"

requestCreateExperiment :: CreateExperiment -> TestTree
requestCreateExperiment =
  req
    "CreateExperiment"
    "fixture/CreateExperiment.yaml"

requestCreateFeature :: CreateFeature -> TestTree
requestCreateFeature =
  req
    "CreateFeature"
    "fixture/CreateFeature.yaml"

requestCreateLaunch :: CreateLaunch -> TestTree
requestCreateLaunch =
  req
    "CreateLaunch"
    "fixture/CreateLaunch.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestCreateSegment :: CreateSegment -> TestTree
requestCreateSegment =
  req
    "CreateSegment"
    "fixture/CreateSegment.yaml"

requestDeleteExperiment :: DeleteExperiment -> TestTree
requestDeleteExperiment =
  req
    "DeleteExperiment"
    "fixture/DeleteExperiment.yaml"

requestDeleteFeature :: DeleteFeature -> TestTree
requestDeleteFeature =
  req
    "DeleteFeature"
    "fixture/DeleteFeature.yaml"

requestDeleteLaunch :: DeleteLaunch -> TestTree
requestDeleteLaunch =
  req
    "DeleteLaunch"
    "fixture/DeleteLaunch.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject =
  req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestDeleteSegment :: DeleteSegment -> TestTree
requestDeleteSegment =
  req
    "DeleteSegment"
    "fixture/DeleteSegment.yaml"

requestEvaluateFeature :: EvaluateFeature -> TestTree
requestEvaluateFeature =
  req
    "EvaluateFeature"
    "fixture/EvaluateFeature.yaml"

requestGetExperiment :: GetExperiment -> TestTree
requestGetExperiment =
  req
    "GetExperiment"
    "fixture/GetExperiment.yaml"

requestGetExperimentResults :: GetExperimentResults -> TestTree
requestGetExperimentResults =
  req
    "GetExperimentResults"
    "fixture/GetExperimentResults.yaml"

requestGetFeature :: GetFeature -> TestTree
requestGetFeature =
  req
    "GetFeature"
    "fixture/GetFeature.yaml"

requestGetLaunch :: GetLaunch -> TestTree
requestGetLaunch =
  req
    "GetLaunch"
    "fixture/GetLaunch.yaml"

requestGetProject :: GetProject -> TestTree
requestGetProject =
  req
    "GetProject"
    "fixture/GetProject.yaml"

requestGetSegment :: GetSegment -> TestTree
requestGetSegment =
  req
    "GetSegment"
    "fixture/GetSegment.yaml"

requestListExperiments :: ListExperiments -> TestTree
requestListExperiments =
  req
    "ListExperiments"
    "fixture/ListExperiments.yaml"

requestListFeatures :: ListFeatures -> TestTree
requestListFeatures =
  req
    "ListFeatures"
    "fixture/ListFeatures.yaml"

requestListLaunches :: ListLaunches -> TestTree
requestListLaunches =
  req
    "ListLaunches"
    "fixture/ListLaunches.yaml"

requestListProjects :: ListProjects -> TestTree
requestListProjects =
  req
    "ListProjects"
    "fixture/ListProjects.yaml"

requestListSegmentReferences :: ListSegmentReferences -> TestTree
requestListSegmentReferences =
  req
    "ListSegmentReferences"
    "fixture/ListSegmentReferences.yaml"

requestListSegments :: ListSegments -> TestTree
requestListSegments =
  req
    "ListSegments"
    "fixture/ListSegments.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutProjectEvents :: PutProjectEvents -> TestTree
requestPutProjectEvents =
  req
    "PutProjectEvents"
    "fixture/PutProjectEvents.yaml"

requestStartExperiment :: StartExperiment -> TestTree
requestStartExperiment =
  req
    "StartExperiment"
    "fixture/StartExperiment.yaml"

requestStartLaunch :: StartLaunch -> TestTree
requestStartLaunch =
  req
    "StartLaunch"
    "fixture/StartLaunch.yaml"

requestStopExperiment :: StopExperiment -> TestTree
requestStopExperiment =
  req
    "StopExperiment"
    "fixture/StopExperiment.yaml"

requestStopLaunch :: StopLaunch -> TestTree
requestStopLaunch =
  req
    "StopLaunch"
    "fixture/StopLaunch.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTestSegmentPattern :: TestSegmentPattern -> TestTree
requestTestSegmentPattern =
  req
    "TestSegmentPattern"
    "fixture/TestSegmentPattern.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateExperiment :: UpdateExperiment -> TestTree
requestUpdateExperiment =
  req
    "UpdateExperiment"
    "fixture/UpdateExperiment.yaml"

requestUpdateFeature :: UpdateFeature -> TestTree
requestUpdateFeature =
  req
    "UpdateFeature"
    "fixture/UpdateFeature.yaml"

requestUpdateLaunch :: UpdateLaunch -> TestTree
requestUpdateLaunch =
  req
    "UpdateLaunch"
    "fixture/UpdateLaunch.yaml"

requestUpdateProject :: UpdateProject -> TestTree
requestUpdateProject =
  req
    "UpdateProject"
    "fixture/UpdateProject.yaml"

requestUpdateProjectDataDelivery :: UpdateProjectDataDelivery -> TestTree
requestUpdateProjectDataDelivery =
  req
    "UpdateProjectDataDelivery"
    "fixture/UpdateProjectDataDelivery.yaml"

-- Responses

responseBatchEvaluateFeature :: BatchEvaluateFeatureResponse -> TestTree
responseBatchEvaluateFeature =
  res
    "BatchEvaluateFeatureResponse"
    "fixture/BatchEvaluateFeatureResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchEvaluateFeature)

responseCreateExperiment :: CreateExperimentResponse -> TestTree
responseCreateExperiment =
  res
    "CreateExperimentResponse"
    "fixture/CreateExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateExperiment)

responseCreateFeature :: CreateFeatureResponse -> TestTree
responseCreateFeature =
  res
    "CreateFeatureResponse"
    "fixture/CreateFeatureResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFeature)

responseCreateLaunch :: CreateLaunchResponse -> TestTree
responseCreateLaunch =
  res
    "CreateLaunchResponse"
    "fixture/CreateLaunchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLaunch)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProject)

responseCreateSegment :: CreateSegmentResponse -> TestTree
responseCreateSegment =
  res
    "CreateSegmentResponse"
    "fixture/CreateSegmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSegment)

responseDeleteExperiment :: DeleteExperimentResponse -> TestTree
responseDeleteExperiment =
  res
    "DeleteExperimentResponse"
    "fixture/DeleteExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteExperiment)

responseDeleteFeature :: DeleteFeatureResponse -> TestTree
responseDeleteFeature =
  res
    "DeleteFeatureResponse"
    "fixture/DeleteFeatureResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFeature)

responseDeleteLaunch :: DeleteLaunchResponse -> TestTree
responseDeleteLaunch =
  res
    "DeleteLaunchResponse"
    "fixture/DeleteLaunchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLaunch)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProject)

responseDeleteSegment :: DeleteSegmentResponse -> TestTree
responseDeleteSegment =
  res
    "DeleteSegmentResponse"
    "fixture/DeleteSegmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSegment)

responseEvaluateFeature :: EvaluateFeatureResponse -> TestTree
responseEvaluateFeature =
  res
    "EvaluateFeatureResponse"
    "fixture/EvaluateFeatureResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EvaluateFeature)

responseGetExperiment :: GetExperimentResponse -> TestTree
responseGetExperiment =
  res
    "GetExperimentResponse"
    "fixture/GetExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExperiment)

responseGetExperimentResults :: GetExperimentResultsResponse -> TestTree
responseGetExperimentResults =
  res
    "GetExperimentResultsResponse"
    "fixture/GetExperimentResultsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExperimentResults)

responseGetFeature :: GetFeatureResponse -> TestTree
responseGetFeature =
  res
    "GetFeatureResponse"
    "fixture/GetFeatureResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFeature)

responseGetLaunch :: GetLaunchResponse -> TestTree
responseGetLaunch =
  res
    "GetLaunchResponse"
    "fixture/GetLaunchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLaunch)

responseGetProject :: GetProjectResponse -> TestTree
responseGetProject =
  res
    "GetProjectResponse"
    "fixture/GetProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProject)

responseGetSegment :: GetSegmentResponse -> TestTree
responseGetSegment =
  res
    "GetSegmentResponse"
    "fixture/GetSegmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSegment)

responseListExperiments :: ListExperimentsResponse -> TestTree
responseListExperiments =
  res
    "ListExperimentsResponse"
    "fixture/ListExperimentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExperiments)

responseListFeatures :: ListFeaturesResponse -> TestTree
responseListFeatures =
  res
    "ListFeaturesResponse"
    "fixture/ListFeaturesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFeatures)

responseListLaunches :: ListLaunchesResponse -> TestTree
responseListLaunches =
  res
    "ListLaunchesResponse"
    "fixture/ListLaunchesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLaunches)

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProjects)

responseListSegmentReferences :: ListSegmentReferencesResponse -> TestTree
responseListSegmentReferences =
  res
    "ListSegmentReferencesResponse"
    "fixture/ListSegmentReferencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSegmentReferences)

responseListSegments :: ListSegmentsResponse -> TestTree
responseListSegments =
  res
    "ListSegmentsResponse"
    "fixture/ListSegmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSegments)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutProjectEvents :: PutProjectEventsResponse -> TestTree
responsePutProjectEvents =
  res
    "PutProjectEventsResponse"
    "fixture/PutProjectEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutProjectEvents)

responseStartExperiment :: StartExperimentResponse -> TestTree
responseStartExperiment =
  res
    "StartExperimentResponse"
    "fixture/StartExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartExperiment)

responseStartLaunch :: StartLaunchResponse -> TestTree
responseStartLaunch =
  res
    "StartLaunchResponse"
    "fixture/StartLaunchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartLaunch)

responseStopExperiment :: StopExperimentResponse -> TestTree
responseStopExperiment =
  res
    "StopExperimentResponse"
    "fixture/StopExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopExperiment)

responseStopLaunch :: StopLaunchResponse -> TestTree
responseStopLaunch =
  res
    "StopLaunchResponse"
    "fixture/StopLaunchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopLaunch)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTestSegmentPattern :: TestSegmentPatternResponse -> TestTree
responseTestSegmentPattern =
  res
    "TestSegmentPatternResponse"
    "fixture/TestSegmentPatternResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestSegmentPattern)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateExperiment :: UpdateExperimentResponse -> TestTree
responseUpdateExperiment =
  res
    "UpdateExperimentResponse"
    "fixture/UpdateExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateExperiment)

responseUpdateFeature :: UpdateFeatureResponse -> TestTree
responseUpdateFeature =
  res
    "UpdateFeatureResponse"
    "fixture/UpdateFeatureResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFeature)

responseUpdateLaunch :: UpdateLaunchResponse -> TestTree
responseUpdateLaunch =
  res
    "UpdateLaunchResponse"
    "fixture/UpdateLaunchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLaunch)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProject)

responseUpdateProjectDataDelivery :: UpdateProjectDataDeliveryResponse -> TestTree
responseUpdateProjectDataDelivery =
  res
    "UpdateProjectDataDeliveryResponse"
    "fixture/UpdateProjectDataDeliveryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProjectDataDelivery)

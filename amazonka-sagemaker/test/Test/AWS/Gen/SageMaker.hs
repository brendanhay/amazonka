{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SageMaker
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.SageMaker where

import Data.Proxy
import Network.AWS.SageMaker
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.SageMaker.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateNotebookInstance $
--             createNotebookInstance
--
--         , requestDeleteModelPackage $
--             deleteModelPackage
--
--         , requestDescribeEndpointConfig $
--             describeEndpointConfig
--
--         , requestListLabelingJobsForWorkteam $
--             listLabelingJobsForWorkteam
--
--         , requestCreateTransformJob $
--             createTransformJob
--
--         , requestListCompilationJobs $
--             listCompilationJobs
--
--         , requestStopHyperParameterTuningJob $
--             stopHyperParameterTuningJob
--
--         , requestCreateEndpoint $
--             createEndpoint
--
--         , requestGetSearchSuggestions $
--             getSearchSuggestions
--
--         , requestDescribeCodeRepository $
--             describeCodeRepository
--
--         , requestDescribeTrainingJob $
--             describeTrainingJob
--
--         , requestDeleteEndpoint $
--             deleteEndpoint
--
--         , requestUpdateEndpoint $
--             updateEndpoint
--
--         , requestCreateCompilationJob $
--             createCompilationJob
--
--         , requestDeleteNotebookInstanceLifecycleConfig $
--             deleteNotebookInstanceLifecycleConfig
--
--         , requestUpdateNotebookInstanceLifecycleConfig $
--             updateNotebookInstanceLifecycleConfig
--
--         , requestCreateLabelingJob $
--             createLabelingJob
--
--         , requestDescribeNotebookInstance $
--             describeNotebookInstance
--
--         , requestCreateEndpointConfig $
--             createEndpointConfig
--
--         , requestStopNotebookInstance $
--             stopNotebookInstance
--
--         , requestUpdateEndpointWeightsAndCapacities $
--             updateEndpointWeightsAndCapacities
--
--         , requestDeleteTags $
--             deleteTags
--
--         , requestDescribeModelPackage $
--             describeModelPackage
--
--         , requestDeleteEndpointConfig $
--             deleteEndpointConfig
--
--         , requestCreateAlgorithm $
--             createAlgorithm
--
--         , requestStopTransformJob $
--             stopTransformJob
--
--         , requestCreateModel $
--             createModel
--
--         , requestCreateCodeRepository $
--             createCodeRepository
--
--         , requestCreateHyperParameterTuningJob $
--             createHyperParameterTuningJob
--
--         , requestListCodeRepositories $
--             listCodeRepositories
--
--         , requestDescribeCompilationJob $
--             describeCompilationJob
--
--         , requestListHyperParameterTuningJobs $
--             listHyperParameterTuningJobs
--
--         , requestListAlgorithms $
--             listAlgorithms
--
--         , requestRenderUiTemplate $
--             renderUiTemplate
--
--         , requestDeleteModel $
--             deleteModel
--
--         , requestListModels $
--             listModels
--
--         , requestDeleteAlgorithm $
--             deleteAlgorithm
--
--         , requestDescribeNotebookInstanceLifecycleConfig $
--             describeNotebookInstanceLifecycleConfig
--
--         , requestCreateModelPackage $
--             createModelPackage
--
--         , requestListNotebookInstances $
--             listNotebookInstances
--
--         , requestStopLabelingJob $
--             stopLabelingJob
--
--         , requestDeleteNotebookInstance $
--             deleteNotebookInstance
--
--         , requestUpdateNotebookInstance $
--             updateNotebookInstance
--
--         , requestListModelPackages $
--             listModelPackages
--
--         , requestDescribeLabelingJob $
--             describeLabelingJob
--
--         , requestStopTrainingJob $
--             stopTrainingJob
--
--         , requestDescribeAlgorithm $
--             describeAlgorithm
--
--         , requestDescribeModel $
--             describeModel
--
--         , requestListTransformJobs $
--             listTransformJobs
--
--         , requestDescribeHyperParameterTuningJob $
--             describeHyperParameterTuningJob
--
--         , requestListEndpoints $
--             listEndpoints
--
--         , requestCreatePresignedNotebookInstanceURL $
--             createPresignedNotebookInstanceURL
--
--         , requestListTrainingJobsForHyperParameterTuningJob $
--             listTrainingJobsForHyperParameterTuningJob
--
--         , requestUpdateWorkteam $
--             updateWorkteam
--
--         , requestDeleteWorkteam $
--             deleteWorkteam
--
--         , requestListWorkteams $
--             listWorkteams
--
--         , requestListNotebookInstanceLifecycleConfigs $
--             listNotebookInstanceLifecycleConfigs
--
--         , requestDescribeSubscribedWorkteam $
--             describeSubscribedWorkteam
--
--         , requestCreateWorkteam $
--             createWorkteam
--
--         , requestCreateNotebookInstanceLifecycleConfig $
--             createNotebookInstanceLifecycleConfig
--
--         , requestListLabelingJobs $
--             listLabelingJobs
--
--         , requestStartNotebookInstance $
--             startNotebookInstance
--
--         , requestAddTags $
--             addTags
--
--         , requestListEndpointConfigs $
--             listEndpointConfigs
--
--         , requestListTags $
--             listTags
--
--         , requestCreateTrainingJob $
--             createTrainingJob
--
--         , requestStopCompilationJob $
--             stopCompilationJob
--
--         , requestSearch $
--             search
--
--         , requestUpdateCodeRepository $
--             updateCodeRepository
--
--         , requestDeleteCodeRepository $
--             deleteCodeRepository
--
--         , requestDescribeTransformJob $
--             describeTransformJob
--
--         , requestDescribeEndpoint $
--             describeEndpoint
--
--         , requestListTrainingJobs $
--             listTrainingJobs
--
--         , requestDescribeWorkteam $
--             describeWorkteam
--
--         , requestListSubscribedWorkteams $
--             listSubscribedWorkteams
--
--           ]

--     , testGroup "response"
--         [ responseCreateNotebookInstance $
--             createNotebookInstanceResponse
--
--         , responseDeleteModelPackage $
--             deleteModelPackageResponse
--
--         , responseDescribeEndpointConfig $
--             describeEndpointConfigResponse
--
--         , responseListLabelingJobsForWorkteam $
--             listLabelingJobsForWorkteamResponse
--
--         , responseCreateTransformJob $
--             createTransformJobResponse
--
--         , responseListCompilationJobs $
--             listCompilationJobsResponse
--
--         , responseStopHyperParameterTuningJob $
--             stopHyperParameterTuningJobResponse
--
--         , responseCreateEndpoint $
--             createEndpointResponse
--
--         , responseGetSearchSuggestions $
--             getSearchSuggestionsResponse
--
--         , responseDescribeCodeRepository $
--             describeCodeRepositoryResponse
--
--         , responseDescribeTrainingJob $
--             describeTrainingJobResponse
--
--         , responseDeleteEndpoint $
--             deleteEndpointResponse
--
--         , responseUpdateEndpoint $
--             updateEndpointResponse
--
--         , responseCreateCompilationJob $
--             createCompilationJobResponse
--
--         , responseDeleteNotebookInstanceLifecycleConfig $
--             deleteNotebookInstanceLifecycleConfigResponse
--
--         , responseUpdateNotebookInstanceLifecycleConfig $
--             updateNotebookInstanceLifecycleConfigResponse
--
--         , responseCreateLabelingJob $
--             createLabelingJobResponse
--
--         , responseDescribeNotebookInstance $
--             describeNotebookInstanceResponse
--
--         , responseCreateEndpointConfig $
--             createEndpointConfigResponse
--
--         , responseStopNotebookInstance $
--             stopNotebookInstanceResponse
--
--         , responseUpdateEndpointWeightsAndCapacities $
--             updateEndpointWeightsAndCapacitiesResponse
--
--         , responseDeleteTags $
--             deleteTagsResponse
--
--         , responseDescribeModelPackage $
--             describeModelPackageResponse
--
--         , responseDeleteEndpointConfig $
--             deleteEndpointConfigResponse
--
--         , responseCreateAlgorithm $
--             createAlgorithmResponse
--
--         , responseStopTransformJob $
--             stopTransformJobResponse
--
--         , responseCreateModel $
--             createModelResponse
--
--         , responseCreateCodeRepository $
--             createCodeRepositoryResponse
--
--         , responseCreateHyperParameterTuningJob $
--             createHyperParameterTuningJobResponse
--
--         , responseListCodeRepositories $
--             listCodeRepositoriesResponse
--
--         , responseDescribeCompilationJob $
--             describeCompilationJobResponse
--
--         , responseListHyperParameterTuningJobs $
--             listHyperParameterTuningJobsResponse
--
--         , responseListAlgorithms $
--             listAlgorithmsResponse
--
--         , responseRenderUiTemplate $
--             renderUiTemplateResponse
--
--         , responseDeleteModel $
--             deleteModelResponse
--
--         , responseListModels $
--             listModelsResponse
--
--         , responseDeleteAlgorithm $
--             deleteAlgorithmResponse
--
--         , responseDescribeNotebookInstanceLifecycleConfig $
--             describeNotebookInstanceLifecycleConfigResponse
--
--         , responseCreateModelPackage $
--             createModelPackageResponse
--
--         , responseListNotebookInstances $
--             listNotebookInstancesResponse
--
--         , responseStopLabelingJob $
--             stopLabelingJobResponse
--
--         , responseDeleteNotebookInstance $
--             deleteNotebookInstanceResponse
--
--         , responseUpdateNotebookInstance $
--             updateNotebookInstanceResponse
--
--         , responseListModelPackages $
--             listModelPackagesResponse
--
--         , responseDescribeLabelingJob $
--             describeLabelingJobResponse
--
--         , responseStopTrainingJob $
--             stopTrainingJobResponse
--
--         , responseDescribeAlgorithm $
--             describeAlgorithmResponse
--
--         , responseDescribeModel $
--             describeModelResponse
--
--         , responseListTransformJobs $
--             listTransformJobsResponse
--
--         , responseDescribeHyperParameterTuningJob $
--             describeHyperParameterTuningJobResponse
--
--         , responseListEndpoints $
--             listEndpointsResponse
--
--         , responseCreatePresignedNotebookInstanceURL $
--             createPresignedNotebookInstanceURLResponse
--
--         , responseListTrainingJobsForHyperParameterTuningJob $
--             listTrainingJobsForHyperParameterTuningJobResponse
--
--         , responseUpdateWorkteam $
--             updateWorkteamResponse
--
--         , responseDeleteWorkteam $
--             deleteWorkteamResponse
--
--         , responseListWorkteams $
--             listWorkteamsResponse
--
--         , responseListNotebookInstanceLifecycleConfigs $
--             listNotebookInstanceLifecycleConfigsResponse
--
--         , responseDescribeSubscribedWorkteam $
--             describeSubscribedWorkteamResponse
--
--         , responseCreateWorkteam $
--             createWorkteamResponse
--
--         , responseCreateNotebookInstanceLifecycleConfig $
--             createNotebookInstanceLifecycleConfigResponse
--
--         , responseListLabelingJobs $
--             listLabelingJobsResponse
--
--         , responseStartNotebookInstance $
--             startNotebookInstanceResponse
--
--         , responseAddTags $
--             addTagsResponse
--
--         , responseListEndpointConfigs $
--             listEndpointConfigsResponse
--
--         , responseListTags $
--             listTagsResponse
--
--         , responseCreateTrainingJob $
--             createTrainingJobResponse
--
--         , responseStopCompilationJob $
--             stopCompilationJobResponse
--
--         , responseSearch $
--             searchResponse
--
--         , responseUpdateCodeRepository $
--             updateCodeRepositoryResponse
--
--         , responseDeleteCodeRepository $
--             deleteCodeRepositoryResponse
--
--         , responseDescribeTransformJob $
--             describeTransformJobResponse
--
--         , responseDescribeEndpoint $
--             describeEndpointResponse
--
--         , responseListTrainingJobs $
--             listTrainingJobsResponse
--
--         , responseDescribeWorkteam $
--             describeWorkteamResponse
--
--         , responseListSubscribedWorkteams $
--             listSubscribedWorkteamsResponse
--
--           ]
--     ]

-- Requests

requestCreateNotebookInstance :: CreateNotebookInstance -> TestTree
requestCreateNotebookInstance = req
    "CreateNotebookInstance"
    "fixture/CreateNotebookInstance.yaml"

requestDeleteModelPackage :: DeleteModelPackage -> TestTree
requestDeleteModelPackage = req
    "DeleteModelPackage"
    "fixture/DeleteModelPackage.yaml"

requestDescribeEndpointConfig :: DescribeEndpointConfig -> TestTree
requestDescribeEndpointConfig = req
    "DescribeEndpointConfig"
    "fixture/DescribeEndpointConfig.yaml"

requestListLabelingJobsForWorkteam :: ListLabelingJobsForWorkteam -> TestTree
requestListLabelingJobsForWorkteam = req
    "ListLabelingJobsForWorkteam"
    "fixture/ListLabelingJobsForWorkteam.yaml"

requestCreateTransformJob :: CreateTransformJob -> TestTree
requestCreateTransformJob = req
    "CreateTransformJob"
    "fixture/CreateTransformJob.yaml"

requestListCompilationJobs :: ListCompilationJobs -> TestTree
requestListCompilationJobs = req
    "ListCompilationJobs"
    "fixture/ListCompilationJobs.yaml"

requestStopHyperParameterTuningJob :: StopHyperParameterTuningJob -> TestTree
requestStopHyperParameterTuningJob = req
    "StopHyperParameterTuningJob"
    "fixture/StopHyperParameterTuningJob.yaml"

requestCreateEndpoint :: CreateEndpoint -> TestTree
requestCreateEndpoint = req
    "CreateEndpoint"
    "fixture/CreateEndpoint.yaml"

requestGetSearchSuggestions :: GetSearchSuggestions -> TestTree
requestGetSearchSuggestions = req
    "GetSearchSuggestions"
    "fixture/GetSearchSuggestions.yaml"

requestDescribeCodeRepository :: DescribeCodeRepository -> TestTree
requestDescribeCodeRepository = req
    "DescribeCodeRepository"
    "fixture/DescribeCodeRepository.yaml"

requestDescribeTrainingJob :: DescribeTrainingJob -> TestTree
requestDescribeTrainingJob = req
    "DescribeTrainingJob"
    "fixture/DescribeTrainingJob.yaml"

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint = req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

requestUpdateEndpoint :: UpdateEndpoint -> TestTree
requestUpdateEndpoint = req
    "UpdateEndpoint"
    "fixture/UpdateEndpoint.yaml"

requestCreateCompilationJob :: CreateCompilationJob -> TestTree
requestCreateCompilationJob = req
    "CreateCompilationJob"
    "fixture/CreateCompilationJob.yaml"

requestDeleteNotebookInstanceLifecycleConfig :: DeleteNotebookInstanceLifecycleConfig -> TestTree
requestDeleteNotebookInstanceLifecycleConfig = req
    "DeleteNotebookInstanceLifecycleConfig"
    "fixture/DeleteNotebookInstanceLifecycleConfig.yaml"

requestUpdateNotebookInstanceLifecycleConfig :: UpdateNotebookInstanceLifecycleConfig -> TestTree
requestUpdateNotebookInstanceLifecycleConfig = req
    "UpdateNotebookInstanceLifecycleConfig"
    "fixture/UpdateNotebookInstanceLifecycleConfig.yaml"

requestCreateLabelingJob :: CreateLabelingJob -> TestTree
requestCreateLabelingJob = req
    "CreateLabelingJob"
    "fixture/CreateLabelingJob.yaml"

requestDescribeNotebookInstance :: DescribeNotebookInstance -> TestTree
requestDescribeNotebookInstance = req
    "DescribeNotebookInstance"
    "fixture/DescribeNotebookInstance.yaml"

requestCreateEndpointConfig :: CreateEndpointConfig -> TestTree
requestCreateEndpointConfig = req
    "CreateEndpointConfig"
    "fixture/CreateEndpointConfig.yaml"

requestStopNotebookInstance :: StopNotebookInstance -> TestTree
requestStopNotebookInstance = req
    "StopNotebookInstance"
    "fixture/StopNotebookInstance.yaml"

requestUpdateEndpointWeightsAndCapacities :: UpdateEndpointWeightsAndCapacities -> TestTree
requestUpdateEndpointWeightsAndCapacities = req
    "UpdateEndpointWeightsAndCapacities"
    "fixture/UpdateEndpointWeightsAndCapacities.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestDescribeModelPackage :: DescribeModelPackage -> TestTree
requestDescribeModelPackage = req
    "DescribeModelPackage"
    "fixture/DescribeModelPackage.yaml"

requestDeleteEndpointConfig :: DeleteEndpointConfig -> TestTree
requestDeleteEndpointConfig = req
    "DeleteEndpointConfig"
    "fixture/DeleteEndpointConfig.yaml"

requestCreateAlgorithm :: CreateAlgorithm -> TestTree
requestCreateAlgorithm = req
    "CreateAlgorithm"
    "fixture/CreateAlgorithm.yaml"

requestStopTransformJob :: StopTransformJob -> TestTree
requestStopTransformJob = req
    "StopTransformJob"
    "fixture/StopTransformJob.yaml"

requestCreateModel :: CreateModel -> TestTree
requestCreateModel = req
    "CreateModel"
    "fixture/CreateModel.yaml"

requestCreateCodeRepository :: CreateCodeRepository -> TestTree
requestCreateCodeRepository = req
    "CreateCodeRepository"
    "fixture/CreateCodeRepository.yaml"

requestCreateHyperParameterTuningJob :: CreateHyperParameterTuningJob -> TestTree
requestCreateHyperParameterTuningJob = req
    "CreateHyperParameterTuningJob"
    "fixture/CreateHyperParameterTuningJob.yaml"

requestListCodeRepositories :: ListCodeRepositories -> TestTree
requestListCodeRepositories = req
    "ListCodeRepositories"
    "fixture/ListCodeRepositories.yaml"

requestDescribeCompilationJob :: DescribeCompilationJob -> TestTree
requestDescribeCompilationJob = req
    "DescribeCompilationJob"
    "fixture/DescribeCompilationJob.yaml"

requestListHyperParameterTuningJobs :: ListHyperParameterTuningJobs -> TestTree
requestListHyperParameterTuningJobs = req
    "ListHyperParameterTuningJobs"
    "fixture/ListHyperParameterTuningJobs.yaml"

requestListAlgorithms :: ListAlgorithms -> TestTree
requestListAlgorithms = req
    "ListAlgorithms"
    "fixture/ListAlgorithms.yaml"

requestRenderUiTemplate :: RenderUiTemplate -> TestTree
requestRenderUiTemplate = req
    "RenderUiTemplate"
    "fixture/RenderUiTemplate.yaml"

requestDeleteModel :: DeleteModel -> TestTree
requestDeleteModel = req
    "DeleteModel"
    "fixture/DeleteModel.yaml"

requestListModels :: ListModels -> TestTree
requestListModels = req
    "ListModels"
    "fixture/ListModels.yaml"

requestDeleteAlgorithm :: DeleteAlgorithm -> TestTree
requestDeleteAlgorithm = req
    "DeleteAlgorithm"
    "fixture/DeleteAlgorithm.yaml"

requestDescribeNotebookInstanceLifecycleConfig :: DescribeNotebookInstanceLifecycleConfig -> TestTree
requestDescribeNotebookInstanceLifecycleConfig = req
    "DescribeNotebookInstanceLifecycleConfig"
    "fixture/DescribeNotebookInstanceLifecycleConfig.yaml"

requestCreateModelPackage :: CreateModelPackage -> TestTree
requestCreateModelPackage = req
    "CreateModelPackage"
    "fixture/CreateModelPackage.yaml"

requestListNotebookInstances :: ListNotebookInstances -> TestTree
requestListNotebookInstances = req
    "ListNotebookInstances"
    "fixture/ListNotebookInstances.yaml"

requestStopLabelingJob :: StopLabelingJob -> TestTree
requestStopLabelingJob = req
    "StopLabelingJob"
    "fixture/StopLabelingJob.yaml"

requestDeleteNotebookInstance :: DeleteNotebookInstance -> TestTree
requestDeleteNotebookInstance = req
    "DeleteNotebookInstance"
    "fixture/DeleteNotebookInstance.yaml"

requestUpdateNotebookInstance :: UpdateNotebookInstance -> TestTree
requestUpdateNotebookInstance = req
    "UpdateNotebookInstance"
    "fixture/UpdateNotebookInstance.yaml"

requestListModelPackages :: ListModelPackages -> TestTree
requestListModelPackages = req
    "ListModelPackages"
    "fixture/ListModelPackages.yaml"

requestDescribeLabelingJob :: DescribeLabelingJob -> TestTree
requestDescribeLabelingJob = req
    "DescribeLabelingJob"
    "fixture/DescribeLabelingJob.yaml"

requestStopTrainingJob :: StopTrainingJob -> TestTree
requestStopTrainingJob = req
    "StopTrainingJob"
    "fixture/StopTrainingJob.yaml"

requestDescribeAlgorithm :: DescribeAlgorithm -> TestTree
requestDescribeAlgorithm = req
    "DescribeAlgorithm"
    "fixture/DescribeAlgorithm.yaml"

requestDescribeModel :: DescribeModel -> TestTree
requestDescribeModel = req
    "DescribeModel"
    "fixture/DescribeModel.yaml"

requestListTransformJobs :: ListTransformJobs -> TestTree
requestListTransformJobs = req
    "ListTransformJobs"
    "fixture/ListTransformJobs.yaml"

requestDescribeHyperParameterTuningJob :: DescribeHyperParameterTuningJob -> TestTree
requestDescribeHyperParameterTuningJob = req
    "DescribeHyperParameterTuningJob"
    "fixture/DescribeHyperParameterTuningJob.yaml"

requestListEndpoints :: ListEndpoints -> TestTree
requestListEndpoints = req
    "ListEndpoints"
    "fixture/ListEndpoints.yaml"

requestCreatePresignedNotebookInstanceURL :: CreatePresignedNotebookInstanceURL -> TestTree
requestCreatePresignedNotebookInstanceURL = req
    "CreatePresignedNotebookInstanceURL"
    "fixture/CreatePresignedNotebookInstanceURL.yaml"

requestListTrainingJobsForHyperParameterTuningJob :: ListTrainingJobsForHyperParameterTuningJob -> TestTree
requestListTrainingJobsForHyperParameterTuningJob = req
    "ListTrainingJobsForHyperParameterTuningJob"
    "fixture/ListTrainingJobsForHyperParameterTuningJob.yaml"

requestUpdateWorkteam :: UpdateWorkteam -> TestTree
requestUpdateWorkteam = req
    "UpdateWorkteam"
    "fixture/UpdateWorkteam.yaml"

requestDeleteWorkteam :: DeleteWorkteam -> TestTree
requestDeleteWorkteam = req
    "DeleteWorkteam"
    "fixture/DeleteWorkteam.yaml"

requestListWorkteams :: ListWorkteams -> TestTree
requestListWorkteams = req
    "ListWorkteams"
    "fixture/ListWorkteams.yaml"

requestListNotebookInstanceLifecycleConfigs :: ListNotebookInstanceLifecycleConfigs -> TestTree
requestListNotebookInstanceLifecycleConfigs = req
    "ListNotebookInstanceLifecycleConfigs"
    "fixture/ListNotebookInstanceLifecycleConfigs.yaml"

requestDescribeSubscribedWorkteam :: DescribeSubscribedWorkteam -> TestTree
requestDescribeSubscribedWorkteam = req
    "DescribeSubscribedWorkteam"
    "fixture/DescribeSubscribedWorkteam.yaml"

requestCreateWorkteam :: CreateWorkteam -> TestTree
requestCreateWorkteam = req
    "CreateWorkteam"
    "fixture/CreateWorkteam.yaml"

requestCreateNotebookInstanceLifecycleConfig :: CreateNotebookInstanceLifecycleConfig -> TestTree
requestCreateNotebookInstanceLifecycleConfig = req
    "CreateNotebookInstanceLifecycleConfig"
    "fixture/CreateNotebookInstanceLifecycleConfig.yaml"

requestListLabelingJobs :: ListLabelingJobs -> TestTree
requestListLabelingJobs = req
    "ListLabelingJobs"
    "fixture/ListLabelingJobs.yaml"

requestStartNotebookInstance :: StartNotebookInstance -> TestTree
requestStartNotebookInstance = req
    "StartNotebookInstance"
    "fixture/StartNotebookInstance.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags = req
    "AddTags"
    "fixture/AddTags.yaml"

requestListEndpointConfigs :: ListEndpointConfigs -> TestTree
requestListEndpointConfigs = req
    "ListEndpointConfigs"
    "fixture/ListEndpointConfigs.yaml"

requestListTags :: ListTags -> TestTree
requestListTags = req
    "ListTags"
    "fixture/ListTags.yaml"

requestCreateTrainingJob :: CreateTrainingJob -> TestTree
requestCreateTrainingJob = req
    "CreateTrainingJob"
    "fixture/CreateTrainingJob.yaml"

requestStopCompilationJob :: StopCompilationJob -> TestTree
requestStopCompilationJob = req
    "StopCompilationJob"
    "fixture/StopCompilationJob.yaml"

requestSearch :: Search -> TestTree
requestSearch = req
    "Search"
    "fixture/Search.yaml"

requestUpdateCodeRepository :: UpdateCodeRepository -> TestTree
requestUpdateCodeRepository = req
    "UpdateCodeRepository"
    "fixture/UpdateCodeRepository.yaml"

requestDeleteCodeRepository :: DeleteCodeRepository -> TestTree
requestDeleteCodeRepository = req
    "DeleteCodeRepository"
    "fixture/DeleteCodeRepository.yaml"

requestDescribeTransformJob :: DescribeTransformJob -> TestTree
requestDescribeTransformJob = req
    "DescribeTransformJob"
    "fixture/DescribeTransformJob.yaml"

requestDescribeEndpoint :: DescribeEndpoint -> TestTree
requestDescribeEndpoint = req
    "DescribeEndpoint"
    "fixture/DescribeEndpoint.yaml"

requestListTrainingJobs :: ListTrainingJobs -> TestTree
requestListTrainingJobs = req
    "ListTrainingJobs"
    "fixture/ListTrainingJobs.yaml"

requestDescribeWorkteam :: DescribeWorkteam -> TestTree
requestDescribeWorkteam = req
    "DescribeWorkteam"
    "fixture/DescribeWorkteam.yaml"

requestListSubscribedWorkteams :: ListSubscribedWorkteams -> TestTree
requestListSubscribedWorkteams = req
    "ListSubscribedWorkteams"
    "fixture/ListSubscribedWorkteams.yaml"

-- Responses

responseCreateNotebookInstance :: CreateNotebookInstanceResponse -> TestTree
responseCreateNotebookInstance = res
    "CreateNotebookInstanceResponse"
    "fixture/CreateNotebookInstanceResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateNotebookInstance)

responseDeleteModelPackage :: DeleteModelPackageResponse -> TestTree
responseDeleteModelPackage = res
    "DeleteModelPackageResponse"
    "fixture/DeleteModelPackageResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteModelPackage)

responseDescribeEndpointConfig :: DescribeEndpointConfigResponse -> TestTree
responseDescribeEndpointConfig = res
    "DescribeEndpointConfigResponse"
    "fixture/DescribeEndpointConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeEndpointConfig)

responseListLabelingJobsForWorkteam :: ListLabelingJobsForWorkteamResponse -> TestTree
responseListLabelingJobsForWorkteam = res
    "ListLabelingJobsForWorkteamResponse"
    "fixture/ListLabelingJobsForWorkteamResponse.proto"
    sageMaker
    (Proxy :: Proxy ListLabelingJobsForWorkteam)

responseCreateTransformJob :: CreateTransformJobResponse -> TestTree
responseCreateTransformJob = res
    "CreateTransformJobResponse"
    "fixture/CreateTransformJobResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateTransformJob)

responseListCompilationJobs :: ListCompilationJobsResponse -> TestTree
responseListCompilationJobs = res
    "ListCompilationJobsResponse"
    "fixture/ListCompilationJobsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListCompilationJobs)

responseStopHyperParameterTuningJob :: StopHyperParameterTuningJobResponse -> TestTree
responseStopHyperParameterTuningJob = res
    "StopHyperParameterTuningJobResponse"
    "fixture/StopHyperParameterTuningJobResponse.proto"
    sageMaker
    (Proxy :: Proxy StopHyperParameterTuningJob)

responseCreateEndpoint :: CreateEndpointResponse -> TestTree
responseCreateEndpoint = res
    "CreateEndpointResponse"
    "fixture/CreateEndpointResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateEndpoint)

responseGetSearchSuggestions :: GetSearchSuggestionsResponse -> TestTree
responseGetSearchSuggestions = res
    "GetSearchSuggestionsResponse"
    "fixture/GetSearchSuggestionsResponse.proto"
    sageMaker
    (Proxy :: Proxy GetSearchSuggestions)

responseDescribeCodeRepository :: DescribeCodeRepositoryResponse -> TestTree
responseDescribeCodeRepository = res
    "DescribeCodeRepositoryResponse"
    "fixture/DescribeCodeRepositoryResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeCodeRepository)

responseDescribeTrainingJob :: DescribeTrainingJobResponse -> TestTree
responseDescribeTrainingJob = res
    "DescribeTrainingJobResponse"
    "fixture/DescribeTrainingJobResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeTrainingJob)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint = res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteEndpoint)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint = res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateEndpoint)

responseCreateCompilationJob :: CreateCompilationJobResponse -> TestTree
responseCreateCompilationJob = res
    "CreateCompilationJobResponse"
    "fixture/CreateCompilationJobResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateCompilationJob)

responseDeleteNotebookInstanceLifecycleConfig :: DeleteNotebookInstanceLifecycleConfigResponse -> TestTree
responseDeleteNotebookInstanceLifecycleConfig = res
    "DeleteNotebookInstanceLifecycleConfigResponse"
    "fixture/DeleteNotebookInstanceLifecycleConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteNotebookInstanceLifecycleConfig)

responseUpdateNotebookInstanceLifecycleConfig :: UpdateNotebookInstanceLifecycleConfigResponse -> TestTree
responseUpdateNotebookInstanceLifecycleConfig = res
    "UpdateNotebookInstanceLifecycleConfigResponse"
    "fixture/UpdateNotebookInstanceLifecycleConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateNotebookInstanceLifecycleConfig)

responseCreateLabelingJob :: CreateLabelingJobResponse -> TestTree
responseCreateLabelingJob = res
    "CreateLabelingJobResponse"
    "fixture/CreateLabelingJobResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateLabelingJob)

responseDescribeNotebookInstance :: DescribeNotebookInstanceResponse -> TestTree
responseDescribeNotebookInstance = res
    "DescribeNotebookInstanceResponse"
    "fixture/DescribeNotebookInstanceResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeNotebookInstance)

responseCreateEndpointConfig :: CreateEndpointConfigResponse -> TestTree
responseCreateEndpointConfig = res
    "CreateEndpointConfigResponse"
    "fixture/CreateEndpointConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateEndpointConfig)

responseStopNotebookInstance :: StopNotebookInstanceResponse -> TestTree
responseStopNotebookInstance = res
    "StopNotebookInstanceResponse"
    "fixture/StopNotebookInstanceResponse.proto"
    sageMaker
    (Proxy :: Proxy StopNotebookInstance)

responseUpdateEndpointWeightsAndCapacities :: UpdateEndpointWeightsAndCapacitiesResponse -> TestTree
responseUpdateEndpointWeightsAndCapacities = res
    "UpdateEndpointWeightsAndCapacitiesResponse"
    "fixture/UpdateEndpointWeightsAndCapacitiesResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateEndpointWeightsAndCapacities)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteTags)

responseDescribeModelPackage :: DescribeModelPackageResponse -> TestTree
responseDescribeModelPackage = res
    "DescribeModelPackageResponse"
    "fixture/DescribeModelPackageResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeModelPackage)

responseDeleteEndpointConfig :: DeleteEndpointConfigResponse -> TestTree
responseDeleteEndpointConfig = res
    "DeleteEndpointConfigResponse"
    "fixture/DeleteEndpointConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteEndpointConfig)

responseCreateAlgorithm :: CreateAlgorithmResponse -> TestTree
responseCreateAlgorithm = res
    "CreateAlgorithmResponse"
    "fixture/CreateAlgorithmResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateAlgorithm)

responseStopTransformJob :: StopTransformJobResponse -> TestTree
responseStopTransformJob = res
    "StopTransformJobResponse"
    "fixture/StopTransformJobResponse.proto"
    sageMaker
    (Proxy :: Proxy StopTransformJob)

responseCreateModel :: CreateModelResponse -> TestTree
responseCreateModel = res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateModel)

responseCreateCodeRepository :: CreateCodeRepositoryResponse -> TestTree
responseCreateCodeRepository = res
    "CreateCodeRepositoryResponse"
    "fixture/CreateCodeRepositoryResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateCodeRepository)

responseCreateHyperParameterTuningJob :: CreateHyperParameterTuningJobResponse -> TestTree
responseCreateHyperParameterTuningJob = res
    "CreateHyperParameterTuningJobResponse"
    "fixture/CreateHyperParameterTuningJobResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateHyperParameterTuningJob)

responseListCodeRepositories :: ListCodeRepositoriesResponse -> TestTree
responseListCodeRepositories = res
    "ListCodeRepositoriesResponse"
    "fixture/ListCodeRepositoriesResponse.proto"
    sageMaker
    (Proxy :: Proxy ListCodeRepositories)

responseDescribeCompilationJob :: DescribeCompilationJobResponse -> TestTree
responseDescribeCompilationJob = res
    "DescribeCompilationJobResponse"
    "fixture/DescribeCompilationJobResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeCompilationJob)

responseListHyperParameterTuningJobs :: ListHyperParameterTuningJobsResponse -> TestTree
responseListHyperParameterTuningJobs = res
    "ListHyperParameterTuningJobsResponse"
    "fixture/ListHyperParameterTuningJobsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListHyperParameterTuningJobs)

responseListAlgorithms :: ListAlgorithmsResponse -> TestTree
responseListAlgorithms = res
    "ListAlgorithmsResponse"
    "fixture/ListAlgorithmsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListAlgorithms)

responseRenderUiTemplate :: RenderUiTemplateResponse -> TestTree
responseRenderUiTemplate = res
    "RenderUiTemplateResponse"
    "fixture/RenderUiTemplateResponse.proto"
    sageMaker
    (Proxy :: Proxy RenderUiTemplate)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel = res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteModel)

responseListModels :: ListModelsResponse -> TestTree
responseListModels = res
    "ListModelsResponse"
    "fixture/ListModelsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListModels)

responseDeleteAlgorithm :: DeleteAlgorithmResponse -> TestTree
responseDeleteAlgorithm = res
    "DeleteAlgorithmResponse"
    "fixture/DeleteAlgorithmResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteAlgorithm)

responseDescribeNotebookInstanceLifecycleConfig :: DescribeNotebookInstanceLifecycleConfigResponse -> TestTree
responseDescribeNotebookInstanceLifecycleConfig = res
    "DescribeNotebookInstanceLifecycleConfigResponse"
    "fixture/DescribeNotebookInstanceLifecycleConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeNotebookInstanceLifecycleConfig)

responseCreateModelPackage :: CreateModelPackageResponse -> TestTree
responseCreateModelPackage = res
    "CreateModelPackageResponse"
    "fixture/CreateModelPackageResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateModelPackage)

responseListNotebookInstances :: ListNotebookInstancesResponse -> TestTree
responseListNotebookInstances = res
    "ListNotebookInstancesResponse"
    "fixture/ListNotebookInstancesResponse.proto"
    sageMaker
    (Proxy :: Proxy ListNotebookInstances)

responseStopLabelingJob :: StopLabelingJobResponse -> TestTree
responseStopLabelingJob = res
    "StopLabelingJobResponse"
    "fixture/StopLabelingJobResponse.proto"
    sageMaker
    (Proxy :: Proxy StopLabelingJob)

responseDeleteNotebookInstance :: DeleteNotebookInstanceResponse -> TestTree
responseDeleteNotebookInstance = res
    "DeleteNotebookInstanceResponse"
    "fixture/DeleteNotebookInstanceResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteNotebookInstance)

responseUpdateNotebookInstance :: UpdateNotebookInstanceResponse -> TestTree
responseUpdateNotebookInstance = res
    "UpdateNotebookInstanceResponse"
    "fixture/UpdateNotebookInstanceResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateNotebookInstance)

responseListModelPackages :: ListModelPackagesResponse -> TestTree
responseListModelPackages = res
    "ListModelPackagesResponse"
    "fixture/ListModelPackagesResponse.proto"
    sageMaker
    (Proxy :: Proxy ListModelPackages)

responseDescribeLabelingJob :: DescribeLabelingJobResponse -> TestTree
responseDescribeLabelingJob = res
    "DescribeLabelingJobResponse"
    "fixture/DescribeLabelingJobResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeLabelingJob)

responseStopTrainingJob :: StopTrainingJobResponse -> TestTree
responseStopTrainingJob = res
    "StopTrainingJobResponse"
    "fixture/StopTrainingJobResponse.proto"
    sageMaker
    (Proxy :: Proxy StopTrainingJob)

responseDescribeAlgorithm :: DescribeAlgorithmResponse -> TestTree
responseDescribeAlgorithm = res
    "DescribeAlgorithmResponse"
    "fixture/DescribeAlgorithmResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeAlgorithm)

responseDescribeModel :: DescribeModelResponse -> TestTree
responseDescribeModel = res
    "DescribeModelResponse"
    "fixture/DescribeModelResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeModel)

responseListTransformJobs :: ListTransformJobsResponse -> TestTree
responseListTransformJobs = res
    "ListTransformJobsResponse"
    "fixture/ListTransformJobsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListTransformJobs)

responseDescribeHyperParameterTuningJob :: DescribeHyperParameterTuningJobResponse -> TestTree
responseDescribeHyperParameterTuningJob = res
    "DescribeHyperParameterTuningJobResponse"
    "fixture/DescribeHyperParameterTuningJobResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeHyperParameterTuningJob)

responseListEndpoints :: ListEndpointsResponse -> TestTree
responseListEndpoints = res
    "ListEndpointsResponse"
    "fixture/ListEndpointsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListEndpoints)

responseCreatePresignedNotebookInstanceURL :: CreatePresignedNotebookInstanceURLResponse -> TestTree
responseCreatePresignedNotebookInstanceURL = res
    "CreatePresignedNotebookInstanceURLResponse"
    "fixture/CreatePresignedNotebookInstanceURLResponse.proto"
    sageMaker
    (Proxy :: Proxy CreatePresignedNotebookInstanceURL)

responseListTrainingJobsForHyperParameterTuningJob :: ListTrainingJobsForHyperParameterTuningJobResponse -> TestTree
responseListTrainingJobsForHyperParameterTuningJob = res
    "ListTrainingJobsForHyperParameterTuningJobResponse"
    "fixture/ListTrainingJobsForHyperParameterTuningJobResponse.proto"
    sageMaker
    (Proxy :: Proxy ListTrainingJobsForHyperParameterTuningJob)

responseUpdateWorkteam :: UpdateWorkteamResponse -> TestTree
responseUpdateWorkteam = res
    "UpdateWorkteamResponse"
    "fixture/UpdateWorkteamResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateWorkteam)

responseDeleteWorkteam :: DeleteWorkteamResponse -> TestTree
responseDeleteWorkteam = res
    "DeleteWorkteamResponse"
    "fixture/DeleteWorkteamResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteWorkteam)

responseListWorkteams :: ListWorkteamsResponse -> TestTree
responseListWorkteams = res
    "ListWorkteamsResponse"
    "fixture/ListWorkteamsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListWorkteams)

responseListNotebookInstanceLifecycleConfigs :: ListNotebookInstanceLifecycleConfigsResponse -> TestTree
responseListNotebookInstanceLifecycleConfigs = res
    "ListNotebookInstanceLifecycleConfigsResponse"
    "fixture/ListNotebookInstanceLifecycleConfigsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListNotebookInstanceLifecycleConfigs)

responseDescribeSubscribedWorkteam :: DescribeSubscribedWorkteamResponse -> TestTree
responseDescribeSubscribedWorkteam = res
    "DescribeSubscribedWorkteamResponse"
    "fixture/DescribeSubscribedWorkteamResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeSubscribedWorkteam)

responseCreateWorkteam :: CreateWorkteamResponse -> TestTree
responseCreateWorkteam = res
    "CreateWorkteamResponse"
    "fixture/CreateWorkteamResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateWorkteam)

responseCreateNotebookInstanceLifecycleConfig :: CreateNotebookInstanceLifecycleConfigResponse -> TestTree
responseCreateNotebookInstanceLifecycleConfig = res
    "CreateNotebookInstanceLifecycleConfigResponse"
    "fixture/CreateNotebookInstanceLifecycleConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateNotebookInstanceLifecycleConfig)

responseListLabelingJobs :: ListLabelingJobsResponse -> TestTree
responseListLabelingJobs = res
    "ListLabelingJobsResponse"
    "fixture/ListLabelingJobsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListLabelingJobs)

responseStartNotebookInstance :: StartNotebookInstanceResponse -> TestTree
responseStartNotebookInstance = res
    "StartNotebookInstanceResponse"
    "fixture/StartNotebookInstanceResponse.proto"
    sageMaker
    (Proxy :: Proxy StartNotebookInstance)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags = res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    sageMaker
    (Proxy :: Proxy AddTags)

responseListEndpointConfigs :: ListEndpointConfigsResponse -> TestTree
responseListEndpointConfigs = res
    "ListEndpointConfigsResponse"
    "fixture/ListEndpointConfigsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListEndpointConfigs)

responseListTags :: ListTagsResponse -> TestTree
responseListTags = res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListTags)

responseCreateTrainingJob :: CreateTrainingJobResponse -> TestTree
responseCreateTrainingJob = res
    "CreateTrainingJobResponse"
    "fixture/CreateTrainingJobResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateTrainingJob)

responseStopCompilationJob :: StopCompilationJobResponse -> TestTree
responseStopCompilationJob = res
    "StopCompilationJobResponse"
    "fixture/StopCompilationJobResponse.proto"
    sageMaker
    (Proxy :: Proxy StopCompilationJob)

responseSearch :: SearchResponse -> TestTree
responseSearch = res
    "SearchResponse"
    "fixture/SearchResponse.proto"
    sageMaker
    (Proxy :: Proxy Search)

responseUpdateCodeRepository :: UpdateCodeRepositoryResponse -> TestTree
responseUpdateCodeRepository = res
    "UpdateCodeRepositoryResponse"
    "fixture/UpdateCodeRepositoryResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateCodeRepository)

responseDeleteCodeRepository :: DeleteCodeRepositoryResponse -> TestTree
responseDeleteCodeRepository = res
    "DeleteCodeRepositoryResponse"
    "fixture/DeleteCodeRepositoryResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteCodeRepository)

responseDescribeTransformJob :: DescribeTransformJobResponse -> TestTree
responseDescribeTransformJob = res
    "DescribeTransformJobResponse"
    "fixture/DescribeTransformJobResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeTransformJob)

responseDescribeEndpoint :: DescribeEndpointResponse -> TestTree
responseDescribeEndpoint = res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeEndpoint)

responseListTrainingJobs :: ListTrainingJobsResponse -> TestTree
responseListTrainingJobs = res
    "ListTrainingJobsResponse"
    "fixture/ListTrainingJobsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListTrainingJobs)

responseDescribeWorkteam :: DescribeWorkteamResponse -> TestTree
responseDescribeWorkteam = res
    "DescribeWorkteamResponse"
    "fixture/DescribeWorkteamResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeWorkteam)

responseListSubscribedWorkteams :: ListSubscribedWorkteamsResponse -> TestTree
responseListSubscribedWorkteams = res
    "ListSubscribedWorkteamsResponse"
    "fixture/ListSubscribedWorkteamsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListSubscribedWorkteams)

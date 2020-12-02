{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SageMaker
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         , requestDescribeMonitoringSchedule $
--             describeMonitoringSchedule
--
--         , requestListTrialComponents $
--             listTrialComponents
--
--         , requestDescribeEndpointConfig $
--             describeEndpointConfig
--
--         , requestDescribeApp $
--             describeApp
--
--         , requestListImageVersions $
--             listImageVersions
--
--         , requestDescribeAutoMLJob $
--             describeAutoMLJob
--
--         , requestStopProcessingJob $
--             stopProcessingJob
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
--         , requestDisassociateTrialComponent $
--             disassociateTrialComponent
--
--         , requestStopHyperParameterTuningJob $
--             stopHyperParameterTuningJob
--
--         , requestListHumanTaskUis $
--             listHumanTaskUis
--
--         , requestCreateEndpoint $
--             createEndpoint
--
--         , requestGetSearchSuggestions $
--             getSearchSuggestions
--
--         , requestDescribeTrial $
--             describeTrial
--
--         , requestCreatePresignedDomainURL $
--             createPresignedDomainURL
--
--         , requestDescribeCodeRepository $
--             describeCodeRepository
--
--         , requestDescribeImage $
--             describeImage
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
--         , requestCreateHumanTaskUi $
--             createHumanTaskUi
--
--         , requestCreateCompilationJob $
--             createCompilationJob
--
--         , requestDeleteAppImageConfig $
--             deleteAppImageConfig
--
--         , requestUpdateAppImageConfig $
--             updateAppImageConfig
--
--         , requestDeleteNotebookInstanceLifecycleConfig $
--             deleteNotebookInstanceLifecycleConfig
--
--         , requestUpdateNotebookInstanceLifecycleConfig $
--             updateNotebookInstanceLifecycleConfig
--
--         , requestDeleteWorkforce $
--             deleteWorkforce
--
--         , requestUpdateWorkforce $
--             updateWorkforce
--
--         , requestListProcessingJobs $
--             listProcessingJobs
--
--         , requestCreateLabelingJob $
--             createLabelingJob
--
--         , requestDescribeNotebookInstance $
--             describeNotebookInstance
--
--         , requestCreateMonitoringSchedule $
--             createMonitoringSchedule
--
--         , requestListAppImageConfigs $
--             listAppImageConfigs
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
--         , requestCreateAppImageConfig $
--             createAppImageConfig
--
--         , requestDeleteTags $
--             deleteTags
--
--         , requestListExperiments $
--             listExperiments
--
--         , requestListAutoMLJobs $
--             listAutoMLJobs
--
--         , requestListApps $
--             listApps
--
--         , requestCreateProcessingJob $
--             createProcessingJob
--
--         , requestDeleteMonitoringSchedule $
--             deleteMonitoringSchedule
--
--         , requestDescribeModelPackage $
--             describeModelPackage
--
--         , requestDeleteEndpointConfig $
--             deleteEndpointConfig
--
--         , requestUpdateMonitoringSchedule $
--             updateMonitoringSchedule
--
--         , requestDeleteApp $
--             deleteApp
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
--         , requestListUserProfiles $
--             listUserProfiles
--
--         , requestCreateCodeRepository $
--             createCodeRepository
--
--         , requestCreateHyperParameterTuningJob $
--             createHyperParameterTuningJob
--
--         , requestDeleteTrial $
--             deleteTrial
--
--         , requestUpdateTrial $
--             updateTrial
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
--         , requestDeleteFlowDefinition $
--             deleteFlowDefinition
--
--         , requestCreateTrial $
--             createTrial
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
--         , requestAssociateTrialComponent $
--             associateTrialComponent
--
--         , requestDescribeNotebookInstanceLifecycleConfig $
--             describeNotebookInstanceLifecycleConfig
--
--         , requestDescribeWorkforce $
--             describeWorkforce
--
--         , requestCreateModelPackage $
--             createModelPackage
--
--         , requestStopMonitoringSchedule $
--             stopMonitoringSchedule
--
--         , requestDescribeAppImageConfig $
--             describeAppImageConfig
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
--         , requestDeleteImageVersion $
--             deleteImageVersion
--
--         , requestDescribeExperiment $
--             describeExperiment
--
--         , requestDeleteTrialComponent $
--             deleteTrialComponent
--
--         , requestUpdateTrialComponent $
--             updateTrialComponent
--
--         , requestDescribeLabelingJob $
--             describeLabelingJob
--
--         , requestCreateDomain $
--             createDomain
--
--         , requestDescribeUserProfile $
--             describeUserProfile
--
--         , requestListMonitoringExecutions $
--             listMonitoringExecutions
--
--         , requestDeleteHumanTaskUi $
--             deleteHumanTaskUi
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
--         , requestDescribeFlowDefinition $
--             describeFlowDefinition
--
--         , requestCreatePresignedNotebookInstanceURL $
--             createPresignedNotebookInstanceURL
--
--         , requestListTrainingJobsForHyperParameterTuningJob $
--             listTrainingJobsForHyperParameterTuningJob
--
--         , requestDescribeDomain $
--             describeDomain
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
--         , requestCreateAutoMLJob $
--             createAutoMLJob
--
--         , requestCreateApp $
--             createApp
--
--         , requestCreateExperiment $
--             createExperiment
--
--         , requestListNotebookInstanceLifecycleConfigs $
--             listNotebookInstanceLifecycleConfigs
--
--         , requestListWorkforces $
--             listWorkforces
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
--         , requestListMonitoringSchedules $
--             listMonitoringSchedules
--
--         , requestListLabelingJobs $
--             listLabelingJobs
--
--         , requestStartNotebookInstance $
--             startNotebookInstance
--
--         , requestUpdateExperiment $
--             updateExperiment
--
--         , requestDeleteExperiment $
--             deleteExperiment
--
--         , requestAddTags $
--             addTags
--
--         , requestCreateWorkforce $
--             createWorkforce
--
--         , requestDescribeTrialComponent $
--             describeTrialComponent
--
--         , requestDescribeImageVersion $
--             describeImageVersion
--
--         , requestListEndpointConfigs $
--             listEndpointConfigs
--
--         , requestCreateFlowDefinition $
--             createFlowDefinition
--
--         , requestListTags $
--             listTags
--
--         , requestDescribeHumanTaskUi $
--             describeHumanTaskUi
--
--         , requestCreateTrainingJob $
--             createTrainingJob
--
--         , requestDeleteUserProfile $
--             deleteUserProfile
--
--         , requestUpdateUserProfile $
--             updateUserProfile
--
--         , requestCreateImage $
--             createImage
--
--         , requestListTrials $
--             listTrials
--
--         , requestStopCompilationJob $
--             stopCompilationJob
--
--         , requestListImages $
--             listImages
--
--         , requestCreateUserProfile $
--             createUserProfile
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
--         , requestListCandidatesForAutoMLJob $
--             listCandidatesForAutoMLJob
--
--         , requestDeleteImage $
--             deleteImage
--
--         , requestUpdateImage $
--             updateImage
--
--         , requestListFlowDefinitions $
--             listFlowDefinitions
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
--         , requestDeleteDomain $
--             deleteDomain
--
--         , requestUpdateDomain $
--             updateDomain
--
--         , requestListDomains $
--             listDomains
--
--         , requestCreateImageVersion $
--             createImageVersion
--
--         , requestStartMonitoringSchedule $
--             startMonitoringSchedule
--
--         , requestStopAutoMLJob $
--             stopAutoMLJob
--
--         , requestCreateTrialComponent $
--             createTrialComponent
--
--         , requestDescribeProcessingJob $
--             describeProcessingJob
--
--           ]

--     , testGroup "response"
--         [ responseCreateNotebookInstance $
--             createNotebookInstanceResponse
--
--         , responseDeleteModelPackage $
--             deleteModelPackageResponse
--
--         , responseDescribeMonitoringSchedule $
--             describeMonitoringScheduleResponse
--
--         , responseListTrialComponents $
--             listTrialComponentsResponse
--
--         , responseDescribeEndpointConfig $
--             describeEndpointConfigResponse
--
--         , responseDescribeApp $
--             describeAppResponse
--
--         , responseListImageVersions $
--             listImageVersionsResponse
--
--         , responseDescribeAutoMLJob $
--             describeAutoMLJobResponse
--
--         , responseStopProcessingJob $
--             stopProcessingJobResponse
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
--         , responseDisassociateTrialComponent $
--             disassociateTrialComponentResponse
--
--         , responseStopHyperParameterTuningJob $
--             stopHyperParameterTuningJobResponse
--
--         , responseListHumanTaskUis $
--             listHumanTaskUisResponse
--
--         , responseCreateEndpoint $
--             createEndpointResponse
--
--         , responseGetSearchSuggestions $
--             getSearchSuggestionsResponse
--
--         , responseDescribeTrial $
--             describeTrialResponse
--
--         , responseCreatePresignedDomainURL $
--             createPresignedDomainURLResponse
--
--         , responseDescribeCodeRepository $
--             describeCodeRepositoryResponse
--
--         , responseDescribeImage $
--             describeImageResponse
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
--         , responseCreateHumanTaskUi $
--             createHumanTaskUiResponse
--
--         , responseCreateCompilationJob $
--             createCompilationJobResponse
--
--         , responseDeleteAppImageConfig $
--             deleteAppImageConfigResponse
--
--         , responseUpdateAppImageConfig $
--             updateAppImageConfigResponse
--
--         , responseDeleteNotebookInstanceLifecycleConfig $
--             deleteNotebookInstanceLifecycleConfigResponse
--
--         , responseUpdateNotebookInstanceLifecycleConfig $
--             updateNotebookInstanceLifecycleConfigResponse
--
--         , responseDeleteWorkforce $
--             deleteWorkforceResponse
--
--         , responseUpdateWorkforce $
--             updateWorkforceResponse
--
--         , responseListProcessingJobs $
--             listProcessingJobsResponse
--
--         , responseCreateLabelingJob $
--             createLabelingJobResponse
--
--         , responseDescribeNotebookInstance $
--             describeNotebookInstanceResponse
--
--         , responseCreateMonitoringSchedule $
--             createMonitoringScheduleResponse
--
--         , responseListAppImageConfigs $
--             listAppImageConfigsResponse
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
--         , responseCreateAppImageConfig $
--             createAppImageConfigResponse
--
--         , responseDeleteTags $
--             deleteTagsResponse
--
--         , responseListExperiments $
--             listExperimentsResponse
--
--         , responseListAutoMLJobs $
--             listAutoMLJobsResponse
--
--         , responseListApps $
--             listAppsResponse
--
--         , responseCreateProcessingJob $
--             createProcessingJobResponse
--
--         , responseDeleteMonitoringSchedule $
--             deleteMonitoringScheduleResponse
--
--         , responseDescribeModelPackage $
--             describeModelPackageResponse
--
--         , responseDeleteEndpointConfig $
--             deleteEndpointConfigResponse
--
--         , responseUpdateMonitoringSchedule $
--             updateMonitoringScheduleResponse
--
--         , responseDeleteApp $
--             deleteAppResponse
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
--         , responseListUserProfiles $
--             listUserProfilesResponse
--
--         , responseCreateCodeRepository $
--             createCodeRepositoryResponse
--
--         , responseCreateHyperParameterTuningJob $
--             createHyperParameterTuningJobResponse
--
--         , responseDeleteTrial $
--             deleteTrialResponse
--
--         , responseUpdateTrial $
--             updateTrialResponse
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
--         , responseDeleteFlowDefinition $
--             deleteFlowDefinitionResponse
--
--         , responseCreateTrial $
--             createTrialResponse
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
--         , responseAssociateTrialComponent $
--             associateTrialComponentResponse
--
--         , responseDescribeNotebookInstanceLifecycleConfig $
--             describeNotebookInstanceLifecycleConfigResponse
--
--         , responseDescribeWorkforce $
--             describeWorkforceResponse
--
--         , responseCreateModelPackage $
--             createModelPackageResponse
--
--         , responseStopMonitoringSchedule $
--             stopMonitoringScheduleResponse
--
--         , responseDescribeAppImageConfig $
--             describeAppImageConfigResponse
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
--         , responseDeleteImageVersion $
--             deleteImageVersionResponse
--
--         , responseDescribeExperiment $
--             describeExperimentResponse
--
--         , responseDeleteTrialComponent $
--             deleteTrialComponentResponse
--
--         , responseUpdateTrialComponent $
--             updateTrialComponentResponse
--
--         , responseDescribeLabelingJob $
--             describeLabelingJobResponse
--
--         , responseCreateDomain $
--             createDomainResponse
--
--         , responseDescribeUserProfile $
--             describeUserProfileResponse
--
--         , responseListMonitoringExecutions $
--             listMonitoringExecutionsResponse
--
--         , responseDeleteHumanTaskUi $
--             deleteHumanTaskUiResponse
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
--         , responseDescribeFlowDefinition $
--             describeFlowDefinitionResponse
--
--         , responseCreatePresignedNotebookInstanceURL $
--             createPresignedNotebookInstanceURLResponse
--
--         , responseListTrainingJobsForHyperParameterTuningJob $
--             listTrainingJobsForHyperParameterTuningJobResponse
--
--         , responseDescribeDomain $
--             describeDomainResponse
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
--         , responseCreateAutoMLJob $
--             createAutoMLJobResponse
--
--         , responseCreateApp $
--             createAppResponse
--
--         , responseCreateExperiment $
--             createExperimentResponse
--
--         , responseListNotebookInstanceLifecycleConfigs $
--             listNotebookInstanceLifecycleConfigsResponse
--
--         , responseListWorkforces $
--             listWorkforcesResponse
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
--         , responseListMonitoringSchedules $
--             listMonitoringSchedulesResponse
--
--         , responseListLabelingJobs $
--             listLabelingJobsResponse
--
--         , responseStartNotebookInstance $
--             startNotebookInstanceResponse
--
--         , responseUpdateExperiment $
--             updateExperimentResponse
--
--         , responseDeleteExperiment $
--             deleteExperimentResponse
--
--         , responseAddTags $
--             addTagsResponse
--
--         , responseCreateWorkforce $
--             createWorkforceResponse
--
--         , responseDescribeTrialComponent $
--             describeTrialComponentResponse
--
--         , responseDescribeImageVersion $
--             describeImageVersionResponse
--
--         , responseListEndpointConfigs $
--             listEndpointConfigsResponse
--
--         , responseCreateFlowDefinition $
--             createFlowDefinitionResponse
--
--         , responseListTags $
--             listTagsResponse
--
--         , responseDescribeHumanTaskUi $
--             describeHumanTaskUiResponse
--
--         , responseCreateTrainingJob $
--             createTrainingJobResponse
--
--         , responseDeleteUserProfile $
--             deleteUserProfileResponse
--
--         , responseUpdateUserProfile $
--             updateUserProfileResponse
--
--         , responseCreateImage $
--             createImageResponse
--
--         , responseListTrials $
--             listTrialsResponse
--
--         , responseStopCompilationJob $
--             stopCompilationJobResponse
--
--         , responseListImages $
--             listImagesResponse
--
--         , responseCreateUserProfile $
--             createUserProfileResponse
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
--         , responseListCandidatesForAutoMLJob $
--             listCandidatesForAutoMLJobResponse
--
--         , responseDeleteImage $
--             deleteImageResponse
--
--         , responseUpdateImage $
--             updateImageResponse
--
--         , responseListFlowDefinitions $
--             listFlowDefinitionsResponse
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
--         , responseDeleteDomain $
--             deleteDomainResponse
--
--         , responseUpdateDomain $
--             updateDomainResponse
--
--         , responseListDomains $
--             listDomainsResponse
--
--         , responseCreateImageVersion $
--             createImageVersionResponse
--
--         , responseStartMonitoringSchedule $
--             startMonitoringScheduleResponse
--
--         , responseStopAutoMLJob $
--             stopAutoMLJobResponse
--
--         , responseCreateTrialComponent $
--             createTrialComponentResponse
--
--         , responseDescribeProcessingJob $
--             describeProcessingJobResponse
--
--           ]
--     ]

-- Requests

requestCreateNotebookInstance :: CreateNotebookInstance -> TestTree
requestCreateNotebookInstance =
  req
    "CreateNotebookInstance"
    "fixture/CreateNotebookInstance.yaml"

requestDeleteModelPackage :: DeleteModelPackage -> TestTree
requestDeleteModelPackage =
  req
    "DeleteModelPackage"
    "fixture/DeleteModelPackage.yaml"

requestDescribeMonitoringSchedule :: DescribeMonitoringSchedule -> TestTree
requestDescribeMonitoringSchedule =
  req
    "DescribeMonitoringSchedule"
    "fixture/DescribeMonitoringSchedule.yaml"

requestListTrialComponents :: ListTrialComponents -> TestTree
requestListTrialComponents =
  req
    "ListTrialComponents"
    "fixture/ListTrialComponents.yaml"

requestDescribeEndpointConfig :: DescribeEndpointConfig -> TestTree
requestDescribeEndpointConfig =
  req
    "DescribeEndpointConfig"
    "fixture/DescribeEndpointConfig.yaml"

requestDescribeApp :: DescribeApp -> TestTree
requestDescribeApp =
  req
    "DescribeApp"
    "fixture/DescribeApp.yaml"

requestListImageVersions :: ListImageVersions -> TestTree
requestListImageVersions =
  req
    "ListImageVersions"
    "fixture/ListImageVersions.yaml"

requestDescribeAutoMLJob :: DescribeAutoMLJob -> TestTree
requestDescribeAutoMLJob =
  req
    "DescribeAutoMLJob"
    "fixture/DescribeAutoMLJob.yaml"

requestStopProcessingJob :: StopProcessingJob -> TestTree
requestStopProcessingJob =
  req
    "StopProcessingJob"
    "fixture/StopProcessingJob.yaml"

requestListLabelingJobsForWorkteam :: ListLabelingJobsForWorkteam -> TestTree
requestListLabelingJobsForWorkteam =
  req
    "ListLabelingJobsForWorkteam"
    "fixture/ListLabelingJobsForWorkteam.yaml"

requestCreateTransformJob :: CreateTransformJob -> TestTree
requestCreateTransformJob =
  req
    "CreateTransformJob"
    "fixture/CreateTransformJob.yaml"

requestListCompilationJobs :: ListCompilationJobs -> TestTree
requestListCompilationJobs =
  req
    "ListCompilationJobs"
    "fixture/ListCompilationJobs.yaml"

requestDisassociateTrialComponent :: DisassociateTrialComponent -> TestTree
requestDisassociateTrialComponent =
  req
    "DisassociateTrialComponent"
    "fixture/DisassociateTrialComponent.yaml"

requestStopHyperParameterTuningJob :: StopHyperParameterTuningJob -> TestTree
requestStopHyperParameterTuningJob =
  req
    "StopHyperParameterTuningJob"
    "fixture/StopHyperParameterTuningJob.yaml"

requestListHumanTaskUis :: ListHumanTaskUis -> TestTree
requestListHumanTaskUis =
  req
    "ListHumanTaskUis"
    "fixture/ListHumanTaskUis.yaml"

requestCreateEndpoint :: CreateEndpoint -> TestTree
requestCreateEndpoint =
  req
    "CreateEndpoint"
    "fixture/CreateEndpoint.yaml"

requestGetSearchSuggestions :: GetSearchSuggestions -> TestTree
requestGetSearchSuggestions =
  req
    "GetSearchSuggestions"
    "fixture/GetSearchSuggestions.yaml"

requestDescribeTrial :: DescribeTrial -> TestTree
requestDescribeTrial =
  req
    "DescribeTrial"
    "fixture/DescribeTrial.yaml"

requestCreatePresignedDomainURL :: CreatePresignedDomainURL -> TestTree
requestCreatePresignedDomainURL =
  req
    "CreatePresignedDomainURL"
    "fixture/CreatePresignedDomainURL.yaml"

requestDescribeCodeRepository :: DescribeCodeRepository -> TestTree
requestDescribeCodeRepository =
  req
    "DescribeCodeRepository"
    "fixture/DescribeCodeRepository.yaml"

requestDescribeImage :: DescribeImage -> TestTree
requestDescribeImage =
  req
    "DescribeImage"
    "fixture/DescribeImage.yaml"

requestDescribeTrainingJob :: DescribeTrainingJob -> TestTree
requestDescribeTrainingJob =
  req
    "DescribeTrainingJob"
    "fixture/DescribeTrainingJob.yaml"

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint =
  req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

requestUpdateEndpoint :: UpdateEndpoint -> TestTree
requestUpdateEndpoint =
  req
    "UpdateEndpoint"
    "fixture/UpdateEndpoint.yaml"

requestCreateHumanTaskUi :: CreateHumanTaskUi -> TestTree
requestCreateHumanTaskUi =
  req
    "CreateHumanTaskUi"
    "fixture/CreateHumanTaskUi.yaml"

requestCreateCompilationJob :: CreateCompilationJob -> TestTree
requestCreateCompilationJob =
  req
    "CreateCompilationJob"
    "fixture/CreateCompilationJob.yaml"

requestDeleteAppImageConfig :: DeleteAppImageConfig -> TestTree
requestDeleteAppImageConfig =
  req
    "DeleteAppImageConfig"
    "fixture/DeleteAppImageConfig.yaml"

requestUpdateAppImageConfig :: UpdateAppImageConfig -> TestTree
requestUpdateAppImageConfig =
  req
    "UpdateAppImageConfig"
    "fixture/UpdateAppImageConfig.yaml"

requestDeleteNotebookInstanceLifecycleConfig :: DeleteNotebookInstanceLifecycleConfig -> TestTree
requestDeleteNotebookInstanceLifecycleConfig =
  req
    "DeleteNotebookInstanceLifecycleConfig"
    "fixture/DeleteNotebookInstanceLifecycleConfig.yaml"

requestUpdateNotebookInstanceLifecycleConfig :: UpdateNotebookInstanceLifecycleConfig -> TestTree
requestUpdateNotebookInstanceLifecycleConfig =
  req
    "UpdateNotebookInstanceLifecycleConfig"
    "fixture/UpdateNotebookInstanceLifecycleConfig.yaml"

requestDeleteWorkforce :: DeleteWorkforce -> TestTree
requestDeleteWorkforce =
  req
    "DeleteWorkforce"
    "fixture/DeleteWorkforce.yaml"

requestUpdateWorkforce :: UpdateWorkforce -> TestTree
requestUpdateWorkforce =
  req
    "UpdateWorkforce"
    "fixture/UpdateWorkforce.yaml"

requestListProcessingJobs :: ListProcessingJobs -> TestTree
requestListProcessingJobs =
  req
    "ListProcessingJobs"
    "fixture/ListProcessingJobs.yaml"

requestCreateLabelingJob :: CreateLabelingJob -> TestTree
requestCreateLabelingJob =
  req
    "CreateLabelingJob"
    "fixture/CreateLabelingJob.yaml"

requestDescribeNotebookInstance :: DescribeNotebookInstance -> TestTree
requestDescribeNotebookInstance =
  req
    "DescribeNotebookInstance"
    "fixture/DescribeNotebookInstance.yaml"

requestCreateMonitoringSchedule :: CreateMonitoringSchedule -> TestTree
requestCreateMonitoringSchedule =
  req
    "CreateMonitoringSchedule"
    "fixture/CreateMonitoringSchedule.yaml"

requestListAppImageConfigs :: ListAppImageConfigs -> TestTree
requestListAppImageConfigs =
  req
    "ListAppImageConfigs"
    "fixture/ListAppImageConfigs.yaml"

requestCreateEndpointConfig :: CreateEndpointConfig -> TestTree
requestCreateEndpointConfig =
  req
    "CreateEndpointConfig"
    "fixture/CreateEndpointConfig.yaml"

requestStopNotebookInstance :: StopNotebookInstance -> TestTree
requestStopNotebookInstance =
  req
    "StopNotebookInstance"
    "fixture/StopNotebookInstance.yaml"

requestUpdateEndpointWeightsAndCapacities :: UpdateEndpointWeightsAndCapacities -> TestTree
requestUpdateEndpointWeightsAndCapacities =
  req
    "UpdateEndpointWeightsAndCapacities"
    "fixture/UpdateEndpointWeightsAndCapacities.yaml"

requestCreateAppImageConfig :: CreateAppImageConfig -> TestTree
requestCreateAppImageConfig =
  req
    "CreateAppImageConfig"
    "fixture/CreateAppImageConfig.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestListExperiments :: ListExperiments -> TestTree
requestListExperiments =
  req
    "ListExperiments"
    "fixture/ListExperiments.yaml"

requestListAutoMLJobs :: ListAutoMLJobs -> TestTree
requestListAutoMLJobs =
  req
    "ListAutoMLJobs"
    "fixture/ListAutoMLJobs.yaml"

requestListApps :: ListApps -> TestTree
requestListApps =
  req
    "ListApps"
    "fixture/ListApps.yaml"

requestCreateProcessingJob :: CreateProcessingJob -> TestTree
requestCreateProcessingJob =
  req
    "CreateProcessingJob"
    "fixture/CreateProcessingJob.yaml"

requestDeleteMonitoringSchedule :: DeleteMonitoringSchedule -> TestTree
requestDeleteMonitoringSchedule =
  req
    "DeleteMonitoringSchedule"
    "fixture/DeleteMonitoringSchedule.yaml"

requestDescribeModelPackage :: DescribeModelPackage -> TestTree
requestDescribeModelPackage =
  req
    "DescribeModelPackage"
    "fixture/DescribeModelPackage.yaml"

requestDeleteEndpointConfig :: DeleteEndpointConfig -> TestTree
requestDeleteEndpointConfig =
  req
    "DeleteEndpointConfig"
    "fixture/DeleteEndpointConfig.yaml"

requestUpdateMonitoringSchedule :: UpdateMonitoringSchedule -> TestTree
requestUpdateMonitoringSchedule =
  req
    "UpdateMonitoringSchedule"
    "fixture/UpdateMonitoringSchedule.yaml"

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp =
  req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

requestCreateAlgorithm :: CreateAlgorithm -> TestTree
requestCreateAlgorithm =
  req
    "CreateAlgorithm"
    "fixture/CreateAlgorithm.yaml"

requestStopTransformJob :: StopTransformJob -> TestTree
requestStopTransformJob =
  req
    "StopTransformJob"
    "fixture/StopTransformJob.yaml"

requestCreateModel :: CreateModel -> TestTree
requestCreateModel =
  req
    "CreateModel"
    "fixture/CreateModel.yaml"

requestListUserProfiles :: ListUserProfiles -> TestTree
requestListUserProfiles =
  req
    "ListUserProfiles"
    "fixture/ListUserProfiles.yaml"

requestCreateCodeRepository :: CreateCodeRepository -> TestTree
requestCreateCodeRepository =
  req
    "CreateCodeRepository"
    "fixture/CreateCodeRepository.yaml"

requestCreateHyperParameterTuningJob :: CreateHyperParameterTuningJob -> TestTree
requestCreateHyperParameterTuningJob =
  req
    "CreateHyperParameterTuningJob"
    "fixture/CreateHyperParameterTuningJob.yaml"

requestDeleteTrial :: DeleteTrial -> TestTree
requestDeleteTrial =
  req
    "DeleteTrial"
    "fixture/DeleteTrial.yaml"

requestUpdateTrial :: UpdateTrial -> TestTree
requestUpdateTrial =
  req
    "UpdateTrial"
    "fixture/UpdateTrial.yaml"

requestListCodeRepositories :: ListCodeRepositories -> TestTree
requestListCodeRepositories =
  req
    "ListCodeRepositories"
    "fixture/ListCodeRepositories.yaml"

requestDescribeCompilationJob :: DescribeCompilationJob -> TestTree
requestDescribeCompilationJob =
  req
    "DescribeCompilationJob"
    "fixture/DescribeCompilationJob.yaml"

requestListHyperParameterTuningJobs :: ListHyperParameterTuningJobs -> TestTree
requestListHyperParameterTuningJobs =
  req
    "ListHyperParameterTuningJobs"
    "fixture/ListHyperParameterTuningJobs.yaml"

requestListAlgorithms :: ListAlgorithms -> TestTree
requestListAlgorithms =
  req
    "ListAlgorithms"
    "fixture/ListAlgorithms.yaml"

requestRenderUiTemplate :: RenderUiTemplate -> TestTree
requestRenderUiTemplate =
  req
    "RenderUiTemplate"
    "fixture/RenderUiTemplate.yaml"

requestDeleteFlowDefinition :: DeleteFlowDefinition -> TestTree
requestDeleteFlowDefinition =
  req
    "DeleteFlowDefinition"
    "fixture/DeleteFlowDefinition.yaml"

requestCreateTrial :: CreateTrial -> TestTree
requestCreateTrial =
  req
    "CreateTrial"
    "fixture/CreateTrial.yaml"

requestDeleteModel :: DeleteModel -> TestTree
requestDeleteModel =
  req
    "DeleteModel"
    "fixture/DeleteModel.yaml"

requestListModels :: ListModels -> TestTree
requestListModels =
  req
    "ListModels"
    "fixture/ListModels.yaml"

requestDeleteAlgorithm :: DeleteAlgorithm -> TestTree
requestDeleteAlgorithm =
  req
    "DeleteAlgorithm"
    "fixture/DeleteAlgorithm.yaml"

requestAssociateTrialComponent :: AssociateTrialComponent -> TestTree
requestAssociateTrialComponent =
  req
    "AssociateTrialComponent"
    "fixture/AssociateTrialComponent.yaml"

requestDescribeNotebookInstanceLifecycleConfig :: DescribeNotebookInstanceLifecycleConfig -> TestTree
requestDescribeNotebookInstanceLifecycleConfig =
  req
    "DescribeNotebookInstanceLifecycleConfig"
    "fixture/DescribeNotebookInstanceLifecycleConfig.yaml"

requestDescribeWorkforce :: DescribeWorkforce -> TestTree
requestDescribeWorkforce =
  req
    "DescribeWorkforce"
    "fixture/DescribeWorkforce.yaml"

requestCreateModelPackage :: CreateModelPackage -> TestTree
requestCreateModelPackage =
  req
    "CreateModelPackage"
    "fixture/CreateModelPackage.yaml"

requestStopMonitoringSchedule :: StopMonitoringSchedule -> TestTree
requestStopMonitoringSchedule =
  req
    "StopMonitoringSchedule"
    "fixture/StopMonitoringSchedule.yaml"

requestDescribeAppImageConfig :: DescribeAppImageConfig -> TestTree
requestDescribeAppImageConfig =
  req
    "DescribeAppImageConfig"
    "fixture/DescribeAppImageConfig.yaml"

requestListNotebookInstances :: ListNotebookInstances -> TestTree
requestListNotebookInstances =
  req
    "ListNotebookInstances"
    "fixture/ListNotebookInstances.yaml"

requestStopLabelingJob :: StopLabelingJob -> TestTree
requestStopLabelingJob =
  req
    "StopLabelingJob"
    "fixture/StopLabelingJob.yaml"

requestDeleteNotebookInstance :: DeleteNotebookInstance -> TestTree
requestDeleteNotebookInstance =
  req
    "DeleteNotebookInstance"
    "fixture/DeleteNotebookInstance.yaml"

requestUpdateNotebookInstance :: UpdateNotebookInstance -> TestTree
requestUpdateNotebookInstance =
  req
    "UpdateNotebookInstance"
    "fixture/UpdateNotebookInstance.yaml"

requestListModelPackages :: ListModelPackages -> TestTree
requestListModelPackages =
  req
    "ListModelPackages"
    "fixture/ListModelPackages.yaml"

requestDeleteImageVersion :: DeleteImageVersion -> TestTree
requestDeleteImageVersion =
  req
    "DeleteImageVersion"
    "fixture/DeleteImageVersion.yaml"

requestDescribeExperiment :: DescribeExperiment -> TestTree
requestDescribeExperiment =
  req
    "DescribeExperiment"
    "fixture/DescribeExperiment.yaml"

requestDeleteTrialComponent :: DeleteTrialComponent -> TestTree
requestDeleteTrialComponent =
  req
    "DeleteTrialComponent"
    "fixture/DeleteTrialComponent.yaml"

requestUpdateTrialComponent :: UpdateTrialComponent -> TestTree
requestUpdateTrialComponent =
  req
    "UpdateTrialComponent"
    "fixture/UpdateTrialComponent.yaml"

requestDescribeLabelingJob :: DescribeLabelingJob -> TestTree
requestDescribeLabelingJob =
  req
    "DescribeLabelingJob"
    "fixture/DescribeLabelingJob.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestDescribeUserProfile :: DescribeUserProfile -> TestTree
requestDescribeUserProfile =
  req
    "DescribeUserProfile"
    "fixture/DescribeUserProfile.yaml"

requestListMonitoringExecutions :: ListMonitoringExecutions -> TestTree
requestListMonitoringExecutions =
  req
    "ListMonitoringExecutions"
    "fixture/ListMonitoringExecutions.yaml"

requestDeleteHumanTaskUi :: DeleteHumanTaskUi -> TestTree
requestDeleteHumanTaskUi =
  req
    "DeleteHumanTaskUi"
    "fixture/DeleteHumanTaskUi.yaml"

requestStopTrainingJob :: StopTrainingJob -> TestTree
requestStopTrainingJob =
  req
    "StopTrainingJob"
    "fixture/StopTrainingJob.yaml"

requestDescribeAlgorithm :: DescribeAlgorithm -> TestTree
requestDescribeAlgorithm =
  req
    "DescribeAlgorithm"
    "fixture/DescribeAlgorithm.yaml"

requestDescribeModel :: DescribeModel -> TestTree
requestDescribeModel =
  req
    "DescribeModel"
    "fixture/DescribeModel.yaml"

requestListTransformJobs :: ListTransformJobs -> TestTree
requestListTransformJobs =
  req
    "ListTransformJobs"
    "fixture/ListTransformJobs.yaml"

requestDescribeHyperParameterTuningJob :: DescribeHyperParameterTuningJob -> TestTree
requestDescribeHyperParameterTuningJob =
  req
    "DescribeHyperParameterTuningJob"
    "fixture/DescribeHyperParameterTuningJob.yaml"

requestListEndpoints :: ListEndpoints -> TestTree
requestListEndpoints =
  req
    "ListEndpoints"
    "fixture/ListEndpoints.yaml"

requestDescribeFlowDefinition :: DescribeFlowDefinition -> TestTree
requestDescribeFlowDefinition =
  req
    "DescribeFlowDefinition"
    "fixture/DescribeFlowDefinition.yaml"

requestCreatePresignedNotebookInstanceURL :: CreatePresignedNotebookInstanceURL -> TestTree
requestCreatePresignedNotebookInstanceURL =
  req
    "CreatePresignedNotebookInstanceURL"
    "fixture/CreatePresignedNotebookInstanceURL.yaml"

requestListTrainingJobsForHyperParameterTuningJob :: ListTrainingJobsForHyperParameterTuningJob -> TestTree
requestListTrainingJobsForHyperParameterTuningJob =
  req
    "ListTrainingJobsForHyperParameterTuningJob"
    "fixture/ListTrainingJobsForHyperParameterTuningJob.yaml"

requestDescribeDomain :: DescribeDomain -> TestTree
requestDescribeDomain =
  req
    "DescribeDomain"
    "fixture/DescribeDomain.yaml"

requestUpdateWorkteam :: UpdateWorkteam -> TestTree
requestUpdateWorkteam =
  req
    "UpdateWorkteam"
    "fixture/UpdateWorkteam.yaml"

requestDeleteWorkteam :: DeleteWorkteam -> TestTree
requestDeleteWorkteam =
  req
    "DeleteWorkteam"
    "fixture/DeleteWorkteam.yaml"

requestListWorkteams :: ListWorkteams -> TestTree
requestListWorkteams =
  req
    "ListWorkteams"
    "fixture/ListWorkteams.yaml"

requestCreateAutoMLJob :: CreateAutoMLJob -> TestTree
requestCreateAutoMLJob =
  req
    "CreateAutoMLJob"
    "fixture/CreateAutoMLJob.yaml"

requestCreateApp :: CreateApp -> TestTree
requestCreateApp =
  req
    "CreateApp"
    "fixture/CreateApp.yaml"

requestCreateExperiment :: CreateExperiment -> TestTree
requestCreateExperiment =
  req
    "CreateExperiment"
    "fixture/CreateExperiment.yaml"

requestListNotebookInstanceLifecycleConfigs :: ListNotebookInstanceLifecycleConfigs -> TestTree
requestListNotebookInstanceLifecycleConfigs =
  req
    "ListNotebookInstanceLifecycleConfigs"
    "fixture/ListNotebookInstanceLifecycleConfigs.yaml"

requestListWorkforces :: ListWorkforces -> TestTree
requestListWorkforces =
  req
    "ListWorkforces"
    "fixture/ListWorkforces.yaml"

requestDescribeSubscribedWorkteam :: DescribeSubscribedWorkteam -> TestTree
requestDescribeSubscribedWorkteam =
  req
    "DescribeSubscribedWorkteam"
    "fixture/DescribeSubscribedWorkteam.yaml"

requestCreateWorkteam :: CreateWorkteam -> TestTree
requestCreateWorkteam =
  req
    "CreateWorkteam"
    "fixture/CreateWorkteam.yaml"

requestCreateNotebookInstanceLifecycleConfig :: CreateNotebookInstanceLifecycleConfig -> TestTree
requestCreateNotebookInstanceLifecycleConfig =
  req
    "CreateNotebookInstanceLifecycleConfig"
    "fixture/CreateNotebookInstanceLifecycleConfig.yaml"

requestListMonitoringSchedules :: ListMonitoringSchedules -> TestTree
requestListMonitoringSchedules =
  req
    "ListMonitoringSchedules"
    "fixture/ListMonitoringSchedules.yaml"

requestListLabelingJobs :: ListLabelingJobs -> TestTree
requestListLabelingJobs =
  req
    "ListLabelingJobs"
    "fixture/ListLabelingJobs.yaml"

requestStartNotebookInstance :: StartNotebookInstance -> TestTree
requestStartNotebookInstance =
  req
    "StartNotebookInstance"
    "fixture/StartNotebookInstance.yaml"

requestUpdateExperiment :: UpdateExperiment -> TestTree
requestUpdateExperiment =
  req
    "UpdateExperiment"
    "fixture/UpdateExperiment.yaml"

requestDeleteExperiment :: DeleteExperiment -> TestTree
requestDeleteExperiment =
  req
    "DeleteExperiment"
    "fixture/DeleteExperiment.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

requestCreateWorkforce :: CreateWorkforce -> TestTree
requestCreateWorkforce =
  req
    "CreateWorkforce"
    "fixture/CreateWorkforce.yaml"

requestDescribeTrialComponent :: DescribeTrialComponent -> TestTree
requestDescribeTrialComponent =
  req
    "DescribeTrialComponent"
    "fixture/DescribeTrialComponent.yaml"

requestDescribeImageVersion :: DescribeImageVersion -> TestTree
requestDescribeImageVersion =
  req
    "DescribeImageVersion"
    "fixture/DescribeImageVersion.yaml"

requestListEndpointConfigs :: ListEndpointConfigs -> TestTree
requestListEndpointConfigs =
  req
    "ListEndpointConfigs"
    "fixture/ListEndpointConfigs.yaml"

requestCreateFlowDefinition :: CreateFlowDefinition -> TestTree
requestCreateFlowDefinition =
  req
    "CreateFlowDefinition"
    "fixture/CreateFlowDefinition.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestDescribeHumanTaskUi :: DescribeHumanTaskUi -> TestTree
requestDescribeHumanTaskUi =
  req
    "DescribeHumanTaskUi"
    "fixture/DescribeHumanTaskUi.yaml"

requestCreateTrainingJob :: CreateTrainingJob -> TestTree
requestCreateTrainingJob =
  req
    "CreateTrainingJob"
    "fixture/CreateTrainingJob.yaml"

requestDeleteUserProfile :: DeleteUserProfile -> TestTree
requestDeleteUserProfile =
  req
    "DeleteUserProfile"
    "fixture/DeleteUserProfile.yaml"

requestUpdateUserProfile :: UpdateUserProfile -> TestTree
requestUpdateUserProfile =
  req
    "UpdateUserProfile"
    "fixture/UpdateUserProfile.yaml"

requestCreateImage :: CreateImage -> TestTree
requestCreateImage =
  req
    "CreateImage"
    "fixture/CreateImage.yaml"

requestListTrials :: ListTrials -> TestTree
requestListTrials =
  req
    "ListTrials"
    "fixture/ListTrials.yaml"

requestStopCompilationJob :: StopCompilationJob -> TestTree
requestStopCompilationJob =
  req
    "StopCompilationJob"
    "fixture/StopCompilationJob.yaml"

requestListImages :: ListImages -> TestTree
requestListImages =
  req
    "ListImages"
    "fixture/ListImages.yaml"

requestCreateUserProfile :: CreateUserProfile -> TestTree
requestCreateUserProfile =
  req
    "CreateUserProfile"
    "fixture/CreateUserProfile.yaml"

requestSearch :: Search -> TestTree
requestSearch =
  req
    "Search"
    "fixture/Search.yaml"

requestUpdateCodeRepository :: UpdateCodeRepository -> TestTree
requestUpdateCodeRepository =
  req
    "UpdateCodeRepository"
    "fixture/UpdateCodeRepository.yaml"

requestDeleteCodeRepository :: DeleteCodeRepository -> TestTree
requestDeleteCodeRepository =
  req
    "DeleteCodeRepository"
    "fixture/DeleteCodeRepository.yaml"

requestDescribeTransformJob :: DescribeTransformJob -> TestTree
requestDescribeTransformJob =
  req
    "DescribeTransformJob"
    "fixture/DescribeTransformJob.yaml"

requestListCandidatesForAutoMLJob :: ListCandidatesForAutoMLJob -> TestTree
requestListCandidatesForAutoMLJob =
  req
    "ListCandidatesForAutoMLJob"
    "fixture/ListCandidatesForAutoMLJob.yaml"

requestDeleteImage :: DeleteImage -> TestTree
requestDeleteImage =
  req
    "DeleteImage"
    "fixture/DeleteImage.yaml"

requestUpdateImage :: UpdateImage -> TestTree
requestUpdateImage =
  req
    "UpdateImage"
    "fixture/UpdateImage.yaml"

requestListFlowDefinitions :: ListFlowDefinitions -> TestTree
requestListFlowDefinitions =
  req
    "ListFlowDefinitions"
    "fixture/ListFlowDefinitions.yaml"

requestDescribeEndpoint :: DescribeEndpoint -> TestTree
requestDescribeEndpoint =
  req
    "DescribeEndpoint"
    "fixture/DescribeEndpoint.yaml"

requestListTrainingJobs :: ListTrainingJobs -> TestTree
requestListTrainingJobs =
  req
    "ListTrainingJobs"
    "fixture/ListTrainingJobs.yaml"

requestDescribeWorkteam :: DescribeWorkteam -> TestTree
requestDescribeWorkteam =
  req
    "DescribeWorkteam"
    "fixture/DescribeWorkteam.yaml"

requestListSubscribedWorkteams :: ListSubscribedWorkteams -> TestTree
requestListSubscribedWorkteams =
  req
    "ListSubscribedWorkteams"
    "fixture/ListSubscribedWorkteams.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestUpdateDomain :: UpdateDomain -> TestTree
requestUpdateDomain =
  req
    "UpdateDomain"
    "fixture/UpdateDomain.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

requestCreateImageVersion :: CreateImageVersion -> TestTree
requestCreateImageVersion =
  req
    "CreateImageVersion"
    "fixture/CreateImageVersion.yaml"

requestStartMonitoringSchedule :: StartMonitoringSchedule -> TestTree
requestStartMonitoringSchedule =
  req
    "StartMonitoringSchedule"
    "fixture/StartMonitoringSchedule.yaml"

requestStopAutoMLJob :: StopAutoMLJob -> TestTree
requestStopAutoMLJob =
  req
    "StopAutoMLJob"
    "fixture/StopAutoMLJob.yaml"

requestCreateTrialComponent :: CreateTrialComponent -> TestTree
requestCreateTrialComponent =
  req
    "CreateTrialComponent"
    "fixture/CreateTrialComponent.yaml"

requestDescribeProcessingJob :: DescribeProcessingJob -> TestTree
requestDescribeProcessingJob =
  req
    "DescribeProcessingJob"
    "fixture/DescribeProcessingJob.yaml"

-- Responses

responseCreateNotebookInstance :: CreateNotebookInstanceResponse -> TestTree
responseCreateNotebookInstance =
  res
    "CreateNotebookInstanceResponse"
    "fixture/CreateNotebookInstanceResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateNotebookInstance)

responseDeleteModelPackage :: DeleteModelPackageResponse -> TestTree
responseDeleteModelPackage =
  res
    "DeleteModelPackageResponse"
    "fixture/DeleteModelPackageResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteModelPackage)

responseDescribeMonitoringSchedule :: DescribeMonitoringScheduleResponse -> TestTree
responseDescribeMonitoringSchedule =
  res
    "DescribeMonitoringScheduleResponse"
    "fixture/DescribeMonitoringScheduleResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeMonitoringSchedule)

responseListTrialComponents :: ListTrialComponentsResponse -> TestTree
responseListTrialComponents =
  res
    "ListTrialComponentsResponse"
    "fixture/ListTrialComponentsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListTrialComponents)

responseDescribeEndpointConfig :: DescribeEndpointConfigResponse -> TestTree
responseDescribeEndpointConfig =
  res
    "DescribeEndpointConfigResponse"
    "fixture/DescribeEndpointConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeEndpointConfig)

responseDescribeApp :: DescribeAppResponse -> TestTree
responseDescribeApp =
  res
    "DescribeAppResponse"
    "fixture/DescribeAppResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeApp)

responseListImageVersions :: ListImageVersionsResponse -> TestTree
responseListImageVersions =
  res
    "ListImageVersionsResponse"
    "fixture/ListImageVersionsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListImageVersions)

responseDescribeAutoMLJob :: DescribeAutoMLJobResponse -> TestTree
responseDescribeAutoMLJob =
  res
    "DescribeAutoMLJobResponse"
    "fixture/DescribeAutoMLJobResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeAutoMLJob)

responseStopProcessingJob :: StopProcessingJobResponse -> TestTree
responseStopProcessingJob =
  res
    "StopProcessingJobResponse"
    "fixture/StopProcessingJobResponse.proto"
    sageMaker
    (Proxy :: Proxy StopProcessingJob)

responseListLabelingJobsForWorkteam :: ListLabelingJobsForWorkteamResponse -> TestTree
responseListLabelingJobsForWorkteam =
  res
    "ListLabelingJobsForWorkteamResponse"
    "fixture/ListLabelingJobsForWorkteamResponse.proto"
    sageMaker
    (Proxy :: Proxy ListLabelingJobsForWorkteam)

responseCreateTransformJob :: CreateTransformJobResponse -> TestTree
responseCreateTransformJob =
  res
    "CreateTransformJobResponse"
    "fixture/CreateTransformJobResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateTransformJob)

responseListCompilationJobs :: ListCompilationJobsResponse -> TestTree
responseListCompilationJobs =
  res
    "ListCompilationJobsResponse"
    "fixture/ListCompilationJobsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListCompilationJobs)

responseDisassociateTrialComponent :: DisassociateTrialComponentResponse -> TestTree
responseDisassociateTrialComponent =
  res
    "DisassociateTrialComponentResponse"
    "fixture/DisassociateTrialComponentResponse.proto"
    sageMaker
    (Proxy :: Proxy DisassociateTrialComponent)

responseStopHyperParameterTuningJob :: StopHyperParameterTuningJobResponse -> TestTree
responseStopHyperParameterTuningJob =
  res
    "StopHyperParameterTuningJobResponse"
    "fixture/StopHyperParameterTuningJobResponse.proto"
    sageMaker
    (Proxy :: Proxy StopHyperParameterTuningJob)

responseListHumanTaskUis :: ListHumanTaskUisResponse -> TestTree
responseListHumanTaskUis =
  res
    "ListHumanTaskUisResponse"
    "fixture/ListHumanTaskUisResponse.proto"
    sageMaker
    (Proxy :: Proxy ListHumanTaskUis)

responseCreateEndpoint :: CreateEndpointResponse -> TestTree
responseCreateEndpoint =
  res
    "CreateEndpointResponse"
    "fixture/CreateEndpointResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateEndpoint)

responseGetSearchSuggestions :: GetSearchSuggestionsResponse -> TestTree
responseGetSearchSuggestions =
  res
    "GetSearchSuggestionsResponse"
    "fixture/GetSearchSuggestionsResponse.proto"
    sageMaker
    (Proxy :: Proxy GetSearchSuggestions)

responseDescribeTrial :: DescribeTrialResponse -> TestTree
responseDescribeTrial =
  res
    "DescribeTrialResponse"
    "fixture/DescribeTrialResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeTrial)

responseCreatePresignedDomainURL :: CreatePresignedDomainURLResponse -> TestTree
responseCreatePresignedDomainURL =
  res
    "CreatePresignedDomainURLResponse"
    "fixture/CreatePresignedDomainURLResponse.proto"
    sageMaker
    (Proxy :: Proxy CreatePresignedDomainURL)

responseDescribeCodeRepository :: DescribeCodeRepositoryResponse -> TestTree
responseDescribeCodeRepository =
  res
    "DescribeCodeRepositoryResponse"
    "fixture/DescribeCodeRepositoryResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeCodeRepository)

responseDescribeImage :: DescribeImageResponse -> TestTree
responseDescribeImage =
  res
    "DescribeImageResponse"
    "fixture/DescribeImageResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeImage)

responseDescribeTrainingJob :: DescribeTrainingJobResponse -> TestTree
responseDescribeTrainingJob =
  res
    "DescribeTrainingJobResponse"
    "fixture/DescribeTrainingJobResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeTrainingJob)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteEndpoint)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint =
  res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateEndpoint)

responseCreateHumanTaskUi :: CreateHumanTaskUiResponse -> TestTree
responseCreateHumanTaskUi =
  res
    "CreateHumanTaskUiResponse"
    "fixture/CreateHumanTaskUiResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateHumanTaskUi)

responseCreateCompilationJob :: CreateCompilationJobResponse -> TestTree
responseCreateCompilationJob =
  res
    "CreateCompilationJobResponse"
    "fixture/CreateCompilationJobResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateCompilationJob)

responseDeleteAppImageConfig :: DeleteAppImageConfigResponse -> TestTree
responseDeleteAppImageConfig =
  res
    "DeleteAppImageConfigResponse"
    "fixture/DeleteAppImageConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteAppImageConfig)

responseUpdateAppImageConfig :: UpdateAppImageConfigResponse -> TestTree
responseUpdateAppImageConfig =
  res
    "UpdateAppImageConfigResponse"
    "fixture/UpdateAppImageConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateAppImageConfig)

responseDeleteNotebookInstanceLifecycleConfig :: DeleteNotebookInstanceLifecycleConfigResponse -> TestTree
responseDeleteNotebookInstanceLifecycleConfig =
  res
    "DeleteNotebookInstanceLifecycleConfigResponse"
    "fixture/DeleteNotebookInstanceLifecycleConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteNotebookInstanceLifecycleConfig)

responseUpdateNotebookInstanceLifecycleConfig :: UpdateNotebookInstanceLifecycleConfigResponse -> TestTree
responseUpdateNotebookInstanceLifecycleConfig =
  res
    "UpdateNotebookInstanceLifecycleConfigResponse"
    "fixture/UpdateNotebookInstanceLifecycleConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateNotebookInstanceLifecycleConfig)

responseDeleteWorkforce :: DeleteWorkforceResponse -> TestTree
responseDeleteWorkforce =
  res
    "DeleteWorkforceResponse"
    "fixture/DeleteWorkforceResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteWorkforce)

responseUpdateWorkforce :: UpdateWorkforceResponse -> TestTree
responseUpdateWorkforce =
  res
    "UpdateWorkforceResponse"
    "fixture/UpdateWorkforceResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateWorkforce)

responseListProcessingJobs :: ListProcessingJobsResponse -> TestTree
responseListProcessingJobs =
  res
    "ListProcessingJobsResponse"
    "fixture/ListProcessingJobsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListProcessingJobs)

responseCreateLabelingJob :: CreateLabelingJobResponse -> TestTree
responseCreateLabelingJob =
  res
    "CreateLabelingJobResponse"
    "fixture/CreateLabelingJobResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateLabelingJob)

responseDescribeNotebookInstance :: DescribeNotebookInstanceResponse -> TestTree
responseDescribeNotebookInstance =
  res
    "DescribeNotebookInstanceResponse"
    "fixture/DescribeNotebookInstanceResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeNotebookInstance)

responseCreateMonitoringSchedule :: CreateMonitoringScheduleResponse -> TestTree
responseCreateMonitoringSchedule =
  res
    "CreateMonitoringScheduleResponse"
    "fixture/CreateMonitoringScheduleResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateMonitoringSchedule)

responseListAppImageConfigs :: ListAppImageConfigsResponse -> TestTree
responseListAppImageConfigs =
  res
    "ListAppImageConfigsResponse"
    "fixture/ListAppImageConfigsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListAppImageConfigs)

responseCreateEndpointConfig :: CreateEndpointConfigResponse -> TestTree
responseCreateEndpointConfig =
  res
    "CreateEndpointConfigResponse"
    "fixture/CreateEndpointConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateEndpointConfig)

responseStopNotebookInstance :: StopNotebookInstanceResponse -> TestTree
responseStopNotebookInstance =
  res
    "StopNotebookInstanceResponse"
    "fixture/StopNotebookInstanceResponse.proto"
    sageMaker
    (Proxy :: Proxy StopNotebookInstance)

responseUpdateEndpointWeightsAndCapacities :: UpdateEndpointWeightsAndCapacitiesResponse -> TestTree
responseUpdateEndpointWeightsAndCapacities =
  res
    "UpdateEndpointWeightsAndCapacitiesResponse"
    "fixture/UpdateEndpointWeightsAndCapacitiesResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateEndpointWeightsAndCapacities)

responseCreateAppImageConfig :: CreateAppImageConfigResponse -> TestTree
responseCreateAppImageConfig =
  res
    "CreateAppImageConfigResponse"
    "fixture/CreateAppImageConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateAppImageConfig)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteTags)

responseListExperiments :: ListExperimentsResponse -> TestTree
responseListExperiments =
  res
    "ListExperimentsResponse"
    "fixture/ListExperimentsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListExperiments)

responseListAutoMLJobs :: ListAutoMLJobsResponse -> TestTree
responseListAutoMLJobs =
  res
    "ListAutoMLJobsResponse"
    "fixture/ListAutoMLJobsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListAutoMLJobs)

responseListApps :: ListAppsResponse -> TestTree
responseListApps =
  res
    "ListAppsResponse"
    "fixture/ListAppsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListApps)

responseCreateProcessingJob :: CreateProcessingJobResponse -> TestTree
responseCreateProcessingJob =
  res
    "CreateProcessingJobResponse"
    "fixture/CreateProcessingJobResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateProcessingJob)

responseDeleteMonitoringSchedule :: DeleteMonitoringScheduleResponse -> TestTree
responseDeleteMonitoringSchedule =
  res
    "DeleteMonitoringScheduleResponse"
    "fixture/DeleteMonitoringScheduleResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteMonitoringSchedule)

responseDescribeModelPackage :: DescribeModelPackageResponse -> TestTree
responseDescribeModelPackage =
  res
    "DescribeModelPackageResponse"
    "fixture/DescribeModelPackageResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeModelPackage)

responseDeleteEndpointConfig :: DeleteEndpointConfigResponse -> TestTree
responseDeleteEndpointConfig =
  res
    "DeleteEndpointConfigResponse"
    "fixture/DeleteEndpointConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteEndpointConfig)

responseUpdateMonitoringSchedule :: UpdateMonitoringScheduleResponse -> TestTree
responseUpdateMonitoringSchedule =
  res
    "UpdateMonitoringScheduleResponse"
    "fixture/UpdateMonitoringScheduleResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateMonitoringSchedule)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteApp)

responseCreateAlgorithm :: CreateAlgorithmResponse -> TestTree
responseCreateAlgorithm =
  res
    "CreateAlgorithmResponse"
    "fixture/CreateAlgorithmResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateAlgorithm)

responseStopTransformJob :: StopTransformJobResponse -> TestTree
responseStopTransformJob =
  res
    "StopTransformJobResponse"
    "fixture/StopTransformJobResponse.proto"
    sageMaker
    (Proxy :: Proxy StopTransformJob)

responseCreateModel :: CreateModelResponse -> TestTree
responseCreateModel =
  res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateModel)

responseListUserProfiles :: ListUserProfilesResponse -> TestTree
responseListUserProfiles =
  res
    "ListUserProfilesResponse"
    "fixture/ListUserProfilesResponse.proto"
    sageMaker
    (Proxy :: Proxy ListUserProfiles)

responseCreateCodeRepository :: CreateCodeRepositoryResponse -> TestTree
responseCreateCodeRepository =
  res
    "CreateCodeRepositoryResponse"
    "fixture/CreateCodeRepositoryResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateCodeRepository)

responseCreateHyperParameterTuningJob :: CreateHyperParameterTuningJobResponse -> TestTree
responseCreateHyperParameterTuningJob =
  res
    "CreateHyperParameterTuningJobResponse"
    "fixture/CreateHyperParameterTuningJobResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateHyperParameterTuningJob)

responseDeleteTrial :: DeleteTrialResponse -> TestTree
responseDeleteTrial =
  res
    "DeleteTrialResponse"
    "fixture/DeleteTrialResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteTrial)

responseUpdateTrial :: UpdateTrialResponse -> TestTree
responseUpdateTrial =
  res
    "UpdateTrialResponse"
    "fixture/UpdateTrialResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateTrial)

responseListCodeRepositories :: ListCodeRepositoriesResponse -> TestTree
responseListCodeRepositories =
  res
    "ListCodeRepositoriesResponse"
    "fixture/ListCodeRepositoriesResponse.proto"
    sageMaker
    (Proxy :: Proxy ListCodeRepositories)

responseDescribeCompilationJob :: DescribeCompilationJobResponse -> TestTree
responseDescribeCompilationJob =
  res
    "DescribeCompilationJobResponse"
    "fixture/DescribeCompilationJobResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeCompilationJob)

responseListHyperParameterTuningJobs :: ListHyperParameterTuningJobsResponse -> TestTree
responseListHyperParameterTuningJobs =
  res
    "ListHyperParameterTuningJobsResponse"
    "fixture/ListHyperParameterTuningJobsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListHyperParameterTuningJobs)

responseListAlgorithms :: ListAlgorithmsResponse -> TestTree
responseListAlgorithms =
  res
    "ListAlgorithmsResponse"
    "fixture/ListAlgorithmsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListAlgorithms)

responseRenderUiTemplate :: RenderUiTemplateResponse -> TestTree
responseRenderUiTemplate =
  res
    "RenderUiTemplateResponse"
    "fixture/RenderUiTemplateResponse.proto"
    sageMaker
    (Proxy :: Proxy RenderUiTemplate)

responseDeleteFlowDefinition :: DeleteFlowDefinitionResponse -> TestTree
responseDeleteFlowDefinition =
  res
    "DeleteFlowDefinitionResponse"
    "fixture/DeleteFlowDefinitionResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteFlowDefinition)

responseCreateTrial :: CreateTrialResponse -> TestTree
responseCreateTrial =
  res
    "CreateTrialResponse"
    "fixture/CreateTrialResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateTrial)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel =
  res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteModel)

responseListModels :: ListModelsResponse -> TestTree
responseListModels =
  res
    "ListModelsResponse"
    "fixture/ListModelsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListModels)

responseDeleteAlgorithm :: DeleteAlgorithmResponse -> TestTree
responseDeleteAlgorithm =
  res
    "DeleteAlgorithmResponse"
    "fixture/DeleteAlgorithmResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteAlgorithm)

responseAssociateTrialComponent :: AssociateTrialComponentResponse -> TestTree
responseAssociateTrialComponent =
  res
    "AssociateTrialComponentResponse"
    "fixture/AssociateTrialComponentResponse.proto"
    sageMaker
    (Proxy :: Proxy AssociateTrialComponent)

responseDescribeNotebookInstanceLifecycleConfig :: DescribeNotebookInstanceLifecycleConfigResponse -> TestTree
responseDescribeNotebookInstanceLifecycleConfig =
  res
    "DescribeNotebookInstanceLifecycleConfigResponse"
    "fixture/DescribeNotebookInstanceLifecycleConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeNotebookInstanceLifecycleConfig)

responseDescribeWorkforce :: DescribeWorkforceResponse -> TestTree
responseDescribeWorkforce =
  res
    "DescribeWorkforceResponse"
    "fixture/DescribeWorkforceResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeWorkforce)

responseCreateModelPackage :: CreateModelPackageResponse -> TestTree
responseCreateModelPackage =
  res
    "CreateModelPackageResponse"
    "fixture/CreateModelPackageResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateModelPackage)

responseStopMonitoringSchedule :: StopMonitoringScheduleResponse -> TestTree
responseStopMonitoringSchedule =
  res
    "StopMonitoringScheduleResponse"
    "fixture/StopMonitoringScheduleResponse.proto"
    sageMaker
    (Proxy :: Proxy StopMonitoringSchedule)

responseDescribeAppImageConfig :: DescribeAppImageConfigResponse -> TestTree
responseDescribeAppImageConfig =
  res
    "DescribeAppImageConfigResponse"
    "fixture/DescribeAppImageConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeAppImageConfig)

responseListNotebookInstances :: ListNotebookInstancesResponse -> TestTree
responseListNotebookInstances =
  res
    "ListNotebookInstancesResponse"
    "fixture/ListNotebookInstancesResponse.proto"
    sageMaker
    (Proxy :: Proxy ListNotebookInstances)

responseStopLabelingJob :: StopLabelingJobResponse -> TestTree
responseStopLabelingJob =
  res
    "StopLabelingJobResponse"
    "fixture/StopLabelingJobResponse.proto"
    sageMaker
    (Proxy :: Proxy StopLabelingJob)

responseDeleteNotebookInstance :: DeleteNotebookInstanceResponse -> TestTree
responseDeleteNotebookInstance =
  res
    "DeleteNotebookInstanceResponse"
    "fixture/DeleteNotebookInstanceResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteNotebookInstance)

responseUpdateNotebookInstance :: UpdateNotebookInstanceResponse -> TestTree
responseUpdateNotebookInstance =
  res
    "UpdateNotebookInstanceResponse"
    "fixture/UpdateNotebookInstanceResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateNotebookInstance)

responseListModelPackages :: ListModelPackagesResponse -> TestTree
responseListModelPackages =
  res
    "ListModelPackagesResponse"
    "fixture/ListModelPackagesResponse.proto"
    sageMaker
    (Proxy :: Proxy ListModelPackages)

responseDeleteImageVersion :: DeleteImageVersionResponse -> TestTree
responseDeleteImageVersion =
  res
    "DeleteImageVersionResponse"
    "fixture/DeleteImageVersionResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteImageVersion)

responseDescribeExperiment :: DescribeExperimentResponse -> TestTree
responseDescribeExperiment =
  res
    "DescribeExperimentResponse"
    "fixture/DescribeExperimentResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeExperiment)

responseDeleteTrialComponent :: DeleteTrialComponentResponse -> TestTree
responseDeleteTrialComponent =
  res
    "DeleteTrialComponentResponse"
    "fixture/DeleteTrialComponentResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteTrialComponent)

responseUpdateTrialComponent :: UpdateTrialComponentResponse -> TestTree
responseUpdateTrialComponent =
  res
    "UpdateTrialComponentResponse"
    "fixture/UpdateTrialComponentResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateTrialComponent)

responseDescribeLabelingJob :: DescribeLabelingJobResponse -> TestTree
responseDescribeLabelingJob =
  res
    "DescribeLabelingJobResponse"
    "fixture/DescribeLabelingJobResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeLabelingJob)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateDomain)

responseDescribeUserProfile :: DescribeUserProfileResponse -> TestTree
responseDescribeUserProfile =
  res
    "DescribeUserProfileResponse"
    "fixture/DescribeUserProfileResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeUserProfile)

responseListMonitoringExecutions :: ListMonitoringExecutionsResponse -> TestTree
responseListMonitoringExecutions =
  res
    "ListMonitoringExecutionsResponse"
    "fixture/ListMonitoringExecutionsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListMonitoringExecutions)

responseDeleteHumanTaskUi :: DeleteHumanTaskUiResponse -> TestTree
responseDeleteHumanTaskUi =
  res
    "DeleteHumanTaskUiResponse"
    "fixture/DeleteHumanTaskUiResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteHumanTaskUi)

responseStopTrainingJob :: StopTrainingJobResponse -> TestTree
responseStopTrainingJob =
  res
    "StopTrainingJobResponse"
    "fixture/StopTrainingJobResponse.proto"
    sageMaker
    (Proxy :: Proxy StopTrainingJob)

responseDescribeAlgorithm :: DescribeAlgorithmResponse -> TestTree
responseDescribeAlgorithm =
  res
    "DescribeAlgorithmResponse"
    "fixture/DescribeAlgorithmResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeAlgorithm)

responseDescribeModel :: DescribeModelResponse -> TestTree
responseDescribeModel =
  res
    "DescribeModelResponse"
    "fixture/DescribeModelResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeModel)

responseListTransformJobs :: ListTransformJobsResponse -> TestTree
responseListTransformJobs =
  res
    "ListTransformJobsResponse"
    "fixture/ListTransformJobsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListTransformJobs)

responseDescribeHyperParameterTuningJob :: DescribeHyperParameterTuningJobResponse -> TestTree
responseDescribeHyperParameterTuningJob =
  res
    "DescribeHyperParameterTuningJobResponse"
    "fixture/DescribeHyperParameterTuningJobResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeHyperParameterTuningJob)

responseListEndpoints :: ListEndpointsResponse -> TestTree
responseListEndpoints =
  res
    "ListEndpointsResponse"
    "fixture/ListEndpointsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListEndpoints)

responseDescribeFlowDefinition :: DescribeFlowDefinitionResponse -> TestTree
responseDescribeFlowDefinition =
  res
    "DescribeFlowDefinitionResponse"
    "fixture/DescribeFlowDefinitionResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeFlowDefinition)

responseCreatePresignedNotebookInstanceURL :: CreatePresignedNotebookInstanceURLResponse -> TestTree
responseCreatePresignedNotebookInstanceURL =
  res
    "CreatePresignedNotebookInstanceURLResponse"
    "fixture/CreatePresignedNotebookInstanceURLResponse.proto"
    sageMaker
    (Proxy :: Proxy CreatePresignedNotebookInstanceURL)

responseListTrainingJobsForHyperParameterTuningJob :: ListTrainingJobsForHyperParameterTuningJobResponse -> TestTree
responseListTrainingJobsForHyperParameterTuningJob =
  res
    "ListTrainingJobsForHyperParameterTuningJobResponse"
    "fixture/ListTrainingJobsForHyperParameterTuningJobResponse.proto"
    sageMaker
    (Proxy :: Proxy ListTrainingJobsForHyperParameterTuningJob)

responseDescribeDomain :: DescribeDomainResponse -> TestTree
responseDescribeDomain =
  res
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeDomain)

responseUpdateWorkteam :: UpdateWorkteamResponse -> TestTree
responseUpdateWorkteam =
  res
    "UpdateWorkteamResponse"
    "fixture/UpdateWorkteamResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateWorkteam)

responseDeleteWorkteam :: DeleteWorkteamResponse -> TestTree
responseDeleteWorkteam =
  res
    "DeleteWorkteamResponse"
    "fixture/DeleteWorkteamResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteWorkteam)

responseListWorkteams :: ListWorkteamsResponse -> TestTree
responseListWorkteams =
  res
    "ListWorkteamsResponse"
    "fixture/ListWorkteamsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListWorkteams)

responseCreateAutoMLJob :: CreateAutoMLJobResponse -> TestTree
responseCreateAutoMLJob =
  res
    "CreateAutoMLJobResponse"
    "fixture/CreateAutoMLJobResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateAutoMLJob)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateApp)

responseCreateExperiment :: CreateExperimentResponse -> TestTree
responseCreateExperiment =
  res
    "CreateExperimentResponse"
    "fixture/CreateExperimentResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateExperiment)

responseListNotebookInstanceLifecycleConfigs :: ListNotebookInstanceLifecycleConfigsResponse -> TestTree
responseListNotebookInstanceLifecycleConfigs =
  res
    "ListNotebookInstanceLifecycleConfigsResponse"
    "fixture/ListNotebookInstanceLifecycleConfigsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListNotebookInstanceLifecycleConfigs)

responseListWorkforces :: ListWorkforcesResponse -> TestTree
responseListWorkforces =
  res
    "ListWorkforcesResponse"
    "fixture/ListWorkforcesResponse.proto"
    sageMaker
    (Proxy :: Proxy ListWorkforces)

responseDescribeSubscribedWorkteam :: DescribeSubscribedWorkteamResponse -> TestTree
responseDescribeSubscribedWorkteam =
  res
    "DescribeSubscribedWorkteamResponse"
    "fixture/DescribeSubscribedWorkteamResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeSubscribedWorkteam)

responseCreateWorkteam :: CreateWorkteamResponse -> TestTree
responseCreateWorkteam =
  res
    "CreateWorkteamResponse"
    "fixture/CreateWorkteamResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateWorkteam)

responseCreateNotebookInstanceLifecycleConfig :: CreateNotebookInstanceLifecycleConfigResponse -> TestTree
responseCreateNotebookInstanceLifecycleConfig =
  res
    "CreateNotebookInstanceLifecycleConfigResponse"
    "fixture/CreateNotebookInstanceLifecycleConfigResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateNotebookInstanceLifecycleConfig)

responseListMonitoringSchedules :: ListMonitoringSchedulesResponse -> TestTree
responseListMonitoringSchedules =
  res
    "ListMonitoringSchedulesResponse"
    "fixture/ListMonitoringSchedulesResponse.proto"
    sageMaker
    (Proxy :: Proxy ListMonitoringSchedules)

responseListLabelingJobs :: ListLabelingJobsResponse -> TestTree
responseListLabelingJobs =
  res
    "ListLabelingJobsResponse"
    "fixture/ListLabelingJobsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListLabelingJobs)

responseStartNotebookInstance :: StartNotebookInstanceResponse -> TestTree
responseStartNotebookInstance =
  res
    "StartNotebookInstanceResponse"
    "fixture/StartNotebookInstanceResponse.proto"
    sageMaker
    (Proxy :: Proxy StartNotebookInstance)

responseUpdateExperiment :: UpdateExperimentResponse -> TestTree
responseUpdateExperiment =
  res
    "UpdateExperimentResponse"
    "fixture/UpdateExperimentResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateExperiment)

responseDeleteExperiment :: DeleteExperimentResponse -> TestTree
responseDeleteExperiment =
  res
    "DeleteExperimentResponse"
    "fixture/DeleteExperimentResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteExperiment)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    sageMaker
    (Proxy :: Proxy AddTags)

responseCreateWorkforce :: CreateWorkforceResponse -> TestTree
responseCreateWorkforce =
  res
    "CreateWorkforceResponse"
    "fixture/CreateWorkforceResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateWorkforce)

responseDescribeTrialComponent :: DescribeTrialComponentResponse -> TestTree
responseDescribeTrialComponent =
  res
    "DescribeTrialComponentResponse"
    "fixture/DescribeTrialComponentResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeTrialComponent)

responseDescribeImageVersion :: DescribeImageVersionResponse -> TestTree
responseDescribeImageVersion =
  res
    "DescribeImageVersionResponse"
    "fixture/DescribeImageVersionResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeImageVersion)

responseListEndpointConfigs :: ListEndpointConfigsResponse -> TestTree
responseListEndpointConfigs =
  res
    "ListEndpointConfigsResponse"
    "fixture/ListEndpointConfigsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListEndpointConfigs)

responseCreateFlowDefinition :: CreateFlowDefinitionResponse -> TestTree
responseCreateFlowDefinition =
  res
    "CreateFlowDefinitionResponse"
    "fixture/CreateFlowDefinitionResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateFlowDefinition)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListTags)

responseDescribeHumanTaskUi :: DescribeHumanTaskUiResponse -> TestTree
responseDescribeHumanTaskUi =
  res
    "DescribeHumanTaskUiResponse"
    "fixture/DescribeHumanTaskUiResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeHumanTaskUi)

responseCreateTrainingJob :: CreateTrainingJobResponse -> TestTree
responseCreateTrainingJob =
  res
    "CreateTrainingJobResponse"
    "fixture/CreateTrainingJobResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateTrainingJob)

responseDeleteUserProfile :: DeleteUserProfileResponse -> TestTree
responseDeleteUserProfile =
  res
    "DeleteUserProfileResponse"
    "fixture/DeleteUserProfileResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteUserProfile)

responseUpdateUserProfile :: UpdateUserProfileResponse -> TestTree
responseUpdateUserProfile =
  res
    "UpdateUserProfileResponse"
    "fixture/UpdateUserProfileResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateUserProfile)

responseCreateImage :: CreateImageResponse -> TestTree
responseCreateImage =
  res
    "CreateImageResponse"
    "fixture/CreateImageResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateImage)

responseListTrials :: ListTrialsResponse -> TestTree
responseListTrials =
  res
    "ListTrialsResponse"
    "fixture/ListTrialsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListTrials)

responseStopCompilationJob :: StopCompilationJobResponse -> TestTree
responseStopCompilationJob =
  res
    "StopCompilationJobResponse"
    "fixture/StopCompilationJobResponse.proto"
    sageMaker
    (Proxy :: Proxy StopCompilationJob)

responseListImages :: ListImagesResponse -> TestTree
responseListImages =
  res
    "ListImagesResponse"
    "fixture/ListImagesResponse.proto"
    sageMaker
    (Proxy :: Proxy ListImages)

responseCreateUserProfile :: CreateUserProfileResponse -> TestTree
responseCreateUserProfile =
  res
    "CreateUserProfileResponse"
    "fixture/CreateUserProfileResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateUserProfile)

responseSearch :: SearchResponse -> TestTree
responseSearch =
  res
    "SearchResponse"
    "fixture/SearchResponse.proto"
    sageMaker
    (Proxy :: Proxy Search)

responseUpdateCodeRepository :: UpdateCodeRepositoryResponse -> TestTree
responseUpdateCodeRepository =
  res
    "UpdateCodeRepositoryResponse"
    "fixture/UpdateCodeRepositoryResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateCodeRepository)

responseDeleteCodeRepository :: DeleteCodeRepositoryResponse -> TestTree
responseDeleteCodeRepository =
  res
    "DeleteCodeRepositoryResponse"
    "fixture/DeleteCodeRepositoryResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteCodeRepository)

responseDescribeTransformJob :: DescribeTransformJobResponse -> TestTree
responseDescribeTransformJob =
  res
    "DescribeTransformJobResponse"
    "fixture/DescribeTransformJobResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeTransformJob)

responseListCandidatesForAutoMLJob :: ListCandidatesForAutoMLJobResponse -> TestTree
responseListCandidatesForAutoMLJob =
  res
    "ListCandidatesForAutoMLJobResponse"
    "fixture/ListCandidatesForAutoMLJobResponse.proto"
    sageMaker
    (Proxy :: Proxy ListCandidatesForAutoMLJob)

responseDeleteImage :: DeleteImageResponse -> TestTree
responseDeleteImage =
  res
    "DeleteImageResponse"
    "fixture/DeleteImageResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteImage)

responseUpdateImage :: UpdateImageResponse -> TestTree
responseUpdateImage =
  res
    "UpdateImageResponse"
    "fixture/UpdateImageResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateImage)

responseListFlowDefinitions :: ListFlowDefinitionsResponse -> TestTree
responseListFlowDefinitions =
  res
    "ListFlowDefinitionsResponse"
    "fixture/ListFlowDefinitionsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListFlowDefinitions)

responseDescribeEndpoint :: DescribeEndpointResponse -> TestTree
responseDescribeEndpoint =
  res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeEndpoint)

responseListTrainingJobs :: ListTrainingJobsResponse -> TestTree
responseListTrainingJobs =
  res
    "ListTrainingJobsResponse"
    "fixture/ListTrainingJobsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListTrainingJobs)

responseDescribeWorkteam :: DescribeWorkteamResponse -> TestTree
responseDescribeWorkteam =
  res
    "DescribeWorkteamResponse"
    "fixture/DescribeWorkteamResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeWorkteam)

responseListSubscribedWorkteams :: ListSubscribedWorkteamsResponse -> TestTree
responseListSubscribedWorkteams =
  res
    "ListSubscribedWorkteamsResponse"
    "fixture/ListSubscribedWorkteamsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListSubscribedWorkteams)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    sageMaker
    (Proxy :: Proxy DeleteDomain)

responseUpdateDomain :: UpdateDomainResponse -> TestTree
responseUpdateDomain =
  res
    "UpdateDomainResponse"
    "fixture/UpdateDomainResponse.proto"
    sageMaker
    (Proxy :: Proxy UpdateDomain)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    sageMaker
    (Proxy :: Proxy ListDomains)

responseCreateImageVersion :: CreateImageVersionResponse -> TestTree
responseCreateImageVersion =
  res
    "CreateImageVersionResponse"
    "fixture/CreateImageVersionResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateImageVersion)

responseStartMonitoringSchedule :: StartMonitoringScheduleResponse -> TestTree
responseStartMonitoringSchedule =
  res
    "StartMonitoringScheduleResponse"
    "fixture/StartMonitoringScheduleResponse.proto"
    sageMaker
    (Proxy :: Proxy StartMonitoringSchedule)

responseStopAutoMLJob :: StopAutoMLJobResponse -> TestTree
responseStopAutoMLJob =
  res
    "StopAutoMLJobResponse"
    "fixture/StopAutoMLJobResponse.proto"
    sageMaker
    (Proxy :: Proxy StopAutoMLJob)

responseCreateTrialComponent :: CreateTrialComponentResponse -> TestTree
responseCreateTrialComponent =
  res
    "CreateTrialComponentResponse"
    "fixture/CreateTrialComponentResponse.proto"
    sageMaker
    (Proxy :: Proxy CreateTrialComponent)

responseDescribeProcessingJob :: DescribeProcessingJobResponse -> TestTree
responseDescribeProcessingJob =
  res
    "DescribeProcessingJobResponse"
    "fixture/DescribeProcessingJobResponse.proto"
    sageMaker
    (Proxy :: Proxy DescribeProcessingJob)

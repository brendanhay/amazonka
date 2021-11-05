{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SageMaker
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.SageMaker where

import Amazonka.SageMaker
import qualified Data.Proxy as Proxy
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
--         [ requestListProjects $
--             newListProjects
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestGetModelPackageGroupPolicy $
--             newGetModelPackageGroupPolicy
--
--         , requestCreateNotebookInstance $
--             newCreateNotebookInstance
--
--         , requestUpdateModelPackage $
--             newUpdateModelPackage
--
--         , requestDeleteModelPackage $
--             newDeleteModelPackage
--
--         , requestDescribeMonitoringSchedule $
--             newDescribeMonitoringSchedule
--
--         , requestListTrialComponents $
--             newListTrialComponents
--
--         , requestDescribeEndpointConfig $
--             newDescribeEndpointConfig
--
--         , requestCreateModelExplainabilityJobDefinition $
--             newCreateModelExplainabilityJobDefinition
--
--         , requestDescribeApp $
--             newDescribeApp
--
--         , requestListImageVersions $
--             newListImageVersions
--
--         , requestDescribeAutoMLJob $
--             newDescribeAutoMLJob
--
--         , requestStopProcessingJob $
--             newStopProcessingJob
--
--         , requestDeleteAction $
--             newDeleteAction
--
--         , requestUpdateAction $
--             newUpdateAction
--
--         , requestListLabelingJobsForWorkteam $
--             newListLabelingJobsForWorkteam
--
--         , requestCreateTransformJob $
--             newCreateTransformJob
--
--         , requestListArtifacts $
--             newListArtifacts
--
--         , requestDeleteDeviceFleet $
--             newDeleteDeviceFleet
--
--         , requestUpdateDeviceFleet $
--             newUpdateDeviceFleet
--
--         , requestListCompilationJobs $
--             newListCompilationJobs
--
--         , requestDescribePipeline $
--             newDescribePipeline
--
--         , requestDisassociateTrialComponent $
--             newDisassociateTrialComponent
--
--         , requestDescribeModelPackageGroup $
--             newDescribeModelPackageGroup
--
--         , requestCreateEdgePackagingJob $
--             newCreateEdgePackagingJob
--
--         , requestStopHyperParameterTuningJob $
--             newStopHyperParameterTuningJob
--
--         , requestListHumanTaskUis $
--             newListHumanTaskUis
--
--         , requestCreateEndpoint $
--             newCreateEndpoint
--
--         , requestGetSearchSuggestions $
--             newGetSearchSuggestions
--
--         , requestUpdateArtifact $
--             newUpdateArtifact
--
--         , requestDeleteArtifact $
--             newDeleteArtifact
--
--         , requestDescribeTrial $
--             newDescribeTrial
--
--         , requestListActions $
--             newListActions
--
--         , requestCreateArtifact $
--             newCreateArtifact
--
--         , requestCreatePresignedDomainUrl $
--             newCreatePresignedDomainUrl
--
--         , requestListFeatureGroups $
--             newListFeatureGroups
--
--         , requestDescribeCodeRepository $
--             newDescribeCodeRepository
--
--         , requestDescribeContext $
--             newDescribeContext
--
--         , requestDescribeImage $
--             newDescribeImage
--
--         , requestDescribeTrainingJob $
--             newDescribeTrainingJob
--
--         , requestCreateAction $
--             newCreateAction
--
--         , requestDeleteEndpoint $
--             newDeleteEndpoint
--
--         , requestUpdateEndpoint $
--             newUpdateEndpoint
--
--         , requestDescribeDataQualityJobDefinition $
--             newDescribeDataQualityJobDefinition
--
--         , requestCreateHumanTaskUi $
--             newCreateHumanTaskUi
--
--         , requestRegisterDevices $
--             newRegisterDevices
--
--         , requestCreateCompilationJob $
--             newCreateCompilationJob
--
--         , requestDeleteAppImageConfig $
--             newDeleteAppImageConfig
--
--         , requestUpdateAppImageConfig $
--             newUpdateAppImageConfig
--
--         , requestDescribePipelineExecution $
--             newDescribePipelineExecution
--
--         , requestDeleteNotebookInstanceLifecycleConfig $
--             newDeleteNotebookInstanceLifecycleConfig
--
--         , requestUpdateNotebookInstanceLifecycleConfig $
--             newUpdateNotebookInstanceLifecycleConfig
--
--         , requestDeleteWorkforce $
--             newDeleteWorkforce
--
--         , requestUpdateWorkforce $
--             newUpdateWorkforce
--
--         , requestListProcessingJobs $
--             newListProcessingJobs
--
--         , requestCreateLabelingJob $
--             newCreateLabelingJob
--
--         , requestEnableSagemakerServicecatalogPortfolio $
--             newEnableSagemakerServicecatalogPortfolio
--
--         , requestDescribeNotebookInstance $
--             newDescribeNotebookInstance
--
--         , requestCreateMonitoringSchedule $
--             newCreateMonitoringSchedule
--
--         , requestListAppImageConfigs $
--             newListAppImageConfigs
--
--         , requestCreateEndpointConfig $
--             newCreateEndpointConfig
--
--         , requestSendPipelineExecutionStepSuccess $
--             newSendPipelineExecutionStepSuccess
--
--         , requestDescribeModelQualityJobDefinition $
--             newDescribeModelQualityJobDefinition
--
--         , requestDeleteStudioLifecycleConfig $
--             newDeleteStudioLifecycleConfig
--
--         , requestDescribeModelExplainabilityJobDefinition $
--             newDescribeModelExplainabilityJobDefinition
--
--         , requestStopNotebookInstance $
--             newStopNotebookInstance
--
--         , requestUpdateEndpointWeightsAndCapacities $
--             newUpdateEndpointWeightsAndCapacities
--
--         , requestCreateAppImageConfig $
--             newCreateAppImageConfig
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestListExperiments $
--             newListExperiments
--
--         , requestDescribeProject $
--             newDescribeProject
--
--         , requestListAutoMLJobs $
--             newListAutoMLJobs
--
--         , requestListApps $
--             newListApps
--
--         , requestRetryPipelineExecution $
--             newRetryPipelineExecution
--
--         , requestCreateProcessingJob $
--             newCreateProcessingJob
--
--         , requestDeleteMonitoringSchedule $
--             newDeleteMonitoringSchedule
--
--         , requestDescribeModelPackage $
--             newDescribeModelPackage
--
--         , requestDeleteEndpointConfig $
--             newDeleteEndpointConfig
--
--         , requestUpdateMonitoringSchedule $
--             newUpdateMonitoringSchedule
--
--         , requestAddAssociation $
--             newAddAssociation
--
--         , requestStartPipelineExecution $
--             newStartPipelineExecution
--
--         , requestDeleteApp $
--             newDeleteApp
--
--         , requestCreateAlgorithm $
--             newCreateAlgorithm
--
--         , requestListPipelineExecutionSteps $
--             newListPipelineExecutionSteps
--
--         , requestUpdatePipeline $
--             newUpdatePipeline
--
--         , requestStopTransformJob $
--             newStopTransformJob
--
--         , requestDeletePipeline $
--             newDeletePipeline
--
--         , requestDescribeAction $
--             newDescribeAction
--
--         , requestCreateModel $
--             newCreateModel
--
--         , requestListUserProfiles $
--             newListUserProfiles
--
--         , requestCreateDataQualityJobDefinition $
--             newCreateDataQualityJobDefinition
--
--         , requestDeleteModelPackageGroup $
--             newDeleteModelPackageGroup
--
--         , requestDescribeArtifact $
--             newDescribeArtifact
--
--         , requestStopEdgePackagingJob $
--             newStopEdgePackagingJob
--
--         , requestCreateCodeRepository $
--             newCreateCodeRepository
--
--         , requestCreateHyperParameterTuningJob $
--             newCreateHyperParameterTuningJob
--
--         , requestDeleteTrial $
--             newDeleteTrial
--
--         , requestUpdateTrial $
--             newUpdateTrial
--
--         , requestDescribeDeviceFleet $
--             newDescribeDeviceFleet
--
--         , requestListCodeRepositories $
--             newListCodeRepositories
--
--         , requestDescribeCompilationJob $
--             newDescribeCompilationJob
--
--         , requestListPipelines $
--             newListPipelines
--
--         , requestListHyperParameterTuningJobs $
--             newListHyperParameterTuningJobs
--
--         , requestListAlgorithms $
--             newListAlgorithms
--
--         , requestCreateModelPackageGroup $
--             newCreateModelPackageGroup
--
--         , requestGetSagemakerServicecatalogPortfolioStatus $
--             newGetSagemakerServicecatalogPortfolioStatus
--
--         , requestDescribeFeatureGroup $
--             newDescribeFeatureGroup
--
--         , requestRenderUiTemplate $
--             newRenderUiTemplate
--
--         , requestDeleteFlowDefinition $
--             newDeleteFlowDefinition
--
--         , requestSendPipelineExecutionStepFailure $
--             newSendPipelineExecutionStepFailure
--
--         , requestCreateTrial $
--             newCreateTrial
--
--         , requestDeleteModel $
--             newDeleteModel
--
--         , requestListDataQualityJobDefinitions $
--             newListDataQualityJobDefinitions
--
--         , requestListModels $
--             newListModels
--
--         , requestDeleteAlgorithm $
--             newDeleteAlgorithm
--
--         , requestAssociateTrialComponent $
--             newAssociateTrialComponent
--
--         , requestUpdatePipelineExecution $
--             newUpdatePipelineExecution
--
--         , requestDescribeNotebookInstanceLifecycleConfig $
--             newDescribeNotebookInstanceLifecycleConfig
--
--         , requestDescribeWorkforce $
--             newDescribeWorkforce
--
--         , requestDeleteModelExplainabilityJobDefinition $
--             newDeleteModelExplainabilityJobDefinition
--
--         , requestCreateModelPackage $
--             newCreateModelPackage
--
--         , requestDeleteModelQualityJobDefinition $
--             newDeleteModelQualityJobDefinition
--
--         , requestStopMonitoringSchedule $
--             newStopMonitoringSchedule
--
--         , requestListModelExplainabilityJobDefinitions $
--             newListModelExplainabilityJobDefinitions
--
--         , requestDescribeAppImageConfig $
--             newDescribeAppImageConfig
--
--         , requestListNotebookInstances $
--             newListNotebookInstances
--
--         , requestDescribeStudioLifecycleConfig $
--             newDescribeStudioLifecycleConfig
--
--         , requestStopLabelingJob $
--             newStopLabelingJob
--
--         , requestDeleteNotebookInstance $
--             newDeleteNotebookInstance
--
--         , requestUpdateNotebookInstance $
--             newUpdateNotebookInstance
--
--         , requestListModelPackages $
--             newListModelPackages
--
--         , requestCreateModelQualityJobDefinition $
--             newCreateModelQualityJobDefinition
--
--         , requestDeleteImageVersion $
--             newDeleteImageVersion
--
--         , requestDescribeExperiment $
--             newDescribeExperiment
--
--         , requestDeleteTrialComponent $
--             newDeleteTrialComponent
--
--         , requestUpdateTrialComponent $
--             newUpdateTrialComponent
--
--         , requestDescribeLabelingJob $
--             newDescribeLabelingJob
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestListDeviceFleets $
--             newListDeviceFleets
--
--         , requestDescribeUserProfile $
--             newDescribeUserProfile
--
--         , requestListMonitoringExecutions $
--             newListMonitoringExecutions
--
--         , requestDeleteHumanTaskUi $
--             newDeleteHumanTaskUi
--
--         , requestStopTrainingJob $
--             newStopTrainingJob
--
--         , requestCreateFeatureGroup $
--             newCreateFeatureGroup
--
--         , requestDescribeAlgorithm $
--             newDescribeAlgorithm
--
--         , requestUpdateDevices $
--             newUpdateDevices
--
--         , requestDescribeModel $
--             newDescribeModel
--
--         , requestListTransformJobs $
--             newListTransformJobs
--
--         , requestDeleteFeatureGroup $
--             newDeleteFeatureGroup
--
--         , requestListEdgePackagingJobs $
--             newListEdgePackagingJobs
--
--         , requestDescribeHyperParameterTuningJob $
--             newDescribeHyperParameterTuningJob
--
--         , requestListEndpoints $
--             newListEndpoints
--
--         , requestDescribeFlowDefinition $
--             newDescribeFlowDefinition
--
--         , requestCreateDeviceFleet $
--             newCreateDeviceFleet
--
--         , requestCreatePresignedNotebookInstanceUrl $
--             newCreatePresignedNotebookInstanceUrl
--
--         , requestListTrainingJobsForHyperParameterTuningJob $
--             newListTrainingJobsForHyperParameterTuningJob
--
--         , requestDescribeDomain $
--             newDescribeDomain
--
--         , requestDeleteModelBiasJobDefinition $
--             newDeleteModelBiasJobDefinition
--
--         , requestUpdateWorkteam $
--             newUpdateWorkteam
--
--         , requestDeleteWorkteam $
--             newDeleteWorkteam
--
--         , requestListWorkteams $
--             newListWorkteams
--
--         , requestDescribeDevice $
--             newDescribeDevice
--
--         , requestCreateAutoMLJob $
--             newCreateAutoMLJob
--
--         , requestCreateApp $
--             newCreateApp
--
--         , requestCreateExperiment $
--             newCreateExperiment
--
--         , requestListNotebookInstanceLifecycleConfigs $
--             newListNotebookInstanceLifecycleConfigs
--
--         , requestListWorkforces $
--             newListWorkforces
--
--         , requestDescribeSubscribedWorkteam $
--             newDescribeSubscribedWorkteam
--
--         , requestListStudioLifecycleConfigs $
--             newListStudioLifecycleConfigs
--
--         , requestListModelBiasJobDefinitions $
--             newListModelBiasJobDefinitions
--
--         , requestCreateStudioLifecycleConfig $
--             newCreateStudioLifecycleConfig
--
--         , requestDisableSagemakerServicecatalogPortfolio $
--             newDisableSagemakerServicecatalogPortfolio
--
--         , requestCreateWorkteam $
--             newCreateWorkteam
--
--         , requestCreateNotebookInstanceLifecycleConfig $
--             newCreateNotebookInstanceLifecycleConfig
--
--         , requestListMonitoringSchedules $
--             newListMonitoringSchedules
--
--         , requestListLabelingJobs $
--             newListLabelingJobs
--
--         , requestStartNotebookInstance $
--             newStartNotebookInstance
--
--         , requestUpdateExperiment $
--             newUpdateExperiment
--
--         , requestDeleteExperiment $
--             newDeleteExperiment
--
--         , requestStopPipelineExecution $
--             newStopPipelineExecution
--
--         , requestAddTags $
--             newAddTags
--
--         , requestListAssociations $
--             newListAssociations
--
--         , requestCreateWorkforce $
--             newCreateWorkforce
--
--         , requestDescribeTrialComponent $
--             newDescribeTrialComponent
--
--         , requestDescribeImageVersion $
--             newDescribeImageVersion
--
--         , requestCreateModelBiasJobDefinition $
--             newCreateModelBiasJobDefinition
--
--         , requestListEndpointConfigs $
--             newListEndpointConfigs
--
--         , requestDeleteAssociation $
--             newDeleteAssociation
--
--         , requestCreateFlowDefinition $
--             newCreateFlowDefinition
--
--         , requestListModelPackageGroups $
--             newListModelPackageGroups
--
--         , requestListTags $
--             newListTags
--
--         , requestDeregisterDevices $
--             newDeregisterDevices
--
--         , requestDescribeHumanTaskUi $
--             newDescribeHumanTaskUi
--
--         , requestCreateTrainingJob $
--             newCreateTrainingJob
--
--         , requestDeleteModelPackageGroupPolicy $
--             newDeleteModelPackageGroupPolicy
--
--         , requestDeleteUserProfile $
--             newDeleteUserProfile
--
--         , requestUpdateUserProfile $
--             newUpdateUserProfile
--
--         , requestCreateImage $
--             newCreateImage
--
--         , requestPutModelPackageGroupPolicy $
--             newPutModelPackageGroupPolicy
--
--         , requestListPipelineParametersForExecution $
--             newListPipelineParametersForExecution
--
--         , requestCreateContext $
--             newCreateContext
--
--         , requestDescribePipelineDefinitionForExecution $
--             newDescribePipelineDefinitionForExecution
--
--         , requestListTrials $
--             newListTrials
--
--         , requestStopCompilationJob $
--             newStopCompilationJob
--
--         , requestListImages $
--             newListImages
--
--         , requestCreateUserProfile $
--             newCreateUserProfile
--
--         , requestSearch $
--             newSearch
--
--         , requestUpdateCodeRepository $
--             newUpdateCodeRepository
--
--         , requestDeleteCodeRepository $
--             newDeleteCodeRepository
--
--         , requestListContexts $
--             newListContexts
--
--         , requestDescribeTransformJob $
--             newDescribeTransformJob
--
--         , requestDescribeEdgePackagingJob $
--             newDescribeEdgePackagingJob
--
--         , requestCreatePipeline $
--             newCreatePipeline
--
--         , requestListCandidatesForAutoMLJob $
--             newListCandidatesForAutoMLJob
--
--         , requestDeleteImage $
--             newDeleteImage
--
--         , requestUpdateImage $
--             newUpdateImage
--
--         , requestListFlowDefinitions $
--             newListFlowDefinitions
--
--         , requestDeleteContext $
--             newDeleteContext
--
--         , requestUpdateContext $
--             newUpdateContext
--
--         , requestDescribeEndpoint $
--             newDescribeEndpoint
--
--         , requestUpdateTrainingJob $
--             newUpdateTrainingJob
--
--         , requestListTrainingJobs $
--             newListTrainingJobs
--
--         , requestGetDeviceFleetReport $
--             newGetDeviceFleetReport
--
--         , requestDeleteDataQualityJobDefinition $
--             newDeleteDataQualityJobDefinition
--
--         , requestDescribeWorkteam $
--             newDescribeWorkteam
--
--         , requestListSubscribedWorkteams $
--             newListSubscribedWorkteams
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestUpdateDomain $
--             newUpdateDomain
--
--         , requestListDomains $
--             newListDomains
--
--         , requestListModelQualityJobDefinitions $
--             newListModelQualityJobDefinitions
--
--         , requestCreateImageVersion $
--             newCreateImageVersion
--
--         , requestListDevices $
--             newListDevices
--
--         , requestListPipelineExecutions $
--             newListPipelineExecutions
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestDescribeModelBiasJobDefinition $
--             newDescribeModelBiasJobDefinition
--
--         , requestStartMonitoringSchedule $
--             newStartMonitoringSchedule
--
--         , requestStopAutoMLJob $
--             newStopAutoMLJob
--
--         , requestCreateTrialComponent $
--             newCreateTrialComponent
--
--         , requestDescribeProcessingJob $
--             newDescribeProcessingJob
--
--           ]

--     , testGroup "response"
--         [ responseListProjects $
--             newListProjectsResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseGetModelPackageGroupPolicy $
--             newGetModelPackageGroupPolicyResponse
--
--         , responseCreateNotebookInstance $
--             newCreateNotebookInstanceResponse
--
--         , responseUpdateModelPackage $
--             newUpdateModelPackageResponse
--
--         , responseDeleteModelPackage $
--             newDeleteModelPackageResponse
--
--         , responseDescribeMonitoringSchedule $
--             newDescribeMonitoringScheduleResponse
--
--         , responseListTrialComponents $
--             newListTrialComponentsResponse
--
--         , responseDescribeEndpointConfig $
--             newDescribeEndpointConfigResponse
--
--         , responseCreateModelExplainabilityJobDefinition $
--             newCreateModelExplainabilityJobDefinitionResponse
--
--         , responseDescribeApp $
--             newDescribeAppResponse
--
--         , responseListImageVersions $
--             newListImageVersionsResponse
--
--         , responseDescribeAutoMLJob $
--             newDescribeAutoMLJobResponse
--
--         , responseStopProcessingJob $
--             newStopProcessingJobResponse
--
--         , responseDeleteAction $
--             newDeleteActionResponse
--
--         , responseUpdateAction $
--             newUpdateActionResponse
--
--         , responseListLabelingJobsForWorkteam $
--             newListLabelingJobsForWorkteamResponse
--
--         , responseCreateTransformJob $
--             newCreateTransformJobResponse
--
--         , responseListArtifacts $
--             newListArtifactsResponse
--
--         , responseDeleteDeviceFleet $
--             newDeleteDeviceFleetResponse
--
--         , responseUpdateDeviceFleet $
--             newUpdateDeviceFleetResponse
--
--         , responseListCompilationJobs $
--             newListCompilationJobsResponse
--
--         , responseDescribePipeline $
--             newDescribePipelineResponse
--
--         , responseDisassociateTrialComponent $
--             newDisassociateTrialComponentResponse
--
--         , responseDescribeModelPackageGroup $
--             newDescribeModelPackageGroupResponse
--
--         , responseCreateEdgePackagingJob $
--             newCreateEdgePackagingJobResponse
--
--         , responseStopHyperParameterTuningJob $
--             newStopHyperParameterTuningJobResponse
--
--         , responseListHumanTaskUis $
--             newListHumanTaskUisResponse
--
--         , responseCreateEndpoint $
--             newCreateEndpointResponse
--
--         , responseGetSearchSuggestions $
--             newGetSearchSuggestionsResponse
--
--         , responseUpdateArtifact $
--             newUpdateArtifactResponse
--
--         , responseDeleteArtifact $
--             newDeleteArtifactResponse
--
--         , responseDescribeTrial $
--             newDescribeTrialResponse
--
--         , responseListActions $
--             newListActionsResponse
--
--         , responseCreateArtifact $
--             newCreateArtifactResponse
--
--         , responseCreatePresignedDomainUrl $
--             newCreatePresignedDomainUrlResponse
--
--         , responseListFeatureGroups $
--             newListFeatureGroupsResponse
--
--         , responseDescribeCodeRepository $
--             newDescribeCodeRepositoryResponse
--
--         , responseDescribeContext $
--             newDescribeContextResponse
--
--         , responseDescribeImage $
--             newDescribeImageResponse
--
--         , responseDescribeTrainingJob $
--             newDescribeTrainingJobResponse
--
--         , responseCreateAction $
--             newCreateActionResponse
--
--         , responseDeleteEndpoint $
--             newDeleteEndpointResponse
--
--         , responseUpdateEndpoint $
--             newUpdateEndpointResponse
--
--         , responseDescribeDataQualityJobDefinition $
--             newDescribeDataQualityJobDefinitionResponse
--
--         , responseCreateHumanTaskUi $
--             newCreateHumanTaskUiResponse
--
--         , responseRegisterDevices $
--             newRegisterDevicesResponse
--
--         , responseCreateCompilationJob $
--             newCreateCompilationJobResponse
--
--         , responseDeleteAppImageConfig $
--             newDeleteAppImageConfigResponse
--
--         , responseUpdateAppImageConfig $
--             newUpdateAppImageConfigResponse
--
--         , responseDescribePipelineExecution $
--             newDescribePipelineExecutionResponse
--
--         , responseDeleteNotebookInstanceLifecycleConfig $
--             newDeleteNotebookInstanceLifecycleConfigResponse
--
--         , responseUpdateNotebookInstanceLifecycleConfig $
--             newUpdateNotebookInstanceLifecycleConfigResponse
--
--         , responseDeleteWorkforce $
--             newDeleteWorkforceResponse
--
--         , responseUpdateWorkforce $
--             newUpdateWorkforceResponse
--
--         , responseListProcessingJobs $
--             newListProcessingJobsResponse
--
--         , responseCreateLabelingJob $
--             newCreateLabelingJobResponse
--
--         , responseEnableSagemakerServicecatalogPortfolio $
--             newEnableSagemakerServicecatalogPortfolioResponse
--
--         , responseDescribeNotebookInstance $
--             newDescribeNotebookInstanceResponse
--
--         , responseCreateMonitoringSchedule $
--             newCreateMonitoringScheduleResponse
--
--         , responseListAppImageConfigs $
--             newListAppImageConfigsResponse
--
--         , responseCreateEndpointConfig $
--             newCreateEndpointConfigResponse
--
--         , responseSendPipelineExecutionStepSuccess $
--             newSendPipelineExecutionStepSuccessResponse
--
--         , responseDescribeModelQualityJobDefinition $
--             newDescribeModelQualityJobDefinitionResponse
--
--         , responseDeleteStudioLifecycleConfig $
--             newDeleteStudioLifecycleConfigResponse
--
--         , responseDescribeModelExplainabilityJobDefinition $
--             newDescribeModelExplainabilityJobDefinitionResponse
--
--         , responseStopNotebookInstance $
--             newStopNotebookInstanceResponse
--
--         , responseUpdateEndpointWeightsAndCapacities $
--             newUpdateEndpointWeightsAndCapacitiesResponse
--
--         , responseCreateAppImageConfig $
--             newCreateAppImageConfigResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseListExperiments $
--             newListExperimentsResponse
--
--         , responseDescribeProject $
--             newDescribeProjectResponse
--
--         , responseListAutoMLJobs $
--             newListAutoMLJobsResponse
--
--         , responseListApps $
--             newListAppsResponse
--
--         , responseRetryPipelineExecution $
--             newRetryPipelineExecutionResponse
--
--         , responseCreateProcessingJob $
--             newCreateProcessingJobResponse
--
--         , responseDeleteMonitoringSchedule $
--             newDeleteMonitoringScheduleResponse
--
--         , responseDescribeModelPackage $
--             newDescribeModelPackageResponse
--
--         , responseDeleteEndpointConfig $
--             newDeleteEndpointConfigResponse
--
--         , responseUpdateMonitoringSchedule $
--             newUpdateMonitoringScheduleResponse
--
--         , responseAddAssociation $
--             newAddAssociationResponse
--
--         , responseStartPipelineExecution $
--             newStartPipelineExecutionResponse
--
--         , responseDeleteApp $
--             newDeleteAppResponse
--
--         , responseCreateAlgorithm $
--             newCreateAlgorithmResponse
--
--         , responseListPipelineExecutionSteps $
--             newListPipelineExecutionStepsResponse
--
--         , responseUpdatePipeline $
--             newUpdatePipelineResponse
--
--         , responseStopTransformJob $
--             newStopTransformJobResponse
--
--         , responseDeletePipeline $
--             newDeletePipelineResponse
--
--         , responseDescribeAction $
--             newDescribeActionResponse
--
--         , responseCreateModel $
--             newCreateModelResponse
--
--         , responseListUserProfiles $
--             newListUserProfilesResponse
--
--         , responseCreateDataQualityJobDefinition $
--             newCreateDataQualityJobDefinitionResponse
--
--         , responseDeleteModelPackageGroup $
--             newDeleteModelPackageGroupResponse
--
--         , responseDescribeArtifact $
--             newDescribeArtifactResponse
--
--         , responseStopEdgePackagingJob $
--             newStopEdgePackagingJobResponse
--
--         , responseCreateCodeRepository $
--             newCreateCodeRepositoryResponse
--
--         , responseCreateHyperParameterTuningJob $
--             newCreateHyperParameterTuningJobResponse
--
--         , responseDeleteTrial $
--             newDeleteTrialResponse
--
--         , responseUpdateTrial $
--             newUpdateTrialResponse
--
--         , responseDescribeDeviceFleet $
--             newDescribeDeviceFleetResponse
--
--         , responseListCodeRepositories $
--             newListCodeRepositoriesResponse
--
--         , responseDescribeCompilationJob $
--             newDescribeCompilationJobResponse
--
--         , responseListPipelines $
--             newListPipelinesResponse
--
--         , responseListHyperParameterTuningJobs $
--             newListHyperParameterTuningJobsResponse
--
--         , responseListAlgorithms $
--             newListAlgorithmsResponse
--
--         , responseCreateModelPackageGroup $
--             newCreateModelPackageGroupResponse
--
--         , responseGetSagemakerServicecatalogPortfolioStatus $
--             newGetSagemakerServicecatalogPortfolioStatusResponse
--
--         , responseDescribeFeatureGroup $
--             newDescribeFeatureGroupResponse
--
--         , responseRenderUiTemplate $
--             newRenderUiTemplateResponse
--
--         , responseDeleteFlowDefinition $
--             newDeleteFlowDefinitionResponse
--
--         , responseSendPipelineExecutionStepFailure $
--             newSendPipelineExecutionStepFailureResponse
--
--         , responseCreateTrial $
--             newCreateTrialResponse
--
--         , responseDeleteModel $
--             newDeleteModelResponse
--
--         , responseListDataQualityJobDefinitions $
--             newListDataQualityJobDefinitionsResponse
--
--         , responseListModels $
--             newListModelsResponse
--
--         , responseDeleteAlgorithm $
--             newDeleteAlgorithmResponse
--
--         , responseAssociateTrialComponent $
--             newAssociateTrialComponentResponse
--
--         , responseUpdatePipelineExecution $
--             newUpdatePipelineExecutionResponse
--
--         , responseDescribeNotebookInstanceLifecycleConfig $
--             newDescribeNotebookInstanceLifecycleConfigResponse
--
--         , responseDescribeWorkforce $
--             newDescribeWorkforceResponse
--
--         , responseDeleteModelExplainabilityJobDefinition $
--             newDeleteModelExplainabilityJobDefinitionResponse
--
--         , responseCreateModelPackage $
--             newCreateModelPackageResponse
--
--         , responseDeleteModelQualityJobDefinition $
--             newDeleteModelQualityJobDefinitionResponse
--
--         , responseStopMonitoringSchedule $
--             newStopMonitoringScheduleResponse
--
--         , responseListModelExplainabilityJobDefinitions $
--             newListModelExplainabilityJobDefinitionsResponse
--
--         , responseDescribeAppImageConfig $
--             newDescribeAppImageConfigResponse
--
--         , responseListNotebookInstances $
--             newListNotebookInstancesResponse
--
--         , responseDescribeStudioLifecycleConfig $
--             newDescribeStudioLifecycleConfigResponse
--
--         , responseStopLabelingJob $
--             newStopLabelingJobResponse
--
--         , responseDeleteNotebookInstance $
--             newDeleteNotebookInstanceResponse
--
--         , responseUpdateNotebookInstance $
--             newUpdateNotebookInstanceResponse
--
--         , responseListModelPackages $
--             newListModelPackagesResponse
--
--         , responseCreateModelQualityJobDefinition $
--             newCreateModelQualityJobDefinitionResponse
--
--         , responseDeleteImageVersion $
--             newDeleteImageVersionResponse
--
--         , responseDescribeExperiment $
--             newDescribeExperimentResponse
--
--         , responseDeleteTrialComponent $
--             newDeleteTrialComponentResponse
--
--         , responseUpdateTrialComponent $
--             newUpdateTrialComponentResponse
--
--         , responseDescribeLabelingJob $
--             newDescribeLabelingJobResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseListDeviceFleets $
--             newListDeviceFleetsResponse
--
--         , responseDescribeUserProfile $
--             newDescribeUserProfileResponse
--
--         , responseListMonitoringExecutions $
--             newListMonitoringExecutionsResponse
--
--         , responseDeleteHumanTaskUi $
--             newDeleteHumanTaskUiResponse
--
--         , responseStopTrainingJob $
--             newStopTrainingJobResponse
--
--         , responseCreateFeatureGroup $
--             newCreateFeatureGroupResponse
--
--         , responseDescribeAlgorithm $
--             newDescribeAlgorithmResponse
--
--         , responseUpdateDevices $
--             newUpdateDevicesResponse
--
--         , responseDescribeModel $
--             newDescribeModelResponse
--
--         , responseListTransformJobs $
--             newListTransformJobsResponse
--
--         , responseDeleteFeatureGroup $
--             newDeleteFeatureGroupResponse
--
--         , responseListEdgePackagingJobs $
--             newListEdgePackagingJobsResponse
--
--         , responseDescribeHyperParameterTuningJob $
--             newDescribeHyperParameterTuningJobResponse
--
--         , responseListEndpoints $
--             newListEndpointsResponse
--
--         , responseDescribeFlowDefinition $
--             newDescribeFlowDefinitionResponse
--
--         , responseCreateDeviceFleet $
--             newCreateDeviceFleetResponse
--
--         , responseCreatePresignedNotebookInstanceUrl $
--             newCreatePresignedNotebookInstanceUrlResponse
--
--         , responseListTrainingJobsForHyperParameterTuningJob $
--             newListTrainingJobsForHyperParameterTuningJobResponse
--
--         , responseDescribeDomain $
--             newDescribeDomainResponse
--
--         , responseDeleteModelBiasJobDefinition $
--             newDeleteModelBiasJobDefinitionResponse
--
--         , responseUpdateWorkteam $
--             newUpdateWorkteamResponse
--
--         , responseDeleteWorkteam $
--             newDeleteWorkteamResponse
--
--         , responseListWorkteams $
--             newListWorkteamsResponse
--
--         , responseDescribeDevice $
--             newDescribeDeviceResponse
--
--         , responseCreateAutoMLJob $
--             newCreateAutoMLJobResponse
--
--         , responseCreateApp $
--             newCreateAppResponse
--
--         , responseCreateExperiment $
--             newCreateExperimentResponse
--
--         , responseListNotebookInstanceLifecycleConfigs $
--             newListNotebookInstanceLifecycleConfigsResponse
--
--         , responseListWorkforces $
--             newListWorkforcesResponse
--
--         , responseDescribeSubscribedWorkteam $
--             newDescribeSubscribedWorkteamResponse
--
--         , responseListStudioLifecycleConfigs $
--             newListStudioLifecycleConfigsResponse
--
--         , responseListModelBiasJobDefinitions $
--             newListModelBiasJobDefinitionsResponse
--
--         , responseCreateStudioLifecycleConfig $
--             newCreateStudioLifecycleConfigResponse
--
--         , responseDisableSagemakerServicecatalogPortfolio $
--             newDisableSagemakerServicecatalogPortfolioResponse
--
--         , responseCreateWorkteam $
--             newCreateWorkteamResponse
--
--         , responseCreateNotebookInstanceLifecycleConfig $
--             newCreateNotebookInstanceLifecycleConfigResponse
--
--         , responseListMonitoringSchedules $
--             newListMonitoringSchedulesResponse
--
--         , responseListLabelingJobs $
--             newListLabelingJobsResponse
--
--         , responseStartNotebookInstance $
--             newStartNotebookInstanceResponse
--
--         , responseUpdateExperiment $
--             newUpdateExperimentResponse
--
--         , responseDeleteExperiment $
--             newDeleteExperimentResponse
--
--         , responseStopPipelineExecution $
--             newStopPipelineExecutionResponse
--
--         , responseAddTags $
--             newAddTagsResponse
--
--         , responseListAssociations $
--             newListAssociationsResponse
--
--         , responseCreateWorkforce $
--             newCreateWorkforceResponse
--
--         , responseDescribeTrialComponent $
--             newDescribeTrialComponentResponse
--
--         , responseDescribeImageVersion $
--             newDescribeImageVersionResponse
--
--         , responseCreateModelBiasJobDefinition $
--             newCreateModelBiasJobDefinitionResponse
--
--         , responseListEndpointConfigs $
--             newListEndpointConfigsResponse
--
--         , responseDeleteAssociation $
--             newDeleteAssociationResponse
--
--         , responseCreateFlowDefinition $
--             newCreateFlowDefinitionResponse
--
--         , responseListModelPackageGroups $
--             newListModelPackageGroupsResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseDeregisterDevices $
--             newDeregisterDevicesResponse
--
--         , responseDescribeHumanTaskUi $
--             newDescribeHumanTaskUiResponse
--
--         , responseCreateTrainingJob $
--             newCreateTrainingJobResponse
--
--         , responseDeleteModelPackageGroupPolicy $
--             newDeleteModelPackageGroupPolicyResponse
--
--         , responseDeleteUserProfile $
--             newDeleteUserProfileResponse
--
--         , responseUpdateUserProfile $
--             newUpdateUserProfileResponse
--
--         , responseCreateImage $
--             newCreateImageResponse
--
--         , responsePutModelPackageGroupPolicy $
--             newPutModelPackageGroupPolicyResponse
--
--         , responseListPipelineParametersForExecution $
--             newListPipelineParametersForExecutionResponse
--
--         , responseCreateContext $
--             newCreateContextResponse
--
--         , responseDescribePipelineDefinitionForExecution $
--             newDescribePipelineDefinitionForExecutionResponse
--
--         , responseListTrials $
--             newListTrialsResponse
--
--         , responseStopCompilationJob $
--             newStopCompilationJobResponse
--
--         , responseListImages $
--             newListImagesResponse
--
--         , responseCreateUserProfile $
--             newCreateUserProfileResponse
--
--         , responseSearch $
--             newSearchResponse
--
--         , responseUpdateCodeRepository $
--             newUpdateCodeRepositoryResponse
--
--         , responseDeleteCodeRepository $
--             newDeleteCodeRepositoryResponse
--
--         , responseListContexts $
--             newListContextsResponse
--
--         , responseDescribeTransformJob $
--             newDescribeTransformJobResponse
--
--         , responseDescribeEdgePackagingJob $
--             newDescribeEdgePackagingJobResponse
--
--         , responseCreatePipeline $
--             newCreatePipelineResponse
--
--         , responseListCandidatesForAutoMLJob $
--             newListCandidatesForAutoMLJobResponse
--
--         , responseDeleteImage $
--             newDeleteImageResponse
--
--         , responseUpdateImage $
--             newUpdateImageResponse
--
--         , responseListFlowDefinitions $
--             newListFlowDefinitionsResponse
--
--         , responseDeleteContext $
--             newDeleteContextResponse
--
--         , responseUpdateContext $
--             newUpdateContextResponse
--
--         , responseDescribeEndpoint $
--             newDescribeEndpointResponse
--
--         , responseUpdateTrainingJob $
--             newUpdateTrainingJobResponse
--
--         , responseListTrainingJobs $
--             newListTrainingJobsResponse
--
--         , responseGetDeviceFleetReport $
--             newGetDeviceFleetReportResponse
--
--         , responseDeleteDataQualityJobDefinition $
--             newDeleteDataQualityJobDefinitionResponse
--
--         , responseDescribeWorkteam $
--             newDescribeWorkteamResponse
--
--         , responseListSubscribedWorkteams $
--             newListSubscribedWorkteamsResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseUpdateDomain $
--             newUpdateDomainResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--         , responseListModelQualityJobDefinitions $
--             newListModelQualityJobDefinitionsResponse
--
--         , responseCreateImageVersion $
--             newCreateImageVersionResponse
--
--         , responseListDevices $
--             newListDevicesResponse
--
--         , responseListPipelineExecutions $
--             newListPipelineExecutionsResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseDescribeModelBiasJobDefinition $
--             newDescribeModelBiasJobDefinitionResponse
--
--         , responseStartMonitoringSchedule $
--             newStartMonitoringScheduleResponse
--
--         , responseStopAutoMLJob $
--             newStopAutoMLJobResponse
--
--         , responseCreateTrialComponent $
--             newCreateTrialComponentResponse
--
--         , responseDescribeProcessingJob $
--             newDescribeProcessingJobResponse
--
--           ]
--     ]

-- Requests

requestListProjects :: ListProjects -> TestTree
requestListProjects =
  req
    "ListProjects"
    "fixture/ListProjects.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject =
  req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestGetModelPackageGroupPolicy :: GetModelPackageGroupPolicy -> TestTree
requestGetModelPackageGroupPolicy =
  req
    "GetModelPackageGroupPolicy"
    "fixture/GetModelPackageGroupPolicy.yaml"

requestCreateNotebookInstance :: CreateNotebookInstance -> TestTree
requestCreateNotebookInstance =
  req
    "CreateNotebookInstance"
    "fixture/CreateNotebookInstance.yaml"

requestUpdateModelPackage :: UpdateModelPackage -> TestTree
requestUpdateModelPackage =
  req
    "UpdateModelPackage"
    "fixture/UpdateModelPackage.yaml"

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

requestCreateModelExplainabilityJobDefinition :: CreateModelExplainabilityJobDefinition -> TestTree
requestCreateModelExplainabilityJobDefinition =
  req
    "CreateModelExplainabilityJobDefinition"
    "fixture/CreateModelExplainabilityJobDefinition.yaml"

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

requestDeleteAction :: DeleteAction -> TestTree
requestDeleteAction =
  req
    "DeleteAction"
    "fixture/DeleteAction.yaml"

requestUpdateAction :: UpdateAction -> TestTree
requestUpdateAction =
  req
    "UpdateAction"
    "fixture/UpdateAction.yaml"

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

requestListArtifacts :: ListArtifacts -> TestTree
requestListArtifacts =
  req
    "ListArtifacts"
    "fixture/ListArtifacts.yaml"

requestDeleteDeviceFleet :: DeleteDeviceFleet -> TestTree
requestDeleteDeviceFleet =
  req
    "DeleteDeviceFleet"
    "fixture/DeleteDeviceFleet.yaml"

requestUpdateDeviceFleet :: UpdateDeviceFleet -> TestTree
requestUpdateDeviceFleet =
  req
    "UpdateDeviceFleet"
    "fixture/UpdateDeviceFleet.yaml"

requestListCompilationJobs :: ListCompilationJobs -> TestTree
requestListCompilationJobs =
  req
    "ListCompilationJobs"
    "fixture/ListCompilationJobs.yaml"

requestDescribePipeline :: DescribePipeline -> TestTree
requestDescribePipeline =
  req
    "DescribePipeline"
    "fixture/DescribePipeline.yaml"

requestDisassociateTrialComponent :: DisassociateTrialComponent -> TestTree
requestDisassociateTrialComponent =
  req
    "DisassociateTrialComponent"
    "fixture/DisassociateTrialComponent.yaml"

requestDescribeModelPackageGroup :: DescribeModelPackageGroup -> TestTree
requestDescribeModelPackageGroup =
  req
    "DescribeModelPackageGroup"
    "fixture/DescribeModelPackageGroup.yaml"

requestCreateEdgePackagingJob :: CreateEdgePackagingJob -> TestTree
requestCreateEdgePackagingJob =
  req
    "CreateEdgePackagingJob"
    "fixture/CreateEdgePackagingJob.yaml"

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

requestUpdateArtifact :: UpdateArtifact -> TestTree
requestUpdateArtifact =
  req
    "UpdateArtifact"
    "fixture/UpdateArtifact.yaml"

requestDeleteArtifact :: DeleteArtifact -> TestTree
requestDeleteArtifact =
  req
    "DeleteArtifact"
    "fixture/DeleteArtifact.yaml"

requestDescribeTrial :: DescribeTrial -> TestTree
requestDescribeTrial =
  req
    "DescribeTrial"
    "fixture/DescribeTrial.yaml"

requestListActions :: ListActions -> TestTree
requestListActions =
  req
    "ListActions"
    "fixture/ListActions.yaml"

requestCreateArtifact :: CreateArtifact -> TestTree
requestCreateArtifact =
  req
    "CreateArtifact"
    "fixture/CreateArtifact.yaml"

requestCreatePresignedDomainUrl :: CreatePresignedDomainUrl -> TestTree
requestCreatePresignedDomainUrl =
  req
    "CreatePresignedDomainUrl"
    "fixture/CreatePresignedDomainUrl.yaml"

requestListFeatureGroups :: ListFeatureGroups -> TestTree
requestListFeatureGroups =
  req
    "ListFeatureGroups"
    "fixture/ListFeatureGroups.yaml"

requestDescribeCodeRepository :: DescribeCodeRepository -> TestTree
requestDescribeCodeRepository =
  req
    "DescribeCodeRepository"
    "fixture/DescribeCodeRepository.yaml"

requestDescribeContext :: DescribeContext -> TestTree
requestDescribeContext =
  req
    "DescribeContext"
    "fixture/DescribeContext.yaml"

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

requestCreateAction :: CreateAction -> TestTree
requestCreateAction =
  req
    "CreateAction"
    "fixture/CreateAction.yaml"

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

requestDescribeDataQualityJobDefinition :: DescribeDataQualityJobDefinition -> TestTree
requestDescribeDataQualityJobDefinition =
  req
    "DescribeDataQualityJobDefinition"
    "fixture/DescribeDataQualityJobDefinition.yaml"

requestCreateHumanTaskUi :: CreateHumanTaskUi -> TestTree
requestCreateHumanTaskUi =
  req
    "CreateHumanTaskUi"
    "fixture/CreateHumanTaskUi.yaml"

requestRegisterDevices :: RegisterDevices -> TestTree
requestRegisterDevices =
  req
    "RegisterDevices"
    "fixture/RegisterDevices.yaml"

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

requestDescribePipelineExecution :: DescribePipelineExecution -> TestTree
requestDescribePipelineExecution =
  req
    "DescribePipelineExecution"
    "fixture/DescribePipelineExecution.yaml"

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

requestEnableSagemakerServicecatalogPortfolio :: EnableSagemakerServicecatalogPortfolio -> TestTree
requestEnableSagemakerServicecatalogPortfolio =
  req
    "EnableSagemakerServicecatalogPortfolio"
    "fixture/EnableSagemakerServicecatalogPortfolio.yaml"

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

requestSendPipelineExecutionStepSuccess :: SendPipelineExecutionStepSuccess -> TestTree
requestSendPipelineExecutionStepSuccess =
  req
    "SendPipelineExecutionStepSuccess"
    "fixture/SendPipelineExecutionStepSuccess.yaml"

requestDescribeModelQualityJobDefinition :: DescribeModelQualityJobDefinition -> TestTree
requestDescribeModelQualityJobDefinition =
  req
    "DescribeModelQualityJobDefinition"
    "fixture/DescribeModelQualityJobDefinition.yaml"

requestDeleteStudioLifecycleConfig :: DeleteStudioLifecycleConfig -> TestTree
requestDeleteStudioLifecycleConfig =
  req
    "DeleteStudioLifecycleConfig"
    "fixture/DeleteStudioLifecycleConfig.yaml"

requestDescribeModelExplainabilityJobDefinition :: DescribeModelExplainabilityJobDefinition -> TestTree
requestDescribeModelExplainabilityJobDefinition =
  req
    "DescribeModelExplainabilityJobDefinition"
    "fixture/DescribeModelExplainabilityJobDefinition.yaml"

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

requestDescribeProject :: DescribeProject -> TestTree
requestDescribeProject =
  req
    "DescribeProject"
    "fixture/DescribeProject.yaml"

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

requestRetryPipelineExecution :: RetryPipelineExecution -> TestTree
requestRetryPipelineExecution =
  req
    "RetryPipelineExecution"
    "fixture/RetryPipelineExecution.yaml"

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

requestAddAssociation :: AddAssociation -> TestTree
requestAddAssociation =
  req
    "AddAssociation"
    "fixture/AddAssociation.yaml"

requestStartPipelineExecution :: StartPipelineExecution -> TestTree
requestStartPipelineExecution =
  req
    "StartPipelineExecution"
    "fixture/StartPipelineExecution.yaml"

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

requestListPipelineExecutionSteps :: ListPipelineExecutionSteps -> TestTree
requestListPipelineExecutionSteps =
  req
    "ListPipelineExecutionSteps"
    "fixture/ListPipelineExecutionSteps.yaml"

requestUpdatePipeline :: UpdatePipeline -> TestTree
requestUpdatePipeline =
  req
    "UpdatePipeline"
    "fixture/UpdatePipeline.yaml"

requestStopTransformJob :: StopTransformJob -> TestTree
requestStopTransformJob =
  req
    "StopTransformJob"
    "fixture/StopTransformJob.yaml"

requestDeletePipeline :: DeletePipeline -> TestTree
requestDeletePipeline =
  req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

requestDescribeAction :: DescribeAction -> TestTree
requestDescribeAction =
  req
    "DescribeAction"
    "fixture/DescribeAction.yaml"

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

requestCreateDataQualityJobDefinition :: CreateDataQualityJobDefinition -> TestTree
requestCreateDataQualityJobDefinition =
  req
    "CreateDataQualityJobDefinition"
    "fixture/CreateDataQualityJobDefinition.yaml"

requestDeleteModelPackageGroup :: DeleteModelPackageGroup -> TestTree
requestDeleteModelPackageGroup =
  req
    "DeleteModelPackageGroup"
    "fixture/DeleteModelPackageGroup.yaml"

requestDescribeArtifact :: DescribeArtifact -> TestTree
requestDescribeArtifact =
  req
    "DescribeArtifact"
    "fixture/DescribeArtifact.yaml"

requestStopEdgePackagingJob :: StopEdgePackagingJob -> TestTree
requestStopEdgePackagingJob =
  req
    "StopEdgePackagingJob"
    "fixture/StopEdgePackagingJob.yaml"

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

requestDescribeDeviceFleet :: DescribeDeviceFleet -> TestTree
requestDescribeDeviceFleet =
  req
    "DescribeDeviceFleet"
    "fixture/DescribeDeviceFleet.yaml"

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

requestListPipelines :: ListPipelines -> TestTree
requestListPipelines =
  req
    "ListPipelines"
    "fixture/ListPipelines.yaml"

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

requestCreateModelPackageGroup :: CreateModelPackageGroup -> TestTree
requestCreateModelPackageGroup =
  req
    "CreateModelPackageGroup"
    "fixture/CreateModelPackageGroup.yaml"

requestGetSagemakerServicecatalogPortfolioStatus :: GetSagemakerServicecatalogPortfolioStatus -> TestTree
requestGetSagemakerServicecatalogPortfolioStatus =
  req
    "GetSagemakerServicecatalogPortfolioStatus"
    "fixture/GetSagemakerServicecatalogPortfolioStatus.yaml"

requestDescribeFeatureGroup :: DescribeFeatureGroup -> TestTree
requestDescribeFeatureGroup =
  req
    "DescribeFeatureGroup"
    "fixture/DescribeFeatureGroup.yaml"

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

requestSendPipelineExecutionStepFailure :: SendPipelineExecutionStepFailure -> TestTree
requestSendPipelineExecutionStepFailure =
  req
    "SendPipelineExecutionStepFailure"
    "fixture/SendPipelineExecutionStepFailure.yaml"

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

requestListDataQualityJobDefinitions :: ListDataQualityJobDefinitions -> TestTree
requestListDataQualityJobDefinitions =
  req
    "ListDataQualityJobDefinitions"
    "fixture/ListDataQualityJobDefinitions.yaml"

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

requestUpdatePipelineExecution :: UpdatePipelineExecution -> TestTree
requestUpdatePipelineExecution =
  req
    "UpdatePipelineExecution"
    "fixture/UpdatePipelineExecution.yaml"

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

requestDeleteModelExplainabilityJobDefinition :: DeleteModelExplainabilityJobDefinition -> TestTree
requestDeleteModelExplainabilityJobDefinition =
  req
    "DeleteModelExplainabilityJobDefinition"
    "fixture/DeleteModelExplainabilityJobDefinition.yaml"

requestCreateModelPackage :: CreateModelPackage -> TestTree
requestCreateModelPackage =
  req
    "CreateModelPackage"
    "fixture/CreateModelPackage.yaml"

requestDeleteModelQualityJobDefinition :: DeleteModelQualityJobDefinition -> TestTree
requestDeleteModelQualityJobDefinition =
  req
    "DeleteModelQualityJobDefinition"
    "fixture/DeleteModelQualityJobDefinition.yaml"

requestStopMonitoringSchedule :: StopMonitoringSchedule -> TestTree
requestStopMonitoringSchedule =
  req
    "StopMonitoringSchedule"
    "fixture/StopMonitoringSchedule.yaml"

requestListModelExplainabilityJobDefinitions :: ListModelExplainabilityJobDefinitions -> TestTree
requestListModelExplainabilityJobDefinitions =
  req
    "ListModelExplainabilityJobDefinitions"
    "fixture/ListModelExplainabilityJobDefinitions.yaml"

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

requestDescribeStudioLifecycleConfig :: DescribeStudioLifecycleConfig -> TestTree
requestDescribeStudioLifecycleConfig =
  req
    "DescribeStudioLifecycleConfig"
    "fixture/DescribeStudioLifecycleConfig.yaml"

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

requestCreateModelQualityJobDefinition :: CreateModelQualityJobDefinition -> TestTree
requestCreateModelQualityJobDefinition =
  req
    "CreateModelQualityJobDefinition"
    "fixture/CreateModelQualityJobDefinition.yaml"

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

requestListDeviceFleets :: ListDeviceFleets -> TestTree
requestListDeviceFleets =
  req
    "ListDeviceFleets"
    "fixture/ListDeviceFleets.yaml"

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

requestCreateFeatureGroup :: CreateFeatureGroup -> TestTree
requestCreateFeatureGroup =
  req
    "CreateFeatureGroup"
    "fixture/CreateFeatureGroup.yaml"

requestDescribeAlgorithm :: DescribeAlgorithm -> TestTree
requestDescribeAlgorithm =
  req
    "DescribeAlgorithm"
    "fixture/DescribeAlgorithm.yaml"

requestUpdateDevices :: UpdateDevices -> TestTree
requestUpdateDevices =
  req
    "UpdateDevices"
    "fixture/UpdateDevices.yaml"

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

requestDeleteFeatureGroup :: DeleteFeatureGroup -> TestTree
requestDeleteFeatureGroup =
  req
    "DeleteFeatureGroup"
    "fixture/DeleteFeatureGroup.yaml"

requestListEdgePackagingJobs :: ListEdgePackagingJobs -> TestTree
requestListEdgePackagingJobs =
  req
    "ListEdgePackagingJobs"
    "fixture/ListEdgePackagingJobs.yaml"

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

requestCreateDeviceFleet :: CreateDeviceFleet -> TestTree
requestCreateDeviceFleet =
  req
    "CreateDeviceFleet"
    "fixture/CreateDeviceFleet.yaml"

requestCreatePresignedNotebookInstanceUrl :: CreatePresignedNotebookInstanceUrl -> TestTree
requestCreatePresignedNotebookInstanceUrl =
  req
    "CreatePresignedNotebookInstanceUrl"
    "fixture/CreatePresignedNotebookInstanceUrl.yaml"

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

requestDeleteModelBiasJobDefinition :: DeleteModelBiasJobDefinition -> TestTree
requestDeleteModelBiasJobDefinition =
  req
    "DeleteModelBiasJobDefinition"
    "fixture/DeleteModelBiasJobDefinition.yaml"

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

requestDescribeDevice :: DescribeDevice -> TestTree
requestDescribeDevice =
  req
    "DescribeDevice"
    "fixture/DescribeDevice.yaml"

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

requestListStudioLifecycleConfigs :: ListStudioLifecycleConfigs -> TestTree
requestListStudioLifecycleConfigs =
  req
    "ListStudioLifecycleConfigs"
    "fixture/ListStudioLifecycleConfigs.yaml"

requestListModelBiasJobDefinitions :: ListModelBiasJobDefinitions -> TestTree
requestListModelBiasJobDefinitions =
  req
    "ListModelBiasJobDefinitions"
    "fixture/ListModelBiasJobDefinitions.yaml"

requestCreateStudioLifecycleConfig :: CreateStudioLifecycleConfig -> TestTree
requestCreateStudioLifecycleConfig =
  req
    "CreateStudioLifecycleConfig"
    "fixture/CreateStudioLifecycleConfig.yaml"

requestDisableSagemakerServicecatalogPortfolio :: DisableSagemakerServicecatalogPortfolio -> TestTree
requestDisableSagemakerServicecatalogPortfolio =
  req
    "DisableSagemakerServicecatalogPortfolio"
    "fixture/DisableSagemakerServicecatalogPortfolio.yaml"

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

requestStopPipelineExecution :: StopPipelineExecution -> TestTree
requestStopPipelineExecution =
  req
    "StopPipelineExecution"
    "fixture/StopPipelineExecution.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

requestListAssociations :: ListAssociations -> TestTree
requestListAssociations =
  req
    "ListAssociations"
    "fixture/ListAssociations.yaml"

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

requestCreateModelBiasJobDefinition :: CreateModelBiasJobDefinition -> TestTree
requestCreateModelBiasJobDefinition =
  req
    "CreateModelBiasJobDefinition"
    "fixture/CreateModelBiasJobDefinition.yaml"

requestListEndpointConfigs :: ListEndpointConfigs -> TestTree
requestListEndpointConfigs =
  req
    "ListEndpointConfigs"
    "fixture/ListEndpointConfigs.yaml"

requestDeleteAssociation :: DeleteAssociation -> TestTree
requestDeleteAssociation =
  req
    "DeleteAssociation"
    "fixture/DeleteAssociation.yaml"

requestCreateFlowDefinition :: CreateFlowDefinition -> TestTree
requestCreateFlowDefinition =
  req
    "CreateFlowDefinition"
    "fixture/CreateFlowDefinition.yaml"

requestListModelPackageGroups :: ListModelPackageGroups -> TestTree
requestListModelPackageGroups =
  req
    "ListModelPackageGroups"
    "fixture/ListModelPackageGroups.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestDeregisterDevices :: DeregisterDevices -> TestTree
requestDeregisterDevices =
  req
    "DeregisterDevices"
    "fixture/DeregisterDevices.yaml"

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

requestDeleteModelPackageGroupPolicy :: DeleteModelPackageGroupPolicy -> TestTree
requestDeleteModelPackageGroupPolicy =
  req
    "DeleteModelPackageGroupPolicy"
    "fixture/DeleteModelPackageGroupPolicy.yaml"

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

requestPutModelPackageGroupPolicy :: PutModelPackageGroupPolicy -> TestTree
requestPutModelPackageGroupPolicy =
  req
    "PutModelPackageGroupPolicy"
    "fixture/PutModelPackageGroupPolicy.yaml"

requestListPipelineParametersForExecution :: ListPipelineParametersForExecution -> TestTree
requestListPipelineParametersForExecution =
  req
    "ListPipelineParametersForExecution"
    "fixture/ListPipelineParametersForExecution.yaml"

requestCreateContext :: CreateContext -> TestTree
requestCreateContext =
  req
    "CreateContext"
    "fixture/CreateContext.yaml"

requestDescribePipelineDefinitionForExecution :: DescribePipelineDefinitionForExecution -> TestTree
requestDescribePipelineDefinitionForExecution =
  req
    "DescribePipelineDefinitionForExecution"
    "fixture/DescribePipelineDefinitionForExecution.yaml"

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

requestListContexts :: ListContexts -> TestTree
requestListContexts =
  req
    "ListContexts"
    "fixture/ListContexts.yaml"

requestDescribeTransformJob :: DescribeTransformJob -> TestTree
requestDescribeTransformJob =
  req
    "DescribeTransformJob"
    "fixture/DescribeTransformJob.yaml"

requestDescribeEdgePackagingJob :: DescribeEdgePackagingJob -> TestTree
requestDescribeEdgePackagingJob =
  req
    "DescribeEdgePackagingJob"
    "fixture/DescribeEdgePackagingJob.yaml"

requestCreatePipeline :: CreatePipeline -> TestTree
requestCreatePipeline =
  req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

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

requestDeleteContext :: DeleteContext -> TestTree
requestDeleteContext =
  req
    "DeleteContext"
    "fixture/DeleteContext.yaml"

requestUpdateContext :: UpdateContext -> TestTree
requestUpdateContext =
  req
    "UpdateContext"
    "fixture/UpdateContext.yaml"

requestDescribeEndpoint :: DescribeEndpoint -> TestTree
requestDescribeEndpoint =
  req
    "DescribeEndpoint"
    "fixture/DescribeEndpoint.yaml"

requestUpdateTrainingJob :: UpdateTrainingJob -> TestTree
requestUpdateTrainingJob =
  req
    "UpdateTrainingJob"
    "fixture/UpdateTrainingJob.yaml"

requestListTrainingJobs :: ListTrainingJobs -> TestTree
requestListTrainingJobs =
  req
    "ListTrainingJobs"
    "fixture/ListTrainingJobs.yaml"

requestGetDeviceFleetReport :: GetDeviceFleetReport -> TestTree
requestGetDeviceFleetReport =
  req
    "GetDeviceFleetReport"
    "fixture/GetDeviceFleetReport.yaml"

requestDeleteDataQualityJobDefinition :: DeleteDataQualityJobDefinition -> TestTree
requestDeleteDataQualityJobDefinition =
  req
    "DeleteDataQualityJobDefinition"
    "fixture/DeleteDataQualityJobDefinition.yaml"

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

requestListModelQualityJobDefinitions :: ListModelQualityJobDefinitions -> TestTree
requestListModelQualityJobDefinitions =
  req
    "ListModelQualityJobDefinitions"
    "fixture/ListModelQualityJobDefinitions.yaml"

requestCreateImageVersion :: CreateImageVersion -> TestTree
requestCreateImageVersion =
  req
    "CreateImageVersion"
    "fixture/CreateImageVersion.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices =
  req
    "ListDevices"
    "fixture/ListDevices.yaml"

requestListPipelineExecutions :: ListPipelineExecutions -> TestTree
requestListPipelineExecutions =
  req
    "ListPipelineExecutions"
    "fixture/ListPipelineExecutions.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestDescribeModelBiasJobDefinition :: DescribeModelBiasJobDefinition -> TestTree
requestDescribeModelBiasJobDefinition =
  req
    "DescribeModelBiasJobDefinition"
    "fixture/DescribeModelBiasJobDefinition.yaml"

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

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProjects)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProject)

responseGetModelPackageGroupPolicy :: GetModelPackageGroupPolicyResponse -> TestTree
responseGetModelPackageGroupPolicy =
  res
    "GetModelPackageGroupPolicyResponse"
    "fixture/GetModelPackageGroupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetModelPackageGroupPolicy)

responseCreateNotebookInstance :: CreateNotebookInstanceResponse -> TestTree
responseCreateNotebookInstance =
  res
    "CreateNotebookInstanceResponse"
    "fixture/CreateNotebookInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNotebookInstance)

responseUpdateModelPackage :: UpdateModelPackageResponse -> TestTree
responseUpdateModelPackage =
  res
    "UpdateModelPackageResponse"
    "fixture/UpdateModelPackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateModelPackage)

responseDeleteModelPackage :: DeleteModelPackageResponse -> TestTree
responseDeleteModelPackage =
  res
    "DeleteModelPackageResponse"
    "fixture/DeleteModelPackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModelPackage)

responseDescribeMonitoringSchedule :: DescribeMonitoringScheduleResponse -> TestTree
responseDescribeMonitoringSchedule =
  res
    "DescribeMonitoringScheduleResponse"
    "fixture/DescribeMonitoringScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMonitoringSchedule)

responseListTrialComponents :: ListTrialComponentsResponse -> TestTree
responseListTrialComponents =
  res
    "ListTrialComponentsResponse"
    "fixture/ListTrialComponentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrialComponents)

responseDescribeEndpointConfig :: DescribeEndpointConfigResponse -> TestTree
responseDescribeEndpointConfig =
  res
    "DescribeEndpointConfigResponse"
    "fixture/DescribeEndpointConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpointConfig)

responseCreateModelExplainabilityJobDefinition :: CreateModelExplainabilityJobDefinitionResponse -> TestTree
responseCreateModelExplainabilityJobDefinition =
  res
    "CreateModelExplainabilityJobDefinitionResponse"
    "fixture/CreateModelExplainabilityJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModelExplainabilityJobDefinition)

responseDescribeApp :: DescribeAppResponse -> TestTree
responseDescribeApp =
  res
    "DescribeAppResponse"
    "fixture/DescribeAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeApp)

responseListImageVersions :: ListImageVersionsResponse -> TestTree
responseListImageVersions =
  res
    "ListImageVersionsResponse"
    "fixture/ListImageVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImageVersions)

responseDescribeAutoMLJob :: DescribeAutoMLJobResponse -> TestTree
responseDescribeAutoMLJob =
  res
    "DescribeAutoMLJobResponse"
    "fixture/DescribeAutoMLJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAutoMLJob)

responseStopProcessingJob :: StopProcessingJobResponse -> TestTree
responseStopProcessingJob =
  res
    "StopProcessingJobResponse"
    "fixture/StopProcessingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopProcessingJob)

responseDeleteAction :: DeleteActionResponse -> TestTree
responseDeleteAction =
  res
    "DeleteActionResponse"
    "fixture/DeleteActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAction)

responseUpdateAction :: UpdateActionResponse -> TestTree
responseUpdateAction =
  res
    "UpdateActionResponse"
    "fixture/UpdateActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAction)

responseListLabelingJobsForWorkteam :: ListLabelingJobsForWorkteamResponse -> TestTree
responseListLabelingJobsForWorkteam =
  res
    "ListLabelingJobsForWorkteamResponse"
    "fixture/ListLabelingJobsForWorkteamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLabelingJobsForWorkteam)

responseCreateTransformJob :: CreateTransformJobResponse -> TestTree
responseCreateTransformJob =
  res
    "CreateTransformJobResponse"
    "fixture/CreateTransformJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransformJob)

responseListArtifacts :: ListArtifactsResponse -> TestTree
responseListArtifacts =
  res
    "ListArtifactsResponse"
    "fixture/ListArtifactsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListArtifacts)

responseDeleteDeviceFleet :: DeleteDeviceFleetResponse -> TestTree
responseDeleteDeviceFleet =
  res
    "DeleteDeviceFleetResponse"
    "fixture/DeleteDeviceFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDeviceFleet)

responseUpdateDeviceFleet :: UpdateDeviceFleetResponse -> TestTree
responseUpdateDeviceFleet =
  res
    "UpdateDeviceFleetResponse"
    "fixture/UpdateDeviceFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDeviceFleet)

responseListCompilationJobs :: ListCompilationJobsResponse -> TestTree
responseListCompilationJobs =
  res
    "ListCompilationJobsResponse"
    "fixture/ListCompilationJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCompilationJobs)

responseDescribePipeline :: DescribePipelineResponse -> TestTree
responseDescribePipeline =
  res
    "DescribePipelineResponse"
    "fixture/DescribePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePipeline)

responseDisassociateTrialComponent :: DisassociateTrialComponentResponse -> TestTree
responseDisassociateTrialComponent =
  res
    "DisassociateTrialComponentResponse"
    "fixture/DisassociateTrialComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateTrialComponent)

responseDescribeModelPackageGroup :: DescribeModelPackageGroupResponse -> TestTree
responseDescribeModelPackageGroup =
  res
    "DescribeModelPackageGroupResponse"
    "fixture/DescribeModelPackageGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeModelPackageGroup)

responseCreateEdgePackagingJob :: CreateEdgePackagingJobResponse -> TestTree
responseCreateEdgePackagingJob =
  res
    "CreateEdgePackagingJobResponse"
    "fixture/CreateEdgePackagingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEdgePackagingJob)

responseStopHyperParameterTuningJob :: StopHyperParameterTuningJobResponse -> TestTree
responseStopHyperParameterTuningJob =
  res
    "StopHyperParameterTuningJobResponse"
    "fixture/StopHyperParameterTuningJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopHyperParameterTuningJob)

responseListHumanTaskUis :: ListHumanTaskUisResponse -> TestTree
responseListHumanTaskUis =
  res
    "ListHumanTaskUisResponse"
    "fixture/ListHumanTaskUisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHumanTaskUis)

responseCreateEndpoint :: CreateEndpointResponse -> TestTree
responseCreateEndpoint =
  res
    "CreateEndpointResponse"
    "fixture/CreateEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEndpoint)

responseGetSearchSuggestions :: GetSearchSuggestionsResponse -> TestTree
responseGetSearchSuggestions =
  res
    "GetSearchSuggestionsResponse"
    "fixture/GetSearchSuggestionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSearchSuggestions)

responseUpdateArtifact :: UpdateArtifactResponse -> TestTree
responseUpdateArtifact =
  res
    "UpdateArtifactResponse"
    "fixture/UpdateArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateArtifact)

responseDeleteArtifact :: DeleteArtifactResponse -> TestTree
responseDeleteArtifact =
  res
    "DeleteArtifactResponse"
    "fixture/DeleteArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteArtifact)

responseDescribeTrial :: DescribeTrialResponse -> TestTree
responseDescribeTrial =
  res
    "DescribeTrialResponse"
    "fixture/DescribeTrialResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrial)

responseListActions :: ListActionsResponse -> TestTree
responseListActions =
  res
    "ListActionsResponse"
    "fixture/ListActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListActions)

responseCreateArtifact :: CreateArtifactResponse -> TestTree
responseCreateArtifact =
  res
    "CreateArtifactResponse"
    "fixture/CreateArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateArtifact)

responseCreatePresignedDomainUrl :: CreatePresignedDomainUrlResponse -> TestTree
responseCreatePresignedDomainUrl =
  res
    "CreatePresignedDomainUrlResponse"
    "fixture/CreatePresignedDomainUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePresignedDomainUrl)

responseListFeatureGroups :: ListFeatureGroupsResponse -> TestTree
responseListFeatureGroups =
  res
    "ListFeatureGroupsResponse"
    "fixture/ListFeatureGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFeatureGroups)

responseDescribeCodeRepository :: DescribeCodeRepositoryResponse -> TestTree
responseDescribeCodeRepository =
  res
    "DescribeCodeRepositoryResponse"
    "fixture/DescribeCodeRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCodeRepository)

responseDescribeContext :: DescribeContextResponse -> TestTree
responseDescribeContext =
  res
    "DescribeContextResponse"
    "fixture/DescribeContextResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeContext)

responseDescribeImage :: DescribeImageResponse -> TestTree
responseDescribeImage =
  res
    "DescribeImageResponse"
    "fixture/DescribeImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImage)

responseDescribeTrainingJob :: DescribeTrainingJobResponse -> TestTree
responseDescribeTrainingJob =
  res
    "DescribeTrainingJobResponse"
    "fixture/DescribeTrainingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrainingJob)

responseCreateAction :: CreateActionResponse -> TestTree
responseCreateAction =
  res
    "CreateActionResponse"
    "fixture/CreateActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAction)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEndpoint)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint =
  res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEndpoint)

responseDescribeDataQualityJobDefinition :: DescribeDataQualityJobDefinitionResponse -> TestTree
responseDescribeDataQualityJobDefinition =
  res
    "DescribeDataQualityJobDefinitionResponse"
    "fixture/DescribeDataQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataQualityJobDefinition)

responseCreateHumanTaskUi :: CreateHumanTaskUiResponse -> TestTree
responseCreateHumanTaskUi =
  res
    "CreateHumanTaskUiResponse"
    "fixture/CreateHumanTaskUiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHumanTaskUi)

responseRegisterDevices :: RegisterDevicesResponse -> TestTree
responseRegisterDevices =
  res
    "RegisterDevicesResponse"
    "fixture/RegisterDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterDevices)

responseCreateCompilationJob :: CreateCompilationJobResponse -> TestTree
responseCreateCompilationJob =
  res
    "CreateCompilationJobResponse"
    "fixture/CreateCompilationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCompilationJob)

responseDeleteAppImageConfig :: DeleteAppImageConfigResponse -> TestTree
responseDeleteAppImageConfig =
  res
    "DeleteAppImageConfigResponse"
    "fixture/DeleteAppImageConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppImageConfig)

responseUpdateAppImageConfig :: UpdateAppImageConfigResponse -> TestTree
responseUpdateAppImageConfig =
  res
    "UpdateAppImageConfigResponse"
    "fixture/UpdateAppImageConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAppImageConfig)

responseDescribePipelineExecution :: DescribePipelineExecutionResponse -> TestTree
responseDescribePipelineExecution =
  res
    "DescribePipelineExecutionResponse"
    "fixture/DescribePipelineExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePipelineExecution)

responseDeleteNotebookInstanceLifecycleConfig :: DeleteNotebookInstanceLifecycleConfigResponse -> TestTree
responseDeleteNotebookInstanceLifecycleConfig =
  res
    "DeleteNotebookInstanceLifecycleConfigResponse"
    "fixture/DeleteNotebookInstanceLifecycleConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNotebookInstanceLifecycleConfig)

responseUpdateNotebookInstanceLifecycleConfig :: UpdateNotebookInstanceLifecycleConfigResponse -> TestTree
responseUpdateNotebookInstanceLifecycleConfig =
  res
    "UpdateNotebookInstanceLifecycleConfigResponse"
    "fixture/UpdateNotebookInstanceLifecycleConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNotebookInstanceLifecycleConfig)

responseDeleteWorkforce :: DeleteWorkforceResponse -> TestTree
responseDeleteWorkforce =
  res
    "DeleteWorkforceResponse"
    "fixture/DeleteWorkforceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkforce)

responseUpdateWorkforce :: UpdateWorkforceResponse -> TestTree
responseUpdateWorkforce =
  res
    "UpdateWorkforceResponse"
    "fixture/UpdateWorkforceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkforce)

responseListProcessingJobs :: ListProcessingJobsResponse -> TestTree
responseListProcessingJobs =
  res
    "ListProcessingJobsResponse"
    "fixture/ListProcessingJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProcessingJobs)

responseCreateLabelingJob :: CreateLabelingJobResponse -> TestTree
responseCreateLabelingJob =
  res
    "CreateLabelingJobResponse"
    "fixture/CreateLabelingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLabelingJob)

responseEnableSagemakerServicecatalogPortfolio :: EnableSagemakerServicecatalogPortfolioResponse -> TestTree
responseEnableSagemakerServicecatalogPortfolio =
  res
    "EnableSagemakerServicecatalogPortfolioResponse"
    "fixture/EnableSagemakerServicecatalogPortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableSagemakerServicecatalogPortfolio)

responseDescribeNotebookInstance :: DescribeNotebookInstanceResponse -> TestTree
responseDescribeNotebookInstance =
  res
    "DescribeNotebookInstanceResponse"
    "fixture/DescribeNotebookInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNotebookInstance)

responseCreateMonitoringSchedule :: CreateMonitoringScheduleResponse -> TestTree
responseCreateMonitoringSchedule =
  res
    "CreateMonitoringScheduleResponse"
    "fixture/CreateMonitoringScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMonitoringSchedule)

responseListAppImageConfigs :: ListAppImageConfigsResponse -> TestTree
responseListAppImageConfigs =
  res
    "ListAppImageConfigsResponse"
    "fixture/ListAppImageConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppImageConfigs)

responseCreateEndpointConfig :: CreateEndpointConfigResponse -> TestTree
responseCreateEndpointConfig =
  res
    "CreateEndpointConfigResponse"
    "fixture/CreateEndpointConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEndpointConfig)

responseSendPipelineExecutionStepSuccess :: SendPipelineExecutionStepSuccessResponse -> TestTree
responseSendPipelineExecutionStepSuccess =
  res
    "SendPipelineExecutionStepSuccessResponse"
    "fixture/SendPipelineExecutionStepSuccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendPipelineExecutionStepSuccess)

responseDescribeModelQualityJobDefinition :: DescribeModelQualityJobDefinitionResponse -> TestTree
responseDescribeModelQualityJobDefinition =
  res
    "DescribeModelQualityJobDefinitionResponse"
    "fixture/DescribeModelQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeModelQualityJobDefinition)

responseDeleteStudioLifecycleConfig :: DeleteStudioLifecycleConfigResponse -> TestTree
responseDeleteStudioLifecycleConfig =
  res
    "DeleteStudioLifecycleConfigResponse"
    "fixture/DeleteStudioLifecycleConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStudioLifecycleConfig)

responseDescribeModelExplainabilityJobDefinition :: DescribeModelExplainabilityJobDefinitionResponse -> TestTree
responseDescribeModelExplainabilityJobDefinition =
  res
    "DescribeModelExplainabilityJobDefinitionResponse"
    "fixture/DescribeModelExplainabilityJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeModelExplainabilityJobDefinition)

responseStopNotebookInstance :: StopNotebookInstanceResponse -> TestTree
responseStopNotebookInstance =
  res
    "StopNotebookInstanceResponse"
    "fixture/StopNotebookInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopNotebookInstance)

responseUpdateEndpointWeightsAndCapacities :: UpdateEndpointWeightsAndCapacitiesResponse -> TestTree
responseUpdateEndpointWeightsAndCapacities =
  res
    "UpdateEndpointWeightsAndCapacitiesResponse"
    "fixture/UpdateEndpointWeightsAndCapacitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEndpointWeightsAndCapacities)

responseCreateAppImageConfig :: CreateAppImageConfigResponse -> TestTree
responseCreateAppImageConfig =
  res
    "CreateAppImageConfigResponse"
    "fixture/CreateAppImageConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAppImageConfig)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTags)

responseListExperiments :: ListExperimentsResponse -> TestTree
responseListExperiments =
  res
    "ListExperimentsResponse"
    "fixture/ListExperimentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExperiments)

responseDescribeProject :: DescribeProjectResponse -> TestTree
responseDescribeProject =
  res
    "DescribeProjectResponse"
    "fixture/DescribeProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProject)

responseListAutoMLJobs :: ListAutoMLJobsResponse -> TestTree
responseListAutoMLJobs =
  res
    "ListAutoMLJobsResponse"
    "fixture/ListAutoMLJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAutoMLJobs)

responseListApps :: ListAppsResponse -> TestTree
responseListApps =
  res
    "ListAppsResponse"
    "fixture/ListAppsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApps)

responseRetryPipelineExecution :: RetryPipelineExecutionResponse -> TestTree
responseRetryPipelineExecution =
  res
    "RetryPipelineExecutionResponse"
    "fixture/RetryPipelineExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RetryPipelineExecution)

responseCreateProcessingJob :: CreateProcessingJobResponse -> TestTree
responseCreateProcessingJob =
  res
    "CreateProcessingJobResponse"
    "fixture/CreateProcessingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProcessingJob)

responseDeleteMonitoringSchedule :: DeleteMonitoringScheduleResponse -> TestTree
responseDeleteMonitoringSchedule =
  res
    "DeleteMonitoringScheduleResponse"
    "fixture/DeleteMonitoringScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMonitoringSchedule)

responseDescribeModelPackage :: DescribeModelPackageResponse -> TestTree
responseDescribeModelPackage =
  res
    "DescribeModelPackageResponse"
    "fixture/DescribeModelPackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeModelPackage)

responseDeleteEndpointConfig :: DeleteEndpointConfigResponse -> TestTree
responseDeleteEndpointConfig =
  res
    "DeleteEndpointConfigResponse"
    "fixture/DeleteEndpointConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEndpointConfig)

responseUpdateMonitoringSchedule :: UpdateMonitoringScheduleResponse -> TestTree
responseUpdateMonitoringSchedule =
  res
    "UpdateMonitoringScheduleResponse"
    "fixture/UpdateMonitoringScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMonitoringSchedule)

responseAddAssociation :: AddAssociationResponse -> TestTree
responseAddAssociation =
  res
    "AddAssociationResponse"
    "fixture/AddAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddAssociation)

responseStartPipelineExecution :: StartPipelineExecutionResponse -> TestTree
responseStartPipelineExecution =
  res
    "StartPipelineExecutionResponse"
    "fixture/StartPipelineExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartPipelineExecution)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApp)

responseCreateAlgorithm :: CreateAlgorithmResponse -> TestTree
responseCreateAlgorithm =
  res
    "CreateAlgorithmResponse"
    "fixture/CreateAlgorithmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAlgorithm)

responseListPipelineExecutionSteps :: ListPipelineExecutionStepsResponse -> TestTree
responseListPipelineExecutionSteps =
  res
    "ListPipelineExecutionStepsResponse"
    "fixture/ListPipelineExecutionStepsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPipelineExecutionSteps)

responseUpdatePipeline :: UpdatePipelineResponse -> TestTree
responseUpdatePipeline =
  res
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePipeline)

responseStopTransformJob :: StopTransformJobResponse -> TestTree
responseStopTransformJob =
  res
    "StopTransformJobResponse"
    "fixture/StopTransformJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopTransformJob)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline =
  res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePipeline)

responseDescribeAction :: DescribeActionResponse -> TestTree
responseDescribeAction =
  res
    "DescribeActionResponse"
    "fixture/DescribeActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAction)

responseCreateModel :: CreateModelResponse -> TestTree
responseCreateModel =
  res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModel)

responseListUserProfiles :: ListUserProfilesResponse -> TestTree
responseListUserProfiles =
  res
    "ListUserProfilesResponse"
    "fixture/ListUserProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUserProfiles)

responseCreateDataQualityJobDefinition :: CreateDataQualityJobDefinitionResponse -> TestTree
responseCreateDataQualityJobDefinition =
  res
    "CreateDataQualityJobDefinitionResponse"
    "fixture/CreateDataQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataQualityJobDefinition)

responseDeleteModelPackageGroup :: DeleteModelPackageGroupResponse -> TestTree
responseDeleteModelPackageGroup =
  res
    "DeleteModelPackageGroupResponse"
    "fixture/DeleteModelPackageGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModelPackageGroup)

responseDescribeArtifact :: DescribeArtifactResponse -> TestTree
responseDescribeArtifact =
  res
    "DescribeArtifactResponse"
    "fixture/DescribeArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeArtifact)

responseStopEdgePackagingJob :: StopEdgePackagingJobResponse -> TestTree
responseStopEdgePackagingJob =
  res
    "StopEdgePackagingJobResponse"
    "fixture/StopEdgePackagingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopEdgePackagingJob)

responseCreateCodeRepository :: CreateCodeRepositoryResponse -> TestTree
responseCreateCodeRepository =
  res
    "CreateCodeRepositoryResponse"
    "fixture/CreateCodeRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCodeRepository)

responseCreateHyperParameterTuningJob :: CreateHyperParameterTuningJobResponse -> TestTree
responseCreateHyperParameterTuningJob =
  res
    "CreateHyperParameterTuningJobResponse"
    "fixture/CreateHyperParameterTuningJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHyperParameterTuningJob)

responseDeleteTrial :: DeleteTrialResponse -> TestTree
responseDeleteTrial =
  res
    "DeleteTrialResponse"
    "fixture/DeleteTrialResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrial)

responseUpdateTrial :: UpdateTrialResponse -> TestTree
responseUpdateTrial =
  res
    "UpdateTrialResponse"
    "fixture/UpdateTrialResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTrial)

responseDescribeDeviceFleet :: DescribeDeviceFleetResponse -> TestTree
responseDescribeDeviceFleet =
  res
    "DescribeDeviceFleetResponse"
    "fixture/DescribeDeviceFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDeviceFleet)

responseListCodeRepositories :: ListCodeRepositoriesResponse -> TestTree
responseListCodeRepositories =
  res
    "ListCodeRepositoriesResponse"
    "fixture/ListCodeRepositoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCodeRepositories)

responseDescribeCompilationJob :: DescribeCompilationJobResponse -> TestTree
responseDescribeCompilationJob =
  res
    "DescribeCompilationJobResponse"
    "fixture/DescribeCompilationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCompilationJob)

responseListPipelines :: ListPipelinesResponse -> TestTree
responseListPipelines =
  res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPipelines)

responseListHyperParameterTuningJobs :: ListHyperParameterTuningJobsResponse -> TestTree
responseListHyperParameterTuningJobs =
  res
    "ListHyperParameterTuningJobsResponse"
    "fixture/ListHyperParameterTuningJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHyperParameterTuningJobs)

responseListAlgorithms :: ListAlgorithmsResponse -> TestTree
responseListAlgorithms =
  res
    "ListAlgorithmsResponse"
    "fixture/ListAlgorithmsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAlgorithms)

responseCreateModelPackageGroup :: CreateModelPackageGroupResponse -> TestTree
responseCreateModelPackageGroup =
  res
    "CreateModelPackageGroupResponse"
    "fixture/CreateModelPackageGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModelPackageGroup)

responseGetSagemakerServicecatalogPortfolioStatus :: GetSagemakerServicecatalogPortfolioStatusResponse -> TestTree
responseGetSagemakerServicecatalogPortfolioStatus =
  res
    "GetSagemakerServicecatalogPortfolioStatusResponse"
    "fixture/GetSagemakerServicecatalogPortfolioStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSagemakerServicecatalogPortfolioStatus)

responseDescribeFeatureGroup :: DescribeFeatureGroupResponse -> TestTree
responseDescribeFeatureGroup =
  res
    "DescribeFeatureGroupResponse"
    "fixture/DescribeFeatureGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFeatureGroup)

responseRenderUiTemplate :: RenderUiTemplateResponse -> TestTree
responseRenderUiTemplate =
  res
    "RenderUiTemplateResponse"
    "fixture/RenderUiTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RenderUiTemplate)

responseDeleteFlowDefinition :: DeleteFlowDefinitionResponse -> TestTree
responseDeleteFlowDefinition =
  res
    "DeleteFlowDefinitionResponse"
    "fixture/DeleteFlowDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFlowDefinition)

responseSendPipelineExecutionStepFailure :: SendPipelineExecutionStepFailureResponse -> TestTree
responseSendPipelineExecutionStepFailure =
  res
    "SendPipelineExecutionStepFailureResponse"
    "fixture/SendPipelineExecutionStepFailureResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendPipelineExecutionStepFailure)

responseCreateTrial :: CreateTrialResponse -> TestTree
responseCreateTrial =
  res
    "CreateTrialResponse"
    "fixture/CreateTrialResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrial)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel =
  res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModel)

responseListDataQualityJobDefinitions :: ListDataQualityJobDefinitionsResponse -> TestTree
responseListDataQualityJobDefinitions =
  res
    "ListDataQualityJobDefinitionsResponse"
    "fixture/ListDataQualityJobDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataQualityJobDefinitions)

responseListModels :: ListModelsResponse -> TestTree
responseListModels =
  res
    "ListModelsResponse"
    "fixture/ListModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModels)

responseDeleteAlgorithm :: DeleteAlgorithmResponse -> TestTree
responseDeleteAlgorithm =
  res
    "DeleteAlgorithmResponse"
    "fixture/DeleteAlgorithmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlgorithm)

responseAssociateTrialComponent :: AssociateTrialComponentResponse -> TestTree
responseAssociateTrialComponent =
  res
    "AssociateTrialComponentResponse"
    "fixture/AssociateTrialComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateTrialComponent)

responseUpdatePipelineExecution :: UpdatePipelineExecutionResponse -> TestTree
responseUpdatePipelineExecution =
  res
    "UpdatePipelineExecutionResponse"
    "fixture/UpdatePipelineExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePipelineExecution)

responseDescribeNotebookInstanceLifecycleConfig :: DescribeNotebookInstanceLifecycleConfigResponse -> TestTree
responseDescribeNotebookInstanceLifecycleConfig =
  res
    "DescribeNotebookInstanceLifecycleConfigResponse"
    "fixture/DescribeNotebookInstanceLifecycleConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNotebookInstanceLifecycleConfig)

responseDescribeWorkforce :: DescribeWorkforceResponse -> TestTree
responseDescribeWorkforce =
  res
    "DescribeWorkforceResponse"
    "fixture/DescribeWorkforceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkforce)

responseDeleteModelExplainabilityJobDefinition :: DeleteModelExplainabilityJobDefinitionResponse -> TestTree
responseDeleteModelExplainabilityJobDefinition =
  res
    "DeleteModelExplainabilityJobDefinitionResponse"
    "fixture/DeleteModelExplainabilityJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModelExplainabilityJobDefinition)

responseCreateModelPackage :: CreateModelPackageResponse -> TestTree
responseCreateModelPackage =
  res
    "CreateModelPackageResponse"
    "fixture/CreateModelPackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModelPackage)

responseDeleteModelQualityJobDefinition :: DeleteModelQualityJobDefinitionResponse -> TestTree
responseDeleteModelQualityJobDefinition =
  res
    "DeleteModelQualityJobDefinitionResponse"
    "fixture/DeleteModelQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModelQualityJobDefinition)

responseStopMonitoringSchedule :: StopMonitoringScheduleResponse -> TestTree
responseStopMonitoringSchedule =
  res
    "StopMonitoringScheduleResponse"
    "fixture/StopMonitoringScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopMonitoringSchedule)

responseListModelExplainabilityJobDefinitions :: ListModelExplainabilityJobDefinitionsResponse -> TestTree
responseListModelExplainabilityJobDefinitions =
  res
    "ListModelExplainabilityJobDefinitionsResponse"
    "fixture/ListModelExplainabilityJobDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModelExplainabilityJobDefinitions)

responseDescribeAppImageConfig :: DescribeAppImageConfigResponse -> TestTree
responseDescribeAppImageConfig =
  res
    "DescribeAppImageConfigResponse"
    "fixture/DescribeAppImageConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAppImageConfig)

responseListNotebookInstances :: ListNotebookInstancesResponse -> TestTree
responseListNotebookInstances =
  res
    "ListNotebookInstancesResponse"
    "fixture/ListNotebookInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNotebookInstances)

responseDescribeStudioLifecycleConfig :: DescribeStudioLifecycleConfigResponse -> TestTree
responseDescribeStudioLifecycleConfig =
  res
    "DescribeStudioLifecycleConfigResponse"
    "fixture/DescribeStudioLifecycleConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStudioLifecycleConfig)

responseStopLabelingJob :: StopLabelingJobResponse -> TestTree
responseStopLabelingJob =
  res
    "StopLabelingJobResponse"
    "fixture/StopLabelingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopLabelingJob)

responseDeleteNotebookInstance :: DeleteNotebookInstanceResponse -> TestTree
responseDeleteNotebookInstance =
  res
    "DeleteNotebookInstanceResponse"
    "fixture/DeleteNotebookInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNotebookInstance)

responseUpdateNotebookInstance :: UpdateNotebookInstanceResponse -> TestTree
responseUpdateNotebookInstance =
  res
    "UpdateNotebookInstanceResponse"
    "fixture/UpdateNotebookInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNotebookInstance)

responseListModelPackages :: ListModelPackagesResponse -> TestTree
responseListModelPackages =
  res
    "ListModelPackagesResponse"
    "fixture/ListModelPackagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModelPackages)

responseCreateModelQualityJobDefinition :: CreateModelQualityJobDefinitionResponse -> TestTree
responseCreateModelQualityJobDefinition =
  res
    "CreateModelQualityJobDefinitionResponse"
    "fixture/CreateModelQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModelQualityJobDefinition)

responseDeleteImageVersion :: DeleteImageVersionResponse -> TestTree
responseDeleteImageVersion =
  res
    "DeleteImageVersionResponse"
    "fixture/DeleteImageVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteImageVersion)

responseDescribeExperiment :: DescribeExperimentResponse -> TestTree
responseDescribeExperiment =
  res
    "DescribeExperimentResponse"
    "fixture/DescribeExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExperiment)

responseDeleteTrialComponent :: DeleteTrialComponentResponse -> TestTree
responseDeleteTrialComponent =
  res
    "DeleteTrialComponentResponse"
    "fixture/DeleteTrialComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrialComponent)

responseUpdateTrialComponent :: UpdateTrialComponentResponse -> TestTree
responseUpdateTrialComponent =
  res
    "UpdateTrialComponentResponse"
    "fixture/UpdateTrialComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTrialComponent)

responseDescribeLabelingJob :: DescribeLabelingJobResponse -> TestTree
responseDescribeLabelingJob =
  res
    "DescribeLabelingJobResponse"
    "fixture/DescribeLabelingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLabelingJob)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomain)

responseListDeviceFleets :: ListDeviceFleetsResponse -> TestTree
responseListDeviceFleets =
  res
    "ListDeviceFleetsResponse"
    "fixture/ListDeviceFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeviceFleets)

responseDescribeUserProfile :: DescribeUserProfileResponse -> TestTree
responseDescribeUserProfile =
  res
    "DescribeUserProfileResponse"
    "fixture/DescribeUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUserProfile)

responseListMonitoringExecutions :: ListMonitoringExecutionsResponse -> TestTree
responseListMonitoringExecutions =
  res
    "ListMonitoringExecutionsResponse"
    "fixture/ListMonitoringExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMonitoringExecutions)

responseDeleteHumanTaskUi :: DeleteHumanTaskUiResponse -> TestTree
responseDeleteHumanTaskUi =
  res
    "DeleteHumanTaskUiResponse"
    "fixture/DeleteHumanTaskUiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHumanTaskUi)

responseStopTrainingJob :: StopTrainingJobResponse -> TestTree
responseStopTrainingJob =
  res
    "StopTrainingJobResponse"
    "fixture/StopTrainingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopTrainingJob)

responseCreateFeatureGroup :: CreateFeatureGroupResponse -> TestTree
responseCreateFeatureGroup =
  res
    "CreateFeatureGroupResponse"
    "fixture/CreateFeatureGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFeatureGroup)

responseDescribeAlgorithm :: DescribeAlgorithmResponse -> TestTree
responseDescribeAlgorithm =
  res
    "DescribeAlgorithmResponse"
    "fixture/DescribeAlgorithmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAlgorithm)

responseUpdateDevices :: UpdateDevicesResponse -> TestTree
responseUpdateDevices =
  res
    "UpdateDevicesResponse"
    "fixture/UpdateDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDevices)

responseDescribeModel :: DescribeModelResponse -> TestTree
responseDescribeModel =
  res
    "DescribeModelResponse"
    "fixture/DescribeModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeModel)

responseListTransformJobs :: ListTransformJobsResponse -> TestTree
responseListTransformJobs =
  res
    "ListTransformJobsResponse"
    "fixture/ListTransformJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTransformJobs)

responseDeleteFeatureGroup :: DeleteFeatureGroupResponse -> TestTree
responseDeleteFeatureGroup =
  res
    "DeleteFeatureGroupResponse"
    "fixture/DeleteFeatureGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFeatureGroup)

responseListEdgePackagingJobs :: ListEdgePackagingJobsResponse -> TestTree
responseListEdgePackagingJobs =
  res
    "ListEdgePackagingJobsResponse"
    "fixture/ListEdgePackagingJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEdgePackagingJobs)

responseDescribeHyperParameterTuningJob :: DescribeHyperParameterTuningJobResponse -> TestTree
responseDescribeHyperParameterTuningJob =
  res
    "DescribeHyperParameterTuningJobResponse"
    "fixture/DescribeHyperParameterTuningJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHyperParameterTuningJob)

responseListEndpoints :: ListEndpointsResponse -> TestTree
responseListEndpoints =
  res
    "ListEndpointsResponse"
    "fixture/ListEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEndpoints)

responseDescribeFlowDefinition :: DescribeFlowDefinitionResponse -> TestTree
responseDescribeFlowDefinition =
  res
    "DescribeFlowDefinitionResponse"
    "fixture/DescribeFlowDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFlowDefinition)

responseCreateDeviceFleet :: CreateDeviceFleetResponse -> TestTree
responseCreateDeviceFleet =
  res
    "CreateDeviceFleetResponse"
    "fixture/CreateDeviceFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeviceFleet)

responseCreatePresignedNotebookInstanceUrl :: CreatePresignedNotebookInstanceUrlResponse -> TestTree
responseCreatePresignedNotebookInstanceUrl =
  res
    "CreatePresignedNotebookInstanceUrlResponse"
    "fixture/CreatePresignedNotebookInstanceUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePresignedNotebookInstanceUrl)

responseListTrainingJobsForHyperParameterTuningJob :: ListTrainingJobsForHyperParameterTuningJobResponse -> TestTree
responseListTrainingJobsForHyperParameterTuningJob =
  res
    "ListTrainingJobsForHyperParameterTuningJobResponse"
    "fixture/ListTrainingJobsForHyperParameterTuningJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrainingJobsForHyperParameterTuningJob)

responseDescribeDomain :: DescribeDomainResponse -> TestTree
responseDescribeDomain =
  res
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomain)

responseDeleteModelBiasJobDefinition :: DeleteModelBiasJobDefinitionResponse -> TestTree
responseDeleteModelBiasJobDefinition =
  res
    "DeleteModelBiasJobDefinitionResponse"
    "fixture/DeleteModelBiasJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModelBiasJobDefinition)

responseUpdateWorkteam :: UpdateWorkteamResponse -> TestTree
responseUpdateWorkteam =
  res
    "UpdateWorkteamResponse"
    "fixture/UpdateWorkteamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkteam)

responseDeleteWorkteam :: DeleteWorkteamResponse -> TestTree
responseDeleteWorkteam =
  res
    "DeleteWorkteamResponse"
    "fixture/DeleteWorkteamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkteam)

responseListWorkteams :: ListWorkteamsResponse -> TestTree
responseListWorkteams =
  res
    "ListWorkteamsResponse"
    "fixture/ListWorkteamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkteams)

responseDescribeDevice :: DescribeDeviceResponse -> TestTree
responseDescribeDevice =
  res
    "DescribeDeviceResponse"
    "fixture/DescribeDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDevice)

responseCreateAutoMLJob :: CreateAutoMLJobResponse -> TestTree
responseCreateAutoMLJob =
  res
    "CreateAutoMLJobResponse"
    "fixture/CreateAutoMLJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAutoMLJob)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApp)

responseCreateExperiment :: CreateExperimentResponse -> TestTree
responseCreateExperiment =
  res
    "CreateExperimentResponse"
    "fixture/CreateExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateExperiment)

responseListNotebookInstanceLifecycleConfigs :: ListNotebookInstanceLifecycleConfigsResponse -> TestTree
responseListNotebookInstanceLifecycleConfigs =
  res
    "ListNotebookInstanceLifecycleConfigsResponse"
    "fixture/ListNotebookInstanceLifecycleConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNotebookInstanceLifecycleConfigs)

responseListWorkforces :: ListWorkforcesResponse -> TestTree
responseListWorkforces =
  res
    "ListWorkforcesResponse"
    "fixture/ListWorkforcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkforces)

responseDescribeSubscribedWorkteam :: DescribeSubscribedWorkteamResponse -> TestTree
responseDescribeSubscribedWorkteam =
  res
    "DescribeSubscribedWorkteamResponse"
    "fixture/DescribeSubscribedWorkteamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSubscribedWorkteam)

responseListStudioLifecycleConfigs :: ListStudioLifecycleConfigsResponse -> TestTree
responseListStudioLifecycleConfigs =
  res
    "ListStudioLifecycleConfigsResponse"
    "fixture/ListStudioLifecycleConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStudioLifecycleConfigs)

responseListModelBiasJobDefinitions :: ListModelBiasJobDefinitionsResponse -> TestTree
responseListModelBiasJobDefinitions =
  res
    "ListModelBiasJobDefinitionsResponse"
    "fixture/ListModelBiasJobDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModelBiasJobDefinitions)

responseCreateStudioLifecycleConfig :: CreateStudioLifecycleConfigResponse -> TestTree
responseCreateStudioLifecycleConfig =
  res
    "CreateStudioLifecycleConfigResponse"
    "fixture/CreateStudioLifecycleConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStudioLifecycleConfig)

responseDisableSagemakerServicecatalogPortfolio :: DisableSagemakerServicecatalogPortfolioResponse -> TestTree
responseDisableSagemakerServicecatalogPortfolio =
  res
    "DisableSagemakerServicecatalogPortfolioResponse"
    "fixture/DisableSagemakerServicecatalogPortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableSagemakerServicecatalogPortfolio)

responseCreateWorkteam :: CreateWorkteamResponse -> TestTree
responseCreateWorkteam =
  res
    "CreateWorkteamResponse"
    "fixture/CreateWorkteamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkteam)

responseCreateNotebookInstanceLifecycleConfig :: CreateNotebookInstanceLifecycleConfigResponse -> TestTree
responseCreateNotebookInstanceLifecycleConfig =
  res
    "CreateNotebookInstanceLifecycleConfigResponse"
    "fixture/CreateNotebookInstanceLifecycleConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNotebookInstanceLifecycleConfig)

responseListMonitoringSchedules :: ListMonitoringSchedulesResponse -> TestTree
responseListMonitoringSchedules =
  res
    "ListMonitoringSchedulesResponse"
    "fixture/ListMonitoringSchedulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMonitoringSchedules)

responseListLabelingJobs :: ListLabelingJobsResponse -> TestTree
responseListLabelingJobs =
  res
    "ListLabelingJobsResponse"
    "fixture/ListLabelingJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLabelingJobs)

responseStartNotebookInstance :: StartNotebookInstanceResponse -> TestTree
responseStartNotebookInstance =
  res
    "StartNotebookInstanceResponse"
    "fixture/StartNotebookInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartNotebookInstance)

responseUpdateExperiment :: UpdateExperimentResponse -> TestTree
responseUpdateExperiment =
  res
    "UpdateExperimentResponse"
    "fixture/UpdateExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateExperiment)

responseDeleteExperiment :: DeleteExperimentResponse -> TestTree
responseDeleteExperiment =
  res
    "DeleteExperimentResponse"
    "fixture/DeleteExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteExperiment)

responseStopPipelineExecution :: StopPipelineExecutionResponse -> TestTree
responseStopPipelineExecution =
  res
    "StopPipelineExecutionResponse"
    "fixture/StopPipelineExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopPipelineExecution)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTags)

responseListAssociations :: ListAssociationsResponse -> TestTree
responseListAssociations =
  res
    "ListAssociationsResponse"
    "fixture/ListAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssociations)

responseCreateWorkforce :: CreateWorkforceResponse -> TestTree
responseCreateWorkforce =
  res
    "CreateWorkforceResponse"
    "fixture/CreateWorkforceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkforce)

responseDescribeTrialComponent :: DescribeTrialComponentResponse -> TestTree
responseDescribeTrialComponent =
  res
    "DescribeTrialComponentResponse"
    "fixture/DescribeTrialComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrialComponent)

responseDescribeImageVersion :: DescribeImageVersionResponse -> TestTree
responseDescribeImageVersion =
  res
    "DescribeImageVersionResponse"
    "fixture/DescribeImageVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImageVersion)

responseCreateModelBiasJobDefinition :: CreateModelBiasJobDefinitionResponse -> TestTree
responseCreateModelBiasJobDefinition =
  res
    "CreateModelBiasJobDefinitionResponse"
    "fixture/CreateModelBiasJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModelBiasJobDefinition)

responseListEndpointConfigs :: ListEndpointConfigsResponse -> TestTree
responseListEndpointConfigs =
  res
    "ListEndpointConfigsResponse"
    "fixture/ListEndpointConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEndpointConfigs)

responseDeleteAssociation :: DeleteAssociationResponse -> TestTree
responseDeleteAssociation =
  res
    "DeleteAssociationResponse"
    "fixture/DeleteAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAssociation)

responseCreateFlowDefinition :: CreateFlowDefinitionResponse -> TestTree
responseCreateFlowDefinition =
  res
    "CreateFlowDefinitionResponse"
    "fixture/CreateFlowDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFlowDefinition)

responseListModelPackageGroups :: ListModelPackageGroupsResponse -> TestTree
responseListModelPackageGroups =
  res
    "ListModelPackageGroupsResponse"
    "fixture/ListModelPackageGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModelPackageGroups)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTags)

responseDeregisterDevices :: DeregisterDevicesResponse -> TestTree
responseDeregisterDevices =
  res
    "DeregisterDevicesResponse"
    "fixture/DeregisterDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterDevices)

responseDescribeHumanTaskUi :: DescribeHumanTaskUiResponse -> TestTree
responseDescribeHumanTaskUi =
  res
    "DescribeHumanTaskUiResponse"
    "fixture/DescribeHumanTaskUiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHumanTaskUi)

responseCreateTrainingJob :: CreateTrainingJobResponse -> TestTree
responseCreateTrainingJob =
  res
    "CreateTrainingJobResponse"
    "fixture/CreateTrainingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrainingJob)

responseDeleteModelPackageGroupPolicy :: DeleteModelPackageGroupPolicyResponse -> TestTree
responseDeleteModelPackageGroupPolicy =
  res
    "DeleteModelPackageGroupPolicyResponse"
    "fixture/DeleteModelPackageGroupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModelPackageGroupPolicy)

responseDeleteUserProfile :: DeleteUserProfileResponse -> TestTree
responseDeleteUserProfile =
  res
    "DeleteUserProfileResponse"
    "fixture/DeleteUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserProfile)

responseUpdateUserProfile :: UpdateUserProfileResponse -> TestTree
responseUpdateUserProfile =
  res
    "UpdateUserProfileResponse"
    "fixture/UpdateUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserProfile)

responseCreateImage :: CreateImageResponse -> TestTree
responseCreateImage =
  res
    "CreateImageResponse"
    "fixture/CreateImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateImage)

responsePutModelPackageGroupPolicy :: PutModelPackageGroupPolicyResponse -> TestTree
responsePutModelPackageGroupPolicy =
  res
    "PutModelPackageGroupPolicyResponse"
    "fixture/PutModelPackageGroupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutModelPackageGroupPolicy)

responseListPipelineParametersForExecution :: ListPipelineParametersForExecutionResponse -> TestTree
responseListPipelineParametersForExecution =
  res
    "ListPipelineParametersForExecutionResponse"
    "fixture/ListPipelineParametersForExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPipelineParametersForExecution)

responseCreateContext :: CreateContextResponse -> TestTree
responseCreateContext =
  res
    "CreateContextResponse"
    "fixture/CreateContextResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateContext)

responseDescribePipelineDefinitionForExecution :: DescribePipelineDefinitionForExecutionResponse -> TestTree
responseDescribePipelineDefinitionForExecution =
  res
    "DescribePipelineDefinitionForExecutionResponse"
    "fixture/DescribePipelineDefinitionForExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePipelineDefinitionForExecution)

responseListTrials :: ListTrialsResponse -> TestTree
responseListTrials =
  res
    "ListTrialsResponse"
    "fixture/ListTrialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrials)

responseStopCompilationJob :: StopCompilationJobResponse -> TestTree
responseStopCompilationJob =
  res
    "StopCompilationJobResponse"
    "fixture/StopCompilationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopCompilationJob)

responseListImages :: ListImagesResponse -> TestTree
responseListImages =
  res
    "ListImagesResponse"
    "fixture/ListImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImages)

responseCreateUserProfile :: CreateUserProfileResponse -> TestTree
responseCreateUserProfile =
  res
    "CreateUserProfileResponse"
    "fixture/CreateUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUserProfile)

responseSearch :: SearchResponse -> TestTree
responseSearch =
  res
    "SearchResponse"
    "fixture/SearchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Search)

responseUpdateCodeRepository :: UpdateCodeRepositoryResponse -> TestTree
responseUpdateCodeRepository =
  res
    "UpdateCodeRepositoryResponse"
    "fixture/UpdateCodeRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCodeRepository)

responseDeleteCodeRepository :: DeleteCodeRepositoryResponse -> TestTree
responseDeleteCodeRepository =
  res
    "DeleteCodeRepositoryResponse"
    "fixture/DeleteCodeRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCodeRepository)

responseListContexts :: ListContextsResponse -> TestTree
responseListContexts =
  res
    "ListContextsResponse"
    "fixture/ListContextsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListContexts)

responseDescribeTransformJob :: DescribeTransformJobResponse -> TestTree
responseDescribeTransformJob =
  res
    "DescribeTransformJobResponse"
    "fixture/DescribeTransformJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTransformJob)

responseDescribeEdgePackagingJob :: DescribeEdgePackagingJobResponse -> TestTree
responseDescribeEdgePackagingJob =
  res
    "DescribeEdgePackagingJobResponse"
    "fixture/DescribeEdgePackagingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEdgePackagingJob)

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline =
  res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePipeline)

responseListCandidatesForAutoMLJob :: ListCandidatesForAutoMLJobResponse -> TestTree
responseListCandidatesForAutoMLJob =
  res
    "ListCandidatesForAutoMLJobResponse"
    "fixture/ListCandidatesForAutoMLJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCandidatesForAutoMLJob)

responseDeleteImage :: DeleteImageResponse -> TestTree
responseDeleteImage =
  res
    "DeleteImageResponse"
    "fixture/DeleteImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteImage)

responseUpdateImage :: UpdateImageResponse -> TestTree
responseUpdateImage =
  res
    "UpdateImageResponse"
    "fixture/UpdateImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateImage)

responseListFlowDefinitions :: ListFlowDefinitionsResponse -> TestTree
responseListFlowDefinitions =
  res
    "ListFlowDefinitionsResponse"
    "fixture/ListFlowDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFlowDefinitions)

responseDeleteContext :: DeleteContextResponse -> TestTree
responseDeleteContext =
  res
    "DeleteContextResponse"
    "fixture/DeleteContextResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteContext)

responseUpdateContext :: UpdateContextResponse -> TestTree
responseUpdateContext =
  res
    "UpdateContextResponse"
    "fixture/UpdateContextResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContext)

responseDescribeEndpoint :: DescribeEndpointResponse -> TestTree
responseDescribeEndpoint =
  res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpoint)

responseUpdateTrainingJob :: UpdateTrainingJobResponse -> TestTree
responseUpdateTrainingJob =
  res
    "UpdateTrainingJobResponse"
    "fixture/UpdateTrainingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTrainingJob)

responseListTrainingJobs :: ListTrainingJobsResponse -> TestTree
responseListTrainingJobs =
  res
    "ListTrainingJobsResponse"
    "fixture/ListTrainingJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrainingJobs)

responseGetDeviceFleetReport :: GetDeviceFleetReportResponse -> TestTree
responseGetDeviceFleetReport =
  res
    "GetDeviceFleetReportResponse"
    "fixture/GetDeviceFleetReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeviceFleetReport)

responseDeleteDataQualityJobDefinition :: DeleteDataQualityJobDefinitionResponse -> TestTree
responseDeleteDataQualityJobDefinition =
  res
    "DeleteDataQualityJobDefinitionResponse"
    "fixture/DeleteDataQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataQualityJobDefinition)

responseDescribeWorkteam :: DescribeWorkteamResponse -> TestTree
responseDescribeWorkteam =
  res
    "DescribeWorkteamResponse"
    "fixture/DescribeWorkteamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkteam)

responseListSubscribedWorkteams :: ListSubscribedWorkteamsResponse -> TestTree
responseListSubscribedWorkteams =
  res
    "ListSubscribedWorkteamsResponse"
    "fixture/ListSubscribedWorkteamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSubscribedWorkteams)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomain)

responseUpdateDomain :: UpdateDomainResponse -> TestTree
responseUpdateDomain =
  res
    "UpdateDomainResponse"
    "fixture/UpdateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomain)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomains)

responseListModelQualityJobDefinitions :: ListModelQualityJobDefinitionsResponse -> TestTree
responseListModelQualityJobDefinitions =
  res
    "ListModelQualityJobDefinitionsResponse"
    "fixture/ListModelQualityJobDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModelQualityJobDefinitions)

responseCreateImageVersion :: CreateImageVersionResponse -> TestTree
responseCreateImageVersion =
  res
    "CreateImageVersionResponse"
    "fixture/CreateImageVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateImageVersion)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevices)

responseListPipelineExecutions :: ListPipelineExecutionsResponse -> TestTree
responseListPipelineExecutions =
  res
    "ListPipelineExecutionsResponse"
    "fixture/ListPipelineExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPipelineExecutions)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProject)

responseDescribeModelBiasJobDefinition :: DescribeModelBiasJobDefinitionResponse -> TestTree
responseDescribeModelBiasJobDefinition =
  res
    "DescribeModelBiasJobDefinitionResponse"
    "fixture/DescribeModelBiasJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeModelBiasJobDefinition)

responseStartMonitoringSchedule :: StartMonitoringScheduleResponse -> TestTree
responseStartMonitoringSchedule =
  res
    "StartMonitoringScheduleResponse"
    "fixture/StartMonitoringScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMonitoringSchedule)

responseStopAutoMLJob :: StopAutoMLJobResponse -> TestTree
responseStopAutoMLJob =
  res
    "StopAutoMLJobResponse"
    "fixture/StopAutoMLJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopAutoMLJob)

responseCreateTrialComponent :: CreateTrialComponentResponse -> TestTree
responseCreateTrialComponent =
  res
    "CreateTrialComponentResponse"
    "fixture/CreateTrialComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrialComponent)

responseDescribeProcessingJob :: DescribeProcessingJobResponse -> TestTree
responseDescribeProcessingJob =
  res
    "DescribeProcessingJobResponse"
    "fixture/DescribeProcessingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProcessingJob)

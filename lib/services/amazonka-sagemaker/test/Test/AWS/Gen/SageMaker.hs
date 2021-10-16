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
--         [ requestCreateEdgePackagingJob $
--             newCreateEdgePackagingJob
--
--         , requestDescribeUserProfile $
--             newDescribeUserProfile
--
--         , requestListHumanTaskUis $
--             newListHumanTaskUis
--
--         , requestDeleteHumanTaskUi $
--             newDeleteHumanTaskUi
--
--         , requestUpdateAction $
--             newUpdateAction
--
--         , requestCreateEndpoint $
--             newCreateEndpoint
--
--         , requestDeleteArtifact $
--             newDeleteArtifact
--
--         , requestCreateTransformJob $
--             newCreateTransformJob
--
--         , requestDeleteAction $
--             newDeleteAction
--
--         , requestDescribePipeline $
--             newDescribePipeline
--
--         , requestUpdateArtifact $
--             newUpdateArtifact
--
--         , requestStopTrainingJob $
--             newStopTrainingJob
--
--         , requestDisassociateTrialComponent $
--             newDisassociateTrialComponent
--
--         , requestGetSearchSuggestions $
--             newGetSearchSuggestions
--
--         , requestDeleteModelPackage $
--             newDeleteModelPackage
--
--         , requestCreateModelQualityJobDefinition $
--             newCreateModelQualityJobDefinition
--
--         , requestListModelPackages $
--             newListModelPackages
--
--         , requestListProjects $
--             newListProjects
--
--         , requestCreateModelExplainabilityJobDefinition $
--             newCreateModelExplainabilityJobDefinition
--
--         , requestDescribeEndpointConfig $
--             newDescribeEndpointConfig
--
--         , requestDescribeMonitoringSchedule $
--             newDescribeMonitoringSchedule
--
--         , requestDescribeLabelingJob $
--             newDescribeLabelingJob
--
--         , requestGetModelPackageGroupPolicy $
--             newGetModelPackageGroupPolicy
--
--         , requestUpdateModelPackage $
--             newUpdateModelPackage
--
--         , requestCreateNotebookInstance $
--             newCreateNotebookInstance
--
--         , requestStopMonitoringSchedule $
--             newStopMonitoringSchedule
--
--         , requestCreateModelPackage $
--             newCreateModelPackage
--
--         , requestStopAutoMLJob $
--             newStopAutoMLJob
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestDescribeAppImageConfig $
--             newDescribeAppImageConfig
--
--         , requestListSubscribedWorkteams $
--             newListSubscribedWorkteams
--
--         , requestListDevices $
--             newListDevices
--
--         , requestUpdateNotebookInstance $
--             newUpdateNotebookInstance
--
--         , requestDeleteModelExplainabilityJobDefinition $
--             newDeleteModelExplainabilityJobDefinition
--
--         , requestDescribeProcessingJob $
--             newDescribeProcessingJob
--
--         , requestStartMonitoringSchedule $
--             newStartMonitoringSchedule
--
--         , requestDeleteNotebookInstance $
--             newDeleteNotebookInstance
--
--         , requestListDomains $
--             newListDomains
--
--         , requestCreateTrial $
--             newCreateTrial
--
--         , requestDescribeTransformJob $
--             newDescribeTransformJob
--
--         , requestDescribeEdgePackagingJob $
--             newDescribeEdgePackagingJob
--
--         , requestDeleteModel $
--             newDeleteModel
--
--         , requestListTrainingJobs $
--             newListTrainingJobs
--
--         , requestListDataQualityJobDefinitions $
--             newListDataQualityJobDefinitions
--
--         , requestGetSagemakerServicecatalogPortfolioStatus $
--             newGetSagemakerServicecatalogPortfolioStatus
--
--         , requestSearch $
--             newSearch
--
--         , requestStopCompilationJob $
--             newStopCompilationJob
--
--         , requestListImages $
--             newListImages
--
--         , requestListCandidatesForAutoMLJob $
--             newListCandidatesForAutoMLJob
--
--         , requestDeleteAlgorithm $
--             newDeleteAlgorithm
--
--         , requestGetDeviceFleetReport $
--             newGetDeviceFleetReport
--
--         , requestDeleteDataQualityJobDefinition $
--             newDeleteDataQualityJobDefinition
--
--         , requestCreateModelPackageGroup $
--             newCreateModelPackageGroup
--
--         , requestCreatePipeline $
--             newCreatePipeline
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
--         , requestDescribeEndpoint $
--             newDescribeEndpoint
--
--         , requestDeregisterDevices $
--             newDeregisterDevices
--
--         , requestCreateCodeRepository $
--             newCreateCodeRepository
--
--         , requestCreateTrainingJob $
--             newCreateTrainingJob
--
--         , requestDeletePipeline $
--             newDeletePipeline
--
--         , requestCreateContext $
--             newCreateContext
--
--         , requestStopEdgePackagingJob $
--             newStopEdgePackagingJob
--
--         , requestUpdateUserProfile $
--             newUpdateUserProfile
--
--         , requestDescribeCompilationJob $
--             newDescribeCompilationJob
--
--         , requestListPipelineExecutionSteps $
--             newListPipelineExecutionSteps
--
--         , requestListUserProfiles $
--             newListUserProfiles
--
--         , requestDescribeHumanTaskUi $
--             newDescribeHumanTaskUi
--
--         , requestListCodeRepositories $
--             newListCodeRepositories
--
--         , requestDescribeAction $
--             newDescribeAction
--
--         , requestUpdatePipeline $
--             newUpdatePipeline
--
--         , requestDescribeArtifact $
--             newDescribeArtifact
--
--         , requestDeleteUserProfile $
--             newDeleteUserProfile
--
--         , requestStopTransformJob $
--             newStopTransformJob
--
--         , requestCreateDataQualityJobDefinition $
--             newCreateDataQualityJobDefinition
--
--         , requestDeleteModelPackageGroupPolicy $
--             newDeleteModelPackageGroupPolicy
--
--         , requestCreateImage $
--             newCreateImage
--
--         , requestDescribeModelPackage $
--             newDescribeModelPackage
--
--         , requestRetryPipelineExecution $
--             newRetryPipelineExecution
--
--         , requestDeleteEndpointConfig $
--             newDeleteEndpointConfig
--
--         , requestStopPipelineExecution $
--             newStopPipelineExecution
--
--         , requestListApps $
--             newListApps
--
--         , requestCreateWorkforce $
--             newCreateWorkforce
--
--         , requestCreateStudioLifecycleConfig $
--             newCreateStudioLifecycleConfig
--
--         , requestListAutoMLJobs $
--             newListAutoMLJobs
--
--         , requestUpdateEndpointWeightsAndCapacities $
--             newUpdateEndpointWeightsAndCapacities
--
--         , requestStartNotebookInstance $
--             newStartNotebookInstance
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestCreateAppImageConfig $
--             newCreateAppImageConfig
--
--         , requestDeleteAssociation $
--             newDeleteAssociation
--
--         , requestListMonitoringSchedules $
--             newListMonitoringSchedules
--
--         , requestDeleteMonitoringSchedule $
--             newDeleteMonitoringSchedule
--
--         , requestListEndpointConfigs $
--             newListEndpointConfigs
--
--         , requestStartPipelineExecution $
--             newStartPipelineExecution
--
--         , requestStopNotebookInstance $
--             newStopNotebookInstance
--
--         , requestUpdateMonitoringSchedule $
--             newUpdateMonitoringSchedule
--
--         , requestAddAssociation $
--             newAddAssociation
--
--         , requestCreateNotebookInstanceLifecycleConfig $
--             newCreateNotebookInstanceLifecycleConfig
--
--         , requestAddTags $
--             newAddTags
--
--         , requestDisableSagemakerServicecatalogPortfolio $
--             newDisableSagemakerServicecatalogPortfolio
--
--         , requestUpdateNotebookInstanceLifecycleConfig $
--             newUpdateNotebookInstanceLifecycleConfig
--
--         , requestDescribeDomain $
--             newDescribeDomain
--
--         , requestListStudioLifecycleConfigs $
--             newListStudioLifecycleConfigs
--
--         , requestListAppImageConfigs $
--             newListAppImageConfigs
--
--         , requestDescribeModelQualityJobDefinition $
--             newDescribeModelQualityJobDefinition
--
--         , requestDescribeDevice $
--             newDescribeDevice
--
--         , requestListNotebookInstanceLifecycleConfigs $
--             newListNotebookInstanceLifecycleConfigs
--
--         , requestListWorkforces $
--             newListWorkforces
--
--         , requestDeleteStudioLifecycleConfig $
--             newDeleteStudioLifecycleConfig
--
--         , requestCreateLabelingJob $
--             newCreateLabelingJob
--
--         , requestDeleteModelBiasJobDefinition $
--             newDeleteModelBiasJobDefinition
--
--         , requestCreateExperiment $
--             newCreateExperiment
--
--         , requestDescribePipelineExecution $
--             newDescribePipelineExecution
--
--         , requestDeleteWorkforce $
--             newDeleteWorkforce
--
--         , requestUpdateWorkforce $
--             newUpdateWorkforce
--
--         , requestUpdateWorkteam $
--             newUpdateWorkteam
--
--         , requestDescribeSubscribedWorkteam $
--             newDescribeSubscribedWorkteam
--
--         , requestDeleteWorkteam $
--             newDeleteWorkteam
--
--         , requestDeleteNotebookInstanceLifecycleConfig $
--             newDeleteNotebookInstanceLifecycleConfig
--
--         , requestListEdgePackagingJobs $
--             newListEdgePackagingJobs
--
--         , requestCreateCompilationJob $
--             newCreateCompilationJob
--
--         , requestCreateAction $
--             newCreateAction
--
--         , requestCreateArtifact $
--             newCreateArtifact
--
--         , requestDeleteEndpoint $
--             newDeleteEndpoint
--
--         , requestListTransformJobs $
--             newListTransformJobs
--
--         , requestDescribeCodeRepository $
--             newDescribeCodeRepository
--
--         , requestCreateDeviceFleet $
--             newCreateDeviceFleet
--
--         , requestDescribeModel $
--             newDescribeModel
--
--         , requestDescribeDataQualityJobDefinition $
--             newDescribeDataQualityJobDefinition
--
--         , requestUpdateEndpoint $
--             newUpdateEndpoint
--
--         , requestCreatePresignedDomainUrl $
--             newCreatePresignedDomainUrl
--
--         , requestDescribeHyperParameterTuningJob $
--             newDescribeHyperParameterTuningJob
--
--         , requestListEndpoints $
--             newListEndpoints
--
--         , requestDescribeAlgorithm $
--             newDescribeAlgorithm
--
--         , requestUpdateDevices $
--             newUpdateDevices
--
--         , requestListDeviceFleets $
--             newListDeviceFleets
--
--         , requestListLabelingJobsForWorkteam $
--             newListLabelingJobsForWorkteam
--
--         , requestCreateFeatureGroup $
--             newCreateFeatureGroup
--
--         , requestListMonitoringExecutions $
--             newListMonitoringExecutions
--
--         , requestDescribeModelPackageGroup $
--             newDescribeModelPackageGroup
--
--         , requestUpdateDeviceFleet $
--             newUpdateDeviceFleet
--
--         , requestStopHyperParameterTuningJob $
--             newStopHyperParameterTuningJob
--
--         , requestDeleteDeviceFleet $
--             newDeleteDeviceFleet
--
--         , requestListActions $
--             newListActions
--
--         , requestListArtifacts $
--             newListArtifacts
--
--         , requestListCompilationJobs $
--             newListCompilationJobs
--
--         , requestDescribeTrial $
--             newDescribeTrial
--
--         , requestDeleteImageVersion $
--             newDeleteImageVersion
--
--         , requestDeleteTrialComponent $
--             newDeleteTrialComponent
--
--         , requestListTrialComponents $
--             newListTrialComponents
--
--         , requestDescribeAutoMLJob $
--             newDescribeAutoMLJob
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestDescribeApp $
--             newDescribeApp
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestDescribeExperiment $
--             newDescribeExperiment
--
--         , requestListImageVersions $
--             newListImageVersions
--
--         , requestStopProcessingJob $
--             newStopProcessingJob
--
--         , requestUpdateTrialComponent $
--             newUpdateTrialComponent
--
--         , requestUpdatePipelineExecution $
--             newUpdatePipelineExecution
--
--         , requestCreateTrialComponent $
--             newCreateTrialComponent
--
--         , requestListPipelineExecutions $
--             newListPipelineExecutions
--
--         , requestListModelExplainabilityJobDefinitions $
--             newListModelExplainabilityJobDefinitions
--
--         , requestAssociateTrialComponent $
--             newAssociateTrialComponent
--
--         , requestDescribeModelBiasJobDefinition $
--             newDescribeModelBiasJobDefinition
--
--         , requestDescribeStudioLifecycleConfig $
--             newDescribeStudioLifecycleConfig
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestListNotebookInstances $
--             newListNotebookInstances
--
--         , requestDescribeNotebookInstanceLifecycleConfig $
--             newDescribeNotebookInstanceLifecycleConfig
--
--         , requestStopLabelingJob $
--             newStopLabelingJob
--
--         , requestUpdateDomain $
--             newUpdateDomain
--
--         , requestDescribeWorkforce $
--             newDescribeWorkforce
--
--         , requestCreateImageVersion $
--             newCreateImageVersion
--
--         , requestDeleteModelQualityJobDefinition $
--             newDeleteModelQualityJobDefinition
--
--         , requestDescribeWorkteam $
--             newDescribeWorkteam
--
--         , requestListModelQualityJobDefinitions $
--             newListModelQualityJobDefinitions
--
--         , requestUpdateContext $
--             newUpdateContext
--
--         , requestListModels $
--             newListModels
--
--         , requestListHyperParameterTuningJobs $
--             newListHyperParameterTuningJobs
--
--         , requestSendPipelineExecutionStepFailure $
--             newSendPipelineExecutionStepFailure
--
--         , requestDescribeFeatureGroup $
--             newDescribeFeatureGroup
--
--         , requestUpdateImage $
--             newUpdateImage
--
--         , requestListFlowDefinitions $
--             newListFlowDefinitions
--
--         , requestListAlgorithms $
--             newListAlgorithms
--
--         , requestUpdateTrainingJob $
--             newUpdateTrainingJob
--
--         , requestDeleteFlowDefinition $
--             newDeleteFlowDefinition
--
--         , requestDeleteImage $
--             newDeleteImage
--
--         , requestRenderUiTemplate $
--             newRenderUiTemplate
--
--         , requestCreateUserProfile $
--             newCreateUserProfile
--
--         , requestDeleteContext $
--             newDeleteContext
--
--         , requestListTags $
--             newListTags
--
--         , requestPutModelPackageGroupPolicy $
--             newPutModelPackageGroupPolicy
--
--         , requestListTrials $
--             newListTrials
--
--         , requestDescribePipelineDefinitionForExecution $
--             newDescribePipelineDefinitionForExecution
--
--         , requestListModelPackageGroups $
--             newListModelPackageGroups
--
--         , requestListPipelines $
--             newListPipelines
--
--         , requestListPipelineParametersForExecution $
--             newListPipelineParametersForExecution
--
--         , requestDeleteTrial $
--             newDeleteTrial
--
--         , requestCreateAlgorithm $
--             newCreateAlgorithm
--
--         , requestUpdateTrial $
--             newUpdateTrial
--
--         , requestDeleteModelPackageGroup $
--             newDeleteModelPackageGroup
--
--         , requestDescribeDeviceFleet $
--             newDescribeDeviceFleet
--
--         , requestCreateHyperParameterTuningJob $
--             newCreateHyperParameterTuningJob
--
--         , requestCreateModel $
--             newCreateModel
--
--         , requestCreateFlowDefinition $
--             newCreateFlowDefinition
--
--         , requestListAssociations $
--             newListAssociations
--
--         , requestDeleteExperiment $
--             newDeleteExperiment
--
--         , requestDescribeProject $
--             newDescribeProject
--
--         , requestDescribeImageVersion $
--             newDescribeImageVersion
--
--         , requestListExperiments $
--             newListExperiments
--
--         , requestCreateModelBiasJobDefinition $
--             newCreateModelBiasJobDefinition
--
--         , requestUpdateExperiment $
--             newUpdateExperiment
--
--         , requestDescribeTrialComponent $
--             newDescribeTrialComponent
--
--         , requestCreateWorkteam $
--             newCreateWorkteam
--
--         , requestCreateProcessingJob $
--             newCreateProcessingJob
--
--         , requestDeleteApp $
--             newDeleteApp
--
--         , requestListLabelingJobs $
--             newListLabelingJobs
--
--         , requestListWorkteams $
--             newListWorkteams
--
--         , requestDeleteAppImageConfig $
--             newDeleteAppImageConfig
--
--         , requestCreateApp $
--             newCreateApp
--
--         , requestEnableSagemakerServicecatalogPortfolio $
--             newEnableSagemakerServicecatalogPortfolio
--
--         , requestDescribeNotebookInstance $
--             newDescribeNotebookInstance
--
--         , requestDescribeModelExplainabilityJobDefinition $
--             newDescribeModelExplainabilityJobDefinition
--
--         , requestCreateAutoMLJob $
--             newCreateAutoMLJob
--
--         , requestCreateEndpointConfig $
--             newCreateEndpointConfig
--
--         , requestListModelBiasJobDefinitions $
--             newListModelBiasJobDefinitions
--
--         , requestSendPipelineExecutionStepSuccess $
--             newSendPipelineExecutionStepSuccess
--
--         , requestCreateMonitoringSchedule $
--             newCreateMonitoringSchedule
--
--         , requestListProcessingJobs $
--             newListProcessingJobs
--
--         , requestUpdateAppImageConfig $
--             newUpdateAppImageConfig
--
--         , requestDescribeContext $
--             newDescribeContext
--
--         , requestCreateHumanTaskUi $
--             newCreateHumanTaskUi
--
--         , requestDeleteFeatureGroup $
--             newDeleteFeatureGroup
--
--         , requestDescribeTrainingJob $
--             newDescribeTrainingJob
--
--         , requestDescribeFlowDefinition $
--             newDescribeFlowDefinition
--
--         , requestListTrainingJobsForHyperParameterTuningJob $
--             newListTrainingJobsForHyperParameterTuningJob
--
--         , requestCreatePresignedNotebookInstanceUrl $
--             newCreatePresignedNotebookInstanceUrl
--
--         , requestRegisterDevices $
--             newRegisterDevices
--
--         , requestListFeatureGroups $
--             newListFeatureGroups
--
--         , requestDescribeImage $
--             newDescribeImage
--
--           ]

--     , testGroup "response"
--         [ responseCreateEdgePackagingJob $
--             newCreateEdgePackagingJobResponse
--
--         , responseDescribeUserProfile $
--             newDescribeUserProfileResponse
--
--         , responseListHumanTaskUis $
--             newListHumanTaskUisResponse
--
--         , responseDeleteHumanTaskUi $
--             newDeleteHumanTaskUiResponse
--
--         , responseUpdateAction $
--             newUpdateActionResponse
--
--         , responseCreateEndpoint $
--             newCreateEndpointResponse
--
--         , responseDeleteArtifact $
--             newDeleteArtifactResponse
--
--         , responseCreateTransformJob $
--             newCreateTransformJobResponse
--
--         , responseDeleteAction $
--             newDeleteActionResponse
--
--         , responseDescribePipeline $
--             newDescribePipelineResponse
--
--         , responseUpdateArtifact $
--             newUpdateArtifactResponse
--
--         , responseStopTrainingJob $
--             newStopTrainingJobResponse
--
--         , responseDisassociateTrialComponent $
--             newDisassociateTrialComponentResponse
--
--         , responseGetSearchSuggestions $
--             newGetSearchSuggestionsResponse
--
--         , responseDeleteModelPackage $
--             newDeleteModelPackageResponse
--
--         , responseCreateModelQualityJobDefinition $
--             newCreateModelQualityJobDefinitionResponse
--
--         , responseListModelPackages $
--             newListModelPackagesResponse
--
--         , responseListProjects $
--             newListProjectsResponse
--
--         , responseCreateModelExplainabilityJobDefinition $
--             newCreateModelExplainabilityJobDefinitionResponse
--
--         , responseDescribeEndpointConfig $
--             newDescribeEndpointConfigResponse
--
--         , responseDescribeMonitoringSchedule $
--             newDescribeMonitoringScheduleResponse
--
--         , responseDescribeLabelingJob $
--             newDescribeLabelingJobResponse
--
--         , responseGetModelPackageGroupPolicy $
--             newGetModelPackageGroupPolicyResponse
--
--         , responseUpdateModelPackage $
--             newUpdateModelPackageResponse
--
--         , responseCreateNotebookInstance $
--             newCreateNotebookInstanceResponse
--
--         , responseStopMonitoringSchedule $
--             newStopMonitoringScheduleResponse
--
--         , responseCreateModelPackage $
--             newCreateModelPackageResponse
--
--         , responseStopAutoMLJob $
--             newStopAutoMLJobResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseDescribeAppImageConfig $
--             newDescribeAppImageConfigResponse
--
--         , responseListSubscribedWorkteams $
--             newListSubscribedWorkteamsResponse
--
--         , responseListDevices $
--             newListDevicesResponse
--
--         , responseUpdateNotebookInstance $
--             newUpdateNotebookInstanceResponse
--
--         , responseDeleteModelExplainabilityJobDefinition $
--             newDeleteModelExplainabilityJobDefinitionResponse
--
--         , responseDescribeProcessingJob $
--             newDescribeProcessingJobResponse
--
--         , responseStartMonitoringSchedule $
--             newStartMonitoringScheduleResponse
--
--         , responseDeleteNotebookInstance $
--             newDeleteNotebookInstanceResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--         , responseCreateTrial $
--             newCreateTrialResponse
--
--         , responseDescribeTransformJob $
--             newDescribeTransformJobResponse
--
--         , responseDescribeEdgePackagingJob $
--             newDescribeEdgePackagingJobResponse
--
--         , responseDeleteModel $
--             newDeleteModelResponse
--
--         , responseListTrainingJobs $
--             newListTrainingJobsResponse
--
--         , responseListDataQualityJobDefinitions $
--             newListDataQualityJobDefinitionsResponse
--
--         , responseGetSagemakerServicecatalogPortfolioStatus $
--             newGetSagemakerServicecatalogPortfolioStatusResponse
--
--         , responseSearch $
--             newSearchResponse
--
--         , responseStopCompilationJob $
--             newStopCompilationJobResponse
--
--         , responseListImages $
--             newListImagesResponse
--
--         , responseListCandidatesForAutoMLJob $
--             newListCandidatesForAutoMLJobResponse
--
--         , responseDeleteAlgorithm $
--             newDeleteAlgorithmResponse
--
--         , responseGetDeviceFleetReport $
--             newGetDeviceFleetReportResponse
--
--         , responseDeleteDataQualityJobDefinition $
--             newDeleteDataQualityJobDefinitionResponse
--
--         , responseCreateModelPackageGroup $
--             newCreateModelPackageGroupResponse
--
--         , responseCreatePipeline $
--             newCreatePipelineResponse
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
--         , responseDescribeEndpoint $
--             newDescribeEndpointResponse
--
--         , responseDeregisterDevices $
--             newDeregisterDevicesResponse
--
--         , responseCreateCodeRepository $
--             newCreateCodeRepositoryResponse
--
--         , responseCreateTrainingJob $
--             newCreateTrainingJobResponse
--
--         , responseDeletePipeline $
--             newDeletePipelineResponse
--
--         , responseCreateContext $
--             newCreateContextResponse
--
--         , responseStopEdgePackagingJob $
--             newStopEdgePackagingJobResponse
--
--         , responseUpdateUserProfile $
--             newUpdateUserProfileResponse
--
--         , responseDescribeCompilationJob $
--             newDescribeCompilationJobResponse
--
--         , responseListPipelineExecutionSteps $
--             newListPipelineExecutionStepsResponse
--
--         , responseListUserProfiles $
--             newListUserProfilesResponse
--
--         , responseDescribeHumanTaskUi $
--             newDescribeHumanTaskUiResponse
--
--         , responseListCodeRepositories $
--             newListCodeRepositoriesResponse
--
--         , responseDescribeAction $
--             newDescribeActionResponse
--
--         , responseUpdatePipeline $
--             newUpdatePipelineResponse
--
--         , responseDescribeArtifact $
--             newDescribeArtifactResponse
--
--         , responseDeleteUserProfile $
--             newDeleteUserProfileResponse
--
--         , responseStopTransformJob $
--             newStopTransformJobResponse
--
--         , responseCreateDataQualityJobDefinition $
--             newCreateDataQualityJobDefinitionResponse
--
--         , responseDeleteModelPackageGroupPolicy $
--             newDeleteModelPackageGroupPolicyResponse
--
--         , responseCreateImage $
--             newCreateImageResponse
--
--         , responseDescribeModelPackage $
--             newDescribeModelPackageResponse
--
--         , responseRetryPipelineExecution $
--             newRetryPipelineExecutionResponse
--
--         , responseDeleteEndpointConfig $
--             newDeleteEndpointConfigResponse
--
--         , responseStopPipelineExecution $
--             newStopPipelineExecutionResponse
--
--         , responseListApps $
--             newListAppsResponse
--
--         , responseCreateWorkforce $
--             newCreateWorkforceResponse
--
--         , responseCreateStudioLifecycleConfig $
--             newCreateStudioLifecycleConfigResponse
--
--         , responseListAutoMLJobs $
--             newListAutoMLJobsResponse
--
--         , responseUpdateEndpointWeightsAndCapacities $
--             newUpdateEndpointWeightsAndCapacitiesResponse
--
--         , responseStartNotebookInstance $
--             newStartNotebookInstanceResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseCreateAppImageConfig $
--             newCreateAppImageConfigResponse
--
--         , responseDeleteAssociation $
--             newDeleteAssociationResponse
--
--         , responseListMonitoringSchedules $
--             newListMonitoringSchedulesResponse
--
--         , responseDeleteMonitoringSchedule $
--             newDeleteMonitoringScheduleResponse
--
--         , responseListEndpointConfigs $
--             newListEndpointConfigsResponse
--
--         , responseStartPipelineExecution $
--             newStartPipelineExecutionResponse
--
--         , responseStopNotebookInstance $
--             newStopNotebookInstanceResponse
--
--         , responseUpdateMonitoringSchedule $
--             newUpdateMonitoringScheduleResponse
--
--         , responseAddAssociation $
--             newAddAssociationResponse
--
--         , responseCreateNotebookInstanceLifecycleConfig $
--             newCreateNotebookInstanceLifecycleConfigResponse
--
--         , responseAddTags $
--             newAddTagsResponse
--
--         , responseDisableSagemakerServicecatalogPortfolio $
--             newDisableSagemakerServicecatalogPortfolioResponse
--
--         , responseUpdateNotebookInstanceLifecycleConfig $
--             newUpdateNotebookInstanceLifecycleConfigResponse
--
--         , responseDescribeDomain $
--             newDescribeDomainResponse
--
--         , responseListStudioLifecycleConfigs $
--             newListStudioLifecycleConfigsResponse
--
--         , responseListAppImageConfigs $
--             newListAppImageConfigsResponse
--
--         , responseDescribeModelQualityJobDefinition $
--             newDescribeModelQualityJobDefinitionResponse
--
--         , responseDescribeDevice $
--             newDescribeDeviceResponse
--
--         , responseListNotebookInstanceLifecycleConfigs $
--             newListNotebookInstanceLifecycleConfigsResponse
--
--         , responseListWorkforces $
--             newListWorkforcesResponse
--
--         , responseDeleteStudioLifecycleConfig $
--             newDeleteStudioLifecycleConfigResponse
--
--         , responseCreateLabelingJob $
--             newCreateLabelingJobResponse
--
--         , responseDeleteModelBiasJobDefinition $
--             newDeleteModelBiasJobDefinitionResponse
--
--         , responseCreateExperiment $
--             newCreateExperimentResponse
--
--         , responseDescribePipelineExecution $
--             newDescribePipelineExecutionResponse
--
--         , responseDeleteWorkforce $
--             newDeleteWorkforceResponse
--
--         , responseUpdateWorkforce $
--             newUpdateWorkforceResponse
--
--         , responseUpdateWorkteam $
--             newUpdateWorkteamResponse
--
--         , responseDescribeSubscribedWorkteam $
--             newDescribeSubscribedWorkteamResponse
--
--         , responseDeleteWorkteam $
--             newDeleteWorkteamResponse
--
--         , responseDeleteNotebookInstanceLifecycleConfig $
--             newDeleteNotebookInstanceLifecycleConfigResponse
--
--         , responseListEdgePackagingJobs $
--             newListEdgePackagingJobsResponse
--
--         , responseCreateCompilationJob $
--             newCreateCompilationJobResponse
--
--         , responseCreateAction $
--             newCreateActionResponse
--
--         , responseCreateArtifact $
--             newCreateArtifactResponse
--
--         , responseDeleteEndpoint $
--             newDeleteEndpointResponse
--
--         , responseListTransformJobs $
--             newListTransformJobsResponse
--
--         , responseDescribeCodeRepository $
--             newDescribeCodeRepositoryResponse
--
--         , responseCreateDeviceFleet $
--             newCreateDeviceFleetResponse
--
--         , responseDescribeModel $
--             newDescribeModelResponse
--
--         , responseDescribeDataQualityJobDefinition $
--             newDescribeDataQualityJobDefinitionResponse
--
--         , responseUpdateEndpoint $
--             newUpdateEndpointResponse
--
--         , responseCreatePresignedDomainUrl $
--             newCreatePresignedDomainUrlResponse
--
--         , responseDescribeHyperParameterTuningJob $
--             newDescribeHyperParameterTuningJobResponse
--
--         , responseListEndpoints $
--             newListEndpointsResponse
--
--         , responseDescribeAlgorithm $
--             newDescribeAlgorithmResponse
--
--         , responseUpdateDevices $
--             newUpdateDevicesResponse
--
--         , responseListDeviceFleets $
--             newListDeviceFleetsResponse
--
--         , responseListLabelingJobsForWorkteam $
--             newListLabelingJobsForWorkteamResponse
--
--         , responseCreateFeatureGroup $
--             newCreateFeatureGroupResponse
--
--         , responseListMonitoringExecutions $
--             newListMonitoringExecutionsResponse
--
--         , responseDescribeModelPackageGroup $
--             newDescribeModelPackageGroupResponse
--
--         , responseUpdateDeviceFleet $
--             newUpdateDeviceFleetResponse
--
--         , responseStopHyperParameterTuningJob $
--             newStopHyperParameterTuningJobResponse
--
--         , responseDeleteDeviceFleet $
--             newDeleteDeviceFleetResponse
--
--         , responseListActions $
--             newListActionsResponse
--
--         , responseListArtifacts $
--             newListArtifactsResponse
--
--         , responseListCompilationJobs $
--             newListCompilationJobsResponse
--
--         , responseDescribeTrial $
--             newDescribeTrialResponse
--
--         , responseDeleteImageVersion $
--             newDeleteImageVersionResponse
--
--         , responseDeleteTrialComponent $
--             newDeleteTrialComponentResponse
--
--         , responseListTrialComponents $
--             newListTrialComponentsResponse
--
--         , responseDescribeAutoMLJob $
--             newDescribeAutoMLJobResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseDescribeApp $
--             newDescribeAppResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseDescribeExperiment $
--             newDescribeExperimentResponse
--
--         , responseListImageVersions $
--             newListImageVersionsResponse
--
--         , responseStopProcessingJob $
--             newStopProcessingJobResponse
--
--         , responseUpdateTrialComponent $
--             newUpdateTrialComponentResponse
--
--         , responseUpdatePipelineExecution $
--             newUpdatePipelineExecutionResponse
--
--         , responseCreateTrialComponent $
--             newCreateTrialComponentResponse
--
--         , responseListPipelineExecutions $
--             newListPipelineExecutionsResponse
--
--         , responseListModelExplainabilityJobDefinitions $
--             newListModelExplainabilityJobDefinitionsResponse
--
--         , responseAssociateTrialComponent $
--             newAssociateTrialComponentResponse
--
--         , responseDescribeModelBiasJobDefinition $
--             newDescribeModelBiasJobDefinitionResponse
--
--         , responseDescribeStudioLifecycleConfig $
--             newDescribeStudioLifecycleConfigResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseListNotebookInstances $
--             newListNotebookInstancesResponse
--
--         , responseDescribeNotebookInstanceLifecycleConfig $
--             newDescribeNotebookInstanceLifecycleConfigResponse
--
--         , responseStopLabelingJob $
--             newStopLabelingJobResponse
--
--         , responseUpdateDomain $
--             newUpdateDomainResponse
--
--         , responseDescribeWorkforce $
--             newDescribeWorkforceResponse
--
--         , responseCreateImageVersion $
--             newCreateImageVersionResponse
--
--         , responseDeleteModelQualityJobDefinition $
--             newDeleteModelQualityJobDefinitionResponse
--
--         , responseDescribeWorkteam $
--             newDescribeWorkteamResponse
--
--         , responseListModelQualityJobDefinitions $
--             newListModelQualityJobDefinitionsResponse
--
--         , responseUpdateContext $
--             newUpdateContextResponse
--
--         , responseListModels $
--             newListModelsResponse
--
--         , responseListHyperParameterTuningJobs $
--             newListHyperParameterTuningJobsResponse
--
--         , responseSendPipelineExecutionStepFailure $
--             newSendPipelineExecutionStepFailureResponse
--
--         , responseDescribeFeatureGroup $
--             newDescribeFeatureGroupResponse
--
--         , responseUpdateImage $
--             newUpdateImageResponse
--
--         , responseListFlowDefinitions $
--             newListFlowDefinitionsResponse
--
--         , responseListAlgorithms $
--             newListAlgorithmsResponse
--
--         , responseUpdateTrainingJob $
--             newUpdateTrainingJobResponse
--
--         , responseDeleteFlowDefinition $
--             newDeleteFlowDefinitionResponse
--
--         , responseDeleteImage $
--             newDeleteImageResponse
--
--         , responseRenderUiTemplate $
--             newRenderUiTemplateResponse
--
--         , responseCreateUserProfile $
--             newCreateUserProfileResponse
--
--         , responseDeleteContext $
--             newDeleteContextResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responsePutModelPackageGroupPolicy $
--             newPutModelPackageGroupPolicyResponse
--
--         , responseListTrials $
--             newListTrialsResponse
--
--         , responseDescribePipelineDefinitionForExecution $
--             newDescribePipelineDefinitionForExecutionResponse
--
--         , responseListModelPackageGroups $
--             newListModelPackageGroupsResponse
--
--         , responseListPipelines $
--             newListPipelinesResponse
--
--         , responseListPipelineParametersForExecution $
--             newListPipelineParametersForExecutionResponse
--
--         , responseDeleteTrial $
--             newDeleteTrialResponse
--
--         , responseCreateAlgorithm $
--             newCreateAlgorithmResponse
--
--         , responseUpdateTrial $
--             newUpdateTrialResponse
--
--         , responseDeleteModelPackageGroup $
--             newDeleteModelPackageGroupResponse
--
--         , responseDescribeDeviceFleet $
--             newDescribeDeviceFleetResponse
--
--         , responseCreateHyperParameterTuningJob $
--             newCreateHyperParameterTuningJobResponse
--
--         , responseCreateModel $
--             newCreateModelResponse
--
--         , responseCreateFlowDefinition $
--             newCreateFlowDefinitionResponse
--
--         , responseListAssociations $
--             newListAssociationsResponse
--
--         , responseDeleteExperiment $
--             newDeleteExperimentResponse
--
--         , responseDescribeProject $
--             newDescribeProjectResponse
--
--         , responseDescribeImageVersion $
--             newDescribeImageVersionResponse
--
--         , responseListExperiments $
--             newListExperimentsResponse
--
--         , responseCreateModelBiasJobDefinition $
--             newCreateModelBiasJobDefinitionResponse
--
--         , responseUpdateExperiment $
--             newUpdateExperimentResponse
--
--         , responseDescribeTrialComponent $
--             newDescribeTrialComponentResponse
--
--         , responseCreateWorkteam $
--             newCreateWorkteamResponse
--
--         , responseCreateProcessingJob $
--             newCreateProcessingJobResponse
--
--         , responseDeleteApp $
--             newDeleteAppResponse
--
--         , responseListLabelingJobs $
--             newListLabelingJobsResponse
--
--         , responseListWorkteams $
--             newListWorkteamsResponse
--
--         , responseDeleteAppImageConfig $
--             newDeleteAppImageConfigResponse
--
--         , responseCreateApp $
--             newCreateAppResponse
--
--         , responseEnableSagemakerServicecatalogPortfolio $
--             newEnableSagemakerServicecatalogPortfolioResponse
--
--         , responseDescribeNotebookInstance $
--             newDescribeNotebookInstanceResponse
--
--         , responseDescribeModelExplainabilityJobDefinition $
--             newDescribeModelExplainabilityJobDefinitionResponse
--
--         , responseCreateAutoMLJob $
--             newCreateAutoMLJobResponse
--
--         , responseCreateEndpointConfig $
--             newCreateEndpointConfigResponse
--
--         , responseListModelBiasJobDefinitions $
--             newListModelBiasJobDefinitionsResponse
--
--         , responseSendPipelineExecutionStepSuccess $
--             newSendPipelineExecutionStepSuccessResponse
--
--         , responseCreateMonitoringSchedule $
--             newCreateMonitoringScheduleResponse
--
--         , responseListProcessingJobs $
--             newListProcessingJobsResponse
--
--         , responseUpdateAppImageConfig $
--             newUpdateAppImageConfigResponse
--
--         , responseDescribeContext $
--             newDescribeContextResponse
--
--         , responseCreateHumanTaskUi $
--             newCreateHumanTaskUiResponse
--
--         , responseDeleteFeatureGroup $
--             newDeleteFeatureGroupResponse
--
--         , responseDescribeTrainingJob $
--             newDescribeTrainingJobResponse
--
--         , responseDescribeFlowDefinition $
--             newDescribeFlowDefinitionResponse
--
--         , responseListTrainingJobsForHyperParameterTuningJob $
--             newListTrainingJobsForHyperParameterTuningJobResponse
--
--         , responseCreatePresignedNotebookInstanceUrl $
--             newCreatePresignedNotebookInstanceUrlResponse
--
--         , responseRegisterDevices $
--             newRegisterDevicesResponse
--
--         , responseListFeatureGroups $
--             newListFeatureGroupsResponse
--
--         , responseDescribeImage $
--             newDescribeImageResponse
--
--           ]
--     ]

-- Requests

requestCreateEdgePackagingJob :: CreateEdgePackagingJob -> TestTree
requestCreateEdgePackagingJob =
  req
    "CreateEdgePackagingJob"
    "fixture/CreateEdgePackagingJob.yaml"

requestDescribeUserProfile :: DescribeUserProfile -> TestTree
requestDescribeUserProfile =
  req
    "DescribeUserProfile"
    "fixture/DescribeUserProfile.yaml"

requestListHumanTaskUis :: ListHumanTaskUis -> TestTree
requestListHumanTaskUis =
  req
    "ListHumanTaskUis"
    "fixture/ListHumanTaskUis.yaml"

requestDeleteHumanTaskUi :: DeleteHumanTaskUi -> TestTree
requestDeleteHumanTaskUi =
  req
    "DeleteHumanTaskUi"
    "fixture/DeleteHumanTaskUi.yaml"

requestUpdateAction :: UpdateAction -> TestTree
requestUpdateAction =
  req
    "UpdateAction"
    "fixture/UpdateAction.yaml"

requestCreateEndpoint :: CreateEndpoint -> TestTree
requestCreateEndpoint =
  req
    "CreateEndpoint"
    "fixture/CreateEndpoint.yaml"

requestDeleteArtifact :: DeleteArtifact -> TestTree
requestDeleteArtifact =
  req
    "DeleteArtifact"
    "fixture/DeleteArtifact.yaml"

requestCreateTransformJob :: CreateTransformJob -> TestTree
requestCreateTransformJob =
  req
    "CreateTransformJob"
    "fixture/CreateTransformJob.yaml"

requestDeleteAction :: DeleteAction -> TestTree
requestDeleteAction =
  req
    "DeleteAction"
    "fixture/DeleteAction.yaml"

requestDescribePipeline :: DescribePipeline -> TestTree
requestDescribePipeline =
  req
    "DescribePipeline"
    "fixture/DescribePipeline.yaml"

requestUpdateArtifact :: UpdateArtifact -> TestTree
requestUpdateArtifact =
  req
    "UpdateArtifact"
    "fixture/UpdateArtifact.yaml"

requestStopTrainingJob :: StopTrainingJob -> TestTree
requestStopTrainingJob =
  req
    "StopTrainingJob"
    "fixture/StopTrainingJob.yaml"

requestDisassociateTrialComponent :: DisassociateTrialComponent -> TestTree
requestDisassociateTrialComponent =
  req
    "DisassociateTrialComponent"
    "fixture/DisassociateTrialComponent.yaml"

requestGetSearchSuggestions :: GetSearchSuggestions -> TestTree
requestGetSearchSuggestions =
  req
    "GetSearchSuggestions"
    "fixture/GetSearchSuggestions.yaml"

requestDeleteModelPackage :: DeleteModelPackage -> TestTree
requestDeleteModelPackage =
  req
    "DeleteModelPackage"
    "fixture/DeleteModelPackage.yaml"

requestCreateModelQualityJobDefinition :: CreateModelQualityJobDefinition -> TestTree
requestCreateModelQualityJobDefinition =
  req
    "CreateModelQualityJobDefinition"
    "fixture/CreateModelQualityJobDefinition.yaml"

requestListModelPackages :: ListModelPackages -> TestTree
requestListModelPackages =
  req
    "ListModelPackages"
    "fixture/ListModelPackages.yaml"

requestListProjects :: ListProjects -> TestTree
requestListProjects =
  req
    "ListProjects"
    "fixture/ListProjects.yaml"

requestCreateModelExplainabilityJobDefinition :: CreateModelExplainabilityJobDefinition -> TestTree
requestCreateModelExplainabilityJobDefinition =
  req
    "CreateModelExplainabilityJobDefinition"
    "fixture/CreateModelExplainabilityJobDefinition.yaml"

requestDescribeEndpointConfig :: DescribeEndpointConfig -> TestTree
requestDescribeEndpointConfig =
  req
    "DescribeEndpointConfig"
    "fixture/DescribeEndpointConfig.yaml"

requestDescribeMonitoringSchedule :: DescribeMonitoringSchedule -> TestTree
requestDescribeMonitoringSchedule =
  req
    "DescribeMonitoringSchedule"
    "fixture/DescribeMonitoringSchedule.yaml"

requestDescribeLabelingJob :: DescribeLabelingJob -> TestTree
requestDescribeLabelingJob =
  req
    "DescribeLabelingJob"
    "fixture/DescribeLabelingJob.yaml"

requestGetModelPackageGroupPolicy :: GetModelPackageGroupPolicy -> TestTree
requestGetModelPackageGroupPolicy =
  req
    "GetModelPackageGroupPolicy"
    "fixture/GetModelPackageGroupPolicy.yaml"

requestUpdateModelPackage :: UpdateModelPackage -> TestTree
requestUpdateModelPackage =
  req
    "UpdateModelPackage"
    "fixture/UpdateModelPackage.yaml"

requestCreateNotebookInstance :: CreateNotebookInstance -> TestTree
requestCreateNotebookInstance =
  req
    "CreateNotebookInstance"
    "fixture/CreateNotebookInstance.yaml"

requestStopMonitoringSchedule :: StopMonitoringSchedule -> TestTree
requestStopMonitoringSchedule =
  req
    "StopMonitoringSchedule"
    "fixture/StopMonitoringSchedule.yaml"

requestCreateModelPackage :: CreateModelPackage -> TestTree
requestCreateModelPackage =
  req
    "CreateModelPackage"
    "fixture/CreateModelPackage.yaml"

requestStopAutoMLJob :: StopAutoMLJob -> TestTree
requestStopAutoMLJob =
  req
    "StopAutoMLJob"
    "fixture/StopAutoMLJob.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestDescribeAppImageConfig :: DescribeAppImageConfig -> TestTree
requestDescribeAppImageConfig =
  req
    "DescribeAppImageConfig"
    "fixture/DescribeAppImageConfig.yaml"

requestListSubscribedWorkteams :: ListSubscribedWorkteams -> TestTree
requestListSubscribedWorkteams =
  req
    "ListSubscribedWorkteams"
    "fixture/ListSubscribedWorkteams.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices =
  req
    "ListDevices"
    "fixture/ListDevices.yaml"

requestUpdateNotebookInstance :: UpdateNotebookInstance -> TestTree
requestUpdateNotebookInstance =
  req
    "UpdateNotebookInstance"
    "fixture/UpdateNotebookInstance.yaml"

requestDeleteModelExplainabilityJobDefinition :: DeleteModelExplainabilityJobDefinition -> TestTree
requestDeleteModelExplainabilityJobDefinition =
  req
    "DeleteModelExplainabilityJobDefinition"
    "fixture/DeleteModelExplainabilityJobDefinition.yaml"

requestDescribeProcessingJob :: DescribeProcessingJob -> TestTree
requestDescribeProcessingJob =
  req
    "DescribeProcessingJob"
    "fixture/DescribeProcessingJob.yaml"

requestStartMonitoringSchedule :: StartMonitoringSchedule -> TestTree
requestStartMonitoringSchedule =
  req
    "StartMonitoringSchedule"
    "fixture/StartMonitoringSchedule.yaml"

requestDeleteNotebookInstance :: DeleteNotebookInstance -> TestTree
requestDeleteNotebookInstance =
  req
    "DeleteNotebookInstance"
    "fixture/DeleteNotebookInstance.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

requestCreateTrial :: CreateTrial -> TestTree
requestCreateTrial =
  req
    "CreateTrial"
    "fixture/CreateTrial.yaml"

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

requestDeleteModel :: DeleteModel -> TestTree
requestDeleteModel =
  req
    "DeleteModel"
    "fixture/DeleteModel.yaml"

requestListTrainingJobs :: ListTrainingJobs -> TestTree
requestListTrainingJobs =
  req
    "ListTrainingJobs"
    "fixture/ListTrainingJobs.yaml"

requestListDataQualityJobDefinitions :: ListDataQualityJobDefinitions -> TestTree
requestListDataQualityJobDefinitions =
  req
    "ListDataQualityJobDefinitions"
    "fixture/ListDataQualityJobDefinitions.yaml"

requestGetSagemakerServicecatalogPortfolioStatus :: GetSagemakerServicecatalogPortfolioStatus -> TestTree
requestGetSagemakerServicecatalogPortfolioStatus =
  req
    "GetSagemakerServicecatalogPortfolioStatus"
    "fixture/GetSagemakerServicecatalogPortfolioStatus.yaml"

requestSearch :: Search -> TestTree
requestSearch =
  req
    "Search"
    "fixture/Search.yaml"

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

requestListCandidatesForAutoMLJob :: ListCandidatesForAutoMLJob -> TestTree
requestListCandidatesForAutoMLJob =
  req
    "ListCandidatesForAutoMLJob"
    "fixture/ListCandidatesForAutoMLJob.yaml"

requestDeleteAlgorithm :: DeleteAlgorithm -> TestTree
requestDeleteAlgorithm =
  req
    "DeleteAlgorithm"
    "fixture/DeleteAlgorithm.yaml"

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

requestCreateModelPackageGroup :: CreateModelPackageGroup -> TestTree
requestCreateModelPackageGroup =
  req
    "CreateModelPackageGroup"
    "fixture/CreateModelPackageGroup.yaml"

requestCreatePipeline :: CreatePipeline -> TestTree
requestCreatePipeline =
  req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

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

requestDescribeEndpoint :: DescribeEndpoint -> TestTree
requestDescribeEndpoint =
  req
    "DescribeEndpoint"
    "fixture/DescribeEndpoint.yaml"

requestDeregisterDevices :: DeregisterDevices -> TestTree
requestDeregisterDevices =
  req
    "DeregisterDevices"
    "fixture/DeregisterDevices.yaml"

requestCreateCodeRepository :: CreateCodeRepository -> TestTree
requestCreateCodeRepository =
  req
    "CreateCodeRepository"
    "fixture/CreateCodeRepository.yaml"

requestCreateTrainingJob :: CreateTrainingJob -> TestTree
requestCreateTrainingJob =
  req
    "CreateTrainingJob"
    "fixture/CreateTrainingJob.yaml"

requestDeletePipeline :: DeletePipeline -> TestTree
requestDeletePipeline =
  req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

requestCreateContext :: CreateContext -> TestTree
requestCreateContext =
  req
    "CreateContext"
    "fixture/CreateContext.yaml"

requestStopEdgePackagingJob :: StopEdgePackagingJob -> TestTree
requestStopEdgePackagingJob =
  req
    "StopEdgePackagingJob"
    "fixture/StopEdgePackagingJob.yaml"

requestUpdateUserProfile :: UpdateUserProfile -> TestTree
requestUpdateUserProfile =
  req
    "UpdateUserProfile"
    "fixture/UpdateUserProfile.yaml"

requestDescribeCompilationJob :: DescribeCompilationJob -> TestTree
requestDescribeCompilationJob =
  req
    "DescribeCompilationJob"
    "fixture/DescribeCompilationJob.yaml"

requestListPipelineExecutionSteps :: ListPipelineExecutionSteps -> TestTree
requestListPipelineExecutionSteps =
  req
    "ListPipelineExecutionSteps"
    "fixture/ListPipelineExecutionSteps.yaml"

requestListUserProfiles :: ListUserProfiles -> TestTree
requestListUserProfiles =
  req
    "ListUserProfiles"
    "fixture/ListUserProfiles.yaml"

requestDescribeHumanTaskUi :: DescribeHumanTaskUi -> TestTree
requestDescribeHumanTaskUi =
  req
    "DescribeHumanTaskUi"
    "fixture/DescribeHumanTaskUi.yaml"

requestListCodeRepositories :: ListCodeRepositories -> TestTree
requestListCodeRepositories =
  req
    "ListCodeRepositories"
    "fixture/ListCodeRepositories.yaml"

requestDescribeAction :: DescribeAction -> TestTree
requestDescribeAction =
  req
    "DescribeAction"
    "fixture/DescribeAction.yaml"

requestUpdatePipeline :: UpdatePipeline -> TestTree
requestUpdatePipeline =
  req
    "UpdatePipeline"
    "fixture/UpdatePipeline.yaml"

requestDescribeArtifact :: DescribeArtifact -> TestTree
requestDescribeArtifact =
  req
    "DescribeArtifact"
    "fixture/DescribeArtifact.yaml"

requestDeleteUserProfile :: DeleteUserProfile -> TestTree
requestDeleteUserProfile =
  req
    "DeleteUserProfile"
    "fixture/DeleteUserProfile.yaml"

requestStopTransformJob :: StopTransformJob -> TestTree
requestStopTransformJob =
  req
    "StopTransformJob"
    "fixture/StopTransformJob.yaml"

requestCreateDataQualityJobDefinition :: CreateDataQualityJobDefinition -> TestTree
requestCreateDataQualityJobDefinition =
  req
    "CreateDataQualityJobDefinition"
    "fixture/CreateDataQualityJobDefinition.yaml"

requestDeleteModelPackageGroupPolicy :: DeleteModelPackageGroupPolicy -> TestTree
requestDeleteModelPackageGroupPolicy =
  req
    "DeleteModelPackageGroupPolicy"
    "fixture/DeleteModelPackageGroupPolicy.yaml"

requestCreateImage :: CreateImage -> TestTree
requestCreateImage =
  req
    "CreateImage"
    "fixture/CreateImage.yaml"

requestDescribeModelPackage :: DescribeModelPackage -> TestTree
requestDescribeModelPackage =
  req
    "DescribeModelPackage"
    "fixture/DescribeModelPackage.yaml"

requestRetryPipelineExecution :: RetryPipelineExecution -> TestTree
requestRetryPipelineExecution =
  req
    "RetryPipelineExecution"
    "fixture/RetryPipelineExecution.yaml"

requestDeleteEndpointConfig :: DeleteEndpointConfig -> TestTree
requestDeleteEndpointConfig =
  req
    "DeleteEndpointConfig"
    "fixture/DeleteEndpointConfig.yaml"

requestStopPipelineExecution :: StopPipelineExecution -> TestTree
requestStopPipelineExecution =
  req
    "StopPipelineExecution"
    "fixture/StopPipelineExecution.yaml"

requestListApps :: ListApps -> TestTree
requestListApps =
  req
    "ListApps"
    "fixture/ListApps.yaml"

requestCreateWorkforce :: CreateWorkforce -> TestTree
requestCreateWorkforce =
  req
    "CreateWorkforce"
    "fixture/CreateWorkforce.yaml"

requestCreateStudioLifecycleConfig :: CreateStudioLifecycleConfig -> TestTree
requestCreateStudioLifecycleConfig =
  req
    "CreateStudioLifecycleConfig"
    "fixture/CreateStudioLifecycleConfig.yaml"

requestListAutoMLJobs :: ListAutoMLJobs -> TestTree
requestListAutoMLJobs =
  req
    "ListAutoMLJobs"
    "fixture/ListAutoMLJobs.yaml"

requestUpdateEndpointWeightsAndCapacities :: UpdateEndpointWeightsAndCapacities -> TestTree
requestUpdateEndpointWeightsAndCapacities =
  req
    "UpdateEndpointWeightsAndCapacities"
    "fixture/UpdateEndpointWeightsAndCapacities.yaml"

requestStartNotebookInstance :: StartNotebookInstance -> TestTree
requestStartNotebookInstance =
  req
    "StartNotebookInstance"
    "fixture/StartNotebookInstance.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestCreateAppImageConfig :: CreateAppImageConfig -> TestTree
requestCreateAppImageConfig =
  req
    "CreateAppImageConfig"
    "fixture/CreateAppImageConfig.yaml"

requestDeleteAssociation :: DeleteAssociation -> TestTree
requestDeleteAssociation =
  req
    "DeleteAssociation"
    "fixture/DeleteAssociation.yaml"

requestListMonitoringSchedules :: ListMonitoringSchedules -> TestTree
requestListMonitoringSchedules =
  req
    "ListMonitoringSchedules"
    "fixture/ListMonitoringSchedules.yaml"

requestDeleteMonitoringSchedule :: DeleteMonitoringSchedule -> TestTree
requestDeleteMonitoringSchedule =
  req
    "DeleteMonitoringSchedule"
    "fixture/DeleteMonitoringSchedule.yaml"

requestListEndpointConfigs :: ListEndpointConfigs -> TestTree
requestListEndpointConfigs =
  req
    "ListEndpointConfigs"
    "fixture/ListEndpointConfigs.yaml"

requestStartPipelineExecution :: StartPipelineExecution -> TestTree
requestStartPipelineExecution =
  req
    "StartPipelineExecution"
    "fixture/StartPipelineExecution.yaml"

requestStopNotebookInstance :: StopNotebookInstance -> TestTree
requestStopNotebookInstance =
  req
    "StopNotebookInstance"
    "fixture/StopNotebookInstance.yaml"

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

requestCreateNotebookInstanceLifecycleConfig :: CreateNotebookInstanceLifecycleConfig -> TestTree
requestCreateNotebookInstanceLifecycleConfig =
  req
    "CreateNotebookInstanceLifecycleConfig"
    "fixture/CreateNotebookInstanceLifecycleConfig.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

requestDisableSagemakerServicecatalogPortfolio :: DisableSagemakerServicecatalogPortfolio -> TestTree
requestDisableSagemakerServicecatalogPortfolio =
  req
    "DisableSagemakerServicecatalogPortfolio"
    "fixture/DisableSagemakerServicecatalogPortfolio.yaml"

requestUpdateNotebookInstanceLifecycleConfig :: UpdateNotebookInstanceLifecycleConfig -> TestTree
requestUpdateNotebookInstanceLifecycleConfig =
  req
    "UpdateNotebookInstanceLifecycleConfig"
    "fixture/UpdateNotebookInstanceLifecycleConfig.yaml"

requestDescribeDomain :: DescribeDomain -> TestTree
requestDescribeDomain =
  req
    "DescribeDomain"
    "fixture/DescribeDomain.yaml"

requestListStudioLifecycleConfigs :: ListStudioLifecycleConfigs -> TestTree
requestListStudioLifecycleConfigs =
  req
    "ListStudioLifecycleConfigs"
    "fixture/ListStudioLifecycleConfigs.yaml"

requestListAppImageConfigs :: ListAppImageConfigs -> TestTree
requestListAppImageConfigs =
  req
    "ListAppImageConfigs"
    "fixture/ListAppImageConfigs.yaml"

requestDescribeModelQualityJobDefinition :: DescribeModelQualityJobDefinition -> TestTree
requestDescribeModelQualityJobDefinition =
  req
    "DescribeModelQualityJobDefinition"
    "fixture/DescribeModelQualityJobDefinition.yaml"

requestDescribeDevice :: DescribeDevice -> TestTree
requestDescribeDevice =
  req
    "DescribeDevice"
    "fixture/DescribeDevice.yaml"

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

requestDeleteStudioLifecycleConfig :: DeleteStudioLifecycleConfig -> TestTree
requestDeleteStudioLifecycleConfig =
  req
    "DeleteStudioLifecycleConfig"
    "fixture/DeleteStudioLifecycleConfig.yaml"

requestCreateLabelingJob :: CreateLabelingJob -> TestTree
requestCreateLabelingJob =
  req
    "CreateLabelingJob"
    "fixture/CreateLabelingJob.yaml"

requestDeleteModelBiasJobDefinition :: DeleteModelBiasJobDefinition -> TestTree
requestDeleteModelBiasJobDefinition =
  req
    "DeleteModelBiasJobDefinition"
    "fixture/DeleteModelBiasJobDefinition.yaml"

requestCreateExperiment :: CreateExperiment -> TestTree
requestCreateExperiment =
  req
    "CreateExperiment"
    "fixture/CreateExperiment.yaml"

requestDescribePipelineExecution :: DescribePipelineExecution -> TestTree
requestDescribePipelineExecution =
  req
    "DescribePipelineExecution"
    "fixture/DescribePipelineExecution.yaml"

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

requestUpdateWorkteam :: UpdateWorkteam -> TestTree
requestUpdateWorkteam =
  req
    "UpdateWorkteam"
    "fixture/UpdateWorkteam.yaml"

requestDescribeSubscribedWorkteam :: DescribeSubscribedWorkteam -> TestTree
requestDescribeSubscribedWorkteam =
  req
    "DescribeSubscribedWorkteam"
    "fixture/DescribeSubscribedWorkteam.yaml"

requestDeleteWorkteam :: DeleteWorkteam -> TestTree
requestDeleteWorkteam =
  req
    "DeleteWorkteam"
    "fixture/DeleteWorkteam.yaml"

requestDeleteNotebookInstanceLifecycleConfig :: DeleteNotebookInstanceLifecycleConfig -> TestTree
requestDeleteNotebookInstanceLifecycleConfig =
  req
    "DeleteNotebookInstanceLifecycleConfig"
    "fixture/DeleteNotebookInstanceLifecycleConfig.yaml"

requestListEdgePackagingJobs :: ListEdgePackagingJobs -> TestTree
requestListEdgePackagingJobs =
  req
    "ListEdgePackagingJobs"
    "fixture/ListEdgePackagingJobs.yaml"

requestCreateCompilationJob :: CreateCompilationJob -> TestTree
requestCreateCompilationJob =
  req
    "CreateCompilationJob"
    "fixture/CreateCompilationJob.yaml"

requestCreateAction :: CreateAction -> TestTree
requestCreateAction =
  req
    "CreateAction"
    "fixture/CreateAction.yaml"

requestCreateArtifact :: CreateArtifact -> TestTree
requestCreateArtifact =
  req
    "CreateArtifact"
    "fixture/CreateArtifact.yaml"

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint =
  req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

requestListTransformJobs :: ListTransformJobs -> TestTree
requestListTransformJobs =
  req
    "ListTransformJobs"
    "fixture/ListTransformJobs.yaml"

requestDescribeCodeRepository :: DescribeCodeRepository -> TestTree
requestDescribeCodeRepository =
  req
    "DescribeCodeRepository"
    "fixture/DescribeCodeRepository.yaml"

requestCreateDeviceFleet :: CreateDeviceFleet -> TestTree
requestCreateDeviceFleet =
  req
    "CreateDeviceFleet"
    "fixture/CreateDeviceFleet.yaml"

requestDescribeModel :: DescribeModel -> TestTree
requestDescribeModel =
  req
    "DescribeModel"
    "fixture/DescribeModel.yaml"

requestDescribeDataQualityJobDefinition :: DescribeDataQualityJobDefinition -> TestTree
requestDescribeDataQualityJobDefinition =
  req
    "DescribeDataQualityJobDefinition"
    "fixture/DescribeDataQualityJobDefinition.yaml"

requestUpdateEndpoint :: UpdateEndpoint -> TestTree
requestUpdateEndpoint =
  req
    "UpdateEndpoint"
    "fixture/UpdateEndpoint.yaml"

requestCreatePresignedDomainUrl :: CreatePresignedDomainUrl -> TestTree
requestCreatePresignedDomainUrl =
  req
    "CreatePresignedDomainUrl"
    "fixture/CreatePresignedDomainUrl.yaml"

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

requestListDeviceFleets :: ListDeviceFleets -> TestTree
requestListDeviceFleets =
  req
    "ListDeviceFleets"
    "fixture/ListDeviceFleets.yaml"

requestListLabelingJobsForWorkteam :: ListLabelingJobsForWorkteam -> TestTree
requestListLabelingJobsForWorkteam =
  req
    "ListLabelingJobsForWorkteam"
    "fixture/ListLabelingJobsForWorkteam.yaml"

requestCreateFeatureGroup :: CreateFeatureGroup -> TestTree
requestCreateFeatureGroup =
  req
    "CreateFeatureGroup"
    "fixture/CreateFeatureGroup.yaml"

requestListMonitoringExecutions :: ListMonitoringExecutions -> TestTree
requestListMonitoringExecutions =
  req
    "ListMonitoringExecutions"
    "fixture/ListMonitoringExecutions.yaml"

requestDescribeModelPackageGroup :: DescribeModelPackageGroup -> TestTree
requestDescribeModelPackageGroup =
  req
    "DescribeModelPackageGroup"
    "fixture/DescribeModelPackageGroup.yaml"

requestUpdateDeviceFleet :: UpdateDeviceFleet -> TestTree
requestUpdateDeviceFleet =
  req
    "UpdateDeviceFleet"
    "fixture/UpdateDeviceFleet.yaml"

requestStopHyperParameterTuningJob :: StopHyperParameterTuningJob -> TestTree
requestStopHyperParameterTuningJob =
  req
    "StopHyperParameterTuningJob"
    "fixture/StopHyperParameterTuningJob.yaml"

requestDeleteDeviceFleet :: DeleteDeviceFleet -> TestTree
requestDeleteDeviceFleet =
  req
    "DeleteDeviceFleet"
    "fixture/DeleteDeviceFleet.yaml"

requestListActions :: ListActions -> TestTree
requestListActions =
  req
    "ListActions"
    "fixture/ListActions.yaml"

requestListArtifacts :: ListArtifacts -> TestTree
requestListArtifacts =
  req
    "ListArtifacts"
    "fixture/ListArtifacts.yaml"

requestListCompilationJobs :: ListCompilationJobs -> TestTree
requestListCompilationJobs =
  req
    "ListCompilationJobs"
    "fixture/ListCompilationJobs.yaml"

requestDescribeTrial :: DescribeTrial -> TestTree
requestDescribeTrial =
  req
    "DescribeTrial"
    "fixture/DescribeTrial.yaml"

requestDeleteImageVersion :: DeleteImageVersion -> TestTree
requestDeleteImageVersion =
  req
    "DeleteImageVersion"
    "fixture/DeleteImageVersion.yaml"

requestDeleteTrialComponent :: DeleteTrialComponent -> TestTree
requestDeleteTrialComponent =
  req
    "DeleteTrialComponent"
    "fixture/DeleteTrialComponent.yaml"

requestListTrialComponents :: ListTrialComponents -> TestTree
requestListTrialComponents =
  req
    "ListTrialComponents"
    "fixture/ListTrialComponents.yaml"

requestDescribeAutoMLJob :: DescribeAutoMLJob -> TestTree
requestDescribeAutoMLJob =
  req
    "DescribeAutoMLJob"
    "fixture/DescribeAutoMLJob.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject =
  req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestDescribeApp :: DescribeApp -> TestTree
requestDescribeApp =
  req
    "DescribeApp"
    "fixture/DescribeApp.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestDescribeExperiment :: DescribeExperiment -> TestTree
requestDescribeExperiment =
  req
    "DescribeExperiment"
    "fixture/DescribeExperiment.yaml"

requestListImageVersions :: ListImageVersions -> TestTree
requestListImageVersions =
  req
    "ListImageVersions"
    "fixture/ListImageVersions.yaml"

requestStopProcessingJob :: StopProcessingJob -> TestTree
requestStopProcessingJob =
  req
    "StopProcessingJob"
    "fixture/StopProcessingJob.yaml"

requestUpdateTrialComponent :: UpdateTrialComponent -> TestTree
requestUpdateTrialComponent =
  req
    "UpdateTrialComponent"
    "fixture/UpdateTrialComponent.yaml"

requestUpdatePipelineExecution :: UpdatePipelineExecution -> TestTree
requestUpdatePipelineExecution =
  req
    "UpdatePipelineExecution"
    "fixture/UpdatePipelineExecution.yaml"

requestCreateTrialComponent :: CreateTrialComponent -> TestTree
requestCreateTrialComponent =
  req
    "CreateTrialComponent"
    "fixture/CreateTrialComponent.yaml"

requestListPipelineExecutions :: ListPipelineExecutions -> TestTree
requestListPipelineExecutions =
  req
    "ListPipelineExecutions"
    "fixture/ListPipelineExecutions.yaml"

requestListModelExplainabilityJobDefinitions :: ListModelExplainabilityJobDefinitions -> TestTree
requestListModelExplainabilityJobDefinitions =
  req
    "ListModelExplainabilityJobDefinitions"
    "fixture/ListModelExplainabilityJobDefinitions.yaml"

requestAssociateTrialComponent :: AssociateTrialComponent -> TestTree
requestAssociateTrialComponent =
  req
    "AssociateTrialComponent"
    "fixture/AssociateTrialComponent.yaml"

requestDescribeModelBiasJobDefinition :: DescribeModelBiasJobDefinition -> TestTree
requestDescribeModelBiasJobDefinition =
  req
    "DescribeModelBiasJobDefinition"
    "fixture/DescribeModelBiasJobDefinition.yaml"

requestDescribeStudioLifecycleConfig :: DescribeStudioLifecycleConfig -> TestTree
requestDescribeStudioLifecycleConfig =
  req
    "DescribeStudioLifecycleConfig"
    "fixture/DescribeStudioLifecycleConfig.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestListNotebookInstances :: ListNotebookInstances -> TestTree
requestListNotebookInstances =
  req
    "ListNotebookInstances"
    "fixture/ListNotebookInstances.yaml"

requestDescribeNotebookInstanceLifecycleConfig :: DescribeNotebookInstanceLifecycleConfig -> TestTree
requestDescribeNotebookInstanceLifecycleConfig =
  req
    "DescribeNotebookInstanceLifecycleConfig"
    "fixture/DescribeNotebookInstanceLifecycleConfig.yaml"

requestStopLabelingJob :: StopLabelingJob -> TestTree
requestStopLabelingJob =
  req
    "StopLabelingJob"
    "fixture/StopLabelingJob.yaml"

requestUpdateDomain :: UpdateDomain -> TestTree
requestUpdateDomain =
  req
    "UpdateDomain"
    "fixture/UpdateDomain.yaml"

requestDescribeWorkforce :: DescribeWorkforce -> TestTree
requestDescribeWorkforce =
  req
    "DescribeWorkforce"
    "fixture/DescribeWorkforce.yaml"

requestCreateImageVersion :: CreateImageVersion -> TestTree
requestCreateImageVersion =
  req
    "CreateImageVersion"
    "fixture/CreateImageVersion.yaml"

requestDeleteModelQualityJobDefinition :: DeleteModelQualityJobDefinition -> TestTree
requestDeleteModelQualityJobDefinition =
  req
    "DeleteModelQualityJobDefinition"
    "fixture/DeleteModelQualityJobDefinition.yaml"

requestDescribeWorkteam :: DescribeWorkteam -> TestTree
requestDescribeWorkteam =
  req
    "DescribeWorkteam"
    "fixture/DescribeWorkteam.yaml"

requestListModelQualityJobDefinitions :: ListModelQualityJobDefinitions -> TestTree
requestListModelQualityJobDefinitions =
  req
    "ListModelQualityJobDefinitions"
    "fixture/ListModelQualityJobDefinitions.yaml"

requestUpdateContext :: UpdateContext -> TestTree
requestUpdateContext =
  req
    "UpdateContext"
    "fixture/UpdateContext.yaml"

requestListModels :: ListModels -> TestTree
requestListModels =
  req
    "ListModels"
    "fixture/ListModels.yaml"

requestListHyperParameterTuningJobs :: ListHyperParameterTuningJobs -> TestTree
requestListHyperParameterTuningJobs =
  req
    "ListHyperParameterTuningJobs"
    "fixture/ListHyperParameterTuningJobs.yaml"

requestSendPipelineExecutionStepFailure :: SendPipelineExecutionStepFailure -> TestTree
requestSendPipelineExecutionStepFailure =
  req
    "SendPipelineExecutionStepFailure"
    "fixture/SendPipelineExecutionStepFailure.yaml"

requestDescribeFeatureGroup :: DescribeFeatureGroup -> TestTree
requestDescribeFeatureGroup =
  req
    "DescribeFeatureGroup"
    "fixture/DescribeFeatureGroup.yaml"

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

requestListAlgorithms :: ListAlgorithms -> TestTree
requestListAlgorithms =
  req
    "ListAlgorithms"
    "fixture/ListAlgorithms.yaml"

requestUpdateTrainingJob :: UpdateTrainingJob -> TestTree
requestUpdateTrainingJob =
  req
    "UpdateTrainingJob"
    "fixture/UpdateTrainingJob.yaml"

requestDeleteFlowDefinition :: DeleteFlowDefinition -> TestTree
requestDeleteFlowDefinition =
  req
    "DeleteFlowDefinition"
    "fixture/DeleteFlowDefinition.yaml"

requestDeleteImage :: DeleteImage -> TestTree
requestDeleteImage =
  req
    "DeleteImage"
    "fixture/DeleteImage.yaml"

requestRenderUiTemplate :: RenderUiTemplate -> TestTree
requestRenderUiTemplate =
  req
    "RenderUiTemplate"
    "fixture/RenderUiTemplate.yaml"

requestCreateUserProfile :: CreateUserProfile -> TestTree
requestCreateUserProfile =
  req
    "CreateUserProfile"
    "fixture/CreateUserProfile.yaml"

requestDeleteContext :: DeleteContext -> TestTree
requestDeleteContext =
  req
    "DeleteContext"
    "fixture/DeleteContext.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestPutModelPackageGroupPolicy :: PutModelPackageGroupPolicy -> TestTree
requestPutModelPackageGroupPolicy =
  req
    "PutModelPackageGroupPolicy"
    "fixture/PutModelPackageGroupPolicy.yaml"

requestListTrials :: ListTrials -> TestTree
requestListTrials =
  req
    "ListTrials"
    "fixture/ListTrials.yaml"

requestDescribePipelineDefinitionForExecution :: DescribePipelineDefinitionForExecution -> TestTree
requestDescribePipelineDefinitionForExecution =
  req
    "DescribePipelineDefinitionForExecution"
    "fixture/DescribePipelineDefinitionForExecution.yaml"

requestListModelPackageGroups :: ListModelPackageGroups -> TestTree
requestListModelPackageGroups =
  req
    "ListModelPackageGroups"
    "fixture/ListModelPackageGroups.yaml"

requestListPipelines :: ListPipelines -> TestTree
requestListPipelines =
  req
    "ListPipelines"
    "fixture/ListPipelines.yaml"

requestListPipelineParametersForExecution :: ListPipelineParametersForExecution -> TestTree
requestListPipelineParametersForExecution =
  req
    "ListPipelineParametersForExecution"
    "fixture/ListPipelineParametersForExecution.yaml"

requestDeleteTrial :: DeleteTrial -> TestTree
requestDeleteTrial =
  req
    "DeleteTrial"
    "fixture/DeleteTrial.yaml"

requestCreateAlgorithm :: CreateAlgorithm -> TestTree
requestCreateAlgorithm =
  req
    "CreateAlgorithm"
    "fixture/CreateAlgorithm.yaml"

requestUpdateTrial :: UpdateTrial -> TestTree
requestUpdateTrial =
  req
    "UpdateTrial"
    "fixture/UpdateTrial.yaml"

requestDeleteModelPackageGroup :: DeleteModelPackageGroup -> TestTree
requestDeleteModelPackageGroup =
  req
    "DeleteModelPackageGroup"
    "fixture/DeleteModelPackageGroup.yaml"

requestDescribeDeviceFleet :: DescribeDeviceFleet -> TestTree
requestDescribeDeviceFleet =
  req
    "DescribeDeviceFleet"
    "fixture/DescribeDeviceFleet.yaml"

requestCreateHyperParameterTuningJob :: CreateHyperParameterTuningJob -> TestTree
requestCreateHyperParameterTuningJob =
  req
    "CreateHyperParameterTuningJob"
    "fixture/CreateHyperParameterTuningJob.yaml"

requestCreateModel :: CreateModel -> TestTree
requestCreateModel =
  req
    "CreateModel"
    "fixture/CreateModel.yaml"

requestCreateFlowDefinition :: CreateFlowDefinition -> TestTree
requestCreateFlowDefinition =
  req
    "CreateFlowDefinition"
    "fixture/CreateFlowDefinition.yaml"

requestListAssociations :: ListAssociations -> TestTree
requestListAssociations =
  req
    "ListAssociations"
    "fixture/ListAssociations.yaml"

requestDeleteExperiment :: DeleteExperiment -> TestTree
requestDeleteExperiment =
  req
    "DeleteExperiment"
    "fixture/DeleteExperiment.yaml"

requestDescribeProject :: DescribeProject -> TestTree
requestDescribeProject =
  req
    "DescribeProject"
    "fixture/DescribeProject.yaml"

requestDescribeImageVersion :: DescribeImageVersion -> TestTree
requestDescribeImageVersion =
  req
    "DescribeImageVersion"
    "fixture/DescribeImageVersion.yaml"

requestListExperiments :: ListExperiments -> TestTree
requestListExperiments =
  req
    "ListExperiments"
    "fixture/ListExperiments.yaml"

requestCreateModelBiasJobDefinition :: CreateModelBiasJobDefinition -> TestTree
requestCreateModelBiasJobDefinition =
  req
    "CreateModelBiasJobDefinition"
    "fixture/CreateModelBiasJobDefinition.yaml"

requestUpdateExperiment :: UpdateExperiment -> TestTree
requestUpdateExperiment =
  req
    "UpdateExperiment"
    "fixture/UpdateExperiment.yaml"

requestDescribeTrialComponent :: DescribeTrialComponent -> TestTree
requestDescribeTrialComponent =
  req
    "DescribeTrialComponent"
    "fixture/DescribeTrialComponent.yaml"

requestCreateWorkteam :: CreateWorkteam -> TestTree
requestCreateWorkteam =
  req
    "CreateWorkteam"
    "fixture/CreateWorkteam.yaml"

requestCreateProcessingJob :: CreateProcessingJob -> TestTree
requestCreateProcessingJob =
  req
    "CreateProcessingJob"
    "fixture/CreateProcessingJob.yaml"

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp =
  req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

requestListLabelingJobs :: ListLabelingJobs -> TestTree
requestListLabelingJobs =
  req
    "ListLabelingJobs"
    "fixture/ListLabelingJobs.yaml"

requestListWorkteams :: ListWorkteams -> TestTree
requestListWorkteams =
  req
    "ListWorkteams"
    "fixture/ListWorkteams.yaml"

requestDeleteAppImageConfig :: DeleteAppImageConfig -> TestTree
requestDeleteAppImageConfig =
  req
    "DeleteAppImageConfig"
    "fixture/DeleteAppImageConfig.yaml"

requestCreateApp :: CreateApp -> TestTree
requestCreateApp =
  req
    "CreateApp"
    "fixture/CreateApp.yaml"

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

requestDescribeModelExplainabilityJobDefinition :: DescribeModelExplainabilityJobDefinition -> TestTree
requestDescribeModelExplainabilityJobDefinition =
  req
    "DescribeModelExplainabilityJobDefinition"
    "fixture/DescribeModelExplainabilityJobDefinition.yaml"

requestCreateAutoMLJob :: CreateAutoMLJob -> TestTree
requestCreateAutoMLJob =
  req
    "CreateAutoMLJob"
    "fixture/CreateAutoMLJob.yaml"

requestCreateEndpointConfig :: CreateEndpointConfig -> TestTree
requestCreateEndpointConfig =
  req
    "CreateEndpointConfig"
    "fixture/CreateEndpointConfig.yaml"

requestListModelBiasJobDefinitions :: ListModelBiasJobDefinitions -> TestTree
requestListModelBiasJobDefinitions =
  req
    "ListModelBiasJobDefinitions"
    "fixture/ListModelBiasJobDefinitions.yaml"

requestSendPipelineExecutionStepSuccess :: SendPipelineExecutionStepSuccess -> TestTree
requestSendPipelineExecutionStepSuccess =
  req
    "SendPipelineExecutionStepSuccess"
    "fixture/SendPipelineExecutionStepSuccess.yaml"

requestCreateMonitoringSchedule :: CreateMonitoringSchedule -> TestTree
requestCreateMonitoringSchedule =
  req
    "CreateMonitoringSchedule"
    "fixture/CreateMonitoringSchedule.yaml"

requestListProcessingJobs :: ListProcessingJobs -> TestTree
requestListProcessingJobs =
  req
    "ListProcessingJobs"
    "fixture/ListProcessingJobs.yaml"

requestUpdateAppImageConfig :: UpdateAppImageConfig -> TestTree
requestUpdateAppImageConfig =
  req
    "UpdateAppImageConfig"
    "fixture/UpdateAppImageConfig.yaml"

requestDescribeContext :: DescribeContext -> TestTree
requestDescribeContext =
  req
    "DescribeContext"
    "fixture/DescribeContext.yaml"

requestCreateHumanTaskUi :: CreateHumanTaskUi -> TestTree
requestCreateHumanTaskUi =
  req
    "CreateHumanTaskUi"
    "fixture/CreateHumanTaskUi.yaml"

requestDeleteFeatureGroup :: DeleteFeatureGroup -> TestTree
requestDeleteFeatureGroup =
  req
    "DeleteFeatureGroup"
    "fixture/DeleteFeatureGroup.yaml"

requestDescribeTrainingJob :: DescribeTrainingJob -> TestTree
requestDescribeTrainingJob =
  req
    "DescribeTrainingJob"
    "fixture/DescribeTrainingJob.yaml"

requestDescribeFlowDefinition :: DescribeFlowDefinition -> TestTree
requestDescribeFlowDefinition =
  req
    "DescribeFlowDefinition"
    "fixture/DescribeFlowDefinition.yaml"

requestListTrainingJobsForHyperParameterTuningJob :: ListTrainingJobsForHyperParameterTuningJob -> TestTree
requestListTrainingJobsForHyperParameterTuningJob =
  req
    "ListTrainingJobsForHyperParameterTuningJob"
    "fixture/ListTrainingJobsForHyperParameterTuningJob.yaml"

requestCreatePresignedNotebookInstanceUrl :: CreatePresignedNotebookInstanceUrl -> TestTree
requestCreatePresignedNotebookInstanceUrl =
  req
    "CreatePresignedNotebookInstanceUrl"
    "fixture/CreatePresignedNotebookInstanceUrl.yaml"

requestRegisterDevices :: RegisterDevices -> TestTree
requestRegisterDevices =
  req
    "RegisterDevices"
    "fixture/RegisterDevices.yaml"

requestListFeatureGroups :: ListFeatureGroups -> TestTree
requestListFeatureGroups =
  req
    "ListFeatureGroups"
    "fixture/ListFeatureGroups.yaml"

requestDescribeImage :: DescribeImage -> TestTree
requestDescribeImage =
  req
    "DescribeImage"
    "fixture/DescribeImage.yaml"

-- Responses

responseCreateEdgePackagingJob :: CreateEdgePackagingJobResponse -> TestTree
responseCreateEdgePackagingJob =
  res
    "CreateEdgePackagingJobResponse"
    "fixture/CreateEdgePackagingJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEdgePackagingJob)

responseDescribeUserProfile :: DescribeUserProfileResponse -> TestTree
responseDescribeUserProfile =
  res
    "DescribeUserProfileResponse"
    "fixture/DescribeUserProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUserProfile)

responseListHumanTaskUis :: ListHumanTaskUisResponse -> TestTree
responseListHumanTaskUis =
  res
    "ListHumanTaskUisResponse"
    "fixture/ListHumanTaskUisResponse.proto"
    defaultService
    (Proxy :: Proxy ListHumanTaskUis)

responseDeleteHumanTaskUi :: DeleteHumanTaskUiResponse -> TestTree
responseDeleteHumanTaskUi =
  res
    "DeleteHumanTaskUiResponse"
    "fixture/DeleteHumanTaskUiResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteHumanTaskUi)

responseUpdateAction :: UpdateActionResponse -> TestTree
responseUpdateAction =
  res
    "UpdateActionResponse"
    "fixture/UpdateActionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAction)

responseCreateEndpoint :: CreateEndpointResponse -> TestTree
responseCreateEndpoint =
  res
    "CreateEndpointResponse"
    "fixture/CreateEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEndpoint)

responseDeleteArtifact :: DeleteArtifactResponse -> TestTree
responseDeleteArtifact =
  res
    "DeleteArtifactResponse"
    "fixture/DeleteArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteArtifact)

responseCreateTransformJob :: CreateTransformJobResponse -> TestTree
responseCreateTransformJob =
  res
    "CreateTransformJobResponse"
    "fixture/CreateTransformJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTransformJob)

responseDeleteAction :: DeleteActionResponse -> TestTree
responseDeleteAction =
  res
    "DeleteActionResponse"
    "fixture/DeleteActionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAction)

responseDescribePipeline :: DescribePipelineResponse -> TestTree
responseDescribePipeline =
  res
    "DescribePipelineResponse"
    "fixture/DescribePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePipeline)

responseUpdateArtifact :: UpdateArtifactResponse -> TestTree
responseUpdateArtifact =
  res
    "UpdateArtifactResponse"
    "fixture/UpdateArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateArtifact)

responseStopTrainingJob :: StopTrainingJobResponse -> TestTree
responseStopTrainingJob =
  res
    "StopTrainingJobResponse"
    "fixture/StopTrainingJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopTrainingJob)

responseDisassociateTrialComponent :: DisassociateTrialComponentResponse -> TestTree
responseDisassociateTrialComponent =
  res
    "DisassociateTrialComponentResponse"
    "fixture/DisassociateTrialComponentResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateTrialComponent)

responseGetSearchSuggestions :: GetSearchSuggestionsResponse -> TestTree
responseGetSearchSuggestions =
  res
    "GetSearchSuggestionsResponse"
    "fixture/GetSearchSuggestionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetSearchSuggestions)

responseDeleteModelPackage :: DeleteModelPackageResponse -> TestTree
responseDeleteModelPackage =
  res
    "DeleteModelPackageResponse"
    "fixture/DeleteModelPackageResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteModelPackage)

responseCreateModelQualityJobDefinition :: CreateModelQualityJobDefinitionResponse -> TestTree
responseCreateModelQualityJobDefinition =
  res
    "CreateModelQualityJobDefinitionResponse"
    "fixture/CreateModelQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateModelQualityJobDefinition)

responseListModelPackages :: ListModelPackagesResponse -> TestTree
responseListModelPackages =
  res
    "ListModelPackagesResponse"
    "fixture/ListModelPackagesResponse.proto"
    defaultService
    (Proxy :: Proxy ListModelPackages)

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    defaultService
    (Proxy :: Proxy ListProjects)

responseCreateModelExplainabilityJobDefinition :: CreateModelExplainabilityJobDefinitionResponse -> TestTree
responseCreateModelExplainabilityJobDefinition =
  res
    "CreateModelExplainabilityJobDefinitionResponse"
    "fixture/CreateModelExplainabilityJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateModelExplainabilityJobDefinition)

responseDescribeEndpointConfig :: DescribeEndpointConfigResponse -> TestTree
responseDescribeEndpointConfig =
  res
    "DescribeEndpointConfigResponse"
    "fixture/DescribeEndpointConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEndpointConfig)

responseDescribeMonitoringSchedule :: DescribeMonitoringScheduleResponse -> TestTree
responseDescribeMonitoringSchedule =
  res
    "DescribeMonitoringScheduleResponse"
    "fixture/DescribeMonitoringScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMonitoringSchedule)

responseDescribeLabelingJob :: DescribeLabelingJobResponse -> TestTree
responseDescribeLabelingJob =
  res
    "DescribeLabelingJobResponse"
    "fixture/DescribeLabelingJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLabelingJob)

responseGetModelPackageGroupPolicy :: GetModelPackageGroupPolicyResponse -> TestTree
responseGetModelPackageGroupPolicy =
  res
    "GetModelPackageGroupPolicyResponse"
    "fixture/GetModelPackageGroupPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetModelPackageGroupPolicy)

responseUpdateModelPackage :: UpdateModelPackageResponse -> TestTree
responseUpdateModelPackage =
  res
    "UpdateModelPackageResponse"
    "fixture/UpdateModelPackageResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateModelPackage)

responseCreateNotebookInstance :: CreateNotebookInstanceResponse -> TestTree
responseCreateNotebookInstance =
  res
    "CreateNotebookInstanceResponse"
    "fixture/CreateNotebookInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNotebookInstance)

responseStopMonitoringSchedule :: StopMonitoringScheduleResponse -> TestTree
responseStopMonitoringSchedule =
  res
    "StopMonitoringScheduleResponse"
    "fixture/StopMonitoringScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy StopMonitoringSchedule)

responseCreateModelPackage :: CreateModelPackageResponse -> TestTree
responseCreateModelPackage =
  res
    "CreateModelPackageResponse"
    "fixture/CreateModelPackageResponse.proto"
    defaultService
    (Proxy :: Proxy CreateModelPackage)

responseStopAutoMLJob :: StopAutoMLJobResponse -> TestTree
responseStopAutoMLJob =
  res
    "StopAutoMLJobResponse"
    "fixture/StopAutoMLJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopAutoMLJob)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProject)

responseDescribeAppImageConfig :: DescribeAppImageConfigResponse -> TestTree
responseDescribeAppImageConfig =
  res
    "DescribeAppImageConfigResponse"
    "fixture/DescribeAppImageConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAppImageConfig)

responseListSubscribedWorkteams :: ListSubscribedWorkteamsResponse -> TestTree
responseListSubscribedWorkteams =
  res
    "ListSubscribedWorkteamsResponse"
    "fixture/ListSubscribedWorkteamsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSubscribedWorkteams)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListDevices)

responseUpdateNotebookInstance :: UpdateNotebookInstanceResponse -> TestTree
responseUpdateNotebookInstance =
  res
    "UpdateNotebookInstanceResponse"
    "fixture/UpdateNotebookInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateNotebookInstance)

responseDeleteModelExplainabilityJobDefinition :: DeleteModelExplainabilityJobDefinitionResponse -> TestTree
responseDeleteModelExplainabilityJobDefinition =
  res
    "DeleteModelExplainabilityJobDefinitionResponse"
    "fixture/DeleteModelExplainabilityJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteModelExplainabilityJobDefinition)

responseDescribeProcessingJob :: DescribeProcessingJobResponse -> TestTree
responseDescribeProcessingJob =
  res
    "DescribeProcessingJobResponse"
    "fixture/DescribeProcessingJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProcessingJob)

responseStartMonitoringSchedule :: StartMonitoringScheduleResponse -> TestTree
responseStartMonitoringSchedule =
  res
    "StartMonitoringScheduleResponse"
    "fixture/StartMonitoringScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy StartMonitoringSchedule)

responseDeleteNotebookInstance :: DeleteNotebookInstanceResponse -> TestTree
responseDeleteNotebookInstance =
  res
    "DeleteNotebookInstanceResponse"
    "fixture/DeleteNotebookInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNotebookInstance)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDomains)

responseCreateTrial :: CreateTrialResponse -> TestTree
responseCreateTrial =
  res
    "CreateTrialResponse"
    "fixture/CreateTrialResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTrial)

responseDescribeTransformJob :: DescribeTransformJobResponse -> TestTree
responseDescribeTransformJob =
  res
    "DescribeTransformJobResponse"
    "fixture/DescribeTransformJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTransformJob)

responseDescribeEdgePackagingJob :: DescribeEdgePackagingJobResponse -> TestTree
responseDescribeEdgePackagingJob =
  res
    "DescribeEdgePackagingJobResponse"
    "fixture/DescribeEdgePackagingJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEdgePackagingJob)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel =
  res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteModel)

responseListTrainingJobs :: ListTrainingJobsResponse -> TestTree
responseListTrainingJobs =
  res
    "ListTrainingJobsResponse"
    "fixture/ListTrainingJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTrainingJobs)

responseListDataQualityJobDefinitions :: ListDataQualityJobDefinitionsResponse -> TestTree
responseListDataQualityJobDefinitions =
  res
    "ListDataQualityJobDefinitionsResponse"
    "fixture/ListDataQualityJobDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDataQualityJobDefinitions)

responseGetSagemakerServicecatalogPortfolioStatus :: GetSagemakerServicecatalogPortfolioStatusResponse -> TestTree
responseGetSagemakerServicecatalogPortfolioStatus =
  res
    "GetSagemakerServicecatalogPortfolioStatusResponse"
    "fixture/GetSagemakerServicecatalogPortfolioStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetSagemakerServicecatalogPortfolioStatus)

responseSearch :: SearchResponse -> TestTree
responseSearch =
  res
    "SearchResponse"
    "fixture/SearchResponse.proto"
    defaultService
    (Proxy :: Proxy Search)

responseStopCompilationJob :: StopCompilationJobResponse -> TestTree
responseStopCompilationJob =
  res
    "StopCompilationJobResponse"
    "fixture/StopCompilationJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopCompilationJob)

responseListImages :: ListImagesResponse -> TestTree
responseListImages =
  res
    "ListImagesResponse"
    "fixture/ListImagesResponse.proto"
    defaultService
    (Proxy :: Proxy ListImages)

responseListCandidatesForAutoMLJob :: ListCandidatesForAutoMLJobResponse -> TestTree
responseListCandidatesForAutoMLJob =
  res
    "ListCandidatesForAutoMLJobResponse"
    "fixture/ListCandidatesForAutoMLJobResponse.proto"
    defaultService
    (Proxy :: Proxy ListCandidatesForAutoMLJob)

responseDeleteAlgorithm :: DeleteAlgorithmResponse -> TestTree
responseDeleteAlgorithm =
  res
    "DeleteAlgorithmResponse"
    "fixture/DeleteAlgorithmResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAlgorithm)

responseGetDeviceFleetReport :: GetDeviceFleetReportResponse -> TestTree
responseGetDeviceFleetReport =
  res
    "GetDeviceFleetReportResponse"
    "fixture/GetDeviceFleetReportResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeviceFleetReport)

responseDeleteDataQualityJobDefinition :: DeleteDataQualityJobDefinitionResponse -> TestTree
responseDeleteDataQualityJobDefinition =
  res
    "DeleteDataQualityJobDefinitionResponse"
    "fixture/DeleteDataQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDataQualityJobDefinition)

responseCreateModelPackageGroup :: CreateModelPackageGroupResponse -> TestTree
responseCreateModelPackageGroup =
  res
    "CreateModelPackageGroupResponse"
    "fixture/CreateModelPackageGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateModelPackageGroup)

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline =
  res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePipeline)

responseUpdateCodeRepository :: UpdateCodeRepositoryResponse -> TestTree
responseUpdateCodeRepository =
  res
    "UpdateCodeRepositoryResponse"
    "fixture/UpdateCodeRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCodeRepository)

responseDeleteCodeRepository :: DeleteCodeRepositoryResponse -> TestTree
responseDeleteCodeRepository =
  res
    "DeleteCodeRepositoryResponse"
    "fixture/DeleteCodeRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCodeRepository)

responseListContexts :: ListContextsResponse -> TestTree
responseListContexts =
  res
    "ListContextsResponse"
    "fixture/ListContextsResponse.proto"
    defaultService
    (Proxy :: Proxy ListContexts)

responseDescribeEndpoint :: DescribeEndpointResponse -> TestTree
responseDescribeEndpoint =
  res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEndpoint)

responseDeregisterDevices :: DeregisterDevicesResponse -> TestTree
responseDeregisterDevices =
  res
    "DeregisterDevicesResponse"
    "fixture/DeregisterDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterDevices)

responseCreateCodeRepository :: CreateCodeRepositoryResponse -> TestTree
responseCreateCodeRepository =
  res
    "CreateCodeRepositoryResponse"
    "fixture/CreateCodeRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCodeRepository)

responseCreateTrainingJob :: CreateTrainingJobResponse -> TestTree
responseCreateTrainingJob =
  res
    "CreateTrainingJobResponse"
    "fixture/CreateTrainingJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTrainingJob)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline =
  res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePipeline)

responseCreateContext :: CreateContextResponse -> TestTree
responseCreateContext =
  res
    "CreateContextResponse"
    "fixture/CreateContextResponse.proto"
    defaultService
    (Proxy :: Proxy CreateContext)

responseStopEdgePackagingJob :: StopEdgePackagingJobResponse -> TestTree
responseStopEdgePackagingJob =
  res
    "StopEdgePackagingJobResponse"
    "fixture/StopEdgePackagingJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopEdgePackagingJob)

responseUpdateUserProfile :: UpdateUserProfileResponse -> TestTree
responseUpdateUserProfile =
  res
    "UpdateUserProfileResponse"
    "fixture/UpdateUserProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserProfile)

responseDescribeCompilationJob :: DescribeCompilationJobResponse -> TestTree
responseDescribeCompilationJob =
  res
    "DescribeCompilationJobResponse"
    "fixture/DescribeCompilationJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCompilationJob)

responseListPipelineExecutionSteps :: ListPipelineExecutionStepsResponse -> TestTree
responseListPipelineExecutionSteps =
  res
    "ListPipelineExecutionStepsResponse"
    "fixture/ListPipelineExecutionStepsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPipelineExecutionSteps)

responseListUserProfiles :: ListUserProfilesResponse -> TestTree
responseListUserProfiles =
  res
    "ListUserProfilesResponse"
    "fixture/ListUserProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy ListUserProfiles)

responseDescribeHumanTaskUi :: DescribeHumanTaskUiResponse -> TestTree
responseDescribeHumanTaskUi =
  res
    "DescribeHumanTaskUiResponse"
    "fixture/DescribeHumanTaskUiResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHumanTaskUi)

responseListCodeRepositories :: ListCodeRepositoriesResponse -> TestTree
responseListCodeRepositories =
  res
    "ListCodeRepositoriesResponse"
    "fixture/ListCodeRepositoriesResponse.proto"
    defaultService
    (Proxy :: Proxy ListCodeRepositories)

responseDescribeAction :: DescribeActionResponse -> TestTree
responseDescribeAction =
  res
    "DescribeActionResponse"
    "fixture/DescribeActionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAction)

responseUpdatePipeline :: UpdatePipelineResponse -> TestTree
responseUpdatePipeline =
  res
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePipeline)

responseDescribeArtifact :: DescribeArtifactResponse -> TestTree
responseDescribeArtifact =
  res
    "DescribeArtifactResponse"
    "fixture/DescribeArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeArtifact)

responseDeleteUserProfile :: DeleteUserProfileResponse -> TestTree
responseDeleteUserProfile =
  res
    "DeleteUserProfileResponse"
    "fixture/DeleteUserProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserProfile)

responseStopTransformJob :: StopTransformJobResponse -> TestTree
responseStopTransformJob =
  res
    "StopTransformJobResponse"
    "fixture/StopTransformJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopTransformJob)

responseCreateDataQualityJobDefinition :: CreateDataQualityJobDefinitionResponse -> TestTree
responseCreateDataQualityJobDefinition =
  res
    "CreateDataQualityJobDefinitionResponse"
    "fixture/CreateDataQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDataQualityJobDefinition)

responseDeleteModelPackageGroupPolicy :: DeleteModelPackageGroupPolicyResponse -> TestTree
responseDeleteModelPackageGroupPolicy =
  res
    "DeleteModelPackageGroupPolicyResponse"
    "fixture/DeleteModelPackageGroupPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteModelPackageGroupPolicy)

responseCreateImage :: CreateImageResponse -> TestTree
responseCreateImage =
  res
    "CreateImageResponse"
    "fixture/CreateImageResponse.proto"
    defaultService
    (Proxy :: Proxy CreateImage)

responseDescribeModelPackage :: DescribeModelPackageResponse -> TestTree
responseDescribeModelPackage =
  res
    "DescribeModelPackageResponse"
    "fixture/DescribeModelPackageResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeModelPackage)

responseRetryPipelineExecution :: RetryPipelineExecutionResponse -> TestTree
responseRetryPipelineExecution =
  res
    "RetryPipelineExecutionResponse"
    "fixture/RetryPipelineExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy RetryPipelineExecution)

responseDeleteEndpointConfig :: DeleteEndpointConfigResponse -> TestTree
responseDeleteEndpointConfig =
  res
    "DeleteEndpointConfigResponse"
    "fixture/DeleteEndpointConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEndpointConfig)

responseStopPipelineExecution :: StopPipelineExecutionResponse -> TestTree
responseStopPipelineExecution =
  res
    "StopPipelineExecutionResponse"
    "fixture/StopPipelineExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StopPipelineExecution)

responseListApps :: ListAppsResponse -> TestTree
responseListApps =
  res
    "ListAppsResponse"
    "fixture/ListAppsResponse.proto"
    defaultService
    (Proxy :: Proxy ListApps)

responseCreateWorkforce :: CreateWorkforceResponse -> TestTree
responseCreateWorkforce =
  res
    "CreateWorkforceResponse"
    "fixture/CreateWorkforceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateWorkforce)

responseCreateStudioLifecycleConfig :: CreateStudioLifecycleConfigResponse -> TestTree
responseCreateStudioLifecycleConfig =
  res
    "CreateStudioLifecycleConfigResponse"
    "fixture/CreateStudioLifecycleConfigResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStudioLifecycleConfig)

responseListAutoMLJobs :: ListAutoMLJobsResponse -> TestTree
responseListAutoMLJobs =
  res
    "ListAutoMLJobsResponse"
    "fixture/ListAutoMLJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAutoMLJobs)

responseUpdateEndpointWeightsAndCapacities :: UpdateEndpointWeightsAndCapacitiesResponse -> TestTree
responseUpdateEndpointWeightsAndCapacities =
  res
    "UpdateEndpointWeightsAndCapacitiesResponse"
    "fixture/UpdateEndpointWeightsAndCapacitiesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEndpointWeightsAndCapacities)

responseStartNotebookInstance :: StartNotebookInstanceResponse -> TestTree
responseStartNotebookInstance =
  res
    "StartNotebookInstanceResponse"
    "fixture/StartNotebookInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy StartNotebookInstance)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTags)

responseCreateAppImageConfig :: CreateAppImageConfigResponse -> TestTree
responseCreateAppImageConfig =
  res
    "CreateAppImageConfigResponse"
    "fixture/CreateAppImageConfigResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAppImageConfig)

responseDeleteAssociation :: DeleteAssociationResponse -> TestTree
responseDeleteAssociation =
  res
    "DeleteAssociationResponse"
    "fixture/DeleteAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAssociation)

responseListMonitoringSchedules :: ListMonitoringSchedulesResponse -> TestTree
responseListMonitoringSchedules =
  res
    "ListMonitoringSchedulesResponse"
    "fixture/ListMonitoringSchedulesResponse.proto"
    defaultService
    (Proxy :: Proxy ListMonitoringSchedules)

responseDeleteMonitoringSchedule :: DeleteMonitoringScheduleResponse -> TestTree
responseDeleteMonitoringSchedule =
  res
    "DeleteMonitoringScheduleResponse"
    "fixture/DeleteMonitoringScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMonitoringSchedule)

responseListEndpointConfigs :: ListEndpointConfigsResponse -> TestTree
responseListEndpointConfigs =
  res
    "ListEndpointConfigsResponse"
    "fixture/ListEndpointConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListEndpointConfigs)

responseStartPipelineExecution :: StartPipelineExecutionResponse -> TestTree
responseStartPipelineExecution =
  res
    "StartPipelineExecutionResponse"
    "fixture/StartPipelineExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StartPipelineExecution)

responseStopNotebookInstance :: StopNotebookInstanceResponse -> TestTree
responseStopNotebookInstance =
  res
    "StopNotebookInstanceResponse"
    "fixture/StopNotebookInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy StopNotebookInstance)

responseUpdateMonitoringSchedule :: UpdateMonitoringScheduleResponse -> TestTree
responseUpdateMonitoringSchedule =
  res
    "UpdateMonitoringScheduleResponse"
    "fixture/UpdateMonitoringScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMonitoringSchedule)

responseAddAssociation :: AddAssociationResponse -> TestTree
responseAddAssociation =
  res
    "AddAssociationResponse"
    "fixture/AddAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy AddAssociation)

responseCreateNotebookInstanceLifecycleConfig :: CreateNotebookInstanceLifecycleConfigResponse -> TestTree
responseCreateNotebookInstanceLifecycleConfig =
  res
    "CreateNotebookInstanceLifecycleConfigResponse"
    "fixture/CreateNotebookInstanceLifecycleConfigResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNotebookInstanceLifecycleConfig)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    defaultService
    (Proxy :: Proxy AddTags)

responseDisableSagemakerServicecatalogPortfolio :: DisableSagemakerServicecatalogPortfolioResponse -> TestTree
responseDisableSagemakerServicecatalogPortfolio =
  res
    "DisableSagemakerServicecatalogPortfolioResponse"
    "fixture/DisableSagemakerServicecatalogPortfolioResponse.proto"
    defaultService
    (Proxy :: Proxy DisableSagemakerServicecatalogPortfolio)

responseUpdateNotebookInstanceLifecycleConfig :: UpdateNotebookInstanceLifecycleConfigResponse -> TestTree
responseUpdateNotebookInstanceLifecycleConfig =
  res
    "UpdateNotebookInstanceLifecycleConfigResponse"
    "fixture/UpdateNotebookInstanceLifecycleConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateNotebookInstanceLifecycleConfig)

responseDescribeDomain :: DescribeDomainResponse -> TestTree
responseDescribeDomain =
  res
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDomain)

responseListStudioLifecycleConfigs :: ListStudioLifecycleConfigsResponse -> TestTree
responseListStudioLifecycleConfigs =
  res
    "ListStudioLifecycleConfigsResponse"
    "fixture/ListStudioLifecycleConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListStudioLifecycleConfigs)

responseListAppImageConfigs :: ListAppImageConfigsResponse -> TestTree
responseListAppImageConfigs =
  res
    "ListAppImageConfigsResponse"
    "fixture/ListAppImageConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAppImageConfigs)

responseDescribeModelQualityJobDefinition :: DescribeModelQualityJobDefinitionResponse -> TestTree
responseDescribeModelQualityJobDefinition =
  res
    "DescribeModelQualityJobDefinitionResponse"
    "fixture/DescribeModelQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeModelQualityJobDefinition)

responseDescribeDevice :: DescribeDeviceResponse -> TestTree
responseDescribeDevice =
  res
    "DescribeDeviceResponse"
    "fixture/DescribeDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDevice)

responseListNotebookInstanceLifecycleConfigs :: ListNotebookInstanceLifecycleConfigsResponse -> TestTree
responseListNotebookInstanceLifecycleConfigs =
  res
    "ListNotebookInstanceLifecycleConfigsResponse"
    "fixture/ListNotebookInstanceLifecycleConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListNotebookInstanceLifecycleConfigs)

responseListWorkforces :: ListWorkforcesResponse -> TestTree
responseListWorkforces =
  res
    "ListWorkforcesResponse"
    "fixture/ListWorkforcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListWorkforces)

responseDeleteStudioLifecycleConfig :: DeleteStudioLifecycleConfigResponse -> TestTree
responseDeleteStudioLifecycleConfig =
  res
    "DeleteStudioLifecycleConfigResponse"
    "fixture/DeleteStudioLifecycleConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStudioLifecycleConfig)

responseCreateLabelingJob :: CreateLabelingJobResponse -> TestTree
responseCreateLabelingJob =
  res
    "CreateLabelingJobResponse"
    "fixture/CreateLabelingJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLabelingJob)

responseDeleteModelBiasJobDefinition :: DeleteModelBiasJobDefinitionResponse -> TestTree
responseDeleteModelBiasJobDefinition =
  res
    "DeleteModelBiasJobDefinitionResponse"
    "fixture/DeleteModelBiasJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteModelBiasJobDefinition)

responseCreateExperiment :: CreateExperimentResponse -> TestTree
responseCreateExperiment =
  res
    "CreateExperimentResponse"
    "fixture/CreateExperimentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateExperiment)

responseDescribePipelineExecution :: DescribePipelineExecutionResponse -> TestTree
responseDescribePipelineExecution =
  res
    "DescribePipelineExecutionResponse"
    "fixture/DescribePipelineExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePipelineExecution)

responseDeleteWorkforce :: DeleteWorkforceResponse -> TestTree
responseDeleteWorkforce =
  res
    "DeleteWorkforceResponse"
    "fixture/DeleteWorkforceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWorkforce)

responseUpdateWorkforce :: UpdateWorkforceResponse -> TestTree
responseUpdateWorkforce =
  res
    "UpdateWorkforceResponse"
    "fixture/UpdateWorkforceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateWorkforce)

responseUpdateWorkteam :: UpdateWorkteamResponse -> TestTree
responseUpdateWorkteam =
  res
    "UpdateWorkteamResponse"
    "fixture/UpdateWorkteamResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateWorkteam)

responseDescribeSubscribedWorkteam :: DescribeSubscribedWorkteamResponse -> TestTree
responseDescribeSubscribedWorkteam =
  res
    "DescribeSubscribedWorkteamResponse"
    "fixture/DescribeSubscribedWorkteamResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSubscribedWorkteam)

responseDeleteWorkteam :: DeleteWorkteamResponse -> TestTree
responseDeleteWorkteam =
  res
    "DeleteWorkteamResponse"
    "fixture/DeleteWorkteamResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWorkteam)

responseDeleteNotebookInstanceLifecycleConfig :: DeleteNotebookInstanceLifecycleConfigResponse -> TestTree
responseDeleteNotebookInstanceLifecycleConfig =
  res
    "DeleteNotebookInstanceLifecycleConfigResponse"
    "fixture/DeleteNotebookInstanceLifecycleConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNotebookInstanceLifecycleConfig)

responseListEdgePackagingJobs :: ListEdgePackagingJobsResponse -> TestTree
responseListEdgePackagingJobs =
  res
    "ListEdgePackagingJobsResponse"
    "fixture/ListEdgePackagingJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListEdgePackagingJobs)

responseCreateCompilationJob :: CreateCompilationJobResponse -> TestTree
responseCreateCompilationJob =
  res
    "CreateCompilationJobResponse"
    "fixture/CreateCompilationJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCompilationJob)

responseCreateAction :: CreateActionResponse -> TestTree
responseCreateAction =
  res
    "CreateActionResponse"
    "fixture/CreateActionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAction)

responseCreateArtifact :: CreateArtifactResponse -> TestTree
responseCreateArtifact =
  res
    "CreateArtifactResponse"
    "fixture/CreateArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy CreateArtifact)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEndpoint)

responseListTransformJobs :: ListTransformJobsResponse -> TestTree
responseListTransformJobs =
  res
    "ListTransformJobsResponse"
    "fixture/ListTransformJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTransformJobs)

responseDescribeCodeRepository :: DescribeCodeRepositoryResponse -> TestTree
responseDescribeCodeRepository =
  res
    "DescribeCodeRepositoryResponse"
    "fixture/DescribeCodeRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCodeRepository)

responseCreateDeviceFleet :: CreateDeviceFleetResponse -> TestTree
responseCreateDeviceFleet =
  res
    "CreateDeviceFleetResponse"
    "fixture/CreateDeviceFleetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDeviceFleet)

responseDescribeModel :: DescribeModelResponse -> TestTree
responseDescribeModel =
  res
    "DescribeModelResponse"
    "fixture/DescribeModelResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeModel)

responseDescribeDataQualityJobDefinition :: DescribeDataQualityJobDefinitionResponse -> TestTree
responseDescribeDataQualityJobDefinition =
  res
    "DescribeDataQualityJobDefinitionResponse"
    "fixture/DescribeDataQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDataQualityJobDefinition)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint =
  res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEndpoint)

responseCreatePresignedDomainUrl :: CreatePresignedDomainUrlResponse -> TestTree
responseCreatePresignedDomainUrl =
  res
    "CreatePresignedDomainUrlResponse"
    "fixture/CreatePresignedDomainUrlResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePresignedDomainUrl)

responseDescribeHyperParameterTuningJob :: DescribeHyperParameterTuningJobResponse -> TestTree
responseDescribeHyperParameterTuningJob =
  res
    "DescribeHyperParameterTuningJobResponse"
    "fixture/DescribeHyperParameterTuningJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHyperParameterTuningJob)

responseListEndpoints :: ListEndpointsResponse -> TestTree
responseListEndpoints =
  res
    "ListEndpointsResponse"
    "fixture/ListEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy ListEndpoints)

responseDescribeAlgorithm :: DescribeAlgorithmResponse -> TestTree
responseDescribeAlgorithm =
  res
    "DescribeAlgorithmResponse"
    "fixture/DescribeAlgorithmResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAlgorithm)

responseUpdateDevices :: UpdateDevicesResponse -> TestTree
responseUpdateDevices =
  res
    "UpdateDevicesResponse"
    "fixture/UpdateDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDevices)

responseListDeviceFleets :: ListDeviceFleetsResponse -> TestTree
responseListDeviceFleets =
  res
    "ListDeviceFleetsResponse"
    "fixture/ListDeviceFleetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeviceFleets)

responseListLabelingJobsForWorkteam :: ListLabelingJobsForWorkteamResponse -> TestTree
responseListLabelingJobsForWorkteam =
  res
    "ListLabelingJobsForWorkteamResponse"
    "fixture/ListLabelingJobsForWorkteamResponse.proto"
    defaultService
    (Proxy :: Proxy ListLabelingJobsForWorkteam)

responseCreateFeatureGroup :: CreateFeatureGroupResponse -> TestTree
responseCreateFeatureGroup =
  res
    "CreateFeatureGroupResponse"
    "fixture/CreateFeatureGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFeatureGroup)

responseListMonitoringExecutions :: ListMonitoringExecutionsResponse -> TestTree
responseListMonitoringExecutions =
  res
    "ListMonitoringExecutionsResponse"
    "fixture/ListMonitoringExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMonitoringExecutions)

responseDescribeModelPackageGroup :: DescribeModelPackageGroupResponse -> TestTree
responseDescribeModelPackageGroup =
  res
    "DescribeModelPackageGroupResponse"
    "fixture/DescribeModelPackageGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeModelPackageGroup)

responseUpdateDeviceFleet :: UpdateDeviceFleetResponse -> TestTree
responseUpdateDeviceFleet =
  res
    "UpdateDeviceFleetResponse"
    "fixture/UpdateDeviceFleetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDeviceFleet)

responseStopHyperParameterTuningJob :: StopHyperParameterTuningJobResponse -> TestTree
responseStopHyperParameterTuningJob =
  res
    "StopHyperParameterTuningJobResponse"
    "fixture/StopHyperParameterTuningJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopHyperParameterTuningJob)

responseDeleteDeviceFleet :: DeleteDeviceFleetResponse -> TestTree
responseDeleteDeviceFleet =
  res
    "DeleteDeviceFleetResponse"
    "fixture/DeleteDeviceFleetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDeviceFleet)

responseListActions :: ListActionsResponse -> TestTree
responseListActions =
  res
    "ListActionsResponse"
    "fixture/ListActionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListActions)

responseListArtifacts :: ListArtifactsResponse -> TestTree
responseListArtifacts =
  res
    "ListArtifactsResponse"
    "fixture/ListArtifactsResponse.proto"
    defaultService
    (Proxy :: Proxy ListArtifacts)

responseListCompilationJobs :: ListCompilationJobsResponse -> TestTree
responseListCompilationJobs =
  res
    "ListCompilationJobsResponse"
    "fixture/ListCompilationJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListCompilationJobs)

responseDescribeTrial :: DescribeTrialResponse -> TestTree
responseDescribeTrial =
  res
    "DescribeTrialResponse"
    "fixture/DescribeTrialResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTrial)

responseDeleteImageVersion :: DeleteImageVersionResponse -> TestTree
responseDeleteImageVersion =
  res
    "DeleteImageVersionResponse"
    "fixture/DeleteImageVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteImageVersion)

responseDeleteTrialComponent :: DeleteTrialComponentResponse -> TestTree
responseDeleteTrialComponent =
  res
    "DeleteTrialComponentResponse"
    "fixture/DeleteTrialComponentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTrialComponent)

responseListTrialComponents :: ListTrialComponentsResponse -> TestTree
responseListTrialComponents =
  res
    "ListTrialComponentsResponse"
    "fixture/ListTrialComponentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTrialComponents)

responseDescribeAutoMLJob :: DescribeAutoMLJobResponse -> TestTree
responseDescribeAutoMLJob =
  res
    "DescribeAutoMLJobResponse"
    "fixture/DescribeAutoMLJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAutoMLJob)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProject)

responseDescribeApp :: DescribeAppResponse -> TestTree
responseDescribeApp =
  res
    "DescribeAppResponse"
    "fixture/DescribeAppResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeApp)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDomain)

responseDescribeExperiment :: DescribeExperimentResponse -> TestTree
responseDescribeExperiment =
  res
    "DescribeExperimentResponse"
    "fixture/DescribeExperimentResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeExperiment)

responseListImageVersions :: ListImageVersionsResponse -> TestTree
responseListImageVersions =
  res
    "ListImageVersionsResponse"
    "fixture/ListImageVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListImageVersions)

responseStopProcessingJob :: StopProcessingJobResponse -> TestTree
responseStopProcessingJob =
  res
    "StopProcessingJobResponse"
    "fixture/StopProcessingJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopProcessingJob)

responseUpdateTrialComponent :: UpdateTrialComponentResponse -> TestTree
responseUpdateTrialComponent =
  res
    "UpdateTrialComponentResponse"
    "fixture/UpdateTrialComponentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTrialComponent)

responseUpdatePipelineExecution :: UpdatePipelineExecutionResponse -> TestTree
responseUpdatePipelineExecution =
  res
    "UpdatePipelineExecutionResponse"
    "fixture/UpdatePipelineExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePipelineExecution)

responseCreateTrialComponent :: CreateTrialComponentResponse -> TestTree
responseCreateTrialComponent =
  res
    "CreateTrialComponentResponse"
    "fixture/CreateTrialComponentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTrialComponent)

responseListPipelineExecutions :: ListPipelineExecutionsResponse -> TestTree
responseListPipelineExecutions =
  res
    "ListPipelineExecutionsResponse"
    "fixture/ListPipelineExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPipelineExecutions)

responseListModelExplainabilityJobDefinitions :: ListModelExplainabilityJobDefinitionsResponse -> TestTree
responseListModelExplainabilityJobDefinitions =
  res
    "ListModelExplainabilityJobDefinitionsResponse"
    "fixture/ListModelExplainabilityJobDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListModelExplainabilityJobDefinitions)

responseAssociateTrialComponent :: AssociateTrialComponentResponse -> TestTree
responseAssociateTrialComponent =
  res
    "AssociateTrialComponentResponse"
    "fixture/AssociateTrialComponentResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateTrialComponent)

responseDescribeModelBiasJobDefinition :: DescribeModelBiasJobDefinitionResponse -> TestTree
responseDescribeModelBiasJobDefinition =
  res
    "DescribeModelBiasJobDefinitionResponse"
    "fixture/DescribeModelBiasJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeModelBiasJobDefinition)

responseDescribeStudioLifecycleConfig :: DescribeStudioLifecycleConfigResponse -> TestTree
responseDescribeStudioLifecycleConfig =
  res
    "DescribeStudioLifecycleConfigResponse"
    "fixture/DescribeStudioLifecycleConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStudioLifecycleConfig)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDomain)

responseListNotebookInstances :: ListNotebookInstancesResponse -> TestTree
responseListNotebookInstances =
  res
    "ListNotebookInstancesResponse"
    "fixture/ListNotebookInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy ListNotebookInstances)

responseDescribeNotebookInstanceLifecycleConfig :: DescribeNotebookInstanceLifecycleConfigResponse -> TestTree
responseDescribeNotebookInstanceLifecycleConfig =
  res
    "DescribeNotebookInstanceLifecycleConfigResponse"
    "fixture/DescribeNotebookInstanceLifecycleConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNotebookInstanceLifecycleConfig)

responseStopLabelingJob :: StopLabelingJobResponse -> TestTree
responseStopLabelingJob =
  res
    "StopLabelingJobResponse"
    "fixture/StopLabelingJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopLabelingJob)

responseUpdateDomain :: UpdateDomainResponse -> TestTree
responseUpdateDomain =
  res
    "UpdateDomainResponse"
    "fixture/UpdateDomainResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDomain)

responseDescribeWorkforce :: DescribeWorkforceResponse -> TestTree
responseDescribeWorkforce =
  res
    "DescribeWorkforceResponse"
    "fixture/DescribeWorkforceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkforce)

responseCreateImageVersion :: CreateImageVersionResponse -> TestTree
responseCreateImageVersion =
  res
    "CreateImageVersionResponse"
    "fixture/CreateImageVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateImageVersion)

responseDeleteModelQualityJobDefinition :: DeleteModelQualityJobDefinitionResponse -> TestTree
responseDeleteModelQualityJobDefinition =
  res
    "DeleteModelQualityJobDefinitionResponse"
    "fixture/DeleteModelQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteModelQualityJobDefinition)

responseDescribeWorkteam :: DescribeWorkteamResponse -> TestTree
responseDescribeWorkteam =
  res
    "DescribeWorkteamResponse"
    "fixture/DescribeWorkteamResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkteam)

responseListModelQualityJobDefinitions :: ListModelQualityJobDefinitionsResponse -> TestTree
responseListModelQualityJobDefinitions =
  res
    "ListModelQualityJobDefinitionsResponse"
    "fixture/ListModelQualityJobDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListModelQualityJobDefinitions)

responseUpdateContext :: UpdateContextResponse -> TestTree
responseUpdateContext =
  res
    "UpdateContextResponse"
    "fixture/UpdateContextResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateContext)

responseListModels :: ListModelsResponse -> TestTree
responseListModels =
  res
    "ListModelsResponse"
    "fixture/ListModelsResponse.proto"
    defaultService
    (Proxy :: Proxy ListModels)

responseListHyperParameterTuningJobs :: ListHyperParameterTuningJobsResponse -> TestTree
responseListHyperParameterTuningJobs =
  res
    "ListHyperParameterTuningJobsResponse"
    "fixture/ListHyperParameterTuningJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListHyperParameterTuningJobs)

responseSendPipelineExecutionStepFailure :: SendPipelineExecutionStepFailureResponse -> TestTree
responseSendPipelineExecutionStepFailure =
  res
    "SendPipelineExecutionStepFailureResponse"
    "fixture/SendPipelineExecutionStepFailureResponse.proto"
    defaultService
    (Proxy :: Proxy SendPipelineExecutionStepFailure)

responseDescribeFeatureGroup :: DescribeFeatureGroupResponse -> TestTree
responseDescribeFeatureGroup =
  res
    "DescribeFeatureGroupResponse"
    "fixture/DescribeFeatureGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFeatureGroup)

responseUpdateImage :: UpdateImageResponse -> TestTree
responseUpdateImage =
  res
    "UpdateImageResponse"
    "fixture/UpdateImageResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateImage)

responseListFlowDefinitions :: ListFlowDefinitionsResponse -> TestTree
responseListFlowDefinitions =
  res
    "ListFlowDefinitionsResponse"
    "fixture/ListFlowDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListFlowDefinitions)

responseListAlgorithms :: ListAlgorithmsResponse -> TestTree
responseListAlgorithms =
  res
    "ListAlgorithmsResponse"
    "fixture/ListAlgorithmsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAlgorithms)

responseUpdateTrainingJob :: UpdateTrainingJobResponse -> TestTree
responseUpdateTrainingJob =
  res
    "UpdateTrainingJobResponse"
    "fixture/UpdateTrainingJobResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTrainingJob)

responseDeleteFlowDefinition :: DeleteFlowDefinitionResponse -> TestTree
responseDeleteFlowDefinition =
  res
    "DeleteFlowDefinitionResponse"
    "fixture/DeleteFlowDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFlowDefinition)

responseDeleteImage :: DeleteImageResponse -> TestTree
responseDeleteImage =
  res
    "DeleteImageResponse"
    "fixture/DeleteImageResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteImage)

responseRenderUiTemplate :: RenderUiTemplateResponse -> TestTree
responseRenderUiTemplate =
  res
    "RenderUiTemplateResponse"
    "fixture/RenderUiTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy RenderUiTemplate)

responseCreateUserProfile :: CreateUserProfileResponse -> TestTree
responseCreateUserProfile =
  res
    "CreateUserProfileResponse"
    "fixture/CreateUserProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUserProfile)

responseDeleteContext :: DeleteContextResponse -> TestTree
responseDeleteContext =
  res
    "DeleteContextResponse"
    "fixture/DeleteContextResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteContext)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTags)

responsePutModelPackageGroupPolicy :: PutModelPackageGroupPolicyResponse -> TestTree
responsePutModelPackageGroupPolicy =
  res
    "PutModelPackageGroupPolicyResponse"
    "fixture/PutModelPackageGroupPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutModelPackageGroupPolicy)

responseListTrials :: ListTrialsResponse -> TestTree
responseListTrials =
  res
    "ListTrialsResponse"
    "fixture/ListTrialsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTrials)

responseDescribePipelineDefinitionForExecution :: DescribePipelineDefinitionForExecutionResponse -> TestTree
responseDescribePipelineDefinitionForExecution =
  res
    "DescribePipelineDefinitionForExecutionResponse"
    "fixture/DescribePipelineDefinitionForExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePipelineDefinitionForExecution)

responseListModelPackageGroups :: ListModelPackageGroupsResponse -> TestTree
responseListModelPackageGroups =
  res
    "ListModelPackageGroupsResponse"
    "fixture/ListModelPackageGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListModelPackageGroups)

responseListPipelines :: ListPipelinesResponse -> TestTree
responseListPipelines =
  res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    defaultService
    (Proxy :: Proxy ListPipelines)

responseListPipelineParametersForExecution :: ListPipelineParametersForExecutionResponse -> TestTree
responseListPipelineParametersForExecution =
  res
    "ListPipelineParametersForExecutionResponse"
    "fixture/ListPipelineParametersForExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy ListPipelineParametersForExecution)

responseDeleteTrial :: DeleteTrialResponse -> TestTree
responseDeleteTrial =
  res
    "DeleteTrialResponse"
    "fixture/DeleteTrialResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTrial)

responseCreateAlgorithm :: CreateAlgorithmResponse -> TestTree
responseCreateAlgorithm =
  res
    "CreateAlgorithmResponse"
    "fixture/CreateAlgorithmResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAlgorithm)

responseUpdateTrial :: UpdateTrialResponse -> TestTree
responseUpdateTrial =
  res
    "UpdateTrialResponse"
    "fixture/UpdateTrialResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTrial)

responseDeleteModelPackageGroup :: DeleteModelPackageGroupResponse -> TestTree
responseDeleteModelPackageGroup =
  res
    "DeleteModelPackageGroupResponse"
    "fixture/DeleteModelPackageGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteModelPackageGroup)

responseDescribeDeviceFleet :: DescribeDeviceFleetResponse -> TestTree
responseDescribeDeviceFleet =
  res
    "DescribeDeviceFleetResponse"
    "fixture/DescribeDeviceFleetResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDeviceFleet)

responseCreateHyperParameterTuningJob :: CreateHyperParameterTuningJobResponse -> TestTree
responseCreateHyperParameterTuningJob =
  res
    "CreateHyperParameterTuningJobResponse"
    "fixture/CreateHyperParameterTuningJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateHyperParameterTuningJob)

responseCreateModel :: CreateModelResponse -> TestTree
responseCreateModel =
  res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    defaultService
    (Proxy :: Proxy CreateModel)

responseCreateFlowDefinition :: CreateFlowDefinitionResponse -> TestTree
responseCreateFlowDefinition =
  res
    "CreateFlowDefinitionResponse"
    "fixture/CreateFlowDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFlowDefinition)

responseListAssociations :: ListAssociationsResponse -> TestTree
responseListAssociations =
  res
    "ListAssociationsResponse"
    "fixture/ListAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssociations)

responseDeleteExperiment :: DeleteExperimentResponse -> TestTree
responseDeleteExperiment =
  res
    "DeleteExperimentResponse"
    "fixture/DeleteExperimentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteExperiment)

responseDescribeProject :: DescribeProjectResponse -> TestTree
responseDescribeProject =
  res
    "DescribeProjectResponse"
    "fixture/DescribeProjectResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProject)

responseDescribeImageVersion :: DescribeImageVersionResponse -> TestTree
responseDescribeImageVersion =
  res
    "DescribeImageVersionResponse"
    "fixture/DescribeImageVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImageVersion)

responseListExperiments :: ListExperimentsResponse -> TestTree
responseListExperiments =
  res
    "ListExperimentsResponse"
    "fixture/ListExperimentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListExperiments)

responseCreateModelBiasJobDefinition :: CreateModelBiasJobDefinitionResponse -> TestTree
responseCreateModelBiasJobDefinition =
  res
    "CreateModelBiasJobDefinitionResponse"
    "fixture/CreateModelBiasJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateModelBiasJobDefinition)

responseUpdateExperiment :: UpdateExperimentResponse -> TestTree
responseUpdateExperiment =
  res
    "UpdateExperimentResponse"
    "fixture/UpdateExperimentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateExperiment)

responseDescribeTrialComponent :: DescribeTrialComponentResponse -> TestTree
responseDescribeTrialComponent =
  res
    "DescribeTrialComponentResponse"
    "fixture/DescribeTrialComponentResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTrialComponent)

responseCreateWorkteam :: CreateWorkteamResponse -> TestTree
responseCreateWorkteam =
  res
    "CreateWorkteamResponse"
    "fixture/CreateWorkteamResponse.proto"
    defaultService
    (Proxy :: Proxy CreateWorkteam)

responseCreateProcessingJob :: CreateProcessingJobResponse -> TestTree
responseCreateProcessingJob =
  res
    "CreateProcessingJobResponse"
    "fixture/CreateProcessingJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProcessingJob)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApp)

responseListLabelingJobs :: ListLabelingJobsResponse -> TestTree
responseListLabelingJobs =
  res
    "ListLabelingJobsResponse"
    "fixture/ListLabelingJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListLabelingJobs)

responseListWorkteams :: ListWorkteamsResponse -> TestTree
responseListWorkteams =
  res
    "ListWorkteamsResponse"
    "fixture/ListWorkteamsResponse.proto"
    defaultService
    (Proxy :: Proxy ListWorkteams)

responseDeleteAppImageConfig :: DeleteAppImageConfigResponse -> TestTree
responseDeleteAppImageConfig =
  res
    "DeleteAppImageConfigResponse"
    "fixture/DeleteAppImageConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAppImageConfig)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApp)

responseEnableSagemakerServicecatalogPortfolio :: EnableSagemakerServicecatalogPortfolioResponse -> TestTree
responseEnableSagemakerServicecatalogPortfolio =
  res
    "EnableSagemakerServicecatalogPortfolioResponse"
    "fixture/EnableSagemakerServicecatalogPortfolioResponse.proto"
    defaultService
    (Proxy :: Proxy EnableSagemakerServicecatalogPortfolio)

responseDescribeNotebookInstance :: DescribeNotebookInstanceResponse -> TestTree
responseDescribeNotebookInstance =
  res
    "DescribeNotebookInstanceResponse"
    "fixture/DescribeNotebookInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNotebookInstance)

responseDescribeModelExplainabilityJobDefinition :: DescribeModelExplainabilityJobDefinitionResponse -> TestTree
responseDescribeModelExplainabilityJobDefinition =
  res
    "DescribeModelExplainabilityJobDefinitionResponse"
    "fixture/DescribeModelExplainabilityJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeModelExplainabilityJobDefinition)

responseCreateAutoMLJob :: CreateAutoMLJobResponse -> TestTree
responseCreateAutoMLJob =
  res
    "CreateAutoMLJobResponse"
    "fixture/CreateAutoMLJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAutoMLJob)

responseCreateEndpointConfig :: CreateEndpointConfigResponse -> TestTree
responseCreateEndpointConfig =
  res
    "CreateEndpointConfigResponse"
    "fixture/CreateEndpointConfigResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEndpointConfig)

responseListModelBiasJobDefinitions :: ListModelBiasJobDefinitionsResponse -> TestTree
responseListModelBiasJobDefinitions =
  res
    "ListModelBiasJobDefinitionsResponse"
    "fixture/ListModelBiasJobDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListModelBiasJobDefinitions)

responseSendPipelineExecutionStepSuccess :: SendPipelineExecutionStepSuccessResponse -> TestTree
responseSendPipelineExecutionStepSuccess =
  res
    "SendPipelineExecutionStepSuccessResponse"
    "fixture/SendPipelineExecutionStepSuccessResponse.proto"
    defaultService
    (Proxy :: Proxy SendPipelineExecutionStepSuccess)

responseCreateMonitoringSchedule :: CreateMonitoringScheduleResponse -> TestTree
responseCreateMonitoringSchedule =
  res
    "CreateMonitoringScheduleResponse"
    "fixture/CreateMonitoringScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMonitoringSchedule)

responseListProcessingJobs :: ListProcessingJobsResponse -> TestTree
responseListProcessingJobs =
  res
    "ListProcessingJobsResponse"
    "fixture/ListProcessingJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListProcessingJobs)

responseUpdateAppImageConfig :: UpdateAppImageConfigResponse -> TestTree
responseUpdateAppImageConfig =
  res
    "UpdateAppImageConfigResponse"
    "fixture/UpdateAppImageConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAppImageConfig)

responseDescribeContext :: DescribeContextResponse -> TestTree
responseDescribeContext =
  res
    "DescribeContextResponse"
    "fixture/DescribeContextResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeContext)

responseCreateHumanTaskUi :: CreateHumanTaskUiResponse -> TestTree
responseCreateHumanTaskUi =
  res
    "CreateHumanTaskUiResponse"
    "fixture/CreateHumanTaskUiResponse.proto"
    defaultService
    (Proxy :: Proxy CreateHumanTaskUi)

responseDeleteFeatureGroup :: DeleteFeatureGroupResponse -> TestTree
responseDeleteFeatureGroup =
  res
    "DeleteFeatureGroupResponse"
    "fixture/DeleteFeatureGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFeatureGroup)

responseDescribeTrainingJob :: DescribeTrainingJobResponse -> TestTree
responseDescribeTrainingJob =
  res
    "DescribeTrainingJobResponse"
    "fixture/DescribeTrainingJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTrainingJob)

responseDescribeFlowDefinition :: DescribeFlowDefinitionResponse -> TestTree
responseDescribeFlowDefinition =
  res
    "DescribeFlowDefinitionResponse"
    "fixture/DescribeFlowDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFlowDefinition)

responseListTrainingJobsForHyperParameterTuningJob :: ListTrainingJobsForHyperParameterTuningJobResponse -> TestTree
responseListTrainingJobsForHyperParameterTuningJob =
  res
    "ListTrainingJobsForHyperParameterTuningJobResponse"
    "fixture/ListTrainingJobsForHyperParameterTuningJobResponse.proto"
    defaultService
    (Proxy :: Proxy ListTrainingJobsForHyperParameterTuningJob)

responseCreatePresignedNotebookInstanceUrl :: CreatePresignedNotebookInstanceUrlResponse -> TestTree
responseCreatePresignedNotebookInstanceUrl =
  res
    "CreatePresignedNotebookInstanceUrlResponse"
    "fixture/CreatePresignedNotebookInstanceUrlResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePresignedNotebookInstanceUrl)

responseRegisterDevices :: RegisterDevicesResponse -> TestTree
responseRegisterDevices =
  res
    "RegisterDevicesResponse"
    "fixture/RegisterDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterDevices)

responseListFeatureGroups :: ListFeatureGroupsResponse -> TestTree
responseListFeatureGroups =
  res
    "ListFeatureGroupsResponse"
    "fixture/ListFeatureGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListFeatureGroups)

responseDescribeImage :: DescribeImageResponse -> TestTree
responseDescribeImage =
  res
    "DescribeImageResponse"
    "fixture/DescribeImageResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImage)

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SageMaker
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SageMaker where

import Amazonka.SageMaker
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SageMaker.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddAssociation $
--             newAddAssociation
--
--         , requestAddTags $
--             newAddTags
--
--         , requestAssociateTrialComponent $
--             newAssociateTrialComponent
--
--         , requestBatchDescribeModelPackage $
--             newBatchDescribeModelPackage
--
--         , requestCreateAction $
--             newCreateAction
--
--         , requestCreateAlgorithm $
--             newCreateAlgorithm
--
--         , requestCreateApp $
--             newCreateApp
--
--         , requestCreateAppImageConfig $
--             newCreateAppImageConfig
--
--         , requestCreateArtifact $
--             newCreateArtifact
--
--         , requestCreateAutoMLJob $
--             newCreateAutoMLJob
--
--         , requestCreateCodeRepository $
--             newCreateCodeRepository
--
--         , requestCreateCompilationJob $
--             newCreateCompilationJob
--
--         , requestCreateContext $
--             newCreateContext
--
--         , requestCreateDataQualityJobDefinition $
--             newCreateDataQualityJobDefinition
--
--         , requestCreateDeviceFleet $
--             newCreateDeviceFleet
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestCreateEdgeDeploymentPlan $
--             newCreateEdgeDeploymentPlan
--
--         , requestCreateEdgeDeploymentStage $
--             newCreateEdgeDeploymentStage
--
--         , requestCreateEdgePackagingJob $
--             newCreateEdgePackagingJob
--
--         , requestCreateEndpoint $
--             newCreateEndpoint
--
--         , requestCreateEndpointConfig $
--             newCreateEndpointConfig
--
--         , requestCreateExperiment $
--             newCreateExperiment
--
--         , requestCreateFeatureGroup $
--             newCreateFeatureGroup
--
--         , requestCreateFlowDefinition $
--             newCreateFlowDefinition
--
--         , requestCreateHub $
--             newCreateHub
--
--         , requestCreateHumanTaskUi $
--             newCreateHumanTaskUi
--
--         , requestCreateHyperParameterTuningJob $
--             newCreateHyperParameterTuningJob
--
--         , requestCreateImage $
--             newCreateImage
--
--         , requestCreateImageVersion $
--             newCreateImageVersion
--
--         , requestCreateInferenceExperiment $
--             newCreateInferenceExperiment
--
--         , requestCreateInferenceRecommendationsJob $
--             newCreateInferenceRecommendationsJob
--
--         , requestCreateLabelingJob $
--             newCreateLabelingJob
--
--         , requestCreateModel $
--             newCreateModel
--
--         , requestCreateModelBiasJobDefinition $
--             newCreateModelBiasJobDefinition
--
--         , requestCreateModelCard $
--             newCreateModelCard
--
--         , requestCreateModelCardExportJob $
--             newCreateModelCardExportJob
--
--         , requestCreateModelExplainabilityJobDefinition $
--             newCreateModelExplainabilityJobDefinition
--
--         , requestCreateModelPackage $
--             newCreateModelPackage
--
--         , requestCreateModelPackageGroup $
--             newCreateModelPackageGroup
--
--         , requestCreateModelQualityJobDefinition $
--             newCreateModelQualityJobDefinition
--
--         , requestCreateMonitoringSchedule $
--             newCreateMonitoringSchedule
--
--         , requestCreateNotebookInstance $
--             newCreateNotebookInstance
--
--         , requestCreateNotebookInstanceLifecycleConfig $
--             newCreateNotebookInstanceLifecycleConfig
--
--         , requestCreatePipeline $
--             newCreatePipeline
--
--         , requestCreatePresignedDomainUrl $
--             newCreatePresignedDomainUrl
--
--         , requestCreatePresignedNotebookInstanceUrl $
--             newCreatePresignedNotebookInstanceUrl
--
--         , requestCreateProcessingJob $
--             newCreateProcessingJob
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestCreateSpace $
--             newCreateSpace
--
--         , requestCreateStudioLifecycleConfig $
--             newCreateStudioLifecycleConfig
--
--         , requestCreateTrainingJob $
--             newCreateTrainingJob
--
--         , requestCreateTransformJob $
--             newCreateTransformJob
--
--         , requestCreateTrial $
--             newCreateTrial
--
--         , requestCreateTrialComponent $
--             newCreateTrialComponent
--
--         , requestCreateUserProfile $
--             newCreateUserProfile
--
--         , requestCreateWorkforce $
--             newCreateWorkforce
--
--         , requestCreateWorkteam $
--             newCreateWorkteam
--
--         , requestDeleteAction $
--             newDeleteAction
--
--         , requestDeleteAlgorithm $
--             newDeleteAlgorithm
--
--         , requestDeleteApp $
--             newDeleteApp
--
--         , requestDeleteAppImageConfig $
--             newDeleteAppImageConfig
--
--         , requestDeleteArtifact $
--             newDeleteArtifact
--
--         , requestDeleteAssociation $
--             newDeleteAssociation
--
--         , requestDeleteCodeRepository $
--             newDeleteCodeRepository
--
--         , requestDeleteContext $
--             newDeleteContext
--
--         , requestDeleteDataQualityJobDefinition $
--             newDeleteDataQualityJobDefinition
--
--         , requestDeleteDeviceFleet $
--             newDeleteDeviceFleet
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestDeleteEdgeDeploymentPlan $
--             newDeleteEdgeDeploymentPlan
--
--         , requestDeleteEdgeDeploymentStage $
--             newDeleteEdgeDeploymentStage
--
--         , requestDeleteEndpoint $
--             newDeleteEndpoint
--
--         , requestDeleteEndpointConfig $
--             newDeleteEndpointConfig
--
--         , requestDeleteExperiment $
--             newDeleteExperiment
--
--         , requestDeleteFeatureGroup $
--             newDeleteFeatureGroup
--
--         , requestDeleteFlowDefinition $
--             newDeleteFlowDefinition
--
--         , requestDeleteHub $
--             newDeleteHub
--
--         , requestDeleteHubContent $
--             newDeleteHubContent
--
--         , requestDeleteHumanTaskUi $
--             newDeleteHumanTaskUi
--
--         , requestDeleteImage $
--             newDeleteImage
--
--         , requestDeleteImageVersion $
--             newDeleteImageVersion
--
--         , requestDeleteInferenceExperiment $
--             newDeleteInferenceExperiment
--
--         , requestDeleteModel $
--             newDeleteModel
--
--         , requestDeleteModelBiasJobDefinition $
--             newDeleteModelBiasJobDefinition
--
--         , requestDeleteModelCard $
--             newDeleteModelCard
--
--         , requestDeleteModelExplainabilityJobDefinition $
--             newDeleteModelExplainabilityJobDefinition
--
--         , requestDeleteModelPackage $
--             newDeleteModelPackage
--
--         , requestDeleteModelPackageGroup $
--             newDeleteModelPackageGroup
--
--         , requestDeleteModelPackageGroupPolicy $
--             newDeleteModelPackageGroupPolicy
--
--         , requestDeleteModelQualityJobDefinition $
--             newDeleteModelQualityJobDefinition
--
--         , requestDeleteMonitoringSchedule $
--             newDeleteMonitoringSchedule
--
--         , requestDeleteNotebookInstance $
--             newDeleteNotebookInstance
--
--         , requestDeleteNotebookInstanceLifecycleConfig $
--             newDeleteNotebookInstanceLifecycleConfig
--
--         , requestDeletePipeline $
--             newDeletePipeline
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestDeleteSpace $
--             newDeleteSpace
--
--         , requestDeleteStudioLifecycleConfig $
--             newDeleteStudioLifecycleConfig
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestDeleteTrial $
--             newDeleteTrial
--
--         , requestDeleteTrialComponent $
--             newDeleteTrialComponent
--
--         , requestDeleteUserProfile $
--             newDeleteUserProfile
--
--         , requestDeleteWorkforce $
--             newDeleteWorkforce
--
--         , requestDeleteWorkteam $
--             newDeleteWorkteam
--
--         , requestDeregisterDevices $
--             newDeregisterDevices
--
--         , requestDescribeAction $
--             newDescribeAction
--
--         , requestDescribeAlgorithm $
--             newDescribeAlgorithm
--
--         , requestDescribeApp $
--             newDescribeApp
--
--         , requestDescribeAppImageConfig $
--             newDescribeAppImageConfig
--
--         , requestDescribeArtifact $
--             newDescribeArtifact
--
--         , requestDescribeAutoMLJob $
--             newDescribeAutoMLJob
--
--         , requestDescribeCodeRepository $
--             newDescribeCodeRepository
--
--         , requestDescribeCompilationJob $
--             newDescribeCompilationJob
--
--         , requestDescribeContext $
--             newDescribeContext
--
--         , requestDescribeDataQualityJobDefinition $
--             newDescribeDataQualityJobDefinition
--
--         , requestDescribeDevice $
--             newDescribeDevice
--
--         , requestDescribeDeviceFleet $
--             newDescribeDeviceFleet
--
--         , requestDescribeDomain $
--             newDescribeDomain
--
--         , requestDescribeEdgeDeploymentPlan $
--             newDescribeEdgeDeploymentPlan
--
--         , requestDescribeEdgePackagingJob $
--             newDescribeEdgePackagingJob
--
--         , requestDescribeEndpoint $
--             newDescribeEndpoint
--
--         , requestDescribeEndpointConfig $
--             newDescribeEndpointConfig
--
--         , requestDescribeExperiment $
--             newDescribeExperiment
--
--         , requestDescribeFeatureGroup $
--             newDescribeFeatureGroup
--
--         , requestDescribeFeatureMetadata $
--             newDescribeFeatureMetadata
--
--         , requestDescribeFlowDefinition $
--             newDescribeFlowDefinition
--
--         , requestDescribeHub $
--             newDescribeHub
--
--         , requestDescribeHubContent $
--             newDescribeHubContent
--
--         , requestDescribeHumanTaskUi $
--             newDescribeHumanTaskUi
--
--         , requestDescribeHyperParameterTuningJob $
--             newDescribeHyperParameterTuningJob
--
--         , requestDescribeImage $
--             newDescribeImage
--
--         , requestDescribeImageVersion $
--             newDescribeImageVersion
--
--         , requestDescribeInferenceExperiment $
--             newDescribeInferenceExperiment
--
--         , requestDescribeInferenceRecommendationsJob $
--             newDescribeInferenceRecommendationsJob
--
--         , requestDescribeLabelingJob $
--             newDescribeLabelingJob
--
--         , requestDescribeLineageGroup $
--             newDescribeLineageGroup
--
--         , requestDescribeModel $
--             newDescribeModel
--
--         , requestDescribeModelBiasJobDefinition $
--             newDescribeModelBiasJobDefinition
--
--         , requestDescribeModelCard $
--             newDescribeModelCard
--
--         , requestDescribeModelCardExportJob $
--             newDescribeModelCardExportJob
--
--         , requestDescribeModelExplainabilityJobDefinition $
--             newDescribeModelExplainabilityJobDefinition
--
--         , requestDescribeModelPackage $
--             newDescribeModelPackage
--
--         , requestDescribeModelPackageGroup $
--             newDescribeModelPackageGroup
--
--         , requestDescribeModelQualityJobDefinition $
--             newDescribeModelQualityJobDefinition
--
--         , requestDescribeMonitoringSchedule $
--             newDescribeMonitoringSchedule
--
--         , requestDescribeNotebookInstance $
--             newDescribeNotebookInstance
--
--         , requestDescribeNotebookInstanceLifecycleConfig $
--             newDescribeNotebookInstanceLifecycleConfig
--
--         , requestDescribePipeline $
--             newDescribePipeline
--
--         , requestDescribePipelineDefinitionForExecution $
--             newDescribePipelineDefinitionForExecution
--
--         , requestDescribePipelineExecution $
--             newDescribePipelineExecution
--
--         , requestDescribeProcessingJob $
--             newDescribeProcessingJob
--
--         , requestDescribeProject $
--             newDescribeProject
--
--         , requestDescribeSpace $
--             newDescribeSpace
--
--         , requestDescribeStudioLifecycleConfig $
--             newDescribeStudioLifecycleConfig
--
--         , requestDescribeSubscribedWorkteam $
--             newDescribeSubscribedWorkteam
--
--         , requestDescribeTrainingJob $
--             newDescribeTrainingJob
--
--         , requestDescribeTransformJob $
--             newDescribeTransformJob
--
--         , requestDescribeTrial $
--             newDescribeTrial
--
--         , requestDescribeTrialComponent $
--             newDescribeTrialComponent
--
--         , requestDescribeUserProfile $
--             newDescribeUserProfile
--
--         , requestDescribeWorkforce $
--             newDescribeWorkforce
--
--         , requestDescribeWorkteam $
--             newDescribeWorkteam
--
--         , requestDisableSagemakerServicecatalogPortfolio $
--             newDisableSagemakerServicecatalogPortfolio
--
--         , requestDisassociateTrialComponent $
--             newDisassociateTrialComponent
--
--         , requestEnableSagemakerServicecatalogPortfolio $
--             newEnableSagemakerServicecatalogPortfolio
--
--         , requestGetDeviceFleetReport $
--             newGetDeviceFleetReport
--
--         , requestGetLineageGroupPolicy $
--             newGetLineageGroupPolicy
--
--         , requestGetModelPackageGroupPolicy $
--             newGetModelPackageGroupPolicy
--
--         , requestGetSagemakerServicecatalogPortfolioStatus $
--             newGetSagemakerServicecatalogPortfolioStatus
--
--         , requestGetSearchSuggestions $
--             newGetSearchSuggestions
--
--         , requestImportHubContent $
--             newImportHubContent
--
--         , requestListActions $
--             newListActions
--
--         , requestListAlgorithms $
--             newListAlgorithms
--
--         , requestListAppImageConfigs $
--             newListAppImageConfigs
--
--         , requestListApps $
--             newListApps
--
--         , requestListArtifacts $
--             newListArtifacts
--
--         , requestListAssociations $
--             newListAssociations
--
--         , requestListAutoMLJobs $
--             newListAutoMLJobs
--
--         , requestListCandidatesForAutoMLJob $
--             newListCandidatesForAutoMLJob
--
--         , requestListCodeRepositories $
--             newListCodeRepositories
--
--         , requestListCompilationJobs $
--             newListCompilationJobs
--
--         , requestListContexts $
--             newListContexts
--
--         , requestListDataQualityJobDefinitions $
--             newListDataQualityJobDefinitions
--
--         , requestListDeviceFleets $
--             newListDeviceFleets
--
--         , requestListDevices $
--             newListDevices
--
--         , requestListDomains $
--             newListDomains
--
--         , requestListEdgeDeploymentPlans $
--             newListEdgeDeploymentPlans
--
--         , requestListEdgePackagingJobs $
--             newListEdgePackagingJobs
--
--         , requestListEndpointConfigs $
--             newListEndpointConfigs
--
--         , requestListEndpoints $
--             newListEndpoints
--
--         , requestListExperiments $
--             newListExperiments
--
--         , requestListFeatureGroups $
--             newListFeatureGroups
--
--         , requestListFlowDefinitions $
--             newListFlowDefinitions
--
--         , requestListHubContentVersions $
--             newListHubContentVersions
--
--         , requestListHubContents $
--             newListHubContents
--
--         , requestListHubs $
--             newListHubs
--
--         , requestListHumanTaskUis $
--             newListHumanTaskUis
--
--         , requestListHyperParameterTuningJobs $
--             newListHyperParameterTuningJobs
--
--         , requestListImageVersions $
--             newListImageVersions
--
--         , requestListImages $
--             newListImages
--
--         , requestListInferenceExperiments $
--             newListInferenceExperiments
--
--         , requestListInferenceRecommendationsJobSteps $
--             newListInferenceRecommendationsJobSteps
--
--         , requestListInferenceRecommendationsJobs $
--             newListInferenceRecommendationsJobs
--
--         , requestListLabelingJobs $
--             newListLabelingJobs
--
--         , requestListLabelingJobsForWorkteam $
--             newListLabelingJobsForWorkteam
--
--         , requestListLineageGroups $
--             newListLineageGroups
--
--         , requestListModelBiasJobDefinitions $
--             newListModelBiasJobDefinitions
--
--         , requestListModelCardExportJobs $
--             newListModelCardExportJobs
--
--         , requestListModelCardVersions $
--             newListModelCardVersions
--
--         , requestListModelCards $
--             newListModelCards
--
--         , requestListModelExplainabilityJobDefinitions $
--             newListModelExplainabilityJobDefinitions
--
--         , requestListModelMetadata $
--             newListModelMetadata
--
--         , requestListModelPackageGroups $
--             newListModelPackageGroups
--
--         , requestListModelPackages $
--             newListModelPackages
--
--         , requestListModelQualityJobDefinitions $
--             newListModelQualityJobDefinitions
--
--         , requestListModels $
--             newListModels
--
--         , requestListMonitoringAlertHistory $
--             newListMonitoringAlertHistory
--
--         , requestListMonitoringAlerts $
--             newListMonitoringAlerts
--
--         , requestListMonitoringExecutions $
--             newListMonitoringExecutions
--
--         , requestListMonitoringSchedules $
--             newListMonitoringSchedules
--
--         , requestListNotebookInstanceLifecycleConfigs $
--             newListNotebookInstanceLifecycleConfigs
--
--         , requestListNotebookInstances $
--             newListNotebookInstances
--
--         , requestListPipelineExecutionSteps $
--             newListPipelineExecutionSteps
--
--         , requestListPipelineExecutions $
--             newListPipelineExecutions
--
--         , requestListPipelineParametersForExecution $
--             newListPipelineParametersForExecution
--
--         , requestListPipelines $
--             newListPipelines
--
--         , requestListProcessingJobs $
--             newListProcessingJobs
--
--         , requestListProjects $
--             newListProjects
--
--         , requestListSpaces $
--             newListSpaces
--
--         , requestListStageDevices $
--             newListStageDevices
--
--         , requestListStudioLifecycleConfigs $
--             newListStudioLifecycleConfigs
--
--         , requestListSubscribedWorkteams $
--             newListSubscribedWorkteams
--
--         , requestListTags $
--             newListTags
--
--         , requestListTrainingJobs $
--             newListTrainingJobs
--
--         , requestListTrainingJobsForHyperParameterTuningJob $
--             newListTrainingJobsForHyperParameterTuningJob
--
--         , requestListTransformJobs $
--             newListTransformJobs
--
--         , requestListTrialComponents $
--             newListTrialComponents
--
--         , requestListTrials $
--             newListTrials
--
--         , requestListUserProfiles $
--             newListUserProfiles
--
--         , requestListWorkforces $
--             newListWorkforces
--
--         , requestListWorkteams $
--             newListWorkteams
--
--         , requestPutModelPackageGroupPolicy $
--             newPutModelPackageGroupPolicy
--
--         , requestQueryLineage $
--             newQueryLineage
--
--         , requestRegisterDevices $
--             newRegisterDevices
--
--         , requestRenderUiTemplate $
--             newRenderUiTemplate
--
--         , requestRetryPipelineExecution $
--             newRetryPipelineExecution
--
--         , requestSearch $
--             newSearch
--
--         , requestSendPipelineExecutionStepFailure $
--             newSendPipelineExecutionStepFailure
--
--         , requestSendPipelineExecutionStepSuccess $
--             newSendPipelineExecutionStepSuccess
--
--         , requestStartEdgeDeploymentStage $
--             newStartEdgeDeploymentStage
--
--         , requestStartInferenceExperiment $
--             newStartInferenceExperiment
--
--         , requestStartMonitoringSchedule $
--             newStartMonitoringSchedule
--
--         , requestStartNotebookInstance $
--             newStartNotebookInstance
--
--         , requestStartPipelineExecution $
--             newStartPipelineExecution
--
--         , requestStopAutoMLJob $
--             newStopAutoMLJob
--
--         , requestStopCompilationJob $
--             newStopCompilationJob
--
--         , requestStopEdgeDeploymentStage $
--             newStopEdgeDeploymentStage
--
--         , requestStopEdgePackagingJob $
--             newStopEdgePackagingJob
--
--         , requestStopHyperParameterTuningJob $
--             newStopHyperParameterTuningJob
--
--         , requestStopInferenceExperiment $
--             newStopInferenceExperiment
--
--         , requestStopInferenceRecommendationsJob $
--             newStopInferenceRecommendationsJob
--
--         , requestStopLabelingJob $
--             newStopLabelingJob
--
--         , requestStopMonitoringSchedule $
--             newStopMonitoringSchedule
--
--         , requestStopNotebookInstance $
--             newStopNotebookInstance
--
--         , requestStopPipelineExecution $
--             newStopPipelineExecution
--
--         , requestStopProcessingJob $
--             newStopProcessingJob
--
--         , requestStopTrainingJob $
--             newStopTrainingJob
--
--         , requestStopTransformJob $
--             newStopTransformJob
--
--         , requestUpdateAction $
--             newUpdateAction
--
--         , requestUpdateAppImageConfig $
--             newUpdateAppImageConfig
--
--         , requestUpdateArtifact $
--             newUpdateArtifact
--
--         , requestUpdateCodeRepository $
--             newUpdateCodeRepository
--
--         , requestUpdateContext $
--             newUpdateContext
--
--         , requestUpdateDeviceFleet $
--             newUpdateDeviceFleet
--
--         , requestUpdateDevices $
--             newUpdateDevices
--
--         , requestUpdateDomain $
--             newUpdateDomain
--
--         , requestUpdateEndpoint $
--             newUpdateEndpoint
--
--         , requestUpdateEndpointWeightsAndCapacities $
--             newUpdateEndpointWeightsAndCapacities
--
--         , requestUpdateExperiment $
--             newUpdateExperiment
--
--         , requestUpdateFeatureGroup $
--             newUpdateFeatureGroup
--
--         , requestUpdateFeatureMetadata $
--             newUpdateFeatureMetadata
--
--         , requestUpdateHub $
--             newUpdateHub
--
--         , requestUpdateImage $
--             newUpdateImage
--
--         , requestUpdateInferenceExperiment $
--             newUpdateInferenceExperiment
--
--         , requestUpdateModelCard $
--             newUpdateModelCard
--
--         , requestUpdateModelPackage $
--             newUpdateModelPackage
--
--         , requestUpdateMonitoringAlert $
--             newUpdateMonitoringAlert
--
--         , requestUpdateMonitoringSchedule $
--             newUpdateMonitoringSchedule
--
--         , requestUpdateNotebookInstance $
--             newUpdateNotebookInstance
--
--         , requestUpdateNotebookInstanceLifecycleConfig $
--             newUpdateNotebookInstanceLifecycleConfig
--
--         , requestUpdatePipeline $
--             newUpdatePipeline
--
--         , requestUpdatePipelineExecution $
--             newUpdatePipelineExecution
--
--         , requestUpdateProject $
--             newUpdateProject
--
--         , requestUpdateSpace $
--             newUpdateSpace
--
--         , requestUpdateTrainingJob $
--             newUpdateTrainingJob
--
--         , requestUpdateTrial $
--             newUpdateTrial
--
--         , requestUpdateTrialComponent $
--             newUpdateTrialComponent
--
--         , requestUpdateUserProfile $
--             newUpdateUserProfile
--
--         , requestUpdateWorkforce $
--             newUpdateWorkforce
--
--         , requestUpdateWorkteam $
--             newUpdateWorkteam
--
--           ]

--     , testGroup "response"
--         [ responseAddAssociation $
--             newAddAssociationResponse
--
--         , responseAddTags $
--             newAddTagsResponse
--
--         , responseAssociateTrialComponent $
--             newAssociateTrialComponentResponse
--
--         , responseBatchDescribeModelPackage $
--             newBatchDescribeModelPackageResponse
--
--         , responseCreateAction $
--             newCreateActionResponse
--
--         , responseCreateAlgorithm $
--             newCreateAlgorithmResponse
--
--         , responseCreateApp $
--             newCreateAppResponse
--
--         , responseCreateAppImageConfig $
--             newCreateAppImageConfigResponse
--
--         , responseCreateArtifact $
--             newCreateArtifactResponse
--
--         , responseCreateAutoMLJob $
--             newCreateAutoMLJobResponse
--
--         , responseCreateCodeRepository $
--             newCreateCodeRepositoryResponse
--
--         , responseCreateCompilationJob $
--             newCreateCompilationJobResponse
--
--         , responseCreateContext $
--             newCreateContextResponse
--
--         , responseCreateDataQualityJobDefinition $
--             newCreateDataQualityJobDefinitionResponse
--
--         , responseCreateDeviceFleet $
--             newCreateDeviceFleetResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseCreateEdgeDeploymentPlan $
--             newCreateEdgeDeploymentPlanResponse
--
--         , responseCreateEdgeDeploymentStage $
--             newCreateEdgeDeploymentStageResponse
--
--         , responseCreateEdgePackagingJob $
--             newCreateEdgePackagingJobResponse
--
--         , responseCreateEndpoint $
--             newCreateEndpointResponse
--
--         , responseCreateEndpointConfig $
--             newCreateEndpointConfigResponse
--
--         , responseCreateExperiment $
--             newCreateExperimentResponse
--
--         , responseCreateFeatureGroup $
--             newCreateFeatureGroupResponse
--
--         , responseCreateFlowDefinition $
--             newCreateFlowDefinitionResponse
--
--         , responseCreateHub $
--             newCreateHubResponse
--
--         , responseCreateHumanTaskUi $
--             newCreateHumanTaskUiResponse
--
--         , responseCreateHyperParameterTuningJob $
--             newCreateHyperParameterTuningJobResponse
--
--         , responseCreateImage $
--             newCreateImageResponse
--
--         , responseCreateImageVersion $
--             newCreateImageVersionResponse
--
--         , responseCreateInferenceExperiment $
--             newCreateInferenceExperimentResponse
--
--         , responseCreateInferenceRecommendationsJob $
--             newCreateInferenceRecommendationsJobResponse
--
--         , responseCreateLabelingJob $
--             newCreateLabelingJobResponse
--
--         , responseCreateModel $
--             newCreateModelResponse
--
--         , responseCreateModelBiasJobDefinition $
--             newCreateModelBiasJobDefinitionResponse
--
--         , responseCreateModelCard $
--             newCreateModelCardResponse
--
--         , responseCreateModelCardExportJob $
--             newCreateModelCardExportJobResponse
--
--         , responseCreateModelExplainabilityJobDefinition $
--             newCreateModelExplainabilityJobDefinitionResponse
--
--         , responseCreateModelPackage $
--             newCreateModelPackageResponse
--
--         , responseCreateModelPackageGroup $
--             newCreateModelPackageGroupResponse
--
--         , responseCreateModelQualityJobDefinition $
--             newCreateModelQualityJobDefinitionResponse
--
--         , responseCreateMonitoringSchedule $
--             newCreateMonitoringScheduleResponse
--
--         , responseCreateNotebookInstance $
--             newCreateNotebookInstanceResponse
--
--         , responseCreateNotebookInstanceLifecycleConfig $
--             newCreateNotebookInstanceLifecycleConfigResponse
--
--         , responseCreatePipeline $
--             newCreatePipelineResponse
--
--         , responseCreatePresignedDomainUrl $
--             newCreatePresignedDomainUrlResponse
--
--         , responseCreatePresignedNotebookInstanceUrl $
--             newCreatePresignedNotebookInstanceUrlResponse
--
--         , responseCreateProcessingJob $
--             newCreateProcessingJobResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseCreateSpace $
--             newCreateSpaceResponse
--
--         , responseCreateStudioLifecycleConfig $
--             newCreateStudioLifecycleConfigResponse
--
--         , responseCreateTrainingJob $
--             newCreateTrainingJobResponse
--
--         , responseCreateTransformJob $
--             newCreateTransformJobResponse
--
--         , responseCreateTrial $
--             newCreateTrialResponse
--
--         , responseCreateTrialComponent $
--             newCreateTrialComponentResponse
--
--         , responseCreateUserProfile $
--             newCreateUserProfileResponse
--
--         , responseCreateWorkforce $
--             newCreateWorkforceResponse
--
--         , responseCreateWorkteam $
--             newCreateWorkteamResponse
--
--         , responseDeleteAction $
--             newDeleteActionResponse
--
--         , responseDeleteAlgorithm $
--             newDeleteAlgorithmResponse
--
--         , responseDeleteApp $
--             newDeleteAppResponse
--
--         , responseDeleteAppImageConfig $
--             newDeleteAppImageConfigResponse
--
--         , responseDeleteArtifact $
--             newDeleteArtifactResponse
--
--         , responseDeleteAssociation $
--             newDeleteAssociationResponse
--
--         , responseDeleteCodeRepository $
--             newDeleteCodeRepositoryResponse
--
--         , responseDeleteContext $
--             newDeleteContextResponse
--
--         , responseDeleteDataQualityJobDefinition $
--             newDeleteDataQualityJobDefinitionResponse
--
--         , responseDeleteDeviceFleet $
--             newDeleteDeviceFleetResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseDeleteEdgeDeploymentPlan $
--             newDeleteEdgeDeploymentPlanResponse
--
--         , responseDeleteEdgeDeploymentStage $
--             newDeleteEdgeDeploymentStageResponse
--
--         , responseDeleteEndpoint $
--             newDeleteEndpointResponse
--
--         , responseDeleteEndpointConfig $
--             newDeleteEndpointConfigResponse
--
--         , responseDeleteExperiment $
--             newDeleteExperimentResponse
--
--         , responseDeleteFeatureGroup $
--             newDeleteFeatureGroupResponse
--
--         , responseDeleteFlowDefinition $
--             newDeleteFlowDefinitionResponse
--
--         , responseDeleteHub $
--             newDeleteHubResponse
--
--         , responseDeleteHubContent $
--             newDeleteHubContentResponse
--
--         , responseDeleteHumanTaskUi $
--             newDeleteHumanTaskUiResponse
--
--         , responseDeleteImage $
--             newDeleteImageResponse
--
--         , responseDeleteImageVersion $
--             newDeleteImageVersionResponse
--
--         , responseDeleteInferenceExperiment $
--             newDeleteInferenceExperimentResponse
--
--         , responseDeleteModel $
--             newDeleteModelResponse
--
--         , responseDeleteModelBiasJobDefinition $
--             newDeleteModelBiasJobDefinitionResponse
--
--         , responseDeleteModelCard $
--             newDeleteModelCardResponse
--
--         , responseDeleteModelExplainabilityJobDefinition $
--             newDeleteModelExplainabilityJobDefinitionResponse
--
--         , responseDeleteModelPackage $
--             newDeleteModelPackageResponse
--
--         , responseDeleteModelPackageGroup $
--             newDeleteModelPackageGroupResponse
--
--         , responseDeleteModelPackageGroupPolicy $
--             newDeleteModelPackageGroupPolicyResponse
--
--         , responseDeleteModelQualityJobDefinition $
--             newDeleteModelQualityJobDefinitionResponse
--
--         , responseDeleteMonitoringSchedule $
--             newDeleteMonitoringScheduleResponse
--
--         , responseDeleteNotebookInstance $
--             newDeleteNotebookInstanceResponse
--
--         , responseDeleteNotebookInstanceLifecycleConfig $
--             newDeleteNotebookInstanceLifecycleConfigResponse
--
--         , responseDeletePipeline $
--             newDeletePipelineResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseDeleteSpace $
--             newDeleteSpaceResponse
--
--         , responseDeleteStudioLifecycleConfig $
--             newDeleteStudioLifecycleConfigResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseDeleteTrial $
--             newDeleteTrialResponse
--
--         , responseDeleteTrialComponent $
--             newDeleteTrialComponentResponse
--
--         , responseDeleteUserProfile $
--             newDeleteUserProfileResponse
--
--         , responseDeleteWorkforce $
--             newDeleteWorkforceResponse
--
--         , responseDeleteWorkteam $
--             newDeleteWorkteamResponse
--
--         , responseDeregisterDevices $
--             newDeregisterDevicesResponse
--
--         , responseDescribeAction $
--             newDescribeActionResponse
--
--         , responseDescribeAlgorithm $
--             newDescribeAlgorithmResponse
--
--         , responseDescribeApp $
--             newDescribeAppResponse
--
--         , responseDescribeAppImageConfig $
--             newDescribeAppImageConfigResponse
--
--         , responseDescribeArtifact $
--             newDescribeArtifactResponse
--
--         , responseDescribeAutoMLJob $
--             newDescribeAutoMLJobResponse
--
--         , responseDescribeCodeRepository $
--             newDescribeCodeRepositoryResponse
--
--         , responseDescribeCompilationJob $
--             newDescribeCompilationJobResponse
--
--         , responseDescribeContext $
--             newDescribeContextResponse
--
--         , responseDescribeDataQualityJobDefinition $
--             newDescribeDataQualityJobDefinitionResponse
--
--         , responseDescribeDevice $
--             newDescribeDeviceResponse
--
--         , responseDescribeDeviceFleet $
--             newDescribeDeviceFleetResponse
--
--         , responseDescribeDomain $
--             newDescribeDomainResponse
--
--         , responseDescribeEdgeDeploymentPlan $
--             newDescribeEdgeDeploymentPlanResponse
--
--         , responseDescribeEdgePackagingJob $
--             newDescribeEdgePackagingJobResponse
--
--         , responseDescribeEndpoint $
--             newDescribeEndpointResponse
--
--         , responseDescribeEndpointConfig $
--             newDescribeEndpointConfigResponse
--
--         , responseDescribeExperiment $
--             newDescribeExperimentResponse
--
--         , responseDescribeFeatureGroup $
--             newDescribeFeatureGroupResponse
--
--         , responseDescribeFeatureMetadata $
--             newDescribeFeatureMetadataResponse
--
--         , responseDescribeFlowDefinition $
--             newDescribeFlowDefinitionResponse
--
--         , responseDescribeHub $
--             newDescribeHubResponse
--
--         , responseDescribeHubContent $
--             newDescribeHubContentResponse
--
--         , responseDescribeHumanTaskUi $
--             newDescribeHumanTaskUiResponse
--
--         , responseDescribeHyperParameterTuningJob $
--             newDescribeHyperParameterTuningJobResponse
--
--         , responseDescribeImage $
--             newDescribeImageResponse
--
--         , responseDescribeImageVersion $
--             newDescribeImageVersionResponse
--
--         , responseDescribeInferenceExperiment $
--             newDescribeInferenceExperimentResponse
--
--         , responseDescribeInferenceRecommendationsJob $
--             newDescribeInferenceRecommendationsJobResponse
--
--         , responseDescribeLabelingJob $
--             newDescribeLabelingJobResponse
--
--         , responseDescribeLineageGroup $
--             newDescribeLineageGroupResponse
--
--         , responseDescribeModel $
--             newDescribeModelResponse
--
--         , responseDescribeModelBiasJobDefinition $
--             newDescribeModelBiasJobDefinitionResponse
--
--         , responseDescribeModelCard $
--             newDescribeModelCardResponse
--
--         , responseDescribeModelCardExportJob $
--             newDescribeModelCardExportJobResponse
--
--         , responseDescribeModelExplainabilityJobDefinition $
--             newDescribeModelExplainabilityJobDefinitionResponse
--
--         , responseDescribeModelPackage $
--             newDescribeModelPackageResponse
--
--         , responseDescribeModelPackageGroup $
--             newDescribeModelPackageGroupResponse
--
--         , responseDescribeModelQualityJobDefinition $
--             newDescribeModelQualityJobDefinitionResponse
--
--         , responseDescribeMonitoringSchedule $
--             newDescribeMonitoringScheduleResponse
--
--         , responseDescribeNotebookInstance $
--             newDescribeNotebookInstanceResponse
--
--         , responseDescribeNotebookInstanceLifecycleConfig $
--             newDescribeNotebookInstanceLifecycleConfigResponse
--
--         , responseDescribePipeline $
--             newDescribePipelineResponse
--
--         , responseDescribePipelineDefinitionForExecution $
--             newDescribePipelineDefinitionForExecutionResponse
--
--         , responseDescribePipelineExecution $
--             newDescribePipelineExecutionResponse
--
--         , responseDescribeProcessingJob $
--             newDescribeProcessingJobResponse
--
--         , responseDescribeProject $
--             newDescribeProjectResponse
--
--         , responseDescribeSpace $
--             newDescribeSpaceResponse
--
--         , responseDescribeStudioLifecycleConfig $
--             newDescribeStudioLifecycleConfigResponse
--
--         , responseDescribeSubscribedWorkteam $
--             newDescribeSubscribedWorkteamResponse
--
--         , responseDescribeTrainingJob $
--             newDescribeTrainingJobResponse
--
--         , responseDescribeTransformJob $
--             newDescribeTransformJobResponse
--
--         , responseDescribeTrial $
--             newDescribeTrialResponse
--
--         , responseDescribeTrialComponent $
--             newDescribeTrialComponentResponse
--
--         , responseDescribeUserProfile $
--             newDescribeUserProfileResponse
--
--         , responseDescribeWorkforce $
--             newDescribeWorkforceResponse
--
--         , responseDescribeWorkteam $
--             newDescribeWorkteamResponse
--
--         , responseDisableSagemakerServicecatalogPortfolio $
--             newDisableSagemakerServicecatalogPortfolioResponse
--
--         , responseDisassociateTrialComponent $
--             newDisassociateTrialComponentResponse
--
--         , responseEnableSagemakerServicecatalogPortfolio $
--             newEnableSagemakerServicecatalogPortfolioResponse
--
--         , responseGetDeviceFleetReport $
--             newGetDeviceFleetReportResponse
--
--         , responseGetLineageGroupPolicy $
--             newGetLineageGroupPolicyResponse
--
--         , responseGetModelPackageGroupPolicy $
--             newGetModelPackageGroupPolicyResponse
--
--         , responseGetSagemakerServicecatalogPortfolioStatus $
--             newGetSagemakerServicecatalogPortfolioStatusResponse
--
--         , responseGetSearchSuggestions $
--             newGetSearchSuggestionsResponse
--
--         , responseImportHubContent $
--             newImportHubContentResponse
--
--         , responseListActions $
--             newListActionsResponse
--
--         , responseListAlgorithms $
--             newListAlgorithmsResponse
--
--         , responseListAppImageConfigs $
--             newListAppImageConfigsResponse
--
--         , responseListApps $
--             newListAppsResponse
--
--         , responseListArtifacts $
--             newListArtifactsResponse
--
--         , responseListAssociations $
--             newListAssociationsResponse
--
--         , responseListAutoMLJobs $
--             newListAutoMLJobsResponse
--
--         , responseListCandidatesForAutoMLJob $
--             newListCandidatesForAutoMLJobResponse
--
--         , responseListCodeRepositories $
--             newListCodeRepositoriesResponse
--
--         , responseListCompilationJobs $
--             newListCompilationJobsResponse
--
--         , responseListContexts $
--             newListContextsResponse
--
--         , responseListDataQualityJobDefinitions $
--             newListDataQualityJobDefinitionsResponse
--
--         , responseListDeviceFleets $
--             newListDeviceFleetsResponse
--
--         , responseListDevices $
--             newListDevicesResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--         , responseListEdgeDeploymentPlans $
--             newListEdgeDeploymentPlansResponse
--
--         , responseListEdgePackagingJobs $
--             newListEdgePackagingJobsResponse
--
--         , responseListEndpointConfigs $
--             newListEndpointConfigsResponse
--
--         , responseListEndpoints $
--             newListEndpointsResponse
--
--         , responseListExperiments $
--             newListExperimentsResponse
--
--         , responseListFeatureGroups $
--             newListFeatureGroupsResponse
--
--         , responseListFlowDefinitions $
--             newListFlowDefinitionsResponse
--
--         , responseListHubContentVersions $
--             newListHubContentVersionsResponse
--
--         , responseListHubContents $
--             newListHubContentsResponse
--
--         , responseListHubs $
--             newListHubsResponse
--
--         , responseListHumanTaskUis $
--             newListHumanTaskUisResponse
--
--         , responseListHyperParameterTuningJobs $
--             newListHyperParameterTuningJobsResponse
--
--         , responseListImageVersions $
--             newListImageVersionsResponse
--
--         , responseListImages $
--             newListImagesResponse
--
--         , responseListInferenceExperiments $
--             newListInferenceExperimentsResponse
--
--         , responseListInferenceRecommendationsJobSteps $
--             newListInferenceRecommendationsJobStepsResponse
--
--         , responseListInferenceRecommendationsJobs $
--             newListInferenceRecommendationsJobsResponse
--
--         , responseListLabelingJobs $
--             newListLabelingJobsResponse
--
--         , responseListLabelingJobsForWorkteam $
--             newListLabelingJobsForWorkteamResponse
--
--         , responseListLineageGroups $
--             newListLineageGroupsResponse
--
--         , responseListModelBiasJobDefinitions $
--             newListModelBiasJobDefinitionsResponse
--
--         , responseListModelCardExportJobs $
--             newListModelCardExportJobsResponse
--
--         , responseListModelCardVersions $
--             newListModelCardVersionsResponse
--
--         , responseListModelCards $
--             newListModelCardsResponse
--
--         , responseListModelExplainabilityJobDefinitions $
--             newListModelExplainabilityJobDefinitionsResponse
--
--         , responseListModelMetadata $
--             newListModelMetadataResponse
--
--         , responseListModelPackageGroups $
--             newListModelPackageGroupsResponse
--
--         , responseListModelPackages $
--             newListModelPackagesResponse
--
--         , responseListModelQualityJobDefinitions $
--             newListModelQualityJobDefinitionsResponse
--
--         , responseListModels $
--             newListModelsResponse
--
--         , responseListMonitoringAlertHistory $
--             newListMonitoringAlertHistoryResponse
--
--         , responseListMonitoringAlerts $
--             newListMonitoringAlertsResponse
--
--         , responseListMonitoringExecutions $
--             newListMonitoringExecutionsResponse
--
--         , responseListMonitoringSchedules $
--             newListMonitoringSchedulesResponse
--
--         , responseListNotebookInstanceLifecycleConfigs $
--             newListNotebookInstanceLifecycleConfigsResponse
--
--         , responseListNotebookInstances $
--             newListNotebookInstancesResponse
--
--         , responseListPipelineExecutionSteps $
--             newListPipelineExecutionStepsResponse
--
--         , responseListPipelineExecutions $
--             newListPipelineExecutionsResponse
--
--         , responseListPipelineParametersForExecution $
--             newListPipelineParametersForExecutionResponse
--
--         , responseListPipelines $
--             newListPipelinesResponse
--
--         , responseListProcessingJobs $
--             newListProcessingJobsResponse
--
--         , responseListProjects $
--             newListProjectsResponse
--
--         , responseListSpaces $
--             newListSpacesResponse
--
--         , responseListStageDevices $
--             newListStageDevicesResponse
--
--         , responseListStudioLifecycleConfigs $
--             newListStudioLifecycleConfigsResponse
--
--         , responseListSubscribedWorkteams $
--             newListSubscribedWorkteamsResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseListTrainingJobs $
--             newListTrainingJobsResponse
--
--         , responseListTrainingJobsForHyperParameterTuningJob $
--             newListTrainingJobsForHyperParameterTuningJobResponse
--
--         , responseListTransformJobs $
--             newListTransformJobsResponse
--
--         , responseListTrialComponents $
--             newListTrialComponentsResponse
--
--         , responseListTrials $
--             newListTrialsResponse
--
--         , responseListUserProfiles $
--             newListUserProfilesResponse
--
--         , responseListWorkforces $
--             newListWorkforcesResponse
--
--         , responseListWorkteams $
--             newListWorkteamsResponse
--
--         , responsePutModelPackageGroupPolicy $
--             newPutModelPackageGroupPolicyResponse
--
--         , responseQueryLineage $
--             newQueryLineageResponse
--
--         , responseRegisterDevices $
--             newRegisterDevicesResponse
--
--         , responseRenderUiTemplate $
--             newRenderUiTemplateResponse
--
--         , responseRetryPipelineExecution $
--             newRetryPipelineExecutionResponse
--
--         , responseSearch $
--             newSearchResponse
--
--         , responseSendPipelineExecutionStepFailure $
--             newSendPipelineExecutionStepFailureResponse
--
--         , responseSendPipelineExecutionStepSuccess $
--             newSendPipelineExecutionStepSuccessResponse
--
--         , responseStartEdgeDeploymentStage $
--             newStartEdgeDeploymentStageResponse
--
--         , responseStartInferenceExperiment $
--             newStartInferenceExperimentResponse
--
--         , responseStartMonitoringSchedule $
--             newStartMonitoringScheduleResponse
--
--         , responseStartNotebookInstance $
--             newStartNotebookInstanceResponse
--
--         , responseStartPipelineExecution $
--             newStartPipelineExecutionResponse
--
--         , responseStopAutoMLJob $
--             newStopAutoMLJobResponse
--
--         , responseStopCompilationJob $
--             newStopCompilationJobResponse
--
--         , responseStopEdgeDeploymentStage $
--             newStopEdgeDeploymentStageResponse
--
--         , responseStopEdgePackagingJob $
--             newStopEdgePackagingJobResponse
--
--         , responseStopHyperParameterTuningJob $
--             newStopHyperParameterTuningJobResponse
--
--         , responseStopInferenceExperiment $
--             newStopInferenceExperimentResponse
--
--         , responseStopInferenceRecommendationsJob $
--             newStopInferenceRecommendationsJobResponse
--
--         , responseStopLabelingJob $
--             newStopLabelingJobResponse
--
--         , responseStopMonitoringSchedule $
--             newStopMonitoringScheduleResponse
--
--         , responseStopNotebookInstance $
--             newStopNotebookInstanceResponse
--
--         , responseStopPipelineExecution $
--             newStopPipelineExecutionResponse
--
--         , responseStopProcessingJob $
--             newStopProcessingJobResponse
--
--         , responseStopTrainingJob $
--             newStopTrainingJobResponse
--
--         , responseStopTransformJob $
--             newStopTransformJobResponse
--
--         , responseUpdateAction $
--             newUpdateActionResponse
--
--         , responseUpdateAppImageConfig $
--             newUpdateAppImageConfigResponse
--
--         , responseUpdateArtifact $
--             newUpdateArtifactResponse
--
--         , responseUpdateCodeRepository $
--             newUpdateCodeRepositoryResponse
--
--         , responseUpdateContext $
--             newUpdateContextResponse
--
--         , responseUpdateDeviceFleet $
--             newUpdateDeviceFleetResponse
--
--         , responseUpdateDevices $
--             newUpdateDevicesResponse
--
--         , responseUpdateDomain $
--             newUpdateDomainResponse
--
--         , responseUpdateEndpoint $
--             newUpdateEndpointResponse
--
--         , responseUpdateEndpointWeightsAndCapacities $
--             newUpdateEndpointWeightsAndCapacitiesResponse
--
--         , responseUpdateExperiment $
--             newUpdateExperimentResponse
--
--         , responseUpdateFeatureGroup $
--             newUpdateFeatureGroupResponse
--
--         , responseUpdateFeatureMetadata $
--             newUpdateFeatureMetadataResponse
--
--         , responseUpdateHub $
--             newUpdateHubResponse
--
--         , responseUpdateImage $
--             newUpdateImageResponse
--
--         , responseUpdateInferenceExperiment $
--             newUpdateInferenceExperimentResponse
--
--         , responseUpdateModelCard $
--             newUpdateModelCardResponse
--
--         , responseUpdateModelPackage $
--             newUpdateModelPackageResponse
--
--         , responseUpdateMonitoringAlert $
--             newUpdateMonitoringAlertResponse
--
--         , responseUpdateMonitoringSchedule $
--             newUpdateMonitoringScheduleResponse
--
--         , responseUpdateNotebookInstance $
--             newUpdateNotebookInstanceResponse
--
--         , responseUpdateNotebookInstanceLifecycleConfig $
--             newUpdateNotebookInstanceLifecycleConfigResponse
--
--         , responseUpdatePipeline $
--             newUpdatePipelineResponse
--
--         , responseUpdatePipelineExecution $
--             newUpdatePipelineExecutionResponse
--
--         , responseUpdateProject $
--             newUpdateProjectResponse
--
--         , responseUpdateSpace $
--             newUpdateSpaceResponse
--
--         , responseUpdateTrainingJob $
--             newUpdateTrainingJobResponse
--
--         , responseUpdateTrial $
--             newUpdateTrialResponse
--
--         , responseUpdateTrialComponent $
--             newUpdateTrialComponentResponse
--
--         , responseUpdateUserProfile $
--             newUpdateUserProfileResponse
--
--         , responseUpdateWorkforce $
--             newUpdateWorkforceResponse
--
--         , responseUpdateWorkteam $
--             newUpdateWorkteamResponse
--
--           ]
--     ]

-- Requests

requestAddAssociation :: AddAssociation -> TestTree
requestAddAssociation =
  req
    "AddAssociation"
    "fixture/AddAssociation.yaml"

requestAddTags :: AddTags -> TestTree
requestAddTags =
  req
    "AddTags"
    "fixture/AddTags.yaml"

requestAssociateTrialComponent :: AssociateTrialComponent -> TestTree
requestAssociateTrialComponent =
  req
    "AssociateTrialComponent"
    "fixture/AssociateTrialComponent.yaml"

requestBatchDescribeModelPackage :: BatchDescribeModelPackage -> TestTree
requestBatchDescribeModelPackage =
  req
    "BatchDescribeModelPackage"
    "fixture/BatchDescribeModelPackage.yaml"

requestCreateAction :: CreateAction -> TestTree
requestCreateAction =
  req
    "CreateAction"
    "fixture/CreateAction.yaml"

requestCreateAlgorithm :: CreateAlgorithm -> TestTree
requestCreateAlgorithm =
  req
    "CreateAlgorithm"
    "fixture/CreateAlgorithm.yaml"

requestCreateApp :: CreateApp -> TestTree
requestCreateApp =
  req
    "CreateApp"
    "fixture/CreateApp.yaml"

requestCreateAppImageConfig :: CreateAppImageConfig -> TestTree
requestCreateAppImageConfig =
  req
    "CreateAppImageConfig"
    "fixture/CreateAppImageConfig.yaml"

requestCreateArtifact :: CreateArtifact -> TestTree
requestCreateArtifact =
  req
    "CreateArtifact"
    "fixture/CreateArtifact.yaml"

requestCreateAutoMLJob :: CreateAutoMLJob -> TestTree
requestCreateAutoMLJob =
  req
    "CreateAutoMLJob"
    "fixture/CreateAutoMLJob.yaml"

requestCreateCodeRepository :: CreateCodeRepository -> TestTree
requestCreateCodeRepository =
  req
    "CreateCodeRepository"
    "fixture/CreateCodeRepository.yaml"

requestCreateCompilationJob :: CreateCompilationJob -> TestTree
requestCreateCompilationJob =
  req
    "CreateCompilationJob"
    "fixture/CreateCompilationJob.yaml"

requestCreateContext :: CreateContext -> TestTree
requestCreateContext =
  req
    "CreateContext"
    "fixture/CreateContext.yaml"

requestCreateDataQualityJobDefinition :: CreateDataQualityJobDefinition -> TestTree
requestCreateDataQualityJobDefinition =
  req
    "CreateDataQualityJobDefinition"
    "fixture/CreateDataQualityJobDefinition.yaml"

requestCreateDeviceFleet :: CreateDeviceFleet -> TestTree
requestCreateDeviceFleet =
  req
    "CreateDeviceFleet"
    "fixture/CreateDeviceFleet.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestCreateEdgeDeploymentPlan :: CreateEdgeDeploymentPlan -> TestTree
requestCreateEdgeDeploymentPlan =
  req
    "CreateEdgeDeploymentPlan"
    "fixture/CreateEdgeDeploymentPlan.yaml"

requestCreateEdgeDeploymentStage :: CreateEdgeDeploymentStage -> TestTree
requestCreateEdgeDeploymentStage =
  req
    "CreateEdgeDeploymentStage"
    "fixture/CreateEdgeDeploymentStage.yaml"

requestCreateEdgePackagingJob :: CreateEdgePackagingJob -> TestTree
requestCreateEdgePackagingJob =
  req
    "CreateEdgePackagingJob"
    "fixture/CreateEdgePackagingJob.yaml"

requestCreateEndpoint :: CreateEndpoint -> TestTree
requestCreateEndpoint =
  req
    "CreateEndpoint"
    "fixture/CreateEndpoint.yaml"

requestCreateEndpointConfig :: CreateEndpointConfig -> TestTree
requestCreateEndpointConfig =
  req
    "CreateEndpointConfig"
    "fixture/CreateEndpointConfig.yaml"

requestCreateExperiment :: CreateExperiment -> TestTree
requestCreateExperiment =
  req
    "CreateExperiment"
    "fixture/CreateExperiment.yaml"

requestCreateFeatureGroup :: CreateFeatureGroup -> TestTree
requestCreateFeatureGroup =
  req
    "CreateFeatureGroup"
    "fixture/CreateFeatureGroup.yaml"

requestCreateFlowDefinition :: CreateFlowDefinition -> TestTree
requestCreateFlowDefinition =
  req
    "CreateFlowDefinition"
    "fixture/CreateFlowDefinition.yaml"

requestCreateHub :: CreateHub -> TestTree
requestCreateHub =
  req
    "CreateHub"
    "fixture/CreateHub.yaml"

requestCreateHumanTaskUi :: CreateHumanTaskUi -> TestTree
requestCreateHumanTaskUi =
  req
    "CreateHumanTaskUi"
    "fixture/CreateHumanTaskUi.yaml"

requestCreateHyperParameterTuningJob :: CreateHyperParameterTuningJob -> TestTree
requestCreateHyperParameterTuningJob =
  req
    "CreateHyperParameterTuningJob"
    "fixture/CreateHyperParameterTuningJob.yaml"

requestCreateImage :: CreateImage -> TestTree
requestCreateImage =
  req
    "CreateImage"
    "fixture/CreateImage.yaml"

requestCreateImageVersion :: CreateImageVersion -> TestTree
requestCreateImageVersion =
  req
    "CreateImageVersion"
    "fixture/CreateImageVersion.yaml"

requestCreateInferenceExperiment :: CreateInferenceExperiment -> TestTree
requestCreateInferenceExperiment =
  req
    "CreateInferenceExperiment"
    "fixture/CreateInferenceExperiment.yaml"

requestCreateInferenceRecommendationsJob :: CreateInferenceRecommendationsJob -> TestTree
requestCreateInferenceRecommendationsJob =
  req
    "CreateInferenceRecommendationsJob"
    "fixture/CreateInferenceRecommendationsJob.yaml"

requestCreateLabelingJob :: CreateLabelingJob -> TestTree
requestCreateLabelingJob =
  req
    "CreateLabelingJob"
    "fixture/CreateLabelingJob.yaml"

requestCreateModel :: CreateModel -> TestTree
requestCreateModel =
  req
    "CreateModel"
    "fixture/CreateModel.yaml"

requestCreateModelBiasJobDefinition :: CreateModelBiasJobDefinition -> TestTree
requestCreateModelBiasJobDefinition =
  req
    "CreateModelBiasJobDefinition"
    "fixture/CreateModelBiasJobDefinition.yaml"

requestCreateModelCard :: CreateModelCard -> TestTree
requestCreateModelCard =
  req
    "CreateModelCard"
    "fixture/CreateModelCard.yaml"

requestCreateModelCardExportJob :: CreateModelCardExportJob -> TestTree
requestCreateModelCardExportJob =
  req
    "CreateModelCardExportJob"
    "fixture/CreateModelCardExportJob.yaml"

requestCreateModelExplainabilityJobDefinition :: CreateModelExplainabilityJobDefinition -> TestTree
requestCreateModelExplainabilityJobDefinition =
  req
    "CreateModelExplainabilityJobDefinition"
    "fixture/CreateModelExplainabilityJobDefinition.yaml"

requestCreateModelPackage :: CreateModelPackage -> TestTree
requestCreateModelPackage =
  req
    "CreateModelPackage"
    "fixture/CreateModelPackage.yaml"

requestCreateModelPackageGroup :: CreateModelPackageGroup -> TestTree
requestCreateModelPackageGroup =
  req
    "CreateModelPackageGroup"
    "fixture/CreateModelPackageGroup.yaml"

requestCreateModelQualityJobDefinition :: CreateModelQualityJobDefinition -> TestTree
requestCreateModelQualityJobDefinition =
  req
    "CreateModelQualityJobDefinition"
    "fixture/CreateModelQualityJobDefinition.yaml"

requestCreateMonitoringSchedule :: CreateMonitoringSchedule -> TestTree
requestCreateMonitoringSchedule =
  req
    "CreateMonitoringSchedule"
    "fixture/CreateMonitoringSchedule.yaml"

requestCreateNotebookInstance :: CreateNotebookInstance -> TestTree
requestCreateNotebookInstance =
  req
    "CreateNotebookInstance"
    "fixture/CreateNotebookInstance.yaml"

requestCreateNotebookInstanceLifecycleConfig :: CreateNotebookInstanceLifecycleConfig -> TestTree
requestCreateNotebookInstanceLifecycleConfig =
  req
    "CreateNotebookInstanceLifecycleConfig"
    "fixture/CreateNotebookInstanceLifecycleConfig.yaml"

requestCreatePipeline :: CreatePipeline -> TestTree
requestCreatePipeline =
  req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

requestCreatePresignedDomainUrl :: CreatePresignedDomainUrl -> TestTree
requestCreatePresignedDomainUrl =
  req
    "CreatePresignedDomainUrl"
    "fixture/CreatePresignedDomainUrl.yaml"

requestCreatePresignedNotebookInstanceUrl :: CreatePresignedNotebookInstanceUrl -> TestTree
requestCreatePresignedNotebookInstanceUrl =
  req
    "CreatePresignedNotebookInstanceUrl"
    "fixture/CreatePresignedNotebookInstanceUrl.yaml"

requestCreateProcessingJob :: CreateProcessingJob -> TestTree
requestCreateProcessingJob =
  req
    "CreateProcessingJob"
    "fixture/CreateProcessingJob.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestCreateSpace :: CreateSpace -> TestTree
requestCreateSpace =
  req
    "CreateSpace"
    "fixture/CreateSpace.yaml"

requestCreateStudioLifecycleConfig :: CreateStudioLifecycleConfig -> TestTree
requestCreateStudioLifecycleConfig =
  req
    "CreateStudioLifecycleConfig"
    "fixture/CreateStudioLifecycleConfig.yaml"

requestCreateTrainingJob :: CreateTrainingJob -> TestTree
requestCreateTrainingJob =
  req
    "CreateTrainingJob"
    "fixture/CreateTrainingJob.yaml"

requestCreateTransformJob :: CreateTransformJob -> TestTree
requestCreateTransformJob =
  req
    "CreateTransformJob"
    "fixture/CreateTransformJob.yaml"

requestCreateTrial :: CreateTrial -> TestTree
requestCreateTrial =
  req
    "CreateTrial"
    "fixture/CreateTrial.yaml"

requestCreateTrialComponent :: CreateTrialComponent -> TestTree
requestCreateTrialComponent =
  req
    "CreateTrialComponent"
    "fixture/CreateTrialComponent.yaml"

requestCreateUserProfile :: CreateUserProfile -> TestTree
requestCreateUserProfile =
  req
    "CreateUserProfile"
    "fixture/CreateUserProfile.yaml"

requestCreateWorkforce :: CreateWorkforce -> TestTree
requestCreateWorkforce =
  req
    "CreateWorkforce"
    "fixture/CreateWorkforce.yaml"

requestCreateWorkteam :: CreateWorkteam -> TestTree
requestCreateWorkteam =
  req
    "CreateWorkteam"
    "fixture/CreateWorkteam.yaml"

requestDeleteAction :: DeleteAction -> TestTree
requestDeleteAction =
  req
    "DeleteAction"
    "fixture/DeleteAction.yaml"

requestDeleteAlgorithm :: DeleteAlgorithm -> TestTree
requestDeleteAlgorithm =
  req
    "DeleteAlgorithm"
    "fixture/DeleteAlgorithm.yaml"

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp =
  req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

requestDeleteAppImageConfig :: DeleteAppImageConfig -> TestTree
requestDeleteAppImageConfig =
  req
    "DeleteAppImageConfig"
    "fixture/DeleteAppImageConfig.yaml"

requestDeleteArtifact :: DeleteArtifact -> TestTree
requestDeleteArtifact =
  req
    "DeleteArtifact"
    "fixture/DeleteArtifact.yaml"

requestDeleteAssociation :: DeleteAssociation -> TestTree
requestDeleteAssociation =
  req
    "DeleteAssociation"
    "fixture/DeleteAssociation.yaml"

requestDeleteCodeRepository :: DeleteCodeRepository -> TestTree
requestDeleteCodeRepository =
  req
    "DeleteCodeRepository"
    "fixture/DeleteCodeRepository.yaml"

requestDeleteContext :: DeleteContext -> TestTree
requestDeleteContext =
  req
    "DeleteContext"
    "fixture/DeleteContext.yaml"

requestDeleteDataQualityJobDefinition :: DeleteDataQualityJobDefinition -> TestTree
requestDeleteDataQualityJobDefinition =
  req
    "DeleteDataQualityJobDefinition"
    "fixture/DeleteDataQualityJobDefinition.yaml"

requestDeleteDeviceFleet :: DeleteDeviceFleet -> TestTree
requestDeleteDeviceFleet =
  req
    "DeleteDeviceFleet"
    "fixture/DeleteDeviceFleet.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestDeleteEdgeDeploymentPlan :: DeleteEdgeDeploymentPlan -> TestTree
requestDeleteEdgeDeploymentPlan =
  req
    "DeleteEdgeDeploymentPlan"
    "fixture/DeleteEdgeDeploymentPlan.yaml"

requestDeleteEdgeDeploymentStage :: DeleteEdgeDeploymentStage -> TestTree
requestDeleteEdgeDeploymentStage =
  req
    "DeleteEdgeDeploymentStage"
    "fixture/DeleteEdgeDeploymentStage.yaml"

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint =
  req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

requestDeleteEndpointConfig :: DeleteEndpointConfig -> TestTree
requestDeleteEndpointConfig =
  req
    "DeleteEndpointConfig"
    "fixture/DeleteEndpointConfig.yaml"

requestDeleteExperiment :: DeleteExperiment -> TestTree
requestDeleteExperiment =
  req
    "DeleteExperiment"
    "fixture/DeleteExperiment.yaml"

requestDeleteFeatureGroup :: DeleteFeatureGroup -> TestTree
requestDeleteFeatureGroup =
  req
    "DeleteFeatureGroup"
    "fixture/DeleteFeatureGroup.yaml"

requestDeleteFlowDefinition :: DeleteFlowDefinition -> TestTree
requestDeleteFlowDefinition =
  req
    "DeleteFlowDefinition"
    "fixture/DeleteFlowDefinition.yaml"

requestDeleteHub :: DeleteHub -> TestTree
requestDeleteHub =
  req
    "DeleteHub"
    "fixture/DeleteHub.yaml"

requestDeleteHubContent :: DeleteHubContent -> TestTree
requestDeleteHubContent =
  req
    "DeleteHubContent"
    "fixture/DeleteHubContent.yaml"

requestDeleteHumanTaskUi :: DeleteHumanTaskUi -> TestTree
requestDeleteHumanTaskUi =
  req
    "DeleteHumanTaskUi"
    "fixture/DeleteHumanTaskUi.yaml"

requestDeleteImage :: DeleteImage -> TestTree
requestDeleteImage =
  req
    "DeleteImage"
    "fixture/DeleteImage.yaml"

requestDeleteImageVersion :: DeleteImageVersion -> TestTree
requestDeleteImageVersion =
  req
    "DeleteImageVersion"
    "fixture/DeleteImageVersion.yaml"

requestDeleteInferenceExperiment :: DeleteInferenceExperiment -> TestTree
requestDeleteInferenceExperiment =
  req
    "DeleteInferenceExperiment"
    "fixture/DeleteInferenceExperiment.yaml"

requestDeleteModel :: DeleteModel -> TestTree
requestDeleteModel =
  req
    "DeleteModel"
    "fixture/DeleteModel.yaml"

requestDeleteModelBiasJobDefinition :: DeleteModelBiasJobDefinition -> TestTree
requestDeleteModelBiasJobDefinition =
  req
    "DeleteModelBiasJobDefinition"
    "fixture/DeleteModelBiasJobDefinition.yaml"

requestDeleteModelCard :: DeleteModelCard -> TestTree
requestDeleteModelCard =
  req
    "DeleteModelCard"
    "fixture/DeleteModelCard.yaml"

requestDeleteModelExplainabilityJobDefinition :: DeleteModelExplainabilityJobDefinition -> TestTree
requestDeleteModelExplainabilityJobDefinition =
  req
    "DeleteModelExplainabilityJobDefinition"
    "fixture/DeleteModelExplainabilityJobDefinition.yaml"

requestDeleteModelPackage :: DeleteModelPackage -> TestTree
requestDeleteModelPackage =
  req
    "DeleteModelPackage"
    "fixture/DeleteModelPackage.yaml"

requestDeleteModelPackageGroup :: DeleteModelPackageGroup -> TestTree
requestDeleteModelPackageGroup =
  req
    "DeleteModelPackageGroup"
    "fixture/DeleteModelPackageGroup.yaml"

requestDeleteModelPackageGroupPolicy :: DeleteModelPackageGroupPolicy -> TestTree
requestDeleteModelPackageGroupPolicy =
  req
    "DeleteModelPackageGroupPolicy"
    "fixture/DeleteModelPackageGroupPolicy.yaml"

requestDeleteModelQualityJobDefinition :: DeleteModelQualityJobDefinition -> TestTree
requestDeleteModelQualityJobDefinition =
  req
    "DeleteModelQualityJobDefinition"
    "fixture/DeleteModelQualityJobDefinition.yaml"

requestDeleteMonitoringSchedule :: DeleteMonitoringSchedule -> TestTree
requestDeleteMonitoringSchedule =
  req
    "DeleteMonitoringSchedule"
    "fixture/DeleteMonitoringSchedule.yaml"

requestDeleteNotebookInstance :: DeleteNotebookInstance -> TestTree
requestDeleteNotebookInstance =
  req
    "DeleteNotebookInstance"
    "fixture/DeleteNotebookInstance.yaml"

requestDeleteNotebookInstanceLifecycleConfig :: DeleteNotebookInstanceLifecycleConfig -> TestTree
requestDeleteNotebookInstanceLifecycleConfig =
  req
    "DeleteNotebookInstanceLifecycleConfig"
    "fixture/DeleteNotebookInstanceLifecycleConfig.yaml"

requestDeletePipeline :: DeletePipeline -> TestTree
requestDeletePipeline =
  req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject =
  req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestDeleteSpace :: DeleteSpace -> TestTree
requestDeleteSpace =
  req
    "DeleteSpace"
    "fixture/DeleteSpace.yaml"

requestDeleteStudioLifecycleConfig :: DeleteStudioLifecycleConfig -> TestTree
requestDeleteStudioLifecycleConfig =
  req
    "DeleteStudioLifecycleConfig"
    "fixture/DeleteStudioLifecycleConfig.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestDeleteTrial :: DeleteTrial -> TestTree
requestDeleteTrial =
  req
    "DeleteTrial"
    "fixture/DeleteTrial.yaml"

requestDeleteTrialComponent :: DeleteTrialComponent -> TestTree
requestDeleteTrialComponent =
  req
    "DeleteTrialComponent"
    "fixture/DeleteTrialComponent.yaml"

requestDeleteUserProfile :: DeleteUserProfile -> TestTree
requestDeleteUserProfile =
  req
    "DeleteUserProfile"
    "fixture/DeleteUserProfile.yaml"

requestDeleteWorkforce :: DeleteWorkforce -> TestTree
requestDeleteWorkforce =
  req
    "DeleteWorkforce"
    "fixture/DeleteWorkforce.yaml"

requestDeleteWorkteam :: DeleteWorkteam -> TestTree
requestDeleteWorkteam =
  req
    "DeleteWorkteam"
    "fixture/DeleteWorkteam.yaml"

requestDeregisterDevices :: DeregisterDevices -> TestTree
requestDeregisterDevices =
  req
    "DeregisterDevices"
    "fixture/DeregisterDevices.yaml"

requestDescribeAction :: DescribeAction -> TestTree
requestDescribeAction =
  req
    "DescribeAction"
    "fixture/DescribeAction.yaml"

requestDescribeAlgorithm :: DescribeAlgorithm -> TestTree
requestDescribeAlgorithm =
  req
    "DescribeAlgorithm"
    "fixture/DescribeAlgorithm.yaml"

requestDescribeApp :: DescribeApp -> TestTree
requestDescribeApp =
  req
    "DescribeApp"
    "fixture/DescribeApp.yaml"

requestDescribeAppImageConfig :: DescribeAppImageConfig -> TestTree
requestDescribeAppImageConfig =
  req
    "DescribeAppImageConfig"
    "fixture/DescribeAppImageConfig.yaml"

requestDescribeArtifact :: DescribeArtifact -> TestTree
requestDescribeArtifact =
  req
    "DescribeArtifact"
    "fixture/DescribeArtifact.yaml"

requestDescribeAutoMLJob :: DescribeAutoMLJob -> TestTree
requestDescribeAutoMLJob =
  req
    "DescribeAutoMLJob"
    "fixture/DescribeAutoMLJob.yaml"

requestDescribeCodeRepository :: DescribeCodeRepository -> TestTree
requestDescribeCodeRepository =
  req
    "DescribeCodeRepository"
    "fixture/DescribeCodeRepository.yaml"

requestDescribeCompilationJob :: DescribeCompilationJob -> TestTree
requestDescribeCompilationJob =
  req
    "DescribeCompilationJob"
    "fixture/DescribeCompilationJob.yaml"

requestDescribeContext :: DescribeContext -> TestTree
requestDescribeContext =
  req
    "DescribeContext"
    "fixture/DescribeContext.yaml"

requestDescribeDataQualityJobDefinition :: DescribeDataQualityJobDefinition -> TestTree
requestDescribeDataQualityJobDefinition =
  req
    "DescribeDataQualityJobDefinition"
    "fixture/DescribeDataQualityJobDefinition.yaml"

requestDescribeDevice :: DescribeDevice -> TestTree
requestDescribeDevice =
  req
    "DescribeDevice"
    "fixture/DescribeDevice.yaml"

requestDescribeDeviceFleet :: DescribeDeviceFleet -> TestTree
requestDescribeDeviceFleet =
  req
    "DescribeDeviceFleet"
    "fixture/DescribeDeviceFleet.yaml"

requestDescribeDomain :: DescribeDomain -> TestTree
requestDescribeDomain =
  req
    "DescribeDomain"
    "fixture/DescribeDomain.yaml"

requestDescribeEdgeDeploymentPlan :: DescribeEdgeDeploymentPlan -> TestTree
requestDescribeEdgeDeploymentPlan =
  req
    "DescribeEdgeDeploymentPlan"
    "fixture/DescribeEdgeDeploymentPlan.yaml"

requestDescribeEdgePackagingJob :: DescribeEdgePackagingJob -> TestTree
requestDescribeEdgePackagingJob =
  req
    "DescribeEdgePackagingJob"
    "fixture/DescribeEdgePackagingJob.yaml"

requestDescribeEndpoint :: DescribeEndpoint -> TestTree
requestDescribeEndpoint =
  req
    "DescribeEndpoint"
    "fixture/DescribeEndpoint.yaml"

requestDescribeEndpointConfig :: DescribeEndpointConfig -> TestTree
requestDescribeEndpointConfig =
  req
    "DescribeEndpointConfig"
    "fixture/DescribeEndpointConfig.yaml"

requestDescribeExperiment :: DescribeExperiment -> TestTree
requestDescribeExperiment =
  req
    "DescribeExperiment"
    "fixture/DescribeExperiment.yaml"

requestDescribeFeatureGroup :: DescribeFeatureGroup -> TestTree
requestDescribeFeatureGroup =
  req
    "DescribeFeatureGroup"
    "fixture/DescribeFeatureGroup.yaml"

requestDescribeFeatureMetadata :: DescribeFeatureMetadata -> TestTree
requestDescribeFeatureMetadata =
  req
    "DescribeFeatureMetadata"
    "fixture/DescribeFeatureMetadata.yaml"

requestDescribeFlowDefinition :: DescribeFlowDefinition -> TestTree
requestDescribeFlowDefinition =
  req
    "DescribeFlowDefinition"
    "fixture/DescribeFlowDefinition.yaml"

requestDescribeHub :: DescribeHub -> TestTree
requestDescribeHub =
  req
    "DescribeHub"
    "fixture/DescribeHub.yaml"

requestDescribeHubContent :: DescribeHubContent -> TestTree
requestDescribeHubContent =
  req
    "DescribeHubContent"
    "fixture/DescribeHubContent.yaml"

requestDescribeHumanTaskUi :: DescribeHumanTaskUi -> TestTree
requestDescribeHumanTaskUi =
  req
    "DescribeHumanTaskUi"
    "fixture/DescribeHumanTaskUi.yaml"

requestDescribeHyperParameterTuningJob :: DescribeHyperParameterTuningJob -> TestTree
requestDescribeHyperParameterTuningJob =
  req
    "DescribeHyperParameterTuningJob"
    "fixture/DescribeHyperParameterTuningJob.yaml"

requestDescribeImage :: DescribeImage -> TestTree
requestDescribeImage =
  req
    "DescribeImage"
    "fixture/DescribeImage.yaml"

requestDescribeImageVersion :: DescribeImageVersion -> TestTree
requestDescribeImageVersion =
  req
    "DescribeImageVersion"
    "fixture/DescribeImageVersion.yaml"

requestDescribeInferenceExperiment :: DescribeInferenceExperiment -> TestTree
requestDescribeInferenceExperiment =
  req
    "DescribeInferenceExperiment"
    "fixture/DescribeInferenceExperiment.yaml"

requestDescribeInferenceRecommendationsJob :: DescribeInferenceRecommendationsJob -> TestTree
requestDescribeInferenceRecommendationsJob =
  req
    "DescribeInferenceRecommendationsJob"
    "fixture/DescribeInferenceRecommendationsJob.yaml"

requestDescribeLabelingJob :: DescribeLabelingJob -> TestTree
requestDescribeLabelingJob =
  req
    "DescribeLabelingJob"
    "fixture/DescribeLabelingJob.yaml"

requestDescribeLineageGroup :: DescribeLineageGroup -> TestTree
requestDescribeLineageGroup =
  req
    "DescribeLineageGroup"
    "fixture/DescribeLineageGroup.yaml"

requestDescribeModel :: DescribeModel -> TestTree
requestDescribeModel =
  req
    "DescribeModel"
    "fixture/DescribeModel.yaml"

requestDescribeModelBiasJobDefinition :: DescribeModelBiasJobDefinition -> TestTree
requestDescribeModelBiasJobDefinition =
  req
    "DescribeModelBiasJobDefinition"
    "fixture/DescribeModelBiasJobDefinition.yaml"

requestDescribeModelCard :: DescribeModelCard -> TestTree
requestDescribeModelCard =
  req
    "DescribeModelCard"
    "fixture/DescribeModelCard.yaml"

requestDescribeModelCardExportJob :: DescribeModelCardExportJob -> TestTree
requestDescribeModelCardExportJob =
  req
    "DescribeModelCardExportJob"
    "fixture/DescribeModelCardExportJob.yaml"

requestDescribeModelExplainabilityJobDefinition :: DescribeModelExplainabilityJobDefinition -> TestTree
requestDescribeModelExplainabilityJobDefinition =
  req
    "DescribeModelExplainabilityJobDefinition"
    "fixture/DescribeModelExplainabilityJobDefinition.yaml"

requestDescribeModelPackage :: DescribeModelPackage -> TestTree
requestDescribeModelPackage =
  req
    "DescribeModelPackage"
    "fixture/DescribeModelPackage.yaml"

requestDescribeModelPackageGroup :: DescribeModelPackageGroup -> TestTree
requestDescribeModelPackageGroup =
  req
    "DescribeModelPackageGroup"
    "fixture/DescribeModelPackageGroup.yaml"

requestDescribeModelQualityJobDefinition :: DescribeModelQualityJobDefinition -> TestTree
requestDescribeModelQualityJobDefinition =
  req
    "DescribeModelQualityJobDefinition"
    "fixture/DescribeModelQualityJobDefinition.yaml"

requestDescribeMonitoringSchedule :: DescribeMonitoringSchedule -> TestTree
requestDescribeMonitoringSchedule =
  req
    "DescribeMonitoringSchedule"
    "fixture/DescribeMonitoringSchedule.yaml"

requestDescribeNotebookInstance :: DescribeNotebookInstance -> TestTree
requestDescribeNotebookInstance =
  req
    "DescribeNotebookInstance"
    "fixture/DescribeNotebookInstance.yaml"

requestDescribeNotebookInstanceLifecycleConfig :: DescribeNotebookInstanceLifecycleConfig -> TestTree
requestDescribeNotebookInstanceLifecycleConfig =
  req
    "DescribeNotebookInstanceLifecycleConfig"
    "fixture/DescribeNotebookInstanceLifecycleConfig.yaml"

requestDescribePipeline :: DescribePipeline -> TestTree
requestDescribePipeline =
  req
    "DescribePipeline"
    "fixture/DescribePipeline.yaml"

requestDescribePipelineDefinitionForExecution :: DescribePipelineDefinitionForExecution -> TestTree
requestDescribePipelineDefinitionForExecution =
  req
    "DescribePipelineDefinitionForExecution"
    "fixture/DescribePipelineDefinitionForExecution.yaml"

requestDescribePipelineExecution :: DescribePipelineExecution -> TestTree
requestDescribePipelineExecution =
  req
    "DescribePipelineExecution"
    "fixture/DescribePipelineExecution.yaml"

requestDescribeProcessingJob :: DescribeProcessingJob -> TestTree
requestDescribeProcessingJob =
  req
    "DescribeProcessingJob"
    "fixture/DescribeProcessingJob.yaml"

requestDescribeProject :: DescribeProject -> TestTree
requestDescribeProject =
  req
    "DescribeProject"
    "fixture/DescribeProject.yaml"

requestDescribeSpace :: DescribeSpace -> TestTree
requestDescribeSpace =
  req
    "DescribeSpace"
    "fixture/DescribeSpace.yaml"

requestDescribeStudioLifecycleConfig :: DescribeStudioLifecycleConfig -> TestTree
requestDescribeStudioLifecycleConfig =
  req
    "DescribeStudioLifecycleConfig"
    "fixture/DescribeStudioLifecycleConfig.yaml"

requestDescribeSubscribedWorkteam :: DescribeSubscribedWorkteam -> TestTree
requestDescribeSubscribedWorkteam =
  req
    "DescribeSubscribedWorkteam"
    "fixture/DescribeSubscribedWorkteam.yaml"

requestDescribeTrainingJob :: DescribeTrainingJob -> TestTree
requestDescribeTrainingJob =
  req
    "DescribeTrainingJob"
    "fixture/DescribeTrainingJob.yaml"

requestDescribeTransformJob :: DescribeTransformJob -> TestTree
requestDescribeTransformJob =
  req
    "DescribeTransformJob"
    "fixture/DescribeTransformJob.yaml"

requestDescribeTrial :: DescribeTrial -> TestTree
requestDescribeTrial =
  req
    "DescribeTrial"
    "fixture/DescribeTrial.yaml"

requestDescribeTrialComponent :: DescribeTrialComponent -> TestTree
requestDescribeTrialComponent =
  req
    "DescribeTrialComponent"
    "fixture/DescribeTrialComponent.yaml"

requestDescribeUserProfile :: DescribeUserProfile -> TestTree
requestDescribeUserProfile =
  req
    "DescribeUserProfile"
    "fixture/DescribeUserProfile.yaml"

requestDescribeWorkforce :: DescribeWorkforce -> TestTree
requestDescribeWorkforce =
  req
    "DescribeWorkforce"
    "fixture/DescribeWorkforce.yaml"

requestDescribeWorkteam :: DescribeWorkteam -> TestTree
requestDescribeWorkteam =
  req
    "DescribeWorkteam"
    "fixture/DescribeWorkteam.yaml"

requestDisableSagemakerServicecatalogPortfolio :: DisableSagemakerServicecatalogPortfolio -> TestTree
requestDisableSagemakerServicecatalogPortfolio =
  req
    "DisableSagemakerServicecatalogPortfolio"
    "fixture/DisableSagemakerServicecatalogPortfolio.yaml"

requestDisassociateTrialComponent :: DisassociateTrialComponent -> TestTree
requestDisassociateTrialComponent =
  req
    "DisassociateTrialComponent"
    "fixture/DisassociateTrialComponent.yaml"

requestEnableSagemakerServicecatalogPortfolio :: EnableSagemakerServicecatalogPortfolio -> TestTree
requestEnableSagemakerServicecatalogPortfolio =
  req
    "EnableSagemakerServicecatalogPortfolio"
    "fixture/EnableSagemakerServicecatalogPortfolio.yaml"

requestGetDeviceFleetReport :: GetDeviceFleetReport -> TestTree
requestGetDeviceFleetReport =
  req
    "GetDeviceFleetReport"
    "fixture/GetDeviceFleetReport.yaml"

requestGetLineageGroupPolicy :: GetLineageGroupPolicy -> TestTree
requestGetLineageGroupPolicy =
  req
    "GetLineageGroupPolicy"
    "fixture/GetLineageGroupPolicy.yaml"

requestGetModelPackageGroupPolicy :: GetModelPackageGroupPolicy -> TestTree
requestGetModelPackageGroupPolicy =
  req
    "GetModelPackageGroupPolicy"
    "fixture/GetModelPackageGroupPolicy.yaml"

requestGetSagemakerServicecatalogPortfolioStatus :: GetSagemakerServicecatalogPortfolioStatus -> TestTree
requestGetSagemakerServicecatalogPortfolioStatus =
  req
    "GetSagemakerServicecatalogPortfolioStatus"
    "fixture/GetSagemakerServicecatalogPortfolioStatus.yaml"

requestGetSearchSuggestions :: GetSearchSuggestions -> TestTree
requestGetSearchSuggestions =
  req
    "GetSearchSuggestions"
    "fixture/GetSearchSuggestions.yaml"

requestImportHubContent :: ImportHubContent -> TestTree
requestImportHubContent =
  req
    "ImportHubContent"
    "fixture/ImportHubContent.yaml"

requestListActions :: ListActions -> TestTree
requestListActions =
  req
    "ListActions"
    "fixture/ListActions.yaml"

requestListAlgorithms :: ListAlgorithms -> TestTree
requestListAlgorithms =
  req
    "ListAlgorithms"
    "fixture/ListAlgorithms.yaml"

requestListAppImageConfigs :: ListAppImageConfigs -> TestTree
requestListAppImageConfigs =
  req
    "ListAppImageConfigs"
    "fixture/ListAppImageConfigs.yaml"

requestListApps :: ListApps -> TestTree
requestListApps =
  req
    "ListApps"
    "fixture/ListApps.yaml"

requestListArtifacts :: ListArtifacts -> TestTree
requestListArtifacts =
  req
    "ListArtifacts"
    "fixture/ListArtifacts.yaml"

requestListAssociations :: ListAssociations -> TestTree
requestListAssociations =
  req
    "ListAssociations"
    "fixture/ListAssociations.yaml"

requestListAutoMLJobs :: ListAutoMLJobs -> TestTree
requestListAutoMLJobs =
  req
    "ListAutoMLJobs"
    "fixture/ListAutoMLJobs.yaml"

requestListCandidatesForAutoMLJob :: ListCandidatesForAutoMLJob -> TestTree
requestListCandidatesForAutoMLJob =
  req
    "ListCandidatesForAutoMLJob"
    "fixture/ListCandidatesForAutoMLJob.yaml"

requestListCodeRepositories :: ListCodeRepositories -> TestTree
requestListCodeRepositories =
  req
    "ListCodeRepositories"
    "fixture/ListCodeRepositories.yaml"

requestListCompilationJobs :: ListCompilationJobs -> TestTree
requestListCompilationJobs =
  req
    "ListCompilationJobs"
    "fixture/ListCompilationJobs.yaml"

requestListContexts :: ListContexts -> TestTree
requestListContexts =
  req
    "ListContexts"
    "fixture/ListContexts.yaml"

requestListDataQualityJobDefinitions :: ListDataQualityJobDefinitions -> TestTree
requestListDataQualityJobDefinitions =
  req
    "ListDataQualityJobDefinitions"
    "fixture/ListDataQualityJobDefinitions.yaml"

requestListDeviceFleets :: ListDeviceFleets -> TestTree
requestListDeviceFleets =
  req
    "ListDeviceFleets"
    "fixture/ListDeviceFleets.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices =
  req
    "ListDevices"
    "fixture/ListDevices.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

requestListEdgeDeploymentPlans :: ListEdgeDeploymentPlans -> TestTree
requestListEdgeDeploymentPlans =
  req
    "ListEdgeDeploymentPlans"
    "fixture/ListEdgeDeploymentPlans.yaml"

requestListEdgePackagingJobs :: ListEdgePackagingJobs -> TestTree
requestListEdgePackagingJobs =
  req
    "ListEdgePackagingJobs"
    "fixture/ListEdgePackagingJobs.yaml"

requestListEndpointConfigs :: ListEndpointConfigs -> TestTree
requestListEndpointConfigs =
  req
    "ListEndpointConfigs"
    "fixture/ListEndpointConfigs.yaml"

requestListEndpoints :: ListEndpoints -> TestTree
requestListEndpoints =
  req
    "ListEndpoints"
    "fixture/ListEndpoints.yaml"

requestListExperiments :: ListExperiments -> TestTree
requestListExperiments =
  req
    "ListExperiments"
    "fixture/ListExperiments.yaml"

requestListFeatureGroups :: ListFeatureGroups -> TestTree
requestListFeatureGroups =
  req
    "ListFeatureGroups"
    "fixture/ListFeatureGroups.yaml"

requestListFlowDefinitions :: ListFlowDefinitions -> TestTree
requestListFlowDefinitions =
  req
    "ListFlowDefinitions"
    "fixture/ListFlowDefinitions.yaml"

requestListHubContentVersions :: ListHubContentVersions -> TestTree
requestListHubContentVersions =
  req
    "ListHubContentVersions"
    "fixture/ListHubContentVersions.yaml"

requestListHubContents :: ListHubContents -> TestTree
requestListHubContents =
  req
    "ListHubContents"
    "fixture/ListHubContents.yaml"

requestListHubs :: ListHubs -> TestTree
requestListHubs =
  req
    "ListHubs"
    "fixture/ListHubs.yaml"

requestListHumanTaskUis :: ListHumanTaskUis -> TestTree
requestListHumanTaskUis =
  req
    "ListHumanTaskUis"
    "fixture/ListHumanTaskUis.yaml"

requestListHyperParameterTuningJobs :: ListHyperParameterTuningJobs -> TestTree
requestListHyperParameterTuningJobs =
  req
    "ListHyperParameterTuningJobs"
    "fixture/ListHyperParameterTuningJobs.yaml"

requestListImageVersions :: ListImageVersions -> TestTree
requestListImageVersions =
  req
    "ListImageVersions"
    "fixture/ListImageVersions.yaml"

requestListImages :: ListImages -> TestTree
requestListImages =
  req
    "ListImages"
    "fixture/ListImages.yaml"

requestListInferenceExperiments :: ListInferenceExperiments -> TestTree
requestListInferenceExperiments =
  req
    "ListInferenceExperiments"
    "fixture/ListInferenceExperiments.yaml"

requestListInferenceRecommendationsJobSteps :: ListInferenceRecommendationsJobSteps -> TestTree
requestListInferenceRecommendationsJobSteps =
  req
    "ListInferenceRecommendationsJobSteps"
    "fixture/ListInferenceRecommendationsJobSteps.yaml"

requestListInferenceRecommendationsJobs :: ListInferenceRecommendationsJobs -> TestTree
requestListInferenceRecommendationsJobs =
  req
    "ListInferenceRecommendationsJobs"
    "fixture/ListInferenceRecommendationsJobs.yaml"

requestListLabelingJobs :: ListLabelingJobs -> TestTree
requestListLabelingJobs =
  req
    "ListLabelingJobs"
    "fixture/ListLabelingJobs.yaml"

requestListLabelingJobsForWorkteam :: ListLabelingJobsForWorkteam -> TestTree
requestListLabelingJobsForWorkteam =
  req
    "ListLabelingJobsForWorkteam"
    "fixture/ListLabelingJobsForWorkteam.yaml"

requestListLineageGroups :: ListLineageGroups -> TestTree
requestListLineageGroups =
  req
    "ListLineageGroups"
    "fixture/ListLineageGroups.yaml"

requestListModelBiasJobDefinitions :: ListModelBiasJobDefinitions -> TestTree
requestListModelBiasJobDefinitions =
  req
    "ListModelBiasJobDefinitions"
    "fixture/ListModelBiasJobDefinitions.yaml"

requestListModelCardExportJobs :: ListModelCardExportJobs -> TestTree
requestListModelCardExportJobs =
  req
    "ListModelCardExportJobs"
    "fixture/ListModelCardExportJobs.yaml"

requestListModelCardVersions :: ListModelCardVersions -> TestTree
requestListModelCardVersions =
  req
    "ListModelCardVersions"
    "fixture/ListModelCardVersions.yaml"

requestListModelCards :: ListModelCards -> TestTree
requestListModelCards =
  req
    "ListModelCards"
    "fixture/ListModelCards.yaml"

requestListModelExplainabilityJobDefinitions :: ListModelExplainabilityJobDefinitions -> TestTree
requestListModelExplainabilityJobDefinitions =
  req
    "ListModelExplainabilityJobDefinitions"
    "fixture/ListModelExplainabilityJobDefinitions.yaml"

requestListModelMetadata :: ListModelMetadata -> TestTree
requestListModelMetadata =
  req
    "ListModelMetadata"
    "fixture/ListModelMetadata.yaml"

requestListModelPackageGroups :: ListModelPackageGroups -> TestTree
requestListModelPackageGroups =
  req
    "ListModelPackageGroups"
    "fixture/ListModelPackageGroups.yaml"

requestListModelPackages :: ListModelPackages -> TestTree
requestListModelPackages =
  req
    "ListModelPackages"
    "fixture/ListModelPackages.yaml"

requestListModelQualityJobDefinitions :: ListModelQualityJobDefinitions -> TestTree
requestListModelQualityJobDefinitions =
  req
    "ListModelQualityJobDefinitions"
    "fixture/ListModelQualityJobDefinitions.yaml"

requestListModels :: ListModels -> TestTree
requestListModels =
  req
    "ListModels"
    "fixture/ListModels.yaml"

requestListMonitoringAlertHistory :: ListMonitoringAlertHistory -> TestTree
requestListMonitoringAlertHistory =
  req
    "ListMonitoringAlertHistory"
    "fixture/ListMonitoringAlertHistory.yaml"

requestListMonitoringAlerts :: ListMonitoringAlerts -> TestTree
requestListMonitoringAlerts =
  req
    "ListMonitoringAlerts"
    "fixture/ListMonitoringAlerts.yaml"

requestListMonitoringExecutions :: ListMonitoringExecutions -> TestTree
requestListMonitoringExecutions =
  req
    "ListMonitoringExecutions"
    "fixture/ListMonitoringExecutions.yaml"

requestListMonitoringSchedules :: ListMonitoringSchedules -> TestTree
requestListMonitoringSchedules =
  req
    "ListMonitoringSchedules"
    "fixture/ListMonitoringSchedules.yaml"

requestListNotebookInstanceLifecycleConfigs :: ListNotebookInstanceLifecycleConfigs -> TestTree
requestListNotebookInstanceLifecycleConfigs =
  req
    "ListNotebookInstanceLifecycleConfigs"
    "fixture/ListNotebookInstanceLifecycleConfigs.yaml"

requestListNotebookInstances :: ListNotebookInstances -> TestTree
requestListNotebookInstances =
  req
    "ListNotebookInstances"
    "fixture/ListNotebookInstances.yaml"

requestListPipelineExecutionSteps :: ListPipelineExecutionSteps -> TestTree
requestListPipelineExecutionSteps =
  req
    "ListPipelineExecutionSteps"
    "fixture/ListPipelineExecutionSteps.yaml"

requestListPipelineExecutions :: ListPipelineExecutions -> TestTree
requestListPipelineExecutions =
  req
    "ListPipelineExecutions"
    "fixture/ListPipelineExecutions.yaml"

requestListPipelineParametersForExecution :: ListPipelineParametersForExecution -> TestTree
requestListPipelineParametersForExecution =
  req
    "ListPipelineParametersForExecution"
    "fixture/ListPipelineParametersForExecution.yaml"

requestListPipelines :: ListPipelines -> TestTree
requestListPipelines =
  req
    "ListPipelines"
    "fixture/ListPipelines.yaml"

requestListProcessingJobs :: ListProcessingJobs -> TestTree
requestListProcessingJobs =
  req
    "ListProcessingJobs"
    "fixture/ListProcessingJobs.yaml"

requestListProjects :: ListProjects -> TestTree
requestListProjects =
  req
    "ListProjects"
    "fixture/ListProjects.yaml"

requestListSpaces :: ListSpaces -> TestTree
requestListSpaces =
  req
    "ListSpaces"
    "fixture/ListSpaces.yaml"

requestListStageDevices :: ListStageDevices -> TestTree
requestListStageDevices =
  req
    "ListStageDevices"
    "fixture/ListStageDevices.yaml"

requestListStudioLifecycleConfigs :: ListStudioLifecycleConfigs -> TestTree
requestListStudioLifecycleConfigs =
  req
    "ListStudioLifecycleConfigs"
    "fixture/ListStudioLifecycleConfigs.yaml"

requestListSubscribedWorkteams :: ListSubscribedWorkteams -> TestTree
requestListSubscribedWorkteams =
  req
    "ListSubscribedWorkteams"
    "fixture/ListSubscribedWorkteams.yaml"

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestListTrainingJobs :: ListTrainingJobs -> TestTree
requestListTrainingJobs =
  req
    "ListTrainingJobs"
    "fixture/ListTrainingJobs.yaml"

requestListTrainingJobsForHyperParameterTuningJob :: ListTrainingJobsForHyperParameterTuningJob -> TestTree
requestListTrainingJobsForHyperParameterTuningJob =
  req
    "ListTrainingJobsForHyperParameterTuningJob"
    "fixture/ListTrainingJobsForHyperParameterTuningJob.yaml"

requestListTransformJobs :: ListTransformJobs -> TestTree
requestListTransformJobs =
  req
    "ListTransformJobs"
    "fixture/ListTransformJobs.yaml"

requestListTrialComponents :: ListTrialComponents -> TestTree
requestListTrialComponents =
  req
    "ListTrialComponents"
    "fixture/ListTrialComponents.yaml"

requestListTrials :: ListTrials -> TestTree
requestListTrials =
  req
    "ListTrials"
    "fixture/ListTrials.yaml"

requestListUserProfiles :: ListUserProfiles -> TestTree
requestListUserProfiles =
  req
    "ListUserProfiles"
    "fixture/ListUserProfiles.yaml"

requestListWorkforces :: ListWorkforces -> TestTree
requestListWorkforces =
  req
    "ListWorkforces"
    "fixture/ListWorkforces.yaml"

requestListWorkteams :: ListWorkteams -> TestTree
requestListWorkteams =
  req
    "ListWorkteams"
    "fixture/ListWorkteams.yaml"

requestPutModelPackageGroupPolicy :: PutModelPackageGroupPolicy -> TestTree
requestPutModelPackageGroupPolicy =
  req
    "PutModelPackageGroupPolicy"
    "fixture/PutModelPackageGroupPolicy.yaml"

requestQueryLineage :: QueryLineage -> TestTree
requestQueryLineage =
  req
    "QueryLineage"
    "fixture/QueryLineage.yaml"

requestRegisterDevices :: RegisterDevices -> TestTree
requestRegisterDevices =
  req
    "RegisterDevices"
    "fixture/RegisterDevices.yaml"

requestRenderUiTemplate :: RenderUiTemplate -> TestTree
requestRenderUiTemplate =
  req
    "RenderUiTemplate"
    "fixture/RenderUiTemplate.yaml"

requestRetryPipelineExecution :: RetryPipelineExecution -> TestTree
requestRetryPipelineExecution =
  req
    "RetryPipelineExecution"
    "fixture/RetryPipelineExecution.yaml"

requestSearch :: Search -> TestTree
requestSearch =
  req
    "Search"
    "fixture/Search.yaml"

requestSendPipelineExecutionStepFailure :: SendPipelineExecutionStepFailure -> TestTree
requestSendPipelineExecutionStepFailure =
  req
    "SendPipelineExecutionStepFailure"
    "fixture/SendPipelineExecutionStepFailure.yaml"

requestSendPipelineExecutionStepSuccess :: SendPipelineExecutionStepSuccess -> TestTree
requestSendPipelineExecutionStepSuccess =
  req
    "SendPipelineExecutionStepSuccess"
    "fixture/SendPipelineExecutionStepSuccess.yaml"

requestStartEdgeDeploymentStage :: StartEdgeDeploymentStage -> TestTree
requestStartEdgeDeploymentStage =
  req
    "StartEdgeDeploymentStage"
    "fixture/StartEdgeDeploymentStage.yaml"

requestStartInferenceExperiment :: StartInferenceExperiment -> TestTree
requestStartInferenceExperiment =
  req
    "StartInferenceExperiment"
    "fixture/StartInferenceExperiment.yaml"

requestStartMonitoringSchedule :: StartMonitoringSchedule -> TestTree
requestStartMonitoringSchedule =
  req
    "StartMonitoringSchedule"
    "fixture/StartMonitoringSchedule.yaml"

requestStartNotebookInstance :: StartNotebookInstance -> TestTree
requestStartNotebookInstance =
  req
    "StartNotebookInstance"
    "fixture/StartNotebookInstance.yaml"

requestStartPipelineExecution :: StartPipelineExecution -> TestTree
requestStartPipelineExecution =
  req
    "StartPipelineExecution"
    "fixture/StartPipelineExecution.yaml"

requestStopAutoMLJob :: StopAutoMLJob -> TestTree
requestStopAutoMLJob =
  req
    "StopAutoMLJob"
    "fixture/StopAutoMLJob.yaml"

requestStopCompilationJob :: StopCompilationJob -> TestTree
requestStopCompilationJob =
  req
    "StopCompilationJob"
    "fixture/StopCompilationJob.yaml"

requestStopEdgeDeploymentStage :: StopEdgeDeploymentStage -> TestTree
requestStopEdgeDeploymentStage =
  req
    "StopEdgeDeploymentStage"
    "fixture/StopEdgeDeploymentStage.yaml"

requestStopEdgePackagingJob :: StopEdgePackagingJob -> TestTree
requestStopEdgePackagingJob =
  req
    "StopEdgePackagingJob"
    "fixture/StopEdgePackagingJob.yaml"

requestStopHyperParameterTuningJob :: StopHyperParameterTuningJob -> TestTree
requestStopHyperParameterTuningJob =
  req
    "StopHyperParameterTuningJob"
    "fixture/StopHyperParameterTuningJob.yaml"

requestStopInferenceExperiment :: StopInferenceExperiment -> TestTree
requestStopInferenceExperiment =
  req
    "StopInferenceExperiment"
    "fixture/StopInferenceExperiment.yaml"

requestStopInferenceRecommendationsJob :: StopInferenceRecommendationsJob -> TestTree
requestStopInferenceRecommendationsJob =
  req
    "StopInferenceRecommendationsJob"
    "fixture/StopInferenceRecommendationsJob.yaml"

requestStopLabelingJob :: StopLabelingJob -> TestTree
requestStopLabelingJob =
  req
    "StopLabelingJob"
    "fixture/StopLabelingJob.yaml"

requestStopMonitoringSchedule :: StopMonitoringSchedule -> TestTree
requestStopMonitoringSchedule =
  req
    "StopMonitoringSchedule"
    "fixture/StopMonitoringSchedule.yaml"

requestStopNotebookInstance :: StopNotebookInstance -> TestTree
requestStopNotebookInstance =
  req
    "StopNotebookInstance"
    "fixture/StopNotebookInstance.yaml"

requestStopPipelineExecution :: StopPipelineExecution -> TestTree
requestStopPipelineExecution =
  req
    "StopPipelineExecution"
    "fixture/StopPipelineExecution.yaml"

requestStopProcessingJob :: StopProcessingJob -> TestTree
requestStopProcessingJob =
  req
    "StopProcessingJob"
    "fixture/StopProcessingJob.yaml"

requestStopTrainingJob :: StopTrainingJob -> TestTree
requestStopTrainingJob =
  req
    "StopTrainingJob"
    "fixture/StopTrainingJob.yaml"

requestStopTransformJob :: StopTransformJob -> TestTree
requestStopTransformJob =
  req
    "StopTransformJob"
    "fixture/StopTransformJob.yaml"

requestUpdateAction :: UpdateAction -> TestTree
requestUpdateAction =
  req
    "UpdateAction"
    "fixture/UpdateAction.yaml"

requestUpdateAppImageConfig :: UpdateAppImageConfig -> TestTree
requestUpdateAppImageConfig =
  req
    "UpdateAppImageConfig"
    "fixture/UpdateAppImageConfig.yaml"

requestUpdateArtifact :: UpdateArtifact -> TestTree
requestUpdateArtifact =
  req
    "UpdateArtifact"
    "fixture/UpdateArtifact.yaml"

requestUpdateCodeRepository :: UpdateCodeRepository -> TestTree
requestUpdateCodeRepository =
  req
    "UpdateCodeRepository"
    "fixture/UpdateCodeRepository.yaml"

requestUpdateContext :: UpdateContext -> TestTree
requestUpdateContext =
  req
    "UpdateContext"
    "fixture/UpdateContext.yaml"

requestUpdateDeviceFleet :: UpdateDeviceFleet -> TestTree
requestUpdateDeviceFleet =
  req
    "UpdateDeviceFleet"
    "fixture/UpdateDeviceFleet.yaml"

requestUpdateDevices :: UpdateDevices -> TestTree
requestUpdateDevices =
  req
    "UpdateDevices"
    "fixture/UpdateDevices.yaml"

requestUpdateDomain :: UpdateDomain -> TestTree
requestUpdateDomain =
  req
    "UpdateDomain"
    "fixture/UpdateDomain.yaml"

requestUpdateEndpoint :: UpdateEndpoint -> TestTree
requestUpdateEndpoint =
  req
    "UpdateEndpoint"
    "fixture/UpdateEndpoint.yaml"

requestUpdateEndpointWeightsAndCapacities :: UpdateEndpointWeightsAndCapacities -> TestTree
requestUpdateEndpointWeightsAndCapacities =
  req
    "UpdateEndpointWeightsAndCapacities"
    "fixture/UpdateEndpointWeightsAndCapacities.yaml"

requestUpdateExperiment :: UpdateExperiment -> TestTree
requestUpdateExperiment =
  req
    "UpdateExperiment"
    "fixture/UpdateExperiment.yaml"

requestUpdateFeatureGroup :: UpdateFeatureGroup -> TestTree
requestUpdateFeatureGroup =
  req
    "UpdateFeatureGroup"
    "fixture/UpdateFeatureGroup.yaml"

requestUpdateFeatureMetadata :: UpdateFeatureMetadata -> TestTree
requestUpdateFeatureMetadata =
  req
    "UpdateFeatureMetadata"
    "fixture/UpdateFeatureMetadata.yaml"

requestUpdateHub :: UpdateHub -> TestTree
requestUpdateHub =
  req
    "UpdateHub"
    "fixture/UpdateHub.yaml"

requestUpdateImage :: UpdateImage -> TestTree
requestUpdateImage =
  req
    "UpdateImage"
    "fixture/UpdateImage.yaml"

requestUpdateInferenceExperiment :: UpdateInferenceExperiment -> TestTree
requestUpdateInferenceExperiment =
  req
    "UpdateInferenceExperiment"
    "fixture/UpdateInferenceExperiment.yaml"

requestUpdateModelCard :: UpdateModelCard -> TestTree
requestUpdateModelCard =
  req
    "UpdateModelCard"
    "fixture/UpdateModelCard.yaml"

requestUpdateModelPackage :: UpdateModelPackage -> TestTree
requestUpdateModelPackage =
  req
    "UpdateModelPackage"
    "fixture/UpdateModelPackage.yaml"

requestUpdateMonitoringAlert :: UpdateMonitoringAlert -> TestTree
requestUpdateMonitoringAlert =
  req
    "UpdateMonitoringAlert"
    "fixture/UpdateMonitoringAlert.yaml"

requestUpdateMonitoringSchedule :: UpdateMonitoringSchedule -> TestTree
requestUpdateMonitoringSchedule =
  req
    "UpdateMonitoringSchedule"
    "fixture/UpdateMonitoringSchedule.yaml"

requestUpdateNotebookInstance :: UpdateNotebookInstance -> TestTree
requestUpdateNotebookInstance =
  req
    "UpdateNotebookInstance"
    "fixture/UpdateNotebookInstance.yaml"

requestUpdateNotebookInstanceLifecycleConfig :: UpdateNotebookInstanceLifecycleConfig -> TestTree
requestUpdateNotebookInstanceLifecycleConfig =
  req
    "UpdateNotebookInstanceLifecycleConfig"
    "fixture/UpdateNotebookInstanceLifecycleConfig.yaml"

requestUpdatePipeline :: UpdatePipeline -> TestTree
requestUpdatePipeline =
  req
    "UpdatePipeline"
    "fixture/UpdatePipeline.yaml"

requestUpdatePipelineExecution :: UpdatePipelineExecution -> TestTree
requestUpdatePipelineExecution =
  req
    "UpdatePipelineExecution"
    "fixture/UpdatePipelineExecution.yaml"

requestUpdateProject :: UpdateProject -> TestTree
requestUpdateProject =
  req
    "UpdateProject"
    "fixture/UpdateProject.yaml"

requestUpdateSpace :: UpdateSpace -> TestTree
requestUpdateSpace =
  req
    "UpdateSpace"
    "fixture/UpdateSpace.yaml"

requestUpdateTrainingJob :: UpdateTrainingJob -> TestTree
requestUpdateTrainingJob =
  req
    "UpdateTrainingJob"
    "fixture/UpdateTrainingJob.yaml"

requestUpdateTrial :: UpdateTrial -> TestTree
requestUpdateTrial =
  req
    "UpdateTrial"
    "fixture/UpdateTrial.yaml"

requestUpdateTrialComponent :: UpdateTrialComponent -> TestTree
requestUpdateTrialComponent =
  req
    "UpdateTrialComponent"
    "fixture/UpdateTrialComponent.yaml"

requestUpdateUserProfile :: UpdateUserProfile -> TestTree
requestUpdateUserProfile =
  req
    "UpdateUserProfile"
    "fixture/UpdateUserProfile.yaml"

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

-- Responses

responseAddAssociation :: AddAssociationResponse -> TestTree
responseAddAssociation =
  res
    "AddAssociationResponse"
    "fixture/AddAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddAssociation)

responseAddTags :: AddTagsResponse -> TestTree
responseAddTags =
  res
    "AddTagsResponse"
    "fixture/AddTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddTags)

responseAssociateTrialComponent :: AssociateTrialComponentResponse -> TestTree
responseAssociateTrialComponent =
  res
    "AssociateTrialComponentResponse"
    "fixture/AssociateTrialComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateTrialComponent)

responseBatchDescribeModelPackage :: BatchDescribeModelPackageResponse -> TestTree
responseBatchDescribeModelPackage =
  res
    "BatchDescribeModelPackageResponse"
    "fixture/BatchDescribeModelPackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDescribeModelPackage)

responseCreateAction :: CreateActionResponse -> TestTree
responseCreateAction =
  res
    "CreateActionResponse"
    "fixture/CreateActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAction)

responseCreateAlgorithm :: CreateAlgorithmResponse -> TestTree
responseCreateAlgorithm =
  res
    "CreateAlgorithmResponse"
    "fixture/CreateAlgorithmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAlgorithm)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApp)

responseCreateAppImageConfig :: CreateAppImageConfigResponse -> TestTree
responseCreateAppImageConfig =
  res
    "CreateAppImageConfigResponse"
    "fixture/CreateAppImageConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAppImageConfig)

responseCreateArtifact :: CreateArtifactResponse -> TestTree
responseCreateArtifact =
  res
    "CreateArtifactResponse"
    "fixture/CreateArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateArtifact)

responseCreateAutoMLJob :: CreateAutoMLJobResponse -> TestTree
responseCreateAutoMLJob =
  res
    "CreateAutoMLJobResponse"
    "fixture/CreateAutoMLJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAutoMLJob)

responseCreateCodeRepository :: CreateCodeRepositoryResponse -> TestTree
responseCreateCodeRepository =
  res
    "CreateCodeRepositoryResponse"
    "fixture/CreateCodeRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCodeRepository)

responseCreateCompilationJob :: CreateCompilationJobResponse -> TestTree
responseCreateCompilationJob =
  res
    "CreateCompilationJobResponse"
    "fixture/CreateCompilationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCompilationJob)

responseCreateContext :: CreateContextResponse -> TestTree
responseCreateContext =
  res
    "CreateContextResponse"
    "fixture/CreateContextResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateContext)

responseCreateDataQualityJobDefinition :: CreateDataQualityJobDefinitionResponse -> TestTree
responseCreateDataQualityJobDefinition =
  res
    "CreateDataQualityJobDefinitionResponse"
    "fixture/CreateDataQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDataQualityJobDefinition)

responseCreateDeviceFleet :: CreateDeviceFleetResponse -> TestTree
responseCreateDeviceFleet =
  res
    "CreateDeviceFleetResponse"
    "fixture/CreateDeviceFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeviceFleet)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomain)

responseCreateEdgeDeploymentPlan :: CreateEdgeDeploymentPlanResponse -> TestTree
responseCreateEdgeDeploymentPlan =
  res
    "CreateEdgeDeploymentPlanResponse"
    "fixture/CreateEdgeDeploymentPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEdgeDeploymentPlan)

responseCreateEdgeDeploymentStage :: CreateEdgeDeploymentStageResponse -> TestTree
responseCreateEdgeDeploymentStage =
  res
    "CreateEdgeDeploymentStageResponse"
    "fixture/CreateEdgeDeploymentStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEdgeDeploymentStage)

responseCreateEdgePackagingJob :: CreateEdgePackagingJobResponse -> TestTree
responseCreateEdgePackagingJob =
  res
    "CreateEdgePackagingJobResponse"
    "fixture/CreateEdgePackagingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEdgePackagingJob)

responseCreateEndpoint :: CreateEndpointResponse -> TestTree
responseCreateEndpoint =
  res
    "CreateEndpointResponse"
    "fixture/CreateEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEndpoint)

responseCreateEndpointConfig :: CreateEndpointConfigResponse -> TestTree
responseCreateEndpointConfig =
  res
    "CreateEndpointConfigResponse"
    "fixture/CreateEndpointConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEndpointConfig)

responseCreateExperiment :: CreateExperimentResponse -> TestTree
responseCreateExperiment =
  res
    "CreateExperimentResponse"
    "fixture/CreateExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateExperiment)

responseCreateFeatureGroup :: CreateFeatureGroupResponse -> TestTree
responseCreateFeatureGroup =
  res
    "CreateFeatureGroupResponse"
    "fixture/CreateFeatureGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFeatureGroup)

responseCreateFlowDefinition :: CreateFlowDefinitionResponse -> TestTree
responseCreateFlowDefinition =
  res
    "CreateFlowDefinitionResponse"
    "fixture/CreateFlowDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFlowDefinition)

responseCreateHub :: CreateHubResponse -> TestTree
responseCreateHub =
  res
    "CreateHubResponse"
    "fixture/CreateHubResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHub)

responseCreateHumanTaskUi :: CreateHumanTaskUiResponse -> TestTree
responseCreateHumanTaskUi =
  res
    "CreateHumanTaskUiResponse"
    "fixture/CreateHumanTaskUiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHumanTaskUi)

responseCreateHyperParameterTuningJob :: CreateHyperParameterTuningJobResponse -> TestTree
responseCreateHyperParameterTuningJob =
  res
    "CreateHyperParameterTuningJobResponse"
    "fixture/CreateHyperParameterTuningJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHyperParameterTuningJob)

responseCreateImage :: CreateImageResponse -> TestTree
responseCreateImage =
  res
    "CreateImageResponse"
    "fixture/CreateImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateImage)

responseCreateImageVersion :: CreateImageVersionResponse -> TestTree
responseCreateImageVersion =
  res
    "CreateImageVersionResponse"
    "fixture/CreateImageVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateImageVersion)

responseCreateInferenceExperiment :: CreateInferenceExperimentResponse -> TestTree
responseCreateInferenceExperiment =
  res
    "CreateInferenceExperimentResponse"
    "fixture/CreateInferenceExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInferenceExperiment)

responseCreateInferenceRecommendationsJob :: CreateInferenceRecommendationsJobResponse -> TestTree
responseCreateInferenceRecommendationsJob =
  res
    "CreateInferenceRecommendationsJobResponse"
    "fixture/CreateInferenceRecommendationsJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInferenceRecommendationsJob)

responseCreateLabelingJob :: CreateLabelingJobResponse -> TestTree
responseCreateLabelingJob =
  res
    "CreateLabelingJobResponse"
    "fixture/CreateLabelingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLabelingJob)

responseCreateModel :: CreateModelResponse -> TestTree
responseCreateModel =
  res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModel)

responseCreateModelBiasJobDefinition :: CreateModelBiasJobDefinitionResponse -> TestTree
responseCreateModelBiasJobDefinition =
  res
    "CreateModelBiasJobDefinitionResponse"
    "fixture/CreateModelBiasJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModelBiasJobDefinition)

responseCreateModelCard :: CreateModelCardResponse -> TestTree
responseCreateModelCard =
  res
    "CreateModelCardResponse"
    "fixture/CreateModelCardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModelCard)

responseCreateModelCardExportJob :: CreateModelCardExportJobResponse -> TestTree
responseCreateModelCardExportJob =
  res
    "CreateModelCardExportJobResponse"
    "fixture/CreateModelCardExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModelCardExportJob)

responseCreateModelExplainabilityJobDefinition :: CreateModelExplainabilityJobDefinitionResponse -> TestTree
responseCreateModelExplainabilityJobDefinition =
  res
    "CreateModelExplainabilityJobDefinitionResponse"
    "fixture/CreateModelExplainabilityJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModelExplainabilityJobDefinition)

responseCreateModelPackage :: CreateModelPackageResponse -> TestTree
responseCreateModelPackage =
  res
    "CreateModelPackageResponse"
    "fixture/CreateModelPackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModelPackage)

responseCreateModelPackageGroup :: CreateModelPackageGroupResponse -> TestTree
responseCreateModelPackageGroup =
  res
    "CreateModelPackageGroupResponse"
    "fixture/CreateModelPackageGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModelPackageGroup)

responseCreateModelQualityJobDefinition :: CreateModelQualityJobDefinitionResponse -> TestTree
responseCreateModelQualityJobDefinition =
  res
    "CreateModelQualityJobDefinitionResponse"
    "fixture/CreateModelQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateModelQualityJobDefinition)

responseCreateMonitoringSchedule :: CreateMonitoringScheduleResponse -> TestTree
responseCreateMonitoringSchedule =
  res
    "CreateMonitoringScheduleResponse"
    "fixture/CreateMonitoringScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMonitoringSchedule)

responseCreateNotebookInstance :: CreateNotebookInstanceResponse -> TestTree
responseCreateNotebookInstance =
  res
    "CreateNotebookInstanceResponse"
    "fixture/CreateNotebookInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNotebookInstance)

responseCreateNotebookInstanceLifecycleConfig :: CreateNotebookInstanceLifecycleConfigResponse -> TestTree
responseCreateNotebookInstanceLifecycleConfig =
  res
    "CreateNotebookInstanceLifecycleConfigResponse"
    "fixture/CreateNotebookInstanceLifecycleConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNotebookInstanceLifecycleConfig)

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline =
  res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePipeline)

responseCreatePresignedDomainUrl :: CreatePresignedDomainUrlResponse -> TestTree
responseCreatePresignedDomainUrl =
  res
    "CreatePresignedDomainUrlResponse"
    "fixture/CreatePresignedDomainUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePresignedDomainUrl)

responseCreatePresignedNotebookInstanceUrl :: CreatePresignedNotebookInstanceUrlResponse -> TestTree
responseCreatePresignedNotebookInstanceUrl =
  res
    "CreatePresignedNotebookInstanceUrlResponse"
    "fixture/CreatePresignedNotebookInstanceUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePresignedNotebookInstanceUrl)

responseCreateProcessingJob :: CreateProcessingJobResponse -> TestTree
responseCreateProcessingJob =
  res
    "CreateProcessingJobResponse"
    "fixture/CreateProcessingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProcessingJob)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProject)

responseCreateSpace :: CreateSpaceResponse -> TestTree
responseCreateSpace =
  res
    "CreateSpaceResponse"
    "fixture/CreateSpaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSpace)

responseCreateStudioLifecycleConfig :: CreateStudioLifecycleConfigResponse -> TestTree
responseCreateStudioLifecycleConfig =
  res
    "CreateStudioLifecycleConfigResponse"
    "fixture/CreateStudioLifecycleConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStudioLifecycleConfig)

responseCreateTrainingJob :: CreateTrainingJobResponse -> TestTree
responseCreateTrainingJob =
  res
    "CreateTrainingJobResponse"
    "fixture/CreateTrainingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrainingJob)

responseCreateTransformJob :: CreateTransformJobResponse -> TestTree
responseCreateTransformJob =
  res
    "CreateTransformJobResponse"
    "fixture/CreateTransformJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTransformJob)

responseCreateTrial :: CreateTrialResponse -> TestTree
responseCreateTrial =
  res
    "CreateTrialResponse"
    "fixture/CreateTrialResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrial)

responseCreateTrialComponent :: CreateTrialComponentResponse -> TestTree
responseCreateTrialComponent =
  res
    "CreateTrialComponentResponse"
    "fixture/CreateTrialComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrialComponent)

responseCreateUserProfile :: CreateUserProfileResponse -> TestTree
responseCreateUserProfile =
  res
    "CreateUserProfileResponse"
    "fixture/CreateUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUserProfile)

responseCreateWorkforce :: CreateWorkforceResponse -> TestTree
responseCreateWorkforce =
  res
    "CreateWorkforceResponse"
    "fixture/CreateWorkforceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkforce)

responseCreateWorkteam :: CreateWorkteamResponse -> TestTree
responseCreateWorkteam =
  res
    "CreateWorkteamResponse"
    "fixture/CreateWorkteamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkteam)

responseDeleteAction :: DeleteActionResponse -> TestTree
responseDeleteAction =
  res
    "DeleteActionResponse"
    "fixture/DeleteActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAction)

responseDeleteAlgorithm :: DeleteAlgorithmResponse -> TestTree
responseDeleteAlgorithm =
  res
    "DeleteAlgorithmResponse"
    "fixture/DeleteAlgorithmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlgorithm)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApp)

responseDeleteAppImageConfig :: DeleteAppImageConfigResponse -> TestTree
responseDeleteAppImageConfig =
  res
    "DeleteAppImageConfigResponse"
    "fixture/DeleteAppImageConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppImageConfig)

responseDeleteArtifact :: DeleteArtifactResponse -> TestTree
responseDeleteArtifact =
  res
    "DeleteArtifactResponse"
    "fixture/DeleteArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteArtifact)

responseDeleteAssociation :: DeleteAssociationResponse -> TestTree
responseDeleteAssociation =
  res
    "DeleteAssociationResponse"
    "fixture/DeleteAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAssociation)

responseDeleteCodeRepository :: DeleteCodeRepositoryResponse -> TestTree
responseDeleteCodeRepository =
  res
    "DeleteCodeRepositoryResponse"
    "fixture/DeleteCodeRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCodeRepository)

responseDeleteContext :: DeleteContextResponse -> TestTree
responseDeleteContext =
  res
    "DeleteContextResponse"
    "fixture/DeleteContextResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteContext)

responseDeleteDataQualityJobDefinition :: DeleteDataQualityJobDefinitionResponse -> TestTree
responseDeleteDataQualityJobDefinition =
  res
    "DeleteDataQualityJobDefinitionResponse"
    "fixture/DeleteDataQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataQualityJobDefinition)

responseDeleteDeviceFleet :: DeleteDeviceFleetResponse -> TestTree
responseDeleteDeviceFleet =
  res
    "DeleteDeviceFleetResponse"
    "fixture/DeleteDeviceFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDeviceFleet)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomain)

responseDeleteEdgeDeploymentPlan :: DeleteEdgeDeploymentPlanResponse -> TestTree
responseDeleteEdgeDeploymentPlan =
  res
    "DeleteEdgeDeploymentPlanResponse"
    "fixture/DeleteEdgeDeploymentPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEdgeDeploymentPlan)

responseDeleteEdgeDeploymentStage :: DeleteEdgeDeploymentStageResponse -> TestTree
responseDeleteEdgeDeploymentStage =
  res
    "DeleteEdgeDeploymentStageResponse"
    "fixture/DeleteEdgeDeploymentStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEdgeDeploymentStage)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEndpoint)

responseDeleteEndpointConfig :: DeleteEndpointConfigResponse -> TestTree
responseDeleteEndpointConfig =
  res
    "DeleteEndpointConfigResponse"
    "fixture/DeleteEndpointConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEndpointConfig)

responseDeleteExperiment :: DeleteExperimentResponse -> TestTree
responseDeleteExperiment =
  res
    "DeleteExperimentResponse"
    "fixture/DeleteExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteExperiment)

responseDeleteFeatureGroup :: DeleteFeatureGroupResponse -> TestTree
responseDeleteFeatureGroup =
  res
    "DeleteFeatureGroupResponse"
    "fixture/DeleteFeatureGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFeatureGroup)

responseDeleteFlowDefinition :: DeleteFlowDefinitionResponse -> TestTree
responseDeleteFlowDefinition =
  res
    "DeleteFlowDefinitionResponse"
    "fixture/DeleteFlowDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFlowDefinition)

responseDeleteHub :: DeleteHubResponse -> TestTree
responseDeleteHub =
  res
    "DeleteHubResponse"
    "fixture/DeleteHubResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHub)

responseDeleteHubContent :: DeleteHubContentResponse -> TestTree
responseDeleteHubContent =
  res
    "DeleteHubContentResponse"
    "fixture/DeleteHubContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHubContent)

responseDeleteHumanTaskUi :: DeleteHumanTaskUiResponse -> TestTree
responseDeleteHumanTaskUi =
  res
    "DeleteHumanTaskUiResponse"
    "fixture/DeleteHumanTaskUiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHumanTaskUi)

responseDeleteImage :: DeleteImageResponse -> TestTree
responseDeleteImage =
  res
    "DeleteImageResponse"
    "fixture/DeleteImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteImage)

responseDeleteImageVersion :: DeleteImageVersionResponse -> TestTree
responseDeleteImageVersion =
  res
    "DeleteImageVersionResponse"
    "fixture/DeleteImageVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteImageVersion)

responseDeleteInferenceExperiment :: DeleteInferenceExperimentResponse -> TestTree
responseDeleteInferenceExperiment =
  res
    "DeleteInferenceExperimentResponse"
    "fixture/DeleteInferenceExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInferenceExperiment)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel =
  res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModel)

responseDeleteModelBiasJobDefinition :: DeleteModelBiasJobDefinitionResponse -> TestTree
responseDeleteModelBiasJobDefinition =
  res
    "DeleteModelBiasJobDefinitionResponse"
    "fixture/DeleteModelBiasJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModelBiasJobDefinition)

responseDeleteModelCard :: DeleteModelCardResponse -> TestTree
responseDeleteModelCard =
  res
    "DeleteModelCardResponse"
    "fixture/DeleteModelCardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModelCard)

responseDeleteModelExplainabilityJobDefinition :: DeleteModelExplainabilityJobDefinitionResponse -> TestTree
responseDeleteModelExplainabilityJobDefinition =
  res
    "DeleteModelExplainabilityJobDefinitionResponse"
    "fixture/DeleteModelExplainabilityJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModelExplainabilityJobDefinition)

responseDeleteModelPackage :: DeleteModelPackageResponse -> TestTree
responseDeleteModelPackage =
  res
    "DeleteModelPackageResponse"
    "fixture/DeleteModelPackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModelPackage)

responseDeleteModelPackageGroup :: DeleteModelPackageGroupResponse -> TestTree
responseDeleteModelPackageGroup =
  res
    "DeleteModelPackageGroupResponse"
    "fixture/DeleteModelPackageGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModelPackageGroup)

responseDeleteModelPackageGroupPolicy :: DeleteModelPackageGroupPolicyResponse -> TestTree
responseDeleteModelPackageGroupPolicy =
  res
    "DeleteModelPackageGroupPolicyResponse"
    "fixture/DeleteModelPackageGroupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModelPackageGroupPolicy)

responseDeleteModelQualityJobDefinition :: DeleteModelQualityJobDefinitionResponse -> TestTree
responseDeleteModelQualityJobDefinition =
  res
    "DeleteModelQualityJobDefinitionResponse"
    "fixture/DeleteModelQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteModelQualityJobDefinition)

responseDeleteMonitoringSchedule :: DeleteMonitoringScheduleResponse -> TestTree
responseDeleteMonitoringSchedule =
  res
    "DeleteMonitoringScheduleResponse"
    "fixture/DeleteMonitoringScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMonitoringSchedule)

responseDeleteNotebookInstance :: DeleteNotebookInstanceResponse -> TestTree
responseDeleteNotebookInstance =
  res
    "DeleteNotebookInstanceResponse"
    "fixture/DeleteNotebookInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNotebookInstance)

responseDeleteNotebookInstanceLifecycleConfig :: DeleteNotebookInstanceLifecycleConfigResponse -> TestTree
responseDeleteNotebookInstanceLifecycleConfig =
  res
    "DeleteNotebookInstanceLifecycleConfigResponse"
    "fixture/DeleteNotebookInstanceLifecycleConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNotebookInstanceLifecycleConfig)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline =
  res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePipeline)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProject)

responseDeleteSpace :: DeleteSpaceResponse -> TestTree
responseDeleteSpace =
  res
    "DeleteSpaceResponse"
    "fixture/DeleteSpaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSpace)

responseDeleteStudioLifecycleConfig :: DeleteStudioLifecycleConfigResponse -> TestTree
responseDeleteStudioLifecycleConfig =
  res
    "DeleteStudioLifecycleConfigResponse"
    "fixture/DeleteStudioLifecycleConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStudioLifecycleConfig)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTags)

responseDeleteTrial :: DeleteTrialResponse -> TestTree
responseDeleteTrial =
  res
    "DeleteTrialResponse"
    "fixture/DeleteTrialResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrial)

responseDeleteTrialComponent :: DeleteTrialComponentResponse -> TestTree
responseDeleteTrialComponent =
  res
    "DeleteTrialComponentResponse"
    "fixture/DeleteTrialComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrialComponent)

responseDeleteUserProfile :: DeleteUserProfileResponse -> TestTree
responseDeleteUserProfile =
  res
    "DeleteUserProfileResponse"
    "fixture/DeleteUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserProfile)

responseDeleteWorkforce :: DeleteWorkforceResponse -> TestTree
responseDeleteWorkforce =
  res
    "DeleteWorkforceResponse"
    "fixture/DeleteWorkforceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkforce)

responseDeleteWorkteam :: DeleteWorkteamResponse -> TestTree
responseDeleteWorkteam =
  res
    "DeleteWorkteamResponse"
    "fixture/DeleteWorkteamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkteam)

responseDeregisterDevices :: DeregisterDevicesResponse -> TestTree
responseDeregisterDevices =
  res
    "DeregisterDevicesResponse"
    "fixture/DeregisterDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterDevices)

responseDescribeAction :: DescribeActionResponse -> TestTree
responseDescribeAction =
  res
    "DescribeActionResponse"
    "fixture/DescribeActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAction)

responseDescribeAlgorithm :: DescribeAlgorithmResponse -> TestTree
responseDescribeAlgorithm =
  res
    "DescribeAlgorithmResponse"
    "fixture/DescribeAlgorithmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAlgorithm)

responseDescribeApp :: DescribeAppResponse -> TestTree
responseDescribeApp =
  res
    "DescribeAppResponse"
    "fixture/DescribeAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeApp)

responseDescribeAppImageConfig :: DescribeAppImageConfigResponse -> TestTree
responseDescribeAppImageConfig =
  res
    "DescribeAppImageConfigResponse"
    "fixture/DescribeAppImageConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAppImageConfig)

responseDescribeArtifact :: DescribeArtifactResponse -> TestTree
responseDescribeArtifact =
  res
    "DescribeArtifactResponse"
    "fixture/DescribeArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeArtifact)

responseDescribeAutoMLJob :: DescribeAutoMLJobResponse -> TestTree
responseDescribeAutoMLJob =
  res
    "DescribeAutoMLJobResponse"
    "fixture/DescribeAutoMLJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAutoMLJob)

responseDescribeCodeRepository :: DescribeCodeRepositoryResponse -> TestTree
responseDescribeCodeRepository =
  res
    "DescribeCodeRepositoryResponse"
    "fixture/DescribeCodeRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCodeRepository)

responseDescribeCompilationJob :: DescribeCompilationJobResponse -> TestTree
responseDescribeCompilationJob =
  res
    "DescribeCompilationJobResponse"
    "fixture/DescribeCompilationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCompilationJob)

responseDescribeContext :: DescribeContextResponse -> TestTree
responseDescribeContext =
  res
    "DescribeContextResponse"
    "fixture/DescribeContextResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeContext)

responseDescribeDataQualityJobDefinition :: DescribeDataQualityJobDefinitionResponse -> TestTree
responseDescribeDataQualityJobDefinition =
  res
    "DescribeDataQualityJobDefinitionResponse"
    "fixture/DescribeDataQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataQualityJobDefinition)

responseDescribeDevice :: DescribeDeviceResponse -> TestTree
responseDescribeDevice =
  res
    "DescribeDeviceResponse"
    "fixture/DescribeDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDevice)

responseDescribeDeviceFleet :: DescribeDeviceFleetResponse -> TestTree
responseDescribeDeviceFleet =
  res
    "DescribeDeviceFleetResponse"
    "fixture/DescribeDeviceFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDeviceFleet)

responseDescribeDomain :: DescribeDomainResponse -> TestTree
responseDescribeDomain =
  res
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomain)

responseDescribeEdgeDeploymentPlan :: DescribeEdgeDeploymentPlanResponse -> TestTree
responseDescribeEdgeDeploymentPlan =
  res
    "DescribeEdgeDeploymentPlanResponse"
    "fixture/DescribeEdgeDeploymentPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEdgeDeploymentPlan)

responseDescribeEdgePackagingJob :: DescribeEdgePackagingJobResponse -> TestTree
responseDescribeEdgePackagingJob =
  res
    "DescribeEdgePackagingJobResponse"
    "fixture/DescribeEdgePackagingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEdgePackagingJob)

responseDescribeEndpoint :: DescribeEndpointResponse -> TestTree
responseDescribeEndpoint =
  res
    "DescribeEndpointResponse"
    "fixture/DescribeEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpoint)

responseDescribeEndpointConfig :: DescribeEndpointConfigResponse -> TestTree
responseDescribeEndpointConfig =
  res
    "DescribeEndpointConfigResponse"
    "fixture/DescribeEndpointConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEndpointConfig)

responseDescribeExperiment :: DescribeExperimentResponse -> TestTree
responseDescribeExperiment =
  res
    "DescribeExperimentResponse"
    "fixture/DescribeExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExperiment)

responseDescribeFeatureGroup :: DescribeFeatureGroupResponse -> TestTree
responseDescribeFeatureGroup =
  res
    "DescribeFeatureGroupResponse"
    "fixture/DescribeFeatureGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFeatureGroup)

responseDescribeFeatureMetadata :: DescribeFeatureMetadataResponse -> TestTree
responseDescribeFeatureMetadata =
  res
    "DescribeFeatureMetadataResponse"
    "fixture/DescribeFeatureMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFeatureMetadata)

responseDescribeFlowDefinition :: DescribeFlowDefinitionResponse -> TestTree
responseDescribeFlowDefinition =
  res
    "DescribeFlowDefinitionResponse"
    "fixture/DescribeFlowDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFlowDefinition)

responseDescribeHub :: DescribeHubResponse -> TestTree
responseDescribeHub =
  res
    "DescribeHubResponse"
    "fixture/DescribeHubResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHub)

responseDescribeHubContent :: DescribeHubContentResponse -> TestTree
responseDescribeHubContent =
  res
    "DescribeHubContentResponse"
    "fixture/DescribeHubContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHubContent)

responseDescribeHumanTaskUi :: DescribeHumanTaskUiResponse -> TestTree
responseDescribeHumanTaskUi =
  res
    "DescribeHumanTaskUiResponse"
    "fixture/DescribeHumanTaskUiResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHumanTaskUi)

responseDescribeHyperParameterTuningJob :: DescribeHyperParameterTuningJobResponse -> TestTree
responseDescribeHyperParameterTuningJob =
  res
    "DescribeHyperParameterTuningJobResponse"
    "fixture/DescribeHyperParameterTuningJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHyperParameterTuningJob)

responseDescribeImage :: DescribeImageResponse -> TestTree
responseDescribeImage =
  res
    "DescribeImageResponse"
    "fixture/DescribeImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImage)

responseDescribeImageVersion :: DescribeImageVersionResponse -> TestTree
responseDescribeImageVersion =
  res
    "DescribeImageVersionResponse"
    "fixture/DescribeImageVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImageVersion)

responseDescribeInferenceExperiment :: DescribeInferenceExperimentResponse -> TestTree
responseDescribeInferenceExperiment =
  res
    "DescribeInferenceExperimentResponse"
    "fixture/DescribeInferenceExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInferenceExperiment)

responseDescribeInferenceRecommendationsJob :: DescribeInferenceRecommendationsJobResponse -> TestTree
responseDescribeInferenceRecommendationsJob =
  res
    "DescribeInferenceRecommendationsJobResponse"
    "fixture/DescribeInferenceRecommendationsJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInferenceRecommendationsJob)

responseDescribeLabelingJob :: DescribeLabelingJobResponse -> TestTree
responseDescribeLabelingJob =
  res
    "DescribeLabelingJobResponse"
    "fixture/DescribeLabelingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLabelingJob)

responseDescribeLineageGroup :: DescribeLineageGroupResponse -> TestTree
responseDescribeLineageGroup =
  res
    "DescribeLineageGroupResponse"
    "fixture/DescribeLineageGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeLineageGroup)

responseDescribeModel :: DescribeModelResponse -> TestTree
responseDescribeModel =
  res
    "DescribeModelResponse"
    "fixture/DescribeModelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeModel)

responseDescribeModelBiasJobDefinition :: DescribeModelBiasJobDefinitionResponse -> TestTree
responseDescribeModelBiasJobDefinition =
  res
    "DescribeModelBiasJobDefinitionResponse"
    "fixture/DescribeModelBiasJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeModelBiasJobDefinition)

responseDescribeModelCard :: DescribeModelCardResponse -> TestTree
responseDescribeModelCard =
  res
    "DescribeModelCardResponse"
    "fixture/DescribeModelCardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeModelCard)

responseDescribeModelCardExportJob :: DescribeModelCardExportJobResponse -> TestTree
responseDescribeModelCardExportJob =
  res
    "DescribeModelCardExportJobResponse"
    "fixture/DescribeModelCardExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeModelCardExportJob)

responseDescribeModelExplainabilityJobDefinition :: DescribeModelExplainabilityJobDefinitionResponse -> TestTree
responseDescribeModelExplainabilityJobDefinition =
  res
    "DescribeModelExplainabilityJobDefinitionResponse"
    "fixture/DescribeModelExplainabilityJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeModelExplainabilityJobDefinition)

responseDescribeModelPackage :: DescribeModelPackageResponse -> TestTree
responseDescribeModelPackage =
  res
    "DescribeModelPackageResponse"
    "fixture/DescribeModelPackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeModelPackage)

responseDescribeModelPackageGroup :: DescribeModelPackageGroupResponse -> TestTree
responseDescribeModelPackageGroup =
  res
    "DescribeModelPackageGroupResponse"
    "fixture/DescribeModelPackageGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeModelPackageGroup)

responseDescribeModelQualityJobDefinition :: DescribeModelQualityJobDefinitionResponse -> TestTree
responseDescribeModelQualityJobDefinition =
  res
    "DescribeModelQualityJobDefinitionResponse"
    "fixture/DescribeModelQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeModelQualityJobDefinition)

responseDescribeMonitoringSchedule :: DescribeMonitoringScheduleResponse -> TestTree
responseDescribeMonitoringSchedule =
  res
    "DescribeMonitoringScheduleResponse"
    "fixture/DescribeMonitoringScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMonitoringSchedule)

responseDescribeNotebookInstance :: DescribeNotebookInstanceResponse -> TestTree
responseDescribeNotebookInstance =
  res
    "DescribeNotebookInstanceResponse"
    "fixture/DescribeNotebookInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNotebookInstance)

responseDescribeNotebookInstanceLifecycleConfig :: DescribeNotebookInstanceLifecycleConfigResponse -> TestTree
responseDescribeNotebookInstanceLifecycleConfig =
  res
    "DescribeNotebookInstanceLifecycleConfigResponse"
    "fixture/DescribeNotebookInstanceLifecycleConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNotebookInstanceLifecycleConfig)

responseDescribePipeline :: DescribePipelineResponse -> TestTree
responseDescribePipeline =
  res
    "DescribePipelineResponse"
    "fixture/DescribePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePipeline)

responseDescribePipelineDefinitionForExecution :: DescribePipelineDefinitionForExecutionResponse -> TestTree
responseDescribePipelineDefinitionForExecution =
  res
    "DescribePipelineDefinitionForExecutionResponse"
    "fixture/DescribePipelineDefinitionForExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePipelineDefinitionForExecution)

responseDescribePipelineExecution :: DescribePipelineExecutionResponse -> TestTree
responseDescribePipelineExecution =
  res
    "DescribePipelineExecutionResponse"
    "fixture/DescribePipelineExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePipelineExecution)

responseDescribeProcessingJob :: DescribeProcessingJobResponse -> TestTree
responseDescribeProcessingJob =
  res
    "DescribeProcessingJobResponse"
    "fixture/DescribeProcessingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProcessingJob)

responseDescribeProject :: DescribeProjectResponse -> TestTree
responseDescribeProject =
  res
    "DescribeProjectResponse"
    "fixture/DescribeProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProject)

responseDescribeSpace :: DescribeSpaceResponse -> TestTree
responseDescribeSpace =
  res
    "DescribeSpaceResponse"
    "fixture/DescribeSpaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSpace)

responseDescribeStudioLifecycleConfig :: DescribeStudioLifecycleConfigResponse -> TestTree
responseDescribeStudioLifecycleConfig =
  res
    "DescribeStudioLifecycleConfigResponse"
    "fixture/DescribeStudioLifecycleConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStudioLifecycleConfig)

responseDescribeSubscribedWorkteam :: DescribeSubscribedWorkteamResponse -> TestTree
responseDescribeSubscribedWorkteam =
  res
    "DescribeSubscribedWorkteamResponse"
    "fixture/DescribeSubscribedWorkteamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSubscribedWorkteam)

responseDescribeTrainingJob :: DescribeTrainingJobResponse -> TestTree
responseDescribeTrainingJob =
  res
    "DescribeTrainingJobResponse"
    "fixture/DescribeTrainingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrainingJob)

responseDescribeTransformJob :: DescribeTransformJobResponse -> TestTree
responseDescribeTransformJob =
  res
    "DescribeTransformJobResponse"
    "fixture/DescribeTransformJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTransformJob)

responseDescribeTrial :: DescribeTrialResponse -> TestTree
responseDescribeTrial =
  res
    "DescribeTrialResponse"
    "fixture/DescribeTrialResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrial)

responseDescribeTrialComponent :: DescribeTrialComponentResponse -> TestTree
responseDescribeTrialComponent =
  res
    "DescribeTrialComponentResponse"
    "fixture/DescribeTrialComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeTrialComponent)

responseDescribeUserProfile :: DescribeUserProfileResponse -> TestTree
responseDescribeUserProfile =
  res
    "DescribeUserProfileResponse"
    "fixture/DescribeUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUserProfile)

responseDescribeWorkforce :: DescribeWorkforceResponse -> TestTree
responseDescribeWorkforce =
  res
    "DescribeWorkforceResponse"
    "fixture/DescribeWorkforceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkforce)

responseDescribeWorkteam :: DescribeWorkteamResponse -> TestTree
responseDescribeWorkteam =
  res
    "DescribeWorkteamResponse"
    "fixture/DescribeWorkteamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkteam)

responseDisableSagemakerServicecatalogPortfolio :: DisableSagemakerServicecatalogPortfolioResponse -> TestTree
responseDisableSagemakerServicecatalogPortfolio =
  res
    "DisableSagemakerServicecatalogPortfolioResponse"
    "fixture/DisableSagemakerServicecatalogPortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableSagemakerServicecatalogPortfolio)

responseDisassociateTrialComponent :: DisassociateTrialComponentResponse -> TestTree
responseDisassociateTrialComponent =
  res
    "DisassociateTrialComponentResponse"
    "fixture/DisassociateTrialComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateTrialComponent)

responseEnableSagemakerServicecatalogPortfolio :: EnableSagemakerServicecatalogPortfolioResponse -> TestTree
responseEnableSagemakerServicecatalogPortfolio =
  res
    "EnableSagemakerServicecatalogPortfolioResponse"
    "fixture/EnableSagemakerServicecatalogPortfolioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableSagemakerServicecatalogPortfolio)

responseGetDeviceFleetReport :: GetDeviceFleetReportResponse -> TestTree
responseGetDeviceFleetReport =
  res
    "GetDeviceFleetReportResponse"
    "fixture/GetDeviceFleetReportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeviceFleetReport)

responseGetLineageGroupPolicy :: GetLineageGroupPolicyResponse -> TestTree
responseGetLineageGroupPolicy =
  res
    "GetLineageGroupPolicyResponse"
    "fixture/GetLineageGroupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLineageGroupPolicy)

responseGetModelPackageGroupPolicy :: GetModelPackageGroupPolicyResponse -> TestTree
responseGetModelPackageGroupPolicy =
  res
    "GetModelPackageGroupPolicyResponse"
    "fixture/GetModelPackageGroupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetModelPackageGroupPolicy)

responseGetSagemakerServicecatalogPortfolioStatus :: GetSagemakerServicecatalogPortfolioStatusResponse -> TestTree
responseGetSagemakerServicecatalogPortfolioStatus =
  res
    "GetSagemakerServicecatalogPortfolioStatusResponse"
    "fixture/GetSagemakerServicecatalogPortfolioStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSagemakerServicecatalogPortfolioStatus)

responseGetSearchSuggestions :: GetSearchSuggestionsResponse -> TestTree
responseGetSearchSuggestions =
  res
    "GetSearchSuggestionsResponse"
    "fixture/GetSearchSuggestionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSearchSuggestions)

responseImportHubContent :: ImportHubContentResponse -> TestTree
responseImportHubContent =
  res
    "ImportHubContentResponse"
    "fixture/ImportHubContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportHubContent)

responseListActions :: ListActionsResponse -> TestTree
responseListActions =
  res
    "ListActionsResponse"
    "fixture/ListActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListActions)

responseListAlgorithms :: ListAlgorithmsResponse -> TestTree
responseListAlgorithms =
  res
    "ListAlgorithmsResponse"
    "fixture/ListAlgorithmsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAlgorithms)

responseListAppImageConfigs :: ListAppImageConfigsResponse -> TestTree
responseListAppImageConfigs =
  res
    "ListAppImageConfigsResponse"
    "fixture/ListAppImageConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppImageConfigs)

responseListApps :: ListAppsResponse -> TestTree
responseListApps =
  res
    "ListAppsResponse"
    "fixture/ListAppsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApps)

responseListArtifacts :: ListArtifactsResponse -> TestTree
responseListArtifacts =
  res
    "ListArtifactsResponse"
    "fixture/ListArtifactsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListArtifacts)

responseListAssociations :: ListAssociationsResponse -> TestTree
responseListAssociations =
  res
    "ListAssociationsResponse"
    "fixture/ListAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssociations)

responseListAutoMLJobs :: ListAutoMLJobsResponse -> TestTree
responseListAutoMLJobs =
  res
    "ListAutoMLJobsResponse"
    "fixture/ListAutoMLJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAutoMLJobs)

responseListCandidatesForAutoMLJob :: ListCandidatesForAutoMLJobResponse -> TestTree
responseListCandidatesForAutoMLJob =
  res
    "ListCandidatesForAutoMLJobResponse"
    "fixture/ListCandidatesForAutoMLJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCandidatesForAutoMLJob)

responseListCodeRepositories :: ListCodeRepositoriesResponse -> TestTree
responseListCodeRepositories =
  res
    "ListCodeRepositoriesResponse"
    "fixture/ListCodeRepositoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCodeRepositories)

responseListCompilationJobs :: ListCompilationJobsResponse -> TestTree
responseListCompilationJobs =
  res
    "ListCompilationJobsResponse"
    "fixture/ListCompilationJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCompilationJobs)

responseListContexts :: ListContextsResponse -> TestTree
responseListContexts =
  res
    "ListContextsResponse"
    "fixture/ListContextsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListContexts)

responseListDataQualityJobDefinitions :: ListDataQualityJobDefinitionsResponse -> TestTree
responseListDataQualityJobDefinitions =
  res
    "ListDataQualityJobDefinitionsResponse"
    "fixture/ListDataQualityJobDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDataQualityJobDefinitions)

responseListDeviceFleets :: ListDeviceFleetsResponse -> TestTree
responseListDeviceFleets =
  res
    "ListDeviceFleetsResponse"
    "fixture/ListDeviceFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeviceFleets)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevices)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomains)

responseListEdgeDeploymentPlans :: ListEdgeDeploymentPlansResponse -> TestTree
responseListEdgeDeploymentPlans =
  res
    "ListEdgeDeploymentPlansResponse"
    "fixture/ListEdgeDeploymentPlansResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEdgeDeploymentPlans)

responseListEdgePackagingJobs :: ListEdgePackagingJobsResponse -> TestTree
responseListEdgePackagingJobs =
  res
    "ListEdgePackagingJobsResponse"
    "fixture/ListEdgePackagingJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEdgePackagingJobs)

responseListEndpointConfigs :: ListEndpointConfigsResponse -> TestTree
responseListEndpointConfigs =
  res
    "ListEndpointConfigsResponse"
    "fixture/ListEndpointConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEndpointConfigs)

responseListEndpoints :: ListEndpointsResponse -> TestTree
responseListEndpoints =
  res
    "ListEndpointsResponse"
    "fixture/ListEndpointsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEndpoints)

responseListExperiments :: ListExperimentsResponse -> TestTree
responseListExperiments =
  res
    "ListExperimentsResponse"
    "fixture/ListExperimentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExperiments)

responseListFeatureGroups :: ListFeatureGroupsResponse -> TestTree
responseListFeatureGroups =
  res
    "ListFeatureGroupsResponse"
    "fixture/ListFeatureGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFeatureGroups)

responseListFlowDefinitions :: ListFlowDefinitionsResponse -> TestTree
responseListFlowDefinitions =
  res
    "ListFlowDefinitionsResponse"
    "fixture/ListFlowDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFlowDefinitions)

responseListHubContentVersions :: ListHubContentVersionsResponse -> TestTree
responseListHubContentVersions =
  res
    "ListHubContentVersionsResponse"
    "fixture/ListHubContentVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHubContentVersions)

responseListHubContents :: ListHubContentsResponse -> TestTree
responseListHubContents =
  res
    "ListHubContentsResponse"
    "fixture/ListHubContentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHubContents)

responseListHubs :: ListHubsResponse -> TestTree
responseListHubs =
  res
    "ListHubsResponse"
    "fixture/ListHubsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHubs)

responseListHumanTaskUis :: ListHumanTaskUisResponse -> TestTree
responseListHumanTaskUis =
  res
    "ListHumanTaskUisResponse"
    "fixture/ListHumanTaskUisResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHumanTaskUis)

responseListHyperParameterTuningJobs :: ListHyperParameterTuningJobsResponse -> TestTree
responseListHyperParameterTuningJobs =
  res
    "ListHyperParameterTuningJobsResponse"
    "fixture/ListHyperParameterTuningJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHyperParameterTuningJobs)

responseListImageVersions :: ListImageVersionsResponse -> TestTree
responseListImageVersions =
  res
    "ListImageVersionsResponse"
    "fixture/ListImageVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImageVersions)

responseListImages :: ListImagesResponse -> TestTree
responseListImages =
  res
    "ListImagesResponse"
    "fixture/ListImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImages)

responseListInferenceExperiments :: ListInferenceExperimentsResponse -> TestTree
responseListInferenceExperiments =
  res
    "ListInferenceExperimentsResponse"
    "fixture/ListInferenceExperimentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInferenceExperiments)

responseListInferenceRecommendationsJobSteps :: ListInferenceRecommendationsJobStepsResponse -> TestTree
responseListInferenceRecommendationsJobSteps =
  res
    "ListInferenceRecommendationsJobStepsResponse"
    "fixture/ListInferenceRecommendationsJobStepsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInferenceRecommendationsJobSteps)

responseListInferenceRecommendationsJobs :: ListInferenceRecommendationsJobsResponse -> TestTree
responseListInferenceRecommendationsJobs =
  res
    "ListInferenceRecommendationsJobsResponse"
    "fixture/ListInferenceRecommendationsJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInferenceRecommendationsJobs)

responseListLabelingJobs :: ListLabelingJobsResponse -> TestTree
responseListLabelingJobs =
  res
    "ListLabelingJobsResponse"
    "fixture/ListLabelingJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLabelingJobs)

responseListLabelingJobsForWorkteam :: ListLabelingJobsForWorkteamResponse -> TestTree
responseListLabelingJobsForWorkteam =
  res
    "ListLabelingJobsForWorkteamResponse"
    "fixture/ListLabelingJobsForWorkteamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLabelingJobsForWorkteam)

responseListLineageGroups :: ListLineageGroupsResponse -> TestTree
responseListLineageGroups =
  res
    "ListLineageGroupsResponse"
    "fixture/ListLineageGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLineageGroups)

responseListModelBiasJobDefinitions :: ListModelBiasJobDefinitionsResponse -> TestTree
responseListModelBiasJobDefinitions =
  res
    "ListModelBiasJobDefinitionsResponse"
    "fixture/ListModelBiasJobDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModelBiasJobDefinitions)

responseListModelCardExportJobs :: ListModelCardExportJobsResponse -> TestTree
responseListModelCardExportJobs =
  res
    "ListModelCardExportJobsResponse"
    "fixture/ListModelCardExportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModelCardExportJobs)

responseListModelCardVersions :: ListModelCardVersionsResponse -> TestTree
responseListModelCardVersions =
  res
    "ListModelCardVersionsResponse"
    "fixture/ListModelCardVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModelCardVersions)

responseListModelCards :: ListModelCardsResponse -> TestTree
responseListModelCards =
  res
    "ListModelCardsResponse"
    "fixture/ListModelCardsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModelCards)

responseListModelExplainabilityJobDefinitions :: ListModelExplainabilityJobDefinitionsResponse -> TestTree
responseListModelExplainabilityJobDefinitions =
  res
    "ListModelExplainabilityJobDefinitionsResponse"
    "fixture/ListModelExplainabilityJobDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModelExplainabilityJobDefinitions)

responseListModelMetadata :: ListModelMetadataResponse -> TestTree
responseListModelMetadata =
  res
    "ListModelMetadataResponse"
    "fixture/ListModelMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModelMetadata)

responseListModelPackageGroups :: ListModelPackageGroupsResponse -> TestTree
responseListModelPackageGroups =
  res
    "ListModelPackageGroupsResponse"
    "fixture/ListModelPackageGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModelPackageGroups)

responseListModelPackages :: ListModelPackagesResponse -> TestTree
responseListModelPackages =
  res
    "ListModelPackagesResponse"
    "fixture/ListModelPackagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModelPackages)

responseListModelQualityJobDefinitions :: ListModelQualityJobDefinitionsResponse -> TestTree
responseListModelQualityJobDefinitions =
  res
    "ListModelQualityJobDefinitionsResponse"
    "fixture/ListModelQualityJobDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModelQualityJobDefinitions)

responseListModels :: ListModelsResponse -> TestTree
responseListModels =
  res
    "ListModelsResponse"
    "fixture/ListModelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListModels)

responseListMonitoringAlertHistory :: ListMonitoringAlertHistoryResponse -> TestTree
responseListMonitoringAlertHistory =
  res
    "ListMonitoringAlertHistoryResponse"
    "fixture/ListMonitoringAlertHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMonitoringAlertHistory)

responseListMonitoringAlerts :: ListMonitoringAlertsResponse -> TestTree
responseListMonitoringAlerts =
  res
    "ListMonitoringAlertsResponse"
    "fixture/ListMonitoringAlertsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMonitoringAlerts)

responseListMonitoringExecutions :: ListMonitoringExecutionsResponse -> TestTree
responseListMonitoringExecutions =
  res
    "ListMonitoringExecutionsResponse"
    "fixture/ListMonitoringExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMonitoringExecutions)

responseListMonitoringSchedules :: ListMonitoringSchedulesResponse -> TestTree
responseListMonitoringSchedules =
  res
    "ListMonitoringSchedulesResponse"
    "fixture/ListMonitoringSchedulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMonitoringSchedules)

responseListNotebookInstanceLifecycleConfigs :: ListNotebookInstanceLifecycleConfigsResponse -> TestTree
responseListNotebookInstanceLifecycleConfigs =
  res
    "ListNotebookInstanceLifecycleConfigsResponse"
    "fixture/ListNotebookInstanceLifecycleConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNotebookInstanceLifecycleConfigs)

responseListNotebookInstances :: ListNotebookInstancesResponse -> TestTree
responseListNotebookInstances =
  res
    "ListNotebookInstancesResponse"
    "fixture/ListNotebookInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNotebookInstances)

responseListPipelineExecutionSteps :: ListPipelineExecutionStepsResponse -> TestTree
responseListPipelineExecutionSteps =
  res
    "ListPipelineExecutionStepsResponse"
    "fixture/ListPipelineExecutionStepsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPipelineExecutionSteps)

responseListPipelineExecutions :: ListPipelineExecutionsResponse -> TestTree
responseListPipelineExecutions =
  res
    "ListPipelineExecutionsResponse"
    "fixture/ListPipelineExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPipelineExecutions)

responseListPipelineParametersForExecution :: ListPipelineParametersForExecutionResponse -> TestTree
responseListPipelineParametersForExecution =
  res
    "ListPipelineParametersForExecutionResponse"
    "fixture/ListPipelineParametersForExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPipelineParametersForExecution)

responseListPipelines :: ListPipelinesResponse -> TestTree
responseListPipelines =
  res
    "ListPipelinesResponse"
    "fixture/ListPipelinesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPipelines)

responseListProcessingJobs :: ListProcessingJobsResponse -> TestTree
responseListProcessingJobs =
  res
    "ListProcessingJobsResponse"
    "fixture/ListProcessingJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProcessingJobs)

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProjects)

responseListSpaces :: ListSpacesResponse -> TestTree
responseListSpaces =
  res
    "ListSpacesResponse"
    "fixture/ListSpacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSpaces)

responseListStageDevices :: ListStageDevicesResponse -> TestTree
responseListStageDevices =
  res
    "ListStageDevicesResponse"
    "fixture/ListStageDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStageDevices)

responseListStudioLifecycleConfigs :: ListStudioLifecycleConfigsResponse -> TestTree
responseListStudioLifecycleConfigs =
  res
    "ListStudioLifecycleConfigsResponse"
    "fixture/ListStudioLifecycleConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStudioLifecycleConfigs)

responseListSubscribedWorkteams :: ListSubscribedWorkteamsResponse -> TestTree
responseListSubscribedWorkteams =
  res
    "ListSubscribedWorkteamsResponse"
    "fixture/ListSubscribedWorkteamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSubscribedWorkteams)

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTags)

responseListTrainingJobs :: ListTrainingJobsResponse -> TestTree
responseListTrainingJobs =
  res
    "ListTrainingJobsResponse"
    "fixture/ListTrainingJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrainingJobs)

responseListTrainingJobsForHyperParameterTuningJob :: ListTrainingJobsForHyperParameterTuningJobResponse -> TestTree
responseListTrainingJobsForHyperParameterTuningJob =
  res
    "ListTrainingJobsForHyperParameterTuningJobResponse"
    "fixture/ListTrainingJobsForHyperParameterTuningJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrainingJobsForHyperParameterTuningJob)

responseListTransformJobs :: ListTransformJobsResponse -> TestTree
responseListTransformJobs =
  res
    "ListTransformJobsResponse"
    "fixture/ListTransformJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTransformJobs)

responseListTrialComponents :: ListTrialComponentsResponse -> TestTree
responseListTrialComponents =
  res
    "ListTrialComponentsResponse"
    "fixture/ListTrialComponentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrialComponents)

responseListTrials :: ListTrialsResponse -> TestTree
responseListTrials =
  res
    "ListTrialsResponse"
    "fixture/ListTrialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrials)

responseListUserProfiles :: ListUserProfilesResponse -> TestTree
responseListUserProfiles =
  res
    "ListUserProfilesResponse"
    "fixture/ListUserProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUserProfiles)

responseListWorkforces :: ListWorkforcesResponse -> TestTree
responseListWorkforces =
  res
    "ListWorkforcesResponse"
    "fixture/ListWorkforcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkforces)

responseListWorkteams :: ListWorkteamsResponse -> TestTree
responseListWorkteams =
  res
    "ListWorkteamsResponse"
    "fixture/ListWorkteamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkteams)

responsePutModelPackageGroupPolicy :: PutModelPackageGroupPolicyResponse -> TestTree
responsePutModelPackageGroupPolicy =
  res
    "PutModelPackageGroupPolicyResponse"
    "fixture/PutModelPackageGroupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutModelPackageGroupPolicy)

responseQueryLineage :: QueryLineageResponse -> TestTree
responseQueryLineage =
  res
    "QueryLineageResponse"
    "fixture/QueryLineageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy QueryLineage)

responseRegisterDevices :: RegisterDevicesResponse -> TestTree
responseRegisterDevices =
  res
    "RegisterDevicesResponse"
    "fixture/RegisterDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterDevices)

responseRenderUiTemplate :: RenderUiTemplateResponse -> TestTree
responseRenderUiTemplate =
  res
    "RenderUiTemplateResponse"
    "fixture/RenderUiTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RenderUiTemplate)

responseRetryPipelineExecution :: RetryPipelineExecutionResponse -> TestTree
responseRetryPipelineExecution =
  res
    "RetryPipelineExecutionResponse"
    "fixture/RetryPipelineExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RetryPipelineExecution)

responseSearch :: SearchResponse -> TestTree
responseSearch =
  res
    "SearchResponse"
    "fixture/SearchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Search)

responseSendPipelineExecutionStepFailure :: SendPipelineExecutionStepFailureResponse -> TestTree
responseSendPipelineExecutionStepFailure =
  res
    "SendPipelineExecutionStepFailureResponse"
    "fixture/SendPipelineExecutionStepFailureResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendPipelineExecutionStepFailure)

responseSendPipelineExecutionStepSuccess :: SendPipelineExecutionStepSuccessResponse -> TestTree
responseSendPipelineExecutionStepSuccess =
  res
    "SendPipelineExecutionStepSuccessResponse"
    "fixture/SendPipelineExecutionStepSuccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendPipelineExecutionStepSuccess)

responseStartEdgeDeploymentStage :: StartEdgeDeploymentStageResponse -> TestTree
responseStartEdgeDeploymentStage =
  res
    "StartEdgeDeploymentStageResponse"
    "fixture/StartEdgeDeploymentStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartEdgeDeploymentStage)

responseStartInferenceExperiment :: StartInferenceExperimentResponse -> TestTree
responseStartInferenceExperiment =
  res
    "StartInferenceExperimentResponse"
    "fixture/StartInferenceExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartInferenceExperiment)

responseStartMonitoringSchedule :: StartMonitoringScheduleResponse -> TestTree
responseStartMonitoringSchedule =
  res
    "StartMonitoringScheduleResponse"
    "fixture/StartMonitoringScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMonitoringSchedule)

responseStartNotebookInstance :: StartNotebookInstanceResponse -> TestTree
responseStartNotebookInstance =
  res
    "StartNotebookInstanceResponse"
    "fixture/StartNotebookInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartNotebookInstance)

responseStartPipelineExecution :: StartPipelineExecutionResponse -> TestTree
responseStartPipelineExecution =
  res
    "StartPipelineExecutionResponse"
    "fixture/StartPipelineExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartPipelineExecution)

responseStopAutoMLJob :: StopAutoMLJobResponse -> TestTree
responseStopAutoMLJob =
  res
    "StopAutoMLJobResponse"
    "fixture/StopAutoMLJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopAutoMLJob)

responseStopCompilationJob :: StopCompilationJobResponse -> TestTree
responseStopCompilationJob =
  res
    "StopCompilationJobResponse"
    "fixture/StopCompilationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopCompilationJob)

responseStopEdgeDeploymentStage :: StopEdgeDeploymentStageResponse -> TestTree
responseStopEdgeDeploymentStage =
  res
    "StopEdgeDeploymentStageResponse"
    "fixture/StopEdgeDeploymentStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopEdgeDeploymentStage)

responseStopEdgePackagingJob :: StopEdgePackagingJobResponse -> TestTree
responseStopEdgePackagingJob =
  res
    "StopEdgePackagingJobResponse"
    "fixture/StopEdgePackagingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopEdgePackagingJob)

responseStopHyperParameterTuningJob :: StopHyperParameterTuningJobResponse -> TestTree
responseStopHyperParameterTuningJob =
  res
    "StopHyperParameterTuningJobResponse"
    "fixture/StopHyperParameterTuningJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopHyperParameterTuningJob)

responseStopInferenceExperiment :: StopInferenceExperimentResponse -> TestTree
responseStopInferenceExperiment =
  res
    "StopInferenceExperimentResponse"
    "fixture/StopInferenceExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopInferenceExperiment)

responseStopInferenceRecommendationsJob :: StopInferenceRecommendationsJobResponse -> TestTree
responseStopInferenceRecommendationsJob =
  res
    "StopInferenceRecommendationsJobResponse"
    "fixture/StopInferenceRecommendationsJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopInferenceRecommendationsJob)

responseStopLabelingJob :: StopLabelingJobResponse -> TestTree
responseStopLabelingJob =
  res
    "StopLabelingJobResponse"
    "fixture/StopLabelingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopLabelingJob)

responseStopMonitoringSchedule :: StopMonitoringScheduleResponse -> TestTree
responseStopMonitoringSchedule =
  res
    "StopMonitoringScheduleResponse"
    "fixture/StopMonitoringScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopMonitoringSchedule)

responseStopNotebookInstance :: StopNotebookInstanceResponse -> TestTree
responseStopNotebookInstance =
  res
    "StopNotebookInstanceResponse"
    "fixture/StopNotebookInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopNotebookInstance)

responseStopPipelineExecution :: StopPipelineExecutionResponse -> TestTree
responseStopPipelineExecution =
  res
    "StopPipelineExecutionResponse"
    "fixture/StopPipelineExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopPipelineExecution)

responseStopProcessingJob :: StopProcessingJobResponse -> TestTree
responseStopProcessingJob =
  res
    "StopProcessingJobResponse"
    "fixture/StopProcessingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopProcessingJob)

responseStopTrainingJob :: StopTrainingJobResponse -> TestTree
responseStopTrainingJob =
  res
    "StopTrainingJobResponse"
    "fixture/StopTrainingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopTrainingJob)

responseStopTransformJob :: StopTransformJobResponse -> TestTree
responseStopTransformJob =
  res
    "StopTransformJobResponse"
    "fixture/StopTransformJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopTransformJob)

responseUpdateAction :: UpdateActionResponse -> TestTree
responseUpdateAction =
  res
    "UpdateActionResponse"
    "fixture/UpdateActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAction)

responseUpdateAppImageConfig :: UpdateAppImageConfigResponse -> TestTree
responseUpdateAppImageConfig =
  res
    "UpdateAppImageConfigResponse"
    "fixture/UpdateAppImageConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAppImageConfig)

responseUpdateArtifact :: UpdateArtifactResponse -> TestTree
responseUpdateArtifact =
  res
    "UpdateArtifactResponse"
    "fixture/UpdateArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateArtifact)

responseUpdateCodeRepository :: UpdateCodeRepositoryResponse -> TestTree
responseUpdateCodeRepository =
  res
    "UpdateCodeRepositoryResponse"
    "fixture/UpdateCodeRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCodeRepository)

responseUpdateContext :: UpdateContextResponse -> TestTree
responseUpdateContext =
  res
    "UpdateContextResponse"
    "fixture/UpdateContextResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContext)

responseUpdateDeviceFleet :: UpdateDeviceFleetResponse -> TestTree
responseUpdateDeviceFleet =
  res
    "UpdateDeviceFleetResponse"
    "fixture/UpdateDeviceFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDeviceFleet)

responseUpdateDevices :: UpdateDevicesResponse -> TestTree
responseUpdateDevices =
  res
    "UpdateDevicesResponse"
    "fixture/UpdateDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDevices)

responseUpdateDomain :: UpdateDomainResponse -> TestTree
responseUpdateDomain =
  res
    "UpdateDomainResponse"
    "fixture/UpdateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomain)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint =
  res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEndpoint)

responseUpdateEndpointWeightsAndCapacities :: UpdateEndpointWeightsAndCapacitiesResponse -> TestTree
responseUpdateEndpointWeightsAndCapacities =
  res
    "UpdateEndpointWeightsAndCapacitiesResponse"
    "fixture/UpdateEndpointWeightsAndCapacitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEndpointWeightsAndCapacities)

responseUpdateExperiment :: UpdateExperimentResponse -> TestTree
responseUpdateExperiment =
  res
    "UpdateExperimentResponse"
    "fixture/UpdateExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateExperiment)

responseUpdateFeatureGroup :: UpdateFeatureGroupResponse -> TestTree
responseUpdateFeatureGroup =
  res
    "UpdateFeatureGroupResponse"
    "fixture/UpdateFeatureGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFeatureGroup)

responseUpdateFeatureMetadata :: UpdateFeatureMetadataResponse -> TestTree
responseUpdateFeatureMetadata =
  res
    "UpdateFeatureMetadataResponse"
    "fixture/UpdateFeatureMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFeatureMetadata)

responseUpdateHub :: UpdateHubResponse -> TestTree
responseUpdateHub =
  res
    "UpdateHubResponse"
    "fixture/UpdateHubResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateHub)

responseUpdateImage :: UpdateImageResponse -> TestTree
responseUpdateImage =
  res
    "UpdateImageResponse"
    "fixture/UpdateImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateImage)

responseUpdateInferenceExperiment :: UpdateInferenceExperimentResponse -> TestTree
responseUpdateInferenceExperiment =
  res
    "UpdateInferenceExperimentResponse"
    "fixture/UpdateInferenceExperimentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInferenceExperiment)

responseUpdateModelCard :: UpdateModelCardResponse -> TestTree
responseUpdateModelCard =
  res
    "UpdateModelCardResponse"
    "fixture/UpdateModelCardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateModelCard)

responseUpdateModelPackage :: UpdateModelPackageResponse -> TestTree
responseUpdateModelPackage =
  res
    "UpdateModelPackageResponse"
    "fixture/UpdateModelPackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateModelPackage)

responseUpdateMonitoringAlert :: UpdateMonitoringAlertResponse -> TestTree
responseUpdateMonitoringAlert =
  res
    "UpdateMonitoringAlertResponse"
    "fixture/UpdateMonitoringAlertResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMonitoringAlert)

responseUpdateMonitoringSchedule :: UpdateMonitoringScheduleResponse -> TestTree
responseUpdateMonitoringSchedule =
  res
    "UpdateMonitoringScheduleResponse"
    "fixture/UpdateMonitoringScheduleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMonitoringSchedule)

responseUpdateNotebookInstance :: UpdateNotebookInstanceResponse -> TestTree
responseUpdateNotebookInstance =
  res
    "UpdateNotebookInstanceResponse"
    "fixture/UpdateNotebookInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNotebookInstance)

responseUpdateNotebookInstanceLifecycleConfig :: UpdateNotebookInstanceLifecycleConfigResponse -> TestTree
responseUpdateNotebookInstanceLifecycleConfig =
  res
    "UpdateNotebookInstanceLifecycleConfigResponse"
    "fixture/UpdateNotebookInstanceLifecycleConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNotebookInstanceLifecycleConfig)

responseUpdatePipeline :: UpdatePipelineResponse -> TestTree
responseUpdatePipeline =
  res
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePipeline)

responseUpdatePipelineExecution :: UpdatePipelineExecutionResponse -> TestTree
responseUpdatePipelineExecution =
  res
    "UpdatePipelineExecutionResponse"
    "fixture/UpdatePipelineExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePipelineExecution)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProject)

responseUpdateSpace :: UpdateSpaceResponse -> TestTree
responseUpdateSpace =
  res
    "UpdateSpaceResponse"
    "fixture/UpdateSpaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSpace)

responseUpdateTrainingJob :: UpdateTrainingJobResponse -> TestTree
responseUpdateTrainingJob =
  res
    "UpdateTrainingJobResponse"
    "fixture/UpdateTrainingJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTrainingJob)

responseUpdateTrial :: UpdateTrialResponse -> TestTree
responseUpdateTrial =
  res
    "UpdateTrialResponse"
    "fixture/UpdateTrialResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTrial)

responseUpdateTrialComponent :: UpdateTrialComponentResponse -> TestTree
responseUpdateTrialComponent =
  res
    "UpdateTrialComponentResponse"
    "fixture/UpdateTrialComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTrialComponent)

responseUpdateUserProfile :: UpdateUserProfileResponse -> TestTree
responseUpdateUserProfile =
  res
    "UpdateUserProfileResponse"
    "fixture/UpdateUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserProfile)

responseUpdateWorkforce :: UpdateWorkforceResponse -> TestTree
responseUpdateWorkforce =
  res
    "UpdateWorkforceResponse"
    "fixture/UpdateWorkforceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkforce)

responseUpdateWorkteam :: UpdateWorkteamResponse -> TestTree
responseUpdateWorkteam =
  res
    "UpdateWorkteamResponse"
    "fixture/UpdateWorkteamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkteam)

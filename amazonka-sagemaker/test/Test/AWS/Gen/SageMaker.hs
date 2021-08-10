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
--         [ requestDisassociateTrialComponent $
--             newDisassociateTrialComponent
--
--         , requestDeleteArtifact $
--             newDeleteArtifact
--
--         , requestCreateTransformJob $
--             newCreateTransformJob
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
--         , requestDescribePipeline $
--             newDescribePipeline
--
--         , requestUpdateArtifact $
--             newUpdateArtifact
--
--         , requestDescribeUserProfile $
--             newDescribeUserProfile
--
--         , requestStopTrainingJob $
--             newStopTrainingJob
--
--         , requestCreateEndpoint $
--             newCreateEndpoint
--
--         , requestGetSearchSuggestions $
--             newGetSearchSuggestions
--
--         , requestDeleteAction $
--             newDeleteAction
--
--         , requestCreateEdgePackagingJob $
--             newCreateEdgePackagingJob
--
--         , requestDescribeEndpointConfig $
--             newDescribeEndpointConfig
--
--         , requestListModelPackages $
--             newListModelPackages
--
--         , requestGetModelPackageGroupPolicy $
--             newGetModelPackageGroupPolicy
--
--         , requestDescribeMonitoringSchedule $
--             newDescribeMonitoringSchedule
--
--         , requestCreateModelExplainabilityJobDefinition $
--             newCreateModelExplainabilityJobDefinition
--
--         , requestDescribeLabelingJob $
--             newDescribeLabelingJob
--
--         , requestCreateNotebookInstance $
--             newCreateNotebookInstance
--
--         , requestUpdateModelPackage $
--             newUpdateModelPackage
--
--         , requestCreateModelQualityJobDefinition $
--             newCreateModelQualityJobDefinition
--
--         , requestDeleteModelPackage $
--             newDeleteModelPackage
--
--         , requestListProjects $
--             newListProjects
--
--         , requestListSubscribedWorkteams $
--             newListSubscribedWorkteams
--
--         , requestDeleteNotebookInstance $
--             newDeleteNotebookInstance
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestDescribeProcessingJob $
--             newDescribeProcessingJob
--
--         , requestListDomains $
--             newListDomains
--
--         , requestDeleteModelExplainabilityJobDefinition $
--             newDeleteModelExplainabilityJobDefinition
--
--         , requestStopMonitoringSchedule $
--             newStopMonitoringSchedule
--
--         , requestListDevices $
--             newListDevices
--
--         , requestCreateModelPackage $
--             newCreateModelPackage
--
--         , requestUpdateNotebookInstance $
--             newUpdateNotebookInstance
--
--         , requestStopAutoMLJob $
--             newStopAutoMLJob
--
--         , requestDescribeAppImageConfig $
--             newDescribeAppImageConfig
--
--         , requestStartMonitoringSchedule $
--             newStartMonitoringSchedule
--
--         , requestStopCompilationJob $
--             newStopCompilationJob
--
--         , requestCreateTrial $
--             newCreateTrial
--
--         , requestGetSagemakerServicecatalogPortfolioStatus $
--             newGetSagemakerServicecatalogPortfolioStatus
--
--         , requestUpdateCodeRepository $
--             newUpdateCodeRepository
--
--         , requestSearch $
--             newSearch
--
--         , requestDeleteModel $
--             newDeleteModel
--
--         , requestDeleteDataQualityJobDefinition $
--             newDeleteDataQualityJobDefinition
--
--         , requestListImages $
--             newListImages
--
--         , requestListTrainingJobs $
--             newListTrainingJobs
--
--         , requestDescribeTransformJob $
--             newDescribeTransformJob
--
--         , requestCreatePipeline $
--             newCreatePipeline
--
--         , requestCreateModelPackageGroup $
--             newCreateModelPackageGroup
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
--         , requestListDataQualityJobDefinitions $
--             newListDataQualityJobDefinitions
--
--         , requestDescribeEdgePackagingJob $
--             newDescribeEdgePackagingJob
--
--         , requestListContexts $
--             newListContexts
--
--         , requestDescribeEndpoint $
--             newDescribeEndpoint
--
--         , requestDeleteCodeRepository $
--             newDeleteCodeRepository
--
--         , requestDeleteModelPackageGroupPolicy $
--             newDeleteModelPackageGroupPolicy
--
--         , requestListUserProfiles $
--             newListUserProfiles
--
--         , requestDescribeCompilationJob $
--             newDescribeCompilationJob
--
--         , requestUpdatePipeline $
--             newUpdatePipeline
--
--         , requestCreateCodeRepository $
--             newCreateCodeRepository
--
--         , requestDescribeArtifact $
--             newDescribeArtifact
--
--         , requestDescribeHumanTaskUi $
--             newDescribeHumanTaskUi
--
--         , requestListPipelineExecutionSteps $
--             newListPipelineExecutionSteps
--
--         , requestListCodeRepositories $
--             newListCodeRepositories
--
--         , requestUpdateUserProfile $
--             newUpdateUserProfile
--
--         , requestDescribeAction $
--             newDescribeAction
--
--         , requestStopTransformJob $
--             newStopTransformJob
--
--         , requestCreateTrainingJob $
--             newCreateTrainingJob
--
--         , requestDeleteUserProfile $
--             newDeleteUserProfile
--
--         , requestCreateContext $
--             newCreateContext
--
--         , requestStopEdgePackagingJob $
--             newStopEdgePackagingJob
--
--         , requestCreateImage $
--             newCreateImage
--
--         , requestDeregisterDevices $
--             newDeregisterDevices
--
--         , requestCreateDataQualityJobDefinition $
--             newCreateDataQualityJobDefinition
--
--         , requestDeletePipeline $
--             newDeletePipeline
--
--         , requestCreateAppImageConfig $
--             newCreateAppImageConfig
--
--         , requestAddTags $
--             newAddTags
--
--         , requestDisableSagemakerServicecatalogPortfolio $
--             newDisableSagemakerServicecatalogPortfolio
--
--         , requestDeleteAssociation $
--             newDeleteAssociation
--
--         , requestUpdateMonitoringSchedule $
--             newUpdateMonitoringSchedule
--
--         , requestListMonitoringSchedules $
--             newListMonitoringSchedules
--
--         , requestStopNotebookInstance $
--             newStopNotebookInstance
--
--         , requestDeleteMonitoringSchedule $
--             newDeleteMonitoringSchedule
--
--         , requestDeleteEndpointConfig $
--             newDeleteEndpointConfig
--
--         , requestStartPipelineExecution $
--             newStartPipelineExecution
--
--         , requestDescribeModelPackage $
--             newDescribeModelPackage
--
--         , requestDeleteTags $
--             newDeleteTags
--
--         , requestAddAssociation $
--             newAddAssociation
--
--         , requestCreateNotebookInstanceLifecycleConfig $
--             newCreateNotebookInstanceLifecycleConfig
--
--         , requestListApps $
--             newListApps
--
--         , requestCreateWorkforce $
--             newCreateWorkforce
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
--         , requestStopPipelineExecution $
--             newStopPipelineExecution
--
--         , requestListEndpointConfigs $
--             newListEndpointConfigs
--
--         , requestDeleteWorkteam $
--             newDeleteWorkteam
--
--         , requestDeleteWorkforce $
--             newDeleteWorkforce
--
--         , requestDeleteModelBiasJobDefinition $
--             newDeleteModelBiasJobDefinition
--
--         , requestUpdateWorkforce $
--             newUpdateWorkforce
--
--         , requestDescribeDevice $
--             newDescribeDevice
--
--         , requestDescribeDomain $
--             newDescribeDomain
--
--         , requestDeleteNotebookInstanceLifecycleConfig $
--             newDeleteNotebookInstanceLifecycleConfig
--
--         , requestDescribePipelineExecution $
--             newDescribePipelineExecution
--
--         , requestUpdateWorkteam $
--             newUpdateWorkteam
--
--         , requestCreateLabelingJob $
--             newCreateLabelingJob
--
--         , requestDescribeModelQualityJobDefinition $
--             newDescribeModelQualityJobDefinition
--
--         , requestCreateExperiment $
--             newCreateExperiment
--
--         , requestListWorkforces $
--             newListWorkforces
--
--         , requestListAppImageConfigs $
--             newListAppImageConfigs
--
--         , requestUpdateNotebookInstanceLifecycleConfig $
--             newUpdateNotebookInstanceLifecycleConfig
--
--         , requestDescribeSubscribedWorkteam $
--             newDescribeSubscribedWorkteam
--
--         , requestListNotebookInstanceLifecycleConfigs $
--             newListNotebookInstanceLifecycleConfigs
--
--         , requestListEdgePackagingJobs $
--             newListEdgePackagingJobs
--
--         , requestDescribeCodeRepository $
--             newDescribeCodeRepository
--
--         , requestListEndpoints $
--             newListEndpoints
--
--         , requestDescribeDataQualityJobDefinition $
--             newDescribeDataQualityJobDefinition
--
--         , requestDescribeAlgorithm $
--             newDescribeAlgorithm
--
--         , requestCreateAction $
--             newCreateAction
--
--         , requestDeleteEndpoint $
--             newDeleteEndpoint
--
--         , requestCreatePresignedDomainUrl $
--             newCreatePresignedDomainUrl
--
--         , requestListTransformJobs $
--             newListTransformJobs
--
--         , requestDescribeHyperParameterTuningJob $
--             newDescribeHyperParameterTuningJob
--
--         , requestCreateCompilationJob $
--             newCreateCompilationJob
--
--         , requestUpdateEndpoint $
--             newUpdateEndpoint
--
--         , requestDescribeModel $
--             newDescribeModel
--
--         , requestCreateDeviceFleet $
--             newCreateDeviceFleet
--
--         , requestCreateArtifact $
--             newCreateArtifact
--
--         , requestUpdateDevices $
--             newUpdateDevices
--
--         , requestListArtifacts $
--             newListArtifacts
--
--         , requestDeleteDeviceFleet $
--             newDeleteDeviceFleet
--
--         , requestListMonitoringExecutions $
--             newListMonitoringExecutions
--
--         , requestListCompilationJobs $
--             newListCompilationJobs
--
--         , requestListActions $
--             newListActions
--
--         , requestListDeviceFleets $
--             newListDeviceFleets
--
--         , requestDescribeModelPackageGroup $
--             newDescribeModelPackageGroup
--
--         , requestStopHyperParameterTuningJob $
--             newStopHyperParameterTuningJob
--
--         , requestDescribeTrial $
--             newDescribeTrial
--
--         , requestUpdateDeviceFleet $
--             newUpdateDeviceFleet
--
--         , requestListLabelingJobsForWorkteam $
--             newListLabelingJobsForWorkteam
--
--         , requestCreateFeatureGroup $
--             newCreateFeatureGroup
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestListImageVersions $
--             newListImageVersions
--
--         , requestStopProcessingJob $
--             newStopProcessingJob
--
--         , requestDeleteImageVersion $
--             newDeleteImageVersion
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestDescribeExperiment $
--             newDescribeExperiment
--
--         , requestDescribeAutoMLJob $
--             newDescribeAutoMLJob
--
--         , requestDescribeApp $
--             newDescribeApp
--
--         , requestListTrialComponents $
--             newListTrialComponents
--
--         , requestUpdateTrialComponent $
--             newUpdateTrialComponent
--
--         , requestDeleteTrialComponent $
--             newDeleteTrialComponent
--
--         , requestCreateTrialComponent $
--             newCreateTrialComponent
--
--         , requestDescribeWorkforce $
--             newDescribeWorkforce
--
--         , requestListNotebookInstances $
--             newListNotebookInstances
--
--         , requestListModelExplainabilityJobDefinitions $
--             newListModelExplainabilityJobDefinitions
--
--         , requestDeleteModelQualityJobDefinition $
--             newDeleteModelQualityJobDefinition
--
--         , requestStopLabelingJob $
--             newStopLabelingJob
--
--         , requestListModelQualityJobDefinitions $
--             newListModelQualityJobDefinitions
--
--         , requestDescribeModelBiasJobDefinition $
--             newDescribeModelBiasJobDefinition
--
--         , requestDescribeWorkteam $
--             newDescribeWorkteam
--
--         , requestDescribeNotebookInstanceLifecycleConfig $
--             newDescribeNotebookInstanceLifecycleConfig
--
--         , requestListPipelineExecutions $
--             newListPipelineExecutions
--
--         , requestUpdateDomain $
--             newUpdateDomain
--
--         , requestAssociateTrialComponent $
--             newAssociateTrialComponent
--
--         , requestUpdatePipelineExecution $
--             newUpdatePipelineExecution
--
--         , requestCreateImageVersion $
--             newCreateImageVersion
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestUpdateTrainingJob $
--             newUpdateTrainingJob
--
--         , requestUpdateImage $
--             newUpdateImage
--
--         , requestUpdateContext $
--             newUpdateContext
--
--         , requestDeleteImage $
--             newDeleteImage
--
--         , requestListFlowDefinitions $
--             newListFlowDefinitions
--
--         , requestListModels $
--             newListModels
--
--         , requestCreateUserProfile $
--             newCreateUserProfile
--
--         , requestRenderUiTemplate $
--             newRenderUiTemplate
--
--         , requestDescribeFeatureGroup $
--             newDescribeFeatureGroup
--
--         , requestDeleteContext $
--             newDeleteContext
--
--         , requestListHyperParameterTuningJobs $
--             newListHyperParameterTuningJobs
--
--         , requestDeleteFlowDefinition $
--             newDeleteFlowDefinition
--
--         , requestListAlgorithms $
--             newListAlgorithms
--
--         , requestCreateAlgorithm $
--             newCreateAlgorithm
--
--         , requestCreateFlowDefinition $
--             newCreateFlowDefinition
--
--         , requestListPipelineParametersForExecution $
--             newListPipelineParametersForExecution
--
--         , requestListTrials $
--             newListTrials
--
--         , requestCreateHyperParameterTuningJob $
--             newCreateHyperParameterTuningJob
--
--         , requestCreateModel $
--             newCreateModel
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
--         , requestListModelPackageGroups $
--             newListModelPackageGroups
--
--         , requestListPipelines $
--             newListPipelines
--
--         , requestListTags $
--             newListTags
--
--         , requestDescribePipelineDefinitionForExecution $
--             newDescribePipelineDefinitionForExecution
--
--         , requestDeleteTrial $
--             newDeleteTrial
--
--         , requestPutModelPackageGroupPolicy $
--             newPutModelPackageGroupPolicy
--
--         , requestListExperiments $
--             newListExperiments
--
--         , requestUpdateExperiment $
--             newUpdateExperiment
--
--         , requestDeleteExperiment $
--             newDeleteExperiment
--
--         , requestListLabelingJobs $
--             newListLabelingJobs
--
--         , requestDescribeImageVersion $
--             newDescribeImageVersion
--
--         , requestDeleteApp $
--             newDeleteApp
--
--         , requestCreateModelBiasJobDefinition $
--             newCreateModelBiasJobDefinition
--
--         , requestDescribeTrialComponent $
--             newDescribeTrialComponent
--
--         , requestCreateWorkteam $
--             newCreateWorkteam
--
--         , requestDescribeProject $
--             newDescribeProject
--
--         , requestCreateProcessingJob $
--             newCreateProcessingJob
--
--         , requestListAssociations $
--             newListAssociations
--
--         , requestEnableSagemakerServicecatalogPortfolio $
--             newEnableSagemakerServicecatalogPortfolio
--
--         , requestUpdateAppImageConfig $
--             newUpdateAppImageConfig
--
--         , requestListModelBiasJobDefinitions $
--             newListModelBiasJobDefinitions
--
--         , requestCreateAutoMLJob $
--             newCreateAutoMLJob
--
--         , requestCreateApp $
--             newCreateApp
--
--         , requestDescribeNotebookInstance $
--             newDescribeNotebookInstance
--
--         , requestDeleteAppImageConfig $
--             newDeleteAppImageConfig
--
--         , requestCreateEndpointConfig $
--             newCreateEndpointConfig
--
--         , requestListProcessingJobs $
--             newListProcessingJobs
--
--         , requestCreateMonitoringSchedule $
--             newCreateMonitoringSchedule
--
--         , requestDescribeModelExplainabilityJobDefinition $
--             newDescribeModelExplainabilityJobDefinition
--
--         , requestListWorkteams $
--             newListWorkteams
--
--         , requestDescribeFlowDefinition $
--             newDescribeFlowDefinition
--
--         , requestDescribeContext $
--             newDescribeContext
--
--         , requestRegisterDevices $
--             newRegisterDevices
--
--         , requestListFeatureGroups $
--             newListFeatureGroups
--
--         , requestCreatePresignedNotebookInstanceUrl $
--             newCreatePresignedNotebookInstanceUrl
--
--         , requestDescribeTrainingJob $
--             newDescribeTrainingJob
--
--         , requestCreateHumanTaskUi $
--             newCreateHumanTaskUi
--
--         , requestListTrainingJobsForHyperParameterTuningJob $
--             newListTrainingJobsForHyperParameterTuningJob
--
--         , requestDescribeImage $
--             newDescribeImage
--
--         , requestDeleteFeatureGroup $
--             newDeleteFeatureGroup
--
--           ]

--     , testGroup "response"
--         [ responseDisassociateTrialComponent $
--             newDisassociateTrialComponentResponse
--
--         , responseDeleteArtifact $
--             newDeleteArtifactResponse
--
--         , responseCreateTransformJob $
--             newCreateTransformJobResponse
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
--         , responseDescribePipeline $
--             newDescribePipelineResponse
--
--         , responseUpdateArtifact $
--             newUpdateArtifactResponse
--
--         , responseDescribeUserProfile $
--             newDescribeUserProfileResponse
--
--         , responseStopTrainingJob $
--             newStopTrainingJobResponse
--
--         , responseCreateEndpoint $
--             newCreateEndpointResponse
--
--         , responseGetSearchSuggestions $
--             newGetSearchSuggestionsResponse
--
--         , responseDeleteAction $
--             newDeleteActionResponse
--
--         , responseCreateEdgePackagingJob $
--             newCreateEdgePackagingJobResponse
--
--         , responseDescribeEndpointConfig $
--             newDescribeEndpointConfigResponse
--
--         , responseListModelPackages $
--             newListModelPackagesResponse
--
--         , responseGetModelPackageGroupPolicy $
--             newGetModelPackageGroupPolicyResponse
--
--         , responseDescribeMonitoringSchedule $
--             newDescribeMonitoringScheduleResponse
--
--         , responseCreateModelExplainabilityJobDefinition $
--             newCreateModelExplainabilityJobDefinitionResponse
--
--         , responseDescribeLabelingJob $
--             newDescribeLabelingJobResponse
--
--         , responseCreateNotebookInstance $
--             newCreateNotebookInstanceResponse
--
--         , responseUpdateModelPackage $
--             newUpdateModelPackageResponse
--
--         , responseCreateModelQualityJobDefinition $
--             newCreateModelQualityJobDefinitionResponse
--
--         , responseDeleteModelPackage $
--             newDeleteModelPackageResponse
--
--         , responseListProjects $
--             newListProjectsResponse
--
--         , responseListSubscribedWorkteams $
--             newListSubscribedWorkteamsResponse
--
--         , responseDeleteNotebookInstance $
--             newDeleteNotebookInstanceResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseDescribeProcessingJob $
--             newDescribeProcessingJobResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--         , responseDeleteModelExplainabilityJobDefinition $
--             newDeleteModelExplainabilityJobDefinitionResponse
--
--         , responseStopMonitoringSchedule $
--             newStopMonitoringScheduleResponse
--
--         , responseListDevices $
--             newListDevicesResponse
--
--         , responseCreateModelPackage $
--             newCreateModelPackageResponse
--
--         , responseUpdateNotebookInstance $
--             newUpdateNotebookInstanceResponse
--
--         , responseStopAutoMLJob $
--             newStopAutoMLJobResponse
--
--         , responseDescribeAppImageConfig $
--             newDescribeAppImageConfigResponse
--
--         , responseStartMonitoringSchedule $
--             newStartMonitoringScheduleResponse
--
--         , responseStopCompilationJob $
--             newStopCompilationJobResponse
--
--         , responseCreateTrial $
--             newCreateTrialResponse
--
--         , responseGetSagemakerServicecatalogPortfolioStatus $
--             newGetSagemakerServicecatalogPortfolioStatusResponse
--
--         , responseUpdateCodeRepository $
--             newUpdateCodeRepositoryResponse
--
--         , responseSearch $
--             newSearchResponse
--
--         , responseDeleteModel $
--             newDeleteModelResponse
--
--         , responseDeleteDataQualityJobDefinition $
--             newDeleteDataQualityJobDefinitionResponse
--
--         , responseListImages $
--             newListImagesResponse
--
--         , responseListTrainingJobs $
--             newListTrainingJobsResponse
--
--         , responseDescribeTransformJob $
--             newDescribeTransformJobResponse
--
--         , responseCreatePipeline $
--             newCreatePipelineResponse
--
--         , responseCreateModelPackageGroup $
--             newCreateModelPackageGroupResponse
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
--         , responseListDataQualityJobDefinitions $
--             newListDataQualityJobDefinitionsResponse
--
--         , responseDescribeEdgePackagingJob $
--             newDescribeEdgePackagingJobResponse
--
--         , responseListContexts $
--             newListContextsResponse
--
--         , responseDescribeEndpoint $
--             newDescribeEndpointResponse
--
--         , responseDeleteCodeRepository $
--             newDeleteCodeRepositoryResponse
--
--         , responseDeleteModelPackageGroupPolicy $
--             newDeleteModelPackageGroupPolicyResponse
--
--         , responseListUserProfiles $
--             newListUserProfilesResponse
--
--         , responseDescribeCompilationJob $
--             newDescribeCompilationJobResponse
--
--         , responseUpdatePipeline $
--             newUpdatePipelineResponse
--
--         , responseCreateCodeRepository $
--             newCreateCodeRepositoryResponse
--
--         , responseDescribeArtifact $
--             newDescribeArtifactResponse
--
--         , responseDescribeHumanTaskUi $
--             newDescribeHumanTaskUiResponse
--
--         , responseListPipelineExecutionSteps $
--             newListPipelineExecutionStepsResponse
--
--         , responseListCodeRepositories $
--             newListCodeRepositoriesResponse
--
--         , responseUpdateUserProfile $
--             newUpdateUserProfileResponse
--
--         , responseDescribeAction $
--             newDescribeActionResponse
--
--         , responseStopTransformJob $
--             newStopTransformJobResponse
--
--         , responseCreateTrainingJob $
--             newCreateTrainingJobResponse
--
--         , responseDeleteUserProfile $
--             newDeleteUserProfileResponse
--
--         , responseCreateContext $
--             newCreateContextResponse
--
--         , responseStopEdgePackagingJob $
--             newStopEdgePackagingJobResponse
--
--         , responseCreateImage $
--             newCreateImageResponse
--
--         , responseDeregisterDevices $
--             newDeregisterDevicesResponse
--
--         , responseCreateDataQualityJobDefinition $
--             newCreateDataQualityJobDefinitionResponse
--
--         , responseDeletePipeline $
--             newDeletePipelineResponse
--
--         , responseCreateAppImageConfig $
--             newCreateAppImageConfigResponse
--
--         , responseAddTags $
--             newAddTagsResponse
--
--         , responseDisableSagemakerServicecatalogPortfolio $
--             newDisableSagemakerServicecatalogPortfolioResponse
--
--         , responseDeleteAssociation $
--             newDeleteAssociationResponse
--
--         , responseUpdateMonitoringSchedule $
--             newUpdateMonitoringScheduleResponse
--
--         , responseListMonitoringSchedules $
--             newListMonitoringSchedulesResponse
--
--         , responseStopNotebookInstance $
--             newStopNotebookInstanceResponse
--
--         , responseDeleteMonitoringSchedule $
--             newDeleteMonitoringScheduleResponse
--
--         , responseDeleteEndpointConfig $
--             newDeleteEndpointConfigResponse
--
--         , responseStartPipelineExecution $
--             newStartPipelineExecutionResponse
--
--         , responseDescribeModelPackage $
--             newDescribeModelPackageResponse
--
--         , responseDeleteTags $
--             newDeleteTagsResponse
--
--         , responseAddAssociation $
--             newAddAssociationResponse
--
--         , responseCreateNotebookInstanceLifecycleConfig $
--             newCreateNotebookInstanceLifecycleConfigResponse
--
--         , responseListApps $
--             newListAppsResponse
--
--         , responseCreateWorkforce $
--             newCreateWorkforceResponse
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
--         , responseStopPipelineExecution $
--             newStopPipelineExecutionResponse
--
--         , responseListEndpointConfigs $
--             newListEndpointConfigsResponse
--
--         , responseDeleteWorkteam $
--             newDeleteWorkteamResponse
--
--         , responseDeleteWorkforce $
--             newDeleteWorkforceResponse
--
--         , responseDeleteModelBiasJobDefinition $
--             newDeleteModelBiasJobDefinitionResponse
--
--         , responseUpdateWorkforce $
--             newUpdateWorkforceResponse
--
--         , responseDescribeDevice $
--             newDescribeDeviceResponse
--
--         , responseDescribeDomain $
--             newDescribeDomainResponse
--
--         , responseDeleteNotebookInstanceLifecycleConfig $
--             newDeleteNotebookInstanceLifecycleConfigResponse
--
--         , responseDescribePipelineExecution $
--             newDescribePipelineExecutionResponse
--
--         , responseUpdateWorkteam $
--             newUpdateWorkteamResponse
--
--         , responseCreateLabelingJob $
--             newCreateLabelingJobResponse
--
--         , responseDescribeModelQualityJobDefinition $
--             newDescribeModelQualityJobDefinitionResponse
--
--         , responseCreateExperiment $
--             newCreateExperimentResponse
--
--         , responseListWorkforces $
--             newListWorkforcesResponse
--
--         , responseListAppImageConfigs $
--             newListAppImageConfigsResponse
--
--         , responseUpdateNotebookInstanceLifecycleConfig $
--             newUpdateNotebookInstanceLifecycleConfigResponse
--
--         , responseDescribeSubscribedWorkteam $
--             newDescribeSubscribedWorkteamResponse
--
--         , responseListNotebookInstanceLifecycleConfigs $
--             newListNotebookInstanceLifecycleConfigsResponse
--
--         , responseListEdgePackagingJobs $
--             newListEdgePackagingJobsResponse
--
--         , responseDescribeCodeRepository $
--             newDescribeCodeRepositoryResponse
--
--         , responseListEndpoints $
--             newListEndpointsResponse
--
--         , responseDescribeDataQualityJobDefinition $
--             newDescribeDataQualityJobDefinitionResponse
--
--         , responseDescribeAlgorithm $
--             newDescribeAlgorithmResponse
--
--         , responseCreateAction $
--             newCreateActionResponse
--
--         , responseDeleteEndpoint $
--             newDeleteEndpointResponse
--
--         , responseCreatePresignedDomainUrl $
--             newCreatePresignedDomainUrlResponse
--
--         , responseListTransformJobs $
--             newListTransformJobsResponse
--
--         , responseDescribeHyperParameterTuningJob $
--             newDescribeHyperParameterTuningJobResponse
--
--         , responseCreateCompilationJob $
--             newCreateCompilationJobResponse
--
--         , responseUpdateEndpoint $
--             newUpdateEndpointResponse
--
--         , responseDescribeModel $
--             newDescribeModelResponse
--
--         , responseCreateDeviceFleet $
--             newCreateDeviceFleetResponse
--
--         , responseCreateArtifact $
--             newCreateArtifactResponse
--
--         , responseUpdateDevices $
--             newUpdateDevicesResponse
--
--         , responseListArtifacts $
--             newListArtifactsResponse
--
--         , responseDeleteDeviceFleet $
--             newDeleteDeviceFleetResponse
--
--         , responseListMonitoringExecutions $
--             newListMonitoringExecutionsResponse
--
--         , responseListCompilationJobs $
--             newListCompilationJobsResponse
--
--         , responseListActions $
--             newListActionsResponse
--
--         , responseListDeviceFleets $
--             newListDeviceFleetsResponse
--
--         , responseDescribeModelPackageGroup $
--             newDescribeModelPackageGroupResponse
--
--         , responseStopHyperParameterTuningJob $
--             newStopHyperParameterTuningJobResponse
--
--         , responseDescribeTrial $
--             newDescribeTrialResponse
--
--         , responseUpdateDeviceFleet $
--             newUpdateDeviceFleetResponse
--
--         , responseListLabelingJobsForWorkteam $
--             newListLabelingJobsForWorkteamResponse
--
--         , responseCreateFeatureGroup $
--             newCreateFeatureGroupResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseListImageVersions $
--             newListImageVersionsResponse
--
--         , responseStopProcessingJob $
--             newStopProcessingJobResponse
--
--         , responseDeleteImageVersion $
--             newDeleteImageVersionResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseDescribeExperiment $
--             newDescribeExperimentResponse
--
--         , responseDescribeAutoMLJob $
--             newDescribeAutoMLJobResponse
--
--         , responseDescribeApp $
--             newDescribeAppResponse
--
--         , responseListTrialComponents $
--             newListTrialComponentsResponse
--
--         , responseUpdateTrialComponent $
--             newUpdateTrialComponentResponse
--
--         , responseDeleteTrialComponent $
--             newDeleteTrialComponentResponse
--
--         , responseCreateTrialComponent $
--             newCreateTrialComponentResponse
--
--         , responseDescribeWorkforce $
--             newDescribeWorkforceResponse
--
--         , responseListNotebookInstances $
--             newListNotebookInstancesResponse
--
--         , responseListModelExplainabilityJobDefinitions $
--             newListModelExplainabilityJobDefinitionsResponse
--
--         , responseDeleteModelQualityJobDefinition $
--             newDeleteModelQualityJobDefinitionResponse
--
--         , responseStopLabelingJob $
--             newStopLabelingJobResponse
--
--         , responseListModelQualityJobDefinitions $
--             newListModelQualityJobDefinitionsResponse
--
--         , responseDescribeModelBiasJobDefinition $
--             newDescribeModelBiasJobDefinitionResponse
--
--         , responseDescribeWorkteam $
--             newDescribeWorkteamResponse
--
--         , responseDescribeNotebookInstanceLifecycleConfig $
--             newDescribeNotebookInstanceLifecycleConfigResponse
--
--         , responseListPipelineExecutions $
--             newListPipelineExecutionsResponse
--
--         , responseUpdateDomain $
--             newUpdateDomainResponse
--
--         , responseAssociateTrialComponent $
--             newAssociateTrialComponentResponse
--
--         , responseUpdatePipelineExecution $
--             newUpdatePipelineExecutionResponse
--
--         , responseCreateImageVersion $
--             newCreateImageVersionResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseUpdateTrainingJob $
--             newUpdateTrainingJobResponse
--
--         , responseUpdateImage $
--             newUpdateImageResponse
--
--         , responseUpdateContext $
--             newUpdateContextResponse
--
--         , responseDeleteImage $
--             newDeleteImageResponse
--
--         , responseListFlowDefinitions $
--             newListFlowDefinitionsResponse
--
--         , responseListModels $
--             newListModelsResponse
--
--         , responseCreateUserProfile $
--             newCreateUserProfileResponse
--
--         , responseRenderUiTemplate $
--             newRenderUiTemplateResponse
--
--         , responseDescribeFeatureGroup $
--             newDescribeFeatureGroupResponse
--
--         , responseDeleteContext $
--             newDeleteContextResponse
--
--         , responseListHyperParameterTuningJobs $
--             newListHyperParameterTuningJobsResponse
--
--         , responseDeleteFlowDefinition $
--             newDeleteFlowDefinitionResponse
--
--         , responseListAlgorithms $
--             newListAlgorithmsResponse
--
--         , responseCreateAlgorithm $
--             newCreateAlgorithmResponse
--
--         , responseCreateFlowDefinition $
--             newCreateFlowDefinitionResponse
--
--         , responseListPipelineParametersForExecution $
--             newListPipelineParametersForExecutionResponse
--
--         , responseListTrials $
--             newListTrialsResponse
--
--         , responseCreateHyperParameterTuningJob $
--             newCreateHyperParameterTuningJobResponse
--
--         , responseCreateModel $
--             newCreateModelResponse
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
--         , responseListModelPackageGroups $
--             newListModelPackageGroupsResponse
--
--         , responseListPipelines $
--             newListPipelinesResponse
--
--         , responseListTags $
--             newListTagsResponse
--
--         , responseDescribePipelineDefinitionForExecution $
--             newDescribePipelineDefinitionForExecutionResponse
--
--         , responseDeleteTrial $
--             newDeleteTrialResponse
--
--         , responsePutModelPackageGroupPolicy $
--             newPutModelPackageGroupPolicyResponse
--
--         , responseListExperiments $
--             newListExperimentsResponse
--
--         , responseUpdateExperiment $
--             newUpdateExperimentResponse
--
--         , responseDeleteExperiment $
--             newDeleteExperimentResponse
--
--         , responseListLabelingJobs $
--             newListLabelingJobsResponse
--
--         , responseDescribeImageVersion $
--             newDescribeImageVersionResponse
--
--         , responseDeleteApp $
--             newDeleteAppResponse
--
--         , responseCreateModelBiasJobDefinition $
--             newCreateModelBiasJobDefinitionResponse
--
--         , responseDescribeTrialComponent $
--             newDescribeTrialComponentResponse
--
--         , responseCreateWorkteam $
--             newCreateWorkteamResponse
--
--         , responseDescribeProject $
--             newDescribeProjectResponse
--
--         , responseCreateProcessingJob $
--             newCreateProcessingJobResponse
--
--         , responseListAssociations $
--             newListAssociationsResponse
--
--         , responseEnableSagemakerServicecatalogPortfolio $
--             newEnableSagemakerServicecatalogPortfolioResponse
--
--         , responseUpdateAppImageConfig $
--             newUpdateAppImageConfigResponse
--
--         , responseListModelBiasJobDefinitions $
--             newListModelBiasJobDefinitionsResponse
--
--         , responseCreateAutoMLJob $
--             newCreateAutoMLJobResponse
--
--         , responseCreateApp $
--             newCreateAppResponse
--
--         , responseDescribeNotebookInstance $
--             newDescribeNotebookInstanceResponse
--
--         , responseDeleteAppImageConfig $
--             newDeleteAppImageConfigResponse
--
--         , responseCreateEndpointConfig $
--             newCreateEndpointConfigResponse
--
--         , responseListProcessingJobs $
--             newListProcessingJobsResponse
--
--         , responseCreateMonitoringSchedule $
--             newCreateMonitoringScheduleResponse
--
--         , responseDescribeModelExplainabilityJobDefinition $
--             newDescribeModelExplainabilityJobDefinitionResponse
--
--         , responseListWorkteams $
--             newListWorkteamsResponse
--
--         , responseDescribeFlowDefinition $
--             newDescribeFlowDefinitionResponse
--
--         , responseDescribeContext $
--             newDescribeContextResponse
--
--         , responseRegisterDevices $
--             newRegisterDevicesResponse
--
--         , responseListFeatureGroups $
--             newListFeatureGroupsResponse
--
--         , responseCreatePresignedNotebookInstanceUrl $
--             newCreatePresignedNotebookInstanceUrlResponse
--
--         , responseDescribeTrainingJob $
--             newDescribeTrainingJobResponse
--
--         , responseCreateHumanTaskUi $
--             newCreateHumanTaskUiResponse
--
--         , responseListTrainingJobsForHyperParameterTuningJob $
--             newListTrainingJobsForHyperParameterTuningJobResponse
--
--         , responseDescribeImage $
--             newDescribeImageResponse
--
--         , responseDeleteFeatureGroup $
--             newDeleteFeatureGroupResponse
--
--           ]
--     ]

-- Requests

requestDisassociateTrialComponent :: DisassociateTrialComponent -> TestTree
requestDisassociateTrialComponent =
  req
    "DisassociateTrialComponent"
    "fixture/DisassociateTrialComponent.yaml"

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

requestDescribeUserProfile :: DescribeUserProfile -> TestTree
requestDescribeUserProfile =
  req
    "DescribeUserProfile"
    "fixture/DescribeUserProfile.yaml"

requestStopTrainingJob :: StopTrainingJob -> TestTree
requestStopTrainingJob =
  req
    "StopTrainingJob"
    "fixture/StopTrainingJob.yaml"

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

requestDeleteAction :: DeleteAction -> TestTree
requestDeleteAction =
  req
    "DeleteAction"
    "fixture/DeleteAction.yaml"

requestCreateEdgePackagingJob :: CreateEdgePackagingJob -> TestTree
requestCreateEdgePackagingJob =
  req
    "CreateEdgePackagingJob"
    "fixture/CreateEdgePackagingJob.yaml"

requestDescribeEndpointConfig :: DescribeEndpointConfig -> TestTree
requestDescribeEndpointConfig =
  req
    "DescribeEndpointConfig"
    "fixture/DescribeEndpointConfig.yaml"

requestListModelPackages :: ListModelPackages -> TestTree
requestListModelPackages =
  req
    "ListModelPackages"
    "fixture/ListModelPackages.yaml"

requestGetModelPackageGroupPolicy :: GetModelPackageGroupPolicy -> TestTree
requestGetModelPackageGroupPolicy =
  req
    "GetModelPackageGroupPolicy"
    "fixture/GetModelPackageGroupPolicy.yaml"

requestDescribeMonitoringSchedule :: DescribeMonitoringSchedule -> TestTree
requestDescribeMonitoringSchedule =
  req
    "DescribeMonitoringSchedule"
    "fixture/DescribeMonitoringSchedule.yaml"

requestCreateModelExplainabilityJobDefinition :: CreateModelExplainabilityJobDefinition -> TestTree
requestCreateModelExplainabilityJobDefinition =
  req
    "CreateModelExplainabilityJobDefinition"
    "fixture/CreateModelExplainabilityJobDefinition.yaml"

requestDescribeLabelingJob :: DescribeLabelingJob -> TestTree
requestDescribeLabelingJob =
  req
    "DescribeLabelingJob"
    "fixture/DescribeLabelingJob.yaml"

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

requestCreateModelQualityJobDefinition :: CreateModelQualityJobDefinition -> TestTree
requestCreateModelQualityJobDefinition =
  req
    "CreateModelQualityJobDefinition"
    "fixture/CreateModelQualityJobDefinition.yaml"

requestDeleteModelPackage :: DeleteModelPackage -> TestTree
requestDeleteModelPackage =
  req
    "DeleteModelPackage"
    "fixture/DeleteModelPackage.yaml"

requestListProjects :: ListProjects -> TestTree
requestListProjects =
  req
    "ListProjects"
    "fixture/ListProjects.yaml"

requestListSubscribedWorkteams :: ListSubscribedWorkteams -> TestTree
requestListSubscribedWorkteams =
  req
    "ListSubscribedWorkteams"
    "fixture/ListSubscribedWorkteams.yaml"

requestDeleteNotebookInstance :: DeleteNotebookInstance -> TestTree
requestDeleteNotebookInstance =
  req
    "DeleteNotebookInstance"
    "fixture/DeleteNotebookInstance.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestDescribeProcessingJob :: DescribeProcessingJob -> TestTree
requestDescribeProcessingJob =
  req
    "DescribeProcessingJob"
    "fixture/DescribeProcessingJob.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

requestDeleteModelExplainabilityJobDefinition :: DeleteModelExplainabilityJobDefinition -> TestTree
requestDeleteModelExplainabilityJobDefinition =
  req
    "DeleteModelExplainabilityJobDefinition"
    "fixture/DeleteModelExplainabilityJobDefinition.yaml"

requestStopMonitoringSchedule :: StopMonitoringSchedule -> TestTree
requestStopMonitoringSchedule =
  req
    "StopMonitoringSchedule"
    "fixture/StopMonitoringSchedule.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices =
  req
    "ListDevices"
    "fixture/ListDevices.yaml"

requestCreateModelPackage :: CreateModelPackage -> TestTree
requestCreateModelPackage =
  req
    "CreateModelPackage"
    "fixture/CreateModelPackage.yaml"

requestUpdateNotebookInstance :: UpdateNotebookInstance -> TestTree
requestUpdateNotebookInstance =
  req
    "UpdateNotebookInstance"
    "fixture/UpdateNotebookInstance.yaml"

requestStopAutoMLJob :: StopAutoMLJob -> TestTree
requestStopAutoMLJob =
  req
    "StopAutoMLJob"
    "fixture/StopAutoMLJob.yaml"

requestDescribeAppImageConfig :: DescribeAppImageConfig -> TestTree
requestDescribeAppImageConfig =
  req
    "DescribeAppImageConfig"
    "fixture/DescribeAppImageConfig.yaml"

requestStartMonitoringSchedule :: StartMonitoringSchedule -> TestTree
requestStartMonitoringSchedule =
  req
    "StartMonitoringSchedule"
    "fixture/StartMonitoringSchedule.yaml"

requestStopCompilationJob :: StopCompilationJob -> TestTree
requestStopCompilationJob =
  req
    "StopCompilationJob"
    "fixture/StopCompilationJob.yaml"

requestCreateTrial :: CreateTrial -> TestTree
requestCreateTrial =
  req
    "CreateTrial"
    "fixture/CreateTrial.yaml"

requestGetSagemakerServicecatalogPortfolioStatus :: GetSagemakerServicecatalogPortfolioStatus -> TestTree
requestGetSagemakerServicecatalogPortfolioStatus =
  req
    "GetSagemakerServicecatalogPortfolioStatus"
    "fixture/GetSagemakerServicecatalogPortfolioStatus.yaml"

requestUpdateCodeRepository :: UpdateCodeRepository -> TestTree
requestUpdateCodeRepository =
  req
    "UpdateCodeRepository"
    "fixture/UpdateCodeRepository.yaml"

requestSearch :: Search -> TestTree
requestSearch =
  req
    "Search"
    "fixture/Search.yaml"

requestDeleteModel :: DeleteModel -> TestTree
requestDeleteModel =
  req
    "DeleteModel"
    "fixture/DeleteModel.yaml"

requestDeleteDataQualityJobDefinition :: DeleteDataQualityJobDefinition -> TestTree
requestDeleteDataQualityJobDefinition =
  req
    "DeleteDataQualityJobDefinition"
    "fixture/DeleteDataQualityJobDefinition.yaml"

requestListImages :: ListImages -> TestTree
requestListImages =
  req
    "ListImages"
    "fixture/ListImages.yaml"

requestListTrainingJobs :: ListTrainingJobs -> TestTree
requestListTrainingJobs =
  req
    "ListTrainingJobs"
    "fixture/ListTrainingJobs.yaml"

requestDescribeTransformJob :: DescribeTransformJob -> TestTree
requestDescribeTransformJob =
  req
    "DescribeTransformJob"
    "fixture/DescribeTransformJob.yaml"

requestCreatePipeline :: CreatePipeline -> TestTree
requestCreatePipeline =
  req
    "CreatePipeline"
    "fixture/CreatePipeline.yaml"

requestCreateModelPackageGroup :: CreateModelPackageGroup -> TestTree
requestCreateModelPackageGroup =
  req
    "CreateModelPackageGroup"
    "fixture/CreateModelPackageGroup.yaml"

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

requestListDataQualityJobDefinitions :: ListDataQualityJobDefinitions -> TestTree
requestListDataQualityJobDefinitions =
  req
    "ListDataQualityJobDefinitions"
    "fixture/ListDataQualityJobDefinitions.yaml"

requestDescribeEdgePackagingJob :: DescribeEdgePackagingJob -> TestTree
requestDescribeEdgePackagingJob =
  req
    "DescribeEdgePackagingJob"
    "fixture/DescribeEdgePackagingJob.yaml"

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

requestDeleteCodeRepository :: DeleteCodeRepository -> TestTree
requestDeleteCodeRepository =
  req
    "DeleteCodeRepository"
    "fixture/DeleteCodeRepository.yaml"

requestDeleteModelPackageGroupPolicy :: DeleteModelPackageGroupPolicy -> TestTree
requestDeleteModelPackageGroupPolicy =
  req
    "DeleteModelPackageGroupPolicy"
    "fixture/DeleteModelPackageGroupPolicy.yaml"

requestListUserProfiles :: ListUserProfiles -> TestTree
requestListUserProfiles =
  req
    "ListUserProfiles"
    "fixture/ListUserProfiles.yaml"

requestDescribeCompilationJob :: DescribeCompilationJob -> TestTree
requestDescribeCompilationJob =
  req
    "DescribeCompilationJob"
    "fixture/DescribeCompilationJob.yaml"

requestUpdatePipeline :: UpdatePipeline -> TestTree
requestUpdatePipeline =
  req
    "UpdatePipeline"
    "fixture/UpdatePipeline.yaml"

requestCreateCodeRepository :: CreateCodeRepository -> TestTree
requestCreateCodeRepository =
  req
    "CreateCodeRepository"
    "fixture/CreateCodeRepository.yaml"

requestDescribeArtifact :: DescribeArtifact -> TestTree
requestDescribeArtifact =
  req
    "DescribeArtifact"
    "fixture/DescribeArtifact.yaml"

requestDescribeHumanTaskUi :: DescribeHumanTaskUi -> TestTree
requestDescribeHumanTaskUi =
  req
    "DescribeHumanTaskUi"
    "fixture/DescribeHumanTaskUi.yaml"

requestListPipelineExecutionSteps :: ListPipelineExecutionSteps -> TestTree
requestListPipelineExecutionSteps =
  req
    "ListPipelineExecutionSteps"
    "fixture/ListPipelineExecutionSteps.yaml"

requestListCodeRepositories :: ListCodeRepositories -> TestTree
requestListCodeRepositories =
  req
    "ListCodeRepositories"
    "fixture/ListCodeRepositories.yaml"

requestUpdateUserProfile :: UpdateUserProfile -> TestTree
requestUpdateUserProfile =
  req
    "UpdateUserProfile"
    "fixture/UpdateUserProfile.yaml"

requestDescribeAction :: DescribeAction -> TestTree
requestDescribeAction =
  req
    "DescribeAction"
    "fixture/DescribeAction.yaml"

requestStopTransformJob :: StopTransformJob -> TestTree
requestStopTransformJob =
  req
    "StopTransformJob"
    "fixture/StopTransformJob.yaml"

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

requestCreateImage :: CreateImage -> TestTree
requestCreateImage =
  req
    "CreateImage"
    "fixture/CreateImage.yaml"

requestDeregisterDevices :: DeregisterDevices -> TestTree
requestDeregisterDevices =
  req
    "DeregisterDevices"
    "fixture/DeregisterDevices.yaml"

requestCreateDataQualityJobDefinition :: CreateDataQualityJobDefinition -> TestTree
requestCreateDataQualityJobDefinition =
  req
    "CreateDataQualityJobDefinition"
    "fixture/CreateDataQualityJobDefinition.yaml"

requestDeletePipeline :: DeletePipeline -> TestTree
requestDeletePipeline =
  req
    "DeletePipeline"
    "fixture/DeletePipeline.yaml"

requestCreateAppImageConfig :: CreateAppImageConfig -> TestTree
requestCreateAppImageConfig =
  req
    "CreateAppImageConfig"
    "fixture/CreateAppImageConfig.yaml"

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

requestDeleteAssociation :: DeleteAssociation -> TestTree
requestDeleteAssociation =
  req
    "DeleteAssociation"
    "fixture/DeleteAssociation.yaml"

requestUpdateMonitoringSchedule :: UpdateMonitoringSchedule -> TestTree
requestUpdateMonitoringSchedule =
  req
    "UpdateMonitoringSchedule"
    "fixture/UpdateMonitoringSchedule.yaml"

requestListMonitoringSchedules :: ListMonitoringSchedules -> TestTree
requestListMonitoringSchedules =
  req
    "ListMonitoringSchedules"
    "fixture/ListMonitoringSchedules.yaml"

requestStopNotebookInstance :: StopNotebookInstance -> TestTree
requestStopNotebookInstance =
  req
    "StopNotebookInstance"
    "fixture/StopNotebookInstance.yaml"

requestDeleteMonitoringSchedule :: DeleteMonitoringSchedule -> TestTree
requestDeleteMonitoringSchedule =
  req
    "DeleteMonitoringSchedule"
    "fixture/DeleteMonitoringSchedule.yaml"

requestDeleteEndpointConfig :: DeleteEndpointConfig -> TestTree
requestDeleteEndpointConfig =
  req
    "DeleteEndpointConfig"
    "fixture/DeleteEndpointConfig.yaml"

requestStartPipelineExecution :: StartPipelineExecution -> TestTree
requestStartPipelineExecution =
  req
    "StartPipelineExecution"
    "fixture/StartPipelineExecution.yaml"

requestDescribeModelPackage :: DescribeModelPackage -> TestTree
requestDescribeModelPackage =
  req
    "DescribeModelPackage"
    "fixture/DescribeModelPackage.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags =
  req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

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

requestStopPipelineExecution :: StopPipelineExecution -> TestTree
requestStopPipelineExecution =
  req
    "StopPipelineExecution"
    "fixture/StopPipelineExecution.yaml"

requestListEndpointConfigs :: ListEndpointConfigs -> TestTree
requestListEndpointConfigs =
  req
    "ListEndpointConfigs"
    "fixture/ListEndpointConfigs.yaml"

requestDeleteWorkteam :: DeleteWorkteam -> TestTree
requestDeleteWorkteam =
  req
    "DeleteWorkteam"
    "fixture/DeleteWorkteam.yaml"

requestDeleteWorkforce :: DeleteWorkforce -> TestTree
requestDeleteWorkforce =
  req
    "DeleteWorkforce"
    "fixture/DeleteWorkforce.yaml"

requestDeleteModelBiasJobDefinition :: DeleteModelBiasJobDefinition -> TestTree
requestDeleteModelBiasJobDefinition =
  req
    "DeleteModelBiasJobDefinition"
    "fixture/DeleteModelBiasJobDefinition.yaml"

requestUpdateWorkforce :: UpdateWorkforce -> TestTree
requestUpdateWorkforce =
  req
    "UpdateWorkforce"
    "fixture/UpdateWorkforce.yaml"

requestDescribeDevice :: DescribeDevice -> TestTree
requestDescribeDevice =
  req
    "DescribeDevice"
    "fixture/DescribeDevice.yaml"

requestDescribeDomain :: DescribeDomain -> TestTree
requestDescribeDomain =
  req
    "DescribeDomain"
    "fixture/DescribeDomain.yaml"

requestDeleteNotebookInstanceLifecycleConfig :: DeleteNotebookInstanceLifecycleConfig -> TestTree
requestDeleteNotebookInstanceLifecycleConfig =
  req
    "DeleteNotebookInstanceLifecycleConfig"
    "fixture/DeleteNotebookInstanceLifecycleConfig.yaml"

requestDescribePipelineExecution :: DescribePipelineExecution -> TestTree
requestDescribePipelineExecution =
  req
    "DescribePipelineExecution"
    "fixture/DescribePipelineExecution.yaml"

requestUpdateWorkteam :: UpdateWorkteam -> TestTree
requestUpdateWorkteam =
  req
    "UpdateWorkteam"
    "fixture/UpdateWorkteam.yaml"

requestCreateLabelingJob :: CreateLabelingJob -> TestTree
requestCreateLabelingJob =
  req
    "CreateLabelingJob"
    "fixture/CreateLabelingJob.yaml"

requestDescribeModelQualityJobDefinition :: DescribeModelQualityJobDefinition -> TestTree
requestDescribeModelQualityJobDefinition =
  req
    "DescribeModelQualityJobDefinition"
    "fixture/DescribeModelQualityJobDefinition.yaml"

requestCreateExperiment :: CreateExperiment -> TestTree
requestCreateExperiment =
  req
    "CreateExperiment"
    "fixture/CreateExperiment.yaml"

requestListWorkforces :: ListWorkforces -> TestTree
requestListWorkforces =
  req
    "ListWorkforces"
    "fixture/ListWorkforces.yaml"

requestListAppImageConfigs :: ListAppImageConfigs -> TestTree
requestListAppImageConfigs =
  req
    "ListAppImageConfigs"
    "fixture/ListAppImageConfigs.yaml"

requestUpdateNotebookInstanceLifecycleConfig :: UpdateNotebookInstanceLifecycleConfig -> TestTree
requestUpdateNotebookInstanceLifecycleConfig =
  req
    "UpdateNotebookInstanceLifecycleConfig"
    "fixture/UpdateNotebookInstanceLifecycleConfig.yaml"

requestDescribeSubscribedWorkteam :: DescribeSubscribedWorkteam -> TestTree
requestDescribeSubscribedWorkteam =
  req
    "DescribeSubscribedWorkteam"
    "fixture/DescribeSubscribedWorkteam.yaml"

requestListNotebookInstanceLifecycleConfigs :: ListNotebookInstanceLifecycleConfigs -> TestTree
requestListNotebookInstanceLifecycleConfigs =
  req
    "ListNotebookInstanceLifecycleConfigs"
    "fixture/ListNotebookInstanceLifecycleConfigs.yaml"

requestListEdgePackagingJobs :: ListEdgePackagingJobs -> TestTree
requestListEdgePackagingJobs =
  req
    "ListEdgePackagingJobs"
    "fixture/ListEdgePackagingJobs.yaml"

requestDescribeCodeRepository :: DescribeCodeRepository -> TestTree
requestDescribeCodeRepository =
  req
    "DescribeCodeRepository"
    "fixture/DescribeCodeRepository.yaml"

requestListEndpoints :: ListEndpoints -> TestTree
requestListEndpoints =
  req
    "ListEndpoints"
    "fixture/ListEndpoints.yaml"

requestDescribeDataQualityJobDefinition :: DescribeDataQualityJobDefinition -> TestTree
requestDescribeDataQualityJobDefinition =
  req
    "DescribeDataQualityJobDefinition"
    "fixture/DescribeDataQualityJobDefinition.yaml"

requestDescribeAlgorithm :: DescribeAlgorithm -> TestTree
requestDescribeAlgorithm =
  req
    "DescribeAlgorithm"
    "fixture/DescribeAlgorithm.yaml"

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

requestCreatePresignedDomainUrl :: CreatePresignedDomainUrl -> TestTree
requestCreatePresignedDomainUrl =
  req
    "CreatePresignedDomainUrl"
    "fixture/CreatePresignedDomainUrl.yaml"

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

requestCreateCompilationJob :: CreateCompilationJob -> TestTree
requestCreateCompilationJob =
  req
    "CreateCompilationJob"
    "fixture/CreateCompilationJob.yaml"

requestUpdateEndpoint :: UpdateEndpoint -> TestTree
requestUpdateEndpoint =
  req
    "UpdateEndpoint"
    "fixture/UpdateEndpoint.yaml"

requestDescribeModel :: DescribeModel -> TestTree
requestDescribeModel =
  req
    "DescribeModel"
    "fixture/DescribeModel.yaml"

requestCreateDeviceFleet :: CreateDeviceFleet -> TestTree
requestCreateDeviceFleet =
  req
    "CreateDeviceFleet"
    "fixture/CreateDeviceFleet.yaml"

requestCreateArtifact :: CreateArtifact -> TestTree
requestCreateArtifact =
  req
    "CreateArtifact"
    "fixture/CreateArtifact.yaml"

requestUpdateDevices :: UpdateDevices -> TestTree
requestUpdateDevices =
  req
    "UpdateDevices"
    "fixture/UpdateDevices.yaml"

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

requestListMonitoringExecutions :: ListMonitoringExecutions -> TestTree
requestListMonitoringExecutions =
  req
    "ListMonitoringExecutions"
    "fixture/ListMonitoringExecutions.yaml"

requestListCompilationJobs :: ListCompilationJobs -> TestTree
requestListCompilationJobs =
  req
    "ListCompilationJobs"
    "fixture/ListCompilationJobs.yaml"

requestListActions :: ListActions -> TestTree
requestListActions =
  req
    "ListActions"
    "fixture/ListActions.yaml"

requestListDeviceFleets :: ListDeviceFleets -> TestTree
requestListDeviceFleets =
  req
    "ListDeviceFleets"
    "fixture/ListDeviceFleets.yaml"

requestDescribeModelPackageGroup :: DescribeModelPackageGroup -> TestTree
requestDescribeModelPackageGroup =
  req
    "DescribeModelPackageGroup"
    "fixture/DescribeModelPackageGroup.yaml"

requestStopHyperParameterTuningJob :: StopHyperParameterTuningJob -> TestTree
requestStopHyperParameterTuningJob =
  req
    "StopHyperParameterTuningJob"
    "fixture/StopHyperParameterTuningJob.yaml"

requestDescribeTrial :: DescribeTrial -> TestTree
requestDescribeTrial =
  req
    "DescribeTrial"
    "fixture/DescribeTrial.yaml"

requestUpdateDeviceFleet :: UpdateDeviceFleet -> TestTree
requestUpdateDeviceFleet =
  req
    "UpdateDeviceFleet"
    "fixture/UpdateDeviceFleet.yaml"

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

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

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

requestDeleteImageVersion :: DeleteImageVersion -> TestTree
requestDeleteImageVersion =
  req
    "DeleteImageVersion"
    "fixture/DeleteImageVersion.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject =
  req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestDescribeExperiment :: DescribeExperiment -> TestTree
requestDescribeExperiment =
  req
    "DescribeExperiment"
    "fixture/DescribeExperiment.yaml"

requestDescribeAutoMLJob :: DescribeAutoMLJob -> TestTree
requestDescribeAutoMLJob =
  req
    "DescribeAutoMLJob"
    "fixture/DescribeAutoMLJob.yaml"

requestDescribeApp :: DescribeApp -> TestTree
requestDescribeApp =
  req
    "DescribeApp"
    "fixture/DescribeApp.yaml"

requestListTrialComponents :: ListTrialComponents -> TestTree
requestListTrialComponents =
  req
    "ListTrialComponents"
    "fixture/ListTrialComponents.yaml"

requestUpdateTrialComponent :: UpdateTrialComponent -> TestTree
requestUpdateTrialComponent =
  req
    "UpdateTrialComponent"
    "fixture/UpdateTrialComponent.yaml"

requestDeleteTrialComponent :: DeleteTrialComponent -> TestTree
requestDeleteTrialComponent =
  req
    "DeleteTrialComponent"
    "fixture/DeleteTrialComponent.yaml"

requestCreateTrialComponent :: CreateTrialComponent -> TestTree
requestCreateTrialComponent =
  req
    "CreateTrialComponent"
    "fixture/CreateTrialComponent.yaml"

requestDescribeWorkforce :: DescribeWorkforce -> TestTree
requestDescribeWorkforce =
  req
    "DescribeWorkforce"
    "fixture/DescribeWorkforce.yaml"

requestListNotebookInstances :: ListNotebookInstances -> TestTree
requestListNotebookInstances =
  req
    "ListNotebookInstances"
    "fixture/ListNotebookInstances.yaml"

requestListModelExplainabilityJobDefinitions :: ListModelExplainabilityJobDefinitions -> TestTree
requestListModelExplainabilityJobDefinitions =
  req
    "ListModelExplainabilityJobDefinitions"
    "fixture/ListModelExplainabilityJobDefinitions.yaml"

requestDeleteModelQualityJobDefinition :: DeleteModelQualityJobDefinition -> TestTree
requestDeleteModelQualityJobDefinition =
  req
    "DeleteModelQualityJobDefinition"
    "fixture/DeleteModelQualityJobDefinition.yaml"

requestStopLabelingJob :: StopLabelingJob -> TestTree
requestStopLabelingJob =
  req
    "StopLabelingJob"
    "fixture/StopLabelingJob.yaml"

requestListModelQualityJobDefinitions :: ListModelQualityJobDefinitions -> TestTree
requestListModelQualityJobDefinitions =
  req
    "ListModelQualityJobDefinitions"
    "fixture/ListModelQualityJobDefinitions.yaml"

requestDescribeModelBiasJobDefinition :: DescribeModelBiasJobDefinition -> TestTree
requestDescribeModelBiasJobDefinition =
  req
    "DescribeModelBiasJobDefinition"
    "fixture/DescribeModelBiasJobDefinition.yaml"

requestDescribeWorkteam :: DescribeWorkteam -> TestTree
requestDescribeWorkteam =
  req
    "DescribeWorkteam"
    "fixture/DescribeWorkteam.yaml"

requestDescribeNotebookInstanceLifecycleConfig :: DescribeNotebookInstanceLifecycleConfig -> TestTree
requestDescribeNotebookInstanceLifecycleConfig =
  req
    "DescribeNotebookInstanceLifecycleConfig"
    "fixture/DescribeNotebookInstanceLifecycleConfig.yaml"

requestListPipelineExecutions :: ListPipelineExecutions -> TestTree
requestListPipelineExecutions =
  req
    "ListPipelineExecutions"
    "fixture/ListPipelineExecutions.yaml"

requestUpdateDomain :: UpdateDomain -> TestTree
requestUpdateDomain =
  req
    "UpdateDomain"
    "fixture/UpdateDomain.yaml"

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

requestCreateImageVersion :: CreateImageVersion -> TestTree
requestCreateImageVersion =
  req
    "CreateImageVersion"
    "fixture/CreateImageVersion.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestUpdateTrainingJob :: UpdateTrainingJob -> TestTree
requestUpdateTrainingJob =
  req
    "UpdateTrainingJob"
    "fixture/UpdateTrainingJob.yaml"

requestUpdateImage :: UpdateImage -> TestTree
requestUpdateImage =
  req
    "UpdateImage"
    "fixture/UpdateImage.yaml"

requestUpdateContext :: UpdateContext -> TestTree
requestUpdateContext =
  req
    "UpdateContext"
    "fixture/UpdateContext.yaml"

requestDeleteImage :: DeleteImage -> TestTree
requestDeleteImage =
  req
    "DeleteImage"
    "fixture/DeleteImage.yaml"

requestListFlowDefinitions :: ListFlowDefinitions -> TestTree
requestListFlowDefinitions =
  req
    "ListFlowDefinitions"
    "fixture/ListFlowDefinitions.yaml"

requestListModels :: ListModels -> TestTree
requestListModels =
  req
    "ListModels"
    "fixture/ListModels.yaml"

requestCreateUserProfile :: CreateUserProfile -> TestTree
requestCreateUserProfile =
  req
    "CreateUserProfile"
    "fixture/CreateUserProfile.yaml"

requestRenderUiTemplate :: RenderUiTemplate -> TestTree
requestRenderUiTemplate =
  req
    "RenderUiTemplate"
    "fixture/RenderUiTemplate.yaml"

requestDescribeFeatureGroup :: DescribeFeatureGroup -> TestTree
requestDescribeFeatureGroup =
  req
    "DescribeFeatureGroup"
    "fixture/DescribeFeatureGroup.yaml"

requestDeleteContext :: DeleteContext -> TestTree
requestDeleteContext =
  req
    "DeleteContext"
    "fixture/DeleteContext.yaml"

requestListHyperParameterTuningJobs :: ListHyperParameterTuningJobs -> TestTree
requestListHyperParameterTuningJobs =
  req
    "ListHyperParameterTuningJobs"
    "fixture/ListHyperParameterTuningJobs.yaml"

requestDeleteFlowDefinition :: DeleteFlowDefinition -> TestTree
requestDeleteFlowDefinition =
  req
    "DeleteFlowDefinition"
    "fixture/DeleteFlowDefinition.yaml"

requestListAlgorithms :: ListAlgorithms -> TestTree
requestListAlgorithms =
  req
    "ListAlgorithms"
    "fixture/ListAlgorithms.yaml"

requestCreateAlgorithm :: CreateAlgorithm -> TestTree
requestCreateAlgorithm =
  req
    "CreateAlgorithm"
    "fixture/CreateAlgorithm.yaml"

requestCreateFlowDefinition :: CreateFlowDefinition -> TestTree
requestCreateFlowDefinition =
  req
    "CreateFlowDefinition"
    "fixture/CreateFlowDefinition.yaml"

requestListPipelineParametersForExecution :: ListPipelineParametersForExecution -> TestTree
requestListPipelineParametersForExecution =
  req
    "ListPipelineParametersForExecution"
    "fixture/ListPipelineParametersForExecution.yaml"

requestListTrials :: ListTrials -> TestTree
requestListTrials =
  req
    "ListTrials"
    "fixture/ListTrials.yaml"

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

requestListTags :: ListTags -> TestTree
requestListTags =
  req
    "ListTags"
    "fixture/ListTags.yaml"

requestDescribePipelineDefinitionForExecution :: DescribePipelineDefinitionForExecution -> TestTree
requestDescribePipelineDefinitionForExecution =
  req
    "DescribePipelineDefinitionForExecution"
    "fixture/DescribePipelineDefinitionForExecution.yaml"

requestDeleteTrial :: DeleteTrial -> TestTree
requestDeleteTrial =
  req
    "DeleteTrial"
    "fixture/DeleteTrial.yaml"

requestPutModelPackageGroupPolicy :: PutModelPackageGroupPolicy -> TestTree
requestPutModelPackageGroupPolicy =
  req
    "PutModelPackageGroupPolicy"
    "fixture/PutModelPackageGroupPolicy.yaml"

requestListExperiments :: ListExperiments -> TestTree
requestListExperiments =
  req
    "ListExperiments"
    "fixture/ListExperiments.yaml"

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

requestListLabelingJobs :: ListLabelingJobs -> TestTree
requestListLabelingJobs =
  req
    "ListLabelingJobs"
    "fixture/ListLabelingJobs.yaml"

requestDescribeImageVersion :: DescribeImageVersion -> TestTree
requestDescribeImageVersion =
  req
    "DescribeImageVersion"
    "fixture/DescribeImageVersion.yaml"

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp =
  req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

requestCreateModelBiasJobDefinition :: CreateModelBiasJobDefinition -> TestTree
requestCreateModelBiasJobDefinition =
  req
    "CreateModelBiasJobDefinition"
    "fixture/CreateModelBiasJobDefinition.yaml"

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

requestDescribeProject :: DescribeProject -> TestTree
requestDescribeProject =
  req
    "DescribeProject"
    "fixture/DescribeProject.yaml"

requestCreateProcessingJob :: CreateProcessingJob -> TestTree
requestCreateProcessingJob =
  req
    "CreateProcessingJob"
    "fixture/CreateProcessingJob.yaml"

requestListAssociations :: ListAssociations -> TestTree
requestListAssociations =
  req
    "ListAssociations"
    "fixture/ListAssociations.yaml"

requestEnableSagemakerServicecatalogPortfolio :: EnableSagemakerServicecatalogPortfolio -> TestTree
requestEnableSagemakerServicecatalogPortfolio =
  req
    "EnableSagemakerServicecatalogPortfolio"
    "fixture/EnableSagemakerServicecatalogPortfolio.yaml"

requestUpdateAppImageConfig :: UpdateAppImageConfig -> TestTree
requestUpdateAppImageConfig =
  req
    "UpdateAppImageConfig"
    "fixture/UpdateAppImageConfig.yaml"

requestListModelBiasJobDefinitions :: ListModelBiasJobDefinitions -> TestTree
requestListModelBiasJobDefinitions =
  req
    "ListModelBiasJobDefinitions"
    "fixture/ListModelBiasJobDefinitions.yaml"

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

requestDescribeNotebookInstance :: DescribeNotebookInstance -> TestTree
requestDescribeNotebookInstance =
  req
    "DescribeNotebookInstance"
    "fixture/DescribeNotebookInstance.yaml"

requestDeleteAppImageConfig :: DeleteAppImageConfig -> TestTree
requestDeleteAppImageConfig =
  req
    "DeleteAppImageConfig"
    "fixture/DeleteAppImageConfig.yaml"

requestCreateEndpointConfig :: CreateEndpointConfig -> TestTree
requestCreateEndpointConfig =
  req
    "CreateEndpointConfig"
    "fixture/CreateEndpointConfig.yaml"

requestListProcessingJobs :: ListProcessingJobs -> TestTree
requestListProcessingJobs =
  req
    "ListProcessingJobs"
    "fixture/ListProcessingJobs.yaml"

requestCreateMonitoringSchedule :: CreateMonitoringSchedule -> TestTree
requestCreateMonitoringSchedule =
  req
    "CreateMonitoringSchedule"
    "fixture/CreateMonitoringSchedule.yaml"

requestDescribeModelExplainabilityJobDefinition :: DescribeModelExplainabilityJobDefinition -> TestTree
requestDescribeModelExplainabilityJobDefinition =
  req
    "DescribeModelExplainabilityJobDefinition"
    "fixture/DescribeModelExplainabilityJobDefinition.yaml"

requestListWorkteams :: ListWorkteams -> TestTree
requestListWorkteams =
  req
    "ListWorkteams"
    "fixture/ListWorkteams.yaml"

requestDescribeFlowDefinition :: DescribeFlowDefinition -> TestTree
requestDescribeFlowDefinition =
  req
    "DescribeFlowDefinition"
    "fixture/DescribeFlowDefinition.yaml"

requestDescribeContext :: DescribeContext -> TestTree
requestDescribeContext =
  req
    "DescribeContext"
    "fixture/DescribeContext.yaml"

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

requestCreatePresignedNotebookInstanceUrl :: CreatePresignedNotebookInstanceUrl -> TestTree
requestCreatePresignedNotebookInstanceUrl =
  req
    "CreatePresignedNotebookInstanceUrl"
    "fixture/CreatePresignedNotebookInstanceUrl.yaml"

requestDescribeTrainingJob :: DescribeTrainingJob -> TestTree
requestDescribeTrainingJob =
  req
    "DescribeTrainingJob"
    "fixture/DescribeTrainingJob.yaml"

requestCreateHumanTaskUi :: CreateHumanTaskUi -> TestTree
requestCreateHumanTaskUi =
  req
    "CreateHumanTaskUi"
    "fixture/CreateHumanTaskUi.yaml"

requestListTrainingJobsForHyperParameterTuningJob :: ListTrainingJobsForHyperParameterTuningJob -> TestTree
requestListTrainingJobsForHyperParameterTuningJob =
  req
    "ListTrainingJobsForHyperParameterTuningJob"
    "fixture/ListTrainingJobsForHyperParameterTuningJob.yaml"

requestDescribeImage :: DescribeImage -> TestTree
requestDescribeImage =
  req
    "DescribeImage"
    "fixture/DescribeImage.yaml"

requestDeleteFeatureGroup :: DeleteFeatureGroup -> TestTree
requestDeleteFeatureGroup =
  req
    "DeleteFeatureGroup"
    "fixture/DeleteFeatureGroup.yaml"

-- Responses

responseDisassociateTrialComponent :: DisassociateTrialComponentResponse -> TestTree
responseDisassociateTrialComponent =
  res
    "DisassociateTrialComponentResponse"
    "fixture/DisassociateTrialComponentResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateTrialComponent)

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

responseDescribeUserProfile :: DescribeUserProfileResponse -> TestTree
responseDescribeUserProfile =
  res
    "DescribeUserProfileResponse"
    "fixture/DescribeUserProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUserProfile)

responseStopTrainingJob :: StopTrainingJobResponse -> TestTree
responseStopTrainingJob =
  res
    "StopTrainingJobResponse"
    "fixture/StopTrainingJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopTrainingJob)

responseCreateEndpoint :: CreateEndpointResponse -> TestTree
responseCreateEndpoint =
  res
    "CreateEndpointResponse"
    "fixture/CreateEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEndpoint)

responseGetSearchSuggestions :: GetSearchSuggestionsResponse -> TestTree
responseGetSearchSuggestions =
  res
    "GetSearchSuggestionsResponse"
    "fixture/GetSearchSuggestionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetSearchSuggestions)

responseDeleteAction :: DeleteActionResponse -> TestTree
responseDeleteAction =
  res
    "DeleteActionResponse"
    "fixture/DeleteActionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAction)

responseCreateEdgePackagingJob :: CreateEdgePackagingJobResponse -> TestTree
responseCreateEdgePackagingJob =
  res
    "CreateEdgePackagingJobResponse"
    "fixture/CreateEdgePackagingJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEdgePackagingJob)

responseDescribeEndpointConfig :: DescribeEndpointConfigResponse -> TestTree
responseDescribeEndpointConfig =
  res
    "DescribeEndpointConfigResponse"
    "fixture/DescribeEndpointConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEndpointConfig)

responseListModelPackages :: ListModelPackagesResponse -> TestTree
responseListModelPackages =
  res
    "ListModelPackagesResponse"
    "fixture/ListModelPackagesResponse.proto"
    defaultService
    (Proxy :: Proxy ListModelPackages)

responseGetModelPackageGroupPolicy :: GetModelPackageGroupPolicyResponse -> TestTree
responseGetModelPackageGroupPolicy =
  res
    "GetModelPackageGroupPolicyResponse"
    "fixture/GetModelPackageGroupPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetModelPackageGroupPolicy)

responseDescribeMonitoringSchedule :: DescribeMonitoringScheduleResponse -> TestTree
responseDescribeMonitoringSchedule =
  res
    "DescribeMonitoringScheduleResponse"
    "fixture/DescribeMonitoringScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMonitoringSchedule)

responseCreateModelExplainabilityJobDefinition :: CreateModelExplainabilityJobDefinitionResponse -> TestTree
responseCreateModelExplainabilityJobDefinition =
  res
    "CreateModelExplainabilityJobDefinitionResponse"
    "fixture/CreateModelExplainabilityJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateModelExplainabilityJobDefinition)

responseDescribeLabelingJob :: DescribeLabelingJobResponse -> TestTree
responseDescribeLabelingJob =
  res
    "DescribeLabelingJobResponse"
    "fixture/DescribeLabelingJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeLabelingJob)

responseCreateNotebookInstance :: CreateNotebookInstanceResponse -> TestTree
responseCreateNotebookInstance =
  res
    "CreateNotebookInstanceResponse"
    "fixture/CreateNotebookInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNotebookInstance)

responseUpdateModelPackage :: UpdateModelPackageResponse -> TestTree
responseUpdateModelPackage =
  res
    "UpdateModelPackageResponse"
    "fixture/UpdateModelPackageResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateModelPackage)

responseCreateModelQualityJobDefinition :: CreateModelQualityJobDefinitionResponse -> TestTree
responseCreateModelQualityJobDefinition =
  res
    "CreateModelQualityJobDefinitionResponse"
    "fixture/CreateModelQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateModelQualityJobDefinition)

responseDeleteModelPackage :: DeleteModelPackageResponse -> TestTree
responseDeleteModelPackage =
  res
    "DeleteModelPackageResponse"
    "fixture/DeleteModelPackageResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteModelPackage)

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    defaultService
    (Proxy :: Proxy ListProjects)

responseListSubscribedWorkteams :: ListSubscribedWorkteamsResponse -> TestTree
responseListSubscribedWorkteams =
  res
    "ListSubscribedWorkteamsResponse"
    "fixture/ListSubscribedWorkteamsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSubscribedWorkteams)

responseDeleteNotebookInstance :: DeleteNotebookInstanceResponse -> TestTree
responseDeleteNotebookInstance =
  res
    "DeleteNotebookInstanceResponse"
    "fixture/DeleteNotebookInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNotebookInstance)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProject)

responseDescribeProcessingJob :: DescribeProcessingJobResponse -> TestTree
responseDescribeProcessingJob =
  res
    "DescribeProcessingJobResponse"
    "fixture/DescribeProcessingJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProcessingJob)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDomains)

responseDeleteModelExplainabilityJobDefinition :: DeleteModelExplainabilityJobDefinitionResponse -> TestTree
responseDeleteModelExplainabilityJobDefinition =
  res
    "DeleteModelExplainabilityJobDefinitionResponse"
    "fixture/DeleteModelExplainabilityJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteModelExplainabilityJobDefinition)

responseStopMonitoringSchedule :: StopMonitoringScheduleResponse -> TestTree
responseStopMonitoringSchedule =
  res
    "StopMonitoringScheduleResponse"
    "fixture/StopMonitoringScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy StopMonitoringSchedule)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListDevices)

responseCreateModelPackage :: CreateModelPackageResponse -> TestTree
responseCreateModelPackage =
  res
    "CreateModelPackageResponse"
    "fixture/CreateModelPackageResponse.proto"
    defaultService
    (Proxy :: Proxy CreateModelPackage)

responseUpdateNotebookInstance :: UpdateNotebookInstanceResponse -> TestTree
responseUpdateNotebookInstance =
  res
    "UpdateNotebookInstanceResponse"
    "fixture/UpdateNotebookInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateNotebookInstance)

responseStopAutoMLJob :: StopAutoMLJobResponse -> TestTree
responseStopAutoMLJob =
  res
    "StopAutoMLJobResponse"
    "fixture/StopAutoMLJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopAutoMLJob)

responseDescribeAppImageConfig :: DescribeAppImageConfigResponse -> TestTree
responseDescribeAppImageConfig =
  res
    "DescribeAppImageConfigResponse"
    "fixture/DescribeAppImageConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAppImageConfig)

responseStartMonitoringSchedule :: StartMonitoringScheduleResponse -> TestTree
responseStartMonitoringSchedule =
  res
    "StartMonitoringScheduleResponse"
    "fixture/StartMonitoringScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy StartMonitoringSchedule)

responseStopCompilationJob :: StopCompilationJobResponse -> TestTree
responseStopCompilationJob =
  res
    "StopCompilationJobResponse"
    "fixture/StopCompilationJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopCompilationJob)

responseCreateTrial :: CreateTrialResponse -> TestTree
responseCreateTrial =
  res
    "CreateTrialResponse"
    "fixture/CreateTrialResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTrial)

responseGetSagemakerServicecatalogPortfolioStatus :: GetSagemakerServicecatalogPortfolioStatusResponse -> TestTree
responseGetSagemakerServicecatalogPortfolioStatus =
  res
    "GetSagemakerServicecatalogPortfolioStatusResponse"
    "fixture/GetSagemakerServicecatalogPortfolioStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetSagemakerServicecatalogPortfolioStatus)

responseUpdateCodeRepository :: UpdateCodeRepositoryResponse -> TestTree
responseUpdateCodeRepository =
  res
    "UpdateCodeRepositoryResponse"
    "fixture/UpdateCodeRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCodeRepository)

responseSearch :: SearchResponse -> TestTree
responseSearch =
  res
    "SearchResponse"
    "fixture/SearchResponse.proto"
    defaultService
    (Proxy :: Proxy Search)

responseDeleteModel :: DeleteModelResponse -> TestTree
responseDeleteModel =
  res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteModel)

responseDeleteDataQualityJobDefinition :: DeleteDataQualityJobDefinitionResponse -> TestTree
responseDeleteDataQualityJobDefinition =
  res
    "DeleteDataQualityJobDefinitionResponse"
    "fixture/DeleteDataQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDataQualityJobDefinition)

responseListImages :: ListImagesResponse -> TestTree
responseListImages =
  res
    "ListImagesResponse"
    "fixture/ListImagesResponse.proto"
    defaultService
    (Proxy :: Proxy ListImages)

responseListTrainingJobs :: ListTrainingJobsResponse -> TestTree
responseListTrainingJobs =
  res
    "ListTrainingJobsResponse"
    "fixture/ListTrainingJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTrainingJobs)

responseDescribeTransformJob :: DescribeTransformJobResponse -> TestTree
responseDescribeTransformJob =
  res
    "DescribeTransformJobResponse"
    "fixture/DescribeTransformJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTransformJob)

responseCreatePipeline :: CreatePipelineResponse -> TestTree
responseCreatePipeline =
  res
    "CreatePipelineResponse"
    "fixture/CreatePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePipeline)

responseCreateModelPackageGroup :: CreateModelPackageGroupResponse -> TestTree
responseCreateModelPackageGroup =
  res
    "CreateModelPackageGroupResponse"
    "fixture/CreateModelPackageGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateModelPackageGroup)

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

responseListDataQualityJobDefinitions :: ListDataQualityJobDefinitionsResponse -> TestTree
responseListDataQualityJobDefinitions =
  res
    "ListDataQualityJobDefinitionsResponse"
    "fixture/ListDataQualityJobDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDataQualityJobDefinitions)

responseDescribeEdgePackagingJob :: DescribeEdgePackagingJobResponse -> TestTree
responseDescribeEdgePackagingJob =
  res
    "DescribeEdgePackagingJobResponse"
    "fixture/DescribeEdgePackagingJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEdgePackagingJob)

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

responseDeleteCodeRepository :: DeleteCodeRepositoryResponse -> TestTree
responseDeleteCodeRepository =
  res
    "DeleteCodeRepositoryResponse"
    "fixture/DeleteCodeRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCodeRepository)

responseDeleteModelPackageGroupPolicy :: DeleteModelPackageGroupPolicyResponse -> TestTree
responseDeleteModelPackageGroupPolicy =
  res
    "DeleteModelPackageGroupPolicyResponse"
    "fixture/DeleteModelPackageGroupPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteModelPackageGroupPolicy)

responseListUserProfiles :: ListUserProfilesResponse -> TestTree
responseListUserProfiles =
  res
    "ListUserProfilesResponse"
    "fixture/ListUserProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy ListUserProfiles)

responseDescribeCompilationJob :: DescribeCompilationJobResponse -> TestTree
responseDescribeCompilationJob =
  res
    "DescribeCompilationJobResponse"
    "fixture/DescribeCompilationJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCompilationJob)

responseUpdatePipeline :: UpdatePipelineResponse -> TestTree
responseUpdatePipeline =
  res
    "UpdatePipelineResponse"
    "fixture/UpdatePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePipeline)

responseCreateCodeRepository :: CreateCodeRepositoryResponse -> TestTree
responseCreateCodeRepository =
  res
    "CreateCodeRepositoryResponse"
    "fixture/CreateCodeRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCodeRepository)

responseDescribeArtifact :: DescribeArtifactResponse -> TestTree
responseDescribeArtifact =
  res
    "DescribeArtifactResponse"
    "fixture/DescribeArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeArtifact)

responseDescribeHumanTaskUi :: DescribeHumanTaskUiResponse -> TestTree
responseDescribeHumanTaskUi =
  res
    "DescribeHumanTaskUiResponse"
    "fixture/DescribeHumanTaskUiResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHumanTaskUi)

responseListPipelineExecutionSteps :: ListPipelineExecutionStepsResponse -> TestTree
responseListPipelineExecutionSteps =
  res
    "ListPipelineExecutionStepsResponse"
    "fixture/ListPipelineExecutionStepsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPipelineExecutionSteps)

responseListCodeRepositories :: ListCodeRepositoriesResponse -> TestTree
responseListCodeRepositories =
  res
    "ListCodeRepositoriesResponse"
    "fixture/ListCodeRepositoriesResponse.proto"
    defaultService
    (Proxy :: Proxy ListCodeRepositories)

responseUpdateUserProfile :: UpdateUserProfileResponse -> TestTree
responseUpdateUserProfile =
  res
    "UpdateUserProfileResponse"
    "fixture/UpdateUserProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserProfile)

responseDescribeAction :: DescribeActionResponse -> TestTree
responseDescribeAction =
  res
    "DescribeActionResponse"
    "fixture/DescribeActionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAction)

responseStopTransformJob :: StopTransformJobResponse -> TestTree
responseStopTransformJob =
  res
    "StopTransformJobResponse"
    "fixture/StopTransformJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopTransformJob)

responseCreateTrainingJob :: CreateTrainingJobResponse -> TestTree
responseCreateTrainingJob =
  res
    "CreateTrainingJobResponse"
    "fixture/CreateTrainingJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTrainingJob)

responseDeleteUserProfile :: DeleteUserProfileResponse -> TestTree
responseDeleteUserProfile =
  res
    "DeleteUserProfileResponse"
    "fixture/DeleteUserProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserProfile)

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

responseCreateImage :: CreateImageResponse -> TestTree
responseCreateImage =
  res
    "CreateImageResponse"
    "fixture/CreateImageResponse.proto"
    defaultService
    (Proxy :: Proxy CreateImage)

responseDeregisterDevices :: DeregisterDevicesResponse -> TestTree
responseDeregisterDevices =
  res
    "DeregisterDevicesResponse"
    "fixture/DeregisterDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterDevices)

responseCreateDataQualityJobDefinition :: CreateDataQualityJobDefinitionResponse -> TestTree
responseCreateDataQualityJobDefinition =
  res
    "CreateDataQualityJobDefinitionResponse"
    "fixture/CreateDataQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDataQualityJobDefinition)

responseDeletePipeline :: DeletePipelineResponse -> TestTree
responseDeletePipeline =
  res
    "DeletePipelineResponse"
    "fixture/DeletePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePipeline)

responseCreateAppImageConfig :: CreateAppImageConfigResponse -> TestTree
responseCreateAppImageConfig =
  res
    "CreateAppImageConfigResponse"
    "fixture/CreateAppImageConfigResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAppImageConfig)

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

responseDeleteAssociation :: DeleteAssociationResponse -> TestTree
responseDeleteAssociation =
  res
    "DeleteAssociationResponse"
    "fixture/DeleteAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAssociation)

responseUpdateMonitoringSchedule :: UpdateMonitoringScheduleResponse -> TestTree
responseUpdateMonitoringSchedule =
  res
    "UpdateMonitoringScheduleResponse"
    "fixture/UpdateMonitoringScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMonitoringSchedule)

responseListMonitoringSchedules :: ListMonitoringSchedulesResponse -> TestTree
responseListMonitoringSchedules =
  res
    "ListMonitoringSchedulesResponse"
    "fixture/ListMonitoringSchedulesResponse.proto"
    defaultService
    (Proxy :: Proxy ListMonitoringSchedules)

responseStopNotebookInstance :: StopNotebookInstanceResponse -> TestTree
responseStopNotebookInstance =
  res
    "StopNotebookInstanceResponse"
    "fixture/StopNotebookInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy StopNotebookInstance)

responseDeleteMonitoringSchedule :: DeleteMonitoringScheduleResponse -> TestTree
responseDeleteMonitoringSchedule =
  res
    "DeleteMonitoringScheduleResponse"
    "fixture/DeleteMonitoringScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMonitoringSchedule)

responseDeleteEndpointConfig :: DeleteEndpointConfigResponse -> TestTree
responseDeleteEndpointConfig =
  res
    "DeleteEndpointConfigResponse"
    "fixture/DeleteEndpointConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEndpointConfig)

responseStartPipelineExecution :: StartPipelineExecutionResponse -> TestTree
responseStartPipelineExecution =
  res
    "StartPipelineExecutionResponse"
    "fixture/StartPipelineExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StartPipelineExecution)

responseDescribeModelPackage :: DescribeModelPackageResponse -> TestTree
responseDescribeModelPackage =
  res
    "DescribeModelPackageResponse"
    "fixture/DescribeModelPackageResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeModelPackage)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags =
  res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTags)

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

responseStopPipelineExecution :: StopPipelineExecutionResponse -> TestTree
responseStopPipelineExecution =
  res
    "StopPipelineExecutionResponse"
    "fixture/StopPipelineExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StopPipelineExecution)

responseListEndpointConfigs :: ListEndpointConfigsResponse -> TestTree
responseListEndpointConfigs =
  res
    "ListEndpointConfigsResponse"
    "fixture/ListEndpointConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListEndpointConfigs)

responseDeleteWorkteam :: DeleteWorkteamResponse -> TestTree
responseDeleteWorkteam =
  res
    "DeleteWorkteamResponse"
    "fixture/DeleteWorkteamResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWorkteam)

responseDeleteWorkforce :: DeleteWorkforceResponse -> TestTree
responseDeleteWorkforce =
  res
    "DeleteWorkforceResponse"
    "fixture/DeleteWorkforceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWorkforce)

responseDeleteModelBiasJobDefinition :: DeleteModelBiasJobDefinitionResponse -> TestTree
responseDeleteModelBiasJobDefinition =
  res
    "DeleteModelBiasJobDefinitionResponse"
    "fixture/DeleteModelBiasJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteModelBiasJobDefinition)

responseUpdateWorkforce :: UpdateWorkforceResponse -> TestTree
responseUpdateWorkforce =
  res
    "UpdateWorkforceResponse"
    "fixture/UpdateWorkforceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateWorkforce)

responseDescribeDevice :: DescribeDeviceResponse -> TestTree
responseDescribeDevice =
  res
    "DescribeDeviceResponse"
    "fixture/DescribeDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDevice)

responseDescribeDomain :: DescribeDomainResponse -> TestTree
responseDescribeDomain =
  res
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDomain)

responseDeleteNotebookInstanceLifecycleConfig :: DeleteNotebookInstanceLifecycleConfigResponse -> TestTree
responseDeleteNotebookInstanceLifecycleConfig =
  res
    "DeleteNotebookInstanceLifecycleConfigResponse"
    "fixture/DeleteNotebookInstanceLifecycleConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNotebookInstanceLifecycleConfig)

responseDescribePipelineExecution :: DescribePipelineExecutionResponse -> TestTree
responseDescribePipelineExecution =
  res
    "DescribePipelineExecutionResponse"
    "fixture/DescribePipelineExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePipelineExecution)

responseUpdateWorkteam :: UpdateWorkteamResponse -> TestTree
responseUpdateWorkteam =
  res
    "UpdateWorkteamResponse"
    "fixture/UpdateWorkteamResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateWorkteam)

responseCreateLabelingJob :: CreateLabelingJobResponse -> TestTree
responseCreateLabelingJob =
  res
    "CreateLabelingJobResponse"
    "fixture/CreateLabelingJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLabelingJob)

responseDescribeModelQualityJobDefinition :: DescribeModelQualityJobDefinitionResponse -> TestTree
responseDescribeModelQualityJobDefinition =
  res
    "DescribeModelQualityJobDefinitionResponse"
    "fixture/DescribeModelQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeModelQualityJobDefinition)

responseCreateExperiment :: CreateExperimentResponse -> TestTree
responseCreateExperiment =
  res
    "CreateExperimentResponse"
    "fixture/CreateExperimentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateExperiment)

responseListWorkforces :: ListWorkforcesResponse -> TestTree
responseListWorkforces =
  res
    "ListWorkforcesResponse"
    "fixture/ListWorkforcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListWorkforces)

responseListAppImageConfigs :: ListAppImageConfigsResponse -> TestTree
responseListAppImageConfigs =
  res
    "ListAppImageConfigsResponse"
    "fixture/ListAppImageConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAppImageConfigs)

responseUpdateNotebookInstanceLifecycleConfig :: UpdateNotebookInstanceLifecycleConfigResponse -> TestTree
responseUpdateNotebookInstanceLifecycleConfig =
  res
    "UpdateNotebookInstanceLifecycleConfigResponse"
    "fixture/UpdateNotebookInstanceLifecycleConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateNotebookInstanceLifecycleConfig)

responseDescribeSubscribedWorkteam :: DescribeSubscribedWorkteamResponse -> TestTree
responseDescribeSubscribedWorkteam =
  res
    "DescribeSubscribedWorkteamResponse"
    "fixture/DescribeSubscribedWorkteamResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSubscribedWorkteam)

responseListNotebookInstanceLifecycleConfigs :: ListNotebookInstanceLifecycleConfigsResponse -> TestTree
responseListNotebookInstanceLifecycleConfigs =
  res
    "ListNotebookInstanceLifecycleConfigsResponse"
    "fixture/ListNotebookInstanceLifecycleConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListNotebookInstanceLifecycleConfigs)

responseListEdgePackagingJobs :: ListEdgePackagingJobsResponse -> TestTree
responseListEdgePackagingJobs =
  res
    "ListEdgePackagingJobsResponse"
    "fixture/ListEdgePackagingJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListEdgePackagingJobs)

responseDescribeCodeRepository :: DescribeCodeRepositoryResponse -> TestTree
responseDescribeCodeRepository =
  res
    "DescribeCodeRepositoryResponse"
    "fixture/DescribeCodeRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCodeRepository)

responseListEndpoints :: ListEndpointsResponse -> TestTree
responseListEndpoints =
  res
    "ListEndpointsResponse"
    "fixture/ListEndpointsResponse.proto"
    defaultService
    (Proxy :: Proxy ListEndpoints)

responseDescribeDataQualityJobDefinition :: DescribeDataQualityJobDefinitionResponse -> TestTree
responseDescribeDataQualityJobDefinition =
  res
    "DescribeDataQualityJobDefinitionResponse"
    "fixture/DescribeDataQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDataQualityJobDefinition)

responseDescribeAlgorithm :: DescribeAlgorithmResponse -> TestTree
responseDescribeAlgorithm =
  res
    "DescribeAlgorithmResponse"
    "fixture/DescribeAlgorithmResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAlgorithm)

responseCreateAction :: CreateActionResponse -> TestTree
responseCreateAction =
  res
    "CreateActionResponse"
    "fixture/CreateActionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAction)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEndpoint)

responseCreatePresignedDomainUrl :: CreatePresignedDomainUrlResponse -> TestTree
responseCreatePresignedDomainUrl =
  res
    "CreatePresignedDomainUrlResponse"
    "fixture/CreatePresignedDomainUrlResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePresignedDomainUrl)

responseListTransformJobs :: ListTransformJobsResponse -> TestTree
responseListTransformJobs =
  res
    "ListTransformJobsResponse"
    "fixture/ListTransformJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTransformJobs)

responseDescribeHyperParameterTuningJob :: DescribeHyperParameterTuningJobResponse -> TestTree
responseDescribeHyperParameterTuningJob =
  res
    "DescribeHyperParameterTuningJobResponse"
    "fixture/DescribeHyperParameterTuningJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHyperParameterTuningJob)

responseCreateCompilationJob :: CreateCompilationJobResponse -> TestTree
responseCreateCompilationJob =
  res
    "CreateCompilationJobResponse"
    "fixture/CreateCompilationJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCompilationJob)

responseUpdateEndpoint :: UpdateEndpointResponse -> TestTree
responseUpdateEndpoint =
  res
    "UpdateEndpointResponse"
    "fixture/UpdateEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEndpoint)

responseDescribeModel :: DescribeModelResponse -> TestTree
responseDescribeModel =
  res
    "DescribeModelResponse"
    "fixture/DescribeModelResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeModel)

responseCreateDeviceFleet :: CreateDeviceFleetResponse -> TestTree
responseCreateDeviceFleet =
  res
    "CreateDeviceFleetResponse"
    "fixture/CreateDeviceFleetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDeviceFleet)

responseCreateArtifact :: CreateArtifactResponse -> TestTree
responseCreateArtifact =
  res
    "CreateArtifactResponse"
    "fixture/CreateArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy CreateArtifact)

responseUpdateDevices :: UpdateDevicesResponse -> TestTree
responseUpdateDevices =
  res
    "UpdateDevicesResponse"
    "fixture/UpdateDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDevices)

responseListArtifacts :: ListArtifactsResponse -> TestTree
responseListArtifacts =
  res
    "ListArtifactsResponse"
    "fixture/ListArtifactsResponse.proto"
    defaultService
    (Proxy :: Proxy ListArtifacts)

responseDeleteDeviceFleet :: DeleteDeviceFleetResponse -> TestTree
responseDeleteDeviceFleet =
  res
    "DeleteDeviceFleetResponse"
    "fixture/DeleteDeviceFleetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDeviceFleet)

responseListMonitoringExecutions :: ListMonitoringExecutionsResponse -> TestTree
responseListMonitoringExecutions =
  res
    "ListMonitoringExecutionsResponse"
    "fixture/ListMonitoringExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMonitoringExecutions)

responseListCompilationJobs :: ListCompilationJobsResponse -> TestTree
responseListCompilationJobs =
  res
    "ListCompilationJobsResponse"
    "fixture/ListCompilationJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListCompilationJobs)

responseListActions :: ListActionsResponse -> TestTree
responseListActions =
  res
    "ListActionsResponse"
    "fixture/ListActionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListActions)

responseListDeviceFleets :: ListDeviceFleetsResponse -> TestTree
responseListDeviceFleets =
  res
    "ListDeviceFleetsResponse"
    "fixture/ListDeviceFleetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeviceFleets)

responseDescribeModelPackageGroup :: DescribeModelPackageGroupResponse -> TestTree
responseDescribeModelPackageGroup =
  res
    "DescribeModelPackageGroupResponse"
    "fixture/DescribeModelPackageGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeModelPackageGroup)

responseStopHyperParameterTuningJob :: StopHyperParameterTuningJobResponse -> TestTree
responseStopHyperParameterTuningJob =
  res
    "StopHyperParameterTuningJobResponse"
    "fixture/StopHyperParameterTuningJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopHyperParameterTuningJob)

responseDescribeTrial :: DescribeTrialResponse -> TestTree
responseDescribeTrial =
  res
    "DescribeTrialResponse"
    "fixture/DescribeTrialResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTrial)

responseUpdateDeviceFleet :: UpdateDeviceFleetResponse -> TestTree
responseUpdateDeviceFleet =
  res
    "UpdateDeviceFleetResponse"
    "fixture/UpdateDeviceFleetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDeviceFleet)

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

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDomain)

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

responseDeleteImageVersion :: DeleteImageVersionResponse -> TestTree
responseDeleteImageVersion =
  res
    "DeleteImageVersionResponse"
    "fixture/DeleteImageVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteImageVersion)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProject)

responseDescribeExperiment :: DescribeExperimentResponse -> TestTree
responseDescribeExperiment =
  res
    "DescribeExperimentResponse"
    "fixture/DescribeExperimentResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeExperiment)

responseDescribeAutoMLJob :: DescribeAutoMLJobResponse -> TestTree
responseDescribeAutoMLJob =
  res
    "DescribeAutoMLJobResponse"
    "fixture/DescribeAutoMLJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAutoMLJob)

responseDescribeApp :: DescribeAppResponse -> TestTree
responseDescribeApp =
  res
    "DescribeAppResponse"
    "fixture/DescribeAppResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeApp)

responseListTrialComponents :: ListTrialComponentsResponse -> TestTree
responseListTrialComponents =
  res
    "ListTrialComponentsResponse"
    "fixture/ListTrialComponentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTrialComponents)

responseUpdateTrialComponent :: UpdateTrialComponentResponse -> TestTree
responseUpdateTrialComponent =
  res
    "UpdateTrialComponentResponse"
    "fixture/UpdateTrialComponentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTrialComponent)

responseDeleteTrialComponent :: DeleteTrialComponentResponse -> TestTree
responseDeleteTrialComponent =
  res
    "DeleteTrialComponentResponse"
    "fixture/DeleteTrialComponentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTrialComponent)

responseCreateTrialComponent :: CreateTrialComponentResponse -> TestTree
responseCreateTrialComponent =
  res
    "CreateTrialComponentResponse"
    "fixture/CreateTrialComponentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTrialComponent)

responseDescribeWorkforce :: DescribeWorkforceResponse -> TestTree
responseDescribeWorkforce =
  res
    "DescribeWorkforceResponse"
    "fixture/DescribeWorkforceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkforce)

responseListNotebookInstances :: ListNotebookInstancesResponse -> TestTree
responseListNotebookInstances =
  res
    "ListNotebookInstancesResponse"
    "fixture/ListNotebookInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy ListNotebookInstances)

responseListModelExplainabilityJobDefinitions :: ListModelExplainabilityJobDefinitionsResponse -> TestTree
responseListModelExplainabilityJobDefinitions =
  res
    "ListModelExplainabilityJobDefinitionsResponse"
    "fixture/ListModelExplainabilityJobDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListModelExplainabilityJobDefinitions)

responseDeleteModelQualityJobDefinition :: DeleteModelQualityJobDefinitionResponse -> TestTree
responseDeleteModelQualityJobDefinition =
  res
    "DeleteModelQualityJobDefinitionResponse"
    "fixture/DeleteModelQualityJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteModelQualityJobDefinition)

responseStopLabelingJob :: StopLabelingJobResponse -> TestTree
responseStopLabelingJob =
  res
    "StopLabelingJobResponse"
    "fixture/StopLabelingJobResponse.proto"
    defaultService
    (Proxy :: Proxy StopLabelingJob)

responseListModelQualityJobDefinitions :: ListModelQualityJobDefinitionsResponse -> TestTree
responseListModelQualityJobDefinitions =
  res
    "ListModelQualityJobDefinitionsResponse"
    "fixture/ListModelQualityJobDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListModelQualityJobDefinitions)

responseDescribeModelBiasJobDefinition :: DescribeModelBiasJobDefinitionResponse -> TestTree
responseDescribeModelBiasJobDefinition =
  res
    "DescribeModelBiasJobDefinitionResponse"
    "fixture/DescribeModelBiasJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeModelBiasJobDefinition)

responseDescribeWorkteam :: DescribeWorkteamResponse -> TestTree
responseDescribeWorkteam =
  res
    "DescribeWorkteamResponse"
    "fixture/DescribeWorkteamResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeWorkteam)

responseDescribeNotebookInstanceLifecycleConfig :: DescribeNotebookInstanceLifecycleConfigResponse -> TestTree
responseDescribeNotebookInstanceLifecycleConfig =
  res
    "DescribeNotebookInstanceLifecycleConfigResponse"
    "fixture/DescribeNotebookInstanceLifecycleConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNotebookInstanceLifecycleConfig)

responseListPipelineExecutions :: ListPipelineExecutionsResponse -> TestTree
responseListPipelineExecutions =
  res
    "ListPipelineExecutionsResponse"
    "fixture/ListPipelineExecutionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPipelineExecutions)

responseUpdateDomain :: UpdateDomainResponse -> TestTree
responseUpdateDomain =
  res
    "UpdateDomainResponse"
    "fixture/UpdateDomainResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDomain)

responseAssociateTrialComponent :: AssociateTrialComponentResponse -> TestTree
responseAssociateTrialComponent =
  res
    "AssociateTrialComponentResponse"
    "fixture/AssociateTrialComponentResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateTrialComponent)

responseUpdatePipelineExecution :: UpdatePipelineExecutionResponse -> TestTree
responseUpdatePipelineExecution =
  res
    "UpdatePipelineExecutionResponse"
    "fixture/UpdatePipelineExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePipelineExecution)

responseCreateImageVersion :: CreateImageVersionResponse -> TestTree
responseCreateImageVersion =
  res
    "CreateImageVersionResponse"
    "fixture/CreateImageVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateImageVersion)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDomain)

responseUpdateTrainingJob :: UpdateTrainingJobResponse -> TestTree
responseUpdateTrainingJob =
  res
    "UpdateTrainingJobResponse"
    "fixture/UpdateTrainingJobResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTrainingJob)

responseUpdateImage :: UpdateImageResponse -> TestTree
responseUpdateImage =
  res
    "UpdateImageResponse"
    "fixture/UpdateImageResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateImage)

responseUpdateContext :: UpdateContextResponse -> TestTree
responseUpdateContext =
  res
    "UpdateContextResponse"
    "fixture/UpdateContextResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateContext)

responseDeleteImage :: DeleteImageResponse -> TestTree
responseDeleteImage =
  res
    "DeleteImageResponse"
    "fixture/DeleteImageResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteImage)

responseListFlowDefinitions :: ListFlowDefinitionsResponse -> TestTree
responseListFlowDefinitions =
  res
    "ListFlowDefinitionsResponse"
    "fixture/ListFlowDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListFlowDefinitions)

responseListModels :: ListModelsResponse -> TestTree
responseListModels =
  res
    "ListModelsResponse"
    "fixture/ListModelsResponse.proto"
    defaultService
    (Proxy :: Proxy ListModels)

responseCreateUserProfile :: CreateUserProfileResponse -> TestTree
responseCreateUserProfile =
  res
    "CreateUserProfileResponse"
    "fixture/CreateUserProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUserProfile)

responseRenderUiTemplate :: RenderUiTemplateResponse -> TestTree
responseRenderUiTemplate =
  res
    "RenderUiTemplateResponse"
    "fixture/RenderUiTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy RenderUiTemplate)

responseDescribeFeatureGroup :: DescribeFeatureGroupResponse -> TestTree
responseDescribeFeatureGroup =
  res
    "DescribeFeatureGroupResponse"
    "fixture/DescribeFeatureGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFeatureGroup)

responseDeleteContext :: DeleteContextResponse -> TestTree
responseDeleteContext =
  res
    "DeleteContextResponse"
    "fixture/DeleteContextResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteContext)

responseListHyperParameterTuningJobs :: ListHyperParameterTuningJobsResponse -> TestTree
responseListHyperParameterTuningJobs =
  res
    "ListHyperParameterTuningJobsResponse"
    "fixture/ListHyperParameterTuningJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListHyperParameterTuningJobs)

responseDeleteFlowDefinition :: DeleteFlowDefinitionResponse -> TestTree
responseDeleteFlowDefinition =
  res
    "DeleteFlowDefinitionResponse"
    "fixture/DeleteFlowDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFlowDefinition)

responseListAlgorithms :: ListAlgorithmsResponse -> TestTree
responseListAlgorithms =
  res
    "ListAlgorithmsResponse"
    "fixture/ListAlgorithmsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAlgorithms)

responseCreateAlgorithm :: CreateAlgorithmResponse -> TestTree
responseCreateAlgorithm =
  res
    "CreateAlgorithmResponse"
    "fixture/CreateAlgorithmResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAlgorithm)

responseCreateFlowDefinition :: CreateFlowDefinitionResponse -> TestTree
responseCreateFlowDefinition =
  res
    "CreateFlowDefinitionResponse"
    "fixture/CreateFlowDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFlowDefinition)

responseListPipelineParametersForExecution :: ListPipelineParametersForExecutionResponse -> TestTree
responseListPipelineParametersForExecution =
  res
    "ListPipelineParametersForExecutionResponse"
    "fixture/ListPipelineParametersForExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy ListPipelineParametersForExecution)

responseListTrials :: ListTrialsResponse -> TestTree
responseListTrials =
  res
    "ListTrialsResponse"
    "fixture/ListTrialsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTrials)

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

responseListTags :: ListTagsResponse -> TestTree
responseListTags =
  res
    "ListTagsResponse"
    "fixture/ListTagsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTags)

responseDescribePipelineDefinitionForExecution :: DescribePipelineDefinitionForExecutionResponse -> TestTree
responseDescribePipelineDefinitionForExecution =
  res
    "DescribePipelineDefinitionForExecutionResponse"
    "fixture/DescribePipelineDefinitionForExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePipelineDefinitionForExecution)

responseDeleteTrial :: DeleteTrialResponse -> TestTree
responseDeleteTrial =
  res
    "DeleteTrialResponse"
    "fixture/DeleteTrialResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTrial)

responsePutModelPackageGroupPolicy :: PutModelPackageGroupPolicyResponse -> TestTree
responsePutModelPackageGroupPolicy =
  res
    "PutModelPackageGroupPolicyResponse"
    "fixture/PutModelPackageGroupPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutModelPackageGroupPolicy)

responseListExperiments :: ListExperimentsResponse -> TestTree
responseListExperiments =
  res
    "ListExperimentsResponse"
    "fixture/ListExperimentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListExperiments)

responseUpdateExperiment :: UpdateExperimentResponse -> TestTree
responseUpdateExperiment =
  res
    "UpdateExperimentResponse"
    "fixture/UpdateExperimentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateExperiment)

responseDeleteExperiment :: DeleteExperimentResponse -> TestTree
responseDeleteExperiment =
  res
    "DeleteExperimentResponse"
    "fixture/DeleteExperimentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteExperiment)

responseListLabelingJobs :: ListLabelingJobsResponse -> TestTree
responseListLabelingJobs =
  res
    "ListLabelingJobsResponse"
    "fixture/ListLabelingJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListLabelingJobs)

responseDescribeImageVersion :: DescribeImageVersionResponse -> TestTree
responseDescribeImageVersion =
  res
    "DescribeImageVersionResponse"
    "fixture/DescribeImageVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImageVersion)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApp)

responseCreateModelBiasJobDefinition :: CreateModelBiasJobDefinitionResponse -> TestTree
responseCreateModelBiasJobDefinition =
  res
    "CreateModelBiasJobDefinitionResponse"
    "fixture/CreateModelBiasJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateModelBiasJobDefinition)

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

responseDescribeProject :: DescribeProjectResponse -> TestTree
responseDescribeProject =
  res
    "DescribeProjectResponse"
    "fixture/DescribeProjectResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProject)

responseCreateProcessingJob :: CreateProcessingJobResponse -> TestTree
responseCreateProcessingJob =
  res
    "CreateProcessingJobResponse"
    "fixture/CreateProcessingJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProcessingJob)

responseListAssociations :: ListAssociationsResponse -> TestTree
responseListAssociations =
  res
    "ListAssociationsResponse"
    "fixture/ListAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssociations)

responseEnableSagemakerServicecatalogPortfolio :: EnableSagemakerServicecatalogPortfolioResponse -> TestTree
responseEnableSagemakerServicecatalogPortfolio =
  res
    "EnableSagemakerServicecatalogPortfolioResponse"
    "fixture/EnableSagemakerServicecatalogPortfolioResponse.proto"
    defaultService
    (Proxy :: Proxy EnableSagemakerServicecatalogPortfolio)

responseUpdateAppImageConfig :: UpdateAppImageConfigResponse -> TestTree
responseUpdateAppImageConfig =
  res
    "UpdateAppImageConfigResponse"
    "fixture/UpdateAppImageConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateAppImageConfig)

responseListModelBiasJobDefinitions :: ListModelBiasJobDefinitionsResponse -> TestTree
responseListModelBiasJobDefinitions =
  res
    "ListModelBiasJobDefinitionsResponse"
    "fixture/ListModelBiasJobDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListModelBiasJobDefinitions)

responseCreateAutoMLJob :: CreateAutoMLJobResponse -> TestTree
responseCreateAutoMLJob =
  res
    "CreateAutoMLJobResponse"
    "fixture/CreateAutoMLJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAutoMLJob)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApp)

responseDescribeNotebookInstance :: DescribeNotebookInstanceResponse -> TestTree
responseDescribeNotebookInstance =
  res
    "DescribeNotebookInstanceResponse"
    "fixture/DescribeNotebookInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNotebookInstance)

responseDeleteAppImageConfig :: DeleteAppImageConfigResponse -> TestTree
responseDeleteAppImageConfig =
  res
    "DeleteAppImageConfigResponse"
    "fixture/DeleteAppImageConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAppImageConfig)

responseCreateEndpointConfig :: CreateEndpointConfigResponse -> TestTree
responseCreateEndpointConfig =
  res
    "CreateEndpointConfigResponse"
    "fixture/CreateEndpointConfigResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEndpointConfig)

responseListProcessingJobs :: ListProcessingJobsResponse -> TestTree
responseListProcessingJobs =
  res
    "ListProcessingJobsResponse"
    "fixture/ListProcessingJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListProcessingJobs)

responseCreateMonitoringSchedule :: CreateMonitoringScheduleResponse -> TestTree
responseCreateMonitoringSchedule =
  res
    "CreateMonitoringScheduleResponse"
    "fixture/CreateMonitoringScheduleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMonitoringSchedule)

responseDescribeModelExplainabilityJobDefinition :: DescribeModelExplainabilityJobDefinitionResponse -> TestTree
responseDescribeModelExplainabilityJobDefinition =
  res
    "DescribeModelExplainabilityJobDefinitionResponse"
    "fixture/DescribeModelExplainabilityJobDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeModelExplainabilityJobDefinition)

responseListWorkteams :: ListWorkteamsResponse -> TestTree
responseListWorkteams =
  res
    "ListWorkteamsResponse"
    "fixture/ListWorkteamsResponse.proto"
    defaultService
    (Proxy :: Proxy ListWorkteams)

responseDescribeFlowDefinition :: DescribeFlowDefinitionResponse -> TestTree
responseDescribeFlowDefinition =
  res
    "DescribeFlowDefinitionResponse"
    "fixture/DescribeFlowDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFlowDefinition)

responseDescribeContext :: DescribeContextResponse -> TestTree
responseDescribeContext =
  res
    "DescribeContextResponse"
    "fixture/DescribeContextResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeContext)

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

responseCreatePresignedNotebookInstanceUrl :: CreatePresignedNotebookInstanceUrlResponse -> TestTree
responseCreatePresignedNotebookInstanceUrl =
  res
    "CreatePresignedNotebookInstanceUrlResponse"
    "fixture/CreatePresignedNotebookInstanceUrlResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePresignedNotebookInstanceUrl)

responseDescribeTrainingJob :: DescribeTrainingJobResponse -> TestTree
responseDescribeTrainingJob =
  res
    "DescribeTrainingJobResponse"
    "fixture/DescribeTrainingJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeTrainingJob)

responseCreateHumanTaskUi :: CreateHumanTaskUiResponse -> TestTree
responseCreateHumanTaskUi =
  res
    "CreateHumanTaskUiResponse"
    "fixture/CreateHumanTaskUiResponse.proto"
    defaultService
    (Proxy :: Proxy CreateHumanTaskUi)

responseListTrainingJobsForHyperParameterTuningJob :: ListTrainingJobsForHyperParameterTuningJobResponse -> TestTree
responseListTrainingJobsForHyperParameterTuningJob =
  res
    "ListTrainingJobsForHyperParameterTuningJobResponse"
    "fixture/ListTrainingJobsForHyperParameterTuningJobResponse.proto"
    defaultService
    (Proxy :: Proxy ListTrainingJobsForHyperParameterTuningJob)

responseDescribeImage :: DescribeImageResponse -> TestTree
responseDescribeImage =
  res
    "DescribeImageResponse"
    "fixture/DescribeImageResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImage)

responseDeleteFeatureGroup :: DeleteFeatureGroupResponse -> TestTree
responseDeleteFeatureGroup =
  res
    "DeleteFeatureGroupResponse"
    "fixture/DeleteFeatureGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFeatureGroup)

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.SageMaker
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-07-24@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Provides APIs for creating and managing Amazon SageMaker resources.
--
-- Other Resources:
--
-- -   <https://docs.aws.amazon.com/sagemaker/latest/dg/whatis.html#first-time-user Amazon SageMaker Developer Guide>
--
-- -   <https://docs.aws.amazon.com/augmented-ai/2019-11-07/APIReference/Welcome.html Amazon Augmented AI Runtime API Reference>
module Network.AWS.SageMaker
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ResourceInUse
    _ResourceInUse,

    -- ** ResourceLimitExceeded
    _ResourceLimitExceeded,

    -- ** ConflictException
    _ConflictException,

    -- ** ResourceNotFound
    _ResourceNotFound,

    -- * Waiters
    -- $waiters

    -- ** NotebookInstanceStopped
    newNotebookInstanceStopped,

    -- ** EndpointDeleted
    newEndpointDeleted,

    -- ** NotebookInstanceDeleted
    newNotebookInstanceDeleted,

    -- ** NotebookInstanceInService
    newNotebookInstanceInService,

    -- ** ImageUpdated
    newImageUpdated,

    -- ** ImageDeleted
    newImageDeleted,

    -- ** ImageCreated
    newImageCreated,

    -- ** EndpointInService
    newEndpointInService,

    -- ** TrainingJobCompletedOrStopped
    newTrainingJobCompletedOrStopped,

    -- ** ImageVersionDeleted
    newImageVersionDeleted,

    -- ** ImageVersionCreated
    newImageVersionCreated,

    -- ** ProcessingJobCompletedOrStopped
    newProcessingJobCompletedOrStopped,

    -- ** TransformJobCompletedOrStopped
    newTransformJobCompletedOrStopped,

    -- * Operations
    -- $operations

    -- ** CreateEdgePackagingJob
    CreateEdgePackagingJob (CreateEdgePackagingJob'),
    newCreateEdgePackagingJob,
    CreateEdgePackagingJobResponse (CreateEdgePackagingJobResponse'),
    newCreateEdgePackagingJobResponse,

    -- ** DescribeUserProfile
    DescribeUserProfile (DescribeUserProfile'),
    newDescribeUserProfile,
    DescribeUserProfileResponse (DescribeUserProfileResponse'),
    newDescribeUserProfileResponse,

    -- ** ListHumanTaskUis (Paginated)
    ListHumanTaskUis (ListHumanTaskUis'),
    newListHumanTaskUis,
    ListHumanTaskUisResponse (ListHumanTaskUisResponse'),
    newListHumanTaskUisResponse,

    -- ** DeleteHumanTaskUi
    DeleteHumanTaskUi (DeleteHumanTaskUi'),
    newDeleteHumanTaskUi,
    DeleteHumanTaskUiResponse (DeleteHumanTaskUiResponse'),
    newDeleteHumanTaskUiResponse,

    -- ** UpdateAction
    UpdateAction (UpdateAction'),
    newUpdateAction,
    UpdateActionResponse (UpdateActionResponse'),
    newUpdateActionResponse,

    -- ** CreateEndpoint
    CreateEndpoint (CreateEndpoint'),
    newCreateEndpoint,
    CreateEndpointResponse (CreateEndpointResponse'),
    newCreateEndpointResponse,

    -- ** DeleteArtifact
    DeleteArtifact (DeleteArtifact'),
    newDeleteArtifact,
    DeleteArtifactResponse (DeleteArtifactResponse'),
    newDeleteArtifactResponse,

    -- ** CreateTransformJob
    CreateTransformJob (CreateTransformJob'),
    newCreateTransformJob,
    CreateTransformJobResponse (CreateTransformJobResponse'),
    newCreateTransformJobResponse,

    -- ** DeleteAction
    DeleteAction (DeleteAction'),
    newDeleteAction,
    DeleteActionResponse (DeleteActionResponse'),
    newDeleteActionResponse,

    -- ** DescribePipeline
    DescribePipeline (DescribePipeline'),
    newDescribePipeline,
    DescribePipelineResponse (DescribePipelineResponse'),
    newDescribePipelineResponse,

    -- ** UpdateArtifact
    UpdateArtifact (UpdateArtifact'),
    newUpdateArtifact,
    UpdateArtifactResponse (UpdateArtifactResponse'),
    newUpdateArtifactResponse,

    -- ** StopTrainingJob
    StopTrainingJob (StopTrainingJob'),
    newStopTrainingJob,
    StopTrainingJobResponse (StopTrainingJobResponse'),
    newStopTrainingJobResponse,

    -- ** DisassociateTrialComponent
    DisassociateTrialComponent (DisassociateTrialComponent'),
    newDisassociateTrialComponent,
    DisassociateTrialComponentResponse (DisassociateTrialComponentResponse'),
    newDisassociateTrialComponentResponse,

    -- ** GetSearchSuggestions
    GetSearchSuggestions (GetSearchSuggestions'),
    newGetSearchSuggestions,
    GetSearchSuggestionsResponse (GetSearchSuggestionsResponse'),
    newGetSearchSuggestionsResponse,

    -- ** DeleteModelPackage
    DeleteModelPackage (DeleteModelPackage'),
    newDeleteModelPackage,
    DeleteModelPackageResponse (DeleteModelPackageResponse'),
    newDeleteModelPackageResponse,

    -- ** CreateModelQualityJobDefinition
    CreateModelQualityJobDefinition (CreateModelQualityJobDefinition'),
    newCreateModelQualityJobDefinition,
    CreateModelQualityJobDefinitionResponse (CreateModelQualityJobDefinitionResponse'),
    newCreateModelQualityJobDefinitionResponse,

    -- ** ListModelPackages (Paginated)
    ListModelPackages (ListModelPackages'),
    newListModelPackages,
    ListModelPackagesResponse (ListModelPackagesResponse'),
    newListModelPackagesResponse,

    -- ** ListProjects
    ListProjects (ListProjects'),
    newListProjects,
    ListProjectsResponse (ListProjectsResponse'),
    newListProjectsResponse,

    -- ** CreateModelExplainabilityJobDefinition
    CreateModelExplainabilityJobDefinition (CreateModelExplainabilityJobDefinition'),
    newCreateModelExplainabilityJobDefinition,
    CreateModelExplainabilityJobDefinitionResponse (CreateModelExplainabilityJobDefinitionResponse'),
    newCreateModelExplainabilityJobDefinitionResponse,

    -- ** DescribeEndpointConfig
    DescribeEndpointConfig (DescribeEndpointConfig'),
    newDescribeEndpointConfig,
    DescribeEndpointConfigResponse (DescribeEndpointConfigResponse'),
    newDescribeEndpointConfigResponse,

    -- ** DescribeMonitoringSchedule
    DescribeMonitoringSchedule (DescribeMonitoringSchedule'),
    newDescribeMonitoringSchedule,
    DescribeMonitoringScheduleResponse (DescribeMonitoringScheduleResponse'),
    newDescribeMonitoringScheduleResponse,

    -- ** DescribeLabelingJob
    DescribeLabelingJob (DescribeLabelingJob'),
    newDescribeLabelingJob,
    DescribeLabelingJobResponse (DescribeLabelingJobResponse'),
    newDescribeLabelingJobResponse,

    -- ** GetModelPackageGroupPolicy
    GetModelPackageGroupPolicy (GetModelPackageGroupPolicy'),
    newGetModelPackageGroupPolicy,
    GetModelPackageGroupPolicyResponse (GetModelPackageGroupPolicyResponse'),
    newGetModelPackageGroupPolicyResponse,

    -- ** UpdateModelPackage
    UpdateModelPackage (UpdateModelPackage'),
    newUpdateModelPackage,
    UpdateModelPackageResponse (UpdateModelPackageResponse'),
    newUpdateModelPackageResponse,

    -- ** CreateNotebookInstance
    CreateNotebookInstance (CreateNotebookInstance'),
    newCreateNotebookInstance,
    CreateNotebookInstanceResponse (CreateNotebookInstanceResponse'),
    newCreateNotebookInstanceResponse,

    -- ** StopMonitoringSchedule
    StopMonitoringSchedule (StopMonitoringSchedule'),
    newStopMonitoringSchedule,
    StopMonitoringScheduleResponse (StopMonitoringScheduleResponse'),
    newStopMonitoringScheduleResponse,

    -- ** CreateModelPackage
    CreateModelPackage (CreateModelPackage'),
    newCreateModelPackage,
    CreateModelPackageResponse (CreateModelPackageResponse'),
    newCreateModelPackageResponse,

    -- ** StopAutoMLJob
    StopAutoMLJob (StopAutoMLJob'),
    newStopAutoMLJob,
    StopAutoMLJobResponse (StopAutoMLJobResponse'),
    newStopAutoMLJobResponse,

    -- ** CreateProject
    CreateProject (CreateProject'),
    newCreateProject,
    CreateProjectResponse (CreateProjectResponse'),
    newCreateProjectResponse,

    -- ** DescribeAppImageConfig
    DescribeAppImageConfig (DescribeAppImageConfig'),
    newDescribeAppImageConfig,
    DescribeAppImageConfigResponse (DescribeAppImageConfigResponse'),
    newDescribeAppImageConfigResponse,

    -- ** ListSubscribedWorkteams (Paginated)
    ListSubscribedWorkteams (ListSubscribedWorkteams'),
    newListSubscribedWorkteams,
    ListSubscribedWorkteamsResponse (ListSubscribedWorkteamsResponse'),
    newListSubscribedWorkteamsResponse,

    -- ** ListDevices (Paginated)
    ListDevices (ListDevices'),
    newListDevices,
    ListDevicesResponse (ListDevicesResponse'),
    newListDevicesResponse,

    -- ** UpdateNotebookInstance
    UpdateNotebookInstance (UpdateNotebookInstance'),
    newUpdateNotebookInstance,
    UpdateNotebookInstanceResponse (UpdateNotebookInstanceResponse'),
    newUpdateNotebookInstanceResponse,

    -- ** DeleteModelExplainabilityJobDefinition
    DeleteModelExplainabilityJobDefinition (DeleteModelExplainabilityJobDefinition'),
    newDeleteModelExplainabilityJobDefinition,
    DeleteModelExplainabilityJobDefinitionResponse (DeleteModelExplainabilityJobDefinitionResponse'),
    newDeleteModelExplainabilityJobDefinitionResponse,

    -- ** DescribeProcessingJob
    DescribeProcessingJob (DescribeProcessingJob'),
    newDescribeProcessingJob,
    DescribeProcessingJobResponse (DescribeProcessingJobResponse'),
    newDescribeProcessingJobResponse,

    -- ** StartMonitoringSchedule
    StartMonitoringSchedule (StartMonitoringSchedule'),
    newStartMonitoringSchedule,
    StartMonitoringScheduleResponse (StartMonitoringScheduleResponse'),
    newStartMonitoringScheduleResponse,

    -- ** DeleteNotebookInstance
    DeleteNotebookInstance (DeleteNotebookInstance'),
    newDeleteNotebookInstance,
    DeleteNotebookInstanceResponse (DeleteNotebookInstanceResponse'),
    newDeleteNotebookInstanceResponse,

    -- ** ListDomains (Paginated)
    ListDomains (ListDomains'),
    newListDomains,
    ListDomainsResponse (ListDomainsResponse'),
    newListDomainsResponse,

    -- ** CreateTrial
    CreateTrial (CreateTrial'),
    newCreateTrial,
    CreateTrialResponse (CreateTrialResponse'),
    newCreateTrialResponse,

    -- ** DescribeTransformJob
    DescribeTransformJob (DescribeTransformJob'),
    newDescribeTransformJob,
    DescribeTransformJobResponse (DescribeTransformJobResponse'),
    newDescribeTransformJobResponse,

    -- ** DescribeEdgePackagingJob
    DescribeEdgePackagingJob (DescribeEdgePackagingJob'),
    newDescribeEdgePackagingJob,
    DescribeEdgePackagingJobResponse (DescribeEdgePackagingJobResponse'),
    newDescribeEdgePackagingJobResponse,

    -- ** DeleteModel
    DeleteModel (DeleteModel'),
    newDeleteModel,
    DeleteModelResponse (DeleteModelResponse'),
    newDeleteModelResponse,

    -- ** ListTrainingJobs (Paginated)
    ListTrainingJobs (ListTrainingJobs'),
    newListTrainingJobs,
    ListTrainingJobsResponse (ListTrainingJobsResponse'),
    newListTrainingJobsResponse,

    -- ** ListDataQualityJobDefinitions (Paginated)
    ListDataQualityJobDefinitions (ListDataQualityJobDefinitions'),
    newListDataQualityJobDefinitions,
    ListDataQualityJobDefinitionsResponse (ListDataQualityJobDefinitionsResponse'),
    newListDataQualityJobDefinitionsResponse,

    -- ** GetSagemakerServicecatalogPortfolioStatus
    GetSagemakerServicecatalogPortfolioStatus (GetSagemakerServicecatalogPortfolioStatus'),
    newGetSagemakerServicecatalogPortfolioStatus,
    GetSagemakerServicecatalogPortfolioStatusResponse (GetSagemakerServicecatalogPortfolioStatusResponse'),
    newGetSagemakerServicecatalogPortfolioStatusResponse,

    -- ** Search (Paginated)
    Search (Search'),
    newSearch,
    SearchResponse (SearchResponse'),
    newSearchResponse,

    -- ** StopCompilationJob
    StopCompilationJob (StopCompilationJob'),
    newStopCompilationJob,
    StopCompilationJobResponse (StopCompilationJobResponse'),
    newStopCompilationJobResponse,

    -- ** ListImages (Paginated)
    ListImages (ListImages'),
    newListImages,
    ListImagesResponse (ListImagesResponse'),
    newListImagesResponse,

    -- ** ListCandidatesForAutoMLJob (Paginated)
    ListCandidatesForAutoMLJob (ListCandidatesForAutoMLJob'),
    newListCandidatesForAutoMLJob,
    ListCandidatesForAutoMLJobResponse (ListCandidatesForAutoMLJobResponse'),
    newListCandidatesForAutoMLJobResponse,

    -- ** DeleteAlgorithm
    DeleteAlgorithm (DeleteAlgorithm'),
    newDeleteAlgorithm,
    DeleteAlgorithmResponse (DeleteAlgorithmResponse'),
    newDeleteAlgorithmResponse,

    -- ** GetDeviceFleetReport
    GetDeviceFleetReport (GetDeviceFleetReport'),
    newGetDeviceFleetReport,
    GetDeviceFleetReportResponse (GetDeviceFleetReportResponse'),
    newGetDeviceFleetReportResponse,

    -- ** DeleteDataQualityJobDefinition
    DeleteDataQualityJobDefinition (DeleteDataQualityJobDefinition'),
    newDeleteDataQualityJobDefinition,
    DeleteDataQualityJobDefinitionResponse (DeleteDataQualityJobDefinitionResponse'),
    newDeleteDataQualityJobDefinitionResponse,

    -- ** CreateModelPackageGroup
    CreateModelPackageGroup (CreateModelPackageGroup'),
    newCreateModelPackageGroup,
    CreateModelPackageGroupResponse (CreateModelPackageGroupResponse'),
    newCreateModelPackageGroupResponse,

    -- ** CreatePipeline
    CreatePipeline (CreatePipeline'),
    newCreatePipeline,
    CreatePipelineResponse (CreatePipelineResponse'),
    newCreatePipelineResponse,

    -- ** UpdateCodeRepository
    UpdateCodeRepository (UpdateCodeRepository'),
    newUpdateCodeRepository,
    UpdateCodeRepositoryResponse (UpdateCodeRepositoryResponse'),
    newUpdateCodeRepositoryResponse,

    -- ** DeleteCodeRepository
    DeleteCodeRepository (DeleteCodeRepository'),
    newDeleteCodeRepository,
    DeleteCodeRepositoryResponse (DeleteCodeRepositoryResponse'),
    newDeleteCodeRepositoryResponse,

    -- ** ListContexts (Paginated)
    ListContexts (ListContexts'),
    newListContexts,
    ListContextsResponse (ListContextsResponse'),
    newListContextsResponse,

    -- ** DescribeEndpoint
    DescribeEndpoint (DescribeEndpoint'),
    newDescribeEndpoint,
    DescribeEndpointResponse (DescribeEndpointResponse'),
    newDescribeEndpointResponse,

    -- ** DeregisterDevices
    DeregisterDevices (DeregisterDevices'),
    newDeregisterDevices,
    DeregisterDevicesResponse (DeregisterDevicesResponse'),
    newDeregisterDevicesResponse,

    -- ** CreateCodeRepository
    CreateCodeRepository (CreateCodeRepository'),
    newCreateCodeRepository,
    CreateCodeRepositoryResponse (CreateCodeRepositoryResponse'),
    newCreateCodeRepositoryResponse,

    -- ** CreateTrainingJob
    CreateTrainingJob (CreateTrainingJob'),
    newCreateTrainingJob,
    CreateTrainingJobResponse (CreateTrainingJobResponse'),
    newCreateTrainingJobResponse,

    -- ** DeletePipeline
    DeletePipeline (DeletePipeline'),
    newDeletePipeline,
    DeletePipelineResponse (DeletePipelineResponse'),
    newDeletePipelineResponse,

    -- ** CreateContext
    CreateContext (CreateContext'),
    newCreateContext,
    CreateContextResponse (CreateContextResponse'),
    newCreateContextResponse,

    -- ** StopEdgePackagingJob
    StopEdgePackagingJob (StopEdgePackagingJob'),
    newStopEdgePackagingJob,
    StopEdgePackagingJobResponse (StopEdgePackagingJobResponse'),
    newStopEdgePackagingJobResponse,

    -- ** UpdateUserProfile
    UpdateUserProfile (UpdateUserProfile'),
    newUpdateUserProfile,
    UpdateUserProfileResponse (UpdateUserProfileResponse'),
    newUpdateUserProfileResponse,

    -- ** DescribeCompilationJob
    DescribeCompilationJob (DescribeCompilationJob'),
    newDescribeCompilationJob,
    DescribeCompilationJobResponse (DescribeCompilationJobResponse'),
    newDescribeCompilationJobResponse,

    -- ** ListPipelineExecutionSteps (Paginated)
    ListPipelineExecutionSteps (ListPipelineExecutionSteps'),
    newListPipelineExecutionSteps,
    ListPipelineExecutionStepsResponse (ListPipelineExecutionStepsResponse'),
    newListPipelineExecutionStepsResponse,

    -- ** ListUserProfiles (Paginated)
    ListUserProfiles (ListUserProfiles'),
    newListUserProfiles,
    ListUserProfilesResponse (ListUserProfilesResponse'),
    newListUserProfilesResponse,

    -- ** DescribeHumanTaskUi
    DescribeHumanTaskUi (DescribeHumanTaskUi'),
    newDescribeHumanTaskUi,
    DescribeHumanTaskUiResponse (DescribeHumanTaskUiResponse'),
    newDescribeHumanTaskUiResponse,

    -- ** ListCodeRepositories (Paginated)
    ListCodeRepositories (ListCodeRepositories'),
    newListCodeRepositories,
    ListCodeRepositoriesResponse (ListCodeRepositoriesResponse'),
    newListCodeRepositoriesResponse,

    -- ** DescribeAction
    DescribeAction (DescribeAction'),
    newDescribeAction,
    DescribeActionResponse (DescribeActionResponse'),
    newDescribeActionResponse,

    -- ** UpdatePipeline
    UpdatePipeline (UpdatePipeline'),
    newUpdatePipeline,
    UpdatePipelineResponse (UpdatePipelineResponse'),
    newUpdatePipelineResponse,

    -- ** DescribeArtifact
    DescribeArtifact (DescribeArtifact'),
    newDescribeArtifact,
    DescribeArtifactResponse (DescribeArtifactResponse'),
    newDescribeArtifactResponse,

    -- ** DeleteUserProfile
    DeleteUserProfile (DeleteUserProfile'),
    newDeleteUserProfile,
    DeleteUserProfileResponse (DeleteUserProfileResponse'),
    newDeleteUserProfileResponse,

    -- ** StopTransformJob
    StopTransformJob (StopTransformJob'),
    newStopTransformJob,
    StopTransformJobResponse (StopTransformJobResponse'),
    newStopTransformJobResponse,

    -- ** CreateDataQualityJobDefinition
    CreateDataQualityJobDefinition (CreateDataQualityJobDefinition'),
    newCreateDataQualityJobDefinition,
    CreateDataQualityJobDefinitionResponse (CreateDataQualityJobDefinitionResponse'),
    newCreateDataQualityJobDefinitionResponse,

    -- ** DeleteModelPackageGroupPolicy
    DeleteModelPackageGroupPolicy (DeleteModelPackageGroupPolicy'),
    newDeleteModelPackageGroupPolicy,
    DeleteModelPackageGroupPolicyResponse (DeleteModelPackageGroupPolicyResponse'),
    newDeleteModelPackageGroupPolicyResponse,

    -- ** CreateImage
    CreateImage (CreateImage'),
    newCreateImage,
    CreateImageResponse (CreateImageResponse'),
    newCreateImageResponse,

    -- ** DescribeModelPackage
    DescribeModelPackage (DescribeModelPackage'),
    newDescribeModelPackage,
    DescribeModelPackageResponse (DescribeModelPackageResponse'),
    newDescribeModelPackageResponse,

    -- ** RetryPipelineExecution
    RetryPipelineExecution (RetryPipelineExecution'),
    newRetryPipelineExecution,
    RetryPipelineExecutionResponse (RetryPipelineExecutionResponse'),
    newRetryPipelineExecutionResponse,

    -- ** DeleteEndpointConfig
    DeleteEndpointConfig (DeleteEndpointConfig'),
    newDeleteEndpointConfig,
    DeleteEndpointConfigResponse (DeleteEndpointConfigResponse'),
    newDeleteEndpointConfigResponse,

    -- ** StopPipelineExecution
    StopPipelineExecution (StopPipelineExecution'),
    newStopPipelineExecution,
    StopPipelineExecutionResponse (StopPipelineExecutionResponse'),
    newStopPipelineExecutionResponse,

    -- ** ListApps (Paginated)
    ListApps (ListApps'),
    newListApps,
    ListAppsResponse (ListAppsResponse'),
    newListAppsResponse,

    -- ** CreateWorkforce
    CreateWorkforce (CreateWorkforce'),
    newCreateWorkforce,
    CreateWorkforceResponse (CreateWorkforceResponse'),
    newCreateWorkforceResponse,

    -- ** CreateStudioLifecycleConfig
    CreateStudioLifecycleConfig (CreateStudioLifecycleConfig'),
    newCreateStudioLifecycleConfig,
    CreateStudioLifecycleConfigResponse (CreateStudioLifecycleConfigResponse'),
    newCreateStudioLifecycleConfigResponse,

    -- ** ListAutoMLJobs (Paginated)
    ListAutoMLJobs (ListAutoMLJobs'),
    newListAutoMLJobs,
    ListAutoMLJobsResponse (ListAutoMLJobsResponse'),
    newListAutoMLJobsResponse,

    -- ** UpdateEndpointWeightsAndCapacities
    UpdateEndpointWeightsAndCapacities (UpdateEndpointWeightsAndCapacities'),
    newUpdateEndpointWeightsAndCapacities,
    UpdateEndpointWeightsAndCapacitiesResponse (UpdateEndpointWeightsAndCapacitiesResponse'),
    newUpdateEndpointWeightsAndCapacitiesResponse,

    -- ** StartNotebookInstance
    StartNotebookInstance (StartNotebookInstance'),
    newStartNotebookInstance,
    StartNotebookInstanceResponse (StartNotebookInstanceResponse'),
    newStartNotebookInstanceResponse,

    -- ** DeleteTags
    DeleteTags (DeleteTags'),
    newDeleteTags,
    DeleteTagsResponse (DeleteTagsResponse'),
    newDeleteTagsResponse,

    -- ** CreateAppImageConfig
    CreateAppImageConfig (CreateAppImageConfig'),
    newCreateAppImageConfig,
    CreateAppImageConfigResponse (CreateAppImageConfigResponse'),
    newCreateAppImageConfigResponse,

    -- ** DeleteAssociation
    DeleteAssociation (DeleteAssociation'),
    newDeleteAssociation,
    DeleteAssociationResponse (DeleteAssociationResponse'),
    newDeleteAssociationResponse,

    -- ** ListMonitoringSchedules (Paginated)
    ListMonitoringSchedules (ListMonitoringSchedules'),
    newListMonitoringSchedules,
    ListMonitoringSchedulesResponse (ListMonitoringSchedulesResponse'),
    newListMonitoringSchedulesResponse,

    -- ** DeleteMonitoringSchedule
    DeleteMonitoringSchedule (DeleteMonitoringSchedule'),
    newDeleteMonitoringSchedule,
    DeleteMonitoringScheduleResponse (DeleteMonitoringScheduleResponse'),
    newDeleteMonitoringScheduleResponse,

    -- ** ListEndpointConfigs (Paginated)
    ListEndpointConfigs (ListEndpointConfigs'),
    newListEndpointConfigs,
    ListEndpointConfigsResponse (ListEndpointConfigsResponse'),
    newListEndpointConfigsResponse,

    -- ** StartPipelineExecution
    StartPipelineExecution (StartPipelineExecution'),
    newStartPipelineExecution,
    StartPipelineExecutionResponse (StartPipelineExecutionResponse'),
    newStartPipelineExecutionResponse,

    -- ** StopNotebookInstance
    StopNotebookInstance (StopNotebookInstance'),
    newStopNotebookInstance,
    StopNotebookInstanceResponse (StopNotebookInstanceResponse'),
    newStopNotebookInstanceResponse,

    -- ** UpdateMonitoringSchedule
    UpdateMonitoringSchedule (UpdateMonitoringSchedule'),
    newUpdateMonitoringSchedule,
    UpdateMonitoringScheduleResponse (UpdateMonitoringScheduleResponse'),
    newUpdateMonitoringScheduleResponse,

    -- ** AddAssociation
    AddAssociation (AddAssociation'),
    newAddAssociation,
    AddAssociationResponse (AddAssociationResponse'),
    newAddAssociationResponse,

    -- ** CreateNotebookInstanceLifecycleConfig
    CreateNotebookInstanceLifecycleConfig (CreateNotebookInstanceLifecycleConfig'),
    newCreateNotebookInstanceLifecycleConfig,
    CreateNotebookInstanceLifecycleConfigResponse (CreateNotebookInstanceLifecycleConfigResponse'),
    newCreateNotebookInstanceLifecycleConfigResponse,

    -- ** AddTags
    AddTags (AddTags'),
    newAddTags,
    AddTagsResponse (AddTagsResponse'),
    newAddTagsResponse,

    -- ** DisableSagemakerServicecatalogPortfolio
    DisableSagemakerServicecatalogPortfolio (DisableSagemakerServicecatalogPortfolio'),
    newDisableSagemakerServicecatalogPortfolio,
    DisableSagemakerServicecatalogPortfolioResponse (DisableSagemakerServicecatalogPortfolioResponse'),
    newDisableSagemakerServicecatalogPortfolioResponse,

    -- ** UpdateNotebookInstanceLifecycleConfig
    UpdateNotebookInstanceLifecycleConfig (UpdateNotebookInstanceLifecycleConfig'),
    newUpdateNotebookInstanceLifecycleConfig,
    UpdateNotebookInstanceLifecycleConfigResponse (UpdateNotebookInstanceLifecycleConfigResponse'),
    newUpdateNotebookInstanceLifecycleConfigResponse,

    -- ** DescribeDomain
    DescribeDomain (DescribeDomain'),
    newDescribeDomain,
    DescribeDomainResponse (DescribeDomainResponse'),
    newDescribeDomainResponse,

    -- ** ListStudioLifecycleConfigs (Paginated)
    ListStudioLifecycleConfigs (ListStudioLifecycleConfigs'),
    newListStudioLifecycleConfigs,
    ListStudioLifecycleConfigsResponse (ListStudioLifecycleConfigsResponse'),
    newListStudioLifecycleConfigsResponse,

    -- ** ListAppImageConfigs (Paginated)
    ListAppImageConfigs (ListAppImageConfigs'),
    newListAppImageConfigs,
    ListAppImageConfigsResponse (ListAppImageConfigsResponse'),
    newListAppImageConfigsResponse,

    -- ** DescribeModelQualityJobDefinition
    DescribeModelQualityJobDefinition (DescribeModelQualityJobDefinition'),
    newDescribeModelQualityJobDefinition,
    DescribeModelQualityJobDefinitionResponse (DescribeModelQualityJobDefinitionResponse'),
    newDescribeModelQualityJobDefinitionResponse,

    -- ** DescribeDevice
    DescribeDevice (DescribeDevice'),
    newDescribeDevice,
    DescribeDeviceResponse (DescribeDeviceResponse'),
    newDescribeDeviceResponse,

    -- ** ListNotebookInstanceLifecycleConfigs (Paginated)
    ListNotebookInstanceLifecycleConfigs (ListNotebookInstanceLifecycleConfigs'),
    newListNotebookInstanceLifecycleConfigs,
    ListNotebookInstanceLifecycleConfigsResponse (ListNotebookInstanceLifecycleConfigsResponse'),
    newListNotebookInstanceLifecycleConfigsResponse,

    -- ** ListWorkforces (Paginated)
    ListWorkforces (ListWorkforces'),
    newListWorkforces,
    ListWorkforcesResponse (ListWorkforcesResponse'),
    newListWorkforcesResponse,

    -- ** DeleteStudioLifecycleConfig
    DeleteStudioLifecycleConfig (DeleteStudioLifecycleConfig'),
    newDeleteStudioLifecycleConfig,
    DeleteStudioLifecycleConfigResponse (DeleteStudioLifecycleConfigResponse'),
    newDeleteStudioLifecycleConfigResponse,

    -- ** CreateLabelingJob
    CreateLabelingJob (CreateLabelingJob'),
    newCreateLabelingJob,
    CreateLabelingJobResponse (CreateLabelingJobResponse'),
    newCreateLabelingJobResponse,

    -- ** DeleteModelBiasJobDefinition
    DeleteModelBiasJobDefinition (DeleteModelBiasJobDefinition'),
    newDeleteModelBiasJobDefinition,
    DeleteModelBiasJobDefinitionResponse (DeleteModelBiasJobDefinitionResponse'),
    newDeleteModelBiasJobDefinitionResponse,

    -- ** CreateExperiment
    CreateExperiment (CreateExperiment'),
    newCreateExperiment,
    CreateExperimentResponse (CreateExperimentResponse'),
    newCreateExperimentResponse,

    -- ** DescribePipelineExecution
    DescribePipelineExecution (DescribePipelineExecution'),
    newDescribePipelineExecution,
    DescribePipelineExecutionResponse (DescribePipelineExecutionResponse'),
    newDescribePipelineExecutionResponse,

    -- ** DeleteWorkforce
    DeleteWorkforce (DeleteWorkforce'),
    newDeleteWorkforce,
    DeleteWorkforceResponse (DeleteWorkforceResponse'),
    newDeleteWorkforceResponse,

    -- ** UpdateWorkforce
    UpdateWorkforce (UpdateWorkforce'),
    newUpdateWorkforce,
    UpdateWorkforceResponse (UpdateWorkforceResponse'),
    newUpdateWorkforceResponse,

    -- ** UpdateWorkteam
    UpdateWorkteam (UpdateWorkteam'),
    newUpdateWorkteam,
    UpdateWorkteamResponse (UpdateWorkteamResponse'),
    newUpdateWorkteamResponse,

    -- ** DescribeSubscribedWorkteam
    DescribeSubscribedWorkteam (DescribeSubscribedWorkteam'),
    newDescribeSubscribedWorkteam,
    DescribeSubscribedWorkteamResponse (DescribeSubscribedWorkteamResponse'),
    newDescribeSubscribedWorkteamResponse,

    -- ** DeleteWorkteam
    DeleteWorkteam (DeleteWorkteam'),
    newDeleteWorkteam,
    DeleteWorkteamResponse (DeleteWorkteamResponse'),
    newDeleteWorkteamResponse,

    -- ** DeleteNotebookInstanceLifecycleConfig
    DeleteNotebookInstanceLifecycleConfig (DeleteNotebookInstanceLifecycleConfig'),
    newDeleteNotebookInstanceLifecycleConfig,
    DeleteNotebookInstanceLifecycleConfigResponse (DeleteNotebookInstanceLifecycleConfigResponse'),
    newDeleteNotebookInstanceLifecycleConfigResponse,

    -- ** ListEdgePackagingJobs (Paginated)
    ListEdgePackagingJobs (ListEdgePackagingJobs'),
    newListEdgePackagingJobs,
    ListEdgePackagingJobsResponse (ListEdgePackagingJobsResponse'),
    newListEdgePackagingJobsResponse,

    -- ** CreateCompilationJob
    CreateCompilationJob (CreateCompilationJob'),
    newCreateCompilationJob,
    CreateCompilationJobResponse (CreateCompilationJobResponse'),
    newCreateCompilationJobResponse,

    -- ** CreateAction
    CreateAction (CreateAction'),
    newCreateAction,
    CreateActionResponse (CreateActionResponse'),
    newCreateActionResponse,

    -- ** CreateArtifact
    CreateArtifact (CreateArtifact'),
    newCreateArtifact,
    CreateArtifactResponse (CreateArtifactResponse'),
    newCreateArtifactResponse,

    -- ** DeleteEndpoint
    DeleteEndpoint (DeleteEndpoint'),
    newDeleteEndpoint,
    DeleteEndpointResponse (DeleteEndpointResponse'),
    newDeleteEndpointResponse,

    -- ** ListTransformJobs (Paginated)
    ListTransformJobs (ListTransformJobs'),
    newListTransformJobs,
    ListTransformJobsResponse (ListTransformJobsResponse'),
    newListTransformJobsResponse,

    -- ** DescribeCodeRepository
    DescribeCodeRepository (DescribeCodeRepository'),
    newDescribeCodeRepository,
    DescribeCodeRepositoryResponse (DescribeCodeRepositoryResponse'),
    newDescribeCodeRepositoryResponse,

    -- ** CreateDeviceFleet
    CreateDeviceFleet (CreateDeviceFleet'),
    newCreateDeviceFleet,
    CreateDeviceFleetResponse (CreateDeviceFleetResponse'),
    newCreateDeviceFleetResponse,

    -- ** DescribeModel
    DescribeModel (DescribeModel'),
    newDescribeModel,
    DescribeModelResponse (DescribeModelResponse'),
    newDescribeModelResponse,

    -- ** DescribeDataQualityJobDefinition
    DescribeDataQualityJobDefinition (DescribeDataQualityJobDefinition'),
    newDescribeDataQualityJobDefinition,
    DescribeDataQualityJobDefinitionResponse (DescribeDataQualityJobDefinitionResponse'),
    newDescribeDataQualityJobDefinitionResponse,

    -- ** UpdateEndpoint
    UpdateEndpoint (UpdateEndpoint'),
    newUpdateEndpoint,
    UpdateEndpointResponse (UpdateEndpointResponse'),
    newUpdateEndpointResponse,

    -- ** CreatePresignedDomainUrl
    CreatePresignedDomainUrl (CreatePresignedDomainUrl'),
    newCreatePresignedDomainUrl,
    CreatePresignedDomainUrlResponse (CreatePresignedDomainUrlResponse'),
    newCreatePresignedDomainUrlResponse,

    -- ** DescribeHyperParameterTuningJob
    DescribeHyperParameterTuningJob (DescribeHyperParameterTuningJob'),
    newDescribeHyperParameterTuningJob,
    DescribeHyperParameterTuningJobResponse (DescribeHyperParameterTuningJobResponse'),
    newDescribeHyperParameterTuningJobResponse,

    -- ** ListEndpoints (Paginated)
    ListEndpoints (ListEndpoints'),
    newListEndpoints,
    ListEndpointsResponse (ListEndpointsResponse'),
    newListEndpointsResponse,

    -- ** DescribeAlgorithm
    DescribeAlgorithm (DescribeAlgorithm'),
    newDescribeAlgorithm,
    DescribeAlgorithmResponse (DescribeAlgorithmResponse'),
    newDescribeAlgorithmResponse,

    -- ** UpdateDevices
    UpdateDevices (UpdateDevices'),
    newUpdateDevices,
    UpdateDevicesResponse (UpdateDevicesResponse'),
    newUpdateDevicesResponse,

    -- ** ListDeviceFleets (Paginated)
    ListDeviceFleets (ListDeviceFleets'),
    newListDeviceFleets,
    ListDeviceFleetsResponse (ListDeviceFleetsResponse'),
    newListDeviceFleetsResponse,

    -- ** ListLabelingJobsForWorkteam (Paginated)
    ListLabelingJobsForWorkteam (ListLabelingJobsForWorkteam'),
    newListLabelingJobsForWorkteam,
    ListLabelingJobsForWorkteamResponse (ListLabelingJobsForWorkteamResponse'),
    newListLabelingJobsForWorkteamResponse,

    -- ** CreateFeatureGroup
    CreateFeatureGroup (CreateFeatureGroup'),
    newCreateFeatureGroup,
    CreateFeatureGroupResponse (CreateFeatureGroupResponse'),
    newCreateFeatureGroupResponse,

    -- ** ListMonitoringExecutions (Paginated)
    ListMonitoringExecutions (ListMonitoringExecutions'),
    newListMonitoringExecutions,
    ListMonitoringExecutionsResponse (ListMonitoringExecutionsResponse'),
    newListMonitoringExecutionsResponse,

    -- ** DescribeModelPackageGroup
    DescribeModelPackageGroup (DescribeModelPackageGroup'),
    newDescribeModelPackageGroup,
    DescribeModelPackageGroupResponse (DescribeModelPackageGroupResponse'),
    newDescribeModelPackageGroupResponse,

    -- ** UpdateDeviceFleet
    UpdateDeviceFleet (UpdateDeviceFleet'),
    newUpdateDeviceFleet,
    UpdateDeviceFleetResponse (UpdateDeviceFleetResponse'),
    newUpdateDeviceFleetResponse,

    -- ** StopHyperParameterTuningJob
    StopHyperParameterTuningJob (StopHyperParameterTuningJob'),
    newStopHyperParameterTuningJob,
    StopHyperParameterTuningJobResponse (StopHyperParameterTuningJobResponse'),
    newStopHyperParameterTuningJobResponse,

    -- ** DeleteDeviceFleet
    DeleteDeviceFleet (DeleteDeviceFleet'),
    newDeleteDeviceFleet,
    DeleteDeviceFleetResponse (DeleteDeviceFleetResponse'),
    newDeleteDeviceFleetResponse,

    -- ** ListActions (Paginated)
    ListActions (ListActions'),
    newListActions,
    ListActionsResponse (ListActionsResponse'),
    newListActionsResponse,

    -- ** ListArtifacts (Paginated)
    ListArtifacts (ListArtifacts'),
    newListArtifacts,
    ListArtifactsResponse (ListArtifactsResponse'),
    newListArtifactsResponse,

    -- ** ListCompilationJobs (Paginated)
    ListCompilationJobs (ListCompilationJobs'),
    newListCompilationJobs,
    ListCompilationJobsResponse (ListCompilationJobsResponse'),
    newListCompilationJobsResponse,

    -- ** DescribeTrial
    DescribeTrial (DescribeTrial'),
    newDescribeTrial,
    DescribeTrialResponse (DescribeTrialResponse'),
    newDescribeTrialResponse,

    -- ** DeleteImageVersion
    DeleteImageVersion (DeleteImageVersion'),
    newDeleteImageVersion,
    DeleteImageVersionResponse (DeleteImageVersionResponse'),
    newDeleteImageVersionResponse,

    -- ** DeleteTrialComponent
    DeleteTrialComponent (DeleteTrialComponent'),
    newDeleteTrialComponent,
    DeleteTrialComponentResponse (DeleteTrialComponentResponse'),
    newDeleteTrialComponentResponse,

    -- ** ListTrialComponents (Paginated)
    ListTrialComponents (ListTrialComponents'),
    newListTrialComponents,
    ListTrialComponentsResponse (ListTrialComponentsResponse'),
    newListTrialComponentsResponse,

    -- ** DescribeAutoMLJob
    DescribeAutoMLJob (DescribeAutoMLJob'),
    newDescribeAutoMLJob,
    DescribeAutoMLJobResponse (DescribeAutoMLJobResponse'),
    newDescribeAutoMLJobResponse,

    -- ** DeleteProject
    DeleteProject (DeleteProject'),
    newDeleteProject,
    DeleteProjectResponse (DeleteProjectResponse'),
    newDeleteProjectResponse,

    -- ** DescribeApp
    DescribeApp (DescribeApp'),
    newDescribeApp,
    DescribeAppResponse (DescribeAppResponse'),
    newDescribeAppResponse,

    -- ** CreateDomain
    CreateDomain (CreateDomain'),
    newCreateDomain,
    CreateDomainResponse (CreateDomainResponse'),
    newCreateDomainResponse,

    -- ** DescribeExperiment
    DescribeExperiment (DescribeExperiment'),
    newDescribeExperiment,
    DescribeExperimentResponse (DescribeExperimentResponse'),
    newDescribeExperimentResponse,

    -- ** ListImageVersions (Paginated)
    ListImageVersions (ListImageVersions'),
    newListImageVersions,
    ListImageVersionsResponse (ListImageVersionsResponse'),
    newListImageVersionsResponse,

    -- ** StopProcessingJob
    StopProcessingJob (StopProcessingJob'),
    newStopProcessingJob,
    StopProcessingJobResponse (StopProcessingJobResponse'),
    newStopProcessingJobResponse,

    -- ** UpdateTrialComponent
    UpdateTrialComponent (UpdateTrialComponent'),
    newUpdateTrialComponent,
    UpdateTrialComponentResponse (UpdateTrialComponentResponse'),
    newUpdateTrialComponentResponse,

    -- ** UpdatePipelineExecution
    UpdatePipelineExecution (UpdatePipelineExecution'),
    newUpdatePipelineExecution,
    UpdatePipelineExecutionResponse (UpdatePipelineExecutionResponse'),
    newUpdatePipelineExecutionResponse,

    -- ** CreateTrialComponent
    CreateTrialComponent (CreateTrialComponent'),
    newCreateTrialComponent,
    CreateTrialComponentResponse (CreateTrialComponentResponse'),
    newCreateTrialComponentResponse,

    -- ** ListPipelineExecutions (Paginated)
    ListPipelineExecutions (ListPipelineExecutions'),
    newListPipelineExecutions,
    ListPipelineExecutionsResponse (ListPipelineExecutionsResponse'),
    newListPipelineExecutionsResponse,

    -- ** ListModelExplainabilityJobDefinitions (Paginated)
    ListModelExplainabilityJobDefinitions (ListModelExplainabilityJobDefinitions'),
    newListModelExplainabilityJobDefinitions,
    ListModelExplainabilityJobDefinitionsResponse (ListModelExplainabilityJobDefinitionsResponse'),
    newListModelExplainabilityJobDefinitionsResponse,

    -- ** AssociateTrialComponent
    AssociateTrialComponent (AssociateTrialComponent'),
    newAssociateTrialComponent,
    AssociateTrialComponentResponse (AssociateTrialComponentResponse'),
    newAssociateTrialComponentResponse,

    -- ** DescribeModelBiasJobDefinition
    DescribeModelBiasJobDefinition (DescribeModelBiasJobDefinition'),
    newDescribeModelBiasJobDefinition,
    DescribeModelBiasJobDefinitionResponse (DescribeModelBiasJobDefinitionResponse'),
    newDescribeModelBiasJobDefinitionResponse,

    -- ** DescribeStudioLifecycleConfig
    DescribeStudioLifecycleConfig (DescribeStudioLifecycleConfig'),
    newDescribeStudioLifecycleConfig,
    DescribeStudioLifecycleConfigResponse (DescribeStudioLifecycleConfigResponse'),
    newDescribeStudioLifecycleConfigResponse,

    -- ** DeleteDomain
    DeleteDomain (DeleteDomain'),
    newDeleteDomain,
    DeleteDomainResponse (DeleteDomainResponse'),
    newDeleteDomainResponse,

    -- ** ListNotebookInstances (Paginated)
    ListNotebookInstances (ListNotebookInstances'),
    newListNotebookInstances,
    ListNotebookInstancesResponse (ListNotebookInstancesResponse'),
    newListNotebookInstancesResponse,

    -- ** DescribeNotebookInstanceLifecycleConfig
    DescribeNotebookInstanceLifecycleConfig (DescribeNotebookInstanceLifecycleConfig'),
    newDescribeNotebookInstanceLifecycleConfig,
    DescribeNotebookInstanceLifecycleConfigResponse (DescribeNotebookInstanceLifecycleConfigResponse'),
    newDescribeNotebookInstanceLifecycleConfigResponse,

    -- ** StopLabelingJob
    StopLabelingJob (StopLabelingJob'),
    newStopLabelingJob,
    StopLabelingJobResponse (StopLabelingJobResponse'),
    newStopLabelingJobResponse,

    -- ** UpdateDomain
    UpdateDomain (UpdateDomain'),
    newUpdateDomain,
    UpdateDomainResponse (UpdateDomainResponse'),
    newUpdateDomainResponse,

    -- ** DescribeWorkforce
    DescribeWorkforce (DescribeWorkforce'),
    newDescribeWorkforce,
    DescribeWorkforceResponse (DescribeWorkforceResponse'),
    newDescribeWorkforceResponse,

    -- ** CreateImageVersion
    CreateImageVersion (CreateImageVersion'),
    newCreateImageVersion,
    CreateImageVersionResponse (CreateImageVersionResponse'),
    newCreateImageVersionResponse,

    -- ** DeleteModelQualityJobDefinition
    DeleteModelQualityJobDefinition (DeleteModelQualityJobDefinition'),
    newDeleteModelQualityJobDefinition,
    DeleteModelQualityJobDefinitionResponse (DeleteModelQualityJobDefinitionResponse'),
    newDeleteModelQualityJobDefinitionResponse,

    -- ** DescribeWorkteam
    DescribeWorkteam (DescribeWorkteam'),
    newDescribeWorkteam,
    DescribeWorkteamResponse (DescribeWorkteamResponse'),
    newDescribeWorkteamResponse,

    -- ** ListModelQualityJobDefinitions (Paginated)
    ListModelQualityJobDefinitions (ListModelQualityJobDefinitions'),
    newListModelQualityJobDefinitions,
    ListModelQualityJobDefinitionsResponse (ListModelQualityJobDefinitionsResponse'),
    newListModelQualityJobDefinitionsResponse,

    -- ** UpdateContext
    UpdateContext (UpdateContext'),
    newUpdateContext,
    UpdateContextResponse (UpdateContextResponse'),
    newUpdateContextResponse,

    -- ** ListModels (Paginated)
    ListModels (ListModels'),
    newListModels,
    ListModelsResponse (ListModelsResponse'),
    newListModelsResponse,

    -- ** ListHyperParameterTuningJobs (Paginated)
    ListHyperParameterTuningJobs (ListHyperParameterTuningJobs'),
    newListHyperParameterTuningJobs,
    ListHyperParameterTuningJobsResponse (ListHyperParameterTuningJobsResponse'),
    newListHyperParameterTuningJobsResponse,

    -- ** SendPipelineExecutionStepFailure
    SendPipelineExecutionStepFailure (SendPipelineExecutionStepFailure'),
    newSendPipelineExecutionStepFailure,
    SendPipelineExecutionStepFailureResponse (SendPipelineExecutionStepFailureResponse'),
    newSendPipelineExecutionStepFailureResponse,

    -- ** DescribeFeatureGroup
    DescribeFeatureGroup (DescribeFeatureGroup'),
    newDescribeFeatureGroup,
    DescribeFeatureGroupResponse (DescribeFeatureGroupResponse'),
    newDescribeFeatureGroupResponse,

    -- ** UpdateImage
    UpdateImage (UpdateImage'),
    newUpdateImage,
    UpdateImageResponse (UpdateImageResponse'),
    newUpdateImageResponse,

    -- ** ListFlowDefinitions (Paginated)
    ListFlowDefinitions (ListFlowDefinitions'),
    newListFlowDefinitions,
    ListFlowDefinitionsResponse (ListFlowDefinitionsResponse'),
    newListFlowDefinitionsResponse,

    -- ** ListAlgorithms (Paginated)
    ListAlgorithms (ListAlgorithms'),
    newListAlgorithms,
    ListAlgorithmsResponse (ListAlgorithmsResponse'),
    newListAlgorithmsResponse,

    -- ** UpdateTrainingJob
    UpdateTrainingJob (UpdateTrainingJob'),
    newUpdateTrainingJob,
    UpdateTrainingJobResponse (UpdateTrainingJobResponse'),
    newUpdateTrainingJobResponse,

    -- ** DeleteFlowDefinition
    DeleteFlowDefinition (DeleteFlowDefinition'),
    newDeleteFlowDefinition,
    DeleteFlowDefinitionResponse (DeleteFlowDefinitionResponse'),
    newDeleteFlowDefinitionResponse,

    -- ** DeleteImage
    DeleteImage (DeleteImage'),
    newDeleteImage,
    DeleteImageResponse (DeleteImageResponse'),
    newDeleteImageResponse,

    -- ** RenderUiTemplate
    RenderUiTemplate (RenderUiTemplate'),
    newRenderUiTemplate,
    RenderUiTemplateResponse (RenderUiTemplateResponse'),
    newRenderUiTemplateResponse,

    -- ** CreateUserProfile
    CreateUserProfile (CreateUserProfile'),
    newCreateUserProfile,
    CreateUserProfileResponse (CreateUserProfileResponse'),
    newCreateUserProfileResponse,

    -- ** DeleteContext
    DeleteContext (DeleteContext'),
    newDeleteContext,
    DeleteContextResponse (DeleteContextResponse'),
    newDeleteContextResponse,

    -- ** ListTags (Paginated)
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** PutModelPackageGroupPolicy
    PutModelPackageGroupPolicy (PutModelPackageGroupPolicy'),
    newPutModelPackageGroupPolicy,
    PutModelPackageGroupPolicyResponse (PutModelPackageGroupPolicyResponse'),
    newPutModelPackageGroupPolicyResponse,

    -- ** ListTrials (Paginated)
    ListTrials (ListTrials'),
    newListTrials,
    ListTrialsResponse (ListTrialsResponse'),
    newListTrialsResponse,

    -- ** DescribePipelineDefinitionForExecution
    DescribePipelineDefinitionForExecution (DescribePipelineDefinitionForExecution'),
    newDescribePipelineDefinitionForExecution,
    DescribePipelineDefinitionForExecutionResponse (DescribePipelineDefinitionForExecutionResponse'),
    newDescribePipelineDefinitionForExecutionResponse,

    -- ** ListModelPackageGroups (Paginated)
    ListModelPackageGroups (ListModelPackageGroups'),
    newListModelPackageGroups,
    ListModelPackageGroupsResponse (ListModelPackageGroupsResponse'),
    newListModelPackageGroupsResponse,

    -- ** ListPipelines (Paginated)
    ListPipelines (ListPipelines'),
    newListPipelines,
    ListPipelinesResponse (ListPipelinesResponse'),
    newListPipelinesResponse,

    -- ** ListPipelineParametersForExecution (Paginated)
    ListPipelineParametersForExecution (ListPipelineParametersForExecution'),
    newListPipelineParametersForExecution,
    ListPipelineParametersForExecutionResponse (ListPipelineParametersForExecutionResponse'),
    newListPipelineParametersForExecutionResponse,

    -- ** DeleteTrial
    DeleteTrial (DeleteTrial'),
    newDeleteTrial,
    DeleteTrialResponse (DeleteTrialResponse'),
    newDeleteTrialResponse,

    -- ** CreateAlgorithm
    CreateAlgorithm (CreateAlgorithm'),
    newCreateAlgorithm,
    CreateAlgorithmResponse (CreateAlgorithmResponse'),
    newCreateAlgorithmResponse,

    -- ** UpdateTrial
    UpdateTrial (UpdateTrial'),
    newUpdateTrial,
    UpdateTrialResponse (UpdateTrialResponse'),
    newUpdateTrialResponse,

    -- ** DeleteModelPackageGroup
    DeleteModelPackageGroup (DeleteModelPackageGroup'),
    newDeleteModelPackageGroup,
    DeleteModelPackageGroupResponse (DeleteModelPackageGroupResponse'),
    newDeleteModelPackageGroupResponse,

    -- ** DescribeDeviceFleet
    DescribeDeviceFleet (DescribeDeviceFleet'),
    newDescribeDeviceFleet,
    DescribeDeviceFleetResponse (DescribeDeviceFleetResponse'),
    newDescribeDeviceFleetResponse,

    -- ** CreateHyperParameterTuningJob
    CreateHyperParameterTuningJob (CreateHyperParameterTuningJob'),
    newCreateHyperParameterTuningJob,
    CreateHyperParameterTuningJobResponse (CreateHyperParameterTuningJobResponse'),
    newCreateHyperParameterTuningJobResponse,

    -- ** CreateModel
    CreateModel (CreateModel'),
    newCreateModel,
    CreateModelResponse (CreateModelResponse'),
    newCreateModelResponse,

    -- ** CreateFlowDefinition
    CreateFlowDefinition (CreateFlowDefinition'),
    newCreateFlowDefinition,
    CreateFlowDefinitionResponse (CreateFlowDefinitionResponse'),
    newCreateFlowDefinitionResponse,

    -- ** ListAssociations (Paginated)
    ListAssociations (ListAssociations'),
    newListAssociations,
    ListAssociationsResponse (ListAssociationsResponse'),
    newListAssociationsResponse,

    -- ** DeleteExperiment
    DeleteExperiment (DeleteExperiment'),
    newDeleteExperiment,
    DeleteExperimentResponse (DeleteExperimentResponse'),
    newDeleteExperimentResponse,

    -- ** DescribeProject
    DescribeProject (DescribeProject'),
    newDescribeProject,
    DescribeProjectResponse (DescribeProjectResponse'),
    newDescribeProjectResponse,

    -- ** DescribeImageVersion
    DescribeImageVersion (DescribeImageVersion'),
    newDescribeImageVersion,
    DescribeImageVersionResponse (DescribeImageVersionResponse'),
    newDescribeImageVersionResponse,

    -- ** ListExperiments (Paginated)
    ListExperiments (ListExperiments'),
    newListExperiments,
    ListExperimentsResponse (ListExperimentsResponse'),
    newListExperimentsResponse,

    -- ** CreateModelBiasJobDefinition
    CreateModelBiasJobDefinition (CreateModelBiasJobDefinition'),
    newCreateModelBiasJobDefinition,
    CreateModelBiasJobDefinitionResponse (CreateModelBiasJobDefinitionResponse'),
    newCreateModelBiasJobDefinitionResponse,

    -- ** UpdateExperiment
    UpdateExperiment (UpdateExperiment'),
    newUpdateExperiment,
    UpdateExperimentResponse (UpdateExperimentResponse'),
    newUpdateExperimentResponse,

    -- ** DescribeTrialComponent
    DescribeTrialComponent (DescribeTrialComponent'),
    newDescribeTrialComponent,
    DescribeTrialComponentResponse (DescribeTrialComponentResponse'),
    newDescribeTrialComponentResponse,

    -- ** CreateWorkteam
    CreateWorkteam (CreateWorkteam'),
    newCreateWorkteam,
    CreateWorkteamResponse (CreateWorkteamResponse'),
    newCreateWorkteamResponse,

    -- ** CreateProcessingJob
    CreateProcessingJob (CreateProcessingJob'),
    newCreateProcessingJob,
    CreateProcessingJobResponse (CreateProcessingJobResponse'),
    newCreateProcessingJobResponse,

    -- ** DeleteApp
    DeleteApp (DeleteApp'),
    newDeleteApp,
    DeleteAppResponse (DeleteAppResponse'),
    newDeleteAppResponse,

    -- ** ListLabelingJobs (Paginated)
    ListLabelingJobs (ListLabelingJobs'),
    newListLabelingJobs,
    ListLabelingJobsResponse (ListLabelingJobsResponse'),
    newListLabelingJobsResponse,

    -- ** ListWorkteams (Paginated)
    ListWorkteams (ListWorkteams'),
    newListWorkteams,
    ListWorkteamsResponse (ListWorkteamsResponse'),
    newListWorkteamsResponse,

    -- ** DeleteAppImageConfig
    DeleteAppImageConfig (DeleteAppImageConfig'),
    newDeleteAppImageConfig,
    DeleteAppImageConfigResponse (DeleteAppImageConfigResponse'),
    newDeleteAppImageConfigResponse,

    -- ** CreateApp
    CreateApp (CreateApp'),
    newCreateApp,
    CreateAppResponse (CreateAppResponse'),
    newCreateAppResponse,

    -- ** EnableSagemakerServicecatalogPortfolio
    EnableSagemakerServicecatalogPortfolio (EnableSagemakerServicecatalogPortfolio'),
    newEnableSagemakerServicecatalogPortfolio,
    EnableSagemakerServicecatalogPortfolioResponse (EnableSagemakerServicecatalogPortfolioResponse'),
    newEnableSagemakerServicecatalogPortfolioResponse,

    -- ** DescribeNotebookInstance
    DescribeNotebookInstance (DescribeNotebookInstance'),
    newDescribeNotebookInstance,
    DescribeNotebookInstanceResponse (DescribeNotebookInstanceResponse'),
    newDescribeNotebookInstanceResponse,

    -- ** DescribeModelExplainabilityJobDefinition
    DescribeModelExplainabilityJobDefinition (DescribeModelExplainabilityJobDefinition'),
    newDescribeModelExplainabilityJobDefinition,
    DescribeModelExplainabilityJobDefinitionResponse (DescribeModelExplainabilityJobDefinitionResponse'),
    newDescribeModelExplainabilityJobDefinitionResponse,

    -- ** CreateAutoMLJob
    CreateAutoMLJob (CreateAutoMLJob'),
    newCreateAutoMLJob,
    CreateAutoMLJobResponse (CreateAutoMLJobResponse'),
    newCreateAutoMLJobResponse,

    -- ** CreateEndpointConfig
    CreateEndpointConfig (CreateEndpointConfig'),
    newCreateEndpointConfig,
    CreateEndpointConfigResponse (CreateEndpointConfigResponse'),
    newCreateEndpointConfigResponse,

    -- ** ListModelBiasJobDefinitions (Paginated)
    ListModelBiasJobDefinitions (ListModelBiasJobDefinitions'),
    newListModelBiasJobDefinitions,
    ListModelBiasJobDefinitionsResponse (ListModelBiasJobDefinitionsResponse'),
    newListModelBiasJobDefinitionsResponse,

    -- ** SendPipelineExecutionStepSuccess
    SendPipelineExecutionStepSuccess (SendPipelineExecutionStepSuccess'),
    newSendPipelineExecutionStepSuccess,
    SendPipelineExecutionStepSuccessResponse (SendPipelineExecutionStepSuccessResponse'),
    newSendPipelineExecutionStepSuccessResponse,

    -- ** CreateMonitoringSchedule
    CreateMonitoringSchedule (CreateMonitoringSchedule'),
    newCreateMonitoringSchedule,
    CreateMonitoringScheduleResponse (CreateMonitoringScheduleResponse'),
    newCreateMonitoringScheduleResponse,

    -- ** ListProcessingJobs (Paginated)
    ListProcessingJobs (ListProcessingJobs'),
    newListProcessingJobs,
    ListProcessingJobsResponse (ListProcessingJobsResponse'),
    newListProcessingJobsResponse,

    -- ** UpdateAppImageConfig
    UpdateAppImageConfig (UpdateAppImageConfig'),
    newUpdateAppImageConfig,
    UpdateAppImageConfigResponse (UpdateAppImageConfigResponse'),
    newUpdateAppImageConfigResponse,

    -- ** DescribeContext
    DescribeContext (DescribeContext'),
    newDescribeContext,
    DescribeContextResponse (DescribeContextResponse'),
    newDescribeContextResponse,

    -- ** CreateHumanTaskUi
    CreateHumanTaskUi (CreateHumanTaskUi'),
    newCreateHumanTaskUi,
    CreateHumanTaskUiResponse (CreateHumanTaskUiResponse'),
    newCreateHumanTaskUiResponse,

    -- ** DeleteFeatureGroup
    DeleteFeatureGroup (DeleteFeatureGroup'),
    newDeleteFeatureGroup,
    DeleteFeatureGroupResponse (DeleteFeatureGroupResponse'),
    newDeleteFeatureGroupResponse,

    -- ** DescribeTrainingJob
    DescribeTrainingJob (DescribeTrainingJob'),
    newDescribeTrainingJob,
    DescribeTrainingJobResponse (DescribeTrainingJobResponse'),
    newDescribeTrainingJobResponse,

    -- ** DescribeFlowDefinition
    DescribeFlowDefinition (DescribeFlowDefinition'),
    newDescribeFlowDefinition,
    DescribeFlowDefinitionResponse (DescribeFlowDefinitionResponse'),
    newDescribeFlowDefinitionResponse,

    -- ** ListTrainingJobsForHyperParameterTuningJob (Paginated)
    ListTrainingJobsForHyperParameterTuningJob (ListTrainingJobsForHyperParameterTuningJob'),
    newListTrainingJobsForHyperParameterTuningJob,
    ListTrainingJobsForHyperParameterTuningJobResponse (ListTrainingJobsForHyperParameterTuningJobResponse'),
    newListTrainingJobsForHyperParameterTuningJobResponse,

    -- ** CreatePresignedNotebookInstanceUrl
    CreatePresignedNotebookInstanceUrl (CreatePresignedNotebookInstanceUrl'),
    newCreatePresignedNotebookInstanceUrl,
    CreatePresignedNotebookInstanceUrlResponse (CreatePresignedNotebookInstanceUrlResponse'),
    newCreatePresignedNotebookInstanceUrlResponse,

    -- ** RegisterDevices
    RegisterDevices (RegisterDevices'),
    newRegisterDevices,
    RegisterDevicesResponse (RegisterDevicesResponse'),
    newRegisterDevicesResponse,

    -- ** ListFeatureGroups (Paginated)
    ListFeatureGroups (ListFeatureGroups'),
    newListFeatureGroups,
    ListFeatureGroupsResponse (ListFeatureGroupsResponse'),
    newListFeatureGroupsResponse,

    -- ** DescribeImage
    DescribeImage (DescribeImage'),
    newDescribeImage,
    DescribeImageResponse (DescribeImageResponse'),
    newDescribeImageResponse,

    -- * Types

    -- ** ActionStatus
    ActionStatus (..),

    -- ** AlgorithmSortBy
    AlgorithmSortBy (..),

    -- ** AlgorithmStatus
    AlgorithmStatus (..),

    -- ** AppImageConfigSortKey
    AppImageConfigSortKey (..),

    -- ** AppInstanceType
    AppInstanceType (..),

    -- ** AppNetworkAccessType
    AppNetworkAccessType (..),

    -- ** AppSortKey
    AppSortKey (..),

    -- ** AppStatus
    AppStatus (..),

    -- ** AppType
    AppType (..),

    -- ** ArtifactSourceIdType
    ArtifactSourceIdType (..),

    -- ** AssemblyType
    AssemblyType (..),

    -- ** AssociationEdgeType
    AssociationEdgeType (..),

    -- ** AthenaResultCompressionType
    AthenaResultCompressionType (..),

    -- ** AthenaResultFormat
    AthenaResultFormat (..),

    -- ** AuthMode
    AuthMode (..),

    -- ** AutoMLJobObjectiveType
    AutoMLJobObjectiveType (..),

    -- ** AutoMLJobSecondaryStatus
    AutoMLJobSecondaryStatus (..),

    -- ** AutoMLJobStatus
    AutoMLJobStatus (..),

    -- ** AutoMLMetricEnum
    AutoMLMetricEnum (..),

    -- ** AutoMLS3DataType
    AutoMLS3DataType (..),

    -- ** AutoMLSortBy
    AutoMLSortBy (..),

    -- ** AutoMLSortOrder
    AutoMLSortOrder (..),

    -- ** AwsManagedHumanLoopRequestSource
    AwsManagedHumanLoopRequestSource (..),

    -- ** BatchStrategy
    BatchStrategy (..),

    -- ** BooleanOperator
    BooleanOperator (..),

    -- ** CandidateSortBy
    CandidateSortBy (..),

    -- ** CandidateStatus
    CandidateStatus (..),

    -- ** CandidateStepType
    CandidateStepType (..),

    -- ** CapacitySizeType
    CapacitySizeType (..),

    -- ** CaptureMode
    CaptureMode (..),

    -- ** CaptureStatus
    CaptureStatus (..),

    -- ** CodeRepositorySortBy
    CodeRepositorySortBy (..),

    -- ** CodeRepositorySortOrder
    CodeRepositorySortOrder (..),

    -- ** CompilationJobStatus
    CompilationJobStatus (..),

    -- ** CompressionType
    CompressionType (..),

    -- ** ConditionOutcome
    ConditionOutcome (..),

    -- ** ContainerMode
    ContainerMode (..),

    -- ** ContentClassifier
    ContentClassifier (..),

    -- ** DataDistributionType
    DataDistributionType (..),

    -- ** DetailedAlgorithmStatus
    DetailedAlgorithmStatus (..),

    -- ** DetailedModelPackageStatus
    DetailedModelPackageStatus (..),

    -- ** DirectInternetAccess
    DirectInternetAccess (..),

    -- ** DomainStatus
    DomainStatus (..),

    -- ** EdgePackagingJobStatus
    EdgePackagingJobStatus (..),

    -- ** EdgePresetDeploymentStatus
    EdgePresetDeploymentStatus (..),

    -- ** EdgePresetDeploymentType
    EdgePresetDeploymentType (..),

    -- ** EndpointConfigSortKey
    EndpointConfigSortKey (..),

    -- ** EndpointSortKey
    EndpointSortKey (..),

    -- ** EndpointStatus
    EndpointStatus (..),

    -- ** ExecutionStatus
    ExecutionStatus (..),

    -- ** FeatureGroupSortBy
    FeatureGroupSortBy (..),

    -- ** FeatureGroupSortOrder
    FeatureGroupSortOrder (..),

    -- ** FeatureGroupStatus
    FeatureGroupStatus (..),

    -- ** FeatureType
    FeatureType (..),

    -- ** FileSystemAccessMode
    FileSystemAccessMode (..),

    -- ** FileSystemType
    FileSystemType (..),

    -- ** FlowDefinitionStatus
    FlowDefinitionStatus (..),

    -- ** Framework
    Framework (..),

    -- ** HumanTaskUiStatus
    HumanTaskUiStatus (..),

    -- ** HyperParameterScalingType
    HyperParameterScalingType (..),

    -- ** HyperParameterTuningJobObjectiveType
    HyperParameterTuningJobObjectiveType (..),

    -- ** HyperParameterTuningJobSortByOptions
    HyperParameterTuningJobSortByOptions (..),

    -- ** HyperParameterTuningJobStatus
    HyperParameterTuningJobStatus (..),

    -- ** HyperParameterTuningJobStrategyType
    HyperParameterTuningJobStrategyType (..),

    -- ** HyperParameterTuningJobWarmStartType
    HyperParameterTuningJobWarmStartType (..),

    -- ** ImageSortBy
    ImageSortBy (..),

    -- ** ImageSortOrder
    ImageSortOrder (..),

    -- ** ImageStatus
    ImageStatus (..),

    -- ** ImageVersionSortBy
    ImageVersionSortBy (..),

    -- ** ImageVersionSortOrder
    ImageVersionSortOrder (..),

    -- ** ImageVersionStatus
    ImageVersionStatus (..),

    -- ** InferenceExecutionMode
    InferenceExecutionMode (..),

    -- ** InputMode
    InputMode (..),

    -- ** InstanceType
    InstanceType (..),

    -- ** JoinSource
    JoinSource (..),

    -- ** LabelingJobStatus
    LabelingJobStatus (..),

    -- ** ListCompilationJobsSortBy
    ListCompilationJobsSortBy (..),

    -- ** ListDeviceFleetsSortBy
    ListDeviceFleetsSortBy (..),

    -- ** ListEdgePackagingJobsSortBy
    ListEdgePackagingJobsSortBy (..),

    -- ** ListLabelingJobsForWorkteamSortByOptions
    ListLabelingJobsForWorkteamSortByOptions (..),

    -- ** ListWorkforcesSortByOptions
    ListWorkforcesSortByOptions (..),

    -- ** ListWorkteamsSortByOptions
    ListWorkteamsSortByOptions (..),

    -- ** MetricSetSource
    MetricSetSource (..),

    -- ** ModelApprovalStatus
    ModelApprovalStatus (..),

    -- ** ModelCacheSetting
    ModelCacheSetting (..),

    -- ** ModelPackageGroupSortBy
    ModelPackageGroupSortBy (..),

    -- ** ModelPackageGroupStatus
    ModelPackageGroupStatus (..),

    -- ** ModelPackageSortBy
    ModelPackageSortBy (..),

    -- ** ModelPackageStatus
    ModelPackageStatus (..),

    -- ** ModelPackageType
    ModelPackageType (..),

    -- ** ModelSortKey
    ModelSortKey (..),

    -- ** MonitoringExecutionSortKey
    MonitoringExecutionSortKey (..),

    -- ** MonitoringJobDefinitionSortKey
    MonitoringJobDefinitionSortKey (..),

    -- ** MonitoringProblemType
    MonitoringProblemType (..),

    -- ** MonitoringScheduleSortKey
    MonitoringScheduleSortKey (..),

    -- ** MonitoringType
    MonitoringType (..),

    -- ** NotebookInstanceAcceleratorType
    NotebookInstanceAcceleratorType (..),

    -- ** NotebookInstanceLifecycleConfigSortKey
    NotebookInstanceLifecycleConfigSortKey (..),

    -- ** NotebookInstanceLifecycleConfigSortOrder
    NotebookInstanceLifecycleConfigSortOrder (..),

    -- ** NotebookInstanceSortKey
    NotebookInstanceSortKey (..),

    -- ** NotebookInstanceSortOrder
    NotebookInstanceSortOrder (..),

    -- ** NotebookInstanceStatus
    NotebookInstanceStatus (..),

    -- ** NotebookOutputOption
    NotebookOutputOption (..),

    -- ** ObjectiveStatus
    ObjectiveStatus (..),

    -- ** OfflineStoreStatusValue
    OfflineStoreStatusValue (..),

    -- ** Operator
    Operator (..),

    -- ** OrderKey
    OrderKey (..),

    -- ** ParameterType
    ParameterType (..),

    -- ** PipelineExecutionStatus
    PipelineExecutionStatus (..),

    -- ** PipelineStatus
    PipelineStatus (..),

    -- ** ProblemType
    ProblemType (..),

    -- ** ProcessingInstanceType
    ProcessingInstanceType (..),

    -- ** ProcessingJobStatus
    ProcessingJobStatus (..),

    -- ** ProcessingS3CompressionType
    ProcessingS3CompressionType (..),

    -- ** ProcessingS3DataDistributionType
    ProcessingS3DataDistributionType (..),

    -- ** ProcessingS3DataType
    ProcessingS3DataType (..),

    -- ** ProcessingS3InputMode
    ProcessingS3InputMode (..),

    -- ** ProcessingS3UploadMode
    ProcessingS3UploadMode (..),

    -- ** ProductionVariantAcceleratorType
    ProductionVariantAcceleratorType (..),

    -- ** ProductionVariantInstanceType
    ProductionVariantInstanceType (..),

    -- ** ProfilingStatus
    ProfilingStatus (..),

    -- ** ProjectSortBy
    ProjectSortBy (..),

    -- ** ProjectSortOrder
    ProjectSortOrder (..),

    -- ** ProjectStatus
    ProjectStatus (..),

    -- ** RecordWrapper
    RecordWrapper (..),

    -- ** RedshiftResultCompressionType
    RedshiftResultCompressionType (..),

    -- ** RedshiftResultFormat
    RedshiftResultFormat (..),

    -- ** RepositoryAccessMode
    RepositoryAccessMode (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** RetentionType
    RetentionType (..),

    -- ** RootAccess
    RootAccess (..),

    -- ** RuleEvaluationStatus
    RuleEvaluationStatus (..),

    -- ** S3DataDistribution
    S3DataDistribution (..),

    -- ** S3DataType
    S3DataType (..),

    -- ** SagemakerServicecatalogStatus
    SagemakerServicecatalogStatus (..),

    -- ** ScheduleStatus
    ScheduleStatus (..),

    -- ** SearchSortOrder
    SearchSortOrder (..),

    -- ** SecondaryStatus
    SecondaryStatus (..),

    -- ** SortActionsBy
    SortActionsBy (..),

    -- ** SortArtifactsBy
    SortArtifactsBy (..),

    -- ** SortAssociationsBy
    SortAssociationsBy (..),

    -- ** SortBy
    SortBy (..),

    -- ** SortContextsBy
    SortContextsBy (..),

    -- ** SortExperimentsBy
    SortExperimentsBy (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** SortPipelineExecutionsBy
    SortPipelineExecutionsBy (..),

    -- ** SortPipelinesBy
    SortPipelinesBy (..),

    -- ** SortTrialComponentsBy
    SortTrialComponentsBy (..),

    -- ** SortTrialsBy
    SortTrialsBy (..),

    -- ** SplitType
    SplitType (..),

    -- ** StepStatus
    StepStatus (..),

    -- ** StudioLifecycleConfigAppType
    StudioLifecycleConfigAppType (..),

    -- ** StudioLifecycleConfigSortKey
    StudioLifecycleConfigSortKey (..),

    -- ** TargetDevice
    TargetDevice (..),

    -- ** TargetPlatformAccelerator
    TargetPlatformAccelerator (..),

    -- ** TargetPlatformArch
    TargetPlatformArch (..),

    -- ** TargetPlatformOs
    TargetPlatformOs (..),

    -- ** TrafficRoutingConfigType
    TrafficRoutingConfigType (..),

    -- ** TrainingInputMode
    TrainingInputMode (..),

    -- ** TrainingInstanceType
    TrainingInstanceType (..),

    -- ** TrainingJobEarlyStoppingType
    TrainingJobEarlyStoppingType (..),

    -- ** TrainingJobSortByOptions
    TrainingJobSortByOptions (..),

    -- ** TrainingJobStatus
    TrainingJobStatus (..),

    -- ** TransformInstanceType
    TransformInstanceType (..),

    -- ** TransformJobStatus
    TransformJobStatus (..),

    -- ** TrialComponentPrimaryStatus
    TrialComponentPrimaryStatus (..),

    -- ** UserProfileSortKey
    UserProfileSortKey (..),

    -- ** UserProfileStatus
    UserProfileStatus (..),

    -- ** VariantPropertyType
    VariantPropertyType (..),

    -- ** ActionSource
    ActionSource (ActionSource'),
    newActionSource,

    -- ** ActionSummary
    ActionSummary (ActionSummary'),
    newActionSummary,

    -- ** AgentVersion
    AgentVersion (AgentVersion'),
    newAgentVersion,

    -- ** Alarm
    Alarm (Alarm'),
    newAlarm,

    -- ** AlgorithmSpecification
    AlgorithmSpecification (AlgorithmSpecification'),
    newAlgorithmSpecification,

    -- ** AlgorithmStatusDetails
    AlgorithmStatusDetails (AlgorithmStatusDetails'),
    newAlgorithmStatusDetails,

    -- ** AlgorithmStatusItem
    AlgorithmStatusItem (AlgorithmStatusItem'),
    newAlgorithmStatusItem,

    -- ** AlgorithmSummary
    AlgorithmSummary (AlgorithmSummary'),
    newAlgorithmSummary,

    -- ** AlgorithmValidationProfile
    AlgorithmValidationProfile (AlgorithmValidationProfile'),
    newAlgorithmValidationProfile,

    -- ** AlgorithmValidationSpecification
    AlgorithmValidationSpecification (AlgorithmValidationSpecification'),
    newAlgorithmValidationSpecification,

    -- ** AnnotationConsolidationConfig
    AnnotationConsolidationConfig (AnnotationConsolidationConfig'),
    newAnnotationConsolidationConfig,

    -- ** AppDetails
    AppDetails (AppDetails'),
    newAppDetails,

    -- ** AppImageConfigDetails
    AppImageConfigDetails (AppImageConfigDetails'),
    newAppImageConfigDetails,

    -- ** AppSpecification
    AppSpecification (AppSpecification'),
    newAppSpecification,

    -- ** ArtifactSource
    ArtifactSource (ArtifactSource'),
    newArtifactSource,

    -- ** ArtifactSourceType
    ArtifactSourceType (ArtifactSourceType'),
    newArtifactSourceType,

    -- ** ArtifactSummary
    ArtifactSummary (ArtifactSummary'),
    newArtifactSummary,

    -- ** AssociationSummary
    AssociationSummary (AssociationSummary'),
    newAssociationSummary,

    -- ** AsyncInferenceClientConfig
    AsyncInferenceClientConfig (AsyncInferenceClientConfig'),
    newAsyncInferenceClientConfig,

    -- ** AsyncInferenceConfig
    AsyncInferenceConfig (AsyncInferenceConfig'),
    newAsyncInferenceConfig,

    -- ** AsyncInferenceNotificationConfig
    AsyncInferenceNotificationConfig (AsyncInferenceNotificationConfig'),
    newAsyncInferenceNotificationConfig,

    -- ** AsyncInferenceOutputConfig
    AsyncInferenceOutputConfig (AsyncInferenceOutputConfig'),
    newAsyncInferenceOutputConfig,

    -- ** AthenaDatasetDefinition
    AthenaDatasetDefinition (AthenaDatasetDefinition'),
    newAthenaDatasetDefinition,

    -- ** AutoMLCandidate
    AutoMLCandidate (AutoMLCandidate'),
    newAutoMLCandidate,

    -- ** AutoMLCandidateStep
    AutoMLCandidateStep (AutoMLCandidateStep'),
    newAutoMLCandidateStep,

    -- ** AutoMLChannel
    AutoMLChannel (AutoMLChannel'),
    newAutoMLChannel,

    -- ** AutoMLContainerDefinition
    AutoMLContainerDefinition (AutoMLContainerDefinition'),
    newAutoMLContainerDefinition,

    -- ** AutoMLDataSource
    AutoMLDataSource (AutoMLDataSource'),
    newAutoMLDataSource,

    -- ** AutoMLJobArtifacts
    AutoMLJobArtifacts (AutoMLJobArtifacts'),
    newAutoMLJobArtifacts,

    -- ** AutoMLJobCompletionCriteria
    AutoMLJobCompletionCriteria (AutoMLJobCompletionCriteria'),
    newAutoMLJobCompletionCriteria,

    -- ** AutoMLJobConfig
    AutoMLJobConfig (AutoMLJobConfig'),
    newAutoMLJobConfig,

    -- ** AutoMLJobObjective
    AutoMLJobObjective (AutoMLJobObjective'),
    newAutoMLJobObjective,

    -- ** AutoMLJobSummary
    AutoMLJobSummary (AutoMLJobSummary'),
    newAutoMLJobSummary,

    -- ** AutoMLOutputDataConfig
    AutoMLOutputDataConfig (AutoMLOutputDataConfig'),
    newAutoMLOutputDataConfig,

    -- ** AutoMLPartialFailureReason
    AutoMLPartialFailureReason (AutoMLPartialFailureReason'),
    newAutoMLPartialFailureReason,

    -- ** AutoMLS3DataSource
    AutoMLS3DataSource (AutoMLS3DataSource'),
    newAutoMLS3DataSource,

    -- ** AutoMLSecurityConfig
    AutoMLSecurityConfig (AutoMLSecurityConfig'),
    newAutoMLSecurityConfig,

    -- ** AutoRollbackConfig
    AutoRollbackConfig (AutoRollbackConfig'),
    newAutoRollbackConfig,

    -- ** Bias
    Bias (Bias'),
    newBias,

    -- ** BlueGreenUpdatePolicy
    BlueGreenUpdatePolicy (BlueGreenUpdatePolicy'),
    newBlueGreenUpdatePolicy,

    -- ** CacheHitResult
    CacheHitResult (CacheHitResult'),
    newCacheHitResult,

    -- ** CallbackStepMetadata
    CallbackStepMetadata (CallbackStepMetadata'),
    newCallbackStepMetadata,

    -- ** CandidateArtifactLocations
    CandidateArtifactLocations (CandidateArtifactLocations'),
    newCandidateArtifactLocations,

    -- ** CandidateProperties
    CandidateProperties (CandidateProperties'),
    newCandidateProperties,

    -- ** CapacitySize
    CapacitySize (CapacitySize'),
    newCapacitySize,

    -- ** CaptureContentTypeHeader
    CaptureContentTypeHeader (CaptureContentTypeHeader'),
    newCaptureContentTypeHeader,

    -- ** CaptureOption
    CaptureOption (CaptureOption'),
    newCaptureOption,

    -- ** CategoricalParameterRange
    CategoricalParameterRange (CategoricalParameterRange'),
    newCategoricalParameterRange,

    -- ** CategoricalParameterRangeSpecification
    CategoricalParameterRangeSpecification (CategoricalParameterRangeSpecification'),
    newCategoricalParameterRangeSpecification,

    -- ** Channel
    Channel (Channel'),
    newChannel,

    -- ** ChannelSpecification
    ChannelSpecification (ChannelSpecification'),
    newChannelSpecification,

    -- ** CheckpointConfig
    CheckpointConfig (CheckpointConfig'),
    newCheckpointConfig,

    -- ** CodeRepositorySummary
    CodeRepositorySummary (CodeRepositorySummary'),
    newCodeRepositorySummary,

    -- ** CognitoConfig
    CognitoConfig (CognitoConfig'),
    newCognitoConfig,

    -- ** CognitoMemberDefinition
    CognitoMemberDefinition (CognitoMemberDefinition'),
    newCognitoMemberDefinition,

    -- ** CollectionConfiguration
    CollectionConfiguration (CollectionConfiguration'),
    newCollectionConfiguration,

    -- ** CompilationJobSummary
    CompilationJobSummary (CompilationJobSummary'),
    newCompilationJobSummary,

    -- ** ConditionStepMetadata
    ConditionStepMetadata (ConditionStepMetadata'),
    newConditionStepMetadata,

    -- ** ContainerDefinition
    ContainerDefinition (ContainerDefinition'),
    newContainerDefinition,

    -- ** ContextSource
    ContextSource (ContextSource'),
    newContextSource,

    -- ** ContextSummary
    ContextSummary (ContextSummary'),
    newContextSummary,

    -- ** ContinuousParameterRange
    ContinuousParameterRange (ContinuousParameterRange'),
    newContinuousParameterRange,

    -- ** ContinuousParameterRangeSpecification
    ContinuousParameterRangeSpecification (ContinuousParameterRangeSpecification'),
    newContinuousParameterRangeSpecification,

    -- ** CustomImage
    CustomImage (CustomImage'),
    newCustomImage,

    -- ** DataCaptureConfig
    DataCaptureConfig (DataCaptureConfig'),
    newDataCaptureConfig,

    -- ** DataCaptureConfigSummary
    DataCaptureConfigSummary (DataCaptureConfigSummary'),
    newDataCaptureConfigSummary,

    -- ** DataCatalogConfig
    DataCatalogConfig (DataCatalogConfig'),
    newDataCatalogConfig,

    -- ** DataProcessing
    DataProcessing (DataProcessing'),
    newDataProcessing,

    -- ** DataQualityAppSpecification
    DataQualityAppSpecification (DataQualityAppSpecification'),
    newDataQualityAppSpecification,

    -- ** DataQualityBaselineConfig
    DataQualityBaselineConfig (DataQualityBaselineConfig'),
    newDataQualityBaselineConfig,

    -- ** DataQualityJobInput
    DataQualityJobInput (DataQualityJobInput'),
    newDataQualityJobInput,

    -- ** DataSource
    DataSource (DataSource'),
    newDataSource,

    -- ** DatasetDefinition
    DatasetDefinition (DatasetDefinition'),
    newDatasetDefinition,

    -- ** DebugHookConfig
    DebugHookConfig (DebugHookConfig'),
    newDebugHookConfig,

    -- ** DebugRuleConfiguration
    DebugRuleConfiguration (DebugRuleConfiguration'),
    newDebugRuleConfiguration,

    -- ** DebugRuleEvaluationStatus
    DebugRuleEvaluationStatus (DebugRuleEvaluationStatus'),
    newDebugRuleEvaluationStatus,

    -- ** DeployedImage
    DeployedImage (DeployedImage'),
    newDeployedImage,

    -- ** DeploymentConfig
    DeploymentConfig (DeploymentConfig'),
    newDeploymentConfig,

    -- ** DesiredWeightAndCapacity
    DesiredWeightAndCapacity (DesiredWeightAndCapacity'),
    newDesiredWeightAndCapacity,

    -- ** Device
    Device (Device'),
    newDevice,

    -- ** DeviceFleetSummary
    DeviceFleetSummary (DeviceFleetSummary'),
    newDeviceFleetSummary,

    -- ** DeviceStats
    DeviceStats (DeviceStats'),
    newDeviceStats,

    -- ** DeviceSummary
    DeviceSummary (DeviceSummary'),
    newDeviceSummary,

    -- ** DomainDetails
    DomainDetails (DomainDetails'),
    newDomainDetails,

    -- ** EdgeModel
    EdgeModel (EdgeModel'),
    newEdgeModel,

    -- ** EdgeModelStat
    EdgeModelStat (EdgeModelStat'),
    newEdgeModelStat,

    -- ** EdgeModelSummary
    EdgeModelSummary (EdgeModelSummary'),
    newEdgeModelSummary,

    -- ** EdgeOutputConfig
    EdgeOutputConfig (EdgeOutputConfig'),
    newEdgeOutputConfig,

    -- ** EdgePackagingJobSummary
    EdgePackagingJobSummary (EdgePackagingJobSummary'),
    newEdgePackagingJobSummary,

    -- ** EdgePresetDeploymentOutput
    EdgePresetDeploymentOutput (EdgePresetDeploymentOutput'),
    newEdgePresetDeploymentOutput,

    -- ** Endpoint
    Endpoint (Endpoint'),
    newEndpoint,

    -- ** EndpointConfigSummary
    EndpointConfigSummary (EndpointConfigSummary'),
    newEndpointConfigSummary,

    -- ** EndpointInput
    EndpointInput (EndpointInput'),
    newEndpointInput,

    -- ** EndpointSummary
    EndpointSummary (EndpointSummary'),
    newEndpointSummary,

    -- ** Experiment
    Experiment (Experiment'),
    newExperiment,

    -- ** ExperimentConfig
    ExperimentConfig (ExperimentConfig'),
    newExperimentConfig,

    -- ** ExperimentSource
    ExperimentSource (ExperimentSource'),
    newExperimentSource,

    -- ** ExperimentSummary
    ExperimentSummary (ExperimentSummary'),
    newExperimentSummary,

    -- ** Explainability
    Explainability (Explainability'),
    newExplainability,

    -- ** FeatureDefinition
    FeatureDefinition (FeatureDefinition'),
    newFeatureDefinition,

    -- ** FeatureGroup
    FeatureGroup (FeatureGroup'),
    newFeatureGroup,

    -- ** FeatureGroupSummary
    FeatureGroupSummary (FeatureGroupSummary'),
    newFeatureGroupSummary,

    -- ** FileSystemConfig
    FileSystemConfig (FileSystemConfig'),
    newFileSystemConfig,

    -- ** FileSystemDataSource
    FileSystemDataSource (FileSystemDataSource'),
    newFileSystemDataSource,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** FinalAutoMLJobObjectiveMetric
    FinalAutoMLJobObjectiveMetric (FinalAutoMLJobObjectiveMetric'),
    newFinalAutoMLJobObjectiveMetric,

    -- ** FinalHyperParameterTuningJobObjectiveMetric
    FinalHyperParameterTuningJobObjectiveMetric (FinalHyperParameterTuningJobObjectiveMetric'),
    newFinalHyperParameterTuningJobObjectiveMetric,

    -- ** FlowDefinitionOutputConfig
    FlowDefinitionOutputConfig (FlowDefinitionOutputConfig'),
    newFlowDefinitionOutputConfig,

    -- ** FlowDefinitionSummary
    FlowDefinitionSummary (FlowDefinitionSummary'),
    newFlowDefinitionSummary,

    -- ** GitConfig
    GitConfig (GitConfig'),
    newGitConfig,

    -- ** GitConfigForUpdate
    GitConfigForUpdate (GitConfigForUpdate'),
    newGitConfigForUpdate,

    -- ** HumanLoopActivationConditionsConfig
    HumanLoopActivationConditionsConfig (HumanLoopActivationConditionsConfig'),
    newHumanLoopActivationConditionsConfig,

    -- ** HumanLoopActivationConfig
    HumanLoopActivationConfig (HumanLoopActivationConfig'),
    newHumanLoopActivationConfig,

    -- ** HumanLoopConfig
    HumanLoopConfig (HumanLoopConfig'),
    newHumanLoopConfig,

    -- ** HumanLoopRequestSource
    HumanLoopRequestSource (HumanLoopRequestSource'),
    newHumanLoopRequestSource,

    -- ** HumanTaskConfig
    HumanTaskConfig (HumanTaskConfig'),
    newHumanTaskConfig,

    -- ** HumanTaskUiSummary
    HumanTaskUiSummary (HumanTaskUiSummary'),
    newHumanTaskUiSummary,

    -- ** HyperParameterAlgorithmSpecification
    HyperParameterAlgorithmSpecification (HyperParameterAlgorithmSpecification'),
    newHyperParameterAlgorithmSpecification,

    -- ** HyperParameterSpecification
    HyperParameterSpecification (HyperParameterSpecification'),
    newHyperParameterSpecification,

    -- ** HyperParameterTrainingJobDefinition
    HyperParameterTrainingJobDefinition (HyperParameterTrainingJobDefinition'),
    newHyperParameterTrainingJobDefinition,

    -- ** HyperParameterTrainingJobSummary
    HyperParameterTrainingJobSummary (HyperParameterTrainingJobSummary'),
    newHyperParameterTrainingJobSummary,

    -- ** HyperParameterTuningJobConfig
    HyperParameterTuningJobConfig (HyperParameterTuningJobConfig'),
    newHyperParameterTuningJobConfig,

    -- ** HyperParameterTuningJobObjective
    HyperParameterTuningJobObjective (HyperParameterTuningJobObjective'),
    newHyperParameterTuningJobObjective,

    -- ** HyperParameterTuningJobSummary
    HyperParameterTuningJobSummary (HyperParameterTuningJobSummary'),
    newHyperParameterTuningJobSummary,

    -- ** HyperParameterTuningJobWarmStartConfig
    HyperParameterTuningJobWarmStartConfig (HyperParameterTuningJobWarmStartConfig'),
    newHyperParameterTuningJobWarmStartConfig,

    -- ** Image
    Image (Image'),
    newImage,

    -- ** ImageConfig
    ImageConfig (ImageConfig'),
    newImageConfig,

    -- ** ImageVersion
    ImageVersion (ImageVersion'),
    newImageVersion,

    -- ** InferenceExecutionConfig
    InferenceExecutionConfig (InferenceExecutionConfig'),
    newInferenceExecutionConfig,

    -- ** InferenceSpecification
    InferenceSpecification (InferenceSpecification'),
    newInferenceSpecification,

    -- ** InputConfig
    InputConfig (InputConfig'),
    newInputConfig,

    -- ** IntegerParameterRange
    IntegerParameterRange (IntegerParameterRange'),
    newIntegerParameterRange,

    -- ** IntegerParameterRangeSpecification
    IntegerParameterRangeSpecification (IntegerParameterRangeSpecification'),
    newIntegerParameterRangeSpecification,

    -- ** JupyterServerAppSettings
    JupyterServerAppSettings (JupyterServerAppSettings'),
    newJupyterServerAppSettings,

    -- ** KernelGatewayAppSettings
    KernelGatewayAppSettings (KernelGatewayAppSettings'),
    newKernelGatewayAppSettings,

    -- ** KernelGatewayImageConfig
    KernelGatewayImageConfig (KernelGatewayImageConfig'),
    newKernelGatewayImageConfig,

    -- ** KernelSpec
    KernelSpec (KernelSpec'),
    newKernelSpec,

    -- ** LabelCounters
    LabelCounters (LabelCounters'),
    newLabelCounters,

    -- ** LabelCountersForWorkteam
    LabelCountersForWorkteam (LabelCountersForWorkteam'),
    newLabelCountersForWorkteam,

    -- ** LabelingJobAlgorithmsConfig
    LabelingJobAlgorithmsConfig (LabelingJobAlgorithmsConfig'),
    newLabelingJobAlgorithmsConfig,

    -- ** LabelingJobDataAttributes
    LabelingJobDataAttributes (LabelingJobDataAttributes'),
    newLabelingJobDataAttributes,

    -- ** LabelingJobDataSource
    LabelingJobDataSource (LabelingJobDataSource'),
    newLabelingJobDataSource,

    -- ** LabelingJobForWorkteamSummary
    LabelingJobForWorkteamSummary (LabelingJobForWorkteamSummary'),
    newLabelingJobForWorkteamSummary,

    -- ** LabelingJobInputConfig
    LabelingJobInputConfig (LabelingJobInputConfig'),
    newLabelingJobInputConfig,

    -- ** LabelingJobOutput
    LabelingJobOutput (LabelingJobOutput'),
    newLabelingJobOutput,

    -- ** LabelingJobOutputConfig
    LabelingJobOutputConfig (LabelingJobOutputConfig'),
    newLabelingJobOutputConfig,

    -- ** LabelingJobResourceConfig
    LabelingJobResourceConfig (LabelingJobResourceConfig'),
    newLabelingJobResourceConfig,

    -- ** LabelingJobS3DataSource
    LabelingJobS3DataSource (LabelingJobS3DataSource'),
    newLabelingJobS3DataSource,

    -- ** LabelingJobSnsDataSource
    LabelingJobSnsDataSource (LabelingJobSnsDataSource'),
    newLabelingJobSnsDataSource,

    -- ** LabelingJobStoppingConditions
    LabelingJobStoppingConditions (LabelingJobStoppingConditions'),
    newLabelingJobStoppingConditions,

    -- ** LabelingJobSummary
    LabelingJobSummary (LabelingJobSummary'),
    newLabelingJobSummary,

    -- ** LambdaStepMetadata
    LambdaStepMetadata (LambdaStepMetadata'),
    newLambdaStepMetadata,

    -- ** MemberDefinition
    MemberDefinition (MemberDefinition'),
    newMemberDefinition,

    -- ** MetadataProperties
    MetadataProperties (MetadataProperties'),
    newMetadataProperties,

    -- ** MetricData
    MetricData (MetricData'),
    newMetricData,

    -- ** MetricDatum
    MetricDatum (MetricDatum'),
    newMetricDatum,

    -- ** MetricDefinition
    MetricDefinition (MetricDefinition'),
    newMetricDefinition,

    -- ** MetricsSource
    MetricsSource (MetricsSource'),
    newMetricsSource,

    -- ** ModelArtifacts
    ModelArtifacts (ModelArtifacts'),
    newModelArtifacts,

    -- ** ModelBiasAppSpecification
    ModelBiasAppSpecification (ModelBiasAppSpecification'),
    newModelBiasAppSpecification,

    -- ** ModelBiasBaselineConfig
    ModelBiasBaselineConfig (ModelBiasBaselineConfig'),
    newModelBiasBaselineConfig,

    -- ** ModelBiasJobInput
    ModelBiasJobInput (ModelBiasJobInput'),
    newModelBiasJobInput,

    -- ** ModelClientConfig
    ModelClientConfig (ModelClientConfig'),
    newModelClientConfig,

    -- ** ModelDataQuality
    ModelDataQuality (ModelDataQuality'),
    newModelDataQuality,

    -- ** ModelDeployConfig
    ModelDeployConfig (ModelDeployConfig'),
    newModelDeployConfig,

    -- ** ModelDeployResult
    ModelDeployResult (ModelDeployResult'),
    newModelDeployResult,

    -- ** ModelDigests
    ModelDigests (ModelDigests'),
    newModelDigests,

    -- ** ModelExplainabilityAppSpecification
    ModelExplainabilityAppSpecification (ModelExplainabilityAppSpecification'),
    newModelExplainabilityAppSpecification,

    -- ** ModelExplainabilityBaselineConfig
    ModelExplainabilityBaselineConfig (ModelExplainabilityBaselineConfig'),
    newModelExplainabilityBaselineConfig,

    -- ** ModelExplainabilityJobInput
    ModelExplainabilityJobInput (ModelExplainabilityJobInput'),
    newModelExplainabilityJobInput,

    -- ** ModelMetrics
    ModelMetrics (ModelMetrics'),
    newModelMetrics,

    -- ** ModelPackage
    ModelPackage (ModelPackage'),
    newModelPackage,

    -- ** ModelPackageContainerDefinition
    ModelPackageContainerDefinition (ModelPackageContainerDefinition'),
    newModelPackageContainerDefinition,

    -- ** ModelPackageGroup
    ModelPackageGroup (ModelPackageGroup'),
    newModelPackageGroup,

    -- ** ModelPackageGroupSummary
    ModelPackageGroupSummary (ModelPackageGroupSummary'),
    newModelPackageGroupSummary,

    -- ** ModelPackageStatusDetails
    ModelPackageStatusDetails (ModelPackageStatusDetails'),
    newModelPackageStatusDetails,

    -- ** ModelPackageStatusItem
    ModelPackageStatusItem (ModelPackageStatusItem'),
    newModelPackageStatusItem,

    -- ** ModelPackageSummary
    ModelPackageSummary (ModelPackageSummary'),
    newModelPackageSummary,

    -- ** ModelPackageValidationProfile
    ModelPackageValidationProfile (ModelPackageValidationProfile'),
    newModelPackageValidationProfile,

    -- ** ModelPackageValidationSpecification
    ModelPackageValidationSpecification (ModelPackageValidationSpecification'),
    newModelPackageValidationSpecification,

    -- ** ModelQuality
    ModelQuality (ModelQuality'),
    newModelQuality,

    -- ** ModelQualityAppSpecification
    ModelQualityAppSpecification (ModelQualityAppSpecification'),
    newModelQualityAppSpecification,

    -- ** ModelQualityBaselineConfig
    ModelQualityBaselineConfig (ModelQualityBaselineConfig'),
    newModelQualityBaselineConfig,

    -- ** ModelQualityJobInput
    ModelQualityJobInput (ModelQualityJobInput'),
    newModelQualityJobInput,

    -- ** ModelStepMetadata
    ModelStepMetadata (ModelStepMetadata'),
    newModelStepMetadata,

    -- ** ModelSummary
    ModelSummary (ModelSummary'),
    newModelSummary,

    -- ** MonitoringAppSpecification
    MonitoringAppSpecification (MonitoringAppSpecification'),
    newMonitoringAppSpecification,

    -- ** MonitoringBaselineConfig
    MonitoringBaselineConfig (MonitoringBaselineConfig'),
    newMonitoringBaselineConfig,

    -- ** MonitoringClusterConfig
    MonitoringClusterConfig (MonitoringClusterConfig'),
    newMonitoringClusterConfig,

    -- ** MonitoringConstraintsResource
    MonitoringConstraintsResource (MonitoringConstraintsResource'),
    newMonitoringConstraintsResource,

    -- ** MonitoringExecutionSummary
    MonitoringExecutionSummary (MonitoringExecutionSummary'),
    newMonitoringExecutionSummary,

    -- ** MonitoringGroundTruthS3Input
    MonitoringGroundTruthS3Input (MonitoringGroundTruthS3Input'),
    newMonitoringGroundTruthS3Input,

    -- ** MonitoringInput
    MonitoringInput (MonitoringInput'),
    newMonitoringInput,

    -- ** MonitoringJobDefinition
    MonitoringJobDefinition (MonitoringJobDefinition'),
    newMonitoringJobDefinition,

    -- ** MonitoringJobDefinitionSummary
    MonitoringJobDefinitionSummary (MonitoringJobDefinitionSummary'),
    newMonitoringJobDefinitionSummary,

    -- ** MonitoringNetworkConfig
    MonitoringNetworkConfig (MonitoringNetworkConfig'),
    newMonitoringNetworkConfig,

    -- ** MonitoringOutput
    MonitoringOutput (MonitoringOutput'),
    newMonitoringOutput,

    -- ** MonitoringOutputConfig
    MonitoringOutputConfig (MonitoringOutputConfig'),
    newMonitoringOutputConfig,

    -- ** MonitoringResources
    MonitoringResources (MonitoringResources'),
    newMonitoringResources,

    -- ** MonitoringS3Output
    MonitoringS3Output (MonitoringS3Output'),
    newMonitoringS3Output,

    -- ** MonitoringSchedule
    MonitoringSchedule (MonitoringSchedule'),
    newMonitoringSchedule,

    -- ** MonitoringScheduleConfig
    MonitoringScheduleConfig (MonitoringScheduleConfig'),
    newMonitoringScheduleConfig,

    -- ** MonitoringScheduleSummary
    MonitoringScheduleSummary (MonitoringScheduleSummary'),
    newMonitoringScheduleSummary,

    -- ** MonitoringStatisticsResource
    MonitoringStatisticsResource (MonitoringStatisticsResource'),
    newMonitoringStatisticsResource,

    -- ** MonitoringStoppingCondition
    MonitoringStoppingCondition (MonitoringStoppingCondition'),
    newMonitoringStoppingCondition,

    -- ** MultiModelConfig
    MultiModelConfig (MultiModelConfig'),
    newMultiModelConfig,

    -- ** NeoVpcConfig
    NeoVpcConfig (NeoVpcConfig'),
    newNeoVpcConfig,

    -- ** NestedFilters
    NestedFilters (NestedFilters'),
    newNestedFilters,

    -- ** NetworkConfig
    NetworkConfig (NetworkConfig'),
    newNetworkConfig,

    -- ** NotebookInstanceLifecycleConfigSummary
    NotebookInstanceLifecycleConfigSummary (NotebookInstanceLifecycleConfigSummary'),
    newNotebookInstanceLifecycleConfigSummary,

    -- ** NotebookInstanceLifecycleHook
    NotebookInstanceLifecycleHook (NotebookInstanceLifecycleHook'),
    newNotebookInstanceLifecycleHook,

    -- ** NotebookInstanceSummary
    NotebookInstanceSummary (NotebookInstanceSummary'),
    newNotebookInstanceSummary,

    -- ** NotificationConfiguration
    NotificationConfiguration (NotificationConfiguration'),
    newNotificationConfiguration,

    -- ** ObjectiveStatusCounters
    ObjectiveStatusCounters (ObjectiveStatusCounters'),
    newObjectiveStatusCounters,

    -- ** OfflineStoreConfig
    OfflineStoreConfig (OfflineStoreConfig'),
    newOfflineStoreConfig,

    -- ** OfflineStoreStatus
    OfflineStoreStatus (OfflineStoreStatus'),
    newOfflineStoreStatus,

    -- ** OidcConfig
    OidcConfig (OidcConfig'),
    newOidcConfig,

    -- ** OidcConfigForResponse
    OidcConfigForResponse (OidcConfigForResponse'),
    newOidcConfigForResponse,

    -- ** OidcMemberDefinition
    OidcMemberDefinition (OidcMemberDefinition'),
    newOidcMemberDefinition,

    -- ** OnlineStoreConfig
    OnlineStoreConfig (OnlineStoreConfig'),
    newOnlineStoreConfig,

    -- ** OnlineStoreSecurityConfig
    OnlineStoreSecurityConfig (OnlineStoreSecurityConfig'),
    newOnlineStoreSecurityConfig,

    -- ** OutputConfig
    OutputConfig (OutputConfig'),
    newOutputConfig,

    -- ** OutputDataConfig
    OutputDataConfig (OutputDataConfig'),
    newOutputDataConfig,

    -- ** OutputParameter
    OutputParameter (OutputParameter'),
    newOutputParameter,

    -- ** Parameter
    Parameter (Parameter'),
    newParameter,

    -- ** ParameterRange
    ParameterRange (ParameterRange'),
    newParameterRange,

    -- ** ParameterRanges
    ParameterRanges (ParameterRanges'),
    newParameterRanges,

    -- ** Parent
    Parent (Parent'),
    newParent,

    -- ** ParentHyperParameterTuningJob
    ParentHyperParameterTuningJob (ParentHyperParameterTuningJob'),
    newParentHyperParameterTuningJob,

    -- ** Pipeline
    Pipeline (Pipeline'),
    newPipeline,

    -- ** PipelineExecution
    PipelineExecution (PipelineExecution'),
    newPipelineExecution,

    -- ** PipelineExecutionStep
    PipelineExecutionStep (PipelineExecutionStep'),
    newPipelineExecutionStep,

    -- ** PipelineExecutionStepMetadata
    PipelineExecutionStepMetadata (PipelineExecutionStepMetadata'),
    newPipelineExecutionStepMetadata,

    -- ** PipelineExecutionSummary
    PipelineExecutionSummary (PipelineExecutionSummary'),
    newPipelineExecutionSummary,

    -- ** PipelineExperimentConfig
    PipelineExperimentConfig (PipelineExperimentConfig'),
    newPipelineExperimentConfig,

    -- ** PipelineSummary
    PipelineSummary (PipelineSummary'),
    newPipelineSummary,

    -- ** ProcessingClusterConfig
    ProcessingClusterConfig (ProcessingClusterConfig'),
    newProcessingClusterConfig,

    -- ** ProcessingFeatureStoreOutput
    ProcessingFeatureStoreOutput (ProcessingFeatureStoreOutput'),
    newProcessingFeatureStoreOutput,

    -- ** ProcessingInput
    ProcessingInput (ProcessingInput'),
    newProcessingInput,

    -- ** ProcessingJob
    ProcessingJob (ProcessingJob'),
    newProcessingJob,

    -- ** ProcessingJobStepMetadata
    ProcessingJobStepMetadata (ProcessingJobStepMetadata'),
    newProcessingJobStepMetadata,

    -- ** ProcessingJobSummary
    ProcessingJobSummary (ProcessingJobSummary'),
    newProcessingJobSummary,

    -- ** ProcessingOutput
    ProcessingOutput (ProcessingOutput'),
    newProcessingOutput,

    -- ** ProcessingOutputConfig
    ProcessingOutputConfig (ProcessingOutputConfig'),
    newProcessingOutputConfig,

    -- ** ProcessingResources
    ProcessingResources (ProcessingResources'),
    newProcessingResources,

    -- ** ProcessingS3Input
    ProcessingS3Input (ProcessingS3Input'),
    newProcessingS3Input,

    -- ** ProcessingS3Output
    ProcessingS3Output (ProcessingS3Output'),
    newProcessingS3Output,

    -- ** ProcessingStoppingCondition
    ProcessingStoppingCondition (ProcessingStoppingCondition'),
    newProcessingStoppingCondition,

    -- ** ProductionVariant
    ProductionVariant (ProductionVariant'),
    newProductionVariant,

    -- ** ProductionVariantCoreDumpConfig
    ProductionVariantCoreDumpConfig (ProductionVariantCoreDumpConfig'),
    newProductionVariantCoreDumpConfig,

    -- ** ProductionVariantSummary
    ProductionVariantSummary (ProductionVariantSummary'),
    newProductionVariantSummary,

    -- ** ProfilerConfig
    ProfilerConfig (ProfilerConfig'),
    newProfilerConfig,

    -- ** ProfilerConfigForUpdate
    ProfilerConfigForUpdate (ProfilerConfigForUpdate'),
    newProfilerConfigForUpdate,

    -- ** ProfilerRuleConfiguration
    ProfilerRuleConfiguration (ProfilerRuleConfiguration'),
    newProfilerRuleConfiguration,

    -- ** ProfilerRuleEvaluationStatus
    ProfilerRuleEvaluationStatus (ProfilerRuleEvaluationStatus'),
    newProfilerRuleEvaluationStatus,

    -- ** Project
    Project (Project'),
    newProject,

    -- ** ProjectSummary
    ProjectSummary (ProjectSummary'),
    newProjectSummary,

    -- ** PropertyNameQuery
    PropertyNameQuery (PropertyNameQuery'),
    newPropertyNameQuery,

    -- ** PropertyNameSuggestion
    PropertyNameSuggestion (PropertyNameSuggestion'),
    newPropertyNameSuggestion,

    -- ** ProvisioningParameter
    ProvisioningParameter (ProvisioningParameter'),
    newProvisioningParameter,

    -- ** PublicWorkforceTaskPrice
    PublicWorkforceTaskPrice (PublicWorkforceTaskPrice'),
    newPublicWorkforceTaskPrice,

    -- ** RedshiftDatasetDefinition
    RedshiftDatasetDefinition (RedshiftDatasetDefinition'),
    newRedshiftDatasetDefinition,

    -- ** RegisterModelStepMetadata
    RegisterModelStepMetadata (RegisterModelStepMetadata'),
    newRegisterModelStepMetadata,

    -- ** RenderableTask
    RenderableTask (RenderableTask'),
    newRenderableTask,

    -- ** RenderingError
    RenderingError (RenderingError'),
    newRenderingError,

    -- ** RepositoryAuthConfig
    RepositoryAuthConfig (RepositoryAuthConfig'),
    newRepositoryAuthConfig,

    -- ** ResolvedAttributes
    ResolvedAttributes (ResolvedAttributes'),
    newResolvedAttributes,

    -- ** ResourceConfig
    ResourceConfig (ResourceConfig'),
    newResourceConfig,

    -- ** ResourceLimits
    ResourceLimits (ResourceLimits'),
    newResourceLimits,

    -- ** ResourceSpec
    ResourceSpec (ResourceSpec'),
    newResourceSpec,

    -- ** RetentionPolicy
    RetentionPolicy (RetentionPolicy'),
    newRetentionPolicy,

    -- ** RetryStrategy
    RetryStrategy (RetryStrategy'),
    newRetryStrategy,

    -- ** S3DataSource
    S3DataSource (S3DataSource'),
    newS3DataSource,

    -- ** S3StorageConfig
    S3StorageConfig (S3StorageConfig'),
    newS3StorageConfig,

    -- ** ScheduleConfig
    ScheduleConfig (ScheduleConfig'),
    newScheduleConfig,

    -- ** SearchExpression
    SearchExpression (SearchExpression'),
    newSearchExpression,

    -- ** SearchRecord
    SearchRecord (SearchRecord'),
    newSearchRecord,

    -- ** SecondaryStatusTransition
    SecondaryStatusTransition (SecondaryStatusTransition'),
    newSecondaryStatusTransition,

    -- ** ServiceCatalogProvisionedProductDetails
    ServiceCatalogProvisionedProductDetails (ServiceCatalogProvisionedProductDetails'),
    newServiceCatalogProvisionedProductDetails,

    -- ** ServiceCatalogProvisioningDetails
    ServiceCatalogProvisioningDetails (ServiceCatalogProvisioningDetails'),
    newServiceCatalogProvisioningDetails,

    -- ** SharingSettings
    SharingSettings (SharingSettings'),
    newSharingSettings,

    -- ** ShuffleConfig
    ShuffleConfig (ShuffleConfig'),
    newShuffleConfig,

    -- ** SourceAlgorithm
    SourceAlgorithm (SourceAlgorithm'),
    newSourceAlgorithm,

    -- ** SourceAlgorithmSpecification
    SourceAlgorithmSpecification (SourceAlgorithmSpecification'),
    newSourceAlgorithmSpecification,

    -- ** SourceIpConfig
    SourceIpConfig (SourceIpConfig'),
    newSourceIpConfig,

    -- ** StoppingCondition
    StoppingCondition (StoppingCondition'),
    newStoppingCondition,

    -- ** StudioLifecycleConfigDetails
    StudioLifecycleConfigDetails (StudioLifecycleConfigDetails'),
    newStudioLifecycleConfigDetails,

    -- ** SubscribedWorkteam
    SubscribedWorkteam (SubscribedWorkteam'),
    newSubscribedWorkteam,

    -- ** SuggestionQuery
    SuggestionQuery (SuggestionQuery'),
    newSuggestionQuery,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TargetPlatform
    TargetPlatform (TargetPlatform'),
    newTargetPlatform,

    -- ** TensorBoardAppSettings
    TensorBoardAppSettings (TensorBoardAppSettings'),
    newTensorBoardAppSettings,

    -- ** TensorBoardOutputConfig
    TensorBoardOutputConfig (TensorBoardOutputConfig'),
    newTensorBoardOutputConfig,

    -- ** TrafficRoutingConfig
    TrafficRoutingConfig (TrafficRoutingConfig'),
    newTrafficRoutingConfig,

    -- ** TrainingJob
    TrainingJob (TrainingJob'),
    newTrainingJob,

    -- ** TrainingJobDefinition
    TrainingJobDefinition (TrainingJobDefinition'),
    newTrainingJobDefinition,

    -- ** TrainingJobStatusCounters
    TrainingJobStatusCounters (TrainingJobStatusCounters'),
    newTrainingJobStatusCounters,

    -- ** TrainingJobStepMetadata
    TrainingJobStepMetadata (TrainingJobStepMetadata'),
    newTrainingJobStepMetadata,

    -- ** TrainingJobSummary
    TrainingJobSummary (TrainingJobSummary'),
    newTrainingJobSummary,

    -- ** TrainingSpecification
    TrainingSpecification (TrainingSpecification'),
    newTrainingSpecification,

    -- ** TransformDataSource
    TransformDataSource (TransformDataSource'),
    newTransformDataSource,

    -- ** TransformInput
    TransformInput (TransformInput'),
    newTransformInput,

    -- ** TransformJob
    TransformJob (TransformJob'),
    newTransformJob,

    -- ** TransformJobDefinition
    TransformJobDefinition (TransformJobDefinition'),
    newTransformJobDefinition,

    -- ** TransformJobStepMetadata
    TransformJobStepMetadata (TransformJobStepMetadata'),
    newTransformJobStepMetadata,

    -- ** TransformJobSummary
    TransformJobSummary (TransformJobSummary'),
    newTransformJobSummary,

    -- ** TransformOutput
    TransformOutput (TransformOutput'),
    newTransformOutput,

    -- ** TransformResources
    TransformResources (TransformResources'),
    newTransformResources,

    -- ** TransformS3DataSource
    TransformS3DataSource (TransformS3DataSource'),
    newTransformS3DataSource,

    -- ** Trial
    Trial (Trial'),
    newTrial,

    -- ** TrialComponent
    TrialComponent (TrialComponent'),
    newTrialComponent,

    -- ** TrialComponentArtifact
    TrialComponentArtifact (TrialComponentArtifact'),
    newTrialComponentArtifact,

    -- ** TrialComponentMetricSummary
    TrialComponentMetricSummary (TrialComponentMetricSummary'),
    newTrialComponentMetricSummary,

    -- ** TrialComponentParameterValue
    TrialComponentParameterValue (TrialComponentParameterValue'),
    newTrialComponentParameterValue,

    -- ** TrialComponentSimpleSummary
    TrialComponentSimpleSummary (TrialComponentSimpleSummary'),
    newTrialComponentSimpleSummary,

    -- ** TrialComponentSource
    TrialComponentSource (TrialComponentSource'),
    newTrialComponentSource,

    -- ** TrialComponentSourceDetail
    TrialComponentSourceDetail (TrialComponentSourceDetail'),
    newTrialComponentSourceDetail,

    -- ** TrialComponentStatus
    TrialComponentStatus (TrialComponentStatus'),
    newTrialComponentStatus,

    -- ** TrialComponentSummary
    TrialComponentSummary (TrialComponentSummary'),
    newTrialComponentSummary,

    -- ** TrialSource
    TrialSource (TrialSource'),
    newTrialSource,

    -- ** TrialSummary
    TrialSummary (TrialSummary'),
    newTrialSummary,

    -- ** TuningJobCompletionCriteria
    TuningJobCompletionCriteria (TuningJobCompletionCriteria'),
    newTuningJobCompletionCriteria,

    -- ** TuningJobStepMetaData
    TuningJobStepMetaData (TuningJobStepMetaData'),
    newTuningJobStepMetaData,

    -- ** USD
    USD (USD'),
    newUSD,

    -- ** UiConfig
    UiConfig (UiConfig'),
    newUiConfig,

    -- ** UiTemplate
    UiTemplate (UiTemplate'),
    newUiTemplate,

    -- ** UiTemplateInfo
    UiTemplateInfo (UiTemplateInfo'),
    newUiTemplateInfo,

    -- ** UserContext
    UserContext (UserContext'),
    newUserContext,

    -- ** UserProfileDetails
    UserProfileDetails (UserProfileDetails'),
    newUserProfileDetails,

    -- ** UserSettings
    UserSettings (UserSettings'),
    newUserSettings,

    -- ** VariantProperty
    VariantProperty (VariantProperty'),
    newVariantProperty,

    -- ** VpcConfig
    VpcConfig (VpcConfig'),
    newVpcConfig,

    -- ** Workforce
    Workforce (Workforce'),
    newWorkforce,

    -- ** Workteam
    Workteam (Workteam'),
    newWorkteam,
  )
where

import Network.AWS.SageMaker.AddAssociation
import Network.AWS.SageMaker.AddTags
import Network.AWS.SageMaker.AssociateTrialComponent
import Network.AWS.SageMaker.CreateAction
import Network.AWS.SageMaker.CreateAlgorithm
import Network.AWS.SageMaker.CreateApp
import Network.AWS.SageMaker.CreateAppImageConfig
import Network.AWS.SageMaker.CreateArtifact
import Network.AWS.SageMaker.CreateAutoMLJob
import Network.AWS.SageMaker.CreateCodeRepository
import Network.AWS.SageMaker.CreateCompilationJob
import Network.AWS.SageMaker.CreateContext
import Network.AWS.SageMaker.CreateDataQualityJobDefinition
import Network.AWS.SageMaker.CreateDeviceFleet
import Network.AWS.SageMaker.CreateDomain
import Network.AWS.SageMaker.CreateEdgePackagingJob
import Network.AWS.SageMaker.CreateEndpoint
import Network.AWS.SageMaker.CreateEndpointConfig
import Network.AWS.SageMaker.CreateExperiment
import Network.AWS.SageMaker.CreateFeatureGroup
import Network.AWS.SageMaker.CreateFlowDefinition
import Network.AWS.SageMaker.CreateHumanTaskUi
import Network.AWS.SageMaker.CreateHyperParameterTuningJob
import Network.AWS.SageMaker.CreateImage
import Network.AWS.SageMaker.CreateImageVersion
import Network.AWS.SageMaker.CreateLabelingJob
import Network.AWS.SageMaker.CreateModel
import Network.AWS.SageMaker.CreateModelBiasJobDefinition
import Network.AWS.SageMaker.CreateModelExplainabilityJobDefinition
import Network.AWS.SageMaker.CreateModelPackage
import Network.AWS.SageMaker.CreateModelPackageGroup
import Network.AWS.SageMaker.CreateModelQualityJobDefinition
import Network.AWS.SageMaker.CreateMonitoringSchedule
import Network.AWS.SageMaker.CreateNotebookInstance
import Network.AWS.SageMaker.CreateNotebookInstanceLifecycleConfig
import Network.AWS.SageMaker.CreatePipeline
import Network.AWS.SageMaker.CreatePresignedDomainUrl
import Network.AWS.SageMaker.CreatePresignedNotebookInstanceUrl
import Network.AWS.SageMaker.CreateProcessingJob
import Network.AWS.SageMaker.CreateProject
import Network.AWS.SageMaker.CreateStudioLifecycleConfig
import Network.AWS.SageMaker.CreateTrainingJob
import Network.AWS.SageMaker.CreateTransformJob
import Network.AWS.SageMaker.CreateTrial
import Network.AWS.SageMaker.CreateTrialComponent
import Network.AWS.SageMaker.CreateUserProfile
import Network.AWS.SageMaker.CreateWorkforce
import Network.AWS.SageMaker.CreateWorkteam
import Network.AWS.SageMaker.DeleteAction
import Network.AWS.SageMaker.DeleteAlgorithm
import Network.AWS.SageMaker.DeleteApp
import Network.AWS.SageMaker.DeleteAppImageConfig
import Network.AWS.SageMaker.DeleteArtifact
import Network.AWS.SageMaker.DeleteAssociation
import Network.AWS.SageMaker.DeleteCodeRepository
import Network.AWS.SageMaker.DeleteContext
import Network.AWS.SageMaker.DeleteDataQualityJobDefinition
import Network.AWS.SageMaker.DeleteDeviceFleet
import Network.AWS.SageMaker.DeleteDomain
import Network.AWS.SageMaker.DeleteEndpoint
import Network.AWS.SageMaker.DeleteEndpointConfig
import Network.AWS.SageMaker.DeleteExperiment
import Network.AWS.SageMaker.DeleteFeatureGroup
import Network.AWS.SageMaker.DeleteFlowDefinition
import Network.AWS.SageMaker.DeleteHumanTaskUi
import Network.AWS.SageMaker.DeleteImage
import Network.AWS.SageMaker.DeleteImageVersion
import Network.AWS.SageMaker.DeleteModel
import Network.AWS.SageMaker.DeleteModelBiasJobDefinition
import Network.AWS.SageMaker.DeleteModelExplainabilityJobDefinition
import Network.AWS.SageMaker.DeleteModelPackage
import Network.AWS.SageMaker.DeleteModelPackageGroup
import Network.AWS.SageMaker.DeleteModelPackageGroupPolicy
import Network.AWS.SageMaker.DeleteModelQualityJobDefinition
import Network.AWS.SageMaker.DeleteMonitoringSchedule
import Network.AWS.SageMaker.DeleteNotebookInstance
import Network.AWS.SageMaker.DeleteNotebookInstanceLifecycleConfig
import Network.AWS.SageMaker.DeletePipeline
import Network.AWS.SageMaker.DeleteProject
import Network.AWS.SageMaker.DeleteStudioLifecycleConfig
import Network.AWS.SageMaker.DeleteTags
import Network.AWS.SageMaker.DeleteTrial
import Network.AWS.SageMaker.DeleteTrialComponent
import Network.AWS.SageMaker.DeleteUserProfile
import Network.AWS.SageMaker.DeleteWorkforce
import Network.AWS.SageMaker.DeleteWorkteam
import Network.AWS.SageMaker.DeregisterDevices
import Network.AWS.SageMaker.DescribeAction
import Network.AWS.SageMaker.DescribeAlgorithm
import Network.AWS.SageMaker.DescribeApp
import Network.AWS.SageMaker.DescribeAppImageConfig
import Network.AWS.SageMaker.DescribeArtifact
import Network.AWS.SageMaker.DescribeAutoMLJob
import Network.AWS.SageMaker.DescribeCodeRepository
import Network.AWS.SageMaker.DescribeCompilationJob
import Network.AWS.SageMaker.DescribeContext
import Network.AWS.SageMaker.DescribeDataQualityJobDefinition
import Network.AWS.SageMaker.DescribeDevice
import Network.AWS.SageMaker.DescribeDeviceFleet
import Network.AWS.SageMaker.DescribeDomain
import Network.AWS.SageMaker.DescribeEdgePackagingJob
import Network.AWS.SageMaker.DescribeEndpoint
import Network.AWS.SageMaker.DescribeEndpointConfig
import Network.AWS.SageMaker.DescribeExperiment
import Network.AWS.SageMaker.DescribeFeatureGroup
import Network.AWS.SageMaker.DescribeFlowDefinition
import Network.AWS.SageMaker.DescribeHumanTaskUi
import Network.AWS.SageMaker.DescribeHyperParameterTuningJob
import Network.AWS.SageMaker.DescribeImage
import Network.AWS.SageMaker.DescribeImageVersion
import Network.AWS.SageMaker.DescribeLabelingJob
import Network.AWS.SageMaker.DescribeModel
import Network.AWS.SageMaker.DescribeModelBiasJobDefinition
import Network.AWS.SageMaker.DescribeModelExplainabilityJobDefinition
import Network.AWS.SageMaker.DescribeModelPackage
import Network.AWS.SageMaker.DescribeModelPackageGroup
import Network.AWS.SageMaker.DescribeModelQualityJobDefinition
import Network.AWS.SageMaker.DescribeMonitoringSchedule
import Network.AWS.SageMaker.DescribeNotebookInstance
import Network.AWS.SageMaker.DescribeNotebookInstanceLifecycleConfig
import Network.AWS.SageMaker.DescribePipeline
import Network.AWS.SageMaker.DescribePipelineDefinitionForExecution
import Network.AWS.SageMaker.DescribePipelineExecution
import Network.AWS.SageMaker.DescribeProcessingJob
import Network.AWS.SageMaker.DescribeProject
import Network.AWS.SageMaker.DescribeStudioLifecycleConfig
import Network.AWS.SageMaker.DescribeSubscribedWorkteam
import Network.AWS.SageMaker.DescribeTrainingJob
import Network.AWS.SageMaker.DescribeTransformJob
import Network.AWS.SageMaker.DescribeTrial
import Network.AWS.SageMaker.DescribeTrialComponent
import Network.AWS.SageMaker.DescribeUserProfile
import Network.AWS.SageMaker.DescribeWorkforce
import Network.AWS.SageMaker.DescribeWorkteam
import Network.AWS.SageMaker.DisableSagemakerServicecatalogPortfolio
import Network.AWS.SageMaker.DisassociateTrialComponent
import Network.AWS.SageMaker.EnableSagemakerServicecatalogPortfolio
import Network.AWS.SageMaker.GetDeviceFleetReport
import Network.AWS.SageMaker.GetModelPackageGroupPolicy
import Network.AWS.SageMaker.GetSagemakerServicecatalogPortfolioStatus
import Network.AWS.SageMaker.GetSearchSuggestions
import Network.AWS.SageMaker.Lens
import Network.AWS.SageMaker.ListActions
import Network.AWS.SageMaker.ListAlgorithms
import Network.AWS.SageMaker.ListAppImageConfigs
import Network.AWS.SageMaker.ListApps
import Network.AWS.SageMaker.ListArtifacts
import Network.AWS.SageMaker.ListAssociations
import Network.AWS.SageMaker.ListAutoMLJobs
import Network.AWS.SageMaker.ListCandidatesForAutoMLJob
import Network.AWS.SageMaker.ListCodeRepositories
import Network.AWS.SageMaker.ListCompilationJobs
import Network.AWS.SageMaker.ListContexts
import Network.AWS.SageMaker.ListDataQualityJobDefinitions
import Network.AWS.SageMaker.ListDeviceFleets
import Network.AWS.SageMaker.ListDevices
import Network.AWS.SageMaker.ListDomains
import Network.AWS.SageMaker.ListEdgePackagingJobs
import Network.AWS.SageMaker.ListEndpointConfigs
import Network.AWS.SageMaker.ListEndpoints
import Network.AWS.SageMaker.ListExperiments
import Network.AWS.SageMaker.ListFeatureGroups
import Network.AWS.SageMaker.ListFlowDefinitions
import Network.AWS.SageMaker.ListHumanTaskUis
import Network.AWS.SageMaker.ListHyperParameterTuningJobs
import Network.AWS.SageMaker.ListImageVersions
import Network.AWS.SageMaker.ListImages
import Network.AWS.SageMaker.ListLabelingJobs
import Network.AWS.SageMaker.ListLabelingJobsForWorkteam
import Network.AWS.SageMaker.ListModelBiasJobDefinitions
import Network.AWS.SageMaker.ListModelExplainabilityJobDefinitions
import Network.AWS.SageMaker.ListModelPackageGroups
import Network.AWS.SageMaker.ListModelPackages
import Network.AWS.SageMaker.ListModelQualityJobDefinitions
import Network.AWS.SageMaker.ListModels
import Network.AWS.SageMaker.ListMonitoringExecutions
import Network.AWS.SageMaker.ListMonitoringSchedules
import Network.AWS.SageMaker.ListNotebookInstanceLifecycleConfigs
import Network.AWS.SageMaker.ListNotebookInstances
import Network.AWS.SageMaker.ListPipelineExecutionSteps
import Network.AWS.SageMaker.ListPipelineExecutions
import Network.AWS.SageMaker.ListPipelineParametersForExecution
import Network.AWS.SageMaker.ListPipelines
import Network.AWS.SageMaker.ListProcessingJobs
import Network.AWS.SageMaker.ListProjects
import Network.AWS.SageMaker.ListStudioLifecycleConfigs
import Network.AWS.SageMaker.ListSubscribedWorkteams
import Network.AWS.SageMaker.ListTags
import Network.AWS.SageMaker.ListTrainingJobs
import Network.AWS.SageMaker.ListTrainingJobsForHyperParameterTuningJob
import Network.AWS.SageMaker.ListTransformJobs
import Network.AWS.SageMaker.ListTrialComponents
import Network.AWS.SageMaker.ListTrials
import Network.AWS.SageMaker.ListUserProfiles
import Network.AWS.SageMaker.ListWorkforces
import Network.AWS.SageMaker.ListWorkteams
import Network.AWS.SageMaker.PutModelPackageGroupPolicy
import Network.AWS.SageMaker.RegisterDevices
import Network.AWS.SageMaker.RenderUiTemplate
import Network.AWS.SageMaker.RetryPipelineExecution
import Network.AWS.SageMaker.Search
import Network.AWS.SageMaker.SendPipelineExecutionStepFailure
import Network.AWS.SageMaker.SendPipelineExecutionStepSuccess
import Network.AWS.SageMaker.StartMonitoringSchedule
import Network.AWS.SageMaker.StartNotebookInstance
import Network.AWS.SageMaker.StartPipelineExecution
import Network.AWS.SageMaker.StopAutoMLJob
import Network.AWS.SageMaker.StopCompilationJob
import Network.AWS.SageMaker.StopEdgePackagingJob
import Network.AWS.SageMaker.StopHyperParameterTuningJob
import Network.AWS.SageMaker.StopLabelingJob
import Network.AWS.SageMaker.StopMonitoringSchedule
import Network.AWS.SageMaker.StopNotebookInstance
import Network.AWS.SageMaker.StopPipelineExecution
import Network.AWS.SageMaker.StopProcessingJob
import Network.AWS.SageMaker.StopTrainingJob
import Network.AWS.SageMaker.StopTransformJob
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.UpdateAction
import Network.AWS.SageMaker.UpdateAppImageConfig
import Network.AWS.SageMaker.UpdateArtifact
import Network.AWS.SageMaker.UpdateCodeRepository
import Network.AWS.SageMaker.UpdateContext
import Network.AWS.SageMaker.UpdateDeviceFleet
import Network.AWS.SageMaker.UpdateDevices
import Network.AWS.SageMaker.UpdateDomain
import Network.AWS.SageMaker.UpdateEndpoint
import Network.AWS.SageMaker.UpdateEndpointWeightsAndCapacities
import Network.AWS.SageMaker.UpdateExperiment
import Network.AWS.SageMaker.UpdateImage
import Network.AWS.SageMaker.UpdateModelPackage
import Network.AWS.SageMaker.UpdateMonitoringSchedule
import Network.AWS.SageMaker.UpdateNotebookInstance
import Network.AWS.SageMaker.UpdateNotebookInstanceLifecycleConfig
import Network.AWS.SageMaker.UpdatePipeline
import Network.AWS.SageMaker.UpdatePipelineExecution
import Network.AWS.SageMaker.UpdateTrainingJob
import Network.AWS.SageMaker.UpdateTrial
import Network.AWS.SageMaker.UpdateTrialComponent
import Network.AWS.SageMaker.UpdateUserProfile
import Network.AWS.SageMaker.UpdateWorkforce
import Network.AWS.SageMaker.UpdateWorkteam
import Network.AWS.SageMaker.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SageMaker'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

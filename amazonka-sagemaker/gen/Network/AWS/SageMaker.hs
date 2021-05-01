{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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

    -- ** EndpointInService
    newEndpointInService,

    -- ** TrainingJobCompletedOrStopped
    newTrainingJobCompletedOrStopped,

    -- ** ProcessingJobCompletedOrStopped
    newProcessingJobCompletedOrStopped,

    -- ** TransformJobCompletedOrStopped
    newTransformJobCompletedOrStopped,

    -- * Operations
    -- $operations

    -- ** DisassociateTrialComponent
    DisassociateTrialComponent (DisassociateTrialComponent'),
    newDisassociateTrialComponent,
    DisassociateTrialComponentResponse (DisassociateTrialComponentResponse'),
    newDisassociateTrialComponentResponse,

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

    -- ** DescribeUserProfile
    DescribeUserProfile (DescribeUserProfile'),
    newDescribeUserProfile,
    DescribeUserProfileResponse (DescribeUserProfileResponse'),
    newDescribeUserProfileResponse,

    -- ** StopTrainingJob
    StopTrainingJob (StopTrainingJob'),
    newStopTrainingJob,
    StopTrainingJobResponse (StopTrainingJobResponse'),
    newStopTrainingJobResponse,

    -- ** CreateEndpoint
    CreateEndpoint (CreateEndpoint'),
    newCreateEndpoint,
    CreateEndpointResponse (CreateEndpointResponse'),
    newCreateEndpointResponse,

    -- ** GetSearchSuggestions
    GetSearchSuggestions (GetSearchSuggestions'),
    newGetSearchSuggestions,
    GetSearchSuggestionsResponse (GetSearchSuggestionsResponse'),
    newGetSearchSuggestionsResponse,

    -- ** DeleteAction
    DeleteAction (DeleteAction'),
    newDeleteAction,
    DeleteActionResponse (DeleteActionResponse'),
    newDeleteActionResponse,

    -- ** CreateEdgePackagingJob
    CreateEdgePackagingJob (CreateEdgePackagingJob'),
    newCreateEdgePackagingJob,
    CreateEdgePackagingJobResponse (CreateEdgePackagingJobResponse'),
    newCreateEdgePackagingJobResponse,

    -- ** DescribeEndpointConfig
    DescribeEndpointConfig (DescribeEndpointConfig'),
    newDescribeEndpointConfig,
    DescribeEndpointConfigResponse (DescribeEndpointConfigResponse'),
    newDescribeEndpointConfigResponse,

    -- ** ListModelPackages (Paginated)
    ListModelPackages (ListModelPackages'),
    newListModelPackages,
    ListModelPackagesResponse (ListModelPackagesResponse'),
    newListModelPackagesResponse,

    -- ** GetModelPackageGroupPolicy
    GetModelPackageGroupPolicy (GetModelPackageGroupPolicy'),
    newGetModelPackageGroupPolicy,
    GetModelPackageGroupPolicyResponse (GetModelPackageGroupPolicyResponse'),
    newGetModelPackageGroupPolicyResponse,

    -- ** DescribeMonitoringSchedule
    DescribeMonitoringSchedule (DescribeMonitoringSchedule'),
    newDescribeMonitoringSchedule,
    DescribeMonitoringScheduleResponse (DescribeMonitoringScheduleResponse'),
    newDescribeMonitoringScheduleResponse,

    -- ** CreateModelExplainabilityJobDefinition
    CreateModelExplainabilityJobDefinition (CreateModelExplainabilityJobDefinition'),
    newCreateModelExplainabilityJobDefinition,
    CreateModelExplainabilityJobDefinitionResponse (CreateModelExplainabilityJobDefinitionResponse'),
    newCreateModelExplainabilityJobDefinitionResponse,

    -- ** DescribeLabelingJob
    DescribeLabelingJob (DescribeLabelingJob'),
    newDescribeLabelingJob,
    DescribeLabelingJobResponse (DescribeLabelingJobResponse'),
    newDescribeLabelingJobResponse,

    -- ** CreateNotebookInstance
    CreateNotebookInstance (CreateNotebookInstance'),
    newCreateNotebookInstance,
    CreateNotebookInstanceResponse (CreateNotebookInstanceResponse'),
    newCreateNotebookInstanceResponse,

    -- ** UpdateModelPackage
    UpdateModelPackage (UpdateModelPackage'),
    newUpdateModelPackage,
    UpdateModelPackageResponse (UpdateModelPackageResponse'),
    newUpdateModelPackageResponse,

    -- ** CreateModelQualityJobDefinition
    CreateModelQualityJobDefinition (CreateModelQualityJobDefinition'),
    newCreateModelQualityJobDefinition,
    CreateModelQualityJobDefinitionResponse (CreateModelQualityJobDefinitionResponse'),
    newCreateModelQualityJobDefinitionResponse,

    -- ** DeleteModelPackage
    DeleteModelPackage (DeleteModelPackage'),
    newDeleteModelPackage,
    DeleteModelPackageResponse (DeleteModelPackageResponse'),
    newDeleteModelPackageResponse,

    -- ** ListProjects
    ListProjects (ListProjects'),
    newListProjects,
    ListProjectsResponse (ListProjectsResponse'),
    newListProjectsResponse,

    -- ** ListSubscribedWorkteams (Paginated)
    ListSubscribedWorkteams (ListSubscribedWorkteams'),
    newListSubscribedWorkteams,
    ListSubscribedWorkteamsResponse (ListSubscribedWorkteamsResponse'),
    newListSubscribedWorkteamsResponse,

    -- ** DeleteNotebookInstance
    DeleteNotebookInstance (DeleteNotebookInstance'),
    newDeleteNotebookInstance,
    DeleteNotebookInstanceResponse (DeleteNotebookInstanceResponse'),
    newDeleteNotebookInstanceResponse,

    -- ** CreateProject
    CreateProject (CreateProject'),
    newCreateProject,
    CreateProjectResponse (CreateProjectResponse'),
    newCreateProjectResponse,

    -- ** DescribeProcessingJob
    DescribeProcessingJob (DescribeProcessingJob'),
    newDescribeProcessingJob,
    DescribeProcessingJobResponse (DescribeProcessingJobResponse'),
    newDescribeProcessingJobResponse,

    -- ** ListDomains (Paginated)
    ListDomains (ListDomains'),
    newListDomains,
    ListDomainsResponse (ListDomainsResponse'),
    newListDomainsResponse,

    -- ** DeleteModelExplainabilityJobDefinition
    DeleteModelExplainabilityJobDefinition (DeleteModelExplainabilityJobDefinition'),
    newDeleteModelExplainabilityJobDefinition,
    DeleteModelExplainabilityJobDefinitionResponse (DeleteModelExplainabilityJobDefinitionResponse'),
    newDeleteModelExplainabilityJobDefinitionResponse,

    -- ** StopMonitoringSchedule
    StopMonitoringSchedule (StopMonitoringSchedule'),
    newStopMonitoringSchedule,
    StopMonitoringScheduleResponse (StopMonitoringScheduleResponse'),
    newStopMonitoringScheduleResponse,

    -- ** ListDevices (Paginated)
    ListDevices (ListDevices'),
    newListDevices,
    ListDevicesResponse (ListDevicesResponse'),
    newListDevicesResponse,

    -- ** CreateModelPackage
    CreateModelPackage (CreateModelPackage'),
    newCreateModelPackage,
    CreateModelPackageResponse (CreateModelPackageResponse'),
    newCreateModelPackageResponse,

    -- ** UpdateNotebookInstance
    UpdateNotebookInstance (UpdateNotebookInstance'),
    newUpdateNotebookInstance,
    UpdateNotebookInstanceResponse (UpdateNotebookInstanceResponse'),
    newUpdateNotebookInstanceResponse,

    -- ** StopAutoMLJob
    StopAutoMLJob (StopAutoMLJob'),
    newStopAutoMLJob,
    StopAutoMLJobResponse (StopAutoMLJobResponse'),
    newStopAutoMLJobResponse,

    -- ** DescribeAppImageConfig
    DescribeAppImageConfig (DescribeAppImageConfig'),
    newDescribeAppImageConfig,
    DescribeAppImageConfigResponse (DescribeAppImageConfigResponse'),
    newDescribeAppImageConfigResponse,

    -- ** StartMonitoringSchedule
    StartMonitoringSchedule (StartMonitoringSchedule'),
    newStartMonitoringSchedule,
    StartMonitoringScheduleResponse (StartMonitoringScheduleResponse'),
    newStartMonitoringScheduleResponse,

    -- ** StopCompilationJob
    StopCompilationJob (StopCompilationJob'),
    newStopCompilationJob,
    StopCompilationJobResponse (StopCompilationJobResponse'),
    newStopCompilationJobResponse,

    -- ** CreateTrial
    CreateTrial (CreateTrial'),
    newCreateTrial,
    CreateTrialResponse (CreateTrialResponse'),
    newCreateTrialResponse,

    -- ** GetSagemakerServicecatalogPortfolioStatus
    GetSagemakerServicecatalogPortfolioStatus (GetSagemakerServicecatalogPortfolioStatus'),
    newGetSagemakerServicecatalogPortfolioStatus,
    GetSagemakerServicecatalogPortfolioStatusResponse (GetSagemakerServicecatalogPortfolioStatusResponse'),
    newGetSagemakerServicecatalogPortfolioStatusResponse,

    -- ** UpdateCodeRepository
    UpdateCodeRepository (UpdateCodeRepository'),
    newUpdateCodeRepository,
    UpdateCodeRepositoryResponse (UpdateCodeRepositoryResponse'),
    newUpdateCodeRepositoryResponse,

    -- ** Search (Paginated)
    Search (Search'),
    newSearch,
    SearchResponse (SearchResponse'),
    newSearchResponse,

    -- ** DeleteModel
    DeleteModel (DeleteModel'),
    newDeleteModel,
    DeleteModelResponse (DeleteModelResponse'),
    newDeleteModelResponse,

    -- ** DeleteDataQualityJobDefinition
    DeleteDataQualityJobDefinition (DeleteDataQualityJobDefinition'),
    newDeleteDataQualityJobDefinition,
    DeleteDataQualityJobDefinitionResponse (DeleteDataQualityJobDefinitionResponse'),
    newDeleteDataQualityJobDefinitionResponse,

    -- ** ListImages (Paginated)
    ListImages (ListImages'),
    newListImages,
    ListImagesResponse (ListImagesResponse'),
    newListImagesResponse,

    -- ** ListTrainingJobs (Paginated)
    ListTrainingJobs (ListTrainingJobs'),
    newListTrainingJobs,
    ListTrainingJobsResponse (ListTrainingJobsResponse'),
    newListTrainingJobsResponse,

    -- ** DescribeTransformJob
    DescribeTransformJob (DescribeTransformJob'),
    newDescribeTransformJob,
    DescribeTransformJobResponse (DescribeTransformJobResponse'),
    newDescribeTransformJobResponse,

    -- ** CreatePipeline
    CreatePipeline (CreatePipeline'),
    newCreatePipeline,
    CreatePipelineResponse (CreatePipelineResponse'),
    newCreatePipelineResponse,

    -- ** CreateModelPackageGroup
    CreateModelPackageGroup (CreateModelPackageGroup'),
    newCreateModelPackageGroup,
    CreateModelPackageGroupResponse (CreateModelPackageGroupResponse'),
    newCreateModelPackageGroupResponse,

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

    -- ** ListDataQualityJobDefinitions (Paginated)
    ListDataQualityJobDefinitions (ListDataQualityJobDefinitions'),
    newListDataQualityJobDefinitions,
    ListDataQualityJobDefinitionsResponse (ListDataQualityJobDefinitionsResponse'),
    newListDataQualityJobDefinitionsResponse,

    -- ** DescribeEdgePackagingJob
    DescribeEdgePackagingJob (DescribeEdgePackagingJob'),
    newDescribeEdgePackagingJob,
    DescribeEdgePackagingJobResponse (DescribeEdgePackagingJobResponse'),
    newDescribeEdgePackagingJobResponse,

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

    -- ** DeleteCodeRepository
    DeleteCodeRepository (DeleteCodeRepository'),
    newDeleteCodeRepository,
    DeleteCodeRepositoryResponse (DeleteCodeRepositoryResponse'),
    newDeleteCodeRepositoryResponse,

    -- ** DeleteModelPackageGroupPolicy
    DeleteModelPackageGroupPolicy (DeleteModelPackageGroupPolicy'),
    newDeleteModelPackageGroupPolicy,
    DeleteModelPackageGroupPolicyResponse (DeleteModelPackageGroupPolicyResponse'),
    newDeleteModelPackageGroupPolicyResponse,

    -- ** ListUserProfiles (Paginated)
    ListUserProfiles (ListUserProfiles'),
    newListUserProfiles,
    ListUserProfilesResponse (ListUserProfilesResponse'),
    newListUserProfilesResponse,

    -- ** DescribeCompilationJob
    DescribeCompilationJob (DescribeCompilationJob'),
    newDescribeCompilationJob,
    DescribeCompilationJobResponse (DescribeCompilationJobResponse'),
    newDescribeCompilationJobResponse,

    -- ** UpdatePipeline
    UpdatePipeline (UpdatePipeline'),
    newUpdatePipeline,
    UpdatePipelineResponse (UpdatePipelineResponse'),
    newUpdatePipelineResponse,

    -- ** CreateCodeRepository
    CreateCodeRepository (CreateCodeRepository'),
    newCreateCodeRepository,
    CreateCodeRepositoryResponse (CreateCodeRepositoryResponse'),
    newCreateCodeRepositoryResponse,

    -- ** DescribeArtifact
    DescribeArtifact (DescribeArtifact'),
    newDescribeArtifact,
    DescribeArtifactResponse (DescribeArtifactResponse'),
    newDescribeArtifactResponse,

    -- ** DescribeHumanTaskUi
    DescribeHumanTaskUi (DescribeHumanTaskUi'),
    newDescribeHumanTaskUi,
    DescribeHumanTaskUiResponse (DescribeHumanTaskUiResponse'),
    newDescribeHumanTaskUiResponse,

    -- ** ListPipelineExecutionSteps (Paginated)
    ListPipelineExecutionSteps (ListPipelineExecutionSteps'),
    newListPipelineExecutionSteps,
    ListPipelineExecutionStepsResponse (ListPipelineExecutionStepsResponse'),
    newListPipelineExecutionStepsResponse,

    -- ** ListCodeRepositories (Paginated)
    ListCodeRepositories (ListCodeRepositories'),
    newListCodeRepositories,
    ListCodeRepositoriesResponse (ListCodeRepositoriesResponse'),
    newListCodeRepositoriesResponse,

    -- ** UpdateUserProfile
    UpdateUserProfile (UpdateUserProfile'),
    newUpdateUserProfile,
    UpdateUserProfileResponse (UpdateUserProfileResponse'),
    newUpdateUserProfileResponse,

    -- ** DescribeAction
    DescribeAction (DescribeAction'),
    newDescribeAction,
    DescribeActionResponse (DescribeActionResponse'),
    newDescribeActionResponse,

    -- ** StopTransformJob
    StopTransformJob (StopTransformJob'),
    newStopTransformJob,
    StopTransformJobResponse (StopTransformJobResponse'),
    newStopTransformJobResponse,

    -- ** CreateTrainingJob
    CreateTrainingJob (CreateTrainingJob'),
    newCreateTrainingJob,
    CreateTrainingJobResponse (CreateTrainingJobResponse'),
    newCreateTrainingJobResponse,

    -- ** DeleteUserProfile
    DeleteUserProfile (DeleteUserProfile'),
    newDeleteUserProfile,
    DeleteUserProfileResponse (DeleteUserProfileResponse'),
    newDeleteUserProfileResponse,

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

    -- ** CreateImage
    CreateImage (CreateImage'),
    newCreateImage,
    CreateImageResponse (CreateImageResponse'),
    newCreateImageResponse,

    -- ** DeregisterDevices
    DeregisterDevices (DeregisterDevices'),
    newDeregisterDevices,
    DeregisterDevicesResponse (DeregisterDevicesResponse'),
    newDeregisterDevicesResponse,

    -- ** CreateDataQualityJobDefinition
    CreateDataQualityJobDefinition (CreateDataQualityJobDefinition'),
    newCreateDataQualityJobDefinition,
    CreateDataQualityJobDefinitionResponse (CreateDataQualityJobDefinitionResponse'),
    newCreateDataQualityJobDefinitionResponse,

    -- ** DeletePipeline
    DeletePipeline (DeletePipeline'),
    newDeletePipeline,
    DeletePipelineResponse (DeletePipelineResponse'),
    newDeletePipelineResponse,

    -- ** CreateAppImageConfig
    CreateAppImageConfig (CreateAppImageConfig'),
    newCreateAppImageConfig,
    CreateAppImageConfigResponse (CreateAppImageConfigResponse'),
    newCreateAppImageConfigResponse,

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

    -- ** DeleteAssociation
    DeleteAssociation (DeleteAssociation'),
    newDeleteAssociation,
    DeleteAssociationResponse (DeleteAssociationResponse'),
    newDeleteAssociationResponse,

    -- ** UpdateMonitoringSchedule
    UpdateMonitoringSchedule (UpdateMonitoringSchedule'),
    newUpdateMonitoringSchedule,
    UpdateMonitoringScheduleResponse (UpdateMonitoringScheduleResponse'),
    newUpdateMonitoringScheduleResponse,

    -- ** ListMonitoringSchedules (Paginated)
    ListMonitoringSchedules (ListMonitoringSchedules'),
    newListMonitoringSchedules,
    ListMonitoringSchedulesResponse (ListMonitoringSchedulesResponse'),
    newListMonitoringSchedulesResponse,

    -- ** StopNotebookInstance
    StopNotebookInstance (StopNotebookInstance'),
    newStopNotebookInstance,
    StopNotebookInstanceResponse (StopNotebookInstanceResponse'),
    newStopNotebookInstanceResponse,

    -- ** DeleteMonitoringSchedule
    DeleteMonitoringSchedule (DeleteMonitoringSchedule'),
    newDeleteMonitoringSchedule,
    DeleteMonitoringScheduleResponse (DeleteMonitoringScheduleResponse'),
    newDeleteMonitoringScheduleResponse,

    -- ** DeleteEndpointConfig
    DeleteEndpointConfig (DeleteEndpointConfig'),
    newDeleteEndpointConfig,
    DeleteEndpointConfigResponse (DeleteEndpointConfigResponse'),
    newDeleteEndpointConfigResponse,

    -- ** StartPipelineExecution
    StartPipelineExecution (StartPipelineExecution'),
    newStartPipelineExecution,
    StartPipelineExecutionResponse (StartPipelineExecutionResponse'),
    newStartPipelineExecutionResponse,

    -- ** DescribeModelPackage
    DescribeModelPackage (DescribeModelPackage'),
    newDescribeModelPackage,
    DescribeModelPackageResponse (DescribeModelPackageResponse'),
    newDescribeModelPackageResponse,

    -- ** DeleteTags
    DeleteTags (DeleteTags'),
    newDeleteTags,
    DeleteTagsResponse (DeleteTagsResponse'),
    newDeleteTagsResponse,

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

    -- ** StopPipelineExecution
    StopPipelineExecution (StopPipelineExecution'),
    newStopPipelineExecution,
    StopPipelineExecutionResponse (StopPipelineExecutionResponse'),
    newStopPipelineExecutionResponse,

    -- ** ListEndpointConfigs (Paginated)
    ListEndpointConfigs (ListEndpointConfigs'),
    newListEndpointConfigs,
    ListEndpointConfigsResponse (ListEndpointConfigsResponse'),
    newListEndpointConfigsResponse,

    -- ** DeleteWorkteam
    DeleteWorkteam (DeleteWorkteam'),
    newDeleteWorkteam,
    DeleteWorkteamResponse (DeleteWorkteamResponse'),
    newDeleteWorkteamResponse,

    -- ** DeleteWorkforce
    DeleteWorkforce (DeleteWorkforce'),
    newDeleteWorkforce,
    DeleteWorkforceResponse (DeleteWorkforceResponse'),
    newDeleteWorkforceResponse,

    -- ** DeleteModelBiasJobDefinition
    DeleteModelBiasJobDefinition (DeleteModelBiasJobDefinition'),
    newDeleteModelBiasJobDefinition,
    DeleteModelBiasJobDefinitionResponse (DeleteModelBiasJobDefinitionResponse'),
    newDeleteModelBiasJobDefinitionResponse,

    -- ** UpdateWorkforce
    UpdateWorkforce (UpdateWorkforce'),
    newUpdateWorkforce,
    UpdateWorkforceResponse (UpdateWorkforceResponse'),
    newUpdateWorkforceResponse,

    -- ** DescribeDevice
    DescribeDevice (DescribeDevice'),
    newDescribeDevice,
    DescribeDeviceResponse (DescribeDeviceResponse'),
    newDescribeDeviceResponse,

    -- ** DescribeDomain
    DescribeDomain (DescribeDomain'),
    newDescribeDomain,
    DescribeDomainResponse (DescribeDomainResponse'),
    newDescribeDomainResponse,

    -- ** DeleteNotebookInstanceLifecycleConfig
    DeleteNotebookInstanceLifecycleConfig (DeleteNotebookInstanceLifecycleConfig'),
    newDeleteNotebookInstanceLifecycleConfig,
    DeleteNotebookInstanceLifecycleConfigResponse (DeleteNotebookInstanceLifecycleConfigResponse'),
    newDeleteNotebookInstanceLifecycleConfigResponse,

    -- ** DescribePipelineExecution
    DescribePipelineExecution (DescribePipelineExecution'),
    newDescribePipelineExecution,
    DescribePipelineExecutionResponse (DescribePipelineExecutionResponse'),
    newDescribePipelineExecutionResponse,

    -- ** UpdateWorkteam
    UpdateWorkteam (UpdateWorkteam'),
    newUpdateWorkteam,
    UpdateWorkteamResponse (UpdateWorkteamResponse'),
    newUpdateWorkteamResponse,

    -- ** CreateLabelingJob
    CreateLabelingJob (CreateLabelingJob'),
    newCreateLabelingJob,
    CreateLabelingJobResponse (CreateLabelingJobResponse'),
    newCreateLabelingJobResponse,

    -- ** DescribeModelQualityJobDefinition
    DescribeModelQualityJobDefinition (DescribeModelQualityJobDefinition'),
    newDescribeModelQualityJobDefinition,
    DescribeModelQualityJobDefinitionResponse (DescribeModelQualityJobDefinitionResponse'),
    newDescribeModelQualityJobDefinitionResponse,

    -- ** CreateExperiment
    CreateExperiment (CreateExperiment'),
    newCreateExperiment,
    CreateExperimentResponse (CreateExperimentResponse'),
    newCreateExperimentResponse,

    -- ** ListWorkforces (Paginated)
    ListWorkforces (ListWorkforces'),
    newListWorkforces,
    ListWorkforcesResponse (ListWorkforcesResponse'),
    newListWorkforcesResponse,

    -- ** ListAppImageConfigs (Paginated)
    ListAppImageConfigs (ListAppImageConfigs'),
    newListAppImageConfigs,
    ListAppImageConfigsResponse (ListAppImageConfigsResponse'),
    newListAppImageConfigsResponse,

    -- ** UpdateNotebookInstanceLifecycleConfig
    UpdateNotebookInstanceLifecycleConfig (UpdateNotebookInstanceLifecycleConfig'),
    newUpdateNotebookInstanceLifecycleConfig,
    UpdateNotebookInstanceLifecycleConfigResponse (UpdateNotebookInstanceLifecycleConfigResponse'),
    newUpdateNotebookInstanceLifecycleConfigResponse,

    -- ** DescribeSubscribedWorkteam
    DescribeSubscribedWorkteam (DescribeSubscribedWorkteam'),
    newDescribeSubscribedWorkteam,
    DescribeSubscribedWorkteamResponse (DescribeSubscribedWorkteamResponse'),
    newDescribeSubscribedWorkteamResponse,

    -- ** ListNotebookInstanceLifecycleConfigs (Paginated)
    ListNotebookInstanceLifecycleConfigs (ListNotebookInstanceLifecycleConfigs'),
    newListNotebookInstanceLifecycleConfigs,
    ListNotebookInstanceLifecycleConfigsResponse (ListNotebookInstanceLifecycleConfigsResponse'),
    newListNotebookInstanceLifecycleConfigsResponse,

    -- ** ListEdgePackagingJobs (Paginated)
    ListEdgePackagingJobs (ListEdgePackagingJobs'),
    newListEdgePackagingJobs,
    ListEdgePackagingJobsResponse (ListEdgePackagingJobsResponse'),
    newListEdgePackagingJobsResponse,

    -- ** DescribeCodeRepository
    DescribeCodeRepository (DescribeCodeRepository'),
    newDescribeCodeRepository,
    DescribeCodeRepositoryResponse (DescribeCodeRepositoryResponse'),
    newDescribeCodeRepositoryResponse,

    -- ** ListEndpoints (Paginated)
    ListEndpoints (ListEndpoints'),
    newListEndpoints,
    ListEndpointsResponse (ListEndpointsResponse'),
    newListEndpointsResponse,

    -- ** DescribeDataQualityJobDefinition
    DescribeDataQualityJobDefinition (DescribeDataQualityJobDefinition'),
    newDescribeDataQualityJobDefinition,
    DescribeDataQualityJobDefinitionResponse (DescribeDataQualityJobDefinitionResponse'),
    newDescribeDataQualityJobDefinitionResponse,

    -- ** DescribeAlgorithm
    DescribeAlgorithm (DescribeAlgorithm'),
    newDescribeAlgorithm,
    DescribeAlgorithmResponse (DescribeAlgorithmResponse'),
    newDescribeAlgorithmResponse,

    -- ** CreateAction
    CreateAction (CreateAction'),
    newCreateAction,
    CreateActionResponse (CreateActionResponse'),
    newCreateActionResponse,

    -- ** DeleteEndpoint
    DeleteEndpoint (DeleteEndpoint'),
    newDeleteEndpoint,
    DeleteEndpointResponse (DeleteEndpointResponse'),
    newDeleteEndpointResponse,

    -- ** CreatePresignedDomainUrl
    CreatePresignedDomainUrl (CreatePresignedDomainUrl'),
    newCreatePresignedDomainUrl,
    CreatePresignedDomainUrlResponse (CreatePresignedDomainUrlResponse'),
    newCreatePresignedDomainUrlResponse,

    -- ** ListTransformJobs (Paginated)
    ListTransformJobs (ListTransformJobs'),
    newListTransformJobs,
    ListTransformJobsResponse (ListTransformJobsResponse'),
    newListTransformJobsResponse,

    -- ** DescribeHyperParameterTuningJob
    DescribeHyperParameterTuningJob (DescribeHyperParameterTuningJob'),
    newDescribeHyperParameterTuningJob,
    DescribeHyperParameterTuningJobResponse (DescribeHyperParameterTuningJobResponse'),
    newDescribeHyperParameterTuningJobResponse,

    -- ** CreateCompilationJob
    CreateCompilationJob (CreateCompilationJob'),
    newCreateCompilationJob,
    CreateCompilationJobResponse (CreateCompilationJobResponse'),
    newCreateCompilationJobResponse,

    -- ** UpdateEndpoint
    UpdateEndpoint (UpdateEndpoint'),
    newUpdateEndpoint,
    UpdateEndpointResponse (UpdateEndpointResponse'),
    newUpdateEndpointResponse,

    -- ** DescribeModel
    DescribeModel (DescribeModel'),
    newDescribeModel,
    DescribeModelResponse (DescribeModelResponse'),
    newDescribeModelResponse,

    -- ** CreateDeviceFleet
    CreateDeviceFleet (CreateDeviceFleet'),
    newCreateDeviceFleet,
    CreateDeviceFleetResponse (CreateDeviceFleetResponse'),
    newCreateDeviceFleetResponse,

    -- ** CreateArtifact
    CreateArtifact (CreateArtifact'),
    newCreateArtifact,
    CreateArtifactResponse (CreateArtifactResponse'),
    newCreateArtifactResponse,

    -- ** UpdateDevices
    UpdateDevices (UpdateDevices'),
    newUpdateDevices,
    UpdateDevicesResponse (UpdateDevicesResponse'),
    newUpdateDevicesResponse,

    -- ** ListArtifacts (Paginated)
    ListArtifacts (ListArtifacts'),
    newListArtifacts,
    ListArtifactsResponse (ListArtifactsResponse'),
    newListArtifactsResponse,

    -- ** DeleteDeviceFleet
    DeleteDeviceFleet (DeleteDeviceFleet'),
    newDeleteDeviceFleet,
    DeleteDeviceFleetResponse (DeleteDeviceFleetResponse'),
    newDeleteDeviceFleetResponse,

    -- ** ListMonitoringExecutions (Paginated)
    ListMonitoringExecutions (ListMonitoringExecutions'),
    newListMonitoringExecutions,
    ListMonitoringExecutionsResponse (ListMonitoringExecutionsResponse'),
    newListMonitoringExecutionsResponse,

    -- ** ListCompilationJobs (Paginated)
    ListCompilationJobs (ListCompilationJobs'),
    newListCompilationJobs,
    ListCompilationJobsResponse (ListCompilationJobsResponse'),
    newListCompilationJobsResponse,

    -- ** ListActions (Paginated)
    ListActions (ListActions'),
    newListActions,
    ListActionsResponse (ListActionsResponse'),
    newListActionsResponse,

    -- ** ListDeviceFleets (Paginated)
    ListDeviceFleets (ListDeviceFleets'),
    newListDeviceFleets,
    ListDeviceFleetsResponse (ListDeviceFleetsResponse'),
    newListDeviceFleetsResponse,

    -- ** DescribeModelPackageGroup
    DescribeModelPackageGroup (DescribeModelPackageGroup'),
    newDescribeModelPackageGroup,
    DescribeModelPackageGroupResponse (DescribeModelPackageGroupResponse'),
    newDescribeModelPackageGroupResponse,

    -- ** StopHyperParameterTuningJob
    StopHyperParameterTuningJob (StopHyperParameterTuningJob'),
    newStopHyperParameterTuningJob,
    StopHyperParameterTuningJobResponse (StopHyperParameterTuningJobResponse'),
    newStopHyperParameterTuningJobResponse,

    -- ** DescribeTrial
    DescribeTrial (DescribeTrial'),
    newDescribeTrial,
    DescribeTrialResponse (DescribeTrialResponse'),
    newDescribeTrialResponse,

    -- ** UpdateDeviceFleet
    UpdateDeviceFleet (UpdateDeviceFleet'),
    newUpdateDeviceFleet,
    UpdateDeviceFleetResponse (UpdateDeviceFleetResponse'),
    newUpdateDeviceFleetResponse,

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

    -- ** CreateDomain
    CreateDomain (CreateDomain'),
    newCreateDomain,
    CreateDomainResponse (CreateDomainResponse'),
    newCreateDomainResponse,

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

    -- ** DeleteImageVersion
    DeleteImageVersion (DeleteImageVersion'),
    newDeleteImageVersion,
    DeleteImageVersionResponse (DeleteImageVersionResponse'),
    newDeleteImageVersionResponse,

    -- ** DeleteProject
    DeleteProject (DeleteProject'),
    newDeleteProject,
    DeleteProjectResponse (DeleteProjectResponse'),
    newDeleteProjectResponse,

    -- ** DescribeExperiment
    DescribeExperiment (DescribeExperiment'),
    newDescribeExperiment,
    DescribeExperimentResponse (DescribeExperimentResponse'),
    newDescribeExperimentResponse,

    -- ** DescribeAutoMLJob
    DescribeAutoMLJob (DescribeAutoMLJob'),
    newDescribeAutoMLJob,
    DescribeAutoMLJobResponse (DescribeAutoMLJobResponse'),
    newDescribeAutoMLJobResponse,

    -- ** DescribeApp
    DescribeApp (DescribeApp'),
    newDescribeApp,
    DescribeAppResponse (DescribeAppResponse'),
    newDescribeAppResponse,

    -- ** ListTrialComponents (Paginated)
    ListTrialComponents (ListTrialComponents'),
    newListTrialComponents,
    ListTrialComponentsResponse (ListTrialComponentsResponse'),
    newListTrialComponentsResponse,

    -- ** UpdateTrialComponent
    UpdateTrialComponent (UpdateTrialComponent'),
    newUpdateTrialComponent,
    UpdateTrialComponentResponse (UpdateTrialComponentResponse'),
    newUpdateTrialComponentResponse,

    -- ** DeleteTrialComponent
    DeleteTrialComponent (DeleteTrialComponent'),
    newDeleteTrialComponent,
    DeleteTrialComponentResponse (DeleteTrialComponentResponse'),
    newDeleteTrialComponentResponse,

    -- ** CreateTrialComponent
    CreateTrialComponent (CreateTrialComponent'),
    newCreateTrialComponent,
    CreateTrialComponentResponse (CreateTrialComponentResponse'),
    newCreateTrialComponentResponse,

    -- ** DescribeWorkforce
    DescribeWorkforce (DescribeWorkforce'),
    newDescribeWorkforce,
    DescribeWorkforceResponse (DescribeWorkforceResponse'),
    newDescribeWorkforceResponse,

    -- ** ListNotebookInstances (Paginated)
    ListNotebookInstances (ListNotebookInstances'),
    newListNotebookInstances,
    ListNotebookInstancesResponse (ListNotebookInstancesResponse'),
    newListNotebookInstancesResponse,

    -- ** ListModelExplainabilityJobDefinitions (Paginated)
    ListModelExplainabilityJobDefinitions (ListModelExplainabilityJobDefinitions'),
    newListModelExplainabilityJobDefinitions,
    ListModelExplainabilityJobDefinitionsResponse (ListModelExplainabilityJobDefinitionsResponse'),
    newListModelExplainabilityJobDefinitionsResponse,

    -- ** DeleteModelQualityJobDefinition
    DeleteModelQualityJobDefinition (DeleteModelQualityJobDefinition'),
    newDeleteModelQualityJobDefinition,
    DeleteModelQualityJobDefinitionResponse (DeleteModelQualityJobDefinitionResponse'),
    newDeleteModelQualityJobDefinitionResponse,

    -- ** StopLabelingJob
    StopLabelingJob (StopLabelingJob'),
    newStopLabelingJob,
    StopLabelingJobResponse (StopLabelingJobResponse'),
    newStopLabelingJobResponse,

    -- ** ListModelQualityJobDefinitions (Paginated)
    ListModelQualityJobDefinitions (ListModelQualityJobDefinitions'),
    newListModelQualityJobDefinitions,
    ListModelQualityJobDefinitionsResponse (ListModelQualityJobDefinitionsResponse'),
    newListModelQualityJobDefinitionsResponse,

    -- ** DescribeModelBiasJobDefinition
    DescribeModelBiasJobDefinition (DescribeModelBiasJobDefinition'),
    newDescribeModelBiasJobDefinition,
    DescribeModelBiasJobDefinitionResponse (DescribeModelBiasJobDefinitionResponse'),
    newDescribeModelBiasJobDefinitionResponse,

    -- ** DescribeWorkteam
    DescribeWorkteam (DescribeWorkteam'),
    newDescribeWorkteam,
    DescribeWorkteamResponse (DescribeWorkteamResponse'),
    newDescribeWorkteamResponse,

    -- ** DescribeNotebookInstanceLifecycleConfig
    DescribeNotebookInstanceLifecycleConfig (DescribeNotebookInstanceLifecycleConfig'),
    newDescribeNotebookInstanceLifecycleConfig,
    DescribeNotebookInstanceLifecycleConfigResponse (DescribeNotebookInstanceLifecycleConfigResponse'),
    newDescribeNotebookInstanceLifecycleConfigResponse,

    -- ** ListPipelineExecutions (Paginated)
    ListPipelineExecutions (ListPipelineExecutions'),
    newListPipelineExecutions,
    ListPipelineExecutionsResponse (ListPipelineExecutionsResponse'),
    newListPipelineExecutionsResponse,

    -- ** UpdateDomain
    UpdateDomain (UpdateDomain'),
    newUpdateDomain,
    UpdateDomainResponse (UpdateDomainResponse'),
    newUpdateDomainResponse,

    -- ** AssociateTrialComponent
    AssociateTrialComponent (AssociateTrialComponent'),
    newAssociateTrialComponent,
    AssociateTrialComponentResponse (AssociateTrialComponentResponse'),
    newAssociateTrialComponentResponse,

    -- ** UpdatePipelineExecution
    UpdatePipelineExecution (UpdatePipelineExecution'),
    newUpdatePipelineExecution,
    UpdatePipelineExecutionResponse (UpdatePipelineExecutionResponse'),
    newUpdatePipelineExecutionResponse,

    -- ** CreateImageVersion
    CreateImageVersion (CreateImageVersion'),
    newCreateImageVersion,
    CreateImageVersionResponse (CreateImageVersionResponse'),
    newCreateImageVersionResponse,

    -- ** DeleteDomain
    DeleteDomain (DeleteDomain'),
    newDeleteDomain,
    DeleteDomainResponse (DeleteDomainResponse'),
    newDeleteDomainResponse,

    -- ** UpdateTrainingJob
    UpdateTrainingJob (UpdateTrainingJob'),
    newUpdateTrainingJob,
    UpdateTrainingJobResponse (UpdateTrainingJobResponse'),
    newUpdateTrainingJobResponse,

    -- ** UpdateImage
    UpdateImage (UpdateImage'),
    newUpdateImage,
    UpdateImageResponse (UpdateImageResponse'),
    newUpdateImageResponse,

    -- ** UpdateContext
    UpdateContext (UpdateContext'),
    newUpdateContext,
    UpdateContextResponse (UpdateContextResponse'),
    newUpdateContextResponse,

    -- ** DeleteImage
    DeleteImage (DeleteImage'),
    newDeleteImage,
    DeleteImageResponse (DeleteImageResponse'),
    newDeleteImageResponse,

    -- ** ListFlowDefinitions (Paginated)
    ListFlowDefinitions (ListFlowDefinitions'),
    newListFlowDefinitions,
    ListFlowDefinitionsResponse (ListFlowDefinitionsResponse'),
    newListFlowDefinitionsResponse,

    -- ** ListModels (Paginated)
    ListModels (ListModels'),
    newListModels,
    ListModelsResponse (ListModelsResponse'),
    newListModelsResponse,

    -- ** CreateUserProfile
    CreateUserProfile (CreateUserProfile'),
    newCreateUserProfile,
    CreateUserProfileResponse (CreateUserProfileResponse'),
    newCreateUserProfileResponse,

    -- ** RenderUiTemplate
    RenderUiTemplate (RenderUiTemplate'),
    newRenderUiTemplate,
    RenderUiTemplateResponse (RenderUiTemplateResponse'),
    newRenderUiTemplateResponse,

    -- ** DescribeFeatureGroup
    DescribeFeatureGroup (DescribeFeatureGroup'),
    newDescribeFeatureGroup,
    DescribeFeatureGroupResponse (DescribeFeatureGroupResponse'),
    newDescribeFeatureGroupResponse,

    -- ** DeleteContext
    DeleteContext (DeleteContext'),
    newDeleteContext,
    DeleteContextResponse (DeleteContextResponse'),
    newDeleteContextResponse,

    -- ** ListHyperParameterTuningJobs (Paginated)
    ListHyperParameterTuningJobs (ListHyperParameterTuningJobs'),
    newListHyperParameterTuningJobs,
    ListHyperParameterTuningJobsResponse (ListHyperParameterTuningJobsResponse'),
    newListHyperParameterTuningJobsResponse,

    -- ** DeleteFlowDefinition
    DeleteFlowDefinition (DeleteFlowDefinition'),
    newDeleteFlowDefinition,
    DeleteFlowDefinitionResponse (DeleteFlowDefinitionResponse'),
    newDeleteFlowDefinitionResponse,

    -- ** ListAlgorithms (Paginated)
    ListAlgorithms (ListAlgorithms'),
    newListAlgorithms,
    ListAlgorithmsResponse (ListAlgorithmsResponse'),
    newListAlgorithmsResponse,

    -- ** CreateAlgorithm
    CreateAlgorithm (CreateAlgorithm'),
    newCreateAlgorithm,
    CreateAlgorithmResponse (CreateAlgorithmResponse'),
    newCreateAlgorithmResponse,

    -- ** CreateFlowDefinition
    CreateFlowDefinition (CreateFlowDefinition'),
    newCreateFlowDefinition,
    CreateFlowDefinitionResponse (CreateFlowDefinitionResponse'),
    newCreateFlowDefinitionResponse,

    -- ** ListPipelineParametersForExecution (Paginated)
    ListPipelineParametersForExecution (ListPipelineParametersForExecution'),
    newListPipelineParametersForExecution,
    ListPipelineParametersForExecutionResponse (ListPipelineParametersForExecutionResponse'),
    newListPipelineParametersForExecutionResponse,

    -- ** ListTrials (Paginated)
    ListTrials (ListTrials'),
    newListTrials,
    ListTrialsResponse (ListTrialsResponse'),
    newListTrialsResponse,

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

    -- ** ListTags (Paginated)
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** DescribePipelineDefinitionForExecution
    DescribePipelineDefinitionForExecution (DescribePipelineDefinitionForExecution'),
    newDescribePipelineDefinitionForExecution,
    DescribePipelineDefinitionForExecutionResponse (DescribePipelineDefinitionForExecutionResponse'),
    newDescribePipelineDefinitionForExecutionResponse,

    -- ** DeleteTrial
    DeleteTrial (DeleteTrial'),
    newDeleteTrial,
    DeleteTrialResponse (DeleteTrialResponse'),
    newDeleteTrialResponse,

    -- ** PutModelPackageGroupPolicy
    PutModelPackageGroupPolicy (PutModelPackageGroupPolicy'),
    newPutModelPackageGroupPolicy,
    PutModelPackageGroupPolicyResponse (PutModelPackageGroupPolicyResponse'),
    newPutModelPackageGroupPolicyResponse,

    -- ** ListExperiments (Paginated)
    ListExperiments (ListExperiments'),
    newListExperiments,
    ListExperimentsResponse (ListExperimentsResponse'),
    newListExperimentsResponse,

    -- ** UpdateExperiment
    UpdateExperiment (UpdateExperiment'),
    newUpdateExperiment,
    UpdateExperimentResponse (UpdateExperimentResponse'),
    newUpdateExperimentResponse,

    -- ** DeleteExperiment
    DeleteExperiment (DeleteExperiment'),
    newDeleteExperiment,
    DeleteExperimentResponse (DeleteExperimentResponse'),
    newDeleteExperimentResponse,

    -- ** ListLabelingJobs (Paginated)
    ListLabelingJobs (ListLabelingJobs'),
    newListLabelingJobs,
    ListLabelingJobsResponse (ListLabelingJobsResponse'),
    newListLabelingJobsResponse,

    -- ** DescribeImageVersion
    DescribeImageVersion (DescribeImageVersion'),
    newDescribeImageVersion,
    DescribeImageVersionResponse (DescribeImageVersionResponse'),
    newDescribeImageVersionResponse,

    -- ** DeleteApp
    DeleteApp (DeleteApp'),
    newDeleteApp,
    DeleteAppResponse (DeleteAppResponse'),
    newDeleteAppResponse,

    -- ** CreateModelBiasJobDefinition
    CreateModelBiasJobDefinition (CreateModelBiasJobDefinition'),
    newCreateModelBiasJobDefinition,
    CreateModelBiasJobDefinitionResponse (CreateModelBiasJobDefinitionResponse'),
    newCreateModelBiasJobDefinitionResponse,

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

    -- ** DescribeProject
    DescribeProject (DescribeProject'),
    newDescribeProject,
    DescribeProjectResponse (DescribeProjectResponse'),
    newDescribeProjectResponse,

    -- ** CreateProcessingJob
    CreateProcessingJob (CreateProcessingJob'),
    newCreateProcessingJob,
    CreateProcessingJobResponse (CreateProcessingJobResponse'),
    newCreateProcessingJobResponse,

    -- ** ListAssociations (Paginated)
    ListAssociations (ListAssociations'),
    newListAssociations,
    ListAssociationsResponse (ListAssociationsResponse'),
    newListAssociationsResponse,

    -- ** EnableSagemakerServicecatalogPortfolio
    EnableSagemakerServicecatalogPortfolio (EnableSagemakerServicecatalogPortfolio'),
    newEnableSagemakerServicecatalogPortfolio,
    EnableSagemakerServicecatalogPortfolioResponse (EnableSagemakerServicecatalogPortfolioResponse'),
    newEnableSagemakerServicecatalogPortfolioResponse,

    -- ** UpdateAppImageConfig
    UpdateAppImageConfig (UpdateAppImageConfig'),
    newUpdateAppImageConfig,
    UpdateAppImageConfigResponse (UpdateAppImageConfigResponse'),
    newUpdateAppImageConfigResponse,

    -- ** ListModelBiasJobDefinitions (Paginated)
    ListModelBiasJobDefinitions (ListModelBiasJobDefinitions'),
    newListModelBiasJobDefinitions,
    ListModelBiasJobDefinitionsResponse (ListModelBiasJobDefinitionsResponse'),
    newListModelBiasJobDefinitionsResponse,

    -- ** CreateAutoMLJob
    CreateAutoMLJob (CreateAutoMLJob'),
    newCreateAutoMLJob,
    CreateAutoMLJobResponse (CreateAutoMLJobResponse'),
    newCreateAutoMLJobResponse,

    -- ** CreateApp
    CreateApp (CreateApp'),
    newCreateApp,
    CreateAppResponse (CreateAppResponse'),
    newCreateAppResponse,

    -- ** DescribeNotebookInstance
    DescribeNotebookInstance (DescribeNotebookInstance'),
    newDescribeNotebookInstance,
    DescribeNotebookInstanceResponse (DescribeNotebookInstanceResponse'),
    newDescribeNotebookInstanceResponse,

    -- ** DeleteAppImageConfig
    DeleteAppImageConfig (DeleteAppImageConfig'),
    newDeleteAppImageConfig,
    DeleteAppImageConfigResponse (DeleteAppImageConfigResponse'),
    newDeleteAppImageConfigResponse,

    -- ** CreateEndpointConfig
    CreateEndpointConfig (CreateEndpointConfig'),
    newCreateEndpointConfig,
    CreateEndpointConfigResponse (CreateEndpointConfigResponse'),
    newCreateEndpointConfigResponse,

    -- ** ListProcessingJobs (Paginated)
    ListProcessingJobs (ListProcessingJobs'),
    newListProcessingJobs,
    ListProcessingJobsResponse (ListProcessingJobsResponse'),
    newListProcessingJobsResponse,

    -- ** CreateMonitoringSchedule
    CreateMonitoringSchedule (CreateMonitoringSchedule'),
    newCreateMonitoringSchedule,
    CreateMonitoringScheduleResponse (CreateMonitoringScheduleResponse'),
    newCreateMonitoringScheduleResponse,

    -- ** DescribeModelExplainabilityJobDefinition
    DescribeModelExplainabilityJobDefinition (DescribeModelExplainabilityJobDefinition'),
    newDescribeModelExplainabilityJobDefinition,
    DescribeModelExplainabilityJobDefinitionResponse (DescribeModelExplainabilityJobDefinitionResponse'),
    newDescribeModelExplainabilityJobDefinitionResponse,

    -- ** ListWorkteams (Paginated)
    ListWorkteams (ListWorkteams'),
    newListWorkteams,
    ListWorkteamsResponse (ListWorkteamsResponse'),
    newListWorkteamsResponse,

    -- ** DescribeFlowDefinition
    DescribeFlowDefinition (DescribeFlowDefinition'),
    newDescribeFlowDefinition,
    DescribeFlowDefinitionResponse (DescribeFlowDefinitionResponse'),
    newDescribeFlowDefinitionResponse,

    -- ** DescribeContext
    DescribeContext (DescribeContext'),
    newDescribeContext,
    DescribeContextResponse (DescribeContextResponse'),
    newDescribeContextResponse,

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

    -- ** CreatePresignedNotebookInstanceUrl
    CreatePresignedNotebookInstanceUrl (CreatePresignedNotebookInstanceUrl'),
    newCreatePresignedNotebookInstanceUrl,
    CreatePresignedNotebookInstanceUrlResponse (CreatePresignedNotebookInstanceUrlResponse'),
    newCreatePresignedNotebookInstanceUrlResponse,

    -- ** DescribeTrainingJob
    DescribeTrainingJob (DescribeTrainingJob'),
    newDescribeTrainingJob,
    DescribeTrainingJobResponse (DescribeTrainingJobResponse'),
    newDescribeTrainingJobResponse,

    -- ** CreateHumanTaskUi
    CreateHumanTaskUi (CreateHumanTaskUi'),
    newCreateHumanTaskUi,
    CreateHumanTaskUiResponse (CreateHumanTaskUiResponse'),
    newCreateHumanTaskUiResponse,

    -- ** ListTrainingJobsForHyperParameterTuningJob (Paginated)
    ListTrainingJobsForHyperParameterTuningJob (ListTrainingJobsForHyperParameterTuningJob'),
    newListTrainingJobsForHyperParameterTuningJob,
    ListTrainingJobsForHyperParameterTuningJobResponse (ListTrainingJobsForHyperParameterTuningJobResponse'),
    newListTrainingJobsForHyperParameterTuningJobResponse,

    -- ** DescribeImage
    DescribeImage (DescribeImage'),
    newDescribeImage,
    DescribeImageResponse (DescribeImageResponse'),
    newDescribeImageResponse,

    -- ** DeleteFeatureGroup
    DeleteFeatureGroup (DeleteFeatureGroup'),
    newDeleteFeatureGroup,
    DeleteFeatureGroupResponse (DeleteFeatureGroupResponse'),
    newDeleteFeatureGroupResponse,

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

    -- ** MemberDefinition
    MemberDefinition (MemberDefinition'),
    newMemberDefinition,

    -- ** MetadataProperties
    MetadataProperties (MetadataProperties'),
    newMetadataProperties,

    -- ** MetricData
    MetricData (MetricData'),
    newMetricData,

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
import Network.AWS.SageMaker.Search
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

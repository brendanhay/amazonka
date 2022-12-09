{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SageMaker
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-07-24@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Provides APIs for creating and managing SageMaker resources.
--
-- Other Resources:
--
-- -   <https://docs.aws.amazon.com/sagemaker/latest/dg/whatis.html#first-time-user SageMaker Developer Guide>
--
-- -   <https://docs.aws.amazon.com/augmented-ai/2019-11-07/APIReference/Welcome.html Amazon Augmented AI Runtime API Reference>
module Amazonka.SageMaker
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ConflictException
    _ConflictException,

    -- ** ResourceInUse
    _ResourceInUse,

    -- ** ResourceLimitExceeded
    _ResourceLimitExceeded,

    -- ** ResourceNotFound
    _ResourceNotFound,

    -- * Waiters
    -- $waiters

    -- ** EndpointDeleted
    newEndpointDeleted,

    -- ** EndpointInService
    newEndpointInService,

    -- ** ImageCreated
    newImageCreated,

    -- ** ImageDeleted
    newImageDeleted,

    -- ** ImageUpdated
    newImageUpdated,

    -- ** ImageVersionCreated
    newImageVersionCreated,

    -- ** ImageVersionDeleted
    newImageVersionDeleted,

    -- ** NotebookInstanceDeleted
    newNotebookInstanceDeleted,

    -- ** NotebookInstanceInService
    newNotebookInstanceInService,

    -- ** NotebookInstanceStopped
    newNotebookInstanceStopped,

    -- ** ProcessingJobCompletedOrStopped
    newProcessingJobCompletedOrStopped,

    -- ** TrainingJobCompletedOrStopped
    newTrainingJobCompletedOrStopped,

    -- ** TransformJobCompletedOrStopped
    newTransformJobCompletedOrStopped,

    -- * Operations
    -- $operations

    -- ** AddAssociation
    AddAssociation (AddAssociation'),
    newAddAssociation,
    AddAssociationResponse (AddAssociationResponse'),
    newAddAssociationResponse,

    -- ** AddTags
    AddTags (AddTags'),
    newAddTags,
    AddTagsResponse (AddTagsResponse'),
    newAddTagsResponse,

    -- ** AssociateTrialComponent
    AssociateTrialComponent (AssociateTrialComponent'),
    newAssociateTrialComponent,
    AssociateTrialComponentResponse (AssociateTrialComponentResponse'),
    newAssociateTrialComponentResponse,

    -- ** BatchDescribeModelPackage
    BatchDescribeModelPackage (BatchDescribeModelPackage'),
    newBatchDescribeModelPackage,
    BatchDescribeModelPackageResponse (BatchDescribeModelPackageResponse'),
    newBatchDescribeModelPackageResponse,

    -- ** CreateAction
    CreateAction (CreateAction'),
    newCreateAction,
    CreateActionResponse (CreateActionResponse'),
    newCreateActionResponse,

    -- ** CreateAlgorithm
    CreateAlgorithm (CreateAlgorithm'),
    newCreateAlgorithm,
    CreateAlgorithmResponse (CreateAlgorithmResponse'),
    newCreateAlgorithmResponse,

    -- ** CreateApp
    CreateApp (CreateApp'),
    newCreateApp,
    CreateAppResponse (CreateAppResponse'),
    newCreateAppResponse,

    -- ** CreateAppImageConfig
    CreateAppImageConfig (CreateAppImageConfig'),
    newCreateAppImageConfig,
    CreateAppImageConfigResponse (CreateAppImageConfigResponse'),
    newCreateAppImageConfigResponse,

    -- ** CreateArtifact
    CreateArtifact (CreateArtifact'),
    newCreateArtifact,
    CreateArtifactResponse (CreateArtifactResponse'),
    newCreateArtifactResponse,

    -- ** CreateAutoMLJob
    CreateAutoMLJob (CreateAutoMLJob'),
    newCreateAutoMLJob,
    CreateAutoMLJobResponse (CreateAutoMLJobResponse'),
    newCreateAutoMLJobResponse,

    -- ** CreateCodeRepository
    CreateCodeRepository (CreateCodeRepository'),
    newCreateCodeRepository,
    CreateCodeRepositoryResponse (CreateCodeRepositoryResponse'),
    newCreateCodeRepositoryResponse,

    -- ** CreateCompilationJob
    CreateCompilationJob (CreateCompilationJob'),
    newCreateCompilationJob,
    CreateCompilationJobResponse (CreateCompilationJobResponse'),
    newCreateCompilationJobResponse,

    -- ** CreateContext
    CreateContext (CreateContext'),
    newCreateContext,
    CreateContextResponse (CreateContextResponse'),
    newCreateContextResponse,

    -- ** CreateDataQualityJobDefinition
    CreateDataQualityJobDefinition (CreateDataQualityJobDefinition'),
    newCreateDataQualityJobDefinition,
    CreateDataQualityJobDefinitionResponse (CreateDataQualityJobDefinitionResponse'),
    newCreateDataQualityJobDefinitionResponse,

    -- ** CreateDeviceFleet
    CreateDeviceFleet (CreateDeviceFleet'),
    newCreateDeviceFleet,
    CreateDeviceFleetResponse (CreateDeviceFleetResponse'),
    newCreateDeviceFleetResponse,

    -- ** CreateDomain
    CreateDomain (CreateDomain'),
    newCreateDomain,
    CreateDomainResponse (CreateDomainResponse'),
    newCreateDomainResponse,

    -- ** CreateEdgeDeploymentPlan
    CreateEdgeDeploymentPlan (CreateEdgeDeploymentPlan'),
    newCreateEdgeDeploymentPlan,
    CreateEdgeDeploymentPlanResponse (CreateEdgeDeploymentPlanResponse'),
    newCreateEdgeDeploymentPlanResponse,

    -- ** CreateEdgeDeploymentStage
    CreateEdgeDeploymentStage (CreateEdgeDeploymentStage'),
    newCreateEdgeDeploymentStage,
    CreateEdgeDeploymentStageResponse (CreateEdgeDeploymentStageResponse'),
    newCreateEdgeDeploymentStageResponse,

    -- ** CreateEdgePackagingJob
    CreateEdgePackagingJob (CreateEdgePackagingJob'),
    newCreateEdgePackagingJob,
    CreateEdgePackagingJobResponse (CreateEdgePackagingJobResponse'),
    newCreateEdgePackagingJobResponse,

    -- ** CreateEndpoint
    CreateEndpoint (CreateEndpoint'),
    newCreateEndpoint,
    CreateEndpointResponse (CreateEndpointResponse'),
    newCreateEndpointResponse,

    -- ** CreateEndpointConfig
    CreateEndpointConfig (CreateEndpointConfig'),
    newCreateEndpointConfig,
    CreateEndpointConfigResponse (CreateEndpointConfigResponse'),
    newCreateEndpointConfigResponse,

    -- ** CreateExperiment
    CreateExperiment (CreateExperiment'),
    newCreateExperiment,
    CreateExperimentResponse (CreateExperimentResponse'),
    newCreateExperimentResponse,

    -- ** CreateFeatureGroup
    CreateFeatureGroup (CreateFeatureGroup'),
    newCreateFeatureGroup,
    CreateFeatureGroupResponse (CreateFeatureGroupResponse'),
    newCreateFeatureGroupResponse,

    -- ** CreateFlowDefinition
    CreateFlowDefinition (CreateFlowDefinition'),
    newCreateFlowDefinition,
    CreateFlowDefinitionResponse (CreateFlowDefinitionResponse'),
    newCreateFlowDefinitionResponse,

    -- ** CreateHub
    CreateHub (CreateHub'),
    newCreateHub,
    CreateHubResponse (CreateHubResponse'),
    newCreateHubResponse,

    -- ** CreateHumanTaskUi
    CreateHumanTaskUi (CreateHumanTaskUi'),
    newCreateHumanTaskUi,
    CreateHumanTaskUiResponse (CreateHumanTaskUiResponse'),
    newCreateHumanTaskUiResponse,

    -- ** CreateHyperParameterTuningJob
    CreateHyperParameterTuningJob (CreateHyperParameterTuningJob'),
    newCreateHyperParameterTuningJob,
    CreateHyperParameterTuningJobResponse (CreateHyperParameterTuningJobResponse'),
    newCreateHyperParameterTuningJobResponse,

    -- ** CreateImage
    CreateImage (CreateImage'),
    newCreateImage,
    CreateImageResponse (CreateImageResponse'),
    newCreateImageResponse,

    -- ** CreateImageVersion
    CreateImageVersion (CreateImageVersion'),
    newCreateImageVersion,
    CreateImageVersionResponse (CreateImageVersionResponse'),
    newCreateImageVersionResponse,

    -- ** CreateInferenceExperiment
    CreateInferenceExperiment (CreateInferenceExperiment'),
    newCreateInferenceExperiment,
    CreateInferenceExperimentResponse (CreateInferenceExperimentResponse'),
    newCreateInferenceExperimentResponse,

    -- ** CreateInferenceRecommendationsJob
    CreateInferenceRecommendationsJob (CreateInferenceRecommendationsJob'),
    newCreateInferenceRecommendationsJob,
    CreateInferenceRecommendationsJobResponse (CreateInferenceRecommendationsJobResponse'),
    newCreateInferenceRecommendationsJobResponse,

    -- ** CreateLabelingJob
    CreateLabelingJob (CreateLabelingJob'),
    newCreateLabelingJob,
    CreateLabelingJobResponse (CreateLabelingJobResponse'),
    newCreateLabelingJobResponse,

    -- ** CreateModel
    CreateModel (CreateModel'),
    newCreateModel,
    CreateModelResponse (CreateModelResponse'),
    newCreateModelResponse,

    -- ** CreateModelBiasJobDefinition
    CreateModelBiasJobDefinition (CreateModelBiasJobDefinition'),
    newCreateModelBiasJobDefinition,
    CreateModelBiasJobDefinitionResponse (CreateModelBiasJobDefinitionResponse'),
    newCreateModelBiasJobDefinitionResponse,

    -- ** CreateModelCard
    CreateModelCard (CreateModelCard'),
    newCreateModelCard,
    CreateModelCardResponse (CreateModelCardResponse'),
    newCreateModelCardResponse,

    -- ** CreateModelCardExportJob
    CreateModelCardExportJob (CreateModelCardExportJob'),
    newCreateModelCardExportJob,
    CreateModelCardExportJobResponse (CreateModelCardExportJobResponse'),
    newCreateModelCardExportJobResponse,

    -- ** CreateModelExplainabilityJobDefinition
    CreateModelExplainabilityJobDefinition (CreateModelExplainabilityJobDefinition'),
    newCreateModelExplainabilityJobDefinition,
    CreateModelExplainabilityJobDefinitionResponse (CreateModelExplainabilityJobDefinitionResponse'),
    newCreateModelExplainabilityJobDefinitionResponse,

    -- ** CreateModelPackage
    CreateModelPackage (CreateModelPackage'),
    newCreateModelPackage,
    CreateModelPackageResponse (CreateModelPackageResponse'),
    newCreateModelPackageResponse,

    -- ** CreateModelPackageGroup
    CreateModelPackageGroup (CreateModelPackageGroup'),
    newCreateModelPackageGroup,
    CreateModelPackageGroupResponse (CreateModelPackageGroupResponse'),
    newCreateModelPackageGroupResponse,

    -- ** CreateModelQualityJobDefinition
    CreateModelQualityJobDefinition (CreateModelQualityJobDefinition'),
    newCreateModelQualityJobDefinition,
    CreateModelQualityJobDefinitionResponse (CreateModelQualityJobDefinitionResponse'),
    newCreateModelQualityJobDefinitionResponse,

    -- ** CreateMonitoringSchedule
    CreateMonitoringSchedule (CreateMonitoringSchedule'),
    newCreateMonitoringSchedule,
    CreateMonitoringScheduleResponse (CreateMonitoringScheduleResponse'),
    newCreateMonitoringScheduleResponse,

    -- ** CreateNotebookInstance
    CreateNotebookInstance (CreateNotebookInstance'),
    newCreateNotebookInstance,
    CreateNotebookInstanceResponse (CreateNotebookInstanceResponse'),
    newCreateNotebookInstanceResponse,

    -- ** CreateNotebookInstanceLifecycleConfig
    CreateNotebookInstanceLifecycleConfig (CreateNotebookInstanceLifecycleConfig'),
    newCreateNotebookInstanceLifecycleConfig,
    CreateNotebookInstanceLifecycleConfigResponse (CreateNotebookInstanceLifecycleConfigResponse'),
    newCreateNotebookInstanceLifecycleConfigResponse,

    -- ** CreatePipeline
    CreatePipeline (CreatePipeline'),
    newCreatePipeline,
    CreatePipelineResponse (CreatePipelineResponse'),
    newCreatePipelineResponse,

    -- ** CreatePresignedDomainUrl
    CreatePresignedDomainUrl (CreatePresignedDomainUrl'),
    newCreatePresignedDomainUrl,
    CreatePresignedDomainUrlResponse (CreatePresignedDomainUrlResponse'),
    newCreatePresignedDomainUrlResponse,

    -- ** CreatePresignedNotebookInstanceUrl
    CreatePresignedNotebookInstanceUrl (CreatePresignedNotebookInstanceUrl'),
    newCreatePresignedNotebookInstanceUrl,
    CreatePresignedNotebookInstanceUrlResponse (CreatePresignedNotebookInstanceUrlResponse'),
    newCreatePresignedNotebookInstanceUrlResponse,

    -- ** CreateProcessingJob
    CreateProcessingJob (CreateProcessingJob'),
    newCreateProcessingJob,
    CreateProcessingJobResponse (CreateProcessingJobResponse'),
    newCreateProcessingJobResponse,

    -- ** CreateProject
    CreateProject (CreateProject'),
    newCreateProject,
    CreateProjectResponse (CreateProjectResponse'),
    newCreateProjectResponse,

    -- ** CreateSpace
    CreateSpace (CreateSpace'),
    newCreateSpace,
    CreateSpaceResponse (CreateSpaceResponse'),
    newCreateSpaceResponse,

    -- ** CreateStudioLifecycleConfig
    CreateStudioLifecycleConfig (CreateStudioLifecycleConfig'),
    newCreateStudioLifecycleConfig,
    CreateStudioLifecycleConfigResponse (CreateStudioLifecycleConfigResponse'),
    newCreateStudioLifecycleConfigResponse,

    -- ** CreateTrainingJob
    CreateTrainingJob (CreateTrainingJob'),
    newCreateTrainingJob,
    CreateTrainingJobResponse (CreateTrainingJobResponse'),
    newCreateTrainingJobResponse,

    -- ** CreateTransformJob
    CreateTransformJob (CreateTransformJob'),
    newCreateTransformJob,
    CreateTransformJobResponse (CreateTransformJobResponse'),
    newCreateTransformJobResponse,

    -- ** CreateTrial
    CreateTrial (CreateTrial'),
    newCreateTrial,
    CreateTrialResponse (CreateTrialResponse'),
    newCreateTrialResponse,

    -- ** CreateTrialComponent
    CreateTrialComponent (CreateTrialComponent'),
    newCreateTrialComponent,
    CreateTrialComponentResponse (CreateTrialComponentResponse'),
    newCreateTrialComponentResponse,

    -- ** CreateUserProfile
    CreateUserProfile (CreateUserProfile'),
    newCreateUserProfile,
    CreateUserProfileResponse (CreateUserProfileResponse'),
    newCreateUserProfileResponse,

    -- ** CreateWorkforce
    CreateWorkforce (CreateWorkforce'),
    newCreateWorkforce,
    CreateWorkforceResponse (CreateWorkforceResponse'),
    newCreateWorkforceResponse,

    -- ** CreateWorkteam
    CreateWorkteam (CreateWorkteam'),
    newCreateWorkteam,
    CreateWorkteamResponse (CreateWorkteamResponse'),
    newCreateWorkteamResponse,

    -- ** DeleteAction
    DeleteAction (DeleteAction'),
    newDeleteAction,
    DeleteActionResponse (DeleteActionResponse'),
    newDeleteActionResponse,

    -- ** DeleteAlgorithm
    DeleteAlgorithm (DeleteAlgorithm'),
    newDeleteAlgorithm,
    DeleteAlgorithmResponse (DeleteAlgorithmResponse'),
    newDeleteAlgorithmResponse,

    -- ** DeleteApp
    DeleteApp (DeleteApp'),
    newDeleteApp,
    DeleteAppResponse (DeleteAppResponse'),
    newDeleteAppResponse,

    -- ** DeleteAppImageConfig
    DeleteAppImageConfig (DeleteAppImageConfig'),
    newDeleteAppImageConfig,
    DeleteAppImageConfigResponse (DeleteAppImageConfigResponse'),
    newDeleteAppImageConfigResponse,

    -- ** DeleteArtifact
    DeleteArtifact (DeleteArtifact'),
    newDeleteArtifact,
    DeleteArtifactResponse (DeleteArtifactResponse'),
    newDeleteArtifactResponse,

    -- ** DeleteAssociation
    DeleteAssociation (DeleteAssociation'),
    newDeleteAssociation,
    DeleteAssociationResponse (DeleteAssociationResponse'),
    newDeleteAssociationResponse,

    -- ** DeleteCodeRepository
    DeleteCodeRepository (DeleteCodeRepository'),
    newDeleteCodeRepository,
    DeleteCodeRepositoryResponse (DeleteCodeRepositoryResponse'),
    newDeleteCodeRepositoryResponse,

    -- ** DeleteContext
    DeleteContext (DeleteContext'),
    newDeleteContext,
    DeleteContextResponse (DeleteContextResponse'),
    newDeleteContextResponse,

    -- ** DeleteDataQualityJobDefinition
    DeleteDataQualityJobDefinition (DeleteDataQualityJobDefinition'),
    newDeleteDataQualityJobDefinition,
    DeleteDataQualityJobDefinitionResponse (DeleteDataQualityJobDefinitionResponse'),
    newDeleteDataQualityJobDefinitionResponse,

    -- ** DeleteDeviceFleet
    DeleteDeviceFleet (DeleteDeviceFleet'),
    newDeleteDeviceFleet,
    DeleteDeviceFleetResponse (DeleteDeviceFleetResponse'),
    newDeleteDeviceFleetResponse,

    -- ** DeleteDomain
    DeleteDomain (DeleteDomain'),
    newDeleteDomain,
    DeleteDomainResponse (DeleteDomainResponse'),
    newDeleteDomainResponse,

    -- ** DeleteEdgeDeploymentPlan
    DeleteEdgeDeploymentPlan (DeleteEdgeDeploymentPlan'),
    newDeleteEdgeDeploymentPlan,
    DeleteEdgeDeploymentPlanResponse (DeleteEdgeDeploymentPlanResponse'),
    newDeleteEdgeDeploymentPlanResponse,

    -- ** DeleteEdgeDeploymentStage
    DeleteEdgeDeploymentStage (DeleteEdgeDeploymentStage'),
    newDeleteEdgeDeploymentStage,
    DeleteEdgeDeploymentStageResponse (DeleteEdgeDeploymentStageResponse'),
    newDeleteEdgeDeploymentStageResponse,

    -- ** DeleteEndpoint
    DeleteEndpoint (DeleteEndpoint'),
    newDeleteEndpoint,
    DeleteEndpointResponse (DeleteEndpointResponse'),
    newDeleteEndpointResponse,

    -- ** DeleteEndpointConfig
    DeleteEndpointConfig (DeleteEndpointConfig'),
    newDeleteEndpointConfig,
    DeleteEndpointConfigResponse (DeleteEndpointConfigResponse'),
    newDeleteEndpointConfigResponse,

    -- ** DeleteExperiment
    DeleteExperiment (DeleteExperiment'),
    newDeleteExperiment,
    DeleteExperimentResponse (DeleteExperimentResponse'),
    newDeleteExperimentResponse,

    -- ** DeleteFeatureGroup
    DeleteFeatureGroup (DeleteFeatureGroup'),
    newDeleteFeatureGroup,
    DeleteFeatureGroupResponse (DeleteFeatureGroupResponse'),
    newDeleteFeatureGroupResponse,

    -- ** DeleteFlowDefinition
    DeleteFlowDefinition (DeleteFlowDefinition'),
    newDeleteFlowDefinition,
    DeleteFlowDefinitionResponse (DeleteFlowDefinitionResponse'),
    newDeleteFlowDefinitionResponse,

    -- ** DeleteHub
    DeleteHub (DeleteHub'),
    newDeleteHub,
    DeleteHubResponse (DeleteHubResponse'),
    newDeleteHubResponse,

    -- ** DeleteHubContent
    DeleteHubContent (DeleteHubContent'),
    newDeleteHubContent,
    DeleteHubContentResponse (DeleteHubContentResponse'),
    newDeleteHubContentResponse,

    -- ** DeleteHumanTaskUi
    DeleteHumanTaskUi (DeleteHumanTaskUi'),
    newDeleteHumanTaskUi,
    DeleteHumanTaskUiResponse (DeleteHumanTaskUiResponse'),
    newDeleteHumanTaskUiResponse,

    -- ** DeleteImage
    DeleteImage (DeleteImage'),
    newDeleteImage,
    DeleteImageResponse (DeleteImageResponse'),
    newDeleteImageResponse,

    -- ** DeleteImageVersion
    DeleteImageVersion (DeleteImageVersion'),
    newDeleteImageVersion,
    DeleteImageVersionResponse (DeleteImageVersionResponse'),
    newDeleteImageVersionResponse,

    -- ** DeleteInferenceExperiment
    DeleteInferenceExperiment (DeleteInferenceExperiment'),
    newDeleteInferenceExperiment,
    DeleteInferenceExperimentResponse (DeleteInferenceExperimentResponse'),
    newDeleteInferenceExperimentResponse,

    -- ** DeleteModel
    DeleteModel (DeleteModel'),
    newDeleteModel,
    DeleteModelResponse (DeleteModelResponse'),
    newDeleteModelResponse,

    -- ** DeleteModelBiasJobDefinition
    DeleteModelBiasJobDefinition (DeleteModelBiasJobDefinition'),
    newDeleteModelBiasJobDefinition,
    DeleteModelBiasJobDefinitionResponse (DeleteModelBiasJobDefinitionResponse'),
    newDeleteModelBiasJobDefinitionResponse,

    -- ** DeleteModelCard
    DeleteModelCard (DeleteModelCard'),
    newDeleteModelCard,
    DeleteModelCardResponse (DeleteModelCardResponse'),
    newDeleteModelCardResponse,

    -- ** DeleteModelExplainabilityJobDefinition
    DeleteModelExplainabilityJobDefinition (DeleteModelExplainabilityJobDefinition'),
    newDeleteModelExplainabilityJobDefinition,
    DeleteModelExplainabilityJobDefinitionResponse (DeleteModelExplainabilityJobDefinitionResponse'),
    newDeleteModelExplainabilityJobDefinitionResponse,

    -- ** DeleteModelPackage
    DeleteModelPackage (DeleteModelPackage'),
    newDeleteModelPackage,
    DeleteModelPackageResponse (DeleteModelPackageResponse'),
    newDeleteModelPackageResponse,

    -- ** DeleteModelPackageGroup
    DeleteModelPackageGroup (DeleteModelPackageGroup'),
    newDeleteModelPackageGroup,
    DeleteModelPackageGroupResponse (DeleteModelPackageGroupResponse'),
    newDeleteModelPackageGroupResponse,

    -- ** DeleteModelPackageGroupPolicy
    DeleteModelPackageGroupPolicy (DeleteModelPackageGroupPolicy'),
    newDeleteModelPackageGroupPolicy,
    DeleteModelPackageGroupPolicyResponse (DeleteModelPackageGroupPolicyResponse'),
    newDeleteModelPackageGroupPolicyResponse,

    -- ** DeleteModelQualityJobDefinition
    DeleteModelQualityJobDefinition (DeleteModelQualityJobDefinition'),
    newDeleteModelQualityJobDefinition,
    DeleteModelQualityJobDefinitionResponse (DeleteModelQualityJobDefinitionResponse'),
    newDeleteModelQualityJobDefinitionResponse,

    -- ** DeleteMonitoringSchedule
    DeleteMonitoringSchedule (DeleteMonitoringSchedule'),
    newDeleteMonitoringSchedule,
    DeleteMonitoringScheduleResponse (DeleteMonitoringScheduleResponse'),
    newDeleteMonitoringScheduleResponse,

    -- ** DeleteNotebookInstance
    DeleteNotebookInstance (DeleteNotebookInstance'),
    newDeleteNotebookInstance,
    DeleteNotebookInstanceResponse (DeleteNotebookInstanceResponse'),
    newDeleteNotebookInstanceResponse,

    -- ** DeleteNotebookInstanceLifecycleConfig
    DeleteNotebookInstanceLifecycleConfig (DeleteNotebookInstanceLifecycleConfig'),
    newDeleteNotebookInstanceLifecycleConfig,
    DeleteNotebookInstanceLifecycleConfigResponse (DeleteNotebookInstanceLifecycleConfigResponse'),
    newDeleteNotebookInstanceLifecycleConfigResponse,

    -- ** DeletePipeline
    DeletePipeline (DeletePipeline'),
    newDeletePipeline,
    DeletePipelineResponse (DeletePipelineResponse'),
    newDeletePipelineResponse,

    -- ** DeleteProject
    DeleteProject (DeleteProject'),
    newDeleteProject,
    DeleteProjectResponse (DeleteProjectResponse'),
    newDeleteProjectResponse,

    -- ** DeleteSpace
    DeleteSpace (DeleteSpace'),
    newDeleteSpace,
    DeleteSpaceResponse (DeleteSpaceResponse'),
    newDeleteSpaceResponse,

    -- ** DeleteStudioLifecycleConfig
    DeleteStudioLifecycleConfig (DeleteStudioLifecycleConfig'),
    newDeleteStudioLifecycleConfig,
    DeleteStudioLifecycleConfigResponse (DeleteStudioLifecycleConfigResponse'),
    newDeleteStudioLifecycleConfigResponse,

    -- ** DeleteTags
    DeleteTags (DeleteTags'),
    newDeleteTags,
    DeleteTagsResponse (DeleteTagsResponse'),
    newDeleteTagsResponse,

    -- ** DeleteTrial
    DeleteTrial (DeleteTrial'),
    newDeleteTrial,
    DeleteTrialResponse (DeleteTrialResponse'),
    newDeleteTrialResponse,

    -- ** DeleteTrialComponent
    DeleteTrialComponent (DeleteTrialComponent'),
    newDeleteTrialComponent,
    DeleteTrialComponentResponse (DeleteTrialComponentResponse'),
    newDeleteTrialComponentResponse,

    -- ** DeleteUserProfile
    DeleteUserProfile (DeleteUserProfile'),
    newDeleteUserProfile,
    DeleteUserProfileResponse (DeleteUserProfileResponse'),
    newDeleteUserProfileResponse,

    -- ** DeleteWorkforce
    DeleteWorkforce (DeleteWorkforce'),
    newDeleteWorkforce,
    DeleteWorkforceResponse (DeleteWorkforceResponse'),
    newDeleteWorkforceResponse,

    -- ** DeleteWorkteam
    DeleteWorkteam (DeleteWorkteam'),
    newDeleteWorkteam,
    DeleteWorkteamResponse (DeleteWorkteamResponse'),
    newDeleteWorkteamResponse,

    -- ** DeregisterDevices
    DeregisterDevices (DeregisterDevices'),
    newDeregisterDevices,
    DeregisterDevicesResponse (DeregisterDevicesResponse'),
    newDeregisterDevicesResponse,

    -- ** DescribeAction
    DescribeAction (DescribeAction'),
    newDescribeAction,
    DescribeActionResponse (DescribeActionResponse'),
    newDescribeActionResponse,

    -- ** DescribeAlgorithm
    DescribeAlgorithm (DescribeAlgorithm'),
    newDescribeAlgorithm,
    DescribeAlgorithmResponse (DescribeAlgorithmResponse'),
    newDescribeAlgorithmResponse,

    -- ** DescribeApp
    DescribeApp (DescribeApp'),
    newDescribeApp,
    DescribeAppResponse (DescribeAppResponse'),
    newDescribeAppResponse,

    -- ** DescribeAppImageConfig
    DescribeAppImageConfig (DescribeAppImageConfig'),
    newDescribeAppImageConfig,
    DescribeAppImageConfigResponse (DescribeAppImageConfigResponse'),
    newDescribeAppImageConfigResponse,

    -- ** DescribeArtifact
    DescribeArtifact (DescribeArtifact'),
    newDescribeArtifact,
    DescribeArtifactResponse (DescribeArtifactResponse'),
    newDescribeArtifactResponse,

    -- ** DescribeAutoMLJob
    DescribeAutoMLJob (DescribeAutoMLJob'),
    newDescribeAutoMLJob,
    DescribeAutoMLJobResponse (DescribeAutoMLJobResponse'),
    newDescribeAutoMLJobResponse,

    -- ** DescribeCodeRepository
    DescribeCodeRepository (DescribeCodeRepository'),
    newDescribeCodeRepository,
    DescribeCodeRepositoryResponse (DescribeCodeRepositoryResponse'),
    newDescribeCodeRepositoryResponse,

    -- ** DescribeCompilationJob
    DescribeCompilationJob (DescribeCompilationJob'),
    newDescribeCompilationJob,
    DescribeCompilationJobResponse (DescribeCompilationJobResponse'),
    newDescribeCompilationJobResponse,

    -- ** DescribeContext
    DescribeContext (DescribeContext'),
    newDescribeContext,
    DescribeContextResponse (DescribeContextResponse'),
    newDescribeContextResponse,

    -- ** DescribeDataQualityJobDefinition
    DescribeDataQualityJobDefinition (DescribeDataQualityJobDefinition'),
    newDescribeDataQualityJobDefinition,
    DescribeDataQualityJobDefinitionResponse (DescribeDataQualityJobDefinitionResponse'),
    newDescribeDataQualityJobDefinitionResponse,

    -- ** DescribeDevice
    DescribeDevice (DescribeDevice'),
    newDescribeDevice,
    DescribeDeviceResponse (DescribeDeviceResponse'),
    newDescribeDeviceResponse,

    -- ** DescribeDeviceFleet
    DescribeDeviceFleet (DescribeDeviceFleet'),
    newDescribeDeviceFleet,
    DescribeDeviceFleetResponse (DescribeDeviceFleetResponse'),
    newDescribeDeviceFleetResponse,

    -- ** DescribeDomain
    DescribeDomain (DescribeDomain'),
    newDescribeDomain,
    DescribeDomainResponse (DescribeDomainResponse'),
    newDescribeDomainResponse,

    -- ** DescribeEdgeDeploymentPlan
    DescribeEdgeDeploymentPlan (DescribeEdgeDeploymentPlan'),
    newDescribeEdgeDeploymentPlan,
    DescribeEdgeDeploymentPlanResponse (DescribeEdgeDeploymentPlanResponse'),
    newDescribeEdgeDeploymentPlanResponse,

    -- ** DescribeEdgePackagingJob
    DescribeEdgePackagingJob (DescribeEdgePackagingJob'),
    newDescribeEdgePackagingJob,
    DescribeEdgePackagingJobResponse (DescribeEdgePackagingJobResponse'),
    newDescribeEdgePackagingJobResponse,

    -- ** DescribeEndpoint
    DescribeEndpoint (DescribeEndpoint'),
    newDescribeEndpoint,
    DescribeEndpointResponse (DescribeEndpointResponse'),
    newDescribeEndpointResponse,

    -- ** DescribeEndpointConfig
    DescribeEndpointConfig (DescribeEndpointConfig'),
    newDescribeEndpointConfig,
    DescribeEndpointConfigResponse (DescribeEndpointConfigResponse'),
    newDescribeEndpointConfigResponse,

    -- ** DescribeExperiment
    DescribeExperiment (DescribeExperiment'),
    newDescribeExperiment,
    DescribeExperimentResponse (DescribeExperimentResponse'),
    newDescribeExperimentResponse,

    -- ** DescribeFeatureGroup
    DescribeFeatureGroup (DescribeFeatureGroup'),
    newDescribeFeatureGroup,
    DescribeFeatureGroupResponse (DescribeFeatureGroupResponse'),
    newDescribeFeatureGroupResponse,

    -- ** DescribeFeatureMetadata
    DescribeFeatureMetadata (DescribeFeatureMetadata'),
    newDescribeFeatureMetadata,
    DescribeFeatureMetadataResponse (DescribeFeatureMetadataResponse'),
    newDescribeFeatureMetadataResponse,

    -- ** DescribeFlowDefinition
    DescribeFlowDefinition (DescribeFlowDefinition'),
    newDescribeFlowDefinition,
    DescribeFlowDefinitionResponse (DescribeFlowDefinitionResponse'),
    newDescribeFlowDefinitionResponse,

    -- ** DescribeHub
    DescribeHub (DescribeHub'),
    newDescribeHub,
    DescribeHubResponse (DescribeHubResponse'),
    newDescribeHubResponse,

    -- ** DescribeHubContent
    DescribeHubContent (DescribeHubContent'),
    newDescribeHubContent,
    DescribeHubContentResponse (DescribeHubContentResponse'),
    newDescribeHubContentResponse,

    -- ** DescribeHumanTaskUi
    DescribeHumanTaskUi (DescribeHumanTaskUi'),
    newDescribeHumanTaskUi,
    DescribeHumanTaskUiResponse (DescribeHumanTaskUiResponse'),
    newDescribeHumanTaskUiResponse,

    -- ** DescribeHyperParameterTuningJob
    DescribeHyperParameterTuningJob (DescribeHyperParameterTuningJob'),
    newDescribeHyperParameterTuningJob,
    DescribeHyperParameterTuningJobResponse (DescribeHyperParameterTuningJobResponse'),
    newDescribeHyperParameterTuningJobResponse,

    -- ** DescribeImage
    DescribeImage (DescribeImage'),
    newDescribeImage,
    DescribeImageResponse (DescribeImageResponse'),
    newDescribeImageResponse,

    -- ** DescribeImageVersion
    DescribeImageVersion (DescribeImageVersion'),
    newDescribeImageVersion,
    DescribeImageVersionResponse (DescribeImageVersionResponse'),
    newDescribeImageVersionResponse,

    -- ** DescribeInferenceExperiment
    DescribeInferenceExperiment (DescribeInferenceExperiment'),
    newDescribeInferenceExperiment,
    DescribeInferenceExperimentResponse (DescribeInferenceExperimentResponse'),
    newDescribeInferenceExperimentResponse,

    -- ** DescribeInferenceRecommendationsJob
    DescribeInferenceRecommendationsJob (DescribeInferenceRecommendationsJob'),
    newDescribeInferenceRecommendationsJob,
    DescribeInferenceRecommendationsJobResponse (DescribeInferenceRecommendationsJobResponse'),
    newDescribeInferenceRecommendationsJobResponse,

    -- ** DescribeLabelingJob
    DescribeLabelingJob (DescribeLabelingJob'),
    newDescribeLabelingJob,
    DescribeLabelingJobResponse (DescribeLabelingJobResponse'),
    newDescribeLabelingJobResponse,

    -- ** DescribeLineageGroup
    DescribeLineageGroup (DescribeLineageGroup'),
    newDescribeLineageGroup,
    DescribeLineageGroupResponse (DescribeLineageGroupResponse'),
    newDescribeLineageGroupResponse,

    -- ** DescribeModel
    DescribeModel (DescribeModel'),
    newDescribeModel,
    DescribeModelResponse (DescribeModelResponse'),
    newDescribeModelResponse,

    -- ** DescribeModelBiasJobDefinition
    DescribeModelBiasJobDefinition (DescribeModelBiasJobDefinition'),
    newDescribeModelBiasJobDefinition,
    DescribeModelBiasJobDefinitionResponse (DescribeModelBiasJobDefinitionResponse'),
    newDescribeModelBiasJobDefinitionResponse,

    -- ** DescribeModelCard
    DescribeModelCard (DescribeModelCard'),
    newDescribeModelCard,
    DescribeModelCardResponse (DescribeModelCardResponse'),
    newDescribeModelCardResponse,

    -- ** DescribeModelCardExportJob
    DescribeModelCardExportJob (DescribeModelCardExportJob'),
    newDescribeModelCardExportJob,
    DescribeModelCardExportJobResponse (DescribeModelCardExportJobResponse'),
    newDescribeModelCardExportJobResponse,

    -- ** DescribeModelExplainabilityJobDefinition
    DescribeModelExplainabilityJobDefinition (DescribeModelExplainabilityJobDefinition'),
    newDescribeModelExplainabilityJobDefinition,
    DescribeModelExplainabilityJobDefinitionResponse (DescribeModelExplainabilityJobDefinitionResponse'),
    newDescribeModelExplainabilityJobDefinitionResponse,

    -- ** DescribeModelPackage
    DescribeModelPackage (DescribeModelPackage'),
    newDescribeModelPackage,
    DescribeModelPackageResponse (DescribeModelPackageResponse'),
    newDescribeModelPackageResponse,

    -- ** DescribeModelPackageGroup
    DescribeModelPackageGroup (DescribeModelPackageGroup'),
    newDescribeModelPackageGroup,
    DescribeModelPackageGroupResponse (DescribeModelPackageGroupResponse'),
    newDescribeModelPackageGroupResponse,

    -- ** DescribeModelQualityJobDefinition
    DescribeModelQualityJobDefinition (DescribeModelQualityJobDefinition'),
    newDescribeModelQualityJobDefinition,
    DescribeModelQualityJobDefinitionResponse (DescribeModelQualityJobDefinitionResponse'),
    newDescribeModelQualityJobDefinitionResponse,

    -- ** DescribeMonitoringSchedule
    DescribeMonitoringSchedule (DescribeMonitoringSchedule'),
    newDescribeMonitoringSchedule,
    DescribeMonitoringScheduleResponse (DescribeMonitoringScheduleResponse'),
    newDescribeMonitoringScheduleResponse,

    -- ** DescribeNotebookInstance
    DescribeNotebookInstance (DescribeNotebookInstance'),
    newDescribeNotebookInstance,
    DescribeNotebookInstanceResponse (DescribeNotebookInstanceResponse'),
    newDescribeNotebookInstanceResponse,

    -- ** DescribeNotebookInstanceLifecycleConfig
    DescribeNotebookInstanceLifecycleConfig (DescribeNotebookInstanceLifecycleConfig'),
    newDescribeNotebookInstanceLifecycleConfig,
    DescribeNotebookInstanceLifecycleConfigResponse (DescribeNotebookInstanceLifecycleConfigResponse'),
    newDescribeNotebookInstanceLifecycleConfigResponse,

    -- ** DescribePipeline
    DescribePipeline (DescribePipeline'),
    newDescribePipeline,
    DescribePipelineResponse (DescribePipelineResponse'),
    newDescribePipelineResponse,

    -- ** DescribePipelineDefinitionForExecution
    DescribePipelineDefinitionForExecution (DescribePipelineDefinitionForExecution'),
    newDescribePipelineDefinitionForExecution,
    DescribePipelineDefinitionForExecutionResponse (DescribePipelineDefinitionForExecutionResponse'),
    newDescribePipelineDefinitionForExecutionResponse,

    -- ** DescribePipelineExecution
    DescribePipelineExecution (DescribePipelineExecution'),
    newDescribePipelineExecution,
    DescribePipelineExecutionResponse (DescribePipelineExecutionResponse'),
    newDescribePipelineExecutionResponse,

    -- ** DescribeProcessingJob
    DescribeProcessingJob (DescribeProcessingJob'),
    newDescribeProcessingJob,
    DescribeProcessingJobResponse (DescribeProcessingJobResponse'),
    newDescribeProcessingJobResponse,

    -- ** DescribeProject
    DescribeProject (DescribeProject'),
    newDescribeProject,
    DescribeProjectResponse (DescribeProjectResponse'),
    newDescribeProjectResponse,

    -- ** DescribeSpace
    DescribeSpace (DescribeSpace'),
    newDescribeSpace,
    DescribeSpaceResponse (DescribeSpaceResponse'),
    newDescribeSpaceResponse,

    -- ** DescribeStudioLifecycleConfig
    DescribeStudioLifecycleConfig (DescribeStudioLifecycleConfig'),
    newDescribeStudioLifecycleConfig,
    DescribeStudioLifecycleConfigResponse (DescribeStudioLifecycleConfigResponse'),
    newDescribeStudioLifecycleConfigResponse,

    -- ** DescribeSubscribedWorkteam
    DescribeSubscribedWorkteam (DescribeSubscribedWorkteam'),
    newDescribeSubscribedWorkteam,
    DescribeSubscribedWorkteamResponse (DescribeSubscribedWorkteamResponse'),
    newDescribeSubscribedWorkteamResponse,

    -- ** DescribeTrainingJob
    DescribeTrainingJob (DescribeTrainingJob'),
    newDescribeTrainingJob,
    DescribeTrainingJobResponse (DescribeTrainingJobResponse'),
    newDescribeTrainingJobResponse,

    -- ** DescribeTransformJob
    DescribeTransformJob (DescribeTransformJob'),
    newDescribeTransformJob,
    DescribeTransformJobResponse (DescribeTransformJobResponse'),
    newDescribeTransformJobResponse,

    -- ** DescribeTrial
    DescribeTrial (DescribeTrial'),
    newDescribeTrial,
    DescribeTrialResponse (DescribeTrialResponse'),
    newDescribeTrialResponse,

    -- ** DescribeTrialComponent
    DescribeTrialComponent (DescribeTrialComponent'),
    newDescribeTrialComponent,
    DescribeTrialComponentResponse (DescribeTrialComponentResponse'),
    newDescribeTrialComponentResponse,

    -- ** DescribeUserProfile
    DescribeUserProfile (DescribeUserProfile'),
    newDescribeUserProfile,
    DescribeUserProfileResponse (DescribeUserProfileResponse'),
    newDescribeUserProfileResponse,

    -- ** DescribeWorkforce
    DescribeWorkforce (DescribeWorkforce'),
    newDescribeWorkforce,
    DescribeWorkforceResponse (DescribeWorkforceResponse'),
    newDescribeWorkforceResponse,

    -- ** DescribeWorkteam
    DescribeWorkteam (DescribeWorkteam'),
    newDescribeWorkteam,
    DescribeWorkteamResponse (DescribeWorkteamResponse'),
    newDescribeWorkteamResponse,

    -- ** DisableSagemakerServicecatalogPortfolio
    DisableSagemakerServicecatalogPortfolio (DisableSagemakerServicecatalogPortfolio'),
    newDisableSagemakerServicecatalogPortfolio,
    DisableSagemakerServicecatalogPortfolioResponse (DisableSagemakerServicecatalogPortfolioResponse'),
    newDisableSagemakerServicecatalogPortfolioResponse,

    -- ** DisassociateTrialComponent
    DisassociateTrialComponent (DisassociateTrialComponent'),
    newDisassociateTrialComponent,
    DisassociateTrialComponentResponse (DisassociateTrialComponentResponse'),
    newDisassociateTrialComponentResponse,

    -- ** EnableSagemakerServicecatalogPortfolio
    EnableSagemakerServicecatalogPortfolio (EnableSagemakerServicecatalogPortfolio'),
    newEnableSagemakerServicecatalogPortfolio,
    EnableSagemakerServicecatalogPortfolioResponse (EnableSagemakerServicecatalogPortfolioResponse'),
    newEnableSagemakerServicecatalogPortfolioResponse,

    -- ** GetDeviceFleetReport
    GetDeviceFleetReport (GetDeviceFleetReport'),
    newGetDeviceFleetReport,
    GetDeviceFleetReportResponse (GetDeviceFleetReportResponse'),
    newGetDeviceFleetReportResponse,

    -- ** GetLineageGroupPolicy
    GetLineageGroupPolicy (GetLineageGroupPolicy'),
    newGetLineageGroupPolicy,
    GetLineageGroupPolicyResponse (GetLineageGroupPolicyResponse'),
    newGetLineageGroupPolicyResponse,

    -- ** GetModelPackageGroupPolicy
    GetModelPackageGroupPolicy (GetModelPackageGroupPolicy'),
    newGetModelPackageGroupPolicy,
    GetModelPackageGroupPolicyResponse (GetModelPackageGroupPolicyResponse'),
    newGetModelPackageGroupPolicyResponse,

    -- ** GetSagemakerServicecatalogPortfolioStatus
    GetSagemakerServicecatalogPortfolioStatus (GetSagemakerServicecatalogPortfolioStatus'),
    newGetSagemakerServicecatalogPortfolioStatus,
    GetSagemakerServicecatalogPortfolioStatusResponse (GetSagemakerServicecatalogPortfolioStatusResponse'),
    newGetSagemakerServicecatalogPortfolioStatusResponse,

    -- ** GetSearchSuggestions
    GetSearchSuggestions (GetSearchSuggestions'),
    newGetSearchSuggestions,
    GetSearchSuggestionsResponse (GetSearchSuggestionsResponse'),
    newGetSearchSuggestionsResponse,

    -- ** ImportHubContent
    ImportHubContent (ImportHubContent'),
    newImportHubContent,
    ImportHubContentResponse (ImportHubContentResponse'),
    newImportHubContentResponse,

    -- ** ListActions (Paginated)
    ListActions (ListActions'),
    newListActions,
    ListActionsResponse (ListActionsResponse'),
    newListActionsResponse,

    -- ** ListAlgorithms (Paginated)
    ListAlgorithms (ListAlgorithms'),
    newListAlgorithms,
    ListAlgorithmsResponse (ListAlgorithmsResponse'),
    newListAlgorithmsResponse,

    -- ** ListAppImageConfigs (Paginated)
    ListAppImageConfigs (ListAppImageConfigs'),
    newListAppImageConfigs,
    ListAppImageConfigsResponse (ListAppImageConfigsResponse'),
    newListAppImageConfigsResponse,

    -- ** ListApps (Paginated)
    ListApps (ListApps'),
    newListApps,
    ListAppsResponse (ListAppsResponse'),
    newListAppsResponse,

    -- ** ListArtifacts (Paginated)
    ListArtifacts (ListArtifacts'),
    newListArtifacts,
    ListArtifactsResponse (ListArtifactsResponse'),
    newListArtifactsResponse,

    -- ** ListAssociations (Paginated)
    ListAssociations (ListAssociations'),
    newListAssociations,
    ListAssociationsResponse (ListAssociationsResponse'),
    newListAssociationsResponse,

    -- ** ListAutoMLJobs (Paginated)
    ListAutoMLJobs (ListAutoMLJobs'),
    newListAutoMLJobs,
    ListAutoMLJobsResponse (ListAutoMLJobsResponse'),
    newListAutoMLJobsResponse,

    -- ** ListCandidatesForAutoMLJob (Paginated)
    ListCandidatesForAutoMLJob (ListCandidatesForAutoMLJob'),
    newListCandidatesForAutoMLJob,
    ListCandidatesForAutoMLJobResponse (ListCandidatesForAutoMLJobResponse'),
    newListCandidatesForAutoMLJobResponse,

    -- ** ListCodeRepositories (Paginated)
    ListCodeRepositories (ListCodeRepositories'),
    newListCodeRepositories,
    ListCodeRepositoriesResponse (ListCodeRepositoriesResponse'),
    newListCodeRepositoriesResponse,

    -- ** ListCompilationJobs (Paginated)
    ListCompilationJobs (ListCompilationJobs'),
    newListCompilationJobs,
    ListCompilationJobsResponse (ListCompilationJobsResponse'),
    newListCompilationJobsResponse,

    -- ** ListContexts (Paginated)
    ListContexts (ListContexts'),
    newListContexts,
    ListContextsResponse (ListContextsResponse'),
    newListContextsResponse,

    -- ** ListDataQualityJobDefinitions (Paginated)
    ListDataQualityJobDefinitions (ListDataQualityJobDefinitions'),
    newListDataQualityJobDefinitions,
    ListDataQualityJobDefinitionsResponse (ListDataQualityJobDefinitionsResponse'),
    newListDataQualityJobDefinitionsResponse,

    -- ** ListDeviceFleets (Paginated)
    ListDeviceFleets (ListDeviceFleets'),
    newListDeviceFleets,
    ListDeviceFleetsResponse (ListDeviceFleetsResponse'),
    newListDeviceFleetsResponse,

    -- ** ListDevices (Paginated)
    ListDevices (ListDevices'),
    newListDevices,
    ListDevicesResponse (ListDevicesResponse'),
    newListDevicesResponse,

    -- ** ListDomains (Paginated)
    ListDomains (ListDomains'),
    newListDomains,
    ListDomainsResponse (ListDomainsResponse'),
    newListDomainsResponse,

    -- ** ListEdgeDeploymentPlans (Paginated)
    ListEdgeDeploymentPlans (ListEdgeDeploymentPlans'),
    newListEdgeDeploymentPlans,
    ListEdgeDeploymentPlansResponse (ListEdgeDeploymentPlansResponse'),
    newListEdgeDeploymentPlansResponse,

    -- ** ListEdgePackagingJobs (Paginated)
    ListEdgePackagingJobs (ListEdgePackagingJobs'),
    newListEdgePackagingJobs,
    ListEdgePackagingJobsResponse (ListEdgePackagingJobsResponse'),
    newListEdgePackagingJobsResponse,

    -- ** ListEndpointConfigs (Paginated)
    ListEndpointConfigs (ListEndpointConfigs'),
    newListEndpointConfigs,
    ListEndpointConfigsResponse (ListEndpointConfigsResponse'),
    newListEndpointConfigsResponse,

    -- ** ListEndpoints (Paginated)
    ListEndpoints (ListEndpoints'),
    newListEndpoints,
    ListEndpointsResponse (ListEndpointsResponse'),
    newListEndpointsResponse,

    -- ** ListExperiments (Paginated)
    ListExperiments (ListExperiments'),
    newListExperiments,
    ListExperimentsResponse (ListExperimentsResponse'),
    newListExperimentsResponse,

    -- ** ListFeatureGroups (Paginated)
    ListFeatureGroups (ListFeatureGroups'),
    newListFeatureGroups,
    ListFeatureGroupsResponse (ListFeatureGroupsResponse'),
    newListFeatureGroupsResponse,

    -- ** ListFlowDefinitions (Paginated)
    ListFlowDefinitions (ListFlowDefinitions'),
    newListFlowDefinitions,
    ListFlowDefinitionsResponse (ListFlowDefinitionsResponse'),
    newListFlowDefinitionsResponse,

    -- ** ListHubContentVersions
    ListHubContentVersions (ListHubContentVersions'),
    newListHubContentVersions,
    ListHubContentVersionsResponse (ListHubContentVersionsResponse'),
    newListHubContentVersionsResponse,

    -- ** ListHubContents
    ListHubContents (ListHubContents'),
    newListHubContents,
    ListHubContentsResponse (ListHubContentsResponse'),
    newListHubContentsResponse,

    -- ** ListHubs
    ListHubs (ListHubs'),
    newListHubs,
    ListHubsResponse (ListHubsResponse'),
    newListHubsResponse,

    -- ** ListHumanTaskUis (Paginated)
    ListHumanTaskUis (ListHumanTaskUis'),
    newListHumanTaskUis,
    ListHumanTaskUisResponse (ListHumanTaskUisResponse'),
    newListHumanTaskUisResponse,

    -- ** ListHyperParameterTuningJobs (Paginated)
    ListHyperParameterTuningJobs (ListHyperParameterTuningJobs'),
    newListHyperParameterTuningJobs,
    ListHyperParameterTuningJobsResponse (ListHyperParameterTuningJobsResponse'),
    newListHyperParameterTuningJobsResponse,

    -- ** ListImageVersions (Paginated)
    ListImageVersions (ListImageVersions'),
    newListImageVersions,
    ListImageVersionsResponse (ListImageVersionsResponse'),
    newListImageVersionsResponse,

    -- ** ListImages (Paginated)
    ListImages (ListImages'),
    newListImages,
    ListImagesResponse (ListImagesResponse'),
    newListImagesResponse,

    -- ** ListInferenceExperiments (Paginated)
    ListInferenceExperiments (ListInferenceExperiments'),
    newListInferenceExperiments,
    ListInferenceExperimentsResponse (ListInferenceExperimentsResponse'),
    newListInferenceExperimentsResponse,

    -- ** ListInferenceRecommendationsJobSteps (Paginated)
    ListInferenceRecommendationsJobSteps (ListInferenceRecommendationsJobSteps'),
    newListInferenceRecommendationsJobSteps,
    ListInferenceRecommendationsJobStepsResponse (ListInferenceRecommendationsJobStepsResponse'),
    newListInferenceRecommendationsJobStepsResponse,

    -- ** ListInferenceRecommendationsJobs (Paginated)
    ListInferenceRecommendationsJobs (ListInferenceRecommendationsJobs'),
    newListInferenceRecommendationsJobs,
    ListInferenceRecommendationsJobsResponse (ListInferenceRecommendationsJobsResponse'),
    newListInferenceRecommendationsJobsResponse,

    -- ** ListLabelingJobs (Paginated)
    ListLabelingJobs (ListLabelingJobs'),
    newListLabelingJobs,
    ListLabelingJobsResponse (ListLabelingJobsResponse'),
    newListLabelingJobsResponse,

    -- ** ListLabelingJobsForWorkteam (Paginated)
    ListLabelingJobsForWorkteam (ListLabelingJobsForWorkteam'),
    newListLabelingJobsForWorkteam,
    ListLabelingJobsForWorkteamResponse (ListLabelingJobsForWorkteamResponse'),
    newListLabelingJobsForWorkteamResponse,

    -- ** ListLineageGroups (Paginated)
    ListLineageGroups (ListLineageGroups'),
    newListLineageGroups,
    ListLineageGroupsResponse (ListLineageGroupsResponse'),
    newListLineageGroupsResponse,

    -- ** ListModelBiasJobDefinitions (Paginated)
    ListModelBiasJobDefinitions (ListModelBiasJobDefinitions'),
    newListModelBiasJobDefinitions,
    ListModelBiasJobDefinitionsResponse (ListModelBiasJobDefinitionsResponse'),
    newListModelBiasJobDefinitionsResponse,

    -- ** ListModelCardExportJobs (Paginated)
    ListModelCardExportJobs (ListModelCardExportJobs'),
    newListModelCardExportJobs,
    ListModelCardExportJobsResponse (ListModelCardExportJobsResponse'),
    newListModelCardExportJobsResponse,

    -- ** ListModelCardVersions (Paginated)
    ListModelCardVersions (ListModelCardVersions'),
    newListModelCardVersions,
    ListModelCardVersionsResponse (ListModelCardVersionsResponse'),
    newListModelCardVersionsResponse,

    -- ** ListModelCards (Paginated)
    ListModelCards (ListModelCards'),
    newListModelCards,
    ListModelCardsResponse (ListModelCardsResponse'),
    newListModelCardsResponse,

    -- ** ListModelExplainabilityJobDefinitions (Paginated)
    ListModelExplainabilityJobDefinitions (ListModelExplainabilityJobDefinitions'),
    newListModelExplainabilityJobDefinitions,
    ListModelExplainabilityJobDefinitionsResponse (ListModelExplainabilityJobDefinitionsResponse'),
    newListModelExplainabilityJobDefinitionsResponse,

    -- ** ListModelMetadata (Paginated)
    ListModelMetadata (ListModelMetadata'),
    newListModelMetadata,
    ListModelMetadataResponse (ListModelMetadataResponse'),
    newListModelMetadataResponse,

    -- ** ListModelPackageGroups (Paginated)
    ListModelPackageGroups (ListModelPackageGroups'),
    newListModelPackageGroups,
    ListModelPackageGroupsResponse (ListModelPackageGroupsResponse'),
    newListModelPackageGroupsResponse,

    -- ** ListModelPackages (Paginated)
    ListModelPackages (ListModelPackages'),
    newListModelPackages,
    ListModelPackagesResponse (ListModelPackagesResponse'),
    newListModelPackagesResponse,

    -- ** ListModelQualityJobDefinitions (Paginated)
    ListModelQualityJobDefinitions (ListModelQualityJobDefinitions'),
    newListModelQualityJobDefinitions,
    ListModelQualityJobDefinitionsResponse (ListModelQualityJobDefinitionsResponse'),
    newListModelQualityJobDefinitionsResponse,

    -- ** ListModels (Paginated)
    ListModels (ListModels'),
    newListModels,
    ListModelsResponse (ListModelsResponse'),
    newListModelsResponse,

    -- ** ListMonitoringAlertHistory (Paginated)
    ListMonitoringAlertHistory (ListMonitoringAlertHistory'),
    newListMonitoringAlertHistory,
    ListMonitoringAlertHistoryResponse (ListMonitoringAlertHistoryResponse'),
    newListMonitoringAlertHistoryResponse,

    -- ** ListMonitoringAlerts (Paginated)
    ListMonitoringAlerts (ListMonitoringAlerts'),
    newListMonitoringAlerts,
    ListMonitoringAlertsResponse (ListMonitoringAlertsResponse'),
    newListMonitoringAlertsResponse,

    -- ** ListMonitoringExecutions (Paginated)
    ListMonitoringExecutions (ListMonitoringExecutions'),
    newListMonitoringExecutions,
    ListMonitoringExecutionsResponse (ListMonitoringExecutionsResponse'),
    newListMonitoringExecutionsResponse,

    -- ** ListMonitoringSchedules (Paginated)
    ListMonitoringSchedules (ListMonitoringSchedules'),
    newListMonitoringSchedules,
    ListMonitoringSchedulesResponse (ListMonitoringSchedulesResponse'),
    newListMonitoringSchedulesResponse,

    -- ** ListNotebookInstanceLifecycleConfigs (Paginated)
    ListNotebookInstanceLifecycleConfigs (ListNotebookInstanceLifecycleConfigs'),
    newListNotebookInstanceLifecycleConfigs,
    ListNotebookInstanceLifecycleConfigsResponse (ListNotebookInstanceLifecycleConfigsResponse'),
    newListNotebookInstanceLifecycleConfigsResponse,

    -- ** ListNotebookInstances (Paginated)
    ListNotebookInstances (ListNotebookInstances'),
    newListNotebookInstances,
    ListNotebookInstancesResponse (ListNotebookInstancesResponse'),
    newListNotebookInstancesResponse,

    -- ** ListPipelineExecutionSteps (Paginated)
    ListPipelineExecutionSteps (ListPipelineExecutionSteps'),
    newListPipelineExecutionSteps,
    ListPipelineExecutionStepsResponse (ListPipelineExecutionStepsResponse'),
    newListPipelineExecutionStepsResponse,

    -- ** ListPipelineExecutions (Paginated)
    ListPipelineExecutions (ListPipelineExecutions'),
    newListPipelineExecutions,
    ListPipelineExecutionsResponse (ListPipelineExecutionsResponse'),
    newListPipelineExecutionsResponse,

    -- ** ListPipelineParametersForExecution (Paginated)
    ListPipelineParametersForExecution (ListPipelineParametersForExecution'),
    newListPipelineParametersForExecution,
    ListPipelineParametersForExecutionResponse (ListPipelineParametersForExecutionResponse'),
    newListPipelineParametersForExecutionResponse,

    -- ** ListPipelines (Paginated)
    ListPipelines (ListPipelines'),
    newListPipelines,
    ListPipelinesResponse (ListPipelinesResponse'),
    newListPipelinesResponse,

    -- ** ListProcessingJobs (Paginated)
    ListProcessingJobs (ListProcessingJobs'),
    newListProcessingJobs,
    ListProcessingJobsResponse (ListProcessingJobsResponse'),
    newListProcessingJobsResponse,

    -- ** ListProjects
    ListProjects (ListProjects'),
    newListProjects,
    ListProjectsResponse (ListProjectsResponse'),
    newListProjectsResponse,

    -- ** ListSpaces (Paginated)
    ListSpaces (ListSpaces'),
    newListSpaces,
    ListSpacesResponse (ListSpacesResponse'),
    newListSpacesResponse,

    -- ** ListStageDevices (Paginated)
    ListStageDevices (ListStageDevices'),
    newListStageDevices,
    ListStageDevicesResponse (ListStageDevicesResponse'),
    newListStageDevicesResponse,

    -- ** ListStudioLifecycleConfigs (Paginated)
    ListStudioLifecycleConfigs (ListStudioLifecycleConfigs'),
    newListStudioLifecycleConfigs,
    ListStudioLifecycleConfigsResponse (ListStudioLifecycleConfigsResponse'),
    newListStudioLifecycleConfigsResponse,

    -- ** ListSubscribedWorkteams (Paginated)
    ListSubscribedWorkteams (ListSubscribedWorkteams'),
    newListSubscribedWorkteams,
    ListSubscribedWorkteamsResponse (ListSubscribedWorkteamsResponse'),
    newListSubscribedWorkteamsResponse,

    -- ** ListTags (Paginated)
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** ListTrainingJobs (Paginated)
    ListTrainingJobs (ListTrainingJobs'),
    newListTrainingJobs,
    ListTrainingJobsResponse (ListTrainingJobsResponse'),
    newListTrainingJobsResponse,

    -- ** ListTrainingJobsForHyperParameterTuningJob (Paginated)
    ListTrainingJobsForHyperParameterTuningJob (ListTrainingJobsForHyperParameterTuningJob'),
    newListTrainingJobsForHyperParameterTuningJob,
    ListTrainingJobsForHyperParameterTuningJobResponse (ListTrainingJobsForHyperParameterTuningJobResponse'),
    newListTrainingJobsForHyperParameterTuningJobResponse,

    -- ** ListTransformJobs (Paginated)
    ListTransformJobs (ListTransformJobs'),
    newListTransformJobs,
    ListTransformJobsResponse (ListTransformJobsResponse'),
    newListTransformJobsResponse,

    -- ** ListTrialComponents (Paginated)
    ListTrialComponents (ListTrialComponents'),
    newListTrialComponents,
    ListTrialComponentsResponse (ListTrialComponentsResponse'),
    newListTrialComponentsResponse,

    -- ** ListTrials (Paginated)
    ListTrials (ListTrials'),
    newListTrials,
    ListTrialsResponse (ListTrialsResponse'),
    newListTrialsResponse,

    -- ** ListUserProfiles (Paginated)
    ListUserProfiles (ListUserProfiles'),
    newListUserProfiles,
    ListUserProfilesResponse (ListUserProfilesResponse'),
    newListUserProfilesResponse,

    -- ** ListWorkforces (Paginated)
    ListWorkforces (ListWorkforces'),
    newListWorkforces,
    ListWorkforcesResponse (ListWorkforcesResponse'),
    newListWorkforcesResponse,

    -- ** ListWorkteams (Paginated)
    ListWorkteams (ListWorkteams'),
    newListWorkteams,
    ListWorkteamsResponse (ListWorkteamsResponse'),
    newListWorkteamsResponse,

    -- ** PutModelPackageGroupPolicy
    PutModelPackageGroupPolicy (PutModelPackageGroupPolicy'),
    newPutModelPackageGroupPolicy,
    PutModelPackageGroupPolicyResponse (PutModelPackageGroupPolicyResponse'),
    newPutModelPackageGroupPolicyResponse,

    -- ** QueryLineage
    QueryLineage (QueryLineage'),
    newQueryLineage,
    QueryLineageResponse (QueryLineageResponse'),
    newQueryLineageResponse,

    -- ** RegisterDevices
    RegisterDevices (RegisterDevices'),
    newRegisterDevices,
    RegisterDevicesResponse (RegisterDevicesResponse'),
    newRegisterDevicesResponse,

    -- ** RenderUiTemplate
    RenderUiTemplate (RenderUiTemplate'),
    newRenderUiTemplate,
    RenderUiTemplateResponse (RenderUiTemplateResponse'),
    newRenderUiTemplateResponse,

    -- ** RetryPipelineExecution
    RetryPipelineExecution (RetryPipelineExecution'),
    newRetryPipelineExecution,
    RetryPipelineExecutionResponse (RetryPipelineExecutionResponse'),
    newRetryPipelineExecutionResponse,

    -- ** Search (Paginated)
    Search (Search'),
    newSearch,
    SearchResponse (SearchResponse'),
    newSearchResponse,

    -- ** SendPipelineExecutionStepFailure
    SendPipelineExecutionStepFailure (SendPipelineExecutionStepFailure'),
    newSendPipelineExecutionStepFailure,
    SendPipelineExecutionStepFailureResponse (SendPipelineExecutionStepFailureResponse'),
    newSendPipelineExecutionStepFailureResponse,

    -- ** SendPipelineExecutionStepSuccess
    SendPipelineExecutionStepSuccess (SendPipelineExecutionStepSuccess'),
    newSendPipelineExecutionStepSuccess,
    SendPipelineExecutionStepSuccessResponse (SendPipelineExecutionStepSuccessResponse'),
    newSendPipelineExecutionStepSuccessResponse,

    -- ** StartEdgeDeploymentStage
    StartEdgeDeploymentStage (StartEdgeDeploymentStage'),
    newStartEdgeDeploymentStage,
    StartEdgeDeploymentStageResponse (StartEdgeDeploymentStageResponse'),
    newStartEdgeDeploymentStageResponse,

    -- ** StartInferenceExperiment
    StartInferenceExperiment (StartInferenceExperiment'),
    newStartInferenceExperiment,
    StartInferenceExperimentResponse (StartInferenceExperimentResponse'),
    newStartInferenceExperimentResponse,

    -- ** StartMonitoringSchedule
    StartMonitoringSchedule (StartMonitoringSchedule'),
    newStartMonitoringSchedule,
    StartMonitoringScheduleResponse (StartMonitoringScheduleResponse'),
    newStartMonitoringScheduleResponse,

    -- ** StartNotebookInstance
    StartNotebookInstance (StartNotebookInstance'),
    newStartNotebookInstance,
    StartNotebookInstanceResponse (StartNotebookInstanceResponse'),
    newStartNotebookInstanceResponse,

    -- ** StartPipelineExecution
    StartPipelineExecution (StartPipelineExecution'),
    newStartPipelineExecution,
    StartPipelineExecutionResponse (StartPipelineExecutionResponse'),
    newStartPipelineExecutionResponse,

    -- ** StopAutoMLJob
    StopAutoMLJob (StopAutoMLJob'),
    newStopAutoMLJob,
    StopAutoMLJobResponse (StopAutoMLJobResponse'),
    newStopAutoMLJobResponse,

    -- ** StopCompilationJob
    StopCompilationJob (StopCompilationJob'),
    newStopCompilationJob,
    StopCompilationJobResponse (StopCompilationJobResponse'),
    newStopCompilationJobResponse,

    -- ** StopEdgeDeploymentStage
    StopEdgeDeploymentStage (StopEdgeDeploymentStage'),
    newStopEdgeDeploymentStage,
    StopEdgeDeploymentStageResponse (StopEdgeDeploymentStageResponse'),
    newStopEdgeDeploymentStageResponse,

    -- ** StopEdgePackagingJob
    StopEdgePackagingJob (StopEdgePackagingJob'),
    newStopEdgePackagingJob,
    StopEdgePackagingJobResponse (StopEdgePackagingJobResponse'),
    newStopEdgePackagingJobResponse,

    -- ** StopHyperParameterTuningJob
    StopHyperParameterTuningJob (StopHyperParameterTuningJob'),
    newStopHyperParameterTuningJob,
    StopHyperParameterTuningJobResponse (StopHyperParameterTuningJobResponse'),
    newStopHyperParameterTuningJobResponse,

    -- ** StopInferenceExperiment
    StopInferenceExperiment (StopInferenceExperiment'),
    newStopInferenceExperiment,
    StopInferenceExperimentResponse (StopInferenceExperimentResponse'),
    newStopInferenceExperimentResponse,

    -- ** StopInferenceRecommendationsJob
    StopInferenceRecommendationsJob (StopInferenceRecommendationsJob'),
    newStopInferenceRecommendationsJob,
    StopInferenceRecommendationsJobResponse (StopInferenceRecommendationsJobResponse'),
    newStopInferenceRecommendationsJobResponse,

    -- ** StopLabelingJob
    StopLabelingJob (StopLabelingJob'),
    newStopLabelingJob,
    StopLabelingJobResponse (StopLabelingJobResponse'),
    newStopLabelingJobResponse,

    -- ** StopMonitoringSchedule
    StopMonitoringSchedule (StopMonitoringSchedule'),
    newStopMonitoringSchedule,
    StopMonitoringScheduleResponse (StopMonitoringScheduleResponse'),
    newStopMonitoringScheduleResponse,

    -- ** StopNotebookInstance
    StopNotebookInstance (StopNotebookInstance'),
    newStopNotebookInstance,
    StopNotebookInstanceResponse (StopNotebookInstanceResponse'),
    newStopNotebookInstanceResponse,

    -- ** StopPipelineExecution
    StopPipelineExecution (StopPipelineExecution'),
    newStopPipelineExecution,
    StopPipelineExecutionResponse (StopPipelineExecutionResponse'),
    newStopPipelineExecutionResponse,

    -- ** StopProcessingJob
    StopProcessingJob (StopProcessingJob'),
    newStopProcessingJob,
    StopProcessingJobResponse (StopProcessingJobResponse'),
    newStopProcessingJobResponse,

    -- ** StopTrainingJob
    StopTrainingJob (StopTrainingJob'),
    newStopTrainingJob,
    StopTrainingJobResponse (StopTrainingJobResponse'),
    newStopTrainingJobResponse,

    -- ** StopTransformJob
    StopTransformJob (StopTransformJob'),
    newStopTransformJob,
    StopTransformJobResponse (StopTransformJobResponse'),
    newStopTransformJobResponse,

    -- ** UpdateAction
    UpdateAction (UpdateAction'),
    newUpdateAction,
    UpdateActionResponse (UpdateActionResponse'),
    newUpdateActionResponse,

    -- ** UpdateAppImageConfig
    UpdateAppImageConfig (UpdateAppImageConfig'),
    newUpdateAppImageConfig,
    UpdateAppImageConfigResponse (UpdateAppImageConfigResponse'),
    newUpdateAppImageConfigResponse,

    -- ** UpdateArtifact
    UpdateArtifact (UpdateArtifact'),
    newUpdateArtifact,
    UpdateArtifactResponse (UpdateArtifactResponse'),
    newUpdateArtifactResponse,

    -- ** UpdateCodeRepository
    UpdateCodeRepository (UpdateCodeRepository'),
    newUpdateCodeRepository,
    UpdateCodeRepositoryResponse (UpdateCodeRepositoryResponse'),
    newUpdateCodeRepositoryResponse,

    -- ** UpdateContext
    UpdateContext (UpdateContext'),
    newUpdateContext,
    UpdateContextResponse (UpdateContextResponse'),
    newUpdateContextResponse,

    -- ** UpdateDeviceFleet
    UpdateDeviceFleet (UpdateDeviceFleet'),
    newUpdateDeviceFleet,
    UpdateDeviceFleetResponse (UpdateDeviceFleetResponse'),
    newUpdateDeviceFleetResponse,

    -- ** UpdateDevices
    UpdateDevices (UpdateDevices'),
    newUpdateDevices,
    UpdateDevicesResponse (UpdateDevicesResponse'),
    newUpdateDevicesResponse,

    -- ** UpdateDomain
    UpdateDomain (UpdateDomain'),
    newUpdateDomain,
    UpdateDomainResponse (UpdateDomainResponse'),
    newUpdateDomainResponse,

    -- ** UpdateEndpoint
    UpdateEndpoint (UpdateEndpoint'),
    newUpdateEndpoint,
    UpdateEndpointResponse (UpdateEndpointResponse'),
    newUpdateEndpointResponse,

    -- ** UpdateEndpointWeightsAndCapacities
    UpdateEndpointWeightsAndCapacities (UpdateEndpointWeightsAndCapacities'),
    newUpdateEndpointWeightsAndCapacities,
    UpdateEndpointWeightsAndCapacitiesResponse (UpdateEndpointWeightsAndCapacitiesResponse'),
    newUpdateEndpointWeightsAndCapacitiesResponse,

    -- ** UpdateExperiment
    UpdateExperiment (UpdateExperiment'),
    newUpdateExperiment,
    UpdateExperimentResponse (UpdateExperimentResponse'),
    newUpdateExperimentResponse,

    -- ** UpdateFeatureGroup
    UpdateFeatureGroup (UpdateFeatureGroup'),
    newUpdateFeatureGroup,
    UpdateFeatureGroupResponse (UpdateFeatureGroupResponse'),
    newUpdateFeatureGroupResponse,

    -- ** UpdateFeatureMetadata
    UpdateFeatureMetadata (UpdateFeatureMetadata'),
    newUpdateFeatureMetadata,
    UpdateFeatureMetadataResponse (UpdateFeatureMetadataResponse'),
    newUpdateFeatureMetadataResponse,

    -- ** UpdateHub
    UpdateHub (UpdateHub'),
    newUpdateHub,
    UpdateHubResponse (UpdateHubResponse'),
    newUpdateHubResponse,

    -- ** UpdateImage
    UpdateImage (UpdateImage'),
    newUpdateImage,
    UpdateImageResponse (UpdateImageResponse'),
    newUpdateImageResponse,

    -- ** UpdateInferenceExperiment
    UpdateInferenceExperiment (UpdateInferenceExperiment'),
    newUpdateInferenceExperiment,
    UpdateInferenceExperimentResponse (UpdateInferenceExperimentResponse'),
    newUpdateInferenceExperimentResponse,

    -- ** UpdateModelCard
    UpdateModelCard (UpdateModelCard'),
    newUpdateModelCard,
    UpdateModelCardResponse (UpdateModelCardResponse'),
    newUpdateModelCardResponse,

    -- ** UpdateModelPackage
    UpdateModelPackage (UpdateModelPackage'),
    newUpdateModelPackage,
    UpdateModelPackageResponse (UpdateModelPackageResponse'),
    newUpdateModelPackageResponse,

    -- ** UpdateMonitoringAlert
    UpdateMonitoringAlert (UpdateMonitoringAlert'),
    newUpdateMonitoringAlert,
    UpdateMonitoringAlertResponse (UpdateMonitoringAlertResponse'),
    newUpdateMonitoringAlertResponse,

    -- ** UpdateMonitoringSchedule
    UpdateMonitoringSchedule (UpdateMonitoringSchedule'),
    newUpdateMonitoringSchedule,
    UpdateMonitoringScheduleResponse (UpdateMonitoringScheduleResponse'),
    newUpdateMonitoringScheduleResponse,

    -- ** UpdateNotebookInstance
    UpdateNotebookInstance (UpdateNotebookInstance'),
    newUpdateNotebookInstance,
    UpdateNotebookInstanceResponse (UpdateNotebookInstanceResponse'),
    newUpdateNotebookInstanceResponse,

    -- ** UpdateNotebookInstanceLifecycleConfig
    UpdateNotebookInstanceLifecycleConfig (UpdateNotebookInstanceLifecycleConfig'),
    newUpdateNotebookInstanceLifecycleConfig,
    UpdateNotebookInstanceLifecycleConfigResponse (UpdateNotebookInstanceLifecycleConfigResponse'),
    newUpdateNotebookInstanceLifecycleConfigResponse,

    -- ** UpdatePipeline
    UpdatePipeline (UpdatePipeline'),
    newUpdatePipeline,
    UpdatePipelineResponse (UpdatePipelineResponse'),
    newUpdatePipelineResponse,

    -- ** UpdatePipelineExecution
    UpdatePipelineExecution (UpdatePipelineExecution'),
    newUpdatePipelineExecution,
    UpdatePipelineExecutionResponse (UpdatePipelineExecutionResponse'),
    newUpdatePipelineExecutionResponse,

    -- ** UpdateProject
    UpdateProject (UpdateProject'),
    newUpdateProject,
    UpdateProjectResponse (UpdateProjectResponse'),
    newUpdateProjectResponse,

    -- ** UpdateSpace
    UpdateSpace (UpdateSpace'),
    newUpdateSpace,
    UpdateSpaceResponse (UpdateSpaceResponse'),
    newUpdateSpaceResponse,

    -- ** UpdateTrainingJob
    UpdateTrainingJob (UpdateTrainingJob'),
    newUpdateTrainingJob,
    UpdateTrainingJobResponse (UpdateTrainingJobResponse'),
    newUpdateTrainingJobResponse,

    -- ** UpdateTrial
    UpdateTrial (UpdateTrial'),
    newUpdateTrial,
    UpdateTrialResponse (UpdateTrialResponse'),
    newUpdateTrialResponse,

    -- ** UpdateTrialComponent
    UpdateTrialComponent (UpdateTrialComponent'),
    newUpdateTrialComponent,
    UpdateTrialComponentResponse (UpdateTrialComponentResponse'),
    newUpdateTrialComponentResponse,

    -- ** UpdateUserProfile
    UpdateUserProfile (UpdateUserProfile'),
    newUpdateUserProfile,
    UpdateUserProfileResponse (UpdateUserProfileResponse'),
    newUpdateUserProfileResponse,

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

    -- ** AppSecurityGroupManagement
    AppSecurityGroupManagement (..),

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

    -- ** AutoMLChannelType
    AutoMLChannelType (..),

    -- ** AutoMLJobObjectiveType
    AutoMLJobObjectiveType (..),

    -- ** AutoMLJobSecondaryStatus
    AutoMLJobSecondaryStatus (..),

    -- ** AutoMLJobStatus
    AutoMLJobStatus (..),

    -- ** AutoMLMetricEnum
    AutoMLMetricEnum (..),

    -- ** AutoMLMetricExtendedEnum
    AutoMLMetricExtendedEnum (..),

    -- ** AutoMLMode
    AutoMLMode (..),

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

    -- ** ClarifyFeatureType
    ClarifyFeatureType (..),

    -- ** ClarifyTextGranularity
    ClarifyTextGranularity (..),

    -- ** ClarifyTextLanguage
    ClarifyTextLanguage (..),

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

    -- ** DeviceDeploymentStatus
    DeviceDeploymentStatus (..),

    -- ** DeviceSubsetType
    DeviceSubsetType (..),

    -- ** DirectInternetAccess
    DirectInternetAccess (..),

    -- ** Direction
    Direction (..),

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

    -- ** ExecutionRoleIdentityConfig
    ExecutionRoleIdentityConfig (..),

    -- ** ExecutionStatus
    ExecutionStatus (..),

    -- ** FailureHandlingPolicy
    FailureHandlingPolicy (..),

    -- ** FeatureGroupSortBy
    FeatureGroupSortBy (..),

    -- ** FeatureGroupSortOrder
    FeatureGroupSortOrder (..),

    -- ** FeatureGroupStatus
    FeatureGroupStatus (..),

    -- ** FeatureStatus
    FeatureStatus (..),

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

    -- ** HubContentSortBy
    HubContentSortBy (..),

    -- ** HubContentStatus
    HubContentStatus (..),

    -- ** HubContentType
    HubContentType (..),

    -- ** HubSortBy
    HubSortBy (..),

    -- ** HubStatus
    HubStatus (..),

    -- ** HumanTaskUiStatus
    HumanTaskUiStatus (..),

    -- ** HyperParameterScalingType
    HyperParameterScalingType (..),

    -- ** HyperParameterTuningAllocationStrategy
    HyperParameterTuningAllocationStrategy (..),

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

    -- ** InferenceExperimentStatus
    InferenceExperimentStatus (..),

    -- ** InferenceExperimentStopDesiredState
    InferenceExperimentStopDesiredState (..),

    -- ** InferenceExperimentType
    InferenceExperimentType (..),

    -- ** InputMode
    InputMode (..),

    -- ** InstanceType
    InstanceType (..),

    -- ** JoinSource
    JoinSource (..),

    -- ** LabelingJobStatus
    LabelingJobStatus (..),

    -- ** LastUpdateStatusValue
    LastUpdateStatusValue (..),

    -- ** LineageType
    LineageType (..),

    -- ** ListCompilationJobsSortBy
    ListCompilationJobsSortBy (..),

    -- ** ListDeviceFleetsSortBy
    ListDeviceFleetsSortBy (..),

    -- ** ListEdgeDeploymentPlansSortBy
    ListEdgeDeploymentPlansSortBy (..),

    -- ** ListEdgePackagingJobsSortBy
    ListEdgePackagingJobsSortBy (..),

    -- ** ListInferenceRecommendationsJobsSortBy
    ListInferenceRecommendationsJobsSortBy (..),

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

    -- ** ModelCardExportJobSortBy
    ModelCardExportJobSortBy (..),

    -- ** ModelCardExportJobSortOrder
    ModelCardExportJobSortOrder (..),

    -- ** ModelCardExportJobStatus
    ModelCardExportJobStatus (..),

    -- ** ModelCardProcessingStatus
    ModelCardProcessingStatus (..),

    -- ** ModelCardSortBy
    ModelCardSortBy (..),

    -- ** ModelCardSortOrder
    ModelCardSortOrder (..),

    -- ** ModelCardStatus
    ModelCardStatus (..),

    -- ** ModelCardVersionSortBy
    ModelCardVersionSortBy (..),

    -- ** ModelInfrastructureType
    ModelInfrastructureType (..),

    -- ** ModelMetadataFilterType
    ModelMetadataFilterType (..),

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

    -- ** ModelVariantAction
    ModelVariantAction (..),

    -- ** ModelVariantStatus
    ModelVariantStatus (..),

    -- ** MonitoringAlertHistorySortKey
    MonitoringAlertHistorySortKey (..),

    -- ** MonitoringAlertStatus
    MonitoringAlertStatus (..),

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

    -- ** RStudioServerProAccessStatus
    RStudioServerProAccessStatus (..),

    -- ** RStudioServerProUserGroup
    RStudioServerProUserGroup (..),

    -- ** RecommendationJobStatus
    RecommendationJobStatus (..),

    -- ** RecommendationJobType
    RecommendationJobType (..),

    -- ** RecommendationStepType
    RecommendationStepType (..),

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

    -- ** SortInferenceExperimentsBy
    SortInferenceExperimentsBy (..),

    -- ** SortLineageGroupsBy
    SortLineageGroupsBy (..),

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

    -- ** SpaceSortKey
    SpaceSortKey (..),

    -- ** SpaceStatus
    SpaceStatus (..),

    -- ** SplitType
    SplitType (..),

    -- ** StageStatus
    StageStatus (..),

    -- ** StepStatus
    StepStatus (..),

    -- ** StudioLifecycleConfigAppType
    StudioLifecycleConfigAppType (..),

    -- ** StudioLifecycleConfigSortKey
    StudioLifecycleConfigSortKey (..),

    -- ** TableFormat
    TableFormat (..),

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

    -- ** TrafficType
    TrafficType (..),

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

    -- ** VariantStatus
    VariantStatus (..),

    -- ** WarmPoolResourceStatus
    WarmPoolResourceStatus (..),

    -- ** WorkforceStatus
    WorkforceStatus (..),

    -- ** ActionSource
    ActionSource (ActionSource'),
    newActionSource,

    -- ** ActionSummary
    ActionSummary (ActionSummary'),
    newActionSummary,

    -- ** AdditionalInferenceSpecificationDefinition
    AdditionalInferenceSpecificationDefinition (AdditionalInferenceSpecificationDefinition'),
    newAdditionalInferenceSpecificationDefinition,

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

    -- ** AutoMLCandidateGenerationConfig
    AutoMLCandidateGenerationConfig (AutoMLCandidateGenerationConfig'),
    newAutoMLCandidateGenerationConfig,

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

    -- ** AutoMLDataSplitConfig
    AutoMLDataSplitConfig (AutoMLDataSplitConfig'),
    newAutoMLDataSplitConfig,

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

    -- ** AutoMLJobStepMetadata
    AutoMLJobStepMetadata (AutoMLJobStepMetadata'),
    newAutoMLJobStepMetadata,

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

    -- ** BatchDataCaptureConfig
    BatchDataCaptureConfig (BatchDataCaptureConfig'),
    newBatchDataCaptureConfig,

    -- ** BatchDescribeModelPackageError
    BatchDescribeModelPackageError (BatchDescribeModelPackageError'),
    newBatchDescribeModelPackageError,

    -- ** BatchDescribeModelPackageSummary
    BatchDescribeModelPackageSummary (BatchDescribeModelPackageSummary'),
    newBatchDescribeModelPackageSummary,

    -- ** BatchTransformInput
    BatchTransformInput (BatchTransformInput'),
    newBatchTransformInput,

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

    -- ** CanvasAppSettings
    CanvasAppSettings (CanvasAppSettings'),
    newCanvasAppSettings,

    -- ** CapacitySize
    CapacitySize (CapacitySize'),
    newCapacitySize,

    -- ** CaptureContentTypeHeader
    CaptureContentTypeHeader (CaptureContentTypeHeader'),
    newCaptureContentTypeHeader,

    -- ** CaptureOption
    CaptureOption (CaptureOption'),
    newCaptureOption,

    -- ** CategoricalParameter
    CategoricalParameter (CategoricalParameter'),
    newCategoricalParameter,

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

    -- ** ClarifyCheckStepMetadata
    ClarifyCheckStepMetadata (ClarifyCheckStepMetadata'),
    newClarifyCheckStepMetadata,

    -- ** ClarifyExplainerConfig
    ClarifyExplainerConfig (ClarifyExplainerConfig'),
    newClarifyExplainerConfig,

    -- ** ClarifyInferenceConfig
    ClarifyInferenceConfig (ClarifyInferenceConfig'),
    newClarifyInferenceConfig,

    -- ** ClarifyShapBaselineConfig
    ClarifyShapBaselineConfig (ClarifyShapBaselineConfig'),
    newClarifyShapBaselineConfig,

    -- ** ClarifyShapConfig
    ClarifyShapConfig (ClarifyShapConfig'),
    newClarifyShapConfig,

    -- ** ClarifyTextConfig
    ClarifyTextConfig (ClarifyTextConfig'),
    newClarifyTextConfig,

    -- ** CodeRepository
    CodeRepository (CodeRepository'),
    newCodeRepository,

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

    -- ** DefaultSpaceSettings
    DefaultSpaceSettings (DefaultSpaceSettings'),
    newDefaultSpaceSettings,

    -- ** DeployedImage
    DeployedImage (DeployedImage'),
    newDeployedImage,

    -- ** DeploymentConfig
    DeploymentConfig (DeploymentConfig'),
    newDeploymentConfig,

    -- ** DeploymentStage
    DeploymentStage (DeploymentStage'),
    newDeploymentStage,

    -- ** DeploymentStageStatusSummary
    DeploymentStageStatusSummary (DeploymentStageStatusSummary'),
    newDeploymentStageStatusSummary,

    -- ** DesiredWeightAndCapacity
    DesiredWeightAndCapacity (DesiredWeightAndCapacity'),
    newDesiredWeightAndCapacity,

    -- ** Device
    Device (Device'),
    newDevice,

    -- ** DeviceDeploymentSummary
    DeviceDeploymentSummary (DeviceDeploymentSummary'),
    newDeviceDeploymentSummary,

    -- ** DeviceFleetSummary
    DeviceFleetSummary (DeviceFleetSummary'),
    newDeviceFleetSummary,

    -- ** DeviceSelectionConfig
    DeviceSelectionConfig (DeviceSelectionConfig'),
    newDeviceSelectionConfig,

    -- ** DeviceStats
    DeviceStats (DeviceStats'),
    newDeviceStats,

    -- ** DeviceSummary
    DeviceSummary (DeviceSummary'),
    newDeviceSummary,

    -- ** DomainDetails
    DomainDetails (DomainDetails'),
    newDomainDetails,

    -- ** DomainSettings
    DomainSettings (DomainSettings'),
    newDomainSettings,

    -- ** DomainSettingsForUpdate
    DomainSettingsForUpdate (DomainSettingsForUpdate'),
    newDomainSettingsForUpdate,

    -- ** DriftCheckBaselines
    DriftCheckBaselines (DriftCheckBaselines'),
    newDriftCheckBaselines,

    -- ** DriftCheckBias
    DriftCheckBias (DriftCheckBias'),
    newDriftCheckBias,

    -- ** DriftCheckExplainability
    DriftCheckExplainability (DriftCheckExplainability'),
    newDriftCheckExplainability,

    -- ** DriftCheckModelDataQuality
    DriftCheckModelDataQuality (DriftCheckModelDataQuality'),
    newDriftCheckModelDataQuality,

    -- ** DriftCheckModelQuality
    DriftCheckModelQuality (DriftCheckModelQuality'),
    newDriftCheckModelQuality,

    -- ** EMRStepMetadata
    EMRStepMetadata (EMRStepMetadata'),
    newEMRStepMetadata,

    -- ** Edge
    Edge (Edge'),
    newEdge,

    -- ** EdgeDeploymentConfig
    EdgeDeploymentConfig (EdgeDeploymentConfig'),
    newEdgeDeploymentConfig,

    -- ** EdgeDeploymentModelConfig
    EdgeDeploymentModelConfig (EdgeDeploymentModelConfig'),
    newEdgeDeploymentModelConfig,

    -- ** EdgeDeploymentPlanSummary
    EdgeDeploymentPlanSummary (EdgeDeploymentPlanSummary'),
    newEdgeDeploymentPlanSummary,

    -- ** EdgeDeploymentStatus
    EdgeDeploymentStatus (EdgeDeploymentStatus'),
    newEdgeDeploymentStatus,

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

    -- ** EndpointInfo
    EndpointInfo (EndpointInfo'),
    newEndpointInfo,

    -- ** EndpointInput
    EndpointInput (EndpointInput'),
    newEndpointInput,

    -- ** EndpointInputConfiguration
    EndpointInputConfiguration (EndpointInputConfiguration'),
    newEndpointInputConfiguration,

    -- ** EndpointMetadata
    EndpointMetadata (EndpointMetadata'),
    newEndpointMetadata,

    -- ** EndpointOutputConfiguration
    EndpointOutputConfiguration (EndpointOutputConfiguration'),
    newEndpointOutputConfiguration,

    -- ** EndpointPerformance
    EndpointPerformance (EndpointPerformance'),
    newEndpointPerformance,

    -- ** EndpointSummary
    EndpointSummary (EndpointSummary'),
    newEndpointSummary,

    -- ** EnvironmentParameter
    EnvironmentParameter (EnvironmentParameter'),
    newEnvironmentParameter,

    -- ** EnvironmentParameterRanges
    EnvironmentParameterRanges (EnvironmentParameterRanges'),
    newEnvironmentParameterRanges,

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

    -- ** ExplainerConfig
    ExplainerConfig (ExplainerConfig'),
    newExplainerConfig,

    -- ** FailStepMetadata
    FailStepMetadata (FailStepMetadata'),
    newFailStepMetadata,

    -- ** FeatureDefinition
    FeatureDefinition (FeatureDefinition'),
    newFeatureDefinition,

    -- ** FeatureGroup
    FeatureGroup (FeatureGroup'),
    newFeatureGroup,

    -- ** FeatureGroupSummary
    FeatureGroupSummary (FeatureGroupSummary'),
    newFeatureGroupSummary,

    -- ** FeatureMetadata
    FeatureMetadata (FeatureMetadata'),
    newFeatureMetadata,

    -- ** FeatureParameter
    FeatureParameter (FeatureParameter'),
    newFeatureParameter,

    -- ** FileSource
    FileSource (FileSource'),
    newFileSource,

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

    -- ** HubContentDependency
    HubContentDependency (HubContentDependency'),
    newHubContentDependency,

    -- ** HubContentInfo
    HubContentInfo (HubContentInfo'),
    newHubContentInfo,

    -- ** HubInfo
    HubInfo (HubInfo'),
    newHubInfo,

    -- ** HubS3StorageConfig
    HubS3StorageConfig (HubS3StorageConfig'),
    newHubS3StorageConfig,

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

    -- ** HyperParameterTuningInstanceConfig
    HyperParameterTuningInstanceConfig (HyperParameterTuningInstanceConfig'),
    newHyperParameterTuningInstanceConfig,

    -- ** HyperParameterTuningJobConfig
    HyperParameterTuningJobConfig (HyperParameterTuningJobConfig'),
    newHyperParameterTuningJobConfig,

    -- ** HyperParameterTuningJobObjective
    HyperParameterTuningJobObjective (HyperParameterTuningJobObjective'),
    newHyperParameterTuningJobObjective,

    -- ** HyperParameterTuningJobSearchEntity
    HyperParameterTuningJobSearchEntity (HyperParameterTuningJobSearchEntity'),
    newHyperParameterTuningJobSearchEntity,

    -- ** HyperParameterTuningJobStrategyConfig
    HyperParameterTuningJobStrategyConfig (HyperParameterTuningJobStrategyConfig'),
    newHyperParameterTuningJobStrategyConfig,

    -- ** HyperParameterTuningJobSummary
    HyperParameterTuningJobSummary (HyperParameterTuningJobSummary'),
    newHyperParameterTuningJobSummary,

    -- ** HyperParameterTuningJobWarmStartConfig
    HyperParameterTuningJobWarmStartConfig (HyperParameterTuningJobWarmStartConfig'),
    newHyperParameterTuningJobWarmStartConfig,

    -- ** HyperParameterTuningResourceConfig
    HyperParameterTuningResourceConfig (HyperParameterTuningResourceConfig'),
    newHyperParameterTuningResourceConfig,

    -- ** HyperbandStrategyConfig
    HyperbandStrategyConfig (HyperbandStrategyConfig'),
    newHyperbandStrategyConfig,

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

    -- ** InferenceExperimentDataStorageConfig
    InferenceExperimentDataStorageConfig (InferenceExperimentDataStorageConfig'),
    newInferenceExperimentDataStorageConfig,

    -- ** InferenceExperimentSchedule
    InferenceExperimentSchedule (InferenceExperimentSchedule'),
    newInferenceExperimentSchedule,

    -- ** InferenceExperimentSummary
    InferenceExperimentSummary (InferenceExperimentSummary'),
    newInferenceExperimentSummary,

    -- ** InferenceMetrics
    InferenceMetrics (InferenceMetrics'),
    newInferenceMetrics,

    -- ** InferenceRecommendation
    InferenceRecommendation (InferenceRecommendation'),
    newInferenceRecommendation,

    -- ** InferenceRecommendationsJob
    InferenceRecommendationsJob (InferenceRecommendationsJob'),
    newInferenceRecommendationsJob,

    -- ** InferenceRecommendationsJobStep
    InferenceRecommendationsJobStep (InferenceRecommendationsJobStep'),
    newInferenceRecommendationsJobStep,

    -- ** InferenceSpecification
    InferenceSpecification (InferenceSpecification'),
    newInferenceSpecification,

    -- ** InputConfig
    InputConfig (InputConfig'),
    newInputConfig,

    -- ** InstanceGroup
    InstanceGroup (InstanceGroup'),
    newInstanceGroup,

    -- ** InstanceMetadataServiceConfiguration
    InstanceMetadataServiceConfiguration (InstanceMetadataServiceConfiguration'),
    newInstanceMetadataServiceConfiguration,

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

    -- ** LastUpdateStatus
    LastUpdateStatus (LastUpdateStatus'),
    newLastUpdateStatus,

    -- ** LineageGroupSummary
    LineageGroupSummary (LineageGroupSummary'),
    newLineageGroupSummary,

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

    -- ** Model
    Model (Model'),
    newModel,

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

    -- ** ModelCard
    ModelCard (ModelCard'),
    newModelCard,

    -- ** ModelCardExportArtifacts
    ModelCardExportArtifacts (ModelCardExportArtifacts'),
    newModelCardExportArtifacts,

    -- ** ModelCardExportJobSummary
    ModelCardExportJobSummary (ModelCardExportJobSummary'),
    newModelCardExportJobSummary,

    -- ** ModelCardExportOutputConfig
    ModelCardExportOutputConfig (ModelCardExportOutputConfig'),
    newModelCardExportOutputConfig,

    -- ** ModelCardSecurityConfig
    ModelCardSecurityConfig (ModelCardSecurityConfig'),
    newModelCardSecurityConfig,

    -- ** ModelCardSummary
    ModelCardSummary (ModelCardSummary'),
    newModelCardSummary,

    -- ** ModelCardVersionSummary
    ModelCardVersionSummary (ModelCardVersionSummary'),
    newModelCardVersionSummary,

    -- ** ModelClientConfig
    ModelClientConfig (ModelClientConfig'),
    newModelClientConfig,

    -- ** ModelConfiguration
    ModelConfiguration (ModelConfiguration'),
    newModelConfiguration,

    -- ** ModelDashboardEndpoint
    ModelDashboardEndpoint (ModelDashboardEndpoint'),
    newModelDashboardEndpoint,

    -- ** ModelDashboardIndicatorAction
    ModelDashboardIndicatorAction (ModelDashboardIndicatorAction'),
    newModelDashboardIndicatorAction,

    -- ** ModelDashboardModel
    ModelDashboardModel (ModelDashboardModel'),
    newModelDashboardModel,

    -- ** ModelDashboardModelCard
    ModelDashboardModelCard (ModelDashboardModelCard'),
    newModelDashboardModelCard,

    -- ** ModelDashboardMonitoringSchedule
    ModelDashboardMonitoringSchedule (ModelDashboardMonitoringSchedule'),
    newModelDashboardMonitoringSchedule,

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

    -- ** ModelInfrastructureConfig
    ModelInfrastructureConfig (ModelInfrastructureConfig'),
    newModelInfrastructureConfig,

    -- ** ModelInput
    ModelInput (ModelInput'),
    newModelInput,

    -- ** ModelLatencyThreshold
    ModelLatencyThreshold (ModelLatencyThreshold'),
    newModelLatencyThreshold,

    -- ** ModelMetadataFilter
    ModelMetadataFilter (ModelMetadataFilter'),
    newModelMetadataFilter,

    -- ** ModelMetadataSearchExpression
    ModelMetadataSearchExpression (ModelMetadataSearchExpression'),
    newModelMetadataSearchExpression,

    -- ** ModelMetadataSummary
    ModelMetadataSummary (ModelMetadataSummary'),
    newModelMetadataSummary,

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

    -- ** ModelVariantConfig
    ModelVariantConfig (ModelVariantConfig'),
    newModelVariantConfig,

    -- ** ModelVariantConfigSummary
    ModelVariantConfigSummary (ModelVariantConfigSummary'),
    newModelVariantConfigSummary,

    -- ** MonitoringAlertActions
    MonitoringAlertActions (MonitoringAlertActions'),
    newMonitoringAlertActions,

    -- ** MonitoringAlertHistorySummary
    MonitoringAlertHistorySummary (MonitoringAlertHistorySummary'),
    newMonitoringAlertHistorySummary,

    -- ** MonitoringAlertSummary
    MonitoringAlertSummary (MonitoringAlertSummary'),
    newMonitoringAlertSummary,

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

    -- ** MonitoringCsvDatasetFormat
    MonitoringCsvDatasetFormat (MonitoringCsvDatasetFormat'),
    newMonitoringCsvDatasetFormat,

    -- ** MonitoringDatasetFormat
    MonitoringDatasetFormat (MonitoringDatasetFormat'),
    newMonitoringDatasetFormat,

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

    -- ** MonitoringJsonDatasetFormat
    MonitoringJsonDatasetFormat (MonitoringJsonDatasetFormat'),
    newMonitoringJsonDatasetFormat,

    -- ** MonitoringNetworkConfig
    MonitoringNetworkConfig (MonitoringNetworkConfig'),
    newMonitoringNetworkConfig,

    -- ** MonitoringOutput
    MonitoringOutput (MonitoringOutput'),
    newMonitoringOutput,

    -- ** MonitoringOutputConfig
    MonitoringOutputConfig (MonitoringOutputConfig'),
    newMonitoringOutputConfig,

    -- ** MonitoringParquetDatasetFormat
    MonitoringParquetDatasetFormat (MonitoringParquetDatasetFormat'),
    newMonitoringParquetDatasetFormat,

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

    -- ** ParallelismConfiguration
    ParallelismConfiguration (ParallelismConfiguration'),
    newParallelismConfiguration,

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

    -- ** PendingDeploymentSummary
    PendingDeploymentSummary (PendingDeploymentSummary'),
    newPendingDeploymentSummary,

    -- ** PendingProductionVariantSummary
    PendingProductionVariantSummary (PendingProductionVariantSummary'),
    newPendingProductionVariantSummary,

    -- ** Phase
    Phase (Phase'),
    newPhase,

    -- ** Pipeline
    Pipeline (Pipeline'),
    newPipeline,

    -- ** PipelineDefinitionS3Location
    PipelineDefinitionS3Location (PipelineDefinitionS3Location'),
    newPipelineDefinitionS3Location,

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

    -- ** ProductionVariantServerlessConfig
    ProductionVariantServerlessConfig (ProductionVariantServerlessConfig'),
    newProductionVariantServerlessConfig,

    -- ** ProductionVariantStatus
    ProductionVariantStatus (ProductionVariantStatus'),
    newProductionVariantStatus,

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

    -- ** QualityCheckStepMetadata
    QualityCheckStepMetadata (QualityCheckStepMetadata'),
    newQualityCheckStepMetadata,

    -- ** QueryFilters
    QueryFilters (QueryFilters'),
    newQueryFilters,

    -- ** RSessionAppSettings
    RSessionAppSettings (RSessionAppSettings'),
    newRSessionAppSettings,

    -- ** RStudioServerProAppSettings
    RStudioServerProAppSettings (RStudioServerProAppSettings'),
    newRStudioServerProAppSettings,

    -- ** RStudioServerProDomainSettings
    RStudioServerProDomainSettings (RStudioServerProDomainSettings'),
    newRStudioServerProDomainSettings,

    -- ** RStudioServerProDomainSettingsForUpdate
    RStudioServerProDomainSettingsForUpdate (RStudioServerProDomainSettingsForUpdate'),
    newRStudioServerProDomainSettingsForUpdate,

    -- ** RealTimeInferenceConfig
    RealTimeInferenceConfig (RealTimeInferenceConfig'),
    newRealTimeInferenceConfig,

    -- ** RecommendationJobCompiledOutputConfig
    RecommendationJobCompiledOutputConfig (RecommendationJobCompiledOutputConfig'),
    newRecommendationJobCompiledOutputConfig,

    -- ** RecommendationJobContainerConfig
    RecommendationJobContainerConfig (RecommendationJobContainerConfig'),
    newRecommendationJobContainerConfig,

    -- ** RecommendationJobInferenceBenchmark
    RecommendationJobInferenceBenchmark (RecommendationJobInferenceBenchmark'),
    newRecommendationJobInferenceBenchmark,

    -- ** RecommendationJobInputConfig
    RecommendationJobInputConfig (RecommendationJobInputConfig'),
    newRecommendationJobInputConfig,

    -- ** RecommendationJobOutputConfig
    RecommendationJobOutputConfig (RecommendationJobOutputConfig'),
    newRecommendationJobOutputConfig,

    -- ** RecommendationJobPayloadConfig
    RecommendationJobPayloadConfig (RecommendationJobPayloadConfig'),
    newRecommendationJobPayloadConfig,

    -- ** RecommendationJobResourceLimit
    RecommendationJobResourceLimit (RecommendationJobResourceLimit'),
    newRecommendationJobResourceLimit,

    -- ** RecommendationJobStoppingConditions
    RecommendationJobStoppingConditions (RecommendationJobStoppingConditions'),
    newRecommendationJobStoppingConditions,

    -- ** RecommendationMetrics
    RecommendationMetrics (RecommendationMetrics'),
    newRecommendationMetrics,

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

    -- ** ResourceConfigForUpdate
    ResourceConfigForUpdate (ResourceConfigForUpdate'),
    newResourceConfigForUpdate,

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

    -- ** ServiceCatalogProvisioningUpdateDetails
    ServiceCatalogProvisioningUpdateDetails (ServiceCatalogProvisioningUpdateDetails'),
    newServiceCatalogProvisioningUpdateDetails,

    -- ** ShadowModeConfig
    ShadowModeConfig (ShadowModeConfig'),
    newShadowModeConfig,

    -- ** ShadowModelVariantConfig
    ShadowModelVariantConfig (ShadowModelVariantConfig'),
    newShadowModelVariantConfig,

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

    -- ** SpaceDetails
    SpaceDetails (SpaceDetails'),
    newSpaceDetails,

    -- ** SpaceSettings
    SpaceSettings (SpaceSettings'),
    newSpaceSettings,

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

    -- ** TimeSeriesForecastingSettings
    TimeSeriesForecastingSettings (TimeSeriesForecastingSettings'),
    newTimeSeriesForecastingSettings,

    -- ** TrafficPattern
    TrafficPattern (TrafficPattern'),
    newTrafficPattern,

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

    -- ** Vertex
    Vertex (Vertex'),
    newVertex,

    -- ** VpcConfig
    VpcConfig (VpcConfig'),
    newVpcConfig,

    -- ** WarmPoolStatus
    WarmPoolStatus (WarmPoolStatus'),
    newWarmPoolStatus,

    -- ** Workforce
    Workforce (Workforce'),
    newWorkforce,

    -- ** WorkforceVpcConfigRequest
    WorkforceVpcConfigRequest (WorkforceVpcConfigRequest'),
    newWorkforceVpcConfigRequest,

    -- ** WorkforceVpcConfigResponse
    WorkforceVpcConfigResponse (WorkforceVpcConfigResponse'),
    newWorkforceVpcConfigResponse,

    -- ** Workteam
    Workteam (Workteam'),
    newWorkteam,
  )
where

import Amazonka.SageMaker.AddAssociation
import Amazonka.SageMaker.AddTags
import Amazonka.SageMaker.AssociateTrialComponent
import Amazonka.SageMaker.BatchDescribeModelPackage
import Amazonka.SageMaker.CreateAction
import Amazonka.SageMaker.CreateAlgorithm
import Amazonka.SageMaker.CreateApp
import Amazonka.SageMaker.CreateAppImageConfig
import Amazonka.SageMaker.CreateArtifact
import Amazonka.SageMaker.CreateAutoMLJob
import Amazonka.SageMaker.CreateCodeRepository
import Amazonka.SageMaker.CreateCompilationJob
import Amazonka.SageMaker.CreateContext
import Amazonka.SageMaker.CreateDataQualityJobDefinition
import Amazonka.SageMaker.CreateDeviceFleet
import Amazonka.SageMaker.CreateDomain
import Amazonka.SageMaker.CreateEdgeDeploymentPlan
import Amazonka.SageMaker.CreateEdgeDeploymentStage
import Amazonka.SageMaker.CreateEdgePackagingJob
import Amazonka.SageMaker.CreateEndpoint
import Amazonka.SageMaker.CreateEndpointConfig
import Amazonka.SageMaker.CreateExperiment
import Amazonka.SageMaker.CreateFeatureGroup
import Amazonka.SageMaker.CreateFlowDefinition
import Amazonka.SageMaker.CreateHub
import Amazonka.SageMaker.CreateHumanTaskUi
import Amazonka.SageMaker.CreateHyperParameterTuningJob
import Amazonka.SageMaker.CreateImage
import Amazonka.SageMaker.CreateImageVersion
import Amazonka.SageMaker.CreateInferenceExperiment
import Amazonka.SageMaker.CreateInferenceRecommendationsJob
import Amazonka.SageMaker.CreateLabelingJob
import Amazonka.SageMaker.CreateModel
import Amazonka.SageMaker.CreateModelBiasJobDefinition
import Amazonka.SageMaker.CreateModelCard
import Amazonka.SageMaker.CreateModelCardExportJob
import Amazonka.SageMaker.CreateModelExplainabilityJobDefinition
import Amazonka.SageMaker.CreateModelPackage
import Amazonka.SageMaker.CreateModelPackageGroup
import Amazonka.SageMaker.CreateModelQualityJobDefinition
import Amazonka.SageMaker.CreateMonitoringSchedule
import Amazonka.SageMaker.CreateNotebookInstance
import Amazonka.SageMaker.CreateNotebookInstanceLifecycleConfig
import Amazonka.SageMaker.CreatePipeline
import Amazonka.SageMaker.CreatePresignedDomainUrl
import Amazonka.SageMaker.CreatePresignedNotebookInstanceUrl
import Amazonka.SageMaker.CreateProcessingJob
import Amazonka.SageMaker.CreateProject
import Amazonka.SageMaker.CreateSpace
import Amazonka.SageMaker.CreateStudioLifecycleConfig
import Amazonka.SageMaker.CreateTrainingJob
import Amazonka.SageMaker.CreateTransformJob
import Amazonka.SageMaker.CreateTrial
import Amazonka.SageMaker.CreateTrialComponent
import Amazonka.SageMaker.CreateUserProfile
import Amazonka.SageMaker.CreateWorkforce
import Amazonka.SageMaker.CreateWorkteam
import Amazonka.SageMaker.DeleteAction
import Amazonka.SageMaker.DeleteAlgorithm
import Amazonka.SageMaker.DeleteApp
import Amazonka.SageMaker.DeleteAppImageConfig
import Amazonka.SageMaker.DeleteArtifact
import Amazonka.SageMaker.DeleteAssociation
import Amazonka.SageMaker.DeleteCodeRepository
import Amazonka.SageMaker.DeleteContext
import Amazonka.SageMaker.DeleteDataQualityJobDefinition
import Amazonka.SageMaker.DeleteDeviceFleet
import Amazonka.SageMaker.DeleteDomain
import Amazonka.SageMaker.DeleteEdgeDeploymentPlan
import Amazonka.SageMaker.DeleteEdgeDeploymentStage
import Amazonka.SageMaker.DeleteEndpoint
import Amazonka.SageMaker.DeleteEndpointConfig
import Amazonka.SageMaker.DeleteExperiment
import Amazonka.SageMaker.DeleteFeatureGroup
import Amazonka.SageMaker.DeleteFlowDefinition
import Amazonka.SageMaker.DeleteHub
import Amazonka.SageMaker.DeleteHubContent
import Amazonka.SageMaker.DeleteHumanTaskUi
import Amazonka.SageMaker.DeleteImage
import Amazonka.SageMaker.DeleteImageVersion
import Amazonka.SageMaker.DeleteInferenceExperiment
import Amazonka.SageMaker.DeleteModel
import Amazonka.SageMaker.DeleteModelBiasJobDefinition
import Amazonka.SageMaker.DeleteModelCard
import Amazonka.SageMaker.DeleteModelExplainabilityJobDefinition
import Amazonka.SageMaker.DeleteModelPackage
import Amazonka.SageMaker.DeleteModelPackageGroup
import Amazonka.SageMaker.DeleteModelPackageGroupPolicy
import Amazonka.SageMaker.DeleteModelQualityJobDefinition
import Amazonka.SageMaker.DeleteMonitoringSchedule
import Amazonka.SageMaker.DeleteNotebookInstance
import Amazonka.SageMaker.DeleteNotebookInstanceLifecycleConfig
import Amazonka.SageMaker.DeletePipeline
import Amazonka.SageMaker.DeleteProject
import Amazonka.SageMaker.DeleteSpace
import Amazonka.SageMaker.DeleteStudioLifecycleConfig
import Amazonka.SageMaker.DeleteTags
import Amazonka.SageMaker.DeleteTrial
import Amazonka.SageMaker.DeleteTrialComponent
import Amazonka.SageMaker.DeleteUserProfile
import Amazonka.SageMaker.DeleteWorkforce
import Amazonka.SageMaker.DeleteWorkteam
import Amazonka.SageMaker.DeregisterDevices
import Amazonka.SageMaker.DescribeAction
import Amazonka.SageMaker.DescribeAlgorithm
import Amazonka.SageMaker.DescribeApp
import Amazonka.SageMaker.DescribeAppImageConfig
import Amazonka.SageMaker.DescribeArtifact
import Amazonka.SageMaker.DescribeAutoMLJob
import Amazonka.SageMaker.DescribeCodeRepository
import Amazonka.SageMaker.DescribeCompilationJob
import Amazonka.SageMaker.DescribeContext
import Amazonka.SageMaker.DescribeDataQualityJobDefinition
import Amazonka.SageMaker.DescribeDevice
import Amazonka.SageMaker.DescribeDeviceFleet
import Amazonka.SageMaker.DescribeDomain
import Amazonka.SageMaker.DescribeEdgeDeploymentPlan
import Amazonka.SageMaker.DescribeEdgePackagingJob
import Amazonka.SageMaker.DescribeEndpoint
import Amazonka.SageMaker.DescribeEndpointConfig
import Amazonka.SageMaker.DescribeExperiment
import Amazonka.SageMaker.DescribeFeatureGroup
import Amazonka.SageMaker.DescribeFeatureMetadata
import Amazonka.SageMaker.DescribeFlowDefinition
import Amazonka.SageMaker.DescribeHub
import Amazonka.SageMaker.DescribeHubContent
import Amazonka.SageMaker.DescribeHumanTaskUi
import Amazonka.SageMaker.DescribeHyperParameterTuningJob
import Amazonka.SageMaker.DescribeImage
import Amazonka.SageMaker.DescribeImageVersion
import Amazonka.SageMaker.DescribeInferenceExperiment
import Amazonka.SageMaker.DescribeInferenceRecommendationsJob
import Amazonka.SageMaker.DescribeLabelingJob
import Amazonka.SageMaker.DescribeLineageGroup
import Amazonka.SageMaker.DescribeModel
import Amazonka.SageMaker.DescribeModelBiasJobDefinition
import Amazonka.SageMaker.DescribeModelCard
import Amazonka.SageMaker.DescribeModelCardExportJob
import Amazonka.SageMaker.DescribeModelExplainabilityJobDefinition
import Amazonka.SageMaker.DescribeModelPackage
import Amazonka.SageMaker.DescribeModelPackageGroup
import Amazonka.SageMaker.DescribeModelQualityJobDefinition
import Amazonka.SageMaker.DescribeMonitoringSchedule
import Amazonka.SageMaker.DescribeNotebookInstance
import Amazonka.SageMaker.DescribeNotebookInstanceLifecycleConfig
import Amazonka.SageMaker.DescribePipeline
import Amazonka.SageMaker.DescribePipelineDefinitionForExecution
import Amazonka.SageMaker.DescribePipelineExecution
import Amazonka.SageMaker.DescribeProcessingJob
import Amazonka.SageMaker.DescribeProject
import Amazonka.SageMaker.DescribeSpace
import Amazonka.SageMaker.DescribeStudioLifecycleConfig
import Amazonka.SageMaker.DescribeSubscribedWorkteam
import Amazonka.SageMaker.DescribeTrainingJob
import Amazonka.SageMaker.DescribeTransformJob
import Amazonka.SageMaker.DescribeTrial
import Amazonka.SageMaker.DescribeTrialComponent
import Amazonka.SageMaker.DescribeUserProfile
import Amazonka.SageMaker.DescribeWorkforce
import Amazonka.SageMaker.DescribeWorkteam
import Amazonka.SageMaker.DisableSagemakerServicecatalogPortfolio
import Amazonka.SageMaker.DisassociateTrialComponent
import Amazonka.SageMaker.EnableSagemakerServicecatalogPortfolio
import Amazonka.SageMaker.GetDeviceFleetReport
import Amazonka.SageMaker.GetLineageGroupPolicy
import Amazonka.SageMaker.GetModelPackageGroupPolicy
import Amazonka.SageMaker.GetSagemakerServicecatalogPortfolioStatus
import Amazonka.SageMaker.GetSearchSuggestions
import Amazonka.SageMaker.ImportHubContent
import Amazonka.SageMaker.Lens
import Amazonka.SageMaker.ListActions
import Amazonka.SageMaker.ListAlgorithms
import Amazonka.SageMaker.ListAppImageConfigs
import Amazonka.SageMaker.ListApps
import Amazonka.SageMaker.ListArtifacts
import Amazonka.SageMaker.ListAssociations
import Amazonka.SageMaker.ListAutoMLJobs
import Amazonka.SageMaker.ListCandidatesForAutoMLJob
import Amazonka.SageMaker.ListCodeRepositories
import Amazonka.SageMaker.ListCompilationJobs
import Amazonka.SageMaker.ListContexts
import Amazonka.SageMaker.ListDataQualityJobDefinitions
import Amazonka.SageMaker.ListDeviceFleets
import Amazonka.SageMaker.ListDevices
import Amazonka.SageMaker.ListDomains
import Amazonka.SageMaker.ListEdgeDeploymentPlans
import Amazonka.SageMaker.ListEdgePackagingJobs
import Amazonka.SageMaker.ListEndpointConfigs
import Amazonka.SageMaker.ListEndpoints
import Amazonka.SageMaker.ListExperiments
import Amazonka.SageMaker.ListFeatureGroups
import Amazonka.SageMaker.ListFlowDefinitions
import Amazonka.SageMaker.ListHubContentVersions
import Amazonka.SageMaker.ListHubContents
import Amazonka.SageMaker.ListHubs
import Amazonka.SageMaker.ListHumanTaskUis
import Amazonka.SageMaker.ListHyperParameterTuningJobs
import Amazonka.SageMaker.ListImageVersions
import Amazonka.SageMaker.ListImages
import Amazonka.SageMaker.ListInferenceExperiments
import Amazonka.SageMaker.ListInferenceRecommendationsJobSteps
import Amazonka.SageMaker.ListInferenceRecommendationsJobs
import Amazonka.SageMaker.ListLabelingJobs
import Amazonka.SageMaker.ListLabelingJobsForWorkteam
import Amazonka.SageMaker.ListLineageGroups
import Amazonka.SageMaker.ListModelBiasJobDefinitions
import Amazonka.SageMaker.ListModelCardExportJobs
import Amazonka.SageMaker.ListModelCardVersions
import Amazonka.SageMaker.ListModelCards
import Amazonka.SageMaker.ListModelExplainabilityJobDefinitions
import Amazonka.SageMaker.ListModelMetadata
import Amazonka.SageMaker.ListModelPackageGroups
import Amazonka.SageMaker.ListModelPackages
import Amazonka.SageMaker.ListModelQualityJobDefinitions
import Amazonka.SageMaker.ListModels
import Amazonka.SageMaker.ListMonitoringAlertHistory
import Amazonka.SageMaker.ListMonitoringAlerts
import Amazonka.SageMaker.ListMonitoringExecutions
import Amazonka.SageMaker.ListMonitoringSchedules
import Amazonka.SageMaker.ListNotebookInstanceLifecycleConfigs
import Amazonka.SageMaker.ListNotebookInstances
import Amazonka.SageMaker.ListPipelineExecutionSteps
import Amazonka.SageMaker.ListPipelineExecutions
import Amazonka.SageMaker.ListPipelineParametersForExecution
import Amazonka.SageMaker.ListPipelines
import Amazonka.SageMaker.ListProcessingJobs
import Amazonka.SageMaker.ListProjects
import Amazonka.SageMaker.ListSpaces
import Amazonka.SageMaker.ListStageDevices
import Amazonka.SageMaker.ListStudioLifecycleConfigs
import Amazonka.SageMaker.ListSubscribedWorkteams
import Amazonka.SageMaker.ListTags
import Amazonka.SageMaker.ListTrainingJobs
import Amazonka.SageMaker.ListTrainingJobsForHyperParameterTuningJob
import Amazonka.SageMaker.ListTransformJobs
import Amazonka.SageMaker.ListTrialComponents
import Amazonka.SageMaker.ListTrials
import Amazonka.SageMaker.ListUserProfiles
import Amazonka.SageMaker.ListWorkforces
import Amazonka.SageMaker.ListWorkteams
import Amazonka.SageMaker.PutModelPackageGroupPolicy
import Amazonka.SageMaker.QueryLineage
import Amazonka.SageMaker.RegisterDevices
import Amazonka.SageMaker.RenderUiTemplate
import Amazonka.SageMaker.RetryPipelineExecution
import Amazonka.SageMaker.Search
import Amazonka.SageMaker.SendPipelineExecutionStepFailure
import Amazonka.SageMaker.SendPipelineExecutionStepSuccess
import Amazonka.SageMaker.StartEdgeDeploymentStage
import Amazonka.SageMaker.StartInferenceExperiment
import Amazonka.SageMaker.StartMonitoringSchedule
import Amazonka.SageMaker.StartNotebookInstance
import Amazonka.SageMaker.StartPipelineExecution
import Amazonka.SageMaker.StopAutoMLJob
import Amazonka.SageMaker.StopCompilationJob
import Amazonka.SageMaker.StopEdgeDeploymentStage
import Amazonka.SageMaker.StopEdgePackagingJob
import Amazonka.SageMaker.StopHyperParameterTuningJob
import Amazonka.SageMaker.StopInferenceExperiment
import Amazonka.SageMaker.StopInferenceRecommendationsJob
import Amazonka.SageMaker.StopLabelingJob
import Amazonka.SageMaker.StopMonitoringSchedule
import Amazonka.SageMaker.StopNotebookInstance
import Amazonka.SageMaker.StopPipelineExecution
import Amazonka.SageMaker.StopProcessingJob
import Amazonka.SageMaker.StopTrainingJob
import Amazonka.SageMaker.StopTransformJob
import Amazonka.SageMaker.Types
import Amazonka.SageMaker.UpdateAction
import Amazonka.SageMaker.UpdateAppImageConfig
import Amazonka.SageMaker.UpdateArtifact
import Amazonka.SageMaker.UpdateCodeRepository
import Amazonka.SageMaker.UpdateContext
import Amazonka.SageMaker.UpdateDeviceFleet
import Amazonka.SageMaker.UpdateDevices
import Amazonka.SageMaker.UpdateDomain
import Amazonka.SageMaker.UpdateEndpoint
import Amazonka.SageMaker.UpdateEndpointWeightsAndCapacities
import Amazonka.SageMaker.UpdateExperiment
import Amazonka.SageMaker.UpdateFeatureGroup
import Amazonka.SageMaker.UpdateFeatureMetadata
import Amazonka.SageMaker.UpdateHub
import Amazonka.SageMaker.UpdateImage
import Amazonka.SageMaker.UpdateInferenceExperiment
import Amazonka.SageMaker.UpdateModelCard
import Amazonka.SageMaker.UpdateModelPackage
import Amazonka.SageMaker.UpdateMonitoringAlert
import Amazonka.SageMaker.UpdateMonitoringSchedule
import Amazonka.SageMaker.UpdateNotebookInstance
import Amazonka.SageMaker.UpdateNotebookInstanceLifecycleConfig
import Amazonka.SageMaker.UpdatePipeline
import Amazonka.SageMaker.UpdatePipelineExecution
import Amazonka.SageMaker.UpdateProject
import Amazonka.SageMaker.UpdateSpace
import Amazonka.SageMaker.UpdateTrainingJob
import Amazonka.SageMaker.UpdateTrial
import Amazonka.SageMaker.UpdateTrialComponent
import Amazonka.SageMaker.UpdateUserProfile
import Amazonka.SageMaker.UpdateWorkforce
import Amazonka.SageMaker.UpdateWorkteam
import Amazonka.SageMaker.Waiters

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

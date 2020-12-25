{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides APIs for creating and managing Amazon SageMaker resources.
--
-- Other Resources:
--
--     * <https://docs.aws.amazon.com/sagemaker/latest/dg/whatis.html#first-time-user Amazon SageMaker Developer Guide>
--
--
--     * <https://docs.aws.amazon.com/augmented-ai/2019-11-07/APIReference/Welcome.html Amazon Augmented AI Runtime API Reference>
module Network.AWS.SageMaker
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** ResourceLimitExceeded
    _ResourceLimitExceeded,

    -- ** ResourceInUse
    _ResourceInUse,

    -- ** ConflictException
    _ConflictException,

    -- ** ResourceNotFound
    _ResourceNotFound,

    -- * Waiters
    -- $waiters

    -- ** NotebookInstanceDeleted
    mkNotebookInstanceDeleted,

    -- ** EndpointDeleted
    mkEndpointDeleted,

    -- ** EndpointInService
    mkEndpointInService,

    -- ** TransformJobCompletedOrStopped
    mkTransformJobCompletedOrStopped,

    -- ** NotebookInstanceInService
    mkNotebookInstanceInService,

    -- ** ProcessingJobCompletedOrStopped
    mkProcessingJobCompletedOrStopped,

    -- ** TrainingJobCompletedOrStopped
    mkTrainingJobCompletedOrStopped,

    -- ** NotebookInstanceStopped
    mkNotebookInstanceStopped,

    -- * Operations
    -- $operations

    -- ** CreateNotebookInstance
    module Network.AWS.SageMaker.CreateNotebookInstance,

    -- ** DeleteModelPackage
    module Network.AWS.SageMaker.DeleteModelPackage,

    -- ** DescribeMonitoringSchedule
    module Network.AWS.SageMaker.DescribeMonitoringSchedule,

    -- ** ListTrialComponents (Paginated)
    module Network.AWS.SageMaker.ListTrialComponents,

    -- ** DescribeEndpointConfig
    module Network.AWS.SageMaker.DescribeEndpointConfig,

    -- ** DescribeApp
    module Network.AWS.SageMaker.DescribeApp,

    -- ** ListImageVersions (Paginated)
    module Network.AWS.SageMaker.ListImageVersions,

    -- ** DescribeAutoMLJob
    module Network.AWS.SageMaker.DescribeAutoMLJob,

    -- ** StopProcessingJob
    module Network.AWS.SageMaker.StopProcessingJob,

    -- ** ListLabelingJobsForWorkteam (Paginated)
    module Network.AWS.SageMaker.ListLabelingJobsForWorkteam,

    -- ** CreateTransformJob
    module Network.AWS.SageMaker.CreateTransformJob,

    -- ** ListCompilationJobs (Paginated)
    module Network.AWS.SageMaker.ListCompilationJobs,

    -- ** DisassociateTrialComponent
    module Network.AWS.SageMaker.DisassociateTrialComponent,

    -- ** StopHyperParameterTuningJob
    module Network.AWS.SageMaker.StopHyperParameterTuningJob,

    -- ** ListHumanTaskUis (Paginated)
    module Network.AWS.SageMaker.ListHumanTaskUis,

    -- ** CreateEndpoint
    module Network.AWS.SageMaker.CreateEndpoint,

    -- ** GetSearchSuggestions
    module Network.AWS.SageMaker.GetSearchSuggestions,

    -- ** DescribeTrial
    module Network.AWS.SageMaker.DescribeTrial,

    -- ** CreatePresignedDomainUrl
    module Network.AWS.SageMaker.CreatePresignedDomainUrl,

    -- ** DescribeCodeRepository
    module Network.AWS.SageMaker.DescribeCodeRepository,

    -- ** DescribeImage
    module Network.AWS.SageMaker.DescribeImage,

    -- ** DescribeTrainingJob
    module Network.AWS.SageMaker.DescribeTrainingJob,

    -- ** DeleteEndpoint
    module Network.AWS.SageMaker.DeleteEndpoint,

    -- ** UpdateEndpoint
    module Network.AWS.SageMaker.UpdateEndpoint,

    -- ** CreateHumanTaskUi
    module Network.AWS.SageMaker.CreateHumanTaskUi,

    -- ** CreateCompilationJob
    module Network.AWS.SageMaker.CreateCompilationJob,

    -- ** DeleteAppImageConfig
    module Network.AWS.SageMaker.DeleteAppImageConfig,

    -- ** UpdateAppImageConfig
    module Network.AWS.SageMaker.UpdateAppImageConfig,

    -- ** DeleteNotebookInstanceLifecycleConfig
    module Network.AWS.SageMaker.DeleteNotebookInstanceLifecycleConfig,

    -- ** UpdateNotebookInstanceLifecycleConfig
    module Network.AWS.SageMaker.UpdateNotebookInstanceLifecycleConfig,

    -- ** DeleteWorkforce
    module Network.AWS.SageMaker.DeleteWorkforce,

    -- ** UpdateWorkforce
    module Network.AWS.SageMaker.UpdateWorkforce,

    -- ** ListProcessingJobs (Paginated)
    module Network.AWS.SageMaker.ListProcessingJobs,

    -- ** CreateLabelingJob
    module Network.AWS.SageMaker.CreateLabelingJob,

    -- ** DescribeNotebookInstance
    module Network.AWS.SageMaker.DescribeNotebookInstance,

    -- ** CreateMonitoringSchedule
    module Network.AWS.SageMaker.CreateMonitoringSchedule,

    -- ** ListAppImageConfigs
    module Network.AWS.SageMaker.ListAppImageConfigs,

    -- ** CreateEndpointConfig
    module Network.AWS.SageMaker.CreateEndpointConfig,

    -- ** StopNotebookInstance
    module Network.AWS.SageMaker.StopNotebookInstance,

    -- ** UpdateEndpointWeightsAndCapacities
    module Network.AWS.SageMaker.UpdateEndpointWeightsAndCapacities,

    -- ** CreateAppImageConfig
    module Network.AWS.SageMaker.CreateAppImageConfig,

    -- ** DeleteTags
    module Network.AWS.SageMaker.DeleteTags,

    -- ** ListExperiments (Paginated)
    module Network.AWS.SageMaker.ListExperiments,

    -- ** ListAutoMLJobs (Paginated)
    module Network.AWS.SageMaker.ListAutoMLJobs,

    -- ** ListApps (Paginated)
    module Network.AWS.SageMaker.ListApps,

    -- ** CreateProcessingJob
    module Network.AWS.SageMaker.CreateProcessingJob,

    -- ** DeleteMonitoringSchedule
    module Network.AWS.SageMaker.DeleteMonitoringSchedule,

    -- ** DescribeModelPackage
    module Network.AWS.SageMaker.DescribeModelPackage,

    -- ** DeleteEndpointConfig
    module Network.AWS.SageMaker.DeleteEndpointConfig,

    -- ** UpdateMonitoringSchedule
    module Network.AWS.SageMaker.UpdateMonitoringSchedule,

    -- ** DeleteApp
    module Network.AWS.SageMaker.DeleteApp,

    -- ** CreateAlgorithm
    module Network.AWS.SageMaker.CreateAlgorithm,

    -- ** StopTransformJob
    module Network.AWS.SageMaker.StopTransformJob,

    -- ** CreateModel
    module Network.AWS.SageMaker.CreateModel,

    -- ** ListUserProfiles (Paginated)
    module Network.AWS.SageMaker.ListUserProfiles,

    -- ** CreateCodeRepository
    module Network.AWS.SageMaker.CreateCodeRepository,

    -- ** CreateHyperParameterTuningJob
    module Network.AWS.SageMaker.CreateHyperParameterTuningJob,

    -- ** DeleteTrial
    module Network.AWS.SageMaker.DeleteTrial,

    -- ** UpdateTrial
    module Network.AWS.SageMaker.UpdateTrial,

    -- ** ListCodeRepositories (Paginated)
    module Network.AWS.SageMaker.ListCodeRepositories,

    -- ** DescribeCompilationJob
    module Network.AWS.SageMaker.DescribeCompilationJob,

    -- ** ListHyperParameterTuningJobs (Paginated)
    module Network.AWS.SageMaker.ListHyperParameterTuningJobs,

    -- ** ListAlgorithms (Paginated)
    module Network.AWS.SageMaker.ListAlgorithms,

    -- ** RenderUiTemplate
    module Network.AWS.SageMaker.RenderUiTemplate,

    -- ** DeleteFlowDefinition
    module Network.AWS.SageMaker.DeleteFlowDefinition,

    -- ** CreateTrial
    module Network.AWS.SageMaker.CreateTrial,

    -- ** DeleteModel
    module Network.AWS.SageMaker.DeleteModel,

    -- ** ListModels (Paginated)
    module Network.AWS.SageMaker.ListModels,

    -- ** DeleteAlgorithm
    module Network.AWS.SageMaker.DeleteAlgorithm,

    -- ** AssociateTrialComponent
    module Network.AWS.SageMaker.AssociateTrialComponent,

    -- ** DescribeNotebookInstanceLifecycleConfig
    module Network.AWS.SageMaker.DescribeNotebookInstanceLifecycleConfig,

    -- ** DescribeWorkforce
    module Network.AWS.SageMaker.DescribeWorkforce,

    -- ** CreateModelPackage
    module Network.AWS.SageMaker.CreateModelPackage,

    -- ** StopMonitoringSchedule
    module Network.AWS.SageMaker.StopMonitoringSchedule,

    -- ** DescribeAppImageConfig
    module Network.AWS.SageMaker.DescribeAppImageConfig,

    -- ** ListNotebookInstances (Paginated)
    module Network.AWS.SageMaker.ListNotebookInstances,

    -- ** StopLabelingJob
    module Network.AWS.SageMaker.StopLabelingJob,

    -- ** DeleteNotebookInstance
    module Network.AWS.SageMaker.DeleteNotebookInstance,

    -- ** UpdateNotebookInstance
    module Network.AWS.SageMaker.UpdateNotebookInstance,

    -- ** ListModelPackages (Paginated)
    module Network.AWS.SageMaker.ListModelPackages,

    -- ** DeleteImageVersion
    module Network.AWS.SageMaker.DeleteImageVersion,

    -- ** DescribeExperiment
    module Network.AWS.SageMaker.DescribeExperiment,

    -- ** DeleteTrialComponent
    module Network.AWS.SageMaker.DeleteTrialComponent,

    -- ** UpdateTrialComponent
    module Network.AWS.SageMaker.UpdateTrialComponent,

    -- ** DescribeLabelingJob
    module Network.AWS.SageMaker.DescribeLabelingJob,

    -- ** CreateDomain
    module Network.AWS.SageMaker.CreateDomain,

    -- ** DescribeUserProfile
    module Network.AWS.SageMaker.DescribeUserProfile,

    -- ** ListMonitoringExecutions (Paginated)
    module Network.AWS.SageMaker.ListMonitoringExecutions,

    -- ** DeleteHumanTaskUi
    module Network.AWS.SageMaker.DeleteHumanTaskUi,

    -- ** StopTrainingJob
    module Network.AWS.SageMaker.StopTrainingJob,

    -- ** DescribeAlgorithm
    module Network.AWS.SageMaker.DescribeAlgorithm,

    -- ** DescribeModel
    module Network.AWS.SageMaker.DescribeModel,

    -- ** ListTransformJobs (Paginated)
    module Network.AWS.SageMaker.ListTransformJobs,

    -- ** DescribeHyperParameterTuningJob
    module Network.AWS.SageMaker.DescribeHyperParameterTuningJob,

    -- ** ListEndpoints (Paginated)
    module Network.AWS.SageMaker.ListEndpoints,

    -- ** DescribeFlowDefinition
    module Network.AWS.SageMaker.DescribeFlowDefinition,

    -- ** CreatePresignedNotebookInstanceUrl
    module Network.AWS.SageMaker.CreatePresignedNotebookInstanceUrl,

    -- ** ListTrainingJobsForHyperParameterTuningJob (Paginated)
    module Network.AWS.SageMaker.ListTrainingJobsForHyperParameterTuningJob,

    -- ** DescribeDomain
    module Network.AWS.SageMaker.DescribeDomain,

    -- ** UpdateWorkteam
    module Network.AWS.SageMaker.UpdateWorkteam,

    -- ** DeleteWorkteam
    module Network.AWS.SageMaker.DeleteWorkteam,

    -- ** ListWorkteams (Paginated)
    module Network.AWS.SageMaker.ListWorkteams,

    -- ** CreateAutoMLJob
    module Network.AWS.SageMaker.CreateAutoMLJob,

    -- ** CreateApp
    module Network.AWS.SageMaker.CreateApp,

    -- ** CreateExperiment
    module Network.AWS.SageMaker.CreateExperiment,

    -- ** ListNotebookInstanceLifecycleConfigs (Paginated)
    module Network.AWS.SageMaker.ListNotebookInstanceLifecycleConfigs,

    -- ** ListWorkforces (Paginated)
    module Network.AWS.SageMaker.ListWorkforces,

    -- ** DescribeSubscribedWorkteam
    module Network.AWS.SageMaker.DescribeSubscribedWorkteam,

    -- ** CreateWorkteam
    module Network.AWS.SageMaker.CreateWorkteam,

    -- ** CreateNotebookInstanceLifecycleConfig
    module Network.AWS.SageMaker.CreateNotebookInstanceLifecycleConfig,

    -- ** ListMonitoringSchedules (Paginated)
    module Network.AWS.SageMaker.ListMonitoringSchedules,

    -- ** ListLabelingJobs (Paginated)
    module Network.AWS.SageMaker.ListLabelingJobs,

    -- ** StartNotebookInstance
    module Network.AWS.SageMaker.StartNotebookInstance,

    -- ** UpdateExperiment
    module Network.AWS.SageMaker.UpdateExperiment,

    -- ** DeleteExperiment
    module Network.AWS.SageMaker.DeleteExperiment,

    -- ** AddTags
    module Network.AWS.SageMaker.AddTags,

    -- ** CreateWorkforce
    module Network.AWS.SageMaker.CreateWorkforce,

    -- ** DescribeTrialComponent
    module Network.AWS.SageMaker.DescribeTrialComponent,

    -- ** DescribeImageVersion
    module Network.AWS.SageMaker.DescribeImageVersion,

    -- ** ListEndpointConfigs (Paginated)
    module Network.AWS.SageMaker.ListEndpointConfigs,

    -- ** CreateFlowDefinition
    module Network.AWS.SageMaker.CreateFlowDefinition,

    -- ** ListTags (Paginated)
    module Network.AWS.SageMaker.ListTags,

    -- ** DescribeHumanTaskUi
    module Network.AWS.SageMaker.DescribeHumanTaskUi,

    -- ** CreateTrainingJob
    module Network.AWS.SageMaker.CreateTrainingJob,

    -- ** DeleteUserProfile
    module Network.AWS.SageMaker.DeleteUserProfile,

    -- ** UpdateUserProfile
    module Network.AWS.SageMaker.UpdateUserProfile,

    -- ** CreateImage
    module Network.AWS.SageMaker.CreateImage,

    -- ** ListTrials (Paginated)
    module Network.AWS.SageMaker.ListTrials,

    -- ** StopCompilationJob
    module Network.AWS.SageMaker.StopCompilationJob,

    -- ** ListImages (Paginated)
    module Network.AWS.SageMaker.ListImages,

    -- ** CreateUserProfile
    module Network.AWS.SageMaker.CreateUserProfile,

    -- ** Search (Paginated)
    module Network.AWS.SageMaker.Search,

    -- ** UpdateCodeRepository
    module Network.AWS.SageMaker.UpdateCodeRepository,

    -- ** DeleteCodeRepository
    module Network.AWS.SageMaker.DeleteCodeRepository,

    -- ** DescribeTransformJob
    module Network.AWS.SageMaker.DescribeTransformJob,

    -- ** ListCandidatesForAutoMLJob (Paginated)
    module Network.AWS.SageMaker.ListCandidatesForAutoMLJob,

    -- ** DeleteImage
    module Network.AWS.SageMaker.DeleteImage,

    -- ** UpdateImage
    module Network.AWS.SageMaker.UpdateImage,

    -- ** ListFlowDefinitions (Paginated)
    module Network.AWS.SageMaker.ListFlowDefinitions,

    -- ** DescribeEndpoint
    module Network.AWS.SageMaker.DescribeEndpoint,

    -- ** ListTrainingJobs (Paginated)
    module Network.AWS.SageMaker.ListTrainingJobs,

    -- ** DescribeWorkteam
    module Network.AWS.SageMaker.DescribeWorkteam,

    -- ** ListSubscribedWorkteams (Paginated)
    module Network.AWS.SageMaker.ListSubscribedWorkteams,

    -- ** DeleteDomain
    module Network.AWS.SageMaker.DeleteDomain,

    -- ** UpdateDomain
    module Network.AWS.SageMaker.UpdateDomain,

    -- ** ListDomains (Paginated)
    module Network.AWS.SageMaker.ListDomains,

    -- ** CreateImageVersion
    module Network.AWS.SageMaker.CreateImageVersion,

    -- ** StartMonitoringSchedule
    module Network.AWS.SageMaker.StartMonitoringSchedule,

    -- ** StopAutoMLJob
    module Network.AWS.SageMaker.StopAutoMLJob,

    -- ** CreateTrialComponent
    module Network.AWS.SageMaker.CreateTrialComponent,

    -- ** DescribeProcessingJob
    module Network.AWS.SageMaker.DescribeProcessingJob,

    -- * Types

    -- ** EndpointConfigArn
    EndpointConfigArn (..),

    -- ** DomainStatus
    DomainStatus (..),

    -- ** Parent
    Parent (..),
    mkParent,
    pExperimentName,
    pTrialName,

    -- ** TrainingJobStatusCounters
    TrainingJobStatusCounters (..),
    mkTrainingJobStatusCounters,
    tjscCompleted,
    tjscInProgress,
    tjscNonRetryableError,
    tjscRetryableError,
    tjscStopped,

    -- ** UiTemplate
    UiTemplate (..),
    mkUiTemplate,
    utContent,

    -- ** LabelingJobArn
    LabelingJobArn (..),

    -- ** CodeRepositoryContains
    CodeRepositoryContains (..),

    -- ** MonitoringScheduleSortKey
    MonitoringScheduleSortKey (..),

    -- ** HyperParameterValue
    HyperParameterValue (..),

    -- ** TransformJobName
    TransformJobName (..),

    -- ** CandidateDefinitionNotebookLocation
    CandidateDefinitionNotebookLocation (..),

    -- ** NameContains
    NameContains (..),

    -- ** UserSettings
    UserSettings (..),
    mkUserSettings,
    usExecutionRole,
    usJupyterServerAppSettings,
    usKernelGatewayAppSettings,
    usSecurityGroups,
    usSharingSettings,
    usTensorBoardAppSettings,

    -- ** NotebookInstanceSortOrder
    NotebookInstanceSortOrder (..),

    -- ** ModelPackageSortBy
    ModelPackageSortBy (..),

    -- ** FailureReason
    FailureReason (..),

    -- ** ModelPackageStatusItem
    ModelPackageStatusItem (..),
    mkModelPackageStatusItem,
    mpsiName,
    mpsiStatus,
    mpsiFailureReason,

    -- ** ProcessingOutput
    ProcessingOutput (..),
    mkProcessingOutput,
    poOutputName,
    poS3Output,

    -- ** MonitoringScheduleArn
    MonitoringScheduleArn (..),

    -- ** LabelingJobOutputConfig
    LabelingJobOutputConfig (..),
    mkLabelingJobOutputConfig,
    ljocS3OutputPath,
    ljocKmsKeyId,
    ljocSnsTopicArn,

    -- ** HyperParameterTuningJobStrategyType
    HyperParameterTuningJobStrategyType (..),

    -- ** Workforce
    Workforce (..),
    mkWorkforce,
    wWorkforceName,
    wWorkforceArn,
    wCognitoConfig,
    wCreateDate,
    wLastUpdatedDate,
    wOidcConfig,
    wSourceIpConfig,
    wSubDomain,

    -- ** OidcMemberDefinition
    OidcMemberDefinition (..),
    mkOidcMemberDefinition,
    omdGroups,

    -- ** ResourceLimits
    ResourceLimits (..),
    mkResourceLimits,
    rlMaxNumberOfTrainingJobs,
    rlMaxParallelTrainingJobs,

    -- ** Framework
    Framework (..),

    -- ** ModelClientConfig
    ModelClientConfig (..),
    mkModelClientConfig,
    mccInvocationsMaxRetries,
    mccInvocationsTimeoutInSeconds,

    -- ** HyperParameterTrainingJobSummary
    HyperParameterTrainingJobSummary (..),
    mkHyperParameterTrainingJobSummary,
    hptjsTrainingJobName,
    hptjsTrainingJobArn,
    hptjsCreationTime,
    hptjsTrainingJobStatus,
    hptjsTunedHyperParameters,
    hptjsFailureReason,
    hptjsFinalHyperParameterTuningJobObjectiveMetric,
    hptjsObjectiveStatus,
    hptjsTrainingEndTime,
    hptjsTrainingJobDefinitionName,
    hptjsTrainingStartTime,
    hptjsTuningJobName,

    -- ** AppSpecification
    AppSpecification (..),
    mkAppSpecification,
    asImageUri,
    asContainerArguments,
    asContainerEntrypoint,

    -- ** MediaType
    MediaType (..),

    -- ** EndpointConfigSortKey
    EndpointConfigSortKey (..),

    -- ** ObjectiveStatusCounters
    ObjectiveStatusCounters (..),
    mkObjectiveStatusCounters,
    oscFailed,
    oscPending,
    oscSucceeded,

    -- ** FileSystemAccessMode
    FileSystemAccessMode (..),

    -- ** String256
    String256 (..),

    -- ** LabelingJobDataSource
    LabelingJobDataSource (..),
    mkLabelingJobDataSource,
    ljdsS3DataSource,
    ljdsSnsDataSource,

    -- ** EndpointName
    EndpointName (..),

    -- ** ClientId
    ClientId (..),

    -- ** Workteam
    Workteam (..),
    mkWorkteam,
    wfWorkteamName,
    wfMemberDefinitions,
    wfWorkteamArn,
    wfDescription,
    wfCreateDate,
    wfLastUpdatedDate,
    wfNotificationConfiguration,
    wfProductListingIds,
    wfSubDomain,
    wfWorkforceArn,

    -- ** EntityName
    EntityName (..),

    -- ** AutoMLContainerDefinition
    AutoMLContainerDefinition (..),
    mkAutoMLContainerDefinition,
    amlcdImage,
    amlcdModelDataUrl,
    amlcdEnvironment,

    -- ** CsvContentType
    CsvContentType (..),

    -- ** PaginationToken
    PaginationToken (..),

    -- ** BatchStrategy
    BatchStrategy (..),

    -- ** TrialComponentParameterValue
    TrialComponentParameterValue (..),
    mkTrialComponentParameterValue,
    tcpvNumberValue,
    tcpvStringValue,

    -- ** AlgorithmValidationProfile
    AlgorithmValidationProfile (..),
    mkAlgorithmValidationProfile,
    avpProfileName,
    avpTrainingJobDefinition,
    avpTransformJobDefinition,

    -- ** EndpointSummary
    EndpointSummary (..),
    mkEndpointSummary,
    esEndpointName,
    esEndpointArn,
    esCreationTime,
    esLastModifiedTime,
    esEndpointStatus,

    -- ** Group
    Group (..),

    -- ** ScheduleConfig
    ScheduleConfig (..),
    mkScheduleConfig,
    scScheduleExpression,

    -- ** SourceAlgorithmSpecification
    SourceAlgorithmSpecification (..),
    mkSourceAlgorithmSpecification,
    sasSourceAlgorithms,

    -- ** String200
    String200 (..),

    -- ** TuningJobCompletionCriteria
    TuningJobCompletionCriteria (..),
    mkTuningJobCompletionCriteria,
    tjccTargetObjectiveMetricValue,

    -- ** ClientSecret
    ClientSecret (..),

    -- ** NotebookInstanceName
    NotebookInstanceName (..),

    -- ** TensorBoardAppSettings
    TensorBoardAppSettings (..),
    mkTensorBoardAppSettings,
    tbasDefaultResourceSpec,

    -- ** KernelGatewayAppSettings
    KernelGatewayAppSettings (..),
    mkKernelGatewayAppSettings,
    kgasCustomImages,
    kgasDefaultResourceSpec,

    -- ** AwsManagedHumanLoopRequestSource
    AwsManagedHumanLoopRequestSource (..),

    -- ** TrainingJob
    TrainingJob (..),
    mkTrainingJob,
    tjAlgorithmSpecification,
    tjAutoMLJobArn,
    tjBillableTimeInSeconds,
    tjCheckpointConfig,
    tjCreationTime,
    tjDebugHookConfig,
    tjDebugRuleConfigurations,
    tjDebugRuleEvaluationStatuses,
    tjEnableInterContainerTrafficEncryption,
    tjEnableManagedSpotTraining,
    tjEnableNetworkIsolation,
    tjExperimentConfig,
    tjFailureReason,
    tjFinalMetricDataList,
    tjHyperParameters,
    tjInputDataConfig,
    tjLabelingJobArn,
    tjLastModifiedTime,
    tjModelArtifacts,
    tjOutputDataConfig,
    tjResourceConfig,
    tjRoleArn,
    tjSecondaryStatus,
    tjSecondaryStatusTransitions,
    tjStoppingCondition,
    tjTags,
    tjTensorBoardOutputConfig,
    tjTrainingEndTime,
    tjTrainingJobArn,
    tjTrainingJobName,
    tjTrainingJobStatus,
    tjTrainingStartTime,
    tjTrainingTimeInSeconds,
    tjTuningJobArn,
    tjVpcConfig,

    -- ** CaptureMode
    CaptureMode (..),

    -- ** ImageContainerImage
    ImageContainerImage (..),

    -- ** AutoMLSortBy
    AutoMLSortBy (..),

    -- ** ArnOrName
    ArnOrName (..),

    -- ** CandidateStepType
    CandidateStepType (..),

    -- ** TransformJobSummary
    TransformJobSummary (..),
    mkTransformJobSummary,
    tjsTransformJobName,
    tjsTransformJobArn,
    tjsCreationTime,
    tjsTransformJobStatus,
    tjsFailureReason,
    tjsLastModifiedTime,
    tjsTransformEndTime,

    -- ** LabelingJobAlgorithmsConfig
    LabelingJobAlgorithmsConfig (..),
    mkLabelingJobAlgorithmsConfig,
    ljacLabelingJobAlgorithmSpecificationArn,
    ljacInitialActiveLearningModelArn,
    ljacLabelingJobResourceConfig,

    -- ** ResourceSpec
    ResourceSpec (..),
    mkResourceSpec,
    rsInstanceType,
    rsSageMakerImageArn,
    rsSageMakerImageVersionArn,

    -- ** ResourceId
    ResourceId (..),

    -- ** TrialArn
    TrialArn (..),

    -- ** HumanLoopActivationConditions
    HumanLoopActivationConditions (..),

    -- ** TrialComponentSimpleSummary
    TrialComponentSimpleSummary (..),
    mkTrialComponentSimpleSummary,
    tcssCreatedBy,
    tcssCreationTime,
    tcssTrialComponentArn,
    tcssTrialComponentName,
    tcssTrialComponentSource,

    -- ** Image
    Image (..),
    mkImage,
    iCreationTime,
    iImageArn,
    iImageName,
    iImageStatus,
    iLastModifiedTime,
    iDescription,
    iDisplayName,
    iFailureReason,

    -- ** MonitoringInput
    MonitoringInput (..),
    mkMonitoringInput,
    miEndpointInput,

    -- ** CaptureContentTypeHeader
    CaptureContentTypeHeader (..),
    mkCaptureContentTypeHeader,
    ccthCsvContentTypes,
    ccthJsonContentTypes,

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** TrialComponentStatusMessage
    TrialComponentStatusMessage (..),

    -- ** DataCaptureConfigSummary
    DataCaptureConfigSummary (..),
    mkDataCaptureConfigSummary,
    dccsEnableCapture,
    dccsCaptureStatus,
    dccsCurrentSamplingPercentage,
    dccsDestinationS3Uri,
    dccsKmsKeyId,

    -- ** PropertyNameQuery
    PropertyNameQuery (..),
    mkPropertyNameQuery,
    pnqPropertyNameHint,

    -- ** FlowDefinitionTaskTitle
    FlowDefinitionTaskTitle (..),

    -- ** CodeRepositoryArn
    CodeRepositoryArn (..),

    -- ** ProcessingResources
    ProcessingResources (..),
    mkProcessingResources,
    prClusterConfig,

    -- ** ImageDisplayName
    ImageDisplayName (..),

    -- ** ModelArn
    ModelArn (..),

    -- ** Trial
    Trial (..),
    mkTrial,
    tCreatedBy,
    tCreationTime,
    tDisplayName,
    tExperimentName,
    tLastModifiedBy,
    tLastModifiedTime,
    tSource,
    tTags,
    tTrialArn,
    tTrialComponentSummaries,
    tTrialName,

    -- ** SnsTopicArn
    SnsTopicArn (..),

    -- ** HyperParameterTuningJobArn
    HyperParameterTuningJobArn (..),

    -- ** ModelArtifacts
    ModelArtifacts (..),
    mkModelArtifacts,
    maS3ModelArtifacts,

    -- ** HyperParameterScalingType
    HyperParameterScalingType (..),

    -- ** USD
    USD (..),
    mkUSD,
    usdCents,
    usdDollars,
    usdTenthFractionsOfACent,

    -- ** CognitoMemberDefinition
    CognitoMemberDefinition (..),
    mkCognitoMemberDefinition,
    cmdUserPool,
    cmdUserGroup,
    cmdClientId,

    -- ** TemplateContentSha256
    TemplateContentSha256 (..),

    -- ** DestinationS3Uri
    DestinationS3Uri (..),

    -- ** ModelSortKey
    ModelSortKey (..),

    -- ** CandidateStepArn
    CandidateStepArn (..),

    -- ** CognitoUserPool
    CognitoUserPool (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** OidcEndpoint
    OidcEndpoint (..),

    -- ** AutoMLSortOrder
    AutoMLSortOrder (..),

    -- ** AlgorithmArn
    AlgorithmArn (..),

    -- ** EnvironmentKey
    EnvironmentKey (..),

    -- ** JobReferenceCodeContains
    JobReferenceCodeContains (..),

    -- ** ClientToken
    ClientToken (..),

    -- ** ParameterValue
    ParameterValue (..),

    -- ** ResourcePropertyName
    ResourcePropertyName (..),

    -- ** LabelingJobSnsDataSource
    LabelingJobSnsDataSource (..),
    mkLabelingJobSnsDataSource,
    ljsdsSnsTopicArn,

    -- ** TargetPlatformOs
    TargetPlatformOs (..),

    -- ** TrainingJobDefinition
    TrainingJobDefinition (..),
    mkTrainingJobDefinition,
    tjdTrainingInputMode,
    tjdInputDataConfig,
    tjdOutputDataConfig,
    tjdResourceConfig,
    tjdStoppingCondition,
    tjdHyperParameters,

    -- ** TrialSourceArn
    TrialSourceArn (..),

    -- ** UiTemplateInfo
    UiTemplateInfo (..),
    mkUiTemplateInfo,
    utiContentSha256,
    utiUrl,

    -- ** MonitoringExecutionSummary
    MonitoringExecutionSummary (..),
    mkMonitoringExecutionSummary,
    mesMonitoringScheduleName,
    mesScheduledTime,
    mesCreationTime,
    mesLastModifiedTime,
    mesMonitoringExecutionStatus,
    mesEndpointName,
    mesFailureReason,
    mesProcessingJobArn,

    -- ** MonitoringScheduleSummary
    MonitoringScheduleSummary (..),
    mkMonitoringScheduleSummary,
    mssMonitoringScheduleName,
    mssMonitoringScheduleArn,
    mssCreationTime,
    mssLastModifiedTime,
    mssMonitoringScheduleStatus,
    mssEndpointName,

    -- ** NotificationConfiguration
    NotificationConfiguration (..),
    mkNotificationConfiguration,
    ncNotificationTopicArn,

    -- ** ShuffleConfig
    ShuffleConfig (..),
    mkShuffleConfig,
    scSeed,

    -- ** ParameterRanges
    ParameterRanges (..),
    mkParameterRanges,
    prCategoricalParameterRanges,
    prContinuousParameterRanges,
    prIntegerParameterRanges,

    -- ** EntityDescription
    EntityDescription (..),

    -- ** HyperParameterAlgorithmSpecification
    HyperParameterAlgorithmSpecification (..),
    mkHyperParameterAlgorithmSpecification,
    hpasTrainingInputMode,
    hpasAlgorithmName,
    hpasMetricDefinitions,
    hpasTrainingImage,

    -- ** DeployedImage
    DeployedImage (..),
    mkDeployedImage,
    diResolutionTime,
    diResolvedImage,
    diSpecifiedImage,

    -- ** HyperParameterTrainingJobDefinition
    HyperParameterTrainingJobDefinition (..),
    mkHyperParameterTrainingJobDefinition,
    hptjdAlgorithmSpecification,
    hptjdRoleArn,
    hptjdOutputDataConfig,
    hptjdResourceConfig,
    hptjdStoppingCondition,
    hptjdCheckpointConfig,
    hptjdDefinitionName,
    hptjdEnableInterContainerTrafficEncryption,
    hptjdEnableManagedSpotTraining,
    hptjdEnableNetworkIsolation,
    hptjdHyperParameterRanges,
    hptjdInputDataConfig,
    hptjdStaticHyperParameters,
    hptjdTuningObjective,
    hptjdVpcConfig,

    -- ** UserProfileName
    UserProfileName (..),

    -- ** TrialComponent
    TrialComponent (..),
    mkTrialComponent,
    tcCreatedBy,
    tcCreationTime,
    tcDisplayName,
    tcEndTime,
    tcInputArtifacts,
    tcLastModifiedBy,
    tcLastModifiedTime,
    tcMetrics,
    tcOutputArtifacts,
    tcParameters,
    tcParents,
    tcSource,
    tcSourceDetail,
    tcStartTime,
    tcStatus,
    tcTags,
    tcTrialComponentArn,
    tcTrialComponentName,

    -- ** TrainingInputMode
    TrainingInputMode (..),

    -- ** ImageConfig
    ImageConfig (..),
    mkImageConfig,
    icRepositoryAccessMode,

    -- ** ModelPackageValidationProfile
    ModelPackageValidationProfile (..),
    mkModelPackageValidationProfile,
    mpvpProfileName,
    mpvpTransformJobDefinition,

    -- ** NotebookInstanceLifecycleConfigContent
    NotebookInstanceLifecycleConfigContent (..),

    -- ** EndpointConfigSummary
    EndpointConfigSummary (..),
    mkEndpointConfigSummary,
    ecsEndpointConfigName,
    ecsEndpointConfigArn,
    ecsCreationTime,

    -- ** AutoMLJobStatus
    AutoMLJobStatus (..),

    -- ** HumanLoopActivationConditionsConfig
    HumanLoopActivationConditionsConfig (..),
    mkHumanLoopActivationConditionsConfig,
    hlaccHumanLoopActivationConditions,

    -- ** String
    String (..),

    -- ** TransformResources
    TransformResources (..),
    mkTransformResources,
    trInstanceType,
    trInstanceCount,
    trVolumeKmsKeyId,

    -- ** LabelingJobSummary
    LabelingJobSummary (..),
    mkLabelingJobSummary,
    ljsLabelingJobName,
    ljsLabelingJobArn,
    ljsCreationTime,
    ljsLastModifiedTime,
    ljsLabelingJobStatus,
    ljsLabelCounters,
    ljsWorkteamArn,
    ljsPreHumanTaskLambdaArn,
    ljsAnnotationConsolidationLambdaArn,
    ljsFailureReason,
    ljsInputConfig,
    ljsLabelingJobOutput,

    -- ** TrialComponentPrimaryStatus
    TrialComponentPrimaryStatus (..),

    -- ** ProcessingEnvironmentValue
    ProcessingEnvironmentValue (..),

    -- ** SourceType
    SourceType (..),

    -- ** TrainingSpecification
    TrainingSpecification (..),
    mkTrainingSpecification,
    tsTrainingImage,
    tsSupportedTrainingInstanceTypes,
    tsTrainingChannels,
    tsMetricDefinitions,
    tsSupportedHyperParameters,
    tsSupportedTuningJobObjectiveMetrics,
    tsSupportsDistributedTraining,
    tsTrainingImageDigest,

    -- ** AppStatus
    AppStatus (..),

    -- ** ImageBaseImage
    ImageBaseImage (..),

    -- ** RepositoryAccessMode
    RepositoryAccessMode (..),

    -- ** JsonPath
    JsonPath (..),

    -- ** NotebookOutputOption
    NotebookOutputOption (..),

    -- ** PublicWorkforceTaskPrice
    PublicWorkforceTaskPrice (..),
    mkPublicWorkforceTaskPrice,
    pwtpAmountInUsd,

    -- ** ProcessingS3Input
    ProcessingS3Input (..),
    mkProcessingS3Input,
    psiS3Uri,
    psiLocalPath,
    psiS3DataType,
    psiS3InputMode,
    psiS3CompressionType,
    psiS3DataDistributionType,

    -- ** CategoricalParameterRangeSpecification
    CategoricalParameterRangeSpecification (..),
    mkCategoricalParameterRangeSpecification,
    cprsValues,

    -- ** DomainArn
    DomainArn (..),

    -- ** ImageVersion
    ImageVersion (..),
    mkImageVersion,
    ivCreationTime,
    ivImageArn,
    ivImageVersionArn,
    ivImageVersionStatus,
    ivLastModifiedTime,
    ivVersion,
    ivFailureReason,

    -- ** S3DataSource
    S3DataSource (..),
    mkS3DataSource,
    sdsS3DataType,
    sdsS3Uri,
    sdsAttributeNames,
    sdsS3DataDistributionType,

    -- ** NotebookInstanceLifecycleHook
    NotebookInstanceLifecycleHook (..),
    mkNotebookInstanceLifecycleHook,
    nilhContent,

    -- ** IntegerParameterRangeSpecification
    IntegerParameterRangeSpecification (..),
    mkIntegerParameterRangeSpecification,
    iprsMinValue,
    iprsMaxValue,

    -- ** TransformInstanceType
    TransformInstanceType (..),

    -- ** TrialComponentMetricSummary
    TrialComponentMetricSummary (..),
    mkTrialComponentMetricSummary,
    tcmsAvg,
    tcmsCount,
    tcmsLast,
    tcmsMax,
    tcmsMetricName,
    tcmsMin,
    tcmsSourceArn,
    tcmsStdDev,
    tcmsTimeStamp,

    -- ** MonitoringBaselineConfig
    MonitoringBaselineConfig (..),
    mkMonitoringBaselineConfig,
    mbcConstraintsResource,
    mbcStatisticsResource,

    -- ** ExperimentEntityName
    ExperimentEntityName (..),

    -- ** ModelName
    ModelName (..),

    -- ** UserProfileDetails
    UserProfileDetails (..),
    mkUserProfileDetails,
    updCreationTime,
    updDomainId,
    updLastModifiedTime,
    updStatus,
    updUserProfileName,

    -- ** HyperParameterTuningJobName
    HyperParameterTuningJobName (..),

    -- ** VpcId
    VpcId (..),

    -- ** HyperParameterTuningJobObjective
    HyperParameterTuningJobObjective (..),
    mkHyperParameterTuningJobObjective,
    hptjoType,
    hptjoMetricName,

    -- ** NotebookInstanceLifecycleConfigNameContains
    NotebookInstanceLifecycleConfigNameContains (..),

    -- ** NotebookInstanceLifecycleConfigSummary
    NotebookInstanceLifecycleConfigSummary (..),
    mkNotebookInstanceLifecycleConfigSummary,
    nilcsNotebookInstanceLifecycleConfigName,
    nilcsNotebookInstanceLifecycleConfigArn,
    nilcsCreationTime,
    nilcsLastModifiedTime,

    -- ** ModelPackageArn
    ModelPackageArn (..),

    -- ** CaptureStatus
    CaptureStatus (..),

    -- ** EfsUid
    EfsUid (..),

    -- ** S3DataType
    S3DataType (..),

    -- ** VariantProperty
    VariantProperty (..),
    mkVariantProperty,
    vpVariantPropertyType,

    -- ** HumanTaskConfig
    HumanTaskConfig (..),
    mkHumanTaskConfig,
    htcWorkteamArn,
    htcUiConfig,
    htcPreHumanTaskLambdaArn,
    htcTaskTitle,
    htcTaskDescription,
    htcNumberOfHumanWorkersPerDataObject,
    htcTaskTimeLimitInSeconds,
    htcAnnotationConsolidationConfig,
    htcMaxConcurrentTaskCount,
    htcPublicWorkforceTaskPrice,
    htcTaskAvailabilityLifetimeInSeconds,
    htcTaskKeywords,

    -- ** LambdaFunctionArn
    LambdaFunctionArn (..),

    -- ** Accept
    Accept (..),

    -- ** TrialComponentArtifactValue
    TrialComponentArtifactValue (..),

    -- ** Operator
    Operator (..),

    -- ** ContainerImage
    ContainerImage (..),

    -- ** ProcessingStoppingCondition
    ProcessingStoppingCondition (..),
    mkProcessingStoppingCondition,
    pscMaxRuntimeInSeconds,

    -- ** SubscribedWorkteam
    SubscribedWorkteam (..),
    mkSubscribedWorkteam,
    swWorkteamArn,
    swListingId,
    swMarketplaceDescription,
    swMarketplaceTitle,
    swSellerName,

    -- ** AppInstanceType
    AppInstanceType (..),

    -- ** SortTrialsBy
    SortTrialsBy (..),

    -- ** CandidateStepName
    CandidateStepName (..),

    -- ** AlgorithmImage
    AlgorithmImage (..),

    -- ** DebugHookConfig
    DebugHookConfig (..),
    mkDebugHookConfig,
    dhcS3OutputPath,
    dhcCollectionConfigurations,
    dhcHookParameters,
    dhcLocalPath,

    -- ** CheckpointConfig
    CheckpointConfig (..),
    mkCheckpointConfig,
    ccS3Uri,
    ccLocalPath,

    -- ** ModelSummary
    ModelSummary (..),
    mkModelSummary,
    msModelName,
    msModelArn,
    msCreationTime,

    -- ** CodeRepositorySummary
    CodeRepositorySummary (..),
    mkCodeRepositorySummary,
    crsCodeRepositoryName,
    crsCodeRepositoryArn,
    crsCreationTime,
    crsLastModifiedTime,
    crsGitConfig,

    -- ** MetricName
    MetricName (..),

    -- ** ProcessingJobName
    ProcessingJobName (..),

    -- ** HyperParameterTuningJobSummary
    HyperParameterTuningJobSummary (..),
    mkHyperParameterTuningJobSummary,
    hHyperParameterTuningJobName,
    hHyperParameterTuningJobArn,
    hHyperParameterTuningJobStatus,
    hStrategy,
    hCreationTime,
    hTrainingJobStatusCounters,
    hObjectiveStatusCounters,
    hHyperParameterTuningEndTime,
    hLastModifiedTime,
    hResourceLimits,

    -- ** ImageStatus
    ImageStatus (..),

    -- ** ProcessingClusterConfig
    ProcessingClusterConfig (..),
    mkProcessingClusterConfig,
    pccInstanceCount,
    pccInstanceType,
    pccVolumeSizeInGB,
    pccVolumeKmsKeyId,

    -- ** ProcessingLocalPath
    ProcessingLocalPath (..),

    -- ** MonitoringOutput
    MonitoringOutput (..),
    mkMonitoringOutput,
    moS3Output,

    -- ** ImageNameContains
    ImageNameContains (..),

    -- ** AppDetails
    AppDetails (..),
    mkAppDetails,
    adAppName,
    adAppType,
    adCreationTime,
    adDomainId,
    adStatus,
    adUserProfileName,

    -- ** StoppingCondition
    StoppingCondition (..),
    mkStoppingCondition,
    scMaxRuntimeInSeconds,
    scMaxWaitTimeInSeconds,

    -- ** AlgorithmSummary
    AlgorithmSummary (..),
    mkAlgorithmSummary,
    aAlgorithmName,
    aAlgorithmArn,
    aCreationTime,
    aAlgorithmStatus,
    aAlgorithmDescription,

    -- ** ImageSortOrder
    ImageSortOrder (..),

    -- ** MonitoringExecutionSortKey
    MonitoringExecutionSortKey (..),

    -- ** ProblemType
    ProblemType (..),

    -- ** AutoMLOutputDataConfig
    AutoMLOutputDataConfig (..),
    mkAutoMLOutputDataConfig,
    amlodcS3OutputPath,
    amlodcKmsKeyId,

    -- ** Url
    Url (..),

    -- ** AutoMLJobConfig
    AutoMLJobConfig (..),
    mkAutoMLJobConfig,
    amljcCompletionCriteria,
    amljcSecurityConfig,

    -- ** AppImageConfigName
    AppImageConfigName (..),

    -- ** AuthMode
    AuthMode (..),

    -- ** AutoMLDataSource
    AutoMLDataSource (..),
    mkAutoMLDataSource,
    amldsS3DataSource,

    -- ** Channel
    Channel (..),
    mkChannel,
    cChannelName,
    cDataSource,
    cCompressionType,
    cContentType,
    cInputMode,
    cRecordWrapperType,
    cShuffleConfig,

    -- ** ObjectiveStatus
    ObjectiveStatus (..),

    -- ** FlowDefinitionTaskKeyword
    FlowDefinitionTaskKeyword (..),

    -- ** TrainingJobStatus
    TrainingJobStatus (..),

    -- ** TaskDescription
    TaskDescription (..),

    -- ** FlowDefinitionStatus
    FlowDefinitionStatus (..),

    -- ** ExperimentConfig
    ExperimentConfig (..),
    mkExperimentConfig,
    ecExperimentName,
    ecTrialComponentDisplayName,
    ecTrialName,

    -- ** AppName
    AppName (..),

    -- ** NetworkInterfaceId
    NetworkInterfaceId (..),

    -- ** UiConfig
    UiConfig (..),
    mkUiConfig,
    ucHumanTaskUiArn,
    ucUiTemplateS3Uri,

    -- ** NestedFilters
    NestedFilters (..),
    mkNestedFilters,
    nfNestedPropertyName,
    nfFilters,

    -- ** ImageSortBy
    ImageSortBy (..),

    -- ** SuggestionQuery
    SuggestionQuery (..),
    mkSuggestionQuery,
    sqPropertyNameQuery,

    -- ** LabelingJobAlgorithmSpecificationArn
    LabelingJobAlgorithmSpecificationArn (..),

    -- ** AutoMLJobName
    AutoMLJobName (..),

    -- ** UserContext
    UserContext (..),
    mkUserContext,
    ucDomainId,
    ucUserProfileArn,
    ucUserProfileName,

    -- ** AssemblyType
    AssemblyType (..),

    -- ** AutoMLJobCompletionCriteria
    AutoMLJobCompletionCriteria (..),
    mkAutoMLJobCompletionCriteria,
    amljccMaxAutoMLJobRuntimeInSeconds,
    amljccMaxCandidates,
    amljccMaxRuntimePerTrainingJobInSeconds,

    -- ** ExecutionStatus
    ExecutionStatus (..),

    -- ** ParameterKey
    ParameterKey (..),

    -- ** FileSystemId
    FileSystemId (..),

    -- ** InferenceSpecification
    InferenceSpecification (..),
    mkInferenceSpecification,
    isContainers,
    isSupportedTransformInstanceTypes,
    isSupportedRealtimeInferenceInstanceTypes,
    isSupportedContentTypes,
    isSupportedResponseMIMETypes,

    -- ** TargetPlatform
    TargetPlatform (..),
    mkTargetPlatform,
    tpOs,
    tpArch,
    tpAccelerator,

    -- ** FileSystemDataSource
    FileSystemDataSource (..),
    mkFileSystemDataSource,
    fsdsFileSystemId,
    fsdsFileSystemAccessMode,
    fsdsFileSystemType,
    fsdsDirectoryPath,

    -- ** SourceIpConfig
    SourceIpConfig (..),
    mkSourceIpConfig,
    sicCidrs,

    -- ** PresignedDomainUrl
    PresignedDomainUrl (..),

    -- ** ParameterRange
    ParameterRange (..),
    mkParameterRange,
    prCategoricalParameterRangeSpecification,
    prContinuousParameterRangeSpecification,
    prIntegerParameterRangeSpecification,

    -- ** HyperParameterTuningJobSortByOptions
    HyperParameterTuningJobSortByOptions (..),

    -- ** ListCompilationJobsSortBy
    ListCompilationJobsSortBy (..),

    -- ** TransformJobDefinition
    TransformJobDefinition (..),
    mkTransformJobDefinition,
    tjdTransformInput,
    tjdTransformOutput,
    tjdTransformResources,
    tjdBatchStrategy,
    tjdEnvironment,
    tjdMaxConcurrentTransforms,
    tjdMaxPayloadInMB,

    -- ** ContentClassifier
    ContentClassifier (..),

    -- ** EnvironmentValue
    EnvironmentValue (..),

    -- ** KernelSpec
    KernelSpec (..),
    mkKernelSpec,
    ksName,
    ksDisplayName,

    -- ** AutoMLJobObjective
    AutoMLJobObjective (..),
    mkAutoMLJobObjective,
    amljoMetricName,

    -- ** UserProfileStatus
    UserProfileStatus (..),

    -- ** AutoMLS3DataType
    AutoMLS3DataType (..),

    -- ** SubnetId
    SubnetId (..),

    -- ** ContinuousParameterRangeSpecification
    ContinuousParameterRangeSpecification (..),
    mkContinuousParameterRangeSpecification,
    cprsMinValue,
    cprsMaxValue,

    -- ** StatusDetails
    StatusDetails (..),

    -- ** WorkforceArn
    WorkforceArn (..),

    -- ** SecondaryStatusTransition
    SecondaryStatusTransition (..),
    mkSecondaryStatusTransition,
    sstStatus,
    sstStartTime,
    sstEndTime,
    sstStatusMessage,

    -- ** CollectionName
    CollectionName (..),

    -- ** WorkteamArn
    WorkteamArn (..),

    -- ** AutoMLCandidateStep
    AutoMLCandidateStep (..),
    mkAutoMLCandidateStep,
    amlcsCandidateStepType,
    amlcsCandidateStepArn,
    amlcsCandidateStepName,

    -- ** HumanLoopConfig
    HumanLoopConfig (..),
    mkHumanLoopConfig,
    hlcWorkteamArn,
    hlcHumanTaskUiArn,
    hlcTaskTitle,
    hlcTaskDescription,
    hlcTaskCount,
    hlcPublicWorkforceTaskPrice,
    hlcTaskAvailabilityLifetimeInSeconds,
    hlcTaskKeywords,
    hlcTaskTimeLimitInSeconds,

    -- ** TrialComponentStatus
    TrialComponentStatus (..),
    mkTrialComponentStatus,
    tcsMessage,
    tcsPrimaryStatus,

    -- ** LabelCounters
    LabelCounters (..),
    mkLabelCounters,
    lcFailedNonRetryableError,
    lcHumanLabeled,
    lcMachineLabeled,
    lcTotalLabeled,
    lcUnlabeled,

    -- ** ScheduleStatus
    ScheduleStatus (..),

    -- ** ParameterType
    ParameterType (..),

    -- ** ImageVersionStatus
    ImageVersionStatus (..),

    -- ** LabelingJobResourceConfig
    LabelingJobResourceConfig (..),
    mkLabelingJobResourceConfig,
    ljrcVolumeKmsKeyId,

    -- ** MonitoringOutputConfig
    MonitoringOutputConfig (..),
    mkMonitoringOutputConfig,
    mocMonitoringOutputs,
    mocKmsKeyId,

    -- ** InstanceType
    InstanceType (..),

    -- ** ImageVersionSortOrder
    ImageVersionSortOrder (..),

    -- ** NotebookInstanceLifecycleConfigSortKey
    NotebookInstanceLifecycleConfigSortKey (..),

    -- ** TransformS3DataSource
    TransformS3DataSource (..),
    mkTransformS3DataSource,
    tsdsS3DataType,
    tsdsS3Uri,

    -- ** ModelPackageSummary
    ModelPackageSummary (..),
    mkModelPackageSummary,
    mpsModelPackageName,
    mpsModelPackageArn,
    mpsCreationTime,
    mpsModelPackageStatus,
    mpsModelPackageDescription,

    -- ** ConfigValue
    ConfigValue (..),

    -- ** HumanTaskUiName
    HumanTaskUiName (..),

    -- ** NotebookInstanceLifecycleConfigArn
    NotebookInstanceLifecycleConfigArn (..),

    -- ** CandidateStatus
    CandidateStatus (..),

    -- ** ImageUri
    ImageUri (..),

    -- ** TrialComponentSourceDetail
    TrialComponentSourceDetail (..),
    mkTrialComponentSourceDetail,
    tcsdProcessingJob,
    tcsdSourceArn,
    tcsdTrainingJob,
    tcsdTransformJob,

    -- ** Experiment
    Experiment (..),
    mkExperiment,
    eCreatedBy,
    eCreationTime,
    eDescription,
    eDisplayName,
    eExperimentArn,
    eExperimentName,
    eLastModifiedBy,
    eLastModifiedTime,
    eSource,
    eTags,

    -- ** TargetPlatformAccelerator
    TargetPlatformAccelerator (..),

    -- ** FileSystemConfig
    FileSystemConfig (..),
    mkFileSystemConfig,
    fscDefaultGid,
    fscDefaultUid,
    fscMountPath,

    -- ** InputConfig
    InputConfig (..),
    mkInputConfig,
    icS3Uri,
    icDataInputConfig,
    icFramework,

    -- ** SecurityGroupId
    SecurityGroupId (..),

    -- ** NetworkConfig
    NetworkConfig (..),
    mkNetworkConfig,
    ncEnableInterContainerTrafficEncryption,
    ncEnableNetworkIsolation,
    ncVpcConfig,

    -- ** AccountId
    AccountId (..),

    -- ** ProductionVariant
    ProductionVariant (..),
    mkProductionVariant,
    pvVariantName,
    pvModelName,
    pvInitialInstanceCount,
    pvInstanceType,
    pvAcceleratorType,
    pvInitialVariantWeight,

    -- ** NotebookInstanceStatus
    NotebookInstanceStatus (..),

    -- ** PropertyNameSuggestion
    PropertyNameSuggestion (..),
    mkPropertyNameSuggestion,
    pnsPropertyName,

    -- ** Branch
    Branch (..),

    -- ** ProcessingS3Output
    ProcessingS3Output (..),
    mkProcessingS3Output,
    psoS3Uri,
    psoLocalPath,
    psoS3UploadMode,

    -- ** ExperimentSource
    ExperimentSource (..),
    mkExperimentSource,
    esSourceArn,
    esSourceType,

    -- ** ListWorkforcesSortByOptions
    ListWorkforcesSortByOptions (..),

    -- ** NextToken
    NextToken (..),

    -- ** RetentionPolicy
    RetentionPolicy (..),
    mkRetentionPolicy,
    rpHomeEfsFileSystem,

    -- ** VersionedArnOrName
    VersionedArnOrName (..),

    -- ** RetentionType
    RetentionType (..),

    -- ** BooleanOperator
    BooleanOperator (..),

    -- ** AppSortKey
    AppSortKey (..),

    -- ** CandidateSortBy
    CandidateSortBy (..),

    -- ** ChannelName
    ChannelName (..),

    -- ** DirectoryPath
    DirectoryPath (..),

    -- ** Cidr
    Cidr (..),

    -- ** SearchExpression
    SearchExpression (..),
    mkSearchExpression,
    seFilters,
    seNestedFilters,
    seOperator,
    seSubExpressions,

    -- ** JupyterServerAppSettings
    JupyterServerAppSettings (..),
    mkJupyterServerAppSettings,
    jsasDefaultResourceSpec,

    -- ** ContainerArgument
    ContainerArgument (..),

    -- ** LabelingJobS3DataSource
    LabelingJobS3DataSource (..),
    mkLabelingJobS3DataSource,
    ljsdsManifestS3Uri,

    -- ** RuleEvaluationStatus
    RuleEvaluationStatus (..),

    -- ** ExperimentArn
    ExperimentArn (..),

    -- ** ListWorkteamsSortByOptions
    ListWorkteamsSortByOptions (..),

    -- ** CustomImage
    CustomImage (..),
    mkCustomImage,
    ciImageName,
    ciAppImageConfigName,
    ciImageVersionNumber,

    -- ** ProcessingJob
    ProcessingJob (..),
    mkProcessingJob,
    pjAppSpecification,
    pjAutoMLJobArn,
    pjCreationTime,
    pjEnvironment,
    pjExitMessage,
    pjExperimentConfig,
    pjFailureReason,
    pjLastModifiedTime,
    pjMonitoringScheduleArn,
    pjNetworkConfig,
    pjProcessingEndTime,
    pjProcessingInputs,
    pjProcessingJobArn,
    pjProcessingJobName,
    pjProcessingJobStatus,
    pjProcessingOutputConfig,
    pjProcessingResources,
    pjProcessingStartTime,
    pjRoleArn,
    pjStoppingCondition,
    pjTags,
    pjTrainingJobArn,

    -- ** DataInputConfig
    DataInputConfig (..),

    -- ** AutoMLJobArtifacts
    AutoMLJobArtifacts (..),
    mkAutoMLJobArtifacts,
    amljaCandidateDefinitionNotebookLocation,
    amljaDataExplorationNotebookLocation,

    -- ** HyperParameterTuningJobWarmStartConfig
    HyperParameterTuningJobWarmStartConfig (..),
    mkHyperParameterTuningJobWarmStartConfig,
    hptjwscParentHyperParameterTuningJobs,
    hptjwscWarmStartType,

    -- ** AppArn
    AppArn (..),

    -- ** NotebookInstanceSummary
    NotebookInstanceSummary (..),
    mkNotebookInstanceSummary,
    nisNotebookInstanceName,
    nisNotebookInstanceArn,
    nisAdditionalCodeRepositories,
    nisCreationTime,
    nisDefaultCodeRepository,
    nisInstanceType,
    nisLastModifiedTime,
    nisNotebookInstanceLifecycleConfigName,
    nisNotebookInstanceStatus,
    nisUrl,

    -- ** MetricData
    MetricData (..),
    mkMetricData,
    mdMetricName,
    mdTimestamp,
    mdValue,

    -- ** AutoMLJobArn
    AutoMLJobArn (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** TaskKeyword
    TaskKeyword (..),

    -- ** TrainingJobEarlyStoppingType
    TrainingJobEarlyStoppingType (..),

    -- ** ContainerMode
    ContainerMode (..),

    -- ** ImageVersionSortBy
    ImageVersionSortBy (..),

    -- ** MonitoringClusterConfig
    MonitoringClusterConfig (..),
    mkMonitoringClusterConfig,
    mccInstanceCount,
    mccInstanceType,
    mccVolumeSizeInGB,
    mccVolumeKmsKeyId,

    -- ** VpcConfig
    VpcConfig (..),
    mkVpcConfig,
    vcSecurityGroupIds,
    vcSubnets,

    -- ** ImageDigest
    ImageDigest (..),

    -- ** AutoMLJobSecondaryStatus
    AutoMLJobSecondaryStatus (..),

    -- ** TrialSource
    TrialSource (..),
    mkTrialSource,
    tsSourceArn,
    tsSourceType,

    -- ** NotebookInstanceAcceleratorType
    NotebookInstanceAcceleratorType (..),

    -- ** KmsKeyId
    KmsKeyId (..),

    -- ** ResourceArn
    ResourceArn (..),

    -- ** ResponseMIMEType
    ResponseMIMEType (..),

    -- ** DomainName
    DomainName (..),

    -- ** TransformJobStatus
    TransformJobStatus (..),

    -- ** ModelPackageContainerDefinition
    ModelPackageContainerDefinition (..),
    mkModelPackageContainerDefinition,
    mpcdImage,
    mpcdContainerHostname,
    mpcdImageDigest,
    mpcdModelDataUrl,
    mpcdProductId,

    -- ** DebugRuleConfiguration
    DebugRuleConfiguration (..),
    mkDebugRuleConfiguration,
    drcRuleConfigurationName,
    drcRuleEvaluatorImage,
    drcInstanceType,
    drcLocalPath,
    drcRuleParameters,
    drcS3OutputPath,
    drcVolumeSizeInGB,

    -- ** StatusMessage
    StatusMessage (..),

    -- ** ScheduleExpression
    ScheduleExpression (..),

    -- ** DesiredWeightAndCapacity
    DesiredWeightAndCapacity (..),
    mkDesiredWeightAndCapacity,
    dwacVariantName,
    dwacDesiredInstanceCount,
    dwacDesiredWeight,

    -- ** HumanLoopRequestSource
    HumanLoopRequestSource (..),
    mkHumanLoopRequestSource,
    hlrsAwsManagedHumanLoopRequestSource,

    -- ** AnnotationConsolidationConfig
    AnnotationConsolidationConfig (..),
    mkAnnotationConsolidationConfig,
    accAnnotationConsolidationLambdaArn,

    -- ** SortTrialComponentsBy
    SortTrialComponentsBy (..),

    -- ** EndpointStatus
    EndpointStatus (..),

    -- ** UserProfileSortKey
    UserProfileSortKey (..),

    -- ** JoinSource
    JoinSource (..),

    -- ** CaptureOption
    CaptureOption (..),
    mkCaptureOption,
    coCaptureMode,

    -- ** ResolvedAttributes
    ResolvedAttributes (..),
    mkResolvedAttributes,
    raAutoMLJobObjective,
    raCompletionCriteria,
    raProblemType,

    -- ** MonitoringStoppingCondition
    MonitoringStoppingCondition (..),
    mkMonitoringStoppingCondition,
    mscMaxRuntimeInSeconds,

    -- ** UserProfileArn
    UserProfileArn (..),

    -- ** MonitoringS3Uri
    MonitoringS3Uri (..),

    -- ** SourceAlgorithm
    SourceAlgorithm (..),
    mkSourceAlgorithm,
    saAlgorithmName,
    saModelDataUrl,

    -- ** SearchSortOrder
    SearchSortOrder (..),

    -- ** CompilationJobStatus
    CompilationJobStatus (..),

    -- ** FlowDefinitionArn
    FlowDefinitionArn (..),

    -- ** DomainDetails
    DomainDetails (..),
    mkDomainDetails,
    ddCreationTime,
    ddDomainArn,
    ddDomainId,
    ddDomainName,
    ddLastModifiedTime,
    ddStatus,
    ddUrl,

    -- ** TransformInput
    TransformInput (..),
    mkTransformInput,
    tiDataSource,
    tiCompressionType,
    tiContentType,
    tiSplitType,

    -- ** RootAccess
    RootAccess (..),

    -- ** HumanTaskUiSummary
    HumanTaskUiSummary (..),
    mkHumanTaskUiSummary,
    htusHumanTaskUiName,
    htusHumanTaskUiArn,
    htusCreationTime,

    -- ** TrainingJobArn
    TrainingJobArn (..),

    -- ** AlgorithmSpecification
    AlgorithmSpecification (..),
    mkAlgorithmSpecification,
    asTrainingInputMode,
    asAlgorithmName,
    asEnableSageMakerMetricsTimeSeries,
    asMetricDefinitions,
    asTrainingImage,

    -- ** AutoMLNameContains
    AutoMLNameContains (..),

    -- ** TargetAttributeName
    TargetAttributeName (..),

    -- ** GitConfigForUpdate
    GitConfigForUpdate (..),
    mkGitConfigForUpdate,
    gcfuSecretArn,

    -- ** LabelingJobForWorkteamSummary
    LabelingJobForWorkteamSummary (..),
    mkLabelingJobForWorkteamSummary,
    ljfwsJobReferenceCode,
    ljfwsWorkRequesterAccountId,
    ljfwsCreationTime,
    ljfwsLabelCounters,
    ljfwsLabelingJobName,
    ljfwsNumberOfHumanWorkersPerDataObject,

    -- ** FlowDefinitionOutputConfig
    FlowDefinitionOutputConfig (..),
    mkFlowDefinitionOutputConfig,
    fdocS3OutputPath,
    fdocKmsKeyId,

    -- ** CandidateName
    CandidateName (..),

    -- ** TransformEnvironmentValue
    TransformEnvironmentValue (..),

    -- ** ContainerDefinition
    ContainerDefinition (..),
    mkContainerDefinition,
    cdContainerHostname,
    cdEnvironment,
    cdImage,
    cdImageConfig,
    cdMode,
    cdModelDataUrl,
    cdModelPackageName,

    -- ** SplitType
    SplitType (..),

    -- ** AutoMLFailureReason
    AutoMLFailureReason (..),

    -- ** HyperParameterSpecification
    HyperParameterSpecification (..),
    mkHyperParameterSpecification,
    hpsName,
    hpsType,
    hpsDefaultValue,
    hpsDescription,
    hpsIsRequired,
    hpsIsTunable,
    hpsRange,

    -- ** RenderingError
    RenderingError (..),
    mkRenderingError,
    reCode,
    reMessage,

    -- ** AutoMLSecurityConfig
    AutoMLSecurityConfig (..),
    mkAutoMLSecurityConfig,
    amlscEnableInterContainerTrafficEncryption,
    amlscVolumeKmsKeyId,
    amlscVpcConfig,

    -- ** DirectInternetAccess
    DirectInternetAccess (..),

    -- ** ProcessingInstanceType
    ProcessingInstanceType (..),

    -- ** HumanTaskUiStatus
    HumanTaskUiStatus (..),

    -- ** CompilationJobSummary
    CompilationJobSummary (..),
    mkCompilationJobSummary,
    cjsCompilationJobName,
    cjsCompilationJobArn,
    cjsCreationTime,
    cjsCompilationJobStatus,
    cjsCompilationEndTime,
    cjsCompilationStartTime,
    cjsCompilationTargetDevice,
    cjsCompilationTargetPlatformAccelerator,
    cjsCompilationTargetPlatformArch,
    cjsCompilationTargetPlatformOs,
    cjsLastModifiedTime,

    -- ** ContainerEntrypointString
    ContainerEntrypointString (..),

    -- ** S3DataDistribution
    S3DataDistribution (..),

    -- ** SingleSignOnUserIdentifier
    SingleSignOnUserIdentifier (..),

    -- ** ImageArn
    ImageArn (..),

    -- ** LabelAttributeName
    LabelAttributeName (..),

    -- ** SharingSettings
    SharingSettings (..),
    mkSharingSettings,
    ssNotebookOutputOption,
    ssS3KmsKeyId,
    ssS3OutputPath,

    -- ** LabelingJobStatus
    LabelingJobStatus (..),

    -- ** MonitoringResources
    MonitoringResources (..),
    mkMonitoringResources,
    mrClusterConfig,

    -- ** RenderableTask
    RenderableTask (..),
    mkRenderableTask,
    rtInput,

    -- ** OutputConfig
    OutputConfig (..),
    mkOutputConfig,
    ocS3OutputLocation,
    ocCompilerOptions,
    ocTargetDevice,
    ocTargetPlatform,

    -- ** ProcessingInput
    ProcessingInput (..),
    mkProcessingInput,
    piInputName,
    piS3Input,

    -- ** DebugRuleEvaluationStatus
    DebugRuleEvaluationStatus (..),
    mkDebugRuleEvaluationStatus,
    dresLastModifiedTime,
    dresRuleConfigurationName,
    dresRuleEvaluationJobArn,
    dresRuleEvaluationStatus,
    dresStatusDetails,

    -- ** DataSource
    DataSource (..),
    mkDataSource,
    dsFileSystemDataSource,
    dsS3DataSource,

    -- ** CognitoConfig
    CognitoConfig (..),
    mkCognitoConfig,
    ccUserPool,
    ccClientId,

    -- ** OutputDataConfig
    OutputDataConfig (..),
    mkOutputDataConfig,
    odcS3OutputPath,
    odcKmsKeyId,

    -- ** NotebookInstanceSortKey
    NotebookInstanceSortKey (..),

    -- ** MemberDefinition
    MemberDefinition (..),
    mkMemberDefinition,
    mdCognitoMemberDefinition,
    mdOidcMemberDefinition,

    -- ** TaskTitle
    TaskTitle (..),

    -- ** ConfigKey
    ConfigKey (..),

    -- ** EndpointConfigNameContains
    EndpointConfigNameContains (..),

    -- ** ExperimentSummary
    ExperimentSummary (..),
    mkExperimentSummary,
    esfCreationTime,
    esfDisplayName,
    esfExperimentArn,
    esfExperimentName,
    esfExperimentSource,
    esfLastModifiedTime,

    -- ** ProductionVariantInstanceType
    ProductionVariantInstanceType (..),

    -- ** LabelingJobInputConfig
    LabelingJobInputConfig (..),
    mkLabelingJobInputConfig,
    ljicDataSource,
    ljicDataAttributes,

    -- ** AutoMLJobSummary
    AutoMLJobSummary (..),
    mkAutoMLJobSummary,
    amljsAutoMLJobName,
    amljsAutoMLJobArn,
    amljsAutoMLJobStatus,
    amljsAutoMLJobSecondaryStatus,
    amljsCreationTime,
    amljsLastModifiedTime,
    amljsEndTime,
    amljsFailureReason,

    -- ** HyperParameterTuningJobConfig
    HyperParameterTuningJobConfig (..),
    mkHyperParameterTuningJobConfig,
    hptjcStrategy,
    hptjcResourceLimits,
    hptjcHyperParameterTuningJobObjective,
    hptjcParameterRanges,
    hptjcTrainingJobEarlyStoppingType,
    hptjcTuningJobCompletionCriteria,

    -- ** EndpointInput
    EndpointInput (..),
    mkEndpointInput,
    eiEndpointName,
    eiLocalPath,
    eiS3DataDistributionType,
    eiS3InputMode,

    -- ** NotebookInstanceArn
    NotebookInstanceArn (..),

    -- ** ProcessingS3CompressionType
    ProcessingS3CompressionType (..),

    -- ** TrialComponentSourceArn
    TrialComponentSourceArn (..),

    -- ** AutoMLChannel
    AutoMLChannel (..),
    mkAutoMLChannel,
    amlcDataSource,
    amlcTargetAttributeName,
    amlcCompressionType,

    -- ** DataExplorationNotebookLocation
    DataExplorationNotebookLocation (..),

    -- ** DomainId
    DomainId (..),

    -- ** HumanLoopActivationConfig
    HumanLoopActivationConfig (..),
    mkHumanLoopActivationConfig,
    hlacHumanLoopActivationConditionsConfig,

    -- ** TagKey
    TagKey (..),

    -- ** ContainerHostname
    ContainerHostname (..),

    -- ** VariantPropertyType
    VariantPropertyType (..),

    -- ** IntegerParameterRange
    IntegerParameterRange (..),
    mkIntegerParameterRange,
    iprName,
    iprMinValue,
    iprMaxValue,
    iprScalingType,

    -- ** HyperParameterTuningJobObjectiveType
    HyperParameterTuningJobObjectiveType (..),

    -- ** TrainingJobName
    TrainingJobName (..),

    -- ** ProcessingJobSummary
    ProcessingJobSummary (..),
    mkProcessingJobSummary,
    pjsProcessingJobName,
    pjsProcessingJobArn,
    pjsCreationTime,
    pjsProcessingJobStatus,
    pjsExitMessage,
    pjsFailureReason,
    pjsLastModifiedTime,
    pjsProcessingEndTime,

    -- ** ProductionVariantSummary
    ProductionVariantSummary (..),
    mkProductionVariantSummary,
    pvsVariantName,
    pvsCurrentInstanceCount,
    pvsCurrentWeight,
    pvsDeployedImages,
    pvsDesiredInstanceCount,
    pvsDesiredWeight,

    -- ** CategoricalParameterRange
    CategoricalParameterRange (..),
    mkCategoricalParameterRange,
    cprName,
    cprValues,

    -- ** MonitoringAppSpecification
    MonitoringAppSpecification (..),
    mkMonitoringAppSpecification,
    masImageUri,
    masContainerArguments,
    masContainerEntrypoint,
    masPostAnalyticsProcessorSourceUri,
    masRecordPreprocessorSourceUri,

    -- ** MountPath
    MountPath (..),

    -- ** TrialComponentKey64
    TrialComponentKey64 (..),

    -- ** NotebookInstanceLifecycleConfigSortOrder
    NotebookInstanceLifecycleConfigSortOrder (..),

    -- ** VariantName
    VariantName (..),

    -- ** ImageVersionArn
    ImageVersionArn (..),

    -- ** ResourceConfig
    ResourceConfig (..),
    mkResourceConfig,
    rcInstanceType,
    rcInstanceCount,
    rcVolumeSizeInGB,
    rcVolumeKmsKeyId,

    -- ** Filter
    Filter (..),
    mkFilter,
    fName,
    fOperator,
    fValue,

    -- ** FlowDefinitionName
    FlowDefinitionName (..),

    -- ** FinalHyperParameterTuningJobObjectiveMetric
    FinalHyperParameterTuningJobObjectiveMetric (..),
    mkFinalHyperParameterTuningJobObjectiveMetric,
    fhptjomMetricName,
    fhptjomValue,
    fhptjomType,

    -- ** TrialComponentArn
    TrialComponentArn (..),

    -- ** GitConfig
    GitConfig (..),
    mkGitConfig,
    gcRepositoryUrl,
    gcBranch,
    gcSecretArn,

    -- ** ImageName
    ImageName (..),

    -- ** HyperParameterKey
    HyperParameterKey (..),

    -- ** MonitoringConstraintsResource
    MonitoringConstraintsResource (..),
    mkMonitoringConstraintsResource,
    mcrS3Uri,

    -- ** ProcessingJobStatus
    ProcessingJobStatus (..),

    -- ** TrialComponentSource
    TrialComponentSource (..),
    mkTrialComponentSource,
    tcsSourceArn,
    tcsSourceType,

    -- ** WorkteamName
    WorkteamName (..),

    -- ** ContinuousParameterRange
    ContinuousParameterRange (..),
    mkContinuousParameterRange,
    cName,
    cMinValue,
    cMaxValue,
    cScalingType,

    -- ** DetailedAlgorithmStatus
    DetailedAlgorithmStatus (..),

    -- ** TemplateUrl
    TemplateUrl (..),

    -- ** MonitoringStatisticsResource
    MonitoringStatisticsResource (..),
    mkMonitoringStatisticsResource,
    msrS3Uri,

    -- ** TrialComponentKey256
    TrialComponentKey256 (..),

    -- ** AlgorithmStatus
    AlgorithmStatus (..),

    -- ** ProductionVariantAcceleratorType
    ProductionVariantAcceleratorType (..),

    -- ** FlowDefinitionSummary
    FlowDefinitionSummary (..),
    mkFlowDefinitionSummary,
    fdsFlowDefinitionName,
    fdsFlowDefinitionArn,
    fdsFlowDefinitionStatus,
    fdsCreationTime,
    fdsFailureReason,

    -- ** PropertyNameHint
    PropertyNameHint (..),

    -- ** TransformOutput
    TransformOutput (..),
    mkTransformOutput,
    toS3OutputPath,
    toAccept,
    toAssembleWith,
    toKmsKeyId,

    -- ** ExitMessage
    ExitMessage (..),

    -- ** ParentHyperParameterTuningJob
    ParentHyperParameterTuningJob (..),
    mkParentHyperParameterTuningJob,
    phptjHyperParameterTuningJobName,

    -- ** AppNetworkAccessType
    AppNetworkAccessType (..),

    -- ** CodeRepositorySortOrder
    CodeRepositorySortOrder (..),

    -- ** OidcConfig
    OidcConfig (..),
    mkOidcConfig,
    ocClientId,
    ocClientSecret,
    ocIssuer,
    ocAuthorizationEndpoint,
    ocTokenEndpoint,
    ocUserInfoEndpoint,
    ocLogoutEndpoint,
    ocJwksUri,

    -- ** CompilationJobArn
    CompilationJobArn (..),

    -- ** SearchRecord
    SearchRecord (..),
    mkSearchRecord,
    srExperiment,
    srTrainingJob,
    srTrial,
    srTrialComponent,

    -- ** HyperParameterTuningJobStatus
    HyperParameterTuningJobStatus (..),

    -- ** ModelPackageStatusDetails
    ModelPackageStatusDetails (..),
    mkModelPackageStatusDetails,
    mpsdValidationStatuses,
    mpsdImageScanStatuses,

    -- ** MonitoringS3Output
    MonitoringS3Output (..),
    mkMonitoringS3Output,
    msoS3Uri,
    msoLocalPath,
    msoS3UploadMode,

    -- ** AttributeName
    AttributeName (..),

    -- ** TrainingJobSummary
    TrainingJobSummary (..),
    mkTrainingJobSummary,
    tjsfTrainingJobName,
    tjsfTrainingJobArn,
    tjsfCreationTime,
    tjsfTrainingJobStatus,
    tjsfLastModifiedTime,
    tjsfTrainingEndTime,

    -- ** TrialComponentArtifact
    TrialComponentArtifact (..),
    mkTrialComponentArtifact,
    tcaValue,
    tcaMediaType,

    -- ** WorkforceName
    WorkforceName (..),

    -- ** OidcConfigForResponse
    OidcConfigForResponse (..),
    mkOidcConfigForResponse,
    ocfrAuthorizationEndpoint,
    ocfrClientId,
    ocfrIssuer,
    ocfrJwksUri,
    ocfrLogoutEndpoint,
    ocfrTokenEndpoint,
    ocfrUserInfoEndpoint,

    -- ** DataProcessing
    DataProcessing (..),
    mkDataProcessing,
    dpInputFilter,
    dpJoinSource,
    dpOutputFilter,

    -- ** ProcessingS3UploadMode
    ProcessingS3UploadMode (..),

    -- ** HumanTaskUiArn
    HumanTaskUiArn (..),

    -- ** AutoMLCandidate
    AutoMLCandidate (..),
    mkAutoMLCandidate,
    amlcCandidateName,
    amlcObjectiveStatus,
    amlcCandidateSteps,
    amlcCandidateStatus,
    amlcCreationTime,
    amlcLastModifiedTime,
    amlcEndTime,
    amlcFailureReason,
    amlcFinalAutoMLJobObjectiveMetric,
    amlcInferenceContainers,

    -- ** SecretArn
    SecretArn (..),

    -- ** SortExperimentsBy
    SortExperimentsBy (..),

    -- ** ProcessingS3DataType
    ProcessingS3DataType (..),

    -- ** TransformJob
    TransformJob (..),
    mkTransformJob,
    tjfAutoMLJobArn,
    tjfBatchStrategy,
    tjfCreationTime,
    tjfDataProcessing,
    tjfEnvironment,
    tjfExperimentConfig,
    tjfFailureReason,
    tjfLabelingJobArn,
    tjfMaxConcurrentTransforms,
    tjfMaxPayloadInMB,
    tjfModelClientConfig,
    tjfModelName,
    tjfTags,
    tjfTransformEndTime,
    tjfTransformInput,
    tjfTransformJobArn,
    tjfTransformJobName,
    tjfTransformJobStatus,
    tjfTransformOutput,
    tjfTransformResources,
    tjfTransformStartTime,

    -- ** NotebookInstanceLifecycleConfigName
    NotebookInstanceLifecycleConfigName (..),

    -- ** MonitoringScheduleConfig
    MonitoringScheduleConfig (..),
    mkMonitoringScheduleConfig,
    mscMonitoringJobDefinition,
    mscScheduleConfig,

    -- ** CompilerOptions
    CompilerOptions (..),

    -- ** FileSystemType
    FileSystemType (..),

    -- ** ModelPackageValidationSpecification
    ModelPackageValidationSpecification (..),
    mkModelPackageValidationSpecification,
    mpvsValidationRole,
    mpvsValidationProfiles,

    -- ** AutoMLS3DataSource
    AutoMLS3DataSource (..),
    mkAutoMLS3DataSource,
    amlsdsS3DataType,
    amlsdsS3Uri,

    -- ** TargetPlatformArch
    TargetPlatformArch (..),

    -- ** LabelingJobName
    LabelingJobName (..),

    -- ** CompressionType
    CompressionType (..),

    -- ** KernelGatewayImageConfig
    KernelGatewayImageConfig (..),
    mkKernelGatewayImageConfig,
    kgicKernelSpecs,
    kgicFileSystemConfig,

    -- ** AlgorithmStatusItem
    AlgorithmStatusItem (..),
    mkAlgorithmStatusItem,
    asiName,
    asiStatus,
    asiFailureReason,

    -- ** ChannelSpecification
    ChannelSpecification (..),
    mkChannelSpecification,
    csName,
    csSupportedContentTypes,
    csSupportedInputModes,
    csDescription,
    csIsRequired,
    csSupportedCompressionTypes,

    -- ** TransformEnvironmentKey
    TransformEnvironmentKey (..),

    -- ** EndpointConfigName
    EndpointConfigName (..),

    -- ** ProcessingOutputConfig
    ProcessingOutputConfig (..),
    mkProcessingOutputConfig,
    pocOutputs,
    pocKmsKeyId,

    -- ** AlgorithmSortBy
    AlgorithmSortBy (..),

    -- ** AppImageConfigDetails
    AppImageConfigDetails (..),
    mkAppImageConfigDetails,
    aicdAppImageConfigArn,
    aicdAppImageConfigName,
    aicdCreationTime,
    aicdKernelGatewayImageConfig,
    aicdLastModifiedTime,

    -- ** S3Uri
    S3Uri (..),

    -- ** AutoMLJobObjectiveType
    AutoMLJobObjectiveType (..),

    -- ** TensorBoardOutputConfig
    TensorBoardOutputConfig (..),
    mkTensorBoardOutputConfig,
    tbocS3OutputPath,
    tbocLocalPath,

    -- ** MonitoringScheduleName
    MonitoringScheduleName (..),

    -- ** TrialSummary
    TrialSummary (..),
    mkTrialSummary,
    tsCreationTime,
    tsDisplayName,
    tsLastModifiedTime,
    tsTrialArn,
    tsTrialName,
    tsTrialSource,

    -- ** CodeRepositoryNameOrUrl
    CodeRepositoryNameOrUrl (..),

    -- ** MonitoringJobDefinition
    MonitoringJobDefinition (..),
    mkMonitoringJobDefinition,
    mjdMonitoringInputs,
    mjdMonitoringOutputConfig,
    mjdMonitoringResources,
    mjdMonitoringAppSpecification,
    mjdRoleArn,
    mjdBaselineConfig,
    mjdEnvironment,
    mjdNetworkConfig,
    mjdStoppingCondition,

    -- ** LabelingJobDataAttributes
    LabelingJobDataAttributes (..),
    mkLabelingJobDataAttributes,
    ljdaContentClassifiers,

    -- ** FinalAutoMLJobObjectiveMetric
    FinalAutoMLJobObjectiveMetric (..),
    mkFinalAutoMLJobObjectiveMetric,
    famljomMetricName,
    famljomValue,
    famljomType,

    -- ** EndpointArn
    EndpointArn (..),

    -- ** ListLabelingJobsForWorkteamSortByOptions
    ListLabelingJobsForWorkteamSortByOptions (..),

    -- ** CodeRepositorySortBy
    CodeRepositorySortBy (..),

    -- ** HyperParameterTuningJobWarmStartType
    HyperParameterTuningJobWarmStartType (..),

    -- ** CollectionConfiguration
    CollectionConfiguration (..),
    mkCollectionConfiguration,
    ccCollectionName,
    ccCollectionParameters,

    -- ** RecordWrapper
    RecordWrapper (..),

    -- ** SecondaryStatus
    SecondaryStatus (..),

    -- ** LabelingJobOutput
    LabelingJobOutput (..),
    mkLabelingJobOutput,
    ljoOutputDatasetS3Uri,
    ljoFinalActiveLearningModelArn,

    -- ** EndpointSortKey
    EndpointSortKey (..),

    -- ** TransformJobArn
    TransformJobArn (..),

    -- ** ProductId
    ProductId (..),

    -- ** LabelCountersForWorkteam
    LabelCountersForWorkteam (..),
    mkLabelCountersForWorkteam,
    lcfwHumanLabeled,
    lcfwPendingHuman,
    lcfwTotal,

    -- ** DataCaptureConfig
    DataCaptureConfig (..),
    mkDataCaptureConfig,
    dccInitialSamplingPercentage,
    dccDestinationS3Uri,
    dccCaptureOptions,
    dccCaptureContentTypeHeader,
    dccEnableCapture,
    dccKmsKeyId,

    -- ** TrainingJobSortByOptions
    TrainingJobSortByOptions (..),

    -- ** ProcessingS3InputMode
    ProcessingS3InputMode (..),

    -- ** AlgorithmValidationSpecification
    AlgorithmValidationSpecification (..),
    mkAlgorithmValidationSpecification,
    avsValidationRole,
    avsValidationProfiles,

    -- ** NotificationTopicArn
    NotificationTopicArn (..),

    -- ** AppImageConfigSortKey
    AppImageConfigSortKey (..),

    -- ** TargetDevice
    TargetDevice (..),

    -- ** TransformDataSource
    TransformDataSource (..),
    mkTransformDataSource,
    tdsS3DataSource,

    -- ** AutoMLMetricEnum
    AutoMLMetricEnum (..),

    -- ** SortBy
    SortBy (..),

    -- ** RuleConfigurationName
    RuleConfigurationName (..),

    -- ** ProcessingS3DataDistributionType
    ProcessingS3DataDistributionType (..),

    -- ** AlgorithmStatusDetails
    AlgorithmStatusDetails (..),
    mkAlgorithmStatusDetails,
    asdImageScanStatuses,
    asdValidationStatuses,

    -- ** MetricRegex
    MetricRegex (..),

    -- ** ContentType
    ContentType (..),

    -- ** ProcessingJobArn
    ProcessingJobArn (..),

    -- ** LabelingJobStoppingConditions
    LabelingJobStoppingConditions (..),
    mkLabelingJobStoppingConditions,
    ljscMaxHumanLabeledObjectCount,
    ljscMaxPercentageOfInputDatasetLabeled,

    -- ** DetailedModelPackageStatus
    DetailedModelPackageStatus (..),

    -- ** ModelPackageStatus
    ModelPackageStatus (..),

    -- ** JobReferenceCode
    JobReferenceCode (..),

    -- ** TrialComponentSummary
    TrialComponentSummary (..),
    mkTrialComponentSummary,
    tcsCreatedBy,
    tcsCreationTime,
    tcsDisplayName,
    tcsEndTime,
    tcsLastModifiedBy,
    tcsLastModifiedTime,
    tcsStartTime,
    tcsStatus,
    tcsTrialComponentArn,
    tcsTrialComponentName,
    tcsTrialComponentSource,

    -- ** MetricDefinition
    MetricDefinition (..),
    mkMetricDefinition,
    mdName,
    mdRegex,

    -- ** ImageDeleteProperty
    ImageDeleteProperty (..),

    -- ** JsonContentType
    JsonContentType (..),

    -- ** TrainingInstanceType
    TrainingInstanceType (..),

    -- ** AppType
    AppType (..),

    -- ** AppImageConfigArn
    AppImageConfigArn (..),

    -- ** RoleArn
    RoleArn (..),

    -- ** ProcessingEnvironmentKey
    ProcessingEnvironmentKey (..),

    -- ** OrderKey
    OrderKey (..),

    -- ** AlgorithmName
    AlgorithmName (..),

    -- ** ExperimentName
    ExperimentName (..),

    -- ** TrialName
    TrialName (..),

    -- ** Content
    Content (..),

    -- ** LabelCategoryConfigS3Uri
    LabelCategoryConfigS3Uri (..),

    -- ** ExecutionRole
    ExecutionRole (..),

    -- ** Name
    Name (..),

    -- ** OutputName
    OutputName (..),

    -- ** S3OutputPath
    S3OutputPath (..),

    -- ** DisplayName
    DisplayName (..),

    -- ** SubDomain
    SubDomain (..),

    -- ** TrainingJobDefinitionName
    TrainingJobDefinitionName (..),

    -- ** TuningJobName
    TuningJobName (..),

    -- ** Description
    Description (..),

    -- ** ModelDataUrl
    ModelDataUrl (..),

    -- ** DefaultCodeRepository
    DefaultCodeRepository (..),

    -- ** LifecycleConfigName
    LifecycleConfigName (..),

    -- ** ModelPackageDescription
    ModelPackageDescription (..),

    -- ** StringValue
    StringValue (..),

    -- ** TuningJobArn
    TuningJobArn (..),

    -- ** InitialActiveLearningModelArn
    InitialActiveLearningModelArn (..),

    -- ** SageMakerImageArn
    SageMakerImageArn (..),

    -- ** SageMakerImageVersionArn
    SageMakerImageVersionArn (..),

    -- ** TrialComponentName
    TrialComponentName (..),

    -- ** AdditionalCodeRepositoryEquals
    AdditionalCodeRepositoryEquals (..),

    -- ** Key
    Key (..),

    -- ** Value
    Value (..),

    -- ** S3ModelArtifacts
    S3ModelArtifacts (..),

    -- ** UserPool
    UserPool (..),

    -- ** UserGroup
    UserGroup (..),

    -- ** DomainIdEquals
    DomainIdEquals (..),

    -- ** UserProfileNameContains
    UserProfileNameContains (..),

    -- ** TrainingImage
    TrainingImage (..),

    -- ** ResolvedImage
    ResolvedImage (..),

    -- ** SpecifiedImage
    SpecifiedImage (..),

    -- ** DefinitionName
    DefinitionName (..),

    -- ** AuthorizedUrl
    AuthorizedUrl (..),

    -- ** ExecutionRoleArn
    ExecutionRoleArn (..),

    -- ** HomeEfsFileSystemKmsKeyId
    HomeEfsFileSystemKmsKeyId (..),

    -- ** VolumeKmsKeyId
    VolumeKmsKeyId (..),

    -- ** PreHumanTaskLambdaArn
    PreHumanTaskLambdaArn (..),

    -- ** AnnotationConsolidationLambdaArn
    AnnotationConsolidationLambdaArn (..),

    -- ** TrainingImageDigest
    TrainingImageDigest (..),

    -- ** LocalPath
    LocalPath (..),

    -- ** SourceArn
    SourceArn (..),

    -- ** UiTemplateS3Uri
    UiTemplateS3Uri (..),

    -- ** ModelPackageName
    ModelPackageName (..),

    -- ** ManifestS3Uri
    ManifestS3Uri (..),

    -- ** CandidateNameEquals
    CandidateNameEquals (..),

    -- ** Input
    Input (..),

    -- ** S3OutputLocation
    S3OutputLocation (..),

    -- ** RuleEvaluationJobArn
    RuleEvaluationJobArn (..),

    -- ** PostAnalyticsProcessorSourceUri
    PostAnalyticsProcessorSourceUri (..),

    -- ** RecordPreprocessorSourceUri
    RecordPreprocessorSourceUri (..),

    -- ** RepositoryUrl
    RepositoryUrl (..),

    -- ** ValidationRole
    ValidationRole (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.AddTags
import Network.AWS.SageMaker.AssociateTrialComponent
import Network.AWS.SageMaker.CreateAlgorithm
import Network.AWS.SageMaker.CreateApp
import Network.AWS.SageMaker.CreateAppImageConfig
import Network.AWS.SageMaker.CreateAutoMLJob
import Network.AWS.SageMaker.CreateCodeRepository
import Network.AWS.SageMaker.CreateCompilationJob
import Network.AWS.SageMaker.CreateDomain
import Network.AWS.SageMaker.CreateEndpoint
import Network.AWS.SageMaker.CreateEndpointConfig
import Network.AWS.SageMaker.CreateExperiment
import Network.AWS.SageMaker.CreateFlowDefinition
import Network.AWS.SageMaker.CreateHumanTaskUi
import Network.AWS.SageMaker.CreateHyperParameterTuningJob
import Network.AWS.SageMaker.CreateImage
import Network.AWS.SageMaker.CreateImageVersion
import Network.AWS.SageMaker.CreateLabelingJob
import Network.AWS.SageMaker.CreateModel
import Network.AWS.SageMaker.CreateModelPackage
import Network.AWS.SageMaker.CreateMonitoringSchedule
import Network.AWS.SageMaker.CreateNotebookInstance
import Network.AWS.SageMaker.CreateNotebookInstanceLifecycleConfig
import Network.AWS.SageMaker.CreatePresignedDomainUrl
import Network.AWS.SageMaker.CreatePresignedNotebookInstanceUrl
import Network.AWS.SageMaker.CreateProcessingJob
import Network.AWS.SageMaker.CreateTrainingJob
import Network.AWS.SageMaker.CreateTransformJob
import Network.AWS.SageMaker.CreateTrial
import Network.AWS.SageMaker.CreateTrialComponent
import Network.AWS.SageMaker.CreateUserProfile
import Network.AWS.SageMaker.CreateWorkforce
import Network.AWS.SageMaker.CreateWorkteam
import Network.AWS.SageMaker.DeleteAlgorithm
import Network.AWS.SageMaker.DeleteApp
import Network.AWS.SageMaker.DeleteAppImageConfig
import Network.AWS.SageMaker.DeleteCodeRepository
import Network.AWS.SageMaker.DeleteDomain
import Network.AWS.SageMaker.DeleteEndpoint
import Network.AWS.SageMaker.DeleteEndpointConfig
import Network.AWS.SageMaker.DeleteExperiment
import Network.AWS.SageMaker.DeleteFlowDefinition
import Network.AWS.SageMaker.DeleteHumanTaskUi
import Network.AWS.SageMaker.DeleteImage
import Network.AWS.SageMaker.DeleteImageVersion
import Network.AWS.SageMaker.DeleteModel
import Network.AWS.SageMaker.DeleteModelPackage
import Network.AWS.SageMaker.DeleteMonitoringSchedule
import Network.AWS.SageMaker.DeleteNotebookInstance
import Network.AWS.SageMaker.DeleteNotebookInstanceLifecycleConfig
import Network.AWS.SageMaker.DeleteTags
import Network.AWS.SageMaker.DeleteTrial
import Network.AWS.SageMaker.DeleteTrialComponent
import Network.AWS.SageMaker.DeleteUserProfile
import Network.AWS.SageMaker.DeleteWorkforce
import Network.AWS.SageMaker.DeleteWorkteam
import Network.AWS.SageMaker.DescribeAlgorithm
import Network.AWS.SageMaker.DescribeApp
import Network.AWS.SageMaker.DescribeAppImageConfig
import Network.AWS.SageMaker.DescribeAutoMLJob
import Network.AWS.SageMaker.DescribeCodeRepository
import Network.AWS.SageMaker.DescribeCompilationJob
import Network.AWS.SageMaker.DescribeDomain
import Network.AWS.SageMaker.DescribeEndpoint
import Network.AWS.SageMaker.DescribeEndpointConfig
import Network.AWS.SageMaker.DescribeExperiment
import Network.AWS.SageMaker.DescribeFlowDefinition
import Network.AWS.SageMaker.DescribeHumanTaskUi
import Network.AWS.SageMaker.DescribeHyperParameterTuningJob
import Network.AWS.SageMaker.DescribeImage
import Network.AWS.SageMaker.DescribeImageVersion
import Network.AWS.SageMaker.DescribeLabelingJob
import Network.AWS.SageMaker.DescribeModel
import Network.AWS.SageMaker.DescribeModelPackage
import Network.AWS.SageMaker.DescribeMonitoringSchedule
import Network.AWS.SageMaker.DescribeNotebookInstance
import Network.AWS.SageMaker.DescribeNotebookInstanceLifecycleConfig
import Network.AWS.SageMaker.DescribeProcessingJob
import Network.AWS.SageMaker.DescribeSubscribedWorkteam
import Network.AWS.SageMaker.DescribeTrainingJob
import Network.AWS.SageMaker.DescribeTransformJob
import Network.AWS.SageMaker.DescribeTrial
import Network.AWS.SageMaker.DescribeTrialComponent
import Network.AWS.SageMaker.DescribeUserProfile
import Network.AWS.SageMaker.DescribeWorkforce
import Network.AWS.SageMaker.DescribeWorkteam
import Network.AWS.SageMaker.DisassociateTrialComponent
import Network.AWS.SageMaker.GetSearchSuggestions
import Network.AWS.SageMaker.ListAlgorithms
import Network.AWS.SageMaker.ListAppImageConfigs
import Network.AWS.SageMaker.ListApps
import Network.AWS.SageMaker.ListAutoMLJobs
import Network.AWS.SageMaker.ListCandidatesForAutoMLJob
import Network.AWS.SageMaker.ListCodeRepositories
import Network.AWS.SageMaker.ListCompilationJobs
import Network.AWS.SageMaker.ListDomains
import Network.AWS.SageMaker.ListEndpointConfigs
import Network.AWS.SageMaker.ListEndpoints
import Network.AWS.SageMaker.ListExperiments
import Network.AWS.SageMaker.ListFlowDefinitions
import Network.AWS.SageMaker.ListHumanTaskUis
import Network.AWS.SageMaker.ListHyperParameterTuningJobs
import Network.AWS.SageMaker.ListImageVersions
import Network.AWS.SageMaker.ListImages
import Network.AWS.SageMaker.ListLabelingJobs
import Network.AWS.SageMaker.ListLabelingJobsForWorkteam
import Network.AWS.SageMaker.ListModelPackages
import Network.AWS.SageMaker.ListModels
import Network.AWS.SageMaker.ListMonitoringExecutions
import Network.AWS.SageMaker.ListMonitoringSchedules
import Network.AWS.SageMaker.ListNotebookInstanceLifecycleConfigs
import Network.AWS.SageMaker.ListNotebookInstances
import Network.AWS.SageMaker.ListProcessingJobs
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
import Network.AWS.SageMaker.RenderUiTemplate
import Network.AWS.SageMaker.Search
import Network.AWS.SageMaker.StartMonitoringSchedule
import Network.AWS.SageMaker.StartNotebookInstance
import Network.AWS.SageMaker.StopAutoMLJob
import Network.AWS.SageMaker.StopCompilationJob
import Network.AWS.SageMaker.StopHyperParameterTuningJob
import Network.AWS.SageMaker.StopLabelingJob
import Network.AWS.SageMaker.StopMonitoringSchedule
import Network.AWS.SageMaker.StopNotebookInstance
import Network.AWS.SageMaker.StopProcessingJob
import Network.AWS.SageMaker.StopTrainingJob
import Network.AWS.SageMaker.StopTransformJob
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.UpdateAppImageConfig
import Network.AWS.SageMaker.UpdateCodeRepository
import Network.AWS.SageMaker.UpdateDomain
import Network.AWS.SageMaker.UpdateEndpoint
import Network.AWS.SageMaker.UpdateEndpointWeightsAndCapacities
import Network.AWS.SageMaker.UpdateExperiment
import Network.AWS.SageMaker.UpdateImage
import Network.AWS.SageMaker.UpdateMonitoringSchedule
import Network.AWS.SageMaker.UpdateNotebookInstance
import Network.AWS.SageMaker.UpdateNotebookInstanceLifecycleConfig
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

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
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
--
-- Other Resources:
--
--     * <https://docs.aws.amazon.com/sagemaker/latest/dg/whatis.html#first-time-user Amazon SageMaker Developer Guide>
--
--     * <https://docs.aws.amazon.com/augmented-ai/2019-11-07/APIReference/Welcome.html Amazon Augmented AI Runtime API Reference>
module Network.AWS.SageMaker
  ( -- * Service Configuration
    sageMaker,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- ** NotebookInstanceDeleted
    notebookInstanceDeleted,

    -- ** EndpointDeleted
    endpointDeleted,

    -- ** EndpointInService
    endpointInService,

    -- ** TransformJobCompletedOrStopped
    transformJobCompletedOrStopped,

    -- ** NotebookInstanceInService
    notebookInstanceInService,

    -- ** ProcessingJobCompletedOrStopped
    processingJobCompletedOrStopped,

    -- ** TrainingJobCompletedOrStopped
    trainingJobCompletedOrStopped,

    -- ** NotebookInstanceStopped
    notebookInstanceStopped,

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

    -- ** CreatePresignedDomainURL
    module Network.AWS.SageMaker.CreatePresignedDomainURL,

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

    -- ** CreatePresignedNotebookInstanceURL
    module Network.AWS.SageMaker.CreatePresignedNotebookInstanceURL,

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

    -- ** AWSManagedHumanLoopRequestSource
    AWSManagedHumanLoopRequestSource (..),

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

    -- ** AssemblyType
    AssemblyType (..),

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

    -- ** ContainerMode
    ContainerMode (..),

    -- ** ContentClassifier
    ContentClassifier (..),

    -- ** DetailedAlgorithmStatus
    DetailedAlgorithmStatus (..),

    -- ** DetailedModelPackageStatus
    DetailedModelPackageStatus (..),

    -- ** DirectInternetAccess
    DirectInternetAccess (..),

    -- ** DomainStatus
    DomainStatus (..),

    -- ** EndpointConfigSortKey
    EndpointConfigSortKey (..),

    -- ** EndpointSortKey
    EndpointSortKey (..),

    -- ** EndpointStatus
    EndpointStatus (..),

    -- ** ExecutionStatus
    ExecutionStatus (..),

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

    -- ** InstanceType
    InstanceType (..),

    -- ** JoinSource
    JoinSource (..),

    -- ** LabelingJobStatus
    LabelingJobStatus (..),

    -- ** ListCompilationJobsSortBy
    ListCompilationJobsSortBy (..),

    -- ** ListLabelingJobsForWorkteamSortByOptions
    ListLabelingJobsForWorkteamSortByOptions (..),

    -- ** ListWorkforcesSortByOptions
    ListWorkforcesSortByOptions (..),

    -- ** ListWorkteamsSortByOptions
    ListWorkteamsSortByOptions (..),

    -- ** ModelPackageSortBy
    ModelPackageSortBy (..),

    -- ** ModelPackageStatus
    ModelPackageStatus (..),

    -- ** ModelSortKey
    ModelSortKey (..),

    -- ** MonitoringExecutionSortKey
    MonitoringExecutionSortKey (..),

    -- ** MonitoringScheduleSortKey
    MonitoringScheduleSortKey (..),

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

    -- ** Operator
    Operator (..),

    -- ** OrderKey
    OrderKey (..),

    -- ** ParameterType
    ParameterType (..),

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

    -- ** RecordWrapper
    RecordWrapper (..),

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

    -- ** ScheduleStatus
    ScheduleStatus (..),

    -- ** SearchSortOrder
    SearchSortOrder (..),

    -- ** SecondaryStatus
    SecondaryStatus (..),

    -- ** SortBy
    SortBy (..),

    -- ** SortExperimentsBy
    SortExperimentsBy (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** SortTrialComponentsBy
    SortTrialComponentsBy (..),

    -- ** SortTrialsBy
    SortTrialsBy (..),

    -- ** SplitType
    SplitType (..),

    -- ** TargetDevice
    TargetDevice (..),

    -- ** TargetPlatformAccelerator
    TargetPlatformAccelerator (..),

    -- ** TargetPlatformArch
    TargetPlatformArch (..),

    -- ** TargetPlatformOS
    TargetPlatformOS (..),

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

    -- ** AlgorithmSpecification
    AlgorithmSpecification,
    algorithmSpecification,
    asEnableSageMakerMetricsTimeSeries,
    asAlgorithmName,
    asTrainingImage,
    asMetricDefinitions,
    asTrainingInputMode,

    -- ** AlgorithmStatusDetails
    AlgorithmStatusDetails,
    algorithmStatusDetails,
    asdImageScanStatuses,
    asdValidationStatuses,

    -- ** AlgorithmStatusItem
    AlgorithmStatusItem,
    algorithmStatusItem,
    asiFailureReason,
    asiName,
    asiStatus,

    -- ** AlgorithmSummary
    AlgorithmSummary,
    algorithmSummary,
    aAlgorithmDescription,
    aAlgorithmName,
    aAlgorithmARN,
    aCreationTime,
    aAlgorithmStatus,

    -- ** AlgorithmValidationProfile
    AlgorithmValidationProfile,
    algorithmValidationProfile,
    avpTransformJobDefinition,
    avpProfileName,
    avpTrainingJobDefinition,

    -- ** AlgorithmValidationSpecification
    AlgorithmValidationSpecification,
    algorithmValidationSpecification,
    avsValidationRole,
    avsValidationProfiles,

    -- ** AnnotationConsolidationConfig
    AnnotationConsolidationConfig,
    annotationConsolidationConfig,
    accAnnotationConsolidationLambdaARN,

    -- ** AppDetails
    AppDetails,
    appDetails,
    adCreationTime,
    adStatus,
    adUserProfileName,
    adAppName,
    adDomainId,
    adAppType,

    -- ** AppImageConfigDetails
    AppImageConfigDetails,
    appImageConfigDetails,
    aicdCreationTime,
    aicdAppImageConfigName,
    aicdLastModifiedTime,
    aicdKernelGatewayImageConfig,
    aicdAppImageConfigARN,

    -- ** AppSpecification
    AppSpecification,
    appSpecification,
    asContainerArguments,
    asContainerEntrypoint,
    asImageURI,

    -- ** AutoMLCandidate
    AutoMLCandidate,
    autoMLCandidate,
    amlcFailureReason,
    amlcInferenceContainers,
    amlcEndTime,
    amlcFinalAutoMLJobObjectiveMetric,
    amlcCandidateName,
    amlcObjectiveStatus,
    amlcCandidateSteps,
    amlcCandidateStatus,
    amlcCreationTime,
    amlcLastModifiedTime,

    -- ** AutoMLCandidateStep
    AutoMLCandidateStep,
    autoMLCandidateStep,
    amlcsCandidateStepType,
    amlcsCandidateStepARN,
    amlcsCandidateStepName,

    -- ** AutoMLChannel
    AutoMLChannel,
    autoMLChannel,
    amlcCompressionType,
    amlcDataSource,
    amlcTargetAttributeName,

    -- ** AutoMLContainerDefinition
    AutoMLContainerDefinition,
    autoMLContainerDefinition,
    amlcdEnvironment,
    amlcdImage,
    amlcdModelDataURL,

    -- ** AutoMLDataSource
    AutoMLDataSource,
    autoMLDataSource,
    amldsS3DataSource,

    -- ** AutoMLJobArtifacts
    AutoMLJobArtifacts,
    autoMLJobArtifacts,
    amljaCandidateDefinitionNotebookLocation,
    amljaDataExplorationNotebookLocation,

    -- ** AutoMLJobCompletionCriteria
    AutoMLJobCompletionCriteria,
    autoMLJobCompletionCriteria,
    amljccMaxCandidates,
    amljccMaxRuntimePerTrainingJobInSeconds,
    amljccMaxAutoMLJobRuntimeInSeconds,

    -- ** AutoMLJobConfig
    AutoMLJobConfig,
    autoMLJobConfig,
    amljcSecurityConfig,
    amljcCompletionCriteria,

    -- ** AutoMLJobObjective
    AutoMLJobObjective,
    autoMLJobObjective,
    amljoMetricName,

    -- ** AutoMLJobSummary
    AutoMLJobSummary,
    autoMLJobSummary,
    amljsFailureReason,
    amljsEndTime,
    amljsAutoMLJobName,
    amljsAutoMLJobARN,
    amljsAutoMLJobStatus,
    amljsAutoMLJobSecondaryStatus,
    amljsCreationTime,
    amljsLastModifiedTime,

    -- ** AutoMLOutputDataConfig
    AutoMLOutputDataConfig,
    autoMLOutputDataConfig,
    amlodcKMSKeyId,
    amlodcS3OutputPath,

    -- ** AutoMLS3DataSource
    AutoMLS3DataSource,
    autoMLS3DataSource,
    amlsdsS3DataType,
    amlsdsS3URI,

    -- ** AutoMLSecurityConfig
    AutoMLSecurityConfig,
    autoMLSecurityConfig,
    amlscVPCConfig,
    amlscVolumeKMSKeyId,
    amlscEnableInterContainerTrafficEncryption,

    -- ** CaptureContentTypeHeader
    CaptureContentTypeHeader,
    captureContentTypeHeader,
    ccthCSVContentTypes,
    ccthJSONContentTypes,

    -- ** CaptureOption
    CaptureOption,
    captureOption,
    coCaptureMode,

    -- ** CategoricalParameterRange
    CategoricalParameterRange,
    categoricalParameterRange,
    cprName,
    cprValues,

    -- ** CategoricalParameterRangeSpecification
    CategoricalParameterRangeSpecification,
    categoricalParameterRangeSpecification,
    cprsValues,

    -- ** Channel
    Channel,
    channel,
    cShuffleConfig,
    cRecordWrapperType,
    cInputMode,
    cCompressionType,
    cContentType,
    cChannelName,
    cDataSource,

    -- ** ChannelSpecification
    ChannelSpecification,
    channelSpecification,
    csSupportedCompressionTypes,
    csIsRequired,
    csDescription,
    csName,
    csSupportedContentTypes,
    csSupportedInputModes,

    -- ** CheckpointConfig
    CheckpointConfig,
    checkpointConfig,
    ccLocalPath,
    ccS3URI,

    -- ** CodeRepositorySummary
    CodeRepositorySummary,
    codeRepositorySummary,
    crsGitConfig,
    crsCodeRepositoryName,
    crsCodeRepositoryARN,
    crsCreationTime,
    crsLastModifiedTime,

    -- ** CognitoConfig
    CognitoConfig,
    cognitoConfig,
    ccUserPool,
    ccClientId,

    -- ** CognitoMemberDefinition
    CognitoMemberDefinition,
    cognitoMemberDefinition,
    cmdUserPool,
    cmdUserGroup,
    cmdClientId,

    -- ** CollectionConfiguration
    CollectionConfiguration,
    collectionConfiguration,
    ccCollectionParameters,
    ccCollectionName,

    -- ** CompilationJobSummary
    CompilationJobSummary,
    compilationJobSummary,
    cjsCompilationStartTime,
    cjsCompilationTargetPlatformAccelerator,
    cjsCompilationTargetDevice,
    cjsLastModifiedTime,
    cjsCompilationTargetPlatformArch,
    cjsCompilationEndTime,
    cjsCompilationTargetPlatformOS,
    cjsCompilationJobName,
    cjsCompilationJobARN,
    cjsCreationTime,
    cjsCompilationJobStatus,

    -- ** ContainerDefinition
    ContainerDefinition,
    containerDefinition,
    cdModelDataURL,
    cdImage,
    cdModelPackageName,
    cdEnvironment,
    cdImageConfig,
    cdMode,
    cdContainerHostname,

    -- ** ContinuousParameterRange
    ContinuousParameterRange,
    continuousParameterRange,
    cScalingType,
    cName,
    cMinValue,
    cMaxValue,

    -- ** ContinuousParameterRangeSpecification
    ContinuousParameterRangeSpecification,
    continuousParameterRangeSpecification,
    cprsMinValue,
    cprsMaxValue,

    -- ** CustomImage
    CustomImage,
    customImage,
    ciImageVersionNumber,
    ciImageName,
    ciAppImageConfigName,

    -- ** DataCaptureConfig
    DataCaptureConfig,
    dataCaptureConfig,
    dccCaptureContentTypeHeader,
    dccKMSKeyId,
    dccEnableCapture,
    dccInitialSamplingPercentage,
    dccDestinationS3URI,
    dccCaptureOptions,

    -- ** DataCaptureConfigSummary
    DataCaptureConfigSummary,
    dataCaptureConfigSummary,
    dccsEnableCapture,
    dccsCaptureStatus,
    dccsCurrentSamplingPercentage,
    dccsDestinationS3URI,
    dccsKMSKeyId,

    -- ** DataProcessing
    DataProcessing,
    dataProcessing,
    dpOutputFilter,
    dpJoinSource,
    dpInputFilter,

    -- ** DataSource
    DataSource,
    dataSource,
    dsS3DataSource,
    dsFileSystemDataSource,

    -- ** DebugHookConfig
    DebugHookConfig,
    debugHookConfig,
    dhcLocalPath,
    dhcCollectionConfigurations,
    dhcHookParameters,
    dhcS3OutputPath,

    -- ** DebugRuleConfiguration
    DebugRuleConfiguration,
    debugRuleConfiguration,
    drcRuleParameters,
    drcS3OutputPath,
    drcLocalPath,
    drcInstanceType,
    drcVolumeSizeInGB,
    drcRuleConfigurationName,
    drcRuleEvaluatorImage,

    -- ** DebugRuleEvaluationStatus
    DebugRuleEvaluationStatus,
    debugRuleEvaluationStatus,
    dresLastModifiedTime,
    dresStatusDetails,
    dresRuleEvaluationStatus,
    dresRuleEvaluationJobARN,
    dresRuleConfigurationName,

    -- ** DeployedImage
    DeployedImage,
    deployedImage,
    diResolvedImage,
    diSpecifiedImage,
    diResolutionTime,

    -- ** DesiredWeightAndCapacity
    DesiredWeightAndCapacity,
    desiredWeightAndCapacity,
    dwacDesiredInstanceCount,
    dwacDesiredWeight,
    dwacVariantName,

    -- ** DomainDetails
    DomainDetails,
    domainDetails,
    ddCreationTime,
    ddStatus,
    ddDomainARN,
    ddURL,
    ddLastModifiedTime,
    ddDomainName,
    ddDomainId,

    -- ** EndpointConfigSummary
    EndpointConfigSummary,
    endpointConfigSummary,
    ecsEndpointConfigName,
    ecsEndpointConfigARN,
    ecsCreationTime,

    -- ** EndpointInput
    EndpointInput,
    endpointInput,
    eiS3DataDistributionType,
    eiS3InputMode,
    eiEndpointName,
    eiLocalPath,

    -- ** EndpointSummary
    EndpointSummary,
    endpointSummary,
    esEndpointName,
    esEndpointARN,
    esCreationTime,
    esLastModifiedTime,
    esEndpointStatus,

    -- ** Experiment
    Experiment,
    experiment,
    eCreationTime,
    eCreatedBy,
    eLastModifiedTime,
    eExperimentName,
    eExperimentARN,
    eSource,
    eDisplayName,
    eLastModifiedBy,
    eDescription,
    eTags,

    -- ** ExperimentConfig
    ExperimentConfig,
    experimentConfig,
    ecTrialComponentDisplayName,
    ecExperimentName,
    ecTrialName,

    -- ** ExperimentSource
    ExperimentSource,
    experimentSource,
    esSourceType,
    esSourceARN,

    -- ** ExperimentSummary
    ExperimentSummary,
    experimentSummary,
    expCreationTime,
    expLastModifiedTime,
    expExperimentName,
    expExperimentSource,
    expExperimentARN,
    expDisplayName,

    -- ** FileSystemConfig
    FileSystemConfig,
    fileSystemConfig,
    fscDefaultGid,
    fscMountPath,
    fscDefaultUid,

    -- ** FileSystemDataSource
    FileSystemDataSource,
    fileSystemDataSource,
    fsdsFileSystemId,
    fsdsFileSystemAccessMode,
    fsdsFileSystemType,
    fsdsDirectoryPath,

    -- ** Filter
    Filter,
    filter',
    fOperator,
    fValue,
    fName,

    -- ** FinalAutoMLJobObjectiveMetric
    FinalAutoMLJobObjectiveMetric,
    finalAutoMLJobObjectiveMetric,
    famljomType,
    famljomMetricName,
    famljomValue,

    -- ** FinalHyperParameterTuningJobObjectiveMetric
    FinalHyperParameterTuningJobObjectiveMetric,
    finalHyperParameterTuningJobObjectiveMetric,
    fhptjomType,
    fhptjomMetricName,
    fhptjomValue,

    -- ** FlowDefinitionOutputConfig
    FlowDefinitionOutputConfig,
    flowDefinitionOutputConfig,
    fdocKMSKeyId,
    fdocS3OutputPath,

    -- ** FlowDefinitionSummary
    FlowDefinitionSummary,
    flowDefinitionSummary,
    fdsFailureReason,
    fdsFlowDefinitionName,
    fdsFlowDefinitionARN,
    fdsFlowDefinitionStatus,
    fdsCreationTime,

    -- ** GitConfig
    GitConfig,
    gitConfig,
    gcBranch,
    gcSecretARN,
    gcRepositoryURL,

    -- ** GitConfigForUpdate
    GitConfigForUpdate,
    gitConfigForUpdate,
    gcfuSecretARN,

    -- ** HumanLoopActivationConditionsConfig
    HumanLoopActivationConditionsConfig,
    humanLoopActivationConditionsConfig,
    hlaccHumanLoopActivationConditions,

    -- ** HumanLoopActivationConfig
    HumanLoopActivationConfig,
    humanLoopActivationConfig,
    hlacHumanLoopActivationConditionsConfig,

    -- ** HumanLoopConfig
    HumanLoopConfig,
    humanLoopConfig,
    hlcTaskKeywords,
    hlcPublicWorkforceTaskPrice,
    hlcTaskTimeLimitInSeconds,
    hlcTaskAvailabilityLifetimeInSeconds,
    hlcWorkteamARN,
    hlcHumanTaskUiARN,
    hlcTaskTitle,
    hlcTaskDescription,
    hlcTaskCount,

    -- ** HumanLoopRequestSource
    HumanLoopRequestSource,
    humanLoopRequestSource,
    hlrsAWSManagedHumanLoopRequestSource,

    -- ** HumanTaskConfig
    HumanTaskConfig,
    humanTaskConfig,
    htcTaskKeywords,
    htcPublicWorkforceTaskPrice,
    htcTaskAvailabilityLifetimeInSeconds,
    htcMaxConcurrentTaskCount,
    htcWorkteamARN,
    htcUiConfig,
    htcPreHumanTaskLambdaARN,
    htcTaskTitle,
    htcTaskDescription,
    htcNumberOfHumanWorkersPerDataObject,
    htcTaskTimeLimitInSeconds,
    htcAnnotationConsolidationConfig,

    -- ** HumanTaskUiSummary
    HumanTaskUiSummary,
    humanTaskUiSummary,
    htusHumanTaskUiName,
    htusHumanTaskUiARN,
    htusCreationTime,

    -- ** HyperParameterAlgorithmSpecification
    HyperParameterAlgorithmSpecification,
    hyperParameterAlgorithmSpecification,
    hpasAlgorithmName,
    hpasTrainingImage,
    hpasMetricDefinitions,
    hpasTrainingInputMode,

    -- ** HyperParameterSpecification
    HyperParameterSpecification,
    hyperParameterSpecification,
    hpsIsTunable,
    hpsRange,
    hpsDefaultValue,
    hpsIsRequired,
    hpsDescription,
    hpsName,
    hpsType,

    -- ** HyperParameterTrainingJobDefinition
    HyperParameterTrainingJobDefinition,
    hyperParameterTrainingJobDefinition,
    hptjdTuningObjective,
    hptjdCheckpointConfig,
    hptjdHyperParameterRanges,
    hptjdEnableNetworkIsolation,
    hptjdStaticHyperParameters,
    hptjdEnableManagedSpotTraining,
    hptjdInputDataConfig,
    hptjdVPCConfig,
    hptjdDefinitionName,
    hptjdEnableInterContainerTrafficEncryption,
    hptjdAlgorithmSpecification,
    hptjdRoleARN,
    hptjdOutputDataConfig,
    hptjdResourceConfig,
    hptjdStoppingCondition,

    -- ** HyperParameterTrainingJobSummary
    HyperParameterTrainingJobSummary,
    hyperParameterTrainingJobSummary,
    hptjsFailureReason,
    hptjsTuningJobName,
    hptjsTrainingEndTime,
    hptjsObjectiveStatus,
    hptjsTrainingJobDefinitionName,
    hptjsTrainingStartTime,
    hptjsFinalHyperParameterTuningJobObjectiveMetric,
    hptjsTrainingJobName,
    hptjsTrainingJobARN,
    hptjsCreationTime,
    hptjsTrainingJobStatus,
    hptjsTunedHyperParameters,

    -- ** HyperParameterTuningJobConfig
    HyperParameterTuningJobConfig,
    hyperParameterTuningJobConfig,
    hptjcTuningJobCompletionCriteria,
    hptjcParameterRanges,
    hptjcHyperParameterTuningJobObjective,
    hptjcTrainingJobEarlyStoppingType,
    hptjcStrategy,
    hptjcResourceLimits,

    -- ** HyperParameterTuningJobObjective
    HyperParameterTuningJobObjective,
    hyperParameterTuningJobObjective,
    hptjoType,
    hptjoMetricName,

    -- ** HyperParameterTuningJobSummary
    HyperParameterTuningJobSummary,
    hyperParameterTuningJobSummary,
    hResourceLimits,
    hLastModifiedTime,
    hHyperParameterTuningEndTime,
    hHyperParameterTuningJobName,
    hHyperParameterTuningJobARN,
    hHyperParameterTuningJobStatus,
    hStrategy,
    hCreationTime,
    hTrainingJobStatusCounters,
    hObjectiveStatusCounters,

    -- ** HyperParameterTuningJobWarmStartConfig
    HyperParameterTuningJobWarmStartConfig,
    hyperParameterTuningJobWarmStartConfig,
    hptjwscParentHyperParameterTuningJobs,
    hptjwscWarmStartType,

    -- ** Image
    Image,
    image,
    iFailureReason,
    iDisplayName,
    iDescription,
    iCreationTime,
    iImageARN,
    iImageName,
    iImageStatus,
    iLastModifiedTime,

    -- ** ImageConfig
    ImageConfig,
    imageConfig,
    icRepositoryAccessMode,

    -- ** ImageVersion
    ImageVersion,
    imageVersion,
    ivFailureReason,
    ivCreationTime,
    ivImageARN,
    ivImageVersionARN,
    ivImageVersionStatus,
    ivLastModifiedTime,
    ivVersion,

    -- ** InferenceSpecification
    InferenceSpecification,
    inferenceSpecification,
    isContainers,
    isSupportedTransformInstanceTypes,
    isSupportedRealtimeInferenceInstanceTypes,
    isSupportedContentTypes,
    isSupportedResponseMIMETypes,

    -- ** InputConfig
    InputConfig,
    inputConfig,
    icS3URI,
    icDataInputConfig,
    icFramework,

    -- ** IntegerParameterRange
    IntegerParameterRange,
    integerParameterRange,
    iprScalingType,
    iprName,
    iprMinValue,
    iprMaxValue,

    -- ** IntegerParameterRangeSpecification
    IntegerParameterRangeSpecification,
    integerParameterRangeSpecification,
    iprsMinValue,
    iprsMaxValue,

    -- ** JupyterServerAppSettings
    JupyterServerAppSettings,
    jupyterServerAppSettings,
    jsasDefaultResourceSpec,

    -- ** KernelGatewayAppSettings
    KernelGatewayAppSettings,
    kernelGatewayAppSettings,
    kgasDefaultResourceSpec,
    kgasCustomImages,

    -- ** KernelGatewayImageConfig
    KernelGatewayImageConfig,
    kernelGatewayImageConfig,
    kgicFileSystemConfig,
    kgicKernelSpecs,

    -- ** KernelSpec
    KernelSpec,
    kernelSpec,
    ksDisplayName,
    ksName,

    -- ** LabelCounters
    LabelCounters,
    labelCounters,
    lcMachineLabeled,
    lcTotalLabeled,
    lcFailedNonRetryableError,
    lcUnlabeled,
    lcHumanLabeled,

    -- ** LabelCountersForWorkteam
    LabelCountersForWorkteam,
    labelCountersForWorkteam,
    lcfwPendingHuman,
    lcfwTotal,
    lcfwHumanLabeled,

    -- ** LabelingJobAlgorithmsConfig
    LabelingJobAlgorithmsConfig,
    labelingJobAlgorithmsConfig,
    ljacLabelingJobResourceConfig,
    ljacInitialActiveLearningModelARN,
    ljacLabelingJobAlgorithmSpecificationARN,

    -- ** LabelingJobDataAttributes
    LabelingJobDataAttributes,
    labelingJobDataAttributes,
    ljdaContentClassifiers,

    -- ** LabelingJobDataSource
    LabelingJobDataSource,
    labelingJobDataSource,
    ljdsS3DataSource,
    ljdsSNSDataSource,

    -- ** LabelingJobForWorkteamSummary
    LabelingJobForWorkteamSummary,
    labelingJobForWorkteamSummary,
    ljfwsNumberOfHumanWorkersPerDataObject,
    ljfwsLabelCounters,
    ljfwsLabelingJobName,
    ljfwsJobReferenceCode,
    ljfwsWorkRequesterAccountId,
    ljfwsCreationTime,

    -- ** LabelingJobInputConfig
    LabelingJobInputConfig,
    labelingJobInputConfig,
    ljicDataAttributes,
    ljicDataSource,

    -- ** LabelingJobOutput
    LabelingJobOutput,
    labelingJobOutput,
    ljoFinalActiveLearningModelARN,
    ljoOutputDatasetS3URI,

    -- ** LabelingJobOutputConfig
    LabelingJobOutputConfig,
    labelingJobOutputConfig,
    ljocSNSTopicARN,
    ljocKMSKeyId,
    ljocS3OutputPath,

    -- ** LabelingJobResourceConfig
    LabelingJobResourceConfig,
    labelingJobResourceConfig,
    ljrcVolumeKMSKeyId,

    -- ** LabelingJobS3DataSource
    LabelingJobS3DataSource,
    labelingJobS3DataSource,
    ljsdsManifestS3URI,

    -- ** LabelingJobSNSDataSource
    LabelingJobSNSDataSource,
    labelingJobSNSDataSource,
    ljsdsSNSTopicARN,

    -- ** LabelingJobStoppingConditions
    LabelingJobStoppingConditions,
    labelingJobStoppingConditions,
    ljscMaxHumanLabeledObjectCount,
    ljscMaxPercentageOfInputDatasetLabeled,

    -- ** LabelingJobSummary
    LabelingJobSummary,
    labelingJobSummary,
    ljsFailureReason,
    ljsAnnotationConsolidationLambdaARN,
    ljsInputConfig,
    ljsLabelingJobOutput,
    ljsLabelingJobName,
    ljsLabelingJobARN,
    ljsCreationTime,
    ljsLastModifiedTime,
    ljsLabelingJobStatus,
    ljsLabelCounters,
    ljsWorkteamARN,
    ljsPreHumanTaskLambdaARN,

    -- ** MemberDefinition
    MemberDefinition,
    memberDefinition,
    mdOidcMemberDefinition,
    mdCognitoMemberDefinition,

    -- ** MetricData
    MetricData,
    metricData,
    mdMetricName,
    mdValue,
    mdTimestamp,

    -- ** MetricDefinition
    MetricDefinition,
    metricDefinition,
    mdName,
    mdRegex,

    -- ** ModelArtifacts
    ModelArtifacts,
    modelArtifacts,
    maS3ModelArtifacts,

    -- ** ModelClientConfig
    ModelClientConfig,
    modelClientConfig,
    mccInvocationsTimeoutInSeconds,
    mccInvocationsMaxRetries,

    -- ** ModelPackageContainerDefinition
    ModelPackageContainerDefinition,
    modelPackageContainerDefinition,
    mpcdModelDataURL,
    mpcdImageDigest,
    mpcdContainerHostname,
    mpcdProductId,
    mpcdImage,

    -- ** ModelPackageStatusDetails
    ModelPackageStatusDetails,
    modelPackageStatusDetails,
    mpsdImageScanStatuses,
    mpsdValidationStatuses,

    -- ** ModelPackageStatusItem
    ModelPackageStatusItem,
    modelPackageStatusItem,
    mpsiFailureReason,
    mpsiName,
    mpsiStatus,

    -- ** ModelPackageSummary
    ModelPackageSummary,
    modelPackageSummary,
    mpsModelPackageDescription,
    mpsModelPackageName,
    mpsModelPackageARN,
    mpsCreationTime,
    mpsModelPackageStatus,

    -- ** ModelPackageValidationProfile
    ModelPackageValidationProfile,
    modelPackageValidationProfile,
    mpvpProfileName,
    mpvpTransformJobDefinition,

    -- ** ModelPackageValidationSpecification
    ModelPackageValidationSpecification,
    modelPackageValidationSpecification,
    mpvsValidationRole,
    mpvsValidationProfiles,

    -- ** ModelSummary
    ModelSummary,
    modelSummary,
    msModelName,
    msModelARN,
    msCreationTime,

    -- ** MonitoringAppSpecification
    MonitoringAppSpecification,
    monitoringAppSpecification,
    masContainerArguments,
    masRecordPreprocessorSourceURI,
    masContainerEntrypoint,
    masPostAnalyticsProcessorSourceURI,
    masImageURI,

    -- ** MonitoringBaselineConfig
    MonitoringBaselineConfig,
    monitoringBaselineConfig,
    mbcConstraintsResource,
    mbcStatisticsResource,

    -- ** MonitoringClusterConfig
    MonitoringClusterConfig,
    monitoringClusterConfig,
    mccVolumeKMSKeyId,
    mccInstanceCount,
    mccInstanceType,
    mccVolumeSizeInGB,

    -- ** MonitoringConstraintsResource
    MonitoringConstraintsResource,
    monitoringConstraintsResource,
    mcrS3URI,

    -- ** MonitoringExecutionSummary
    MonitoringExecutionSummary,
    monitoringExecutionSummary,
    mesFailureReason,
    mesEndpointName,
    mesProcessingJobARN,
    mesMonitoringScheduleName,
    mesScheduledTime,
    mesCreationTime,
    mesLastModifiedTime,
    mesMonitoringExecutionStatus,

    -- ** MonitoringInput
    MonitoringInput,
    monitoringInput,
    miEndpointInput,

    -- ** MonitoringJobDefinition
    MonitoringJobDefinition,
    monitoringJobDefinition,
    mjdEnvironment,
    mjdStoppingCondition,
    mjdNetworkConfig,
    mjdBaselineConfig,
    mjdMonitoringInputs,
    mjdMonitoringOutputConfig,
    mjdMonitoringResources,
    mjdMonitoringAppSpecification,
    mjdRoleARN,

    -- ** MonitoringOutput
    MonitoringOutput,
    monitoringOutput,
    moS3Output,

    -- ** MonitoringOutputConfig
    MonitoringOutputConfig,
    monitoringOutputConfig,
    mocKMSKeyId,
    mocMonitoringOutputs,

    -- ** MonitoringResources
    MonitoringResources,
    monitoringResources,
    mrClusterConfig,

    -- ** MonitoringS3Output
    MonitoringS3Output,
    monitoringS3Output,
    msoS3UploadMode,
    msoS3URI,
    msoLocalPath,

    -- ** MonitoringScheduleConfig
    MonitoringScheduleConfig,
    monitoringScheduleConfig,
    mscScheduleConfig,
    mscMonitoringJobDefinition,

    -- ** MonitoringScheduleSummary
    MonitoringScheduleSummary,
    monitoringScheduleSummary,
    mssEndpointName,
    mssMonitoringScheduleName,
    mssMonitoringScheduleARN,
    mssCreationTime,
    mssLastModifiedTime,
    mssMonitoringScheduleStatus,

    -- ** MonitoringStatisticsResource
    MonitoringStatisticsResource,
    monitoringStatisticsResource,
    msrS3URI,

    -- ** MonitoringStoppingCondition
    MonitoringStoppingCondition,
    monitoringStoppingCondition,
    mscMaxRuntimeInSeconds,

    -- ** NestedFilters
    NestedFilters,
    nestedFilters,
    nfNestedPropertyName,
    nfFilters,

    -- ** NetworkConfig
    NetworkConfig,
    networkConfig,
    ncEnableNetworkIsolation,
    ncVPCConfig,
    ncEnableInterContainerTrafficEncryption,

    -- ** NotebookInstanceLifecycleConfigSummary
    NotebookInstanceLifecycleConfigSummary,
    notebookInstanceLifecycleConfigSummary,
    nilcsCreationTime,
    nilcsLastModifiedTime,
    nilcsNotebookInstanceLifecycleConfigName,
    nilcsNotebookInstanceLifecycleConfigARN,

    -- ** NotebookInstanceLifecycleHook
    NotebookInstanceLifecycleHook,
    notebookInstanceLifecycleHook,
    nilhContent,

    -- ** NotebookInstanceSummary
    NotebookInstanceSummary,
    notebookInstanceSummary,
    nisCreationTime,
    nisAdditionalCodeRepositories,
    nisURL,
    nisLastModifiedTime,
    nisInstanceType,
    nisNotebookInstanceStatus,
    nisDefaultCodeRepository,
    nisNotebookInstanceLifecycleConfigName,
    nisNotebookInstanceName,
    nisNotebookInstanceARN,

    -- ** NotificationConfiguration
    NotificationConfiguration,
    notificationConfiguration,
    ncNotificationTopicARN,

    -- ** ObjectiveStatusCounters
    ObjectiveStatusCounters,
    objectiveStatusCounters,
    oscPending,
    oscSucceeded,
    oscFailed,

    -- ** OidcConfig
    OidcConfig,
    oidcConfig,
    ocClientId,
    ocClientSecret,
    ocIssuer,
    ocAuthorizationEndpoint,
    ocTokenEndpoint,
    ocUserInfoEndpoint,
    ocLogoutEndpoint,
    ocJwksURI,

    -- ** OidcConfigForResponse
    OidcConfigForResponse,
    oidcConfigForResponse,
    ocfClientId,
    ocfJwksURI,
    ocfUserInfoEndpoint,
    ocfAuthorizationEndpoint,
    ocfTokenEndpoint,
    ocfIssuer,
    ocfLogoutEndpoint,

    -- ** OidcMemberDefinition
    OidcMemberDefinition,
    oidcMemberDefinition,
    omdGroups,

    -- ** OutputConfig
    OutputConfig,
    outputConfig,
    ocTargetPlatform,
    ocCompilerOptions,
    ocTargetDevice,
    ocS3OutputLocation,

    -- ** OutputDataConfig
    OutputDataConfig,
    outputDataConfig,
    odcKMSKeyId,
    odcS3OutputPath,

    -- ** ParameterRange
    ParameterRange,
    parameterRange,
    prCategoricalParameterRangeSpecification,
    prIntegerParameterRangeSpecification,
    prContinuousParameterRangeSpecification,

    -- ** ParameterRanges
    ParameterRanges,
    parameterRanges,
    prCategoricalParameterRanges,
    prIntegerParameterRanges,
    prContinuousParameterRanges,

    -- ** Parent
    Parent,
    parent,
    pExperimentName,
    pTrialName,

    -- ** ParentHyperParameterTuningJob
    ParentHyperParameterTuningJob,
    parentHyperParameterTuningJob,
    phptjHyperParameterTuningJobName,

    -- ** ProcessingClusterConfig
    ProcessingClusterConfig,
    processingClusterConfig,
    pccVolumeKMSKeyId,
    pccInstanceCount,
    pccInstanceType,
    pccVolumeSizeInGB,

    -- ** ProcessingInput
    ProcessingInput,
    processingInput,
    piInputName,
    piS3Input,

    -- ** ProcessingJob
    ProcessingJob,
    processingJob,
    pjCreationTime,
    pjFailureReason,
    pjMonitoringScheduleARN,
    pjAppSpecification,
    pjProcessingResources,
    pjEnvironment,
    pjProcessingJobName,
    pjStoppingCondition,
    pjExperimentConfig,
    pjLastModifiedTime,
    pjProcessingInputs,
    pjNetworkConfig,
    pjAutoMLJobARN,
    pjTrainingJobARN,
    pjProcessingJobStatus,
    pjExitMessage,
    pjProcessingOutputConfig,
    pjProcessingStartTime,
    pjProcessingEndTime,
    pjTags,
    pjProcessingJobARN,
    pjRoleARN,

    -- ** ProcessingJobSummary
    ProcessingJobSummary,
    processingJobSummary,
    pjsFailureReason,
    pjsLastModifiedTime,
    pjsExitMessage,
    pjsProcessingEndTime,
    pjsProcessingJobName,
    pjsProcessingJobARN,
    pjsCreationTime,
    pjsProcessingJobStatus,

    -- ** ProcessingOutput
    ProcessingOutput,
    processingOutput,
    poOutputName,
    poS3Output,

    -- ** ProcessingOutputConfig
    ProcessingOutputConfig,
    processingOutputConfig,
    pocKMSKeyId,
    pocOutputs,

    -- ** ProcessingResources
    ProcessingResources,
    processingResources,
    prClusterConfig,

    -- ** ProcessingS3Input
    ProcessingS3Input,
    processingS3Input,
    psiS3DataDistributionType,
    psiS3CompressionType,
    psiS3URI,
    psiLocalPath,
    psiS3DataType,
    psiS3InputMode,

    -- ** ProcessingS3Output
    ProcessingS3Output,
    processingS3Output,
    psoS3URI,
    psoLocalPath,
    psoS3UploadMode,

    -- ** ProcessingStoppingCondition
    ProcessingStoppingCondition,
    processingStoppingCondition,
    pscMaxRuntimeInSeconds,

    -- ** ProductionVariant
    ProductionVariant,
    productionVariant,
    pvAcceleratorType,
    pvInitialVariantWeight,
    pvVariantName,
    pvModelName,
    pvInitialInstanceCount,
    pvInstanceType,

    -- ** ProductionVariantSummary
    ProductionVariantSummary,
    productionVariantSummary,
    pvsDesiredInstanceCount,
    pvsDesiredWeight,
    pvsCurrentWeight,
    pvsCurrentInstanceCount,
    pvsDeployedImages,
    pvsVariantName,

    -- ** PropertyNameQuery
    PropertyNameQuery,
    propertyNameQuery,
    pnqPropertyNameHint,

    -- ** PropertyNameSuggestion
    PropertyNameSuggestion,
    propertyNameSuggestion,
    pnsPropertyName,

    -- ** PublicWorkforceTaskPrice
    PublicWorkforceTaskPrice,
    publicWorkforceTaskPrice,
    pwtpAmountInUsd,

    -- ** RenderableTask
    RenderableTask,
    renderableTask,
    rtInput,

    -- ** RenderingError
    RenderingError,
    renderingError,
    reCode,
    reMessage,

    -- ** ResolvedAttributes
    ResolvedAttributes,
    resolvedAttributes,
    raProblemType,
    raAutoMLJobObjective,
    raCompletionCriteria,

    -- ** ResourceConfig
    ResourceConfig,
    resourceConfig,
    rcVolumeKMSKeyId,
    rcInstanceType,
    rcInstanceCount,
    rcVolumeSizeInGB,

    -- ** ResourceLimits
    ResourceLimits,
    resourceLimits,
    rlMaxNumberOfTrainingJobs,
    rlMaxParallelTrainingJobs,

    -- ** ResourceSpec
    ResourceSpec,
    resourceSpec,
    rsInstanceType,
    rsSageMakerImageARN,
    rsSageMakerImageVersionARN,

    -- ** RetentionPolicy
    RetentionPolicy,
    retentionPolicy,
    rpHomeEfsFileSystem,

    -- ** S3DataSource
    S3DataSource,
    s3DataSource,
    sdsS3DataDistributionType,
    sdsAttributeNames,
    sdsS3DataType,
    sdsS3URI,

    -- ** ScheduleConfig
    ScheduleConfig,
    scheduleConfig,
    scScheduleExpression,

    -- ** SearchExpression
    SearchExpression,
    searchExpression,
    seSubExpressions,
    seOperator,
    seFilters,
    seNestedFilters,

    -- ** SearchRecord
    SearchRecord,
    searchRecord,
    srTrainingJob,
    srTrial,
    srTrialComponent,
    srExperiment,

    -- ** SecondaryStatusTransition
    SecondaryStatusTransition,
    secondaryStatusTransition,
    sstStatusMessage,
    sstEndTime,
    sstStatus,
    sstStartTime,

    -- ** SharingSettings
    SharingSettings,
    sharingSettings,
    ssS3KMSKeyId,
    ssS3OutputPath,
    ssNotebookOutputOption,

    -- ** ShuffleConfig
    ShuffleConfig,
    shuffleConfig,
    scSeed,

    -- ** SourceAlgorithm
    SourceAlgorithm,
    sourceAlgorithm,
    saModelDataURL,
    saAlgorithmName,

    -- ** SourceAlgorithmSpecification
    SourceAlgorithmSpecification,
    sourceAlgorithmSpecification,
    sasSourceAlgorithms,

    -- ** SourceIPConfig
    SourceIPConfig,
    sourceIPConfig,
    sicCidrs,

    -- ** StoppingCondition
    StoppingCondition,
    stoppingCondition,
    scMaxWaitTimeInSeconds,
    scMaxRuntimeInSeconds,

    -- ** SubscribedWorkteam
    SubscribedWorkteam,
    subscribedWorkteam,
    swMarketplaceTitle,
    swSellerName,
    swListingId,
    swMarketplaceDescription,
    swWorkteamARN,

    -- ** SuggestionQuery
    SuggestionQuery,
    suggestionQuery,
    sqPropertyNameQuery,

    -- ** Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- ** TargetPlatform
    TargetPlatform,
    targetPlatform,
    tpAccelerator,
    tpOS,
    tpArch,

    -- ** TensorBoardAppSettings
    TensorBoardAppSettings,
    tensorBoardAppSettings,
    tbasDefaultResourceSpec,

    -- ** TensorBoardOutputConfig
    TensorBoardOutputConfig,
    tensorBoardOutputConfig,
    tbocLocalPath,
    tbocS3OutputPath,

    -- ** TrainingJob
    TrainingJob,
    trainingJob,
    tjCreationTime,
    tjLabelingJobARN,
    tjFailureReason,
    tjSecondaryStatusTransitions,
    tjModelArtifacts,
    tjTrainingEndTime,
    tjBillableTimeInSeconds,
    tjDebugHookConfig,
    tjCheckpointConfig,
    tjStoppingCondition,
    tjDebugRuleEvaluationStatuses,
    tjTrainingJobStatus,
    tjEnableNetworkIsolation,
    tjExperimentConfig,
    tjLastModifiedTime,
    tjDebugRuleConfigurations,
    tjEnableManagedSpotTraining,
    tjAutoMLJobARN,
    tjHyperParameters,
    tjInputDataConfig,
    tjVPCConfig,
    tjTrainingJobARN,
    tjAlgorithmSpecification,
    tjFinalMetricDataList,
    tjOutputDataConfig,
    tjTrainingStartTime,
    tjTuningJobARN,
    tjTrainingJobName,
    tjResourceConfig,
    tjEnableInterContainerTrafficEncryption,
    tjTensorBoardOutputConfig,
    tjSecondaryStatus,
    tjTags,
    tjTrainingTimeInSeconds,
    tjRoleARN,

    -- ** TrainingJobDefinition
    TrainingJobDefinition,
    trainingJobDefinition,
    tjdHyperParameters,
    tjdTrainingInputMode,
    tjdInputDataConfig,
    tjdOutputDataConfig,
    tjdResourceConfig,
    tjdStoppingCondition,

    -- ** TrainingJobStatusCounters
    TrainingJobStatusCounters,
    trainingJobStatusCounters,
    tjscStopped,
    tjscRetryableError,
    tjscInProgress,
    tjscNonRetryableError,
    tjscCompleted,

    -- ** TrainingJobSummary
    TrainingJobSummary,
    trainingJobSummary,
    tjsjTrainingEndTime,
    tjsjLastModifiedTime,
    tjsjTrainingJobName,
    tjsjTrainingJobARN,
    tjsjCreationTime,
    tjsjTrainingJobStatus,

    -- ** TrainingSpecification
    TrainingSpecification,
    trainingSpecification,
    tsTrainingImageDigest,
    tsSupportsDistributedTraining,
    tsSupportedHyperParameters,
    tsSupportedTuningJobObjectiveMetrics,
    tsMetricDefinitions,
    tsTrainingImage,
    tsSupportedTrainingInstanceTypes,
    tsTrainingChannels,

    -- ** TransformDataSource
    TransformDataSource,
    transformDataSource,
    tdsS3DataSource,

    -- ** TransformInput
    TransformInput,
    transformInput,
    tiSplitType,
    tiCompressionType,
    tiContentType,
    tiDataSource,

    -- ** TransformJob
    TransformJob,
    transformJob,
    traCreationTime,
    traLabelingJobARN,
    traTransformJobName,
    traFailureReason,
    traModelClientConfig,
    traBatchStrategy,
    traMaxPayloadInMB,
    traEnvironment,
    traTransformResources,
    traModelName,
    traExperimentConfig,
    traTransformEndTime,
    traTransformStartTime,
    traAutoMLJobARN,
    traTransformJobStatus,
    traTransformInput,
    traMaxConcurrentTransforms,
    traTransformOutput,
    traDataProcessing,
    traTransformJobARN,
    traTags,

    -- ** TransformJobDefinition
    TransformJobDefinition,
    transformJobDefinition,
    tjdBatchStrategy,
    tjdMaxPayloadInMB,
    tjdEnvironment,
    tjdMaxConcurrentTransforms,
    tjdTransformInput,
    tjdTransformOutput,
    tjdTransformResources,

    -- ** TransformJobSummary
    TransformJobSummary,
    transformJobSummary,
    tjsFailureReason,
    tjsLastModifiedTime,
    tjsTransformEndTime,
    tjsTransformJobName,
    tjsTransformJobARN,
    tjsCreationTime,
    tjsTransformJobStatus,

    -- ** TransformOutput
    TransformOutput,
    transformOutput,
    toAssembleWith,
    toAccept,
    toKMSKeyId,
    toS3OutputPath,

    -- ** TransformResources
    TransformResources,
    transformResources,
    trVolumeKMSKeyId,
    trInstanceType,
    trInstanceCount,

    -- ** TransformS3DataSource
    TransformS3DataSource,
    transformS3DataSource,
    tsdsS3DataType,
    tsdsS3URI,

    -- ** Trial
    Trial,
    trial,
    tCreationTime,
    tTrialComponentSummaries,
    tTrialARN,
    tCreatedBy,
    tLastModifiedTime,
    tExperimentName,
    tSource,
    tDisplayName,
    tTrialName,
    tLastModifiedBy,
    tTags,

    -- ** TrialComponent
    TrialComponent,
    trialComponent,
    tcCreationTime,
    tcStatus,
    tcSourceDetail,
    tcMetrics,
    tcOutputArtifacts,
    tcStartTime,
    tcCreatedBy,
    tcLastModifiedTime,
    tcParents,
    tcEndTime,
    tcTrialComponentName,
    tcParameters,
    tcSource,
    tcDisplayName,
    tcLastModifiedBy,
    tcTrialComponentARN,
    tcInputArtifacts,
    tcTags,

    -- ** TrialComponentArtifact
    TrialComponentArtifact,
    trialComponentArtifact,
    tcaMediaType,
    tcaValue,

    -- ** TrialComponentMetricSummary
    TrialComponentMetricSummary,
    trialComponentMetricSummary,
    tcmsMax,
    tcmsSourceARN,
    tcmsAvg,
    tcmsCount,
    tcmsMetricName,
    tcmsStdDev,
    tcmsMin,
    tcmsLast,
    tcmsTimeStamp,

    -- ** TrialComponentParameterValue
    TrialComponentParameterValue,
    trialComponentParameterValue,
    tcpvNumberValue,
    tcpvStringValue,

    -- ** TrialComponentSimpleSummary
    TrialComponentSimpleSummary,
    trialComponentSimpleSummary,
    tcssCreationTime,
    tcssCreatedBy,
    tcssTrialComponentName,
    tcssTrialComponentARN,
    tcssTrialComponentSource,

    -- ** TrialComponentSource
    TrialComponentSource,
    trialComponentSource,
    tcsSourceType,
    tcsSourceARN,

    -- ** TrialComponentSourceDetail
    TrialComponentSourceDetail,
    trialComponentSourceDetail,
    tcsdTrainingJob,
    tcsdSourceARN,
    tcsdProcessingJob,
    tcsdTransformJob,

    -- ** TrialComponentStatus
    TrialComponentStatus,
    trialComponentStatus,
    tcsPrimaryStatus,
    tcsMessage,

    -- ** TrialComponentSummary
    TrialComponentSummary,
    trialComponentSummary,
    tcsCreationTime,
    tcsStatus,
    tcsStartTime,
    tcsCreatedBy,
    tcsLastModifiedTime,
    tcsEndTime,
    tcsTrialComponentName,
    tcsDisplayName,
    tcsLastModifiedBy,
    tcsTrialComponentARN,
    tcsTrialComponentSource,

    -- ** TrialSource
    TrialSource,
    trialSource,
    tsSourceType,
    tsSourceARN,

    -- ** TrialSummary
    TrialSummary,
    trialSummary,
    tsCreationTime,
    tsTrialARN,
    tsLastModifiedTime,
    tsTrialSource,
    tsDisplayName,
    tsTrialName,

    -- ** TuningJobCompletionCriteria
    TuningJobCompletionCriteria,
    tuningJobCompletionCriteria,
    tjccTargetObjectiveMetricValue,

    -- ** USD
    USD,
    uSD,
    usdCents,
    usdDollars,
    usdTenthFractionsOfACent,

    -- ** UiConfig
    UiConfig,
    uiConfig,
    ucUiTemplateS3URI,
    ucHumanTaskUiARN,

    -- ** UiTemplate
    UiTemplate,
    uiTemplate,
    utContent,

    -- ** UiTemplateInfo
    UiTemplateInfo,
    uiTemplateInfo,
    utiURL,
    utiContentSha256,

    -- ** UserContext
    UserContext,
    userContext,
    ucUserProfileName,
    ucUserProfileARN,
    ucDomainId,

    -- ** UserProfileDetails
    UserProfileDetails,
    userProfileDetails,
    updCreationTime,
    updStatus,
    updUserProfileName,
    updLastModifiedTime,
    updDomainId,

    -- ** UserSettings
    UserSettings,
    userSettings,
    usTensorBoardAppSettings,
    usKernelGatewayAppSettings,
    usSecurityGroups,
    usJupyterServerAppSettings,
    usSharingSettings,
    usExecutionRole,

    -- ** VPCConfig
    VPCConfig,
    vpcConfig,
    vcSecurityGroupIds,
    vcSubnets,

    -- ** VariantProperty
    VariantProperty,
    variantProperty,
    vpVariantPropertyType,

    -- ** Workforce
    Workforce,
    workforce,
    wSubDomain,
    wCreateDate,
    wSourceIPConfig,
    wCognitoConfig,
    wLastUpdatedDate,
    wOidcConfig,
    wWorkforceName,
    wWorkforceARN,

    -- ** Workteam
    Workteam,
    workteam,
    worSubDomain,
    worProductListingIds,
    worNotificationConfiguration,
    worCreateDate,
    worWorkforceARN,
    worLastUpdatedDate,
    worWorkteamName,
    worMemberDefinitions,
    worWorkteamARN,
    worDescription,
  )
where

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
import Network.AWS.SageMaker.CreatePresignedDomainURL
import Network.AWS.SageMaker.CreatePresignedNotebookInstanceURL
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

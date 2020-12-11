-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types
  ( -- * Service configuration
    sageMakerService,

    -- * Errors

    -- * AWSManagedHumanLoopRequestSource
    AWSManagedHumanLoopRequestSource (..),

    -- * AlgorithmSortBy
    AlgorithmSortBy (..),

    -- * AlgorithmStatus
    AlgorithmStatus (..),

    -- * AppImageConfigSortKey
    AppImageConfigSortKey (..),

    -- * AppInstanceType
    AppInstanceType (..),

    -- * AppNetworkAccessType
    AppNetworkAccessType (..),

    -- * AppSortKey
    AppSortKey (..),

    -- * AppStatus
    AppStatus (..),

    -- * AppType
    AppType (..),

    -- * AssemblyType
    AssemblyType (..),

    -- * AuthMode
    AuthMode (..),

    -- * AutoMLJobObjectiveType
    AutoMLJobObjectiveType (..),

    -- * AutoMLJobSecondaryStatus
    AutoMLJobSecondaryStatus (..),

    -- * AutoMLJobStatus
    AutoMLJobStatus (..),

    -- * AutoMLMetricEnum
    AutoMLMetricEnum (..),

    -- * AutoMLS3DataType
    AutoMLS3DataType (..),

    -- * AutoMLSortBy
    AutoMLSortBy (..),

    -- * AutoMLSortOrder
    AutoMLSortOrder (..),

    -- * BatchStrategy
    BatchStrategy (..),

    -- * BooleanOperator
    BooleanOperator (..),

    -- * CandidateSortBy
    CandidateSortBy (..),

    -- * CandidateStatus
    CandidateStatus (..),

    -- * CandidateStepType
    CandidateStepType (..),

    -- * CaptureMode
    CaptureMode (..),

    -- * CaptureStatus
    CaptureStatus (..),

    -- * CodeRepositorySortBy
    CodeRepositorySortBy (..),

    -- * CodeRepositorySortOrder
    CodeRepositorySortOrder (..),

    -- * CompilationJobStatus
    CompilationJobStatus (..),

    -- * CompressionType
    CompressionType (..),

    -- * ContainerMode
    ContainerMode (..),

    -- * ContentClassifier
    ContentClassifier (..),

    -- * DetailedAlgorithmStatus
    DetailedAlgorithmStatus (..),

    -- * DetailedModelPackageStatus
    DetailedModelPackageStatus (..),

    -- * DirectInternetAccess
    DirectInternetAccess (..),

    -- * DomainStatus
    DomainStatus (..),

    -- * EndpointConfigSortKey
    EndpointConfigSortKey (..),

    -- * EndpointSortKey
    EndpointSortKey (..),

    -- * EndpointStatus
    EndpointStatus (..),

    -- * ExecutionStatus
    ExecutionStatus (..),

    -- * FileSystemAccessMode
    FileSystemAccessMode (..),

    -- * FileSystemType
    FileSystemType (..),

    -- * FlowDefinitionStatus
    FlowDefinitionStatus (..),

    -- * Framework
    Framework (..),

    -- * HumanTaskUiStatus
    HumanTaskUiStatus (..),

    -- * HyperParameterScalingType
    HyperParameterScalingType (..),

    -- * HyperParameterTuningJobObjectiveType
    HyperParameterTuningJobObjectiveType (..),

    -- * HyperParameterTuningJobSortByOptions
    HyperParameterTuningJobSortByOptions (..),

    -- * HyperParameterTuningJobStatus
    HyperParameterTuningJobStatus (..),

    -- * HyperParameterTuningJobStrategyType
    HyperParameterTuningJobStrategyType (..),

    -- * HyperParameterTuningJobWarmStartType
    HyperParameterTuningJobWarmStartType (..),

    -- * ImageSortBy
    ImageSortBy (..),

    -- * ImageSortOrder
    ImageSortOrder (..),

    -- * ImageStatus
    ImageStatus (..),

    -- * ImageVersionSortBy
    ImageVersionSortBy (..),

    -- * ImageVersionSortOrder
    ImageVersionSortOrder (..),

    -- * ImageVersionStatus
    ImageVersionStatus (..),

    -- * InstanceType
    InstanceType (..),

    -- * JoinSource
    JoinSource (..),

    -- * LabelingJobStatus
    LabelingJobStatus (..),

    -- * ListCompilationJobsSortBy
    ListCompilationJobsSortBy (..),

    -- * ListLabelingJobsForWorkteamSortByOptions
    ListLabelingJobsForWorkteamSortByOptions (..),

    -- * ListWorkforcesSortByOptions
    ListWorkforcesSortByOptions (..),

    -- * ListWorkteamsSortByOptions
    ListWorkteamsSortByOptions (..),

    -- * ModelPackageSortBy
    ModelPackageSortBy (..),

    -- * ModelPackageStatus
    ModelPackageStatus (..),

    -- * ModelSortKey
    ModelSortKey (..),

    -- * MonitoringExecutionSortKey
    MonitoringExecutionSortKey (..),

    -- * MonitoringScheduleSortKey
    MonitoringScheduleSortKey (..),

    -- * NotebookInstanceAcceleratorType
    NotebookInstanceAcceleratorType (..),

    -- * NotebookInstanceLifecycleConfigSortKey
    NotebookInstanceLifecycleConfigSortKey (..),

    -- * NotebookInstanceLifecycleConfigSortOrder
    NotebookInstanceLifecycleConfigSortOrder (..),

    -- * NotebookInstanceSortKey
    NotebookInstanceSortKey (..),

    -- * NotebookInstanceSortOrder
    NotebookInstanceSortOrder (..),

    -- * NotebookInstanceStatus
    NotebookInstanceStatus (..),

    -- * NotebookOutputOption
    NotebookOutputOption (..),

    -- * ObjectiveStatus
    ObjectiveStatus (..),

    -- * Operator
    Operator (..),

    -- * OrderKey
    OrderKey (..),

    -- * ParameterType
    ParameterType (..),

    -- * ProblemType
    ProblemType (..),

    -- * ProcessingInstanceType
    ProcessingInstanceType (..),

    -- * ProcessingJobStatus
    ProcessingJobStatus (..),

    -- * ProcessingS3CompressionType
    ProcessingS3CompressionType (..),

    -- * ProcessingS3DataDistributionType
    ProcessingS3DataDistributionType (..),

    -- * ProcessingS3DataType
    ProcessingS3DataType (..),

    -- * ProcessingS3InputMode
    ProcessingS3InputMode (..),

    -- * ProcessingS3UploadMode
    ProcessingS3UploadMode (..),

    -- * ProductionVariantAcceleratorType
    ProductionVariantAcceleratorType (..),

    -- * ProductionVariantInstanceType
    ProductionVariantInstanceType (..),

    -- * RecordWrapper
    RecordWrapper (..),

    -- * RepositoryAccessMode
    RepositoryAccessMode (..),

    -- * ResourceType
    ResourceType (..),

    -- * RetentionType
    RetentionType (..),

    -- * RootAccess
    RootAccess (..),

    -- * RuleEvaluationStatus
    RuleEvaluationStatus (..),

    -- * S3DataDistribution
    S3DataDistribution (..),

    -- * S3DataType
    S3DataType (..),

    -- * ScheduleStatus
    ScheduleStatus (..),

    -- * SearchSortOrder
    SearchSortOrder (..),

    -- * SecondaryStatus
    SecondaryStatus (..),

    -- * SortBy
    SortBy (..),

    -- * SortExperimentsBy
    SortExperimentsBy (..),

    -- * SortOrder
    SortOrder (..),

    -- * SortTrialComponentsBy
    SortTrialComponentsBy (..),

    -- * SortTrialsBy
    SortTrialsBy (..),

    -- * SplitType
    SplitType (..),

    -- * TargetDevice
    TargetDevice (..),

    -- * TargetPlatformAccelerator
    TargetPlatformAccelerator (..),

    -- * TargetPlatformArch
    TargetPlatformArch (..),

    -- * TargetPlatformOS
    TargetPlatformOS (..),

    -- * TrainingInputMode
    TrainingInputMode (..),

    -- * TrainingInstanceType
    TrainingInstanceType (..),

    -- * TrainingJobEarlyStoppingType
    TrainingJobEarlyStoppingType (..),

    -- * TrainingJobSortByOptions
    TrainingJobSortByOptions (..),

    -- * TrainingJobStatus
    TrainingJobStatus (..),

    -- * TransformInstanceType
    TransformInstanceType (..),

    -- * TransformJobStatus
    TransformJobStatus (..),

    -- * TrialComponentPrimaryStatus
    TrialComponentPrimaryStatus (..),

    -- * UserProfileSortKey
    UserProfileSortKey (..),

    -- * UserProfileStatus
    UserProfileStatus (..),

    -- * VariantPropertyType
    VariantPropertyType (..),

    -- * AlgorithmSpecification
    AlgorithmSpecification (..),
    mkAlgorithmSpecification,
    asEnableSageMakerMetricsTimeSeries,
    asAlgorithmName,
    asTrainingImage,
    asMetricDefinitions,
    asTrainingInputMode,

    -- * AlgorithmStatusDetails
    AlgorithmStatusDetails (..),
    mkAlgorithmStatusDetails,
    asdImageScanStatuses,
    asdValidationStatuses,

    -- * AlgorithmStatusItem
    AlgorithmStatusItem (..),
    mkAlgorithmStatusItem,
    asiFailureReason,
    asiName,
    asiStatus,

    -- * AlgorithmSummary
    AlgorithmSummary (..),
    mkAlgorithmSummary,
    aAlgorithmDescription,
    aAlgorithmName,
    aAlgorithmARN,
    aCreationTime,
    aAlgorithmStatus,

    -- * AlgorithmValidationProfile
    AlgorithmValidationProfile (..),
    mkAlgorithmValidationProfile,
    avpTransformJobDefinition,
    avpProfileName,
    avpTrainingJobDefinition,

    -- * AlgorithmValidationSpecification
    AlgorithmValidationSpecification (..),
    mkAlgorithmValidationSpecification,
    avsValidationRole,
    avsValidationProfiles,

    -- * AnnotationConsolidationConfig
    AnnotationConsolidationConfig (..),
    mkAnnotationConsolidationConfig,
    accAnnotationConsolidationLambdaARN,

    -- * AppDetails
    AppDetails (..),
    mkAppDetails,
    adCreationTime,
    adStatus,
    adUserProfileName,
    adAppName,
    adDomainId,
    adAppType,

    -- * AppImageConfigDetails
    AppImageConfigDetails (..),
    mkAppImageConfigDetails,
    aicdCreationTime,
    aicdAppImageConfigName,
    aicdLastModifiedTime,
    aicdKernelGatewayImageConfig,
    aicdAppImageConfigARN,

    -- * AppSpecification
    AppSpecification (..),
    mkAppSpecification,
    asContainerArguments,
    asContainerEntrypoint,
    asImageURI,

    -- * AutoMLCandidate
    AutoMLCandidate (..),
    mkAutoMLCandidate,
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

    -- * AutoMLCandidateStep
    AutoMLCandidateStep (..),
    mkAutoMLCandidateStep,
    amlcsCandidateStepType,
    amlcsCandidateStepARN,
    amlcsCandidateStepName,

    -- * AutoMLChannel
    AutoMLChannel (..),
    mkAutoMLChannel,
    amlcCompressionType,
    amlcDataSource,
    amlcTargetAttributeName,

    -- * AutoMLContainerDefinition
    AutoMLContainerDefinition (..),
    mkAutoMLContainerDefinition,
    amlcdEnvironment,
    amlcdImage,
    amlcdModelDataURL,

    -- * AutoMLDataSource
    AutoMLDataSource (..),
    mkAutoMLDataSource,
    amldsS3DataSource,

    -- * AutoMLJobArtifacts
    AutoMLJobArtifacts (..),
    mkAutoMLJobArtifacts,
    amljaCandidateDefinitionNotebookLocation,
    amljaDataExplorationNotebookLocation,

    -- * AutoMLJobCompletionCriteria
    AutoMLJobCompletionCriteria (..),
    mkAutoMLJobCompletionCriteria,
    amljccMaxCandidates,
    amljccMaxRuntimePerTrainingJobInSeconds,
    amljccMaxAutoMLJobRuntimeInSeconds,

    -- * AutoMLJobConfig
    AutoMLJobConfig (..),
    mkAutoMLJobConfig,
    amljcSecurityConfig,
    amljcCompletionCriteria,

    -- * AutoMLJobObjective
    AutoMLJobObjective (..),
    mkAutoMLJobObjective,
    amljoMetricName,

    -- * AutoMLJobSummary
    AutoMLJobSummary (..),
    mkAutoMLJobSummary,
    amljsFailureReason,
    amljsEndTime,
    amljsAutoMLJobName,
    amljsAutoMLJobARN,
    amljsAutoMLJobStatus,
    amljsAutoMLJobSecondaryStatus,
    amljsCreationTime,
    amljsLastModifiedTime,

    -- * AutoMLOutputDataConfig
    AutoMLOutputDataConfig (..),
    mkAutoMLOutputDataConfig,
    amlodcKMSKeyId,
    amlodcS3OutputPath,

    -- * AutoMLS3DataSource
    AutoMLS3DataSource (..),
    mkAutoMLS3DataSource,
    amlsdsS3DataType,
    amlsdsS3URI,

    -- * AutoMLSecurityConfig
    AutoMLSecurityConfig (..),
    mkAutoMLSecurityConfig,
    amlscVPCConfig,
    amlscVolumeKMSKeyId,
    amlscEnableInterContainerTrafficEncryption,

    -- * CaptureContentTypeHeader
    CaptureContentTypeHeader (..),
    mkCaptureContentTypeHeader,
    ccthCSVContentTypes,
    ccthJSONContentTypes,

    -- * CaptureOption
    CaptureOption (..),
    mkCaptureOption,
    coCaptureMode,

    -- * CategoricalParameterRange
    CategoricalParameterRange (..),
    mkCategoricalParameterRange,
    cprName,
    cprValues,

    -- * CategoricalParameterRangeSpecification
    CategoricalParameterRangeSpecification (..),
    mkCategoricalParameterRangeSpecification,
    cprsValues,

    -- * Channel
    Channel (..),
    mkChannel,
    cShuffleConfig,
    cRecordWrapperType,
    cInputMode,
    cCompressionType,
    cContentType,
    cChannelName,
    cDataSource,

    -- * ChannelSpecification
    ChannelSpecification (..),
    mkChannelSpecification,
    csSupportedCompressionTypes,
    csIsRequired,
    csDescription,
    csName,
    csSupportedContentTypes,
    csSupportedInputModes,

    -- * CheckpointConfig
    CheckpointConfig (..),
    mkCheckpointConfig,
    ccLocalPath,
    ccS3URI,

    -- * CodeRepositorySummary
    CodeRepositorySummary (..),
    mkCodeRepositorySummary,
    crsGitConfig,
    crsCodeRepositoryName,
    crsCodeRepositoryARN,
    crsCreationTime,
    crsLastModifiedTime,

    -- * CognitoConfig
    CognitoConfig (..),
    mkCognitoConfig,
    ccUserPool,
    ccClientId,

    -- * CognitoMemberDefinition
    CognitoMemberDefinition (..),
    mkCognitoMemberDefinition,
    cmdUserPool,
    cmdUserGroup,
    cmdClientId,

    -- * CollectionConfiguration
    CollectionConfiguration (..),
    mkCollectionConfiguration,
    ccCollectionParameters,
    ccCollectionName,

    -- * CompilationJobSummary
    CompilationJobSummary (..),
    mkCompilationJobSummary,
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

    -- * ContainerDefinition
    ContainerDefinition (..),
    mkContainerDefinition,
    cdModelDataURL,
    cdImage,
    cdModelPackageName,
    cdEnvironment,
    cdImageConfig,
    cdMode,
    cdContainerHostname,

    -- * ContinuousParameterRange
    ContinuousParameterRange (..),
    mkContinuousParameterRange,
    cScalingType,
    cName,
    cMinValue,
    cMaxValue,

    -- * ContinuousParameterRangeSpecification
    ContinuousParameterRangeSpecification (..),
    mkContinuousParameterRangeSpecification,
    cprsMinValue,
    cprsMaxValue,

    -- * CustomImage
    CustomImage (..),
    mkCustomImage,
    ciImageVersionNumber,
    ciImageName,
    ciAppImageConfigName,

    -- * DataCaptureConfig
    DataCaptureConfig (..),
    mkDataCaptureConfig,
    dccCaptureContentTypeHeader,
    dccKMSKeyId,
    dccEnableCapture,
    dccInitialSamplingPercentage,
    dccDestinationS3URI,
    dccCaptureOptions,

    -- * DataCaptureConfigSummary
    DataCaptureConfigSummary (..),
    mkDataCaptureConfigSummary,
    dccsEnableCapture,
    dccsCaptureStatus,
    dccsCurrentSamplingPercentage,
    dccsDestinationS3URI,
    dccsKMSKeyId,

    -- * DataProcessing
    DataProcessing (..),
    mkDataProcessing,
    dpOutputFilter,
    dpJoinSource,
    dpInputFilter,

    -- * DataSource
    DataSource (..),
    mkDataSource,
    dsS3DataSource,
    dsFileSystemDataSource,

    -- * DebugHookConfig
    DebugHookConfig (..),
    mkDebugHookConfig,
    dhcLocalPath,
    dhcCollectionConfigurations,
    dhcHookParameters,
    dhcS3OutputPath,

    -- * DebugRuleConfiguration
    DebugRuleConfiguration (..),
    mkDebugRuleConfiguration,
    drcRuleParameters,
    drcS3OutputPath,
    drcLocalPath,
    drcInstanceType,
    drcVolumeSizeInGB,
    drcRuleConfigurationName,
    drcRuleEvaluatorImage,

    -- * DebugRuleEvaluationStatus
    DebugRuleEvaluationStatus (..),
    mkDebugRuleEvaluationStatus,
    dresLastModifiedTime,
    dresStatusDetails,
    dresRuleEvaluationStatus,
    dresRuleEvaluationJobARN,
    dresRuleConfigurationName,

    -- * DeployedImage
    DeployedImage (..),
    mkDeployedImage,
    diResolvedImage,
    diSpecifiedImage,
    diResolutionTime,

    -- * DesiredWeightAndCapacity
    DesiredWeightAndCapacity (..),
    mkDesiredWeightAndCapacity,
    dwacDesiredInstanceCount,
    dwacDesiredWeight,
    dwacVariantName,

    -- * DomainDetails
    DomainDetails (..),
    mkDomainDetails,
    ddCreationTime,
    ddStatus,
    ddDomainARN,
    ddURL,
    ddLastModifiedTime,
    ddDomainName,
    ddDomainId,

    -- * EndpointConfigSummary
    EndpointConfigSummary (..),
    mkEndpointConfigSummary,
    ecsEndpointConfigName,
    ecsEndpointConfigARN,
    ecsCreationTime,

    -- * EndpointInput
    EndpointInput (..),
    mkEndpointInput,
    eiS3DataDistributionType,
    eiS3InputMode,
    eiEndpointName,
    eiLocalPath,

    -- * EndpointSummary
    EndpointSummary (..),
    mkEndpointSummary,
    esEndpointName,
    esEndpointARN,
    esCreationTime,
    esLastModifiedTime,
    esEndpointStatus,

    -- * Experiment
    Experiment (..),
    mkExperiment,
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

    -- * ExperimentConfig
    ExperimentConfig (..),
    mkExperimentConfig,
    ecTrialComponentDisplayName,
    ecExperimentName,
    ecTrialName,

    -- * ExperimentSource
    ExperimentSource (..),
    mkExperimentSource,
    esSourceType,
    esSourceARN,

    -- * ExperimentSummary
    ExperimentSummary (..),
    mkExperimentSummary,
    expCreationTime,
    expLastModifiedTime,
    expExperimentName,
    expExperimentSource,
    expExperimentARN,
    expDisplayName,

    -- * FileSystemConfig
    FileSystemConfig (..),
    mkFileSystemConfig,
    fscDefaultGid,
    fscMountPath,
    fscDefaultUid,

    -- * FileSystemDataSource
    FileSystemDataSource (..),
    mkFileSystemDataSource,
    fsdsFileSystemId,
    fsdsFileSystemAccessMode,
    fsdsFileSystemType,
    fsdsDirectoryPath,

    -- * Filter
    Filter (..),
    mkFilter,
    fOperator,
    fValue,
    fName,

    -- * FinalAutoMLJobObjectiveMetric
    FinalAutoMLJobObjectiveMetric (..),
    mkFinalAutoMLJobObjectiveMetric,
    famljomType,
    famljomMetricName,
    famljomValue,

    -- * FinalHyperParameterTuningJobObjectiveMetric
    FinalHyperParameterTuningJobObjectiveMetric (..),
    mkFinalHyperParameterTuningJobObjectiveMetric,
    fhptjomType,
    fhptjomMetricName,
    fhptjomValue,

    -- * FlowDefinitionOutputConfig
    FlowDefinitionOutputConfig (..),
    mkFlowDefinitionOutputConfig,
    fdocKMSKeyId,
    fdocS3OutputPath,

    -- * FlowDefinitionSummary
    FlowDefinitionSummary (..),
    mkFlowDefinitionSummary,
    fdsFailureReason,
    fdsFlowDefinitionName,
    fdsFlowDefinitionARN,
    fdsFlowDefinitionStatus,
    fdsCreationTime,

    -- * GitConfig
    GitConfig (..),
    mkGitConfig,
    gcBranch,
    gcSecretARN,
    gcRepositoryURL,

    -- * GitConfigForUpdate
    GitConfigForUpdate (..),
    mkGitConfigForUpdate,
    gcfuSecretARN,

    -- * HumanLoopActivationConditionsConfig
    HumanLoopActivationConditionsConfig (..),
    mkHumanLoopActivationConditionsConfig,
    hlaccHumanLoopActivationConditions,

    -- * HumanLoopActivationConfig
    HumanLoopActivationConfig (..),
    mkHumanLoopActivationConfig,
    hlacHumanLoopActivationConditionsConfig,

    -- * HumanLoopConfig
    HumanLoopConfig (..),
    mkHumanLoopConfig,
    hlcTaskKeywords,
    hlcPublicWorkforceTaskPrice,
    hlcTaskTimeLimitInSeconds,
    hlcTaskAvailabilityLifetimeInSeconds,
    hlcWorkteamARN,
    hlcHumanTaskUiARN,
    hlcTaskTitle,
    hlcTaskDescription,
    hlcTaskCount,

    -- * HumanLoopRequestSource
    HumanLoopRequestSource (..),
    mkHumanLoopRequestSource,
    hlrsAWSManagedHumanLoopRequestSource,

    -- * HumanTaskConfig
    HumanTaskConfig (..),
    mkHumanTaskConfig,
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

    -- * HumanTaskUiSummary
    HumanTaskUiSummary (..),
    mkHumanTaskUiSummary,
    htusHumanTaskUiName,
    htusHumanTaskUiARN,
    htusCreationTime,

    -- * HyperParameterAlgorithmSpecification
    HyperParameterAlgorithmSpecification (..),
    mkHyperParameterAlgorithmSpecification,
    hpasAlgorithmName,
    hpasTrainingImage,
    hpasMetricDefinitions,
    hpasTrainingInputMode,

    -- * HyperParameterSpecification
    HyperParameterSpecification (..),
    mkHyperParameterSpecification,
    hpsIsTunable,
    hpsRange,
    hpsDefaultValue,
    hpsIsRequired,
    hpsDescription,
    hpsName,
    hpsType,

    -- * HyperParameterTrainingJobDefinition
    HyperParameterTrainingJobDefinition (..),
    mkHyperParameterTrainingJobDefinition,
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

    -- * HyperParameterTrainingJobSummary
    HyperParameterTrainingJobSummary (..),
    mkHyperParameterTrainingJobSummary,
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

    -- * HyperParameterTuningJobConfig
    HyperParameterTuningJobConfig (..),
    mkHyperParameterTuningJobConfig,
    hptjcTuningJobCompletionCriteria,
    hptjcParameterRanges,
    hptjcHyperParameterTuningJobObjective,
    hptjcTrainingJobEarlyStoppingType,
    hptjcStrategy,
    hptjcResourceLimits,

    -- * HyperParameterTuningJobObjective
    HyperParameterTuningJobObjective (..),
    mkHyperParameterTuningJobObjective,
    hptjoType,
    hptjoMetricName,

    -- * HyperParameterTuningJobSummary
    HyperParameterTuningJobSummary (..),
    mkHyperParameterTuningJobSummary,
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

    -- * HyperParameterTuningJobWarmStartConfig
    HyperParameterTuningJobWarmStartConfig (..),
    mkHyperParameterTuningJobWarmStartConfig,
    hptjwscParentHyperParameterTuningJobs,
    hptjwscWarmStartType,

    -- * Image
    Image (..),
    mkImage,
    iFailureReason,
    iDisplayName,
    iDescription,
    iCreationTime,
    iImageARN,
    iImageName,
    iImageStatus,
    iLastModifiedTime,

    -- * ImageConfig
    ImageConfig (..),
    mkImageConfig,
    icRepositoryAccessMode,

    -- * ImageVersion
    ImageVersion (..),
    mkImageVersion,
    ivFailureReason,
    ivCreationTime,
    ivImageARN,
    ivImageVersionARN,
    ivImageVersionStatus,
    ivLastModifiedTime,
    ivVersion,

    -- * InferenceSpecification
    InferenceSpecification (..),
    mkInferenceSpecification,
    isContainers,
    isSupportedTransformInstanceTypes,
    isSupportedRealtimeInferenceInstanceTypes,
    isSupportedContentTypes,
    isSupportedResponseMIMETypes,

    -- * InputConfig
    InputConfig (..),
    mkInputConfig,
    icS3URI,
    icDataInputConfig,
    icFramework,

    -- * IntegerParameterRange
    IntegerParameterRange (..),
    mkIntegerParameterRange,
    iprScalingType,
    iprName,
    iprMinValue,
    iprMaxValue,

    -- * IntegerParameterRangeSpecification
    IntegerParameterRangeSpecification (..),
    mkIntegerParameterRangeSpecification,
    iprsMinValue,
    iprsMaxValue,

    -- * JupyterServerAppSettings
    JupyterServerAppSettings (..),
    mkJupyterServerAppSettings,
    jsasDefaultResourceSpec,

    -- * KernelGatewayAppSettings
    KernelGatewayAppSettings (..),
    mkKernelGatewayAppSettings,
    kgasDefaultResourceSpec,
    kgasCustomImages,

    -- * KernelGatewayImageConfig
    KernelGatewayImageConfig (..),
    mkKernelGatewayImageConfig,
    kgicFileSystemConfig,
    kgicKernelSpecs,

    -- * KernelSpec
    KernelSpec (..),
    mkKernelSpec,
    ksDisplayName,
    ksName,

    -- * LabelCounters
    LabelCounters (..),
    mkLabelCounters,
    lcMachineLabeled,
    lcTotalLabeled,
    lcFailedNonRetryableError,
    lcUnlabeled,
    lcHumanLabeled,

    -- * LabelCountersForWorkteam
    LabelCountersForWorkteam (..),
    mkLabelCountersForWorkteam,
    lcfwPendingHuman,
    lcfwTotal,
    lcfwHumanLabeled,

    -- * LabelingJobAlgorithmsConfig
    LabelingJobAlgorithmsConfig (..),
    mkLabelingJobAlgorithmsConfig,
    ljacLabelingJobResourceConfig,
    ljacInitialActiveLearningModelARN,
    ljacLabelingJobAlgorithmSpecificationARN,

    -- * LabelingJobDataAttributes
    LabelingJobDataAttributes (..),
    mkLabelingJobDataAttributes,
    ljdaContentClassifiers,

    -- * LabelingJobDataSource
    LabelingJobDataSource (..),
    mkLabelingJobDataSource,
    ljdsS3DataSource,
    ljdsSNSDataSource,

    -- * LabelingJobForWorkteamSummary
    LabelingJobForWorkteamSummary (..),
    mkLabelingJobForWorkteamSummary,
    ljfwsNumberOfHumanWorkersPerDataObject,
    ljfwsLabelCounters,
    ljfwsLabelingJobName,
    ljfwsJobReferenceCode,
    ljfwsWorkRequesterAccountId,
    ljfwsCreationTime,

    -- * LabelingJobInputConfig
    LabelingJobInputConfig (..),
    mkLabelingJobInputConfig,
    ljicDataAttributes,
    ljicDataSource,

    -- * LabelingJobOutput
    LabelingJobOutput (..),
    mkLabelingJobOutput,
    ljoFinalActiveLearningModelARN,
    ljoOutputDatasetS3URI,

    -- * LabelingJobOutputConfig
    LabelingJobOutputConfig (..),
    mkLabelingJobOutputConfig,
    ljocSNSTopicARN,
    ljocKMSKeyId,
    ljocS3OutputPath,

    -- * LabelingJobResourceConfig
    LabelingJobResourceConfig (..),
    mkLabelingJobResourceConfig,
    ljrcVolumeKMSKeyId,

    -- * LabelingJobS3DataSource
    LabelingJobS3DataSource (..),
    mkLabelingJobS3DataSource,
    ljsdsManifestS3URI,

    -- * LabelingJobSNSDataSource
    LabelingJobSNSDataSource (..),
    mkLabelingJobSNSDataSource,
    ljsdsSNSTopicARN,

    -- * LabelingJobStoppingConditions
    LabelingJobStoppingConditions (..),
    mkLabelingJobStoppingConditions,
    ljscMaxHumanLabeledObjectCount,
    ljscMaxPercentageOfInputDatasetLabeled,

    -- * LabelingJobSummary
    LabelingJobSummary (..),
    mkLabelingJobSummary,
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

    -- * MemberDefinition
    MemberDefinition (..),
    mkMemberDefinition,
    mdOidcMemberDefinition,
    mdCognitoMemberDefinition,

    -- * MetricData
    MetricData (..),
    mkMetricData,
    mdMetricName,
    mdValue,
    mdTimestamp,

    -- * MetricDefinition
    MetricDefinition (..),
    mkMetricDefinition,
    mdName,
    mdRegex,

    -- * ModelArtifacts
    ModelArtifacts (..),
    mkModelArtifacts,
    maS3ModelArtifacts,

    -- * ModelClientConfig
    ModelClientConfig (..),
    mkModelClientConfig,
    mccInvocationsTimeoutInSeconds,
    mccInvocationsMaxRetries,

    -- * ModelPackageContainerDefinition
    ModelPackageContainerDefinition (..),
    mkModelPackageContainerDefinition,
    mpcdModelDataURL,
    mpcdImageDigest,
    mpcdContainerHostname,
    mpcdProductId,
    mpcdImage,

    -- * ModelPackageStatusDetails
    ModelPackageStatusDetails (..),
    mkModelPackageStatusDetails,
    mpsdImageScanStatuses,
    mpsdValidationStatuses,

    -- * ModelPackageStatusItem
    ModelPackageStatusItem (..),
    mkModelPackageStatusItem,
    mpsiFailureReason,
    mpsiName,
    mpsiStatus,

    -- * ModelPackageSummary
    ModelPackageSummary (..),
    mkModelPackageSummary,
    mpsModelPackageDescription,
    mpsModelPackageName,
    mpsModelPackageARN,
    mpsCreationTime,
    mpsModelPackageStatus,

    -- * ModelPackageValidationProfile
    ModelPackageValidationProfile (..),
    mkModelPackageValidationProfile,
    mpvpProfileName,
    mpvpTransformJobDefinition,

    -- * ModelPackageValidationSpecification
    ModelPackageValidationSpecification (..),
    mkModelPackageValidationSpecification,
    mpvsValidationRole,
    mpvsValidationProfiles,

    -- * ModelSummary
    ModelSummary (..),
    mkModelSummary,
    msModelName,
    msModelARN,
    msCreationTime,

    -- * MonitoringAppSpecification
    MonitoringAppSpecification (..),
    mkMonitoringAppSpecification,
    masContainerArguments,
    masRecordPreprocessorSourceURI,
    masContainerEntrypoint,
    masPostAnalyticsProcessorSourceURI,
    masImageURI,

    -- * MonitoringBaselineConfig
    MonitoringBaselineConfig (..),
    mkMonitoringBaselineConfig,
    mbcConstraintsResource,
    mbcStatisticsResource,

    -- * MonitoringClusterConfig
    MonitoringClusterConfig (..),
    mkMonitoringClusterConfig,
    mccVolumeKMSKeyId,
    mccInstanceCount,
    mccInstanceType,
    mccVolumeSizeInGB,

    -- * MonitoringConstraintsResource
    MonitoringConstraintsResource (..),
    mkMonitoringConstraintsResource,
    mcrS3URI,

    -- * MonitoringExecutionSummary
    MonitoringExecutionSummary (..),
    mkMonitoringExecutionSummary,
    mesFailureReason,
    mesEndpointName,
    mesProcessingJobARN,
    mesMonitoringScheduleName,
    mesScheduledTime,
    mesCreationTime,
    mesLastModifiedTime,
    mesMonitoringExecutionStatus,

    -- * MonitoringInput
    MonitoringInput (..),
    mkMonitoringInput,
    miEndpointInput,

    -- * MonitoringJobDefinition
    MonitoringJobDefinition (..),
    mkMonitoringJobDefinition,
    mjdEnvironment,
    mjdStoppingCondition,
    mjdNetworkConfig,
    mjdBaselineConfig,
    mjdMonitoringInputs,
    mjdMonitoringOutputConfig,
    mjdMonitoringResources,
    mjdMonitoringAppSpecification,
    mjdRoleARN,

    -- * MonitoringOutput
    MonitoringOutput (..),
    mkMonitoringOutput,
    moS3Output,

    -- * MonitoringOutputConfig
    MonitoringOutputConfig (..),
    mkMonitoringOutputConfig,
    mocKMSKeyId,
    mocMonitoringOutputs,

    -- * MonitoringResources
    MonitoringResources (..),
    mkMonitoringResources,
    mrClusterConfig,

    -- * MonitoringS3Output
    MonitoringS3Output (..),
    mkMonitoringS3Output,
    msoS3UploadMode,
    msoS3URI,
    msoLocalPath,

    -- * MonitoringScheduleConfig
    MonitoringScheduleConfig (..),
    mkMonitoringScheduleConfig,
    mscScheduleConfig,
    mscMonitoringJobDefinition,

    -- * MonitoringScheduleSummary
    MonitoringScheduleSummary (..),
    mkMonitoringScheduleSummary,
    mssEndpointName,
    mssMonitoringScheduleName,
    mssMonitoringScheduleARN,
    mssCreationTime,
    mssLastModifiedTime,
    mssMonitoringScheduleStatus,

    -- * MonitoringStatisticsResource
    MonitoringStatisticsResource (..),
    mkMonitoringStatisticsResource,
    msrS3URI,

    -- * MonitoringStoppingCondition
    MonitoringStoppingCondition (..),
    mkMonitoringStoppingCondition,
    mscMaxRuntimeInSeconds,

    -- * NestedFilters
    NestedFilters (..),
    mkNestedFilters,
    nfNestedPropertyName,
    nfFilters,

    -- * NetworkConfig
    NetworkConfig (..),
    mkNetworkConfig,
    ncEnableNetworkIsolation,
    ncVPCConfig,
    ncEnableInterContainerTrafficEncryption,

    -- * NotebookInstanceLifecycleConfigSummary
    NotebookInstanceLifecycleConfigSummary (..),
    mkNotebookInstanceLifecycleConfigSummary,
    nilcsCreationTime,
    nilcsLastModifiedTime,
    nilcsNotebookInstanceLifecycleConfigName,
    nilcsNotebookInstanceLifecycleConfigARN,

    -- * NotebookInstanceLifecycleHook
    NotebookInstanceLifecycleHook (..),
    mkNotebookInstanceLifecycleHook,
    nilhContent,

    -- * NotebookInstanceSummary
    NotebookInstanceSummary (..),
    mkNotebookInstanceSummary,
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

    -- * NotificationConfiguration
    NotificationConfiguration (..),
    mkNotificationConfiguration,
    ncNotificationTopicARN,

    -- * ObjectiveStatusCounters
    ObjectiveStatusCounters (..),
    mkObjectiveStatusCounters,
    oscPending,
    oscSucceeded,
    oscFailed,

    -- * OidcConfig
    OidcConfig (..),
    mkOidcConfig,
    ocClientId,
    ocClientSecret,
    ocIssuer,
    ocAuthorizationEndpoint,
    ocTokenEndpoint,
    ocUserInfoEndpoint,
    ocLogoutEndpoint,
    ocJwksURI,

    -- * OidcConfigForResponse
    OidcConfigForResponse (..),
    mkOidcConfigForResponse,
    ocfClientId,
    ocfJwksURI,
    ocfUserInfoEndpoint,
    ocfAuthorizationEndpoint,
    ocfTokenEndpoint,
    ocfIssuer,
    ocfLogoutEndpoint,

    -- * OidcMemberDefinition
    OidcMemberDefinition (..),
    mkOidcMemberDefinition,
    omdGroups,

    -- * OutputConfig
    OutputConfig (..),
    mkOutputConfig,
    ocTargetPlatform,
    ocCompilerOptions,
    ocTargetDevice,
    ocS3OutputLocation,

    -- * OutputDataConfig
    OutputDataConfig (..),
    mkOutputDataConfig,
    odcKMSKeyId,
    odcS3OutputPath,

    -- * ParameterRange
    ParameterRange (..),
    mkParameterRange,
    prCategoricalParameterRangeSpecification,
    prIntegerParameterRangeSpecification,
    prContinuousParameterRangeSpecification,

    -- * ParameterRanges
    ParameterRanges (..),
    mkParameterRanges,
    prCategoricalParameterRanges,
    prIntegerParameterRanges,
    prContinuousParameterRanges,

    -- * Parent
    Parent (..),
    mkParent,
    pExperimentName,
    pTrialName,

    -- * ParentHyperParameterTuningJob
    ParentHyperParameterTuningJob (..),
    mkParentHyperParameterTuningJob,
    phptjHyperParameterTuningJobName,

    -- * ProcessingClusterConfig
    ProcessingClusterConfig (..),
    mkProcessingClusterConfig,
    pccVolumeKMSKeyId,
    pccInstanceCount,
    pccInstanceType,
    pccVolumeSizeInGB,

    -- * ProcessingInput
    ProcessingInput (..),
    mkProcessingInput,
    piInputName,
    piS3Input,

    -- * ProcessingJob
    ProcessingJob (..),
    mkProcessingJob,
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

    -- * ProcessingJobSummary
    ProcessingJobSummary (..),
    mkProcessingJobSummary,
    pjsFailureReason,
    pjsLastModifiedTime,
    pjsExitMessage,
    pjsProcessingEndTime,
    pjsProcessingJobName,
    pjsProcessingJobARN,
    pjsCreationTime,
    pjsProcessingJobStatus,

    -- * ProcessingOutput
    ProcessingOutput (..),
    mkProcessingOutput,
    poOutputName,
    poS3Output,

    -- * ProcessingOutputConfig
    ProcessingOutputConfig (..),
    mkProcessingOutputConfig,
    pocKMSKeyId,
    pocOutputs,

    -- * ProcessingResources
    ProcessingResources (..),
    mkProcessingResources,
    prClusterConfig,

    -- * ProcessingS3Input
    ProcessingS3Input (..),
    mkProcessingS3Input,
    psiS3DataDistributionType,
    psiS3CompressionType,
    psiS3URI,
    psiLocalPath,
    psiS3DataType,
    psiS3InputMode,

    -- * ProcessingS3Output
    ProcessingS3Output (..),
    mkProcessingS3Output,
    psoS3URI,
    psoLocalPath,
    psoS3UploadMode,

    -- * ProcessingStoppingCondition
    ProcessingStoppingCondition (..),
    mkProcessingStoppingCondition,
    pscMaxRuntimeInSeconds,

    -- * ProductionVariant
    ProductionVariant (..),
    mkProductionVariant,
    pvAcceleratorType,
    pvInitialVariantWeight,
    pvVariantName,
    pvModelName,
    pvInitialInstanceCount,
    pvInstanceType,

    -- * ProductionVariantSummary
    ProductionVariantSummary (..),
    mkProductionVariantSummary,
    pvsDesiredInstanceCount,
    pvsDesiredWeight,
    pvsCurrentWeight,
    pvsCurrentInstanceCount,
    pvsDeployedImages,
    pvsVariantName,

    -- * PropertyNameQuery
    PropertyNameQuery (..),
    mkPropertyNameQuery,
    pnqPropertyNameHint,

    -- * PropertyNameSuggestion
    PropertyNameSuggestion (..),
    mkPropertyNameSuggestion,
    pnsPropertyName,

    -- * PublicWorkforceTaskPrice
    PublicWorkforceTaskPrice (..),
    mkPublicWorkforceTaskPrice,
    pwtpAmountInUsd,

    -- * RenderableTask
    RenderableTask (..),
    mkRenderableTask,
    rtInput,

    -- * RenderingError
    RenderingError (..),
    mkRenderingError,
    reCode,
    reMessage,

    -- * ResolvedAttributes
    ResolvedAttributes (..),
    mkResolvedAttributes,
    raProblemType,
    raAutoMLJobObjective,
    raCompletionCriteria,

    -- * ResourceConfig
    ResourceConfig (..),
    mkResourceConfig,
    rcVolumeKMSKeyId,
    rcInstanceType,
    rcInstanceCount,
    rcVolumeSizeInGB,

    -- * ResourceLimits
    ResourceLimits (..),
    mkResourceLimits,
    rlMaxNumberOfTrainingJobs,
    rlMaxParallelTrainingJobs,

    -- * ResourceSpec
    ResourceSpec (..),
    mkResourceSpec,
    rsInstanceType,
    rsSageMakerImageARN,
    rsSageMakerImageVersionARN,

    -- * RetentionPolicy
    RetentionPolicy (..),
    mkRetentionPolicy,
    rpHomeEfsFileSystem,

    -- * S3DataSource
    S3DataSource (..),
    mkS3DataSource,
    sdsS3DataDistributionType,
    sdsAttributeNames,
    sdsS3DataType,
    sdsS3URI,

    -- * ScheduleConfig
    ScheduleConfig (..),
    mkScheduleConfig,
    scScheduleExpression,

    -- * SearchExpression
    SearchExpression (..),
    mkSearchExpression,
    seSubExpressions,
    seOperator,
    seFilters,
    seNestedFilters,

    -- * SearchRecord
    SearchRecord (..),
    mkSearchRecord,
    srTrainingJob,
    srTrial,
    srTrialComponent,
    srExperiment,

    -- * SecondaryStatusTransition
    SecondaryStatusTransition (..),
    mkSecondaryStatusTransition,
    sstStatusMessage,
    sstEndTime,
    sstStatus,
    sstStartTime,

    -- * SharingSettings
    SharingSettings (..),
    mkSharingSettings,
    ssS3KMSKeyId,
    ssS3OutputPath,
    ssNotebookOutputOption,

    -- * ShuffleConfig
    ShuffleConfig (..),
    mkShuffleConfig,
    scSeed,

    -- * SourceAlgorithm
    SourceAlgorithm (..),
    mkSourceAlgorithm,
    saModelDataURL,
    saAlgorithmName,

    -- * SourceAlgorithmSpecification
    SourceAlgorithmSpecification (..),
    mkSourceAlgorithmSpecification,
    sasSourceAlgorithms,

    -- * SourceIPConfig
    SourceIPConfig (..),
    mkSourceIPConfig,
    sicCidrs,

    -- * StoppingCondition
    StoppingCondition (..),
    mkStoppingCondition,
    scMaxWaitTimeInSeconds,
    scMaxRuntimeInSeconds,

    -- * SubscribedWorkteam
    SubscribedWorkteam (..),
    mkSubscribedWorkteam,
    swMarketplaceTitle,
    swSellerName,
    swListingId,
    swMarketplaceDescription,
    swWorkteamARN,

    -- * SuggestionQuery
    SuggestionQuery (..),
    mkSuggestionQuery,
    sqPropertyNameQuery,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * TargetPlatform
    TargetPlatform (..),
    mkTargetPlatform,
    tpAccelerator,
    tpOS,
    tpArch,

    -- * TensorBoardAppSettings
    TensorBoardAppSettings (..),
    mkTensorBoardAppSettings,
    tbasDefaultResourceSpec,

    -- * TensorBoardOutputConfig
    TensorBoardOutputConfig (..),
    mkTensorBoardOutputConfig,
    tbocLocalPath,
    tbocS3OutputPath,

    -- * TrainingJob
    TrainingJob (..),
    mkTrainingJob,
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

    -- * TrainingJobDefinition
    TrainingJobDefinition (..),
    mkTrainingJobDefinition,
    tjdHyperParameters,
    tjdTrainingInputMode,
    tjdInputDataConfig,
    tjdOutputDataConfig,
    tjdResourceConfig,
    tjdStoppingCondition,

    -- * TrainingJobStatusCounters
    TrainingJobStatusCounters (..),
    mkTrainingJobStatusCounters,
    tjscStopped,
    tjscRetryableError,
    tjscInProgress,
    tjscNonRetryableError,
    tjscCompleted,

    -- * TrainingJobSummary
    TrainingJobSummary (..),
    mkTrainingJobSummary,
    tjsjTrainingEndTime,
    tjsjLastModifiedTime,
    tjsjTrainingJobName,
    tjsjTrainingJobARN,
    tjsjCreationTime,
    tjsjTrainingJobStatus,

    -- * TrainingSpecification
    TrainingSpecification (..),
    mkTrainingSpecification,
    tsTrainingImageDigest,
    tsSupportsDistributedTraining,
    tsSupportedHyperParameters,
    tsSupportedTuningJobObjectiveMetrics,
    tsMetricDefinitions,
    tsTrainingImage,
    tsSupportedTrainingInstanceTypes,
    tsTrainingChannels,

    -- * TransformDataSource
    TransformDataSource (..),
    mkTransformDataSource,
    tdsS3DataSource,

    -- * TransformInput
    TransformInput (..),
    mkTransformInput,
    tiSplitType,
    tiCompressionType,
    tiContentType,
    tiDataSource,

    -- * TransformJob
    TransformJob (..),
    mkTransformJob,
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

    -- * TransformJobDefinition
    TransformJobDefinition (..),
    mkTransformJobDefinition,
    tjdBatchStrategy,
    tjdMaxPayloadInMB,
    tjdEnvironment,
    tjdMaxConcurrentTransforms,
    tjdTransformInput,
    tjdTransformOutput,
    tjdTransformResources,

    -- * TransformJobSummary
    TransformJobSummary (..),
    mkTransformJobSummary,
    tjsFailureReason,
    tjsLastModifiedTime,
    tjsTransformEndTime,
    tjsTransformJobName,
    tjsTransformJobARN,
    tjsCreationTime,
    tjsTransformJobStatus,

    -- * TransformOutput
    TransformOutput (..),
    mkTransformOutput,
    toAssembleWith,
    toAccept,
    toKMSKeyId,
    toS3OutputPath,

    -- * TransformResources
    TransformResources (..),
    mkTransformResources,
    trVolumeKMSKeyId,
    trInstanceType,
    trInstanceCount,

    -- * TransformS3DataSource
    TransformS3DataSource (..),
    mkTransformS3DataSource,
    tsdsS3DataType,
    tsdsS3URI,

    -- * Trial
    Trial (..),
    mkTrial,
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

    -- * TrialComponent
    TrialComponent (..),
    mkTrialComponent,
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

    -- * TrialComponentArtifact
    TrialComponentArtifact (..),
    mkTrialComponentArtifact,
    tcaMediaType,
    tcaValue,

    -- * TrialComponentMetricSummary
    TrialComponentMetricSummary (..),
    mkTrialComponentMetricSummary,
    tcmsMax,
    tcmsSourceARN,
    tcmsAvg,
    tcmsCount,
    tcmsMetricName,
    tcmsStdDev,
    tcmsMin,
    tcmsLast,
    tcmsTimeStamp,

    -- * TrialComponentParameterValue
    TrialComponentParameterValue (..),
    mkTrialComponentParameterValue,
    tcpvNumberValue,
    tcpvStringValue,

    -- * TrialComponentSimpleSummary
    TrialComponentSimpleSummary (..),
    mkTrialComponentSimpleSummary,
    tcssCreationTime,
    tcssCreatedBy,
    tcssTrialComponentName,
    tcssTrialComponentARN,
    tcssTrialComponentSource,

    -- * TrialComponentSource
    TrialComponentSource (..),
    mkTrialComponentSource,
    tcsSourceType,
    tcsSourceARN,

    -- * TrialComponentSourceDetail
    TrialComponentSourceDetail (..),
    mkTrialComponentSourceDetail,
    tcsdTrainingJob,
    tcsdSourceARN,
    tcsdProcessingJob,
    tcsdTransformJob,

    -- * TrialComponentStatus
    TrialComponentStatus (..),
    mkTrialComponentStatus,
    tcsPrimaryStatus,
    tcsMessage,

    -- * TrialComponentSummary
    TrialComponentSummary (..),
    mkTrialComponentSummary,
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

    -- * TrialSource
    TrialSource (..),
    mkTrialSource,
    tsSourceType,
    tsSourceARN,

    -- * TrialSummary
    TrialSummary (..),
    mkTrialSummary,
    tsCreationTime,
    tsTrialARN,
    tsLastModifiedTime,
    tsTrialSource,
    tsDisplayName,
    tsTrialName,

    -- * TuningJobCompletionCriteria
    TuningJobCompletionCriteria (..),
    mkTuningJobCompletionCriteria,
    tjccTargetObjectiveMetricValue,

    -- * USD
    USD (..),
    mkUSD,
    usdCents,
    usdDollars,
    usdTenthFractionsOfACent,

    -- * UiConfig
    UiConfig (..),
    mkUiConfig,
    ucUiTemplateS3URI,
    ucHumanTaskUiARN,

    -- * UiTemplate
    UiTemplate (..),
    mkUiTemplate,
    utContent,

    -- * UiTemplateInfo
    UiTemplateInfo (..),
    mkUiTemplateInfo,
    utiURL,
    utiContentSha256,

    -- * UserContext
    UserContext (..),
    mkUserContext,
    ucUserProfileName,
    ucUserProfileARN,
    ucDomainId,

    -- * UserProfileDetails
    UserProfileDetails (..),
    mkUserProfileDetails,
    updCreationTime,
    updStatus,
    updUserProfileName,
    updLastModifiedTime,
    updDomainId,

    -- * UserSettings
    UserSettings (..),
    mkUserSettings,
    usTensorBoardAppSettings,
    usKernelGatewayAppSettings,
    usSecurityGroups,
    usJupyterServerAppSettings,
    usSharingSettings,
    usExecutionRole,

    -- * VPCConfig
    VPCConfig (..),
    mkVPCConfig,
    vcSecurityGroupIds,
    vcSubnets,

    -- * VariantProperty
    VariantProperty (..),
    mkVariantProperty,
    vpVariantPropertyType,

    -- * Workforce
    Workforce (..),
    mkWorkforce,
    wSubDomain,
    wCreateDate,
    wSourceIPConfig,
    wCognitoConfig,
    wLastUpdatedDate,
    wOidcConfig,
    wWorkforceName,
    wWorkforceARN,

    -- * Workteam
    Workteam (..),
    mkWorkteam,
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.AWSManagedHumanLoopRequestSource
import Network.AWS.SageMaker.Types.AlgorithmSortBy
import Network.AWS.SageMaker.Types.AlgorithmSpecification
import Network.AWS.SageMaker.Types.AlgorithmStatus
import Network.AWS.SageMaker.Types.AlgorithmStatusDetails
import Network.AWS.SageMaker.Types.AlgorithmStatusItem
import Network.AWS.SageMaker.Types.AlgorithmSummary
import Network.AWS.SageMaker.Types.AlgorithmValidationProfile
import Network.AWS.SageMaker.Types.AlgorithmValidationSpecification
import Network.AWS.SageMaker.Types.AnnotationConsolidationConfig
import Network.AWS.SageMaker.Types.AppDetails
import Network.AWS.SageMaker.Types.AppImageConfigDetails
import Network.AWS.SageMaker.Types.AppImageConfigSortKey
import Network.AWS.SageMaker.Types.AppInstanceType
import Network.AWS.SageMaker.Types.AppNetworkAccessType
import Network.AWS.SageMaker.Types.AppSortKey
import Network.AWS.SageMaker.Types.AppSpecification
import Network.AWS.SageMaker.Types.AppStatus
import Network.AWS.SageMaker.Types.AppType
import Network.AWS.SageMaker.Types.AssemblyType
import Network.AWS.SageMaker.Types.AuthMode
import Network.AWS.SageMaker.Types.AutoMLCandidate
import Network.AWS.SageMaker.Types.AutoMLCandidateStep
import Network.AWS.SageMaker.Types.AutoMLChannel
import Network.AWS.SageMaker.Types.AutoMLContainerDefinition
import Network.AWS.SageMaker.Types.AutoMLDataSource
import Network.AWS.SageMaker.Types.AutoMLJobArtifacts
import Network.AWS.SageMaker.Types.AutoMLJobCompletionCriteria
import Network.AWS.SageMaker.Types.AutoMLJobConfig
import Network.AWS.SageMaker.Types.AutoMLJobObjective
import Network.AWS.SageMaker.Types.AutoMLJobObjectiveType
import Network.AWS.SageMaker.Types.AutoMLJobSecondaryStatus
import Network.AWS.SageMaker.Types.AutoMLJobStatus
import Network.AWS.SageMaker.Types.AutoMLJobSummary
import Network.AWS.SageMaker.Types.AutoMLMetricEnum
import Network.AWS.SageMaker.Types.AutoMLOutputDataConfig
import Network.AWS.SageMaker.Types.AutoMLS3DataSource
import Network.AWS.SageMaker.Types.AutoMLS3DataType
import Network.AWS.SageMaker.Types.AutoMLSecurityConfig
import Network.AWS.SageMaker.Types.AutoMLSortBy
import Network.AWS.SageMaker.Types.AutoMLSortOrder
import Network.AWS.SageMaker.Types.BatchStrategy
import Network.AWS.SageMaker.Types.BooleanOperator
import Network.AWS.SageMaker.Types.CandidateSortBy
import Network.AWS.SageMaker.Types.CandidateStatus
import Network.AWS.SageMaker.Types.CandidateStepType
import Network.AWS.SageMaker.Types.CaptureContentTypeHeader
import Network.AWS.SageMaker.Types.CaptureMode
import Network.AWS.SageMaker.Types.CaptureOption
import Network.AWS.SageMaker.Types.CaptureStatus
import Network.AWS.SageMaker.Types.CategoricalParameterRange
import Network.AWS.SageMaker.Types.CategoricalParameterRangeSpecification
import Network.AWS.SageMaker.Types.Channel
import Network.AWS.SageMaker.Types.ChannelSpecification
import Network.AWS.SageMaker.Types.CheckpointConfig
import Network.AWS.SageMaker.Types.CodeRepositorySortBy
import Network.AWS.SageMaker.Types.CodeRepositorySortOrder
import Network.AWS.SageMaker.Types.CodeRepositorySummary
import Network.AWS.SageMaker.Types.CognitoConfig
import Network.AWS.SageMaker.Types.CognitoMemberDefinition
import Network.AWS.SageMaker.Types.CollectionConfiguration
import Network.AWS.SageMaker.Types.CompilationJobStatus
import Network.AWS.SageMaker.Types.CompilationJobSummary
import Network.AWS.SageMaker.Types.CompressionType
import Network.AWS.SageMaker.Types.ContainerDefinition
import Network.AWS.SageMaker.Types.ContainerMode
import Network.AWS.SageMaker.Types.ContentClassifier
import Network.AWS.SageMaker.Types.ContinuousParameterRange
import Network.AWS.SageMaker.Types.ContinuousParameterRangeSpecification
import Network.AWS.SageMaker.Types.CustomImage
import Network.AWS.SageMaker.Types.DataCaptureConfig
import Network.AWS.SageMaker.Types.DataCaptureConfigSummary
import Network.AWS.SageMaker.Types.DataProcessing
import Network.AWS.SageMaker.Types.DataSource
import Network.AWS.SageMaker.Types.DebugHookConfig
import Network.AWS.SageMaker.Types.DebugRuleConfiguration
import Network.AWS.SageMaker.Types.DebugRuleEvaluationStatus
import Network.AWS.SageMaker.Types.DeployedImage
import Network.AWS.SageMaker.Types.DesiredWeightAndCapacity
import Network.AWS.SageMaker.Types.DetailedAlgorithmStatus
import Network.AWS.SageMaker.Types.DetailedModelPackageStatus
import Network.AWS.SageMaker.Types.DirectInternetAccess
import Network.AWS.SageMaker.Types.DomainDetails
import Network.AWS.SageMaker.Types.DomainStatus
import Network.AWS.SageMaker.Types.EndpointConfigSortKey
import Network.AWS.SageMaker.Types.EndpointConfigSummary
import Network.AWS.SageMaker.Types.EndpointInput
import Network.AWS.SageMaker.Types.EndpointSortKey
import Network.AWS.SageMaker.Types.EndpointStatus
import Network.AWS.SageMaker.Types.EndpointSummary
import Network.AWS.SageMaker.Types.ExecutionStatus
import Network.AWS.SageMaker.Types.Experiment
import Network.AWS.SageMaker.Types.ExperimentConfig
import Network.AWS.SageMaker.Types.ExperimentSource
import Network.AWS.SageMaker.Types.ExperimentSummary
import Network.AWS.SageMaker.Types.FileSystemAccessMode
import Network.AWS.SageMaker.Types.FileSystemConfig
import Network.AWS.SageMaker.Types.FileSystemDataSource
import Network.AWS.SageMaker.Types.FileSystemType
import Network.AWS.SageMaker.Types.Filter
import Network.AWS.SageMaker.Types.FinalAutoMLJobObjectiveMetric
import Network.AWS.SageMaker.Types.FinalHyperParameterTuningJobObjectiveMetric
import Network.AWS.SageMaker.Types.FlowDefinitionOutputConfig
import Network.AWS.SageMaker.Types.FlowDefinitionStatus
import Network.AWS.SageMaker.Types.FlowDefinitionSummary
import Network.AWS.SageMaker.Types.Framework
import Network.AWS.SageMaker.Types.GitConfig
import Network.AWS.SageMaker.Types.GitConfigForUpdate
import Network.AWS.SageMaker.Types.HumanLoopActivationConditionsConfig
import Network.AWS.SageMaker.Types.HumanLoopActivationConfig
import Network.AWS.SageMaker.Types.HumanLoopConfig
import Network.AWS.SageMaker.Types.HumanLoopRequestSource
import Network.AWS.SageMaker.Types.HumanTaskConfig
import Network.AWS.SageMaker.Types.HumanTaskUiStatus
import Network.AWS.SageMaker.Types.HumanTaskUiSummary
import Network.AWS.SageMaker.Types.HyperParameterAlgorithmSpecification
import Network.AWS.SageMaker.Types.HyperParameterScalingType
import Network.AWS.SageMaker.Types.HyperParameterSpecification
import Network.AWS.SageMaker.Types.HyperParameterTrainingJobDefinition
import Network.AWS.SageMaker.Types.HyperParameterTrainingJobSummary
import Network.AWS.SageMaker.Types.HyperParameterTuningJobConfig
import Network.AWS.SageMaker.Types.HyperParameterTuningJobObjective
import Network.AWS.SageMaker.Types.HyperParameterTuningJobObjectiveType
import Network.AWS.SageMaker.Types.HyperParameterTuningJobSortByOptions
import Network.AWS.SageMaker.Types.HyperParameterTuningJobStatus
import Network.AWS.SageMaker.Types.HyperParameterTuningJobStrategyType
import Network.AWS.SageMaker.Types.HyperParameterTuningJobSummary
import Network.AWS.SageMaker.Types.HyperParameterTuningJobWarmStartConfig
import Network.AWS.SageMaker.Types.HyperParameterTuningJobWarmStartType
import Network.AWS.SageMaker.Types.Image
import Network.AWS.SageMaker.Types.ImageConfig
import Network.AWS.SageMaker.Types.ImageSortBy
import Network.AWS.SageMaker.Types.ImageSortOrder
import Network.AWS.SageMaker.Types.ImageStatus
import Network.AWS.SageMaker.Types.ImageVersion
import Network.AWS.SageMaker.Types.ImageVersionSortBy
import Network.AWS.SageMaker.Types.ImageVersionSortOrder
import Network.AWS.SageMaker.Types.ImageVersionStatus
import Network.AWS.SageMaker.Types.InferenceSpecification
import Network.AWS.SageMaker.Types.InputConfig
import Network.AWS.SageMaker.Types.InstanceType
import Network.AWS.SageMaker.Types.IntegerParameterRange
import Network.AWS.SageMaker.Types.IntegerParameterRangeSpecification
import Network.AWS.SageMaker.Types.JoinSource
import Network.AWS.SageMaker.Types.JupyterServerAppSettings
import Network.AWS.SageMaker.Types.KernelGatewayAppSettings
import Network.AWS.SageMaker.Types.KernelGatewayImageConfig
import Network.AWS.SageMaker.Types.KernelSpec
import Network.AWS.SageMaker.Types.LabelCounters
import Network.AWS.SageMaker.Types.LabelCountersForWorkteam
import Network.AWS.SageMaker.Types.LabelingJobAlgorithmsConfig
import Network.AWS.SageMaker.Types.LabelingJobDataAttributes
import Network.AWS.SageMaker.Types.LabelingJobDataSource
import Network.AWS.SageMaker.Types.LabelingJobForWorkteamSummary
import Network.AWS.SageMaker.Types.LabelingJobInputConfig
import Network.AWS.SageMaker.Types.LabelingJobOutput
import Network.AWS.SageMaker.Types.LabelingJobOutputConfig
import Network.AWS.SageMaker.Types.LabelingJobResourceConfig
import Network.AWS.SageMaker.Types.LabelingJobS3DataSource
import Network.AWS.SageMaker.Types.LabelingJobSNSDataSource
import Network.AWS.SageMaker.Types.LabelingJobStatus
import Network.AWS.SageMaker.Types.LabelingJobStoppingConditions
import Network.AWS.SageMaker.Types.LabelingJobSummary
import Network.AWS.SageMaker.Types.ListCompilationJobsSortBy
import Network.AWS.SageMaker.Types.ListLabelingJobsForWorkteamSortByOptions
import Network.AWS.SageMaker.Types.ListWorkforcesSortByOptions
import Network.AWS.SageMaker.Types.ListWorkteamsSortByOptions
import Network.AWS.SageMaker.Types.MemberDefinition
import Network.AWS.SageMaker.Types.MetricData
import Network.AWS.SageMaker.Types.MetricDefinition
import Network.AWS.SageMaker.Types.ModelArtifacts
import Network.AWS.SageMaker.Types.ModelClientConfig
import Network.AWS.SageMaker.Types.ModelPackageContainerDefinition
import Network.AWS.SageMaker.Types.ModelPackageSortBy
import Network.AWS.SageMaker.Types.ModelPackageStatus
import Network.AWS.SageMaker.Types.ModelPackageStatusDetails
import Network.AWS.SageMaker.Types.ModelPackageStatusItem
import Network.AWS.SageMaker.Types.ModelPackageSummary
import Network.AWS.SageMaker.Types.ModelPackageValidationProfile
import Network.AWS.SageMaker.Types.ModelPackageValidationSpecification
import Network.AWS.SageMaker.Types.ModelSortKey
import Network.AWS.SageMaker.Types.ModelSummary
import Network.AWS.SageMaker.Types.MonitoringAppSpecification
import Network.AWS.SageMaker.Types.MonitoringBaselineConfig
import Network.AWS.SageMaker.Types.MonitoringClusterConfig
import Network.AWS.SageMaker.Types.MonitoringConstraintsResource
import Network.AWS.SageMaker.Types.MonitoringExecutionSortKey
import Network.AWS.SageMaker.Types.MonitoringExecutionSummary
import Network.AWS.SageMaker.Types.MonitoringInput
import Network.AWS.SageMaker.Types.MonitoringJobDefinition
import Network.AWS.SageMaker.Types.MonitoringOutput
import Network.AWS.SageMaker.Types.MonitoringOutputConfig
import Network.AWS.SageMaker.Types.MonitoringResources
import Network.AWS.SageMaker.Types.MonitoringS3Output
import Network.AWS.SageMaker.Types.MonitoringScheduleConfig
import Network.AWS.SageMaker.Types.MonitoringScheduleSortKey
import Network.AWS.SageMaker.Types.MonitoringScheduleSummary
import Network.AWS.SageMaker.Types.MonitoringStatisticsResource
import Network.AWS.SageMaker.Types.MonitoringStoppingCondition
import Network.AWS.SageMaker.Types.NestedFilters
import Network.AWS.SageMaker.Types.NetworkConfig
import Network.AWS.SageMaker.Types.NotebookInstanceAcceleratorType
import Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSortKey
import Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSortOrder
import Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSummary
import Network.AWS.SageMaker.Types.NotebookInstanceLifecycleHook
import Network.AWS.SageMaker.Types.NotebookInstanceSortKey
import Network.AWS.SageMaker.Types.NotebookInstanceSortOrder
import Network.AWS.SageMaker.Types.NotebookInstanceStatus
import Network.AWS.SageMaker.Types.NotebookInstanceSummary
import Network.AWS.SageMaker.Types.NotebookOutputOption
import Network.AWS.SageMaker.Types.NotificationConfiguration
import Network.AWS.SageMaker.Types.ObjectiveStatus
import Network.AWS.SageMaker.Types.ObjectiveStatusCounters
import Network.AWS.SageMaker.Types.OidcConfig
import Network.AWS.SageMaker.Types.OidcConfigForResponse
import Network.AWS.SageMaker.Types.OidcMemberDefinition
import Network.AWS.SageMaker.Types.Operator
import Network.AWS.SageMaker.Types.OrderKey
import Network.AWS.SageMaker.Types.OutputConfig
import Network.AWS.SageMaker.Types.OutputDataConfig
import Network.AWS.SageMaker.Types.ParameterRange
import Network.AWS.SageMaker.Types.ParameterRanges
import Network.AWS.SageMaker.Types.ParameterType
import Network.AWS.SageMaker.Types.Parent
import Network.AWS.SageMaker.Types.ParentHyperParameterTuningJob
import Network.AWS.SageMaker.Types.ProblemType
import Network.AWS.SageMaker.Types.ProcessingClusterConfig
import Network.AWS.SageMaker.Types.ProcessingInput
import Network.AWS.SageMaker.Types.ProcessingInstanceType
import Network.AWS.SageMaker.Types.ProcessingJob
import Network.AWS.SageMaker.Types.ProcessingJobStatus
import Network.AWS.SageMaker.Types.ProcessingJobSummary
import Network.AWS.SageMaker.Types.ProcessingOutput
import Network.AWS.SageMaker.Types.ProcessingOutputConfig
import Network.AWS.SageMaker.Types.ProcessingResources
import Network.AWS.SageMaker.Types.ProcessingS3CompressionType
import Network.AWS.SageMaker.Types.ProcessingS3DataDistributionType
import Network.AWS.SageMaker.Types.ProcessingS3DataType
import Network.AWS.SageMaker.Types.ProcessingS3Input
import Network.AWS.SageMaker.Types.ProcessingS3InputMode
import Network.AWS.SageMaker.Types.ProcessingS3Output
import Network.AWS.SageMaker.Types.ProcessingS3UploadMode
import Network.AWS.SageMaker.Types.ProcessingStoppingCondition
import Network.AWS.SageMaker.Types.ProductionVariant
import Network.AWS.SageMaker.Types.ProductionVariantAcceleratorType
import Network.AWS.SageMaker.Types.ProductionVariantInstanceType
import Network.AWS.SageMaker.Types.ProductionVariantSummary
import Network.AWS.SageMaker.Types.PropertyNameQuery
import Network.AWS.SageMaker.Types.PropertyNameSuggestion
import Network.AWS.SageMaker.Types.PublicWorkforceTaskPrice
import Network.AWS.SageMaker.Types.RecordWrapper
import Network.AWS.SageMaker.Types.RenderableTask
import Network.AWS.SageMaker.Types.RenderingError
import Network.AWS.SageMaker.Types.RepositoryAccessMode
import Network.AWS.SageMaker.Types.ResolvedAttributes
import Network.AWS.SageMaker.Types.ResourceConfig
import Network.AWS.SageMaker.Types.ResourceLimits
import Network.AWS.SageMaker.Types.ResourceSpec
import Network.AWS.SageMaker.Types.ResourceType
import Network.AWS.SageMaker.Types.RetentionPolicy
import Network.AWS.SageMaker.Types.RetentionType
import Network.AWS.SageMaker.Types.RootAccess
import Network.AWS.SageMaker.Types.RuleEvaluationStatus
import Network.AWS.SageMaker.Types.S3DataDistribution
import Network.AWS.SageMaker.Types.S3DataSource
import Network.AWS.SageMaker.Types.S3DataType
import Network.AWS.SageMaker.Types.ScheduleConfig
import Network.AWS.SageMaker.Types.ScheduleStatus
import Network.AWS.SageMaker.Types.SearchExpression
import Network.AWS.SageMaker.Types.SearchRecord
import Network.AWS.SageMaker.Types.SearchSortOrder
import Network.AWS.SageMaker.Types.SecondaryStatus
import Network.AWS.SageMaker.Types.SecondaryStatusTransition
import Network.AWS.SageMaker.Types.SharingSettings
import Network.AWS.SageMaker.Types.ShuffleConfig
import Network.AWS.SageMaker.Types.SortBy
import Network.AWS.SageMaker.Types.SortExperimentsBy
import Network.AWS.SageMaker.Types.SortOrder
import Network.AWS.SageMaker.Types.SortTrialComponentsBy
import Network.AWS.SageMaker.Types.SortTrialsBy
import Network.AWS.SageMaker.Types.SourceAlgorithm
import Network.AWS.SageMaker.Types.SourceAlgorithmSpecification
import Network.AWS.SageMaker.Types.SourceIPConfig
import Network.AWS.SageMaker.Types.SplitType
import Network.AWS.SageMaker.Types.StoppingCondition
import Network.AWS.SageMaker.Types.SubscribedWorkteam
import Network.AWS.SageMaker.Types.SuggestionQuery
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.TargetDevice
import Network.AWS.SageMaker.Types.TargetPlatform
import Network.AWS.SageMaker.Types.TargetPlatformAccelerator
import Network.AWS.SageMaker.Types.TargetPlatformArch
import Network.AWS.SageMaker.Types.TargetPlatformOS
import Network.AWS.SageMaker.Types.TensorBoardAppSettings
import Network.AWS.SageMaker.Types.TensorBoardOutputConfig
import Network.AWS.SageMaker.Types.TrainingInputMode
import Network.AWS.SageMaker.Types.TrainingInstanceType
import Network.AWS.SageMaker.Types.TrainingJob
import Network.AWS.SageMaker.Types.TrainingJobDefinition
import Network.AWS.SageMaker.Types.TrainingJobEarlyStoppingType
import Network.AWS.SageMaker.Types.TrainingJobSortByOptions
import Network.AWS.SageMaker.Types.TrainingJobStatus
import Network.AWS.SageMaker.Types.TrainingJobStatusCounters
import Network.AWS.SageMaker.Types.TrainingJobSummary
import Network.AWS.SageMaker.Types.TrainingSpecification
import Network.AWS.SageMaker.Types.TransformDataSource
import Network.AWS.SageMaker.Types.TransformInput
import Network.AWS.SageMaker.Types.TransformInstanceType
import Network.AWS.SageMaker.Types.TransformJob
import Network.AWS.SageMaker.Types.TransformJobDefinition
import Network.AWS.SageMaker.Types.TransformJobStatus
import Network.AWS.SageMaker.Types.TransformJobSummary
import Network.AWS.SageMaker.Types.TransformOutput
import Network.AWS.SageMaker.Types.TransformResources
import Network.AWS.SageMaker.Types.TransformS3DataSource
import Network.AWS.SageMaker.Types.Trial
import Network.AWS.SageMaker.Types.TrialComponent
import Network.AWS.SageMaker.Types.TrialComponentArtifact
import Network.AWS.SageMaker.Types.TrialComponentMetricSummary
import Network.AWS.SageMaker.Types.TrialComponentParameterValue
import Network.AWS.SageMaker.Types.TrialComponentPrimaryStatus
import Network.AWS.SageMaker.Types.TrialComponentSimpleSummary
import Network.AWS.SageMaker.Types.TrialComponentSource
import Network.AWS.SageMaker.Types.TrialComponentSourceDetail
import Network.AWS.SageMaker.Types.TrialComponentStatus
import Network.AWS.SageMaker.Types.TrialComponentSummary
import Network.AWS.SageMaker.Types.TrialSource
import Network.AWS.SageMaker.Types.TrialSummary
import Network.AWS.SageMaker.Types.TuningJobCompletionCriteria
import Network.AWS.SageMaker.Types.USD
import Network.AWS.SageMaker.Types.UiConfig
import Network.AWS.SageMaker.Types.UiTemplate
import Network.AWS.SageMaker.Types.UiTemplateInfo
import Network.AWS.SageMaker.Types.UserContext
import Network.AWS.SageMaker.Types.UserProfileDetails
import Network.AWS.SageMaker.Types.UserProfileSortKey
import Network.AWS.SageMaker.Types.UserProfileStatus
import Network.AWS.SageMaker.Types.UserSettings
import Network.AWS.SageMaker.Types.VPCConfig
import Network.AWS.SageMaker.Types.VariantProperty
import Network.AWS.SageMaker.Types.VariantPropertyType
import Network.AWS.SageMaker.Types.Workforce
import Network.AWS.SageMaker.Types.Workteam
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-07-24@ of the Amazon SageMaker Service SDK configuration.
sageMakerService :: Lude.Service
sageMakerService =
  Lude.Service
    { Lude._svcAbbrev = "SageMaker",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "api.sagemaker",
      Lude._svcVersion = "2017-07-24",
      Lude._svcEndpoint = Lude.defaultEndpoint sageMakerService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "SageMaker",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing

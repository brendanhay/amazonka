{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceLimitExceeded,
    _ResourceInUse,
    _ConflictException,
    _ResourceNotFound,

    -- * ActionStatus
    ActionStatus (..),

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

    -- * ArtifactSourceIdType
    ArtifactSourceIdType (..),

    -- * AssemblyType
    AssemblyType (..),

    -- * AssociationEdgeType
    AssociationEdgeType (..),

    -- * AthenaResultCompressionType
    AthenaResultCompressionType (..),

    -- * AthenaResultFormat
    AthenaResultFormat (..),

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

    -- * AwsManagedHumanLoopRequestSource
    AwsManagedHumanLoopRequestSource (..),

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

    -- * CapacitySizeType
    CapacitySizeType (..),

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

    -- * ConditionOutcome
    ConditionOutcome (..),

    -- * ContainerMode
    ContainerMode (..),

    -- * ContentClassifier
    ContentClassifier (..),

    -- * DataDistributionType
    DataDistributionType (..),

    -- * DetailedAlgorithmStatus
    DetailedAlgorithmStatus (..),

    -- * DetailedModelPackageStatus
    DetailedModelPackageStatus (..),

    -- * DirectInternetAccess
    DirectInternetAccess (..),

    -- * DomainStatus
    DomainStatus (..),

    -- * EdgePackagingJobStatus
    EdgePackagingJobStatus (..),

    -- * EdgePresetDeploymentStatus
    EdgePresetDeploymentStatus (..),

    -- * EdgePresetDeploymentType
    EdgePresetDeploymentType (..),

    -- * EndpointConfigSortKey
    EndpointConfigSortKey (..),

    -- * EndpointSortKey
    EndpointSortKey (..),

    -- * EndpointStatus
    EndpointStatus (..),

    -- * ExecutionStatus
    ExecutionStatus (..),

    -- * FeatureGroupSortBy
    FeatureGroupSortBy (..),

    -- * FeatureGroupSortOrder
    FeatureGroupSortOrder (..),

    -- * FeatureGroupStatus
    FeatureGroupStatus (..),

    -- * FeatureType
    FeatureType (..),

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

    -- * InferenceExecutionMode
    InferenceExecutionMode (..),

    -- * InputMode
    InputMode (..),

    -- * InstanceType
    InstanceType (..),

    -- * JoinSource
    JoinSource (..),

    -- * LabelingJobStatus
    LabelingJobStatus (..),

    -- * ListCompilationJobsSortBy
    ListCompilationJobsSortBy (..),

    -- * ListDeviceFleetsSortBy
    ListDeviceFleetsSortBy (..),

    -- * ListEdgePackagingJobsSortBy
    ListEdgePackagingJobsSortBy (..),

    -- * ListLabelingJobsForWorkteamSortByOptions
    ListLabelingJobsForWorkteamSortByOptions (..),

    -- * ListWorkforcesSortByOptions
    ListWorkforcesSortByOptions (..),

    -- * ListWorkteamsSortByOptions
    ListWorkteamsSortByOptions (..),

    -- * MetricSetSource
    MetricSetSource (..),

    -- * ModelApprovalStatus
    ModelApprovalStatus (..),

    -- * ModelCacheSetting
    ModelCacheSetting (..),

    -- * ModelPackageGroupSortBy
    ModelPackageGroupSortBy (..),

    -- * ModelPackageGroupStatus
    ModelPackageGroupStatus (..),

    -- * ModelPackageSortBy
    ModelPackageSortBy (..),

    -- * ModelPackageStatus
    ModelPackageStatus (..),

    -- * ModelPackageType
    ModelPackageType (..),

    -- * ModelSortKey
    ModelSortKey (..),

    -- * MonitoringExecutionSortKey
    MonitoringExecutionSortKey (..),

    -- * MonitoringJobDefinitionSortKey
    MonitoringJobDefinitionSortKey (..),

    -- * MonitoringProblemType
    MonitoringProblemType (..),

    -- * MonitoringScheduleSortKey
    MonitoringScheduleSortKey (..),

    -- * MonitoringType
    MonitoringType (..),

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

    -- * OfflineStoreStatusValue
    OfflineStoreStatusValue (..),

    -- * Operator
    Operator (..),

    -- * OrderKey
    OrderKey (..),

    -- * ParameterType
    ParameterType (..),

    -- * PipelineExecutionStatus
    PipelineExecutionStatus (..),

    -- * PipelineStatus
    PipelineStatus (..),

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

    -- * ProfilingStatus
    ProfilingStatus (..),

    -- * ProjectSortBy
    ProjectSortBy (..),

    -- * ProjectSortOrder
    ProjectSortOrder (..),

    -- * ProjectStatus
    ProjectStatus (..),

    -- * RecordWrapper
    RecordWrapper (..),

    -- * RedshiftResultCompressionType
    RedshiftResultCompressionType (..),

    -- * RedshiftResultFormat
    RedshiftResultFormat (..),

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

    -- * SagemakerServicecatalogStatus
    SagemakerServicecatalogStatus (..),

    -- * ScheduleStatus
    ScheduleStatus (..),

    -- * SearchSortOrder
    SearchSortOrder (..),

    -- * SecondaryStatus
    SecondaryStatus (..),

    -- * SortActionsBy
    SortActionsBy (..),

    -- * SortArtifactsBy
    SortArtifactsBy (..),

    -- * SortAssociationsBy
    SortAssociationsBy (..),

    -- * SortBy
    SortBy (..),

    -- * SortContextsBy
    SortContextsBy (..),

    -- * SortExperimentsBy
    SortExperimentsBy (..),

    -- * SortOrder
    SortOrder (..),

    -- * SortPipelineExecutionsBy
    SortPipelineExecutionsBy (..),

    -- * SortPipelinesBy
    SortPipelinesBy (..),

    -- * SortTrialComponentsBy
    SortTrialComponentsBy (..),

    -- * SortTrialsBy
    SortTrialsBy (..),

    -- * SplitType
    SplitType (..),

    -- * StepStatus
    StepStatus (..),

    -- * StudioLifecycleConfigAppType
    StudioLifecycleConfigAppType (..),

    -- * StudioLifecycleConfigSortKey
    StudioLifecycleConfigSortKey (..),

    -- * TargetDevice
    TargetDevice (..),

    -- * TargetPlatformAccelerator
    TargetPlatformAccelerator (..),

    -- * TargetPlatformArch
    TargetPlatformArch (..),

    -- * TargetPlatformOs
    TargetPlatformOs (..),

    -- * TrafficRoutingConfigType
    TrafficRoutingConfigType (..),

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

    -- * ActionSource
    ActionSource (..),
    newActionSource,
    actionSource_sourceType,
    actionSource_sourceId,
    actionSource_sourceUri,

    -- * ActionSummary
    ActionSummary (..),
    newActionSummary,
    actionSummary_creationTime,
    actionSummary_status,
    actionSummary_lastModifiedTime,
    actionSummary_actionName,
    actionSummary_source,
    actionSummary_actionArn,
    actionSummary_actionType,

    -- * AgentVersion
    AgentVersion (..),
    newAgentVersion,
    agentVersion_version,
    agentVersion_agentCount,

    -- * Alarm
    Alarm (..),
    newAlarm,
    alarm_alarmName,

    -- * AlgorithmSpecification
    AlgorithmSpecification (..),
    newAlgorithmSpecification,
    algorithmSpecification_enableSageMakerMetricsTimeSeries,
    algorithmSpecification_algorithmName,
    algorithmSpecification_trainingImage,
    algorithmSpecification_metricDefinitions,
    algorithmSpecification_trainingInputMode,

    -- * AlgorithmStatusDetails
    AlgorithmStatusDetails (..),
    newAlgorithmStatusDetails,
    algorithmStatusDetails_imageScanStatuses,
    algorithmStatusDetails_validationStatuses,

    -- * AlgorithmStatusItem
    AlgorithmStatusItem (..),
    newAlgorithmStatusItem,
    algorithmStatusItem_failureReason,
    algorithmStatusItem_name,
    algorithmStatusItem_status,

    -- * AlgorithmSummary
    AlgorithmSummary (..),
    newAlgorithmSummary,
    algorithmSummary_algorithmDescription,
    algorithmSummary_algorithmName,
    algorithmSummary_algorithmArn,
    algorithmSummary_creationTime,
    algorithmSummary_algorithmStatus,

    -- * AlgorithmValidationProfile
    AlgorithmValidationProfile (..),
    newAlgorithmValidationProfile,
    algorithmValidationProfile_transformJobDefinition,
    algorithmValidationProfile_profileName,
    algorithmValidationProfile_trainingJobDefinition,

    -- * AlgorithmValidationSpecification
    AlgorithmValidationSpecification (..),
    newAlgorithmValidationSpecification,
    algorithmValidationSpecification_validationRole,
    algorithmValidationSpecification_validationProfiles,

    -- * AnnotationConsolidationConfig
    AnnotationConsolidationConfig (..),
    newAnnotationConsolidationConfig,
    annotationConsolidationConfig_annotationConsolidationLambdaArn,

    -- * AppDetails
    AppDetails (..),
    newAppDetails,
    appDetails_creationTime,
    appDetails_status,
    appDetails_userProfileName,
    appDetails_appName,
    appDetails_domainId,
    appDetails_appType,

    -- * AppImageConfigDetails
    AppImageConfigDetails (..),
    newAppImageConfigDetails,
    appImageConfigDetails_creationTime,
    appImageConfigDetails_appImageConfigName,
    appImageConfigDetails_lastModifiedTime,
    appImageConfigDetails_kernelGatewayImageConfig,
    appImageConfigDetails_appImageConfigArn,

    -- * AppSpecification
    AppSpecification (..),
    newAppSpecification,
    appSpecification_containerArguments,
    appSpecification_containerEntrypoint,
    appSpecification_imageUri,

    -- * ArtifactSource
    ArtifactSource (..),
    newArtifactSource,
    artifactSource_sourceTypes,
    artifactSource_sourceUri,

    -- * ArtifactSourceType
    ArtifactSourceType (..),
    newArtifactSourceType,
    artifactSourceType_sourceIdType,
    artifactSourceType_value,

    -- * ArtifactSummary
    ArtifactSummary (..),
    newArtifactSummary,
    artifactSummary_creationTime,
    artifactSummary_lastModifiedTime,
    artifactSummary_artifactName,
    artifactSummary_source,
    artifactSummary_artifactArn,
    artifactSummary_artifactType,

    -- * AssociationSummary
    AssociationSummary (..),
    newAssociationSummary,
    associationSummary_creationTime,
    associationSummary_sourceName,
    associationSummary_sourceType,
    associationSummary_sourceArn,
    associationSummary_createdBy,
    associationSummary_associationType,
    associationSummary_destinationArn,
    associationSummary_destinationType,
    associationSummary_destinationName,

    -- * AsyncInferenceClientConfig
    AsyncInferenceClientConfig (..),
    newAsyncInferenceClientConfig,
    asyncInferenceClientConfig_maxConcurrentInvocationsPerInstance,

    -- * AsyncInferenceConfig
    AsyncInferenceConfig (..),
    newAsyncInferenceConfig,
    asyncInferenceConfig_clientConfig,
    asyncInferenceConfig_outputConfig,

    -- * AsyncInferenceNotificationConfig
    AsyncInferenceNotificationConfig (..),
    newAsyncInferenceNotificationConfig,
    asyncInferenceNotificationConfig_errorTopic,
    asyncInferenceNotificationConfig_successTopic,

    -- * AsyncInferenceOutputConfig
    AsyncInferenceOutputConfig (..),
    newAsyncInferenceOutputConfig,
    asyncInferenceOutputConfig_notificationConfig,
    asyncInferenceOutputConfig_kmsKeyId,
    asyncInferenceOutputConfig_s3OutputPath,

    -- * AthenaDatasetDefinition
    AthenaDatasetDefinition (..),
    newAthenaDatasetDefinition,
    athenaDatasetDefinition_kmsKeyId,
    athenaDatasetDefinition_workGroup,
    athenaDatasetDefinition_outputCompression,
    athenaDatasetDefinition_catalog,
    athenaDatasetDefinition_database,
    athenaDatasetDefinition_queryString,
    athenaDatasetDefinition_outputS3Uri,
    athenaDatasetDefinition_outputFormat,

    -- * AutoMLCandidate
    AutoMLCandidate (..),
    newAutoMLCandidate,
    autoMLCandidate_failureReason,
    autoMLCandidate_inferenceContainers,
    autoMLCandidate_candidateProperties,
    autoMLCandidate_endTime,
    autoMLCandidate_finalAutoMLJobObjectiveMetric,
    autoMLCandidate_candidateName,
    autoMLCandidate_objectiveStatus,
    autoMLCandidate_candidateSteps,
    autoMLCandidate_candidateStatus,
    autoMLCandidate_creationTime,
    autoMLCandidate_lastModifiedTime,

    -- * AutoMLCandidateStep
    AutoMLCandidateStep (..),
    newAutoMLCandidateStep,
    autoMLCandidateStep_candidateStepType,
    autoMLCandidateStep_candidateStepArn,
    autoMLCandidateStep_candidateStepName,

    -- * AutoMLChannel
    AutoMLChannel (..),
    newAutoMLChannel,
    autoMLChannel_compressionType,
    autoMLChannel_dataSource,
    autoMLChannel_targetAttributeName,

    -- * AutoMLContainerDefinition
    AutoMLContainerDefinition (..),
    newAutoMLContainerDefinition,
    autoMLContainerDefinition_environment,
    autoMLContainerDefinition_image,
    autoMLContainerDefinition_modelDataUrl,

    -- * AutoMLDataSource
    AutoMLDataSource (..),
    newAutoMLDataSource,
    autoMLDataSource_s3DataSource,

    -- * AutoMLJobArtifacts
    AutoMLJobArtifacts (..),
    newAutoMLJobArtifacts,
    autoMLJobArtifacts_candidateDefinitionNotebookLocation,
    autoMLJobArtifacts_dataExplorationNotebookLocation,

    -- * AutoMLJobCompletionCriteria
    AutoMLJobCompletionCriteria (..),
    newAutoMLJobCompletionCriteria,
    autoMLJobCompletionCriteria_maxCandidates,
    autoMLJobCompletionCriteria_maxRuntimePerTrainingJobInSeconds,
    autoMLJobCompletionCriteria_maxAutoMLJobRuntimeInSeconds,

    -- * AutoMLJobConfig
    AutoMLJobConfig (..),
    newAutoMLJobConfig,
    autoMLJobConfig_securityConfig,
    autoMLJobConfig_completionCriteria,

    -- * AutoMLJobObjective
    AutoMLJobObjective (..),
    newAutoMLJobObjective,
    autoMLJobObjective_metricName,

    -- * AutoMLJobSummary
    AutoMLJobSummary (..),
    newAutoMLJobSummary,
    autoMLJobSummary_failureReason,
    autoMLJobSummary_partialFailureReasons,
    autoMLJobSummary_endTime,
    autoMLJobSummary_autoMLJobName,
    autoMLJobSummary_autoMLJobArn,
    autoMLJobSummary_autoMLJobStatus,
    autoMLJobSummary_autoMLJobSecondaryStatus,
    autoMLJobSummary_creationTime,
    autoMLJobSummary_lastModifiedTime,

    -- * AutoMLOutputDataConfig
    AutoMLOutputDataConfig (..),
    newAutoMLOutputDataConfig,
    autoMLOutputDataConfig_kmsKeyId,
    autoMLOutputDataConfig_s3OutputPath,

    -- * AutoMLPartialFailureReason
    AutoMLPartialFailureReason (..),
    newAutoMLPartialFailureReason,
    autoMLPartialFailureReason_partialFailureMessage,

    -- * AutoMLS3DataSource
    AutoMLS3DataSource (..),
    newAutoMLS3DataSource,
    autoMLS3DataSource_s3DataType,
    autoMLS3DataSource_s3Uri,

    -- * AutoMLSecurityConfig
    AutoMLSecurityConfig (..),
    newAutoMLSecurityConfig,
    autoMLSecurityConfig_vpcConfig,
    autoMLSecurityConfig_volumeKmsKeyId,
    autoMLSecurityConfig_enableInterContainerTrafficEncryption,

    -- * AutoRollbackConfig
    AutoRollbackConfig (..),
    newAutoRollbackConfig,
    autoRollbackConfig_alarms,

    -- * Bias
    Bias (..),
    newBias,
    bias_report,

    -- * BlueGreenUpdatePolicy
    BlueGreenUpdatePolicy (..),
    newBlueGreenUpdatePolicy,
    blueGreenUpdatePolicy_maximumExecutionTimeoutInSeconds,
    blueGreenUpdatePolicy_terminationWaitInSeconds,
    blueGreenUpdatePolicy_trafficRoutingConfiguration,

    -- * CacheHitResult
    CacheHitResult (..),
    newCacheHitResult,
    cacheHitResult_sourcePipelineExecutionArn,

    -- * CallbackStepMetadata
    CallbackStepMetadata (..),
    newCallbackStepMetadata,
    callbackStepMetadata_callbackToken,
    callbackStepMetadata_outputParameters,
    callbackStepMetadata_sqsQueueUrl,

    -- * CandidateArtifactLocations
    CandidateArtifactLocations (..),
    newCandidateArtifactLocations,
    candidateArtifactLocations_explainability,

    -- * CandidateProperties
    CandidateProperties (..),
    newCandidateProperties,
    candidateProperties_candidateArtifactLocations,
    candidateProperties_candidateMetrics,

    -- * CapacitySize
    CapacitySize (..),
    newCapacitySize,
    capacitySize_type,
    capacitySize_value,

    -- * CaptureContentTypeHeader
    CaptureContentTypeHeader (..),
    newCaptureContentTypeHeader,
    captureContentTypeHeader_csvContentTypes,
    captureContentTypeHeader_jsonContentTypes,

    -- * CaptureOption
    CaptureOption (..),
    newCaptureOption,
    captureOption_captureMode,

    -- * CategoricalParameterRange
    CategoricalParameterRange (..),
    newCategoricalParameterRange,
    categoricalParameterRange_name,
    categoricalParameterRange_values,

    -- * CategoricalParameterRangeSpecification
    CategoricalParameterRangeSpecification (..),
    newCategoricalParameterRangeSpecification,
    categoricalParameterRangeSpecification_values,

    -- * Channel
    Channel (..),
    newChannel,
    channel_shuffleConfig,
    channel_recordWrapperType,
    channel_inputMode,
    channel_compressionType,
    channel_contentType,
    channel_channelName,
    channel_dataSource,

    -- * ChannelSpecification
    ChannelSpecification (..),
    newChannelSpecification,
    channelSpecification_supportedCompressionTypes,
    channelSpecification_isRequired,
    channelSpecification_description,
    channelSpecification_name,
    channelSpecification_supportedContentTypes,
    channelSpecification_supportedInputModes,

    -- * CheckpointConfig
    CheckpointConfig (..),
    newCheckpointConfig,
    checkpointConfig_localPath,
    checkpointConfig_s3Uri,

    -- * CodeRepositorySummary
    CodeRepositorySummary (..),
    newCodeRepositorySummary,
    codeRepositorySummary_gitConfig,
    codeRepositorySummary_codeRepositoryName,
    codeRepositorySummary_codeRepositoryArn,
    codeRepositorySummary_creationTime,
    codeRepositorySummary_lastModifiedTime,

    -- * CognitoConfig
    CognitoConfig (..),
    newCognitoConfig,
    cognitoConfig_userPool,
    cognitoConfig_clientId,

    -- * CognitoMemberDefinition
    CognitoMemberDefinition (..),
    newCognitoMemberDefinition,
    cognitoMemberDefinition_userPool,
    cognitoMemberDefinition_userGroup,
    cognitoMemberDefinition_clientId,

    -- * CollectionConfiguration
    CollectionConfiguration (..),
    newCollectionConfiguration,
    collectionConfiguration_collectionParameters,
    collectionConfiguration_collectionName,

    -- * CompilationJobSummary
    CompilationJobSummary (..),
    newCompilationJobSummary,
    compilationJobSummary_compilationStartTime,
    compilationJobSummary_compilationTargetPlatformAccelerator,
    compilationJobSummary_compilationTargetDevice,
    compilationJobSummary_lastModifiedTime,
    compilationJobSummary_compilationTargetPlatformArch,
    compilationJobSummary_compilationEndTime,
    compilationJobSummary_compilationTargetPlatformOs,
    compilationJobSummary_compilationJobName,
    compilationJobSummary_compilationJobArn,
    compilationJobSummary_creationTime,
    compilationJobSummary_compilationJobStatus,

    -- * ConditionStepMetadata
    ConditionStepMetadata (..),
    newConditionStepMetadata,
    conditionStepMetadata_outcome,

    -- * ContainerDefinition
    ContainerDefinition (..),
    newContainerDefinition,
    containerDefinition_multiModelConfig,
    containerDefinition_modelDataUrl,
    containerDefinition_image,
    containerDefinition_modelPackageName,
    containerDefinition_environment,
    containerDefinition_imageConfig,
    containerDefinition_mode,
    containerDefinition_containerHostname,

    -- * ContextSource
    ContextSource (..),
    newContextSource,
    contextSource_sourceType,
    contextSource_sourceId,
    contextSource_sourceUri,

    -- * ContextSummary
    ContextSummary (..),
    newContextSummary,
    contextSummary_creationTime,
    contextSummary_lastModifiedTime,
    contextSummary_contextType,
    contextSummary_contextArn,
    contextSummary_source,
    contextSummary_contextName,

    -- * ContinuousParameterRange
    ContinuousParameterRange (..),
    newContinuousParameterRange,
    continuousParameterRange_scalingType,
    continuousParameterRange_name,
    continuousParameterRange_minValue,
    continuousParameterRange_maxValue,

    -- * ContinuousParameterRangeSpecification
    ContinuousParameterRangeSpecification (..),
    newContinuousParameterRangeSpecification,
    continuousParameterRangeSpecification_minValue,
    continuousParameterRangeSpecification_maxValue,

    -- * CustomImage
    CustomImage (..),
    newCustomImage,
    customImage_imageVersionNumber,
    customImage_imageName,
    customImage_appImageConfigName,

    -- * DataCaptureConfig
    DataCaptureConfig (..),
    newDataCaptureConfig,
    dataCaptureConfig_captureContentTypeHeader,
    dataCaptureConfig_kmsKeyId,
    dataCaptureConfig_enableCapture,
    dataCaptureConfig_initialSamplingPercentage,
    dataCaptureConfig_destinationS3Uri,
    dataCaptureConfig_captureOptions,

    -- * DataCaptureConfigSummary
    DataCaptureConfigSummary (..),
    newDataCaptureConfigSummary,
    dataCaptureConfigSummary_enableCapture,
    dataCaptureConfigSummary_captureStatus,
    dataCaptureConfigSummary_currentSamplingPercentage,
    dataCaptureConfigSummary_destinationS3Uri,
    dataCaptureConfigSummary_kmsKeyId,

    -- * DataCatalogConfig
    DataCatalogConfig (..),
    newDataCatalogConfig,
    dataCatalogConfig_tableName,
    dataCatalogConfig_catalog,
    dataCatalogConfig_database,

    -- * DataProcessing
    DataProcessing (..),
    newDataProcessing,
    dataProcessing_outputFilter,
    dataProcessing_joinSource,
    dataProcessing_inputFilter,

    -- * DataQualityAppSpecification
    DataQualityAppSpecification (..),
    newDataQualityAppSpecification,
    dataQualityAppSpecification_containerArguments,
    dataQualityAppSpecification_recordPreprocessorSourceUri,
    dataQualityAppSpecification_environment,
    dataQualityAppSpecification_containerEntrypoint,
    dataQualityAppSpecification_postAnalyticsProcessorSourceUri,
    dataQualityAppSpecification_imageUri,

    -- * DataQualityBaselineConfig
    DataQualityBaselineConfig (..),
    newDataQualityBaselineConfig,
    dataQualityBaselineConfig_constraintsResource,
    dataQualityBaselineConfig_statisticsResource,
    dataQualityBaselineConfig_baseliningJobName,

    -- * DataQualityJobInput
    DataQualityJobInput (..),
    newDataQualityJobInput,
    dataQualityJobInput_endpointInput,

    -- * DataSource
    DataSource (..),
    newDataSource,
    dataSource_s3DataSource,
    dataSource_fileSystemDataSource,

    -- * DatasetDefinition
    DatasetDefinition (..),
    newDatasetDefinition,
    datasetDefinition_redshiftDatasetDefinition,
    datasetDefinition_athenaDatasetDefinition,
    datasetDefinition_localPath,
    datasetDefinition_dataDistributionType,
    datasetDefinition_inputMode,

    -- * DebugHookConfig
    DebugHookConfig (..),
    newDebugHookConfig,
    debugHookConfig_localPath,
    debugHookConfig_collectionConfigurations,
    debugHookConfig_hookParameters,
    debugHookConfig_s3OutputPath,

    -- * DebugRuleConfiguration
    DebugRuleConfiguration (..),
    newDebugRuleConfiguration,
    debugRuleConfiguration_ruleParameters,
    debugRuleConfiguration_s3OutputPath,
    debugRuleConfiguration_localPath,
    debugRuleConfiguration_instanceType,
    debugRuleConfiguration_volumeSizeInGB,
    debugRuleConfiguration_ruleConfigurationName,
    debugRuleConfiguration_ruleEvaluatorImage,

    -- * DebugRuleEvaluationStatus
    DebugRuleEvaluationStatus (..),
    newDebugRuleEvaluationStatus,
    debugRuleEvaluationStatus_lastModifiedTime,
    debugRuleEvaluationStatus_statusDetails,
    debugRuleEvaluationStatus_ruleEvaluationStatus,
    debugRuleEvaluationStatus_ruleEvaluationJobArn,
    debugRuleEvaluationStatus_ruleConfigurationName,

    -- * DeployedImage
    DeployedImage (..),
    newDeployedImage,
    deployedImage_resolvedImage,
    deployedImage_specifiedImage,
    deployedImage_resolutionTime,

    -- * DeploymentConfig
    DeploymentConfig (..),
    newDeploymentConfig,
    deploymentConfig_autoRollbackConfiguration,
    deploymentConfig_blueGreenUpdatePolicy,

    -- * DesiredWeightAndCapacity
    DesiredWeightAndCapacity (..),
    newDesiredWeightAndCapacity,
    desiredWeightAndCapacity_desiredInstanceCount,
    desiredWeightAndCapacity_desiredWeight,
    desiredWeightAndCapacity_variantName,

    -- * Device
    Device (..),
    newDevice,
    device_description,
    device_iotThingName,
    device_deviceName,

    -- * DeviceFleetSummary
    DeviceFleetSummary (..),
    newDeviceFleetSummary,
    deviceFleetSummary_creationTime,
    deviceFleetSummary_lastModifiedTime,
    deviceFleetSummary_deviceFleetArn,
    deviceFleetSummary_deviceFleetName,

    -- * DeviceStats
    DeviceStats (..),
    newDeviceStats,
    deviceStats_connectedDeviceCount,
    deviceStats_registeredDeviceCount,

    -- * DeviceSummary
    DeviceSummary (..),
    newDeviceSummary,
    deviceSummary_registrationTime,
    deviceSummary_models,
    deviceSummary_latestHeartbeat,
    deviceSummary_description,
    deviceSummary_deviceFleetName,
    deviceSummary_iotThingName,
    deviceSummary_deviceName,
    deviceSummary_deviceArn,

    -- * DomainDetails
    DomainDetails (..),
    newDomainDetails,
    domainDetails_creationTime,
    domainDetails_status,
    domainDetails_domainArn,
    domainDetails_url,
    domainDetails_lastModifiedTime,
    domainDetails_domainName,
    domainDetails_domainId,

    -- * EdgeModel
    EdgeModel (..),
    newEdgeModel,
    edgeModel_latestInference,
    edgeModel_latestSampleTime,
    edgeModel_modelName,
    edgeModel_modelVersion,

    -- * EdgeModelStat
    EdgeModelStat (..),
    newEdgeModelStat,
    edgeModelStat_modelName,
    edgeModelStat_modelVersion,
    edgeModelStat_offlineDeviceCount,
    edgeModelStat_connectedDeviceCount,
    edgeModelStat_activeDeviceCount,
    edgeModelStat_samplingDeviceCount,

    -- * EdgeModelSummary
    EdgeModelSummary (..),
    newEdgeModelSummary,
    edgeModelSummary_modelName,
    edgeModelSummary_modelVersion,

    -- * EdgeOutputConfig
    EdgeOutputConfig (..),
    newEdgeOutputConfig,
    edgeOutputConfig_presetDeploymentType,
    edgeOutputConfig_kmsKeyId,
    edgeOutputConfig_presetDeploymentConfig,
    edgeOutputConfig_s3OutputLocation,

    -- * EdgePackagingJobSummary
    EdgePackagingJobSummary (..),
    newEdgePackagingJobSummary,
    edgePackagingJobSummary_creationTime,
    edgePackagingJobSummary_modelName,
    edgePackagingJobSummary_lastModifiedTime,
    edgePackagingJobSummary_compilationJobName,
    edgePackagingJobSummary_modelVersion,
    edgePackagingJobSummary_edgePackagingJobArn,
    edgePackagingJobSummary_edgePackagingJobName,
    edgePackagingJobSummary_edgePackagingJobStatus,

    -- * EdgePresetDeploymentOutput
    EdgePresetDeploymentOutput (..),
    newEdgePresetDeploymentOutput,
    edgePresetDeploymentOutput_status,
    edgePresetDeploymentOutput_artifact,
    edgePresetDeploymentOutput_statusMessage,
    edgePresetDeploymentOutput_type,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_failureReason,
    endpoint_productionVariants,
    endpoint_monitoringSchedules,
    endpoint_dataCaptureConfig,
    endpoint_tags,
    endpoint_endpointName,
    endpoint_endpointArn,
    endpoint_endpointConfigName,
    endpoint_endpointStatus,
    endpoint_creationTime,
    endpoint_lastModifiedTime,

    -- * EndpointConfigSummary
    EndpointConfigSummary (..),
    newEndpointConfigSummary,
    endpointConfigSummary_endpointConfigName,
    endpointConfigSummary_endpointConfigArn,
    endpointConfigSummary_creationTime,

    -- * EndpointInput
    EndpointInput (..),
    newEndpointInput,
    endpointInput_inferenceAttribute,
    endpointInput_s3DataDistributionType,
    endpointInput_s3InputMode,
    endpointInput_startTimeOffset,
    endpointInput_featuresAttribute,
    endpointInput_endTimeOffset,
    endpointInput_probabilityThresholdAttribute,
    endpointInput_probabilityAttribute,
    endpointInput_endpointName,
    endpointInput_localPath,

    -- * EndpointSummary
    EndpointSummary (..),
    newEndpointSummary,
    endpointSummary_endpointName,
    endpointSummary_endpointArn,
    endpointSummary_creationTime,
    endpointSummary_lastModifiedTime,
    endpointSummary_endpointStatus,

    -- * Experiment
    Experiment (..),
    newExperiment,
    experiment_creationTime,
    experiment_createdBy,
    experiment_lastModifiedTime,
    experiment_experimentName,
    experiment_experimentArn,
    experiment_source,
    experiment_displayName,
    experiment_lastModifiedBy,
    experiment_description,
    experiment_tags,

    -- * ExperimentConfig
    ExperimentConfig (..),
    newExperimentConfig,
    experimentConfig_trialComponentDisplayName,
    experimentConfig_experimentName,
    experimentConfig_trialName,

    -- * ExperimentSource
    ExperimentSource (..),
    newExperimentSource,
    experimentSource_sourceType,
    experimentSource_sourceArn,

    -- * ExperimentSummary
    ExperimentSummary (..),
    newExperimentSummary,
    experimentSummary_creationTime,
    experimentSummary_lastModifiedTime,
    experimentSummary_experimentName,
    experimentSummary_experimentSource,
    experimentSummary_experimentArn,
    experimentSummary_displayName,

    -- * Explainability
    Explainability (..),
    newExplainability,
    explainability_report,

    -- * FeatureDefinition
    FeatureDefinition (..),
    newFeatureDefinition,
    featureDefinition_featureType,
    featureDefinition_featureName,

    -- * FeatureGroup
    FeatureGroup (..),
    newFeatureGroup,
    featureGroup_creationTime,
    featureGroup_offlineStoreConfig,
    featureGroup_failureReason,
    featureGroup_featureGroupStatus,
    featureGroup_featureDefinitions,
    featureGroup_offlineStoreStatus,
    featureGroup_onlineStoreConfig,
    featureGroup_eventTimeFeatureName,
    featureGroup_recordIdentifierFeatureName,
    featureGroup_featureGroupArn,
    featureGroup_featureGroupName,
    featureGroup_description,
    featureGroup_tags,
    featureGroup_roleArn,

    -- * FeatureGroupSummary
    FeatureGroupSummary (..),
    newFeatureGroupSummary,
    featureGroupSummary_featureGroupStatus,
    featureGroupSummary_offlineStoreStatus,
    featureGroupSummary_featureGroupName,
    featureGroupSummary_featureGroupArn,
    featureGroupSummary_creationTime,

    -- * FileSystemConfig
    FileSystemConfig (..),
    newFileSystemConfig,
    fileSystemConfig_defaultGid,
    fileSystemConfig_mountPath,
    fileSystemConfig_defaultUid,

    -- * FileSystemDataSource
    FileSystemDataSource (..),
    newFileSystemDataSource,
    fileSystemDataSource_fileSystemId,
    fileSystemDataSource_fileSystemAccessMode,
    fileSystemDataSource_fileSystemType,
    fileSystemDataSource_directoryPath,

    -- * Filter
    Filter (..),
    newFilter,
    filter_operator,
    filter_value,
    filter_name,

    -- * FinalAutoMLJobObjectiveMetric
    FinalAutoMLJobObjectiveMetric (..),
    newFinalAutoMLJobObjectiveMetric,
    finalAutoMLJobObjectiveMetric_type,
    finalAutoMLJobObjectiveMetric_metricName,
    finalAutoMLJobObjectiveMetric_value,

    -- * FinalHyperParameterTuningJobObjectiveMetric
    FinalHyperParameterTuningJobObjectiveMetric (..),
    newFinalHyperParameterTuningJobObjectiveMetric,
    finalHyperParameterTuningJobObjectiveMetric_type,
    finalHyperParameterTuningJobObjectiveMetric_metricName,
    finalHyperParameterTuningJobObjectiveMetric_value,

    -- * FlowDefinitionOutputConfig
    FlowDefinitionOutputConfig (..),
    newFlowDefinitionOutputConfig,
    flowDefinitionOutputConfig_kmsKeyId,
    flowDefinitionOutputConfig_s3OutputPath,

    -- * FlowDefinitionSummary
    FlowDefinitionSummary (..),
    newFlowDefinitionSummary,
    flowDefinitionSummary_failureReason,
    flowDefinitionSummary_flowDefinitionName,
    flowDefinitionSummary_flowDefinitionArn,
    flowDefinitionSummary_flowDefinitionStatus,
    flowDefinitionSummary_creationTime,

    -- * GitConfig
    GitConfig (..),
    newGitConfig,
    gitConfig_branch,
    gitConfig_secretArn,
    gitConfig_repositoryUrl,

    -- * GitConfigForUpdate
    GitConfigForUpdate (..),
    newGitConfigForUpdate,
    gitConfigForUpdate_secretArn,

    -- * HumanLoopActivationConditionsConfig
    HumanLoopActivationConditionsConfig (..),
    newHumanLoopActivationConditionsConfig,
    humanLoopActivationConditionsConfig_humanLoopActivationConditions,

    -- * HumanLoopActivationConfig
    HumanLoopActivationConfig (..),
    newHumanLoopActivationConfig,
    humanLoopActivationConfig_humanLoopActivationConditionsConfig,

    -- * HumanLoopConfig
    HumanLoopConfig (..),
    newHumanLoopConfig,
    humanLoopConfig_taskKeywords,
    humanLoopConfig_publicWorkforceTaskPrice,
    humanLoopConfig_taskTimeLimitInSeconds,
    humanLoopConfig_taskAvailabilityLifetimeInSeconds,
    humanLoopConfig_workteamArn,
    humanLoopConfig_humanTaskUiArn,
    humanLoopConfig_taskTitle,
    humanLoopConfig_taskDescription,
    humanLoopConfig_taskCount,

    -- * HumanLoopRequestSource
    HumanLoopRequestSource (..),
    newHumanLoopRequestSource,
    humanLoopRequestSource_awsManagedHumanLoopRequestSource,

    -- * HumanTaskConfig
    HumanTaskConfig (..),
    newHumanTaskConfig,
    humanTaskConfig_taskKeywords,
    humanTaskConfig_publicWorkforceTaskPrice,
    humanTaskConfig_taskAvailabilityLifetimeInSeconds,
    humanTaskConfig_maxConcurrentTaskCount,
    humanTaskConfig_workteamArn,
    humanTaskConfig_uiConfig,
    humanTaskConfig_preHumanTaskLambdaArn,
    humanTaskConfig_taskTitle,
    humanTaskConfig_taskDescription,
    humanTaskConfig_numberOfHumanWorkersPerDataObject,
    humanTaskConfig_taskTimeLimitInSeconds,
    humanTaskConfig_annotationConsolidationConfig,

    -- * HumanTaskUiSummary
    HumanTaskUiSummary (..),
    newHumanTaskUiSummary,
    humanTaskUiSummary_humanTaskUiName,
    humanTaskUiSummary_humanTaskUiArn,
    humanTaskUiSummary_creationTime,

    -- * HyperParameterAlgorithmSpecification
    HyperParameterAlgorithmSpecification (..),
    newHyperParameterAlgorithmSpecification,
    hyperParameterAlgorithmSpecification_algorithmName,
    hyperParameterAlgorithmSpecification_trainingImage,
    hyperParameterAlgorithmSpecification_metricDefinitions,
    hyperParameterAlgorithmSpecification_trainingInputMode,

    -- * HyperParameterSpecification
    HyperParameterSpecification (..),
    newHyperParameterSpecification,
    hyperParameterSpecification_isTunable,
    hyperParameterSpecification_range,
    hyperParameterSpecification_defaultValue,
    hyperParameterSpecification_isRequired,
    hyperParameterSpecification_description,
    hyperParameterSpecification_name,
    hyperParameterSpecification_type,

    -- * HyperParameterTrainingJobDefinition
    HyperParameterTrainingJobDefinition (..),
    newHyperParameterTrainingJobDefinition,
    hyperParameterTrainingJobDefinition_tuningObjective,
    hyperParameterTrainingJobDefinition_checkpointConfig,
    hyperParameterTrainingJobDefinition_hyperParameterRanges,
    hyperParameterTrainingJobDefinition_retryStrategy,
    hyperParameterTrainingJobDefinition_enableNetworkIsolation,
    hyperParameterTrainingJobDefinition_staticHyperParameters,
    hyperParameterTrainingJobDefinition_enableManagedSpotTraining,
    hyperParameterTrainingJobDefinition_inputDataConfig,
    hyperParameterTrainingJobDefinition_vpcConfig,
    hyperParameterTrainingJobDefinition_definitionName,
    hyperParameterTrainingJobDefinition_enableInterContainerTrafficEncryption,
    hyperParameterTrainingJobDefinition_algorithmSpecification,
    hyperParameterTrainingJobDefinition_roleArn,
    hyperParameterTrainingJobDefinition_outputDataConfig,
    hyperParameterTrainingJobDefinition_resourceConfig,
    hyperParameterTrainingJobDefinition_stoppingCondition,

    -- * HyperParameterTrainingJobSummary
    HyperParameterTrainingJobSummary (..),
    newHyperParameterTrainingJobSummary,
    hyperParameterTrainingJobSummary_failureReason,
    hyperParameterTrainingJobSummary_tuningJobName,
    hyperParameterTrainingJobSummary_trainingEndTime,
    hyperParameterTrainingJobSummary_objectiveStatus,
    hyperParameterTrainingJobSummary_trainingJobDefinitionName,
    hyperParameterTrainingJobSummary_trainingStartTime,
    hyperParameterTrainingJobSummary_finalHyperParameterTuningJobObjectiveMetric,
    hyperParameterTrainingJobSummary_trainingJobName,
    hyperParameterTrainingJobSummary_trainingJobArn,
    hyperParameterTrainingJobSummary_creationTime,
    hyperParameterTrainingJobSummary_trainingJobStatus,
    hyperParameterTrainingJobSummary_tunedHyperParameters,

    -- * HyperParameterTuningJobConfig
    HyperParameterTuningJobConfig (..),
    newHyperParameterTuningJobConfig,
    hyperParameterTuningJobConfig_tuningJobCompletionCriteria,
    hyperParameterTuningJobConfig_parameterRanges,
    hyperParameterTuningJobConfig_hyperParameterTuningJobObjective,
    hyperParameterTuningJobConfig_trainingJobEarlyStoppingType,
    hyperParameterTuningJobConfig_strategy,
    hyperParameterTuningJobConfig_resourceLimits,

    -- * HyperParameterTuningJobObjective
    HyperParameterTuningJobObjective (..),
    newHyperParameterTuningJobObjective,
    hyperParameterTuningJobObjective_type,
    hyperParameterTuningJobObjective_metricName,

    -- * HyperParameterTuningJobSummary
    HyperParameterTuningJobSummary (..),
    newHyperParameterTuningJobSummary,
    hyperParameterTuningJobSummary_resourceLimits,
    hyperParameterTuningJobSummary_lastModifiedTime,
    hyperParameterTuningJobSummary_hyperParameterTuningEndTime,
    hyperParameterTuningJobSummary_hyperParameterTuningJobName,
    hyperParameterTuningJobSummary_hyperParameterTuningJobArn,
    hyperParameterTuningJobSummary_hyperParameterTuningJobStatus,
    hyperParameterTuningJobSummary_strategy,
    hyperParameterTuningJobSummary_creationTime,
    hyperParameterTuningJobSummary_trainingJobStatusCounters,
    hyperParameterTuningJobSummary_objectiveStatusCounters,

    -- * HyperParameterTuningJobWarmStartConfig
    HyperParameterTuningJobWarmStartConfig (..),
    newHyperParameterTuningJobWarmStartConfig,
    hyperParameterTuningJobWarmStartConfig_parentHyperParameterTuningJobs,
    hyperParameterTuningJobWarmStartConfig_warmStartType,

    -- * Image
    Image (..),
    newImage,
    image_failureReason,
    image_displayName,
    image_description,
    image_creationTime,
    image_imageArn,
    image_imageName,
    image_imageStatus,
    image_lastModifiedTime,

    -- * ImageConfig
    ImageConfig (..),
    newImageConfig,
    imageConfig_repositoryAuthConfig,
    imageConfig_repositoryAccessMode,

    -- * ImageVersion
    ImageVersion (..),
    newImageVersion,
    imageVersion_failureReason,
    imageVersion_creationTime,
    imageVersion_imageArn,
    imageVersion_imageVersionArn,
    imageVersion_imageVersionStatus,
    imageVersion_lastModifiedTime,
    imageVersion_version,

    -- * InferenceExecutionConfig
    InferenceExecutionConfig (..),
    newInferenceExecutionConfig,
    inferenceExecutionConfig_mode,

    -- * InferenceSpecification
    InferenceSpecification (..),
    newInferenceSpecification,
    inferenceSpecification_supportedRealtimeInferenceInstanceTypes,
    inferenceSpecification_supportedTransformInstanceTypes,
    inferenceSpecification_containers,
    inferenceSpecification_supportedContentTypes,
    inferenceSpecification_supportedResponseMIMETypes,

    -- * InputConfig
    InputConfig (..),
    newInputConfig,
    inputConfig_frameworkVersion,
    inputConfig_s3Uri,
    inputConfig_dataInputConfig,
    inputConfig_framework,

    -- * IntegerParameterRange
    IntegerParameterRange (..),
    newIntegerParameterRange,
    integerParameterRange_scalingType,
    integerParameterRange_name,
    integerParameterRange_minValue,
    integerParameterRange_maxValue,

    -- * IntegerParameterRangeSpecification
    IntegerParameterRangeSpecification (..),
    newIntegerParameterRangeSpecification,
    integerParameterRangeSpecification_minValue,
    integerParameterRangeSpecification_maxValue,

    -- * JupyterServerAppSettings
    JupyterServerAppSettings (..),
    newJupyterServerAppSettings,
    jupyterServerAppSettings_defaultResourceSpec,
    jupyterServerAppSettings_lifecycleConfigArns,

    -- * KernelGatewayAppSettings
    KernelGatewayAppSettings (..),
    newKernelGatewayAppSettings,
    kernelGatewayAppSettings_defaultResourceSpec,
    kernelGatewayAppSettings_customImages,
    kernelGatewayAppSettings_lifecycleConfigArns,

    -- * KernelGatewayImageConfig
    KernelGatewayImageConfig (..),
    newKernelGatewayImageConfig,
    kernelGatewayImageConfig_fileSystemConfig,
    kernelGatewayImageConfig_kernelSpecs,

    -- * KernelSpec
    KernelSpec (..),
    newKernelSpec,
    kernelSpec_displayName,
    kernelSpec_name,

    -- * LabelCounters
    LabelCounters (..),
    newLabelCounters,
    labelCounters_machineLabeled,
    labelCounters_totalLabeled,
    labelCounters_failedNonRetryableError,
    labelCounters_unlabeled,
    labelCounters_humanLabeled,

    -- * LabelCountersForWorkteam
    LabelCountersForWorkteam (..),
    newLabelCountersForWorkteam,
    labelCountersForWorkteam_pendingHuman,
    labelCountersForWorkteam_total,
    labelCountersForWorkteam_humanLabeled,

    -- * LabelingJobAlgorithmsConfig
    LabelingJobAlgorithmsConfig (..),
    newLabelingJobAlgorithmsConfig,
    labelingJobAlgorithmsConfig_labelingJobResourceConfig,
    labelingJobAlgorithmsConfig_initialActiveLearningModelArn,
    labelingJobAlgorithmsConfig_labelingJobAlgorithmSpecificationArn,

    -- * LabelingJobDataAttributes
    LabelingJobDataAttributes (..),
    newLabelingJobDataAttributes,
    labelingJobDataAttributes_contentClassifiers,

    -- * LabelingJobDataSource
    LabelingJobDataSource (..),
    newLabelingJobDataSource,
    labelingJobDataSource_s3DataSource,
    labelingJobDataSource_snsDataSource,

    -- * LabelingJobForWorkteamSummary
    LabelingJobForWorkteamSummary (..),
    newLabelingJobForWorkteamSummary,
    labelingJobForWorkteamSummary_numberOfHumanWorkersPerDataObject,
    labelingJobForWorkteamSummary_labelCounters,
    labelingJobForWorkteamSummary_labelingJobName,
    labelingJobForWorkteamSummary_jobReferenceCode,
    labelingJobForWorkteamSummary_workRequesterAccountId,
    labelingJobForWorkteamSummary_creationTime,

    -- * LabelingJobInputConfig
    LabelingJobInputConfig (..),
    newLabelingJobInputConfig,
    labelingJobInputConfig_dataAttributes,
    labelingJobInputConfig_dataSource,

    -- * LabelingJobOutput
    LabelingJobOutput (..),
    newLabelingJobOutput,
    labelingJobOutput_finalActiveLearningModelArn,
    labelingJobOutput_outputDatasetS3Uri,

    -- * LabelingJobOutputConfig
    LabelingJobOutputConfig (..),
    newLabelingJobOutputConfig,
    labelingJobOutputConfig_snsTopicArn,
    labelingJobOutputConfig_kmsKeyId,
    labelingJobOutputConfig_s3OutputPath,

    -- * LabelingJobResourceConfig
    LabelingJobResourceConfig (..),
    newLabelingJobResourceConfig,
    labelingJobResourceConfig_volumeKmsKeyId,

    -- * LabelingJobS3DataSource
    LabelingJobS3DataSource (..),
    newLabelingJobS3DataSource,
    labelingJobS3DataSource_manifestS3Uri,

    -- * LabelingJobSnsDataSource
    LabelingJobSnsDataSource (..),
    newLabelingJobSnsDataSource,
    labelingJobSnsDataSource_snsTopicArn,

    -- * LabelingJobStoppingConditions
    LabelingJobStoppingConditions (..),
    newLabelingJobStoppingConditions,
    labelingJobStoppingConditions_maxHumanLabeledObjectCount,
    labelingJobStoppingConditions_maxPercentageOfInputDatasetLabeled,

    -- * LabelingJobSummary
    LabelingJobSummary (..),
    newLabelingJobSummary,
    labelingJobSummary_failureReason,
    labelingJobSummary_annotationConsolidationLambdaArn,
    labelingJobSummary_inputConfig,
    labelingJobSummary_labelingJobOutput,
    labelingJobSummary_labelingJobName,
    labelingJobSummary_labelingJobArn,
    labelingJobSummary_creationTime,
    labelingJobSummary_lastModifiedTime,
    labelingJobSummary_labelingJobStatus,
    labelingJobSummary_labelCounters,
    labelingJobSummary_workteamArn,
    labelingJobSummary_preHumanTaskLambdaArn,

    -- * LambdaStepMetadata
    LambdaStepMetadata (..),
    newLambdaStepMetadata,
    lambdaStepMetadata_arn,
    lambdaStepMetadata_outputParameters,

    -- * MemberDefinition
    MemberDefinition (..),
    newMemberDefinition,
    memberDefinition_oidcMemberDefinition,
    memberDefinition_cognitoMemberDefinition,

    -- * MetadataProperties
    MetadataProperties (..),
    newMetadataProperties,
    metadataProperties_commitId,
    metadataProperties_repository,
    metadataProperties_generatedBy,
    metadataProperties_projectId,

    -- * MetricData
    MetricData (..),
    newMetricData,
    metricData_metricName,
    metricData_value,
    metricData_timestamp,

    -- * MetricDatum
    MetricDatum (..),
    newMetricDatum,
    metricDatum_set,
    metricDatum_metricName,
    metricDatum_value,

    -- * MetricDefinition
    MetricDefinition (..),
    newMetricDefinition,
    metricDefinition_name,
    metricDefinition_regex,

    -- * MetricsSource
    MetricsSource (..),
    newMetricsSource,
    metricsSource_contentDigest,
    metricsSource_contentType,
    metricsSource_s3Uri,

    -- * ModelArtifacts
    ModelArtifacts (..),
    newModelArtifacts,
    modelArtifacts_s3ModelArtifacts,

    -- * ModelBiasAppSpecification
    ModelBiasAppSpecification (..),
    newModelBiasAppSpecification,
    modelBiasAppSpecification_environment,
    modelBiasAppSpecification_imageUri,
    modelBiasAppSpecification_configUri,

    -- * ModelBiasBaselineConfig
    ModelBiasBaselineConfig (..),
    newModelBiasBaselineConfig,
    modelBiasBaselineConfig_constraintsResource,
    modelBiasBaselineConfig_baseliningJobName,

    -- * ModelBiasJobInput
    ModelBiasJobInput (..),
    newModelBiasJobInput,
    modelBiasJobInput_endpointInput,
    modelBiasJobInput_groundTruthS3Input,

    -- * ModelClientConfig
    ModelClientConfig (..),
    newModelClientConfig,
    modelClientConfig_invocationsTimeoutInSeconds,
    modelClientConfig_invocationsMaxRetries,

    -- * ModelDataQuality
    ModelDataQuality (..),
    newModelDataQuality,
    modelDataQuality_constraints,
    modelDataQuality_statistics,

    -- * ModelDeployConfig
    ModelDeployConfig (..),
    newModelDeployConfig,
    modelDeployConfig_endpointName,
    modelDeployConfig_autoGenerateEndpointName,

    -- * ModelDeployResult
    ModelDeployResult (..),
    newModelDeployResult,
    modelDeployResult_endpointName,

    -- * ModelDigests
    ModelDigests (..),
    newModelDigests,
    modelDigests_artifactDigest,

    -- * ModelExplainabilityAppSpecification
    ModelExplainabilityAppSpecification (..),
    newModelExplainabilityAppSpecification,
    modelExplainabilityAppSpecification_environment,
    modelExplainabilityAppSpecification_imageUri,
    modelExplainabilityAppSpecification_configUri,

    -- * ModelExplainabilityBaselineConfig
    ModelExplainabilityBaselineConfig (..),
    newModelExplainabilityBaselineConfig,
    modelExplainabilityBaselineConfig_constraintsResource,
    modelExplainabilityBaselineConfig_baseliningJobName,

    -- * ModelExplainabilityJobInput
    ModelExplainabilityJobInput (..),
    newModelExplainabilityJobInput,
    modelExplainabilityJobInput_endpointInput,

    -- * ModelMetrics
    ModelMetrics (..),
    newModelMetrics,
    modelMetrics_bias,
    modelMetrics_modelDataQuality,
    modelMetrics_modelQuality,
    modelMetrics_explainability,

    -- * ModelPackage
    ModelPackage (..),
    newModelPackage,
    modelPackage_creationTime,
    modelPackage_metadataProperties,
    modelPackage_modelApprovalStatus,
    modelPackage_sourceAlgorithmSpecification,
    modelPackage_modelPackageName,
    modelPackage_modelPackageArn,
    modelPackage_modelMetrics,
    modelPackage_modelPackageDescription,
    modelPackage_createdBy,
    modelPackage_lastModifiedTime,
    modelPackage_validationSpecification,
    modelPackage_inferenceSpecification,
    modelPackage_approvalDescription,
    modelPackage_modelPackageVersion,
    modelPackage_certifyForMarketplace,
    modelPackage_modelPackageGroupName,
    modelPackage_lastModifiedBy,
    modelPackage_modelPackageStatusDetails,
    modelPackage_tags,
    modelPackage_modelPackageStatus,

    -- * ModelPackageContainerDefinition
    ModelPackageContainerDefinition (..),
    newModelPackageContainerDefinition,
    modelPackageContainerDefinition_modelDataUrl,
    modelPackageContainerDefinition_environment,
    modelPackageContainerDefinition_imageDigest,
    modelPackageContainerDefinition_containerHostname,
    modelPackageContainerDefinition_productId,
    modelPackageContainerDefinition_image,

    -- * ModelPackageGroup
    ModelPackageGroup (..),
    newModelPackageGroup,
    modelPackageGroup_creationTime,
    modelPackageGroup_modelPackageGroupDescription,
    modelPackageGroup_modelPackageGroupArn,
    modelPackageGroup_createdBy,
    modelPackageGroup_modelPackageGroupName,
    modelPackageGroup_modelPackageGroupStatus,
    modelPackageGroup_tags,

    -- * ModelPackageGroupSummary
    ModelPackageGroupSummary (..),
    newModelPackageGroupSummary,
    modelPackageGroupSummary_modelPackageGroupDescription,
    modelPackageGroupSummary_modelPackageGroupName,
    modelPackageGroupSummary_modelPackageGroupArn,
    modelPackageGroupSummary_creationTime,
    modelPackageGroupSummary_modelPackageGroupStatus,

    -- * ModelPackageStatusDetails
    ModelPackageStatusDetails (..),
    newModelPackageStatusDetails,
    modelPackageStatusDetails_imageScanStatuses,
    modelPackageStatusDetails_validationStatuses,

    -- * ModelPackageStatusItem
    ModelPackageStatusItem (..),
    newModelPackageStatusItem,
    modelPackageStatusItem_failureReason,
    modelPackageStatusItem_name,
    modelPackageStatusItem_status,

    -- * ModelPackageSummary
    ModelPackageSummary (..),
    newModelPackageSummary,
    modelPackageSummary_modelApprovalStatus,
    modelPackageSummary_modelPackageDescription,
    modelPackageSummary_modelPackageVersion,
    modelPackageSummary_modelPackageGroupName,
    modelPackageSummary_modelPackageName,
    modelPackageSummary_modelPackageArn,
    modelPackageSummary_creationTime,
    modelPackageSummary_modelPackageStatus,

    -- * ModelPackageValidationProfile
    ModelPackageValidationProfile (..),
    newModelPackageValidationProfile,
    modelPackageValidationProfile_profileName,
    modelPackageValidationProfile_transformJobDefinition,

    -- * ModelPackageValidationSpecification
    ModelPackageValidationSpecification (..),
    newModelPackageValidationSpecification,
    modelPackageValidationSpecification_validationRole,
    modelPackageValidationSpecification_validationProfiles,

    -- * ModelQuality
    ModelQuality (..),
    newModelQuality,
    modelQuality_constraints,
    modelQuality_statistics,

    -- * ModelQualityAppSpecification
    ModelQualityAppSpecification (..),
    newModelQualityAppSpecification,
    modelQualityAppSpecification_containerArguments,
    modelQualityAppSpecification_recordPreprocessorSourceUri,
    modelQualityAppSpecification_environment,
    modelQualityAppSpecification_problemType,
    modelQualityAppSpecification_containerEntrypoint,
    modelQualityAppSpecification_postAnalyticsProcessorSourceUri,
    modelQualityAppSpecification_imageUri,

    -- * ModelQualityBaselineConfig
    ModelQualityBaselineConfig (..),
    newModelQualityBaselineConfig,
    modelQualityBaselineConfig_constraintsResource,
    modelQualityBaselineConfig_baseliningJobName,

    -- * ModelQualityJobInput
    ModelQualityJobInput (..),
    newModelQualityJobInput,
    modelQualityJobInput_endpointInput,
    modelQualityJobInput_groundTruthS3Input,

    -- * ModelStepMetadata
    ModelStepMetadata (..),
    newModelStepMetadata,
    modelStepMetadata_arn,

    -- * ModelSummary
    ModelSummary (..),
    newModelSummary,
    modelSummary_modelName,
    modelSummary_modelArn,
    modelSummary_creationTime,

    -- * MonitoringAppSpecification
    MonitoringAppSpecification (..),
    newMonitoringAppSpecification,
    monitoringAppSpecification_containerArguments,
    monitoringAppSpecification_recordPreprocessorSourceUri,
    monitoringAppSpecification_containerEntrypoint,
    monitoringAppSpecification_postAnalyticsProcessorSourceUri,
    monitoringAppSpecification_imageUri,

    -- * MonitoringBaselineConfig
    MonitoringBaselineConfig (..),
    newMonitoringBaselineConfig,
    monitoringBaselineConfig_constraintsResource,
    monitoringBaselineConfig_statisticsResource,
    monitoringBaselineConfig_baseliningJobName,

    -- * MonitoringClusterConfig
    MonitoringClusterConfig (..),
    newMonitoringClusterConfig,
    monitoringClusterConfig_volumeKmsKeyId,
    monitoringClusterConfig_instanceCount,
    monitoringClusterConfig_instanceType,
    monitoringClusterConfig_volumeSizeInGB,

    -- * MonitoringConstraintsResource
    MonitoringConstraintsResource (..),
    newMonitoringConstraintsResource,
    monitoringConstraintsResource_s3Uri,

    -- * MonitoringExecutionSummary
    MonitoringExecutionSummary (..),
    newMonitoringExecutionSummary,
    monitoringExecutionSummary_monitoringType,
    monitoringExecutionSummary_failureReason,
    monitoringExecutionSummary_endpointName,
    monitoringExecutionSummary_processingJobArn,
    monitoringExecutionSummary_monitoringJobDefinitionName,
    monitoringExecutionSummary_monitoringScheduleName,
    monitoringExecutionSummary_scheduledTime,
    monitoringExecutionSummary_creationTime,
    monitoringExecutionSummary_lastModifiedTime,
    monitoringExecutionSummary_monitoringExecutionStatus,

    -- * MonitoringGroundTruthS3Input
    MonitoringGroundTruthS3Input (..),
    newMonitoringGroundTruthS3Input,
    monitoringGroundTruthS3Input_s3Uri,

    -- * MonitoringInput
    MonitoringInput (..),
    newMonitoringInput,
    monitoringInput_endpointInput,

    -- * MonitoringJobDefinition
    MonitoringJobDefinition (..),
    newMonitoringJobDefinition,
    monitoringJobDefinition_environment,
    monitoringJobDefinition_stoppingCondition,
    monitoringJobDefinition_networkConfig,
    monitoringJobDefinition_baselineConfig,
    monitoringJobDefinition_monitoringInputs,
    monitoringJobDefinition_monitoringOutputConfig,
    monitoringJobDefinition_monitoringResources,
    monitoringJobDefinition_monitoringAppSpecification,
    monitoringJobDefinition_roleArn,

    -- * MonitoringJobDefinitionSummary
    MonitoringJobDefinitionSummary (..),
    newMonitoringJobDefinitionSummary,
    monitoringJobDefinitionSummary_monitoringJobDefinitionName,
    monitoringJobDefinitionSummary_monitoringJobDefinitionArn,
    monitoringJobDefinitionSummary_creationTime,
    monitoringJobDefinitionSummary_endpointName,

    -- * MonitoringNetworkConfig
    MonitoringNetworkConfig (..),
    newMonitoringNetworkConfig,
    monitoringNetworkConfig_enableNetworkIsolation,
    monitoringNetworkConfig_vpcConfig,
    monitoringNetworkConfig_enableInterContainerTrafficEncryption,

    -- * MonitoringOutput
    MonitoringOutput (..),
    newMonitoringOutput,
    monitoringOutput_s3Output,

    -- * MonitoringOutputConfig
    MonitoringOutputConfig (..),
    newMonitoringOutputConfig,
    monitoringOutputConfig_kmsKeyId,
    monitoringOutputConfig_monitoringOutputs,

    -- * MonitoringResources
    MonitoringResources (..),
    newMonitoringResources,
    monitoringResources_clusterConfig,

    -- * MonitoringS3Output
    MonitoringS3Output (..),
    newMonitoringS3Output,
    monitoringS3Output_s3UploadMode,
    monitoringS3Output_s3Uri,
    monitoringS3Output_localPath,

    -- * MonitoringSchedule
    MonitoringSchedule (..),
    newMonitoringSchedule,
    monitoringSchedule_creationTime,
    monitoringSchedule_monitoringType,
    monitoringSchedule_failureReason,
    monitoringSchedule_monitoringScheduleArn,
    monitoringSchedule_endpointName,
    monitoringSchedule_lastModifiedTime,
    monitoringSchedule_monitoringScheduleStatus,
    monitoringSchedule_lastMonitoringExecutionSummary,
    monitoringSchedule_monitoringScheduleConfig,
    monitoringSchedule_monitoringScheduleName,
    monitoringSchedule_tags,

    -- * MonitoringScheduleConfig
    MonitoringScheduleConfig (..),
    newMonitoringScheduleConfig,
    monitoringScheduleConfig_monitoringType,
    monitoringScheduleConfig_scheduleConfig,
    monitoringScheduleConfig_monitoringJobDefinition,
    monitoringScheduleConfig_monitoringJobDefinitionName,

    -- * MonitoringScheduleSummary
    MonitoringScheduleSummary (..),
    newMonitoringScheduleSummary,
    monitoringScheduleSummary_monitoringType,
    monitoringScheduleSummary_endpointName,
    monitoringScheduleSummary_monitoringJobDefinitionName,
    monitoringScheduleSummary_monitoringScheduleName,
    monitoringScheduleSummary_monitoringScheduleArn,
    monitoringScheduleSummary_creationTime,
    monitoringScheduleSummary_lastModifiedTime,
    monitoringScheduleSummary_monitoringScheduleStatus,

    -- * MonitoringStatisticsResource
    MonitoringStatisticsResource (..),
    newMonitoringStatisticsResource,
    monitoringStatisticsResource_s3Uri,

    -- * MonitoringStoppingCondition
    MonitoringStoppingCondition (..),
    newMonitoringStoppingCondition,
    monitoringStoppingCondition_maxRuntimeInSeconds,

    -- * MultiModelConfig
    MultiModelConfig (..),
    newMultiModelConfig,
    multiModelConfig_modelCacheSetting,

    -- * NeoVpcConfig
    NeoVpcConfig (..),
    newNeoVpcConfig,
    neoVpcConfig_securityGroupIds,
    neoVpcConfig_subnets,

    -- * NestedFilters
    NestedFilters (..),
    newNestedFilters,
    nestedFilters_nestedPropertyName,
    nestedFilters_filters,

    -- * NetworkConfig
    NetworkConfig (..),
    newNetworkConfig,
    networkConfig_enableNetworkIsolation,
    networkConfig_vpcConfig,
    networkConfig_enableInterContainerTrafficEncryption,

    -- * NotebookInstanceLifecycleConfigSummary
    NotebookInstanceLifecycleConfigSummary (..),
    newNotebookInstanceLifecycleConfigSummary,
    notebookInstanceLifecycleConfigSummary_creationTime,
    notebookInstanceLifecycleConfigSummary_lastModifiedTime,
    notebookInstanceLifecycleConfigSummary_notebookInstanceLifecycleConfigName,
    notebookInstanceLifecycleConfigSummary_notebookInstanceLifecycleConfigArn,

    -- * NotebookInstanceLifecycleHook
    NotebookInstanceLifecycleHook (..),
    newNotebookInstanceLifecycleHook,
    notebookInstanceLifecycleHook_content,

    -- * NotebookInstanceSummary
    NotebookInstanceSummary (..),
    newNotebookInstanceSummary,
    notebookInstanceSummary_creationTime,
    notebookInstanceSummary_additionalCodeRepositories,
    notebookInstanceSummary_url,
    notebookInstanceSummary_lastModifiedTime,
    notebookInstanceSummary_instanceType,
    notebookInstanceSummary_notebookInstanceStatus,
    notebookInstanceSummary_defaultCodeRepository,
    notebookInstanceSummary_notebookInstanceLifecycleConfigName,
    notebookInstanceSummary_notebookInstanceName,
    notebookInstanceSummary_notebookInstanceArn,

    -- * NotificationConfiguration
    NotificationConfiguration (..),
    newNotificationConfiguration,
    notificationConfiguration_notificationTopicArn,

    -- * ObjectiveStatusCounters
    ObjectiveStatusCounters (..),
    newObjectiveStatusCounters,
    objectiveStatusCounters_pending,
    objectiveStatusCounters_succeeded,
    objectiveStatusCounters_failed,

    -- * OfflineStoreConfig
    OfflineStoreConfig (..),
    newOfflineStoreConfig,
    offlineStoreConfig_disableGlueTableCreation,
    offlineStoreConfig_dataCatalogConfig,
    offlineStoreConfig_s3StorageConfig,

    -- * OfflineStoreStatus
    OfflineStoreStatus (..),
    newOfflineStoreStatus,
    offlineStoreStatus_blockedReason,
    offlineStoreStatus_status,

    -- * OidcConfig
    OidcConfig (..),
    newOidcConfig,
    oidcConfig_clientId,
    oidcConfig_clientSecret,
    oidcConfig_issuer,
    oidcConfig_authorizationEndpoint,
    oidcConfig_tokenEndpoint,
    oidcConfig_userInfoEndpoint,
    oidcConfig_logoutEndpoint,
    oidcConfig_jwksUri,

    -- * OidcConfigForResponse
    OidcConfigForResponse (..),
    newOidcConfigForResponse,
    oidcConfigForResponse_clientId,
    oidcConfigForResponse_jwksUri,
    oidcConfigForResponse_userInfoEndpoint,
    oidcConfigForResponse_authorizationEndpoint,
    oidcConfigForResponse_tokenEndpoint,
    oidcConfigForResponse_issuer,
    oidcConfigForResponse_logoutEndpoint,

    -- * OidcMemberDefinition
    OidcMemberDefinition (..),
    newOidcMemberDefinition,
    oidcMemberDefinition_groups,

    -- * OnlineStoreConfig
    OnlineStoreConfig (..),
    newOnlineStoreConfig,
    onlineStoreConfig_securityConfig,
    onlineStoreConfig_enableOnlineStore,

    -- * OnlineStoreSecurityConfig
    OnlineStoreSecurityConfig (..),
    newOnlineStoreSecurityConfig,
    onlineStoreSecurityConfig_kmsKeyId,

    -- * OutputConfig
    OutputConfig (..),
    newOutputConfig,
    outputConfig_targetPlatform,
    outputConfig_kmsKeyId,
    outputConfig_compilerOptions,
    outputConfig_targetDevice,
    outputConfig_s3OutputLocation,

    -- * OutputDataConfig
    OutputDataConfig (..),
    newOutputDataConfig,
    outputDataConfig_kmsKeyId,
    outputDataConfig_s3OutputPath,

    -- * OutputParameter
    OutputParameter (..),
    newOutputParameter,
    outputParameter_name,
    outputParameter_value,

    -- * Parameter
    Parameter (..),
    newParameter,
    parameter_name,
    parameter_value,

    -- * ParameterRange
    ParameterRange (..),
    newParameterRange,
    parameterRange_categoricalParameterRangeSpecification,
    parameterRange_integerParameterRangeSpecification,
    parameterRange_continuousParameterRangeSpecification,

    -- * ParameterRanges
    ParameterRanges (..),
    newParameterRanges,
    parameterRanges_categoricalParameterRanges,
    parameterRanges_integerParameterRanges,
    parameterRanges_continuousParameterRanges,

    -- * Parent
    Parent (..),
    newParent,
    parent_experimentName,
    parent_trialName,

    -- * ParentHyperParameterTuningJob
    ParentHyperParameterTuningJob (..),
    newParentHyperParameterTuningJob,
    parentHyperParameterTuningJob_hyperParameterTuningJobName,

    -- * Pipeline
    Pipeline (..),
    newPipeline,
    pipeline_creationTime,
    pipeline_pipelineDisplayName,
    pipeline_pipelineName,
    pipeline_createdBy,
    pipeline_lastRunTime,
    pipeline_lastModifiedTime,
    pipeline_pipelineStatus,
    pipeline_pipelineDescription,
    pipeline_pipelineArn,
    pipeline_lastModifiedBy,
    pipeline_tags,
    pipeline_roleArn,

    -- * PipelineExecution
    PipelineExecution (..),
    newPipelineExecution,
    pipelineExecution_creationTime,
    pipelineExecution_pipelineExecutionStatus,
    pipelineExecution_failureReason,
    pipelineExecution_pipelineExecutionArn,
    pipelineExecution_pipelineParameters,
    pipelineExecution_createdBy,
    pipelineExecution_lastModifiedTime,
    pipelineExecution_pipelineArn,
    pipelineExecution_pipelineExecutionDisplayName,
    pipelineExecution_lastModifiedBy,
    pipelineExecution_pipelineExecutionDescription,
    pipelineExecution_pipelineExperimentConfig,

    -- * PipelineExecutionStep
    PipelineExecutionStep (..),
    newPipelineExecutionStep,
    pipelineExecutionStep_failureReason,
    pipelineExecutionStep_startTime,
    pipelineExecutionStep_stepName,
    pipelineExecutionStep_stepStatus,
    pipelineExecutionStep_endTime,
    pipelineExecutionStep_metadata,
    pipelineExecutionStep_cacheHitResult,

    -- * PipelineExecutionStepMetadata
    PipelineExecutionStepMetadata (..),
    newPipelineExecutionStepMetadata,
    pipelineExecutionStepMetadata_trainingJob,
    pipelineExecutionStepMetadata_processingJob,
    pipelineExecutionStepMetadata_model,
    pipelineExecutionStepMetadata_lambda,
    pipelineExecutionStepMetadata_tuningJob,
    pipelineExecutionStepMetadata_condition,
    pipelineExecutionStepMetadata_transformJob,
    pipelineExecutionStepMetadata_registerModel,
    pipelineExecutionStepMetadata_callback,

    -- * PipelineExecutionSummary
    PipelineExecutionSummary (..),
    newPipelineExecutionSummary,
    pipelineExecutionSummary_pipelineExecutionStatus,
    pipelineExecutionSummary_startTime,
    pipelineExecutionSummary_pipelineExecutionArn,
    pipelineExecutionSummary_pipelineExecutionDisplayName,
    pipelineExecutionSummary_pipelineExecutionDescription,

    -- * PipelineExperimentConfig
    PipelineExperimentConfig (..),
    newPipelineExperimentConfig,
    pipelineExperimentConfig_experimentName,
    pipelineExperimentConfig_trialName,

    -- * PipelineSummary
    PipelineSummary (..),
    newPipelineSummary,
    pipelineSummary_creationTime,
    pipelineSummary_pipelineDisplayName,
    pipelineSummary_lastExecutionTime,
    pipelineSummary_pipelineName,
    pipelineSummary_lastModifiedTime,
    pipelineSummary_pipelineDescription,
    pipelineSummary_pipelineArn,
    pipelineSummary_roleArn,

    -- * ProcessingClusterConfig
    ProcessingClusterConfig (..),
    newProcessingClusterConfig,
    processingClusterConfig_volumeKmsKeyId,
    processingClusterConfig_instanceCount,
    processingClusterConfig_instanceType,
    processingClusterConfig_volumeSizeInGB,

    -- * ProcessingFeatureStoreOutput
    ProcessingFeatureStoreOutput (..),
    newProcessingFeatureStoreOutput,
    processingFeatureStoreOutput_featureGroupName,

    -- * ProcessingInput
    ProcessingInput (..),
    newProcessingInput,
    processingInput_datasetDefinition,
    processingInput_appManaged,
    processingInput_s3Input,
    processingInput_inputName,

    -- * ProcessingJob
    ProcessingJob (..),
    newProcessingJob,
    processingJob_creationTime,
    processingJob_failureReason,
    processingJob_monitoringScheduleArn,
    processingJob_appSpecification,
    processingJob_processingResources,
    processingJob_environment,
    processingJob_processingJobName,
    processingJob_stoppingCondition,
    processingJob_experimentConfig,
    processingJob_lastModifiedTime,
    processingJob_processingInputs,
    processingJob_networkConfig,
    processingJob_autoMLJobArn,
    processingJob_trainingJobArn,
    processingJob_processingJobStatus,
    processingJob_exitMessage,
    processingJob_processingOutputConfig,
    processingJob_processingStartTime,
    processingJob_processingEndTime,
    processingJob_tags,
    processingJob_processingJobArn,
    processingJob_roleArn,

    -- * ProcessingJobStepMetadata
    ProcessingJobStepMetadata (..),
    newProcessingJobStepMetadata,
    processingJobStepMetadata_arn,

    -- * ProcessingJobSummary
    ProcessingJobSummary (..),
    newProcessingJobSummary,
    processingJobSummary_failureReason,
    processingJobSummary_lastModifiedTime,
    processingJobSummary_exitMessage,
    processingJobSummary_processingEndTime,
    processingJobSummary_processingJobName,
    processingJobSummary_processingJobArn,
    processingJobSummary_creationTime,
    processingJobSummary_processingJobStatus,

    -- * ProcessingOutput
    ProcessingOutput (..),
    newProcessingOutput,
    processingOutput_featureStoreOutput,
    processingOutput_s3Output,
    processingOutput_appManaged,
    processingOutput_outputName,

    -- * ProcessingOutputConfig
    ProcessingOutputConfig (..),
    newProcessingOutputConfig,
    processingOutputConfig_kmsKeyId,
    processingOutputConfig_outputs,

    -- * ProcessingResources
    ProcessingResources (..),
    newProcessingResources,
    processingResources_clusterConfig,

    -- * ProcessingS3Input
    ProcessingS3Input (..),
    newProcessingS3Input,
    processingS3Input_s3DataDistributionType,
    processingS3Input_s3InputMode,
    processingS3Input_localPath,
    processingS3Input_s3CompressionType,
    processingS3Input_s3Uri,
    processingS3Input_s3DataType,

    -- * ProcessingS3Output
    ProcessingS3Output (..),
    newProcessingS3Output,
    processingS3Output_s3Uri,
    processingS3Output_localPath,
    processingS3Output_s3UploadMode,

    -- * ProcessingStoppingCondition
    ProcessingStoppingCondition (..),
    newProcessingStoppingCondition,
    processingStoppingCondition_maxRuntimeInSeconds,

    -- * ProductionVariant
    ProductionVariant (..),
    newProductionVariant,
    productionVariant_acceleratorType,
    productionVariant_coreDumpConfig,
    productionVariant_initialVariantWeight,
    productionVariant_variantName,
    productionVariant_modelName,
    productionVariant_initialInstanceCount,
    productionVariant_instanceType,

    -- * ProductionVariantCoreDumpConfig
    ProductionVariantCoreDumpConfig (..),
    newProductionVariantCoreDumpConfig,
    productionVariantCoreDumpConfig_kmsKeyId,
    productionVariantCoreDumpConfig_destinationS3Uri,

    -- * ProductionVariantSummary
    ProductionVariantSummary (..),
    newProductionVariantSummary,
    productionVariantSummary_desiredInstanceCount,
    productionVariantSummary_desiredWeight,
    productionVariantSummary_currentWeight,
    productionVariantSummary_currentInstanceCount,
    productionVariantSummary_deployedImages,
    productionVariantSummary_variantName,

    -- * ProfilerConfig
    ProfilerConfig (..),
    newProfilerConfig,
    profilerConfig_profilingParameters,
    profilerConfig_profilingIntervalInMilliseconds,
    profilerConfig_s3OutputPath,

    -- * ProfilerConfigForUpdate
    ProfilerConfigForUpdate (..),
    newProfilerConfigForUpdate,
    profilerConfigForUpdate_profilingParameters,
    profilerConfigForUpdate_s3OutputPath,
    profilerConfigForUpdate_profilingIntervalInMilliseconds,
    profilerConfigForUpdate_disableProfiler,

    -- * ProfilerRuleConfiguration
    ProfilerRuleConfiguration (..),
    newProfilerRuleConfiguration,
    profilerRuleConfiguration_ruleParameters,
    profilerRuleConfiguration_s3OutputPath,
    profilerRuleConfiguration_localPath,
    profilerRuleConfiguration_instanceType,
    profilerRuleConfiguration_volumeSizeInGB,
    profilerRuleConfiguration_ruleConfigurationName,
    profilerRuleConfiguration_ruleEvaluatorImage,

    -- * ProfilerRuleEvaluationStatus
    ProfilerRuleEvaluationStatus (..),
    newProfilerRuleEvaluationStatus,
    profilerRuleEvaluationStatus_lastModifiedTime,
    profilerRuleEvaluationStatus_statusDetails,
    profilerRuleEvaluationStatus_ruleEvaluationStatus,
    profilerRuleEvaluationStatus_ruleEvaluationJobArn,
    profilerRuleEvaluationStatus_ruleConfigurationName,

    -- * Project
    Project (..),
    newProject,
    project_creationTime,
    project_serviceCatalogProvisionedProductDetails,
    project_createdBy,
    project_projectStatus,
    project_projectName,
    project_serviceCatalogProvisioningDetails,
    project_projectId,
    project_projectArn,
    project_projectDescription,
    project_tags,

    -- * ProjectSummary
    ProjectSummary (..),
    newProjectSummary,
    projectSummary_projectDescription,
    projectSummary_projectName,
    projectSummary_projectArn,
    projectSummary_projectId,
    projectSummary_creationTime,
    projectSummary_projectStatus,

    -- * PropertyNameQuery
    PropertyNameQuery (..),
    newPropertyNameQuery,
    propertyNameQuery_propertyNameHint,

    -- * PropertyNameSuggestion
    PropertyNameSuggestion (..),
    newPropertyNameSuggestion,
    propertyNameSuggestion_propertyName,

    -- * ProvisioningParameter
    ProvisioningParameter (..),
    newProvisioningParameter,
    provisioningParameter_value,
    provisioningParameter_key,

    -- * PublicWorkforceTaskPrice
    PublicWorkforceTaskPrice (..),
    newPublicWorkforceTaskPrice,
    publicWorkforceTaskPrice_amountInUsd,

    -- * RedshiftDatasetDefinition
    RedshiftDatasetDefinition (..),
    newRedshiftDatasetDefinition,
    redshiftDatasetDefinition_kmsKeyId,
    redshiftDatasetDefinition_outputCompression,
    redshiftDatasetDefinition_clusterId,
    redshiftDatasetDefinition_database,
    redshiftDatasetDefinition_dbUser,
    redshiftDatasetDefinition_queryString,
    redshiftDatasetDefinition_clusterRoleArn,
    redshiftDatasetDefinition_outputS3Uri,
    redshiftDatasetDefinition_outputFormat,

    -- * RegisterModelStepMetadata
    RegisterModelStepMetadata (..),
    newRegisterModelStepMetadata,
    registerModelStepMetadata_arn,

    -- * RenderableTask
    RenderableTask (..),
    newRenderableTask,
    renderableTask_input,

    -- * RenderingError
    RenderingError (..),
    newRenderingError,
    renderingError_code,
    renderingError_message,

    -- * RepositoryAuthConfig
    RepositoryAuthConfig (..),
    newRepositoryAuthConfig,
    repositoryAuthConfig_repositoryCredentialsProviderArn,

    -- * ResolvedAttributes
    ResolvedAttributes (..),
    newResolvedAttributes,
    resolvedAttributes_problemType,
    resolvedAttributes_autoMLJobObjective,
    resolvedAttributes_completionCriteria,

    -- * ResourceConfig
    ResourceConfig (..),
    newResourceConfig,
    resourceConfig_volumeKmsKeyId,
    resourceConfig_instanceType,
    resourceConfig_instanceCount,
    resourceConfig_volumeSizeInGB,

    -- * ResourceLimits
    ResourceLimits (..),
    newResourceLimits,
    resourceLimits_maxNumberOfTrainingJobs,
    resourceLimits_maxParallelTrainingJobs,

    -- * ResourceSpec
    ResourceSpec (..),
    newResourceSpec,
    resourceSpec_instanceType,
    resourceSpec_sageMakerImageArn,
    resourceSpec_sageMakerImageVersionArn,
    resourceSpec_lifecycleConfigArn,

    -- * RetentionPolicy
    RetentionPolicy (..),
    newRetentionPolicy,
    retentionPolicy_homeEfsFileSystem,

    -- * RetryStrategy
    RetryStrategy (..),
    newRetryStrategy,
    retryStrategy_maximumRetryAttempts,

    -- * S3DataSource
    S3DataSource (..),
    newS3DataSource,
    s3DataSource_s3DataDistributionType,
    s3DataSource_attributeNames,
    s3DataSource_s3DataType,
    s3DataSource_s3Uri,

    -- * S3StorageConfig
    S3StorageConfig (..),
    newS3StorageConfig,
    s3StorageConfig_resolvedOutputS3Uri,
    s3StorageConfig_kmsKeyId,
    s3StorageConfig_s3Uri,

    -- * ScheduleConfig
    ScheduleConfig (..),
    newScheduleConfig,
    scheduleConfig_scheduleExpression,

    -- * SearchExpression
    SearchExpression (..),
    newSearchExpression,
    searchExpression_subExpressions,
    searchExpression_operator,
    searchExpression_filters,
    searchExpression_nestedFilters,

    -- * SearchRecord
    SearchRecord (..),
    newSearchRecord,
    searchRecord_trainingJob,
    searchRecord_trial,
    searchRecord_modelPackageGroup,
    searchRecord_trialComponent,
    searchRecord_project,
    searchRecord_pipelineExecution,
    searchRecord_featureGroup,
    searchRecord_experiment,
    searchRecord_pipeline,
    searchRecord_modelPackage,
    searchRecord_endpoint,

    -- * SecondaryStatusTransition
    SecondaryStatusTransition (..),
    newSecondaryStatusTransition,
    secondaryStatusTransition_statusMessage,
    secondaryStatusTransition_endTime,
    secondaryStatusTransition_status,
    secondaryStatusTransition_startTime,

    -- * ServiceCatalogProvisionedProductDetails
    ServiceCatalogProvisionedProductDetails (..),
    newServiceCatalogProvisionedProductDetails,
    serviceCatalogProvisionedProductDetails_provisionedProductStatusMessage,
    serviceCatalogProvisionedProductDetails_provisionedProductId,

    -- * ServiceCatalogProvisioningDetails
    ServiceCatalogProvisioningDetails (..),
    newServiceCatalogProvisioningDetails,
    serviceCatalogProvisioningDetails_provisioningArtifactId,
    serviceCatalogProvisioningDetails_pathId,
    serviceCatalogProvisioningDetails_provisioningParameters,
    serviceCatalogProvisioningDetails_productId,

    -- * SharingSettings
    SharingSettings (..),
    newSharingSettings,
    sharingSettings_s3KmsKeyId,
    sharingSettings_s3OutputPath,
    sharingSettings_notebookOutputOption,

    -- * ShuffleConfig
    ShuffleConfig (..),
    newShuffleConfig,
    shuffleConfig_seed,

    -- * SourceAlgorithm
    SourceAlgorithm (..),
    newSourceAlgorithm,
    sourceAlgorithm_modelDataUrl,
    sourceAlgorithm_algorithmName,

    -- * SourceAlgorithmSpecification
    SourceAlgorithmSpecification (..),
    newSourceAlgorithmSpecification,
    sourceAlgorithmSpecification_sourceAlgorithms,

    -- * SourceIpConfig
    SourceIpConfig (..),
    newSourceIpConfig,
    sourceIpConfig_cidrs,

    -- * StoppingCondition
    StoppingCondition (..),
    newStoppingCondition,
    stoppingCondition_maxWaitTimeInSeconds,
    stoppingCondition_maxRuntimeInSeconds,

    -- * StudioLifecycleConfigDetails
    StudioLifecycleConfigDetails (..),
    newStudioLifecycleConfigDetails,
    studioLifecycleConfigDetails_creationTime,
    studioLifecycleConfigDetails_lastModifiedTime,
    studioLifecycleConfigDetails_studioLifecycleConfigArn,
    studioLifecycleConfigDetails_studioLifecycleConfigAppType,
    studioLifecycleConfigDetails_studioLifecycleConfigName,

    -- * SubscribedWorkteam
    SubscribedWorkteam (..),
    newSubscribedWorkteam,
    subscribedWorkteam_marketplaceTitle,
    subscribedWorkteam_sellerName,
    subscribedWorkteam_listingId,
    subscribedWorkteam_marketplaceDescription,
    subscribedWorkteam_workteamArn,

    -- * SuggestionQuery
    SuggestionQuery (..),
    newSuggestionQuery,
    suggestionQuery_propertyNameQuery,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TargetPlatform
    TargetPlatform (..),
    newTargetPlatform,
    targetPlatform_accelerator,
    targetPlatform_os,
    targetPlatform_arch,

    -- * TensorBoardAppSettings
    TensorBoardAppSettings (..),
    newTensorBoardAppSettings,
    tensorBoardAppSettings_defaultResourceSpec,

    -- * TensorBoardOutputConfig
    TensorBoardOutputConfig (..),
    newTensorBoardOutputConfig,
    tensorBoardOutputConfig_localPath,
    tensorBoardOutputConfig_s3OutputPath,

    -- * TrafficRoutingConfig
    TrafficRoutingConfig (..),
    newTrafficRoutingConfig,
    trafficRoutingConfig_canarySize,
    trafficRoutingConfig_type,
    trafficRoutingConfig_waitIntervalInSeconds,

    -- * TrainingJob
    TrainingJob (..),
    newTrainingJob,
    trainingJob_creationTime,
    trainingJob_labelingJobArn,
    trainingJob_failureReason,
    trainingJob_secondaryStatusTransitions,
    trainingJob_modelArtifacts,
    trainingJob_trainingEndTime,
    trainingJob_environment,
    trainingJob_billableTimeInSeconds,
    trainingJob_debugHookConfig,
    trainingJob_checkpointConfig,
    trainingJob_retryStrategy,
    trainingJob_stoppingCondition,
    trainingJob_debugRuleEvaluationStatuses,
    trainingJob_trainingJobStatus,
    trainingJob_enableNetworkIsolation,
    trainingJob_experimentConfig,
    trainingJob_lastModifiedTime,
    trainingJob_debugRuleConfigurations,
    trainingJob_enableManagedSpotTraining,
    trainingJob_autoMLJobArn,
    trainingJob_hyperParameters,
    trainingJob_inputDataConfig,
    trainingJob_vpcConfig,
    trainingJob_trainingJobArn,
    trainingJob_algorithmSpecification,
    trainingJob_finalMetricDataList,
    trainingJob_outputDataConfig,
    trainingJob_trainingStartTime,
    trainingJob_tuningJobArn,
    trainingJob_trainingJobName,
    trainingJob_resourceConfig,
    trainingJob_enableInterContainerTrafficEncryption,
    trainingJob_tensorBoardOutputConfig,
    trainingJob_secondaryStatus,
    trainingJob_tags,
    trainingJob_trainingTimeInSeconds,
    trainingJob_roleArn,

    -- * TrainingJobDefinition
    TrainingJobDefinition (..),
    newTrainingJobDefinition,
    trainingJobDefinition_hyperParameters,
    trainingJobDefinition_trainingInputMode,
    trainingJobDefinition_inputDataConfig,
    trainingJobDefinition_outputDataConfig,
    trainingJobDefinition_resourceConfig,
    trainingJobDefinition_stoppingCondition,

    -- * TrainingJobStatusCounters
    TrainingJobStatusCounters (..),
    newTrainingJobStatusCounters,
    trainingJobStatusCounters_stopped,
    trainingJobStatusCounters_retryableError,
    trainingJobStatusCounters_inProgress,
    trainingJobStatusCounters_nonRetryableError,
    trainingJobStatusCounters_completed,

    -- * TrainingJobStepMetadata
    TrainingJobStepMetadata (..),
    newTrainingJobStepMetadata,
    trainingJobStepMetadata_arn,

    -- * TrainingJobSummary
    TrainingJobSummary (..),
    newTrainingJobSummary,
    trainingJobSummary_trainingEndTime,
    trainingJobSummary_lastModifiedTime,
    trainingJobSummary_trainingJobName,
    trainingJobSummary_trainingJobArn,
    trainingJobSummary_creationTime,
    trainingJobSummary_trainingJobStatus,

    -- * TrainingSpecification
    TrainingSpecification (..),
    newTrainingSpecification,
    trainingSpecification_trainingImageDigest,
    trainingSpecification_supportsDistributedTraining,
    trainingSpecification_supportedHyperParameters,
    trainingSpecification_supportedTuningJobObjectiveMetrics,
    trainingSpecification_metricDefinitions,
    trainingSpecification_trainingImage,
    trainingSpecification_supportedTrainingInstanceTypes,
    trainingSpecification_trainingChannels,

    -- * TransformDataSource
    TransformDataSource (..),
    newTransformDataSource,
    transformDataSource_s3DataSource,

    -- * TransformInput
    TransformInput (..),
    newTransformInput,
    transformInput_splitType,
    transformInput_compressionType,
    transformInput_contentType,
    transformInput_dataSource,

    -- * TransformJob
    TransformJob (..),
    newTransformJob,
    transformJob_creationTime,
    transformJob_labelingJobArn,
    transformJob_transformJobName,
    transformJob_failureReason,
    transformJob_modelClientConfig,
    transformJob_batchStrategy,
    transformJob_maxPayloadInMB,
    transformJob_environment,
    transformJob_transformResources,
    transformJob_modelName,
    transformJob_experimentConfig,
    transformJob_transformEndTime,
    transformJob_transformStartTime,
    transformJob_autoMLJobArn,
    transformJob_transformJobStatus,
    transformJob_transformInput,
    transformJob_maxConcurrentTransforms,
    transformJob_transformOutput,
    transformJob_dataProcessing,
    transformJob_transformJobArn,
    transformJob_tags,

    -- * TransformJobDefinition
    TransformJobDefinition (..),
    newTransformJobDefinition,
    transformJobDefinition_batchStrategy,
    transformJobDefinition_maxPayloadInMB,
    transformJobDefinition_environment,
    transformJobDefinition_maxConcurrentTransforms,
    transformJobDefinition_transformInput,
    transformJobDefinition_transformOutput,
    transformJobDefinition_transformResources,

    -- * TransformJobStepMetadata
    TransformJobStepMetadata (..),
    newTransformJobStepMetadata,
    transformJobStepMetadata_arn,

    -- * TransformJobSummary
    TransformJobSummary (..),
    newTransformJobSummary,
    transformJobSummary_failureReason,
    transformJobSummary_lastModifiedTime,
    transformJobSummary_transformEndTime,
    transformJobSummary_transformJobName,
    transformJobSummary_transformJobArn,
    transformJobSummary_creationTime,
    transformJobSummary_transformJobStatus,

    -- * TransformOutput
    TransformOutput (..),
    newTransformOutput,
    transformOutput_assembleWith,
    transformOutput_accept,
    transformOutput_kmsKeyId,
    transformOutput_s3OutputPath,

    -- * TransformResources
    TransformResources (..),
    newTransformResources,
    transformResources_volumeKmsKeyId,
    transformResources_instanceType,
    transformResources_instanceCount,

    -- * TransformS3DataSource
    TransformS3DataSource (..),
    newTransformS3DataSource,
    transformS3DataSource_s3DataType,
    transformS3DataSource_s3Uri,

    -- * Trial
    Trial (..),
    newTrial,
    trial_creationTime,
    trial_metadataProperties,
    trial_trialComponentSummaries,
    trial_trialArn,
    trial_createdBy,
    trial_lastModifiedTime,
    trial_experimentName,
    trial_source,
    trial_displayName,
    trial_trialName,
    trial_lastModifiedBy,
    trial_tags,

    -- * TrialComponent
    TrialComponent (..),
    newTrialComponent,
    trialComponent_creationTime,
    trialComponent_metadataProperties,
    trialComponent_status,
    trialComponent_sourceDetail,
    trialComponent_metrics,
    trialComponent_outputArtifacts,
    trialComponent_startTime,
    trialComponent_createdBy,
    trialComponent_lastModifiedTime,
    trialComponent_parents,
    trialComponent_endTime,
    trialComponent_trialComponentName,
    trialComponent_parameters,
    trialComponent_source,
    trialComponent_displayName,
    trialComponent_lastModifiedBy,
    trialComponent_trialComponentArn,
    trialComponent_inputArtifacts,
    trialComponent_tags,

    -- * TrialComponentArtifact
    TrialComponentArtifact (..),
    newTrialComponentArtifact,
    trialComponentArtifact_mediaType,
    trialComponentArtifact_value,

    -- * TrialComponentMetricSummary
    TrialComponentMetricSummary (..),
    newTrialComponentMetricSummary,
    trialComponentMetricSummary_max,
    trialComponentMetricSummary_sourceArn,
    trialComponentMetricSummary_avg,
    trialComponentMetricSummary_count,
    trialComponentMetricSummary_metricName,
    trialComponentMetricSummary_stdDev,
    trialComponentMetricSummary_min,
    trialComponentMetricSummary_last,
    trialComponentMetricSummary_timeStamp,

    -- * TrialComponentParameterValue
    TrialComponentParameterValue (..),
    newTrialComponentParameterValue,
    trialComponentParameterValue_numberValue,
    trialComponentParameterValue_stringValue,

    -- * TrialComponentSimpleSummary
    TrialComponentSimpleSummary (..),
    newTrialComponentSimpleSummary,
    trialComponentSimpleSummary_creationTime,
    trialComponentSimpleSummary_createdBy,
    trialComponentSimpleSummary_trialComponentName,
    trialComponentSimpleSummary_trialComponentArn,
    trialComponentSimpleSummary_trialComponentSource,

    -- * TrialComponentSource
    TrialComponentSource (..),
    newTrialComponentSource,
    trialComponentSource_sourceType,
    trialComponentSource_sourceArn,

    -- * TrialComponentSourceDetail
    TrialComponentSourceDetail (..),
    newTrialComponentSourceDetail,
    trialComponentSourceDetail_trainingJob,
    trialComponentSourceDetail_sourceArn,
    trialComponentSourceDetail_processingJob,
    trialComponentSourceDetail_transformJob,

    -- * TrialComponentStatus
    TrialComponentStatus (..),
    newTrialComponentStatus,
    trialComponentStatus_primaryStatus,
    trialComponentStatus_message,

    -- * TrialComponentSummary
    TrialComponentSummary (..),
    newTrialComponentSummary,
    trialComponentSummary_creationTime,
    trialComponentSummary_status,
    trialComponentSummary_startTime,
    trialComponentSummary_createdBy,
    trialComponentSummary_lastModifiedTime,
    trialComponentSummary_endTime,
    trialComponentSummary_trialComponentName,
    trialComponentSummary_displayName,
    trialComponentSummary_lastModifiedBy,
    trialComponentSummary_trialComponentArn,
    trialComponentSummary_trialComponentSource,

    -- * TrialSource
    TrialSource (..),
    newTrialSource,
    trialSource_sourceType,
    trialSource_sourceArn,

    -- * TrialSummary
    TrialSummary (..),
    newTrialSummary,
    trialSummary_creationTime,
    trialSummary_trialArn,
    trialSummary_lastModifiedTime,
    trialSummary_trialSource,
    trialSummary_displayName,
    trialSummary_trialName,

    -- * TuningJobCompletionCriteria
    TuningJobCompletionCriteria (..),
    newTuningJobCompletionCriteria,
    tuningJobCompletionCriteria_targetObjectiveMetricValue,

    -- * TuningJobStepMetaData
    TuningJobStepMetaData (..),
    newTuningJobStepMetaData,
    tuningJobStepMetaData_arn,

    -- * USD
    USD (..),
    newUSD,
    usd_cents,
    usd_dollars,
    usd_tenthFractionsOfACent,

    -- * UiConfig
    UiConfig (..),
    newUiConfig,
    uiConfig_uiTemplateS3Uri,
    uiConfig_humanTaskUiArn,

    -- * UiTemplate
    UiTemplate (..),
    newUiTemplate,
    uiTemplate_content,

    -- * UiTemplateInfo
    UiTemplateInfo (..),
    newUiTemplateInfo,
    uiTemplateInfo_url,
    uiTemplateInfo_contentSha256,

    -- * UserContext
    UserContext (..),
    newUserContext,
    userContext_userProfileName,
    userContext_userProfileArn,
    userContext_domainId,

    -- * UserProfileDetails
    UserProfileDetails (..),
    newUserProfileDetails,
    userProfileDetails_creationTime,
    userProfileDetails_status,
    userProfileDetails_userProfileName,
    userProfileDetails_lastModifiedTime,
    userProfileDetails_domainId,

    -- * UserSettings
    UserSettings (..),
    newUserSettings,
    userSettings_tensorBoardAppSettings,
    userSettings_kernelGatewayAppSettings,
    userSettings_securityGroups,
    userSettings_jupyterServerAppSettings,
    userSettings_sharingSettings,
    userSettings_executionRole,

    -- * VariantProperty
    VariantProperty (..),
    newVariantProperty,
    variantProperty_variantPropertyType,

    -- * VpcConfig
    VpcConfig (..),
    newVpcConfig,
    vpcConfig_securityGroupIds,
    vpcConfig_subnets,

    -- * Workforce
    Workforce (..),
    newWorkforce,
    workforce_subDomain,
    workforce_createDate,
    workforce_sourceIpConfig,
    workforce_cognitoConfig,
    workforce_lastUpdatedDate,
    workforce_oidcConfig,
    workforce_workforceName,
    workforce_workforceArn,

    -- * Workteam
    Workteam (..),
    newWorkteam,
    workteam_subDomain,
    workteam_productListingIds,
    workteam_notificationConfiguration,
    workteam_createDate,
    workteam_workforceArn,
    workteam_lastUpdatedDate,
    workteam_workteamName,
    workteam_memberDefinitions,
    workteam_workteamArn,
    workteam_description,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ActionSource
import Network.AWS.SageMaker.Types.ActionStatus
import Network.AWS.SageMaker.Types.ActionSummary
import Network.AWS.SageMaker.Types.AgentVersion
import Network.AWS.SageMaker.Types.Alarm
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
import Network.AWS.SageMaker.Types.ArtifactSource
import Network.AWS.SageMaker.Types.ArtifactSourceIdType
import Network.AWS.SageMaker.Types.ArtifactSourceType
import Network.AWS.SageMaker.Types.ArtifactSummary
import Network.AWS.SageMaker.Types.AssemblyType
import Network.AWS.SageMaker.Types.AssociationEdgeType
import Network.AWS.SageMaker.Types.AssociationSummary
import Network.AWS.SageMaker.Types.AsyncInferenceClientConfig
import Network.AWS.SageMaker.Types.AsyncInferenceConfig
import Network.AWS.SageMaker.Types.AsyncInferenceNotificationConfig
import Network.AWS.SageMaker.Types.AsyncInferenceOutputConfig
import Network.AWS.SageMaker.Types.AthenaDatasetDefinition
import Network.AWS.SageMaker.Types.AthenaResultCompressionType
import Network.AWS.SageMaker.Types.AthenaResultFormat
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
import Network.AWS.SageMaker.Types.AutoMLPartialFailureReason
import Network.AWS.SageMaker.Types.AutoMLS3DataSource
import Network.AWS.SageMaker.Types.AutoMLS3DataType
import Network.AWS.SageMaker.Types.AutoMLSecurityConfig
import Network.AWS.SageMaker.Types.AutoMLSortBy
import Network.AWS.SageMaker.Types.AutoMLSortOrder
import Network.AWS.SageMaker.Types.AutoRollbackConfig
import Network.AWS.SageMaker.Types.AwsManagedHumanLoopRequestSource
import Network.AWS.SageMaker.Types.BatchStrategy
import Network.AWS.SageMaker.Types.Bias
import Network.AWS.SageMaker.Types.BlueGreenUpdatePolicy
import Network.AWS.SageMaker.Types.BooleanOperator
import Network.AWS.SageMaker.Types.CacheHitResult
import Network.AWS.SageMaker.Types.CallbackStepMetadata
import Network.AWS.SageMaker.Types.CandidateArtifactLocations
import Network.AWS.SageMaker.Types.CandidateProperties
import Network.AWS.SageMaker.Types.CandidateSortBy
import Network.AWS.SageMaker.Types.CandidateStatus
import Network.AWS.SageMaker.Types.CandidateStepType
import Network.AWS.SageMaker.Types.CapacitySize
import Network.AWS.SageMaker.Types.CapacitySizeType
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
import Network.AWS.SageMaker.Types.ConditionOutcome
import Network.AWS.SageMaker.Types.ConditionStepMetadata
import Network.AWS.SageMaker.Types.ContainerDefinition
import Network.AWS.SageMaker.Types.ContainerMode
import Network.AWS.SageMaker.Types.ContentClassifier
import Network.AWS.SageMaker.Types.ContextSource
import Network.AWS.SageMaker.Types.ContextSummary
import Network.AWS.SageMaker.Types.ContinuousParameterRange
import Network.AWS.SageMaker.Types.ContinuousParameterRangeSpecification
import Network.AWS.SageMaker.Types.CustomImage
import Network.AWS.SageMaker.Types.DataCaptureConfig
import Network.AWS.SageMaker.Types.DataCaptureConfigSummary
import Network.AWS.SageMaker.Types.DataCatalogConfig
import Network.AWS.SageMaker.Types.DataDistributionType
import Network.AWS.SageMaker.Types.DataProcessing
import Network.AWS.SageMaker.Types.DataQualityAppSpecification
import Network.AWS.SageMaker.Types.DataQualityBaselineConfig
import Network.AWS.SageMaker.Types.DataQualityJobInput
import Network.AWS.SageMaker.Types.DataSource
import Network.AWS.SageMaker.Types.DatasetDefinition
import Network.AWS.SageMaker.Types.DebugHookConfig
import Network.AWS.SageMaker.Types.DebugRuleConfiguration
import Network.AWS.SageMaker.Types.DebugRuleEvaluationStatus
import Network.AWS.SageMaker.Types.DeployedImage
import Network.AWS.SageMaker.Types.DeploymentConfig
import Network.AWS.SageMaker.Types.DesiredWeightAndCapacity
import Network.AWS.SageMaker.Types.DetailedAlgorithmStatus
import Network.AWS.SageMaker.Types.DetailedModelPackageStatus
import Network.AWS.SageMaker.Types.Device
import Network.AWS.SageMaker.Types.DeviceFleetSummary
import Network.AWS.SageMaker.Types.DeviceStats
import Network.AWS.SageMaker.Types.DeviceSummary
import Network.AWS.SageMaker.Types.DirectInternetAccess
import Network.AWS.SageMaker.Types.DomainDetails
import Network.AWS.SageMaker.Types.DomainStatus
import Network.AWS.SageMaker.Types.EdgeModel
import Network.AWS.SageMaker.Types.EdgeModelStat
import Network.AWS.SageMaker.Types.EdgeModelSummary
import Network.AWS.SageMaker.Types.EdgeOutputConfig
import Network.AWS.SageMaker.Types.EdgePackagingJobStatus
import Network.AWS.SageMaker.Types.EdgePackagingJobSummary
import Network.AWS.SageMaker.Types.EdgePresetDeploymentOutput
import Network.AWS.SageMaker.Types.EdgePresetDeploymentStatus
import Network.AWS.SageMaker.Types.EdgePresetDeploymentType
import Network.AWS.SageMaker.Types.Endpoint
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
import Network.AWS.SageMaker.Types.Explainability
import Network.AWS.SageMaker.Types.FeatureDefinition
import Network.AWS.SageMaker.Types.FeatureGroup
import Network.AWS.SageMaker.Types.FeatureGroupSortBy
import Network.AWS.SageMaker.Types.FeatureGroupSortOrder
import Network.AWS.SageMaker.Types.FeatureGroupStatus
import Network.AWS.SageMaker.Types.FeatureGroupSummary
import Network.AWS.SageMaker.Types.FeatureType
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
import Network.AWS.SageMaker.Types.InferenceExecutionConfig
import Network.AWS.SageMaker.Types.InferenceExecutionMode
import Network.AWS.SageMaker.Types.InferenceSpecification
import Network.AWS.SageMaker.Types.InputConfig
import Network.AWS.SageMaker.Types.InputMode
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
import Network.AWS.SageMaker.Types.LabelingJobSnsDataSource
import Network.AWS.SageMaker.Types.LabelingJobStatus
import Network.AWS.SageMaker.Types.LabelingJobStoppingConditions
import Network.AWS.SageMaker.Types.LabelingJobSummary
import Network.AWS.SageMaker.Types.LambdaStepMetadata
import Network.AWS.SageMaker.Types.ListCompilationJobsSortBy
import Network.AWS.SageMaker.Types.ListDeviceFleetsSortBy
import Network.AWS.SageMaker.Types.ListEdgePackagingJobsSortBy
import Network.AWS.SageMaker.Types.ListLabelingJobsForWorkteamSortByOptions
import Network.AWS.SageMaker.Types.ListWorkforcesSortByOptions
import Network.AWS.SageMaker.Types.ListWorkteamsSortByOptions
import Network.AWS.SageMaker.Types.MemberDefinition
import Network.AWS.SageMaker.Types.MetadataProperties
import Network.AWS.SageMaker.Types.MetricData
import Network.AWS.SageMaker.Types.MetricDatum
import Network.AWS.SageMaker.Types.MetricDefinition
import Network.AWS.SageMaker.Types.MetricSetSource
import Network.AWS.SageMaker.Types.MetricsSource
import Network.AWS.SageMaker.Types.ModelApprovalStatus
import Network.AWS.SageMaker.Types.ModelArtifacts
import Network.AWS.SageMaker.Types.ModelBiasAppSpecification
import Network.AWS.SageMaker.Types.ModelBiasBaselineConfig
import Network.AWS.SageMaker.Types.ModelBiasJobInput
import Network.AWS.SageMaker.Types.ModelCacheSetting
import Network.AWS.SageMaker.Types.ModelClientConfig
import Network.AWS.SageMaker.Types.ModelDataQuality
import Network.AWS.SageMaker.Types.ModelDeployConfig
import Network.AWS.SageMaker.Types.ModelDeployResult
import Network.AWS.SageMaker.Types.ModelDigests
import Network.AWS.SageMaker.Types.ModelExplainabilityAppSpecification
import Network.AWS.SageMaker.Types.ModelExplainabilityBaselineConfig
import Network.AWS.SageMaker.Types.ModelExplainabilityJobInput
import Network.AWS.SageMaker.Types.ModelMetrics
import Network.AWS.SageMaker.Types.ModelPackage
import Network.AWS.SageMaker.Types.ModelPackageContainerDefinition
import Network.AWS.SageMaker.Types.ModelPackageGroup
import Network.AWS.SageMaker.Types.ModelPackageGroupSortBy
import Network.AWS.SageMaker.Types.ModelPackageGroupStatus
import Network.AWS.SageMaker.Types.ModelPackageGroupSummary
import Network.AWS.SageMaker.Types.ModelPackageSortBy
import Network.AWS.SageMaker.Types.ModelPackageStatus
import Network.AWS.SageMaker.Types.ModelPackageStatusDetails
import Network.AWS.SageMaker.Types.ModelPackageStatusItem
import Network.AWS.SageMaker.Types.ModelPackageSummary
import Network.AWS.SageMaker.Types.ModelPackageType
import Network.AWS.SageMaker.Types.ModelPackageValidationProfile
import Network.AWS.SageMaker.Types.ModelPackageValidationSpecification
import Network.AWS.SageMaker.Types.ModelQuality
import Network.AWS.SageMaker.Types.ModelQualityAppSpecification
import Network.AWS.SageMaker.Types.ModelQualityBaselineConfig
import Network.AWS.SageMaker.Types.ModelQualityJobInput
import Network.AWS.SageMaker.Types.ModelSortKey
import Network.AWS.SageMaker.Types.ModelStepMetadata
import Network.AWS.SageMaker.Types.ModelSummary
import Network.AWS.SageMaker.Types.MonitoringAppSpecification
import Network.AWS.SageMaker.Types.MonitoringBaselineConfig
import Network.AWS.SageMaker.Types.MonitoringClusterConfig
import Network.AWS.SageMaker.Types.MonitoringConstraintsResource
import Network.AWS.SageMaker.Types.MonitoringExecutionSortKey
import Network.AWS.SageMaker.Types.MonitoringExecutionSummary
import Network.AWS.SageMaker.Types.MonitoringGroundTruthS3Input
import Network.AWS.SageMaker.Types.MonitoringInput
import Network.AWS.SageMaker.Types.MonitoringJobDefinition
import Network.AWS.SageMaker.Types.MonitoringJobDefinitionSortKey
import Network.AWS.SageMaker.Types.MonitoringJobDefinitionSummary
import Network.AWS.SageMaker.Types.MonitoringNetworkConfig
import Network.AWS.SageMaker.Types.MonitoringOutput
import Network.AWS.SageMaker.Types.MonitoringOutputConfig
import Network.AWS.SageMaker.Types.MonitoringProblemType
import Network.AWS.SageMaker.Types.MonitoringResources
import Network.AWS.SageMaker.Types.MonitoringS3Output
import Network.AWS.SageMaker.Types.MonitoringSchedule
import Network.AWS.SageMaker.Types.MonitoringScheduleConfig
import Network.AWS.SageMaker.Types.MonitoringScheduleSortKey
import Network.AWS.SageMaker.Types.MonitoringScheduleSummary
import Network.AWS.SageMaker.Types.MonitoringStatisticsResource
import Network.AWS.SageMaker.Types.MonitoringStoppingCondition
import Network.AWS.SageMaker.Types.MonitoringType
import Network.AWS.SageMaker.Types.MultiModelConfig
import Network.AWS.SageMaker.Types.NeoVpcConfig
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
import Network.AWS.SageMaker.Types.OfflineStoreConfig
import Network.AWS.SageMaker.Types.OfflineStoreStatus
import Network.AWS.SageMaker.Types.OfflineStoreStatusValue
import Network.AWS.SageMaker.Types.OidcConfig
import Network.AWS.SageMaker.Types.OidcConfigForResponse
import Network.AWS.SageMaker.Types.OidcMemberDefinition
import Network.AWS.SageMaker.Types.OnlineStoreConfig
import Network.AWS.SageMaker.Types.OnlineStoreSecurityConfig
import Network.AWS.SageMaker.Types.Operator
import Network.AWS.SageMaker.Types.OrderKey
import Network.AWS.SageMaker.Types.OutputConfig
import Network.AWS.SageMaker.Types.OutputDataConfig
import Network.AWS.SageMaker.Types.OutputParameter
import Network.AWS.SageMaker.Types.Parameter
import Network.AWS.SageMaker.Types.ParameterRange
import Network.AWS.SageMaker.Types.ParameterRanges
import Network.AWS.SageMaker.Types.ParameterType
import Network.AWS.SageMaker.Types.Parent
import Network.AWS.SageMaker.Types.ParentHyperParameterTuningJob
import Network.AWS.SageMaker.Types.Pipeline
import Network.AWS.SageMaker.Types.PipelineExecution
import Network.AWS.SageMaker.Types.PipelineExecutionStatus
import Network.AWS.SageMaker.Types.PipelineExecutionStep
import Network.AWS.SageMaker.Types.PipelineExecutionStepMetadata
import Network.AWS.SageMaker.Types.PipelineExecutionSummary
import Network.AWS.SageMaker.Types.PipelineExperimentConfig
import Network.AWS.SageMaker.Types.PipelineStatus
import Network.AWS.SageMaker.Types.PipelineSummary
import Network.AWS.SageMaker.Types.ProblemType
import Network.AWS.SageMaker.Types.ProcessingClusterConfig
import Network.AWS.SageMaker.Types.ProcessingFeatureStoreOutput
import Network.AWS.SageMaker.Types.ProcessingInput
import Network.AWS.SageMaker.Types.ProcessingInstanceType
import Network.AWS.SageMaker.Types.ProcessingJob
import Network.AWS.SageMaker.Types.ProcessingJobStatus
import Network.AWS.SageMaker.Types.ProcessingJobStepMetadata
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
import Network.AWS.SageMaker.Types.ProductionVariantCoreDumpConfig
import Network.AWS.SageMaker.Types.ProductionVariantInstanceType
import Network.AWS.SageMaker.Types.ProductionVariantSummary
import Network.AWS.SageMaker.Types.ProfilerConfig
import Network.AWS.SageMaker.Types.ProfilerConfigForUpdate
import Network.AWS.SageMaker.Types.ProfilerRuleConfiguration
import Network.AWS.SageMaker.Types.ProfilerRuleEvaluationStatus
import Network.AWS.SageMaker.Types.ProfilingStatus
import Network.AWS.SageMaker.Types.Project
import Network.AWS.SageMaker.Types.ProjectSortBy
import Network.AWS.SageMaker.Types.ProjectSortOrder
import Network.AWS.SageMaker.Types.ProjectStatus
import Network.AWS.SageMaker.Types.ProjectSummary
import Network.AWS.SageMaker.Types.PropertyNameQuery
import Network.AWS.SageMaker.Types.PropertyNameSuggestion
import Network.AWS.SageMaker.Types.ProvisioningParameter
import Network.AWS.SageMaker.Types.PublicWorkforceTaskPrice
import Network.AWS.SageMaker.Types.RecordWrapper
import Network.AWS.SageMaker.Types.RedshiftDatasetDefinition
import Network.AWS.SageMaker.Types.RedshiftResultCompressionType
import Network.AWS.SageMaker.Types.RedshiftResultFormat
import Network.AWS.SageMaker.Types.RegisterModelStepMetadata
import Network.AWS.SageMaker.Types.RenderableTask
import Network.AWS.SageMaker.Types.RenderingError
import Network.AWS.SageMaker.Types.RepositoryAccessMode
import Network.AWS.SageMaker.Types.RepositoryAuthConfig
import Network.AWS.SageMaker.Types.ResolvedAttributes
import Network.AWS.SageMaker.Types.ResourceConfig
import Network.AWS.SageMaker.Types.ResourceLimits
import Network.AWS.SageMaker.Types.ResourceSpec
import Network.AWS.SageMaker.Types.ResourceType
import Network.AWS.SageMaker.Types.RetentionPolicy
import Network.AWS.SageMaker.Types.RetentionType
import Network.AWS.SageMaker.Types.RetryStrategy
import Network.AWS.SageMaker.Types.RootAccess
import Network.AWS.SageMaker.Types.RuleEvaluationStatus
import Network.AWS.SageMaker.Types.S3DataDistribution
import Network.AWS.SageMaker.Types.S3DataSource
import Network.AWS.SageMaker.Types.S3DataType
import Network.AWS.SageMaker.Types.S3StorageConfig
import Network.AWS.SageMaker.Types.SagemakerServicecatalogStatus
import Network.AWS.SageMaker.Types.ScheduleConfig
import Network.AWS.SageMaker.Types.ScheduleStatus
import Network.AWS.SageMaker.Types.SearchExpression
import Network.AWS.SageMaker.Types.SearchRecord
import Network.AWS.SageMaker.Types.SearchSortOrder
import Network.AWS.SageMaker.Types.SecondaryStatus
import Network.AWS.SageMaker.Types.SecondaryStatusTransition
import Network.AWS.SageMaker.Types.ServiceCatalogProvisionedProductDetails
import Network.AWS.SageMaker.Types.ServiceCatalogProvisioningDetails
import Network.AWS.SageMaker.Types.SharingSettings
import Network.AWS.SageMaker.Types.ShuffleConfig
import Network.AWS.SageMaker.Types.SortActionsBy
import Network.AWS.SageMaker.Types.SortArtifactsBy
import Network.AWS.SageMaker.Types.SortAssociationsBy
import Network.AWS.SageMaker.Types.SortBy
import Network.AWS.SageMaker.Types.SortContextsBy
import Network.AWS.SageMaker.Types.SortExperimentsBy
import Network.AWS.SageMaker.Types.SortOrder
import Network.AWS.SageMaker.Types.SortPipelineExecutionsBy
import Network.AWS.SageMaker.Types.SortPipelinesBy
import Network.AWS.SageMaker.Types.SortTrialComponentsBy
import Network.AWS.SageMaker.Types.SortTrialsBy
import Network.AWS.SageMaker.Types.SourceAlgorithm
import Network.AWS.SageMaker.Types.SourceAlgorithmSpecification
import Network.AWS.SageMaker.Types.SourceIpConfig
import Network.AWS.SageMaker.Types.SplitType
import Network.AWS.SageMaker.Types.StepStatus
import Network.AWS.SageMaker.Types.StoppingCondition
import Network.AWS.SageMaker.Types.StudioLifecycleConfigAppType
import Network.AWS.SageMaker.Types.StudioLifecycleConfigDetails
import Network.AWS.SageMaker.Types.StudioLifecycleConfigSortKey
import Network.AWS.SageMaker.Types.SubscribedWorkteam
import Network.AWS.SageMaker.Types.SuggestionQuery
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.TargetDevice
import Network.AWS.SageMaker.Types.TargetPlatform
import Network.AWS.SageMaker.Types.TargetPlatformAccelerator
import Network.AWS.SageMaker.Types.TargetPlatformArch
import Network.AWS.SageMaker.Types.TargetPlatformOs
import Network.AWS.SageMaker.Types.TensorBoardAppSettings
import Network.AWS.SageMaker.Types.TensorBoardOutputConfig
import Network.AWS.SageMaker.Types.TrafficRoutingConfig
import Network.AWS.SageMaker.Types.TrafficRoutingConfigType
import Network.AWS.SageMaker.Types.TrainingInputMode
import Network.AWS.SageMaker.Types.TrainingInstanceType
import Network.AWS.SageMaker.Types.TrainingJob
import Network.AWS.SageMaker.Types.TrainingJobDefinition
import Network.AWS.SageMaker.Types.TrainingJobEarlyStoppingType
import Network.AWS.SageMaker.Types.TrainingJobSortByOptions
import Network.AWS.SageMaker.Types.TrainingJobStatus
import Network.AWS.SageMaker.Types.TrainingJobStatusCounters
import Network.AWS.SageMaker.Types.TrainingJobStepMetadata
import Network.AWS.SageMaker.Types.TrainingJobSummary
import Network.AWS.SageMaker.Types.TrainingSpecification
import Network.AWS.SageMaker.Types.TransformDataSource
import Network.AWS.SageMaker.Types.TransformInput
import Network.AWS.SageMaker.Types.TransformInstanceType
import Network.AWS.SageMaker.Types.TransformJob
import Network.AWS.SageMaker.Types.TransformJobDefinition
import Network.AWS.SageMaker.Types.TransformJobStatus
import Network.AWS.SageMaker.Types.TransformJobStepMetadata
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
import Network.AWS.SageMaker.Types.TuningJobStepMetaData
import Network.AWS.SageMaker.Types.USD
import Network.AWS.SageMaker.Types.UiConfig
import Network.AWS.SageMaker.Types.UiTemplate
import Network.AWS.SageMaker.Types.UiTemplateInfo
import Network.AWS.SageMaker.Types.UserContext
import Network.AWS.SageMaker.Types.UserProfileDetails
import Network.AWS.SageMaker.Types.UserProfileSortKey
import Network.AWS.SageMaker.Types.UserProfileStatus
import Network.AWS.SageMaker.Types.UserSettings
import Network.AWS.SageMaker.Types.VariantProperty
import Network.AWS.SageMaker.Types.VariantPropertyType
import Network.AWS.SageMaker.Types.VpcConfig
import Network.AWS.SageMaker.Types.Workforce
import Network.AWS.SageMaker.Types.Workteam
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-07-24@ of the Amazon SageMaker Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "SageMaker",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "api.sagemaker",
      Core._serviceSigningName = "sagemaker",
      Core._serviceVersion = "2017-07-24",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "SageMaker",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | You have exceeded an Amazon SageMaker resource limit. For example, you
-- might have too many training jobs created.
_ResourceLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceeded"

-- | Resource being accessed is in use.
_ResourceInUse :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUse =
  Core._MatchServiceError
    defaultService
    "ResourceInUse"

-- | There was a conflict when you attempted to modify a SageMaker entity
-- such as an @Experiment@ or @Artifact@.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | Resource being access is not found.
_ResourceNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFound =
  Core._MatchServiceError
    defaultService
    "ResourceNotFound"

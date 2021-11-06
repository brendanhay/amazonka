{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ActionSource
import Amazonka.SageMaker.Types.ActionStatus
import Amazonka.SageMaker.Types.ActionSummary
import Amazonka.SageMaker.Types.AgentVersion
import Amazonka.SageMaker.Types.Alarm
import Amazonka.SageMaker.Types.AlgorithmSortBy
import Amazonka.SageMaker.Types.AlgorithmSpecification
import Amazonka.SageMaker.Types.AlgorithmStatus
import Amazonka.SageMaker.Types.AlgorithmStatusDetails
import Amazonka.SageMaker.Types.AlgorithmStatusItem
import Amazonka.SageMaker.Types.AlgorithmSummary
import Amazonka.SageMaker.Types.AlgorithmValidationProfile
import Amazonka.SageMaker.Types.AlgorithmValidationSpecification
import Amazonka.SageMaker.Types.AnnotationConsolidationConfig
import Amazonka.SageMaker.Types.AppDetails
import Amazonka.SageMaker.Types.AppImageConfigDetails
import Amazonka.SageMaker.Types.AppImageConfigSortKey
import Amazonka.SageMaker.Types.AppInstanceType
import Amazonka.SageMaker.Types.AppNetworkAccessType
import Amazonka.SageMaker.Types.AppSortKey
import Amazonka.SageMaker.Types.AppSpecification
import Amazonka.SageMaker.Types.AppStatus
import Amazonka.SageMaker.Types.AppType
import Amazonka.SageMaker.Types.ArtifactSource
import Amazonka.SageMaker.Types.ArtifactSourceIdType
import Amazonka.SageMaker.Types.ArtifactSourceType
import Amazonka.SageMaker.Types.ArtifactSummary
import Amazonka.SageMaker.Types.AssemblyType
import Amazonka.SageMaker.Types.AssociationEdgeType
import Amazonka.SageMaker.Types.AssociationSummary
import Amazonka.SageMaker.Types.AsyncInferenceClientConfig
import Amazonka.SageMaker.Types.AsyncInferenceConfig
import Amazonka.SageMaker.Types.AsyncInferenceNotificationConfig
import Amazonka.SageMaker.Types.AsyncInferenceOutputConfig
import Amazonka.SageMaker.Types.AthenaDatasetDefinition
import Amazonka.SageMaker.Types.AthenaResultCompressionType
import Amazonka.SageMaker.Types.AthenaResultFormat
import Amazonka.SageMaker.Types.AuthMode
import Amazonka.SageMaker.Types.AutoMLCandidate
import Amazonka.SageMaker.Types.AutoMLCandidateStep
import Amazonka.SageMaker.Types.AutoMLChannel
import Amazonka.SageMaker.Types.AutoMLContainerDefinition
import Amazonka.SageMaker.Types.AutoMLDataSource
import Amazonka.SageMaker.Types.AutoMLJobArtifacts
import Amazonka.SageMaker.Types.AutoMLJobCompletionCriteria
import Amazonka.SageMaker.Types.AutoMLJobConfig
import Amazonka.SageMaker.Types.AutoMLJobObjective
import Amazonka.SageMaker.Types.AutoMLJobObjectiveType
import Amazonka.SageMaker.Types.AutoMLJobSecondaryStatus
import Amazonka.SageMaker.Types.AutoMLJobStatus
import Amazonka.SageMaker.Types.AutoMLJobSummary
import Amazonka.SageMaker.Types.AutoMLMetricEnum
import Amazonka.SageMaker.Types.AutoMLOutputDataConfig
import Amazonka.SageMaker.Types.AutoMLPartialFailureReason
import Amazonka.SageMaker.Types.AutoMLS3DataSource
import Amazonka.SageMaker.Types.AutoMLS3DataType
import Amazonka.SageMaker.Types.AutoMLSecurityConfig
import Amazonka.SageMaker.Types.AutoMLSortBy
import Amazonka.SageMaker.Types.AutoMLSortOrder
import Amazonka.SageMaker.Types.AutoRollbackConfig
import Amazonka.SageMaker.Types.AwsManagedHumanLoopRequestSource
import Amazonka.SageMaker.Types.BatchStrategy
import Amazonka.SageMaker.Types.Bias
import Amazonka.SageMaker.Types.BlueGreenUpdatePolicy
import Amazonka.SageMaker.Types.BooleanOperator
import Amazonka.SageMaker.Types.CacheHitResult
import Amazonka.SageMaker.Types.CallbackStepMetadata
import Amazonka.SageMaker.Types.CandidateArtifactLocations
import Amazonka.SageMaker.Types.CandidateProperties
import Amazonka.SageMaker.Types.CandidateSortBy
import Amazonka.SageMaker.Types.CandidateStatus
import Amazonka.SageMaker.Types.CandidateStepType
import Amazonka.SageMaker.Types.CapacitySize
import Amazonka.SageMaker.Types.CapacitySizeType
import Amazonka.SageMaker.Types.CaptureContentTypeHeader
import Amazonka.SageMaker.Types.CaptureMode
import Amazonka.SageMaker.Types.CaptureOption
import Amazonka.SageMaker.Types.CaptureStatus
import Amazonka.SageMaker.Types.CategoricalParameterRange
import Amazonka.SageMaker.Types.CategoricalParameterRangeSpecification
import Amazonka.SageMaker.Types.Channel
import Amazonka.SageMaker.Types.ChannelSpecification
import Amazonka.SageMaker.Types.CheckpointConfig
import Amazonka.SageMaker.Types.CodeRepositorySortBy
import Amazonka.SageMaker.Types.CodeRepositorySortOrder
import Amazonka.SageMaker.Types.CodeRepositorySummary
import Amazonka.SageMaker.Types.CognitoConfig
import Amazonka.SageMaker.Types.CognitoMemberDefinition
import Amazonka.SageMaker.Types.CollectionConfiguration
import Amazonka.SageMaker.Types.CompilationJobStatus
import Amazonka.SageMaker.Types.CompilationJobSummary
import Amazonka.SageMaker.Types.CompressionType
import Amazonka.SageMaker.Types.ConditionOutcome
import Amazonka.SageMaker.Types.ConditionStepMetadata
import Amazonka.SageMaker.Types.ContainerDefinition
import Amazonka.SageMaker.Types.ContainerMode
import Amazonka.SageMaker.Types.ContentClassifier
import Amazonka.SageMaker.Types.ContextSource
import Amazonka.SageMaker.Types.ContextSummary
import Amazonka.SageMaker.Types.ContinuousParameterRange
import Amazonka.SageMaker.Types.ContinuousParameterRangeSpecification
import Amazonka.SageMaker.Types.CustomImage
import Amazonka.SageMaker.Types.DataCaptureConfig
import Amazonka.SageMaker.Types.DataCaptureConfigSummary
import Amazonka.SageMaker.Types.DataCatalogConfig
import Amazonka.SageMaker.Types.DataDistributionType
import Amazonka.SageMaker.Types.DataProcessing
import Amazonka.SageMaker.Types.DataQualityAppSpecification
import Amazonka.SageMaker.Types.DataQualityBaselineConfig
import Amazonka.SageMaker.Types.DataQualityJobInput
import Amazonka.SageMaker.Types.DataSource
import Amazonka.SageMaker.Types.DatasetDefinition
import Amazonka.SageMaker.Types.DebugHookConfig
import Amazonka.SageMaker.Types.DebugRuleConfiguration
import Amazonka.SageMaker.Types.DebugRuleEvaluationStatus
import Amazonka.SageMaker.Types.DeployedImage
import Amazonka.SageMaker.Types.DeploymentConfig
import Amazonka.SageMaker.Types.DesiredWeightAndCapacity
import Amazonka.SageMaker.Types.DetailedAlgorithmStatus
import Amazonka.SageMaker.Types.DetailedModelPackageStatus
import Amazonka.SageMaker.Types.Device
import Amazonka.SageMaker.Types.DeviceFleetSummary
import Amazonka.SageMaker.Types.DeviceStats
import Amazonka.SageMaker.Types.DeviceSummary
import Amazonka.SageMaker.Types.DirectInternetAccess
import Amazonka.SageMaker.Types.DomainDetails
import Amazonka.SageMaker.Types.DomainStatus
import Amazonka.SageMaker.Types.EdgeModel
import Amazonka.SageMaker.Types.EdgeModelStat
import Amazonka.SageMaker.Types.EdgeModelSummary
import Amazonka.SageMaker.Types.EdgeOutputConfig
import Amazonka.SageMaker.Types.EdgePackagingJobStatus
import Amazonka.SageMaker.Types.EdgePackagingJobSummary
import Amazonka.SageMaker.Types.EdgePresetDeploymentOutput
import Amazonka.SageMaker.Types.EdgePresetDeploymentStatus
import Amazonka.SageMaker.Types.EdgePresetDeploymentType
import Amazonka.SageMaker.Types.Endpoint
import Amazonka.SageMaker.Types.EndpointConfigSortKey
import Amazonka.SageMaker.Types.EndpointConfigSummary
import Amazonka.SageMaker.Types.EndpointInput
import Amazonka.SageMaker.Types.EndpointSortKey
import Amazonka.SageMaker.Types.EndpointStatus
import Amazonka.SageMaker.Types.EndpointSummary
import Amazonka.SageMaker.Types.ExecutionStatus
import Amazonka.SageMaker.Types.Experiment
import Amazonka.SageMaker.Types.ExperimentConfig
import Amazonka.SageMaker.Types.ExperimentSource
import Amazonka.SageMaker.Types.ExperimentSummary
import Amazonka.SageMaker.Types.Explainability
import Amazonka.SageMaker.Types.FeatureDefinition
import Amazonka.SageMaker.Types.FeatureGroup
import Amazonka.SageMaker.Types.FeatureGroupSortBy
import Amazonka.SageMaker.Types.FeatureGroupSortOrder
import Amazonka.SageMaker.Types.FeatureGroupStatus
import Amazonka.SageMaker.Types.FeatureGroupSummary
import Amazonka.SageMaker.Types.FeatureType
import Amazonka.SageMaker.Types.FileSystemAccessMode
import Amazonka.SageMaker.Types.FileSystemConfig
import Amazonka.SageMaker.Types.FileSystemDataSource
import Amazonka.SageMaker.Types.FileSystemType
import Amazonka.SageMaker.Types.Filter
import Amazonka.SageMaker.Types.FinalAutoMLJobObjectiveMetric
import Amazonka.SageMaker.Types.FinalHyperParameterTuningJobObjectiveMetric
import Amazonka.SageMaker.Types.FlowDefinitionOutputConfig
import Amazonka.SageMaker.Types.FlowDefinitionStatus
import Amazonka.SageMaker.Types.FlowDefinitionSummary
import Amazonka.SageMaker.Types.Framework
import Amazonka.SageMaker.Types.GitConfig
import Amazonka.SageMaker.Types.GitConfigForUpdate
import Amazonka.SageMaker.Types.HumanLoopActivationConditionsConfig
import Amazonka.SageMaker.Types.HumanLoopActivationConfig
import Amazonka.SageMaker.Types.HumanLoopConfig
import Amazonka.SageMaker.Types.HumanLoopRequestSource
import Amazonka.SageMaker.Types.HumanTaskConfig
import Amazonka.SageMaker.Types.HumanTaskUiStatus
import Amazonka.SageMaker.Types.HumanTaskUiSummary
import Amazonka.SageMaker.Types.HyperParameterAlgorithmSpecification
import Amazonka.SageMaker.Types.HyperParameterScalingType
import Amazonka.SageMaker.Types.HyperParameterSpecification
import Amazonka.SageMaker.Types.HyperParameterTrainingJobDefinition
import Amazonka.SageMaker.Types.HyperParameterTrainingJobSummary
import Amazonka.SageMaker.Types.HyperParameterTuningJobConfig
import Amazonka.SageMaker.Types.HyperParameterTuningJobObjective
import Amazonka.SageMaker.Types.HyperParameterTuningJobObjectiveType
import Amazonka.SageMaker.Types.HyperParameterTuningJobSortByOptions
import Amazonka.SageMaker.Types.HyperParameterTuningJobStatus
import Amazonka.SageMaker.Types.HyperParameterTuningJobStrategyType
import Amazonka.SageMaker.Types.HyperParameterTuningJobSummary
import Amazonka.SageMaker.Types.HyperParameterTuningJobWarmStartConfig
import Amazonka.SageMaker.Types.HyperParameterTuningJobWarmStartType
import Amazonka.SageMaker.Types.Image
import Amazonka.SageMaker.Types.ImageConfig
import Amazonka.SageMaker.Types.ImageSortBy
import Amazonka.SageMaker.Types.ImageSortOrder
import Amazonka.SageMaker.Types.ImageStatus
import Amazonka.SageMaker.Types.ImageVersion
import Amazonka.SageMaker.Types.ImageVersionSortBy
import Amazonka.SageMaker.Types.ImageVersionSortOrder
import Amazonka.SageMaker.Types.ImageVersionStatus
import Amazonka.SageMaker.Types.InferenceExecutionConfig
import Amazonka.SageMaker.Types.InferenceExecutionMode
import Amazonka.SageMaker.Types.InferenceSpecification
import Amazonka.SageMaker.Types.InputConfig
import Amazonka.SageMaker.Types.InputMode
import Amazonka.SageMaker.Types.InstanceType
import Amazonka.SageMaker.Types.IntegerParameterRange
import Amazonka.SageMaker.Types.IntegerParameterRangeSpecification
import Amazonka.SageMaker.Types.JoinSource
import Amazonka.SageMaker.Types.JupyterServerAppSettings
import Amazonka.SageMaker.Types.KernelGatewayAppSettings
import Amazonka.SageMaker.Types.KernelGatewayImageConfig
import Amazonka.SageMaker.Types.KernelSpec
import Amazonka.SageMaker.Types.LabelCounters
import Amazonka.SageMaker.Types.LabelCountersForWorkteam
import Amazonka.SageMaker.Types.LabelingJobAlgorithmsConfig
import Amazonka.SageMaker.Types.LabelingJobDataAttributes
import Amazonka.SageMaker.Types.LabelingJobDataSource
import Amazonka.SageMaker.Types.LabelingJobForWorkteamSummary
import Amazonka.SageMaker.Types.LabelingJobInputConfig
import Amazonka.SageMaker.Types.LabelingJobOutput
import Amazonka.SageMaker.Types.LabelingJobOutputConfig
import Amazonka.SageMaker.Types.LabelingJobResourceConfig
import Amazonka.SageMaker.Types.LabelingJobS3DataSource
import Amazonka.SageMaker.Types.LabelingJobSnsDataSource
import Amazonka.SageMaker.Types.LabelingJobStatus
import Amazonka.SageMaker.Types.LabelingJobStoppingConditions
import Amazonka.SageMaker.Types.LabelingJobSummary
import Amazonka.SageMaker.Types.LambdaStepMetadata
import Amazonka.SageMaker.Types.ListCompilationJobsSortBy
import Amazonka.SageMaker.Types.ListDeviceFleetsSortBy
import Amazonka.SageMaker.Types.ListEdgePackagingJobsSortBy
import Amazonka.SageMaker.Types.ListLabelingJobsForWorkteamSortByOptions
import Amazonka.SageMaker.Types.ListWorkforcesSortByOptions
import Amazonka.SageMaker.Types.ListWorkteamsSortByOptions
import Amazonka.SageMaker.Types.MemberDefinition
import Amazonka.SageMaker.Types.MetadataProperties
import Amazonka.SageMaker.Types.MetricData
import Amazonka.SageMaker.Types.MetricDatum
import Amazonka.SageMaker.Types.MetricDefinition
import Amazonka.SageMaker.Types.MetricSetSource
import Amazonka.SageMaker.Types.MetricsSource
import Amazonka.SageMaker.Types.ModelApprovalStatus
import Amazonka.SageMaker.Types.ModelArtifacts
import Amazonka.SageMaker.Types.ModelBiasAppSpecification
import Amazonka.SageMaker.Types.ModelBiasBaselineConfig
import Amazonka.SageMaker.Types.ModelBiasJobInput
import Amazonka.SageMaker.Types.ModelCacheSetting
import Amazonka.SageMaker.Types.ModelClientConfig
import Amazonka.SageMaker.Types.ModelDataQuality
import Amazonka.SageMaker.Types.ModelDeployConfig
import Amazonka.SageMaker.Types.ModelDeployResult
import Amazonka.SageMaker.Types.ModelDigests
import Amazonka.SageMaker.Types.ModelExplainabilityAppSpecification
import Amazonka.SageMaker.Types.ModelExplainabilityBaselineConfig
import Amazonka.SageMaker.Types.ModelExplainabilityJobInput
import Amazonka.SageMaker.Types.ModelMetrics
import Amazonka.SageMaker.Types.ModelPackage
import Amazonka.SageMaker.Types.ModelPackageContainerDefinition
import Amazonka.SageMaker.Types.ModelPackageGroup
import Amazonka.SageMaker.Types.ModelPackageGroupSortBy
import Amazonka.SageMaker.Types.ModelPackageGroupStatus
import Amazonka.SageMaker.Types.ModelPackageGroupSummary
import Amazonka.SageMaker.Types.ModelPackageSortBy
import Amazonka.SageMaker.Types.ModelPackageStatus
import Amazonka.SageMaker.Types.ModelPackageStatusDetails
import Amazonka.SageMaker.Types.ModelPackageStatusItem
import Amazonka.SageMaker.Types.ModelPackageSummary
import Amazonka.SageMaker.Types.ModelPackageType
import Amazonka.SageMaker.Types.ModelPackageValidationProfile
import Amazonka.SageMaker.Types.ModelPackageValidationSpecification
import Amazonka.SageMaker.Types.ModelQuality
import Amazonka.SageMaker.Types.ModelQualityAppSpecification
import Amazonka.SageMaker.Types.ModelQualityBaselineConfig
import Amazonka.SageMaker.Types.ModelQualityJobInput
import Amazonka.SageMaker.Types.ModelSortKey
import Amazonka.SageMaker.Types.ModelStepMetadata
import Amazonka.SageMaker.Types.ModelSummary
import Amazonka.SageMaker.Types.MonitoringAppSpecification
import Amazonka.SageMaker.Types.MonitoringBaselineConfig
import Amazonka.SageMaker.Types.MonitoringClusterConfig
import Amazonka.SageMaker.Types.MonitoringConstraintsResource
import Amazonka.SageMaker.Types.MonitoringExecutionSortKey
import Amazonka.SageMaker.Types.MonitoringExecutionSummary
import Amazonka.SageMaker.Types.MonitoringGroundTruthS3Input
import Amazonka.SageMaker.Types.MonitoringInput
import Amazonka.SageMaker.Types.MonitoringJobDefinition
import Amazonka.SageMaker.Types.MonitoringJobDefinitionSortKey
import Amazonka.SageMaker.Types.MonitoringJobDefinitionSummary
import Amazonka.SageMaker.Types.MonitoringNetworkConfig
import Amazonka.SageMaker.Types.MonitoringOutput
import Amazonka.SageMaker.Types.MonitoringOutputConfig
import Amazonka.SageMaker.Types.MonitoringProblemType
import Amazonka.SageMaker.Types.MonitoringResources
import Amazonka.SageMaker.Types.MonitoringS3Output
import Amazonka.SageMaker.Types.MonitoringSchedule
import Amazonka.SageMaker.Types.MonitoringScheduleConfig
import Amazonka.SageMaker.Types.MonitoringScheduleSortKey
import Amazonka.SageMaker.Types.MonitoringScheduleSummary
import Amazonka.SageMaker.Types.MonitoringStatisticsResource
import Amazonka.SageMaker.Types.MonitoringStoppingCondition
import Amazonka.SageMaker.Types.MonitoringType
import Amazonka.SageMaker.Types.MultiModelConfig
import Amazonka.SageMaker.Types.NeoVpcConfig
import Amazonka.SageMaker.Types.NestedFilters
import Amazonka.SageMaker.Types.NetworkConfig
import Amazonka.SageMaker.Types.NotebookInstanceAcceleratorType
import Amazonka.SageMaker.Types.NotebookInstanceLifecycleConfigSortKey
import Amazonka.SageMaker.Types.NotebookInstanceLifecycleConfigSortOrder
import Amazonka.SageMaker.Types.NotebookInstanceLifecycleConfigSummary
import Amazonka.SageMaker.Types.NotebookInstanceLifecycleHook
import Amazonka.SageMaker.Types.NotebookInstanceSortKey
import Amazonka.SageMaker.Types.NotebookInstanceSortOrder
import Amazonka.SageMaker.Types.NotebookInstanceStatus
import Amazonka.SageMaker.Types.NotebookInstanceSummary
import Amazonka.SageMaker.Types.NotebookOutputOption
import Amazonka.SageMaker.Types.NotificationConfiguration
import Amazonka.SageMaker.Types.ObjectiveStatus
import Amazonka.SageMaker.Types.ObjectiveStatusCounters
import Amazonka.SageMaker.Types.OfflineStoreConfig
import Amazonka.SageMaker.Types.OfflineStoreStatus
import Amazonka.SageMaker.Types.OfflineStoreStatusValue
import Amazonka.SageMaker.Types.OidcConfig
import Amazonka.SageMaker.Types.OidcConfigForResponse
import Amazonka.SageMaker.Types.OidcMemberDefinition
import Amazonka.SageMaker.Types.OnlineStoreConfig
import Amazonka.SageMaker.Types.OnlineStoreSecurityConfig
import Amazonka.SageMaker.Types.Operator
import Amazonka.SageMaker.Types.OrderKey
import Amazonka.SageMaker.Types.OutputConfig
import Amazonka.SageMaker.Types.OutputDataConfig
import Amazonka.SageMaker.Types.OutputParameter
import Amazonka.SageMaker.Types.Parameter
import Amazonka.SageMaker.Types.ParameterRange
import Amazonka.SageMaker.Types.ParameterRanges
import Amazonka.SageMaker.Types.ParameterType
import Amazonka.SageMaker.Types.Parent
import Amazonka.SageMaker.Types.ParentHyperParameterTuningJob
import Amazonka.SageMaker.Types.Pipeline
import Amazonka.SageMaker.Types.PipelineExecution
import Amazonka.SageMaker.Types.PipelineExecutionStatus
import Amazonka.SageMaker.Types.PipelineExecutionStep
import Amazonka.SageMaker.Types.PipelineExecutionStepMetadata
import Amazonka.SageMaker.Types.PipelineExecutionSummary
import Amazonka.SageMaker.Types.PipelineExperimentConfig
import Amazonka.SageMaker.Types.PipelineStatus
import Amazonka.SageMaker.Types.PipelineSummary
import Amazonka.SageMaker.Types.ProblemType
import Amazonka.SageMaker.Types.ProcessingClusterConfig
import Amazonka.SageMaker.Types.ProcessingFeatureStoreOutput
import Amazonka.SageMaker.Types.ProcessingInput
import Amazonka.SageMaker.Types.ProcessingInstanceType
import Amazonka.SageMaker.Types.ProcessingJob
import Amazonka.SageMaker.Types.ProcessingJobStatus
import Amazonka.SageMaker.Types.ProcessingJobStepMetadata
import Amazonka.SageMaker.Types.ProcessingJobSummary
import Amazonka.SageMaker.Types.ProcessingOutput
import Amazonka.SageMaker.Types.ProcessingOutputConfig
import Amazonka.SageMaker.Types.ProcessingResources
import Amazonka.SageMaker.Types.ProcessingS3CompressionType
import Amazonka.SageMaker.Types.ProcessingS3DataDistributionType
import Amazonka.SageMaker.Types.ProcessingS3DataType
import Amazonka.SageMaker.Types.ProcessingS3Input
import Amazonka.SageMaker.Types.ProcessingS3InputMode
import Amazonka.SageMaker.Types.ProcessingS3Output
import Amazonka.SageMaker.Types.ProcessingS3UploadMode
import Amazonka.SageMaker.Types.ProcessingStoppingCondition
import Amazonka.SageMaker.Types.ProductionVariant
import Amazonka.SageMaker.Types.ProductionVariantAcceleratorType
import Amazonka.SageMaker.Types.ProductionVariantCoreDumpConfig
import Amazonka.SageMaker.Types.ProductionVariantInstanceType
import Amazonka.SageMaker.Types.ProductionVariantSummary
import Amazonka.SageMaker.Types.ProfilerConfig
import Amazonka.SageMaker.Types.ProfilerConfigForUpdate
import Amazonka.SageMaker.Types.ProfilerRuleConfiguration
import Amazonka.SageMaker.Types.ProfilerRuleEvaluationStatus
import Amazonka.SageMaker.Types.ProfilingStatus
import Amazonka.SageMaker.Types.Project
import Amazonka.SageMaker.Types.ProjectSortBy
import Amazonka.SageMaker.Types.ProjectSortOrder
import Amazonka.SageMaker.Types.ProjectStatus
import Amazonka.SageMaker.Types.ProjectSummary
import Amazonka.SageMaker.Types.PropertyNameQuery
import Amazonka.SageMaker.Types.PropertyNameSuggestion
import Amazonka.SageMaker.Types.ProvisioningParameter
import Amazonka.SageMaker.Types.PublicWorkforceTaskPrice
import Amazonka.SageMaker.Types.RecordWrapper
import Amazonka.SageMaker.Types.RedshiftDatasetDefinition
import Amazonka.SageMaker.Types.RedshiftResultCompressionType
import Amazonka.SageMaker.Types.RedshiftResultFormat
import Amazonka.SageMaker.Types.RegisterModelStepMetadata
import Amazonka.SageMaker.Types.RenderableTask
import Amazonka.SageMaker.Types.RenderingError
import Amazonka.SageMaker.Types.RepositoryAccessMode
import Amazonka.SageMaker.Types.RepositoryAuthConfig
import Amazonka.SageMaker.Types.ResolvedAttributes
import Amazonka.SageMaker.Types.ResourceConfig
import Amazonka.SageMaker.Types.ResourceLimits
import Amazonka.SageMaker.Types.ResourceSpec
import Amazonka.SageMaker.Types.ResourceType
import Amazonka.SageMaker.Types.RetentionPolicy
import Amazonka.SageMaker.Types.RetentionType
import Amazonka.SageMaker.Types.RetryStrategy
import Amazonka.SageMaker.Types.RootAccess
import Amazonka.SageMaker.Types.RuleEvaluationStatus
import Amazonka.SageMaker.Types.S3DataDistribution
import Amazonka.SageMaker.Types.S3DataSource
import Amazonka.SageMaker.Types.S3DataType
import Amazonka.SageMaker.Types.S3StorageConfig
import Amazonka.SageMaker.Types.SagemakerServicecatalogStatus
import Amazonka.SageMaker.Types.ScheduleConfig
import Amazonka.SageMaker.Types.ScheduleStatus
import Amazonka.SageMaker.Types.SearchExpression
import Amazonka.SageMaker.Types.SearchRecord
import Amazonka.SageMaker.Types.SearchSortOrder
import Amazonka.SageMaker.Types.SecondaryStatus
import Amazonka.SageMaker.Types.SecondaryStatusTransition
import Amazonka.SageMaker.Types.ServiceCatalogProvisionedProductDetails
import Amazonka.SageMaker.Types.ServiceCatalogProvisioningDetails
import Amazonka.SageMaker.Types.SharingSettings
import Amazonka.SageMaker.Types.ShuffleConfig
import Amazonka.SageMaker.Types.SortActionsBy
import Amazonka.SageMaker.Types.SortArtifactsBy
import Amazonka.SageMaker.Types.SortAssociationsBy
import Amazonka.SageMaker.Types.SortBy
import Amazonka.SageMaker.Types.SortContextsBy
import Amazonka.SageMaker.Types.SortExperimentsBy
import Amazonka.SageMaker.Types.SortOrder
import Amazonka.SageMaker.Types.SortPipelineExecutionsBy
import Amazonka.SageMaker.Types.SortPipelinesBy
import Amazonka.SageMaker.Types.SortTrialComponentsBy
import Amazonka.SageMaker.Types.SortTrialsBy
import Amazonka.SageMaker.Types.SourceAlgorithm
import Amazonka.SageMaker.Types.SourceAlgorithmSpecification
import Amazonka.SageMaker.Types.SourceIpConfig
import Amazonka.SageMaker.Types.SplitType
import Amazonka.SageMaker.Types.StepStatus
import Amazonka.SageMaker.Types.StoppingCondition
import Amazonka.SageMaker.Types.StudioLifecycleConfigAppType
import Amazonka.SageMaker.Types.StudioLifecycleConfigDetails
import Amazonka.SageMaker.Types.StudioLifecycleConfigSortKey
import Amazonka.SageMaker.Types.SubscribedWorkteam
import Amazonka.SageMaker.Types.SuggestionQuery
import Amazonka.SageMaker.Types.Tag
import Amazonka.SageMaker.Types.TargetDevice
import Amazonka.SageMaker.Types.TargetPlatform
import Amazonka.SageMaker.Types.TargetPlatformAccelerator
import Amazonka.SageMaker.Types.TargetPlatformArch
import Amazonka.SageMaker.Types.TargetPlatformOs
import Amazonka.SageMaker.Types.TensorBoardAppSettings
import Amazonka.SageMaker.Types.TensorBoardOutputConfig
import Amazonka.SageMaker.Types.TrafficRoutingConfig
import Amazonka.SageMaker.Types.TrafficRoutingConfigType
import Amazonka.SageMaker.Types.TrainingInputMode
import Amazonka.SageMaker.Types.TrainingInstanceType
import Amazonka.SageMaker.Types.TrainingJob
import Amazonka.SageMaker.Types.TrainingJobDefinition
import Amazonka.SageMaker.Types.TrainingJobEarlyStoppingType
import Amazonka.SageMaker.Types.TrainingJobSortByOptions
import Amazonka.SageMaker.Types.TrainingJobStatus
import Amazonka.SageMaker.Types.TrainingJobStatusCounters
import Amazonka.SageMaker.Types.TrainingJobStepMetadata
import Amazonka.SageMaker.Types.TrainingJobSummary
import Amazonka.SageMaker.Types.TrainingSpecification
import Amazonka.SageMaker.Types.TransformDataSource
import Amazonka.SageMaker.Types.TransformInput
import Amazonka.SageMaker.Types.TransformInstanceType
import Amazonka.SageMaker.Types.TransformJob
import Amazonka.SageMaker.Types.TransformJobDefinition
import Amazonka.SageMaker.Types.TransformJobStatus
import Amazonka.SageMaker.Types.TransformJobStepMetadata
import Amazonka.SageMaker.Types.TransformJobSummary
import Amazonka.SageMaker.Types.TransformOutput
import Amazonka.SageMaker.Types.TransformResources
import Amazonka.SageMaker.Types.TransformS3DataSource
import Amazonka.SageMaker.Types.Trial
import Amazonka.SageMaker.Types.TrialComponent
import Amazonka.SageMaker.Types.TrialComponentArtifact
import Amazonka.SageMaker.Types.TrialComponentMetricSummary
import Amazonka.SageMaker.Types.TrialComponentParameterValue
import Amazonka.SageMaker.Types.TrialComponentPrimaryStatus
import Amazonka.SageMaker.Types.TrialComponentSimpleSummary
import Amazonka.SageMaker.Types.TrialComponentSource
import Amazonka.SageMaker.Types.TrialComponentSourceDetail
import Amazonka.SageMaker.Types.TrialComponentStatus
import Amazonka.SageMaker.Types.TrialComponentSummary
import Amazonka.SageMaker.Types.TrialSource
import Amazonka.SageMaker.Types.TrialSummary
import Amazonka.SageMaker.Types.TuningJobCompletionCriteria
import Amazonka.SageMaker.Types.TuningJobStepMetaData
import Amazonka.SageMaker.Types.USD
import Amazonka.SageMaker.Types.UiConfig
import Amazonka.SageMaker.Types.UiTemplate
import Amazonka.SageMaker.Types.UiTemplateInfo
import Amazonka.SageMaker.Types.UserContext
import Amazonka.SageMaker.Types.UserProfileDetails
import Amazonka.SageMaker.Types.UserProfileSortKey
import Amazonka.SageMaker.Types.UserProfileStatus
import Amazonka.SageMaker.Types.UserSettings
import Amazonka.SageMaker.Types.VariantProperty
import Amazonka.SageMaker.Types.VariantPropertyType
import Amazonka.SageMaker.Types.VpcConfig
import Amazonka.SageMaker.Types.Workforce
import Amazonka.SageMaker.Types.Workteam
import qualified Amazonka.Sign.V4 as Sign

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

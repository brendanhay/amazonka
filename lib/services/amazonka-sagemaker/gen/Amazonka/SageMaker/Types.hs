{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceNotFound,
    _ResourceLimitExceeded,
    _ConflictException,
    _ResourceInUse,

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

    -- * AppSecurityGroupManagement
    AppSecurityGroupManagement (..),

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

    -- * AutoMLChannelType
    AutoMLChannelType (..),

    -- * AutoMLJobObjectiveType
    AutoMLJobObjectiveType (..),

    -- * AutoMLJobSecondaryStatus
    AutoMLJobSecondaryStatus (..),

    -- * AutoMLJobStatus
    AutoMLJobStatus (..),

    -- * AutoMLMetricEnum
    AutoMLMetricEnum (..),

    -- * AutoMLMetricExtendedEnum
    AutoMLMetricExtendedEnum (..),

    -- * AutoMLMode
    AutoMLMode (..),

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

    -- * ClarifyFeatureType
    ClarifyFeatureType (..),

    -- * ClarifyTextGranularity
    ClarifyTextGranularity (..),

    -- * ClarifyTextLanguage
    ClarifyTextLanguage (..),

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

    -- * DeviceDeploymentStatus
    DeviceDeploymentStatus (..),

    -- * DeviceSubsetType
    DeviceSubsetType (..),

    -- * DirectInternetAccess
    DirectInternetAccess (..),

    -- * Direction
    Direction (..),

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

    -- * ExecutionRoleIdentityConfig
    ExecutionRoleIdentityConfig (..),

    -- * ExecutionStatus
    ExecutionStatus (..),

    -- * FailureHandlingPolicy
    FailureHandlingPolicy (..),

    -- * FeatureGroupSortBy
    FeatureGroupSortBy (..),

    -- * FeatureGroupSortOrder
    FeatureGroupSortOrder (..),

    -- * FeatureGroupStatus
    FeatureGroupStatus (..),

    -- * FeatureStatus
    FeatureStatus (..),

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

    -- * HyperParameterTuningAllocationStrategy
    HyperParameterTuningAllocationStrategy (..),

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

    -- * LastUpdateStatusValue
    LastUpdateStatusValue (..),

    -- * LineageType
    LineageType (..),

    -- * ListCompilationJobsSortBy
    ListCompilationJobsSortBy (..),

    -- * ListDeviceFleetsSortBy
    ListDeviceFleetsSortBy (..),

    -- * ListEdgeDeploymentPlansSortBy
    ListEdgeDeploymentPlansSortBy (..),

    -- * ListEdgePackagingJobsSortBy
    ListEdgePackagingJobsSortBy (..),

    -- * ListInferenceRecommendationsJobsSortBy
    ListInferenceRecommendationsJobsSortBy (..),

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

    -- * ModelMetadataFilterType
    ModelMetadataFilterType (..),

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

    -- * RStudioServerProAccessStatus
    RStudioServerProAccessStatus (..),

    -- * RStudioServerProUserGroup
    RStudioServerProUserGroup (..),

    -- * RecommendationJobStatus
    RecommendationJobStatus (..),

    -- * RecommendationJobType
    RecommendationJobType (..),

    -- * RecommendationStepType
    RecommendationStepType (..),

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

    -- * SortLineageGroupsBy
    SortLineageGroupsBy (..),

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

    -- * StageStatus
    StageStatus (..),

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

    -- * TrafficType
    TrafficType (..),

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

    -- * VariantStatus
    VariantStatus (..),

    -- * WarmPoolResourceStatus
    WarmPoolResourceStatus (..),

    -- * WorkforceStatus
    WorkforceStatus (..),

    -- * ActionSource
    ActionSource (..),
    newActionSource,
    actionSource_sourceId,
    actionSource_sourceType,
    actionSource_sourceUri,

    -- * ActionSummary
    ActionSummary (..),
    newActionSummary,
    actionSummary_actionName,
    actionSummary_actionType,
    actionSummary_status,
    actionSummary_lastModifiedTime,
    actionSummary_source,
    actionSummary_actionArn,
    actionSummary_creationTime,

    -- * AdditionalInferenceSpecificationDefinition
    AdditionalInferenceSpecificationDefinition (..),
    newAdditionalInferenceSpecificationDefinition,
    additionalInferenceSpecificationDefinition_description,
    additionalInferenceSpecificationDefinition_supportedContentTypes,
    additionalInferenceSpecificationDefinition_supportedResponseMIMETypes,
    additionalInferenceSpecificationDefinition_supportedRealtimeInferenceInstanceTypes,
    additionalInferenceSpecificationDefinition_supportedTransformInstanceTypes,
    additionalInferenceSpecificationDefinition_name,
    additionalInferenceSpecificationDefinition_containers,

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
    algorithmSpecification_containerEntrypoint,
    algorithmSpecification_algorithmName,
    algorithmSpecification_metricDefinitions,
    algorithmSpecification_containerArguments,
    algorithmSpecification_trainingImage,
    algorithmSpecification_enableSageMakerMetricsTimeSeries,
    algorithmSpecification_trainingInputMode,

    -- * AlgorithmStatusDetails
    AlgorithmStatusDetails (..),
    newAlgorithmStatusDetails,
    algorithmStatusDetails_validationStatuses,
    algorithmStatusDetails_imageScanStatuses,

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
    appDetails_appName,
    appDetails_appType,
    appDetails_status,
    appDetails_userProfileName,
    appDetails_creationTime,
    appDetails_domainId,

    -- * AppImageConfigDetails
    AppImageConfigDetails (..),
    newAppImageConfigDetails,
    appImageConfigDetails_appImageConfigArn,
    appImageConfigDetails_appImageConfigName,
    appImageConfigDetails_kernelGatewayImageConfig,
    appImageConfigDetails_lastModifiedTime,
    appImageConfigDetails_creationTime,

    -- * AppSpecification
    AppSpecification (..),
    newAppSpecification,
    appSpecification_containerEntrypoint,
    appSpecification_containerArguments,
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
    artifactSummary_artifactName,
    artifactSummary_artifactType,
    artifactSummary_artifactArn,
    artifactSummary_lastModifiedTime,
    artifactSummary_source,
    artifactSummary_creationTime,

    -- * AssociationSummary
    AssociationSummary (..),
    newAssociationSummary,
    associationSummary_associationType,
    associationSummary_sourceArn,
    associationSummary_sourceName,
    associationSummary_destinationName,
    associationSummary_destinationType,
    associationSummary_sourceType,
    associationSummary_creationTime,
    associationSummary_createdBy,
    associationSummary_destinationArn,

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
    asyncInferenceOutputConfig_kmsKeyId,
    asyncInferenceOutputConfig_notificationConfig,
    asyncInferenceOutputConfig_s3OutputPath,

    -- * AthenaDatasetDefinition
    AthenaDatasetDefinition (..),
    newAthenaDatasetDefinition,
    athenaDatasetDefinition_workGroup,
    athenaDatasetDefinition_kmsKeyId,
    athenaDatasetDefinition_outputCompression,
    athenaDatasetDefinition_catalog,
    athenaDatasetDefinition_database,
    athenaDatasetDefinition_queryString,
    athenaDatasetDefinition_outputS3Uri,
    athenaDatasetDefinition_outputFormat,

    -- * AutoMLCandidate
    AutoMLCandidate (..),
    newAutoMLCandidate,
    autoMLCandidate_finalAutoMLJobObjectiveMetric,
    autoMLCandidate_candidateProperties,
    autoMLCandidate_inferenceContainers,
    autoMLCandidate_endTime,
    autoMLCandidate_failureReason,
    autoMLCandidate_candidateName,
    autoMLCandidate_objectiveStatus,
    autoMLCandidate_candidateSteps,
    autoMLCandidate_candidateStatus,
    autoMLCandidate_creationTime,
    autoMLCandidate_lastModifiedTime,

    -- * AutoMLCandidateGenerationConfig
    AutoMLCandidateGenerationConfig (..),
    newAutoMLCandidateGenerationConfig,
    autoMLCandidateGenerationConfig_featureSpecificationS3Uri,

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
    autoMLChannel_channelType,
    autoMLChannel_contentType,
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

    -- * AutoMLDataSplitConfig
    AutoMLDataSplitConfig (..),
    newAutoMLDataSplitConfig,
    autoMLDataSplitConfig_validationFraction,

    -- * AutoMLJobArtifacts
    AutoMLJobArtifacts (..),
    newAutoMLJobArtifacts,
    autoMLJobArtifacts_dataExplorationNotebookLocation,
    autoMLJobArtifacts_candidateDefinitionNotebookLocation,

    -- * AutoMLJobCompletionCriteria
    AutoMLJobCompletionCriteria (..),
    newAutoMLJobCompletionCriteria,
    autoMLJobCompletionCriteria_maxCandidates,
    autoMLJobCompletionCriteria_maxRuntimePerTrainingJobInSeconds,
    autoMLJobCompletionCriteria_maxAutoMLJobRuntimeInSeconds,

    -- * AutoMLJobConfig
    AutoMLJobConfig (..),
    newAutoMLJobConfig,
    autoMLJobConfig_dataSplitConfig,
    autoMLJobConfig_completionCriteria,
    autoMLJobConfig_candidateGenerationConfig,
    autoMLJobConfig_securityConfig,
    autoMLJobConfig_mode,

    -- * AutoMLJobObjective
    AutoMLJobObjective (..),
    newAutoMLJobObjective,
    autoMLJobObjective_metricName,

    -- * AutoMLJobSummary
    AutoMLJobSummary (..),
    newAutoMLJobSummary,
    autoMLJobSummary_partialFailureReasons,
    autoMLJobSummary_endTime,
    autoMLJobSummary_failureReason,
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

    -- * BatchDataCaptureConfig
    BatchDataCaptureConfig (..),
    newBatchDataCaptureConfig,
    batchDataCaptureConfig_kmsKeyId,
    batchDataCaptureConfig_generateInferenceId,
    batchDataCaptureConfig_destinationS3Uri,

    -- * BatchDescribeModelPackageError
    BatchDescribeModelPackageError (..),
    newBatchDescribeModelPackageError,
    batchDescribeModelPackageError_errorCode,
    batchDescribeModelPackageError_errorResponse,

    -- * BatchDescribeModelPackageSummary
    BatchDescribeModelPackageSummary (..),
    newBatchDescribeModelPackageSummary,
    batchDescribeModelPackageSummary_modelPackageVersion,
    batchDescribeModelPackageSummary_modelApprovalStatus,
    batchDescribeModelPackageSummary_modelPackageDescription,
    batchDescribeModelPackageSummary_modelPackageGroupName,
    batchDescribeModelPackageSummary_modelPackageArn,
    batchDescribeModelPackageSummary_creationTime,
    batchDescribeModelPackageSummary_inferenceSpecification,
    batchDescribeModelPackageSummary_modelPackageStatus,

    -- * BatchTransformInput
    BatchTransformInput (..),
    newBatchTransformInput,
    batchTransformInput_probabilityThresholdAttribute,
    batchTransformInput_s3InputMode,
    batchTransformInput_s3DataDistributionType,
    batchTransformInput_probabilityAttribute,
    batchTransformInput_startTimeOffset,
    batchTransformInput_featuresAttribute,
    batchTransformInput_inferenceAttribute,
    batchTransformInput_endTimeOffset,
    batchTransformInput_dataCapturedDestinationS3Uri,
    batchTransformInput_datasetFormat,
    batchTransformInput_localPath,

    -- * Bias
    Bias (..),
    newBias,
    bias_preTrainingReport,
    bias_postTrainingReport,
    bias_report,

    -- * BlueGreenUpdatePolicy
    BlueGreenUpdatePolicy (..),
    newBlueGreenUpdatePolicy,
    blueGreenUpdatePolicy_terminationWaitInSeconds,
    blueGreenUpdatePolicy_maximumExecutionTimeoutInSeconds,
    blueGreenUpdatePolicy_trafficRoutingConfiguration,

    -- * CacheHitResult
    CacheHitResult (..),
    newCacheHitResult,
    cacheHitResult_sourcePipelineExecutionArn,

    -- * CallbackStepMetadata
    CallbackStepMetadata (..),
    newCallbackStepMetadata,
    callbackStepMetadata_outputParameters,
    callbackStepMetadata_callbackToken,
    callbackStepMetadata_sqsQueueUrl,

    -- * CandidateArtifactLocations
    CandidateArtifactLocations (..),
    newCandidateArtifactLocations,
    candidateArtifactLocations_modelInsights,
    candidateArtifactLocations_explainability,

    -- * CandidateProperties
    CandidateProperties (..),
    newCandidateProperties,
    candidateProperties_candidateMetrics,
    candidateProperties_candidateArtifactLocations,

    -- * CanvasAppSettings
    CanvasAppSettings (..),
    newCanvasAppSettings,
    canvasAppSettings_timeSeriesForecastingSettings,

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

    -- * CategoricalParameter
    CategoricalParameter (..),
    newCategoricalParameter,
    categoricalParameter_name,
    categoricalParameter_value,

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
    channel_inputMode,
    channel_compressionType,
    channel_recordWrapperType,
    channel_contentType,
    channel_channelName,
    channel_dataSource,

    -- * ChannelSpecification
    ChannelSpecification (..),
    newChannelSpecification,
    channelSpecification_supportedCompressionTypes,
    channelSpecification_description,
    channelSpecification_isRequired,
    channelSpecification_name,
    channelSpecification_supportedContentTypes,
    channelSpecification_supportedInputModes,

    -- * CheckpointConfig
    CheckpointConfig (..),
    newCheckpointConfig,
    checkpointConfig_localPath,
    checkpointConfig_s3Uri,

    -- * ClarifyCheckStepMetadata
    ClarifyCheckStepMetadata (..),
    newClarifyCheckStepMetadata,
    clarifyCheckStepMetadata_modelPackageGroupName,
    clarifyCheckStepMetadata_checkJobArn,
    clarifyCheckStepMetadata_checkType,
    clarifyCheckStepMetadata_registerNewBaseline,
    clarifyCheckStepMetadata_skipCheck,
    clarifyCheckStepMetadata_baselineUsedForDriftCheckConstraints,
    clarifyCheckStepMetadata_calculatedBaselineConstraints,
    clarifyCheckStepMetadata_violationReport,

    -- * ClarifyExplainerConfig
    ClarifyExplainerConfig (..),
    newClarifyExplainerConfig,
    clarifyExplainerConfig_enableExplanations,
    clarifyExplainerConfig_inferenceConfig,
    clarifyExplainerConfig_shapConfig,

    -- * ClarifyInferenceConfig
    ClarifyInferenceConfig (..),
    newClarifyInferenceConfig,
    clarifyInferenceConfig_probabilityIndex,
    clarifyInferenceConfig_maxRecordCount,
    clarifyInferenceConfig_labelAttribute,
    clarifyInferenceConfig_labelIndex,
    clarifyInferenceConfig_contentTemplate,
    clarifyInferenceConfig_probabilityAttribute,
    clarifyInferenceConfig_featureTypes,
    clarifyInferenceConfig_featuresAttribute,
    clarifyInferenceConfig_maxPayloadInMB,
    clarifyInferenceConfig_labelHeaders,
    clarifyInferenceConfig_featureHeaders,

    -- * ClarifyShapBaselineConfig
    ClarifyShapBaselineConfig (..),
    newClarifyShapBaselineConfig,
    clarifyShapBaselineConfig_shapBaseline,
    clarifyShapBaselineConfig_shapBaselineUri,
    clarifyShapBaselineConfig_mimeType,

    -- * ClarifyShapConfig
    ClarifyShapConfig (..),
    newClarifyShapConfig,
    clarifyShapConfig_seed,
    clarifyShapConfig_textConfig,
    clarifyShapConfig_useLogit,
    clarifyShapConfig_numberOfSamples,
    clarifyShapConfig_shapBaselineConfig,

    -- * ClarifyTextConfig
    ClarifyTextConfig (..),
    newClarifyTextConfig,
    clarifyTextConfig_language,
    clarifyTextConfig_granularity,

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
    collectionConfiguration_collectionName,
    collectionConfiguration_collectionParameters,

    -- * CompilationJobSummary
    CompilationJobSummary (..),
    newCompilationJobSummary,
    compilationJobSummary_compilationEndTime,
    compilationJobSummary_compilationTargetDevice,
    compilationJobSummary_compilationTargetPlatformOs,
    compilationJobSummary_compilationStartTime,
    compilationJobSummary_lastModifiedTime,
    compilationJobSummary_compilationTargetPlatformArch,
    compilationJobSummary_compilationTargetPlatformAccelerator,
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
    containerDefinition_imageConfig,
    containerDefinition_environment,
    containerDefinition_containerHostname,
    containerDefinition_modelDataUrl,
    containerDefinition_multiModelConfig,
    containerDefinition_inferenceSpecificationName,
    containerDefinition_mode,
    containerDefinition_image,
    containerDefinition_modelPackageName,

    -- * ContextSource
    ContextSource (..),
    newContextSource,
    contextSource_sourceId,
    contextSource_sourceType,
    contextSource_sourceUri,

    -- * ContextSummary
    ContextSummary (..),
    newContextSummary,
    contextSummary_contextName,
    contextSummary_lastModifiedTime,
    contextSummary_source,
    contextSummary_creationTime,
    contextSummary_contextType,
    contextSummary_contextArn,

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
    dataProcessing_inputFilter,
    dataProcessing_joinSource,

    -- * DataQualityAppSpecification
    DataQualityAppSpecification (..),
    newDataQualityAppSpecification,
    dataQualityAppSpecification_containerEntrypoint,
    dataQualityAppSpecification_recordPreprocessorSourceUri,
    dataQualityAppSpecification_environment,
    dataQualityAppSpecification_containerArguments,
    dataQualityAppSpecification_postAnalyticsProcessorSourceUri,
    dataQualityAppSpecification_imageUri,

    -- * DataQualityBaselineConfig
    DataQualityBaselineConfig (..),
    newDataQualityBaselineConfig,
    dataQualityBaselineConfig_baseliningJobName,
    dataQualityBaselineConfig_constraintsResource,
    dataQualityBaselineConfig_statisticsResource,

    -- * DataQualityJobInput
    DataQualityJobInput (..),
    newDataQualityJobInput,
    dataQualityJobInput_endpointInput,
    dataQualityJobInput_batchTransformInput,

    -- * DataSource
    DataSource (..),
    newDataSource,
    dataSource_s3DataSource,
    dataSource_fileSystemDataSource,

    -- * DatasetDefinition
    DatasetDefinition (..),
    newDatasetDefinition,
    datasetDefinition_inputMode,
    datasetDefinition_athenaDatasetDefinition,
    datasetDefinition_dataDistributionType,
    datasetDefinition_localPath,
    datasetDefinition_redshiftDatasetDefinition,

    -- * DebugHookConfig
    DebugHookConfig (..),
    newDebugHookConfig,
    debugHookConfig_collectionConfigurations,
    debugHookConfig_localPath,
    debugHookConfig_hookParameters,
    debugHookConfig_s3OutputPath,

    -- * DebugRuleConfiguration
    DebugRuleConfiguration (..),
    newDebugRuleConfiguration,
    debugRuleConfiguration_s3OutputPath,
    debugRuleConfiguration_instanceType,
    debugRuleConfiguration_ruleParameters,
    debugRuleConfiguration_localPath,
    debugRuleConfiguration_volumeSizeInGB,
    debugRuleConfiguration_ruleConfigurationName,
    debugRuleConfiguration_ruleEvaluatorImage,

    -- * DebugRuleEvaluationStatus
    DebugRuleEvaluationStatus (..),
    newDebugRuleEvaluationStatus,
    debugRuleEvaluationStatus_statusDetails,
    debugRuleEvaluationStatus_lastModifiedTime,
    debugRuleEvaluationStatus_ruleEvaluationJobArn,
    debugRuleEvaluationStatus_ruleConfigurationName,
    debugRuleEvaluationStatus_ruleEvaluationStatus,

    -- * DeployedImage
    DeployedImage (..),
    newDeployedImage,
    deployedImage_specifiedImage,
    deployedImage_resolvedImage,
    deployedImage_resolutionTime,

    -- * DeploymentConfig
    DeploymentConfig (..),
    newDeploymentConfig,
    deploymentConfig_autoRollbackConfiguration,
    deploymentConfig_blueGreenUpdatePolicy,

    -- * DeploymentStage
    DeploymentStage (..),
    newDeploymentStage,
    deploymentStage_deploymentConfig,
    deploymentStage_stageName,
    deploymentStage_deviceSelectionConfig,

    -- * DeploymentStageStatusSummary
    DeploymentStageStatusSummary (..),
    newDeploymentStageStatusSummary,
    deploymentStageStatusSummary_stageName,
    deploymentStageStatusSummary_deviceSelectionConfig,
    deploymentStageStatusSummary_deploymentConfig,
    deploymentStageStatusSummary_deploymentStatus,

    -- * DesiredWeightAndCapacity
    DesiredWeightAndCapacity (..),
    newDesiredWeightAndCapacity,
    desiredWeightAndCapacity_desiredWeight,
    desiredWeightAndCapacity_desiredInstanceCount,
    desiredWeightAndCapacity_variantName,

    -- * Device
    Device (..),
    newDevice,
    device_iotThingName,
    device_description,
    device_deviceName,

    -- * DeviceDeploymentSummary
    DeviceDeploymentSummary (..),
    newDeviceDeploymentSummary,
    deviceDeploymentSummary_deviceDeploymentStatus,
    deviceDeploymentSummary_deviceDeploymentStatusMessage,
    deviceDeploymentSummary_deployedStageName,
    deviceDeploymentSummary_deviceFleetName,
    deviceDeploymentSummary_description,
    deviceDeploymentSummary_deploymentStartTime,
    deviceDeploymentSummary_edgeDeploymentPlanArn,
    deviceDeploymentSummary_edgeDeploymentPlanName,
    deviceDeploymentSummary_stageName,
    deviceDeploymentSummary_deviceName,
    deviceDeploymentSummary_deviceArn,

    -- * DeviceFleetSummary
    DeviceFleetSummary (..),
    newDeviceFleetSummary,
    deviceFleetSummary_lastModifiedTime,
    deviceFleetSummary_creationTime,
    deviceFleetSummary_deviceFleetArn,
    deviceFleetSummary_deviceFleetName,

    -- * DeviceSelectionConfig
    DeviceSelectionConfig (..),
    newDeviceSelectionConfig,
    deviceSelectionConfig_deviceNameContains,
    deviceSelectionConfig_percentage,
    deviceSelectionConfig_deviceNames,
    deviceSelectionConfig_deviceSubsetType,

    -- * DeviceStats
    DeviceStats (..),
    newDeviceStats,
    deviceStats_connectedDeviceCount,
    deviceStats_registeredDeviceCount,

    -- * DeviceSummary
    DeviceSummary (..),
    newDeviceSummary,
    deviceSummary_models,
    deviceSummary_iotThingName,
    deviceSummary_deviceFleetName,
    deviceSummary_description,
    deviceSummary_registrationTime,
    deviceSummary_latestHeartbeat,
    deviceSummary_agentVersion,
    deviceSummary_deviceName,
    deviceSummary_deviceArn,

    -- * DomainDetails
    DomainDetails (..),
    newDomainDetails,
    domainDetails_domainName,
    domainDetails_domainArn,
    domainDetails_url,
    domainDetails_status,
    domainDetails_lastModifiedTime,
    domainDetails_creationTime,
    domainDetails_domainId,

    -- * DomainSettings
    DomainSettings (..),
    newDomainSettings,
    domainSettings_securityGroupIds,
    domainSettings_executionRoleIdentityConfig,
    domainSettings_rStudioServerProDomainSettings,

    -- * DomainSettingsForUpdate
    DomainSettingsForUpdate (..),
    newDomainSettingsForUpdate,
    domainSettingsForUpdate_executionRoleIdentityConfig,
    domainSettingsForUpdate_rStudioServerProDomainSettingsForUpdate,

    -- * DriftCheckBaselines
    DriftCheckBaselines (..),
    newDriftCheckBaselines,
    driftCheckBaselines_modelDataQuality,
    driftCheckBaselines_modelQuality,
    driftCheckBaselines_bias,
    driftCheckBaselines_explainability,

    -- * DriftCheckBias
    DriftCheckBias (..),
    newDriftCheckBias,
    driftCheckBias_postTrainingConstraints,
    driftCheckBias_preTrainingConstraints,
    driftCheckBias_configFile,

    -- * DriftCheckExplainability
    DriftCheckExplainability (..),
    newDriftCheckExplainability,
    driftCheckExplainability_constraints,
    driftCheckExplainability_configFile,

    -- * DriftCheckModelDataQuality
    DriftCheckModelDataQuality (..),
    newDriftCheckModelDataQuality,
    driftCheckModelDataQuality_constraints,
    driftCheckModelDataQuality_statistics,

    -- * DriftCheckModelQuality
    DriftCheckModelQuality (..),
    newDriftCheckModelQuality,
    driftCheckModelQuality_constraints,
    driftCheckModelQuality_statistics,

    -- * EMRStepMetadata
    EMRStepMetadata (..),
    newEMRStepMetadata,
    eMRStepMetadata_logFilePath,
    eMRStepMetadata_stepName,
    eMRStepMetadata_clusterId,
    eMRStepMetadata_stepId,

    -- * Edge
    Edge (..),
    newEdge,
    edge_associationType,
    edge_sourceArn,
    edge_destinationArn,

    -- * EdgeDeploymentConfig
    EdgeDeploymentConfig (..),
    newEdgeDeploymentConfig,
    edgeDeploymentConfig_failureHandlingPolicy,

    -- * EdgeDeploymentModelConfig
    EdgeDeploymentModelConfig (..),
    newEdgeDeploymentModelConfig,
    edgeDeploymentModelConfig_modelHandle,
    edgeDeploymentModelConfig_edgePackagingJobName,

    -- * EdgeDeploymentPlanSummary
    EdgeDeploymentPlanSummary (..),
    newEdgeDeploymentPlanSummary,
    edgeDeploymentPlanSummary_lastModifiedTime,
    edgeDeploymentPlanSummary_creationTime,
    edgeDeploymentPlanSummary_edgeDeploymentPlanArn,
    edgeDeploymentPlanSummary_edgeDeploymentPlanName,
    edgeDeploymentPlanSummary_deviceFleetName,
    edgeDeploymentPlanSummary_edgeDeploymentSuccess,
    edgeDeploymentPlanSummary_edgeDeploymentPending,
    edgeDeploymentPlanSummary_edgeDeploymentFailed,

    -- * EdgeDeploymentStatus
    EdgeDeploymentStatus (..),
    newEdgeDeploymentStatus,
    edgeDeploymentStatus_edgeDeploymentStatusMessage,
    edgeDeploymentStatus_edgeDeploymentStageStartTime,
    edgeDeploymentStatus_stageStatus,
    edgeDeploymentStatus_edgeDeploymentSuccessInStage,
    edgeDeploymentStatus_edgeDeploymentPendingInStage,
    edgeDeploymentStatus_edgeDeploymentFailedInStage,

    -- * EdgeModel
    EdgeModel (..),
    newEdgeModel,
    edgeModel_latestSampleTime,
    edgeModel_latestInference,
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
    edgeOutputConfig_presetDeploymentConfig,
    edgeOutputConfig_kmsKeyId,
    edgeOutputConfig_s3OutputLocation,

    -- * EdgePackagingJobSummary
    EdgePackagingJobSummary (..),
    newEdgePackagingJobSummary,
    edgePackagingJobSummary_compilationJobName,
    edgePackagingJobSummary_modelVersion,
    edgePackagingJobSummary_lastModifiedTime,
    edgePackagingJobSummary_modelName,
    edgePackagingJobSummary_creationTime,
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
    endpoint_tags,
    endpoint_monitoringSchedules,
    endpoint_dataCaptureConfig,
    endpoint_productionVariants,
    endpoint_failureReason,
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

    -- * EndpointInfo
    EndpointInfo (..),
    newEndpointInfo,
    endpointInfo_endpointName,

    -- * EndpointInput
    EndpointInput (..),
    newEndpointInput,
    endpointInput_probabilityThresholdAttribute,
    endpointInput_s3InputMode,
    endpointInput_s3DataDistributionType,
    endpointInput_probabilityAttribute,
    endpointInput_startTimeOffset,
    endpointInput_featuresAttribute,
    endpointInput_inferenceAttribute,
    endpointInput_endTimeOffset,
    endpointInput_endpointName,
    endpointInput_localPath,

    -- * EndpointInputConfiguration
    EndpointInputConfiguration (..),
    newEndpointInputConfiguration,
    endpointInputConfiguration_inferenceSpecificationName,
    endpointInputConfiguration_environmentParameterRanges,
    endpointInputConfiguration_instanceType,

    -- * EndpointOutputConfiguration
    EndpointOutputConfiguration (..),
    newEndpointOutputConfiguration,
    endpointOutputConfiguration_endpointName,
    endpointOutputConfiguration_variantName,
    endpointOutputConfiguration_instanceType,
    endpointOutputConfiguration_initialInstanceCount,

    -- * EndpointPerformance
    EndpointPerformance (..),
    newEndpointPerformance,
    endpointPerformance_metrics,
    endpointPerformance_endpointInfo,

    -- * EndpointSummary
    EndpointSummary (..),
    newEndpointSummary,
    endpointSummary_endpointName,
    endpointSummary_endpointArn,
    endpointSummary_creationTime,
    endpointSummary_lastModifiedTime,
    endpointSummary_endpointStatus,

    -- * EnvironmentParameter
    EnvironmentParameter (..),
    newEnvironmentParameter,
    environmentParameter_key,
    environmentParameter_valueType,
    environmentParameter_value,

    -- * EnvironmentParameterRanges
    EnvironmentParameterRanges (..),
    newEnvironmentParameterRanges,
    environmentParameterRanges_categoricalParameterRanges,

    -- * Experiment
    Experiment (..),
    newExperiment,
    experiment_tags,
    experiment_displayName,
    experiment_description,
    experiment_lastModifiedTime,
    experiment_source,
    experiment_experimentArn,
    experiment_creationTime,
    experiment_lastModifiedBy,
    experiment_createdBy,
    experiment_experimentName,

    -- * ExperimentConfig
    ExperimentConfig (..),
    newExperimentConfig,
    experimentConfig_trialName,
    experimentConfig_trialComponentDisplayName,
    experimentConfig_experimentName,

    -- * ExperimentSource
    ExperimentSource (..),
    newExperimentSource,
    experimentSource_sourceType,
    experimentSource_sourceArn,

    -- * ExperimentSummary
    ExperimentSummary (..),
    newExperimentSummary,
    experimentSummary_displayName,
    experimentSummary_lastModifiedTime,
    experimentSummary_experimentSource,
    experimentSummary_experimentArn,
    experimentSummary_creationTime,
    experimentSummary_experimentName,

    -- * Explainability
    Explainability (..),
    newExplainability,
    explainability_report,

    -- * ExplainerConfig
    ExplainerConfig (..),
    newExplainerConfig,
    explainerConfig_clarifyExplainerConfig,

    -- * FailStepMetadata
    FailStepMetadata (..),
    newFailStepMetadata,
    failStepMetadata_errorMessage,

    -- * FeatureDefinition
    FeatureDefinition (..),
    newFeatureDefinition,
    featureDefinition_featureType,
    featureDefinition_featureName,

    -- * FeatureGroup
    FeatureGroup (..),
    newFeatureGroup,
    featureGroup_tags,
    featureGroup_recordIdentifierFeatureName,
    featureGroup_roleArn,
    featureGroup_description,
    featureGroup_offlineStoreStatus,
    featureGroup_onlineStoreConfig,
    featureGroup_lastModifiedTime,
    featureGroup_featureGroupName,
    featureGroup_lastUpdateStatus,
    featureGroup_creationTime,
    featureGroup_featureGroupStatus,
    featureGroup_offlineStoreConfig,
    featureGroup_eventTimeFeatureName,
    featureGroup_featureGroupArn,
    featureGroup_failureReason,
    featureGroup_featureDefinitions,

    -- * FeatureGroupSummary
    FeatureGroupSummary (..),
    newFeatureGroupSummary,
    featureGroupSummary_offlineStoreStatus,
    featureGroupSummary_featureGroupStatus,
    featureGroupSummary_featureGroupName,
    featureGroupSummary_featureGroupArn,
    featureGroupSummary_creationTime,

    -- * FeatureMetadata
    FeatureMetadata (..),
    newFeatureMetadata,
    featureMetadata_featureType,
    featureMetadata_featureName,
    featureMetadata_description,
    featureMetadata_lastModifiedTime,
    featureMetadata_featureGroupName,
    featureMetadata_creationTime,
    featureMetadata_featureGroupArn,
    featureMetadata_parameters,

    -- * FeatureParameter
    FeatureParameter (..),
    newFeatureParameter,
    featureParameter_key,
    featureParameter_value,

    -- * FileSource
    FileSource (..),
    newFileSource,
    fileSource_contentDigest,
    fileSource_contentType,
    fileSource_s3Uri,

    -- * FileSystemConfig
    FileSystemConfig (..),
    newFileSystemConfig,
    fileSystemConfig_defaultGid,
    fileSystemConfig_defaultUid,
    fileSystemConfig_mountPath,

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
    humanLoopConfig_publicWorkforceTaskPrice,
    humanLoopConfig_taskKeywords,
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
    humanTaskConfig_maxConcurrentTaskCount,
    humanTaskConfig_publicWorkforceTaskPrice,
    humanTaskConfig_taskKeywords,
    humanTaskConfig_taskAvailabilityLifetimeInSeconds,
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
    hyperParameterAlgorithmSpecification_metricDefinitions,
    hyperParameterAlgorithmSpecification_trainingImage,
    hyperParameterAlgorithmSpecification_trainingInputMode,

    -- * HyperParameterSpecification
    HyperParameterSpecification (..),
    newHyperParameterSpecification,
    hyperParameterSpecification_defaultValue,
    hyperParameterSpecification_description,
    hyperParameterSpecification_isTunable,
    hyperParameterSpecification_range,
    hyperParameterSpecification_isRequired,
    hyperParameterSpecification_name,
    hyperParameterSpecification_type,

    -- * HyperParameterTrainingJobDefinition
    HyperParameterTrainingJobDefinition (..),
    newHyperParameterTrainingJobDefinition,
    hyperParameterTrainingJobDefinition_tuningObjective,
    hyperParameterTrainingJobDefinition_enableManagedSpotTraining,
    hyperParameterTrainingJobDefinition_hyperParameterTuningResourceConfig,
    hyperParameterTrainingJobDefinition_hyperParameterRanges,
    hyperParameterTrainingJobDefinition_retryStrategy,
    hyperParameterTrainingJobDefinition_vpcConfig,
    hyperParameterTrainingJobDefinition_enableNetworkIsolation,
    hyperParameterTrainingJobDefinition_resourceConfig,
    hyperParameterTrainingJobDefinition_definitionName,
    hyperParameterTrainingJobDefinition_staticHyperParameters,
    hyperParameterTrainingJobDefinition_checkpointConfig,
    hyperParameterTrainingJobDefinition_enableInterContainerTrafficEncryption,
    hyperParameterTrainingJobDefinition_inputDataConfig,
    hyperParameterTrainingJobDefinition_algorithmSpecification,
    hyperParameterTrainingJobDefinition_roleArn,
    hyperParameterTrainingJobDefinition_outputDataConfig,
    hyperParameterTrainingJobDefinition_stoppingCondition,

    -- * HyperParameterTrainingJobSummary
    HyperParameterTrainingJobSummary (..),
    newHyperParameterTrainingJobSummary,
    hyperParameterTrainingJobSummary_trainingStartTime,
    hyperParameterTrainingJobSummary_objectiveStatus,
    hyperParameterTrainingJobSummary_tuningJobName,
    hyperParameterTrainingJobSummary_trainingJobDefinitionName,
    hyperParameterTrainingJobSummary_finalHyperParameterTuningJobObjectiveMetric,
    hyperParameterTrainingJobSummary_failureReason,
    hyperParameterTrainingJobSummary_trainingEndTime,
    hyperParameterTrainingJobSummary_trainingJobName,
    hyperParameterTrainingJobSummary_trainingJobArn,
    hyperParameterTrainingJobSummary_creationTime,
    hyperParameterTrainingJobSummary_trainingJobStatus,
    hyperParameterTrainingJobSummary_tunedHyperParameters,

    -- * HyperParameterTuningInstanceConfig
    HyperParameterTuningInstanceConfig (..),
    newHyperParameterTuningInstanceConfig,
    hyperParameterTuningInstanceConfig_instanceType,
    hyperParameterTuningInstanceConfig_instanceCount,
    hyperParameterTuningInstanceConfig_volumeSizeInGB,

    -- * HyperParameterTuningJobConfig
    HyperParameterTuningJobConfig (..),
    newHyperParameterTuningJobConfig,
    hyperParameterTuningJobConfig_tuningJobCompletionCriteria,
    hyperParameterTuningJobConfig_trainingJobEarlyStoppingType,
    hyperParameterTuningJobConfig_hyperParameterTuningJobObjective,
    hyperParameterTuningJobConfig_parameterRanges,
    hyperParameterTuningJobConfig_strategyConfig,
    hyperParameterTuningJobConfig_strategy,
    hyperParameterTuningJobConfig_resourceLimits,

    -- * HyperParameterTuningJobObjective
    HyperParameterTuningJobObjective (..),
    newHyperParameterTuningJobObjective,
    hyperParameterTuningJobObjective_type,
    hyperParameterTuningJobObjective_metricName,

    -- * HyperParameterTuningJobSearchEntity
    HyperParameterTuningJobSearchEntity (..),
    newHyperParameterTuningJobSearchEntity,
    hyperParameterTuningJobSearchEntity_tags,
    hyperParameterTuningJobSearchEntity_overallBestTrainingJob,
    hyperParameterTuningJobSearchEntity_bestTrainingJob,
    hyperParameterTuningJobSearchEntity_trainingJobDefinitions,
    hyperParameterTuningJobSearchEntity_hyperParameterTuningJobStatus,
    hyperParameterTuningJobSearchEntity_lastModifiedTime,
    hyperParameterTuningJobSearchEntity_hyperParameterTuningJobName,
    hyperParameterTuningJobSearchEntity_hyperParameterTuningJobArn,
    hyperParameterTuningJobSearchEntity_creationTime,
    hyperParameterTuningJobSearchEntity_warmStartConfig,
    hyperParameterTuningJobSearchEntity_hyperParameterTuningJobConfig,
    hyperParameterTuningJobSearchEntity_trainingJobStatusCounters,
    hyperParameterTuningJobSearchEntity_hyperParameterTuningEndTime,
    hyperParameterTuningJobSearchEntity_objectiveStatusCounters,
    hyperParameterTuningJobSearchEntity_trainingJobDefinition,
    hyperParameterTuningJobSearchEntity_failureReason,

    -- * HyperParameterTuningJobStrategyConfig
    HyperParameterTuningJobStrategyConfig (..),
    newHyperParameterTuningJobStrategyConfig,
    hyperParameterTuningJobStrategyConfig_hyperbandStrategyConfig,

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

    -- * HyperParameterTuningResourceConfig
    HyperParameterTuningResourceConfig (..),
    newHyperParameterTuningResourceConfig,
    hyperParameterTuningResourceConfig_volumeKmsKeyId,
    hyperParameterTuningResourceConfig_instanceType,
    hyperParameterTuningResourceConfig_allocationStrategy,
    hyperParameterTuningResourceConfig_instanceCount,
    hyperParameterTuningResourceConfig_instanceConfigs,
    hyperParameterTuningResourceConfig_volumeSizeInGB,

    -- * HyperbandStrategyConfig
    HyperbandStrategyConfig (..),
    newHyperbandStrategyConfig,
    hyperbandStrategyConfig_minResource,
    hyperbandStrategyConfig_maxResource,

    -- * Image
    Image (..),
    newImage,
    image_displayName,
    image_description,
    image_failureReason,
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

    -- * InferenceMetrics
    InferenceMetrics (..),
    newInferenceMetrics,
    inferenceMetrics_maxInvocations,
    inferenceMetrics_modelLatency,

    -- * InferenceRecommendation
    InferenceRecommendation (..),
    newInferenceRecommendation,
    inferenceRecommendation_metrics,
    inferenceRecommendation_endpointConfiguration,
    inferenceRecommendation_modelConfiguration,

    -- * InferenceRecommendationsJob
    InferenceRecommendationsJob (..),
    newInferenceRecommendationsJob,
    inferenceRecommendationsJob_completionTime,
    inferenceRecommendationsJob_failureReason,
    inferenceRecommendationsJob_jobName,
    inferenceRecommendationsJob_jobDescription,
    inferenceRecommendationsJob_jobType,
    inferenceRecommendationsJob_jobArn,
    inferenceRecommendationsJob_status,
    inferenceRecommendationsJob_creationTime,
    inferenceRecommendationsJob_roleArn,
    inferenceRecommendationsJob_lastModifiedTime,

    -- * InferenceRecommendationsJobStep
    InferenceRecommendationsJobStep (..),
    newInferenceRecommendationsJobStep,
    inferenceRecommendationsJobStep_inferenceBenchmark,
    inferenceRecommendationsJobStep_stepType,
    inferenceRecommendationsJobStep_jobName,
    inferenceRecommendationsJobStep_status,

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

    -- * InstanceGroup
    InstanceGroup (..),
    newInstanceGroup,
    instanceGroup_instanceType,
    instanceGroup_instanceCount,
    instanceGroup_instanceGroupName,

    -- * InstanceMetadataServiceConfiguration
    InstanceMetadataServiceConfiguration (..),
    newInstanceMetadataServiceConfiguration,
    instanceMetadataServiceConfiguration_minimumInstanceMetadataServiceVersion,

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
    jupyterServerAppSettings_lifecycleConfigArns,
    jupyterServerAppSettings_defaultResourceSpec,

    -- * KernelGatewayAppSettings
    KernelGatewayAppSettings (..),
    newKernelGatewayAppSettings,
    kernelGatewayAppSettings_lifecycleConfigArns,
    kernelGatewayAppSettings_defaultResourceSpec,
    kernelGatewayAppSettings_customImages,

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
    labelCounters_unlabeled,
    labelCounters_failedNonRetryableError,
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
    labelingJobAlgorithmsConfig_initialActiveLearningModelArn,
    labelingJobAlgorithmsConfig_labelingJobResourceConfig,
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
    labelingJobForWorkteamSummary_labelingJobName,
    labelingJobForWorkteamSummary_labelCounters,
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
    labelingJobResourceConfig_vpcConfig,
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
    labelingJobStoppingConditions_maxPercentageOfInputDatasetLabeled,
    labelingJobStoppingConditions_maxHumanLabeledObjectCount,

    -- * LabelingJobSummary
    LabelingJobSummary (..),
    newLabelingJobSummary,
    labelingJobSummary_labelingJobOutput,
    labelingJobSummary_annotationConsolidationLambdaArn,
    labelingJobSummary_inputConfig,
    labelingJobSummary_failureReason,
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
    lambdaStepMetadata_outputParameters,
    lambdaStepMetadata_arn,

    -- * LastUpdateStatus
    LastUpdateStatus (..),
    newLastUpdateStatus,
    lastUpdateStatus_failureReason,
    lastUpdateStatus_status,

    -- * LineageGroupSummary
    LineageGroupSummary (..),
    newLineageGroupSummary,
    lineageGroupSummary_lineageGroupName,
    lineageGroupSummary_displayName,
    lineageGroupSummary_lastModifiedTime,
    lineageGroupSummary_lineageGroupArn,
    lineageGroupSummary_creationTime,

    -- * MemberDefinition
    MemberDefinition (..),
    newMemberDefinition,
    memberDefinition_cognitoMemberDefinition,
    memberDefinition_oidcMemberDefinition,

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
    metricData_timestamp,
    metricData_metricName,
    metricData_value,

    -- * MetricDatum
    MetricDatum (..),
    newMetricDatum,
    metricDatum_metricName,
    metricDatum_set,
    metricDatum_standardMetricName,
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
    modelBiasBaselineConfig_baseliningJobName,
    modelBiasBaselineConfig_constraintsResource,

    -- * ModelBiasJobInput
    ModelBiasJobInput (..),
    newModelBiasJobInput,
    modelBiasJobInput_endpointInput,
    modelBiasJobInput_batchTransformInput,
    modelBiasJobInput_groundTruthS3Input,

    -- * ModelClientConfig
    ModelClientConfig (..),
    newModelClientConfig,
    modelClientConfig_invocationsMaxRetries,
    modelClientConfig_invocationsTimeoutInSeconds,

    -- * ModelConfiguration
    ModelConfiguration (..),
    newModelConfiguration,
    modelConfiguration_inferenceSpecificationName,
    modelConfiguration_environmentParameters,

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
    modelExplainabilityBaselineConfig_baseliningJobName,
    modelExplainabilityBaselineConfig_constraintsResource,

    -- * ModelExplainabilityJobInput
    ModelExplainabilityJobInput (..),
    newModelExplainabilityJobInput,
    modelExplainabilityJobInput_endpointInput,
    modelExplainabilityJobInput_batchTransformInput,

    -- * ModelInput
    ModelInput (..),
    newModelInput,
    modelInput_dataInputConfig,

    -- * ModelLatencyThreshold
    ModelLatencyThreshold (..),
    newModelLatencyThreshold,
    modelLatencyThreshold_valueInMilliseconds,
    modelLatencyThreshold_percentile,

    -- * ModelMetadataFilter
    ModelMetadataFilter (..),
    newModelMetadataFilter,
    modelMetadataFilter_name,
    modelMetadataFilter_value,

    -- * ModelMetadataSearchExpression
    ModelMetadataSearchExpression (..),
    newModelMetadataSearchExpression,
    modelMetadataSearchExpression_filters,

    -- * ModelMetadataSummary
    ModelMetadataSummary (..),
    newModelMetadataSummary,
    modelMetadataSummary_domain,
    modelMetadataSummary_framework,
    modelMetadataSummary_task,
    modelMetadataSummary_model,
    modelMetadataSummary_frameworkVersion,

    -- * ModelMetrics
    ModelMetrics (..),
    newModelMetrics,
    modelMetrics_modelDataQuality,
    modelMetrics_modelQuality,
    modelMetrics_bias,
    modelMetrics_explainability,

    -- * ModelPackage
    ModelPackage (..),
    newModelPackage,
    modelPackage_tags,
    modelPackage_modelPackageVersion,
    modelPackage_modelPackageGroupName,
    modelPackage_sourceAlgorithmSpecification,
    modelPackage_validationSpecification,
    modelPackage_samplePayloadUrl,
    modelPackage_task,
    modelPackage_certifyForMarketplace,
    modelPackage_inferenceSpecification,
    modelPackage_modelApprovalStatus,
    modelPackage_metadataProperties,
    modelPackage_domain,
    modelPackage_modelPackageDescription,
    modelPackage_driftCheckBaselines,
    modelPackage_approvalDescription,
    modelPackage_modelPackageArn,
    modelPackage_lastModifiedTime,
    modelPackage_modelPackageStatus,
    modelPackage_modelMetrics,
    modelPackage_modelPackageStatusDetails,
    modelPackage_creationTime,
    modelPackage_lastModifiedBy,
    modelPackage_additionalInferenceSpecifications,
    modelPackage_createdBy,
    modelPackage_customerMetadataProperties,
    modelPackage_modelPackageName,

    -- * ModelPackageContainerDefinition
    ModelPackageContainerDefinition (..),
    newModelPackageContainerDefinition,
    modelPackageContainerDefinition_environment,
    modelPackageContainerDefinition_containerHostname,
    modelPackageContainerDefinition_modelDataUrl,
    modelPackageContainerDefinition_modelInput,
    modelPackageContainerDefinition_productId,
    modelPackageContainerDefinition_nearestModelName,
    modelPackageContainerDefinition_frameworkVersion,
    modelPackageContainerDefinition_imageDigest,
    modelPackageContainerDefinition_framework,
    modelPackageContainerDefinition_image,

    -- * ModelPackageGroup
    ModelPackageGroup (..),
    newModelPackageGroup,
    modelPackageGroup_tags,
    modelPackageGroup_modelPackageGroupName,
    modelPackageGroup_modelPackageGroupArn,
    modelPackageGroup_modelPackageGroupStatus,
    modelPackageGroup_modelPackageGroupDescription,
    modelPackageGroup_creationTime,
    modelPackageGroup_createdBy,

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
    modelPackageSummary_modelPackageVersion,
    modelPackageSummary_modelPackageGroupName,
    modelPackageSummary_modelApprovalStatus,
    modelPackageSummary_modelPackageDescription,
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
    modelQualityAppSpecification_containerEntrypoint,
    modelQualityAppSpecification_recordPreprocessorSourceUri,
    modelQualityAppSpecification_environment,
    modelQualityAppSpecification_containerArguments,
    modelQualityAppSpecification_postAnalyticsProcessorSourceUri,
    modelQualityAppSpecification_problemType,
    modelQualityAppSpecification_imageUri,

    -- * ModelQualityBaselineConfig
    ModelQualityBaselineConfig (..),
    newModelQualityBaselineConfig,
    modelQualityBaselineConfig_baseliningJobName,
    modelQualityBaselineConfig_constraintsResource,

    -- * ModelQualityJobInput
    ModelQualityJobInput (..),
    newModelQualityJobInput,
    modelQualityJobInput_endpointInput,
    modelQualityJobInput_batchTransformInput,
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
    monitoringAppSpecification_containerEntrypoint,
    monitoringAppSpecification_recordPreprocessorSourceUri,
    monitoringAppSpecification_containerArguments,
    monitoringAppSpecification_postAnalyticsProcessorSourceUri,
    monitoringAppSpecification_imageUri,

    -- * MonitoringBaselineConfig
    MonitoringBaselineConfig (..),
    newMonitoringBaselineConfig,
    monitoringBaselineConfig_baseliningJobName,
    monitoringBaselineConfig_constraintsResource,
    monitoringBaselineConfig_statisticsResource,

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

    -- * MonitoringCsvDatasetFormat
    MonitoringCsvDatasetFormat (..),
    newMonitoringCsvDatasetFormat,
    monitoringCsvDatasetFormat_header,

    -- * MonitoringDatasetFormat
    MonitoringDatasetFormat (..),
    newMonitoringDatasetFormat,
    monitoringDatasetFormat_parquet,
    monitoringDatasetFormat_json,
    monitoringDatasetFormat_csv,

    -- * MonitoringExecutionSummary
    MonitoringExecutionSummary (..),
    newMonitoringExecutionSummary,
    monitoringExecutionSummary_endpointName,
    monitoringExecutionSummary_processingJobArn,
    monitoringExecutionSummary_monitoringType,
    monitoringExecutionSummary_failureReason,
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
    monitoringInput_batchTransformInput,

    -- * MonitoringJobDefinition
    MonitoringJobDefinition (..),
    newMonitoringJobDefinition,
    monitoringJobDefinition_environment,
    monitoringJobDefinition_baselineConfig,
    monitoringJobDefinition_networkConfig,
    monitoringJobDefinition_stoppingCondition,
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

    -- * MonitoringJsonDatasetFormat
    MonitoringJsonDatasetFormat (..),
    newMonitoringJsonDatasetFormat,
    monitoringJsonDatasetFormat_line,

    -- * MonitoringNetworkConfig
    MonitoringNetworkConfig (..),
    newMonitoringNetworkConfig,
    monitoringNetworkConfig_vpcConfig,
    monitoringNetworkConfig_enableNetworkIsolation,
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

    -- * MonitoringParquetDatasetFormat
    MonitoringParquetDatasetFormat (..),
    newMonitoringParquetDatasetFormat,

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
    monitoringSchedule_tags,
    monitoringSchedule_monitoringScheduleArn,
    monitoringSchedule_endpointName,
    monitoringSchedule_monitoringScheduleStatus,
    monitoringSchedule_monitoringScheduleConfig,
    monitoringSchedule_monitoringScheduleName,
    monitoringSchedule_lastModifiedTime,
    monitoringSchedule_monitoringType,
    monitoringSchedule_creationTime,
    monitoringSchedule_lastMonitoringExecutionSummary,
    monitoringSchedule_failureReason,

    -- * MonitoringScheduleConfig
    MonitoringScheduleConfig (..),
    newMonitoringScheduleConfig,
    monitoringScheduleConfig_scheduleConfig,
    monitoringScheduleConfig_monitoringJobDefinition,
    monitoringScheduleConfig_monitoringType,
    monitoringScheduleConfig_monitoringJobDefinitionName,

    -- * MonitoringScheduleSummary
    MonitoringScheduleSummary (..),
    newMonitoringScheduleSummary,
    monitoringScheduleSummary_endpointName,
    monitoringScheduleSummary_monitoringType,
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
    networkConfig_vpcConfig,
    networkConfig_enableNetworkIsolation,
    networkConfig_enableInterContainerTrafficEncryption,

    -- * NotebookInstanceLifecycleConfigSummary
    NotebookInstanceLifecycleConfigSummary (..),
    newNotebookInstanceLifecycleConfigSummary,
    notebookInstanceLifecycleConfigSummary_lastModifiedTime,
    notebookInstanceLifecycleConfigSummary_creationTime,
    notebookInstanceLifecycleConfigSummary_notebookInstanceLifecycleConfigName,
    notebookInstanceLifecycleConfigSummary_notebookInstanceLifecycleConfigArn,

    -- * NotebookInstanceLifecycleHook
    NotebookInstanceLifecycleHook (..),
    newNotebookInstanceLifecycleHook,
    notebookInstanceLifecycleHook_content,

    -- * NotebookInstanceSummary
    NotebookInstanceSummary (..),
    newNotebookInstanceSummary,
    notebookInstanceSummary_notebookInstanceLifecycleConfigName,
    notebookInstanceSummary_notebookInstanceStatus,
    notebookInstanceSummary_url,
    notebookInstanceSummary_instanceType,
    notebookInstanceSummary_lastModifiedTime,
    notebookInstanceSummary_additionalCodeRepositories,
    notebookInstanceSummary_creationTime,
    notebookInstanceSummary_defaultCodeRepository,
    notebookInstanceSummary_notebookInstanceName,
    notebookInstanceSummary_notebookInstanceArn,

    -- * NotificationConfiguration
    NotificationConfiguration (..),
    newNotificationConfiguration,
    notificationConfiguration_notificationTopicArn,

    -- * ObjectiveStatusCounters
    ObjectiveStatusCounters (..),
    newObjectiveStatusCounters,
    objectiveStatusCounters_failed,
    objectiveStatusCounters_succeeded,
    objectiveStatusCounters_pending,

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
    oidcConfigForResponse_issuer,
    oidcConfigForResponse_authorizationEndpoint,
    oidcConfigForResponse_userInfoEndpoint,
    oidcConfigForResponse_clientId,
    oidcConfigForResponse_logoutEndpoint,
    oidcConfigForResponse_jwksUri,
    oidcConfigForResponse_tokenEndpoint,

    -- * OidcMemberDefinition
    OidcMemberDefinition (..),
    newOidcMemberDefinition,
    oidcMemberDefinition_groups,

    -- * OnlineStoreConfig
    OnlineStoreConfig (..),
    newOnlineStoreConfig,
    onlineStoreConfig_enableOnlineStore,
    onlineStoreConfig_securityConfig,

    -- * OnlineStoreSecurityConfig
    OnlineStoreSecurityConfig (..),
    newOnlineStoreSecurityConfig,
    onlineStoreSecurityConfig_kmsKeyId,

    -- * OutputConfig
    OutputConfig (..),
    newOutputConfig,
    outputConfig_targetDevice,
    outputConfig_targetPlatform,
    outputConfig_kmsKeyId,
    outputConfig_compilerOptions,
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

    -- * ParallelismConfiguration
    ParallelismConfiguration (..),
    newParallelismConfiguration,
    parallelismConfiguration_maxParallelExecutionSteps,

    -- * Parameter
    Parameter (..),
    newParameter,
    parameter_name,
    parameter_value,

    -- * ParameterRange
    ParameterRange (..),
    newParameterRange,
    parameterRange_continuousParameterRangeSpecification,
    parameterRange_integerParameterRangeSpecification,
    parameterRange_categoricalParameterRangeSpecification,

    -- * ParameterRanges
    ParameterRanges (..),
    newParameterRanges,
    parameterRanges_categoricalParameterRanges,
    parameterRanges_integerParameterRanges,
    parameterRanges_continuousParameterRanges,

    -- * Parent
    Parent (..),
    newParent,
    parent_trialName,
    parent_experimentName,

    -- * ParentHyperParameterTuningJob
    ParentHyperParameterTuningJob (..),
    newParentHyperParameterTuningJob,
    parentHyperParameterTuningJob_hyperParameterTuningJobName,

    -- * PendingDeploymentSummary
    PendingDeploymentSummary (..),
    newPendingDeploymentSummary,
    pendingDeploymentSummary_productionVariants,
    pendingDeploymentSummary_startTime,
    pendingDeploymentSummary_endpointConfigName,

    -- * PendingProductionVariantSummary
    PendingProductionVariantSummary (..),
    newPendingProductionVariantSummary,
    pendingProductionVariantSummary_desiredServerlessConfig,
    pendingProductionVariantSummary_desiredWeight,
    pendingProductionVariantSummary_acceleratorType,
    pendingProductionVariantSummary_currentServerlessConfig,
    pendingProductionVariantSummary_variantStatus,
    pendingProductionVariantSummary_desiredInstanceCount,
    pendingProductionVariantSummary_instanceType,
    pendingProductionVariantSummary_currentWeight,
    pendingProductionVariantSummary_deployedImages,
    pendingProductionVariantSummary_currentInstanceCount,
    pendingProductionVariantSummary_variantName,

    -- * Phase
    Phase (..),
    newPhase,
    phase_spawnRate,
    phase_initialNumberOfUsers,
    phase_durationInSeconds,

    -- * Pipeline
    Pipeline (..),
    newPipeline,
    pipeline_tags,
    pipeline_roleArn,
    pipeline_pipelineArn,
    pipeline_pipelineDisplayName,
    pipeline_pipelineDescription,
    pipeline_lastModifiedTime,
    pipeline_parallelismConfiguration,
    pipeline_pipelineName,
    pipeline_lastRunTime,
    pipeline_creationTime,
    pipeline_lastModifiedBy,
    pipeline_createdBy,
    pipeline_pipelineStatus,

    -- * PipelineDefinitionS3Location
    PipelineDefinitionS3Location (..),
    newPipelineDefinitionS3Location,
    pipelineDefinitionS3Location_versionId,
    pipelineDefinitionS3Location_bucket,
    pipelineDefinitionS3Location_objectKey,

    -- * PipelineExecution
    PipelineExecution (..),
    newPipelineExecution,
    pipelineExecution_pipelineArn,
    pipelineExecution_pipelineExperimentConfig,
    pipelineExecution_pipelineParameters,
    pipelineExecution_lastModifiedTime,
    pipelineExecution_parallelismConfiguration,
    pipelineExecution_pipelineExecutionDescription,
    pipelineExecution_creationTime,
    pipelineExecution_lastModifiedBy,
    pipelineExecution_createdBy,
    pipelineExecution_pipelineExecutionStatus,
    pipelineExecution_pipelineExecutionDisplayName,
    pipelineExecution_pipelineExecutionArn,
    pipelineExecution_failureReason,

    -- * PipelineExecutionStep
    PipelineExecutionStep (..),
    newPipelineExecutionStep,
    pipelineExecutionStep_attemptCount,
    pipelineExecutionStep_metadata,
    pipelineExecutionStep_endTime,
    pipelineExecutionStep_stepName,
    pipelineExecutionStep_stepDisplayName,
    pipelineExecutionStep_cacheHitResult,
    pipelineExecutionStep_stepStatus,
    pipelineExecutionStep_stepDescription,
    pipelineExecutionStep_startTime,
    pipelineExecutionStep_failureReason,

    -- * PipelineExecutionStepMetadata
    PipelineExecutionStepMetadata (..),
    newPipelineExecutionStepMetadata,
    pipelineExecutionStepMetadata_trainingJob,
    pipelineExecutionStepMetadata_model,
    pipelineExecutionStepMetadata_processingJob,
    pipelineExecutionStepMetadata_fail,
    pipelineExecutionStepMetadata_clarifyCheck,
    pipelineExecutionStepMetadata_registerModel,
    pipelineExecutionStepMetadata_emr,
    pipelineExecutionStepMetadata_condition,
    pipelineExecutionStepMetadata_lambda,
    pipelineExecutionStepMetadata_qualityCheck,
    pipelineExecutionStepMetadata_transformJob,
    pipelineExecutionStepMetadata_callback,
    pipelineExecutionStepMetadata_tuningJob,

    -- * PipelineExecutionSummary
    PipelineExecutionSummary (..),
    newPipelineExecutionSummary,
    pipelineExecutionSummary_pipelineExecutionDescription,
    pipelineExecutionSummary_pipelineExecutionFailureReason,
    pipelineExecutionSummary_pipelineExecutionStatus,
    pipelineExecutionSummary_pipelineExecutionDisplayName,
    pipelineExecutionSummary_pipelineExecutionArn,
    pipelineExecutionSummary_startTime,

    -- * PipelineExperimentConfig
    PipelineExperimentConfig (..),
    newPipelineExperimentConfig,
    pipelineExperimentConfig_trialName,
    pipelineExperimentConfig_experimentName,

    -- * PipelineSummary
    PipelineSummary (..),
    newPipelineSummary,
    pipelineSummary_roleArn,
    pipelineSummary_lastExecutionTime,
    pipelineSummary_pipelineArn,
    pipelineSummary_pipelineDisplayName,
    pipelineSummary_pipelineDescription,
    pipelineSummary_lastModifiedTime,
    pipelineSummary_pipelineName,
    pipelineSummary_creationTime,

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
    processingInput_s3Input,
    processingInput_datasetDefinition,
    processingInput_appManaged,
    processingInput_inputName,

    -- * ProcessingJob
    ProcessingJob (..),
    newProcessingJob,
    processingJob_tags,
    processingJob_processingJobName,
    processingJob_roleArn,
    processingJob_environment,
    processingJob_processingJobStatus,
    processingJob_monitoringScheduleArn,
    processingJob_networkConfig,
    processingJob_experimentConfig,
    processingJob_appSpecification,
    processingJob_autoMLJobArn,
    processingJob_processingInputs,
    processingJob_lastModifiedTime,
    processingJob_stoppingCondition,
    processingJob_processingJobArn,
    processingJob_processingStartTime,
    processingJob_creationTime,
    processingJob_processingEndTime,
    processingJob_processingResources,
    processingJob_trainingJobArn,
    processingJob_exitMessage,
    processingJob_failureReason,
    processingJob_processingOutputConfig,

    -- * ProcessingJobStepMetadata
    ProcessingJobStepMetadata (..),
    newProcessingJobStepMetadata,
    processingJobStepMetadata_arn,

    -- * ProcessingJobSummary
    ProcessingJobSummary (..),
    newProcessingJobSummary,
    processingJobSummary_lastModifiedTime,
    processingJobSummary_processingEndTime,
    processingJobSummary_exitMessage,
    processingJobSummary_failureReason,
    processingJobSummary_processingJobName,
    processingJobSummary_processingJobArn,
    processingJobSummary_creationTime,
    processingJobSummary_processingJobStatus,

    -- * ProcessingOutput
    ProcessingOutput (..),
    newProcessingOutput,
    processingOutput_s3Output,
    processingOutput_featureStoreOutput,
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
    processingS3Input_s3InputMode,
    processingS3Input_s3DataDistributionType,
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
    productionVariant_containerStartupHealthCheckTimeoutInSeconds,
    productionVariant_initialInstanceCount,
    productionVariant_instanceType,
    productionVariant_serverlessConfig,
    productionVariant_coreDumpConfig,
    productionVariant_initialVariantWeight,
    productionVariant_modelDataDownloadTimeoutInSeconds,
    productionVariant_volumeSizeInGB,
    productionVariant_variantName,
    productionVariant_modelName,

    -- * ProductionVariantCoreDumpConfig
    ProductionVariantCoreDumpConfig (..),
    newProductionVariantCoreDumpConfig,
    productionVariantCoreDumpConfig_kmsKeyId,
    productionVariantCoreDumpConfig_destinationS3Uri,

    -- * ProductionVariantServerlessConfig
    ProductionVariantServerlessConfig (..),
    newProductionVariantServerlessConfig,
    productionVariantServerlessConfig_memorySizeInMB,
    productionVariantServerlessConfig_maxConcurrency,

    -- * ProductionVariantStatus
    ProductionVariantStatus (..),
    newProductionVariantStatus,
    productionVariantStatus_statusMessage,
    productionVariantStatus_startTime,
    productionVariantStatus_status,

    -- * ProductionVariantSummary
    ProductionVariantSummary (..),
    newProductionVariantSummary,
    productionVariantSummary_desiredServerlessConfig,
    productionVariantSummary_desiredWeight,
    productionVariantSummary_currentServerlessConfig,
    productionVariantSummary_variantStatus,
    productionVariantSummary_desiredInstanceCount,
    productionVariantSummary_currentWeight,
    productionVariantSummary_deployedImages,
    productionVariantSummary_currentInstanceCount,
    productionVariantSummary_variantName,

    -- * ProfilerConfig
    ProfilerConfig (..),
    newProfilerConfig,
    profilerConfig_profilingIntervalInMilliseconds,
    profilerConfig_s3OutputPath,
    profilerConfig_profilingParameters,
    profilerConfig_disableProfiler,

    -- * ProfilerConfigForUpdate
    ProfilerConfigForUpdate (..),
    newProfilerConfigForUpdate,
    profilerConfigForUpdate_profilingIntervalInMilliseconds,
    profilerConfigForUpdate_s3OutputPath,
    profilerConfigForUpdate_profilingParameters,
    profilerConfigForUpdate_disableProfiler,

    -- * ProfilerRuleConfiguration
    ProfilerRuleConfiguration (..),
    newProfilerRuleConfiguration,
    profilerRuleConfiguration_s3OutputPath,
    profilerRuleConfiguration_instanceType,
    profilerRuleConfiguration_ruleParameters,
    profilerRuleConfiguration_localPath,
    profilerRuleConfiguration_volumeSizeInGB,
    profilerRuleConfiguration_ruleConfigurationName,
    profilerRuleConfiguration_ruleEvaluatorImage,

    -- * ProfilerRuleEvaluationStatus
    ProfilerRuleEvaluationStatus (..),
    newProfilerRuleEvaluationStatus,
    profilerRuleEvaluationStatus_statusDetails,
    profilerRuleEvaluationStatus_lastModifiedTime,
    profilerRuleEvaluationStatus_ruleEvaluationJobArn,
    profilerRuleEvaluationStatus_ruleConfigurationName,
    profilerRuleEvaluationStatus_ruleEvaluationStatus,

    -- * Project
    Project (..),
    newProject,
    project_tags,
    project_serviceCatalogProvisionedProductDetails,
    project_projectId,
    project_lastModifiedTime,
    project_projectDescription,
    project_projectStatus,
    project_creationTime,
    project_lastModifiedBy,
    project_projectName,
    project_createdBy,
    project_projectArn,
    project_serviceCatalogProvisioningDetails,

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
    provisioningParameter_key,
    provisioningParameter_value,

    -- * PublicWorkforceTaskPrice
    PublicWorkforceTaskPrice (..),
    newPublicWorkforceTaskPrice,
    publicWorkforceTaskPrice_amountInUsd,

    -- * QualityCheckStepMetadata
    QualityCheckStepMetadata (..),
    newQualityCheckStepMetadata,
    qualityCheckStepMetadata_modelPackageGroupName,
    qualityCheckStepMetadata_baselineUsedForDriftCheckStatistics,
    qualityCheckStepMetadata_checkJobArn,
    qualityCheckStepMetadata_checkType,
    qualityCheckStepMetadata_registerNewBaseline,
    qualityCheckStepMetadata_skipCheck,
    qualityCheckStepMetadata_baselineUsedForDriftCheckConstraints,
    qualityCheckStepMetadata_calculatedBaselineStatistics,
    qualityCheckStepMetadata_calculatedBaselineConstraints,
    qualityCheckStepMetadata_violationReport,

    -- * QueryFilters
    QueryFilters (..),
    newQueryFilters,
    queryFilters_modifiedAfter,
    queryFilters_properties,
    queryFilters_createdBefore,
    queryFilters_types,
    queryFilters_lineageTypes,
    queryFilters_createdAfter,
    queryFilters_modifiedBefore,

    -- * RSessionAppSettings
    RSessionAppSettings (..),
    newRSessionAppSettings,
    rSessionAppSettings_defaultResourceSpec,
    rSessionAppSettings_customImages,

    -- * RStudioServerProAppSettings
    RStudioServerProAppSettings (..),
    newRStudioServerProAppSettings,
    rStudioServerProAppSettings_accessStatus,
    rStudioServerProAppSettings_userGroup,

    -- * RStudioServerProDomainSettings
    RStudioServerProDomainSettings (..),
    newRStudioServerProDomainSettings,
    rStudioServerProDomainSettings_defaultResourceSpec,
    rStudioServerProDomainSettings_rStudioConnectUrl,
    rStudioServerProDomainSettings_rStudioPackageManagerUrl,
    rStudioServerProDomainSettings_domainExecutionRoleArn,

    -- * RStudioServerProDomainSettingsForUpdate
    RStudioServerProDomainSettingsForUpdate (..),
    newRStudioServerProDomainSettingsForUpdate,
    rStudioServerProDomainSettingsForUpdate_defaultResourceSpec,
    rStudioServerProDomainSettingsForUpdate_domainExecutionRoleArn,

    -- * RecommendationJobCompiledOutputConfig
    RecommendationJobCompiledOutputConfig (..),
    newRecommendationJobCompiledOutputConfig,
    recommendationJobCompiledOutputConfig_s3OutputUri,

    -- * RecommendationJobContainerConfig
    RecommendationJobContainerConfig (..),
    newRecommendationJobContainerConfig,
    recommendationJobContainerConfig_task,
    recommendationJobContainerConfig_supportedInstanceTypes,
    recommendationJobContainerConfig_domain,
    recommendationJobContainerConfig_nearestModelName,
    recommendationJobContainerConfig_frameworkVersion,
    recommendationJobContainerConfig_payloadConfig,
    recommendationJobContainerConfig_framework,

    -- * RecommendationJobInferenceBenchmark
    RecommendationJobInferenceBenchmark (..),
    newRecommendationJobInferenceBenchmark,
    recommendationJobInferenceBenchmark_metrics,
    recommendationJobInferenceBenchmark_endpointConfiguration,
    recommendationJobInferenceBenchmark_failureReason,
    recommendationJobInferenceBenchmark_modelConfiguration,

    -- * RecommendationJobInputConfig
    RecommendationJobInputConfig (..),
    newRecommendationJobInputConfig,
    recommendationJobInputConfig_trafficPattern,
    recommendationJobInputConfig_jobDurationInSeconds,
    recommendationJobInputConfig_volumeKmsKeyId,
    recommendationJobInputConfig_endpoints,
    recommendationJobInputConfig_endpointConfigurations,
    recommendationJobInputConfig_resourceLimit,
    recommendationJobInputConfig_containerConfig,
    recommendationJobInputConfig_modelPackageVersionArn,

    -- * RecommendationJobOutputConfig
    RecommendationJobOutputConfig (..),
    newRecommendationJobOutputConfig,
    recommendationJobOutputConfig_compiledOutputConfig,
    recommendationJobOutputConfig_kmsKeyId,

    -- * RecommendationJobPayloadConfig
    RecommendationJobPayloadConfig (..),
    newRecommendationJobPayloadConfig,
    recommendationJobPayloadConfig_samplePayloadUrl,
    recommendationJobPayloadConfig_supportedContentTypes,

    -- * RecommendationJobResourceLimit
    RecommendationJobResourceLimit (..),
    newRecommendationJobResourceLimit,
    recommendationJobResourceLimit_maxNumberOfTests,
    recommendationJobResourceLimit_maxParallelOfTests,

    -- * RecommendationJobStoppingConditions
    RecommendationJobStoppingConditions (..),
    newRecommendationJobStoppingConditions,
    recommendationJobStoppingConditions_maxInvocations,
    recommendationJobStoppingConditions_modelLatencyThresholds,

    -- * RecommendationMetrics
    RecommendationMetrics (..),
    newRecommendationMetrics,
    recommendationMetrics_costPerHour,
    recommendationMetrics_costPerInference,
    recommendationMetrics_maxInvocations,
    recommendationMetrics_modelLatency,

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
    resolvedAttributes_completionCriteria,
    resolvedAttributes_autoMLJobObjective,
    resolvedAttributes_problemType,

    -- * ResourceConfig
    ResourceConfig (..),
    newResourceConfig,
    resourceConfig_keepAlivePeriodInSeconds,
    resourceConfig_volumeKmsKeyId,
    resourceConfig_instanceType,
    resourceConfig_instanceCount,
    resourceConfig_instanceGroups,
    resourceConfig_volumeSizeInGB,

    -- * ResourceConfigForUpdate
    ResourceConfigForUpdate (..),
    newResourceConfigForUpdate,
    resourceConfigForUpdate_keepAlivePeriodInSeconds,

    -- * ResourceLimits
    ResourceLimits (..),
    newResourceLimits,
    resourceLimits_maxNumberOfTrainingJobs,
    resourceLimits_maxParallelTrainingJobs,

    -- * ResourceSpec
    ResourceSpec (..),
    newResourceSpec,
    resourceSpec_lifecycleConfigArn,
    resourceSpec_sageMakerImageVersionArn,
    resourceSpec_instanceType,
    resourceSpec_sageMakerImageArn,

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
    s3DataSource_attributeNames,
    s3DataSource_s3DataDistributionType,
    s3DataSource_instanceGroupNames,
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
    searchExpression_filters,
    searchExpression_subExpressions,
    searchExpression_operator,
    searchExpression_nestedFilters,

    -- * SearchRecord
    SearchRecord (..),
    newSearchRecord,
    searchRecord_modelPackageGroup,
    searchRecord_trainingJob,
    searchRecord_project,
    searchRecord_trialComponent,
    searchRecord_experiment,
    searchRecord_hyperParameterTuningJob,
    searchRecord_endpoint,
    searchRecord_featureMetadata,
    searchRecord_trial,
    searchRecord_modelPackage,
    searchRecord_pipeline,
    searchRecord_featureGroup,
    searchRecord_pipelineExecution,

    -- * SecondaryStatusTransition
    SecondaryStatusTransition (..),
    newSecondaryStatusTransition,
    secondaryStatusTransition_endTime,
    secondaryStatusTransition_statusMessage,
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
    serviceCatalogProvisioningDetails_pathId,
    serviceCatalogProvisioningDetails_provisioningParameters,
    serviceCatalogProvisioningDetails_provisioningArtifactId,
    serviceCatalogProvisioningDetails_productId,

    -- * ServiceCatalogProvisioningUpdateDetails
    ServiceCatalogProvisioningUpdateDetails (..),
    newServiceCatalogProvisioningUpdateDetails,
    serviceCatalogProvisioningUpdateDetails_provisioningParameters,
    serviceCatalogProvisioningUpdateDetails_provisioningArtifactId,

    -- * SharingSettings
    SharingSettings (..),
    newSharingSettings,
    sharingSettings_s3OutputPath,
    sharingSettings_notebookOutputOption,
    sharingSettings_s3KmsKeyId,

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
    studioLifecycleConfigDetails_studioLifecycleConfigName,
    studioLifecycleConfigDetails_studioLifecycleConfigArn,
    studioLifecycleConfigDetails_studioLifecycleConfigAppType,
    studioLifecycleConfigDetails_lastModifiedTime,
    studioLifecycleConfigDetails_creationTime,

    -- * SubscribedWorkteam
    SubscribedWorkteam (..),
    newSubscribedWorkteam,
    subscribedWorkteam_listingId,
    subscribedWorkteam_marketplaceDescription,
    subscribedWorkteam_marketplaceTitle,
    subscribedWorkteam_sellerName,
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

    -- * TimeSeriesForecastingSettings
    TimeSeriesForecastingSettings (..),
    newTimeSeriesForecastingSettings,
    timeSeriesForecastingSettings_status,
    timeSeriesForecastingSettings_amazonForecastRoleArn,

    -- * TrafficPattern
    TrafficPattern (..),
    newTrafficPattern,
    trafficPattern_trafficType,
    trafficPattern_phases,

    -- * TrafficRoutingConfig
    TrafficRoutingConfig (..),
    newTrafficRoutingConfig,
    trafficRoutingConfig_linearStepSize,
    trafficRoutingConfig_canarySize,
    trafficRoutingConfig_type,
    trafficRoutingConfig_waitIntervalInSeconds,

    -- * TrainingJob
    TrainingJob (..),
    newTrainingJob,
    trainingJob_tags,
    trainingJob_outputDataConfig,
    trainingJob_enableManagedSpotTraining,
    trainingJob_roleArn,
    trainingJob_environment,
    trainingJob_trainingTimeInSeconds,
    trainingJob_debugRuleEvaluationStatuses,
    trainingJob_retryStrategy,
    trainingJob_vpcConfig,
    trainingJob_secondaryStatusTransitions,
    trainingJob_enableNetworkIsolation,
    trainingJob_resourceConfig,
    trainingJob_experimentConfig,
    trainingJob_tuningJobArn,
    trainingJob_trainingStartTime,
    trainingJob_checkpointConfig,
    trainingJob_autoMLJobArn,
    trainingJob_debugHookConfig,
    trainingJob_lastModifiedTime,
    trainingJob_secondaryStatus,
    trainingJob_enableInterContainerTrafficEncryption,
    trainingJob_finalMetricDataList,
    trainingJob_trainingJobStatus,
    trainingJob_modelArtifacts,
    trainingJob_stoppingCondition,
    trainingJob_algorithmSpecification,
    trainingJob_debugRuleConfigurations,
    trainingJob_labelingJobArn,
    trainingJob_creationTime,
    trainingJob_trainingJobName,
    trainingJob_tensorBoardOutputConfig,
    trainingJob_billableTimeInSeconds,
    trainingJob_trainingJobArn,
    trainingJob_inputDataConfig,
    trainingJob_hyperParameters,
    trainingJob_failureReason,
    trainingJob_trainingEndTime,

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
    trainingJobStatusCounters_retryableError,
    trainingJobStatusCounters_completed,
    trainingJobStatusCounters_stopped,
    trainingJobStatusCounters_nonRetryableError,
    trainingJobStatusCounters_inProgress,

    -- * TrainingJobStepMetadata
    TrainingJobStepMetadata (..),
    newTrainingJobStepMetadata,
    trainingJobStepMetadata_arn,

    -- * TrainingJobSummary
    TrainingJobSummary (..),
    newTrainingJobSummary,
    trainingJobSummary_warmPoolStatus,
    trainingJobSummary_lastModifiedTime,
    trainingJobSummary_trainingEndTime,
    trainingJobSummary_trainingJobName,
    trainingJobSummary_trainingJobArn,
    trainingJobSummary_creationTime,
    trainingJobSummary_trainingJobStatus,

    -- * TrainingSpecification
    TrainingSpecification (..),
    newTrainingSpecification,
    trainingSpecification_supportsDistributedTraining,
    trainingSpecification_supportedTuningJobObjectiveMetrics,
    trainingSpecification_supportedHyperParameters,
    trainingSpecification_metricDefinitions,
    trainingSpecification_trainingImageDigest,
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
    transformJob_tags,
    transformJob_maxConcurrentTransforms,
    transformJob_transformJobArn,
    transformJob_modelClientConfig,
    transformJob_environment,
    transformJob_transformOutput,
    transformJob_experimentConfig,
    transformJob_transformInput,
    transformJob_transformJobName,
    transformJob_autoMLJobArn,
    transformJob_transformJobStatus,
    transformJob_transformEndTime,
    transformJob_maxPayloadInMB,
    transformJob_batchStrategy,
    transformJob_labelingJobArn,
    transformJob_modelName,
    transformJob_transformResources,
    transformJob_creationTime,
    transformJob_dataProcessing,
    transformJob_transformStartTime,
    transformJob_failureReason,

    -- * TransformJobDefinition
    TransformJobDefinition (..),
    newTransformJobDefinition,
    transformJobDefinition_maxConcurrentTransforms,
    transformJobDefinition_environment,
    transformJobDefinition_maxPayloadInMB,
    transformJobDefinition_batchStrategy,
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
    transformJobSummary_transformEndTime,
    transformJobSummary_lastModifiedTime,
    transformJobSummary_failureReason,
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
    trial_tags,
    trial_metadataProperties,
    trial_displayName,
    trial_trialComponentSummaries,
    trial_lastModifiedTime,
    trial_source,
    trial_trialName,
    trial_creationTime,
    trial_lastModifiedBy,
    trial_createdBy,
    trial_trialArn,
    trial_experimentName,

    -- * TrialComponent
    TrialComponent (..),
    newTrialComponent,
    trialComponent_tags,
    trialComponent_trialComponentArn,
    trialComponent_trialComponentName,
    trialComponent_sourceDetail,
    trialComponent_metadataProperties,
    trialComponent_displayName,
    trialComponent_status,
    trialComponent_metrics,
    trialComponent_outputArtifacts,
    trialComponent_endTime,
    trialComponent_lastModifiedTime,
    trialComponent_source,
    trialComponent_parents,
    trialComponent_lineageGroupArn,
    trialComponent_creationTime,
    trialComponent_inputArtifacts,
    trialComponent_lastModifiedBy,
    trialComponent_createdBy,
    trialComponent_startTime,
    trialComponent_parameters,

    -- * TrialComponentArtifact
    TrialComponentArtifact (..),
    newTrialComponentArtifact,
    trialComponentArtifact_mediaType,
    trialComponentArtifact_value,

    -- * TrialComponentMetricSummary
    TrialComponentMetricSummary (..),
    newTrialComponentMetricSummary,
    trialComponentMetricSummary_sourceArn,
    trialComponentMetricSummary_max,
    trialComponentMetricSummary_timeStamp,
    trialComponentMetricSummary_avg,
    trialComponentMetricSummary_count,
    trialComponentMetricSummary_last,
    trialComponentMetricSummary_min,
    trialComponentMetricSummary_metricName,
    trialComponentMetricSummary_stdDev,

    -- * TrialComponentParameterValue
    TrialComponentParameterValue (..),
    newTrialComponentParameterValue,
    trialComponentParameterValue_numberValue,
    trialComponentParameterValue_stringValue,

    -- * TrialComponentSimpleSummary
    TrialComponentSimpleSummary (..),
    newTrialComponentSimpleSummary,
    trialComponentSimpleSummary_trialComponentArn,
    trialComponentSimpleSummary_trialComponentName,
    trialComponentSimpleSummary_trialComponentSource,
    trialComponentSimpleSummary_creationTime,
    trialComponentSimpleSummary_createdBy,

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
    trialComponentStatus_message,
    trialComponentStatus_primaryStatus,

    -- * TrialComponentSummary
    TrialComponentSummary (..),
    newTrialComponentSummary,
    trialComponentSummary_trialComponentArn,
    trialComponentSummary_trialComponentName,
    trialComponentSummary_displayName,
    trialComponentSummary_status,
    trialComponentSummary_endTime,
    trialComponentSummary_lastModifiedTime,
    trialComponentSummary_trialComponentSource,
    trialComponentSummary_creationTime,
    trialComponentSummary_lastModifiedBy,
    trialComponentSummary_createdBy,
    trialComponentSummary_startTime,

    -- * TrialSource
    TrialSource (..),
    newTrialSource,
    trialSource_sourceType,
    trialSource_sourceArn,

    -- * TrialSummary
    TrialSummary (..),
    newTrialSummary,
    trialSummary_displayName,
    trialSummary_lastModifiedTime,
    trialSummary_trialName,
    trialSummary_creationTime,
    trialSummary_trialSource,
    trialSummary_trialArn,

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
    usd_tenthFractionsOfACent,
    usd_dollars,
    usd_cents,

    -- * UiConfig
    UiConfig (..),
    newUiConfig,
    uiConfig_humanTaskUiArn,
    uiConfig_uiTemplateS3Uri,

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
    userContext_userProfileArn,
    userContext_userProfileName,
    userContext_domainId,

    -- * UserProfileDetails
    UserProfileDetails (..),
    newUserProfileDetails,
    userProfileDetails_status,
    userProfileDetails_lastModifiedTime,
    userProfileDetails_userProfileName,
    userProfileDetails_creationTime,
    userProfileDetails_domainId,

    -- * UserSettings
    UserSettings (..),
    newUserSettings,
    userSettings_executionRole,
    userSettings_rSessionAppSettings,
    userSettings_tensorBoardAppSettings,
    userSettings_kernelGatewayAppSettings,
    userSettings_securityGroups,
    userSettings_canvasAppSettings,
    userSettings_jupyterServerAppSettings,
    userSettings_rStudioServerProAppSettings,
    userSettings_sharingSettings,

    -- * VariantProperty
    VariantProperty (..),
    newVariantProperty,
    variantProperty_variantPropertyType,

    -- * Vertex
    Vertex (..),
    newVertex,
    vertex_type,
    vertex_arn,
    vertex_lineageType,

    -- * VpcConfig
    VpcConfig (..),
    newVpcConfig,
    vpcConfig_securityGroupIds,
    vpcConfig_subnets,

    -- * WarmPoolStatus
    WarmPoolStatus (..),
    newWarmPoolStatus,
    warmPoolStatus_reusedByJob,
    warmPoolStatus_resourceRetainedBillableTimeInSeconds,
    warmPoolStatus_status,

    -- * Workforce
    Workforce (..),
    newWorkforce,
    workforce_cognitoConfig,
    workforce_lastUpdatedDate,
    workforce_subDomain,
    workforce_status,
    workforce_sourceIpConfig,
    workforce_workforceVpcConfig,
    workforce_createDate,
    workforce_oidcConfig,
    workforce_failureReason,
    workforce_workforceName,
    workforce_workforceArn,

    -- * WorkforceVpcConfigRequest
    WorkforceVpcConfigRequest (..),
    newWorkforceVpcConfigRequest,
    workforceVpcConfigRequest_securityGroupIds,
    workforceVpcConfigRequest_subnets,
    workforceVpcConfigRequest_vpcId,

    -- * WorkforceVpcConfigResponse
    WorkforceVpcConfigResponse (..),
    newWorkforceVpcConfigResponse,
    workforceVpcConfigResponse_vpcEndpointId,
    workforceVpcConfigResponse_vpcId,
    workforceVpcConfigResponse_securityGroupIds,
    workforceVpcConfigResponse_subnets,

    -- * Workteam
    Workteam (..),
    newWorkteam,
    workteam_lastUpdatedDate,
    workteam_subDomain,
    workteam_notificationConfiguration,
    workteam_workforceArn,
    workteam_createDate,
    workteam_productListingIds,
    workteam_workteamName,
    workteam_memberDefinitions,
    workteam_workteamArn,
    workteam_description,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ActionSource
import Amazonka.SageMaker.Types.ActionStatus
import Amazonka.SageMaker.Types.ActionSummary
import Amazonka.SageMaker.Types.AdditionalInferenceSpecificationDefinition
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
import Amazonka.SageMaker.Types.AppSecurityGroupManagement
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
import Amazonka.SageMaker.Types.AutoMLCandidateGenerationConfig
import Amazonka.SageMaker.Types.AutoMLCandidateStep
import Amazonka.SageMaker.Types.AutoMLChannel
import Amazonka.SageMaker.Types.AutoMLChannelType
import Amazonka.SageMaker.Types.AutoMLContainerDefinition
import Amazonka.SageMaker.Types.AutoMLDataSource
import Amazonka.SageMaker.Types.AutoMLDataSplitConfig
import Amazonka.SageMaker.Types.AutoMLJobArtifacts
import Amazonka.SageMaker.Types.AutoMLJobCompletionCriteria
import Amazonka.SageMaker.Types.AutoMLJobConfig
import Amazonka.SageMaker.Types.AutoMLJobObjective
import Amazonka.SageMaker.Types.AutoMLJobObjectiveType
import Amazonka.SageMaker.Types.AutoMLJobSecondaryStatus
import Amazonka.SageMaker.Types.AutoMLJobStatus
import Amazonka.SageMaker.Types.AutoMLJobSummary
import Amazonka.SageMaker.Types.AutoMLMetricEnum
import Amazonka.SageMaker.Types.AutoMLMetricExtendedEnum
import Amazonka.SageMaker.Types.AutoMLMode
import Amazonka.SageMaker.Types.AutoMLOutputDataConfig
import Amazonka.SageMaker.Types.AutoMLPartialFailureReason
import Amazonka.SageMaker.Types.AutoMLS3DataSource
import Amazonka.SageMaker.Types.AutoMLS3DataType
import Amazonka.SageMaker.Types.AutoMLSecurityConfig
import Amazonka.SageMaker.Types.AutoMLSortBy
import Amazonka.SageMaker.Types.AutoMLSortOrder
import Amazonka.SageMaker.Types.AutoRollbackConfig
import Amazonka.SageMaker.Types.AwsManagedHumanLoopRequestSource
import Amazonka.SageMaker.Types.BatchDataCaptureConfig
import Amazonka.SageMaker.Types.BatchDescribeModelPackageError
import Amazonka.SageMaker.Types.BatchDescribeModelPackageSummary
import Amazonka.SageMaker.Types.BatchStrategy
import Amazonka.SageMaker.Types.BatchTransformInput
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
import Amazonka.SageMaker.Types.CanvasAppSettings
import Amazonka.SageMaker.Types.CapacitySize
import Amazonka.SageMaker.Types.CapacitySizeType
import Amazonka.SageMaker.Types.CaptureContentTypeHeader
import Amazonka.SageMaker.Types.CaptureMode
import Amazonka.SageMaker.Types.CaptureOption
import Amazonka.SageMaker.Types.CaptureStatus
import Amazonka.SageMaker.Types.CategoricalParameter
import Amazonka.SageMaker.Types.CategoricalParameterRange
import Amazonka.SageMaker.Types.CategoricalParameterRangeSpecification
import Amazonka.SageMaker.Types.Channel
import Amazonka.SageMaker.Types.ChannelSpecification
import Amazonka.SageMaker.Types.CheckpointConfig
import Amazonka.SageMaker.Types.ClarifyCheckStepMetadata
import Amazonka.SageMaker.Types.ClarifyExplainerConfig
import Amazonka.SageMaker.Types.ClarifyFeatureType
import Amazonka.SageMaker.Types.ClarifyInferenceConfig
import Amazonka.SageMaker.Types.ClarifyShapBaselineConfig
import Amazonka.SageMaker.Types.ClarifyShapConfig
import Amazonka.SageMaker.Types.ClarifyTextConfig
import Amazonka.SageMaker.Types.ClarifyTextGranularity
import Amazonka.SageMaker.Types.ClarifyTextLanguage
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
import Amazonka.SageMaker.Types.DeploymentStage
import Amazonka.SageMaker.Types.DeploymentStageStatusSummary
import Amazonka.SageMaker.Types.DesiredWeightAndCapacity
import Amazonka.SageMaker.Types.DetailedAlgorithmStatus
import Amazonka.SageMaker.Types.DetailedModelPackageStatus
import Amazonka.SageMaker.Types.Device
import Amazonka.SageMaker.Types.DeviceDeploymentStatus
import Amazonka.SageMaker.Types.DeviceDeploymentSummary
import Amazonka.SageMaker.Types.DeviceFleetSummary
import Amazonka.SageMaker.Types.DeviceSelectionConfig
import Amazonka.SageMaker.Types.DeviceStats
import Amazonka.SageMaker.Types.DeviceSubsetType
import Amazonka.SageMaker.Types.DeviceSummary
import Amazonka.SageMaker.Types.DirectInternetAccess
import Amazonka.SageMaker.Types.Direction
import Amazonka.SageMaker.Types.DomainDetails
import Amazonka.SageMaker.Types.DomainSettings
import Amazonka.SageMaker.Types.DomainSettingsForUpdate
import Amazonka.SageMaker.Types.DomainStatus
import Amazonka.SageMaker.Types.DriftCheckBaselines
import Amazonka.SageMaker.Types.DriftCheckBias
import Amazonka.SageMaker.Types.DriftCheckExplainability
import Amazonka.SageMaker.Types.DriftCheckModelDataQuality
import Amazonka.SageMaker.Types.DriftCheckModelQuality
import Amazonka.SageMaker.Types.EMRStepMetadata
import Amazonka.SageMaker.Types.Edge
import Amazonka.SageMaker.Types.EdgeDeploymentConfig
import Amazonka.SageMaker.Types.EdgeDeploymentModelConfig
import Amazonka.SageMaker.Types.EdgeDeploymentPlanSummary
import Amazonka.SageMaker.Types.EdgeDeploymentStatus
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
import Amazonka.SageMaker.Types.EndpointInfo
import Amazonka.SageMaker.Types.EndpointInput
import Amazonka.SageMaker.Types.EndpointInputConfiguration
import Amazonka.SageMaker.Types.EndpointOutputConfiguration
import Amazonka.SageMaker.Types.EndpointPerformance
import Amazonka.SageMaker.Types.EndpointSortKey
import Amazonka.SageMaker.Types.EndpointStatus
import Amazonka.SageMaker.Types.EndpointSummary
import Amazonka.SageMaker.Types.EnvironmentParameter
import Amazonka.SageMaker.Types.EnvironmentParameterRanges
import Amazonka.SageMaker.Types.ExecutionRoleIdentityConfig
import Amazonka.SageMaker.Types.ExecutionStatus
import Amazonka.SageMaker.Types.Experiment
import Amazonka.SageMaker.Types.ExperimentConfig
import Amazonka.SageMaker.Types.ExperimentSource
import Amazonka.SageMaker.Types.ExperimentSummary
import Amazonka.SageMaker.Types.Explainability
import Amazonka.SageMaker.Types.ExplainerConfig
import Amazonka.SageMaker.Types.FailStepMetadata
import Amazonka.SageMaker.Types.FailureHandlingPolicy
import Amazonka.SageMaker.Types.FeatureDefinition
import Amazonka.SageMaker.Types.FeatureGroup
import Amazonka.SageMaker.Types.FeatureGroupSortBy
import Amazonka.SageMaker.Types.FeatureGroupSortOrder
import Amazonka.SageMaker.Types.FeatureGroupStatus
import Amazonka.SageMaker.Types.FeatureGroupSummary
import Amazonka.SageMaker.Types.FeatureMetadata
import Amazonka.SageMaker.Types.FeatureParameter
import Amazonka.SageMaker.Types.FeatureStatus
import Amazonka.SageMaker.Types.FeatureType
import Amazonka.SageMaker.Types.FileSource
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
import Amazonka.SageMaker.Types.HyperParameterTuningAllocationStrategy
import Amazonka.SageMaker.Types.HyperParameterTuningInstanceConfig
import Amazonka.SageMaker.Types.HyperParameterTuningJobConfig
import Amazonka.SageMaker.Types.HyperParameterTuningJobObjective
import Amazonka.SageMaker.Types.HyperParameterTuningJobObjectiveType
import Amazonka.SageMaker.Types.HyperParameterTuningJobSearchEntity
import Amazonka.SageMaker.Types.HyperParameterTuningJobSortByOptions
import Amazonka.SageMaker.Types.HyperParameterTuningJobStatus
import Amazonka.SageMaker.Types.HyperParameterTuningJobStrategyConfig
import Amazonka.SageMaker.Types.HyperParameterTuningJobStrategyType
import Amazonka.SageMaker.Types.HyperParameterTuningJobSummary
import Amazonka.SageMaker.Types.HyperParameterTuningJobWarmStartConfig
import Amazonka.SageMaker.Types.HyperParameterTuningJobWarmStartType
import Amazonka.SageMaker.Types.HyperParameterTuningResourceConfig
import Amazonka.SageMaker.Types.HyperbandStrategyConfig
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
import Amazonka.SageMaker.Types.InferenceMetrics
import Amazonka.SageMaker.Types.InferenceRecommendation
import Amazonka.SageMaker.Types.InferenceRecommendationsJob
import Amazonka.SageMaker.Types.InferenceRecommendationsJobStep
import Amazonka.SageMaker.Types.InferenceSpecification
import Amazonka.SageMaker.Types.InputConfig
import Amazonka.SageMaker.Types.InputMode
import Amazonka.SageMaker.Types.InstanceGroup
import Amazonka.SageMaker.Types.InstanceMetadataServiceConfiguration
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
import Amazonka.SageMaker.Types.LastUpdateStatus
import Amazonka.SageMaker.Types.LastUpdateStatusValue
import Amazonka.SageMaker.Types.LineageGroupSummary
import Amazonka.SageMaker.Types.LineageType
import Amazonka.SageMaker.Types.ListCompilationJobsSortBy
import Amazonka.SageMaker.Types.ListDeviceFleetsSortBy
import Amazonka.SageMaker.Types.ListEdgeDeploymentPlansSortBy
import Amazonka.SageMaker.Types.ListEdgePackagingJobsSortBy
import Amazonka.SageMaker.Types.ListInferenceRecommendationsJobsSortBy
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
import Amazonka.SageMaker.Types.ModelConfiguration
import Amazonka.SageMaker.Types.ModelDataQuality
import Amazonka.SageMaker.Types.ModelDeployConfig
import Amazonka.SageMaker.Types.ModelDeployResult
import Amazonka.SageMaker.Types.ModelDigests
import Amazonka.SageMaker.Types.ModelExplainabilityAppSpecification
import Amazonka.SageMaker.Types.ModelExplainabilityBaselineConfig
import Amazonka.SageMaker.Types.ModelExplainabilityJobInput
import Amazonka.SageMaker.Types.ModelInput
import Amazonka.SageMaker.Types.ModelLatencyThreshold
import Amazonka.SageMaker.Types.ModelMetadataFilter
import Amazonka.SageMaker.Types.ModelMetadataFilterType
import Amazonka.SageMaker.Types.ModelMetadataSearchExpression
import Amazonka.SageMaker.Types.ModelMetadataSummary
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
import Amazonka.SageMaker.Types.MonitoringCsvDatasetFormat
import Amazonka.SageMaker.Types.MonitoringDatasetFormat
import Amazonka.SageMaker.Types.MonitoringExecutionSortKey
import Amazonka.SageMaker.Types.MonitoringExecutionSummary
import Amazonka.SageMaker.Types.MonitoringGroundTruthS3Input
import Amazonka.SageMaker.Types.MonitoringInput
import Amazonka.SageMaker.Types.MonitoringJobDefinition
import Amazonka.SageMaker.Types.MonitoringJobDefinitionSortKey
import Amazonka.SageMaker.Types.MonitoringJobDefinitionSummary
import Amazonka.SageMaker.Types.MonitoringJsonDatasetFormat
import Amazonka.SageMaker.Types.MonitoringNetworkConfig
import Amazonka.SageMaker.Types.MonitoringOutput
import Amazonka.SageMaker.Types.MonitoringOutputConfig
import Amazonka.SageMaker.Types.MonitoringParquetDatasetFormat
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
import Amazonka.SageMaker.Types.ParallelismConfiguration
import Amazonka.SageMaker.Types.Parameter
import Amazonka.SageMaker.Types.ParameterRange
import Amazonka.SageMaker.Types.ParameterRanges
import Amazonka.SageMaker.Types.ParameterType
import Amazonka.SageMaker.Types.Parent
import Amazonka.SageMaker.Types.ParentHyperParameterTuningJob
import Amazonka.SageMaker.Types.PendingDeploymentSummary
import Amazonka.SageMaker.Types.PendingProductionVariantSummary
import Amazonka.SageMaker.Types.Phase
import Amazonka.SageMaker.Types.Pipeline
import Amazonka.SageMaker.Types.PipelineDefinitionS3Location
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
import Amazonka.SageMaker.Types.ProductionVariantServerlessConfig
import Amazonka.SageMaker.Types.ProductionVariantStatus
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
import Amazonka.SageMaker.Types.QualityCheckStepMetadata
import Amazonka.SageMaker.Types.QueryFilters
import Amazonka.SageMaker.Types.RSessionAppSettings
import Amazonka.SageMaker.Types.RStudioServerProAccessStatus
import Amazonka.SageMaker.Types.RStudioServerProAppSettings
import Amazonka.SageMaker.Types.RStudioServerProDomainSettings
import Amazonka.SageMaker.Types.RStudioServerProDomainSettingsForUpdate
import Amazonka.SageMaker.Types.RStudioServerProUserGroup
import Amazonka.SageMaker.Types.RecommendationJobCompiledOutputConfig
import Amazonka.SageMaker.Types.RecommendationJobContainerConfig
import Amazonka.SageMaker.Types.RecommendationJobInferenceBenchmark
import Amazonka.SageMaker.Types.RecommendationJobInputConfig
import Amazonka.SageMaker.Types.RecommendationJobOutputConfig
import Amazonka.SageMaker.Types.RecommendationJobPayloadConfig
import Amazonka.SageMaker.Types.RecommendationJobResourceLimit
import Amazonka.SageMaker.Types.RecommendationJobStatus
import Amazonka.SageMaker.Types.RecommendationJobStoppingConditions
import Amazonka.SageMaker.Types.RecommendationJobType
import Amazonka.SageMaker.Types.RecommendationMetrics
import Amazonka.SageMaker.Types.RecommendationStepType
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
import Amazonka.SageMaker.Types.ResourceConfigForUpdate
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
import Amazonka.SageMaker.Types.ServiceCatalogProvisioningUpdateDetails
import Amazonka.SageMaker.Types.SharingSettings
import Amazonka.SageMaker.Types.ShuffleConfig
import Amazonka.SageMaker.Types.SortActionsBy
import Amazonka.SageMaker.Types.SortArtifactsBy
import Amazonka.SageMaker.Types.SortAssociationsBy
import Amazonka.SageMaker.Types.SortBy
import Amazonka.SageMaker.Types.SortContextsBy
import Amazonka.SageMaker.Types.SortExperimentsBy
import Amazonka.SageMaker.Types.SortLineageGroupsBy
import Amazonka.SageMaker.Types.SortOrder
import Amazonka.SageMaker.Types.SortPipelineExecutionsBy
import Amazonka.SageMaker.Types.SortPipelinesBy
import Amazonka.SageMaker.Types.SortTrialComponentsBy
import Amazonka.SageMaker.Types.SortTrialsBy
import Amazonka.SageMaker.Types.SourceAlgorithm
import Amazonka.SageMaker.Types.SourceAlgorithmSpecification
import Amazonka.SageMaker.Types.SourceIpConfig
import Amazonka.SageMaker.Types.SplitType
import Amazonka.SageMaker.Types.StageStatus
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
import Amazonka.SageMaker.Types.TimeSeriesForecastingSettings
import Amazonka.SageMaker.Types.TrafficPattern
import Amazonka.SageMaker.Types.TrafficRoutingConfig
import Amazonka.SageMaker.Types.TrafficRoutingConfigType
import Amazonka.SageMaker.Types.TrafficType
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
import Amazonka.SageMaker.Types.VariantStatus
import Amazonka.SageMaker.Types.Vertex
import Amazonka.SageMaker.Types.VpcConfig
import Amazonka.SageMaker.Types.WarmPoolResourceStatus
import Amazonka.SageMaker.Types.WarmPoolStatus
import Amazonka.SageMaker.Types.Workforce
import Amazonka.SageMaker.Types.WorkforceStatus
import Amazonka.SageMaker.Types.WorkforceVpcConfigRequest
import Amazonka.SageMaker.Types.WorkforceVpcConfigResponse
import Amazonka.SageMaker.Types.Workteam
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-07-24@ of the Amazon SageMaker Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "SageMaker",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "api.sagemaker",
      Core.signingName = "sagemaker",
      Core.version = "2017-07-24",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "SageMaker",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Resource being access is not found.
_ResourceNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFound =
  Core._MatchServiceError
    defaultService
    "ResourceNotFound"

-- | You have exceeded an SageMaker resource limit. For example, you might
-- have too many training jobs created.
_ResourceLimitExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceeded"

-- | There was a conflict when you attempted to modify a SageMaker entity
-- such as an @Experiment@ or @Artifact@.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | Resource being accessed is in use.
_ResourceInUse :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUse =
  Core._MatchServiceError
    defaultService
    "ResourceInUse"

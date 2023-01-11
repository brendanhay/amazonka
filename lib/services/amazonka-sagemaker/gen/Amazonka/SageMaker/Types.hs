{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConflictException,
    _ResourceInUse,
    _ResourceLimitExceeded,
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

    -- * HubContentSortBy
    HubContentSortBy (..),

    -- * HubContentStatus
    HubContentStatus (..),

    -- * HubContentType
    HubContentType (..),

    -- * HubSortBy
    HubSortBy (..),

    -- * HubStatus
    HubStatus (..),

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

    -- * InferenceExperimentStatus
    InferenceExperimentStatus (..),

    -- * InferenceExperimentStopDesiredState
    InferenceExperimentStopDesiredState (..),

    -- * InferenceExperimentType
    InferenceExperimentType (..),

    -- * InputMode
    InputMode (..),

    -- * InstanceType
    InstanceType (..),

    -- * JobType
    JobType (..),

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

    -- * ModelCardExportJobSortBy
    ModelCardExportJobSortBy (..),

    -- * ModelCardExportJobSortOrder
    ModelCardExportJobSortOrder (..),

    -- * ModelCardExportJobStatus
    ModelCardExportJobStatus (..),

    -- * ModelCardProcessingStatus
    ModelCardProcessingStatus (..),

    -- * ModelCardSortBy
    ModelCardSortBy (..),

    -- * ModelCardSortOrder
    ModelCardSortOrder (..),

    -- * ModelCardStatus
    ModelCardStatus (..),

    -- * ModelCardVersionSortBy
    ModelCardVersionSortBy (..),

    -- * ModelInfrastructureType
    ModelInfrastructureType (..),

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

    -- * ModelVariantAction
    ModelVariantAction (..),

    -- * ModelVariantStatus
    ModelVariantStatus (..),

    -- * MonitoringAlertHistorySortKey
    MonitoringAlertHistorySortKey (..),

    -- * MonitoringAlertStatus
    MonitoringAlertStatus (..),

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

    -- * Processor
    Processor (..),

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

    -- * SortInferenceExperimentsBy
    SortInferenceExperimentsBy (..),

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

    -- * SpaceSortKey
    SpaceSortKey (..),

    -- * SpaceStatus
    SpaceStatus (..),

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

    -- * TableFormat
    TableFormat (..),

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

    -- * VendorGuidance
    VendorGuidance (..),

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
    actionSummary_actionArn,
    actionSummary_actionName,
    actionSummary_actionType,
    actionSummary_creationTime,
    actionSummary_lastModifiedTime,
    actionSummary_source,
    actionSummary_status,

    -- * AdditionalInferenceSpecificationDefinition
    AdditionalInferenceSpecificationDefinition (..),
    newAdditionalInferenceSpecificationDefinition,
    additionalInferenceSpecificationDefinition_description,
    additionalInferenceSpecificationDefinition_supportedContentTypes,
    additionalInferenceSpecificationDefinition_supportedRealtimeInferenceInstanceTypes,
    additionalInferenceSpecificationDefinition_supportedResponseMIMETypes,
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
    algorithmSpecification_algorithmName,
    algorithmSpecification_containerArguments,
    algorithmSpecification_containerEntrypoint,
    algorithmSpecification_enableSageMakerMetricsTimeSeries,
    algorithmSpecification_metricDefinitions,
    algorithmSpecification_trainingImage,
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
    appDetails_appName,
    appDetails_appType,
    appDetails_creationTime,
    appDetails_domainId,
    appDetails_spaceName,
    appDetails_status,
    appDetails_userProfileName,

    -- * AppImageConfigDetails
    AppImageConfigDetails (..),
    newAppImageConfigDetails,
    appImageConfigDetails_appImageConfigArn,
    appImageConfigDetails_appImageConfigName,
    appImageConfigDetails_creationTime,
    appImageConfigDetails_kernelGatewayImageConfig,
    appImageConfigDetails_lastModifiedTime,

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
    artifactSummary_artifactArn,
    artifactSummary_artifactName,
    artifactSummary_artifactType,
    artifactSummary_creationTime,
    artifactSummary_lastModifiedTime,
    artifactSummary_source,

    -- * AssociationSummary
    AssociationSummary (..),
    newAssociationSummary,
    associationSummary_associationType,
    associationSummary_createdBy,
    associationSummary_creationTime,
    associationSummary_destinationArn,
    associationSummary_destinationName,
    associationSummary_destinationType,
    associationSummary_sourceArn,
    associationSummary_sourceName,
    associationSummary_sourceType,

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
    athenaDatasetDefinition_kmsKeyId,
    athenaDatasetDefinition_outputCompression,
    athenaDatasetDefinition_workGroup,
    athenaDatasetDefinition_catalog,
    athenaDatasetDefinition_database,
    athenaDatasetDefinition_queryString,
    athenaDatasetDefinition_outputS3Uri,
    athenaDatasetDefinition_outputFormat,

    -- * AutoMLCandidate
    AutoMLCandidate (..),
    newAutoMLCandidate,
    autoMLCandidate_candidateProperties,
    autoMLCandidate_endTime,
    autoMLCandidate_failureReason,
    autoMLCandidate_finalAutoMLJobObjectiveMetric,
    autoMLCandidate_inferenceContainers,
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
    autoMLChannel_channelType,
    autoMLChannel_compressionType,
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
    autoMLJobArtifacts_candidateDefinitionNotebookLocation,
    autoMLJobArtifacts_dataExplorationNotebookLocation,

    -- * AutoMLJobCompletionCriteria
    AutoMLJobCompletionCriteria (..),
    newAutoMLJobCompletionCriteria,
    autoMLJobCompletionCriteria_maxAutoMLJobRuntimeInSeconds,
    autoMLJobCompletionCriteria_maxCandidates,
    autoMLJobCompletionCriteria_maxRuntimePerTrainingJobInSeconds,

    -- * AutoMLJobConfig
    AutoMLJobConfig (..),
    newAutoMLJobConfig,
    autoMLJobConfig_candidateGenerationConfig,
    autoMLJobConfig_completionCriteria,
    autoMLJobConfig_dataSplitConfig,
    autoMLJobConfig_mode,
    autoMLJobConfig_securityConfig,

    -- * AutoMLJobObjective
    AutoMLJobObjective (..),
    newAutoMLJobObjective,
    autoMLJobObjective_metricName,

    -- * AutoMLJobStepMetadata
    AutoMLJobStepMetadata (..),
    newAutoMLJobStepMetadata,
    autoMLJobStepMetadata_arn,

    -- * AutoMLJobSummary
    AutoMLJobSummary (..),
    newAutoMLJobSummary,
    autoMLJobSummary_endTime,
    autoMLJobSummary_failureReason,
    autoMLJobSummary_partialFailureReasons,
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
    autoMLSecurityConfig_enableInterContainerTrafficEncryption,
    autoMLSecurityConfig_volumeKmsKeyId,
    autoMLSecurityConfig_vpcConfig,

    -- * AutoRollbackConfig
    AutoRollbackConfig (..),
    newAutoRollbackConfig,
    autoRollbackConfig_alarms,

    -- * BatchDataCaptureConfig
    BatchDataCaptureConfig (..),
    newBatchDataCaptureConfig,
    batchDataCaptureConfig_generateInferenceId,
    batchDataCaptureConfig_kmsKeyId,
    batchDataCaptureConfig_destinationS3Uri,

    -- * BatchDescribeModelPackageError
    BatchDescribeModelPackageError (..),
    newBatchDescribeModelPackageError,
    batchDescribeModelPackageError_errorCode,
    batchDescribeModelPackageError_errorResponse,

    -- * BatchDescribeModelPackageSummary
    BatchDescribeModelPackageSummary (..),
    newBatchDescribeModelPackageSummary,
    batchDescribeModelPackageSummary_modelApprovalStatus,
    batchDescribeModelPackageSummary_modelPackageDescription,
    batchDescribeModelPackageSummary_modelPackageVersion,
    batchDescribeModelPackageSummary_modelPackageGroupName,
    batchDescribeModelPackageSummary_modelPackageArn,
    batchDescribeModelPackageSummary_creationTime,
    batchDescribeModelPackageSummary_inferenceSpecification,
    batchDescribeModelPackageSummary_modelPackageStatus,

    -- * BatchTransformInput
    BatchTransformInput (..),
    newBatchTransformInput,
    batchTransformInput_endTimeOffset,
    batchTransformInput_featuresAttribute,
    batchTransformInput_inferenceAttribute,
    batchTransformInput_probabilityAttribute,
    batchTransformInput_probabilityThresholdAttribute,
    batchTransformInput_s3DataDistributionType,
    batchTransformInput_s3InputMode,
    batchTransformInput_startTimeOffset,
    batchTransformInput_dataCapturedDestinationS3Uri,
    batchTransformInput_datasetFormat,
    batchTransformInput_localPath,

    -- * Bias
    Bias (..),
    newBias,
    bias_postTrainingReport,
    bias_preTrainingReport,
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
    candidateArtifactLocations_modelInsights,
    candidateArtifactLocations_explainability,

    -- * CandidateProperties
    CandidateProperties (..),
    newCandidateProperties,
    candidateProperties_candidateArtifactLocations,
    candidateProperties_candidateMetrics,

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
    channel_compressionType,
    channel_contentType,
    channel_inputMode,
    channel_recordWrapperType,
    channel_shuffleConfig,
    channel_channelName,
    channel_dataSource,

    -- * ChannelSpecification
    ChannelSpecification (..),
    newChannelSpecification,
    channelSpecification_description,
    channelSpecification_isRequired,
    channelSpecification_supportedCompressionTypes,
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
    clarifyCheckStepMetadata_baselineUsedForDriftCheckConstraints,
    clarifyCheckStepMetadata_calculatedBaselineConstraints,
    clarifyCheckStepMetadata_checkJobArn,
    clarifyCheckStepMetadata_checkType,
    clarifyCheckStepMetadata_modelPackageGroupName,
    clarifyCheckStepMetadata_registerNewBaseline,
    clarifyCheckStepMetadata_skipCheck,
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
    clarifyInferenceConfig_contentTemplate,
    clarifyInferenceConfig_featureHeaders,
    clarifyInferenceConfig_featureTypes,
    clarifyInferenceConfig_featuresAttribute,
    clarifyInferenceConfig_labelAttribute,
    clarifyInferenceConfig_labelHeaders,
    clarifyInferenceConfig_labelIndex,
    clarifyInferenceConfig_maxPayloadInMB,
    clarifyInferenceConfig_maxRecordCount,
    clarifyInferenceConfig_probabilityAttribute,
    clarifyInferenceConfig_probabilityIndex,

    -- * ClarifyShapBaselineConfig
    ClarifyShapBaselineConfig (..),
    newClarifyShapBaselineConfig,
    clarifyShapBaselineConfig_mimeType,
    clarifyShapBaselineConfig_shapBaseline,
    clarifyShapBaselineConfig_shapBaselineUri,

    -- * ClarifyShapConfig
    ClarifyShapConfig (..),
    newClarifyShapConfig,
    clarifyShapConfig_numberOfSamples,
    clarifyShapConfig_seed,
    clarifyShapConfig_textConfig,
    clarifyShapConfig_useLogit,
    clarifyShapConfig_shapBaselineConfig,

    -- * ClarifyTextConfig
    ClarifyTextConfig (..),
    newClarifyTextConfig,
    clarifyTextConfig_language,
    clarifyTextConfig_granularity,

    -- * CodeRepository
    CodeRepository (..),
    newCodeRepository,
    codeRepository_repositoryUrl,

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
    compilationJobSummary_compilationStartTime,
    compilationJobSummary_compilationTargetDevice,
    compilationJobSummary_compilationTargetPlatformAccelerator,
    compilationJobSummary_compilationTargetPlatformArch,
    compilationJobSummary_compilationTargetPlatformOs,
    compilationJobSummary_lastModifiedTime,
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
    containerDefinition_containerHostname,
    containerDefinition_environment,
    containerDefinition_image,
    containerDefinition_imageConfig,
    containerDefinition_inferenceSpecificationName,
    containerDefinition_mode,
    containerDefinition_modelDataUrl,
    containerDefinition_modelPackageName,
    containerDefinition_multiModelConfig,

    -- * ContextSource
    ContextSource (..),
    newContextSource,
    contextSource_sourceId,
    contextSource_sourceType,
    contextSource_sourceUri,

    -- * ContextSummary
    ContextSummary (..),
    newContextSummary,
    contextSummary_contextArn,
    contextSummary_contextName,
    contextSummary_contextType,
    contextSummary_creationTime,
    contextSummary_lastModifiedTime,
    contextSummary_source,

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
    dataCaptureConfig_enableCapture,
    dataCaptureConfig_kmsKeyId,
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
    dataProcessing_inputFilter,
    dataProcessing_joinSource,
    dataProcessing_outputFilter,

    -- * DataQualityAppSpecification
    DataQualityAppSpecification (..),
    newDataQualityAppSpecification,
    dataQualityAppSpecification_containerArguments,
    dataQualityAppSpecification_containerEntrypoint,
    dataQualityAppSpecification_environment,
    dataQualityAppSpecification_postAnalyticsProcessorSourceUri,
    dataQualityAppSpecification_recordPreprocessorSourceUri,
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
    dataQualityJobInput_batchTransformInput,
    dataQualityJobInput_endpointInput,

    -- * DataSource
    DataSource (..),
    newDataSource,
    dataSource_fileSystemDataSource,
    dataSource_s3DataSource,

    -- * DatasetDefinition
    DatasetDefinition (..),
    newDatasetDefinition,
    datasetDefinition_athenaDatasetDefinition,
    datasetDefinition_dataDistributionType,
    datasetDefinition_inputMode,
    datasetDefinition_localPath,
    datasetDefinition_redshiftDatasetDefinition,

    -- * DebugHookConfig
    DebugHookConfig (..),
    newDebugHookConfig,
    debugHookConfig_collectionConfigurations,
    debugHookConfig_hookParameters,
    debugHookConfig_localPath,
    debugHookConfig_s3OutputPath,

    -- * DebugRuleConfiguration
    DebugRuleConfiguration (..),
    newDebugRuleConfiguration,
    debugRuleConfiguration_instanceType,
    debugRuleConfiguration_localPath,
    debugRuleConfiguration_ruleParameters,
    debugRuleConfiguration_s3OutputPath,
    debugRuleConfiguration_volumeSizeInGB,
    debugRuleConfiguration_ruleConfigurationName,
    debugRuleConfiguration_ruleEvaluatorImage,

    -- * DebugRuleEvaluationStatus
    DebugRuleEvaluationStatus (..),
    newDebugRuleEvaluationStatus,
    debugRuleEvaluationStatus_lastModifiedTime,
    debugRuleEvaluationStatus_ruleConfigurationName,
    debugRuleEvaluationStatus_ruleEvaluationJobArn,
    debugRuleEvaluationStatus_ruleEvaluationStatus,
    debugRuleEvaluationStatus_statusDetails,

    -- * DefaultSpaceSettings
    DefaultSpaceSettings (..),
    newDefaultSpaceSettings,
    defaultSpaceSettings_executionRole,
    defaultSpaceSettings_jupyterServerAppSettings,
    defaultSpaceSettings_kernelGatewayAppSettings,
    defaultSpaceSettings_securityGroups,

    -- * DeployedImage
    DeployedImage (..),
    newDeployedImage,
    deployedImage_resolutionTime,
    deployedImage_resolvedImage,
    deployedImage_specifiedImage,

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
    desiredWeightAndCapacity_desiredInstanceCount,
    desiredWeightAndCapacity_desiredWeight,
    desiredWeightAndCapacity_variantName,

    -- * Device
    Device (..),
    newDevice,
    device_description,
    device_iotThingName,
    device_deviceName,

    -- * DeviceDeploymentSummary
    DeviceDeploymentSummary (..),
    newDeviceDeploymentSummary,
    deviceDeploymentSummary_deployedStageName,
    deviceDeploymentSummary_deploymentStartTime,
    deviceDeploymentSummary_description,
    deviceDeploymentSummary_deviceDeploymentStatus,
    deviceDeploymentSummary_deviceDeploymentStatusMessage,
    deviceDeploymentSummary_deviceFleetName,
    deviceDeploymentSummary_edgeDeploymentPlanArn,
    deviceDeploymentSummary_edgeDeploymentPlanName,
    deviceDeploymentSummary_stageName,
    deviceDeploymentSummary_deviceName,
    deviceDeploymentSummary_deviceArn,

    -- * DeviceFleetSummary
    DeviceFleetSummary (..),
    newDeviceFleetSummary,
    deviceFleetSummary_creationTime,
    deviceFleetSummary_lastModifiedTime,
    deviceFleetSummary_deviceFleetArn,
    deviceFleetSummary_deviceFleetName,

    -- * DeviceSelectionConfig
    DeviceSelectionConfig (..),
    newDeviceSelectionConfig,
    deviceSelectionConfig_deviceNameContains,
    deviceSelectionConfig_deviceNames,
    deviceSelectionConfig_percentage,
    deviceSelectionConfig_deviceSubsetType,

    -- * DeviceStats
    DeviceStats (..),
    newDeviceStats,
    deviceStats_connectedDeviceCount,
    deviceStats_registeredDeviceCount,

    -- * DeviceSummary
    DeviceSummary (..),
    newDeviceSummary,
    deviceSummary_agentVersion,
    deviceSummary_description,
    deviceSummary_deviceFleetName,
    deviceSummary_iotThingName,
    deviceSummary_latestHeartbeat,
    deviceSummary_models,
    deviceSummary_registrationTime,
    deviceSummary_deviceName,
    deviceSummary_deviceArn,

    -- * DomainDetails
    DomainDetails (..),
    newDomainDetails,
    domainDetails_creationTime,
    domainDetails_domainArn,
    domainDetails_domainId,
    domainDetails_domainName,
    domainDetails_lastModifiedTime,
    domainDetails_status,
    domainDetails_url,

    -- * DomainSettings
    DomainSettings (..),
    newDomainSettings,
    domainSettings_executionRoleIdentityConfig,
    domainSettings_rStudioServerProDomainSettings,
    domainSettings_securityGroupIds,

    -- * DomainSettingsForUpdate
    DomainSettingsForUpdate (..),
    newDomainSettingsForUpdate,
    domainSettingsForUpdate_executionRoleIdentityConfig,
    domainSettingsForUpdate_rStudioServerProDomainSettingsForUpdate,
    domainSettingsForUpdate_securityGroupIds,

    -- * DriftCheckBaselines
    DriftCheckBaselines (..),
    newDriftCheckBaselines,
    driftCheckBaselines_bias,
    driftCheckBaselines_explainability,
    driftCheckBaselines_modelDataQuality,
    driftCheckBaselines_modelQuality,

    -- * DriftCheckBias
    DriftCheckBias (..),
    newDriftCheckBias,
    driftCheckBias_configFile,
    driftCheckBias_postTrainingConstraints,
    driftCheckBias_preTrainingConstraints,

    -- * DriftCheckExplainability
    DriftCheckExplainability (..),
    newDriftCheckExplainability,
    driftCheckExplainability_configFile,
    driftCheckExplainability_constraints,

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
    eMRStepMetadata_clusterId,
    eMRStepMetadata_logFilePath,
    eMRStepMetadata_stepId,
    eMRStepMetadata_stepName,

    -- * Edge
    Edge (..),
    newEdge,
    edge_associationType,
    edge_destinationArn,
    edge_sourceArn,

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
    edgeDeploymentPlanSummary_creationTime,
    edgeDeploymentPlanSummary_lastModifiedTime,
    edgeDeploymentPlanSummary_edgeDeploymentPlanArn,
    edgeDeploymentPlanSummary_edgeDeploymentPlanName,
    edgeDeploymentPlanSummary_deviceFleetName,
    edgeDeploymentPlanSummary_edgeDeploymentSuccess,
    edgeDeploymentPlanSummary_edgeDeploymentPending,
    edgeDeploymentPlanSummary_edgeDeploymentFailed,

    -- * EdgeDeploymentStatus
    EdgeDeploymentStatus (..),
    newEdgeDeploymentStatus,
    edgeDeploymentStatus_edgeDeploymentStageStartTime,
    edgeDeploymentStatus_edgeDeploymentStatusMessage,
    edgeDeploymentStatus_stageStatus,
    edgeDeploymentStatus_edgeDeploymentSuccessInStage,
    edgeDeploymentStatus_edgeDeploymentPendingInStage,
    edgeDeploymentStatus_edgeDeploymentFailedInStage,

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
    edgeOutputConfig_kmsKeyId,
    edgeOutputConfig_presetDeploymentConfig,
    edgeOutputConfig_presetDeploymentType,
    edgeOutputConfig_s3OutputLocation,

    -- * EdgePackagingJobSummary
    EdgePackagingJobSummary (..),
    newEdgePackagingJobSummary,
    edgePackagingJobSummary_compilationJobName,
    edgePackagingJobSummary_creationTime,
    edgePackagingJobSummary_lastModifiedTime,
    edgePackagingJobSummary_modelName,
    edgePackagingJobSummary_modelVersion,
    edgePackagingJobSummary_edgePackagingJobArn,
    edgePackagingJobSummary_edgePackagingJobName,
    edgePackagingJobSummary_edgePackagingJobStatus,

    -- * EdgePresetDeploymentOutput
    EdgePresetDeploymentOutput (..),
    newEdgePresetDeploymentOutput,
    edgePresetDeploymentOutput_artifact,
    edgePresetDeploymentOutput_status,
    edgePresetDeploymentOutput_statusMessage,
    edgePresetDeploymentOutput_type,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_dataCaptureConfig,
    endpoint_failureReason,
    endpoint_monitoringSchedules,
    endpoint_productionVariants,
    endpoint_shadowProductionVariants,
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

    -- * EndpointInfo
    EndpointInfo (..),
    newEndpointInfo,
    endpointInfo_endpointName,

    -- * EndpointInput
    EndpointInput (..),
    newEndpointInput,
    endpointInput_endTimeOffset,
    endpointInput_featuresAttribute,
    endpointInput_inferenceAttribute,
    endpointInput_probabilityAttribute,
    endpointInput_probabilityThresholdAttribute,
    endpointInput_s3DataDistributionType,
    endpointInput_s3InputMode,
    endpointInput_startTimeOffset,
    endpointInput_endpointName,
    endpointInput_localPath,

    -- * EndpointInputConfiguration
    EndpointInputConfiguration (..),
    newEndpointInputConfiguration,
    endpointInputConfiguration_environmentParameterRanges,
    endpointInputConfiguration_inferenceSpecificationName,
    endpointInputConfiguration_instanceType,

    -- * EndpointMetadata
    EndpointMetadata (..),
    newEndpointMetadata,
    endpointMetadata_endpointConfigName,
    endpointMetadata_endpointStatus,
    endpointMetadata_failureReason,
    endpointMetadata_endpointName,

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
    experiment_createdBy,
    experiment_creationTime,
    experiment_description,
    experiment_displayName,
    experiment_experimentArn,
    experiment_experimentName,
    experiment_lastModifiedBy,
    experiment_lastModifiedTime,
    experiment_source,
    experiment_tags,

    -- * ExperimentConfig
    ExperimentConfig (..),
    newExperimentConfig,
    experimentConfig_experimentName,
    experimentConfig_runName,
    experimentConfig_trialComponentDisplayName,
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
    experimentSummary_displayName,
    experimentSummary_experimentArn,
    experimentSummary_experimentName,
    experimentSummary_experimentSource,
    experimentSummary_lastModifiedTime,

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
    featureDefinition_featureName,
    featureDefinition_featureType,

    -- * FeatureGroup
    FeatureGroup (..),
    newFeatureGroup,
    featureGroup_creationTime,
    featureGroup_description,
    featureGroup_eventTimeFeatureName,
    featureGroup_failureReason,
    featureGroup_featureDefinitions,
    featureGroup_featureGroupArn,
    featureGroup_featureGroupName,
    featureGroup_featureGroupStatus,
    featureGroup_lastModifiedTime,
    featureGroup_lastUpdateStatus,
    featureGroup_offlineStoreConfig,
    featureGroup_offlineStoreStatus,
    featureGroup_onlineStoreConfig,
    featureGroup_recordIdentifierFeatureName,
    featureGroup_roleArn,
    featureGroup_tags,

    -- * FeatureGroupSummary
    FeatureGroupSummary (..),
    newFeatureGroupSummary,
    featureGroupSummary_featureGroupStatus,
    featureGroupSummary_offlineStoreStatus,
    featureGroupSummary_featureGroupName,
    featureGroupSummary_featureGroupArn,
    featureGroupSummary_creationTime,

    -- * FeatureMetadata
    FeatureMetadata (..),
    newFeatureMetadata,
    featureMetadata_creationTime,
    featureMetadata_description,
    featureMetadata_featureGroupArn,
    featureMetadata_featureGroupName,
    featureMetadata_featureName,
    featureMetadata_featureType,
    featureMetadata_lastModifiedTime,
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

    -- * HubContentDependency
    HubContentDependency (..),
    newHubContentDependency,
    hubContentDependency_dependencyCopyPath,
    hubContentDependency_dependencyOriginPath,

    -- * HubContentInfo
    HubContentInfo (..),
    newHubContentInfo,
    hubContentInfo_hubContentDescription,
    hubContentInfo_hubContentDisplayName,
    hubContentInfo_hubContentSearchKeywords,
    hubContentInfo_hubContentName,
    hubContentInfo_hubContentArn,
    hubContentInfo_hubContentVersion,
    hubContentInfo_hubContentType,
    hubContentInfo_documentSchemaVersion,
    hubContentInfo_hubContentStatus,
    hubContentInfo_creationTime,

    -- * HubInfo
    HubInfo (..),
    newHubInfo,
    hubInfo_hubDescription,
    hubInfo_hubDisplayName,
    hubInfo_hubSearchKeywords,
    hubInfo_hubName,
    hubInfo_hubArn,
    hubInfo_hubStatus,
    hubInfo_creationTime,
    hubInfo_lastModifiedTime,

    -- * HubS3StorageConfig
    HubS3StorageConfig (..),
    newHubS3StorageConfig,
    hubS3StorageConfig_s3OutputPath,

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
    humanLoopConfig_taskAvailabilityLifetimeInSeconds,
    humanLoopConfig_taskKeywords,
    humanLoopConfig_taskTimeLimitInSeconds,
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
    humanTaskConfig_taskAvailabilityLifetimeInSeconds,
    humanTaskConfig_taskKeywords,
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
    hyperParameterSpecification_isRequired,
    hyperParameterSpecification_isTunable,
    hyperParameterSpecification_range,
    hyperParameterSpecification_name,
    hyperParameterSpecification_type,

    -- * HyperParameterTrainingJobDefinition
    HyperParameterTrainingJobDefinition (..),
    newHyperParameterTrainingJobDefinition,
    hyperParameterTrainingJobDefinition_checkpointConfig,
    hyperParameterTrainingJobDefinition_definitionName,
    hyperParameterTrainingJobDefinition_enableInterContainerTrafficEncryption,
    hyperParameterTrainingJobDefinition_enableManagedSpotTraining,
    hyperParameterTrainingJobDefinition_enableNetworkIsolation,
    hyperParameterTrainingJobDefinition_hyperParameterRanges,
    hyperParameterTrainingJobDefinition_hyperParameterTuningResourceConfig,
    hyperParameterTrainingJobDefinition_inputDataConfig,
    hyperParameterTrainingJobDefinition_resourceConfig,
    hyperParameterTrainingJobDefinition_retryStrategy,
    hyperParameterTrainingJobDefinition_staticHyperParameters,
    hyperParameterTrainingJobDefinition_tuningObjective,
    hyperParameterTrainingJobDefinition_vpcConfig,
    hyperParameterTrainingJobDefinition_algorithmSpecification,
    hyperParameterTrainingJobDefinition_roleArn,
    hyperParameterTrainingJobDefinition_outputDataConfig,
    hyperParameterTrainingJobDefinition_stoppingCondition,

    -- * HyperParameterTrainingJobSummary
    HyperParameterTrainingJobSummary (..),
    newHyperParameterTrainingJobSummary,
    hyperParameterTrainingJobSummary_failureReason,
    hyperParameterTrainingJobSummary_finalHyperParameterTuningJobObjectiveMetric,
    hyperParameterTrainingJobSummary_objectiveStatus,
    hyperParameterTrainingJobSummary_trainingEndTime,
    hyperParameterTrainingJobSummary_trainingJobDefinitionName,
    hyperParameterTrainingJobSummary_trainingStartTime,
    hyperParameterTrainingJobSummary_tuningJobName,
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
    hyperParameterTuningJobConfig_hyperParameterTuningJobObjective,
    hyperParameterTuningJobConfig_parameterRanges,
    hyperParameterTuningJobConfig_randomSeed,
    hyperParameterTuningJobConfig_strategyConfig,
    hyperParameterTuningJobConfig_trainingJobEarlyStoppingType,
    hyperParameterTuningJobConfig_tuningJobCompletionCriteria,
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
    hyperParameterTuningJobSearchEntity_bestTrainingJob,
    hyperParameterTuningJobSearchEntity_creationTime,
    hyperParameterTuningJobSearchEntity_failureReason,
    hyperParameterTuningJobSearchEntity_hyperParameterTuningEndTime,
    hyperParameterTuningJobSearchEntity_hyperParameterTuningJobArn,
    hyperParameterTuningJobSearchEntity_hyperParameterTuningJobConfig,
    hyperParameterTuningJobSearchEntity_hyperParameterTuningJobName,
    hyperParameterTuningJobSearchEntity_hyperParameterTuningJobStatus,
    hyperParameterTuningJobSearchEntity_lastModifiedTime,
    hyperParameterTuningJobSearchEntity_objectiveStatusCounters,
    hyperParameterTuningJobSearchEntity_overallBestTrainingJob,
    hyperParameterTuningJobSearchEntity_tags,
    hyperParameterTuningJobSearchEntity_trainingJobDefinition,
    hyperParameterTuningJobSearchEntity_trainingJobDefinitions,
    hyperParameterTuningJobSearchEntity_trainingJobStatusCounters,
    hyperParameterTuningJobSearchEntity_warmStartConfig,

    -- * HyperParameterTuningJobStrategyConfig
    HyperParameterTuningJobStrategyConfig (..),
    newHyperParameterTuningJobStrategyConfig,
    hyperParameterTuningJobStrategyConfig_hyperbandStrategyConfig,

    -- * HyperParameterTuningJobSummary
    HyperParameterTuningJobSummary (..),
    newHyperParameterTuningJobSummary,
    hyperParameterTuningJobSummary_hyperParameterTuningEndTime,
    hyperParameterTuningJobSummary_lastModifiedTime,
    hyperParameterTuningJobSummary_resourceLimits,
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
    hyperParameterTuningResourceConfig_allocationStrategy,
    hyperParameterTuningResourceConfig_instanceConfigs,
    hyperParameterTuningResourceConfig_instanceCount,
    hyperParameterTuningResourceConfig_instanceType,
    hyperParameterTuningResourceConfig_volumeKmsKeyId,
    hyperParameterTuningResourceConfig_volumeSizeInGB,

    -- * HyperbandStrategyConfig
    HyperbandStrategyConfig (..),
    newHyperbandStrategyConfig,
    hyperbandStrategyConfig_maxResource,
    hyperbandStrategyConfig_minResource,

    -- * Image
    Image (..),
    newImage,
    image_description,
    image_displayName,
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

    -- * InferenceExperimentDataStorageConfig
    InferenceExperimentDataStorageConfig (..),
    newInferenceExperimentDataStorageConfig,
    inferenceExperimentDataStorageConfig_contentType,
    inferenceExperimentDataStorageConfig_kmsKey,
    inferenceExperimentDataStorageConfig_destination,

    -- * InferenceExperimentSchedule
    InferenceExperimentSchedule (..),
    newInferenceExperimentSchedule,
    inferenceExperimentSchedule_endTime,
    inferenceExperimentSchedule_startTime,

    -- * InferenceExperimentSummary
    InferenceExperimentSummary (..),
    newInferenceExperimentSummary,
    inferenceExperimentSummary_completionTime,
    inferenceExperimentSummary_description,
    inferenceExperimentSummary_roleArn,
    inferenceExperimentSummary_schedule,
    inferenceExperimentSummary_statusReason,
    inferenceExperimentSummary_name,
    inferenceExperimentSummary_type,
    inferenceExperimentSummary_status,
    inferenceExperimentSummary_creationTime,
    inferenceExperimentSummary_lastModifiedTime,

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
    jupyterServerAppSettings_codeRepositories,
    jupyterServerAppSettings_defaultResourceSpec,
    jupyterServerAppSettings_lifecycleConfigArns,

    -- * KernelGatewayAppSettings
    KernelGatewayAppSettings (..),
    newKernelGatewayAppSettings,
    kernelGatewayAppSettings_customImages,
    kernelGatewayAppSettings_defaultResourceSpec,
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
    labelCounters_failedNonRetryableError,
    labelCounters_humanLabeled,
    labelCounters_machineLabeled,
    labelCounters_totalLabeled,
    labelCounters_unlabeled,

    -- * LabelCountersForWorkteam
    LabelCountersForWorkteam (..),
    newLabelCountersForWorkteam,
    labelCountersForWorkteam_humanLabeled,
    labelCountersForWorkteam_pendingHuman,
    labelCountersForWorkteam_total,

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
    labelingJobForWorkteamSummary_labelCounters,
    labelingJobForWorkteamSummary_labelingJobName,
    labelingJobForWorkteamSummary_numberOfHumanWorkersPerDataObject,
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
    labelingJobOutputConfig_kmsKeyId,
    labelingJobOutputConfig_snsTopicArn,
    labelingJobOutputConfig_s3OutputPath,

    -- * LabelingJobResourceConfig
    LabelingJobResourceConfig (..),
    newLabelingJobResourceConfig,
    labelingJobResourceConfig_volumeKmsKeyId,
    labelingJobResourceConfig_vpcConfig,

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
    labelingJobSummary_annotationConsolidationLambdaArn,
    labelingJobSummary_failureReason,
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

    -- * LastUpdateStatus
    LastUpdateStatus (..),
    newLastUpdateStatus,
    lastUpdateStatus_failureReason,
    lastUpdateStatus_status,

    -- * LineageGroupSummary
    LineageGroupSummary (..),
    newLineageGroupSummary,
    lineageGroupSummary_creationTime,
    lineageGroupSummary_displayName,
    lineageGroupSummary_lastModifiedTime,
    lineageGroupSummary_lineageGroupArn,
    lineageGroupSummary_lineageGroupName,

    -- * MemberDefinition
    MemberDefinition (..),
    newMemberDefinition,
    memberDefinition_cognitoMemberDefinition,
    memberDefinition_oidcMemberDefinition,

    -- * MetadataProperties
    MetadataProperties (..),
    newMetadataProperties,
    metadataProperties_commitId,
    metadataProperties_generatedBy,
    metadataProperties_projectId,
    metadataProperties_repository,

    -- * MetricData
    MetricData (..),
    newMetricData,
    metricData_metricName,
    metricData_timestamp,
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

    -- * Model
    Model (..),
    newModel,
    model_containers,
    model_creationTime,
    model_enableNetworkIsolation,
    model_executionRoleArn,
    model_inferenceExecutionConfig,
    model_modelArn,
    model_modelName,
    model_primaryContainer,
    model_tags,
    model_vpcConfig,

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
    modelBiasJobInput_batchTransformInput,
    modelBiasJobInput_endpointInput,
    modelBiasJobInput_groundTruthS3Input,

    -- * ModelCard
    ModelCard (..),
    newModelCard,
    modelCard_content,
    modelCard_createdBy,
    modelCard_creationTime,
    modelCard_lastModifiedBy,
    modelCard_lastModifiedTime,
    modelCard_modelCardArn,
    modelCard_modelCardName,
    modelCard_modelCardStatus,
    modelCard_modelCardVersion,
    modelCard_modelId,
    modelCard_riskRating,
    modelCard_securityConfig,
    modelCard_tags,

    -- * ModelCardExportArtifacts
    ModelCardExportArtifacts (..),
    newModelCardExportArtifacts,
    modelCardExportArtifacts_s3ExportArtifacts,

    -- * ModelCardExportJobSummary
    ModelCardExportJobSummary (..),
    newModelCardExportJobSummary,
    modelCardExportJobSummary_modelCardExportJobName,
    modelCardExportJobSummary_modelCardExportJobArn,
    modelCardExportJobSummary_status,
    modelCardExportJobSummary_modelCardName,
    modelCardExportJobSummary_modelCardVersion,
    modelCardExportJobSummary_createdAt,
    modelCardExportJobSummary_lastModifiedAt,

    -- * ModelCardExportOutputConfig
    ModelCardExportOutputConfig (..),
    newModelCardExportOutputConfig,
    modelCardExportOutputConfig_s3OutputPath,

    -- * ModelCardSecurityConfig
    ModelCardSecurityConfig (..),
    newModelCardSecurityConfig,
    modelCardSecurityConfig_kmsKeyId,

    -- * ModelCardSummary
    ModelCardSummary (..),
    newModelCardSummary,
    modelCardSummary_lastModifiedTime,
    modelCardSummary_modelCardName,
    modelCardSummary_modelCardArn,
    modelCardSummary_modelCardStatus,
    modelCardSummary_creationTime,

    -- * ModelCardVersionSummary
    ModelCardVersionSummary (..),
    newModelCardVersionSummary,
    modelCardVersionSummary_lastModifiedTime,
    modelCardVersionSummary_modelCardName,
    modelCardVersionSummary_modelCardArn,
    modelCardVersionSummary_modelCardStatus,
    modelCardVersionSummary_modelCardVersion,
    modelCardVersionSummary_creationTime,

    -- * ModelClientConfig
    ModelClientConfig (..),
    newModelClientConfig,
    modelClientConfig_invocationsMaxRetries,
    modelClientConfig_invocationsTimeoutInSeconds,

    -- * ModelConfiguration
    ModelConfiguration (..),
    newModelConfiguration,
    modelConfiguration_environmentParameters,
    modelConfiguration_inferenceSpecificationName,

    -- * ModelDashboardEndpoint
    ModelDashboardEndpoint (..),
    newModelDashboardEndpoint,
    modelDashboardEndpoint_endpointName,
    modelDashboardEndpoint_endpointArn,
    modelDashboardEndpoint_creationTime,
    modelDashboardEndpoint_lastModifiedTime,
    modelDashboardEndpoint_endpointStatus,

    -- * ModelDashboardIndicatorAction
    ModelDashboardIndicatorAction (..),
    newModelDashboardIndicatorAction,
    modelDashboardIndicatorAction_enabled,

    -- * ModelDashboardModel
    ModelDashboardModel (..),
    newModelDashboardModel,
    modelDashboardModel_endpoints,
    modelDashboardModel_lastBatchTransformJob,
    modelDashboardModel_model,
    modelDashboardModel_modelCard,
    modelDashboardModel_monitoringSchedules,

    -- * ModelDashboardModelCard
    ModelDashboardModelCard (..),
    newModelDashboardModelCard,
    modelDashboardModelCard_createdBy,
    modelDashboardModelCard_creationTime,
    modelDashboardModelCard_lastModifiedBy,
    modelDashboardModelCard_lastModifiedTime,
    modelDashboardModelCard_modelCardArn,
    modelDashboardModelCard_modelCardName,
    modelDashboardModelCard_modelCardStatus,
    modelDashboardModelCard_modelCardVersion,
    modelDashboardModelCard_modelId,
    modelDashboardModelCard_riskRating,
    modelDashboardModelCard_securityConfig,
    modelDashboardModelCard_tags,

    -- * ModelDashboardMonitoringSchedule
    ModelDashboardMonitoringSchedule (..),
    newModelDashboardMonitoringSchedule,
    modelDashboardMonitoringSchedule_creationTime,
    modelDashboardMonitoringSchedule_endpointName,
    modelDashboardMonitoringSchedule_failureReason,
    modelDashboardMonitoringSchedule_lastModifiedTime,
    modelDashboardMonitoringSchedule_lastMonitoringExecutionSummary,
    modelDashboardMonitoringSchedule_monitoringAlertSummaries,
    modelDashboardMonitoringSchedule_monitoringScheduleArn,
    modelDashboardMonitoringSchedule_monitoringScheduleConfig,
    modelDashboardMonitoringSchedule_monitoringScheduleName,
    modelDashboardMonitoringSchedule_monitoringScheduleStatus,
    modelDashboardMonitoringSchedule_monitoringType,

    -- * ModelDataQuality
    ModelDataQuality (..),
    newModelDataQuality,
    modelDataQuality_constraints,
    modelDataQuality_statistics,

    -- * ModelDeployConfig
    ModelDeployConfig (..),
    newModelDeployConfig,
    modelDeployConfig_autoGenerateEndpointName,
    modelDeployConfig_endpointName,

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
    modelExplainabilityJobInput_batchTransformInput,
    modelExplainabilityJobInput_endpointInput,

    -- * ModelInfrastructureConfig
    ModelInfrastructureConfig (..),
    newModelInfrastructureConfig,
    modelInfrastructureConfig_infrastructureType,
    modelInfrastructureConfig_realTimeInferenceConfig,

    -- * ModelInput
    ModelInput (..),
    newModelInput,
    modelInput_dataInputConfig,

    -- * ModelLatencyThreshold
    ModelLatencyThreshold (..),
    newModelLatencyThreshold,
    modelLatencyThreshold_percentile,
    modelLatencyThreshold_valueInMilliseconds,

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
    modelMetrics_bias,
    modelMetrics_explainability,
    modelMetrics_modelDataQuality,
    modelMetrics_modelQuality,

    -- * ModelPackage
    ModelPackage (..),
    newModelPackage,
    modelPackage_additionalInferenceSpecifications,
    modelPackage_approvalDescription,
    modelPackage_certifyForMarketplace,
    modelPackage_createdBy,
    modelPackage_creationTime,
    modelPackage_customerMetadataProperties,
    modelPackage_domain,
    modelPackage_driftCheckBaselines,
    modelPackage_inferenceSpecification,
    modelPackage_lastModifiedBy,
    modelPackage_lastModifiedTime,
    modelPackage_metadataProperties,
    modelPackage_modelApprovalStatus,
    modelPackage_modelMetrics,
    modelPackage_modelPackageArn,
    modelPackage_modelPackageDescription,
    modelPackage_modelPackageGroupName,
    modelPackage_modelPackageName,
    modelPackage_modelPackageStatus,
    modelPackage_modelPackageStatusDetails,
    modelPackage_modelPackageVersion,
    modelPackage_samplePayloadUrl,
    modelPackage_sourceAlgorithmSpecification,
    modelPackage_tags,
    modelPackage_task,
    modelPackage_validationSpecification,

    -- * ModelPackageContainerDefinition
    ModelPackageContainerDefinition (..),
    newModelPackageContainerDefinition,
    modelPackageContainerDefinition_containerHostname,
    modelPackageContainerDefinition_environment,
    modelPackageContainerDefinition_framework,
    modelPackageContainerDefinition_frameworkVersion,
    modelPackageContainerDefinition_imageDigest,
    modelPackageContainerDefinition_modelDataUrl,
    modelPackageContainerDefinition_modelInput,
    modelPackageContainerDefinition_nearestModelName,
    modelPackageContainerDefinition_productId,
    modelPackageContainerDefinition_image,

    -- * ModelPackageGroup
    ModelPackageGroup (..),
    newModelPackageGroup,
    modelPackageGroup_createdBy,
    modelPackageGroup_creationTime,
    modelPackageGroup_modelPackageGroupArn,
    modelPackageGroup_modelPackageGroupDescription,
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
    modelPackageSummary_modelPackageGroupName,
    modelPackageSummary_modelPackageVersion,
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
    modelQualityAppSpecification_containerEntrypoint,
    modelQualityAppSpecification_environment,
    modelQualityAppSpecification_postAnalyticsProcessorSourceUri,
    modelQualityAppSpecification_problemType,
    modelQualityAppSpecification_recordPreprocessorSourceUri,
    modelQualityAppSpecification_imageUri,

    -- * ModelQualityBaselineConfig
    ModelQualityBaselineConfig (..),
    newModelQualityBaselineConfig,
    modelQualityBaselineConfig_baseliningJobName,
    modelQualityBaselineConfig_constraintsResource,

    -- * ModelQualityJobInput
    ModelQualityJobInput (..),
    newModelQualityJobInput,
    modelQualityJobInput_batchTransformInput,
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

    -- * ModelVariantConfig
    ModelVariantConfig (..),
    newModelVariantConfig,
    modelVariantConfig_modelName,
    modelVariantConfig_variantName,
    modelVariantConfig_infrastructureConfig,

    -- * ModelVariantConfigSummary
    ModelVariantConfigSummary (..),
    newModelVariantConfigSummary,
    modelVariantConfigSummary_modelName,
    modelVariantConfigSummary_variantName,
    modelVariantConfigSummary_infrastructureConfig,
    modelVariantConfigSummary_status,

    -- * MonitoringAlertActions
    MonitoringAlertActions (..),
    newMonitoringAlertActions,
    monitoringAlertActions_modelDashboardIndicator,

    -- * MonitoringAlertHistorySummary
    MonitoringAlertHistorySummary (..),
    newMonitoringAlertHistorySummary,
    monitoringAlertHistorySummary_monitoringScheduleName,
    monitoringAlertHistorySummary_monitoringAlertName,
    monitoringAlertHistorySummary_creationTime,
    monitoringAlertHistorySummary_alertStatus,

    -- * MonitoringAlertSummary
    MonitoringAlertSummary (..),
    newMonitoringAlertSummary,
    monitoringAlertSummary_monitoringAlertName,
    monitoringAlertSummary_creationTime,
    monitoringAlertSummary_lastModifiedTime,
    monitoringAlertSummary_alertStatus,
    monitoringAlertSummary_datapointsToAlert,
    monitoringAlertSummary_evaluationPeriod,
    monitoringAlertSummary_actions,

    -- * MonitoringAppSpecification
    MonitoringAppSpecification (..),
    newMonitoringAppSpecification,
    monitoringAppSpecification_containerArguments,
    monitoringAppSpecification_containerEntrypoint,
    monitoringAppSpecification_postAnalyticsProcessorSourceUri,
    monitoringAppSpecification_recordPreprocessorSourceUri,
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
    monitoringDatasetFormat_csv,
    monitoringDatasetFormat_json,
    monitoringDatasetFormat_parquet,

    -- * MonitoringExecutionSummary
    MonitoringExecutionSummary (..),
    newMonitoringExecutionSummary,
    monitoringExecutionSummary_endpointName,
    monitoringExecutionSummary_failureReason,
    monitoringExecutionSummary_monitoringJobDefinitionName,
    monitoringExecutionSummary_monitoringType,
    monitoringExecutionSummary_processingJobArn,
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
    monitoringInput_batchTransformInput,
    monitoringInput_endpointInput,

    -- * MonitoringJobDefinition
    MonitoringJobDefinition (..),
    newMonitoringJobDefinition,
    monitoringJobDefinition_baselineConfig,
    monitoringJobDefinition_environment,
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
    monitoringNetworkConfig_enableInterContainerTrafficEncryption,
    monitoringNetworkConfig_enableNetworkIsolation,
    monitoringNetworkConfig_vpcConfig,

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
    monitoringSchedule_creationTime,
    monitoringSchedule_endpointName,
    monitoringSchedule_failureReason,
    monitoringSchedule_lastModifiedTime,
    monitoringSchedule_lastMonitoringExecutionSummary,
    monitoringSchedule_monitoringScheduleArn,
    monitoringSchedule_monitoringScheduleConfig,
    monitoringSchedule_monitoringScheduleName,
    monitoringSchedule_monitoringScheduleStatus,
    monitoringSchedule_monitoringType,
    monitoringSchedule_tags,

    -- * MonitoringScheduleConfig
    MonitoringScheduleConfig (..),
    newMonitoringScheduleConfig,
    monitoringScheduleConfig_monitoringJobDefinition,
    monitoringScheduleConfig_monitoringJobDefinitionName,
    monitoringScheduleConfig_monitoringType,
    monitoringScheduleConfig_scheduleConfig,

    -- * MonitoringScheduleSummary
    MonitoringScheduleSummary (..),
    newMonitoringScheduleSummary,
    monitoringScheduleSummary_endpointName,
    monitoringScheduleSummary_monitoringJobDefinitionName,
    monitoringScheduleSummary_monitoringType,
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
    networkConfig_enableInterContainerTrafficEncryption,
    networkConfig_enableNetworkIsolation,
    networkConfig_vpcConfig,

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
    notebookInstanceSummary_additionalCodeRepositories,
    notebookInstanceSummary_creationTime,
    notebookInstanceSummary_defaultCodeRepository,
    notebookInstanceSummary_instanceType,
    notebookInstanceSummary_lastModifiedTime,
    notebookInstanceSummary_notebookInstanceLifecycleConfigName,
    notebookInstanceSummary_notebookInstanceStatus,
    notebookInstanceSummary_url,
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
    objectiveStatusCounters_pending,
    objectiveStatusCounters_succeeded,

    -- * OfflineStoreConfig
    OfflineStoreConfig (..),
    newOfflineStoreConfig,
    offlineStoreConfig_dataCatalogConfig,
    offlineStoreConfig_disableGlueTableCreation,
    offlineStoreConfig_tableFormat,
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
    oidcConfigForResponse_authorizationEndpoint,
    oidcConfigForResponse_clientId,
    oidcConfigForResponse_issuer,
    oidcConfigForResponse_jwksUri,
    oidcConfigForResponse_logoutEndpoint,
    oidcConfigForResponse_tokenEndpoint,
    oidcConfigForResponse_userInfoEndpoint,

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
    outputConfig_compilerOptions,
    outputConfig_kmsKeyId,
    outputConfig_targetDevice,
    outputConfig_targetPlatform,
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
    parameterRange_categoricalParameterRangeSpecification,
    parameterRange_continuousParameterRangeSpecification,
    parameterRange_integerParameterRangeSpecification,

    -- * ParameterRanges
    ParameterRanges (..),
    newParameterRanges,
    parameterRanges_categoricalParameterRanges,
    parameterRanges_continuousParameterRanges,
    parameterRanges_integerParameterRanges,

    -- * Parent
    Parent (..),
    newParent,
    parent_experimentName,
    parent_trialName,

    -- * ParentHyperParameterTuningJob
    ParentHyperParameterTuningJob (..),
    newParentHyperParameterTuningJob,
    parentHyperParameterTuningJob_hyperParameterTuningJobName,

    -- * PendingDeploymentSummary
    PendingDeploymentSummary (..),
    newPendingDeploymentSummary,
    pendingDeploymentSummary_productionVariants,
    pendingDeploymentSummary_shadowProductionVariants,
    pendingDeploymentSummary_startTime,
    pendingDeploymentSummary_endpointConfigName,

    -- * PendingProductionVariantSummary
    PendingProductionVariantSummary (..),
    newPendingProductionVariantSummary,
    pendingProductionVariantSummary_acceleratorType,
    pendingProductionVariantSummary_currentInstanceCount,
    pendingProductionVariantSummary_currentServerlessConfig,
    pendingProductionVariantSummary_currentWeight,
    pendingProductionVariantSummary_deployedImages,
    pendingProductionVariantSummary_desiredInstanceCount,
    pendingProductionVariantSummary_desiredServerlessConfig,
    pendingProductionVariantSummary_desiredWeight,
    pendingProductionVariantSummary_instanceType,
    pendingProductionVariantSummary_variantStatus,
    pendingProductionVariantSummary_variantName,

    -- * Phase
    Phase (..),
    newPhase,
    phase_durationInSeconds,
    phase_initialNumberOfUsers,
    phase_spawnRate,

    -- * Pipeline
    Pipeline (..),
    newPipeline,
    pipeline_createdBy,
    pipeline_creationTime,
    pipeline_lastModifiedBy,
    pipeline_lastModifiedTime,
    pipeline_lastRunTime,
    pipeline_parallelismConfiguration,
    pipeline_pipelineArn,
    pipeline_pipelineDescription,
    pipeline_pipelineDisplayName,
    pipeline_pipelineName,
    pipeline_pipelineStatus,
    pipeline_roleArn,
    pipeline_tags,

    -- * PipelineDefinitionS3Location
    PipelineDefinitionS3Location (..),
    newPipelineDefinitionS3Location,
    pipelineDefinitionS3Location_versionId,
    pipelineDefinitionS3Location_bucket,
    pipelineDefinitionS3Location_objectKey,

    -- * PipelineExecution
    PipelineExecution (..),
    newPipelineExecution,
    pipelineExecution_createdBy,
    pipelineExecution_creationTime,
    pipelineExecution_failureReason,
    pipelineExecution_lastModifiedBy,
    pipelineExecution_lastModifiedTime,
    pipelineExecution_parallelismConfiguration,
    pipelineExecution_pipelineArn,
    pipelineExecution_pipelineExecutionArn,
    pipelineExecution_pipelineExecutionDescription,
    pipelineExecution_pipelineExecutionDisplayName,
    pipelineExecution_pipelineExecutionStatus,
    pipelineExecution_pipelineExperimentConfig,
    pipelineExecution_pipelineParameters,

    -- * PipelineExecutionStep
    PipelineExecutionStep (..),
    newPipelineExecutionStep,
    pipelineExecutionStep_attemptCount,
    pipelineExecutionStep_cacheHitResult,
    pipelineExecutionStep_endTime,
    pipelineExecutionStep_failureReason,
    pipelineExecutionStep_metadata,
    pipelineExecutionStep_startTime,
    pipelineExecutionStep_stepDescription,
    pipelineExecutionStep_stepDisplayName,
    pipelineExecutionStep_stepName,
    pipelineExecutionStep_stepStatus,

    -- * PipelineExecutionStepMetadata
    PipelineExecutionStepMetadata (..),
    newPipelineExecutionStepMetadata,
    pipelineExecutionStepMetadata_autoMLJob,
    pipelineExecutionStepMetadata_callback,
    pipelineExecutionStepMetadata_clarifyCheck,
    pipelineExecutionStepMetadata_condition,
    pipelineExecutionStepMetadata_emr,
    pipelineExecutionStepMetadata_fail,
    pipelineExecutionStepMetadata_lambda,
    pipelineExecutionStepMetadata_model,
    pipelineExecutionStepMetadata_processingJob,
    pipelineExecutionStepMetadata_qualityCheck,
    pipelineExecutionStepMetadata_registerModel,
    pipelineExecutionStepMetadata_trainingJob,
    pipelineExecutionStepMetadata_transformJob,
    pipelineExecutionStepMetadata_tuningJob,

    -- * PipelineExecutionSummary
    PipelineExecutionSummary (..),
    newPipelineExecutionSummary,
    pipelineExecutionSummary_pipelineExecutionArn,
    pipelineExecutionSummary_pipelineExecutionDescription,
    pipelineExecutionSummary_pipelineExecutionDisplayName,
    pipelineExecutionSummary_pipelineExecutionFailureReason,
    pipelineExecutionSummary_pipelineExecutionStatus,
    pipelineExecutionSummary_startTime,

    -- * PipelineExperimentConfig
    PipelineExperimentConfig (..),
    newPipelineExperimentConfig,
    pipelineExperimentConfig_experimentName,
    pipelineExperimentConfig_trialName,

    -- * PipelineSummary
    PipelineSummary (..),
    newPipelineSummary,
    pipelineSummary_creationTime,
    pipelineSummary_lastExecutionTime,
    pipelineSummary_lastModifiedTime,
    pipelineSummary_pipelineArn,
    pipelineSummary_pipelineDescription,
    pipelineSummary_pipelineDisplayName,
    pipelineSummary_pipelineName,
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
    processingInput_appManaged,
    processingInput_datasetDefinition,
    processingInput_s3Input,
    processingInput_inputName,

    -- * ProcessingJob
    ProcessingJob (..),
    newProcessingJob,
    processingJob_appSpecification,
    processingJob_autoMLJobArn,
    processingJob_creationTime,
    processingJob_environment,
    processingJob_exitMessage,
    processingJob_experimentConfig,
    processingJob_failureReason,
    processingJob_lastModifiedTime,
    processingJob_monitoringScheduleArn,
    processingJob_networkConfig,
    processingJob_processingEndTime,
    processingJob_processingInputs,
    processingJob_processingJobArn,
    processingJob_processingJobName,
    processingJob_processingJobStatus,
    processingJob_processingOutputConfig,
    processingJob_processingResources,
    processingJob_processingStartTime,
    processingJob_roleArn,
    processingJob_stoppingCondition,
    processingJob_tags,
    processingJob_trainingJobArn,

    -- * ProcessingJobStepMetadata
    ProcessingJobStepMetadata (..),
    newProcessingJobStepMetadata,
    processingJobStepMetadata_arn,

    -- * ProcessingJobSummary
    ProcessingJobSummary (..),
    newProcessingJobSummary,
    processingJobSummary_exitMessage,
    processingJobSummary_failureReason,
    processingJobSummary_lastModifiedTime,
    processingJobSummary_processingEndTime,
    processingJobSummary_processingJobName,
    processingJobSummary_processingJobArn,
    processingJobSummary_creationTime,
    processingJobSummary_processingJobStatus,

    -- * ProcessingOutput
    ProcessingOutput (..),
    newProcessingOutput,
    processingOutput_appManaged,
    processingOutput_featureStoreOutput,
    processingOutput_s3Output,
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
    processingS3Input_localPath,
    processingS3Input_s3CompressionType,
    processingS3Input_s3DataDistributionType,
    processingS3Input_s3InputMode,
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
    productionVariant_coreDumpConfig,
    productionVariant_initialInstanceCount,
    productionVariant_initialVariantWeight,
    productionVariant_instanceType,
    productionVariant_modelDataDownloadTimeoutInSeconds,
    productionVariant_serverlessConfig,
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
    productionVariantStatus_startTime,
    productionVariantStatus_statusMessage,
    productionVariantStatus_status,

    -- * ProductionVariantSummary
    ProductionVariantSummary (..),
    newProductionVariantSummary,
    productionVariantSummary_currentInstanceCount,
    productionVariantSummary_currentServerlessConfig,
    productionVariantSummary_currentWeight,
    productionVariantSummary_deployedImages,
    productionVariantSummary_desiredInstanceCount,
    productionVariantSummary_desiredServerlessConfig,
    productionVariantSummary_desiredWeight,
    productionVariantSummary_variantStatus,
    productionVariantSummary_variantName,

    -- * ProfilerConfig
    ProfilerConfig (..),
    newProfilerConfig,
    profilerConfig_disableProfiler,
    profilerConfig_profilingIntervalInMilliseconds,
    profilerConfig_profilingParameters,
    profilerConfig_s3OutputPath,

    -- * ProfilerConfigForUpdate
    ProfilerConfigForUpdate (..),
    newProfilerConfigForUpdate,
    profilerConfigForUpdate_disableProfiler,
    profilerConfigForUpdate_profilingIntervalInMilliseconds,
    profilerConfigForUpdate_profilingParameters,
    profilerConfigForUpdate_s3OutputPath,

    -- * ProfilerRuleConfiguration
    ProfilerRuleConfiguration (..),
    newProfilerRuleConfiguration,
    profilerRuleConfiguration_instanceType,
    profilerRuleConfiguration_localPath,
    profilerRuleConfiguration_ruleParameters,
    profilerRuleConfiguration_s3OutputPath,
    profilerRuleConfiguration_volumeSizeInGB,
    profilerRuleConfiguration_ruleConfigurationName,
    profilerRuleConfiguration_ruleEvaluatorImage,

    -- * ProfilerRuleEvaluationStatus
    ProfilerRuleEvaluationStatus (..),
    newProfilerRuleEvaluationStatus,
    profilerRuleEvaluationStatus_lastModifiedTime,
    profilerRuleEvaluationStatus_ruleConfigurationName,
    profilerRuleEvaluationStatus_ruleEvaluationJobArn,
    profilerRuleEvaluationStatus_ruleEvaluationStatus,
    profilerRuleEvaluationStatus_statusDetails,

    -- * Project
    Project (..),
    newProject,
    project_createdBy,
    project_creationTime,
    project_lastModifiedBy,
    project_lastModifiedTime,
    project_projectArn,
    project_projectDescription,
    project_projectId,
    project_projectName,
    project_projectStatus,
    project_serviceCatalogProvisionedProductDetails,
    project_serviceCatalogProvisioningDetails,
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
    provisioningParameter_key,
    provisioningParameter_value,

    -- * PublicWorkforceTaskPrice
    PublicWorkforceTaskPrice (..),
    newPublicWorkforceTaskPrice,
    publicWorkforceTaskPrice_amountInUsd,

    -- * QualityCheckStepMetadata
    QualityCheckStepMetadata (..),
    newQualityCheckStepMetadata,
    qualityCheckStepMetadata_baselineUsedForDriftCheckConstraints,
    qualityCheckStepMetadata_baselineUsedForDriftCheckStatistics,
    qualityCheckStepMetadata_calculatedBaselineConstraints,
    qualityCheckStepMetadata_calculatedBaselineStatistics,
    qualityCheckStepMetadata_checkJobArn,
    qualityCheckStepMetadata_checkType,
    qualityCheckStepMetadata_modelPackageGroupName,
    qualityCheckStepMetadata_registerNewBaseline,
    qualityCheckStepMetadata_skipCheck,
    qualityCheckStepMetadata_violationReport,

    -- * QueryFilters
    QueryFilters (..),
    newQueryFilters,
    queryFilters_createdAfter,
    queryFilters_createdBefore,
    queryFilters_lineageTypes,
    queryFilters_modifiedAfter,
    queryFilters_modifiedBefore,
    queryFilters_properties,
    queryFilters_types,

    -- * RSessionAppSettings
    RSessionAppSettings (..),
    newRSessionAppSettings,
    rSessionAppSettings_customImages,
    rSessionAppSettings_defaultResourceSpec,

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
    rStudioServerProDomainSettingsForUpdate_rStudioConnectUrl,
    rStudioServerProDomainSettingsForUpdate_rStudioPackageManagerUrl,
    rStudioServerProDomainSettingsForUpdate_domainExecutionRoleArn,

    -- * RealTimeInferenceConfig
    RealTimeInferenceConfig (..),
    newRealTimeInferenceConfig,
    realTimeInferenceConfig_instanceType,
    realTimeInferenceConfig_instanceCount,

    -- * RecommendationJobCompiledOutputConfig
    RecommendationJobCompiledOutputConfig (..),
    newRecommendationJobCompiledOutputConfig,
    recommendationJobCompiledOutputConfig_s3OutputUri,

    -- * RecommendationJobContainerConfig
    RecommendationJobContainerConfig (..),
    newRecommendationJobContainerConfig,
    recommendationJobContainerConfig_domain,
    recommendationJobContainerConfig_framework,
    recommendationJobContainerConfig_frameworkVersion,
    recommendationJobContainerConfig_nearestModelName,
    recommendationJobContainerConfig_payloadConfig,
    recommendationJobContainerConfig_supportedInstanceTypes,
    recommendationJobContainerConfig_task,

    -- * RecommendationJobInferenceBenchmark
    RecommendationJobInferenceBenchmark (..),
    newRecommendationJobInferenceBenchmark,
    recommendationJobInferenceBenchmark_endpointConfiguration,
    recommendationJobInferenceBenchmark_failureReason,
    recommendationJobInferenceBenchmark_metrics,
    recommendationJobInferenceBenchmark_modelConfiguration,

    -- * RecommendationJobInputConfig
    RecommendationJobInputConfig (..),
    newRecommendationJobInputConfig,
    recommendationJobInputConfig_containerConfig,
    recommendationJobInputConfig_endpointConfigurations,
    recommendationJobInputConfig_endpoints,
    recommendationJobInputConfig_jobDurationInSeconds,
    recommendationJobInputConfig_resourceLimit,
    recommendationJobInputConfig_trafficPattern,
    recommendationJobInputConfig_volumeKmsKeyId,
    recommendationJobInputConfig_vpcConfig,
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

    -- * RecommendationJobVpcConfig
    RecommendationJobVpcConfig (..),
    newRecommendationJobVpcConfig,
    recommendationJobVpcConfig_securityGroupIds,
    recommendationJobVpcConfig_subnets,

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
    resolvedAttributes_autoMLJobObjective,
    resolvedAttributes_completionCriteria,
    resolvedAttributes_problemType,

    -- * ResourceConfig
    ResourceConfig (..),
    newResourceConfig,
    resourceConfig_instanceCount,
    resourceConfig_instanceGroups,
    resourceConfig_instanceType,
    resourceConfig_keepAlivePeriodInSeconds,
    resourceConfig_volumeKmsKeyId,
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
    resourceSpec_instanceType,
    resourceSpec_lifecycleConfigArn,
    resourceSpec_sageMakerImageArn,
    resourceSpec_sageMakerImageVersionArn,

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
    s3DataSource_instanceGroupNames,
    s3DataSource_s3DataDistributionType,
    s3DataSource_s3DataType,
    s3DataSource_s3Uri,

    -- * S3StorageConfig
    S3StorageConfig (..),
    newS3StorageConfig,
    s3StorageConfig_kmsKeyId,
    s3StorageConfig_resolvedOutputS3Uri,
    s3StorageConfig_s3Uri,

    -- * ScheduleConfig
    ScheduleConfig (..),
    newScheduleConfig,
    scheduleConfig_scheduleExpression,

    -- * SearchExpression
    SearchExpression (..),
    newSearchExpression,
    searchExpression_filters,
    searchExpression_nestedFilters,
    searchExpression_operator,
    searchExpression_subExpressions,

    -- * SearchRecord
    SearchRecord (..),
    newSearchRecord,
    searchRecord_endpoint,
    searchRecord_experiment,
    searchRecord_featureGroup,
    searchRecord_featureMetadata,
    searchRecord_hyperParameterTuningJob,
    searchRecord_model,
    searchRecord_modelCard,
    searchRecord_modelPackage,
    searchRecord_modelPackageGroup,
    searchRecord_pipeline,
    searchRecord_pipelineExecution,
    searchRecord_project,
    searchRecord_trainingJob,
    searchRecord_trial,
    searchRecord_trialComponent,

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
    serviceCatalogProvisionedProductDetails_provisionedProductId,
    serviceCatalogProvisionedProductDetails_provisionedProductStatusMessage,

    -- * ServiceCatalogProvisioningDetails
    ServiceCatalogProvisioningDetails (..),
    newServiceCatalogProvisioningDetails,
    serviceCatalogProvisioningDetails_pathId,
    serviceCatalogProvisioningDetails_provisioningArtifactId,
    serviceCatalogProvisioningDetails_provisioningParameters,
    serviceCatalogProvisioningDetails_productId,

    -- * ServiceCatalogProvisioningUpdateDetails
    ServiceCatalogProvisioningUpdateDetails (..),
    newServiceCatalogProvisioningUpdateDetails,
    serviceCatalogProvisioningUpdateDetails_provisioningArtifactId,
    serviceCatalogProvisioningUpdateDetails_provisioningParameters,

    -- * ShadowModeConfig
    ShadowModeConfig (..),
    newShadowModeConfig,
    shadowModeConfig_sourceModelVariantName,
    shadowModeConfig_shadowModelVariants,

    -- * ShadowModelVariantConfig
    ShadowModelVariantConfig (..),
    newShadowModelVariantConfig,
    shadowModelVariantConfig_shadowModelVariantName,
    shadowModelVariantConfig_samplingPercentage,

    -- * SharingSettings
    SharingSettings (..),
    newSharingSettings,
    sharingSettings_notebookOutputOption,
    sharingSettings_s3KmsKeyId,
    sharingSettings_s3OutputPath,

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

    -- * SpaceDetails
    SpaceDetails (..),
    newSpaceDetails,
    spaceDetails_creationTime,
    spaceDetails_domainId,
    spaceDetails_lastModifiedTime,
    spaceDetails_spaceName,
    spaceDetails_status,

    -- * SpaceSettings
    SpaceSettings (..),
    newSpaceSettings,
    spaceSettings_jupyterServerAppSettings,
    spaceSettings_kernelGatewayAppSettings,

    -- * StoppingCondition
    StoppingCondition (..),
    newStoppingCondition,
    stoppingCondition_maxRuntimeInSeconds,
    stoppingCondition_maxWaitTimeInSeconds,

    -- * StudioLifecycleConfigDetails
    StudioLifecycleConfigDetails (..),
    newStudioLifecycleConfigDetails,
    studioLifecycleConfigDetails_creationTime,
    studioLifecycleConfigDetails_lastModifiedTime,
    studioLifecycleConfigDetails_studioLifecycleConfigAppType,
    studioLifecycleConfigDetails_studioLifecycleConfigArn,
    studioLifecycleConfigDetails_studioLifecycleConfigName,

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
    timeSeriesForecastingSettings_amazonForecastRoleArn,
    timeSeriesForecastingSettings_status,

    -- * TrafficPattern
    TrafficPattern (..),
    newTrafficPattern,
    trafficPattern_phases,
    trafficPattern_trafficType,

    -- * TrafficRoutingConfig
    TrafficRoutingConfig (..),
    newTrafficRoutingConfig,
    trafficRoutingConfig_canarySize,
    trafficRoutingConfig_linearStepSize,
    trafficRoutingConfig_type,
    trafficRoutingConfig_waitIntervalInSeconds,

    -- * TrainingJob
    TrainingJob (..),
    newTrainingJob,
    trainingJob_algorithmSpecification,
    trainingJob_autoMLJobArn,
    trainingJob_billableTimeInSeconds,
    trainingJob_checkpointConfig,
    trainingJob_creationTime,
    trainingJob_debugHookConfig,
    trainingJob_debugRuleConfigurations,
    trainingJob_debugRuleEvaluationStatuses,
    trainingJob_enableInterContainerTrafficEncryption,
    trainingJob_enableManagedSpotTraining,
    trainingJob_enableNetworkIsolation,
    trainingJob_environment,
    trainingJob_experimentConfig,
    trainingJob_failureReason,
    trainingJob_finalMetricDataList,
    trainingJob_hyperParameters,
    trainingJob_inputDataConfig,
    trainingJob_labelingJobArn,
    trainingJob_lastModifiedTime,
    trainingJob_modelArtifacts,
    trainingJob_outputDataConfig,
    trainingJob_resourceConfig,
    trainingJob_retryStrategy,
    trainingJob_roleArn,
    trainingJob_secondaryStatus,
    trainingJob_secondaryStatusTransitions,
    trainingJob_stoppingCondition,
    trainingJob_tags,
    trainingJob_tensorBoardOutputConfig,
    trainingJob_trainingEndTime,
    trainingJob_trainingJobArn,
    trainingJob_trainingJobName,
    trainingJob_trainingJobStatus,
    trainingJob_trainingStartTime,
    trainingJob_trainingTimeInSeconds,
    trainingJob_tuningJobArn,
    trainingJob_vpcConfig,

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
    trainingJobStatusCounters_completed,
    trainingJobStatusCounters_inProgress,
    trainingJobStatusCounters_nonRetryableError,
    trainingJobStatusCounters_retryableError,
    trainingJobStatusCounters_stopped,

    -- * TrainingJobStepMetadata
    TrainingJobStepMetadata (..),
    newTrainingJobStepMetadata,
    trainingJobStepMetadata_arn,

    -- * TrainingJobSummary
    TrainingJobSummary (..),
    newTrainingJobSummary,
    trainingJobSummary_lastModifiedTime,
    trainingJobSummary_trainingEndTime,
    trainingJobSummary_warmPoolStatus,
    trainingJobSummary_trainingJobName,
    trainingJobSummary_trainingJobArn,
    trainingJobSummary_creationTime,
    trainingJobSummary_trainingJobStatus,

    -- * TrainingSpecification
    TrainingSpecification (..),
    newTrainingSpecification,
    trainingSpecification_metricDefinitions,
    trainingSpecification_supportedHyperParameters,
    trainingSpecification_supportedTuningJobObjectiveMetrics,
    trainingSpecification_supportsDistributedTraining,
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
    transformInput_compressionType,
    transformInput_contentType,
    transformInput_splitType,
    transformInput_dataSource,

    -- * TransformJob
    TransformJob (..),
    newTransformJob,
    transformJob_autoMLJobArn,
    transformJob_batchStrategy,
    transformJob_creationTime,
    transformJob_dataProcessing,
    transformJob_environment,
    transformJob_experimentConfig,
    transformJob_failureReason,
    transformJob_labelingJobArn,
    transformJob_maxConcurrentTransforms,
    transformJob_maxPayloadInMB,
    transformJob_modelClientConfig,
    transformJob_modelName,
    transformJob_tags,
    transformJob_transformEndTime,
    transformJob_transformInput,
    transformJob_transformJobArn,
    transformJob_transformJobName,
    transformJob_transformJobStatus,
    transformJob_transformOutput,
    transformJob_transformResources,
    transformJob_transformStartTime,

    -- * TransformJobDefinition
    TransformJobDefinition (..),
    newTransformJobDefinition,
    transformJobDefinition_batchStrategy,
    transformJobDefinition_environment,
    transformJobDefinition_maxConcurrentTransforms,
    transformJobDefinition_maxPayloadInMB,
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
    transformOutput_accept,
    transformOutput_assembleWith,
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
    trial_createdBy,
    trial_creationTime,
    trial_displayName,
    trial_experimentName,
    trial_lastModifiedBy,
    trial_lastModifiedTime,
    trial_metadataProperties,
    trial_source,
    trial_tags,
    trial_trialArn,
    trial_trialComponentSummaries,
    trial_trialName,

    -- * TrialComponent
    TrialComponent (..),
    newTrialComponent,
    trialComponent_createdBy,
    trialComponent_creationTime,
    trialComponent_displayName,
    trialComponent_endTime,
    trialComponent_inputArtifacts,
    trialComponent_lastModifiedBy,
    trialComponent_lastModifiedTime,
    trialComponent_lineageGroupArn,
    trialComponent_metadataProperties,
    trialComponent_metrics,
    trialComponent_outputArtifacts,
    trialComponent_parameters,
    trialComponent_parents,
    trialComponent_runName,
    trialComponent_source,
    trialComponent_sourceDetail,
    trialComponent_startTime,
    trialComponent_status,
    trialComponent_tags,
    trialComponent_trialComponentArn,
    trialComponent_trialComponentName,

    -- * TrialComponentArtifact
    TrialComponentArtifact (..),
    newTrialComponentArtifact,
    trialComponentArtifact_mediaType,
    trialComponentArtifact_value,

    -- * TrialComponentMetricSummary
    TrialComponentMetricSummary (..),
    newTrialComponentMetricSummary,
    trialComponentMetricSummary_avg,
    trialComponentMetricSummary_count,
    trialComponentMetricSummary_last,
    trialComponentMetricSummary_max,
    trialComponentMetricSummary_metricName,
    trialComponentMetricSummary_min,
    trialComponentMetricSummary_sourceArn,
    trialComponentMetricSummary_stdDev,
    trialComponentMetricSummary_timeStamp,

    -- * TrialComponentParameterValue
    TrialComponentParameterValue (..),
    newTrialComponentParameterValue,
    trialComponentParameterValue_numberValue,
    trialComponentParameterValue_stringValue,

    -- * TrialComponentSimpleSummary
    TrialComponentSimpleSummary (..),
    newTrialComponentSimpleSummary,
    trialComponentSimpleSummary_createdBy,
    trialComponentSimpleSummary_creationTime,
    trialComponentSimpleSummary_trialComponentArn,
    trialComponentSimpleSummary_trialComponentName,
    trialComponentSimpleSummary_trialComponentSource,

    -- * TrialComponentSource
    TrialComponentSource (..),
    newTrialComponentSource,
    trialComponentSource_sourceType,
    trialComponentSource_sourceArn,

    -- * TrialComponentSourceDetail
    TrialComponentSourceDetail (..),
    newTrialComponentSourceDetail,
    trialComponentSourceDetail_processingJob,
    trialComponentSourceDetail_sourceArn,
    trialComponentSourceDetail_trainingJob,
    trialComponentSourceDetail_transformJob,

    -- * TrialComponentStatus
    TrialComponentStatus (..),
    newTrialComponentStatus,
    trialComponentStatus_message,
    trialComponentStatus_primaryStatus,

    -- * TrialComponentSummary
    TrialComponentSummary (..),
    newTrialComponentSummary,
    trialComponentSummary_createdBy,
    trialComponentSummary_creationTime,
    trialComponentSummary_displayName,
    trialComponentSummary_endTime,
    trialComponentSummary_lastModifiedBy,
    trialComponentSummary_lastModifiedTime,
    trialComponentSummary_startTime,
    trialComponentSummary_status,
    trialComponentSummary_trialComponentArn,
    trialComponentSummary_trialComponentName,
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
    trialSummary_displayName,
    trialSummary_lastModifiedTime,
    trialSummary_trialArn,
    trialSummary_trialName,
    trialSummary_trialSource,

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
    uiConfig_humanTaskUiArn,
    uiConfig_uiTemplateS3Uri,

    -- * UiTemplate
    UiTemplate (..),
    newUiTemplate,
    uiTemplate_content,

    -- * UiTemplateInfo
    UiTemplateInfo (..),
    newUiTemplateInfo,
    uiTemplateInfo_contentSha256,
    uiTemplateInfo_url,

    -- * UserContext
    UserContext (..),
    newUserContext,
    userContext_domainId,
    userContext_userProfileArn,
    userContext_userProfileName,

    -- * UserProfileDetails
    UserProfileDetails (..),
    newUserProfileDetails,
    userProfileDetails_creationTime,
    userProfileDetails_domainId,
    userProfileDetails_lastModifiedTime,
    userProfileDetails_status,
    userProfileDetails_userProfileName,

    -- * UserSettings
    UserSettings (..),
    newUserSettings,
    userSettings_canvasAppSettings,
    userSettings_executionRole,
    userSettings_jupyterServerAppSettings,
    userSettings_kernelGatewayAppSettings,
    userSettings_rSessionAppSettings,
    userSettings_rStudioServerProAppSettings,
    userSettings_securityGroups,
    userSettings_sharingSettings,
    userSettings_tensorBoardAppSettings,

    -- * VariantProperty
    VariantProperty (..),
    newVariantProperty,
    variantProperty_variantPropertyType,

    -- * Vertex
    Vertex (..),
    newVertex,
    vertex_arn,
    vertex_lineageType,
    vertex_type,

    -- * VpcConfig
    VpcConfig (..),
    newVpcConfig,
    vpcConfig_securityGroupIds,
    vpcConfig_subnets,

    -- * WarmPoolStatus
    WarmPoolStatus (..),
    newWarmPoolStatus,
    warmPoolStatus_resourceRetainedBillableTimeInSeconds,
    warmPoolStatus_reusedByJob,
    warmPoolStatus_status,

    -- * Workforce
    Workforce (..),
    newWorkforce,
    workforce_cognitoConfig,
    workforce_createDate,
    workforce_failureReason,
    workforce_lastUpdatedDate,
    workforce_oidcConfig,
    workforce_sourceIpConfig,
    workforce_status,
    workforce_subDomain,
    workforce_workforceVpcConfig,
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
    workteam_createDate,
    workteam_lastUpdatedDate,
    workteam_notificationConfiguration,
    workteam_productListingIds,
    workteam_subDomain,
    workteam_workforceArn,
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
import Amazonka.SageMaker.Types.AutoMLJobStepMetadata
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
import Amazonka.SageMaker.Types.CodeRepository
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
import Amazonka.SageMaker.Types.DefaultSpaceSettings
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
import Amazonka.SageMaker.Types.EndpointMetadata
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
import Amazonka.SageMaker.Types.HubContentDependency
import Amazonka.SageMaker.Types.HubContentInfo
import Amazonka.SageMaker.Types.HubContentSortBy
import Amazonka.SageMaker.Types.HubContentStatus
import Amazonka.SageMaker.Types.HubContentType
import Amazonka.SageMaker.Types.HubInfo
import Amazonka.SageMaker.Types.HubS3StorageConfig
import Amazonka.SageMaker.Types.HubSortBy
import Amazonka.SageMaker.Types.HubStatus
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
import Amazonka.SageMaker.Types.InferenceExperimentDataStorageConfig
import Amazonka.SageMaker.Types.InferenceExperimentSchedule
import Amazonka.SageMaker.Types.InferenceExperimentStatus
import Amazonka.SageMaker.Types.InferenceExperimentStopDesiredState
import Amazonka.SageMaker.Types.InferenceExperimentSummary
import Amazonka.SageMaker.Types.InferenceExperimentType
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
import Amazonka.SageMaker.Types.JobType
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
import Amazonka.SageMaker.Types.Model
import Amazonka.SageMaker.Types.ModelApprovalStatus
import Amazonka.SageMaker.Types.ModelArtifacts
import Amazonka.SageMaker.Types.ModelBiasAppSpecification
import Amazonka.SageMaker.Types.ModelBiasBaselineConfig
import Amazonka.SageMaker.Types.ModelBiasJobInput
import Amazonka.SageMaker.Types.ModelCacheSetting
import Amazonka.SageMaker.Types.ModelCard
import Amazonka.SageMaker.Types.ModelCardExportArtifacts
import Amazonka.SageMaker.Types.ModelCardExportJobSortBy
import Amazonka.SageMaker.Types.ModelCardExportJobSortOrder
import Amazonka.SageMaker.Types.ModelCardExportJobStatus
import Amazonka.SageMaker.Types.ModelCardExportJobSummary
import Amazonka.SageMaker.Types.ModelCardExportOutputConfig
import Amazonka.SageMaker.Types.ModelCardProcessingStatus
import Amazonka.SageMaker.Types.ModelCardSecurityConfig
import Amazonka.SageMaker.Types.ModelCardSortBy
import Amazonka.SageMaker.Types.ModelCardSortOrder
import Amazonka.SageMaker.Types.ModelCardStatus
import Amazonka.SageMaker.Types.ModelCardSummary
import Amazonka.SageMaker.Types.ModelCardVersionSortBy
import Amazonka.SageMaker.Types.ModelCardVersionSummary
import Amazonka.SageMaker.Types.ModelClientConfig
import Amazonka.SageMaker.Types.ModelConfiguration
import Amazonka.SageMaker.Types.ModelDashboardEndpoint
import Amazonka.SageMaker.Types.ModelDashboardIndicatorAction
import Amazonka.SageMaker.Types.ModelDashboardModel
import Amazonka.SageMaker.Types.ModelDashboardModelCard
import Amazonka.SageMaker.Types.ModelDashboardMonitoringSchedule
import Amazonka.SageMaker.Types.ModelDataQuality
import Amazonka.SageMaker.Types.ModelDeployConfig
import Amazonka.SageMaker.Types.ModelDeployResult
import Amazonka.SageMaker.Types.ModelDigests
import Amazonka.SageMaker.Types.ModelExplainabilityAppSpecification
import Amazonka.SageMaker.Types.ModelExplainabilityBaselineConfig
import Amazonka.SageMaker.Types.ModelExplainabilityJobInput
import Amazonka.SageMaker.Types.ModelInfrastructureConfig
import Amazonka.SageMaker.Types.ModelInfrastructureType
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
import Amazonka.SageMaker.Types.ModelVariantAction
import Amazonka.SageMaker.Types.ModelVariantConfig
import Amazonka.SageMaker.Types.ModelVariantConfigSummary
import Amazonka.SageMaker.Types.ModelVariantStatus
import Amazonka.SageMaker.Types.MonitoringAlertActions
import Amazonka.SageMaker.Types.MonitoringAlertHistorySortKey
import Amazonka.SageMaker.Types.MonitoringAlertHistorySummary
import Amazonka.SageMaker.Types.MonitoringAlertStatus
import Amazonka.SageMaker.Types.MonitoringAlertSummary
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
import Amazonka.SageMaker.Types.Processor
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
import Amazonka.SageMaker.Types.RealTimeInferenceConfig
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
import Amazonka.SageMaker.Types.RecommendationJobVpcConfig
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
import Amazonka.SageMaker.Types.ShadowModeConfig
import Amazonka.SageMaker.Types.ShadowModelVariantConfig
import Amazonka.SageMaker.Types.SharingSettings
import Amazonka.SageMaker.Types.ShuffleConfig
import Amazonka.SageMaker.Types.SortActionsBy
import Amazonka.SageMaker.Types.SortArtifactsBy
import Amazonka.SageMaker.Types.SortAssociationsBy
import Amazonka.SageMaker.Types.SortBy
import Amazonka.SageMaker.Types.SortContextsBy
import Amazonka.SageMaker.Types.SortExperimentsBy
import Amazonka.SageMaker.Types.SortInferenceExperimentsBy
import Amazonka.SageMaker.Types.SortLineageGroupsBy
import Amazonka.SageMaker.Types.SortOrder
import Amazonka.SageMaker.Types.SortPipelineExecutionsBy
import Amazonka.SageMaker.Types.SortPipelinesBy
import Amazonka.SageMaker.Types.SortTrialComponentsBy
import Amazonka.SageMaker.Types.SortTrialsBy
import Amazonka.SageMaker.Types.SourceAlgorithm
import Amazonka.SageMaker.Types.SourceAlgorithmSpecification
import Amazonka.SageMaker.Types.SourceIpConfig
import Amazonka.SageMaker.Types.SpaceDetails
import Amazonka.SageMaker.Types.SpaceSettings
import Amazonka.SageMaker.Types.SpaceSortKey
import Amazonka.SageMaker.Types.SpaceStatus
import Amazonka.SageMaker.Types.SplitType
import Amazonka.SageMaker.Types.StageStatus
import Amazonka.SageMaker.Types.StepStatus
import Amazonka.SageMaker.Types.StoppingCondition
import Amazonka.SageMaker.Types.StudioLifecycleConfigAppType
import Amazonka.SageMaker.Types.StudioLifecycleConfigDetails
import Amazonka.SageMaker.Types.StudioLifecycleConfigSortKey
import Amazonka.SageMaker.Types.SubscribedWorkteam
import Amazonka.SageMaker.Types.SuggestionQuery
import Amazonka.SageMaker.Types.TableFormat
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
import Amazonka.SageMaker.Types.VendorGuidance
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
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | There was a conflict when you attempted to modify a SageMaker entity
-- such as an @Experiment@ or @Artifact@.
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | Resource being accessed is in use.
_ResourceInUse :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceInUse =
  Core._MatchServiceError
    defaultService
    "ResourceInUse"

-- | You have exceeded an SageMaker resource limit. For example, you might
-- have too many training jobs created.
_ResourceLimitExceeded :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceLimitExceeded =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceeded"

-- | Resource being access is not found.
_ResourceNotFound :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFound =
  Core._MatchServiceError
    defaultService
    "ResourceNotFound"

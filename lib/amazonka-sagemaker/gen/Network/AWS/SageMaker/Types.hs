-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _ResourceLimitExceeded
    , _ResourceInUse
    , _ConflictException
    , _ResourceNotFound

    -- * EndpointConfigArn
    , EndpointConfigArn (..)

    -- * DomainStatus
    , DomainStatus (..)

    -- * Parent
    , Parent (..)
    , mkParent
    , pExperimentName
    , pTrialName

    -- * TrainingJobStatusCounters
    , TrainingJobStatusCounters (..)
    , mkTrainingJobStatusCounters
    , tjscCompleted
    , tjscInProgress
    , tjscNonRetryableError
    , tjscRetryableError
    , tjscStopped

    -- * UiTemplate
    , UiTemplate (..)
    , mkUiTemplate
    , utContent

    -- * LabelingJobArn
    , LabelingJobArn (..)

    -- * CodeRepositoryContains
    , CodeRepositoryContains (..)

    -- * MonitoringScheduleSortKey
    , MonitoringScheduleSortKey (..)

    -- * HyperParameterValue
    , HyperParameterValue (..)

    -- * TransformJobName
    , TransformJobName (..)

    -- * CandidateDefinitionNotebookLocation
    , CandidateDefinitionNotebookLocation (..)

    -- * NameContains
    , NameContains (..)

    -- * UserSettings
    , UserSettings (..)
    , mkUserSettings
    , usExecutionRole
    , usJupyterServerAppSettings
    , usKernelGatewayAppSettings
    , usSecurityGroups
    , usSharingSettings
    , usTensorBoardAppSettings

    -- * NotebookInstanceSortOrder
    , NotebookInstanceSortOrder (..)

    -- * ModelPackageSortBy
    , ModelPackageSortBy (..)

    -- * FailureReason
    , FailureReason (..)

    -- * ModelPackageStatusItem
    , ModelPackageStatusItem (..)
    , mkModelPackageStatusItem
    , mpsiName
    , mpsiStatus
    , mpsiFailureReason

    -- * ProcessingOutput
    , ProcessingOutput (..)
    , mkProcessingOutput
    , poOutputName
    , poS3Output

    -- * MonitoringScheduleArn
    , MonitoringScheduleArn (..)

    -- * LabelingJobOutputConfig
    , LabelingJobOutputConfig (..)
    , mkLabelingJobOutputConfig
    , ljocS3OutputPath
    , ljocKmsKeyId
    , ljocSnsTopicArn

    -- * HyperParameterTuningJobStrategyType
    , HyperParameterTuningJobStrategyType (..)

    -- * Workforce
    , Workforce (..)
    , mkWorkforce
    , wWorkforceName
    , wWorkforceArn
    , wCognitoConfig
    , wCreateDate
    , wLastUpdatedDate
    , wOidcConfig
    , wSourceIpConfig
    , wSubDomain

    -- * OidcMemberDefinition
    , OidcMemberDefinition (..)
    , mkOidcMemberDefinition
    , omdGroups

    -- * ResourceLimits
    , ResourceLimits (..)
    , mkResourceLimits
    , rlMaxNumberOfTrainingJobs
    , rlMaxParallelTrainingJobs

    -- * Framework
    , Framework (..)

    -- * ModelClientConfig
    , ModelClientConfig (..)
    , mkModelClientConfig
    , mccInvocationsMaxRetries
    , mccInvocationsTimeoutInSeconds

    -- * HyperParameterTrainingJobSummary
    , HyperParameterTrainingJobSummary (..)
    , mkHyperParameterTrainingJobSummary
    , hptjsTrainingJobName
    , hptjsTrainingJobArn
    , hptjsCreationTime
    , hptjsTrainingJobStatus
    , hptjsTunedHyperParameters
    , hptjsFailureReason
    , hptjsFinalHyperParameterTuningJobObjectiveMetric
    , hptjsObjectiveStatus
    , hptjsTrainingEndTime
    , hptjsTrainingJobDefinitionName
    , hptjsTrainingStartTime
    , hptjsTuningJobName

    -- * AppSpecification
    , AppSpecification (..)
    , mkAppSpecification
    , asImageUri
    , asContainerArguments
    , asContainerEntrypoint

    -- * MediaType
    , MediaType (..)

    -- * EndpointConfigSortKey
    , EndpointConfigSortKey (..)

    -- * ObjectiveStatusCounters
    , ObjectiveStatusCounters (..)
    , mkObjectiveStatusCounters
    , oscFailed
    , oscPending
    , oscSucceeded

    -- * FileSystemAccessMode
    , FileSystemAccessMode (..)

    -- * String256
    , String256 (..)

    -- * LabelingJobDataSource
    , LabelingJobDataSource (..)
    , mkLabelingJobDataSource
    , ljdsS3DataSource
    , ljdsSnsDataSource

    -- * EndpointName
    , EndpointName (..)

    -- * ClientId
    , ClientId (..)

    -- * Workteam
    , Workteam (..)
    , mkWorkteam
    , wfWorkteamName
    , wfMemberDefinitions
    , wfWorkteamArn
    , wfDescription
    , wfCreateDate
    , wfLastUpdatedDate
    , wfNotificationConfiguration
    , wfProductListingIds
    , wfSubDomain
    , wfWorkforceArn

    -- * EntityName
    , EntityName (..)

    -- * AutoMLContainerDefinition
    , AutoMLContainerDefinition (..)
    , mkAutoMLContainerDefinition
    , amlcdImage
    , amlcdModelDataUrl
    , amlcdEnvironment

    -- * CsvContentType
    , CsvContentType (..)

    -- * PaginationToken
    , PaginationToken (..)

    -- * BatchStrategy
    , BatchStrategy (..)

    -- * TrialComponentParameterValue
    , TrialComponentParameterValue (..)
    , mkTrialComponentParameterValue
    , tcpvNumberValue
    , tcpvStringValue

    -- * AlgorithmValidationProfile
    , AlgorithmValidationProfile (..)
    , mkAlgorithmValidationProfile
    , avpProfileName
    , avpTrainingJobDefinition
    , avpTransformJobDefinition

    -- * EndpointSummary
    , EndpointSummary (..)
    , mkEndpointSummary
    , esEndpointName
    , esEndpointArn
    , esCreationTime
    , esLastModifiedTime
    , esEndpointStatus

    -- * Group
    , Group (..)

    -- * ScheduleConfig
    , ScheduleConfig (..)
    , mkScheduleConfig
    , scScheduleExpression

    -- * SourceAlgorithmSpecification
    , SourceAlgorithmSpecification (..)
    , mkSourceAlgorithmSpecification
    , sasSourceAlgorithms

    -- * String200
    , String200 (..)

    -- * TuningJobCompletionCriteria
    , TuningJobCompletionCriteria (..)
    , mkTuningJobCompletionCriteria
    , tjccTargetObjectiveMetricValue

    -- * ClientSecret
    , ClientSecret (..)

    -- * NotebookInstanceName
    , NotebookInstanceName (..)

    -- * TensorBoardAppSettings
    , TensorBoardAppSettings (..)
    , mkTensorBoardAppSettings
    , tbasDefaultResourceSpec

    -- * KernelGatewayAppSettings
    , KernelGatewayAppSettings (..)
    , mkKernelGatewayAppSettings
    , kgasCustomImages
    , kgasDefaultResourceSpec

    -- * AwsManagedHumanLoopRequestSource
    , AwsManagedHumanLoopRequestSource (..)

    -- * TrainingJob
    , TrainingJob (..)
    , mkTrainingJob
    , tjAlgorithmSpecification
    , tjAutoMLJobArn
    , tjBillableTimeInSeconds
    , tjCheckpointConfig
    , tjCreationTime
    , tjDebugHookConfig
    , tjDebugRuleConfigurations
    , tjDebugRuleEvaluationStatuses
    , tjEnableInterContainerTrafficEncryption
    , tjEnableManagedSpotTraining
    , tjEnableNetworkIsolation
    , tjExperimentConfig
    , tjFailureReason
    , tjFinalMetricDataList
    , tjHyperParameters
    , tjInputDataConfig
    , tjLabelingJobArn
    , tjLastModifiedTime
    , tjModelArtifacts
    , tjOutputDataConfig
    , tjResourceConfig
    , tjRoleArn
    , tjSecondaryStatus
    , tjSecondaryStatusTransitions
    , tjStoppingCondition
    , tjTags
    , tjTensorBoardOutputConfig
    , tjTrainingEndTime
    , tjTrainingJobArn
    , tjTrainingJobName
    , tjTrainingJobStatus
    , tjTrainingStartTime
    , tjTrainingTimeInSeconds
    , tjTuningJobArn
    , tjVpcConfig

    -- * CaptureMode
    , CaptureMode (..)

    -- * ImageContainerImage
    , ImageContainerImage (..)

    -- * AutoMLSortBy
    , AutoMLSortBy (..)

    -- * ArnOrName
    , ArnOrName (..)

    -- * CandidateStepType
    , CandidateStepType (..)

    -- * TransformJobSummary
    , TransformJobSummary (..)
    , mkTransformJobSummary
    , tjsTransformJobName
    , tjsTransformJobArn
    , tjsCreationTime
    , tjsTransformJobStatus
    , tjsFailureReason
    , tjsLastModifiedTime
    , tjsTransformEndTime

    -- * LabelingJobAlgorithmsConfig
    , LabelingJobAlgorithmsConfig (..)
    , mkLabelingJobAlgorithmsConfig
    , ljacLabelingJobAlgorithmSpecificationArn
    , ljacInitialActiveLearningModelArn
    , ljacLabelingJobResourceConfig

    -- * ResourceSpec
    , ResourceSpec (..)
    , mkResourceSpec
    , rsInstanceType
    , rsSageMakerImageArn
    , rsSageMakerImageVersionArn

    -- * ResourceId
    , ResourceId (..)

    -- * TrialArn
    , TrialArn (..)

    -- * HumanLoopActivationConditions
    , HumanLoopActivationConditions (..)

    -- * TrialComponentSimpleSummary
    , TrialComponentSimpleSummary (..)
    , mkTrialComponentSimpleSummary
    , tcssCreatedBy
    , tcssCreationTime
    , tcssTrialComponentArn
    , tcssTrialComponentName
    , tcssTrialComponentSource

    -- * Image
    , Image (..)
    , mkImage
    , iCreationTime
    , iImageArn
    , iImageName
    , iImageStatus
    , iLastModifiedTime
    , iDescription
    , iDisplayName
    , iFailureReason

    -- * MonitoringInput
    , MonitoringInput (..)
    , mkMonitoringInput
    , miEndpointInput

    -- * CaptureContentTypeHeader
    , CaptureContentTypeHeader (..)
    , mkCaptureContentTypeHeader
    , ccthCsvContentTypes
    , ccthJsonContentTypes

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * TrialComponentStatusMessage
    , TrialComponentStatusMessage (..)

    -- * DataCaptureConfigSummary
    , DataCaptureConfigSummary (..)
    , mkDataCaptureConfigSummary
    , dccsEnableCapture
    , dccsCaptureStatus
    , dccsCurrentSamplingPercentage
    , dccsDestinationS3Uri
    , dccsKmsKeyId

    -- * PropertyNameQuery
    , PropertyNameQuery (..)
    , mkPropertyNameQuery
    , pnqPropertyNameHint

    -- * FlowDefinitionTaskTitle
    , FlowDefinitionTaskTitle (..)

    -- * CodeRepositoryArn
    , CodeRepositoryArn (..)

    -- * ProcessingResources
    , ProcessingResources (..)
    , mkProcessingResources
    , prClusterConfig

    -- * ImageDisplayName
    , ImageDisplayName (..)

    -- * ModelArn
    , ModelArn (..)

    -- * Trial
    , Trial (..)
    , mkTrial
    , tCreatedBy
    , tCreationTime
    , tDisplayName
    , tExperimentName
    , tLastModifiedBy
    , tLastModifiedTime
    , tSource
    , tTags
    , tTrialArn
    , tTrialComponentSummaries
    , tTrialName

    -- * SnsTopicArn
    , SnsTopicArn (..)

    -- * HyperParameterTuningJobArn
    , HyperParameterTuningJobArn (..)

    -- * ModelArtifacts
    , ModelArtifacts (..)
    , mkModelArtifacts
    , maS3ModelArtifacts

    -- * HyperParameterScalingType
    , HyperParameterScalingType (..)

    -- * USD
    , USD (..)
    , mkUSD
    , usdCents
    , usdDollars
    , usdTenthFractionsOfACent

    -- * CognitoMemberDefinition
    , CognitoMemberDefinition (..)
    , mkCognitoMemberDefinition
    , cmdUserPool
    , cmdUserGroup
    , cmdClientId

    -- * TemplateContentSha256
    , TemplateContentSha256 (..)

    -- * DestinationS3Uri
    , DestinationS3Uri (..)

    -- * ModelSortKey
    , ModelSortKey (..)

    -- * CandidateStepArn
    , CandidateStepArn (..)

    -- * CognitoUserPool
    , CognitoUserPool (..)

    -- * ResourceType
    , ResourceType (..)

    -- * OidcEndpoint
    , OidcEndpoint (..)

    -- * AutoMLSortOrder
    , AutoMLSortOrder (..)

    -- * AlgorithmArn
    , AlgorithmArn (..)

    -- * EnvironmentKey
    , EnvironmentKey (..)

    -- * JobReferenceCodeContains
    , JobReferenceCodeContains (..)

    -- * ClientToken
    , ClientToken (..)

    -- * ParameterValue
    , ParameterValue (..)

    -- * ResourcePropertyName
    , ResourcePropertyName (..)

    -- * LabelingJobSnsDataSource
    , LabelingJobSnsDataSource (..)
    , mkLabelingJobSnsDataSource
    , ljsdsSnsTopicArn

    -- * TargetPlatformOs
    , TargetPlatformOs (..)

    -- * TrainingJobDefinition
    , TrainingJobDefinition (..)
    , mkTrainingJobDefinition
    , tjdTrainingInputMode
    , tjdInputDataConfig
    , tjdOutputDataConfig
    , tjdResourceConfig
    , tjdStoppingCondition
    , tjdHyperParameters

    -- * TrialSourceArn
    , TrialSourceArn (..)

    -- * UiTemplateInfo
    , UiTemplateInfo (..)
    , mkUiTemplateInfo
    , utiContentSha256
    , utiUrl

    -- * MonitoringExecutionSummary
    , MonitoringExecutionSummary (..)
    , mkMonitoringExecutionSummary
    , mesMonitoringScheduleName
    , mesScheduledTime
    , mesCreationTime
    , mesLastModifiedTime
    , mesMonitoringExecutionStatus
    , mesEndpointName
    , mesFailureReason
    , mesProcessingJobArn

    -- * MonitoringScheduleSummary
    , MonitoringScheduleSummary (..)
    , mkMonitoringScheduleSummary
    , mssMonitoringScheduleName
    , mssMonitoringScheduleArn
    , mssCreationTime
    , mssLastModifiedTime
    , mssMonitoringScheduleStatus
    , mssEndpointName

    -- * NotificationConfiguration
    , NotificationConfiguration (..)
    , mkNotificationConfiguration
    , ncNotificationTopicArn

    -- * ShuffleConfig
    , ShuffleConfig (..)
    , mkShuffleConfig
    , scSeed

    -- * ParameterRanges
    , ParameterRanges (..)
    , mkParameterRanges
    , prCategoricalParameterRanges
    , prContinuousParameterRanges
    , prIntegerParameterRanges

    -- * EntityDescription
    , EntityDescription (..)

    -- * HyperParameterAlgorithmSpecification
    , HyperParameterAlgorithmSpecification (..)
    , mkHyperParameterAlgorithmSpecification
    , hpasTrainingInputMode
    , hpasAlgorithmName
    , hpasMetricDefinitions
    , hpasTrainingImage

    -- * DeployedImage
    , DeployedImage (..)
    , mkDeployedImage
    , diResolutionTime
    , diResolvedImage
    , diSpecifiedImage

    -- * HyperParameterTrainingJobDefinition
    , HyperParameterTrainingJobDefinition (..)
    , mkHyperParameterTrainingJobDefinition
    , hptjdAlgorithmSpecification
    , hptjdRoleArn
    , hptjdOutputDataConfig
    , hptjdResourceConfig
    , hptjdStoppingCondition
    , hptjdCheckpointConfig
    , hptjdDefinitionName
    , hptjdEnableInterContainerTrafficEncryption
    , hptjdEnableManagedSpotTraining
    , hptjdEnableNetworkIsolation
    , hptjdHyperParameterRanges
    , hptjdInputDataConfig
    , hptjdStaticHyperParameters
    , hptjdTuningObjective
    , hptjdVpcConfig

    -- * UserProfileName
    , UserProfileName (..)

    -- * TrialComponent
    , TrialComponent (..)
    , mkTrialComponent
    , tcCreatedBy
    , tcCreationTime
    , tcDisplayName
    , tcEndTime
    , tcInputArtifacts
    , tcLastModifiedBy
    , tcLastModifiedTime
    , tcMetrics
    , tcOutputArtifacts
    , tcParameters
    , tcParents
    , tcSource
    , tcSourceDetail
    , tcStartTime
    , tcStatus
    , tcTags
    , tcTrialComponentArn
    , tcTrialComponentName

    -- * TrainingInputMode
    , TrainingInputMode (..)

    -- * ImageConfig
    , ImageConfig (..)
    , mkImageConfig
    , icRepositoryAccessMode

    -- * ModelPackageValidationProfile
    , ModelPackageValidationProfile (..)
    , mkModelPackageValidationProfile
    , mpvpProfileName
    , mpvpTransformJobDefinition

    -- * NotebookInstanceLifecycleConfigContent
    , NotebookInstanceLifecycleConfigContent (..)

    -- * EndpointConfigSummary
    , EndpointConfigSummary (..)
    , mkEndpointConfigSummary
    , ecsEndpointConfigName
    , ecsEndpointConfigArn
    , ecsCreationTime

    -- * AutoMLJobStatus
    , AutoMLJobStatus (..)

    -- * HumanLoopActivationConditionsConfig
    , HumanLoopActivationConditionsConfig (..)
    , mkHumanLoopActivationConditionsConfig
    , hlaccHumanLoopActivationConditions

    -- * TransformResources
    , TransformResources (..)
    , mkTransformResources
    , trInstanceType
    , trInstanceCount
    , trVolumeKmsKeyId

    -- * LabelingJobSummary
    , LabelingJobSummary (..)
    , mkLabelingJobSummary
    , ljsLabelingJobName
    , ljsLabelingJobArn
    , ljsCreationTime
    , ljsLastModifiedTime
    , ljsLabelingJobStatus
    , ljsLabelCounters
    , ljsWorkteamArn
    , ljsPreHumanTaskLambdaArn
    , ljsAnnotationConsolidationLambdaArn
    , ljsFailureReason
    , ljsInputConfig
    , ljsLabelingJobOutput

    -- * TrialComponentPrimaryStatus
    , TrialComponentPrimaryStatus (..)

    -- * ProcessingEnvironmentValue
    , ProcessingEnvironmentValue (..)

    -- * SourceType
    , SourceType (..)

    -- * TrainingSpecification
    , TrainingSpecification (..)
    , mkTrainingSpecification
    , tsTrainingImage
    , tsSupportedTrainingInstanceTypes
    , tsTrainingChannels
    , tsMetricDefinitions
    , tsSupportedHyperParameters
    , tsSupportedTuningJobObjectiveMetrics
    , tsSupportsDistributedTraining
    , tsTrainingImageDigest

    -- * AppStatus
    , AppStatus (..)

    -- * ImageBaseImage
    , ImageBaseImage (..)

    -- * RepositoryAccessMode
    , RepositoryAccessMode (..)

    -- * JsonPath
    , JsonPath (..)

    -- * NotebookOutputOption
    , NotebookOutputOption (..)

    -- * PublicWorkforceTaskPrice
    , PublicWorkforceTaskPrice (..)
    , mkPublicWorkforceTaskPrice
    , pwtpAmountInUsd

    -- * ProcessingS3Input
    , ProcessingS3Input (..)
    , mkProcessingS3Input
    , psiS3Uri
    , psiLocalPath
    , psiS3DataType
    , psiS3InputMode
    , psiS3CompressionType
    , psiS3DataDistributionType

    -- * CategoricalParameterRangeSpecification
    , CategoricalParameterRangeSpecification (..)
    , mkCategoricalParameterRangeSpecification
    , cprsValues

    -- * DomainArn
    , DomainArn (..)

    -- * ImageVersion
    , ImageVersion (..)
    , mkImageVersion
    , ivCreationTime
    , ivImageArn
    , ivImageVersionArn
    , ivImageVersionStatus
    , ivLastModifiedTime
    , ivVersion
    , ivFailureReason

    -- * S3DataSource
    , S3DataSource (..)
    , mkS3DataSource
    , sdsS3DataType
    , sdsS3Uri
    , sdsAttributeNames
    , sdsS3DataDistributionType

    -- * NotebookInstanceLifecycleHook
    , NotebookInstanceLifecycleHook (..)
    , mkNotebookInstanceLifecycleHook
    , nilhContent

    -- * IntegerParameterRangeSpecification
    , IntegerParameterRangeSpecification (..)
    , mkIntegerParameterRangeSpecification
    , iprsMinValue
    , iprsMaxValue

    -- * TransformInstanceType
    , TransformInstanceType (..)

    -- * TrialComponentMetricSummary
    , TrialComponentMetricSummary (..)
    , mkTrialComponentMetricSummary
    , tcmsAvg
    , tcmsCount
    , tcmsLast
    , tcmsMax
    , tcmsMetricName
    , tcmsMin
    , tcmsSourceArn
    , tcmsStdDev
    , tcmsTimeStamp

    -- * MonitoringBaselineConfig
    , MonitoringBaselineConfig (..)
    , mkMonitoringBaselineConfig
    , mbcConstraintsResource
    , mbcStatisticsResource

    -- * ExperimentEntityName
    , ExperimentEntityName (..)

    -- * ModelName
    , ModelName (..)

    -- * UserProfileDetails
    , UserProfileDetails (..)
    , mkUserProfileDetails
    , updCreationTime
    , updDomainId
    , updLastModifiedTime
    , updStatus
    , updUserProfileName

    -- * HyperParameterTuningJobName
    , HyperParameterTuningJobName (..)

    -- * VpcId
    , VpcId (..)

    -- * HyperParameterTuningJobObjective
    , HyperParameterTuningJobObjective (..)
    , mkHyperParameterTuningJobObjective
    , hptjoType
    , hptjoMetricName

    -- * NotebookInstanceLifecycleConfigNameContains
    , NotebookInstanceLifecycleConfigNameContains (..)

    -- * NotebookInstanceLifecycleConfigSummary
    , NotebookInstanceLifecycleConfigSummary (..)
    , mkNotebookInstanceLifecycleConfigSummary
    , nilcsNotebookInstanceLifecycleConfigName
    , nilcsNotebookInstanceLifecycleConfigArn
    , nilcsCreationTime
    , nilcsLastModifiedTime

    -- * ModelPackageArn
    , ModelPackageArn (..)

    -- * CaptureStatus
    , CaptureStatus (..)

    -- * EfsUid
    , EfsUid (..)

    -- * S3DataType
    , S3DataType (..)

    -- * VariantProperty
    , VariantProperty (..)
    , mkVariantProperty
    , vpVariantPropertyType

    -- * HumanTaskConfig
    , HumanTaskConfig (..)
    , mkHumanTaskConfig
    , htcWorkteamArn
    , htcUiConfig
    , htcPreHumanTaskLambdaArn
    , htcTaskTitle
    , htcTaskDescription
    , htcNumberOfHumanWorkersPerDataObject
    , htcTaskTimeLimitInSeconds
    , htcAnnotationConsolidationConfig
    , htcMaxConcurrentTaskCount
    , htcPublicWorkforceTaskPrice
    , htcTaskAvailabilityLifetimeInSeconds
    , htcTaskKeywords

    -- * LambdaFunctionArn
    , LambdaFunctionArn (..)

    -- * Accept
    , Accept (..)

    -- * TrialComponentArtifactValue
    , TrialComponentArtifactValue (..)

    -- * Operator
    , Operator (..)

    -- * ContainerImage
    , ContainerImage (..)

    -- * ProcessingStoppingCondition
    , ProcessingStoppingCondition (..)
    , mkProcessingStoppingCondition
    , pscMaxRuntimeInSeconds

    -- * SubscribedWorkteam
    , SubscribedWorkteam (..)
    , mkSubscribedWorkteam
    , swWorkteamArn
    , swListingId
    , swMarketplaceDescription
    , swMarketplaceTitle
    , swSellerName

    -- * AppInstanceType
    , AppInstanceType (..)

    -- * SortTrialsBy
    , SortTrialsBy (..)

    -- * CandidateStepName
    , CandidateStepName (..)

    -- * AlgorithmImage
    , AlgorithmImage (..)

    -- * DebugHookConfig
    , DebugHookConfig (..)
    , mkDebugHookConfig
    , dhcS3OutputPath
    , dhcCollectionConfigurations
    , dhcHookParameters
    , dhcLocalPath

    -- * CheckpointConfig
    , CheckpointConfig (..)
    , mkCheckpointConfig
    , ccS3Uri
    , ccLocalPath

    -- * ModelSummary
    , ModelSummary (..)
    , mkModelSummary
    , msModelName
    , msModelArn
    , msCreationTime

    -- * CodeRepositorySummary
    , CodeRepositorySummary (..)
    , mkCodeRepositorySummary
    , crsCodeRepositoryName
    , crsCodeRepositoryArn
    , crsCreationTime
    , crsLastModifiedTime
    , crsGitConfig

    -- * MetricName
    , MetricName (..)

    -- * ProcessingJobName
    , ProcessingJobName (..)

    -- * HyperParameterTuningJobSummary
    , HyperParameterTuningJobSummary (..)
    , mkHyperParameterTuningJobSummary
    , hHyperParameterTuningJobName
    , hHyperParameterTuningJobArn
    , hHyperParameterTuningJobStatus
    , hStrategy
    , hCreationTime
    , hTrainingJobStatusCounters
    , hObjectiveStatusCounters
    , hHyperParameterTuningEndTime
    , hLastModifiedTime
    , hResourceLimits

    -- * ImageStatus
    , ImageStatus (..)

    -- * ProcessingClusterConfig
    , ProcessingClusterConfig (..)
    , mkProcessingClusterConfig
    , pccInstanceCount
    , pccInstanceType
    , pccVolumeSizeInGB
    , pccVolumeKmsKeyId

    -- * ProcessingLocalPath
    , ProcessingLocalPath (..)

    -- * MonitoringOutput
    , MonitoringOutput (..)
    , mkMonitoringOutput
    , moS3Output

    -- * ImageNameContains
    , ImageNameContains (..)

    -- * AppDetails
    , AppDetails (..)
    , mkAppDetails
    , adAppName
    , adAppType
    , adCreationTime
    , adDomainId
    , adStatus
    , adUserProfileName

    -- * StoppingCondition
    , StoppingCondition (..)
    , mkStoppingCondition
    , scMaxRuntimeInSeconds
    , scMaxWaitTimeInSeconds

    -- * AlgorithmSummary
    , AlgorithmSummary (..)
    , mkAlgorithmSummary
    , aAlgorithmName
    , aAlgorithmArn
    , aCreationTime
    , aAlgorithmStatus
    , aAlgorithmDescription

    -- * ImageSortOrder
    , ImageSortOrder (..)

    -- * MonitoringExecutionSortKey
    , MonitoringExecutionSortKey (..)

    -- * ProblemType
    , ProblemType (..)

    -- * AutoMLOutputDataConfig
    , AutoMLOutputDataConfig (..)
    , mkAutoMLOutputDataConfig
    , amlodcS3OutputPath
    , amlodcKmsKeyId

    -- * Url
    , Url (..)

    -- * AutoMLJobConfig
    , AutoMLJobConfig (..)
    , mkAutoMLJobConfig
    , amljcCompletionCriteria
    , amljcSecurityConfig

    -- * AppImageConfigName
    , AppImageConfigName (..)

    -- * AuthMode
    , AuthMode (..)

    -- * AutoMLDataSource
    , AutoMLDataSource (..)
    , mkAutoMLDataSource
    , amldsS3DataSource

    -- * Channel
    , Channel (..)
    , mkChannel
    , cChannelName
    , cDataSource
    , cCompressionType
    , cContentType
    , cInputMode
    , cRecordWrapperType
    , cShuffleConfig

    -- * ObjectiveStatus
    , ObjectiveStatus (..)

    -- * FlowDefinitionTaskKeyword
    , FlowDefinitionTaskKeyword (..)

    -- * TrainingJobStatus
    , TrainingJobStatus (..)

    -- * TaskDescription
    , TaskDescription (..)

    -- * FlowDefinitionStatus
    , FlowDefinitionStatus (..)

    -- * ExperimentConfig
    , ExperimentConfig (..)
    , mkExperimentConfig
    , ecExperimentName
    , ecTrialComponentDisplayName
    , ecTrialName

    -- * AppName
    , AppName (..)

    -- * NetworkInterfaceId
    , NetworkInterfaceId (..)

    -- * UiConfig
    , UiConfig (..)
    , mkUiConfig
    , ucHumanTaskUiArn
    , ucUiTemplateS3Uri

    -- * NestedFilters
    , NestedFilters (..)
    , mkNestedFilters
    , nfNestedPropertyName
    , nfFilters

    -- * ImageSortBy
    , ImageSortBy (..)

    -- * SuggestionQuery
    , SuggestionQuery (..)
    , mkSuggestionQuery
    , sqPropertyNameQuery

    -- * LabelingJobAlgorithmSpecificationArn
    , LabelingJobAlgorithmSpecificationArn (..)

    -- * AutoMLJobName
    , AutoMLJobName (..)

    -- * UserContext
    , UserContext (..)
    , mkUserContext
    , ucDomainId
    , ucUserProfileArn
    , ucUserProfileName

    -- * AssemblyType
    , AssemblyType (..)

    -- * AutoMLJobCompletionCriteria
    , AutoMLJobCompletionCriteria (..)
    , mkAutoMLJobCompletionCriteria
    , amljccMaxAutoMLJobRuntimeInSeconds
    , amljccMaxCandidates
    , amljccMaxRuntimePerTrainingJobInSeconds

    -- * ExecutionStatus
    , ExecutionStatus (..)

    -- * ParameterKey
    , ParameterKey (..)

    -- * FileSystemId
    , FileSystemId (..)

    -- * InferenceSpecification
    , InferenceSpecification (..)
    , mkInferenceSpecification
    , isContainers
    , isSupportedTransformInstanceTypes
    , isSupportedRealtimeInferenceInstanceTypes
    , isSupportedContentTypes
    , isSupportedResponseMIMETypes

    -- * TargetPlatform
    , TargetPlatform (..)
    , mkTargetPlatform
    , tpOs
    , tpArch
    , tpAccelerator

    -- * FileSystemDataSource
    , FileSystemDataSource (..)
    , mkFileSystemDataSource
    , fsdsFileSystemId
    , fsdsFileSystemAccessMode
    , fsdsFileSystemType
    , fsdsDirectoryPath

    -- * SourceIpConfig
    , SourceIpConfig (..)
    , mkSourceIpConfig
    , sicCidrs

    -- * PresignedDomainUrl
    , PresignedDomainUrl (..)

    -- * ParameterRange
    , ParameterRange (..)
    , mkParameterRange
    , prCategoricalParameterRangeSpecification
    , prContinuousParameterRangeSpecification
    , prIntegerParameterRangeSpecification

    -- * HyperParameterTuningJobSortByOptions
    , HyperParameterTuningJobSortByOptions (..)

    -- * ListCompilationJobsSortBy
    , ListCompilationJobsSortBy (..)

    -- * TransformJobDefinition
    , TransformJobDefinition (..)
    , mkTransformJobDefinition
    , tjdTransformInput
    , tjdTransformOutput
    , tjdTransformResources
    , tjdBatchStrategy
    , tjdEnvironment
    , tjdMaxConcurrentTransforms
    , tjdMaxPayloadInMB

    -- * ContentClassifier
    , ContentClassifier (..)

    -- * EnvironmentValue
    , EnvironmentValue (..)

    -- * KernelSpec
    , KernelSpec (..)
    , mkKernelSpec
    , ksName
    , ksDisplayName

    -- * AutoMLJobObjective
    , AutoMLJobObjective (..)
    , mkAutoMLJobObjective
    , amljoMetricName

    -- * UserProfileStatus
    , UserProfileStatus (..)

    -- * AutoMLS3DataType
    , AutoMLS3DataType (..)

    -- * SubnetId
    , SubnetId (..)

    -- * ContinuousParameterRangeSpecification
    , ContinuousParameterRangeSpecification (..)
    , mkContinuousParameterRangeSpecification
    , cprsMinValue
    , cprsMaxValue

    -- * StatusDetails
    , StatusDetails (..)

    -- * WorkforceArn
    , WorkforceArn (..)

    -- * SecondaryStatusTransition
    , SecondaryStatusTransition (..)
    , mkSecondaryStatusTransition
    , sstStatus
    , sstStartTime
    , sstEndTime
    , sstStatusMessage

    -- * CollectionName
    , CollectionName (..)

    -- * WorkteamArn
    , WorkteamArn (..)

    -- * AutoMLCandidateStep
    , AutoMLCandidateStep (..)
    , mkAutoMLCandidateStep
    , amlcsCandidateStepType
    , amlcsCandidateStepArn
    , amlcsCandidateStepName

    -- * HumanLoopConfig
    , HumanLoopConfig (..)
    , mkHumanLoopConfig
    , hlcWorkteamArn
    , hlcHumanTaskUiArn
    , hlcTaskTitle
    , hlcTaskDescription
    , hlcTaskCount
    , hlcPublicWorkforceTaskPrice
    , hlcTaskAvailabilityLifetimeInSeconds
    , hlcTaskKeywords
    , hlcTaskTimeLimitInSeconds

    -- * TrialComponentStatus
    , TrialComponentStatus (..)
    , mkTrialComponentStatus
    , tcsMessage
    , tcsPrimaryStatus

    -- * LabelCounters
    , LabelCounters (..)
    , mkLabelCounters
    , lcFailedNonRetryableError
    , lcHumanLabeled
    , lcMachineLabeled
    , lcTotalLabeled
    , lcUnlabeled

    -- * ScheduleStatus
    , ScheduleStatus (..)

    -- * ParameterType
    , ParameterType (..)

    -- * ImageVersionStatus
    , ImageVersionStatus (..)

    -- * LabelingJobResourceConfig
    , LabelingJobResourceConfig (..)
    , mkLabelingJobResourceConfig
    , ljrcVolumeKmsKeyId

    -- * MonitoringOutputConfig
    , MonitoringOutputConfig (..)
    , mkMonitoringOutputConfig
    , mocMonitoringOutputs
    , mocKmsKeyId

    -- * InstanceType
    , InstanceType (..)

    -- * ImageVersionSortOrder
    , ImageVersionSortOrder (..)

    -- * NotebookInstanceLifecycleConfigSortKey
    , NotebookInstanceLifecycleConfigSortKey (..)

    -- * TransformS3DataSource
    , TransformS3DataSource (..)
    , mkTransformS3DataSource
    , tsdsS3DataType
    , tsdsS3Uri

    -- * ModelPackageSummary
    , ModelPackageSummary (..)
    , mkModelPackageSummary
    , mpsModelPackageName
    , mpsModelPackageArn
    , mpsCreationTime
    , mpsModelPackageStatus
    , mpsModelPackageDescription

    -- * ConfigValue
    , ConfigValue (..)

    -- * HumanTaskUiName
    , HumanTaskUiName (..)

    -- * NotebookInstanceLifecycleConfigArn
    , NotebookInstanceLifecycleConfigArn (..)

    -- * CandidateStatus
    , CandidateStatus (..)

    -- * ImageUri
    , ImageUri (..)

    -- * TrialComponentSourceDetail
    , TrialComponentSourceDetail (..)
    , mkTrialComponentSourceDetail
    , tcsdProcessingJob
    , tcsdSourceArn
    , tcsdTrainingJob
    , tcsdTransformJob

    -- * Experiment
    , Experiment (..)
    , mkExperiment
    , eCreatedBy
    , eCreationTime
    , eDescription
    , eDisplayName
    , eExperimentArn
    , eExperimentName
    , eLastModifiedBy
    , eLastModifiedTime
    , eSource
    , eTags

    -- * TargetPlatformAccelerator
    , TargetPlatformAccelerator (..)

    -- * FileSystemConfig
    , FileSystemConfig (..)
    , mkFileSystemConfig
    , fscDefaultGid
    , fscDefaultUid
    , fscMountPath

    -- * InputConfig
    , InputConfig (..)
    , mkInputConfig
    , icS3Uri
    , icDataInputConfig
    , icFramework

    -- * SecurityGroupId
    , SecurityGroupId (..)

    -- * NetworkConfig
    , NetworkConfig (..)
    , mkNetworkConfig
    , ncEnableInterContainerTrafficEncryption
    , ncEnableNetworkIsolation
    , ncVpcConfig

    -- * AccountId
    , AccountId (..)

    -- * ProductionVariant
    , ProductionVariant (..)
    , mkProductionVariant
    , pvVariantName
    , pvModelName
    , pvInitialInstanceCount
    , pvInstanceType
    , pvAcceleratorType
    , pvInitialVariantWeight

    -- * NotebookInstanceStatus
    , NotebookInstanceStatus (..)

    -- * PropertyNameSuggestion
    , PropertyNameSuggestion (..)
    , mkPropertyNameSuggestion
    , pnsPropertyName

    -- * Branch
    , Branch (..)

    -- * ProcessingS3Output
    , ProcessingS3Output (..)
    , mkProcessingS3Output
    , psoS3Uri
    , psoLocalPath
    , psoS3UploadMode

    -- * ExperimentSource
    , ExperimentSource (..)
    , mkExperimentSource
    , esSourceArn
    , esSourceType

    -- * ListWorkforcesSortByOptions
    , ListWorkforcesSortByOptions (..)

    -- * NextToken
    , NextToken (..)

    -- * RetentionPolicy
    , RetentionPolicy (..)
    , mkRetentionPolicy
    , rpHomeEfsFileSystem

    -- * VersionedArnOrName
    , VersionedArnOrName (..)

    -- * RetentionType
    , RetentionType (..)

    -- * BooleanOperator
    , BooleanOperator (..)

    -- * AppSortKey
    , AppSortKey (..)

    -- * CandidateSortBy
    , CandidateSortBy (..)

    -- * ChannelName
    , ChannelName (..)

    -- * DirectoryPath
    , DirectoryPath (..)

    -- * Cidr
    , Cidr (..)

    -- * SearchExpression
    , SearchExpression (..)
    , mkSearchExpression
    , seFilters
    , seNestedFilters
    , seOperator
    , seSubExpressions

    -- * JupyterServerAppSettings
    , JupyterServerAppSettings (..)
    , mkJupyterServerAppSettings
    , jsasDefaultResourceSpec

    -- * ContainerArgument
    , ContainerArgument (..)

    -- * LabelingJobS3DataSource
    , LabelingJobS3DataSource (..)
    , mkLabelingJobS3DataSource
    , ljsdsManifestS3Uri

    -- * RuleEvaluationStatus
    , RuleEvaluationStatus (..)

    -- * ExperimentArn
    , ExperimentArn (..)

    -- * ListWorkteamsSortByOptions
    , ListWorkteamsSortByOptions (..)

    -- * CustomImage
    , CustomImage (..)
    , mkCustomImage
    , ciImageName
    , ciAppImageConfigName
    , ciImageVersionNumber

    -- * ProcessingJob
    , ProcessingJob (..)
    , mkProcessingJob
    , pjAppSpecification
    , pjAutoMLJobArn
    , pjCreationTime
    , pjEnvironment
    , pjExitMessage
    , pjExperimentConfig
    , pjFailureReason
    , pjLastModifiedTime
    , pjMonitoringScheduleArn
    , pjNetworkConfig
    , pjProcessingEndTime
    , pjProcessingInputs
    , pjProcessingJobArn
    , pjProcessingJobName
    , pjProcessingJobStatus
    , pjProcessingOutputConfig
    , pjProcessingResources
    , pjProcessingStartTime
    , pjRoleArn
    , pjStoppingCondition
    , pjTags
    , pjTrainingJobArn

    -- * DataInputConfig
    , DataInputConfig (..)

    -- * AutoMLJobArtifacts
    , AutoMLJobArtifacts (..)
    , mkAutoMLJobArtifacts
    , amljaCandidateDefinitionNotebookLocation
    , amljaDataExplorationNotebookLocation

    -- * HyperParameterTuningJobWarmStartConfig
    , HyperParameterTuningJobWarmStartConfig (..)
    , mkHyperParameterTuningJobWarmStartConfig
    , hptjwscParentHyperParameterTuningJobs
    , hptjwscWarmStartType

    -- * AppArn
    , AppArn (..)

    -- * NotebookInstanceSummary
    , NotebookInstanceSummary (..)
    , mkNotebookInstanceSummary
    , nisNotebookInstanceName
    , nisNotebookInstanceArn
    , nisAdditionalCodeRepositories
    , nisCreationTime
    , nisDefaultCodeRepository
    , nisInstanceType
    , nisLastModifiedTime
    , nisNotebookInstanceLifecycleConfigName
    , nisNotebookInstanceStatus
    , nisUrl

    -- * MetricData
    , MetricData (..)
    , mkMetricData
    , mdMetricName
    , mdTimestamp
    , mdValue

    -- * AutoMLJobArn
    , AutoMLJobArn (..)

    -- * SortOrder
    , SortOrder (..)

    -- * TaskKeyword
    , TaskKeyword (..)

    -- * TrainingJobEarlyStoppingType
    , TrainingJobEarlyStoppingType (..)

    -- * ContainerMode
    , ContainerMode (..)

    -- * ImageVersionSortBy
    , ImageVersionSortBy (..)

    -- * MonitoringClusterConfig
    , MonitoringClusterConfig (..)
    , mkMonitoringClusterConfig
    , mccInstanceCount
    , mccInstanceType
    , mccVolumeSizeInGB
    , mccVolumeKmsKeyId

    -- * VpcConfig
    , VpcConfig (..)
    , mkVpcConfig
    , vcSecurityGroupIds
    , vcSubnets

    -- * ImageDigest
    , ImageDigest (..)

    -- * AutoMLJobSecondaryStatus
    , AutoMLJobSecondaryStatus (..)

    -- * TrialSource
    , TrialSource (..)
    , mkTrialSource
    , tsSourceArn
    , tsSourceType

    -- * NotebookInstanceAcceleratorType
    , NotebookInstanceAcceleratorType (..)

    -- * KmsKeyId
    , KmsKeyId (..)

    -- * ResourceArn
    , ResourceArn (..)

    -- * ResponseMIMEType
    , ResponseMIMEType (..)

    -- * DomainName
    , DomainName (..)

    -- * TransformJobStatus
    , TransformJobStatus (..)

    -- * ModelPackageContainerDefinition
    , ModelPackageContainerDefinition (..)
    , mkModelPackageContainerDefinition
    , mpcdImage
    , mpcdContainerHostname
    , mpcdImageDigest
    , mpcdModelDataUrl
    , mpcdProductId

    -- * DebugRuleConfiguration
    , DebugRuleConfiguration (..)
    , mkDebugRuleConfiguration
    , drcRuleConfigurationName
    , drcRuleEvaluatorImage
    , drcInstanceType
    , drcLocalPath
    , drcRuleParameters
    , drcS3OutputPath
    , drcVolumeSizeInGB

    -- * StatusMessage
    , StatusMessage (..)

    -- * ScheduleExpression
    , ScheduleExpression (..)

    -- * DesiredWeightAndCapacity
    , DesiredWeightAndCapacity (..)
    , mkDesiredWeightAndCapacity
    , dwacVariantName
    , dwacDesiredInstanceCount
    , dwacDesiredWeight

    -- * HumanLoopRequestSource
    , HumanLoopRequestSource (..)
    , mkHumanLoopRequestSource
    , hlrsAwsManagedHumanLoopRequestSource

    -- * AnnotationConsolidationConfig
    , AnnotationConsolidationConfig (..)
    , mkAnnotationConsolidationConfig
    , accAnnotationConsolidationLambdaArn

    -- * SortTrialComponentsBy
    , SortTrialComponentsBy (..)

    -- * EndpointStatus
    , EndpointStatus (..)

    -- * UserProfileSortKey
    , UserProfileSortKey (..)

    -- * JoinSource
    , JoinSource (..)

    -- * CaptureOption
    , CaptureOption (..)
    , mkCaptureOption
    , coCaptureMode

    -- * ResolvedAttributes
    , ResolvedAttributes (..)
    , mkResolvedAttributes
    , raAutoMLJobObjective
    , raCompletionCriteria
    , raProblemType

    -- * MonitoringStoppingCondition
    , MonitoringStoppingCondition (..)
    , mkMonitoringStoppingCondition
    , mscMaxRuntimeInSeconds

    -- * UserProfileArn
    , UserProfileArn (..)

    -- * MonitoringS3Uri
    , MonitoringS3Uri (..)

    -- * SourceAlgorithm
    , SourceAlgorithm (..)
    , mkSourceAlgorithm
    , saAlgorithmName
    , saModelDataUrl

    -- * SearchSortOrder
    , SearchSortOrder (..)

    -- * CompilationJobStatus
    , CompilationJobStatus (..)

    -- * FlowDefinitionArn
    , FlowDefinitionArn (..)

    -- * DomainDetails
    , DomainDetails (..)
    , mkDomainDetails
    , ddCreationTime
    , ddDomainArn
    , ddDomainId
    , ddDomainName
    , ddLastModifiedTime
    , ddStatus
    , ddUrl

    -- * TransformInput
    , TransformInput (..)
    , mkTransformInput
    , tiDataSource
    , tiCompressionType
    , tiContentType
    , tiSplitType

    -- * RootAccess
    , RootAccess (..)

    -- * HumanTaskUiSummary
    , HumanTaskUiSummary (..)
    , mkHumanTaskUiSummary
    , htusHumanTaskUiName
    , htusHumanTaskUiArn
    , htusCreationTime

    -- * TrainingJobArn
    , TrainingJobArn (..)

    -- * AlgorithmSpecification
    , AlgorithmSpecification (..)
    , mkAlgorithmSpecification
    , asTrainingInputMode
    , asAlgorithmName
    , asEnableSageMakerMetricsTimeSeries
    , asMetricDefinitions
    , asTrainingImage

    -- * AutoMLNameContains
    , AutoMLNameContains (..)

    -- * TargetAttributeName
    , TargetAttributeName (..)

    -- * GitConfigForUpdate
    , GitConfigForUpdate (..)
    , mkGitConfigForUpdate
    , gcfuSecretArn

    -- * LabelingJobForWorkteamSummary
    , LabelingJobForWorkteamSummary (..)
    , mkLabelingJobForWorkteamSummary
    , ljfwsJobReferenceCode
    , ljfwsWorkRequesterAccountId
    , ljfwsCreationTime
    , ljfwsLabelCounters
    , ljfwsLabelingJobName
    , ljfwsNumberOfHumanWorkersPerDataObject

    -- * FlowDefinitionOutputConfig
    , FlowDefinitionOutputConfig (..)
    , mkFlowDefinitionOutputConfig
    , fdocS3OutputPath
    , fdocKmsKeyId

    -- * CandidateName
    , CandidateName (..)

    -- * TransformEnvironmentValue
    , TransformEnvironmentValue (..)

    -- * ContainerDefinition
    , ContainerDefinition (..)
    , mkContainerDefinition
    , cdContainerHostname
    , cdEnvironment
    , cdImage
    , cdImageConfig
    , cdMode
    , cdModelDataUrl
    , cdModelPackageName

    -- * SplitType
    , SplitType (..)

    -- * AutoMLFailureReason
    , AutoMLFailureReason (..)

    -- * HyperParameterSpecification
    , HyperParameterSpecification (..)
    , mkHyperParameterSpecification
    , hpsName
    , hpsType
    , hpsDefaultValue
    , hpsDescription
    , hpsIsRequired
    , hpsIsTunable
    , hpsRange

    -- * RenderingError
    , RenderingError (..)
    , mkRenderingError
    , reCode
    , reMessage

    -- * AutoMLSecurityConfig
    , AutoMLSecurityConfig (..)
    , mkAutoMLSecurityConfig
    , amlscEnableInterContainerTrafficEncryption
    , amlscVolumeKmsKeyId
    , amlscVpcConfig

    -- * DirectInternetAccess
    , DirectInternetAccess (..)

    -- * ProcessingInstanceType
    , ProcessingInstanceType (..)

    -- * HumanTaskUiStatus
    , HumanTaskUiStatus (..)

    -- * CompilationJobSummary
    , CompilationJobSummary (..)
    , mkCompilationJobSummary
    , cjsCompilationJobName
    , cjsCompilationJobArn
    , cjsCreationTime
    , cjsCompilationJobStatus
    , cjsCompilationEndTime
    , cjsCompilationStartTime
    , cjsCompilationTargetDevice
    , cjsCompilationTargetPlatformAccelerator
    , cjsCompilationTargetPlatformArch
    , cjsCompilationTargetPlatformOs
    , cjsLastModifiedTime

    -- * ContainerEntrypointString
    , ContainerEntrypointString (..)

    -- * S3DataDistribution
    , S3DataDistribution (..)

    -- * SingleSignOnUserIdentifier
    , SingleSignOnUserIdentifier (..)

    -- * ImageArn
    , ImageArn (..)

    -- * LabelAttributeName
    , LabelAttributeName (..)

    -- * SharingSettings
    , SharingSettings (..)
    , mkSharingSettings
    , ssNotebookOutputOption
    , ssS3KmsKeyId
    , ssS3OutputPath

    -- * LabelingJobStatus
    , LabelingJobStatus (..)

    -- * MonitoringResources
    , MonitoringResources (..)
    , mkMonitoringResources
    , mrClusterConfig

    -- * RenderableTask
    , RenderableTask (..)
    , mkRenderableTask
    , rtInput

    -- * OutputConfig
    , OutputConfig (..)
    , mkOutputConfig
    , ocS3OutputLocation
    , ocCompilerOptions
    , ocTargetDevice
    , ocTargetPlatform

    -- * ProcessingInput
    , ProcessingInput (..)
    , mkProcessingInput
    , piInputName
    , piS3Input

    -- * DebugRuleEvaluationStatus
    , DebugRuleEvaluationStatus (..)
    , mkDebugRuleEvaluationStatus
    , dresLastModifiedTime
    , dresRuleConfigurationName
    , dresRuleEvaluationJobArn
    , dresRuleEvaluationStatus
    , dresStatusDetails

    -- * DataSource
    , DataSource (..)
    , mkDataSource
    , dsFileSystemDataSource
    , dsS3DataSource

    -- * CognitoConfig
    , CognitoConfig (..)
    , mkCognitoConfig
    , ccUserPool
    , ccClientId

    -- * OutputDataConfig
    , OutputDataConfig (..)
    , mkOutputDataConfig
    , odcS3OutputPath
    , odcKmsKeyId

    -- * NotebookInstanceSortKey
    , NotebookInstanceSortKey (..)

    -- * MemberDefinition
    , MemberDefinition (..)
    , mkMemberDefinition
    , mdCognitoMemberDefinition
    , mdOidcMemberDefinition

    -- * TaskTitle
    , TaskTitle (..)

    -- * ConfigKey
    , ConfigKey (..)

    -- * EndpointConfigNameContains
    , EndpointConfigNameContains (..)

    -- * ExperimentSummary
    , ExperimentSummary (..)
    , mkExperimentSummary
    , esfCreationTime
    , esfDisplayName
    , esfExperimentArn
    , esfExperimentName
    , esfExperimentSource
    , esfLastModifiedTime

    -- * ProductionVariantInstanceType
    , ProductionVariantInstanceType (..)

    -- * LabelingJobInputConfig
    , LabelingJobInputConfig (..)
    , mkLabelingJobInputConfig
    , ljicDataSource
    , ljicDataAttributes

    -- * AutoMLJobSummary
    , AutoMLJobSummary (..)
    , mkAutoMLJobSummary
    , amljsAutoMLJobName
    , amljsAutoMLJobArn
    , amljsAutoMLJobStatus
    , amljsAutoMLJobSecondaryStatus
    , amljsCreationTime
    , amljsLastModifiedTime
    , amljsEndTime
    , amljsFailureReason

    -- * HyperParameterTuningJobConfig
    , HyperParameterTuningJobConfig (..)
    , mkHyperParameterTuningJobConfig
    , hptjcStrategy
    , hptjcResourceLimits
    , hptjcHyperParameterTuningJobObjective
    , hptjcParameterRanges
    , hptjcTrainingJobEarlyStoppingType
    , hptjcTuningJobCompletionCriteria

    -- * EndpointInput
    , EndpointInput (..)
    , mkEndpointInput
    , eiEndpointName
    , eiLocalPath
    , eiS3DataDistributionType
    , eiS3InputMode

    -- * NotebookInstanceArn
    , NotebookInstanceArn (..)

    -- * ProcessingS3CompressionType
    , ProcessingS3CompressionType (..)

    -- * TrialComponentSourceArn
    , TrialComponentSourceArn (..)

    -- * AutoMLChannel
    , AutoMLChannel (..)
    , mkAutoMLChannel
    , amlcDataSource
    , amlcTargetAttributeName
    , amlcCompressionType

    -- * DataExplorationNotebookLocation
    , DataExplorationNotebookLocation (..)

    -- * DomainId
    , DomainId (..)

    -- * HumanLoopActivationConfig
    , HumanLoopActivationConfig (..)
    , mkHumanLoopActivationConfig
    , hlacHumanLoopActivationConditionsConfig

    -- * TagKey
    , TagKey (..)

    -- * ContainerHostname
    , ContainerHostname (..)

    -- * VariantPropertyType
    , VariantPropertyType (..)

    -- * IntegerParameterRange
    , IntegerParameterRange (..)
    , mkIntegerParameterRange
    , iprName
    , iprMinValue
    , iprMaxValue
    , iprScalingType

    -- * HyperParameterTuningJobObjectiveType
    , HyperParameterTuningJobObjectiveType (..)

    -- * TrainingJobName
    , TrainingJobName (..)

    -- * ProcessingJobSummary
    , ProcessingJobSummary (..)
    , mkProcessingJobSummary
    , pjsProcessingJobName
    , pjsProcessingJobArn
    , pjsCreationTime
    , pjsProcessingJobStatus
    , pjsExitMessage
    , pjsFailureReason
    , pjsLastModifiedTime
    , pjsProcessingEndTime

    -- * ProductionVariantSummary
    , ProductionVariantSummary (..)
    , mkProductionVariantSummary
    , pvsVariantName
    , pvsCurrentInstanceCount
    , pvsCurrentWeight
    , pvsDeployedImages
    , pvsDesiredInstanceCount
    , pvsDesiredWeight

    -- * CategoricalParameterRange
    , CategoricalParameterRange (..)
    , mkCategoricalParameterRange
    , cprName
    , cprValues

    -- * MonitoringAppSpecification
    , MonitoringAppSpecification (..)
    , mkMonitoringAppSpecification
    , masImageUri
    , masContainerArguments
    , masContainerEntrypoint
    , masPostAnalyticsProcessorSourceUri
    , masRecordPreprocessorSourceUri

    -- * MountPath
    , MountPath (..)

    -- * TrialComponentKey64
    , TrialComponentKey64 (..)

    -- * NotebookInstanceLifecycleConfigSortOrder
    , NotebookInstanceLifecycleConfigSortOrder (..)

    -- * VariantName
    , VariantName (..)

    -- * ImageVersionArn
    , ImageVersionArn (..)

    -- * ResourceConfig
    , ResourceConfig (..)
    , mkResourceConfig
    , rcInstanceType
    , rcInstanceCount
    , rcVolumeSizeInGB
    , rcVolumeKmsKeyId

    -- * Filter
    , Filter (..)
    , mkFilter
    , fName
    , fOperator
    , fValue

    -- * FlowDefinitionName
    , FlowDefinitionName (..)

    -- * FinalHyperParameterTuningJobObjectiveMetric
    , FinalHyperParameterTuningJobObjectiveMetric (..)
    , mkFinalHyperParameterTuningJobObjectiveMetric
    , fhptjomMetricName
    , fhptjomValue
    , fhptjomType

    -- * TrialComponentArn
    , TrialComponentArn (..)

    -- * GitConfig
    , GitConfig (..)
    , mkGitConfig
    , gcRepositoryUrl
    , gcBranch
    , gcSecretArn

    -- * ImageName
    , ImageName (..)

    -- * HyperParameterKey
    , HyperParameterKey (..)

    -- * MonitoringConstraintsResource
    , MonitoringConstraintsResource (..)
    , mkMonitoringConstraintsResource
    , mcrS3Uri

    -- * ProcessingJobStatus
    , ProcessingJobStatus (..)

    -- * TrialComponentSource
    , TrialComponentSource (..)
    , mkTrialComponentSource
    , tcsSourceArn
    , tcsSourceType

    -- * WorkteamName
    , WorkteamName (..)

    -- * ContinuousParameterRange
    , ContinuousParameterRange (..)
    , mkContinuousParameterRange
    , cName
    , cMinValue
    , cMaxValue
    , cScalingType

    -- * DetailedAlgorithmStatus
    , DetailedAlgorithmStatus (..)

    -- * TemplateUrl
    , TemplateUrl (..)

    -- * MonitoringStatisticsResource
    , MonitoringStatisticsResource (..)
    , mkMonitoringStatisticsResource
    , msrS3Uri

    -- * TrialComponentKey256
    , TrialComponentKey256 (..)

    -- * AlgorithmStatus
    , AlgorithmStatus (..)

    -- * ProductionVariantAcceleratorType
    , ProductionVariantAcceleratorType (..)

    -- * FlowDefinitionSummary
    , FlowDefinitionSummary (..)
    , mkFlowDefinitionSummary
    , fdsFlowDefinitionName
    , fdsFlowDefinitionArn
    , fdsFlowDefinitionStatus
    , fdsCreationTime
    , fdsFailureReason

    -- * PropertyNameHint
    , PropertyNameHint (..)

    -- * TransformOutput
    , TransformOutput (..)
    , mkTransformOutput
    , toS3OutputPath
    , toAccept
    , toAssembleWith
    , toKmsKeyId

    -- * ExitMessage
    , ExitMessage (..)

    -- * ParentHyperParameterTuningJob
    , ParentHyperParameterTuningJob (..)
    , mkParentHyperParameterTuningJob
    , phptjHyperParameterTuningJobName

    -- * AppNetworkAccessType
    , AppNetworkAccessType (..)

    -- * CodeRepositorySortOrder
    , CodeRepositorySortOrder (..)

    -- * OidcConfig
    , OidcConfig (..)
    , mkOidcConfig
    , ocClientId
    , ocClientSecret
    , ocIssuer
    , ocAuthorizationEndpoint
    , ocTokenEndpoint
    , ocUserInfoEndpoint
    , ocLogoutEndpoint
    , ocJwksUri

    -- * CompilationJobArn
    , CompilationJobArn (..)

    -- * SearchRecord
    , SearchRecord (..)
    , mkSearchRecord
    , srExperiment
    , srTrainingJob
    , srTrial
    , srTrialComponent

    -- * HyperParameterTuningJobStatus
    , HyperParameterTuningJobStatus (..)

    -- * ModelPackageStatusDetails
    , ModelPackageStatusDetails (..)
    , mkModelPackageStatusDetails
    , mpsdValidationStatuses
    , mpsdImageScanStatuses

    -- * MonitoringS3Output
    , MonitoringS3Output (..)
    , mkMonitoringS3Output
    , msoS3Uri
    , msoLocalPath
    , msoS3UploadMode

    -- * AttributeName
    , AttributeName (..)

    -- * TrainingJobSummary
    , TrainingJobSummary (..)
    , mkTrainingJobSummary
    , tjsfTrainingJobName
    , tjsfTrainingJobArn
    , tjsfCreationTime
    , tjsfTrainingJobStatus
    , tjsfLastModifiedTime
    , tjsfTrainingEndTime

    -- * TrialComponentArtifact
    , TrialComponentArtifact (..)
    , mkTrialComponentArtifact
    , tcaValue
    , tcaMediaType

    -- * WorkforceName
    , WorkforceName (..)

    -- * OidcConfigForResponse
    , OidcConfigForResponse (..)
    , mkOidcConfigForResponse
    , ocfrAuthorizationEndpoint
    , ocfrClientId
    , ocfrIssuer
    , ocfrJwksUri
    , ocfrLogoutEndpoint
    , ocfrTokenEndpoint
    , ocfrUserInfoEndpoint

    -- * DataProcessing
    , DataProcessing (..)
    , mkDataProcessing
    , dpInputFilter
    , dpJoinSource
    , dpOutputFilter

    -- * ProcessingS3UploadMode
    , ProcessingS3UploadMode (..)

    -- * HumanTaskUiArn
    , HumanTaskUiArn (..)

    -- * AutoMLCandidate
    , AutoMLCandidate (..)
    , mkAutoMLCandidate
    , amlcCandidateName
    , amlcObjectiveStatus
    , amlcCandidateSteps
    , amlcCandidateStatus
    , amlcCreationTime
    , amlcLastModifiedTime
    , amlcEndTime
    , amlcFailureReason
    , amlcFinalAutoMLJobObjectiveMetric
    , amlcInferenceContainers

    -- * SecretArn
    , SecretArn (..)

    -- * SortExperimentsBy
    , SortExperimentsBy (..)

    -- * ProcessingS3DataType
    , ProcessingS3DataType (..)

    -- * TransformJob
    , TransformJob (..)
    , mkTransformJob
    , tjfAutoMLJobArn
    , tjfBatchStrategy
    , tjfCreationTime
    , tjfDataProcessing
    , tjfEnvironment
    , tjfExperimentConfig
    , tjfFailureReason
    , tjfLabelingJobArn
    , tjfMaxConcurrentTransforms
    , tjfMaxPayloadInMB
    , tjfModelClientConfig
    , tjfModelName
    , tjfTags
    , tjfTransformEndTime
    , tjfTransformInput
    , tjfTransformJobArn
    , tjfTransformJobName
    , tjfTransformJobStatus
    , tjfTransformOutput
    , tjfTransformResources
    , tjfTransformStartTime

    -- * NotebookInstanceLifecycleConfigName
    , NotebookInstanceLifecycleConfigName (..)

    -- * MonitoringScheduleConfig
    , MonitoringScheduleConfig (..)
    , mkMonitoringScheduleConfig
    , mscMonitoringJobDefinition
    , mscScheduleConfig

    -- * CompilerOptions
    , CompilerOptions (..)

    -- * FileSystemType
    , FileSystemType (..)

    -- * ModelPackageValidationSpecification
    , ModelPackageValidationSpecification (..)
    , mkModelPackageValidationSpecification
    , mpvsValidationRole
    , mpvsValidationProfiles

    -- * AutoMLS3DataSource
    , AutoMLS3DataSource (..)
    , mkAutoMLS3DataSource
    , amlsdsS3DataType
    , amlsdsS3Uri

    -- * TargetPlatformArch
    , TargetPlatformArch (..)

    -- * LabelingJobName
    , LabelingJobName (..)

    -- * CompressionType
    , CompressionType (..)

    -- * KernelGatewayImageConfig
    , KernelGatewayImageConfig (..)
    , mkKernelGatewayImageConfig
    , kgicKernelSpecs
    , kgicFileSystemConfig

    -- * AlgorithmStatusItem
    , AlgorithmStatusItem (..)
    , mkAlgorithmStatusItem
    , asiName
    , asiStatus
    , asiFailureReason

    -- * ChannelSpecification
    , ChannelSpecification (..)
    , mkChannelSpecification
    , csName
    , csSupportedContentTypes
    , csSupportedInputModes
    , csDescription
    , csIsRequired
    , csSupportedCompressionTypes

    -- * TransformEnvironmentKey
    , TransformEnvironmentKey (..)

    -- * EndpointConfigName
    , EndpointConfigName (..)

    -- * ProcessingOutputConfig
    , ProcessingOutputConfig (..)
    , mkProcessingOutputConfig
    , pocOutputs
    , pocKmsKeyId

    -- * AlgorithmSortBy
    , AlgorithmSortBy (..)

    -- * AppImageConfigDetails
    , AppImageConfigDetails (..)
    , mkAppImageConfigDetails
    , aicdAppImageConfigArn
    , aicdAppImageConfigName
    , aicdCreationTime
    , aicdKernelGatewayImageConfig
    , aicdLastModifiedTime

    -- * S3Uri
    , S3Uri (..)

    -- * AutoMLJobObjectiveType
    , AutoMLJobObjectiveType (..)

    -- * TensorBoardOutputConfig
    , TensorBoardOutputConfig (..)
    , mkTensorBoardOutputConfig
    , tbocS3OutputPath
    , tbocLocalPath

    -- * MonitoringScheduleName
    , MonitoringScheduleName (..)

    -- * TrialSummary
    , TrialSummary (..)
    , mkTrialSummary
    , tsCreationTime
    , tsDisplayName
    , tsLastModifiedTime
    , tsTrialArn
    , tsTrialName
    , tsTrialSource

    -- * CodeRepositoryNameOrUrl
    , CodeRepositoryNameOrUrl (..)

    -- * MonitoringJobDefinition
    , MonitoringJobDefinition (..)
    , mkMonitoringJobDefinition
    , mjdMonitoringInputs
    , mjdMonitoringOutputConfig
    , mjdMonitoringResources
    , mjdMonitoringAppSpecification
    , mjdRoleArn
    , mjdBaselineConfig
    , mjdEnvironment
    , mjdNetworkConfig
    , mjdStoppingCondition

    -- * LabelingJobDataAttributes
    , LabelingJobDataAttributes (..)
    , mkLabelingJobDataAttributes
    , ljdaContentClassifiers

    -- * FinalAutoMLJobObjectiveMetric
    , FinalAutoMLJobObjectiveMetric (..)
    , mkFinalAutoMLJobObjectiveMetric
    , famljomMetricName
    , famljomValue
    , famljomType

    -- * EndpointArn
    , EndpointArn (..)

    -- * ListLabelingJobsForWorkteamSortByOptions
    , ListLabelingJobsForWorkteamSortByOptions (..)

    -- * CodeRepositorySortBy
    , CodeRepositorySortBy (..)

    -- * HyperParameterTuningJobWarmStartType
    , HyperParameterTuningJobWarmStartType (..)

    -- * CollectionConfiguration
    , CollectionConfiguration (..)
    , mkCollectionConfiguration
    , ccCollectionName
    , ccCollectionParameters

    -- * RecordWrapper
    , RecordWrapper (..)

    -- * SecondaryStatus
    , SecondaryStatus (..)

    -- * LabelingJobOutput
    , LabelingJobOutput (..)
    , mkLabelingJobOutput
    , ljoOutputDatasetS3Uri
    , ljoFinalActiveLearningModelArn

    -- * EndpointSortKey
    , EndpointSortKey (..)

    -- * TransformJobArn
    , TransformJobArn (..)

    -- * ProductId
    , ProductId (..)

    -- * LabelCountersForWorkteam
    , LabelCountersForWorkteam (..)
    , mkLabelCountersForWorkteam
    , lcfwHumanLabeled
    , lcfwPendingHuman
    , lcfwTotal

    -- * DataCaptureConfig
    , DataCaptureConfig (..)
    , mkDataCaptureConfig
    , dccInitialSamplingPercentage
    , dccDestinationS3Uri
    , dccCaptureOptions
    , dccCaptureContentTypeHeader
    , dccEnableCapture
    , dccKmsKeyId

    -- * TrainingJobSortByOptions
    , TrainingJobSortByOptions (..)

    -- * ProcessingS3InputMode
    , ProcessingS3InputMode (..)

    -- * AlgorithmValidationSpecification
    , AlgorithmValidationSpecification (..)
    , mkAlgorithmValidationSpecification
    , avsValidationRole
    , avsValidationProfiles

    -- * NotificationTopicArn
    , NotificationTopicArn (..)

    -- * AppImageConfigSortKey
    , AppImageConfigSortKey (..)

    -- * TargetDevice
    , TargetDevice (..)

    -- * TransformDataSource
    , TransformDataSource (..)
    , mkTransformDataSource
    , tdsS3DataSource

    -- * AutoMLMetricEnum
    , AutoMLMetricEnum (..)

    -- * SortBy
    , SortBy (..)

    -- * RuleConfigurationName
    , RuleConfigurationName (..)

    -- * ProcessingS3DataDistributionType
    , ProcessingS3DataDistributionType (..)

    -- * AlgorithmStatusDetails
    , AlgorithmStatusDetails (..)
    , mkAlgorithmStatusDetails
    , asdImageScanStatuses
    , asdValidationStatuses

    -- * MetricRegex
    , MetricRegex (..)

    -- * ContentType
    , ContentType (..)

    -- * ProcessingJobArn
    , ProcessingJobArn (..)

    -- * LabelingJobStoppingConditions
    , LabelingJobStoppingConditions (..)
    , mkLabelingJobStoppingConditions
    , ljscMaxHumanLabeledObjectCount
    , ljscMaxPercentageOfInputDatasetLabeled

    -- * DetailedModelPackageStatus
    , DetailedModelPackageStatus (..)

    -- * ModelPackageStatus
    , ModelPackageStatus (..)

    -- * JobReferenceCode
    , JobReferenceCode (..)

    -- * TrialComponentSummary
    , TrialComponentSummary (..)
    , mkTrialComponentSummary
    , tcsCreatedBy
    , tcsCreationTime
    , tcsDisplayName
    , tcsEndTime
    , tcsLastModifiedBy
    , tcsLastModifiedTime
    , tcsStartTime
    , tcsStatus
    , tcsTrialComponentArn
    , tcsTrialComponentName
    , tcsTrialComponentSource

    -- * MetricDefinition
    , MetricDefinition (..)
    , mkMetricDefinition
    , mdName
    , mdRegex

    -- * ImageDeleteProperty
    , ImageDeleteProperty (..)

    -- * JsonContentType
    , JsonContentType (..)

    -- * TrainingInstanceType
    , TrainingInstanceType (..)

    -- * AppType
    , AppType (..)

    -- * AppImageConfigArn
    , AppImageConfigArn (..)

    -- * RoleArn
    , RoleArn (..)

    -- * ProcessingEnvironmentKey
    , ProcessingEnvironmentKey (..)

    -- * OrderKey
    , OrderKey (..)

    -- * AlgorithmName
    , AlgorithmName (..)

    -- * ExperimentName
    , ExperimentName (..)

    -- * TrialName
    , TrialName (..)

    -- * Content
    , Content (..)

    -- * LabelCategoryConfigS3Uri
    , LabelCategoryConfigS3Uri (..)

    -- * ExecutionRole
    , ExecutionRole (..)

    -- * Name
    , Name (..)

    -- * S3OutputPath
    , S3OutputPath (..)

    -- * DisplayName
    , DisplayName (..)

    -- * TrainingJobDefinitionName
    , TrainingJobDefinitionName (..)

    -- * TuningJobName
    , TuningJobName (..)

    -- * Description
    , Description (..)

    -- * ModelDataUrl
    , ModelDataUrl (..)

    -- * DefaultCodeRepository
    , DefaultCodeRepository (..)

    -- * LifecycleConfigName
    , LifecycleConfigName (..)

    -- * ModelPackageDescription
    , ModelPackageDescription (..)

    -- * StringValue
    , StringValue (..)

    -- * TuningJobArn
    , TuningJobArn (..)

    -- * InitialActiveLearningModelArn
    , InitialActiveLearningModelArn (..)

    -- * SageMakerImageArn
    , SageMakerImageArn (..)

    -- * SageMakerImageVersionArn
    , SageMakerImageVersionArn (..)

    -- * TrialComponentName
    , TrialComponentName (..)

    -- * AdditionalCodeRepositoryEquals
    , AdditionalCodeRepositoryEquals (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * S3ModelArtifacts
    , S3ModelArtifacts (..)

    -- * UserPool
    , UserPool (..)

    -- * UserGroup
    , UserGroup (..)

    -- * DomainIdEquals
    , DomainIdEquals (..)

    -- * UserProfileNameContains
    , UserProfileNameContains (..)

    -- * TrainingImage
    , TrainingImage (..)

    -- * ResolvedImage
    , ResolvedImage (..)

    -- * SpecifiedImage
    , SpecifiedImage (..)

    -- * DefinitionName
    , DefinitionName (..)

    -- * AuthorizedUrl
    , AuthorizedUrl (..)

    -- * ExecutionRoleArn
    , ExecutionRoleArn (..)

    -- * HomeEfsFileSystemKmsKeyId
    , HomeEfsFileSystemKmsKeyId (..)

    -- * VolumeKmsKeyId
    , VolumeKmsKeyId (..)

    -- * PreHumanTaskLambdaArn
    , PreHumanTaskLambdaArn (..)

    -- * AnnotationConsolidationLambdaArn
    , AnnotationConsolidationLambdaArn (..)

    -- * TrainingImageDigest
    , TrainingImageDigest (..)

    -- * LocalPath
    , LocalPath (..)

    -- * SourceArn
    , SourceArn (..)

    -- * UiTemplateS3Uri
    , UiTemplateS3Uri (..)

    -- * ModelPackageName
    , ModelPackageName (..)

    -- * ManifestS3Uri
    , ManifestS3Uri (..)

    -- * CandidateNameEquals
    , CandidateNameEquals (..)

    -- * Input
    , Input (..)

    -- * S3OutputLocation
    , S3OutputLocation (..)

    -- * RuleEvaluationJobArn
    , RuleEvaluationJobArn (..)

    -- * PostAnalyticsProcessorSourceUri
    , PostAnalyticsProcessorSourceUri (..)

    -- * RecordPreprocessorSourceUri
    , RecordPreprocessorSourceUri (..)

    -- * RepositoryUrl
    , RepositoryUrl (..)

    -- * ValidationRole
    , ValidationRole (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.SageMaker.Types.EndpointConfigArn
  
import Network.AWS.SageMaker.Types.DomainStatus
  
import Network.AWS.SageMaker.Types.Parent
  
import Network.AWS.SageMaker.Types.TrainingJobStatusCounters
  
import Network.AWS.SageMaker.Types.UiTemplate
  
import Network.AWS.SageMaker.Types.LabelingJobArn
  
import Network.AWS.SageMaker.Types.CodeRepositoryContains
  
import Network.AWS.SageMaker.Types.MonitoringScheduleSortKey
  
import Network.AWS.SageMaker.Types.HyperParameterValue
  
import Network.AWS.SageMaker.Types.TransformJobName
  
import Network.AWS.SageMaker.Types.CandidateDefinitionNotebookLocation
  
import Network.AWS.SageMaker.Types.NameContains
  
import Network.AWS.SageMaker.Types.UserSettings
  
import Network.AWS.SageMaker.Types.NotebookInstanceSortOrder
  
import Network.AWS.SageMaker.Types.ModelPackageSortBy
  
import Network.AWS.SageMaker.Types.FailureReason
  
import Network.AWS.SageMaker.Types.ModelPackageStatusItem
  
import Network.AWS.SageMaker.Types.ProcessingOutput
  
import Network.AWS.SageMaker.Types.MonitoringScheduleArn
  
import Network.AWS.SageMaker.Types.LabelingJobOutputConfig
  
import Network.AWS.SageMaker.Types.HyperParameterTuningJobStrategyType
  
import Network.AWS.SageMaker.Types.Workforce
  
import Network.AWS.SageMaker.Types.OidcMemberDefinition
  
import Network.AWS.SageMaker.Types.ResourceLimits
  
import Network.AWS.SageMaker.Types.Framework
  
import Network.AWS.SageMaker.Types.ModelClientConfig
  
import Network.AWS.SageMaker.Types.HyperParameterTrainingJobSummary
  
import Network.AWS.SageMaker.Types.AppSpecification
  
import Network.AWS.SageMaker.Types.MediaType
  
import Network.AWS.SageMaker.Types.EndpointConfigSortKey
  
import Network.AWS.SageMaker.Types.ObjectiveStatusCounters
  
import Network.AWS.SageMaker.Types.FileSystemAccessMode
  
import Network.AWS.SageMaker.Types.String256
  
import Network.AWS.SageMaker.Types.LabelingJobDataSource
  
import Network.AWS.SageMaker.Types.EndpointName
  
import Network.AWS.SageMaker.Types.ClientId
  
import Network.AWS.SageMaker.Types.Workteam
  
  
import Network.AWS.SageMaker.Types.EntityName
  
import Network.AWS.SageMaker.Types.AutoMLContainerDefinition
  
import Network.AWS.SageMaker.Types.CsvContentType
  
import Network.AWS.SageMaker.Types.PaginationToken
  
import Network.AWS.SageMaker.Types.BatchStrategy
  
import Network.AWS.SageMaker.Types.TrialComponentParameterValue
  
import Network.AWS.SageMaker.Types.AlgorithmValidationProfile
  
import Network.AWS.SageMaker.Types.EndpointSummary
  
import Network.AWS.SageMaker.Types.Group
  
import Network.AWS.SageMaker.Types.ScheduleConfig
  
import Network.AWS.SageMaker.Types.SourceAlgorithmSpecification
  
import Network.AWS.SageMaker.Types.String200
  
import Network.AWS.SageMaker.Types.TuningJobCompletionCriteria
  
import Network.AWS.SageMaker.Types.ClientSecret
  
import Network.AWS.SageMaker.Types.NotebookInstanceName
  
import Network.AWS.SageMaker.Types.TensorBoardAppSettings
  
import Network.AWS.SageMaker.Types.KernelGatewayAppSettings
  
import Network.AWS.SageMaker.Types.AwsManagedHumanLoopRequestSource
  
import Network.AWS.SageMaker.Types.TrainingJob
  
import Network.AWS.SageMaker.Types.CaptureMode
  
import Network.AWS.SageMaker.Types.ImageContainerImage
  
import Network.AWS.SageMaker.Types.AutoMLSortBy
  
import Network.AWS.SageMaker.Types.ArnOrName
  
import Network.AWS.SageMaker.Types.CandidateStepType
  
import Network.AWS.SageMaker.Types.TransformJobSummary
  
import Network.AWS.SageMaker.Types.LabelingJobAlgorithmsConfig
  
import Network.AWS.SageMaker.Types.ResourceSpec
  
import Network.AWS.SageMaker.Types.ResourceId
  
import Network.AWS.SageMaker.Types.TrialArn
  
import Network.AWS.SageMaker.Types.HumanLoopActivationConditions
  
import Network.AWS.SageMaker.Types.TrialComponentSimpleSummary
  
import Network.AWS.SageMaker.Types.Image
  
import Network.AWS.SageMaker.Types.MonitoringInput
  
import Network.AWS.SageMaker.Types.CaptureContentTypeHeader
  
import Network.AWS.SageMaker.Types.Tag
  
import Network.AWS.SageMaker.Types.TrialComponentStatusMessage
  
import Network.AWS.SageMaker.Types.DataCaptureConfigSummary
  
import Network.AWS.SageMaker.Types.PropertyNameQuery
  
import Network.AWS.SageMaker.Types.FlowDefinitionTaskTitle
  
import Network.AWS.SageMaker.Types.CodeRepositoryArn
  
import Network.AWS.SageMaker.Types.ProcessingResources
  
import Network.AWS.SageMaker.Types.ImageDisplayName
  
import Network.AWS.SageMaker.Types.ModelArn
  
import Network.AWS.SageMaker.Types.Trial
  
import Network.AWS.SageMaker.Types.SnsTopicArn
  
import Network.AWS.SageMaker.Types.HyperParameterTuningJobArn
  
import Network.AWS.SageMaker.Types.ModelArtifacts
  
import Network.AWS.SageMaker.Types.HyperParameterScalingType
  
import Network.AWS.SageMaker.Types.USD
  
import Network.AWS.SageMaker.Types.CognitoMemberDefinition
  
import Network.AWS.SageMaker.Types.TemplateContentSha256
  
import Network.AWS.SageMaker.Types.DestinationS3Uri
  
import Network.AWS.SageMaker.Types.ModelSortKey
  
import Network.AWS.SageMaker.Types.CandidateStepArn
  
import Network.AWS.SageMaker.Types.CognitoUserPool
  
import Network.AWS.SageMaker.Types.ResourceType
  
import Network.AWS.SageMaker.Types.OidcEndpoint
  
import Network.AWS.SageMaker.Types.AutoMLSortOrder
  
import Network.AWS.SageMaker.Types.AlgorithmArn
  
import Network.AWS.SageMaker.Types.EnvironmentKey
  
import Network.AWS.SageMaker.Types.JobReferenceCodeContains
  
import Network.AWS.SageMaker.Types.ClientToken
  
import Network.AWS.SageMaker.Types.ParameterValue
  
import Network.AWS.SageMaker.Types.ResourcePropertyName
  
import Network.AWS.SageMaker.Types.LabelingJobSnsDataSource
  
import Network.AWS.SageMaker.Types.TargetPlatformOs
  
import Network.AWS.SageMaker.Types.TrainingJobDefinition
  
import Network.AWS.SageMaker.Types.TrialSourceArn
  
import Network.AWS.SageMaker.Types.UiTemplateInfo
  
import Network.AWS.SageMaker.Types.MonitoringExecutionSummary
  
import Network.AWS.SageMaker.Types.MonitoringScheduleSummary
  
import Network.AWS.SageMaker.Types.NotificationConfiguration
  
import Network.AWS.SageMaker.Types.ShuffleConfig
  
import Network.AWS.SageMaker.Types.ParameterRanges
  
import Network.AWS.SageMaker.Types.EntityDescription
  
import Network.AWS.SageMaker.Types.HyperParameterAlgorithmSpecification
  
import Network.AWS.SageMaker.Types.DeployedImage
  
import Network.AWS.SageMaker.Types.HyperParameterTrainingJobDefinition
  
import Network.AWS.SageMaker.Types.UserProfileName
  
import Network.AWS.SageMaker.Types.TrialComponent
  
import Network.AWS.SageMaker.Types.TrainingInputMode
  
import Network.AWS.SageMaker.Types.ImageConfig
  
import Network.AWS.SageMaker.Types.ModelPackageValidationProfile
  
import Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigContent
  
import Network.AWS.SageMaker.Types.EndpointConfigSummary
  
import Network.AWS.SageMaker.Types.AutoMLJobStatus
  
import Network.AWS.SageMaker.Types.HumanLoopActivationConditionsConfig
  
import Network.AWS.SageMaker.Types.TransformResources
  
import Network.AWS.SageMaker.Types.LabelingJobSummary
  
import Network.AWS.SageMaker.Types.TrialComponentPrimaryStatus
  
import Network.AWS.SageMaker.Types.ProcessingEnvironmentValue
  
import Network.AWS.SageMaker.Types.SourceType
  
import Network.AWS.SageMaker.Types.TrainingSpecification
  
import Network.AWS.SageMaker.Types.AppStatus
  
import Network.AWS.SageMaker.Types.ImageBaseImage
  
import Network.AWS.SageMaker.Types.RepositoryAccessMode
  
import Network.AWS.SageMaker.Types.JsonPath
  
import Network.AWS.SageMaker.Types.NotebookOutputOption
  
import Network.AWS.SageMaker.Types.PublicWorkforceTaskPrice
  
import Network.AWS.SageMaker.Types.ProcessingS3Input
  
import Network.AWS.SageMaker.Types.CategoricalParameterRangeSpecification
  
import Network.AWS.SageMaker.Types.DomainArn
  
import Network.AWS.SageMaker.Types.ImageVersion
  
import Network.AWS.SageMaker.Types.S3DataSource
  
import Network.AWS.SageMaker.Types.NotebookInstanceLifecycleHook
  
import Network.AWS.SageMaker.Types.IntegerParameterRangeSpecification
  
import Network.AWS.SageMaker.Types.TransformInstanceType
  
import Network.AWS.SageMaker.Types.TrialComponentMetricSummary
  
import Network.AWS.SageMaker.Types.MonitoringBaselineConfig
  
import Network.AWS.SageMaker.Types.ExperimentEntityName
  
import Network.AWS.SageMaker.Types.ModelName
  
import Network.AWS.SageMaker.Types.UserProfileDetails
  
import Network.AWS.SageMaker.Types.HyperParameterTuningJobName
  
import Network.AWS.SageMaker.Types.VpcId
  
import Network.AWS.SageMaker.Types.HyperParameterTuningJobObjective
  
import Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigNameContains
  
import Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSummary
  
import Network.AWS.SageMaker.Types.ModelPackageArn
  
import Network.AWS.SageMaker.Types.CaptureStatus
  
import Network.AWS.SageMaker.Types.EfsUid
  
import Network.AWS.SageMaker.Types.S3DataType
  
import Network.AWS.SageMaker.Types.VariantProperty
  
import Network.AWS.SageMaker.Types.HumanTaskConfig
  
import Network.AWS.SageMaker.Types.LambdaFunctionArn
  
import Network.AWS.SageMaker.Types.Accept
  
import Network.AWS.SageMaker.Types.TrialComponentArtifactValue
  
import Network.AWS.SageMaker.Types.Operator
  
import Network.AWS.SageMaker.Types.ContainerImage
  
import Network.AWS.SageMaker.Types.ProcessingStoppingCondition
  
import Network.AWS.SageMaker.Types.SubscribedWorkteam
  
import Network.AWS.SageMaker.Types.AppInstanceType
  
import Network.AWS.SageMaker.Types.SortTrialsBy
  
import Network.AWS.SageMaker.Types.CandidateStepName
  
import Network.AWS.SageMaker.Types.AlgorithmImage
  
import Network.AWS.SageMaker.Types.DebugHookConfig
  
import Network.AWS.SageMaker.Types.CheckpointConfig
  
import Network.AWS.SageMaker.Types.ModelSummary
  
import Network.AWS.SageMaker.Types.CodeRepositorySummary
  
import Network.AWS.SageMaker.Types.MetricName
  
import Network.AWS.SageMaker.Types.ProcessingJobName
  
import Network.AWS.SageMaker.Types.HyperParameterTuningJobSummary
  
import Network.AWS.SageMaker.Types.ImageStatus
  
import Network.AWS.SageMaker.Types.ProcessingClusterConfig
  
import Network.AWS.SageMaker.Types.ProcessingLocalPath
  
import Network.AWS.SageMaker.Types.MonitoringOutput
  
import Network.AWS.SageMaker.Types.ImageNameContains
  
import Network.AWS.SageMaker.Types.AppDetails
  
import Network.AWS.SageMaker.Types.StoppingCondition
  
import Network.AWS.SageMaker.Types.AlgorithmSummary
  
import Network.AWS.SageMaker.Types.ImageSortOrder
  
import Network.AWS.SageMaker.Types.MonitoringExecutionSortKey
  
import Network.AWS.SageMaker.Types.ProblemType
  
import Network.AWS.SageMaker.Types.AutoMLOutputDataConfig
  
import Network.AWS.SageMaker.Types.Url
  
import Network.AWS.SageMaker.Types.AutoMLJobConfig
  
import Network.AWS.SageMaker.Types.AppImageConfigName
  
import Network.AWS.SageMaker.Types.AuthMode
  
import Network.AWS.SageMaker.Types.AutoMLDataSource
  
import Network.AWS.SageMaker.Types.Channel
  
import Network.AWS.SageMaker.Types.ObjectiveStatus
  
import Network.AWS.SageMaker.Types.FlowDefinitionTaskKeyword
  
import Network.AWS.SageMaker.Types.TrainingJobStatus
  
import Network.AWS.SageMaker.Types.TaskDescription
  
import Network.AWS.SageMaker.Types.FlowDefinitionStatus
  
import Network.AWS.SageMaker.Types.ExperimentConfig
  
import Network.AWS.SageMaker.Types.AppName
  
import Network.AWS.SageMaker.Types.NetworkInterfaceId
  
import Network.AWS.SageMaker.Types.UiConfig
  
import Network.AWS.SageMaker.Types.NestedFilters
  
import Network.AWS.SageMaker.Types.ImageSortBy
  
import Network.AWS.SageMaker.Types.SuggestionQuery
  
import Network.AWS.SageMaker.Types.LabelingJobAlgorithmSpecificationArn
  
import Network.AWS.SageMaker.Types.AutoMLJobName
  
import Network.AWS.SageMaker.Types.UserContext
  
import Network.AWS.SageMaker.Types.AssemblyType
  
import Network.AWS.SageMaker.Types.AutoMLJobCompletionCriteria
  
import Network.AWS.SageMaker.Types.ExecutionStatus
  
import Network.AWS.SageMaker.Types.ParameterKey
  
import Network.AWS.SageMaker.Types.FileSystemId
  
import Network.AWS.SageMaker.Types.InferenceSpecification
  
import Network.AWS.SageMaker.Types.TargetPlatform
  
import Network.AWS.SageMaker.Types.FileSystemDataSource
  
import Network.AWS.SageMaker.Types.SourceIpConfig
  
import Network.AWS.SageMaker.Types.PresignedDomainUrl
  
import Network.AWS.SageMaker.Types.ParameterRange
  
import Network.AWS.SageMaker.Types.HyperParameterTuningJobSortByOptions
  
import Network.AWS.SageMaker.Types.ListCompilationJobsSortBy
  
import Network.AWS.SageMaker.Types.TransformJobDefinition
  
import Network.AWS.SageMaker.Types.ContentClassifier
  
import Network.AWS.SageMaker.Types.EnvironmentValue
  
import Network.AWS.SageMaker.Types.KernelSpec
  
import Network.AWS.SageMaker.Types.AutoMLJobObjective
  
import Network.AWS.SageMaker.Types.UserProfileStatus
  
  
import Network.AWS.SageMaker.Types.AutoMLS3DataType
  
import Network.AWS.SageMaker.Types.SubnetId
  
import Network.AWS.SageMaker.Types.ContinuousParameterRangeSpecification
  
import Network.AWS.SageMaker.Types.StatusDetails
  
import Network.AWS.SageMaker.Types.WorkforceArn
  
import Network.AWS.SageMaker.Types.SecondaryStatusTransition
  
import Network.AWS.SageMaker.Types.CollectionName
  
import Network.AWS.SageMaker.Types.WorkteamArn
  
import Network.AWS.SageMaker.Types.AutoMLCandidateStep
  
import Network.AWS.SageMaker.Types.HumanLoopConfig
  
import Network.AWS.SageMaker.Types.TrialComponentStatus
  
import Network.AWS.SageMaker.Types.LabelCounters
  
import Network.AWS.SageMaker.Types.ScheduleStatus
  
import Network.AWS.SageMaker.Types.ParameterType
  
import Network.AWS.SageMaker.Types.ImageVersionStatus
  
import Network.AWS.SageMaker.Types.LabelingJobResourceConfig
  
  
import Network.AWS.SageMaker.Types.MonitoringOutputConfig
  
import Network.AWS.SageMaker.Types.InstanceType
  
import Network.AWS.SageMaker.Types.ImageVersionSortOrder
  
import Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSortKey
  
import Network.AWS.SageMaker.Types.TransformS3DataSource
  
import Network.AWS.SageMaker.Types.ModelPackageSummary
  
import Network.AWS.SageMaker.Types.ConfigValue
  
import Network.AWS.SageMaker.Types.HumanTaskUiName
  
import Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigArn
  
import Network.AWS.SageMaker.Types.CandidateStatus
  
import Network.AWS.SageMaker.Types.ImageUri
  
import Network.AWS.SageMaker.Types.TrialComponentSourceDetail
  
import Network.AWS.SageMaker.Types.Experiment
  
import Network.AWS.SageMaker.Types.TargetPlatformAccelerator
  
import Network.AWS.SageMaker.Types.FileSystemConfig
  
import Network.AWS.SageMaker.Types.InputConfig
  
import Network.AWS.SageMaker.Types.SecurityGroupId
  
import Network.AWS.SageMaker.Types.NetworkConfig
  
import Network.AWS.SageMaker.Types.AccountId
  
import Network.AWS.SageMaker.Types.ProductionVariant
  
import Network.AWS.SageMaker.Types.NotebookInstanceStatus
  
import Network.AWS.SageMaker.Types.PropertyNameSuggestion
  
import Network.AWS.SageMaker.Types.Branch
  
import Network.AWS.SageMaker.Types.ProcessingS3Output
  
import Network.AWS.SageMaker.Types.ExperimentSource
  
import Network.AWS.SageMaker.Types.ListWorkforcesSortByOptions
  
import Network.AWS.SageMaker.Types.NextToken
  
import Network.AWS.SageMaker.Types.RetentionPolicy
  
import Network.AWS.SageMaker.Types.VersionedArnOrName
  
import Network.AWS.SageMaker.Types.RetentionType
  
import Network.AWS.SageMaker.Types.BooleanOperator
  
import Network.AWS.SageMaker.Types.AppSortKey
  
import Network.AWS.SageMaker.Types.CandidateSortBy
  
import Network.AWS.SageMaker.Types.ChannelName
  
import Network.AWS.SageMaker.Types.DirectoryPath
  
import Network.AWS.SageMaker.Types.Cidr
  
import Network.AWS.SageMaker.Types.SearchExpression
  
import Network.AWS.SageMaker.Types.JupyterServerAppSettings
  
import Network.AWS.SageMaker.Types.ContainerArgument
  
import Network.AWS.SageMaker.Types.LabelingJobS3DataSource
  
import Network.AWS.SageMaker.Types.RuleEvaluationStatus
  
import Network.AWS.SageMaker.Types.ExperimentArn
  
import Network.AWS.SageMaker.Types.ListWorkteamsSortByOptions
  
import Network.AWS.SageMaker.Types.CustomImage
  
import Network.AWS.SageMaker.Types.ProcessingJob
  
import Network.AWS.SageMaker.Types.DataInputConfig
  
import Network.AWS.SageMaker.Types.AutoMLJobArtifacts
  
import Network.AWS.SageMaker.Types.HyperParameterTuningJobWarmStartConfig
  
import Network.AWS.SageMaker.Types.AppArn
  
import Network.AWS.SageMaker.Types.NotebookInstanceSummary
  
import Network.AWS.SageMaker.Types.MetricData
  
import Network.AWS.SageMaker.Types.AutoMLJobArn
  
import Network.AWS.SageMaker.Types.SortOrder
  
import Network.AWS.SageMaker.Types.TaskKeyword
  
import Network.AWS.SageMaker.Types.TrainingJobEarlyStoppingType
  
import Network.AWS.SageMaker.Types.ContainerMode
  
import Network.AWS.SageMaker.Types.ImageVersionSortBy
  
import Network.AWS.SageMaker.Types.MonitoringClusterConfig
  
import Network.AWS.SageMaker.Types.VpcConfig
  
import Network.AWS.SageMaker.Types.ImageDigest
  
import Network.AWS.SageMaker.Types.AutoMLJobSecondaryStatus
  
import Network.AWS.SageMaker.Types.TrialSource
  
import Network.AWS.SageMaker.Types.NotebookInstanceAcceleratorType
  
import Network.AWS.SageMaker.Types.KmsKeyId
  
import Network.AWS.SageMaker.Types.ResourceArn
  
import Network.AWS.SageMaker.Types.ResponseMIMEType
  
import Network.AWS.SageMaker.Types.DomainName
  
import Network.AWS.SageMaker.Types.TransformJobStatus
  
import Network.AWS.SageMaker.Types.ModelPackageContainerDefinition
  
import Network.AWS.SageMaker.Types.DebugRuleConfiguration
  
import Network.AWS.SageMaker.Types.StatusMessage
  
import Network.AWS.SageMaker.Types.ScheduleExpression
  
import Network.AWS.SageMaker.Types.DesiredWeightAndCapacity
  
import Network.AWS.SageMaker.Types.HumanLoopRequestSource
  
import Network.AWS.SageMaker.Types.AnnotationConsolidationConfig
  
import Network.AWS.SageMaker.Types.SortTrialComponentsBy
  
import Network.AWS.SageMaker.Types.EndpointStatus
  
import Network.AWS.SageMaker.Types.UserProfileSortKey
  
import Network.AWS.SageMaker.Types.JoinSource
  
import Network.AWS.SageMaker.Types.CaptureOption
  
import Network.AWS.SageMaker.Types.ResolvedAttributes
  
import Network.AWS.SageMaker.Types.MonitoringStoppingCondition
  
import Network.AWS.SageMaker.Types.UserProfileArn
  
import Network.AWS.SageMaker.Types.MonitoringS3Uri
  
import Network.AWS.SageMaker.Types.SourceAlgorithm
  
import Network.AWS.SageMaker.Types.SearchSortOrder
  
import Network.AWS.SageMaker.Types.CompilationJobStatus
  
import Network.AWS.SageMaker.Types.FlowDefinitionArn
  
import Network.AWS.SageMaker.Types.DomainDetails
  
import Network.AWS.SageMaker.Types.TransformInput
  
import Network.AWS.SageMaker.Types.RootAccess
  
import Network.AWS.SageMaker.Types.HumanTaskUiSummary
  
import Network.AWS.SageMaker.Types.TrainingJobArn
  
import Network.AWS.SageMaker.Types.AlgorithmSpecification
  
import Network.AWS.SageMaker.Types.AutoMLNameContains
  
import Network.AWS.SageMaker.Types.TargetAttributeName
  
import Network.AWS.SageMaker.Types.GitConfigForUpdate
  
import Network.AWS.SageMaker.Types.LabelingJobForWorkteamSummary
  
import Network.AWS.SageMaker.Types.FlowDefinitionOutputConfig
  
import Network.AWS.SageMaker.Types.CandidateName
  
import Network.AWS.SageMaker.Types.TransformEnvironmentValue
  
import Network.AWS.SageMaker.Types.ContainerDefinition
  
import Network.AWS.SageMaker.Types.SplitType
  
import Network.AWS.SageMaker.Types.AutoMLFailureReason
  
import Network.AWS.SageMaker.Types.HyperParameterSpecification
  
import Network.AWS.SageMaker.Types.RenderingError
  
import Network.AWS.SageMaker.Types.AutoMLSecurityConfig
  
import Network.AWS.SageMaker.Types.DirectInternetAccess
  
import Network.AWS.SageMaker.Types.ProcessingInstanceType
  
import Network.AWS.SageMaker.Types.HumanTaskUiStatus
  
import Network.AWS.SageMaker.Types.CompilationJobSummary
  
import Network.AWS.SageMaker.Types.ContainerEntrypointString
  
import Network.AWS.SageMaker.Types.S3DataDistribution
  
import Network.AWS.SageMaker.Types.SingleSignOnUserIdentifier
  
import Network.AWS.SageMaker.Types.ImageArn
  
import Network.AWS.SageMaker.Types.LabelAttributeName
  
import Network.AWS.SageMaker.Types.SharingSettings
  
import Network.AWS.SageMaker.Types.LabelingJobStatus
  
import Network.AWS.SageMaker.Types.MonitoringResources
  
import Network.AWS.SageMaker.Types.RenderableTask
  
import Network.AWS.SageMaker.Types.OutputConfig
  
import Network.AWS.SageMaker.Types.ProcessingInput
  
import Network.AWS.SageMaker.Types.DebugRuleEvaluationStatus
  
import Network.AWS.SageMaker.Types.DataSource
  
import Network.AWS.SageMaker.Types.CognitoConfig
  
import Network.AWS.SageMaker.Types.OutputDataConfig
  
import Network.AWS.SageMaker.Types.NotebookInstanceSortKey
  
import Network.AWS.SageMaker.Types.MemberDefinition
  
import Network.AWS.SageMaker.Types.TaskTitle
  
import Network.AWS.SageMaker.Types.ConfigKey
  
import Network.AWS.SageMaker.Types.EndpointConfigNameContains
  
import Network.AWS.SageMaker.Types.ExperimentSummary
  
import Network.AWS.SageMaker.Types.ProductionVariantInstanceType
  
import Network.AWS.SageMaker.Types.LabelingJobInputConfig
  
import Network.AWS.SageMaker.Types.AutoMLJobSummary
  
import Network.AWS.SageMaker.Types.HyperParameterTuningJobConfig
  
import Network.AWS.SageMaker.Types.EndpointInput
  
import Network.AWS.SageMaker.Types.NotebookInstanceArn
  
import Network.AWS.SageMaker.Types.ProcessingS3CompressionType
  
import Network.AWS.SageMaker.Types.TrialComponentSourceArn
  
import Network.AWS.SageMaker.Types.AutoMLChannel
  
import Network.AWS.SageMaker.Types.DataExplorationNotebookLocation
  
import Network.AWS.SageMaker.Types.DomainId
  
import Network.AWS.SageMaker.Types.HumanLoopActivationConfig
  
import Network.AWS.SageMaker.Types.TagKey
  
import Network.AWS.SageMaker.Types.ContainerHostname
  
import Network.AWS.SageMaker.Types.VariantPropertyType
  
import Network.AWS.SageMaker.Types.IntegerParameterRange
  
import Network.AWS.SageMaker.Types.HyperParameterTuningJobObjectiveType
  
import Network.AWS.SageMaker.Types.TrainingJobName
  
import Network.AWS.SageMaker.Types.ProcessingJobSummary
  
import Network.AWS.SageMaker.Types.ProductionVariantSummary
  
import Network.AWS.SageMaker.Types.CategoricalParameterRange
  
import Network.AWS.SageMaker.Types.MonitoringAppSpecification
  
import Network.AWS.SageMaker.Types.MountPath
  
import Network.AWS.SageMaker.Types.TrialComponentKey64
  
import Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSortOrder
  
import Network.AWS.SageMaker.Types.VariantName
  
import Network.AWS.SageMaker.Types.ImageVersionArn
  
import Network.AWS.SageMaker.Types.ResourceConfig
  
import Network.AWS.SageMaker.Types.Filter
  
import Network.AWS.SageMaker.Types.FlowDefinitionName
  
import Network.AWS.SageMaker.Types.FinalHyperParameterTuningJobObjectiveMetric
  
import Network.AWS.SageMaker.Types.TrialComponentArn
  
import Network.AWS.SageMaker.Types.GitConfig
  
import Network.AWS.SageMaker.Types.ImageName
  
import Network.AWS.SageMaker.Types.HyperParameterKey
  
import Network.AWS.SageMaker.Types.MonitoringConstraintsResource
  
import Network.AWS.SageMaker.Types.ProcessingJobStatus
  
import Network.AWS.SageMaker.Types.TrialComponentSource
  
import Network.AWS.SageMaker.Types.WorkteamName
  
import Network.AWS.SageMaker.Types.ContinuousParameterRange
  
import Network.AWS.SageMaker.Types.DetailedAlgorithmStatus
  
import Network.AWS.SageMaker.Types.TemplateUrl
  
import Network.AWS.SageMaker.Types.MonitoringStatisticsResource
  
import Network.AWS.SageMaker.Types.TrialComponentKey256
  
import Network.AWS.SageMaker.Types.AlgorithmStatus
  
import Network.AWS.SageMaker.Types.ProductionVariantAcceleratorType
  
import Network.AWS.SageMaker.Types.FlowDefinitionSummary
  
import Network.AWS.SageMaker.Types.PropertyNameHint
  
import Network.AWS.SageMaker.Types.TransformOutput
  
import Network.AWS.SageMaker.Types.ExitMessage
  
import Network.AWS.SageMaker.Types.ParentHyperParameterTuningJob
  
import Network.AWS.SageMaker.Types.AppNetworkAccessType
  
import Network.AWS.SageMaker.Types.CodeRepositorySortOrder
  
import Network.AWS.SageMaker.Types.OidcConfig
  
import Network.AWS.SageMaker.Types.CompilationJobArn
  
import Network.AWS.SageMaker.Types.SearchRecord
  
import Network.AWS.SageMaker.Types.HyperParameterTuningJobStatus
  
import Network.AWS.SageMaker.Types.ModelPackageStatusDetails
  
import Network.AWS.SageMaker.Types.MonitoringS3Output
  
import Network.AWS.SageMaker.Types.AttributeName
  
import Network.AWS.SageMaker.Types.TrainingJobSummary
  
import Network.AWS.SageMaker.Types.TrialComponentArtifact
  
import Network.AWS.SageMaker.Types.WorkforceName
  
import Network.AWS.SageMaker.Types.OidcConfigForResponse
  
import Network.AWS.SageMaker.Types.DataProcessing
  
import Network.AWS.SageMaker.Types.ProcessingS3UploadMode
  
import Network.AWS.SageMaker.Types.HumanTaskUiArn
  
import Network.AWS.SageMaker.Types.AutoMLCandidate
  
import Network.AWS.SageMaker.Types.SecretArn
  
import Network.AWS.SageMaker.Types.SortExperimentsBy
  
import Network.AWS.SageMaker.Types.ProcessingS3DataType
  
import Network.AWS.SageMaker.Types.TransformJob
  
import Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigName
  
import Network.AWS.SageMaker.Types.MonitoringScheduleConfig
  
import Network.AWS.SageMaker.Types.CompilerOptions
  
import Network.AWS.SageMaker.Types.FileSystemType
  
import Network.AWS.SageMaker.Types.ModelPackageValidationSpecification
  
import Network.AWS.SageMaker.Types.AutoMLS3DataSource
  
import Network.AWS.SageMaker.Types.TargetPlatformArch
  
import Network.AWS.SageMaker.Types.LabelingJobName
  
import Network.AWS.SageMaker.Types.CompressionType
  
import Network.AWS.SageMaker.Types.KernelGatewayImageConfig
  
import Network.AWS.SageMaker.Types.AlgorithmStatusItem
  
import Network.AWS.SageMaker.Types.ChannelSpecification
  
import Network.AWS.SageMaker.Types.TransformEnvironmentKey
  
import Network.AWS.SageMaker.Types.EndpointConfigName
  
import Network.AWS.SageMaker.Types.ProcessingOutputConfig
  
import Network.AWS.SageMaker.Types.AlgorithmSortBy
  
import Network.AWS.SageMaker.Types.AppImageConfigDetails
  
import Network.AWS.SageMaker.Types.S3Uri
  
import Network.AWS.SageMaker.Types.AutoMLJobObjectiveType
  
import Network.AWS.SageMaker.Types.TensorBoardOutputConfig
  
import Network.AWS.SageMaker.Types.MonitoringScheduleName
  
import Network.AWS.SageMaker.Types.TrialSummary
  
import Network.AWS.SageMaker.Types.CodeRepositoryNameOrUrl
  
import Network.AWS.SageMaker.Types.MonitoringJobDefinition
  
import Network.AWS.SageMaker.Types.LabelingJobDataAttributes
  
import Network.AWS.SageMaker.Types.FinalAutoMLJobObjectiveMetric
  
import Network.AWS.SageMaker.Types.EndpointArn
  
import Network.AWS.SageMaker.Types.ListLabelingJobsForWorkteamSortByOptions
  
import Network.AWS.SageMaker.Types.CodeRepositorySortBy
  
import Network.AWS.SageMaker.Types.HyperParameterTuningJobWarmStartType
  
import Network.AWS.SageMaker.Types.CollectionConfiguration
  
import Network.AWS.SageMaker.Types.RecordWrapper
  
import Network.AWS.SageMaker.Types.SecondaryStatus
  
import Network.AWS.SageMaker.Types.LabelingJobOutput
  
import Network.AWS.SageMaker.Types.EndpointSortKey
  
import Network.AWS.SageMaker.Types.TransformJobArn
  
import Network.AWS.SageMaker.Types.ProductId
  
import Network.AWS.SageMaker.Types.LabelCountersForWorkteam
  
import Network.AWS.SageMaker.Types.DataCaptureConfig
  
import Network.AWS.SageMaker.Types.TrainingJobSortByOptions
  
import Network.AWS.SageMaker.Types.ProcessingS3InputMode
  
import Network.AWS.SageMaker.Types.AlgorithmValidationSpecification
  
import Network.AWS.SageMaker.Types.NotificationTopicArn
  
import Network.AWS.SageMaker.Types.AppImageConfigSortKey
  
import Network.AWS.SageMaker.Types.TargetDevice
  
import Network.AWS.SageMaker.Types.TransformDataSource
  
import Network.AWS.SageMaker.Types.AutoMLMetricEnum
  
import Network.AWS.SageMaker.Types.SortBy
  
import Network.AWS.SageMaker.Types.RuleConfigurationName
  
import Network.AWS.SageMaker.Types.ProcessingS3DataDistributionType
  
import Network.AWS.SageMaker.Types.AlgorithmStatusDetails
  
import Network.AWS.SageMaker.Types.MetricRegex
  
import Network.AWS.SageMaker.Types.ContentType
  
import Network.AWS.SageMaker.Types.ProcessingJobArn
  
import Network.AWS.SageMaker.Types.LabelingJobStoppingConditions
  
import Network.AWS.SageMaker.Types.DetailedModelPackageStatus
  
import Network.AWS.SageMaker.Types.ModelPackageStatus
  
import Network.AWS.SageMaker.Types.JobReferenceCode
  
import Network.AWS.SageMaker.Types.TrialComponentSummary
  
import Network.AWS.SageMaker.Types.MetricDefinition
  
import Network.AWS.SageMaker.Types.ImageDeleteProperty
  
import Network.AWS.SageMaker.Types.JsonContentType
  
import Network.AWS.SageMaker.Types.TrainingInstanceType
  
import Network.AWS.SageMaker.Types.AppType
  
import Network.AWS.SageMaker.Types.AppImageConfigArn
  
import Network.AWS.SageMaker.Types.RoleArn
  
import Network.AWS.SageMaker.Types.ProcessingEnvironmentKey
  
import Network.AWS.SageMaker.Types.OrderKey
  
  
import Network.AWS.SageMaker.Types.AlgorithmName
  
import Network.AWS.SageMaker.Types.ExperimentName
  
import Network.AWS.SageMaker.Types.TrialName
  
import Network.AWS.SageMaker.Types.Content
  
import Network.AWS.SageMaker.Types.LabelCategoryConfigS3Uri
  
import Network.AWS.SageMaker.Types.ExecutionRole
  
import Network.AWS.SageMaker.Types.Name
  
import Network.AWS.SageMaker.Types.S3OutputPath
  
import Network.AWS.SageMaker.Types.DisplayName
  
import Network.AWS.SageMaker.Types.TrainingJobDefinitionName
  
import Network.AWS.SageMaker.Types.TuningJobName
  
import Network.AWS.SageMaker.Types.Description
  
import Network.AWS.SageMaker.Types.ModelDataUrl
  
import Network.AWS.SageMaker.Types.DefaultCodeRepository
  
import Network.AWS.SageMaker.Types.LifecycleConfigName
  
import Network.AWS.SageMaker.Types.ModelPackageDescription
  
import Network.AWS.SageMaker.Types.StringValue
  
import Network.AWS.SageMaker.Types.TuningJobArn
  
import Network.AWS.SageMaker.Types.InitialActiveLearningModelArn
  
import Network.AWS.SageMaker.Types.SageMakerImageArn
  
import Network.AWS.SageMaker.Types.SageMakerImageVersionArn
  
import Network.AWS.SageMaker.Types.TrialComponentName
  
import Network.AWS.SageMaker.Types.AdditionalCodeRepositoryEquals
  
import Network.AWS.SageMaker.Types.Key
  
import Network.AWS.SageMaker.Types.Value
  
import Network.AWS.SageMaker.Types.S3ModelArtifacts
  
import Network.AWS.SageMaker.Types.UserPool
  
import Network.AWS.SageMaker.Types.UserGroup
  
import Network.AWS.SageMaker.Types.DomainIdEquals
  
import Network.AWS.SageMaker.Types.UserProfileNameContains
  
import Network.AWS.SageMaker.Types.TrainingImage
  
import Network.AWS.SageMaker.Types.ResolvedImage
  
import Network.AWS.SageMaker.Types.SpecifiedImage
  
import Network.AWS.SageMaker.Types.DefinitionName
  
import Network.AWS.SageMaker.Types.AuthorizedUrl
  
import Network.AWS.SageMaker.Types.ExecutionRoleArn
  
import Network.AWS.SageMaker.Types.HomeEfsFileSystemKmsKeyId
  
import Network.AWS.SageMaker.Types.VolumeKmsKeyId
  
import Network.AWS.SageMaker.Types.PreHumanTaskLambdaArn
  
import Network.AWS.SageMaker.Types.AnnotationConsolidationLambdaArn
  
import Network.AWS.SageMaker.Types.TrainingImageDigest
  
import Network.AWS.SageMaker.Types.LocalPath
  
import Network.AWS.SageMaker.Types.SourceArn
  
import Network.AWS.SageMaker.Types.UiTemplateS3Uri
  
import Network.AWS.SageMaker.Types.ModelPackageName
  
import Network.AWS.SageMaker.Types.ManifestS3Uri
  
import Network.AWS.SageMaker.Types.CandidateNameEquals
  
import Network.AWS.SageMaker.Types.Input
  
import Network.AWS.SageMaker.Types.S3OutputLocation
  
import Network.AWS.SageMaker.Types.RuleEvaluationJobArn
  
import Network.AWS.SageMaker.Types.PostAnalyticsProcessorSourceUri
  
import Network.AWS.SageMaker.Types.RecordPreprocessorSourceUri
  
import Network.AWS.SageMaker.Types.RepositoryUrl
  
import Network.AWS.SageMaker.Types.ValidationRole
  

-- | API version @2017-07-24@ of the Amazon SageMaker Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "SageMaker",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "api.sagemaker",
                 Core._svcVersion = "2017-07-24", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "SageMaker",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | You have exceeded an Amazon SageMaker resource limit. For example, you might have too many training jobs created. 
_ResourceLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceLimitExceeded
  = Core._MatchServiceError mkServiceConfig "ResourceLimitExceeded"
{-# INLINEABLE _ResourceLimitExceeded #-}
{-# DEPRECATED _ResourceLimitExceeded "Use generic-lens or generic-optics instead"  #-}

-- | Resource being accessed is in use.
_ResourceInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUse
  = Core._MatchServiceError mkServiceConfig "ResourceInUse"
{-# INLINEABLE _ResourceInUse #-}
{-# DEPRECATED _ResourceInUse "Use generic-lens or generic-optics instead"  #-}

-- | There was a conflict when you attempted to modify an experiment, trial, or trial component.
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException
  = Core._MatchServiceError mkServiceConfig "ConflictException"
{-# INLINEABLE _ConflictException #-}
{-# DEPRECATED _ConflictException "Use generic-lens or generic-optics instead"  #-}

-- | Resource being access is not found.
_ResourceNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFound
  = Core._MatchServiceError mkServiceConfig "ResourceNotFound"
{-# INLINEABLE _ResourceNotFound #-}
{-# DEPRECATED _ResourceNotFound "Use generic-lens or generic-optics instead"  #-}

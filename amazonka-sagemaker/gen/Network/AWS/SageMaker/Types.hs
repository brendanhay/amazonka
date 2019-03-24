{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types
    (
    -- * Service Configuration
      sageMaker

    -- * Errors
    , _ResourceLimitExceeded
    , _ResourceInUse
    , _ResourceNotFound

    -- * AlgorithmSortBy
    , AlgorithmSortBy (..)

    -- * AlgorithmStatus
    , AlgorithmStatus (..)

    -- * AssemblyType
    , AssemblyType (..)

    -- * BatchStrategy
    , BatchStrategy (..)

    -- * BooleanOperator
    , BooleanOperator (..)

    -- * CodeRepositorySortBy
    , CodeRepositorySortBy (..)

    -- * CodeRepositorySortOrder
    , CodeRepositorySortOrder (..)

    -- * CompilationJobStatus
    , CompilationJobStatus (..)

    -- * CompressionType
    , CompressionType (..)

    -- * ContentClassifier
    , ContentClassifier (..)

    -- * DetailedAlgorithmStatus
    , DetailedAlgorithmStatus (..)

    -- * DetailedModelPackageStatus
    , DetailedModelPackageStatus (..)

    -- * DirectInternetAccess
    , DirectInternetAccess (..)

    -- * EndpointConfigSortKey
    , EndpointConfigSortKey (..)

    -- * EndpointSortKey
    , EndpointSortKey (..)

    -- * EndpointStatus
    , EndpointStatus (..)

    -- * Framework
    , Framework (..)

    -- * HyperParameterScalingType
    , HyperParameterScalingType (..)

    -- * HyperParameterTuningJobObjectiveType
    , HyperParameterTuningJobObjectiveType (..)

    -- * HyperParameterTuningJobSortByOptions
    , HyperParameterTuningJobSortByOptions (..)

    -- * HyperParameterTuningJobStatus
    , HyperParameterTuningJobStatus (..)

    -- * HyperParameterTuningJobStrategyType
    , HyperParameterTuningJobStrategyType (..)

    -- * HyperParameterTuningJobWarmStartType
    , HyperParameterTuningJobWarmStartType (..)

    -- * InstanceType
    , InstanceType (..)

    -- * LabelingJobStatus
    , LabelingJobStatus (..)

    -- * ListCompilationJobsSortBy
    , ListCompilationJobsSortBy (..)

    -- * ListLabelingJobsForWorkteamSortByOptions
    , ListLabelingJobsForWorkteamSortByOptions (..)

    -- * ListWorkteamsSortByOptions
    , ListWorkteamsSortByOptions (..)

    -- * ModelPackageSortBy
    , ModelPackageSortBy (..)

    -- * ModelPackageStatus
    , ModelPackageStatus (..)

    -- * ModelSortKey
    , ModelSortKey (..)

    -- * NotebookInstanceAcceleratorType
    , NotebookInstanceAcceleratorType (..)

    -- * NotebookInstanceLifecycleConfigSortKey
    , NotebookInstanceLifecycleConfigSortKey (..)

    -- * NotebookInstanceLifecycleConfigSortOrder
    , NotebookInstanceLifecycleConfigSortOrder (..)

    -- * NotebookInstanceSortKey
    , NotebookInstanceSortKey (..)

    -- * NotebookInstanceSortOrder
    , NotebookInstanceSortOrder (..)

    -- * NotebookInstanceStatus
    , NotebookInstanceStatus (..)

    -- * ObjectiveStatus
    , ObjectiveStatus (..)

    -- * Operator
    , Operator (..)

    -- * OrderKey
    , OrderKey (..)

    -- * ParameterType
    , ParameterType (..)

    -- * ProductionVariantAcceleratorType
    , ProductionVariantAcceleratorType (..)

    -- * ProductionVariantInstanceType
    , ProductionVariantInstanceType (..)

    -- * RecordWrapper
    , RecordWrapper (..)

    -- * ResourceType
    , ResourceType (..)

    -- * RootAccess
    , RootAccess (..)

    -- * S3DataDistribution
    , S3DataDistribution (..)

    -- * S3DataType
    , S3DataType (..)

    -- * SearchSortOrder
    , SearchSortOrder (..)

    -- * SecondaryStatus
    , SecondaryStatus (..)

    -- * SortBy
    , SortBy (..)

    -- * SortOrder
    , SortOrder (..)

    -- * SplitType
    , SplitType (..)

    -- * TargetDevice
    , TargetDevice (..)

    -- * TrainingInputMode
    , TrainingInputMode (..)

    -- * TrainingInstanceType
    , TrainingInstanceType (..)

    -- * TrainingJobEarlyStoppingType
    , TrainingJobEarlyStoppingType (..)

    -- * TrainingJobSortByOptions
    , TrainingJobSortByOptions (..)

    -- * TrainingJobStatus
    , TrainingJobStatus (..)

    -- * TransformInstanceType
    , TransformInstanceType (..)

    -- * TransformJobStatus
    , TransformJobStatus (..)

    -- * AlgorithmSpecification
    , AlgorithmSpecification
    , algorithmSpecification
    , asAlgorithmName
    , asTrainingImage
    , asMetricDefinitions
    , asTrainingInputMode

    -- * AlgorithmStatusDetails
    , AlgorithmStatusDetails
    , algorithmStatusDetails
    , asdImageScanStatuses
    , asdValidationStatuses

    -- * AlgorithmStatusItem
    , AlgorithmStatusItem
    , algorithmStatusItem
    , asiFailureReason
    , asiName
    , asiStatus

    -- * AlgorithmSummary
    , AlgorithmSummary
    , algorithmSummary
    , aAlgorithmDescription
    , aAlgorithmName
    , aAlgorithmARN
    , aCreationTime
    , aAlgorithmStatus

    -- * AlgorithmValidationProfile
    , AlgorithmValidationProfile
    , algorithmValidationProfile
    , avpTransformJobDefinition
    , avpProfileName
    , avpTrainingJobDefinition

    -- * AlgorithmValidationSpecification
    , AlgorithmValidationSpecification
    , algorithmValidationSpecification
    , avsValidationRole
    , avsValidationProfiles

    -- * AnnotationConsolidationConfig
    , AnnotationConsolidationConfig
    , annotationConsolidationConfig
    , accAnnotationConsolidationLambdaARN

    -- * CategoricalParameterRange
    , CategoricalParameterRange
    , categoricalParameterRange
    , cprName
    , cprValues

    -- * CategoricalParameterRangeSpecification
    , CategoricalParameterRangeSpecification
    , categoricalParameterRangeSpecification
    , cprsValues

    -- * Channel
    , Channel
    , channel
    , cShuffleConfig
    , cRecordWrapperType
    , cInputMode
    , cCompressionType
    , cContentType
    , cChannelName
    , cDataSource

    -- * ChannelSpecification
    , ChannelSpecification
    , channelSpecification
    , csSupportedCompressionTypes
    , csIsRequired
    , csDescription
    , csName
    , csSupportedContentTypes
    , csSupportedInputModes

    -- * CodeRepositorySummary
    , CodeRepositorySummary
    , codeRepositorySummary
    , crsGitConfig
    , crsCodeRepositoryName
    , crsCodeRepositoryARN
    , crsCreationTime
    , crsLastModifiedTime

    -- * CognitoMemberDefinition
    , CognitoMemberDefinition
    , cognitoMemberDefinition
    , cmdUserPool
    , cmdUserGroup
    , cmdClientId

    -- * CompilationJobSummary
    , CompilationJobSummary
    , compilationJobSummary
    , cjsCompilationStartTime
    , cjsLastModifiedTime
    , cjsCompilationEndTime
    , cjsCompilationJobName
    , cjsCompilationJobARN
    , cjsCreationTime
    , cjsCompilationTargetDevice
    , cjsCompilationJobStatus

    -- * ContainerDefinition
    , ContainerDefinition
    , containerDefinition
    , cdModelDataURL
    , cdImage
    , cdModelPackageName
    , cdEnvironment
    , cdContainerHostname

    -- * ContinuousParameterRange
    , ContinuousParameterRange
    , continuousParameterRange
    , cScalingType
    , cName
    , cMinValue
    , cMaxValue

    -- * ContinuousParameterRangeSpecification
    , ContinuousParameterRangeSpecification
    , continuousParameterRangeSpecification
    , cprsMinValue
    , cprsMaxValue

    -- * DataSource
    , DataSource
    , dataSource
    , dsS3DataSource

    -- * DeployedImage
    , DeployedImage
    , deployedImage
    , diResolvedImage
    , diSpecifiedImage
    , diResolutionTime

    -- * DesiredWeightAndCapacity
    , DesiredWeightAndCapacity
    , desiredWeightAndCapacity
    , dwacDesiredInstanceCount
    , dwacDesiredWeight
    , dwacVariantName

    -- * EndpointConfigSummary
    , EndpointConfigSummary
    , endpointConfigSummary
    , ecsEndpointConfigName
    , ecsEndpointConfigARN
    , ecsCreationTime

    -- * EndpointSummary
    , EndpointSummary
    , endpointSummary
    , esEndpointName
    , esEndpointARN
    , esCreationTime
    , esLastModifiedTime
    , esEndpointStatus

    -- * Filter
    , Filter
    , filter'
    , fOperator
    , fValue
    , fName

    -- * FinalHyperParameterTuningJobObjectiveMetric
    , FinalHyperParameterTuningJobObjectiveMetric
    , finalHyperParameterTuningJobObjectiveMetric
    , fhptjomType
    , fhptjomMetricName
    , fhptjomValue

    -- * GitConfig
    , GitConfig
    , gitConfig
    , gcBranch
    , gcSecretARN
    , gcRepositoryURL

    -- * GitConfigForUpdate
    , GitConfigForUpdate
    , gitConfigForUpdate
    , gcfuSecretARN

    -- * HumanTaskConfig
    , HumanTaskConfig
    , humanTaskConfig
    , htcTaskKeywords
    , htcPublicWorkforceTaskPrice
    , htcTaskAvailabilityLifetimeInSeconds
    , htcMaxConcurrentTaskCount
    , htcWorkteamARN
    , htcUiConfig
    , htcPreHumanTaskLambdaARN
    , htcTaskTitle
    , htcTaskDescription
    , htcNumberOfHumanWorkersPerDataObject
    , htcTaskTimeLimitInSeconds
    , htcAnnotationConsolidationConfig

    -- * HyperParameterAlgorithmSpecification
    , HyperParameterAlgorithmSpecification
    , hyperParameterAlgorithmSpecification
    , hpasAlgorithmName
    , hpasTrainingImage
    , hpasMetricDefinitions
    , hpasTrainingInputMode

    -- * HyperParameterSpecification
    , HyperParameterSpecification
    , hyperParameterSpecification
    , hpsIsTunable
    , hpsRange
    , hpsDefaultValue
    , hpsIsRequired
    , hpsDescription
    , hpsName
    , hpsType

    -- * HyperParameterTrainingJobDefinition
    , HyperParameterTrainingJobDefinition
    , hyperParameterTrainingJobDefinition
    , hptjdEnableNetworkIsolation
    , hptjdStaticHyperParameters
    , hptjdInputDataConfig
    , hptjdVPCConfig
    , hptjdEnableInterContainerTrafficEncryption
    , hptjdAlgorithmSpecification
    , hptjdRoleARN
    , hptjdOutputDataConfig
    , hptjdResourceConfig
    , hptjdStoppingCondition

    -- * HyperParameterTrainingJobSummary
    , HyperParameterTrainingJobSummary
    , hyperParameterTrainingJobSummary
    , hptjsFailureReason
    , hptjsTuningJobName
    , hptjsTrainingEndTime
    , hptjsObjectiveStatus
    , hptjsTrainingStartTime
    , hptjsFinalHyperParameterTuningJobObjectiveMetric
    , hptjsTrainingJobName
    , hptjsTrainingJobARN
    , hptjsCreationTime
    , hptjsTrainingJobStatus
    , hptjsTunedHyperParameters

    -- * HyperParameterTuningJobConfig
    , HyperParameterTuningJobConfig
    , hyperParameterTuningJobConfig
    , hptjcTrainingJobEarlyStoppingType
    , hptjcStrategy
    , hptjcHyperParameterTuningJobObjective
    , hptjcResourceLimits
    , hptjcParameterRanges

    -- * HyperParameterTuningJobObjective
    , HyperParameterTuningJobObjective
    , hyperParameterTuningJobObjective
    , hptjoType
    , hptjoMetricName

    -- * HyperParameterTuningJobSummary
    , HyperParameterTuningJobSummary
    , hyperParameterTuningJobSummary
    , hResourceLimits
    , hLastModifiedTime
    , hHyperParameterTuningEndTime
    , hHyperParameterTuningJobName
    , hHyperParameterTuningJobARN
    , hHyperParameterTuningJobStatus
    , hStrategy
    , hCreationTime
    , hTrainingJobStatusCounters
    , hObjectiveStatusCounters

    -- * HyperParameterTuningJobWarmStartConfig
    , HyperParameterTuningJobWarmStartConfig
    , hyperParameterTuningJobWarmStartConfig
    , hptjwscParentHyperParameterTuningJobs
    , hptjwscWarmStartType

    -- * InferenceSpecification
    , InferenceSpecification
    , inferenceSpecification
    , isContainers
    , isSupportedTransformInstanceTypes
    , isSupportedRealtimeInferenceInstanceTypes
    , isSupportedContentTypes
    , isSupportedResponseMIMETypes

    -- * InputConfig
    , InputConfig
    , inputConfig
    , icS3URI
    , icDataInputConfig
    , icFramework

    -- * IntegerParameterRange
    , IntegerParameterRange
    , integerParameterRange
    , iprScalingType
    , iprName
    , iprMinValue
    , iprMaxValue

    -- * IntegerParameterRangeSpecification
    , IntegerParameterRangeSpecification
    , integerParameterRangeSpecification
    , iprsMinValue
    , iprsMaxValue

    -- * LabelCounters
    , LabelCounters
    , labelCounters
    , lcMachineLabeled
    , lcTotalLabeled
    , lcFailedNonRetryableError
    , lcUnlabeled
    , lcHumanLabeled

    -- * LabelCountersForWorkteam
    , LabelCountersForWorkteam
    , labelCountersForWorkteam
    , lcfwPendingHuman
    , lcfwTotal
    , lcfwHumanLabeled

    -- * LabelingJobAlgorithmsConfig
    , LabelingJobAlgorithmsConfig
    , labelingJobAlgorithmsConfig
    , ljacLabelingJobResourceConfig
    , ljacInitialActiveLearningModelARN
    , ljacLabelingJobAlgorithmSpecificationARN

    -- * LabelingJobDataAttributes
    , LabelingJobDataAttributes
    , labelingJobDataAttributes
    , ljdaContentClassifiers

    -- * LabelingJobDataSource
    , LabelingJobDataSource
    , labelingJobDataSource
    , ljdsS3DataSource

    -- * LabelingJobForWorkteamSummary
    , LabelingJobForWorkteamSummary
    , labelingJobForWorkteamSummary
    , ljfwsLabelCounters
    , ljfwsLabelingJobName
    , ljfwsJobReferenceCode
    , ljfwsWorkRequesterAccountId
    , ljfwsCreationTime

    -- * LabelingJobInputConfig
    , LabelingJobInputConfig
    , labelingJobInputConfig
    , ljicDataAttributes
    , ljicDataSource

    -- * LabelingJobOutput
    , LabelingJobOutput
    , labelingJobOutput
    , ljoFinalActiveLearningModelARN
    , ljoOutputDatasetS3URI

    -- * LabelingJobOutputConfig
    , LabelingJobOutputConfig
    , labelingJobOutputConfig
    , ljocKMSKeyId
    , ljocS3OutputPath

    -- * LabelingJobResourceConfig
    , LabelingJobResourceConfig
    , labelingJobResourceConfig
    , ljrcVolumeKMSKeyId

    -- * LabelingJobS3DataSource
    , LabelingJobS3DataSource
    , labelingJobS3DataSource
    , ljsdsManifestS3URI

    -- * LabelingJobStoppingConditions
    , LabelingJobStoppingConditions
    , labelingJobStoppingConditions
    , ljscMaxHumanLabeledObjectCount
    , ljscMaxPercentageOfInputDatasetLabeled

    -- * LabelingJobSummary
    , LabelingJobSummary
    , labelingJobSummary
    , ljsFailureReason
    , ljsAnnotationConsolidationLambdaARN
    , ljsInputConfig
    , ljsLabelingJobOutput
    , ljsLabelingJobName
    , ljsLabelingJobARN
    , ljsCreationTime
    , ljsLastModifiedTime
    , ljsLabelingJobStatus
    , ljsLabelCounters
    , ljsWorkteamARN
    , ljsPreHumanTaskLambdaARN

    -- * MemberDefinition
    , MemberDefinition
    , memberDefinition
    , mdCognitoMemberDefinition

    -- * MetricData
    , MetricData
    , metricData
    , mdMetricName
    , mdValue
    , mdTimestamp

    -- * MetricDefinition
    , MetricDefinition
    , metricDefinition
    , mdName
    , mdRegex

    -- * ModelArtifacts
    , ModelArtifacts
    , modelArtifacts
    , maS3ModelArtifacts

    -- * ModelPackageContainerDefinition
    , ModelPackageContainerDefinition
    , modelPackageContainerDefinition
    , mpcdModelDataURL
    , mpcdImageDigest
    , mpcdContainerHostname
    , mpcdProductId
    , mpcdImage

    -- * ModelPackageStatusDetails
    , ModelPackageStatusDetails
    , modelPackageStatusDetails
    , mpsdImageScanStatuses
    , mpsdValidationStatuses

    -- * ModelPackageStatusItem
    , ModelPackageStatusItem
    , modelPackageStatusItem
    , mpsiFailureReason
    , mpsiName
    , mpsiStatus

    -- * ModelPackageSummary
    , ModelPackageSummary
    , modelPackageSummary
    , mpsModelPackageDescription
    , mpsModelPackageName
    , mpsModelPackageARN
    , mpsCreationTime
    , mpsModelPackageStatus

    -- * ModelPackageValidationProfile
    , ModelPackageValidationProfile
    , modelPackageValidationProfile
    , mpvpProfileName
    , mpvpTransformJobDefinition

    -- * ModelPackageValidationSpecification
    , ModelPackageValidationSpecification
    , modelPackageValidationSpecification
    , mpvsValidationRole
    , mpvsValidationProfiles

    -- * ModelSummary
    , ModelSummary
    , modelSummary
    , msModelName
    , msModelARN
    , msCreationTime

    -- * NestedFilters
    , NestedFilters
    , nestedFilters
    , nfNestedPropertyName
    , nfFilters

    -- * NotebookInstanceLifecycleConfigSummary
    , NotebookInstanceLifecycleConfigSummary
    , notebookInstanceLifecycleConfigSummary
    , nilcsCreationTime
    , nilcsLastModifiedTime
    , nilcsNotebookInstanceLifecycleConfigName
    , nilcsNotebookInstanceLifecycleConfigARN

    -- * NotebookInstanceLifecycleHook
    , NotebookInstanceLifecycleHook
    , notebookInstanceLifecycleHook
    , nilhContent

    -- * NotebookInstanceSummary
    , NotebookInstanceSummary
    , notebookInstanceSummary
    , nisCreationTime
    , nisAdditionalCodeRepositories
    , nisURL
    , nisLastModifiedTime
    , nisInstanceType
    , nisNotebookInstanceStatus
    , nisDefaultCodeRepository
    , nisNotebookInstanceLifecycleConfigName
    , nisNotebookInstanceName
    , nisNotebookInstanceARN

    -- * ObjectiveStatusCounters
    , ObjectiveStatusCounters
    , objectiveStatusCounters
    , oscPending
    , oscSucceeded
    , oscFailed

    -- * OutputConfig
    , OutputConfig
    , outputConfig
    , ocS3OutputLocation
    , ocTargetDevice

    -- * OutputDataConfig
    , OutputDataConfig
    , outputDataConfig
    , odcKMSKeyId
    , odcS3OutputPath

    -- * ParameterRange
    , ParameterRange
    , parameterRange
    , prCategoricalParameterRangeSpecification
    , prIntegerParameterRangeSpecification
    , prContinuousParameterRangeSpecification

    -- * ParameterRanges
    , ParameterRanges
    , parameterRanges
    , prCategoricalParameterRanges
    , prIntegerParameterRanges
    , prContinuousParameterRanges

    -- * ParentHyperParameterTuningJob
    , ParentHyperParameterTuningJob
    , parentHyperParameterTuningJob
    , phptjHyperParameterTuningJobName

    -- * ProductionVariant
    , ProductionVariant
    , productionVariant
    , pvAcceleratorType
    , pvInitialVariantWeight
    , pvVariantName
    , pvModelName
    , pvInitialInstanceCount
    , pvInstanceType

    -- * ProductionVariantSummary
    , ProductionVariantSummary
    , productionVariantSummary
    , pvsDesiredInstanceCount
    , pvsDesiredWeight
    , pvsCurrentWeight
    , pvsCurrentInstanceCount
    , pvsDeployedImages
    , pvsVariantName

    -- * PropertyNameQuery
    , PropertyNameQuery
    , propertyNameQuery
    , pnqPropertyNameHint

    -- * PropertyNameSuggestion
    , PropertyNameSuggestion
    , propertyNameSuggestion
    , pnsPropertyName

    -- * PublicWorkforceTaskPrice
    , PublicWorkforceTaskPrice
    , publicWorkforceTaskPrice
    , pwtpAmountInUsd

    -- * RenderableTask
    , RenderableTask
    , renderableTask
    , rtInput

    -- * RenderingError
    , RenderingError
    , renderingError
    , reCode
    , reMessage

    -- * ResourceConfig
    , ResourceConfig
    , resourceConfig
    , rcVolumeKMSKeyId
    , rcInstanceType
    , rcInstanceCount
    , rcVolumeSizeInGB

    -- * ResourceLimits
    , ResourceLimits
    , resourceLimits
    , rlMaxNumberOfTrainingJobs
    , rlMaxParallelTrainingJobs

    -- * S3DataSource
    , S3DataSource
    , s3DataSource
    , sdsS3DataDistributionType
    , sdsAttributeNames
    , sdsS3DataType
    , sdsS3URI

    -- * SearchExpression
    , SearchExpression
    , searchExpression
    , seSubExpressions
    , seOperator
    , seFilters
    , seNestedFilters

    -- * SearchRecord
    , SearchRecord
    , searchRecord
    , srTrainingJob

    -- * SecondaryStatusTransition
    , SecondaryStatusTransition
    , secondaryStatusTransition
    , sstStatusMessage
    , sstEndTime
    , sstStatus
    , sstStartTime

    -- * ShuffleConfig
    , ShuffleConfig
    , shuffleConfig
    , scSeed

    -- * SourceAlgorithm
    , SourceAlgorithm
    , sourceAlgorithm
    , saModelDataURL
    , saAlgorithmName

    -- * SourceAlgorithmSpecification
    , SourceAlgorithmSpecification
    , sourceAlgorithmSpecification
    , sasSourceAlgorithms

    -- * StoppingCondition
    , StoppingCondition
    , stoppingCondition
    , scMaxRuntimeInSeconds

    -- * SubscribedWorkteam
    , SubscribedWorkteam
    , subscribedWorkteam
    , swMarketplaceTitle
    , swSellerName
    , swListingId
    , swMarketplaceDescription
    , swWorkteamARN

    -- * SuggestionQuery
    , SuggestionQuery
    , suggestionQuery
    , sqPropertyNameQuery

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * TrainingJob
    , TrainingJob
    , trainingJob
    , tjCreationTime
    , tjLabelingJobARN
    , tjFailureReason
    , tjSecondaryStatusTransitions
    , tjModelArtifacts
    , tjTrainingEndTime
    , tjStoppingCondition
    , tjTrainingJobStatus
    , tjEnableNetworkIsolation
    , tjLastModifiedTime
    , tjHyperParameters
    , tjInputDataConfig
    , tjVPCConfig
    , tjTrainingJobARN
    , tjAlgorithmSpecification
    , tjFinalMetricDataList
    , tjOutputDataConfig
    , tjTrainingStartTime
    , tjTuningJobARN
    , tjTrainingJobName
    , tjResourceConfig
    , tjEnableInterContainerTrafficEncryption
    , tjSecondaryStatus
    , tjTags
    , tjRoleARN

    -- * TrainingJobDefinition
    , TrainingJobDefinition
    , trainingJobDefinition
    , tjdHyperParameters
    , tjdTrainingInputMode
    , tjdInputDataConfig
    , tjdOutputDataConfig
    , tjdResourceConfig
    , tjdStoppingCondition

    -- * TrainingJobStatusCounters
    , TrainingJobStatusCounters
    , trainingJobStatusCounters
    , tjscStopped
    , tjscRetryableError
    , tjscInProgress
    , tjscNonRetryableError
    , tjscCompleted

    -- * TrainingJobSummary
    , TrainingJobSummary
    , trainingJobSummary
    , tTrainingEndTime
    , tLastModifiedTime
    , tTrainingJobName
    , tTrainingJobARN
    , tCreationTime
    , tTrainingJobStatus

    -- * TrainingSpecification
    , TrainingSpecification
    , trainingSpecification
    , tsTrainingImageDigest
    , tsSupportsDistributedTraining
    , tsSupportedHyperParameters
    , tsSupportedTuningJobObjectiveMetrics
    , tsMetricDefinitions
    , tsTrainingImage
    , tsSupportedTrainingInstanceTypes
    , tsTrainingChannels

    -- * TransformDataSource
    , TransformDataSource
    , transformDataSource
    , tdsS3DataSource

    -- * TransformInput
    , TransformInput
    , transformInput
    , tiSplitType
    , tiCompressionType
    , tiContentType
    , tiDataSource

    -- * TransformJobDefinition
    , TransformJobDefinition
    , transformJobDefinition
    , tjdBatchStrategy
    , tjdMaxPayloadInMB
    , tjdEnvironment
    , tjdMaxConcurrentTransforms
    , tjdTransformInput
    , tjdTransformOutput
    , tjdTransformResources

    -- * TransformJobSummary
    , TransformJobSummary
    , transformJobSummary
    , tjsFailureReason
    , tjsLastModifiedTime
    , tjsTransformEndTime
    , tjsTransformJobName
    , tjsTransformJobARN
    , tjsCreationTime
    , tjsTransformJobStatus

    -- * TransformOutput
    , TransformOutput
    , transformOutput
    , toAssembleWith
    , toAccept
    , toKMSKeyId
    , toS3OutputPath

    -- * TransformResources
    , TransformResources
    , transformResources
    , trVolumeKMSKeyId
    , trInstanceType
    , trInstanceCount

    -- * TransformS3DataSource
    , TransformS3DataSource
    , transformS3DataSource
    , tsdsS3DataType
    , tsdsS3URI

    -- * USD
    , USD
    , uSD
    , usdCents
    , usdDollars
    , usdTenthFractionsOfACent

    -- * UiConfig
    , UiConfig
    , uiConfig
    , ucUiTemplateS3URI

    -- * UiTemplate
    , UiTemplate
    , uiTemplate
    , utContent

    -- * VPCConfig
    , VPCConfig
    , vpcConfig
    , vcSecurityGroupIds
    , vcSubnets

    -- * Workteam
    , Workteam
    , workteam
    , wSubDomain
    , wProductListingIds
    , wCreateDate
    , wLastUpdatedDate
    , wWorkteamName
    , wMemberDefinitions
    , wWorkteamARN
    , wDescription
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.Product
import Network.AWS.SageMaker.Types.Sum
import Network.AWS.Sign.V4

-- | API version @2017-07-24@ of the Amazon SageMaker Service SDK configuration.
sageMaker :: Service
sageMaker =
  Service
    { _svcAbbrev = "SageMaker"
    , _svcSigner = v4
    , _svcPrefix = "api.sagemaker"
    , _svcVersion = "2017-07-24"
    , _svcEndpoint = defaultEndpoint sageMaker
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "SageMaker"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | You have exceeded an Amazon SageMaker resource limit. For example, you might have too many training jobs created.
--
--
_ResourceLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceLimitExceeded = _MatchServiceError sageMaker "ResourceLimitExceeded"


-- | Resource being accessed is in use.
--
--
_ResourceInUse :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUse = _MatchServiceError sageMaker "ResourceInUse"


-- | Resource being access is not found.
--
--
_ResourceNotFound :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFound = _MatchServiceError sageMaker "ResourceNotFound"


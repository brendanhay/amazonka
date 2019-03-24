{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides APIs for creating and managing Amazon SageMaker resources.
--
--
module Network.AWS.SageMaker
    (
    -- * Service Configuration
      sageMaker

    -- * Errors
    -- $errors

    -- ** ResourceLimitExceeded
    , _ResourceLimitExceeded

    -- ** ResourceInUse
    , _ResourceInUse

    -- ** ResourceNotFound
    , _ResourceNotFound

    -- * Waiters
    -- $waiters

    -- ** NotebookInstanceDeleted
    , notebookInstanceDeleted

    -- ** EndpointDeleted
    , endpointDeleted

    -- ** EndpointInService
    , endpointInService

    -- ** TransformJobCompletedOrStopped
    , transformJobCompletedOrStopped

    -- ** NotebookInstanceInService
    , notebookInstanceInService

    -- ** TrainingJobCompletedOrStopped
    , trainingJobCompletedOrStopped

    -- ** NotebookInstanceStopped
    , notebookInstanceStopped

    -- * Operations
    -- $operations

    -- ** CreateNotebookInstance
    , module Network.AWS.SageMaker.CreateNotebookInstance

    -- ** DeleteModelPackage
    , module Network.AWS.SageMaker.DeleteModelPackage

    -- ** DescribeEndpointConfig
    , module Network.AWS.SageMaker.DescribeEndpointConfig

    -- ** ListLabelingJobsForWorkteam (Paginated)
    , module Network.AWS.SageMaker.ListLabelingJobsForWorkteam

    -- ** CreateTransformJob
    , module Network.AWS.SageMaker.CreateTransformJob

    -- ** ListCompilationJobs (Paginated)
    , module Network.AWS.SageMaker.ListCompilationJobs

    -- ** StopHyperParameterTuningJob
    , module Network.AWS.SageMaker.StopHyperParameterTuningJob

    -- ** CreateEndpoint
    , module Network.AWS.SageMaker.CreateEndpoint

    -- ** GetSearchSuggestions
    , module Network.AWS.SageMaker.GetSearchSuggestions

    -- ** DescribeCodeRepository
    , module Network.AWS.SageMaker.DescribeCodeRepository

    -- ** DescribeTrainingJob
    , module Network.AWS.SageMaker.DescribeTrainingJob

    -- ** DeleteEndpoint
    , module Network.AWS.SageMaker.DeleteEndpoint

    -- ** UpdateEndpoint
    , module Network.AWS.SageMaker.UpdateEndpoint

    -- ** CreateCompilationJob
    , module Network.AWS.SageMaker.CreateCompilationJob

    -- ** DeleteNotebookInstanceLifecycleConfig
    , module Network.AWS.SageMaker.DeleteNotebookInstanceLifecycleConfig

    -- ** UpdateNotebookInstanceLifecycleConfig
    , module Network.AWS.SageMaker.UpdateNotebookInstanceLifecycleConfig

    -- ** CreateLabelingJob
    , module Network.AWS.SageMaker.CreateLabelingJob

    -- ** DescribeNotebookInstance
    , module Network.AWS.SageMaker.DescribeNotebookInstance

    -- ** CreateEndpointConfig
    , module Network.AWS.SageMaker.CreateEndpointConfig

    -- ** StopNotebookInstance
    , module Network.AWS.SageMaker.StopNotebookInstance

    -- ** UpdateEndpointWeightsAndCapacities
    , module Network.AWS.SageMaker.UpdateEndpointWeightsAndCapacities

    -- ** DeleteTags
    , module Network.AWS.SageMaker.DeleteTags

    -- ** DescribeModelPackage
    , module Network.AWS.SageMaker.DescribeModelPackage

    -- ** DeleteEndpointConfig
    , module Network.AWS.SageMaker.DeleteEndpointConfig

    -- ** CreateAlgorithm
    , module Network.AWS.SageMaker.CreateAlgorithm

    -- ** StopTransformJob
    , module Network.AWS.SageMaker.StopTransformJob

    -- ** CreateModel
    , module Network.AWS.SageMaker.CreateModel

    -- ** CreateCodeRepository
    , module Network.AWS.SageMaker.CreateCodeRepository

    -- ** CreateHyperParameterTuningJob
    , module Network.AWS.SageMaker.CreateHyperParameterTuningJob

    -- ** ListCodeRepositories (Paginated)
    , module Network.AWS.SageMaker.ListCodeRepositories

    -- ** DescribeCompilationJob
    , module Network.AWS.SageMaker.DescribeCompilationJob

    -- ** ListHyperParameterTuningJobs (Paginated)
    , module Network.AWS.SageMaker.ListHyperParameterTuningJobs

    -- ** ListAlgorithms (Paginated)
    , module Network.AWS.SageMaker.ListAlgorithms

    -- ** RenderUiTemplate
    , module Network.AWS.SageMaker.RenderUiTemplate

    -- ** DeleteModel
    , module Network.AWS.SageMaker.DeleteModel

    -- ** ListModels (Paginated)
    , module Network.AWS.SageMaker.ListModels

    -- ** DeleteAlgorithm
    , module Network.AWS.SageMaker.DeleteAlgorithm

    -- ** DescribeNotebookInstanceLifecycleConfig
    , module Network.AWS.SageMaker.DescribeNotebookInstanceLifecycleConfig

    -- ** CreateModelPackage
    , module Network.AWS.SageMaker.CreateModelPackage

    -- ** ListNotebookInstances (Paginated)
    , module Network.AWS.SageMaker.ListNotebookInstances

    -- ** StopLabelingJob
    , module Network.AWS.SageMaker.StopLabelingJob

    -- ** DeleteNotebookInstance
    , module Network.AWS.SageMaker.DeleteNotebookInstance

    -- ** UpdateNotebookInstance
    , module Network.AWS.SageMaker.UpdateNotebookInstance

    -- ** ListModelPackages (Paginated)
    , module Network.AWS.SageMaker.ListModelPackages

    -- ** DescribeLabelingJob
    , module Network.AWS.SageMaker.DescribeLabelingJob

    -- ** StopTrainingJob
    , module Network.AWS.SageMaker.StopTrainingJob

    -- ** DescribeAlgorithm
    , module Network.AWS.SageMaker.DescribeAlgorithm

    -- ** DescribeModel
    , module Network.AWS.SageMaker.DescribeModel

    -- ** ListTransformJobs (Paginated)
    , module Network.AWS.SageMaker.ListTransformJobs

    -- ** DescribeHyperParameterTuningJob
    , module Network.AWS.SageMaker.DescribeHyperParameterTuningJob

    -- ** ListEndpoints (Paginated)
    , module Network.AWS.SageMaker.ListEndpoints

    -- ** CreatePresignedNotebookInstanceURL
    , module Network.AWS.SageMaker.CreatePresignedNotebookInstanceURL

    -- ** ListTrainingJobsForHyperParameterTuningJob (Paginated)
    , module Network.AWS.SageMaker.ListTrainingJobsForHyperParameterTuningJob

    -- ** UpdateWorkteam
    , module Network.AWS.SageMaker.UpdateWorkteam

    -- ** DeleteWorkteam
    , module Network.AWS.SageMaker.DeleteWorkteam

    -- ** ListWorkteams (Paginated)
    , module Network.AWS.SageMaker.ListWorkteams

    -- ** ListNotebookInstanceLifecycleConfigs (Paginated)
    , module Network.AWS.SageMaker.ListNotebookInstanceLifecycleConfigs

    -- ** DescribeSubscribedWorkteam
    , module Network.AWS.SageMaker.DescribeSubscribedWorkteam

    -- ** CreateWorkteam
    , module Network.AWS.SageMaker.CreateWorkteam

    -- ** CreateNotebookInstanceLifecycleConfig
    , module Network.AWS.SageMaker.CreateNotebookInstanceLifecycleConfig

    -- ** ListLabelingJobs (Paginated)
    , module Network.AWS.SageMaker.ListLabelingJobs

    -- ** StartNotebookInstance
    , module Network.AWS.SageMaker.StartNotebookInstance

    -- ** AddTags
    , module Network.AWS.SageMaker.AddTags

    -- ** ListEndpointConfigs (Paginated)
    , module Network.AWS.SageMaker.ListEndpointConfigs

    -- ** ListTags (Paginated)
    , module Network.AWS.SageMaker.ListTags

    -- ** CreateTrainingJob
    , module Network.AWS.SageMaker.CreateTrainingJob

    -- ** StopCompilationJob
    , module Network.AWS.SageMaker.StopCompilationJob

    -- ** Search (Paginated)
    , module Network.AWS.SageMaker.Search

    -- ** UpdateCodeRepository
    , module Network.AWS.SageMaker.UpdateCodeRepository

    -- ** DeleteCodeRepository
    , module Network.AWS.SageMaker.DeleteCodeRepository

    -- ** DescribeTransformJob
    , module Network.AWS.SageMaker.DescribeTransformJob

    -- ** DescribeEndpoint
    , module Network.AWS.SageMaker.DescribeEndpoint

    -- ** ListTrainingJobs (Paginated)
    , module Network.AWS.SageMaker.ListTrainingJobs

    -- ** DescribeWorkteam
    , module Network.AWS.SageMaker.DescribeWorkteam

    -- ** ListSubscribedWorkteams (Paginated)
    , module Network.AWS.SageMaker.ListSubscribedWorkteams

    -- * Types

    -- ** AlgorithmSortBy
    , AlgorithmSortBy (..)

    -- ** AlgorithmStatus
    , AlgorithmStatus (..)

    -- ** AssemblyType
    , AssemblyType (..)

    -- ** BatchStrategy
    , BatchStrategy (..)

    -- ** BooleanOperator
    , BooleanOperator (..)

    -- ** CodeRepositorySortBy
    , CodeRepositorySortBy (..)

    -- ** CodeRepositorySortOrder
    , CodeRepositorySortOrder (..)

    -- ** CompilationJobStatus
    , CompilationJobStatus (..)

    -- ** CompressionType
    , CompressionType (..)

    -- ** ContentClassifier
    , ContentClassifier (..)

    -- ** DetailedAlgorithmStatus
    , DetailedAlgorithmStatus (..)

    -- ** DetailedModelPackageStatus
    , DetailedModelPackageStatus (..)

    -- ** DirectInternetAccess
    , DirectInternetAccess (..)

    -- ** EndpointConfigSortKey
    , EndpointConfigSortKey (..)

    -- ** EndpointSortKey
    , EndpointSortKey (..)

    -- ** EndpointStatus
    , EndpointStatus (..)

    -- ** Framework
    , Framework (..)

    -- ** HyperParameterScalingType
    , HyperParameterScalingType (..)

    -- ** HyperParameterTuningJobObjectiveType
    , HyperParameterTuningJobObjectiveType (..)

    -- ** HyperParameterTuningJobSortByOptions
    , HyperParameterTuningJobSortByOptions (..)

    -- ** HyperParameterTuningJobStatus
    , HyperParameterTuningJobStatus (..)

    -- ** HyperParameterTuningJobStrategyType
    , HyperParameterTuningJobStrategyType (..)

    -- ** HyperParameterTuningJobWarmStartType
    , HyperParameterTuningJobWarmStartType (..)

    -- ** InstanceType
    , InstanceType (..)

    -- ** LabelingJobStatus
    , LabelingJobStatus (..)

    -- ** ListCompilationJobsSortBy
    , ListCompilationJobsSortBy (..)

    -- ** ListLabelingJobsForWorkteamSortByOptions
    , ListLabelingJobsForWorkteamSortByOptions (..)

    -- ** ListWorkteamsSortByOptions
    , ListWorkteamsSortByOptions (..)

    -- ** ModelPackageSortBy
    , ModelPackageSortBy (..)

    -- ** ModelPackageStatus
    , ModelPackageStatus (..)

    -- ** ModelSortKey
    , ModelSortKey (..)

    -- ** NotebookInstanceAcceleratorType
    , NotebookInstanceAcceleratorType (..)

    -- ** NotebookInstanceLifecycleConfigSortKey
    , NotebookInstanceLifecycleConfigSortKey (..)

    -- ** NotebookInstanceLifecycleConfigSortOrder
    , NotebookInstanceLifecycleConfigSortOrder (..)

    -- ** NotebookInstanceSortKey
    , NotebookInstanceSortKey (..)

    -- ** NotebookInstanceSortOrder
    , NotebookInstanceSortOrder (..)

    -- ** NotebookInstanceStatus
    , NotebookInstanceStatus (..)

    -- ** ObjectiveStatus
    , ObjectiveStatus (..)

    -- ** Operator
    , Operator (..)

    -- ** OrderKey
    , OrderKey (..)

    -- ** ParameterType
    , ParameterType (..)

    -- ** ProductionVariantAcceleratorType
    , ProductionVariantAcceleratorType (..)

    -- ** ProductionVariantInstanceType
    , ProductionVariantInstanceType (..)

    -- ** RecordWrapper
    , RecordWrapper (..)

    -- ** ResourceType
    , ResourceType (..)

    -- ** RootAccess
    , RootAccess (..)

    -- ** S3DataDistribution
    , S3DataDistribution (..)

    -- ** S3DataType
    , S3DataType (..)

    -- ** SearchSortOrder
    , SearchSortOrder (..)

    -- ** SecondaryStatus
    , SecondaryStatus (..)

    -- ** SortBy
    , SortBy (..)

    -- ** SortOrder
    , SortOrder (..)

    -- ** SplitType
    , SplitType (..)

    -- ** TargetDevice
    , TargetDevice (..)

    -- ** TrainingInputMode
    , TrainingInputMode (..)

    -- ** TrainingInstanceType
    , TrainingInstanceType (..)

    -- ** TrainingJobEarlyStoppingType
    , TrainingJobEarlyStoppingType (..)

    -- ** TrainingJobSortByOptions
    , TrainingJobSortByOptions (..)

    -- ** TrainingJobStatus
    , TrainingJobStatus (..)

    -- ** TransformInstanceType
    , TransformInstanceType (..)

    -- ** TransformJobStatus
    , TransformJobStatus (..)

    -- ** AlgorithmSpecification
    , AlgorithmSpecification
    , algorithmSpecification
    , asAlgorithmName
    , asTrainingImage
    , asMetricDefinitions
    , asTrainingInputMode

    -- ** AlgorithmStatusDetails
    , AlgorithmStatusDetails
    , algorithmStatusDetails
    , asdImageScanStatuses
    , asdValidationStatuses

    -- ** AlgorithmStatusItem
    , AlgorithmStatusItem
    , algorithmStatusItem
    , asiFailureReason
    , asiName
    , asiStatus

    -- ** AlgorithmSummary
    , AlgorithmSummary
    , algorithmSummary
    , aAlgorithmDescription
    , aAlgorithmName
    , aAlgorithmARN
    , aCreationTime
    , aAlgorithmStatus

    -- ** AlgorithmValidationProfile
    , AlgorithmValidationProfile
    , algorithmValidationProfile
    , avpTransformJobDefinition
    , avpProfileName
    , avpTrainingJobDefinition

    -- ** AlgorithmValidationSpecification
    , AlgorithmValidationSpecification
    , algorithmValidationSpecification
    , avsValidationRole
    , avsValidationProfiles

    -- ** AnnotationConsolidationConfig
    , AnnotationConsolidationConfig
    , annotationConsolidationConfig
    , accAnnotationConsolidationLambdaARN

    -- ** CategoricalParameterRange
    , CategoricalParameterRange
    , categoricalParameterRange
    , cprName
    , cprValues

    -- ** CategoricalParameterRangeSpecification
    , CategoricalParameterRangeSpecification
    , categoricalParameterRangeSpecification
    , cprsValues

    -- ** Channel
    , Channel
    , channel
    , cShuffleConfig
    , cRecordWrapperType
    , cInputMode
    , cCompressionType
    , cContentType
    , cChannelName
    , cDataSource

    -- ** ChannelSpecification
    , ChannelSpecification
    , channelSpecification
    , csSupportedCompressionTypes
    , csIsRequired
    , csDescription
    , csName
    , csSupportedContentTypes
    , csSupportedInputModes

    -- ** CodeRepositorySummary
    , CodeRepositorySummary
    , codeRepositorySummary
    , crsGitConfig
    , crsCodeRepositoryName
    , crsCodeRepositoryARN
    , crsCreationTime
    , crsLastModifiedTime

    -- ** CognitoMemberDefinition
    , CognitoMemberDefinition
    , cognitoMemberDefinition
    , cmdUserPool
    , cmdUserGroup
    , cmdClientId

    -- ** CompilationJobSummary
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

    -- ** ContainerDefinition
    , ContainerDefinition
    , containerDefinition
    , cdModelDataURL
    , cdImage
    , cdModelPackageName
    , cdEnvironment
    , cdContainerHostname

    -- ** ContinuousParameterRange
    , ContinuousParameterRange
    , continuousParameterRange
    , cScalingType
    , cName
    , cMinValue
    , cMaxValue

    -- ** ContinuousParameterRangeSpecification
    , ContinuousParameterRangeSpecification
    , continuousParameterRangeSpecification
    , cprsMinValue
    , cprsMaxValue

    -- ** DataSource
    , DataSource
    , dataSource
    , dsS3DataSource

    -- ** DeployedImage
    , DeployedImage
    , deployedImage
    , diResolvedImage
    , diSpecifiedImage
    , diResolutionTime

    -- ** DesiredWeightAndCapacity
    , DesiredWeightAndCapacity
    , desiredWeightAndCapacity
    , dwacDesiredInstanceCount
    , dwacDesiredWeight
    , dwacVariantName

    -- ** EndpointConfigSummary
    , EndpointConfigSummary
    , endpointConfigSummary
    , ecsEndpointConfigName
    , ecsEndpointConfigARN
    , ecsCreationTime

    -- ** EndpointSummary
    , EndpointSummary
    , endpointSummary
    , esEndpointName
    , esEndpointARN
    , esCreationTime
    , esLastModifiedTime
    , esEndpointStatus

    -- ** Filter
    , Filter
    , filter'
    , fOperator
    , fValue
    , fName

    -- ** FinalHyperParameterTuningJobObjectiveMetric
    , FinalHyperParameterTuningJobObjectiveMetric
    , finalHyperParameterTuningJobObjectiveMetric
    , fhptjomType
    , fhptjomMetricName
    , fhptjomValue

    -- ** GitConfig
    , GitConfig
    , gitConfig
    , gcBranch
    , gcSecretARN
    , gcRepositoryURL

    -- ** GitConfigForUpdate
    , GitConfigForUpdate
    , gitConfigForUpdate
    , gcfuSecretARN

    -- ** HumanTaskConfig
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

    -- ** HyperParameterAlgorithmSpecification
    , HyperParameterAlgorithmSpecification
    , hyperParameterAlgorithmSpecification
    , hpasAlgorithmName
    , hpasTrainingImage
    , hpasMetricDefinitions
    , hpasTrainingInputMode

    -- ** HyperParameterSpecification
    , HyperParameterSpecification
    , hyperParameterSpecification
    , hpsIsTunable
    , hpsRange
    , hpsDefaultValue
    , hpsIsRequired
    , hpsDescription
    , hpsName
    , hpsType

    -- ** HyperParameterTrainingJobDefinition
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

    -- ** HyperParameterTrainingJobSummary
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

    -- ** HyperParameterTuningJobConfig
    , HyperParameterTuningJobConfig
    , hyperParameterTuningJobConfig
    , hptjcTrainingJobEarlyStoppingType
    , hptjcStrategy
    , hptjcHyperParameterTuningJobObjective
    , hptjcResourceLimits
    , hptjcParameterRanges

    -- ** HyperParameterTuningJobObjective
    , HyperParameterTuningJobObjective
    , hyperParameterTuningJobObjective
    , hptjoType
    , hptjoMetricName

    -- ** HyperParameterTuningJobSummary
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

    -- ** HyperParameterTuningJobWarmStartConfig
    , HyperParameterTuningJobWarmStartConfig
    , hyperParameterTuningJobWarmStartConfig
    , hptjwscParentHyperParameterTuningJobs
    , hptjwscWarmStartType

    -- ** InferenceSpecification
    , InferenceSpecification
    , inferenceSpecification
    , isContainers
    , isSupportedTransformInstanceTypes
    , isSupportedRealtimeInferenceInstanceTypes
    , isSupportedContentTypes
    , isSupportedResponseMIMETypes

    -- ** InputConfig
    , InputConfig
    , inputConfig
    , icS3URI
    , icDataInputConfig
    , icFramework

    -- ** IntegerParameterRange
    , IntegerParameterRange
    , integerParameterRange
    , iprScalingType
    , iprName
    , iprMinValue
    , iprMaxValue

    -- ** IntegerParameterRangeSpecification
    , IntegerParameterRangeSpecification
    , integerParameterRangeSpecification
    , iprsMinValue
    , iprsMaxValue

    -- ** LabelCounters
    , LabelCounters
    , labelCounters
    , lcMachineLabeled
    , lcTotalLabeled
    , lcFailedNonRetryableError
    , lcUnlabeled
    , lcHumanLabeled

    -- ** LabelCountersForWorkteam
    , LabelCountersForWorkteam
    , labelCountersForWorkteam
    , lcfwPendingHuman
    , lcfwTotal
    , lcfwHumanLabeled

    -- ** LabelingJobAlgorithmsConfig
    , LabelingJobAlgorithmsConfig
    , labelingJobAlgorithmsConfig
    , ljacLabelingJobResourceConfig
    , ljacInitialActiveLearningModelARN
    , ljacLabelingJobAlgorithmSpecificationARN

    -- ** LabelingJobDataAttributes
    , LabelingJobDataAttributes
    , labelingJobDataAttributes
    , ljdaContentClassifiers

    -- ** LabelingJobDataSource
    , LabelingJobDataSource
    , labelingJobDataSource
    , ljdsS3DataSource

    -- ** LabelingJobForWorkteamSummary
    , LabelingJobForWorkteamSummary
    , labelingJobForWorkteamSummary
    , ljfwsLabelCounters
    , ljfwsLabelingJobName
    , ljfwsJobReferenceCode
    , ljfwsWorkRequesterAccountId
    , ljfwsCreationTime

    -- ** LabelingJobInputConfig
    , LabelingJobInputConfig
    , labelingJobInputConfig
    , ljicDataAttributes
    , ljicDataSource

    -- ** LabelingJobOutput
    , LabelingJobOutput
    , labelingJobOutput
    , ljoFinalActiveLearningModelARN
    , ljoOutputDatasetS3URI

    -- ** LabelingJobOutputConfig
    , LabelingJobOutputConfig
    , labelingJobOutputConfig
    , ljocKMSKeyId
    , ljocS3OutputPath

    -- ** LabelingJobResourceConfig
    , LabelingJobResourceConfig
    , labelingJobResourceConfig
    , ljrcVolumeKMSKeyId

    -- ** LabelingJobS3DataSource
    , LabelingJobS3DataSource
    , labelingJobS3DataSource
    , ljsdsManifestS3URI

    -- ** LabelingJobStoppingConditions
    , LabelingJobStoppingConditions
    , labelingJobStoppingConditions
    , ljscMaxHumanLabeledObjectCount
    , ljscMaxPercentageOfInputDatasetLabeled

    -- ** LabelingJobSummary
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

    -- ** MemberDefinition
    , MemberDefinition
    , memberDefinition
    , mdCognitoMemberDefinition

    -- ** MetricData
    , MetricData
    , metricData
    , mdMetricName
    , mdValue
    , mdTimestamp

    -- ** MetricDefinition
    , MetricDefinition
    , metricDefinition
    , mdName
    , mdRegex

    -- ** ModelArtifacts
    , ModelArtifacts
    , modelArtifacts
    , maS3ModelArtifacts

    -- ** ModelPackageContainerDefinition
    , ModelPackageContainerDefinition
    , modelPackageContainerDefinition
    , mpcdModelDataURL
    , mpcdImageDigest
    , mpcdContainerHostname
    , mpcdProductId
    , mpcdImage

    -- ** ModelPackageStatusDetails
    , ModelPackageStatusDetails
    , modelPackageStatusDetails
    , mpsdImageScanStatuses
    , mpsdValidationStatuses

    -- ** ModelPackageStatusItem
    , ModelPackageStatusItem
    , modelPackageStatusItem
    , mpsiFailureReason
    , mpsiName
    , mpsiStatus

    -- ** ModelPackageSummary
    , ModelPackageSummary
    , modelPackageSummary
    , mpsModelPackageDescription
    , mpsModelPackageName
    , mpsModelPackageARN
    , mpsCreationTime
    , mpsModelPackageStatus

    -- ** ModelPackageValidationProfile
    , ModelPackageValidationProfile
    , modelPackageValidationProfile
    , mpvpProfileName
    , mpvpTransformJobDefinition

    -- ** ModelPackageValidationSpecification
    , ModelPackageValidationSpecification
    , modelPackageValidationSpecification
    , mpvsValidationRole
    , mpvsValidationProfiles

    -- ** ModelSummary
    , ModelSummary
    , modelSummary
    , msModelName
    , msModelARN
    , msCreationTime

    -- ** NestedFilters
    , NestedFilters
    , nestedFilters
    , nfNestedPropertyName
    , nfFilters

    -- ** NotebookInstanceLifecycleConfigSummary
    , NotebookInstanceLifecycleConfigSummary
    , notebookInstanceLifecycleConfigSummary
    , nilcsCreationTime
    , nilcsLastModifiedTime
    , nilcsNotebookInstanceLifecycleConfigName
    , nilcsNotebookInstanceLifecycleConfigARN

    -- ** NotebookInstanceLifecycleHook
    , NotebookInstanceLifecycleHook
    , notebookInstanceLifecycleHook
    , nilhContent

    -- ** NotebookInstanceSummary
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

    -- ** ObjectiveStatusCounters
    , ObjectiveStatusCounters
    , objectiveStatusCounters
    , oscPending
    , oscSucceeded
    , oscFailed

    -- ** OutputConfig
    , OutputConfig
    , outputConfig
    , ocS3OutputLocation
    , ocTargetDevice

    -- ** OutputDataConfig
    , OutputDataConfig
    , outputDataConfig
    , odcKMSKeyId
    , odcS3OutputPath

    -- ** ParameterRange
    , ParameterRange
    , parameterRange
    , prCategoricalParameterRangeSpecification
    , prIntegerParameterRangeSpecification
    , prContinuousParameterRangeSpecification

    -- ** ParameterRanges
    , ParameterRanges
    , parameterRanges
    , prCategoricalParameterRanges
    , prIntegerParameterRanges
    , prContinuousParameterRanges

    -- ** ParentHyperParameterTuningJob
    , ParentHyperParameterTuningJob
    , parentHyperParameterTuningJob
    , phptjHyperParameterTuningJobName

    -- ** ProductionVariant
    , ProductionVariant
    , productionVariant
    , pvAcceleratorType
    , pvInitialVariantWeight
    , pvVariantName
    , pvModelName
    , pvInitialInstanceCount
    , pvInstanceType

    -- ** ProductionVariantSummary
    , ProductionVariantSummary
    , productionVariantSummary
    , pvsDesiredInstanceCount
    , pvsDesiredWeight
    , pvsCurrentWeight
    , pvsCurrentInstanceCount
    , pvsDeployedImages
    , pvsVariantName

    -- ** PropertyNameQuery
    , PropertyNameQuery
    , propertyNameQuery
    , pnqPropertyNameHint

    -- ** PropertyNameSuggestion
    , PropertyNameSuggestion
    , propertyNameSuggestion
    , pnsPropertyName

    -- ** PublicWorkforceTaskPrice
    , PublicWorkforceTaskPrice
    , publicWorkforceTaskPrice
    , pwtpAmountInUsd

    -- ** RenderableTask
    , RenderableTask
    , renderableTask
    , rtInput

    -- ** RenderingError
    , RenderingError
    , renderingError
    , reCode
    , reMessage

    -- ** ResourceConfig
    , ResourceConfig
    , resourceConfig
    , rcVolumeKMSKeyId
    , rcInstanceType
    , rcInstanceCount
    , rcVolumeSizeInGB

    -- ** ResourceLimits
    , ResourceLimits
    , resourceLimits
    , rlMaxNumberOfTrainingJobs
    , rlMaxParallelTrainingJobs

    -- ** S3DataSource
    , S3DataSource
    , s3DataSource
    , sdsS3DataDistributionType
    , sdsAttributeNames
    , sdsS3DataType
    , sdsS3URI

    -- ** SearchExpression
    , SearchExpression
    , searchExpression
    , seSubExpressions
    , seOperator
    , seFilters
    , seNestedFilters

    -- ** SearchRecord
    , SearchRecord
    , searchRecord
    , srTrainingJob

    -- ** SecondaryStatusTransition
    , SecondaryStatusTransition
    , secondaryStatusTransition
    , sstStatusMessage
    , sstEndTime
    , sstStatus
    , sstStartTime

    -- ** ShuffleConfig
    , ShuffleConfig
    , shuffleConfig
    , scSeed

    -- ** SourceAlgorithm
    , SourceAlgorithm
    , sourceAlgorithm
    , saModelDataURL
    , saAlgorithmName

    -- ** SourceAlgorithmSpecification
    , SourceAlgorithmSpecification
    , sourceAlgorithmSpecification
    , sasSourceAlgorithms

    -- ** StoppingCondition
    , StoppingCondition
    , stoppingCondition
    , scMaxRuntimeInSeconds

    -- ** SubscribedWorkteam
    , SubscribedWorkteam
    , subscribedWorkteam
    , swMarketplaceTitle
    , swSellerName
    , swListingId
    , swMarketplaceDescription
    , swWorkteamARN

    -- ** SuggestionQuery
    , SuggestionQuery
    , suggestionQuery
    , sqPropertyNameQuery

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- ** TrainingJob
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

    -- ** TrainingJobDefinition
    , TrainingJobDefinition
    , trainingJobDefinition
    , tjdHyperParameters
    , tjdTrainingInputMode
    , tjdInputDataConfig
    , tjdOutputDataConfig
    , tjdResourceConfig
    , tjdStoppingCondition

    -- ** TrainingJobStatusCounters
    , TrainingJobStatusCounters
    , trainingJobStatusCounters
    , tjscStopped
    , tjscRetryableError
    , tjscInProgress
    , tjscNonRetryableError
    , tjscCompleted

    -- ** TrainingJobSummary
    , TrainingJobSummary
    , trainingJobSummary
    , tTrainingEndTime
    , tLastModifiedTime
    , tTrainingJobName
    , tTrainingJobARN
    , tCreationTime
    , tTrainingJobStatus

    -- ** TrainingSpecification
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

    -- ** TransformDataSource
    , TransformDataSource
    , transformDataSource
    , tdsS3DataSource

    -- ** TransformInput
    , TransformInput
    , transformInput
    , tiSplitType
    , tiCompressionType
    , tiContentType
    , tiDataSource

    -- ** TransformJobDefinition
    , TransformJobDefinition
    , transformJobDefinition
    , tjdBatchStrategy
    , tjdMaxPayloadInMB
    , tjdEnvironment
    , tjdMaxConcurrentTransforms
    , tjdTransformInput
    , tjdTransformOutput
    , tjdTransformResources

    -- ** TransformJobSummary
    , TransformJobSummary
    , transformJobSummary
    , tjsFailureReason
    , tjsLastModifiedTime
    , tjsTransformEndTime
    , tjsTransformJobName
    , tjsTransformJobARN
    , tjsCreationTime
    , tjsTransformJobStatus

    -- ** TransformOutput
    , TransformOutput
    , transformOutput
    , toAssembleWith
    , toAccept
    , toKMSKeyId
    , toS3OutputPath

    -- ** TransformResources
    , TransformResources
    , transformResources
    , trVolumeKMSKeyId
    , trInstanceType
    , trInstanceCount

    -- ** TransformS3DataSource
    , TransformS3DataSource
    , transformS3DataSource
    , tsdsS3DataType
    , tsdsS3URI

    -- ** USD
    , USD
    , uSD
    , usdCents
    , usdDollars
    , usdTenthFractionsOfACent

    -- ** UiConfig
    , UiConfig
    , uiConfig
    , ucUiTemplateS3URI

    -- ** UiTemplate
    , UiTemplate
    , uiTemplate
    , utContent

    -- ** VPCConfig
    , VPCConfig
    , vpcConfig
    , vcSecurityGroupIds
    , vcSubnets

    -- ** Workteam
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

import Network.AWS.SageMaker.AddTags
import Network.AWS.SageMaker.CreateAlgorithm
import Network.AWS.SageMaker.CreateCodeRepository
import Network.AWS.SageMaker.CreateCompilationJob
import Network.AWS.SageMaker.CreateEndpoint
import Network.AWS.SageMaker.CreateEndpointConfig
import Network.AWS.SageMaker.CreateHyperParameterTuningJob
import Network.AWS.SageMaker.CreateLabelingJob
import Network.AWS.SageMaker.CreateModel
import Network.AWS.SageMaker.CreateModelPackage
import Network.AWS.SageMaker.CreateNotebookInstance
import Network.AWS.SageMaker.CreateNotebookInstanceLifecycleConfig
import Network.AWS.SageMaker.CreatePresignedNotebookInstanceURL
import Network.AWS.SageMaker.CreateTrainingJob
import Network.AWS.SageMaker.CreateTransformJob
import Network.AWS.SageMaker.CreateWorkteam
import Network.AWS.SageMaker.DeleteAlgorithm
import Network.AWS.SageMaker.DeleteCodeRepository
import Network.AWS.SageMaker.DeleteEndpoint
import Network.AWS.SageMaker.DeleteEndpointConfig
import Network.AWS.SageMaker.DeleteModel
import Network.AWS.SageMaker.DeleteModelPackage
import Network.AWS.SageMaker.DeleteNotebookInstance
import Network.AWS.SageMaker.DeleteNotebookInstanceLifecycleConfig
import Network.AWS.SageMaker.DeleteTags
import Network.AWS.SageMaker.DeleteWorkteam
import Network.AWS.SageMaker.DescribeAlgorithm
import Network.AWS.SageMaker.DescribeCodeRepository
import Network.AWS.SageMaker.DescribeCompilationJob
import Network.AWS.SageMaker.DescribeEndpoint
import Network.AWS.SageMaker.DescribeEndpointConfig
import Network.AWS.SageMaker.DescribeHyperParameterTuningJob
import Network.AWS.SageMaker.DescribeLabelingJob
import Network.AWS.SageMaker.DescribeModel
import Network.AWS.SageMaker.DescribeModelPackage
import Network.AWS.SageMaker.DescribeNotebookInstance
import Network.AWS.SageMaker.DescribeNotebookInstanceLifecycleConfig
import Network.AWS.SageMaker.DescribeSubscribedWorkteam
import Network.AWS.SageMaker.DescribeTrainingJob
import Network.AWS.SageMaker.DescribeTransformJob
import Network.AWS.SageMaker.DescribeWorkteam
import Network.AWS.SageMaker.GetSearchSuggestions
import Network.AWS.SageMaker.ListAlgorithms
import Network.AWS.SageMaker.ListCodeRepositories
import Network.AWS.SageMaker.ListCompilationJobs
import Network.AWS.SageMaker.ListEndpointConfigs
import Network.AWS.SageMaker.ListEndpoints
import Network.AWS.SageMaker.ListHyperParameterTuningJobs
import Network.AWS.SageMaker.ListLabelingJobs
import Network.AWS.SageMaker.ListLabelingJobsForWorkteam
import Network.AWS.SageMaker.ListModelPackages
import Network.AWS.SageMaker.ListModels
import Network.AWS.SageMaker.ListNotebookInstanceLifecycleConfigs
import Network.AWS.SageMaker.ListNotebookInstances
import Network.AWS.SageMaker.ListSubscribedWorkteams
import Network.AWS.SageMaker.ListTags
import Network.AWS.SageMaker.ListTrainingJobs
import Network.AWS.SageMaker.ListTrainingJobsForHyperParameterTuningJob
import Network.AWS.SageMaker.ListTransformJobs
import Network.AWS.SageMaker.ListWorkteams
import Network.AWS.SageMaker.RenderUiTemplate
import Network.AWS.SageMaker.Search
import Network.AWS.SageMaker.StartNotebookInstance
import Network.AWS.SageMaker.StopCompilationJob
import Network.AWS.SageMaker.StopHyperParameterTuningJob
import Network.AWS.SageMaker.StopLabelingJob
import Network.AWS.SageMaker.StopNotebookInstance
import Network.AWS.SageMaker.StopTrainingJob
import Network.AWS.SageMaker.StopTransformJob
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.UpdateCodeRepository
import Network.AWS.SageMaker.UpdateEndpoint
import Network.AWS.SageMaker.UpdateEndpointWeightsAndCapacities
import Network.AWS.SageMaker.UpdateNotebookInstance
import Network.AWS.SageMaker.UpdateNotebookInstanceLifecycleConfig
import Network.AWS.SageMaker.UpdateWorkteam
import Network.AWS.SageMaker.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'SageMaker'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _LifecycleHookLimitExceededException
    , _InvalidTimeRangeException
    , _InvalidComputePlatformException
    , _InvalidTagException
    , _InvalidFileExistsBehaviorException
    , _InvalidAlarmConfigException
    , _InstanceNameAlreadyRegisteredException
    , _IamUserArnRequiredException
    , _InvalidDeploymentGroupNameException
    , _InvalidInstanceTypeException
    , _IamSessionArnAlreadyRegisteredException
    , _InvalidTrafficRoutingConfigurationException
    , _DescriptionTooLongException
    , _InvalidIamUserArnException
    , _InvalidOnPremisesTagCombinationException
    , _DeploymentNotStartedException
    , _DeploymentConfigLimitExceededException
    , _RoleRequiredException
    , _InvalidLoadBalancerInfoException
    , _InvalidBlueGreenDeploymentConfigurationException
    , _InvalidRoleException
    , _DeploymentConfigAlreadyExistsException
    , _InvalidTargetInstancesException
    , _InvalidTagsToAddException
    , _DeploymentLimitExceededException
    , _IamUserArnAlreadyRegisteredException
    , _InvalidIamSessionArnException
    , _InstanceLimitExceededException
    , _InvalidLifecycleEventHookExecutionIdException
    , _InvalidDeploymentStyleException
    , _InvalidTargetFilterNameException
    , _DeploymentTargetListSizeExceededException
    , _InvalidDeployedStateFilterException
    , _InvalidAutoScalingGroupException
    , _InvalidApplicationNameException
    , _GitHubAccountTokenDoesNotExistException
    , _ApplicationDoesNotExistException
    , _InvalidMinimumHealthyHostValueException
    , _UnsupportedActionForDeploymentTypeException
    , _ResourceValidationException
    , _ArnNotSupportedException
    , _InvalidGitHubAccountTokenException
    , _InvalidEC2TagCombinationException
    , _InvalidLifecycleEventHookExecutionStatusException
    , _AlarmsLimitExceededException
    , _OperationNotSupportedException
    , _DeploymentTargetDoesNotExistException
    , _InvalidTagFilterException
    , _InvalidTriggerConfigException
    , _InvalidDeploymentWaitTypeException
    , _InvalidIgnoreApplicationStopFailuresValueException
    , _InvalidUpdateOutdatedInstancesOnlyValueException
    , _TagRequiredException
    , _DeploymentGroupNameRequiredException
    , _BucketNameFilterRequiredException
    , _DeploymentConfigDoesNotExistException
    , _InvalidBucketNameFilterException
    , _DeploymentGroupAlreadyExistsException
    , _InvalidSortByException
    , _RevisionDoesNotExistException
    , _InvalidTargetException
    , _DeploymentGroupLimitExceededException
    , _DeploymentGroupDoesNotExistException
    , _ThrottlingException
    , _InvalidDeploymentConfigNameException
    , _DeploymentConfigNameRequiredException
    , _DeploymentIdRequiredException
    , _InvalidInstanceIdException
    , _DeploymentIsNotInReadyStateException
    , _InvalidTargetGroupPairException
    , _InvalidNextTokenException
    , _InstanceIdRequiredException
    , _InvalidDeploymentIdException
    , _InvalidSortOrderException
    , _InvalidAutoRollbackConfigException
    , _DeploymentAlreadyCompletedException
    , _ECSServiceMappingLimitExceededException
    , _DeploymentDoesNotExistException
    , _BatchLimitExceededException
    , _InvalidRevisionException
    , _RevisionRequiredException
    , _InstanceDoesNotExistException
    , _DeploymentConfigInUseException
    , _InvalidDeploymentConfigIdException
    , _InvalidInputException
    , _InvalidEC2TagException
    , _InvalidInstanceNameException
    , _InstanceNameRequiredException
    , _MultipleIamArnsProvidedException
    , _TriggerTargetsLimitExceededException
    , _InvalidDeploymentStatusException
    , _InvalidRegistrationStatusException
    , _ApplicationNameRequiredException
    , _InstanceNotRegisteredException
    , _ApplicationAlreadyExistsException
    , _InvalidInstanceStatusException
    , _InvalidDeploymentTargetIdException
    , _TagLimitExceededException
    , _ApplicationLimitExceededException
    , _TagSetListLimitExceededException
    , _InvalidArnException
    , _InvalidOperationException
    , _DeploymentTargetIdRequiredException
    , _GitHubAccountTokenNameRequiredException
    , _InvalidECSServiceException
    , _InvalidDeploymentInstanceTypeException
    , _InvalidExternalIdException
    , _IamArnRequiredException
    , _InvalidGitHubAccountTokenNameException
    , _LifecycleEventAlreadyCompletedException
    , _InvalidKeyPrefixFilterException
    , _ResourceArnRequiredException
    , _DeploymentAlreadyStartedException

    -- * CommitId
    , CommitId (..)

    -- * AlarmName
    , AlarmName (..)

    -- * TrafficRoute
    , TrafficRoute (..)
    , mkTrafficRoute
    , trListenerArns

    -- * DeploymentId
    , DeploymentId (..)

    -- * TriggerEventType
    , TriggerEventType (..)

    -- * GenericRevisionInfo
    , GenericRevisionInfo (..)
    , mkGenericRevisionInfo
    , griDeploymentGroups
    , griDescription
    , griFirstUsedTime
    , griLastUsedTime
    , griRegisterTime

    -- * EC2TagSet
    , EC2TagSet (..)
    , mkEC2TagSet
    , ectsEc2TagSetList

    -- * TargetId
    , TargetId (..)

    -- * DeploymentConfigName
    , DeploymentConfigName (..)

    -- * TargetArn
    , TargetArn (..)

    -- * InstanceArn
    , InstanceArn (..)

    -- * ApplicationInfo
    , ApplicationInfo (..)
    , mkApplicationInfo
    , aiApplicationId
    , aiApplicationName
    , aiComputePlatform
    , aiCreateTime
    , aiGitHubAccountName
    , aiLinkedToGitHub

    -- * DeploymentReadyAction
    , DeploymentReadyAction (..)

    -- * BundleType
    , BundleType (..)

    -- * TriggerName
    , TriggerName (..)

    -- * ETag
    , ETag (..)

    -- * OnPremisesTagSet
    , OnPremisesTagSet (..)
    , mkOnPremisesTagSet
    , optsOnPremisesTagSetList

    -- * ECSServiceName
    , ECSServiceName (..)

    -- * InstanceAction
    , InstanceAction (..)

    -- * TargetLabel
    , TargetLabel (..)

    -- * VersionId
    , VersionId (..)

    -- * S3Key
    , S3Key (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * Repository
    , Repository (..)

    -- * TimeBasedCanary
    , TimeBasedCanary (..)
    , mkTimeBasedCanary
    , tbcCanaryInterval
    , tbcCanaryPercentage

    -- * DeploymentOption
    , DeploymentOption (..)

    -- * TagFilterType
    , TagFilterType (..)

    -- * DeploymentWaitType
    , DeploymentWaitType (..)

    -- * TimeRange
    , TimeRange (..)
    , mkTimeRange
    , trEnd
    , trStart

    -- * LambdaFunctionName
    , LambdaFunctionName (..)

    -- * ComputePlatform
    , ComputePlatform (..)

    -- * DeploymentCreator
    , DeploymentCreator (..)

    -- * LifecycleEventName
    , LifecycleEventName (..)

    -- * LastDeploymentInfo
    , LastDeploymentInfo (..)
    , mkLastDeploymentInfo
    , ldiCreateTime
    , ldiDeploymentId
    , ldiEndTime
    , ldiStatus

    -- * AutoScalingGroup
    , AutoScalingGroup (..)
    , mkAutoScalingGroup
    , asgHook
    , asgName

    -- * Arn
    , Arn (..)

    -- * ELBInfo
    , ELBInfo (..)
    , mkELBInfo
    , elbiName

    -- * AutoRollbackEvent
    , AutoRollbackEvent (..)

    -- * LifecycleMessage
    , LifecycleMessage (..)

    -- * DeploymentGroupInfo
    , DeploymentGroupInfo (..)
    , mkDeploymentGroupInfo
    , dgiAlarmConfiguration
    , dgiApplicationName
    , dgiAutoRollbackConfiguration
    , dgiAutoScalingGroups
    , dgiBlueGreenDeploymentConfiguration
    , dgiComputePlatform
    , dgiDeploymentConfigName
    , dgiDeploymentGroupId
    , dgiDeploymentGroupName
    , dgiDeploymentStyle
    , dgiEc2TagFilters
    , dgiEc2TagSet
    , dgiEcsServices
    , dgiLastAttemptedDeployment
    , dgiLastSuccessfulDeployment
    , dgiLoadBalancerInfo
    , dgiOnPremisesInstanceTagFilters
    , dgiOnPremisesTagSet
    , dgiServiceRoleArn
    , dgiTargetRevision
    , dgiTriggerConfigurations

    -- * ApplicationRevisionSortBy
    , ApplicationRevisionSortBy (..)

    -- * ECSClusterName
    , ECSClusterName (..)

    -- * MinimumHealthyHosts
    , MinimumHealthyHosts (..)
    , mkMinimumHealthyHosts
    , mhhType
    , mhhValue

    -- * InstanceTarget
    , InstanceTarget (..)
    , mkInstanceTarget
    , itDeploymentId
    , itInstanceLabel
    , itLastUpdatedAt
    , itLifecycleEvents
    , itStatus
    , itTargetArn
    , itTargetId

    -- * TargetGroupInfo
    , TargetGroupInfo (..)
    , mkTargetGroupInfo
    , tgiName

    -- * DeploymentTarget
    , DeploymentTarget (..)
    , mkDeploymentTarget
    , dtCloudFormationTarget
    , dtDeploymentTargetType
    , dtEcsTarget
    , dtInstanceTarget
    , dtLambdaTarget

    -- * ListStateFilterAction
    , ListStateFilterAction (..)

    -- * CloudFormationTarget
    , CloudFormationTarget (..)
    , mkCloudFormationTarget
    , cftDeploymentId
    , cftLastUpdatedAt
    , cftLifecycleEvents
    , cftResourceType
    , cftStatus
    , cftTargetId
    , cftTargetVersionWeight

    -- * Value
    , Value (..)

    -- * LifecycleErrorCode
    , LifecycleErrorCode (..)

    -- * ListenerArn
    , ListenerArn (..)

    -- * RevisionLocation
    , RevisionLocation (..)
    , mkRevisionLocation
    , rlAppSpecContent
    , rlGitHubLocation
    , rlRevisionType
    , rlS3Location
    , rlString

    -- * LifecycleEventStatus
    , LifecycleEventStatus (..)

    -- * EC2TagFilter
    , EC2TagFilter (..)
    , mkEC2TagFilter
    , ectfKey
    , ectfType
    , ectfValue

    -- * ECSTaskSetStatus
    , ECSTaskSetStatus (..)

    -- * TargetFilterName
    , TargetFilterName (..)

    -- * DeploymentReadyOption
    , DeploymentReadyOption (..)
    , mkDeploymentReadyOption
    , droActionOnTimeout
    , droWaitTimeInMinutes

    -- * TimeBasedLinear
    , TimeBasedLinear (..)
    , mkTimeBasedLinear
    , tblLinearInterval
    , tblLinearPercentage

    -- * IamUserArn
    , IamUserArn (..)

    -- * DeploymentType
    , DeploymentType (..)

    -- * TrafficRoutingType
    , TrafficRoutingType (..)

    -- * RawString
    , RawString (..)
    , mkRawString
    , rsContent
    , rsSha256

    -- * ECSTarget
    , ECSTarget (..)
    , mkECSTarget
    , ecstDeploymentId
    , ecstLastUpdatedAt
    , ecstLifecycleEvents
    , ecstStatus
    , ecstTargetArn
    , ecstTargetId
    , ecstTaskSetsInfo

    -- * Diagnostics
    , Diagnostics (..)
    , mkDiagnostics
    , dErrorCode
    , dLogTail
    , dMessage
    , dScriptName

    -- * NextToken
    , NextToken (..)

    -- * TriggerTargetArn
    , TriggerTargetArn (..)

    -- * BlueGreenDeploymentConfiguration
    , BlueGreenDeploymentConfiguration (..)
    , mkBlueGreenDeploymentConfiguration
    , bgdcDeploymentReadyOption
    , bgdcGreenFleetProvisioningOption
    , bgdcTerminateBlueInstancesOnDeploymentSuccess

    -- * LogTail
    , LogTail (..)

    -- * StopStatus
    , StopStatus (..)

    -- * ErrorInformation
    , ErrorInformation (..)
    , mkErrorInformation
    , eiCode
    , eiMessage

    -- * LoadBalancerInfo
    , LoadBalancerInfo (..)
    , mkLoadBalancerInfo
    , lbiElbInfoList
    , lbiTargetGroupInfoList
    , lbiTargetGroupPairInfoList

    -- * SortOrder
    , SortOrder (..)

    -- * ApplicationId
    , ApplicationId (..)

    -- * InstanceInfo
    , InstanceInfo (..)
    , mkInstanceInfo
    , iiDeregisterTime
    , iiIamSessionArn
    , iiIamUserArn
    , iiInstanceArn
    , iiInstanceName
    , iiRegisterTime
    , iiTags

    -- * GreenFleetProvisioningOption
    , GreenFleetProvisioningOption (..)
    , mkGreenFleetProvisioningOption
    , gfpoAction

    -- * Key
    , Key (..)

    -- * DeploymentInfo
    , DeploymentInfo (..)
    , mkDeploymentInfo
    , diAdditionalDeploymentStatusInfo
    , diApplicationName
    , diAutoRollbackConfiguration
    , diBlueGreenDeploymentConfiguration
    , diCompleteTime
    , diComputePlatform
    , diCreateTime
    , diCreator
    , diDeploymentConfigName
    , diDeploymentGroupName
    , diDeploymentId
    , diDeploymentOverview
    , diDeploymentStatusMessages
    , diDeploymentStyle
    , diDescription
    , diErrorInformation
    , diExternalId
    , diFileExistsBehavior
    , diIgnoreApplicationStopFailures
    , diInstanceTerminationWaitTimeStarted
    , diLoadBalancerInfo
    , diPreviousRevision
    , diRevision
    , diRollbackInfo
    , diStartTime
    , diStatus
    , diTargetInstances
    , diUpdateOutdatedInstancesOnly

    -- * GreenFleetProvisioningAction
    , GreenFleetProvisioningAction (..)

    -- * RawStringSha256
    , RawStringSha256 (..)

    -- * TargetGroupPairInfo
    , TargetGroupPairInfo (..)
    , mkTargetGroupPairInfo
    , tgpiProdTrafficRoute
    , tgpiTargetGroups
    , tgpiTestTrafficRoute

    -- * FileExistsBehavior
    , FileExistsBehavior (..)

    -- * AutoScalingGroupName
    , AutoScalingGroupName (..)

    -- * TagFilter
    , TagFilter (..)
    , mkTagFilter
    , tfKey
    , tfType
    , tfValue

    -- * Version
    , Version (..)

    -- * LifecycleEvent
    , LifecycleEvent (..)
    , mkLifecycleEvent
    , leDiagnostics
    , leEndTime
    , leLifecycleEventName
    , leStartTime
    , leStatus

    -- * AdditionalDeploymentStatusInfo
    , AdditionalDeploymentStatusInfo (..)

    -- * ECSTaskSet
    , ECSTaskSet (..)
    , mkECSTaskSet
    , ecstsDesiredCount
    , ecstsIdentifer
    , ecstsPendingCount
    , ecstsRunningCount
    , ecstsStatus
    , ecstsTargetGroup
    , ecstsTaskSetLabel
    , ecstsTrafficWeight

    -- * TriggerConfig
    , TriggerConfig (..)
    , mkTriggerConfig
    , tcTriggerEvents
    , tcTriggerName
    , tcTriggerTargetArn

    -- * DeploymentOverview
    , DeploymentOverview (..)
    , mkDeploymentOverview
    , doFailed
    , doInProgress
    , doPending
    , doReady
    , doSkipped
    , doSucceeded

    -- * AppSpecContent
    , AppSpecContent (..)
    , mkAppSpecContent
    , ascContent
    , ascSha256

    -- * DeployErrorCode
    , DeployErrorCode (..)

    -- * ApplicationName
    , ApplicationName (..)

    -- * DeploymentConfigInfo
    , DeploymentConfigInfo (..)
    , mkDeploymentConfigInfo
    , dciComputePlatform
    , dciCreateTime
    , dciDeploymentConfigId
    , dciDeploymentConfigName
    , dciMinimumHealthyHosts
    , dciTrafficRoutingConfig

    -- * RevisionInfo
    , RevisionInfo (..)
    , mkRevisionInfo
    , riGenericRevisionInfo
    , riRevisionLocation

    -- * TargetStatus
    , TargetStatus (..)

    -- * DeploymentStatus
    , DeploymentStatus (..)

    -- * RegistrationStatus
    , RegistrationStatus (..)

    -- * S3Location
    , S3Location (..)
    , mkS3Location
    , slBucket
    , slBundleType
    , slETag
    , slKey
    , slVersion

    -- * ScriptName
    , ScriptName (..)

    -- * ECSService
    , ECSService (..)
    , mkECSService
    , ecssClusterName
    , ecssServiceName

    -- * BlueInstanceTerminationOption
    , BlueInstanceTerminationOption (..)
    , mkBlueInstanceTerminationOption
    , bitoAction
    , bitoTerminationWaitTimeInMinutes

    -- * RollbackInfo
    , RollbackInfo (..)
    , mkRollbackInfo
    , riRollbackDeploymentId
    , riRollbackMessage
    , riRollbackTriggeringDeploymentId

    -- * DeploymentTargetType
    , DeploymentTargetType (..)

    -- * MinimumHealthyHostsType
    , MinimumHealthyHostsType (..)

    -- * GitHubAccountTokenName
    , GitHubAccountTokenName (..)

    -- * ExternalId
    , ExternalId (..)

    -- * GitHubLocation
    , GitHubLocation (..)
    , mkGitHubLocation
    , ghlCommitId
    , ghlRepository

    -- * TrafficRoutingConfig
    , TrafficRoutingConfig (..)
    , mkTrafficRoutingConfig
    , trcTimeBasedCanary
    , trcTimeBasedLinear
    , trcType

    -- * FilterValue
    , FilterValue (..)

    -- * LambdaFunctionInfo
    , LambdaFunctionInfo (..)
    , mkLambdaFunctionInfo
    , lfiCurrentVersion
    , lfiFunctionAlias
    , lfiFunctionName
    , lfiTargetVersion
    , lfiTargetVersionWeight

    -- * DeploymentGroupId
    , DeploymentGroupId (..)

    -- * ErrorMessage
    , ErrorMessage (..)

    -- * Message
    , Message (..)

    -- * Alarm
    , Alarm (..)
    , mkAlarm
    , aName

    -- * AlarmConfiguration
    , AlarmConfiguration (..)
    , mkAlarmConfiguration
    , acAlarms
    , acEnabled
    , acIgnorePollAlarmFailure

    -- * TargetInstances
    , TargetInstances (..)
    , mkTargetInstances
    , tiAutoScalingGroups
    , tiEc2TagSet
    , tiTagFilters

    -- * LifecycleEventHookExecutionId
    , LifecycleEventHookExecutionId (..)

    -- * Description
    , Description (..)

    -- * InstanceName
    , InstanceName (..)

    -- * RevisionLocationType
    , RevisionLocationType (..)

    -- * DeploymentConfigId
    , DeploymentConfigId (..)

    -- * DeploymentStyle
    , DeploymentStyle (..)
    , mkDeploymentStyle
    , dsDeploymentOption
    , dsDeploymentType

    -- * LambdaTarget
    , LambdaTarget (..)
    , mkLambdaTarget
    , ltDeploymentId
    , ltLambdaFunctionInfo
    , ltLastUpdatedAt
    , ltLifecycleEvents
    , ltStatus
    , ltTargetArn
    , ltTargetId

    -- * EC2TagFilterType
    , EC2TagFilterType (..)

    -- * S3Bucket
    , S3Bucket (..)

    -- * IamSessionArn
    , IamSessionArn (..)

    -- * AutoRollbackConfiguration
    , AutoRollbackConfiguration (..)
    , mkAutoRollbackConfiguration
    , arcEnabled
    , arcEvents

    -- * DeploymentGroupName
    , DeploymentGroupName (..)

    -- * ServiceRoleArn
    , ServiceRoleArn (..)

    -- * GitHubAccountName
    , GitHubAccountName (..)

    -- * Hook
    , Hook (..)

    -- * Name
    , Name (..)

    -- * NewApplicationName
    , NewApplicationName (..)

    -- * TokenName
    , TokenName (..)

    -- * ResourceType
    , ResourceType (..)

    -- * StatusMessage
    , StatusMessage (..)

    -- * CurrentDeploymentGroupName
    , CurrentDeploymentGroupName (..)

    -- * NewDeploymentGroupName
    , NewDeploymentGroupName (..)

    -- * Content
    , Content (..)

    -- * Sha256
    , Sha256 (..)

    -- * Identifer
    , Identifer (..)

    -- * Bucket
    , Bucket (..)

    -- * RollbackMessage
    , RollbackMessage (..)

    -- * FunctionAlias
    , FunctionAlias (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.CodeDeploy.Types.CommitId
  
  
import Network.AWS.CodeDeploy.Types.AlarmName
  
import Network.AWS.CodeDeploy.Types.TrafficRoute
  
  
import Network.AWS.CodeDeploy.Types.DeploymentId
  
import Network.AWS.CodeDeploy.Types.TriggerEventType
  
  
import Network.AWS.CodeDeploy.Types.GenericRevisionInfo
  
  
import Network.AWS.CodeDeploy.Types.EC2TagSet
  
import Network.AWS.CodeDeploy.Types.TargetId
  
import Network.AWS.CodeDeploy.Types.DeploymentConfigName
  
import Network.AWS.CodeDeploy.Types.TargetArn
  
import Network.AWS.CodeDeploy.Types.InstanceArn
  
  
import Network.AWS.CodeDeploy.Types.ApplicationInfo
  
import Network.AWS.CodeDeploy.Types.DeploymentReadyAction
  
import Network.AWS.CodeDeploy.Types.BundleType
  
  
  
import Network.AWS.CodeDeploy.Types.TriggerName
  
  
import Network.AWS.CodeDeploy.Types.ETag
  
  
import Network.AWS.CodeDeploy.Types.OnPremisesTagSet
  
  
import Network.AWS.CodeDeploy.Types.ECSServiceName
  
import Network.AWS.CodeDeploy.Types.InstanceAction
  
  
import Network.AWS.CodeDeploy.Types.TargetLabel
  
import Network.AWS.CodeDeploy.Types.VersionId
  
  
import Network.AWS.CodeDeploy.Types.S3Key
  
  
  
import Network.AWS.CodeDeploy.Types.Tag
  
  
import Network.AWS.CodeDeploy.Types.Repository
  
  
import Network.AWS.CodeDeploy.Types.TimeBasedCanary
  
import Network.AWS.CodeDeploy.Types.DeploymentOption
  
import Network.AWS.CodeDeploy.Types.TagFilterType
  
  
import Network.AWS.CodeDeploy.Types.DeploymentWaitType
  
  
  
  
  
  
import Network.AWS.CodeDeploy.Types.TimeRange
  
import Network.AWS.CodeDeploy.Types.LambdaFunctionName
  
import Network.AWS.CodeDeploy.Types.ComputePlatform
  
import Network.AWS.CodeDeploy.Types.DeploymentCreator
  
import Network.AWS.CodeDeploy.Types.LifecycleEventName
  
import Network.AWS.CodeDeploy.Types.LastDeploymentInfo
  
import Network.AWS.CodeDeploy.Types.AutoScalingGroup
  
import Network.AWS.CodeDeploy.Types.Arn
  
  
import Network.AWS.CodeDeploy.Types.ELBInfo
  
import Network.AWS.CodeDeploy.Types.AutoRollbackEvent
  
  
import Network.AWS.CodeDeploy.Types.LifecycleMessage
  
  
import Network.AWS.CodeDeploy.Types.DeploymentGroupInfo
  
import Network.AWS.CodeDeploy.Types.ApplicationRevisionSortBy
  
  
import Network.AWS.CodeDeploy.Types.ECSClusterName
  
import Network.AWS.CodeDeploy.Types.MinimumHealthyHosts
  
  
  
import Network.AWS.CodeDeploy.Types.InstanceTarget
  
  
import Network.AWS.CodeDeploy.Types.TargetGroupInfo
  
  
import Network.AWS.CodeDeploy.Types.DeploymentTarget
  
  
  
  
import Network.AWS.CodeDeploy.Types.ListStateFilterAction
  
  
  
import Network.AWS.CodeDeploy.Types.CloudFormationTarget
  
  
import Network.AWS.CodeDeploy.Types.Value
  
import Network.AWS.CodeDeploy.Types.LifecycleErrorCode
  
import Network.AWS.CodeDeploy.Types.ListenerArn
  
import Network.AWS.CodeDeploy.Types.RevisionLocation
  
  
import Network.AWS.CodeDeploy.Types.LifecycleEventStatus
  
  
import Network.AWS.CodeDeploy.Types.EC2TagFilter
  
import Network.AWS.CodeDeploy.Types.ECSTaskSetStatus
  
  
import Network.AWS.CodeDeploy.Types.TargetFilterName
  
  
import Network.AWS.CodeDeploy.Types.DeploymentReadyOption
  
  
  
  
import Network.AWS.CodeDeploy.Types.TimeBasedLinear
  
import Network.AWS.CodeDeploy.Types.IamUserArn
  
  
  
import Network.AWS.CodeDeploy.Types.DeploymentType
  
import Network.AWS.CodeDeploy.Types.TrafficRoutingType
  
import Network.AWS.CodeDeploy.Types.RawString
  
  
  
  
import Network.AWS.CodeDeploy.Types.ECSTarget
  
  
import Network.AWS.CodeDeploy.Types.Diagnostics
  
import Network.AWS.CodeDeploy.Types.NextToken
  
  
import Network.AWS.CodeDeploy.Types.TriggerTargetArn
  
  
import Network.AWS.CodeDeploy.Types.BlueGreenDeploymentConfiguration
  
  
import Network.AWS.CodeDeploy.Types.LogTail
  
import Network.AWS.CodeDeploy.Types.StopStatus
  
import Network.AWS.CodeDeploy.Types.ErrorInformation
  
import Network.AWS.CodeDeploy.Types.LoadBalancerInfo
  
import Network.AWS.CodeDeploy.Types.SortOrder
  
  
  
  
  
import Network.AWS.CodeDeploy.Types.ApplicationId
  
import Network.AWS.CodeDeploy.Types.InstanceInfo
  
  
import Network.AWS.CodeDeploy.Types.GreenFleetProvisioningOption
  
  
import Network.AWS.CodeDeploy.Types.Key
  
  
  
  
  
import Network.AWS.CodeDeploy.Types.DeploymentInfo
  
  
import Network.AWS.CodeDeploy.Types.GreenFleetProvisioningAction
  
  
  
  
import Network.AWS.CodeDeploy.Types.RawStringSha256
  
import Network.AWS.CodeDeploy.Types.TargetGroupPairInfo
  
import Network.AWS.CodeDeploy.Types.FileExistsBehavior
  
import Network.AWS.CodeDeploy.Types.AutoScalingGroupName
  
  
import Network.AWS.CodeDeploy.Types.TagFilter
  
  
import Network.AWS.CodeDeploy.Types.Version
  
import Network.AWS.CodeDeploy.Types.LifecycleEvent
  
  
  
import Network.AWS.CodeDeploy.Types.AdditionalDeploymentStatusInfo
  
  
import Network.AWS.CodeDeploy.Types.ECSTaskSet
  
import Network.AWS.CodeDeploy.Types.TriggerConfig
  
import Network.AWS.CodeDeploy.Types.DeploymentOverview
  
  
  
  
import Network.AWS.CodeDeploy.Types.AppSpecContent
  
import Network.AWS.CodeDeploy.Types.DeployErrorCode
  
  
import Network.AWS.CodeDeploy.Types.ApplicationName
  
import Network.AWS.CodeDeploy.Types.DeploymentConfigInfo
  
  
  
  
import Network.AWS.CodeDeploy.Types.RevisionInfo
  
  
import Network.AWS.CodeDeploy.Types.TargetStatus
  
  
  
  
import Network.AWS.CodeDeploy.Types.DeploymentStatus
  
import Network.AWS.CodeDeploy.Types.RegistrationStatus
  
import Network.AWS.CodeDeploy.Types.S3Location
  
import Network.AWS.CodeDeploy.Types.ScriptName
  
  
import Network.AWS.CodeDeploy.Types.ECSService
  
  
  
import Network.AWS.CodeDeploy.Types.BlueInstanceTerminationOption
  
import Network.AWS.CodeDeploy.Types.RollbackInfo
  
  
  
import Network.AWS.CodeDeploy.Types.DeploymentTargetType
  
import Network.AWS.CodeDeploy.Types.MinimumHealthyHostsType
  
import Network.AWS.CodeDeploy.Types.GitHubAccountTokenName
  
import Network.AWS.CodeDeploy.Types.ExternalId
  
import Network.AWS.CodeDeploy.Types.GitHubLocation
  
  
  
  
  
  
import Network.AWS.CodeDeploy.Types.TrafficRoutingConfig
  
import Network.AWS.CodeDeploy.Types.FilterValue
  
  
import Network.AWS.CodeDeploy.Types.LambdaFunctionInfo
  
import Network.AWS.CodeDeploy.Types.DeploymentGroupId
  
import Network.AWS.CodeDeploy.Types.ErrorMessage
  
  
import Network.AWS.CodeDeploy.Types.Message
  
  
  
import Network.AWS.CodeDeploy.Types.Alarm
  
  
  
  
  
  
  
import Network.AWS.CodeDeploy.Types.AlarmConfiguration
  
  
import Network.AWS.CodeDeploy.Types.TargetInstances
  
  
import Network.AWS.CodeDeploy.Types.LifecycleEventHookExecutionId
  
  
  
import Network.AWS.CodeDeploy.Types.Description
  
import Network.AWS.CodeDeploy.Types.InstanceName
  
import Network.AWS.CodeDeploy.Types.RevisionLocationType
  
  
import Network.AWS.CodeDeploy.Types.DeploymentConfigId
  
import Network.AWS.CodeDeploy.Types.DeploymentStyle
  
import Network.AWS.CodeDeploy.Types.LambdaTarget
  
import Network.AWS.CodeDeploy.Types.EC2TagFilterType
  
import Network.AWS.CodeDeploy.Types.S3Bucket
  
  
import Network.AWS.CodeDeploy.Types.IamSessionArn
  
  
  
  
import Network.AWS.CodeDeploy.Types.AutoRollbackConfiguration
  
  
import Network.AWS.CodeDeploy.Types.DeploymentGroupName
  
  
import Network.AWS.CodeDeploy.Types.ServiceRoleArn
  
import Network.AWS.CodeDeploy.Types.GitHubAccountName
  
import Network.AWS.CodeDeploy.Types.Hook
  
import Network.AWS.CodeDeploy.Types.Name
  
import Network.AWS.CodeDeploy.Types.NewApplicationName
  
import Network.AWS.CodeDeploy.Types.TokenName
  
import Network.AWS.CodeDeploy.Types.ResourceType
  
import Network.AWS.CodeDeploy.Types.StatusMessage
  
import Network.AWS.CodeDeploy.Types.CurrentDeploymentGroupName
  
import Network.AWS.CodeDeploy.Types.NewDeploymentGroupName
  
import Network.AWS.CodeDeploy.Types.Content
  
import Network.AWS.CodeDeploy.Types.Sha256
  
import Network.AWS.CodeDeploy.Types.Identifer
  
import Network.AWS.CodeDeploy.Types.Bucket
  
import Network.AWS.CodeDeploy.Types.RollbackMessage
  
import Network.AWS.CodeDeploy.Types.FunctionAlias
  

-- | API version @2014-10-06@ of the Amazon CodeDeploy SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "CodeDeploy",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "codedeploy",
                 Core._svcVersion = "2014-10-06", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "CodeDeploy",
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

-- | The limit for lifecycle hooks was exceeded.
_LifecycleHookLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LifecycleHookLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "LifecycleHookLimitExceededException"
{-# INLINEABLE _LifecycleHookLimitExceededException #-}
{-# DEPRECATED _LifecycleHookLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The specified time range was specified in an invalid format.
_InvalidTimeRangeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTimeRangeException
  = Core._MatchServiceError mkServiceConfig
      "InvalidTimeRangeException"
{-# INLINEABLE _InvalidTimeRangeException #-}
{-# DEPRECATED _InvalidTimeRangeException "Use generic-lens or generic-optics instead"  #-}

-- | The computePlatform is invalid. The computePlatform should be @Lambda@ , @Server@ , or @ECS@ .
_InvalidComputePlatformException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidComputePlatformException
  = Core._MatchServiceError mkServiceConfig
      "InvalidComputePlatformException"
{-# INLINEABLE _InvalidComputePlatformException #-}
{-# DEPRECATED _InvalidComputePlatformException "Use generic-lens or generic-optics instead"  #-}

-- | The tag was specified in an invalid format.
_InvalidTagException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTagException
  = Core._MatchServiceError mkServiceConfig "InvalidTagException"
{-# INLINEABLE _InvalidTagException #-}
{-# DEPRECATED _InvalidTagException "Use generic-lens or generic-optics instead"  #-}

-- | An invalid fileExistsBehavior option was specified to determine how AWS CodeDeploy handles files or directories that already exist in a deployment target location, but weren't part of the previous successful deployment. Valid values include "DISALLOW," "OVERWRITE," and "RETAIN."
_InvalidFileExistsBehaviorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidFileExistsBehaviorException
  = Core._MatchServiceError mkServiceConfig
      "InvalidFileExistsBehaviorException"
{-# INLINEABLE _InvalidFileExistsBehaviorException #-}
{-# DEPRECATED _InvalidFileExistsBehaviorException "Use generic-lens or generic-optics instead"  #-}

-- | The format of the alarm configuration is invalid. Possible causes include:
--
--
--     * The alarm list is null.
--
--
--     * The alarm object is null.
--
--
--     * The alarm name is empty or null or exceeds the limit of 255 characters.
--
--
--     * Two alarms with the same name have been specified.
--
--
--     * The alarm configuration is enabled, but the alarm list is empty.
--
--
_InvalidAlarmConfigException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAlarmConfigException
  = Core._MatchServiceError mkServiceConfig
      "InvalidAlarmConfigException"
{-# INLINEABLE _InvalidAlarmConfigException #-}
{-# DEPRECATED _InvalidAlarmConfigException "Use generic-lens or generic-optics instead"  #-}

-- | The specified on-premises instance name is already registered.
_InstanceNameAlreadyRegisteredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InstanceNameAlreadyRegisteredException
  = Core._MatchServiceError mkServiceConfig
      "InstanceNameAlreadyRegisteredException"
{-# INLINEABLE _InstanceNameAlreadyRegisteredException #-}
{-# DEPRECATED _InstanceNameAlreadyRegisteredException "Use generic-lens or generic-optics instead"  #-}

-- | An IAM user ARN was not specified.
_IamUserArnRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IamUserArnRequiredException
  = Core._MatchServiceError mkServiceConfig
      "IamUserArnRequiredException"
{-# INLINEABLE _IamUserArnRequiredException #-}
{-# DEPRECATED _IamUserArnRequiredException "Use generic-lens or generic-optics instead"  #-}

-- | The deployment group name was specified in an invalid format.
_InvalidDeploymentGroupNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentGroupNameException
  = Core._MatchServiceError mkServiceConfig
      "InvalidDeploymentGroupNameException"
{-# INLINEABLE _InvalidDeploymentGroupNameException #-}
{-# DEPRECATED _InvalidDeploymentGroupNameException "Use generic-lens or generic-optics instead"  #-}

-- | An invalid instance type was specified for instances in a blue/green deployment. Valid values include "Blue" for an original environment and "Green" for a replacement environment.
_InvalidInstanceTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInstanceTypeException
  = Core._MatchServiceError mkServiceConfig
      "InvalidInstanceTypeException"
{-# INLINEABLE _InvalidInstanceTypeException #-}
{-# DEPRECATED _InvalidInstanceTypeException "Use generic-lens or generic-optics instead"  #-}

-- | The request included an IAM session ARN that has already been used to register a different instance.
_IamSessionArnAlreadyRegisteredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IamSessionArnAlreadyRegisteredException
  = Core._MatchServiceError mkServiceConfig
      "IamSessionArnAlreadyRegisteredException"
{-# INLINEABLE _IamSessionArnAlreadyRegisteredException #-}
{-# DEPRECATED _IamSessionArnAlreadyRegisteredException "Use generic-lens or generic-optics instead"  #-}

-- | The configuration that specifies how traffic is routed during a deployment is invalid.
_InvalidTrafficRoutingConfigurationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTrafficRoutingConfigurationException
  = Core._MatchServiceError mkServiceConfig
      "InvalidTrafficRoutingConfigurationException"
{-# INLINEABLE _InvalidTrafficRoutingConfigurationException #-}
{-# DEPRECATED _InvalidTrafficRoutingConfigurationException "Use generic-lens or generic-optics instead"  #-}

-- | The description is too long.
_DescriptionTooLongException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DescriptionTooLongException
  = Core._MatchServiceError mkServiceConfig
      "DescriptionTooLongException"
{-# INLINEABLE _DescriptionTooLongException #-}
{-# DEPRECATED _DescriptionTooLongException "Use generic-lens or generic-optics instead"  #-}

-- | The IAM user ARN was specified in an invalid format.
_InvalidIamUserArnException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidIamUserArnException
  = Core._MatchServiceError mkServiceConfig
      "InvalidIamUserArnException"
{-# INLINEABLE _InvalidIamUserArnException #-}
{-# DEPRECATED _InvalidIamUserArnException "Use generic-lens or generic-optics instead"  #-}

-- | A call was submitted that specified both OnPremisesTagFilters and OnPremisesTagSet, but only one of these data types can be used in a single call.
_InvalidOnPremisesTagCombinationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOnPremisesTagCombinationException
  = Core._MatchServiceError mkServiceConfig
      "InvalidOnPremisesTagCombinationException"
{-# INLINEABLE _InvalidOnPremisesTagCombinationException #-}
{-# DEPRECATED _InvalidOnPremisesTagCombinationException "Use generic-lens or generic-optics instead"  #-}

-- | The specified deployment has not started.
_DeploymentNotStartedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeploymentNotStartedException
  = Core._MatchServiceError mkServiceConfig
      "DeploymentNotStartedException"
{-# INLINEABLE _DeploymentNotStartedException #-}
{-# DEPRECATED _DeploymentNotStartedException "Use generic-lens or generic-optics instead"  #-}

-- | The deployment configurations limit was exceeded.
_DeploymentConfigLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeploymentConfigLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "DeploymentConfigLimitExceededException"
{-# INLINEABLE _DeploymentConfigLimitExceededException #-}
{-# DEPRECATED _DeploymentConfigLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The role ID was not specified.
_RoleRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RoleRequiredException
  = Core._MatchServiceError mkServiceConfig "RoleRequiredException"
{-# INLINEABLE _RoleRequiredException #-}
{-# DEPRECATED _RoleRequiredException "Use generic-lens or generic-optics instead"  #-}

-- | An invalid load balancer name, or no load balancer name, was specified.
_InvalidLoadBalancerInfoException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidLoadBalancerInfoException
  = Core._MatchServiceError mkServiceConfig
      "InvalidLoadBalancerInfoException"
{-# INLINEABLE _InvalidLoadBalancerInfoException #-}
{-# DEPRECATED _InvalidLoadBalancerInfoException "Use generic-lens or generic-optics instead"  #-}

-- | The configuration for the blue/green deployment group was provided in an invalid format. For information about deployment configuration format, see 'CreateDeploymentConfig' .
_InvalidBlueGreenDeploymentConfigurationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidBlueGreenDeploymentConfigurationException
  = Core._MatchServiceError mkServiceConfig
      "InvalidBlueGreenDeploymentConfigurationException"
{-# INLINEABLE _InvalidBlueGreenDeploymentConfigurationException #-}
{-# DEPRECATED _InvalidBlueGreenDeploymentConfigurationException "Use generic-lens or generic-optics instead"  #-}

-- | The service role ARN was specified in an invalid format. Or, if an Auto Scaling group was specified, the specified service role does not grant the appropriate permissions to Amazon EC2 Auto Scaling.
_InvalidRoleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRoleException
  = Core._MatchServiceError mkServiceConfig "InvalidRoleException"
{-# INLINEABLE _InvalidRoleException #-}
{-# DEPRECATED _InvalidRoleException "Use generic-lens or generic-optics instead"  #-}

-- | A deployment configuration with the specified name with the IAM user or AWS account already exists.
_DeploymentConfigAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeploymentConfigAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "DeploymentConfigAlreadyExistsException"
{-# INLINEABLE _DeploymentConfigAlreadyExistsException #-}
{-# DEPRECATED _DeploymentConfigAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | The target instance configuration is invalid. Possible causes include:
--
--
--     * Configuration data for target instances was entered for an in-place deployment.
--
--
--     * The limit of 10 tags for a tag type was exceeded.
--
--
--     * The combined length of the tag names exceeded the limit. 
--
--
--     * A specified tag is not currently applied to any instances.
--
--
_InvalidTargetInstancesException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTargetInstancesException
  = Core._MatchServiceError mkServiceConfig
      "InvalidTargetInstancesException"
{-# INLINEABLE _InvalidTargetInstancesException #-}
{-# DEPRECATED _InvalidTargetInstancesException "Use generic-lens or generic-optics instead"  #-}

-- | The specified tags are not valid. 
_InvalidTagsToAddException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTagsToAddException
  = Core._MatchServiceError mkServiceConfig
      "InvalidTagsToAddException"
{-# INLINEABLE _InvalidTagsToAddException #-}
{-# DEPRECATED _InvalidTagsToAddException "Use generic-lens or generic-optics instead"  #-}

-- | The number of allowed deployments was exceeded.
_DeploymentLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeploymentLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "DeploymentLimitExceededException"
{-# INLINEABLE _DeploymentLimitExceededException #-}
{-# DEPRECATED _DeploymentLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The specified IAM user ARN is already registered with an on-premises instance.
_IamUserArnAlreadyRegisteredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IamUserArnAlreadyRegisteredException
  = Core._MatchServiceError mkServiceConfig
      "IamUserArnAlreadyRegisteredException"
{-# INLINEABLE _IamUserArnAlreadyRegisteredException #-}
{-# DEPRECATED _IamUserArnAlreadyRegisteredException "Use generic-lens or generic-optics instead"  #-}

-- | The IAM session ARN was specified in an invalid format.
_InvalidIamSessionArnException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidIamSessionArnException
  = Core._MatchServiceError mkServiceConfig
      "InvalidIamSessionArnException"
{-# INLINEABLE _InvalidIamSessionArnException #-}
{-# DEPRECATED _InvalidIamSessionArnException "Use generic-lens or generic-optics instead"  #-}

-- | The maximum number of allowed on-premises instances in a single call was exceeded.
_InstanceLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InstanceLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "InstanceLimitExceededException"
{-# INLINEABLE _InstanceLimitExceededException #-}
{-# DEPRECATED _InstanceLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | A lifecycle event hook is invalid. Review the @hooks@ section in your AppSpec file to ensure the lifecycle events and @hooks@ functions are valid.
_InvalidLifecycleEventHookExecutionIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidLifecycleEventHookExecutionIdException
  = Core._MatchServiceError mkServiceConfig
      "InvalidLifecycleEventHookExecutionIdException"
{-# INLINEABLE _InvalidLifecycleEventHookExecutionIdException #-}
{-# DEPRECATED _InvalidLifecycleEventHookExecutionIdException "Use generic-lens or generic-optics instead"  #-}

-- | An invalid deployment style was specified. Valid deployment types include "IN_PLACE" and "BLUE_GREEN." Valid deployment options include "WITH_TRAFFIC_CONTROL" and "WITHOUT_TRAFFIC_CONTROL."
_InvalidDeploymentStyleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentStyleException
  = Core._MatchServiceError mkServiceConfig
      "InvalidDeploymentStyleException"
{-# INLINEABLE _InvalidDeploymentStyleException #-}
{-# DEPRECATED _InvalidDeploymentStyleException "Use generic-lens or generic-optics instead"  #-}

-- | The target filter name is invalid. 
_InvalidTargetFilterNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTargetFilterNameException
  = Core._MatchServiceError mkServiceConfig
      "InvalidTargetFilterNameException"
{-# INLINEABLE _InvalidTargetFilterNameException #-}
{-# DEPRECATED _InvalidTargetFilterNameException "Use generic-lens or generic-optics instead"  #-}

-- | The maximum number of targets that can be associated with an Amazon ECS or AWS Lambda deployment was exceeded. The target list of both types of deployments must have exactly one item. This exception does not apply to EC2/On-premises deployments. 
_DeploymentTargetListSizeExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeploymentTargetListSizeExceededException
  = Core._MatchServiceError mkServiceConfig
      "DeploymentTargetListSizeExceededException"
{-# INLINEABLE _DeploymentTargetListSizeExceededException #-}
{-# DEPRECATED _DeploymentTargetListSizeExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The deployed state filter was specified in an invalid format.
_InvalidDeployedStateFilterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDeployedStateFilterException
  = Core._MatchServiceError mkServiceConfig
      "InvalidDeployedStateFilterException"
{-# INLINEABLE _InvalidDeployedStateFilterException #-}
{-# DEPRECATED _InvalidDeployedStateFilterException "Use generic-lens or generic-optics instead"  #-}

-- | The Auto Scaling group was specified in an invalid format or does not exist.
_InvalidAutoScalingGroupException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAutoScalingGroupException
  = Core._MatchServiceError mkServiceConfig
      "InvalidAutoScalingGroupException"
{-# INLINEABLE _InvalidAutoScalingGroupException #-}
{-# DEPRECATED _InvalidAutoScalingGroupException "Use generic-lens or generic-optics instead"  #-}

-- | The application name was specified in an invalid format.
_InvalidApplicationNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidApplicationNameException
  = Core._MatchServiceError mkServiceConfig
      "InvalidApplicationNameException"
{-# INLINEABLE _InvalidApplicationNameException #-}
{-# DEPRECATED _InvalidApplicationNameException "Use generic-lens or generic-optics instead"  #-}

-- | No GitHub account connection exists with the named specified in the call.
_GitHubAccountTokenDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_GitHubAccountTokenDoesNotExistException
  = Core._MatchServiceError mkServiceConfig
      "GitHubAccountTokenDoesNotExistException"
{-# INLINEABLE _GitHubAccountTokenDoesNotExistException #-}
{-# DEPRECATED _GitHubAccountTokenDoesNotExistException "Use generic-lens or generic-optics instead"  #-}

-- | The application does not exist with the IAM user or AWS account.
_ApplicationDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ApplicationDoesNotExistException
  = Core._MatchServiceError mkServiceConfig
      "ApplicationDoesNotExistException"
{-# INLINEABLE _ApplicationDoesNotExistException #-}
{-# DEPRECATED _ApplicationDoesNotExistException "Use generic-lens or generic-optics instead"  #-}

-- | The minimum healthy instance value was specified in an invalid format.
_InvalidMinimumHealthyHostValueException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidMinimumHealthyHostValueException
  = Core._MatchServiceError mkServiceConfig
      "InvalidMinimumHealthyHostValueException"
{-# INLINEABLE _InvalidMinimumHealthyHostValueException #-}
{-# DEPRECATED _InvalidMinimumHealthyHostValueException "Use generic-lens or generic-optics instead"  #-}

-- | A call was submitted that is not supported for the specified deployment type.
_UnsupportedActionForDeploymentTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedActionForDeploymentTypeException
  = Core._MatchServiceError mkServiceConfig
      "UnsupportedActionForDeploymentTypeException"
{-# INLINEABLE _UnsupportedActionForDeploymentTypeException #-}
{-# DEPRECATED _UnsupportedActionForDeploymentTypeException "Use generic-lens or generic-optics instead"  #-}

-- | The specified resource could not be validated.
_ResourceValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceValidationException
  = Core._MatchServiceError mkServiceConfig
      "ResourceValidationException"
{-# INLINEABLE _ResourceValidationException #-}
{-# DEPRECATED _ResourceValidationException "Use generic-lens or generic-optics instead"  #-}

-- | The specified ARN is not supported. For example, it might be an ARN for a resource that is not expected. 
_ArnNotSupportedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ArnNotSupportedException
  = Core._MatchServiceError mkServiceConfig
      "ArnNotSupportedException"
{-# INLINEABLE _ArnNotSupportedException #-}
{-# DEPRECATED _ArnNotSupportedException "Use generic-lens or generic-optics instead"  #-}

-- | The GitHub token is not valid.
_InvalidGitHubAccountTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidGitHubAccountTokenException
  = Core._MatchServiceError mkServiceConfig
      "InvalidGitHubAccountTokenException"
{-# INLINEABLE _InvalidGitHubAccountTokenException #-}
{-# DEPRECATED _InvalidGitHubAccountTokenException "Use generic-lens or generic-optics instead"  #-}

-- | A call was submitted that specified both Ec2TagFilters and Ec2TagSet, but only one of these data types can be used in a single call.
_InvalidEC2TagCombinationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidEC2TagCombinationException
  = Core._MatchServiceError mkServiceConfig
      "InvalidEC2TagCombinationException"
{-# INLINEABLE _InvalidEC2TagCombinationException #-}
{-# DEPRECATED _InvalidEC2TagCombinationException "Use generic-lens or generic-optics instead"  #-}

-- | The result of a Lambda validation function that verifies a lifecycle event is invalid. It should return @Succeeded@ or @Failed@ .
_InvalidLifecycleEventHookExecutionStatusException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidLifecycleEventHookExecutionStatusException
  = Core._MatchServiceError mkServiceConfig
      "InvalidLifecycleEventHookExecutionStatusException"
{-# INLINEABLE _InvalidLifecycleEventHookExecutionStatusException #-}
{-# DEPRECATED _InvalidLifecycleEventHookExecutionStatusException "Use generic-lens or generic-optics instead"  #-}

-- | The maximum number of alarms for a deployment group (10) was exceeded.
_AlarmsLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AlarmsLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "AlarmsLimitExceededException"
{-# INLINEABLE _AlarmsLimitExceededException #-}
{-# DEPRECATED _AlarmsLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The API used does not support the deployment.
_OperationNotSupportedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationNotSupportedException
  = Core._MatchServiceError mkServiceConfig
      "OperationNotSupportedException"
{-# INLINEABLE _OperationNotSupportedException #-}
{-# DEPRECATED _OperationNotSupportedException "Use generic-lens or generic-optics instead"  #-}

-- | The provided target ID does not belong to the attempted deployment. 
_DeploymentTargetDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeploymentTargetDoesNotExistException
  = Core._MatchServiceError mkServiceConfig
      "DeploymentTargetDoesNotExistException"
{-# INLINEABLE _DeploymentTargetDoesNotExistException #-}
{-# DEPRECATED _DeploymentTargetDoesNotExistException "Use generic-lens or generic-optics instead"  #-}

-- | The tag filter was specified in an invalid format.
_InvalidTagFilterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTagFilterException
  = Core._MatchServiceError mkServiceConfig
      "InvalidTagFilterException"
{-# INLINEABLE _InvalidTagFilterException #-}
{-# DEPRECATED _InvalidTagFilterException "Use generic-lens or generic-optics instead"  #-}

-- | The trigger was specified in an invalid format.
_InvalidTriggerConfigException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTriggerConfigException
  = Core._MatchServiceError mkServiceConfig
      "InvalidTriggerConfigException"
{-# INLINEABLE _InvalidTriggerConfigException #-}
{-# DEPRECATED _InvalidTriggerConfigException "Use generic-lens or generic-optics instead"  #-}

-- | The wait type is invalid. 
_InvalidDeploymentWaitTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentWaitTypeException
  = Core._MatchServiceError mkServiceConfig
      "InvalidDeploymentWaitTypeException"
{-# INLINEABLE _InvalidDeploymentWaitTypeException #-}
{-# DEPRECATED _InvalidDeploymentWaitTypeException "Use generic-lens or generic-optics instead"  #-}

-- | The IgnoreApplicationStopFailures value is invalid. For AWS Lambda deployments, @false@ is expected. For EC2/On-premises deployments, @true@ or @false@ is expected.
_InvalidIgnoreApplicationStopFailuresValueException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidIgnoreApplicationStopFailuresValueException
  = Core._MatchServiceError mkServiceConfig
      "InvalidIgnoreApplicationStopFailuresValueException"
{-# INLINEABLE _InvalidIgnoreApplicationStopFailuresValueException #-}
{-# DEPRECATED _InvalidIgnoreApplicationStopFailuresValueException "Use generic-lens or generic-optics instead"  #-}

-- | The UpdateOutdatedInstancesOnly value is invalid. For AWS Lambda deployments, @false@ is expected. For EC2/On-premises deployments, @true@ or @false@ is expected.
_InvalidUpdateOutdatedInstancesOnlyValueException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidUpdateOutdatedInstancesOnlyValueException
  = Core._MatchServiceError mkServiceConfig
      "InvalidUpdateOutdatedInstancesOnlyValueException"
{-# INLINEABLE _InvalidUpdateOutdatedInstancesOnlyValueException #-}
{-# DEPRECATED _InvalidUpdateOutdatedInstancesOnlyValueException "Use generic-lens or generic-optics instead"  #-}

-- | A tag was not specified.
_TagRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagRequiredException
  = Core._MatchServiceError mkServiceConfig "TagRequiredException"
{-# INLINEABLE _TagRequiredException #-}
{-# DEPRECATED _TagRequiredException "Use generic-lens or generic-optics instead"  #-}

-- | The deployment group name was not specified.
_DeploymentGroupNameRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeploymentGroupNameRequiredException
  = Core._MatchServiceError mkServiceConfig
      "DeploymentGroupNameRequiredException"
{-# INLINEABLE _DeploymentGroupNameRequiredException #-}
{-# DEPRECATED _DeploymentGroupNameRequiredException "Use generic-lens or generic-optics instead"  #-}

-- | A bucket name is required, but was not provided.
_BucketNameFilterRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BucketNameFilterRequiredException
  = Core._MatchServiceError mkServiceConfig
      "BucketNameFilterRequiredException"
{-# INLINEABLE _BucketNameFilterRequiredException #-}
{-# DEPRECATED _BucketNameFilterRequiredException "Use generic-lens or generic-optics instead"  #-}

-- | The deployment configuration does not exist with the IAM user or AWS account.
_DeploymentConfigDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeploymentConfigDoesNotExistException
  = Core._MatchServiceError mkServiceConfig
      "DeploymentConfigDoesNotExistException"
{-# INLINEABLE _DeploymentConfigDoesNotExistException #-}
{-# DEPRECATED _DeploymentConfigDoesNotExistException "Use generic-lens or generic-optics instead"  #-}

-- | The bucket name either doesn't exist or was specified in an invalid format.
_InvalidBucketNameFilterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidBucketNameFilterException
  = Core._MatchServiceError mkServiceConfig
      "InvalidBucketNameFilterException"
{-# INLINEABLE _InvalidBucketNameFilterException #-}
{-# DEPRECATED _InvalidBucketNameFilterException "Use generic-lens or generic-optics instead"  #-}

-- | A deployment group with the specified name with the IAM user or AWS account already exists.
_DeploymentGroupAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeploymentGroupAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "DeploymentGroupAlreadyExistsException"
{-# INLINEABLE _DeploymentGroupAlreadyExistsException #-}
{-# DEPRECATED _DeploymentGroupAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | The column name to sort by is either not present or was specified in an invalid format.
_InvalidSortByException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSortByException
  = Core._MatchServiceError mkServiceConfig "InvalidSortByException"
{-# INLINEABLE _InvalidSortByException #-}
{-# DEPRECATED _InvalidSortByException "Use generic-lens or generic-optics instead"  #-}

-- | The named revision does not exist with the IAM user or AWS account.
_RevisionDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RevisionDoesNotExistException
  = Core._MatchServiceError mkServiceConfig
      "RevisionDoesNotExistException"
{-# INLINEABLE _RevisionDoesNotExistException #-}
{-# DEPRECATED _RevisionDoesNotExistException "Use generic-lens or generic-optics instead"  #-}

-- | A target is not valid. 
_InvalidTargetException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTargetException
  = Core._MatchServiceError mkServiceConfig "InvalidTargetException"
{-# INLINEABLE _InvalidTargetException #-}
{-# DEPRECATED _InvalidTargetException "Use generic-lens or generic-optics instead"  #-}

-- | The deployment groups limit was exceeded.
_DeploymentGroupLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeploymentGroupLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "DeploymentGroupLimitExceededException"
{-# INLINEABLE _DeploymentGroupLimitExceededException #-}
{-# DEPRECATED _DeploymentGroupLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The named deployment group with the IAM user or AWS account does not exist.
_DeploymentGroupDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeploymentGroupDoesNotExistException
  = Core._MatchServiceError mkServiceConfig
      "DeploymentGroupDoesNotExistException"
{-# INLINEABLE _DeploymentGroupDoesNotExistException #-}
{-# DEPRECATED _DeploymentGroupDoesNotExistException "Use generic-lens or generic-optics instead"  #-}

-- | An API function was called too frequently.
_ThrottlingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ThrottlingException
  = Core._MatchServiceError mkServiceConfig "ThrottlingException"
{-# INLINEABLE _ThrottlingException #-}
{-# DEPRECATED _ThrottlingException "Use generic-lens or generic-optics instead"  #-}

-- | The deployment configuration name was specified in an invalid format.
_InvalidDeploymentConfigNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentConfigNameException
  = Core._MatchServiceError mkServiceConfig
      "InvalidDeploymentConfigNameException"
{-# INLINEABLE _InvalidDeploymentConfigNameException #-}
{-# DEPRECATED _InvalidDeploymentConfigNameException "Use generic-lens or generic-optics instead"  #-}

-- | The deployment configuration name was not specified.
_DeploymentConfigNameRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeploymentConfigNameRequiredException
  = Core._MatchServiceError mkServiceConfig
      "DeploymentConfigNameRequiredException"
{-# INLINEABLE _DeploymentConfigNameRequiredException #-}
{-# DEPRECATED _DeploymentConfigNameRequiredException "Use generic-lens or generic-optics instead"  #-}

-- | At least one deployment ID must be specified.
_DeploymentIdRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeploymentIdRequiredException
  = Core._MatchServiceError mkServiceConfig
      "DeploymentIdRequiredException"
{-# INLINEABLE _DeploymentIdRequiredException #-}
{-# DEPRECATED _DeploymentIdRequiredException "Use generic-lens or generic-optics instead"  #-}

-- | 
_InvalidInstanceIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInstanceIdException
  = Core._MatchServiceError mkServiceConfig
      "InvalidInstanceIdException"
{-# INLINEABLE _InvalidInstanceIdException #-}
{-# DEPRECATED _InvalidInstanceIdException "Use generic-lens or generic-optics instead"  #-}

-- | The deployment does not have a status of Ready and can't continue yet.
_DeploymentIsNotInReadyStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeploymentIsNotInReadyStateException
  = Core._MatchServiceError mkServiceConfig
      "DeploymentIsNotInReadyStateException"
{-# INLINEABLE _DeploymentIsNotInReadyStateException #-}
{-# DEPRECATED _DeploymentIsNotInReadyStateException "Use generic-lens or generic-optics instead"  #-}

-- | A target group pair associated with this deployment is not valid. 
_InvalidTargetGroupPairException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTargetGroupPairException
  = Core._MatchServiceError mkServiceConfig
      "InvalidTargetGroupPairException"
{-# INLINEABLE _InvalidTargetGroupPairException #-}
{-# DEPRECATED _InvalidTargetGroupPairException "Use generic-lens or generic-optics instead"  #-}

-- | The next token was specified in an invalid format.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException
  = Core._MatchServiceError mkServiceConfig
      "InvalidNextTokenException"
{-# INLINEABLE _InvalidNextTokenException #-}
{-# DEPRECATED _InvalidNextTokenException "Use generic-lens or generic-optics instead"  #-}

-- | The instance ID was not specified.
_InstanceIdRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InstanceIdRequiredException
  = Core._MatchServiceError mkServiceConfig
      "InstanceIdRequiredException"
{-# INLINEABLE _InstanceIdRequiredException #-}
{-# DEPRECATED _InstanceIdRequiredException "Use generic-lens or generic-optics instead"  #-}

-- | At least one of the deployment IDs was specified in an invalid format.
_InvalidDeploymentIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentIdException
  = Core._MatchServiceError mkServiceConfig
      "InvalidDeploymentIdException"
{-# INLINEABLE _InvalidDeploymentIdException #-}
{-# DEPRECATED _InvalidDeploymentIdException "Use generic-lens or generic-optics instead"  #-}

-- | The sort order was specified in an invalid format.
_InvalidSortOrderException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSortOrderException
  = Core._MatchServiceError mkServiceConfig
      "InvalidSortOrderException"
{-# INLINEABLE _InvalidSortOrderException #-}
{-# DEPRECATED _InvalidSortOrderException "Use generic-lens or generic-optics instead"  #-}

-- | The automatic rollback configuration was specified in an invalid format. For example, automatic rollback is enabled, but an invalid triggering event type or no event types were listed.
_InvalidAutoRollbackConfigException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAutoRollbackConfigException
  = Core._MatchServiceError mkServiceConfig
      "InvalidAutoRollbackConfigException"
{-# INLINEABLE _InvalidAutoRollbackConfigException #-}
{-# DEPRECATED _InvalidAutoRollbackConfigException "Use generic-lens or generic-optics instead"  #-}

-- | The deployment is already complete.
_DeploymentAlreadyCompletedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeploymentAlreadyCompletedException
  = Core._MatchServiceError mkServiceConfig
      "DeploymentAlreadyCompletedException"
{-# INLINEABLE _DeploymentAlreadyCompletedException #-}
{-# DEPRECATED _DeploymentAlreadyCompletedException "Use generic-lens or generic-optics instead"  #-}

-- | The Amazon ECS service is associated with more than one deployment groups. An Amazon ECS service can be associated with only one deployment group. 
_ECSServiceMappingLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ECSServiceMappingLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "ECSServiceMappingLimitExceededException"
{-# INLINEABLE _ECSServiceMappingLimitExceededException #-}
{-# DEPRECATED _ECSServiceMappingLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The deployment with the IAM user or AWS account does not exist.
_DeploymentDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeploymentDoesNotExistException
  = Core._MatchServiceError mkServiceConfig
      "DeploymentDoesNotExistException"
{-# INLINEABLE _DeploymentDoesNotExistException #-}
{-# DEPRECATED _DeploymentDoesNotExistException "Use generic-lens or generic-optics instead"  #-}

-- | The maximum number of names or IDs allowed for this request (100) was exceeded.
_BatchLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BatchLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "BatchLimitExceededException"
{-# INLINEABLE _BatchLimitExceededException #-}
{-# DEPRECATED _BatchLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The revision was specified in an invalid format.
_InvalidRevisionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRevisionException
  = Core._MatchServiceError mkServiceConfig
      "InvalidRevisionException"
{-# INLINEABLE _InvalidRevisionException #-}
{-# DEPRECATED _InvalidRevisionException "Use generic-lens or generic-optics instead"  #-}

-- | The revision ID was not specified.
_RevisionRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RevisionRequiredException
  = Core._MatchServiceError mkServiceConfig
      "RevisionRequiredException"
{-# INLINEABLE _RevisionRequiredException #-}
{-# DEPRECATED _RevisionRequiredException "Use generic-lens or generic-optics instead"  #-}

-- | The specified instance does not exist in the deployment group.
_InstanceDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InstanceDoesNotExistException
  = Core._MatchServiceError mkServiceConfig
      "InstanceDoesNotExistException"
{-# INLINEABLE _InstanceDoesNotExistException #-}
{-# DEPRECATED _InstanceDoesNotExistException "Use generic-lens or generic-optics instead"  #-}

-- | The deployment configuration is still in use.
_DeploymentConfigInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeploymentConfigInUseException
  = Core._MatchServiceError mkServiceConfig
      "DeploymentConfigInUseException"
{-# INLINEABLE _DeploymentConfigInUseException #-}
{-# DEPRECATED _DeploymentConfigInUseException "Use generic-lens or generic-optics instead"  #-}

-- | The ID of the deployment configuration is invalid. 
_InvalidDeploymentConfigIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentConfigIdException
  = Core._MatchServiceError mkServiceConfig
      "InvalidDeploymentConfigIdException"
{-# INLINEABLE _InvalidDeploymentConfigIdException #-}
{-# DEPRECATED _InvalidDeploymentConfigIdException "Use generic-lens or generic-optics instead"  #-}

-- | The input was specified in an invalid format.
_InvalidInputException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInputException
  = Core._MatchServiceError mkServiceConfig "InvalidInputException"
{-# INLINEABLE _InvalidInputException #-}
{-# DEPRECATED _InvalidInputException "Use generic-lens or generic-optics instead"  #-}

-- | The tag was specified in an invalid format.
_InvalidEC2TagException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidEC2TagException
  = Core._MatchServiceError mkServiceConfig "InvalidEC2TagException"
{-# INLINEABLE _InvalidEC2TagException #-}
{-# DEPRECATED _InvalidEC2TagException "Use generic-lens or generic-optics instead"  #-}

-- | The on-premises instance name was specified in an invalid format.
_InvalidInstanceNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInstanceNameException
  = Core._MatchServiceError mkServiceConfig
      "InvalidInstanceNameException"
{-# INLINEABLE _InvalidInstanceNameException #-}
{-# DEPRECATED _InvalidInstanceNameException "Use generic-lens or generic-optics instead"  #-}

-- | An on-premises instance name was not specified.
_InstanceNameRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InstanceNameRequiredException
  = Core._MatchServiceError mkServiceConfig
      "InstanceNameRequiredException"
{-# INLINEABLE _InstanceNameRequiredException #-}
{-# DEPRECATED _InstanceNameRequiredException "Use generic-lens or generic-optics instead"  #-}

-- | Both an IAM user ARN and an IAM session ARN were included in the request. Use only one ARN type.
_MultipleIamArnsProvidedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MultipleIamArnsProvidedException
  = Core._MatchServiceError mkServiceConfig
      "MultipleIamArnsProvidedException"
{-# INLINEABLE _MultipleIamArnsProvidedException #-}
{-# DEPRECATED _MultipleIamArnsProvidedException "Use generic-lens or generic-optics instead"  #-}

-- | The maximum allowed number of triggers was exceeded.
_TriggerTargetsLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TriggerTargetsLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "TriggerTargetsLimitExceededException"
{-# INLINEABLE _TriggerTargetsLimitExceededException #-}
{-# DEPRECATED _TriggerTargetsLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The specified deployment status doesn't exist or cannot be determined.
_InvalidDeploymentStatusException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentStatusException
  = Core._MatchServiceError mkServiceConfig
      "InvalidDeploymentStatusException"
{-# INLINEABLE _InvalidDeploymentStatusException #-}
{-# DEPRECATED _InvalidDeploymentStatusException "Use generic-lens or generic-optics instead"  #-}

-- | The registration status was specified in an invalid format.
_InvalidRegistrationStatusException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRegistrationStatusException
  = Core._MatchServiceError mkServiceConfig
      "InvalidRegistrationStatusException"
{-# INLINEABLE _InvalidRegistrationStatusException #-}
{-# DEPRECATED _InvalidRegistrationStatusException "Use generic-lens or generic-optics instead"  #-}

-- | The minimum number of required application names was not specified.
_ApplicationNameRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ApplicationNameRequiredException
  = Core._MatchServiceError mkServiceConfig
      "ApplicationNameRequiredException"
{-# INLINEABLE _ApplicationNameRequiredException #-}
{-# DEPRECATED _ApplicationNameRequiredException "Use generic-lens or generic-optics instead"  #-}

-- | The specified on-premises instance is not registered.
_InstanceNotRegisteredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InstanceNotRegisteredException
  = Core._MatchServiceError mkServiceConfig
      "InstanceNotRegisteredException"
{-# INLINEABLE _InstanceNotRegisteredException #-}
{-# DEPRECATED _InstanceNotRegisteredException "Use generic-lens or generic-optics instead"  #-}

-- | An application with the specified name with the IAM user or AWS account already exists.
_ApplicationAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ApplicationAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "ApplicationAlreadyExistsException"
{-# INLINEABLE _ApplicationAlreadyExistsException #-}
{-# DEPRECATED _ApplicationAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | The specified instance status does not exist.
_InvalidInstanceStatusException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInstanceStatusException
  = Core._MatchServiceError mkServiceConfig
      "InvalidInstanceStatusException"
{-# INLINEABLE _InvalidInstanceStatusException #-}
{-# DEPRECATED _InvalidInstanceStatusException "Use generic-lens or generic-optics instead"  #-}

-- | The target ID provided was not valid. 
_InvalidDeploymentTargetIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentTargetIdException
  = Core._MatchServiceError mkServiceConfig
      "InvalidDeploymentTargetIdException"
{-# INLINEABLE _InvalidDeploymentTargetIdException #-}
{-# DEPRECATED _InvalidDeploymentTargetIdException "Use generic-lens or generic-optics instead"  #-}

-- | The maximum allowed number of tags was exceeded.
_TagLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "TagLimitExceededException"
{-# INLINEABLE _TagLimitExceededException #-}
{-# DEPRECATED _TagLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | More applications were attempted to be created than are allowed.
_ApplicationLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ApplicationLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "ApplicationLimitExceededException"
{-# INLINEABLE _ApplicationLimitExceededException #-}
{-# DEPRECATED _ApplicationLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The number of tag groups included in the tag set list exceeded the maximum allowed limit of 3.
_TagSetListLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagSetListLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "TagSetListLimitExceededException"
{-# INLINEABLE _TagSetListLimitExceededException #-}
{-# DEPRECATED _TagSetListLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The specified ARN is not in a valid format. 
_InvalidArnException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArnException
  = Core._MatchServiceError mkServiceConfig "InvalidArnException"
{-# INLINEABLE _InvalidArnException #-}
{-# DEPRECATED _InvalidArnException "Use generic-lens or generic-optics instead"  #-}

-- | An invalid operation was detected.
_InvalidOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOperationException
  = Core._MatchServiceError mkServiceConfig
      "InvalidOperationException"
{-# INLINEABLE _InvalidOperationException #-}
{-# DEPRECATED _InvalidOperationException "Use generic-lens or generic-optics instead"  #-}

-- | A deployment target ID was not provided. 
_DeploymentTargetIdRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeploymentTargetIdRequiredException
  = Core._MatchServiceError mkServiceConfig
      "DeploymentTargetIdRequiredException"
{-# INLINEABLE _DeploymentTargetIdRequiredException #-}
{-# DEPRECATED _DeploymentTargetIdRequiredException "Use generic-lens or generic-optics instead"  #-}

-- | The call is missing a required GitHub account connection name.
_GitHubAccountTokenNameRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_GitHubAccountTokenNameRequiredException
  = Core._MatchServiceError mkServiceConfig
      "GitHubAccountTokenNameRequiredException"
{-# INLINEABLE _GitHubAccountTokenNameRequiredException #-}
{-# DEPRECATED _GitHubAccountTokenNameRequiredException "Use generic-lens or generic-optics instead"  #-}

-- | The Amazon ECS service identifier is not valid. 
_InvalidECSServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidECSServiceException
  = Core._MatchServiceError mkServiceConfig
      "InvalidECSServiceException"
{-# INLINEABLE _InvalidECSServiceException #-}
{-# DEPRECATED _InvalidECSServiceException "Use generic-lens or generic-optics instead"  #-}

-- | An instance type was specified for an in-place deployment. Instance types are supported for blue/green deployments only.
_InvalidDeploymentInstanceTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDeploymentInstanceTypeException
  = Core._MatchServiceError mkServiceConfig
      "InvalidDeploymentInstanceTypeException"
{-# INLINEABLE _InvalidDeploymentInstanceTypeException #-}
{-# DEPRECATED _InvalidDeploymentInstanceTypeException "Use generic-lens or generic-optics instead"  #-}

-- | The external ID was specified in an invalid format.
_InvalidExternalIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidExternalIdException
  = Core._MatchServiceError mkServiceConfig
      "InvalidExternalIdException"
{-# INLINEABLE _InvalidExternalIdException #-}
{-# DEPRECATED _InvalidExternalIdException "Use generic-lens or generic-optics instead"  #-}

-- | No IAM ARN was included in the request. You must use an IAM session ARN or IAM user ARN in the request.
_IamArnRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IamArnRequiredException
  = Core._MatchServiceError mkServiceConfig "IamArnRequiredException"
{-# INLINEABLE _IamArnRequiredException #-}
{-# DEPRECATED _IamArnRequiredException "Use generic-lens or generic-optics instead"  #-}

-- | The format of the specified GitHub account connection name is invalid.
_InvalidGitHubAccountTokenNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidGitHubAccountTokenNameException
  = Core._MatchServiceError mkServiceConfig
      "InvalidGitHubAccountTokenNameException"
{-# INLINEABLE _InvalidGitHubAccountTokenNameException #-}
{-# DEPRECATED _InvalidGitHubAccountTokenNameException "Use generic-lens or generic-optics instead"  #-}

-- | An attempt to return the status of an already completed lifecycle event occurred.
_LifecycleEventAlreadyCompletedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LifecycleEventAlreadyCompletedException
  = Core._MatchServiceError mkServiceConfig
      "LifecycleEventAlreadyCompletedException"
{-# INLINEABLE _LifecycleEventAlreadyCompletedException #-}
{-# DEPRECATED _LifecycleEventAlreadyCompletedException "Use generic-lens or generic-optics instead"  #-}

-- | The specified key prefix filter was specified in an invalid format.
_InvalidKeyPrefixFilterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidKeyPrefixFilterException
  = Core._MatchServiceError mkServiceConfig
      "InvalidKeyPrefixFilterException"
{-# INLINEABLE _InvalidKeyPrefixFilterException #-}
{-# DEPRECATED _InvalidKeyPrefixFilterException "Use generic-lens or generic-optics instead"  #-}

-- | The ARN of a resource is required, but was not found. 
_ResourceArnRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceArnRequiredException
  = Core._MatchServiceError mkServiceConfig
      "ResourceArnRequiredException"
{-# INLINEABLE _ResourceArnRequiredException #-}
{-# DEPRECATED _ResourceArnRequiredException "Use generic-lens or generic-optics instead"  #-}

-- | A deployment to a target was attempted while another deployment was in progress. 
_DeploymentAlreadyStartedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DeploymentAlreadyStartedException
  = Core._MatchServiceError mkServiceConfig
      "DeploymentAlreadyStartedException"
{-# INLINEABLE _DeploymentAlreadyStartedException #-}
{-# DEPRECATED _DeploymentAlreadyStartedException "Use generic-lens or generic-optics instead"  #-}

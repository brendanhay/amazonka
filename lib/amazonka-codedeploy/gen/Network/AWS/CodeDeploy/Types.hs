{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types
    (
    -- * Service Configuration
      codeDeploy

    -- * Errors
    , _LifecycleHookLimitExceededException
    , _InvalidTimeRangeException
    , _InvalidComputePlatformException
    , _InvalidTagException
    , _InvalidFileExistsBehaviorException
    , _InvalidAlarmConfigException
    , _InstanceNameAlreadyRegisteredException
    , _IAMUserARNRequiredException
    , _InvalidDeploymentGroupNameException
    , _InvalidInstanceTypeException
    , _IAMSessionARNAlreadyRegisteredException
    , _InvalidTrafficRoutingConfigurationException
    , _DescriptionTooLongException
    , _InvalidIAMUserARNException
    , _InvalidOnPremisesTagCombinationException
    , _DeploymentNotStartedException
    , _DeploymentConfigLimitExceededException
    , _RoleRequiredException
    , _InvalidLoadBalancerInfoException
    , _InvalidBlueGreenDeploymentConfigurationException
    , _InvalidRoleException
    , _DeploymentConfigAlreadyExistsException
    , _InvalidTargetInstancesException
    , _DeploymentLimitExceededException
    , _IAMUserARNAlreadyRegisteredException
    , _InvalidIAMSessionARNException
    , _InstanceLimitExceededException
    , _InvalidLifecycleEventHookExecutionIdException
    , _InvalidDeploymentStyleException
    , _InvalidDeployedStateFilterException
    , _InvalidAutoScalingGroupException
    , _InvalidApplicationNameException
    , _GitHubAccountTokenDoesNotExistException
    , _ApplicationDoesNotExistException
    , _InvalidMinimumHealthyHostValueException
    , _UnsupportedActionForDeploymentTypeException
    , _ResourceValidationException
    , _InvalidGitHubAccountTokenException
    , _InvalidEC2TagCombinationException
    , _InvalidLifecycleEventHookExecutionStatusException
    , _AlarmsLimitExceededException
    , _OperationNotSupportedException
    , _InvalidTagFilterException
    , _InvalidTriggerConfigException
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
    , _DeploymentGroupLimitExceededException
    , _DeploymentGroupDoesNotExistException
    , _ThrottlingException
    , _InvalidDeploymentConfigNameException
    , _DeploymentConfigNameRequiredException
    , _DeploymentIdRequiredException
    , _InvalidInstanceIdException
    , _DeploymentIsNotInReadyStateException
    , _InvalidNextTokenException
    , _InstanceIdRequiredException
    , _InvalidDeploymentIdException
    , _InvalidSortOrderException
    , _InvalidAutoRollbackConfigException
    , _DeploymentAlreadyCompletedException
    , _DeploymentDoesNotExistException
    , _BatchLimitExceededException
    , _InvalidRevisionException
    , _RevisionRequiredException
    , _InstanceDoesNotExistException
    , _DeploymentConfigInUseException
    , _InvalidInputException
    , _InvalidEC2TagException
    , _InvalidInstanceNameException
    , _InstanceNameRequiredException
    , _MultipleIAMARNsProvidedException
    , _TriggerTargetsLimitExceededException
    , _InvalidDeploymentStatusException
    , _InvalidRegistrationStatusException
    , _ApplicationNameRequiredException
    , _InstanceNotRegisteredException
    , _ApplicationAlreadyExistsException
    , _InvalidInstanceStatusException
    , _TagLimitExceededException
    , _ApplicationLimitExceededException
    , _TagSetListLimitExceededException
    , _InvalidOperationException
    , _GitHubAccountTokenNameRequiredException
    , _InvalidDeploymentInstanceTypeException
    , _IAMARNRequiredException
    , _InvalidGitHubAccountTokenNameException
    , _LifecycleEventAlreadyCompletedException
    , _InvalidKeyPrefixFilterException

    -- * ApplicationRevisionSortBy
    , ApplicationRevisionSortBy (..)

    -- * AutoRollbackEvent
    , AutoRollbackEvent (..)

    -- * BundleType
    , BundleType (..)

    -- * ComputePlatform
    , ComputePlatform (..)

    -- * DeployErrorCode
    , DeployErrorCode (..)

    -- * DeploymentCreator
    , DeploymentCreator (..)

    -- * DeploymentOption
    , DeploymentOption (..)

    -- * DeploymentReadyAction
    , DeploymentReadyAction (..)

    -- * DeploymentStatus
    , DeploymentStatus (..)

    -- * DeploymentType
    , DeploymentType (..)

    -- * EC2TagFilterType
    , EC2TagFilterType (..)

    -- * FileExistsBehavior
    , FileExistsBehavior (..)

    -- * GreenFleetProvisioningAction
    , GreenFleetProvisioningAction (..)

    -- * InstanceAction
    , InstanceAction (..)

    -- * InstanceStatus
    , InstanceStatus (..)

    -- * InstanceType
    , InstanceType (..)

    -- * LifecycleErrorCode
    , LifecycleErrorCode (..)

    -- * LifecycleEventStatus
    , LifecycleEventStatus (..)

    -- * ListStateFilterAction
    , ListStateFilterAction (..)

    -- * MinimumHealthyHostsType
    , MinimumHealthyHostsType (..)

    -- * RegistrationStatus
    , RegistrationStatus (..)

    -- * RevisionLocationType
    , RevisionLocationType (..)

    -- * SortOrder
    , SortOrder (..)

    -- * StopStatus
    , StopStatus (..)

    -- * TagFilterType
    , TagFilterType (..)

    -- * TrafficRoutingType
    , TrafficRoutingType (..)

    -- * TriggerEventType
    , TriggerEventType (..)

    -- * Alarm
    , Alarm
    , alarm
    , aName

    -- * AlarmConfiguration
    , AlarmConfiguration
    , alarmConfiguration
    , acIgnorePollAlarmFailure
    , acEnabled
    , acAlarms

    -- * ApplicationInfo
    , ApplicationInfo
    , applicationInfo
    , aiLinkedToGitHub
    , aiComputePlatform
    , aiApplicationId
    , aiApplicationName
    , aiGitHubAccountName
    , aiCreateTime

    -- * AutoRollbackConfiguration
    , AutoRollbackConfiguration
    , autoRollbackConfiguration
    , arcEnabled
    , arcEvents

    -- * AutoScalingGroup
    , AutoScalingGroup
    , autoScalingGroup
    , asgHook
    , asgName

    -- * BlueGreenDeploymentConfiguration
    , BlueGreenDeploymentConfiguration
    , blueGreenDeploymentConfiguration
    , bgdcDeploymentReadyOption
    , bgdcGreenFleetProvisioningOption
    , bgdcTerminateBlueInstancesOnDeploymentSuccess

    -- * BlueInstanceTerminationOption
    , BlueInstanceTerminationOption
    , blueInstanceTerminationOption
    , bitoAction
    , bitoTerminationWaitTimeInMinutes

    -- * DeploymentConfigInfo
    , DeploymentConfigInfo
    , deploymentConfigInfo
    , dciDeploymentConfigName
    , dciComputePlatform
    , dciMinimumHealthyHosts
    , dciTrafficRoutingConfig
    , dciDeploymentConfigId
    , dciCreateTime

    -- * DeploymentGroupInfo
    , DeploymentGroupInfo
    , deploymentGroupInfo
    , dgiServiceRoleARN
    , dgiEc2TagSet
    , dgiDeploymentConfigName
    , dgiLastAttemptedDeployment
    , dgiOnPremisesTagSet
    , dgiComputePlatform
    , dgiTargetRevision
    , dgiEc2TagFilters
    , dgiBlueGreenDeploymentConfiguration
    , dgiLoadBalancerInfo
    , dgiOnPremisesInstanceTagFilters
    , dgiLastSuccessfulDeployment
    , dgiApplicationName
    , dgiAlarmConfiguration
    , dgiTriggerConfigurations
    , dgiDeploymentGroupId
    , dgiAutoScalingGroups
    , dgiDeploymentStyle
    , dgiAutoRollbackConfiguration
    , dgiDeploymentGroupName

    -- * DeploymentInfo
    , DeploymentInfo
    , deploymentInfo
    , diCreator
    , diStatus
    , diDeploymentId
    , diDeploymentConfigName
    , diComputePlatform
    , diPreviousRevision
    , diInstanceTerminationWaitTimeStarted
    , diDeploymentStatusMessages
    , diStartTime
    , diCompleteTime
    , diBlueGreenDeploymentConfiguration
    , diErrorInformation
    , diLoadBalancerInfo
    , diAdditionalDeploymentStatusInfo
    , diDeploymentOverview
    , diFileExistsBehavior
    , diApplicationName
    , diRollbackInfo
    , diTargetInstances
    , diRevision
    , diDescription
    , diDeploymentStyle
    , diCreateTime
    , diAutoRollbackConfiguration
    , diUpdateOutdatedInstancesOnly
    , diDeploymentGroupName
    , diIgnoreApplicationStopFailures

    -- * DeploymentOverview
    , DeploymentOverview
    , deploymentOverview
    , doPending
    , doSkipped
    , doInProgress
    , doSucceeded
    , doReady
    , doFailed

    -- * DeploymentReadyOption
    , DeploymentReadyOption
    , deploymentReadyOption
    , droActionOnTimeout
    , droWaitTimeInMinutes

    -- * DeploymentStyle
    , DeploymentStyle
    , deploymentStyle
    , dsDeploymentOption
    , dsDeploymentType

    -- * Diagnostics
    , Diagnostics
    , diagnostics
    , dLogTail
    , dErrorCode
    , dScriptName
    , dMessage

    -- * EC2TagFilter
    , EC2TagFilter
    , ec2TagFilter
    , etfValue
    , etfKey
    , etfType

    -- * EC2TagSet
    , EC2TagSet
    , ec2TagSet
    , etsEc2TagSetList

    -- * ELBInfo
    , ELBInfo
    , eLBInfo
    , elbiName

    -- * ErrorInformation
    , ErrorInformation
    , errorInformation
    , eiCode
    , eiMessage

    -- * GenericRevisionInfo
    , GenericRevisionInfo
    , genericRevisionInfo
    , griRegisterTime
    , griFirstUsedTime
    , griDeploymentGroups
    , griLastUsedTime
    , griDescription

    -- * GitHubLocation
    , GitHubLocation
    , gitHubLocation
    , ghlCommitId
    , ghlRepository

    -- * GreenFleetProvisioningOption
    , GreenFleetProvisioningOption
    , greenFleetProvisioningOption
    , gfpoAction

    -- * InstanceInfo
    , InstanceInfo
    , instanceInfo
    , iiRegisterTime
    , iiInstanceARN
    , iiDeregisterTime
    , iiIamUserARN
    , iiInstanceName
    , iiIamSessionARN
    , iiTags

    -- * InstanceSummary
    , InstanceSummary
    , instanceSummary
    , isInstanceId
    , isStatus
    , isDeploymentId
    , isLastUpdatedAt
    , isLifecycleEvents
    , isInstanceType

    -- * LastDeploymentInfo
    , LastDeploymentInfo
    , lastDeploymentInfo
    , ldiStatus
    , ldiDeploymentId
    , ldiEndTime
    , ldiCreateTime

    -- * LifecycleEvent
    , LifecycleEvent
    , lifecycleEvent
    , leStatus
    , leLifecycleEventName
    , leStartTime
    , leDiagnostics
    , leEndTime

    -- * LoadBalancerInfo
    , LoadBalancerInfo
    , loadBalancerInfo
    , lbiElbInfoList
    , lbiTargetGroupInfoList

    -- * MinimumHealthyHosts
    , MinimumHealthyHosts
    , minimumHealthyHosts
    , mhhValue
    , mhhType

    -- * OnPremisesTagSet
    , OnPremisesTagSet
    , onPremisesTagSet
    , optsOnPremisesTagSetList

    -- * RawString
    , RawString
    , rawString
    , rsContent
    , rsSha256

    -- * RevisionInfo
    , RevisionInfo
    , revisionInfo
    , riGenericRevisionInfo
    , riRevisionLocation

    -- * RevisionLocation
    , RevisionLocation
    , revisionLocation
    , rlString
    , rlRevisionType
    , rlS3Location
    , rlGitHubLocation

    -- * RollbackInfo
    , RollbackInfo
    , rollbackInfo
    , riRollbackTriggeringDeploymentId
    , riRollbackMessage
    , riRollbackDeploymentId

    -- * S3Location
    , S3Location
    , s3Location
    , slBundleType
    , slETag
    , slBucket
    , slKey
    , slVersion

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * TagFilter
    , TagFilter
    , tagFilter
    , tfValue
    , tfKey
    , tfType

    -- * TargetGroupInfo
    , TargetGroupInfo
    , targetGroupInfo
    , tgiName

    -- * TargetInstances
    , TargetInstances
    , targetInstances
    , tiEc2TagSet
    , tiTagFilters
    , tiAutoScalingGroups

    -- * TimeBasedCanary
    , TimeBasedCanary
    , timeBasedCanary
    , tbcCanaryInterval
    , tbcCanaryPercentage

    -- * TimeBasedLinear
    , TimeBasedLinear
    , timeBasedLinear
    , tblLinearInterval
    , tblLinearPercentage

    -- * TimeRange
    , TimeRange
    , timeRange
    , trStart
    , trEnd

    -- * TrafficRoutingConfig
    , TrafficRoutingConfig
    , trafficRoutingConfig
    , trcTimeBasedCanary
    , trcTimeBasedLinear
    , trcType

    -- * TriggerConfig
    , TriggerConfig
    , triggerConfig
    , tcTriggerName
    , tcTriggerEvents
    , tcTriggerTargetARN
    ) where

import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.CodeDeploy.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2014-10-06@ of the Amazon CodeDeploy SDK configuration.
codeDeploy :: Service
codeDeploy =
  Service
    { _svcAbbrev = "CodeDeploy"
    , _svcSigner = v4
    , _svcPrefix = "codedeploy"
    , _svcVersion = "2014-10-06"
    , _svcEndpoint = defaultEndpoint codeDeploy
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "CodeDeploy"
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


-- | The limit for lifecycle hooks was exceeded.
--
--
_LifecycleHookLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LifecycleHookLimitExceededException =
  _MatchServiceError codeDeploy "LifecycleHookLimitExceededException"


-- | The specified time range was specified in an invalid format.
--
--
_InvalidTimeRangeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTimeRangeException =
  _MatchServiceError codeDeploy "InvalidTimeRangeException"


-- | The computePlatform is invalid. The computePlatform should be @Lambda@ or @Server@ .
--
--
_InvalidComputePlatformException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidComputePlatformException =
  _MatchServiceError codeDeploy "InvalidComputePlatformException"


-- | The specified tag was specified in an invalid format.
--
--
_InvalidTagException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTagException = _MatchServiceError codeDeploy "InvalidTagException"


-- | An invalid fileExistsBehavior option was specified to determine how AWS CodeDeploy handles files or directories that already exist in a deployment target location but weren't part of the previous successful deployment. Valid values include "DISALLOW", "OVERWRITE", and "RETAIN".
--
--
_InvalidFileExistsBehaviorException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidFileExistsBehaviorException =
  _MatchServiceError codeDeploy "InvalidFileExistsBehaviorException"


-- | The format of the alarm configuration is invalid. Possible causes include:
--
--
--     * The alarm list is null.
--
--     * The alarm object is null.
--
--     * The alarm name is empty or null or exceeds the 255 character limit.
--
--     * Two alarms with the same name have been specified.
--
--     * The alarm configuration is enabled but the alarm list is empty.
--
--
--
_InvalidAlarmConfigException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAlarmConfigException =
  _MatchServiceError codeDeploy "InvalidAlarmConfigException"


-- | The specified on-premises instance name is already registered.
--
--
_InstanceNameAlreadyRegisteredException :: AsError a => Getting (First ServiceError) a ServiceError
_InstanceNameAlreadyRegisteredException =
  _MatchServiceError codeDeploy "InstanceNameAlreadyRegisteredException"


-- | An IAM user ARN was not specified.
--
--
_IAMUserARNRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_IAMUserARNRequiredException =
  _MatchServiceError codeDeploy "IamUserArnRequiredException"


-- | The deployment group name was specified in an invalid format.
--
--
_InvalidDeploymentGroupNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeploymentGroupNameException =
  _MatchServiceError codeDeploy "InvalidDeploymentGroupNameException"


-- | An invalid instance type was specified for instances in a blue/green deployment. Valid values include "Blue" for an original environment and "Green" for a replacement environment.
--
--
_InvalidInstanceTypeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInstanceTypeException =
  _MatchServiceError codeDeploy "InvalidInstanceTypeException"


-- | The request included an IAM session ARN that has already been used to register a different instance.
--
--
_IAMSessionARNAlreadyRegisteredException :: AsError a => Getting (First ServiceError) a ServiceError
_IAMSessionARNAlreadyRegisteredException =
  _MatchServiceError codeDeploy "IamSessionArnAlreadyRegisteredException"


-- | The configuration that specifies how traffic is routed during a deployment is invalid.
--
--
_InvalidTrafficRoutingConfigurationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTrafficRoutingConfigurationException =
  _MatchServiceError codeDeploy "InvalidTrafficRoutingConfigurationException"


-- | The description is too long.
--
--
_DescriptionTooLongException :: AsError a => Getting (First ServiceError) a ServiceError
_DescriptionTooLongException =
  _MatchServiceError codeDeploy "DescriptionTooLongException"


-- | The IAM user ARN was specified in an invalid format.
--
--
_InvalidIAMUserARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidIAMUserARNException =
  _MatchServiceError codeDeploy "InvalidIamUserArnException"


-- | A call was submitted that specified both OnPremisesTagFilters and OnPremisesTagSet, but only one of these data types can be used in a single call.
--
--
_InvalidOnPremisesTagCombinationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOnPremisesTagCombinationException =
  _MatchServiceError codeDeploy "InvalidOnPremisesTagCombinationException"


-- | The specified deployment has not started.
--
--
_DeploymentNotStartedException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentNotStartedException =
  _MatchServiceError codeDeploy "DeploymentNotStartedException"


-- | The deployment configurations limit was exceeded.
--
--
_DeploymentConfigLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentConfigLimitExceededException =
  _MatchServiceError codeDeploy "DeploymentConfigLimitExceededException"


-- | The role ID was not specified.
--
--
_RoleRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RoleRequiredException = _MatchServiceError codeDeploy "RoleRequiredException"


-- | An invalid load balancer name, or no load balancer name, was specified.
--
--
_InvalidLoadBalancerInfoException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidLoadBalancerInfoException =
  _MatchServiceError codeDeploy "InvalidLoadBalancerInfoException"


-- | The configuration for the blue/green deployment group was provided in an invalid format. For information about deployment configuration format, see 'CreateDeploymentConfig' .
--
--
_InvalidBlueGreenDeploymentConfigurationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidBlueGreenDeploymentConfigurationException =
  _MatchServiceError
    codeDeploy
    "InvalidBlueGreenDeploymentConfigurationException"


-- | The service role ARN was specified in an invalid format. Or, if an Auto Scaling group was specified, the specified service role does not grant the appropriate permissions to Auto Scaling.
--
--
_InvalidRoleException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRoleException = _MatchServiceError codeDeploy "InvalidRoleException"


-- | A deployment configuration with the specified name already exists with the applicable IAM user or AWS account.
--
--
_DeploymentConfigAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentConfigAlreadyExistsException =
  _MatchServiceError codeDeploy "DeploymentConfigAlreadyExistsException"


-- | The target instance configuration is invalid. Possible causes include:
--
--
--     * Configuration data for target instances was entered for an in-place deployment.
--
--     * The limit of 10 tags for a tag type was exceeded.
--
--     * The combined length of the tag names exceeded the limit.
--
--     * A specified tag is not currently applied to any instances.
--
--
--
_InvalidTargetInstancesException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTargetInstancesException =
  _MatchServiceError codeDeploy "InvalidTargetInstancesException"


-- | The number of allowed deployments was exceeded.
--
--
_DeploymentLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentLimitExceededException =
  _MatchServiceError codeDeploy "DeploymentLimitExceededException"


-- | The specified IAM user ARN is already registered with an on-premises instance.
--
--
_IAMUserARNAlreadyRegisteredException :: AsError a => Getting (First ServiceError) a ServiceError
_IAMUserARNAlreadyRegisteredException =
  _MatchServiceError codeDeploy "IamUserArnAlreadyRegisteredException"


-- | The IAM session ARN was specified in an invalid format.
--
--
_InvalidIAMSessionARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidIAMSessionARNException =
  _MatchServiceError codeDeploy "InvalidIamSessionArnException"


-- | The maximum number of allowed on-premises instances in a single call was exceeded.
--
--
_InstanceLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_InstanceLimitExceededException =
  _MatchServiceError codeDeploy "InstanceLimitExceededException"


-- | A lifecycle event hook is invalid. Review the @hooks@ section in your AppSpec file to ensure the lifecycle events and @hooks@ functions are valid.
--
--
_InvalidLifecycleEventHookExecutionIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidLifecycleEventHookExecutionIdException =
  _MatchServiceError codeDeploy "InvalidLifecycleEventHookExecutionIdException"


-- | An invalid deployment style was specified. Valid deployment types include "IN_PLACE" and "BLUE_GREEN". Valid deployment options include "WITH_TRAFFIC_CONTROL" and "WITHOUT_TRAFFIC_CONTROL".
--
--
_InvalidDeploymentStyleException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeploymentStyleException =
  _MatchServiceError codeDeploy "InvalidDeploymentStyleException"


-- | The deployed state filter was specified in an invalid format.
--
--
_InvalidDeployedStateFilterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeployedStateFilterException =
  _MatchServiceError codeDeploy "InvalidDeployedStateFilterException"


-- | The Auto Scaling group was specified in an invalid format or does not exist.
--
--
_InvalidAutoScalingGroupException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAutoScalingGroupException =
  _MatchServiceError codeDeploy "InvalidAutoScalingGroupException"


-- | The application name was specified in an invalid format.
--
--
_InvalidApplicationNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidApplicationNameException =
  _MatchServiceError codeDeploy "InvalidApplicationNameException"


-- | No GitHub account connection exists with the named specified in the call.
--
--
_GitHubAccountTokenDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_GitHubAccountTokenDoesNotExistException =
  _MatchServiceError codeDeploy "GitHubAccountTokenDoesNotExistException"


-- | The application does not exist with the applicable IAM user or AWS account.
--
--
_ApplicationDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_ApplicationDoesNotExistException =
  _MatchServiceError codeDeploy "ApplicationDoesNotExistException"


-- | The minimum healthy instance value was specified in an invalid format.
--
--
_InvalidMinimumHealthyHostValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidMinimumHealthyHostValueException =
  _MatchServiceError codeDeploy "InvalidMinimumHealthyHostValueException"


-- | A call was submitted that is not supported for the specified deployment type.
--
--
_UnsupportedActionForDeploymentTypeException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedActionForDeploymentTypeException =
  _MatchServiceError codeDeploy "UnsupportedActionForDeploymentTypeException"


-- | The specified resource could not be validated.
--
--
_ResourceValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceValidationException =
  _MatchServiceError codeDeploy "ResourceValidationException"


-- | The GitHub token is not valid.
--
--
_InvalidGitHubAccountTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidGitHubAccountTokenException =
  _MatchServiceError codeDeploy "InvalidGitHubAccountTokenException"


-- | A call was submitted that specified both Ec2TagFilters and Ec2TagSet, but only one of these data types can be used in a single call.
--
--
_InvalidEC2TagCombinationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidEC2TagCombinationException =
  _MatchServiceError codeDeploy "InvalidEC2TagCombinationException"


-- | The result of a Lambda validation function that verifies a lifecycle event is invalid. It should return @Succeeded@ or @Failed@ .
--
--
_InvalidLifecycleEventHookExecutionStatusException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidLifecycleEventHookExecutionStatusException =
  _MatchServiceError
    codeDeploy
    "InvalidLifecycleEventHookExecutionStatusException"


-- | The maximum number of alarms for a deployment group (10) was exceeded.
--
--
_AlarmsLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_AlarmsLimitExceededException =
  _MatchServiceError codeDeploy "AlarmsLimitExceededException"


-- | The API used does not support the deployment.
--
--
_OperationNotSupportedException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationNotSupportedException =
  _MatchServiceError codeDeploy "OperationNotSupportedException"


-- | The specified tag filter was specified in an invalid format.
--
--
_InvalidTagFilterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTagFilterException =
  _MatchServiceError codeDeploy "InvalidTagFilterException"


-- | The trigger was specified in an invalid format.
--
--
_InvalidTriggerConfigException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTriggerConfigException =
  _MatchServiceError codeDeploy "InvalidTriggerConfigException"


-- | The IgnoreApplicationStopFailures value is invalid. For AWS Lambda deployments, @false@ is expected. For EC2/On-premises deployments, @true@ or @false@ is expected.
--
--
_InvalidIgnoreApplicationStopFailuresValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidIgnoreApplicationStopFailuresValueException =
  _MatchServiceError
    codeDeploy
    "InvalidIgnoreApplicationStopFailuresValueException"


-- | The UpdateOutdatedInstancesOnly value is invalid. For AWS Lambda deployments, @false@ is expected. For EC2/On-premises deployments, @true@ or @false@ is expected.
--
--
_InvalidUpdateOutdatedInstancesOnlyValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidUpdateOutdatedInstancesOnlyValueException =
  _MatchServiceError
    codeDeploy
    "InvalidUpdateOutdatedInstancesOnlyValueException"


-- | A tag was not specified.
--
--
_TagRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_TagRequiredException = _MatchServiceError codeDeploy "TagRequiredException"


-- | The deployment group name was not specified.
--
--
_DeploymentGroupNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentGroupNameRequiredException =
  _MatchServiceError codeDeploy "DeploymentGroupNameRequiredException"


-- | A bucket name is required, but was not provided.
--
--
_BucketNameFilterRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_BucketNameFilterRequiredException =
  _MatchServiceError codeDeploy "BucketNameFilterRequiredException"


-- | The deployment configuration does not exist with the applicable IAM user or AWS account.
--
--
_DeploymentConfigDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentConfigDoesNotExistException =
  _MatchServiceError codeDeploy "DeploymentConfigDoesNotExistException"


-- | The bucket name either doesn't exist or was specified in an invalid format.
--
--
_InvalidBucketNameFilterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidBucketNameFilterException =
  _MatchServiceError codeDeploy "InvalidBucketNameFilterException"


-- | A deployment group with the specified name already exists with the applicable IAM user or AWS account.
--
--
_DeploymentGroupAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentGroupAlreadyExistsException =
  _MatchServiceError codeDeploy "DeploymentGroupAlreadyExistsException"


-- | The column name to sort by is either not present or was specified in an invalid format.
--
--
_InvalidSortByException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSortByException = _MatchServiceError codeDeploy "InvalidSortByException"


-- | The named revision does not exist with the applicable IAM user or AWS account.
--
--
_RevisionDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_RevisionDoesNotExistException =
  _MatchServiceError codeDeploy "RevisionDoesNotExistException"


-- | The deployment groups limit was exceeded.
--
--
_DeploymentGroupLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentGroupLimitExceededException =
  _MatchServiceError codeDeploy "DeploymentGroupLimitExceededException"


-- | The named deployment group does not exist with the applicable IAM user or AWS account.
--
--
_DeploymentGroupDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentGroupDoesNotExistException =
  _MatchServiceError codeDeploy "DeploymentGroupDoesNotExistException"


-- | An API function was called too frequently.
--
--
_ThrottlingException :: AsError a => Getting (First ServiceError) a ServiceError
_ThrottlingException = _MatchServiceError codeDeploy "ThrottlingException"


-- | The deployment configuration name was specified in an invalid format.
--
--
_InvalidDeploymentConfigNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeploymentConfigNameException =
  _MatchServiceError codeDeploy "InvalidDeploymentConfigNameException"


-- | The deployment configuration name was not specified.
--
--
_DeploymentConfigNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentConfigNameRequiredException =
  _MatchServiceError codeDeploy "DeploymentConfigNameRequiredException"


-- | At least one deployment ID must be specified.
--
--
_DeploymentIdRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentIdRequiredException =
  _MatchServiceError codeDeploy "DeploymentIdRequiredException"


-- |
--
--
_InvalidInstanceIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInstanceIdException =
  _MatchServiceError codeDeploy "InvalidInstanceIdException"


-- | The deployment does not have a status of Ready and can't continue yet.
--
--
_DeploymentIsNotInReadyStateException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentIsNotInReadyStateException =
  _MatchServiceError codeDeploy "DeploymentIsNotInReadyStateException"


-- | The next token was specified in an invalid format.
--
--
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
  _MatchServiceError codeDeploy "InvalidNextTokenException"


-- | The instance ID was not specified.
--
--
_InstanceIdRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_InstanceIdRequiredException =
  _MatchServiceError codeDeploy "InstanceIdRequiredException"


-- | At least one of the deployment IDs was specified in an invalid format.
--
--
_InvalidDeploymentIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeploymentIdException =
  _MatchServiceError codeDeploy "InvalidDeploymentIdException"


-- | The sort order was specified in an invalid format.
--
--
_InvalidSortOrderException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSortOrderException =
  _MatchServiceError codeDeploy "InvalidSortOrderException"


-- | The automatic rollback configuration was specified in an invalid format. For example, automatic rollback is enabled but an invalid triggering event type or no event types were listed.
--
--
_InvalidAutoRollbackConfigException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAutoRollbackConfigException =
  _MatchServiceError codeDeploy "InvalidAutoRollbackConfigException"


-- | The deployment is already complete.
--
--
_DeploymentAlreadyCompletedException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentAlreadyCompletedException =
  _MatchServiceError codeDeploy "DeploymentAlreadyCompletedException"


-- | The deployment does not exist with the applicable IAM user or AWS account.
--
--
_DeploymentDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentDoesNotExistException =
  _MatchServiceError codeDeploy "DeploymentDoesNotExistException"


-- | The maximum number of names or IDs allowed for this request (100) was exceeded.
--
--
_BatchLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_BatchLimitExceededException =
  _MatchServiceError codeDeploy "BatchLimitExceededException"


-- | The revision was specified in an invalid format.
--
--
_InvalidRevisionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRevisionException =
  _MatchServiceError codeDeploy "InvalidRevisionException"


-- | The revision ID was not specified.
--
--
_RevisionRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RevisionRequiredException =
  _MatchServiceError codeDeploy "RevisionRequiredException"


-- | The specified instance does not exist in the deployment group.
--
--
_InstanceDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_InstanceDoesNotExistException =
  _MatchServiceError codeDeploy "InstanceDoesNotExistException"


-- | The deployment configuration is still in use.
--
--
_DeploymentConfigInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentConfigInUseException =
  _MatchServiceError codeDeploy "DeploymentConfigInUseException"


-- | The specified input was specified in an invalid format.
--
--
_InvalidInputException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInputException = _MatchServiceError codeDeploy "InvalidInputException"


-- | The tag was specified in an invalid format.
--
--
_InvalidEC2TagException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidEC2TagException = _MatchServiceError codeDeploy "InvalidEC2TagException"


-- | The specified on-premises instance name was specified in an invalid format.
--
--
_InvalidInstanceNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInstanceNameException =
  _MatchServiceError codeDeploy "InvalidInstanceNameException"


-- | An on-premises instance name was not specified.
--
--
_InstanceNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_InstanceNameRequiredException =
  _MatchServiceError codeDeploy "InstanceNameRequiredException"


-- | Both an IAM user ARN and an IAM session ARN were included in the request. Use only one ARN type.
--
--
_MultipleIAMARNsProvidedException :: AsError a => Getting (First ServiceError) a ServiceError
_MultipleIAMARNsProvidedException =
  _MatchServiceError codeDeploy "MultipleIamArnsProvidedException"


-- | The maximum allowed number of triggers was exceeded.
--
--
_TriggerTargetsLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_TriggerTargetsLimitExceededException =
  _MatchServiceError codeDeploy "TriggerTargetsLimitExceededException"


-- | The specified deployment status doesn't exist or cannot be determined.
--
--
_InvalidDeploymentStatusException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeploymentStatusException =
  _MatchServiceError codeDeploy "InvalidDeploymentStatusException"


-- | The registration status was specified in an invalid format.
--
--
_InvalidRegistrationStatusException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRegistrationStatusException =
  _MatchServiceError codeDeploy "InvalidRegistrationStatusException"


-- | The minimum number of required application names was not specified.
--
--
_ApplicationNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_ApplicationNameRequiredException =
  _MatchServiceError codeDeploy "ApplicationNameRequiredException"


-- | The specified on-premises instance is not registered.
--
--
_InstanceNotRegisteredException :: AsError a => Getting (First ServiceError) a ServiceError
_InstanceNotRegisteredException =
  _MatchServiceError codeDeploy "InstanceNotRegisteredException"


-- | An application with the specified name already exists with the applicable IAM user or AWS account.
--
--
_ApplicationAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ApplicationAlreadyExistsException =
  _MatchServiceError codeDeploy "ApplicationAlreadyExistsException"


-- | The specified instance status does not exist.
--
--
_InvalidInstanceStatusException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInstanceStatusException =
  _MatchServiceError codeDeploy "InvalidInstanceStatusException"


-- | The maximum allowed number of tags was exceeded.
--
--
_TagLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_TagLimitExceededException =
  _MatchServiceError codeDeploy "TagLimitExceededException"


-- | More applications were attempted to be created than are allowed.
--
--
_ApplicationLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ApplicationLimitExceededException =
  _MatchServiceError codeDeploy "ApplicationLimitExceededException"


-- | The number of tag groups included in the tag set list exceeded the maximum allowed limit of 3.
--
--
_TagSetListLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_TagSetListLimitExceededException =
  _MatchServiceError codeDeploy "TagSetListLimitExceededException"


-- | An invalid operation was detected.
--
--
_InvalidOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOperationException =
  _MatchServiceError codeDeploy "InvalidOperationException"


-- | The call is missing a required GitHub account connection name.
--
--
_GitHubAccountTokenNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_GitHubAccountTokenNameRequiredException =
  _MatchServiceError codeDeploy "GitHubAccountTokenNameRequiredException"


-- | An instance type was specified for an in-place deployment. Instance types are supported for blue/green deployments only.
--
--
_InvalidDeploymentInstanceTypeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeploymentInstanceTypeException =
  _MatchServiceError codeDeploy "InvalidDeploymentInstanceTypeException"


-- | No IAM ARN was included in the request. You must use an IAM session ARN or IAM user ARN in the request.
--
--
_IAMARNRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_IAMARNRequiredException =
  _MatchServiceError codeDeploy "IamArnRequiredException"


-- | The format of the specified GitHub account connection name is invalid.
--
--
_InvalidGitHubAccountTokenNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidGitHubAccountTokenNameException =
  _MatchServiceError codeDeploy "InvalidGitHubAccountTokenNameException"


-- | An attempt to return the status of an already completed lifecycle event occurred.
--
--
_LifecycleEventAlreadyCompletedException :: AsError a => Getting (First ServiceError) a ServiceError
_LifecycleEventAlreadyCompletedException =
  _MatchServiceError codeDeploy "LifecycleEventAlreadyCompletedException"


-- | The specified key prefix filter was specified in an invalid format.
--
--
_InvalidKeyPrefixFilterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidKeyPrefixFilterException =
  _MatchServiceError codeDeploy "InvalidKeyPrefixFilterException"


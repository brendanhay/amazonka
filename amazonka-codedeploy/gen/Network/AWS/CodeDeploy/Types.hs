{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
    , _InvalidTagException
    , _InstanceNameAlreadyRegisteredException
    , _IAMUserARNRequiredException
    , _InvalidDeploymentGroupNameException
    , _DescriptionTooLongException
    , _InvalidIAMUserARNException
    , _DeploymentNotStartedException
    , _DeploymentConfigLimitExceededException
    , _RoleRequiredException
    , _InvalidRoleException
    , _DeploymentConfigAlreadyExistsException
    , _DeploymentLimitExceededException
    , _IAMUserARNAlreadyRegisteredException
    , _InstanceLimitExceededException
    , _InvalidDeployedStateFilterException
    , _InvalidAutoScalingGroupException
    , _InvalidApplicationNameException
    , _ApplicationDoesNotExistException
    , _InvalidMinimumHealthyHostValueException
    , _InvalidTagFilterException
    , _InvalidTriggerConfigException
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
    , _InvalidDeploymentConfigNameException
    , _DeploymentConfigNameRequiredException
    , _DeploymentIdRequiredException
    , _InvalidNextTokenException
    , _InstanceIdRequiredException
    , _InvalidDeploymentIdException
    , _InvalidSortOrderException
    , _DeploymentAlreadyCompletedException
    , _DeploymentDoesNotExistException
    , _BatchLimitExceededException
    , _InvalidRevisionException
    , _RevisionRequiredException
    , _InstanceDoesNotExistException
    , _DeploymentConfigInUseException
    , _InvalidEC2TagException
    , _InvalidInstanceNameException
    , _InstanceNameRequiredException
    , _TriggerTargetsLimitExceededException
    , _InvalidDeploymentStatusException
    , _InvalidRegistrationStatusException
    , _ApplicationNameRequiredException
    , _InstanceNotRegisteredException
    , _ApplicationAlreadyExistsException
    , _InvalidInstanceStatusException
    , _TagLimitExceededException
    , _ApplicationLimitExceededException
    , _InvalidOperationException
    , _InvalidKeyPrefixFilterException

    -- * ApplicationRevisionSortBy
    , ApplicationRevisionSortBy (..)

    -- * BundleType
    , BundleType (..)

    -- * DeployErrorCode
    , DeployErrorCode (..)

    -- * DeploymentCreator
    , DeploymentCreator (..)

    -- * DeploymentStatus
    , DeploymentStatus (..)

    -- * EC2TagFilterType
    , EC2TagFilterType (..)

    -- * InstanceStatus
    , InstanceStatus (..)

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

    -- * TriggerEventType
    , TriggerEventType (..)

    -- * ApplicationInfo
    , ApplicationInfo
    , applicationInfo
    , aiLinkedToGitHub
    , aiApplicationId
    , aiApplicationName
    , aiCreateTime

    -- * AutoScalingGroup
    , AutoScalingGroup
    , autoScalingGroup
    , asgHook
    , asgName

    -- * DeploymentConfigInfo
    , DeploymentConfigInfo
    , deploymentConfigInfo
    , dciDeploymentConfigName
    , dciMinimumHealthyHosts
    , dciDeploymentConfigId
    , dciCreateTime

    -- * DeploymentGroupInfo
    , DeploymentGroupInfo
    , deploymentGroupInfo
    , dgiServiceRoleARN
    , dgiDeploymentConfigName
    , dgiTargetRevision
    , dgiEc2TagFilters
    , dgiOnPremisesInstanceTagFilters
    , dgiApplicationName
    , dgiTriggerConfigurations
    , dgiDeploymentGroupId
    , dgiAutoScalingGroups
    , dgiDeploymentGroupName

    -- * DeploymentInfo
    , DeploymentInfo
    , deploymentInfo
    , diCreator
    , diStatus
    , diDeploymentId
    , diDeploymentConfigName
    , diStartTime
    , diCompleteTime
    , diErrorInformation
    , diDeploymentOverview
    , diApplicationName
    , diRevision
    , diDescription
    , diCreateTime
    , diDeploymentGroupName
    , diIgnoreApplicationStopFailures

    -- * DeploymentOverview
    , DeploymentOverview
    , deploymentOverview
    , doPending
    , doSkipped
    , doInProgress
    , doSucceeded
    , doFailed

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

    -- * InstanceInfo
    , InstanceInfo
    , instanceInfo
    , iiRegisterTime
    , iiInstanceARN
    , iiDeregisterTime
    , iiIamUserARN
    , iiInstanceName
    , iiTags

    -- * InstanceSummary
    , InstanceSummary
    , instanceSummary
    , isInstanceId
    , isStatus
    , isDeploymentId
    , isLastUpdatedAt
    , isLifecycleEvents

    -- * LifecycleEvent
    , LifecycleEvent
    , lifecycleEvent
    , leStatus
    , leLifecycleEventName
    , leStartTime
    , leDiagnostics
    , leEndTime

    -- * MinimumHealthyHosts
    , MinimumHealthyHosts
    , minimumHealthyHosts
    , mhhValue
    , mhhType

    -- * RevisionInfo
    , RevisionInfo
    , revisionInfo
    , riGenericRevisionInfo
    , riRevisionLocation

    -- * RevisionLocation
    , RevisionLocation
    , revisionLocation
    , rlRevisionType
    , rlS3Location
    , rlGitHubLocation

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

    -- * TimeRange
    , TimeRange
    , timeRange
    , trStart
    , trEnd

    -- * TriggerConfig
    , TriggerConfig
    , triggerConfig
    , tcTriggerName
    , tcTriggerEvents
    , tcTriggerTargetARN
    ) where

import           Network.AWS.CodeDeploy.Types.Product
import           Network.AWS.CodeDeploy.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2014-10-06' of the Amazon CodeDeploy SDK configuration.
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
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | The limit for lifecycle hooks was exceeded.
_LifecycleHookLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LifecycleHookLimitExceededException =
    _ServiceError . hasCode "LifecycleHookLimitExceededException"

-- | The specified time range was specified in an invalid format.
_InvalidTimeRangeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTimeRangeException =
    _ServiceError . hasCode "InvalidTimeRangeException"

-- | The specified tag was specified in an invalid format.
_InvalidTagException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTagException = _ServiceError . hasCode "InvalidTagException"

-- | The specified on-premises instance name is already registered.
_InstanceNameAlreadyRegisteredException :: AsError a => Getting (First ServiceError) a ServiceError
_InstanceNameAlreadyRegisteredException =
    _ServiceError . hasCode "InstanceNameAlreadyRegisteredException"

-- | An IAM user ARN was not specified.
_IAMUserARNRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_IAMUserARNRequiredException =
    _ServiceError . hasCode "IamUserArnRequiredException"

-- | The deployment group name was specified in an invalid format.
_InvalidDeploymentGroupNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeploymentGroupNameException =
    _ServiceError . hasCode "InvalidDeploymentGroupNameException"

-- | The description is too long.
_DescriptionTooLongException :: AsError a => Getting (First ServiceError) a ServiceError
_DescriptionTooLongException =
    _ServiceError . hasCode "DescriptionTooLongException"

-- | The IAM user ARN was specified in an invalid format.
_InvalidIAMUserARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidIAMUserARNException =
    _ServiceError . hasCode "InvalidIamUserArnException"

-- | The specified deployment has not started.
_DeploymentNotStartedException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentNotStartedException =
    _ServiceError . hasCode "DeploymentNotStartedException"

-- | The deployment configurations limit was exceeded.
_DeploymentConfigLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentConfigLimitExceededException =
    _ServiceError . hasCode "DeploymentConfigLimitExceededException"

-- | The role ID was not specified.
_RoleRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RoleRequiredException = _ServiceError . hasCode "RoleRequiredException"

-- | The service role ARN was specified in an invalid format. Or, if an Auto Scaling group was specified, the specified service role does not grant the appropriate permissions to Auto Scaling.
_InvalidRoleException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRoleException = _ServiceError . hasCode "InvalidRoleException"

-- | A deployment configuration with the specified name already exists with the applicable IAM user or AWS account.
_DeploymentConfigAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentConfigAlreadyExistsException =
    _ServiceError . hasCode "DeploymentConfigAlreadyExistsException"

-- | The number of allowed deployments was exceeded.
_DeploymentLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentLimitExceededException =
    _ServiceError . hasCode "DeploymentLimitExceededException"

-- | The specified IAM user ARN is already registered with an on-premises instance.
_IAMUserARNAlreadyRegisteredException :: AsError a => Getting (First ServiceError) a ServiceError
_IAMUserARNAlreadyRegisteredException =
    _ServiceError . hasCode "IamUserArnAlreadyRegisteredException"

-- | The maximum number of allowed on-premises instances in a single call was exceeded.
_InstanceLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_InstanceLimitExceededException =
    _ServiceError . hasCode "InstanceLimitExceededException"

-- | The deployed state filter was specified in an invalid format.
_InvalidDeployedStateFilterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeployedStateFilterException =
    _ServiceError . hasCode "InvalidDeployedStateFilterException"

-- | The Auto Scaling group was specified in an invalid format or does not exist.
_InvalidAutoScalingGroupException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAutoScalingGroupException =
    _ServiceError . hasCode "InvalidAutoScalingGroupException"

-- | The application name was specified in an invalid format.
_InvalidApplicationNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidApplicationNameException =
    _ServiceError . hasCode "InvalidApplicationNameException"

-- | The application does not exist with the applicable IAM user or AWS account.
_ApplicationDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_ApplicationDoesNotExistException =
    _ServiceError . hasCode "ApplicationDoesNotExistException"

-- | The minimum healthy instance value was specified in an invalid format.
_InvalidMinimumHealthyHostValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidMinimumHealthyHostValueException =
    _ServiceError . hasCode "InvalidMinimumHealthyHostValueException"

-- | The specified tag filter was specified in an invalid format.
_InvalidTagFilterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTagFilterException =
    _ServiceError . hasCode "InvalidTagFilterException"

-- | The trigger was specified in an invalid format.
_InvalidTriggerConfigException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTriggerConfigException =
    _ServiceError . hasCode "InvalidTriggerConfigException"

-- | A tag was not specified.
_TagRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_TagRequiredException = _ServiceError . hasCode "TagRequiredException"

-- | The deployment group name was not specified.
_DeploymentGroupNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentGroupNameRequiredException =
    _ServiceError . hasCode "DeploymentGroupNameRequiredException"

-- | A bucket name is required, but was not provided.
_BucketNameFilterRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_BucketNameFilterRequiredException =
    _ServiceError . hasCode "BucketNameFilterRequiredException"

-- | The deployment configuration does not exist with the applicable IAM user or AWS account.
_DeploymentConfigDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentConfigDoesNotExistException =
    _ServiceError . hasCode "DeploymentConfigDoesNotExistException"

-- | The bucket name either doesn\'t exist or was specified in an invalid format.
_InvalidBucketNameFilterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidBucketNameFilterException =
    _ServiceError . hasCode "InvalidBucketNameFilterException"

-- | A deployment group with the specified name already exists with the applicable IAM user or AWS account.
_DeploymentGroupAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentGroupAlreadyExistsException =
    _ServiceError . hasCode "DeploymentGroupAlreadyExistsException"

-- | The column name to sort by is either not present or was specified in an invalid format.
_InvalidSortByException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSortByException = _ServiceError . hasCode "InvalidSortByException"

-- | The named revision does not exist with the applicable IAM user or AWS account.
_RevisionDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_RevisionDoesNotExistException =
    _ServiceError . hasCode "RevisionDoesNotExistException"

-- | The deployment groups limit was exceeded.
_DeploymentGroupLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentGroupLimitExceededException =
    _ServiceError . hasCode "DeploymentGroupLimitExceededException"

-- | The named deployment group does not exist with the applicable IAM user or AWS account.
_DeploymentGroupDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentGroupDoesNotExistException =
    _ServiceError . hasCode "DeploymentGroupDoesNotExistException"

-- | The deployment configuration name was specified in an invalid format.
_InvalidDeploymentConfigNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeploymentConfigNameException =
    _ServiceError . hasCode "InvalidDeploymentConfigNameException"

-- | The deployment configuration name was not specified.
_DeploymentConfigNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentConfigNameRequiredException =
    _ServiceError . hasCode "DeploymentConfigNameRequiredException"

-- | At least one deployment ID must be specified.
_DeploymentIdRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentIdRequiredException =
    _ServiceError . hasCode "DeploymentIdRequiredException"

-- | The next token was specified in an invalid format.
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
    _ServiceError . hasCode "InvalidNextTokenException"

-- | The instance ID was not specified.
_InstanceIdRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_InstanceIdRequiredException =
    _ServiceError . hasCode "InstanceIdRequiredException"

-- | At least one of the deployment IDs was specified in an invalid format.
_InvalidDeploymentIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeploymentIdException =
    _ServiceError . hasCode "InvalidDeploymentIdException"

-- | The sort order was specified in an invalid format.
_InvalidSortOrderException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSortOrderException =
    _ServiceError . hasCode "InvalidSortOrderException"

-- | The deployment is already complete.
_DeploymentAlreadyCompletedException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentAlreadyCompletedException =
    _ServiceError . hasCode "DeploymentAlreadyCompletedException"

-- | The deployment does not exist with the applicable IAM user or AWS account.
_DeploymentDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentDoesNotExistException =
    _ServiceError . hasCode "DeploymentDoesNotExistException"

-- | The maximum number of names or IDs allowed for this request (100) was exceeded.
_BatchLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_BatchLimitExceededException =
    _ServiceError . hasCode "BatchLimitExceededException"

-- | The revision was specified in an invalid format.
_InvalidRevisionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRevisionException = _ServiceError . hasCode "InvalidRevisionException"

-- | The revision ID was not specified.
_RevisionRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RevisionRequiredException =
    _ServiceError . hasCode "RevisionRequiredException"

-- | The specified instance does not exist in the deployment group.
_InstanceDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_InstanceDoesNotExistException =
    _ServiceError . hasCode "InstanceDoesNotExistException"

-- | The deployment configuration is still in use.
_DeploymentConfigInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentConfigInUseException =
    _ServiceError . hasCode "DeploymentConfigInUseException"

-- | The tag was specified in an invalid format.
_InvalidEC2TagException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidEC2TagException = _ServiceError . hasCode "InvalidEC2TagException"

-- | The specified on-premises instance name was specified in an invalid format.
_InvalidInstanceNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInstanceNameException =
    _ServiceError . hasCode "InvalidInstanceNameException"

-- | An on-premises instance name was not specified.
_InstanceNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_InstanceNameRequiredException =
    _ServiceError . hasCode "InstanceNameRequiredException"

-- | The maximum allowed number of triggers was exceeded.
_TriggerTargetsLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_TriggerTargetsLimitExceededException =
    _ServiceError . hasCode "TriggerTargetsLimitExceededException"

-- | The specified deployment status doesn\'t exist or cannot be determined.
_InvalidDeploymentStatusException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeploymentStatusException =
    _ServiceError . hasCode "InvalidDeploymentStatusException"

-- | The registration status was specified in an invalid format.
_InvalidRegistrationStatusException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRegistrationStatusException =
    _ServiceError . hasCode "InvalidRegistrationStatusException"

-- | The minimum number of required application names was not specified.
_ApplicationNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_ApplicationNameRequiredException =
    _ServiceError . hasCode "ApplicationNameRequiredException"

-- | The specified on-premises instance is not registered.
_InstanceNotRegisteredException :: AsError a => Getting (First ServiceError) a ServiceError
_InstanceNotRegisteredException =
    _ServiceError . hasCode "InstanceNotRegisteredException"

-- | An application with the specified name already exists with the applicable IAM user or AWS account.
_ApplicationAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ApplicationAlreadyExistsException =
    _ServiceError . hasCode "ApplicationAlreadyExistsException"

-- | The specified instance status does not exist.
_InvalidInstanceStatusException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInstanceStatusException =
    _ServiceError . hasCode "InvalidInstanceStatusException"

-- | The maximum allowed number of tags was exceeded.
_TagLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_TagLimitExceededException =
    _ServiceError . hasCode "TagLimitExceededException"

-- | More applications were attempted to be created than are allowed.
_ApplicationLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ApplicationLimitExceededException =
    _ServiceError . hasCode "ApplicationLimitExceededException"

-- | An invalid operation was detected.
_InvalidOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOperationException =
    _ServiceError . hasCode "InvalidOperationException"

-- | The specified key prefix filter was specified in an invalid format.
_InvalidKeyPrefixFilterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidKeyPrefixFilterException =
    _ServiceError . hasCode "InvalidKeyPrefixFilterException"

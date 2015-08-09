{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types
    (
    -- * Service Decription
      CodeDeploy

    -- * Error Matchers
    , _InvalidTimeRangeException
    , _InvalidTagException
    , _InstanceNameAlreadyRegisteredException
    , _InvalidIAMUserARNException
    , _IAMUserARNRequiredException
    , _InvalidDeploymentGroupNameException
    , _DescriptionTooLongException
    , _DeploymentConfigAlreadyExistsException
    , _DeploymentConfigLimitExceededException
    , _InvalidRoleException
    , _DeploymentNotStartedException
    , _RoleRequiredException
    , _IAMUserARNAlreadyRegisteredException
    , _DeploymentLimitExceededException
    , _InstanceLimitExceededException
    , _InvalidAutoScalingGroupException
    , _InvalidApplicationNameException
    , _InvalidDeployedStateFilterException
    , _InvalidMinimumHealthyHostValueException
    , _ApplicationDoesNotExistException
    , _InvalidTagFilterException
    , _TagRequiredException
    , _RevisionDoesNotExistException
    , _DeploymentGroupNameRequiredException
    , _InvalidBucketNameFilterException
    , _DeploymentConfigDoesNotExistException
    , _InvalidSortByException
    , _BucketNameFilterRequiredException
    , _DeploymentGroupLimitExceededException
    , _DeploymentGroupAlreadyExistsException
    , _InvalidDeploymentIdException
    , _DeploymentGroupDoesNotExistException
    , _DeploymentIdRequiredException
    , _InstanceIdRequiredException
    , _DeploymentConfigNameRequiredException
    , _InvalidDeploymentConfigNameException
    , _InvalidSortOrderException
    , _InvalidNextTokenException
    , _InvalidRevisionException
    , _DeploymentAlreadyCompletedException
    , _RevisionRequiredException
    , _InstanceDoesNotExistException
    , _DeploymentDoesNotExistException
    , _InstanceNameRequiredException
    , _DeploymentConfigInUseException
    , _InvalidEC2TagException
    , _InvalidInstanceNameException
    , _InvalidDeploymentStatusException
    , _InvalidRegistrationStatusException
    , _TagLimitExceededException
    , _InstanceNotRegisteredException
    , _ApplicationLimitExceededException
    , _InvalidOperationException
    , _ApplicationAlreadyExistsException
    , _InvalidInstanceStatusException
    , _ApplicationNameRequiredException
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
    , dgiDeploymentGroupId
    , dgiAutoScalingGroups
    , dgiDeploymentGroupName

    -- * DeploymentInfo
    , DeploymentInfo
    , deploymentInfo
    , diDeploymentId
    , diCreator
    , diStatus
    , diDeploymentConfigName
    , diStartTime
    , diCompleteTime
    , diErrorInformation
    , diDeploymentOverview
    , diApplicationName
    , diRevision
    , diDescription
    , diIgnoreApplicationStopFailures
    , diDeploymentGroupName
    , diCreateTime

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
    , iiInstanceARN
    , iiRegisterTime
    , iiDeregisterTime
    , iiIamUserARN
    , iiInstanceName
    , iiTags

    -- * InstanceSummary
    , InstanceSummary
    , instanceSummary
    , isInstanceId
    , isDeploymentId
    , isStatus
    , isLastUpdatedAt
    , isLifecycleEvents

    -- * LifecycleEvent
    , LifecycleEvent
    , lifecycleEvent
    , leStatus
    , leStartTime
    , leLifecycleEventName
    , leDiagnostics
    , leEndTime

    -- * MinimumHealthyHosts
    , MinimumHealthyHosts
    , minimumHealthyHosts
    , mhhValue
    , mhhType

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
    ) where

import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.CodeDeploy.Types.Sum
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | Version @2014-10-06@ of the Amazon CodeDeploy SDK.
data CodeDeploy

instance AWSService CodeDeploy where
    type Sg CodeDeploy = V4
    service = const svc
      where
        svc = 
            Service
            { _svcAbbrev = "CodeDeploy"
            , _svcPrefix = "codedeploy"
            , _svcVersion = "2014-10-06"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry = 
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e = 
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

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

-- | The IAM user ARN was specified in an invalid format.
_InvalidIAMUserARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidIAMUserARNException = 
    _ServiceError . hasCode "InvalidIamUserArnException"

-- | An IAM user ARN was not specified.
_IAMUserARNRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_IAMUserARNRequiredException = 
    _ServiceError . hasCode "IamUserArnRequiredException"

-- | The deployment group name was specified in an invalid format.
_InvalidDeploymentGroupNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeploymentGroupNameException = 
    _ServiceError . hasCode "InvalidDeploymentGroupNameException"

-- | The description that was provided is too long.
_DescriptionTooLongException :: AsError a => Getting (First ServiceError) a ServiceError
_DescriptionTooLongException = 
    _ServiceError . hasCode "DescriptionTooLongException"

-- | A deployment configuration with the specified name already exists with
-- the applicable IAM user or AWS account.
_DeploymentConfigAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentConfigAlreadyExistsException = 
    _ServiceError . hasCode "DeploymentConfigAlreadyExistsException"

-- | The deployment configurations limit was exceeded.
_DeploymentConfigLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentConfigLimitExceededException = 
    _ServiceError . hasCode "DeploymentConfigLimitExceededException"

-- | The service role ARN was specified in an invalid format. Or, if an Auto
-- Scaling group was specified, the specified service role does not grant
-- the appropriate permissions to Auto Scaling.
_InvalidRoleException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRoleException = _ServiceError . hasCode "InvalidRoleException"

-- | The specified deployment has not started.
_DeploymentNotStartedException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentNotStartedException = 
    _ServiceError . hasCode "DeploymentNotStartedException"

-- | The role ID was not specified.
_RoleRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RoleRequiredException = _ServiceError . hasCode "RoleRequiredException"

-- | The specified IAM user ARN is already registered with an on-premises
-- instance.
_IAMUserARNAlreadyRegisteredException :: AsError a => Getting (First ServiceError) a ServiceError
_IAMUserARNAlreadyRegisteredException = 
    _ServiceError . hasCode "IamUserArnAlreadyRegisteredException"

-- | The number of allowed deployments was exceeded.
_DeploymentLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentLimitExceededException = 
    _ServiceError . hasCode "DeploymentLimitExceededException"

-- | The maximum number of allowed on-premises instances in a single call was
-- exceeded.
_InstanceLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_InstanceLimitExceededException = 
    _ServiceError . hasCode "InstanceLimitExceededException"

-- | The Auto Scaling group was specified in an invalid format or does not
-- exist.
_InvalidAutoScalingGroupException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAutoScalingGroupException = 
    _ServiceError . hasCode "InvalidAutoScalingGroupException"

-- | The application name was specified in an invalid format.
_InvalidApplicationNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidApplicationNameException = 
    _ServiceError . hasCode "InvalidApplicationNameException"

-- | The deployed state filter was specified in an invalid format.
_InvalidDeployedStateFilterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeployedStateFilterException = 
    _ServiceError . hasCode "InvalidDeployedStateFilterException"

-- | The minimum healthy instances value was specified in an invalid format.
_InvalidMinimumHealthyHostValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidMinimumHealthyHostValueException = 
    _ServiceError . hasCode "InvalidMinimumHealthyHostValueException"

-- | The application does not exist with the applicable IAM user or AWS
-- account.
_ApplicationDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_ApplicationDoesNotExistException = 
    _ServiceError . hasCode "ApplicationDoesNotExistException"

-- | The specified tag filter was specified in an invalid format.
_InvalidTagFilterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTagFilterException = 
    _ServiceError . hasCode "InvalidTagFilterException"

-- | A tag was not specified.
_TagRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_TagRequiredException = _ServiceError . hasCode "TagRequiredException"

-- | The named revision does not exist with the applicable IAM user or AWS
-- account.
_RevisionDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_RevisionDoesNotExistException = 
    _ServiceError . hasCode "RevisionDoesNotExistException"

-- | The deployment group name was not specified.
_DeploymentGroupNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentGroupNameRequiredException = 
    _ServiceError . hasCode "DeploymentGroupNameRequiredException"

-- | The bucket name either doesn\'t exist or was specified in an invalid
-- format.
_InvalidBucketNameFilterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidBucketNameFilterException = 
    _ServiceError . hasCode "InvalidBucketNameFilterException"

-- | The deployment configuration does not exist with the applicable IAM user
-- or AWS account.
_DeploymentConfigDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentConfigDoesNotExistException = 
    _ServiceError . hasCode "DeploymentConfigDoesNotExistException"

-- | The column name to sort by is either not present or was specified in an
-- invalid format.
_InvalidSortByException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSortByException = _ServiceError . hasCode "InvalidSortByException"

-- | A bucket name is required but was not provided.
_BucketNameFilterRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_BucketNameFilterRequiredException = 
    _ServiceError . hasCode "BucketNameFilterRequiredException"

-- | The deployment groups limit was exceeded.
_DeploymentGroupLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentGroupLimitExceededException = 
    _ServiceError . hasCode "DeploymentGroupLimitExceededException"

-- | A deployment group with the specified name already exists with the
-- applicable IAM user or AWS account.
_DeploymentGroupAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentGroupAlreadyExistsException = 
    _ServiceError . hasCode "DeploymentGroupAlreadyExistsException"

-- | At least one of the deployment IDs was specified in an invalid format.
_InvalidDeploymentIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeploymentIdException = 
    _ServiceError . hasCode "InvalidDeploymentIdException"

-- | The named deployment group does not exist with the applicable IAM user
-- or AWS account.
_DeploymentGroupDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentGroupDoesNotExistException = 
    _ServiceError . hasCode "DeploymentGroupDoesNotExistException"

-- | At least one deployment ID must be specified.
_DeploymentIdRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentIdRequiredException = 
    _ServiceError . hasCode "DeploymentIdRequiredException"

-- | The instance ID was not specified.
_InstanceIdRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_InstanceIdRequiredException = 
    _ServiceError . hasCode "InstanceIdRequiredException"

-- | The deployment configuration name was not specified.
_DeploymentConfigNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentConfigNameRequiredException = 
    _ServiceError . hasCode "DeploymentConfigNameRequiredException"

-- | The deployment configuration name was specified in an invalid format.
_InvalidDeploymentConfigNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeploymentConfigNameException = 
    _ServiceError . hasCode "InvalidDeploymentConfigNameException"

-- | The sort order was specified in an invalid format.
_InvalidSortOrderException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSortOrderException = 
    _ServiceError . hasCode "InvalidSortOrderException"

-- | The next token was specified in an invalid format.
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException = 
    _ServiceError . hasCode "InvalidNextTokenException"

-- | The revision was specified in an invalid format.
_InvalidRevisionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRevisionException = _ServiceError . hasCode "InvalidRevisionException"

-- | The deployment is already completed.
_DeploymentAlreadyCompletedException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentAlreadyCompletedException = 
    _ServiceError . hasCode "DeploymentAlreadyCompletedException"

-- | The revision ID was not specified.
_RevisionRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RevisionRequiredException = 
    _ServiceError . hasCode "RevisionRequiredException"

-- | The specified instance does not exist in the deployment group.
_InstanceDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_InstanceDoesNotExistException = 
    _ServiceError . hasCode "InstanceDoesNotExistException"

-- | The deployment does not exist with the applicable IAM user or AWS
-- account.
_DeploymentDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentDoesNotExistException = 
    _ServiceError . hasCode "DeploymentDoesNotExistException"

-- | An on-premises instance name was not specified.
_InstanceNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_InstanceNameRequiredException = 
    _ServiceError . hasCode "InstanceNameRequiredException"

-- | The deployment configuration is still in use.
_DeploymentConfigInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_DeploymentConfigInUseException = 
    _ServiceError . hasCode "DeploymentConfigInUseException"

-- | The tag was specified in an invalid format.
_InvalidEC2TagException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidEC2TagException = _ServiceError . hasCode "InvalidEC2TagException"

-- | The specified on-premises instance name was specified in an invalid
-- format.
_InvalidInstanceNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInstanceNameException = 
    _ServiceError . hasCode "InvalidInstanceNameException"

-- | The specified deployment status doesn\'t exist or cannot be determined.
_InvalidDeploymentStatusException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeploymentStatusException = 
    _ServiceError . hasCode "InvalidDeploymentStatusException"

-- | The registration status was specified in an invalid format.
_InvalidRegistrationStatusException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRegistrationStatusException = 
    _ServiceError . hasCode "InvalidRegistrationStatusException"

-- | The maximum allowed number of tags was exceeded.
_TagLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_TagLimitExceededException = 
    _ServiceError . hasCode "TagLimitExceededException"

-- | The specified on-premises instance is not registered.
_InstanceNotRegisteredException :: AsError a => Getting (First ServiceError) a ServiceError
_InstanceNotRegisteredException = 
    _ServiceError . hasCode "InstanceNotRegisteredException"

-- | More applications were attempted to be created than were allowed.
_ApplicationLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ApplicationLimitExceededException = 
    _ServiceError . hasCode "ApplicationLimitExceededException"

-- | An invalid operation was detected.
_InvalidOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOperationException = 
    _ServiceError . hasCode "InvalidOperationException"

-- | An application with the specified name already exists with the
-- applicable IAM user or AWS account.
_ApplicationAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ApplicationAlreadyExistsException = 
    _ServiceError . hasCode "ApplicationAlreadyExistsException"

-- | The specified instance status does not exist.
_InvalidInstanceStatusException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInstanceStatusException = 
    _ServiceError . hasCode "InvalidInstanceStatusException"

-- | The minimum number of required application names was not specified.
_ApplicationNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_ApplicationNameRequiredException = 
    _ServiceError . hasCode "ApplicationNameRequiredException"

-- | The specified key prefix filter was specified in an invalid format.
_InvalidKeyPrefixFilterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidKeyPrefixFilterException = 
    _ServiceError . hasCode "InvalidKeyPrefixFilterException"

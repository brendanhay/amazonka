{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.CodeDeploy.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.CodeDeploy.Types
    (
    -- * Service
      CodeDeploy

    -- * Errors
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
    , diaLogTail
    , diaErrorCode
    , diaScriptName
    , diaMessage

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

import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | Version @2014-10-06@ of the Amazon CodeDeploy SDK.
data CodeDeploy

instance AWSService CodeDeploy where
    type Sg CodeDeploy = V4

    service = const svc
      where
        svc :: Service CodeDeploy
        svc = Service
            { _svcAbbrev   = "CodeDeploy"
            , _svcPrefix   = "codedeploy"
            , _svcVersion  = "2014-10-06"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout  = 80000000
            , _svcStatus   = statusSuccess
            , _svcError    = parseJSONError
            , _svcRetry    = retry
            }

        retry :: Retry
        retry = Exponential
            { _retryBase     = 0
            , _retryGrowth   = 0
            , _retryAttempts = 0
            , _retryCheck    = check
            }

        check :: ServiceError -> Bool
        check ServiceError'{..} = error "FIXME: Retry check not implemented."

-- | The specified time range was specified in an invalid format.
_InvalidTimeRangeException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidTimeRangeException = _ServiceError . hasCode "InvalidTimeRangeException";

-- | The specified tag was specified in an invalid format.
_InvalidTagException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidTagException = _ServiceError . hasCode "InvalidTagException";

-- | The specified on-premises instance name is already registered.
_InstanceNameAlreadyRegisteredException :: AWSError a => Geting (First ServiceError) a ServiceError
_InstanceNameAlreadyRegisteredException = _ServiceError . hasCode "InstanceNameAlreadyRegisteredException";

-- | The IAM user ARN was specified in an invalid format.
_InvalidIAMUserARNException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidIAMUserARNException = _ServiceError . hasCode "InvalidIamUserArnException";

-- | An IAM user ARN was not specified.
_IAMUserARNRequiredException :: AWSError a => Geting (First ServiceError) a ServiceError
_IAMUserARNRequiredException = _ServiceError . hasCode "IamUserArnRequiredException";

-- | The deployment group name was specified in an invalid format.
_InvalidDeploymentGroupNameException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidDeploymentGroupNameException = _ServiceError . hasCode "InvalidDeploymentGroupNameException";

-- | The description that was provided is too long.
_DescriptionTooLongException :: AWSError a => Geting (First ServiceError) a ServiceError
_DescriptionTooLongException = _ServiceError . hasCode "DescriptionTooLongException";

-- | A deployment configuration with the specified name already exists with
-- the applicable IAM user or AWS account.
_DeploymentConfigAlreadyExistsException :: AWSError a => Geting (First ServiceError) a ServiceError
_DeploymentConfigAlreadyExistsException = _ServiceError . hasCode "DeploymentConfigAlreadyExistsException";

-- | The deployment configurations limit was exceeded.
_DeploymentConfigLimitExceededException :: AWSError a => Geting (First ServiceError) a ServiceError
_DeploymentConfigLimitExceededException = _ServiceError . hasCode "DeploymentConfigLimitExceededException";

-- | The service role ARN was specified in an invalid format. Or, if an Auto
-- Scaling group was specified, the specified service role does not grant
-- the appropriate permissions to Auto Scaling.
_InvalidRoleException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidRoleException = _ServiceError . hasCode "InvalidRoleException";

-- | The specified deployment has not started.
_DeploymentNotStartedException :: AWSError a => Geting (First ServiceError) a ServiceError
_DeploymentNotStartedException = _ServiceError . hasCode "DeploymentNotStartedException";

-- | The role ID was not specified.
_RoleRequiredException :: AWSError a => Geting (First ServiceError) a ServiceError
_RoleRequiredException = _ServiceError . hasCode "RoleRequiredException";

-- | The specified IAM user ARN is already registered with an on-premises
-- instance.
_IAMUserARNAlreadyRegisteredException :: AWSError a => Geting (First ServiceError) a ServiceError
_IAMUserARNAlreadyRegisteredException = _ServiceError . hasCode "IamUserArnAlreadyRegisteredException";

-- | The number of allowed deployments was exceeded.
_DeploymentLimitExceededException :: AWSError a => Geting (First ServiceError) a ServiceError
_DeploymentLimitExceededException = _ServiceError . hasCode "DeploymentLimitExceededException";

-- | The maximum number of allowed on-premises instances in a single call was
-- exceeded.
_InstanceLimitExceededException :: AWSError a => Geting (First ServiceError) a ServiceError
_InstanceLimitExceededException = _ServiceError . hasCode "InstanceLimitExceededException";

-- | The Auto Scaling group was specified in an invalid format or does not
-- exist.
_InvalidAutoScalingGroupException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidAutoScalingGroupException = _ServiceError . hasCode "InvalidAutoScalingGroupException";

-- | The application name was specified in an invalid format.
_InvalidApplicationNameException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidApplicationNameException = _ServiceError . hasCode "InvalidApplicationNameException";

-- | The deployed state filter was specified in an invalid format.
_InvalidDeployedStateFilterException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidDeployedStateFilterException = _ServiceError . hasCode "InvalidDeployedStateFilterException";

-- | The minimum healthy instances value was specified in an invalid format.
_InvalidMinimumHealthyHostValueException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidMinimumHealthyHostValueException = _ServiceError . hasCode "InvalidMinimumHealthyHostValueException";

-- | The application does not exist with the applicable IAM user or AWS
-- account.
_ApplicationDoesNotExistException :: AWSError a => Geting (First ServiceError) a ServiceError
_ApplicationDoesNotExistException = _ServiceError . hasCode "ApplicationDoesNotExistException";

-- | The specified tag filter was specified in an invalid format.
_InvalidTagFilterException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidTagFilterException = _ServiceError . hasCode "InvalidTagFilterException";

-- | A tag was not specified.
_TagRequiredException :: AWSError a => Geting (First ServiceError) a ServiceError
_TagRequiredException = _ServiceError . hasCode "TagRequiredException";

-- | The named revision does not exist with the applicable IAM user or AWS
-- account.
_RevisionDoesNotExistException :: AWSError a => Geting (First ServiceError) a ServiceError
_RevisionDoesNotExistException = _ServiceError . hasCode "RevisionDoesNotExistException";

-- | The deployment group name was not specified.
_DeploymentGroupNameRequiredException :: AWSError a => Geting (First ServiceError) a ServiceError
_DeploymentGroupNameRequiredException = _ServiceError . hasCode "DeploymentGroupNameRequiredException";

-- | The bucket name either doesn\'t exist or was specified in an invalid
-- format.
_InvalidBucketNameFilterException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidBucketNameFilterException = _ServiceError . hasCode "InvalidBucketNameFilterException";

-- | The deployment configuration does not exist with the applicable IAM user
-- or AWS account.
_DeploymentConfigDoesNotExistException :: AWSError a => Geting (First ServiceError) a ServiceError
_DeploymentConfigDoesNotExistException = _ServiceError . hasCode "DeploymentConfigDoesNotExistException";

-- | The column name to sort by is either not present or was specified in an
-- invalid format.
_InvalidSortByException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidSortByException = _ServiceError . hasCode "InvalidSortByException";

-- | A bucket name is required but was not provided.
_BucketNameFilterRequiredException :: AWSError a => Geting (First ServiceError) a ServiceError
_BucketNameFilterRequiredException = _ServiceError . hasCode "BucketNameFilterRequiredException";

-- | The deployment groups limit was exceeded.
_DeploymentGroupLimitExceededException :: AWSError a => Geting (First ServiceError) a ServiceError
_DeploymentGroupLimitExceededException = _ServiceError . hasCode "DeploymentGroupLimitExceededException";

-- | A deployment group with the specified name already exists with the
-- applicable IAM user or AWS account.
_DeploymentGroupAlreadyExistsException :: AWSError a => Geting (First ServiceError) a ServiceError
_DeploymentGroupAlreadyExistsException = _ServiceError . hasCode "DeploymentGroupAlreadyExistsException";

-- | At least one of the deployment IDs was specified in an invalid format.
_InvalidDeploymentIdException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidDeploymentIdException = _ServiceError . hasCode "InvalidDeploymentIdException";

-- | The named deployment group does not exist with the applicable IAM user
-- or AWS account.
_DeploymentGroupDoesNotExistException :: AWSError a => Geting (First ServiceError) a ServiceError
_DeploymentGroupDoesNotExistException = _ServiceError . hasCode "DeploymentGroupDoesNotExistException";

-- | At least one deployment ID must be specified.
_DeploymentIdRequiredException :: AWSError a => Geting (First ServiceError) a ServiceError
_DeploymentIdRequiredException = _ServiceError . hasCode "DeploymentIdRequiredException";

-- | The instance ID was not specified.
_InstanceIdRequiredException :: AWSError a => Geting (First ServiceError) a ServiceError
_InstanceIdRequiredException = _ServiceError . hasCode "InstanceIdRequiredException";

-- | The deployment configuration name was not specified.
_DeploymentConfigNameRequiredException :: AWSError a => Geting (First ServiceError) a ServiceError
_DeploymentConfigNameRequiredException = _ServiceError . hasCode "DeploymentConfigNameRequiredException";

-- | The deployment configuration name was specified in an invalid format.
_InvalidDeploymentConfigNameException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidDeploymentConfigNameException = _ServiceError . hasCode "InvalidDeploymentConfigNameException";

-- | The sort order was specified in an invalid format.
_InvalidSortOrderException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidSortOrderException = _ServiceError . hasCode "InvalidSortOrderException";

-- | The next token was specified in an invalid format.
_InvalidNextTokenException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidNextTokenException = _ServiceError . hasCode "InvalidNextTokenException";

-- | The revision was specified in an invalid format.
_InvalidRevisionException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidRevisionException = _ServiceError . hasCode "InvalidRevisionException";

-- | The deployment is already completed.
_DeploymentAlreadyCompletedException :: AWSError a => Geting (First ServiceError) a ServiceError
_DeploymentAlreadyCompletedException = _ServiceError . hasCode "DeploymentAlreadyCompletedException";

-- | The revision ID was not specified.
_RevisionRequiredException :: AWSError a => Geting (First ServiceError) a ServiceError
_RevisionRequiredException = _ServiceError . hasCode "RevisionRequiredException";

-- | The specified instance does not exist in the deployment group.
_InstanceDoesNotExistException :: AWSError a => Geting (First ServiceError) a ServiceError
_InstanceDoesNotExistException = _ServiceError . hasCode "InstanceDoesNotExistException";

-- | The deployment does not exist with the applicable IAM user or AWS
-- account.
_DeploymentDoesNotExistException :: AWSError a => Geting (First ServiceError) a ServiceError
_DeploymentDoesNotExistException = _ServiceError . hasCode "DeploymentDoesNotExistException";

-- | An on-premises instance name was not specified.
_InstanceNameRequiredException :: AWSError a => Geting (First ServiceError) a ServiceError
_InstanceNameRequiredException = _ServiceError . hasCode "InstanceNameRequiredException";

-- | The deployment configuration is still in use.
_DeploymentConfigInUseException :: AWSError a => Geting (First ServiceError) a ServiceError
_DeploymentConfigInUseException = _ServiceError . hasCode "DeploymentConfigInUseException";

-- | The tag was specified in an invalid format.
_InvalidEC2TagException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidEC2TagException = _ServiceError . hasCode "InvalidEC2TagException";

-- | The specified on-premises instance name was specified in an invalid
-- format.
_InvalidInstanceNameException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidInstanceNameException = _ServiceError . hasCode "InvalidInstanceNameException";

-- | The specified deployment status doesn\'t exist or cannot be determined.
_InvalidDeploymentStatusException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidDeploymentStatusException = _ServiceError . hasCode "InvalidDeploymentStatusException";

-- | The registration status was specified in an invalid format.
_InvalidRegistrationStatusException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidRegistrationStatusException = _ServiceError . hasCode "InvalidRegistrationStatusException";

-- | The maximum allowed number of tags was exceeded.
_TagLimitExceededException :: AWSError a => Geting (First ServiceError) a ServiceError
_TagLimitExceededException = _ServiceError . hasCode "TagLimitExceededException";

-- | The specified on-premises instance is not registered.
_InstanceNotRegisteredException :: AWSError a => Geting (First ServiceError) a ServiceError
_InstanceNotRegisteredException = _ServiceError . hasCode "InstanceNotRegisteredException";

-- | More applications were attempted to be created than were allowed.
_ApplicationLimitExceededException :: AWSError a => Geting (First ServiceError) a ServiceError
_ApplicationLimitExceededException = _ServiceError . hasCode "ApplicationLimitExceededException";

-- | An invalid operation was detected.
_InvalidOperationException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidOperationException = _ServiceError . hasCode "InvalidOperationException";

-- | An application with the specified name already exists with the
-- applicable IAM user or AWS account.
_ApplicationAlreadyExistsException :: AWSError a => Geting (First ServiceError) a ServiceError
_ApplicationAlreadyExistsException = _ServiceError . hasCode "ApplicationAlreadyExistsException";

-- | The specified instance status does not exist.
_InvalidInstanceStatusException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidInstanceStatusException = _ServiceError . hasCode "InvalidInstanceStatusException";

-- | The minimum number of required application names was not specified.
_ApplicationNameRequiredException :: AWSError a => Geting (First ServiceError) a ServiceError
_ApplicationNameRequiredException = _ServiceError . hasCode "ApplicationNameRequiredException";

-- | The specified key prefix filter was specified in an invalid format.
_InvalidKeyPrefixFilterException :: AWSError a => Geting (First ServiceError) a ServiceError
_InvalidKeyPrefixFilterException = _ServiceError . hasCode "InvalidKeyPrefixFilterException";

data ApplicationRevisionSortBy = RegisterTime | FirstUsedTime | LastUsedTime deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ApplicationRevisionSortBy where
    parser = takeLowerText >>= \case
        "firstUsedTime" -> pure FirstUsedTime
        "lastUsedTime" -> pure LastUsedTime
        "registerTime" -> pure RegisterTime
        e -> fail ("Failure parsing ApplicationRevisionSortBy from " ++ show e)

instance ToText ApplicationRevisionSortBy where
    toText = \case
        FirstUsedTime -> "firstUsedTime"
        LastUsedTime -> "lastUsedTime"
        RegisterTime -> "registerTime"

instance Hashable ApplicationRevisionSortBy
instance ToQuery ApplicationRevisionSortBy
instance ToHeader ApplicationRevisionSortBy

instance ToJSON ApplicationRevisionSortBy where
    toJSON = toJSONText

data BundleType = Zip | TGZ | TAR deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText BundleType where
    parser = takeLowerText >>= \case
        "tar" -> pure TAR
        "tgz" -> pure TGZ
        "zip" -> pure Zip
        e -> fail ("Failure parsing BundleType from " ++ show e)

instance ToText BundleType where
    toText = \case
        TAR -> "tar"
        TGZ -> "tgz"
        Zip -> "zip"

instance Hashable BundleType
instance ToQuery BundleType
instance ToHeader BundleType

instance ToJSON BundleType where
    toJSON = toJSONText

instance FromJSON BundleType where
    parseJSON = parseJSONText "BundleType"

data DeployErrorCode = Throttled | HealthConstraints | OverMaxInstances | HealthConstraintsInvalid | NOInstances | ApplicationMissing | RevisionMissing | InternalError | DeploymentGroupMissing | IAMRoleMissing | Timeout | NOEC2Subscription | IAMRolePermissions deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText DeployErrorCode where
    parser = takeLowerText >>= \case
        "APPLICATION_MISSING" -> pure ApplicationMissing
        "DEPLOYMENT_GROUP_MISSING" -> pure DeploymentGroupMissing
        "HEALTH_CONSTRAINTS" -> pure HealthConstraints
        "HEALTH_CONSTRAINTS_INVALID" -> pure HealthConstraintsInvalid
        "IAM_ROLE_MISSING" -> pure IAMRoleMissing
        "IAM_ROLE_PERMISSIONS" -> pure IAMRolePermissions
        "INTERNAL_ERROR" -> pure InternalError
        "NO_EC2_SUBSCRIPTION" -> pure NOEC2Subscription
        "NO_INSTANCES" -> pure NOInstances
        "OVER_MAX_INSTANCES" -> pure OverMaxInstances
        "REVISION_MISSING" -> pure RevisionMissing
        "THROTTLED" -> pure Throttled
        "TIMEOUT" -> pure Timeout
        e -> fail ("Failure parsing DeployErrorCode from " ++ show e)

instance ToText DeployErrorCode where
    toText = \case
        ApplicationMissing -> "APPLICATION_MISSING"
        DeploymentGroupMissing -> "DEPLOYMENT_GROUP_MISSING"
        HealthConstraints -> "HEALTH_CONSTRAINTS"
        HealthConstraintsInvalid -> "HEALTH_CONSTRAINTS_INVALID"
        IAMRoleMissing -> "IAM_ROLE_MISSING"
        IAMRolePermissions -> "IAM_ROLE_PERMISSIONS"
        InternalError -> "INTERNAL_ERROR"
        NOEC2Subscription -> "NO_EC2_SUBSCRIPTION"
        NOInstances -> "NO_INSTANCES"
        OverMaxInstances -> "OVER_MAX_INSTANCES"
        RevisionMissing -> "REVISION_MISSING"
        Throttled -> "THROTTLED"
        Timeout -> "TIMEOUT"

instance Hashable DeployErrorCode
instance ToQuery DeployErrorCode
instance ToHeader DeployErrorCode

instance FromJSON DeployErrorCode where
    parseJSON = parseJSONText "DeployErrorCode"

data DeploymentCreator = Autoscaling | User deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText DeploymentCreator where
    parser = takeLowerText >>= \case
        "autoscaling" -> pure Autoscaling
        "user" -> pure User
        e -> fail ("Failure parsing DeploymentCreator from " ++ show e)

instance ToText DeploymentCreator where
    toText = \case
        Autoscaling -> "autoscaling"
        User -> "user"

instance Hashable DeploymentCreator
instance ToQuery DeploymentCreator
instance ToHeader DeploymentCreator

instance FromJSON DeploymentCreator where
    parseJSON = parseJSONText "DeploymentCreator"

data DeploymentStatus = Queued | Created | Stopped | InProgress | Succeeded | Failed deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText DeploymentStatus where
    parser = takeLowerText >>= \case
        "Created" -> pure Created
        "Failed" -> pure Failed
        "InProgress" -> pure InProgress
        "Queued" -> pure Queued
        "Stopped" -> pure Stopped
        "Succeeded" -> pure Succeeded
        e -> fail ("Failure parsing DeploymentStatus from " ++ show e)

instance ToText DeploymentStatus where
    toText = \case
        Created -> "Created"
        Failed -> "Failed"
        InProgress -> "InProgress"
        Queued -> "Queued"
        Stopped -> "Stopped"
        Succeeded -> "Succeeded"

instance Hashable DeploymentStatus
instance ToQuery DeploymentStatus
instance ToHeader DeploymentStatus

instance ToJSON DeploymentStatus where
    toJSON = toJSONText

instance FromJSON DeploymentStatus where
    parseJSON = parseJSONText "DeploymentStatus"

data EC2TagFilterType = KeyAndValue | ValueOnly | KeyOnly deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText EC2TagFilterType where
    parser = takeLowerText >>= \case
        "KEY_AND_VALUE" -> pure KeyAndValue
        "KEY_ONLY" -> pure KeyOnly
        "VALUE_ONLY" -> pure ValueOnly
        e -> fail ("Failure parsing EC2TagFilterType from " ++ show e)

instance ToText EC2TagFilterType where
    toText = \case
        KeyAndValue -> "KEY_AND_VALUE"
        KeyOnly -> "KEY_ONLY"
        ValueOnly -> "VALUE_ONLY"

instance Hashable EC2TagFilterType
instance ToQuery EC2TagFilterType
instance ToHeader EC2TagFilterType

instance ToJSON EC2TagFilterType where
    toJSON = toJSONText

instance FromJSON EC2TagFilterType where
    parseJSON = parseJSONText "EC2TagFilterType"

data InstanceStatus = ISInProgress | ISFailed | ISSucceeded | ISUnknown | ISSkipped | ISPending deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText InstanceStatus where
    parser = takeLowerText >>= \case
        "Failed" -> pure ISFailed
        "InProgress" -> pure ISInProgress
        "Pending" -> pure ISPending
        "Skipped" -> pure ISSkipped
        "Succeeded" -> pure ISSucceeded
        "Unknown" -> pure ISUnknown
        e -> fail ("Failure parsing InstanceStatus from " ++ show e)

instance ToText InstanceStatus where
    toText = \case
        ISFailed -> "Failed"
        ISInProgress -> "InProgress"
        ISPending -> "Pending"
        ISSkipped -> "Skipped"
        ISSucceeded -> "Succeeded"
        ISUnknown -> "Unknown"

instance Hashable InstanceStatus
instance ToQuery InstanceStatus
instance ToHeader InstanceStatus

instance ToJSON InstanceStatus where
    toJSON = toJSONText

instance FromJSON InstanceStatus where
    parseJSON = parseJSONText "InstanceStatus"

data LifecycleErrorCode = UnknownError | ScriptMissing | Success | ScriptFailed | ScriptNotExecutable | ScriptTimedOut deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText LifecycleErrorCode where
    parser = takeLowerText >>= \case
        "ScriptFailed" -> pure ScriptFailed
        "ScriptMissing" -> pure ScriptMissing
        "ScriptNotExecutable" -> pure ScriptNotExecutable
        "ScriptTimedOut" -> pure ScriptTimedOut
        "Success" -> pure Success
        "UnknownError" -> pure UnknownError
        e -> fail ("Failure parsing LifecycleErrorCode from " ++ show e)

instance ToText LifecycleErrorCode where
    toText = \case
        ScriptFailed -> "ScriptFailed"
        ScriptMissing -> "ScriptMissing"
        ScriptNotExecutable -> "ScriptNotExecutable"
        ScriptTimedOut -> "ScriptTimedOut"
        Success -> "Success"
        UnknownError -> "UnknownError"

instance Hashable LifecycleErrorCode
instance ToQuery LifecycleErrorCode
instance ToHeader LifecycleErrorCode

instance FromJSON LifecycleErrorCode where
    parseJSON = parseJSONText "LifecycleErrorCode"

data LifecycleEventStatus = LESInProgress | LESFailed | LESSucceeded | LESSkipped | LESUnknown | LESPending deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText LifecycleEventStatus where
    parser = takeLowerText >>= \case
        "Failed" -> pure LESFailed
        "InProgress" -> pure LESInProgress
        "Pending" -> pure LESPending
        "Skipped" -> pure LESSkipped
        "Succeeded" -> pure LESSucceeded
        "Unknown" -> pure LESUnknown
        e -> fail ("Failure parsing LifecycleEventStatus from " ++ show e)

instance ToText LifecycleEventStatus where
    toText = \case
        LESFailed -> "Failed"
        LESInProgress -> "InProgress"
        LESPending -> "Pending"
        LESSkipped -> "Skipped"
        LESSucceeded -> "Succeeded"
        LESUnknown -> "Unknown"

instance Hashable LifecycleEventStatus
instance ToQuery LifecycleEventStatus
instance ToHeader LifecycleEventStatus

instance FromJSON LifecycleEventStatus where
    parseJSON = parseJSONText "LifecycleEventStatus"

data ListStateFilterAction = Include | Ignore | Exclude deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ListStateFilterAction where
    parser = takeLowerText >>= \case
        "exclude" -> pure Exclude
        "ignore" -> pure Ignore
        "include" -> pure Include
        e -> fail ("Failure parsing ListStateFilterAction from " ++ show e)

instance ToText ListStateFilterAction where
    toText = \case
        Exclude -> "exclude"
        Ignore -> "ignore"
        Include -> "include"

instance Hashable ListStateFilterAction
instance ToQuery ListStateFilterAction
instance ToHeader ListStateFilterAction

instance ToJSON ListStateFilterAction where
    toJSON = toJSONText

data MinimumHealthyHostsType = FleetPercent | HostCount deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText MinimumHealthyHostsType where
    parser = takeLowerText >>= \case
        "FLEET_PERCENT" -> pure FleetPercent
        "HOST_COUNT" -> pure HostCount
        e -> fail ("Failure parsing MinimumHealthyHostsType from " ++ show e)

instance ToText MinimumHealthyHostsType where
    toText = \case
        FleetPercent -> "FLEET_PERCENT"
        HostCount -> "HOST_COUNT"

instance Hashable MinimumHealthyHostsType
instance ToQuery MinimumHealthyHostsType
instance ToHeader MinimumHealthyHostsType

instance ToJSON MinimumHealthyHostsType where
    toJSON = toJSONText

instance FromJSON MinimumHealthyHostsType where
    parseJSON = parseJSONText "MinimumHealthyHostsType"

data RegistrationStatus = Registered | Deregistered deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText RegistrationStatus where
    parser = takeLowerText >>= \case
        "Deregistered" -> pure Deregistered
        "Registered" -> pure Registered
        e -> fail ("Failure parsing RegistrationStatus from " ++ show e)

instance ToText RegistrationStatus where
    toText = \case
        Deregistered -> "Deregistered"
        Registered -> "Registered"

instance Hashable RegistrationStatus
instance ToQuery RegistrationStatus
instance ToHeader RegistrationStatus

instance ToJSON RegistrationStatus where
    toJSON = toJSONText

data RevisionLocationType = GitHub | S3 deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText RevisionLocationType where
    parser = takeLowerText >>= \case
        "GitHub" -> pure GitHub
        "S3" -> pure S3
        e -> fail ("Failure parsing RevisionLocationType from " ++ show e)

instance ToText RevisionLocationType where
    toText = \case
        GitHub -> "GitHub"
        S3 -> "S3"

instance Hashable RevisionLocationType
instance ToQuery RevisionLocationType
instance ToHeader RevisionLocationType

instance ToJSON RevisionLocationType where
    toJSON = toJSONText

instance FromJSON RevisionLocationType where
    parseJSON = parseJSONText "RevisionLocationType"

data SortOrder = Ascending | Descending deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText SortOrder where
    parser = takeLowerText >>= \case
        "ascending" -> pure Ascending
        "descending" -> pure Descending
        e -> fail ("Failure parsing SortOrder from " ++ show e)

instance ToText SortOrder where
    toText = \case
        Ascending -> "ascending"
        Descending -> "descending"

instance Hashable SortOrder
instance ToQuery SortOrder
instance ToHeader SortOrder

instance ToJSON SortOrder where
    toJSON = toJSONText

data StopStatus = SSSucceeded | SSPending deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText StopStatus where
    parser = takeLowerText >>= \case
        "Pending" -> pure SSPending
        "Succeeded" -> pure SSSucceeded
        e -> fail ("Failure parsing StopStatus from " ++ show e)

instance ToText StopStatus where
    toText = \case
        SSPending -> "Pending"
        SSSucceeded -> "Succeeded"

instance Hashable StopStatus
instance ToQuery StopStatus
instance ToHeader StopStatus

instance FromJSON StopStatus where
    parseJSON = parseJSONText "StopStatus"

data TagFilterType = TFTKeyAndValue | TFTValueOnly | TFTKeyOnly deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText TagFilterType where
    parser = takeLowerText >>= \case
        "KEY_AND_VALUE" -> pure TFTKeyAndValue
        "KEY_ONLY" -> pure TFTKeyOnly
        "VALUE_ONLY" -> pure TFTValueOnly
        e -> fail ("Failure parsing TagFilterType from " ++ show e)

instance ToText TagFilterType where
    toText = \case
        TFTKeyAndValue -> "KEY_AND_VALUE"
        TFTKeyOnly -> "KEY_ONLY"
        TFTValueOnly -> "VALUE_ONLY"

instance Hashable TagFilterType
instance ToQuery TagFilterType
instance ToHeader TagFilterType

instance ToJSON TagFilterType where
    toJSON = toJSONText

instance FromJSON TagFilterType where
    parseJSON = parseJSONText "TagFilterType"

-- | Information about an application.
--
-- /See:/ 'applicationInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aiLinkedToGitHub'
--
-- * 'aiApplicationId'
--
-- * 'aiApplicationName'
--
-- * 'aiCreateTime'
data ApplicationInfo = ApplicationInfo'{_aiLinkedToGitHub :: Maybe Bool, _aiApplicationId :: Maybe Text, _aiApplicationName :: Maybe Text, _aiCreateTime :: Maybe POSIX} deriving (Eq, Read, Show)

-- | 'ApplicationInfo' smart constructor.
applicationInfo :: ApplicationInfo
applicationInfo = ApplicationInfo'{_aiLinkedToGitHub = Nothing, _aiApplicationId = Nothing, _aiApplicationName = Nothing, _aiCreateTime = Nothing};

-- | True if the user has authenticated with GitHub for the specified
-- application; otherwise, false.
aiLinkedToGitHub :: Lens' ApplicationInfo (Maybe Bool)
aiLinkedToGitHub = lens _aiLinkedToGitHub (\ s a -> s{_aiLinkedToGitHub = a});

-- | The application ID.
aiApplicationId :: Lens' ApplicationInfo (Maybe Text)
aiApplicationId = lens _aiApplicationId (\ s a -> s{_aiApplicationId = a});

-- | The application name.
aiApplicationName :: Lens' ApplicationInfo (Maybe Text)
aiApplicationName = lens _aiApplicationName (\ s a -> s{_aiApplicationName = a});

-- | The time that the application was created.
aiCreateTime :: Lens' ApplicationInfo (Maybe UTCTime)
aiCreateTime = lens _aiCreateTime (\ s a -> s{_aiCreateTime = a}) . mapping _Time;

instance FromJSON ApplicationInfo where
        parseJSON
          = withObject "ApplicationInfo"
              (\ x ->
                 ApplicationInfo' <$>
                   (x .:? "linkedToGitHub") <*> (x .:? "applicationId")
                     <*> (x .:? "applicationName")
                     <*> (x .:? "createTime"))

-- | Information about an Auto Scaling group.
--
-- /See:/ 'autoScalingGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asgHook'
--
-- * 'asgName'
data AutoScalingGroup = AutoScalingGroup'{_asgHook :: Maybe Text, _asgName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'AutoScalingGroup' smart constructor.
autoScalingGroup :: AutoScalingGroup
autoScalingGroup = AutoScalingGroup'{_asgHook = Nothing, _asgName = Nothing};

-- | An Auto Scaling lifecycle event hook name.
asgHook :: Lens' AutoScalingGroup (Maybe Text)
asgHook = lens _asgHook (\ s a -> s{_asgHook = a});

-- | The Auto Scaling group name.
asgName :: Lens' AutoScalingGroup (Maybe Text)
asgName = lens _asgName (\ s a -> s{_asgName = a});

instance FromJSON AutoScalingGroup where
        parseJSON
          = withObject "AutoScalingGroup"
              (\ x ->
                 AutoScalingGroup' <$>
                   (x .:? "hook") <*> (x .:? "name"))

-- | Information about a deployment configuration.
--
-- /See:/ 'deploymentConfigInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dciDeploymentConfigName'
--
-- * 'dciMinimumHealthyHosts'
--
-- * 'dciDeploymentConfigId'
--
-- * 'dciCreateTime'
data DeploymentConfigInfo = DeploymentConfigInfo'{_dciDeploymentConfigName :: Maybe Text, _dciMinimumHealthyHosts :: Maybe MinimumHealthyHosts, _dciDeploymentConfigId :: Maybe Text, _dciCreateTime :: Maybe POSIX} deriving (Eq, Read, Show)

-- | 'DeploymentConfigInfo' smart constructor.
deploymentConfigInfo :: DeploymentConfigInfo
deploymentConfigInfo = DeploymentConfigInfo'{_dciDeploymentConfigName = Nothing, _dciMinimumHealthyHosts = Nothing, _dciDeploymentConfigId = Nothing, _dciCreateTime = Nothing};

-- | The deployment configuration name.
dciDeploymentConfigName :: Lens' DeploymentConfigInfo (Maybe Text)
dciDeploymentConfigName = lens _dciDeploymentConfigName (\ s a -> s{_dciDeploymentConfigName = a});

-- | Information about the number or percentage of minimum healthy instances.
dciMinimumHealthyHosts :: Lens' DeploymentConfigInfo (Maybe MinimumHealthyHosts)
dciMinimumHealthyHosts = lens _dciMinimumHealthyHosts (\ s a -> s{_dciMinimumHealthyHosts = a});

-- | The deployment configuration ID.
dciDeploymentConfigId :: Lens' DeploymentConfigInfo (Maybe Text)
dciDeploymentConfigId = lens _dciDeploymentConfigId (\ s a -> s{_dciDeploymentConfigId = a});

-- | The time that the deployment configuration was created.
dciCreateTime :: Lens' DeploymentConfigInfo (Maybe UTCTime)
dciCreateTime = lens _dciCreateTime (\ s a -> s{_dciCreateTime = a}) . mapping _Time;

instance FromJSON DeploymentConfigInfo where
        parseJSON
          = withObject "DeploymentConfigInfo"
              (\ x ->
                 DeploymentConfigInfo' <$>
                   (x .:? "deploymentConfigName") <*>
                     (x .:? "minimumHealthyHosts")
                     <*> (x .:? "deploymentConfigId")
                     <*> (x .:? "createTime"))

-- | Information about a deployment group.
--
-- /See:/ 'deploymentGroupInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgiServiceRoleARN'
--
-- * 'dgiDeploymentConfigName'
--
-- * 'dgiTargetRevision'
--
-- * 'dgiEc2TagFilters'
--
-- * 'dgiOnPremisesInstanceTagFilters'
--
-- * 'dgiApplicationName'
--
-- * 'dgiDeploymentGroupId'
--
-- * 'dgiAutoScalingGroups'
--
-- * 'dgiDeploymentGroupName'
data DeploymentGroupInfo = DeploymentGroupInfo'{_dgiServiceRoleARN :: Maybe Text, _dgiDeploymentConfigName :: Maybe Text, _dgiTargetRevision :: Maybe RevisionLocation, _dgiEc2TagFilters :: Maybe [EC2TagFilter], _dgiOnPremisesInstanceTagFilters :: Maybe [TagFilter], _dgiApplicationName :: Maybe Text, _dgiDeploymentGroupId :: Maybe Text, _dgiAutoScalingGroups :: Maybe [AutoScalingGroup], _dgiDeploymentGroupName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DeploymentGroupInfo' smart constructor.
deploymentGroupInfo :: DeploymentGroupInfo
deploymentGroupInfo = DeploymentGroupInfo'{_dgiServiceRoleARN = Nothing, _dgiDeploymentConfigName = Nothing, _dgiTargetRevision = Nothing, _dgiEc2TagFilters = Nothing, _dgiOnPremisesInstanceTagFilters = Nothing, _dgiApplicationName = Nothing, _dgiDeploymentGroupId = Nothing, _dgiAutoScalingGroups = Nothing, _dgiDeploymentGroupName = Nothing};

-- | A service role ARN.
dgiServiceRoleARN :: Lens' DeploymentGroupInfo (Maybe Text)
dgiServiceRoleARN = lens _dgiServiceRoleARN (\ s a -> s{_dgiServiceRoleARN = a});

-- | The deployment configuration name.
dgiDeploymentConfigName :: Lens' DeploymentGroupInfo (Maybe Text)
dgiDeploymentConfigName = lens _dgiDeploymentConfigName (\ s a -> s{_dgiDeploymentConfigName = a});

-- | Information about the deployment group\'s target revision, including the
-- revision\'s type and its location.
dgiTargetRevision :: Lens' DeploymentGroupInfo (Maybe RevisionLocation)
dgiTargetRevision = lens _dgiTargetRevision (\ s a -> s{_dgiTargetRevision = a});

-- | The Amazon EC2 tags to filter on.
dgiEc2TagFilters :: Lens' DeploymentGroupInfo [EC2TagFilter]
dgiEc2TagFilters = lens _dgiEc2TagFilters (\ s a -> s{_dgiEc2TagFilters = a}) . _Default;

-- | The on-premises instance tags to filter on.
dgiOnPremisesInstanceTagFilters :: Lens' DeploymentGroupInfo [TagFilter]
dgiOnPremisesInstanceTagFilters = lens _dgiOnPremisesInstanceTagFilters (\ s a -> s{_dgiOnPremisesInstanceTagFilters = a}) . _Default;

-- | The application name.
dgiApplicationName :: Lens' DeploymentGroupInfo (Maybe Text)
dgiApplicationName = lens _dgiApplicationName (\ s a -> s{_dgiApplicationName = a});

-- | The deployment group ID.
dgiDeploymentGroupId :: Lens' DeploymentGroupInfo (Maybe Text)
dgiDeploymentGroupId = lens _dgiDeploymentGroupId (\ s a -> s{_dgiDeploymentGroupId = a});

-- | A list of associated Auto Scaling groups.
dgiAutoScalingGroups :: Lens' DeploymentGroupInfo [AutoScalingGroup]
dgiAutoScalingGroups = lens _dgiAutoScalingGroups (\ s a -> s{_dgiAutoScalingGroups = a}) . _Default;

-- | The deployment group name.
dgiDeploymentGroupName :: Lens' DeploymentGroupInfo (Maybe Text)
dgiDeploymentGroupName = lens _dgiDeploymentGroupName (\ s a -> s{_dgiDeploymentGroupName = a});

instance FromJSON DeploymentGroupInfo where
        parseJSON
          = withObject "DeploymentGroupInfo"
              (\ x ->
                 DeploymentGroupInfo' <$>
                   (x .:? "serviceRoleArn") <*>
                     (x .:? "deploymentConfigName")
                     <*> (x .:? "targetRevision")
                     <*> (x .:? "ec2TagFilters" .!= mempty)
                     <*> (x .:? "onPremisesInstanceTagFilters" .!= mempty)
                     <*> (x .:? "applicationName")
                     <*> (x .:? "deploymentGroupId")
                     <*> (x .:? "autoScalingGroups" .!= mempty)
                     <*> (x .:? "deploymentGroupName"))

-- | Information about a deployment.
--
-- /See:/ 'deploymentInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diDeploymentId'
--
-- * 'diCreator'
--
-- * 'diStatus'
--
-- * 'diDeploymentConfigName'
--
-- * 'diStartTime'
--
-- * 'diCompleteTime'
--
-- * 'diErrorInformation'
--
-- * 'diDeploymentOverview'
--
-- * 'diApplicationName'
--
-- * 'diRevision'
--
-- * 'diDescription'
--
-- * 'diIgnoreApplicationStopFailures'
--
-- * 'diDeploymentGroupName'
--
-- * 'diCreateTime'
data DeploymentInfo = DeploymentInfo'{_diDeploymentId :: Maybe Text, _diCreator :: Maybe DeploymentCreator, _diStatus :: Maybe DeploymentStatus, _diDeploymentConfigName :: Maybe Text, _diStartTime :: Maybe POSIX, _diCompleteTime :: Maybe POSIX, _diErrorInformation :: Maybe ErrorInformation, _diDeploymentOverview :: Maybe DeploymentOverview, _diApplicationName :: Maybe Text, _diRevision :: Maybe RevisionLocation, _diDescription :: Maybe Text, _diIgnoreApplicationStopFailures :: Maybe Bool, _diDeploymentGroupName :: Maybe Text, _diCreateTime :: Maybe POSIX} deriving (Eq, Read, Show)

-- | 'DeploymentInfo' smart constructor.
deploymentInfo :: DeploymentInfo
deploymentInfo = DeploymentInfo'{_diDeploymentId = Nothing, _diCreator = Nothing, _diStatus = Nothing, _diDeploymentConfigName = Nothing, _diStartTime = Nothing, _diCompleteTime = Nothing, _diErrorInformation = Nothing, _diDeploymentOverview = Nothing, _diApplicationName = Nothing, _diRevision = Nothing, _diDescription = Nothing, _diIgnoreApplicationStopFailures = Nothing, _diDeploymentGroupName = Nothing, _diCreateTime = Nothing};

-- | The deployment ID.
diDeploymentId :: Lens' DeploymentInfo (Maybe Text)
diDeploymentId = lens _diDeploymentId (\ s a -> s{_diDeploymentId = a});

-- | How the deployment was created:
--
-- -   user: A user created the deployment.
-- -   autoscaling: Auto Scaling created the deployment.
diCreator :: Lens' DeploymentInfo (Maybe DeploymentCreator)
diCreator = lens _diCreator (\ s a -> s{_diCreator = a});

-- | The current state of the deployment as a whole.
diStatus :: Lens' DeploymentInfo (Maybe DeploymentStatus)
diStatus = lens _diStatus (\ s a -> s{_diStatus = a});

-- | The deployment configuration name.
diDeploymentConfigName :: Lens' DeploymentInfo (Maybe Text)
diDeploymentConfigName = lens _diDeploymentConfigName (\ s a -> s{_diDeploymentConfigName = a});

-- | A timestamp indicating when the deployment began deploying to the
-- deployment group.
--
-- Note that in some cases, the reported value of the start time may be
-- later than the complete time. This is due to differences in the clock
-- settings of various back-end servers that participate in the overall
-- deployment process.
diStartTime :: Lens' DeploymentInfo (Maybe UTCTime)
diStartTime = lens _diStartTime (\ s a -> s{_diStartTime = a}) . mapping _Time;

-- | A timestamp indicating when the deployment was completed.
diCompleteTime :: Lens' DeploymentInfo (Maybe UTCTime)
diCompleteTime = lens _diCompleteTime (\ s a -> s{_diCompleteTime = a}) . mapping _Time;

-- | Information about any error associated with this deployment.
diErrorInformation :: Lens' DeploymentInfo (Maybe ErrorInformation)
diErrorInformation = lens _diErrorInformation (\ s a -> s{_diErrorInformation = a});

-- | A summary of the deployment status of the instances in the deployment.
diDeploymentOverview :: Lens' DeploymentInfo (Maybe DeploymentOverview)
diDeploymentOverview = lens _diDeploymentOverview (\ s a -> s{_diDeploymentOverview = a});

-- | The application name.
diApplicationName :: Lens' DeploymentInfo (Maybe Text)
diApplicationName = lens _diApplicationName (\ s a -> s{_diApplicationName = a});

-- | Information about the location of application artifacts that are stored
-- and the service to retrieve them from.
diRevision :: Lens' DeploymentInfo (Maybe RevisionLocation)
diRevision = lens _diRevision (\ s a -> s{_diRevision = a});

-- | A comment about the deployment.
diDescription :: Lens' DeploymentInfo (Maybe Text)
diDescription = lens _diDescription (\ s a -> s{_diDescription = a});

-- | If true, then if the deployment causes the ApplicationStop deployment
-- lifecycle event to fail to a specific instance, the deployment will not
-- be considered to have failed to that instance at that point and will
-- continue on to the BeforeInstall deployment lifecycle event.
--
-- If false or not specified, then if the deployment causes the
-- ApplicationStop deployment lifecycle event to fail to a specific
-- instance, the deployment will stop to that instance, and the deployment
-- to that instance will be considered to have failed.
diIgnoreApplicationStopFailures :: Lens' DeploymentInfo (Maybe Bool)
diIgnoreApplicationStopFailures = lens _diIgnoreApplicationStopFailures (\ s a -> s{_diIgnoreApplicationStopFailures = a});

-- | The deployment group name.
diDeploymentGroupName :: Lens' DeploymentInfo (Maybe Text)
diDeploymentGroupName = lens _diDeploymentGroupName (\ s a -> s{_diDeploymentGroupName = a});

-- | A timestamp indicating when the deployment was created.
diCreateTime :: Lens' DeploymentInfo (Maybe UTCTime)
diCreateTime = lens _diCreateTime (\ s a -> s{_diCreateTime = a}) . mapping _Time;

instance FromJSON DeploymentInfo where
        parseJSON
          = withObject "DeploymentInfo"
              (\ x ->
                 DeploymentInfo' <$>
                   (x .:? "deploymentId") <*> (x .:? "creator") <*>
                     (x .:? "status")
                     <*> (x .:? "deploymentConfigName")
                     <*> (x .:? "startTime")
                     <*> (x .:? "completeTime")
                     <*> (x .:? "errorInformation")
                     <*> (x .:? "deploymentOverview")
                     <*> (x .:? "applicationName")
                     <*> (x .:? "revision")
                     <*> (x .:? "description")
                     <*> (x .:? "ignoreApplicationStopFailures")
                     <*> (x .:? "deploymentGroupName")
                     <*> (x .:? "createTime"))

-- | Information about the deployment status of the instances in the
-- deployment.
--
-- /See:/ 'deploymentOverview' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'doPending'
--
-- * 'doSkipped'
--
-- * 'doInProgress'
--
-- * 'doSucceeded'
--
-- * 'doFailed'
data DeploymentOverview = DeploymentOverview'{_doPending :: Maybe Integer, _doSkipped :: Maybe Integer, _doInProgress :: Maybe Integer, _doSucceeded :: Maybe Integer, _doFailed :: Maybe Integer} deriving (Eq, Read, Show)

-- | 'DeploymentOverview' smart constructor.
deploymentOverview :: DeploymentOverview
deploymentOverview = DeploymentOverview'{_doPending = Nothing, _doSkipped = Nothing, _doInProgress = Nothing, _doSucceeded = Nothing, _doFailed = Nothing};

-- | The number of instances that are pending in the deployment.
doPending :: Lens' DeploymentOverview (Maybe Integer)
doPending = lens _doPending (\ s a -> s{_doPending = a});

-- | The number of instances that have been skipped in the deployment.
doSkipped :: Lens' DeploymentOverview (Maybe Integer)
doSkipped = lens _doSkipped (\ s a -> s{_doSkipped = a});

-- | The number of instances that are in progress in the deployment.
doInProgress :: Lens' DeploymentOverview (Maybe Integer)
doInProgress = lens _doInProgress (\ s a -> s{_doInProgress = a});

-- | The number of instances that have succeeded in the deployment.
doSucceeded :: Lens' DeploymentOverview (Maybe Integer)
doSucceeded = lens _doSucceeded (\ s a -> s{_doSucceeded = a});

-- | The number of instances that have failed in the deployment.
doFailed :: Lens' DeploymentOverview (Maybe Integer)
doFailed = lens _doFailed (\ s a -> s{_doFailed = a});

instance FromJSON DeploymentOverview where
        parseJSON
          = withObject "DeploymentOverview"
              (\ x ->
                 DeploymentOverview' <$>
                   (x .:? "Pending") <*> (x .:? "Skipped") <*>
                     (x .:? "InProgress")
                     <*> (x .:? "Succeeded")
                     <*> (x .:? "Failed"))

-- | Diagnostic information about executable scripts that are part of a
-- deployment.
--
-- /See:/ 'diagnostics' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diaLogTail'
--
-- * 'diaErrorCode'
--
-- * 'diaScriptName'
--
-- * 'diaMessage'
data Diagnostics = Diagnostics'{_diaLogTail :: Maybe Text, _diaErrorCode :: Maybe LifecycleErrorCode, _diaScriptName :: Maybe Text, _diaMessage :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Diagnostics' smart constructor.
diagnostics :: Diagnostics
diagnostics = Diagnostics'{_diaLogTail = Nothing, _diaErrorCode = Nothing, _diaScriptName = Nothing, _diaMessage = Nothing};

-- | The last portion of the associated diagnostic log.
diaLogTail :: Lens' Diagnostics (Maybe Text)
diaLogTail = lens _diaLogTail (\ s a -> s{_diaLogTail = a});

-- | The associated error code:
--
-- -   Success: The specified script ran.
-- -   ScriptMissing: The specified script was not found in the specified
--     location.
-- -   ScriptNotExecutable: The specified script is not a recognized
--     executable file type.
-- -   ScriptTimedOut: The specified script did not finish running in the
--     specified time period.
-- -   ScriptFailed: The specified script failed to run as expected.
-- -   UnknownError: The specified script did not run for an unknown
--     reason.
diaErrorCode :: Lens' Diagnostics (Maybe LifecycleErrorCode)
diaErrorCode = lens _diaErrorCode (\ s a -> s{_diaErrorCode = a});

-- | The name of the script.
diaScriptName :: Lens' Diagnostics (Maybe Text)
diaScriptName = lens _diaScriptName (\ s a -> s{_diaScriptName = a});

-- | The message associated with the error.
diaMessage :: Lens' Diagnostics (Maybe Text)
diaMessage = lens _diaMessage (\ s a -> s{_diaMessage = a});

instance FromJSON Diagnostics where
        parseJSON
          = withObject "Diagnostics"
              (\ x ->
                 Diagnostics' <$>
                   (x .:? "logTail") <*> (x .:? "errorCode") <*>
                     (x .:? "scriptName")
                     <*> (x .:? "message"))

-- | Information about a tag filter.
--
-- /See:/ 'ec2TagFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'etfValue'
--
-- * 'etfKey'
--
-- * 'etfType'
data EC2TagFilter = EC2TagFilter'{_etfValue :: Maybe Text, _etfKey :: Maybe Text, _etfType :: Maybe EC2TagFilterType} deriving (Eq, Read, Show)

-- | 'EC2TagFilter' smart constructor.
ec2TagFilter :: EC2TagFilter
ec2TagFilter = EC2TagFilter'{_etfValue = Nothing, _etfKey = Nothing, _etfType = Nothing};

-- | The tag filter value.
etfValue :: Lens' EC2TagFilter (Maybe Text)
etfValue = lens _etfValue (\ s a -> s{_etfValue = a});

-- | The tag filter key.
etfKey :: Lens' EC2TagFilter (Maybe Text)
etfKey = lens _etfKey (\ s a -> s{_etfKey = a});

-- | The tag filter type:
--
-- -   KEY_ONLY: Key only.
-- -   VALUE_ONLY: Value only.
-- -   KEY_AND_VALUE: Key and value.
etfType :: Lens' EC2TagFilter (Maybe EC2TagFilterType)
etfType = lens _etfType (\ s a -> s{_etfType = a});

instance FromJSON EC2TagFilter where
        parseJSON
          = withObject "EC2TagFilter"
              (\ x ->
                 EC2TagFilter' <$>
                   (x .:? "Value") <*> (x .:? "Key") <*> (x .:? "Type"))

instance ToJSON EC2TagFilter where
        toJSON EC2TagFilter'{..}
          = object
              ["Value" .= _etfValue, "Key" .= _etfKey,
               "Type" .= _etfType]

-- | Information about a deployment error.
--
-- /See:/ 'errorInformation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eiCode'
--
-- * 'eiMessage'
data ErrorInformation = ErrorInformation'{_eiCode :: Maybe DeployErrorCode, _eiMessage :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ErrorInformation' smart constructor.
errorInformation :: ErrorInformation
errorInformation = ErrorInformation'{_eiCode = Nothing, _eiMessage = Nothing};

-- | The error code:
--
-- -   APPLICATION_MISSING: The application was missing. Note that this
--     error code will most likely be raised if the application is deleted
--     after the deployment is created but before it starts.
-- -   DEPLOYMENT_GROUP_MISSING: The deployment group was missing. Note
--     that this error code will most likely be raised if the deployment
--     group is deleted after the deployment is created but before it
--     starts.
-- -   HEALTH_CONSTRAINTS: The deployment failed on too many instances to
--     be able to successfully deploy within the specified instance health
--     constraints.
-- -   HEALTH_CONSTRAINTS_INVALID: The revision can never successfully
--     deploy within the instance health constraints as specified.
-- -   IAM_ROLE_MISSING: The service role cannot be accessed.
-- -   IAM_ROLE_PERMISSIONS: The service role does not have the correct
--     permissions.
-- -   INTERNAL_ERROR: There was an internal error.
-- -   NO_EC2_SUBSCRIPTION: The calling account is not subscribed to the
--     Amazon EC2 service.
-- -   NO_INSTANCES: No instances were specified, or no instances can be
--     found.
-- -   OVER_MAX_INSTANCES: The maximum number of instances was exceeded.
-- -   THROTTLED: The operation was throttled because the calling account
--     exceeded the throttling limits of one or more AWS services.
-- -   TIMEOUT: The deployment has timed out.
-- -   REVISION_MISSING: The revision ID was missing. Note that this error
--     code will most likely be raised if the revision is deleted after the
--     deployment is created but before it starts.
eiCode :: Lens' ErrorInformation (Maybe DeployErrorCode)
eiCode = lens _eiCode (\ s a -> s{_eiCode = a});

-- | An accompanying error message.
eiMessage :: Lens' ErrorInformation (Maybe Text)
eiMessage = lens _eiMessage (\ s a -> s{_eiMessage = a});

instance FromJSON ErrorInformation where
        parseJSON
          = withObject "ErrorInformation"
              (\ x ->
                 ErrorInformation' <$>
                   (x .:? "code") <*> (x .:? "message"))

-- | Information about an application revision.
--
-- /See:/ 'genericRevisionInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'griRegisterTime'
--
-- * 'griFirstUsedTime'
--
-- * 'griDeploymentGroups'
--
-- * 'griLastUsedTime'
--
-- * 'griDescription'
data GenericRevisionInfo = GenericRevisionInfo'{_griRegisterTime :: Maybe POSIX, _griFirstUsedTime :: Maybe POSIX, _griDeploymentGroups :: Maybe [Text], _griLastUsedTime :: Maybe POSIX, _griDescription :: Maybe Text} deriving (Eq, Read, Show)

-- | 'GenericRevisionInfo' smart constructor.
genericRevisionInfo :: GenericRevisionInfo
genericRevisionInfo = GenericRevisionInfo'{_griRegisterTime = Nothing, _griFirstUsedTime = Nothing, _griDeploymentGroups = Nothing, _griLastUsedTime = Nothing, _griDescription = Nothing};

-- | When the revision was registered with AWS CodeDeploy.
griRegisterTime :: Lens' GenericRevisionInfo (Maybe UTCTime)
griRegisterTime = lens _griRegisterTime (\ s a -> s{_griRegisterTime = a}) . mapping _Time;

-- | When the revision was first used by AWS CodeDeploy.
griFirstUsedTime :: Lens' GenericRevisionInfo (Maybe UTCTime)
griFirstUsedTime = lens _griFirstUsedTime (\ s a -> s{_griFirstUsedTime = a}) . mapping _Time;

-- | A list of deployment groups that use this revision.
griDeploymentGroups :: Lens' GenericRevisionInfo [Text]
griDeploymentGroups = lens _griDeploymentGroups (\ s a -> s{_griDeploymentGroups = a}) . _Default;

-- | When the revision was last used by AWS CodeDeploy.
griLastUsedTime :: Lens' GenericRevisionInfo (Maybe UTCTime)
griLastUsedTime = lens _griLastUsedTime (\ s a -> s{_griLastUsedTime = a}) . mapping _Time;

-- | A comment about the revision.
griDescription :: Lens' GenericRevisionInfo (Maybe Text)
griDescription = lens _griDescription (\ s a -> s{_griDescription = a});

instance FromJSON GenericRevisionInfo where
        parseJSON
          = withObject "GenericRevisionInfo"
              (\ x ->
                 GenericRevisionInfo' <$>
                   (x .:? "registerTime") <*> (x .:? "firstUsedTime")
                     <*> (x .:? "deploymentGroups" .!= mempty)
                     <*> (x .:? "lastUsedTime")
                     <*> (x .:? "description"))

-- | Information about the location of application artifacts that are stored
-- in GitHub.
--
-- /See:/ 'gitHubLocation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ghlCommitId'
--
-- * 'ghlRepository'
data GitHubLocation = GitHubLocation'{_ghlCommitId :: Maybe Text, _ghlRepository :: Maybe Text} deriving (Eq, Read, Show)

-- | 'GitHubLocation' smart constructor.
gitHubLocation :: GitHubLocation
gitHubLocation = GitHubLocation'{_ghlCommitId = Nothing, _ghlRepository = Nothing};

-- | The SHA1 commit ID of the GitHub commit that references the that
-- represents the bundled artifacts for the application revision.
ghlCommitId :: Lens' GitHubLocation (Maybe Text)
ghlCommitId = lens _ghlCommitId (\ s a -> s{_ghlCommitId = a});

-- | The GitHub account and repository pair that stores a reference to the
-- commit that represents the bundled artifacts for the application
-- revision.
--
-- Specified as account\/repository.
ghlRepository :: Lens' GitHubLocation (Maybe Text)
ghlRepository = lens _ghlRepository (\ s a -> s{_ghlRepository = a});

instance FromJSON GitHubLocation where
        parseJSON
          = withObject "GitHubLocation"
              (\ x ->
                 GitHubLocation' <$>
                   (x .:? "commitId") <*> (x .:? "repository"))

instance ToJSON GitHubLocation where
        toJSON GitHubLocation'{..}
          = object
              ["commitId" .= _ghlCommitId,
               "repository" .= _ghlRepository]

-- | Information about an on-premises instance.
--
-- /See:/ 'instanceInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iiInstanceARN'
--
-- * 'iiRegisterTime'
--
-- * 'iiDeregisterTime'
--
-- * 'iiIamUserARN'
--
-- * 'iiInstanceName'
--
-- * 'iiTags'
data InstanceInfo = InstanceInfo'{_iiInstanceARN :: Maybe Text, _iiRegisterTime :: Maybe POSIX, _iiDeregisterTime :: Maybe POSIX, _iiIamUserARN :: Maybe Text, _iiInstanceName :: Maybe Text, _iiTags :: Maybe [Tag]} deriving (Eq, Read, Show)

-- | 'InstanceInfo' smart constructor.
instanceInfo :: InstanceInfo
instanceInfo = InstanceInfo'{_iiInstanceARN = Nothing, _iiRegisterTime = Nothing, _iiDeregisterTime = Nothing, _iiIamUserARN = Nothing, _iiInstanceName = Nothing, _iiTags = Nothing};

-- | The ARN of the on-premises instance.
iiInstanceARN :: Lens' InstanceInfo (Maybe Text)
iiInstanceARN = lens _iiInstanceARN (\ s a -> s{_iiInstanceARN = a});

-- | The time that the on-premises instance was registered.
iiRegisterTime :: Lens' InstanceInfo (Maybe UTCTime)
iiRegisterTime = lens _iiRegisterTime (\ s a -> s{_iiRegisterTime = a}) . mapping _Time;

-- | If the on-premises instance was deregistered, the time that the
-- on-premises instance was deregistered.
iiDeregisterTime :: Lens' InstanceInfo (Maybe UTCTime)
iiDeregisterTime = lens _iiDeregisterTime (\ s a -> s{_iiDeregisterTime = a}) . mapping _Time;

-- | The IAM user ARN associated with the on-premises instance.
iiIamUserARN :: Lens' InstanceInfo (Maybe Text)
iiIamUserARN = lens _iiIamUserARN (\ s a -> s{_iiIamUserARN = a});

-- | The name of the on-premises instance.
iiInstanceName :: Lens' InstanceInfo (Maybe Text)
iiInstanceName = lens _iiInstanceName (\ s a -> s{_iiInstanceName = a});

-- | The tags that are currently associated with the on-premises instance.
iiTags :: Lens' InstanceInfo [Tag]
iiTags = lens _iiTags (\ s a -> s{_iiTags = a}) . _Default;

instance FromJSON InstanceInfo where
        parseJSON
          = withObject "InstanceInfo"
              (\ x ->
                 InstanceInfo' <$>
                   (x .:? "instanceArn") <*> (x .:? "registerTime") <*>
                     (x .:? "deregisterTime")
                     <*> (x .:? "iamUserArn")
                     <*> (x .:? "instanceName")
                     <*> (x .:? "tags" .!= mempty))

-- | Information about an instance in a deployment.
--
-- /See:/ 'instanceSummary' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'isInstanceId'
--
-- * 'isDeploymentId'
--
-- * 'isStatus'
--
-- * 'isLastUpdatedAt'
--
-- * 'isLifecycleEvents'
data InstanceSummary = InstanceSummary'{_isInstanceId :: Maybe Text, _isDeploymentId :: Maybe Text, _isStatus :: Maybe InstanceStatus, _isLastUpdatedAt :: Maybe POSIX, _isLifecycleEvents :: Maybe [LifecycleEvent]} deriving (Eq, Read, Show)

-- | 'InstanceSummary' smart constructor.
instanceSummary :: InstanceSummary
instanceSummary = InstanceSummary'{_isInstanceId = Nothing, _isDeploymentId = Nothing, _isStatus = Nothing, _isLastUpdatedAt = Nothing, _isLifecycleEvents = Nothing};

-- | The instance ID.
isInstanceId :: Lens' InstanceSummary (Maybe Text)
isInstanceId = lens _isInstanceId (\ s a -> s{_isInstanceId = a});

-- | The deployment ID.
isDeploymentId :: Lens' InstanceSummary (Maybe Text)
isDeploymentId = lens _isDeploymentId (\ s a -> s{_isDeploymentId = a});

-- | The deployment status for this instance:
--
-- -   Pending: The deployment is pending for this instance.
-- -   In Progress: The deployment is in progress for this instance.
-- -   Succeeded: The deployment has succeeded for this instance.
-- -   Failed: The deployment has failed for this instance.
-- -   Skipped: The deployment has been skipped for this instance.
-- -   Unknown: The deployment status is unknown for this instance.
isStatus :: Lens' InstanceSummary (Maybe InstanceStatus)
isStatus = lens _isStatus (\ s a -> s{_isStatus = a});

-- | A timestamp indicating when the instance information was last updated.
isLastUpdatedAt :: Lens' InstanceSummary (Maybe UTCTime)
isLastUpdatedAt = lens _isLastUpdatedAt (\ s a -> s{_isLastUpdatedAt = a}) . mapping _Time;

-- | A list of lifecycle events for this instance.
isLifecycleEvents :: Lens' InstanceSummary [LifecycleEvent]
isLifecycleEvents = lens _isLifecycleEvents (\ s a -> s{_isLifecycleEvents = a}) . _Default;

instance FromJSON InstanceSummary where
        parseJSON
          = withObject "InstanceSummary"
              (\ x ->
                 InstanceSummary' <$>
                   (x .:? "instanceId") <*> (x .:? "deploymentId") <*>
                     (x .:? "status")
                     <*> (x .:? "lastUpdatedAt")
                     <*> (x .:? "lifecycleEvents" .!= mempty))

-- | Information about a deployment lifecycle event.
--
-- /See:/ 'lifecycleEvent' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'leStatus'
--
-- * 'leStartTime'
--
-- * 'leLifecycleEventName'
--
-- * 'leDiagnostics'
--
-- * 'leEndTime'
data LifecycleEvent = LifecycleEvent'{_leStatus :: Maybe LifecycleEventStatus, _leStartTime :: Maybe POSIX, _leLifecycleEventName :: Maybe Text, _leDiagnostics :: Maybe Diagnostics, _leEndTime :: Maybe POSIX} deriving (Eq, Read, Show)

-- | 'LifecycleEvent' smart constructor.
lifecycleEvent :: LifecycleEvent
lifecycleEvent = LifecycleEvent'{_leStatus = Nothing, _leStartTime = Nothing, _leLifecycleEventName = Nothing, _leDiagnostics = Nothing, _leEndTime = Nothing};

-- | The deployment lifecycle event status:
--
-- -   Pending: The deployment lifecycle event is pending.
-- -   InProgress: The deployment lifecycle event is in progress.
-- -   Succeeded: The deployment lifecycle event has succeeded.
-- -   Failed: The deployment lifecycle event has failed.
-- -   Skipped: The deployment lifecycle event has been skipped.
-- -   Unknown: The deployment lifecycle event is unknown.
leStatus :: Lens' LifecycleEvent (Maybe LifecycleEventStatus)
leStatus = lens _leStatus (\ s a -> s{_leStatus = a});

-- | A timestamp indicating when the deployment lifecycle event started.
leStartTime :: Lens' LifecycleEvent (Maybe UTCTime)
leStartTime = lens _leStartTime (\ s a -> s{_leStartTime = a}) . mapping _Time;

-- | The deployment lifecycle event name, such as ApplicationStop,
-- BeforeInstall, AfterInstall, ApplicationStart, or ValidateService.
leLifecycleEventName :: Lens' LifecycleEvent (Maybe Text)
leLifecycleEventName = lens _leLifecycleEventName (\ s a -> s{_leLifecycleEventName = a});

-- | Diagnostic information about the deployment lifecycle event.
leDiagnostics :: Lens' LifecycleEvent (Maybe Diagnostics)
leDiagnostics = lens _leDiagnostics (\ s a -> s{_leDiagnostics = a});

-- | A timestamp indicating when the deployment lifecycle event ended.
leEndTime :: Lens' LifecycleEvent (Maybe UTCTime)
leEndTime = lens _leEndTime (\ s a -> s{_leEndTime = a}) . mapping _Time;

instance FromJSON LifecycleEvent where
        parseJSON
          = withObject "LifecycleEvent"
              (\ x ->
                 LifecycleEvent' <$>
                   (x .:? "status") <*> (x .:? "startTime") <*>
                     (x .:? "lifecycleEventName")
                     <*> (x .:? "diagnostics")
                     <*> (x .:? "endTime"))

-- | Information about minimum healthy instances.
--
-- /See:/ 'minimumHealthyHosts' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mhhValue'
--
-- * 'mhhType'
data MinimumHealthyHosts = MinimumHealthyHosts'{_mhhValue :: Maybe Int, _mhhType :: Maybe MinimumHealthyHostsType} deriving (Eq, Read, Show)

-- | 'MinimumHealthyHosts' smart constructor.
minimumHealthyHosts :: MinimumHealthyHosts
minimumHealthyHosts = MinimumHealthyHosts'{_mhhValue = Nothing, _mhhType = Nothing};

-- | The minimum healthy instances value.
mhhValue :: Lens' MinimumHealthyHosts (Maybe Int)
mhhValue = lens _mhhValue (\ s a -> s{_mhhValue = a});

-- | The minimum healthy instances type:
--
-- -   HOST_COUNT: The minimum number of healthy instances, as an absolute
--     value.
-- -   FLEET_PERCENT: The minimum number of healthy instances, as a
--     percentage of the total number of instances in the deployment.
--
-- For example, for 9 instances, if a HOST_COUNT of 6 is specified, deploy
-- to up to 3 instances at a time. The deployment succeeds if 6 or more
-- instances are successfully deployed to; otherwise, the deployment fails.
-- If a FLEET_PERCENT of 40 is specified, deploy to up to 5 instances at a
-- time. The deployment succeeds if 4 or more instances are successfully
-- deployed to; otherwise, the deployment fails.
--
-- In a call to the get deployment configuration operation,
-- CodeDeployDefault.OneAtATime will return a minimum healthy instances
-- type of MOST_CONCURRENCY and a value of 1. This means a deployment to
-- only one instances at a time. (You cannot set the type to
-- MOST_CONCURRENCY, only to HOST_COUNT or FLEET_PERCENT.)
mhhType :: Lens' MinimumHealthyHosts (Maybe MinimumHealthyHostsType)
mhhType = lens _mhhType (\ s a -> s{_mhhType = a});

instance FromJSON MinimumHealthyHosts where
        parseJSON
          = withObject "MinimumHealthyHosts"
              (\ x ->
                 MinimumHealthyHosts' <$>
                   (x .:? "value") <*> (x .:? "type"))

instance ToJSON MinimumHealthyHosts where
        toJSON MinimumHealthyHosts'{..}
          = object ["value" .= _mhhValue, "type" .= _mhhType]

-- | Information about an application revision\'s location.
--
-- /See:/ 'revisionLocation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rlRevisionType'
--
-- * 'rlS3Location'
--
-- * 'rlGitHubLocation'
data RevisionLocation = RevisionLocation'{_rlRevisionType :: Maybe RevisionLocationType, _rlS3Location :: Maybe S3Location, _rlGitHubLocation :: Maybe GitHubLocation} deriving (Eq, Read, Show)

-- | 'RevisionLocation' smart constructor.
revisionLocation :: RevisionLocation
revisionLocation = RevisionLocation'{_rlRevisionType = Nothing, _rlS3Location = Nothing, _rlGitHubLocation = Nothing};

-- | The application revision\'s type:
--
-- -   S3: An application revision stored in Amazon S3.
-- -   GitHub: An application revision stored in GitHub.
rlRevisionType :: Lens' RevisionLocation (Maybe RevisionLocationType)
rlRevisionType = lens _rlRevisionType (\ s a -> s{_rlRevisionType = a});

-- | FIXME: Undocumented member.
rlS3Location :: Lens' RevisionLocation (Maybe S3Location)
rlS3Location = lens _rlS3Location (\ s a -> s{_rlS3Location = a});

-- | FIXME: Undocumented member.
rlGitHubLocation :: Lens' RevisionLocation (Maybe GitHubLocation)
rlGitHubLocation = lens _rlGitHubLocation (\ s a -> s{_rlGitHubLocation = a});

instance FromJSON RevisionLocation where
        parseJSON
          = withObject "RevisionLocation"
              (\ x ->
                 RevisionLocation' <$>
                   (x .:? "revisionType") <*> (x .:? "s3Location") <*>
                     (x .:? "gitHubLocation"))

instance ToJSON RevisionLocation where
        toJSON RevisionLocation'{..}
          = object
              ["revisionType" .= _rlRevisionType,
               "s3Location" .= _rlS3Location,
               "gitHubLocation" .= _rlGitHubLocation]

-- | Information about the location of application artifacts that are stored
-- in Amazon S3.
--
-- /See:/ 's3Location' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slBundleType'
--
-- * 'slETag'
--
-- * 'slBucket'
--
-- * 'slKey'
--
-- * 'slVersion'
data S3Location = S3Location'{_slBundleType :: Maybe BundleType, _slETag :: Maybe Text, _slBucket :: Maybe Text, _slKey :: Maybe Text, _slVersion :: Maybe Text} deriving (Eq, Read, Show)

-- | 'S3Location' smart constructor.
s3Location :: S3Location
s3Location = S3Location'{_slBundleType = Nothing, _slETag = Nothing, _slBucket = Nothing, _slKey = Nothing, _slVersion = Nothing};

-- | The file type of the application revision. Must be one of the following:
--
-- -   tar: A tar archive file.
-- -   tgz: A compressed tar archive file.
-- -   zip: A zip archive file.
slBundleType :: Lens' S3Location (Maybe BundleType)
slBundleType = lens _slBundleType (\ s a -> s{_slBundleType = a});

-- | The ETag of the Amazon S3 object that represents the bundled artifacts
-- for the application revision.
--
-- If the ETag is not specified as an input parameter, ETag validation of
-- the object will be skipped.
slETag :: Lens' S3Location (Maybe Text)
slETag = lens _slETag (\ s a -> s{_slETag = a});

-- | The name of the Amazon S3 bucket where the application revision is
-- stored.
slBucket :: Lens' S3Location (Maybe Text)
slBucket = lens _slBucket (\ s a -> s{_slBucket = a});

-- | The name of the Amazon S3 object that represents the bundled artifacts
-- for the application revision.
slKey :: Lens' S3Location (Maybe Text)
slKey = lens _slKey (\ s a -> s{_slKey = a});

-- | A specific version of the Amazon S3 object that represents the bundled
-- artifacts for the application revision.
--
-- If the version is not specified, the system will use the most recent
-- version by default.
slVersion :: Lens' S3Location (Maybe Text)
slVersion = lens _slVersion (\ s a -> s{_slVersion = a});

instance FromJSON S3Location where
        parseJSON
          = withObject "S3Location"
              (\ x ->
                 S3Location' <$>
                   (x .:? "bundleType") <*> (x .:? "eTag") <*>
                     (x .:? "bucket")
                     <*> (x .:? "key")
                     <*> (x .:? "version"))

instance ToJSON S3Location where
        toJSON S3Location'{..}
          = object
              ["bundleType" .= _slBundleType, "eTag" .= _slETag,
               "bucket" .= _slBucket, "key" .= _slKey,
               "version" .= _slVersion]

-- | Information about a tag.
--
-- /See:/ 'tag' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagValue'
--
-- * 'tagKey'
data Tag = Tag'{_tagValue :: Maybe Text, _tagKey :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Tag' smart constructor.
tag :: Tag
tag = Tag'{_tagValue = Nothing, _tagKey = Nothing};

-- | The tag\'s value.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | The tag\'s key.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "Value") <*> (x .:? "Key"))

instance ToJSON Tag where
        toJSON Tag'{..}
          = object ["Value" .= _tagValue, "Key" .= _tagKey]

-- | Information about an on-premises instance tag filter.
--
-- /See:/ 'tagFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tfValue'
--
-- * 'tfKey'
--
-- * 'tfType'
data TagFilter = TagFilter'{_tfValue :: Maybe Text, _tfKey :: Maybe Text, _tfType :: Maybe TagFilterType} deriving (Eq, Read, Show)

-- | 'TagFilter' smart constructor.
tagFilter :: TagFilter
tagFilter = TagFilter'{_tfValue = Nothing, _tfKey = Nothing, _tfType = Nothing};

-- | The on-premises instance tag filter value.
tfValue :: Lens' TagFilter (Maybe Text)
tfValue = lens _tfValue (\ s a -> s{_tfValue = a});

-- | The on-premises instance tag filter key.
tfKey :: Lens' TagFilter (Maybe Text)
tfKey = lens _tfKey (\ s a -> s{_tfKey = a});

-- | The on-premises instance tag filter type:
--
-- -   KEY_ONLY: Key only.
-- -   VALUE_ONLY: Value only.
-- -   KEY_AND_VALUE: Key and value.
tfType :: Lens' TagFilter (Maybe TagFilterType)
tfType = lens _tfType (\ s a -> s{_tfType = a});

instance FromJSON TagFilter where
        parseJSON
          = withObject "TagFilter"
              (\ x ->
                 TagFilter' <$>
                   (x .:? "Value") <*> (x .:? "Key") <*> (x .:? "Type"))

instance ToJSON TagFilter where
        toJSON TagFilter'{..}
          = object
              ["Value" .= _tfValue, "Key" .= _tfKey,
               "Type" .= _tfType]

-- | Information about a time range.
--
-- /See:/ 'timeRange' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'trStart'
--
-- * 'trEnd'
data TimeRange = TimeRange'{_trStart :: Maybe POSIX, _trEnd :: Maybe POSIX} deriving (Eq, Read, Show)

-- | 'TimeRange' smart constructor.
timeRange :: TimeRange
timeRange = TimeRange'{_trStart = Nothing, _trEnd = Nothing};

-- | The time range\'s start time.
--
-- Specify null to leave the time range\'s start time open-ended.
trStart :: Lens' TimeRange (Maybe UTCTime)
trStart = lens _trStart (\ s a -> s{_trStart = a}) . mapping _Time;

-- | The time range\'s end time.
--
-- Specify null to leave the time range\'s end time open-ended.
trEnd :: Lens' TimeRange (Maybe UTCTime)
trEnd = lens _trEnd (\ s a -> s{_trEnd = a}) . mapping _Time;

instance ToJSON TimeRange where
        toJSON TimeRange'{..}
          = object ["start" .= _trStart, "end" .= _trEnd]

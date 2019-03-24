{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EFS.Types
    (
    -- * Service Configuration
      efs

    -- * Errors
    , _MountTargetNotFound
    , _SecurityGroupLimitExceeded
    , _SecurityGroupNotFound
    , _MountTargetConflict
    , _UnsupportedAvailabilityZone
    , _FileSystemLimitExceeded
    , _TooManyRequests
    , _NetworkInterfaceLimitExceeded
    , _FileSystemAlreadyExists
    , _SubnetNotFound
    , _FileSystemNotFound
    , _IncorrectFileSystemLifeCycleState
    , _BadRequest
    , _NoFreeAddressesInSubnet
    , _ThroughputLimitExceeded
    , _DependencyTimeout
    , _FileSystemInUse
    , _IncorrectMountTargetState
    , _InternalServerError
    , _IPAddressInUse
    , _InsufficientThroughputCapacity

    -- * LifeCycleState
    , LifeCycleState (..)

    -- * PerformanceMode
    , PerformanceMode (..)

    -- * ThroughputMode
    , ThroughputMode (..)

    -- * TransitionToIARules
    , TransitionToIARules (..)

    -- * FileSystemDescription
    , FileSystemDescription
    , fileSystemDescription
    , fsdProvisionedThroughputInMibps
    , fsdEncrypted
    , fsdThroughputMode
    , fsdKMSKeyId
    , fsdName
    , fsdOwnerId
    , fsdCreationToken
    , fsdFileSystemId
    , fsdCreationTime
    , fsdLifeCycleState
    , fsdNumberOfMountTargets
    , fsdSizeInBytes
    , fsdPerformanceMode
    , fsdTags

    -- * FileSystemSize
    , FileSystemSize
    , fileSystemSize
    , fssValueInIA
    , fssValueInStandard
    , fssTimestamp
    , fssValue

    -- * LifecycleConfigurationDescription
    , LifecycleConfigurationDescription
    , lifecycleConfigurationDescription
    , lcdLifecyclePolicies

    -- * LifecyclePolicy
    , LifecyclePolicy
    , lifecyclePolicy
    , lpTransitionToIA

    -- * MountTargetDescription
    , MountTargetDescription
    , mountTargetDescription
    , mtdIPAddress
    , mtdNetworkInterfaceId
    , mtdOwnerId
    , mtdMountTargetId
    , mtdFileSystemId
    , mtdSubnetId
    , mtdLifeCycleState

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue
    ) where

import Network.AWS.EFS.Types.Product
import Network.AWS.EFS.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-02-01@ of the Amazon Elastic File System SDK configuration.
efs :: Service
efs =
  Service
    { _svcAbbrev = "EFS"
    , _svcSigner = v4
    , _svcPrefix = "elasticfilesystem"
    , _svcVersion = "2015-02-01"
    , _svcEndpoint = defaultEndpoint efs
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "EFS"
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


-- | Returned if there is no mount target with the specified ID found in the caller's account.
--
--
_MountTargetNotFound :: AsError a => Getting (First ServiceError) a ServiceError
_MountTargetNotFound =
  _MatchServiceError efs "MountTargetNotFound" . hasStatus 404


-- | Returned if the size of @SecurityGroups@ specified in the request is greater than five.
--
--
_SecurityGroupLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_SecurityGroupLimitExceeded =
  _MatchServiceError efs "SecurityGroupLimitExceeded" . hasStatus 400


-- | Returned if one of the specified security groups doesn't exist in the subnet's VPC.
--
--
_SecurityGroupNotFound :: AsError a => Getting (First ServiceError) a ServiceError
_SecurityGroupNotFound =
  _MatchServiceError efs "SecurityGroupNotFound" . hasStatus 400


-- | Returned if the mount target would violate one of the specified restrictions based on the file system's existing mount targets.
--
--
_MountTargetConflict :: AsError a => Getting (First ServiceError) a ServiceError
_MountTargetConflict =
  _MatchServiceError efs "MountTargetConflict" . hasStatus 409


-- |
--
--
_UnsupportedAvailabilityZone :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedAvailabilityZone =
  _MatchServiceError efs "UnsupportedAvailabilityZone" . hasStatus 400


-- | Returned if the AWS account has already created the maximum number of file systems allowed per account.
--
--
_FileSystemLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_FileSystemLimitExceeded =
  _MatchServiceError efs "FileSystemLimitExceeded" . hasStatus 403


-- | Returned if you don

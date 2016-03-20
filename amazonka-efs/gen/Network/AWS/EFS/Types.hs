{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
    , _NetworkInterfaceLimitExceeded
    , _FileSystemAlreadyExists
    , _SubnetNotFound
    , _FileSystemNotFound
    , _IncorrectFileSystemLifeCycleState
    , _BadRequest
    , _NoFreeAddressesInSubnet
    , _DependencyTimeout
    , _FileSystemInUse
    , _IncorrectMountTargetState
    , _InternalServerError
    , _IPAddressInUse

    -- * LifeCycleState
    , LifeCycleState (..)

    -- * FileSystemDescription
    , FileSystemDescription
    , fileSystemDescription
    , fsdName
    , fsdOwnerId
    , fsdCreationToken
    , fsdFileSystemId
    , fsdCreationTime
    , fsdLifeCycleState
    , fsdNumberOfMountTargets
    , fsdSizeInBytes

    -- * FileSystemSize
    , FileSystemSize
    , fileSystemSize
    , fssTimestamp
    , fssValue

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

import           Network.AWS.EFS.Types.Product
import           Network.AWS.EFS.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2015-02-01' of the Amazon Elastic File System SDK configuration.
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
    , _svcError = parseJSONError
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
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | Returned if there is no mount target with the specified ID found in the
-- caller\'s account.
_MountTargetNotFound :: AsError a => Getting (First ServiceError) a ServiceError
_MountTargetNotFound =
    _ServiceError . hasStatus 404 . hasCode "MountTargetNotFound"

-- | Returned if the size of 'SecurityGroups' specified in the request is
-- greater than five.
_SecurityGroupLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_SecurityGroupLimitExceeded =
    _ServiceError . hasStatus 400 . hasCode "SecurityGroupLimitExceeded"

-- | Returned if one of the specified security groups does not exist in the
-- subnet\'s VPC.
_SecurityGroupNotFound :: AsError a => Getting (First ServiceError) a ServiceError
_SecurityGroupNotFound =
    _ServiceError . hasStatus 400 . hasCode "SecurityGroupNotFound"

-- | Returned if the mount target would violate one of the specified
-- restrictions based on the file system\'s existing mount targets.
_MountTargetConflict :: AsError a => Getting (First ServiceError) a ServiceError
_MountTargetConflict =
    _ServiceError . hasStatus 409 . hasCode "MountTargetConflict"

-- | Prism for UnsupportedAvailabilityZone' errors.
_UnsupportedAvailabilityZone :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedAvailabilityZone =
    _ServiceError . hasStatus 400 . hasCode "UnsupportedAvailabilityZone"

-- | Returned if the AWS account has already created maximum number of file
-- systems allowed per account.
_FileSystemLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_FileSystemLimitExceeded =
    _ServiceError . hasStatus 403 . hasCode "FileSystemLimitExceeded"

-- | The calling account has reached the ENI limit for the specific AWS
-- region. Client should try to delete some ENIs or get its account limit
-- raised. For more information, go to
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Appendix_Limits.html Amazon VPC Limits>
-- in the Amazon Virtual Private Cloud User Guide (see the Network
-- interfaces per VPC entry in the table).
_NetworkInterfaceLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_NetworkInterfaceLimitExceeded =
    _ServiceError . hasStatus 409 . hasCode "NetworkInterfaceLimitExceeded"

-- | Returned if the file system you are trying to create already exists,
-- with the creation token you provided.
_FileSystemAlreadyExists :: AsError a => Getting (First ServiceError) a ServiceError
_FileSystemAlreadyExists =
    _ServiceError . hasStatus 409 . hasCode "FileSystemAlreadyExists"

-- | Returned if there is no subnet with ID 'SubnetId' provided in the
-- request.
_SubnetNotFound :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetNotFound = _ServiceError . hasStatus 400 . hasCode "SubnetNotFound"

-- | Returned if the specified 'FileSystemId' does not exist in the
-- requester\'s AWS account.
_FileSystemNotFound :: AsError a => Getting (First ServiceError) a ServiceError
_FileSystemNotFound =
    _ServiceError . hasStatus 404 . hasCode "FileSystemNotFound"

-- | Returned if the file system\'s life cycle state is not \"created\".
_IncorrectFileSystemLifeCycleState :: AsError a => Getting (First ServiceError) a ServiceError
_IncorrectFileSystemLifeCycleState =
    _ServiceError . hasStatus 409 . hasCode "IncorrectFileSystemLifeCycleState"

-- | Returned if the request is malformed or contains an error such as an
-- invalid parameter value or a missing required parameter.
_BadRequest :: AsError a => Getting (First ServiceError) a ServiceError
_BadRequest = _ServiceError . hasStatus 400 . hasCode "BadRequest"

-- | Returned if 'IpAddress' was not specified in the request and there are
-- no free IP addresses in the subnet.
_NoFreeAddressesInSubnet :: AsError a => Getting (First ServiceError) a ServiceError
_NoFreeAddressesInSubnet =
    _ServiceError . hasStatus 409 . hasCode "NoFreeAddressesInSubnet"

-- | The service timed out trying to fulfill the request, and the client
-- should try the call again.
_DependencyTimeout :: AsError a => Getting (First ServiceError) a ServiceError
_DependencyTimeout =
    _ServiceError . hasStatus 504 . hasCode "DependencyTimeout"

-- | Returned if a file system has mount targets.
_FileSystemInUse :: AsError a => Getting (First ServiceError) a ServiceError
_FileSystemInUse = _ServiceError . hasStatus 409 . hasCode "FileSystemInUse"

-- | Returned if the mount target is not in the correct state for the
-- operation.
_IncorrectMountTargetState :: AsError a => Getting (First ServiceError) a ServiceError
_IncorrectMountTargetState =
    _ServiceError . hasStatus 409 . hasCode "IncorrectMountTargetState"

-- | Returned if an error occurred on the server side.
_InternalServerError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerError =
    _ServiceError . hasStatus 500 . hasCode "InternalServerError"

-- | Returned if the request specified an 'IpAddress' that is already in use
-- in the subnet.
_IPAddressInUse :: AsError a => Getting (First ServiceError) a ServiceError
_IPAddressInUse = _ServiceError . hasStatus 409 . hasCode "IpAddressInUse"

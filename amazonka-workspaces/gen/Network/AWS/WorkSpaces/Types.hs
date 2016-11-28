{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types
    (
    -- * Service Configuration
      workSpaces

    -- * Errors
    , _AccessDeniedException
    , _ResourceUnavailableException
    , _InvalidParameterValuesException
    , _OperationInProgressException
    , _ResourceLimitExceededException
    , _InvalidResourceStateException
    , _UnsupportedWorkspaceConfigurationException
    , _ResourceNotFoundException

    -- * Compute
    , Compute (..)

    -- * ConnectionState
    , ConnectionState (..)

    -- * RunningMode
    , RunningMode (..)

    -- * WorkspaceDirectoryState
    , WorkspaceDirectoryState (..)

    -- * WorkspaceDirectoryType
    , WorkspaceDirectoryType (..)

    -- * WorkspaceState
    , WorkspaceState (..)

    -- * ComputeType
    , ComputeType
    , computeType
    , ctName

    -- * DefaultWorkspaceCreationProperties
    , DefaultWorkspaceCreationProperties
    , defaultWorkspaceCreationProperties
    , dwcpCustomSecurityGroupId
    , dwcpUserEnabledAsLocalAdministrator
    , dwcpEnableWorkDocs
    , dwcpEnableInternetAccess
    , dwcpDefaultOu

    -- * FailedCreateWorkspaceRequest
    , FailedCreateWorkspaceRequest
    , failedCreateWorkspaceRequest
    , fcwrWorkspaceRequest
    , fcwrErrorCode
    , fcwrErrorMessage

    -- * FailedWorkspaceChangeRequest
    , FailedWorkspaceChangeRequest
    , failedWorkspaceChangeRequest
    , fwcrErrorCode
    , fwcrWorkspaceId
    , fwcrErrorMessage

    -- * RebootRequest
    , RebootRequest
    , rebootRequest
    , rWorkspaceId

    -- * RebuildRequest
    , RebuildRequest
    , rebuildRequest
    , rrWorkspaceId

    -- * StartRequest
    , StartRequest
    , startRequest
    , sWorkspaceId

    -- * StopRequest
    , StopRequest
    , stopRequest
    , srWorkspaceId

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * TerminateRequest
    , TerminateRequest
    , terminateRequest
    , trWorkspaceId

    -- * UserStorage
    , UserStorage
    , userStorage
    , usCapacity

    -- * Workspace
    , Workspace
    , workspace
    , wDirectoryId
    , wState
    , wIPAddress
    , wUserName
    , wSubnetId
    , wBundleId
    , wWorkspaceProperties
    , wRootVolumeEncryptionEnabled
    , wErrorCode
    , wVolumeEncryptionKey
    , wComputerName
    , wWorkspaceId
    , wUserVolumeEncryptionEnabled
    , wErrorMessage

    -- * WorkspaceBundle
    , WorkspaceBundle
    , workspaceBundle
    , wbBundleId
    , wbOwner
    , wbName
    , wbComputeType
    , wbUserStorage
    , wbDescription

    -- * WorkspaceConnectionStatus
    , WorkspaceConnectionStatus
    , workspaceConnectionStatus
    , wcsLastKnownUserConnectionTimestamp
    , wcsConnectionStateCheckTimestamp
    , wcsWorkspaceId
    , wcsConnectionState

    -- * WorkspaceDirectory
    , WorkspaceDirectory
    , workspaceDirectory
    , wdRegistrationCode
    , wdIAMRoleId
    , wdDirectoryId
    , wdState
    , wdCustomerUserName
    , wdSubnetIds
    , wdAlias
    , wdWorkspaceSecurityGroupId
    , wdDirectoryType
    , wdWorkspaceCreationProperties
    , wdDNSIPAddresses
    , wdDirectoryName

    -- * WorkspaceProperties
    , WorkspaceProperties
    , workspaceProperties
    , wpRunningMode
    , wpRunningModeAutoStopTimeoutInMinutes

    -- * WorkspaceRequest
    , WorkspaceRequest
    , workspaceRequest
    , wrWorkspaceProperties
    , wrRootVolumeEncryptionEnabled
    , wrVolumeEncryptionKey
    , wrUserVolumeEncryptionEnabled
    , wrTags
    , wrDirectoryId
    , wrUserName
    , wrBundleId
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4
import           Network.AWS.WorkSpaces.Types.Product
import           Network.AWS.WorkSpaces.Types.Sum

-- | API version @2015-04-08@ of the Amazon WorkSpaces SDK configuration.
workSpaces :: Service
workSpaces =
    Service
    { _svcAbbrev = "WorkSpaces"
    , _svcSigner = v4
    , _svcPrefix = "workspaces"
    , _svcVersion = "2015-04-08"
    , _svcEndpoint = defaultEndpoint workSpaces
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "WorkSpaces"
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

-- | Prism for AccessDeniedException' errors.
_AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedException = _ServiceError . hasCode "AccessDeniedException"

-- | The specified resource is not available.
--
--
_ResourceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceUnavailableException =
    _ServiceError . hasCode "ResourceUnavailableException"

-- | One or more parameter values are not valid.
--
--
_InvalidParameterValuesException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValuesException =
    _ServiceError . hasCode "InvalidParameterValuesException"

-- | The properties of this WorkSpace are currently being modified. Try again in a moment.
--
--
_OperationInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationInProgressException =
    _ServiceError . hasCode "OperationInProgressException"

-- | Your resource limits have been exceeded.
--
--
_ResourceLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceLimitExceededException =
    _ServiceError . hasCode "ResourceLimitExceededException"

-- | The specified WorkSpace has an invalid state for this operation.
--
--
_InvalidResourceStateException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidResourceStateException =
    _ServiceError . hasCode "InvalidResourceStateException"

-- | The WorkSpace does not have the supported configuration for this operation. For more information, see the <http://docs.aws.amazon.com/workspaces/latest/adminguide Amazon WorkSpaces Administration Guide> .
--
--
_UnsupportedWorkspaceConfigurationException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedWorkspaceConfigurationException =
    _ServiceError . hasCode "UnsupportedWorkspaceConfigurationException"

-- | The resource could not be found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasCode "ResourceNotFoundException"

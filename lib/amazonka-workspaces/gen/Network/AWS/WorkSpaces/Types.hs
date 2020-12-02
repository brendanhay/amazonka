{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types
    (
    -- * Service Configuration
      workSpaces

    -- * Errors
    , _AccessDeniedException
    , _ResourceCreationFailedException
    , _ResourceUnavailableException
    , _InvalidParameterValuesException
    , _ResourceAssociatedException
    , _OperationInProgressException
    , _ResourceAlreadyExistsException
    , _ResourceLimitExceededException
    , _InvalidResourceStateException
    , _OperationNotSupportedException
    , _UnsupportedWorkspaceConfigurationException
    , _ResourceNotFoundException

    -- * Compute
    , Compute (..)

    -- * ConnectionState
    , ConnectionState (..)

    -- * ModificationResourceEnum
    , ModificationResourceEnum (..)

    -- * ModificationStateEnum
    , ModificationStateEnum (..)

    -- * RunningMode
    , RunningMode (..)

    -- * TargetWorkspaceState
    , TargetWorkspaceState (..)

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

    -- * IPRuleItem
    , IPRuleItem
    , ipRuleItem
    , iriRuleDesc
    , iriIpRule

    -- * ModificationState
    , ModificationState
    , modificationState
    , msState
    , msResource

    -- * RebootRequest
    , RebootRequest
    , rebootRequest
    , rWorkspaceId

    -- * RebuildRequest
    , RebuildRequest
    , rebuildRequest
    , rrWorkspaceId

    -- * RootStorage
    , RootStorage
    , rootStorage
    , rsCapacity

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
    , wModificationStates
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
    , wbRootStorage
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
    , wdIpGroupIds
    , wdAlias
    , wdWorkspaceSecurityGroupId
    , wdDirectoryType
    , wdWorkspaceCreationProperties
    , wdDNSIPAddresses
    , wdDirectoryName

    -- * WorkspaceProperties
    , WorkspaceProperties
    , workspaceProperties
    , wpComputeTypeName
    , wpRunningMode
    , wpRootVolumeSizeGib
    , wpRunningModeAutoStopTimeoutInMinutes
    , wpUserVolumeSizeGib

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

    -- * WorkspacesIPGroup
    , WorkspacesIPGroup
    , workspacesIPGroup
    , wigGroupDesc
    , wigUserRules
    , wigGroupId
    , wigGroupName
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.WorkSpaces.Types.Product
import Network.AWS.WorkSpaces.Types.Sum

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


-- | The user is not authorized to access a resource.
--
--
_AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedException = _MatchServiceError workSpaces "AccessDeniedException"


-- | The resource could not be created.
--
--
_ResourceCreationFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceCreationFailedException =
  _MatchServiceError workSpaces "ResourceCreationFailedException"


-- | The specified resource is not available.
--
--
_ResourceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceUnavailableException =
  _MatchServiceError workSpaces "ResourceUnavailableException"


-- | One or more parameter values are not valid.
--
--
_InvalidParameterValuesException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValuesException =
  _MatchServiceError workSpaces "InvalidParameterValuesException"


-- | The resource is associated with a directory.
--
--
_ResourceAssociatedException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAssociatedException =
  _MatchServiceError workSpaces "ResourceAssociatedException"


-- | The properties of this WorkSpace are currently being modified. Try again in a moment.
--
--
_OperationInProgressException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationInProgressException =
  _MatchServiceError workSpaces "OperationInProgressException"


-- | The specified resource already exists.
--
--
_ResourceAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsException =
  _MatchServiceError workSpaces "ResourceAlreadyExistsException"


-- | Your resource limits have been exceeded.
--
--
_ResourceLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceLimitExceededException =
  _MatchServiceError workSpaces "ResourceLimitExceededException"


-- | The state of the resource is not valid for this operation.
--
--
_InvalidResourceStateException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidResourceStateException =
  _MatchServiceError workSpaces "InvalidResourceStateException"


-- | This operation is not supported.
--
--
_OperationNotSupportedException :: AsError a => Getting (First ServiceError) a ServiceError
_OperationNotSupportedException =
  _MatchServiceError workSpaces "OperationNotSupportedException"


-- | The configuration of this WorkSpace is not supported for this operation. For more information, see the <http://docs.aws.amazon.com/workspaces/latest/adminguide/ Amazon WorkSpaces Administration Guide> .
--
--
_UnsupportedWorkspaceConfigurationException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedWorkspaceConfigurationException =
  _MatchServiceError workSpaces "UnsupportedWorkspaceConfigurationException"


-- | The resource could not be found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError workSpaces "ResourceNotFoundException"


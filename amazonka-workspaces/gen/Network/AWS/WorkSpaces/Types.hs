{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types
    (
    -- * Service
      WorkSpaces

    -- * Errors
    , _InvalidParameterValuesException
    , _ResourceUnavailableException
    , _ResourceLimitExceededException

    -- * Compute
    , Compute (..)

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
    , wIPAddress
    , wState
    , wUserName
    , wSubnetId
    , wBundleId
    , wErrorCode
    , wWorkspaceId
    , wErrorMessage

    -- * WorkspaceBundle
    , WorkspaceBundle
    , workspaceBundle
    , wbOwner
    , wbBundleId
    , wbName
    , wbComputeType
    , wbUserStorage
    , wbDescription

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
    , wdDirectoryType
    , wdWorkspaceSecurityGroupId
    , wdWorkspaceCreationProperties
    , wdDNSIPAddresses
    , wdDirectoryName

    -- * WorkspaceRequest
    , WorkspaceRequest
    , workspaceRequest
    , wrDirectoryId
    , wrUserName
    , wrBundleId
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4
import           Network.AWS.WorkSpaces.Types.Product
import           Network.AWS.WorkSpaces.Types.Sum

-- | Version @2015-04-08@ of the Amazon WorkSpaces SDK.
data WorkSpaces

instance AWSService WorkSpaces where
    type Sg WorkSpaces = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "WorkSpaces"
            , _svcPrefix = "workspaces"
            , _svcVersion = "2015-04-08"
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

-- | One or more parameter values are not valid.
_InvalidParameterValuesException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValuesException =
    _ServiceError . hasCode "InvalidParameterValuesException"

-- | The specified resource is not available.
_ResourceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceUnavailableException =
    _ServiceError . hasCode "ResourceUnavailableException"

-- | Your resource limits have been exceeded.
_ResourceLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceLimitExceededException =
    _ServiceError . hasCode "ResourceLimitExceededException"

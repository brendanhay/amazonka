{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon WorkSpaces Service__
--
-- This reference provides detailed information about the Amazon WorkSpaces operations.
--
module Network.AWS.WorkSpaces
    (
    -- * Service Configuration
      workSpaces

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    , _AccessDeniedException

    -- ** ResourceUnavailableException
    , _ResourceUnavailableException

    -- ** InvalidParameterValuesException
    , _InvalidParameterValuesException

    -- ** OperationInProgressException
    , _OperationInProgressException

    -- ** ResourceLimitExceededException
    , _ResourceLimitExceededException

    -- ** InvalidResourceStateException
    , _InvalidResourceStateException

    -- ** UnsupportedWorkspaceConfigurationException
    , _UnsupportedWorkspaceConfigurationException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ModifyWorkspaceProperties
    , module Network.AWS.WorkSpaces.ModifyWorkspaceProperties

    -- ** DescribeTags
    , module Network.AWS.WorkSpaces.DescribeTags

    -- ** DescribeWorkspaceDirectories (Paginated)
    , module Network.AWS.WorkSpaces.DescribeWorkspaceDirectories

    -- ** DescribeWorkspaceBundles (Paginated)
    , module Network.AWS.WorkSpaces.DescribeWorkspaceBundles

    -- ** RebuildWorkspaces
    , module Network.AWS.WorkSpaces.RebuildWorkspaces

    -- ** CreateTags
    , module Network.AWS.WorkSpaces.CreateTags

    -- ** DeleteTags
    , module Network.AWS.WorkSpaces.DeleteTags

    -- ** StopWorkspaces
    , module Network.AWS.WorkSpaces.StopWorkspaces

    -- ** DescribeWorkspacesConnectionStatus
    , module Network.AWS.WorkSpaces.DescribeWorkspacesConnectionStatus

    -- ** RebootWorkspaces
    , module Network.AWS.WorkSpaces.RebootWorkspaces

    -- ** TerminateWorkspaces
    , module Network.AWS.WorkSpaces.TerminateWorkspaces

    -- ** CreateWorkspaces
    , module Network.AWS.WorkSpaces.CreateWorkspaces

    -- ** DescribeWorkspaces (Paginated)
    , module Network.AWS.WorkSpaces.DescribeWorkspaces

    -- ** StartWorkspaces
    , module Network.AWS.WorkSpaces.StartWorkspaces

    -- * Types

    -- ** Compute
    , Compute (..)

    -- ** ConnectionState
    , ConnectionState (..)

    -- ** RunningMode
    , RunningMode (..)

    -- ** WorkspaceDirectoryState
    , WorkspaceDirectoryState (..)

    -- ** WorkspaceDirectoryType
    , WorkspaceDirectoryType (..)

    -- ** WorkspaceState
    , WorkspaceState (..)

    -- ** ComputeType
    , ComputeType
    , computeType
    , ctName

    -- ** DefaultWorkspaceCreationProperties
    , DefaultWorkspaceCreationProperties
    , defaultWorkspaceCreationProperties
    , dwcpCustomSecurityGroupId
    , dwcpUserEnabledAsLocalAdministrator
    , dwcpEnableWorkDocs
    , dwcpEnableInternetAccess
    , dwcpDefaultOu

    -- ** FailedCreateWorkspaceRequest
    , FailedCreateWorkspaceRequest
    , failedCreateWorkspaceRequest
    , fcwrWorkspaceRequest
    , fcwrErrorCode
    , fcwrErrorMessage

    -- ** FailedWorkspaceChangeRequest
    , FailedWorkspaceChangeRequest
    , failedWorkspaceChangeRequest
    , fwcrErrorCode
    , fwcrWorkspaceId
    , fwcrErrorMessage

    -- ** RebootRequest
    , RebootRequest
    , rebootRequest
    , rWorkspaceId

    -- ** RebuildRequest
    , RebuildRequest
    , rebuildRequest
    , rrWorkspaceId

    -- ** StartRequest
    , StartRequest
    , startRequest
    , sWorkspaceId

    -- ** StopRequest
    , StopRequest
    , stopRequest
    , srWorkspaceId

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** TerminateRequest
    , TerminateRequest
    , terminateRequest
    , trWorkspaceId

    -- ** UserStorage
    , UserStorage
    , userStorage
    , usCapacity

    -- ** Workspace
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

    -- ** WorkspaceBundle
    , WorkspaceBundle
    , workspaceBundle
    , wbBundleId
    , wbOwner
    , wbName
    , wbComputeType
    , wbUserStorage
    , wbDescription

    -- ** WorkspaceConnectionStatus
    , WorkspaceConnectionStatus
    , workspaceConnectionStatus
    , wcsLastKnownUserConnectionTimestamp
    , wcsConnectionStateCheckTimestamp
    , wcsWorkspaceId
    , wcsConnectionState

    -- ** WorkspaceDirectory
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

    -- ** WorkspaceProperties
    , WorkspaceProperties
    , workspaceProperties
    , wpRunningMode
    , wpRunningModeAutoStopTimeoutInMinutes

    -- ** WorkspaceRequest
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

import Network.AWS.WorkSpaces.CreateTags
import Network.AWS.WorkSpaces.CreateWorkspaces
import Network.AWS.WorkSpaces.DeleteTags
import Network.AWS.WorkSpaces.DescribeTags
import Network.AWS.WorkSpaces.DescribeWorkspaceBundles
import Network.AWS.WorkSpaces.DescribeWorkspaceDirectories
import Network.AWS.WorkSpaces.DescribeWorkspaces
import Network.AWS.WorkSpaces.DescribeWorkspacesConnectionStatus
import Network.AWS.WorkSpaces.ModifyWorkspaceProperties
import Network.AWS.WorkSpaces.RebootWorkspaces
import Network.AWS.WorkSpaces.RebuildWorkspaces
import Network.AWS.WorkSpaces.StartWorkspaces
import Network.AWS.WorkSpaces.StopWorkspaces
import Network.AWS.WorkSpaces.TerminateWorkspaces
import Network.AWS.WorkSpaces.Types
import Network.AWS.WorkSpaces.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'WorkSpaces'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}

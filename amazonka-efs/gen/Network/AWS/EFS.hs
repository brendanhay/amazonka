{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Elastic File System
--
-- /See:/ <http://docs.aws.amazon.com/elasticfilesystem/latest/devguide/welcome.html AWS API Reference>
module Network.AWS.EFS
    (
    -- * Service
      EFS

    -- * Errors
    -- $errors

    -- ** MountTargetNotFound
    , _MountTargetNotFound

    -- ** SecurityGroupLimitExceeded
    , _SecurityGroupLimitExceeded

    -- ** MountTargetConflict
    , _MountTargetConflict

    -- ** UnsupportedAvailabilityZone
    , _UnsupportedAvailabilityZone

    -- ** SecurityGroupNotFound
    , _SecurityGroupNotFound

    -- ** FileSystemAlreadyExists
    , _FileSystemAlreadyExists

    -- ** FileSystemLimitExceeded
    , _FileSystemLimitExceeded

    -- ** NetworkInterfaceLimitExceeded
    , _NetworkInterfaceLimitExceeded

    -- ** FileSystemNotFound
    , _FileSystemNotFound

    -- ** SubnetNotFound
    , _SubnetNotFound

    -- ** IncorrectFileSystemLifeCycleState
    , _IncorrectFileSystemLifeCycleState

    -- ** BadRequest
    , _BadRequest

    -- ** NoFreeAddressesInSubnet
    , _NoFreeAddressesInSubnet

    -- ** DependencyTimeout
    , _DependencyTimeout

    -- ** FileSystemInUse
    , _FileSystemInUse

    -- ** IncorrectMountTargetState
    , _IncorrectMountTargetState

    -- ** InternalServerError
    , _InternalServerError

    -- ** IPAddressInUse
    , _IPAddressInUse

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeTags
    , module Network.AWS.EFS.DescribeTags

    -- ** DescribeMountTargets
    , module Network.AWS.EFS.DescribeMountTargets

    -- ** DeleteMountTarget
    , module Network.AWS.EFS.DeleteMountTarget

    -- ** CreateTags
    , module Network.AWS.EFS.CreateTags

    -- ** DescribeFileSystems
    , module Network.AWS.EFS.DescribeFileSystems

    -- ** DeleteTags
    , module Network.AWS.EFS.DeleteTags

    -- ** DescribeMountTargetSecurityGroups
    , module Network.AWS.EFS.DescribeMountTargetSecurityGroups

    -- ** ModifyMountTargetSecurityGroups
    , module Network.AWS.EFS.ModifyMountTargetSecurityGroups

    -- ** CreateFileSystem
    , module Network.AWS.EFS.CreateFileSystem

    -- ** DeleteFileSystem
    , module Network.AWS.EFS.DeleteFileSystem

    -- ** CreateMountTarget
    , module Network.AWS.EFS.CreateMountTarget

    -- * Types

    -- ** LifeCycleState
    , LifeCycleState (..)

    -- ** FileSystemDescription
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

    -- ** FileSystemSize
    , FileSystemSize
    , fileSystemSize
    , fssTimestamp
    , fssValue

    -- ** MountTargetDescription
    , MountTargetDescription
    , mountTargetDescription
    , mtdIPAddress
    , mtdNetworkInterfaceId
    , mtdOwnerId
    , mtdMountTargetId
    , mtdFileSystemId
    , mtdSubnetId
    , mtdLifeCycleState

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue
    ) where

import           Network.AWS.EFS.CreateFileSystem
import           Network.AWS.EFS.CreateMountTarget
import           Network.AWS.EFS.CreateTags
import           Network.AWS.EFS.DeleteFileSystem
import           Network.AWS.EFS.DeleteMountTarget
import           Network.AWS.EFS.DeleteTags
import           Network.AWS.EFS.DescribeFileSystems
import           Network.AWS.EFS.DescribeMountTargets
import           Network.AWS.EFS.DescribeMountTargetSecurityGroups
import           Network.AWS.EFS.DescribeTags
import           Network.AWS.EFS.ModifyMountTargetSecurityGroups
import           Network.AWS.EFS.Types
import           Network.AWS.EFS.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'EFS'.
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

{- $pager
This operation can return paginated results.
-}

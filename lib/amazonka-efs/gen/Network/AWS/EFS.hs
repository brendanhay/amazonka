{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Elastic File System__
--
-- Amazon Elastic File System (Amazon EFS) provides simple, scalable file storage for use with Amazon EC2 instances in the AWS Cloud. With Amazon EFS, storage capacity is elastic, growing and shrinking automatically as you add and remove files, so your applications have the storage they need, when they need it. For more information, see the <http://docs.aws.amazon.com/efs/latest/ug/api-reference.html User Guide> .
--
module Network.AWS.EFS
    (
    -- * Service Configuration
      efs

    -- * Errors
    -- $errors

    -- ** MountTargetNotFound
    , _MountTargetNotFound

    -- ** SecurityGroupLimitExceeded
    , _SecurityGroupLimitExceeded

    -- ** SecurityGroupNotFound
    , _SecurityGroupNotFound

    -- ** MountTargetConflict
    , _MountTargetConflict

    -- ** UnsupportedAvailabilityZone
    , _UnsupportedAvailabilityZone

    -- ** FileSystemLimitExceeded
    , _FileSystemLimitExceeded

    -- ** NetworkInterfaceLimitExceeded
    , _NetworkInterfaceLimitExceeded

    -- ** FileSystemAlreadyExists
    , _FileSystemAlreadyExists

    -- ** SubnetNotFound
    , _SubnetNotFound

    -- ** FileSystemNotFound
    , _FileSystemNotFound

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

    -- ** DescribeTags (Paginated)
    , module Network.AWS.EFS.DescribeTags

    -- ** DescribeMountTargets (Paginated)
    , module Network.AWS.EFS.DescribeMountTargets

    -- ** DescribeFileSystems (Paginated)
    , module Network.AWS.EFS.DescribeFileSystems

    -- ** DeleteMountTarget
    , module Network.AWS.EFS.DeleteMountTarget

    -- ** CreateTags
    , module Network.AWS.EFS.CreateTags

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

    -- ** PerformanceMode
    , PerformanceMode (..)

    -- ** FileSystemDescription
    , FileSystemDescription
    , fileSystemDescription
    , fsdEncrypted
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

import Network.AWS.EFS.CreateFileSystem
import Network.AWS.EFS.CreateMountTarget
import Network.AWS.EFS.CreateTags
import Network.AWS.EFS.DeleteFileSystem
import Network.AWS.EFS.DeleteMountTarget
import Network.AWS.EFS.DeleteTags
import Network.AWS.EFS.DescribeFileSystems
import Network.AWS.EFS.DescribeMountTargets
import Network.AWS.EFS.DescribeMountTargetSecurityGroups
import Network.AWS.EFS.DescribeTags
import Network.AWS.EFS.ModifyMountTargetSecurityGroups
import Network.AWS.EFS.Types
import Network.AWS.EFS.Waiters

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

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For more information about AWS CloudHSM, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> and the
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>.
module Network.AWS.CloudHSMv2
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** CloudHsmInternalFailureException
    _CloudHsmInternalFailureException,

    -- ** CloudHsmResourceNotFoundException
    _CloudHsmResourceNotFoundException,

    -- ** CloudHsmAccessDeniedException
    _CloudHsmAccessDeniedException,

    -- ** CloudHsmInvalidRequestException
    _CloudHsmInvalidRequestException,

    -- ** CloudHsmServiceException
    _CloudHsmServiceException,

    -- ** CloudHsmTagException
    _CloudHsmTagException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteHsm
    DeleteHsm (DeleteHsm'),
    newDeleteHsm,
    DeleteHsmResponse (DeleteHsmResponse'),
    newDeleteHsmResponse,

    -- ** DeleteBackup
    DeleteBackup (DeleteBackup'),
    newDeleteBackup,
    DeleteBackupResponse (DeleteBackupResponse'),
    newDeleteBackupResponse,

    -- ** DescribeClusters (Paginated)
    DescribeClusters (DescribeClusters'),
    newDescribeClusters,
    DescribeClustersResponse (DescribeClustersResponse'),
    newDescribeClustersResponse,

    -- ** RestoreBackup
    RestoreBackup (RestoreBackup'),
    newRestoreBackup,
    RestoreBackupResponse (RestoreBackupResponse'),
    newRestoreBackupResponse,

    -- ** CreateCluster
    CreateCluster (CreateCluster'),
    newCreateCluster,
    CreateClusterResponse (CreateClusterResponse'),
    newCreateClusterResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** CopyBackupToRegion
    CopyBackupToRegion (CopyBackupToRegion'),
    newCopyBackupToRegion,
    CopyBackupToRegionResponse (CopyBackupToRegionResponse'),
    newCopyBackupToRegionResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ModifyCluster
    ModifyCluster (ModifyCluster'),
    newModifyCluster,
    ModifyClusterResponse (ModifyClusterResponse'),
    newModifyClusterResponse,

    -- ** ModifyBackupAttributes
    ModifyBackupAttributes (ModifyBackupAttributes'),
    newModifyBackupAttributes,
    ModifyBackupAttributesResponse (ModifyBackupAttributesResponse'),
    newModifyBackupAttributesResponse,

    -- ** DeleteCluster
    DeleteCluster (DeleteCluster'),
    newDeleteCluster,
    DeleteClusterResponse (DeleteClusterResponse'),
    newDeleteClusterResponse,

    -- ** ListTags (Paginated)
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** DescribeBackups (Paginated)
    DescribeBackups (DescribeBackups'),
    newDescribeBackups,
    DescribeBackupsResponse (DescribeBackupsResponse'),
    newDescribeBackupsResponse,

    -- ** CreateHsm
    CreateHsm (CreateHsm'),
    newCreateHsm,
    CreateHsmResponse (CreateHsmResponse'),
    newCreateHsmResponse,

    -- ** InitializeCluster
    InitializeCluster (InitializeCluster'),
    newInitializeCluster,
    InitializeClusterResponse (InitializeClusterResponse'),
    newInitializeClusterResponse,

    -- * Types

    -- ** BackupPolicy
    BackupPolicy (..),

    -- ** BackupRetentionType
    BackupRetentionType (..),

    -- ** BackupState
    BackupState (..),

    -- ** ClusterState
    ClusterState (..),

    -- ** HsmState
    HsmState (..),

    -- ** Backup
    Backup (Backup'),
    newBackup,

    -- ** BackupRetentionPolicy
    BackupRetentionPolicy (BackupRetentionPolicy'),
    newBackupRetentionPolicy,

    -- ** Certificates
    Certificates (Certificates'),
    newCertificates,

    -- ** Cluster
    Cluster (Cluster'),
    newCluster,

    -- ** DestinationBackup
    DestinationBackup (DestinationBackup'),
    newDestinationBackup,

    -- ** Hsm
    Hsm (Hsm'),
    newHsm,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Network.AWS.CloudHSMv2.CopyBackupToRegion
import Network.AWS.CloudHSMv2.CreateCluster
import Network.AWS.CloudHSMv2.CreateHsm
import Network.AWS.CloudHSMv2.DeleteBackup
import Network.AWS.CloudHSMv2.DeleteCluster
import Network.AWS.CloudHSMv2.DeleteHsm
import Network.AWS.CloudHSMv2.DescribeBackups
import Network.AWS.CloudHSMv2.DescribeClusters
import Network.AWS.CloudHSMv2.InitializeCluster
import Network.AWS.CloudHSMv2.Lens
import Network.AWS.CloudHSMv2.ListTags
import Network.AWS.CloudHSMv2.ModifyBackupAttributes
import Network.AWS.CloudHSMv2.ModifyCluster
import Network.AWS.CloudHSMv2.RestoreBackup
import Network.AWS.CloudHSMv2.TagResource
import Network.AWS.CloudHSMv2.Types
import Network.AWS.CloudHSMv2.UntagResource
import Network.AWS.CloudHSMv2.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CloudHSMv2'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

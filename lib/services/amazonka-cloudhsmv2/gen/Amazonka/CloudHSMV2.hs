{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CloudHSMV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-04-28@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- For more information about AWS CloudHSM, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> and the
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>.
module Amazonka.CloudHSMV2
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** CloudHsmAccessDeniedException
    _CloudHsmAccessDeniedException,

    -- ** CloudHsmInternalFailureException
    _CloudHsmInternalFailureException,

    -- ** CloudHsmInvalidRequestException
    _CloudHsmInvalidRequestException,

    -- ** CloudHsmResourceNotFoundException
    _CloudHsmResourceNotFoundException,

    -- ** CloudHsmServiceException
    _CloudHsmServiceException,

    -- ** CloudHsmTagException
    _CloudHsmTagException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CopyBackupToRegion
    CopyBackupToRegion (CopyBackupToRegion'),
    newCopyBackupToRegion,
    CopyBackupToRegionResponse (CopyBackupToRegionResponse'),
    newCopyBackupToRegionResponse,

    -- ** CreateCluster
    CreateCluster (CreateCluster'),
    newCreateCluster,
    CreateClusterResponse (CreateClusterResponse'),
    newCreateClusterResponse,

    -- ** CreateHsm
    CreateHsm (CreateHsm'),
    newCreateHsm,
    CreateHsmResponse (CreateHsmResponse'),
    newCreateHsmResponse,

    -- ** DeleteBackup
    DeleteBackup (DeleteBackup'),
    newDeleteBackup,
    DeleteBackupResponse (DeleteBackupResponse'),
    newDeleteBackupResponse,

    -- ** DeleteCluster
    DeleteCluster (DeleteCluster'),
    newDeleteCluster,
    DeleteClusterResponse (DeleteClusterResponse'),
    newDeleteClusterResponse,

    -- ** DeleteHsm
    DeleteHsm (DeleteHsm'),
    newDeleteHsm,
    DeleteHsmResponse (DeleteHsmResponse'),
    newDeleteHsmResponse,

    -- ** DescribeBackups (Paginated)
    DescribeBackups (DescribeBackups'),
    newDescribeBackups,
    DescribeBackupsResponse (DescribeBackupsResponse'),
    newDescribeBackupsResponse,

    -- ** DescribeClusters (Paginated)
    DescribeClusters (DescribeClusters'),
    newDescribeClusters,
    DescribeClustersResponse (DescribeClustersResponse'),
    newDescribeClustersResponse,

    -- ** InitializeCluster
    InitializeCluster (InitializeCluster'),
    newInitializeCluster,
    InitializeClusterResponse (InitializeClusterResponse'),
    newInitializeClusterResponse,

    -- ** ListTags (Paginated)
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** ModifyBackupAttributes
    ModifyBackupAttributes (ModifyBackupAttributes'),
    newModifyBackupAttributes,
    ModifyBackupAttributesResponse (ModifyBackupAttributesResponse'),
    newModifyBackupAttributesResponse,

    -- ** ModifyCluster
    ModifyCluster (ModifyCluster'),
    newModifyCluster,
    ModifyClusterResponse (ModifyClusterResponse'),
    newModifyClusterResponse,

    -- ** RestoreBackup
    RestoreBackup (RestoreBackup'),
    newRestoreBackup,
    RestoreBackupResponse (RestoreBackupResponse'),
    newRestoreBackupResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

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

import Amazonka.CloudHSMV2.CopyBackupToRegion
import Amazonka.CloudHSMV2.CreateCluster
import Amazonka.CloudHSMV2.CreateHsm
import Amazonka.CloudHSMV2.DeleteBackup
import Amazonka.CloudHSMV2.DeleteCluster
import Amazonka.CloudHSMV2.DeleteHsm
import Amazonka.CloudHSMV2.DescribeBackups
import Amazonka.CloudHSMV2.DescribeClusters
import Amazonka.CloudHSMV2.InitializeCluster
import Amazonka.CloudHSMV2.Lens
import Amazonka.CloudHSMV2.ListTags
import Amazonka.CloudHSMV2.ModifyBackupAttributes
import Amazonka.CloudHSMV2.ModifyCluster
import Amazonka.CloudHSMV2.RestoreBackup
import Amazonka.CloudHSMV2.TagResource
import Amazonka.CloudHSMV2.Types
import Amazonka.CloudHSMV2.UntagResource
import Amazonka.CloudHSMV2.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CloudHSMV2'.

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

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For more information about AWS CloudHSM, see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> and the <https://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> .
module Network.AWS.CloudHSMv2
  ( -- * Service configuration
    cloudHSMv2Service,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeClusters (Paginated)
    module Network.AWS.CloudHSMv2.DescribeClusters,

    -- ** DeleteBackup
    module Network.AWS.CloudHSMv2.DeleteBackup,

    -- ** InitializeCluster
    module Network.AWS.CloudHSMv2.InitializeCluster,

    -- ** CreateHSM
    module Network.AWS.CloudHSMv2.CreateHSM,

    -- ** DescribeBackups (Paginated)
    module Network.AWS.CloudHSMv2.DescribeBackups,

    -- ** CopyBackupToRegion
    module Network.AWS.CloudHSMv2.CopyBackupToRegion,

    -- ** DeleteCluster
    module Network.AWS.CloudHSMv2.DeleteCluster,

    -- ** CreateCluster
    module Network.AWS.CloudHSMv2.CreateCluster,

    -- ** RestoreBackup
    module Network.AWS.CloudHSMv2.RestoreBackup,

    -- ** DeleteHSM
    module Network.AWS.CloudHSMv2.DeleteHSM,

    -- ** ModifyCluster
    module Network.AWS.CloudHSMv2.ModifyCluster,

    -- ** TagResource
    module Network.AWS.CloudHSMv2.TagResource,

    -- ** ListTags (Paginated)
    module Network.AWS.CloudHSMv2.ListTags,

    -- ** UntagResource
    module Network.AWS.CloudHSMv2.UntagResource,

    -- ** ModifyBackupAttributes
    module Network.AWS.CloudHSMv2.ModifyBackupAttributes,

    -- * Types

    -- ** BackupPolicy
    BackupPolicy (..),

    -- ** BackupRetentionType
    BackupRetentionType (..),

    -- ** BackupState
    BackupState (..),

    -- ** ClusterState
    ClusterState (..),

    -- ** HSMState
    HSMState (..),

    -- ** Backup
    Backup (..),
    mkBackup,
    bDeleteTimestamp,
    bSourceCluster,
    bNeverExpires,
    bSourceRegion,
    bTagList,
    bSourceBackup,
    bClusterId,
    bCreateTimestamp,
    bCopyTimestamp,
    bBackupState,
    bBackupId,

    -- ** BackupRetentionPolicy
    BackupRetentionPolicy (..),
    mkBackupRetentionPolicy,
    brpValue,
    brpType,

    -- ** Certificates
    Certificates (..),
    mkCertificates,
    cManufacturerHardwareCertificate,
    cClusterCSR,
    cHSMCertificate,
    cClusterCertificate,
    cAWSHardwareCertificate,

    -- ** Cluster
    Cluster (..),
    mkCluster,
    cPreCoPassword,
    cStateMessage,
    cState,
    cSubnetMapping,
    cBackupRetentionPolicy,
    cHSMs,
    cVPCId,
    cTagList,
    cSourceBackupId,
    cCertificates,
    cSecurityGroup,
    cClusterId,
    cCreateTimestamp,
    cBackupPolicy,
    cHSMType,

    -- ** DestinationBackup
    DestinationBackup (..),
    mkDestinationBackup,
    dbSourceCluster,
    dbSourceRegion,
    dbSourceBackup,
    dbCreateTimestamp,

    -- ** HSM
    HSM (..),
    mkHSM,
    hsmStateMessage,
    hsmState,
    hsmEniId,
    hsmSubnetId,
    hsmAvailabilityZone,
    hsmClusterId,
    hsmEniIP,
    hsmHSMId,

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.ISO8601,
    Lude.Timestamp,
    Lude.UTCTime,
  )
where

import Network.AWS.CloudHSMv2.CopyBackupToRegion
import Network.AWS.CloudHSMv2.CreateCluster
import Network.AWS.CloudHSMv2.CreateHSM
import Network.AWS.CloudHSMv2.DeleteBackup
import Network.AWS.CloudHSMv2.DeleteCluster
import Network.AWS.CloudHSMv2.DeleteHSM
import Network.AWS.CloudHSMv2.DescribeBackups
import Network.AWS.CloudHSMv2.DescribeClusters
import Network.AWS.CloudHSMv2.InitializeCluster
import Network.AWS.CloudHSMv2.ListTags
import Network.AWS.CloudHSMv2.ModifyBackupAttributes
import Network.AWS.CloudHSMv2.ModifyCluster
import Network.AWS.CloudHSMv2.RestoreBackup
import Network.AWS.CloudHSMv2.TagResource
import Network.AWS.CloudHSMv2.Types
import Network.AWS.CloudHSMv2.UntagResource
import Network.AWS.CloudHSMv2.Waiters
import qualified Network.AWS.Prelude as Lude

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

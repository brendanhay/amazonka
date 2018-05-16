{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For more information about AWS CloudHSM, see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> and the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> .
--
--
module Network.AWS.CloudHSMv2
    (
    -- * Service Configuration
      cloudHSMv2

    -- * Errors
    -- $errors

    -- ** CloudHSMInternalFailureException
    , _CloudHSMInternalFailureException

    -- ** CloudHSMServiceException
    , _CloudHSMServiceException

    -- ** CloudHSMInvalidRequestException
    , _CloudHSMInvalidRequestException

    -- ** CloudHSMAccessDeniedException
    , _CloudHSMAccessDeniedException

    -- ** CloudHSMResourceNotFoundException
    , _CloudHSMResourceNotFoundException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeClusters (Paginated)
    , module Network.AWS.CloudHSMv2.DescribeClusters

    -- ** InitializeCluster
    , module Network.AWS.CloudHSMv2.InitializeCluster

    -- ** CreateHSM
    , module Network.AWS.CloudHSMv2.CreateHSM

    -- ** DescribeBackups (Paginated)
    , module Network.AWS.CloudHSMv2.DescribeBackups

    -- ** DeleteCluster
    , module Network.AWS.CloudHSMv2.DeleteCluster

    -- ** CreateCluster
    , module Network.AWS.CloudHSMv2.CreateCluster

    -- ** DeleteHSM
    , module Network.AWS.CloudHSMv2.DeleteHSM

    -- ** TagResource
    , module Network.AWS.CloudHSMv2.TagResource

    -- ** ListTags (Paginated)
    , module Network.AWS.CloudHSMv2.ListTags

    -- ** UntagResource
    , module Network.AWS.CloudHSMv2.UntagResource

    -- * Types

    -- ** BackupPolicy
    , BackupPolicy (..)

    -- ** BackupState
    , BackupState (..)

    -- ** ClusterState
    , ClusterState (..)

    -- ** HSMState
    , HSMState (..)

    -- ** Backup
    , Backup
    , backup
    , bClusterId
    , bCreateTimestamp
    , bBackupState
    , bBackupId

    -- ** Certificates
    , Certificates
    , certificates
    , cManufacturerHardwareCertificate
    , cClusterCSR
    , cHSMCertificate
    , cClusterCertificate
    , cAWSHardwareCertificate

    -- ** Cluster
    , Cluster
    , cluster
    , cPreCoPassword
    , cStateMessage
    , cState
    , cSubnetMapping
    , cHSMs
    , cVPCId
    , cSourceBackupId
    , cCertificates
    , cSecurityGroup
    , cClusterId
    , cCreateTimestamp
    , cBackupPolicy
    , cHSMType

    -- ** HSM
    , HSM
    , hsm
    , hsmStateMessage
    , hsmState
    , hsmEniId
    , hsmSubnetId
    , hsmAvailabilityZone
    , hsmClusterId
    , hsmEniIP
    , hsmHSMId

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue
    ) where

import Network.AWS.CloudHSMv2.CreateCluster
import Network.AWS.CloudHSMv2.CreateHSM
import Network.AWS.CloudHSMv2.DeleteCluster
import Network.AWS.CloudHSMv2.DeleteHSM
import Network.AWS.CloudHSMv2.DescribeBackups
import Network.AWS.CloudHSMv2.DescribeClusters
import Network.AWS.CloudHSMv2.InitializeCluster
import Network.AWS.CloudHSMv2.ListTags
import Network.AWS.CloudHSMv2.TagResource
import Network.AWS.CloudHSMv2.Types
import Network.AWS.CloudHSMv2.UntagResource
import Network.AWS.CloudHSMv2.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CloudHSMv2'.
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

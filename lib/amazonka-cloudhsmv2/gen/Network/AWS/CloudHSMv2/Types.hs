-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types
  ( -- * Service configuration
    cloudHSMv2Service,

    -- * Errors

    -- * BackupPolicy
    BackupPolicy (..),

    -- * BackupRetentionType
    BackupRetentionType (..),

    -- * BackupState
    BackupState (..),

    -- * ClusterState
    ClusterState (..),

    -- * HSMState
    HSMState (..),

    -- * Backup
    Backup (..),
    mkBackup,
    bDeleteTimestamp,
    bSourceCluster,
    bNeverExpires,
    bSourceRegion,
    bTagList,
    bBackupId,
    bSourceBackup,
    bClusterId,
    bCreateTimestamp,
    bCopyTimestamp,
    bBackupState,

    -- * BackupRetentionPolicy
    BackupRetentionPolicy (..),
    mkBackupRetentionPolicy,
    brpValue,
    brpType,

    -- * Certificates
    Certificates (..),
    mkCertificates,
    cManufacturerHardwareCertificate,
    cClusterCSR,
    cHSMCertificate,
    cClusterCertificate,
    cAWSHardwareCertificate,

    -- * Cluster
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

    -- * DestinationBackup
    DestinationBackup (..),
    mkDestinationBackup,
    dbSourceCluster,
    dbSourceRegion,
    dbSourceBackup,
    dbCreateTimestamp,

    -- * HSM
    HSM (..),
    mkHSM,
    hsmStateMessage,
    hsmState,
    hsmEniId,
    hsmHSMId,
    hsmSubnetId,
    hsmAvailabilityZone,
    hsmClusterId,
    hsmEniIP,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,
  )
where

import Network.AWS.CloudHSMv2.Types.Backup
import Network.AWS.CloudHSMv2.Types.BackupPolicy
import Network.AWS.CloudHSMv2.Types.BackupRetentionPolicy
import Network.AWS.CloudHSMv2.Types.BackupRetentionType
import Network.AWS.CloudHSMv2.Types.BackupState
import Network.AWS.CloudHSMv2.Types.Certificates
import Network.AWS.CloudHSMv2.Types.Cluster
import Network.AWS.CloudHSMv2.Types.ClusterState
import Network.AWS.CloudHSMv2.Types.DestinationBackup
import Network.AWS.CloudHSMv2.Types.HSM
import Network.AWS.CloudHSMv2.Types.HSMState
import Network.AWS.CloudHSMv2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-04-28@ of the Amazon CloudHSM V2 SDK configuration.
cloudHSMv2Service :: Lude.Service
cloudHSMv2Service =
  Lude.Service
    { Lude._svcAbbrev = "CloudHSMv2",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "cloudhsmv2",
      Lude._svcVersion = "2017-04-28",
      Lude._svcEndpoint = Lude.defaultEndpoint cloudHSMv2Service,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "CloudHSMv2",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing

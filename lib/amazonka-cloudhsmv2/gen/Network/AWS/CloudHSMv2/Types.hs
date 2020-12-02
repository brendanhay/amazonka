{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types
  ( -- * Service Configuration
    cloudHSMv2,

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
    Backup,
    backup,
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

    -- * BackupRetentionPolicy
    BackupRetentionPolicy,
    backupRetentionPolicy,
    brpValue,
    brpType,

    -- * Certificates
    Certificates,
    certificates,
    cManufacturerHardwareCertificate,
    cClusterCSR,
    cHSMCertificate,
    cClusterCertificate,
    cAWSHardwareCertificate,

    -- * Cluster
    Cluster,
    cluster,
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
    DestinationBackup,
    destinationBackup,
    dbSourceCluster,
    dbSourceRegion,
    dbSourceBackup,
    dbCreateTimestamp,

    -- * HSM
    HSM,
    hsm,
    hsmStateMessage,
    hsmState,
    hsmEniId,
    hsmSubnetId,
    hsmAvailabilityZone,
    hsmClusterId,
    hsmEniIP,
    hsmHSMId,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-04-28@ of the Amazon CloudHSM V2 SDK configuration.
cloudHSMv2 :: Service
cloudHSMv2 =
  Service
    { _svcAbbrev = "CloudHSMv2",
      _svcSigner = v4,
      _svcPrefix = "cloudhsmv2",
      _svcVersion = "2017-04-28",
      _svcEndpoint = defaultEndpoint cloudHSMv2,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "CloudHSMv2",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _CloudHsmInternalFailureException,
    _CloudHsmServiceException,
    _CloudHsmInvalidRequestException,
    _CloudHsmAccessDeniedException,
    _CloudHsmResourceNotFoundException,
    _CloudHsmTagException,

    -- * BackupPolicy
    BackupPolicy (..),

    -- * BackupRetentionType
    BackupRetentionType (..),

    -- * BackupState
    BackupState (..),

    -- * ClusterState
    ClusterState (..),

    -- * HsmState
    HsmState (..),

    -- * Backup
    Backup (..),
    newBackup,
    backup_deleteTimestamp,
    backup_sourceCluster,
    backup_neverExpires,
    backup_sourceRegion,
    backup_tagList,
    backup_sourceBackup,
    backup_clusterId,
    backup_createTimestamp,
    backup_copyTimestamp,
    backup_backupState,
    backup_backupId,

    -- * BackupRetentionPolicy
    BackupRetentionPolicy (..),
    newBackupRetentionPolicy,
    backupRetentionPolicy_value,
    backupRetentionPolicy_type,

    -- * Certificates
    Certificates (..),
    newCertificates,
    certificates_manufacturerHardwareCertificate,
    certificates_clusterCsr,
    certificates_hsmCertificate,
    certificates_clusterCertificate,
    certificates_awsHardwareCertificate,

    -- * Cluster
    Cluster (..),
    newCluster,
    cluster_preCoPassword,
    cluster_stateMessage,
    cluster_state,
    cluster_subnetMapping,
    cluster_backupRetentionPolicy,
    cluster_hsms,
    cluster_vpcId,
    cluster_tagList,
    cluster_sourceBackupId,
    cluster_certificates,
    cluster_securityGroup,
    cluster_clusterId,
    cluster_createTimestamp,
    cluster_backupPolicy,
    cluster_hsmType,

    -- * DestinationBackup
    DestinationBackup (..),
    newDestinationBackup,
    destinationBackup_sourceCluster,
    destinationBackup_sourceRegion,
    destinationBackup_sourceBackup,
    destinationBackup_createTimestamp,

    -- * Hsm
    Hsm (..),
    newHsm,
    hsm_stateMessage,
    hsm_state,
    hsm_eniId,
    hsm_subnetId,
    hsm_availabilityZone,
    hsm_clusterId,
    hsm_eniIp,
    hsm_hsmId,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
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
import Network.AWS.CloudHSMv2.Types.Hsm
import Network.AWS.CloudHSMv2.Types.HsmState
import Network.AWS.CloudHSMv2.Types.Tag
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-04-28@ of the Amazon CloudHSM V2 SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "CloudHSMv2",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "cloudhsmv2",
      Core._serviceSigningName = "cloudhsm",
      Core._serviceVersion = "2017-04-28",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "CloudHSMv2",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The request was rejected because of an AWS CloudHSM internal failure.
-- The request can be retried.
_CloudHsmInternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CloudHsmInternalFailureException =
  Core._MatchServiceError
    defaultService
    "CloudHsmInternalFailureException"

-- | The request was rejected because an error occurred.
_CloudHsmServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CloudHsmServiceException =
  Core._MatchServiceError
    defaultService
    "CloudHsmServiceException"

-- | The request was rejected because it is not a valid request.
_CloudHsmInvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CloudHsmInvalidRequestException =
  Core._MatchServiceError
    defaultService
    "CloudHsmInvalidRequestException"

-- | The request was rejected because the requester does not have permission
-- to perform the requested operation.
_CloudHsmAccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CloudHsmAccessDeniedException =
  Core._MatchServiceError
    defaultService
    "CloudHsmAccessDeniedException"

-- | The request was rejected because it refers to a resource that cannot be
-- found.
_CloudHsmResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CloudHsmResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "CloudHsmResourceNotFoundException"

-- | The request was rejected because of a tagging failure. Verify the tag
-- conditions in all applicable policies, and then retry the request.
_CloudHsmTagException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CloudHsmTagException =
  Core._MatchServiceError
    defaultService
    "CloudHsmTagException"

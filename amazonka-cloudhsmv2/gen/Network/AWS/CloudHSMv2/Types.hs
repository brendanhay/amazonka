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
    _CloudHsmResourceNotFoundException,
    _CloudHsmAccessDeniedException,
    _CloudHsmInvalidRequestException,
    _CloudHsmServiceException,
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
    backup_clusterId,
    backup_backupState,
    backup_sourceBackup,
    backup_copyTimestamp,
    backup_createTimestamp,
    backup_neverExpires,
    backup_sourceCluster,
    backup_deleteTimestamp,
    backup_tagList,
    backup_sourceRegion,
    backup_backupId,

    -- * BackupRetentionPolicy
    BackupRetentionPolicy (..),
    newBackupRetentionPolicy,
    backupRetentionPolicy_value,
    backupRetentionPolicy_type,

    -- * Certificates
    Certificates (..),
    newCertificates,
    certificates_awsHardwareCertificate,
    certificates_hsmCertificate,
    certificates_clusterCsr,
    certificates_clusterCertificate,
    certificates_manufacturerHardwareCertificate,

    -- * Cluster
    Cluster (..),
    newCluster,
    cluster_clusterId,
    cluster_stateMessage,
    cluster_backupPolicy,
    cluster_createTimestamp,
    cluster_subnetMapping,
    cluster_state,
    cluster_preCoPassword,
    cluster_securityGroup,
    cluster_hsmType,
    cluster_sourceBackupId,
    cluster_certificates,
    cluster_tagList,
    cluster_vpcId,
    cluster_hsms,
    cluster_backupRetentionPolicy,

    -- * DestinationBackup
    DestinationBackup (..),
    newDestinationBackup,
    destinationBackup_sourceBackup,
    destinationBackup_createTimestamp,
    destinationBackup_sourceCluster,
    destinationBackup_sourceRegion,

    -- * Hsm
    Hsm (..),
    newHsm,
    hsm_clusterId,
    hsm_stateMessage,
    hsm_eniIp,
    hsm_eniId,
    hsm_state,
    hsm_availabilityZone,
    hsm_subnetId,
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
      Core._serviceTimeout = Core.Just 70,
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
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | The request was rejected because of an AWS CloudHSM internal failure.
-- The request can be retried.
_CloudHsmInternalFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudHsmInternalFailureException =
  Core._MatchServiceError
    defaultService
    "CloudHsmInternalFailureException"

-- | The request was rejected because it refers to a resource that cannot be
-- found.
_CloudHsmResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudHsmResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "CloudHsmResourceNotFoundException"

-- | The request was rejected because the requester does not have permission
-- to perform the requested operation.
_CloudHsmAccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudHsmAccessDeniedException =
  Core._MatchServiceError
    defaultService
    "CloudHsmAccessDeniedException"

-- | The request was rejected because it is not a valid request.
_CloudHsmInvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudHsmInvalidRequestException =
  Core._MatchServiceError
    defaultService
    "CloudHsmInvalidRequestException"

-- | The request was rejected because an error occurred.
_CloudHsmServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudHsmServiceException =
  Core._MatchServiceError
    defaultService
    "CloudHsmServiceException"

-- | The request was rejected because of a tagging failure. Verify the tag
-- conditions in all applicable policies, and then retry the request.
_CloudHsmTagException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudHsmTagException =
  Core._MatchServiceError
    defaultService
    "CloudHsmTagException"

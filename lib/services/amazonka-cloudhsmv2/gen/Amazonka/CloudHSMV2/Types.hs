{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudHSMV2.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudHSMV2.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _CloudHsmResourceNotFoundException,
    _CloudHsmAccessDeniedException,
    _CloudHsmInvalidRequestException,
    _CloudHsmServiceException,
    _CloudHsmTagException,
    _CloudHsmInternalFailureException,

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
    backup_neverExpires,
    backup_sourceCluster,
    backup_sourceRegion,
    backup_deleteTimestamp,
    backup_createTimestamp,
    backup_tagList,
    backup_clusterId,
    backup_sourceBackup,
    backup_backupState,
    backup_copyTimestamp,
    backup_backupId,

    -- * BackupRetentionPolicy
    BackupRetentionPolicy (..),
    newBackupRetentionPolicy,
    backupRetentionPolicy_type,
    backupRetentionPolicy_value,

    -- * Certificates
    Certificates (..),
    newCertificates,
    certificates_hsmCertificate,
    certificates_clusterCertificate,
    certificates_clusterCsr,
    certificates_manufacturerHardwareCertificate,
    certificates_awsHardwareCertificate,

    -- * Cluster
    Cluster (..),
    newCluster,
    cluster_subnetMapping,
    cluster_securityGroup,
    cluster_createTimestamp,
    cluster_tagList,
    cluster_hsmType,
    cluster_state,
    cluster_backupRetentionPolicy,
    cluster_hsms,
    cluster_certificates,
    cluster_preCoPassword,
    cluster_backupPolicy,
    cluster_clusterId,
    cluster_stateMessage,
    cluster_vpcId,
    cluster_sourceBackupId,

    -- * DestinationBackup
    DestinationBackup (..),
    newDestinationBackup,
    destinationBackup_sourceCluster,
    destinationBackup_sourceRegion,
    destinationBackup_createTimestamp,
    destinationBackup_sourceBackup,

    -- * Hsm
    Hsm (..),
    newHsm,
    hsm_subnetId,
    hsm_state,
    hsm_availabilityZone,
    hsm_clusterId,
    hsm_stateMessage,
    hsm_eniIp,
    hsm_eniId,
    hsm_hsmId,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import Amazonka.CloudHSMV2.Types.Backup
import Amazonka.CloudHSMV2.Types.BackupPolicy
import Amazonka.CloudHSMV2.Types.BackupRetentionPolicy
import Amazonka.CloudHSMV2.Types.BackupRetentionType
import Amazonka.CloudHSMV2.Types.BackupState
import Amazonka.CloudHSMV2.Types.Certificates
import Amazonka.CloudHSMV2.Types.Cluster
import Amazonka.CloudHSMV2.Types.ClusterState
import Amazonka.CloudHSMV2.Types.DestinationBackup
import Amazonka.CloudHSMV2.Types.Hsm
import Amazonka.CloudHSMV2.Types.HsmState
import Amazonka.CloudHSMV2.Types.Tag
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-04-28@ of the Amazon CloudHSM V2 SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CloudHSMV2",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "cloudhsmv2",
      Core.signingName = "cloudhsm",
      Core.version = "2017-04-28",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "CloudHSMV2",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The request was rejected because it refers to a resource that cannot be
-- found.
_CloudHsmResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CloudHsmResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "CloudHsmResourceNotFoundException"

-- | The request was rejected because the requester does not have permission
-- to perform the requested operation.
_CloudHsmAccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CloudHsmAccessDeniedException =
  Core._MatchServiceError
    defaultService
    "CloudHsmAccessDeniedException"

-- | The request was rejected because it is not a valid request.
_CloudHsmInvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CloudHsmInvalidRequestException =
  Core._MatchServiceError
    defaultService
    "CloudHsmInvalidRequestException"

-- | The request was rejected because an error occurred.
_CloudHsmServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CloudHsmServiceException =
  Core._MatchServiceError
    defaultService
    "CloudHsmServiceException"

-- | The request was rejected because of a tagging failure. Verify the tag
-- conditions in all applicable policies, and then retry the request.
_CloudHsmTagException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CloudHsmTagException =
  Core._MatchServiceError
    defaultService
    "CloudHsmTagException"

-- | The request was rejected because of an AWS CloudHSM internal failure.
-- The request can be retried.
_CloudHsmInternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CloudHsmInternalFailureException =
  Core._MatchServiceError
    defaultService
    "CloudHsmInternalFailureException"

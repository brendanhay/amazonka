{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DocDbElastic.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocDbElastic.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * Auth
    Auth (..),

    -- * Status
    Status (..),

    -- * Cluster
    Cluster (..),
    newCluster,
    cluster_adminUserName,
    cluster_authType,
    cluster_clusterArn,
    cluster_clusterEndpoint,
    cluster_clusterName,
    cluster_createTime,
    cluster_kmsKeyId,
    cluster_preferredMaintenanceWindow,
    cluster_shardCapacity,
    cluster_shardCount,
    cluster_status,
    cluster_subnetIds,
    cluster_vpcSecurityGroupIds,

    -- * ClusterInList
    ClusterInList (..),
    newClusterInList,
    clusterInList_clusterArn,
    clusterInList_clusterName,
    clusterInList_status,

    -- * ClusterSnapshot
    ClusterSnapshot (..),
    newClusterSnapshot,
    clusterSnapshot_adminUserName,
    clusterSnapshot_clusterArn,
    clusterSnapshot_clusterCreationTime,
    clusterSnapshot_kmsKeyId,
    clusterSnapshot_snapshotArn,
    clusterSnapshot_snapshotCreationTime,
    clusterSnapshot_snapshotName,
    clusterSnapshot_status,
    clusterSnapshot_subnetIds,
    clusterSnapshot_vpcSecurityGroupIds,

    -- * ClusterSnapshotInList
    ClusterSnapshotInList (..),
    newClusterSnapshotInList,
    clusterSnapshotInList_clusterArn,
    clusterSnapshotInList_snapshotArn,
    clusterSnapshotInList_snapshotCreationTime,
    clusterSnapshotInList_snapshotName,
    clusterSnapshotInList_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DocDbElastic.Types.Auth
import Amazonka.DocDbElastic.Types.Cluster
import Amazonka.DocDbElastic.Types.ClusterInList
import Amazonka.DocDbElastic.Types.ClusterSnapshot
import Amazonka.DocDbElastic.Types.ClusterSnapshotInList
import Amazonka.DocDbElastic.Types.Status
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2022-11-28@ of the Amazon DocumentDB Elastic Clusters SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "DocDbElastic",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "docdb-elastic",
      Core.signingName = "docdb-elastic",
      Core.version = "2022-11-28",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "DocDbElastic",
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
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | An exception that occurs when there are not sufficient permissions to
-- perform an action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | There was an access conflict.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | There was an internal server error.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The specified resource could not be located.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The service quota for the action was exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | ThrottlingException will be thrown when request was denied due to
-- request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | A structure defining a validation exception.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

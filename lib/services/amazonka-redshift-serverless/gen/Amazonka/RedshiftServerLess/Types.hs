{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RedshiftServerLess.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftServerLess.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InsufficientCapacityException,
    _InternalServerException,
    _InvalidPaginationException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _TooManyTagsException,
    _ValidationException,

    -- * LogExport
    LogExport (..),

    -- * NamespaceStatus
    NamespaceStatus (..),

    -- * SnapshotStatus
    SnapshotStatus (..),

    -- * UsageLimitBreachAction
    UsageLimitBreachAction (..),

    -- * UsageLimitPeriod
    UsageLimitPeriod (..),

    -- * UsageLimitUsageType
    UsageLimitUsageType (..),

    -- * WorkgroupStatus
    WorkgroupStatus (..),

    -- * ConfigParameter
    ConfigParameter (..),
    newConfigParameter,
    configParameter_parameterKey,
    configParameter_parameterValue,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_address,
    endpoint_port,
    endpoint_vpcEndpoints,

    -- * EndpointAccess
    EndpointAccess (..),
    newEndpointAccess,
    endpointAccess_address,
    endpointAccess_endpointArn,
    endpointAccess_endpointCreateTime,
    endpointAccess_endpointName,
    endpointAccess_endpointStatus,
    endpointAccess_port,
    endpointAccess_subnetIds,
    endpointAccess_vpcEndpoint,
    endpointAccess_vpcSecurityGroups,
    endpointAccess_workgroupName,

    -- * Namespace
    Namespace (..),
    newNamespace,
    namespace_adminUsername,
    namespace_creationDate,
    namespace_dbName,
    namespace_defaultIamRoleArn,
    namespace_iamRoles,
    namespace_kmsKeyId,
    namespace_logExports,
    namespace_namespaceArn,
    namespace_namespaceId,
    namespace_namespaceName,
    namespace_status,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_availabilityZone,
    networkInterface_networkInterfaceId,
    networkInterface_privateIpAddress,
    networkInterface_subnetId,

    -- * RecoveryPoint
    RecoveryPoint (..),
    newRecoveryPoint,
    recoveryPoint_namespaceArn,
    recoveryPoint_namespaceName,
    recoveryPoint_recoveryPointCreateTime,
    recoveryPoint_recoveryPointId,
    recoveryPoint_totalSizeInMegaBytes,
    recoveryPoint_workgroupName,

    -- * ResourcePolicy
    ResourcePolicy (..),
    newResourcePolicy,
    resourcePolicy_policy,
    resourcePolicy_resourceArn,

    -- * Snapshot
    Snapshot (..),
    newSnapshot,
    snapshot_accountsWithProvisionedRestoreAccess,
    snapshot_accountsWithRestoreAccess,
    snapshot_actualIncrementalBackupSizeInMegaBytes,
    snapshot_adminUsername,
    snapshot_backupProgressInMegaBytes,
    snapshot_currentBackupRateInMegaBytesPerSecond,
    snapshot_elapsedTimeInSeconds,
    snapshot_estimatedSecondsToCompletion,
    snapshot_kmsKeyId,
    snapshot_namespaceArn,
    snapshot_namespaceName,
    snapshot_ownerAccount,
    snapshot_snapshotArn,
    snapshot_snapshotCreateTime,
    snapshot_snapshotName,
    snapshot_snapshotRemainingDays,
    snapshot_snapshotRetentionPeriod,
    snapshot_snapshotRetentionStartTime,
    snapshot_status,
    snapshot_totalBackupSizeInMegaBytes,

    -- * TableRestoreStatus
    TableRestoreStatus (..),
    newTableRestoreStatus,
    tableRestoreStatus_message,
    tableRestoreStatus_namespaceName,
    tableRestoreStatus_newTableName,
    tableRestoreStatus_progressInMegaBytes,
    tableRestoreStatus_requestTime,
    tableRestoreStatus_snapshotName,
    tableRestoreStatus_sourceDatabaseName,
    tableRestoreStatus_sourceSchemaName,
    tableRestoreStatus_sourceTableName,
    tableRestoreStatus_status,
    tableRestoreStatus_tableRestoreRequestId,
    tableRestoreStatus_targetDatabaseName,
    tableRestoreStatus_targetSchemaName,
    tableRestoreStatus_totalDataInMegaBytes,
    tableRestoreStatus_workgroupName,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * UsageLimit
    UsageLimit (..),
    newUsageLimit,
    usageLimit_amount,
    usageLimit_breachAction,
    usageLimit_period,
    usageLimit_resourceArn,
    usageLimit_usageLimitArn,
    usageLimit_usageLimitId,
    usageLimit_usageType,

    -- * VpcEndpoint
    VpcEndpoint (..),
    newVpcEndpoint,
    vpcEndpoint_networkInterfaces,
    vpcEndpoint_vpcEndpointId,
    vpcEndpoint_vpcId,

    -- * VpcSecurityGroupMembership
    VpcSecurityGroupMembership (..),
    newVpcSecurityGroupMembership,
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,

    -- * Workgroup
    Workgroup (..),
    newWorkgroup,
    workgroup_baseCapacity,
    workgroup_configParameters,
    workgroup_creationDate,
    workgroup_endpoint,
    workgroup_enhancedVpcRouting,
    workgroup_namespaceName,
    workgroup_port,
    workgroup_publiclyAccessible,
    workgroup_securityGroupIds,
    workgroup_status,
    workgroup_subnetIds,
    workgroup_workgroupArn,
    workgroup_workgroupId,
    workgroup_workgroupName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types.ConfigParameter
import Amazonka.RedshiftServerLess.Types.Endpoint
import Amazonka.RedshiftServerLess.Types.EndpointAccess
import Amazonka.RedshiftServerLess.Types.LogExport
import Amazonka.RedshiftServerLess.Types.Namespace
import Amazonka.RedshiftServerLess.Types.NamespaceStatus
import Amazonka.RedshiftServerLess.Types.NetworkInterface
import Amazonka.RedshiftServerLess.Types.RecoveryPoint
import Amazonka.RedshiftServerLess.Types.ResourcePolicy
import Amazonka.RedshiftServerLess.Types.Snapshot
import Amazonka.RedshiftServerLess.Types.SnapshotStatus
import Amazonka.RedshiftServerLess.Types.TableRestoreStatus
import Amazonka.RedshiftServerLess.Types.Tag
import Amazonka.RedshiftServerLess.Types.UsageLimit
import Amazonka.RedshiftServerLess.Types.UsageLimitBreachAction
import Amazonka.RedshiftServerLess.Types.UsageLimitPeriod
import Amazonka.RedshiftServerLess.Types.UsageLimitUsageType
import Amazonka.RedshiftServerLess.Types.VpcEndpoint
import Amazonka.RedshiftServerLess.Types.VpcSecurityGroupMembership
import Amazonka.RedshiftServerLess.Types.Workgroup
import Amazonka.RedshiftServerLess.Types.WorkgroupStatus
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-04-21@ of the Amazon Redshift Serverless SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "RedshiftServerLess",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "redshift-serverless",
      Core.signingName = "redshift-serverless",
      Core.version = "2021-04-21",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "RedshiftServerLess",
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

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The submitted action has conflicts.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | There is an insufficient capacity to perform the action.
_InsufficientCapacityException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientCapacityException =
  Core._MatchServiceError
    defaultService
    "InsufficientCapacityException"

-- | The request processing has failed because of an unknown error, exception
-- or failure.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The provided pagination token is invalid.
_InvalidPaginationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidPaginationException =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationException"

-- | The resource could not be found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The service limit was exceeded.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | The request was denied due to request throttling.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The request exceeded the number of tags allowed for a resource.
_TooManyTagsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The input failed to satisfy the constraints specified by an AWS service.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

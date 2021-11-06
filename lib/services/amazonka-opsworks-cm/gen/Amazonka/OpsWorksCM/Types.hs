{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpsWorksCM.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorksCM.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _ResourceAlreadyExistsException,
    _InvalidNextTokenException,
    _ResourceNotFoundException,
    _InvalidStateException,
    _LimitExceededException,

    -- * BackupStatus
    BackupStatus (..),

    -- * BackupType
    BackupType (..),

    -- * MaintenanceStatus
    MaintenanceStatus (..),

    -- * NodeAssociationStatus
    NodeAssociationStatus (..),

    -- * ServerStatus
    ServerStatus (..),

    -- * AccountAttribute
    AccountAttribute (..),
    newAccountAttribute,
    accountAttribute_used,
    accountAttribute_maximum,
    accountAttribute_name,

    -- * Backup
    Backup (..),
    newBackup,
    backup_engineVersion,
    backup_serviceRoleArn,
    backup_status,
    backup_instanceProfileArn,
    backup_securityGroupIds,
    backup_statusDescription,
    backup_serverName,
    backup_subnetIds,
    backup_keyPair,
    backup_createdAt,
    backup_backupId,
    backup_engine,
    backup_instanceType,
    backup_engineModel,
    backup_preferredMaintenanceWindow,
    backup_userArn,
    backup_preferredBackupWindow,
    backup_s3LogUrl,
    backup_s3DataSize,
    backup_backupArn,
    backup_s3DataUrl,
    backup_description,
    backup_backupType,
    backup_toolsVersion,

    -- * EngineAttribute
    EngineAttribute (..),
    newEngineAttribute,
    engineAttribute_value,
    engineAttribute_name,

    -- * Server
    Server (..),
    newServer,
    server_engineVersion,
    server_serviceRoleArn,
    server_disableAutomatedBackup,
    server_status,
    server_instanceProfileArn,
    server_securityGroupIds,
    server_associatePublicIpAddress,
    server_serverName,
    server_subnetIds,
    server_keyPair,
    server_createdAt,
    server_serverArn,
    server_customDomain,
    server_engine,
    server_maintenanceStatus,
    server_instanceType,
    server_engineModel,
    server_engineAttributes,
    server_preferredMaintenanceWindow,
    server_preferredBackupWindow,
    server_statusReason,
    server_endpoint,
    server_cloudFormationStackArn,
    server_backupRetentionCount,

    -- * ServerEvent
    ServerEvent (..),
    newServerEvent,
    serverEvent_logUrl,
    serverEvent_serverName,
    serverEvent_createdAt,
    serverEvent_message,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpsWorksCM.Types.AccountAttribute
import Amazonka.OpsWorksCM.Types.Backup
import Amazonka.OpsWorksCM.Types.BackupStatus
import Amazonka.OpsWorksCM.Types.BackupType
import Amazonka.OpsWorksCM.Types.EngineAttribute
import Amazonka.OpsWorksCM.Types.MaintenanceStatus
import Amazonka.OpsWorksCM.Types.NodeAssociationStatus
import Amazonka.OpsWorksCM.Types.Server
import Amazonka.OpsWorksCM.Types.ServerEvent
import Amazonka.OpsWorksCM.Types.ServerStatus
import Amazonka.OpsWorksCM.Types.Tag
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2016-11-01@ of the Amazon OpsWorks CM SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "OpsWorksCM",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "opsworks-cm",
      Core._serviceSigningName = "opsworks-cm",
      Core._serviceVersion = "2016-11-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "OpsWorksCM",
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

-- | One or more of the provided request parameters are not valid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | The requested resource cannot be created because it already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | This occurs when the provided nextToken is not valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | The requested resource does not exist, or access was denied.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The resource is in a state that does not allow you to perform a
-- specified action.
_InvalidStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidStateException =
  Core._MatchServiceError
    defaultService
    "InvalidStateException"

-- | The limit of servers or backups has been reached.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

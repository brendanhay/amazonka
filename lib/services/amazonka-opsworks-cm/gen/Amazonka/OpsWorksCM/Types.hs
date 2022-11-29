{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpsWorksCM.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorksCM.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceAlreadyExistsException,
    _InvalidStateException,
    _ResourceNotFoundException,
    _LimitExceededException,
    _InvalidNextTokenException,
    _ValidationException,

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
    accountAttribute_name,
    accountAttribute_used,
    accountAttribute_maximum,

    -- * Backup
    Backup (..),
    newBackup,
    backup_statusDescription,
    backup_backupId,
    backup_preferredBackupWindow,
    backup_s3DataUrl,
    backup_serviceRoleArn,
    backup_securityGroupIds,
    backup_s3LogUrl,
    backup_serverName,
    backup_engineModel,
    backup_toolsVersion,
    backup_instanceProfileArn,
    backup_status,
    backup_description,
    backup_keyPair,
    backup_instanceType,
    backup_backupType,
    backup_backupArn,
    backup_userArn,
    backup_s3DataSize,
    backup_engine,
    backup_preferredMaintenanceWindow,
    backup_subnetIds,
    backup_createdAt,
    backup_engineVersion,

    -- * EngineAttribute
    EngineAttribute (..),
    newEngineAttribute,
    engineAttribute_name,
    engineAttribute_value,

    -- * Server
    Server (..),
    newServer,
    server_maintenanceStatus,
    server_preferredBackupWindow,
    server_associatePublicIpAddress,
    server_serviceRoleArn,
    server_securityGroupIds,
    server_serverName,
    server_engineModel,
    server_instanceProfileArn,
    server_statusReason,
    server_engineAttributes,
    server_status,
    server_cloudFormationStackArn,
    server_keyPair,
    server_backupRetentionCount,
    server_instanceType,
    server_serverArn,
    server_engine,
    server_preferredMaintenanceWindow,
    server_endpoint,
    server_subnetIds,
    server_createdAt,
    server_disableAutomatedBackup,
    server_engineVersion,
    server_customDomain,

    -- * ServerEvent
    ServerEvent (..),
    newServerEvent,
    serverEvent_message,
    serverEvent_serverName,
    serverEvent_logUrl,
    serverEvent_createdAt,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
    { Core.abbrev = "OpsWorksCM",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "opsworks-cm",
      Core.signingName = "opsworks-cm",
      Core.version = "2016-11-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "OpsWorksCM",
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

-- | The requested resource cannot be created because it already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | The resource is in a state that does not allow you to perform a
-- specified action.
_InvalidStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidStateException =
  Core._MatchServiceError
    defaultService
    "InvalidStateException"

-- | The requested resource does not exist, or access was denied.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The limit of servers or backups has been reached.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | This occurs when the provided nextToken is not valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | One or more of the provided request parameters are not valid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

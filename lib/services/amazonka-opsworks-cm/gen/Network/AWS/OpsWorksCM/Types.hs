{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidStateException,
    _ResourceAlreadyExistsException,
    _InvalidNextTokenException,
    _ValidationException,
    _LimitExceededException,
    _ResourceNotFoundException,

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
    accountAttribute_name,
    accountAttribute_maximum,

    -- * Backup
    Backup (..),
    newBackup,
    backup_instanceProfileArn,
    backup_preferredBackupWindow,
    backup_securityGroupIds,
    backup_serviceRoleArn,
    backup_status,
    backup_userArn,
    backup_instanceType,
    backup_backupType,
    backup_s3DataUrl,
    backup_backupArn,
    backup_backupId,
    backup_createdAt,
    backup_subnetIds,
    backup_s3DataSize,
    backup_keyPair,
    backup_serverName,
    backup_s3LogUrl,
    backup_engineVersion,
    backup_preferredMaintenanceWindow,
    backup_toolsVersion,
    backup_engineModel,
    backup_engine,
    backup_description,
    backup_statusDescription,

    -- * EngineAttribute
    EngineAttribute (..),
    newEngineAttribute,
    engineAttribute_name,
    engineAttribute_value,

    -- * Server
    Server (..),
    newServer,
    server_instanceProfileArn,
    server_preferredBackupWindow,
    server_securityGroupIds,
    server_serviceRoleArn,
    server_status,
    server_disableAutomatedBackup,
    server_engineAttributes,
    server_instanceType,
    server_customDomain,
    server_createdAt,
    server_subnetIds,
    server_keyPair,
    server_associatePublicIpAddress,
    server_serverName,
    server_engineVersion,
    server_preferredMaintenanceWindow,
    server_cloudFormationStackArn,
    server_maintenanceStatus,
    server_engineModel,
    server_backupRetentionCount,
    server_engine,
    server_endpoint,
    server_statusReason,
    server_serverArn,

    -- * ServerEvent
    ServerEvent (..),
    newServerEvent,
    serverEvent_logUrl,
    serverEvent_message,
    serverEvent_createdAt,
    serverEvent_serverName,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types.AccountAttribute
import Network.AWS.OpsWorksCM.Types.Backup
import Network.AWS.OpsWorksCM.Types.BackupStatus
import Network.AWS.OpsWorksCM.Types.BackupType
import Network.AWS.OpsWorksCM.Types.EngineAttribute
import Network.AWS.OpsWorksCM.Types.MaintenanceStatus
import Network.AWS.OpsWorksCM.Types.NodeAssociationStatus
import Network.AWS.OpsWorksCM.Types.Server
import Network.AWS.OpsWorksCM.Types.ServerEvent
import Network.AWS.OpsWorksCM.Types.ServerStatus
import Network.AWS.OpsWorksCM.Types.Tag
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
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
      | Prelude.otherwise = Prelude.Nothing

-- | The resource is in a state that does not allow you to perform a
-- specified action.
_InvalidStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidStateException =
  Core._MatchServiceError
    defaultService
    "InvalidStateException"

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

-- | One or more of the provided request parameters are not valid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | The limit of servers or backups has been reached.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The requested resource does not exist, or access was denied.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

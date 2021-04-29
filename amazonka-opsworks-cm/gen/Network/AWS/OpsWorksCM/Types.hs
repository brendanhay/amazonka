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
    backup_securityGroupIds,
    backup_instanceProfileArn,
    backup_preferredBackupWindow,
    backup_status,
    backup_serviceRoleArn,
    backup_userArn,
    backup_instanceType,
    backup_backupType,
    backup_backupId,
    backup_s3DataUrl,
    backup_backupArn,
    backup_subnetIds,
    backup_keyPair,
    backup_s3DataSize,
    backup_createdAt,
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
    server_securityGroupIds,
    server_instanceProfileArn,
    server_preferredBackupWindow,
    server_status,
    server_disableAutomatedBackup,
    server_serviceRoleArn,
    server_instanceType,
    server_engineAttributes,
    server_customDomain,
    server_subnetIds,
    server_keyPair,
    server_createdAt,
    server_serverName,
    server_associatePublicIpAddress,
    server_engineVersion,
    server_preferredMaintenanceWindow,
    server_backupRetentionCount,
    server_maintenanceStatus,
    server_cloudFormationStackArn,
    server_engineModel,
    server_engine,
    server_endpoint,
    server_serverArn,
    server_statusReason,

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
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "OpsWorksCM",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "opsworks-cm",
      Prelude._svcVersion = "2016-11-01",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "OpsWorksCM",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The resource is in a state that does not allow you to perform a
-- specified action.
_InvalidStateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidStateException =
  Prelude._MatchServiceError
    defaultService
    "InvalidStateException"

-- | The requested resource cannot be created because it already exists.
_ResourceAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | This occurs when the provided nextToken is not valid.
_InvalidNextTokenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidNextTokenException =
  Prelude._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | One or more of the provided request parameters are not valid.
_ValidationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ValidationException =
  Prelude._MatchServiceError
    defaultService
    "ValidationException"

-- | The limit of servers or backups has been reached.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The requested resource does not exist, or access was denied.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"

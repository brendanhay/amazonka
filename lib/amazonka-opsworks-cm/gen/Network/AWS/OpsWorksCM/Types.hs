-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _ValidationException,
    _ResourceAlreadyExistsException,
    _InvalidNextTokenException,
    _ResourceNotFoundException,
    _InvalidStateException,
    _LimitExceededException,

    -- * ServiceRoleArn
    ServiceRoleArn (..),

    -- * TimeWindowDefinition
    TimeWindowDefinition (..),

    -- * InstanceProfileArn
    InstanceProfileArn (..),

    -- * AttributeValue
    AttributeValue (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * ServerName
    ServerName (..),

    -- * EngineAttributeName
    EngineAttributeName (..),

    -- * String
    String (..),

    -- * KeyPair
    KeyPair (..),

    -- * BackupId
    BackupId (..),

    -- * CustomDomain
    CustomDomain (..),

    -- * Backup
    Backup (..),
    mkBackup,
    bBackupArn,
    bBackupId,
    bBackupType,
    bCreatedAt,
    bDescription,
    bEngine,
    bEngineModel,
    bEngineVersion,
    bInstanceProfileArn,
    bInstanceType,
    bKeyPair,
    bPreferredBackupWindow,
    bPreferredMaintenanceWindow,
    bS3DataSize,
    bS3DataUrl,
    bS3LogUrl,
    bSecurityGroupIds,
    bServerName,
    bServiceRoleArn,
    bStatus,
    bStatusDescription,
    bSubnetIds,
    bToolsVersion,
    bUserArn,

    -- * MaintenanceStatus
    MaintenanceStatus (..),

    -- * CustomPrivateKey
    CustomPrivateKey (..),

    -- * ServerEvent
    ServerEvent (..),
    mkServerEvent,
    seCreatedAt,
    seLogUrl,
    seMessage,
    seServerName,

    -- * NextToken
    NextToken (..),

    -- * NodeName
    NodeName (..),

    -- * BackupStatus
    BackupStatus (..),

    -- * EngineAttribute
    EngineAttribute (..),
    mkEngineAttribute,
    eaName,
    eaValue,

    -- * AWSOpsWorksCMResourceArn
    AWSOpsWorksCMResourceArn (..),

    -- * NodeAssociationStatus
    NodeAssociationStatus (..),

    -- * AccountAttribute
    AccountAttribute (..),
    mkAccountAttribute,
    aaMaximum,
    aaName,
    aaUsed,

    -- * TagKey
    TagKey (..),

    -- * Server
    Server (..),
    mkServer,
    sAssociatePublicIpAddress,
    sBackupRetentionCount,
    sCloudFormationStackArn,
    sCreatedAt,
    sCustomDomain,
    sDisableAutomatedBackup,
    sEndpoint,
    sEngine,
    sEngineAttributes,
    sEngineModel,
    sEngineVersion,
    sInstanceProfileArn,
    sInstanceType,
    sKeyPair,
    sMaintenanceStatus,
    sPreferredBackupWindow,
    sPreferredMaintenanceWindow,
    sSecurityGroupIds,
    sServerArn,
    sServerName,
    sServiceRoleArn,
    sStatus,
    sStatusReason,
    sSubnetIds,

    -- * AttributeName
    AttributeName (..),

    -- * CustomCertificate
    CustomCertificate (..),

    -- * NodeAssociationStatusToken
    NodeAssociationStatusToken (..),

    -- * BackupType
    BackupType (..),

    -- * ServerStatus
    ServerStatus (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * ResourceArn
    ResourceArn (..),
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types.AWSOpsWorksCMResourceArn
import Network.AWS.OpsWorksCM.Types.AccountAttribute
import Network.AWS.OpsWorksCM.Types.AttributeName
import Network.AWS.OpsWorksCM.Types.AttributeValue
import Network.AWS.OpsWorksCM.Types.Backup
import Network.AWS.OpsWorksCM.Types.BackupId
import Network.AWS.OpsWorksCM.Types.BackupStatus
import Network.AWS.OpsWorksCM.Types.BackupType
import Network.AWS.OpsWorksCM.Types.CustomCertificate
import Network.AWS.OpsWorksCM.Types.CustomDomain
import Network.AWS.OpsWorksCM.Types.CustomPrivateKey
import Network.AWS.OpsWorksCM.Types.EngineAttribute
import Network.AWS.OpsWorksCM.Types.EngineAttributeName
import Network.AWS.OpsWorksCM.Types.InstanceProfileArn
import Network.AWS.OpsWorksCM.Types.Key
import Network.AWS.OpsWorksCM.Types.KeyPair
import Network.AWS.OpsWorksCM.Types.MaintenanceStatus
import Network.AWS.OpsWorksCM.Types.NextToken
import Network.AWS.OpsWorksCM.Types.NodeAssociationStatus
import Network.AWS.OpsWorksCM.Types.NodeAssociationStatusToken
import Network.AWS.OpsWorksCM.Types.NodeName
import Network.AWS.OpsWorksCM.Types.ResourceArn
import Network.AWS.OpsWorksCM.Types.Server
import Network.AWS.OpsWorksCM.Types.ServerEvent
import Network.AWS.OpsWorksCM.Types.ServerName
import Network.AWS.OpsWorksCM.Types.ServerStatus
import Network.AWS.OpsWorksCM.Types.ServiceRoleArn
import Network.AWS.OpsWorksCM.Types.String
import Network.AWS.OpsWorksCM.Types.Tag
import Network.AWS.OpsWorksCM.Types.TagKey
import Network.AWS.OpsWorksCM.Types.TimeWindowDefinition
import Network.AWS.OpsWorksCM.Types.Value
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-11-01@ of the Amazon OpsWorks CM SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "OpsWorksCM",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "opsworks-cm",
      Core._svcVersion = "2016-11-01",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "OpsWorksCM",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | One or more of the provided request parameters are not valid.
_ValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError mkServiceConfig "ValidationException"
{-# DEPRECATED _ValidationException "Use generic-lens or generic-optics instead." #-}

-- | The requested resource cannot be created because it already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceAlreadyExistsException"
{-# DEPRECATED _ResourceAlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | This occurs when the provided nextToken is not valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidNextTokenException"
{-# DEPRECATED _InvalidNextTokenException "Use generic-lens or generic-optics instead." #-}

-- | The requested resource does not exist, or access was denied.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceNotFoundException"
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The resource is in a state that does not allow you to perform a specified action.
_InvalidStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidStateException =
  Core._MatchServiceError mkServiceConfig "InvalidStateException"
{-# DEPRECATED _InvalidStateException "Use generic-lens or generic-optics instead." #-}

-- | The limit of servers or backups has been reached.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}

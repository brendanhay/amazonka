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
    opsWorksCMService,

    -- * Errors

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
    mkAccountAttribute,
    aaUsed,
    aaMaximum,
    aaName,

    -- * Backup
    Backup (..),
    mkBackup,
    bEngineVersion,
    bServiceRoleARN,
    bStatus,
    bInstanceProfileARN,
    bSecurityGroupIds,
    bStatusDescription,
    bServerName,
    bSubnetIds,
    bKeyPair,
    bCreatedAt,
    bBackupId,
    bEngine,
    bInstanceType,
    bEngineModel,
    bPreferredMaintenanceWindow,
    bUserARN,
    bPreferredBackupWindow,
    bS3LogURL,
    bS3DataSize,
    bBackupARN,
    bS3DataURL,
    bDescription,
    bBackupType,
    bToolsVersion,

    -- * EngineAttribute
    EngineAttribute (..),
    mkEngineAttribute,
    eaValue,
    eaName,

    -- * Server
    Server (..),
    mkServer,
    sEngineVersion,
    sServiceRoleARN,
    sDisableAutomatedBackup,
    sStatus,
    sInstanceProfileARN,
    sSecurityGroupIds,
    sAssociatePublicIPAddress,
    sServerName,
    sSubnetIds,
    sKeyPair,
    sCreatedAt,
    sServerARN,
    sCustomDomain,
    sEngine,
    sMaintenanceStatus,
    sInstanceType,
    sEngineModel,
    sEngineAttributes,
    sPreferredMaintenanceWindow,
    sPreferredBackupWindow,
    sStatusReason,
    sEndpoint,
    sCloudFormationStackARN,
    sBackupRetentionCount,

    -- * ServerEvent
    ServerEvent (..),
    mkServerEvent,
    seLogURL,
    seServerName,
    seCreatedAt,
    seMessage,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-11-01@ of the Amazon OpsWorks CM SDK configuration.
opsWorksCMService :: Lude.Service
opsWorksCMService =
  Lude.Service
    { Lude._svcAbbrev = "OpsWorksCM",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "opsworks-cm",
      Lude._svcVersion = "2016-11-01",
      Lude._svcEndpoint = Lude.defaultEndpoint opsWorksCMService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "OpsWorksCM",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing

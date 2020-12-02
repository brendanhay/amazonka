{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types
  ( -- * Service Configuration
    opsWorksCM,

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
    AccountAttribute,
    accountAttribute,
    aaUsed,
    aaMaximum,
    aaName,

    -- * Backup
    Backup,
    backup,
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
    EngineAttribute,
    engineAttribute,
    eaValue,
    eaName,

    -- * Server
    Server,
    server,
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
    ServerEvent,
    serverEvent,
    seLogURL,
    seServerName,
    seCreatedAt,
    seMessage,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,
  )
where

import Network.AWS.Lens
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
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2016-11-01@ of the Amazon OpsWorks CM SDK configuration.
opsWorksCM :: Service
opsWorksCM =
  Service
    { _svcAbbrev = "OpsWorksCM",
      _svcSigner = v4,
      _svcPrefix = "opsworks-cm",
      _svcVersion = "2016-11-01",
      _svcEndpoint = defaultEndpoint opsWorksCM,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "OpsWorksCM",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

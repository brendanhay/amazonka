{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types
  ( -- * Service Configuration
    migrationHub,

    -- * Errors

    -- * ApplicationStatus
    ApplicationStatus (..),

    -- * MigrationStatus
    MigrationStatus (..),

    -- * ResourceAttributeType
    ResourceAttributeType (..),

    -- * ApplicationState
    ApplicationState,
    applicationState,
    asLastUpdatedTime,
    asApplicationId,
    asApplicationStatus,

    -- * CreatedArtifact
    CreatedArtifact,
    createdArtifact,
    caDescription,
    caName,

    -- * DiscoveredResource
    DiscoveredResource,
    discoveredResource,
    drDescription,
    drConfigurationId,

    -- * MigrationTask
    MigrationTask,
    migrationTask,
    mtUpdateDateTime,
    mtResourceAttributeList,
    mtTask,
    mtProgressUpdateStream,
    mtMigrationTaskName,

    -- * MigrationTaskSummary
    MigrationTaskSummary,
    migrationTaskSummary,
    mtsStatus,
    mtsUpdateDateTime,
    mtsProgressPercent,
    mtsStatusDetail,
    mtsProgressUpdateStream,
    mtsMigrationTaskName,

    -- * ProgressUpdateStreamSummary
    ProgressUpdateStreamSummary,
    progressUpdateStreamSummary,
    pussProgressUpdateStreamName,

    -- * ResourceAttribute
    ResourceAttribute,
    resourceAttribute,
    raType,
    raValue,

    -- * Task
    Task,
    task,
    tProgressPercent,
    tStatusDetail,
    tStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types.ApplicationState
import Network.AWS.MigrationHub.Types.ApplicationStatus
import Network.AWS.MigrationHub.Types.CreatedArtifact
import Network.AWS.MigrationHub.Types.DiscoveredResource
import Network.AWS.MigrationHub.Types.MigrationStatus
import Network.AWS.MigrationHub.Types.MigrationTask
import Network.AWS.MigrationHub.Types.MigrationTaskSummary
import Network.AWS.MigrationHub.Types.ProgressUpdateStreamSummary
import Network.AWS.MigrationHub.Types.ResourceAttribute
import Network.AWS.MigrationHub.Types.ResourceAttributeType
import Network.AWS.MigrationHub.Types.Task
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-05-31@ of the Amazon Migration Hub SDK configuration.
migrationHub :: Service
migrationHub =
  Service
    { _svcAbbrev = "MigrationHub",
      _svcSigner = v4,
      _svcPrefix = "mgh",
      _svcVersion = "2017-05-31",
      _svcEndpoint = defaultEndpoint migrationHub,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "MigrationHub",
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

{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MigrationHub.Types
    (
    -- * Service Configuration
      migrationHub

    -- * Errors
    , _AccessDeniedException
    , _DryRunOperation
    , _PolicyErrorException
    , _InternalServerError
    , _InvalidInputException
    , _ServiceUnavailableException
    , _ResourceNotFoundException
    , _UnauthorizedOperation

    -- * ApplicationStatus
    , ApplicationStatus (..)

    -- * MigrationStatus
    , MigrationStatus (..)

    -- * ResourceAttributeType
    , ResourceAttributeType (..)

    -- * CreatedArtifact
    , CreatedArtifact
    , createdArtifact
    , caDescription
    , caName

    -- * DiscoveredResource
    , DiscoveredResource
    , discoveredResource
    , drDescription
    , drConfigurationId

    -- * MigrationTask
    , MigrationTask
    , migrationTask
    , mtUpdateDateTime
    , mtResourceAttributeList
    , mtTask
    , mtProgressUpdateStream
    , mtMigrationTaskName

    -- * MigrationTaskSummary
    , MigrationTaskSummary
    , migrationTaskSummary
    , mtsStatus
    , mtsUpdateDateTime
    , mtsProgressPercent
    , mtsStatusDetail
    , mtsProgressUpdateStream
    , mtsMigrationTaskName

    -- * ProgressUpdateStreamSummary
    , ProgressUpdateStreamSummary
    , progressUpdateStreamSummary
    , pussProgressUpdateStreamName

    -- * ResourceAttribute
    , ResourceAttribute
    , resourceAttribute
    , raType
    , raValue

    -- * Task
    , Task
    , task
    , tProgressPercent
    , tStatusDetail
    , tStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types.Product
import Network.AWS.MigrationHub.Types.Sum
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-05-31@ of the Amazon Migration Hub SDK configuration.
migrationHub :: Service
migrationHub =
  Service
    { _svcAbbrev = "MigrationHub"
    , _svcSigner = v4
    , _svcPrefix = "mgh"
    , _svcVersion = "2017-05-31"
    , _svcEndpoint = defaultEndpoint migrationHub
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "MigrationHub"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | You do not have sufficient access to perform this action.
--
--
_AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedException = _MatchServiceError migrationHub "AccessDeniedException"


-- | Exception raised to indicate a successfully authorized action when the @DryRun@ flag is set to "true".
--
--
_DryRunOperation :: AsError a => Getting (First ServiceError) a ServiceError
_DryRunOperation = _MatchServiceError migrationHub "DryRunOperation"


-- | Exception raised when there are problems accessing ADS (Application Discovery Service); most likely due to a misconfigured policy or the @migrationhub-discovery@ role is missing or not configured correctly.
--
--
_PolicyErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_PolicyErrorException = _MatchServiceError migrationHub "PolicyErrorException"


-- | Exception raised when there is an internal, configuration, or dependency error encountered.
--
--
_InternalServerError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerError = _MatchServiceError migrationHub "InternalServerError"


-- | Exception raised when the provided input violates a policy constraint or is entered in the wrong format or data type.
--
--
_InvalidInputException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInputException = _MatchServiceError migrationHub "InvalidInputException"


-- | Exception raised when there is an internal, configuration, or dependency error encountered.
--
--
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
  _MatchServiceError migrationHub "ServiceUnavailableException"


-- | Exception raised when the request references a resource (ADS configuration, update stream, migration task, etc.) that does not exist in ADS (Application Discovery Service) or in Migration Hub's repository.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError migrationHub "ResourceNotFoundException"


-- | Exception raised to indicate a request was not authorized when the @DryRun@ flag is set to "true".
--
--
_UnauthorizedOperation :: AsError a => Getting (First ServiceError) a ServiceError
_UnauthorizedOperation = _MatchServiceError migrationHub "UnauthorizedOperation"


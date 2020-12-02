{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecretsManager.Types
  ( -- * Service Configuration
    secretsManager,

    -- * Errors

    -- * FilterNameStringType
    FilterNameStringType (..),

    -- * SortOrderType
    SortOrderType (..),

    -- * Filter
    Filter,
    filter',
    fValues,
    fKey,

    -- * RotationRulesType
    RotationRulesType,
    rotationRulesType,
    rrtAutomaticallyAfterDays,

    -- * SecretListEntry
    SecretListEntry,
    secretListEntry,
    sleLastChangedDate,
    sleARN,
    sleSecretVersionsToStages,
    sleRotationRules,
    sleDeletedDate,
    sleRotationEnabled,
    sleCreatedDate,
    sleKMSKeyId,
    sleName,
    sleOwningService,
    sleLastRotatedDate,
    sleLastAccessedDate,
    sleDescription,
    sleRotationLambdaARN,
    sleTags,

    -- * SecretVersionsListEntry
    SecretVersionsListEntry,
    secretVersionsListEntry,
    svleVersionId,
    svleVersionStages,
    svleCreatedDate,
    svleLastAccessedDate,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * ValidationErrorsEntry
    ValidationErrorsEntry,
    validationErrorsEntry,
    veeCheckName,
    veeErrorMessage,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SecretsManager.Types.Filter
import Network.AWS.SecretsManager.Types.FilterNameStringType
import Network.AWS.SecretsManager.Types.RotationRulesType
import Network.AWS.SecretsManager.Types.SecretListEntry
import Network.AWS.SecretsManager.Types.SecretVersionsListEntry
import Network.AWS.SecretsManager.Types.SortOrderType
import Network.AWS.SecretsManager.Types.Tag
import Network.AWS.SecretsManager.Types.ValidationErrorsEntry
import Network.AWS.Sign.V4

-- | API version @2017-10-17@ of the Amazon Secrets Manager SDK configuration.
secretsManager :: Service
secretsManager =
  Service
    { _svcAbbrev = "SecretsManager",
      _svcSigner = v4,
      _svcPrefix = "secretsmanager",
      _svcVersion = "2017-10-17",
      _svcEndpoint = defaultEndpoint secretsManager,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "SecretsManager",
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

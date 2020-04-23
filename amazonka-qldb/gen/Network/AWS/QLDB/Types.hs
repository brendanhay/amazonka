{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.QLDB.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.QLDB.Types
    (
    -- * Service Configuration
      qldb

    -- * Errors
    , _InvalidParameterException
    , _ResourcePreconditionNotMetException
    , _ResourceAlreadyExistsException
    , _ResourceNotFoundException
    , _LimitExceededException
    , _ResourceInUseException

    -- * ExportStatus
    , ExportStatus (..)

    -- * LedgerState
    , LedgerState (..)

    -- * PermissionsMode
    , PermissionsMode (..)

    -- * S3ObjectEncryptionType
    , S3ObjectEncryptionType (..)

    -- * JournalS3ExportDescription
    , JournalS3ExportDescription
    , journalS3ExportDescription
    , jsedLedgerName
    , jsedExportId
    , jsedExportCreationTime
    , jsedStatus
    , jsedInclusiveStartTime
    , jsedExclusiveEndTime
    , jsedS3ExportConfiguration
    , jsedRoleARN

    -- * LedgerSummary
    , LedgerSummary
    , ledgerSummary
    , lsState
    , lsName
    , lsCreationDateTime

    -- * S3EncryptionConfiguration
    , S3EncryptionConfiguration
    , s3EncryptionConfiguration
    , secKMSKeyARN
    , secObjectEncryptionType

    -- * S3ExportConfiguration
    , S3ExportConfiguration
    , s3ExportConfiguration
    , secBucket
    , secPrefix
    , secEncryptionConfiguration

    -- * ValueHolder
    , ValueHolder
    , valueHolder
    , vhIonText
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.QLDB.Types.Product
import Network.AWS.QLDB.Types.Sum
import Network.AWS.Sign.V4

-- | API version @2019-01-02@ of the Amazon QLDB SDK configuration.
qldb :: Service
qldb =
  Service
    { _svcAbbrev = "QLDB"
    , _svcSigner = v4
    , _svcPrefix = "qldb"
    , _svcVersion = "2019-01-02"
    , _svcEndpoint = defaultEndpoint qldb
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "QLDB"
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


-- | Prism for InvalidParameterException' errors.
_InvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
  _MatchServiceError qldb "InvalidParameterException" . hasStatus 400


-- | Prism for ResourcePreconditionNotMetException' errors.
_ResourcePreconditionNotMetException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourcePreconditionNotMetException =
  _MatchServiceError qldb "ResourcePreconditionNotMetException" . hasStatus 412


-- | Prism for ResourceAlreadyExistsException' errors.
_ResourceAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsException =
  _MatchServiceError qldb "ResourceAlreadyExistsException" . hasStatus 409


-- | Prism for ResourceNotFoundException' errors.
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError qldb "ResourceNotFoundException" . hasStatus 404


-- | Prism for LimitExceededException' errors.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError qldb "LimitExceededException" . hasStatus 400


-- | Prism for ResourceInUseException' errors.
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException =
  _MatchServiceError qldb "ResourceInUseException" . hasStatus 409


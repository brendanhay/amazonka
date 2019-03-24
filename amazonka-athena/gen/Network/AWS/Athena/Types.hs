{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Athena.Types
    (
    -- * Service Configuration
      athena

    -- * Errors
    , _InvalidRequestException
    , _TooManyRequestsException
    , _InternalServerException
    , _ResourceNotFoundException

    -- * ColumnNullable
    , ColumnNullable (..)

    -- * EncryptionOption
    , EncryptionOption (..)

    -- * QueryExecutionState
    , QueryExecutionState (..)

    -- * StatementType
    , StatementType (..)

    -- * WorkGroupState
    , WorkGroupState (..)

    -- * ColumnInfo
    , ColumnInfo
    , columnInfo
    , ciScale
    , ciPrecision
    , ciSchemaName
    , ciCatalogName
    , ciCaseSensitive
    , ciLabel
    , ciTableName
    , ciNullable
    , ciName
    , ciType

    -- * Datum
    , Datum
    , datum
    , dVarCharValue

    -- * EncryptionConfiguration
    , EncryptionConfiguration
    , encryptionConfiguration
    , ecKMSKey
    , ecEncryptionOption

    -- * NamedQuery
    , NamedQuery
    , namedQuery
    , nqNamedQueryId
    , nqDescription
    , nqWorkGroup
    , nqName
    , nqDatabase
    , nqQueryString

    -- * QueryExecution
    , QueryExecution
    , queryExecution
    , qeStatus
    , qeQueryExecutionContext
    , qeResultConfiguration
    , qeQuery
    , qeStatementType
    , qeStatistics
    , qeQueryExecutionId
    , qeWorkGroup

    -- * QueryExecutionContext
    , QueryExecutionContext
    , queryExecutionContext
    , qecDatabase

    -- * QueryExecutionStatistics
    , QueryExecutionStatistics
    , queryExecutionStatistics
    , qesEngineExecutionTimeInMillis
    , qesDataScannedInBytes

    -- * QueryExecutionStatus
    , QueryExecutionStatus
    , queryExecutionStatus
    , qesState
    , qesStateChangeReason
    , qesSubmissionDateTime
    , qesCompletionDateTime

    -- * ResultConfiguration
    , ResultConfiguration
    , resultConfiguration
    , rcEncryptionConfiguration
    , rcOutputLocation

    -- * ResultConfigurationUpdates
    , ResultConfigurationUpdates
    , resultConfigurationUpdates
    , rcuRemoveOutputLocation
    , rcuRemoveEncryptionConfiguration
    , rcuEncryptionConfiguration
    , rcuOutputLocation

    -- * ResultSet
    , ResultSet
    , resultSet
    , rsRows
    , rsResultSetMetadata

    -- * ResultSetMetadata
    , ResultSetMetadata
    , resultSetMetadata
    , rsmColumnInfo

    -- * Row
    , Row
    , row
    , rowData

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * UnprocessedNamedQueryId
    , UnprocessedNamedQueryId
    , unprocessedNamedQueryId
    , unqiNamedQueryId
    , unqiErrorCode
    , unqiErrorMessage

    -- * UnprocessedQueryExecutionId
    , UnprocessedQueryExecutionId
    , unprocessedQueryExecutionId
    , uqeiErrorCode
    , uqeiQueryExecutionId
    , uqeiErrorMessage

    -- * WorkGroup
    , WorkGroup
    , workGroup
    , wgCreationTime
    , wgState
    , wgConfiguration
    , wgDescription
    , wgName

    -- * WorkGroupConfiguration
    , WorkGroupConfiguration
    , workGroupConfiguration
    , wgcResultConfiguration
    , wgcBytesScannedCutoffPerQuery
    , wgcEnforceWorkGroupConfiguration
    , wgcPublishCloudWatchMetricsEnabled

    -- * WorkGroupConfigurationUpdates
    , WorkGroupConfigurationUpdates
    , workGroupConfigurationUpdates
    , wgcuResultConfigurationUpdates
    , wgcuBytesScannedCutoffPerQuery
    , wgcuRemoveBytesScannedCutoffPerQuery
    , wgcuEnforceWorkGroupConfiguration
    , wgcuPublishCloudWatchMetricsEnabled

    -- * WorkGroupSummary
    , WorkGroupSummary
    , workGroupSummary
    , wgsCreationTime
    , wgsState
    , wgsName
    , wgsDescription
    ) where

import Network.AWS.Athena.Types.Product
import Network.AWS.Athena.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-05-18@ of the Amazon Athena SDK configuration.
athena :: Service
athena =
  Service
    { _svcAbbrev = "Athena"
    , _svcSigner = v4
    , _svcPrefix = "athena"
    , _svcVersion = "2017-05-18"
    , _svcEndpoint = defaultEndpoint athena
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Athena"
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


-- | Indicates that something is wrong with the input to the request. For example, a required parameter may be missing or out of range.
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException = _MatchServiceError athena "InvalidRequestException"


-- | Indicates that the request was throttled.
--
--
_TooManyRequestsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestsException = _MatchServiceError athena "TooManyRequestsException"


-- | Indicates a platform issue, which may be due to a transient condition or outage.
--
--
_InternalServerException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerException = _MatchServiceError athena "InternalServerException"


-- | A resource, such as a workgroup, was not found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError athena "ResourceNotFoundException"


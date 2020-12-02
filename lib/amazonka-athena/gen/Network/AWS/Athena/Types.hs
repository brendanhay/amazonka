{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types
  ( -- * Service Configuration
    athena,

    -- * Errors

    -- * ColumnNullable
    ColumnNullable (..),

    -- * DataCatalogType
    DataCatalogType (..),

    -- * EncryptionOption
    EncryptionOption (..),

    -- * QueryExecutionState
    QueryExecutionState (..),

    -- * StatementType
    StatementType (..),

    -- * WorkGroupState
    WorkGroupState (..),

    -- * Column
    Column,
    column,
    cType,
    cComment,
    cName,

    -- * ColumnInfo
    ColumnInfo,
    columnInfo,
    ciScale,
    ciPrecision,
    ciSchemaName,
    ciCatalogName,
    ciCaseSensitive,
    ciLabel,
    ciTableName,
    ciNullable,
    ciName,
    ciType,

    -- * DataCatalog
    DataCatalog,
    dataCatalog,
    dcParameters,
    dcDescription,
    dcName,
    dcType,

    -- * DataCatalogSummary
    DataCatalogSummary,
    dataCatalogSummary,
    dcsCatalogName,
    dcsType,

    -- * Database
    Database,
    database,
    dParameters,
    dDescription,
    dName,

    -- * Datum
    Datum,
    datum,
    dVarCharValue,

    -- * EncryptionConfiguration
    EncryptionConfiguration,
    encryptionConfiguration,
    ecKMSKey,
    ecEncryptionOption,

    -- * NamedQuery
    NamedQuery,
    namedQuery,
    nqNamedQueryId,
    nqDescription,
    nqWorkGroup,
    nqName,
    nqDatabase,
    nqQueryString,

    -- * QueryExecution
    QueryExecution,
    queryExecution,
    qeStatus,
    qeQueryExecutionContext,
    qeResultConfiguration,
    qeQuery,
    qeStatementType,
    qeStatistics,
    qeQueryExecutionId,
    qeWorkGroup,

    -- * QueryExecutionContext
    QueryExecutionContext,
    queryExecutionContext,
    qecDatabase,
    qecCatalog,

    -- * QueryExecutionStatistics
    QueryExecutionStatistics,
    queryExecutionStatistics,
    qesTotalExecutionTimeInMillis,
    qesEngineExecutionTimeInMillis,
    qesQueryPlanningTimeInMillis,
    qesDataScannedInBytes,
    qesQueryQueueTimeInMillis,
    qesDataManifestLocation,
    qesServiceProcessingTimeInMillis,

    -- * QueryExecutionStatus
    QueryExecutionStatus,
    queryExecutionStatus,
    qesState,
    qesStateChangeReason,
    qesSubmissionDateTime,
    qesCompletionDateTime,

    -- * ResultConfiguration
    ResultConfiguration,
    resultConfiguration,
    rcEncryptionConfiguration,
    rcOutputLocation,

    -- * ResultConfigurationUpdates
    ResultConfigurationUpdates,
    resultConfigurationUpdates,
    rcuRemoveOutputLocation,
    rcuRemoveEncryptionConfiguration,
    rcuEncryptionConfiguration,
    rcuOutputLocation,

    -- * ResultSet
    ResultSet,
    resultSet,
    rsRows,
    rsResultSetMetadata,

    -- * ResultSetMetadata
    ResultSetMetadata,
    resultSetMetadata,
    rsmColumnInfo,

    -- * Row
    Row,
    row,
    rowData,

    -- * TableMetadata
    TableMetadata,
    tableMetadata,
    tmTableType,
    tmParameters,
    tmColumns,
    tmLastAccessTime,
    tmPartitionKeys,
    tmCreateTime,
    tmName,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * UnprocessedNamedQueryId
    UnprocessedNamedQueryId,
    unprocessedNamedQueryId,
    unqiNamedQueryId,
    unqiErrorCode,
    unqiErrorMessage,

    -- * UnprocessedQueryExecutionId
    UnprocessedQueryExecutionId,
    unprocessedQueryExecutionId,
    uqeiErrorCode,
    uqeiQueryExecutionId,
    uqeiErrorMessage,

    -- * WorkGroup
    WorkGroup,
    workGroup,
    wgCreationTime,
    wgState,
    wgConfiguration,
    wgDescription,
    wgName,

    -- * WorkGroupConfiguration
    WorkGroupConfiguration,
    workGroupConfiguration,
    wgcRequesterPaysEnabled,
    wgcResultConfiguration,
    wgcBytesScannedCutoffPerQuery,
    wgcEnforceWorkGroupConfiguration,
    wgcPublishCloudWatchMetricsEnabled,

    -- * WorkGroupConfigurationUpdates
    WorkGroupConfigurationUpdates,
    workGroupConfigurationUpdates,
    wgcuRequesterPaysEnabled,
    wgcuResultConfigurationUpdates,
    wgcuBytesScannedCutoffPerQuery,
    wgcuRemoveBytesScannedCutoffPerQuery,
    wgcuEnforceWorkGroupConfiguration,
    wgcuPublishCloudWatchMetricsEnabled,

    -- * WorkGroupSummary
    WorkGroupSummary,
    workGroupSummary,
    wgsCreationTime,
    wgsState,
    wgsName,
    wgsDescription,
  )
where

import Network.AWS.Athena.Types.Column
import Network.AWS.Athena.Types.ColumnInfo
import Network.AWS.Athena.Types.ColumnNullable
import Network.AWS.Athena.Types.DataCatalog
import Network.AWS.Athena.Types.DataCatalogSummary
import Network.AWS.Athena.Types.DataCatalogType
import Network.AWS.Athena.Types.Database
import Network.AWS.Athena.Types.Datum
import Network.AWS.Athena.Types.EncryptionConfiguration
import Network.AWS.Athena.Types.EncryptionOption
import Network.AWS.Athena.Types.NamedQuery
import Network.AWS.Athena.Types.QueryExecution
import Network.AWS.Athena.Types.QueryExecutionContext
import Network.AWS.Athena.Types.QueryExecutionState
import Network.AWS.Athena.Types.QueryExecutionStatistics
import Network.AWS.Athena.Types.QueryExecutionStatus
import Network.AWS.Athena.Types.ResultConfiguration
import Network.AWS.Athena.Types.ResultConfigurationUpdates
import Network.AWS.Athena.Types.ResultSet
import Network.AWS.Athena.Types.ResultSetMetadata
import Network.AWS.Athena.Types.Row
import Network.AWS.Athena.Types.StatementType
import Network.AWS.Athena.Types.TableMetadata
import Network.AWS.Athena.Types.Tag
import Network.AWS.Athena.Types.UnprocessedNamedQueryId
import Network.AWS.Athena.Types.UnprocessedQueryExecutionId
import Network.AWS.Athena.Types.WorkGroup
import Network.AWS.Athena.Types.WorkGroupConfiguration
import Network.AWS.Athena.Types.WorkGroupConfigurationUpdates
import Network.AWS.Athena.Types.WorkGroupState
import Network.AWS.Athena.Types.WorkGroupSummary
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-05-18@ of the Amazon Athena SDK configuration.
athena :: Service
athena =
  Service
    { _svcAbbrev = "Athena",
      _svcSigner = v4,
      _svcPrefix = "athena",
      _svcVersion = "2017-05-18",
      _svcEndpoint = defaultEndpoint athena,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Athena",
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Athena.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidRequestException,
    _MetadataException,
    _TooManyRequestsException,
    _InternalServerException,
    _ResourceNotFoundException,

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
    Column (..),
    newColumn,
    column_type,
    column_comment,
    column_name,

    -- * ColumnInfo
    ColumnInfo (..),
    newColumnInfo,
    columnInfo_scale,
    columnInfo_precision,
    columnInfo_schemaName,
    columnInfo_catalogName,
    columnInfo_caseSensitive,
    columnInfo_label,
    columnInfo_tableName,
    columnInfo_nullable,
    columnInfo_name,
    columnInfo_type,

    -- * DataCatalog
    DataCatalog (..),
    newDataCatalog,
    dataCatalog_parameters,
    dataCatalog_description,
    dataCatalog_name,
    dataCatalog_type,

    -- * DataCatalogSummary
    DataCatalogSummary (..),
    newDataCatalogSummary,
    dataCatalogSummary_catalogName,
    dataCatalogSummary_type,

    -- * Database
    Database (..),
    newDatabase,
    database_parameters,
    database_description,
    database_name,

    -- * Datum
    Datum (..),
    newDatum,
    datum_varCharValue,

    -- * EncryptionConfiguration
    EncryptionConfiguration (..),
    newEncryptionConfiguration,
    encryptionConfiguration_kmsKey,
    encryptionConfiguration_encryptionOption,

    -- * EngineVersion
    EngineVersion (..),
    newEngineVersion,
    engineVersion_effectiveEngineVersion,
    engineVersion_selectedEngineVersion,

    -- * NamedQuery
    NamedQuery (..),
    newNamedQuery,
    namedQuery_namedQueryId,
    namedQuery_description,
    namedQuery_workGroup,
    namedQuery_name,
    namedQuery_database,
    namedQuery_queryString,

    -- * PreparedStatement
    PreparedStatement (..),
    newPreparedStatement,
    preparedStatement_lastModifiedTime,
    preparedStatement_queryStatement,
    preparedStatement_statementName,
    preparedStatement_description,
    preparedStatement_workGroupName,

    -- * PreparedStatementSummary
    PreparedStatementSummary (..),
    newPreparedStatementSummary,
    preparedStatementSummary_lastModifiedTime,
    preparedStatementSummary_statementName,

    -- * QueryExecution
    QueryExecution (..),
    newQueryExecution,
    queryExecution_engineVersion,
    queryExecution_status,
    queryExecution_queryExecutionContext,
    queryExecution_resultConfiguration,
    queryExecution_query,
    queryExecution_statementType,
    queryExecution_statistics,
    queryExecution_queryExecutionId,
    queryExecution_workGroup,

    -- * QueryExecutionContext
    QueryExecutionContext (..),
    newQueryExecutionContext,
    queryExecutionContext_database,
    queryExecutionContext_catalog,

    -- * QueryExecutionStatistics
    QueryExecutionStatistics (..),
    newQueryExecutionStatistics,
    queryExecutionStatistics_totalExecutionTimeInMillis,
    queryExecutionStatistics_engineExecutionTimeInMillis,
    queryExecutionStatistics_queryPlanningTimeInMillis,
    queryExecutionStatistics_dataScannedInBytes,
    queryExecutionStatistics_queryQueueTimeInMillis,
    queryExecutionStatistics_dataManifestLocation,
    queryExecutionStatistics_serviceProcessingTimeInMillis,

    -- * QueryExecutionStatus
    QueryExecutionStatus (..),
    newQueryExecutionStatus,
    queryExecutionStatus_state,
    queryExecutionStatus_stateChangeReason,
    queryExecutionStatus_submissionDateTime,
    queryExecutionStatus_completionDateTime,

    -- * ResultConfiguration
    ResultConfiguration (..),
    newResultConfiguration,
    resultConfiguration_encryptionConfiguration,
    resultConfiguration_outputLocation,

    -- * ResultConfigurationUpdates
    ResultConfigurationUpdates (..),
    newResultConfigurationUpdates,
    resultConfigurationUpdates_removeOutputLocation,
    resultConfigurationUpdates_removeEncryptionConfiguration,
    resultConfigurationUpdates_encryptionConfiguration,
    resultConfigurationUpdates_outputLocation,

    -- * ResultSet
    ResultSet (..),
    newResultSet,
    resultSet_rows,
    resultSet_resultSetMetadata,

    -- * ResultSetMetadata
    ResultSetMetadata (..),
    newResultSetMetadata,
    resultSetMetadata_columnInfo,

    -- * Row
    Row (..),
    newRow,
    row_data,

    -- * TableMetadata
    TableMetadata (..),
    newTableMetadata,
    tableMetadata_tableType,
    tableMetadata_parameters,
    tableMetadata_columns,
    tableMetadata_lastAccessTime,
    tableMetadata_partitionKeys,
    tableMetadata_createTime,
    tableMetadata_name,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * UnprocessedNamedQueryId
    UnprocessedNamedQueryId (..),
    newUnprocessedNamedQueryId,
    unprocessedNamedQueryId_namedQueryId,
    unprocessedNamedQueryId_errorCode,
    unprocessedNamedQueryId_errorMessage,

    -- * UnprocessedQueryExecutionId
    UnprocessedQueryExecutionId (..),
    newUnprocessedQueryExecutionId,
    unprocessedQueryExecutionId_errorCode,
    unprocessedQueryExecutionId_queryExecutionId,
    unprocessedQueryExecutionId_errorMessage,

    -- * WorkGroup
    WorkGroup (..),
    newWorkGroup,
    workGroup_creationTime,
    workGroup_state,
    workGroup_configuration,
    workGroup_description,
    workGroup_name,

    -- * WorkGroupConfiguration
    WorkGroupConfiguration (..),
    newWorkGroupConfiguration,
    workGroupConfiguration_engineVersion,
    workGroupConfiguration_requesterPaysEnabled,
    workGroupConfiguration_resultConfiguration,
    workGroupConfiguration_bytesScannedCutoffPerQuery,
    workGroupConfiguration_enforceWorkGroupConfiguration,
    workGroupConfiguration_publishCloudWatchMetricsEnabled,

    -- * WorkGroupConfigurationUpdates
    WorkGroupConfigurationUpdates (..),
    newWorkGroupConfigurationUpdates,
    workGroupConfigurationUpdates_engineVersion,
    workGroupConfigurationUpdates_requesterPaysEnabled,
    workGroupConfigurationUpdates_resultConfigurationUpdates,
    workGroupConfigurationUpdates_bytesScannedCutoffPerQuery,
    workGroupConfigurationUpdates_removeBytesScannedCutoffPerQuery,
    workGroupConfigurationUpdates_enforceWorkGroupConfiguration,
    workGroupConfigurationUpdates_publishCloudWatchMetricsEnabled,

    -- * WorkGroupSummary
    WorkGroupSummary (..),
    newWorkGroupSummary,
    workGroupSummary_creationTime,
    workGroupSummary_engineVersion,
    workGroupSummary_state,
    workGroupSummary_name,
    workGroupSummary_description,
  )
where

import Amazonka.Athena.Types.Column
import Amazonka.Athena.Types.ColumnInfo
import Amazonka.Athena.Types.ColumnNullable
import Amazonka.Athena.Types.DataCatalog
import Amazonka.Athena.Types.DataCatalogSummary
import Amazonka.Athena.Types.DataCatalogType
import Amazonka.Athena.Types.Database
import Amazonka.Athena.Types.Datum
import Amazonka.Athena.Types.EncryptionConfiguration
import Amazonka.Athena.Types.EncryptionOption
import Amazonka.Athena.Types.EngineVersion
import Amazonka.Athena.Types.NamedQuery
import Amazonka.Athena.Types.PreparedStatement
import Amazonka.Athena.Types.PreparedStatementSummary
import Amazonka.Athena.Types.QueryExecution
import Amazonka.Athena.Types.QueryExecutionContext
import Amazonka.Athena.Types.QueryExecutionState
import Amazonka.Athena.Types.QueryExecutionStatistics
import Amazonka.Athena.Types.QueryExecutionStatus
import Amazonka.Athena.Types.ResultConfiguration
import Amazonka.Athena.Types.ResultConfigurationUpdates
import Amazonka.Athena.Types.ResultSet
import Amazonka.Athena.Types.ResultSetMetadata
import Amazonka.Athena.Types.Row
import Amazonka.Athena.Types.StatementType
import Amazonka.Athena.Types.TableMetadata
import Amazonka.Athena.Types.Tag
import Amazonka.Athena.Types.UnprocessedNamedQueryId
import Amazonka.Athena.Types.UnprocessedQueryExecutionId
import Amazonka.Athena.Types.WorkGroup
import Amazonka.Athena.Types.WorkGroupConfiguration
import Amazonka.Athena.Types.WorkGroupConfigurationUpdates
import Amazonka.Athena.Types.WorkGroupState
import Amazonka.Athena.Types.WorkGroupSummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-05-18@ of the Amazon Athena SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Athena",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "athena",
      Core._serviceSigningName = "athena",
      Core._serviceVersion = "2017-05-18",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Athena",
      Core._serviceRetry = retry
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
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Indicates that something is wrong with the input to the request. For
-- example, a required parameter may be missing or out of range.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | An exception that Athena received when it called a custom metastore.
-- Occurs if the error is not caused by user input
-- (@InvalidRequestException@) or from the Athena platform
-- (@InternalServerException@). For example, if a user-created Lambda
-- function is missing permissions, the Lambda @4XX@ exception is returned
-- in a @MetadataException@.
_MetadataException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MetadataException =
  Core._MatchServiceError
    defaultService
    "MetadataException"

-- | Indicates that the request was throttled.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"

-- | Indicates a platform issue, which may be due to a transient condition or
-- outage.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | A resource, such as a workgroup, was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

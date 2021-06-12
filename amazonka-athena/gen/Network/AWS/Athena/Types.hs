{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidRequestException,
    _ResourceNotFoundException,
    _InternalServerException,
    _MetadataException,
    _TooManyRequestsException,

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
    column_comment,
    column_type,
    column_name,

    -- * ColumnInfo
    ColumnInfo (..),
    newColumnInfo,
    columnInfo_catalogName,
    columnInfo_tableName,
    columnInfo_precision,
    columnInfo_caseSensitive,
    columnInfo_nullable,
    columnInfo_label,
    columnInfo_schemaName,
    columnInfo_scale,
    columnInfo_name,
    columnInfo_type,

    -- * DataCatalog
    DataCatalog (..),
    newDataCatalog,
    dataCatalog_description,
    dataCatalog_parameters,
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
    database_description,
    database_parameters,
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
    namedQuery_workGroup,
    namedQuery_description,
    namedQuery_name,
    namedQuery_database,
    namedQuery_queryString,

    -- * QueryExecution
    QueryExecution (..),
    newQueryExecution,
    queryExecution_status,
    queryExecution_queryExecutionId,
    queryExecution_statistics,
    queryExecution_query,
    queryExecution_queryExecutionContext,
    queryExecution_engineVersion,
    queryExecution_resultConfiguration,
    queryExecution_workGroup,
    queryExecution_statementType,

    -- * QueryExecutionContext
    QueryExecutionContext (..),
    newQueryExecutionContext,
    queryExecutionContext_catalog,
    queryExecutionContext_database,

    -- * QueryExecutionStatistics
    QueryExecutionStatistics (..),
    newQueryExecutionStatistics,
    queryExecutionStatistics_totalExecutionTimeInMillis,
    queryExecutionStatistics_serviceProcessingTimeInMillis,
    queryExecutionStatistics_queryQueueTimeInMillis,
    queryExecutionStatistics_dataScannedInBytes,
    queryExecutionStatistics_queryPlanningTimeInMillis,
    queryExecutionStatistics_engineExecutionTimeInMillis,
    queryExecutionStatistics_dataManifestLocation,

    -- * QueryExecutionStatus
    QueryExecutionStatus (..),
    newQueryExecutionStatus,
    queryExecutionStatus_submissionDateTime,
    queryExecutionStatus_stateChangeReason,
    queryExecutionStatus_completionDateTime,
    queryExecutionStatus_state,

    -- * ResultConfiguration
    ResultConfiguration (..),
    newResultConfiguration,
    resultConfiguration_encryptionConfiguration,
    resultConfiguration_outputLocation,

    -- * ResultConfigurationUpdates
    ResultConfigurationUpdates (..),
    newResultConfigurationUpdates,
    resultConfigurationUpdates_encryptionConfiguration,
    resultConfigurationUpdates_removeOutputLocation,
    resultConfigurationUpdates_removeEncryptionConfiguration,
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
    tableMetadata_createTime,
    tableMetadata_partitionKeys,
    tableMetadata_lastAccessTime,
    tableMetadata_columns,
    tableMetadata_parameters,
    tableMetadata_name,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * UnprocessedNamedQueryId
    UnprocessedNamedQueryId (..),
    newUnprocessedNamedQueryId,
    unprocessedNamedQueryId_namedQueryId,
    unprocessedNamedQueryId_errorMessage,
    unprocessedNamedQueryId_errorCode,

    -- * UnprocessedQueryExecutionId
    UnprocessedQueryExecutionId (..),
    newUnprocessedQueryExecutionId,
    unprocessedQueryExecutionId_queryExecutionId,
    unprocessedQueryExecutionId_errorMessage,
    unprocessedQueryExecutionId_errorCode,

    -- * WorkGroup
    WorkGroup (..),
    newWorkGroup,
    workGroup_creationTime,
    workGroup_configuration,
    workGroup_state,
    workGroup_description,
    workGroup_name,

    -- * WorkGroupConfiguration
    WorkGroupConfiguration (..),
    newWorkGroupConfiguration,
    workGroupConfiguration_bytesScannedCutoffPerQuery,
    workGroupConfiguration_publishCloudWatchMetricsEnabled,
    workGroupConfiguration_enforceWorkGroupConfiguration,
    workGroupConfiguration_requesterPaysEnabled,
    workGroupConfiguration_engineVersion,
    workGroupConfiguration_resultConfiguration,

    -- * WorkGroupConfigurationUpdates
    WorkGroupConfigurationUpdates (..),
    newWorkGroupConfigurationUpdates,
    workGroupConfigurationUpdates_bytesScannedCutoffPerQuery,
    workGroupConfigurationUpdates_resultConfigurationUpdates,
    workGroupConfigurationUpdates_publishCloudWatchMetricsEnabled,
    workGroupConfigurationUpdates_enforceWorkGroupConfiguration,
    workGroupConfigurationUpdates_requesterPaysEnabled,
    workGroupConfigurationUpdates_removeBytesScannedCutoffPerQuery,
    workGroupConfigurationUpdates_engineVersion,

    -- * WorkGroupSummary
    WorkGroupSummary (..),
    newWorkGroupSummary,
    workGroupSummary_creationTime,
    workGroupSummary_state,
    workGroupSummary_name,
    workGroupSummary_engineVersion,
    workGroupSummary_description,
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
import Network.AWS.Athena.Types.EngineVersion
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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign

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
      Core._serviceTimeout = Core.Just 70,
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
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | Indicates that something is wrong with the input to the request. For
-- example, a required parameter may be missing or out of range.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | A resource, such as a workgroup, was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | Indicates a platform issue, which may be due to a transient condition or
-- outage.
_InternalServerException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | An exception that Athena received when it called a custom metastore.
-- Occurs if the error is not caused by user input
-- (@InvalidRequestException@) or from the Athena platform
-- (@InternalServerException@). For example, if a user-created Lambda
-- function is missing permissions, the Lambda @4XX@ exception is returned
-- in a @MetadataException@.
_MetadataException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MetadataException =
  Core._MatchServiceError
    defaultService
    "MetadataException"

-- | Indicates that the request was throttled.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"

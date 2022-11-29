{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Athena.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _MetadataException,
    _InternalServerException,
    _ResourceNotFoundException,
    _TooManyRequestsException,
    _InvalidRequestException,

    -- * ColumnNullable
    ColumnNullable (..),

    -- * DataCatalogType
    DataCatalogType (..),

    -- * EncryptionOption
    EncryptionOption (..),

    -- * QueryExecutionState
    QueryExecutionState (..),

    -- * S3AclOption
    S3AclOption (..),

    -- * StatementType
    StatementType (..),

    -- * WorkGroupState
    WorkGroupState (..),

    -- * AclConfiguration
    AclConfiguration (..),
    newAclConfiguration,
    aclConfiguration_s3AclOption,

    -- * AthenaError
    AthenaError (..),
    newAthenaError,
    athenaError_retryable,
    athenaError_errorCategory,
    athenaError_errorMessage,
    athenaError_errorType,

    -- * Column
    Column (..),
    newColumn,
    column_type,
    column_comment,
    column_name,

    -- * ColumnInfo
    ColumnInfo (..),
    newColumnInfo,
    columnInfo_tableName,
    columnInfo_catalogName,
    columnInfo_label,
    columnInfo_caseSensitive,
    columnInfo_schemaName,
    columnInfo_nullable,
    columnInfo_precision,
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
    dataCatalogSummary_type,
    dataCatalogSummary_catalogName,

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
    engineVersion_selectedEngineVersion,
    engineVersion_effectiveEngineVersion,

    -- * NamedQuery
    NamedQuery (..),
    newNamedQuery,
    namedQuery_workGroup,
    namedQuery_description,
    namedQuery_namedQueryId,
    namedQuery_name,
    namedQuery_database,
    namedQuery_queryString,

    -- * PreparedStatement
    PreparedStatement (..),
    newPreparedStatement,
    preparedStatement_workGroupName,
    preparedStatement_description,
    preparedStatement_lastModifiedTime,
    preparedStatement_queryStatement,
    preparedStatement_statementName,

    -- * PreparedStatementSummary
    PreparedStatementSummary (..),
    newPreparedStatementSummary,
    preparedStatementSummary_lastModifiedTime,
    preparedStatementSummary_statementName,

    -- * QueryExecution
    QueryExecution (..),
    newQueryExecution,
    queryExecution_resultReuseConfiguration,
    queryExecution_queryExecutionId,
    queryExecution_statistics,
    queryExecution_statementType,
    queryExecution_workGroup,
    queryExecution_status,
    queryExecution_resultConfiguration,
    queryExecution_query,
    queryExecution_queryExecutionContext,
    queryExecution_executionParameters,
    queryExecution_engineVersion,

    -- * QueryExecutionContext
    QueryExecutionContext (..),
    newQueryExecutionContext,
    queryExecutionContext_catalog,
    queryExecutionContext_database,

    -- * QueryExecutionStatistics
    QueryExecutionStatistics (..),
    newQueryExecutionStatistics,
    queryExecutionStatistics_dataScannedInBytes,
    queryExecutionStatistics_queryQueueTimeInMillis,
    queryExecutionStatistics_resultReuseInformation,
    queryExecutionStatistics_serviceProcessingTimeInMillis,
    queryExecutionStatistics_dataManifestLocation,
    queryExecutionStatistics_totalExecutionTimeInMillis,
    queryExecutionStatistics_engineExecutionTimeInMillis,
    queryExecutionStatistics_queryPlanningTimeInMillis,

    -- * QueryExecutionStatus
    QueryExecutionStatus (..),
    newQueryExecutionStatus,
    queryExecutionStatus_stateChangeReason,
    queryExecutionStatus_submissionDateTime,
    queryExecutionStatus_state,
    queryExecutionStatus_athenaError,
    queryExecutionStatus_completionDateTime,

    -- * QueryRuntimeStatistics
    QueryRuntimeStatistics (..),
    newQueryRuntimeStatistics,
    queryRuntimeStatistics_rows,
    queryRuntimeStatistics_timeline,
    queryRuntimeStatistics_outputStage,

    -- * QueryRuntimeStatisticsRows
    QueryRuntimeStatisticsRows (..),
    newQueryRuntimeStatisticsRows,
    queryRuntimeStatisticsRows_inputBytes,
    queryRuntimeStatisticsRows_outputBytes,
    queryRuntimeStatisticsRows_inputRows,
    queryRuntimeStatisticsRows_outputRows,

    -- * QueryRuntimeStatisticsTimeline
    QueryRuntimeStatisticsTimeline (..),
    newQueryRuntimeStatisticsTimeline,
    queryRuntimeStatisticsTimeline_queryQueueTimeInMillis,
    queryRuntimeStatisticsTimeline_serviceProcessingTimeInMillis,
    queryRuntimeStatisticsTimeline_totalExecutionTimeInMillis,
    queryRuntimeStatisticsTimeline_engineExecutionTimeInMillis,
    queryRuntimeStatisticsTimeline_queryPlanningTimeInMillis,

    -- * QueryStage
    QueryStage (..),
    newQueryStage,
    queryStage_inputBytes,
    queryStage_outputBytes,
    queryStage_inputRows,
    queryStage_queryStagePlan,
    queryStage_state,
    queryStage_executionTime,
    queryStage_subStages,
    queryStage_stageId,
    queryStage_outputRows,

    -- * QueryStagePlanNode
    QueryStagePlanNode (..),
    newQueryStagePlanNode,
    queryStagePlanNode_name,
    queryStagePlanNode_remoteSources,
    queryStagePlanNode_children,
    queryStagePlanNode_identifier,

    -- * ResultConfiguration
    ResultConfiguration (..),
    newResultConfiguration,
    resultConfiguration_aclConfiguration,
    resultConfiguration_expectedBucketOwner,
    resultConfiguration_outputLocation,
    resultConfiguration_encryptionConfiguration,

    -- * ResultConfigurationUpdates
    ResultConfigurationUpdates (..),
    newResultConfigurationUpdates,
    resultConfigurationUpdates_aclConfiguration,
    resultConfigurationUpdates_removeEncryptionConfiguration,
    resultConfigurationUpdates_expectedBucketOwner,
    resultConfigurationUpdates_removeAclConfiguration,
    resultConfigurationUpdates_outputLocation,
    resultConfigurationUpdates_removeExpectedBucketOwner,
    resultConfigurationUpdates_removeOutputLocation,
    resultConfigurationUpdates_encryptionConfiguration,

    -- * ResultReuseByAgeConfiguration
    ResultReuseByAgeConfiguration (..),
    newResultReuseByAgeConfiguration,
    resultReuseByAgeConfiguration_maxAgeInMinutes,
    resultReuseByAgeConfiguration_enabled,

    -- * ResultReuseConfiguration
    ResultReuseConfiguration (..),
    newResultReuseConfiguration,
    resultReuseConfiguration_resultReuseByAgeConfiguration,

    -- * ResultReuseInformation
    ResultReuseInformation (..),
    newResultReuseInformation,
    resultReuseInformation_reusedPreviousResult,

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
    tableMetadata_columns,
    tableMetadata_lastAccessTime,
    tableMetadata_partitionKeys,
    tableMetadata_tableType,
    tableMetadata_createTime,
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
    unprocessedNamedQueryId_errorMessage,
    unprocessedNamedQueryId_errorCode,
    unprocessedNamedQueryId_namedQueryId,

    -- * UnprocessedPreparedStatementName
    UnprocessedPreparedStatementName (..),
    newUnprocessedPreparedStatementName,
    unprocessedPreparedStatementName_errorMessage,
    unprocessedPreparedStatementName_errorCode,
    unprocessedPreparedStatementName_statementName,

    -- * UnprocessedQueryExecutionId
    UnprocessedQueryExecutionId (..),
    newUnprocessedQueryExecutionId,
    unprocessedQueryExecutionId_queryExecutionId,
    unprocessedQueryExecutionId_errorMessage,
    unprocessedQueryExecutionId_errorCode,

    -- * WorkGroup
    WorkGroup (..),
    newWorkGroup,
    workGroup_configuration,
    workGroup_state,
    workGroup_description,
    workGroup_creationTime,
    workGroup_name,

    -- * WorkGroupConfiguration
    WorkGroupConfiguration (..),
    newWorkGroupConfiguration,
    workGroupConfiguration_publishCloudWatchMetricsEnabled,
    workGroupConfiguration_enforceWorkGroupConfiguration,
    workGroupConfiguration_resultConfiguration,
    workGroupConfiguration_bytesScannedCutoffPerQuery,
    workGroupConfiguration_requesterPaysEnabled,
    workGroupConfiguration_engineVersion,

    -- * WorkGroupConfigurationUpdates
    WorkGroupConfigurationUpdates (..),
    newWorkGroupConfigurationUpdates,
    workGroupConfigurationUpdates_resultConfigurationUpdates,
    workGroupConfigurationUpdates_publishCloudWatchMetricsEnabled,
    workGroupConfigurationUpdates_enforceWorkGroupConfiguration,
    workGroupConfigurationUpdates_removeBytesScannedCutoffPerQuery,
    workGroupConfigurationUpdates_bytesScannedCutoffPerQuery,
    workGroupConfigurationUpdates_requesterPaysEnabled,
    workGroupConfigurationUpdates_engineVersion,

    -- * WorkGroupSummary
    WorkGroupSummary (..),
    newWorkGroupSummary,
    workGroupSummary_name,
    workGroupSummary_state,
    workGroupSummary_description,
    workGroupSummary_creationTime,
    workGroupSummary_engineVersion,
  )
where

import Amazonka.Athena.Types.AclConfiguration
import Amazonka.Athena.Types.AthenaError
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
import Amazonka.Athena.Types.QueryRuntimeStatistics
import Amazonka.Athena.Types.QueryRuntimeStatisticsRows
import Amazonka.Athena.Types.QueryRuntimeStatisticsTimeline
import Amazonka.Athena.Types.QueryStage
import Amazonka.Athena.Types.QueryStagePlanNode
import Amazonka.Athena.Types.ResultConfiguration
import Amazonka.Athena.Types.ResultConfigurationUpdates
import Amazonka.Athena.Types.ResultReuseByAgeConfiguration
import Amazonka.Athena.Types.ResultReuseConfiguration
import Amazonka.Athena.Types.ResultReuseInformation
import Amazonka.Athena.Types.ResultSet
import Amazonka.Athena.Types.ResultSetMetadata
import Amazonka.Athena.Types.Row
import Amazonka.Athena.Types.S3AclOption
import Amazonka.Athena.Types.StatementType
import Amazonka.Athena.Types.TableMetadata
import Amazonka.Athena.Types.Tag
import Amazonka.Athena.Types.UnprocessedNamedQueryId
import Amazonka.Athena.Types.UnprocessedPreparedStatementName
import Amazonka.Athena.Types.UnprocessedQueryExecutionId
import Amazonka.Athena.Types.WorkGroup
import Amazonka.Athena.Types.WorkGroupConfiguration
import Amazonka.Athena.Types.WorkGroupConfigurationUpdates
import Amazonka.Athena.Types.WorkGroupState
import Amazonka.Athena.Types.WorkGroupSummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-05-18@ of the Amazon Athena SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Athena",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "athena",
      Core.signingName = "athena",
      Core.version = "2017-05-18",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Athena",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

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

-- | Indicates that the request was throttled.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"

-- | Indicates that something is wrong with the input to the request. For
-- example, a required parameter may be missing or out of range.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

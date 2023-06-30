{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Athena.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalServerException,
    _InvalidRequestException,
    _MetadataException,
    _ResourceNotFoundException,
    _SessionAlreadyExistsException,
    _TooManyRequestsException,

    -- * CalculationExecutionState
    CalculationExecutionState (..),

    -- * ColumnNullable
    ColumnNullable (..),

    -- * DataCatalogType
    DataCatalogType (..),

    -- * EncryptionOption
    EncryptionOption (..),

    -- * ExecutorState
    ExecutorState (..),

    -- * ExecutorType
    ExecutorType (..),

    -- * NotebookType
    NotebookType (..),

    -- * QueryExecutionState
    QueryExecutionState (..),

    -- * S3AclOption
    S3AclOption (..),

    -- * SessionState
    SessionState (..),

    -- * StatementType
    StatementType (..),

    -- * WorkGroupState
    WorkGroupState (..),

    -- * AclConfiguration
    AclConfiguration (..),
    newAclConfiguration,
    aclConfiguration_s3AclOption,

    -- * ApplicationDPUSizes
    ApplicationDPUSizes (..),
    newApplicationDPUSizes,
    applicationDPUSizes_applicationRuntimeId,
    applicationDPUSizes_supportedDPUSizes,

    -- * AthenaError
    AthenaError (..),
    newAthenaError,
    athenaError_errorCategory,
    athenaError_errorMessage,
    athenaError_errorType,
    athenaError_retryable,

    -- * CalculationConfiguration
    CalculationConfiguration (..),
    newCalculationConfiguration,
    calculationConfiguration_codeBlock,

    -- * CalculationResult
    CalculationResult (..),
    newCalculationResult,
    calculationResult_resultS3Uri,
    calculationResult_resultType,
    calculationResult_stdErrorS3Uri,
    calculationResult_stdOutS3Uri,

    -- * CalculationStatistics
    CalculationStatistics (..),
    newCalculationStatistics,
    calculationStatistics_dpuExecutionInMillis,
    calculationStatistics_progress,

    -- * CalculationStatus
    CalculationStatus (..),
    newCalculationStatus,
    calculationStatus_completionDateTime,
    calculationStatus_state,
    calculationStatus_stateChangeReason,
    calculationStatus_submissionDateTime,

    -- * CalculationSummary
    CalculationSummary (..),
    newCalculationSummary,
    calculationSummary_calculationExecutionId,
    calculationSummary_description,
    calculationSummary_status,

    -- * Column
    Column (..),
    newColumn,
    column_comment,
    column_type,
    column_name,

    -- * ColumnInfo
    ColumnInfo (..),
    newColumnInfo,
    columnInfo_caseSensitive,
    columnInfo_catalogName,
    columnInfo_label,
    columnInfo_nullable,
    columnInfo_precision,
    columnInfo_scale,
    columnInfo_schemaName,
    columnInfo_tableName,
    columnInfo_name,
    columnInfo_type,

    -- * CustomerContentEncryptionConfiguration
    CustomerContentEncryptionConfiguration (..),
    newCustomerContentEncryptionConfiguration,
    customerContentEncryptionConfiguration_kmsKey,

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

    -- * EngineConfiguration
    EngineConfiguration (..),
    newEngineConfiguration,
    engineConfiguration_additionalConfigs,
    engineConfiguration_coordinatorDpuSize,
    engineConfiguration_defaultExecutorDpuSize,
    engineConfiguration_maxConcurrentDpus,

    -- * EngineVersion
    EngineVersion (..),
    newEngineVersion,
    engineVersion_effectiveEngineVersion,
    engineVersion_selectedEngineVersion,

    -- * ExecutorsSummary
    ExecutorsSummary (..),
    newExecutorsSummary,
    executorsSummary_executorSize,
    executorsSummary_executorState,
    executorsSummary_executorType,
    executorsSummary_startDateTime,
    executorsSummary_terminationDateTime,
    executorsSummary_executorId,

    -- * FilterDefinition
    FilterDefinition (..),
    newFilterDefinition,
    filterDefinition_name,

    -- * NamedQuery
    NamedQuery (..),
    newNamedQuery,
    namedQuery_description,
    namedQuery_namedQueryId,
    namedQuery_workGroup,
    namedQuery_name,
    namedQuery_database,
    namedQuery_queryString,

    -- * NotebookMetadata
    NotebookMetadata (..),
    newNotebookMetadata,
    notebookMetadata_creationTime,
    notebookMetadata_lastModifiedTime,
    notebookMetadata_name,
    notebookMetadata_notebookId,
    notebookMetadata_type,
    notebookMetadata_workGroup,

    -- * NotebookSessionSummary
    NotebookSessionSummary (..),
    newNotebookSessionSummary,
    notebookSessionSummary_creationTime,
    notebookSessionSummary_sessionId,

    -- * PreparedStatement
    PreparedStatement (..),
    newPreparedStatement,
    preparedStatement_description,
    preparedStatement_lastModifiedTime,
    preparedStatement_queryStatement,
    preparedStatement_statementName,
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
    queryExecution_executionParameters,
    queryExecution_query,
    queryExecution_queryExecutionContext,
    queryExecution_queryExecutionId,
    queryExecution_resultConfiguration,
    queryExecution_resultReuseConfiguration,
    queryExecution_statementType,
    queryExecution_statistics,
    queryExecution_status,
    queryExecution_workGroup,

    -- * QueryExecutionContext
    QueryExecutionContext (..),
    newQueryExecutionContext,
    queryExecutionContext_catalog,
    queryExecutionContext_database,

    -- * QueryExecutionStatistics
    QueryExecutionStatistics (..),
    newQueryExecutionStatistics,
    queryExecutionStatistics_dataManifestLocation,
    queryExecutionStatistics_dataScannedInBytes,
    queryExecutionStatistics_engineExecutionTimeInMillis,
    queryExecutionStatistics_queryPlanningTimeInMillis,
    queryExecutionStatistics_queryQueueTimeInMillis,
    queryExecutionStatistics_resultReuseInformation,
    queryExecutionStatistics_serviceProcessingTimeInMillis,
    queryExecutionStatistics_totalExecutionTimeInMillis,

    -- * QueryExecutionStatus
    QueryExecutionStatus (..),
    newQueryExecutionStatus,
    queryExecutionStatus_athenaError,
    queryExecutionStatus_completionDateTime,
    queryExecutionStatus_state,
    queryExecutionStatus_stateChangeReason,
    queryExecutionStatus_submissionDateTime,

    -- * QueryRuntimeStatistics
    QueryRuntimeStatistics (..),
    newQueryRuntimeStatistics,
    queryRuntimeStatistics_outputStage,
    queryRuntimeStatistics_rows,
    queryRuntimeStatistics_timeline,

    -- * QueryRuntimeStatisticsRows
    QueryRuntimeStatisticsRows (..),
    newQueryRuntimeStatisticsRows,
    queryRuntimeStatisticsRows_inputBytes,
    queryRuntimeStatisticsRows_inputRows,
    queryRuntimeStatisticsRows_outputBytes,
    queryRuntimeStatisticsRows_outputRows,

    -- * QueryRuntimeStatisticsTimeline
    QueryRuntimeStatisticsTimeline (..),
    newQueryRuntimeStatisticsTimeline,
    queryRuntimeStatisticsTimeline_engineExecutionTimeInMillis,
    queryRuntimeStatisticsTimeline_queryPlanningTimeInMillis,
    queryRuntimeStatisticsTimeline_queryQueueTimeInMillis,
    queryRuntimeStatisticsTimeline_serviceProcessingTimeInMillis,
    queryRuntimeStatisticsTimeline_totalExecutionTimeInMillis,

    -- * QueryStage
    QueryStage (..),
    newQueryStage,
    queryStage_executionTime,
    queryStage_inputBytes,
    queryStage_inputRows,
    queryStage_outputBytes,
    queryStage_outputRows,
    queryStage_queryStagePlan,
    queryStage_stageId,
    queryStage_state,
    queryStage_subStages,

    -- * QueryStagePlanNode
    QueryStagePlanNode (..),
    newQueryStagePlanNode,
    queryStagePlanNode_children,
    queryStagePlanNode_identifier,
    queryStagePlanNode_name,
    queryStagePlanNode_remoteSources,

    -- * ResultConfiguration
    ResultConfiguration (..),
    newResultConfiguration,
    resultConfiguration_aclConfiguration,
    resultConfiguration_encryptionConfiguration,
    resultConfiguration_expectedBucketOwner,
    resultConfiguration_outputLocation,

    -- * ResultConfigurationUpdates
    ResultConfigurationUpdates (..),
    newResultConfigurationUpdates,
    resultConfigurationUpdates_aclConfiguration,
    resultConfigurationUpdates_encryptionConfiguration,
    resultConfigurationUpdates_expectedBucketOwner,
    resultConfigurationUpdates_outputLocation,
    resultConfigurationUpdates_removeAclConfiguration,
    resultConfigurationUpdates_removeEncryptionConfiguration,
    resultConfigurationUpdates_removeExpectedBucketOwner,
    resultConfigurationUpdates_removeOutputLocation,

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
    resultSet_resultSetMetadata,
    resultSet_rows,

    -- * ResultSetMetadata
    ResultSetMetadata (..),
    newResultSetMetadata,
    resultSetMetadata_columnInfo,

    -- * Row
    Row (..),
    newRow,
    row_data,

    -- * SessionConfiguration
    SessionConfiguration (..),
    newSessionConfiguration,
    sessionConfiguration_encryptionConfiguration,
    sessionConfiguration_executionRole,
    sessionConfiguration_idleTimeoutSeconds,
    sessionConfiguration_workingDirectory,

    -- * SessionStatistics
    SessionStatistics (..),
    newSessionStatistics,
    sessionStatistics_dpuExecutionInMillis,

    -- * SessionStatus
    SessionStatus (..),
    newSessionStatus,
    sessionStatus_endDateTime,
    sessionStatus_idleSinceDateTime,
    sessionStatus_lastModifiedDateTime,
    sessionStatus_startDateTime,
    sessionStatus_state,
    sessionStatus_stateChangeReason,

    -- * SessionSummary
    SessionSummary (..),
    newSessionSummary,
    sessionSummary_description,
    sessionSummary_engineVersion,
    sessionSummary_notebookVersion,
    sessionSummary_sessionId,
    sessionSummary_status,

    -- * TableMetadata
    TableMetadata (..),
    newTableMetadata,
    tableMetadata_columns,
    tableMetadata_createTime,
    tableMetadata_lastAccessTime,
    tableMetadata_parameters,
    tableMetadata_partitionKeys,
    tableMetadata_tableType,
    tableMetadata_name,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * UnprocessedNamedQueryId
    UnprocessedNamedQueryId (..),
    newUnprocessedNamedQueryId,
    unprocessedNamedQueryId_errorCode,
    unprocessedNamedQueryId_errorMessage,
    unprocessedNamedQueryId_namedQueryId,

    -- * UnprocessedPreparedStatementName
    UnprocessedPreparedStatementName (..),
    newUnprocessedPreparedStatementName,
    unprocessedPreparedStatementName_errorCode,
    unprocessedPreparedStatementName_errorMessage,
    unprocessedPreparedStatementName_statementName,

    -- * UnprocessedQueryExecutionId
    UnprocessedQueryExecutionId (..),
    newUnprocessedQueryExecutionId,
    unprocessedQueryExecutionId_errorCode,
    unprocessedQueryExecutionId_errorMessage,
    unprocessedQueryExecutionId_queryExecutionId,

    -- * WorkGroup
    WorkGroup (..),
    newWorkGroup,
    workGroup_configuration,
    workGroup_creationTime,
    workGroup_description,
    workGroup_state,
    workGroup_name,

    -- * WorkGroupConfiguration
    WorkGroupConfiguration (..),
    newWorkGroupConfiguration,
    workGroupConfiguration_additionalConfiguration,
    workGroupConfiguration_bytesScannedCutoffPerQuery,
    workGroupConfiguration_customerContentEncryptionConfiguration,
    workGroupConfiguration_enforceWorkGroupConfiguration,
    workGroupConfiguration_engineVersion,
    workGroupConfiguration_executionRole,
    workGroupConfiguration_publishCloudWatchMetricsEnabled,
    workGroupConfiguration_requesterPaysEnabled,
    workGroupConfiguration_resultConfiguration,

    -- * WorkGroupConfigurationUpdates
    WorkGroupConfigurationUpdates (..),
    newWorkGroupConfigurationUpdates,
    workGroupConfigurationUpdates_additionalConfiguration,
    workGroupConfigurationUpdates_bytesScannedCutoffPerQuery,
    workGroupConfigurationUpdates_customerContentEncryptionConfiguration,
    workGroupConfigurationUpdates_enforceWorkGroupConfiguration,
    workGroupConfigurationUpdates_engineVersion,
    workGroupConfigurationUpdates_executionRole,
    workGroupConfigurationUpdates_publishCloudWatchMetricsEnabled,
    workGroupConfigurationUpdates_removeBytesScannedCutoffPerQuery,
    workGroupConfigurationUpdates_removeCustomerContentEncryptionConfiguration,
    workGroupConfigurationUpdates_requesterPaysEnabled,
    workGroupConfigurationUpdates_resultConfigurationUpdates,

    -- * WorkGroupSummary
    WorkGroupSummary (..),
    newWorkGroupSummary,
    workGroupSummary_creationTime,
    workGroupSummary_description,
    workGroupSummary_engineVersion,
    workGroupSummary_name,
    workGroupSummary_state,
  )
where

import Amazonka.Athena.Types.AclConfiguration
import Amazonka.Athena.Types.ApplicationDPUSizes
import Amazonka.Athena.Types.AthenaError
import Amazonka.Athena.Types.CalculationConfiguration
import Amazonka.Athena.Types.CalculationExecutionState
import Amazonka.Athena.Types.CalculationResult
import Amazonka.Athena.Types.CalculationStatistics
import Amazonka.Athena.Types.CalculationStatus
import Amazonka.Athena.Types.CalculationSummary
import Amazonka.Athena.Types.Column
import Amazonka.Athena.Types.ColumnInfo
import Amazonka.Athena.Types.ColumnNullable
import Amazonka.Athena.Types.CustomerContentEncryptionConfiguration
import Amazonka.Athena.Types.DataCatalog
import Amazonka.Athena.Types.DataCatalogSummary
import Amazonka.Athena.Types.DataCatalogType
import Amazonka.Athena.Types.Database
import Amazonka.Athena.Types.Datum
import Amazonka.Athena.Types.EncryptionConfiguration
import Amazonka.Athena.Types.EncryptionOption
import Amazonka.Athena.Types.EngineConfiguration
import Amazonka.Athena.Types.EngineVersion
import Amazonka.Athena.Types.ExecutorState
import Amazonka.Athena.Types.ExecutorType
import Amazonka.Athena.Types.ExecutorsSummary
import Amazonka.Athena.Types.FilterDefinition
import Amazonka.Athena.Types.NamedQuery
import Amazonka.Athena.Types.NotebookMetadata
import Amazonka.Athena.Types.NotebookSessionSummary
import Amazonka.Athena.Types.NotebookType
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
import Amazonka.Athena.Types.SessionConfiguration
import Amazonka.Athena.Types.SessionState
import Amazonka.Athena.Types.SessionStatistics
import Amazonka.Athena.Types.SessionStatus
import Amazonka.Athena.Types.SessionSummary
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
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | Indicates a platform issue, which may be due to a transient condition or
-- outage.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | Indicates that something is wrong with the input to the request. For
-- example, a required parameter may be missing or out of range.
_InvalidRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
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
_MetadataException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MetadataException =
  Core._MatchServiceError
    defaultService
    "MetadataException"

-- | A resource, such as a workgroup, was not found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The specified session already exists.
_SessionAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SessionAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "SessionAlreadyExistsException"

-- | Indicates that the request was throttled.
_TooManyRequestsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"

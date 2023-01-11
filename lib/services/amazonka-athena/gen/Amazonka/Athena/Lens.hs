{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Athena.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Lens
  ( -- * Operations

    -- ** BatchGetNamedQuery
    batchGetNamedQuery_namedQueryIds,
    batchGetNamedQueryResponse_namedQueries,
    batchGetNamedQueryResponse_unprocessedNamedQueryIds,
    batchGetNamedQueryResponse_httpStatus,

    -- ** BatchGetPreparedStatement
    batchGetPreparedStatement_preparedStatementNames,
    batchGetPreparedStatement_workGroup,
    batchGetPreparedStatementResponse_preparedStatements,
    batchGetPreparedStatementResponse_unprocessedPreparedStatementNames,
    batchGetPreparedStatementResponse_httpStatus,

    -- ** BatchGetQueryExecution
    batchGetQueryExecution_queryExecutionIds,
    batchGetQueryExecutionResponse_queryExecutions,
    batchGetQueryExecutionResponse_unprocessedQueryExecutionIds,
    batchGetQueryExecutionResponse_httpStatus,

    -- ** CreateDataCatalog
    createDataCatalog_description,
    createDataCatalog_parameters,
    createDataCatalog_tags,
    createDataCatalog_name,
    createDataCatalog_type,
    createDataCatalogResponse_httpStatus,

    -- ** CreateNamedQuery
    createNamedQuery_clientRequestToken,
    createNamedQuery_description,
    createNamedQuery_workGroup,
    createNamedQuery_name,
    createNamedQuery_database,
    createNamedQuery_queryString,
    createNamedQueryResponse_namedQueryId,
    createNamedQueryResponse_httpStatus,

    -- ** CreateNotebook
    createNotebook_clientRequestToken,
    createNotebook_workGroup,
    createNotebook_name,
    createNotebookResponse_notebookId,
    createNotebookResponse_httpStatus,

    -- ** CreatePreparedStatement
    createPreparedStatement_description,
    createPreparedStatement_statementName,
    createPreparedStatement_workGroup,
    createPreparedStatement_queryStatement,
    createPreparedStatementResponse_httpStatus,

    -- ** CreatePresignedNotebookUrl
    createPresignedNotebookUrl_sessionId,
    createPresignedNotebookUrlResponse_httpStatus,
    createPresignedNotebookUrlResponse_notebookUrl,
    createPresignedNotebookUrlResponse_authToken,
    createPresignedNotebookUrlResponse_authTokenExpirationTime,

    -- ** CreateWorkGroup
    createWorkGroup_configuration,
    createWorkGroup_description,
    createWorkGroup_tags,
    createWorkGroup_name,
    createWorkGroupResponse_httpStatus,

    -- ** DeleteDataCatalog
    deleteDataCatalog_name,
    deleteDataCatalogResponse_httpStatus,

    -- ** DeleteNamedQuery
    deleteNamedQuery_namedQueryId,
    deleteNamedQueryResponse_httpStatus,

    -- ** DeleteNotebook
    deleteNotebook_notebookId,
    deleteNotebookResponse_httpStatus,

    -- ** DeletePreparedStatement
    deletePreparedStatement_statementName,
    deletePreparedStatement_workGroup,
    deletePreparedStatementResponse_httpStatus,

    -- ** DeleteWorkGroup
    deleteWorkGroup_recursiveDeleteOption,
    deleteWorkGroup_workGroup,
    deleteWorkGroupResponse_httpStatus,

    -- ** ExportNotebook
    exportNotebook_notebookId,
    exportNotebookResponse_notebookMetadata,
    exportNotebookResponse_payload,
    exportNotebookResponse_httpStatus,

    -- ** GetCalculationExecution
    getCalculationExecution_calculationExecutionId,
    getCalculationExecutionResponse_calculationExecutionId,
    getCalculationExecutionResponse_description,
    getCalculationExecutionResponse_result,
    getCalculationExecutionResponse_sessionId,
    getCalculationExecutionResponse_statistics,
    getCalculationExecutionResponse_status,
    getCalculationExecutionResponse_workingDirectory,
    getCalculationExecutionResponse_httpStatus,

    -- ** GetCalculationExecutionCode
    getCalculationExecutionCode_calculationExecutionId,
    getCalculationExecutionCodeResponse_codeBlock,
    getCalculationExecutionCodeResponse_httpStatus,

    -- ** GetCalculationExecutionStatus
    getCalculationExecutionStatus_calculationExecutionId,
    getCalculationExecutionStatusResponse_statistics,
    getCalculationExecutionStatusResponse_status,
    getCalculationExecutionStatusResponse_httpStatus,

    -- ** GetDataCatalog
    getDataCatalog_name,
    getDataCatalogResponse_dataCatalog,
    getDataCatalogResponse_httpStatus,

    -- ** GetDatabase
    getDatabase_catalogName,
    getDatabase_databaseName,
    getDatabaseResponse_database,
    getDatabaseResponse_httpStatus,

    -- ** GetNamedQuery
    getNamedQuery_namedQueryId,
    getNamedQueryResponse_namedQuery,
    getNamedQueryResponse_httpStatus,

    -- ** GetNotebookMetadata
    getNotebookMetadata_notebookId,
    getNotebookMetadataResponse_notebookMetadata,
    getNotebookMetadataResponse_httpStatus,

    -- ** GetPreparedStatement
    getPreparedStatement_statementName,
    getPreparedStatement_workGroup,
    getPreparedStatementResponse_preparedStatement,
    getPreparedStatementResponse_httpStatus,

    -- ** GetQueryExecution
    getQueryExecution_queryExecutionId,
    getQueryExecutionResponse_queryExecution,
    getQueryExecutionResponse_httpStatus,

    -- ** GetQueryResults
    getQueryResults_maxResults,
    getQueryResults_nextToken,
    getQueryResults_queryExecutionId,
    getQueryResultsResponse_nextToken,
    getQueryResultsResponse_resultSet,
    getQueryResultsResponse_updateCount,
    getQueryResultsResponse_httpStatus,

    -- ** GetQueryRuntimeStatistics
    getQueryRuntimeStatistics_queryExecutionId,
    getQueryRuntimeStatisticsResponse_queryRuntimeStatistics,
    getQueryRuntimeStatisticsResponse_httpStatus,

    -- ** GetSession
    getSession_sessionId,
    getSessionResponse_description,
    getSessionResponse_engineConfiguration,
    getSessionResponse_engineVersion,
    getSessionResponse_notebookVersion,
    getSessionResponse_sessionConfiguration,
    getSessionResponse_sessionId,
    getSessionResponse_statistics,
    getSessionResponse_status,
    getSessionResponse_workGroup,
    getSessionResponse_httpStatus,

    -- ** GetSessionStatus
    getSessionStatus_sessionId,
    getSessionStatusResponse_sessionId,
    getSessionStatusResponse_status,
    getSessionStatusResponse_httpStatus,

    -- ** GetTableMetadata
    getTableMetadata_catalogName,
    getTableMetadata_databaseName,
    getTableMetadata_tableName,
    getTableMetadataResponse_tableMetadata,
    getTableMetadataResponse_httpStatus,

    -- ** GetWorkGroup
    getWorkGroup_workGroup,
    getWorkGroupResponse_workGroup,
    getWorkGroupResponse_httpStatus,

    -- ** ImportNotebook
    importNotebook_clientRequestToken,
    importNotebook_workGroup,
    importNotebook_name,
    importNotebook_payload,
    importNotebook_type,
    importNotebookResponse_notebookId,
    importNotebookResponse_httpStatus,

    -- ** ListApplicationDPUSizes
    listApplicationDPUSizes_maxResults,
    listApplicationDPUSizes_nextToken,
    listApplicationDPUSizesResponse_applicationDPUSizes,
    listApplicationDPUSizesResponse_nextToken,
    listApplicationDPUSizesResponse_httpStatus,

    -- ** ListCalculationExecutions
    listCalculationExecutions_maxResults,
    listCalculationExecutions_nextToken,
    listCalculationExecutions_stateFilter,
    listCalculationExecutions_sessionId,
    listCalculationExecutionsResponse_calculations,
    listCalculationExecutionsResponse_nextToken,
    listCalculationExecutionsResponse_httpStatus,

    -- ** ListDataCatalogs
    listDataCatalogs_maxResults,
    listDataCatalogs_nextToken,
    listDataCatalogsResponse_dataCatalogsSummary,
    listDataCatalogsResponse_nextToken,
    listDataCatalogsResponse_httpStatus,

    -- ** ListDatabases
    listDatabases_maxResults,
    listDatabases_nextToken,
    listDatabases_catalogName,
    listDatabasesResponse_databaseList,
    listDatabasesResponse_nextToken,
    listDatabasesResponse_httpStatus,

    -- ** ListEngineVersions
    listEngineVersions_maxResults,
    listEngineVersions_nextToken,
    listEngineVersionsResponse_engineVersions,
    listEngineVersionsResponse_nextToken,
    listEngineVersionsResponse_httpStatus,

    -- ** ListExecutors
    listExecutors_executorStateFilter,
    listExecutors_maxResults,
    listExecutors_nextToken,
    listExecutors_sessionId,
    listExecutorsResponse_executorsSummary,
    listExecutorsResponse_nextToken,
    listExecutorsResponse_httpStatus,
    listExecutorsResponse_sessionId,

    -- ** ListNamedQueries
    listNamedQueries_maxResults,
    listNamedQueries_nextToken,
    listNamedQueries_workGroup,
    listNamedQueriesResponse_namedQueryIds,
    listNamedQueriesResponse_nextToken,
    listNamedQueriesResponse_httpStatus,

    -- ** ListNotebookMetadata
    listNotebookMetadata_filters,
    listNotebookMetadata_maxResults,
    listNotebookMetadata_nextToken,
    listNotebookMetadata_workGroup,
    listNotebookMetadataResponse_nextToken,
    listNotebookMetadataResponse_notebookMetadataList,
    listNotebookMetadataResponse_httpStatus,

    -- ** ListNotebookSessions
    listNotebookSessions_maxResults,
    listNotebookSessions_nextToken,
    listNotebookSessions_notebookId,
    listNotebookSessionsResponse_nextToken,
    listNotebookSessionsResponse_httpStatus,
    listNotebookSessionsResponse_notebookSessionsList,

    -- ** ListPreparedStatements
    listPreparedStatements_maxResults,
    listPreparedStatements_nextToken,
    listPreparedStatements_workGroup,
    listPreparedStatementsResponse_nextToken,
    listPreparedStatementsResponse_preparedStatements,
    listPreparedStatementsResponse_httpStatus,

    -- ** ListQueryExecutions
    listQueryExecutions_maxResults,
    listQueryExecutions_nextToken,
    listQueryExecutions_workGroup,
    listQueryExecutionsResponse_nextToken,
    listQueryExecutionsResponse_queryExecutionIds,
    listQueryExecutionsResponse_httpStatus,

    -- ** ListSessions
    listSessions_maxResults,
    listSessions_nextToken,
    listSessions_stateFilter,
    listSessions_workGroup,
    listSessionsResponse_nextToken,
    listSessionsResponse_sessions,
    listSessionsResponse_httpStatus,

    -- ** ListTableMetadata
    listTableMetadata_expression,
    listTableMetadata_maxResults,
    listTableMetadata_nextToken,
    listTableMetadata_catalogName,
    listTableMetadata_databaseName,
    listTableMetadataResponse_nextToken,
    listTableMetadataResponse_tableMetadataList,
    listTableMetadataResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_maxResults,
    listTagsForResource_nextToken,
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListWorkGroups
    listWorkGroups_maxResults,
    listWorkGroups_nextToken,
    listWorkGroupsResponse_nextToken,
    listWorkGroupsResponse_workGroups,
    listWorkGroupsResponse_httpStatus,

    -- ** StartCalculationExecution
    startCalculationExecution_calculationConfiguration,
    startCalculationExecution_clientRequestToken,
    startCalculationExecution_codeBlock,
    startCalculationExecution_description,
    startCalculationExecution_sessionId,
    startCalculationExecutionResponse_calculationExecutionId,
    startCalculationExecutionResponse_state,
    startCalculationExecutionResponse_httpStatus,

    -- ** StartQueryExecution
    startQueryExecution_clientRequestToken,
    startQueryExecution_executionParameters,
    startQueryExecution_queryExecutionContext,
    startQueryExecution_resultConfiguration,
    startQueryExecution_resultReuseConfiguration,
    startQueryExecution_workGroup,
    startQueryExecution_queryString,
    startQueryExecutionResponse_queryExecutionId,
    startQueryExecutionResponse_httpStatus,

    -- ** StartSession
    startSession_clientRequestToken,
    startSession_description,
    startSession_notebookVersion,
    startSession_sessionIdleTimeoutInMinutes,
    startSession_workGroup,
    startSession_engineConfiguration,
    startSessionResponse_sessionId,
    startSessionResponse_state,
    startSessionResponse_httpStatus,

    -- ** StopCalculationExecution
    stopCalculationExecution_calculationExecutionId,
    stopCalculationExecutionResponse_state,
    stopCalculationExecutionResponse_httpStatus,

    -- ** StopQueryExecution
    stopQueryExecution_queryExecutionId,
    stopQueryExecutionResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** TerminateSession
    terminateSession_sessionId,
    terminateSessionResponse_state,
    terminateSessionResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateDataCatalog
    updateDataCatalog_description,
    updateDataCatalog_parameters,
    updateDataCatalog_name,
    updateDataCatalog_type,
    updateDataCatalogResponse_httpStatus,

    -- ** UpdateNamedQuery
    updateNamedQuery_description,
    updateNamedQuery_namedQueryId,
    updateNamedQuery_name,
    updateNamedQuery_queryString,
    updateNamedQueryResponse_httpStatus,

    -- ** UpdateNotebook
    updateNotebook_clientRequestToken,
    updateNotebook_sessionId,
    updateNotebook_notebookId,
    updateNotebook_payload,
    updateNotebook_type,
    updateNotebookResponse_httpStatus,

    -- ** UpdateNotebookMetadata
    updateNotebookMetadata_clientRequestToken,
    updateNotebookMetadata_notebookId,
    updateNotebookMetadata_name,
    updateNotebookMetadataResponse_httpStatus,

    -- ** UpdatePreparedStatement
    updatePreparedStatement_description,
    updatePreparedStatement_statementName,
    updatePreparedStatement_workGroup,
    updatePreparedStatement_queryStatement,
    updatePreparedStatementResponse_httpStatus,

    -- ** UpdateWorkGroup
    updateWorkGroup_configurationUpdates,
    updateWorkGroup_description,
    updateWorkGroup_state,
    updateWorkGroup_workGroup,
    updateWorkGroupResponse_httpStatus,

    -- * Types

    -- ** AclConfiguration
    aclConfiguration_s3AclOption,

    -- ** ApplicationDPUSizes
    applicationDPUSizes_applicationRuntimeId,
    applicationDPUSizes_supportedDPUSizes,

    -- ** AthenaError
    athenaError_errorCategory,
    athenaError_errorMessage,
    athenaError_errorType,
    athenaError_retryable,

    -- ** CalculationConfiguration
    calculationConfiguration_codeBlock,

    -- ** CalculationResult
    calculationResult_resultS3Uri,
    calculationResult_resultType,
    calculationResult_stdErrorS3Uri,
    calculationResult_stdOutS3Uri,

    -- ** CalculationStatistics
    calculationStatistics_dpuExecutionInMillis,
    calculationStatistics_progress,

    -- ** CalculationStatus
    calculationStatus_completionDateTime,
    calculationStatus_state,
    calculationStatus_stateChangeReason,
    calculationStatus_submissionDateTime,

    -- ** CalculationSummary
    calculationSummary_calculationExecutionId,
    calculationSummary_description,
    calculationSummary_status,

    -- ** Column
    column_comment,
    column_type,
    column_name,

    -- ** ColumnInfo
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

    -- ** CustomerContentEncryptionConfiguration
    customerContentEncryptionConfiguration_kmsKey,

    -- ** DataCatalog
    dataCatalog_description,
    dataCatalog_parameters,
    dataCatalog_name,
    dataCatalog_type,

    -- ** DataCatalogSummary
    dataCatalogSummary_catalogName,
    dataCatalogSummary_type,

    -- ** Database
    database_description,
    database_parameters,
    database_name,

    -- ** Datum
    datum_varCharValue,

    -- ** EncryptionConfiguration
    encryptionConfiguration_kmsKey,
    encryptionConfiguration_encryptionOption,

    -- ** EngineConfiguration
    engineConfiguration_additionalConfigs,
    engineConfiguration_coordinatorDpuSize,
    engineConfiguration_defaultExecutorDpuSize,
    engineConfiguration_maxConcurrentDpus,

    -- ** EngineVersion
    engineVersion_effectiveEngineVersion,
    engineVersion_selectedEngineVersion,

    -- ** ExecutorsSummary
    executorsSummary_executorSize,
    executorsSummary_executorState,
    executorsSummary_executorType,
    executorsSummary_startDateTime,
    executorsSummary_terminationDateTime,
    executorsSummary_executorId,

    -- ** FilterDefinition
    filterDefinition_name,

    -- ** NamedQuery
    namedQuery_description,
    namedQuery_namedQueryId,
    namedQuery_workGroup,
    namedQuery_name,
    namedQuery_database,
    namedQuery_queryString,

    -- ** NotebookMetadata
    notebookMetadata_creationTime,
    notebookMetadata_lastModifiedTime,
    notebookMetadata_name,
    notebookMetadata_notebookId,
    notebookMetadata_type,
    notebookMetadata_workGroup,

    -- ** NotebookSessionSummary
    notebookSessionSummary_creationTime,
    notebookSessionSummary_sessionId,

    -- ** PreparedStatement
    preparedStatement_description,
    preparedStatement_lastModifiedTime,
    preparedStatement_queryStatement,
    preparedStatement_statementName,
    preparedStatement_workGroupName,

    -- ** PreparedStatementSummary
    preparedStatementSummary_lastModifiedTime,
    preparedStatementSummary_statementName,

    -- ** QueryExecution
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

    -- ** QueryExecutionContext
    queryExecutionContext_catalog,
    queryExecutionContext_database,

    -- ** QueryExecutionStatistics
    queryExecutionStatistics_dataManifestLocation,
    queryExecutionStatistics_dataScannedInBytes,
    queryExecutionStatistics_engineExecutionTimeInMillis,
    queryExecutionStatistics_queryPlanningTimeInMillis,
    queryExecutionStatistics_queryQueueTimeInMillis,
    queryExecutionStatistics_resultReuseInformation,
    queryExecutionStatistics_serviceProcessingTimeInMillis,
    queryExecutionStatistics_totalExecutionTimeInMillis,

    -- ** QueryExecutionStatus
    queryExecutionStatus_athenaError,
    queryExecutionStatus_completionDateTime,
    queryExecutionStatus_state,
    queryExecutionStatus_stateChangeReason,
    queryExecutionStatus_submissionDateTime,

    -- ** QueryRuntimeStatistics
    queryRuntimeStatistics_outputStage,
    queryRuntimeStatistics_rows,
    queryRuntimeStatistics_timeline,

    -- ** QueryRuntimeStatisticsRows
    queryRuntimeStatisticsRows_inputBytes,
    queryRuntimeStatisticsRows_inputRows,
    queryRuntimeStatisticsRows_outputBytes,
    queryRuntimeStatisticsRows_outputRows,

    -- ** QueryRuntimeStatisticsTimeline
    queryRuntimeStatisticsTimeline_engineExecutionTimeInMillis,
    queryRuntimeStatisticsTimeline_queryPlanningTimeInMillis,
    queryRuntimeStatisticsTimeline_queryQueueTimeInMillis,
    queryRuntimeStatisticsTimeline_serviceProcessingTimeInMillis,
    queryRuntimeStatisticsTimeline_totalExecutionTimeInMillis,

    -- ** QueryStage
    queryStage_executionTime,
    queryStage_inputBytes,
    queryStage_inputRows,
    queryStage_outputBytes,
    queryStage_outputRows,
    queryStage_queryStagePlan,
    queryStage_stageId,
    queryStage_state,
    queryStage_subStages,

    -- ** QueryStagePlanNode
    queryStagePlanNode_children,
    queryStagePlanNode_identifier,
    queryStagePlanNode_name,
    queryStagePlanNode_remoteSources,

    -- ** ResultConfiguration
    resultConfiguration_aclConfiguration,
    resultConfiguration_encryptionConfiguration,
    resultConfiguration_expectedBucketOwner,
    resultConfiguration_outputLocation,

    -- ** ResultConfigurationUpdates
    resultConfigurationUpdates_aclConfiguration,
    resultConfigurationUpdates_encryptionConfiguration,
    resultConfigurationUpdates_expectedBucketOwner,
    resultConfigurationUpdates_outputLocation,
    resultConfigurationUpdates_removeAclConfiguration,
    resultConfigurationUpdates_removeEncryptionConfiguration,
    resultConfigurationUpdates_removeExpectedBucketOwner,
    resultConfigurationUpdates_removeOutputLocation,

    -- ** ResultReuseByAgeConfiguration
    resultReuseByAgeConfiguration_maxAgeInMinutes,
    resultReuseByAgeConfiguration_enabled,

    -- ** ResultReuseConfiguration
    resultReuseConfiguration_resultReuseByAgeConfiguration,

    -- ** ResultReuseInformation
    resultReuseInformation_reusedPreviousResult,

    -- ** ResultSet
    resultSet_resultSetMetadata,
    resultSet_rows,

    -- ** ResultSetMetadata
    resultSetMetadata_columnInfo,

    -- ** Row
    row_data,

    -- ** SessionConfiguration
    sessionConfiguration_encryptionConfiguration,
    sessionConfiguration_executionRole,
    sessionConfiguration_idleTimeoutSeconds,
    sessionConfiguration_workingDirectory,

    -- ** SessionStatistics
    sessionStatistics_dpuExecutionInMillis,

    -- ** SessionStatus
    sessionStatus_endDateTime,
    sessionStatus_idleSinceDateTime,
    sessionStatus_lastModifiedDateTime,
    sessionStatus_startDateTime,
    sessionStatus_state,
    sessionStatus_stateChangeReason,

    -- ** SessionSummary
    sessionSummary_description,
    sessionSummary_engineVersion,
    sessionSummary_notebookVersion,
    sessionSummary_sessionId,
    sessionSummary_status,

    -- ** TableMetadata
    tableMetadata_columns,
    tableMetadata_createTime,
    tableMetadata_lastAccessTime,
    tableMetadata_parameters,
    tableMetadata_partitionKeys,
    tableMetadata_tableType,
    tableMetadata_name,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** UnprocessedNamedQueryId
    unprocessedNamedQueryId_errorCode,
    unprocessedNamedQueryId_errorMessage,
    unprocessedNamedQueryId_namedQueryId,

    -- ** UnprocessedPreparedStatementName
    unprocessedPreparedStatementName_errorCode,
    unprocessedPreparedStatementName_errorMessage,
    unprocessedPreparedStatementName_statementName,

    -- ** UnprocessedQueryExecutionId
    unprocessedQueryExecutionId_errorCode,
    unprocessedQueryExecutionId_errorMessage,
    unprocessedQueryExecutionId_queryExecutionId,

    -- ** WorkGroup
    workGroup_configuration,
    workGroup_creationTime,
    workGroup_description,
    workGroup_state,
    workGroup_name,

    -- ** WorkGroupConfiguration
    workGroupConfiguration_additionalConfiguration,
    workGroupConfiguration_bytesScannedCutoffPerQuery,
    workGroupConfiguration_customerContentEncryptionConfiguration,
    workGroupConfiguration_enforceWorkGroupConfiguration,
    workGroupConfiguration_engineVersion,
    workGroupConfiguration_executionRole,
    workGroupConfiguration_publishCloudWatchMetricsEnabled,
    workGroupConfiguration_requesterPaysEnabled,
    workGroupConfiguration_resultConfiguration,

    -- ** WorkGroupConfigurationUpdates
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

    -- ** WorkGroupSummary
    workGroupSummary_creationTime,
    workGroupSummary_description,
    workGroupSummary_engineVersion,
    workGroupSummary_name,
    workGroupSummary_state,
  )
where

import Amazonka.Athena.BatchGetNamedQuery
import Amazonka.Athena.BatchGetPreparedStatement
import Amazonka.Athena.BatchGetQueryExecution
import Amazonka.Athena.CreateDataCatalog
import Amazonka.Athena.CreateNamedQuery
import Amazonka.Athena.CreateNotebook
import Amazonka.Athena.CreatePreparedStatement
import Amazonka.Athena.CreatePresignedNotebookUrl
import Amazonka.Athena.CreateWorkGroup
import Amazonka.Athena.DeleteDataCatalog
import Amazonka.Athena.DeleteNamedQuery
import Amazonka.Athena.DeleteNotebook
import Amazonka.Athena.DeletePreparedStatement
import Amazonka.Athena.DeleteWorkGroup
import Amazonka.Athena.ExportNotebook
import Amazonka.Athena.GetCalculationExecution
import Amazonka.Athena.GetCalculationExecutionCode
import Amazonka.Athena.GetCalculationExecutionStatus
import Amazonka.Athena.GetDataCatalog
import Amazonka.Athena.GetDatabase
import Amazonka.Athena.GetNamedQuery
import Amazonka.Athena.GetNotebookMetadata
import Amazonka.Athena.GetPreparedStatement
import Amazonka.Athena.GetQueryExecution
import Amazonka.Athena.GetQueryResults
import Amazonka.Athena.GetQueryRuntimeStatistics
import Amazonka.Athena.GetSession
import Amazonka.Athena.GetSessionStatus
import Amazonka.Athena.GetTableMetadata
import Amazonka.Athena.GetWorkGroup
import Amazonka.Athena.ImportNotebook
import Amazonka.Athena.ListApplicationDPUSizes
import Amazonka.Athena.ListCalculationExecutions
import Amazonka.Athena.ListDataCatalogs
import Amazonka.Athena.ListDatabases
import Amazonka.Athena.ListEngineVersions
import Amazonka.Athena.ListExecutors
import Amazonka.Athena.ListNamedQueries
import Amazonka.Athena.ListNotebookMetadata
import Amazonka.Athena.ListNotebookSessions
import Amazonka.Athena.ListPreparedStatements
import Amazonka.Athena.ListQueryExecutions
import Amazonka.Athena.ListSessions
import Amazonka.Athena.ListTableMetadata
import Amazonka.Athena.ListTagsForResource
import Amazonka.Athena.ListWorkGroups
import Amazonka.Athena.StartCalculationExecution
import Amazonka.Athena.StartQueryExecution
import Amazonka.Athena.StartSession
import Amazonka.Athena.StopCalculationExecution
import Amazonka.Athena.StopQueryExecution
import Amazonka.Athena.TagResource
import Amazonka.Athena.TerminateSession
import Amazonka.Athena.Types.AclConfiguration
import Amazonka.Athena.Types.ApplicationDPUSizes
import Amazonka.Athena.Types.AthenaError
import Amazonka.Athena.Types.CalculationConfiguration
import Amazonka.Athena.Types.CalculationResult
import Amazonka.Athena.Types.CalculationStatistics
import Amazonka.Athena.Types.CalculationStatus
import Amazonka.Athena.Types.CalculationSummary
import Amazonka.Athena.Types.Column
import Amazonka.Athena.Types.ColumnInfo
import Amazonka.Athena.Types.CustomerContentEncryptionConfiguration
import Amazonka.Athena.Types.DataCatalog
import Amazonka.Athena.Types.DataCatalogSummary
import Amazonka.Athena.Types.Database
import Amazonka.Athena.Types.Datum
import Amazonka.Athena.Types.EncryptionConfiguration
import Amazonka.Athena.Types.EngineConfiguration
import Amazonka.Athena.Types.EngineVersion
import Amazonka.Athena.Types.ExecutorsSummary
import Amazonka.Athena.Types.FilterDefinition
import Amazonka.Athena.Types.NamedQuery
import Amazonka.Athena.Types.NotebookMetadata
import Amazonka.Athena.Types.NotebookSessionSummary
import Amazonka.Athena.Types.PreparedStatement
import Amazonka.Athena.Types.PreparedStatementSummary
import Amazonka.Athena.Types.QueryExecution
import Amazonka.Athena.Types.QueryExecutionContext
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
import Amazonka.Athena.Types.SessionConfiguration
import Amazonka.Athena.Types.SessionStatistics
import Amazonka.Athena.Types.SessionStatus
import Amazonka.Athena.Types.SessionSummary
import Amazonka.Athena.Types.TableMetadata
import Amazonka.Athena.Types.Tag
import Amazonka.Athena.Types.UnprocessedNamedQueryId
import Amazonka.Athena.Types.UnprocessedPreparedStatementName
import Amazonka.Athena.Types.UnprocessedQueryExecutionId
import Amazonka.Athena.Types.WorkGroup
import Amazonka.Athena.Types.WorkGroupConfiguration
import Amazonka.Athena.Types.WorkGroupConfigurationUpdates
import Amazonka.Athena.Types.WorkGroupSummary
import Amazonka.Athena.UntagResource
import Amazonka.Athena.UpdateDataCatalog
import Amazonka.Athena.UpdateNamedQuery
import Amazonka.Athena.UpdateNotebook
import Amazonka.Athena.UpdateNotebookMetadata
import Amazonka.Athena.UpdatePreparedStatement
import Amazonka.Athena.UpdateWorkGroup

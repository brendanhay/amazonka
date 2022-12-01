{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Athena.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    batchGetQueryExecutionResponse_unprocessedQueryExecutionIds,
    batchGetQueryExecutionResponse_queryExecutions,
    batchGetQueryExecutionResponse_httpStatus,

    -- ** CreateDataCatalog
    createDataCatalog_tags,
    createDataCatalog_description,
    createDataCatalog_parameters,
    createDataCatalog_name,
    createDataCatalog_type,
    createDataCatalogResponse_httpStatus,

    -- ** CreateNamedQuery
    createNamedQuery_clientRequestToken,
    createNamedQuery_workGroup,
    createNamedQuery_description,
    createNamedQuery_name,
    createNamedQuery_database,
    createNamedQuery_queryString,
    createNamedQueryResponse_namedQueryId,
    createNamedQueryResponse_httpStatus,

    -- ** CreatePreparedStatement
    createPreparedStatement_description,
    createPreparedStatement_statementName,
    createPreparedStatement_workGroup,
    createPreparedStatement_queryStatement,
    createPreparedStatementResponse_httpStatus,

    -- ** CreateWorkGroup
    createWorkGroup_tags,
    createWorkGroup_configuration,
    createWorkGroup_description,
    createWorkGroup_name,
    createWorkGroupResponse_httpStatus,

    -- ** DeleteDataCatalog
    deleteDataCatalog_name,
    deleteDataCatalogResponse_httpStatus,

    -- ** DeleteNamedQuery
    deleteNamedQuery_namedQueryId,
    deleteNamedQueryResponse_httpStatus,

    -- ** DeletePreparedStatement
    deletePreparedStatement_statementName,
    deletePreparedStatement_workGroup,
    deletePreparedStatementResponse_httpStatus,

    -- ** DeleteWorkGroup
    deleteWorkGroup_recursiveDeleteOption,
    deleteWorkGroup_workGroup,
    deleteWorkGroupResponse_httpStatus,

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
    getQueryResults_nextToken,
    getQueryResults_maxResults,
    getQueryResults_queryExecutionId,
    getQueryResultsResponse_nextToken,
    getQueryResultsResponse_resultSet,
    getQueryResultsResponse_updateCount,
    getQueryResultsResponse_httpStatus,

    -- ** GetQueryRuntimeStatistics
    getQueryRuntimeStatistics_queryExecutionId,
    getQueryRuntimeStatisticsResponse_queryRuntimeStatistics,
    getQueryRuntimeStatisticsResponse_httpStatus,

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

    -- ** ListDataCatalogs
    listDataCatalogs_nextToken,
    listDataCatalogs_maxResults,
    listDataCatalogsResponse_nextToken,
    listDataCatalogsResponse_dataCatalogsSummary,
    listDataCatalogsResponse_httpStatus,

    -- ** ListDatabases
    listDatabases_nextToken,
    listDatabases_maxResults,
    listDatabases_catalogName,
    listDatabasesResponse_nextToken,
    listDatabasesResponse_databaseList,
    listDatabasesResponse_httpStatus,

    -- ** ListEngineVersions
    listEngineVersions_nextToken,
    listEngineVersions_maxResults,
    listEngineVersionsResponse_nextToken,
    listEngineVersionsResponse_engineVersions,
    listEngineVersionsResponse_httpStatus,

    -- ** ListNamedQueries
    listNamedQueries_nextToken,
    listNamedQueries_workGroup,
    listNamedQueries_maxResults,
    listNamedQueriesResponse_nextToken,
    listNamedQueriesResponse_namedQueryIds,
    listNamedQueriesResponse_httpStatus,

    -- ** ListPreparedStatements
    listPreparedStatements_nextToken,
    listPreparedStatements_maxResults,
    listPreparedStatements_workGroup,
    listPreparedStatementsResponse_nextToken,
    listPreparedStatementsResponse_preparedStatements,
    listPreparedStatementsResponse_httpStatus,

    -- ** ListQueryExecutions
    listQueryExecutions_nextToken,
    listQueryExecutions_workGroup,
    listQueryExecutions_maxResults,
    listQueryExecutionsResponse_nextToken,
    listQueryExecutionsResponse_queryExecutionIds,
    listQueryExecutionsResponse_httpStatus,

    -- ** ListTableMetadata
    listTableMetadata_nextToken,
    listTableMetadata_expression,
    listTableMetadata_maxResults,
    listTableMetadata_catalogName,
    listTableMetadata_databaseName,
    listTableMetadataResponse_nextToken,
    listTableMetadataResponse_tableMetadataList,
    listTableMetadataResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_httpStatus,

    -- ** ListWorkGroups
    listWorkGroups_nextToken,
    listWorkGroups_maxResults,
    listWorkGroupsResponse_nextToken,
    listWorkGroupsResponse_workGroups,
    listWorkGroupsResponse_httpStatus,

    -- ** StartQueryExecution
    startQueryExecution_resultReuseConfiguration,
    startQueryExecution_clientRequestToken,
    startQueryExecution_workGroup,
    startQueryExecution_resultConfiguration,
    startQueryExecution_queryExecutionContext,
    startQueryExecution_executionParameters,
    startQueryExecution_queryString,
    startQueryExecutionResponse_queryExecutionId,
    startQueryExecutionResponse_httpStatus,

    -- ** StopQueryExecution
    stopQueryExecution_queryExecutionId,
    stopQueryExecutionResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

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

    -- ** UpdatePreparedStatement
    updatePreparedStatement_description,
    updatePreparedStatement_statementName,
    updatePreparedStatement_workGroup,
    updatePreparedStatement_queryStatement,
    updatePreparedStatementResponse_httpStatus,

    -- ** UpdateWorkGroup
    updateWorkGroup_configurationUpdates,
    updateWorkGroup_state,
    updateWorkGroup_description,
    updateWorkGroup_workGroup,
    updateWorkGroupResponse_httpStatus,

    -- * Types

    -- ** AclConfiguration
    aclConfiguration_s3AclOption,

    -- ** AthenaError
    athenaError_retryable,
    athenaError_errorCategory,
    athenaError_errorMessage,
    athenaError_errorType,

    -- ** Column
    column_type,
    column_comment,
    column_name,

    -- ** ColumnInfo
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

    -- ** DataCatalog
    dataCatalog_description,
    dataCatalog_parameters,
    dataCatalog_name,
    dataCatalog_type,

    -- ** DataCatalogSummary
    dataCatalogSummary_type,
    dataCatalogSummary_catalogName,

    -- ** Database
    database_description,
    database_parameters,
    database_name,

    -- ** Datum
    datum_varCharValue,

    -- ** EncryptionConfiguration
    encryptionConfiguration_kmsKey,
    encryptionConfiguration_encryptionOption,

    -- ** EngineVersion
    engineVersion_selectedEngineVersion,
    engineVersion_effectiveEngineVersion,

    -- ** NamedQuery
    namedQuery_workGroup,
    namedQuery_description,
    namedQuery_namedQueryId,
    namedQuery_name,
    namedQuery_database,
    namedQuery_queryString,

    -- ** PreparedStatement
    preparedStatement_workGroupName,
    preparedStatement_description,
    preparedStatement_lastModifiedTime,
    preparedStatement_queryStatement,
    preparedStatement_statementName,

    -- ** PreparedStatementSummary
    preparedStatementSummary_lastModifiedTime,
    preparedStatementSummary_statementName,

    -- ** QueryExecution
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

    -- ** QueryExecutionContext
    queryExecutionContext_catalog,
    queryExecutionContext_database,

    -- ** QueryExecutionStatistics
    queryExecutionStatistics_dataScannedInBytes,
    queryExecutionStatistics_queryQueueTimeInMillis,
    queryExecutionStatistics_resultReuseInformation,
    queryExecutionStatistics_serviceProcessingTimeInMillis,
    queryExecutionStatistics_dataManifestLocation,
    queryExecutionStatistics_totalExecutionTimeInMillis,
    queryExecutionStatistics_engineExecutionTimeInMillis,
    queryExecutionStatistics_queryPlanningTimeInMillis,

    -- ** QueryExecutionStatus
    queryExecutionStatus_stateChangeReason,
    queryExecutionStatus_submissionDateTime,
    queryExecutionStatus_state,
    queryExecutionStatus_athenaError,
    queryExecutionStatus_completionDateTime,

    -- ** QueryRuntimeStatistics
    queryRuntimeStatistics_rows,
    queryRuntimeStatistics_timeline,
    queryRuntimeStatistics_outputStage,

    -- ** QueryRuntimeStatisticsRows
    queryRuntimeStatisticsRows_inputBytes,
    queryRuntimeStatisticsRows_outputBytes,
    queryRuntimeStatisticsRows_inputRows,
    queryRuntimeStatisticsRows_outputRows,

    -- ** QueryRuntimeStatisticsTimeline
    queryRuntimeStatisticsTimeline_queryQueueTimeInMillis,
    queryRuntimeStatisticsTimeline_serviceProcessingTimeInMillis,
    queryRuntimeStatisticsTimeline_totalExecutionTimeInMillis,
    queryRuntimeStatisticsTimeline_engineExecutionTimeInMillis,
    queryRuntimeStatisticsTimeline_queryPlanningTimeInMillis,

    -- ** QueryStage
    queryStage_inputBytes,
    queryStage_outputBytes,
    queryStage_inputRows,
    queryStage_queryStagePlan,
    queryStage_state,
    queryStage_executionTime,
    queryStage_subStages,
    queryStage_stageId,
    queryStage_outputRows,

    -- ** QueryStagePlanNode
    queryStagePlanNode_name,
    queryStagePlanNode_remoteSources,
    queryStagePlanNode_children,
    queryStagePlanNode_identifier,

    -- ** ResultConfiguration
    resultConfiguration_aclConfiguration,
    resultConfiguration_expectedBucketOwner,
    resultConfiguration_outputLocation,
    resultConfiguration_encryptionConfiguration,

    -- ** ResultConfigurationUpdates
    resultConfigurationUpdates_aclConfiguration,
    resultConfigurationUpdates_removeEncryptionConfiguration,
    resultConfigurationUpdates_expectedBucketOwner,
    resultConfigurationUpdates_removeAclConfiguration,
    resultConfigurationUpdates_outputLocation,
    resultConfigurationUpdates_removeExpectedBucketOwner,
    resultConfigurationUpdates_removeOutputLocation,
    resultConfigurationUpdates_encryptionConfiguration,

    -- ** ResultReuseByAgeConfiguration
    resultReuseByAgeConfiguration_maxAgeInMinutes,
    resultReuseByAgeConfiguration_enabled,

    -- ** ResultReuseConfiguration
    resultReuseConfiguration_resultReuseByAgeConfiguration,

    -- ** ResultReuseInformation
    resultReuseInformation_reusedPreviousResult,

    -- ** ResultSet
    resultSet_rows,
    resultSet_resultSetMetadata,

    -- ** ResultSetMetadata
    resultSetMetadata_columnInfo,

    -- ** Row
    row_data,

    -- ** TableMetadata
    tableMetadata_columns,
    tableMetadata_lastAccessTime,
    tableMetadata_partitionKeys,
    tableMetadata_tableType,
    tableMetadata_createTime,
    tableMetadata_parameters,
    tableMetadata_name,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** UnprocessedNamedQueryId
    unprocessedNamedQueryId_errorMessage,
    unprocessedNamedQueryId_errorCode,
    unprocessedNamedQueryId_namedQueryId,

    -- ** UnprocessedPreparedStatementName
    unprocessedPreparedStatementName_errorMessage,
    unprocessedPreparedStatementName_errorCode,
    unprocessedPreparedStatementName_statementName,

    -- ** UnprocessedQueryExecutionId
    unprocessedQueryExecutionId_queryExecutionId,
    unprocessedQueryExecutionId_errorMessage,
    unprocessedQueryExecutionId_errorCode,

    -- ** WorkGroup
    workGroup_configuration,
    workGroup_state,
    workGroup_description,
    workGroup_creationTime,
    workGroup_name,

    -- ** WorkGroupConfiguration
    workGroupConfiguration_publishCloudWatchMetricsEnabled,
    workGroupConfiguration_enforceWorkGroupConfiguration,
    workGroupConfiguration_resultConfiguration,
    workGroupConfiguration_bytesScannedCutoffPerQuery,
    workGroupConfiguration_requesterPaysEnabled,
    workGroupConfiguration_engineVersion,

    -- ** WorkGroupConfigurationUpdates
    workGroupConfigurationUpdates_resultConfigurationUpdates,
    workGroupConfigurationUpdates_publishCloudWatchMetricsEnabled,
    workGroupConfigurationUpdates_enforceWorkGroupConfiguration,
    workGroupConfigurationUpdates_removeBytesScannedCutoffPerQuery,
    workGroupConfigurationUpdates_bytesScannedCutoffPerQuery,
    workGroupConfigurationUpdates_requesterPaysEnabled,
    workGroupConfigurationUpdates_engineVersion,

    -- ** WorkGroupSummary
    workGroupSummary_name,
    workGroupSummary_state,
    workGroupSummary_description,
    workGroupSummary_creationTime,
    workGroupSummary_engineVersion,
  )
where

import Amazonka.Athena.BatchGetNamedQuery
import Amazonka.Athena.BatchGetPreparedStatement
import Amazonka.Athena.BatchGetQueryExecution
import Amazonka.Athena.CreateDataCatalog
import Amazonka.Athena.CreateNamedQuery
import Amazonka.Athena.CreatePreparedStatement
import Amazonka.Athena.CreateWorkGroup
import Amazonka.Athena.DeleteDataCatalog
import Amazonka.Athena.DeleteNamedQuery
import Amazonka.Athena.DeletePreparedStatement
import Amazonka.Athena.DeleteWorkGroup
import Amazonka.Athena.GetDataCatalog
import Amazonka.Athena.GetDatabase
import Amazonka.Athena.GetNamedQuery
import Amazonka.Athena.GetPreparedStatement
import Amazonka.Athena.GetQueryExecution
import Amazonka.Athena.GetQueryResults
import Amazonka.Athena.GetQueryRuntimeStatistics
import Amazonka.Athena.GetTableMetadata
import Amazonka.Athena.GetWorkGroup
import Amazonka.Athena.ListDataCatalogs
import Amazonka.Athena.ListDatabases
import Amazonka.Athena.ListEngineVersions
import Amazonka.Athena.ListNamedQueries
import Amazonka.Athena.ListPreparedStatements
import Amazonka.Athena.ListQueryExecutions
import Amazonka.Athena.ListTableMetadata
import Amazonka.Athena.ListTagsForResource
import Amazonka.Athena.ListWorkGroups
import Amazonka.Athena.StartQueryExecution
import Amazonka.Athena.StopQueryExecution
import Amazonka.Athena.TagResource
import Amazonka.Athena.Types.AclConfiguration
import Amazonka.Athena.Types.AthenaError
import Amazonka.Athena.Types.Column
import Amazonka.Athena.Types.ColumnInfo
import Amazonka.Athena.Types.DataCatalog
import Amazonka.Athena.Types.DataCatalogSummary
import Amazonka.Athena.Types.Database
import Amazonka.Athena.Types.Datum
import Amazonka.Athena.Types.EncryptionConfiguration
import Amazonka.Athena.Types.EngineVersion
import Amazonka.Athena.Types.NamedQuery
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
import Amazonka.Athena.UpdatePreparedStatement
import Amazonka.Athena.UpdateWorkGroup

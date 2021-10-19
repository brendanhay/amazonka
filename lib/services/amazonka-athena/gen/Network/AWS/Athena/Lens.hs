{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Lens
  ( -- * Operations

    -- ** ListDatabases
    listDatabases_nextToken,
    listDatabases_maxResults,
    listDatabases_catalogName,
    listDatabasesResponse_databaseList,
    listDatabasesResponse_nextToken,
    listDatabasesResponse_httpStatus,

    -- ** CreatePreparedStatement
    createPreparedStatement_description,
    createPreparedStatement_statementName,
    createPreparedStatement_workGroup,
    createPreparedStatement_queryStatement,
    createPreparedStatementResponse_httpStatus,

    -- ** DeleteWorkGroup
    deleteWorkGroup_recursiveDeleteOption,
    deleteWorkGroup_workGroup,
    deleteWorkGroupResponse_httpStatus,

    -- ** UpdateWorkGroup
    updateWorkGroup_state,
    updateWorkGroup_configurationUpdates,
    updateWorkGroup_description,
    updateWorkGroup_workGroup,
    updateWorkGroupResponse_httpStatus,

    -- ** GetNamedQuery
    getNamedQuery_namedQueryId,
    getNamedQueryResponse_namedQuery,
    getNamedQueryResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DeleteDataCatalog
    deleteDataCatalog_name,
    deleteDataCatalogResponse_httpStatus,

    -- ** UpdateDataCatalog
    updateDataCatalog_parameters,
    updateDataCatalog_description,
    updateDataCatalog_name,
    updateDataCatalog_type,
    updateDataCatalogResponse_httpStatus,

    -- ** ListDataCatalogs
    listDataCatalogs_nextToken,
    listDataCatalogs_maxResults,
    listDataCatalogsResponse_dataCatalogsSummary,
    listDataCatalogsResponse_nextToken,
    listDataCatalogsResponse_httpStatus,

    -- ** CreateNamedQuery
    createNamedQuery_clientRequestToken,
    createNamedQuery_description,
    createNamedQuery_workGroup,
    createNamedQuery_name,
    createNamedQuery_database,
    createNamedQuery_queryString,
    createNamedQueryResponse_namedQueryId,
    createNamedQueryResponse_httpStatus,

    -- ** GetTableMetadata
    getTableMetadata_catalogName,
    getTableMetadata_databaseName,
    getTableMetadata_tableName,
    getTableMetadataResponse_tableMetadata,
    getTableMetadataResponse_httpStatus,

    -- ** ListNamedQueries
    listNamedQueries_nextToken,
    listNamedQueries_workGroup,
    listNamedQueries_maxResults,
    listNamedQueriesResponse_nextToken,
    listNamedQueriesResponse_namedQueryIds,
    listNamedQueriesResponse_httpStatus,

    -- ** DeleteNamedQuery
    deleteNamedQuery_namedQueryId,
    deleteNamedQueryResponse_httpStatus,

    -- ** StartQueryExecution
    startQueryExecution_queryExecutionContext,
    startQueryExecution_resultConfiguration,
    startQueryExecution_clientRequestToken,
    startQueryExecution_workGroup,
    startQueryExecution_queryString,
    startQueryExecutionResponse_queryExecutionId,
    startQueryExecutionResponse_httpStatus,

    -- ** BatchGetNamedQuery
    batchGetNamedQuery_namedQueryIds,
    batchGetNamedQueryResponse_namedQueries,
    batchGetNamedQueryResponse_unprocessedNamedQueryIds,
    batchGetNamedQueryResponse_httpStatus,

    -- ** GetQueryExecution
    getQueryExecution_queryExecutionId,
    getQueryExecutionResponse_queryExecution,
    getQueryExecutionResponse_httpStatus,

    -- ** ListPreparedStatements
    listPreparedStatements_nextToken,
    listPreparedStatements_maxResults,
    listPreparedStatements_workGroup,
    listPreparedStatementsResponse_preparedStatements,
    listPreparedStatementsResponse_nextToken,
    listPreparedStatementsResponse_httpStatus,

    -- ** CreateDataCatalog
    createDataCatalog_parameters,
    createDataCatalog_description,
    createDataCatalog_tags,
    createDataCatalog_name,
    createDataCatalog_type,
    createDataCatalogResponse_httpStatus,

    -- ** ListWorkGroups
    listWorkGroups_nextToken,
    listWorkGroups_maxResults,
    listWorkGroupsResponse_nextToken,
    listWorkGroupsResponse_workGroups,
    listWorkGroupsResponse_httpStatus,

    -- ** CreateWorkGroup
    createWorkGroup_configuration,
    createWorkGroup_description,
    createWorkGroup_tags,
    createWorkGroup_name,
    createWorkGroupResponse_httpStatus,

    -- ** BatchGetQueryExecution
    batchGetQueryExecution_queryExecutionIds,
    batchGetQueryExecutionResponse_unprocessedQueryExecutionIds,
    batchGetQueryExecutionResponse_queryExecutions,
    batchGetQueryExecutionResponse_httpStatus,

    -- ** ListEngineVersions
    listEngineVersions_nextToken,
    listEngineVersions_maxResults,
    listEngineVersionsResponse_nextToken,
    listEngineVersionsResponse_engineVersions,
    listEngineVersionsResponse_httpStatus,

    -- ** GetDataCatalog
    getDataCatalog_name,
    getDataCatalogResponse_dataCatalog,
    getDataCatalogResponse_httpStatus,

    -- ** StopQueryExecution
    stopQueryExecution_queryExecutionId,
    stopQueryExecutionResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** GetWorkGroup
    getWorkGroup_workGroup,
    getWorkGroupResponse_workGroup,
    getWorkGroupResponse_httpStatus,

    -- ** GetDatabase
    getDatabase_catalogName,
    getDatabase_databaseName,
    getDatabaseResponse_database,
    getDatabaseResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** GetPreparedStatement
    getPreparedStatement_statementName,
    getPreparedStatement_workGroup,
    getPreparedStatementResponse_preparedStatement,
    getPreparedStatementResponse_httpStatus,

    -- ** GetQueryResults
    getQueryResults_nextToken,
    getQueryResults_maxResults,
    getQueryResults_queryExecutionId,
    getQueryResultsResponse_updateCount,
    getQueryResultsResponse_nextToken,
    getQueryResultsResponse_resultSet,
    getQueryResultsResponse_httpStatus,

    -- ** ListTableMetadata
    listTableMetadata_nextToken,
    listTableMetadata_expression,
    listTableMetadata_maxResults,
    listTableMetadata_catalogName,
    listTableMetadata_databaseName,
    listTableMetadataResponse_nextToken,
    listTableMetadataResponse_tableMetadataList,
    listTableMetadataResponse_httpStatus,

    -- ** ListQueryExecutions
    listQueryExecutions_nextToken,
    listQueryExecutions_workGroup,
    listQueryExecutions_maxResults,
    listQueryExecutionsResponse_queryExecutionIds,
    listQueryExecutionsResponse_nextToken,
    listQueryExecutionsResponse_httpStatus,

    -- ** DeletePreparedStatement
    deletePreparedStatement_statementName,
    deletePreparedStatement_workGroup,
    deletePreparedStatementResponse_httpStatus,

    -- ** UpdatePreparedStatement
    updatePreparedStatement_description,
    updatePreparedStatement_statementName,
    updatePreparedStatement_workGroup,
    updatePreparedStatement_queryStatement,
    updatePreparedStatementResponse_httpStatus,

    -- * Types

    -- ** Column
    column_type,
    column_comment,
    column_name,

    -- ** ColumnInfo
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

    -- ** DataCatalog
    dataCatalog_parameters,
    dataCatalog_description,
    dataCatalog_name,
    dataCatalog_type,

    -- ** DataCatalogSummary
    dataCatalogSummary_catalogName,
    dataCatalogSummary_type,

    -- ** Database
    database_parameters,
    database_description,
    database_name,

    -- ** Datum
    datum_varCharValue,

    -- ** EncryptionConfiguration
    encryptionConfiguration_kmsKey,
    encryptionConfiguration_encryptionOption,

    -- ** EngineVersion
    engineVersion_effectiveEngineVersion,
    engineVersion_selectedEngineVersion,

    -- ** NamedQuery
    namedQuery_namedQueryId,
    namedQuery_description,
    namedQuery_workGroup,
    namedQuery_name,
    namedQuery_database,
    namedQuery_queryString,

    -- ** PreparedStatement
    preparedStatement_lastModifiedTime,
    preparedStatement_queryStatement,
    preparedStatement_statementName,
    preparedStatement_description,
    preparedStatement_workGroupName,

    -- ** PreparedStatementSummary
    preparedStatementSummary_lastModifiedTime,
    preparedStatementSummary_statementName,

    -- ** QueryExecution
    queryExecution_engineVersion,
    queryExecution_status,
    queryExecution_queryExecutionContext,
    queryExecution_resultConfiguration,
    queryExecution_query,
    queryExecution_statementType,
    queryExecution_statistics,
    queryExecution_queryExecutionId,
    queryExecution_workGroup,

    -- ** QueryExecutionContext
    queryExecutionContext_database,
    queryExecutionContext_catalog,

    -- ** QueryExecutionStatistics
    queryExecutionStatistics_totalExecutionTimeInMillis,
    queryExecutionStatistics_engineExecutionTimeInMillis,
    queryExecutionStatistics_queryPlanningTimeInMillis,
    queryExecutionStatistics_dataScannedInBytes,
    queryExecutionStatistics_queryQueueTimeInMillis,
    queryExecutionStatistics_dataManifestLocation,
    queryExecutionStatistics_serviceProcessingTimeInMillis,

    -- ** QueryExecutionStatus
    queryExecutionStatus_state,
    queryExecutionStatus_stateChangeReason,
    queryExecutionStatus_submissionDateTime,
    queryExecutionStatus_completionDateTime,

    -- ** ResultConfiguration
    resultConfiguration_encryptionConfiguration,
    resultConfiguration_outputLocation,

    -- ** ResultConfigurationUpdates
    resultConfigurationUpdates_removeOutputLocation,
    resultConfigurationUpdates_removeEncryptionConfiguration,
    resultConfigurationUpdates_encryptionConfiguration,
    resultConfigurationUpdates_outputLocation,

    -- ** ResultSet
    resultSet_rows,
    resultSet_resultSetMetadata,

    -- ** ResultSetMetadata
    resultSetMetadata_columnInfo,

    -- ** Row
    row_data,

    -- ** TableMetadata
    tableMetadata_tableType,
    tableMetadata_parameters,
    tableMetadata_columns,
    tableMetadata_lastAccessTime,
    tableMetadata_partitionKeys,
    tableMetadata_createTime,
    tableMetadata_name,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** UnprocessedNamedQueryId
    unprocessedNamedQueryId_namedQueryId,
    unprocessedNamedQueryId_errorCode,
    unprocessedNamedQueryId_errorMessage,

    -- ** UnprocessedQueryExecutionId
    unprocessedQueryExecutionId_errorCode,
    unprocessedQueryExecutionId_queryExecutionId,
    unprocessedQueryExecutionId_errorMessage,

    -- ** WorkGroup
    workGroup_creationTime,
    workGroup_state,
    workGroup_configuration,
    workGroup_description,
    workGroup_name,

    -- ** WorkGroupConfiguration
    workGroupConfiguration_engineVersion,
    workGroupConfiguration_requesterPaysEnabled,
    workGroupConfiguration_resultConfiguration,
    workGroupConfiguration_bytesScannedCutoffPerQuery,
    workGroupConfiguration_enforceWorkGroupConfiguration,
    workGroupConfiguration_publishCloudWatchMetricsEnabled,

    -- ** WorkGroupConfigurationUpdates
    workGroupConfigurationUpdates_engineVersion,
    workGroupConfigurationUpdates_requesterPaysEnabled,
    workGroupConfigurationUpdates_resultConfigurationUpdates,
    workGroupConfigurationUpdates_bytesScannedCutoffPerQuery,
    workGroupConfigurationUpdates_removeBytesScannedCutoffPerQuery,
    workGroupConfigurationUpdates_enforceWorkGroupConfiguration,
    workGroupConfigurationUpdates_publishCloudWatchMetricsEnabled,

    -- ** WorkGroupSummary
    workGroupSummary_creationTime,
    workGroupSummary_engineVersion,
    workGroupSummary_state,
    workGroupSummary_name,
    workGroupSummary_description,
  )
where

import Network.AWS.Athena.BatchGetNamedQuery
import Network.AWS.Athena.BatchGetQueryExecution
import Network.AWS.Athena.CreateDataCatalog
import Network.AWS.Athena.CreateNamedQuery
import Network.AWS.Athena.CreatePreparedStatement
import Network.AWS.Athena.CreateWorkGroup
import Network.AWS.Athena.DeleteDataCatalog
import Network.AWS.Athena.DeleteNamedQuery
import Network.AWS.Athena.DeletePreparedStatement
import Network.AWS.Athena.DeleteWorkGroup
import Network.AWS.Athena.GetDataCatalog
import Network.AWS.Athena.GetDatabase
import Network.AWS.Athena.GetNamedQuery
import Network.AWS.Athena.GetPreparedStatement
import Network.AWS.Athena.GetQueryExecution
import Network.AWS.Athena.GetQueryResults
import Network.AWS.Athena.GetTableMetadata
import Network.AWS.Athena.GetWorkGroup
import Network.AWS.Athena.ListDataCatalogs
import Network.AWS.Athena.ListDatabases
import Network.AWS.Athena.ListEngineVersions
import Network.AWS.Athena.ListNamedQueries
import Network.AWS.Athena.ListPreparedStatements
import Network.AWS.Athena.ListQueryExecutions
import Network.AWS.Athena.ListTableMetadata
import Network.AWS.Athena.ListTagsForResource
import Network.AWS.Athena.ListWorkGroups
import Network.AWS.Athena.StartQueryExecution
import Network.AWS.Athena.StopQueryExecution
import Network.AWS.Athena.TagResource
import Network.AWS.Athena.Types.Column
import Network.AWS.Athena.Types.ColumnInfo
import Network.AWS.Athena.Types.DataCatalog
import Network.AWS.Athena.Types.DataCatalogSummary
import Network.AWS.Athena.Types.Database
import Network.AWS.Athena.Types.Datum
import Network.AWS.Athena.Types.EncryptionConfiguration
import Network.AWS.Athena.Types.EngineVersion
import Network.AWS.Athena.Types.NamedQuery
import Network.AWS.Athena.Types.PreparedStatement
import Network.AWS.Athena.Types.PreparedStatementSummary
import Network.AWS.Athena.Types.QueryExecution
import Network.AWS.Athena.Types.QueryExecutionContext
import Network.AWS.Athena.Types.QueryExecutionStatistics
import Network.AWS.Athena.Types.QueryExecutionStatus
import Network.AWS.Athena.Types.ResultConfiguration
import Network.AWS.Athena.Types.ResultConfigurationUpdates
import Network.AWS.Athena.Types.ResultSet
import Network.AWS.Athena.Types.ResultSetMetadata
import Network.AWS.Athena.Types.Row
import Network.AWS.Athena.Types.TableMetadata
import Network.AWS.Athena.Types.Tag
import Network.AWS.Athena.Types.UnprocessedNamedQueryId
import Network.AWS.Athena.Types.UnprocessedQueryExecutionId
import Network.AWS.Athena.Types.WorkGroup
import Network.AWS.Athena.Types.WorkGroupConfiguration
import Network.AWS.Athena.Types.WorkGroupConfigurationUpdates
import Network.AWS.Athena.Types.WorkGroupSummary
import Network.AWS.Athena.UntagResource
import Network.AWS.Athena.UpdateDataCatalog
import Network.AWS.Athena.UpdatePreparedStatement
import Network.AWS.Athena.UpdateWorkGroup

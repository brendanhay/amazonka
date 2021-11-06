{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Athena.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Lens
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

import Amazonka.Athena.BatchGetNamedQuery
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
import Amazonka.Athena.Types.ResultConfiguration
import Amazonka.Athena.Types.ResultConfigurationUpdates
import Amazonka.Athena.Types.ResultSet
import Amazonka.Athena.Types.ResultSetMetadata
import Amazonka.Athena.Types.Row
import Amazonka.Athena.Types.TableMetadata
import Amazonka.Athena.Types.Tag
import Amazonka.Athena.Types.UnprocessedNamedQueryId
import Amazonka.Athena.Types.UnprocessedQueryExecutionId
import Amazonka.Athena.Types.WorkGroup
import Amazonka.Athena.Types.WorkGroupConfiguration
import Amazonka.Athena.Types.WorkGroupConfigurationUpdates
import Amazonka.Athena.Types.WorkGroupSummary
import Amazonka.Athena.UntagResource
import Amazonka.Athena.UpdateDataCatalog
import Amazonka.Athena.UpdatePreparedStatement
import Amazonka.Athena.UpdateWorkGroup

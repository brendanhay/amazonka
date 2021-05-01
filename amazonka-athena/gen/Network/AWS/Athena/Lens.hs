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

    -- ** CreateDataCatalog
    createDataCatalog_tags,
    createDataCatalog_description,
    createDataCatalog_parameters,
    createDataCatalog_name,
    createDataCatalog_type,
    createDataCatalogResponse_httpStatus,

    -- ** ListQueryExecutions
    listQueryExecutions_nextToken,
    listQueryExecutions_maxResults,
    listQueryExecutions_workGroup,
    listQueryExecutionsResponse_nextToken,
    listQueryExecutionsResponse_queryExecutionIds,
    listQueryExecutionsResponse_httpStatus,

    -- ** ListTableMetadata
    listTableMetadata_nextToken,
    listTableMetadata_maxResults,
    listTableMetadata_expression,
    listTableMetadata_catalogName,
    listTableMetadata_databaseName,
    listTableMetadataResponse_nextToken,
    listTableMetadataResponse_tableMetadataList,
    listTableMetadataResponse_httpStatus,

    -- ** GetQueryExecution
    getQueryExecution_queryExecutionId,
    getQueryExecutionResponse_queryExecution,
    getQueryExecutionResponse_httpStatus,

    -- ** BatchGetNamedQuery
    batchGetNamedQuery_namedQueryIds,
    batchGetNamedQueryResponse_namedQueries,
    batchGetNamedQueryResponse_unprocessedNamedQueryIds,
    batchGetNamedQueryResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** GetDatabase
    getDatabase_catalogName,
    getDatabase_databaseName,
    getDatabaseResponse_database,
    getDatabaseResponse_httpStatus,

    -- ** DeleteNamedQuery
    deleteNamedQuery_namedQueryId,
    deleteNamedQueryResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

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

    -- ** ListDataCatalogs
    listDataCatalogs_nextToken,
    listDataCatalogs_maxResults,
    listDataCatalogsResponse_nextToken,
    listDataCatalogsResponse_dataCatalogsSummary,
    listDataCatalogsResponse_httpStatus,

    -- ** CreateWorkGroup
    createWorkGroup_configuration,
    createWorkGroup_tags,
    createWorkGroup_description,
    createWorkGroup_name,
    createWorkGroupResponse_httpStatus,

    -- ** GetNamedQuery
    getNamedQuery_namedQueryId,
    getNamedQueryResponse_namedQuery,
    getNamedQueryResponse_httpStatus,

    -- ** UpdateWorkGroup
    updateWorkGroup_configurationUpdates,
    updateWorkGroup_state,
    updateWorkGroup_description,
    updateWorkGroup_workGroup,
    updateWorkGroupResponse_httpStatus,

    -- ** DeleteWorkGroup
    deleteWorkGroup_recursiveDeleteOption,
    deleteWorkGroup_workGroup,
    deleteWorkGroupResponse_httpStatus,

    -- ** ListWorkGroups
    listWorkGroups_nextToken,
    listWorkGroups_maxResults,
    listWorkGroupsResponse_nextToken,
    listWorkGroupsResponse_workGroups,
    listWorkGroupsResponse_httpStatus,

    -- ** ListDatabases
    listDatabases_nextToken,
    listDatabases_maxResults,
    listDatabases_catalogName,
    listDatabasesResponse_databaseList,
    listDatabasesResponse_nextToken,
    listDatabasesResponse_httpStatus,

    -- ** GetQueryResults
    getQueryResults_nextToken,
    getQueryResults_maxResults,
    getQueryResults_queryExecutionId,
    getQueryResultsResponse_nextToken,
    getQueryResultsResponse_resultSet,
    getQueryResultsResponse_updateCount,
    getQueryResultsResponse_httpStatus,

    -- ** GetWorkGroup
    getWorkGroup_workGroup,
    getWorkGroupResponse_workGroup,
    getWorkGroupResponse_httpStatus,

    -- ** StartQueryExecution
    startQueryExecution_queryExecutionContext,
    startQueryExecution_resultConfiguration,
    startQueryExecution_workGroup,
    startQueryExecution_clientRequestToken,
    startQueryExecution_queryString,
    startQueryExecutionResponse_queryExecutionId,
    startQueryExecutionResponse_httpStatus,

    -- ** StopQueryExecution
    stopQueryExecution_queryExecutionId,
    stopQueryExecutionResponse_httpStatus,

    -- ** GetTableMetadata
    getTableMetadata_catalogName,
    getTableMetadata_databaseName,
    getTableMetadata_tableName,
    getTableMetadataResponse_tableMetadata,
    getTableMetadataResponse_httpStatus,

    -- ** CreateNamedQuery
    createNamedQuery_workGroup,
    createNamedQuery_description,
    createNamedQuery_clientRequestToken,
    createNamedQuery_name,
    createNamedQuery_database,
    createNamedQuery_queryString,
    createNamedQueryResponse_namedQueryId,
    createNamedQueryResponse_httpStatus,

    -- ** ListNamedQueries
    listNamedQueries_nextToken,
    listNamedQueries_maxResults,
    listNamedQueries_workGroup,
    listNamedQueriesResponse_namedQueryIds,
    listNamedQueriesResponse_nextToken,
    listNamedQueriesResponse_httpStatus,

    -- ** BatchGetQueryExecution
    batchGetQueryExecution_queryExecutionIds,
    batchGetQueryExecutionResponse_queryExecutions,
    batchGetQueryExecutionResponse_unprocessedQueryExecutionIds,
    batchGetQueryExecutionResponse_httpStatus,

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
    updateDataCatalog_description,
    updateDataCatalog_parameters,
    updateDataCatalog_name,
    updateDataCatalog_type,
    updateDataCatalogResponse_httpStatus,

    -- * Types

    -- ** Column
    column_comment,
    column_type,
    column_name,

    -- ** ColumnInfo
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

    -- ** EngineVersion
    engineVersion_effectiveEngineVersion,
    engineVersion_selectedEngineVersion,

    -- ** NamedQuery
    namedQuery_namedQueryId,
    namedQuery_workGroup,
    namedQuery_description,
    namedQuery_name,
    namedQuery_database,
    namedQuery_queryString,

    -- ** QueryExecution
    queryExecution_status,
    queryExecution_queryExecutionId,
    queryExecution_statistics,
    queryExecution_query,
    queryExecution_queryExecutionContext,
    queryExecution_engineVersion,
    queryExecution_resultConfiguration,
    queryExecution_workGroup,
    queryExecution_statementType,

    -- ** QueryExecutionContext
    queryExecutionContext_catalog,
    queryExecutionContext_database,

    -- ** QueryExecutionStatistics
    queryExecutionStatistics_totalExecutionTimeInMillis,
    queryExecutionStatistics_serviceProcessingTimeInMillis,
    queryExecutionStatistics_queryQueueTimeInMillis,
    queryExecutionStatistics_dataScannedInBytes,
    queryExecutionStatistics_queryPlanningTimeInMillis,
    queryExecutionStatistics_engineExecutionTimeInMillis,
    queryExecutionStatistics_dataManifestLocation,

    -- ** QueryExecutionStatus
    queryExecutionStatus_submissionDateTime,
    queryExecutionStatus_stateChangeReason,
    queryExecutionStatus_completionDateTime,
    queryExecutionStatus_state,

    -- ** ResultConfiguration
    resultConfiguration_encryptionConfiguration,
    resultConfiguration_outputLocation,

    -- ** ResultConfigurationUpdates
    resultConfigurationUpdates_encryptionConfiguration,
    resultConfigurationUpdates_removeOutputLocation,
    resultConfigurationUpdates_removeEncryptionConfiguration,
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
    tableMetadata_createTime,
    tableMetadata_partitionKeys,
    tableMetadata_lastAccessTime,
    tableMetadata_columns,
    tableMetadata_parameters,
    tableMetadata_name,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** UnprocessedNamedQueryId
    unprocessedNamedQueryId_namedQueryId,
    unprocessedNamedQueryId_errorMessage,
    unprocessedNamedQueryId_errorCode,

    -- ** UnprocessedQueryExecutionId
    unprocessedQueryExecutionId_queryExecutionId,
    unprocessedQueryExecutionId_errorMessage,
    unprocessedQueryExecutionId_errorCode,

    -- ** WorkGroup
    workGroup_creationTime,
    workGroup_configuration,
    workGroup_state,
    workGroup_description,
    workGroup_name,

    -- ** WorkGroupConfiguration
    workGroupConfiguration_bytesScannedCutoffPerQuery,
    workGroupConfiguration_publishCloudWatchMetricsEnabled,
    workGroupConfiguration_enforceWorkGroupConfiguration,
    workGroupConfiguration_requesterPaysEnabled,
    workGroupConfiguration_engineVersion,
    workGroupConfiguration_resultConfiguration,

    -- ** WorkGroupConfigurationUpdates
    workGroupConfigurationUpdates_bytesScannedCutoffPerQuery,
    workGroupConfigurationUpdates_resultConfigurationUpdates,
    workGroupConfigurationUpdates_publishCloudWatchMetricsEnabled,
    workGroupConfigurationUpdates_enforceWorkGroupConfiguration,
    workGroupConfigurationUpdates_requesterPaysEnabled,
    workGroupConfigurationUpdates_removeBytesScannedCutoffPerQuery,
    workGroupConfigurationUpdates_engineVersion,

    -- ** WorkGroupSummary
    workGroupSummary_creationTime,
    workGroupSummary_state,
    workGroupSummary_name,
    workGroupSummary_engineVersion,
    workGroupSummary_description,
  )
where

import Network.AWS.Athena.BatchGetNamedQuery
import Network.AWS.Athena.BatchGetQueryExecution
import Network.AWS.Athena.CreateDataCatalog
import Network.AWS.Athena.CreateNamedQuery
import Network.AWS.Athena.CreateWorkGroup
import Network.AWS.Athena.DeleteDataCatalog
import Network.AWS.Athena.DeleteNamedQuery
import Network.AWS.Athena.DeleteWorkGroup
import Network.AWS.Athena.GetDataCatalog
import Network.AWS.Athena.GetDatabase
import Network.AWS.Athena.GetNamedQuery
import Network.AWS.Athena.GetQueryExecution
import Network.AWS.Athena.GetQueryResults
import Network.AWS.Athena.GetTableMetadata
import Network.AWS.Athena.GetWorkGroup
import Network.AWS.Athena.ListDataCatalogs
import Network.AWS.Athena.ListDatabases
import Network.AWS.Athena.ListEngineVersions
import Network.AWS.Athena.ListNamedQueries
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
import Network.AWS.Athena.UpdateWorkGroup

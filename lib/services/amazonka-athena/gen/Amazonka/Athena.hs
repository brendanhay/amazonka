{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Athena
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-05-18@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Athena is an interactive query service that lets you use standard
-- SQL to analyze data directly in Amazon S3. You can point Athena at your
-- data in Amazon S3 and run ad-hoc queries and get results in seconds.
-- Athena is serverless, so there is no infrastructure to set up or manage.
-- You pay only for the queries you run. Athena scales
-- automatically—executing queries in parallel—so results are fast, even
-- with large datasets and complex queries. For more information, see
-- <http://docs.aws.amazon.com/athena/latest/ug/what-is.html What is Amazon Athena>
-- in the /Amazon Athena User Guide/.
--
-- If you connect to Athena using the JDBC driver, use version 1.1.0 of the
-- driver or later with the Amazon Athena API. Earlier version drivers do
-- not support the API. For more information and to download the driver,
-- see
-- <https://docs.aws.amazon.com/athena/latest/ug/connect-with-jdbc.html Accessing Amazon Athena with JDBC>.
--
-- For code samples using the Amazon Web Services SDK for Java, see
-- <https://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples>
-- in the /Amazon Athena User Guide/.
module Amazonka.Athena
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** MetadataException
    _MetadataException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchGetNamedQuery
    BatchGetNamedQuery (BatchGetNamedQuery'),
    newBatchGetNamedQuery,
    BatchGetNamedQueryResponse (BatchGetNamedQueryResponse'),
    newBatchGetNamedQueryResponse,

    -- ** BatchGetPreparedStatement
    BatchGetPreparedStatement (BatchGetPreparedStatement'),
    newBatchGetPreparedStatement,
    BatchGetPreparedStatementResponse (BatchGetPreparedStatementResponse'),
    newBatchGetPreparedStatementResponse,

    -- ** BatchGetQueryExecution
    BatchGetQueryExecution (BatchGetQueryExecution'),
    newBatchGetQueryExecution,
    BatchGetQueryExecutionResponse (BatchGetQueryExecutionResponse'),
    newBatchGetQueryExecutionResponse,

    -- ** CreateDataCatalog
    CreateDataCatalog (CreateDataCatalog'),
    newCreateDataCatalog,
    CreateDataCatalogResponse (CreateDataCatalogResponse'),
    newCreateDataCatalogResponse,

    -- ** CreateNamedQuery
    CreateNamedQuery (CreateNamedQuery'),
    newCreateNamedQuery,
    CreateNamedQueryResponse (CreateNamedQueryResponse'),
    newCreateNamedQueryResponse,

    -- ** CreatePreparedStatement
    CreatePreparedStatement (CreatePreparedStatement'),
    newCreatePreparedStatement,
    CreatePreparedStatementResponse (CreatePreparedStatementResponse'),
    newCreatePreparedStatementResponse,

    -- ** CreateWorkGroup
    CreateWorkGroup (CreateWorkGroup'),
    newCreateWorkGroup,
    CreateWorkGroupResponse (CreateWorkGroupResponse'),
    newCreateWorkGroupResponse,

    -- ** DeleteDataCatalog
    DeleteDataCatalog (DeleteDataCatalog'),
    newDeleteDataCatalog,
    DeleteDataCatalogResponse (DeleteDataCatalogResponse'),
    newDeleteDataCatalogResponse,

    -- ** DeleteNamedQuery
    DeleteNamedQuery (DeleteNamedQuery'),
    newDeleteNamedQuery,
    DeleteNamedQueryResponse (DeleteNamedQueryResponse'),
    newDeleteNamedQueryResponse,

    -- ** DeletePreparedStatement
    DeletePreparedStatement (DeletePreparedStatement'),
    newDeletePreparedStatement,
    DeletePreparedStatementResponse (DeletePreparedStatementResponse'),
    newDeletePreparedStatementResponse,

    -- ** DeleteWorkGroup
    DeleteWorkGroup (DeleteWorkGroup'),
    newDeleteWorkGroup,
    DeleteWorkGroupResponse (DeleteWorkGroupResponse'),
    newDeleteWorkGroupResponse,

    -- ** GetDataCatalog
    GetDataCatalog (GetDataCatalog'),
    newGetDataCatalog,
    GetDataCatalogResponse (GetDataCatalogResponse'),
    newGetDataCatalogResponse,

    -- ** GetDatabase
    GetDatabase (GetDatabase'),
    newGetDatabase,
    GetDatabaseResponse (GetDatabaseResponse'),
    newGetDatabaseResponse,

    -- ** GetNamedQuery
    GetNamedQuery (GetNamedQuery'),
    newGetNamedQuery,
    GetNamedQueryResponse (GetNamedQueryResponse'),
    newGetNamedQueryResponse,

    -- ** GetPreparedStatement
    GetPreparedStatement (GetPreparedStatement'),
    newGetPreparedStatement,
    GetPreparedStatementResponse (GetPreparedStatementResponse'),
    newGetPreparedStatementResponse,

    -- ** GetQueryExecution
    GetQueryExecution (GetQueryExecution'),
    newGetQueryExecution,
    GetQueryExecutionResponse (GetQueryExecutionResponse'),
    newGetQueryExecutionResponse,

    -- ** GetQueryResults (Paginated)
    GetQueryResults (GetQueryResults'),
    newGetQueryResults,
    GetQueryResultsResponse (GetQueryResultsResponse'),
    newGetQueryResultsResponse,

    -- ** GetQueryRuntimeStatistics
    GetQueryRuntimeStatistics (GetQueryRuntimeStatistics'),
    newGetQueryRuntimeStatistics,
    GetQueryRuntimeStatisticsResponse (GetQueryRuntimeStatisticsResponse'),
    newGetQueryRuntimeStatisticsResponse,

    -- ** GetTableMetadata
    GetTableMetadata (GetTableMetadata'),
    newGetTableMetadata,
    GetTableMetadataResponse (GetTableMetadataResponse'),
    newGetTableMetadataResponse,

    -- ** GetWorkGroup
    GetWorkGroup (GetWorkGroup'),
    newGetWorkGroup,
    GetWorkGroupResponse (GetWorkGroupResponse'),
    newGetWorkGroupResponse,

    -- ** ListDataCatalogs (Paginated)
    ListDataCatalogs (ListDataCatalogs'),
    newListDataCatalogs,
    ListDataCatalogsResponse (ListDataCatalogsResponse'),
    newListDataCatalogsResponse,

    -- ** ListDatabases (Paginated)
    ListDatabases (ListDatabases'),
    newListDatabases,
    ListDatabasesResponse (ListDatabasesResponse'),
    newListDatabasesResponse,

    -- ** ListEngineVersions
    ListEngineVersions (ListEngineVersions'),
    newListEngineVersions,
    ListEngineVersionsResponse (ListEngineVersionsResponse'),
    newListEngineVersionsResponse,

    -- ** ListNamedQueries (Paginated)
    ListNamedQueries (ListNamedQueries'),
    newListNamedQueries,
    ListNamedQueriesResponse (ListNamedQueriesResponse'),
    newListNamedQueriesResponse,

    -- ** ListPreparedStatements
    ListPreparedStatements (ListPreparedStatements'),
    newListPreparedStatements,
    ListPreparedStatementsResponse (ListPreparedStatementsResponse'),
    newListPreparedStatementsResponse,

    -- ** ListQueryExecutions (Paginated)
    ListQueryExecutions (ListQueryExecutions'),
    newListQueryExecutions,
    ListQueryExecutionsResponse (ListQueryExecutionsResponse'),
    newListQueryExecutionsResponse,

    -- ** ListTableMetadata (Paginated)
    ListTableMetadata (ListTableMetadata'),
    newListTableMetadata,
    ListTableMetadataResponse (ListTableMetadataResponse'),
    newListTableMetadataResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListWorkGroups
    ListWorkGroups (ListWorkGroups'),
    newListWorkGroups,
    ListWorkGroupsResponse (ListWorkGroupsResponse'),
    newListWorkGroupsResponse,

    -- ** StartQueryExecution
    StartQueryExecution (StartQueryExecution'),
    newStartQueryExecution,
    StartQueryExecutionResponse (StartQueryExecutionResponse'),
    newStartQueryExecutionResponse,

    -- ** StopQueryExecution
    StopQueryExecution (StopQueryExecution'),
    newStopQueryExecution,
    StopQueryExecutionResponse (StopQueryExecutionResponse'),
    newStopQueryExecutionResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateDataCatalog
    UpdateDataCatalog (UpdateDataCatalog'),
    newUpdateDataCatalog,
    UpdateDataCatalogResponse (UpdateDataCatalogResponse'),
    newUpdateDataCatalogResponse,

    -- ** UpdateNamedQuery
    UpdateNamedQuery (UpdateNamedQuery'),
    newUpdateNamedQuery,
    UpdateNamedQueryResponse (UpdateNamedQueryResponse'),
    newUpdateNamedQueryResponse,

    -- ** UpdatePreparedStatement
    UpdatePreparedStatement (UpdatePreparedStatement'),
    newUpdatePreparedStatement,
    UpdatePreparedStatementResponse (UpdatePreparedStatementResponse'),
    newUpdatePreparedStatementResponse,

    -- ** UpdateWorkGroup
    UpdateWorkGroup (UpdateWorkGroup'),
    newUpdateWorkGroup,
    UpdateWorkGroupResponse (UpdateWorkGroupResponse'),
    newUpdateWorkGroupResponse,

    -- * Types

    -- ** ColumnNullable
    ColumnNullable (..),

    -- ** DataCatalogType
    DataCatalogType (..),

    -- ** EncryptionOption
    EncryptionOption (..),

    -- ** QueryExecutionState
    QueryExecutionState (..),

    -- ** S3AclOption
    S3AclOption (..),

    -- ** StatementType
    StatementType (..),

    -- ** WorkGroupState
    WorkGroupState (..),

    -- ** AclConfiguration
    AclConfiguration (AclConfiguration'),
    newAclConfiguration,

    -- ** AthenaError
    AthenaError (AthenaError'),
    newAthenaError,

    -- ** Column
    Column (Column'),
    newColumn,

    -- ** ColumnInfo
    ColumnInfo (ColumnInfo'),
    newColumnInfo,

    -- ** DataCatalog
    DataCatalog (DataCatalog'),
    newDataCatalog,

    -- ** DataCatalogSummary
    DataCatalogSummary (DataCatalogSummary'),
    newDataCatalogSummary,

    -- ** Database
    Database (Database'),
    newDatabase,

    -- ** Datum
    Datum (Datum'),
    newDatum,

    -- ** EncryptionConfiguration
    EncryptionConfiguration (EncryptionConfiguration'),
    newEncryptionConfiguration,

    -- ** EngineVersion
    EngineVersion (EngineVersion'),
    newEngineVersion,

    -- ** NamedQuery
    NamedQuery (NamedQuery'),
    newNamedQuery,

    -- ** PreparedStatement
    PreparedStatement (PreparedStatement'),
    newPreparedStatement,

    -- ** PreparedStatementSummary
    PreparedStatementSummary (PreparedStatementSummary'),
    newPreparedStatementSummary,

    -- ** QueryExecution
    QueryExecution (QueryExecution'),
    newQueryExecution,

    -- ** QueryExecutionContext
    QueryExecutionContext (QueryExecutionContext'),
    newQueryExecutionContext,

    -- ** QueryExecutionStatistics
    QueryExecutionStatistics (QueryExecutionStatistics'),
    newQueryExecutionStatistics,

    -- ** QueryExecutionStatus
    QueryExecutionStatus (QueryExecutionStatus'),
    newQueryExecutionStatus,

    -- ** QueryRuntimeStatistics
    QueryRuntimeStatistics (QueryRuntimeStatistics'),
    newQueryRuntimeStatistics,

    -- ** QueryRuntimeStatisticsRows
    QueryRuntimeStatisticsRows (QueryRuntimeStatisticsRows'),
    newQueryRuntimeStatisticsRows,

    -- ** QueryRuntimeStatisticsTimeline
    QueryRuntimeStatisticsTimeline (QueryRuntimeStatisticsTimeline'),
    newQueryRuntimeStatisticsTimeline,

    -- ** QueryStage
    QueryStage (QueryStage'),
    newQueryStage,

    -- ** QueryStagePlanNode
    QueryStagePlanNode (QueryStagePlanNode'),
    newQueryStagePlanNode,

    -- ** ResultConfiguration
    ResultConfiguration (ResultConfiguration'),
    newResultConfiguration,

    -- ** ResultConfigurationUpdates
    ResultConfigurationUpdates (ResultConfigurationUpdates'),
    newResultConfigurationUpdates,

    -- ** ResultReuseByAgeConfiguration
    ResultReuseByAgeConfiguration (ResultReuseByAgeConfiguration'),
    newResultReuseByAgeConfiguration,

    -- ** ResultReuseConfiguration
    ResultReuseConfiguration (ResultReuseConfiguration'),
    newResultReuseConfiguration,

    -- ** ResultReuseInformation
    ResultReuseInformation (ResultReuseInformation'),
    newResultReuseInformation,

    -- ** ResultSet
    ResultSet (ResultSet'),
    newResultSet,

    -- ** ResultSetMetadata
    ResultSetMetadata (ResultSetMetadata'),
    newResultSetMetadata,

    -- ** Row
    Row (Row'),
    newRow,

    -- ** TableMetadata
    TableMetadata (TableMetadata'),
    newTableMetadata,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** UnprocessedNamedQueryId
    UnprocessedNamedQueryId (UnprocessedNamedQueryId'),
    newUnprocessedNamedQueryId,

    -- ** UnprocessedPreparedStatementName
    UnprocessedPreparedStatementName (UnprocessedPreparedStatementName'),
    newUnprocessedPreparedStatementName,

    -- ** UnprocessedQueryExecutionId
    UnprocessedQueryExecutionId (UnprocessedQueryExecutionId'),
    newUnprocessedQueryExecutionId,

    -- ** WorkGroup
    WorkGroup (WorkGroup'),
    newWorkGroup,

    -- ** WorkGroupConfiguration
    WorkGroupConfiguration (WorkGroupConfiguration'),
    newWorkGroupConfiguration,

    -- ** WorkGroupConfigurationUpdates
    WorkGroupConfigurationUpdates (WorkGroupConfigurationUpdates'),
    newWorkGroupConfigurationUpdates,

    -- ** WorkGroupSummary
    WorkGroupSummary (WorkGroupSummary'),
    newWorkGroupSummary,
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
import Amazonka.Athena.Lens
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
import Amazonka.Athena.Types
import Amazonka.Athena.UntagResource
import Amazonka.Athena.UpdateDataCatalog
import Amazonka.Athena.UpdateNamedQuery
import Amazonka.Athena.UpdatePreparedStatement
import Amazonka.Athena.UpdateWorkGroup
import Amazonka.Athena.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Athena'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

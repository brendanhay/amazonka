{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Athena
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Athena
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** MetadataException
    _MetadataException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListDatabases (Paginated)
    ListDatabases (ListDatabases'),
    newListDatabases,
    ListDatabasesResponse (ListDatabasesResponse'),
    newListDatabasesResponse,

    -- ** CreatePreparedStatement
    CreatePreparedStatement (CreatePreparedStatement'),
    newCreatePreparedStatement,
    CreatePreparedStatementResponse (CreatePreparedStatementResponse'),
    newCreatePreparedStatementResponse,

    -- ** DeleteWorkGroup
    DeleteWorkGroup (DeleteWorkGroup'),
    newDeleteWorkGroup,
    DeleteWorkGroupResponse (DeleteWorkGroupResponse'),
    newDeleteWorkGroupResponse,

    -- ** UpdateWorkGroup
    UpdateWorkGroup (UpdateWorkGroup'),
    newUpdateWorkGroup,
    UpdateWorkGroupResponse (UpdateWorkGroupResponse'),
    newUpdateWorkGroupResponse,

    -- ** GetNamedQuery
    GetNamedQuery (GetNamedQuery'),
    newGetNamedQuery,
    GetNamedQueryResponse (GetNamedQueryResponse'),
    newGetNamedQueryResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DeleteDataCatalog
    DeleteDataCatalog (DeleteDataCatalog'),
    newDeleteDataCatalog,
    DeleteDataCatalogResponse (DeleteDataCatalogResponse'),
    newDeleteDataCatalogResponse,

    -- ** UpdateDataCatalog
    UpdateDataCatalog (UpdateDataCatalog'),
    newUpdateDataCatalog,
    UpdateDataCatalogResponse (UpdateDataCatalogResponse'),
    newUpdateDataCatalogResponse,

    -- ** ListDataCatalogs (Paginated)
    ListDataCatalogs (ListDataCatalogs'),
    newListDataCatalogs,
    ListDataCatalogsResponse (ListDataCatalogsResponse'),
    newListDataCatalogsResponse,

    -- ** CreateNamedQuery
    CreateNamedQuery (CreateNamedQuery'),
    newCreateNamedQuery,
    CreateNamedQueryResponse (CreateNamedQueryResponse'),
    newCreateNamedQueryResponse,

    -- ** GetTableMetadata
    GetTableMetadata (GetTableMetadata'),
    newGetTableMetadata,
    GetTableMetadataResponse (GetTableMetadataResponse'),
    newGetTableMetadataResponse,

    -- ** ListNamedQueries (Paginated)
    ListNamedQueries (ListNamedQueries'),
    newListNamedQueries,
    ListNamedQueriesResponse (ListNamedQueriesResponse'),
    newListNamedQueriesResponse,

    -- ** DeleteNamedQuery
    DeleteNamedQuery (DeleteNamedQuery'),
    newDeleteNamedQuery,
    DeleteNamedQueryResponse (DeleteNamedQueryResponse'),
    newDeleteNamedQueryResponse,

    -- ** StartQueryExecution
    StartQueryExecution (StartQueryExecution'),
    newStartQueryExecution,
    StartQueryExecutionResponse (StartQueryExecutionResponse'),
    newStartQueryExecutionResponse,

    -- ** BatchGetNamedQuery
    BatchGetNamedQuery (BatchGetNamedQuery'),
    newBatchGetNamedQuery,
    BatchGetNamedQueryResponse (BatchGetNamedQueryResponse'),
    newBatchGetNamedQueryResponse,

    -- ** GetQueryExecution
    GetQueryExecution (GetQueryExecution'),
    newGetQueryExecution,
    GetQueryExecutionResponse (GetQueryExecutionResponse'),
    newGetQueryExecutionResponse,

    -- ** ListPreparedStatements
    ListPreparedStatements (ListPreparedStatements'),
    newListPreparedStatements,
    ListPreparedStatementsResponse (ListPreparedStatementsResponse'),
    newListPreparedStatementsResponse,

    -- ** CreateDataCatalog
    CreateDataCatalog (CreateDataCatalog'),
    newCreateDataCatalog,
    CreateDataCatalogResponse (CreateDataCatalogResponse'),
    newCreateDataCatalogResponse,

    -- ** ListWorkGroups
    ListWorkGroups (ListWorkGroups'),
    newListWorkGroups,
    ListWorkGroupsResponse (ListWorkGroupsResponse'),
    newListWorkGroupsResponse,

    -- ** CreateWorkGroup
    CreateWorkGroup (CreateWorkGroup'),
    newCreateWorkGroup,
    CreateWorkGroupResponse (CreateWorkGroupResponse'),
    newCreateWorkGroupResponse,

    -- ** BatchGetQueryExecution
    BatchGetQueryExecution (BatchGetQueryExecution'),
    newBatchGetQueryExecution,
    BatchGetQueryExecutionResponse (BatchGetQueryExecutionResponse'),
    newBatchGetQueryExecutionResponse,

    -- ** ListEngineVersions
    ListEngineVersions (ListEngineVersions'),
    newListEngineVersions,
    ListEngineVersionsResponse (ListEngineVersionsResponse'),
    newListEngineVersionsResponse,

    -- ** GetDataCatalog
    GetDataCatalog (GetDataCatalog'),
    newGetDataCatalog,
    GetDataCatalogResponse (GetDataCatalogResponse'),
    newGetDataCatalogResponse,

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

    -- ** GetWorkGroup
    GetWorkGroup (GetWorkGroup'),
    newGetWorkGroup,
    GetWorkGroupResponse (GetWorkGroupResponse'),
    newGetWorkGroupResponse,

    -- ** GetDatabase
    GetDatabase (GetDatabase'),
    newGetDatabase,
    GetDatabaseResponse (GetDatabaseResponse'),
    newGetDatabaseResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** GetPreparedStatement
    GetPreparedStatement (GetPreparedStatement'),
    newGetPreparedStatement,
    GetPreparedStatementResponse (GetPreparedStatementResponse'),
    newGetPreparedStatementResponse,

    -- ** GetQueryResults (Paginated)
    GetQueryResults (GetQueryResults'),
    newGetQueryResults,
    GetQueryResultsResponse (GetQueryResultsResponse'),
    newGetQueryResultsResponse,

    -- ** ListTableMetadata (Paginated)
    ListTableMetadata (ListTableMetadata'),
    newListTableMetadata,
    ListTableMetadataResponse (ListTableMetadataResponse'),
    newListTableMetadataResponse,

    -- ** ListQueryExecutions (Paginated)
    ListQueryExecutions (ListQueryExecutions'),
    newListQueryExecutions,
    ListQueryExecutionsResponse (ListQueryExecutionsResponse'),
    newListQueryExecutionsResponse,

    -- ** DeletePreparedStatement
    DeletePreparedStatement (DeletePreparedStatement'),
    newDeletePreparedStatement,
    DeletePreparedStatementResponse (DeletePreparedStatementResponse'),
    newDeletePreparedStatementResponse,

    -- ** UpdatePreparedStatement
    UpdatePreparedStatement (UpdatePreparedStatement'),
    newUpdatePreparedStatement,
    UpdatePreparedStatementResponse (UpdatePreparedStatementResponse'),
    newUpdatePreparedStatementResponse,

    -- * Types

    -- ** ColumnNullable
    ColumnNullable (..),

    -- ** DataCatalogType
    DataCatalogType (..),

    -- ** EncryptionOption
    EncryptionOption (..),

    -- ** QueryExecutionState
    QueryExecutionState (..),

    -- ** StatementType
    StatementType (..),

    -- ** WorkGroupState
    WorkGroupState (..),

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

    -- ** ResultConfiguration
    ResultConfiguration (ResultConfiguration'),
    newResultConfiguration,

    -- ** ResultConfigurationUpdates
    ResultConfigurationUpdates (ResultConfigurationUpdates'),
    newResultConfigurationUpdates,

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
import Network.AWS.Athena.Lens
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
import Network.AWS.Athena.Types
import Network.AWS.Athena.UntagResource
import Network.AWS.Athena.UpdateDataCatalog
import Network.AWS.Athena.UpdatePreparedStatement
import Network.AWS.Athena.UpdateWorkGroup
import Network.AWS.Athena.Waiters

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

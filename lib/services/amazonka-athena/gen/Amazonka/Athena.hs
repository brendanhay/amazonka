{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Athena
-- Copyright   : (c) 2013-2023 Brendan Hay
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

    -- ** InternalServerException
    _InternalServerException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** MetadataException
    _MetadataException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** SessionAlreadyExistsException
    _SessionAlreadyExistsException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

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

    -- ** CancelCapacityReservation
    CancelCapacityReservation (CancelCapacityReservation'),
    newCancelCapacityReservation,
    CancelCapacityReservationResponse (CancelCapacityReservationResponse'),
    newCancelCapacityReservationResponse,

    -- ** CreateCapacityReservation
    CreateCapacityReservation (CreateCapacityReservation'),
    newCreateCapacityReservation,
    CreateCapacityReservationResponse (CreateCapacityReservationResponse'),
    newCreateCapacityReservationResponse,

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

    -- ** CreateNotebook
    CreateNotebook (CreateNotebook'),
    newCreateNotebook,
    CreateNotebookResponse (CreateNotebookResponse'),
    newCreateNotebookResponse,

    -- ** CreatePreparedStatement
    CreatePreparedStatement (CreatePreparedStatement'),
    newCreatePreparedStatement,
    CreatePreparedStatementResponse (CreatePreparedStatementResponse'),
    newCreatePreparedStatementResponse,

    -- ** CreatePresignedNotebookUrl
    CreatePresignedNotebookUrl (CreatePresignedNotebookUrl'),
    newCreatePresignedNotebookUrl,
    CreatePresignedNotebookUrlResponse (CreatePresignedNotebookUrlResponse'),
    newCreatePresignedNotebookUrlResponse,

    -- ** CreateWorkGroup
    CreateWorkGroup (CreateWorkGroup'),
    newCreateWorkGroup,
    CreateWorkGroupResponse (CreateWorkGroupResponse'),
    newCreateWorkGroupResponse,

    -- ** DeleteCapacityReservation
    DeleteCapacityReservation (DeleteCapacityReservation'),
    newDeleteCapacityReservation,
    DeleteCapacityReservationResponse (DeleteCapacityReservationResponse'),
    newDeleteCapacityReservationResponse,

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

    -- ** DeleteNotebook
    DeleteNotebook (DeleteNotebook'),
    newDeleteNotebook,
    DeleteNotebookResponse (DeleteNotebookResponse'),
    newDeleteNotebookResponse,

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

    -- ** ExportNotebook
    ExportNotebook (ExportNotebook'),
    newExportNotebook,
    ExportNotebookResponse (ExportNotebookResponse'),
    newExportNotebookResponse,

    -- ** GetCalculationExecution
    GetCalculationExecution (GetCalculationExecution'),
    newGetCalculationExecution,
    GetCalculationExecutionResponse (GetCalculationExecutionResponse'),
    newGetCalculationExecutionResponse,

    -- ** GetCalculationExecutionCode
    GetCalculationExecutionCode (GetCalculationExecutionCode'),
    newGetCalculationExecutionCode,
    GetCalculationExecutionCodeResponse (GetCalculationExecutionCodeResponse'),
    newGetCalculationExecutionCodeResponse,

    -- ** GetCalculationExecutionStatus
    GetCalculationExecutionStatus (GetCalculationExecutionStatus'),
    newGetCalculationExecutionStatus,
    GetCalculationExecutionStatusResponse (GetCalculationExecutionStatusResponse'),
    newGetCalculationExecutionStatusResponse,

    -- ** GetCapacityAssignmentConfiguration
    GetCapacityAssignmentConfiguration (GetCapacityAssignmentConfiguration'),
    newGetCapacityAssignmentConfiguration,
    GetCapacityAssignmentConfigurationResponse (GetCapacityAssignmentConfigurationResponse'),
    newGetCapacityAssignmentConfigurationResponse,

    -- ** GetCapacityReservation
    GetCapacityReservation (GetCapacityReservation'),
    newGetCapacityReservation,
    GetCapacityReservationResponse (GetCapacityReservationResponse'),
    newGetCapacityReservationResponse,

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

    -- ** GetNotebookMetadata
    GetNotebookMetadata (GetNotebookMetadata'),
    newGetNotebookMetadata,
    GetNotebookMetadataResponse (GetNotebookMetadataResponse'),
    newGetNotebookMetadataResponse,

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

    -- ** GetSession
    GetSession (GetSession'),
    newGetSession,
    GetSessionResponse (GetSessionResponse'),
    newGetSessionResponse,

    -- ** GetSessionStatus
    GetSessionStatus (GetSessionStatus'),
    newGetSessionStatus,
    GetSessionStatusResponse (GetSessionStatusResponse'),
    newGetSessionStatusResponse,

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

    -- ** ImportNotebook
    ImportNotebook (ImportNotebook'),
    newImportNotebook,
    ImportNotebookResponse (ImportNotebookResponse'),
    newImportNotebookResponse,

    -- ** ListApplicationDPUSizes
    ListApplicationDPUSizes (ListApplicationDPUSizes'),
    newListApplicationDPUSizes,
    ListApplicationDPUSizesResponse (ListApplicationDPUSizesResponse'),
    newListApplicationDPUSizesResponse,

    -- ** ListCalculationExecutions
    ListCalculationExecutions (ListCalculationExecutions'),
    newListCalculationExecutions,
    ListCalculationExecutionsResponse (ListCalculationExecutionsResponse'),
    newListCalculationExecutionsResponse,

    -- ** ListCapacityReservations
    ListCapacityReservations (ListCapacityReservations'),
    newListCapacityReservations,
    ListCapacityReservationsResponse (ListCapacityReservationsResponse'),
    newListCapacityReservationsResponse,

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

    -- ** ListExecutors
    ListExecutors (ListExecutors'),
    newListExecutors,
    ListExecutorsResponse (ListExecutorsResponse'),
    newListExecutorsResponse,

    -- ** ListNamedQueries (Paginated)
    ListNamedQueries (ListNamedQueries'),
    newListNamedQueries,
    ListNamedQueriesResponse (ListNamedQueriesResponse'),
    newListNamedQueriesResponse,

    -- ** ListNotebookMetadata
    ListNotebookMetadata (ListNotebookMetadata'),
    newListNotebookMetadata,
    ListNotebookMetadataResponse (ListNotebookMetadataResponse'),
    newListNotebookMetadataResponse,

    -- ** ListNotebookSessions
    ListNotebookSessions (ListNotebookSessions'),
    newListNotebookSessions,
    ListNotebookSessionsResponse (ListNotebookSessionsResponse'),
    newListNotebookSessionsResponse,

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

    -- ** ListSessions
    ListSessions (ListSessions'),
    newListSessions,
    ListSessionsResponse (ListSessionsResponse'),
    newListSessionsResponse,

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

    -- ** PutCapacityAssignmentConfiguration
    PutCapacityAssignmentConfiguration (PutCapacityAssignmentConfiguration'),
    newPutCapacityAssignmentConfiguration,
    PutCapacityAssignmentConfigurationResponse (PutCapacityAssignmentConfigurationResponse'),
    newPutCapacityAssignmentConfigurationResponse,

    -- ** StartCalculationExecution
    StartCalculationExecution (StartCalculationExecution'),
    newStartCalculationExecution,
    StartCalculationExecutionResponse (StartCalculationExecutionResponse'),
    newStartCalculationExecutionResponse,

    -- ** StartQueryExecution
    StartQueryExecution (StartQueryExecution'),
    newStartQueryExecution,
    StartQueryExecutionResponse (StartQueryExecutionResponse'),
    newStartQueryExecutionResponse,

    -- ** StartSession
    StartSession (StartSession'),
    newStartSession,
    StartSessionResponse (StartSessionResponse'),
    newStartSessionResponse,

    -- ** StopCalculationExecution
    StopCalculationExecution (StopCalculationExecution'),
    newStopCalculationExecution,
    StopCalculationExecutionResponse (StopCalculationExecutionResponse'),
    newStopCalculationExecutionResponse,

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

    -- ** TerminateSession
    TerminateSession (TerminateSession'),
    newTerminateSession,
    TerminateSessionResponse (TerminateSessionResponse'),
    newTerminateSessionResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateCapacityReservation
    UpdateCapacityReservation (UpdateCapacityReservation'),
    newUpdateCapacityReservation,
    UpdateCapacityReservationResponse (UpdateCapacityReservationResponse'),
    newUpdateCapacityReservationResponse,

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

    -- ** UpdateNotebook
    UpdateNotebook (UpdateNotebook'),
    newUpdateNotebook,
    UpdateNotebookResponse (UpdateNotebookResponse'),
    newUpdateNotebookResponse,

    -- ** UpdateNotebookMetadata
    UpdateNotebookMetadata (UpdateNotebookMetadata'),
    newUpdateNotebookMetadata,
    UpdateNotebookMetadataResponse (UpdateNotebookMetadataResponse'),
    newUpdateNotebookMetadataResponse,

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

    -- ** CalculationExecutionState
    CalculationExecutionState (..),

    -- ** CapacityAllocationStatus
    CapacityAllocationStatus (..),

    -- ** CapacityReservationStatus
    CapacityReservationStatus (..),

    -- ** ColumnNullable
    ColumnNullable (..),

    -- ** DataCatalogType
    DataCatalogType (..),

    -- ** EncryptionOption
    EncryptionOption (..),

    -- ** ExecutorState
    ExecutorState (..),

    -- ** ExecutorType
    ExecutorType (..),

    -- ** NotebookType
    NotebookType (..),

    -- ** QueryExecutionState
    QueryExecutionState (..),

    -- ** S3AclOption
    S3AclOption (..),

    -- ** SessionState
    SessionState (..),

    -- ** StatementType
    StatementType (..),

    -- ** WorkGroupState
    WorkGroupState (..),

    -- ** AclConfiguration
    AclConfiguration (AclConfiguration'),
    newAclConfiguration,

    -- ** ApplicationDPUSizes
    ApplicationDPUSizes (ApplicationDPUSizes'),
    newApplicationDPUSizes,

    -- ** AthenaError
    AthenaError (AthenaError'),
    newAthenaError,

    -- ** CalculationConfiguration
    CalculationConfiguration (CalculationConfiguration'),
    newCalculationConfiguration,

    -- ** CalculationResult
    CalculationResult (CalculationResult'),
    newCalculationResult,

    -- ** CalculationStatistics
    CalculationStatistics (CalculationStatistics'),
    newCalculationStatistics,

    -- ** CalculationStatus
    CalculationStatus (CalculationStatus'),
    newCalculationStatus,

    -- ** CalculationSummary
    CalculationSummary (CalculationSummary'),
    newCalculationSummary,

    -- ** CapacityAllocation
    CapacityAllocation (CapacityAllocation'),
    newCapacityAllocation,

    -- ** CapacityAssignment
    CapacityAssignment (CapacityAssignment'),
    newCapacityAssignment,

    -- ** CapacityAssignmentConfiguration
    CapacityAssignmentConfiguration (CapacityAssignmentConfiguration'),
    newCapacityAssignmentConfiguration,

    -- ** CapacityReservation
    CapacityReservation (CapacityReservation'),
    newCapacityReservation,

    -- ** Column
    Column (Column'),
    newColumn,

    -- ** ColumnInfo
    ColumnInfo (ColumnInfo'),
    newColumnInfo,

    -- ** CustomerContentEncryptionConfiguration
    CustomerContentEncryptionConfiguration (CustomerContentEncryptionConfiguration'),
    newCustomerContentEncryptionConfiguration,

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

    -- ** EngineConfiguration
    EngineConfiguration (EngineConfiguration'),
    newEngineConfiguration,

    -- ** EngineVersion
    EngineVersion (EngineVersion'),
    newEngineVersion,

    -- ** ExecutorsSummary
    ExecutorsSummary (ExecutorsSummary'),
    newExecutorsSummary,

    -- ** FilterDefinition
    FilterDefinition (FilterDefinition'),
    newFilterDefinition,

    -- ** NamedQuery
    NamedQuery (NamedQuery'),
    newNamedQuery,

    -- ** NotebookMetadata
    NotebookMetadata (NotebookMetadata'),
    newNotebookMetadata,

    -- ** NotebookSessionSummary
    NotebookSessionSummary (NotebookSessionSummary'),
    newNotebookSessionSummary,

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

    -- ** SessionConfiguration
    SessionConfiguration (SessionConfiguration'),
    newSessionConfiguration,

    -- ** SessionStatistics
    SessionStatistics (SessionStatistics'),
    newSessionStatistics,

    -- ** SessionStatus
    SessionStatus (SessionStatus'),
    newSessionStatus,

    -- ** SessionSummary
    SessionSummary (SessionSummary'),
    newSessionSummary,

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
import Amazonka.Athena.CancelCapacityReservation
import Amazonka.Athena.CreateCapacityReservation
import Amazonka.Athena.CreateDataCatalog
import Amazonka.Athena.CreateNamedQuery
import Amazonka.Athena.CreateNotebook
import Amazonka.Athena.CreatePreparedStatement
import Amazonka.Athena.CreatePresignedNotebookUrl
import Amazonka.Athena.CreateWorkGroup
import Amazonka.Athena.DeleteCapacityReservation
import Amazonka.Athena.DeleteDataCatalog
import Amazonka.Athena.DeleteNamedQuery
import Amazonka.Athena.DeleteNotebook
import Amazonka.Athena.DeletePreparedStatement
import Amazonka.Athena.DeleteWorkGroup
import Amazonka.Athena.ExportNotebook
import Amazonka.Athena.GetCalculationExecution
import Amazonka.Athena.GetCalculationExecutionCode
import Amazonka.Athena.GetCalculationExecutionStatus
import Amazonka.Athena.GetCapacityAssignmentConfiguration
import Amazonka.Athena.GetCapacityReservation
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
import Amazonka.Athena.Lens
import Amazonka.Athena.ListApplicationDPUSizes
import Amazonka.Athena.ListCalculationExecutions
import Amazonka.Athena.ListCapacityReservations
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
import Amazonka.Athena.PutCapacityAssignmentConfiguration
import Amazonka.Athena.StartCalculationExecution
import Amazonka.Athena.StartQueryExecution
import Amazonka.Athena.StartSession
import Amazonka.Athena.StopCalculationExecution
import Amazonka.Athena.StopQueryExecution
import Amazonka.Athena.TagResource
import Amazonka.Athena.TerminateSession
import Amazonka.Athena.Types
import Amazonka.Athena.UntagResource
import Amazonka.Athena.UpdateCapacityReservation
import Amazonka.Athena.UpdateDataCatalog
import Amazonka.Athena.UpdateNamedQuery
import Amazonka.Athena.UpdateNotebook
import Amazonka.Athena.UpdateNotebookMetadata
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

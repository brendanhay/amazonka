{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.HoneyCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-03-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Honeycode is a fully managed service that allows you to quickly
-- build mobile and web apps for teams—without programming. Build Honeycode
-- apps for managing almost anything, like projects, customers, operations,
-- approvals, resources, and even your team.
module Network.AWS.HoneyCode
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** AutomationExecutionTimeoutException
    _AutomationExecutionTimeoutException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** AutomationExecutionException
    _AutomationExecutionException,

    -- ** RequestTimeoutException
    _RequestTimeoutException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchDeleteTableRows
    BatchDeleteTableRows (BatchDeleteTableRows'),
    newBatchDeleteTableRows,
    BatchDeleteTableRowsResponse (BatchDeleteTableRowsResponse'),
    newBatchDeleteTableRowsResponse,

    -- ** BatchUpdateTableRows
    BatchUpdateTableRows (BatchUpdateTableRows'),
    newBatchUpdateTableRows,
    BatchUpdateTableRowsResponse (BatchUpdateTableRowsResponse'),
    newBatchUpdateTableRowsResponse,

    -- ** ListTableRows (Paginated)
    ListTableRows (ListTableRows'),
    newListTableRows,
    ListTableRowsResponse (ListTableRowsResponse'),
    newListTableRowsResponse,

    -- ** InvokeScreenAutomation
    InvokeScreenAutomation (InvokeScreenAutomation'),
    newInvokeScreenAutomation,
    InvokeScreenAutomationResponse (InvokeScreenAutomationResponse'),
    newInvokeScreenAutomationResponse,

    -- ** DescribeTableDataImportJob
    DescribeTableDataImportJob (DescribeTableDataImportJob'),
    newDescribeTableDataImportJob,
    DescribeTableDataImportJobResponse (DescribeTableDataImportJobResponse'),
    newDescribeTableDataImportJobResponse,

    -- ** StartTableDataImportJob
    StartTableDataImportJob (StartTableDataImportJob'),
    newStartTableDataImportJob,
    StartTableDataImportJobResponse (StartTableDataImportJobResponse'),
    newStartTableDataImportJobResponse,

    -- ** BatchCreateTableRows
    BatchCreateTableRows (BatchCreateTableRows'),
    newBatchCreateTableRows,
    BatchCreateTableRowsResponse (BatchCreateTableRowsResponse'),
    newBatchCreateTableRowsResponse,

    -- ** ListTables (Paginated)
    ListTables (ListTables'),
    newListTables,
    ListTablesResponse (ListTablesResponse'),
    newListTablesResponse,

    -- ** GetScreenData
    GetScreenData (GetScreenData'),
    newGetScreenData,
    GetScreenDataResponse (GetScreenDataResponse'),
    newGetScreenDataResponse,

    -- ** QueryTableRows (Paginated)
    QueryTableRows (QueryTableRows'),
    newQueryTableRows,
    QueryTableRowsResponse (QueryTableRowsResponse'),
    newQueryTableRowsResponse,

    -- ** BatchUpsertTableRows
    BatchUpsertTableRows (BatchUpsertTableRows'),
    newBatchUpsertTableRows,
    BatchUpsertTableRowsResponse (BatchUpsertTableRowsResponse'),
    newBatchUpsertTableRowsResponse,

    -- ** ListTableColumns (Paginated)
    ListTableColumns (ListTableColumns'),
    newListTableColumns,
    ListTableColumnsResponse (ListTableColumnsResponse'),
    newListTableColumnsResponse,

    -- * Types

    -- ** Format
    Format (..),

    -- ** ImportDataCharacterEncoding
    ImportDataCharacterEncoding (..),

    -- ** ImportSourceDataFormat
    ImportSourceDataFormat (..),

    -- ** TableDataImportJobStatus
    TableDataImportJobStatus (..),

    -- ** UpsertAction
    UpsertAction (..),

    -- ** Cell
    Cell (Cell'),
    newCell,

    -- ** CellInput
    CellInput (CellInput'),
    newCellInput,

    -- ** ColumnMetadata
    ColumnMetadata (ColumnMetadata'),
    newColumnMetadata,

    -- ** CreateRowData
    CreateRowData (CreateRowData'),
    newCreateRowData,

    -- ** DataItem
    DataItem (DataItem'),
    newDataItem,

    -- ** DelimitedTextImportOptions
    DelimitedTextImportOptions (DelimitedTextImportOptions'),
    newDelimitedTextImportOptions,

    -- ** DestinationOptions
    DestinationOptions (DestinationOptions'),
    newDestinationOptions,

    -- ** FailedBatchItem
    FailedBatchItem (FailedBatchItem'),
    newFailedBatchItem,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** ImportDataSource
    ImportDataSource (ImportDataSource'),
    newImportDataSource,

    -- ** ImportDataSourceConfig
    ImportDataSourceConfig (ImportDataSourceConfig'),
    newImportDataSourceConfig,

    -- ** ImportJobSubmitter
    ImportJobSubmitter (ImportJobSubmitter'),
    newImportJobSubmitter,

    -- ** ImportOptions
    ImportOptions (ImportOptions'),
    newImportOptions,

    -- ** ResultRow
    ResultRow (ResultRow'),
    newResultRow,

    -- ** ResultSet
    ResultSet (ResultSet'),
    newResultSet,

    -- ** SourceDataColumnProperties
    SourceDataColumnProperties (SourceDataColumnProperties'),
    newSourceDataColumnProperties,

    -- ** Table
    Table (Table'),
    newTable,

    -- ** TableColumn
    TableColumn (TableColumn'),
    newTableColumn,

    -- ** TableDataImportJobMetadata
    TableDataImportJobMetadata (TableDataImportJobMetadata'),
    newTableDataImportJobMetadata,

    -- ** TableRow
    TableRow (TableRow'),
    newTableRow,

    -- ** UpdateRowData
    UpdateRowData (UpdateRowData'),
    newUpdateRowData,

    -- ** UpsertRowData
    UpsertRowData (UpsertRowData'),
    newUpsertRowData,

    -- ** UpsertRowsResult
    UpsertRowsResult (UpsertRowsResult'),
    newUpsertRowsResult,

    -- ** VariableValue
    VariableValue (VariableValue'),
    newVariableValue,
  )
where

import Network.AWS.HoneyCode.BatchCreateTableRows
import Network.AWS.HoneyCode.BatchDeleteTableRows
import Network.AWS.HoneyCode.BatchUpdateTableRows
import Network.AWS.HoneyCode.BatchUpsertTableRows
import Network.AWS.HoneyCode.DescribeTableDataImportJob
import Network.AWS.HoneyCode.GetScreenData
import Network.AWS.HoneyCode.InvokeScreenAutomation
import Network.AWS.HoneyCode.Lens
import Network.AWS.HoneyCode.ListTableColumns
import Network.AWS.HoneyCode.ListTableRows
import Network.AWS.HoneyCode.ListTables
import Network.AWS.HoneyCode.QueryTableRows
import Network.AWS.HoneyCode.StartTableDataImportJob
import Network.AWS.HoneyCode.Types
import Network.AWS.HoneyCode.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'HoneyCode'.

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

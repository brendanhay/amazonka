{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.HoneyCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-03-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Honeycode is a fully managed service that allows you to quickly
-- build mobile and web apps for teams—without programming. Build Honeycode
-- apps for managing almost anything, like projects, customers, operations,
-- approvals, resources, and even your team.
module Amazonka.HoneyCode
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** AutomationExecutionException
    _AutomationExecutionException,

    -- ** AutomationExecutionTimeoutException
    _AutomationExecutionTimeoutException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** RequestTimeoutException
    _RequestTimeoutException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchCreateTableRows
    BatchCreateTableRows (BatchCreateTableRows'),
    newBatchCreateTableRows,
    BatchCreateTableRowsResponse (BatchCreateTableRowsResponse'),
    newBatchCreateTableRowsResponse,

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

    -- ** BatchUpsertTableRows
    BatchUpsertTableRows (BatchUpsertTableRows'),
    newBatchUpsertTableRows,
    BatchUpsertTableRowsResponse (BatchUpsertTableRowsResponse'),
    newBatchUpsertTableRowsResponse,

    -- ** DescribeTableDataImportJob
    DescribeTableDataImportJob (DescribeTableDataImportJob'),
    newDescribeTableDataImportJob,
    DescribeTableDataImportJobResponse (DescribeTableDataImportJobResponse'),
    newDescribeTableDataImportJobResponse,

    -- ** GetScreenData
    GetScreenData (GetScreenData'),
    newGetScreenData,
    GetScreenDataResponse (GetScreenDataResponse'),
    newGetScreenDataResponse,

    -- ** InvokeScreenAutomation
    InvokeScreenAutomation (InvokeScreenAutomation'),
    newInvokeScreenAutomation,
    InvokeScreenAutomationResponse (InvokeScreenAutomationResponse'),
    newInvokeScreenAutomationResponse,

    -- ** ListTableColumns (Paginated)
    ListTableColumns (ListTableColumns'),
    newListTableColumns,
    ListTableColumnsResponse (ListTableColumnsResponse'),
    newListTableColumnsResponse,

    -- ** ListTableRows (Paginated)
    ListTableRows (ListTableRows'),
    newListTableRows,
    ListTableRowsResponse (ListTableRowsResponse'),
    newListTableRowsResponse,

    -- ** ListTables (Paginated)
    ListTables (ListTables'),
    newListTables,
    ListTablesResponse (ListTablesResponse'),
    newListTablesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** QueryTableRows (Paginated)
    QueryTableRows (QueryTableRows'),
    newQueryTableRows,
    QueryTableRowsResponse (QueryTableRowsResponse'),
    newQueryTableRowsResponse,

    -- ** StartTableDataImportJob
    StartTableDataImportJob (StartTableDataImportJob'),
    newStartTableDataImportJob,
    StartTableDataImportJobResponse (StartTableDataImportJobResponse'),
    newStartTableDataImportJobResponse,

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

    -- * Types

    -- ** ErrorCode
    ErrorCode (..),

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

import Amazonka.HoneyCode.BatchCreateTableRows
import Amazonka.HoneyCode.BatchDeleteTableRows
import Amazonka.HoneyCode.BatchUpdateTableRows
import Amazonka.HoneyCode.BatchUpsertTableRows
import Amazonka.HoneyCode.DescribeTableDataImportJob
import Amazonka.HoneyCode.GetScreenData
import Amazonka.HoneyCode.InvokeScreenAutomation
import Amazonka.HoneyCode.Lens
import Amazonka.HoneyCode.ListTableColumns
import Amazonka.HoneyCode.ListTableRows
import Amazonka.HoneyCode.ListTables
import Amazonka.HoneyCode.ListTagsForResource
import Amazonka.HoneyCode.QueryTableRows
import Amazonka.HoneyCode.StartTableDataImportJob
import Amazonka.HoneyCode.TagResource
import Amazonka.HoneyCode.Types
import Amazonka.HoneyCode.UntagResource
import Amazonka.HoneyCode.Waiters

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

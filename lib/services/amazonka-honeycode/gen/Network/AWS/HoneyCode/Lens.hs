{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.HoneyCode.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.HoneyCode.Lens
  ( -- * Operations

    -- ** BatchDeleteTableRows
    batchDeleteTableRows_clientRequestToken,
    batchDeleteTableRows_workbookId,
    batchDeleteTableRows_tableId,
    batchDeleteTableRows_rowIds,
    batchDeleteTableRowsResponse_failedBatchItems,
    batchDeleteTableRowsResponse_httpStatus,
    batchDeleteTableRowsResponse_workbookCursor,

    -- ** BatchUpdateTableRows
    batchUpdateTableRows_clientRequestToken,
    batchUpdateTableRows_workbookId,
    batchUpdateTableRows_tableId,
    batchUpdateTableRows_rowsToUpdate,
    batchUpdateTableRowsResponse_failedBatchItems,
    batchUpdateTableRowsResponse_httpStatus,
    batchUpdateTableRowsResponse_workbookCursor,

    -- ** ListTableRows
    listTableRows_rowIds,
    listTableRows_nextToken,
    listTableRows_maxResults,
    listTableRows_workbookId,
    listTableRows_tableId,
    listTableRowsResponse_rowIdsNotFound,
    listTableRowsResponse_nextToken,
    listTableRowsResponse_httpStatus,
    listTableRowsResponse_columnIds,
    listTableRowsResponse_rows,
    listTableRowsResponse_workbookCursor,

    -- ** InvokeScreenAutomation
    invokeScreenAutomation_rowId,
    invokeScreenAutomation_variables,
    invokeScreenAutomation_clientRequestToken,
    invokeScreenAutomation_workbookId,
    invokeScreenAutomation_appId,
    invokeScreenAutomation_screenId,
    invokeScreenAutomation_screenAutomationId,
    invokeScreenAutomationResponse_httpStatus,
    invokeScreenAutomationResponse_workbookCursor,

    -- ** DescribeTableDataImportJob
    describeTableDataImportJob_workbookId,
    describeTableDataImportJob_tableId,
    describeTableDataImportJob_jobId,
    describeTableDataImportJobResponse_httpStatus,
    describeTableDataImportJobResponse_jobStatus,
    describeTableDataImportJobResponse_message,
    describeTableDataImportJobResponse_jobMetadata,

    -- ** StartTableDataImportJob
    startTableDataImportJob_workbookId,
    startTableDataImportJob_dataSource,
    startTableDataImportJob_dataFormat,
    startTableDataImportJob_destinationTableId,
    startTableDataImportJob_importOptions,
    startTableDataImportJob_clientRequestToken,
    startTableDataImportJobResponse_httpStatus,
    startTableDataImportJobResponse_jobId,
    startTableDataImportJobResponse_jobStatus,

    -- ** BatchCreateTableRows
    batchCreateTableRows_clientRequestToken,
    batchCreateTableRows_workbookId,
    batchCreateTableRows_tableId,
    batchCreateTableRows_rowsToCreate,
    batchCreateTableRowsResponse_failedBatchItems,
    batchCreateTableRowsResponse_httpStatus,
    batchCreateTableRowsResponse_workbookCursor,
    batchCreateTableRowsResponse_createdRows,

    -- ** ListTables
    listTables_nextToken,
    listTables_maxResults,
    listTables_workbookId,
    listTablesResponse_nextToken,
    listTablesResponse_workbookCursor,
    listTablesResponse_httpStatus,
    listTablesResponse_tables,

    -- ** GetScreenData
    getScreenData_variables,
    getScreenData_nextToken,
    getScreenData_maxResults,
    getScreenData_workbookId,
    getScreenData_appId,
    getScreenData_screenId,
    getScreenDataResponse_nextToken,
    getScreenDataResponse_httpStatus,
    getScreenDataResponse_results,
    getScreenDataResponse_workbookCursor,

    -- ** QueryTableRows
    queryTableRows_nextToken,
    queryTableRows_maxResults,
    queryTableRows_workbookId,
    queryTableRows_tableId,
    queryTableRows_filterFormula,
    queryTableRowsResponse_nextToken,
    queryTableRowsResponse_httpStatus,
    queryTableRowsResponse_columnIds,
    queryTableRowsResponse_rows,
    queryTableRowsResponse_workbookCursor,

    -- ** BatchUpsertTableRows
    batchUpsertTableRows_clientRequestToken,
    batchUpsertTableRows_workbookId,
    batchUpsertTableRows_tableId,
    batchUpsertTableRows_rowsToUpsert,
    batchUpsertTableRowsResponse_failedBatchItems,
    batchUpsertTableRowsResponse_httpStatus,
    batchUpsertTableRowsResponse_rows,
    batchUpsertTableRowsResponse_workbookCursor,

    -- ** ListTableColumns
    listTableColumns_nextToken,
    listTableColumns_workbookId,
    listTableColumns_tableId,
    listTableColumnsResponse_nextToken,
    listTableColumnsResponse_workbookCursor,
    listTableColumnsResponse_httpStatus,
    listTableColumnsResponse_tableColumns,

    -- * Types

    -- ** Cell
    cell_rawValue,
    cell_format,
    cell_formula,
    cell_formattedValue,

    -- ** CellInput
    cellInput_fact,

    -- ** ColumnMetadata
    columnMetadata_name,
    columnMetadata_format,

    -- ** CreateRowData
    createRowData_batchItemId,
    createRowData_cellsToCreate,

    -- ** DataItem
    dataItem_rawValue,
    dataItem_overrideFormat,
    dataItem_formattedValue,

    -- ** DelimitedTextImportOptions
    delimitedTextImportOptions_ignoreEmptyRows,
    delimitedTextImportOptions_hasHeaderRow,
    delimitedTextImportOptions_dataCharacterEncoding,
    delimitedTextImportOptions_delimiter,

    -- ** DestinationOptions
    destinationOptions_columnMap,

    -- ** FailedBatchItem
    failedBatchItem_id,
    failedBatchItem_errorMessage,

    -- ** Filter
    filter_contextRowId,
    filter_formula,

    -- ** ImportDataSource
    importDataSource_dataSourceConfig,

    -- ** ImportDataSourceConfig
    importDataSourceConfig_dataSourceUrl,

    -- ** ImportJobSubmitter
    importJobSubmitter_email,
    importJobSubmitter_userArn,

    -- ** ImportOptions
    importOptions_delimitedTextOptions,
    importOptions_destinationOptions,

    -- ** ResultRow
    resultRow_rowId,
    resultRow_dataItems,

    -- ** ResultSet
    resultSet_headers,
    resultSet_rows,

    -- ** SourceDataColumnProperties
    sourceDataColumnProperties_columnIndex,

    -- ** Table
    table_tableId,
    table_tableName,

    -- ** TableColumn
    tableColumn_format,
    tableColumn_tableColumnName,
    tableColumn_tableColumnId,

    -- ** TableDataImportJobMetadata
    tableDataImportJobMetadata_submitter,
    tableDataImportJobMetadata_submitTime,
    tableDataImportJobMetadata_importOptions,
    tableDataImportJobMetadata_dataSource,

    -- ** TableRow
    tableRow_rowId,
    tableRow_cells,

    -- ** UpdateRowData
    updateRowData_rowId,
    updateRowData_cellsToUpdate,

    -- ** UpsertRowData
    upsertRowData_batchItemId,
    upsertRowData_filter,
    upsertRowData_cellsToUpdate,

    -- ** UpsertRowsResult
    upsertRowsResult_rowIds,
    upsertRowsResult_upsertAction,

    -- ** VariableValue
    variableValue_rawValue,
  )
where

import Network.AWS.HoneyCode.BatchCreateTableRows
import Network.AWS.HoneyCode.BatchDeleteTableRows
import Network.AWS.HoneyCode.BatchUpdateTableRows
import Network.AWS.HoneyCode.BatchUpsertTableRows
import Network.AWS.HoneyCode.DescribeTableDataImportJob
import Network.AWS.HoneyCode.GetScreenData
import Network.AWS.HoneyCode.InvokeScreenAutomation
import Network.AWS.HoneyCode.ListTableColumns
import Network.AWS.HoneyCode.ListTableRows
import Network.AWS.HoneyCode.ListTables
import Network.AWS.HoneyCode.QueryTableRows
import Network.AWS.HoneyCode.StartTableDataImportJob
import Network.AWS.HoneyCode.Types.Cell
import Network.AWS.HoneyCode.Types.CellInput
import Network.AWS.HoneyCode.Types.ColumnMetadata
import Network.AWS.HoneyCode.Types.CreateRowData
import Network.AWS.HoneyCode.Types.DataItem
import Network.AWS.HoneyCode.Types.DelimitedTextImportOptions
import Network.AWS.HoneyCode.Types.DestinationOptions
import Network.AWS.HoneyCode.Types.FailedBatchItem
import Network.AWS.HoneyCode.Types.Filter
import Network.AWS.HoneyCode.Types.ImportDataSource
import Network.AWS.HoneyCode.Types.ImportDataSourceConfig
import Network.AWS.HoneyCode.Types.ImportJobSubmitter
import Network.AWS.HoneyCode.Types.ImportOptions
import Network.AWS.HoneyCode.Types.ResultRow
import Network.AWS.HoneyCode.Types.ResultSet
import Network.AWS.HoneyCode.Types.SourceDataColumnProperties
import Network.AWS.HoneyCode.Types.Table
import Network.AWS.HoneyCode.Types.TableColumn
import Network.AWS.HoneyCode.Types.TableDataImportJobMetadata
import Network.AWS.HoneyCode.Types.TableRow
import Network.AWS.HoneyCode.Types.UpdateRowData
import Network.AWS.HoneyCode.Types.UpsertRowData
import Network.AWS.HoneyCode.Types.UpsertRowsResult
import Network.AWS.HoneyCode.Types.VariableValue

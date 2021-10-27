{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.HoneyCode.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.HoneyCode.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _AutomationExecutionTimeoutException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _AutomationExecutionException,
    _RequestTimeoutException,
    _InternalServerException,
    _ServiceUnavailableException,
    _ResourceNotFoundException,

    -- * Format
    Format (..),

    -- * ImportDataCharacterEncoding
    ImportDataCharacterEncoding (..),

    -- * ImportSourceDataFormat
    ImportSourceDataFormat (..),

    -- * TableDataImportJobStatus
    TableDataImportJobStatus (..),

    -- * UpsertAction
    UpsertAction (..),

    -- * Cell
    Cell (..),
    newCell,
    cell_rawValue,
    cell_format,
    cell_formula,
    cell_formattedValue,

    -- * CellInput
    CellInput (..),
    newCellInput,
    cellInput_fact,

    -- * ColumnMetadata
    ColumnMetadata (..),
    newColumnMetadata,
    columnMetadata_name,
    columnMetadata_format,

    -- * CreateRowData
    CreateRowData (..),
    newCreateRowData,
    createRowData_batchItemId,
    createRowData_cellsToCreate,

    -- * DataItem
    DataItem (..),
    newDataItem,
    dataItem_rawValue,
    dataItem_overrideFormat,
    dataItem_formattedValue,

    -- * DelimitedTextImportOptions
    DelimitedTextImportOptions (..),
    newDelimitedTextImportOptions,
    delimitedTextImportOptions_ignoreEmptyRows,
    delimitedTextImportOptions_hasHeaderRow,
    delimitedTextImportOptions_dataCharacterEncoding,
    delimitedTextImportOptions_delimiter,

    -- * DestinationOptions
    DestinationOptions (..),
    newDestinationOptions,
    destinationOptions_columnMap,

    -- * FailedBatchItem
    FailedBatchItem (..),
    newFailedBatchItem,
    failedBatchItem_id,
    failedBatchItem_errorMessage,

    -- * Filter
    Filter (..),
    newFilter,
    filter_contextRowId,
    filter_formula,

    -- * ImportDataSource
    ImportDataSource (..),
    newImportDataSource,
    importDataSource_dataSourceConfig,

    -- * ImportDataSourceConfig
    ImportDataSourceConfig (..),
    newImportDataSourceConfig,
    importDataSourceConfig_dataSourceUrl,

    -- * ImportJobSubmitter
    ImportJobSubmitter (..),
    newImportJobSubmitter,
    importJobSubmitter_email,
    importJobSubmitter_userArn,

    -- * ImportOptions
    ImportOptions (..),
    newImportOptions,
    importOptions_delimitedTextOptions,
    importOptions_destinationOptions,

    -- * ResultRow
    ResultRow (..),
    newResultRow,
    resultRow_rowId,
    resultRow_dataItems,

    -- * ResultSet
    ResultSet (..),
    newResultSet,
    resultSet_headers,
    resultSet_rows,

    -- * SourceDataColumnProperties
    SourceDataColumnProperties (..),
    newSourceDataColumnProperties,
    sourceDataColumnProperties_columnIndex,

    -- * Table
    Table (..),
    newTable,
    table_tableId,
    table_tableName,

    -- * TableColumn
    TableColumn (..),
    newTableColumn,
    tableColumn_format,
    tableColumn_tableColumnName,
    tableColumn_tableColumnId,

    -- * TableDataImportJobMetadata
    TableDataImportJobMetadata (..),
    newTableDataImportJobMetadata,
    tableDataImportJobMetadata_submitter,
    tableDataImportJobMetadata_submitTime,
    tableDataImportJobMetadata_importOptions,
    tableDataImportJobMetadata_dataSource,

    -- * TableRow
    TableRow (..),
    newTableRow,
    tableRow_rowId,
    tableRow_cells,

    -- * UpdateRowData
    UpdateRowData (..),
    newUpdateRowData,
    updateRowData_rowId,
    updateRowData_cellsToUpdate,

    -- * UpsertRowData
    UpsertRowData (..),
    newUpsertRowData,
    upsertRowData_batchItemId,
    upsertRowData_filter,
    upsertRowData_cellsToUpdate,

    -- * UpsertRowsResult
    UpsertRowsResult (..),
    newUpsertRowsResult,
    upsertRowsResult_rowIds,
    upsertRowsResult_upsertAction,

    -- * VariableValue
    VariableValue (..),
    newVariableValue,
    variableValue_rawValue,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.HoneyCode.Types.Cell
import Network.AWS.HoneyCode.Types.CellInput
import Network.AWS.HoneyCode.Types.ColumnMetadata
import Network.AWS.HoneyCode.Types.CreateRowData
import Network.AWS.HoneyCode.Types.DataItem
import Network.AWS.HoneyCode.Types.DelimitedTextImportOptions
import Network.AWS.HoneyCode.Types.DestinationOptions
import Network.AWS.HoneyCode.Types.FailedBatchItem
import Network.AWS.HoneyCode.Types.Filter
import Network.AWS.HoneyCode.Types.Format
import Network.AWS.HoneyCode.Types.ImportDataCharacterEncoding
import Network.AWS.HoneyCode.Types.ImportDataSource
import Network.AWS.HoneyCode.Types.ImportDataSourceConfig
import Network.AWS.HoneyCode.Types.ImportJobSubmitter
import Network.AWS.HoneyCode.Types.ImportOptions
import Network.AWS.HoneyCode.Types.ImportSourceDataFormat
import Network.AWS.HoneyCode.Types.ResultRow
import Network.AWS.HoneyCode.Types.ResultSet
import Network.AWS.HoneyCode.Types.SourceDataColumnProperties
import Network.AWS.HoneyCode.Types.Table
import Network.AWS.HoneyCode.Types.TableColumn
import Network.AWS.HoneyCode.Types.TableDataImportJobMetadata
import Network.AWS.HoneyCode.Types.TableDataImportJobStatus
import Network.AWS.HoneyCode.Types.TableRow
import Network.AWS.HoneyCode.Types.UpdateRowData
import Network.AWS.HoneyCode.Types.UpsertAction
import Network.AWS.HoneyCode.Types.UpsertRowData
import Network.AWS.HoneyCode.Types.UpsertRowsResult
import Network.AWS.HoneyCode.Types.VariableValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2020-03-01@ of the Amazon Honeycode SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "HoneyCode",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "honeycode",
      Core._serviceSigningName = "honeycode",
      Core._serviceVersion = "2020-03-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "HoneyCode",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Request is invalid. The message in the response contains details on why
-- the request is invalid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | You do not have sufficient access to perform this action. Check that the
-- workbook is owned by you and your IAM policy allows access to the
-- resource in the request.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The automation execution timed out.
_AutomationExecutionTimeoutException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationExecutionTimeoutException =
  Core._MatchServiceError
    defaultService
    "AutomationExecutionTimeoutException"
    Prelude.. Core.hasStatus 504

-- | The request caused service quota to be breached.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Tps(transactions per second) rate reached.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The automation execution did not end successfully.
_AutomationExecutionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AutomationExecutionException =
  Core._MatchServiceError
    defaultService
    "AutomationExecutionException"
    Prelude.. Core.hasStatus 400

-- | The request timed out.
_RequestTimeoutException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RequestTimeoutException =
  Core._MatchServiceError
    defaultService
    "RequestTimeoutException"
    Prelude.. Core.hasStatus 504

-- | There were unexpected errors from the server.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | Remote service is unreachable.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | A Workbook, Table, App, Screen or Screen Automation was not found with
-- the given ID.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

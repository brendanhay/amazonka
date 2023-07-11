{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RDSData.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDSData.Lens
  ( -- * Operations

    -- ** BatchExecuteStatement
    batchExecuteStatement_database,
    batchExecuteStatement_parameterSets,
    batchExecuteStatement_schema,
    batchExecuteStatement_transactionId,
    batchExecuteStatement_resourceArn,
    batchExecuteStatement_secretArn,
    batchExecuteStatement_sql,
    batchExecuteStatementResponse_updateResults,
    batchExecuteStatementResponse_httpStatus,

    -- ** BeginTransaction
    beginTransaction_database,
    beginTransaction_schema,
    beginTransaction_resourceArn,
    beginTransaction_secretArn,
    beginTransactionResponse_transactionId,
    beginTransactionResponse_httpStatus,

    -- ** CommitTransaction
    commitTransaction_resourceArn,
    commitTransaction_secretArn,
    commitTransaction_transactionId,
    commitTransactionResponse_transactionStatus,
    commitTransactionResponse_httpStatus,

    -- ** ExecuteStatement
    executeStatement_continueAfterTimeout,
    executeStatement_database,
    executeStatement_formatRecordsAs,
    executeStatement_includeResultMetadata,
    executeStatement_parameters,
    executeStatement_resultSetOptions,
    executeStatement_schema,
    executeStatement_transactionId,
    executeStatement_resourceArn,
    executeStatement_secretArn,
    executeStatement_sql,
    executeStatementResponse_columnMetadata,
    executeStatementResponse_formattedRecords,
    executeStatementResponse_generatedFields,
    executeStatementResponse_numberOfRecordsUpdated,
    executeStatementResponse_records,
    executeStatementResponse_httpStatus,

    -- ** RollbackTransaction
    rollbackTransaction_resourceArn,
    rollbackTransaction_secretArn,
    rollbackTransaction_transactionId,
    rollbackTransactionResponse_transactionStatus,
    rollbackTransactionResponse_httpStatus,

    -- * Types

    -- ** ArrayValue
    arrayValue_arrayValues,
    arrayValue_booleanValues,
    arrayValue_doubleValues,
    arrayValue_longValues,
    arrayValue_stringValues,

    -- ** ColumnMetadata
    columnMetadata_arrayBaseColumnType,
    columnMetadata_isAutoIncrement,
    columnMetadata_isCaseSensitive,
    columnMetadata_isCurrency,
    columnMetadata_isSigned,
    columnMetadata_label,
    columnMetadata_name,
    columnMetadata_nullable,
    columnMetadata_precision,
    columnMetadata_scale,
    columnMetadata_schemaName,
    columnMetadata_tableName,
    columnMetadata_type,
    columnMetadata_typeName,

    -- ** Field
    field_arrayValue,
    field_blobValue,
    field_booleanValue,
    field_doubleValue,
    field_isNull,
    field_longValue,
    field_stringValue,

    -- ** ResultSetOptions
    resultSetOptions_decimalReturnType,
    resultSetOptions_longReturnType,

    -- ** SqlParameter
    sqlParameter_name,
    sqlParameter_typeHint,
    sqlParameter_value,

    -- ** UpdateResult
    updateResult_generatedFields,
  )
where

import Amazonka.RDSData.BatchExecuteStatement
import Amazonka.RDSData.BeginTransaction
import Amazonka.RDSData.CommitTransaction
import Amazonka.RDSData.ExecuteStatement
import Amazonka.RDSData.RollbackTransaction
import Amazonka.RDSData.Types.ArrayValue
import Amazonka.RDSData.Types.ColumnMetadata
import Amazonka.RDSData.Types.Field
import Amazonka.RDSData.Types.ResultSetOptions
import Amazonka.RDSData.Types.SqlParameter
import Amazonka.RDSData.Types.UpdateResult

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RDSData.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDSData.Lens
  ( -- * Operations

    -- ** BatchExecuteStatement
    batchExecuteStatement_parameterSets,
    batchExecuteStatement_database,
    batchExecuteStatement_transactionId,
    batchExecuteStatement_schema,
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
    executeStatement_resultSetOptions,
    executeStatement_includeResultMetadata,
    executeStatement_transactionId,
    executeStatement_schema,
    executeStatement_parameters,
    executeStatement_resourceArn,
    executeStatement_secretArn,
    executeStatement_sql,
    executeStatementResponse_records,
    executeStatementResponse_columnMetadata,
    executeStatementResponse_numberOfRecordsUpdated,
    executeStatementResponse_generatedFields,
    executeStatementResponse_httpStatus,

    -- ** RollbackTransaction
    rollbackTransaction_resourceArn,
    rollbackTransaction_secretArn,
    rollbackTransaction_transactionId,
    rollbackTransactionResponse_transactionStatus,
    rollbackTransactionResponse_httpStatus,

    -- * Types

    -- ** ArrayValue
    arrayValue_stringValues,
    arrayValue_booleanValues,
    arrayValue_longValues,
    arrayValue_doubleValues,
    arrayValue_arrayValues,

    -- ** ColumnMetadata
    columnMetadata_tableName,
    columnMetadata_name,
    columnMetadata_type,
    columnMetadata_label,
    columnMetadata_schemaName,
    columnMetadata_nullable,
    columnMetadata_isCaseSensitive,
    columnMetadata_typeName,
    columnMetadata_arrayBaseColumnType,
    columnMetadata_precision,
    columnMetadata_scale,
    columnMetadata_isAutoIncrement,
    columnMetadata_isCurrency,
    columnMetadata_isSigned,

    -- ** Field
    field_doubleValue,
    field_booleanValue,
    field_isNull,
    field_stringValue,
    field_longValue,
    field_blobValue,
    field_arrayValue,

    -- ** ResultSetOptions
    resultSetOptions_decimalReturnType,

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

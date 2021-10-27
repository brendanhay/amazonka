{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDSData.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDSData.Lens
  ( -- * Operations

    -- ** RollbackTransaction
    rollbackTransaction_resourceArn,
    rollbackTransaction_secretArn,
    rollbackTransaction_transactionId,
    rollbackTransactionResponse_transactionStatus,
    rollbackTransactionResponse_httpStatus,

    -- ** BeginTransaction
    beginTransaction_database,
    beginTransaction_schema,
    beginTransaction_resourceArn,
    beginTransaction_secretArn,
    beginTransactionResponse_transactionId,
    beginTransactionResponse_httpStatus,

    -- ** BatchExecuteStatement
    batchExecuteStatement_database,
    batchExecuteStatement_parameterSets,
    batchExecuteStatement_transactionId,
    batchExecuteStatement_schema,
    batchExecuteStatement_resourceArn,
    batchExecuteStatement_secretArn,
    batchExecuteStatement_sql,
    batchExecuteStatementResponse_updateResults,
    batchExecuteStatementResponse_httpStatus,

    -- ** ExecuteStatement
    executeStatement_database,
    executeStatement_transactionId,
    executeStatement_schema,
    executeStatement_parameters,
    executeStatement_includeResultMetadata,
    executeStatement_resultSetOptions,
    executeStatement_continueAfterTimeout,
    executeStatement_resourceArn,
    executeStatement_secretArn,
    executeStatement_sql,
    executeStatementResponse_records,
    executeStatementResponse_columnMetadata,
    executeStatementResponse_generatedFields,
    executeStatementResponse_numberOfRecordsUpdated,
    executeStatementResponse_httpStatus,

    -- ** CommitTransaction
    commitTransaction_resourceArn,
    commitTransaction_secretArn,
    commitTransaction_transactionId,
    commitTransactionResponse_transactionStatus,
    commitTransactionResponse_httpStatus,

    -- * Types

    -- ** ArrayValue
    arrayValue_longValues,
    arrayValue_doubleValues,
    arrayValue_stringValues,
    arrayValue_arrayValues,
    arrayValue_booleanValues,

    -- ** ColumnMetadata
    columnMetadata_typeName,
    columnMetadata_isCaseSensitive,
    columnMetadata_isCurrency,
    columnMetadata_scale,
    columnMetadata_precision,
    columnMetadata_schemaName,
    columnMetadata_isAutoIncrement,
    columnMetadata_name,
    columnMetadata_arrayBaseColumnType,
    columnMetadata_type,
    columnMetadata_isSigned,
    columnMetadata_label,
    columnMetadata_nullable,
    columnMetadata_tableName,

    -- ** Field
    field_doubleValue,
    field_stringValue,
    field_longValue,
    field_booleanValue,
    field_arrayValue,
    field_blobValue,
    field_isNull,

    -- ** ResultSetOptions
    resultSetOptions_decimalReturnType,

    -- ** SqlParameter
    sqlParameter_value,
    sqlParameter_name,
    sqlParameter_typeHint,

    -- ** UpdateResult
    updateResult_generatedFields,
  )
where

import Network.AWS.RDSData.BatchExecuteStatement
import Network.AWS.RDSData.BeginTransaction
import Network.AWS.RDSData.CommitTransaction
import Network.AWS.RDSData.ExecuteStatement
import Network.AWS.RDSData.RollbackTransaction
import Network.AWS.RDSData.Types.ArrayValue
import Network.AWS.RDSData.Types.ColumnMetadata
import Network.AWS.RDSData.Types.Field
import Network.AWS.RDSData.Types.ResultSetOptions
import Network.AWS.RDSData.Types.SqlParameter
import Network.AWS.RDSData.Types.UpdateResult

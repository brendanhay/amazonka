{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RedshiftData.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftData.Lens
  ( -- * Operations

    -- ** BatchExecuteStatement
    batchExecuteStatement_secretArn,
    batchExecuteStatement_statementName,
    batchExecuteStatement_withEvent,
    batchExecuteStatement_dbUser,
    batchExecuteStatement_clusterIdentifier,
    batchExecuteStatement_database,
    batchExecuteStatement_sqls,
    batchExecuteStatementResponse_clusterIdentifier,
    batchExecuteStatementResponse_id,
    batchExecuteStatementResponse_database,
    batchExecuteStatementResponse_secretArn,
    batchExecuteStatementResponse_dbUser,
    batchExecuteStatementResponse_createdAt,
    batchExecuteStatementResponse_httpStatus,

    -- ** CancelStatement
    cancelStatement_id,
    cancelStatementResponse_status,
    cancelStatementResponse_httpStatus,

    -- ** DescribeStatement
    describeStatement_id,
    describeStatementResponse_clusterIdentifier,
    describeStatementResponse_redshiftQueryId,
    describeStatementResponse_resultRows,
    describeStatementResponse_status,
    describeStatementResponse_queryParameters,
    describeStatementResponse_hasResultSet,
    describeStatementResponse_database,
    describeStatementResponse_duration,
    describeStatementResponse_resultSize,
    describeStatementResponse_secretArn,
    describeStatementResponse_subStatements,
    describeStatementResponse_redshiftPid,
    describeStatementResponse_queryString,
    describeStatementResponse_error,
    describeStatementResponse_dbUser,
    describeStatementResponse_createdAt,
    describeStatementResponse_updatedAt,
    describeStatementResponse_httpStatus,
    describeStatementResponse_id,

    -- ** DescribeTable
    describeTable_nextToken,
    describeTable_connectedDatabase,
    describeTable_maxResults,
    describeTable_secretArn,
    describeTable_schema,
    describeTable_table,
    describeTable_dbUser,
    describeTable_clusterIdentifier,
    describeTable_database,
    describeTableResponse_tableName,
    describeTableResponse_nextToken,
    describeTableResponse_columnList,
    describeTableResponse_httpStatus,

    -- ** ExecuteStatement
    executeStatement_secretArn,
    executeStatement_statementName,
    executeStatement_withEvent,
    executeStatement_dbUser,
    executeStatement_parameters,
    executeStatement_clusterIdentifier,
    executeStatement_database,
    executeStatement_sql,
    executeStatementResponse_clusterIdentifier,
    executeStatementResponse_id,
    executeStatementResponse_database,
    executeStatementResponse_secretArn,
    executeStatementResponse_dbUser,
    executeStatementResponse_createdAt,
    executeStatementResponse_httpStatus,

    -- ** GetStatementResult
    getStatementResult_nextToken,
    getStatementResult_id,
    getStatementResultResponse_nextToken,
    getStatementResultResponse_columnMetadata,
    getStatementResultResponse_totalNumRows,
    getStatementResultResponse_httpStatus,
    getStatementResultResponse_records,

    -- ** ListDatabases
    listDatabases_nextToken,
    listDatabases_maxResults,
    listDatabases_secretArn,
    listDatabases_dbUser,
    listDatabases_clusterIdentifier,
    listDatabases_database,
    listDatabasesResponse_nextToken,
    listDatabasesResponse_databases,
    listDatabasesResponse_httpStatus,

    -- ** ListSchemas
    listSchemas_nextToken,
    listSchemas_connectedDatabase,
    listSchemas_maxResults,
    listSchemas_secretArn,
    listSchemas_dbUser,
    listSchemas_schemaPattern,
    listSchemas_clusterIdentifier,
    listSchemas_database,
    listSchemasResponse_nextToken,
    listSchemasResponse_schemas,
    listSchemasResponse_httpStatus,

    -- ** ListStatements
    listStatements_roleLevel,
    listStatements_nextToken,
    listStatements_status,
    listStatements_maxResults,
    listStatements_statementName,
    listStatementsResponse_nextToken,
    listStatementsResponse_httpStatus,
    listStatementsResponse_statements,

    -- ** ListTables
    listTables_nextToken,
    listTables_tablePattern,
    listTables_connectedDatabase,
    listTables_maxResults,
    listTables_secretArn,
    listTables_dbUser,
    listTables_schemaPattern,
    listTables_clusterIdentifier,
    listTables_database,
    listTablesResponse_tables,
    listTablesResponse_nextToken,
    listTablesResponse_httpStatus,

    -- * Types

    -- ** ColumnMetadata
    columnMetadata_tableName,
    columnMetadata_name,
    columnMetadata_label,
    columnMetadata_schemaName,
    columnMetadata_nullable,
    columnMetadata_length,
    columnMetadata_isCaseSensitive,
    columnMetadata_typeName,
    columnMetadata_columnDefault,
    columnMetadata_precision,
    columnMetadata_scale,
    columnMetadata_isCurrency,
    columnMetadata_isSigned,

    -- ** Field
    field_doubleValue,
    field_booleanValue,
    field_isNull,
    field_stringValue,
    field_longValue,
    field_blobValue,

    -- ** SqlParameter
    sqlParameter_name,
    sqlParameter_value,

    -- ** StatementData
    statementData_isBatchStatement,
    statementData_status,
    statementData_queryStrings,
    statementData_queryParameters,
    statementData_secretArn,
    statementData_queryString,
    statementData_statementName,
    statementData_createdAt,
    statementData_updatedAt,
    statementData_id,

    -- ** SubStatementData
    subStatementData_redshiftQueryId,
    subStatementData_resultRows,
    subStatementData_status,
    subStatementData_hasResultSet,
    subStatementData_duration,
    subStatementData_resultSize,
    subStatementData_queryString,
    subStatementData_error,
    subStatementData_createdAt,
    subStatementData_updatedAt,
    subStatementData_id,

    -- ** TableMember
    tableMember_name,
    tableMember_type,
    tableMember_schema,
  )
where

import Amazonka.RedshiftData.BatchExecuteStatement
import Amazonka.RedshiftData.CancelStatement
import Amazonka.RedshiftData.DescribeStatement
import Amazonka.RedshiftData.DescribeTable
import Amazonka.RedshiftData.ExecuteStatement
import Amazonka.RedshiftData.GetStatementResult
import Amazonka.RedshiftData.ListDatabases
import Amazonka.RedshiftData.ListSchemas
import Amazonka.RedshiftData.ListStatements
import Amazonka.RedshiftData.ListTables
import Amazonka.RedshiftData.Types.ColumnMetadata
import Amazonka.RedshiftData.Types.Field
import Amazonka.RedshiftData.Types.SqlParameter
import Amazonka.RedshiftData.Types.StatementData
import Amazonka.RedshiftData.Types.SubStatementData
import Amazonka.RedshiftData.Types.TableMember

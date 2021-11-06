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

    -- ** ListStatements
    listStatements_status,
    listStatements_nextToken,
    listStatements_statementName,
    listStatements_roleLevel,
    listStatements_maxResults,
    listStatementsResponse_nextToken,
    listStatementsResponse_httpStatus,
    listStatementsResponse_statements,

    -- ** ListDatabases
    listDatabases_dbUser,
    listDatabases_nextToken,
    listDatabases_secretArn,
    listDatabases_maxResults,
    listDatabases_clusterIdentifier,
    listDatabases_database,
    listDatabasesResponse_nextToken,
    listDatabasesResponse_databases,
    listDatabasesResponse_httpStatus,

    -- ** ListSchemas
    listSchemas_dbUser,
    listSchemas_connectedDatabase,
    listSchemas_nextToken,
    listSchemas_secretArn,
    listSchemas_maxResults,
    listSchemas_schemaPattern,
    listSchemas_clusterIdentifier,
    listSchemas_database,
    listSchemasResponse_schemas,
    listSchemasResponse_nextToken,
    listSchemasResponse_httpStatus,

    -- ** DescribeStatement
    describeStatement_id,
    describeStatementResponse_status,
    describeStatementResponse_redshiftQueryId,
    describeStatementResponse_resultSize,
    describeStatementResponse_dbUser,
    describeStatementResponse_subStatements,
    describeStatementResponse_database,
    describeStatementResponse_createdAt,
    describeStatementResponse_queryParameters,
    describeStatementResponse_error,
    describeStatementResponse_resultRows,
    describeStatementResponse_redshiftPid,
    describeStatementResponse_clusterIdentifier,
    describeStatementResponse_hasResultSet,
    describeStatementResponse_queryString,
    describeStatementResponse_updatedAt,
    describeStatementResponse_secretArn,
    describeStatementResponse_duration,
    describeStatementResponse_httpStatus,
    describeStatementResponse_id,

    -- ** CancelStatement
    cancelStatement_id,
    cancelStatementResponse_status,
    cancelStatementResponse_httpStatus,

    -- ** DescribeTable
    describeTable_dbUser,
    describeTable_connectedDatabase,
    describeTable_schema,
    describeTable_nextToken,
    describeTable_secretArn,
    describeTable_table,
    describeTable_maxResults,
    describeTable_clusterIdentifier,
    describeTable_database,
    describeTableResponse_columnList,
    describeTableResponse_nextToken,
    describeTableResponse_tableName,
    describeTableResponse_httpStatus,

    -- ** BatchExecuteStatement
    batchExecuteStatement_dbUser,
    batchExecuteStatement_statementName,
    batchExecuteStatement_secretArn,
    batchExecuteStatement_withEvent,
    batchExecuteStatement_clusterIdentifier,
    batchExecuteStatement_database,
    batchExecuteStatement_sqls,
    batchExecuteStatementResponse_dbUser,
    batchExecuteStatementResponse_database,
    batchExecuteStatementResponse_createdAt,
    batchExecuteStatementResponse_clusterIdentifier,
    batchExecuteStatementResponse_id,
    batchExecuteStatementResponse_secretArn,
    batchExecuteStatementResponse_httpStatus,

    -- ** ListTables
    listTables_dbUser,
    listTables_connectedDatabase,
    listTables_nextToken,
    listTables_secretArn,
    listTables_tablePattern,
    listTables_maxResults,
    listTables_schemaPattern,
    listTables_clusterIdentifier,
    listTables_database,
    listTablesResponse_nextToken,
    listTablesResponse_tables,
    listTablesResponse_httpStatus,

    -- ** ExecuteStatement
    executeStatement_dbUser,
    executeStatement_statementName,
    executeStatement_parameters,
    executeStatement_secretArn,
    executeStatement_withEvent,
    executeStatement_clusterIdentifier,
    executeStatement_database,
    executeStatement_sql,
    executeStatementResponse_dbUser,
    executeStatementResponse_database,
    executeStatementResponse_createdAt,
    executeStatementResponse_clusterIdentifier,
    executeStatementResponse_id,
    executeStatementResponse_secretArn,
    executeStatementResponse_httpStatus,

    -- ** GetStatementResult
    getStatementResult_nextToken,
    getStatementResult_id,
    getStatementResultResponse_totalNumRows,
    getStatementResultResponse_nextToken,
    getStatementResultResponse_columnMetadata,
    getStatementResultResponse_httpStatus,
    getStatementResultResponse_records,

    -- * Types

    -- ** ColumnMetadata
    columnMetadata_length,
    columnMetadata_typeName,
    columnMetadata_isCaseSensitive,
    columnMetadata_columnDefault,
    columnMetadata_isCurrency,
    columnMetadata_scale,
    columnMetadata_precision,
    columnMetadata_schemaName,
    columnMetadata_name,
    columnMetadata_isSigned,
    columnMetadata_label,
    columnMetadata_nullable,
    columnMetadata_tableName,

    -- ** Field
    field_doubleValue,
    field_stringValue,
    field_longValue,
    field_booleanValue,
    field_blobValue,
    field_isNull,

    -- ** SqlParameter
    sqlParameter_name,
    sqlParameter_value,

    -- ** StatementData
    statementData_status,
    statementData_createdAt,
    statementData_queryParameters,
    statementData_queryStrings,
    statementData_queryString,
    statementData_statementName,
    statementData_updatedAt,
    statementData_secretArn,
    statementData_isBatchStatement,
    statementData_id,

    -- ** SubStatementData
    subStatementData_status,
    subStatementData_redshiftQueryId,
    subStatementData_resultSize,
    subStatementData_createdAt,
    subStatementData_error,
    subStatementData_resultRows,
    subStatementData_hasResultSet,
    subStatementData_queryString,
    subStatementData_updatedAt,
    subStatementData_duration,
    subStatementData_id,

    -- ** TableMember
    tableMember_schema,
    tableMember_name,
    tableMember_type,
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

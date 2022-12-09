{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RedshiftData.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftData.Lens
  ( -- * Operations

    -- ** BatchExecuteStatement
    batchExecuteStatement_clusterIdentifier,
    batchExecuteStatement_dbUser,
    batchExecuteStatement_secretArn,
    batchExecuteStatement_statementName,
    batchExecuteStatement_withEvent,
    batchExecuteStatement_workgroupName,
    batchExecuteStatement_database,
    batchExecuteStatement_sqls,
    batchExecuteStatementResponse_clusterIdentifier,
    batchExecuteStatementResponse_createdAt,
    batchExecuteStatementResponse_database,
    batchExecuteStatementResponse_dbUser,
    batchExecuteStatementResponse_id,
    batchExecuteStatementResponse_secretArn,
    batchExecuteStatementResponse_workgroupName,
    batchExecuteStatementResponse_httpStatus,

    -- ** CancelStatement
    cancelStatement_id,
    cancelStatementResponse_status,
    cancelStatementResponse_httpStatus,

    -- ** DescribeStatement
    describeStatement_id,
    describeStatementResponse_clusterIdentifier,
    describeStatementResponse_createdAt,
    describeStatementResponse_database,
    describeStatementResponse_dbUser,
    describeStatementResponse_duration,
    describeStatementResponse_error,
    describeStatementResponse_hasResultSet,
    describeStatementResponse_queryParameters,
    describeStatementResponse_queryString,
    describeStatementResponse_redshiftPid,
    describeStatementResponse_redshiftQueryId,
    describeStatementResponse_resultRows,
    describeStatementResponse_resultSize,
    describeStatementResponse_secretArn,
    describeStatementResponse_status,
    describeStatementResponse_subStatements,
    describeStatementResponse_updatedAt,
    describeStatementResponse_workgroupName,
    describeStatementResponse_httpStatus,
    describeStatementResponse_id,

    -- ** DescribeTable
    describeTable_clusterIdentifier,
    describeTable_connectedDatabase,
    describeTable_dbUser,
    describeTable_maxResults,
    describeTable_nextToken,
    describeTable_schema,
    describeTable_secretArn,
    describeTable_table,
    describeTable_workgroupName,
    describeTable_database,
    describeTableResponse_columnList,
    describeTableResponse_nextToken,
    describeTableResponse_tableName,
    describeTableResponse_httpStatus,

    -- ** ExecuteStatement
    executeStatement_clusterIdentifier,
    executeStatement_dbUser,
    executeStatement_parameters,
    executeStatement_secretArn,
    executeStatement_statementName,
    executeStatement_withEvent,
    executeStatement_workgroupName,
    executeStatement_database,
    executeStatement_sql,
    executeStatementResponse_clusterIdentifier,
    executeStatementResponse_createdAt,
    executeStatementResponse_database,
    executeStatementResponse_dbUser,
    executeStatementResponse_id,
    executeStatementResponse_secretArn,
    executeStatementResponse_workgroupName,
    executeStatementResponse_httpStatus,

    -- ** GetStatementResult
    getStatementResult_nextToken,
    getStatementResult_id,
    getStatementResultResponse_columnMetadata,
    getStatementResultResponse_nextToken,
    getStatementResultResponse_totalNumRows,
    getStatementResultResponse_httpStatus,
    getStatementResultResponse_records,

    -- ** ListDatabases
    listDatabases_clusterIdentifier,
    listDatabases_dbUser,
    listDatabases_maxResults,
    listDatabases_nextToken,
    listDatabases_secretArn,
    listDatabases_workgroupName,
    listDatabases_database,
    listDatabasesResponse_databases,
    listDatabasesResponse_nextToken,
    listDatabasesResponse_httpStatus,

    -- ** ListSchemas
    listSchemas_clusterIdentifier,
    listSchemas_connectedDatabase,
    listSchemas_dbUser,
    listSchemas_maxResults,
    listSchemas_nextToken,
    listSchemas_schemaPattern,
    listSchemas_secretArn,
    listSchemas_workgroupName,
    listSchemas_database,
    listSchemasResponse_nextToken,
    listSchemasResponse_schemas,
    listSchemasResponse_httpStatus,

    -- ** ListStatements
    listStatements_maxResults,
    listStatements_nextToken,
    listStatements_roleLevel,
    listStatements_statementName,
    listStatements_status,
    listStatementsResponse_nextToken,
    listStatementsResponse_httpStatus,
    listStatementsResponse_statements,

    -- ** ListTables
    listTables_clusterIdentifier,
    listTables_connectedDatabase,
    listTables_dbUser,
    listTables_maxResults,
    listTables_nextToken,
    listTables_schemaPattern,
    listTables_secretArn,
    listTables_tablePattern,
    listTables_workgroupName,
    listTables_database,
    listTablesResponse_nextToken,
    listTablesResponse_tables,
    listTablesResponse_httpStatus,

    -- * Types

    -- ** ColumnMetadata
    columnMetadata_columnDefault,
    columnMetadata_isCaseSensitive,
    columnMetadata_isCurrency,
    columnMetadata_isSigned,
    columnMetadata_label,
    columnMetadata_length,
    columnMetadata_name,
    columnMetadata_nullable,
    columnMetadata_precision,
    columnMetadata_scale,
    columnMetadata_schemaName,
    columnMetadata_tableName,
    columnMetadata_typeName,

    -- ** Field
    field_blobValue,
    field_booleanValue,
    field_doubleValue,
    field_isNull,
    field_longValue,
    field_stringValue,

    -- ** SqlParameter
    sqlParameter_name,
    sqlParameter_value,

    -- ** StatementData
    statementData_createdAt,
    statementData_isBatchStatement,
    statementData_queryParameters,
    statementData_queryString,
    statementData_queryStrings,
    statementData_secretArn,
    statementData_statementName,
    statementData_status,
    statementData_updatedAt,
    statementData_id,

    -- ** SubStatementData
    subStatementData_createdAt,
    subStatementData_duration,
    subStatementData_error,
    subStatementData_hasResultSet,
    subStatementData_queryString,
    subStatementData_redshiftQueryId,
    subStatementData_resultRows,
    subStatementData_resultSize,
    subStatementData_status,
    subStatementData_updatedAt,
    subStatementData_id,

    -- ** TableMember
    tableMember_name,
    tableMember_schema,
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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.QueryExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.QueryExecution where

import Network.AWS.Athena.Types.QueryExecutionContext
import Network.AWS.Athena.Types.QueryExecutionStatistics
import Network.AWS.Athena.Types.QueryExecutionStatus
import Network.AWS.Athena.Types.ResultConfiguration
import Network.AWS.Athena.Types.StatementType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a single instance of a query execution.
--
--
--
-- /See:/ 'queryExecution' smart constructor.
data QueryExecution = QueryExecution'
  { _qeStatus ::
      !(Maybe QueryExecutionStatus),
    _qeQueryExecutionContext :: !(Maybe QueryExecutionContext),
    _qeResultConfiguration :: !(Maybe ResultConfiguration),
    _qeQuery :: !(Maybe Text),
    _qeStatementType :: !(Maybe StatementType),
    _qeStatistics :: !(Maybe QueryExecutionStatistics),
    _qeQueryExecutionId :: !(Maybe Text),
    _qeWorkGroup :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QueryExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qeStatus' - The completion date, current state, submission time, and state change reason (if applicable) for the query execution.
--
-- * 'qeQueryExecutionContext' - The database in which the query execution occurred.
--
-- * 'qeResultConfiguration' - The location in Amazon S3 where query results were stored and the encryption option, if any, used for query results. These are known as "client-side settings". If workgroup settings override client-side settings, then the query uses the location for the query results and the encryption configuration that are specified for the workgroup.
--
-- * 'qeQuery' - The SQL query statements which the query execution ran.
--
-- * 'qeStatementType' - The type of query statement that was run. @DDL@ indicates DDL query statements. @DML@ indicates DML (Data Manipulation Language) query statements, such as @CREATE TABLE AS SELECT@ . @UTILITY@ indicates query statements other than DDL and DML, such as @SHOW CREATE TABLE@ , or @DESCRIBE <table>@ .
--
-- * 'qeStatistics' - Query execution statistics, such as the amount of data scanned, the amount of time that the query took to process, and the type of statement that was run.
--
-- * 'qeQueryExecutionId' - The unique identifier for each query execution.
--
-- * 'qeWorkGroup' - The name of the workgroup in which the query ran.
queryExecution ::
  QueryExecution
queryExecution =
  QueryExecution'
    { _qeStatus = Nothing,
      _qeQueryExecutionContext = Nothing,
      _qeResultConfiguration = Nothing,
      _qeQuery = Nothing,
      _qeStatementType = Nothing,
      _qeStatistics = Nothing,
      _qeQueryExecutionId = Nothing,
      _qeWorkGroup = Nothing
    }

-- | The completion date, current state, submission time, and state change reason (if applicable) for the query execution.
qeStatus :: Lens' QueryExecution (Maybe QueryExecutionStatus)
qeStatus = lens _qeStatus (\s a -> s {_qeStatus = a})

-- | The database in which the query execution occurred.
qeQueryExecutionContext :: Lens' QueryExecution (Maybe QueryExecutionContext)
qeQueryExecutionContext = lens _qeQueryExecutionContext (\s a -> s {_qeQueryExecutionContext = a})

-- | The location in Amazon S3 where query results were stored and the encryption option, if any, used for query results. These are known as "client-side settings". If workgroup settings override client-side settings, then the query uses the location for the query results and the encryption configuration that are specified for the workgroup.
qeResultConfiguration :: Lens' QueryExecution (Maybe ResultConfiguration)
qeResultConfiguration = lens _qeResultConfiguration (\s a -> s {_qeResultConfiguration = a})

-- | The SQL query statements which the query execution ran.
qeQuery :: Lens' QueryExecution (Maybe Text)
qeQuery = lens _qeQuery (\s a -> s {_qeQuery = a})

-- | The type of query statement that was run. @DDL@ indicates DDL query statements. @DML@ indicates DML (Data Manipulation Language) query statements, such as @CREATE TABLE AS SELECT@ . @UTILITY@ indicates query statements other than DDL and DML, such as @SHOW CREATE TABLE@ , or @DESCRIBE <table>@ .
qeStatementType :: Lens' QueryExecution (Maybe StatementType)
qeStatementType = lens _qeStatementType (\s a -> s {_qeStatementType = a})

-- | Query execution statistics, such as the amount of data scanned, the amount of time that the query took to process, and the type of statement that was run.
qeStatistics :: Lens' QueryExecution (Maybe QueryExecutionStatistics)
qeStatistics = lens _qeStatistics (\s a -> s {_qeStatistics = a})

-- | The unique identifier for each query execution.
qeQueryExecutionId :: Lens' QueryExecution (Maybe Text)
qeQueryExecutionId = lens _qeQueryExecutionId (\s a -> s {_qeQueryExecutionId = a})

-- | The name of the workgroup in which the query ran.
qeWorkGroup :: Lens' QueryExecution (Maybe Text)
qeWorkGroup = lens _qeWorkGroup (\s a -> s {_qeWorkGroup = a})

instance FromJSON QueryExecution where
  parseJSON =
    withObject
      "QueryExecution"
      ( \x ->
          QueryExecution'
            <$> (x .:? "Status")
            <*> (x .:? "QueryExecutionContext")
            <*> (x .:? "ResultConfiguration")
            <*> (x .:? "Query")
            <*> (x .:? "StatementType")
            <*> (x .:? "Statistics")
            <*> (x .:? "QueryExecutionId")
            <*> (x .:? "WorkGroup")
      )

instance Hashable QueryExecution

instance NFData QueryExecution

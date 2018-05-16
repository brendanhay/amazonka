{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Athena.Types.Product where

import Network.AWS.Athena.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the columns in a query execution result.
--
--
--
-- /See:/ 'columnInfo' smart constructor.
data ColumnInfo = ColumnInfo'
  { _ciScale         :: !(Maybe Int)
  , _ciPrecision     :: !(Maybe Int)
  , _ciSchemaName    :: !(Maybe Text)
  , _ciCatalogName   :: !(Maybe Text)
  , _ciCaseSensitive :: !(Maybe Bool)
  , _ciLabel         :: !(Maybe Text)
  , _ciTableName     :: !(Maybe Text)
  , _ciNullable      :: !(Maybe ColumnNullable)
  , _ciName          :: !Text
  , _ciType          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ColumnInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciScale' - For @DECIMAL@ data types, specifies the total number of digits in the fractional part of the value. Defaults to 0.
--
-- * 'ciPrecision' - For @DECIMAL@ data types, specifies the total number of digits, up to 38. For performance reasons, we recommend up to 18 digits.
--
-- * 'ciSchemaName' - The schema name (database name) to which the query results belong.
--
-- * 'ciCatalogName' - The catalog to which the query results belong.
--
-- * 'ciCaseSensitive' - Indicates whether values in the column are case-sensitive.
--
-- * 'ciLabel' - A column label.
--
-- * 'ciTableName' - The table name for the query results.
--
-- * 'ciNullable' - Indicates the column's nullable status.
--
-- * 'ciName' - The name of the column.
--
-- * 'ciType' - The data type of the column.
columnInfo
    :: Text -- ^ 'ciName'
    -> Text -- ^ 'ciType'
    -> ColumnInfo
columnInfo pName_ pType_ =
  ColumnInfo'
    { _ciScale = Nothing
    , _ciPrecision = Nothing
    , _ciSchemaName = Nothing
    , _ciCatalogName = Nothing
    , _ciCaseSensitive = Nothing
    , _ciLabel = Nothing
    , _ciTableName = Nothing
    , _ciNullable = Nothing
    , _ciName = pName_
    , _ciType = pType_
    }


-- | For @DECIMAL@ data types, specifies the total number of digits in the fractional part of the value. Defaults to 0.
ciScale :: Lens' ColumnInfo (Maybe Int)
ciScale = lens _ciScale (\ s a -> s{_ciScale = a})

-- | For @DECIMAL@ data types, specifies the total number of digits, up to 38. For performance reasons, we recommend up to 18 digits.
ciPrecision :: Lens' ColumnInfo (Maybe Int)
ciPrecision = lens _ciPrecision (\ s a -> s{_ciPrecision = a})

-- | The schema name (database name) to which the query results belong.
ciSchemaName :: Lens' ColumnInfo (Maybe Text)
ciSchemaName = lens _ciSchemaName (\ s a -> s{_ciSchemaName = a})

-- | The catalog to which the query results belong.
ciCatalogName :: Lens' ColumnInfo (Maybe Text)
ciCatalogName = lens _ciCatalogName (\ s a -> s{_ciCatalogName = a})

-- | Indicates whether values in the column are case-sensitive.
ciCaseSensitive :: Lens' ColumnInfo (Maybe Bool)
ciCaseSensitive = lens _ciCaseSensitive (\ s a -> s{_ciCaseSensitive = a})

-- | A column label.
ciLabel :: Lens' ColumnInfo (Maybe Text)
ciLabel = lens _ciLabel (\ s a -> s{_ciLabel = a})

-- | The table name for the query results.
ciTableName :: Lens' ColumnInfo (Maybe Text)
ciTableName = lens _ciTableName (\ s a -> s{_ciTableName = a})

-- | Indicates the column's nullable status.
ciNullable :: Lens' ColumnInfo (Maybe ColumnNullable)
ciNullable = lens _ciNullable (\ s a -> s{_ciNullable = a})

-- | The name of the column.
ciName :: Lens' ColumnInfo Text
ciName = lens _ciName (\ s a -> s{_ciName = a})

-- | The data type of the column.
ciType :: Lens' ColumnInfo Text
ciType = lens _ciType (\ s a -> s{_ciType = a})

instance FromJSON ColumnInfo where
        parseJSON
          = withObject "ColumnInfo"
              (\ x ->
                 ColumnInfo' <$>
                   (x .:? "Scale") <*> (x .:? "Precision") <*>
                     (x .:? "SchemaName")
                     <*> (x .:? "CatalogName")
                     <*> (x .:? "CaseSensitive")
                     <*> (x .:? "Label")
                     <*> (x .:? "TableName")
                     <*> (x .:? "Nullable")
                     <*> (x .: "Name")
                     <*> (x .: "Type"))

instance Hashable ColumnInfo where

instance NFData ColumnInfo where

-- | A piece of data (a field in the table).
--
--
--
-- /See:/ 'datum' smart constructor.
newtype Datum = Datum'
  { _dVarCharValue :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Datum' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dVarCharValue' - The value of the datum.
datum
    :: Datum
datum = Datum' {_dVarCharValue = Nothing}


-- | The value of the datum.
dVarCharValue :: Lens' Datum (Maybe Text)
dVarCharValue = lens _dVarCharValue (\ s a -> s{_dVarCharValue = a})

instance FromJSON Datum where
        parseJSON
          = withObject "Datum"
              (\ x -> Datum' <$> (x .:? "VarCharValue"))

instance Hashable Datum where

instance NFData Datum where

-- | If query results are encrypted in Amazon S3, indicates the Amazon S3 encryption option used.
--
--
--
-- /See:/ 'encryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { _ecKMSKey           :: !(Maybe Text)
  , _ecEncryptionOption :: !EncryptionOption
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EncryptionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecKMSKey' - For @SSE-KMS@ and @CSE-KMS@ , this is the KMS key ARN or ID.
--
-- * 'ecEncryptionOption' - Indicates whether Amazon S3 server-side encryption with Amazon S3-managed keys (@SSE-S3@ ), server-side encryption with KMS-managed keys (@SSE-KMS@ ), or client-side encryption with KMS-managed keys (CSE-KMS) is used.
encryptionConfiguration
    :: EncryptionOption -- ^ 'ecEncryptionOption'
    -> EncryptionConfiguration
encryptionConfiguration pEncryptionOption_ =
  EncryptionConfiguration'
    {_ecKMSKey = Nothing, _ecEncryptionOption = pEncryptionOption_}


-- | For @SSE-KMS@ and @CSE-KMS@ , this is the KMS key ARN or ID.
ecKMSKey :: Lens' EncryptionConfiguration (Maybe Text)
ecKMSKey = lens _ecKMSKey (\ s a -> s{_ecKMSKey = a})

-- | Indicates whether Amazon S3 server-side encryption with Amazon S3-managed keys (@SSE-S3@ ), server-side encryption with KMS-managed keys (@SSE-KMS@ ), or client-side encryption with KMS-managed keys (CSE-KMS) is used.
ecEncryptionOption :: Lens' EncryptionConfiguration EncryptionOption
ecEncryptionOption = lens _ecEncryptionOption (\ s a -> s{_ecEncryptionOption = a})

instance FromJSON EncryptionConfiguration where
        parseJSON
          = withObject "EncryptionConfiguration"
              (\ x ->
                 EncryptionConfiguration' <$>
                   (x .:? "KmsKey") <*> (x .: "EncryptionOption"))

instance Hashable EncryptionConfiguration where

instance NFData EncryptionConfiguration where

instance ToJSON EncryptionConfiguration where
        toJSON EncryptionConfiguration'{..}
          = object
              (catMaybes
                 [("KmsKey" .=) <$> _ecKMSKey,
                  Just ("EncryptionOption" .= _ecEncryptionOption)])

-- | A query, where @QueryString@ is the SQL query statements that comprise the query.
--
--
--
-- /See:/ 'namedQuery' smart constructor.
data NamedQuery = NamedQuery'
  { _nqNamedQueryId :: !(Maybe Text)
  , _nqDescription  :: !(Maybe Text)
  , _nqName         :: !Text
  , _nqDatabase     :: !Text
  , _nqQueryString  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NamedQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nqNamedQueryId' - The unique identifier of the query.
--
-- * 'nqDescription' - A brief description of the query.
--
-- * 'nqName' - The plain-language name of the query.
--
-- * 'nqDatabase' - The database to which the query belongs.
--
-- * 'nqQueryString' - The SQL query statements that comprise the query.
namedQuery
    :: Text -- ^ 'nqName'
    -> Text -- ^ 'nqDatabase'
    -> Text -- ^ 'nqQueryString'
    -> NamedQuery
namedQuery pName_ pDatabase_ pQueryString_ =
  NamedQuery'
    { _nqNamedQueryId = Nothing
    , _nqDescription = Nothing
    , _nqName = pName_
    , _nqDatabase = pDatabase_
    , _nqQueryString = pQueryString_
    }


-- | The unique identifier of the query.
nqNamedQueryId :: Lens' NamedQuery (Maybe Text)
nqNamedQueryId = lens _nqNamedQueryId (\ s a -> s{_nqNamedQueryId = a})

-- | A brief description of the query.
nqDescription :: Lens' NamedQuery (Maybe Text)
nqDescription = lens _nqDescription (\ s a -> s{_nqDescription = a})

-- | The plain-language name of the query.
nqName :: Lens' NamedQuery Text
nqName = lens _nqName (\ s a -> s{_nqName = a})

-- | The database to which the query belongs.
nqDatabase :: Lens' NamedQuery Text
nqDatabase = lens _nqDatabase (\ s a -> s{_nqDatabase = a})

-- | The SQL query statements that comprise the query.
nqQueryString :: Lens' NamedQuery Text
nqQueryString = lens _nqQueryString (\ s a -> s{_nqQueryString = a})

instance FromJSON NamedQuery where
        parseJSON
          = withObject "NamedQuery"
              (\ x ->
                 NamedQuery' <$>
                   (x .:? "NamedQueryId") <*> (x .:? "Description") <*>
                     (x .: "Name")
                     <*> (x .: "Database")
                     <*> (x .: "QueryString"))

instance Hashable NamedQuery where

instance NFData NamedQuery where

-- | Information about a single instance of a query execution.
--
--
--
-- /See:/ 'queryExecution' smart constructor.
data QueryExecution = QueryExecution'
  { _qeStatus                :: !(Maybe QueryExecutionStatus)
  , _qeQueryExecutionContext :: !(Maybe QueryExecutionContext)
  , _qeResultConfiguration   :: !(Maybe ResultConfiguration)
  , _qeQuery                 :: !(Maybe Text)
  , _qeStatistics            :: !(Maybe QueryExecutionStatistics)
  , _qeQueryExecutionId      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'QueryExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qeStatus' - The completion date, current state, submission time, and state change reason (if applicable) for the query execution.
--
-- * 'qeQueryExecutionContext' - The database in which the query execution occurred.
--
-- * 'qeResultConfiguration' - The location in Amazon S3 where query results were stored and the encryption option, if any, used for query results.
--
-- * 'qeQuery' - The SQL query statements which the query execution ran.
--
-- * 'qeStatistics' - The amount of data scanned during the query execution and the amount of time that it took to execute.
--
-- * 'qeQueryExecutionId' - The unique identifier for each query execution.
queryExecution
    :: QueryExecution
queryExecution =
  QueryExecution'
    { _qeStatus = Nothing
    , _qeQueryExecutionContext = Nothing
    , _qeResultConfiguration = Nothing
    , _qeQuery = Nothing
    , _qeStatistics = Nothing
    , _qeQueryExecutionId = Nothing
    }


-- | The completion date, current state, submission time, and state change reason (if applicable) for the query execution.
qeStatus :: Lens' QueryExecution (Maybe QueryExecutionStatus)
qeStatus = lens _qeStatus (\ s a -> s{_qeStatus = a})

-- | The database in which the query execution occurred.
qeQueryExecutionContext :: Lens' QueryExecution (Maybe QueryExecutionContext)
qeQueryExecutionContext = lens _qeQueryExecutionContext (\ s a -> s{_qeQueryExecutionContext = a})

-- | The location in Amazon S3 where query results were stored and the encryption option, if any, used for query results.
qeResultConfiguration :: Lens' QueryExecution (Maybe ResultConfiguration)
qeResultConfiguration = lens _qeResultConfiguration (\ s a -> s{_qeResultConfiguration = a})

-- | The SQL query statements which the query execution ran.
qeQuery :: Lens' QueryExecution (Maybe Text)
qeQuery = lens _qeQuery (\ s a -> s{_qeQuery = a})

-- | The amount of data scanned during the query execution and the amount of time that it took to execute.
qeStatistics :: Lens' QueryExecution (Maybe QueryExecutionStatistics)
qeStatistics = lens _qeStatistics (\ s a -> s{_qeStatistics = a})

-- | The unique identifier for each query execution.
qeQueryExecutionId :: Lens' QueryExecution (Maybe Text)
qeQueryExecutionId = lens _qeQueryExecutionId (\ s a -> s{_qeQueryExecutionId = a})

instance FromJSON QueryExecution where
        parseJSON
          = withObject "QueryExecution"
              (\ x ->
                 QueryExecution' <$>
                   (x .:? "Status") <*> (x .:? "QueryExecutionContext")
                     <*> (x .:? "ResultConfiguration")
                     <*> (x .:? "Query")
                     <*> (x .:? "Statistics")
                     <*> (x .:? "QueryExecutionId"))

instance Hashable QueryExecution where

instance NFData QueryExecution where

-- | The database in which the query execution occurs.
--
--
--
-- /See:/ 'queryExecutionContext' smart constructor.
newtype QueryExecutionContext = QueryExecutionContext'
  { _qecDatabase :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'QueryExecutionContext' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qecDatabase' - The name of the database.
queryExecutionContext
    :: QueryExecutionContext
queryExecutionContext = QueryExecutionContext' {_qecDatabase = Nothing}


-- | The name of the database.
qecDatabase :: Lens' QueryExecutionContext (Maybe Text)
qecDatabase = lens _qecDatabase (\ s a -> s{_qecDatabase = a})

instance FromJSON QueryExecutionContext where
        parseJSON
          = withObject "QueryExecutionContext"
              (\ x ->
                 QueryExecutionContext' <$> (x .:? "Database"))

instance Hashable QueryExecutionContext where

instance NFData QueryExecutionContext where

instance ToJSON QueryExecutionContext where
        toJSON QueryExecutionContext'{..}
          = object
              (catMaybes [("Database" .=) <$> _qecDatabase])

-- | The amount of data scanned during the query execution and the amount of time that it took to execute.
--
--
--
-- /See:/ 'queryExecutionStatistics' smart constructor.
data QueryExecutionStatistics = QueryExecutionStatistics'
  { _qesEngineExecutionTimeInMillis :: !(Maybe Integer)
  , _qesDataScannedInBytes          :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'QueryExecutionStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qesEngineExecutionTimeInMillis' - The number of milliseconds that the query took to execute.
--
-- * 'qesDataScannedInBytes' - The number of bytes in the data that was queried.
queryExecutionStatistics
    :: QueryExecutionStatistics
queryExecutionStatistics =
  QueryExecutionStatistics'
    { _qesEngineExecutionTimeInMillis = Nothing
    , _qesDataScannedInBytes = Nothing
    }


-- | The number of milliseconds that the query took to execute.
qesEngineExecutionTimeInMillis :: Lens' QueryExecutionStatistics (Maybe Integer)
qesEngineExecutionTimeInMillis = lens _qesEngineExecutionTimeInMillis (\ s a -> s{_qesEngineExecutionTimeInMillis = a})

-- | The number of bytes in the data that was queried.
qesDataScannedInBytes :: Lens' QueryExecutionStatistics (Maybe Integer)
qesDataScannedInBytes = lens _qesDataScannedInBytes (\ s a -> s{_qesDataScannedInBytes = a})

instance FromJSON QueryExecutionStatistics where
        parseJSON
          = withObject "QueryExecutionStatistics"
              (\ x ->
                 QueryExecutionStatistics' <$>
                   (x .:? "EngineExecutionTimeInMillis") <*>
                     (x .:? "DataScannedInBytes"))

instance Hashable QueryExecutionStatistics where

instance NFData QueryExecutionStatistics where

-- | The completion date, current state, submission time, and state change reason (if applicable) for the query execution.
--
--
--
-- /See:/ 'queryExecutionStatus' smart constructor.
data QueryExecutionStatus = QueryExecutionStatus'
  { _qesState              :: !(Maybe QueryExecutionState)
  , _qesStateChangeReason  :: !(Maybe Text)
  , _qesSubmissionDateTime :: !(Maybe POSIX)
  , _qesCompletionDateTime :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'QueryExecutionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qesState' - The state of query execution. @SUBMITTED@ indicates that the query is queued for execution. @RUNNING@ indicates that the query is scanning data and returning results. @SUCCEEDED@ indicates that the query completed without error. @FAILED@ indicates that the query experienced an error and did not complete processing. @CANCELLED@ indicates that user input interrupted query execution.
--
-- * 'qesStateChangeReason' - Further detail about the status of the query.
--
-- * 'qesSubmissionDateTime' - The date and time that the query was submitted.
--
-- * 'qesCompletionDateTime' - The date and time that the query completed.
queryExecutionStatus
    :: QueryExecutionStatus
queryExecutionStatus =
  QueryExecutionStatus'
    { _qesState = Nothing
    , _qesStateChangeReason = Nothing
    , _qesSubmissionDateTime = Nothing
    , _qesCompletionDateTime = Nothing
    }


-- | The state of query execution. @SUBMITTED@ indicates that the query is queued for execution. @RUNNING@ indicates that the query is scanning data and returning results. @SUCCEEDED@ indicates that the query completed without error. @FAILED@ indicates that the query experienced an error and did not complete processing. @CANCELLED@ indicates that user input interrupted query execution.
qesState :: Lens' QueryExecutionStatus (Maybe QueryExecutionState)
qesState = lens _qesState (\ s a -> s{_qesState = a})

-- | Further detail about the status of the query.
qesStateChangeReason :: Lens' QueryExecutionStatus (Maybe Text)
qesStateChangeReason = lens _qesStateChangeReason (\ s a -> s{_qesStateChangeReason = a})

-- | The date and time that the query was submitted.
qesSubmissionDateTime :: Lens' QueryExecutionStatus (Maybe UTCTime)
qesSubmissionDateTime = lens _qesSubmissionDateTime (\ s a -> s{_qesSubmissionDateTime = a}) . mapping _Time

-- | The date and time that the query completed.
qesCompletionDateTime :: Lens' QueryExecutionStatus (Maybe UTCTime)
qesCompletionDateTime = lens _qesCompletionDateTime (\ s a -> s{_qesCompletionDateTime = a}) . mapping _Time

instance FromJSON QueryExecutionStatus where
        parseJSON
          = withObject "QueryExecutionStatus"
              (\ x ->
                 QueryExecutionStatus' <$>
                   (x .:? "State") <*> (x .:? "StateChangeReason") <*>
                     (x .:? "SubmissionDateTime")
                     <*> (x .:? "CompletionDateTime"))

instance Hashable QueryExecutionStatus where

instance NFData QueryExecutionStatus where

-- | The location in Amazon S3 where query results are stored and the encryption option, if any, used for query results.
--
--
--
-- /See:/ 'resultConfiguration' smart constructor.
data ResultConfiguration = ResultConfiguration'
  { _rcEncryptionConfiguration :: !(Maybe EncryptionConfiguration)
  , _rcOutputLocation          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResultConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcEncryptionConfiguration' - If query results are encrypted in S3, indicates the S3 encryption option used (for example, @SSE-KMS@ or @CSE-KMS@ and key information.
--
-- * 'rcOutputLocation' - The location in S3 where query results are stored.
resultConfiguration
    :: Text -- ^ 'rcOutputLocation'
    -> ResultConfiguration
resultConfiguration pOutputLocation_ =
  ResultConfiguration'
    {_rcEncryptionConfiguration = Nothing, _rcOutputLocation = pOutputLocation_}


-- | If query results are encrypted in S3, indicates the S3 encryption option used (for example, @SSE-KMS@ or @CSE-KMS@ and key information.
rcEncryptionConfiguration :: Lens' ResultConfiguration (Maybe EncryptionConfiguration)
rcEncryptionConfiguration = lens _rcEncryptionConfiguration (\ s a -> s{_rcEncryptionConfiguration = a})

-- | The location in S3 where query results are stored.
rcOutputLocation :: Lens' ResultConfiguration Text
rcOutputLocation = lens _rcOutputLocation (\ s a -> s{_rcOutputLocation = a})

instance FromJSON ResultConfiguration where
        parseJSON
          = withObject "ResultConfiguration"
              (\ x ->
                 ResultConfiguration' <$>
                   (x .:? "EncryptionConfiguration") <*>
                     (x .: "OutputLocation"))

instance Hashable ResultConfiguration where

instance NFData ResultConfiguration where

instance ToJSON ResultConfiguration where
        toJSON ResultConfiguration'{..}
          = object
              (catMaybes
                 [("EncryptionConfiguration" .=) <$>
                    _rcEncryptionConfiguration,
                  Just ("OutputLocation" .= _rcOutputLocation)])

-- | The metadata and rows that comprise a query result set. The metadata describes the column structure and data types.
--
--
--
-- /See:/ 'resultSet' smart constructor.
data ResultSet = ResultSet'
  { _rsRows              :: !(Maybe [Row])
  , _rsResultSetMetadata :: !(Maybe ResultSetMetadata)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResultSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsRows' - The rows in the table.
--
-- * 'rsResultSetMetadata' - The metadata that describes the column structure and data types of a table of query results.
resultSet
    :: ResultSet
resultSet = ResultSet' {_rsRows = Nothing, _rsResultSetMetadata = Nothing}


-- | The rows in the table.
rsRows :: Lens' ResultSet [Row]
rsRows = lens _rsRows (\ s a -> s{_rsRows = a}) . _Default . _Coerce

-- | The metadata that describes the column structure and data types of a table of query results.
rsResultSetMetadata :: Lens' ResultSet (Maybe ResultSetMetadata)
rsResultSetMetadata = lens _rsResultSetMetadata (\ s a -> s{_rsResultSetMetadata = a})

instance FromJSON ResultSet where
        parseJSON
          = withObject "ResultSet"
              (\ x ->
                 ResultSet' <$>
                   (x .:? "Rows" .!= mempty) <*>
                     (x .:? "ResultSetMetadata"))

instance Hashable ResultSet where

instance NFData ResultSet where

-- | The metadata that describes the column structure and data types of a table of query results.
--
--
--
-- /See:/ 'resultSetMetadata' smart constructor.
newtype ResultSetMetadata = ResultSetMetadata'
  { _rsmColumnInfo :: Maybe [ColumnInfo]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResultSetMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsmColumnInfo' - Information about the columns in a query execution result.
resultSetMetadata
    :: ResultSetMetadata
resultSetMetadata = ResultSetMetadata' {_rsmColumnInfo = Nothing}


-- | Information about the columns in a query execution result.
rsmColumnInfo :: Lens' ResultSetMetadata [ColumnInfo]
rsmColumnInfo = lens _rsmColumnInfo (\ s a -> s{_rsmColumnInfo = a}) . _Default . _Coerce

instance FromJSON ResultSetMetadata where
        parseJSON
          = withObject "ResultSetMetadata"
              (\ x ->
                 ResultSetMetadata' <$>
                   (x .:? "ColumnInfo" .!= mempty))

instance Hashable ResultSetMetadata where

instance NFData ResultSetMetadata where

-- | The rows that comprise a query result table.
--
--
--
-- /See:/ 'row' smart constructor.
newtype Row = Row'
  { _rowData :: Maybe [Datum]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Row' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rowData' - The data that populates a row in a query result table.
row
    :: Row
row = Row' {_rowData = Nothing}


-- | The data that populates a row in a query result table.
rowData :: Lens' Row [Datum]
rowData = lens _rowData (\ s a -> s{_rowData = a}) . _Default . _Coerce

instance FromJSON Row where
        parseJSON
          = withObject "Row"
              (\ x -> Row' <$> (x .:? "Data" .!= mempty))

instance Hashable Row where

instance NFData Row where

-- | Information about a named query ID that could not be processed.
--
--
--
-- /See:/ 'unprocessedNamedQueryId' smart constructor.
data UnprocessedNamedQueryId = UnprocessedNamedQueryId'
  { _unqiNamedQueryId :: !(Maybe Text)
  , _unqiErrorCode    :: !(Maybe Text)
  , _unqiErrorMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnprocessedNamedQueryId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'unqiNamedQueryId' - The unique identifier of the named query.
--
-- * 'unqiErrorCode' - The error code returned when the processing request for the named query failed, if applicable.
--
-- * 'unqiErrorMessage' - The error message returned when the processing request for the named query failed, if applicable.
unprocessedNamedQueryId
    :: UnprocessedNamedQueryId
unprocessedNamedQueryId =
  UnprocessedNamedQueryId'
    { _unqiNamedQueryId = Nothing
    , _unqiErrorCode = Nothing
    , _unqiErrorMessage = Nothing
    }


-- | The unique identifier of the named query.
unqiNamedQueryId :: Lens' UnprocessedNamedQueryId (Maybe Text)
unqiNamedQueryId = lens _unqiNamedQueryId (\ s a -> s{_unqiNamedQueryId = a})

-- | The error code returned when the processing request for the named query failed, if applicable.
unqiErrorCode :: Lens' UnprocessedNamedQueryId (Maybe Text)
unqiErrorCode = lens _unqiErrorCode (\ s a -> s{_unqiErrorCode = a})

-- | The error message returned when the processing request for the named query failed, if applicable.
unqiErrorMessage :: Lens' UnprocessedNamedQueryId (Maybe Text)
unqiErrorMessage = lens _unqiErrorMessage (\ s a -> s{_unqiErrorMessage = a})

instance FromJSON UnprocessedNamedQueryId where
        parseJSON
          = withObject "UnprocessedNamedQueryId"
              (\ x ->
                 UnprocessedNamedQueryId' <$>
                   (x .:? "NamedQueryId") <*> (x .:? "ErrorCode") <*>
                     (x .:? "ErrorMessage"))

instance Hashable UnprocessedNamedQueryId where

instance NFData UnprocessedNamedQueryId where

-- | Describes a query execution that failed to process.
--
--
--
-- /See:/ 'unprocessedQueryExecutionId' smart constructor.
data UnprocessedQueryExecutionId = UnprocessedQueryExecutionId'
  { _uqeiErrorCode        :: !(Maybe Text)
  , _uqeiQueryExecutionId :: !(Maybe Text)
  , _uqeiErrorMessage     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnprocessedQueryExecutionId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uqeiErrorCode' - The error code returned when the query execution failed to process, if applicable.
--
-- * 'uqeiQueryExecutionId' - The unique identifier of the query execution.
--
-- * 'uqeiErrorMessage' - The error message returned when the query execution failed to process, if applicable.
unprocessedQueryExecutionId
    :: UnprocessedQueryExecutionId
unprocessedQueryExecutionId =
  UnprocessedQueryExecutionId'
    { _uqeiErrorCode = Nothing
    , _uqeiQueryExecutionId = Nothing
    , _uqeiErrorMessage = Nothing
    }


-- | The error code returned when the query execution failed to process, if applicable.
uqeiErrorCode :: Lens' UnprocessedQueryExecutionId (Maybe Text)
uqeiErrorCode = lens _uqeiErrorCode (\ s a -> s{_uqeiErrorCode = a})

-- | The unique identifier of the query execution.
uqeiQueryExecutionId :: Lens' UnprocessedQueryExecutionId (Maybe Text)
uqeiQueryExecutionId = lens _uqeiQueryExecutionId (\ s a -> s{_uqeiQueryExecutionId = a})

-- | The error message returned when the query execution failed to process, if applicable.
uqeiErrorMessage :: Lens' UnprocessedQueryExecutionId (Maybe Text)
uqeiErrorMessage = lens _uqeiErrorMessage (\ s a -> s{_uqeiErrorMessage = a})

instance FromJSON UnprocessedQueryExecutionId where
        parseJSON
          = withObject "UnprocessedQueryExecutionId"
              (\ x ->
                 UnprocessedQueryExecutionId' <$>
                   (x .:? "ErrorCode") <*> (x .:? "QueryExecutionId")
                     <*> (x .:? "ErrorMessage"))

instance Hashable UnprocessedQueryExecutionId where

instance NFData UnprocessedQueryExecutionId where

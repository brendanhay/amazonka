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

-- | If query results are encrypted in Amazon S3, indicates the encryption option used (for example, @SSE-KMS@ or @CSE-KMS@ ) and key information.
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
-- * 'ecEncryptionOption' - Indicates whether Amazon S3 server-side encryption with Amazon S3-managed keys (@SSE-S3@ ), server-side encryption with KMS-managed keys (@SSE-KMS@ ), or client-side encryption with KMS-managed keys (CSE-KMS) is used. If a query runs in a workgroup and the workgroup overrides client-side settings, then the workgroup's setting for encryption is used. It specifies whether query results must be encrypted, for all queries that run in this workgroup.
encryptionConfiguration
    :: EncryptionOption -- ^ 'ecEncryptionOption'
    -> EncryptionConfiguration
encryptionConfiguration pEncryptionOption_ =
  EncryptionConfiguration'
    {_ecKMSKey = Nothing, _ecEncryptionOption = pEncryptionOption_}


-- | For @SSE-KMS@ and @CSE-KMS@ , this is the KMS key ARN or ID.
ecKMSKey :: Lens' EncryptionConfiguration (Maybe Text)
ecKMSKey = lens _ecKMSKey (\ s a -> s{_ecKMSKey = a})

-- | Indicates whether Amazon S3 server-side encryption with Amazon S3-managed keys (@SSE-S3@ ), server-side encryption with KMS-managed keys (@SSE-KMS@ ), or client-side encryption with KMS-managed keys (CSE-KMS) is used. If a query runs in a workgroup and the workgroup overrides client-side settings, then the workgroup's setting for encryption is used. It specifies whether query results must be encrypted, for all queries that run in this workgroup.
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

-- | A query, where @QueryString@ is the list of SQL query statements that comprise the query.
--
--
--
-- /See:/ 'namedQuery' smart constructor.
data NamedQuery = NamedQuery'
  { _nqNamedQueryId :: !(Maybe Text)
  , _nqDescription  :: !(Maybe Text)
  , _nqWorkGroup    :: !(Maybe Text)
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
-- * 'nqDescription' - The query description.
--
-- * 'nqWorkGroup' - The name of the workgroup that contains the named query.
--
-- * 'nqName' - The query name.
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
    , _nqWorkGroup = Nothing
    , _nqName = pName_
    , _nqDatabase = pDatabase_
    , _nqQueryString = pQueryString_
    }


-- | The unique identifier of the query.
nqNamedQueryId :: Lens' NamedQuery (Maybe Text)
nqNamedQueryId = lens _nqNamedQueryId (\ s a -> s{_nqNamedQueryId = a})

-- | The query description.
nqDescription :: Lens' NamedQuery (Maybe Text)
nqDescription = lens _nqDescription (\ s a -> s{_nqDescription = a})

-- | The name of the workgroup that contains the named query.
nqWorkGroup :: Lens' NamedQuery (Maybe Text)
nqWorkGroup = lens _nqWorkGroup (\ s a -> s{_nqWorkGroup = a})

-- | The query name.
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
                     (x .:? "WorkGroup")
                     <*> (x .: "Name")
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
  , _qeStatementType         :: !(Maybe StatementType)
  , _qeStatistics            :: !(Maybe QueryExecutionStatistics)
  , _qeQueryExecutionId      :: !(Maybe Text)
  , _qeWorkGroup             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
-- * 'qeStatistics' - The amount of data scanned during the query execution and the amount of time that it took to execute, and the type of statement that was run.
--
-- * 'qeQueryExecutionId' - The unique identifier for each query execution.
--
-- * 'qeWorkGroup' - The name of the workgroup in which the query ran.
queryExecution
    :: QueryExecution
queryExecution =
  QueryExecution'
    { _qeStatus = Nothing
    , _qeQueryExecutionContext = Nothing
    , _qeResultConfiguration = Nothing
    , _qeQuery = Nothing
    , _qeStatementType = Nothing
    , _qeStatistics = Nothing
    , _qeQueryExecutionId = Nothing
    , _qeWorkGroup = Nothing
    }


-- | The completion date, current state, submission time, and state change reason (if applicable) for the query execution.
qeStatus :: Lens' QueryExecution (Maybe QueryExecutionStatus)
qeStatus = lens _qeStatus (\ s a -> s{_qeStatus = a})

-- | The database in which the query execution occurred.
qeQueryExecutionContext :: Lens' QueryExecution (Maybe QueryExecutionContext)
qeQueryExecutionContext = lens _qeQueryExecutionContext (\ s a -> s{_qeQueryExecutionContext = a})

-- | The location in Amazon S3 where query results were stored and the encryption option, if any, used for query results. These are known as "client-side settings". If workgroup settings override client-side settings, then the query uses the location for the query results and the encryption configuration that are specified for the workgroup.
qeResultConfiguration :: Lens' QueryExecution (Maybe ResultConfiguration)
qeResultConfiguration = lens _qeResultConfiguration (\ s a -> s{_qeResultConfiguration = a})

-- | The SQL query statements which the query execution ran.
qeQuery :: Lens' QueryExecution (Maybe Text)
qeQuery = lens _qeQuery (\ s a -> s{_qeQuery = a})

-- | The type of query statement that was run. @DDL@ indicates DDL query statements. @DML@ indicates DML (Data Manipulation Language) query statements, such as @CREATE TABLE AS SELECT@ . @UTILITY@ indicates query statements other than DDL and DML, such as @SHOW CREATE TABLE@ , or @DESCRIBE <table>@ .
qeStatementType :: Lens' QueryExecution (Maybe StatementType)
qeStatementType = lens _qeStatementType (\ s a -> s{_qeStatementType = a})

-- | The amount of data scanned during the query execution and the amount of time that it took to execute, and the type of statement that was run.
qeStatistics :: Lens' QueryExecution (Maybe QueryExecutionStatistics)
qeStatistics = lens _qeStatistics (\ s a -> s{_qeStatistics = a})

-- | The unique identifier for each query execution.
qeQueryExecutionId :: Lens' QueryExecution (Maybe Text)
qeQueryExecutionId = lens _qeQueryExecutionId (\ s a -> s{_qeQueryExecutionId = a})

-- | The name of the workgroup in which the query ran.
qeWorkGroup :: Lens' QueryExecution (Maybe Text)
qeWorkGroup = lens _qeWorkGroup (\ s a -> s{_qeWorkGroup = a})

instance FromJSON QueryExecution where
        parseJSON
          = withObject "QueryExecution"
              (\ x ->
                 QueryExecution' <$>
                   (x .:? "Status") <*> (x .:? "QueryExecutionContext")
                     <*> (x .:? "ResultConfiguration")
                     <*> (x .:? "Query")
                     <*> (x .:? "StatementType")
                     <*> (x .:? "Statistics")
                     <*> (x .:? "QueryExecutionId")
                     <*> (x .:? "WorkGroup"))

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

-- | The amount of data scanned during the query execution and the amount of time that it took to execute, and the type of statement that was run.
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
-- * 'qesState' - The state of query execution. @QUEUED@ state is listed but is not used by Athena and is reserved for future use. @RUNNING@ indicates that the query has been submitted to the service, and Athena will execute the query as soon as resources are available. @SUCCEEDED@ indicates that the query completed without errors. @FAILED@ indicates that the query experienced an error and did not complete processing. @CANCELLED@ indicates that a user input interrupted query execution.
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


-- | The state of query execution. @QUEUED@ state is listed but is not used by Athena and is reserved for future use. @RUNNING@ indicates that the query has been submitted to the service, and Athena will execute the query as soon as resources are available. @SUCCEEDED@ indicates that the query completed without errors. @FAILED@ indicates that the query experienced an error and did not complete processing. @CANCELLED@ indicates that a user input interrupted query execution.
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

-- | The location in Amazon S3 where query results are stored and the encryption option, if any, used for query results. These are known as "client-side settings". If workgroup settings override client-side settings, then the query uses the location for the query results and the encryption configuration that are specified for the workgroup.
--
--
--
-- /See:/ 'resultConfiguration' smart constructor.
data ResultConfiguration = ResultConfiguration'
  { _rcEncryptionConfiguration :: !(Maybe EncryptionConfiguration)
  , _rcOutputLocation          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResultConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcEncryptionConfiguration' - If query results are encrypted in Amazon S3, indicates the encryption option used (for example, @SSE-KMS@ or @CSE-KMS@ ) and key information. This is a client-side setting. If workgroup settings override client-side settings, then the query uses the encryption configuration that is specified for the workgroup, and also uses the location for storing query results specified in the workgroup. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' and <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
--
-- * 'rcOutputLocation' - The location in Amazon S3 where your query results are stored, such as @s3://path/to/query/bucket/@ . For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Queries and Query Result Files.> If workgroup settings override client-side settings, then the query uses the location for the query results and the encryption configuration that are specified for the workgroup. The "workgroup settings override" is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
resultConfiguration
    :: ResultConfiguration
resultConfiguration =
  ResultConfiguration'
    {_rcEncryptionConfiguration = Nothing, _rcOutputLocation = Nothing}


-- | If query results are encrypted in Amazon S3, indicates the encryption option used (for example, @SSE-KMS@ or @CSE-KMS@ ) and key information. This is a client-side setting. If workgroup settings override client-side settings, then the query uses the encryption configuration that is specified for the workgroup, and also uses the location for storing query results specified in the workgroup. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' and <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
rcEncryptionConfiguration :: Lens' ResultConfiguration (Maybe EncryptionConfiguration)
rcEncryptionConfiguration = lens _rcEncryptionConfiguration (\ s a -> s{_rcEncryptionConfiguration = a})

-- | The location in Amazon S3 where your query results are stored, such as @s3://path/to/query/bucket/@ . For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Queries and Query Result Files.> If workgroup settings override client-side settings, then the query uses the location for the query results and the encryption configuration that are specified for the workgroup. The "workgroup settings override" is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
rcOutputLocation :: Lens' ResultConfiguration (Maybe Text)
rcOutputLocation = lens _rcOutputLocation (\ s a -> s{_rcOutputLocation = a})

instance FromJSON ResultConfiguration where
        parseJSON
          = withObject "ResultConfiguration"
              (\ x ->
                 ResultConfiguration' <$>
                   (x .:? "EncryptionConfiguration") <*>
                     (x .:? "OutputLocation"))

instance Hashable ResultConfiguration where

instance NFData ResultConfiguration where

instance ToJSON ResultConfiguration where
        toJSON ResultConfiguration'{..}
          = object
              (catMaybes
                 [("EncryptionConfiguration" .=) <$>
                    _rcEncryptionConfiguration,
                  ("OutputLocation" .=) <$> _rcOutputLocation])

-- | The information about the updates in the query results, such as output location and encryption configuration for the query results.
--
--
--
-- /See:/ 'resultConfigurationUpdates' smart constructor.
data ResultConfigurationUpdates = ResultConfigurationUpdates'
  { _rcuRemoveOutputLocation          :: !(Maybe Bool)
  , _rcuRemoveEncryptionConfiguration :: !(Maybe Bool)
  , _rcuEncryptionConfiguration       :: !(Maybe EncryptionConfiguration)
  , _rcuOutputLocation                :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResultConfigurationUpdates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcuRemoveOutputLocation' - If set to "true", indicates that the previously-specified query results location (also known as a client-side setting) for queries in this workgroup should be ignored and set to null. If set to "false" or not set, and a value is present in the OutputLocation in ResultConfigurationUpdates (the client-side setting), the OutputLocation in the workgroup's ResultConfiguration will be updated with the new value. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
--
-- * 'rcuRemoveEncryptionConfiguration' - If set to "true", indicates that the previously-specified encryption configuration (also known as the client-side setting) for queries in this workgroup should be ignored and set to null. If set to "false" or not set, and a value is present in the EncryptionConfiguration in ResultConfigurationUpdates (the client-side setting), the EncryptionConfiguration in the workgroup's ResultConfiguration will be updated with the new value. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
--
-- * 'rcuEncryptionConfiguration' - The encryption configuration for the query results.
--
-- * 'rcuOutputLocation' - The location in Amazon S3 where your query results are stored, such as @s3://path/to/query/bucket/@ . For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Queries and Query Result Files.> If workgroup settings override client-side settings, then the query uses the location for the query results and the encryption configuration that are specified for the workgroup. The "workgroup settings override" is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
resultConfigurationUpdates
    :: ResultConfigurationUpdates
resultConfigurationUpdates =
  ResultConfigurationUpdates'
    { _rcuRemoveOutputLocation = Nothing
    , _rcuRemoveEncryptionConfiguration = Nothing
    , _rcuEncryptionConfiguration = Nothing
    , _rcuOutputLocation = Nothing
    }


-- | If set to "true", indicates that the previously-specified query results location (also known as a client-side setting) for queries in this workgroup should be ignored and set to null. If set to "false" or not set, and a value is present in the OutputLocation in ResultConfigurationUpdates (the client-side setting), the OutputLocation in the workgroup's ResultConfiguration will be updated with the new value. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
rcuRemoveOutputLocation :: Lens' ResultConfigurationUpdates (Maybe Bool)
rcuRemoveOutputLocation = lens _rcuRemoveOutputLocation (\ s a -> s{_rcuRemoveOutputLocation = a})

-- | If set to "true", indicates that the previously-specified encryption configuration (also known as the client-side setting) for queries in this workgroup should be ignored and set to null. If set to "false" or not set, and a value is present in the EncryptionConfiguration in ResultConfigurationUpdates (the client-side setting), the EncryptionConfiguration in the workgroup's ResultConfiguration will be updated with the new value. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
rcuRemoveEncryptionConfiguration :: Lens' ResultConfigurationUpdates (Maybe Bool)
rcuRemoveEncryptionConfiguration = lens _rcuRemoveEncryptionConfiguration (\ s a -> s{_rcuRemoveEncryptionConfiguration = a})

-- | The encryption configuration for the query results.
rcuEncryptionConfiguration :: Lens' ResultConfigurationUpdates (Maybe EncryptionConfiguration)
rcuEncryptionConfiguration = lens _rcuEncryptionConfiguration (\ s a -> s{_rcuEncryptionConfiguration = a})

-- | The location in Amazon S3 where your query results are stored, such as @s3://path/to/query/bucket/@ . For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Queries and Query Result Files.> If workgroup settings override client-side settings, then the query uses the location for the query results and the encryption configuration that are specified for the workgroup. The "workgroup settings override" is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
rcuOutputLocation :: Lens' ResultConfigurationUpdates (Maybe Text)
rcuOutputLocation = lens _rcuOutputLocation (\ s a -> s{_rcuOutputLocation = a})

instance Hashable ResultConfigurationUpdates where

instance NFData ResultConfigurationUpdates where

instance ToJSON ResultConfigurationUpdates where
        toJSON ResultConfigurationUpdates'{..}
          = object
              (catMaybes
                 [("RemoveOutputLocation" .=) <$>
                    _rcuRemoveOutputLocation,
                  ("RemoveEncryptionConfiguration" .=) <$>
                    _rcuRemoveEncryptionConfiguration,
                  ("EncryptionConfiguration" .=) <$>
                    _rcuEncryptionConfiguration,
                  ("OutputLocation" .=) <$> _rcuOutputLocation])

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
-- * 'rsmColumnInfo' - Information about the columns returned in a query result metadata.
resultSetMetadata
    :: ResultSetMetadata
resultSetMetadata = ResultSetMetadata' {_rsmColumnInfo = Nothing}


-- | Information about the columns returned in a query result metadata.
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

-- | A tag that you can add to a resource. A tag is a label that you assign to an AWS Athena resource (a workgroup). Each tag consists of a key and an optional value, both of which you define. Tags enable you to categorize workgroups in Athena, for example, by purpose, owner, or environment. Use a consistent set of tag keys to make it easier to search and filter workgroups in your account. The maximum tag key length is 128 Unicode characters in UTF-8. The maximum tag value length is 256 Unicode characters in UTF-8. You can use letters and numbers representable in UTF-8, and the following characters: + - = . _ : / @. Tag keys and values are case-sensitive. Tag keys must be unique per resource.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - A tag value. The tag value length is from 0 to 256 Unicode characters in UTF-8. You can use letters and numbers representable in UTF-8, and the following characters: + - = . _ : / @. Tag values are case-sensitive.
--
-- * 'tagKey' - A tag key. The tag key length is from 1 to 128 Unicode characters in UTF-8. You can use letters and numbers representable in UTF-8, and the following characters: + - = . _ : / @. Tag keys are case-sensitive and must be unique per resource.
tag
    :: Tag
tag = Tag' {_tagValue = Nothing, _tagKey = Nothing}


-- | A tag value. The tag value length is from 0 to 256 Unicode characters in UTF-8. You can use letters and numbers representable in UTF-8, and the following characters: + - = . _ : / @. Tag values are case-sensitive.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | A tag key. The tag key length is from 1 to 128 Unicode characters in UTF-8. You can use letters and numbers representable in UTF-8, and the following characters: + - = . _ : / @. Tag keys are case-sensitive and must be unique per resource.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "Value") <*> (x .:? "Key"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _tagValue, ("Key" .=) <$> _tagKey])

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

-- | A workgroup, which contains a name, description, creation time, state, and other configuration, listed under 'WorkGroup$Configuration' . Each workgroup enables you to isolate queries for you or your group of users from other queries in the same account, to configure the query results location and the encryption configuration (known as workgroup settings), to enable sending query metrics to Amazon CloudWatch, and to establish per-query data usage control limits for all queries in a workgroup. The workgroup settings override is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
--
--
--
-- /See:/ 'workGroup' smart constructor.
data WorkGroup = WorkGroup'
  { _wgCreationTime  :: !(Maybe POSIX)
  , _wgState         :: !(Maybe WorkGroupState)
  , _wgConfiguration :: !(Maybe WorkGroupConfiguration)
  , _wgDescription   :: !(Maybe Text)
  , _wgName          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'WorkGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wgCreationTime' - The date and time the workgroup was created.
--
-- * 'wgState' - The state of the workgroup: ENABLED or DISABLED.
--
-- * 'wgConfiguration' - The configuration of the workgroup, which includes the location in Amazon S3 where query results are stored, the encryption configuration, if any, used for query results; whether the Amazon CloudWatch Metrics are enabled for the workgroup; whether workgroup settings override client-side settings; and the data usage limit for the amount of data scanned per query, if it is specified. The workgroup settings override is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
--
-- * 'wgDescription' - The workgroup description.
--
-- * 'wgName' - The workgroup name.
workGroup
    :: Text -- ^ 'wgName'
    -> WorkGroup
workGroup pName_ =
  WorkGroup'
    { _wgCreationTime = Nothing
    , _wgState = Nothing
    , _wgConfiguration = Nothing
    , _wgDescription = Nothing
    , _wgName = pName_
    }


-- | The date and time the workgroup was created.
wgCreationTime :: Lens' WorkGroup (Maybe UTCTime)
wgCreationTime = lens _wgCreationTime (\ s a -> s{_wgCreationTime = a}) . mapping _Time

-- | The state of the workgroup: ENABLED or DISABLED.
wgState :: Lens' WorkGroup (Maybe WorkGroupState)
wgState = lens _wgState (\ s a -> s{_wgState = a})

-- | The configuration of the workgroup, which includes the location in Amazon S3 where query results are stored, the encryption configuration, if any, used for query results; whether the Amazon CloudWatch Metrics are enabled for the workgroup; whether workgroup settings override client-side settings; and the data usage limit for the amount of data scanned per query, if it is specified. The workgroup settings override is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
wgConfiguration :: Lens' WorkGroup (Maybe WorkGroupConfiguration)
wgConfiguration = lens _wgConfiguration (\ s a -> s{_wgConfiguration = a})

-- | The workgroup description.
wgDescription :: Lens' WorkGroup (Maybe Text)
wgDescription = lens _wgDescription (\ s a -> s{_wgDescription = a})

-- | The workgroup name.
wgName :: Lens' WorkGroup Text
wgName = lens _wgName (\ s a -> s{_wgName = a})

instance FromJSON WorkGroup where
        parseJSON
          = withObject "WorkGroup"
              (\ x ->
                 WorkGroup' <$>
                   (x .:? "CreationTime") <*> (x .:? "State") <*>
                     (x .:? "Configuration")
                     <*> (x .:? "Description")
                     <*> (x .: "Name"))

instance Hashable WorkGroup where

instance NFData WorkGroup where

-- | The configuration of the workgroup, which includes the location in Amazon S3 where query results are stored, the encryption option, if any, used for query results, whether the Amazon CloudWatch Metrics are enabled for the workgroup and whether workgroup settings override query settings, and the data usage limit for the amount of data scanned per query, if it is specified. The workgroup settings override is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
--
--
--
-- /See:/ 'workGroupConfiguration' smart constructor.
data WorkGroupConfiguration = WorkGroupConfiguration'
  { _wgcResultConfiguration             :: !(Maybe ResultConfiguration)
  , _wgcBytesScannedCutoffPerQuery      :: !(Maybe Nat)
  , _wgcEnforceWorkGroupConfiguration   :: !(Maybe Bool)
  , _wgcPublishCloudWatchMetricsEnabled :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'WorkGroupConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wgcResultConfiguration' - The configuration for the workgroup, which includes the location in Amazon S3 where query results are stored and the encryption option, if any, used for query results.
--
-- * 'wgcBytesScannedCutoffPerQuery' - The upper data usage limit (cutoff) for the amount of bytes a single query in a workgroup is allowed to scan.
--
-- * 'wgcEnforceWorkGroupConfiguration' - If set to "true", the settings for the workgroup override client-side settings. If set to "false", client-side settings are used. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
--
-- * 'wgcPublishCloudWatchMetricsEnabled' - Indicates that the Amazon CloudWatch metrics are enabled for the workgroup.
workGroupConfiguration
    :: WorkGroupConfiguration
workGroupConfiguration =
  WorkGroupConfiguration'
    { _wgcResultConfiguration = Nothing
    , _wgcBytesScannedCutoffPerQuery = Nothing
    , _wgcEnforceWorkGroupConfiguration = Nothing
    , _wgcPublishCloudWatchMetricsEnabled = Nothing
    }


-- | The configuration for the workgroup, which includes the location in Amazon S3 where query results are stored and the encryption option, if any, used for query results.
wgcResultConfiguration :: Lens' WorkGroupConfiguration (Maybe ResultConfiguration)
wgcResultConfiguration = lens _wgcResultConfiguration (\ s a -> s{_wgcResultConfiguration = a})

-- | The upper data usage limit (cutoff) for the amount of bytes a single query in a workgroup is allowed to scan.
wgcBytesScannedCutoffPerQuery :: Lens' WorkGroupConfiguration (Maybe Natural)
wgcBytesScannedCutoffPerQuery = lens _wgcBytesScannedCutoffPerQuery (\ s a -> s{_wgcBytesScannedCutoffPerQuery = a}) . mapping _Nat

-- | If set to "true", the settings for the workgroup override client-side settings. If set to "false", client-side settings are used. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
wgcEnforceWorkGroupConfiguration :: Lens' WorkGroupConfiguration (Maybe Bool)
wgcEnforceWorkGroupConfiguration = lens _wgcEnforceWorkGroupConfiguration (\ s a -> s{_wgcEnforceWorkGroupConfiguration = a})

-- | Indicates that the Amazon CloudWatch metrics are enabled for the workgroup.
wgcPublishCloudWatchMetricsEnabled :: Lens' WorkGroupConfiguration (Maybe Bool)
wgcPublishCloudWatchMetricsEnabled = lens _wgcPublishCloudWatchMetricsEnabled (\ s a -> s{_wgcPublishCloudWatchMetricsEnabled = a})

instance FromJSON WorkGroupConfiguration where
        parseJSON
          = withObject "WorkGroupConfiguration"
              (\ x ->
                 WorkGroupConfiguration' <$>
                   (x .:? "ResultConfiguration") <*>
                     (x .:? "BytesScannedCutoffPerQuery")
                     <*> (x .:? "EnforceWorkGroupConfiguration")
                     <*> (x .:? "PublishCloudWatchMetricsEnabled"))

instance Hashable WorkGroupConfiguration where

instance NFData WorkGroupConfiguration where

instance ToJSON WorkGroupConfiguration where
        toJSON WorkGroupConfiguration'{..}
          = object
              (catMaybes
                 [("ResultConfiguration" .=) <$>
                    _wgcResultConfiguration,
                  ("BytesScannedCutoffPerQuery" .=) <$>
                    _wgcBytesScannedCutoffPerQuery,
                  ("EnforceWorkGroupConfiguration" .=) <$>
                    _wgcEnforceWorkGroupConfiguration,
                  ("PublishCloudWatchMetricsEnabled" .=) <$>
                    _wgcPublishCloudWatchMetricsEnabled])

-- | The configuration information that will be updated for this workgroup, which includes the location in Amazon S3 where query results are stored, the encryption option, if any, used for query results, whether the Amazon CloudWatch Metrics are enabled for the workgroup, whether the workgroup settings override the client-side settings, and the data usage limit for the amount of bytes scanned per query, if it is specified.
--
--
--
-- /See:/ 'workGroupConfigurationUpdates' smart constructor.
data WorkGroupConfigurationUpdates = WorkGroupConfigurationUpdates'
  { _wgcuResultConfigurationUpdates       :: !(Maybe ResultConfigurationUpdates)
  , _wgcuBytesScannedCutoffPerQuery       :: !(Maybe Nat)
  , _wgcuRemoveBytesScannedCutoffPerQuery :: !(Maybe Bool)
  , _wgcuEnforceWorkGroupConfiguration    :: !(Maybe Bool)
  , _wgcuPublishCloudWatchMetricsEnabled  :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'WorkGroupConfigurationUpdates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wgcuResultConfigurationUpdates' - The result configuration information about the queries in this workgroup that will be updated. Includes the updated results location and an updated option for encrypting query results.
--
-- * 'wgcuBytesScannedCutoffPerQuery' - The upper limit (cutoff) for the amount of bytes a single query in a workgroup is allowed to scan.
--
-- * 'wgcuRemoveBytesScannedCutoffPerQuery' - Indicates that the data usage control limit per query is removed. 'WorkGroupConfiguration$BytesScannedCutoffPerQuery'
--
-- * 'wgcuEnforceWorkGroupConfiguration' - If set to "true", the settings for the workgroup override client-side settings. If set to "false" client-side settings are used. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
--
-- * 'wgcuPublishCloudWatchMetricsEnabled' - Indicates whether this workgroup enables publishing metrics to Amazon CloudWatch.
workGroupConfigurationUpdates
    :: WorkGroupConfigurationUpdates
workGroupConfigurationUpdates =
  WorkGroupConfigurationUpdates'
    { _wgcuResultConfigurationUpdates = Nothing
    , _wgcuBytesScannedCutoffPerQuery = Nothing
    , _wgcuRemoveBytesScannedCutoffPerQuery = Nothing
    , _wgcuEnforceWorkGroupConfiguration = Nothing
    , _wgcuPublishCloudWatchMetricsEnabled = Nothing
    }


-- | The result configuration information about the queries in this workgroup that will be updated. Includes the updated results location and an updated option for encrypting query results.
wgcuResultConfigurationUpdates :: Lens' WorkGroupConfigurationUpdates (Maybe ResultConfigurationUpdates)
wgcuResultConfigurationUpdates = lens _wgcuResultConfigurationUpdates (\ s a -> s{_wgcuResultConfigurationUpdates = a})

-- | The upper limit (cutoff) for the amount of bytes a single query in a workgroup is allowed to scan.
wgcuBytesScannedCutoffPerQuery :: Lens' WorkGroupConfigurationUpdates (Maybe Natural)
wgcuBytesScannedCutoffPerQuery = lens _wgcuBytesScannedCutoffPerQuery (\ s a -> s{_wgcuBytesScannedCutoffPerQuery = a}) . mapping _Nat

-- | Indicates that the data usage control limit per query is removed. 'WorkGroupConfiguration$BytesScannedCutoffPerQuery'
wgcuRemoveBytesScannedCutoffPerQuery :: Lens' WorkGroupConfigurationUpdates (Maybe Bool)
wgcuRemoveBytesScannedCutoffPerQuery = lens _wgcuRemoveBytesScannedCutoffPerQuery (\ s a -> s{_wgcuRemoveBytesScannedCutoffPerQuery = a})

-- | If set to "true", the settings for the workgroup override client-side settings. If set to "false" client-side settings are used. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
wgcuEnforceWorkGroupConfiguration :: Lens' WorkGroupConfigurationUpdates (Maybe Bool)
wgcuEnforceWorkGroupConfiguration = lens _wgcuEnforceWorkGroupConfiguration (\ s a -> s{_wgcuEnforceWorkGroupConfiguration = a})

-- | Indicates whether this workgroup enables publishing metrics to Amazon CloudWatch.
wgcuPublishCloudWatchMetricsEnabled :: Lens' WorkGroupConfigurationUpdates (Maybe Bool)
wgcuPublishCloudWatchMetricsEnabled = lens _wgcuPublishCloudWatchMetricsEnabled (\ s a -> s{_wgcuPublishCloudWatchMetricsEnabled = a})

instance Hashable WorkGroupConfigurationUpdates where

instance NFData WorkGroupConfigurationUpdates where

instance ToJSON WorkGroupConfigurationUpdates where
        toJSON WorkGroupConfigurationUpdates'{..}
          = object
              (catMaybes
                 [("ResultConfigurationUpdates" .=) <$>
                    _wgcuResultConfigurationUpdates,
                  ("BytesScannedCutoffPerQuery" .=) <$>
                    _wgcuBytesScannedCutoffPerQuery,
                  ("RemoveBytesScannedCutoffPerQuery" .=) <$>
                    _wgcuRemoveBytesScannedCutoffPerQuery,
                  ("EnforceWorkGroupConfiguration" .=) <$>
                    _wgcuEnforceWorkGroupConfiguration,
                  ("PublishCloudWatchMetricsEnabled" .=) <$>
                    _wgcuPublishCloudWatchMetricsEnabled])

-- | The summary information for the workgroup, which includes its name, state, description, and the date and time it was created.
--
--
--
-- /See:/ 'workGroupSummary' smart constructor.
data WorkGroupSummary = WorkGroupSummary'
  { _wgsCreationTime :: !(Maybe POSIX)
  , _wgsState        :: !(Maybe WorkGroupState)
  , _wgsName         :: !(Maybe Text)
  , _wgsDescription  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'WorkGroupSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wgsCreationTime' - The workgroup creation date and time.
--
-- * 'wgsState' - The state of the workgroup.
--
-- * 'wgsName' - The name of the workgroup.
--
-- * 'wgsDescription' - The workgroup description.
workGroupSummary
    :: WorkGroupSummary
workGroupSummary =
  WorkGroupSummary'
    { _wgsCreationTime = Nothing
    , _wgsState = Nothing
    , _wgsName = Nothing
    , _wgsDescription = Nothing
    }


-- | The workgroup creation date and time.
wgsCreationTime :: Lens' WorkGroupSummary (Maybe UTCTime)
wgsCreationTime = lens _wgsCreationTime (\ s a -> s{_wgsCreationTime = a}) . mapping _Time

-- | The state of the workgroup.
wgsState :: Lens' WorkGroupSummary (Maybe WorkGroupState)
wgsState = lens _wgsState (\ s a -> s{_wgsState = a})

-- | The name of the workgroup.
wgsName :: Lens' WorkGroupSummary (Maybe Text)
wgsName = lens _wgsName (\ s a -> s{_wgsName = a})

-- | The workgroup description.
wgsDescription :: Lens' WorkGroupSummary (Maybe Text)
wgsDescription = lens _wgsDescription (\ s a -> s{_wgsDescription = a})

instance FromJSON WorkGroupSummary where
        parseJSON
          = withObject "WorkGroupSummary"
              (\ x ->
                 WorkGroupSummary' <$>
                   (x .:? "CreationTime") <*> (x .:? "State") <*>
                     (x .:? "Name")
                     <*> (x .:? "Description"))

instance Hashable WorkGroupSummary where

instance NFData WorkGroupSummary where

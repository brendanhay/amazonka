{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Product where

import Network.AWS.Glue.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines an action to be initiated by a trigger.
--
--
--
-- /See:/ 'action' smart constructor.
data Action = Action'
  { _aArguments :: !(Maybe (Map Text Text))
  , _aJobName   :: !(Maybe Text)
  , _aTimeout   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Action' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aArguments' - Arguments to be passed to the job. You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes. For information about how to specify and consume your own Job arguments, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide. For information about the key-value pairs that AWS Glue consumes to set up your job, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
--
-- * 'aJobName' - The name of a job to be executed.
--
-- * 'aTimeout' - The job run timeout in minutes. It overrides the timeout value of the job.
action
    :: Action
action =
  Action' {_aArguments = Nothing, _aJobName = Nothing, _aTimeout = Nothing}


-- | Arguments to be passed to the job. You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes. For information about how to specify and consume your own Job arguments, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide. For information about the key-value pairs that AWS Glue consumes to set up your job, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
aArguments :: Lens' Action (HashMap Text Text)
aArguments = lens _aArguments (\ s a -> s{_aArguments = a}) . _Default . _Map

-- | The name of a job to be executed.
aJobName :: Lens' Action (Maybe Text)
aJobName = lens _aJobName (\ s a -> s{_aJobName = a})

-- | The job run timeout in minutes. It overrides the timeout value of the job.
aTimeout :: Lens' Action (Maybe Natural)
aTimeout = lens _aTimeout (\ s a -> s{_aTimeout = a}) . mapping _Nat

instance FromJSON Action where
        parseJSON
          = withObject "Action"
              (\ x ->
                 Action' <$>
                   (x .:? "Arguments" .!= mempty) <*> (x .:? "JobName")
                     <*> (x .:? "Timeout"))

instance Hashable Action where

instance NFData Action where

instance ToJSON Action where
        toJSON Action'{..}
          = object
              (catMaybes
                 [("Arguments" .=) <$> _aArguments,
                  ("JobName" .=) <$> _aJobName,
                  ("Timeout" .=) <$> _aTimeout])

-- | Records an error that occurred when attempting to stop a specified job run.
--
--
--
-- /See:/ 'batchStopJobRunError' smart constructor.
data BatchStopJobRunError = BatchStopJobRunError'
  { _bsjreJobName     :: !(Maybe Text)
  , _bsjreJobRunId    :: !(Maybe Text)
  , _bsjreErrorDetail :: !(Maybe ErrorDetail)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchStopJobRunError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsjreJobName' - The name of the job definition used in the job run in question.
--
-- * 'bsjreJobRunId' - The JobRunId of the job run in question.
--
-- * 'bsjreErrorDetail' - Specifies details about the error that was encountered.
batchStopJobRunError
    :: BatchStopJobRunError
batchStopJobRunError =
  BatchStopJobRunError'
    { _bsjreJobName = Nothing
    , _bsjreJobRunId = Nothing
    , _bsjreErrorDetail = Nothing
    }


-- | The name of the job definition used in the job run in question.
bsjreJobName :: Lens' BatchStopJobRunError (Maybe Text)
bsjreJobName = lens _bsjreJobName (\ s a -> s{_bsjreJobName = a})

-- | The JobRunId of the job run in question.
bsjreJobRunId :: Lens' BatchStopJobRunError (Maybe Text)
bsjreJobRunId = lens _bsjreJobRunId (\ s a -> s{_bsjreJobRunId = a})

-- | Specifies details about the error that was encountered.
bsjreErrorDetail :: Lens' BatchStopJobRunError (Maybe ErrorDetail)
bsjreErrorDetail = lens _bsjreErrorDetail (\ s a -> s{_bsjreErrorDetail = a})

instance FromJSON BatchStopJobRunError where
        parseJSON
          = withObject "BatchStopJobRunError"
              (\ x ->
                 BatchStopJobRunError' <$>
                   (x .:? "JobName") <*> (x .:? "JobRunId") <*>
                     (x .:? "ErrorDetail"))

instance Hashable BatchStopJobRunError where

instance NFData BatchStopJobRunError where

-- | Records a successful request to stop a specified JobRun.
--
--
--
-- /See:/ 'batchStopJobRunSuccessfulSubmission' smart constructor.
data BatchStopJobRunSuccessfulSubmission = BatchStopJobRunSuccessfulSubmission'
  { _bsjrssJobName  :: !(Maybe Text)
  , _bsjrssJobRunId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchStopJobRunSuccessfulSubmission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsjrssJobName' - The name of the job definition used in the job run that was stopped.
--
-- * 'bsjrssJobRunId' - The JobRunId of the job run that was stopped.
batchStopJobRunSuccessfulSubmission
    :: BatchStopJobRunSuccessfulSubmission
batchStopJobRunSuccessfulSubmission =
  BatchStopJobRunSuccessfulSubmission'
    {_bsjrssJobName = Nothing, _bsjrssJobRunId = Nothing}


-- | The name of the job definition used in the job run that was stopped.
bsjrssJobName :: Lens' BatchStopJobRunSuccessfulSubmission (Maybe Text)
bsjrssJobName = lens _bsjrssJobName (\ s a -> s{_bsjrssJobName = a})

-- | The JobRunId of the job run that was stopped.
bsjrssJobRunId :: Lens' BatchStopJobRunSuccessfulSubmission (Maybe Text)
bsjrssJobRunId = lens _bsjrssJobRunId (\ s a -> s{_bsjrssJobRunId = a})

instance FromJSON BatchStopJobRunSuccessfulSubmission
         where
        parseJSON
          = withObject "BatchStopJobRunSuccessfulSubmission"
              (\ x ->
                 BatchStopJobRunSuccessfulSubmission' <$>
                   (x .:? "JobName") <*> (x .:? "JobRunId"))

instance Hashable BatchStopJobRunSuccessfulSubmission
         where

instance NFData BatchStopJobRunSuccessfulSubmission
         where

-- | Specifies a table definition in the Data Catalog.
--
--
--
-- /See:/ 'catalogEntry' smart constructor.
data CatalogEntry = CatalogEntry'
  { _ceDatabaseName :: !Text
  , _ceTableName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CatalogEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceDatabaseName' - The database in which the table metadata resides.
--
-- * 'ceTableName' - The name of the table in question.
catalogEntry
    :: Text -- ^ 'ceDatabaseName'
    -> Text -- ^ 'ceTableName'
    -> CatalogEntry
catalogEntry pDatabaseName_ pTableName_ =
  CatalogEntry' {_ceDatabaseName = pDatabaseName_, _ceTableName = pTableName_}


-- | The database in which the table metadata resides.
ceDatabaseName :: Lens' CatalogEntry Text
ceDatabaseName = lens _ceDatabaseName (\ s a -> s{_ceDatabaseName = a})

-- | The name of the table in question.
ceTableName :: Lens' CatalogEntry Text
ceTableName = lens _ceTableName (\ s a -> s{_ceTableName = a})

instance Hashable CatalogEntry where

instance NFData CatalogEntry where

instance ToJSON CatalogEntry where
        toJSON CatalogEntry'{..}
          = object
              (catMaybes
                 [Just ("DatabaseName" .= _ceDatabaseName),
                  Just ("TableName" .= _ceTableName)])

-- | A structure containing migration status information.
--
--
--
-- /See:/ 'catalogImportStatus' smart constructor.
data CatalogImportStatus = CatalogImportStatus'
  { _cisImportedBy      :: !(Maybe Text)
  , _cisImportTime      :: !(Maybe POSIX)
  , _cisImportCompleted :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CatalogImportStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cisImportedBy' - The name of the person who initiated the migration.
--
-- * 'cisImportTime' - The time that the migration was started.
--
-- * 'cisImportCompleted' - True if the migration has completed, or False otherwise.
catalogImportStatus
    :: CatalogImportStatus
catalogImportStatus =
  CatalogImportStatus'
    { _cisImportedBy = Nothing
    , _cisImportTime = Nothing
    , _cisImportCompleted = Nothing
    }


-- | The name of the person who initiated the migration.
cisImportedBy :: Lens' CatalogImportStatus (Maybe Text)
cisImportedBy = lens _cisImportedBy (\ s a -> s{_cisImportedBy = a})

-- | The time that the migration was started.
cisImportTime :: Lens' CatalogImportStatus (Maybe UTCTime)
cisImportTime = lens _cisImportTime (\ s a -> s{_cisImportTime = a}) . mapping _Time

-- | True if the migration has completed, or False otherwise.
cisImportCompleted :: Lens' CatalogImportStatus (Maybe Bool)
cisImportCompleted = lens _cisImportCompleted (\ s a -> s{_cisImportCompleted = a})

instance FromJSON CatalogImportStatus where
        parseJSON
          = withObject "CatalogImportStatus"
              (\ x ->
                 CatalogImportStatus' <$>
                   (x .:? "ImportedBy") <*> (x .:? "ImportTime") <*>
                     (x .:? "ImportCompleted"))

instance Hashable CatalogImportStatus where

instance NFData CatalogImportStatus where

-- | Classifiers are written in Python and triggered during a crawl task. You can write your own classifiers to best categorize your data sources and specify the appropriate schemas to use for them. A classifier checks whether a given file is in a format it can handle, and if it is, the classifier creates a schema in the form of a @StructType@ object that matches that data format.
--
--
-- A classifier can be a @grok@ classifier, an XML classifier, or a JSON classifier, asspecified in one of the fields in the @Classifier@ object.
--
--
-- /See:/ 'classifier' smart constructor.
data Classifier = Classifier'
  { _cGrokClassifier :: !(Maybe GrokClassifier)
  , _cXMLClassifier  :: !(Maybe XMLClassifier)
  , _cJSONClassifier :: !(Maybe JSONClassifier)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Classifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cGrokClassifier' - A @GrokClassifier@ object.
--
-- * 'cXMLClassifier' - An @XMLClassifier@ object.
--
-- * 'cJSONClassifier' - A @JsonClassifier@ object.
classifier
    :: Classifier
classifier =
  Classifier'
    { _cGrokClassifier = Nothing
    , _cXMLClassifier = Nothing
    , _cJSONClassifier = Nothing
    }


-- | A @GrokClassifier@ object.
cGrokClassifier :: Lens' Classifier (Maybe GrokClassifier)
cGrokClassifier = lens _cGrokClassifier (\ s a -> s{_cGrokClassifier = a})

-- | An @XMLClassifier@ object.
cXMLClassifier :: Lens' Classifier (Maybe XMLClassifier)
cXMLClassifier = lens _cXMLClassifier (\ s a -> s{_cXMLClassifier = a})

-- | A @JsonClassifier@ object.
cJSONClassifier :: Lens' Classifier (Maybe JSONClassifier)
cJSONClassifier = lens _cJSONClassifier (\ s a -> s{_cJSONClassifier = a})

instance FromJSON Classifier where
        parseJSON
          = withObject "Classifier"
              (\ x ->
                 Classifier' <$>
                   (x .:? "GrokClassifier") <*> (x .:? "XMLClassifier")
                     <*> (x .:? "JsonClassifier"))

instance Hashable Classifier where

instance NFData Classifier where

-- | Represents a directional edge in a directed acyclic graph (DAG).
--
--
--
-- /See:/ 'codeGenEdge' smart constructor.
data CodeGenEdge = CodeGenEdge'
  { _cgeTargetParameter :: !(Maybe Text)
  , _cgeSource          :: !Text
  , _cgeTarget          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CodeGenEdge' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgeTargetParameter' - The target of the edge.
--
-- * 'cgeSource' - The ID of the node at which the edge starts.
--
-- * 'cgeTarget' - The ID of the node at which the edge ends.
codeGenEdge
    :: Text -- ^ 'cgeSource'
    -> Text -- ^ 'cgeTarget'
    -> CodeGenEdge
codeGenEdge pSource_ pTarget_ =
  CodeGenEdge'
    { _cgeTargetParameter = Nothing
    , _cgeSource = pSource_
    , _cgeTarget = pTarget_
    }


-- | The target of the edge.
cgeTargetParameter :: Lens' CodeGenEdge (Maybe Text)
cgeTargetParameter = lens _cgeTargetParameter (\ s a -> s{_cgeTargetParameter = a})

-- | The ID of the node at which the edge starts.
cgeSource :: Lens' CodeGenEdge Text
cgeSource = lens _cgeSource (\ s a -> s{_cgeSource = a})

-- | The ID of the node at which the edge ends.
cgeTarget :: Lens' CodeGenEdge Text
cgeTarget = lens _cgeTarget (\ s a -> s{_cgeTarget = a})

instance FromJSON CodeGenEdge where
        parseJSON
          = withObject "CodeGenEdge"
              (\ x ->
                 CodeGenEdge' <$>
                   (x .:? "TargetParameter") <*> (x .: "Source") <*>
                     (x .: "Target"))

instance Hashable CodeGenEdge where

instance NFData CodeGenEdge where

instance ToJSON CodeGenEdge where
        toJSON CodeGenEdge'{..}
          = object
              (catMaybes
                 [("TargetParameter" .=) <$> _cgeTargetParameter,
                  Just ("Source" .= _cgeSource),
                  Just ("Target" .= _cgeTarget)])

-- | Represents a node in a directed acyclic graph (DAG)
--
--
--
-- /See:/ 'codeGenNode' smart constructor.
data CodeGenNode = CodeGenNode'
  { _cgnLineNumber :: !(Maybe Int)
  , _cgnId         :: !Text
  , _cgnNodeType   :: !Text
  , _cgnArgs       :: ![CodeGenNodeArg]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CodeGenNode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgnLineNumber' - The line number of the node.
--
-- * 'cgnId' - A node identifier that is unique within the node's graph.
--
-- * 'cgnNodeType' - The type of node this is.
--
-- * 'cgnArgs' - Properties of the node, in the form of name-value pairs.
codeGenNode
    :: Text -- ^ 'cgnId'
    -> Text -- ^ 'cgnNodeType'
    -> CodeGenNode
codeGenNode pId_ pNodeType_ =
  CodeGenNode'
    { _cgnLineNumber = Nothing
    , _cgnId = pId_
    , _cgnNodeType = pNodeType_
    , _cgnArgs = mempty
    }


-- | The line number of the node.
cgnLineNumber :: Lens' CodeGenNode (Maybe Int)
cgnLineNumber = lens _cgnLineNumber (\ s a -> s{_cgnLineNumber = a})

-- | A node identifier that is unique within the node's graph.
cgnId :: Lens' CodeGenNode Text
cgnId = lens _cgnId (\ s a -> s{_cgnId = a})

-- | The type of node this is.
cgnNodeType :: Lens' CodeGenNode Text
cgnNodeType = lens _cgnNodeType (\ s a -> s{_cgnNodeType = a})

-- | Properties of the node, in the form of name-value pairs.
cgnArgs :: Lens' CodeGenNode [CodeGenNodeArg]
cgnArgs = lens _cgnArgs (\ s a -> s{_cgnArgs = a}) . _Coerce

instance FromJSON CodeGenNode where
        parseJSON
          = withObject "CodeGenNode"
              (\ x ->
                 CodeGenNode' <$>
                   (x .:? "LineNumber") <*> (x .: "Id") <*>
                     (x .: "NodeType")
                     <*> (x .:? "Args" .!= mempty))

instance Hashable CodeGenNode where

instance NFData CodeGenNode where

instance ToJSON CodeGenNode where
        toJSON CodeGenNode'{..}
          = object
              (catMaybes
                 [("LineNumber" .=) <$> _cgnLineNumber,
                  Just ("Id" .= _cgnId),
                  Just ("NodeType" .= _cgnNodeType),
                  Just ("Args" .= _cgnArgs)])

-- | An argument or property of a node.
--
--
--
-- /See:/ 'codeGenNodeArg' smart constructor.
data CodeGenNodeArg = CodeGenNodeArg'
  { _cgnaParam :: !(Maybe Bool)
  , _cgnaName  :: !Text
  , _cgnaValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CodeGenNodeArg' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgnaParam' - True if the value is used as a parameter.
--
-- * 'cgnaName' - The name of the argument or property.
--
-- * 'cgnaValue' - The value of the argument or property.
codeGenNodeArg
    :: Text -- ^ 'cgnaName'
    -> Text -- ^ 'cgnaValue'
    -> CodeGenNodeArg
codeGenNodeArg pName_ pValue_ =
  CodeGenNodeArg'
    {_cgnaParam = Nothing, _cgnaName = pName_, _cgnaValue = pValue_}


-- | True if the value is used as a parameter.
cgnaParam :: Lens' CodeGenNodeArg (Maybe Bool)
cgnaParam = lens _cgnaParam (\ s a -> s{_cgnaParam = a})

-- | The name of the argument or property.
cgnaName :: Lens' CodeGenNodeArg Text
cgnaName = lens _cgnaName (\ s a -> s{_cgnaName = a})

-- | The value of the argument or property.
cgnaValue :: Lens' CodeGenNodeArg Text
cgnaValue = lens _cgnaValue (\ s a -> s{_cgnaValue = a})

instance FromJSON CodeGenNodeArg where
        parseJSON
          = withObject "CodeGenNodeArg"
              (\ x ->
                 CodeGenNodeArg' <$>
                   (x .:? "Param") <*> (x .: "Name") <*> (x .: "Value"))

instance Hashable CodeGenNodeArg where

instance NFData CodeGenNodeArg where

instance ToJSON CodeGenNodeArg where
        toJSON CodeGenNodeArg'{..}
          = object
              (catMaybes
                 [("Param" .=) <$> _cgnaParam,
                  Just ("Name" .= _cgnaName),
                  Just ("Value" .= _cgnaValue)])

-- | A column in a @Table@ .
--
--
--
-- /See:/ 'column' smart constructor.
data Column = Column'
  { _cType    :: !(Maybe Text)
  , _cComment :: !(Maybe Text)
  , _cName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Column' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cType' - The datatype of data in the @Column@ .
--
-- * 'cComment' - Free-form text comment.
--
-- * 'cName' - The name of the @Column@ .
column
    :: Text -- ^ 'cName'
    -> Column
column pName_ = Column' {_cType = Nothing, _cComment = Nothing, _cName = pName_}


-- | The datatype of data in the @Column@ .
cType :: Lens' Column (Maybe Text)
cType = lens _cType (\ s a -> s{_cType = a})

-- | Free-form text comment.
cComment :: Lens' Column (Maybe Text)
cComment = lens _cComment (\ s a -> s{_cComment = a})

-- | The name of the @Column@ .
cName :: Lens' Column Text
cName = lens _cName (\ s a -> s{_cName = a})

instance FromJSON Column where
        parseJSON
          = withObject "Column"
              (\ x ->
                 Column' <$>
                   (x .:? "Type") <*> (x .:? "Comment") <*>
                     (x .: "Name"))

instance Hashable Column where

instance NFData Column where

instance ToJSON Column where
        toJSON Column'{..}
          = object
              (catMaybes
                 [("Type" .=) <$> _cType,
                  ("Comment" .=) <$> _cComment,
                  Just ("Name" .= _cName)])

-- | Defines a condition under which a trigger fires.
--
--
--
-- /See:/ 'condition' smart constructor.
data Condition = Condition'
  { _cState           :: !(Maybe JobRunState)
  , _cJobName         :: !(Maybe Text)
  , _cLogicalOperator :: !(Maybe LogicalOperator)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Condition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cState' - The condition state. Currently, the values supported are SUCCEEDED, STOPPED, TIMEOUT and FAILED.
--
-- * 'cJobName' - The name of the Job to whose JobRuns this condition applies and on which this trigger waits.
--
-- * 'cLogicalOperator' - A logical operator.
condition
    :: Condition
condition =
  Condition'
    {_cState = Nothing, _cJobName = Nothing, _cLogicalOperator = Nothing}


-- | The condition state. Currently, the values supported are SUCCEEDED, STOPPED, TIMEOUT and FAILED.
cState :: Lens' Condition (Maybe JobRunState)
cState = lens _cState (\ s a -> s{_cState = a})

-- | The name of the Job to whose JobRuns this condition applies and on which this trigger waits.
cJobName :: Lens' Condition (Maybe Text)
cJobName = lens _cJobName (\ s a -> s{_cJobName = a})

-- | A logical operator.
cLogicalOperator :: Lens' Condition (Maybe LogicalOperator)
cLogicalOperator = lens _cLogicalOperator (\ s a -> s{_cLogicalOperator = a})

instance FromJSON Condition where
        parseJSON
          = withObject "Condition"
              (\ x ->
                 Condition' <$>
                   (x .:? "State") <*> (x .:? "JobName") <*>
                     (x .:? "LogicalOperator"))

instance Hashable Condition where

instance NFData Condition where

instance ToJSON Condition where
        toJSON Condition'{..}
          = object
              (catMaybes
                 [("State" .=) <$> _cState,
                  ("JobName" .=) <$> _cJobName,
                  ("LogicalOperator" .=) <$> _cLogicalOperator])

-- | Defines a connection to a data source.
--
--
--
-- /See:/ 'connection' smart constructor.
data Connection = Connection'
  { _conCreationTime :: !(Maybe POSIX)
  , _conLastUpdatedBy :: !(Maybe Text)
  , _conConnectionProperties :: !(Maybe (Map ConnectionPropertyKey Text))
  , _conLastUpdatedTime :: !(Maybe POSIX)
  , _conMatchCriteria :: !(Maybe [Text])
  , _conPhysicalConnectionRequirements :: !(Maybe PhysicalConnectionRequirements)
  , _conName :: !(Maybe Text)
  , _conDescription :: !(Maybe Text)
  , _conConnectionType :: !(Maybe ConnectionType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Connection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'conCreationTime' - The time this connection definition was created.
--
-- * 'conLastUpdatedBy' - The user, group or role that last updated this connection definition.
--
-- * 'conConnectionProperties' - A list of key-value pairs used as parameters for this connection.
--
-- * 'conLastUpdatedTime' - The last time this connection definition was updated.
--
-- * 'conMatchCriteria' - A list of criteria that can be used in selecting this connection.
--
-- * 'conPhysicalConnectionRequirements' - A map of physical connection requirements, such as VPC and SecurityGroup, needed for making this connection successfully.
--
-- * 'conName' - The name of the connection definition.
--
-- * 'conDescription' - Description of the connection.
--
-- * 'conConnectionType' - The type of the connection. Currently, only JDBC is supported; SFTP is not supported.
connection
    :: Connection
connection =
  Connection'
    { _conCreationTime = Nothing
    , _conLastUpdatedBy = Nothing
    , _conConnectionProperties = Nothing
    , _conLastUpdatedTime = Nothing
    , _conMatchCriteria = Nothing
    , _conPhysicalConnectionRequirements = Nothing
    , _conName = Nothing
    , _conDescription = Nothing
    , _conConnectionType = Nothing
    }


-- | The time this connection definition was created.
conCreationTime :: Lens' Connection (Maybe UTCTime)
conCreationTime = lens _conCreationTime (\ s a -> s{_conCreationTime = a}) . mapping _Time

-- | The user, group or role that last updated this connection definition.
conLastUpdatedBy :: Lens' Connection (Maybe Text)
conLastUpdatedBy = lens _conLastUpdatedBy (\ s a -> s{_conLastUpdatedBy = a})

-- | A list of key-value pairs used as parameters for this connection.
conConnectionProperties :: Lens' Connection (HashMap ConnectionPropertyKey Text)
conConnectionProperties = lens _conConnectionProperties (\ s a -> s{_conConnectionProperties = a}) . _Default . _Map

-- | The last time this connection definition was updated.
conLastUpdatedTime :: Lens' Connection (Maybe UTCTime)
conLastUpdatedTime = lens _conLastUpdatedTime (\ s a -> s{_conLastUpdatedTime = a}) . mapping _Time

-- | A list of criteria that can be used in selecting this connection.
conMatchCriteria :: Lens' Connection [Text]
conMatchCriteria = lens _conMatchCriteria (\ s a -> s{_conMatchCriteria = a}) . _Default . _Coerce

-- | A map of physical connection requirements, such as VPC and SecurityGroup, needed for making this connection successfully.
conPhysicalConnectionRequirements :: Lens' Connection (Maybe PhysicalConnectionRequirements)
conPhysicalConnectionRequirements = lens _conPhysicalConnectionRequirements (\ s a -> s{_conPhysicalConnectionRequirements = a})

-- | The name of the connection definition.
conName :: Lens' Connection (Maybe Text)
conName = lens _conName (\ s a -> s{_conName = a})

-- | Description of the connection.
conDescription :: Lens' Connection (Maybe Text)
conDescription = lens _conDescription (\ s a -> s{_conDescription = a})

-- | The type of the connection. Currently, only JDBC is supported; SFTP is not supported.
conConnectionType :: Lens' Connection (Maybe ConnectionType)
conConnectionType = lens _conConnectionType (\ s a -> s{_conConnectionType = a})

instance FromJSON Connection where
        parseJSON
          = withObject "Connection"
              (\ x ->
                 Connection' <$>
                   (x .:? "CreationTime") <*> (x .:? "LastUpdatedBy")
                     <*> (x .:? "ConnectionProperties" .!= mempty)
                     <*> (x .:? "LastUpdatedTime")
                     <*> (x .:? "MatchCriteria" .!= mempty)
                     <*> (x .:? "PhysicalConnectionRequirements")
                     <*> (x .:? "Name")
                     <*> (x .:? "Description")
                     <*> (x .:? "ConnectionType"))

instance Hashable Connection where

instance NFData Connection where

-- | A structure used to specify a connection to create or update.
--
--
--
-- /See:/ 'connectionInput' smart constructor.
data ConnectionInput = ConnectionInput'
  { _ciMatchCriteria                  :: !(Maybe [Text])
  , _ciPhysicalConnectionRequirements :: !(Maybe PhysicalConnectionRequirements)
  , _ciDescription                    :: !(Maybe Text)
  , _ciName                           :: !Text
  , _ciConnectionType                 :: !ConnectionType
  , _ciConnectionProperties           :: !(Map ConnectionPropertyKey Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConnectionInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciMatchCriteria' - A list of criteria that can be used in selecting this connection.
--
-- * 'ciPhysicalConnectionRequirements' - A map of physical connection requirements, such as VPC and SecurityGroup, needed for making this connection successfully.
--
-- * 'ciDescription' - Description of the connection.
--
-- * 'ciName' - The name of the connection.
--
-- * 'ciConnectionType' - The type of the connection. Currently, only JDBC is supported; SFTP is not supported.
--
-- * 'ciConnectionProperties' - A list of key-value pairs used as parameters for this connection.
connectionInput
    :: Text -- ^ 'ciName'
    -> ConnectionType -- ^ 'ciConnectionType'
    -> ConnectionInput
connectionInput pName_ pConnectionType_ =
  ConnectionInput'
    { _ciMatchCriteria = Nothing
    , _ciPhysicalConnectionRequirements = Nothing
    , _ciDescription = Nothing
    , _ciName = pName_
    , _ciConnectionType = pConnectionType_
    , _ciConnectionProperties = mempty
    }


-- | A list of criteria that can be used in selecting this connection.
ciMatchCriteria :: Lens' ConnectionInput [Text]
ciMatchCriteria = lens _ciMatchCriteria (\ s a -> s{_ciMatchCriteria = a}) . _Default . _Coerce

-- | A map of physical connection requirements, such as VPC and SecurityGroup, needed for making this connection successfully.
ciPhysicalConnectionRequirements :: Lens' ConnectionInput (Maybe PhysicalConnectionRequirements)
ciPhysicalConnectionRequirements = lens _ciPhysicalConnectionRequirements (\ s a -> s{_ciPhysicalConnectionRequirements = a})

-- | Description of the connection.
ciDescription :: Lens' ConnectionInput (Maybe Text)
ciDescription = lens _ciDescription (\ s a -> s{_ciDescription = a})

-- | The name of the connection.
ciName :: Lens' ConnectionInput Text
ciName = lens _ciName (\ s a -> s{_ciName = a})

-- | The type of the connection. Currently, only JDBC is supported; SFTP is not supported.
ciConnectionType :: Lens' ConnectionInput ConnectionType
ciConnectionType = lens _ciConnectionType (\ s a -> s{_ciConnectionType = a})

-- | A list of key-value pairs used as parameters for this connection.
ciConnectionProperties :: Lens' ConnectionInput (HashMap ConnectionPropertyKey Text)
ciConnectionProperties = lens _ciConnectionProperties (\ s a -> s{_ciConnectionProperties = a}) . _Map

instance Hashable ConnectionInput where

instance NFData ConnectionInput where

instance ToJSON ConnectionInput where
        toJSON ConnectionInput'{..}
          = object
              (catMaybes
                 [("MatchCriteria" .=) <$> _ciMatchCriteria,
                  ("PhysicalConnectionRequirements" .=) <$>
                    _ciPhysicalConnectionRequirements,
                  ("Description" .=) <$> _ciDescription,
                  Just ("Name" .= _ciName),
                  Just ("ConnectionType" .= _ciConnectionType),
                  Just
                    ("ConnectionProperties" .= _ciConnectionProperties)])

-- | Specifies the connections used by a job.
--
--
--
-- /See:/ 'connectionsList' smart constructor.
newtype ConnectionsList = ConnectionsList'
  { _clConnections :: Maybe [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConnectionsList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clConnections' - A list of connections used by the job.
connectionsList
    :: ConnectionsList
connectionsList = ConnectionsList' {_clConnections = Nothing}


-- | A list of connections used by the job.
clConnections :: Lens' ConnectionsList [Text]
clConnections = lens _clConnections (\ s a -> s{_clConnections = a}) . _Default . _Coerce

instance FromJSON ConnectionsList where
        parseJSON
          = withObject "ConnectionsList"
              (\ x ->
                 ConnectionsList' <$>
                   (x .:? "Connections" .!= mempty))

instance Hashable ConnectionsList where

instance NFData ConnectionsList where

instance ToJSON ConnectionsList where
        toJSON ConnectionsList'{..}
          = object
              (catMaybes [("Connections" .=) <$> _clConnections])

-- | Specifies a crawler program that examines a data source and uses classifiers to try to determine its schema. If successful, the crawler records metadata concerning the data source in the AWS Glue Data Catalog.
--
--
--
-- /See:/ 'crawler' smart constructor.
data Crawler = Crawler'
  { _craCreationTime       :: !(Maybe POSIX)
  , _craState              :: !(Maybe CrawlerState)
  , _craSchemaChangePolicy :: !(Maybe SchemaChangePolicy)
  , _craLastUpdated        :: !(Maybe POSIX)
  , _craSchedule           :: !(Maybe Schedule)
  , _craLastCrawl          :: !(Maybe LastCrawlInfo)
  , _craCrawlElapsedTime   :: !(Maybe Integer)
  , _craClassifiers        :: !(Maybe [Text])
  , _craRole               :: !(Maybe Text)
  , _craName               :: !(Maybe Text)
  , _craTargets            :: !(Maybe CrawlerTargets)
  , _craVersion            :: !(Maybe Integer)
  , _craDatabaseName       :: !(Maybe Text)
  , _craConfiguration      :: !(Maybe Text)
  , _craTablePrefix        :: !(Maybe Text)
  , _craDescription        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Crawler' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'craCreationTime' - The time when the crawler was created.
--
-- * 'craState' - Indicates whether the crawler is running, or whether a run is pending.
--
-- * 'craSchemaChangePolicy' - Sets the behavior when the crawler finds a changed or deleted object.
--
-- * 'craLastUpdated' - The time the crawler was last updated.
--
-- * 'craSchedule' - For scheduled crawlers, the schedule when the crawler runs.
--
-- * 'craLastCrawl' - The status of the last crawl, and potentially error information if an error occurred.
--
-- * 'craCrawlElapsedTime' - If the crawler is running, contains the total time elapsed since the last crawl began.
--
-- * 'craClassifiers' - A list of custom classifiers associated with the crawler.
--
-- * 'craRole' - The IAM role (or ARN of an IAM role) used to access customer resources, such as data in Amazon S3.
--
-- * 'craName' - The crawler name.
--
-- * 'craTargets' - A collection of targets to crawl.
--
-- * 'craVersion' - The version of the crawler.
--
-- * 'craDatabaseName' - The database where metadata is written by this crawler.
--
-- * 'craConfiguration' - Crawler configuration information. This versioned JSON string allows users to specify aspects of a Crawler's behavior. You can use this field to force partitions to inherit metadata such as classification, input format, output format, serde information, and schema from their parent table, rather than detect this information separately for each partition. Use the following JSON string to specify that behavior: Example: @'{ "Version": 1.0, "CrawlerOutput": { "Partitions": { "AddOrUpdateBehavior": "InheritFromTable" } } }'@
--
-- * 'craTablePrefix' - The prefix added to the names of tables that are created.
--
-- * 'craDescription' - A description of the crawler.
crawler
    :: Crawler
crawler =
  Crawler'
    { _craCreationTime = Nothing
    , _craState = Nothing
    , _craSchemaChangePolicy = Nothing
    , _craLastUpdated = Nothing
    , _craSchedule = Nothing
    , _craLastCrawl = Nothing
    , _craCrawlElapsedTime = Nothing
    , _craClassifiers = Nothing
    , _craRole = Nothing
    , _craName = Nothing
    , _craTargets = Nothing
    , _craVersion = Nothing
    , _craDatabaseName = Nothing
    , _craConfiguration = Nothing
    , _craTablePrefix = Nothing
    , _craDescription = Nothing
    }


-- | The time when the crawler was created.
craCreationTime :: Lens' Crawler (Maybe UTCTime)
craCreationTime = lens _craCreationTime (\ s a -> s{_craCreationTime = a}) . mapping _Time

-- | Indicates whether the crawler is running, or whether a run is pending.
craState :: Lens' Crawler (Maybe CrawlerState)
craState = lens _craState (\ s a -> s{_craState = a})

-- | Sets the behavior when the crawler finds a changed or deleted object.
craSchemaChangePolicy :: Lens' Crawler (Maybe SchemaChangePolicy)
craSchemaChangePolicy = lens _craSchemaChangePolicy (\ s a -> s{_craSchemaChangePolicy = a})

-- | The time the crawler was last updated.
craLastUpdated :: Lens' Crawler (Maybe UTCTime)
craLastUpdated = lens _craLastUpdated (\ s a -> s{_craLastUpdated = a}) . mapping _Time

-- | For scheduled crawlers, the schedule when the crawler runs.
craSchedule :: Lens' Crawler (Maybe Schedule)
craSchedule = lens _craSchedule (\ s a -> s{_craSchedule = a})

-- | The status of the last crawl, and potentially error information if an error occurred.
craLastCrawl :: Lens' Crawler (Maybe LastCrawlInfo)
craLastCrawl = lens _craLastCrawl (\ s a -> s{_craLastCrawl = a})

-- | If the crawler is running, contains the total time elapsed since the last crawl began.
craCrawlElapsedTime :: Lens' Crawler (Maybe Integer)
craCrawlElapsedTime = lens _craCrawlElapsedTime (\ s a -> s{_craCrawlElapsedTime = a})

-- | A list of custom classifiers associated with the crawler.
craClassifiers :: Lens' Crawler [Text]
craClassifiers = lens _craClassifiers (\ s a -> s{_craClassifiers = a}) . _Default . _Coerce

-- | The IAM role (or ARN of an IAM role) used to access customer resources, such as data in Amazon S3.
craRole :: Lens' Crawler (Maybe Text)
craRole = lens _craRole (\ s a -> s{_craRole = a})

-- | The crawler name.
craName :: Lens' Crawler (Maybe Text)
craName = lens _craName (\ s a -> s{_craName = a})

-- | A collection of targets to crawl.
craTargets :: Lens' Crawler (Maybe CrawlerTargets)
craTargets = lens _craTargets (\ s a -> s{_craTargets = a})

-- | The version of the crawler.
craVersion :: Lens' Crawler (Maybe Integer)
craVersion = lens _craVersion (\ s a -> s{_craVersion = a})

-- | The database where metadata is written by this crawler.
craDatabaseName :: Lens' Crawler (Maybe Text)
craDatabaseName = lens _craDatabaseName (\ s a -> s{_craDatabaseName = a})

-- | Crawler configuration information. This versioned JSON string allows users to specify aspects of a Crawler's behavior. You can use this field to force partitions to inherit metadata such as classification, input format, output format, serde information, and schema from their parent table, rather than detect this information separately for each partition. Use the following JSON string to specify that behavior: Example: @'{ "Version": 1.0, "CrawlerOutput": { "Partitions": { "AddOrUpdateBehavior": "InheritFromTable" } } }'@
craConfiguration :: Lens' Crawler (Maybe Text)
craConfiguration = lens _craConfiguration (\ s a -> s{_craConfiguration = a})

-- | The prefix added to the names of tables that are created.
craTablePrefix :: Lens' Crawler (Maybe Text)
craTablePrefix = lens _craTablePrefix (\ s a -> s{_craTablePrefix = a})

-- | A description of the crawler.
craDescription :: Lens' Crawler (Maybe Text)
craDescription = lens _craDescription (\ s a -> s{_craDescription = a})

instance FromJSON Crawler where
        parseJSON
          = withObject "Crawler"
              (\ x ->
                 Crawler' <$>
                   (x .:? "CreationTime") <*> (x .:? "State") <*>
                     (x .:? "SchemaChangePolicy")
                     <*> (x .:? "LastUpdated")
                     <*> (x .:? "Schedule")
                     <*> (x .:? "LastCrawl")
                     <*> (x .:? "CrawlElapsedTime")
                     <*> (x .:? "Classifiers" .!= mempty)
                     <*> (x .:? "Role")
                     <*> (x .:? "Name")
                     <*> (x .:? "Targets")
                     <*> (x .:? "Version")
                     <*> (x .:? "DatabaseName")
                     <*> (x .:? "Configuration")
                     <*> (x .:? "TablePrefix")
                     <*> (x .:? "Description"))

instance Hashable Crawler where

instance NFData Crawler where

-- | Metrics for a specified crawler.
--
--
--
-- /See:/ 'crawlerMetrics' smart constructor.
data CrawlerMetrics = CrawlerMetrics'
  { _cmLastRuntimeSeconds   :: !(Maybe Double)
  , _cmTablesCreated        :: !(Maybe Nat)
  , _cmStillEstimating      :: !(Maybe Bool)
  , _cmMedianRuntimeSeconds :: !(Maybe Double)
  , _cmTimeLeftSeconds      :: !(Maybe Double)
  , _cmTablesDeleted        :: !(Maybe Nat)
  , _cmTablesUpdated        :: !(Maybe Nat)
  , _cmCrawlerName          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CrawlerMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmLastRuntimeSeconds' - The duration of the crawler's most recent run, in seconds.
--
-- * 'cmTablesCreated' - The number of tables created by this crawler.
--
-- * 'cmStillEstimating' - True if the crawler is still estimating how long it will take to complete this run.
--
-- * 'cmMedianRuntimeSeconds' - The median duration of this crawler's runs, in seconds.
--
-- * 'cmTimeLeftSeconds' - The estimated time left to complete a running crawl.
--
-- * 'cmTablesDeleted' - The number of tables deleted by this crawler.
--
-- * 'cmTablesUpdated' - The number of tables updated by this crawler.
--
-- * 'cmCrawlerName' - The name of the crawler.
crawlerMetrics
    :: CrawlerMetrics
crawlerMetrics =
  CrawlerMetrics'
    { _cmLastRuntimeSeconds = Nothing
    , _cmTablesCreated = Nothing
    , _cmStillEstimating = Nothing
    , _cmMedianRuntimeSeconds = Nothing
    , _cmTimeLeftSeconds = Nothing
    , _cmTablesDeleted = Nothing
    , _cmTablesUpdated = Nothing
    , _cmCrawlerName = Nothing
    }


-- | The duration of the crawler's most recent run, in seconds.
cmLastRuntimeSeconds :: Lens' CrawlerMetrics (Maybe Double)
cmLastRuntimeSeconds = lens _cmLastRuntimeSeconds (\ s a -> s{_cmLastRuntimeSeconds = a})

-- | The number of tables created by this crawler.
cmTablesCreated :: Lens' CrawlerMetrics (Maybe Natural)
cmTablesCreated = lens _cmTablesCreated (\ s a -> s{_cmTablesCreated = a}) . mapping _Nat

-- | True if the crawler is still estimating how long it will take to complete this run.
cmStillEstimating :: Lens' CrawlerMetrics (Maybe Bool)
cmStillEstimating = lens _cmStillEstimating (\ s a -> s{_cmStillEstimating = a})

-- | The median duration of this crawler's runs, in seconds.
cmMedianRuntimeSeconds :: Lens' CrawlerMetrics (Maybe Double)
cmMedianRuntimeSeconds = lens _cmMedianRuntimeSeconds (\ s a -> s{_cmMedianRuntimeSeconds = a})

-- | The estimated time left to complete a running crawl.
cmTimeLeftSeconds :: Lens' CrawlerMetrics (Maybe Double)
cmTimeLeftSeconds = lens _cmTimeLeftSeconds (\ s a -> s{_cmTimeLeftSeconds = a})

-- | The number of tables deleted by this crawler.
cmTablesDeleted :: Lens' CrawlerMetrics (Maybe Natural)
cmTablesDeleted = lens _cmTablesDeleted (\ s a -> s{_cmTablesDeleted = a}) . mapping _Nat

-- | The number of tables updated by this crawler.
cmTablesUpdated :: Lens' CrawlerMetrics (Maybe Natural)
cmTablesUpdated = lens _cmTablesUpdated (\ s a -> s{_cmTablesUpdated = a}) . mapping _Nat

-- | The name of the crawler.
cmCrawlerName :: Lens' CrawlerMetrics (Maybe Text)
cmCrawlerName = lens _cmCrawlerName (\ s a -> s{_cmCrawlerName = a})

instance FromJSON CrawlerMetrics where
        parseJSON
          = withObject "CrawlerMetrics"
              (\ x ->
                 CrawlerMetrics' <$>
                   (x .:? "LastRuntimeSeconds") <*>
                     (x .:? "TablesCreated")
                     <*> (x .:? "StillEstimating")
                     <*> (x .:? "MedianRuntimeSeconds")
                     <*> (x .:? "TimeLeftSeconds")
                     <*> (x .:? "TablesDeleted")
                     <*> (x .:? "TablesUpdated")
                     <*> (x .:? "CrawlerName"))

instance Hashable CrawlerMetrics where

instance NFData CrawlerMetrics where

-- | Specifies data stores to crawl.
--
--
--
-- /See:/ 'crawlerTargets' smart constructor.
data CrawlerTargets = CrawlerTargets'
  { _ctS3Targets   :: !(Maybe [S3Target])
  , _ctJdbcTargets :: !(Maybe [JdbcTarget])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CrawlerTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctS3Targets' - Specifies Amazon S3 targets.
--
-- * 'ctJdbcTargets' - Specifies JDBC targets.
crawlerTargets
    :: CrawlerTargets
crawlerTargets =
  CrawlerTargets' {_ctS3Targets = Nothing, _ctJdbcTargets = Nothing}


-- | Specifies Amazon S3 targets.
ctS3Targets :: Lens' CrawlerTargets [S3Target]
ctS3Targets = lens _ctS3Targets (\ s a -> s{_ctS3Targets = a}) . _Default . _Coerce

-- | Specifies JDBC targets.
ctJdbcTargets :: Lens' CrawlerTargets [JdbcTarget]
ctJdbcTargets = lens _ctJdbcTargets (\ s a -> s{_ctJdbcTargets = a}) . _Default . _Coerce

instance FromJSON CrawlerTargets where
        parseJSON
          = withObject "CrawlerTargets"
              (\ x ->
                 CrawlerTargets' <$>
                   (x .:? "S3Targets" .!= mempty) <*>
                     (x .:? "JdbcTargets" .!= mempty))

instance Hashable CrawlerTargets where

instance NFData CrawlerTargets where

instance ToJSON CrawlerTargets where
        toJSON CrawlerTargets'{..}
          = object
              (catMaybes
                 [("S3Targets" .=) <$> _ctS3Targets,
                  ("JdbcTargets" .=) <$> _ctJdbcTargets])

-- | Specifies a @grok@ classifier for @CreateClassifier@ to create.
--
--
--
-- /See:/ 'createGrokClassifierRequest' smart constructor.
data CreateGrokClassifierRequest = CreateGrokClassifierRequest'
  { _cgcrCustomPatterns :: !(Maybe Text)
  , _cgcrClassification :: !Text
  , _cgcrName           :: !Text
  , _cgcrGrokPattern    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGrokClassifierRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgcrCustomPatterns' - Optional custom grok patterns used by this classifier.
--
-- * 'cgcrClassification' - An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
--
-- * 'cgcrName' - The name of the new classifier.
--
-- * 'cgcrGrokPattern' - The grok pattern used by this classifier.
createGrokClassifierRequest
    :: Text -- ^ 'cgcrClassification'
    -> Text -- ^ 'cgcrName'
    -> Text -- ^ 'cgcrGrokPattern'
    -> CreateGrokClassifierRequest
createGrokClassifierRequest pClassification_ pName_ pGrokPattern_ =
  CreateGrokClassifierRequest'
    { _cgcrCustomPatterns = Nothing
    , _cgcrClassification = pClassification_
    , _cgcrName = pName_
    , _cgcrGrokPattern = pGrokPattern_
    }


-- | Optional custom grok patterns used by this classifier.
cgcrCustomPatterns :: Lens' CreateGrokClassifierRequest (Maybe Text)
cgcrCustomPatterns = lens _cgcrCustomPatterns (\ s a -> s{_cgcrCustomPatterns = a})

-- | An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
cgcrClassification :: Lens' CreateGrokClassifierRequest Text
cgcrClassification = lens _cgcrClassification (\ s a -> s{_cgcrClassification = a})

-- | The name of the new classifier.
cgcrName :: Lens' CreateGrokClassifierRequest Text
cgcrName = lens _cgcrName (\ s a -> s{_cgcrName = a})

-- | The grok pattern used by this classifier.
cgcrGrokPattern :: Lens' CreateGrokClassifierRequest Text
cgcrGrokPattern = lens _cgcrGrokPattern (\ s a -> s{_cgcrGrokPattern = a})

instance Hashable CreateGrokClassifierRequest where

instance NFData CreateGrokClassifierRequest where

instance ToJSON CreateGrokClassifierRequest where
        toJSON CreateGrokClassifierRequest'{..}
          = object
              (catMaybes
                 [("CustomPatterns" .=) <$> _cgcrCustomPatterns,
                  Just ("Classification" .= _cgcrClassification),
                  Just ("Name" .= _cgcrName),
                  Just ("GrokPattern" .= _cgcrGrokPattern)])

-- | Specifies a JSON classifier for @CreateClassifier@ to create.
--
--
--
-- /See:/ 'createJSONClassifierRequest' smart constructor.
data CreateJSONClassifierRequest = CreateJSONClassifierRequest'
  { _cjcrName     :: !Text
  , _cjcrJSONPath :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateJSONClassifierRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjcrName' - The name of the classifier.
--
-- * 'cjcrJSONPath' - A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
createJSONClassifierRequest
    :: Text -- ^ 'cjcrName'
    -> Text -- ^ 'cjcrJSONPath'
    -> CreateJSONClassifierRequest
createJSONClassifierRequest pName_ pJSONPath_ =
  CreateJSONClassifierRequest' {_cjcrName = pName_, _cjcrJSONPath = pJSONPath_}


-- | The name of the classifier.
cjcrName :: Lens' CreateJSONClassifierRequest Text
cjcrName = lens _cjcrName (\ s a -> s{_cjcrName = a})

-- | A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
cjcrJSONPath :: Lens' CreateJSONClassifierRequest Text
cjcrJSONPath = lens _cjcrJSONPath (\ s a -> s{_cjcrJSONPath = a})

instance Hashable CreateJSONClassifierRequest where

instance NFData CreateJSONClassifierRequest where

instance ToJSON CreateJSONClassifierRequest where
        toJSON CreateJSONClassifierRequest'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _cjcrName),
                  Just ("JsonPath" .= _cjcrJSONPath)])

-- | Specifies an XML classifier for @CreateClassifier@ to create.
--
--
--
-- /See:/ 'createXMLClassifierRequest' smart constructor.
data CreateXMLClassifierRequest = CreateXMLClassifierRequest'
  { _cxcrRowTag         :: !(Maybe Text)
  , _cxcrClassification :: !Text
  , _cxcrName           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateXMLClassifierRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cxcrRowTag' - The XML tag designating the element that contains each record in an XML document being parsed. Note that this cannot identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
--
-- * 'cxcrClassification' - An identifier of the data format that the classifier matches.
--
-- * 'cxcrName' - The name of the classifier.
createXMLClassifierRequest
    :: Text -- ^ 'cxcrClassification'
    -> Text -- ^ 'cxcrName'
    -> CreateXMLClassifierRequest
createXMLClassifierRequest pClassification_ pName_ =
  CreateXMLClassifierRequest'
    { _cxcrRowTag = Nothing
    , _cxcrClassification = pClassification_
    , _cxcrName = pName_
    }


-- | The XML tag designating the element that contains each record in an XML document being parsed. Note that this cannot identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
cxcrRowTag :: Lens' CreateXMLClassifierRequest (Maybe Text)
cxcrRowTag = lens _cxcrRowTag (\ s a -> s{_cxcrRowTag = a})

-- | An identifier of the data format that the classifier matches.
cxcrClassification :: Lens' CreateXMLClassifierRequest Text
cxcrClassification = lens _cxcrClassification (\ s a -> s{_cxcrClassification = a})

-- | The name of the classifier.
cxcrName :: Lens' CreateXMLClassifierRequest Text
cxcrName = lens _cxcrName (\ s a -> s{_cxcrName = a})

instance Hashable CreateXMLClassifierRequest where

instance NFData CreateXMLClassifierRequest where

instance ToJSON CreateXMLClassifierRequest where
        toJSON CreateXMLClassifierRequest'{..}
          = object
              (catMaybes
                 [("RowTag" .=) <$> _cxcrRowTag,
                  Just ("Classification" .= _cxcrClassification),
                  Just ("Name" .= _cxcrName)])

-- | The @Database@ object represents a logical grouping of tables that may reside in a Hive metastore or an RDBMS.
--
--
--
-- /See:/ 'database' smart constructor.
data Database = Database'
  { _dLocationURI :: !(Maybe Text)
  , _dParameters  :: !(Maybe (Map Text Text))
  , _dDescription :: !(Maybe Text)
  , _dCreateTime  :: !(Maybe POSIX)
  , _dName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Database' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dLocationURI' - The location of the database (for example, an HDFS path).
--
-- * 'dParameters' - A list of key-value pairs that define parameters and properties of the database.
--
-- * 'dDescription' - Description of the database.
--
-- * 'dCreateTime' - The time at which the metadata database was created in the catalog.
--
-- * 'dName' - Name of the database. For Hive compatibility, this is folded to lowercase when it is stored.
database
    :: Text -- ^ 'dName'
    -> Database
database pName_ =
  Database'
    { _dLocationURI = Nothing
    , _dParameters = Nothing
    , _dDescription = Nothing
    , _dCreateTime = Nothing
    , _dName = pName_
    }


-- | The location of the database (for example, an HDFS path).
dLocationURI :: Lens' Database (Maybe Text)
dLocationURI = lens _dLocationURI (\ s a -> s{_dLocationURI = a})

-- | A list of key-value pairs that define parameters and properties of the database.
dParameters :: Lens' Database (HashMap Text Text)
dParameters = lens _dParameters (\ s a -> s{_dParameters = a}) . _Default . _Map

-- | Description of the database.
dDescription :: Lens' Database (Maybe Text)
dDescription = lens _dDescription (\ s a -> s{_dDescription = a})

-- | The time at which the metadata database was created in the catalog.
dCreateTime :: Lens' Database (Maybe UTCTime)
dCreateTime = lens _dCreateTime (\ s a -> s{_dCreateTime = a}) . mapping _Time

-- | Name of the database. For Hive compatibility, this is folded to lowercase when it is stored.
dName :: Lens' Database Text
dName = lens _dName (\ s a -> s{_dName = a})

instance FromJSON Database where
        parseJSON
          = withObject "Database"
              (\ x ->
                 Database' <$>
                   (x .:? "LocationUri") <*>
                     (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "Description")
                     <*> (x .:? "CreateTime")
                     <*> (x .: "Name"))

instance Hashable Database where

instance NFData Database where

-- | The structure used to create or update a database.
--
--
--
-- /See:/ 'databaseInput' smart constructor.
data DatabaseInput = DatabaseInput'
  { _diLocationURI :: !(Maybe Text)
  , _diParameters  :: !(Maybe (Map Text Text))
  , _diDescription :: !(Maybe Text)
  , _diName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DatabaseInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diLocationURI' - The location of the database (for example, an HDFS path).
--
-- * 'diParameters' - A list of key-value pairs that define parameters and properties of the database.
--
-- * 'diDescription' - Description of the database
--
-- * 'diName' - Name of the database. For Hive compatibility, this is folded to lowercase when it is stored.
databaseInput
    :: Text -- ^ 'diName'
    -> DatabaseInput
databaseInput pName_ =
  DatabaseInput'
    { _diLocationURI = Nothing
    , _diParameters = Nothing
    , _diDescription = Nothing
    , _diName = pName_
    }


-- | The location of the database (for example, an HDFS path).
diLocationURI :: Lens' DatabaseInput (Maybe Text)
diLocationURI = lens _diLocationURI (\ s a -> s{_diLocationURI = a})

-- | A list of key-value pairs that define parameters and properties of the database.
diParameters :: Lens' DatabaseInput (HashMap Text Text)
diParameters = lens _diParameters (\ s a -> s{_diParameters = a}) . _Default . _Map

-- | Description of the database
diDescription :: Lens' DatabaseInput (Maybe Text)
diDescription = lens _diDescription (\ s a -> s{_diDescription = a})

-- | Name of the database. For Hive compatibility, this is folded to lowercase when it is stored.
diName :: Lens' DatabaseInput Text
diName = lens _diName (\ s a -> s{_diName = a})

instance Hashable DatabaseInput where

instance NFData DatabaseInput where

instance ToJSON DatabaseInput where
        toJSON DatabaseInput'{..}
          = object
              (catMaybes
                 [("LocationUri" .=) <$> _diLocationURI,
                  ("Parameters" .=) <$> _diParameters,
                  ("Description" .=) <$> _diDescription,
                  Just ("Name" .= _diName)])

-- | A development endpoint where a developer can remotely debug ETL scripts.
--
--
--
-- /See:/ 'devEndpoint' smart constructor.
data DevEndpoint = DevEndpoint'
  { _deStatus                             :: !(Maybe Text)
  , _deFailureReason                      :: !(Maybe Text)
  , _deEndpointName                       :: !(Maybe Text)
  , _deExtraPythonLibsS3Path              :: !(Maybe Text)
  , _deLastUpdateStatus                   :: !(Maybe Text)
  , _deSecurityGroupIds                   :: !(Maybe [Text])
  , _deLastModifiedTimestamp              :: !(Maybe POSIX)
  , _deVPCId                              :: !(Maybe Text)
  , _dePrivateAddress                     :: !(Maybe Text)
  , _dePublicKey                          :: !(Maybe Text)
  , _deSubnetId                           :: !(Maybe Text)
  , _deNumberOfNodes                      :: !(Maybe Int)
  , _dePublicAddress                      :: !(Maybe Text)
  , _deAvailabilityZone                   :: !(Maybe Text)
  , _deZeppelinRemoteSparkInterpreterPort :: !(Maybe Int)
  , _deExtraJARsS3Path                    :: !(Maybe Text)
  , _deCreatedTimestamp                   :: !(Maybe POSIX)
  , _deYarnEndpointAddress                :: !(Maybe Text)
  , _deRoleARN                            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DevEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deStatus' - The current status of this DevEndpoint.
--
-- * 'deFailureReason' - The reason for a current failure in this DevEndpoint.
--
-- * 'deEndpointName' - The name of the DevEndpoint.
--
-- * 'deExtraPythonLibsS3Path' - Path(s) to one or more Python libraries in an S3 bucket that should be loaded in your DevEndpoint. Multiple values must be complete paths separated by a comma. Please note that only pure Python libraries can currently be used on a DevEndpoint. Libraries that rely on C extensions, such as the <http://pandas.pydata.org/ pandas> Python data analysis library, are not yet supported.
--
-- * 'deLastUpdateStatus' - The status of the last update.
--
-- * 'deSecurityGroupIds' - A list of security group identifiers used in this DevEndpoint.
--
-- * 'deLastModifiedTimestamp' - The point in time at which this DevEndpoint was last modified.
--
-- * 'deVPCId' - The ID of the virtual private cloud (VPC) used by this DevEndpoint.
--
-- * 'dePrivateAddress' - The private address used by this DevEndpoint.
--
-- * 'dePublicKey' - The public key to be used by this DevEndpoint for authentication.
--
-- * 'deSubnetId' - The subnet ID for this DevEndpoint.
--
-- * 'deNumberOfNodes' - The number of AWS Glue Data Processing Units (DPUs) allocated to this DevEndpoint.
--
-- * 'dePublicAddress' - The public VPC address used by this DevEndpoint.
--
-- * 'deAvailabilityZone' - The AWS availability zone where this DevEndpoint is located.
--
-- * 'deZeppelinRemoteSparkInterpreterPort' - The Apache Zeppelin port for the remote Apache Spark interpreter.
--
-- * 'deExtraJARsS3Path' - Path to one or more Java Jars in an S3 bucket that should be loaded in your DevEndpoint. Please note that only pure Java/Scala libraries can currently be used on a DevEndpoint.
--
-- * 'deCreatedTimestamp' - The point in time at which this DevEndpoint was created.
--
-- * 'deYarnEndpointAddress' - The YARN endpoint address used by this DevEndpoint.
--
-- * 'deRoleARN' - The AWS ARN of the IAM role used in this DevEndpoint.
devEndpoint
    :: DevEndpoint
devEndpoint =
  DevEndpoint'
    { _deStatus = Nothing
    , _deFailureReason = Nothing
    , _deEndpointName = Nothing
    , _deExtraPythonLibsS3Path = Nothing
    , _deLastUpdateStatus = Nothing
    , _deSecurityGroupIds = Nothing
    , _deLastModifiedTimestamp = Nothing
    , _deVPCId = Nothing
    , _dePrivateAddress = Nothing
    , _dePublicKey = Nothing
    , _deSubnetId = Nothing
    , _deNumberOfNodes = Nothing
    , _dePublicAddress = Nothing
    , _deAvailabilityZone = Nothing
    , _deZeppelinRemoteSparkInterpreterPort = Nothing
    , _deExtraJARsS3Path = Nothing
    , _deCreatedTimestamp = Nothing
    , _deYarnEndpointAddress = Nothing
    , _deRoleARN = Nothing
    }


-- | The current status of this DevEndpoint.
deStatus :: Lens' DevEndpoint (Maybe Text)
deStatus = lens _deStatus (\ s a -> s{_deStatus = a})

-- | The reason for a current failure in this DevEndpoint.
deFailureReason :: Lens' DevEndpoint (Maybe Text)
deFailureReason = lens _deFailureReason (\ s a -> s{_deFailureReason = a})

-- | The name of the DevEndpoint.
deEndpointName :: Lens' DevEndpoint (Maybe Text)
deEndpointName = lens _deEndpointName (\ s a -> s{_deEndpointName = a})

-- | Path(s) to one or more Python libraries in an S3 bucket that should be loaded in your DevEndpoint. Multiple values must be complete paths separated by a comma. Please note that only pure Python libraries can currently be used on a DevEndpoint. Libraries that rely on C extensions, such as the <http://pandas.pydata.org/ pandas> Python data analysis library, are not yet supported.
deExtraPythonLibsS3Path :: Lens' DevEndpoint (Maybe Text)
deExtraPythonLibsS3Path = lens _deExtraPythonLibsS3Path (\ s a -> s{_deExtraPythonLibsS3Path = a})

-- | The status of the last update.
deLastUpdateStatus :: Lens' DevEndpoint (Maybe Text)
deLastUpdateStatus = lens _deLastUpdateStatus (\ s a -> s{_deLastUpdateStatus = a})

-- | A list of security group identifiers used in this DevEndpoint.
deSecurityGroupIds :: Lens' DevEndpoint [Text]
deSecurityGroupIds = lens _deSecurityGroupIds (\ s a -> s{_deSecurityGroupIds = a}) . _Default . _Coerce

-- | The point in time at which this DevEndpoint was last modified.
deLastModifiedTimestamp :: Lens' DevEndpoint (Maybe UTCTime)
deLastModifiedTimestamp = lens _deLastModifiedTimestamp (\ s a -> s{_deLastModifiedTimestamp = a}) . mapping _Time

-- | The ID of the virtual private cloud (VPC) used by this DevEndpoint.
deVPCId :: Lens' DevEndpoint (Maybe Text)
deVPCId = lens _deVPCId (\ s a -> s{_deVPCId = a})

-- | The private address used by this DevEndpoint.
dePrivateAddress :: Lens' DevEndpoint (Maybe Text)
dePrivateAddress = lens _dePrivateAddress (\ s a -> s{_dePrivateAddress = a})

-- | The public key to be used by this DevEndpoint for authentication.
dePublicKey :: Lens' DevEndpoint (Maybe Text)
dePublicKey = lens _dePublicKey (\ s a -> s{_dePublicKey = a})

-- | The subnet ID for this DevEndpoint.
deSubnetId :: Lens' DevEndpoint (Maybe Text)
deSubnetId = lens _deSubnetId (\ s a -> s{_deSubnetId = a})

-- | The number of AWS Glue Data Processing Units (DPUs) allocated to this DevEndpoint.
deNumberOfNodes :: Lens' DevEndpoint (Maybe Int)
deNumberOfNodes = lens _deNumberOfNodes (\ s a -> s{_deNumberOfNodes = a})

-- | The public VPC address used by this DevEndpoint.
dePublicAddress :: Lens' DevEndpoint (Maybe Text)
dePublicAddress = lens _dePublicAddress (\ s a -> s{_dePublicAddress = a})

-- | The AWS availability zone where this DevEndpoint is located.
deAvailabilityZone :: Lens' DevEndpoint (Maybe Text)
deAvailabilityZone = lens _deAvailabilityZone (\ s a -> s{_deAvailabilityZone = a})

-- | The Apache Zeppelin port for the remote Apache Spark interpreter.
deZeppelinRemoteSparkInterpreterPort :: Lens' DevEndpoint (Maybe Int)
deZeppelinRemoteSparkInterpreterPort = lens _deZeppelinRemoteSparkInterpreterPort (\ s a -> s{_deZeppelinRemoteSparkInterpreterPort = a})

-- | Path to one or more Java Jars in an S3 bucket that should be loaded in your DevEndpoint. Please note that only pure Java/Scala libraries can currently be used on a DevEndpoint.
deExtraJARsS3Path :: Lens' DevEndpoint (Maybe Text)
deExtraJARsS3Path = lens _deExtraJARsS3Path (\ s a -> s{_deExtraJARsS3Path = a})

-- | The point in time at which this DevEndpoint was created.
deCreatedTimestamp :: Lens' DevEndpoint (Maybe UTCTime)
deCreatedTimestamp = lens _deCreatedTimestamp (\ s a -> s{_deCreatedTimestamp = a}) . mapping _Time

-- | The YARN endpoint address used by this DevEndpoint.
deYarnEndpointAddress :: Lens' DevEndpoint (Maybe Text)
deYarnEndpointAddress = lens _deYarnEndpointAddress (\ s a -> s{_deYarnEndpointAddress = a})

-- | The AWS ARN of the IAM role used in this DevEndpoint.
deRoleARN :: Lens' DevEndpoint (Maybe Text)
deRoleARN = lens _deRoleARN (\ s a -> s{_deRoleARN = a})

instance FromJSON DevEndpoint where
        parseJSON
          = withObject "DevEndpoint"
              (\ x ->
                 DevEndpoint' <$>
                   (x .:? "Status") <*> (x .:? "FailureReason") <*>
                     (x .:? "EndpointName")
                     <*> (x .:? "ExtraPythonLibsS3Path")
                     <*> (x .:? "LastUpdateStatus")
                     <*> (x .:? "SecurityGroupIds" .!= mempty)
                     <*> (x .:? "LastModifiedTimestamp")
                     <*> (x .:? "VpcId")
                     <*> (x .:? "PrivateAddress")
                     <*> (x .:? "PublicKey")
                     <*> (x .:? "SubnetId")
                     <*> (x .:? "NumberOfNodes")
                     <*> (x .:? "PublicAddress")
                     <*> (x .:? "AvailabilityZone")
                     <*> (x .:? "ZeppelinRemoteSparkInterpreterPort")
                     <*> (x .:? "ExtraJarsS3Path")
                     <*> (x .:? "CreatedTimestamp")
                     <*> (x .:? "YarnEndpointAddress")
                     <*> (x .:? "RoleArn"))

instance Hashable DevEndpoint where

instance NFData DevEndpoint where

-- | Custom libraries to be loaded into a DevEndpoint.
--
--
--
-- /See:/ 'devEndpointCustomLibraries' smart constructor.
data DevEndpointCustomLibraries = DevEndpointCustomLibraries'
  { _declExtraPythonLibsS3Path :: !(Maybe Text)
  , _declExtraJARsS3Path       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DevEndpointCustomLibraries' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'declExtraPythonLibsS3Path' - Path(s) to one or more Python libraries in an S3 bucket that should be loaded in your DevEndpoint. Multiple values must be complete paths separated by a comma. Please note that only pure Python libraries can currently be used on a DevEndpoint. Libraries that rely on C extensions, such as the <http://pandas.pydata.org/ pandas> Python data analysis library, are not yet supported.
--
-- * 'declExtraJARsS3Path' - Path to one or more Java Jars in an S3 bucket that should be loaded in your DevEndpoint. Please note that only pure Java/Scala libraries can currently be used on a DevEndpoint.
devEndpointCustomLibraries
    :: DevEndpointCustomLibraries
devEndpointCustomLibraries =
  DevEndpointCustomLibraries'
    {_declExtraPythonLibsS3Path = Nothing, _declExtraJARsS3Path = Nothing}


-- | Path(s) to one or more Python libraries in an S3 bucket that should be loaded in your DevEndpoint. Multiple values must be complete paths separated by a comma. Please note that only pure Python libraries can currently be used on a DevEndpoint. Libraries that rely on C extensions, such as the <http://pandas.pydata.org/ pandas> Python data analysis library, are not yet supported.
declExtraPythonLibsS3Path :: Lens' DevEndpointCustomLibraries (Maybe Text)
declExtraPythonLibsS3Path = lens _declExtraPythonLibsS3Path (\ s a -> s{_declExtraPythonLibsS3Path = a})

-- | Path to one or more Java Jars in an S3 bucket that should be loaded in your DevEndpoint. Please note that only pure Java/Scala libraries can currently be used on a DevEndpoint.
declExtraJARsS3Path :: Lens' DevEndpointCustomLibraries (Maybe Text)
declExtraJARsS3Path = lens _declExtraJARsS3Path (\ s a -> s{_declExtraJARsS3Path = a})

instance Hashable DevEndpointCustomLibraries where

instance NFData DevEndpointCustomLibraries where

instance ToJSON DevEndpointCustomLibraries where
        toJSON DevEndpointCustomLibraries'{..}
          = object
              (catMaybes
                 [("ExtraPythonLibsS3Path" .=) <$>
                    _declExtraPythonLibsS3Path,
                  ("ExtraJarsS3Path" .=) <$> _declExtraJARsS3Path])

-- | Contains details about an error.
--
--
--
-- /See:/ 'errorDetail' smart constructor.
data ErrorDetail = ErrorDetail'
  { _edErrorCode    :: !(Maybe Text)
  , _edErrorMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ErrorDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edErrorCode' - The code associated with this error.
--
-- * 'edErrorMessage' - A message describing the error.
errorDetail
    :: ErrorDetail
errorDetail = ErrorDetail' {_edErrorCode = Nothing, _edErrorMessage = Nothing}


-- | The code associated with this error.
edErrorCode :: Lens' ErrorDetail (Maybe Text)
edErrorCode = lens _edErrorCode (\ s a -> s{_edErrorCode = a})

-- | A message describing the error.
edErrorMessage :: Lens' ErrorDetail (Maybe Text)
edErrorMessage = lens _edErrorMessage (\ s a -> s{_edErrorMessage = a})

instance FromJSON ErrorDetail where
        parseJSON
          = withObject "ErrorDetail"
              (\ x ->
                 ErrorDetail' <$>
                   (x .:? "ErrorCode") <*> (x .:? "ErrorMessage"))

instance Hashable ErrorDetail where

instance NFData ErrorDetail where

-- | An execution property of a job.
--
--
--
-- /See:/ 'executionProperty' smart constructor.
newtype ExecutionProperty = ExecutionProperty'
  { _epMaxConcurrentRuns :: Maybe Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExecutionProperty' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'epMaxConcurrentRuns' - The maximum number of concurrent runs allowed for the job. The default is 1. An error is returned when this threshold is reached. The maximum value you can specify is controlled by a service limit.
executionProperty
    :: ExecutionProperty
executionProperty = ExecutionProperty' {_epMaxConcurrentRuns = Nothing}


-- | The maximum number of concurrent runs allowed for the job. The default is 1. An error is returned when this threshold is reached. The maximum value you can specify is controlled by a service limit.
epMaxConcurrentRuns :: Lens' ExecutionProperty (Maybe Int)
epMaxConcurrentRuns = lens _epMaxConcurrentRuns (\ s a -> s{_epMaxConcurrentRuns = a})

instance FromJSON ExecutionProperty where
        parseJSON
          = withObject "ExecutionProperty"
              (\ x ->
                 ExecutionProperty' <$> (x .:? "MaxConcurrentRuns"))

instance Hashable ExecutionProperty where

instance NFData ExecutionProperty where

instance ToJSON ExecutionProperty where
        toJSON ExecutionProperty'{..}
          = object
              (catMaybes
                 [("MaxConcurrentRuns" .=) <$> _epMaxConcurrentRuns])

-- | Filters the connection definitions returned by the @GetConnections@ API.
--
--
--
-- /See:/ 'getConnectionsFilter' smart constructor.
data GetConnectionsFilter = GetConnectionsFilter'
  { _gcfMatchCriteria  :: !(Maybe [Text])
  , _gcfConnectionType :: !(Maybe ConnectionType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetConnectionsFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcfMatchCriteria' - A criteria string that must match the criteria recorded in the connection definition for that connection definition to be returned.
--
-- * 'gcfConnectionType' - The type of connections to return. Currently, only JDBC is supported; SFTP is not supported.
getConnectionsFilter
    :: GetConnectionsFilter
getConnectionsFilter =
  GetConnectionsFilter'
    {_gcfMatchCriteria = Nothing, _gcfConnectionType = Nothing}


-- | A criteria string that must match the criteria recorded in the connection definition for that connection definition to be returned.
gcfMatchCriteria :: Lens' GetConnectionsFilter [Text]
gcfMatchCriteria = lens _gcfMatchCriteria (\ s a -> s{_gcfMatchCriteria = a}) . _Default . _Coerce

-- | The type of connections to return. Currently, only JDBC is supported; SFTP is not supported.
gcfConnectionType :: Lens' GetConnectionsFilter (Maybe ConnectionType)
gcfConnectionType = lens _gcfConnectionType (\ s a -> s{_gcfConnectionType = a})

instance Hashable GetConnectionsFilter where

instance NFData GetConnectionsFilter where

instance ToJSON GetConnectionsFilter where
        toJSON GetConnectionsFilter'{..}
          = object
              (catMaybes
                 [("MatchCriteria" .=) <$> _gcfMatchCriteria,
                  ("ConnectionType" .=) <$> _gcfConnectionType])

-- | A classifier that uses @grok@ patterns.
--
--
--
-- /See:/ 'grokClassifier' smart constructor.
data GrokClassifier = GrokClassifier'
  { _gcCreationTime   :: !(Maybe POSIX)
  , _gcLastUpdated    :: !(Maybe POSIX)
  , _gcVersion        :: !(Maybe Integer)
  , _gcCustomPatterns :: !(Maybe Text)
  , _gcName           :: !Text
  , _gcClassification :: !Text
  , _gcGrokPattern    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GrokClassifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcCreationTime' - The time this classifier was registered.
--
-- * 'gcLastUpdated' - The time this classifier was last updated.
--
-- * 'gcVersion' - The version of this classifier.
--
-- * 'gcCustomPatterns' - Optional custom grok patterns defined by this classifier. For more information, see custom patterns in <http://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifers> .
--
-- * 'gcName' - The name of the classifier.
--
-- * 'gcClassification' - An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, and so on.
--
-- * 'gcGrokPattern' - The grok pattern applied to a data store by this classifier. For more information, see built-in patterns in <http://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifers> .
grokClassifier
    :: Text -- ^ 'gcName'
    -> Text -- ^ 'gcClassification'
    -> Text -- ^ 'gcGrokPattern'
    -> GrokClassifier
grokClassifier pName_ pClassification_ pGrokPattern_ =
  GrokClassifier'
    { _gcCreationTime = Nothing
    , _gcLastUpdated = Nothing
    , _gcVersion = Nothing
    , _gcCustomPatterns = Nothing
    , _gcName = pName_
    , _gcClassification = pClassification_
    , _gcGrokPattern = pGrokPattern_
    }


-- | The time this classifier was registered.
gcCreationTime :: Lens' GrokClassifier (Maybe UTCTime)
gcCreationTime = lens _gcCreationTime (\ s a -> s{_gcCreationTime = a}) . mapping _Time

-- | The time this classifier was last updated.
gcLastUpdated :: Lens' GrokClassifier (Maybe UTCTime)
gcLastUpdated = lens _gcLastUpdated (\ s a -> s{_gcLastUpdated = a}) . mapping _Time

-- | The version of this classifier.
gcVersion :: Lens' GrokClassifier (Maybe Integer)
gcVersion = lens _gcVersion (\ s a -> s{_gcVersion = a})

-- | Optional custom grok patterns defined by this classifier. For more information, see custom patterns in <http://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifers> .
gcCustomPatterns :: Lens' GrokClassifier (Maybe Text)
gcCustomPatterns = lens _gcCustomPatterns (\ s a -> s{_gcCustomPatterns = a})

-- | The name of the classifier.
gcName :: Lens' GrokClassifier Text
gcName = lens _gcName (\ s a -> s{_gcName = a})

-- | An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, and so on.
gcClassification :: Lens' GrokClassifier Text
gcClassification = lens _gcClassification (\ s a -> s{_gcClassification = a})

-- | The grok pattern applied to a data store by this classifier. For more information, see built-in patterns in <http://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifers> .
gcGrokPattern :: Lens' GrokClassifier Text
gcGrokPattern = lens _gcGrokPattern (\ s a -> s{_gcGrokPattern = a})

instance FromJSON GrokClassifier where
        parseJSON
          = withObject "GrokClassifier"
              (\ x ->
                 GrokClassifier' <$>
                   (x .:? "CreationTime") <*> (x .:? "LastUpdated") <*>
                     (x .:? "Version")
                     <*> (x .:? "CustomPatterns")
                     <*> (x .: "Name")
                     <*> (x .: "Classification")
                     <*> (x .: "GrokPattern"))

instance Hashable GrokClassifier where

instance NFData GrokClassifier where

-- | A classifier for @JSON@ content.
--
--
--
-- /See:/ 'jsonClassifier' smart constructor.
data JSONClassifier = JSONClassifier'
  { _jcCreationTime :: !(Maybe POSIX)
  , _jcLastUpdated  :: !(Maybe POSIX)
  , _jcVersion      :: !(Maybe Integer)
  , _jcName         :: !Text
  , _jcJSONPath     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JSONClassifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jcCreationTime' - The time this classifier was registered.
--
-- * 'jcLastUpdated' - The time this classifier was last updated.
--
-- * 'jcVersion' - The version of this classifier.
--
-- * 'jcName' - The name of the classifier.
--
-- * 'jcJSONPath' - A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
jsonClassifier
    :: Text -- ^ 'jcName'
    -> Text -- ^ 'jcJSONPath'
    -> JSONClassifier
jsonClassifier pName_ pJSONPath_ =
  JSONClassifier'
    { _jcCreationTime = Nothing
    , _jcLastUpdated = Nothing
    , _jcVersion = Nothing
    , _jcName = pName_
    , _jcJSONPath = pJSONPath_
    }


-- | The time this classifier was registered.
jcCreationTime :: Lens' JSONClassifier (Maybe UTCTime)
jcCreationTime = lens _jcCreationTime (\ s a -> s{_jcCreationTime = a}) . mapping _Time

-- | The time this classifier was last updated.
jcLastUpdated :: Lens' JSONClassifier (Maybe UTCTime)
jcLastUpdated = lens _jcLastUpdated (\ s a -> s{_jcLastUpdated = a}) . mapping _Time

-- | The version of this classifier.
jcVersion :: Lens' JSONClassifier (Maybe Integer)
jcVersion = lens _jcVersion (\ s a -> s{_jcVersion = a})

-- | The name of the classifier.
jcName :: Lens' JSONClassifier Text
jcName = lens _jcName (\ s a -> s{_jcName = a})

-- | A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
jcJSONPath :: Lens' JSONClassifier Text
jcJSONPath = lens _jcJSONPath (\ s a -> s{_jcJSONPath = a})

instance FromJSON JSONClassifier where
        parseJSON
          = withObject "JSONClassifier"
              (\ x ->
                 JSONClassifier' <$>
                   (x .:? "CreationTime") <*> (x .:? "LastUpdated") <*>
                     (x .:? "Version")
                     <*> (x .: "Name")
                     <*> (x .: "JsonPath"))

instance Hashable JSONClassifier where

instance NFData JSONClassifier where

-- | Specifies a JDBC data store to crawl.
--
--
--
-- /See:/ 'jdbcTarget' smart constructor.
data JdbcTarget = JdbcTarget'
  { _jtPath           :: !(Maybe Text)
  , _jtConnectionName :: !(Maybe Text)
  , _jtExclusions     :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JdbcTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jtPath' - The path of the JDBC target.
--
-- * 'jtConnectionName' - The name of the connection to use to connect to the JDBC target.
--
-- * 'jtExclusions' - A list of glob patterns used to exclude from the crawl. For more information, see <http://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler> .
jdbcTarget
    :: JdbcTarget
jdbcTarget =
  JdbcTarget'
    {_jtPath = Nothing, _jtConnectionName = Nothing, _jtExclusions = Nothing}


-- | The path of the JDBC target.
jtPath :: Lens' JdbcTarget (Maybe Text)
jtPath = lens _jtPath (\ s a -> s{_jtPath = a})

-- | The name of the connection to use to connect to the JDBC target.
jtConnectionName :: Lens' JdbcTarget (Maybe Text)
jtConnectionName = lens _jtConnectionName (\ s a -> s{_jtConnectionName = a})

-- | A list of glob patterns used to exclude from the crawl. For more information, see <http://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler> .
jtExclusions :: Lens' JdbcTarget [Text]
jtExclusions = lens _jtExclusions (\ s a -> s{_jtExclusions = a}) . _Default . _Coerce

instance FromJSON JdbcTarget where
        parseJSON
          = withObject "JdbcTarget"
              (\ x ->
                 JdbcTarget' <$>
                   (x .:? "Path") <*> (x .:? "ConnectionName") <*>
                     (x .:? "Exclusions" .!= mempty))

instance Hashable JdbcTarget where

instance NFData JdbcTarget where

instance ToJSON JdbcTarget where
        toJSON JdbcTarget'{..}
          = object
              (catMaybes
                 [("Path" .=) <$> _jtPath,
                  ("ConnectionName" .=) <$> _jtConnectionName,
                  ("Exclusions" .=) <$> _jtExclusions])

-- | Specifies a job definition.
--
--
--
-- /See:/ 'job' smart constructor.
data Job = Job'
  { _jCommand           :: !(Maybe JobCommand)
  , _jLastModifiedOn    :: !(Maybe POSIX)
  , _jConnections       :: !(Maybe ConnectionsList)
  , _jRole              :: !(Maybe Text)
  , _jName              :: !(Maybe Text)
  , _jLogURI            :: !(Maybe Text)
  , _jMaxRetries        :: !(Maybe Int)
  , _jExecutionProperty :: !(Maybe ExecutionProperty)
  , _jAllocatedCapacity :: !(Maybe Int)
  , _jTimeout           :: !(Maybe Nat)
  , _jDefaultArguments  :: !(Maybe (Map Text Text))
  , _jDescription       :: !(Maybe Text)
  , _jCreatedOn         :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Job' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jCommand' - The JobCommand that executes this job.
--
-- * 'jLastModifiedOn' - The last point in time when this job definition was modified.
--
-- * 'jConnections' - The connections used for this job.
--
-- * 'jRole' - The name or ARN of the IAM role associated with this job.
--
-- * 'jName' - The name you assign to this job definition.
--
-- * 'jLogURI' - This field is reserved for future use.
--
-- * 'jMaxRetries' - The maximum number of times to retry this job after a JobRun fails.
--
-- * 'jExecutionProperty' - An ExecutionProperty specifying the maximum number of concurrent runs allowed for this job.
--
-- * 'jAllocatedCapacity' - The number of AWS Glue data processing units (DPUs) allocated to runs of this job. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- * 'jTimeout' - The job timeout in minutes.
--
-- * 'jDefaultArguments' - The default arguments for this job, specified as name-value pairs. You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes. For information about how to specify and consume your own Job arguments, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide. For information about the key-value pairs that AWS Glue consumes to set up your job, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
--
-- * 'jDescription' - Description of the job being defined.
--
-- * 'jCreatedOn' - The time and date that this job definition was created.
job
    :: Job
job =
  Job'
    { _jCommand = Nothing
    , _jLastModifiedOn = Nothing
    , _jConnections = Nothing
    , _jRole = Nothing
    , _jName = Nothing
    , _jLogURI = Nothing
    , _jMaxRetries = Nothing
    , _jExecutionProperty = Nothing
    , _jAllocatedCapacity = Nothing
    , _jTimeout = Nothing
    , _jDefaultArguments = Nothing
    , _jDescription = Nothing
    , _jCreatedOn = Nothing
    }


-- | The JobCommand that executes this job.
jCommand :: Lens' Job (Maybe JobCommand)
jCommand = lens _jCommand (\ s a -> s{_jCommand = a})

-- | The last point in time when this job definition was modified.
jLastModifiedOn :: Lens' Job (Maybe UTCTime)
jLastModifiedOn = lens _jLastModifiedOn (\ s a -> s{_jLastModifiedOn = a}) . mapping _Time

-- | The connections used for this job.
jConnections :: Lens' Job (Maybe ConnectionsList)
jConnections = lens _jConnections (\ s a -> s{_jConnections = a})

-- | The name or ARN of the IAM role associated with this job.
jRole :: Lens' Job (Maybe Text)
jRole = lens _jRole (\ s a -> s{_jRole = a})

-- | The name you assign to this job definition.
jName :: Lens' Job (Maybe Text)
jName = lens _jName (\ s a -> s{_jName = a})

-- | This field is reserved for future use.
jLogURI :: Lens' Job (Maybe Text)
jLogURI = lens _jLogURI (\ s a -> s{_jLogURI = a})

-- | The maximum number of times to retry this job after a JobRun fails.
jMaxRetries :: Lens' Job (Maybe Int)
jMaxRetries = lens _jMaxRetries (\ s a -> s{_jMaxRetries = a})

-- | An ExecutionProperty specifying the maximum number of concurrent runs allowed for this job.
jExecutionProperty :: Lens' Job (Maybe ExecutionProperty)
jExecutionProperty = lens _jExecutionProperty (\ s a -> s{_jExecutionProperty = a})

-- | The number of AWS Glue data processing units (DPUs) allocated to runs of this job. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
jAllocatedCapacity :: Lens' Job (Maybe Int)
jAllocatedCapacity = lens _jAllocatedCapacity (\ s a -> s{_jAllocatedCapacity = a})

-- | The job timeout in minutes.
jTimeout :: Lens' Job (Maybe Natural)
jTimeout = lens _jTimeout (\ s a -> s{_jTimeout = a}) . mapping _Nat

-- | The default arguments for this job, specified as name-value pairs. You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes. For information about how to specify and consume your own Job arguments, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide. For information about the key-value pairs that AWS Glue consumes to set up your job, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
jDefaultArguments :: Lens' Job (HashMap Text Text)
jDefaultArguments = lens _jDefaultArguments (\ s a -> s{_jDefaultArguments = a}) . _Default . _Map

-- | Description of the job being defined.
jDescription :: Lens' Job (Maybe Text)
jDescription = lens _jDescription (\ s a -> s{_jDescription = a})

-- | The time and date that this job definition was created.
jCreatedOn :: Lens' Job (Maybe UTCTime)
jCreatedOn = lens _jCreatedOn (\ s a -> s{_jCreatedOn = a}) . mapping _Time

instance FromJSON Job where
        parseJSON
          = withObject "Job"
              (\ x ->
                 Job' <$>
                   (x .:? "Command") <*> (x .:? "LastModifiedOn") <*>
                     (x .:? "Connections")
                     <*> (x .:? "Role")
                     <*> (x .:? "Name")
                     <*> (x .:? "LogUri")
                     <*> (x .:? "MaxRetries")
                     <*> (x .:? "ExecutionProperty")
                     <*> (x .:? "AllocatedCapacity")
                     <*> (x .:? "Timeout")
                     <*> (x .:? "DefaultArguments" .!= mempty)
                     <*> (x .:? "Description")
                     <*> (x .:? "CreatedOn"))

instance Hashable Job where

instance NFData Job where

-- | Defines a point which a job can resume processing.
--
--
--
-- /See:/ 'jobBookmarkEntry' smart constructor.
data JobBookmarkEntry = JobBookmarkEntry'
  { _jbeJobName     :: !(Maybe Text)
  , _jbeRun         :: !(Maybe Int)
  , _jbeVersion     :: !(Maybe Int)
  , _jbeAttempt     :: !(Maybe Int)
  , _jbeJobBookmark :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobBookmarkEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jbeJobName' - Name of the job in question.
--
-- * 'jbeRun' - The run ID number.
--
-- * 'jbeVersion' - Version of the job.
--
-- * 'jbeAttempt' - The attempt ID number.
--
-- * 'jbeJobBookmark' - The bookmark itself.
jobBookmarkEntry
    :: JobBookmarkEntry
jobBookmarkEntry =
  JobBookmarkEntry'
    { _jbeJobName = Nothing
    , _jbeRun = Nothing
    , _jbeVersion = Nothing
    , _jbeAttempt = Nothing
    , _jbeJobBookmark = Nothing
    }


-- | Name of the job in question.
jbeJobName :: Lens' JobBookmarkEntry (Maybe Text)
jbeJobName = lens _jbeJobName (\ s a -> s{_jbeJobName = a})

-- | The run ID number.
jbeRun :: Lens' JobBookmarkEntry (Maybe Int)
jbeRun = lens _jbeRun (\ s a -> s{_jbeRun = a})

-- | Version of the job.
jbeVersion :: Lens' JobBookmarkEntry (Maybe Int)
jbeVersion = lens _jbeVersion (\ s a -> s{_jbeVersion = a})

-- | The attempt ID number.
jbeAttempt :: Lens' JobBookmarkEntry (Maybe Int)
jbeAttempt = lens _jbeAttempt (\ s a -> s{_jbeAttempt = a})

-- | The bookmark itself.
jbeJobBookmark :: Lens' JobBookmarkEntry (Maybe Text)
jbeJobBookmark = lens _jbeJobBookmark (\ s a -> s{_jbeJobBookmark = a})

instance FromJSON JobBookmarkEntry where
        parseJSON
          = withObject "JobBookmarkEntry"
              (\ x ->
                 JobBookmarkEntry' <$>
                   (x .:? "JobName") <*> (x .:? "Run") <*>
                     (x .:? "Version")
                     <*> (x .:? "Attempt")
                     <*> (x .:? "JobBookmark"))

instance Hashable JobBookmarkEntry where

instance NFData JobBookmarkEntry where

-- | Specifies code executed when a job is run.
--
--
--
-- /See:/ 'jobCommand' smart constructor.
data JobCommand = JobCommand'
  { _jobScriptLocation :: !(Maybe Text)
  , _jobName           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobCommand' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jobScriptLocation' - Specifies the S3 path to a script that executes a job (required).
--
-- * 'jobName' - The name of the job command: this must be @glueetl@ .
jobCommand
    :: JobCommand
jobCommand = JobCommand' {_jobScriptLocation = Nothing, _jobName = Nothing}


-- | Specifies the S3 path to a script that executes a job (required).
jobScriptLocation :: Lens' JobCommand (Maybe Text)
jobScriptLocation = lens _jobScriptLocation (\ s a -> s{_jobScriptLocation = a})

-- | The name of the job command: this must be @glueetl@ .
jobName :: Lens' JobCommand (Maybe Text)
jobName = lens _jobName (\ s a -> s{_jobName = a})

instance FromJSON JobCommand where
        parseJSON
          = withObject "JobCommand"
              (\ x ->
                 JobCommand' <$>
                   (x .:? "ScriptLocation") <*> (x .:? "Name"))

instance Hashable JobCommand where

instance NFData JobCommand where

instance ToJSON JobCommand where
        toJSON JobCommand'{..}
          = object
              (catMaybes
                 [("ScriptLocation" .=) <$> _jobScriptLocation,
                  ("Name" .=) <$> _jobName])

-- | Contains information about a job run.
--
--
--
-- /See:/ 'jobRun' smart constructor.
data JobRun = JobRun'
  { _jrCompletedOn       :: !(Maybe POSIX)
  , _jrTriggerName       :: !(Maybe Text)
  , _jrLastModifiedOn    :: !(Maybe POSIX)
  , _jrArguments         :: !(Maybe (Map Text Text))
  , _jrJobName           :: !(Maybe Text)
  , _jrStartedOn         :: !(Maybe POSIX)
  , _jrJobRunState       :: !(Maybe JobRunState)
  , _jrExecutionTime     :: !(Maybe Int)
  , _jrPredecessorRuns   :: !(Maybe [Predecessor])
  , _jrPreviousRunId     :: !(Maybe Text)
  , _jrId                :: !(Maybe Text)
  , _jrAttempt           :: !(Maybe Int)
  , _jrAllocatedCapacity :: !(Maybe Int)
  , _jrTimeout           :: !(Maybe Nat)
  , _jrErrorMessage      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jrCompletedOn' - The date and time this job run completed.
--
-- * 'jrTriggerName' - The name of the trigger that started this job run.
--
-- * 'jrLastModifiedOn' - The last time this job run was modified.
--
-- * 'jrArguments' - The job arguments associated with this run. These override equivalent default arguments set for the job. You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes. For information about how to specify and consume your own job arguments, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide. For information about the key-value pairs that AWS Glue consumes to set up your job, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
--
-- * 'jrJobName' - The name of the job definition being used in this run.
--
-- * 'jrStartedOn' - The date and time at which this job run was started.
--
-- * 'jrJobRunState' - The current state of the job run.
--
-- * 'jrExecutionTime' - The amount of time (in seconds) that the job run consumed resources.
--
-- * 'jrPredecessorRuns' - A list of predecessors to this job run.
--
-- * 'jrPreviousRunId' - The ID of the previous run of this job. For example, the JobRunId specified in the StartJobRun action.
--
-- * 'jrId' - The ID of this job run.
--
-- * 'jrAttempt' - The number of the attempt to run this job.
--
-- * 'jrAllocatedCapacity' - The number of AWS Glue data processing units (DPUs) allocated to this JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- * 'jrTimeout' - The job run timeout in minutes.
--
-- * 'jrErrorMessage' - An error message associated with this job run.
jobRun
    :: JobRun
jobRun =
  JobRun'
    { _jrCompletedOn = Nothing
    , _jrTriggerName = Nothing
    , _jrLastModifiedOn = Nothing
    , _jrArguments = Nothing
    , _jrJobName = Nothing
    , _jrStartedOn = Nothing
    , _jrJobRunState = Nothing
    , _jrExecutionTime = Nothing
    , _jrPredecessorRuns = Nothing
    , _jrPreviousRunId = Nothing
    , _jrId = Nothing
    , _jrAttempt = Nothing
    , _jrAllocatedCapacity = Nothing
    , _jrTimeout = Nothing
    , _jrErrorMessage = Nothing
    }


-- | The date and time this job run completed.
jrCompletedOn :: Lens' JobRun (Maybe UTCTime)
jrCompletedOn = lens _jrCompletedOn (\ s a -> s{_jrCompletedOn = a}) . mapping _Time

-- | The name of the trigger that started this job run.
jrTriggerName :: Lens' JobRun (Maybe Text)
jrTriggerName = lens _jrTriggerName (\ s a -> s{_jrTriggerName = a})

-- | The last time this job run was modified.
jrLastModifiedOn :: Lens' JobRun (Maybe UTCTime)
jrLastModifiedOn = lens _jrLastModifiedOn (\ s a -> s{_jrLastModifiedOn = a}) . mapping _Time

-- | The job arguments associated with this run. These override equivalent default arguments set for the job. You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes. For information about how to specify and consume your own job arguments, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide. For information about the key-value pairs that AWS Glue consumes to set up your job, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
jrArguments :: Lens' JobRun (HashMap Text Text)
jrArguments = lens _jrArguments (\ s a -> s{_jrArguments = a}) . _Default . _Map

-- | The name of the job definition being used in this run.
jrJobName :: Lens' JobRun (Maybe Text)
jrJobName = lens _jrJobName (\ s a -> s{_jrJobName = a})

-- | The date and time at which this job run was started.
jrStartedOn :: Lens' JobRun (Maybe UTCTime)
jrStartedOn = lens _jrStartedOn (\ s a -> s{_jrStartedOn = a}) . mapping _Time

-- | The current state of the job run.
jrJobRunState :: Lens' JobRun (Maybe JobRunState)
jrJobRunState = lens _jrJobRunState (\ s a -> s{_jrJobRunState = a})

-- | The amount of time (in seconds) that the job run consumed resources.
jrExecutionTime :: Lens' JobRun (Maybe Int)
jrExecutionTime = lens _jrExecutionTime (\ s a -> s{_jrExecutionTime = a})

-- | A list of predecessors to this job run.
jrPredecessorRuns :: Lens' JobRun [Predecessor]
jrPredecessorRuns = lens _jrPredecessorRuns (\ s a -> s{_jrPredecessorRuns = a}) . _Default . _Coerce

-- | The ID of the previous run of this job. For example, the JobRunId specified in the StartJobRun action.
jrPreviousRunId :: Lens' JobRun (Maybe Text)
jrPreviousRunId = lens _jrPreviousRunId (\ s a -> s{_jrPreviousRunId = a})

-- | The ID of this job run.
jrId :: Lens' JobRun (Maybe Text)
jrId = lens _jrId (\ s a -> s{_jrId = a})

-- | The number of the attempt to run this job.
jrAttempt :: Lens' JobRun (Maybe Int)
jrAttempt = lens _jrAttempt (\ s a -> s{_jrAttempt = a})

-- | The number of AWS Glue data processing units (DPUs) allocated to this JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
jrAllocatedCapacity :: Lens' JobRun (Maybe Int)
jrAllocatedCapacity = lens _jrAllocatedCapacity (\ s a -> s{_jrAllocatedCapacity = a})

-- | The job run timeout in minutes.
jrTimeout :: Lens' JobRun (Maybe Natural)
jrTimeout = lens _jrTimeout (\ s a -> s{_jrTimeout = a}) . mapping _Nat

-- | An error message associated with this job run.
jrErrorMessage :: Lens' JobRun (Maybe Text)
jrErrorMessage = lens _jrErrorMessage (\ s a -> s{_jrErrorMessage = a})

instance FromJSON JobRun where
        parseJSON
          = withObject "JobRun"
              (\ x ->
                 JobRun' <$>
                   (x .:? "CompletedOn") <*> (x .:? "TriggerName") <*>
                     (x .:? "LastModifiedOn")
                     <*> (x .:? "Arguments" .!= mempty)
                     <*> (x .:? "JobName")
                     <*> (x .:? "StartedOn")
                     <*> (x .:? "JobRunState")
                     <*> (x .:? "ExecutionTime")
                     <*> (x .:? "PredecessorRuns" .!= mempty)
                     <*> (x .:? "PreviousRunId")
                     <*> (x .:? "Id")
                     <*> (x .:? "Attempt")
                     <*> (x .:? "AllocatedCapacity")
                     <*> (x .:? "Timeout")
                     <*> (x .:? "ErrorMessage"))

instance Hashable JobRun where

instance NFData JobRun where

-- | Specifies information used to update an existing job definition. Note that the previous job definition will be completely overwritten by this information.
--
--
--
-- /See:/ 'jobUpdate' smart constructor.
data JobUpdate = JobUpdate'
  { _juCommand           :: !(Maybe JobCommand)
  , _juConnections       :: !(Maybe ConnectionsList)
  , _juRole              :: !(Maybe Text)
  , _juLogURI            :: !(Maybe Text)
  , _juMaxRetries        :: !(Maybe Int)
  , _juExecutionProperty :: !(Maybe ExecutionProperty)
  , _juAllocatedCapacity :: !(Maybe Int)
  , _juTimeout           :: !(Maybe Nat)
  , _juDefaultArguments  :: !(Maybe (Map Text Text))
  , _juDescription       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'juCommand' - The JobCommand that executes this job (required).
--
-- * 'juConnections' - The connections used for this job.
--
-- * 'juRole' - The name or ARN of the IAM role associated with this job (required).
--
-- * 'juLogURI' - This field is reserved for future use.
--
-- * 'juMaxRetries' - The maximum number of times to retry this job if it fails.
--
-- * 'juExecutionProperty' - An ExecutionProperty specifying the maximum number of concurrent runs allowed for this job.
--
-- * 'juAllocatedCapacity' - The number of AWS Glue data processing units (DPUs) to allocate to this Job. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- * 'juTimeout' - The job timeout in minutes. The default is 2880 minutes (48 hours).
--
-- * 'juDefaultArguments' - The default arguments for this job. You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes. For information about how to specify and consume your own Job arguments, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide. For information about the key-value pairs that AWS Glue consumes to set up your job, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
--
-- * 'juDescription' - Description of the job being defined.
jobUpdate
    :: JobUpdate
jobUpdate =
  JobUpdate'
    { _juCommand = Nothing
    , _juConnections = Nothing
    , _juRole = Nothing
    , _juLogURI = Nothing
    , _juMaxRetries = Nothing
    , _juExecutionProperty = Nothing
    , _juAllocatedCapacity = Nothing
    , _juTimeout = Nothing
    , _juDefaultArguments = Nothing
    , _juDescription = Nothing
    }


-- | The JobCommand that executes this job (required).
juCommand :: Lens' JobUpdate (Maybe JobCommand)
juCommand = lens _juCommand (\ s a -> s{_juCommand = a})

-- | The connections used for this job.
juConnections :: Lens' JobUpdate (Maybe ConnectionsList)
juConnections = lens _juConnections (\ s a -> s{_juConnections = a})

-- | The name or ARN of the IAM role associated with this job (required).
juRole :: Lens' JobUpdate (Maybe Text)
juRole = lens _juRole (\ s a -> s{_juRole = a})

-- | This field is reserved for future use.
juLogURI :: Lens' JobUpdate (Maybe Text)
juLogURI = lens _juLogURI (\ s a -> s{_juLogURI = a})

-- | The maximum number of times to retry this job if it fails.
juMaxRetries :: Lens' JobUpdate (Maybe Int)
juMaxRetries = lens _juMaxRetries (\ s a -> s{_juMaxRetries = a})

-- | An ExecutionProperty specifying the maximum number of concurrent runs allowed for this job.
juExecutionProperty :: Lens' JobUpdate (Maybe ExecutionProperty)
juExecutionProperty = lens _juExecutionProperty (\ s a -> s{_juExecutionProperty = a})

-- | The number of AWS Glue data processing units (DPUs) to allocate to this Job. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
juAllocatedCapacity :: Lens' JobUpdate (Maybe Int)
juAllocatedCapacity = lens _juAllocatedCapacity (\ s a -> s{_juAllocatedCapacity = a})

-- | The job timeout in minutes. The default is 2880 minutes (48 hours).
juTimeout :: Lens' JobUpdate (Maybe Natural)
juTimeout = lens _juTimeout (\ s a -> s{_juTimeout = a}) . mapping _Nat

-- | The default arguments for this job. You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes. For information about how to specify and consume your own Job arguments, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide. For information about the key-value pairs that AWS Glue consumes to set up your job, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
juDefaultArguments :: Lens' JobUpdate (HashMap Text Text)
juDefaultArguments = lens _juDefaultArguments (\ s a -> s{_juDefaultArguments = a}) . _Default . _Map

-- | Description of the job being defined.
juDescription :: Lens' JobUpdate (Maybe Text)
juDescription = lens _juDescription (\ s a -> s{_juDescription = a})

instance Hashable JobUpdate where

instance NFData JobUpdate where

instance ToJSON JobUpdate where
        toJSON JobUpdate'{..}
          = object
              (catMaybes
                 [("Command" .=) <$> _juCommand,
                  ("Connections" .=) <$> _juConnections,
                  ("Role" .=) <$> _juRole, ("LogUri" .=) <$> _juLogURI,
                  ("MaxRetries" .=) <$> _juMaxRetries,
                  ("ExecutionProperty" .=) <$> _juExecutionProperty,
                  ("AllocatedCapacity" .=) <$> _juAllocatedCapacity,
                  ("Timeout" .=) <$> _juTimeout,
                  ("DefaultArguments" .=) <$> _juDefaultArguments,
                  ("Description" .=) <$> _juDescription])

-- | Status and error information about the most recent crawl.
--
--
--
-- /See:/ 'lastCrawlInfo' smart constructor.
data LastCrawlInfo = LastCrawlInfo'
  { _lciStatus        :: !(Maybe LastCrawlStatus)
  , _lciStartTime     :: !(Maybe POSIX)
  , _lciLogStream     :: !(Maybe Text)
  , _lciLogGroup      :: !(Maybe Text)
  , _lciMessagePrefix :: !(Maybe Text)
  , _lciErrorMessage  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LastCrawlInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lciStatus' - Status of the last crawl.
--
-- * 'lciStartTime' - The time at which the crawl started.
--
-- * 'lciLogStream' - The log stream for the last crawl.
--
-- * 'lciLogGroup' - The log group for the last crawl.
--
-- * 'lciMessagePrefix' - The prefix for a message about this crawl.
--
-- * 'lciErrorMessage' - If an error occurred, the error information about the last crawl.
lastCrawlInfo
    :: LastCrawlInfo
lastCrawlInfo =
  LastCrawlInfo'
    { _lciStatus = Nothing
    , _lciStartTime = Nothing
    , _lciLogStream = Nothing
    , _lciLogGroup = Nothing
    , _lciMessagePrefix = Nothing
    , _lciErrorMessage = Nothing
    }


-- | Status of the last crawl.
lciStatus :: Lens' LastCrawlInfo (Maybe LastCrawlStatus)
lciStatus = lens _lciStatus (\ s a -> s{_lciStatus = a})

-- | The time at which the crawl started.
lciStartTime :: Lens' LastCrawlInfo (Maybe UTCTime)
lciStartTime = lens _lciStartTime (\ s a -> s{_lciStartTime = a}) . mapping _Time

-- | The log stream for the last crawl.
lciLogStream :: Lens' LastCrawlInfo (Maybe Text)
lciLogStream = lens _lciLogStream (\ s a -> s{_lciLogStream = a})

-- | The log group for the last crawl.
lciLogGroup :: Lens' LastCrawlInfo (Maybe Text)
lciLogGroup = lens _lciLogGroup (\ s a -> s{_lciLogGroup = a})

-- | The prefix for a message about this crawl.
lciMessagePrefix :: Lens' LastCrawlInfo (Maybe Text)
lciMessagePrefix = lens _lciMessagePrefix (\ s a -> s{_lciMessagePrefix = a})

-- | If an error occurred, the error information about the last crawl.
lciErrorMessage :: Lens' LastCrawlInfo (Maybe Text)
lciErrorMessage = lens _lciErrorMessage (\ s a -> s{_lciErrorMessage = a})

instance FromJSON LastCrawlInfo where
        parseJSON
          = withObject "LastCrawlInfo"
              (\ x ->
                 LastCrawlInfo' <$>
                   (x .:? "Status") <*> (x .:? "StartTime") <*>
                     (x .:? "LogStream")
                     <*> (x .:? "LogGroup")
                     <*> (x .:? "MessagePrefix")
                     <*> (x .:? "ErrorMessage"))

instance Hashable LastCrawlInfo where

instance NFData LastCrawlInfo where

-- | The location of resources.
--
--
--
-- /See:/ 'location' smart constructor.
data Location = Location'
  { _lJdbc :: !(Maybe [CodeGenNodeArg])
  , _lS3   :: !(Maybe [CodeGenNodeArg])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lJdbc' - A JDBC location.
--
-- * 'lS3' - An Amazon S3 location.
location
    :: Location
location = Location' {_lJdbc = Nothing, _lS3 = Nothing}


-- | A JDBC location.
lJdbc :: Lens' Location [CodeGenNodeArg]
lJdbc = lens _lJdbc (\ s a -> s{_lJdbc = a}) . _Default . _Coerce

-- | An Amazon S3 location.
lS3 :: Lens' Location [CodeGenNodeArg]
lS3 = lens _lS3 (\ s a -> s{_lS3 = a}) . _Default . _Coerce

instance Hashable Location where

instance NFData Location where

instance ToJSON Location where
        toJSON Location'{..}
          = object
              (catMaybes
                 [("Jdbc" .=) <$> _lJdbc, ("S3" .=) <$> _lS3])

-- | Defines a mapping.
--
--
--
-- /See:/ 'mappingEntry' smart constructor.
data MappingEntry = MappingEntry'
  { _meTargetTable :: !(Maybe Text)
  , _meSourceType  :: !(Maybe Text)
  , _meSourceTable :: !(Maybe Text)
  , _meTargetType  :: !(Maybe Text)
  , _meTargetPath  :: !(Maybe Text)
  , _meSourcePath  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MappingEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'meTargetTable' - The target table.
--
-- * 'meSourceType' - The source type.
--
-- * 'meSourceTable' - The name of the source table.
--
-- * 'meTargetType' - The target type.
--
-- * 'meTargetPath' - The target path.
--
-- * 'meSourcePath' - The source path.
mappingEntry
    :: MappingEntry
mappingEntry =
  MappingEntry'
    { _meTargetTable = Nothing
    , _meSourceType = Nothing
    , _meSourceTable = Nothing
    , _meTargetType = Nothing
    , _meTargetPath = Nothing
    , _meSourcePath = Nothing
    }


-- | The target table.
meTargetTable :: Lens' MappingEntry (Maybe Text)
meTargetTable = lens _meTargetTable (\ s a -> s{_meTargetTable = a})

-- | The source type.
meSourceType :: Lens' MappingEntry (Maybe Text)
meSourceType = lens _meSourceType (\ s a -> s{_meSourceType = a})

-- | The name of the source table.
meSourceTable :: Lens' MappingEntry (Maybe Text)
meSourceTable = lens _meSourceTable (\ s a -> s{_meSourceTable = a})

-- | The target type.
meTargetType :: Lens' MappingEntry (Maybe Text)
meTargetType = lens _meTargetType (\ s a -> s{_meTargetType = a})

-- | The target path.
meTargetPath :: Lens' MappingEntry (Maybe Text)
meTargetPath = lens _meTargetPath (\ s a -> s{_meTargetPath = a})

-- | The source path.
meSourcePath :: Lens' MappingEntry (Maybe Text)
meSourcePath = lens _meSourcePath (\ s a -> s{_meSourcePath = a})

instance FromJSON MappingEntry where
        parseJSON
          = withObject "MappingEntry"
              (\ x ->
                 MappingEntry' <$>
                   (x .:? "TargetTable") <*> (x .:? "SourceType") <*>
                     (x .:? "SourceTable")
                     <*> (x .:? "TargetType")
                     <*> (x .:? "TargetPath")
                     <*> (x .:? "SourcePath"))

instance Hashable MappingEntry where

instance NFData MappingEntry where

instance ToJSON MappingEntry where
        toJSON MappingEntry'{..}
          = object
              (catMaybes
                 [("TargetTable" .=) <$> _meTargetTable,
                  ("SourceType" .=) <$> _meSourceType,
                  ("SourceTable" .=) <$> _meSourceTable,
                  ("TargetType" .=) <$> _meTargetType,
                  ("TargetPath" .=) <$> _meTargetPath,
                  ("SourcePath" .=) <$> _meSourcePath])

-- | Specifies the sort order of a sorted column.
--
--
--
-- /See:/ 'order' smart constructor.
data Order = Order'
  { _oColumn    :: !Text
  , _oSortOrder :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Order' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oColumn' - The name of the column.
--
-- * 'oSortOrder' - Indicates that the column is sorted in ascending order (@== 1@ ), or in descending order (@==0@ ).
order
    :: Text -- ^ 'oColumn'
    -> Natural -- ^ 'oSortOrder'
    -> Order
order pColumn_ pSortOrder_ =
  Order' {_oColumn = pColumn_, _oSortOrder = _Nat # pSortOrder_}


-- | The name of the column.
oColumn :: Lens' Order Text
oColumn = lens _oColumn (\ s a -> s{_oColumn = a})

-- | Indicates that the column is sorted in ascending order (@== 1@ ), or in descending order (@==0@ ).
oSortOrder :: Lens' Order Natural
oSortOrder = lens _oSortOrder (\ s a -> s{_oSortOrder = a}) . _Nat

instance FromJSON Order where
        parseJSON
          = withObject "Order"
              (\ x ->
                 Order' <$> (x .: "Column") <*> (x .: "SortOrder"))

instance Hashable Order where

instance NFData Order where

instance ToJSON Order where
        toJSON Order'{..}
          = object
              (catMaybes
                 [Just ("Column" .= _oColumn),
                  Just ("SortOrder" .= _oSortOrder)])

-- | Represents a slice of table data.
--
--
--
-- /See:/ 'partition' smart constructor.
data Partition = Partition'
  { _pCreationTime      :: !(Maybe POSIX)
  , _pValues            :: !(Maybe [Text])
  , _pLastAnalyzedTime  :: !(Maybe POSIX)
  , _pStorageDescriptor :: !(Maybe StorageDescriptor)
  , _pDatabaseName      :: !(Maybe Text)
  , _pParameters        :: !(Maybe (Map Text Text))
  , _pLastAccessTime    :: !(Maybe POSIX)
  , _pTableName         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Partition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pCreationTime' - The time at which the partition was created.
--
-- * 'pValues' - The values of the partition.
--
-- * 'pLastAnalyzedTime' - The last time at which column statistics were computed for this partition.
--
-- * 'pStorageDescriptor' - Provides information about the physical location where the partition is stored.
--
-- * 'pDatabaseName' - The name of the catalog database where the table in question is located.
--
-- * 'pParameters' - Partition parameters, in the form of a list of key-value pairs.
--
-- * 'pLastAccessTime' - The last time at which the partition was accessed.
--
-- * 'pTableName' - The name of the table in question.
partition
    :: Partition
partition =
  Partition'
    { _pCreationTime = Nothing
    , _pValues = Nothing
    , _pLastAnalyzedTime = Nothing
    , _pStorageDescriptor = Nothing
    , _pDatabaseName = Nothing
    , _pParameters = Nothing
    , _pLastAccessTime = Nothing
    , _pTableName = Nothing
    }


-- | The time at which the partition was created.
pCreationTime :: Lens' Partition (Maybe UTCTime)
pCreationTime = lens _pCreationTime (\ s a -> s{_pCreationTime = a}) . mapping _Time

-- | The values of the partition.
pValues :: Lens' Partition [Text]
pValues = lens _pValues (\ s a -> s{_pValues = a}) . _Default . _Coerce

-- | The last time at which column statistics were computed for this partition.
pLastAnalyzedTime :: Lens' Partition (Maybe UTCTime)
pLastAnalyzedTime = lens _pLastAnalyzedTime (\ s a -> s{_pLastAnalyzedTime = a}) . mapping _Time

-- | Provides information about the physical location where the partition is stored.
pStorageDescriptor :: Lens' Partition (Maybe StorageDescriptor)
pStorageDescriptor = lens _pStorageDescriptor (\ s a -> s{_pStorageDescriptor = a})

-- | The name of the catalog database where the table in question is located.
pDatabaseName :: Lens' Partition (Maybe Text)
pDatabaseName = lens _pDatabaseName (\ s a -> s{_pDatabaseName = a})

-- | Partition parameters, in the form of a list of key-value pairs.
pParameters :: Lens' Partition (HashMap Text Text)
pParameters = lens _pParameters (\ s a -> s{_pParameters = a}) . _Default . _Map

-- | The last time at which the partition was accessed.
pLastAccessTime :: Lens' Partition (Maybe UTCTime)
pLastAccessTime = lens _pLastAccessTime (\ s a -> s{_pLastAccessTime = a}) . mapping _Time

-- | The name of the table in question.
pTableName :: Lens' Partition (Maybe Text)
pTableName = lens _pTableName (\ s a -> s{_pTableName = a})

instance FromJSON Partition where
        parseJSON
          = withObject "Partition"
              (\ x ->
                 Partition' <$>
                   (x .:? "CreationTime") <*>
                     (x .:? "Values" .!= mempty)
                     <*> (x .:? "LastAnalyzedTime")
                     <*> (x .:? "StorageDescriptor")
                     <*> (x .:? "DatabaseName")
                     <*> (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "LastAccessTime")
                     <*> (x .:? "TableName"))

instance Hashable Partition where

instance NFData Partition where

-- | Contains information about a partition error.
--
--
--
-- /See:/ 'partitionError' smart constructor.
data PartitionError = PartitionError'
  { _pePartitionValues :: !(Maybe [Text])
  , _peErrorDetail     :: !(Maybe ErrorDetail)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PartitionError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pePartitionValues' - The values that define the partition.
--
-- * 'peErrorDetail' - Details about the partition error.
partitionError
    :: PartitionError
partitionError =
  PartitionError' {_pePartitionValues = Nothing, _peErrorDetail = Nothing}


-- | The values that define the partition.
pePartitionValues :: Lens' PartitionError [Text]
pePartitionValues = lens _pePartitionValues (\ s a -> s{_pePartitionValues = a}) . _Default . _Coerce

-- | Details about the partition error.
peErrorDetail :: Lens' PartitionError (Maybe ErrorDetail)
peErrorDetail = lens _peErrorDetail (\ s a -> s{_peErrorDetail = a})

instance FromJSON PartitionError where
        parseJSON
          = withObject "PartitionError"
              (\ x ->
                 PartitionError' <$>
                   (x .:? "PartitionValues" .!= mempty) <*>
                     (x .:? "ErrorDetail"))

instance Hashable PartitionError where

instance NFData PartitionError where

-- | The structure used to create and update a partion.
--
--
--
-- /See:/ 'partitionInput' smart constructor.
data PartitionInput = PartitionInput'
  { _piValues            :: !(Maybe [Text])
  , _piLastAnalyzedTime  :: !(Maybe POSIX)
  , _piStorageDescriptor :: !(Maybe StorageDescriptor)
  , _piParameters        :: !(Maybe (Map Text Text))
  , _piLastAccessTime    :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PartitionInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piValues' - The values of the partition.
--
-- * 'piLastAnalyzedTime' - The last time at which column statistics were computed for this partition.
--
-- * 'piStorageDescriptor' - Provides information about the physical location where the partition is stored.
--
-- * 'piParameters' - Partition parameters, in the form of a list of key-value pairs.
--
-- * 'piLastAccessTime' - The last time at which the partition was accessed.
partitionInput
    :: PartitionInput
partitionInput =
  PartitionInput'
    { _piValues = Nothing
    , _piLastAnalyzedTime = Nothing
    , _piStorageDescriptor = Nothing
    , _piParameters = Nothing
    , _piLastAccessTime = Nothing
    }


-- | The values of the partition.
piValues :: Lens' PartitionInput [Text]
piValues = lens _piValues (\ s a -> s{_piValues = a}) . _Default . _Coerce

-- | The last time at which column statistics were computed for this partition.
piLastAnalyzedTime :: Lens' PartitionInput (Maybe UTCTime)
piLastAnalyzedTime = lens _piLastAnalyzedTime (\ s a -> s{_piLastAnalyzedTime = a}) . mapping _Time

-- | Provides information about the physical location where the partition is stored.
piStorageDescriptor :: Lens' PartitionInput (Maybe StorageDescriptor)
piStorageDescriptor = lens _piStorageDescriptor (\ s a -> s{_piStorageDescriptor = a})

-- | Partition parameters, in the form of a list of key-value pairs.
piParameters :: Lens' PartitionInput (HashMap Text Text)
piParameters = lens _piParameters (\ s a -> s{_piParameters = a}) . _Default . _Map

-- | The last time at which the partition was accessed.
piLastAccessTime :: Lens' PartitionInput (Maybe UTCTime)
piLastAccessTime = lens _piLastAccessTime (\ s a -> s{_piLastAccessTime = a}) . mapping _Time

instance Hashable PartitionInput where

instance NFData PartitionInput where

instance ToJSON PartitionInput where
        toJSON PartitionInput'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _piValues,
                  ("LastAnalyzedTime" .=) <$> _piLastAnalyzedTime,
                  ("StorageDescriptor" .=) <$> _piStorageDescriptor,
                  ("Parameters" .=) <$> _piParameters,
                  ("LastAccessTime" .=) <$> _piLastAccessTime])

-- | Contains a list of values defining partitions.
--
--
--
-- /See:/ 'partitionValueList' smart constructor.
newtype PartitionValueList = PartitionValueList'
  { _pvlValues :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PartitionValueList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvlValues' - The list of values.
partitionValueList
    :: PartitionValueList
partitionValueList = PartitionValueList' {_pvlValues = mempty}


-- | The list of values.
pvlValues :: Lens' PartitionValueList [Text]
pvlValues = lens _pvlValues (\ s a -> s{_pvlValues = a}) . _Coerce

instance FromJSON PartitionValueList where
        parseJSON
          = withObject "PartitionValueList"
              (\ x ->
                 PartitionValueList' <$> (x .:? "Values" .!= mempty))

instance Hashable PartitionValueList where

instance NFData PartitionValueList where

instance ToJSON PartitionValueList where
        toJSON PartitionValueList'{..}
          = object (catMaybes [Just ("Values" .= _pvlValues)])

-- | Specifies the physical requirements for a connection.
--
--
--
-- /See:/ 'physicalConnectionRequirements' smart constructor.
data PhysicalConnectionRequirements = PhysicalConnectionRequirements'
  { _pcrSecurityGroupIdList :: !(Maybe [Text])
  , _pcrSubnetId            :: !(Maybe Text)
  , _pcrAvailabilityZone    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PhysicalConnectionRequirements' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcrSecurityGroupIdList' - The security group ID list used by the connection.
--
-- * 'pcrSubnetId' - The subnet ID used by the connection.
--
-- * 'pcrAvailabilityZone' - The connection's availability zone. This field is deprecated and has no effect.
physicalConnectionRequirements
    :: PhysicalConnectionRequirements
physicalConnectionRequirements =
  PhysicalConnectionRequirements'
    { _pcrSecurityGroupIdList = Nothing
    , _pcrSubnetId = Nothing
    , _pcrAvailabilityZone = Nothing
    }


-- | The security group ID list used by the connection.
pcrSecurityGroupIdList :: Lens' PhysicalConnectionRequirements [Text]
pcrSecurityGroupIdList = lens _pcrSecurityGroupIdList (\ s a -> s{_pcrSecurityGroupIdList = a}) . _Default . _Coerce

-- | The subnet ID used by the connection.
pcrSubnetId :: Lens' PhysicalConnectionRequirements (Maybe Text)
pcrSubnetId = lens _pcrSubnetId (\ s a -> s{_pcrSubnetId = a})

-- | The connection's availability zone. This field is deprecated and has no effect.
pcrAvailabilityZone :: Lens' PhysicalConnectionRequirements (Maybe Text)
pcrAvailabilityZone = lens _pcrAvailabilityZone (\ s a -> s{_pcrAvailabilityZone = a})

instance FromJSON PhysicalConnectionRequirements
         where
        parseJSON
          = withObject "PhysicalConnectionRequirements"
              (\ x ->
                 PhysicalConnectionRequirements' <$>
                   (x .:? "SecurityGroupIdList" .!= mempty) <*>
                     (x .:? "SubnetId")
                     <*> (x .:? "AvailabilityZone"))

instance Hashable PhysicalConnectionRequirements
         where

instance NFData PhysicalConnectionRequirements where

instance ToJSON PhysicalConnectionRequirements where
        toJSON PhysicalConnectionRequirements'{..}
          = object
              (catMaybes
                 [("SecurityGroupIdList" .=) <$>
                    _pcrSecurityGroupIdList,
                  ("SubnetId" .=) <$> _pcrSubnetId,
                  ("AvailabilityZone" .=) <$> _pcrAvailabilityZone])

-- | A job run that was used in the predicate of a conditional trigger that triggered this job run.
--
--
--
-- /See:/ 'predecessor' smart constructor.
data Predecessor = Predecessor'
  { _pJobName :: !(Maybe Text)
  , _pRunId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Predecessor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pJobName' - The name of the job definition used by the predecessor job run.
--
-- * 'pRunId' - The job-run ID of the predecessor job run.
predecessor
    :: Predecessor
predecessor = Predecessor' {_pJobName = Nothing, _pRunId = Nothing}


-- | The name of the job definition used by the predecessor job run.
pJobName :: Lens' Predecessor (Maybe Text)
pJobName = lens _pJobName (\ s a -> s{_pJobName = a})

-- | The job-run ID of the predecessor job run.
pRunId :: Lens' Predecessor (Maybe Text)
pRunId = lens _pRunId (\ s a -> s{_pRunId = a})

instance FromJSON Predecessor where
        parseJSON
          = withObject "Predecessor"
              (\ x ->
                 Predecessor' <$>
                   (x .:? "JobName") <*> (x .:? "RunId"))

instance Hashable Predecessor where

instance NFData Predecessor where

-- | Defines the predicate of the trigger, which determines when it fires.
--
--
--
-- /See:/ 'predicate' smart constructor.
data Predicate = Predicate'
  { _pLogical    :: !(Maybe Logical)
  , _pConditions :: !(Maybe [Condition])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Predicate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pLogical' - Optional field if only one condition is listed. If multiple conditions are listed, then this field is required.
--
-- * 'pConditions' - A list of the conditions that determine when the trigger will fire.
predicate
    :: Predicate
predicate = Predicate' {_pLogical = Nothing, _pConditions = Nothing}


-- | Optional field if only one condition is listed. If multiple conditions are listed, then this field is required.
pLogical :: Lens' Predicate (Maybe Logical)
pLogical = lens _pLogical (\ s a -> s{_pLogical = a})

-- | A list of the conditions that determine when the trigger will fire.
pConditions :: Lens' Predicate [Condition]
pConditions = lens _pConditions (\ s a -> s{_pConditions = a}) . _Default . _Coerce

instance FromJSON Predicate where
        parseJSON
          = withObject "Predicate"
              (\ x ->
                 Predicate' <$>
                   (x .:? "Logical") <*>
                     (x .:? "Conditions" .!= mempty))

instance Hashable Predicate where

instance NFData Predicate where

instance ToJSON Predicate where
        toJSON Predicate'{..}
          = object
              (catMaybes
                 [("Logical" .=) <$> _pLogical,
                  ("Conditions" .=) <$> _pConditions])

-- | URIs for function resources.
--
--
--
-- /See:/ 'resourceURI' smart constructor.
data ResourceURI = ResourceURI'
  { _ruResourceType :: !(Maybe ResourceType)
  , _ruURI          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceURI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ruResourceType' - The type of the resource.
--
-- * 'ruURI' - The URI for accessing the resource.
resourceURI
    :: ResourceURI
resourceURI = ResourceURI' {_ruResourceType = Nothing, _ruURI = Nothing}


-- | The type of the resource.
ruResourceType :: Lens' ResourceURI (Maybe ResourceType)
ruResourceType = lens _ruResourceType (\ s a -> s{_ruResourceType = a})

-- | The URI for accessing the resource.
ruURI :: Lens' ResourceURI (Maybe Text)
ruURI = lens _ruURI (\ s a -> s{_ruURI = a})

instance FromJSON ResourceURI where
        parseJSON
          = withObject "ResourceURI"
              (\ x ->
                 ResourceURI' <$>
                   (x .:? "ResourceType") <*> (x .:? "Uri"))

instance Hashable ResourceURI where

instance NFData ResourceURI where

instance ToJSON ResourceURI where
        toJSON ResourceURI'{..}
          = object
              (catMaybes
                 [("ResourceType" .=) <$> _ruResourceType,
                  ("Uri" .=) <$> _ruURI])

-- | Specifies a data store in Amazon S3.
--
--
--
-- /See:/ 's3Target' smart constructor.
data S3Target = S3Target'
  { _stPath       :: !(Maybe Text)
  , _stExclusions :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3Target' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stPath' - The path to the Amazon S3 target.
--
-- * 'stExclusions' - A list of glob patterns used to exclude from the crawl. For more information, see <http://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler> .
s3Target
    :: S3Target
s3Target = S3Target' {_stPath = Nothing, _stExclusions = Nothing}


-- | The path to the Amazon S3 target.
stPath :: Lens' S3Target (Maybe Text)
stPath = lens _stPath (\ s a -> s{_stPath = a})

-- | A list of glob patterns used to exclude from the crawl. For more information, see <http://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler> .
stExclusions :: Lens' S3Target [Text]
stExclusions = lens _stExclusions (\ s a -> s{_stExclusions = a}) . _Default . _Coerce

instance FromJSON S3Target where
        parseJSON
          = withObject "S3Target"
              (\ x ->
                 S3Target' <$>
                   (x .:? "Path") <*> (x .:? "Exclusions" .!= mempty))

instance Hashable S3Target where

instance NFData S3Target where

instance ToJSON S3Target where
        toJSON S3Target'{..}
          = object
              (catMaybes
                 [("Path" .=) <$> _stPath,
                  ("Exclusions" .=) <$> _stExclusions])

-- | A scheduling object using a @cron@ statement to schedule an event.
--
--
--
-- /See:/ 'schedule' smart constructor.
data Schedule = Schedule'
  { _sState              :: !(Maybe ScheduleState)
  , _sScheduleExpression :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Schedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sState' - The state of the schedule.
--
-- * 'sScheduleExpression' - A @cron@ expression used to specify the schedule (see <http://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
schedule
    :: Schedule
schedule = Schedule' {_sState = Nothing, _sScheduleExpression = Nothing}


-- | The state of the schedule.
sState :: Lens' Schedule (Maybe ScheduleState)
sState = lens _sState (\ s a -> s{_sState = a})

-- | A @cron@ expression used to specify the schedule (see <http://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
sScheduleExpression :: Lens' Schedule (Maybe Text)
sScheduleExpression = lens _sScheduleExpression (\ s a -> s{_sScheduleExpression = a})

instance FromJSON Schedule where
        parseJSON
          = withObject "Schedule"
              (\ x ->
                 Schedule' <$>
                   (x .:? "State") <*> (x .:? "ScheduleExpression"))

instance Hashable Schedule where

instance NFData Schedule where

-- | Crawler policy for update and deletion behavior.
--
--
--
-- /See:/ 'schemaChangePolicy' smart constructor.
data SchemaChangePolicy = SchemaChangePolicy'
  { _scpDeleteBehavior :: !(Maybe DeleteBehavior)
  , _scpUpdateBehavior :: !(Maybe UpdateBehavior)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SchemaChangePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scpDeleteBehavior' - The deletion behavior when the crawler finds a deleted object.
--
-- * 'scpUpdateBehavior' - The update behavior when the crawler finds a changed schema.
schemaChangePolicy
    :: SchemaChangePolicy
schemaChangePolicy =
  SchemaChangePolicy'
    {_scpDeleteBehavior = Nothing, _scpUpdateBehavior = Nothing}


-- | The deletion behavior when the crawler finds a deleted object.
scpDeleteBehavior :: Lens' SchemaChangePolicy (Maybe DeleteBehavior)
scpDeleteBehavior = lens _scpDeleteBehavior (\ s a -> s{_scpDeleteBehavior = a})

-- | The update behavior when the crawler finds a changed schema.
scpUpdateBehavior :: Lens' SchemaChangePolicy (Maybe UpdateBehavior)
scpUpdateBehavior = lens _scpUpdateBehavior (\ s a -> s{_scpUpdateBehavior = a})

instance FromJSON SchemaChangePolicy where
        parseJSON
          = withObject "SchemaChangePolicy"
              (\ x ->
                 SchemaChangePolicy' <$>
                   (x .:? "DeleteBehavior") <*>
                     (x .:? "UpdateBehavior"))

instance Hashable SchemaChangePolicy where

instance NFData SchemaChangePolicy where

instance ToJSON SchemaChangePolicy where
        toJSON SchemaChangePolicy'{..}
          = object
              (catMaybes
                 [("DeleteBehavior" .=) <$> _scpDeleteBehavior,
                  ("UpdateBehavior" .=) <$> _scpUpdateBehavior])

-- | Defines a non-overlapping region of a table's partitions, allowing multiple requests to be executed in parallel.
--
--
--
-- /See:/ 'segment' smart constructor.
data Segment = Segment'
  { _sSegmentNumber :: !Nat
  , _sTotalSegments :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Segment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSegmentNumber' - The zero-based index number of the this segment. For example, if the total number of segments is 4, SegmentNumber values will range from zero through three.
--
-- * 'sTotalSegments' - The total numer of segments.
segment
    :: Natural -- ^ 'sSegmentNumber'
    -> Natural -- ^ 'sTotalSegments'
    -> Segment
segment pSegmentNumber_ pTotalSegments_ =
  Segment'
    { _sSegmentNumber = _Nat # pSegmentNumber_
    , _sTotalSegments = _Nat # pTotalSegments_
    }


-- | The zero-based index number of the this segment. For example, if the total number of segments is 4, SegmentNumber values will range from zero through three.
sSegmentNumber :: Lens' Segment Natural
sSegmentNumber = lens _sSegmentNumber (\ s a -> s{_sSegmentNumber = a}) . _Nat

-- | The total numer of segments.
sTotalSegments :: Lens' Segment Natural
sTotalSegments = lens _sTotalSegments (\ s a -> s{_sTotalSegments = a}) . _Nat

instance Hashable Segment where

instance NFData Segment where

instance ToJSON Segment where
        toJSON Segment'{..}
          = object
              (catMaybes
                 [Just ("SegmentNumber" .= _sSegmentNumber),
                  Just ("TotalSegments" .= _sTotalSegments)])

-- | Information about a serialization/deserialization program (SerDe) which serves as an extractor and loader.
--
--
--
-- /See:/ 'serDeInfo' smart constructor.
data SerDeInfo = SerDeInfo'
  { _sdiSerializationLibrary :: !(Maybe Text)
  , _sdiName                 :: !(Maybe Text)
  , _sdiParameters           :: !(Maybe (Map Text Text))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SerDeInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdiSerializationLibrary' - Usually the class that implements the SerDe. An example is: @org.apache.hadoop.hive.serde2.columnar.ColumnarSerDe@ .
--
-- * 'sdiName' - Name of the SerDe.
--
-- * 'sdiParameters' - A list of initialization parameters for the SerDe, in key-value form.
serDeInfo
    :: SerDeInfo
serDeInfo =
  SerDeInfo'
    { _sdiSerializationLibrary = Nothing
    , _sdiName = Nothing
    , _sdiParameters = Nothing
    }


-- | Usually the class that implements the SerDe. An example is: @org.apache.hadoop.hive.serde2.columnar.ColumnarSerDe@ .
sdiSerializationLibrary :: Lens' SerDeInfo (Maybe Text)
sdiSerializationLibrary = lens _sdiSerializationLibrary (\ s a -> s{_sdiSerializationLibrary = a})

-- | Name of the SerDe.
sdiName :: Lens' SerDeInfo (Maybe Text)
sdiName = lens _sdiName (\ s a -> s{_sdiName = a})

-- | A list of initialization parameters for the SerDe, in key-value form.
sdiParameters :: Lens' SerDeInfo (HashMap Text Text)
sdiParameters = lens _sdiParameters (\ s a -> s{_sdiParameters = a}) . _Default . _Map

instance FromJSON SerDeInfo where
        parseJSON
          = withObject "SerDeInfo"
              (\ x ->
                 SerDeInfo' <$>
                   (x .:? "SerializationLibrary") <*> (x .:? "Name") <*>
                     (x .:? "Parameters" .!= mempty))

instance Hashable SerDeInfo where

instance NFData SerDeInfo where

instance ToJSON SerDeInfo where
        toJSON SerDeInfo'{..}
          = object
              (catMaybes
                 [("SerializationLibrary" .=) <$>
                    _sdiSerializationLibrary,
                  ("Name" .=) <$> _sdiName,
                  ("Parameters" .=) <$> _sdiParameters])

-- | Specifies skewed values in a table. Skewed are ones that occur with very high frequency.
--
--
--
-- /See:/ 'skewedInfo' smart constructor.
data SkewedInfo = SkewedInfo'
  { _siSkewedColumnValueLocationMaps :: !(Maybe (Map Text Text))
  , _siSkewedColumnValues            :: !(Maybe [Text])
  , _siSkewedColumnNames             :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SkewedInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siSkewedColumnValueLocationMaps' - A mapping of skewed values to the columns that contain them.
--
-- * 'siSkewedColumnValues' - A list of values that appear so frequently as to be considered skewed.
--
-- * 'siSkewedColumnNames' - A list of names of columns that contain skewed values.
skewedInfo
    :: SkewedInfo
skewedInfo =
  SkewedInfo'
    { _siSkewedColumnValueLocationMaps = Nothing
    , _siSkewedColumnValues = Nothing
    , _siSkewedColumnNames = Nothing
    }


-- | A mapping of skewed values to the columns that contain them.
siSkewedColumnValueLocationMaps :: Lens' SkewedInfo (HashMap Text Text)
siSkewedColumnValueLocationMaps = lens _siSkewedColumnValueLocationMaps (\ s a -> s{_siSkewedColumnValueLocationMaps = a}) . _Default . _Map

-- | A list of values that appear so frequently as to be considered skewed.
siSkewedColumnValues :: Lens' SkewedInfo [Text]
siSkewedColumnValues = lens _siSkewedColumnValues (\ s a -> s{_siSkewedColumnValues = a}) . _Default . _Coerce

-- | A list of names of columns that contain skewed values.
siSkewedColumnNames :: Lens' SkewedInfo [Text]
siSkewedColumnNames = lens _siSkewedColumnNames (\ s a -> s{_siSkewedColumnNames = a}) . _Default . _Coerce

instance FromJSON SkewedInfo where
        parseJSON
          = withObject "SkewedInfo"
              (\ x ->
                 SkewedInfo' <$>
                   (x .:? "SkewedColumnValueLocationMaps" .!= mempty)
                     <*> (x .:? "SkewedColumnValues" .!= mempty)
                     <*> (x .:? "SkewedColumnNames" .!= mempty))

instance Hashable SkewedInfo where

instance NFData SkewedInfo where

instance ToJSON SkewedInfo where
        toJSON SkewedInfo'{..}
          = object
              (catMaybes
                 [("SkewedColumnValueLocationMaps" .=) <$>
                    _siSkewedColumnValueLocationMaps,
                  ("SkewedColumnValues" .=) <$> _siSkewedColumnValues,
                  ("SkewedColumnNames" .=) <$> _siSkewedColumnNames])

-- | Describes the physical storage of table data.
--
--
--
-- /See:/ 'storageDescriptor' smart constructor.
data StorageDescriptor = StorageDescriptor'
  { _sdSortColumns            :: !(Maybe [Order])
  , _sdCompressed             :: !(Maybe Bool)
  , _sdLocation               :: !(Maybe Text)
  , _sdBucketColumns          :: !(Maybe [Text])
  , _sdSerdeInfo              :: !(Maybe SerDeInfo)
  , _sdOutputFormat           :: !(Maybe Text)
  , _sdNumberOfBuckets        :: !(Maybe Int)
  , _sdStoredAsSubDirectories :: !(Maybe Bool)
  , _sdParameters             :: !(Maybe (Map Text Text))
  , _sdInputFormat            :: !(Maybe Text)
  , _sdSkewedInfo             :: !(Maybe SkewedInfo)
  , _sdColumns                :: !(Maybe [Column])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StorageDescriptor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdSortColumns' - A list specifying the sort order of each bucket in the table.
--
-- * 'sdCompressed' - True if the data in the table is compressed, or False if not.
--
-- * 'sdLocation' - The physical location of the table. By default this takes the form of the warehouse location, followed by the database location in the warehouse, followed by the table name.
--
-- * 'sdBucketColumns' - A list of reducer grouping columns, clustering columns, and bucketing columns in the table.
--
-- * 'sdSerdeInfo' - Serialization/deserialization (SerDe) information.
--
-- * 'sdOutputFormat' - The output format: @SequenceFileOutputFormat@ (binary), or @IgnoreKeyTextOutputFormat@ , or a custom format.
--
-- * 'sdNumberOfBuckets' - Must be specified if the table contains any dimension columns.
--
-- * 'sdStoredAsSubDirectories' - True if the table data is stored in subdirectories, or False if not.
--
-- * 'sdParameters' - User-supplied properties in key-value form.
--
-- * 'sdInputFormat' - The input format: @SequenceFileInputFormat@ (binary), or @TextInputFormat@ , or a custom format.
--
-- * 'sdSkewedInfo' - Information about values that appear very frequently in a column (skewed values).
--
-- * 'sdColumns' - A list of the @Columns@ in the table.
storageDescriptor
    :: StorageDescriptor
storageDescriptor =
  StorageDescriptor'
    { _sdSortColumns = Nothing
    , _sdCompressed = Nothing
    , _sdLocation = Nothing
    , _sdBucketColumns = Nothing
    , _sdSerdeInfo = Nothing
    , _sdOutputFormat = Nothing
    , _sdNumberOfBuckets = Nothing
    , _sdStoredAsSubDirectories = Nothing
    , _sdParameters = Nothing
    , _sdInputFormat = Nothing
    , _sdSkewedInfo = Nothing
    , _sdColumns = Nothing
    }


-- | A list specifying the sort order of each bucket in the table.
sdSortColumns :: Lens' StorageDescriptor [Order]
sdSortColumns = lens _sdSortColumns (\ s a -> s{_sdSortColumns = a}) . _Default . _Coerce

-- | True if the data in the table is compressed, or False if not.
sdCompressed :: Lens' StorageDescriptor (Maybe Bool)
sdCompressed = lens _sdCompressed (\ s a -> s{_sdCompressed = a})

-- | The physical location of the table. By default this takes the form of the warehouse location, followed by the database location in the warehouse, followed by the table name.
sdLocation :: Lens' StorageDescriptor (Maybe Text)
sdLocation = lens _sdLocation (\ s a -> s{_sdLocation = a})

-- | A list of reducer grouping columns, clustering columns, and bucketing columns in the table.
sdBucketColumns :: Lens' StorageDescriptor [Text]
sdBucketColumns = lens _sdBucketColumns (\ s a -> s{_sdBucketColumns = a}) . _Default . _Coerce

-- | Serialization/deserialization (SerDe) information.
sdSerdeInfo :: Lens' StorageDescriptor (Maybe SerDeInfo)
sdSerdeInfo = lens _sdSerdeInfo (\ s a -> s{_sdSerdeInfo = a})

-- | The output format: @SequenceFileOutputFormat@ (binary), or @IgnoreKeyTextOutputFormat@ , or a custom format.
sdOutputFormat :: Lens' StorageDescriptor (Maybe Text)
sdOutputFormat = lens _sdOutputFormat (\ s a -> s{_sdOutputFormat = a})

-- | Must be specified if the table contains any dimension columns.
sdNumberOfBuckets :: Lens' StorageDescriptor (Maybe Int)
sdNumberOfBuckets = lens _sdNumberOfBuckets (\ s a -> s{_sdNumberOfBuckets = a})

-- | True if the table data is stored in subdirectories, or False if not.
sdStoredAsSubDirectories :: Lens' StorageDescriptor (Maybe Bool)
sdStoredAsSubDirectories = lens _sdStoredAsSubDirectories (\ s a -> s{_sdStoredAsSubDirectories = a})

-- | User-supplied properties in key-value form.
sdParameters :: Lens' StorageDescriptor (HashMap Text Text)
sdParameters = lens _sdParameters (\ s a -> s{_sdParameters = a}) . _Default . _Map

-- | The input format: @SequenceFileInputFormat@ (binary), or @TextInputFormat@ , or a custom format.
sdInputFormat :: Lens' StorageDescriptor (Maybe Text)
sdInputFormat = lens _sdInputFormat (\ s a -> s{_sdInputFormat = a})

-- | Information about values that appear very frequently in a column (skewed values).
sdSkewedInfo :: Lens' StorageDescriptor (Maybe SkewedInfo)
sdSkewedInfo = lens _sdSkewedInfo (\ s a -> s{_sdSkewedInfo = a})

-- | A list of the @Columns@ in the table.
sdColumns :: Lens' StorageDescriptor [Column]
sdColumns = lens _sdColumns (\ s a -> s{_sdColumns = a}) . _Default . _Coerce

instance FromJSON StorageDescriptor where
        parseJSON
          = withObject "StorageDescriptor"
              (\ x ->
                 StorageDescriptor' <$>
                   (x .:? "SortColumns" .!= mempty) <*>
                     (x .:? "Compressed")
                     <*> (x .:? "Location")
                     <*> (x .:? "BucketColumns" .!= mempty)
                     <*> (x .:? "SerdeInfo")
                     <*> (x .:? "OutputFormat")
                     <*> (x .:? "NumberOfBuckets")
                     <*> (x .:? "StoredAsSubDirectories")
                     <*> (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "InputFormat")
                     <*> (x .:? "SkewedInfo")
                     <*> (x .:? "Columns" .!= mempty))

instance Hashable StorageDescriptor where

instance NFData StorageDescriptor where

instance ToJSON StorageDescriptor where
        toJSON StorageDescriptor'{..}
          = object
              (catMaybes
                 [("SortColumns" .=) <$> _sdSortColumns,
                  ("Compressed" .=) <$> _sdCompressed,
                  ("Location" .=) <$> _sdLocation,
                  ("BucketColumns" .=) <$> _sdBucketColumns,
                  ("SerdeInfo" .=) <$> _sdSerdeInfo,
                  ("OutputFormat" .=) <$> _sdOutputFormat,
                  ("NumberOfBuckets" .=) <$> _sdNumberOfBuckets,
                  ("StoredAsSubDirectories" .=) <$>
                    _sdStoredAsSubDirectories,
                  ("Parameters" .=) <$> _sdParameters,
                  ("InputFormat" .=) <$> _sdInputFormat,
                  ("SkewedInfo" .=) <$> _sdSkewedInfo,
                  ("Columns" .=) <$> _sdColumns])

-- | Represents a collection of related data organized in columns and rows.
--
--
--
-- /See:/ 'table' smart constructor.
data Table = Table'
  { _tRetention         :: !(Maybe Nat)
  , _tCreatedBy         :: !(Maybe Text)
  , _tTableType         :: !(Maybe Text)
  , _tOwner             :: !(Maybe Text)
  , _tViewOriginalText  :: !(Maybe Text)
  , _tUpdateTime        :: !(Maybe POSIX)
  , _tViewExpandedText  :: !(Maybe Text)
  , _tLastAnalyzedTime  :: !(Maybe POSIX)
  , _tStorageDescriptor :: !(Maybe StorageDescriptor)
  , _tDatabaseName      :: !(Maybe Text)
  , _tParameters        :: !(Maybe (Map Text Text))
  , _tLastAccessTime    :: !(Maybe POSIX)
  , _tDescription       :: !(Maybe Text)
  , _tPartitionKeys     :: !(Maybe [Column])
  , _tCreateTime        :: !(Maybe POSIX)
  , _tName              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Table' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tRetention' - Retention time for this table.
--
-- * 'tCreatedBy' - Person or entity who created the table.
--
-- * 'tTableType' - The type of this table (@EXTERNAL_TABLE@ , @VIRTUAL_VIEW@ , etc.).
--
-- * 'tOwner' - Owner of the table.
--
-- * 'tViewOriginalText' - If the table is a view, the original text of the view; otherwise @null@ .
--
-- * 'tUpdateTime' - Last time the table was updated.
--
-- * 'tViewExpandedText' - If the table is a view, the expanded text of the view; otherwise @null@ .
--
-- * 'tLastAnalyzedTime' - Last time column statistics were computed for this table.
--
-- * 'tStorageDescriptor' - A storage descriptor containing information about the physical storage of this table.
--
-- * 'tDatabaseName' - Name of the metadata database where the table metadata resides. For Hive compatibility, this must be all lowercase.
--
-- * 'tParameters' - Properties associated with this table, as a list of key-value pairs.
--
-- * 'tLastAccessTime' - Last time the table was accessed. This is usually taken from HDFS, and may not be reliable.
--
-- * 'tDescription' - Description of the table.
--
-- * 'tPartitionKeys' - A list of columns by which the table is partitioned. Only primitive types are supported as partition keys.
--
-- * 'tCreateTime' - Time when the table definition was created in the Data Catalog.
--
-- * 'tName' - Name of the table. For Hive compatibility, this must be entirely lowercase.
table
    :: Text -- ^ 'tName'
    -> Table
table pName_ =
  Table'
    { _tRetention = Nothing
    , _tCreatedBy = Nothing
    , _tTableType = Nothing
    , _tOwner = Nothing
    , _tViewOriginalText = Nothing
    , _tUpdateTime = Nothing
    , _tViewExpandedText = Nothing
    , _tLastAnalyzedTime = Nothing
    , _tStorageDescriptor = Nothing
    , _tDatabaseName = Nothing
    , _tParameters = Nothing
    , _tLastAccessTime = Nothing
    , _tDescription = Nothing
    , _tPartitionKeys = Nothing
    , _tCreateTime = Nothing
    , _tName = pName_
    }


-- | Retention time for this table.
tRetention :: Lens' Table (Maybe Natural)
tRetention = lens _tRetention (\ s a -> s{_tRetention = a}) . mapping _Nat

-- | Person or entity who created the table.
tCreatedBy :: Lens' Table (Maybe Text)
tCreatedBy = lens _tCreatedBy (\ s a -> s{_tCreatedBy = a})

-- | The type of this table (@EXTERNAL_TABLE@ , @VIRTUAL_VIEW@ , etc.).
tTableType :: Lens' Table (Maybe Text)
tTableType = lens _tTableType (\ s a -> s{_tTableType = a})

-- | Owner of the table.
tOwner :: Lens' Table (Maybe Text)
tOwner = lens _tOwner (\ s a -> s{_tOwner = a})

-- | If the table is a view, the original text of the view; otherwise @null@ .
tViewOriginalText :: Lens' Table (Maybe Text)
tViewOriginalText = lens _tViewOriginalText (\ s a -> s{_tViewOriginalText = a})

-- | Last time the table was updated.
tUpdateTime :: Lens' Table (Maybe UTCTime)
tUpdateTime = lens _tUpdateTime (\ s a -> s{_tUpdateTime = a}) . mapping _Time

-- | If the table is a view, the expanded text of the view; otherwise @null@ .
tViewExpandedText :: Lens' Table (Maybe Text)
tViewExpandedText = lens _tViewExpandedText (\ s a -> s{_tViewExpandedText = a})

-- | Last time column statistics were computed for this table.
tLastAnalyzedTime :: Lens' Table (Maybe UTCTime)
tLastAnalyzedTime = lens _tLastAnalyzedTime (\ s a -> s{_tLastAnalyzedTime = a}) . mapping _Time

-- | A storage descriptor containing information about the physical storage of this table.
tStorageDescriptor :: Lens' Table (Maybe StorageDescriptor)
tStorageDescriptor = lens _tStorageDescriptor (\ s a -> s{_tStorageDescriptor = a})

-- | Name of the metadata database where the table metadata resides. For Hive compatibility, this must be all lowercase.
tDatabaseName :: Lens' Table (Maybe Text)
tDatabaseName = lens _tDatabaseName (\ s a -> s{_tDatabaseName = a})

-- | Properties associated with this table, as a list of key-value pairs.
tParameters :: Lens' Table (HashMap Text Text)
tParameters = lens _tParameters (\ s a -> s{_tParameters = a}) . _Default . _Map

-- | Last time the table was accessed. This is usually taken from HDFS, and may not be reliable.
tLastAccessTime :: Lens' Table (Maybe UTCTime)
tLastAccessTime = lens _tLastAccessTime (\ s a -> s{_tLastAccessTime = a}) . mapping _Time

-- | Description of the table.
tDescription :: Lens' Table (Maybe Text)
tDescription = lens _tDescription (\ s a -> s{_tDescription = a})

-- | A list of columns by which the table is partitioned. Only primitive types are supported as partition keys.
tPartitionKeys :: Lens' Table [Column]
tPartitionKeys = lens _tPartitionKeys (\ s a -> s{_tPartitionKeys = a}) . _Default . _Coerce

-- | Time when the table definition was created in the Data Catalog.
tCreateTime :: Lens' Table (Maybe UTCTime)
tCreateTime = lens _tCreateTime (\ s a -> s{_tCreateTime = a}) . mapping _Time

-- | Name of the table. For Hive compatibility, this must be entirely lowercase.
tName :: Lens' Table Text
tName = lens _tName (\ s a -> s{_tName = a})

instance FromJSON Table where
        parseJSON
          = withObject "Table"
              (\ x ->
                 Table' <$>
                   (x .:? "Retention") <*> (x .:? "CreatedBy") <*>
                     (x .:? "TableType")
                     <*> (x .:? "Owner")
                     <*> (x .:? "ViewOriginalText")
                     <*> (x .:? "UpdateTime")
                     <*> (x .:? "ViewExpandedText")
                     <*> (x .:? "LastAnalyzedTime")
                     <*> (x .:? "StorageDescriptor")
                     <*> (x .:? "DatabaseName")
                     <*> (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "LastAccessTime")
                     <*> (x .:? "Description")
                     <*> (x .:? "PartitionKeys" .!= mempty)
                     <*> (x .:? "CreateTime")
                     <*> (x .: "Name"))

instance Hashable Table where

instance NFData Table where

-- | An error record for table operations.
--
--
--
-- /See:/ 'tableError' smart constructor.
data TableError = TableError'
  { _teTableName   :: !(Maybe Text)
  , _teErrorDetail :: !(Maybe ErrorDetail)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TableError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'teTableName' - Name of the table. For Hive compatibility, this must be entirely lowercase.
--
-- * 'teErrorDetail' - Detail about the error.
tableError
    :: TableError
tableError = TableError' {_teTableName = Nothing, _teErrorDetail = Nothing}


-- | Name of the table. For Hive compatibility, this must be entirely lowercase.
teTableName :: Lens' TableError (Maybe Text)
teTableName = lens _teTableName (\ s a -> s{_teTableName = a})

-- | Detail about the error.
teErrorDetail :: Lens' TableError (Maybe ErrorDetail)
teErrorDetail = lens _teErrorDetail (\ s a -> s{_teErrorDetail = a})

instance FromJSON TableError where
        parseJSON
          = withObject "TableError"
              (\ x ->
                 TableError' <$>
                   (x .:? "TableName") <*> (x .:? "ErrorDetail"))

instance Hashable TableError where

instance NFData TableError where

-- | Structure used to create or update the table.
--
--
--
-- /See:/ 'tableInput' smart constructor.
data TableInput = TableInput'
  { _tiRetention         :: !(Maybe Nat)
  , _tiTableType         :: !(Maybe Text)
  , _tiOwner             :: !(Maybe Text)
  , _tiViewOriginalText  :: !(Maybe Text)
  , _tiViewExpandedText  :: !(Maybe Text)
  , _tiLastAnalyzedTime  :: !(Maybe POSIX)
  , _tiStorageDescriptor :: !(Maybe StorageDescriptor)
  , _tiParameters        :: !(Maybe (Map Text Text))
  , _tiLastAccessTime    :: !(Maybe POSIX)
  , _tiDescription       :: !(Maybe Text)
  , _tiPartitionKeys     :: !(Maybe [Column])
  , _tiName              :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TableInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiRetention' - Retention time for this table.
--
-- * 'tiTableType' - The type of this table (@EXTERNAL_TABLE@ , @VIRTUAL_VIEW@ , etc.).
--
-- * 'tiOwner' - Owner of the table.
--
-- * 'tiViewOriginalText' - If the table is a view, the original text of the view; otherwise @null@ .
--
-- * 'tiViewExpandedText' - If the table is a view, the expanded text of the view; otherwise @null@ .
--
-- * 'tiLastAnalyzedTime' - Last time column statistics were computed for this table.
--
-- * 'tiStorageDescriptor' - A storage descriptor containing information about the physical storage of this table.
--
-- * 'tiParameters' - Properties associated with this table, as a list of key-value pairs.
--
-- * 'tiLastAccessTime' - Last time the table was accessed.
--
-- * 'tiDescription' - Description of the table.
--
-- * 'tiPartitionKeys' - A list of columns by which the table is partitioned. Only primitive types are supported as partition keys.
--
-- * 'tiName' - Name of the table. For Hive compatibility, this is folded to lowercase when it is stored.
tableInput
    :: Text -- ^ 'tiName'
    -> TableInput
tableInput pName_ =
  TableInput'
    { _tiRetention = Nothing
    , _tiTableType = Nothing
    , _tiOwner = Nothing
    , _tiViewOriginalText = Nothing
    , _tiViewExpandedText = Nothing
    , _tiLastAnalyzedTime = Nothing
    , _tiStorageDescriptor = Nothing
    , _tiParameters = Nothing
    , _tiLastAccessTime = Nothing
    , _tiDescription = Nothing
    , _tiPartitionKeys = Nothing
    , _tiName = pName_
    }


-- | Retention time for this table.
tiRetention :: Lens' TableInput (Maybe Natural)
tiRetention = lens _tiRetention (\ s a -> s{_tiRetention = a}) . mapping _Nat

-- | The type of this table (@EXTERNAL_TABLE@ , @VIRTUAL_VIEW@ , etc.).
tiTableType :: Lens' TableInput (Maybe Text)
tiTableType = lens _tiTableType (\ s a -> s{_tiTableType = a})

-- | Owner of the table.
tiOwner :: Lens' TableInput (Maybe Text)
tiOwner = lens _tiOwner (\ s a -> s{_tiOwner = a})

-- | If the table is a view, the original text of the view; otherwise @null@ .
tiViewOriginalText :: Lens' TableInput (Maybe Text)
tiViewOriginalText = lens _tiViewOriginalText (\ s a -> s{_tiViewOriginalText = a})

-- | If the table is a view, the expanded text of the view; otherwise @null@ .
tiViewExpandedText :: Lens' TableInput (Maybe Text)
tiViewExpandedText = lens _tiViewExpandedText (\ s a -> s{_tiViewExpandedText = a})

-- | Last time column statistics were computed for this table.
tiLastAnalyzedTime :: Lens' TableInput (Maybe UTCTime)
tiLastAnalyzedTime = lens _tiLastAnalyzedTime (\ s a -> s{_tiLastAnalyzedTime = a}) . mapping _Time

-- | A storage descriptor containing information about the physical storage of this table.
tiStorageDescriptor :: Lens' TableInput (Maybe StorageDescriptor)
tiStorageDescriptor = lens _tiStorageDescriptor (\ s a -> s{_tiStorageDescriptor = a})

-- | Properties associated with this table, as a list of key-value pairs.
tiParameters :: Lens' TableInput (HashMap Text Text)
tiParameters = lens _tiParameters (\ s a -> s{_tiParameters = a}) . _Default . _Map

-- | Last time the table was accessed.
tiLastAccessTime :: Lens' TableInput (Maybe UTCTime)
tiLastAccessTime = lens _tiLastAccessTime (\ s a -> s{_tiLastAccessTime = a}) . mapping _Time

-- | Description of the table.
tiDescription :: Lens' TableInput (Maybe Text)
tiDescription = lens _tiDescription (\ s a -> s{_tiDescription = a})

-- | A list of columns by which the table is partitioned. Only primitive types are supported as partition keys.
tiPartitionKeys :: Lens' TableInput [Column]
tiPartitionKeys = lens _tiPartitionKeys (\ s a -> s{_tiPartitionKeys = a}) . _Default . _Coerce

-- | Name of the table. For Hive compatibility, this is folded to lowercase when it is stored.
tiName :: Lens' TableInput Text
tiName = lens _tiName (\ s a -> s{_tiName = a})

instance Hashable TableInput where

instance NFData TableInput where

instance ToJSON TableInput where
        toJSON TableInput'{..}
          = object
              (catMaybes
                 [("Retention" .=) <$> _tiRetention,
                  ("TableType" .=) <$> _tiTableType,
                  ("Owner" .=) <$> _tiOwner,
                  ("ViewOriginalText" .=) <$> _tiViewOriginalText,
                  ("ViewExpandedText" .=) <$> _tiViewExpandedText,
                  ("LastAnalyzedTime" .=) <$> _tiLastAnalyzedTime,
                  ("StorageDescriptor" .=) <$> _tiStorageDescriptor,
                  ("Parameters" .=) <$> _tiParameters,
                  ("LastAccessTime" .=) <$> _tiLastAccessTime,
                  ("Description" .=) <$> _tiDescription,
                  ("PartitionKeys" .=) <$> _tiPartitionKeys,
                  Just ("Name" .= _tiName)])

-- | Specifies a version of a table.
--
--
--
-- /See:/ 'tableVersion' smart constructor.
data TableVersion = TableVersion'
  { _tvVersionId :: !(Maybe Text)
  , _tvTable     :: !(Maybe Table)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TableVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tvVersionId' - The ID value that identifies this table version.
--
-- * 'tvTable' - The table in question
tableVersion
    :: TableVersion
tableVersion = TableVersion' {_tvVersionId = Nothing, _tvTable = Nothing}


-- | The ID value that identifies this table version.
tvVersionId :: Lens' TableVersion (Maybe Text)
tvVersionId = lens _tvVersionId (\ s a -> s{_tvVersionId = a})

-- | The table in question
tvTable :: Lens' TableVersion (Maybe Table)
tvTable = lens _tvTable (\ s a -> s{_tvTable = a})

instance FromJSON TableVersion where
        parseJSON
          = withObject "TableVersion"
              (\ x ->
                 TableVersion' <$>
                   (x .:? "VersionId") <*> (x .:? "Table"))

instance Hashable TableVersion where

instance NFData TableVersion where

-- | An error record for table-version operations.
--
--
--
-- /See:/ 'tableVersionError' smart constructor.
data TableVersionError = TableVersionError'
  { _tveVersionId   :: !(Maybe Text)
  , _tveTableName   :: !(Maybe Text)
  , _tveErrorDetail :: !(Maybe ErrorDetail)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TableVersionError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tveVersionId' - The ID value of the version in question.
--
-- * 'tveTableName' - The name of the table in question.
--
-- * 'tveErrorDetail' - Detail about the error.
tableVersionError
    :: TableVersionError
tableVersionError =
  TableVersionError'
    { _tveVersionId = Nothing
    , _tveTableName = Nothing
    , _tveErrorDetail = Nothing
    }


-- | The ID value of the version in question.
tveVersionId :: Lens' TableVersionError (Maybe Text)
tveVersionId = lens _tveVersionId (\ s a -> s{_tveVersionId = a})

-- | The name of the table in question.
tveTableName :: Lens' TableVersionError (Maybe Text)
tveTableName = lens _tveTableName (\ s a -> s{_tveTableName = a})

-- | Detail about the error.
tveErrorDetail :: Lens' TableVersionError (Maybe ErrorDetail)
tveErrorDetail = lens _tveErrorDetail (\ s a -> s{_tveErrorDetail = a})

instance FromJSON TableVersionError where
        parseJSON
          = withObject "TableVersionError"
              (\ x ->
                 TableVersionError' <$>
                   (x .:? "VersionId") <*> (x .:? "TableName") <*>
                     (x .:? "ErrorDetail"))

instance Hashable TableVersionError where

instance NFData TableVersionError where

-- | Information about a specific trigger.
--
--
--
-- /See:/ 'trigger' smart constructor.
data Trigger = Trigger'
  { _triState       :: !(Maybe TriggerState)
  , _triActions     :: !(Maybe [Action])
  , _triSchedule    :: !(Maybe Text)
  , _triPredicate   :: !(Maybe Predicate)
  , _triName        :: !(Maybe Text)
  , _triId          :: !(Maybe Text)
  , _triType        :: !(Maybe TriggerType)
  , _triDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Trigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'triState' - The current state of the trigger.
--
-- * 'triActions' - The actions initiated by this trigger.
--
-- * 'triSchedule' - A @cron@ expression used to specify the schedule (see <http://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- * 'triPredicate' - The predicate of this trigger, which defines when it will fire.
--
-- * 'triName' - Name of the trigger.
--
-- * 'triId' - Reserved for future use.
--
-- * 'triType' - The type of trigger that this is.
--
-- * 'triDescription' - A description of this trigger.
trigger
    :: Trigger
trigger =
  Trigger'
    { _triState = Nothing
    , _triActions = Nothing
    , _triSchedule = Nothing
    , _triPredicate = Nothing
    , _triName = Nothing
    , _triId = Nothing
    , _triType = Nothing
    , _triDescription = Nothing
    }


-- | The current state of the trigger.
triState :: Lens' Trigger (Maybe TriggerState)
triState = lens _triState (\ s a -> s{_triState = a})

-- | The actions initiated by this trigger.
triActions :: Lens' Trigger [Action]
triActions = lens _triActions (\ s a -> s{_triActions = a}) . _Default . _Coerce

-- | A @cron@ expression used to specify the schedule (see <http://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
triSchedule :: Lens' Trigger (Maybe Text)
triSchedule = lens _triSchedule (\ s a -> s{_triSchedule = a})

-- | The predicate of this trigger, which defines when it will fire.
triPredicate :: Lens' Trigger (Maybe Predicate)
triPredicate = lens _triPredicate (\ s a -> s{_triPredicate = a})

-- | Name of the trigger.
triName :: Lens' Trigger (Maybe Text)
triName = lens _triName (\ s a -> s{_triName = a})

-- | Reserved for future use.
triId :: Lens' Trigger (Maybe Text)
triId = lens _triId (\ s a -> s{_triId = a})

-- | The type of trigger that this is.
triType :: Lens' Trigger (Maybe TriggerType)
triType = lens _triType (\ s a -> s{_triType = a})

-- | A description of this trigger.
triDescription :: Lens' Trigger (Maybe Text)
triDescription = lens _triDescription (\ s a -> s{_triDescription = a})

instance FromJSON Trigger where
        parseJSON
          = withObject "Trigger"
              (\ x ->
                 Trigger' <$>
                   (x .:? "State") <*> (x .:? "Actions" .!= mempty) <*>
                     (x .:? "Schedule")
                     <*> (x .:? "Predicate")
                     <*> (x .:? "Name")
                     <*> (x .:? "Id")
                     <*> (x .:? "Type")
                     <*> (x .:? "Description"))

instance Hashable Trigger where

instance NFData Trigger where

-- | A structure used to provide information used to update a trigger. This object will update the the previous trigger definition by overwriting it completely.
--
--
--
-- /See:/ 'triggerUpdate' smart constructor.
data TriggerUpdate = TriggerUpdate'
  { _tuActions     :: !(Maybe [Action])
  , _tuSchedule    :: !(Maybe Text)
  , _tuPredicate   :: !(Maybe Predicate)
  , _tuName        :: !(Maybe Text)
  , _tuDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TriggerUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tuActions' - The actions initiated by this trigger.
--
-- * 'tuSchedule' - A @cron@ expression used to specify the schedule (see <http://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- * 'tuPredicate' - The predicate of this trigger, which defines when it will fire.
--
-- * 'tuName' - Reserved for future use.
--
-- * 'tuDescription' - A description of this trigger.
triggerUpdate
    :: TriggerUpdate
triggerUpdate =
  TriggerUpdate'
    { _tuActions = Nothing
    , _tuSchedule = Nothing
    , _tuPredicate = Nothing
    , _tuName = Nothing
    , _tuDescription = Nothing
    }


-- | The actions initiated by this trigger.
tuActions :: Lens' TriggerUpdate [Action]
tuActions = lens _tuActions (\ s a -> s{_tuActions = a}) . _Default . _Coerce

-- | A @cron@ expression used to specify the schedule (see <http://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
tuSchedule :: Lens' TriggerUpdate (Maybe Text)
tuSchedule = lens _tuSchedule (\ s a -> s{_tuSchedule = a})

-- | The predicate of this trigger, which defines when it will fire.
tuPredicate :: Lens' TriggerUpdate (Maybe Predicate)
tuPredicate = lens _tuPredicate (\ s a -> s{_tuPredicate = a})

-- | Reserved for future use.
tuName :: Lens' TriggerUpdate (Maybe Text)
tuName = lens _tuName (\ s a -> s{_tuName = a})

-- | A description of this trigger.
tuDescription :: Lens' TriggerUpdate (Maybe Text)
tuDescription = lens _tuDescription (\ s a -> s{_tuDescription = a})

instance Hashable TriggerUpdate where

instance NFData TriggerUpdate where

instance ToJSON TriggerUpdate where
        toJSON TriggerUpdate'{..}
          = object
              (catMaybes
                 [("Actions" .=) <$> _tuActions,
                  ("Schedule" .=) <$> _tuSchedule,
                  ("Predicate" .=) <$> _tuPredicate,
                  ("Name" .=) <$> _tuName,
                  ("Description" .=) <$> _tuDescription])

-- | Specifies a grok classifier to update when passed to @UpdateClassifier@ .
--
--
--
-- /See:/ 'updateGrokClassifierRequest' smart constructor.
data UpdateGrokClassifierRequest = UpdateGrokClassifierRequest'
  { _ugcrClassification :: !(Maybe Text)
  , _ugcrCustomPatterns :: !(Maybe Text)
  , _ugcrGrokPattern    :: !(Maybe Text)
  , _ugcrName           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGrokClassifierRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugcrClassification' - An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
--
-- * 'ugcrCustomPatterns' - Optional custom grok patterns used by this classifier.
--
-- * 'ugcrGrokPattern' - The grok pattern used by this classifier.
--
-- * 'ugcrName' - The name of the @GrokClassifier@ .
updateGrokClassifierRequest
    :: Text -- ^ 'ugcrName'
    -> UpdateGrokClassifierRequest
updateGrokClassifierRequest pName_ =
  UpdateGrokClassifierRequest'
    { _ugcrClassification = Nothing
    , _ugcrCustomPatterns = Nothing
    , _ugcrGrokPattern = Nothing
    , _ugcrName = pName_
    }


-- | An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
ugcrClassification :: Lens' UpdateGrokClassifierRequest (Maybe Text)
ugcrClassification = lens _ugcrClassification (\ s a -> s{_ugcrClassification = a})

-- | Optional custom grok patterns used by this classifier.
ugcrCustomPatterns :: Lens' UpdateGrokClassifierRequest (Maybe Text)
ugcrCustomPatterns = lens _ugcrCustomPatterns (\ s a -> s{_ugcrCustomPatterns = a})

-- | The grok pattern used by this classifier.
ugcrGrokPattern :: Lens' UpdateGrokClassifierRequest (Maybe Text)
ugcrGrokPattern = lens _ugcrGrokPattern (\ s a -> s{_ugcrGrokPattern = a})

-- | The name of the @GrokClassifier@ .
ugcrName :: Lens' UpdateGrokClassifierRequest Text
ugcrName = lens _ugcrName (\ s a -> s{_ugcrName = a})

instance Hashable UpdateGrokClassifierRequest where

instance NFData UpdateGrokClassifierRequest where

instance ToJSON UpdateGrokClassifierRequest where
        toJSON UpdateGrokClassifierRequest'{..}
          = object
              (catMaybes
                 [("Classification" .=) <$> _ugcrClassification,
                  ("CustomPatterns" .=) <$> _ugcrCustomPatterns,
                  ("GrokPattern" .=) <$> _ugcrGrokPattern,
                  Just ("Name" .= _ugcrName)])

-- | Specifies a JSON classifier to be updated.
--
--
--
-- /See:/ 'updateJSONClassifierRequest' smart constructor.
data UpdateJSONClassifierRequest = UpdateJSONClassifierRequest'
  { _ujcrJSONPath :: !(Maybe Text)
  , _ujcrName     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateJSONClassifierRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ujcrJSONPath' - A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
--
-- * 'ujcrName' - The name of the classifier.
updateJSONClassifierRequest
    :: Text -- ^ 'ujcrName'
    -> UpdateJSONClassifierRequest
updateJSONClassifierRequest pName_ =
  UpdateJSONClassifierRequest' {_ujcrJSONPath = Nothing, _ujcrName = pName_}


-- | A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
ujcrJSONPath :: Lens' UpdateJSONClassifierRequest (Maybe Text)
ujcrJSONPath = lens _ujcrJSONPath (\ s a -> s{_ujcrJSONPath = a})

-- | The name of the classifier.
ujcrName :: Lens' UpdateJSONClassifierRequest Text
ujcrName = lens _ujcrName (\ s a -> s{_ujcrName = a})

instance Hashable UpdateJSONClassifierRequest where

instance NFData UpdateJSONClassifierRequest where

instance ToJSON UpdateJSONClassifierRequest where
        toJSON UpdateJSONClassifierRequest'{..}
          = object
              (catMaybes
                 [("JsonPath" .=) <$> _ujcrJSONPath,
                  Just ("Name" .= _ujcrName)])

-- | Specifies an XML classifier to be updated.
--
--
--
-- /See:/ 'updateXMLClassifierRequest' smart constructor.
data UpdateXMLClassifierRequest = UpdateXMLClassifierRequest'
  { _uxcrClassification :: !(Maybe Text)
  , _uxcrRowTag         :: !(Maybe Text)
  , _uxcrName           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateXMLClassifierRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uxcrClassification' - An identifier of the data format that the classifier matches.
--
-- * 'uxcrRowTag' - The XML tag designating the element that contains each record in an XML document being parsed. Note that this cannot identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
--
-- * 'uxcrName' - The name of the classifier.
updateXMLClassifierRequest
    :: Text -- ^ 'uxcrName'
    -> UpdateXMLClassifierRequest
updateXMLClassifierRequest pName_ =
  UpdateXMLClassifierRequest'
    {_uxcrClassification = Nothing, _uxcrRowTag = Nothing, _uxcrName = pName_}


-- | An identifier of the data format that the classifier matches.
uxcrClassification :: Lens' UpdateXMLClassifierRequest (Maybe Text)
uxcrClassification = lens _uxcrClassification (\ s a -> s{_uxcrClassification = a})

-- | The XML tag designating the element that contains each record in an XML document being parsed. Note that this cannot identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
uxcrRowTag :: Lens' UpdateXMLClassifierRequest (Maybe Text)
uxcrRowTag = lens _uxcrRowTag (\ s a -> s{_uxcrRowTag = a})

-- | The name of the classifier.
uxcrName :: Lens' UpdateXMLClassifierRequest Text
uxcrName = lens _uxcrName (\ s a -> s{_uxcrName = a})

instance Hashable UpdateXMLClassifierRequest where

instance NFData UpdateXMLClassifierRequest where

instance ToJSON UpdateXMLClassifierRequest where
        toJSON UpdateXMLClassifierRequest'{..}
          = object
              (catMaybes
                 [("Classification" .=) <$> _uxcrClassification,
                  ("RowTag" .=) <$> _uxcrRowTag,
                  Just ("Name" .= _uxcrName)])

-- | Represents the equivalent of a Hive user-defined function (@UDF@ ) definition.
--
--
--
-- /See:/ 'userDefinedFunction' smart constructor.
data UserDefinedFunction = UserDefinedFunction'
  { _udfOwnerName    :: !(Maybe Text)
  , _udfResourceURIs :: !(Maybe [ResourceURI])
  , _udfFunctionName :: !(Maybe Text)
  , _udfOwnerType    :: !(Maybe PrincipalType)
  , _udfCreateTime   :: !(Maybe POSIX)
  , _udfClassName    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserDefinedFunction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udfOwnerName' - The owner of the function.
--
-- * 'udfResourceURIs' - The resource URIs for the function.
--
-- * 'udfFunctionName' - The name of the function.
--
-- * 'udfOwnerType' - The owner type.
--
-- * 'udfCreateTime' - The time at which the function was created.
--
-- * 'udfClassName' - The Java class that contains the function code.
userDefinedFunction
    :: UserDefinedFunction
userDefinedFunction =
  UserDefinedFunction'
    { _udfOwnerName = Nothing
    , _udfResourceURIs = Nothing
    , _udfFunctionName = Nothing
    , _udfOwnerType = Nothing
    , _udfCreateTime = Nothing
    , _udfClassName = Nothing
    }


-- | The owner of the function.
udfOwnerName :: Lens' UserDefinedFunction (Maybe Text)
udfOwnerName = lens _udfOwnerName (\ s a -> s{_udfOwnerName = a})

-- | The resource URIs for the function.
udfResourceURIs :: Lens' UserDefinedFunction [ResourceURI]
udfResourceURIs = lens _udfResourceURIs (\ s a -> s{_udfResourceURIs = a}) . _Default . _Coerce

-- | The name of the function.
udfFunctionName :: Lens' UserDefinedFunction (Maybe Text)
udfFunctionName = lens _udfFunctionName (\ s a -> s{_udfFunctionName = a})

-- | The owner type.
udfOwnerType :: Lens' UserDefinedFunction (Maybe PrincipalType)
udfOwnerType = lens _udfOwnerType (\ s a -> s{_udfOwnerType = a})

-- | The time at which the function was created.
udfCreateTime :: Lens' UserDefinedFunction (Maybe UTCTime)
udfCreateTime = lens _udfCreateTime (\ s a -> s{_udfCreateTime = a}) . mapping _Time

-- | The Java class that contains the function code.
udfClassName :: Lens' UserDefinedFunction (Maybe Text)
udfClassName = lens _udfClassName (\ s a -> s{_udfClassName = a})

instance FromJSON UserDefinedFunction where
        parseJSON
          = withObject "UserDefinedFunction"
              (\ x ->
                 UserDefinedFunction' <$>
                   (x .:? "OwnerName") <*>
                     (x .:? "ResourceUris" .!= mempty)
                     <*> (x .:? "FunctionName")
                     <*> (x .:? "OwnerType")
                     <*> (x .:? "CreateTime")
                     <*> (x .:? "ClassName"))

instance Hashable UserDefinedFunction where

instance NFData UserDefinedFunction where

-- | A structure used to create or updata a user-defined function.
--
--
--
-- /See:/ 'userDefinedFunctionInput' smart constructor.
data UserDefinedFunctionInput = UserDefinedFunctionInput'
  { _udfiOwnerName    :: !(Maybe Text)
  , _udfiResourceURIs :: !(Maybe [ResourceURI])
  , _udfiFunctionName :: !(Maybe Text)
  , _udfiOwnerType    :: !(Maybe PrincipalType)
  , _udfiClassName    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserDefinedFunctionInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udfiOwnerName' - The owner of the function.
--
-- * 'udfiResourceURIs' - The resource URIs for the function.
--
-- * 'udfiFunctionName' - The name of the function.
--
-- * 'udfiOwnerType' - The owner type.
--
-- * 'udfiClassName' - The Java class that contains the function code.
userDefinedFunctionInput
    :: UserDefinedFunctionInput
userDefinedFunctionInput =
  UserDefinedFunctionInput'
    { _udfiOwnerName = Nothing
    , _udfiResourceURIs = Nothing
    , _udfiFunctionName = Nothing
    , _udfiOwnerType = Nothing
    , _udfiClassName = Nothing
    }


-- | The owner of the function.
udfiOwnerName :: Lens' UserDefinedFunctionInput (Maybe Text)
udfiOwnerName = lens _udfiOwnerName (\ s a -> s{_udfiOwnerName = a})

-- | The resource URIs for the function.
udfiResourceURIs :: Lens' UserDefinedFunctionInput [ResourceURI]
udfiResourceURIs = lens _udfiResourceURIs (\ s a -> s{_udfiResourceURIs = a}) . _Default . _Coerce

-- | The name of the function.
udfiFunctionName :: Lens' UserDefinedFunctionInput (Maybe Text)
udfiFunctionName = lens _udfiFunctionName (\ s a -> s{_udfiFunctionName = a})

-- | The owner type.
udfiOwnerType :: Lens' UserDefinedFunctionInput (Maybe PrincipalType)
udfiOwnerType = lens _udfiOwnerType (\ s a -> s{_udfiOwnerType = a})

-- | The Java class that contains the function code.
udfiClassName :: Lens' UserDefinedFunctionInput (Maybe Text)
udfiClassName = lens _udfiClassName (\ s a -> s{_udfiClassName = a})

instance Hashable UserDefinedFunctionInput where

instance NFData UserDefinedFunctionInput where

instance ToJSON UserDefinedFunctionInput where
        toJSON UserDefinedFunctionInput'{..}
          = object
              (catMaybes
                 [("OwnerName" .=) <$> _udfiOwnerName,
                  ("ResourceUris" .=) <$> _udfiResourceURIs,
                  ("FunctionName" .=) <$> _udfiFunctionName,
                  ("OwnerType" .=) <$> _udfiOwnerType,
                  ("ClassName" .=) <$> _udfiClassName])

-- | A classifier for @XML@ content.
--
--
--
-- /See:/ 'xmlClassifier' smart constructor.
data XMLClassifier = XMLClassifier'
  { _xcCreationTime   :: !(Maybe POSIX)
  , _xcLastUpdated    :: !(Maybe POSIX)
  , _xcVersion        :: !(Maybe Integer)
  , _xcRowTag         :: !(Maybe Text)
  , _xcName           :: !Text
  , _xcClassification :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'XMLClassifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'xcCreationTime' - The time this classifier was registered.
--
-- * 'xcLastUpdated' - The time this classifier was last updated.
--
-- * 'xcVersion' - The version of this classifier.
--
-- * 'xcRowTag' - The XML tag designating the element that contains each record in an XML document being parsed. Note that this cannot identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
--
-- * 'xcName' - The name of the classifier.
--
-- * 'xcClassification' - An identifier of the data format that the classifier matches.
xmlClassifier
    :: Text -- ^ 'xcName'
    -> Text -- ^ 'xcClassification'
    -> XMLClassifier
xmlClassifier pName_ pClassification_ =
  XMLClassifier'
    { _xcCreationTime = Nothing
    , _xcLastUpdated = Nothing
    , _xcVersion = Nothing
    , _xcRowTag = Nothing
    , _xcName = pName_
    , _xcClassification = pClassification_
    }


-- | The time this classifier was registered.
xcCreationTime :: Lens' XMLClassifier (Maybe UTCTime)
xcCreationTime = lens _xcCreationTime (\ s a -> s{_xcCreationTime = a}) . mapping _Time

-- | The time this classifier was last updated.
xcLastUpdated :: Lens' XMLClassifier (Maybe UTCTime)
xcLastUpdated = lens _xcLastUpdated (\ s a -> s{_xcLastUpdated = a}) . mapping _Time

-- | The version of this classifier.
xcVersion :: Lens' XMLClassifier (Maybe Integer)
xcVersion = lens _xcVersion (\ s a -> s{_xcVersion = a})

-- | The XML tag designating the element that contains each record in an XML document being parsed. Note that this cannot identify a self-closing element (closed by @/>@ ). An empty row element that contains only attributes can be parsed as long as it ends with a closing tag (for example, @<row item_a="A" item_b="B"></row>@ is okay, but @<row item_a="A" item_b="B" />@ is not).
xcRowTag :: Lens' XMLClassifier (Maybe Text)
xcRowTag = lens _xcRowTag (\ s a -> s{_xcRowTag = a})

-- | The name of the classifier.
xcName :: Lens' XMLClassifier Text
xcName = lens _xcName (\ s a -> s{_xcName = a})

-- | An identifier of the data format that the classifier matches.
xcClassification :: Lens' XMLClassifier Text
xcClassification = lens _xcClassification (\ s a -> s{_xcClassification = a})

instance FromJSON XMLClassifier where
        parseJSON
          = withObject "XMLClassifier"
              (\ x ->
                 XMLClassifier' <$>
                   (x .:? "CreationTime") <*> (x .:? "LastUpdated") <*>
                     (x .:? "Version")
                     <*> (x .:? "RowTag")
                     <*> (x .: "Name")
                     <*> (x .: "Classification"))

instance Hashable XMLClassifier where

instance NFData XMLClassifier where

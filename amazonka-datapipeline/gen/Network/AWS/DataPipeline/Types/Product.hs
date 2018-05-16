{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DataPipeline.Types.Product where

import Network.AWS.DataPipeline.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A key-value pair that describes a property of a pipeline object. The value is specified as either a string value (@StringValue@ ) or a reference to another object (@RefValue@ ) but not as both.
--
--
--
-- /See:/ 'field' smart constructor.
data Field = Field'
  { _fRefValue    :: !(Maybe Text)
  , _fStringValue :: !(Maybe Text)
  , _fKey         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Field' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fRefValue' - The field value, expressed as the identifier of another object.
--
-- * 'fStringValue' - The field value, expressed as a String.
--
-- * 'fKey' - The field identifier.
field
    :: Text -- ^ 'fKey'
    -> Field
field pKey_ =
  Field' {_fRefValue = Nothing, _fStringValue = Nothing, _fKey = pKey_}


-- | The field value, expressed as the identifier of another object.
fRefValue :: Lens' Field (Maybe Text)
fRefValue = lens _fRefValue (\ s a -> s{_fRefValue = a})

-- | The field value, expressed as a String.
fStringValue :: Lens' Field (Maybe Text)
fStringValue = lens _fStringValue (\ s a -> s{_fStringValue = a})

-- | The field identifier.
fKey :: Lens' Field Text
fKey = lens _fKey (\ s a -> s{_fKey = a})

instance FromJSON Field where
        parseJSON
          = withObject "Field"
              (\ x ->
                 Field' <$>
                   (x .:? "refValue") <*> (x .:? "stringValue") <*>
                     (x .: "key"))

instance Hashable Field where

instance NFData Field where

instance ToJSON Field where
        toJSON Field'{..}
          = object
              (catMaybes
                 [("refValue" .=) <$> _fRefValue,
                  ("stringValue" .=) <$> _fStringValue,
                  Just ("key" .= _fKey)])

-- | Identity information for the EC2 instance that is hosting the task runner. You can get this value by calling a metadata URI from the EC2 instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AESDG-chapter-instancedata.html Instance Metadata> in the /Amazon Elastic Compute Cloud User Guide./ Passing in this value proves that your task runner is running on an EC2 instance, and ensures the proper AWS Data Pipeline service charges are applied to your pipeline.
--
--
--
--
--
-- /See:/ 'instanceIdentity' smart constructor.
data InstanceIdentity = InstanceIdentity'
  { _iiSignature :: !(Maybe Text)
  , _iiDocument  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiSignature' - A signature which can be used to verify the accuracy and authenticity of the information provided in the instance identity document.
--
-- * 'iiDocument' - A description of an EC2 instance that is generated when the instance is launched and exposed to the instance via the instance metadata service in the form of a JSON representation of an object.
instanceIdentity
    :: InstanceIdentity
instanceIdentity =
  InstanceIdentity' {_iiSignature = Nothing, _iiDocument = Nothing}


-- | A signature which can be used to verify the accuracy and authenticity of the information provided in the instance identity document.
iiSignature :: Lens' InstanceIdentity (Maybe Text)
iiSignature = lens _iiSignature (\ s a -> s{_iiSignature = a})

-- | A description of an EC2 instance that is generated when the instance is launched and exposed to the instance via the instance metadata service in the form of a JSON representation of an object.
iiDocument :: Lens' InstanceIdentity (Maybe Text)
iiDocument = lens _iiDocument (\ s a -> s{_iiDocument = a})

instance Hashable InstanceIdentity where

instance NFData InstanceIdentity where

instance ToJSON InstanceIdentity where
        toJSON InstanceIdentity'{..}
          = object
              (catMaybes
                 [("signature" .=) <$> _iiSignature,
                  ("document" .=) <$> _iiDocument])

-- | Contains a logical operation for comparing the value of a field with a specified value.
--
--
--
-- /See:/ 'operator' smart constructor.
data Operator = Operator'
  { _oValues :: !(Maybe [Text])
  , _oType   :: !(Maybe OperatorType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Operator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oValues' - The value that the actual field value will be compared with.
--
-- * 'oType' - The logical operation to be performed: equal (@EQ@ ), equal reference (@REF_EQ@ ), less than or equal (@LE@ ), greater than or equal (@GE@ ), or between (@BETWEEN@ ). Equal reference (@REF_EQ@ ) can be used only with reference fields. The other comparison types can be used only with String fields. The comparison types you can use apply only to certain object fields, as detailed below.  The comparison operators EQ and REF_EQ act on the following fields:      * name    * @sphere    * parent    * @componentParent    * @instanceParent    * @status    * @scheduledStartTime    * @scheduledEndTime    * @actualStartTime    * @actualEndTime The comparison operators @GE@ , @LE@ , and @BETWEEN@ act on the following fields:      * @scheduledStartTime    * @scheduledEndTime    * @actualStartTime    * @actualEndTime Note that fields beginning with the at sign (@) are read-only and set by the web service. When you name fields, you should choose names containing only alpha-numeric values, as symbols may be reserved by AWS Data Pipeline. User-defined fields that you add to a pipeline should prefix their name with the string "my".
operator
    :: Operator
operator = Operator' {_oValues = Nothing, _oType = Nothing}


-- | The value that the actual field value will be compared with.
oValues :: Lens' Operator [Text]
oValues = lens _oValues (\ s a -> s{_oValues = a}) . _Default . _Coerce

-- | The logical operation to be performed: equal (@EQ@ ), equal reference (@REF_EQ@ ), less than or equal (@LE@ ), greater than or equal (@GE@ ), or between (@BETWEEN@ ). Equal reference (@REF_EQ@ ) can be used only with reference fields. The other comparison types can be used only with String fields. The comparison types you can use apply only to certain object fields, as detailed below.  The comparison operators EQ and REF_EQ act on the following fields:      * name    * @sphere    * parent    * @componentParent    * @instanceParent    * @status    * @scheduledStartTime    * @scheduledEndTime    * @actualStartTime    * @actualEndTime The comparison operators @GE@ , @LE@ , and @BETWEEN@ act on the following fields:      * @scheduledStartTime    * @scheduledEndTime    * @actualStartTime    * @actualEndTime Note that fields beginning with the at sign (@) are read-only and set by the web service. When you name fields, you should choose names containing only alpha-numeric values, as symbols may be reserved by AWS Data Pipeline. User-defined fields that you add to a pipeline should prefix their name with the string "my".
oType :: Lens' Operator (Maybe OperatorType)
oType = lens _oType (\ s a -> s{_oType = a})

instance Hashable Operator where

instance NFData Operator where

instance ToJSON Operator where
        toJSON Operator'{..}
          = object
              (catMaybes
                 [("values" .=) <$> _oValues, ("type" .=) <$> _oType])

-- | The attributes allowed or specified with a parameter object.
--
--
--
-- /See:/ 'parameterAttribute' smart constructor.
data ParameterAttribute = ParameterAttribute'
  { _paKey         :: !Text
  , _paStringValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParameterAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paKey' - The field identifier.
--
-- * 'paStringValue' - The field value, expressed as a String.
parameterAttribute
    :: Text -- ^ 'paKey'
    -> Text -- ^ 'paStringValue'
    -> ParameterAttribute
parameterAttribute pKey_ pStringValue_ =
  ParameterAttribute' {_paKey = pKey_, _paStringValue = pStringValue_}


-- | The field identifier.
paKey :: Lens' ParameterAttribute Text
paKey = lens _paKey (\ s a -> s{_paKey = a})

-- | The field value, expressed as a String.
paStringValue :: Lens' ParameterAttribute Text
paStringValue = lens _paStringValue (\ s a -> s{_paStringValue = a})

instance FromJSON ParameterAttribute where
        parseJSON
          = withObject "ParameterAttribute"
              (\ x ->
                 ParameterAttribute' <$>
                   (x .: "key") <*> (x .: "stringValue"))

instance Hashable ParameterAttribute where

instance NFData ParameterAttribute where

instance ToJSON ParameterAttribute where
        toJSON ParameterAttribute'{..}
          = object
              (catMaybes
                 [Just ("key" .= _paKey),
                  Just ("stringValue" .= _paStringValue)])

-- | Contains information about a parameter object.
--
--
--
-- /See:/ 'parameterObject' smart constructor.
data ParameterObject = ParameterObject'
  { _poId         :: !Text
  , _poAttributes :: ![ParameterAttribute]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParameterObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'poId' - The ID of the parameter object.
--
-- * 'poAttributes' - The attributes of the parameter object.
parameterObject
    :: Text -- ^ 'poId'
    -> ParameterObject
parameterObject pId_ = ParameterObject' {_poId = pId_, _poAttributes = mempty}


-- | The ID of the parameter object.
poId :: Lens' ParameterObject Text
poId = lens _poId (\ s a -> s{_poId = a})

-- | The attributes of the parameter object.
poAttributes :: Lens' ParameterObject [ParameterAttribute]
poAttributes = lens _poAttributes (\ s a -> s{_poAttributes = a}) . _Coerce

instance FromJSON ParameterObject where
        parseJSON
          = withObject "ParameterObject"
              (\ x ->
                 ParameterObject' <$>
                   (x .: "id") <*> (x .:? "attributes" .!= mempty))

instance Hashable ParameterObject where

instance NFData ParameterObject where

instance ToJSON ParameterObject where
        toJSON ParameterObject'{..}
          = object
              (catMaybes
                 [Just ("id" .= _poId),
                  Just ("attributes" .= _poAttributes)])

-- | A value or list of parameter values.
--
--
--
-- /See:/ 'parameterValue' smart constructor.
data ParameterValue = ParameterValue'
  { _pvId          :: !Text
  , _pvStringValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParameterValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvId' - The ID of the parameter value.
--
-- * 'pvStringValue' - The field value, expressed as a String.
parameterValue
    :: Text -- ^ 'pvId'
    -> Text -- ^ 'pvStringValue'
    -> ParameterValue
parameterValue pId_ pStringValue_ =
  ParameterValue' {_pvId = pId_, _pvStringValue = pStringValue_}


-- | The ID of the parameter value.
pvId :: Lens' ParameterValue Text
pvId = lens _pvId (\ s a -> s{_pvId = a})

-- | The field value, expressed as a String.
pvStringValue :: Lens' ParameterValue Text
pvStringValue = lens _pvStringValue (\ s a -> s{_pvStringValue = a})

instance FromJSON ParameterValue where
        parseJSON
          = withObject "ParameterValue"
              (\ x ->
                 ParameterValue' <$>
                   (x .: "id") <*> (x .: "stringValue"))

instance Hashable ParameterValue where

instance NFData ParameterValue where

instance ToJSON ParameterValue where
        toJSON ParameterValue'{..}
          = object
              (catMaybes
                 [Just ("id" .= _pvId),
                  Just ("stringValue" .= _pvStringValue)])

-- | Contains pipeline metadata.
--
--
--
-- /See:/ 'pipelineDescription' smart constructor.
data PipelineDescription = PipelineDescription'
  { _pdDescription :: !(Maybe Text)
  , _pdTags        :: !(Maybe [Tag])
  , _pdPipelineId  :: !Text
  , _pdName        :: !Text
  , _pdFields      :: ![Field]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PipelineDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdDescription' - Description of the pipeline.
--
-- * 'pdTags' - A list of tags to associated with a pipeline. Tags let you control access to pipelines. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines> in the /AWS Data Pipeline Developer Guide/ .
--
-- * 'pdPipelineId' - The pipeline identifier that was assigned by AWS Data Pipeline. This is a string of the form @df-297EG78HU43EEXAMPLE@ .
--
-- * 'pdName' - The name of the pipeline.
--
-- * 'pdFields' - A list of read-only fields that contain metadata about the pipeline: @userId, @accountId, and @pipelineState.
pipelineDescription
    :: Text -- ^ 'pdPipelineId'
    -> Text -- ^ 'pdName'
    -> PipelineDescription
pipelineDescription pPipelineId_ pName_ =
  PipelineDescription'
    { _pdDescription = Nothing
    , _pdTags = Nothing
    , _pdPipelineId = pPipelineId_
    , _pdName = pName_
    , _pdFields = mempty
    }


-- | Description of the pipeline.
pdDescription :: Lens' PipelineDescription (Maybe Text)
pdDescription = lens _pdDescription (\ s a -> s{_pdDescription = a})

-- | A list of tags to associated with a pipeline. Tags let you control access to pipelines. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines> in the /AWS Data Pipeline Developer Guide/ .
pdTags :: Lens' PipelineDescription [Tag]
pdTags = lens _pdTags (\ s a -> s{_pdTags = a}) . _Default . _Coerce

-- | The pipeline identifier that was assigned by AWS Data Pipeline. This is a string of the form @df-297EG78HU43EEXAMPLE@ .
pdPipelineId :: Lens' PipelineDescription Text
pdPipelineId = lens _pdPipelineId (\ s a -> s{_pdPipelineId = a})

-- | The name of the pipeline.
pdName :: Lens' PipelineDescription Text
pdName = lens _pdName (\ s a -> s{_pdName = a})

-- | A list of read-only fields that contain metadata about the pipeline: @userId, @accountId, and @pipelineState.
pdFields :: Lens' PipelineDescription [Field]
pdFields = lens _pdFields (\ s a -> s{_pdFields = a}) . _Coerce

instance FromJSON PipelineDescription where
        parseJSON
          = withObject "PipelineDescription"
              (\ x ->
                 PipelineDescription' <$>
                   (x .:? "description") <*> (x .:? "tags" .!= mempty)
                     <*> (x .: "pipelineId")
                     <*> (x .: "name")
                     <*> (x .:? "fields" .!= mempty))

instance Hashable PipelineDescription where

instance NFData PipelineDescription where

-- | Contains the name and identifier of a pipeline.
--
--
--
-- /See:/ 'pipelineIdName' smart constructor.
data PipelineIdName = PipelineIdName'
  { _pinName :: !(Maybe Text)
  , _pinId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PipelineIdName' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pinName' - The name of the pipeline.
--
-- * 'pinId' - The ID of the pipeline that was assigned by AWS Data Pipeline. This is a string of the form @df-297EG78HU43EEXAMPLE@ .
pipelineIdName
    :: PipelineIdName
pipelineIdName = PipelineIdName' {_pinName = Nothing, _pinId = Nothing}


-- | The name of the pipeline.
pinName :: Lens' PipelineIdName (Maybe Text)
pinName = lens _pinName (\ s a -> s{_pinName = a})

-- | The ID of the pipeline that was assigned by AWS Data Pipeline. This is a string of the form @df-297EG78HU43EEXAMPLE@ .
pinId :: Lens' PipelineIdName (Maybe Text)
pinId = lens _pinId (\ s a -> s{_pinId = a})

instance FromJSON PipelineIdName where
        parseJSON
          = withObject "PipelineIdName"
              (\ x ->
                 PipelineIdName' <$> (x .:? "name") <*> (x .:? "id"))

instance Hashable PipelineIdName where

instance NFData PipelineIdName where

-- | Contains information about a pipeline object. This can be a logical, physical, or physical attempt pipeline object. The complete set of components of a pipeline defines the pipeline.
--
--
--
-- /See:/ 'pipelineObject' smart constructor.
data PipelineObject = PipelineObject'
  { _pId     :: !Text
  , _pName   :: !Text
  , _pFields :: ![Field]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PipelineObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pId' - The ID of the object.
--
-- * 'pName' - The name of the object.
--
-- * 'pFields' - Key-value pairs that define the properties of the object.
pipelineObject
    :: Text -- ^ 'pId'
    -> Text -- ^ 'pName'
    -> PipelineObject
pipelineObject pId_ pName_ =
  PipelineObject' {_pId = pId_, _pName = pName_, _pFields = mempty}


-- | The ID of the object.
pId :: Lens' PipelineObject Text
pId = lens _pId (\ s a -> s{_pId = a})

-- | The name of the object.
pName :: Lens' PipelineObject Text
pName = lens _pName (\ s a -> s{_pName = a})

-- | Key-value pairs that define the properties of the object.
pFields :: Lens' PipelineObject [Field]
pFields = lens _pFields (\ s a -> s{_pFields = a}) . _Coerce

instance FromJSON PipelineObject where
        parseJSON
          = withObject "PipelineObject"
              (\ x ->
                 PipelineObject' <$>
                   (x .: "id") <*> (x .: "name") <*>
                     (x .:? "fields" .!= mempty))

instance Hashable PipelineObject where

instance NFData PipelineObject where

instance ToJSON PipelineObject where
        toJSON PipelineObject'{..}
          = object
              (catMaybes
                 [Just ("id" .= _pId), Just ("name" .= _pName),
                  Just ("fields" .= _pFields)])

-- | Defines the query to run against an object.
--
--
--
-- /See:/ 'query' smart constructor.
newtype Query = Query'
  { _qSelectors :: Maybe [Selector]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Query' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qSelectors' - List of selectors that define the query. An object must satisfy all of the selectors to match the query.
query
    :: Query
query = Query' {_qSelectors = Nothing}


-- | List of selectors that define the query. An object must satisfy all of the selectors to match the query.
qSelectors :: Lens' Query [Selector]
qSelectors = lens _qSelectors (\ s a -> s{_qSelectors = a}) . _Default . _Coerce

instance Hashable Query where

instance NFData Query where

instance ToJSON Query where
        toJSON Query'{..}
          = object
              (catMaybes [("selectors" .=) <$> _qSelectors])

-- | A comparision that is used to determine whether a query should return this object.
--
--
--
-- /See:/ 'selector' smart constructor.
data Selector = Selector'
  { _sOperator  :: !(Maybe Operator)
  , _sFieldName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Selector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sOperator' - Undocumented member.
--
-- * 'sFieldName' - The name of the field that the operator will be applied to. The field name is the "key" portion of the field definition in the pipeline definition syntax that is used by the AWS Data Pipeline API. If the field is not set on the object, the condition fails.
selector
    :: Selector
selector = Selector' {_sOperator = Nothing, _sFieldName = Nothing}


-- | Undocumented member.
sOperator :: Lens' Selector (Maybe Operator)
sOperator = lens _sOperator (\ s a -> s{_sOperator = a})

-- | The name of the field that the operator will be applied to. The field name is the "key" portion of the field definition in the pipeline definition syntax that is used by the AWS Data Pipeline API. If the field is not set on the object, the condition fails.
sFieldName :: Lens' Selector (Maybe Text)
sFieldName = lens _sFieldName (\ s a -> s{_sFieldName = a})

instance Hashable Selector where

instance NFData Selector where

instance ToJSON Selector where
        toJSON Selector'{..}
          = object
              (catMaybes
                 [("operator" .=) <$> _sOperator,
                  ("fieldName" .=) <$> _sFieldName])

-- | Tags are key/value pairs defined by a user and associated with a pipeline to control access. AWS Data Pipeline allows you to associate ten tags per pipeline. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines> in the /AWS Data Pipeline Developer Guide/ .
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagKey   :: !Text
  , _tagValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - The key name of a tag defined by a user. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines> in the /AWS Data Pipeline Developer Guide/ .
--
-- * 'tagValue' - The optional value portion of a tag defined by a user. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines> in the /AWS Data Pipeline Developer Guide/ .
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}


-- | The key name of a tag defined by a user. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines> in the /AWS Data Pipeline Developer Guide/ .
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

-- | The optional value portion of a tag defined by a user. For more information, see <http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html Controlling User Access to Pipelines> in the /AWS Data Pipeline Developer Guide/ .
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .: "key") <*> (x .: "value"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [Just ("key" .= _tagKey),
                  Just ("value" .= _tagValue)])

-- | Contains information about a pipeline task that is assigned to a task runner.
--
--
--
-- /See:/ 'taskObject' smart constructor.
data TaskObject = TaskObject'
  { _toPipelineId :: !(Maybe Text)
  , _toAttemptId  :: !(Maybe Text)
  , _toTaskId     :: !(Maybe Text)
  , _toObjects    :: !(Maybe (Map Text PipelineObject))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TaskObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'toPipelineId' - The ID of the pipeline that provided the task.
--
-- * 'toAttemptId' - The ID of the pipeline task attempt object. AWS Data Pipeline uses this value to track how many times a task is attempted.
--
-- * 'toTaskId' - An internal identifier for the task. This ID is passed to the 'SetTaskStatus' and 'ReportTaskProgress' actions.
--
-- * 'toObjects' - Connection information for the location where the task runner will publish the output of the task.
taskObject
    :: TaskObject
taskObject =
  TaskObject'
    { _toPipelineId = Nothing
    , _toAttemptId = Nothing
    , _toTaskId = Nothing
    , _toObjects = Nothing
    }


-- | The ID of the pipeline that provided the task.
toPipelineId :: Lens' TaskObject (Maybe Text)
toPipelineId = lens _toPipelineId (\ s a -> s{_toPipelineId = a})

-- | The ID of the pipeline task attempt object. AWS Data Pipeline uses this value to track how many times a task is attempted.
toAttemptId :: Lens' TaskObject (Maybe Text)
toAttemptId = lens _toAttemptId (\ s a -> s{_toAttemptId = a})

-- | An internal identifier for the task. This ID is passed to the 'SetTaskStatus' and 'ReportTaskProgress' actions.
toTaskId :: Lens' TaskObject (Maybe Text)
toTaskId = lens _toTaskId (\ s a -> s{_toTaskId = a})

-- | Connection information for the location where the task runner will publish the output of the task.
toObjects :: Lens' TaskObject (HashMap Text PipelineObject)
toObjects = lens _toObjects (\ s a -> s{_toObjects = a}) . _Default . _Map

instance FromJSON TaskObject where
        parseJSON
          = withObject "TaskObject"
              (\ x ->
                 TaskObject' <$>
                   (x .:? "pipelineId") <*> (x .:? "attemptId") <*>
                     (x .:? "taskId")
                     <*> (x .:? "objects" .!= mempty))

instance Hashable TaskObject where

instance NFData TaskObject where

-- | Defines a validation error. Validation errors prevent pipeline activation. The set of validation errors that can be returned are defined by AWS Data Pipeline.
--
--
--
-- /See:/ 'validationError' smart constructor.
data ValidationError = ValidationError'
  { _veId     :: !(Maybe Text)
  , _veErrors :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ValidationError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'veId' - The identifier of the object that contains the validation error.
--
-- * 'veErrors' - A description of the validation error.
validationError
    :: ValidationError
validationError = ValidationError' {_veId = Nothing, _veErrors = Nothing}


-- | The identifier of the object that contains the validation error.
veId :: Lens' ValidationError (Maybe Text)
veId = lens _veId (\ s a -> s{_veId = a})

-- | A description of the validation error.
veErrors :: Lens' ValidationError [Text]
veErrors = lens _veErrors (\ s a -> s{_veErrors = a}) . _Default . _Coerce

instance FromJSON ValidationError where
        parseJSON
          = withObject "ValidationError"
              (\ x ->
                 ValidationError' <$>
                   (x .:? "id") <*> (x .:? "errors" .!= mempty))

instance Hashable ValidationError where

instance NFData ValidationError where

-- | Defines a validation warning. Validation warnings do not prevent pipeline activation. The set of validation warnings that can be returned are defined by AWS Data Pipeline.
--
--
--
-- /See:/ 'validationWarning' smart constructor.
data ValidationWarning = ValidationWarning'
  { _vwWarnings :: !(Maybe [Text])
  , _vwId       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ValidationWarning' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vwWarnings' - A description of the validation warning.
--
-- * 'vwId' - The identifier of the object that contains the validation warning.
validationWarning
    :: ValidationWarning
validationWarning = ValidationWarning' {_vwWarnings = Nothing, _vwId = Nothing}


-- | A description of the validation warning.
vwWarnings :: Lens' ValidationWarning [Text]
vwWarnings = lens _vwWarnings (\ s a -> s{_vwWarnings = a}) . _Default . _Coerce

-- | The identifier of the object that contains the validation warning.
vwId :: Lens' ValidationWarning (Maybe Text)
vwId = lens _vwId (\ s a -> s{_vwId = a})

instance FromJSON ValidationWarning where
        parseJSON
          = withObject "ValidationWarning"
              (\ x ->
                 ValidationWarning' <$>
                   (x .:? "warnings" .!= mempty) <*> (x .:? "id"))

instance Hashable ValidationWarning where

instance NFData ValidationWarning where

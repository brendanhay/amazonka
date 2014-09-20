{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS Data Pipeline is a web service that you can use to automate the
-- movement and transformation of data. With AWS Data Pipeline, you can define
-- data-driven workflows, so that tasks can be dependent on the successful
-- completion of previous tasks.
module Network.AWS.DataPipeline.Types
    (
    -- * Service
      DataPipeline
    -- ** Errors
    , DataPipelineError (..)
    , _DataPipelineClient
    , _DataPipelineSerializer
    , _DataPipelineService
    , _InternalServiceError
    , _InvalidRequestException
    , _PipelineDeletedException
    , _PipelineNotFoundException
    , _TaskNotFoundException

    -- * OperatorType
    , OperatorType (..)

    -- * TaskStatus
    , TaskStatus (..)

    -- * Query
    , Query
    , query
    , qSelectors

    -- * Field
    , Field
    , field
    , fKey
    , fStringValue
    , fRefValue

    -- * InstanceIdentity
    , InstanceIdentity
    , instanceIdentity
    , iiDocument
    , iiSignature

    -- * Operator
    , Operator
    , operator
    , oType
    , oValues

    -- * PipelineDescription
    , PipelineDescription
    , pipelineDescription
    , pdPipelineId
    , pdName
    , pdFields
    , pdDescription

    -- * PipelineIdName
    , PipelineIdName
    , pipelineIdName
    , pinId
    , pinName

    -- * PipelineObject
    , PipelineObject
    , pipelineObject
    , poId
    , poName
    , poFields

    -- * Selector
    , Selector
    , selector
    , sFieldName
    , sOperator

    -- * TaskObject
    , TaskObject
    , taskObject
    , toTaskId
    , toPipelineId
    , toAttemptId
    , toObjects

    -- * ValidationError
    , ValidationError
    , validationError
    , veId
    , veErrors

    -- * ValidationWarning
    , ValidationWarning
    , validationWarning
    , vwId
    , vwWarnings
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2012-10-29@) of the
-- @AWS Data Pipeline@ service.
data DataPipeline deriving (Typeable)

instance AWSService DataPipeline where
    type Sg DataPipeline = V4
    type Er DataPipeline = DataPipelineError

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "datapipeline"
        , _svcVersion  = "2012-10-29"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'DataPipeline' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data DataPipelineError
    = DataPipelineClient HttpException
    | DataPipelineSerializer String
    | DataPipelineService String
      -- | An internal service error occurred.
    | InternalServiceError
        { _iseMessage :: Maybe Text
        }
      -- | The request was not valid. Verify that your request was properly
      -- formatted, that the signature was generated with the correct
      -- credentials, and that you haven't exceeded any of the service
      -- limits for your account.
    | InvalidRequestException
        { _ireMessage :: Maybe Text
        }
      -- | The specified pipeline has been deleted.
    | PipelineDeletedException
        { _pdeMessage :: Maybe Text
        }
      -- | The specified pipeline was not found. Verify that you used the
      -- correct user and account identifiers.
    | PipelineNotFoundException
        { _pnfeMessage :: Maybe Text
        }
      -- | The specified task was not found.
    | TaskNotFoundException
        { _tnfeMessage :: Maybe Text
        }
      deriving (Show, Typeable, Generic)

instance AWSError DataPipelineError where
    awsError = const "DataPipelineError"

instance AWSServiceError DataPipelineError where
    serviceError    = DataPipelineService
    clientError     = DataPipelineClient
    serializerError = DataPipelineSerializer

instance Exception DataPipelineError

-- | See: 'DataPipelineClient'
_DataPipelineClient :: Prism' DataPipelineError HttpException
_DataPipelineClient = prism
    DataPipelineClient
    (\case
        DataPipelineClient p1 -> Right p1
        x -> Left x)

-- | See: 'DataPipelineSerializer'
_DataPipelineSerializer :: Prism' DataPipelineError String
_DataPipelineSerializer = prism
    DataPipelineSerializer
    (\case
        DataPipelineSerializer p1 -> Right p1
        x -> Left x)

-- | See: 'DataPipelineService'
_DataPipelineService :: Prism' DataPipelineError String
_DataPipelineService = prism
    DataPipelineService
    (\case
        DataPipelineService p1 -> Right p1
        x -> Left x)

-- | An internal service error occurred.
--
-- See: 'InternalServiceError'
_InternalServiceError :: Prism' DataPipelineError (Maybe Text)
_InternalServiceError = prism
    InternalServiceError
    (\case
        InternalServiceError p1 -> Right p1
        x -> Left x)

-- | The request was not valid. Verify that your request was properly formatted,
-- that the signature was generated with the correct credentials, and that you
-- haven't exceeded any of the service limits for your account.
--
-- See: 'InvalidRequestException'
_InvalidRequestException :: Prism' DataPipelineError (Maybe Text)
_InvalidRequestException = prism
    InvalidRequestException
    (\case
        InvalidRequestException p1 -> Right p1
        x -> Left x)

-- | The specified pipeline has been deleted.
--
-- See: 'PipelineDeletedException'
_PipelineDeletedException :: Prism' DataPipelineError (Maybe Text)
_PipelineDeletedException = prism
    PipelineDeletedException
    (\case
        PipelineDeletedException p1 -> Right p1
        x -> Left x)

-- | The specified pipeline was not found. Verify that you used the correct user
-- and account identifiers.
--
-- See: 'PipelineNotFoundException'
_PipelineNotFoundException :: Prism' DataPipelineError (Maybe Text)
_PipelineNotFoundException = prism
    PipelineNotFoundException
    (\case
        PipelineNotFoundException p1 -> Right p1
        x -> Left x)

-- | The specified task was not found.
--
-- See: 'TaskNotFoundException'
_TaskNotFoundException :: Prism' DataPipelineError (Maybe Text)
_TaskNotFoundException = prism
    TaskNotFoundException
    (\case
        TaskNotFoundException p1 -> Right p1
        x -> Left x)

data OperatorType
    = OperatorTypeBetween -- ^ BETWEEN
    | OperatorTypeEq -- ^ EQ
    | OperatorTypeGe -- ^ GE
    | OperatorTypeLe -- ^ LE
    | OperatorTypeRefEq -- ^ REF_EQ
      deriving (Eq, Ord, Show, Generic)

instance Hashable OperatorType

instance FromText OperatorType where
    parser = match "BETWEEN" OperatorTypeBetween
         <|> match "EQ" OperatorTypeEq
         <|> match "GE" OperatorTypeGe
         <|> match "LE" OperatorTypeLe
         <|> match "REF_EQ" OperatorTypeRefEq

instance ToText OperatorType where
    toText OperatorTypeBetween = "BETWEEN"
    toText OperatorTypeEq = "EQ"
    toText OperatorTypeGe = "GE"
    toText OperatorTypeLe = "LE"
    toText OperatorTypeRefEq = "REF_EQ"

instance ToByteString OperatorType where
    toBS OperatorTypeBetween = "BETWEEN"
    toBS OperatorTypeEq = "EQ"
    toBS OperatorTypeGe = "GE"
    toBS OperatorTypeLe = "LE"
    toBS OperatorTypeRefEq = "REF_EQ"

instance ToHeader OperatorType where
    toHeader k = toHeader k . toBS

instance ToQuery OperatorType where
    toQuery = toQuery . toBS

instance FromJSON OperatorType

instance ToJSON OperatorType

data TaskStatus
    = TaskStatusFailed -- ^ FAILED
    | TaskStatusFalse -- ^ FALSE
    | TaskStatusFinished -- ^ FINISHED
      deriving (Eq, Ord, Show, Generic)

instance Hashable TaskStatus

instance FromText TaskStatus where
    parser = match "FAILED" TaskStatusFailed
         <|> match "FALSE" TaskStatusFalse
         <|> match "FINISHED" TaskStatusFinished

instance ToText TaskStatus where
    toText TaskStatusFailed = "FAILED"
    toText TaskStatusFalse = "FALSE"
    toText TaskStatusFinished = "FINISHED"

instance ToByteString TaskStatus where
    toBS TaskStatusFailed = "FAILED"
    toBS TaskStatusFalse = "FALSE"
    toBS TaskStatusFinished = "FINISHED"

instance ToHeader TaskStatus where
    toHeader k = toHeader k . toBS

instance ToQuery TaskStatus where
    toQuery = toQuery . toBS

instance ToJSON TaskStatus

-- | Query that defines the objects to be returned. The Query object can contain
-- a maximum of ten selectors. The conditions in the query are limited to
-- top-level String fields in the object. These filters can be applied to
-- components, instances, and attempts.
newtype Query = Query
    { _qSelectors :: [Selector]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Query' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Selectors ::@ @[Selector]@
--
query :: Query
query = Query
    { _qSelectors = mempty
    }

-- | List of selectors that define the query. An object must satisfy all of the
-- selectors to match the query.
qSelectors :: Lens' Query [Selector]
qSelectors = lens _qSelectors (\s a -> s { _qSelectors = a })

instance ToJSON Query

-- | A key-value pair that describes a property of a pipeline object. The value
-- is specified as either a string value (StringValue) or a reference to
-- another object (RefValue) but not as both.
data Field = Field
    { _fKey :: Text
    , _fStringValue :: Maybe Text
    , _fRefValue :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Field' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Key ::@ @Text@
--
-- * @StringValue ::@ @Maybe Text@
--
-- * @RefValue ::@ @Maybe Text@
--
field :: Text -- ^ 'fKey'
      -> Field
field p1 = Field
    { _fKey = p1
    , _fStringValue = Nothing
    , _fRefValue = Nothing
    }

-- | The field identifier.
fKey :: Lens' Field Text
fKey = lens _fKey (\s a -> s { _fKey = a })

-- | The field value, expressed as a String.
fStringValue :: Lens' Field (Maybe Text)
fStringValue = lens _fStringValue (\s a -> s { _fStringValue = a })

-- | The field value, expressed as the identifier of another object.
fRefValue :: Lens' Field (Maybe Text)
fRefValue = lens _fRefValue (\s a -> s { _fRefValue = a })

instance FromJSON Field

instance ToJSON Field

-- | Identity information for the Amazon EC2 instance that is hosting the task
-- runner. You can get this value by calling the URI,
-- http://169.254.169.254/latest/meta-data/instance-id, from the EC2 instance.
-- For more information, go to Instance Metadata in the Amazon Elastic Compute
-- Cloud User Guide. Passing in this value proves that your task runner is
-- running on an EC2 instance, and ensures the proper AWS Data Pipeline
-- service charges are applied to your pipeline.
data InstanceIdentity = InstanceIdentity
    { _iiDocument :: Maybe Text
    , _iiSignature :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceIdentity' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Document ::@ @Maybe Text@
--
-- * @Signature ::@ @Maybe Text@
--
instanceIdentity :: InstanceIdentity
instanceIdentity = InstanceIdentity
    { _iiDocument = Nothing
    , _iiSignature = Nothing
    }

-- | A description of an Amazon EC2 instance that is generated when the instance
-- is launched and exposed to the instance via the instance metadata service
-- in the form of a JSON representation of an object.
iiDocument :: Lens' InstanceIdentity (Maybe Text)
iiDocument = lens _iiDocument (\s a -> s { _iiDocument = a })

-- | A signature which can be used to verify the accuracy and authenticity of
-- the information provided in the instance identity document.
iiSignature :: Lens' InstanceIdentity (Maybe Text)
iiSignature = lens _iiSignature (\s a -> s { _iiSignature = a })

instance ToJSON InstanceIdentity

-- | Contains a logical operation for comparing the value of a field with a
-- specified value.
data Operator = Operator
    { _oType :: Maybe OperatorType
    , _oValues :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Operator' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Type ::@ @Maybe OperatorType@
--
-- * @Values ::@ @[Text]@
--
operator :: Operator
operator = Operator
    { _oType = Nothing
    , _oValues = mempty
    }

-- | The logical operation to be performed: equal (EQ), equal reference
-- (REF_EQ), less than or equal (LE), greater than or equal (GE), or between
-- (BETWEEN). Equal reference (REF_EQ) can be used only with reference fields.
-- The other comparison types can be used only with String fields. The
-- comparison types you can use apply only to certain object fields, as
-- detailed below. The comparison operators EQ and REF_EQ act on the following
-- fields: name @sphere parent @componentParent @instanceParent @status
-- @scheduledStartTime @scheduledEndTime @actualStartTime @actualEndTime The
-- comparison operators GE, LE, and BETWEEN act on the following fields:
-- @scheduledStartTime @scheduledEndTime @actualStartTime @actualEndTime Note
-- that fields beginning with the at sign (@) are read-only and set by the web
-- service. When you name fields, you should choose names containing only
-- alpha-numeric values, as symbols may be reserved by AWS Data Pipeline.
-- User-defined fields that you add to a pipeline should prefix their name
-- with the string "my".
oType :: Lens' Operator (Maybe OperatorType)
oType = lens _oType (\s a -> s { _oType = a })

-- | The value that the actual field value will be compared with.
oValues :: Lens' Operator [Text]
oValues = lens _oValues (\s a -> s { _oValues = a })

instance FromJSON Operator

instance ToJSON Operator

-- | Contains pipeline metadata.
data PipelineDescription = PipelineDescription
    { _pdPipelineId :: Text
    , _pdName :: Text
    , _pdFields :: [Field]
    , _pdDescription :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PipelineDescription' data type.
--
-- 'PipelineDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PipelineId ::@ @Text@
--
-- * @Name ::@ @Text@
--
-- * @Fields ::@ @[Field]@
--
-- * @Description ::@ @Maybe Text@
--
pipelineDescription :: Text -- ^ 'pdPipelineId'
                    -> Text -- ^ 'pdName'
                    -> [Field] -- ^ 'pdFields'
                    -> PipelineDescription
pipelineDescription p1 p2 p3 = PipelineDescription
    { _pdPipelineId = p1
    , _pdName = p2
    , _pdFields = p3
    , _pdDescription = Nothing
    }

-- | The pipeline identifier that was assigned by AWS Data Pipeline. This is a
-- string of the form df-297EG78HU43EEXAMPLE.
pdPipelineId :: Lens' PipelineDescription Text
pdPipelineId = lens _pdPipelineId (\s a -> s { _pdPipelineId = a })

-- | Name of the pipeline.
pdName :: Lens' PipelineDescription Text
pdName = lens _pdName (\s a -> s { _pdName = a })

-- | A list of read-only fields that contain metadata about the pipeline:
-- @userId, @accountId, and @pipelineState.
pdFields :: Lens' PipelineDescription [Field]
pdFields = lens _pdFields (\s a -> s { _pdFields = a })

-- | Description of the pipeline.
pdDescription :: Lens' PipelineDescription (Maybe Text)
pdDescription = lens _pdDescription (\s a -> s { _pdDescription = a })

instance FromJSON PipelineDescription

-- | Contains the name and identifier of a pipeline.
data PipelineIdName = PipelineIdName
    { _pinId :: Maybe Text
    , _pinName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PipelineIdName' data type.
--
-- 'PipelineIdName' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Maybe Text@
--
-- * @Name ::@ @Maybe Text@
--
pipelineIdName :: PipelineIdName
pipelineIdName = PipelineIdName
    { _pinId = Nothing
    , _pinName = Nothing
    }

-- | Identifier of the pipeline that was assigned by AWS Data Pipeline. This is
-- a string of the form df-297EG78HU43EEXAMPLE.
pinId :: Lens' PipelineIdName (Maybe Text)
pinId = lens _pinId (\s a -> s { _pinId = a })

-- | Name of the pipeline.
pinName :: Lens' PipelineIdName (Maybe Text)
pinName = lens _pinName (\s a -> s { _pinName = a })

instance FromJSON PipelineIdName

-- | Contains information about a pipeline object. This can be a logical,
-- physical, or physical attempt pipeline object. The complete set of
-- components of a pipeline defines the pipeline.
data PipelineObject = PipelineObject
    { _poId :: Text
    , _poName :: Text
    , _poFields :: [Field]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PipelineObject' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
-- * @Name ::@ @Text@
--
-- * @Fields ::@ @[Field]@
--
pipelineObject :: Text -- ^ 'poId'
               -> Text -- ^ 'poName'
               -> [Field] -- ^ 'poFields'
               -> PipelineObject
pipelineObject p1 p2 p3 = PipelineObject
    { _poId = p1
    , _poName = p2
    , _poFields = p3
    }

-- | Identifier of the object.
poId :: Lens' PipelineObject Text
poId = lens _poId (\s a -> s { _poId = a })

-- | Name of the object.
poName :: Lens' PipelineObject Text
poName = lens _poName (\s a -> s { _poName = a })

-- | Key-value pairs that define the properties of the object.
poFields :: Lens' PipelineObject [Field]
poFields = lens _poFields (\s a -> s { _poFields = a })

instance FromJSON PipelineObject

instance ToJSON PipelineObject

-- | A comparision that is used to determine whether a query should return this
-- object.
data Selector = Selector
    { _sFieldName :: Maybe Text
    , _sOperator :: Maybe Operator
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Selector' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @FieldName ::@ @Maybe Text@
--
-- * @Operator ::@ @Maybe Operator@
--
selector :: Selector
selector = Selector
    { _sFieldName = Nothing
    , _sOperator = Nothing
    }

-- | The name of the field that the operator will be applied to. The field name
-- is the "key" portion of the field definition in the pipeline definition
-- syntax that is used by the AWS Data Pipeline API. If the field is not set
-- on the object, the condition fails.
sFieldName :: Lens' Selector (Maybe Text)
sFieldName = lens _sFieldName (\s a -> s { _sFieldName = a })

-- | Contains a logical operation for comparing the value of a field with a
-- specified value.
sOperator :: Lens' Selector (Maybe Operator)
sOperator = lens _sOperator (\s a -> s { _sOperator = a })

instance ToJSON Selector

-- | An instance of PollForTaskResult, which contains an instance of TaskObject.
-- The returned object contains all the information needed to complete the
-- task that is being assigned to the task runner. One of the fields returned
-- in this object is taskId, which contains an identifier for the task being
-- assigned. The calling task runner uses taskId in subsequent calls to
-- ReportTaskProgress and SetTaskStatus.
data TaskObject = TaskObject
    { _toTaskId :: Maybe Text
    , _toPipelineId :: Maybe Text
    , _toAttemptId :: Maybe Text
    , _toObjects :: Map Text PipelineObject
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'TaskObject' data type.
--
-- 'TaskObject' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TaskId ::@ @Maybe Text@
--
-- * @PipelineId ::@ @Maybe Text@
--
-- * @AttemptId ::@ @Maybe Text@
--
-- * @Objects ::@ @Map Text PipelineObject@
--
taskObject :: TaskObject
taskObject = TaskObject
    { _toTaskId = Nothing
    , _toPipelineId = Nothing
    , _toAttemptId = Nothing
    , _toObjects = mempty
    }

-- | An internal identifier for the task. This ID is passed to the SetTaskStatus
-- and ReportTaskProgress actions.
toTaskId :: Lens' TaskObject (Maybe Text)
toTaskId = lens _toTaskId (\s a -> s { _toTaskId = a })

-- | Identifier of the pipeline that provided the task.
toPipelineId :: Lens' TaskObject (Maybe Text)
toPipelineId = lens _toPipelineId (\s a -> s { _toPipelineId = a })

-- | Identifier of the pipeline task attempt object. AWS Data Pipeline uses this
-- value to track how many times a task is attempted.
toAttemptId :: Lens' TaskObject (Maybe Text)
toAttemptId = lens _toAttemptId (\s a -> s { _toAttemptId = a })

-- | Connection information for the location where the task runner will publish
-- the output of the task.
toObjects :: Lens' TaskObject (Map Text PipelineObject)
toObjects = lens _toObjects (\s a -> s { _toObjects = a })

instance FromJSON TaskObject

-- | Defines a validation error returned by PutPipelineDefinition or
-- ValidatePipelineDefinition. Validation errors prevent pipeline activation.
-- The set of validation errors that can be returned are defined by AWS Data
-- Pipeline.
data ValidationError = ValidationError
    { _veId :: Maybe Text
    , _veErrors :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ValidationError' data type.
--
-- 'ValidationError' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Maybe Text@
--
-- * @Errors ::@ @[Text]@
--
validationError :: ValidationError
validationError = ValidationError
    { _veId = Nothing
    , _veErrors = mempty
    }

-- | The identifier of the object that contains the validation error.
veId :: Lens' ValidationError (Maybe Text)
veId = lens _veId (\s a -> s { _veId = a })

-- | A description of the validation error.
veErrors :: Lens' ValidationError [Text]
veErrors = lens _veErrors (\s a -> s { _veErrors = a })

instance FromJSON ValidationError

-- | Defines a validation warning returned by PutPipelineDefinition or
-- ValidatePipelineDefinition. Validation warnings do not prevent pipeline
-- activation. The set of validation warnings that can be returned are defined
-- by AWS Data Pipeline.
data ValidationWarning = ValidationWarning
    { _vwId :: Maybe Text
    , _vwWarnings :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ValidationWarning' data type.
--
-- 'ValidationWarning' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Maybe Text@
--
-- * @Warnings ::@ @[Text]@
--
validationWarning :: ValidationWarning
validationWarning = ValidationWarning
    { _vwId = Nothing
    , _vwWarnings = mempty
    }

-- | The identifier of the object that contains the validation warning.
vwId :: Lens' ValidationWarning (Maybe Text)
vwId = lens _vwId (\s a -> s { _vwId = a })

-- | A description of the validation warning.
vwWarnings :: Lens' ValidationWarning [Text]
vwWarnings = lens _vwWarnings (\s a -> s { _vwWarnings = a })

instance FromJSON ValidationWarning

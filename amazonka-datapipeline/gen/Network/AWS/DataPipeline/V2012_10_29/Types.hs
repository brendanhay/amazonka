{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.V2012_10_29.Types
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
module Network.AWS.DataPipeline.V2012_10_29.Types
    (
    -- * Service
      DataPipeline
    -- ** Errors
    , Er (..)
    -- * OperatorType
    , OperatorType (..)

    -- * TaskStatus
    , TaskStatus (..)

    -- * Query
    , Query
    , mkQuery
    , qySelectors

    -- * Field
    , Field
    , mkField
    , fKey
    , fStringValue
    , fRefValue

    -- * InstanceIdentity
    , InstanceIdentity
    , mkInstanceIdentity
    , ijDocument
    , ijSignature

    -- * Operator
    , Operator
    , mkOperator
    , orType
    , orValues

    -- * PipelineDescription
    , PipelineDescription
    , pdPipelineId
    , pdName
    , pdFields
    , pdDescription

    -- * PipelineIdName
    , PipelineIdName
    , pinId
    , pinName

    -- * PipelineObject
    , PipelineObject
    , mkPipelineObject
    , poId
    , poName
    , poFields

    -- * Selector
    , Selector
    , mkSelector
    , srFieldName
    , srOperator

    -- * TaskObject
    , TaskObject
    , toTaskId
    , toPipelineId
    , toAttemptId
    , toObjects

    -- * ValidationError
    , ValidationError
    , vfId
    , vfErrors

    -- * ValidationWarning
    , ValidationWarning
    , vxId
    , vxWarnings
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2012-10-29@) of the
-- @AWS Data Pipeline@ service.
data DataPipeline deriving (Typeable)

instance AWSService DataPipeline where
    type Sg DataPipeline = V4
    data Er DataPipeline
        = DataPipelineClient HttpException
        | DataPipelineSerializer String
        | DataPipelineService String
        | InternalServiceError
            { _iseMessage :: Maybe Text
            }
        | InvalidRequestException
            { _ireMessage :: Maybe Text
            }
        | PipelineDeletedException
            { _pdeMessage :: Maybe Text
            }
        | PipelineNotFoundException
            { _pnfeMessage :: Maybe Text
            }
        | TaskNotFoundException
            { _tnfeMessage :: Maybe Text
            }

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "datapipeline"
        , _svcVersion  = "2012-10-29"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er DataPipeline)
deriving instance Generic (Er DataPipeline)

instance AWSError (Er DataPipeline) where
    awsError = const "DataPipelineError"

instance AWSServiceError (Er DataPipeline) where
    serviceError    = DataPipelineService
    clientError     = DataPipelineClient
    serializerError = DataPipelineSerializer

instance Exception (Er DataPipeline)

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
data OperatorType
    = OperatorTypeBetween -- ^ BETWEEN
    | OperatorTypeEq -- ^ EQ
    | OperatorTypeGe -- ^ GE
    | OperatorTypeLe -- ^ LE
    | OperatorTypeRefEq -- ^ REF_EQ
      deriving (Eq, Show, Generic)

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

-- | If FINISHED, the task successfully completed. If FAILED the task ended
-- unsuccessfully. The FALSE value is used by preconditions.
data TaskStatus
    = TaskStatusFailed -- ^ FAILED
    | TaskStatusFalse -- ^ FALSE
    | TaskStatusFinished -- ^ FINISHED
      deriving (Eq, Show, Generic)

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
    { _qySelectors :: [Selector]
      -- ^ List of selectors that define the query. An object must satisfy
      -- all of the selectors to match the query.
    } deriving (Show, Generic)

-- | List of selectors that define the query. An object must satisfy all of the
-- selectors to match the query.
qySelectors :: Lens' Query ([Selector])
qySelectors = lens _qySelectors (\s a -> s { _qySelectors = a })
{-# INLINE qySelectors #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Query' data type to populate a request.
mkQuery :: Query
mkQuery = Query
    { _qySelectors = mempty
    }
{-# INLINE mkQuery #-}

instance ToJSON Query

-- | A key-value pair that describes a property of a pipeline object. The value
-- is specified as either a string value (StringValue) or a reference to
-- another object (RefValue) but not as both.
data Field = Field
    { _fKey :: Text
      -- ^ The field identifier.
    , _fStringValue :: Maybe Text
      -- ^ The field value, expressed as a String.
    , _fRefValue :: Maybe Text
      -- ^ The field value, expressed as the identifier of another object.
    } deriving (Show, Generic)

-- | The field identifier.
fKey :: Lens' Field (Text)
fKey = lens _fKey (\s a -> s { _fKey = a })
{-# INLINE fKey #-}

-- | The field value, expressed as a String.
fStringValue :: Lens' Field (Maybe Text)
fStringValue = lens _fStringValue (\s a -> s { _fStringValue = a })
{-# INLINE fStringValue #-}

-- | The field value, expressed as the identifier of another object.
fRefValue :: Lens' Field (Maybe Text)
fRefValue = lens _fRefValue (\s a -> s { _fRefValue = a })
{-# INLINE fRefValue #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Field' data type to populate a request.
mkField :: Text -- ^ 'fKey'
        -> Field
mkField p1 = Field
    { _fKey = p1
    , _fStringValue = Nothing
    , _fRefValue = Nothing
    }
{-# INLINE mkField #-}

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
    { _ijDocument :: Maybe Text
      -- ^ A description of an Amazon EC2 instance that is generated when
      -- the instance is launched and exposed to the instance via the
      -- instance metadata service in the form of a JSON representation of
      -- an object.
    , _ijSignature :: Maybe Text
      -- ^ A signature which can be used to verify the accuracy and
      -- authenticity of the information provided in the instance identity
      -- document.
    } deriving (Show, Generic)

-- | A description of an Amazon EC2 instance that is generated when the instance
-- is launched and exposed to the instance via the instance metadata service
-- in the form of a JSON representation of an object.
ijDocument :: Lens' InstanceIdentity (Maybe Text)
ijDocument = lens _ijDocument (\s a -> s { _ijDocument = a })
{-# INLINE ijDocument #-}

-- | A signature which can be used to verify the accuracy and authenticity of
-- the information provided in the instance identity document.
ijSignature :: Lens' InstanceIdentity (Maybe Text)
ijSignature = lens _ijSignature (\s a -> s { _ijSignature = a })
{-# INLINE ijSignature #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'InstanceIdentity' data type to populate a request.
mkInstanceIdentity :: InstanceIdentity
mkInstanceIdentity = InstanceIdentity
    { _ijDocument = Nothing
    , _ijSignature = Nothing
    }
{-# INLINE mkInstanceIdentity #-}

instance ToJSON InstanceIdentity

-- | Contains a logical operation for comparing the value of a field with a
-- specified value.
data Operator = Operator
    { _orType :: Maybe OperatorType
      -- ^ The logical operation to be performed: equal (EQ), equal
      -- reference (REF_EQ), less than or equal (LE), greater than or
      -- equal (GE), or between (BETWEEN). Equal reference (REF_EQ) can be
      -- used only with reference fields. The other comparison types can
      -- be used only with String fields. The comparison types you can use
      -- apply only to certain object fields, as detailed below. The
      -- comparison operators EQ and REF_EQ act on the following fields:
      -- name @sphere parent @componentParent @instanceParent @status
      -- @scheduledStartTime @scheduledEndTime @actualStartTime
      -- @actualEndTime The comparison operators GE, LE, and BETWEEN act
      -- on the following fields: @scheduledStartTime @scheduledEndTime
      -- @actualStartTime @actualEndTime Note that fields beginning with
      -- the at sign (@) are read-only and set by the web service. When
      -- you name fields, you should choose names containing only
      -- alpha-numeric values, as symbols may be reserved by AWS Data
      -- Pipeline. User-defined fields that you add to a pipeline should
      -- prefix their name with the string "my".
    , _orValues :: [Text]
      -- ^ The value that the actual field value will be compared with.
    } deriving (Show, Generic)

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
orType :: Lens' Operator (Maybe OperatorType)
orType = lens _orType (\s a -> s { _orType = a })
{-# INLINE orType #-}

-- | The value that the actual field value will be compared with.
orValues :: Lens' Operator ([Text])
orValues = lens _orValues (\s a -> s { _orValues = a })
{-# INLINE orValues #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Operator' data type to populate a request.
mkOperator :: Operator
mkOperator = Operator
    { _orType = Nothing
    , _orValues = mempty
    }
{-# INLINE mkOperator #-}

instance FromJSON Operator

instance ToJSON Operator

-- | Contains pipeline metadata.
data PipelineDescription = PipelineDescription
    { _pdPipelineId :: Text
      -- ^ The pipeline identifier that was assigned by AWS Data Pipeline.
      -- This is a string of the form df-297EG78HU43EEXAMPLE.
    , _pdName :: Text
      -- ^ Name of the pipeline.
    , _pdFields :: [Field]
      -- ^ A list of read-only fields that contain metadata about the
      -- pipeline: @userId, @accountId, and @pipelineState.
    , _pdDescription :: Maybe Text
      -- ^ Description of the pipeline.
    } deriving (Show, Generic)

-- | The pipeline identifier that was assigned by AWS Data Pipeline. This is a
-- string of the form df-297EG78HU43EEXAMPLE.
pdPipelineId :: Lens' PipelineDescription (Text)
pdPipelineId = lens _pdPipelineId (\s a -> s { _pdPipelineId = a })
{-# INLINE pdPipelineId #-}

-- | Name of the pipeline.
pdName :: Lens' PipelineDescription (Text)
pdName = lens _pdName (\s a -> s { _pdName = a })
{-# INLINE pdName #-}

-- | A list of read-only fields that contain metadata about the pipeline:
-- @userId, @accountId, and @pipelineState.
pdFields :: Lens' PipelineDescription ([Field])
pdFields = lens _pdFields (\s a -> s { _pdFields = a })
{-# INLINE pdFields #-}

-- | Description of the pipeline.
pdDescription :: Lens' PipelineDescription (Maybe Text)
pdDescription = lens _pdDescription (\s a -> s { _pdDescription = a })
{-# INLINE pdDescription #-}

instance FromJSON PipelineDescription

-- | Contains the name and identifier of a pipeline.
data PipelineIdName = PipelineIdName
    { _pinId :: Maybe Text
      -- ^ Identifier of the pipeline that was assigned by AWS Data
      -- Pipeline. This is a string of the form df-297EG78HU43EEXAMPLE.
    , _pinName :: Maybe Text
      -- ^ Name of the pipeline.
    } deriving (Show, Generic)

-- | Identifier of the pipeline that was assigned by AWS Data Pipeline. This is
-- a string of the form df-297EG78HU43EEXAMPLE.
pinId :: Lens' PipelineIdName (Maybe Text)
pinId = lens _pinId (\s a -> s { _pinId = a })
{-# INLINE pinId #-}

-- | Name of the pipeline.
pinName :: Lens' PipelineIdName (Maybe Text)
pinName = lens _pinName (\s a -> s { _pinName = a })
{-# INLINE pinName #-}

instance FromJSON PipelineIdName

-- | Contains information about a pipeline object. This can be a logical,
-- physical, or physical attempt pipeline object. The complete set of
-- components of a pipeline defines the pipeline.
data PipelineObject = PipelineObject
    { _poId :: Text
      -- ^ Identifier of the object.
    , _poName :: Text
      -- ^ Name of the object.
    , _poFields :: [Field]
      -- ^ Key-value pairs that define the properties of the object.
    } deriving (Show, Generic)

-- | Identifier of the object.
poId :: Lens' PipelineObject (Text)
poId = lens _poId (\s a -> s { _poId = a })
{-# INLINE poId #-}

-- | Name of the object.
poName :: Lens' PipelineObject (Text)
poName = lens _poName (\s a -> s { _poName = a })
{-# INLINE poName #-}

-- | Key-value pairs that define the properties of the object.
poFields :: Lens' PipelineObject ([Field])
poFields = lens _poFields (\s a -> s { _poFields = a })
{-# INLINE poFields #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PipelineObject' data type to populate a request.
mkPipelineObject :: Text -- ^ 'poId'
                 -> Text -- ^ 'poName'
                 -> [Field] -- ^ 'poFields'
                 -> PipelineObject
mkPipelineObject p1 p2 p3 = PipelineObject
    { _poId = p1
    , _poName = p2
    , _poFields = p3
    }
{-# INLINE mkPipelineObject #-}

instance FromJSON PipelineObject

instance ToJSON PipelineObject

-- | A comparision that is used to determine whether a query should return this
-- object.
data Selector = Selector
    { _srFieldName :: Maybe Text
      -- ^ The name of the field that the operator will be applied to. The
      -- field name is the "key" portion of the field definition in the
      -- pipeline definition syntax that is used by the AWS Data Pipeline
      -- API. If the field is not set on the object, the condition fails.
    , _srOperator :: Maybe Operator
      -- ^ Contains a logical operation for comparing the value of a field
      -- with a specified value.
    } deriving (Show, Generic)

-- | The name of the field that the operator will be applied to. The field name
-- is the "key" portion of the field definition in the pipeline definition
-- syntax that is used by the AWS Data Pipeline API. If the field is not set
-- on the object, the condition fails.
srFieldName :: Lens' Selector (Maybe Text)
srFieldName = lens _srFieldName (\s a -> s { _srFieldName = a })
{-# INLINE srFieldName #-}

-- | Contains a logical operation for comparing the value of a field with a
-- specified value.
srOperator :: Lens' Selector (Maybe Operator)
srOperator = lens _srOperator (\s a -> s { _srOperator = a })
{-# INLINE srOperator #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Selector' data type to populate a request.
mkSelector :: Selector
mkSelector = Selector
    { _srFieldName = Nothing
    , _srOperator = Nothing
    }
{-# INLINE mkSelector #-}

instance ToJSON Selector

-- | An instance of PollForTaskResult, which contains an instance of TaskObject.
-- The returned object contains all the information needed to complete the
-- task that is being assigned to the task runner. One of the fields returned
-- in this object is taskId, which contains an identifier for the task being
-- assigned. The calling task runner uses taskId in subsequent calls to
-- ReportTaskProgress and SetTaskStatus.
data TaskObject = TaskObject
    { _toTaskId :: Maybe Text
      -- ^ An internal identifier for the task. This ID is passed to the
      -- SetTaskStatus and ReportTaskProgress actions.
    , _toPipelineId :: Maybe Text
      -- ^ Identifier of the pipeline that provided the task.
    , _toAttemptId :: Maybe Text
      -- ^ Identifier of the pipeline task attempt object. AWS Data Pipeline
      -- uses this value to track how many times a task is attempted.
    , _toObjects :: Map Text PipelineObject
      -- ^ Connection information for the location where the task runner
      -- will publish the output of the task.
    } deriving (Show, Generic)

-- | An internal identifier for the task. This ID is passed to the SetTaskStatus
-- and ReportTaskProgress actions.
toTaskId :: Lens' TaskObject (Maybe Text)
toTaskId = lens _toTaskId (\s a -> s { _toTaskId = a })
{-# INLINE toTaskId #-}

-- | Identifier of the pipeline that provided the task.
toPipelineId :: Lens' TaskObject (Maybe Text)
toPipelineId = lens _toPipelineId (\s a -> s { _toPipelineId = a })
{-# INLINE toPipelineId #-}

-- | Identifier of the pipeline task attempt object. AWS Data Pipeline uses this
-- value to track how many times a task is attempted.
toAttemptId :: Lens' TaskObject (Maybe Text)
toAttemptId = lens _toAttemptId (\s a -> s { _toAttemptId = a })
{-# INLINE toAttemptId #-}

-- | Connection information for the location where the task runner will publish
-- the output of the task.
toObjects :: Lens' TaskObject (Map Text PipelineObject)
toObjects = lens _toObjects (\s a -> s { _toObjects = a })
{-# INLINE toObjects #-}

instance FromJSON TaskObject

-- | Defines a validation error returned by PutPipelineDefinition or
-- ValidatePipelineDefinition. Validation errors prevent pipeline activation.
-- The set of validation errors that can be returned are defined by AWS Data
-- Pipeline.
data ValidationError = ValidationError
    { _vfId :: Maybe Text
      -- ^ The identifier of the object that contains the validation error.
    , _vfErrors :: [Text]
      -- ^ A description of the validation error.
    } deriving (Show, Generic)

-- | The identifier of the object that contains the validation error.
vfId :: Lens' ValidationError (Maybe Text)
vfId = lens _vfId (\s a -> s { _vfId = a })
{-# INLINE vfId #-}

-- | A description of the validation error.
vfErrors :: Lens' ValidationError ([Text])
vfErrors = lens _vfErrors (\s a -> s { _vfErrors = a })
{-# INLINE vfErrors #-}

instance FromJSON ValidationError

-- | Defines a validation warning returned by PutPipelineDefinition or
-- ValidatePipelineDefinition. Validation warnings do not prevent pipeline
-- activation. The set of validation warnings that can be returned are defined
-- by AWS Data Pipeline.
data ValidationWarning = ValidationWarning
    { _vxId :: Maybe Text
      -- ^ The identifier of the object that contains the validation
      -- warning.
    , _vxWarnings :: [Text]
      -- ^ A description of the validation warning.
    } deriving (Show, Generic)

-- | The identifier of the object that contains the validation warning.
vxId :: Lens' ValidationWarning (Maybe Text)
vxId = lens _vxId (\s a -> s { _vxId = a })
{-# INLINE vxId #-}

-- | A description of the validation warning.
vxWarnings :: Lens' ValidationWarning ([Text])
vxWarnings = lens _vxWarnings (\s a -> s { _vxWarnings = a })
{-# INLINE vxWarnings #-}

instance FromJSON ValidationWarning

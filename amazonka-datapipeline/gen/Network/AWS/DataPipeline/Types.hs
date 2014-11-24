{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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

module Network.AWS.DataPipeline.Types
    (
    -- * Service
      DataPipeline
    -- ** Error
    , JSONError

    -- * PipelineObject
    , PipelineObject
    , pipelineObject
    , poFields
    , poId
    , poName

    -- * Field
    , Field
    , field
    , fKey
    , fRefValue
    , fStringValue

    -- * Selector
    , Selector
    , selector
    , sFieldName
    , sOperator

    -- * Operator
    , Operator
    , operator
    , oType
    , oValues

    -- * TaskObject
    , TaskObject
    , taskObject
    , toAttemptId
    , toObjects
    , toPipelineId
    , toTaskId

    -- * ValidationError
    , ValidationError
    , validationError
    , veErrors
    , veId

    -- * PipelineDescription
    , PipelineDescription
    , pipelineDescription
    , pdDescription
    , pdFields
    , pdName
    , pdPipelineId

    -- * InstanceIdentity
    , InstanceIdentity
    , instanceIdentity
    , iiDocument
    , iiSignature

    -- * Query
    , Query
    , query
    , qSelectors

    -- * OperatorType
    , OperatorType (..)

    -- * PipelineIdName
    , PipelineIdName
    , pipelineIdName
    , pinId
    , pinName

    -- * TaskStatus
    , TaskStatus (..)

    -- * ValidationWarning
    , ValidationWarning
    , validationWarning
    , vwId
    , vwWarnings
    ) where

import Data.Char (isUpper)
import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing.V4
import qualified GHC.Exts

-- | Version @2012-10-29@ of the Amazon Data Pipeline service.
data DataPipeline

instance AWSService DataPipeline where
    type Sg DataPipeline = V4
    type Er DataPipeline = JSONError

    service = Service
        { _svcEndpoint     = regional
        , _svcAbbrev       = "DataPipeline"
        , _svcPrefix       = "datapipeline"
        , _svcVersion      = "2012-10-29"
        , _svcTargetPrefix = Just "DataPipeline"
        , _svcJSONVersion  = Just "1.1"
        }

    handle = jsonError statusSuccess

data PipelineObject = PipelineObject
    { _poFields :: List "fields" Field
    , _poId     :: Text
    , _poName   :: Text
    } deriving (Eq, Show)

-- | 'PipelineObject' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'poFields' @::@ ['Field']
--
-- * 'poId' @::@ 'Text'
--
-- * 'poName' @::@ 'Text'
--
pipelineObject :: Text -- ^ 'poId'
               -> Text -- ^ 'poName'
               -> PipelineObject
pipelineObject p1 p2 = PipelineObject
    { _poId     = p1
    , _poName   = p2
    , _poFields = mempty
    }

-- | Key-value pairs that define the properties of the object.
poFields :: Lens' PipelineObject [Field]
poFields = lens _poFields (\s a -> s { _poFields = a }) . _List

-- | Identifier of the object.
poId :: Lens' PipelineObject Text
poId = lens _poId (\s a -> s { _poId = a })

-- | Name of the object.
poName :: Lens' PipelineObject Text
poName = lens _poName (\s a -> s { _poName = a })

instance FromJSON PipelineObject where
    parseJSON = withObject "PipelineObject" $ \o -> PipelineObject
        <$> o .:  "fields"
        <*> o .:  "id"
        <*> o .:  "name"

instance ToJSON PipelineObject where
    toJSON PipelineObject{..} = object
        [ "id"     .= _poId
        , "name"   .= _poName
        , "fields" .= _poFields
        ]

data Field = Field
    { _fKey         :: Text
    , _fRefValue    :: Maybe Text
    , _fStringValue :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'Field' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fKey' @::@ 'Text'
--
-- * 'fRefValue' @::@ 'Maybe' 'Text'
--
-- * 'fStringValue' @::@ 'Maybe' 'Text'
--
field :: Text -- ^ 'fKey'
      -> Field
field p1 = Field
    { _fKey         = p1
    , _fStringValue = Nothing
    , _fRefValue    = Nothing
    }

-- | The field identifier.
fKey :: Lens' Field Text
fKey = lens _fKey (\s a -> s { _fKey = a })

-- | The field value, expressed as the identifier of another object.
fRefValue :: Lens' Field (Maybe Text)
fRefValue = lens _fRefValue (\s a -> s { _fRefValue = a })

-- | The field value, expressed as a String.
fStringValue :: Lens' Field (Maybe Text)
fStringValue = lens _fStringValue (\s a -> s { _fStringValue = a })

instance FromJSON Field where
    parseJSON = withObject "Field" $ \o -> Field
        <$> o .:  "key"
        <*> o .:? "refValue"
        <*> o .:? "stringValue"

instance ToJSON Field where
    toJSON Field{..} = object
        [ "key"         .= _fKey
        , "stringValue" .= _fStringValue
        , "refValue"    .= _fRefValue
        ]

data Selector = Selector
    { _sFieldName :: Maybe Text
    , _sOperator  :: Maybe Operator
    } deriving (Eq, Show)

-- | 'Selector' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sFieldName' @::@ 'Maybe' 'Text'
--
-- * 'sOperator' @::@ 'Maybe' 'Operator'
--
selector :: Selector
selector = Selector
    { _sFieldName = Nothing
    , _sOperator  = Nothing
    }

-- | The name of the field that the operator will be applied to. The field
-- name is the "key" portion of the field definition in the pipeline
-- definition syntax that is used by the AWS Data Pipeline API. If the field
-- is not set on the object, the condition fails.
sFieldName :: Lens' Selector (Maybe Text)
sFieldName = lens _sFieldName (\s a -> s { _sFieldName = a })

sOperator :: Lens' Selector (Maybe Operator)
sOperator = lens _sOperator (\s a -> s { _sOperator = a })

instance FromJSON Selector where
    parseJSON = withObject "Selector" $ \o -> Selector
        <$> o .:? "fieldName"
        <*> o .:? "operator"

instance ToJSON Selector where
    toJSON Selector{..} = object
        [ "fieldName" .= _sFieldName
        , "operator"  .= _sOperator
        ]

data Operator = Operator
    { _oType   :: Maybe OperatorType
    , _oValues :: List "values" Text
    } deriving (Eq, Show)

-- | 'Operator' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'oType' @::@ 'Maybe' 'OperatorType'
--
-- * 'oValues' @::@ ['Text']
--
operator :: Operator
operator = Operator
    { _oType   = Nothing
    , _oValues = mempty
    }

-- | The logical operation to be performed: equal (EQ), equal reference
-- (REF_EQ), less than or equal (LE), greater than or equal (GE), or between
-- (BETWEEN). Equal reference (REF_EQ) can be used only with reference
-- fields. The other comparison types can be used only with String fields.
-- The comparison types you can use apply only to certain object fields, as
-- detailed below. The comparison operators EQ and REF_EQ act on the
-- following fields: name @sphere parent @componentParent @instanceParent
-- @status @scheduledStartTime @scheduledEndTime @actualStartTime
-- @actualEndTime The comparison operators GE, LE, and BETWEEN act on the
-- following fields: @scheduledStartTime @scheduledEndTime @actualStartTime
-- @actualEndTime Note that fields beginning with the at sign (@) are
-- read-only and set by the web service. When you name fields, you should
-- choose names containing only alpha-numeric values, as symbols may be
-- reserved by AWS Data Pipeline. User-defined fields that you add to a
-- pipeline should prefix their name with the string "my".
oType :: Lens' Operator (Maybe OperatorType)
oType = lens _oType (\s a -> s { _oType = a })

-- | The value that the actual field value will be compared with.
oValues :: Lens' Operator [Text]
oValues = lens _oValues (\s a -> s { _oValues = a }) . _List

instance FromJSON Operator where
    parseJSON = withObject "Operator" $ \o -> Operator
        <$> o .:? "type"
        <*> o .:  "values"

instance ToJSON Operator where
    toJSON Operator{..} = object
        [ "type"   .= _oType
        , "values" .= _oValues
        ]

data TaskObject = TaskObject
    { _toAttemptId  :: Maybe Text
    , _toObjects    :: Map Text PipelineObject
    , _toPipelineId :: Maybe Text
    , _toTaskId     :: Maybe Text
    } deriving (Eq, Show)

-- | 'TaskObject' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'toAttemptId' @::@ 'Maybe' 'Text'
--
-- * 'toObjects' @::@ 'HashMap' 'Text' 'PipelineObject'
--
-- * 'toPipelineId' @::@ 'Maybe' 'Text'
--
-- * 'toTaskId' @::@ 'Maybe' 'Text'
--
taskObject :: TaskObject
taskObject = TaskObject
    { _toTaskId     = Nothing
    , _toPipelineId = Nothing
    , _toAttemptId  = Nothing
    , _toObjects    = mempty
    }

-- | Identifier of the pipeline task attempt object. AWS Data Pipeline uses
-- this value to track how many times a task is attempted.
toAttemptId :: Lens' TaskObject (Maybe Text)
toAttemptId = lens _toAttemptId (\s a -> s { _toAttemptId = a })

-- | Connection information for the location where the task runner will
-- publish the output of the task.
toObjects :: Lens' TaskObject (HashMap Text PipelineObject)
toObjects = lens _toObjects (\s a -> s { _toObjects = a }) . _Map

-- | Identifier of the pipeline that provided the task.
toPipelineId :: Lens' TaskObject (Maybe Text)
toPipelineId = lens _toPipelineId (\s a -> s { _toPipelineId = a })

-- | An internal identifier for the task. This ID is passed to the
-- SetTaskStatus and ReportTaskProgress actions.
toTaskId :: Lens' TaskObject (Maybe Text)
toTaskId = lens _toTaskId (\s a -> s { _toTaskId = a })

instance FromJSON TaskObject where
    parseJSON = withObject "TaskObject" $ \o -> TaskObject
        <$> o .:? "attemptId"
        <*> o .:  "objects"
        <*> o .:? "pipelineId"
        <*> o .:? "taskId"

instance ToJSON TaskObject where
    toJSON TaskObject{..} = object
        [ "taskId"     .= _toTaskId
        , "pipelineId" .= _toPipelineId
        , "attemptId"  .= _toAttemptId
        , "objects"    .= _toObjects
        ]

data ValidationError = ValidationError
    { _veErrors :: List "errors" Text
    , _veId     :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'ValidationError' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'veErrors' @::@ ['Text']
--
-- * 'veId' @::@ 'Maybe' 'Text'
--
validationError :: ValidationError
validationError = ValidationError
    { _veId     = Nothing
    , _veErrors = mempty
    }

-- | A description of the validation error.
veErrors :: Lens' ValidationError [Text]
veErrors = lens _veErrors (\s a -> s { _veErrors = a }) . _List

-- | The identifier of the object that contains the validation error.
veId :: Lens' ValidationError (Maybe Text)
veId = lens _veId (\s a -> s { _veId = a })

instance FromJSON ValidationError where
    parseJSON = withObject "ValidationError" $ \o -> ValidationError
        <$> o .:  "errors"
        <*> o .:? "id"

instance ToJSON ValidationError where
    toJSON ValidationError{..} = object
        [ "id"     .= _veId
        , "errors" .= _veErrors
        ]

data PipelineDescription = PipelineDescription
    { _pdDescription :: Maybe Text
    , _pdFields      :: List "fields" Field
    , _pdName        :: Text
    , _pdPipelineId  :: Text
    } deriving (Eq, Show)

-- | 'PipelineDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pdDescription' @::@ 'Maybe' 'Text'
--
-- * 'pdFields' @::@ ['Field']
--
-- * 'pdName' @::@ 'Text'
--
-- * 'pdPipelineId' @::@ 'Text'
--
pipelineDescription :: Text -- ^ 'pdPipelineId'
                    -> Text -- ^ 'pdName'
                    -> PipelineDescription
pipelineDescription p1 p2 = PipelineDescription
    { _pdPipelineId  = p1
    , _pdName        = p2
    , _pdFields      = mempty
    , _pdDescription = Nothing
    }

-- | Description of the pipeline.
pdDescription :: Lens' PipelineDescription (Maybe Text)
pdDescription = lens _pdDescription (\s a -> s { _pdDescription = a })

-- | A list of read-only fields that contain metadata about the pipeline:
-- @userId, @accountId, and @pipelineState.
pdFields :: Lens' PipelineDescription [Field]
pdFields = lens _pdFields (\s a -> s { _pdFields = a }) . _List

-- | Name of the pipeline.
pdName :: Lens' PipelineDescription Text
pdName = lens _pdName (\s a -> s { _pdName = a })

-- | The pipeline identifier that was assigned by AWS Data Pipeline. This is a
-- string of the form df-297EG78HU43EEXAMPLE.
pdPipelineId :: Lens' PipelineDescription Text
pdPipelineId = lens _pdPipelineId (\s a -> s { _pdPipelineId = a })

instance FromJSON PipelineDescription where
    parseJSON = withObject "PipelineDescription" $ \o -> PipelineDescription
        <$> o .:? "description"
        <*> o .:  "fields"
        <*> o .:  "name"
        <*> o .:  "pipelineId"

instance ToJSON PipelineDescription where
    toJSON PipelineDescription{..} = object
        [ "pipelineId"  .= _pdPipelineId
        , "name"        .= _pdName
        , "fields"      .= _pdFields
        , "description" .= _pdDescription
        ]

data InstanceIdentity = InstanceIdentity
    { _iiDocument  :: Maybe Text
    , _iiSignature :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'InstanceIdentity' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iiDocument' @::@ 'Maybe' 'Text'
--
-- * 'iiSignature' @::@ 'Maybe' 'Text'
--
instanceIdentity :: InstanceIdentity
instanceIdentity = InstanceIdentity
    { _iiDocument  = Nothing
    , _iiSignature = Nothing
    }

-- | A description of an Amazon EC2 instance that is generated when the
-- instance is launched and exposed to the instance via the instance
-- metadata service in the form of a JSON representation of an object.
iiDocument :: Lens' InstanceIdentity (Maybe Text)
iiDocument = lens _iiDocument (\s a -> s { _iiDocument = a })

-- | A signature which can be used to verify the accuracy and authenticity of
-- the information provided in the instance identity document.
iiSignature :: Lens' InstanceIdentity (Maybe Text)
iiSignature = lens _iiSignature (\s a -> s { _iiSignature = a })

instance FromJSON InstanceIdentity where
    parseJSON = withObject "InstanceIdentity" $ \o -> InstanceIdentity
        <$> o .:? "document"
        <*> o .:? "signature"

instance ToJSON InstanceIdentity where
    toJSON InstanceIdentity{..} = object
        [ "document"  .= _iiDocument
        , "signature" .= _iiSignature
        ]

newtype Query = Query
    { _qSelectors :: List "selectors" Selector
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList Query where
    type Item Query = Selector

    fromList = Query . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _qSelectors

-- | 'Query' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'qSelectors' @::@ ['Selector']
--
query :: Query
query = Query
    { _qSelectors = mempty
    }

-- | List of selectors that define the query. An object must satisfy all of
-- the selectors to match the query.
qSelectors :: Lens' Query [Selector]
qSelectors = lens _qSelectors (\s a -> s { _qSelectors = a }) . _List

instance FromJSON Query where
    parseJSON = withObject "Query" $ \o -> Query
        <$> o .:  "selectors"

instance ToJSON Query where
    toJSON Query{..} = object
        [ "selectors" .= _qSelectors
        ]

data OperatorType
    = OperatorBetween -- ^ BETWEEN
    | OperatorEq      -- ^ EQ
    | OperatorGe      -- ^ GE
    | OperatorLe      -- ^ LE
    | OperatorRefEq   -- ^ REF_EQ
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable OperatorType

instance FromText OperatorType where
    parser = takeText >>= \case
        "BETWEEN" -> pure OperatorBetween
        "EQ"      -> pure OperatorEq
        "GE"      -> pure OperatorGe
        "LE"      -> pure OperatorLe
        "REF_EQ"  -> pure OperatorRefEq
        e         -> fail $
            "Failure parsing OperatorType from " ++ show e

instance ToText OperatorType where
    toText = \case
        OperatorBetween -> "BETWEEN"
        OperatorEq      -> "EQ"
        OperatorGe      -> "GE"
        OperatorLe      -> "LE"
        OperatorRefEq   -> "REF_EQ"

instance ToByteString OperatorType
instance ToHeader     OperatorType
instance ToQuery      OperatorType

instance FromJSON OperatorType where
    parseJSON = parseJSONText "OperatorType"

instance ToJSON OperatorType where
    toJSON = toJSONText

data PipelineIdName = PipelineIdName
    { _pinId   :: Maybe Text
    , _pinName :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'PipelineIdName' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pinId' @::@ 'Maybe' 'Text'
--
-- * 'pinName' @::@ 'Maybe' 'Text'
--
pipelineIdName :: PipelineIdName
pipelineIdName = PipelineIdName
    { _pinId   = Nothing
    , _pinName = Nothing
    }

-- | Identifier of the pipeline that was assigned by AWS Data Pipeline. This
-- is a string of the form df-297EG78HU43EEXAMPLE.
pinId :: Lens' PipelineIdName (Maybe Text)
pinId = lens _pinId (\s a -> s { _pinId = a })

-- | Name of the pipeline.
pinName :: Lens' PipelineIdName (Maybe Text)
pinName = lens _pinName (\s a -> s { _pinName = a })

instance FromJSON PipelineIdName where
    parseJSON = withObject "PipelineIdName" $ \o -> PipelineIdName
        <$> o .:? "id"
        <*> o .:? "name"

instance ToJSON PipelineIdName where
    toJSON PipelineIdName{..} = object
        [ "id"   .= _pinId
        , "name" .= _pinName
        ]

data TaskStatus
    = Failed   -- ^ FAILED
    | False'   -- ^ FALSE
    | Finished -- ^ FINISHED
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable TaskStatus

instance FromText TaskStatus where
    parser = takeText >>= \case
        "FAILED"   -> pure Failed
        "FALSE"    -> pure False'
        "FINISHED" -> pure Finished
        e          -> fail $
            "Failure parsing TaskStatus from " ++ show e

instance ToText TaskStatus where
    toText = \case
        Failed   -> "FAILED"
        False'   -> "FALSE"
        Finished -> "FINISHED"

instance ToByteString TaskStatus
instance ToHeader     TaskStatus
instance ToQuery      TaskStatus

instance FromJSON TaskStatus where
    parseJSON = parseJSONText "TaskStatus"

instance ToJSON TaskStatus where
    toJSON = toJSONText

data ValidationWarning = ValidationWarning
    { _vwId       :: Maybe Text
    , _vwWarnings :: List "errors" Text
    } deriving (Eq, Ord, Show)

-- | 'ValidationWarning' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vwId' @::@ 'Maybe' 'Text'
--
-- * 'vwWarnings' @::@ ['Text']
--
validationWarning :: ValidationWarning
validationWarning = ValidationWarning
    { _vwId       = Nothing
    , _vwWarnings = mempty
    }

-- | The identifier of the object that contains the validation warning.
vwId :: Lens' ValidationWarning (Maybe Text)
vwId = lens _vwId (\s a -> s { _vwId = a })

-- | A description of the validation warning.
vwWarnings :: Lens' ValidationWarning [Text]
vwWarnings = lens _vwWarnings (\s a -> s { _vwWarnings = a }) . _List

instance FromJSON ValidationWarning where
    parseJSON = withObject "ValidationWarning" $ \o -> ValidationWarning
        <$> o .:? "id"
        <*> o .:  "warnings"

instance ToJSON ValidationWarning where
    toJSON ValidationWarning{..} = object
        [ "id"       .= _vwId
        , "warnings" .= _vwWarnings
        ]

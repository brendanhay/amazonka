{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDBStreams.Types
    (
    -- * Service
      DynamoDBStreams

    -- * Errors
    , _ExpiredIteratorException
    , _InternalServerError
    , _TrimmedDataAccessException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * KeyType
    , KeyType (..)

    -- * OperationType
    , OperationType (..)

    -- * ShardIteratorType
    , ShardIteratorType (..)

    -- * StreamStatus
    , StreamStatus (..)

    -- * StreamViewType
    , StreamViewType (..)

    -- * AttributeValue
    , AttributeValue
    , attributeValue
    , avL
    , avM
    , avNS
    , avNULL
    , avN
    , avBS
    , avB
    , avSS
    , avS
    , avBOOL

    -- * KeySchemaElement
    , KeySchemaElement
    , keySchemaElement
    , kseAttributeName
    , kseKeyType

    -- * Record
    , Record
    , record
    , recEventVersion
    , recDynamodb
    , recAwsRegion
    , recEventName
    , recEventSource
    , recEventId

    -- * SequenceNumberRange
    , SequenceNumberRange
    , sequenceNumberRange
    , snrStartingSequenceNumber
    , snrEndingSequenceNumber

    -- * Shard
    , Shard
    , shard
    , shaParentShardId
    , shaSequenceNumberRange
    , shaShardId

    -- * Stream
    , Stream
    , stream
    , strStreamLabel
    , strStreamARN
    , strTableName

    -- * StreamDescription
    , StreamDescription
    , streamDescription
    , sdLastEvaluatedShardId
    , sdStreamLabel
    , sdStreamStatus
    , sdKeySchema
    , sdStreamARN
    , sdStreamViewType
    , sdShards
    , sdTableName
    , sdCreationRequestDateTime

    -- * StreamRecord
    , StreamRecord
    , streamRecord
    , srSequenceNumber
    , srSizeBytes
    , srStreamViewType
    , srKeys
    , srOldImage
    , srNewImage
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2012-08-10@ of the Amazon DynamoDB Streams SDK.
data DynamoDBStreams

instance AWSService DynamoDBStreams where
    type Sg DynamoDBStreams = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "DynamoDBStreams"
            , _svcPrefix = "streams.dynamodb"
            , _svcVersion = "2012-08-10"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | The shard iterator has expired and can no longer be used to retrieve
-- stream records. A shard iterator expires 15 minutes after it is
-- retrieved using the /GetShardIterator/ action.
_ExpiredIteratorException :: AWSError a => Getting (First ServiceError) a ServiceError
_ExpiredIteratorException = _ServiceError . hasCode "ExpiredIteratorException"

-- | An error occurred on the server side.
_InternalServerError :: AWSError a => Getting (First ServiceError) a ServiceError
_InternalServerError = _ServiceError . hasCode "InternalServerError"

-- | The operation attempted to read past the oldest stream record in a
-- shard.
--
-- In DynamoDB Streams, there is a 24 hour limit on data retention. Stream
-- records whose age exceeds this limit are subject to removal (trimming)
-- from the stream. You might receive a TrimmedDataAccessException if:
--
-- -   You request a shard iterator with a sequence number older than the
--     trim point (24 hours).
-- -   You obtain a shard iterator, but before you use the iterator in a
--     /GetRecords/ request, a stream record in the shard exceeds the 24
--     hour period and is trimmed. This causes the iterator to access a
--     record that no longer exists.
_TrimmedDataAccessException :: AWSError a => Getting (First ServiceError) a ServiceError
_TrimmedDataAccessException =
    _ServiceError . hasCode "TrimmedDataAccessException"

-- | The operation tried to access a nonexistent stream.
_ResourceNotFoundException :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
    _ServiceError . hasCode "ResourceNotFoundException"

-- | Your request rate is too high. The AWS SDKs for DynamoDB automatically
-- retry requests that receive this exception. Your request is eventually
-- successful, unless your retry queue is too large to finish. Reduce the
-- frequency of requests and use exponential backoff. For more information,
-- go to
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ErrorHandling.html#APIRetries Error Retries and Exponential Backoff>
-- in the /Amazon DynamoDB Developer Guide/.
_LimitExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _ServiceError . hasCode "LimitExceededException"

data KeyType
    = Hash
    | Range
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText KeyType where
    parser = takeLowerText >>= \case
        "hash" -> pure Hash
        "range" -> pure Range
        e -> fromTextError $ "Failure parsing KeyType from value: '" <> e
           <> "'. Accepted values: hash, range"

instance ToText KeyType where
    toText = \case
        Hash -> "hash"
        Range -> "range"

instance Hashable KeyType
instance ToQuery KeyType
instance ToHeader KeyType

instance FromJSON KeyType where
    parseJSON = parseJSONText "KeyType"

data OperationType
    = Insert
    | Remove
    | Modify
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText OperationType where
    parser = takeLowerText >>= \case
        "insert" -> pure Insert
        "modify" -> pure Modify
        "remove" -> pure Remove
        e -> fromTextError $ "Failure parsing OperationType from value: '" <> e
           <> "'. Accepted values: insert, modify, remove"

instance ToText OperationType where
    toText = \case
        Insert -> "insert"
        Modify -> "modify"
        Remove -> "remove"

instance Hashable OperationType
instance ToQuery OperationType
instance ToHeader OperationType

instance FromJSON OperationType where
    parseJSON = parseJSONText "OperationType"

data ShardIteratorType
    = AfterSequenceNumber
    | AtSequenceNumber
    | TrimHorizon
    | Latest
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ShardIteratorType where
    parser = takeLowerText >>= \case
        "after_sequence_number" -> pure AfterSequenceNumber
        "at_sequence_number" -> pure AtSequenceNumber
        "latest" -> pure Latest
        "trim_horizon" -> pure TrimHorizon
        e -> fromTextError $ "Failure parsing ShardIteratorType from value: '" <> e
           <> "'. Accepted values: after_sequence_number, at_sequence_number, latest, trim_horizon"

instance ToText ShardIteratorType where
    toText = \case
        AfterSequenceNumber -> "after_sequence_number"
        AtSequenceNumber -> "at_sequence_number"
        Latest -> "latest"
        TrimHorizon -> "trim_horizon"

instance Hashable ShardIteratorType
instance ToQuery ShardIteratorType
instance ToHeader ShardIteratorType

instance ToJSON ShardIteratorType where
    toJSON = toJSONText

data StreamStatus
    = Enabled
    | Disabled
    | Disabling
    | Enabling
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StreamStatus where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "disabling" -> pure Disabling
        "enabled" -> pure Enabled
        "enabling" -> pure Enabling
        e -> fromTextError $ "Failure parsing StreamStatus from value: '" <> e
           <> "'. Accepted values: disabled, disabling, enabled, enabling"

instance ToText StreamStatus where
    toText = \case
        Disabled -> "disabled"
        Disabling -> "disabling"
        Enabled -> "enabled"
        Enabling -> "enabling"

instance Hashable StreamStatus
instance ToQuery StreamStatus
instance ToHeader StreamStatus

instance FromJSON StreamStatus where
    parseJSON = parseJSONText "StreamStatus"

data StreamViewType
    = NewAndOldImages
    | KeysOnly
    | OldImage
    | NewImage
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText StreamViewType where
    parser = takeLowerText >>= \case
        "keys_only" -> pure KeysOnly
        "new_and_old_images" -> pure NewAndOldImages
        "new_image" -> pure NewImage
        "old_image" -> pure OldImage
        e -> fromTextError $ "Failure parsing StreamViewType from value: '" <> e
           <> "'. Accepted values: keys_only, new_and_old_images, new_image, old_image"

instance ToText StreamViewType where
    toText = \case
        KeysOnly -> "keys_only"
        NewAndOldImages -> "new_and_old_images"
        NewImage -> "new_image"
        OldImage -> "old_image"

instance Hashable StreamViewType
instance ToQuery StreamViewType
instance ToHeader StreamViewType

instance FromJSON StreamViewType where
    parseJSON = parseJSONText "StreamViewType"

-- | Represents the data for an attribute. You can set one, and only one, of
-- the elements.
--
-- Each attribute in an item is a name-value pair. An attribute can be
-- single-valued or multi-valued set. For example, a book item can have
-- title and authors attributes. Each book has one title but can have many
-- authors. The multi-valued attribute is a set; duplicate values are not
-- allowed.
--
-- /See:/ 'attributeValue' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avL'
--
-- * 'avM'
--
-- * 'avNS'
--
-- * 'avNULL'
--
-- * 'avN'
--
-- * 'avBS'
--
-- * 'avB'
--
-- * 'avSS'
--
-- * 'avS'
--
-- * 'avBOOL'
data AttributeValue = AttributeValue'
    { _avL    :: !(Maybe [AttributeValue])
    , _avM    :: !(Maybe (Map Text AttributeValue))
    , _avNS   :: !(Maybe [Text])
    , _avNULL :: !(Maybe Bool)
    , _avN    :: !(Maybe Text)
    , _avBS   :: !(Maybe [Base64])
    , _avB    :: !(Maybe Base64)
    , _avSS   :: !(Maybe [Text])
    , _avS    :: !(Maybe Text)
    , _avBOOL :: !(Maybe Bool)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'AttributeValue' smart constructor.
attributeValue :: AttributeValue
attributeValue =
    AttributeValue'
    { _avL = Nothing
    , _avM = Nothing
    , _avNS = Nothing
    , _avNULL = Nothing
    , _avN = Nothing
    , _avBS = Nothing
    , _avB = Nothing
    , _avSS = Nothing
    , _avS = Nothing
    , _avBOOL = Nothing
    }

-- | A List data type.
avL :: Lens' AttributeValue [AttributeValue]
avL = lens _avL (\ s a -> s{_avL = a}) . _Default;

-- | A Map data type.
avM :: Lens' AttributeValue (HashMap Text AttributeValue)
avM = lens _avM (\ s a -> s{_avM = a}) . _Default . _Map;

-- | A Number Set data type.
avNS :: Lens' AttributeValue [Text]
avNS = lens _avNS (\ s a -> s{_avNS = a}) . _Default;

-- | A Null data type.
avNULL :: Lens' AttributeValue (Maybe Bool)
avNULL = lens _avNULL (\ s a -> s{_avNULL = a});

-- | A Number data type.
avN :: Lens' AttributeValue (Maybe Text)
avN = lens _avN (\ s a -> s{_avN = a});

-- | A Binary Set data type.
avBS :: Lens' AttributeValue [Base64]
avBS = lens _avBS (\ s a -> s{_avBS = a}) . _Default;

-- | A Binary data type.
avB :: Lens' AttributeValue (Maybe Base64)
avB = lens _avB (\ s a -> s{_avB = a});

-- | A String Set data type.
avSS :: Lens' AttributeValue [Text]
avSS = lens _avSS (\ s a -> s{_avSS = a}) . _Default;

-- | A String data type.
avS :: Lens' AttributeValue (Maybe Text)
avS = lens _avS (\ s a -> s{_avS = a});

-- | A Boolean data type.
avBOOL :: Lens' AttributeValue (Maybe Bool)
avBOOL = lens _avBOOL (\ s a -> s{_avBOOL = a});

instance FromJSON AttributeValue where
        parseJSON
          = withObject "AttributeValue"
              (\ x ->
                 AttributeValue' <$>
                   (x .:? "L" .!= mempty) <*> (x .:? "M" .!= mempty) <*>
                     (x .:? "NS" .!= mempty)
                     <*> (x .:? "NULL")
                     <*> (x .:? "N")
                     <*> (x .:? "BS" .!= mempty)
                     <*> (x .:? "B")
                     <*> (x .:? "SS" .!= mempty)
                     <*> (x .:? "S")
                     <*> (x .:? "BOOL"))

-- | Represents /a single element/ of a key schema. A key schema specifies
-- the attributes that make up the primary key of a table, or the key
-- attributes of an index.
--
-- A /KeySchemaElement/ represents exactly one attribute of the primary
-- key. For example, a hash type primary key would be represented by one
-- /KeySchemaElement/. A hash-and-range type primary key would require one
-- /KeySchemaElement/ for the hash attribute, and another
-- /KeySchemaElement/ for the range attribute.
--
-- /See:/ 'keySchemaElement' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kseAttributeName'
--
-- * 'kseKeyType'
data KeySchemaElement = KeySchemaElement'
    { _kseAttributeName :: !Text
    , _kseKeyType       :: !KeyType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'KeySchemaElement' smart constructor.
keySchemaElement :: Text -> KeyType -> KeySchemaElement
keySchemaElement pAttributeName pKeyType =
    KeySchemaElement'
    { _kseAttributeName = pAttributeName
    , _kseKeyType = pKeyType
    }

-- | The name of a key attribute.
kseAttributeName :: Lens' KeySchemaElement Text
kseAttributeName = lens _kseAttributeName (\ s a -> s{_kseAttributeName = a});

-- | The attribute data, consisting of the data type and the attribute value
-- itself.
kseKeyType :: Lens' KeySchemaElement KeyType
kseKeyType = lens _kseKeyType (\ s a -> s{_kseKeyType = a});

instance FromJSON KeySchemaElement where
        parseJSON
          = withObject "KeySchemaElement"
              (\ x ->
                 KeySchemaElement' <$>
                   (x .: "AttributeName") <*> (x .: "KeyType"))

-- | A description of a unique event within a stream.
--
-- /See:/ 'record' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'recEventVersion'
--
-- * 'recDynamodb'
--
-- * 'recAwsRegion'
--
-- * 'recEventName'
--
-- * 'recEventSource'
--
-- * 'recEventId'
data Record = Record'
    { _recEventVersion :: !(Maybe Text)
    , _recDynamodb     :: !(Maybe StreamRecord)
    , _recAwsRegion    :: !(Maybe Text)
    , _recEventName    :: !(Maybe OperationType)
    , _recEventSource  :: !(Maybe Text)
    , _recEventId      :: !(Maybe Text)
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'Record' smart constructor.
record :: Record
record =
    Record'
    { _recEventVersion = Nothing
    , _recDynamodb = Nothing
    , _recAwsRegion = Nothing
    , _recEventName = Nothing
    , _recEventSource = Nothing
    , _recEventId = Nothing
    }

-- | The version number of the stream record format. Currently, this is
-- /1.0/.
recEventVersion :: Lens' Record (Maybe Text)
recEventVersion = lens _recEventVersion (\ s a -> s{_recEventVersion = a});

-- | The main body of the stream record, containing all of the
-- DynamoDB-specific fields.
recDynamodb :: Lens' Record (Maybe StreamRecord)
recDynamodb = lens _recDynamodb (\ s a -> s{_recDynamodb = a});

-- | The region in which the /GetRecords/ request was received.
recAwsRegion :: Lens' Record (Maybe Text)
recAwsRegion = lens _recAwsRegion (\ s a -> s{_recAwsRegion = a});

-- | The type of data modification that was performed on the DynamoDB table:
--
-- -   @INSERT@ - a new item was added to the table.
--
-- -   @MODIFY@ - one or more of the item\'s attributes were updated.
--
-- -   @REMOVE@ - the item was deleted from the table
--
recEventName :: Lens' Record (Maybe OperationType)
recEventName = lens _recEventName (\ s a -> s{_recEventName = a});

-- | The AWS service from which the stream record originated. For DynamoDB
-- Streams, this is /aws:dynamodb/.
recEventSource :: Lens' Record (Maybe Text)
recEventSource = lens _recEventSource (\ s a -> s{_recEventSource = a});

-- | A globally unique identifier for the event that was recorded in this
-- stream record.
recEventId :: Lens' Record (Maybe Text)
recEventId = lens _recEventId (\ s a -> s{_recEventId = a});

instance FromJSON Record where
        parseJSON
          = withObject "Record"
              (\ x ->
                 Record' <$>
                   (x .:? "eventVersion") <*> (x .:? "dynamodb") <*>
                     (x .:? "awsRegion")
                     <*> (x .:? "eventName")
                     <*> (x .:? "eventSource")
                     <*> (x .:? "eventID"))

-- | The beginning and ending sequence numbers for the stream records
-- contained within a shard.
--
-- /See:/ 'sequenceNumberRange' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'snrStartingSequenceNumber'
--
-- * 'snrEndingSequenceNumber'
data SequenceNumberRange = SequenceNumberRange'
    { _snrStartingSequenceNumber :: !(Maybe Text)
    , _snrEndingSequenceNumber   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SequenceNumberRange' smart constructor.
sequenceNumberRange :: SequenceNumberRange
sequenceNumberRange =
    SequenceNumberRange'
    { _snrStartingSequenceNumber = Nothing
    , _snrEndingSequenceNumber = Nothing
    }

-- | The first sequence number.
snrStartingSequenceNumber :: Lens' SequenceNumberRange (Maybe Text)
snrStartingSequenceNumber = lens _snrStartingSequenceNumber (\ s a -> s{_snrStartingSequenceNumber = a});

-- | The last sequence number.
snrEndingSequenceNumber :: Lens' SequenceNumberRange (Maybe Text)
snrEndingSequenceNumber = lens _snrEndingSequenceNumber (\ s a -> s{_snrEndingSequenceNumber = a});

instance FromJSON SequenceNumberRange where
        parseJSON
          = withObject "SequenceNumberRange"
              (\ x ->
                 SequenceNumberRange' <$>
                   (x .:? "StartingSequenceNumber") <*>
                     (x .:? "EndingSequenceNumber"))

-- | A uniquely identified group of stream records within a stream.
--
-- /See:/ 'shard' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'shaParentShardId'
--
-- * 'shaSequenceNumberRange'
--
-- * 'shaShardId'
data Shard = Shard'
    { _shaParentShardId       :: !(Maybe Text)
    , _shaSequenceNumberRange :: !(Maybe SequenceNumberRange)
    , _shaShardId             :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Shard' smart constructor.
shard :: Shard
shard =
    Shard'
    { _shaParentShardId = Nothing
    , _shaSequenceNumberRange = Nothing
    , _shaShardId = Nothing
    }

-- | The shard ID of the current shard\'s parent.
shaParentShardId :: Lens' Shard (Maybe Text)
shaParentShardId = lens _shaParentShardId (\ s a -> s{_shaParentShardId = a});

-- | The range of possible sequence numbers for the shard.
shaSequenceNumberRange :: Lens' Shard (Maybe SequenceNumberRange)
shaSequenceNumberRange = lens _shaSequenceNumberRange (\ s a -> s{_shaSequenceNumberRange = a});

-- | The system-generated identifier for this shard.
shaShardId :: Lens' Shard (Maybe Text)
shaShardId = lens _shaShardId (\ s a -> s{_shaShardId = a});

instance FromJSON Shard where
        parseJSON
          = withObject "Shard"
              (\ x ->
                 Shard' <$>
                   (x .:? "ParentShardId") <*>
                     (x .:? "SequenceNumberRange")
                     <*> (x .:? "ShardId"))

-- | Represents all of the data describing a particular stream.
--
-- /See:/ 'stream' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'strStreamLabel'
--
-- * 'strStreamARN'
--
-- * 'strTableName'
data Stream = Stream'
    { _strStreamLabel :: !(Maybe Text)
    , _strStreamARN   :: !(Maybe Text)
    , _strTableName   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Stream' smart constructor.
stream :: Stream
stream =
    Stream'
    { _strStreamLabel = Nothing
    , _strStreamARN = Nothing
    , _strTableName = Nothing
    }

-- | A timestamp, in ISO 8601 format, for this stream.
--
-- Note that /LatestStreamLabel/ is not a unique identifier for the stream,
-- because it is possible that a stream from another table might have the
-- same timestamp. However, the combination of the following three elements
-- is guaranteed to be unique:
--
-- -   the AWS customer ID.
--
-- -   the table name
--
-- -   the /StreamLabel/
--
strStreamLabel :: Lens' Stream (Maybe Text)
strStreamLabel = lens _strStreamLabel (\ s a -> s{_strStreamLabel = a});

-- | The Amazon Resource Name (ARN) for the stream.
strStreamARN :: Lens' Stream (Maybe Text)
strStreamARN = lens _strStreamARN (\ s a -> s{_strStreamARN = a});

-- | The DynamoDB table with which the stream is associated.
strTableName :: Lens' Stream (Maybe Text)
strTableName = lens _strTableName (\ s a -> s{_strTableName = a});

instance FromJSON Stream where
        parseJSON
          = withObject "Stream"
              (\ x ->
                 Stream' <$>
                   (x .:? "StreamLabel") <*> (x .:? "StreamArn") <*>
                     (x .:? "TableName"))

-- | Represents all of the data describing a particular stream.
--
-- /See:/ 'streamDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdLastEvaluatedShardId'
--
-- * 'sdStreamLabel'
--
-- * 'sdStreamStatus'
--
-- * 'sdKeySchema'
--
-- * 'sdStreamARN'
--
-- * 'sdStreamViewType'
--
-- * 'sdShards'
--
-- * 'sdTableName'
--
-- * 'sdCreationRequestDateTime'
data StreamDescription = StreamDescription'
    { _sdLastEvaluatedShardId    :: !(Maybe Text)
    , _sdStreamLabel             :: !(Maybe Text)
    , _sdStreamStatus            :: !(Maybe StreamStatus)
    , _sdKeySchema               :: !(Maybe (List1 KeySchemaElement))
    , _sdStreamARN               :: !(Maybe Text)
    , _sdStreamViewType          :: !(Maybe StreamViewType)
    , _sdShards                  :: !(Maybe [Shard])
    , _sdTableName               :: !(Maybe Text)
    , _sdCreationRequestDateTime :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StreamDescription' smart constructor.
streamDescription :: StreamDescription
streamDescription =
    StreamDescription'
    { _sdLastEvaluatedShardId = Nothing
    , _sdStreamLabel = Nothing
    , _sdStreamStatus = Nothing
    , _sdKeySchema = Nothing
    , _sdStreamARN = Nothing
    , _sdStreamViewType = Nothing
    , _sdShards = Nothing
    , _sdTableName = Nothing
    , _sdCreationRequestDateTime = Nothing
    }

-- | The shard ID of the item where the operation stopped, inclusive of the
-- previous result set. Use this value to start a new operation, excluding
-- this value in the new request.
--
-- If @LastEvaluatedShardId@ is empty, then the \"last page\" of results
-- has been processed and there is currently no more data to be retrieved.
--
-- If @LastEvaluatedShardId@ is not empty, it does not necessarily mean
-- that there is more data in the result set. The only way to know when you
-- have reached the end of the result set is when @LastEvaluatedShardId@ is
-- empty.
sdLastEvaluatedShardId :: Lens' StreamDescription (Maybe Text)
sdLastEvaluatedShardId = lens _sdLastEvaluatedShardId (\ s a -> s{_sdLastEvaluatedShardId = a});

-- | A timestamp, in ISO 8601 format, for this stream.
--
-- Note that /LatestStreamLabel/ is not a unique identifier for the stream,
-- because it is possible that a stream from another table might have the
-- same timestamp. However, the combination of the following three elements
-- is guaranteed to be unique:
--
-- -   the AWS customer ID.
--
-- -   the table name
--
-- -   the /StreamLabel/
--
sdStreamLabel :: Lens' StreamDescription (Maybe Text)
sdStreamLabel = lens _sdStreamLabel (\ s a -> s{_sdStreamLabel = a});

-- | Indicates the current status of the stream:
--
-- -   @ENABLING@ - Streams is currently being enabled on the DynamoDB
--     table.
--
-- -   @ENABLING@ - the stream is enabled.
--
-- -   @DISABLING@ - Streams is currently being disabled on the DynamoDB
--     table.
--
-- -   @DISABLED@ - the stream is disabled.
--
sdStreamStatus :: Lens' StreamDescription (Maybe StreamStatus)
sdStreamStatus = lens _sdStreamStatus (\ s a -> s{_sdStreamStatus = a});

-- | The key attribute(s) of the stream\'s DynamoDB table.
sdKeySchema :: Lens' StreamDescription (Maybe (NonEmpty KeySchemaElement))
sdKeySchema = lens _sdKeySchema (\ s a -> s{_sdKeySchema = a}) . mapping _List1;

-- | The Amazon Resource Name (ARN) for the stream.
sdStreamARN :: Lens' StreamDescription (Maybe Text)
sdStreamARN = lens _sdStreamARN (\ s a -> s{_sdStreamARN = a});

-- | Indicates the format of the records within this stream:
--
-- -   @KEYS_ONLY@ - only the key attributes of items that were modified in
--     the DynamoDB table.
--
-- -   @NEW_IMAGE@ - entire item from the table, as it appeared after they
--     were modified.
--
-- -   @OLD_IMAGE@ - entire item from the table, as it appeared before they
--     were modified.
--
-- -   @NEW_AND_OLD_IMAGES@ - both the new and the old images of the items
--     from the table.
--
sdStreamViewType :: Lens' StreamDescription (Maybe StreamViewType)
sdStreamViewType = lens _sdStreamViewType (\ s a -> s{_sdStreamViewType = a});

-- | The shards that comprise the stream.
sdShards :: Lens' StreamDescription [Shard]
sdShards = lens _sdShards (\ s a -> s{_sdShards = a}) . _Default;

-- | The DynamoDB table with which the stream is associated.
sdTableName :: Lens' StreamDescription (Maybe Text)
sdTableName = lens _sdTableName (\ s a -> s{_sdTableName = a});

-- | The date and time when the request to create this stream was issued.
sdCreationRequestDateTime :: Lens' StreamDescription (Maybe UTCTime)
sdCreationRequestDateTime = lens _sdCreationRequestDateTime (\ s a -> s{_sdCreationRequestDateTime = a}) . mapping _Time;

instance FromJSON StreamDescription where
        parseJSON
          = withObject "StreamDescription"
              (\ x ->
                 StreamDescription' <$>
                   (x .:? "LastEvaluatedShardId") <*>
                     (x .:? "StreamLabel")
                     <*> (x .:? "StreamStatus")
                     <*> (x .:? "KeySchema")
                     <*> (x .:? "StreamArn")
                     <*> (x .:? "StreamViewType")
                     <*> (x .:? "Shards" .!= mempty)
                     <*> (x .:? "TableName")
                     <*> (x .:? "CreationRequestDateTime"))

-- | A description of a single data modification that was performed on an
-- item in a DynamoDB table.
--
-- /See:/ 'streamRecord' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srSequenceNumber'
--
-- * 'srSizeBytes'
--
-- * 'srStreamViewType'
--
-- * 'srKeys'
--
-- * 'srOldImage'
--
-- * 'srNewImage'
data StreamRecord = StreamRecord'
    { _srSequenceNumber :: !(Maybe Text)
    , _srSizeBytes      :: !(Maybe Nat)
    , _srStreamViewType :: !(Maybe StreamViewType)
    , _srKeys           :: !(Maybe (Map Text AttributeValue))
    , _srOldImage       :: !(Maybe (Map Text AttributeValue))
    , _srNewImage       :: !(Maybe (Map Text AttributeValue))
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'StreamRecord' smart constructor.
streamRecord :: StreamRecord
streamRecord =
    StreamRecord'
    { _srSequenceNumber = Nothing
    , _srSizeBytes = Nothing
    , _srStreamViewType = Nothing
    , _srKeys = Nothing
    , _srOldImage = Nothing
    , _srNewImage = Nothing
    }

-- | The sequence number of the stream record.
srSequenceNumber :: Lens' StreamRecord (Maybe Text)
srSequenceNumber = lens _srSequenceNumber (\ s a -> s{_srSequenceNumber = a});

-- | The size of the stream record, in bytes.
srSizeBytes :: Lens' StreamRecord (Maybe Natural)
srSizeBytes = lens _srSizeBytes (\ s a -> s{_srSizeBytes = a}) . mapping _Nat;

-- | The type of data from the modified DynamoDB item that was captured in
-- this stream record:
--
-- -   @KEYS_ONLY@ - only the key attributes of the modified item.
--
-- -   @NEW_IMAGE@ - the entire item, as it appears after it was modified.
--
-- -   @OLD_IMAGE@ - the entire item, as it appeared before it was
--     modified.
--
-- -   @NEW_AND_OLD_IMAGES@ — both the new and the old item images of the
--     item.
--
srStreamViewType :: Lens' StreamRecord (Maybe StreamViewType)
srStreamViewType = lens _srStreamViewType (\ s a -> s{_srStreamViewType = a});

-- | The primary key attribute(s) for the DynamoDB item that was modified.
srKeys :: Lens' StreamRecord (HashMap Text AttributeValue)
srKeys = lens _srKeys (\ s a -> s{_srKeys = a}) . _Default . _Map;

-- | The item in the DynamoDB table as it appeared before it was modified.
srOldImage :: Lens' StreamRecord (HashMap Text AttributeValue)
srOldImage = lens _srOldImage (\ s a -> s{_srOldImage = a}) . _Default . _Map;

-- | The item in the DynamoDB table as it appeared after it was modified.
srNewImage :: Lens' StreamRecord (HashMap Text AttributeValue)
srNewImage = lens _srNewImage (\ s a -> s{_srNewImage = a}) . _Default . _Map;

instance FromJSON StreamRecord where
        parseJSON
          = withObject "StreamRecord"
              (\ x ->
                 StreamRecord' <$>
                   (x .:? "SequenceNumber") <*> (x .:? "SizeBytes") <*>
                     (x .:? "StreamViewType")
                     <*> (x .:? "Keys" .!= mempty)
                     <*> (x .:? "OldImage" .!= mempty)
                     <*> (x .:? "NewImage" .!= mempty))

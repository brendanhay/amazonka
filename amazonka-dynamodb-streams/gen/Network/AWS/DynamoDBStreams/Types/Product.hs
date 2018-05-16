{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDBStreams.Types.Product where

import Network.AWS.DynamoDBStreams.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the data for an attribute. You can set one, and only one, of the elements.
--
--
-- Each attribute in an item is a name-value pair. An attribute can be single-valued or multi-valued set. For example, a book item can have title and authors attributes. Each book has one title but can have many authors. The multi-valued attribute is a set; duplicate values are not allowed.
--
--
-- /See:/ 'attributeValue' smart constructor.
data AttributeValue = AttributeValue'
  { _avL    :: !(Maybe [AttributeValue])
  , _avNS   :: !(Maybe [Text])
  , _avM    :: !(Maybe (Map Text AttributeValue))
  , _avNULL :: !(Maybe Bool)
  , _avN    :: !(Maybe Text)
  , _avBS   :: !(Maybe [Base64])
  , _avB    :: !(Maybe Base64)
  , _avSS   :: !(Maybe [Text])
  , _avS    :: !(Maybe Text)
  , _avBOOL :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttributeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avL' - A List data type.
--
-- * 'avNS' - A Number Set data type.
--
-- * 'avM' - A Map data type.
--
-- * 'avNULL' - A Null data type.
--
-- * 'avN' - A Number data type.
--
-- * 'avBS' - A Binary Set data type.
--
-- * 'avB' - A Binary data type.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'avSS' - A String Set data type.
--
-- * 'avS' - A String data type.
--
-- * 'avBOOL' - A Boolean data type.
attributeValue
    :: AttributeValue
attributeValue =
  AttributeValue'
    { _avL = Nothing
    , _avNS = Nothing
    , _avM = Nothing
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
avL = lens _avL (\ s a -> s{_avL = a}) . _Default . _Coerce

-- | A Number Set data type.
avNS :: Lens' AttributeValue [Text]
avNS = lens _avNS (\ s a -> s{_avNS = a}) . _Default . _Coerce

-- | A Map data type.
avM :: Lens' AttributeValue (HashMap Text AttributeValue)
avM = lens _avM (\ s a -> s{_avM = a}) . _Default . _Map

-- | A Null data type.
avNULL :: Lens' AttributeValue (Maybe Bool)
avNULL = lens _avNULL (\ s a -> s{_avNULL = a})

-- | A Number data type.
avN :: Lens' AttributeValue (Maybe Text)
avN = lens _avN (\ s a -> s{_avN = a})

-- | A Binary Set data type.
avBS :: Lens' AttributeValue [ByteString]
avBS = lens _avBS (\ s a -> s{_avBS = a}) . _Default . _Coerce

-- | A Binary data type.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
avB :: Lens' AttributeValue (Maybe ByteString)
avB = lens _avB (\ s a -> s{_avB = a}) . mapping _Base64

-- | A String Set data type.
avSS :: Lens' AttributeValue [Text]
avSS = lens _avSS (\ s a -> s{_avSS = a}) . _Default . _Coerce

-- | A String data type.
avS :: Lens' AttributeValue (Maybe Text)
avS = lens _avS (\ s a -> s{_avS = a})

-- | A Boolean data type.
avBOOL :: Lens' AttributeValue (Maybe Bool)
avBOOL = lens _avBOOL (\ s a -> s{_avBOOL = a})

instance FromJSON AttributeValue where
        parseJSON
          = withObject "AttributeValue"
              (\ x ->
                 AttributeValue' <$>
                   (x .:? "L" .!= mempty) <*> (x .:? "NS" .!= mempty)
                     <*> (x .:? "M" .!= mempty)
                     <*> (x .:? "NULL")
                     <*> (x .:? "N")
                     <*> (x .:? "BS" .!= mempty)
                     <*> (x .:? "B")
                     <*> (x .:? "SS" .!= mempty)
                     <*> (x .:? "S")
                     <*> (x .:? "BOOL"))

instance Hashable AttributeValue where

instance NFData AttributeValue where

-- | Contains details about the type of identity that made the request.
--
--
--
-- /See:/ 'identity' smart constructor.
data Identity = Identity'
  { _iPrincipalId :: !(Maybe Text)
  , _iType        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Identity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iPrincipalId' - A unique identifier for the entity that made the call. For Time To Live, the principalId is "dynamodb.amazonaws.com".
--
-- * 'iType' - The type of the identity. For Time To Live, the type is "Service".
identity
    :: Identity
identity = Identity' {_iPrincipalId = Nothing, _iType = Nothing}


-- | A unique identifier for the entity that made the call. For Time To Live, the principalId is "dynamodb.amazonaws.com".
iPrincipalId :: Lens' Identity (Maybe Text)
iPrincipalId = lens _iPrincipalId (\ s a -> s{_iPrincipalId = a})

-- | The type of the identity. For Time To Live, the type is "Service".
iType :: Lens' Identity (Maybe Text)
iType = lens _iType (\ s a -> s{_iType = a})

instance FromJSON Identity where
        parseJSON
          = withObject "Identity"
              (\ x ->
                 Identity' <$>
                   (x .:? "PrincipalId") <*> (x .:? "Type"))

instance Hashable Identity where

instance NFData Identity where

-- | Represents /a single element/ of a key schema. A key schema specifies the attributes that make up the primary key of a table, or the key attributes of an index.
--
--
-- A @KeySchemaElement@ represents exactly one attribute of the primary key. For example, a simple primary key (partition key) would be represented by one @KeySchemaElement@ . A composite primary key (partition key and sort key) would require one @KeySchemaElement@ for the partition key, and another @KeySchemaElement@ for the sort key.
--
--
-- /See:/ 'keySchemaElement' smart constructor.
data KeySchemaElement = KeySchemaElement'
  { _kseAttributeName :: !Text
  , _kseKeyType       :: !KeyType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KeySchemaElement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kseAttributeName' - The name of a key attribute.
--
-- * 'kseKeyType' - The attribute data, consisting of the data type and the attribute value itself.
keySchemaElement
    :: Text -- ^ 'kseAttributeName'
    -> KeyType -- ^ 'kseKeyType'
    -> KeySchemaElement
keySchemaElement pAttributeName_ pKeyType_ =
  KeySchemaElement'
    {_kseAttributeName = pAttributeName_, _kseKeyType = pKeyType_}


-- | The name of a key attribute.
kseAttributeName :: Lens' KeySchemaElement Text
kseAttributeName = lens _kseAttributeName (\ s a -> s{_kseAttributeName = a})

-- | The attribute data, consisting of the data type and the attribute value itself.
kseKeyType :: Lens' KeySchemaElement KeyType
kseKeyType = lens _kseKeyType (\ s a -> s{_kseKeyType = a})

instance FromJSON KeySchemaElement where
        parseJSON
          = withObject "KeySchemaElement"
              (\ x ->
                 KeySchemaElement' <$>
                   (x .: "AttributeName") <*> (x .: "KeyType"))

instance Hashable KeySchemaElement where

instance NFData KeySchemaElement where

-- | A description of a unique event within a stream.
--
--
--
-- /See:/ 'record' smart constructor.
data Record = Record'
  { _rUserIdentity :: !(Maybe Identity)
  , _rEventVersion :: !(Maybe Text)
  , _rDynamodb     :: !(Maybe StreamRecord)
  , _rAwsRegion    :: !(Maybe Text)
  , _rEventName    :: !(Maybe OperationType)
  , _rEventSource  :: !(Maybe Text)
  , _rEventId      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Record' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rUserIdentity' - Items that are deleted by the Time to Live process after expiration have the following fields:      * Records[].userIdentity.type "Service"     * Records[].userIdentity.principalId "dynamodb.amazonaws.com"
--
-- * 'rEventVersion' - The version number of the stream record format. This number is updated whenever the structure of @Record@ is modified. Client applications must not assume that @eventVersion@ will remain at a particular value, as this number is subject to change at any time. In general, @eventVersion@ will only increase as the low-level DynamoDB Streams API evolves.
--
-- * 'rDynamodb' - The main body of the stream record, containing all of the DynamoDB-specific fields.
--
-- * 'rAwsRegion' - The region in which the @GetRecords@ request was received.
--
-- * 'rEventName' - The type of data modification that was performed on the DynamoDB table:     * @INSERT@ - a new item was added to the table.     * @MODIFY@ - one or more of an existing item's attributes were modified.     * @REMOVE@ - the item was deleted from the table
--
-- * 'rEventSource' - The AWS service from which the stream record originated. For DynamoDB Streams, this is @aws:dynamodb@ .
--
-- * 'rEventId' - A globally unique identifier for the event that was recorded in this stream record.
record
    :: Record
record =
  Record'
    { _rUserIdentity = Nothing
    , _rEventVersion = Nothing
    , _rDynamodb = Nothing
    , _rAwsRegion = Nothing
    , _rEventName = Nothing
    , _rEventSource = Nothing
    , _rEventId = Nothing
    }


-- | Items that are deleted by the Time to Live process after expiration have the following fields:      * Records[].userIdentity.type "Service"     * Records[].userIdentity.principalId "dynamodb.amazonaws.com"
rUserIdentity :: Lens' Record (Maybe Identity)
rUserIdentity = lens _rUserIdentity (\ s a -> s{_rUserIdentity = a})

-- | The version number of the stream record format. This number is updated whenever the structure of @Record@ is modified. Client applications must not assume that @eventVersion@ will remain at a particular value, as this number is subject to change at any time. In general, @eventVersion@ will only increase as the low-level DynamoDB Streams API evolves.
rEventVersion :: Lens' Record (Maybe Text)
rEventVersion = lens _rEventVersion (\ s a -> s{_rEventVersion = a})

-- | The main body of the stream record, containing all of the DynamoDB-specific fields.
rDynamodb :: Lens' Record (Maybe StreamRecord)
rDynamodb = lens _rDynamodb (\ s a -> s{_rDynamodb = a})

-- | The region in which the @GetRecords@ request was received.
rAwsRegion :: Lens' Record (Maybe Text)
rAwsRegion = lens _rAwsRegion (\ s a -> s{_rAwsRegion = a})

-- | The type of data modification that was performed on the DynamoDB table:     * @INSERT@ - a new item was added to the table.     * @MODIFY@ - one or more of an existing item's attributes were modified.     * @REMOVE@ - the item was deleted from the table
rEventName :: Lens' Record (Maybe OperationType)
rEventName = lens _rEventName (\ s a -> s{_rEventName = a})

-- | The AWS service from which the stream record originated. For DynamoDB Streams, this is @aws:dynamodb@ .
rEventSource :: Lens' Record (Maybe Text)
rEventSource = lens _rEventSource (\ s a -> s{_rEventSource = a})

-- | A globally unique identifier for the event that was recorded in this stream record.
rEventId :: Lens' Record (Maybe Text)
rEventId = lens _rEventId (\ s a -> s{_rEventId = a})

instance FromJSON Record where
        parseJSON
          = withObject "Record"
              (\ x ->
                 Record' <$>
                   (x .:? "userIdentity") <*> (x .:? "eventVersion") <*>
                     (x .:? "dynamodb")
                     <*> (x .:? "awsRegion")
                     <*> (x .:? "eventName")
                     <*> (x .:? "eventSource")
                     <*> (x .:? "eventID"))

instance Hashable Record where

instance NFData Record where

-- | The beginning and ending sequence numbers for the stream records contained within a shard.
--
--
--
-- /See:/ 'sequenceNumberRange' smart constructor.
data SequenceNumberRange = SequenceNumberRange'
  { _snrStartingSequenceNumber :: !(Maybe Text)
  , _snrEndingSequenceNumber   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SequenceNumberRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'snrStartingSequenceNumber' - The first sequence number.
--
-- * 'snrEndingSequenceNumber' - The last sequence number.
sequenceNumberRange
    :: SequenceNumberRange
sequenceNumberRange =
  SequenceNumberRange'
    {_snrStartingSequenceNumber = Nothing, _snrEndingSequenceNumber = Nothing}


-- | The first sequence number.
snrStartingSequenceNumber :: Lens' SequenceNumberRange (Maybe Text)
snrStartingSequenceNumber = lens _snrStartingSequenceNumber (\ s a -> s{_snrStartingSequenceNumber = a})

-- | The last sequence number.
snrEndingSequenceNumber :: Lens' SequenceNumberRange (Maybe Text)
snrEndingSequenceNumber = lens _snrEndingSequenceNumber (\ s a -> s{_snrEndingSequenceNumber = a})

instance FromJSON SequenceNumberRange where
        parseJSON
          = withObject "SequenceNumberRange"
              (\ x ->
                 SequenceNumberRange' <$>
                   (x .:? "StartingSequenceNumber") <*>
                     (x .:? "EndingSequenceNumber"))

instance Hashable SequenceNumberRange where

instance NFData SequenceNumberRange where

-- | A uniquely identified group of stream records within a stream.
--
--
--
-- /See:/ 'shard' smart constructor.
data Shard = Shard'
  { _sParentShardId       :: !(Maybe Text)
  , _sSequenceNumberRange :: !(Maybe SequenceNumberRange)
  , _sShardId             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Shard' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sParentShardId' - The shard ID of the current shard's parent.
--
-- * 'sSequenceNumberRange' - The range of possible sequence numbers for the shard.
--
-- * 'sShardId' - The system-generated identifier for this shard.
shard
    :: Shard
shard =
  Shard'
    { _sParentShardId = Nothing
    , _sSequenceNumberRange = Nothing
    , _sShardId = Nothing
    }


-- | The shard ID of the current shard's parent.
sParentShardId :: Lens' Shard (Maybe Text)
sParentShardId = lens _sParentShardId (\ s a -> s{_sParentShardId = a})

-- | The range of possible sequence numbers for the shard.
sSequenceNumberRange :: Lens' Shard (Maybe SequenceNumberRange)
sSequenceNumberRange = lens _sSequenceNumberRange (\ s a -> s{_sSequenceNumberRange = a})

-- | The system-generated identifier for this shard.
sShardId :: Lens' Shard (Maybe Text)
sShardId = lens _sShardId (\ s a -> s{_sShardId = a})

instance FromJSON Shard where
        parseJSON
          = withObject "Shard"
              (\ x ->
                 Shard' <$>
                   (x .:? "ParentShardId") <*>
                     (x .:? "SequenceNumberRange")
                     <*> (x .:? "ShardId"))

instance Hashable Shard where

instance NFData Shard where

-- | Represents all of the data describing a particular stream.
--
--
--
-- /See:/ 'stream' smart constructor.
data Stream = Stream'
  { _sStreamLabel :: !(Maybe Text)
  , _sStreamARN   :: !(Maybe Text)
  , _sTableName   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Stream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sStreamLabel' - A timestamp, in ISO 8601 format, for this stream. Note that @LatestStreamLabel@ is not a unique identifier for the stream, because it is possible that a stream from another table might have the same timestamp. However, the combination of the following three elements is guaranteed to be unique:     * the AWS customer ID.     * the table name     * the @StreamLabel@
--
-- * 'sStreamARN' - The Amazon Resource Name (ARN) for the stream.
--
-- * 'sTableName' - The DynamoDB table with which the stream is associated.
stream
    :: Stream
stream =
  Stream'
    {_sStreamLabel = Nothing, _sStreamARN = Nothing, _sTableName = Nothing}


-- | A timestamp, in ISO 8601 format, for this stream. Note that @LatestStreamLabel@ is not a unique identifier for the stream, because it is possible that a stream from another table might have the same timestamp. However, the combination of the following three elements is guaranteed to be unique:     * the AWS customer ID.     * the table name     * the @StreamLabel@
sStreamLabel :: Lens' Stream (Maybe Text)
sStreamLabel = lens _sStreamLabel (\ s a -> s{_sStreamLabel = a})

-- | The Amazon Resource Name (ARN) for the stream.
sStreamARN :: Lens' Stream (Maybe Text)
sStreamARN = lens _sStreamARN (\ s a -> s{_sStreamARN = a})

-- | The DynamoDB table with which the stream is associated.
sTableName :: Lens' Stream (Maybe Text)
sTableName = lens _sTableName (\ s a -> s{_sTableName = a})

instance FromJSON Stream where
        parseJSON
          = withObject "Stream"
              (\ x ->
                 Stream' <$>
                   (x .:? "StreamLabel") <*> (x .:? "StreamArn") <*>
                     (x .:? "TableName"))

instance Hashable Stream where

instance NFData Stream where

-- | Represents all of the data describing a particular stream.
--
--
--
-- /See:/ 'streamDescription' smart constructor.
data StreamDescription = StreamDescription'
  { _sdLastEvaluatedShardId    :: !(Maybe Text)
  , _sdStreamLabel             :: !(Maybe Text)
  , _sdStreamStatus            :: !(Maybe StreamStatus)
  , _sdKeySchema               :: !(Maybe (List1 KeySchemaElement))
  , _sdStreamViewType          :: !(Maybe StreamViewType)
  , _sdStreamARN               :: !(Maybe Text)
  , _sdShards                  :: !(Maybe [Shard])
  , _sdTableName               :: !(Maybe Text)
  , _sdCreationRequestDateTime :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StreamDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdLastEvaluatedShardId' - The shard ID of the item where the operation stopped, inclusive of the previous result set. Use this value to start a new operation, excluding this value in the new request. If @LastEvaluatedShardId@ is empty, then the "last page" of results has been processed and there is currently no more data to be retrieved. If @LastEvaluatedShardId@ is not empty, it does not necessarily mean that there is more data in the result set. The only way to know when you have reached the end of the result set is when @LastEvaluatedShardId@ is empty.
--
-- * 'sdStreamLabel' - A timestamp, in ISO 8601 format, for this stream. Note that @LatestStreamLabel@ is not a unique identifier for the stream, because it is possible that a stream from another table might have the same timestamp. However, the combination of the following three elements is guaranteed to be unique:     * the AWS customer ID.     * the table name     * the @StreamLabel@
--
-- * 'sdStreamStatus' - Indicates the current status of the stream:     * @ENABLING@ - Streams is currently being enabled on the DynamoDB table.     * @ENABLED@ - the stream is enabled.     * @DISABLING@ - Streams is currently being disabled on the DynamoDB table.     * @DISABLED@ - the stream is disabled.
--
-- * 'sdKeySchema' - The key attribute(s) of the stream's DynamoDB table.
--
-- * 'sdStreamViewType' - Indicates the format of the records within this stream:     * @KEYS_ONLY@ - only the key attributes of items that were modified in the DynamoDB table.     * @NEW_IMAGE@ - entire items from the table, as they appeared after they were modified.     * @OLD_IMAGE@ - entire items from the table, as they appeared before they were modified.     * @NEW_AND_OLD_IMAGES@ - both the new and the old images of the items from the table.
--
-- * 'sdStreamARN' - The Amazon Resource Name (ARN) for the stream.
--
-- * 'sdShards' - The shards that comprise the stream.
--
-- * 'sdTableName' - The DynamoDB table with which the stream is associated.
--
-- * 'sdCreationRequestDateTime' - The date and time when the request to create this stream was issued.
streamDescription
    :: StreamDescription
streamDescription =
  StreamDescription'
    { _sdLastEvaluatedShardId = Nothing
    , _sdStreamLabel = Nothing
    , _sdStreamStatus = Nothing
    , _sdKeySchema = Nothing
    , _sdStreamViewType = Nothing
    , _sdStreamARN = Nothing
    , _sdShards = Nothing
    , _sdTableName = Nothing
    , _sdCreationRequestDateTime = Nothing
    }


-- | The shard ID of the item where the operation stopped, inclusive of the previous result set. Use this value to start a new operation, excluding this value in the new request. If @LastEvaluatedShardId@ is empty, then the "last page" of results has been processed and there is currently no more data to be retrieved. If @LastEvaluatedShardId@ is not empty, it does not necessarily mean that there is more data in the result set. The only way to know when you have reached the end of the result set is when @LastEvaluatedShardId@ is empty.
sdLastEvaluatedShardId :: Lens' StreamDescription (Maybe Text)
sdLastEvaluatedShardId = lens _sdLastEvaluatedShardId (\ s a -> s{_sdLastEvaluatedShardId = a})

-- | A timestamp, in ISO 8601 format, for this stream. Note that @LatestStreamLabel@ is not a unique identifier for the stream, because it is possible that a stream from another table might have the same timestamp. However, the combination of the following three elements is guaranteed to be unique:     * the AWS customer ID.     * the table name     * the @StreamLabel@
sdStreamLabel :: Lens' StreamDescription (Maybe Text)
sdStreamLabel = lens _sdStreamLabel (\ s a -> s{_sdStreamLabel = a})

-- | Indicates the current status of the stream:     * @ENABLING@ - Streams is currently being enabled on the DynamoDB table.     * @ENABLED@ - the stream is enabled.     * @DISABLING@ - Streams is currently being disabled on the DynamoDB table.     * @DISABLED@ - the stream is disabled.
sdStreamStatus :: Lens' StreamDescription (Maybe StreamStatus)
sdStreamStatus = lens _sdStreamStatus (\ s a -> s{_sdStreamStatus = a})

-- | The key attribute(s) of the stream's DynamoDB table.
sdKeySchema :: Lens' StreamDescription (Maybe (NonEmpty KeySchemaElement))
sdKeySchema = lens _sdKeySchema (\ s a -> s{_sdKeySchema = a}) . mapping _List1

-- | Indicates the format of the records within this stream:     * @KEYS_ONLY@ - only the key attributes of items that were modified in the DynamoDB table.     * @NEW_IMAGE@ - entire items from the table, as they appeared after they were modified.     * @OLD_IMAGE@ - entire items from the table, as they appeared before they were modified.     * @NEW_AND_OLD_IMAGES@ - both the new and the old images of the items from the table.
sdStreamViewType :: Lens' StreamDescription (Maybe StreamViewType)
sdStreamViewType = lens _sdStreamViewType (\ s a -> s{_sdStreamViewType = a})

-- | The Amazon Resource Name (ARN) for the stream.
sdStreamARN :: Lens' StreamDescription (Maybe Text)
sdStreamARN = lens _sdStreamARN (\ s a -> s{_sdStreamARN = a})

-- | The shards that comprise the stream.
sdShards :: Lens' StreamDescription [Shard]
sdShards = lens _sdShards (\ s a -> s{_sdShards = a}) . _Default . _Coerce

-- | The DynamoDB table with which the stream is associated.
sdTableName :: Lens' StreamDescription (Maybe Text)
sdTableName = lens _sdTableName (\ s a -> s{_sdTableName = a})

-- | The date and time when the request to create this stream was issued.
sdCreationRequestDateTime :: Lens' StreamDescription (Maybe UTCTime)
sdCreationRequestDateTime = lens _sdCreationRequestDateTime (\ s a -> s{_sdCreationRequestDateTime = a}) . mapping _Time

instance FromJSON StreamDescription where
        parseJSON
          = withObject "StreamDescription"
              (\ x ->
                 StreamDescription' <$>
                   (x .:? "LastEvaluatedShardId") <*>
                     (x .:? "StreamLabel")
                     <*> (x .:? "StreamStatus")
                     <*> (x .:? "KeySchema")
                     <*> (x .:? "StreamViewType")
                     <*> (x .:? "StreamArn")
                     <*> (x .:? "Shards" .!= mempty)
                     <*> (x .:? "TableName")
                     <*> (x .:? "CreationRequestDateTime"))

instance Hashable StreamDescription where

instance NFData StreamDescription where

-- | A description of a single data modification that was performed on an item in a DynamoDB table.
--
--
--
-- /See:/ 'streamRecord' smart constructor.
data StreamRecord = StreamRecord'
  { _srSizeBytes                   :: !(Maybe Nat)
  , _srSequenceNumber              :: !(Maybe Text)
  , _srApproximateCreationDateTime :: !(Maybe POSIX)
  , _srStreamViewType              :: !(Maybe StreamViewType)
  , _srKeys                        :: !(Maybe (Map Text AttributeValue))
  , _srOldImage                    :: !(Maybe (Map Text AttributeValue))
  , _srNewImage                    :: !(Maybe (Map Text AttributeValue))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StreamRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srSizeBytes' - The size of the stream record, in bytes.
--
-- * 'srSequenceNumber' - The sequence number of the stream record.
--
-- * 'srApproximateCreationDateTime' - The approximate date and time when the stream record was created, in <http://www.epochconverter.com/ UNIX epoch time> format.
--
-- * 'srStreamViewType' - The type of data from the modified DynamoDB item that was captured in this stream record:     * @KEYS_ONLY@ - only the key attributes of the modified item.     * @NEW_IMAGE@ - the entire item, as it appeared after it was modified.     * @OLD_IMAGE@ - the entire item, as it appeared before it was modified.     * @NEW_AND_OLD_IMAGES@ - both the new and the old item images of the item.
--
-- * 'srKeys' - The primary key attribute(s) for the DynamoDB item that was modified.
--
-- * 'srOldImage' - The item in the DynamoDB table as it appeared before it was modified.
--
-- * 'srNewImage' - The item in the DynamoDB table as it appeared after it was modified.
streamRecord
    :: StreamRecord
streamRecord =
  StreamRecord'
    { _srSizeBytes = Nothing
    , _srSequenceNumber = Nothing
    , _srApproximateCreationDateTime = Nothing
    , _srStreamViewType = Nothing
    , _srKeys = Nothing
    , _srOldImage = Nothing
    , _srNewImage = Nothing
    }


-- | The size of the stream record, in bytes.
srSizeBytes :: Lens' StreamRecord (Maybe Natural)
srSizeBytes = lens _srSizeBytes (\ s a -> s{_srSizeBytes = a}) . mapping _Nat

-- | The sequence number of the stream record.
srSequenceNumber :: Lens' StreamRecord (Maybe Text)
srSequenceNumber = lens _srSequenceNumber (\ s a -> s{_srSequenceNumber = a})

-- | The approximate date and time when the stream record was created, in <http://www.epochconverter.com/ UNIX epoch time> format.
srApproximateCreationDateTime :: Lens' StreamRecord (Maybe UTCTime)
srApproximateCreationDateTime = lens _srApproximateCreationDateTime (\ s a -> s{_srApproximateCreationDateTime = a}) . mapping _Time

-- | The type of data from the modified DynamoDB item that was captured in this stream record:     * @KEYS_ONLY@ - only the key attributes of the modified item.     * @NEW_IMAGE@ - the entire item, as it appeared after it was modified.     * @OLD_IMAGE@ - the entire item, as it appeared before it was modified.     * @NEW_AND_OLD_IMAGES@ - both the new and the old item images of the item.
srStreamViewType :: Lens' StreamRecord (Maybe StreamViewType)
srStreamViewType = lens _srStreamViewType (\ s a -> s{_srStreamViewType = a})

-- | The primary key attribute(s) for the DynamoDB item that was modified.
srKeys :: Lens' StreamRecord (HashMap Text AttributeValue)
srKeys = lens _srKeys (\ s a -> s{_srKeys = a}) . _Default . _Map

-- | The item in the DynamoDB table as it appeared before it was modified.
srOldImage :: Lens' StreamRecord (HashMap Text AttributeValue)
srOldImage = lens _srOldImage (\ s a -> s{_srOldImage = a}) . _Default . _Map

-- | The item in the DynamoDB table as it appeared after it was modified.
srNewImage :: Lens' StreamRecord (HashMap Text AttributeValue)
srNewImage = lens _srNewImage (\ s a -> s{_srNewImage = a}) . _Default . _Map

instance FromJSON StreamRecord where
        parseJSON
          = withObject "StreamRecord"
              (\ x ->
                 StreamRecord' <$>
                   (x .:? "SizeBytes") <*> (x .:? "SequenceNumber") <*>
                     (x .:? "ApproximateCreationDateTime")
                     <*> (x .:? "StreamViewType")
                     <*> (x .:? "Keys" .!= mempty)
                     <*> (x .:? "OldImage" .!= mempty)
                     <*> (x .:? "NewImage" .!= mempty))

instance Hashable StreamRecord where

instance NFData StreamRecord where

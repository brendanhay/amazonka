{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.Product where

import           Network.AWS.Kinesis.Types.Sum
import           Network.AWS.Prelude

-- | The range of possible hash key values for the shard, which is a set of
-- ordered contiguous positive integers.
--
-- /See:/ 'hashKeyRange' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'hkrStartingHashKey'
--
-- * 'hkrEndingHashKey'
data HashKeyRange = HashKeyRange'
    { _hkrStartingHashKey :: !Text
    , _hkrEndingHashKey   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'HashKeyRange' smart constructor.
hashKeyRange :: Text -> Text -> HashKeyRange
hashKeyRange pStartingHashKey_ pEndingHashKey_ =
    HashKeyRange'
    { _hkrStartingHashKey = pStartingHashKey_
    , _hkrEndingHashKey = pEndingHashKey_
    }

-- | The starting hash key of the hash key range.
hkrStartingHashKey :: Lens' HashKeyRange Text
hkrStartingHashKey = lens _hkrStartingHashKey (\ s a -> s{_hkrStartingHashKey = a});

-- | The ending hash key of the hash key range.
hkrEndingHashKey :: Lens' HashKeyRange Text
hkrEndingHashKey = lens _hkrEndingHashKey (\ s a -> s{_hkrEndingHashKey = a});

instance FromJSON HashKeyRange where
        parseJSON
          = withObject "HashKeyRange"
              (\ x ->
                 HashKeyRange' <$>
                   (x .: "StartingHashKey") <*> (x .: "EndingHashKey"))

-- | Represents the output for @PutRecords@.
--
-- /See:/ 'putRecordsRequestEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prreExplicitHashKey'
--
-- * 'prreData'
--
-- * 'prrePartitionKey'
data PutRecordsRequestEntry = PutRecordsRequestEntry'
    { _prreExplicitHashKey :: !(Maybe Text)
    , _prreData            :: !Base64
    , _prrePartitionKey    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutRecordsRequestEntry' smart constructor.
putRecordsRequestEntry :: ByteString -> Text -> PutRecordsRequestEntry
putRecordsRequestEntry pData_ pPartitionKey_ =
    PutRecordsRequestEntry'
    { _prreExplicitHashKey = Nothing
    , _prreData = _Base64 # pData_
    , _prrePartitionKey = pPartitionKey_
    }

-- | The hash value used to determine explicitly the shard that the data
-- record is assigned to by overriding the partition key hash.
prreExplicitHashKey :: Lens' PutRecordsRequestEntry (Maybe Text)
prreExplicitHashKey = lens _prreExplicitHashKey (\ s a -> s{_prreExplicitHashKey = a});

-- | The data blob to put into the record, which is base64-encoded when the
-- blob is serialized. The maximum size of the data blob (the payload
-- before base64-encoding) is 50 kilobytes (KB)
prreData :: Lens' PutRecordsRequestEntry ByteString
prreData = lens _prreData (\ s a -> s{_prreData = a}) . _Base64;

-- | Determines which shard in the stream the data record is assigned to.
-- Partition keys are Unicode strings with a maximum length limit of 256
-- characters for each key. Amazon Kinesis uses the partition key as input
-- to a hash function that maps the partition key and associated data to a
-- specific shard. Specifically, an MD5 hash function is used to map
-- partition keys to 128-bit integer values and to map associated data
-- records to shards. As a result of this hashing mechanism, all data
-- records with the same partition key map to the same shard within the
-- stream.
prrePartitionKey :: Lens' PutRecordsRequestEntry Text
prrePartitionKey = lens _prrePartitionKey (\ s a -> s{_prrePartitionKey = a});

instance ToJSON PutRecordsRequestEntry where
        toJSON PutRecordsRequestEntry'{..}
          = object
              ["ExplicitHashKey" .= _prreExplicitHashKey,
               "Data" .= _prreData,
               "PartitionKey" .= _prrePartitionKey]

-- | Represents the result of an individual record from a @PutRecords@
-- request. A record that is successfully added to your Amazon Kinesis
-- stream includes SequenceNumber and ShardId in the result. A record that
-- fails to be added to your Amazon Kinesis stream includes ErrorCode and
-- ErrorMessage in the result.
--
-- /See:/ 'putRecordsResultEntry' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prreSequenceNumber'
--
-- * 'prreErrorCode'
--
-- * 'prreErrorMessage'
--
-- * 'prreShardId'
data PutRecordsResultEntry = PutRecordsResultEntry'
    { _prreSequenceNumber :: !(Maybe Text)
    , _prreErrorCode      :: !(Maybe Text)
    , _prreErrorMessage   :: !(Maybe Text)
    , _prreShardId        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutRecordsResultEntry' smart constructor.
putRecordsResultEntry :: PutRecordsResultEntry
putRecordsResultEntry =
    PutRecordsResultEntry'
    { _prreSequenceNumber = Nothing
    , _prreErrorCode = Nothing
    , _prreErrorMessage = Nothing
    , _prreShardId = Nothing
    }

-- | The sequence number for an individual record result.
prreSequenceNumber :: Lens' PutRecordsResultEntry (Maybe Text)
prreSequenceNumber = lens _prreSequenceNumber (\ s a -> s{_prreSequenceNumber = a});

-- | The error code for an individual record result. @ErrorCodes@ can be
-- either @ProvisionedThroughputExceededException@ or @InternalFailure@.
prreErrorCode :: Lens' PutRecordsResultEntry (Maybe Text)
prreErrorCode = lens _prreErrorCode (\ s a -> s{_prreErrorCode = a});

-- | The error message for an individual record result. An @ErrorCode@ value
-- of @ProvisionedThroughputExceededException@ has an error message that
-- includes the account ID, stream name, and shard ID. An @ErrorCode@ value
-- of @InternalFailure@ has the error message
-- @\"Internal Service Failure\"@.
prreErrorMessage :: Lens' PutRecordsResultEntry (Maybe Text)
prreErrorMessage = lens _prreErrorMessage (\ s a -> s{_prreErrorMessage = a});

-- | The shard ID for an individual record result.
prreShardId :: Lens' PutRecordsResultEntry (Maybe Text)
prreShardId = lens _prreShardId (\ s a -> s{_prreShardId = a});

instance FromJSON PutRecordsResultEntry where
        parseJSON
          = withObject "PutRecordsResultEntry"
              (\ x ->
                 PutRecordsResultEntry' <$>
                   (x .:? "SequenceNumber") <*> (x .:? "ErrorCode") <*>
                     (x .:? "ErrorMessage")
                     <*> (x .:? "ShardId"))

-- | The unit of data of the Amazon Kinesis stream, which is composed of a
-- sequence number, a partition key, and a data blob.
--
-- /See:/ 'record' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rSequenceNumber'
--
-- * 'rData'
--
-- * 'rPartitionKey'
data Record = Record'
    { _rSequenceNumber :: !Text
    , _rData           :: !Base64
    , _rPartitionKey   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Record' smart constructor.
record :: Text -> ByteString -> Text -> Record
record pSequenceNumber_ pData_ pPartitionKey_ =
    Record'
    { _rSequenceNumber = pSequenceNumber_
    , _rData = _Base64 # pData_
    , _rPartitionKey = pPartitionKey_
    }

-- | The unique identifier for the record in the Amazon Kinesis stream.
rSequenceNumber :: Lens' Record Text
rSequenceNumber = lens _rSequenceNumber (\ s a -> s{_rSequenceNumber = a});

-- | The data blob. The data in the blob is both opaque and immutable to the
-- Amazon Kinesis service, which does not inspect, interpret, or change the
-- data in the blob in any way. The maximum size of the data blob (the
-- payload before base64-encoding) is 50 kilobytes (KB)
rData :: Lens' Record ByteString
rData = lens _rData (\ s a -> s{_rData = a}) . _Base64;

-- | Identifies which shard in the stream the data record is assigned to.
rPartitionKey :: Lens' Record Text
rPartitionKey = lens _rPartitionKey (\ s a -> s{_rPartitionKey = a});

instance FromJSON Record where
        parseJSON
          = withObject "Record"
              (\ x ->
                 Record' <$>
                   (x .: "SequenceNumber") <*> (x .: "Data") <*>
                     (x .: "PartitionKey"))

-- | The range of possible sequence numbers for the shard.
--
-- /See:/ 'sequenceNumberRange' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'snrEndingSequenceNumber'
--
-- * 'snrStartingSequenceNumber'
data SequenceNumberRange = SequenceNumberRange'
    { _snrEndingSequenceNumber   :: !(Maybe Text)
    , _snrStartingSequenceNumber :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SequenceNumberRange' smart constructor.
sequenceNumberRange :: Text -> SequenceNumberRange
sequenceNumberRange pStartingSequenceNumber_ =
    SequenceNumberRange'
    { _snrEndingSequenceNumber = Nothing
    , _snrStartingSequenceNumber = pStartingSequenceNumber_
    }

-- | The ending sequence number for the range. Shards that are in the OPEN
-- state have an ending sequence number of @null@.
snrEndingSequenceNumber :: Lens' SequenceNumberRange (Maybe Text)
snrEndingSequenceNumber = lens _snrEndingSequenceNumber (\ s a -> s{_snrEndingSequenceNumber = a});

-- | The starting sequence number for the range.
snrStartingSequenceNumber :: Lens' SequenceNumberRange Text
snrStartingSequenceNumber = lens _snrStartingSequenceNumber (\ s a -> s{_snrStartingSequenceNumber = a});

instance FromJSON SequenceNumberRange where
        parseJSON
          = withObject "SequenceNumberRange"
              (\ x ->
                 SequenceNumberRange' <$>
                   (x .:? "EndingSequenceNumber") <*>
                     (x .: "StartingSequenceNumber"))

-- | A uniquely identified group of data records in an Amazon Kinesis stream.
--
-- /See:/ 'shard' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sAdjacentParentShardId'
--
-- * 'sParentShardId'
--
-- * 'sShardId'
--
-- * 'sHashKeyRange'
--
-- * 'sSequenceNumberRange'
data Shard = Shard'
    { _sAdjacentParentShardId :: !(Maybe Text)
    , _sParentShardId         :: !(Maybe Text)
    , _sShardId               :: !Text
    , _sHashKeyRange          :: !HashKeyRange
    , _sSequenceNumberRange   :: !SequenceNumberRange
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Shard' smart constructor.
shard :: Text -> HashKeyRange -> SequenceNumberRange -> Shard
shard pShardId_ pHashKeyRange_ pSequenceNumberRange_ =
    Shard'
    { _sAdjacentParentShardId = Nothing
    , _sParentShardId = Nothing
    , _sShardId = pShardId_
    , _sHashKeyRange = pHashKeyRange_
    , _sSequenceNumberRange = pSequenceNumberRange_
    }

-- | The shard Id of the shard adjacent to the shard\'s parent.
sAdjacentParentShardId :: Lens' Shard (Maybe Text)
sAdjacentParentShardId = lens _sAdjacentParentShardId (\ s a -> s{_sAdjacentParentShardId = a});

-- | The shard Id of the shard\'s parent.
sParentShardId :: Lens' Shard (Maybe Text)
sParentShardId = lens _sParentShardId (\ s a -> s{_sParentShardId = a});

-- | The unique identifier of the shard within the Amazon Kinesis stream.
sShardId :: Lens' Shard Text
sShardId = lens _sShardId (\ s a -> s{_sShardId = a});

-- | The range of possible hash key values for the shard, which is a set of
-- ordered contiguous positive integers.
sHashKeyRange :: Lens' Shard HashKeyRange
sHashKeyRange = lens _sHashKeyRange (\ s a -> s{_sHashKeyRange = a});

-- | The range of possible sequence numbers for the shard.
sSequenceNumberRange :: Lens' Shard SequenceNumberRange
sSequenceNumberRange = lens _sSequenceNumberRange (\ s a -> s{_sSequenceNumberRange = a});

instance FromJSON Shard where
        parseJSON
          = withObject "Shard"
              (\ x ->
                 Shard' <$>
                   (x .:? "AdjacentParentShardId") <*>
                     (x .:? "ParentShardId")
                     <*> (x .: "ShardId")
                     <*> (x .: "HashKeyRange")
                     <*> (x .: "SequenceNumberRange"))

-- | Represents the output for @DescribeStream@.
--
-- /See:/ 'streamDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdStreamName'
--
-- * 'sdStreamARN'
--
-- * 'sdStreamStatus'
--
-- * 'sdShards'
--
-- * 'sdHasMoreShards'
data StreamDescription = StreamDescription'
    { _sdStreamName    :: !Text
    , _sdStreamARN     :: !Text
    , _sdStreamStatus  :: !StreamStatus
    , _sdShards        :: ![Shard]
    , _sdHasMoreShards :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StreamDescription' smart constructor.
streamDescription :: Text -> Text -> StreamStatus -> Bool -> StreamDescription
streamDescription pStreamName_ pStreamARN_ pStreamStatus_ pHasMoreShards_ =
    StreamDescription'
    { _sdStreamName = pStreamName_
    , _sdStreamARN = pStreamARN_
    , _sdStreamStatus = pStreamStatus_
    , _sdShards = mempty
    , _sdHasMoreShards = pHasMoreShards_
    }

-- | The name of the stream being described.
sdStreamName :: Lens' StreamDescription Text
sdStreamName = lens _sdStreamName (\ s a -> s{_sdStreamName = a});

-- | The Amazon Resource Name (ARN) for the stream being described.
sdStreamARN :: Lens' StreamDescription Text
sdStreamARN = lens _sdStreamARN (\ s a -> s{_sdStreamARN = a});

-- | The current status of the stream being described.
--
-- The stream status is one of the following states:
--
-- -   @CREATING@ - The stream is being created. Amazon Kinesis immediately
--     returns and sets @StreamStatus@ to @CREATING@.
-- -   @DELETING@ - The stream is being deleted. The specified stream is in
--     the @DELETING@ state until Amazon Kinesis completes the deletion.
-- -   @ACTIVE@ - The stream exists and is ready for read and write
--     operations or deletion. You should perform read and write operations
--     only on an @ACTIVE@ stream.
-- -   @UPDATING@ - Shards in the stream are being merged or split. Read
--     and write operations continue to work while the stream is in the
--     @UPDATING@ state.
sdStreamStatus :: Lens' StreamDescription StreamStatus
sdStreamStatus = lens _sdStreamStatus (\ s a -> s{_sdStreamStatus = a});

-- | The shards that comprise the stream.
sdShards :: Lens' StreamDescription [Shard]
sdShards = lens _sdShards (\ s a -> s{_sdShards = a}) . _Coerce;

-- | If set to @true@, more shards in the stream are available to describe.
sdHasMoreShards :: Lens' StreamDescription Bool
sdHasMoreShards = lens _sdHasMoreShards (\ s a -> s{_sdHasMoreShards = a});

instance FromJSON StreamDescription where
        parseJSON
          = withObject "StreamDescription"
              (\ x ->
                 StreamDescription' <$>
                   (x .: "StreamName") <*> (x .: "StreamARN") <*>
                     (x .: "StreamStatus")
                     <*> (x .:? "Shards" .!= mempty)
                     <*> (x .: "HasMoreShards"))

-- | Metadata assigned to the stream, consisting of a key-value pair.
--
-- /See:/ 'tag' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagValue'
--
-- * 'tagKey'
data Tag = Tag'
    { _tagValue :: !(Maybe Text)
    , _tagKey   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Tag' smart constructor.
tag :: Text -> Tag
tag pKey_ =
    Tag'
    { _tagValue = Nothing
    , _tagKey = pKey_
    }

-- | An optional string, typically used to describe or define the tag.
-- Maximum length: 256 characters. Valid characters: Unicode letters,
-- digits, white space, _ . \/ = + - % \@
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | A unique identifier for the tag. Maximum length: 128 characters. Valid
-- characters: Unicode letters, digits, white space, _ . \/ = + - % \@
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "Value") <*> (x .: "Key"))

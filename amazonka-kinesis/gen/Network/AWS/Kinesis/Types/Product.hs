{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.Product where

import Network.AWS.Kinesis.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents enhanced metrics types.
--
--
--
-- /See:/ 'enhancedMetrics' smart constructor.
newtype EnhancedMetrics = EnhancedMetrics'
  { _emShardLevelMetrics :: Maybe [MetricsName]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnhancedMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'emShardLevelMetrics' - List of shard-level metrics. The following are the valid shard-level metrics. The value "@ALL@ " enhances every metric.     * @IncomingBytes@      * @IncomingRecords@      * @OutgoingBytes@      * @OutgoingRecords@      * @WriteProvisionedThroughputExceeded@      * @ReadProvisionedThroughputExceeded@      * @IteratorAgeMilliseconds@      * @ALL@  For more information, see <http://docs.aws.amazon.com/kinesis/latest/dev/monitoring-with-cloudwatch.html Monitoring the Amazon Kinesis Data Streams Service with Amazon CloudWatch> in the /Amazon Kinesis Data Streams Developer Guide/ .
enhancedMetrics
    :: EnhancedMetrics
enhancedMetrics = EnhancedMetrics' {_emShardLevelMetrics = Nothing}


-- | List of shard-level metrics. The following are the valid shard-level metrics. The value "@ALL@ " enhances every metric.     * @IncomingBytes@      * @IncomingRecords@      * @OutgoingBytes@      * @OutgoingRecords@      * @WriteProvisionedThroughputExceeded@      * @ReadProvisionedThroughputExceeded@      * @IteratorAgeMilliseconds@      * @ALL@  For more information, see <http://docs.aws.amazon.com/kinesis/latest/dev/monitoring-with-cloudwatch.html Monitoring the Amazon Kinesis Data Streams Service with Amazon CloudWatch> in the /Amazon Kinesis Data Streams Developer Guide/ .
emShardLevelMetrics :: Lens' EnhancedMetrics [MetricsName]
emShardLevelMetrics = lens _emShardLevelMetrics (\ s a -> s{_emShardLevelMetrics = a}) . _Default . _Coerce

instance FromJSON EnhancedMetrics where
        parseJSON
          = withObject "EnhancedMetrics"
              (\ x ->
                 EnhancedMetrics' <$>
                   (x .:? "ShardLevelMetrics" .!= mempty))

instance Hashable EnhancedMetrics where

instance NFData EnhancedMetrics where

-- | Represents the output for 'EnableEnhancedMonitoring' and 'DisableEnhancedMonitoring' .
--
--
--
-- /See:/ 'enhancedMonitoringOutput' smart constructor.
data EnhancedMonitoringOutput = EnhancedMonitoringOutput'
  { _emoDesiredShardLevelMetrics :: !(Maybe [MetricsName])
  , _emoCurrentShardLevelMetrics :: !(Maybe [MetricsName])
  , _emoStreamName               :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnhancedMonitoringOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'emoDesiredShardLevelMetrics' - Represents the list of all the metrics that would be in the enhanced state after the operation.
--
-- * 'emoCurrentShardLevelMetrics' - Represents the current state of the metrics that are in the enhanced state before the operation.
--
-- * 'emoStreamName' - The name of the Kinesis data stream.
enhancedMonitoringOutput
    :: EnhancedMonitoringOutput
enhancedMonitoringOutput =
  EnhancedMonitoringOutput'
    { _emoDesiredShardLevelMetrics = Nothing
    , _emoCurrentShardLevelMetrics = Nothing
    , _emoStreamName = Nothing
    }


-- | Represents the list of all the metrics that would be in the enhanced state after the operation.
emoDesiredShardLevelMetrics :: Lens' EnhancedMonitoringOutput [MetricsName]
emoDesiredShardLevelMetrics = lens _emoDesiredShardLevelMetrics (\ s a -> s{_emoDesiredShardLevelMetrics = a}) . _Default . _Coerce

-- | Represents the current state of the metrics that are in the enhanced state before the operation.
emoCurrentShardLevelMetrics :: Lens' EnhancedMonitoringOutput [MetricsName]
emoCurrentShardLevelMetrics = lens _emoCurrentShardLevelMetrics (\ s a -> s{_emoCurrentShardLevelMetrics = a}) . _Default . _Coerce

-- | The name of the Kinesis data stream.
emoStreamName :: Lens' EnhancedMonitoringOutput (Maybe Text)
emoStreamName = lens _emoStreamName (\ s a -> s{_emoStreamName = a})

instance FromJSON EnhancedMonitoringOutput where
        parseJSON
          = withObject "EnhancedMonitoringOutput"
              (\ x ->
                 EnhancedMonitoringOutput' <$>
                   (x .:? "DesiredShardLevelMetrics" .!= mempty) <*>
                     (x .:? "CurrentShardLevelMetrics" .!= mempty)
                     <*> (x .:? "StreamName"))

instance Hashable EnhancedMonitoringOutput where

instance NFData EnhancedMonitoringOutput where

-- | The range of possible hash key values for the shard, which is a set of ordered contiguous positive integers.
--
--
--
-- /See:/ 'hashKeyRange' smart constructor.
data HashKeyRange = HashKeyRange'
  { _hkrStartingHashKey :: !Text
  , _hkrEndingHashKey   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HashKeyRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hkrStartingHashKey' - The starting hash key of the hash key range.
--
-- * 'hkrEndingHashKey' - The ending hash key of the hash key range.
hashKeyRange
    :: Text -- ^ 'hkrStartingHashKey'
    -> Text -- ^ 'hkrEndingHashKey'
    -> HashKeyRange
hashKeyRange pStartingHashKey_ pEndingHashKey_ =
  HashKeyRange'
    { _hkrStartingHashKey = pStartingHashKey_
    , _hkrEndingHashKey = pEndingHashKey_
    }


-- | The starting hash key of the hash key range.
hkrStartingHashKey :: Lens' HashKeyRange Text
hkrStartingHashKey = lens _hkrStartingHashKey (\ s a -> s{_hkrStartingHashKey = a})

-- | The ending hash key of the hash key range.
hkrEndingHashKey :: Lens' HashKeyRange Text
hkrEndingHashKey = lens _hkrEndingHashKey (\ s a -> s{_hkrEndingHashKey = a})

instance FromJSON HashKeyRange where
        parseJSON
          = withObject "HashKeyRange"
              (\ x ->
                 HashKeyRange' <$>
                   (x .: "StartingHashKey") <*> (x .: "EndingHashKey"))

instance Hashable HashKeyRange where

instance NFData HashKeyRange where

-- | Represents the output for @PutRecords@ .
--
--
--
-- /See:/ 'putRecordsRequestEntry' smart constructor.
data PutRecordsRequestEntry = PutRecordsRequestEntry'
  { _prreExplicitHashKey :: !(Maybe Text)
  , _prreData            :: !Base64
  , _prrePartitionKey    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRecordsRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prreExplicitHashKey' - The hash value used to determine explicitly the shard that the data record is assigned to by overriding the partition key hash.
--
-- * 'prreData' - The data blob to put into the record, which is base64-encoded when the blob is serialized. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MB).-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'prrePartitionKey' - Determines which shard in the stream the data record is assigned to. Partition keys are Unicode strings with a maximum length limit of 256 characters for each key. Amazon Kinesis Data Streams uses the partition key as input to a hash function that maps the partition key and associated data to a specific shard. Specifically, an MD5 hash function is used to map partition keys to 128-bit integer values and to map associated data records to shards. As a result of this hashing mechanism, all data records with the same partition key map to the same shard within the stream.
putRecordsRequestEntry
    :: ByteString -- ^ 'prreData'
    -> Text -- ^ 'prrePartitionKey'
    -> PutRecordsRequestEntry
putRecordsRequestEntry pData_ pPartitionKey_ =
  PutRecordsRequestEntry'
    { _prreExplicitHashKey = Nothing
    , _prreData = _Base64 # pData_
    , _prrePartitionKey = pPartitionKey_
    }


-- | The hash value used to determine explicitly the shard that the data record is assigned to by overriding the partition key hash.
prreExplicitHashKey :: Lens' PutRecordsRequestEntry (Maybe Text)
prreExplicitHashKey = lens _prreExplicitHashKey (\ s a -> s{_prreExplicitHashKey = a})

-- | The data blob to put into the record, which is base64-encoded when the blob is serialized. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MB).-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
prreData :: Lens' PutRecordsRequestEntry ByteString
prreData = lens _prreData (\ s a -> s{_prreData = a}) . _Base64

-- | Determines which shard in the stream the data record is assigned to. Partition keys are Unicode strings with a maximum length limit of 256 characters for each key. Amazon Kinesis Data Streams uses the partition key as input to a hash function that maps the partition key and associated data to a specific shard. Specifically, an MD5 hash function is used to map partition keys to 128-bit integer values and to map associated data records to shards. As a result of this hashing mechanism, all data records with the same partition key map to the same shard within the stream.
prrePartitionKey :: Lens' PutRecordsRequestEntry Text
prrePartitionKey = lens _prrePartitionKey (\ s a -> s{_prrePartitionKey = a})

instance Hashable PutRecordsRequestEntry where

instance NFData PutRecordsRequestEntry where

instance ToJSON PutRecordsRequestEntry where
        toJSON PutRecordsRequestEntry'{..}
          = object
              (catMaybes
                 [("ExplicitHashKey" .=) <$> _prreExplicitHashKey,
                  Just ("Data" .= _prreData),
                  Just ("PartitionKey" .= _prrePartitionKey)])

-- | Represents the result of an individual record from a @PutRecords@ request. A record that is successfully added to a stream includes @SequenceNumber@ and @ShardId@ in the result. A record that fails to be added to the stream includes @ErrorCode@ and @ErrorMessage@ in the result.
--
--
--
-- /See:/ 'putRecordsResultEntry' smart constructor.
data PutRecordsResultEntry = PutRecordsResultEntry'
  { _prreSequenceNumber :: !(Maybe Text)
  , _prreErrorCode      :: !(Maybe Text)
  , _prreErrorMessage   :: !(Maybe Text)
  , _prreShardId        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutRecordsResultEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prreSequenceNumber' - The sequence number for an individual record result.
--
-- * 'prreErrorCode' - The error code for an individual record result. @ErrorCodes@ can be either @ProvisionedThroughputExceededException@ or @InternalFailure@ .
--
-- * 'prreErrorMessage' - The error message for an individual record result. An @ErrorCode@ value of @ProvisionedThroughputExceededException@ has an error message that includes the account ID, stream name, and shard ID. An @ErrorCode@ value of @InternalFailure@ has the error message @"Internal Service Failure"@ .
--
-- * 'prreShardId' - The shard ID for an individual record result.
putRecordsResultEntry
    :: PutRecordsResultEntry
putRecordsResultEntry =
  PutRecordsResultEntry'
    { _prreSequenceNumber = Nothing
    , _prreErrorCode = Nothing
    , _prreErrorMessage = Nothing
    , _prreShardId = Nothing
    }


-- | The sequence number for an individual record result.
prreSequenceNumber :: Lens' PutRecordsResultEntry (Maybe Text)
prreSequenceNumber = lens _prreSequenceNumber (\ s a -> s{_prreSequenceNumber = a})

-- | The error code for an individual record result. @ErrorCodes@ can be either @ProvisionedThroughputExceededException@ or @InternalFailure@ .
prreErrorCode :: Lens' PutRecordsResultEntry (Maybe Text)
prreErrorCode = lens _prreErrorCode (\ s a -> s{_prreErrorCode = a})

-- | The error message for an individual record result. An @ErrorCode@ value of @ProvisionedThroughputExceededException@ has an error message that includes the account ID, stream name, and shard ID. An @ErrorCode@ value of @InternalFailure@ has the error message @"Internal Service Failure"@ .
prreErrorMessage :: Lens' PutRecordsResultEntry (Maybe Text)
prreErrorMessage = lens _prreErrorMessage (\ s a -> s{_prreErrorMessage = a})

-- | The shard ID for an individual record result.
prreShardId :: Lens' PutRecordsResultEntry (Maybe Text)
prreShardId = lens _prreShardId (\ s a -> s{_prreShardId = a})

instance FromJSON PutRecordsResultEntry where
        parseJSON
          = withObject "PutRecordsResultEntry"
              (\ x ->
                 PutRecordsResultEntry' <$>
                   (x .:? "SequenceNumber") <*> (x .:? "ErrorCode") <*>
                     (x .:? "ErrorMessage")
                     <*> (x .:? "ShardId"))

instance Hashable PutRecordsResultEntry where

instance NFData PutRecordsResultEntry where

-- | The unit of data of the Kinesis data stream, which is composed of a sequence number, a partition key, and a data blob.
--
--
--
-- /See:/ 'record' smart constructor.
data Record = Record'
  { _rEncryptionType              :: !(Maybe EncryptionType)
  , _rApproximateArrivalTimestamp :: !(Maybe POSIX)
  , _rSequenceNumber              :: !Text
  , _rData                        :: !Base64
  , _rPartitionKey                :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Record' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rEncryptionType' - The encryption type used on the record. This parameter can be one of the following values:     * @NONE@ : Do not encrypt the records in the stream.     * @KMS@ : Use server-side encryption on the records in the stream using a customer-managed AWS KMS key.
--
-- * 'rApproximateArrivalTimestamp' - The approximate time that the record was inserted into the stream.
--
-- * 'rSequenceNumber' - The unique identifier of the record within its shard.
--
-- * 'rData' - The data blob. The data in the blob is both opaque and immutable to Kinesis Data Streams, which does not inspect, interpret, or change the data in the blob in any way. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MB).-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'rPartitionKey' - Identifies which shard in the stream the data record is assigned to.
record
    :: Text -- ^ 'rSequenceNumber'
    -> ByteString -- ^ 'rData'
    -> Text -- ^ 'rPartitionKey'
    -> Record
record pSequenceNumber_ pData_ pPartitionKey_ =
  Record'
    { _rEncryptionType = Nothing
    , _rApproximateArrivalTimestamp = Nothing
    , _rSequenceNumber = pSequenceNumber_
    , _rData = _Base64 # pData_
    , _rPartitionKey = pPartitionKey_
    }


-- | The encryption type used on the record. This parameter can be one of the following values:     * @NONE@ : Do not encrypt the records in the stream.     * @KMS@ : Use server-side encryption on the records in the stream using a customer-managed AWS KMS key.
rEncryptionType :: Lens' Record (Maybe EncryptionType)
rEncryptionType = lens _rEncryptionType (\ s a -> s{_rEncryptionType = a})

-- | The approximate time that the record was inserted into the stream.
rApproximateArrivalTimestamp :: Lens' Record (Maybe UTCTime)
rApproximateArrivalTimestamp = lens _rApproximateArrivalTimestamp (\ s a -> s{_rApproximateArrivalTimestamp = a}) . mapping _Time

-- | The unique identifier of the record within its shard.
rSequenceNumber :: Lens' Record Text
rSequenceNumber = lens _rSequenceNumber (\ s a -> s{_rSequenceNumber = a})

-- | The data blob. The data in the blob is both opaque and immutable to Kinesis Data Streams, which does not inspect, interpret, or change the data in the blob in any way. When the data blob (the payload before base64-encoding) is added to the partition key size, the total size must not exceed the maximum record size (1 MB).-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
rData :: Lens' Record ByteString
rData = lens _rData (\ s a -> s{_rData = a}) . _Base64

-- | Identifies which shard in the stream the data record is assigned to.
rPartitionKey :: Lens' Record Text
rPartitionKey = lens _rPartitionKey (\ s a -> s{_rPartitionKey = a})

instance FromJSON Record where
        parseJSON
          = withObject "Record"
              (\ x ->
                 Record' <$>
                   (x .:? "EncryptionType") <*>
                     (x .:? "ApproximateArrivalTimestamp")
                     <*> (x .: "SequenceNumber")
                     <*> (x .: "Data")
                     <*> (x .: "PartitionKey"))

instance Hashable Record where

instance NFData Record where

-- | The range of possible sequence numbers for the shard.
--
--
--
-- /See:/ 'sequenceNumberRange' smart constructor.
data SequenceNumberRange = SequenceNumberRange'
  { _snrEndingSequenceNumber   :: !(Maybe Text)
  , _snrStartingSequenceNumber :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SequenceNumberRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'snrEndingSequenceNumber' - The ending sequence number for the range. Shards that are in the OPEN state have an ending sequence number of @null@ .
--
-- * 'snrStartingSequenceNumber' - The starting sequence number for the range.
sequenceNumberRange
    :: Text -- ^ 'snrStartingSequenceNumber'
    -> SequenceNumberRange
sequenceNumberRange pStartingSequenceNumber_ =
  SequenceNumberRange'
    { _snrEndingSequenceNumber = Nothing
    , _snrStartingSequenceNumber = pStartingSequenceNumber_
    }


-- | The ending sequence number for the range. Shards that are in the OPEN state have an ending sequence number of @null@ .
snrEndingSequenceNumber :: Lens' SequenceNumberRange (Maybe Text)
snrEndingSequenceNumber = lens _snrEndingSequenceNumber (\ s a -> s{_snrEndingSequenceNumber = a})

-- | The starting sequence number for the range.
snrStartingSequenceNumber :: Lens' SequenceNumberRange Text
snrStartingSequenceNumber = lens _snrStartingSequenceNumber (\ s a -> s{_snrStartingSequenceNumber = a})

instance FromJSON SequenceNumberRange where
        parseJSON
          = withObject "SequenceNumberRange"
              (\ x ->
                 SequenceNumberRange' <$>
                   (x .:? "EndingSequenceNumber") <*>
                     (x .: "StartingSequenceNumber"))

instance Hashable SequenceNumberRange where

instance NFData SequenceNumberRange where

-- | A uniquely identified group of data records in a Kinesis data stream.
--
--
--
-- /See:/ 'shard' smart constructor.
data Shard = Shard'
  { _sAdjacentParentShardId :: !(Maybe Text)
  , _sParentShardId         :: !(Maybe Text)
  , _sShardId               :: !Text
  , _sHashKeyRange          :: !HashKeyRange
  , _sSequenceNumberRange   :: !SequenceNumberRange
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Shard' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sAdjacentParentShardId' - The shard ID of the shard adjacent to the shard's parent.
--
-- * 'sParentShardId' - The shard ID of the shard's parent.
--
-- * 'sShardId' - The unique identifier of the shard within the stream.
--
-- * 'sHashKeyRange' - The range of possible hash key values for the shard, which is a set of ordered contiguous positive integers.
--
-- * 'sSequenceNumberRange' - The range of possible sequence numbers for the shard.
shard
    :: Text -- ^ 'sShardId'
    -> HashKeyRange -- ^ 'sHashKeyRange'
    -> SequenceNumberRange -- ^ 'sSequenceNumberRange'
    -> Shard
shard pShardId_ pHashKeyRange_ pSequenceNumberRange_ =
  Shard'
    { _sAdjacentParentShardId = Nothing
    , _sParentShardId = Nothing
    , _sShardId = pShardId_
    , _sHashKeyRange = pHashKeyRange_
    , _sSequenceNumberRange = pSequenceNumberRange_
    }


-- | The shard ID of the shard adjacent to the shard's parent.
sAdjacentParentShardId :: Lens' Shard (Maybe Text)
sAdjacentParentShardId = lens _sAdjacentParentShardId (\ s a -> s{_sAdjacentParentShardId = a})

-- | The shard ID of the shard's parent.
sParentShardId :: Lens' Shard (Maybe Text)
sParentShardId = lens _sParentShardId (\ s a -> s{_sParentShardId = a})

-- | The unique identifier of the shard within the stream.
sShardId :: Lens' Shard Text
sShardId = lens _sShardId (\ s a -> s{_sShardId = a})

-- | The range of possible hash key values for the shard, which is a set of ordered contiguous positive integers.
sHashKeyRange :: Lens' Shard HashKeyRange
sHashKeyRange = lens _sHashKeyRange (\ s a -> s{_sHashKeyRange = a})

-- | The range of possible sequence numbers for the shard.
sSequenceNumberRange :: Lens' Shard SequenceNumberRange
sSequenceNumberRange = lens _sSequenceNumberRange (\ s a -> s{_sSequenceNumberRange = a})

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

instance Hashable Shard where

instance NFData Shard where

-- | Represents the output for 'DescribeStream' .
--
--
--
-- /See:/ 'streamDescription' smart constructor.
data StreamDescription = StreamDescription'
  { _sdEncryptionType          :: !(Maybe EncryptionType)
  , _sdKeyId                   :: !(Maybe Text)
  , _sdStreamName              :: !Text
  , _sdStreamARN               :: !Text
  , _sdStreamStatus            :: !StreamStatus
  , _sdShards                  :: ![Shard]
  , _sdHasMoreShards           :: !Bool
  , _sdRetentionPeriodHours    :: !Nat
  , _sdStreamCreationTimestamp :: !POSIX
  , _sdEnhancedMonitoring      :: ![EnhancedMetrics]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StreamDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdEncryptionType' - The server-side encryption type used on the stream. This parameter can be one of the following values:     * @NONE@ : Do not encrypt the records in the stream.     * @KMS@ : Use server-side encryption on the records in the stream using a customer-managed AWS KMS key.
--
-- * 'sdKeyId' - The GUID for the customer-managed AWS KMS key to use for encryption. This value can be a globally unique identifier, a fully specified ARN to either an alias or a key, or an alias name prefixed by "alias/".You can also use a master key owned by Kinesis Data Streams by specifying the alias @aws/kinesis@ .     * Key ARN example: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@      * Alias ARN example: @arn:aws:kms:us-east-1:123456789012:alias/MyAliasName@      * Globally unique key ID example: @12345678-1234-1234-1234-123456789012@      * Alias name example: @alias/MyAliasName@      * Master key owned by Kinesis Data Streams: @alias/aws/kinesis@
--
-- * 'sdStreamName' - The name of the stream being described.
--
-- * 'sdStreamARN' - The Amazon Resource Name (ARN) for the stream being described.
--
-- * 'sdStreamStatus' - The current status of the stream being described. The stream status is one of the following states:     * @CREATING@ - The stream is being created. Kinesis Data Streams immediately returns and sets @StreamStatus@ to @CREATING@ .     * @DELETING@ - The stream is being deleted. The specified stream is in the @DELETING@ state until Kinesis Data Streams completes the deletion.     * @ACTIVE@ - The stream exists and is ready for read and write operations or deletion. You should perform read and write operations only on an @ACTIVE@ stream.     * @UPDATING@ - Shards in the stream are being merged or split. Read and write operations continue to work while the stream is in the @UPDATING@ state.
--
-- * 'sdShards' - The shards that comprise the stream.
--
-- * 'sdHasMoreShards' - If set to @true@ , more shards in the stream are available to describe.
--
-- * 'sdRetentionPeriodHours' - The current retention period, in hours.
--
-- * 'sdStreamCreationTimestamp' - The approximate time that the stream was created.
--
-- * 'sdEnhancedMonitoring' - Represents the current enhanced monitoring settings of the stream.
streamDescription
    :: Text -- ^ 'sdStreamName'
    -> Text -- ^ 'sdStreamARN'
    -> StreamStatus -- ^ 'sdStreamStatus'
    -> Bool -- ^ 'sdHasMoreShards'
    -> Natural -- ^ 'sdRetentionPeriodHours'
    -> UTCTime -- ^ 'sdStreamCreationTimestamp'
    -> StreamDescription
streamDescription pStreamName_ pStreamARN_ pStreamStatus_ pHasMoreShards_ pRetentionPeriodHours_ pStreamCreationTimestamp_ =
  StreamDescription'
    { _sdEncryptionType = Nothing
    , _sdKeyId = Nothing
    , _sdStreamName = pStreamName_
    , _sdStreamARN = pStreamARN_
    , _sdStreamStatus = pStreamStatus_
    , _sdShards = mempty
    , _sdHasMoreShards = pHasMoreShards_
    , _sdRetentionPeriodHours = _Nat # pRetentionPeriodHours_
    , _sdStreamCreationTimestamp = _Time # pStreamCreationTimestamp_
    , _sdEnhancedMonitoring = mempty
    }


-- | The server-side encryption type used on the stream. This parameter can be one of the following values:     * @NONE@ : Do not encrypt the records in the stream.     * @KMS@ : Use server-side encryption on the records in the stream using a customer-managed AWS KMS key.
sdEncryptionType :: Lens' StreamDescription (Maybe EncryptionType)
sdEncryptionType = lens _sdEncryptionType (\ s a -> s{_sdEncryptionType = a})

-- | The GUID for the customer-managed AWS KMS key to use for encryption. This value can be a globally unique identifier, a fully specified ARN to either an alias or a key, or an alias name prefixed by "alias/".You can also use a master key owned by Kinesis Data Streams by specifying the alias @aws/kinesis@ .     * Key ARN example: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@      * Alias ARN example: @arn:aws:kms:us-east-1:123456789012:alias/MyAliasName@      * Globally unique key ID example: @12345678-1234-1234-1234-123456789012@      * Alias name example: @alias/MyAliasName@      * Master key owned by Kinesis Data Streams: @alias/aws/kinesis@
sdKeyId :: Lens' StreamDescription (Maybe Text)
sdKeyId = lens _sdKeyId (\ s a -> s{_sdKeyId = a})

-- | The name of the stream being described.
sdStreamName :: Lens' StreamDescription Text
sdStreamName = lens _sdStreamName (\ s a -> s{_sdStreamName = a})

-- | The Amazon Resource Name (ARN) for the stream being described.
sdStreamARN :: Lens' StreamDescription Text
sdStreamARN = lens _sdStreamARN (\ s a -> s{_sdStreamARN = a})

-- | The current status of the stream being described. The stream status is one of the following states:     * @CREATING@ - The stream is being created. Kinesis Data Streams immediately returns and sets @StreamStatus@ to @CREATING@ .     * @DELETING@ - The stream is being deleted. The specified stream is in the @DELETING@ state until Kinesis Data Streams completes the deletion.     * @ACTIVE@ - The stream exists and is ready for read and write operations or deletion. You should perform read and write operations only on an @ACTIVE@ stream.     * @UPDATING@ - Shards in the stream are being merged or split. Read and write operations continue to work while the stream is in the @UPDATING@ state.
sdStreamStatus :: Lens' StreamDescription StreamStatus
sdStreamStatus = lens _sdStreamStatus (\ s a -> s{_sdStreamStatus = a})

-- | The shards that comprise the stream.
sdShards :: Lens' StreamDescription [Shard]
sdShards = lens _sdShards (\ s a -> s{_sdShards = a}) . _Coerce

-- | If set to @true@ , more shards in the stream are available to describe.
sdHasMoreShards :: Lens' StreamDescription Bool
sdHasMoreShards = lens _sdHasMoreShards (\ s a -> s{_sdHasMoreShards = a})

-- | The current retention period, in hours.
sdRetentionPeriodHours :: Lens' StreamDescription Natural
sdRetentionPeriodHours = lens _sdRetentionPeriodHours (\ s a -> s{_sdRetentionPeriodHours = a}) . _Nat

-- | The approximate time that the stream was created.
sdStreamCreationTimestamp :: Lens' StreamDescription UTCTime
sdStreamCreationTimestamp = lens _sdStreamCreationTimestamp (\ s a -> s{_sdStreamCreationTimestamp = a}) . _Time

-- | Represents the current enhanced monitoring settings of the stream.
sdEnhancedMonitoring :: Lens' StreamDescription [EnhancedMetrics]
sdEnhancedMonitoring = lens _sdEnhancedMonitoring (\ s a -> s{_sdEnhancedMonitoring = a}) . _Coerce

instance FromJSON StreamDescription where
        parseJSON
          = withObject "StreamDescription"
              (\ x ->
                 StreamDescription' <$>
                   (x .:? "EncryptionType") <*> (x .:? "KeyId") <*>
                     (x .: "StreamName")
                     <*> (x .: "StreamARN")
                     <*> (x .: "StreamStatus")
                     <*> (x .:? "Shards" .!= mempty)
                     <*> (x .: "HasMoreShards")
                     <*> (x .: "RetentionPeriodHours")
                     <*> (x .: "StreamCreationTimestamp")
                     <*> (x .:? "EnhancedMonitoring" .!= mempty))

instance Hashable StreamDescription where

instance NFData StreamDescription where

-- | Represents the output for 'DescribeStreamSummary'
--
--
--
-- /See:/ 'streamDescriptionSummary' smart constructor.
data StreamDescriptionSummary = StreamDescriptionSummary'
  { _sdsEncryptionType          :: !(Maybe EncryptionType)
  , _sdsKeyId                   :: !(Maybe Text)
  , _sdsStreamName              :: !Text
  , _sdsStreamARN               :: !Text
  , _sdsStreamStatus            :: !StreamStatus
  , _sdsRetentionPeriodHours    :: !Nat
  , _sdsStreamCreationTimestamp :: !POSIX
  , _sdsEnhancedMonitoring      :: ![EnhancedMetrics]
  , _sdsOpenShardCount          :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StreamDescriptionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdsEncryptionType' - The encryption type used. This value is one of the following:     * @KMS@      * @NONE@
--
-- * 'sdsKeyId' - The GUID for the customer-managed AWS KMS key to use for encryption. This value can be a globally unique identifier, a fully specified ARN to either an alias or a key, or an alias name prefixed by "alias/".You can also use a master key owned by Kinesis Data Streams by specifying the alias @aws/kinesis@ .     * Key ARN example: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@      * Alias ARN example: @arn:aws:kms:us-east-1:123456789012:alias/MyAliasName@      * Globally unique key ID example: @12345678-1234-1234-1234-123456789012@      * Alias name example: @alias/MyAliasName@      * Master key owned by Kinesis Data Streams: @alias/aws/kinesis@
--
-- * 'sdsStreamName' - The name of the stream being described.
--
-- * 'sdsStreamARN' - The Amazon Resource Name (ARN) for the stream being described.
--
-- * 'sdsStreamStatus' - The current status of the stream being described. The stream status is one of the following states:     * @CREATING@ - The stream is being created. Kinesis Data Streams immediately returns and sets @StreamStatus@ to @CREATING@ .     * @DELETING@ - The stream is being deleted. The specified stream is in the @DELETING@ state until Kinesis Data Streams completes the deletion.     * @ACTIVE@ - The stream exists and is ready for read and write operations or deletion. You should perform read and write operations only on an @ACTIVE@ stream.     * @UPDATING@ - Shards in the stream are being merged or split. Read and write operations continue to work while the stream is in the @UPDATING@ state.
--
-- * 'sdsRetentionPeriodHours' - The current retention period, in hours.
--
-- * 'sdsStreamCreationTimestamp' - The approximate time that the stream was created.
--
-- * 'sdsEnhancedMonitoring' - Represents the current enhanced monitoring settings of the stream.
--
-- * 'sdsOpenShardCount' - The number of open shards in the stream.
streamDescriptionSummary
    :: Text -- ^ 'sdsStreamName'
    -> Text -- ^ 'sdsStreamARN'
    -> StreamStatus -- ^ 'sdsStreamStatus'
    -> Natural -- ^ 'sdsRetentionPeriodHours'
    -> UTCTime -- ^ 'sdsStreamCreationTimestamp'
    -> Natural -- ^ 'sdsOpenShardCount'
    -> StreamDescriptionSummary
streamDescriptionSummary pStreamName_ pStreamARN_ pStreamStatus_ pRetentionPeriodHours_ pStreamCreationTimestamp_ pOpenShardCount_ =
  StreamDescriptionSummary'
    { _sdsEncryptionType = Nothing
    , _sdsKeyId = Nothing
    , _sdsStreamName = pStreamName_
    , _sdsStreamARN = pStreamARN_
    , _sdsStreamStatus = pStreamStatus_
    , _sdsRetentionPeriodHours = _Nat # pRetentionPeriodHours_
    , _sdsStreamCreationTimestamp = _Time # pStreamCreationTimestamp_
    , _sdsEnhancedMonitoring = mempty
    , _sdsOpenShardCount = _Nat # pOpenShardCount_
    }


-- | The encryption type used. This value is one of the following:     * @KMS@      * @NONE@
sdsEncryptionType :: Lens' StreamDescriptionSummary (Maybe EncryptionType)
sdsEncryptionType = lens _sdsEncryptionType (\ s a -> s{_sdsEncryptionType = a})

-- | The GUID for the customer-managed AWS KMS key to use for encryption. This value can be a globally unique identifier, a fully specified ARN to either an alias or a key, or an alias name prefixed by "alias/".You can also use a master key owned by Kinesis Data Streams by specifying the alias @aws/kinesis@ .     * Key ARN example: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@      * Alias ARN example: @arn:aws:kms:us-east-1:123456789012:alias/MyAliasName@      * Globally unique key ID example: @12345678-1234-1234-1234-123456789012@      * Alias name example: @alias/MyAliasName@      * Master key owned by Kinesis Data Streams: @alias/aws/kinesis@
sdsKeyId :: Lens' StreamDescriptionSummary (Maybe Text)
sdsKeyId = lens _sdsKeyId (\ s a -> s{_sdsKeyId = a})

-- | The name of the stream being described.
sdsStreamName :: Lens' StreamDescriptionSummary Text
sdsStreamName = lens _sdsStreamName (\ s a -> s{_sdsStreamName = a})

-- | The Amazon Resource Name (ARN) for the stream being described.
sdsStreamARN :: Lens' StreamDescriptionSummary Text
sdsStreamARN = lens _sdsStreamARN (\ s a -> s{_sdsStreamARN = a})

-- | The current status of the stream being described. The stream status is one of the following states:     * @CREATING@ - The stream is being created. Kinesis Data Streams immediately returns and sets @StreamStatus@ to @CREATING@ .     * @DELETING@ - The stream is being deleted. The specified stream is in the @DELETING@ state until Kinesis Data Streams completes the deletion.     * @ACTIVE@ - The stream exists and is ready for read and write operations or deletion. You should perform read and write operations only on an @ACTIVE@ stream.     * @UPDATING@ - Shards in the stream are being merged or split. Read and write operations continue to work while the stream is in the @UPDATING@ state.
sdsStreamStatus :: Lens' StreamDescriptionSummary StreamStatus
sdsStreamStatus = lens _sdsStreamStatus (\ s a -> s{_sdsStreamStatus = a})

-- | The current retention period, in hours.
sdsRetentionPeriodHours :: Lens' StreamDescriptionSummary Natural
sdsRetentionPeriodHours = lens _sdsRetentionPeriodHours (\ s a -> s{_sdsRetentionPeriodHours = a}) . _Nat

-- | The approximate time that the stream was created.
sdsStreamCreationTimestamp :: Lens' StreamDescriptionSummary UTCTime
sdsStreamCreationTimestamp = lens _sdsStreamCreationTimestamp (\ s a -> s{_sdsStreamCreationTimestamp = a}) . _Time

-- | Represents the current enhanced monitoring settings of the stream.
sdsEnhancedMonitoring :: Lens' StreamDescriptionSummary [EnhancedMetrics]
sdsEnhancedMonitoring = lens _sdsEnhancedMonitoring (\ s a -> s{_sdsEnhancedMonitoring = a}) . _Coerce

-- | The number of open shards in the stream.
sdsOpenShardCount :: Lens' StreamDescriptionSummary Natural
sdsOpenShardCount = lens _sdsOpenShardCount (\ s a -> s{_sdsOpenShardCount = a}) . _Nat

instance FromJSON StreamDescriptionSummary where
        parseJSON
          = withObject "StreamDescriptionSummary"
              (\ x ->
                 StreamDescriptionSummary' <$>
                   (x .:? "EncryptionType") <*> (x .:? "KeyId") <*>
                     (x .: "StreamName")
                     <*> (x .: "StreamARN")
                     <*> (x .: "StreamStatus")
                     <*> (x .: "RetentionPeriodHours")
                     <*> (x .: "StreamCreationTimestamp")
                     <*> (x .:? "EnhancedMonitoring" .!= mempty)
                     <*> (x .: "OpenShardCount"))

instance Hashable StreamDescriptionSummary where

instance NFData StreamDescriptionSummary where

-- | Metadata assigned to the stream, consisting of a key-value pair.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - An optional string, typically used to describe or define the tag. Maximum length: 256 characters. Valid characters: Unicode letters, digits, white space, _ . / = + - % @
--
-- * 'tagKey' - A unique identifier for the tag. Maximum length: 128 characters. Valid characters: Unicode letters, digits, white space, _ . / = + - % @
tag
    :: Text -- ^ 'tagKey'
    -> Tag
tag pKey_ = Tag' {_tagValue = Nothing, _tagKey = pKey_}


-- | An optional string, typically used to describe or define the tag. Maximum length: 256 characters. Valid characters: Unicode letters, digits, white space, _ . / = + - % @
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | A unique identifier for the tag. Maximum length: 128 characters. Valid characters: Unicode letters, digits, white space, _ . / = + - % @
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "Value") <*> (x .: "Key"))

instance Hashable Tag where

instance NFData Tag where

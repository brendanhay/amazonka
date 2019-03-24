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

-- | An object that represents the details of the consumer you registered.
--
--
--
-- /See:/ 'consumer' smart constructor.
data Consumer = Consumer'
  { _cConsumerName              :: !Text
  , _cConsumerARN               :: !Text
  , _cConsumerStatus            :: !ConsumerStatus
  , _cConsumerCreationTimestamp :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Consumer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cConsumerName' - The name of the consumer is something you choose when you register the consumer.
--
-- * 'cConsumerARN' - When you register a consumer, Kinesis Data Streams generates an ARN for it. You need this ARN to be able to call 'SubscribeToShard' . If you delete a consumer and then create a new one with the same name, it won't have the same ARN. That's because consumer ARNs contain the creation timestamp. This is important to keep in mind if you have IAM policies that reference consumer ARNs.
--
-- * 'cConsumerStatus' - A consumer can't read data while in the @CREATING@ or @DELETING@ states.
--
-- * 'cConsumerCreationTimestamp' -
consumer
    :: Text -- ^ 'cConsumerName'
    -> Text -- ^ 'cConsumerARN'
    -> ConsumerStatus -- ^ 'cConsumerStatus'
    -> UTCTime -- ^ 'cConsumerCreationTimestamp'
    -> Consumer
consumer pConsumerName_ pConsumerARN_ pConsumerStatus_ pConsumerCreationTimestamp_ =
  Consumer'
    { _cConsumerName = pConsumerName_
    , _cConsumerARN = pConsumerARN_
    , _cConsumerStatus = pConsumerStatus_
    , _cConsumerCreationTimestamp = _Time # pConsumerCreationTimestamp_
    }


-- | The name of the consumer is something you choose when you register the consumer.
cConsumerName :: Lens' Consumer Text
cConsumerName = lens _cConsumerName (\ s a -> s{_cConsumerName = a})

-- | When you register a consumer, Kinesis Data Streams generates an ARN for it. You need this ARN to be able to call 'SubscribeToShard' . If you delete a consumer and then create a new one with the same name, it won't have the same ARN. That's because consumer ARNs contain the creation timestamp. This is important to keep in mind if you have IAM policies that reference consumer ARNs.
cConsumerARN :: Lens' Consumer Text
cConsumerARN = lens _cConsumerARN (\ s a -> s{_cConsumerARN = a})

-- | A consumer can't read data while in the @CREATING@ or @DELETING@ states.
cConsumerStatus :: Lens' Consumer ConsumerStatus
cConsumerStatus = lens _cConsumerStatus (\ s a -> s{_cConsumerStatus = a})

-- |
cConsumerCreationTimestamp :: Lens' Consumer UTCTime
cConsumerCreationTimestamp = lens _cConsumerCreationTimestamp (\ s a -> s{_cConsumerCreationTimestamp = a}) . _Time

instance FromJSON Consumer where
        parseJSON
          = withObject "Consumer"
              (\ x ->
                 Consumer' <$>
                   (x .: "ConsumerName") <*> (x .: "ConsumerARN") <*>
                     (x .: "ConsumerStatus")
                     <*> (x .: "ConsumerCreationTimestamp"))

instance Hashable Consumer where

instance NFData Consumer where

-- | An object that represents the details of a registered consumer.
--
--
--
-- /See:/ 'consumerDescription' smart constructor.
data ConsumerDescription = ConsumerDescription'
  { _cdConsumerName              :: !Text
  , _cdConsumerARN               :: !Text
  , _cdConsumerStatus            :: !ConsumerStatus
  , _cdConsumerCreationTimestamp :: !POSIX
  , _cdStreamARN                 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConsumerDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdConsumerName' - The name of the consumer is something you choose when you register the consumer.
--
-- * 'cdConsumerARN' - When you register a consumer, Kinesis Data Streams generates an ARN for it. You need this ARN to be able to call 'SubscribeToShard' . If you delete a consumer and then create a new one with the same name, it won't have the same ARN. That's because consumer ARNs contain the creation timestamp. This is important to keep in mind if you have IAM policies that reference consumer ARNs.
--
-- * 'cdConsumerStatus' - A consumer can't read data while in the @CREATING@ or @DELETING@ states.
--
-- * 'cdConsumerCreationTimestamp' -
--
-- * 'cdStreamARN' - The ARN of the stream with which you registered the consumer.
consumerDescription
    :: Text -- ^ 'cdConsumerName'
    -> Text -- ^ 'cdConsumerARN'
    -> ConsumerStatus -- ^ 'cdConsumerStatus'
    -> UTCTime -- ^ 'cdConsumerCreationTimestamp'
    -> Text -- ^ 'cdStreamARN'
    -> ConsumerDescription
consumerDescription pConsumerName_ pConsumerARN_ pConsumerStatus_ pConsumerCreationTimestamp_ pStreamARN_ =
  ConsumerDescription'
    { _cdConsumerName = pConsumerName_
    , _cdConsumerARN = pConsumerARN_
    , _cdConsumerStatus = pConsumerStatus_
    , _cdConsumerCreationTimestamp = _Time # pConsumerCreationTimestamp_
    , _cdStreamARN = pStreamARN_
    }


-- | The name of the consumer is something you choose when you register the consumer.
cdConsumerName :: Lens' ConsumerDescription Text
cdConsumerName = lens _cdConsumerName (\ s a -> s{_cdConsumerName = a})

-- | When you register a consumer, Kinesis Data Streams generates an ARN for it. You need this ARN to be able to call 'SubscribeToShard' . If you delete a consumer and then create a new one with the same name, it won't have the same ARN. That's because consumer ARNs contain the creation timestamp. This is important to keep in mind if you have IAM policies that reference consumer ARNs.
cdConsumerARN :: Lens' ConsumerDescription Text
cdConsumerARN = lens _cdConsumerARN (\ s a -> s{_cdConsumerARN = a})

-- | A consumer can't read data while in the @CREATING@ or @DELETING@ states.
cdConsumerStatus :: Lens' ConsumerDescription ConsumerStatus
cdConsumerStatus = lens _cdConsumerStatus (\ s a -> s{_cdConsumerStatus = a})

-- |
cdConsumerCreationTimestamp :: Lens' ConsumerDescription UTCTime
cdConsumerCreationTimestamp = lens _cdConsumerCreationTimestamp (\ s a -> s{_cdConsumerCreationTimestamp = a}) . _Time

-- | The ARN of the stream with which you registered the consumer.
cdStreamARN :: Lens' ConsumerDescription Text
cdStreamARN = lens _cdStreamARN (\ s a -> s{_cdStreamARN = a})

instance FromJSON ConsumerDescription where
        parseJSON
          = withObject "ConsumerDescription"
              (\ x ->
                 ConsumerDescription' <$>
                   (x .: "ConsumerName") <*> (x .: "ConsumerARN") <*>
                     (x .: "ConsumerStatus")
                     <*> (x .: "ConsumerCreationTimestamp")
                     <*> (x .: "StreamARN"))

instance Hashable ConsumerDescription where

instance NFData ConsumerDescription where

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

-- | /See:/ 'startingPosition' smart constructor.
data StartingPosition = StartingPosition'
  { _spSequenceNumber :: !(Maybe Text)
  , _spTimestamp      :: !(Maybe POSIX)
  , _spType           :: !ShardIteratorType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartingPosition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spSequenceNumber' - Undocumented member.
--
-- * 'spTimestamp' - Undocumented member.
--
-- * 'spType' - Undocumented member.
startingPosition
    :: ShardIteratorType -- ^ 'spType'
    -> StartingPosition
startingPosition pType_ =
  StartingPosition'
    {_spSequenceNumber = Nothing, _spTimestamp = Nothing, _spType = pType_}


-- | Undocumented member.
spSequenceNumber :: Lens' StartingPosition (Maybe Text)
spSequenceNumber = lens _spSequenceNumber (\ s a -> s{_spSequenceNumber = a})

-- | Undocumented member.
spTimestamp :: Lens' StartingPosition (Maybe UTCTime)
spTimestamp = lens _spTimestamp (\ s a -> s{_spTimestamp = a}) . mapping _Time

-- | Undocumented member.
spType :: Lens' StartingPosition ShardIteratorType
spType = lens _spType (\ s a -> s{_spType = a})

instance Hashable StartingPosition where

instance NFData StartingPosition where

instance ToJSON StartingPosition where
        toJSON StartingPosition'{..}
          = object
              (catMaybes
                 [("SequenceNumber" .=) <$> _spSequenceNumber,
                  ("Timestamp" .=) <$> _spTimestamp,
                  Just ("Type" .= _spType)])

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
  , _sdsConsumerCount           :: !(Maybe Nat)
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
-- * 'sdsConsumerCount' - The number of enhanced fan-out consumers registered with the stream.
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
    , _sdsConsumerCount = Nothing
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

-- | The number of enhanced fan-out consumers registered with the stream.
sdsConsumerCount :: Lens' StreamDescriptionSummary (Maybe Natural)
sdsConsumerCount = lens _sdsConsumerCount (\ s a -> s{_sdsConsumerCount = a}) . mapping _Nat

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
                     (x .:? "ConsumerCount")
                     <*> (x .: "StreamName")
                     <*> (x .: "StreamARN")
                     <*> (x .: "StreamStatus")
                     <*> (x .: "RetentionPeriodHours")
                     <*> (x .: "StreamCreationTimestamp")
                     <*> (x .:? "EnhancedMonitoring" .!= mempty)
                     <*> (x .: "OpenShardCount"))

instance Hashable StreamDescriptionSummary where

instance NFData StreamDescriptionSummary where

-- | After you call 'SubscribeToShard' , Kinesis Data Streams sends events of this type to your consumer.
--
--
--
-- /See:/ 'subscribeToShardEvent' smart constructor.
data SubscribeToShardEvent = SubscribeToShardEvent'
  { _stseRecords                    :: ![Record]
  , _stseContinuationSequenceNumber :: !Text
  , _stseMillisBehindLatest         :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubscribeToShardEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stseRecords' -
--
-- * 'stseContinuationSequenceNumber' - Use this as @StartingSequenceNumber@ in the next call to 'SubscribeToShard' .
--
-- * 'stseMillisBehindLatest' - The number of milliseconds the read records are from the tip of the stream, indicating how far behind current time the consumer is. A value of zero indicates that record processing is caught up, and there are no new records to process at this moment.
subscribeToShardEvent
    :: Text -- ^ 'stseContinuationSequenceNumber'
    -> Natural -- ^ 'stseMillisBehindLatest'
    -> SubscribeToShardEvent
subscribeToShardEvent pContinuationSequenceNumber_ pMillisBehindLatest_ =
  SubscribeToShardEvent'
    { _stseRecords = mempty
    , _stseContinuationSequenceNumber = pContinuationSequenceNumber_
    , _stseMillisBehindLatest = _Nat # pMillisBehindLatest_
    }


-- |
stseRecords :: Lens' SubscribeToShardEvent [Record]
stseRecords = lens _stseRecords (\ s a -> s{_stseRecords = a}) . _Coerce

-- | Use this as @StartingSequenceNumber@ in the next call to 'SubscribeToShard' .
stseContinuationSequenceNumber :: Lens' SubscribeToShardEvent Text
stseContinuationSequenceNumber = lens _stseContinuationSequenceNumber (\ s a -> s{_stseContinuationSequenceNumber = a})

-- | The number of milliseconds the read records are from the tip of the stream, indicating how far behind current time the consumer is. A value of zero indicates that record processing is caught up, and there are no new records to process at this moment.
stseMillisBehindLatest :: Lens' SubscribeToShardEvent Natural
stseMillisBehindLatest = lens _stseMillisBehindLatest (\ s a -> s{_stseMillisBehindLatest = a}) . _Nat

instance FromJSON SubscribeToShardEvent where
        parseJSON
          = withObject "SubscribeToShardEvent"
              (\ x ->
                 SubscribeToShardEvent' <$>
                   (x .:? "Records" .!= mempty) <*>
                     (x .: "ContinuationSequenceNumber")
                     <*> (x .: "MillisBehindLatest"))

instance Hashable SubscribeToShardEvent where

instance NFData SubscribeToShardEvent where

-- | /See:/ 'subscribeToShardEventStream' smart constructor.
data SubscribeToShardEventStream = SubscribeToShardEventStream'
  { _stsesKMSInvalidStateException  :: !(Maybe KMSInvalidStateException)
  , _stsesKMSThrottlingException    :: !(Maybe KMSThrottlingException)
  , _stsesKMSOptInRequired          :: !(Maybe KMSOptInRequired)
  , _stsesKMSNotFoundException      :: !(Maybe KMSNotFoundException)
  , _stsesKMSDisabledException      :: !(Maybe KMSDisabledException)
  , _stsesInternalFailureException  :: !(Maybe InternalFailureException)
  , _stsesResourceNotFoundException :: !(Maybe ResourceNotFoundException)
  , _stsesKMSAccessDeniedException  :: !(Maybe KMSAccessDeniedException)
  , _stsesResourceInUseException    :: !(Maybe ResourceInUseException)
  , _stsesSubscribeToShardEvent     :: !SubscribeToShardEvent
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubscribeToShardEventStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stsesKMSInvalidStateException' - Undocumented member.
--
-- * 'stsesKMSThrottlingException' - Undocumented member.
--
-- * 'stsesKMSOptInRequired' - Undocumented member.
--
-- * 'stsesKMSNotFoundException' - Undocumented member.
--
-- * 'stsesKMSDisabledException' - Undocumented member.
--
-- * 'stsesInternalFailureException' - Undocumented member.
--
-- * 'stsesResourceNotFoundException' - Undocumented member.
--
-- * 'stsesKMSAccessDeniedException' - Undocumented member.
--
-- * 'stsesResourceInUseException' - Undocumented member.
--
-- * 'stsesSubscribeToShardEvent' - Undocumented member.
subscribeToShardEventStream
    :: SubscribeToShardEvent -- ^ 'stsesSubscribeToShardEvent'
    -> SubscribeToShardEventStream
subscribeToShardEventStream pSubscribeToShardEvent_ =
  SubscribeToShardEventStream'
    { _stsesKMSInvalidStateException = Nothing
    , _stsesKMSThrottlingException = Nothing
    , _stsesKMSOptInRequired = Nothing
    , _stsesKMSNotFoundException = Nothing
    , _stsesKMSDisabledException = Nothing
    , _stsesInternalFailureException = Nothing
    , _stsesResourceNotFoundException = Nothing
    , _stsesKMSAccessDeniedException = Nothing
    , _stsesResourceInUseException = Nothing
    , _stsesSubscribeToShardEvent = pSubscribeToShardEvent_
    }


-- | Undocumented member.
stsesKMSInvalidStateException :: Lens' SubscribeToShardEventStream (Maybe KMSInvalidStateException)
stsesKMSInvalidStateException = lens _stsesKMSInvalidStateException (\ s a -> s{_stsesKMSInvalidStateException = a})

-- | Undocumented member.
stsesKMSThrottlingException :: Lens' SubscribeToShardEventStream (Maybe KMSThrottlingException)
stsesKMSThrottlingException = lens _stsesKMSThrottlingException (\ s a -> s{_stsesKMSThrottlingException = a})

-- | Undocumented member.
stsesKMSOptInRequired :: Lens' SubscribeToShardEventStream (Maybe KMSOptInRequired)
stsesKMSOptInRequired = lens _stsesKMSOptInRequired (\ s a -> s{_stsesKMSOptInRequired = a})

-- | Undocumented member.
stsesKMSNotFoundException :: Lens' SubscribeToShardEventStream (Maybe KMSNotFoundException)
stsesKMSNotFoundException = lens _stsesKMSNotFoundException (\ s a -> s{_stsesKMSNotFoundException = a})

-- | Undocumented member.
stsesKMSDisabledException :: Lens' SubscribeToShardEventStream (Maybe KMSDisabledException)
stsesKMSDisabledException = lens _stsesKMSDisabledException (\ s a -> s{_stsesKMSDisabledException = a})

-- | Undocumented member.
stsesInternalFailureException :: Lens' SubscribeToShardEventStream (Maybe InternalFailureException)
stsesInternalFailureException = lens _stsesInternalFailureException (\ s a -> s{_stsesInternalFailureException = a})

-- | Undocumented member.
stsesResourceNotFoundException :: Lens' SubscribeToShardEventStream (Maybe ResourceNotFoundException)
stsesResourceNotFoundException = lens _stsesResourceNotFoundException (\ s a -> s{_stsesResourceNotFoundException = a})

-- | Undocumented member.
stsesKMSAccessDeniedException :: Lens' SubscribeToShardEventStream (Maybe KMSAccessDeniedException)
stsesKMSAccessDeniedException = lens _stsesKMSAccessDeniedException (\ s a -> s{_stsesKMSAccessDeniedException = a})

-- | Undocumented member.
stsesResourceInUseException :: Lens' SubscribeToShardEventStream (Maybe ResourceInUseException)
stsesResourceInUseException = lens _stsesResourceInUseException (\ s a -> s{_stsesResourceInUseException = a})

-- | Undocumented member.
stsesSubscribeToShardEvent :: Lens' SubscribeToShardEventStream SubscribeToShardEvent
stsesSubscribeToShardEvent = lens _stsesSubscribeToShardEvent (\ s a -> s{_stsesSubscribeToShardEvent = a})

instance FromJSON SubscribeToShardEventStream where
        parseJSON
          = withObject "SubscribeToShardEventStream"
              (\ x ->
                 SubscribeToShardEventStream' <$>
                   (x .:? "KMSInvalidStateException") <*>
                     (x .:? "KMSThrottlingException")
                     <*> (x .:? "KMSOptInRequired")
                     <*> (x .:? "KMSNotFoundException")
                     <*> (x .:? "KMSDisabledException")
                     <*> (x .:? "InternalFailureException")
                     <*> (x .:? "ResourceNotFoundException")
                     <*> (x .:? "KMSAccessDeniedException")
                     <*> (x .:? "ResourceInUseException")
                     <*> (x .: "SubscribeToShardEvent"))

instance Hashable SubscribeToShardEventStream where

instance NFData SubscribeToShardEventStream where

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

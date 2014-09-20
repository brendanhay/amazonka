{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Kinesis.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Kinesis is a managed service that scales elastically for real-time
-- processing of streaming big data. The service takes in large streams of
-- data records that can then be consumed in real time by multiple
-- data-processing applications that can be run on Amazon EC2 instances.
module Network.AWS.Kinesis.Types
    (
    -- * Service
      Kinesis
    -- ** Errors
    , KinesisError (..)
    , _ExpiredIteratorException
    , _InvalidArgumentException
    , _KinesisClient
    , _KinesisSerializer
    , _KinesisService
    , _LimitExceededException
    , _ProvisionedThroughputExceededException
    , _ResourceInUseException
    , _ResourceNotFoundException

    -- * ShardIteratorType
    , ShardIteratorType (..)

    -- * StreamStatus
    , StreamStatus (..)

    -- * HashKeyRange
    , HashKeyRange
    , hashKeyRange
    , hkrStartingHashKey
    , hkrEndingHashKey

    -- * Record
    , Record
    , record
    , rSequenceNumber
    , rData
    , rPartitionKey

    -- * SequenceNumberRange
    , SequenceNumberRange
    , sequenceNumberRange
    , snrStartingSequenceNumber
    , snrEndingSequenceNumber

    -- * Shard
    , Shard
    , shard
    , sShardId
    , sParentShardId
    , sAdjacentParentShardId
    , sHashKeyRange
    , sSequenceNumberRange

    -- * StreamDescription
    , StreamDescription
    , streamDescription
    , sdStreamName
    , sdStreamARN
    , sdStreamStatus
    , sdShards
    , sdHasMoreShards

    -- * Tag
    , Tag
    , tag
    , tKey
    , tValue
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2013-12-02@) of the
-- @Amazon Kinesis@ service.
data Kinesis deriving (Typeable)

instance AWSService Kinesis where
    type Sg Kinesis = V4
    type Er Kinesis = KinesisError

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "kinesis"
        , _svcVersion  = "2013-12-02"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'Kinesis' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data KinesisError
      -- | The provided iterator exceeds the maximum age allowed.
    = ExpiredIteratorException
        { _eieMessage :: Maybe Text
        }
      -- | A specified parameter exceeds its restrictions, is not supported,
      -- or can't be used. For more information, see the returned message.
    | InvalidArgumentException
        { _iaeMessage :: Maybe Text
        }
    | KinesisClient HttpException
    | KinesisSerializer String
    | KinesisService String
      -- | The requested resource exceeds the maximum number allowed, or the
      -- number of concurrent stream requests exceeds the maximum number
      -- allowed (5).
    | LimitExceededException
        { _leeMessage :: Maybe Text
        }
      -- | The request rate is too high, or the requested data is too large
      -- for the available throughput. Reduce the frequency or size of
      -- your requests. For more information, see Error Retries and
      -- Exponential Backoff in AWS in the AWS General Reference.
    | ProvisionedThroughputExceededException
        { _pteeMessage :: Maybe Text
        }
      -- | The resource is not available for this operation. For example,
      -- you attempted to split a shard but the stream is not in the
      -- ACTIVE state.
    | ResourceInUseException
        { _riueMessage :: Maybe Text
        }
      -- | The requested resource could not be found. It might not be
      -- specified correctly, or it might not be in the ACTIVE state.
    | ResourceNotFoundException
        { _rnfeMessage :: Maybe Text
        }
      deriving (Show, Typeable, Generic)

instance AWSError KinesisError where
    awsError = const "KinesisError"

instance AWSServiceError KinesisError where
    serviceError    = KinesisService
    clientError     = KinesisClient
    serializerError = KinesisSerializer

instance Exception KinesisError

-- | The provided iterator exceeds the maximum age allowed.
--
-- See: 'ExpiredIteratorException'
_ExpiredIteratorException :: Prism' KinesisError (Maybe Text)
_ExpiredIteratorException = prism
    ExpiredIteratorException
    (\case
        ExpiredIteratorException p1 -> Right p1
        x -> Left x)

-- | A specified parameter exceeds its restrictions, is not supported, or can't
-- be used. For more information, see the returned message.
--
-- See: 'InvalidArgumentException'
_InvalidArgumentException :: Prism' KinesisError (Maybe Text)
_InvalidArgumentException = prism
    InvalidArgumentException
    (\case
        InvalidArgumentException p1 -> Right p1
        x -> Left x)

-- | See: 'KinesisClient'
_KinesisClient :: Prism' KinesisError HttpException
_KinesisClient = prism
    KinesisClient
    (\case
        KinesisClient p1 -> Right p1
        x -> Left x)

-- | See: 'KinesisSerializer'
_KinesisSerializer :: Prism' KinesisError String
_KinesisSerializer = prism
    KinesisSerializer
    (\case
        KinesisSerializer p1 -> Right p1
        x -> Left x)

-- | See: 'KinesisService'
_KinesisService :: Prism' KinesisError String
_KinesisService = prism
    KinesisService
    (\case
        KinesisService p1 -> Right p1
        x -> Left x)

-- | The requested resource exceeds the maximum number allowed, or the number of
-- concurrent stream requests exceeds the maximum number allowed (5).
--
-- See: 'LimitExceededException'
_LimitExceededException :: Prism' KinesisError (Maybe Text)
_LimitExceededException = prism
    LimitExceededException
    (\case
        LimitExceededException p1 -> Right p1
        x -> Left x)

-- | The request rate is too high, or the requested data is too large for the
-- available throughput. Reduce the frequency or size of your requests. For
-- more information, see Error Retries and Exponential Backoff in AWS in the
-- AWS General Reference.
--
-- See: 'ProvisionedThroughputExceededException'
_ProvisionedThroughputExceededException :: Prism' KinesisError (Maybe Text)
_ProvisionedThroughputExceededException = prism
    ProvisionedThroughputExceededException
    (\case
        ProvisionedThroughputExceededException p1 -> Right p1
        x -> Left x)

-- | The resource is not available for this operation. For example, you
-- attempted to split a shard but the stream is not in the ACTIVE state.
--
-- See: 'ResourceInUseException'
_ResourceInUseException :: Prism' KinesisError (Maybe Text)
_ResourceInUseException = prism
    ResourceInUseException
    (\case
        ResourceInUseException p1 -> Right p1
        x -> Left x)

-- | The requested resource could not be found. It might not be specified
-- correctly, or it might not be in the ACTIVE state.
--
-- See: 'ResourceNotFoundException'
_ResourceNotFoundException :: Prism' KinesisError (Maybe Text)
_ResourceNotFoundException = prism
    ResourceNotFoundException
    (\case
        ResourceNotFoundException p1 -> Right p1
        x -> Left x)

data ShardIteratorType
    = ShardIteratorTypeAfterSequenceNumber -- ^ AFTER_SEQUENCE_NUMBER
    | ShardIteratorTypeAtSequenceNumber -- ^ AT_SEQUENCE_NUMBER
    | ShardIteratorTypeLatest -- ^ LATEST
    | ShardIteratorTypeTrimHorizon -- ^ TRIM_HORIZON
      deriving (Eq, Ord, Show, Generic)

instance Hashable ShardIteratorType

instance FromText ShardIteratorType where
    parser = match "AFTER_SEQUENCE_NUMBER" ShardIteratorTypeAfterSequenceNumber
         <|> match "AT_SEQUENCE_NUMBER" ShardIteratorTypeAtSequenceNumber
         <|> match "LATEST" ShardIteratorTypeLatest
         <|> match "TRIM_HORIZON" ShardIteratorTypeTrimHorizon

instance ToText ShardIteratorType where
    toText ShardIteratorTypeAfterSequenceNumber = "AFTER_SEQUENCE_NUMBER"
    toText ShardIteratorTypeAtSequenceNumber = "AT_SEQUENCE_NUMBER"
    toText ShardIteratorTypeLatest = "LATEST"
    toText ShardIteratorTypeTrimHorizon = "TRIM_HORIZON"

instance ToByteString ShardIteratorType where
    toBS ShardIteratorTypeAfterSequenceNumber = "AFTER_SEQUENCE_NUMBER"
    toBS ShardIteratorTypeAtSequenceNumber = "AT_SEQUENCE_NUMBER"
    toBS ShardIteratorTypeLatest = "LATEST"
    toBS ShardIteratorTypeTrimHorizon = "TRIM_HORIZON"

instance ToHeader ShardIteratorType where
    toHeader k = toHeader k . toBS

instance ToQuery ShardIteratorType where
    toQuery = toQuery . toBS

instance ToJSON ShardIteratorType

data StreamStatus
    = StreamStatusActive -- ^ ACTIVE
    | StreamStatusCreating -- ^ CREATING
    | StreamStatusDeleting -- ^ DELETING
    | StreamStatusUpdating -- ^ UPDATING
      deriving (Eq, Ord, Show, Generic)

instance Hashable StreamStatus

instance FromText StreamStatus where
    parser = match "ACTIVE" StreamStatusActive
         <|> match "CREATING" StreamStatusCreating
         <|> match "DELETING" StreamStatusDeleting
         <|> match "UPDATING" StreamStatusUpdating

instance ToText StreamStatus where
    toText StreamStatusActive = "ACTIVE"
    toText StreamStatusCreating = "CREATING"
    toText StreamStatusDeleting = "DELETING"
    toText StreamStatusUpdating = "UPDATING"

instance ToByteString StreamStatus where
    toBS StreamStatusActive = "ACTIVE"
    toBS StreamStatusCreating = "CREATING"
    toBS StreamStatusDeleting = "DELETING"
    toBS StreamStatusUpdating = "UPDATING"

instance ToHeader StreamStatus where
    toHeader k = toHeader k . toBS

instance ToQuery StreamStatus where
    toQuery = toQuery . toBS

instance FromJSON StreamStatus

-- | The range of possible hash key values for the shard, which is a set of
-- ordered contiguous positive integers.
data HashKeyRange = HashKeyRange
    { _hkrStartingHashKey :: Text
    , _hkrEndingHashKey :: Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'HashKeyRange' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StartingHashKey ::@ @Text@
--
-- * @EndingHashKey ::@ @Text@
--
hashKeyRange :: Text -- ^ 'hkrStartingHashKey'
             -> Text -- ^ 'hkrEndingHashKey'
             -> HashKeyRange
hashKeyRange p1 p2 = HashKeyRange
    { _hkrStartingHashKey = p1
    , _hkrEndingHashKey = p2
    }

-- | The starting hash key of the hash key range.
hkrStartingHashKey :: Lens' HashKeyRange Text
hkrStartingHashKey =
    lens _hkrStartingHashKey (\s a -> s { _hkrStartingHashKey = a })

-- | The ending hash key of the hash key range.
hkrEndingHashKey :: Lens' HashKeyRange Text
hkrEndingHashKey =
    lens _hkrEndingHashKey (\s a -> s { _hkrEndingHashKey = a })

instance FromJSON HashKeyRange

instance ToJSON HashKeyRange

-- | The unit of data of the Amazon Kinesis stream, which is composed of a
-- sequence number, a partition key, and a data blob.
data Record = Record
    { _rSequenceNumber :: Text
    , _rData :: Base64
    , _rPartitionKey :: Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Record' data type.
--
-- 'Record' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SequenceNumber ::@ @Text@
--
-- * @Data ::@ @Base64@
--
-- * @PartitionKey ::@ @Text@
--
record :: Text -- ^ 'rSequenceNumber'
       -> Base64 -- ^ 'rData'
       -> Text -- ^ 'rPartitionKey'
       -> Record
record p1 p2 p3 = Record
    { _rSequenceNumber = p1
    , _rData = p2
    , _rPartitionKey = p3
    }

-- | The unique identifier for the record in the Amazon Kinesis stream.
rSequenceNumber :: Lens' Record Text
rSequenceNumber = lens _rSequenceNumber (\s a -> s { _rSequenceNumber = a })

-- | The data blob. The data in the blob is both opaque and immutable to the
-- Amazon Kinesis service, which does not inspect, interpret, or change the
-- data in the blob in any way. The maximum size of the data blob (the payload
-- after Base64-decoding) is 50 kilobytes (KB).
rData :: Lens' Record Base64
rData = lens _rData (\s a -> s { _rData = a })

-- | Identifies which shard in the stream the data record is assigned to.
rPartitionKey :: Lens' Record Text
rPartitionKey = lens _rPartitionKey (\s a -> s { _rPartitionKey = a })

instance FromJSON Record

-- | The range of possible sequence numbers for the shard.
data SequenceNumberRange = SequenceNumberRange
    { _snrStartingSequenceNumber :: Text
    , _snrEndingSequenceNumber :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SequenceNumberRange' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StartingSequenceNumber ::@ @Text@
--
-- * @EndingSequenceNumber ::@ @Maybe Text@
--
sequenceNumberRange :: Text -- ^ 'snrStartingSequenceNumber'
                    -> SequenceNumberRange
sequenceNumberRange p1 = SequenceNumberRange
    { _snrStartingSequenceNumber = p1
    , _snrEndingSequenceNumber = Nothing
    }

-- | The starting sequence number for the range.
snrStartingSequenceNumber :: Lens' SequenceNumberRange Text
snrStartingSequenceNumber =
    lens _snrStartingSequenceNumber
         (\s a -> s { _snrStartingSequenceNumber = a })

-- | The ending sequence number for the range. Shards that are in the OPEN state
-- have an ending sequence number of null.
snrEndingSequenceNumber :: Lens' SequenceNumberRange (Maybe Text)
snrEndingSequenceNumber =
    lens _snrEndingSequenceNumber
         (\s a -> s { _snrEndingSequenceNumber = a })

instance FromJSON SequenceNumberRange

instance ToJSON SequenceNumberRange

-- | A uniquely identified group of data records in an Amazon Kinesis stream.
data Shard = Shard
    { _sShardId :: Text
    , _sParentShardId :: Maybe Text
    , _sAdjacentParentShardId :: Maybe Text
    , _sHashKeyRange :: HashKeyRange
    , _sSequenceNumberRange :: SequenceNumberRange
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Shard' data type.
--
-- 'Shard' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ShardId ::@ @Text@
--
-- * @ParentShardId ::@ @Maybe Text@
--
-- * @AdjacentParentShardId ::@ @Maybe Text@
--
-- * @HashKeyRange ::@ @HashKeyRange@
--
-- * @SequenceNumberRange ::@ @SequenceNumberRange@
--
shard :: Text -- ^ 'sShardId'
      -> HashKeyRange -- ^ 'sHashKeyRange'
      -> SequenceNumberRange -- ^ 'sSequenceNumberRange'
      -> Shard
shard p1 p4 p5 = Shard
    { _sShardId = p1
    , _sParentShardId = Nothing
    , _sAdjacentParentShardId = Nothing
    , _sHashKeyRange = p4
    , _sSequenceNumberRange = p5
    }

-- | The unique identifier of the shard within the Amazon Kinesis stream.
sShardId :: Lens' Shard Text
sShardId = lens _sShardId (\s a -> s { _sShardId = a })

-- | The shard Id of the shard's parent.
sParentShardId :: Lens' Shard (Maybe Text)
sParentShardId = lens _sParentShardId (\s a -> s { _sParentShardId = a })

-- | The shard Id of the shard adjacent to the shard's parent.
sAdjacentParentShardId :: Lens' Shard (Maybe Text)
sAdjacentParentShardId =
    lens _sAdjacentParentShardId (\s a -> s { _sAdjacentParentShardId = a })

-- | The range of possible hash key values for the shard, which is a set of
-- ordered contiguous positive integers.
sHashKeyRange :: Lens' Shard HashKeyRange
sHashKeyRange = lens _sHashKeyRange (\s a -> s { _sHashKeyRange = a })

-- | The range of possible sequence numbers for the shard.
sSequenceNumberRange :: Lens' Shard SequenceNumberRange
sSequenceNumberRange =
    lens _sSequenceNumberRange (\s a -> s { _sSequenceNumberRange = a })

instance FromJSON Shard

-- | The current status of the stream, the stream ARN, an array of shard objects
-- that comprise the stream, and states whether there are more shards
-- available.
data StreamDescription = StreamDescription
    { _sdStreamName :: Text
    , _sdStreamARN :: Text
    , _sdStreamStatus :: StreamStatus
    , _sdShards :: [Shard]
    , _sdHasMoreShards :: !Bool
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'StreamDescription' data type.
--
-- 'StreamDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StreamName ::@ @Text@
--
-- * @StreamARN ::@ @Text@
--
-- * @StreamStatus ::@ @StreamStatus@
--
-- * @Shards ::@ @[Shard]@
--
-- * @HasMoreShards ::@ @Bool@
--
streamDescription :: Text -- ^ 'sdStreamName'
                  -> Text -- ^ 'sdStreamARN'
                  -> StreamStatus -- ^ 'sdStreamStatus'
                  -> [Shard] -- ^ 'sdShards'
                  -> Bool -- ^ 'sdHasMoreShards'
                  -> StreamDescription
streamDescription p1 p2 p3 p4 p5 = StreamDescription
    { _sdStreamName = p1
    , _sdStreamARN = p2
    , _sdStreamStatus = p3
    , _sdShards = p4
    , _sdHasMoreShards = p5
    }

-- | The name of the stream being described.
sdStreamName :: Lens' StreamDescription Text
sdStreamName = lens _sdStreamName (\s a -> s { _sdStreamName = a })

-- | The Amazon Resource Name (ARN) for the stream being described.
sdStreamARN :: Lens' StreamDescription Text
sdStreamARN = lens _sdStreamARN (\s a -> s { _sdStreamARN = a })

-- | The current status of the stream being described. The stream status is one
-- of the following states: CREATING - The stream is being created. Amazon
-- Kinesis immediately returns and sets StreamStatus to CREATING. DELETING -
-- The stream is being deleted. The specified stream is in the DELETING state
-- until Amazon Kinesis completes the deletion. ACTIVE - The stream exists and
-- is ready for read and write operations or deletion. You should perform read
-- and write operations only on an ACTIVE stream. UPDATING - Shards in the
-- stream are being merged or split. Read and write operations continue to
-- work while the stream is in the UPDATING state.
sdStreamStatus :: Lens' StreamDescription StreamStatus
sdStreamStatus = lens _sdStreamStatus (\s a -> s { _sdStreamStatus = a })

-- | The shards that comprise the stream.
sdShards :: Lens' StreamDescription [Shard]
sdShards = lens _sdShards (\s a -> s { _sdShards = a })

-- | If set to true, more shards in the stream are available to describe.
sdHasMoreShards :: Lens' StreamDescription Bool
sdHasMoreShards = lens _sdHasMoreShards (\s a -> s { _sdHasMoreShards = a })

instance FromJSON StreamDescription

-- | Metadata assigned to the stream, consisting of a key-value pair.
data Tag = Tag
    { _tKey :: Text
    , _tValue :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tag' data type.
--
-- 'Tag' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Key ::@ @Text@
--
-- * @Value ::@ @Maybe Text@
--
tag :: Text -- ^ 'tKey'
    -> Tag
tag p1 = Tag
    { _tKey = p1
    , _tValue = Nothing
    }

-- | A unique identifier for the tag. Maximum length: 128 characters. Valid
-- characters: Unicode letters, digits, white space, _ . / = + - % @.
tKey :: Lens' Tag Text
tKey = lens _tKey (\s a -> s { _tKey = a })

-- | An optional string, typically used to describe or define the tag. Maximum
-- length: 256 characters. Valid characters: Unicode letters, digits, white
-- space, _ . / = + - % @.
tValue :: Lens' Tag (Maybe Text)
tValue = lens _tValue (\s a -> s { _tValue = a })

instance FromJSON Tag

{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Kinesis.V2013_12_02.Types
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
module Network.AWS.Kinesis.V2013_12_02.Types where

import Control.Lens.TH (makeLenses)
import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2013-12-02@) of the
-- @Amazon Kinesis@ service.
data Kinesis deriving (Typeable)

instance AWSService Kinesis where
    type Sg Kinesis = V4
    data Er Kinesis
        = ExpiredIteratorException
            { _eieMessage :: Maybe Text
            }
        | InvalidArgumentException
            { _iaeMessage :: Maybe Text
            }
        | KinesisClient HttpException
        | KinesisSerializer String
        | KinesisService String
        | LimitExceededException
            { _leeMessage :: Maybe Text
            }
        | ProvisionedThroughputExceededException
            { _pteeMessage :: Maybe Text
            }
        | ResourceInUseException
            { _riueMessage :: Maybe Text
            }
        | ResourceNotFoundException
            { _rnfeMessage :: Maybe Text
            }

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "kinesis"
        , _svcVersion  = "2013-12-02"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er Kinesis)
deriving instance Generic (Er Kinesis)

instance AWSError (Er Kinesis) where
    awsError = const "KinesisError"

instance AWSServiceError (Er Kinesis) where
    serviceError    = KinesisService
    clientError     = KinesisClient
    serializerError = KinesisSerializer

instance Exception (Er Kinesis)

-- | Determines how the shard iterator is used to start reading data records
-- from the shard. The following are the valid shard iterator types:
-- AT_SEQUENCE_NUMBER - Start reading exactly from the position denoted by a
-- specific sequence number. AFTER_SEQUENCE_NUMBER - Start reading right after
-- the position denoted by a specific sequence number. TRIM_HORIZON - Start
-- reading at the last untrimmed record in the shard in the system, which is
-- the oldest data record in the shard. LATEST - Start reading just after the
-- most recent record in the shard, so that you always read the most recent
-- data in the shard.
data ShardIteratorType
    = ShardIteratorTypeAfterSequenceNumber -- ^ AFTER_SEQUENCE_NUMBER
    | ShardIteratorTypeAtSequenceNumber -- ^ AT_SEQUENCE_NUMBER
    | ShardIteratorTypeLatest -- ^ LATEST
    | ShardIteratorTypeTrimHorizon -- ^ TRIM_HORIZON
      deriving (Eq, Show, Generic)

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

-- | The current status of the stream being described. The stream status is one
-- of the following states: CREATING - The stream is being created. Upon
-- receiving a CreateStream request, Amazon Kinesis immediately returns and
-- sets StreamStatus to CREATING. DELETING - The stream is being deleted.
-- After a DeleteStream request, the specified stream is in the DELETING state
-- until Amazon Kinesis completes the deletion. ACTIVE - The stream exists and
-- is ready for read and write operations or deletion. You should perform read
-- and write operations only on an ACTIVE stream. UPDATING - Shards in the
-- stream are being merged or split. Read and write operations continue to
-- work while the stream is in the UPDATING state.
data StreamStatus
    = StreamStatusActive -- ^ ACTIVE
    | StreamStatusCreating -- ^ CREATING
    | StreamStatusDeleting -- ^ DELETING
    | StreamStatusUpdating -- ^ UPDATING
      deriving (Eq, Show, Generic)

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
    { _hkrEndingHashKey :: Text
      -- ^ The ending hash key of the hash key range.
    , _hkrStartingHashKey :: Text
      -- ^ The starting hash key of the hash key range.
    } deriving (Show, Generic)

instance FromJSON HashKeyRange

instance ToJSON HashKeyRange

-- | The unit of data of the Amazon Kinesis stream, which is composed of a
-- sequence number, a partition key, and a data blob.
data Record = Record
    { _rSequenceNumber :: Text
      -- ^ The unique identifier for the record in the Amazon Kinesis
      -- stream.
    , _rPartitionKey :: Text
      -- ^ Identifies which shard in the stream the data record is assigned
      -- to.
    , _rData :: Base64
      -- ^ The data blob. The data in the blob is both opaque and immutable
      -- to the Amazon Kinesis service, which does not inspect, interpret,
      -- or change the data in the blob in any way. The maximum size of
      -- the data blob (the payload after Base64-decoding) is 50 kilobytes
      -- (KB).
    } deriving (Show, Generic)

instance FromJSON Record

-- | The range of possible sequence numbers for the shard.
data SequenceNumberRange = SequenceNumberRange
    { _snrStartingSequenceNumber :: Text
      -- ^ The starting sequence number for the range.
    , _snrEndingSequenceNumber :: Maybe Text
      -- ^ The ending sequence number for the range. Shards that are in the
      -- OPEN state have an ending sequence number of null.
    } deriving (Show, Generic)

instance FromJSON SequenceNumberRange

instance ToJSON SequenceNumberRange

-- | A uniquely identified group of data records in an Amazon Kinesis stream.
data Shard = Shard
    { _sAdjacentParentShardId :: Maybe Text
      -- ^ The shard Id of the shard adjacent to the shard's parent.
    , _sHashKeyRange :: HashKeyRange
      -- ^ The range of possible hash key values for the shard, which is a
      -- set of ordered contiguous positive integers.
    , _sParentShardId :: Maybe Text
      -- ^ The shard Id of the shard's parent.
    , _sSequenceNumberRange :: SequenceNumberRange
      -- ^ The range of possible sequence numbers for the shard.
    , _sShardId :: Text
      -- ^ The unique identifier of the shard within the Amazon Kinesis
      -- stream.
    } deriving (Show, Generic)

instance FromJSON Shard

-- | Contains the current status of the stream, the stream ARN, an array of
-- shard objects that comprise the stream, and states whether there are more
-- shards available.
data StreamDescription = StreamDescription
    { _sdStreamStatus :: StreamStatus
      -- ^ The current status of the stream being described. The stream
      -- status is one of the following states: CREATING - The stream is
      -- being created. Upon receiving a CreateStream request, Amazon
      -- Kinesis immediately returns and sets StreamStatus to CREATING.
      -- DELETING - The stream is being deleted. After a DeleteStream
      -- request, the specified stream is in the DELETING state until
      -- Amazon Kinesis completes the deletion. ACTIVE - The stream exists
      -- and is ready for read and write operations or deletion. You
      -- should perform read and write operations only on an ACTIVE
      -- stream. UPDATING - Shards in the stream are being merged or
      -- split. Read and write operations continue to work while the
      -- stream is in the UPDATING state.
    , _sdHasMoreShards :: Bool
      -- ^ If set to true there are more shards in the stream available to
      -- describe.
    , _sdStreamARN :: Text
      -- ^ The Amazon Resource Name (ARN) for the stream being described.
    , _sdShards :: [Shard]
      -- ^ The shards that comprise the stream.
    , _sdStreamName :: Text
      -- ^ The name of the stream being described.
    } deriving (Show, Generic)

instance FromJSON StreamDescription

-- Newtypes

-- Products
makeLenses ''HashKeyRange
makeLenses ''Record
makeLenses ''SequenceNumberRange
makeLenses ''Shard
makeLenses ''StreamDescription

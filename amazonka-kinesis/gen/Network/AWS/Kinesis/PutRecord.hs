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

-- Module      : Network.AWS.Kinesis.PutRecord
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Puts a data record from a producer into an Amazon Kinesis stream. You must
-- call PutRecord to send data from the producer into the Amazon Kinesis
-- stream for real-time ingestion and subsequent processing. You must specify
-- the name of the stream that captures, stores, and transports the data; a
-- partition key; and the data blob itself. The data blob could be a segment
-- from a log file, geographic/location data, website clickstream data, or any
-- other data type. The partition key is used to distribute data across
-- shards. Amazon Kinesis segregates the data records that belong to a data
-- stream into multiple shards, using the partition key associated with each
-- data record to determine which shard a given data record belongs to.
-- Partition keys are Unicode strings, with a maximum length limit of 256
-- bytes. An MD5 hash function is used to map partition keys to 128-bit
-- integer values and to map associated data records to shards using the hash
-- key ranges of the shards. You can override hashing the partition key to
-- determine the shard by explicitly specifying a hash value using the
-- ExplicitHashKey parameter. For more information, see
-- <http://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-api-java.html#kinesis-using-api-defn-partition-key
-- Partition Key> in the /Amazon Kinesis Developer Guide/. PutRecord returns
-- the shard ID of where the data record was placed and the sequence number
-- that was assigned to the data record. Sequence numbers generally increase
-- over time. To guarantee strictly increasing ordering, use the
-- SequenceNumberForOrdering parameter. For more information, see
-- <http://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-api-java.html#kinesis-using-api-defn-sequence-number
-- Sequence Number> in the /Amazon Kinesis Developer Guide/. If a PutRecord
-- request cannot be processed because of insufficient provisioned throughput
-- on the shard involved in the request, PutRecord throws
-- ProvisionedThroughputExceededException. Data records are accessible for
-- only 24 hours from the time that they are added to an Amazon Kinesis
-- stream.
--
-- <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_PutRecord.html>
module Network.AWS.Kinesis.PutRecord
    (
    -- * Request
      PutRecord
    -- ** Request constructor
    , putRecord
    -- ** Request lenses
    , prData
    , prExplicitHashKey
    , prPartitionKey
    , prSequenceNumberForOrdering
    , prStreamName

    -- * Response
    , PutRecordResponse
    -- ** Response constructor
    , putRecordResponse
    -- ** Response lenses
    , prrSequenceNumber
    , prrShardId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Kinesis.Types
import qualified GHC.Exts

data PutRecord = PutRecord
    { _prData                      :: Base64
    , _prExplicitHashKey           :: Maybe Text
    , _prPartitionKey              :: Text
    , _prSequenceNumberForOrdering :: Maybe Text
    , _prStreamName                :: Text
    } deriving (Eq, Show)

-- | 'PutRecord' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prData' @::@ 'Base64'
--
-- * 'prExplicitHashKey' @::@ 'Maybe' 'Text'
--
-- * 'prPartitionKey' @::@ 'Text'
--
-- * 'prSequenceNumberForOrdering' @::@ 'Maybe' 'Text'
--
-- * 'prStreamName' @::@ 'Text'
--
putRecord :: Text -- ^ 'prStreamName'
          -> Base64 -- ^ 'prData'
          -> Text -- ^ 'prPartitionKey'
          -> PutRecord
putRecord p1 p2 p3 = PutRecord
    { _prStreamName                = p1
    , _prData                      = p2
    , _prPartitionKey              = p3
    , _prExplicitHashKey           = Nothing
    , _prSequenceNumberForOrdering = Nothing
    }

-- | The data blob to put into the record, which is Base64-encoded when the
-- blob is serialized. The maximum size of the data blob (the payload before
-- Base64-encoding) is 50 kilobytes (KB).
prData :: Lens' PutRecord Base64
prData = lens _prData (\s a -> s { _prData = a })

-- | The hash value used to explicitly determine the shard the data record is
-- assigned to by overriding the partition key hash.
prExplicitHashKey :: Lens' PutRecord (Maybe Text)
prExplicitHashKey =
    lens _prExplicitHashKey (\s a -> s { _prExplicitHashKey = a })

-- | Determines which shard in the stream the data record is assigned to.
-- Partition keys are Unicode strings with a maximum length limit of 256
-- bytes. Amazon Kinesis uses the partition key as input to a hash function
-- that maps the partition key and associated data to a specific shard.
-- Specifically, an MD5 hash function is used to map partition keys to
-- 128-bit integer values and to map associated data records to shards. As a
-- result of this hashing mechanism, all data records with the same
-- partition key will map to the same shard within the stream.
prPartitionKey :: Lens' PutRecord Text
prPartitionKey = lens _prPartitionKey (\s a -> s { _prPartitionKey = a })

-- | Guarantees strictly increasing sequence numbers, for puts from the same
-- client and to the same partition key. Usage: set the
-- SequenceNumberForOrdering of record /n/ to the sequence number of record
-- /n-1/ (as returned in the PutRecordResult> when putting record /n-1/). If
-- this parameter is not set, records will be coarsely ordered based on
-- arrival time.
prSequenceNumberForOrdering :: Lens' PutRecord (Maybe Text)
prSequenceNumberForOrdering =
    lens _prSequenceNumberForOrdering
        (\s a -> s { _prSequenceNumberForOrdering = a })

-- | The name of the stream to put the data record into.
prStreamName :: Lens' PutRecord Text
prStreamName = lens _prStreamName (\s a -> s { _prStreamName = a })

data PutRecordResponse = PutRecordResponse
    { _prrSequenceNumber :: Text
    , _prrShardId        :: Text
    } deriving (Eq, Ord, Show)

-- | 'PutRecordResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prrSequenceNumber' @::@ 'Text'
--
-- * 'prrShardId' @::@ 'Text'
--
putRecordResponse :: Text -- ^ 'prrShardId'
                  -> Text -- ^ 'prrSequenceNumber'
                  -> PutRecordResponse
putRecordResponse p1 p2 = PutRecordResponse
    { _prrShardId        = p1
    , _prrSequenceNumber = p2
    }

-- | The sequence number identifier that was assigned to the put data record.
-- The sequence number for the record is unique across all records in the
-- stream. A sequence number is the identifier associated with every record
-- put into the stream.
prrSequenceNumber :: Lens' PutRecordResponse Text
prrSequenceNumber =
    lens _prrSequenceNumber (\s a -> s { _prrSequenceNumber = a })

-- | The shard ID of the shard where the data record was placed.
prrShardId :: Lens' PutRecordResponse Text
prrShardId = lens _prrShardId (\s a -> s { _prrShardId = a })

instance ToPath PutRecord where
    toPath = const "/"

instance ToQuery PutRecord where
    toQuery = const mempty

instance ToHeaders PutRecord

instance ToJSON PutRecord where
    toJSON PutRecord{..} = object
        [ "StreamName"                .= _prStreamName
        , "Data"                      .= _prData
        , "PartitionKey"              .= _prPartitionKey
        , "ExplicitHashKey"           .= _prExplicitHashKey
        , "SequenceNumberForOrdering" .= _prSequenceNumberForOrdering
        ]

instance AWSRequest PutRecord where
    type Sv PutRecord = Kinesis
    type Rs PutRecord = PutRecordResponse

    request  = post "PutRecord"
    response = jsonResponse

instance FromJSON PutRecordResponse where
    parseJSON = withObject "PutRecordResponse" $ \o -> PutRecordResponse
        <$> o .:  "SequenceNumber"
        <*> o .:  "ShardId"

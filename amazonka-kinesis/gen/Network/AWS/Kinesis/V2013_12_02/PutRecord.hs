{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Kinesis.V2013_12_02.PutRecord
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation puts a data record into an Amazon Kinesis stream from a
-- producer. This operation must be called to send data from the producer into
-- the Amazon Kinesis stream for real-time ingestion and subsequent
-- processing. The PutRecord operation requires the name of the stream that
-- captures, stores, and transports the data; a partition key; and the data
-- blob itself. The data blob could be a segment from a log file,
-- geographic/location data, website clickstream data, or any other data type.
-- The partition key is used to distribute data across shards. Amazon Kinesis
-- segregates the data records that belong to a data stream into multiple
-- shards, using the partition key associated with each data record to
-- determine which shard a given data record belongs to. Partition keys are
-- Unicode strings, with a maximum length limit of 256 bytes. An MD5 hash
-- function is used to map partition keys to 128-bit integer values and to map
-- associated data records to shards using the hash key ranges of the shards.
-- You can override hashing the partition key to determine the shard by
-- explicitly specifying a hash value using the ExplicitHashKey parameter. For
-- more information, see the Amazon Kinesis Developer Guide. PutRecord returns
-- the shard ID of where the data record was placed and the sequence number
-- that was assigned to the data record. Sequence numbers generally increase
-- over time. To guarantee strictly increasing ordering, use the
-- SequenceNumberForOrdering parameter. For more information, see the Amazon
-- Kinesis Developer Guide. If a PutRecord request cannot be processed because
-- of insufficient provisioned throughput on the shard involved in the
-- request, PutRecord throws ProvisionedThroughputExceededException. Data
-- records are accessible for only 24 hours from the time that they are added
-- to an Amazon Kinesis stream. Add Data to a Stream The following is an
-- example of an Amazon Kinesis PutRecord request and response. POST /
-- HTTP/1.1 Host: kinesis.. x-amz-Date: Authorization: AWS4-HMAC-SHA256
-- Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.PutRecord { "StreamName": "exampleStreamName", "Data":
-- "XzxkYXRhPl8x", "PartitionKey": "partitionKey" } HTTP/1.1 200 OK
-- x-amzn-RequestId: Content-Type: application/x-amz-json-1.1 Content-Length:
-- Date: ]]> { "SequenceNumber": "21269319989653637946712965403778482177",
-- "ShardId": "shardId-000000000001" }.
module Network.AWS.Kinesis.V2013_12_02.PutRecord
    (
    -- * Request
      PutRecord
    -- ** Request constructor
    , mkPutRecord
    -- ** Request lenses
    , prStreamName
    , prData
    , prPartitionKey
    , prExplicitHashKey
    , prSequenceNumberForOrdering

    -- * Response
    , PutRecordResponse
    -- ** Response constructor
    , mkPutRecordResponse
    -- ** Response lenses
    , prrShardId
    , prrSequenceNumber
    ) where

import Network.AWS.Kinesis.V2013_12_02.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Represents the input of a PutRecord operation.
data PutRecord = PutRecord
    { _prStreamName :: Text
    , _prData :: Base64
    , _prPartitionKey :: Text
    , _prExplicitHashKey :: Maybe Text
    , _prSequenceNumberForOrdering :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutRecord' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StreamName ::@ @Text@
--
-- * @Data ::@ @Base64@
--
-- * @PartitionKey ::@ @Text@
--
-- * @ExplicitHashKey ::@ @Maybe Text@
--
-- * @SequenceNumberForOrdering ::@ @Maybe Text@
--
mkPutRecord :: Text -- ^ 'prStreamName'
            -> Base64 -- ^ 'prData'
            -> Text -- ^ 'prPartitionKey'
            -> PutRecord
mkPutRecord p1 p2 p3 = PutRecord
    { _prStreamName = p1
    , _prData = p2
    , _prPartitionKey = p3
    , _prExplicitHashKey = Nothing
    , _prSequenceNumberForOrdering = Nothing
    }

-- | The name of the stream to put the data record into.
prStreamName :: Lens' PutRecord Text
prStreamName = lens _prStreamName (\s a -> s { _prStreamName = a })

-- | The data blob to put into the record, which is Base64-encoded when the blob
-- is serialized. The maximum size of the data blob (the payload after
-- Base64-decoding) is 50 kilobytes (KB).
prData :: Lens' PutRecord Base64
prData = lens _prData (\s a -> s { _prData = a })

-- | Determines which shard in the stream the data record is assigned to.
-- Partition keys are Unicode strings with a maximum length limit of 256
-- bytes. Amazon Kinesis uses the partition key as input to a hash function
-- that maps the partition key and associated data to a specific shard.
-- Specifically, an MD5 hash function is used to map partition keys to 128-bit
-- integer values and to map associated data records to shards. As a result of
-- this hashing mechanism, all data records with the same partition key will
-- map to the same shard within the stream.
prPartitionKey :: Lens' PutRecord Text
prPartitionKey = lens _prPartitionKey (\s a -> s { _prPartitionKey = a })

-- | The hash value used to explicitly determine the shard the data record is
-- assigned to by overriding the partition key hash.
prExplicitHashKey :: Lens' PutRecord (Maybe Text)
prExplicitHashKey =
    lens _prExplicitHashKey (\s a -> s { _prExplicitHashKey = a })

-- | Guarantees strictly increasing sequence numbers, for puts from the same
-- client and to the same partition key. Usage: set the
-- SequenceNumberForOrdering of record n to the sequence number of record n-1
-- (as returned in the PutRecordResult when putting record n-1). If this
-- parameter is not set, records will be coarsely ordered based on arrival
-- time.
prSequenceNumberForOrdering :: Lens' PutRecord (Maybe Text)
prSequenceNumberForOrdering =
    lens _prSequenceNumberForOrdering
         (\s a -> s { _prSequenceNumberForOrdering = a })

instance ToPath PutRecord

instance ToQuery PutRecord

instance ToHeaders PutRecord

instance ToJSON PutRecord

-- | Represents the output of a PutRecord operation.
data PutRecordResponse = PutRecordResponse
    { _prrShardId :: Text
    , _prrSequenceNumber :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutRecordResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ShardId ::@ @Text@
--
-- * @SequenceNumber ::@ @Text@
--
mkPutRecordResponse :: Text -- ^ 'prrShardId'
                    -> Text -- ^ 'prrSequenceNumber'
                    -> PutRecordResponse
mkPutRecordResponse p1 p2 = PutRecordResponse
    { _prrShardId = p1
    , _prrSequenceNumber = p2
    }

-- | The shard ID of the shard where the data record was placed.
prrShardId :: Lens' PutRecordResponse Text
prrShardId = lens _prrShardId (\s a -> s { _prrShardId = a })

-- | The sequence number identifier that was assigned to the put data record.
-- The sequence number for the record is unique across all records in the
-- stream. A sequence number is the identifier associated with every record
-- put into the stream.
prrSequenceNumber :: Lens' PutRecordResponse Text
prrSequenceNumber =
    lens _prrSequenceNumber (\s a -> s { _prrSequenceNumber = a })

instance FromJSON PutRecordResponse

instance AWSRequest PutRecord where
    type Sv PutRecord = Kinesis
    type Rs PutRecord = PutRecordResponse

    request = get
    response _ = jsonResponse

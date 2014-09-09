{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.Kinesis.V2013_12_02.Trans
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
--
-- The 'State' operator variants from 'Control.Lens' such as '.=' can be
-- used to modify any additional request parameters before sending.
module Network.AWS.Kinesis.V2013_12_02.Trans
    (
    -- * CreateStream
      createStream
    -- * DeleteStream
    , deleteStream
    -- * DescribeStream
    , describeStream
    -- * GetRecords
    , getRecords
    -- * GetShardIterator
    , getShardIterator
    -- * ListStreams
    , listStreams
    -- * MergeShards
    , mergeShards
    -- * PutRecord
    , putRecord
    -- * SplitShard
    , splitShard

    -- * Re-exported
    , module Control.Monad.Trans.AWS
    , module Network.AWS.Kinesis.V2013_12_02
    -- ** Lenses
    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.Kinesis.V2013_12_02

-- | This operation adds a new Amazon Kinesis stream to your AWS account. A
-- stream captures and transports data records that are continuously emitted
-- from different data sources or producers. Scale-out within an Amazon
-- Kinesis stream is explicitly supported by means of shards, which are
-- uniquely identified groups of data records in an Amazon Kinesis stream. You
-- specify and control the number of shards that a stream is composed of. Each
-- open shard can support up to 5 read transactions per second, up to a
-- maximum total of 2 MB of data read per second. Each shard can support up to
-- 1000 write transactions per second, up to a maximum total of 1 MB data
-- written per second. You can add shards to a stream if the amount of data
-- input increases and you can remove shards if the amount of data input
-- decreases. The stream name identifies the stream. The name is scoped to the
-- AWS account used by the application. It is also scoped by region. That is,
-- two streams in two different accounts can have the same name, and two
-- streams in the same account, but in two different regions, can have the
-- same name. CreateStream is an asynchronous operation. Upon receiving a
-- CreateStream request, Amazon Kinesis immediately returns and sets the
-- stream status to CREATING. After the stream is created, Amazon Kinesis sets
-- the stream status to ACTIVE. You should perform read and write operations
-- only on an ACTIVE stream. You receive a LimitExceededException when making
-- a CreateStream request if you try to do one of the following: Have more
-- than five streams in the CREATING state at any point in time. Create more
-- shards than are authorized for your account. Note: The default limit for an
-- AWS account is 10 shards per stream. If you need to create a stream with
-- more than 10 shards, contact AWS Support to increase the limit on your
-- account. You can use the DescribeStream operation to check the stream
-- status, which is returned in StreamStatus. CreateStream has a limit of 5
-- transactions per second per account. Create a Stream The following is an
-- example of an Amazon Kinesis CreateStream request and response. POST /
-- HTTP/1.1 Host: kinesis.. x-amz-Date: Authorization: AWS4-HMAC-SHA256
-- Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.CreateStream {
-- "StreamName":"exampleStreamName","ShardCount":3 } HTTP/1.1 200 OK
-- x-amzn-RequestId: Content-Type: application/x-amz-json-1.1 Content-Length:
-- Date: ]]>.
--
-- See: 'Network.AWS.Kinesis.V2013_12_02.CreateStream'
createStream :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                , AWSRequest a
                )
             => Text -- ^ 'csStreamName'
             -> Integer -- ^ 'csShardCount'
             -> State CreateStream a
             -> m CreateStreamResponse
createStream p1 p2 s =
    send $ (mkCreateStream p1 p2) &~ s

-- | This operation deletes a stream and all of its shards and data. You must
-- shut down any applications that are operating on the stream before you
-- delete the stream. If an application attempts to operate on a deleted
-- stream, it will receive the exception ResourceNotFoundException. If the
-- stream is in the ACTIVE state, you can delete it. After a DeleteStream
-- request, the specified stream is in the DELETING state until Amazon Kinesis
-- completes the deletion. Note: Amazon Kinesis might continue to accept data
-- read and write operations, such as PutRecord and GetRecords, on a stream in
-- the DELETING state until the stream deletion is complete. When you delete a
-- stream, any shards in that stream are also deleted. You can use the
-- DescribeStream operation to check the state of the stream, which is
-- returned in StreamStatus. DeleteStream has a limit of 5 transactions per
-- second per account. Delete a Stream The following is an example of an
-- Amazon Kinesis DeleteStream request and response. POST / HTTP/1.1 Host:
-- kinesis.. x-amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.DeleteStream { "StreamName":"exampleStreamName" } HTTP/1.1
-- 200 OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]>.
--
-- See: 'Network.AWS.Kinesis.V2013_12_02.DeleteStream'
deleteStream :: ( MonadCatch m
                , MonadResource m
                , MonadError AWS.Error m
                , MonadReader Env m
                , AWSRequest a
                )
             => Text -- ^ 'dsStreamName'
             -> State DeleteStream a
             -> m DeleteStreamResponse
deleteStream p1 s =
    send $ (mkDeleteStream p1) &~ s

-- | This operation returns the following information about the stream: the
-- current status of the stream, the stream Amazon Resource Name (ARN), and an
-- array of shard objects that comprise the stream. For each shard object
-- there is information about the hash key and sequence number ranges that the
-- shard spans, and the IDs of any earlier shards that played in a role in a
-- MergeShards or SplitShard operation that created the shard. A sequence
-- number is the identifier associated with every record ingested in the
-- Amazon Kinesis stream. The sequence number is assigned by the Amazon
-- Kinesis service when a record is put into the stream. You can limit the
-- number of returned shards using the Limit parameter. The number of shards
-- in a stream may be too large to return from a single call to
-- DescribeStream. You can detect this by using the HasMoreShards flag in the
-- returned output. HasMoreShards is set to true when there is more data
-- available. If there are more shards available, you can request more shards
-- by using the shard ID of the last shard returned by the DescribeStream
-- request, in the ExclusiveStartShardId parameter in a subsequent request to
-- DescribeStream. DescribeStream is a paginated operation. DescribeStream has
-- a limit of 10 transactions per second per account. Obtain Information About
-- a Stream The following is an example of an Amazon Kinesis DescribeStream
-- request and response. POST / HTTP/1.1 Host: kinesis.. x-amz-Date:
-- Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.DescribeStream { "StreamName":"exampleStreamName" }
-- HTTP/1.1 200 OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]> { "StreamDescription": { "HasMoreShards": false,
-- "Shards": [ { "HashKeyRange": { "EndingHashKey":
-- "113427455640312821154458202477256070484", "StartingHashKey": "0" },
-- "SequenceNumberRange": { "EndingSequenceNumber":
-- "21269319989741826081360214168359141376", "StartingSequenceNumber":
-- "21267647932558653966460912964485513216" }, "ShardId":
-- "shardId-000000000000" }, { "HashKeyRange": { "EndingHashKey":
-- "226854911280625642308916404954512140969", "StartingHashKey":
-- "113427455640312821154458202477256070485" }, "SequenceNumberRange": {
-- "StartingSequenceNumber": "21267647932558653966460912964485513217" },
-- "ShardId": "shardId-000000000001" }, { "HashKeyRange": { "EndingHashKey":
-- "340282366920938463463374607431768211455", "StartingHashKey":
-- "226854911280625642308916404954512140970" }, "SequenceNumberRange": {
-- "StartingSequenceNumber": "21267647932558653966460912964485513218" },
-- "ShardId": "shardId-000000000002" } ], "StreamARN":
-- "arn:aws:kinesis:us-east-1:052958737983:exampleStreamName", "StreamName":
-- "exampleStreamName", "StreamStatus": "ACTIVE" } }.
--
-- See: 'Network.AWS.Kinesis.V2013_12_02.DescribeStream'
describeStream :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env (ResumableSource m)
                  , AWSPager a
                  )
               => Text -- ^ 'ds1StreamName'
               -> State DescribeStream a
               -> ResumableSource m DescribeStreamResponse
describeStream p1 s =
    paginate $ (mkDescribeStream p1) &~ s

-- | This operation returns one or more data records from a shard. A GetRecords
-- operation request can retrieve up to 10 MB of data. You specify a shard
-- iterator for the shard that you want to read data from in the ShardIterator
-- parameter. The shard iterator specifies the position in the shard from
-- which you want to start reading data records sequentially. A shard iterator
-- specifies this position using the sequence number of a data record in the
-- shard. For more information about the shard iterator, see GetShardIterator.
-- GetRecords may return a partial result if the response size limit is
-- exceeded. You will get an error, but not a partial result if the shard's
-- provisioned throughput is exceeded, the shard iterator has expired, or an
-- internal processing failure has occurred. Clients can request a smaller
-- amount of data by specifying a maximum number of returned records using the
-- Limit parameter. The Limit parameter can be set to an integer value of up
-- to 10,000. If you set the value to an integer greater than 10,000, you will
-- receive InvalidArgumentException. A new shard iterator is returned by every
-- GetRecords request in NextShardIterator, which you use in the ShardIterator
-- parameter of the next GetRecords request. When you repeatedly read from an
-- Amazon Kinesis stream use a GetShardIterator request to get the first shard
-- iterator to use in your first GetRecords request and then use the shard
-- iterator returned in NextShardIterator for subsequent reads. GetRecords can
-- return null for the NextShardIterator to reflect that the shard has been
-- closed and that the requested shard iterator would never have returned more
-- data. If no items can be processed because of insufficient provisioned
-- throughput on the shard involved in the request, GetRecords throws
-- ProvisionedThroughputExceededException. Get Data from the Shards in a
-- Stream The following is an example of an Amazon Kinesis GetRecords request
-- and response. POST / HTTP/1.1 Host: kinesis.. x-amz-Date: Authorization:
-- AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.GetRecords { "ShardIterator":
-- "AAAAAAAAAAETYyAYzd665+8e0X7JTsASDM/Hr2rSwc0X2qz93iuA3udrjTH+ikQvpQk/1ZcMMLzRdAesqwBGPnsthzU0/CBlM/U8/8oEqGwX3pKw0XyeDNRAAZyXBo3MqkQtCpXhr942BRTjvWKhFz7OmCb2Ncfr8Tl2cBktooi6kJhr+djN5WYkB38Rr3akRgCl9qaU4dY=",
-- "Limit": 2 } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]> { "NextShardIterator":
-- "AAAAAAAAAAHsW8zCWf9164uy8Epue6WS3w6wmj4a4USt+CNvMd6uXQ+HL5vAJMznqqC0DLKsIjuoiTi1BpT6nW0LN2M2D56zM5H8anHm30Gbri9ua+qaGgj+3XTyvbhpERfrezgLHbPB/rIcVpykJbaSj5tmcXYRmFnqZBEyHwtZYFmh6hvWVFkIwLuMZLMrpWhG5r5hzkE=",
-- "Records": [ { "Data": "XzxkYXRhPl8w", "PartitionKey": "partitionKey",
-- "SequenceNumber": "21269319989652663814458848515492872193" } ] }.
--
-- See: 'Network.AWS.Kinesis.V2013_12_02.GetRecords'
getRecords :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              , AWSRequest a
              )
           => Text -- ^ 'grShardIterator'
           -> State GetRecords a
           -> m GetRecordsResponse
getRecords p1 s =
    send $ (mkGetRecords p1) &~ s

-- | This operation returns a shard iterator in ShardIterator. The shard
-- iterator specifies the position in the shard from which you want to start
-- reading data records sequentially. A shard iterator specifies this position
-- using the sequence number of a data record in a shard. A sequence number is
-- the identifier associated with every record ingested in the Amazon Kinesis
-- stream. The sequence number is assigned by the Amazon Kinesis service when
-- a record is put into the stream. You must specify the shard iterator type
-- in the GetShardIterator request. For example, you can set the
-- ShardIteratorType parameter to read exactly from the position denoted by a
-- specific sequence number by using the AT_SEQUENCE_NUMBER shard iterator
-- type, or right after the sequence number by using the AFTER_SEQUENCE_NUMBER
-- shard iterator type, using sequence numbers returned by earlier PutRecord,
-- GetRecords or DescribeStream requests. You can specify the shard iterator
-- type TRIM_HORIZON in the request to cause ShardIterator to point to the
-- last untrimmed record in the shard in the system, which is the oldest data
-- record in the shard. Or you can point to just after the most recent record
-- in the shard, by using the shard iterator type LATEST, so that you always
-- read the most recent data in the shard. Note: Each shard iterator expires
-- five minutes after it is returned to the requester. When you repeatedly
-- read from an Amazon Kinesis stream use a GetShardIterator request to get
-- the first shard iterator to to use in your first GetRecords request and
-- then use the shard iterator returned by the GetRecords request in
-- NextShardIterator for subsequent reads. A new shard iterator is returned by
-- every GetRecords request in NextShardIterator, which you use in the
-- ShardIterator parameter of the next GetRecords request. If a
-- GetShardIterator request is made too often, you will receive a
-- ProvisionedThroughputExceededException. For more information about
-- throughput limits, see the Amazon Kinesis Developer Guide. GetShardIterator
-- can return null for its ShardIterator to indicate that the shard has been
-- closed and that the requested iterator will return no more data. A shard
-- can be closed by a SplitShard or MergeShards operation. GetShardIterator
-- has a limit of 5 transactions per second per account per open shard. Get a
-- Shard Iterator The following is an example of an Amazon Kinesis
-- GetShardIterator request and response. POST / HTTP/1.1 Host: kinesis..
-- x-amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.GetShardIterator { "StreamName": "exampleStreamName",
-- "ShardId": "shardId-000000000001", "ShardIteratorType": "LATEST" } HTTP/1.1
-- 200 OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]> { "ShardIterator":
-- "AAAAAAAAAAETYyAYzd665+8e0X7JTsASDM/Hr2rSwc0X2qz93iuA3udrjTH+ikQvpQk/1ZcMMLzRdAesqwBGPnsthzU0/CBlM/U8/8oEqGwX3pKw0XyeDNRAAZyXBo3MqkQtCpXhr942BRTjvWKhFz7OmCb2Ncfr8Tl2cBktooi6kJhr+djN5WYkB38Rr3akRgCl9qaU4dY="
-- }.
--
-- See: 'Network.AWS.Kinesis.V2013_12_02.GetShardIterator'
getShardIterator :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    , AWSRequest a
                    )
                 => Text -- ^ 'gsiStreamName'
                 -> Text -- ^ 'gsiShardId'
                 -> ShardIteratorType -- ^ 'gsiShardIteratorType'
                 -> State GetShardIterator a
                 -> m GetShardIteratorResponse
getShardIterator p1 p2 p3 s =
    send $ (mkGetShardIterator p1 p2 p3) &~ s

-- | This operation returns an array of the names of all the streams that are
-- associated with the AWS account making the ListStreams request. A given AWS
-- account can have many streams active at one time. The number of streams may
-- be too large to return from a single call to ListStreams. You can limit the
-- number of returned streams using the Limit parameter. If you do not specify
-- a value for the Limit parameter, Amazon Kinesis uses the default limit,
-- which is currently 10. You can detect if there are more streams available
-- to list by using the HasMoreStreams flag from the returned output. If there
-- are more streams available, you can request more streams by using the name
-- of the last stream returned by the ListStreams request in the
-- ExclusiveStartStreamName parameter in a subsequent request to ListStreams.
-- The group of stream names returned by the subsequent request is then added
-- to the list. You can continue this process until all the stream names have
-- been collected in the list. ListStreams has a limit of 5 transactions per
-- second per account. List the Streams for an AWS Account The following is an
-- example of an Amazon Kinesis ListStreams request and response. POST /
-- HTTP/1.1 Host: kinesis.. x-amz-Date: Authorization: AWS4-HMAC-SHA256
-- Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.ListStreams HTTP/1.1 200 OK x-amzn-RequestId:
-- Content-Type: application/x-amz-json-1.1 Content-Length: Date: ]]> {
-- "HasMoreStreams": false, "StreamNames": [ "exampleStreamName" ] }.
--
-- See: 'Network.AWS.Kinesis.V2013_12_02.ListStreams'
listStreams :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env (ResumableSource m)
               , AWSPager a
               )
            => State ListStreams a
            -> ResumableSource m ListStreamsResponse
listStreams s =
    paginate (mkListStreams &~ s)

-- | This operation merges two adjacent shards in a stream and combines them
-- into a single shard to reduce the stream's capacity to ingest and transport
-- data. Two shards are considered adjacent if the union of the hash key
-- ranges for the two shards form a contiguous set with no gaps. For example,
-- if you have two shards, one with a hash key range of 276...381 and the
-- other with a hash key range of 382...454, then you could merge these two
-- shards into a single shard that would have a hash key range of 276...454.
-- After the merge, the single child shard receives data for all hash key
-- values covered by the two parent shards. MergeShards is called when there
-- is a need to reduce the overall capacity of a stream because of excess
-- capacity that is not being used. The operation requires that you specify
-- the shard to be merged and the adjacent shard for a given stream. For more
-- information about merging shards, see the Amazon Kinesis Developer Guide.
-- If the stream is in the ACTIVE state, you can call MergeShards. If a stream
-- is in CREATING or UPDATING or DELETING states, then Amazon Kinesis returns
-- a ResourceInUseException. If the specified stream does not exist, Amazon
-- Kinesis returns a ResourceNotFoundException. You can use the DescribeStream
-- operation to check the state of the stream, which is returned in
-- StreamStatus. MergeShards is an asynchronous operation. Upon receiving a
-- MergeShards request, Amazon Kinesis immediately returns a response and sets
-- the StreamStatus to UPDATING. After the operation is completed, Amazon
-- Kinesis sets the StreamStatus to ACTIVE. Read and write operations continue
-- to work while the stream is in the UPDATING state. You use the
-- DescribeStream operation to determine the shard IDs that are specified in
-- the MergeShards request. If you try to operate on too many streams in
-- parallel using CreateStream, DeleteStream, MergeShards or SplitShard, you
-- will receive a LimitExceededException. MergeShards has limit of 5
-- transactions per second per account. Merge Two Adjacent Shards The
-- following is an example of an Amazon Kinesis MergeShards request and
-- response. POST / HTTP/1.1 Host: kinesis.. x-amz-Date: Authorization:
-- AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.MergeShards { "StreamName": "exampleStreamName",
-- "ShardToMerge": "shardId-000000000000", "AdjacentShardToMerge":
-- "shardId-000000000001" } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]>.
--
-- See: 'Network.AWS.Kinesis.V2013_12_02.MergeShards'
mergeShards :: ( MonadCatch m
               , MonadResource m
               , MonadError AWS.Error m
               , MonadReader Env m
               , AWSRequest a
               )
            => Text -- ^ 'msStreamName'
            -> Text -- ^ 'msShardToMerge'
            -> Text -- ^ 'msAdjacentShardToMerge'
            -> State MergeShards a
            -> m MergeShardsResponse
mergeShards p1 p2 p3 s =
    send $ (mkMergeShards p1 p2 p3) &~ s

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
--
-- See: 'Network.AWS.Kinesis.V2013_12_02.PutRecord'
putRecord :: ( MonadCatch m
             , MonadResource m
             , MonadError AWS.Error m
             , MonadReader Env m
             , AWSRequest a
             )
          => Text -- ^ 'prStreamName'
          -> Base64 -- ^ 'prData'
          -> Text -- ^ 'prPartitionKey'
          -> State PutRecord a
          -> m PutRecordResponse
putRecord p1 p2 p3 s =
    send $ (mkPutRecord p1 p2 p3) &~ s

-- | This operation splits a shard into two new shards in the stream, to
-- increase the stream's capacity to ingest and transport data. SplitShard is
-- called when there is a need to increase the overall capacity of stream
-- because of an expected increase in the volume of data records being
-- ingested. SplitShard can also be used when a given shard appears to be
-- approaching its maximum utilization, for example, when the set of producers
-- sending data into the specific shard are suddenly sending more than
-- previously anticipated. You can also call the SplitShard operation to
-- increase stream capacity, so that more Amazon Kinesis applications can
-- simultaneously read data from the stream for real-time processing. The
-- SplitShard operation requires that you specify the shard to be split and
-- the new hash key, which is the position in the shard where the shard gets
-- split in two. In many cases, the new hash key might simply be the average
-- of the beginning and ending hash key, but it can be any hash key value in
-- the range being mapped into the shard. For more information about splitting
-- shards, see the Amazon Kinesis Developer Guide. You can use the
-- DescribeStream operation to determine the shard ID and hash key values for
-- the ShardToSplit and NewStartingHashKey parameters that are specified in
-- the SplitShard request. SplitShard is an asynchronous operation. Upon
-- receiving a SplitShard request, Amazon Kinesis immediately returns a
-- response and sets the stream status to UPDATING. After the operation is
-- completed, Amazon Kinesis sets the stream status to ACTIVE. Read and write
-- operations continue to work while the stream is in the UPDATING state. You
-- can use DescribeStream to check the status of the stream, which is returned
-- in StreamStatus. If the stream is in the ACTIVE state, you can call
-- SplitShard. If a stream is in CREATING or UPDATING or DELETING states, then
-- Amazon Kinesis returns a ResourceInUseException. If the specified stream
-- does not exist, Amazon Kinesis returns a ResourceNotFoundException. If you
-- try to create more shards than are authorized for your account, you receive
-- a LimitExceededException. Note: The default limit for an AWS account is 10
-- shards per stream. If you need to create a stream with more than 10 shards,
-- contact AWS Support to increase the limit on your account. If you try to
-- operate on too many streams in parallel using CreateStream, DeleteStream,
-- MergeShards or SplitShard, you will receive a LimitExceededException.
-- SplitShard has limit of 5 transactions per second per account. Split a
-- Shard The following is an example of an Amazon Kinesis SplitShard request
-- and response. POST / HTTP/1.1 Host: kinesis.. x-amz-Date: Authorization:
-- AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.SplitShard { "StreamName": "exampleStreamName",
-- "ShardToSplit": "shardId-000000000000", "NewStartingHashKey": "10" }
-- HTTP/1.1 200 OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]>.
--
-- See: 'Network.AWS.Kinesis.V2013_12_02.SplitShard'
splitShard :: ( MonadCatch m
              , MonadResource m
              , MonadError AWS.Error m
              , MonadReader Env m
              , AWSRequest a
              )
           => Text -- ^ 'ssStreamName'
           -> Text -- ^ 'ssShardToSplit'
           -> Text -- ^ 'ssNewStartingHashKey'
           -> State SplitShard a
           -> m SplitShardResponse
splitShard p1 p2 p3 s =
    send $ (mkSplitShard p1 p2 p3) &~ s

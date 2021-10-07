{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Lens
  ( -- * Operations

    -- ** GetShardIterator
    getShardIterator_sequenceNumber,
    getShardIterator_streamArn,
    getShardIterator_shardId,
    getShardIterator_shardIteratorType,
    getShardIteratorResponse_shardIterator,
    getShardIteratorResponse_httpStatus,

    -- ** GetRecords
    getRecords_limit,
    getRecords_shardIterator,
    getRecordsResponse_records,
    getRecordsResponse_nextShardIterator,
    getRecordsResponse_httpStatus,

    -- ** DescribeStream
    describeStream_exclusiveStartShardId,
    describeStream_limit,
    describeStream_streamArn,
    describeStreamResponse_streamDescription,
    describeStreamResponse_httpStatus,

    -- ** ListStreams
    listStreams_tableName,
    listStreams_exclusiveStartStreamArn,
    listStreams_limit,
    listStreamsResponse_streams,
    listStreamsResponse_lastEvaluatedStreamArn,
    listStreamsResponse_httpStatus,

    -- * Types

    -- ** AttributeValue
    attributeValue_bs,
    attributeValue_n,
    attributeValue_bool,
    attributeValue_null,
    attributeValue_s,
    attributeValue_m,
    attributeValue_b,
    attributeValue_l,
    attributeValue_ss,
    attributeValue_ns,

    -- ** Identity
    identity_principalId,
    identity_type,

    -- ** KeySchemaElement
    keySchemaElement_attributeName,
    keySchemaElement_keyType,

    -- ** Record
    record_userIdentity,
    record_eventSource,
    record_eventID,
    record_eventName,
    record_eventVersion,
    record_dynamodb,
    record_awsRegion,

    -- ** SequenceNumberRange
    sequenceNumberRange_startingSequenceNumber,
    sequenceNumberRange_endingSequenceNumber,

    -- ** Shard
    shard_shardId,
    shard_sequenceNumberRange,
    shard_parentShardId,

    -- ** Stream
    stream_tableName,
    stream_streamArn,
    stream_streamLabel,

    -- ** StreamDescription
    streamDescription_lastEvaluatedShardId,
    streamDescription_streamViewType,
    streamDescription_tableName,
    streamDescription_creationRequestDateTime,
    streamDescription_keySchema,
    streamDescription_streamStatus,
    streamDescription_shards,
    streamDescription_streamArn,
    streamDescription_streamLabel,

    -- ** StreamRecord
    streamRecord_sequenceNumber,
    streamRecord_streamViewType,
    streamRecord_keys,
    streamRecord_sizeBytes,
    streamRecord_newImage,
    streamRecord_approximateCreationDateTime,
    streamRecord_oldImage,
  )
where

import Network.AWS.DynamoDBStreams.DescribeStream
import Network.AWS.DynamoDBStreams.GetRecords
import Network.AWS.DynamoDBStreams.GetShardIterator
import Network.AWS.DynamoDBStreams.ListStreams
import Network.AWS.DynamoDBStreams.Types.AttributeValue
import Network.AWS.DynamoDBStreams.Types.Identity
import Network.AWS.DynamoDBStreams.Types.KeySchemaElement
import Network.AWS.DynamoDBStreams.Types.Record
import Network.AWS.DynamoDBStreams.Types.SequenceNumberRange
import Network.AWS.DynamoDBStreams.Types.Shard
import Network.AWS.DynamoDBStreams.Types.Stream
import Network.AWS.DynamoDBStreams.Types.StreamDescription
import Network.AWS.DynamoDBStreams.Types.StreamRecord

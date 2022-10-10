{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DynamoDBStreams.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDBStreams.Lens
  ( -- * Operations

    -- ** DescribeStream
    describeStream_exclusiveStartShardId,
    describeStream_limit,
    describeStream_streamArn,
    describeStreamResponse_streamDescription,
    describeStreamResponse_httpStatus,

    -- ** GetRecords
    getRecords_limit,
    getRecords_shardIterator,
    getRecordsResponse_records,
    getRecordsResponse_nextShardIterator,
    getRecordsResponse_httpStatus,

    -- ** GetShardIterator
    getShardIterator_sequenceNumber,
    getShardIterator_streamArn,
    getShardIterator_shardId,
    getShardIterator_shardIteratorType,
    getShardIteratorResponse_shardIterator,
    getShardIteratorResponse_httpStatus,

    -- ** ListStreams
    listStreams_tableName,
    listStreams_exclusiveStartStreamArn,
    listStreams_limit,
    listStreamsResponse_streams,
    listStreamsResponse_lastEvaluatedStreamArn,
    listStreamsResponse_httpStatus,

    -- * Types

    -- ** Identity
    identity_principalId,
    identity_type,

    -- ** KeySchemaElement
    keySchemaElement_attributeName,
    keySchemaElement_keyType,

    -- ** Record
    record_userIdentity,
    record_dynamodb,
    record_eventVersion,
    record_eventName,
    record_eventID,
    record_awsRegion,
    record_eventSource,

    -- ** SequenceNumberRange
    sequenceNumberRange_endingSequenceNumber,
    sequenceNumberRange_startingSequenceNumber,

    -- ** Shard
    shard_sequenceNumberRange,
    shard_parentShardId,
    shard_shardId,

    -- ** Stream
    stream_tableName,
    stream_streamLabel,
    stream_streamArn,

    -- ** StreamDescription
    streamDescription_tableName,
    streamDescription_streamLabel,
    streamDescription_creationRequestDateTime,
    streamDescription_streamViewType,
    streamDescription_streamStatus,
    streamDescription_keySchema,
    streamDescription_shards,
    streamDescription_streamArn,
    streamDescription_lastEvaluatedShardId,

    -- ** StreamRecord
    streamRecord_sizeBytes,
    streamRecord_streamViewType,
    streamRecord_oldImage,
    streamRecord_approximateCreationDateTime,
    streamRecord_sequenceNumber,
    streamRecord_keys,
    streamRecord_newImage,
  )
where

import Amazonka.DynamoDBStreams.DescribeStream
import Amazonka.DynamoDBStreams.GetRecords
import Amazonka.DynamoDBStreams.GetShardIterator
import Amazonka.DynamoDBStreams.ListStreams
import Amazonka.DynamoDBStreams.Types.Identity
import Amazonka.DynamoDBStreams.Types.KeySchemaElement
import Amazonka.DynamoDBStreams.Types.Record
import Amazonka.DynamoDBStreams.Types.SequenceNumberRange
import Amazonka.DynamoDBStreams.Types.Shard
import Amazonka.DynamoDBStreams.Types.Stream
import Amazonka.DynamoDBStreams.Types.StreamDescription
import Amazonka.DynamoDBStreams.Types.StreamRecord

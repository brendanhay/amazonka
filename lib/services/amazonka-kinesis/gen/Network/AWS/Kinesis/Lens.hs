{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Lens
  ( -- * Operations

    -- ** PutRecord
    putRecord_explicitHashKey,
    putRecord_sequenceNumberForOrdering,
    putRecord_streamName,
    putRecord_data,
    putRecord_partitionKey,
    putRecordResponse_encryptionType,
    putRecordResponse_httpStatus,
    putRecordResponse_shardId,
    putRecordResponse_sequenceNumber,

    -- ** SubscribeToShard
    subscribeToShard_consumerARN,
    subscribeToShard_shardId,
    subscribeToShard_startingPosition,
    subscribeToShardResponse_httpStatus,
    subscribeToShardResponse_eventStream,

    -- ** DecreaseStreamRetentionPeriod
    decreaseStreamRetentionPeriod_streamName,
    decreaseStreamRetentionPeriod_retentionPeriodHours,

    -- ** MergeShards
    mergeShards_streamName,
    mergeShards_shardToMerge,
    mergeShards_adjacentShardToMerge,

    -- ** DeregisterStreamConsumer
    deregisterStreamConsumer_consumerARN,
    deregisterStreamConsumer_streamARN,
    deregisterStreamConsumer_consumerName,

    -- ** DescribeStreamSummary
    describeStreamSummary_streamName,
    describeStreamSummaryResponse_httpStatus,
    describeStreamSummaryResponse_streamDescriptionSummary,

    -- ** GetShardIterator
    getShardIterator_startingSequenceNumber,
    getShardIterator_timestamp,
    getShardIterator_streamName,
    getShardIterator_shardId,
    getShardIterator_shardIteratorType,
    getShardIteratorResponse_shardIterator,
    getShardIteratorResponse_httpStatus,

    -- ** GetRecords
    getRecords_limit,
    getRecords_shardIterator,
    getRecordsResponse_nextShardIterator,
    getRecordsResponse_millisBehindLatest,
    getRecordsResponse_childShards,
    getRecordsResponse_httpStatus,
    getRecordsResponse_records,

    -- ** StopStreamEncryption
    stopStreamEncryption_streamName,
    stopStreamEncryption_encryptionType,
    stopStreamEncryption_keyId,

    -- ** EnableEnhancedMonitoring
    enableEnhancedMonitoring_streamName,
    enableEnhancedMonitoring_shardLevelMetrics,
    enhancedMonitoringOutput_desiredShardLevelMetrics,
    enhancedMonitoringOutput_currentShardLevelMetrics,
    enhancedMonitoringOutput_streamName,

    -- ** DescribeLimits
    describeLimitsResponse_httpStatus,
    describeLimitsResponse_shardLimit,
    describeLimitsResponse_openShardCount,

    -- ** RegisterStreamConsumer
    registerStreamConsumer_streamARN,
    registerStreamConsumer_consumerName,
    registerStreamConsumerResponse_httpStatus,
    registerStreamConsumerResponse_consumer,

    -- ** DisableEnhancedMonitoring
    disableEnhancedMonitoring_streamName,
    disableEnhancedMonitoring_shardLevelMetrics,
    enhancedMonitoringOutput_desiredShardLevelMetrics,
    enhancedMonitoringOutput_currentShardLevelMetrics,
    enhancedMonitoringOutput_streamName,

    -- ** UpdateShardCount
    updateShardCount_streamName,
    updateShardCount_targetShardCount,
    updateShardCount_scalingType,
    updateShardCountResponse_targetShardCount,
    updateShardCountResponse_streamName,
    updateShardCountResponse_currentShardCount,
    updateShardCountResponse_httpStatus,

    -- ** ListTagsForStream
    listTagsForStream_limit,
    listTagsForStream_exclusiveStartTagKey,
    listTagsForStream_streamName,
    listTagsForStreamResponse_httpStatus,
    listTagsForStreamResponse_tags,
    listTagsForStreamResponse_hasMoreTags,

    -- ** DescribeStreamConsumer
    describeStreamConsumer_consumerARN,
    describeStreamConsumer_streamARN,
    describeStreamConsumer_consumerName,
    describeStreamConsumerResponse_httpStatus,
    describeStreamConsumerResponse_consumerDescription,

    -- ** AddTagsToStream
    addTagsToStream_streamName,
    addTagsToStream_tags,

    -- ** PutRecords
    putRecords_records,
    putRecords_streamName,
    putRecordsResponse_encryptionType,
    putRecordsResponse_failedRecordCount,
    putRecordsResponse_httpStatus,
    putRecordsResponse_records,

    -- ** ListShards
    listShards_shardFilter,
    listShards_nextToken,
    listShards_exclusiveStartShardId,
    listShards_streamCreationTimestamp,
    listShards_streamName,
    listShards_maxResults,
    listShardsResponse_nextToken,
    listShardsResponse_shards,
    listShardsResponse_httpStatus,

    -- ** DeleteStream
    deleteStream_enforceConsumerDeletion,
    deleteStream_streamName,

    -- ** RemoveTagsFromStream
    removeTagsFromStream_streamName,
    removeTagsFromStream_tagKeys,

    -- ** ListStreams
    listStreams_limit,
    listStreams_exclusiveStartStreamName,
    listStreamsResponse_httpStatus,
    listStreamsResponse_streamNames,
    listStreamsResponse_hasMoreStreams,

    -- ** CreateStream
    createStream_streamName,
    createStream_shardCount,

    -- ** StartStreamEncryption
    startStreamEncryption_streamName,
    startStreamEncryption_encryptionType,
    startStreamEncryption_keyId,

    -- ** ListStreamConsumers
    listStreamConsumers_nextToken,
    listStreamConsumers_streamCreationTimestamp,
    listStreamConsumers_maxResults,
    listStreamConsumers_streamARN,
    listStreamConsumersResponse_nextToken,
    listStreamConsumersResponse_consumers,
    listStreamConsumersResponse_httpStatus,

    -- ** SplitShard
    splitShard_streamName,
    splitShard_shardToSplit,
    splitShard_newStartingHashKey,

    -- ** IncreaseStreamRetentionPeriod
    increaseStreamRetentionPeriod_streamName,
    increaseStreamRetentionPeriod_retentionPeriodHours,

    -- ** DescribeStream
    describeStream_exclusiveStartShardId,
    describeStream_limit,
    describeStream_streamName,
    describeStreamResponse_httpStatus,
    describeStreamResponse_streamDescription,

    -- * Types

    -- ** ChildShard
    childShard_shardId,
    childShard_parentShards,
    childShard_hashKeyRange,

    -- ** Consumer
    consumer_consumerName,
    consumer_consumerARN,
    consumer_consumerStatus,
    consumer_consumerCreationTimestamp,

    -- ** ConsumerDescription
    consumerDescription_consumerName,
    consumerDescription_consumerARN,
    consumerDescription_consumerStatus,
    consumerDescription_consumerCreationTimestamp,
    consumerDescription_streamARN,

    -- ** EnhancedMetrics
    enhancedMetrics_shardLevelMetrics,

    -- ** EnhancedMonitoringOutput
    enhancedMonitoringOutput_desiredShardLevelMetrics,
    enhancedMonitoringOutput_currentShardLevelMetrics,
    enhancedMonitoringOutput_streamName,

    -- ** HashKeyRange
    hashKeyRange_startingHashKey,
    hashKeyRange_endingHashKey,

    -- ** PutRecordsRequestEntry
    putRecordsRequestEntry_explicitHashKey,
    putRecordsRequestEntry_data,
    putRecordsRequestEntry_partitionKey,

    -- ** PutRecordsResultEntry
    putRecordsResultEntry_sequenceNumber,
    putRecordsResultEntry_errorCode,
    putRecordsResultEntry_errorMessage,
    putRecordsResultEntry_shardId,

    -- ** Record
    record_encryptionType,
    record_approximateArrivalTimestamp,
    record_sequenceNumber,
    record_data,
    record_partitionKey,

    -- ** SequenceNumberRange
    sequenceNumberRange_endingSequenceNumber,
    sequenceNumberRange_startingSequenceNumber,

    -- ** Shard
    shard_adjacentParentShardId,
    shard_parentShardId,
    shard_shardId,
    shard_hashKeyRange,
    shard_sequenceNumberRange,

    -- ** ShardFilter
    shardFilter_timestamp,
    shardFilter_shardId,
    shardFilter_type,

    -- ** StartingPosition
    startingPosition_sequenceNumber,
    startingPosition_timestamp,
    startingPosition_type,

    -- ** StreamDescription
    streamDescription_encryptionType,
    streamDescription_keyId,
    streamDescription_streamName,
    streamDescription_streamARN,
    streamDescription_streamStatus,
    streamDescription_shards,
    streamDescription_hasMoreShards,
    streamDescription_retentionPeriodHours,
    streamDescription_streamCreationTimestamp,
    streamDescription_enhancedMonitoring,

    -- ** StreamDescriptionSummary
    streamDescriptionSummary_encryptionType,
    streamDescriptionSummary_keyId,
    streamDescriptionSummary_consumerCount,
    streamDescriptionSummary_streamName,
    streamDescriptionSummary_streamARN,
    streamDescriptionSummary_streamStatus,
    streamDescriptionSummary_retentionPeriodHours,
    streamDescriptionSummary_streamCreationTimestamp,
    streamDescriptionSummary_enhancedMonitoring,
    streamDescriptionSummary_openShardCount,

    -- ** SubscribeToShardEvent
    subscribeToShardEvent_childShards,
    subscribeToShardEvent_records,
    subscribeToShardEvent_continuationSequenceNumber,
    subscribeToShardEvent_millisBehindLatest,

    -- ** Tag
    tag_value,
    tag_key,
  )
where

import Network.AWS.Kinesis.AddTagsToStream
import Network.AWS.Kinesis.CreateStream
import Network.AWS.Kinesis.DecreaseStreamRetentionPeriod
import Network.AWS.Kinesis.DeleteStream
import Network.AWS.Kinesis.DeregisterStreamConsumer
import Network.AWS.Kinesis.DescribeLimits
import Network.AWS.Kinesis.DescribeStream
import Network.AWS.Kinesis.DescribeStreamConsumer
import Network.AWS.Kinesis.DescribeStreamSummary
import Network.AWS.Kinesis.DisableEnhancedMonitoring
import Network.AWS.Kinesis.EnableEnhancedMonitoring
import Network.AWS.Kinesis.GetRecords
import Network.AWS.Kinesis.GetShardIterator
import Network.AWS.Kinesis.IncreaseStreamRetentionPeriod
import Network.AWS.Kinesis.ListShards
import Network.AWS.Kinesis.ListStreamConsumers
import Network.AWS.Kinesis.ListStreams
import Network.AWS.Kinesis.ListTagsForStream
import Network.AWS.Kinesis.MergeShards
import Network.AWS.Kinesis.PutRecord
import Network.AWS.Kinesis.PutRecords
import Network.AWS.Kinesis.RegisterStreamConsumer
import Network.AWS.Kinesis.RemoveTagsFromStream
import Network.AWS.Kinesis.SplitShard
import Network.AWS.Kinesis.StartStreamEncryption
import Network.AWS.Kinesis.StopStreamEncryption
import Network.AWS.Kinesis.SubscribeToShard
import Network.AWS.Kinesis.Types.ChildShard
import Network.AWS.Kinesis.Types.Consumer
import Network.AWS.Kinesis.Types.ConsumerDescription
import Network.AWS.Kinesis.Types.EnhancedMetrics
import Network.AWS.Kinesis.Types.EnhancedMonitoringOutput
import Network.AWS.Kinesis.Types.HashKeyRange
import Network.AWS.Kinesis.Types.PutRecordsRequestEntry
import Network.AWS.Kinesis.Types.PutRecordsResultEntry
import Network.AWS.Kinesis.Types.Record
import Network.AWS.Kinesis.Types.SequenceNumberRange
import Network.AWS.Kinesis.Types.Shard
import Network.AWS.Kinesis.Types.ShardFilter
import Network.AWS.Kinesis.Types.StartingPosition
import Network.AWS.Kinesis.Types.StreamDescription
import Network.AWS.Kinesis.Types.StreamDescriptionSummary
import Network.AWS.Kinesis.Types.SubscribeToShardEvent
import Network.AWS.Kinesis.Types.Tag
import Network.AWS.Kinesis.UpdateShardCount

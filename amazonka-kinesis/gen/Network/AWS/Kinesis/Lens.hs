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

    -- ** AddTagsToStream
    addTagsToStream_streamName,
    addTagsToStream_tags,

    -- ** SubscribeToShard
    subscribeToShard_consumerARN,
    subscribeToShard_shardId,
    subscribeToShard_startingPosition,
    subscribeToShardResponse_httpStatus,
    subscribeToShardResponse_eventStream,

    -- ** ListTagsForStream
    listTagsForStream_exclusiveStartTagKey,
    listTagsForStream_limit,
    listTagsForStream_streamName,
    listTagsForStreamResponse_httpStatus,
    listTagsForStreamResponse_tags,
    listTagsForStreamResponse_hasMoreTags,

    -- ** IncreaseStreamRetentionPeriod
    increaseStreamRetentionPeriod_streamName,
    increaseStreamRetentionPeriod_retentionPeriodHours,

    -- ** DisableEnhancedMonitoring
    disableEnhancedMonitoring_streamName,
    disableEnhancedMonitoring_shardLevelMetrics,
    enhancedMonitoringOutput_currentShardLevelMetrics,
    enhancedMonitoringOutput_streamName,
    enhancedMonitoringOutput_desiredShardLevelMetrics,

    -- ** SplitShard
    splitShard_streamName,
    splitShard_shardToSplit,
    splitShard_newStartingHashKey,

    -- ** StopStreamEncryption
    stopStreamEncryption_streamName,
    stopStreamEncryption_encryptionType,
    stopStreamEncryption_keyId,

    -- ** EnableEnhancedMonitoring
    enableEnhancedMonitoring_streamName,
    enableEnhancedMonitoring_shardLevelMetrics,
    enhancedMonitoringOutput_currentShardLevelMetrics,
    enhancedMonitoringOutput_streamName,
    enhancedMonitoringOutput_desiredShardLevelMetrics,

    -- ** RegisterStreamConsumer
    registerStreamConsumer_streamARN,
    registerStreamConsumer_consumerName,
    registerStreamConsumerResponse_httpStatus,
    registerStreamConsumerResponse_consumer,

    -- ** StartStreamEncryption
    startStreamEncryption_streamName,
    startStreamEncryption_encryptionType,
    startStreamEncryption_keyId,

    -- ** DescribeLimits
    describeLimitsResponse_httpStatus,
    describeLimitsResponse_shardLimit,
    describeLimitsResponse_openShardCount,

    -- ** ListStreamConsumers
    listStreamConsumers_nextToken,
    listStreamConsumers_maxResults,
    listStreamConsumers_streamCreationTimestamp,
    listStreamConsumers_streamARN,
    listStreamConsumersResponse_nextToken,
    listStreamConsumersResponse_consumers,
    listStreamConsumersResponse_httpStatus,

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
    getRecordsResponse_millisBehindLatest,
    getRecordsResponse_nextShardIterator,
    getRecordsResponse_childShards,
    getRecordsResponse_httpStatus,
    getRecordsResponse_records,

    -- ** DeleteStream
    deleteStream_enforceConsumerDeletion,
    deleteStream_streamName,

    -- ** ListShards
    listShards_exclusiveStartShardId,
    listShards_shardFilter,
    listShards_nextToken,
    listShards_maxResults,
    listShards_streamCreationTimestamp,
    listShards_streamName,
    listShardsResponse_nextToken,
    listShardsResponse_shards,
    listShardsResponse_httpStatus,

    -- ** RemoveTagsFromStream
    removeTagsFromStream_streamName,
    removeTagsFromStream_tagKeys,

    -- ** DescribeStreamSummary
    describeStreamSummary_streamName,
    describeStreamSummaryResponse_httpStatus,
    describeStreamSummaryResponse_streamDescriptionSummary,

    -- ** DeregisterStreamConsumer
    deregisterStreamConsumer_consumerName,
    deregisterStreamConsumer_streamARN,
    deregisterStreamConsumer_consumerARN,

    -- ** DecreaseStreamRetentionPeriod
    decreaseStreamRetentionPeriod_streamName,
    decreaseStreamRetentionPeriod_retentionPeriodHours,

    -- ** PutRecords
    putRecords_records,
    putRecords_streamName,
    putRecordsResponse_encryptionType,
    putRecordsResponse_failedRecordCount,
    putRecordsResponse_httpStatus,
    putRecordsResponse_records,

    -- ** MergeShards
    mergeShards_streamName,
    mergeShards_shardToMerge,
    mergeShards_adjacentShardToMerge,

    -- ** DescribeStreamConsumer
    describeStreamConsumer_consumerName,
    describeStreamConsumer_streamARN,
    describeStreamConsumer_consumerARN,
    describeStreamConsumerResponse_httpStatus,
    describeStreamConsumerResponse_consumerDescription,

    -- ** PutRecord
    putRecord_sequenceNumberForOrdering,
    putRecord_explicitHashKey,
    putRecord_streamName,
    putRecord_data,
    putRecord_partitionKey,
    putRecordResponse_encryptionType,
    putRecordResponse_httpStatus,
    putRecordResponse_shardId,
    putRecordResponse_sequenceNumber,

    -- ** DescribeStream
    describeStream_exclusiveStartShardId,
    describeStream_limit,
    describeStream_streamName,
    describeStreamResponse_httpStatus,
    describeStreamResponse_streamDescription,

    -- ** UpdateShardCount
    updateShardCount_streamName,
    updateShardCount_targetShardCount,
    updateShardCount_scalingType,
    updateShardCountResponse_targetShardCount,
    updateShardCountResponse_currentShardCount,
    updateShardCountResponse_streamName,
    updateShardCountResponse_httpStatus,

    -- ** CreateStream
    createStream_streamName,
    createStream_shardCount,

    -- ** ListStreams
    listStreams_exclusiveStartStreamName,
    listStreams_limit,
    listStreamsResponse_httpStatus,
    listStreamsResponse_streamNames,
    listStreamsResponse_hasMoreStreams,

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
    enhancedMonitoringOutput_currentShardLevelMetrics,
    enhancedMonitoringOutput_streamName,
    enhancedMonitoringOutput_desiredShardLevelMetrics,

    -- ** HashKeyRange
    hashKeyRange_startingHashKey,
    hashKeyRange_endingHashKey,

    -- ** PutRecordsRequestEntry
    putRecordsRequestEntry_explicitHashKey,
    putRecordsRequestEntry_data,
    putRecordsRequestEntry_partitionKey,

    -- ** PutRecordsResultEntry
    putRecordsResultEntry_sequenceNumber,
    putRecordsResultEntry_shardId,
    putRecordsResultEntry_errorMessage,
    putRecordsResultEntry_errorCode,

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
    shardFilter_shardId,
    shardFilter_timestamp,
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
    streamDescriptionSummary_consumerCount,
    streamDescriptionSummary_keyId,
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

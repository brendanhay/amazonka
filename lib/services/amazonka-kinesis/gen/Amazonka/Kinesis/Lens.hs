{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Kinesis.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kinesis.Lens
  ( -- * Operations

    -- ** AddTagsToStream
    addTagsToStream_streamARN,
    addTagsToStream_streamName,
    addTagsToStream_tags,

    -- ** CreateStream
    createStream_shardCount,
    createStream_streamModeDetails,
    createStream_streamName,

    -- ** DecreaseStreamRetentionPeriod
    decreaseStreamRetentionPeriod_streamARN,
    decreaseStreamRetentionPeriod_streamName,
    decreaseStreamRetentionPeriod_retentionPeriodHours,

    -- ** DeleteStream
    deleteStream_enforceConsumerDeletion,
    deleteStream_streamARN,
    deleteStream_streamName,

    -- ** DeregisterStreamConsumer
    deregisterStreamConsumer_consumerARN,
    deregisterStreamConsumer_consumerName,
    deregisterStreamConsumer_streamARN,

    -- ** DescribeLimits
    describeLimitsResponse_httpStatus,
    describeLimitsResponse_shardLimit,
    describeLimitsResponse_openShardCount,
    describeLimitsResponse_onDemandStreamCount,
    describeLimitsResponse_onDemandStreamCountLimit,

    -- ** DescribeStream
    describeStream_exclusiveStartShardId,
    describeStream_limit,
    describeStream_streamARN,
    describeStream_streamName,
    describeStreamResponse_httpStatus,
    describeStreamResponse_streamDescription,

    -- ** DescribeStreamConsumer
    describeStreamConsumer_consumerARN,
    describeStreamConsumer_consumerName,
    describeStreamConsumer_streamARN,
    describeStreamConsumerResponse_httpStatus,
    describeStreamConsumerResponse_consumerDescription,

    -- ** DescribeStreamSummary
    describeStreamSummary_streamARN,
    describeStreamSummary_streamName,
    describeStreamSummaryResponse_httpStatus,
    describeStreamSummaryResponse_streamDescriptionSummary,

    -- ** DisableEnhancedMonitoring
    disableEnhancedMonitoring_streamARN,
    disableEnhancedMonitoring_streamName,
    disableEnhancedMonitoring_shardLevelMetrics,
    enhancedMonitoringOutput_currentShardLevelMetrics,
    enhancedMonitoringOutput_desiredShardLevelMetrics,
    enhancedMonitoringOutput_streamARN,
    enhancedMonitoringOutput_streamName,

    -- ** EnableEnhancedMonitoring
    enableEnhancedMonitoring_streamARN,
    enableEnhancedMonitoring_streamName,
    enableEnhancedMonitoring_shardLevelMetrics,
    enhancedMonitoringOutput_currentShardLevelMetrics,
    enhancedMonitoringOutput_desiredShardLevelMetrics,
    enhancedMonitoringOutput_streamARN,
    enhancedMonitoringOutput_streamName,

    -- ** GetRecords
    getRecords_limit,
    getRecords_streamARN,
    getRecords_shardIterator,
    getRecordsResponse_childShards,
    getRecordsResponse_millisBehindLatest,
    getRecordsResponse_nextShardIterator,
    getRecordsResponse_httpStatus,
    getRecordsResponse_records,

    -- ** GetShardIterator
    getShardIterator_startingSequenceNumber,
    getShardIterator_streamARN,
    getShardIterator_streamName,
    getShardIterator_timestamp,
    getShardIterator_shardId,
    getShardIterator_shardIteratorType,
    getShardIteratorResponse_shardIterator,
    getShardIteratorResponse_httpStatus,

    -- ** IncreaseStreamRetentionPeriod
    increaseStreamRetentionPeriod_streamARN,
    increaseStreamRetentionPeriod_streamName,
    increaseStreamRetentionPeriod_retentionPeriodHours,

    -- ** ListShards
    listShards_exclusiveStartShardId,
    listShards_maxResults,
    listShards_nextToken,
    listShards_shardFilter,
    listShards_streamARN,
    listShards_streamCreationTimestamp,
    listShards_streamName,
    listShardsResponse_nextToken,
    listShardsResponse_shards,
    listShardsResponse_httpStatus,

    -- ** ListStreamConsumers
    listStreamConsumers_maxResults,
    listStreamConsumers_nextToken,
    listStreamConsumers_streamCreationTimestamp,
    listStreamConsumers_streamARN,
    listStreamConsumersResponse_consumers,
    listStreamConsumersResponse_nextToken,
    listStreamConsumersResponse_httpStatus,

    -- ** ListStreams
    listStreams_exclusiveStartStreamName,
    listStreams_limit,
    listStreams_nextToken,
    listStreamsResponse_nextToken,
    listStreamsResponse_streamSummaries,
    listStreamsResponse_httpStatus,
    listStreamsResponse_streamNames,
    listStreamsResponse_hasMoreStreams,

    -- ** ListTagsForStream
    listTagsForStream_exclusiveStartTagKey,
    listTagsForStream_limit,
    listTagsForStream_streamARN,
    listTagsForStream_streamName,
    listTagsForStreamResponse_httpStatus,
    listTagsForStreamResponse_tags,
    listTagsForStreamResponse_hasMoreTags,

    -- ** MergeShards
    mergeShards_streamARN,
    mergeShards_streamName,
    mergeShards_shardToMerge,
    mergeShards_adjacentShardToMerge,

    -- ** PutRecord
    putRecord_explicitHashKey,
    putRecord_sequenceNumberForOrdering,
    putRecord_streamARN,
    putRecord_streamName,
    putRecord_data,
    putRecord_partitionKey,
    putRecordResponse_encryptionType,
    putRecordResponse_httpStatus,
    putRecordResponse_shardId,
    putRecordResponse_sequenceNumber,

    -- ** PutRecords
    putRecords_streamARN,
    putRecords_streamName,
    putRecords_records,
    putRecordsResponse_encryptionType,
    putRecordsResponse_failedRecordCount,
    putRecordsResponse_httpStatus,
    putRecordsResponse_records,

    -- ** RegisterStreamConsumer
    registerStreamConsumer_streamARN,
    registerStreamConsumer_consumerName,
    registerStreamConsumerResponse_httpStatus,
    registerStreamConsumerResponse_consumer,

    -- ** RemoveTagsFromStream
    removeTagsFromStream_streamARN,
    removeTagsFromStream_streamName,
    removeTagsFromStream_tagKeys,

    -- ** SplitShard
    splitShard_streamARN,
    splitShard_streamName,
    splitShard_shardToSplit,
    splitShard_newStartingHashKey,

    -- ** StartStreamEncryption
    startStreamEncryption_streamARN,
    startStreamEncryption_streamName,
    startStreamEncryption_encryptionType,
    startStreamEncryption_keyId,

    -- ** StopStreamEncryption
    stopStreamEncryption_streamARN,
    stopStreamEncryption_streamName,
    stopStreamEncryption_encryptionType,
    stopStreamEncryption_keyId,

    -- ** SubscribeToShard
    subscribeToShard_consumerARN,
    subscribeToShard_shardId,
    subscribeToShard_startingPosition,
    subscribeToShardResponse_httpStatus,
    subscribeToShardResponse_eventStream,

    -- ** UpdateShardCount
    updateShardCount_streamARN,
    updateShardCount_streamName,
    updateShardCount_targetShardCount,
    updateShardCount_scalingType,
    updateShardCountResponse_currentShardCount,
    updateShardCountResponse_streamARN,
    updateShardCountResponse_streamName,
    updateShardCountResponse_targetShardCount,
    updateShardCountResponse_httpStatus,

    -- ** UpdateStreamMode
    updateStreamMode_streamARN,
    updateStreamMode_streamModeDetails,

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
    enhancedMonitoringOutput_desiredShardLevelMetrics,
    enhancedMonitoringOutput_streamARN,
    enhancedMonitoringOutput_streamName,

    -- ** HashKeyRange
    hashKeyRange_startingHashKey,
    hashKeyRange_endingHashKey,

    -- ** PutRecordsRequestEntry
    putRecordsRequestEntry_explicitHashKey,
    putRecordsRequestEntry_data,
    putRecordsRequestEntry_partitionKey,

    -- ** PutRecordsResultEntry
    putRecordsResultEntry_errorCode,
    putRecordsResultEntry_errorMessage,
    putRecordsResultEntry_sequenceNumber,
    putRecordsResultEntry_shardId,

    -- ** Record
    record_approximateArrivalTimestamp,
    record_encryptionType,
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
    streamDescription_streamModeDetails,
    streamDescription_streamName,
    streamDescription_streamARN,
    streamDescription_streamStatus,
    streamDescription_shards,
    streamDescription_hasMoreShards,
    streamDescription_retentionPeriodHours,
    streamDescription_streamCreationTimestamp,
    streamDescription_enhancedMonitoring,

    -- ** StreamDescriptionSummary
    streamDescriptionSummary_consumerCount,
    streamDescriptionSummary_encryptionType,
    streamDescriptionSummary_keyId,
    streamDescriptionSummary_streamModeDetails,
    streamDescriptionSummary_streamName,
    streamDescriptionSummary_streamARN,
    streamDescriptionSummary_streamStatus,
    streamDescriptionSummary_retentionPeriodHours,
    streamDescriptionSummary_streamCreationTimestamp,
    streamDescriptionSummary_enhancedMonitoring,
    streamDescriptionSummary_openShardCount,

    -- ** StreamModeDetails
    streamModeDetails_streamMode,

    -- ** StreamSummary
    streamSummary_streamCreationTimestamp,
    streamSummary_streamModeDetails,
    streamSummary_streamName,
    streamSummary_streamARN,
    streamSummary_streamStatus,

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

import Amazonka.Kinesis.AddTagsToStream
import Amazonka.Kinesis.CreateStream
import Amazonka.Kinesis.DecreaseStreamRetentionPeriod
import Amazonka.Kinesis.DeleteStream
import Amazonka.Kinesis.DeregisterStreamConsumer
import Amazonka.Kinesis.DescribeLimits
import Amazonka.Kinesis.DescribeStream
import Amazonka.Kinesis.DescribeStreamConsumer
import Amazonka.Kinesis.DescribeStreamSummary
import Amazonka.Kinesis.DisableEnhancedMonitoring
import Amazonka.Kinesis.EnableEnhancedMonitoring
import Amazonka.Kinesis.GetRecords
import Amazonka.Kinesis.GetShardIterator
import Amazonka.Kinesis.IncreaseStreamRetentionPeriod
import Amazonka.Kinesis.ListShards
import Amazonka.Kinesis.ListStreamConsumers
import Amazonka.Kinesis.ListStreams
import Amazonka.Kinesis.ListTagsForStream
import Amazonka.Kinesis.MergeShards
import Amazonka.Kinesis.PutRecord
import Amazonka.Kinesis.PutRecords
import Amazonka.Kinesis.RegisterStreamConsumer
import Amazonka.Kinesis.RemoveTagsFromStream
import Amazonka.Kinesis.SplitShard
import Amazonka.Kinesis.StartStreamEncryption
import Amazonka.Kinesis.StopStreamEncryption
import Amazonka.Kinesis.SubscribeToShard
import Amazonka.Kinesis.Types.ChildShard
import Amazonka.Kinesis.Types.Consumer
import Amazonka.Kinesis.Types.ConsumerDescription
import Amazonka.Kinesis.Types.EnhancedMetrics
import Amazonka.Kinesis.Types.EnhancedMonitoringOutput
import Amazonka.Kinesis.Types.HashKeyRange
import Amazonka.Kinesis.Types.PutRecordsRequestEntry
import Amazonka.Kinesis.Types.PutRecordsResultEntry
import Amazonka.Kinesis.Types.Record
import Amazonka.Kinesis.Types.SequenceNumberRange
import Amazonka.Kinesis.Types.Shard
import Amazonka.Kinesis.Types.ShardFilter
import Amazonka.Kinesis.Types.StartingPosition
import Amazonka.Kinesis.Types.StreamDescription
import Amazonka.Kinesis.Types.StreamDescriptionSummary
import Amazonka.Kinesis.Types.StreamModeDetails
import Amazonka.Kinesis.Types.StreamSummary
import Amazonka.Kinesis.Types.SubscribeToShardEvent
import Amazonka.Kinesis.Types.Tag
import Amazonka.Kinesis.UpdateShardCount
import Amazonka.Kinesis.UpdateStreamMode

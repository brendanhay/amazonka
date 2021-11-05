{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Kinesis
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2013-12-02@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Kinesis Data Streams Service API Reference
--
-- Amazon Kinesis Data Streams is a managed service that scales elastically
-- for real-time processing of streaming big data.
module Amazonka.Kinesis
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** KMSInvalidStateException
    _KMSInvalidStateException,

    -- ** KMSThrottlingException
    _KMSThrottlingException,

    -- ** ExpiredIteratorException
    _ExpiredIteratorException,

    -- ** InvalidArgumentException
    _InvalidArgumentException,

    -- ** KMSOptInRequired
    _KMSOptInRequired,

    -- ** ProvisionedThroughputExceededException
    _ProvisionedThroughputExceededException,

    -- ** KMSNotFoundException
    _KMSNotFoundException,

    -- ** ExpiredNextTokenException
    _ExpiredNextTokenException,

    -- ** KMSDisabledException
    _KMSDisabledException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** KMSAccessDeniedException
    _KMSAccessDeniedException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- * Waiters
    -- $waiters

    -- ** StreamExists
    newStreamExists,

    -- ** StreamNotExists
    newStreamNotExists,

    -- * Operations
    -- $operations

    -- ** PutRecord
    PutRecord (PutRecord'),
    newPutRecord,
    PutRecordResponse (PutRecordResponse'),
    newPutRecordResponse,

    -- ** SubscribeToShard
    SubscribeToShard (SubscribeToShard'),
    newSubscribeToShard,
    SubscribeToShardResponse (SubscribeToShardResponse'),
    newSubscribeToShardResponse,

    -- ** DecreaseStreamRetentionPeriod
    DecreaseStreamRetentionPeriod (DecreaseStreamRetentionPeriod'),
    newDecreaseStreamRetentionPeriod,
    DecreaseStreamRetentionPeriodResponse (DecreaseStreamRetentionPeriodResponse'),
    newDecreaseStreamRetentionPeriodResponse,

    -- ** MergeShards
    MergeShards (MergeShards'),
    newMergeShards,
    MergeShardsResponse (MergeShardsResponse'),
    newMergeShardsResponse,

    -- ** DeregisterStreamConsumer
    DeregisterStreamConsumer (DeregisterStreamConsumer'),
    newDeregisterStreamConsumer,
    DeregisterStreamConsumerResponse (DeregisterStreamConsumerResponse'),
    newDeregisterStreamConsumerResponse,

    -- ** DescribeStreamSummary
    DescribeStreamSummary (DescribeStreamSummary'),
    newDescribeStreamSummary,
    DescribeStreamSummaryResponse (DescribeStreamSummaryResponse'),
    newDescribeStreamSummaryResponse,

    -- ** GetShardIterator
    GetShardIterator (GetShardIterator'),
    newGetShardIterator,
    GetShardIteratorResponse (GetShardIteratorResponse'),
    newGetShardIteratorResponse,

    -- ** GetRecords
    GetRecords (GetRecords'),
    newGetRecords,
    GetRecordsResponse (GetRecordsResponse'),
    newGetRecordsResponse,

    -- ** StopStreamEncryption
    StopStreamEncryption (StopStreamEncryption'),
    newStopStreamEncryption,
    StopStreamEncryptionResponse (StopStreamEncryptionResponse'),
    newStopStreamEncryptionResponse,

    -- ** EnableEnhancedMonitoring
    EnableEnhancedMonitoring (EnableEnhancedMonitoring'),
    newEnableEnhancedMonitoring,
    EnhancedMonitoringOutput (EnhancedMonitoringOutput'),
    newEnhancedMonitoringOutput,

    -- ** DescribeLimits
    DescribeLimits (DescribeLimits'),
    newDescribeLimits,
    DescribeLimitsResponse (DescribeLimitsResponse'),
    newDescribeLimitsResponse,

    -- ** RegisterStreamConsumer
    RegisterStreamConsumer (RegisterStreamConsumer'),
    newRegisterStreamConsumer,
    RegisterStreamConsumerResponse (RegisterStreamConsumerResponse'),
    newRegisterStreamConsumerResponse,

    -- ** DisableEnhancedMonitoring
    DisableEnhancedMonitoring (DisableEnhancedMonitoring'),
    newDisableEnhancedMonitoring,
    EnhancedMonitoringOutput (EnhancedMonitoringOutput'),
    newEnhancedMonitoringOutput,

    -- ** UpdateShardCount
    UpdateShardCount (UpdateShardCount'),
    newUpdateShardCount,
    UpdateShardCountResponse (UpdateShardCountResponse'),
    newUpdateShardCountResponse,

    -- ** ListTagsForStream
    ListTagsForStream (ListTagsForStream'),
    newListTagsForStream,
    ListTagsForStreamResponse (ListTagsForStreamResponse'),
    newListTagsForStreamResponse,

    -- ** DescribeStreamConsumer
    DescribeStreamConsumer (DescribeStreamConsumer'),
    newDescribeStreamConsumer,
    DescribeStreamConsumerResponse (DescribeStreamConsumerResponse'),
    newDescribeStreamConsumerResponse,

    -- ** AddTagsToStream
    AddTagsToStream (AddTagsToStream'),
    newAddTagsToStream,
    AddTagsToStreamResponse (AddTagsToStreamResponse'),
    newAddTagsToStreamResponse,

    -- ** PutRecords
    PutRecords (PutRecords'),
    newPutRecords,
    PutRecordsResponse (PutRecordsResponse'),
    newPutRecordsResponse,

    -- ** ListShards (Paginated)
    ListShards (ListShards'),
    newListShards,
    ListShardsResponse (ListShardsResponse'),
    newListShardsResponse,

    -- ** DeleteStream
    DeleteStream (DeleteStream'),
    newDeleteStream,
    DeleteStreamResponse (DeleteStreamResponse'),
    newDeleteStreamResponse,

    -- ** RemoveTagsFromStream
    RemoveTagsFromStream (RemoveTagsFromStream'),
    newRemoveTagsFromStream,
    RemoveTagsFromStreamResponse (RemoveTagsFromStreamResponse'),
    newRemoveTagsFromStreamResponse,

    -- ** ListStreams (Paginated)
    ListStreams (ListStreams'),
    newListStreams,
    ListStreamsResponse (ListStreamsResponse'),
    newListStreamsResponse,

    -- ** CreateStream
    CreateStream (CreateStream'),
    newCreateStream,
    CreateStreamResponse (CreateStreamResponse'),
    newCreateStreamResponse,

    -- ** StartStreamEncryption
    StartStreamEncryption (StartStreamEncryption'),
    newStartStreamEncryption,
    StartStreamEncryptionResponse (StartStreamEncryptionResponse'),
    newStartStreamEncryptionResponse,

    -- ** ListStreamConsumers (Paginated)
    ListStreamConsumers (ListStreamConsumers'),
    newListStreamConsumers,
    ListStreamConsumersResponse (ListStreamConsumersResponse'),
    newListStreamConsumersResponse,

    -- ** SplitShard
    SplitShard (SplitShard'),
    newSplitShard,
    SplitShardResponse (SplitShardResponse'),
    newSplitShardResponse,

    -- ** IncreaseStreamRetentionPeriod
    IncreaseStreamRetentionPeriod (IncreaseStreamRetentionPeriod'),
    newIncreaseStreamRetentionPeriod,
    IncreaseStreamRetentionPeriodResponse (IncreaseStreamRetentionPeriodResponse'),
    newIncreaseStreamRetentionPeriodResponse,

    -- ** DescribeStream (Paginated)
    DescribeStream (DescribeStream'),
    newDescribeStream,
    DescribeStreamResponse (DescribeStreamResponse'),
    newDescribeStreamResponse,

    -- * Types

    -- ** ConsumerStatus
    ConsumerStatus (..),

    -- ** EncryptionType
    EncryptionType (..),

    -- ** MetricsName
    MetricsName (..),

    -- ** ScalingType
    ScalingType (..),

    -- ** ShardFilterType
    ShardFilterType (..),

    -- ** ShardIteratorType
    ShardIteratorType (..),

    -- ** StreamStatus
    StreamStatus (..),

    -- ** ChildShard
    ChildShard (ChildShard'),
    newChildShard,

    -- ** Consumer
    Consumer (Consumer'),
    newConsumer,

    -- ** ConsumerDescription
    ConsumerDescription (ConsumerDescription'),
    newConsumerDescription,

    -- ** EnhancedMetrics
    EnhancedMetrics (EnhancedMetrics'),
    newEnhancedMetrics,

    -- ** EnhancedMonitoringOutput
    EnhancedMonitoringOutput (EnhancedMonitoringOutput'),
    newEnhancedMonitoringOutput,

    -- ** HashKeyRange
    HashKeyRange (HashKeyRange'),
    newHashKeyRange,

    -- ** PutRecordsRequestEntry
    PutRecordsRequestEntry (PutRecordsRequestEntry'),
    newPutRecordsRequestEntry,

    -- ** PutRecordsResultEntry
    PutRecordsResultEntry (PutRecordsResultEntry'),
    newPutRecordsResultEntry,

    -- ** Record
    Record (Record'),
    newRecord,

    -- ** SequenceNumberRange
    SequenceNumberRange (SequenceNumberRange'),
    newSequenceNumberRange,

    -- ** Shard
    Shard (Shard'),
    newShard,

    -- ** ShardFilter
    ShardFilter (ShardFilter'),
    newShardFilter,

    -- ** StartingPosition
    StartingPosition (StartingPosition'),
    newStartingPosition,

    -- ** StreamDescription
    StreamDescription (StreamDescription'),
    newStreamDescription,

    -- ** StreamDescriptionSummary
    StreamDescriptionSummary (StreamDescriptionSummary'),
    newStreamDescriptionSummary,

    -- ** SubscribeToShardEvent
    SubscribeToShardEvent (SubscribeToShardEvent'),
    newSubscribeToShardEvent,

    -- ** Tag
    Tag (Tag'),
    newTag,
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
import Amazonka.Kinesis.Lens
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
import Amazonka.Kinesis.Types
import Amazonka.Kinesis.UpdateShardCount
import Amazonka.Kinesis.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Kinesis'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

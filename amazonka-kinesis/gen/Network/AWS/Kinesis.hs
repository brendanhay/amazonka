{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Kinesis
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
module Network.AWS.Kinesis
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** KMSThrottlingException
    _KMSThrottlingException,

    -- ** ExpiredNextTokenException
    _ExpiredNextTokenException,

    -- ** KMSInvalidStateException
    _KMSInvalidStateException,

    -- ** KMSNotFoundException
    _KMSNotFoundException,

    -- ** KMSOptInRequired
    _KMSOptInRequired,

    -- ** ExpiredIteratorException
    _ExpiredIteratorException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** KMSAccessDeniedException
    _KMSAccessDeniedException,

    -- ** ProvisionedThroughputExceededException
    _ProvisionedThroughputExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** InvalidArgumentException
    _InvalidArgumentException,

    -- ** KMSDisabledException
    _KMSDisabledException,

    -- * Waiters
    -- $waiters

    -- ** StreamExists
    newStreamExists,

    -- ** StreamNotExists
    newStreamNotExists,

    -- * Operations
    -- $operations

    -- ** SubscribeToShard
    SubscribeToShard (SubscribeToShard'),
    newSubscribeToShard,
    SubscribeToShardResponse (SubscribeToShardResponse'),
    newSubscribeToShardResponse,

    -- ** AddTagsToStream
    AddTagsToStream (AddTagsToStream'),
    newAddTagsToStream,
    AddTagsToStreamResponse (AddTagsToStreamResponse'),
    newAddTagsToStreamResponse,

    -- ** ListTagsForStream
    ListTagsForStream (ListTagsForStream'),
    newListTagsForStream,
    ListTagsForStreamResponse (ListTagsForStreamResponse'),
    newListTagsForStreamResponse,

    -- ** IncreaseStreamRetentionPeriod
    IncreaseStreamRetentionPeriod (IncreaseStreamRetentionPeriod'),
    newIncreaseStreamRetentionPeriod,
    IncreaseStreamRetentionPeriodResponse (IncreaseStreamRetentionPeriodResponse'),
    newIncreaseStreamRetentionPeriodResponse,

    -- ** DisableEnhancedMonitoring
    DisableEnhancedMonitoring (DisableEnhancedMonitoring'),
    newDisableEnhancedMonitoring,
    EnhancedMonitoringOutput (EnhancedMonitoringOutput'),
    newEnhancedMonitoringOutput,

    -- ** SplitShard
    SplitShard (SplitShard'),
    newSplitShard,
    SplitShardResponse (SplitShardResponse'),
    newSplitShardResponse,

    -- ** ListStreamConsumers (Paginated)
    ListStreamConsumers (ListStreamConsumers'),
    newListStreamConsumers,
    ListStreamConsumersResponse (ListStreamConsumersResponse'),
    newListStreamConsumersResponse,

    -- ** DescribeLimits
    DescribeLimits (DescribeLimits'),
    newDescribeLimits,
    DescribeLimitsResponse (DescribeLimitsResponse'),
    newDescribeLimitsResponse,

    -- ** StopStreamEncryption
    StopStreamEncryption (StopStreamEncryption'),
    newStopStreamEncryption,
    StopStreamEncryptionResponse (StopStreamEncryptionResponse'),
    newStopStreamEncryptionResponse,

    -- ** RegisterStreamConsumer
    RegisterStreamConsumer (RegisterStreamConsumer'),
    newRegisterStreamConsumer,
    RegisterStreamConsumerResponse (RegisterStreamConsumerResponse'),
    newRegisterStreamConsumerResponse,

    -- ** StartStreamEncryption
    StartStreamEncryption (StartStreamEncryption'),
    newStartStreamEncryption,
    StartStreamEncryptionResponse (StartStreamEncryptionResponse'),
    newStartStreamEncryptionResponse,

    -- ** EnableEnhancedMonitoring
    EnableEnhancedMonitoring (EnableEnhancedMonitoring'),
    newEnableEnhancedMonitoring,
    EnhancedMonitoringOutput (EnhancedMonitoringOutput'),
    newEnhancedMonitoringOutput,

    -- ** GetRecords
    GetRecords (GetRecords'),
    newGetRecords,
    GetRecordsResponse (GetRecordsResponse'),
    newGetRecordsResponse,

    -- ** GetShardIterator
    GetShardIterator (GetShardIterator'),
    newGetShardIterator,
    GetShardIteratorResponse (GetShardIteratorResponse'),
    newGetShardIteratorResponse,

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

    -- ** DescribeStreamSummary
    DescribeStreamSummary (DescribeStreamSummary'),
    newDescribeStreamSummary,
    DescribeStreamSummaryResponse (DescribeStreamSummaryResponse'),
    newDescribeStreamSummaryResponse,

    -- ** DeregisterStreamConsumer
    DeregisterStreamConsumer (DeregisterStreamConsumer'),
    newDeregisterStreamConsumer,
    DeregisterStreamConsumerResponse (DeregisterStreamConsumerResponse'),
    newDeregisterStreamConsumerResponse,

    -- ** PutRecords
    PutRecords (PutRecords'),
    newPutRecords,
    PutRecordsResponse (PutRecordsResponse'),
    newPutRecordsResponse,

    -- ** MergeShards
    MergeShards (MergeShards'),
    newMergeShards,
    MergeShardsResponse (MergeShardsResponse'),
    newMergeShardsResponse,

    -- ** DescribeStreamConsumer
    DescribeStreamConsumer (DescribeStreamConsumer'),
    newDescribeStreamConsumer,
    DescribeStreamConsumerResponse (DescribeStreamConsumerResponse'),
    newDescribeStreamConsumerResponse,

    -- ** DecreaseStreamRetentionPeriod
    DecreaseStreamRetentionPeriod (DecreaseStreamRetentionPeriod'),
    newDecreaseStreamRetentionPeriod,
    DecreaseStreamRetentionPeriodResponse (DecreaseStreamRetentionPeriodResponse'),
    newDecreaseStreamRetentionPeriodResponse,

    -- ** PutRecord
    PutRecord (PutRecord'),
    newPutRecord,
    PutRecordResponse (PutRecordResponse'),
    newPutRecordResponse,

    -- ** DescribeStream (Paginated)
    DescribeStream (DescribeStream'),
    newDescribeStream,
    DescribeStreamResponse (DescribeStreamResponse'),
    newDescribeStreamResponse,

    -- ** UpdateShardCount
    UpdateShardCount (UpdateShardCount'),
    newUpdateShardCount,
    UpdateShardCountResponse (UpdateShardCountResponse'),
    newUpdateShardCountResponse,

    -- ** CreateStream
    CreateStream (CreateStream'),
    newCreateStream,
    CreateStreamResponse (CreateStreamResponse'),
    newCreateStreamResponse,

    -- ** ListStreams (Paginated)
    ListStreams (ListStreams'),
    newListStreams,
    ListStreamsResponse (ListStreamsResponse'),
    newListStreamsResponse,

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
import Network.AWS.Kinesis.Lens
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
import Network.AWS.Kinesis.Types
import Network.AWS.Kinesis.UpdateShardCount
import Network.AWS.Kinesis.Waiters

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

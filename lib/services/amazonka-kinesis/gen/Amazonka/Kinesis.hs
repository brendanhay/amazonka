{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Kinesis
-- Copyright   : (c) 2013-2022 Brendan Hay
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

    -- ** ExpiredIteratorException
    _ExpiredIteratorException,

    -- ** ExpiredNextTokenException
    _ExpiredNextTokenException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** InvalidArgumentException
    _InvalidArgumentException,

    -- ** KMSAccessDeniedException
    _KMSAccessDeniedException,

    -- ** KMSDisabledException
    _KMSDisabledException,

    -- ** KMSInvalidStateException
    _KMSInvalidStateException,

    -- ** KMSNotFoundException
    _KMSNotFoundException,

    -- ** KMSOptInRequired
    _KMSOptInRequired,

    -- ** KMSThrottlingException
    _KMSThrottlingException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ProvisionedThroughputExceededException
    _ProvisionedThroughputExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- ** StreamExists
    newStreamExists,

    -- ** StreamNotExists
    newStreamNotExists,

    -- * Operations
    -- $operations

    -- ** AddTagsToStream
    AddTagsToStream (AddTagsToStream'),
    newAddTagsToStream,
    AddTagsToStreamResponse (AddTagsToStreamResponse'),
    newAddTagsToStreamResponse,

    -- ** CreateStream
    CreateStream (CreateStream'),
    newCreateStream,
    CreateStreamResponse (CreateStreamResponse'),
    newCreateStreamResponse,

    -- ** DecreaseStreamRetentionPeriod
    DecreaseStreamRetentionPeriod (DecreaseStreamRetentionPeriod'),
    newDecreaseStreamRetentionPeriod,
    DecreaseStreamRetentionPeriodResponse (DecreaseStreamRetentionPeriodResponse'),
    newDecreaseStreamRetentionPeriodResponse,

    -- ** DeleteStream
    DeleteStream (DeleteStream'),
    newDeleteStream,
    DeleteStreamResponse (DeleteStreamResponse'),
    newDeleteStreamResponse,

    -- ** DeregisterStreamConsumer
    DeregisterStreamConsumer (DeregisterStreamConsumer'),
    newDeregisterStreamConsumer,
    DeregisterStreamConsumerResponse (DeregisterStreamConsumerResponse'),
    newDeregisterStreamConsumerResponse,

    -- ** DescribeLimits
    DescribeLimits (DescribeLimits'),
    newDescribeLimits,
    DescribeLimitsResponse (DescribeLimitsResponse'),
    newDescribeLimitsResponse,

    -- ** DescribeStream (Paginated)
    DescribeStream (DescribeStream'),
    newDescribeStream,
    DescribeStreamResponse (DescribeStreamResponse'),
    newDescribeStreamResponse,

    -- ** DescribeStreamConsumer
    DescribeStreamConsumer (DescribeStreamConsumer'),
    newDescribeStreamConsumer,
    DescribeStreamConsumerResponse (DescribeStreamConsumerResponse'),
    newDescribeStreamConsumerResponse,

    -- ** DescribeStreamSummary
    DescribeStreamSummary (DescribeStreamSummary'),
    newDescribeStreamSummary,
    DescribeStreamSummaryResponse (DescribeStreamSummaryResponse'),
    newDescribeStreamSummaryResponse,

    -- ** DisableEnhancedMonitoring
    DisableEnhancedMonitoring (DisableEnhancedMonitoring'),
    newDisableEnhancedMonitoring,
    EnhancedMonitoringOutput (EnhancedMonitoringOutput'),
    newEnhancedMonitoringOutput,

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

    -- ** IncreaseStreamRetentionPeriod
    IncreaseStreamRetentionPeriod (IncreaseStreamRetentionPeriod'),
    newIncreaseStreamRetentionPeriod,
    IncreaseStreamRetentionPeriodResponse (IncreaseStreamRetentionPeriodResponse'),
    newIncreaseStreamRetentionPeriodResponse,

    -- ** ListShards (Paginated)
    ListShards (ListShards'),
    newListShards,
    ListShardsResponse (ListShardsResponse'),
    newListShardsResponse,

    -- ** ListStreamConsumers (Paginated)
    ListStreamConsumers (ListStreamConsumers'),
    newListStreamConsumers,
    ListStreamConsumersResponse (ListStreamConsumersResponse'),
    newListStreamConsumersResponse,

    -- ** ListStreams (Paginated)
    ListStreams (ListStreams'),
    newListStreams,
    ListStreamsResponse (ListStreamsResponse'),
    newListStreamsResponse,

    -- ** ListTagsForStream
    ListTagsForStream (ListTagsForStream'),
    newListTagsForStream,
    ListTagsForStreamResponse (ListTagsForStreamResponse'),
    newListTagsForStreamResponse,

    -- ** MergeShards
    MergeShards (MergeShards'),
    newMergeShards,
    MergeShardsResponse (MergeShardsResponse'),
    newMergeShardsResponse,

    -- ** PutRecord
    PutRecord (PutRecord'),
    newPutRecord,
    PutRecordResponse (PutRecordResponse'),
    newPutRecordResponse,

    -- ** PutRecords
    PutRecords (PutRecords'),
    newPutRecords,
    PutRecordsResponse (PutRecordsResponse'),
    newPutRecordsResponse,

    -- ** RegisterStreamConsumer
    RegisterStreamConsumer (RegisterStreamConsumer'),
    newRegisterStreamConsumer,
    RegisterStreamConsumerResponse (RegisterStreamConsumerResponse'),
    newRegisterStreamConsumerResponse,

    -- ** RemoveTagsFromStream
    RemoveTagsFromStream (RemoveTagsFromStream'),
    newRemoveTagsFromStream,
    RemoveTagsFromStreamResponse (RemoveTagsFromStreamResponse'),
    newRemoveTagsFromStreamResponse,

    -- ** SplitShard
    SplitShard (SplitShard'),
    newSplitShard,
    SplitShardResponse (SplitShardResponse'),
    newSplitShardResponse,

    -- ** StartStreamEncryption
    StartStreamEncryption (StartStreamEncryption'),
    newStartStreamEncryption,
    StartStreamEncryptionResponse (StartStreamEncryptionResponse'),
    newStartStreamEncryptionResponse,

    -- ** StopStreamEncryption
    StopStreamEncryption (StopStreamEncryption'),
    newStopStreamEncryption,
    StopStreamEncryptionResponse (StopStreamEncryptionResponse'),
    newStopStreamEncryptionResponse,

    -- ** SubscribeToShard
    SubscribeToShard (SubscribeToShard'),
    newSubscribeToShard,
    SubscribeToShardResponse (SubscribeToShardResponse'),
    newSubscribeToShardResponse,

    -- ** UpdateShardCount
    UpdateShardCount (UpdateShardCount'),
    newUpdateShardCount,
    UpdateShardCountResponse (UpdateShardCountResponse'),
    newUpdateShardCountResponse,

    -- ** UpdateStreamMode
    UpdateStreamMode (UpdateStreamMode'),
    newUpdateStreamMode,
    UpdateStreamModeResponse (UpdateStreamModeResponse'),
    newUpdateStreamModeResponse,

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

    -- ** StreamMode
    StreamMode (..),

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

    -- ** StreamModeDetails
    StreamModeDetails (StreamModeDetails'),
    newStreamModeDetails,

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
import Amazonka.Kinesis.UpdateStreamMode
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

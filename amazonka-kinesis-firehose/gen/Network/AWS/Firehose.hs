{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Kinesis Data Firehose API Reference
--
-- Amazon Kinesis Data Firehose is a fully managed service that delivers
-- real-time streaming data to destinations such as Amazon Simple Storage
-- Service (Amazon S3), Amazon Elasticsearch Service (Amazon ES), Amazon
-- Redshift, and Splunk.
module Network.AWS.Firehose
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** InvalidKMSResourceException
    _InvalidKMSResourceException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InvalidArgumentException
    _InvalidArgumentException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** StartDeliveryStreamEncryption
    StartDeliveryStreamEncryption (StartDeliveryStreamEncryption'),
    newStartDeliveryStreamEncryption,
    StartDeliveryStreamEncryptionResponse (StartDeliveryStreamEncryptionResponse'),
    newStartDeliveryStreamEncryptionResponse,

    -- ** StopDeliveryStreamEncryption
    StopDeliveryStreamEncryption (StopDeliveryStreamEncryption'),
    newStopDeliveryStreamEncryption,
    StopDeliveryStreamEncryptionResponse (StopDeliveryStreamEncryptionResponse'),
    newStopDeliveryStreamEncryptionResponse,

    -- ** ListDeliveryStreams
    ListDeliveryStreams (ListDeliveryStreams'),
    newListDeliveryStreams,
    ListDeliveryStreamsResponse (ListDeliveryStreamsResponse'),
    newListDeliveryStreamsResponse,

    -- ** PutRecordBatch
    PutRecordBatch (PutRecordBatch'),
    newPutRecordBatch,
    PutRecordBatchResponse (PutRecordBatchResponse'),
    newPutRecordBatchResponse,

    -- ** ListTagsForDeliveryStream
    ListTagsForDeliveryStream (ListTagsForDeliveryStream'),
    newListTagsForDeliveryStream,
    ListTagsForDeliveryStreamResponse (ListTagsForDeliveryStreamResponse'),
    newListTagsForDeliveryStreamResponse,

    -- ** UpdateDestination
    UpdateDestination (UpdateDestination'),
    newUpdateDestination,
    UpdateDestinationResponse (UpdateDestinationResponse'),
    newUpdateDestinationResponse,

    -- ** DescribeDeliveryStream
    DescribeDeliveryStream (DescribeDeliveryStream'),
    newDescribeDeliveryStream,
    DescribeDeliveryStreamResponse (DescribeDeliveryStreamResponse'),
    newDescribeDeliveryStreamResponse,

    -- ** CreateDeliveryStream
    CreateDeliveryStream (CreateDeliveryStream'),
    newCreateDeliveryStream,
    CreateDeliveryStreamResponse (CreateDeliveryStreamResponse'),
    newCreateDeliveryStreamResponse,

    -- ** PutRecord
    PutRecord (PutRecord'),
    newPutRecord,
    PutRecordResponse (PutRecordResponse'),
    newPutRecordResponse,

    -- ** DeleteDeliveryStream
    DeleteDeliveryStream (DeleteDeliveryStream'),
    newDeleteDeliveryStream,
    DeleteDeliveryStreamResponse (DeleteDeliveryStreamResponse'),
    newDeleteDeliveryStreamResponse,

    -- ** UntagDeliveryStream
    UntagDeliveryStream (UntagDeliveryStream'),
    newUntagDeliveryStream,
    UntagDeliveryStreamResponse (UntagDeliveryStreamResponse'),
    newUntagDeliveryStreamResponse,

    -- ** TagDeliveryStream
    TagDeliveryStream (TagDeliveryStream'),
    newTagDeliveryStream,
    TagDeliveryStreamResponse (TagDeliveryStreamResponse'),
    newTagDeliveryStreamResponse,

    -- * Types

    -- ** CompressionFormat
    CompressionFormat (..),

    -- ** ContentEncoding
    ContentEncoding (..),

    -- ** DeliveryStreamEncryptionStatus
    DeliveryStreamEncryptionStatus (..),

    -- ** DeliveryStreamFailureType
    DeliveryStreamFailureType (..),

    -- ** DeliveryStreamStatus
    DeliveryStreamStatus (..),

    -- ** DeliveryStreamType
    DeliveryStreamType (..),

    -- ** ElasticsearchIndexRotationPeriod
    ElasticsearchIndexRotationPeriod (..),

    -- ** ElasticsearchS3BackupMode
    ElasticsearchS3BackupMode (..),

    -- ** HECEndpointType
    HECEndpointType (..),

    -- ** HttpEndpointS3BackupMode
    HttpEndpointS3BackupMode (..),

    -- ** KeyType
    KeyType (..),

    -- ** NoEncryptionConfig
    NoEncryptionConfig (..),

    -- ** OrcCompression
    OrcCompression (..),

    -- ** OrcFormatVersion
    OrcFormatVersion (..),

    -- ** ParquetCompression
    ParquetCompression (..),

    -- ** ParquetWriterVersion
    ParquetWriterVersion (..),

    -- ** ProcessorParameterName
    ProcessorParameterName (..),

    -- ** ProcessorType
    ProcessorType (..),

    -- ** RedshiftS3BackupMode
    RedshiftS3BackupMode (..),

    -- ** S3BackupMode
    S3BackupMode (..),

    -- ** SplunkS3BackupMode
    SplunkS3BackupMode (..),

    -- ** BufferingHints
    BufferingHints (BufferingHints'),
    newBufferingHints,

    -- ** CloudWatchLoggingOptions
    CloudWatchLoggingOptions (CloudWatchLoggingOptions'),
    newCloudWatchLoggingOptions,

    -- ** CopyCommand
    CopyCommand (CopyCommand'),
    newCopyCommand,

    -- ** DataFormatConversionConfiguration
    DataFormatConversionConfiguration (DataFormatConversionConfiguration'),
    newDataFormatConversionConfiguration,

    -- ** DeliveryStreamDescription
    DeliveryStreamDescription (DeliveryStreamDescription'),
    newDeliveryStreamDescription,

    -- ** DeliveryStreamEncryptionConfiguration
    DeliveryStreamEncryptionConfiguration (DeliveryStreamEncryptionConfiguration'),
    newDeliveryStreamEncryptionConfiguration,

    -- ** DeliveryStreamEncryptionConfigurationInput
    DeliveryStreamEncryptionConfigurationInput (DeliveryStreamEncryptionConfigurationInput'),
    newDeliveryStreamEncryptionConfigurationInput,

    -- ** Deserializer
    Deserializer (Deserializer'),
    newDeserializer,

    -- ** DestinationDescription
    DestinationDescription (DestinationDescription'),
    newDestinationDescription,

    -- ** ElasticsearchBufferingHints
    ElasticsearchBufferingHints (ElasticsearchBufferingHints'),
    newElasticsearchBufferingHints,

    -- ** ElasticsearchDestinationConfiguration
    ElasticsearchDestinationConfiguration (ElasticsearchDestinationConfiguration'),
    newElasticsearchDestinationConfiguration,

    -- ** ElasticsearchDestinationDescription
    ElasticsearchDestinationDescription (ElasticsearchDestinationDescription'),
    newElasticsearchDestinationDescription,

    -- ** ElasticsearchDestinationUpdate
    ElasticsearchDestinationUpdate (ElasticsearchDestinationUpdate'),
    newElasticsearchDestinationUpdate,

    -- ** ElasticsearchRetryOptions
    ElasticsearchRetryOptions (ElasticsearchRetryOptions'),
    newElasticsearchRetryOptions,

    -- ** EncryptionConfiguration
    EncryptionConfiguration (EncryptionConfiguration'),
    newEncryptionConfiguration,

    -- ** ExtendedS3DestinationConfiguration
    ExtendedS3DestinationConfiguration (ExtendedS3DestinationConfiguration'),
    newExtendedS3DestinationConfiguration,

    -- ** ExtendedS3DestinationDescription
    ExtendedS3DestinationDescription (ExtendedS3DestinationDescription'),
    newExtendedS3DestinationDescription,

    -- ** ExtendedS3DestinationUpdate
    ExtendedS3DestinationUpdate (ExtendedS3DestinationUpdate'),
    newExtendedS3DestinationUpdate,

    -- ** FailureDescription
    FailureDescription (FailureDescription'),
    newFailureDescription,

    -- ** HiveJsonSerDe
    HiveJsonSerDe (HiveJsonSerDe'),
    newHiveJsonSerDe,

    -- ** HttpEndpointBufferingHints
    HttpEndpointBufferingHints (HttpEndpointBufferingHints'),
    newHttpEndpointBufferingHints,

    -- ** HttpEndpointCommonAttribute
    HttpEndpointCommonAttribute (HttpEndpointCommonAttribute'),
    newHttpEndpointCommonAttribute,

    -- ** HttpEndpointConfiguration
    HttpEndpointConfiguration (HttpEndpointConfiguration'),
    newHttpEndpointConfiguration,

    -- ** HttpEndpointDescription
    HttpEndpointDescription (HttpEndpointDescription'),
    newHttpEndpointDescription,

    -- ** HttpEndpointDestinationConfiguration
    HttpEndpointDestinationConfiguration (HttpEndpointDestinationConfiguration'),
    newHttpEndpointDestinationConfiguration,

    -- ** HttpEndpointDestinationDescription
    HttpEndpointDestinationDescription (HttpEndpointDestinationDescription'),
    newHttpEndpointDestinationDescription,

    -- ** HttpEndpointDestinationUpdate
    HttpEndpointDestinationUpdate (HttpEndpointDestinationUpdate'),
    newHttpEndpointDestinationUpdate,

    -- ** HttpEndpointRequestConfiguration
    HttpEndpointRequestConfiguration (HttpEndpointRequestConfiguration'),
    newHttpEndpointRequestConfiguration,

    -- ** HttpEndpointRetryOptions
    HttpEndpointRetryOptions (HttpEndpointRetryOptions'),
    newHttpEndpointRetryOptions,

    -- ** InputFormatConfiguration
    InputFormatConfiguration (InputFormatConfiguration'),
    newInputFormatConfiguration,

    -- ** KMSEncryptionConfig
    KMSEncryptionConfig (KMSEncryptionConfig'),
    newKMSEncryptionConfig,

    -- ** KinesisStreamSourceConfiguration
    KinesisStreamSourceConfiguration (KinesisStreamSourceConfiguration'),
    newKinesisStreamSourceConfiguration,

    -- ** KinesisStreamSourceDescription
    KinesisStreamSourceDescription (KinesisStreamSourceDescription'),
    newKinesisStreamSourceDescription,

    -- ** OpenXJsonSerDe
    OpenXJsonSerDe (OpenXJsonSerDe'),
    newOpenXJsonSerDe,

    -- ** OrcSerDe
    OrcSerDe (OrcSerDe'),
    newOrcSerDe,

    -- ** OutputFormatConfiguration
    OutputFormatConfiguration (OutputFormatConfiguration'),
    newOutputFormatConfiguration,

    -- ** ParquetSerDe
    ParquetSerDe (ParquetSerDe'),
    newParquetSerDe,

    -- ** ProcessingConfiguration
    ProcessingConfiguration (ProcessingConfiguration'),
    newProcessingConfiguration,

    -- ** Processor
    Processor (Processor'),
    newProcessor,

    -- ** ProcessorParameter
    ProcessorParameter (ProcessorParameter'),
    newProcessorParameter,

    -- ** PutRecordBatchResponseEntry
    PutRecordBatchResponseEntry (PutRecordBatchResponseEntry'),
    newPutRecordBatchResponseEntry,

    -- ** Record
    Record (Record'),
    newRecord,

    -- ** RedshiftDestinationConfiguration
    RedshiftDestinationConfiguration (RedshiftDestinationConfiguration'),
    newRedshiftDestinationConfiguration,

    -- ** RedshiftDestinationDescription
    RedshiftDestinationDescription (RedshiftDestinationDescription'),
    newRedshiftDestinationDescription,

    -- ** RedshiftDestinationUpdate
    RedshiftDestinationUpdate (RedshiftDestinationUpdate'),
    newRedshiftDestinationUpdate,

    -- ** RedshiftRetryOptions
    RedshiftRetryOptions (RedshiftRetryOptions'),
    newRedshiftRetryOptions,

    -- ** S3DestinationConfiguration
    S3DestinationConfiguration (S3DestinationConfiguration'),
    newS3DestinationConfiguration,

    -- ** S3DestinationDescription
    S3DestinationDescription (S3DestinationDescription'),
    newS3DestinationDescription,

    -- ** S3DestinationUpdate
    S3DestinationUpdate (S3DestinationUpdate'),
    newS3DestinationUpdate,

    -- ** SchemaConfiguration
    SchemaConfiguration (SchemaConfiguration'),
    newSchemaConfiguration,

    -- ** Serializer
    Serializer (Serializer'),
    newSerializer,

    -- ** SourceDescription
    SourceDescription (SourceDescription'),
    newSourceDescription,

    -- ** SplunkDestinationConfiguration
    SplunkDestinationConfiguration (SplunkDestinationConfiguration'),
    newSplunkDestinationConfiguration,

    -- ** SplunkDestinationDescription
    SplunkDestinationDescription (SplunkDestinationDescription'),
    newSplunkDestinationDescription,

    -- ** SplunkDestinationUpdate
    SplunkDestinationUpdate (SplunkDestinationUpdate'),
    newSplunkDestinationUpdate,

    -- ** SplunkRetryOptions
    SplunkRetryOptions (SplunkRetryOptions'),
    newSplunkRetryOptions,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** VpcConfiguration
    VpcConfiguration (VpcConfiguration'),
    newVpcConfiguration,

    -- ** VpcConfigurationDescription
    VpcConfigurationDescription (VpcConfigurationDescription'),
    newVpcConfigurationDescription,
  )
where

import Network.AWS.Firehose.CreateDeliveryStream
import Network.AWS.Firehose.DeleteDeliveryStream
import Network.AWS.Firehose.DescribeDeliveryStream
import Network.AWS.Firehose.Lens
import Network.AWS.Firehose.ListDeliveryStreams
import Network.AWS.Firehose.ListTagsForDeliveryStream
import Network.AWS.Firehose.PutRecord
import Network.AWS.Firehose.PutRecordBatch
import Network.AWS.Firehose.StartDeliveryStreamEncryption
import Network.AWS.Firehose.StopDeliveryStreamEncryption
import Network.AWS.Firehose.TagDeliveryStream
import Network.AWS.Firehose.Types
import Network.AWS.Firehose.UntagDeliveryStream
import Network.AWS.Firehose.UpdateDestination
import Network.AWS.Firehose.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Firehose'.

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

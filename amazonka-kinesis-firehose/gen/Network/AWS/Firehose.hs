{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Kinesis Data Firehose API Reference__
--
-- Amazon Kinesis Data Firehose is a fully managed service that delivers real-time streaming data to destinations such as Amazon Simple Storage Service (Amazon S3), Amazon Elasticsearch Service (Amazon ES), Amazon Redshift, and Splunk.
--
module Network.AWS.Firehose
    (
    -- * Service Configuration
      firehose

    -- * Errors
    -- $errors

    -- ** InvalidArgumentException
    , _InvalidArgumentException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** ServiceUnavailableException
    , _ServiceUnavailableException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** PutRecord
    , module Network.AWS.Firehose.PutRecord

    -- ** TagDeliveryStream
    , module Network.AWS.Firehose.TagDeliveryStream

    -- ** UpdateDestination
    , module Network.AWS.Firehose.UpdateDestination

    -- ** PutRecordBatch
    , module Network.AWS.Firehose.PutRecordBatch

    -- ** UntagDeliveryStream
    , module Network.AWS.Firehose.UntagDeliveryStream

    -- ** CreateDeliveryStream
    , module Network.AWS.Firehose.CreateDeliveryStream

    -- ** DescribeDeliveryStream
    , module Network.AWS.Firehose.DescribeDeliveryStream

    -- ** ListTagsForDeliveryStream
    , module Network.AWS.Firehose.ListTagsForDeliveryStream

    -- ** ListDeliveryStreams
    , module Network.AWS.Firehose.ListDeliveryStreams

    -- ** DeleteDeliveryStream
    , module Network.AWS.Firehose.DeleteDeliveryStream

    -- * Types

    -- ** CompressionFormat
    , CompressionFormat (..)

    -- ** DeliveryStreamStatus
    , DeliveryStreamStatus (..)

    -- ** DeliveryStreamType
    , DeliveryStreamType (..)

    -- ** ElasticsearchIndexRotationPeriod
    , ElasticsearchIndexRotationPeriod (..)

    -- ** ElasticsearchS3BackupMode
    , ElasticsearchS3BackupMode (..)

    -- ** HECEndpointType
    , HECEndpointType (..)

    -- ** NoEncryptionConfig
    , NoEncryptionConfig (..)

    -- ** OrcCompression
    , OrcCompression (..)

    -- ** OrcFormatVersion
    , OrcFormatVersion (..)

    -- ** ParquetCompression
    , ParquetCompression (..)

    -- ** ParquetWriterVersion
    , ParquetWriterVersion (..)

    -- ** ProcessorParameterName
    , ProcessorParameterName (..)

    -- ** ProcessorType
    , ProcessorType (..)

    -- ** RedshiftS3BackupMode
    , RedshiftS3BackupMode (..)

    -- ** S3BackupMode
    , S3BackupMode (..)

    -- ** SplunkS3BackupMode
    , SplunkS3BackupMode (..)

    -- ** BufferingHints
    , BufferingHints
    , bufferingHints
    , bhSizeInMBs
    , bhIntervalInSeconds

    -- ** CloudWatchLoggingOptions
    , CloudWatchLoggingOptions
    , cloudWatchLoggingOptions
    , cwloEnabled
    , cwloLogGroupName
    , cwloLogStreamName

    -- ** CopyCommand
    , CopyCommand
    , copyCommand
    , ccCopyOptions
    , ccDataTableColumns
    , ccDataTableName

    -- ** DataFormatConversionConfiguration
    , DataFormatConversionConfiguration
    , dataFormatConversionConfiguration
    , dfccOutputFormatConfiguration
    , dfccEnabled
    , dfccSchemaConfiguration
    , dfccInputFormatConfiguration

    -- ** DeliveryStreamDescription
    , DeliveryStreamDescription
    , deliveryStreamDescription
    , dsdCreateTimestamp
    , dsdSource
    , dsdLastUpdateTimestamp
    , dsdDeliveryStreamName
    , dsdDeliveryStreamARN
    , dsdDeliveryStreamStatus
    , dsdDeliveryStreamType
    , dsdVersionId
    , dsdDestinations
    , dsdHasMoreDestinations

    -- ** Deserializer
    , Deserializer
    , deserializer
    , dOpenXJSONSerDe
    , dHiveJSONSerDe

    -- ** DestinationDescription
    , DestinationDescription
    , destinationDescription
    , ddSplunkDestinationDescription
    , ddS3DestinationDescription
    , ddExtendedS3DestinationDescription
    , ddElasticsearchDestinationDescription
    , ddRedshiftDestinationDescription
    , ddDestinationId

    -- ** ElasticsearchBufferingHints
    , ElasticsearchBufferingHints
    , elasticsearchBufferingHints
    , ebhSizeInMBs
    , ebhIntervalInSeconds

    -- ** ElasticsearchDestinationConfiguration
    , ElasticsearchDestinationConfiguration
    , elasticsearchDestinationConfiguration
    , edcIndexRotationPeriod
    , edcS3BackupMode
    , edcCloudWatchLoggingOptions
    , edcBufferingHints
    , edcRetryOptions
    , edcProcessingConfiguration
    , edcRoleARN
    , edcDomainARN
    , edcIndexName
    , edcTypeName
    , edcS3Configuration

    -- ** ElasticsearchDestinationDescription
    , ElasticsearchDestinationDescription
    , elasticsearchDestinationDescription
    , eddIndexRotationPeriod
    , eddTypeName
    , eddS3BackupMode
    , eddDomainARN
    , eddCloudWatchLoggingOptions
    , eddS3DestinationDescription
    , eddBufferingHints
    , eddRetryOptions
    , eddProcessingConfiguration
    , eddRoleARN
    , eddIndexName

    -- ** ElasticsearchDestinationUpdate
    , ElasticsearchDestinationUpdate
    , elasticsearchDestinationUpdate
    , eduIndexRotationPeriod
    , eduTypeName
    , eduDomainARN
    , eduCloudWatchLoggingOptions
    , eduS3Update
    , eduBufferingHints
    , eduRetryOptions
    , eduProcessingConfiguration
    , eduRoleARN
    , eduIndexName

    -- ** ElasticsearchRetryOptions
    , ElasticsearchRetryOptions
    , elasticsearchRetryOptions
    , eroDurationInSeconds

    -- ** EncryptionConfiguration
    , EncryptionConfiguration
    , encryptionConfiguration
    , ecNoEncryptionConfig
    , ecKMSEncryptionConfig

    -- ** ExtendedS3DestinationConfiguration
    , ExtendedS3DestinationConfiguration
    , extendedS3DestinationConfiguration
    , esdcS3BackupMode
    , esdcPrefix
    , esdcCloudWatchLoggingOptions
    , esdcS3BackupConfiguration
    , esdcEncryptionConfiguration
    , esdcCompressionFormat
    , esdcBufferingHints
    , esdcDataFormatConversionConfiguration
    , esdcProcessingConfiguration
    , esdcRoleARN
    , esdcBucketARN

    -- ** ExtendedS3DestinationDescription
    , ExtendedS3DestinationDescription
    , extendedS3DestinationDescription
    , esddS3BackupMode
    , esddS3BackupDescription
    , esddPrefix
    , esddCloudWatchLoggingOptions
    , esddDataFormatConversionConfiguration
    , esddProcessingConfiguration
    , esddRoleARN
    , esddBucketARN
    , esddBufferingHints
    , esddCompressionFormat
    , esddEncryptionConfiguration

    -- ** ExtendedS3DestinationUpdate
    , ExtendedS3DestinationUpdate
    , extendedS3DestinationUpdate
    , esduS3BackupMode
    , esduPrefix
    , esduCloudWatchLoggingOptions
    , esduS3BackupUpdate
    , esduEncryptionConfiguration
    , esduCompressionFormat
    , esduBufferingHints
    , esduDataFormatConversionConfiguration
    , esduBucketARN
    , esduProcessingConfiguration
    , esduRoleARN

    -- ** HiveJSONSerDe
    , HiveJSONSerDe
    , hiveJSONSerDe
    , hjsdTimestampFormats

    -- ** InputFormatConfiguration
    , InputFormatConfiguration
    , inputFormatConfiguration
    , ifcDeserializer

    -- ** KMSEncryptionConfig
    , KMSEncryptionConfig
    , kmsEncryptionConfig
    , kecAWSKMSKeyARN

    -- ** KinesisStreamSourceConfiguration
    , KinesisStreamSourceConfiguration
    , kinesisStreamSourceConfiguration
    , ksscKinesisStreamARN
    , ksscRoleARN

    -- ** KinesisStreamSourceDescription
    , KinesisStreamSourceDescription
    , kinesisStreamSourceDescription
    , kssdDeliveryStartTimestamp
    , kssdKinesisStreamARN
    , kssdRoleARN

    -- ** OpenXJSONSerDe
    , OpenXJSONSerDe
    , openXJSONSerDe
    , oxjsdColumnToJSONKeyMappings
    , oxjsdCaseInsensitive
    , oxjsdConvertDotsInJSONKeysToUnderscores

    -- ** OrcSerDe
    , OrcSerDe
    , orcSerDe
    , osdBloomFilterFalsePositiveProbability
    , osdDictionaryKeyThreshold
    , osdEnablePadding
    , osdCompression
    , osdBloomFilterColumns
    , osdRowIndexStride
    , osdFormatVersion
    , osdBlockSizeBytes
    , osdStripeSizeBytes
    , osdPaddingTolerance

    -- ** OutputFormatConfiguration
    , OutputFormatConfiguration
    , outputFormatConfiguration
    , ofcSerializer

    -- ** ParquetSerDe
    , ParquetSerDe
    , parquetSerDe
    , psdWriterVersion
    , psdCompression
    , psdMaxPaddingBytes
    , psdEnableDictionaryCompression
    , psdPageSizeBytes
    , psdBlockSizeBytes

    -- ** ProcessingConfiguration
    , ProcessingConfiguration
    , processingConfiguration
    , pcEnabled
    , pcProcessors

    -- ** Processor
    , Processor
    , processor
    , pParameters
    , pType

    -- ** ProcessorParameter
    , ProcessorParameter
    , processorParameter
    , ppParameterName
    , ppParameterValue

    -- ** PutRecordBatchResponseEntry
    , PutRecordBatchResponseEntry
    , putRecordBatchResponseEntry
    , prbreRecordId
    , prbreErrorCode
    , prbreErrorMessage

    -- ** Record
    , Record
    , record
    , rData

    -- ** RedshiftDestinationConfiguration
    , RedshiftDestinationConfiguration
    , redshiftDestinationConfiguration
    , rdcS3BackupMode
    , rdcCloudWatchLoggingOptions
    , rdcS3BackupConfiguration
    , rdcRetryOptions
    , rdcProcessingConfiguration
    , rdcRoleARN
    , rdcClusterJDBCURL
    , rdcCopyCommand
    , rdcUsername
    , rdcPassword
    , rdcS3Configuration

    -- ** RedshiftDestinationDescription
    , RedshiftDestinationDescription
    , redshiftDestinationDescription
    , rddS3BackupMode
    , rddS3BackupDescription
    , rddCloudWatchLoggingOptions
    , rddRetryOptions
    , rddProcessingConfiguration
    , rddRoleARN
    , rddClusterJDBCURL
    , rddCopyCommand
    , rddUsername
    , rddS3DestinationDescription

    -- ** RedshiftDestinationUpdate
    , RedshiftDestinationUpdate
    , redshiftDestinationUpdate
    , rduS3BackupMode
    , rduCloudWatchLoggingOptions
    , rduUsername
    , rduS3Update
    , rduPassword
    , rduS3BackupUpdate
    , rduCopyCommand
    , rduRetryOptions
    , rduProcessingConfiguration
    , rduClusterJDBCURL
    , rduRoleARN

    -- ** RedshiftRetryOptions
    , RedshiftRetryOptions
    , redshiftRetryOptions
    , rroDurationInSeconds

    -- ** S3DestinationConfiguration
    , S3DestinationConfiguration
    , s3DestinationConfiguration
    , sdcPrefix
    , sdcCloudWatchLoggingOptions
    , sdcEncryptionConfiguration
    , sdcCompressionFormat
    , sdcBufferingHints
    , sdcRoleARN
    , sdcBucketARN

    -- ** S3DestinationDescription
    , S3DestinationDescription
    , s3DestinationDescription
    , s3Prefix
    , s3CloudWatchLoggingOptions
    , s3RoleARN
    , s3BucketARN
    , s3BufferingHints
    , s3CompressionFormat
    , s3EncryptionConfiguration

    -- ** S3DestinationUpdate
    , S3DestinationUpdate
    , s3DestinationUpdate
    , sPrefix
    , sCloudWatchLoggingOptions
    , sEncryptionConfiguration
    , sCompressionFormat
    , sBufferingHints
    , sBucketARN
    , sRoleARN

    -- ** SchemaConfiguration
    , SchemaConfiguration
    , schemaConfiguration
    , scVersionId
    , scCatalogId
    , scDatabaseName
    , scRegion
    , scTableName
    , scRoleARN

    -- ** Serializer
    , Serializer
    , serializer
    , sOrcSerDe
    , sParquetSerDe

    -- ** SourceDescription
    , SourceDescription
    , sourceDescription
    , sdKinesisStreamSourceDescription

    -- ** SplunkDestinationConfiguration
    , SplunkDestinationConfiguration
    , splunkDestinationConfiguration
    , splS3BackupMode
    , splCloudWatchLoggingOptions
    , splHECAcknowledgmentTimeoutInSeconds
    , splRetryOptions
    , splProcessingConfiguration
    , splHECEndpoint
    , splHECEndpointType
    , splHECToken
    , splS3Configuration

    -- ** SplunkDestinationDescription
    , SplunkDestinationDescription
    , splunkDestinationDescription
    , sddS3BackupMode
    , sddHECToken
    , sddHECEndpointType
    , sddCloudWatchLoggingOptions
    , sddHECAcknowledgmentTimeoutInSeconds
    , sddS3DestinationDescription
    , sddHECEndpoint
    , sddRetryOptions
    , sddProcessingConfiguration

    -- ** SplunkDestinationUpdate
    , SplunkDestinationUpdate
    , splunkDestinationUpdate
    , sduS3BackupMode
    , sduHECToken
    , sduHECEndpointType
    , sduCloudWatchLoggingOptions
    , sduHECAcknowledgmentTimeoutInSeconds
    , sduS3Update
    , sduHECEndpoint
    , sduRetryOptions
    , sduProcessingConfiguration

    -- ** SplunkRetryOptions
    , SplunkRetryOptions
    , splunkRetryOptions
    , sroDurationInSeconds

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey
    ) where

import Network.AWS.Firehose.CreateDeliveryStream
import Network.AWS.Firehose.DeleteDeliveryStream
import Network.AWS.Firehose.DescribeDeliveryStream
import Network.AWS.Firehose.ListDeliveryStreams
import Network.AWS.Firehose.ListTagsForDeliveryStream
import Network.AWS.Firehose.PutRecord
import Network.AWS.Firehose.PutRecordBatch
import Network.AWS.Firehose.TagDeliveryStream
import Network.AWS.Firehose.Types
import Network.AWS.Firehose.UntagDeliveryStream
import Network.AWS.Firehose.UpdateDestination
import Network.AWS.Firehose.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Firehose'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}

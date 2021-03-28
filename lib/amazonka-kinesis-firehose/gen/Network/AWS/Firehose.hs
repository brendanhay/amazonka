{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Kinesis Data Firehose API Reference__ 
--
-- Amazon Kinesis Data Firehose is a fully managed service that delivers real-time streaming data to destinations such as Amazon Simple Storage Service (Amazon S3), Amazon Elasticsearch Service (Amazon ES), Amazon Redshift, and Splunk.
module Network.AWS.Firehose
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** InvalidArgumentException
    , _InvalidArgumentException

    -- ** InvalidKMSResourceException
    , _InvalidKMSResourceException

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

    -- ** StopDeliveryStreamEncryption 
    , module Network.AWS.Firehose.StopDeliveryStreamEncryption

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

    -- ** StartDeliveryStreamEncryption 
    , module Network.AWS.Firehose.StartDeliveryStreamEncryption

    -- ** DescribeDeliveryStream 
    , module Network.AWS.Firehose.DescribeDeliveryStream

    -- ** ListTagsForDeliveryStream 
    , module Network.AWS.Firehose.ListTagsForDeliveryStream

    -- ** ListDeliveryStreams 
    , module Network.AWS.Firehose.ListDeliveryStreams

    -- ** DeleteDeliveryStream 
    , module Network.AWS.Firehose.DeleteDeliveryStream

    -- * Types

    -- ** S3DestinationConfiguration
    , S3DestinationConfiguration (..)
    , mkS3DestinationConfiguration
    , sdcRoleARN
    , sdcBucketARN
    , sdcBufferingHints
    , sdcCloudWatchLoggingOptions
    , sdcCompressionFormat
    , sdcEncryptionConfiguration
    , sdcErrorOutputPrefix
    , sdcPrefix

    -- ** ElasticsearchIndexRotationPeriod
    , ElasticsearchIndexRotationPeriod (..)

    -- ** DeliveryStreamStatus
    , DeliveryStreamStatus (..)

    -- ** HttpEndpointAttributeValue
    , HttpEndpointAttributeValue (..)

    -- ** OutputFormatConfiguration
    , OutputFormatConfiguration (..)
    , mkOutputFormatConfiguration
    , ofcSerializer

    -- ** RedshiftDestinationConfiguration
    , RedshiftDestinationConfiguration (..)
    , mkRedshiftDestinationConfiguration
    , rdcRoleARN
    , rdcClusterJDBCURL
    , rdcCopyCommand
    , rdcUsername
    , rdcPassword
    , rdcS3Configuration
    , rdcCloudWatchLoggingOptions
    , rdcProcessingConfiguration
    , rdcRetryOptions
    , rdcS3BackupConfiguration
    , rdcS3BackupMode

    -- ** PutRecordBatchResponseEntry
    , PutRecordBatchResponseEntry (..)
    , mkPutRecordBatchResponseEntry
    , prbreErrorCode
    , prbreErrorMessage
    , prbreRecordId

    -- ** DeliveryStreamVersionId
    , DeliveryStreamVersionId (..)

    -- ** ProcessorParameter
    , ProcessorParameter (..)
    , mkProcessorParameter
    , ppParameterName
    , ppParameterValue

    -- ** ElasticsearchDestinationConfiguration
    , ElasticsearchDestinationConfiguration (..)
    , mkElasticsearchDestinationConfiguration
    , edcRoleARN
    , edcIndexName
    , edcS3Configuration
    , edcBufferingHints
    , edcCloudWatchLoggingOptions
    , edcClusterEndpoint
    , edcDomainARN
    , edcIndexRotationPeriod
    , edcProcessingConfiguration
    , edcRetryOptions
    , edcS3BackupMode
    , edcTypeName
    , edcVpcConfiguration

    -- ** KeyType
    , KeyType (..)

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** SplunkDestinationUpdate
    , SplunkDestinationUpdate (..)
    , mkSplunkDestinationUpdate
    , sduCloudWatchLoggingOptions
    , sduHECAcknowledgmentTimeoutInSeconds
    , sduHECEndpoint
    , sduHECEndpointType
    , sduHECToken
    , sduProcessingConfiguration
    , sduRetryOptions
    , sduS3BackupMode
    , sduS3Update

    -- ** DestinationDescription
    , DestinationDescription (..)
    , mkDestinationDescription
    , ddDestinationId
    , ddElasticsearchDestinationDescription
    , ddExtendedS3DestinationDescription
    , ddHttpEndpointDestinationDescription
    , ddRedshiftDestinationDescription
    , ddS3DestinationDescription
    , ddSplunkDestinationDescription

    -- ** SplunkS3BackupMode
    , SplunkS3BackupMode (..)

    -- ** S3DestinationUpdate
    , S3DestinationUpdate (..)
    , mkS3DestinationUpdate
    , sBucketARN
    , sBufferingHints
    , sCloudWatchLoggingOptions
    , sCompressionFormat
    , sEncryptionConfiguration
    , sErrorOutputPrefix
    , sPrefix
    , sRoleARN

    -- ** S3BackupMode
    , S3BackupMode (..)

    -- ** CopyOptions
    , CopyOptions (..)

    -- ** HECToken
    , HECToken (..)

    -- ** SplunkDestinationDescription
    , SplunkDestinationDescription (..)
    , mkSplunkDestinationDescription
    , sddfCloudWatchLoggingOptions
    , sddfHECAcknowledgmentTimeoutInSeconds
    , sddfHECEndpoint
    , sddfHECEndpointType
    , sddfHECToken
    , sddfProcessingConfiguration
    , sddfRetryOptions
    , sddfS3BackupMode
    , sddfS3DestinationDescription

    -- ** HECEndpointType
    , HECEndpointType (..)

    -- ** DeliveryStreamARN
    , DeliveryStreamARN (..)

    -- ** OrcCompression
    , OrcCompression (..)

    -- ** DeliveryStreamFailureType
    , DeliveryStreamFailureType (..)

    -- ** Prefix
    , Prefix (..)

    -- ** HttpEndpointAccessKey
    , HttpEndpointAccessKey (..)

    -- ** HttpEndpointAttributeName
    , HttpEndpointAttributeName (..)

    -- ** ParquetCompression
    , ParquetCompression (..)

    -- ** ElasticsearchRetryOptions
    , ElasticsearchRetryOptions (..)
    , mkElasticsearchRetryOptions
    , eroDurationInSeconds

    -- ** HttpEndpointConfiguration
    , HttpEndpointConfiguration (..)
    , mkHttpEndpointConfiguration
    , hecUrl
    , hecAccessKey
    , hecName

    -- ** VpcConfigurationDescription
    , VpcConfigurationDescription (..)
    , mkVpcConfigurationDescription
    , vcdSubnetIds
    , vcdRoleARN
    , vcdSecurityGroupIds
    , vcdVpcId

    -- ** ExtendedS3DestinationConfiguration
    , ExtendedS3DestinationConfiguration (..)
    , mkExtendedS3DestinationConfiguration
    , esdcRoleARN
    , esdcBucketARN
    , esdcBufferingHints
    , esdcCloudWatchLoggingOptions
    , esdcCompressionFormat
    , esdcDataFormatConversionConfiguration
    , esdcEncryptionConfiguration
    , esdcErrorOutputPrefix
    , esdcPrefix
    , esdcProcessingConfiguration
    , esdcS3BackupConfiguration
    , esdcS3BackupMode

    -- ** CloudWatchLoggingOptions
    , CloudWatchLoggingOptions (..)
    , mkCloudWatchLoggingOptions
    , cwloEnabled
    , cwloLogGroupName
    , cwloLogStreamName

    -- ** SourceDescription
    , SourceDescription (..)
    , mkSourceDescription
    , sdKinesisStreamSourceDescription

    -- ** RedshiftRetryOptions
    , RedshiftRetryOptions (..)
    , mkRedshiftRetryOptions
    , rroDurationInSeconds

    -- ** ElasticsearchBufferingHints
    , ElasticsearchBufferingHints (..)
    , mkElasticsearchBufferingHints
    , ebhIntervalInSeconds
    , ebhSizeInMBs

    -- ** Processor
    , Processor (..)
    , mkProcessor
    , pType
    , pParameters

    -- ** FailureDescription
    , FailureDescription (..)
    , mkFailureDescription
    , fdType
    , fdDetails

    -- ** ElasticsearchS3BackupMode
    , ElasticsearchS3BackupMode (..)

    -- ** RedshiftDestinationUpdate
    , RedshiftDestinationUpdate (..)
    , mkRedshiftDestinationUpdate
    , rduCloudWatchLoggingOptions
    , rduClusterJDBCURL
    , rduCopyCommand
    , rduPassword
    , rduProcessingConfiguration
    , rduRetryOptions
    , rduRoleARN
    , rduS3BackupMode
    , rduS3BackupUpdate
    , rduS3Update
    , rduUsername

    -- ** RedshiftS3BackupMode
    , RedshiftS3BackupMode (..)

    -- ** ElasticsearchDestinationUpdate
    , ElasticsearchDestinationUpdate (..)
    , mkElasticsearchDestinationUpdate
    , eduBufferingHints
    , eduCloudWatchLoggingOptions
    , eduClusterEndpoint
    , eduDomainARN
    , eduIndexName
    , eduIndexRotationPeriod
    , eduProcessingConfiguration
    , eduRetryOptions
    , eduRoleARN
    , eduS3Update
    , eduTypeName

    -- ** Username
    , Username (..)

    -- ** SchemaConfiguration
    , SchemaConfiguration (..)
    , mkSchemaConfiguration
    , scCatalogId
    , scDatabaseName
    , scRegion
    , scRoleARN
    , scTableName
    , scVersionId

    -- ** HttpEndpointDestinationDescription
    , HttpEndpointDestinationDescription (..)
    , mkHttpEndpointDestinationDescription
    , heddBufferingHints
    , heddCloudWatchLoggingOptions
    , heddEndpointConfiguration
    , heddProcessingConfiguration
    , heddRequestConfiguration
    , heddRetryOptions
    , heddRoleARN
    , heddS3BackupMode
    , heddS3DestinationDescription

    -- ** KinesisStreamSourceConfiguration
    , KinesisStreamSourceConfiguration (..)
    , mkKinesisStreamSourceConfiguration
    , ksscKinesisStreamARN
    , ksscRoleARN

    -- ** OpenXJsonSerDe
    , OpenXJsonSerDe (..)
    , mkOpenXJsonSerDe
    , oxjsdCaseInsensitive
    , oxjsdColumnToJsonKeyMappings
    , oxjsdConvertDotsInJsonKeysToUnderscores

    -- ** LogGroupName
    , LogGroupName (..)

    -- ** HttpEndpointName
    , HttpEndpointName (..)

    -- ** ElasticsearchTypeName
    , ElasticsearchTypeName (..)

    -- ** Serializer
    , Serializer (..)
    , mkSerializer
    , sOrcSerDe
    , sParquetSerDe

    -- ** NoEncryptionConfig
    , NoEncryptionConfig (..)

    -- ** NonEmptyString
    , NonEmptyString (..)

    -- ** ErrorOutputPrefix
    , ErrorOutputPrefix (..)

    -- ** LogStreamName
    , LogStreamName (..)

    -- ** OrcFormatVersion
    , OrcFormatVersion (..)

    -- ** HttpEndpointDestinationConfiguration
    , HttpEndpointDestinationConfiguration (..)
    , mkHttpEndpointDestinationConfiguration
    , hedcEndpointConfiguration
    , hedcS3Configuration
    , hedcBufferingHints
    , hedcCloudWatchLoggingOptions
    , hedcProcessingConfiguration
    , hedcRequestConfiguration
    , hedcRetryOptions
    , hedcRoleARN
    , hedcS3BackupMode

    -- ** SplunkRetryOptions
    , SplunkRetryOptions (..)
    , mkSplunkRetryOptions
    , sroDurationInSeconds

    -- ** DeliveryStreamEncryptionConfiguration
    , DeliveryStreamEncryptionConfiguration (..)
    , mkDeliveryStreamEncryptionConfiguration
    , dsecFailureDescription
    , dsecKeyARN
    , dsecKeyType
    , dsecStatus

    -- ** KinesisStreamSourceDescription
    , KinesisStreamSourceDescription (..)
    , mkKinesisStreamSourceDescription
    , kssdDeliveryStartTimestamp
    , kssdKinesisStreamARN
    , kssdRoleARN

    -- ** HiveJsonSerDe
    , HiveJsonSerDe (..)
    , mkHiveJsonSerDe
    , hjsdTimestampFormats

    -- ** Password
    , Password (..)

    -- ** DeliveryStreamName
    , DeliveryStreamName (..)

    -- ** EncryptionConfiguration
    , EncryptionConfiguration (..)
    , mkEncryptionConfiguration
    , ecKMSEncryptionConfig
    , ecNoEncryptionConfig

    -- ** S3DestinationDescription
    , S3DestinationDescription (..)
    , mkS3DestinationDescription
    , sddRoleARN
    , sddBucketARN
    , sddBufferingHints
    , sddCompressionFormat
    , sddEncryptionConfiguration
    , sddCloudWatchLoggingOptions
    , sddErrorOutputPrefix
    , sddPrefix

    -- ** HttpEndpointRequestConfiguration
    , HttpEndpointRequestConfiguration (..)
    , mkHttpEndpointRequestConfiguration
    , hercCommonAttributes
    , hercContentEncoding

    -- ** HttpEndpointCommonAttribute
    , HttpEndpointCommonAttribute (..)
    , mkHttpEndpointCommonAttribute
    , hecaAttributeName
    , hecaAttributeValue

    -- ** AWSKMSKeyARN
    , AWSKMSKeyARN (..)

    -- ** PutResponseRecordId
    , PutResponseRecordId (..)

    -- ** ElasticsearchClusterEndpoint
    , ElasticsearchClusterEndpoint (..)

    -- ** ExtendedS3DestinationUpdate
    , ExtendedS3DestinationUpdate (..)
    , mkExtendedS3DestinationUpdate
    , esduBucketARN
    , esduBufferingHints
    , esduCloudWatchLoggingOptions
    , esduCompressionFormat
    , esduDataFormatConversionConfiguration
    , esduEncryptionConfiguration
    , esduErrorOutputPrefix
    , esduPrefix
    , esduProcessingConfiguration
    , esduRoleARN
    , esduS3BackupMode
    , esduS3BackupUpdate

    -- ** CopyCommand
    , CopyCommand (..)
    , mkCopyCommand
    , ccDataTableName
    , ccCopyOptions
    , ccDataTableColumns

    -- ** ContentEncoding
    , ContentEncoding (..)

    -- ** ElasticsearchIndexName
    , ElasticsearchIndexName (..)

    -- ** Deserializer
    , Deserializer (..)
    , mkDeserializer
    , dHiveJsonSerDe
    , dOpenXJsonSerDe

    -- ** ErrorCode
    , ErrorCode (..)

    -- ** HttpEndpointDescription
    , HttpEndpointDescription (..)
    , mkHttpEndpointDescription
    , hedName
    , hedUrl

    -- ** VpcConfiguration
    , VpcConfiguration (..)
    , mkVpcConfiguration
    , vcSubnetIds
    , vcRoleARN
    , vcSecurityGroupIds

    -- ** HECEndpoint
    , HECEndpoint (..)

    -- ** ExtendedS3DestinationDescription
    , ExtendedS3DestinationDescription (..)
    , mkExtendedS3DestinationDescription
    , esddRoleARN
    , esddBucketARN
    , esddBufferingHints
    , esddCompressionFormat
    , esddEncryptionConfiguration
    , esddCloudWatchLoggingOptions
    , esddDataFormatConversionConfiguration
    , esddErrorOutputPrefix
    , esddPrefix
    , esddProcessingConfiguration
    , esddS3BackupDescription
    , esddS3BackupMode

    -- ** CompressionFormat
    , CompressionFormat (..)

    -- ** InputFormatConfiguration
    , InputFormatConfiguration (..)
    , mkInputFormatConfiguration
    , ifcDeserializer

    -- ** TagKey
    , TagKey (..)

    -- ** Record
    , Record (..)
    , mkRecord
    , rData

    -- ** KMSEncryptionConfig
    , KMSEncryptionConfig (..)
    , mkKMSEncryptionConfig
    , kmsecAWSKMSKeyARN

    -- ** BufferingHints
    , BufferingHints (..)
    , mkBufferingHints
    , bhIntervalInSeconds
    , bhSizeInMBs

    -- ** DeliveryStreamType
    , DeliveryStreamType (..)

    -- ** NonEmptyStringWithoutWhitespace
    , NonEmptyStringWithoutWhitespace (..)

    -- ** ProcessorParameterName
    , ProcessorParameterName (..)

    -- ** DataFormatConversionConfiguration
    , DataFormatConversionConfiguration (..)
    , mkDataFormatConversionConfiguration
    , dfccEnabled
    , dfccInputFormatConfiguration
    , dfccOutputFormatConfiguration
    , dfccSchemaConfiguration

    -- ** DeliveryStreamEncryptionStatus
    , DeliveryStreamEncryptionStatus (..)

    -- ** HttpEndpointBufferingHints
    , HttpEndpointBufferingHints (..)
    , mkHttpEndpointBufferingHints
    , hebhIntervalInSeconds
    , hebhSizeInMBs

    -- ** ErrorMessage
    , ErrorMessage (..)

    -- ** HttpEndpointRetryOptions
    , HttpEndpointRetryOptions (..)
    , mkHttpEndpointRetryOptions
    , heroDurationInSeconds

    -- ** BucketARN
    , BucketARN (..)

    -- ** DataTableColumns
    , DataTableColumns (..)

    -- ** ParquetWriterVersion
    , ParquetWriterVersion (..)

    -- ** OrcSerDe
    , OrcSerDe (..)
    , mkOrcSerDe
    , osdBlockSizeBytes
    , osdBloomFilterColumns
    , osdBloomFilterFalsePositiveProbability
    , osdCompression
    , osdDictionaryKeyThreshold
    , osdEnablePadding
    , osdFormatVersion
    , osdPaddingTolerance
    , osdRowIndexStride
    , osdStripeSizeBytes

    -- ** ParquetSerDe
    , ParquetSerDe (..)
    , mkParquetSerDe
    , psdBlockSizeBytes
    , psdCompression
    , psdEnableDictionaryCompression
    , psdMaxPaddingBytes
    , psdPageSizeBytes
    , psdWriterVersion

    -- ** SplunkDestinationConfiguration
    , SplunkDestinationConfiguration (..)
    , mkSplunkDestinationConfiguration
    , sdcfHECEndpoint
    , sdcfHECEndpointType
    , sdcfHECToken
    , sdcfS3Configuration
    , sdcfCloudWatchLoggingOptions
    , sdcfHECAcknowledgmentTimeoutInSeconds
    , sdcfProcessingConfiguration
    , sdcfRetryOptions
    , sdcfS3BackupMode

    -- ** ProcessingConfiguration
    , ProcessingConfiguration (..)
    , mkProcessingConfiguration
    , pcEnabled
    , pcProcessors

    -- ** ClusterJDBCURL
    , ClusterJDBCURL (..)

    -- ** HttpEndpointS3BackupMode
    , HttpEndpointS3BackupMode (..)

    -- ** ProcessorType
    , ProcessorType (..)

    -- ** DeliveryStreamDescription
    , DeliveryStreamDescription (..)
    , mkDeliveryStreamDescription
    , dsdDeliveryStreamName
    , dsdDeliveryStreamARN
    , dsdDeliveryStreamStatus
    , dsdDeliveryStreamType
    , dsdVersionId
    , dsdDestinations
    , dsdHasMoreDestinations
    , dsdCreateTimestamp
    , dsdDeliveryStreamEncryptionConfiguration
    , dsdFailureDescription
    , dsdLastUpdateTimestamp
    , dsdSource

    -- ** HttpEndpointDestinationUpdate
    , HttpEndpointDestinationUpdate (..)
    , mkHttpEndpointDestinationUpdate
    , heduBufferingHints
    , heduCloudWatchLoggingOptions
    , heduEndpointConfiguration
    , heduProcessingConfiguration
    , heduRequestConfiguration
    , heduRetryOptions
    , heduRoleARN
    , heduS3BackupMode
    , heduS3Update

    -- ** ElasticsearchDomainARN
    , ElasticsearchDomainARN (..)

    -- ** KinesisStreamARN
    , KinesisStreamARN (..)

    -- ** RoleARN
    , RoleARN (..)

    -- ** ElasticsearchDestinationDescription
    , ElasticsearchDestinationDescription (..)
    , mkElasticsearchDestinationDescription
    , eddBufferingHints
    , eddCloudWatchLoggingOptions
    , eddClusterEndpoint
    , eddDomainARN
    , eddIndexName
    , eddIndexRotationPeriod
    , eddProcessingConfiguration
    , eddRetryOptions
    , eddRoleARN
    , eddS3BackupMode
    , eddS3DestinationDescription
    , eddTypeName
    , eddVpcConfigurationDescription

    -- ** DataTableName
    , DataTableName (..)

    -- ** RedshiftDestinationDescription
    , RedshiftDestinationDescription (..)
    , mkRedshiftDestinationDescription
    , rddRoleARN
    , rddClusterJDBCURL
    , rddCopyCommand
    , rddUsername
    , rddS3DestinationDescription
    , rddCloudWatchLoggingOptions
    , rddProcessingConfiguration
    , rddRetryOptions
    , rddS3BackupDescription
    , rddS3BackupMode

    -- ** DestinationId
    , DestinationId (..)

    -- ** DeliveryStreamEncryptionConfigurationInput
    , DeliveryStreamEncryptionConfigurationInput (..)
    , mkDeliveryStreamEncryptionConfigurationInput
    , dseciKeyType
    , dseciKeyARN

    -- ** RecordId
    , RecordId (..)

    -- ** ParameterValue
    , ParameterValue (..)

    -- ** IndexName
    , IndexName (..)

    -- ** ClusterEndpoint
    , ClusterEndpoint (..)

    -- ** DomainARN
    , DomainARN (..)

    -- ** TypeName
    , TypeName (..)

    -- ** Key
    , Key (..)

    -- ** Value
    , Value (..)

    -- ** Url
    , Url (..)

    -- ** Name
    , Name (..)

    -- ** VpcId
    , VpcId (..)

    -- ** Details
    , Details (..)

    -- ** CatalogId
    , CatalogId (..)

    -- ** DatabaseName
    , DatabaseName (..)

    -- ** Region
    , Region (..)

    -- ** TableName
    , TableName (..)

    -- ** VersionId
    , VersionId (..)

    -- ** ExclusiveStartDeliveryStreamName
    , ExclusiveStartDeliveryStreamName (..)

    -- ** KeyARN
    , KeyARN (..)

    -- ** ExclusiveStartDestinationId
    , ExclusiveStartDestinationId (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.Firehose.Types
import Network.AWS.Firehose.Waiters
import Network.AWS.Firehose.PutRecord
import Network.AWS.Firehose.StopDeliveryStreamEncryption
import Network.AWS.Firehose.TagDeliveryStream
import Network.AWS.Firehose.UpdateDestination
import Network.AWS.Firehose.PutRecordBatch
import Network.AWS.Firehose.UntagDeliveryStream
import Network.AWS.Firehose.CreateDeliveryStream
import Network.AWS.Firehose.StartDeliveryStreamEncryption
import Network.AWS.Firehose.DescribeDeliveryStream
import Network.AWS.Firehose.ListTagsForDeliveryStream
import Network.AWS.Firehose.ListDeliveryStreams
import Network.AWS.Firehose.DeleteDeliveryStream
import qualified Network.AWS.Prelude as Lude

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

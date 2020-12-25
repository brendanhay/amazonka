-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _InvalidArgumentException,
    _InvalidKMSResourceException,
    _ConcurrentModificationException,
    _ServiceUnavailableException,
    _ResourceNotFoundException,
    _LimitExceededException,
    _ResourceInUseException,

    -- * S3DestinationConfiguration
    S3DestinationConfiguration (..),
    mkS3DestinationConfiguration,
    sdcRoleARN,
    sdcBucketARN,
    sdcBufferingHints,
    sdcCloudWatchLoggingOptions,
    sdcCompressionFormat,
    sdcEncryptionConfiguration,
    sdcErrorOutputPrefix,
    sdcPrefix,

    -- * ElasticsearchIndexRotationPeriod
    ElasticsearchIndexRotationPeriod (..),

    -- * DeliveryStreamStatus
    DeliveryStreamStatus (..),

    -- * HttpEndpointAttributeValue
    HttpEndpointAttributeValue (..),

    -- * OutputFormatConfiguration
    OutputFormatConfiguration (..),
    mkOutputFormatConfiguration,
    ofcSerializer,

    -- * RedshiftDestinationConfiguration
    RedshiftDestinationConfiguration (..),
    mkRedshiftDestinationConfiguration,
    rdcRoleARN,
    rdcClusterJDBCURL,
    rdcCopyCommand,
    rdcUsername,
    rdcPassword,
    rdcS3Configuration,
    rdcCloudWatchLoggingOptions,
    rdcProcessingConfiguration,
    rdcRetryOptions,
    rdcS3BackupConfiguration,
    rdcS3BackupMode,

    -- * PutRecordBatchResponseEntry
    PutRecordBatchResponseEntry (..),
    mkPutRecordBatchResponseEntry,
    prbreErrorCode,
    prbreErrorMessage,
    prbreRecordId,

    -- * DeliveryStreamVersionId
    DeliveryStreamVersionId (..),

    -- * ProcessorParameter
    ProcessorParameter (..),
    mkProcessorParameter,
    ppParameterName,
    ppParameterValue,

    -- * ElasticsearchDestinationConfiguration
    ElasticsearchDestinationConfiguration (..),
    mkElasticsearchDestinationConfiguration,
    edcRoleARN,
    edcIndexName,
    edcS3Configuration,
    edcBufferingHints,
    edcCloudWatchLoggingOptions,
    edcClusterEndpoint,
    edcDomainARN,
    edcIndexRotationPeriod,
    edcProcessingConfiguration,
    edcRetryOptions,
    edcS3BackupMode,
    edcTypeName,
    edcVpcConfiguration,

    -- * KeyType
    KeyType (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * SplunkDestinationUpdate
    SplunkDestinationUpdate (..),
    mkSplunkDestinationUpdate,
    sduCloudWatchLoggingOptions,
    sduHECAcknowledgmentTimeoutInSeconds,
    sduHECEndpoint,
    sduHECEndpointType,
    sduHECToken,
    sduProcessingConfiguration,
    sduRetryOptions,
    sduS3BackupMode,
    sduS3Update,

    -- * DestinationDescription
    DestinationDescription (..),
    mkDestinationDescription,
    ddDestinationId,
    ddElasticsearchDestinationDescription,
    ddExtendedS3DestinationDescription,
    ddHttpEndpointDestinationDescription,
    ddRedshiftDestinationDescription,
    ddS3DestinationDescription,
    ddSplunkDestinationDescription,

    -- * SplunkS3BackupMode
    SplunkS3BackupMode (..),

    -- * S3DestinationUpdate
    S3DestinationUpdate (..),
    mkS3DestinationUpdate,
    sBucketARN,
    sBufferingHints,
    sCloudWatchLoggingOptions,
    sCompressionFormat,
    sEncryptionConfiguration,
    sErrorOutputPrefix,
    sPrefix,
    sRoleARN,

    -- * S3BackupMode
    S3BackupMode (..),

    -- * CopyOptions
    CopyOptions (..),

    -- * HECToken
    HECToken (..),

    -- * SplunkDestinationDescription
    SplunkDestinationDescription (..),
    mkSplunkDestinationDescription,
    sddfCloudWatchLoggingOptions,
    sddfHECAcknowledgmentTimeoutInSeconds,
    sddfHECEndpoint,
    sddfHECEndpointType,
    sddfHECToken,
    sddfProcessingConfiguration,
    sddfRetryOptions,
    sddfS3BackupMode,
    sddfS3DestinationDescription,

    -- * HECEndpointType
    HECEndpointType (..),

    -- * DeliveryStreamARN
    DeliveryStreamARN (..),

    -- * OrcCompression
    OrcCompression (..),

    -- * DeliveryStreamFailureType
    DeliveryStreamFailureType (..),

    -- * Prefix
    Prefix (..),

    -- * HttpEndpointAccessKey
    HttpEndpointAccessKey (..),

    -- * HttpEndpointAttributeName
    HttpEndpointAttributeName (..),

    -- * ParquetCompression
    ParquetCompression (..),

    -- * ElasticsearchRetryOptions
    ElasticsearchRetryOptions (..),
    mkElasticsearchRetryOptions,
    eroDurationInSeconds,

    -- * HttpEndpointConfiguration
    HttpEndpointConfiguration (..),
    mkHttpEndpointConfiguration,
    hecUrl,
    hecAccessKey,
    hecName,

    -- * VpcConfigurationDescription
    VpcConfigurationDescription (..),
    mkVpcConfigurationDescription,
    vcdSubnetIds,
    vcdRoleARN,
    vcdSecurityGroupIds,
    vcdVpcId,

    -- * ExtendedS3DestinationConfiguration
    ExtendedS3DestinationConfiguration (..),
    mkExtendedS3DestinationConfiguration,
    esdcRoleARN,
    esdcBucketARN,
    esdcBufferingHints,
    esdcCloudWatchLoggingOptions,
    esdcCompressionFormat,
    esdcDataFormatConversionConfiguration,
    esdcEncryptionConfiguration,
    esdcErrorOutputPrefix,
    esdcPrefix,
    esdcProcessingConfiguration,
    esdcS3BackupConfiguration,
    esdcS3BackupMode,

    -- * CloudWatchLoggingOptions
    CloudWatchLoggingOptions (..),
    mkCloudWatchLoggingOptions,
    cwloEnabled,
    cwloLogGroupName,
    cwloLogStreamName,

    -- * SourceDescription
    SourceDescription (..),
    mkSourceDescription,
    sdKinesisStreamSourceDescription,

    -- * RedshiftRetryOptions
    RedshiftRetryOptions (..),
    mkRedshiftRetryOptions,
    rroDurationInSeconds,

    -- * ElasticsearchBufferingHints
    ElasticsearchBufferingHints (..),
    mkElasticsearchBufferingHints,
    ebhIntervalInSeconds,
    ebhSizeInMBs,

    -- * Processor
    Processor (..),
    mkProcessor,
    pType,
    pParameters,

    -- * FailureDescription
    FailureDescription (..),
    mkFailureDescription,
    fdType,
    fdDetails,

    -- * ElasticsearchS3BackupMode
    ElasticsearchS3BackupMode (..),

    -- * RedshiftDestinationUpdate
    RedshiftDestinationUpdate (..),
    mkRedshiftDestinationUpdate,
    rduCloudWatchLoggingOptions,
    rduClusterJDBCURL,
    rduCopyCommand,
    rduPassword,
    rduProcessingConfiguration,
    rduRetryOptions,
    rduRoleARN,
    rduS3BackupMode,
    rduS3BackupUpdate,
    rduS3Update,
    rduUsername,

    -- * RedshiftS3BackupMode
    RedshiftS3BackupMode (..),

    -- * ElasticsearchDestinationUpdate
    ElasticsearchDestinationUpdate (..),
    mkElasticsearchDestinationUpdate,
    eduBufferingHints,
    eduCloudWatchLoggingOptions,
    eduClusterEndpoint,
    eduDomainARN,
    eduIndexName,
    eduIndexRotationPeriod,
    eduProcessingConfiguration,
    eduRetryOptions,
    eduRoleARN,
    eduS3Update,
    eduTypeName,

    -- * Username
    Username (..),

    -- * SchemaConfiguration
    SchemaConfiguration (..),
    mkSchemaConfiguration,
    scCatalogId,
    scDatabaseName,
    scRegion,
    scRoleARN,
    scTableName,
    scVersionId,

    -- * HttpEndpointDestinationDescription
    HttpEndpointDestinationDescription (..),
    mkHttpEndpointDestinationDescription,
    heddBufferingHints,
    heddCloudWatchLoggingOptions,
    heddEndpointConfiguration,
    heddProcessingConfiguration,
    heddRequestConfiguration,
    heddRetryOptions,
    heddRoleARN,
    heddS3BackupMode,
    heddS3DestinationDescription,

    -- * KinesisStreamSourceConfiguration
    KinesisStreamSourceConfiguration (..),
    mkKinesisStreamSourceConfiguration,
    ksscKinesisStreamARN,
    ksscRoleARN,

    -- * OpenXJsonSerDe
    OpenXJsonSerDe (..),
    mkOpenXJsonSerDe,
    oxjsdCaseInsensitive,
    oxjsdColumnToJsonKeyMappings,
    oxjsdConvertDotsInJsonKeysToUnderscores,

    -- * LogGroupName
    LogGroupName (..),

    -- * HttpEndpointName
    HttpEndpointName (..),

    -- * ElasticsearchTypeName
    ElasticsearchTypeName (..),

    -- * Serializer
    Serializer (..),
    mkSerializer,
    sOrcSerDe,
    sParquetSerDe,

    -- * NoEncryptionConfig
    NoEncryptionConfig (..),

    -- * NonEmptyString
    NonEmptyString (..),

    -- * ErrorOutputPrefix
    ErrorOutputPrefix (..),

    -- * LogStreamName
    LogStreamName (..),

    -- * OrcFormatVersion
    OrcFormatVersion (..),

    -- * HttpEndpointDestinationConfiguration
    HttpEndpointDestinationConfiguration (..),
    mkHttpEndpointDestinationConfiguration,
    hedcEndpointConfiguration,
    hedcS3Configuration,
    hedcBufferingHints,
    hedcCloudWatchLoggingOptions,
    hedcProcessingConfiguration,
    hedcRequestConfiguration,
    hedcRetryOptions,
    hedcRoleARN,
    hedcS3BackupMode,

    -- * SplunkRetryOptions
    SplunkRetryOptions (..),
    mkSplunkRetryOptions,
    sroDurationInSeconds,

    -- * DeliveryStreamEncryptionConfiguration
    DeliveryStreamEncryptionConfiguration (..),
    mkDeliveryStreamEncryptionConfiguration,
    dsecFailureDescription,
    dsecKeyARN,
    dsecKeyType,
    dsecStatus,

    -- * KinesisStreamSourceDescription
    KinesisStreamSourceDescription (..),
    mkKinesisStreamSourceDescription,
    kssdDeliveryStartTimestamp,
    kssdKinesisStreamARN,
    kssdRoleARN,

    -- * HiveJsonSerDe
    HiveJsonSerDe (..),
    mkHiveJsonSerDe,
    hjsdTimestampFormats,

    -- * Password
    Password (..),

    -- * DeliveryStreamName
    DeliveryStreamName (..),

    -- * EncryptionConfiguration
    EncryptionConfiguration (..),
    mkEncryptionConfiguration,
    ecKMSEncryptionConfig,
    ecNoEncryptionConfig,

    -- * S3DestinationDescription
    S3DestinationDescription (..),
    mkS3DestinationDescription,
    sddRoleARN,
    sddBucketARN,
    sddBufferingHints,
    sddCompressionFormat,
    sddEncryptionConfiguration,
    sddCloudWatchLoggingOptions,
    sddErrorOutputPrefix,
    sddPrefix,

    -- * HttpEndpointRequestConfiguration
    HttpEndpointRequestConfiguration (..),
    mkHttpEndpointRequestConfiguration,
    hercCommonAttributes,
    hercContentEncoding,

    -- * HttpEndpointCommonAttribute
    HttpEndpointCommonAttribute (..),
    mkHttpEndpointCommonAttribute,
    hecaAttributeName,
    hecaAttributeValue,

    -- * AWSKMSKeyARN
    AWSKMSKeyARN (..),

    -- * PutResponseRecordId
    PutResponseRecordId (..),

    -- * ElasticsearchClusterEndpoint
    ElasticsearchClusterEndpoint (..),

    -- * ExtendedS3DestinationUpdate
    ExtendedS3DestinationUpdate (..),
    mkExtendedS3DestinationUpdate,
    esduBucketARN,
    esduBufferingHints,
    esduCloudWatchLoggingOptions,
    esduCompressionFormat,
    esduDataFormatConversionConfiguration,
    esduEncryptionConfiguration,
    esduErrorOutputPrefix,
    esduPrefix,
    esduProcessingConfiguration,
    esduRoleARN,
    esduS3BackupMode,
    esduS3BackupUpdate,

    -- * CopyCommand
    CopyCommand (..),
    mkCopyCommand,
    ccDataTableName,
    ccCopyOptions,
    ccDataTableColumns,

    -- * ContentEncoding
    ContentEncoding (..),

    -- * ElasticsearchIndexName
    ElasticsearchIndexName (..),

    -- * Deserializer
    Deserializer (..),
    mkDeserializer,
    dHiveJsonSerDe,
    dOpenXJsonSerDe,

    -- * ErrorCode
    ErrorCode (..),

    -- * HttpEndpointDescription
    HttpEndpointDescription (..),
    mkHttpEndpointDescription,
    hedName,
    hedUrl,

    -- * VpcConfiguration
    VpcConfiguration (..),
    mkVpcConfiguration,
    vcSubnetIds,
    vcRoleARN,
    vcSecurityGroupIds,

    -- * HECEndpoint
    HECEndpoint (..),

    -- * ExtendedS3DestinationDescription
    ExtendedS3DestinationDescription (..),
    mkExtendedS3DestinationDescription,
    esddRoleARN,
    esddBucketARN,
    esddBufferingHints,
    esddCompressionFormat,
    esddEncryptionConfiguration,
    esddCloudWatchLoggingOptions,
    esddDataFormatConversionConfiguration,
    esddErrorOutputPrefix,
    esddPrefix,
    esddProcessingConfiguration,
    esddS3BackupDescription,
    esddS3BackupMode,

    -- * CompressionFormat
    CompressionFormat (..),

    -- * InputFormatConfiguration
    InputFormatConfiguration (..),
    mkInputFormatConfiguration,
    ifcDeserializer,

    -- * TagKey
    TagKey (..),

    -- * Record
    Record (..),
    mkRecord,
    rData,

    -- * KMSEncryptionConfig
    KMSEncryptionConfig (..),
    mkKMSEncryptionConfig,
    kmsecAWSKMSKeyARN,

    -- * BufferingHints
    BufferingHints (..),
    mkBufferingHints,
    bhIntervalInSeconds,
    bhSizeInMBs,

    -- * DeliveryStreamType
    DeliveryStreamType (..),

    -- * NonEmptyStringWithoutWhitespace
    NonEmptyStringWithoutWhitespace (..),

    -- * ProcessorParameterName
    ProcessorParameterName (..),

    -- * DataFormatConversionConfiguration
    DataFormatConversionConfiguration (..),
    mkDataFormatConversionConfiguration,
    dfccEnabled,
    dfccInputFormatConfiguration,
    dfccOutputFormatConfiguration,
    dfccSchemaConfiguration,

    -- * DeliveryStreamEncryptionStatus
    DeliveryStreamEncryptionStatus (..),

    -- * HttpEndpointBufferingHints
    HttpEndpointBufferingHints (..),
    mkHttpEndpointBufferingHints,
    hebhIntervalInSeconds,
    hebhSizeInMBs,

    -- * ErrorMessage
    ErrorMessage (..),

    -- * HttpEndpointRetryOptions
    HttpEndpointRetryOptions (..),
    mkHttpEndpointRetryOptions,
    heroDurationInSeconds,

    -- * BucketARN
    BucketARN (..),

    -- * DataTableColumns
    DataTableColumns (..),

    -- * ParquetWriterVersion
    ParquetWriterVersion (..),

    -- * OrcSerDe
    OrcSerDe (..),
    mkOrcSerDe,
    osdBlockSizeBytes,
    osdBloomFilterColumns,
    osdBloomFilterFalsePositiveProbability,
    osdCompression,
    osdDictionaryKeyThreshold,
    osdEnablePadding,
    osdFormatVersion,
    osdPaddingTolerance,
    osdRowIndexStride,
    osdStripeSizeBytes,

    -- * ParquetSerDe
    ParquetSerDe (..),
    mkParquetSerDe,
    psdBlockSizeBytes,
    psdCompression,
    psdEnableDictionaryCompression,
    psdMaxPaddingBytes,
    psdPageSizeBytes,
    psdWriterVersion,

    -- * SplunkDestinationConfiguration
    SplunkDestinationConfiguration (..),
    mkSplunkDestinationConfiguration,
    sdcfHECEndpoint,
    sdcfHECEndpointType,
    sdcfHECToken,
    sdcfS3Configuration,
    sdcfCloudWatchLoggingOptions,
    sdcfHECAcknowledgmentTimeoutInSeconds,
    sdcfProcessingConfiguration,
    sdcfRetryOptions,
    sdcfS3BackupMode,

    -- * ProcessingConfiguration
    ProcessingConfiguration (..),
    mkProcessingConfiguration,
    pcEnabled,
    pcProcessors,

    -- * ClusterJDBCURL
    ClusterJDBCURL (..),

    -- * HttpEndpointS3BackupMode
    HttpEndpointS3BackupMode (..),

    -- * ProcessorType
    ProcessorType (..),

    -- * DeliveryStreamDescription
    DeliveryStreamDescription (..),
    mkDeliveryStreamDescription,
    dsdDeliveryStreamName,
    dsdDeliveryStreamARN,
    dsdDeliveryStreamStatus,
    dsdDeliveryStreamType,
    dsdVersionId,
    dsdDestinations,
    dsdHasMoreDestinations,
    dsdCreateTimestamp,
    dsdDeliveryStreamEncryptionConfiguration,
    dsdFailureDescription,
    dsdLastUpdateTimestamp,
    dsdSource,

    -- * HttpEndpointDestinationUpdate
    HttpEndpointDestinationUpdate (..),
    mkHttpEndpointDestinationUpdate,
    heduBufferingHints,
    heduCloudWatchLoggingOptions,
    heduEndpointConfiguration,
    heduProcessingConfiguration,
    heduRequestConfiguration,
    heduRetryOptions,
    heduRoleARN,
    heduS3BackupMode,
    heduS3Update,

    -- * ElasticsearchDomainARN
    ElasticsearchDomainARN (..),

    -- * KinesisStreamARN
    KinesisStreamARN (..),

    -- * RoleARN
    RoleARN (..),

    -- * ElasticsearchDestinationDescription
    ElasticsearchDestinationDescription (..),
    mkElasticsearchDestinationDescription,
    eddBufferingHints,
    eddCloudWatchLoggingOptions,
    eddClusterEndpoint,
    eddDomainARN,
    eddIndexName,
    eddIndexRotationPeriod,
    eddProcessingConfiguration,
    eddRetryOptions,
    eddRoleARN,
    eddS3BackupMode,
    eddS3DestinationDescription,
    eddTypeName,
    eddVpcConfigurationDescription,

    -- * DataTableName
    DataTableName (..),

    -- * RedshiftDestinationDescription
    RedshiftDestinationDescription (..),
    mkRedshiftDestinationDescription,
    rddRoleARN,
    rddClusterJDBCURL,
    rddCopyCommand,
    rddUsername,
    rddS3DestinationDescription,
    rddCloudWatchLoggingOptions,
    rddProcessingConfiguration,
    rddRetryOptions,
    rddS3BackupDescription,
    rddS3BackupMode,

    -- * DestinationId
    DestinationId (..),

    -- * DeliveryStreamEncryptionConfigurationInput
    DeliveryStreamEncryptionConfigurationInput (..),
    mkDeliveryStreamEncryptionConfigurationInput,
    dseciKeyType,
    dseciKeyARN,

    -- * RecordId
    RecordId (..),

    -- * ParameterValue
    ParameterValue (..),

    -- * IndexName
    IndexName (..),

    -- * ClusterEndpoint
    ClusterEndpoint (..),

    -- * DomainARN
    DomainARN (..),

    -- * TypeName
    TypeName (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * Url
    Url (..),

    -- * Name
    Name (..),

    -- * VpcId
    VpcId (..),

    -- * Details
    Details (..),

    -- * CatalogId
    CatalogId (..),

    -- * DatabaseName
    DatabaseName (..),

    -- * Region
    Region (..),

    -- * TableName
    TableName (..),

    -- * VersionId
    VersionId (..),

    -- * ExclusiveStartDeliveryStreamName
    ExclusiveStartDeliveryStreamName (..),

    -- * KeyARN
    KeyARN (..),

    -- * ExclusiveStartDestinationId
    ExclusiveStartDestinationId (..),
  )
where

import Network.AWS.Firehose.Types.AWSKMSKeyARN
import Network.AWS.Firehose.Types.BucketARN
import Network.AWS.Firehose.Types.BufferingHints
import Network.AWS.Firehose.Types.CatalogId
import Network.AWS.Firehose.Types.CloudWatchLoggingOptions
import Network.AWS.Firehose.Types.ClusterEndpoint
import Network.AWS.Firehose.Types.ClusterJDBCURL
import Network.AWS.Firehose.Types.CompressionFormat
import Network.AWS.Firehose.Types.ContentEncoding
import Network.AWS.Firehose.Types.CopyCommand
import Network.AWS.Firehose.Types.CopyOptions
import Network.AWS.Firehose.Types.DataFormatConversionConfiguration
import Network.AWS.Firehose.Types.DataTableColumns
import Network.AWS.Firehose.Types.DataTableName
import Network.AWS.Firehose.Types.DatabaseName
import Network.AWS.Firehose.Types.DeliveryStreamARN
import Network.AWS.Firehose.Types.DeliveryStreamDescription
import Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfiguration
import Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfigurationInput
import Network.AWS.Firehose.Types.DeliveryStreamEncryptionStatus
import Network.AWS.Firehose.Types.DeliveryStreamFailureType
import Network.AWS.Firehose.Types.DeliveryStreamName
import Network.AWS.Firehose.Types.DeliveryStreamStatus
import Network.AWS.Firehose.Types.DeliveryStreamType
import Network.AWS.Firehose.Types.DeliveryStreamVersionId
import Network.AWS.Firehose.Types.Deserializer
import Network.AWS.Firehose.Types.DestinationDescription
import Network.AWS.Firehose.Types.DestinationId
import Network.AWS.Firehose.Types.Details
import Network.AWS.Firehose.Types.DomainARN
import Network.AWS.Firehose.Types.ElasticsearchBufferingHints
import Network.AWS.Firehose.Types.ElasticsearchClusterEndpoint
import Network.AWS.Firehose.Types.ElasticsearchDestinationConfiguration
import Network.AWS.Firehose.Types.ElasticsearchDestinationDescription
import Network.AWS.Firehose.Types.ElasticsearchDestinationUpdate
import Network.AWS.Firehose.Types.ElasticsearchDomainARN
import Network.AWS.Firehose.Types.ElasticsearchIndexName
import Network.AWS.Firehose.Types.ElasticsearchIndexRotationPeriod
import Network.AWS.Firehose.Types.ElasticsearchRetryOptions
import Network.AWS.Firehose.Types.ElasticsearchS3BackupMode
import Network.AWS.Firehose.Types.ElasticsearchTypeName
import Network.AWS.Firehose.Types.EncryptionConfiguration
import Network.AWS.Firehose.Types.ErrorCode
import Network.AWS.Firehose.Types.ErrorMessage
import Network.AWS.Firehose.Types.ErrorOutputPrefix
import Network.AWS.Firehose.Types.ExclusiveStartDeliveryStreamName
import Network.AWS.Firehose.Types.ExclusiveStartDestinationId
import Network.AWS.Firehose.Types.ExtendedS3DestinationConfiguration
import Network.AWS.Firehose.Types.ExtendedS3DestinationDescription
import Network.AWS.Firehose.Types.ExtendedS3DestinationUpdate
import Network.AWS.Firehose.Types.FailureDescription
import Network.AWS.Firehose.Types.HECEndpoint
import Network.AWS.Firehose.Types.HECEndpointType
import Network.AWS.Firehose.Types.HECToken
import Network.AWS.Firehose.Types.HiveJsonSerDe
import Network.AWS.Firehose.Types.HttpEndpointAccessKey
import Network.AWS.Firehose.Types.HttpEndpointAttributeName
import Network.AWS.Firehose.Types.HttpEndpointAttributeValue
import Network.AWS.Firehose.Types.HttpEndpointBufferingHints
import Network.AWS.Firehose.Types.HttpEndpointCommonAttribute
import Network.AWS.Firehose.Types.HttpEndpointConfiguration
import Network.AWS.Firehose.Types.HttpEndpointDescription
import Network.AWS.Firehose.Types.HttpEndpointDestinationConfiguration
import Network.AWS.Firehose.Types.HttpEndpointDestinationDescription
import Network.AWS.Firehose.Types.HttpEndpointDestinationUpdate
import Network.AWS.Firehose.Types.HttpEndpointName
import Network.AWS.Firehose.Types.HttpEndpointRequestConfiguration
import Network.AWS.Firehose.Types.HttpEndpointRetryOptions
import Network.AWS.Firehose.Types.HttpEndpointS3BackupMode
import Network.AWS.Firehose.Types.IndexName
import Network.AWS.Firehose.Types.InputFormatConfiguration
import Network.AWS.Firehose.Types.KMSEncryptionConfig
import Network.AWS.Firehose.Types.Key
import Network.AWS.Firehose.Types.KeyARN
import Network.AWS.Firehose.Types.KeyType
import Network.AWS.Firehose.Types.KinesisStreamARN
import Network.AWS.Firehose.Types.KinesisStreamSourceConfiguration
import Network.AWS.Firehose.Types.KinesisStreamSourceDescription
import Network.AWS.Firehose.Types.LogGroupName
import Network.AWS.Firehose.Types.LogStreamName
import Network.AWS.Firehose.Types.Name
import Network.AWS.Firehose.Types.NoEncryptionConfig
import Network.AWS.Firehose.Types.NonEmptyString
import Network.AWS.Firehose.Types.NonEmptyStringWithoutWhitespace
import Network.AWS.Firehose.Types.OpenXJsonSerDe
import Network.AWS.Firehose.Types.OrcCompression
import Network.AWS.Firehose.Types.OrcFormatVersion
import Network.AWS.Firehose.Types.OrcSerDe
import Network.AWS.Firehose.Types.OutputFormatConfiguration
import Network.AWS.Firehose.Types.ParameterValue
import Network.AWS.Firehose.Types.ParquetCompression
import Network.AWS.Firehose.Types.ParquetSerDe
import Network.AWS.Firehose.Types.ParquetWriterVersion
import Network.AWS.Firehose.Types.Password
import Network.AWS.Firehose.Types.Prefix
import Network.AWS.Firehose.Types.ProcessingConfiguration
import Network.AWS.Firehose.Types.Processor
import Network.AWS.Firehose.Types.ProcessorParameter
import Network.AWS.Firehose.Types.ProcessorParameterName
import Network.AWS.Firehose.Types.ProcessorType
import Network.AWS.Firehose.Types.PutRecordBatchResponseEntry
import Network.AWS.Firehose.Types.PutResponseRecordId
import Network.AWS.Firehose.Types.Record
import Network.AWS.Firehose.Types.RecordId
import Network.AWS.Firehose.Types.RedshiftDestinationConfiguration
import Network.AWS.Firehose.Types.RedshiftDestinationDescription
import Network.AWS.Firehose.Types.RedshiftDestinationUpdate
import Network.AWS.Firehose.Types.RedshiftRetryOptions
import Network.AWS.Firehose.Types.RedshiftS3BackupMode
import Network.AWS.Firehose.Types.Region
import Network.AWS.Firehose.Types.RoleARN
import Network.AWS.Firehose.Types.S3BackupMode
import Network.AWS.Firehose.Types.S3DestinationConfiguration
import Network.AWS.Firehose.Types.S3DestinationDescription
import Network.AWS.Firehose.Types.S3DestinationUpdate
import Network.AWS.Firehose.Types.SchemaConfiguration
import Network.AWS.Firehose.Types.Serializer
import Network.AWS.Firehose.Types.SourceDescription
import Network.AWS.Firehose.Types.SplunkDestinationConfiguration
import Network.AWS.Firehose.Types.SplunkDestinationDescription
import Network.AWS.Firehose.Types.SplunkDestinationUpdate
import Network.AWS.Firehose.Types.SplunkRetryOptions
import Network.AWS.Firehose.Types.SplunkS3BackupMode
import Network.AWS.Firehose.Types.TableName
import Network.AWS.Firehose.Types.Tag
import Network.AWS.Firehose.Types.TagKey
import Network.AWS.Firehose.Types.TypeName
import Network.AWS.Firehose.Types.Url
import Network.AWS.Firehose.Types.Username
import Network.AWS.Firehose.Types.Value
import Network.AWS.Firehose.Types.VersionId
import Network.AWS.Firehose.Types.VpcConfiguration
import Network.AWS.Firehose.Types.VpcConfigurationDescription
import Network.AWS.Firehose.Types.VpcId
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-08-04@ of the Amazon Kinesis Firehose SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "Firehose",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "firehose",
      Core._svcVersion = "2015-08-04",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "Firehose",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The specified input parameter has a value that is not valid.
_InvalidArgumentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArgumentException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidArgumentException"
{-# DEPRECATED _InvalidArgumentException "Use generic-lens or generic-optics instead." #-}

-- | Kinesis Data Firehose throws this exception when an attempt to put records or to start or stop delivery stream encryption fails. This happens when the KMS service throws one of the following exception types: @AccessDeniedException@ , @InvalidStateException@ , @DisabledException@ , or @NotFoundException@ .
_InvalidKMSResourceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidKMSResourceException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidKMSResourceException"
{-# DEPRECATED _InvalidKMSResourceException "Use generic-lens or generic-optics instead." #-}

-- | Another modification has already happened. Fetch @VersionId@ again and use it to update the destination.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    mkServiceConfig
    "ConcurrentModificationException"
{-# DEPRECATED _ConcurrentModificationException "Use generic-lens or generic-optics instead." #-}

-- | The service is unavailable. Back off and retry the operation. If you continue to see the exception, throughput limits for the delivery stream may have been exceeded. For more information about limits and how to request an increase, see <https://docs.aws.amazon.com/firehose/latest/dev/limits.html Amazon Kinesis Data Firehose Limits> .
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    mkServiceConfig
    "ServiceUnavailableException"
{-# DEPRECATED _ServiceUnavailableException "Use generic-lens or generic-optics instead." #-}

-- | The specified resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceNotFoundException"
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | You have already reached the limit for a requested resource.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | The resource is already in use and not available for this operation.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError mkServiceConfig "ResourceInUseException"
{-# DEPRECATED _ResourceInUseException "Use generic-lens or generic-optics instead." #-}

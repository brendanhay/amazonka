{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Kinesis Analytics is the easiest way to process streaming data in real time with standard SQL without having to learn new programming languages or processing frameworks. Amazon Kinesis Analytics enables you to create and run SQL queries on streaming data so that you can gain actionable insights and respond to your business and customer needs promptly.
--
--
-- Amazon Kinesis Analytics takes care of everything required to run your queries continuously and scales automatically to match the volume and throughput rate of your incoming data. With Amazon Kinesis Analytics, you only pay for the resources your queries consume. There is no minimum fee or setup cost.
module Network.AWS.KinesisAnalytics
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** InvalidApplicationConfigurationException
    _InvalidApplicationConfigurationException,

    -- ** ResourceProvisionedThroughputExceededException
    _ResourceProvisionedThroughputExceededException,

    -- ** UnsupportedOperationException
    _UnsupportedOperationException,

    -- ** InvalidArgumentException
    _InvalidArgumentException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** CodeValidationException
    _CodeValidationException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** UnableToDetectSchemaException
    _UnableToDetectSchemaException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AddApplicationOutput
    module Network.AWS.KinesisAnalytics.AddApplicationOutput,

    -- ** DiscoverInputSchema
    module Network.AWS.KinesisAnalytics.DiscoverInputSchema,

    -- ** DescribeApplication
    module Network.AWS.KinesisAnalytics.DescribeApplication,

    -- ** StartApplication
    module Network.AWS.KinesisAnalytics.StartApplication,

    -- ** ListTagsForResource
    module Network.AWS.KinesisAnalytics.ListTagsForResource,

    -- ** DeleteApplicationReferenceDataSource
    module Network.AWS.KinesisAnalytics.DeleteApplicationReferenceDataSource,

    -- ** DeleteApplication
    module Network.AWS.KinesisAnalytics.DeleteApplication,

    -- ** UpdateApplication
    module Network.AWS.KinesisAnalytics.UpdateApplication,

    -- ** DeleteApplicationCloudWatchLoggingOption
    module Network.AWS.KinesisAnalytics.DeleteApplicationCloudWatchLoggingOption,

    -- ** AddApplicationInputProcessingConfiguration
    module Network.AWS.KinesisAnalytics.AddApplicationInputProcessingConfiguration,

    -- ** CreateApplication
    module Network.AWS.KinesisAnalytics.CreateApplication,

    -- ** DeleteApplicationOutput
    module Network.AWS.KinesisAnalytics.DeleteApplicationOutput,

    -- ** StopApplication
    module Network.AWS.KinesisAnalytics.StopApplication,

    -- ** AddApplicationReferenceDataSource
    module Network.AWS.KinesisAnalytics.AddApplicationReferenceDataSource,

    -- ** AddApplicationInput
    module Network.AWS.KinesisAnalytics.AddApplicationInput,

    -- ** TagResource
    module Network.AWS.KinesisAnalytics.TagResource,

    -- ** AddApplicationCloudWatchLoggingOption
    module Network.AWS.KinesisAnalytics.AddApplicationCloudWatchLoggingOption,

    -- ** ListApplications
    module Network.AWS.KinesisAnalytics.ListApplications,

    -- ** UntagResource
    module Network.AWS.KinesisAnalytics.UntagResource,

    -- ** DeleteApplicationInputProcessingConfiguration
    module Network.AWS.KinesisAnalytics.DeleteApplicationInputProcessingConfiguration,

    -- * Types

    -- ** ApplicationDescription
    ApplicationDescription (..),

    -- ** RecordColumnName
    RecordColumnName (..),

    -- ** InputStartingPosition
    InputStartingPosition (..),

    -- ** InputUpdate
    InputUpdate (..),
    mkInputUpdate,
    iuInputId,
    iuInputParallelismUpdate,
    iuInputProcessingConfigurationUpdate,
    iuInputSchemaUpdate,
    iuKinesisFirehoseInputUpdate,
    iuKinesisStreamsInputUpdate,
    iuNamePrefixUpdate,

    -- ** InputLambdaProcessor
    InputLambdaProcessor (..),
    mkInputLambdaProcessor,
    ilpResourceARN,
    ilpRoleARN,

    -- ** InputStartingPositionConfiguration
    InputStartingPositionConfiguration (..),
    mkInputStartingPositionConfiguration,
    ispcInputStartingPosition,

    -- ** RawInputRecord
    RawInputRecord (..),

    -- ** ReferenceDataSourceUpdate
    ReferenceDataSourceUpdate (..),
    mkReferenceDataSourceUpdate,
    rdsuReferenceId,
    rdsuReferenceSchemaUpdate,
    rdsuS3ReferenceDataSourceUpdate,
    rdsuTableNameUpdate,

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** InputParallelism
    InputParallelism (..),
    mkInputParallelism,
    ipCount,

    -- ** LogStreamARN
    LogStreamARN (..),

    -- ** DestinationSchema
    DestinationSchema (..),
    mkDestinationSchema,
    dsRecordFormatType,

    -- ** InputProcessingConfiguration
    InputProcessingConfiguration (..),
    mkInputProcessingConfiguration,
    ipcInputLambdaProcessor,

    -- ** LambdaOutput
    LambdaOutput (..),
    mkLambdaOutput,
    loResourceARN,
    loRoleARN,

    -- ** S3Configuration
    S3Configuration (..),
    mkS3Configuration,
    scRoleARN,
    scBucketARN,
    scFileKey,

    -- ** ParsedInputRecordField
    ParsedInputRecordField (..),

    -- ** KinesisFirehoseOutputDescription
    KinesisFirehoseOutputDescription (..),
    mkKinesisFirehoseOutputDescription,
    kfodResourceARN,
    kfodRoleARN,

    -- ** S3ReferenceDataSourceDescription
    S3ReferenceDataSourceDescription (..),
    mkS3ReferenceDataSourceDescription,
    srdsdBucketARN,
    srdsdFileKey,
    srdsdReferenceRoleARN,

    -- ** KinesisAnalyticsARN
    KinesisAnalyticsARN (..),

    -- ** ReferenceDataSource
    ReferenceDataSource (..),
    mkReferenceDataSource,
    rdsTableName,
    rdsReferenceSchema,
    rdsS3ReferenceDataSource,

    -- ** RecordFormat
    RecordFormat (..),
    mkRecordFormat,
    rfRecordFormatType,
    rfMappingParameters,

    -- ** InputLambdaProcessorUpdate
    InputLambdaProcessorUpdate (..),
    mkInputLambdaProcessorUpdate,
    ilpuResourceARNUpdate,
    ilpuRoleARNUpdate,

    -- ** CloudWatchLoggingOptionDescription
    CloudWatchLoggingOptionDescription (..),
    mkCloudWatchLoggingOptionDescription,
    cwlodLogStreamARN,
    cwlodRoleARN,
    cwlodCloudWatchLoggingOptionId,

    -- ** Input
    Input (..),
    mkInput,
    iNamePrefix,
    iInputSchema,
    iInputParallelism,
    iInputProcessingConfiguration,
    iKinesisFirehoseInput,
    iKinesisStreamsInput,

    -- ** KinesisStreamsOutputDescription
    KinesisStreamsOutputDescription (..),
    mkKinesisStreamsOutputDescription,
    ksodResourceARN,
    ksodRoleARN,

    -- ** OutputDescription
    OutputDescription (..),
    mkOutputDescription,
    odDestinationSchema,
    odKinesisFirehoseOutputDescription,
    odKinesisStreamsOutputDescription,
    odLambdaOutputDescription,
    odName,
    odOutputId,

    -- ** KinesisStreamsOutputUpdate
    KinesisStreamsOutputUpdate (..),
    mkKinesisStreamsOutputUpdate,
    ksouResourceARNUpdate,
    ksouRoleARNUpdate,

    -- ** KinesisFirehoseInputDescription
    KinesisFirehoseInputDescription (..),
    mkKinesisFirehoseInputDescription,
    kfidResourceARN,
    kfidRoleARN,

    -- ** CloudWatchLoggingOptionUpdate
    CloudWatchLoggingOptionUpdate (..),
    mkCloudWatchLoggingOptionUpdate,
    cwlouCloudWatchLoggingOptionId,
    cwlouLogStreamARNUpdate,
    cwlouRoleARNUpdate,

    -- ** KinesisStreamsInput
    KinesisStreamsInput (..),
    mkKinesisStreamsInput,
    ksiResourceARN,
    ksiRoleARN,

    -- ** InputProcessingConfigurationUpdate
    InputProcessingConfigurationUpdate (..),
    mkInputProcessingConfigurationUpdate,
    ipcuInputLambdaProcessorUpdate,

    -- ** SourceSchema
    SourceSchema (..),
    mkSourceSchema,
    ssRecordFormat,
    ssRecordColumns,
    ssRecordEncoding,

    -- ** RecordRowDelimiter
    RecordRowDelimiter (..),

    -- ** Output
    Output (..),
    mkOutput,
    oName,
    oDestinationSchema,
    oKinesisFirehoseOutput,
    oKinesisStreamsOutput,
    oLambdaOutput,

    -- ** InAppTableName
    InAppTableName (..),

    -- ** KinesisFirehoseInput
    KinesisFirehoseInput (..),
    mkKinesisFirehoseInput,
    kfiResourceARN,
    kfiRoleARN,

    -- ** ResourceARN
    ResourceARN (..),

    -- ** S3ReferenceDataSourceUpdate
    S3ReferenceDataSourceUpdate (..),
    mkS3ReferenceDataSourceUpdate,
    srdsuBucketARNUpdate,
    srdsuFileKeyUpdate,
    srdsuReferenceRoleARNUpdate,

    -- ** RecordColumnMapping
    RecordColumnMapping (..),

    -- ** KinesisFirehoseOutputUpdate
    KinesisFirehoseOutputUpdate (..),
    mkKinesisFirehoseOutputUpdate,
    kfouResourceARNUpdate,
    kfouRoleARNUpdate,

    -- ** ApplicationCode
    ApplicationCode (..),

    -- ** KinesisStreamsInputDescription
    KinesisStreamsInputDescription (..),
    mkKinesisStreamsInputDescription,
    ksidResourceARN,
    ksidRoleARN,

    -- ** RecordRowPath
    RecordRowPath (..),

    -- ** ApplicationName
    ApplicationName (..),

    -- ** Id
    Id (..),

    -- ** InAppStreamName
    InAppStreamName (..),

    -- ** RecordColumnSqlType
    RecordColumnSqlType (..),

    -- ** TagKey
    TagKey (..),

    -- ** ReferenceDataSourceDescription
    ReferenceDataSourceDescription (..),
    mkReferenceDataSourceDescription,
    rdsdReferenceId,
    rdsdTableName,
    rdsdS3ReferenceDataSourceDescription,
    rdsdReferenceSchema,

    -- ** InputDescription
    InputDescription (..),
    mkInputDescription,
    idInAppStreamNames,
    idInputId,
    idInputParallelism,
    idInputProcessingConfigurationDescription,
    idInputSchema,
    idInputStartingPositionConfiguration,
    idKinesisFirehoseInputDescription,
    idKinesisStreamsInputDescription,
    idNamePrefix,

    -- ** CSVMappingParameters
    CSVMappingParameters (..),
    mkCSVMappingParameters,
    csvmpRecordRowDelimiter,
    csvmpRecordColumnDelimiter,

    -- ** RecordColumnDelimiter
    RecordColumnDelimiter (..),

    -- ** KinesisStreamsOutput
    KinesisStreamsOutput (..),
    mkKinesisStreamsOutput,
    ksoResourceARN,
    ksoRoleARN,

    -- ** CloudWatchLoggingOption
    CloudWatchLoggingOption (..),
    mkCloudWatchLoggingOption,
    cwloLogStreamARN,
    cwloRoleARN,

    -- ** KinesisStreamsInputUpdate
    KinesisStreamsInputUpdate (..),
    mkKinesisStreamsInputUpdate,
    ksiuResourceARNUpdate,
    ksiuRoleARNUpdate,

    -- ** RecordColumn
    RecordColumn (..),
    mkRecordColumn,
    rcName,
    rcSqlType,
    rcMapping,

    -- ** ApplicationUpdate
    ApplicationUpdate (..),
    mkApplicationUpdate,
    auApplicationCodeUpdate,
    auCloudWatchLoggingOptionUpdates,
    auInputUpdates,
    auOutputUpdates,
    auReferenceDataSourceUpdates,

    -- ** BucketARN
    BucketARN (..),

    -- ** InputParallelismUpdate
    InputParallelismUpdate (..),
    mkInputParallelismUpdate,
    ipuCountUpdate,

    -- ** ApplicationDetail
    ApplicationDetail (..),
    mkApplicationDetail,
    adApplicationName,
    adApplicationARN,
    adApplicationStatus,
    adApplicationVersionId,
    adApplicationCode,
    adApplicationDescription,
    adCloudWatchLoggingOptionDescriptions,
    adCreateTimestamp,
    adInputDescriptions,
    adLastUpdateTimestamp,
    adOutputDescriptions,
    adReferenceDataSourceDescriptions,

    -- ** InputProcessingConfigurationDescription
    InputProcessingConfigurationDescription (..),
    mkInputProcessingConfigurationDescription,
    ipcdInputLambdaProcessorDescription,

    -- ** LambdaOutputDescription
    LambdaOutputDescription (..),
    mkLambdaOutputDescription,
    lodResourceARN,
    lodRoleARN,

    -- ** ApplicationStatus
    ApplicationStatus (..),

    -- ** RecordEncoding
    RecordEncoding (..),

    -- ** KinesisFirehoseOutput
    KinesisFirehoseOutput (..),
    mkKinesisFirehoseOutput,
    kfoResourceARN,
    kfoRoleARN,

    -- ** InputSchemaUpdate
    InputSchemaUpdate (..),
    mkInputSchemaUpdate,
    isuRecordColumnUpdates,
    isuRecordEncodingUpdate,
    isuRecordFormatUpdate,

    -- ** JSONMappingParameters
    JSONMappingParameters (..),
    mkJSONMappingParameters,
    jsonmpRecordRowPath,

    -- ** OutputUpdate
    OutputUpdate (..),
    mkOutputUpdate,
    ouOutputId,
    ouDestinationSchemaUpdate,
    ouKinesisFirehoseOutputUpdate,
    ouKinesisStreamsOutputUpdate,
    ouLambdaOutputUpdate,
    ouNameUpdate,

    -- ** S3ReferenceDataSource
    S3ReferenceDataSource (..),
    mkS3ReferenceDataSource,
    srdsBucketARN,
    srdsFileKey,
    srdsReferenceRoleARN,

    -- ** KinesisFirehoseInputUpdate
    KinesisFirehoseInputUpdate (..),
    mkKinesisFirehoseInputUpdate,
    kfiuResourceARNUpdate,
    kfiuRoleARNUpdate,

    -- ** ApplicationSummary
    ApplicationSummary (..),
    mkApplicationSummary,
    asApplicationName,
    asApplicationARN,
    asApplicationStatus,

    -- ** MappingParameters
    MappingParameters (..),
    mkMappingParameters,
    mpCSVMappingParameters,
    mpJSONMappingParameters,

    -- ** InputConfiguration
    InputConfiguration (..),
    mkInputConfiguration,
    icId,
    icInputStartingPositionConfiguration,

    -- ** RecordFormatType
    RecordFormatType (..),

    -- ** FileKey
    FileKey (..),

    -- ** ProcessedInputRecord
    ProcessedInputRecord (..),

    -- ** LambdaOutputUpdate
    LambdaOutputUpdate (..),
    mkLambdaOutputUpdate,
    louResourceARNUpdate,
    louRoleARNUpdate,

    -- ** RoleARN
    RoleARN (..),

    -- ** InputLambdaProcessorDescription
    InputLambdaProcessorDescription (..),
    mkInputLambdaProcessorDescription,
    ilpdResourceARN,
    ilpdRoleARN,

    -- ** ReferenceId
    ReferenceId (..),

    -- ** InputId
    InputId (..),

    -- ** NamePrefixUpdate
    NamePrefixUpdate (..),

    -- ** TableNameUpdate
    TableNameUpdate (..),

    -- ** Key
    Key (..),

    -- ** Value
    Value (..),

    -- ** CloudWatchLoggingOptionId
    CloudWatchLoggingOptionId (..),

    -- ** OutputId
    OutputId (..),

    -- ** ReferenceRoleARN
    ReferenceRoleARN (..),

    -- ** TableName
    TableName (..),

    -- ** ResourceARNUpdate
    ResourceARNUpdate (..),

    -- ** RoleARNUpdate
    RoleARNUpdate (..),

    -- ** NamePrefix
    NamePrefix (..),

    -- ** Name
    Name (..),

    -- ** BucketARNUpdate
    BucketARNUpdate (..),

    -- ** FileKeyUpdate
    FileKeyUpdate (..),

    -- ** ReferenceRoleARNUpdate
    ReferenceRoleARNUpdate (..),

    -- ** ExclusiveStartApplicationName
    ExclusiveStartApplicationName (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
  )
where

import Network.AWS.KinesisAnalytics.AddApplicationCloudWatchLoggingOption
import Network.AWS.KinesisAnalytics.AddApplicationInput
import Network.AWS.KinesisAnalytics.AddApplicationInputProcessingConfiguration
import Network.AWS.KinesisAnalytics.AddApplicationOutput
import Network.AWS.KinesisAnalytics.AddApplicationReferenceDataSource
import Network.AWS.KinesisAnalytics.CreateApplication
import Network.AWS.KinesisAnalytics.DeleteApplication
import Network.AWS.KinesisAnalytics.DeleteApplicationCloudWatchLoggingOption
import Network.AWS.KinesisAnalytics.DeleteApplicationInputProcessingConfiguration
import Network.AWS.KinesisAnalytics.DeleteApplicationOutput
import Network.AWS.KinesisAnalytics.DeleteApplicationReferenceDataSource
import Network.AWS.KinesisAnalytics.DescribeApplication
import Network.AWS.KinesisAnalytics.DiscoverInputSchema
import Network.AWS.KinesisAnalytics.ListApplications
import Network.AWS.KinesisAnalytics.ListTagsForResource
import Network.AWS.KinesisAnalytics.StartApplication
import Network.AWS.KinesisAnalytics.StopApplication
import Network.AWS.KinesisAnalytics.TagResource
import Network.AWS.KinesisAnalytics.Types
import Network.AWS.KinesisAnalytics.UntagResource
import Network.AWS.KinesisAnalytics.UpdateApplication
import Network.AWS.KinesisAnalytics.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'KinesisAnalytics'.

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

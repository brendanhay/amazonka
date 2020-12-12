{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
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
    kinesisAnalyticsService,

    -- * Errors
    -- $errors

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

    -- ** ApplicationStatus
    ApplicationStatus (..),

    -- ** InputStartingPosition
    InputStartingPosition (..),

    -- ** RecordFormatType
    RecordFormatType (..),

    -- ** ApplicationDetail
    ApplicationDetail (..),
    mkApplicationDetail,
    adApplicationDescription,
    adOutputDescriptions,
    adCloudWatchLoggingOptionDescriptions,
    adReferenceDataSourceDescriptions,
    adInputDescriptions,
    adApplicationCode,
    adCreateTimestamp,
    adLastUpdateTimestamp,
    adApplicationName,
    adApplicationARN,
    adApplicationStatus,
    adApplicationVersionId,

    -- ** ApplicationSummary
    ApplicationSummary (..),
    mkApplicationSummary,
    asApplicationName,
    asApplicationARN,
    asApplicationStatus,

    -- ** ApplicationUpdate
    ApplicationUpdate (..),
    mkApplicationUpdate,
    auReferenceDataSourceUpdates,
    auInputUpdates,
    auCloudWatchLoggingOptionUpdates,
    auOutputUpdates,
    auApplicationCodeUpdate,

    -- ** CSVMappingParameters
    CSVMappingParameters (..),
    mkCSVMappingParameters,
    cmpRecordRowDelimiter,
    cmpRecordColumnDelimiter,

    -- ** CloudWatchLoggingOption
    CloudWatchLoggingOption (..),
    mkCloudWatchLoggingOption,
    cwloLogStreamARN,
    cwloRoleARN,

    -- ** CloudWatchLoggingOptionDescription
    CloudWatchLoggingOptionDescription (..),
    mkCloudWatchLoggingOptionDescription,
    cwlodCloudWatchLoggingOptionId,
    cwlodLogStreamARN,
    cwlodRoleARN,

    -- ** CloudWatchLoggingOptionUpdate
    CloudWatchLoggingOptionUpdate (..),
    mkCloudWatchLoggingOptionUpdate,
    cwlouRoleARNUpdate,
    cwlouLogStreamARNUpdate,
    cwlouCloudWatchLoggingOptionId,

    -- ** DestinationSchema
    DestinationSchema (..),
    mkDestinationSchema,
    dsRecordFormatType,

    -- ** Input
    Input (..),
    mkInput,
    iInputParallelism,
    iInputProcessingConfiguration,
    iKinesisStreamsInput,
    iKinesisFirehoseInput,
    iNamePrefix,
    iInputSchema,

    -- ** InputConfiguration
    InputConfiguration (..),
    mkInputConfiguration,
    icId,
    icInputStartingPositionConfiguration,

    -- ** InputDescription
    InputDescription (..),
    mkInputDescription,
    idInputStartingPositionConfiguration,
    idInputParallelism,
    idInputId,
    idInAppStreamNames,
    idKinesisFirehoseInputDescription,
    idInputSchema,
    idKinesisStreamsInputDescription,
    idNamePrefix,
    idInputProcessingConfigurationDescription,

    -- ** InputLambdaProcessor
    InputLambdaProcessor (..),
    mkInputLambdaProcessor,
    ilpResourceARN,
    ilpRoleARN,

    -- ** InputLambdaProcessorDescription
    InputLambdaProcessorDescription (..),
    mkInputLambdaProcessorDescription,
    ilpdResourceARN,
    ilpdRoleARN,

    -- ** InputLambdaProcessorUpdate
    InputLambdaProcessorUpdate (..),
    mkInputLambdaProcessorUpdate,
    ilpuRoleARNUpdate,
    ilpuResourceARNUpdate,

    -- ** InputParallelism
    InputParallelism (..),
    mkInputParallelism,
    ipCount,

    -- ** InputParallelismUpdate
    InputParallelismUpdate (..),
    mkInputParallelismUpdate,
    ipuCountUpdate,

    -- ** InputProcessingConfiguration
    InputProcessingConfiguration (..),
    mkInputProcessingConfiguration,
    ipcInputLambdaProcessor,

    -- ** InputProcessingConfigurationDescription
    InputProcessingConfigurationDescription (..),
    mkInputProcessingConfigurationDescription,
    ipcdInputLambdaProcessorDescription,

    -- ** InputProcessingConfigurationUpdate
    InputProcessingConfigurationUpdate (..),
    mkInputProcessingConfigurationUpdate,
    ipcuInputLambdaProcessorUpdate,

    -- ** InputSchemaUpdate
    InputSchemaUpdate (..),
    mkInputSchemaUpdate,
    isuRecordFormatUpdate,
    isuRecordEncodingUpdate,
    isuRecordColumnUpdates,

    -- ** InputStartingPositionConfiguration
    InputStartingPositionConfiguration (..),
    mkInputStartingPositionConfiguration,
    ispcInputStartingPosition,

    -- ** InputUpdate
    InputUpdate (..),
    mkInputUpdate,
    iuInputProcessingConfigurationUpdate,
    iuKinesisStreamsInputUpdate,
    iuInputParallelismUpdate,
    iuNamePrefixUpdate,
    iuInputSchemaUpdate,
    iuKinesisFirehoseInputUpdate,
    iuInputId,

    -- ** JSONMappingParameters
    JSONMappingParameters (..),
    mkJSONMappingParameters,
    jmpRecordRowPath,

    -- ** KinesisFirehoseInput
    KinesisFirehoseInput (..),
    mkKinesisFirehoseInput,
    kfiResourceARN,
    kfiRoleARN,

    -- ** KinesisFirehoseInputDescription
    KinesisFirehoseInputDescription (..),
    mkKinesisFirehoseInputDescription,
    kfidResourceARN,
    kfidRoleARN,

    -- ** KinesisFirehoseInputUpdate
    KinesisFirehoseInputUpdate (..),
    mkKinesisFirehoseInputUpdate,
    kfiuRoleARNUpdate,
    kfiuResourceARNUpdate,

    -- ** KinesisFirehoseOutput
    KinesisFirehoseOutput (..),
    mkKinesisFirehoseOutput,
    kfoResourceARN,
    kfoRoleARN,

    -- ** KinesisFirehoseOutputDescription
    KinesisFirehoseOutputDescription (..),
    mkKinesisFirehoseOutputDescription,
    kfodResourceARN,
    kfodRoleARN,

    -- ** KinesisFirehoseOutputUpdate
    KinesisFirehoseOutputUpdate (..),
    mkKinesisFirehoseOutputUpdate,
    kfouRoleARNUpdate,
    kfouResourceARNUpdate,

    -- ** KinesisStreamsInput
    KinesisStreamsInput (..),
    mkKinesisStreamsInput,
    ksiResourceARN,
    ksiRoleARN,

    -- ** KinesisStreamsInputDescription
    KinesisStreamsInputDescription (..),
    mkKinesisStreamsInputDescription,
    ksidResourceARN,
    ksidRoleARN,

    -- ** KinesisStreamsInputUpdate
    KinesisStreamsInputUpdate (..),
    mkKinesisStreamsInputUpdate,
    ksiuRoleARNUpdate,
    ksiuResourceARNUpdate,

    -- ** KinesisStreamsOutput
    KinesisStreamsOutput (..),
    mkKinesisStreamsOutput,
    ksoResourceARN,
    ksoRoleARN,

    -- ** KinesisStreamsOutputDescription
    KinesisStreamsOutputDescription (..),
    mkKinesisStreamsOutputDescription,
    ksodResourceARN,
    ksodRoleARN,

    -- ** KinesisStreamsOutputUpdate
    KinesisStreamsOutputUpdate (..),
    mkKinesisStreamsOutputUpdate,
    ksouRoleARNUpdate,
    ksouResourceARNUpdate,

    -- ** LambdaOutput
    LambdaOutput (..),
    mkLambdaOutput,
    loResourceARN,
    loRoleARN,

    -- ** LambdaOutputDescription
    LambdaOutputDescription (..),
    mkLambdaOutputDescription,
    lodResourceARN,
    lodRoleARN,

    -- ** LambdaOutputUpdate
    LambdaOutputUpdate (..),
    mkLambdaOutputUpdate,
    louRoleARNUpdate,
    louResourceARNUpdate,

    -- ** MappingParameters
    MappingParameters (..),
    mkMappingParameters,
    mpCSVMappingParameters,
    mpJSONMappingParameters,

    -- ** Output
    Output (..),
    mkOutput,
    oLambdaOutput,
    oKinesisStreamsOutput,
    oKinesisFirehoseOutput,
    oName,
    oDestinationSchema,

    -- ** OutputDescription
    OutputDescription (..),
    mkOutputDescription,
    odOutputId,
    odDestinationSchema,
    odKinesisFirehoseOutputDescription,
    odKinesisStreamsOutputDescription,
    odName,
    odLambdaOutputDescription,

    -- ** OutputUpdate
    OutputUpdate (..),
    mkOutputUpdate,
    ouKinesisStreamsOutputUpdate,
    ouDestinationSchemaUpdate,
    ouKinesisFirehoseOutputUpdate,
    ouNameUpdate,
    ouLambdaOutputUpdate,
    ouOutputId,

    -- ** RecordColumn
    RecordColumn (..),
    mkRecordColumn,
    rcMapping,
    rcName,
    rcSqlType,

    -- ** RecordFormat
    RecordFormat (..),
    mkRecordFormat,
    rfMappingParameters,
    rfRecordFormatType,

    -- ** ReferenceDataSource
    ReferenceDataSource (..),
    mkReferenceDataSource,
    rdsS3ReferenceDataSource,
    rdsTableName,
    rdsReferenceSchema,

    -- ** ReferenceDataSourceDescription
    ReferenceDataSourceDescription (..),
    mkReferenceDataSourceDescription,
    rdsdReferenceSchema,
    rdsdReferenceId,
    rdsdTableName,
    rdsdS3ReferenceDataSourceDescription,

    -- ** ReferenceDataSourceUpdate
    ReferenceDataSourceUpdate (..),
    mkReferenceDataSourceUpdate,
    rdsuTableNameUpdate,
    rdsuS3ReferenceDataSourceUpdate,
    rdsuReferenceSchemaUpdate,
    rdsuReferenceId,

    -- ** S3Configuration
    S3Configuration (..),
    mkS3Configuration,
    scRoleARN,
    scBucketARN,
    scFileKey,

    -- ** S3ReferenceDataSource
    S3ReferenceDataSource (..),
    mkS3ReferenceDataSource,
    srdsBucketARN,
    srdsFileKey,
    srdsReferenceRoleARN,

    -- ** S3ReferenceDataSourceDescription
    S3ReferenceDataSourceDescription (..),
    mkS3ReferenceDataSourceDescription,
    srdsdBucketARN,
    srdsdFileKey,
    srdsdReferenceRoleARN,

    -- ** S3ReferenceDataSourceUpdate
    S3ReferenceDataSourceUpdate (..),
    mkS3ReferenceDataSourceUpdate,
    srdsuBucketARNUpdate,
    srdsuFileKeyUpdate,
    srdsuReferenceRoleARNUpdate,

    -- ** SourceSchema
    SourceSchema (..),
    mkSourceSchema,
    ssRecordEncoding,
    ssRecordFormat,
    ssRecordColumns,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
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

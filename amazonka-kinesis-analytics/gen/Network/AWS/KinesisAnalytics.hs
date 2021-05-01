{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Kinesis Analytics is the easiest way to process streaming data in
-- real time with standard SQL without having to learn new programming
-- languages or processing frameworks. Amazon Kinesis Analytics enables you
-- to create and run SQL queries on streaming data so that you can gain
-- actionable insights and respond to your business and customer needs
-- promptly. Amazon Kinesis Analytics takes care of everything required to
-- run your queries continuously and scales automatically to match the
-- volume and throughput rate of your incoming data. With Amazon Kinesis
-- Analytics, you only pay for the resources your queries consume. There is
-- no minimum fee or setup cost.
module Network.AWS.KinesisAnalytics
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidApplicationConfigurationException
    _InvalidApplicationConfigurationException,

    -- ** ResourceProvisionedThroughputExceededException
    _ResourceProvisionedThroughputExceededException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** UnableToDetectSchemaException
    _UnableToDetectSchemaException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** UnsupportedOperationException
    _UnsupportedOperationException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** CodeValidationException
    _CodeValidationException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InvalidArgumentException
    _InvalidArgumentException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteApplicationOutput
    DeleteApplicationOutput (DeleteApplicationOutput'),
    newDeleteApplicationOutput,
    DeleteApplicationOutputResponse (DeleteApplicationOutputResponse'),
    newDeleteApplicationOutputResponse,

    -- ** AddApplicationOutput
    AddApplicationOutput (AddApplicationOutput'),
    newAddApplicationOutput,
    AddApplicationOutputResponse (AddApplicationOutputResponse'),
    newAddApplicationOutputResponse,

    -- ** CreateApplication
    CreateApplication (CreateApplication'),
    newCreateApplication,
    CreateApplicationResponse (CreateApplicationResponse'),
    newCreateApplicationResponse,

    -- ** DeleteApplicationCloudWatchLoggingOption
    DeleteApplicationCloudWatchLoggingOption (DeleteApplicationCloudWatchLoggingOption'),
    newDeleteApplicationCloudWatchLoggingOption,
    DeleteApplicationCloudWatchLoggingOptionResponse (DeleteApplicationCloudWatchLoggingOptionResponse'),
    newDeleteApplicationCloudWatchLoggingOptionResponse,

    -- ** AddApplicationCloudWatchLoggingOption
    AddApplicationCloudWatchLoggingOption (AddApplicationCloudWatchLoggingOption'),
    newAddApplicationCloudWatchLoggingOption,
    AddApplicationCloudWatchLoggingOptionResponse (AddApplicationCloudWatchLoggingOptionResponse'),
    newAddApplicationCloudWatchLoggingOptionResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** StopApplication
    StopApplication (StopApplication'),
    newStopApplication,
    StopApplicationResponse (StopApplicationResponse'),
    newStopApplicationResponse,

    -- ** StartApplication
    StartApplication (StartApplication'),
    newStartApplication,
    StartApplicationResponse (StartApplicationResponse'),
    newStartApplicationResponse,

    -- ** DescribeApplication
    DescribeApplication (DescribeApplication'),
    newDescribeApplication,
    DescribeApplicationResponse (DescribeApplicationResponse'),
    newDescribeApplicationResponse,

    -- ** DiscoverInputSchema
    DiscoverInputSchema (DiscoverInputSchema'),
    newDiscoverInputSchema,
    DiscoverInputSchemaResponse (DiscoverInputSchemaResponse'),
    newDiscoverInputSchemaResponse,

    -- ** AddApplicationInputProcessingConfiguration
    AddApplicationInputProcessingConfiguration (AddApplicationInputProcessingConfiguration'),
    newAddApplicationInputProcessingConfiguration,
    AddApplicationInputProcessingConfigurationResponse (AddApplicationInputProcessingConfigurationResponse'),
    newAddApplicationInputProcessingConfigurationResponse,

    -- ** DeleteApplicationInputProcessingConfiguration
    DeleteApplicationInputProcessingConfiguration (DeleteApplicationInputProcessingConfiguration'),
    newDeleteApplicationInputProcessingConfiguration,
    DeleteApplicationInputProcessingConfigurationResponse (DeleteApplicationInputProcessingConfigurationResponse'),
    newDeleteApplicationInputProcessingConfigurationResponse,

    -- ** DeleteApplication
    DeleteApplication (DeleteApplication'),
    newDeleteApplication,
    DeleteApplicationResponse (DeleteApplicationResponse'),
    newDeleteApplicationResponse,

    -- ** ListApplications
    ListApplications (ListApplications'),
    newListApplications,
    ListApplicationsResponse (ListApplicationsResponse'),
    newListApplicationsResponse,

    -- ** UpdateApplication
    UpdateApplication (UpdateApplication'),
    newUpdateApplication,
    UpdateApplicationResponse (UpdateApplicationResponse'),
    newUpdateApplicationResponse,

    -- ** AddApplicationInput
    AddApplicationInput (AddApplicationInput'),
    newAddApplicationInput,
    AddApplicationInputResponse (AddApplicationInputResponse'),
    newAddApplicationInputResponse,

    -- ** AddApplicationReferenceDataSource
    AddApplicationReferenceDataSource (AddApplicationReferenceDataSource'),
    newAddApplicationReferenceDataSource,
    AddApplicationReferenceDataSourceResponse (AddApplicationReferenceDataSourceResponse'),
    newAddApplicationReferenceDataSourceResponse,

    -- ** DeleteApplicationReferenceDataSource
    DeleteApplicationReferenceDataSource (DeleteApplicationReferenceDataSource'),
    newDeleteApplicationReferenceDataSource,
    DeleteApplicationReferenceDataSourceResponse (DeleteApplicationReferenceDataSourceResponse'),
    newDeleteApplicationReferenceDataSourceResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- * Types

    -- ** ApplicationStatus
    ApplicationStatus (..),

    -- ** InputStartingPosition
    InputStartingPosition (..),

    -- ** RecordFormatType
    RecordFormatType (..),

    -- ** ApplicationDetail
    ApplicationDetail (ApplicationDetail'),
    newApplicationDetail,

    -- ** ApplicationSummary
    ApplicationSummary (ApplicationSummary'),
    newApplicationSummary,

    -- ** ApplicationUpdate
    ApplicationUpdate (ApplicationUpdate'),
    newApplicationUpdate,

    -- ** CSVMappingParameters
    CSVMappingParameters (CSVMappingParameters'),
    newCSVMappingParameters,

    -- ** CloudWatchLoggingOption
    CloudWatchLoggingOption (CloudWatchLoggingOption'),
    newCloudWatchLoggingOption,

    -- ** CloudWatchLoggingOptionDescription
    CloudWatchLoggingOptionDescription (CloudWatchLoggingOptionDescription'),
    newCloudWatchLoggingOptionDescription,

    -- ** CloudWatchLoggingOptionUpdate
    CloudWatchLoggingOptionUpdate (CloudWatchLoggingOptionUpdate'),
    newCloudWatchLoggingOptionUpdate,

    -- ** DestinationSchema
    DestinationSchema (DestinationSchema'),
    newDestinationSchema,

    -- ** Input
    Input (Input'),
    newInput,

    -- ** InputConfiguration
    InputConfiguration (InputConfiguration'),
    newInputConfiguration,

    -- ** InputDescription
    InputDescription (InputDescription'),
    newInputDescription,

    -- ** InputLambdaProcessor
    InputLambdaProcessor (InputLambdaProcessor'),
    newInputLambdaProcessor,

    -- ** InputLambdaProcessorDescription
    InputLambdaProcessorDescription (InputLambdaProcessorDescription'),
    newInputLambdaProcessorDescription,

    -- ** InputLambdaProcessorUpdate
    InputLambdaProcessorUpdate (InputLambdaProcessorUpdate'),
    newInputLambdaProcessorUpdate,

    -- ** InputParallelism
    InputParallelism (InputParallelism'),
    newInputParallelism,

    -- ** InputParallelismUpdate
    InputParallelismUpdate (InputParallelismUpdate'),
    newInputParallelismUpdate,

    -- ** InputProcessingConfiguration
    InputProcessingConfiguration (InputProcessingConfiguration'),
    newInputProcessingConfiguration,

    -- ** InputProcessingConfigurationDescription
    InputProcessingConfigurationDescription (InputProcessingConfigurationDescription'),
    newInputProcessingConfigurationDescription,

    -- ** InputProcessingConfigurationUpdate
    InputProcessingConfigurationUpdate (InputProcessingConfigurationUpdate'),
    newInputProcessingConfigurationUpdate,

    -- ** InputSchemaUpdate
    InputSchemaUpdate (InputSchemaUpdate'),
    newInputSchemaUpdate,

    -- ** InputStartingPositionConfiguration
    InputStartingPositionConfiguration (InputStartingPositionConfiguration'),
    newInputStartingPositionConfiguration,

    -- ** InputUpdate
    InputUpdate (InputUpdate'),
    newInputUpdate,

    -- ** JSONMappingParameters
    JSONMappingParameters (JSONMappingParameters'),
    newJSONMappingParameters,

    -- ** KinesisFirehoseInput
    KinesisFirehoseInput (KinesisFirehoseInput'),
    newKinesisFirehoseInput,

    -- ** KinesisFirehoseInputDescription
    KinesisFirehoseInputDescription (KinesisFirehoseInputDescription'),
    newKinesisFirehoseInputDescription,

    -- ** KinesisFirehoseInputUpdate
    KinesisFirehoseInputUpdate (KinesisFirehoseInputUpdate'),
    newKinesisFirehoseInputUpdate,

    -- ** KinesisFirehoseOutput
    KinesisFirehoseOutput (KinesisFirehoseOutput'),
    newKinesisFirehoseOutput,

    -- ** KinesisFirehoseOutputDescription
    KinesisFirehoseOutputDescription (KinesisFirehoseOutputDescription'),
    newKinesisFirehoseOutputDescription,

    -- ** KinesisFirehoseOutputUpdate
    KinesisFirehoseOutputUpdate (KinesisFirehoseOutputUpdate'),
    newKinesisFirehoseOutputUpdate,

    -- ** KinesisStreamsInput
    KinesisStreamsInput (KinesisStreamsInput'),
    newKinesisStreamsInput,

    -- ** KinesisStreamsInputDescription
    KinesisStreamsInputDescription (KinesisStreamsInputDescription'),
    newKinesisStreamsInputDescription,

    -- ** KinesisStreamsInputUpdate
    KinesisStreamsInputUpdate (KinesisStreamsInputUpdate'),
    newKinesisStreamsInputUpdate,

    -- ** KinesisStreamsOutput
    KinesisStreamsOutput (KinesisStreamsOutput'),
    newKinesisStreamsOutput,

    -- ** KinesisStreamsOutputDescription
    KinesisStreamsOutputDescription (KinesisStreamsOutputDescription'),
    newKinesisStreamsOutputDescription,

    -- ** KinesisStreamsOutputUpdate
    KinesisStreamsOutputUpdate (KinesisStreamsOutputUpdate'),
    newKinesisStreamsOutputUpdate,

    -- ** LambdaOutput
    LambdaOutput (LambdaOutput'),
    newLambdaOutput,

    -- ** LambdaOutputDescription
    LambdaOutputDescription (LambdaOutputDescription'),
    newLambdaOutputDescription,

    -- ** LambdaOutputUpdate
    LambdaOutputUpdate (LambdaOutputUpdate'),
    newLambdaOutputUpdate,

    -- ** MappingParameters
    MappingParameters (MappingParameters'),
    newMappingParameters,

    -- ** Output
    Output (Output'),
    newOutput,

    -- ** OutputDescription
    OutputDescription (OutputDescription'),
    newOutputDescription,

    -- ** OutputUpdate
    OutputUpdate (OutputUpdate'),
    newOutputUpdate,

    -- ** RecordColumn
    RecordColumn (RecordColumn'),
    newRecordColumn,

    -- ** RecordFormat
    RecordFormat (RecordFormat'),
    newRecordFormat,

    -- ** ReferenceDataSource
    ReferenceDataSource (ReferenceDataSource'),
    newReferenceDataSource,

    -- ** ReferenceDataSourceDescription
    ReferenceDataSourceDescription (ReferenceDataSourceDescription'),
    newReferenceDataSourceDescription,

    -- ** ReferenceDataSourceUpdate
    ReferenceDataSourceUpdate (ReferenceDataSourceUpdate'),
    newReferenceDataSourceUpdate,

    -- ** S3Configuration
    S3Configuration (S3Configuration'),
    newS3Configuration,

    -- ** S3ReferenceDataSource
    S3ReferenceDataSource (S3ReferenceDataSource'),
    newS3ReferenceDataSource,

    -- ** S3ReferenceDataSourceDescription
    S3ReferenceDataSourceDescription (S3ReferenceDataSourceDescription'),
    newS3ReferenceDataSourceDescription,

    -- ** S3ReferenceDataSourceUpdate
    S3ReferenceDataSourceUpdate (S3ReferenceDataSourceUpdate'),
    newS3ReferenceDataSourceUpdate,

    -- ** SourceSchema
    SourceSchema (SourceSchema'),
    newSourceSchema,

    -- ** Tag
    Tag (Tag'),
    newTag,
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
import Network.AWS.KinesisAnalytics.Lens
import Network.AWS.KinesisAnalytics.ListApplications
import Network.AWS.KinesisAnalytics.ListTagsForResource
import Network.AWS.KinesisAnalytics.StartApplication
import Network.AWS.KinesisAnalytics.StopApplication
import Network.AWS.KinesisAnalytics.TagResource
import Network.AWS.KinesisAnalytics.Types
import Network.AWS.KinesisAnalytics.UntagResource
import Network.AWS.KinesisAnalytics.UpdateApplication
import Network.AWS.KinesisAnalytics.Waiters

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

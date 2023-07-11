{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.KinesisAnalyticsV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-05-23@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Kinesis Data Analytics is a fully managed service that you can
-- use to process and analyze streaming data using Java, SQL, or Scala. The
-- service enables you to quickly author and run Java, SQL, or Scala code
-- against streaming sources to perform time series analytics, feed
-- real-time dashboards, and create real-time metrics.
module Amazonka.KinesisAnalyticsV2
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** CodeValidationException
    _CodeValidationException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** InvalidApplicationConfigurationException
    _InvalidApplicationConfigurationException,

    -- ** InvalidArgumentException
    _InvalidArgumentException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourceProvisionedThroughputExceededException
    _ResourceProvisionedThroughputExceededException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** UnableToDetectSchemaException
    _UnableToDetectSchemaException,

    -- ** UnsupportedOperationException
    _UnsupportedOperationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AddApplicationCloudWatchLoggingOption
    AddApplicationCloudWatchLoggingOption (AddApplicationCloudWatchLoggingOption'),
    newAddApplicationCloudWatchLoggingOption,
    AddApplicationCloudWatchLoggingOptionResponse (AddApplicationCloudWatchLoggingOptionResponse'),
    newAddApplicationCloudWatchLoggingOptionResponse,

    -- ** AddApplicationInput
    AddApplicationInput (AddApplicationInput'),
    newAddApplicationInput,
    AddApplicationInputResponse (AddApplicationInputResponse'),
    newAddApplicationInputResponse,

    -- ** AddApplicationInputProcessingConfiguration
    AddApplicationInputProcessingConfiguration (AddApplicationInputProcessingConfiguration'),
    newAddApplicationInputProcessingConfiguration,
    AddApplicationInputProcessingConfigurationResponse (AddApplicationInputProcessingConfigurationResponse'),
    newAddApplicationInputProcessingConfigurationResponse,

    -- ** AddApplicationOutput
    AddApplicationOutput (AddApplicationOutput'),
    newAddApplicationOutput,
    AddApplicationOutputResponse (AddApplicationOutputResponse'),
    newAddApplicationOutputResponse,

    -- ** AddApplicationReferenceDataSource
    AddApplicationReferenceDataSource (AddApplicationReferenceDataSource'),
    newAddApplicationReferenceDataSource,
    AddApplicationReferenceDataSourceResponse (AddApplicationReferenceDataSourceResponse'),
    newAddApplicationReferenceDataSourceResponse,

    -- ** AddApplicationVpcConfiguration
    AddApplicationVpcConfiguration (AddApplicationVpcConfiguration'),
    newAddApplicationVpcConfiguration,
    AddApplicationVpcConfigurationResponse (AddApplicationVpcConfigurationResponse'),
    newAddApplicationVpcConfigurationResponse,

    -- ** CreateApplication
    CreateApplication (CreateApplication'),
    newCreateApplication,
    CreateApplicationResponse (CreateApplicationResponse'),
    newCreateApplicationResponse,

    -- ** CreateApplicationPresignedUrl
    CreateApplicationPresignedUrl (CreateApplicationPresignedUrl'),
    newCreateApplicationPresignedUrl,
    CreateApplicationPresignedUrlResponse (CreateApplicationPresignedUrlResponse'),
    newCreateApplicationPresignedUrlResponse,

    -- ** CreateApplicationSnapshot
    CreateApplicationSnapshot (CreateApplicationSnapshot'),
    newCreateApplicationSnapshot,
    CreateApplicationSnapshotResponse (CreateApplicationSnapshotResponse'),
    newCreateApplicationSnapshotResponse,

    -- ** DeleteApplication
    DeleteApplication (DeleteApplication'),
    newDeleteApplication,
    DeleteApplicationResponse (DeleteApplicationResponse'),
    newDeleteApplicationResponse,

    -- ** DeleteApplicationCloudWatchLoggingOption
    DeleteApplicationCloudWatchLoggingOption (DeleteApplicationCloudWatchLoggingOption'),
    newDeleteApplicationCloudWatchLoggingOption,
    DeleteApplicationCloudWatchLoggingOptionResponse (DeleteApplicationCloudWatchLoggingOptionResponse'),
    newDeleteApplicationCloudWatchLoggingOptionResponse,

    -- ** DeleteApplicationInputProcessingConfiguration
    DeleteApplicationInputProcessingConfiguration (DeleteApplicationInputProcessingConfiguration'),
    newDeleteApplicationInputProcessingConfiguration,
    DeleteApplicationInputProcessingConfigurationResponse (DeleteApplicationInputProcessingConfigurationResponse'),
    newDeleteApplicationInputProcessingConfigurationResponse,

    -- ** DeleteApplicationOutput
    DeleteApplicationOutput (DeleteApplicationOutput'),
    newDeleteApplicationOutput,
    DeleteApplicationOutputResponse (DeleteApplicationOutputResponse'),
    newDeleteApplicationOutputResponse,

    -- ** DeleteApplicationReferenceDataSource
    DeleteApplicationReferenceDataSource (DeleteApplicationReferenceDataSource'),
    newDeleteApplicationReferenceDataSource,
    DeleteApplicationReferenceDataSourceResponse (DeleteApplicationReferenceDataSourceResponse'),
    newDeleteApplicationReferenceDataSourceResponse,

    -- ** DeleteApplicationSnapshot
    DeleteApplicationSnapshot (DeleteApplicationSnapshot'),
    newDeleteApplicationSnapshot,
    DeleteApplicationSnapshotResponse (DeleteApplicationSnapshotResponse'),
    newDeleteApplicationSnapshotResponse,

    -- ** DeleteApplicationVpcConfiguration
    DeleteApplicationVpcConfiguration (DeleteApplicationVpcConfiguration'),
    newDeleteApplicationVpcConfiguration,
    DeleteApplicationVpcConfigurationResponse (DeleteApplicationVpcConfigurationResponse'),
    newDeleteApplicationVpcConfigurationResponse,

    -- ** DescribeApplication
    DescribeApplication (DescribeApplication'),
    newDescribeApplication,
    DescribeApplicationResponse (DescribeApplicationResponse'),
    newDescribeApplicationResponse,

    -- ** DescribeApplicationSnapshot
    DescribeApplicationSnapshot (DescribeApplicationSnapshot'),
    newDescribeApplicationSnapshot,
    DescribeApplicationSnapshotResponse (DescribeApplicationSnapshotResponse'),
    newDescribeApplicationSnapshotResponse,

    -- ** DescribeApplicationVersion
    DescribeApplicationVersion (DescribeApplicationVersion'),
    newDescribeApplicationVersion,
    DescribeApplicationVersionResponse (DescribeApplicationVersionResponse'),
    newDescribeApplicationVersionResponse,

    -- ** DiscoverInputSchema
    DiscoverInputSchema (DiscoverInputSchema'),
    newDiscoverInputSchema,
    DiscoverInputSchemaResponse (DiscoverInputSchemaResponse'),
    newDiscoverInputSchemaResponse,

    -- ** ListApplicationSnapshots (Paginated)
    ListApplicationSnapshots (ListApplicationSnapshots'),
    newListApplicationSnapshots,
    ListApplicationSnapshotsResponse (ListApplicationSnapshotsResponse'),
    newListApplicationSnapshotsResponse,

    -- ** ListApplicationVersions
    ListApplicationVersions (ListApplicationVersions'),
    newListApplicationVersions,
    ListApplicationVersionsResponse (ListApplicationVersionsResponse'),
    newListApplicationVersionsResponse,

    -- ** ListApplications (Paginated)
    ListApplications (ListApplications'),
    newListApplications,
    ListApplicationsResponse (ListApplicationsResponse'),
    newListApplicationsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** RollbackApplication
    RollbackApplication (RollbackApplication'),
    newRollbackApplication,
    RollbackApplicationResponse (RollbackApplicationResponse'),
    newRollbackApplicationResponse,

    -- ** StartApplication
    StartApplication (StartApplication'),
    newStartApplication,
    StartApplicationResponse (StartApplicationResponse'),
    newStartApplicationResponse,

    -- ** StopApplication
    StopApplication (StopApplication'),
    newStopApplication,
    StopApplicationResponse (StopApplicationResponse'),
    newStopApplicationResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateApplication
    UpdateApplication (UpdateApplication'),
    newUpdateApplication,
    UpdateApplicationResponse (UpdateApplicationResponse'),
    newUpdateApplicationResponse,

    -- ** UpdateApplicationMaintenanceConfiguration
    UpdateApplicationMaintenanceConfiguration (UpdateApplicationMaintenanceConfiguration'),
    newUpdateApplicationMaintenanceConfiguration,
    UpdateApplicationMaintenanceConfigurationResponse (UpdateApplicationMaintenanceConfigurationResponse'),
    newUpdateApplicationMaintenanceConfigurationResponse,

    -- * Types

    -- ** ApplicationMode
    ApplicationMode (..),

    -- ** ApplicationRestoreType
    ApplicationRestoreType (..),

    -- ** ApplicationStatus
    ApplicationStatus (..),

    -- ** ArtifactType
    ArtifactType (..),

    -- ** CodeContentType
    CodeContentType (..),

    -- ** ConfigurationType
    ConfigurationType (..),

    -- ** InputStartingPosition
    InputStartingPosition (..),

    -- ** LogLevel
    LogLevel (..),

    -- ** MetricsLevel
    MetricsLevel (..),

    -- ** RecordFormatType
    RecordFormatType (..),

    -- ** RuntimeEnvironment
    RuntimeEnvironment (..),

    -- ** SnapshotStatus
    SnapshotStatus (..),

    -- ** UrlType
    UrlType (..),

    -- ** ApplicationCodeConfiguration
    ApplicationCodeConfiguration (ApplicationCodeConfiguration'),
    newApplicationCodeConfiguration,

    -- ** ApplicationCodeConfigurationDescription
    ApplicationCodeConfigurationDescription (ApplicationCodeConfigurationDescription'),
    newApplicationCodeConfigurationDescription,

    -- ** ApplicationCodeConfigurationUpdate
    ApplicationCodeConfigurationUpdate (ApplicationCodeConfigurationUpdate'),
    newApplicationCodeConfigurationUpdate,

    -- ** ApplicationConfiguration
    ApplicationConfiguration (ApplicationConfiguration'),
    newApplicationConfiguration,

    -- ** ApplicationConfigurationDescription
    ApplicationConfigurationDescription (ApplicationConfigurationDescription'),
    newApplicationConfigurationDescription,

    -- ** ApplicationConfigurationUpdate
    ApplicationConfigurationUpdate (ApplicationConfigurationUpdate'),
    newApplicationConfigurationUpdate,

    -- ** ApplicationDetail
    ApplicationDetail (ApplicationDetail'),
    newApplicationDetail,

    -- ** ApplicationMaintenanceConfigurationDescription
    ApplicationMaintenanceConfigurationDescription (ApplicationMaintenanceConfigurationDescription'),
    newApplicationMaintenanceConfigurationDescription,

    -- ** ApplicationMaintenanceConfigurationUpdate
    ApplicationMaintenanceConfigurationUpdate (ApplicationMaintenanceConfigurationUpdate'),
    newApplicationMaintenanceConfigurationUpdate,

    -- ** ApplicationRestoreConfiguration
    ApplicationRestoreConfiguration (ApplicationRestoreConfiguration'),
    newApplicationRestoreConfiguration,

    -- ** ApplicationSnapshotConfiguration
    ApplicationSnapshotConfiguration (ApplicationSnapshotConfiguration'),
    newApplicationSnapshotConfiguration,

    -- ** ApplicationSnapshotConfigurationDescription
    ApplicationSnapshotConfigurationDescription (ApplicationSnapshotConfigurationDescription'),
    newApplicationSnapshotConfigurationDescription,

    -- ** ApplicationSnapshotConfigurationUpdate
    ApplicationSnapshotConfigurationUpdate (ApplicationSnapshotConfigurationUpdate'),
    newApplicationSnapshotConfigurationUpdate,

    -- ** ApplicationSummary
    ApplicationSummary (ApplicationSummary'),
    newApplicationSummary,

    -- ** ApplicationVersionSummary
    ApplicationVersionSummary (ApplicationVersionSummary'),
    newApplicationVersionSummary,

    -- ** CSVMappingParameters
    CSVMappingParameters (CSVMappingParameters'),
    newCSVMappingParameters,

    -- ** CatalogConfiguration
    CatalogConfiguration (CatalogConfiguration'),
    newCatalogConfiguration,

    -- ** CatalogConfigurationDescription
    CatalogConfigurationDescription (CatalogConfigurationDescription'),
    newCatalogConfigurationDescription,

    -- ** CatalogConfigurationUpdate
    CatalogConfigurationUpdate (CatalogConfigurationUpdate'),
    newCatalogConfigurationUpdate,

    -- ** CheckpointConfiguration
    CheckpointConfiguration (CheckpointConfiguration'),
    newCheckpointConfiguration,

    -- ** CheckpointConfigurationDescription
    CheckpointConfigurationDescription (CheckpointConfigurationDescription'),
    newCheckpointConfigurationDescription,

    -- ** CheckpointConfigurationUpdate
    CheckpointConfigurationUpdate (CheckpointConfigurationUpdate'),
    newCheckpointConfigurationUpdate,

    -- ** CloudWatchLoggingOption
    CloudWatchLoggingOption (CloudWatchLoggingOption'),
    newCloudWatchLoggingOption,

    -- ** CloudWatchLoggingOptionDescription
    CloudWatchLoggingOptionDescription (CloudWatchLoggingOptionDescription'),
    newCloudWatchLoggingOptionDescription,

    -- ** CloudWatchLoggingOptionUpdate
    CloudWatchLoggingOptionUpdate (CloudWatchLoggingOptionUpdate'),
    newCloudWatchLoggingOptionUpdate,

    -- ** CodeContent
    CodeContent (CodeContent'),
    newCodeContent,

    -- ** CodeContentDescription
    CodeContentDescription (CodeContentDescription'),
    newCodeContentDescription,

    -- ** CodeContentUpdate
    CodeContentUpdate (CodeContentUpdate'),
    newCodeContentUpdate,

    -- ** CustomArtifactConfiguration
    CustomArtifactConfiguration (CustomArtifactConfiguration'),
    newCustomArtifactConfiguration,

    -- ** CustomArtifactConfigurationDescription
    CustomArtifactConfigurationDescription (CustomArtifactConfigurationDescription'),
    newCustomArtifactConfigurationDescription,

    -- ** DeployAsApplicationConfiguration
    DeployAsApplicationConfiguration (DeployAsApplicationConfiguration'),
    newDeployAsApplicationConfiguration,

    -- ** DeployAsApplicationConfigurationDescription
    DeployAsApplicationConfigurationDescription (DeployAsApplicationConfigurationDescription'),
    newDeployAsApplicationConfigurationDescription,

    -- ** DeployAsApplicationConfigurationUpdate
    DeployAsApplicationConfigurationUpdate (DeployAsApplicationConfigurationUpdate'),
    newDeployAsApplicationConfigurationUpdate,

    -- ** DestinationSchema
    DestinationSchema (DestinationSchema'),
    newDestinationSchema,

    -- ** EnvironmentProperties
    EnvironmentProperties (EnvironmentProperties'),
    newEnvironmentProperties,

    -- ** EnvironmentPropertyDescriptions
    EnvironmentPropertyDescriptions (EnvironmentPropertyDescriptions'),
    newEnvironmentPropertyDescriptions,

    -- ** EnvironmentPropertyUpdates
    EnvironmentPropertyUpdates (EnvironmentPropertyUpdates'),
    newEnvironmentPropertyUpdates,

    -- ** FlinkApplicationConfiguration
    FlinkApplicationConfiguration (FlinkApplicationConfiguration'),
    newFlinkApplicationConfiguration,

    -- ** FlinkApplicationConfigurationDescription
    FlinkApplicationConfigurationDescription (FlinkApplicationConfigurationDescription'),
    newFlinkApplicationConfigurationDescription,

    -- ** FlinkApplicationConfigurationUpdate
    FlinkApplicationConfigurationUpdate (FlinkApplicationConfigurationUpdate'),
    newFlinkApplicationConfigurationUpdate,

    -- ** FlinkRunConfiguration
    FlinkRunConfiguration (FlinkRunConfiguration'),
    newFlinkRunConfiguration,

    -- ** GlueDataCatalogConfiguration
    GlueDataCatalogConfiguration (GlueDataCatalogConfiguration'),
    newGlueDataCatalogConfiguration,

    -- ** GlueDataCatalogConfigurationDescription
    GlueDataCatalogConfigurationDescription (GlueDataCatalogConfigurationDescription'),
    newGlueDataCatalogConfigurationDescription,

    -- ** GlueDataCatalogConfigurationUpdate
    GlueDataCatalogConfigurationUpdate (GlueDataCatalogConfigurationUpdate'),
    newGlueDataCatalogConfigurationUpdate,

    -- ** Input
    Input (Input'),
    newInput,

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

    -- ** MavenReference
    MavenReference (MavenReference'),
    newMavenReference,

    -- ** MonitoringConfiguration
    MonitoringConfiguration (MonitoringConfiguration'),
    newMonitoringConfiguration,

    -- ** MonitoringConfigurationDescription
    MonitoringConfigurationDescription (MonitoringConfigurationDescription'),
    newMonitoringConfigurationDescription,

    -- ** MonitoringConfigurationUpdate
    MonitoringConfigurationUpdate (MonitoringConfigurationUpdate'),
    newMonitoringConfigurationUpdate,

    -- ** Output
    Output (Output'),
    newOutput,

    -- ** OutputDescription
    OutputDescription (OutputDescription'),
    newOutputDescription,

    -- ** OutputUpdate
    OutputUpdate (OutputUpdate'),
    newOutputUpdate,

    -- ** ParallelismConfiguration
    ParallelismConfiguration (ParallelismConfiguration'),
    newParallelismConfiguration,

    -- ** ParallelismConfigurationDescription
    ParallelismConfigurationDescription (ParallelismConfigurationDescription'),
    newParallelismConfigurationDescription,

    -- ** ParallelismConfigurationUpdate
    ParallelismConfigurationUpdate (ParallelismConfigurationUpdate'),
    newParallelismConfigurationUpdate,

    -- ** PropertyGroup
    PropertyGroup (PropertyGroup'),
    newPropertyGroup,

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

    -- ** RunConfiguration
    RunConfiguration (RunConfiguration'),
    newRunConfiguration,

    -- ** RunConfigurationDescription
    RunConfigurationDescription (RunConfigurationDescription'),
    newRunConfigurationDescription,

    -- ** RunConfigurationUpdate
    RunConfigurationUpdate (RunConfigurationUpdate'),
    newRunConfigurationUpdate,

    -- ** S3ApplicationCodeLocationDescription
    S3ApplicationCodeLocationDescription (S3ApplicationCodeLocationDescription'),
    newS3ApplicationCodeLocationDescription,

    -- ** S3Configuration
    S3Configuration (S3Configuration'),
    newS3Configuration,

    -- ** S3ContentBaseLocation
    S3ContentBaseLocation (S3ContentBaseLocation'),
    newS3ContentBaseLocation,

    -- ** S3ContentBaseLocationDescription
    S3ContentBaseLocationDescription (S3ContentBaseLocationDescription'),
    newS3ContentBaseLocationDescription,

    -- ** S3ContentBaseLocationUpdate
    S3ContentBaseLocationUpdate (S3ContentBaseLocationUpdate'),
    newS3ContentBaseLocationUpdate,

    -- ** S3ContentLocation
    S3ContentLocation (S3ContentLocation'),
    newS3ContentLocation,

    -- ** S3ContentLocationUpdate
    S3ContentLocationUpdate (S3ContentLocationUpdate'),
    newS3ContentLocationUpdate,

    -- ** S3ReferenceDataSource
    S3ReferenceDataSource (S3ReferenceDataSource'),
    newS3ReferenceDataSource,

    -- ** S3ReferenceDataSourceDescription
    S3ReferenceDataSourceDescription (S3ReferenceDataSourceDescription'),
    newS3ReferenceDataSourceDescription,

    -- ** S3ReferenceDataSourceUpdate
    S3ReferenceDataSourceUpdate (S3ReferenceDataSourceUpdate'),
    newS3ReferenceDataSourceUpdate,

    -- ** SnapshotDetails
    SnapshotDetails (SnapshotDetails'),
    newSnapshotDetails,

    -- ** SourceSchema
    SourceSchema (SourceSchema'),
    newSourceSchema,

    -- ** SqlApplicationConfiguration
    SqlApplicationConfiguration (SqlApplicationConfiguration'),
    newSqlApplicationConfiguration,

    -- ** SqlApplicationConfigurationDescription
    SqlApplicationConfigurationDescription (SqlApplicationConfigurationDescription'),
    newSqlApplicationConfigurationDescription,

    -- ** SqlApplicationConfigurationUpdate
    SqlApplicationConfigurationUpdate (SqlApplicationConfigurationUpdate'),
    newSqlApplicationConfigurationUpdate,

    -- ** SqlRunConfiguration
    SqlRunConfiguration (SqlRunConfiguration'),
    newSqlRunConfiguration,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** VpcConfiguration
    VpcConfiguration (VpcConfiguration'),
    newVpcConfiguration,

    -- ** VpcConfigurationDescription
    VpcConfigurationDescription (VpcConfigurationDescription'),
    newVpcConfigurationDescription,

    -- ** VpcConfigurationUpdate
    VpcConfigurationUpdate (VpcConfigurationUpdate'),
    newVpcConfigurationUpdate,

    -- ** ZeppelinApplicationConfiguration
    ZeppelinApplicationConfiguration (ZeppelinApplicationConfiguration'),
    newZeppelinApplicationConfiguration,

    -- ** ZeppelinApplicationConfigurationDescription
    ZeppelinApplicationConfigurationDescription (ZeppelinApplicationConfigurationDescription'),
    newZeppelinApplicationConfigurationDescription,

    -- ** ZeppelinApplicationConfigurationUpdate
    ZeppelinApplicationConfigurationUpdate (ZeppelinApplicationConfigurationUpdate'),
    newZeppelinApplicationConfigurationUpdate,

    -- ** ZeppelinMonitoringConfiguration
    ZeppelinMonitoringConfiguration (ZeppelinMonitoringConfiguration'),
    newZeppelinMonitoringConfiguration,

    -- ** ZeppelinMonitoringConfigurationDescription
    ZeppelinMonitoringConfigurationDescription (ZeppelinMonitoringConfigurationDescription'),
    newZeppelinMonitoringConfigurationDescription,

    -- ** ZeppelinMonitoringConfigurationUpdate
    ZeppelinMonitoringConfigurationUpdate (ZeppelinMonitoringConfigurationUpdate'),
    newZeppelinMonitoringConfigurationUpdate,
  )
where

import Amazonka.KinesisAnalyticsV2.AddApplicationCloudWatchLoggingOption
import Amazonka.KinesisAnalyticsV2.AddApplicationInput
import Amazonka.KinesisAnalyticsV2.AddApplicationInputProcessingConfiguration
import Amazonka.KinesisAnalyticsV2.AddApplicationOutput
import Amazonka.KinesisAnalyticsV2.AddApplicationReferenceDataSource
import Amazonka.KinesisAnalyticsV2.AddApplicationVpcConfiguration
import Amazonka.KinesisAnalyticsV2.CreateApplication
import Amazonka.KinesisAnalyticsV2.CreateApplicationPresignedUrl
import Amazonka.KinesisAnalyticsV2.CreateApplicationSnapshot
import Amazonka.KinesisAnalyticsV2.DeleteApplication
import Amazonka.KinesisAnalyticsV2.DeleteApplicationCloudWatchLoggingOption
import Amazonka.KinesisAnalyticsV2.DeleteApplicationInputProcessingConfiguration
import Amazonka.KinesisAnalyticsV2.DeleteApplicationOutput
import Amazonka.KinesisAnalyticsV2.DeleteApplicationReferenceDataSource
import Amazonka.KinesisAnalyticsV2.DeleteApplicationSnapshot
import Amazonka.KinesisAnalyticsV2.DeleteApplicationVpcConfiguration
import Amazonka.KinesisAnalyticsV2.DescribeApplication
import Amazonka.KinesisAnalyticsV2.DescribeApplicationSnapshot
import Amazonka.KinesisAnalyticsV2.DescribeApplicationVersion
import Amazonka.KinesisAnalyticsV2.DiscoverInputSchema
import Amazonka.KinesisAnalyticsV2.Lens
import Amazonka.KinesisAnalyticsV2.ListApplicationSnapshots
import Amazonka.KinesisAnalyticsV2.ListApplicationVersions
import Amazonka.KinesisAnalyticsV2.ListApplications
import Amazonka.KinesisAnalyticsV2.ListTagsForResource
import Amazonka.KinesisAnalyticsV2.RollbackApplication
import Amazonka.KinesisAnalyticsV2.StartApplication
import Amazonka.KinesisAnalyticsV2.StopApplication
import Amazonka.KinesisAnalyticsV2.TagResource
import Amazonka.KinesisAnalyticsV2.Types
import Amazonka.KinesisAnalyticsV2.UntagResource
import Amazonka.KinesisAnalyticsV2.UpdateApplication
import Amazonka.KinesisAnalyticsV2.UpdateApplicationMaintenanceConfiguration
import Amazonka.KinesisAnalyticsV2.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'KinesisAnalyticsV2'.

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

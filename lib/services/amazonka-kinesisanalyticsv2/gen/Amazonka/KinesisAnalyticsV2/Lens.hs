{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KinesisAnalyticsV2.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Lens
  ( -- * Operations

    -- ** AddApplicationCloudWatchLoggingOption
    addApplicationCloudWatchLoggingOption_conditionalToken,
    addApplicationCloudWatchLoggingOption_currentApplicationVersionId,
    addApplicationCloudWatchLoggingOption_applicationName,
    addApplicationCloudWatchLoggingOption_cloudWatchLoggingOption,
    addApplicationCloudWatchLoggingOptionResponse_applicationARN,
    addApplicationCloudWatchLoggingOptionResponse_applicationVersionId,
    addApplicationCloudWatchLoggingOptionResponse_cloudWatchLoggingOptionDescriptions,
    addApplicationCloudWatchLoggingOptionResponse_httpStatus,

    -- ** AddApplicationInput
    addApplicationInput_applicationName,
    addApplicationInput_currentApplicationVersionId,
    addApplicationInput_input,
    addApplicationInputResponse_applicationARN,
    addApplicationInputResponse_applicationVersionId,
    addApplicationInputResponse_inputDescriptions,
    addApplicationInputResponse_httpStatus,

    -- ** AddApplicationInputProcessingConfiguration
    addApplicationInputProcessingConfiguration_applicationName,
    addApplicationInputProcessingConfiguration_currentApplicationVersionId,
    addApplicationInputProcessingConfiguration_inputId,
    addApplicationInputProcessingConfiguration_inputProcessingConfiguration,
    addApplicationInputProcessingConfigurationResponse_applicationARN,
    addApplicationInputProcessingConfigurationResponse_applicationVersionId,
    addApplicationInputProcessingConfigurationResponse_inputId,
    addApplicationInputProcessingConfigurationResponse_inputProcessingConfigurationDescription,
    addApplicationInputProcessingConfigurationResponse_httpStatus,

    -- ** AddApplicationOutput
    addApplicationOutput_applicationName,
    addApplicationOutput_currentApplicationVersionId,
    addApplicationOutput_output,
    addApplicationOutputResponse_applicationARN,
    addApplicationOutputResponse_applicationVersionId,
    addApplicationOutputResponse_outputDescriptions,
    addApplicationOutputResponse_httpStatus,

    -- ** AddApplicationReferenceDataSource
    addApplicationReferenceDataSource_applicationName,
    addApplicationReferenceDataSource_currentApplicationVersionId,
    addApplicationReferenceDataSource_referenceDataSource,
    addApplicationReferenceDataSourceResponse_applicationARN,
    addApplicationReferenceDataSourceResponse_applicationVersionId,
    addApplicationReferenceDataSourceResponse_referenceDataSourceDescriptions,
    addApplicationReferenceDataSourceResponse_httpStatus,

    -- ** AddApplicationVpcConfiguration
    addApplicationVpcConfiguration_conditionalToken,
    addApplicationVpcConfiguration_currentApplicationVersionId,
    addApplicationVpcConfiguration_applicationName,
    addApplicationVpcConfiguration_vpcConfiguration,
    addApplicationVpcConfigurationResponse_applicationARN,
    addApplicationVpcConfigurationResponse_applicationVersionId,
    addApplicationVpcConfigurationResponse_vpcConfigurationDescription,
    addApplicationVpcConfigurationResponse_httpStatus,

    -- ** CreateApplication
    createApplication_applicationConfiguration,
    createApplication_applicationDescription,
    createApplication_applicationMode,
    createApplication_cloudWatchLoggingOptions,
    createApplication_tags,
    createApplication_applicationName,
    createApplication_runtimeEnvironment,
    createApplication_serviceExecutionRole,
    createApplicationResponse_httpStatus,
    createApplicationResponse_applicationDetail,

    -- ** CreateApplicationPresignedUrl
    createApplicationPresignedUrl_sessionExpirationDurationInSeconds,
    createApplicationPresignedUrl_applicationName,
    createApplicationPresignedUrl_urlType,
    createApplicationPresignedUrlResponse_authorizedUrl,
    createApplicationPresignedUrlResponse_httpStatus,

    -- ** CreateApplicationSnapshot
    createApplicationSnapshot_applicationName,
    createApplicationSnapshot_snapshotName,
    createApplicationSnapshotResponse_httpStatus,

    -- ** DeleteApplication
    deleteApplication_applicationName,
    deleteApplication_createTimestamp,
    deleteApplicationResponse_httpStatus,

    -- ** DeleteApplicationCloudWatchLoggingOption
    deleteApplicationCloudWatchLoggingOption_conditionalToken,
    deleteApplicationCloudWatchLoggingOption_currentApplicationVersionId,
    deleteApplicationCloudWatchLoggingOption_applicationName,
    deleteApplicationCloudWatchLoggingOption_cloudWatchLoggingOptionId,
    deleteApplicationCloudWatchLoggingOptionResponse_applicationARN,
    deleteApplicationCloudWatchLoggingOptionResponse_applicationVersionId,
    deleteApplicationCloudWatchLoggingOptionResponse_cloudWatchLoggingOptionDescriptions,
    deleteApplicationCloudWatchLoggingOptionResponse_httpStatus,

    -- ** DeleteApplicationInputProcessingConfiguration
    deleteApplicationInputProcessingConfiguration_applicationName,
    deleteApplicationInputProcessingConfiguration_currentApplicationVersionId,
    deleteApplicationInputProcessingConfiguration_inputId,
    deleteApplicationInputProcessingConfigurationResponse_applicationARN,
    deleteApplicationInputProcessingConfigurationResponse_applicationVersionId,
    deleteApplicationInputProcessingConfigurationResponse_httpStatus,

    -- ** DeleteApplicationOutput
    deleteApplicationOutput_applicationName,
    deleteApplicationOutput_currentApplicationVersionId,
    deleteApplicationOutput_outputId,
    deleteApplicationOutputResponse_applicationARN,
    deleteApplicationOutputResponse_applicationVersionId,
    deleteApplicationOutputResponse_httpStatus,

    -- ** DeleteApplicationReferenceDataSource
    deleteApplicationReferenceDataSource_applicationName,
    deleteApplicationReferenceDataSource_currentApplicationVersionId,
    deleteApplicationReferenceDataSource_referenceId,
    deleteApplicationReferenceDataSourceResponse_applicationARN,
    deleteApplicationReferenceDataSourceResponse_applicationVersionId,
    deleteApplicationReferenceDataSourceResponse_httpStatus,

    -- ** DeleteApplicationSnapshot
    deleteApplicationSnapshot_applicationName,
    deleteApplicationSnapshot_snapshotName,
    deleteApplicationSnapshot_snapshotCreationTimestamp,
    deleteApplicationSnapshotResponse_httpStatus,

    -- ** DeleteApplicationVpcConfiguration
    deleteApplicationVpcConfiguration_conditionalToken,
    deleteApplicationVpcConfiguration_currentApplicationVersionId,
    deleteApplicationVpcConfiguration_applicationName,
    deleteApplicationVpcConfiguration_vpcConfigurationId,
    deleteApplicationVpcConfigurationResponse_applicationARN,
    deleteApplicationVpcConfigurationResponse_applicationVersionId,
    deleteApplicationVpcConfigurationResponse_httpStatus,

    -- ** DescribeApplication
    describeApplication_includeAdditionalDetails,
    describeApplication_applicationName,
    describeApplicationResponse_httpStatus,
    describeApplicationResponse_applicationDetail,

    -- ** DescribeApplicationSnapshot
    describeApplicationSnapshot_applicationName,
    describeApplicationSnapshot_snapshotName,
    describeApplicationSnapshotResponse_httpStatus,
    describeApplicationSnapshotResponse_snapshotDetails,

    -- ** DescribeApplicationVersion
    describeApplicationVersion_applicationName,
    describeApplicationVersion_applicationVersionId,
    describeApplicationVersionResponse_applicationVersionDetail,
    describeApplicationVersionResponse_httpStatus,

    -- ** DiscoverInputSchema
    discoverInputSchema_inputProcessingConfiguration,
    discoverInputSchema_inputStartingPositionConfiguration,
    discoverInputSchema_resourceARN,
    discoverInputSchema_s3Configuration,
    discoverInputSchema_serviceExecutionRole,
    discoverInputSchemaResponse_inputSchema,
    discoverInputSchemaResponse_parsedInputRecords,
    discoverInputSchemaResponse_processedInputRecords,
    discoverInputSchemaResponse_rawInputRecords,
    discoverInputSchemaResponse_httpStatus,

    -- ** ListApplicationSnapshots
    listApplicationSnapshots_limit,
    listApplicationSnapshots_nextToken,
    listApplicationSnapshots_applicationName,
    listApplicationSnapshotsResponse_nextToken,
    listApplicationSnapshotsResponse_snapshotSummaries,
    listApplicationSnapshotsResponse_httpStatus,

    -- ** ListApplicationVersions
    listApplicationVersions_limit,
    listApplicationVersions_nextToken,
    listApplicationVersions_applicationName,
    listApplicationVersionsResponse_applicationVersionSummaries,
    listApplicationVersionsResponse_nextToken,
    listApplicationVersionsResponse_httpStatus,

    -- ** ListApplications
    listApplications_limit,
    listApplications_nextToken,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_httpStatus,
    listApplicationsResponse_applicationSummaries,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** RollbackApplication
    rollbackApplication_applicationName,
    rollbackApplication_currentApplicationVersionId,
    rollbackApplicationResponse_httpStatus,
    rollbackApplicationResponse_applicationDetail,

    -- ** StartApplication
    startApplication_runConfiguration,
    startApplication_applicationName,
    startApplicationResponse_httpStatus,

    -- ** StopApplication
    stopApplication_force,
    stopApplication_applicationName,
    stopApplicationResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateApplication
    updateApplication_applicationConfigurationUpdate,
    updateApplication_cloudWatchLoggingOptionUpdates,
    updateApplication_conditionalToken,
    updateApplication_currentApplicationVersionId,
    updateApplication_runConfigurationUpdate,
    updateApplication_serviceExecutionRoleUpdate,
    updateApplication_applicationName,
    updateApplicationResponse_httpStatus,
    updateApplicationResponse_applicationDetail,

    -- ** UpdateApplicationMaintenanceConfiguration
    updateApplicationMaintenanceConfiguration_applicationName,
    updateApplicationMaintenanceConfiguration_applicationMaintenanceConfigurationUpdate,
    updateApplicationMaintenanceConfigurationResponse_applicationARN,
    updateApplicationMaintenanceConfigurationResponse_applicationMaintenanceConfigurationDescription,
    updateApplicationMaintenanceConfigurationResponse_httpStatus,

    -- * Types

    -- ** ApplicationCodeConfiguration
    applicationCodeConfiguration_codeContent,
    applicationCodeConfiguration_codeContentType,

    -- ** ApplicationCodeConfigurationDescription
    applicationCodeConfigurationDescription_codeContentDescription,
    applicationCodeConfigurationDescription_codeContentType,

    -- ** ApplicationCodeConfigurationUpdate
    applicationCodeConfigurationUpdate_codeContentTypeUpdate,
    applicationCodeConfigurationUpdate_codeContentUpdate,

    -- ** ApplicationConfiguration
    applicationConfiguration_applicationCodeConfiguration,
    applicationConfiguration_applicationSnapshotConfiguration,
    applicationConfiguration_environmentProperties,
    applicationConfiguration_flinkApplicationConfiguration,
    applicationConfiguration_sqlApplicationConfiguration,
    applicationConfiguration_vpcConfigurations,
    applicationConfiguration_zeppelinApplicationConfiguration,

    -- ** ApplicationConfigurationDescription
    applicationConfigurationDescription_applicationCodeConfigurationDescription,
    applicationConfigurationDescription_applicationSnapshotConfigurationDescription,
    applicationConfigurationDescription_environmentPropertyDescriptions,
    applicationConfigurationDescription_flinkApplicationConfigurationDescription,
    applicationConfigurationDescription_runConfigurationDescription,
    applicationConfigurationDescription_sqlApplicationConfigurationDescription,
    applicationConfigurationDescription_vpcConfigurationDescriptions,
    applicationConfigurationDescription_zeppelinApplicationConfigurationDescription,

    -- ** ApplicationConfigurationUpdate
    applicationConfigurationUpdate_applicationCodeConfigurationUpdate,
    applicationConfigurationUpdate_applicationSnapshotConfigurationUpdate,
    applicationConfigurationUpdate_environmentPropertyUpdates,
    applicationConfigurationUpdate_flinkApplicationConfigurationUpdate,
    applicationConfigurationUpdate_sqlApplicationConfigurationUpdate,
    applicationConfigurationUpdate_vpcConfigurationUpdates,
    applicationConfigurationUpdate_zeppelinApplicationConfigurationUpdate,

    -- ** ApplicationDetail
    applicationDetail_applicationConfigurationDescription,
    applicationDetail_applicationDescription,
    applicationDetail_applicationMaintenanceConfigurationDescription,
    applicationDetail_applicationMode,
    applicationDetail_applicationVersionRolledBackFrom,
    applicationDetail_applicationVersionRolledBackTo,
    applicationDetail_applicationVersionUpdatedFrom,
    applicationDetail_cloudWatchLoggingOptionDescriptions,
    applicationDetail_conditionalToken,
    applicationDetail_createTimestamp,
    applicationDetail_lastUpdateTimestamp,
    applicationDetail_serviceExecutionRole,
    applicationDetail_applicationARN,
    applicationDetail_applicationName,
    applicationDetail_runtimeEnvironment,
    applicationDetail_applicationStatus,
    applicationDetail_applicationVersionId,

    -- ** ApplicationMaintenanceConfigurationDescription
    applicationMaintenanceConfigurationDescription_applicationMaintenanceWindowStartTime,
    applicationMaintenanceConfigurationDescription_applicationMaintenanceWindowEndTime,

    -- ** ApplicationMaintenanceConfigurationUpdate
    applicationMaintenanceConfigurationUpdate_applicationMaintenanceWindowStartTimeUpdate,

    -- ** ApplicationRestoreConfiguration
    applicationRestoreConfiguration_snapshotName,
    applicationRestoreConfiguration_applicationRestoreType,

    -- ** ApplicationSnapshotConfiguration
    applicationSnapshotConfiguration_snapshotsEnabled,

    -- ** ApplicationSnapshotConfigurationDescription
    applicationSnapshotConfigurationDescription_snapshotsEnabled,

    -- ** ApplicationSnapshotConfigurationUpdate
    applicationSnapshotConfigurationUpdate_snapshotsEnabledUpdate,

    -- ** ApplicationSummary
    applicationSummary_applicationMode,
    applicationSummary_applicationName,
    applicationSummary_applicationARN,
    applicationSummary_applicationStatus,
    applicationSummary_applicationVersionId,
    applicationSummary_runtimeEnvironment,

    -- ** ApplicationVersionSummary
    applicationVersionSummary_applicationVersionId,
    applicationVersionSummary_applicationStatus,

    -- ** CSVMappingParameters
    cSVMappingParameters_recordRowDelimiter,
    cSVMappingParameters_recordColumnDelimiter,

    -- ** CatalogConfiguration
    catalogConfiguration_glueDataCatalogConfiguration,

    -- ** CatalogConfigurationDescription
    catalogConfigurationDescription_glueDataCatalogConfigurationDescription,

    -- ** CatalogConfigurationUpdate
    catalogConfigurationUpdate_glueDataCatalogConfigurationUpdate,

    -- ** CheckpointConfiguration
    checkpointConfiguration_checkpointInterval,
    checkpointConfiguration_checkpointingEnabled,
    checkpointConfiguration_minPauseBetweenCheckpoints,
    checkpointConfiguration_configurationType,

    -- ** CheckpointConfigurationDescription
    checkpointConfigurationDescription_checkpointInterval,
    checkpointConfigurationDescription_checkpointingEnabled,
    checkpointConfigurationDescription_configurationType,
    checkpointConfigurationDescription_minPauseBetweenCheckpoints,

    -- ** CheckpointConfigurationUpdate
    checkpointConfigurationUpdate_checkpointIntervalUpdate,
    checkpointConfigurationUpdate_checkpointingEnabledUpdate,
    checkpointConfigurationUpdate_configurationTypeUpdate,
    checkpointConfigurationUpdate_minPauseBetweenCheckpointsUpdate,

    -- ** CloudWatchLoggingOption
    cloudWatchLoggingOption_logStreamARN,

    -- ** CloudWatchLoggingOptionDescription
    cloudWatchLoggingOptionDescription_cloudWatchLoggingOptionId,
    cloudWatchLoggingOptionDescription_roleARN,
    cloudWatchLoggingOptionDescription_logStreamARN,

    -- ** CloudWatchLoggingOptionUpdate
    cloudWatchLoggingOptionUpdate_logStreamARNUpdate,
    cloudWatchLoggingOptionUpdate_cloudWatchLoggingOptionId,

    -- ** CodeContent
    codeContent_s3ContentLocation,
    codeContent_textContent,
    codeContent_zipFileContent,

    -- ** CodeContentDescription
    codeContentDescription_codeMD5,
    codeContentDescription_codeSize,
    codeContentDescription_s3ApplicationCodeLocationDescription,
    codeContentDescription_textContent,

    -- ** CodeContentUpdate
    codeContentUpdate_s3ContentLocationUpdate,
    codeContentUpdate_textContentUpdate,
    codeContentUpdate_zipFileContentUpdate,

    -- ** CustomArtifactConfiguration
    customArtifactConfiguration_mavenReference,
    customArtifactConfiguration_s3ContentLocation,
    customArtifactConfiguration_artifactType,

    -- ** CustomArtifactConfigurationDescription
    customArtifactConfigurationDescription_artifactType,
    customArtifactConfigurationDescription_mavenReferenceDescription,
    customArtifactConfigurationDescription_s3ContentLocationDescription,

    -- ** DeployAsApplicationConfiguration
    deployAsApplicationConfiguration_s3ContentLocation,

    -- ** DeployAsApplicationConfigurationDescription
    deployAsApplicationConfigurationDescription_s3ContentLocationDescription,

    -- ** DeployAsApplicationConfigurationUpdate
    deployAsApplicationConfigurationUpdate_s3ContentLocationUpdate,

    -- ** DestinationSchema
    destinationSchema_recordFormatType,

    -- ** EnvironmentProperties
    environmentProperties_propertyGroups,

    -- ** EnvironmentPropertyDescriptions
    environmentPropertyDescriptions_propertyGroupDescriptions,

    -- ** EnvironmentPropertyUpdates
    environmentPropertyUpdates_propertyGroups,

    -- ** FlinkApplicationConfiguration
    flinkApplicationConfiguration_checkpointConfiguration,
    flinkApplicationConfiguration_monitoringConfiguration,
    flinkApplicationConfiguration_parallelismConfiguration,

    -- ** FlinkApplicationConfigurationDescription
    flinkApplicationConfigurationDescription_checkpointConfigurationDescription,
    flinkApplicationConfigurationDescription_jobPlanDescription,
    flinkApplicationConfigurationDescription_monitoringConfigurationDescription,
    flinkApplicationConfigurationDescription_parallelismConfigurationDescription,

    -- ** FlinkApplicationConfigurationUpdate
    flinkApplicationConfigurationUpdate_checkpointConfigurationUpdate,
    flinkApplicationConfigurationUpdate_monitoringConfigurationUpdate,
    flinkApplicationConfigurationUpdate_parallelismConfigurationUpdate,

    -- ** FlinkRunConfiguration
    flinkRunConfiguration_allowNonRestoredState,

    -- ** GlueDataCatalogConfiguration
    glueDataCatalogConfiguration_databaseARN,

    -- ** GlueDataCatalogConfigurationDescription
    glueDataCatalogConfigurationDescription_databaseARN,

    -- ** GlueDataCatalogConfigurationUpdate
    glueDataCatalogConfigurationUpdate_databaseARNUpdate,

    -- ** Input
    input_inputParallelism,
    input_inputProcessingConfiguration,
    input_kinesisFirehoseInput,
    input_kinesisStreamsInput,
    input_namePrefix,
    input_inputSchema,

    -- ** InputDescription
    inputDescription_inAppStreamNames,
    inputDescription_inputId,
    inputDescription_inputParallelism,
    inputDescription_inputProcessingConfigurationDescription,
    inputDescription_inputSchema,
    inputDescription_inputStartingPositionConfiguration,
    inputDescription_kinesisFirehoseInputDescription,
    inputDescription_kinesisStreamsInputDescription,
    inputDescription_namePrefix,

    -- ** InputLambdaProcessor
    inputLambdaProcessor_resourceARN,

    -- ** InputLambdaProcessorDescription
    inputLambdaProcessorDescription_roleARN,
    inputLambdaProcessorDescription_resourceARN,

    -- ** InputLambdaProcessorUpdate
    inputLambdaProcessorUpdate_resourceARNUpdate,

    -- ** InputParallelism
    inputParallelism_count,

    -- ** InputParallelismUpdate
    inputParallelismUpdate_countUpdate,

    -- ** InputProcessingConfiguration
    inputProcessingConfiguration_inputLambdaProcessor,

    -- ** InputProcessingConfigurationDescription
    inputProcessingConfigurationDescription_inputLambdaProcessorDescription,

    -- ** InputProcessingConfigurationUpdate
    inputProcessingConfigurationUpdate_inputLambdaProcessorUpdate,

    -- ** InputSchemaUpdate
    inputSchemaUpdate_recordColumnUpdates,
    inputSchemaUpdate_recordEncodingUpdate,
    inputSchemaUpdate_recordFormatUpdate,

    -- ** InputStartingPositionConfiguration
    inputStartingPositionConfiguration_inputStartingPosition,

    -- ** InputUpdate
    inputUpdate_inputParallelismUpdate,
    inputUpdate_inputProcessingConfigurationUpdate,
    inputUpdate_inputSchemaUpdate,
    inputUpdate_kinesisFirehoseInputUpdate,
    inputUpdate_kinesisStreamsInputUpdate,
    inputUpdate_namePrefixUpdate,
    inputUpdate_inputId,

    -- ** JSONMappingParameters
    jSONMappingParameters_recordRowPath,

    -- ** KinesisFirehoseInput
    kinesisFirehoseInput_resourceARN,

    -- ** KinesisFirehoseInputDescription
    kinesisFirehoseInputDescription_roleARN,
    kinesisFirehoseInputDescription_resourceARN,

    -- ** KinesisFirehoseInputUpdate
    kinesisFirehoseInputUpdate_resourceARNUpdate,

    -- ** KinesisFirehoseOutput
    kinesisFirehoseOutput_resourceARN,

    -- ** KinesisFirehoseOutputDescription
    kinesisFirehoseOutputDescription_roleARN,
    kinesisFirehoseOutputDescription_resourceARN,

    -- ** KinesisFirehoseOutputUpdate
    kinesisFirehoseOutputUpdate_resourceARNUpdate,

    -- ** KinesisStreamsInput
    kinesisStreamsInput_resourceARN,

    -- ** KinesisStreamsInputDescription
    kinesisStreamsInputDescription_roleARN,
    kinesisStreamsInputDescription_resourceARN,

    -- ** KinesisStreamsInputUpdate
    kinesisStreamsInputUpdate_resourceARNUpdate,

    -- ** KinesisStreamsOutput
    kinesisStreamsOutput_resourceARN,

    -- ** KinesisStreamsOutputDescription
    kinesisStreamsOutputDescription_roleARN,
    kinesisStreamsOutputDescription_resourceARN,

    -- ** KinesisStreamsOutputUpdate
    kinesisStreamsOutputUpdate_resourceARNUpdate,

    -- ** LambdaOutput
    lambdaOutput_resourceARN,

    -- ** LambdaOutputDescription
    lambdaOutputDescription_roleARN,
    lambdaOutputDescription_resourceARN,

    -- ** LambdaOutputUpdate
    lambdaOutputUpdate_resourceARNUpdate,

    -- ** MappingParameters
    mappingParameters_cSVMappingParameters,
    mappingParameters_jSONMappingParameters,

    -- ** MavenReference
    mavenReference_groupId,
    mavenReference_artifactId,
    mavenReference_version,

    -- ** MonitoringConfiguration
    monitoringConfiguration_logLevel,
    monitoringConfiguration_metricsLevel,
    monitoringConfiguration_configurationType,

    -- ** MonitoringConfigurationDescription
    monitoringConfigurationDescription_configurationType,
    monitoringConfigurationDescription_logLevel,
    monitoringConfigurationDescription_metricsLevel,

    -- ** MonitoringConfigurationUpdate
    monitoringConfigurationUpdate_configurationTypeUpdate,
    monitoringConfigurationUpdate_logLevelUpdate,
    monitoringConfigurationUpdate_metricsLevelUpdate,

    -- ** Output
    output_kinesisFirehoseOutput,
    output_kinesisStreamsOutput,
    output_lambdaOutput,
    output_name,
    output_destinationSchema,

    -- ** OutputDescription
    outputDescription_destinationSchema,
    outputDescription_kinesisFirehoseOutputDescription,
    outputDescription_kinesisStreamsOutputDescription,
    outputDescription_lambdaOutputDescription,
    outputDescription_name,
    outputDescription_outputId,

    -- ** OutputUpdate
    outputUpdate_destinationSchemaUpdate,
    outputUpdate_kinesisFirehoseOutputUpdate,
    outputUpdate_kinesisStreamsOutputUpdate,
    outputUpdate_lambdaOutputUpdate,
    outputUpdate_nameUpdate,
    outputUpdate_outputId,

    -- ** ParallelismConfiguration
    parallelismConfiguration_autoScalingEnabled,
    parallelismConfiguration_parallelism,
    parallelismConfiguration_parallelismPerKPU,
    parallelismConfiguration_configurationType,

    -- ** ParallelismConfigurationDescription
    parallelismConfigurationDescription_autoScalingEnabled,
    parallelismConfigurationDescription_configurationType,
    parallelismConfigurationDescription_currentParallelism,
    parallelismConfigurationDescription_parallelism,
    parallelismConfigurationDescription_parallelismPerKPU,

    -- ** ParallelismConfigurationUpdate
    parallelismConfigurationUpdate_autoScalingEnabledUpdate,
    parallelismConfigurationUpdate_configurationTypeUpdate,
    parallelismConfigurationUpdate_parallelismPerKPUUpdate,
    parallelismConfigurationUpdate_parallelismUpdate,

    -- ** PropertyGroup
    propertyGroup_propertyGroupId,
    propertyGroup_propertyMap,

    -- ** RecordColumn
    recordColumn_mapping,
    recordColumn_name,
    recordColumn_sqlType,

    -- ** RecordFormat
    recordFormat_mappingParameters,
    recordFormat_recordFormatType,

    -- ** ReferenceDataSource
    referenceDataSource_s3ReferenceDataSource,
    referenceDataSource_tableName,
    referenceDataSource_referenceSchema,

    -- ** ReferenceDataSourceDescription
    referenceDataSourceDescription_referenceSchema,
    referenceDataSourceDescription_referenceId,
    referenceDataSourceDescription_tableName,
    referenceDataSourceDescription_s3ReferenceDataSourceDescription,

    -- ** ReferenceDataSourceUpdate
    referenceDataSourceUpdate_referenceSchemaUpdate,
    referenceDataSourceUpdate_s3ReferenceDataSourceUpdate,
    referenceDataSourceUpdate_tableNameUpdate,
    referenceDataSourceUpdate_referenceId,

    -- ** RunConfiguration
    runConfiguration_applicationRestoreConfiguration,
    runConfiguration_flinkRunConfiguration,
    runConfiguration_sqlRunConfigurations,

    -- ** RunConfigurationDescription
    runConfigurationDescription_applicationRestoreConfigurationDescription,
    runConfigurationDescription_flinkRunConfigurationDescription,

    -- ** RunConfigurationUpdate
    runConfigurationUpdate_applicationRestoreConfiguration,
    runConfigurationUpdate_flinkRunConfiguration,

    -- ** S3ApplicationCodeLocationDescription
    s3ApplicationCodeLocationDescription_objectVersion,
    s3ApplicationCodeLocationDescription_bucketARN,
    s3ApplicationCodeLocationDescription_fileKey,

    -- ** S3Configuration
    s3Configuration_bucketARN,
    s3Configuration_fileKey,

    -- ** S3ContentBaseLocation
    s3ContentBaseLocation_basePath,
    s3ContentBaseLocation_bucketARN,

    -- ** S3ContentBaseLocationDescription
    s3ContentBaseLocationDescription_basePath,
    s3ContentBaseLocationDescription_bucketARN,

    -- ** S3ContentBaseLocationUpdate
    s3ContentBaseLocationUpdate_basePathUpdate,
    s3ContentBaseLocationUpdate_bucketARNUpdate,

    -- ** S3ContentLocation
    s3ContentLocation_objectVersion,
    s3ContentLocation_bucketARN,
    s3ContentLocation_fileKey,

    -- ** S3ContentLocationUpdate
    s3ContentLocationUpdate_bucketARNUpdate,
    s3ContentLocationUpdate_fileKeyUpdate,
    s3ContentLocationUpdate_objectVersionUpdate,

    -- ** S3ReferenceDataSource
    s3ReferenceDataSource_bucketARN,
    s3ReferenceDataSource_fileKey,

    -- ** S3ReferenceDataSourceDescription
    s3ReferenceDataSourceDescription_referenceRoleARN,
    s3ReferenceDataSourceDescription_bucketARN,
    s3ReferenceDataSourceDescription_fileKey,

    -- ** S3ReferenceDataSourceUpdate
    s3ReferenceDataSourceUpdate_bucketARNUpdate,
    s3ReferenceDataSourceUpdate_fileKeyUpdate,

    -- ** SnapshotDetails
    snapshotDetails_snapshotCreationTimestamp,
    snapshotDetails_snapshotName,
    snapshotDetails_snapshotStatus,
    snapshotDetails_applicationVersionId,

    -- ** SourceSchema
    sourceSchema_recordEncoding,
    sourceSchema_recordFormat,
    sourceSchema_recordColumns,

    -- ** SqlApplicationConfiguration
    sqlApplicationConfiguration_inputs,
    sqlApplicationConfiguration_outputs,
    sqlApplicationConfiguration_referenceDataSources,

    -- ** SqlApplicationConfigurationDescription
    sqlApplicationConfigurationDescription_inputDescriptions,
    sqlApplicationConfigurationDescription_outputDescriptions,
    sqlApplicationConfigurationDescription_referenceDataSourceDescriptions,

    -- ** SqlApplicationConfigurationUpdate
    sqlApplicationConfigurationUpdate_inputUpdates,
    sqlApplicationConfigurationUpdate_outputUpdates,
    sqlApplicationConfigurationUpdate_referenceDataSourceUpdates,

    -- ** SqlRunConfiguration
    sqlRunConfiguration_inputId,
    sqlRunConfiguration_inputStartingPositionConfiguration,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** VpcConfiguration
    vpcConfiguration_subnetIds,
    vpcConfiguration_securityGroupIds,

    -- ** VpcConfigurationDescription
    vpcConfigurationDescription_vpcConfigurationId,
    vpcConfigurationDescription_vpcId,
    vpcConfigurationDescription_subnetIds,
    vpcConfigurationDescription_securityGroupIds,

    -- ** VpcConfigurationUpdate
    vpcConfigurationUpdate_securityGroupIdUpdates,
    vpcConfigurationUpdate_subnetIdUpdates,
    vpcConfigurationUpdate_vpcConfigurationId,

    -- ** ZeppelinApplicationConfiguration
    zeppelinApplicationConfiguration_catalogConfiguration,
    zeppelinApplicationConfiguration_customArtifactsConfiguration,
    zeppelinApplicationConfiguration_deployAsApplicationConfiguration,
    zeppelinApplicationConfiguration_monitoringConfiguration,

    -- ** ZeppelinApplicationConfigurationDescription
    zeppelinApplicationConfigurationDescription_catalogConfigurationDescription,
    zeppelinApplicationConfigurationDescription_customArtifactsConfigurationDescription,
    zeppelinApplicationConfigurationDescription_deployAsApplicationConfigurationDescription,
    zeppelinApplicationConfigurationDescription_monitoringConfigurationDescription,

    -- ** ZeppelinApplicationConfigurationUpdate
    zeppelinApplicationConfigurationUpdate_catalogConfigurationUpdate,
    zeppelinApplicationConfigurationUpdate_customArtifactsConfigurationUpdate,
    zeppelinApplicationConfigurationUpdate_deployAsApplicationConfigurationUpdate,
    zeppelinApplicationConfigurationUpdate_monitoringConfigurationUpdate,

    -- ** ZeppelinMonitoringConfiguration
    zeppelinMonitoringConfiguration_logLevel,

    -- ** ZeppelinMonitoringConfigurationDescription
    zeppelinMonitoringConfigurationDescription_logLevel,

    -- ** ZeppelinMonitoringConfigurationUpdate
    zeppelinMonitoringConfigurationUpdate_logLevelUpdate,
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
import Amazonka.KinesisAnalyticsV2.ListApplicationSnapshots
import Amazonka.KinesisAnalyticsV2.ListApplicationVersions
import Amazonka.KinesisAnalyticsV2.ListApplications
import Amazonka.KinesisAnalyticsV2.ListTagsForResource
import Amazonka.KinesisAnalyticsV2.RollbackApplication
import Amazonka.KinesisAnalyticsV2.StartApplication
import Amazonka.KinesisAnalyticsV2.StopApplication
import Amazonka.KinesisAnalyticsV2.TagResource
import Amazonka.KinesisAnalyticsV2.Types.ApplicationCodeConfiguration
import Amazonka.KinesisAnalyticsV2.Types.ApplicationCodeConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.ApplicationCodeConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.ApplicationConfiguration
import Amazonka.KinesisAnalyticsV2.Types.ApplicationConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.ApplicationConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.ApplicationDetail
import Amazonka.KinesisAnalyticsV2.Types.ApplicationMaintenanceConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.ApplicationMaintenanceConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.ApplicationRestoreConfiguration
import Amazonka.KinesisAnalyticsV2.Types.ApplicationSnapshotConfiguration
import Amazonka.KinesisAnalyticsV2.Types.ApplicationSnapshotConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.ApplicationSnapshotConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.ApplicationSummary
import Amazonka.KinesisAnalyticsV2.Types.ApplicationVersionSummary
import Amazonka.KinesisAnalyticsV2.Types.CSVMappingParameters
import Amazonka.KinesisAnalyticsV2.Types.CatalogConfiguration
import Amazonka.KinesisAnalyticsV2.Types.CatalogConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.CatalogConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.CheckpointConfiguration
import Amazonka.KinesisAnalyticsV2.Types.CheckpointConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.CheckpointConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.CloudWatchLoggingOption
import Amazonka.KinesisAnalyticsV2.Types.CloudWatchLoggingOptionDescription
import Amazonka.KinesisAnalyticsV2.Types.CloudWatchLoggingOptionUpdate
import Amazonka.KinesisAnalyticsV2.Types.CodeContent
import Amazonka.KinesisAnalyticsV2.Types.CodeContentDescription
import Amazonka.KinesisAnalyticsV2.Types.CodeContentUpdate
import Amazonka.KinesisAnalyticsV2.Types.CustomArtifactConfiguration
import Amazonka.KinesisAnalyticsV2.Types.CustomArtifactConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.DeployAsApplicationConfiguration
import Amazonka.KinesisAnalyticsV2.Types.DeployAsApplicationConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.DeployAsApplicationConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.DestinationSchema
import Amazonka.KinesisAnalyticsV2.Types.EnvironmentProperties
import Amazonka.KinesisAnalyticsV2.Types.EnvironmentPropertyDescriptions
import Amazonka.KinesisAnalyticsV2.Types.EnvironmentPropertyUpdates
import Amazonka.KinesisAnalyticsV2.Types.FlinkApplicationConfiguration
import Amazonka.KinesisAnalyticsV2.Types.FlinkApplicationConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.FlinkApplicationConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.FlinkRunConfiguration
import Amazonka.KinesisAnalyticsV2.Types.GlueDataCatalogConfiguration
import Amazonka.KinesisAnalyticsV2.Types.GlueDataCatalogConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.GlueDataCatalogConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.Input
import Amazonka.KinesisAnalyticsV2.Types.InputDescription
import Amazonka.KinesisAnalyticsV2.Types.InputLambdaProcessor
import Amazonka.KinesisAnalyticsV2.Types.InputLambdaProcessorDescription
import Amazonka.KinesisAnalyticsV2.Types.InputLambdaProcessorUpdate
import Amazonka.KinesisAnalyticsV2.Types.InputParallelism
import Amazonka.KinesisAnalyticsV2.Types.InputParallelismUpdate
import Amazonka.KinesisAnalyticsV2.Types.InputProcessingConfiguration
import Amazonka.KinesisAnalyticsV2.Types.InputProcessingConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.InputProcessingConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.InputSchemaUpdate
import Amazonka.KinesisAnalyticsV2.Types.InputStartingPositionConfiguration
import Amazonka.KinesisAnalyticsV2.Types.InputUpdate
import Amazonka.KinesisAnalyticsV2.Types.JSONMappingParameters
import Amazonka.KinesisAnalyticsV2.Types.KinesisFirehoseInput
import Amazonka.KinesisAnalyticsV2.Types.KinesisFirehoseInputDescription
import Amazonka.KinesisAnalyticsV2.Types.KinesisFirehoseInputUpdate
import Amazonka.KinesisAnalyticsV2.Types.KinesisFirehoseOutput
import Amazonka.KinesisAnalyticsV2.Types.KinesisFirehoseOutputDescription
import Amazonka.KinesisAnalyticsV2.Types.KinesisFirehoseOutputUpdate
import Amazonka.KinesisAnalyticsV2.Types.KinesisStreamsInput
import Amazonka.KinesisAnalyticsV2.Types.KinesisStreamsInputDescription
import Amazonka.KinesisAnalyticsV2.Types.KinesisStreamsInputUpdate
import Amazonka.KinesisAnalyticsV2.Types.KinesisStreamsOutput
import Amazonka.KinesisAnalyticsV2.Types.KinesisStreamsOutputDescription
import Amazonka.KinesisAnalyticsV2.Types.KinesisStreamsOutputUpdate
import Amazonka.KinesisAnalyticsV2.Types.LambdaOutput
import Amazonka.KinesisAnalyticsV2.Types.LambdaOutputDescription
import Amazonka.KinesisAnalyticsV2.Types.LambdaOutputUpdate
import Amazonka.KinesisAnalyticsV2.Types.MappingParameters
import Amazonka.KinesisAnalyticsV2.Types.MavenReference
import Amazonka.KinesisAnalyticsV2.Types.MonitoringConfiguration
import Amazonka.KinesisAnalyticsV2.Types.MonitoringConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.MonitoringConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.Output
import Amazonka.KinesisAnalyticsV2.Types.OutputDescription
import Amazonka.KinesisAnalyticsV2.Types.OutputUpdate
import Amazonka.KinesisAnalyticsV2.Types.ParallelismConfiguration
import Amazonka.KinesisAnalyticsV2.Types.ParallelismConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.ParallelismConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.PropertyGroup
import Amazonka.KinesisAnalyticsV2.Types.RecordColumn
import Amazonka.KinesisAnalyticsV2.Types.RecordFormat
import Amazonka.KinesisAnalyticsV2.Types.ReferenceDataSource
import Amazonka.KinesisAnalyticsV2.Types.ReferenceDataSourceDescription
import Amazonka.KinesisAnalyticsV2.Types.ReferenceDataSourceUpdate
import Amazonka.KinesisAnalyticsV2.Types.RunConfiguration
import Amazonka.KinesisAnalyticsV2.Types.RunConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.RunConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.S3ApplicationCodeLocationDescription
import Amazonka.KinesisAnalyticsV2.Types.S3Configuration
import Amazonka.KinesisAnalyticsV2.Types.S3ContentBaseLocation
import Amazonka.KinesisAnalyticsV2.Types.S3ContentBaseLocationDescription
import Amazonka.KinesisAnalyticsV2.Types.S3ContentBaseLocationUpdate
import Amazonka.KinesisAnalyticsV2.Types.S3ContentLocation
import Amazonka.KinesisAnalyticsV2.Types.S3ContentLocationUpdate
import Amazonka.KinesisAnalyticsV2.Types.S3ReferenceDataSource
import Amazonka.KinesisAnalyticsV2.Types.S3ReferenceDataSourceDescription
import Amazonka.KinesisAnalyticsV2.Types.S3ReferenceDataSourceUpdate
import Amazonka.KinesisAnalyticsV2.Types.SnapshotDetails
import Amazonka.KinesisAnalyticsV2.Types.SourceSchema
import Amazonka.KinesisAnalyticsV2.Types.SqlApplicationConfiguration
import Amazonka.KinesisAnalyticsV2.Types.SqlApplicationConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.SqlApplicationConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.SqlRunConfiguration
import Amazonka.KinesisAnalyticsV2.Types.Tag
import Amazonka.KinesisAnalyticsV2.Types.VpcConfiguration
import Amazonka.KinesisAnalyticsV2.Types.VpcConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.VpcConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.ZeppelinApplicationConfiguration
import Amazonka.KinesisAnalyticsV2.Types.ZeppelinApplicationConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.ZeppelinApplicationConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.ZeppelinMonitoringConfiguration
import Amazonka.KinesisAnalyticsV2.Types.ZeppelinMonitoringConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.ZeppelinMonitoringConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.UntagResource
import Amazonka.KinesisAnalyticsV2.UpdateApplication
import Amazonka.KinesisAnalyticsV2.UpdateApplicationMaintenanceConfiguration

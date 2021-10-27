{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalyticsV2.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalyticsV2.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidApplicationConfigurationException,
    _ResourceProvisionedThroughputExceededException,
    _InvalidRequestException,
    _UnsupportedOperationException,
    _InvalidArgumentException,
    _TooManyTagsException,
    _CodeValidationException,
    _ConcurrentModificationException,
    _ServiceUnavailableException,
    _UnableToDetectSchemaException,
    _ResourceNotFoundException,
    _LimitExceededException,
    _ResourceInUseException,

    -- * ApplicationMode
    ApplicationMode (..),

    -- * ApplicationRestoreType
    ApplicationRestoreType (..),

    -- * ApplicationStatus
    ApplicationStatus (..),

    -- * ArtifactType
    ArtifactType (..),

    -- * CodeContentType
    CodeContentType (..),

    -- * ConfigurationType
    ConfigurationType (..),

    -- * InputStartingPosition
    InputStartingPosition (..),

    -- * LogLevel
    LogLevel (..),

    -- * MetricsLevel
    MetricsLevel (..),

    -- * RecordFormatType
    RecordFormatType (..),

    -- * RuntimeEnvironment
    RuntimeEnvironment (..),

    -- * SnapshotStatus
    SnapshotStatus (..),

    -- * UrlType
    UrlType (..),

    -- * ApplicationCodeConfiguration
    ApplicationCodeConfiguration (..),
    newApplicationCodeConfiguration,
    applicationCodeConfiguration_codeContent,
    applicationCodeConfiguration_codeContentType,

    -- * ApplicationCodeConfigurationDescription
    ApplicationCodeConfigurationDescription (..),
    newApplicationCodeConfigurationDescription,
    applicationCodeConfigurationDescription_codeContentDescription,
    applicationCodeConfigurationDescription_codeContentType,

    -- * ApplicationCodeConfigurationUpdate
    ApplicationCodeConfigurationUpdate (..),
    newApplicationCodeConfigurationUpdate,
    applicationCodeConfigurationUpdate_codeContentTypeUpdate,
    applicationCodeConfigurationUpdate_codeContentUpdate,

    -- * ApplicationConfiguration
    ApplicationConfiguration (..),
    newApplicationConfiguration,
    applicationConfiguration_applicationSnapshotConfiguration,
    applicationConfiguration_vpcConfigurations,
    applicationConfiguration_environmentProperties,
    applicationConfiguration_flinkApplicationConfiguration,
    applicationConfiguration_sqlApplicationConfiguration,
    applicationConfiguration_applicationCodeConfiguration,
    applicationConfiguration_zeppelinApplicationConfiguration,

    -- * ApplicationConfigurationDescription
    ApplicationConfigurationDescription (..),
    newApplicationConfigurationDescription,
    applicationConfigurationDescription_zeppelinApplicationConfigurationDescription,
    applicationConfigurationDescription_vpcConfigurationDescriptions,
    applicationConfigurationDescription_applicationCodeConfigurationDescription,
    applicationConfigurationDescription_environmentPropertyDescriptions,
    applicationConfigurationDescription_runConfigurationDescription,
    applicationConfigurationDescription_sqlApplicationConfigurationDescription,
    applicationConfigurationDescription_flinkApplicationConfigurationDescription,
    applicationConfigurationDescription_applicationSnapshotConfigurationDescription,

    -- * ApplicationConfigurationUpdate
    ApplicationConfigurationUpdate (..),
    newApplicationConfigurationUpdate,
    applicationConfigurationUpdate_applicationSnapshotConfigurationUpdate,
    applicationConfigurationUpdate_zeppelinApplicationConfigurationUpdate,
    applicationConfigurationUpdate_flinkApplicationConfigurationUpdate,
    applicationConfigurationUpdate_sqlApplicationConfigurationUpdate,
    applicationConfigurationUpdate_vpcConfigurationUpdates,
    applicationConfigurationUpdate_environmentPropertyUpdates,
    applicationConfigurationUpdate_applicationCodeConfigurationUpdate,

    -- * ApplicationDetail
    ApplicationDetail (..),
    newApplicationDetail,
    applicationDetail_applicationDescription,
    applicationDetail_conditionalToken,
    applicationDetail_cloudWatchLoggingOptionDescriptions,
    applicationDetail_applicationVersionRolledBackFrom,
    applicationDetail_applicationMode,
    applicationDetail_applicationConfigurationDescription,
    applicationDetail_applicationVersionRolledBackTo,
    applicationDetail_createTimestamp,
    applicationDetail_lastUpdateTimestamp,
    applicationDetail_serviceExecutionRole,
    applicationDetail_applicationVersionUpdatedFrom,
    applicationDetail_applicationMaintenanceConfigurationDescription,
    applicationDetail_applicationARN,
    applicationDetail_applicationName,
    applicationDetail_runtimeEnvironment,
    applicationDetail_applicationStatus,
    applicationDetail_applicationVersionId,

    -- * ApplicationMaintenanceConfigurationDescription
    ApplicationMaintenanceConfigurationDescription (..),
    newApplicationMaintenanceConfigurationDescription,
    applicationMaintenanceConfigurationDescription_applicationMaintenanceWindowStartTime,
    applicationMaintenanceConfigurationDescription_applicationMaintenanceWindowEndTime,

    -- * ApplicationMaintenanceConfigurationUpdate
    ApplicationMaintenanceConfigurationUpdate (..),
    newApplicationMaintenanceConfigurationUpdate,
    applicationMaintenanceConfigurationUpdate_applicationMaintenanceWindowStartTimeUpdate,

    -- * ApplicationRestoreConfiguration
    ApplicationRestoreConfiguration (..),
    newApplicationRestoreConfiguration,
    applicationRestoreConfiguration_snapshotName,
    applicationRestoreConfiguration_applicationRestoreType,

    -- * ApplicationSnapshotConfiguration
    ApplicationSnapshotConfiguration (..),
    newApplicationSnapshotConfiguration,
    applicationSnapshotConfiguration_snapshotsEnabled,

    -- * ApplicationSnapshotConfigurationDescription
    ApplicationSnapshotConfigurationDescription (..),
    newApplicationSnapshotConfigurationDescription,
    applicationSnapshotConfigurationDescription_snapshotsEnabled,

    -- * ApplicationSnapshotConfigurationUpdate
    ApplicationSnapshotConfigurationUpdate (..),
    newApplicationSnapshotConfigurationUpdate,
    applicationSnapshotConfigurationUpdate_snapshotsEnabledUpdate,

    -- * ApplicationSummary
    ApplicationSummary (..),
    newApplicationSummary,
    applicationSummary_applicationMode,
    applicationSummary_applicationName,
    applicationSummary_applicationARN,
    applicationSummary_applicationStatus,
    applicationSummary_applicationVersionId,
    applicationSummary_runtimeEnvironment,

    -- * ApplicationVersionSummary
    ApplicationVersionSummary (..),
    newApplicationVersionSummary,
    applicationVersionSummary_applicationVersionId,
    applicationVersionSummary_applicationStatus,

    -- * CSVMappingParameters
    CSVMappingParameters (..),
    newCSVMappingParameters,
    cSVMappingParameters_recordRowDelimiter,
    cSVMappingParameters_recordColumnDelimiter,

    -- * CatalogConfiguration
    CatalogConfiguration (..),
    newCatalogConfiguration,
    catalogConfiguration_glueDataCatalogConfiguration,

    -- * CatalogConfigurationDescription
    CatalogConfigurationDescription (..),
    newCatalogConfigurationDescription,
    catalogConfigurationDescription_glueDataCatalogConfigurationDescription,

    -- * CatalogConfigurationUpdate
    CatalogConfigurationUpdate (..),
    newCatalogConfigurationUpdate,
    catalogConfigurationUpdate_glueDataCatalogConfigurationUpdate,

    -- * CheckpointConfiguration
    CheckpointConfiguration (..),
    newCheckpointConfiguration,
    checkpointConfiguration_checkpointingEnabled,
    checkpointConfiguration_minPauseBetweenCheckpoints,
    checkpointConfiguration_checkpointInterval,
    checkpointConfiguration_configurationType,

    -- * CheckpointConfigurationDescription
    CheckpointConfigurationDescription (..),
    newCheckpointConfigurationDescription,
    checkpointConfigurationDescription_checkpointingEnabled,
    checkpointConfigurationDescription_configurationType,
    checkpointConfigurationDescription_minPauseBetweenCheckpoints,
    checkpointConfigurationDescription_checkpointInterval,

    -- * CheckpointConfigurationUpdate
    CheckpointConfigurationUpdate (..),
    newCheckpointConfigurationUpdate,
    checkpointConfigurationUpdate_checkpointIntervalUpdate,
    checkpointConfigurationUpdate_checkpointingEnabledUpdate,
    checkpointConfigurationUpdate_minPauseBetweenCheckpointsUpdate,
    checkpointConfigurationUpdate_configurationTypeUpdate,

    -- * CloudWatchLoggingOption
    CloudWatchLoggingOption (..),
    newCloudWatchLoggingOption,
    cloudWatchLoggingOption_logStreamARN,

    -- * CloudWatchLoggingOptionDescription
    CloudWatchLoggingOptionDescription (..),
    newCloudWatchLoggingOptionDescription,
    cloudWatchLoggingOptionDescription_cloudWatchLoggingOptionId,
    cloudWatchLoggingOptionDescription_roleARN,
    cloudWatchLoggingOptionDescription_logStreamARN,

    -- * CloudWatchLoggingOptionUpdate
    CloudWatchLoggingOptionUpdate (..),
    newCloudWatchLoggingOptionUpdate,
    cloudWatchLoggingOptionUpdate_logStreamARNUpdate,
    cloudWatchLoggingOptionUpdate_cloudWatchLoggingOptionId,

    -- * CodeContent
    CodeContent (..),
    newCodeContent,
    codeContent_textContent,
    codeContent_s3ContentLocation,
    codeContent_zipFileContent,

    -- * CodeContentDescription
    CodeContentDescription (..),
    newCodeContentDescription,
    codeContentDescription_s3ApplicationCodeLocationDescription,
    codeContentDescription_codeMD5,
    codeContentDescription_textContent,
    codeContentDescription_codeSize,

    -- * CodeContentUpdate
    CodeContentUpdate (..),
    newCodeContentUpdate,
    codeContentUpdate_textContentUpdate,
    codeContentUpdate_zipFileContentUpdate,
    codeContentUpdate_s3ContentLocationUpdate,

    -- * CustomArtifactConfiguration
    CustomArtifactConfiguration (..),
    newCustomArtifactConfiguration,
    customArtifactConfiguration_s3ContentLocation,
    customArtifactConfiguration_mavenReference,
    customArtifactConfiguration_artifactType,

    -- * CustomArtifactConfigurationDescription
    CustomArtifactConfigurationDescription (..),
    newCustomArtifactConfigurationDescription,
    customArtifactConfigurationDescription_mavenReferenceDescription,
    customArtifactConfigurationDescription_s3ContentLocationDescription,
    customArtifactConfigurationDescription_artifactType,

    -- * DeployAsApplicationConfiguration
    DeployAsApplicationConfiguration (..),
    newDeployAsApplicationConfiguration,
    deployAsApplicationConfiguration_s3ContentLocation,

    -- * DeployAsApplicationConfigurationDescription
    DeployAsApplicationConfigurationDescription (..),
    newDeployAsApplicationConfigurationDescription,
    deployAsApplicationConfigurationDescription_s3ContentLocationDescription,

    -- * DeployAsApplicationConfigurationUpdate
    DeployAsApplicationConfigurationUpdate (..),
    newDeployAsApplicationConfigurationUpdate,
    deployAsApplicationConfigurationUpdate_s3ContentLocationUpdate,

    -- * DestinationSchema
    DestinationSchema (..),
    newDestinationSchema,
    destinationSchema_recordFormatType,

    -- * EnvironmentProperties
    EnvironmentProperties (..),
    newEnvironmentProperties,
    environmentProperties_propertyGroups,

    -- * EnvironmentPropertyDescriptions
    EnvironmentPropertyDescriptions (..),
    newEnvironmentPropertyDescriptions,
    environmentPropertyDescriptions_propertyGroupDescriptions,

    -- * EnvironmentPropertyUpdates
    EnvironmentPropertyUpdates (..),
    newEnvironmentPropertyUpdates,
    environmentPropertyUpdates_propertyGroups,

    -- * FlinkApplicationConfiguration
    FlinkApplicationConfiguration (..),
    newFlinkApplicationConfiguration,
    flinkApplicationConfiguration_monitoringConfiguration,
    flinkApplicationConfiguration_parallelismConfiguration,
    flinkApplicationConfiguration_checkpointConfiguration,

    -- * FlinkApplicationConfigurationDescription
    FlinkApplicationConfigurationDescription (..),
    newFlinkApplicationConfigurationDescription,
    flinkApplicationConfigurationDescription_monitoringConfigurationDescription,
    flinkApplicationConfigurationDescription_parallelismConfigurationDescription,
    flinkApplicationConfigurationDescription_checkpointConfigurationDescription,
    flinkApplicationConfigurationDescription_jobPlanDescription,

    -- * FlinkApplicationConfigurationUpdate
    FlinkApplicationConfigurationUpdate (..),
    newFlinkApplicationConfigurationUpdate,
    flinkApplicationConfigurationUpdate_checkpointConfigurationUpdate,
    flinkApplicationConfigurationUpdate_parallelismConfigurationUpdate,
    flinkApplicationConfigurationUpdate_monitoringConfigurationUpdate,

    -- * FlinkRunConfiguration
    FlinkRunConfiguration (..),
    newFlinkRunConfiguration,
    flinkRunConfiguration_allowNonRestoredState,

    -- * GlueDataCatalogConfiguration
    GlueDataCatalogConfiguration (..),
    newGlueDataCatalogConfiguration,
    glueDataCatalogConfiguration_databaseARN,

    -- * GlueDataCatalogConfigurationDescription
    GlueDataCatalogConfigurationDescription (..),
    newGlueDataCatalogConfigurationDescription,
    glueDataCatalogConfigurationDescription_databaseARN,

    -- * GlueDataCatalogConfigurationUpdate
    GlueDataCatalogConfigurationUpdate (..),
    newGlueDataCatalogConfigurationUpdate,
    glueDataCatalogConfigurationUpdate_databaseARNUpdate,

    -- * Input
    Input (..),
    newInput,
    input_inputParallelism,
    input_inputProcessingConfiguration,
    input_kinesisStreamsInput,
    input_kinesisFirehoseInput,
    input_namePrefix,
    input_inputSchema,

    -- * InputDescription
    InputDescription (..),
    newInputDescription,
    inputDescription_inputStartingPositionConfiguration,
    inputDescription_inputParallelism,
    inputDescription_inputId,
    inputDescription_inAppStreamNames,
    inputDescription_kinesisFirehoseInputDescription,
    inputDescription_inputSchema,
    inputDescription_kinesisStreamsInputDescription,
    inputDescription_namePrefix,
    inputDescription_inputProcessingConfigurationDescription,

    -- * InputLambdaProcessor
    InputLambdaProcessor (..),
    newInputLambdaProcessor,
    inputLambdaProcessor_resourceARN,

    -- * InputLambdaProcessorDescription
    InputLambdaProcessorDescription (..),
    newInputLambdaProcessorDescription,
    inputLambdaProcessorDescription_roleARN,
    inputLambdaProcessorDescription_resourceARN,

    -- * InputLambdaProcessorUpdate
    InputLambdaProcessorUpdate (..),
    newInputLambdaProcessorUpdate,
    inputLambdaProcessorUpdate_resourceARNUpdate,

    -- * InputParallelism
    InputParallelism (..),
    newInputParallelism,
    inputParallelism_count,

    -- * InputParallelismUpdate
    InputParallelismUpdate (..),
    newInputParallelismUpdate,
    inputParallelismUpdate_countUpdate,

    -- * InputProcessingConfiguration
    InputProcessingConfiguration (..),
    newInputProcessingConfiguration,
    inputProcessingConfiguration_inputLambdaProcessor,

    -- * InputProcessingConfigurationDescription
    InputProcessingConfigurationDescription (..),
    newInputProcessingConfigurationDescription,
    inputProcessingConfigurationDescription_inputLambdaProcessorDescription,

    -- * InputProcessingConfigurationUpdate
    InputProcessingConfigurationUpdate (..),
    newInputProcessingConfigurationUpdate,
    inputProcessingConfigurationUpdate_inputLambdaProcessorUpdate,

    -- * InputSchemaUpdate
    InputSchemaUpdate (..),
    newInputSchemaUpdate,
    inputSchemaUpdate_recordFormatUpdate,
    inputSchemaUpdate_recordEncodingUpdate,
    inputSchemaUpdate_recordColumnUpdates,

    -- * InputStartingPositionConfiguration
    InputStartingPositionConfiguration (..),
    newInputStartingPositionConfiguration,
    inputStartingPositionConfiguration_inputStartingPosition,

    -- * InputUpdate
    InputUpdate (..),
    newInputUpdate,
    inputUpdate_inputProcessingConfigurationUpdate,
    inputUpdate_kinesisStreamsInputUpdate,
    inputUpdate_inputParallelismUpdate,
    inputUpdate_namePrefixUpdate,
    inputUpdate_inputSchemaUpdate,
    inputUpdate_kinesisFirehoseInputUpdate,
    inputUpdate_inputId,

    -- * JSONMappingParameters
    JSONMappingParameters (..),
    newJSONMappingParameters,
    jSONMappingParameters_recordRowPath,

    -- * KinesisFirehoseInput
    KinesisFirehoseInput (..),
    newKinesisFirehoseInput,
    kinesisFirehoseInput_resourceARN,

    -- * KinesisFirehoseInputDescription
    KinesisFirehoseInputDescription (..),
    newKinesisFirehoseInputDescription,
    kinesisFirehoseInputDescription_roleARN,
    kinesisFirehoseInputDescription_resourceARN,

    -- * KinesisFirehoseInputUpdate
    KinesisFirehoseInputUpdate (..),
    newKinesisFirehoseInputUpdate,
    kinesisFirehoseInputUpdate_resourceARNUpdate,

    -- * KinesisFirehoseOutput
    KinesisFirehoseOutput (..),
    newKinesisFirehoseOutput,
    kinesisFirehoseOutput_resourceARN,

    -- * KinesisFirehoseOutputDescription
    KinesisFirehoseOutputDescription (..),
    newKinesisFirehoseOutputDescription,
    kinesisFirehoseOutputDescription_roleARN,
    kinesisFirehoseOutputDescription_resourceARN,

    -- * KinesisFirehoseOutputUpdate
    KinesisFirehoseOutputUpdate (..),
    newKinesisFirehoseOutputUpdate,
    kinesisFirehoseOutputUpdate_resourceARNUpdate,

    -- * KinesisStreamsInput
    KinesisStreamsInput (..),
    newKinesisStreamsInput,
    kinesisStreamsInput_resourceARN,

    -- * KinesisStreamsInputDescription
    KinesisStreamsInputDescription (..),
    newKinesisStreamsInputDescription,
    kinesisStreamsInputDescription_roleARN,
    kinesisStreamsInputDescription_resourceARN,

    -- * KinesisStreamsInputUpdate
    KinesisStreamsInputUpdate (..),
    newKinesisStreamsInputUpdate,
    kinesisStreamsInputUpdate_resourceARNUpdate,

    -- * KinesisStreamsOutput
    KinesisStreamsOutput (..),
    newKinesisStreamsOutput,
    kinesisStreamsOutput_resourceARN,

    -- * KinesisStreamsOutputDescription
    KinesisStreamsOutputDescription (..),
    newKinesisStreamsOutputDescription,
    kinesisStreamsOutputDescription_roleARN,
    kinesisStreamsOutputDescription_resourceARN,

    -- * KinesisStreamsOutputUpdate
    KinesisStreamsOutputUpdate (..),
    newKinesisStreamsOutputUpdate,
    kinesisStreamsOutputUpdate_resourceARNUpdate,

    -- * LambdaOutput
    LambdaOutput (..),
    newLambdaOutput,
    lambdaOutput_resourceARN,

    -- * LambdaOutputDescription
    LambdaOutputDescription (..),
    newLambdaOutputDescription,
    lambdaOutputDescription_roleARN,
    lambdaOutputDescription_resourceARN,

    -- * LambdaOutputUpdate
    LambdaOutputUpdate (..),
    newLambdaOutputUpdate,
    lambdaOutputUpdate_resourceARNUpdate,

    -- * MappingParameters
    MappingParameters (..),
    newMappingParameters,
    mappingParameters_cSVMappingParameters,
    mappingParameters_jSONMappingParameters,

    -- * MavenReference
    MavenReference (..),
    newMavenReference,
    mavenReference_groupId,
    mavenReference_artifactId,
    mavenReference_version,

    -- * MonitoringConfiguration
    MonitoringConfiguration (..),
    newMonitoringConfiguration,
    monitoringConfiguration_logLevel,
    monitoringConfiguration_metricsLevel,
    monitoringConfiguration_configurationType,

    -- * MonitoringConfigurationDescription
    MonitoringConfigurationDescription (..),
    newMonitoringConfigurationDescription,
    monitoringConfigurationDescription_logLevel,
    monitoringConfigurationDescription_configurationType,
    monitoringConfigurationDescription_metricsLevel,

    -- * MonitoringConfigurationUpdate
    MonitoringConfigurationUpdate (..),
    newMonitoringConfigurationUpdate,
    monitoringConfigurationUpdate_metricsLevelUpdate,
    monitoringConfigurationUpdate_logLevelUpdate,
    monitoringConfigurationUpdate_configurationTypeUpdate,

    -- * Output
    Output (..),
    newOutput,
    output_lambdaOutput,
    output_kinesisStreamsOutput,
    output_kinesisFirehoseOutput,
    output_name,
    output_destinationSchema,

    -- * OutputDescription
    OutputDescription (..),
    newOutputDescription,
    outputDescription_outputId,
    outputDescription_destinationSchema,
    outputDescription_kinesisFirehoseOutputDescription,
    outputDescription_kinesisStreamsOutputDescription,
    outputDescription_name,
    outputDescription_lambdaOutputDescription,

    -- * OutputUpdate
    OutputUpdate (..),
    newOutputUpdate,
    outputUpdate_kinesisStreamsOutputUpdate,
    outputUpdate_destinationSchemaUpdate,
    outputUpdate_kinesisFirehoseOutputUpdate,
    outputUpdate_nameUpdate,
    outputUpdate_lambdaOutputUpdate,
    outputUpdate_outputId,

    -- * ParallelismConfiguration
    ParallelismConfiguration (..),
    newParallelismConfiguration,
    parallelismConfiguration_autoScalingEnabled,
    parallelismConfiguration_parallelismPerKPU,
    parallelismConfiguration_parallelism,
    parallelismConfiguration_configurationType,

    -- * ParallelismConfigurationDescription
    ParallelismConfigurationDescription (..),
    newParallelismConfigurationDescription,
    parallelismConfigurationDescription_autoScalingEnabled,
    parallelismConfigurationDescription_currentParallelism,
    parallelismConfigurationDescription_parallelismPerKPU,
    parallelismConfigurationDescription_configurationType,
    parallelismConfigurationDescription_parallelism,

    -- * ParallelismConfigurationUpdate
    ParallelismConfigurationUpdate (..),
    newParallelismConfigurationUpdate,
    parallelismConfigurationUpdate_parallelismUpdate,
    parallelismConfigurationUpdate_autoScalingEnabledUpdate,
    parallelismConfigurationUpdate_parallelismPerKPUUpdate,
    parallelismConfigurationUpdate_configurationTypeUpdate,

    -- * PropertyGroup
    PropertyGroup (..),
    newPropertyGroup,
    propertyGroup_propertyGroupId,
    propertyGroup_propertyMap,

    -- * RecordColumn
    RecordColumn (..),
    newRecordColumn,
    recordColumn_mapping,
    recordColumn_name,
    recordColumn_sqlType,

    -- * RecordFormat
    RecordFormat (..),
    newRecordFormat,
    recordFormat_mappingParameters,
    recordFormat_recordFormatType,

    -- * ReferenceDataSource
    ReferenceDataSource (..),
    newReferenceDataSource,
    referenceDataSource_s3ReferenceDataSource,
    referenceDataSource_tableName,
    referenceDataSource_referenceSchema,

    -- * ReferenceDataSourceDescription
    ReferenceDataSourceDescription (..),
    newReferenceDataSourceDescription,
    referenceDataSourceDescription_referenceSchema,
    referenceDataSourceDescription_referenceId,
    referenceDataSourceDescription_tableName,
    referenceDataSourceDescription_s3ReferenceDataSourceDescription,

    -- * ReferenceDataSourceUpdate
    ReferenceDataSourceUpdate (..),
    newReferenceDataSourceUpdate,
    referenceDataSourceUpdate_tableNameUpdate,
    referenceDataSourceUpdate_s3ReferenceDataSourceUpdate,
    referenceDataSourceUpdate_referenceSchemaUpdate,
    referenceDataSourceUpdate_referenceId,

    -- * RunConfiguration
    RunConfiguration (..),
    newRunConfiguration,
    runConfiguration_sqlRunConfigurations,
    runConfiguration_flinkRunConfiguration,
    runConfiguration_applicationRestoreConfiguration,

    -- * RunConfigurationDescription
    RunConfigurationDescription (..),
    newRunConfigurationDescription,
    runConfigurationDescription_applicationRestoreConfigurationDescription,
    runConfigurationDescription_flinkRunConfigurationDescription,

    -- * RunConfigurationUpdate
    RunConfigurationUpdate (..),
    newRunConfigurationUpdate,
    runConfigurationUpdate_flinkRunConfiguration,
    runConfigurationUpdate_applicationRestoreConfiguration,

    -- * S3ApplicationCodeLocationDescription
    S3ApplicationCodeLocationDescription (..),
    newS3ApplicationCodeLocationDescription,
    s3ApplicationCodeLocationDescription_objectVersion,
    s3ApplicationCodeLocationDescription_bucketARN,
    s3ApplicationCodeLocationDescription_fileKey,

    -- * S3Configuration
    S3Configuration (..),
    newS3Configuration,
    s3Configuration_bucketARN,
    s3Configuration_fileKey,

    -- * S3ContentBaseLocation
    S3ContentBaseLocation (..),
    newS3ContentBaseLocation,
    s3ContentBaseLocation_basePath,
    s3ContentBaseLocation_bucketARN,

    -- * S3ContentBaseLocationDescription
    S3ContentBaseLocationDescription (..),
    newS3ContentBaseLocationDescription,
    s3ContentBaseLocationDescription_basePath,
    s3ContentBaseLocationDescription_bucketARN,

    -- * S3ContentBaseLocationUpdate
    S3ContentBaseLocationUpdate (..),
    newS3ContentBaseLocationUpdate,
    s3ContentBaseLocationUpdate_bucketARNUpdate,
    s3ContentBaseLocationUpdate_basePathUpdate,

    -- * S3ContentLocation
    S3ContentLocation (..),
    newS3ContentLocation,
    s3ContentLocation_objectVersion,
    s3ContentLocation_bucketARN,
    s3ContentLocation_fileKey,

    -- * S3ContentLocationUpdate
    S3ContentLocationUpdate (..),
    newS3ContentLocationUpdate,
    s3ContentLocationUpdate_bucketARNUpdate,
    s3ContentLocationUpdate_fileKeyUpdate,
    s3ContentLocationUpdate_objectVersionUpdate,

    -- * S3ReferenceDataSource
    S3ReferenceDataSource (..),
    newS3ReferenceDataSource,
    s3ReferenceDataSource_bucketARN,
    s3ReferenceDataSource_fileKey,

    -- * S3ReferenceDataSourceDescription
    S3ReferenceDataSourceDescription (..),
    newS3ReferenceDataSourceDescription,
    s3ReferenceDataSourceDescription_referenceRoleARN,
    s3ReferenceDataSourceDescription_bucketARN,
    s3ReferenceDataSourceDescription_fileKey,

    -- * S3ReferenceDataSourceUpdate
    S3ReferenceDataSourceUpdate (..),
    newS3ReferenceDataSourceUpdate,
    s3ReferenceDataSourceUpdate_bucketARNUpdate,
    s3ReferenceDataSourceUpdate_fileKeyUpdate,

    -- * SnapshotDetails
    SnapshotDetails (..),
    newSnapshotDetails,
    snapshotDetails_snapshotCreationTimestamp,
    snapshotDetails_snapshotName,
    snapshotDetails_snapshotStatus,
    snapshotDetails_applicationVersionId,

    -- * SourceSchema
    SourceSchema (..),
    newSourceSchema,
    sourceSchema_recordEncoding,
    sourceSchema_recordFormat,
    sourceSchema_recordColumns,

    -- * SqlApplicationConfiguration
    SqlApplicationConfiguration (..),
    newSqlApplicationConfiguration,
    sqlApplicationConfiguration_inputs,
    sqlApplicationConfiguration_referenceDataSources,
    sqlApplicationConfiguration_outputs,

    -- * SqlApplicationConfigurationDescription
    SqlApplicationConfigurationDescription (..),
    newSqlApplicationConfigurationDescription,
    sqlApplicationConfigurationDescription_outputDescriptions,
    sqlApplicationConfigurationDescription_referenceDataSourceDescriptions,
    sqlApplicationConfigurationDescription_inputDescriptions,

    -- * SqlApplicationConfigurationUpdate
    SqlApplicationConfigurationUpdate (..),
    newSqlApplicationConfigurationUpdate,
    sqlApplicationConfigurationUpdate_referenceDataSourceUpdates,
    sqlApplicationConfigurationUpdate_inputUpdates,
    sqlApplicationConfigurationUpdate_outputUpdates,

    -- * SqlRunConfiguration
    SqlRunConfiguration (..),
    newSqlRunConfiguration,
    sqlRunConfiguration_inputId,
    sqlRunConfiguration_inputStartingPositionConfiguration,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * VpcConfiguration
    VpcConfiguration (..),
    newVpcConfiguration,
    vpcConfiguration_subnetIds,
    vpcConfiguration_securityGroupIds,

    -- * VpcConfigurationDescription
    VpcConfigurationDescription (..),
    newVpcConfigurationDescription,
    vpcConfigurationDescription_vpcConfigurationId,
    vpcConfigurationDescription_vpcId,
    vpcConfigurationDescription_subnetIds,
    vpcConfigurationDescription_securityGroupIds,

    -- * VpcConfigurationUpdate
    VpcConfigurationUpdate (..),
    newVpcConfigurationUpdate,
    vpcConfigurationUpdate_subnetIdUpdates,
    vpcConfigurationUpdate_securityGroupIdUpdates,
    vpcConfigurationUpdate_vpcConfigurationId,

    -- * ZeppelinApplicationConfiguration
    ZeppelinApplicationConfiguration (..),
    newZeppelinApplicationConfiguration,
    zeppelinApplicationConfiguration_monitoringConfiguration,
    zeppelinApplicationConfiguration_catalogConfiguration,
    zeppelinApplicationConfiguration_deployAsApplicationConfiguration,
    zeppelinApplicationConfiguration_customArtifactsConfiguration,

    -- * ZeppelinApplicationConfigurationDescription
    ZeppelinApplicationConfigurationDescription (..),
    newZeppelinApplicationConfigurationDescription,
    zeppelinApplicationConfigurationDescription_customArtifactsConfigurationDescription,
    zeppelinApplicationConfigurationDescription_deployAsApplicationConfigurationDescription,
    zeppelinApplicationConfigurationDescription_catalogConfigurationDescription,
    zeppelinApplicationConfigurationDescription_monitoringConfigurationDescription,

    -- * ZeppelinApplicationConfigurationUpdate
    ZeppelinApplicationConfigurationUpdate (..),
    newZeppelinApplicationConfigurationUpdate,
    zeppelinApplicationConfigurationUpdate_customArtifactsConfigurationUpdate,
    zeppelinApplicationConfigurationUpdate_catalogConfigurationUpdate,
    zeppelinApplicationConfigurationUpdate_deployAsApplicationConfigurationUpdate,
    zeppelinApplicationConfigurationUpdate_monitoringConfigurationUpdate,

    -- * ZeppelinMonitoringConfiguration
    ZeppelinMonitoringConfiguration (..),
    newZeppelinMonitoringConfiguration,
    zeppelinMonitoringConfiguration_logLevel,

    -- * ZeppelinMonitoringConfigurationDescription
    ZeppelinMonitoringConfigurationDescription (..),
    newZeppelinMonitoringConfigurationDescription,
    zeppelinMonitoringConfigurationDescription_logLevel,

    -- * ZeppelinMonitoringConfigurationUpdate
    ZeppelinMonitoringConfigurationUpdate (..),
    newZeppelinMonitoringConfigurationUpdate,
    zeppelinMonitoringConfigurationUpdate_logLevelUpdate,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationCodeConfiguration
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationCodeConfigurationDescription
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationCodeConfigurationUpdate
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationConfiguration
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationConfigurationDescription
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationConfigurationUpdate
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationDetail
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationMaintenanceConfigurationDescription
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationMaintenanceConfigurationUpdate
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationMode
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationRestoreConfiguration
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationRestoreType
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationSnapshotConfiguration
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationSnapshotConfigurationDescription
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationSnapshotConfigurationUpdate
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationStatus
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationSummary
import Network.AWS.KinesisAnalyticsV2.Types.ApplicationVersionSummary
import Network.AWS.KinesisAnalyticsV2.Types.ArtifactType
import Network.AWS.KinesisAnalyticsV2.Types.CSVMappingParameters
import Network.AWS.KinesisAnalyticsV2.Types.CatalogConfiguration
import Network.AWS.KinesisAnalyticsV2.Types.CatalogConfigurationDescription
import Network.AWS.KinesisAnalyticsV2.Types.CatalogConfigurationUpdate
import Network.AWS.KinesisAnalyticsV2.Types.CheckpointConfiguration
import Network.AWS.KinesisAnalyticsV2.Types.CheckpointConfigurationDescription
import Network.AWS.KinesisAnalyticsV2.Types.CheckpointConfigurationUpdate
import Network.AWS.KinesisAnalyticsV2.Types.CloudWatchLoggingOption
import Network.AWS.KinesisAnalyticsV2.Types.CloudWatchLoggingOptionDescription
import Network.AWS.KinesisAnalyticsV2.Types.CloudWatchLoggingOptionUpdate
import Network.AWS.KinesisAnalyticsV2.Types.CodeContent
import Network.AWS.KinesisAnalyticsV2.Types.CodeContentDescription
import Network.AWS.KinesisAnalyticsV2.Types.CodeContentType
import Network.AWS.KinesisAnalyticsV2.Types.CodeContentUpdate
import Network.AWS.KinesisAnalyticsV2.Types.ConfigurationType
import Network.AWS.KinesisAnalyticsV2.Types.CustomArtifactConfiguration
import Network.AWS.KinesisAnalyticsV2.Types.CustomArtifactConfigurationDescription
import Network.AWS.KinesisAnalyticsV2.Types.DeployAsApplicationConfiguration
import Network.AWS.KinesisAnalyticsV2.Types.DeployAsApplicationConfigurationDescription
import Network.AWS.KinesisAnalyticsV2.Types.DeployAsApplicationConfigurationUpdate
import Network.AWS.KinesisAnalyticsV2.Types.DestinationSchema
import Network.AWS.KinesisAnalyticsV2.Types.EnvironmentProperties
import Network.AWS.KinesisAnalyticsV2.Types.EnvironmentPropertyDescriptions
import Network.AWS.KinesisAnalyticsV2.Types.EnvironmentPropertyUpdates
import Network.AWS.KinesisAnalyticsV2.Types.FlinkApplicationConfiguration
import Network.AWS.KinesisAnalyticsV2.Types.FlinkApplicationConfigurationDescription
import Network.AWS.KinesisAnalyticsV2.Types.FlinkApplicationConfigurationUpdate
import Network.AWS.KinesisAnalyticsV2.Types.FlinkRunConfiguration
import Network.AWS.KinesisAnalyticsV2.Types.GlueDataCatalogConfiguration
import Network.AWS.KinesisAnalyticsV2.Types.GlueDataCatalogConfigurationDescription
import Network.AWS.KinesisAnalyticsV2.Types.GlueDataCatalogConfigurationUpdate
import Network.AWS.KinesisAnalyticsV2.Types.Input
import Network.AWS.KinesisAnalyticsV2.Types.InputDescription
import Network.AWS.KinesisAnalyticsV2.Types.InputLambdaProcessor
import Network.AWS.KinesisAnalyticsV2.Types.InputLambdaProcessorDescription
import Network.AWS.KinesisAnalyticsV2.Types.InputLambdaProcessorUpdate
import Network.AWS.KinesisAnalyticsV2.Types.InputParallelism
import Network.AWS.KinesisAnalyticsV2.Types.InputParallelismUpdate
import Network.AWS.KinesisAnalyticsV2.Types.InputProcessingConfiguration
import Network.AWS.KinesisAnalyticsV2.Types.InputProcessingConfigurationDescription
import Network.AWS.KinesisAnalyticsV2.Types.InputProcessingConfigurationUpdate
import Network.AWS.KinesisAnalyticsV2.Types.InputSchemaUpdate
import Network.AWS.KinesisAnalyticsV2.Types.InputStartingPosition
import Network.AWS.KinesisAnalyticsV2.Types.InputStartingPositionConfiguration
import Network.AWS.KinesisAnalyticsV2.Types.InputUpdate
import Network.AWS.KinesisAnalyticsV2.Types.JSONMappingParameters
import Network.AWS.KinesisAnalyticsV2.Types.KinesisFirehoseInput
import Network.AWS.KinesisAnalyticsV2.Types.KinesisFirehoseInputDescription
import Network.AWS.KinesisAnalyticsV2.Types.KinesisFirehoseInputUpdate
import Network.AWS.KinesisAnalyticsV2.Types.KinesisFirehoseOutput
import Network.AWS.KinesisAnalyticsV2.Types.KinesisFirehoseOutputDescription
import Network.AWS.KinesisAnalyticsV2.Types.KinesisFirehoseOutputUpdate
import Network.AWS.KinesisAnalyticsV2.Types.KinesisStreamsInput
import Network.AWS.KinesisAnalyticsV2.Types.KinesisStreamsInputDescription
import Network.AWS.KinesisAnalyticsV2.Types.KinesisStreamsInputUpdate
import Network.AWS.KinesisAnalyticsV2.Types.KinesisStreamsOutput
import Network.AWS.KinesisAnalyticsV2.Types.KinesisStreamsOutputDescription
import Network.AWS.KinesisAnalyticsV2.Types.KinesisStreamsOutputUpdate
import Network.AWS.KinesisAnalyticsV2.Types.LambdaOutput
import Network.AWS.KinesisAnalyticsV2.Types.LambdaOutputDescription
import Network.AWS.KinesisAnalyticsV2.Types.LambdaOutputUpdate
import Network.AWS.KinesisAnalyticsV2.Types.LogLevel
import Network.AWS.KinesisAnalyticsV2.Types.MappingParameters
import Network.AWS.KinesisAnalyticsV2.Types.MavenReference
import Network.AWS.KinesisAnalyticsV2.Types.MetricsLevel
import Network.AWS.KinesisAnalyticsV2.Types.MonitoringConfiguration
import Network.AWS.KinesisAnalyticsV2.Types.MonitoringConfigurationDescription
import Network.AWS.KinesisAnalyticsV2.Types.MonitoringConfigurationUpdate
import Network.AWS.KinesisAnalyticsV2.Types.Output
import Network.AWS.KinesisAnalyticsV2.Types.OutputDescription
import Network.AWS.KinesisAnalyticsV2.Types.OutputUpdate
import Network.AWS.KinesisAnalyticsV2.Types.ParallelismConfiguration
import Network.AWS.KinesisAnalyticsV2.Types.ParallelismConfigurationDescription
import Network.AWS.KinesisAnalyticsV2.Types.ParallelismConfigurationUpdate
import Network.AWS.KinesisAnalyticsV2.Types.PropertyGroup
import Network.AWS.KinesisAnalyticsV2.Types.RecordColumn
import Network.AWS.KinesisAnalyticsV2.Types.RecordFormat
import Network.AWS.KinesisAnalyticsV2.Types.RecordFormatType
import Network.AWS.KinesisAnalyticsV2.Types.ReferenceDataSource
import Network.AWS.KinesisAnalyticsV2.Types.ReferenceDataSourceDescription
import Network.AWS.KinesisAnalyticsV2.Types.ReferenceDataSourceUpdate
import Network.AWS.KinesisAnalyticsV2.Types.RunConfiguration
import Network.AWS.KinesisAnalyticsV2.Types.RunConfigurationDescription
import Network.AWS.KinesisAnalyticsV2.Types.RunConfigurationUpdate
import Network.AWS.KinesisAnalyticsV2.Types.RuntimeEnvironment
import Network.AWS.KinesisAnalyticsV2.Types.S3ApplicationCodeLocationDescription
import Network.AWS.KinesisAnalyticsV2.Types.S3Configuration
import Network.AWS.KinesisAnalyticsV2.Types.S3ContentBaseLocation
import Network.AWS.KinesisAnalyticsV2.Types.S3ContentBaseLocationDescription
import Network.AWS.KinesisAnalyticsV2.Types.S3ContentBaseLocationUpdate
import Network.AWS.KinesisAnalyticsV2.Types.S3ContentLocation
import Network.AWS.KinesisAnalyticsV2.Types.S3ContentLocationUpdate
import Network.AWS.KinesisAnalyticsV2.Types.S3ReferenceDataSource
import Network.AWS.KinesisAnalyticsV2.Types.S3ReferenceDataSourceDescription
import Network.AWS.KinesisAnalyticsV2.Types.S3ReferenceDataSourceUpdate
import Network.AWS.KinesisAnalyticsV2.Types.SnapshotDetails
import Network.AWS.KinesisAnalyticsV2.Types.SnapshotStatus
import Network.AWS.KinesisAnalyticsV2.Types.SourceSchema
import Network.AWS.KinesisAnalyticsV2.Types.SqlApplicationConfiguration
import Network.AWS.KinesisAnalyticsV2.Types.SqlApplicationConfigurationDescription
import Network.AWS.KinesisAnalyticsV2.Types.SqlApplicationConfigurationUpdate
import Network.AWS.KinesisAnalyticsV2.Types.SqlRunConfiguration
import Network.AWS.KinesisAnalyticsV2.Types.Tag
import Network.AWS.KinesisAnalyticsV2.Types.UrlType
import Network.AWS.KinesisAnalyticsV2.Types.VpcConfiguration
import Network.AWS.KinesisAnalyticsV2.Types.VpcConfigurationDescription
import Network.AWS.KinesisAnalyticsV2.Types.VpcConfigurationUpdate
import Network.AWS.KinesisAnalyticsV2.Types.ZeppelinApplicationConfiguration
import Network.AWS.KinesisAnalyticsV2.Types.ZeppelinApplicationConfigurationDescription
import Network.AWS.KinesisAnalyticsV2.Types.ZeppelinApplicationConfigurationUpdate
import Network.AWS.KinesisAnalyticsV2.Types.ZeppelinMonitoringConfiguration
import Network.AWS.KinesisAnalyticsV2.Types.ZeppelinMonitoringConfigurationDescription
import Network.AWS.KinesisAnalyticsV2.Types.ZeppelinMonitoringConfigurationUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-05-23@ of the Amazon Kinesis Analytics SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "KinesisAnalyticsV2",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "kinesisanalytics",
      Core._serviceSigningName = "kinesisanalytics",
      Core._serviceVersion = "2018-05-23",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "KinesisAnalyticsV2",
      Core._serviceRetry = retry
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
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The user-provided application configuration is not valid.
_InvalidApplicationConfigurationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidApplicationConfigurationException =
  Core._MatchServiceError
    defaultService
    "InvalidApplicationConfigurationException"

-- | Discovery failed to get a record from the streaming source because of
-- the Kinesis Streams @ProvisionedThroughputExceededException@. For more
-- information, see
-- <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetRecords.html GetRecords>
-- in the Amazon Kinesis Streams API Reference.
_ResourceProvisionedThroughputExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceProvisionedThroughputExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceProvisionedThroughputExceededException"

-- | The request JSON is not valid for the operation.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | The request was rejected because a specified parameter is not supported
-- or a specified resource is not valid for this operation.
_UnsupportedOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperationException"

-- | The specified input parameter value is not valid.
_InvalidArgumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArgumentException =
  Core._MatchServiceError
    defaultService
    "InvalidArgumentException"

-- | Application created with too many tags, or too many tags added to an
-- application. Note that the maximum number of application tags includes
-- system tags. The maximum number of user-defined application tags is 50.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The user-provided application code (query) is not valid. This can be a
-- simple syntax error.
_CodeValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CodeValidationException =
  Core._MatchServiceError
    defaultService
    "CodeValidationException"

-- | Exception thrown as a result of concurrent modifications to an
-- application. This error can be the result of attempting to modify an
-- application without using the current application ID.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The service cannot complete the request.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"

-- | The data format is not valid. Kinesis Data Analytics cannot detect the
-- schema for the given streaming source.
_UnableToDetectSchemaException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnableToDetectSchemaException =
  Core._MatchServiceError
    defaultService
    "UnableToDetectSchemaException"

-- | Specified application can\'t be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The number of allowed resources has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The application is not available for this operation.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

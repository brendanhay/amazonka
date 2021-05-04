{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Lens
  ( -- * Operations

    -- ** DeleteApplicationOutput
    deleteApplicationOutput_applicationName,
    deleteApplicationOutput_currentApplicationVersionId,
    deleteApplicationOutput_outputId,
    deleteApplicationOutputResponse_httpStatus,

    -- ** AddApplicationOutput
    addApplicationOutput_applicationName,
    addApplicationOutput_currentApplicationVersionId,
    addApplicationOutput_output,
    addApplicationOutputResponse_httpStatus,

    -- ** CreateApplication
    createApplication_applicationCode,
    createApplication_applicationDescription,
    createApplication_outputs,
    createApplication_cloudWatchLoggingOptions,
    createApplication_tags,
    createApplication_inputs,
    createApplication_applicationName,
    createApplicationResponse_httpStatus,
    createApplicationResponse_applicationSummary,

    -- ** DeleteApplicationCloudWatchLoggingOption
    deleteApplicationCloudWatchLoggingOption_applicationName,
    deleteApplicationCloudWatchLoggingOption_currentApplicationVersionId,
    deleteApplicationCloudWatchLoggingOption_cloudWatchLoggingOptionId,
    deleteApplicationCloudWatchLoggingOptionResponse_httpStatus,

    -- ** AddApplicationCloudWatchLoggingOption
    addApplicationCloudWatchLoggingOption_applicationName,
    addApplicationCloudWatchLoggingOption_currentApplicationVersionId,
    addApplicationCloudWatchLoggingOption_cloudWatchLoggingOption,
    addApplicationCloudWatchLoggingOptionResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** StopApplication
    stopApplication_applicationName,
    stopApplicationResponse_httpStatus,

    -- ** StartApplication
    startApplication_applicationName,
    startApplication_inputConfigurations,
    startApplicationResponse_httpStatus,

    -- ** DescribeApplication
    describeApplication_applicationName,
    describeApplicationResponse_httpStatus,
    describeApplicationResponse_applicationDetail,

    -- ** DiscoverInputSchema
    discoverInputSchema_resourceARN,
    discoverInputSchema_inputStartingPositionConfiguration,
    discoverInputSchema_roleARN,
    discoverInputSchema_s3Configuration,
    discoverInputSchema_inputProcessingConfiguration,
    discoverInputSchemaResponse_inputSchema,
    discoverInputSchemaResponse_processedInputRecords,
    discoverInputSchemaResponse_rawInputRecords,
    discoverInputSchemaResponse_parsedInputRecords,
    discoverInputSchemaResponse_httpStatus,

    -- ** AddApplicationInputProcessingConfiguration
    addApplicationInputProcessingConfiguration_applicationName,
    addApplicationInputProcessingConfiguration_currentApplicationVersionId,
    addApplicationInputProcessingConfiguration_inputId,
    addApplicationInputProcessingConfiguration_inputProcessingConfiguration,
    addApplicationInputProcessingConfigurationResponse_httpStatus,

    -- ** DeleteApplicationInputProcessingConfiguration
    deleteApplicationInputProcessingConfiguration_applicationName,
    deleteApplicationInputProcessingConfiguration_currentApplicationVersionId,
    deleteApplicationInputProcessingConfiguration_inputId,
    deleteApplicationInputProcessingConfigurationResponse_httpStatus,

    -- ** DeleteApplication
    deleteApplication_applicationName,
    deleteApplication_createTimestamp,
    deleteApplicationResponse_httpStatus,

    -- ** ListApplications
    listApplications_exclusiveStartApplicationName,
    listApplications_limit,
    listApplicationsResponse_httpStatus,
    listApplicationsResponse_applicationSummaries,
    listApplicationsResponse_hasMoreApplications,

    -- ** UpdateApplication
    updateApplication_applicationName,
    updateApplication_currentApplicationVersionId,
    updateApplication_applicationUpdate,
    updateApplicationResponse_httpStatus,

    -- ** AddApplicationInput
    addApplicationInput_applicationName,
    addApplicationInput_currentApplicationVersionId,
    addApplicationInput_input,
    addApplicationInputResponse_httpStatus,

    -- ** AddApplicationReferenceDataSource
    addApplicationReferenceDataSource_applicationName,
    addApplicationReferenceDataSource_currentApplicationVersionId,
    addApplicationReferenceDataSource_referenceDataSource,
    addApplicationReferenceDataSourceResponse_httpStatus,

    -- ** DeleteApplicationReferenceDataSource
    deleteApplicationReferenceDataSource_applicationName,
    deleteApplicationReferenceDataSource_currentApplicationVersionId,
    deleteApplicationReferenceDataSource_referenceId,
    deleteApplicationReferenceDataSourceResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- * Types

    -- ** ApplicationDetail
    applicationDetail_applicationCode,
    applicationDetail_applicationDescription,
    applicationDetail_cloudWatchLoggingOptionDescriptions,
    applicationDetail_createTimestamp,
    applicationDetail_outputDescriptions,
    applicationDetail_referenceDataSourceDescriptions,
    applicationDetail_inputDescriptions,
    applicationDetail_lastUpdateTimestamp,
    applicationDetail_applicationName,
    applicationDetail_applicationARN,
    applicationDetail_applicationStatus,
    applicationDetail_applicationVersionId,

    -- ** ApplicationSummary
    applicationSummary_applicationName,
    applicationSummary_applicationARN,
    applicationSummary_applicationStatus,

    -- ** ApplicationUpdate
    applicationUpdate_referenceDataSourceUpdates,
    applicationUpdate_inputUpdates,
    applicationUpdate_cloudWatchLoggingOptionUpdates,
    applicationUpdate_applicationCodeUpdate,
    applicationUpdate_outputUpdates,

    -- ** CSVMappingParameters
    cSVMappingParameters_recordRowDelimiter,
    cSVMappingParameters_recordColumnDelimiter,

    -- ** CloudWatchLoggingOption
    cloudWatchLoggingOption_logStreamARN,
    cloudWatchLoggingOption_roleARN,

    -- ** CloudWatchLoggingOptionDescription
    cloudWatchLoggingOptionDescription_cloudWatchLoggingOptionId,
    cloudWatchLoggingOptionDescription_logStreamARN,
    cloudWatchLoggingOptionDescription_roleARN,

    -- ** CloudWatchLoggingOptionUpdate
    cloudWatchLoggingOptionUpdate_logStreamARNUpdate,
    cloudWatchLoggingOptionUpdate_roleARNUpdate,
    cloudWatchLoggingOptionUpdate_cloudWatchLoggingOptionId,

    -- ** DestinationSchema
    destinationSchema_recordFormatType,

    -- ** Input
    input_inputParallelism,
    input_kinesisFirehoseInput,
    input_kinesisStreamsInput,
    input_inputProcessingConfiguration,
    input_namePrefix,
    input_inputSchema,

    -- ** InputConfiguration
    inputConfiguration_id,
    inputConfiguration_inputStartingPositionConfiguration,

    -- ** InputDescription
    inputDescription_inputSchema,
    inputDescription_inputStartingPositionConfiguration,
    inputDescription_inputProcessingConfigurationDescription,
    inputDescription_inputParallelism,
    inputDescription_namePrefix,
    inputDescription_kinesisStreamsInputDescription,
    inputDescription_kinesisFirehoseInputDescription,
    inputDescription_inAppStreamNames,
    inputDescription_inputId,

    -- ** InputLambdaProcessor
    inputLambdaProcessor_resourceARN,
    inputLambdaProcessor_roleARN,

    -- ** InputLambdaProcessorDescription
    inputLambdaProcessorDescription_resourceARN,
    inputLambdaProcessorDescription_roleARN,

    -- ** InputLambdaProcessorUpdate
    inputLambdaProcessorUpdate_resourceARNUpdate,
    inputLambdaProcessorUpdate_roleARNUpdate,

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
    inputSchemaUpdate_recordFormatUpdate,
    inputSchemaUpdate_recordColumnUpdates,
    inputSchemaUpdate_recordEncodingUpdate,

    -- ** InputStartingPositionConfiguration
    inputStartingPositionConfiguration_inputStartingPosition,

    -- ** InputUpdate
    inputUpdate_namePrefixUpdate,
    inputUpdate_kinesisFirehoseInputUpdate,
    inputUpdate_kinesisStreamsInputUpdate,
    inputUpdate_inputProcessingConfigurationUpdate,
    inputUpdate_inputSchemaUpdate,
    inputUpdate_inputParallelismUpdate,
    inputUpdate_inputId,

    -- ** JSONMappingParameters
    jSONMappingParameters_recordRowPath,

    -- ** KinesisFirehoseInput
    kinesisFirehoseInput_resourceARN,
    kinesisFirehoseInput_roleARN,

    -- ** KinesisFirehoseInputDescription
    kinesisFirehoseInputDescription_resourceARN,
    kinesisFirehoseInputDescription_roleARN,

    -- ** KinesisFirehoseInputUpdate
    kinesisFirehoseInputUpdate_resourceARNUpdate,
    kinesisFirehoseInputUpdate_roleARNUpdate,

    -- ** KinesisFirehoseOutput
    kinesisFirehoseOutput_resourceARN,
    kinesisFirehoseOutput_roleARN,

    -- ** KinesisFirehoseOutputDescription
    kinesisFirehoseOutputDescription_resourceARN,
    kinesisFirehoseOutputDescription_roleARN,

    -- ** KinesisFirehoseOutputUpdate
    kinesisFirehoseOutputUpdate_resourceARNUpdate,
    kinesisFirehoseOutputUpdate_roleARNUpdate,

    -- ** KinesisStreamsInput
    kinesisStreamsInput_resourceARN,
    kinesisStreamsInput_roleARN,

    -- ** KinesisStreamsInputDescription
    kinesisStreamsInputDescription_resourceARN,
    kinesisStreamsInputDescription_roleARN,

    -- ** KinesisStreamsInputUpdate
    kinesisStreamsInputUpdate_resourceARNUpdate,
    kinesisStreamsInputUpdate_roleARNUpdate,

    -- ** KinesisStreamsOutput
    kinesisStreamsOutput_resourceARN,
    kinesisStreamsOutput_roleARN,

    -- ** KinesisStreamsOutputDescription
    kinesisStreamsOutputDescription_resourceARN,
    kinesisStreamsOutputDescription_roleARN,

    -- ** KinesisStreamsOutputUpdate
    kinesisStreamsOutputUpdate_resourceARNUpdate,
    kinesisStreamsOutputUpdate_roleARNUpdate,

    -- ** LambdaOutput
    lambdaOutput_resourceARN,
    lambdaOutput_roleARN,

    -- ** LambdaOutputDescription
    lambdaOutputDescription_resourceARN,
    lambdaOutputDescription_roleARN,

    -- ** LambdaOutputUpdate
    lambdaOutputUpdate_resourceARNUpdate,
    lambdaOutputUpdate_roleARNUpdate,

    -- ** MappingParameters
    mappingParameters_jSONMappingParameters,
    mappingParameters_cSVMappingParameters,

    -- ** Output
    output_lambdaOutput,
    output_kinesisFirehoseOutput,
    output_kinesisStreamsOutput,
    output_name,
    output_destinationSchema,

    -- ** OutputDescription
    outputDescription_kinesisStreamsOutputDescription,
    outputDescription_kinesisFirehoseOutputDescription,
    outputDescription_destinationSchema,
    outputDescription_outputId,
    outputDescription_name,
    outputDescription_lambdaOutputDescription,

    -- ** OutputUpdate
    outputUpdate_kinesisFirehoseOutputUpdate,
    outputUpdate_destinationSchemaUpdate,
    outputUpdate_kinesisStreamsOutputUpdate,
    outputUpdate_nameUpdate,
    outputUpdate_lambdaOutputUpdate,
    outputUpdate_outputId,

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
    referenceDataSourceUpdate_s3ReferenceDataSourceUpdate,
    referenceDataSourceUpdate_referenceSchemaUpdate,
    referenceDataSourceUpdate_tableNameUpdate,
    referenceDataSourceUpdate_referenceId,

    -- ** S3Configuration
    s3Configuration_roleARN,
    s3Configuration_bucketARN,
    s3Configuration_fileKey,

    -- ** S3ReferenceDataSource
    s3ReferenceDataSource_bucketARN,
    s3ReferenceDataSource_fileKey,
    s3ReferenceDataSource_referenceRoleARN,

    -- ** S3ReferenceDataSourceDescription
    s3ReferenceDataSourceDescription_bucketARN,
    s3ReferenceDataSourceDescription_fileKey,
    s3ReferenceDataSourceDescription_referenceRoleARN,

    -- ** S3ReferenceDataSourceUpdate
    s3ReferenceDataSourceUpdate_referenceRoleARNUpdate,
    s3ReferenceDataSourceUpdate_fileKeyUpdate,
    s3ReferenceDataSourceUpdate_bucketARNUpdate,

    -- ** SourceSchema
    sourceSchema_recordEncoding,
    sourceSchema_recordFormat,
    sourceSchema_recordColumns,

    -- ** Tag
    tag_value,
    tag_key,
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
import Network.AWS.KinesisAnalytics.Types.ApplicationDetail
import Network.AWS.KinesisAnalytics.Types.ApplicationSummary
import Network.AWS.KinesisAnalytics.Types.ApplicationUpdate
import Network.AWS.KinesisAnalytics.Types.CSVMappingParameters
import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOption
import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionDescription
import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionUpdate
import Network.AWS.KinesisAnalytics.Types.DestinationSchema
import Network.AWS.KinesisAnalytics.Types.Input
import Network.AWS.KinesisAnalytics.Types.InputConfiguration
import Network.AWS.KinesisAnalytics.Types.InputDescription
import Network.AWS.KinesisAnalytics.Types.InputLambdaProcessor
import Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorDescription
import Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorUpdate
import Network.AWS.KinesisAnalytics.Types.InputParallelism
import Network.AWS.KinesisAnalytics.Types.InputParallelismUpdate
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfiguration
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationDescription
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationUpdate
import Network.AWS.KinesisAnalytics.Types.InputSchemaUpdate
import Network.AWS.KinesisAnalytics.Types.InputStartingPositionConfiguration
import Network.AWS.KinesisAnalytics.Types.InputUpdate
import Network.AWS.KinesisAnalytics.Types.JSONMappingParameters
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInput
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputDescription
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutput
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputDescription
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInput
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputDescription
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutput
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputDescription
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputUpdate
import Network.AWS.KinesisAnalytics.Types.LambdaOutput
import Network.AWS.KinesisAnalytics.Types.LambdaOutputDescription
import Network.AWS.KinesisAnalytics.Types.LambdaOutputUpdate
import Network.AWS.KinesisAnalytics.Types.MappingParameters
import Network.AWS.KinesisAnalytics.Types.Output
import Network.AWS.KinesisAnalytics.Types.OutputDescription
import Network.AWS.KinesisAnalytics.Types.OutputUpdate
import Network.AWS.KinesisAnalytics.Types.RecordColumn
import Network.AWS.KinesisAnalytics.Types.RecordFormat
import Network.AWS.KinesisAnalytics.Types.ReferenceDataSource
import Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceDescription
import Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceUpdate
import Network.AWS.KinesisAnalytics.Types.S3Configuration
import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSource
import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceDescription
import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceUpdate
import Network.AWS.KinesisAnalytics.Types.SourceSchema
import Network.AWS.KinesisAnalytics.Types.Tag
import Network.AWS.KinesisAnalytics.UntagResource
import Network.AWS.KinesisAnalytics.UpdateApplication

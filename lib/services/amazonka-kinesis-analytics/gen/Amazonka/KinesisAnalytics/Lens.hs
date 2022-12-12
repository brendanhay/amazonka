{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KinesisAnalytics.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Lens
  ( -- * Operations

    -- ** AddApplicationCloudWatchLoggingOption
    addApplicationCloudWatchLoggingOption_applicationName,
    addApplicationCloudWatchLoggingOption_currentApplicationVersionId,
    addApplicationCloudWatchLoggingOption_cloudWatchLoggingOption,
    addApplicationCloudWatchLoggingOptionResponse_httpStatus,

    -- ** AddApplicationInput
    addApplicationInput_applicationName,
    addApplicationInput_currentApplicationVersionId,
    addApplicationInput_input,
    addApplicationInputResponse_httpStatus,

    -- ** AddApplicationInputProcessingConfiguration
    addApplicationInputProcessingConfiguration_applicationName,
    addApplicationInputProcessingConfiguration_currentApplicationVersionId,
    addApplicationInputProcessingConfiguration_inputId,
    addApplicationInputProcessingConfiguration_inputProcessingConfiguration,
    addApplicationInputProcessingConfigurationResponse_httpStatus,

    -- ** AddApplicationOutput
    addApplicationOutput_applicationName,
    addApplicationOutput_currentApplicationVersionId,
    addApplicationOutput_output,
    addApplicationOutputResponse_httpStatus,

    -- ** AddApplicationReferenceDataSource
    addApplicationReferenceDataSource_applicationName,
    addApplicationReferenceDataSource_currentApplicationVersionId,
    addApplicationReferenceDataSource_referenceDataSource,
    addApplicationReferenceDataSourceResponse_httpStatus,

    -- ** CreateApplication
    createApplication_applicationCode,
    createApplication_applicationDescription,
    createApplication_cloudWatchLoggingOptions,
    createApplication_inputs,
    createApplication_outputs,
    createApplication_tags,
    createApplication_applicationName,
    createApplicationResponse_httpStatus,
    createApplicationResponse_applicationSummary,

    -- ** DeleteApplication
    deleteApplication_applicationName,
    deleteApplication_createTimestamp,
    deleteApplicationResponse_httpStatus,

    -- ** DeleteApplicationCloudWatchLoggingOption
    deleteApplicationCloudWatchLoggingOption_applicationName,
    deleteApplicationCloudWatchLoggingOption_currentApplicationVersionId,
    deleteApplicationCloudWatchLoggingOption_cloudWatchLoggingOptionId,
    deleteApplicationCloudWatchLoggingOptionResponse_httpStatus,

    -- ** DeleteApplicationInputProcessingConfiguration
    deleteApplicationInputProcessingConfiguration_applicationName,
    deleteApplicationInputProcessingConfiguration_currentApplicationVersionId,
    deleteApplicationInputProcessingConfiguration_inputId,
    deleteApplicationInputProcessingConfigurationResponse_httpStatus,

    -- ** DeleteApplicationOutput
    deleteApplicationOutput_applicationName,
    deleteApplicationOutput_currentApplicationVersionId,
    deleteApplicationOutput_outputId,
    deleteApplicationOutputResponse_httpStatus,

    -- ** DeleteApplicationReferenceDataSource
    deleteApplicationReferenceDataSource_applicationName,
    deleteApplicationReferenceDataSource_currentApplicationVersionId,
    deleteApplicationReferenceDataSource_referenceId,
    deleteApplicationReferenceDataSourceResponse_httpStatus,

    -- ** DescribeApplication
    describeApplication_applicationName,
    describeApplicationResponse_httpStatus,
    describeApplicationResponse_applicationDetail,

    -- ** DiscoverInputSchema
    discoverInputSchema_inputProcessingConfiguration,
    discoverInputSchema_inputStartingPositionConfiguration,
    discoverInputSchema_resourceARN,
    discoverInputSchema_roleARN,
    discoverInputSchema_s3Configuration,
    discoverInputSchemaResponse_inputSchema,
    discoverInputSchemaResponse_parsedInputRecords,
    discoverInputSchemaResponse_processedInputRecords,
    discoverInputSchemaResponse_rawInputRecords,
    discoverInputSchemaResponse_httpStatus,

    -- ** ListApplications
    listApplications_exclusiveStartApplicationName,
    listApplications_limit,
    listApplicationsResponse_httpStatus,
    listApplicationsResponse_applicationSummaries,
    listApplicationsResponse_hasMoreApplications,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartApplication
    startApplication_applicationName,
    startApplication_inputConfigurations,
    startApplicationResponse_httpStatus,

    -- ** StopApplication
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
    updateApplication_applicationName,
    updateApplication_currentApplicationVersionId,
    updateApplication_applicationUpdate,
    updateApplicationResponse_httpStatus,

    -- * Types

    -- ** ApplicationDetail
    applicationDetail_applicationCode,
    applicationDetail_applicationDescription,
    applicationDetail_cloudWatchLoggingOptionDescriptions,
    applicationDetail_createTimestamp,
    applicationDetail_inputDescriptions,
    applicationDetail_lastUpdateTimestamp,
    applicationDetail_outputDescriptions,
    applicationDetail_referenceDataSourceDescriptions,
    applicationDetail_applicationName,
    applicationDetail_applicationARN,
    applicationDetail_applicationStatus,
    applicationDetail_applicationVersionId,

    -- ** ApplicationSummary
    applicationSummary_applicationName,
    applicationSummary_applicationARN,
    applicationSummary_applicationStatus,

    -- ** ApplicationUpdate
    applicationUpdate_applicationCodeUpdate,
    applicationUpdate_cloudWatchLoggingOptionUpdates,
    applicationUpdate_inputUpdates,
    applicationUpdate_outputUpdates,
    applicationUpdate_referenceDataSourceUpdates,

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
    input_inputProcessingConfiguration,
    input_kinesisFirehoseInput,
    input_kinesisStreamsInput,
    input_namePrefix,
    input_inputSchema,

    -- ** InputConfiguration
    inputConfiguration_id,
    inputConfiguration_inputStartingPositionConfiguration,

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
    mappingParameters_cSVMappingParameters,
    mappingParameters_jSONMappingParameters,

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
    s3ReferenceDataSourceUpdate_bucketARNUpdate,
    s3ReferenceDataSourceUpdate_fileKeyUpdate,
    s3ReferenceDataSourceUpdate_referenceRoleARNUpdate,

    -- ** SourceSchema
    sourceSchema_recordEncoding,
    sourceSchema_recordFormat,
    sourceSchema_recordColumns,

    -- ** Tag
    tag_value,
    tag_key,
  )
where

import Amazonka.KinesisAnalytics.AddApplicationCloudWatchLoggingOption
import Amazonka.KinesisAnalytics.AddApplicationInput
import Amazonka.KinesisAnalytics.AddApplicationInputProcessingConfiguration
import Amazonka.KinesisAnalytics.AddApplicationOutput
import Amazonka.KinesisAnalytics.AddApplicationReferenceDataSource
import Amazonka.KinesisAnalytics.CreateApplication
import Amazonka.KinesisAnalytics.DeleteApplication
import Amazonka.KinesisAnalytics.DeleteApplicationCloudWatchLoggingOption
import Amazonka.KinesisAnalytics.DeleteApplicationInputProcessingConfiguration
import Amazonka.KinesisAnalytics.DeleteApplicationOutput
import Amazonka.KinesisAnalytics.DeleteApplicationReferenceDataSource
import Amazonka.KinesisAnalytics.DescribeApplication
import Amazonka.KinesisAnalytics.DiscoverInputSchema
import Amazonka.KinesisAnalytics.ListApplications
import Amazonka.KinesisAnalytics.ListTagsForResource
import Amazonka.KinesisAnalytics.StartApplication
import Amazonka.KinesisAnalytics.StopApplication
import Amazonka.KinesisAnalytics.TagResource
import Amazonka.KinesisAnalytics.Types.ApplicationDetail
import Amazonka.KinesisAnalytics.Types.ApplicationSummary
import Amazonka.KinesisAnalytics.Types.ApplicationUpdate
import Amazonka.KinesisAnalytics.Types.CSVMappingParameters
import Amazonka.KinesisAnalytics.Types.CloudWatchLoggingOption
import Amazonka.KinesisAnalytics.Types.CloudWatchLoggingOptionDescription
import Amazonka.KinesisAnalytics.Types.CloudWatchLoggingOptionUpdate
import Amazonka.KinesisAnalytics.Types.DestinationSchema
import Amazonka.KinesisAnalytics.Types.Input
import Amazonka.KinesisAnalytics.Types.InputConfiguration
import Amazonka.KinesisAnalytics.Types.InputDescription
import Amazonka.KinesisAnalytics.Types.InputLambdaProcessor
import Amazonka.KinesisAnalytics.Types.InputLambdaProcessorDescription
import Amazonka.KinesisAnalytics.Types.InputLambdaProcessorUpdate
import Amazonka.KinesisAnalytics.Types.InputParallelism
import Amazonka.KinesisAnalytics.Types.InputParallelismUpdate
import Amazonka.KinesisAnalytics.Types.InputProcessingConfiguration
import Amazonka.KinesisAnalytics.Types.InputProcessingConfigurationDescription
import Amazonka.KinesisAnalytics.Types.InputProcessingConfigurationUpdate
import Amazonka.KinesisAnalytics.Types.InputSchemaUpdate
import Amazonka.KinesisAnalytics.Types.InputStartingPositionConfiguration
import Amazonka.KinesisAnalytics.Types.InputUpdate
import Amazonka.KinesisAnalytics.Types.JSONMappingParameters
import Amazonka.KinesisAnalytics.Types.KinesisFirehoseInput
import Amazonka.KinesisAnalytics.Types.KinesisFirehoseInputDescription
import Amazonka.KinesisAnalytics.Types.KinesisFirehoseInputUpdate
import Amazonka.KinesisAnalytics.Types.KinesisFirehoseOutput
import Amazonka.KinesisAnalytics.Types.KinesisFirehoseOutputDescription
import Amazonka.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate
import Amazonka.KinesisAnalytics.Types.KinesisStreamsInput
import Amazonka.KinesisAnalytics.Types.KinesisStreamsInputDescription
import Amazonka.KinesisAnalytics.Types.KinesisStreamsInputUpdate
import Amazonka.KinesisAnalytics.Types.KinesisStreamsOutput
import Amazonka.KinesisAnalytics.Types.KinesisStreamsOutputDescription
import Amazonka.KinesisAnalytics.Types.KinesisStreamsOutputUpdate
import Amazonka.KinesisAnalytics.Types.LambdaOutput
import Amazonka.KinesisAnalytics.Types.LambdaOutputDescription
import Amazonka.KinesisAnalytics.Types.LambdaOutputUpdate
import Amazonka.KinesisAnalytics.Types.MappingParameters
import Amazonka.KinesisAnalytics.Types.Output
import Amazonka.KinesisAnalytics.Types.OutputDescription
import Amazonka.KinesisAnalytics.Types.OutputUpdate
import Amazonka.KinesisAnalytics.Types.RecordColumn
import Amazonka.KinesisAnalytics.Types.RecordFormat
import Amazonka.KinesisAnalytics.Types.ReferenceDataSource
import Amazonka.KinesisAnalytics.Types.ReferenceDataSourceDescription
import Amazonka.KinesisAnalytics.Types.ReferenceDataSourceUpdate
import Amazonka.KinesisAnalytics.Types.S3Configuration
import Amazonka.KinesisAnalytics.Types.S3ReferenceDataSource
import Amazonka.KinesisAnalytics.Types.S3ReferenceDataSourceDescription
import Amazonka.KinesisAnalytics.Types.S3ReferenceDataSourceUpdate
import Amazonka.KinesisAnalytics.Types.SourceSchema
import Amazonka.KinesisAnalytics.Types.Tag
import Amazonka.KinesisAnalytics.UntagResource
import Amazonka.KinesisAnalytics.UpdateApplication

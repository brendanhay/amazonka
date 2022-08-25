{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LookoutEquipment.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Lens
  ( -- * Operations

    -- ** CreateDataset
    createDataset_tags,
    createDataset_serverSideKmsKeyId,
    createDataset_datasetSchema,
    createDataset_datasetName,
    createDataset_clientToken,
    createDatasetResponse_datasetName,
    createDatasetResponse_status,
    createDatasetResponse_datasetArn,
    createDatasetResponse_httpStatus,

    -- ** CreateInferenceScheduler
    createInferenceScheduler_tags,
    createInferenceScheduler_serverSideKmsKeyId,
    createInferenceScheduler_dataDelayOffsetInMinutes,
    createInferenceScheduler_modelName,
    createInferenceScheduler_inferenceSchedulerName,
    createInferenceScheduler_dataUploadFrequency,
    createInferenceScheduler_dataInputConfiguration,
    createInferenceScheduler_dataOutputConfiguration,
    createInferenceScheduler_roleArn,
    createInferenceScheduler_clientToken,
    createInferenceSchedulerResponse_inferenceSchedulerName,
    createInferenceSchedulerResponse_status,
    createInferenceSchedulerResponse_inferenceSchedulerArn,
    createInferenceSchedulerResponse_httpStatus,

    -- ** CreateModel
    createModel_tags,
    createModel_serverSideKmsKeyId,
    createModel_roleArn,
    createModel_dataPreProcessingConfiguration,
    createModel_datasetSchema,
    createModel_labelsInputConfiguration,
    createModel_trainingDataStartTime,
    createModel_evaluationDataStartTime,
    createModel_trainingDataEndTime,
    createModel_evaluationDataEndTime,
    createModel_offCondition,
    createModel_modelName,
    createModel_datasetName,
    createModel_clientToken,
    createModelResponse_status,
    createModelResponse_modelArn,
    createModelResponse_httpStatus,

    -- ** DeleteDataset
    deleteDataset_datasetName,

    -- ** DeleteInferenceScheduler
    deleteInferenceScheduler_inferenceSchedulerName,

    -- ** DeleteModel
    deleteModel_modelName,

    -- ** DescribeDataIngestionJob
    describeDataIngestionJob_jobId,
    describeDataIngestionJobResponse_dataStartTime,
    describeDataIngestionJobResponse_ingestedDataSize,
    describeDataIngestionJobResponse_failedReason,
    describeDataIngestionJobResponse_roleArn,
    describeDataIngestionJobResponse_dataEndTime,
    describeDataIngestionJobResponse_jobId,
    describeDataIngestionJobResponse_statusDetail,
    describeDataIngestionJobResponse_status,
    describeDataIngestionJobResponse_datasetArn,
    describeDataIngestionJobResponse_ingestionInputConfiguration,
    describeDataIngestionJobResponse_ingestedFilesSummary,
    describeDataIngestionJobResponse_dataQualitySummary,
    describeDataIngestionJobResponse_createdAt,
    describeDataIngestionJobResponse_httpStatus,

    -- ** DescribeDataset
    describeDataset_datasetName,
    describeDatasetResponse_dataStartTime,
    describeDatasetResponse_serverSideKmsKeyId,
    describeDatasetResponse_roleArn,
    describeDatasetResponse_lastUpdatedAt,
    describeDatasetResponse_dataEndTime,
    describeDatasetResponse_datasetName,
    describeDatasetResponse_status,
    describeDatasetResponse_datasetArn,
    describeDatasetResponse_ingestionInputConfiguration,
    describeDatasetResponse_ingestedFilesSummary,
    describeDatasetResponse_schema,
    describeDatasetResponse_dataQualitySummary,
    describeDatasetResponse_createdAt,
    describeDatasetResponse_httpStatus,

    -- ** DescribeInferenceScheduler
    describeInferenceScheduler_inferenceSchedulerName,
    describeInferenceSchedulerResponse_inferenceSchedulerName,
    describeInferenceSchedulerResponse_serverSideKmsKeyId,
    describeInferenceSchedulerResponse_roleArn,
    describeInferenceSchedulerResponse_dataDelayOffsetInMinutes,
    describeInferenceSchedulerResponse_dataOutputConfiguration,
    describeInferenceSchedulerResponse_status,
    describeInferenceSchedulerResponse_modelArn,
    describeInferenceSchedulerResponse_modelName,
    describeInferenceSchedulerResponse_dataUploadFrequency,
    describeInferenceSchedulerResponse_createdAt,
    describeInferenceSchedulerResponse_inferenceSchedulerArn,
    describeInferenceSchedulerResponse_updatedAt,
    describeInferenceSchedulerResponse_dataInputConfiguration,
    describeInferenceSchedulerResponse_httpStatus,

    -- ** DescribeModel
    describeModel_modelName,
    describeModelResponse_serverSideKmsKeyId,
    describeModelResponse_failedReason,
    describeModelResponse_roleArn,
    describeModelResponse_dataPreProcessingConfiguration,
    describeModelResponse_labelsInputConfiguration,
    describeModelResponse_datasetName,
    describeModelResponse_status,
    describeModelResponse_lastUpdatedTime,
    describeModelResponse_datasetArn,
    describeModelResponse_trainingExecutionStartTime,
    describeModelResponse_trainingDataStartTime,
    describeModelResponse_modelMetrics,
    describeModelResponse_modelArn,
    describeModelResponse_modelName,
    describeModelResponse_schema,
    describeModelResponse_evaluationDataStartTime,
    describeModelResponse_trainingDataEndTime,
    describeModelResponse_createdAt,
    describeModelResponse_evaluationDataEndTime,
    describeModelResponse_offCondition,
    describeModelResponse_trainingExecutionEndTime,
    describeModelResponse_httpStatus,

    -- ** ListDataIngestionJobs
    listDataIngestionJobs_nextToken,
    listDataIngestionJobs_datasetName,
    listDataIngestionJobs_status,
    listDataIngestionJobs_maxResults,
    listDataIngestionJobsResponse_nextToken,
    listDataIngestionJobsResponse_dataIngestionJobSummaries,
    listDataIngestionJobsResponse_httpStatus,

    -- ** ListDatasets
    listDatasets_nextToken,
    listDatasets_maxResults,
    listDatasets_datasetNameBeginsWith,
    listDatasetsResponse_nextToken,
    listDatasetsResponse_datasetSummaries,
    listDatasetsResponse_httpStatus,

    -- ** ListInferenceEvents
    listInferenceEvents_nextToken,
    listInferenceEvents_maxResults,
    listInferenceEvents_inferenceSchedulerName,
    listInferenceEvents_intervalStartTime,
    listInferenceEvents_intervalEndTime,
    listInferenceEventsResponse_nextToken,
    listInferenceEventsResponse_inferenceEventSummaries,
    listInferenceEventsResponse_httpStatus,

    -- ** ListInferenceExecutions
    listInferenceExecutions_dataStartTimeAfter,
    listInferenceExecutions_nextToken,
    listInferenceExecutions_status,
    listInferenceExecutions_maxResults,
    listInferenceExecutions_dataEndTimeBefore,
    listInferenceExecutions_inferenceSchedulerName,
    listInferenceExecutionsResponse_nextToken,
    listInferenceExecutionsResponse_inferenceExecutionSummaries,
    listInferenceExecutionsResponse_httpStatus,

    -- ** ListInferenceSchedulers
    listInferenceSchedulers_nextToken,
    listInferenceSchedulers_inferenceSchedulerNameBeginsWith,
    listInferenceSchedulers_maxResults,
    listInferenceSchedulers_modelName,
    listInferenceSchedulersResponse_nextToken,
    listInferenceSchedulersResponse_inferenceSchedulerSummaries,
    listInferenceSchedulersResponse_httpStatus,

    -- ** ListModels
    listModels_nextToken,
    listModels_status,
    listModels_maxResults,
    listModels_modelNameBeginsWith,
    listModels_datasetNameBeginsWith,
    listModelsResponse_nextToken,
    listModelsResponse_modelSummaries,
    listModelsResponse_httpStatus,

    -- ** ListSensorStatistics
    listSensorStatistics_ingestionJobId,
    listSensorStatistics_nextToken,
    listSensorStatistics_maxResults,
    listSensorStatistics_datasetName,
    listSensorStatisticsResponse_sensorStatisticsSummaries,
    listSensorStatisticsResponse_nextToken,
    listSensorStatisticsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartDataIngestionJob
    startDataIngestionJob_datasetName,
    startDataIngestionJob_ingestionInputConfiguration,
    startDataIngestionJob_roleArn,
    startDataIngestionJob_clientToken,
    startDataIngestionJobResponse_jobId,
    startDataIngestionJobResponse_status,
    startDataIngestionJobResponse_httpStatus,

    -- ** StartInferenceScheduler
    startInferenceScheduler_inferenceSchedulerName,
    startInferenceSchedulerResponse_inferenceSchedulerName,
    startInferenceSchedulerResponse_status,
    startInferenceSchedulerResponse_modelArn,
    startInferenceSchedulerResponse_modelName,
    startInferenceSchedulerResponse_inferenceSchedulerArn,
    startInferenceSchedulerResponse_httpStatus,

    -- ** StopInferenceScheduler
    stopInferenceScheduler_inferenceSchedulerName,
    stopInferenceSchedulerResponse_inferenceSchedulerName,
    stopInferenceSchedulerResponse_status,
    stopInferenceSchedulerResponse_modelArn,
    stopInferenceSchedulerResponse_modelName,
    stopInferenceSchedulerResponse_inferenceSchedulerArn,
    stopInferenceSchedulerResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateInferenceScheduler
    updateInferenceScheduler_roleArn,
    updateInferenceScheduler_dataDelayOffsetInMinutes,
    updateInferenceScheduler_dataOutputConfiguration,
    updateInferenceScheduler_dataUploadFrequency,
    updateInferenceScheduler_dataInputConfiguration,
    updateInferenceScheduler_inferenceSchedulerName,

    -- * Types

    -- ** CategoricalValues
    categoricalValues_numberOfCategory,
    categoricalValues_status,

    -- ** CountPercent
    countPercent_count,
    countPercent_percentage,

    -- ** DataIngestionJobSummary
    dataIngestionJobSummary_datasetName,
    dataIngestionJobSummary_jobId,
    dataIngestionJobSummary_status,
    dataIngestionJobSummary_datasetArn,
    dataIngestionJobSummary_ingestionInputConfiguration,

    -- ** DataPreProcessingConfiguration
    dataPreProcessingConfiguration_targetSamplingRate,

    -- ** DataQualitySummary
    dataQualitySummary_insufficientSensorData,
    dataQualitySummary_missingSensorData,
    dataQualitySummary_invalidSensorData,
    dataQualitySummary_unsupportedTimestamps,
    dataQualitySummary_duplicateTimestamps,

    -- ** DatasetSchema
    datasetSchema_inlineDataSchema,

    -- ** DatasetSummary
    datasetSummary_datasetName,
    datasetSummary_status,
    datasetSummary_datasetArn,
    datasetSummary_createdAt,

    -- ** DuplicateTimestamps
    duplicateTimestamps_totalNumberOfDuplicateTimestamps,

    -- ** InferenceEventSummary
    inferenceEventSummary_inferenceSchedulerName,
    inferenceEventSummary_eventEndTime,
    inferenceEventSummary_eventDurationInSeconds,
    inferenceEventSummary_diagnostics,
    inferenceEventSummary_inferenceSchedulerArn,
    inferenceEventSummary_eventStartTime,

    -- ** InferenceExecutionSummary
    inferenceExecutionSummary_inferenceSchedulerName,
    inferenceExecutionSummary_scheduledStartTime,
    inferenceExecutionSummary_dataStartTime,
    inferenceExecutionSummary_failedReason,
    inferenceExecutionSummary_dataOutputConfiguration,
    inferenceExecutionSummary_dataEndTime,
    inferenceExecutionSummary_customerResultObject,
    inferenceExecutionSummary_status,
    inferenceExecutionSummary_modelArn,
    inferenceExecutionSummary_modelName,
    inferenceExecutionSummary_inferenceSchedulerArn,
    inferenceExecutionSummary_dataInputConfiguration,

    -- ** InferenceInputConfiguration
    inferenceInputConfiguration_inputTimeZoneOffset,
    inferenceInputConfiguration_s3InputConfiguration,
    inferenceInputConfiguration_inferenceInputNameConfiguration,

    -- ** InferenceInputNameConfiguration
    inferenceInputNameConfiguration_componentTimestampDelimiter,
    inferenceInputNameConfiguration_timestampFormat,

    -- ** InferenceOutputConfiguration
    inferenceOutputConfiguration_kmsKeyId,
    inferenceOutputConfiguration_s3OutputConfiguration,

    -- ** InferenceS3InputConfiguration
    inferenceS3InputConfiguration_prefix,
    inferenceS3InputConfiguration_bucket,

    -- ** InferenceS3OutputConfiguration
    inferenceS3OutputConfiguration_prefix,
    inferenceS3OutputConfiguration_bucket,

    -- ** InferenceSchedulerSummary
    inferenceSchedulerSummary_inferenceSchedulerName,
    inferenceSchedulerSummary_dataDelayOffsetInMinutes,
    inferenceSchedulerSummary_status,
    inferenceSchedulerSummary_modelArn,
    inferenceSchedulerSummary_modelName,
    inferenceSchedulerSummary_dataUploadFrequency,
    inferenceSchedulerSummary_inferenceSchedulerArn,

    -- ** IngestedFilesSummary
    ingestedFilesSummary_discardedFiles,
    ingestedFilesSummary_totalNumberOfFiles,
    ingestedFilesSummary_ingestedNumberOfFiles,

    -- ** IngestionInputConfiguration
    ingestionInputConfiguration_s3InputConfiguration,

    -- ** IngestionS3InputConfiguration
    ingestionS3InputConfiguration_keyPattern,
    ingestionS3InputConfiguration_prefix,
    ingestionS3InputConfiguration_bucket,

    -- ** InsufficientSensorData
    insufficientSensorData_missingCompleteSensorData,
    insufficientSensorData_sensorsWithShortDateRange,

    -- ** InvalidSensorData
    invalidSensorData_affectedSensorCount,
    invalidSensorData_totalNumberOfInvalidValues,

    -- ** LabelsInputConfiguration
    labelsInputConfiguration_s3InputConfiguration,

    -- ** LabelsS3InputConfiguration
    labelsS3InputConfiguration_prefix,
    labelsS3InputConfiguration_bucket,

    -- ** LargeTimestampGaps
    largeTimestampGaps_maxTimestampGapInDays,
    largeTimestampGaps_numberOfLargeTimestampGaps,
    largeTimestampGaps_status,

    -- ** MissingCompleteSensorData
    missingCompleteSensorData_affectedSensorCount,

    -- ** MissingSensorData
    missingSensorData_affectedSensorCount,
    missingSensorData_totalNumberOfMissingValues,

    -- ** ModelSummary
    modelSummary_datasetName,
    modelSummary_status,
    modelSummary_datasetArn,
    modelSummary_modelArn,
    modelSummary_modelName,
    modelSummary_createdAt,

    -- ** MonotonicValues
    monotonicValues_monotonicity,
    monotonicValues_status,

    -- ** MultipleOperatingModes
    multipleOperatingModes_status,

    -- ** S3Object
    s3Object_bucket,
    s3Object_key,

    -- ** SensorStatisticsSummary
    sensorStatisticsSummary_dataStartTime,
    sensorStatisticsSummary_multipleOperatingModes,
    sensorStatisticsSummary_categoricalValues,
    sensorStatisticsSummary_dataExists,
    sensorStatisticsSummary_invalidValues,
    sensorStatisticsSummary_missingValues,
    sensorStatisticsSummary_dataEndTime,
    sensorStatisticsSummary_componentName,
    sensorStatisticsSummary_duplicateTimestamps,
    sensorStatisticsSummary_largeTimestampGaps,
    sensorStatisticsSummary_monotonicValues,
    sensorStatisticsSummary_invalidDateEntries,
    sensorStatisticsSummary_sensorName,

    -- ** SensorsWithShortDateRange
    sensorsWithShortDateRange_affectedSensorCount,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** UnsupportedTimestamps
    unsupportedTimestamps_totalNumberOfUnsupportedTimestamps,
  )
where

import Amazonka.LookoutEquipment.CreateDataset
import Amazonka.LookoutEquipment.CreateInferenceScheduler
import Amazonka.LookoutEquipment.CreateModel
import Amazonka.LookoutEquipment.DeleteDataset
import Amazonka.LookoutEquipment.DeleteInferenceScheduler
import Amazonka.LookoutEquipment.DeleteModel
import Amazonka.LookoutEquipment.DescribeDataIngestionJob
import Amazonka.LookoutEquipment.DescribeDataset
import Amazonka.LookoutEquipment.DescribeInferenceScheduler
import Amazonka.LookoutEquipment.DescribeModel
import Amazonka.LookoutEquipment.ListDataIngestionJobs
import Amazonka.LookoutEquipment.ListDatasets
import Amazonka.LookoutEquipment.ListInferenceEvents
import Amazonka.LookoutEquipment.ListInferenceExecutions
import Amazonka.LookoutEquipment.ListInferenceSchedulers
import Amazonka.LookoutEquipment.ListModels
import Amazonka.LookoutEquipment.ListSensorStatistics
import Amazonka.LookoutEquipment.ListTagsForResource
import Amazonka.LookoutEquipment.StartDataIngestionJob
import Amazonka.LookoutEquipment.StartInferenceScheduler
import Amazonka.LookoutEquipment.StopInferenceScheduler
import Amazonka.LookoutEquipment.TagResource
import Amazonka.LookoutEquipment.Types.CategoricalValues
import Amazonka.LookoutEquipment.Types.CountPercent
import Amazonka.LookoutEquipment.Types.DataIngestionJobSummary
import Amazonka.LookoutEquipment.Types.DataPreProcessingConfiguration
import Amazonka.LookoutEquipment.Types.DataQualitySummary
import Amazonka.LookoutEquipment.Types.DatasetSchema
import Amazonka.LookoutEquipment.Types.DatasetSummary
import Amazonka.LookoutEquipment.Types.DuplicateTimestamps
import Amazonka.LookoutEquipment.Types.InferenceEventSummary
import Amazonka.LookoutEquipment.Types.InferenceExecutionSummary
import Amazonka.LookoutEquipment.Types.InferenceInputConfiguration
import Amazonka.LookoutEquipment.Types.InferenceInputNameConfiguration
import Amazonka.LookoutEquipment.Types.InferenceOutputConfiguration
import Amazonka.LookoutEquipment.Types.InferenceS3InputConfiguration
import Amazonka.LookoutEquipment.Types.InferenceS3OutputConfiguration
import Amazonka.LookoutEquipment.Types.InferenceSchedulerSummary
import Amazonka.LookoutEquipment.Types.IngestedFilesSummary
import Amazonka.LookoutEquipment.Types.IngestionInputConfiguration
import Amazonka.LookoutEquipment.Types.IngestionS3InputConfiguration
import Amazonka.LookoutEquipment.Types.InsufficientSensorData
import Amazonka.LookoutEquipment.Types.InvalidSensorData
import Amazonka.LookoutEquipment.Types.LabelsInputConfiguration
import Amazonka.LookoutEquipment.Types.LabelsS3InputConfiguration
import Amazonka.LookoutEquipment.Types.LargeTimestampGaps
import Amazonka.LookoutEquipment.Types.MissingCompleteSensorData
import Amazonka.LookoutEquipment.Types.MissingSensorData
import Amazonka.LookoutEquipment.Types.ModelSummary
import Amazonka.LookoutEquipment.Types.MonotonicValues
import Amazonka.LookoutEquipment.Types.MultipleOperatingModes
import Amazonka.LookoutEquipment.Types.S3Object
import Amazonka.LookoutEquipment.Types.SensorStatisticsSummary
import Amazonka.LookoutEquipment.Types.SensorsWithShortDateRange
import Amazonka.LookoutEquipment.Types.Tag
import Amazonka.LookoutEquipment.Types.UnsupportedTimestamps
import Amazonka.LookoutEquipment.UntagResource
import Amazonka.LookoutEquipment.UpdateInferenceScheduler

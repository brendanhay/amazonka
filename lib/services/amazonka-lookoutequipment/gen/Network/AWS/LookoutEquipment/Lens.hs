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

    -- ** StartInferenceScheduler
    startInferenceScheduler_inferenceSchedulerName,
    startInferenceSchedulerResponse_status,
    startInferenceSchedulerResponse_modelArn,
    startInferenceSchedulerResponse_modelName,
    startInferenceSchedulerResponse_inferenceSchedulerArn,
    startInferenceSchedulerResponse_inferenceSchedulerName,
    startInferenceSchedulerResponse_httpStatus,

    -- ** DescribeDataset
    describeDataset_datasetName,
    describeDatasetResponse_ingestionInputConfiguration,
    describeDatasetResponse_status,
    describeDatasetResponse_datasetArn,
    describeDatasetResponse_lastUpdatedAt,
    describeDatasetResponse_createdAt,
    describeDatasetResponse_schema,
    describeDatasetResponse_datasetName,
    describeDatasetResponse_serverSideKmsKeyId,
    describeDatasetResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DescribeDataIngestionJob
    describeDataIngestionJob_jobId,
    describeDataIngestionJobResponse_ingestionInputConfiguration,
    describeDataIngestionJobResponse_status,
    describeDataIngestionJobResponse_datasetArn,
    describeDataIngestionJobResponse_failedReason,
    describeDataIngestionJobResponse_jobId,
    describeDataIngestionJobResponse_createdAt,
    describeDataIngestionJobResponse_roleArn,
    describeDataIngestionJobResponse_httpStatus,

    -- ** CreateModel
    createModel_dataPreProcessingConfiguration,
    createModel_trainingDataEndTime,
    createModel_datasetSchema,
    createModel_evaluationDataStartTime,
    createModel_offCondition,
    createModel_evaluationDataEndTime,
    createModel_trainingDataStartTime,
    createModel_labelsInputConfiguration,
    createModel_tags,
    createModel_serverSideKmsKeyId,
    createModel_roleArn,
    createModel_modelName,
    createModel_datasetName,
    createModel_clientToken,
    createModelResponse_status,
    createModelResponse_modelArn,
    createModelResponse_httpStatus,

    -- ** DeleteDataset
    deleteDataset_datasetName,

    -- ** CreateDataset
    createDataset_tags,
    createDataset_serverSideKmsKeyId,
    createDataset_datasetName,
    createDataset_datasetSchema,
    createDataset_clientToken,
    createDatasetResponse_status,
    createDatasetResponse_datasetArn,
    createDatasetResponse_datasetName,
    createDatasetResponse_httpStatus,

    -- ** DeleteModel
    deleteModel_modelName,

    -- ** ListModels
    listModels_status,
    listModels_nextToken,
    listModels_datasetNameBeginsWith,
    listModels_modelNameBeginsWith,
    listModels_maxResults,
    listModelsResponse_nextToken,
    listModelsResponse_modelSummaries,
    listModelsResponse_httpStatus,

    -- ** StopInferenceScheduler
    stopInferenceScheduler_inferenceSchedulerName,
    stopInferenceSchedulerResponse_status,
    stopInferenceSchedulerResponse_modelArn,
    stopInferenceSchedulerResponse_modelName,
    stopInferenceSchedulerResponse_inferenceSchedulerArn,
    stopInferenceSchedulerResponse_inferenceSchedulerName,
    stopInferenceSchedulerResponse_httpStatus,

    -- ** ListDataIngestionJobs
    listDataIngestionJobs_status,
    listDataIngestionJobs_nextToken,
    listDataIngestionJobs_datasetName,
    listDataIngestionJobs_maxResults,
    listDataIngestionJobsResponse_nextToken,
    listDataIngestionJobsResponse_dataIngestionJobSummaries,
    listDataIngestionJobsResponse_httpStatus,

    -- ** DescribeModel
    describeModel_modelName,
    describeModelResponse_status,
    describeModelResponse_dataPreProcessingConfiguration,
    describeModelResponse_trainingExecutionStartTime,
    describeModelResponse_datasetArn,
    describeModelResponse_failedReason,
    describeModelResponse_modelArn,
    describeModelResponse_lastUpdatedTime,
    describeModelResponse_trainingDataEndTime,
    describeModelResponse_createdAt,
    describeModelResponse_modelName,
    describeModelResponse_modelMetrics,
    describeModelResponse_evaluationDataStartTime,
    describeModelResponse_schema,
    describeModelResponse_offCondition,
    describeModelResponse_evaluationDataEndTime,
    describeModelResponse_datasetName,
    describeModelResponse_trainingDataStartTime,
    describeModelResponse_trainingExecutionEndTime,
    describeModelResponse_labelsInputConfiguration,
    describeModelResponse_serverSideKmsKeyId,
    describeModelResponse_roleArn,
    describeModelResponse_httpStatus,

    -- ** StartDataIngestionJob
    startDataIngestionJob_datasetName,
    startDataIngestionJob_ingestionInputConfiguration,
    startDataIngestionJob_roleArn,
    startDataIngestionJob_clientToken,
    startDataIngestionJobResponse_status,
    startDataIngestionJobResponse_jobId,
    startDataIngestionJobResponse_httpStatus,

    -- ** ListInferenceSchedulers
    listInferenceSchedulers_modelName,
    listInferenceSchedulers_nextToken,
    listInferenceSchedulers_inferenceSchedulerNameBeginsWith,
    listInferenceSchedulers_maxResults,
    listInferenceSchedulersResponse_inferenceSchedulerSummaries,
    listInferenceSchedulersResponse_nextToken,
    listInferenceSchedulersResponse_httpStatus,

    -- ** UpdateInferenceScheduler
    updateInferenceScheduler_dataUploadFrequency,
    updateInferenceScheduler_dataDelayOffsetInMinutes,
    updateInferenceScheduler_dataOutputConfiguration,
    updateInferenceScheduler_dataInputConfiguration,
    updateInferenceScheduler_roleArn,
    updateInferenceScheduler_inferenceSchedulerName,

    -- ** DeleteInferenceScheduler
    deleteInferenceScheduler_inferenceSchedulerName,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListInferenceExecutions
    listInferenceExecutions_status,
    listInferenceExecutions_dataEndTimeBefore,
    listInferenceExecutions_nextToken,
    listInferenceExecutions_maxResults,
    listInferenceExecutions_dataStartTimeAfter,
    listInferenceExecutions_inferenceSchedulerName,
    listInferenceExecutionsResponse_nextToken,
    listInferenceExecutionsResponse_inferenceExecutionSummaries,
    listInferenceExecutionsResponse_httpStatus,

    -- ** CreateInferenceScheduler
    createInferenceScheduler_dataDelayOffsetInMinutes,
    createInferenceScheduler_tags,
    createInferenceScheduler_serverSideKmsKeyId,
    createInferenceScheduler_modelName,
    createInferenceScheduler_inferenceSchedulerName,
    createInferenceScheduler_dataUploadFrequency,
    createInferenceScheduler_dataInputConfiguration,
    createInferenceScheduler_dataOutputConfiguration,
    createInferenceScheduler_roleArn,
    createInferenceScheduler_clientToken,
    createInferenceSchedulerResponse_status,
    createInferenceSchedulerResponse_inferenceSchedulerArn,
    createInferenceSchedulerResponse_inferenceSchedulerName,
    createInferenceSchedulerResponse_httpStatus,

    -- ** ListDatasets
    listDatasets_nextToken,
    listDatasets_datasetNameBeginsWith,
    listDatasets_maxResults,
    listDatasetsResponse_nextToken,
    listDatasetsResponse_datasetSummaries,
    listDatasetsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DescribeInferenceScheduler
    describeInferenceScheduler_inferenceSchedulerName,
    describeInferenceSchedulerResponse_status,
    describeInferenceSchedulerResponse_dataUploadFrequency,
    describeInferenceSchedulerResponse_dataDelayOffsetInMinutes,
    describeInferenceSchedulerResponse_modelArn,
    describeInferenceSchedulerResponse_createdAt,
    describeInferenceSchedulerResponse_modelName,
    describeInferenceSchedulerResponse_inferenceSchedulerArn,
    describeInferenceSchedulerResponse_dataOutputConfiguration,
    describeInferenceSchedulerResponse_updatedAt,
    describeInferenceSchedulerResponse_inferenceSchedulerName,
    describeInferenceSchedulerResponse_dataInputConfiguration,
    describeInferenceSchedulerResponse_serverSideKmsKeyId,
    describeInferenceSchedulerResponse_roleArn,
    describeInferenceSchedulerResponse_httpStatus,

    -- * Types

    -- ** DataIngestionJobSummary
    dataIngestionJobSummary_ingestionInputConfiguration,
    dataIngestionJobSummary_status,
    dataIngestionJobSummary_datasetArn,
    dataIngestionJobSummary_jobId,
    dataIngestionJobSummary_datasetName,

    -- ** DataPreProcessingConfiguration
    dataPreProcessingConfiguration_targetSamplingRate,

    -- ** DatasetSchema
    datasetSchema_inlineDataSchema,

    -- ** DatasetSummary
    datasetSummary_status,
    datasetSummary_datasetArn,
    datasetSummary_createdAt,
    datasetSummary_datasetName,

    -- ** InferenceExecutionSummary
    inferenceExecutionSummary_status,
    inferenceExecutionSummary_failedReason,
    inferenceExecutionSummary_modelArn,
    inferenceExecutionSummary_dataStartTime,
    inferenceExecutionSummary_modelName,
    inferenceExecutionSummary_customerResultObject,
    inferenceExecutionSummary_inferenceSchedulerArn,
    inferenceExecutionSummary_scheduledStartTime,
    inferenceExecutionSummary_dataOutputConfiguration,
    inferenceExecutionSummary_dataEndTime,
    inferenceExecutionSummary_inferenceSchedulerName,
    inferenceExecutionSummary_dataInputConfiguration,

    -- ** InferenceInputConfiguration
    inferenceInputConfiguration_inputTimeZoneOffset,
    inferenceInputConfiguration_s3InputConfiguration,
    inferenceInputConfiguration_inferenceInputNameConfiguration,

    -- ** InferenceInputNameConfiguration
    inferenceInputNameConfiguration_timestampFormat,
    inferenceInputNameConfiguration_componentTimestampDelimiter,

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
    inferenceSchedulerSummary_status,
    inferenceSchedulerSummary_dataUploadFrequency,
    inferenceSchedulerSummary_dataDelayOffsetInMinutes,
    inferenceSchedulerSummary_modelArn,
    inferenceSchedulerSummary_modelName,
    inferenceSchedulerSummary_inferenceSchedulerArn,
    inferenceSchedulerSummary_inferenceSchedulerName,

    -- ** IngestionInputConfiguration
    ingestionInputConfiguration_s3InputConfiguration,

    -- ** IngestionS3InputConfiguration
    ingestionS3InputConfiguration_prefix,
    ingestionS3InputConfiguration_bucket,

    -- ** LabelsInputConfiguration
    labelsInputConfiguration_s3InputConfiguration,

    -- ** LabelsS3InputConfiguration
    labelsS3InputConfiguration_prefix,
    labelsS3InputConfiguration_bucket,

    -- ** ModelSummary
    modelSummary_status,
    modelSummary_datasetArn,
    modelSummary_modelArn,
    modelSummary_createdAt,
    modelSummary_modelName,
    modelSummary_datasetName,

    -- ** S3Object
    s3Object_bucket,
    s3Object_key,

    -- ** Tag
    tag_key,
    tag_value,
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
import Amazonka.LookoutEquipment.ListInferenceExecutions
import Amazonka.LookoutEquipment.ListInferenceSchedulers
import Amazonka.LookoutEquipment.ListModels
import Amazonka.LookoutEquipment.ListTagsForResource
import Amazonka.LookoutEquipment.StartDataIngestionJob
import Amazonka.LookoutEquipment.StartInferenceScheduler
import Amazonka.LookoutEquipment.StopInferenceScheduler
import Amazonka.LookoutEquipment.TagResource
import Amazonka.LookoutEquipment.Types.DataIngestionJobSummary
import Amazonka.LookoutEquipment.Types.DataPreProcessingConfiguration
import Amazonka.LookoutEquipment.Types.DatasetSchema
import Amazonka.LookoutEquipment.Types.DatasetSummary
import Amazonka.LookoutEquipment.Types.InferenceExecutionSummary
import Amazonka.LookoutEquipment.Types.InferenceInputConfiguration
import Amazonka.LookoutEquipment.Types.InferenceInputNameConfiguration
import Amazonka.LookoutEquipment.Types.InferenceOutputConfiguration
import Amazonka.LookoutEquipment.Types.InferenceS3InputConfiguration
import Amazonka.LookoutEquipment.Types.InferenceS3OutputConfiguration
import Amazonka.LookoutEquipment.Types.InferenceSchedulerSummary
import Amazonka.LookoutEquipment.Types.IngestionInputConfiguration
import Amazonka.LookoutEquipment.Types.IngestionS3InputConfiguration
import Amazonka.LookoutEquipment.Types.LabelsInputConfiguration
import Amazonka.LookoutEquipment.Types.LabelsS3InputConfiguration
import Amazonka.LookoutEquipment.Types.ModelSummary
import Amazonka.LookoutEquipment.Types.S3Object
import Amazonka.LookoutEquipment.Types.Tag
import Amazonka.LookoutEquipment.UntagResource
import Amazonka.LookoutEquipment.UpdateInferenceScheduler

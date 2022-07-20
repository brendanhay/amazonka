{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MachineLearning.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Lens
  ( -- * Operations

    -- ** AddTags
    addTags_tags,
    addTags_resourceId,
    addTags_resourceType,
    addTagsResponse_resourceId,
    addTagsResponse_resourceType,
    addTagsResponse_httpStatus,

    -- ** CreateBatchPrediction
    createBatchPrediction_batchPredictionName,
    createBatchPrediction_batchPredictionId,
    createBatchPrediction_mLModelId,
    createBatchPrediction_batchPredictionDataSourceId,
    createBatchPrediction_outputUri,
    createBatchPredictionResponse_batchPredictionId,
    createBatchPredictionResponse_httpStatus,

    -- ** CreateDataSourceFromRDS
    createDataSourceFromRDS_dataSourceName,
    createDataSourceFromRDS_computeStatistics,
    createDataSourceFromRDS_dataSourceId,
    createDataSourceFromRDS_rDSData,
    createDataSourceFromRDS_roleARN,
    createDataSourceFromRDSResponse_dataSourceId,
    createDataSourceFromRDSResponse_httpStatus,

    -- ** CreateDataSourceFromRedshift
    createDataSourceFromRedshift_dataSourceName,
    createDataSourceFromRedshift_computeStatistics,
    createDataSourceFromRedshift_dataSourceId,
    createDataSourceFromRedshift_dataSpec,
    createDataSourceFromRedshift_roleARN,
    createDataSourceFromRedshiftResponse_dataSourceId,
    createDataSourceFromRedshiftResponse_httpStatus,

    -- ** CreateDataSourceFromS3
    createDataSourceFromS3_dataSourceName,
    createDataSourceFromS3_computeStatistics,
    createDataSourceFromS3_dataSourceId,
    createDataSourceFromS3_dataSpec,
    createDataSourceFromS3Response_dataSourceId,
    createDataSourceFromS3Response_httpStatus,

    -- ** CreateEvaluation
    createEvaluation_evaluationName,
    createEvaluation_evaluationId,
    createEvaluation_mLModelId,
    createEvaluation_evaluationDataSourceId,
    createEvaluationResponse_evaluationId,
    createEvaluationResponse_httpStatus,

    -- ** CreateMLModel
    createMLModel_mLModelName,
    createMLModel_recipe,
    createMLModel_recipeUri,
    createMLModel_parameters,
    createMLModel_mLModelId,
    createMLModel_mLModelType,
    createMLModel_trainingDataSourceId,
    createMLModelResponse_mLModelId,
    createMLModelResponse_httpStatus,

    -- ** CreateRealtimeEndpoint
    createRealtimeEndpoint_mLModelId,
    createRealtimeEndpointResponse_realtimeEndpointInfo,
    createRealtimeEndpointResponse_mLModelId,
    createRealtimeEndpointResponse_httpStatus,

    -- ** DeleteBatchPrediction
    deleteBatchPrediction_batchPredictionId,
    deleteBatchPredictionResponse_batchPredictionId,
    deleteBatchPredictionResponse_httpStatus,

    -- ** DeleteDataSource
    deleteDataSource_dataSourceId,
    deleteDataSourceResponse_dataSourceId,
    deleteDataSourceResponse_httpStatus,

    -- ** DeleteEvaluation
    deleteEvaluation_evaluationId,
    deleteEvaluationResponse_evaluationId,
    deleteEvaluationResponse_httpStatus,

    -- ** DeleteMLModel
    deleteMLModel_mLModelId,
    deleteMLModelResponse_mLModelId,
    deleteMLModelResponse_httpStatus,

    -- ** DeleteRealtimeEndpoint
    deleteRealtimeEndpoint_mLModelId,
    deleteRealtimeEndpointResponse_realtimeEndpointInfo,
    deleteRealtimeEndpointResponse_mLModelId,
    deleteRealtimeEndpointResponse_httpStatus,

    -- ** DeleteTags
    deleteTags_tagKeys,
    deleteTags_resourceId,
    deleteTags_resourceType,
    deleteTagsResponse_resourceId,
    deleteTagsResponse_resourceType,
    deleteTagsResponse_httpStatus,

    -- ** DescribeBatchPredictions
    describeBatchPredictions_sortOrder,
    describeBatchPredictions_nextToken,
    describeBatchPredictions_filterVariable,
    describeBatchPredictions_limit,
    describeBatchPredictions_le,
    describeBatchPredictions_lt,
    describeBatchPredictions_eq,
    describeBatchPredictions_prefix,
    describeBatchPredictions_gt,
    describeBatchPredictions_ne,
    describeBatchPredictions_ge,
    describeBatchPredictionsResponse_nextToken,
    describeBatchPredictionsResponse_results,
    describeBatchPredictionsResponse_httpStatus,

    -- ** DescribeDataSources
    describeDataSources_sortOrder,
    describeDataSources_nextToken,
    describeDataSources_filterVariable,
    describeDataSources_limit,
    describeDataSources_le,
    describeDataSources_lt,
    describeDataSources_eq,
    describeDataSources_prefix,
    describeDataSources_gt,
    describeDataSources_ne,
    describeDataSources_ge,
    describeDataSourcesResponse_nextToken,
    describeDataSourcesResponse_results,
    describeDataSourcesResponse_httpStatus,

    -- ** DescribeEvaluations
    describeEvaluations_sortOrder,
    describeEvaluations_nextToken,
    describeEvaluations_filterVariable,
    describeEvaluations_limit,
    describeEvaluations_le,
    describeEvaluations_lt,
    describeEvaluations_eq,
    describeEvaluations_prefix,
    describeEvaluations_gt,
    describeEvaluations_ne,
    describeEvaluations_ge,
    describeEvaluationsResponse_nextToken,
    describeEvaluationsResponse_results,
    describeEvaluationsResponse_httpStatus,

    -- ** DescribeMLModels
    describeMLModels_sortOrder,
    describeMLModels_nextToken,
    describeMLModels_filterVariable,
    describeMLModels_limit,
    describeMLModels_le,
    describeMLModels_lt,
    describeMLModels_eq,
    describeMLModels_prefix,
    describeMLModels_gt,
    describeMLModels_ne,
    describeMLModels_ge,
    describeMLModelsResponse_nextToken,
    describeMLModelsResponse_results,
    describeMLModelsResponse_httpStatus,

    -- ** DescribeTags
    describeTags_resourceId,
    describeTags_resourceType,
    describeTagsResponse_resourceId,
    describeTagsResponse_tags,
    describeTagsResponse_resourceType,
    describeTagsResponse_httpStatus,

    -- ** GetBatchPrediction
    getBatchPrediction_batchPredictionId,
    getBatchPredictionResponse_invalidRecordCount,
    getBatchPredictionResponse_message,
    getBatchPredictionResponse_name,
    getBatchPredictionResponse_totalRecordCount,
    getBatchPredictionResponse_lastUpdatedAt,
    getBatchPredictionResponse_batchPredictionDataSourceId,
    getBatchPredictionResponse_finishedAt,
    getBatchPredictionResponse_mLModelId,
    getBatchPredictionResponse_status,
    getBatchPredictionResponse_outputUri,
    getBatchPredictionResponse_startedAt,
    getBatchPredictionResponse_logUri,
    getBatchPredictionResponse_computeTime,
    getBatchPredictionResponse_batchPredictionId,
    getBatchPredictionResponse_createdAt,
    getBatchPredictionResponse_inputDataLocationS3,
    getBatchPredictionResponse_createdByIamUser,
    getBatchPredictionResponse_httpStatus,

    -- ** GetDataSource
    getDataSource_verbose,
    getDataSource_dataSourceId,
    getDataSourceResponse_dataSizeInBytes,
    getDataSourceResponse_message,
    getDataSourceResponse_name,
    getDataSourceResponse_roleARN,
    getDataSourceResponse_dataSourceSchema,
    getDataSourceResponse_dataSourceId,
    getDataSourceResponse_numberOfFiles,
    getDataSourceResponse_rDSMetadata,
    getDataSourceResponse_lastUpdatedAt,
    getDataSourceResponse_finishedAt,
    getDataSourceResponse_redshiftMetadata,
    getDataSourceResponse_dataRearrangement,
    getDataSourceResponse_dataLocationS3,
    getDataSourceResponse_status,
    getDataSourceResponse_startedAt,
    getDataSourceResponse_logUri,
    getDataSourceResponse_computeTime,
    getDataSourceResponse_computeStatistics,
    getDataSourceResponse_createdAt,
    getDataSourceResponse_createdByIamUser,
    getDataSourceResponse_httpStatus,

    -- ** GetEvaluation
    getEvaluation_evaluationId,
    getEvaluationResponse_message,
    getEvaluationResponse_name,
    getEvaluationResponse_lastUpdatedAt,
    getEvaluationResponse_performanceMetrics,
    getEvaluationResponse_finishedAt,
    getEvaluationResponse_mLModelId,
    getEvaluationResponse_status,
    getEvaluationResponse_evaluationDataSourceId,
    getEvaluationResponse_startedAt,
    getEvaluationResponse_logUri,
    getEvaluationResponse_computeTime,
    getEvaluationResponse_evaluationId,
    getEvaluationResponse_createdAt,
    getEvaluationResponse_inputDataLocationS3,
    getEvaluationResponse_createdByIamUser,
    getEvaluationResponse_httpStatus,

    -- ** GetMLModel
    getMLModel_verbose,
    getMLModel_mLModelId,
    getMLModelResponse_message,
    getMLModelResponse_name,
    getMLModelResponse_trainingDataSourceId,
    getMLModelResponse_scoreThresholdLastUpdatedAt,
    getMLModelResponse_lastUpdatedAt,
    getMLModelResponse_finishedAt,
    getMLModelResponse_recipe,
    getMLModelResponse_mLModelId,
    getMLModelResponse_scoreThreshold,
    getMLModelResponse_endpointInfo,
    getMLModelResponse_status,
    getMLModelResponse_mLModelType,
    getMLModelResponse_startedAt,
    getMLModelResponse_logUri,
    getMLModelResponse_computeTime,
    getMLModelResponse_sizeInBytes,
    getMLModelResponse_schema,
    getMLModelResponse_createdAt,
    getMLModelResponse_inputDataLocationS3,
    getMLModelResponse_createdByIamUser,
    getMLModelResponse_trainingParameters,
    getMLModelResponse_httpStatus,

    -- ** Predict
    predict_mLModelId,
    predict_record,
    predict_predictEndpoint,
    predictResponse_prediction,
    predictResponse_httpStatus,

    -- ** UpdateBatchPrediction
    updateBatchPrediction_batchPredictionId,
    updateBatchPrediction_batchPredictionName,
    updateBatchPredictionResponse_batchPredictionId,
    updateBatchPredictionResponse_httpStatus,

    -- ** UpdateDataSource
    updateDataSource_dataSourceId,
    updateDataSource_dataSourceName,
    updateDataSourceResponse_dataSourceId,
    updateDataSourceResponse_httpStatus,

    -- ** UpdateEvaluation
    updateEvaluation_evaluationId,
    updateEvaluation_evaluationName,
    updateEvaluationResponse_evaluationId,
    updateEvaluationResponse_httpStatus,

    -- ** UpdateMLModel
    updateMLModel_mLModelName,
    updateMLModel_scoreThreshold,
    updateMLModel_mLModelId,
    updateMLModelResponse_mLModelId,
    updateMLModelResponse_httpStatus,

    -- * Types

    -- ** BatchPrediction
    batchPrediction_invalidRecordCount,
    batchPrediction_message,
    batchPrediction_name,
    batchPrediction_totalRecordCount,
    batchPrediction_lastUpdatedAt,
    batchPrediction_batchPredictionDataSourceId,
    batchPrediction_finishedAt,
    batchPrediction_mLModelId,
    batchPrediction_status,
    batchPrediction_outputUri,
    batchPrediction_startedAt,
    batchPrediction_computeTime,
    batchPrediction_batchPredictionId,
    batchPrediction_createdAt,
    batchPrediction_inputDataLocationS3,
    batchPrediction_createdByIamUser,

    -- ** DataSource
    dataSource_dataSizeInBytes,
    dataSource_message,
    dataSource_name,
    dataSource_roleARN,
    dataSource_dataSourceId,
    dataSource_numberOfFiles,
    dataSource_rDSMetadata,
    dataSource_lastUpdatedAt,
    dataSource_finishedAt,
    dataSource_redshiftMetadata,
    dataSource_dataRearrangement,
    dataSource_dataLocationS3,
    dataSource_status,
    dataSource_startedAt,
    dataSource_computeTime,
    dataSource_computeStatistics,
    dataSource_createdAt,
    dataSource_createdByIamUser,

    -- ** Evaluation
    evaluation_message,
    evaluation_name,
    evaluation_lastUpdatedAt,
    evaluation_performanceMetrics,
    evaluation_finishedAt,
    evaluation_mLModelId,
    evaluation_status,
    evaluation_evaluationDataSourceId,
    evaluation_startedAt,
    evaluation_computeTime,
    evaluation_evaluationId,
    evaluation_createdAt,
    evaluation_inputDataLocationS3,
    evaluation_createdByIamUser,

    -- ** MLModel
    mLModel_message,
    mLModel_name,
    mLModel_trainingDataSourceId,
    mLModel_scoreThresholdLastUpdatedAt,
    mLModel_lastUpdatedAt,
    mLModel_finishedAt,
    mLModel_mLModelId,
    mLModel_scoreThreshold,
    mLModel_endpointInfo,
    mLModel_status,
    mLModel_mLModelType,
    mLModel_startedAt,
    mLModel_computeTime,
    mLModel_sizeInBytes,
    mLModel_algorithm,
    mLModel_createdAt,
    mLModel_inputDataLocationS3,
    mLModel_createdByIamUser,
    mLModel_trainingParameters,

    -- ** PerformanceMetrics
    performanceMetrics_properties,

    -- ** Prediction
    prediction_predictedScores,
    prediction_details,
    prediction_predictedLabel,
    prediction_predictedValue,

    -- ** RDSDataSpec
    rDSDataSpec_dataRearrangement,
    rDSDataSpec_dataSchema,
    rDSDataSpec_dataSchemaUri,
    rDSDataSpec_databaseInformation,
    rDSDataSpec_selectSqlQuery,
    rDSDataSpec_databaseCredentials,
    rDSDataSpec_s3StagingLocation,
    rDSDataSpec_resourceRole,
    rDSDataSpec_serviceRole,
    rDSDataSpec_subnetId,
    rDSDataSpec_securityGroupIds,

    -- ** RDSDatabase
    rDSDatabase_instanceIdentifier,
    rDSDatabase_databaseName,

    -- ** RDSDatabaseCredentials
    rDSDatabaseCredentials_username,
    rDSDatabaseCredentials_password,

    -- ** RDSMetadata
    rDSMetadata_databaseUserName,
    rDSMetadata_resourceRole,
    rDSMetadata_selectSqlQuery,
    rDSMetadata_serviceRole,
    rDSMetadata_database,
    rDSMetadata_dataPipelineId,

    -- ** RealtimeEndpointInfo
    realtimeEndpointInfo_peakRequestsPerSecond,
    realtimeEndpointInfo_endpointUrl,
    realtimeEndpointInfo_endpointStatus,
    realtimeEndpointInfo_createdAt,

    -- ** RedshiftDataSpec
    redshiftDataSpec_dataRearrangement,
    redshiftDataSpec_dataSchema,
    redshiftDataSpec_dataSchemaUri,
    redshiftDataSpec_databaseInformation,
    redshiftDataSpec_selectSqlQuery,
    redshiftDataSpec_databaseCredentials,
    redshiftDataSpec_s3StagingLocation,

    -- ** RedshiftDatabase
    redshiftDatabase_databaseName,
    redshiftDatabase_clusterIdentifier,

    -- ** RedshiftDatabaseCredentials
    redshiftDatabaseCredentials_username,
    redshiftDatabaseCredentials_password,

    -- ** RedshiftMetadata
    redshiftMetadata_databaseUserName,
    redshiftMetadata_selectSqlQuery,
    redshiftMetadata_redshiftDatabase,

    -- ** S3DataSpec
    s3DataSpec_dataSchemaLocationS3,
    s3DataSpec_dataRearrangement,
    s3DataSpec_dataSchema,
    s3DataSpec_dataLocationS3,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.MachineLearning.AddTags
import Amazonka.MachineLearning.CreateBatchPrediction
import Amazonka.MachineLearning.CreateDataSourceFromRDS
import Amazonka.MachineLearning.CreateDataSourceFromRedshift
import Amazonka.MachineLearning.CreateDataSourceFromS3
import Amazonka.MachineLearning.CreateEvaluation
import Amazonka.MachineLearning.CreateMLModel
import Amazonka.MachineLearning.CreateRealtimeEndpoint
import Amazonka.MachineLearning.DeleteBatchPrediction
import Amazonka.MachineLearning.DeleteDataSource
import Amazonka.MachineLearning.DeleteEvaluation
import Amazonka.MachineLearning.DeleteMLModel
import Amazonka.MachineLearning.DeleteRealtimeEndpoint
import Amazonka.MachineLearning.DeleteTags
import Amazonka.MachineLearning.DescribeBatchPredictions
import Amazonka.MachineLearning.DescribeDataSources
import Amazonka.MachineLearning.DescribeEvaluations
import Amazonka.MachineLearning.DescribeMLModels
import Amazonka.MachineLearning.DescribeTags
import Amazonka.MachineLearning.GetBatchPrediction
import Amazonka.MachineLearning.GetDataSource
import Amazonka.MachineLearning.GetEvaluation
import Amazonka.MachineLearning.GetMLModel
import Amazonka.MachineLearning.Predict
import Amazonka.MachineLearning.Types.BatchPrediction
import Amazonka.MachineLearning.Types.DataSource
import Amazonka.MachineLearning.Types.Evaluation
import Amazonka.MachineLearning.Types.MLModel
import Amazonka.MachineLearning.Types.PerformanceMetrics
import Amazonka.MachineLearning.Types.Prediction
import Amazonka.MachineLearning.Types.RDSDataSpec
import Amazonka.MachineLearning.Types.RDSDatabase
import Amazonka.MachineLearning.Types.RDSDatabaseCredentials
import Amazonka.MachineLearning.Types.RDSMetadata
import Amazonka.MachineLearning.Types.RealtimeEndpointInfo
import Amazonka.MachineLearning.Types.RedshiftDataSpec
import Amazonka.MachineLearning.Types.RedshiftDatabase
import Amazonka.MachineLearning.Types.RedshiftDatabaseCredentials
import Amazonka.MachineLearning.Types.RedshiftMetadata
import Amazonka.MachineLearning.Types.S3DataSpec
import Amazonka.MachineLearning.Types.Tag
import Amazonka.MachineLearning.UpdateBatchPrediction
import Amazonka.MachineLearning.UpdateDataSource
import Amazonka.MachineLearning.UpdateEvaluation
import Amazonka.MachineLearning.UpdateMLModel

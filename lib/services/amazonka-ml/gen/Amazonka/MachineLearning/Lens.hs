{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MachineLearning.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    createDataSourceFromRDS_computeStatistics,
    createDataSourceFromRDS_dataSourceName,
    createDataSourceFromRDS_dataSourceId,
    createDataSourceFromRDS_rDSData,
    createDataSourceFromRDS_roleARN,
    createDataSourceFromRDSResponse_dataSourceId,
    createDataSourceFromRDSResponse_httpStatus,

    -- ** CreateDataSourceFromRedshift
    createDataSourceFromRedshift_computeStatistics,
    createDataSourceFromRedshift_dataSourceName,
    createDataSourceFromRedshift_dataSourceId,
    createDataSourceFromRedshift_dataSpec,
    createDataSourceFromRedshift_roleARN,
    createDataSourceFromRedshiftResponse_dataSourceId,
    createDataSourceFromRedshiftResponse_httpStatus,

    -- ** CreateDataSourceFromS3
    createDataSourceFromS3_computeStatistics,
    createDataSourceFromS3_dataSourceName,
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
    createMLModel_parameters,
    createMLModel_recipe,
    createMLModel_recipeUri,
    createMLModel_mLModelId,
    createMLModel_mLModelType,
    createMLModel_trainingDataSourceId,
    createMLModelResponse_mLModelId,
    createMLModelResponse_httpStatus,

    -- ** CreateRealtimeEndpoint
    createRealtimeEndpoint_mLModelId,
    createRealtimeEndpointResponse_mLModelId,
    createRealtimeEndpointResponse_realtimeEndpointInfo,
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
    deleteRealtimeEndpointResponse_mLModelId,
    deleteRealtimeEndpointResponse_realtimeEndpointInfo,
    deleteRealtimeEndpointResponse_httpStatus,

    -- ** DeleteTags
    deleteTags_tagKeys,
    deleteTags_resourceId,
    deleteTags_resourceType,
    deleteTagsResponse_resourceId,
    deleteTagsResponse_resourceType,
    deleteTagsResponse_httpStatus,

    -- ** DescribeBatchPredictions
    describeBatchPredictions_eq,
    describeBatchPredictions_filterVariable,
    describeBatchPredictions_ge,
    describeBatchPredictions_gt,
    describeBatchPredictions_le,
    describeBatchPredictions_lt,
    describeBatchPredictions_limit,
    describeBatchPredictions_ne,
    describeBatchPredictions_nextToken,
    describeBatchPredictions_prefix,
    describeBatchPredictions_sortOrder,
    describeBatchPredictionsResponse_nextToken,
    describeBatchPredictionsResponse_results,
    describeBatchPredictionsResponse_httpStatus,

    -- ** DescribeDataSources
    describeDataSources_eq,
    describeDataSources_filterVariable,
    describeDataSources_ge,
    describeDataSources_gt,
    describeDataSources_le,
    describeDataSources_lt,
    describeDataSources_limit,
    describeDataSources_ne,
    describeDataSources_nextToken,
    describeDataSources_prefix,
    describeDataSources_sortOrder,
    describeDataSourcesResponse_nextToken,
    describeDataSourcesResponse_results,
    describeDataSourcesResponse_httpStatus,

    -- ** DescribeEvaluations
    describeEvaluations_eq,
    describeEvaluations_filterVariable,
    describeEvaluations_ge,
    describeEvaluations_gt,
    describeEvaluations_le,
    describeEvaluations_lt,
    describeEvaluations_limit,
    describeEvaluations_ne,
    describeEvaluations_nextToken,
    describeEvaluations_prefix,
    describeEvaluations_sortOrder,
    describeEvaluationsResponse_nextToken,
    describeEvaluationsResponse_results,
    describeEvaluationsResponse_httpStatus,

    -- ** DescribeMLModels
    describeMLModels_eq,
    describeMLModels_filterVariable,
    describeMLModels_ge,
    describeMLModels_gt,
    describeMLModels_le,
    describeMLModels_lt,
    describeMLModels_limit,
    describeMLModels_ne,
    describeMLModels_nextToken,
    describeMLModels_prefix,
    describeMLModels_sortOrder,
    describeMLModelsResponse_nextToken,
    describeMLModelsResponse_results,
    describeMLModelsResponse_httpStatus,

    -- ** DescribeTags
    describeTags_resourceId,
    describeTags_resourceType,
    describeTagsResponse_resourceId,
    describeTagsResponse_resourceType,
    describeTagsResponse_tags,
    describeTagsResponse_httpStatus,

    -- ** GetBatchPrediction
    getBatchPrediction_batchPredictionId,
    getBatchPredictionResponse_batchPredictionDataSourceId,
    getBatchPredictionResponse_batchPredictionId,
    getBatchPredictionResponse_computeTime,
    getBatchPredictionResponse_createdAt,
    getBatchPredictionResponse_createdByIamUser,
    getBatchPredictionResponse_finishedAt,
    getBatchPredictionResponse_inputDataLocationS3,
    getBatchPredictionResponse_invalidRecordCount,
    getBatchPredictionResponse_lastUpdatedAt,
    getBatchPredictionResponse_logUri,
    getBatchPredictionResponse_mLModelId,
    getBatchPredictionResponse_message,
    getBatchPredictionResponse_name,
    getBatchPredictionResponse_outputUri,
    getBatchPredictionResponse_startedAt,
    getBatchPredictionResponse_status,
    getBatchPredictionResponse_totalRecordCount,
    getBatchPredictionResponse_httpStatus,

    -- ** GetDataSource
    getDataSource_verbose,
    getDataSource_dataSourceId,
    getDataSourceResponse_computeStatistics,
    getDataSourceResponse_computeTime,
    getDataSourceResponse_createdAt,
    getDataSourceResponse_createdByIamUser,
    getDataSourceResponse_dataLocationS3,
    getDataSourceResponse_dataRearrangement,
    getDataSourceResponse_dataSizeInBytes,
    getDataSourceResponse_dataSourceId,
    getDataSourceResponse_dataSourceSchema,
    getDataSourceResponse_finishedAt,
    getDataSourceResponse_lastUpdatedAt,
    getDataSourceResponse_logUri,
    getDataSourceResponse_message,
    getDataSourceResponse_name,
    getDataSourceResponse_numberOfFiles,
    getDataSourceResponse_rDSMetadata,
    getDataSourceResponse_redshiftMetadata,
    getDataSourceResponse_roleARN,
    getDataSourceResponse_startedAt,
    getDataSourceResponse_status,
    getDataSourceResponse_httpStatus,

    -- ** GetEvaluation
    getEvaluation_evaluationId,
    getEvaluationResponse_computeTime,
    getEvaluationResponse_createdAt,
    getEvaluationResponse_createdByIamUser,
    getEvaluationResponse_evaluationDataSourceId,
    getEvaluationResponse_evaluationId,
    getEvaluationResponse_finishedAt,
    getEvaluationResponse_inputDataLocationS3,
    getEvaluationResponse_lastUpdatedAt,
    getEvaluationResponse_logUri,
    getEvaluationResponse_mLModelId,
    getEvaluationResponse_message,
    getEvaluationResponse_name,
    getEvaluationResponse_performanceMetrics,
    getEvaluationResponse_startedAt,
    getEvaluationResponse_status,
    getEvaluationResponse_httpStatus,

    -- ** GetMLModel
    getMLModel_verbose,
    getMLModel_mLModelId,
    getMLModelResponse_computeTime,
    getMLModelResponse_createdAt,
    getMLModelResponse_createdByIamUser,
    getMLModelResponse_endpointInfo,
    getMLModelResponse_finishedAt,
    getMLModelResponse_inputDataLocationS3,
    getMLModelResponse_lastUpdatedAt,
    getMLModelResponse_logUri,
    getMLModelResponse_mLModelId,
    getMLModelResponse_mLModelType,
    getMLModelResponse_message,
    getMLModelResponse_name,
    getMLModelResponse_recipe,
    getMLModelResponse_schema,
    getMLModelResponse_scoreThreshold,
    getMLModelResponse_scoreThresholdLastUpdatedAt,
    getMLModelResponse_sizeInBytes,
    getMLModelResponse_startedAt,
    getMLModelResponse_status,
    getMLModelResponse_trainingDataSourceId,
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
    batchPrediction_batchPredictionDataSourceId,
    batchPrediction_batchPredictionId,
    batchPrediction_computeTime,
    batchPrediction_createdAt,
    batchPrediction_createdByIamUser,
    batchPrediction_finishedAt,
    batchPrediction_inputDataLocationS3,
    batchPrediction_invalidRecordCount,
    batchPrediction_lastUpdatedAt,
    batchPrediction_mLModelId,
    batchPrediction_message,
    batchPrediction_name,
    batchPrediction_outputUri,
    batchPrediction_startedAt,
    batchPrediction_status,
    batchPrediction_totalRecordCount,

    -- ** DataSource
    dataSource_computeStatistics,
    dataSource_computeTime,
    dataSource_createdAt,
    dataSource_createdByIamUser,
    dataSource_dataLocationS3,
    dataSource_dataRearrangement,
    dataSource_dataSizeInBytes,
    dataSource_dataSourceId,
    dataSource_finishedAt,
    dataSource_lastUpdatedAt,
    dataSource_message,
    dataSource_name,
    dataSource_numberOfFiles,
    dataSource_rDSMetadata,
    dataSource_redshiftMetadata,
    dataSource_roleARN,
    dataSource_startedAt,
    dataSource_status,

    -- ** Evaluation
    evaluation_computeTime,
    evaluation_createdAt,
    evaluation_createdByIamUser,
    evaluation_evaluationDataSourceId,
    evaluation_evaluationId,
    evaluation_finishedAt,
    evaluation_inputDataLocationS3,
    evaluation_lastUpdatedAt,
    evaluation_mLModelId,
    evaluation_message,
    evaluation_name,
    evaluation_performanceMetrics,
    evaluation_startedAt,
    evaluation_status,

    -- ** MLModel
    mLModel_algorithm,
    mLModel_computeTime,
    mLModel_createdAt,
    mLModel_createdByIamUser,
    mLModel_endpointInfo,
    mLModel_finishedAt,
    mLModel_inputDataLocationS3,
    mLModel_lastUpdatedAt,
    mLModel_mLModelId,
    mLModel_mLModelType,
    mLModel_message,
    mLModel_name,
    mLModel_scoreThreshold,
    mLModel_scoreThresholdLastUpdatedAt,
    mLModel_sizeInBytes,
    mLModel_startedAt,
    mLModel_status,
    mLModel_trainingDataSourceId,
    mLModel_trainingParameters,

    -- ** PerformanceMetrics
    performanceMetrics_properties,

    -- ** Prediction
    prediction_details,
    prediction_predictedLabel,
    prediction_predictedScores,
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
    rDSMetadata_dataPipelineId,
    rDSMetadata_database,
    rDSMetadata_databaseUserName,
    rDSMetadata_resourceRole,
    rDSMetadata_selectSqlQuery,
    rDSMetadata_serviceRole,

    -- ** RealtimeEndpointInfo
    realtimeEndpointInfo_createdAt,
    realtimeEndpointInfo_endpointStatus,
    realtimeEndpointInfo_endpointUrl,
    realtimeEndpointInfo_peakRequestsPerSecond,

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
    redshiftMetadata_redshiftDatabase,
    redshiftMetadata_selectSqlQuery,

    -- ** S3DataSpec
    s3DataSpec_dataRearrangement,
    s3DataSpec_dataSchema,
    s3DataSpec_dataSchemaLocationS3,
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

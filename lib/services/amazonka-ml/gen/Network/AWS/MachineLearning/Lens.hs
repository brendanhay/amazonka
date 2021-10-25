{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Lens
  ( -- * Operations

    -- ** UpdateDataSource
    updateDataSource_dataSourceId,
    updateDataSource_dataSourceName,
    updateDataSourceResponse_dataSourceId,
    updateDataSourceResponse_httpStatus,

    -- ** DeleteDataSource
    deleteDataSource_dataSourceId,
    deleteDataSourceResponse_dataSourceId,
    deleteDataSourceResponse_httpStatus,

    -- ** DescribeTags
    describeTags_resourceId,
    describeTags_resourceType,
    describeTagsResponse_resourceId,
    describeTagsResponse_resourceType,
    describeTagsResponse_tags,
    describeTagsResponse_httpStatus,

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

    -- ** CreateMLModel
    createMLModel_recipe,
    createMLModel_recipeUri,
    createMLModel_mLModelName,
    createMLModel_parameters,
    createMLModel_mLModelId,
    createMLModel_mLModelType,
    createMLModel_trainingDataSourceId,
    createMLModelResponse_mLModelId,
    createMLModelResponse_httpStatus,

    -- ** DeleteTags
    deleteTags_tagKeys,
    deleteTags_resourceId,
    deleteTags_resourceType,
    deleteTagsResponse_resourceId,
    deleteTagsResponse_resourceType,
    deleteTagsResponse_httpStatus,

    -- ** DeleteBatchPrediction
    deleteBatchPrediction_batchPredictionId,
    deleteBatchPredictionResponse_batchPredictionId,
    deleteBatchPredictionResponse_httpStatus,

    -- ** UpdateBatchPrediction
    updateBatchPrediction_batchPredictionId,
    updateBatchPrediction_batchPredictionName,
    updateBatchPredictionResponse_batchPredictionId,
    updateBatchPredictionResponse_httpStatus,

    -- ** GetMLModel
    getMLModel_verbose,
    getMLModel_mLModelId,
    getMLModelResponse_status,
    getMLModelResponse_lastUpdatedAt,
    getMLModelResponse_trainingParameters,
    getMLModelResponse_scoreThresholdLastUpdatedAt,
    getMLModelResponse_createdAt,
    getMLModelResponse_computeTime,
    getMLModelResponse_recipe,
    getMLModelResponse_inputDataLocationS3,
    getMLModelResponse_mLModelId,
    getMLModelResponse_sizeInBytes,
    getMLModelResponse_schema,
    getMLModelResponse_startedAt,
    getMLModelResponse_scoreThreshold,
    getMLModelResponse_finishedAt,
    getMLModelResponse_createdByIamUser,
    getMLModelResponse_name,
    getMLModelResponse_logUri,
    getMLModelResponse_endpointInfo,
    getMLModelResponse_trainingDataSourceId,
    getMLModelResponse_message,
    getMLModelResponse_mLModelType,
    getMLModelResponse_httpStatus,

    -- ** GetDataSource
    getDataSource_verbose,
    getDataSource_dataSourceId,
    getDataSourceResponse_status,
    getDataSourceResponse_numberOfFiles,
    getDataSourceResponse_lastUpdatedAt,
    getDataSourceResponse_createdAt,
    getDataSourceResponse_computeTime,
    getDataSourceResponse_dataSourceId,
    getDataSourceResponse_rDSMetadata,
    getDataSourceResponse_dataSizeInBytes,
    getDataSourceResponse_dataSourceSchema,
    getDataSourceResponse_startedAt,
    getDataSourceResponse_finishedAt,
    getDataSourceResponse_createdByIamUser,
    getDataSourceResponse_name,
    getDataSourceResponse_logUri,
    getDataSourceResponse_dataLocationS3,
    getDataSourceResponse_computeStatistics,
    getDataSourceResponse_message,
    getDataSourceResponse_redshiftMetadata,
    getDataSourceResponse_dataRearrangement,
    getDataSourceResponse_roleARN,
    getDataSourceResponse_httpStatus,

    -- ** UpdateEvaluation
    updateEvaluation_evaluationId,
    updateEvaluation_evaluationName,
    updateEvaluationResponse_evaluationId,
    updateEvaluationResponse_httpStatus,

    -- ** DeleteEvaluation
    deleteEvaluation_evaluationId,
    deleteEvaluationResponse_evaluationId,
    deleteEvaluationResponse_httpStatus,

    -- ** DeleteMLModel
    deleteMLModel_mLModelId,
    deleteMLModelResponse_mLModelId,
    deleteMLModelResponse_httpStatus,

    -- ** UpdateMLModel
    updateMLModel_mLModelName,
    updateMLModel_scoreThreshold,
    updateMLModel_mLModelId,
    updateMLModelResponse_mLModelId,
    updateMLModelResponse_httpStatus,

    -- ** GetBatchPrediction
    getBatchPrediction_batchPredictionId,
    getBatchPredictionResponse_status,
    getBatchPredictionResponse_lastUpdatedAt,
    getBatchPredictionResponse_createdAt,
    getBatchPredictionResponse_computeTime,
    getBatchPredictionResponse_inputDataLocationS3,
    getBatchPredictionResponse_mLModelId,
    getBatchPredictionResponse_batchPredictionDataSourceId,
    getBatchPredictionResponse_totalRecordCount,
    getBatchPredictionResponse_startedAt,
    getBatchPredictionResponse_batchPredictionId,
    getBatchPredictionResponse_finishedAt,
    getBatchPredictionResponse_invalidRecordCount,
    getBatchPredictionResponse_createdByIamUser,
    getBatchPredictionResponse_name,
    getBatchPredictionResponse_logUri,
    getBatchPredictionResponse_message,
    getBatchPredictionResponse_outputUri,
    getBatchPredictionResponse_httpStatus,

    -- ** DescribeBatchPredictions
    describeBatchPredictions_eq,
    describeBatchPredictions_ge,
    describeBatchPredictions_prefix,
    describeBatchPredictions_gt,
    describeBatchPredictions_ne,
    describeBatchPredictions_nextToken,
    describeBatchPredictions_sortOrder,
    describeBatchPredictions_limit,
    describeBatchPredictions_lt,
    describeBatchPredictions_filterVariable,
    describeBatchPredictions_le,
    describeBatchPredictionsResponse_results,
    describeBatchPredictionsResponse_nextToken,
    describeBatchPredictionsResponse_httpStatus,

    -- ** CreateDataSourceFromRDS
    createDataSourceFromRDS_dataSourceName,
    createDataSourceFromRDS_computeStatistics,
    createDataSourceFromRDS_dataSourceId,
    createDataSourceFromRDS_rDSData,
    createDataSourceFromRDS_roleARN,
    createDataSourceFromRDSResponse_dataSourceId,
    createDataSourceFromRDSResponse_httpStatus,

    -- ** CreateEvaluation
    createEvaluation_evaluationName,
    createEvaluation_evaluationId,
    createEvaluation_mLModelId,
    createEvaluation_evaluationDataSourceId,
    createEvaluationResponse_evaluationId,
    createEvaluationResponse_httpStatus,

    -- ** Predict
    predict_mLModelId,
    predict_record,
    predict_predictEndpoint,
    predictResponse_prediction,
    predictResponse_httpStatus,

    -- ** DeleteRealtimeEndpoint
    deleteRealtimeEndpoint_mLModelId,
    deleteRealtimeEndpointResponse_realtimeEndpointInfo,
    deleteRealtimeEndpointResponse_mLModelId,
    deleteRealtimeEndpointResponse_httpStatus,

    -- ** CreateBatchPrediction
    createBatchPrediction_batchPredictionName,
    createBatchPrediction_batchPredictionId,
    createBatchPrediction_mLModelId,
    createBatchPrediction_batchPredictionDataSourceId,
    createBatchPrediction_outputUri,
    createBatchPredictionResponse_batchPredictionId,
    createBatchPredictionResponse_httpStatus,

    -- ** GetEvaluation
    getEvaluation_evaluationId,
    getEvaluationResponse_status,
    getEvaluationResponse_performanceMetrics,
    getEvaluationResponse_lastUpdatedAt,
    getEvaluationResponse_createdAt,
    getEvaluationResponse_computeTime,
    getEvaluationResponse_inputDataLocationS3,
    getEvaluationResponse_mLModelId,
    getEvaluationResponse_startedAt,
    getEvaluationResponse_finishedAt,
    getEvaluationResponse_createdByIamUser,
    getEvaluationResponse_name,
    getEvaluationResponse_logUri,
    getEvaluationResponse_evaluationId,
    getEvaluationResponse_message,
    getEvaluationResponse_evaluationDataSourceId,
    getEvaluationResponse_httpStatus,

    -- ** DescribeEvaluations
    describeEvaluations_eq,
    describeEvaluations_ge,
    describeEvaluations_prefix,
    describeEvaluations_gt,
    describeEvaluations_ne,
    describeEvaluations_nextToken,
    describeEvaluations_sortOrder,
    describeEvaluations_limit,
    describeEvaluations_lt,
    describeEvaluations_filterVariable,
    describeEvaluations_le,
    describeEvaluationsResponse_results,
    describeEvaluationsResponse_nextToken,
    describeEvaluationsResponse_httpStatus,

    -- ** CreateRealtimeEndpoint
    createRealtimeEndpoint_mLModelId,
    createRealtimeEndpointResponse_realtimeEndpointInfo,
    createRealtimeEndpointResponse_mLModelId,
    createRealtimeEndpointResponse_httpStatus,

    -- ** AddTags
    addTags_tags,
    addTags_resourceId,
    addTags_resourceType,
    addTagsResponse_resourceId,
    addTagsResponse_resourceType,
    addTagsResponse_httpStatus,

    -- ** DescribeMLModels
    describeMLModels_eq,
    describeMLModels_ge,
    describeMLModels_prefix,
    describeMLModels_gt,
    describeMLModels_ne,
    describeMLModels_nextToken,
    describeMLModels_sortOrder,
    describeMLModels_limit,
    describeMLModels_lt,
    describeMLModels_filterVariable,
    describeMLModels_le,
    describeMLModelsResponse_results,
    describeMLModelsResponse_nextToken,
    describeMLModelsResponse_httpStatus,

    -- ** DescribeDataSources
    describeDataSources_eq,
    describeDataSources_ge,
    describeDataSources_prefix,
    describeDataSources_gt,
    describeDataSources_ne,
    describeDataSources_nextToken,
    describeDataSources_sortOrder,
    describeDataSources_limit,
    describeDataSources_lt,
    describeDataSources_filterVariable,
    describeDataSources_le,
    describeDataSourcesResponse_results,
    describeDataSourcesResponse_nextToken,
    describeDataSourcesResponse_httpStatus,

    -- * Types

    -- ** BatchPrediction
    batchPrediction_status,
    batchPrediction_lastUpdatedAt,
    batchPrediction_createdAt,
    batchPrediction_computeTime,
    batchPrediction_inputDataLocationS3,
    batchPrediction_mLModelId,
    batchPrediction_batchPredictionDataSourceId,
    batchPrediction_totalRecordCount,
    batchPrediction_startedAt,
    batchPrediction_batchPredictionId,
    batchPrediction_finishedAt,
    batchPrediction_invalidRecordCount,
    batchPrediction_createdByIamUser,
    batchPrediction_name,
    batchPrediction_message,
    batchPrediction_outputUri,

    -- ** DataSource
    dataSource_status,
    dataSource_numberOfFiles,
    dataSource_lastUpdatedAt,
    dataSource_createdAt,
    dataSource_computeTime,
    dataSource_dataSourceId,
    dataSource_rDSMetadata,
    dataSource_dataSizeInBytes,
    dataSource_startedAt,
    dataSource_finishedAt,
    dataSource_createdByIamUser,
    dataSource_name,
    dataSource_dataLocationS3,
    dataSource_computeStatistics,
    dataSource_message,
    dataSource_redshiftMetadata,
    dataSource_dataRearrangement,
    dataSource_roleARN,

    -- ** Evaluation
    evaluation_status,
    evaluation_performanceMetrics,
    evaluation_lastUpdatedAt,
    evaluation_createdAt,
    evaluation_computeTime,
    evaluation_inputDataLocationS3,
    evaluation_mLModelId,
    evaluation_startedAt,
    evaluation_finishedAt,
    evaluation_createdByIamUser,
    evaluation_name,
    evaluation_evaluationId,
    evaluation_message,
    evaluation_evaluationDataSourceId,

    -- ** MLModel
    mLModel_status,
    mLModel_lastUpdatedAt,
    mLModel_trainingParameters,
    mLModel_scoreThresholdLastUpdatedAt,
    mLModel_createdAt,
    mLModel_computeTime,
    mLModel_inputDataLocationS3,
    mLModel_mLModelId,
    mLModel_sizeInBytes,
    mLModel_startedAt,
    mLModel_scoreThreshold,
    mLModel_finishedAt,
    mLModel_algorithm,
    mLModel_createdByIamUser,
    mLModel_name,
    mLModel_endpointInfo,
    mLModel_trainingDataSourceId,
    mLModel_message,
    mLModel_mLModelType,

    -- ** PerformanceMetrics
    performanceMetrics_properties,

    -- ** Prediction
    prediction_predictedValue,
    prediction_predictedLabel,
    prediction_predictedScores,
    prediction_details,

    -- ** RDSDataSpec
    rDSDataSpec_dataSchemaUri,
    rDSDataSpec_dataSchema,
    rDSDataSpec_dataRearrangement,
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
    rDSMetadata_selectSqlQuery,
    rDSMetadata_dataPipelineId,
    rDSMetadata_database,
    rDSMetadata_databaseUserName,
    rDSMetadata_resourceRole,
    rDSMetadata_serviceRole,

    -- ** RealtimeEndpointInfo
    realtimeEndpointInfo_createdAt,
    realtimeEndpointInfo_endpointUrl,
    realtimeEndpointInfo_endpointStatus,
    realtimeEndpointInfo_peakRequestsPerSecond,

    -- ** RedshiftDataSpec
    redshiftDataSpec_dataSchemaUri,
    redshiftDataSpec_dataSchema,
    redshiftDataSpec_dataRearrangement,
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
    redshiftMetadata_selectSqlQuery,
    redshiftMetadata_redshiftDatabase,
    redshiftMetadata_databaseUserName,

    -- ** S3DataSpec
    s3DataSpec_dataSchema,
    s3DataSpec_dataSchemaLocationS3,
    s3DataSpec_dataRearrangement,
    s3DataSpec_dataLocationS3,

    -- ** Tag
    tag_value,
    tag_key,
  )
where

import Network.AWS.MachineLearning.AddTags
import Network.AWS.MachineLearning.CreateBatchPrediction
import Network.AWS.MachineLearning.CreateDataSourceFromRDS
import Network.AWS.MachineLearning.CreateDataSourceFromRedshift
import Network.AWS.MachineLearning.CreateDataSourceFromS3
import Network.AWS.MachineLearning.CreateEvaluation
import Network.AWS.MachineLearning.CreateMLModel
import Network.AWS.MachineLearning.CreateRealtimeEndpoint
import Network.AWS.MachineLearning.DeleteBatchPrediction
import Network.AWS.MachineLearning.DeleteDataSource
import Network.AWS.MachineLearning.DeleteEvaluation
import Network.AWS.MachineLearning.DeleteMLModel
import Network.AWS.MachineLearning.DeleteRealtimeEndpoint
import Network.AWS.MachineLearning.DeleteTags
import Network.AWS.MachineLearning.DescribeBatchPredictions
import Network.AWS.MachineLearning.DescribeDataSources
import Network.AWS.MachineLearning.DescribeEvaluations
import Network.AWS.MachineLearning.DescribeMLModels
import Network.AWS.MachineLearning.DescribeTags
import Network.AWS.MachineLearning.GetBatchPrediction
import Network.AWS.MachineLearning.GetDataSource
import Network.AWS.MachineLearning.GetEvaluation
import Network.AWS.MachineLearning.GetMLModel
import Network.AWS.MachineLearning.Predict
import Network.AWS.MachineLearning.Types.BatchPrediction
import Network.AWS.MachineLearning.Types.DataSource
import Network.AWS.MachineLearning.Types.Evaluation
import Network.AWS.MachineLearning.Types.MLModel
import Network.AWS.MachineLearning.Types.PerformanceMetrics
import Network.AWS.MachineLearning.Types.Prediction
import Network.AWS.MachineLearning.Types.RDSDataSpec
import Network.AWS.MachineLearning.Types.RDSDatabase
import Network.AWS.MachineLearning.Types.RDSDatabaseCredentials
import Network.AWS.MachineLearning.Types.RDSMetadata
import Network.AWS.MachineLearning.Types.RealtimeEndpointInfo
import Network.AWS.MachineLearning.Types.RedshiftDataSpec
import Network.AWS.MachineLearning.Types.RedshiftDatabase
import Network.AWS.MachineLearning.Types.RedshiftDatabaseCredentials
import Network.AWS.MachineLearning.Types.RedshiftMetadata
import Network.AWS.MachineLearning.Types.S3DataSpec
import Network.AWS.MachineLearning.Types.Tag
import Network.AWS.MachineLearning.UpdateBatchPrediction
import Network.AWS.MachineLearning.UpdateDataSource
import Network.AWS.MachineLearning.UpdateEvaluation
import Network.AWS.MachineLearning.UpdateMLModel

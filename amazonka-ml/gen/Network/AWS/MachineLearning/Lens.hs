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

    -- ** DeleteMLModel
    deleteMLModel_mLModelId,
    deleteMLModelResponse_mLModelId,
    deleteMLModelResponse_httpStatus,

    -- ** UpdateMLModel
    updateMLModel_scoreThreshold,
    updateMLModel_mLModelName,
    updateMLModel_mLModelId,
    updateMLModelResponse_mLModelId,
    updateMLModelResponse_httpStatus,

    -- ** CreateDataSourceFromS
    createDataSourceFromS_computeStatistics,
    createDataSourceFromS_dataSourceName,
    createDataSourceFromS_dataSourceId,
    createDataSourceFromS_dataSpec,
    createDataSourceFromSResponse_dataSourceId,
    createDataSourceFromSResponse_httpStatus,

    -- ** CreateDataSourceFromRedshift
    createDataSourceFromRedshift_computeStatistics,
    createDataSourceFromRedshift_dataSourceName,
    createDataSourceFromRedshift_dataSourceId,
    createDataSourceFromRedshift_dataSpec,
    createDataSourceFromRedshift_roleARN,
    createDataSourceFromRedshiftResponse_dataSourceId,
    createDataSourceFromRedshiftResponse_httpStatus,

    -- ** UpdateDataSource
    updateDataSource_dataSourceId,
    updateDataSource_dataSourceName,
    updateDataSourceResponse_dataSourceId,
    updateDataSourceResponse_httpStatus,

    -- ** DescribeTags
    describeTags_resourceId,
    describeTags_resourceType,
    describeTagsResponse_resourceId,
    describeTagsResponse_resourceType,
    describeTagsResponse_tags,
    describeTagsResponse_httpStatus,

    -- ** DeleteDataSource
    deleteDataSource_dataSourceId,
    deleteDataSourceResponse_dataSourceId,
    deleteDataSourceResponse_httpStatus,

    -- ** DescribeDataSources
    describeDataSources_sortOrder,
    describeDataSources_eq,
    describeDataSources_nextToken,
    describeDataSources_filterVariable,
    describeDataSources_gt,
    describeDataSources_ne,
    describeDataSources_prefix,
    describeDataSources_ge,
    describeDataSources_le,
    describeDataSources_lt,
    describeDataSources_limit,
    describeDataSourcesResponse_nextToken,
    describeDataSourcesResponse_results,
    describeDataSourcesResponse_httpStatus,

    -- ** DescribeEvaluations
    describeEvaluations_sortOrder,
    describeEvaluations_eq,
    describeEvaluations_nextToken,
    describeEvaluations_filterVariable,
    describeEvaluations_gt,
    describeEvaluations_ne,
    describeEvaluations_prefix,
    describeEvaluations_ge,
    describeEvaluations_le,
    describeEvaluations_lt,
    describeEvaluations_limit,
    describeEvaluationsResponse_nextToken,
    describeEvaluationsResponse_results,
    describeEvaluationsResponse_httpStatus,

    -- ** AddTags
    addTags_tags,
    addTags_resourceId,
    addTags_resourceType,
    addTagsResponse_resourceId,
    addTagsResponse_resourceType,
    addTagsResponse_httpStatus,

    -- ** GetMLModel
    getMLModel_verbose,
    getMLModel_mLModelId,
    getMLModelResponse_status,
    getMLModelResponse_startedAt,
    getMLModelResponse_schema,
    getMLModelResponse_message,
    getMLModelResponse_recipe,
    getMLModelResponse_endpointInfo,
    getMLModelResponse_scoreThresholdLastUpdatedAt,
    getMLModelResponse_createdAt,
    getMLModelResponse_trainingParameters,
    getMLModelResponse_finishedAt,
    getMLModelResponse_scoreThreshold,
    getMLModelResponse_createdByIamUser,
    getMLModelResponse_name,
    getMLModelResponse_mLModelType,
    getMLModelResponse_mLModelId,
    getMLModelResponse_sizeInBytes,
    getMLModelResponse_inputDataLocationS3,
    getMLModelResponse_computeTime,
    getMLModelResponse_trainingDataSourceId,
    getMLModelResponse_lastUpdatedAt,
    getMLModelResponse_logUri,
    getMLModelResponse_httpStatus,

    -- ** GetEvaluation
    getEvaluation_evaluationId,
    getEvaluationResponse_performanceMetrics,
    getEvaluationResponse_status,
    getEvaluationResponse_startedAt,
    getEvaluationResponse_evaluationDataSourceId,
    getEvaluationResponse_message,
    getEvaluationResponse_createdAt,
    getEvaluationResponse_finishedAt,
    getEvaluationResponse_createdByIamUser,
    getEvaluationResponse_name,
    getEvaluationResponse_evaluationId,
    getEvaluationResponse_mLModelId,
    getEvaluationResponse_inputDataLocationS3,
    getEvaluationResponse_computeTime,
    getEvaluationResponse_lastUpdatedAt,
    getEvaluationResponse_logUri,
    getEvaluationResponse_httpStatus,

    -- ** DeleteTags
    deleteTags_tagKeys,
    deleteTags_resourceId,
    deleteTags_resourceType,
    deleteTagsResponse_resourceId,
    deleteTagsResponse_resourceType,
    deleteTagsResponse_httpStatus,

    -- ** DeleteRealtimeEndpoint
    deleteRealtimeEndpoint_mLModelId,
    deleteRealtimeEndpointResponse_realtimeEndpointInfo,
    deleteRealtimeEndpointResponse_mLModelId,
    deleteRealtimeEndpointResponse_httpStatus,

    -- ** CreateDataSourceFromRDS
    createDataSourceFromRDS_computeStatistics,
    createDataSourceFromRDS_dataSourceName,
    createDataSourceFromRDS_dataSourceId,
    createDataSourceFromRDS_rDSData,
    createDataSourceFromRDS_roleARN,
    createDataSourceFromRDSResponse_dataSourceId,
    createDataSourceFromRDSResponse_httpStatus,

    -- ** GetBatchPrediction
    getBatchPrediction_batchPredictionId,
    getBatchPredictionResponse_batchPredictionId,
    getBatchPredictionResponse_status,
    getBatchPredictionResponse_startedAt,
    getBatchPredictionResponse_outputUri,
    getBatchPredictionResponse_message,
    getBatchPredictionResponse_createdAt,
    getBatchPredictionResponse_finishedAt,
    getBatchPredictionResponse_createdByIamUser,
    getBatchPredictionResponse_name,
    getBatchPredictionResponse_invalidRecordCount,
    getBatchPredictionResponse_totalRecordCount,
    getBatchPredictionResponse_batchPredictionDataSourceId,
    getBatchPredictionResponse_mLModelId,
    getBatchPredictionResponse_inputDataLocationS3,
    getBatchPredictionResponse_computeTime,
    getBatchPredictionResponse_lastUpdatedAt,
    getBatchPredictionResponse_logUri,
    getBatchPredictionResponse_httpStatus,

    -- ** DescribeBatchPredictions
    describeBatchPredictions_sortOrder,
    describeBatchPredictions_eq,
    describeBatchPredictions_nextToken,
    describeBatchPredictions_filterVariable,
    describeBatchPredictions_gt,
    describeBatchPredictions_ne,
    describeBatchPredictions_prefix,
    describeBatchPredictions_ge,
    describeBatchPredictions_le,
    describeBatchPredictions_lt,
    describeBatchPredictions_limit,
    describeBatchPredictionsResponse_nextToken,
    describeBatchPredictionsResponse_results,
    describeBatchPredictionsResponse_httpStatus,

    -- ** DeleteEvaluation
    deleteEvaluation_evaluationId,
    deleteEvaluationResponse_evaluationId,
    deleteEvaluationResponse_httpStatus,

    -- ** UpdateEvaluation
    updateEvaluation_evaluationId,
    updateEvaluation_evaluationName,
    updateEvaluationResponse_evaluationId,
    updateEvaluationResponse_httpStatus,

    -- ** GetDataSource
    getDataSource_verbose,
    getDataSource_dataSourceId,
    getDataSourceResponse_status,
    getDataSourceResponse_startedAt,
    getDataSourceResponse_dataRearrangement,
    getDataSourceResponse_roleARN,
    getDataSourceResponse_redshiftMetadata,
    getDataSourceResponse_message,
    getDataSourceResponse_dataSourceId,
    getDataSourceResponse_computeStatistics,
    getDataSourceResponse_dataLocationS3,
    getDataSourceResponse_createdAt,
    getDataSourceResponse_numberOfFiles,
    getDataSourceResponse_finishedAt,
    getDataSourceResponse_createdByIamUser,
    getDataSourceResponse_name,
    getDataSourceResponse_dataSourceSchema,
    getDataSourceResponse_dataSizeInBytes,
    getDataSourceResponse_computeTime,
    getDataSourceResponse_rDSMetadata,
    getDataSourceResponse_lastUpdatedAt,
    getDataSourceResponse_logUri,
    getDataSourceResponse_httpStatus,

    -- ** CreateRealtimeEndpoint
    createRealtimeEndpoint_mLModelId,
    createRealtimeEndpointResponse_realtimeEndpointInfo,
    createRealtimeEndpointResponse_mLModelId,
    createRealtimeEndpointResponse_httpStatus,

    -- ** UpdateBatchPrediction
    updateBatchPrediction_batchPredictionId,
    updateBatchPrediction_batchPredictionName,
    updateBatchPredictionResponse_batchPredictionId,
    updateBatchPredictionResponse_httpStatus,

    -- ** DeleteBatchPrediction
    deleteBatchPrediction_batchPredictionId,
    deleteBatchPredictionResponse_batchPredictionId,
    deleteBatchPredictionResponse_httpStatus,

    -- ** DescribeMLModels
    describeMLModels_sortOrder,
    describeMLModels_eq,
    describeMLModels_nextToken,
    describeMLModels_filterVariable,
    describeMLModels_gt,
    describeMLModels_ne,
    describeMLModels_prefix,
    describeMLModels_ge,
    describeMLModels_le,
    describeMLModels_lt,
    describeMLModels_limit,
    describeMLModelsResponse_nextToken,
    describeMLModelsResponse_results,
    describeMLModelsResponse_httpStatus,

    -- ** CreateBatchPrediction
    createBatchPrediction_batchPredictionName,
    createBatchPrediction_batchPredictionId,
    createBatchPrediction_mLModelId,
    createBatchPrediction_batchPredictionDataSourceId,
    createBatchPrediction_outputUri,
    createBatchPredictionResponse_batchPredictionId,
    createBatchPredictionResponse_httpStatus,

    -- ** Predict
    predict_mLModelId,
    predict_record,
    predict_predictEndpoint,
    predictResponse_prediction,
    predictResponse_httpStatus,

    -- ** CreateMLModel
    createMLModel_recipeUri,
    createMLModel_recipe,
    createMLModel_mLModelName,
    createMLModel_parameters,
    createMLModel_mLModelId,
    createMLModel_mLModelType,
    createMLModel_trainingDataSourceId,
    createMLModelResponse_mLModelId,
    createMLModelResponse_httpStatus,

    -- ** CreateEvaluation
    createEvaluation_evaluationName,
    createEvaluation_evaluationId,
    createEvaluation_mLModelId,
    createEvaluation_evaluationDataSourceId,
    createEvaluationResponse_evaluationId,
    createEvaluationResponse_httpStatus,

    -- * Types

    -- ** BatchPrediction
    batchPrediction_batchPredictionId,
    batchPrediction_status,
    batchPrediction_startedAt,
    batchPrediction_outputUri,
    batchPrediction_message,
    batchPrediction_createdAt,
    batchPrediction_finishedAt,
    batchPrediction_createdByIamUser,
    batchPrediction_name,
    batchPrediction_invalidRecordCount,
    batchPrediction_totalRecordCount,
    batchPrediction_batchPredictionDataSourceId,
    batchPrediction_mLModelId,
    batchPrediction_inputDataLocationS3,
    batchPrediction_computeTime,
    batchPrediction_lastUpdatedAt,

    -- ** DataSource
    dataSource_status,
    dataSource_startedAt,
    dataSource_dataRearrangement,
    dataSource_roleARN,
    dataSource_redshiftMetadata,
    dataSource_message,
    dataSource_dataSourceId,
    dataSource_computeStatistics,
    dataSource_dataLocationS3,
    dataSource_createdAt,
    dataSource_numberOfFiles,
    dataSource_finishedAt,
    dataSource_createdByIamUser,
    dataSource_name,
    dataSource_dataSizeInBytes,
    dataSource_computeTime,
    dataSource_rDSMetadata,
    dataSource_lastUpdatedAt,

    -- ** Evaluation
    evaluation_performanceMetrics,
    evaluation_status,
    evaluation_startedAt,
    evaluation_evaluationDataSourceId,
    evaluation_message,
    evaluation_createdAt,
    evaluation_finishedAt,
    evaluation_createdByIamUser,
    evaluation_name,
    evaluation_evaluationId,
    evaluation_mLModelId,
    evaluation_inputDataLocationS3,
    evaluation_computeTime,
    evaluation_lastUpdatedAt,

    -- ** MLModel
    mLModel_algorithm,
    mLModel_status,
    mLModel_startedAt,
    mLModel_message,
    mLModel_endpointInfo,
    mLModel_scoreThresholdLastUpdatedAt,
    mLModel_createdAt,
    mLModel_trainingParameters,
    mLModel_finishedAt,
    mLModel_scoreThreshold,
    mLModel_createdByIamUser,
    mLModel_name,
    mLModel_mLModelType,
    mLModel_mLModelId,
    mLModel_sizeInBytes,
    mLModel_inputDataLocationS3,
    mLModel_computeTime,
    mLModel_trainingDataSourceId,
    mLModel_lastUpdatedAt,

    -- ** PerformanceMetrics
    performanceMetrics_properties,

    -- ** Prediction
    prediction_predictedValue,
    prediction_predictedScores,
    prediction_predictedLabel,
    prediction_details,

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
    rDSMetadata_selectSqlQuery,
    rDSMetadata_serviceRole,
    rDSMetadata_resourceRole,
    rDSMetadata_databaseUserName,
    rDSMetadata_database,

    -- ** RealtimeEndpointInfo
    realtimeEndpointInfo_createdAt,
    realtimeEndpointInfo_peakRequestsPerSecond,
    realtimeEndpointInfo_endpointStatus,
    realtimeEndpointInfo_endpointUrl,

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
    redshiftMetadata_selectSqlQuery,
    redshiftMetadata_redshiftDatabase,
    redshiftMetadata_databaseUserName,

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

import Network.AWS.MachineLearning.AddTags
import Network.AWS.MachineLearning.CreateBatchPrediction
import Network.AWS.MachineLearning.CreateDataSourceFromRDS
import Network.AWS.MachineLearning.CreateDataSourceFromRedshift
import Network.AWS.MachineLearning.CreateDataSourceFromS
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

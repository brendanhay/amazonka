{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Comprehend.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Lens
  ( -- * Operations

    -- ** BatchDetectDominantLanguage
    batchDetectDominantLanguage_textList,
    batchDetectDominantLanguageResponse_httpStatus,
    batchDetectDominantLanguageResponse_resultList,
    batchDetectDominantLanguageResponse_errorList,

    -- ** BatchDetectEntities
    batchDetectEntities_textList,
    batchDetectEntities_languageCode,
    batchDetectEntitiesResponse_httpStatus,
    batchDetectEntitiesResponse_resultList,
    batchDetectEntitiesResponse_errorList,

    -- ** BatchDetectKeyPhrases
    batchDetectKeyPhrases_textList,
    batchDetectKeyPhrases_languageCode,
    batchDetectKeyPhrasesResponse_httpStatus,
    batchDetectKeyPhrasesResponse_resultList,
    batchDetectKeyPhrasesResponse_errorList,

    -- ** BatchDetectSentiment
    batchDetectSentiment_textList,
    batchDetectSentiment_languageCode,
    batchDetectSentimentResponse_httpStatus,
    batchDetectSentimentResponse_resultList,
    batchDetectSentimentResponse_errorList,

    -- ** BatchDetectSyntax
    batchDetectSyntax_textList,
    batchDetectSyntax_languageCode,
    batchDetectSyntaxResponse_httpStatus,
    batchDetectSyntaxResponse_resultList,
    batchDetectSyntaxResponse_errorList,

    -- ** BatchDetectTargetedSentiment
    batchDetectTargetedSentiment_textList,
    batchDetectTargetedSentiment_languageCode,
    batchDetectTargetedSentimentResponse_httpStatus,
    batchDetectTargetedSentimentResponse_resultList,
    batchDetectTargetedSentimentResponse_errorList,

    -- ** ClassifyDocument
    classifyDocument_bytes,
    classifyDocument_documentReaderConfig,
    classifyDocument_text,
    classifyDocument_endpointArn,
    classifyDocumentResponse_classes,
    classifyDocumentResponse_documentMetadata,
    classifyDocumentResponse_documentType,
    classifyDocumentResponse_errors,
    classifyDocumentResponse_labels,
    classifyDocumentResponse_warnings,
    classifyDocumentResponse_httpStatus,

    -- ** ContainsPiiEntities
    containsPiiEntities_text,
    containsPiiEntities_languageCode,
    containsPiiEntitiesResponse_labels,
    containsPiiEntitiesResponse_httpStatus,

    -- ** CreateDataset
    createDataset_clientRequestToken,
    createDataset_datasetType,
    createDataset_description,
    createDataset_tags,
    createDataset_flywheelArn,
    createDataset_datasetName,
    createDataset_inputDataConfig,
    createDatasetResponse_datasetArn,
    createDatasetResponse_httpStatus,

    -- ** CreateDocumentClassifier
    createDocumentClassifier_clientRequestToken,
    createDocumentClassifier_mode,
    createDocumentClassifier_modelKmsKeyId,
    createDocumentClassifier_modelPolicy,
    createDocumentClassifier_outputDataConfig,
    createDocumentClassifier_tags,
    createDocumentClassifier_versionName,
    createDocumentClassifier_volumeKmsKeyId,
    createDocumentClassifier_vpcConfig,
    createDocumentClassifier_documentClassifierName,
    createDocumentClassifier_dataAccessRoleArn,
    createDocumentClassifier_inputDataConfig,
    createDocumentClassifier_languageCode,
    createDocumentClassifierResponse_documentClassifierArn,
    createDocumentClassifierResponse_httpStatus,

    -- ** CreateEndpoint
    createEndpoint_clientRequestToken,
    createEndpoint_dataAccessRoleArn,
    createEndpoint_flywheelArn,
    createEndpoint_modelArn,
    createEndpoint_tags,
    createEndpoint_endpointName,
    createEndpoint_desiredInferenceUnits,
    createEndpointResponse_endpointArn,
    createEndpointResponse_modelArn,
    createEndpointResponse_httpStatus,

    -- ** CreateEntityRecognizer
    createEntityRecognizer_clientRequestToken,
    createEntityRecognizer_modelKmsKeyId,
    createEntityRecognizer_modelPolicy,
    createEntityRecognizer_tags,
    createEntityRecognizer_versionName,
    createEntityRecognizer_volumeKmsKeyId,
    createEntityRecognizer_vpcConfig,
    createEntityRecognizer_recognizerName,
    createEntityRecognizer_dataAccessRoleArn,
    createEntityRecognizer_inputDataConfig,
    createEntityRecognizer_languageCode,
    createEntityRecognizerResponse_entityRecognizerArn,
    createEntityRecognizerResponse_httpStatus,

    -- ** CreateFlywheel
    createFlywheel_activeModelArn,
    createFlywheel_clientRequestToken,
    createFlywheel_dataSecurityConfig,
    createFlywheel_modelType,
    createFlywheel_tags,
    createFlywheel_taskConfig,
    createFlywheel_flywheelName,
    createFlywheel_dataAccessRoleArn,
    createFlywheel_dataLakeS3Uri,
    createFlywheelResponse_activeModelArn,
    createFlywheelResponse_flywheelArn,
    createFlywheelResponse_httpStatus,

    -- ** DeleteDocumentClassifier
    deleteDocumentClassifier_documentClassifierArn,
    deleteDocumentClassifierResponse_httpStatus,

    -- ** DeleteEndpoint
    deleteEndpoint_endpointArn,
    deleteEndpointResponse_httpStatus,

    -- ** DeleteEntityRecognizer
    deleteEntityRecognizer_entityRecognizerArn,
    deleteEntityRecognizerResponse_httpStatus,

    -- ** DeleteFlywheel
    deleteFlywheel_flywheelArn,
    deleteFlywheelResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_policyRevisionId,
    deleteResourcePolicy_resourceArn,
    deleteResourcePolicyResponse_httpStatus,

    -- ** DescribeDataset
    describeDataset_datasetArn,
    describeDatasetResponse_datasetProperties,
    describeDatasetResponse_httpStatus,

    -- ** DescribeDocumentClassificationJob
    describeDocumentClassificationJob_jobId,
    describeDocumentClassificationJobResponse_documentClassificationJobProperties,
    describeDocumentClassificationJobResponse_httpStatus,

    -- ** DescribeDocumentClassifier
    describeDocumentClassifier_documentClassifierArn,
    describeDocumentClassifierResponse_documentClassifierProperties,
    describeDocumentClassifierResponse_httpStatus,

    -- ** DescribeDominantLanguageDetectionJob
    describeDominantLanguageDetectionJob_jobId,
    describeDominantLanguageDetectionJobResponse_dominantLanguageDetectionJobProperties,
    describeDominantLanguageDetectionJobResponse_httpStatus,

    -- ** DescribeEndpoint
    describeEndpoint_endpointArn,
    describeEndpointResponse_endpointProperties,
    describeEndpointResponse_httpStatus,

    -- ** DescribeEntitiesDetectionJob
    describeEntitiesDetectionJob_jobId,
    describeEntitiesDetectionJobResponse_entitiesDetectionJobProperties,
    describeEntitiesDetectionJobResponse_httpStatus,

    -- ** DescribeEntityRecognizer
    describeEntityRecognizer_entityRecognizerArn,
    describeEntityRecognizerResponse_entityRecognizerProperties,
    describeEntityRecognizerResponse_httpStatus,

    -- ** DescribeEventsDetectionJob
    describeEventsDetectionJob_jobId,
    describeEventsDetectionJobResponse_eventsDetectionJobProperties,
    describeEventsDetectionJobResponse_httpStatus,

    -- ** DescribeFlywheel
    describeFlywheel_flywheelArn,
    describeFlywheelResponse_flywheelProperties,
    describeFlywheelResponse_httpStatus,

    -- ** DescribeFlywheelIteration
    describeFlywheelIteration_flywheelArn,
    describeFlywheelIteration_flywheelIterationId,
    describeFlywheelIterationResponse_flywheelIterationProperties,
    describeFlywheelIterationResponse_httpStatus,

    -- ** DescribeKeyPhrasesDetectionJob
    describeKeyPhrasesDetectionJob_jobId,
    describeKeyPhrasesDetectionJobResponse_keyPhrasesDetectionJobProperties,
    describeKeyPhrasesDetectionJobResponse_httpStatus,

    -- ** DescribePiiEntitiesDetectionJob
    describePiiEntitiesDetectionJob_jobId,
    describePiiEntitiesDetectionJobResponse_piiEntitiesDetectionJobProperties,
    describePiiEntitiesDetectionJobResponse_httpStatus,

    -- ** DescribeResourcePolicy
    describeResourcePolicy_resourceArn,
    describeResourcePolicyResponse_creationTime,
    describeResourcePolicyResponse_lastModifiedTime,
    describeResourcePolicyResponse_policyRevisionId,
    describeResourcePolicyResponse_resourcePolicy,
    describeResourcePolicyResponse_httpStatus,

    -- ** DescribeSentimentDetectionJob
    describeSentimentDetectionJob_jobId,
    describeSentimentDetectionJobResponse_sentimentDetectionJobProperties,
    describeSentimentDetectionJobResponse_httpStatus,

    -- ** DescribeTargetedSentimentDetectionJob
    describeTargetedSentimentDetectionJob_jobId,
    describeTargetedSentimentDetectionJobResponse_targetedSentimentDetectionJobProperties,
    describeTargetedSentimentDetectionJobResponse_httpStatus,

    -- ** DescribeTopicsDetectionJob
    describeTopicsDetectionJob_jobId,
    describeTopicsDetectionJobResponse_topicsDetectionJobProperties,
    describeTopicsDetectionJobResponse_httpStatus,

    -- ** DetectDominantLanguage
    detectDominantLanguage_text,
    detectDominantLanguageResponse_languages,
    detectDominantLanguageResponse_httpStatus,

    -- ** DetectEntities
    detectEntities_bytes,
    detectEntities_documentReaderConfig,
    detectEntities_endpointArn,
    detectEntities_languageCode,
    detectEntities_text,
    detectEntitiesResponse_blocks,
    detectEntitiesResponse_documentMetadata,
    detectEntitiesResponse_documentType,
    detectEntitiesResponse_entities,
    detectEntitiesResponse_errors,
    detectEntitiesResponse_httpStatus,

    -- ** DetectKeyPhrases
    detectKeyPhrases_text,
    detectKeyPhrases_languageCode,
    detectKeyPhrasesResponse_keyPhrases,
    detectKeyPhrasesResponse_httpStatus,

    -- ** DetectPiiEntities
    detectPiiEntities_text,
    detectPiiEntities_languageCode,
    detectPiiEntitiesResponse_entities,
    detectPiiEntitiesResponse_httpStatus,

    -- ** DetectSentiment
    detectSentiment_text,
    detectSentiment_languageCode,
    detectSentimentResponse_sentiment,
    detectSentimentResponse_sentimentScore,
    detectSentimentResponse_httpStatus,

    -- ** DetectSyntax
    detectSyntax_text,
    detectSyntax_languageCode,
    detectSyntaxResponse_syntaxTokens,
    detectSyntaxResponse_httpStatus,

    -- ** DetectTargetedSentiment
    detectTargetedSentiment_text,
    detectTargetedSentiment_languageCode,
    detectTargetedSentimentResponse_entities,
    detectTargetedSentimentResponse_httpStatus,

    -- ** ImportModel
    importModel_dataAccessRoleArn,
    importModel_modelKmsKeyId,
    importModel_modelName,
    importModel_tags,
    importModel_versionName,
    importModel_sourceModelArn,
    importModelResponse_modelArn,
    importModelResponse_httpStatus,

    -- ** ListDatasets
    listDatasets_filter,
    listDatasets_flywheelArn,
    listDatasets_maxResults,
    listDatasets_nextToken,
    listDatasetsResponse_datasetPropertiesList,
    listDatasetsResponse_nextToken,
    listDatasetsResponse_httpStatus,

    -- ** ListDocumentClassificationJobs
    listDocumentClassificationJobs_filter,
    listDocumentClassificationJobs_maxResults,
    listDocumentClassificationJobs_nextToken,
    listDocumentClassificationJobsResponse_documentClassificationJobPropertiesList,
    listDocumentClassificationJobsResponse_nextToken,
    listDocumentClassificationJobsResponse_httpStatus,

    -- ** ListDocumentClassifierSummaries
    listDocumentClassifierSummaries_maxResults,
    listDocumentClassifierSummaries_nextToken,
    listDocumentClassifierSummariesResponse_documentClassifierSummariesList,
    listDocumentClassifierSummariesResponse_nextToken,
    listDocumentClassifierSummariesResponse_httpStatus,

    -- ** ListDocumentClassifiers
    listDocumentClassifiers_filter,
    listDocumentClassifiers_maxResults,
    listDocumentClassifiers_nextToken,
    listDocumentClassifiersResponse_documentClassifierPropertiesList,
    listDocumentClassifiersResponse_nextToken,
    listDocumentClassifiersResponse_httpStatus,

    -- ** ListDominantLanguageDetectionJobs
    listDominantLanguageDetectionJobs_filter,
    listDominantLanguageDetectionJobs_maxResults,
    listDominantLanguageDetectionJobs_nextToken,
    listDominantLanguageDetectionJobsResponse_dominantLanguageDetectionJobPropertiesList,
    listDominantLanguageDetectionJobsResponse_nextToken,
    listDominantLanguageDetectionJobsResponse_httpStatus,

    -- ** ListEndpoints
    listEndpoints_filter,
    listEndpoints_maxResults,
    listEndpoints_nextToken,
    listEndpointsResponse_endpointPropertiesList,
    listEndpointsResponse_nextToken,
    listEndpointsResponse_httpStatus,

    -- ** ListEntitiesDetectionJobs
    listEntitiesDetectionJobs_filter,
    listEntitiesDetectionJobs_maxResults,
    listEntitiesDetectionJobs_nextToken,
    listEntitiesDetectionJobsResponse_entitiesDetectionJobPropertiesList,
    listEntitiesDetectionJobsResponse_nextToken,
    listEntitiesDetectionJobsResponse_httpStatus,

    -- ** ListEntityRecognizerSummaries
    listEntityRecognizerSummaries_maxResults,
    listEntityRecognizerSummaries_nextToken,
    listEntityRecognizerSummariesResponse_entityRecognizerSummariesList,
    listEntityRecognizerSummariesResponse_nextToken,
    listEntityRecognizerSummariesResponse_httpStatus,

    -- ** ListEntityRecognizers
    listEntityRecognizers_filter,
    listEntityRecognizers_maxResults,
    listEntityRecognizers_nextToken,
    listEntityRecognizersResponse_entityRecognizerPropertiesList,
    listEntityRecognizersResponse_nextToken,
    listEntityRecognizersResponse_httpStatus,

    -- ** ListEventsDetectionJobs
    listEventsDetectionJobs_filter,
    listEventsDetectionJobs_maxResults,
    listEventsDetectionJobs_nextToken,
    listEventsDetectionJobsResponse_eventsDetectionJobPropertiesList,
    listEventsDetectionJobsResponse_nextToken,
    listEventsDetectionJobsResponse_httpStatus,

    -- ** ListFlywheelIterationHistory
    listFlywheelIterationHistory_filter,
    listFlywheelIterationHistory_maxResults,
    listFlywheelIterationHistory_nextToken,
    listFlywheelIterationHistory_flywheelArn,
    listFlywheelIterationHistoryResponse_flywheelIterationPropertiesList,
    listFlywheelIterationHistoryResponse_nextToken,
    listFlywheelIterationHistoryResponse_httpStatus,

    -- ** ListFlywheels
    listFlywheels_filter,
    listFlywheels_maxResults,
    listFlywheels_nextToken,
    listFlywheelsResponse_flywheelSummaryList,
    listFlywheelsResponse_nextToken,
    listFlywheelsResponse_httpStatus,

    -- ** ListKeyPhrasesDetectionJobs
    listKeyPhrasesDetectionJobs_filter,
    listKeyPhrasesDetectionJobs_maxResults,
    listKeyPhrasesDetectionJobs_nextToken,
    listKeyPhrasesDetectionJobsResponse_keyPhrasesDetectionJobPropertiesList,
    listKeyPhrasesDetectionJobsResponse_nextToken,
    listKeyPhrasesDetectionJobsResponse_httpStatus,

    -- ** ListPiiEntitiesDetectionJobs
    listPiiEntitiesDetectionJobs_filter,
    listPiiEntitiesDetectionJobs_maxResults,
    listPiiEntitiesDetectionJobs_nextToken,
    listPiiEntitiesDetectionJobsResponse_nextToken,
    listPiiEntitiesDetectionJobsResponse_piiEntitiesDetectionJobPropertiesList,
    listPiiEntitiesDetectionJobsResponse_httpStatus,

    -- ** ListSentimentDetectionJobs
    listSentimentDetectionJobs_filter,
    listSentimentDetectionJobs_maxResults,
    listSentimentDetectionJobs_nextToken,
    listSentimentDetectionJobsResponse_nextToken,
    listSentimentDetectionJobsResponse_sentimentDetectionJobPropertiesList,
    listSentimentDetectionJobsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTargetedSentimentDetectionJobs
    listTargetedSentimentDetectionJobs_filter,
    listTargetedSentimentDetectionJobs_maxResults,
    listTargetedSentimentDetectionJobs_nextToken,
    listTargetedSentimentDetectionJobsResponse_nextToken,
    listTargetedSentimentDetectionJobsResponse_targetedSentimentDetectionJobPropertiesList,
    listTargetedSentimentDetectionJobsResponse_httpStatus,

    -- ** ListTopicsDetectionJobs
    listTopicsDetectionJobs_filter,
    listTopicsDetectionJobs_maxResults,
    listTopicsDetectionJobs_nextToken,
    listTopicsDetectionJobsResponse_nextToken,
    listTopicsDetectionJobsResponse_topicsDetectionJobPropertiesList,
    listTopicsDetectionJobsResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_policyRevisionId,
    putResourcePolicy_resourceArn,
    putResourcePolicy_resourcePolicy,
    putResourcePolicyResponse_policyRevisionId,
    putResourcePolicyResponse_httpStatus,

    -- ** StartDocumentClassificationJob
    startDocumentClassificationJob_clientRequestToken,
    startDocumentClassificationJob_documentClassifierArn,
    startDocumentClassificationJob_flywheelArn,
    startDocumentClassificationJob_jobName,
    startDocumentClassificationJob_tags,
    startDocumentClassificationJob_volumeKmsKeyId,
    startDocumentClassificationJob_vpcConfig,
    startDocumentClassificationJob_inputDataConfig,
    startDocumentClassificationJob_outputDataConfig,
    startDocumentClassificationJob_dataAccessRoleArn,
    startDocumentClassificationJobResponse_documentClassifierArn,
    startDocumentClassificationJobResponse_jobArn,
    startDocumentClassificationJobResponse_jobId,
    startDocumentClassificationJobResponse_jobStatus,
    startDocumentClassificationJobResponse_httpStatus,

    -- ** StartDominantLanguageDetectionJob
    startDominantLanguageDetectionJob_clientRequestToken,
    startDominantLanguageDetectionJob_jobName,
    startDominantLanguageDetectionJob_tags,
    startDominantLanguageDetectionJob_volumeKmsKeyId,
    startDominantLanguageDetectionJob_vpcConfig,
    startDominantLanguageDetectionJob_inputDataConfig,
    startDominantLanguageDetectionJob_outputDataConfig,
    startDominantLanguageDetectionJob_dataAccessRoleArn,
    startDominantLanguageDetectionJobResponse_jobArn,
    startDominantLanguageDetectionJobResponse_jobId,
    startDominantLanguageDetectionJobResponse_jobStatus,
    startDominantLanguageDetectionJobResponse_httpStatus,

    -- ** StartEntitiesDetectionJob
    startEntitiesDetectionJob_clientRequestToken,
    startEntitiesDetectionJob_entityRecognizerArn,
    startEntitiesDetectionJob_flywheelArn,
    startEntitiesDetectionJob_jobName,
    startEntitiesDetectionJob_tags,
    startEntitiesDetectionJob_volumeKmsKeyId,
    startEntitiesDetectionJob_vpcConfig,
    startEntitiesDetectionJob_inputDataConfig,
    startEntitiesDetectionJob_outputDataConfig,
    startEntitiesDetectionJob_dataAccessRoleArn,
    startEntitiesDetectionJob_languageCode,
    startEntitiesDetectionJobResponse_entityRecognizerArn,
    startEntitiesDetectionJobResponse_jobArn,
    startEntitiesDetectionJobResponse_jobId,
    startEntitiesDetectionJobResponse_jobStatus,
    startEntitiesDetectionJobResponse_httpStatus,

    -- ** StartEventsDetectionJob
    startEventsDetectionJob_clientRequestToken,
    startEventsDetectionJob_jobName,
    startEventsDetectionJob_tags,
    startEventsDetectionJob_inputDataConfig,
    startEventsDetectionJob_outputDataConfig,
    startEventsDetectionJob_dataAccessRoleArn,
    startEventsDetectionJob_languageCode,
    startEventsDetectionJob_targetEventTypes,
    startEventsDetectionJobResponse_jobArn,
    startEventsDetectionJobResponse_jobId,
    startEventsDetectionJobResponse_jobStatus,
    startEventsDetectionJobResponse_httpStatus,

    -- ** StartFlywheelIteration
    startFlywheelIteration_clientRequestToken,
    startFlywheelIteration_flywheelArn,
    startFlywheelIterationResponse_flywheelArn,
    startFlywheelIterationResponse_flywheelIterationId,
    startFlywheelIterationResponse_httpStatus,

    -- ** StartKeyPhrasesDetectionJob
    startKeyPhrasesDetectionJob_clientRequestToken,
    startKeyPhrasesDetectionJob_jobName,
    startKeyPhrasesDetectionJob_tags,
    startKeyPhrasesDetectionJob_volumeKmsKeyId,
    startKeyPhrasesDetectionJob_vpcConfig,
    startKeyPhrasesDetectionJob_inputDataConfig,
    startKeyPhrasesDetectionJob_outputDataConfig,
    startKeyPhrasesDetectionJob_dataAccessRoleArn,
    startKeyPhrasesDetectionJob_languageCode,
    startKeyPhrasesDetectionJobResponse_jobArn,
    startKeyPhrasesDetectionJobResponse_jobId,
    startKeyPhrasesDetectionJobResponse_jobStatus,
    startKeyPhrasesDetectionJobResponse_httpStatus,

    -- ** StartPiiEntitiesDetectionJob
    startPiiEntitiesDetectionJob_clientRequestToken,
    startPiiEntitiesDetectionJob_jobName,
    startPiiEntitiesDetectionJob_redactionConfig,
    startPiiEntitiesDetectionJob_tags,
    startPiiEntitiesDetectionJob_inputDataConfig,
    startPiiEntitiesDetectionJob_outputDataConfig,
    startPiiEntitiesDetectionJob_mode,
    startPiiEntitiesDetectionJob_dataAccessRoleArn,
    startPiiEntitiesDetectionJob_languageCode,
    startPiiEntitiesDetectionJobResponse_jobArn,
    startPiiEntitiesDetectionJobResponse_jobId,
    startPiiEntitiesDetectionJobResponse_jobStatus,
    startPiiEntitiesDetectionJobResponse_httpStatus,

    -- ** StartSentimentDetectionJob
    startSentimentDetectionJob_clientRequestToken,
    startSentimentDetectionJob_jobName,
    startSentimentDetectionJob_tags,
    startSentimentDetectionJob_volumeKmsKeyId,
    startSentimentDetectionJob_vpcConfig,
    startSentimentDetectionJob_inputDataConfig,
    startSentimentDetectionJob_outputDataConfig,
    startSentimentDetectionJob_dataAccessRoleArn,
    startSentimentDetectionJob_languageCode,
    startSentimentDetectionJobResponse_jobArn,
    startSentimentDetectionJobResponse_jobId,
    startSentimentDetectionJobResponse_jobStatus,
    startSentimentDetectionJobResponse_httpStatus,

    -- ** StartTargetedSentimentDetectionJob
    startTargetedSentimentDetectionJob_clientRequestToken,
    startTargetedSentimentDetectionJob_jobName,
    startTargetedSentimentDetectionJob_tags,
    startTargetedSentimentDetectionJob_volumeKmsKeyId,
    startTargetedSentimentDetectionJob_vpcConfig,
    startTargetedSentimentDetectionJob_inputDataConfig,
    startTargetedSentimentDetectionJob_outputDataConfig,
    startTargetedSentimentDetectionJob_dataAccessRoleArn,
    startTargetedSentimentDetectionJob_languageCode,
    startTargetedSentimentDetectionJobResponse_jobArn,
    startTargetedSentimentDetectionJobResponse_jobId,
    startTargetedSentimentDetectionJobResponse_jobStatus,
    startTargetedSentimentDetectionJobResponse_httpStatus,

    -- ** StartTopicsDetectionJob
    startTopicsDetectionJob_clientRequestToken,
    startTopicsDetectionJob_jobName,
    startTopicsDetectionJob_numberOfTopics,
    startTopicsDetectionJob_tags,
    startTopicsDetectionJob_volumeKmsKeyId,
    startTopicsDetectionJob_vpcConfig,
    startTopicsDetectionJob_inputDataConfig,
    startTopicsDetectionJob_outputDataConfig,
    startTopicsDetectionJob_dataAccessRoleArn,
    startTopicsDetectionJobResponse_jobArn,
    startTopicsDetectionJobResponse_jobId,
    startTopicsDetectionJobResponse_jobStatus,
    startTopicsDetectionJobResponse_httpStatus,

    -- ** StopDominantLanguageDetectionJob
    stopDominantLanguageDetectionJob_jobId,
    stopDominantLanguageDetectionJobResponse_jobId,
    stopDominantLanguageDetectionJobResponse_jobStatus,
    stopDominantLanguageDetectionJobResponse_httpStatus,

    -- ** StopEntitiesDetectionJob
    stopEntitiesDetectionJob_jobId,
    stopEntitiesDetectionJobResponse_jobId,
    stopEntitiesDetectionJobResponse_jobStatus,
    stopEntitiesDetectionJobResponse_httpStatus,

    -- ** StopEventsDetectionJob
    stopEventsDetectionJob_jobId,
    stopEventsDetectionJobResponse_jobId,
    stopEventsDetectionJobResponse_jobStatus,
    stopEventsDetectionJobResponse_httpStatus,

    -- ** StopKeyPhrasesDetectionJob
    stopKeyPhrasesDetectionJob_jobId,
    stopKeyPhrasesDetectionJobResponse_jobId,
    stopKeyPhrasesDetectionJobResponse_jobStatus,
    stopKeyPhrasesDetectionJobResponse_httpStatus,

    -- ** StopPiiEntitiesDetectionJob
    stopPiiEntitiesDetectionJob_jobId,
    stopPiiEntitiesDetectionJobResponse_jobId,
    stopPiiEntitiesDetectionJobResponse_jobStatus,
    stopPiiEntitiesDetectionJobResponse_httpStatus,

    -- ** StopSentimentDetectionJob
    stopSentimentDetectionJob_jobId,
    stopSentimentDetectionJobResponse_jobId,
    stopSentimentDetectionJobResponse_jobStatus,
    stopSentimentDetectionJobResponse_httpStatus,

    -- ** StopTargetedSentimentDetectionJob
    stopTargetedSentimentDetectionJob_jobId,
    stopTargetedSentimentDetectionJobResponse_jobId,
    stopTargetedSentimentDetectionJobResponse_jobStatus,
    stopTargetedSentimentDetectionJobResponse_httpStatus,

    -- ** StopTrainingDocumentClassifier
    stopTrainingDocumentClassifier_documentClassifierArn,
    stopTrainingDocumentClassifierResponse_httpStatus,

    -- ** StopTrainingEntityRecognizer
    stopTrainingEntityRecognizer_entityRecognizerArn,
    stopTrainingEntityRecognizerResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateEndpoint
    updateEndpoint_desiredDataAccessRoleArn,
    updateEndpoint_desiredInferenceUnits,
    updateEndpoint_desiredModelArn,
    updateEndpoint_flywheelArn,
    updateEndpoint_endpointArn,
    updateEndpointResponse_desiredModelArn,
    updateEndpointResponse_httpStatus,

    -- ** UpdateFlywheel
    updateFlywheel_activeModelArn,
    updateFlywheel_dataAccessRoleArn,
    updateFlywheel_dataSecurityConfig,
    updateFlywheel_flywheelArn,
    updateFlywheelResponse_flywheelProperties,
    updateFlywheelResponse_httpStatus,

    -- * Types

    -- ** AugmentedManifestsListItem
    augmentedManifestsListItem_annotationDataS3Uri,
    augmentedManifestsListItem_documentType,
    augmentedManifestsListItem_sourceDocumentsS3Uri,
    augmentedManifestsListItem_split,
    augmentedManifestsListItem_s3Uri,
    augmentedManifestsListItem_attributeNames,

    -- ** BatchDetectDominantLanguageItemResult
    batchDetectDominantLanguageItemResult_index,
    batchDetectDominantLanguageItemResult_languages,

    -- ** BatchDetectEntitiesItemResult
    batchDetectEntitiesItemResult_entities,
    batchDetectEntitiesItemResult_index,

    -- ** BatchDetectKeyPhrasesItemResult
    batchDetectKeyPhrasesItemResult_index,
    batchDetectKeyPhrasesItemResult_keyPhrases,

    -- ** BatchDetectSentimentItemResult
    batchDetectSentimentItemResult_index,
    batchDetectSentimentItemResult_sentiment,
    batchDetectSentimentItemResult_sentimentScore,

    -- ** BatchDetectSyntaxItemResult
    batchDetectSyntaxItemResult_index,
    batchDetectSyntaxItemResult_syntaxTokens,

    -- ** BatchDetectTargetedSentimentItemResult
    batchDetectTargetedSentimentItemResult_entities,
    batchDetectTargetedSentimentItemResult_index,

    -- ** BatchItemError
    batchItemError_errorCode,
    batchItemError_errorMessage,
    batchItemError_index,

    -- ** Block
    block_blockType,
    block_geometry,
    block_id,
    block_page,
    block_relationships,
    block_text,

    -- ** BlockReference
    blockReference_beginOffset,
    blockReference_blockId,
    blockReference_childBlocks,
    blockReference_endOffset,

    -- ** BoundingBox
    boundingBox_height,
    boundingBox_left,
    boundingBox_top,
    boundingBox_width,

    -- ** ChildBlock
    childBlock_beginOffset,
    childBlock_childBlockId,
    childBlock_endOffset,

    -- ** ClassifierEvaluationMetrics
    classifierEvaluationMetrics_accuracy,
    classifierEvaluationMetrics_f1Score,
    classifierEvaluationMetrics_hammingLoss,
    classifierEvaluationMetrics_microF1Score,
    classifierEvaluationMetrics_microPrecision,
    classifierEvaluationMetrics_microRecall,
    classifierEvaluationMetrics_precision,
    classifierEvaluationMetrics_recall,

    -- ** ClassifierMetadata
    classifierMetadata_evaluationMetrics,
    classifierMetadata_numberOfLabels,
    classifierMetadata_numberOfTestDocuments,
    classifierMetadata_numberOfTrainedDocuments,

    -- ** DataSecurityConfig
    dataSecurityConfig_dataLakeKmsKeyId,
    dataSecurityConfig_modelKmsKeyId,
    dataSecurityConfig_volumeKmsKeyId,
    dataSecurityConfig_vpcConfig,

    -- ** DatasetAugmentedManifestsListItem
    datasetAugmentedManifestsListItem_annotationDataS3Uri,
    datasetAugmentedManifestsListItem_documentType,
    datasetAugmentedManifestsListItem_sourceDocumentsS3Uri,
    datasetAugmentedManifestsListItem_attributeNames,
    datasetAugmentedManifestsListItem_s3Uri,

    -- ** DatasetDocumentClassifierInputDataConfig
    datasetDocumentClassifierInputDataConfig_labelDelimiter,
    datasetDocumentClassifierInputDataConfig_s3Uri,

    -- ** DatasetEntityRecognizerAnnotations
    datasetEntityRecognizerAnnotations_s3Uri,

    -- ** DatasetEntityRecognizerDocuments
    datasetEntityRecognizerDocuments_inputFormat,
    datasetEntityRecognizerDocuments_s3Uri,

    -- ** DatasetEntityRecognizerEntityList
    datasetEntityRecognizerEntityList_s3Uri,

    -- ** DatasetEntityRecognizerInputDataConfig
    datasetEntityRecognizerInputDataConfig_annotations,
    datasetEntityRecognizerInputDataConfig_entityList,
    datasetEntityRecognizerInputDataConfig_documents,

    -- ** DatasetFilter
    datasetFilter_creationTimeAfter,
    datasetFilter_creationTimeBefore,
    datasetFilter_datasetType,
    datasetFilter_status,

    -- ** DatasetInputDataConfig
    datasetInputDataConfig_augmentedManifests,
    datasetInputDataConfig_dataFormat,
    datasetInputDataConfig_documentClassifierInputDataConfig,
    datasetInputDataConfig_entityRecognizerInputDataConfig,

    -- ** DatasetProperties
    datasetProperties_creationTime,
    datasetProperties_datasetArn,
    datasetProperties_datasetName,
    datasetProperties_datasetS3Uri,
    datasetProperties_datasetType,
    datasetProperties_description,
    datasetProperties_endTime,
    datasetProperties_message,
    datasetProperties_numberOfDocuments,
    datasetProperties_status,

    -- ** DocumentClass
    documentClass_name,
    documentClass_page,
    documentClass_score,

    -- ** DocumentClassificationConfig
    documentClassificationConfig_labels,
    documentClassificationConfig_mode,

    -- ** DocumentClassificationJobFilter
    documentClassificationJobFilter_jobName,
    documentClassificationJobFilter_jobStatus,
    documentClassificationJobFilter_submitTimeAfter,
    documentClassificationJobFilter_submitTimeBefore,

    -- ** DocumentClassificationJobProperties
    documentClassificationJobProperties_dataAccessRoleArn,
    documentClassificationJobProperties_documentClassifierArn,
    documentClassificationJobProperties_endTime,
    documentClassificationJobProperties_flywheelArn,
    documentClassificationJobProperties_inputDataConfig,
    documentClassificationJobProperties_jobArn,
    documentClassificationJobProperties_jobId,
    documentClassificationJobProperties_jobName,
    documentClassificationJobProperties_jobStatus,
    documentClassificationJobProperties_message,
    documentClassificationJobProperties_outputDataConfig,
    documentClassificationJobProperties_submitTime,
    documentClassificationJobProperties_volumeKmsKeyId,
    documentClassificationJobProperties_vpcConfig,

    -- ** DocumentClassifierDocuments
    documentClassifierDocuments_testS3Uri,
    documentClassifierDocuments_s3Uri,

    -- ** DocumentClassifierFilter
    documentClassifierFilter_documentClassifierName,
    documentClassifierFilter_status,
    documentClassifierFilter_submitTimeAfter,
    documentClassifierFilter_submitTimeBefore,

    -- ** DocumentClassifierInputDataConfig
    documentClassifierInputDataConfig_augmentedManifests,
    documentClassifierInputDataConfig_dataFormat,
    documentClassifierInputDataConfig_documentReaderConfig,
    documentClassifierInputDataConfig_documentType,
    documentClassifierInputDataConfig_documents,
    documentClassifierInputDataConfig_labelDelimiter,
    documentClassifierInputDataConfig_s3Uri,
    documentClassifierInputDataConfig_testS3Uri,

    -- ** DocumentClassifierOutputDataConfig
    documentClassifierOutputDataConfig_flywheelStatsS3Prefix,
    documentClassifierOutputDataConfig_kmsKeyId,
    documentClassifierOutputDataConfig_s3Uri,

    -- ** DocumentClassifierProperties
    documentClassifierProperties_classifierMetadata,
    documentClassifierProperties_dataAccessRoleArn,
    documentClassifierProperties_documentClassifierArn,
    documentClassifierProperties_endTime,
    documentClassifierProperties_flywheelArn,
    documentClassifierProperties_inputDataConfig,
    documentClassifierProperties_languageCode,
    documentClassifierProperties_message,
    documentClassifierProperties_mode,
    documentClassifierProperties_modelKmsKeyId,
    documentClassifierProperties_outputDataConfig,
    documentClassifierProperties_sourceModelArn,
    documentClassifierProperties_status,
    documentClassifierProperties_submitTime,
    documentClassifierProperties_trainingEndTime,
    documentClassifierProperties_trainingStartTime,
    documentClassifierProperties_versionName,
    documentClassifierProperties_volumeKmsKeyId,
    documentClassifierProperties_vpcConfig,

    -- ** DocumentClassifierSummary
    documentClassifierSummary_documentClassifierName,
    documentClassifierSummary_latestVersionCreatedAt,
    documentClassifierSummary_latestVersionName,
    documentClassifierSummary_latestVersionStatus,
    documentClassifierSummary_numberOfVersions,

    -- ** DocumentLabel
    documentLabel_name,
    documentLabel_page,
    documentLabel_score,

    -- ** DocumentMetadata
    documentMetadata_extractedCharacters,
    documentMetadata_pages,

    -- ** DocumentReaderConfig
    documentReaderConfig_documentReadMode,
    documentReaderConfig_featureTypes,
    documentReaderConfig_documentReadAction,

    -- ** DocumentTypeListItem
    documentTypeListItem_page,
    documentTypeListItem_type,

    -- ** DominantLanguage
    dominantLanguage_languageCode,
    dominantLanguage_score,

    -- ** DominantLanguageDetectionJobFilter
    dominantLanguageDetectionJobFilter_jobName,
    dominantLanguageDetectionJobFilter_jobStatus,
    dominantLanguageDetectionJobFilter_submitTimeAfter,
    dominantLanguageDetectionJobFilter_submitTimeBefore,

    -- ** DominantLanguageDetectionJobProperties
    dominantLanguageDetectionJobProperties_dataAccessRoleArn,
    dominantLanguageDetectionJobProperties_endTime,
    dominantLanguageDetectionJobProperties_inputDataConfig,
    dominantLanguageDetectionJobProperties_jobArn,
    dominantLanguageDetectionJobProperties_jobId,
    dominantLanguageDetectionJobProperties_jobName,
    dominantLanguageDetectionJobProperties_jobStatus,
    dominantLanguageDetectionJobProperties_message,
    dominantLanguageDetectionJobProperties_outputDataConfig,
    dominantLanguageDetectionJobProperties_submitTime,
    dominantLanguageDetectionJobProperties_volumeKmsKeyId,
    dominantLanguageDetectionJobProperties_vpcConfig,

    -- ** EndpointFilter
    endpointFilter_creationTimeAfter,
    endpointFilter_creationTimeBefore,
    endpointFilter_modelArn,
    endpointFilter_status,

    -- ** EndpointProperties
    endpointProperties_creationTime,
    endpointProperties_currentInferenceUnits,
    endpointProperties_dataAccessRoleArn,
    endpointProperties_desiredDataAccessRoleArn,
    endpointProperties_desiredInferenceUnits,
    endpointProperties_desiredModelArn,
    endpointProperties_endpointArn,
    endpointProperties_flywheelArn,
    endpointProperties_lastModifiedTime,
    endpointProperties_message,
    endpointProperties_modelArn,
    endpointProperties_status,

    -- ** EntitiesDetectionJobFilter
    entitiesDetectionJobFilter_jobName,
    entitiesDetectionJobFilter_jobStatus,
    entitiesDetectionJobFilter_submitTimeAfter,
    entitiesDetectionJobFilter_submitTimeBefore,

    -- ** EntitiesDetectionJobProperties
    entitiesDetectionJobProperties_dataAccessRoleArn,
    entitiesDetectionJobProperties_endTime,
    entitiesDetectionJobProperties_entityRecognizerArn,
    entitiesDetectionJobProperties_flywheelArn,
    entitiesDetectionJobProperties_inputDataConfig,
    entitiesDetectionJobProperties_jobArn,
    entitiesDetectionJobProperties_jobId,
    entitiesDetectionJobProperties_jobName,
    entitiesDetectionJobProperties_jobStatus,
    entitiesDetectionJobProperties_languageCode,
    entitiesDetectionJobProperties_message,
    entitiesDetectionJobProperties_outputDataConfig,
    entitiesDetectionJobProperties_submitTime,
    entitiesDetectionJobProperties_volumeKmsKeyId,
    entitiesDetectionJobProperties_vpcConfig,

    -- ** Entity
    entity_beginOffset,
    entity_blockReferences,
    entity_endOffset,
    entity_score,
    entity_text,
    entity_type,

    -- ** EntityLabel
    entityLabel_name,
    entityLabel_score,

    -- ** EntityRecognitionConfig
    entityRecognitionConfig_entityTypes,

    -- ** EntityRecognizerAnnotations
    entityRecognizerAnnotations_testS3Uri,
    entityRecognizerAnnotations_s3Uri,

    -- ** EntityRecognizerDocuments
    entityRecognizerDocuments_inputFormat,
    entityRecognizerDocuments_testS3Uri,
    entityRecognizerDocuments_s3Uri,

    -- ** EntityRecognizerEntityList
    entityRecognizerEntityList_s3Uri,

    -- ** EntityRecognizerEvaluationMetrics
    entityRecognizerEvaluationMetrics_f1Score,
    entityRecognizerEvaluationMetrics_precision,
    entityRecognizerEvaluationMetrics_recall,

    -- ** EntityRecognizerFilter
    entityRecognizerFilter_recognizerName,
    entityRecognizerFilter_status,
    entityRecognizerFilter_submitTimeAfter,
    entityRecognizerFilter_submitTimeBefore,

    -- ** EntityRecognizerInputDataConfig
    entityRecognizerInputDataConfig_annotations,
    entityRecognizerInputDataConfig_augmentedManifests,
    entityRecognizerInputDataConfig_dataFormat,
    entityRecognizerInputDataConfig_documents,
    entityRecognizerInputDataConfig_entityList,
    entityRecognizerInputDataConfig_entityTypes,

    -- ** EntityRecognizerMetadata
    entityRecognizerMetadata_entityTypes,
    entityRecognizerMetadata_evaluationMetrics,
    entityRecognizerMetadata_numberOfTestDocuments,
    entityRecognizerMetadata_numberOfTrainedDocuments,

    -- ** EntityRecognizerMetadataEntityTypesListItem
    entityRecognizerMetadataEntityTypesListItem_evaluationMetrics,
    entityRecognizerMetadataEntityTypesListItem_numberOfTrainMentions,
    entityRecognizerMetadataEntityTypesListItem_type,

    -- ** EntityRecognizerOutputDataConfig
    entityRecognizerOutputDataConfig_flywheelStatsS3Prefix,

    -- ** EntityRecognizerProperties
    entityRecognizerProperties_dataAccessRoleArn,
    entityRecognizerProperties_endTime,
    entityRecognizerProperties_entityRecognizerArn,
    entityRecognizerProperties_flywheelArn,
    entityRecognizerProperties_inputDataConfig,
    entityRecognizerProperties_languageCode,
    entityRecognizerProperties_message,
    entityRecognizerProperties_modelKmsKeyId,
    entityRecognizerProperties_outputDataConfig,
    entityRecognizerProperties_recognizerMetadata,
    entityRecognizerProperties_sourceModelArn,
    entityRecognizerProperties_status,
    entityRecognizerProperties_submitTime,
    entityRecognizerProperties_trainingEndTime,
    entityRecognizerProperties_trainingStartTime,
    entityRecognizerProperties_versionName,
    entityRecognizerProperties_volumeKmsKeyId,
    entityRecognizerProperties_vpcConfig,

    -- ** EntityRecognizerSummary
    entityRecognizerSummary_latestVersionCreatedAt,
    entityRecognizerSummary_latestVersionName,
    entityRecognizerSummary_latestVersionStatus,
    entityRecognizerSummary_numberOfVersions,
    entityRecognizerSummary_recognizerName,

    -- ** EntityTypesEvaluationMetrics
    entityTypesEvaluationMetrics_f1Score,
    entityTypesEvaluationMetrics_precision,
    entityTypesEvaluationMetrics_recall,

    -- ** EntityTypesListItem
    entityTypesListItem_type,

    -- ** ErrorsListItem
    errorsListItem_errorCode,
    errorsListItem_errorMessage,
    errorsListItem_page,

    -- ** EventsDetectionJobFilter
    eventsDetectionJobFilter_jobName,
    eventsDetectionJobFilter_jobStatus,
    eventsDetectionJobFilter_submitTimeAfter,
    eventsDetectionJobFilter_submitTimeBefore,

    -- ** EventsDetectionJobProperties
    eventsDetectionJobProperties_dataAccessRoleArn,
    eventsDetectionJobProperties_endTime,
    eventsDetectionJobProperties_inputDataConfig,
    eventsDetectionJobProperties_jobArn,
    eventsDetectionJobProperties_jobId,
    eventsDetectionJobProperties_jobName,
    eventsDetectionJobProperties_jobStatus,
    eventsDetectionJobProperties_languageCode,
    eventsDetectionJobProperties_message,
    eventsDetectionJobProperties_outputDataConfig,
    eventsDetectionJobProperties_submitTime,
    eventsDetectionJobProperties_targetEventTypes,

    -- ** ExtractedCharactersListItem
    extractedCharactersListItem_count,
    extractedCharactersListItem_page,

    -- ** FlywheelFilter
    flywheelFilter_creationTimeAfter,
    flywheelFilter_creationTimeBefore,
    flywheelFilter_status,

    -- ** FlywheelIterationFilter
    flywheelIterationFilter_creationTimeAfter,
    flywheelIterationFilter_creationTimeBefore,

    -- ** FlywheelIterationProperties
    flywheelIterationProperties_creationTime,
    flywheelIterationProperties_endTime,
    flywheelIterationProperties_evaluatedModelArn,
    flywheelIterationProperties_evaluatedModelMetrics,
    flywheelIterationProperties_evaluationManifestS3Prefix,
    flywheelIterationProperties_flywheelArn,
    flywheelIterationProperties_flywheelIterationId,
    flywheelIterationProperties_message,
    flywheelIterationProperties_status,
    flywheelIterationProperties_trainedModelArn,
    flywheelIterationProperties_trainedModelMetrics,

    -- ** FlywheelModelEvaluationMetrics
    flywheelModelEvaluationMetrics_averageAccuracy,
    flywheelModelEvaluationMetrics_averageF1Score,
    flywheelModelEvaluationMetrics_averagePrecision,
    flywheelModelEvaluationMetrics_averageRecall,

    -- ** FlywheelProperties
    flywheelProperties_activeModelArn,
    flywheelProperties_creationTime,
    flywheelProperties_dataAccessRoleArn,
    flywheelProperties_dataLakeS3Uri,
    flywheelProperties_dataSecurityConfig,
    flywheelProperties_flywheelArn,
    flywheelProperties_lastModifiedTime,
    flywheelProperties_latestFlywheelIteration,
    flywheelProperties_message,
    flywheelProperties_modelType,
    flywheelProperties_status,
    flywheelProperties_taskConfig,

    -- ** FlywheelSummary
    flywheelSummary_activeModelArn,
    flywheelSummary_creationTime,
    flywheelSummary_dataLakeS3Uri,
    flywheelSummary_flywheelArn,
    flywheelSummary_lastModifiedTime,
    flywheelSummary_latestFlywheelIteration,
    flywheelSummary_message,
    flywheelSummary_modelType,
    flywheelSummary_status,

    -- ** Geometry
    geometry_boundingBox,
    geometry_polygon,

    -- ** InputDataConfig
    inputDataConfig_documentReaderConfig,
    inputDataConfig_inputFormat,
    inputDataConfig_s3Uri,

    -- ** KeyPhrase
    keyPhrase_beginOffset,
    keyPhrase_endOffset,
    keyPhrase_score,
    keyPhrase_text,

    -- ** KeyPhrasesDetectionJobFilter
    keyPhrasesDetectionJobFilter_jobName,
    keyPhrasesDetectionJobFilter_jobStatus,
    keyPhrasesDetectionJobFilter_submitTimeAfter,
    keyPhrasesDetectionJobFilter_submitTimeBefore,

    -- ** KeyPhrasesDetectionJobProperties
    keyPhrasesDetectionJobProperties_dataAccessRoleArn,
    keyPhrasesDetectionJobProperties_endTime,
    keyPhrasesDetectionJobProperties_inputDataConfig,
    keyPhrasesDetectionJobProperties_jobArn,
    keyPhrasesDetectionJobProperties_jobId,
    keyPhrasesDetectionJobProperties_jobName,
    keyPhrasesDetectionJobProperties_jobStatus,
    keyPhrasesDetectionJobProperties_languageCode,
    keyPhrasesDetectionJobProperties_message,
    keyPhrasesDetectionJobProperties_outputDataConfig,
    keyPhrasesDetectionJobProperties_submitTime,
    keyPhrasesDetectionJobProperties_volumeKmsKeyId,
    keyPhrasesDetectionJobProperties_vpcConfig,

    -- ** MentionSentiment
    mentionSentiment_sentiment,
    mentionSentiment_sentimentScore,

    -- ** OutputDataConfig
    outputDataConfig_kmsKeyId,
    outputDataConfig_s3Uri,

    -- ** PartOfSpeechTag
    partOfSpeechTag_score,
    partOfSpeechTag_tag,

    -- ** PiiEntitiesDetectionJobFilter
    piiEntitiesDetectionJobFilter_jobName,
    piiEntitiesDetectionJobFilter_jobStatus,
    piiEntitiesDetectionJobFilter_submitTimeAfter,
    piiEntitiesDetectionJobFilter_submitTimeBefore,

    -- ** PiiEntitiesDetectionJobProperties
    piiEntitiesDetectionJobProperties_dataAccessRoleArn,
    piiEntitiesDetectionJobProperties_endTime,
    piiEntitiesDetectionJobProperties_inputDataConfig,
    piiEntitiesDetectionJobProperties_jobArn,
    piiEntitiesDetectionJobProperties_jobId,
    piiEntitiesDetectionJobProperties_jobName,
    piiEntitiesDetectionJobProperties_jobStatus,
    piiEntitiesDetectionJobProperties_languageCode,
    piiEntitiesDetectionJobProperties_message,
    piiEntitiesDetectionJobProperties_mode,
    piiEntitiesDetectionJobProperties_outputDataConfig,
    piiEntitiesDetectionJobProperties_redactionConfig,
    piiEntitiesDetectionJobProperties_submitTime,

    -- ** PiiEntity
    piiEntity_beginOffset,
    piiEntity_endOffset,
    piiEntity_score,
    piiEntity_type,

    -- ** PiiOutputDataConfig
    piiOutputDataConfig_kmsKeyId,
    piiOutputDataConfig_s3Uri,

    -- ** Point
    point_x,
    point_y,

    -- ** RedactionConfig
    redactionConfig_maskCharacter,
    redactionConfig_maskMode,
    redactionConfig_piiEntityTypes,

    -- ** RelationshipsListItem
    relationshipsListItem_ids,
    relationshipsListItem_type,

    -- ** SentimentDetectionJobFilter
    sentimentDetectionJobFilter_jobName,
    sentimentDetectionJobFilter_jobStatus,
    sentimentDetectionJobFilter_submitTimeAfter,
    sentimentDetectionJobFilter_submitTimeBefore,

    -- ** SentimentDetectionJobProperties
    sentimentDetectionJobProperties_dataAccessRoleArn,
    sentimentDetectionJobProperties_endTime,
    sentimentDetectionJobProperties_inputDataConfig,
    sentimentDetectionJobProperties_jobArn,
    sentimentDetectionJobProperties_jobId,
    sentimentDetectionJobProperties_jobName,
    sentimentDetectionJobProperties_jobStatus,
    sentimentDetectionJobProperties_languageCode,
    sentimentDetectionJobProperties_message,
    sentimentDetectionJobProperties_outputDataConfig,
    sentimentDetectionJobProperties_submitTime,
    sentimentDetectionJobProperties_volumeKmsKeyId,
    sentimentDetectionJobProperties_vpcConfig,

    -- ** SentimentScore
    sentimentScore_mixed,
    sentimentScore_negative,
    sentimentScore_neutral,
    sentimentScore_positive,

    -- ** SyntaxToken
    syntaxToken_beginOffset,
    syntaxToken_endOffset,
    syntaxToken_partOfSpeech,
    syntaxToken_text,
    syntaxToken_tokenId,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TargetedSentimentDetectionJobFilter
    targetedSentimentDetectionJobFilter_jobName,
    targetedSentimentDetectionJobFilter_jobStatus,
    targetedSentimentDetectionJobFilter_submitTimeAfter,
    targetedSentimentDetectionJobFilter_submitTimeBefore,

    -- ** TargetedSentimentDetectionJobProperties
    targetedSentimentDetectionJobProperties_dataAccessRoleArn,
    targetedSentimentDetectionJobProperties_endTime,
    targetedSentimentDetectionJobProperties_inputDataConfig,
    targetedSentimentDetectionJobProperties_jobArn,
    targetedSentimentDetectionJobProperties_jobId,
    targetedSentimentDetectionJobProperties_jobName,
    targetedSentimentDetectionJobProperties_jobStatus,
    targetedSentimentDetectionJobProperties_languageCode,
    targetedSentimentDetectionJobProperties_message,
    targetedSentimentDetectionJobProperties_outputDataConfig,
    targetedSentimentDetectionJobProperties_submitTime,
    targetedSentimentDetectionJobProperties_volumeKmsKeyId,
    targetedSentimentDetectionJobProperties_vpcConfig,

    -- ** TargetedSentimentEntity
    targetedSentimentEntity_descriptiveMentionIndex,
    targetedSentimentEntity_mentions,

    -- ** TargetedSentimentMention
    targetedSentimentMention_beginOffset,
    targetedSentimentMention_endOffset,
    targetedSentimentMention_groupScore,
    targetedSentimentMention_mentionSentiment,
    targetedSentimentMention_score,
    targetedSentimentMention_text,
    targetedSentimentMention_type,

    -- ** TaskConfig
    taskConfig_documentClassificationConfig,
    taskConfig_entityRecognitionConfig,
    taskConfig_languageCode,

    -- ** TopicsDetectionJobFilter
    topicsDetectionJobFilter_jobName,
    topicsDetectionJobFilter_jobStatus,
    topicsDetectionJobFilter_submitTimeAfter,
    topicsDetectionJobFilter_submitTimeBefore,

    -- ** TopicsDetectionJobProperties
    topicsDetectionJobProperties_dataAccessRoleArn,
    topicsDetectionJobProperties_endTime,
    topicsDetectionJobProperties_inputDataConfig,
    topicsDetectionJobProperties_jobArn,
    topicsDetectionJobProperties_jobId,
    topicsDetectionJobProperties_jobName,
    topicsDetectionJobProperties_jobStatus,
    topicsDetectionJobProperties_message,
    topicsDetectionJobProperties_numberOfTopics,
    topicsDetectionJobProperties_outputDataConfig,
    topicsDetectionJobProperties_submitTime,
    topicsDetectionJobProperties_volumeKmsKeyId,
    topicsDetectionJobProperties_vpcConfig,

    -- ** UpdateDataSecurityConfig
    updateDataSecurityConfig_modelKmsKeyId,
    updateDataSecurityConfig_volumeKmsKeyId,
    updateDataSecurityConfig_vpcConfig,

    -- ** VpcConfig
    vpcConfig_securityGroupIds,
    vpcConfig_subnets,

    -- ** WarningsListItem
    warningsListItem_page,
    warningsListItem_warnCode,
    warningsListItem_warnMessage,
  )
where

import Amazonka.Comprehend.BatchDetectDominantLanguage
import Amazonka.Comprehend.BatchDetectEntities
import Amazonka.Comprehend.BatchDetectKeyPhrases
import Amazonka.Comprehend.BatchDetectSentiment
import Amazonka.Comprehend.BatchDetectSyntax
import Amazonka.Comprehend.BatchDetectTargetedSentiment
import Amazonka.Comprehend.ClassifyDocument
import Amazonka.Comprehend.ContainsPiiEntities
import Amazonka.Comprehend.CreateDataset
import Amazonka.Comprehend.CreateDocumentClassifier
import Amazonka.Comprehend.CreateEndpoint
import Amazonka.Comprehend.CreateEntityRecognizer
import Amazonka.Comprehend.CreateFlywheel
import Amazonka.Comprehend.DeleteDocumentClassifier
import Amazonka.Comprehend.DeleteEndpoint
import Amazonka.Comprehend.DeleteEntityRecognizer
import Amazonka.Comprehend.DeleteFlywheel
import Amazonka.Comprehend.DeleteResourcePolicy
import Amazonka.Comprehend.DescribeDataset
import Amazonka.Comprehend.DescribeDocumentClassificationJob
import Amazonka.Comprehend.DescribeDocumentClassifier
import Amazonka.Comprehend.DescribeDominantLanguageDetectionJob
import Amazonka.Comprehend.DescribeEndpoint
import Amazonka.Comprehend.DescribeEntitiesDetectionJob
import Amazonka.Comprehend.DescribeEntityRecognizer
import Amazonka.Comprehend.DescribeEventsDetectionJob
import Amazonka.Comprehend.DescribeFlywheel
import Amazonka.Comprehend.DescribeFlywheelIteration
import Amazonka.Comprehend.DescribeKeyPhrasesDetectionJob
import Amazonka.Comprehend.DescribePiiEntitiesDetectionJob
import Amazonka.Comprehend.DescribeResourcePolicy
import Amazonka.Comprehend.DescribeSentimentDetectionJob
import Amazonka.Comprehend.DescribeTargetedSentimentDetectionJob
import Amazonka.Comprehend.DescribeTopicsDetectionJob
import Amazonka.Comprehend.DetectDominantLanguage
import Amazonka.Comprehend.DetectEntities
import Amazonka.Comprehend.DetectKeyPhrases
import Amazonka.Comprehend.DetectPiiEntities
import Amazonka.Comprehend.DetectSentiment
import Amazonka.Comprehend.DetectSyntax
import Amazonka.Comprehend.DetectTargetedSentiment
import Amazonka.Comprehend.ImportModel
import Amazonka.Comprehend.ListDatasets
import Amazonka.Comprehend.ListDocumentClassificationJobs
import Amazonka.Comprehend.ListDocumentClassifierSummaries
import Amazonka.Comprehend.ListDocumentClassifiers
import Amazonka.Comprehend.ListDominantLanguageDetectionJobs
import Amazonka.Comprehend.ListEndpoints
import Amazonka.Comprehend.ListEntitiesDetectionJobs
import Amazonka.Comprehend.ListEntityRecognizerSummaries
import Amazonka.Comprehend.ListEntityRecognizers
import Amazonka.Comprehend.ListEventsDetectionJobs
import Amazonka.Comprehend.ListFlywheelIterationHistory
import Amazonka.Comprehend.ListFlywheels
import Amazonka.Comprehend.ListKeyPhrasesDetectionJobs
import Amazonka.Comprehend.ListPiiEntitiesDetectionJobs
import Amazonka.Comprehend.ListSentimentDetectionJobs
import Amazonka.Comprehend.ListTagsForResource
import Amazonka.Comprehend.ListTargetedSentimentDetectionJobs
import Amazonka.Comprehend.ListTopicsDetectionJobs
import Amazonka.Comprehend.PutResourcePolicy
import Amazonka.Comprehend.StartDocumentClassificationJob
import Amazonka.Comprehend.StartDominantLanguageDetectionJob
import Amazonka.Comprehend.StartEntitiesDetectionJob
import Amazonka.Comprehend.StartEventsDetectionJob
import Amazonka.Comprehend.StartFlywheelIteration
import Amazonka.Comprehend.StartKeyPhrasesDetectionJob
import Amazonka.Comprehend.StartPiiEntitiesDetectionJob
import Amazonka.Comprehend.StartSentimentDetectionJob
import Amazonka.Comprehend.StartTargetedSentimentDetectionJob
import Amazonka.Comprehend.StartTopicsDetectionJob
import Amazonka.Comprehend.StopDominantLanguageDetectionJob
import Amazonka.Comprehend.StopEntitiesDetectionJob
import Amazonka.Comprehend.StopEventsDetectionJob
import Amazonka.Comprehend.StopKeyPhrasesDetectionJob
import Amazonka.Comprehend.StopPiiEntitiesDetectionJob
import Amazonka.Comprehend.StopSentimentDetectionJob
import Amazonka.Comprehend.StopTargetedSentimentDetectionJob
import Amazonka.Comprehend.StopTrainingDocumentClassifier
import Amazonka.Comprehend.StopTrainingEntityRecognizer
import Amazonka.Comprehend.TagResource
import Amazonka.Comprehend.Types.AugmentedManifestsListItem
import Amazonka.Comprehend.Types.BatchDetectDominantLanguageItemResult
import Amazonka.Comprehend.Types.BatchDetectEntitiesItemResult
import Amazonka.Comprehend.Types.BatchDetectKeyPhrasesItemResult
import Amazonka.Comprehend.Types.BatchDetectSentimentItemResult
import Amazonka.Comprehend.Types.BatchDetectSyntaxItemResult
import Amazonka.Comprehend.Types.BatchDetectTargetedSentimentItemResult
import Amazonka.Comprehend.Types.BatchItemError
import Amazonka.Comprehend.Types.Block
import Amazonka.Comprehend.Types.BlockReference
import Amazonka.Comprehend.Types.BoundingBox
import Amazonka.Comprehend.Types.ChildBlock
import Amazonka.Comprehend.Types.ClassifierEvaluationMetrics
import Amazonka.Comprehend.Types.ClassifierMetadata
import Amazonka.Comprehend.Types.DataSecurityConfig
import Amazonka.Comprehend.Types.DatasetAugmentedManifestsListItem
import Amazonka.Comprehend.Types.DatasetDocumentClassifierInputDataConfig
import Amazonka.Comprehend.Types.DatasetEntityRecognizerAnnotations
import Amazonka.Comprehend.Types.DatasetEntityRecognizerDocuments
import Amazonka.Comprehend.Types.DatasetEntityRecognizerEntityList
import Amazonka.Comprehend.Types.DatasetEntityRecognizerInputDataConfig
import Amazonka.Comprehend.Types.DatasetFilter
import Amazonka.Comprehend.Types.DatasetInputDataConfig
import Amazonka.Comprehend.Types.DatasetProperties
import Amazonka.Comprehend.Types.DocumentClass
import Amazonka.Comprehend.Types.DocumentClassificationConfig
import Amazonka.Comprehend.Types.DocumentClassificationJobFilter
import Amazonka.Comprehend.Types.DocumentClassificationJobProperties
import Amazonka.Comprehend.Types.DocumentClassifierDocuments
import Amazonka.Comprehend.Types.DocumentClassifierFilter
import Amazonka.Comprehend.Types.DocumentClassifierInputDataConfig
import Amazonka.Comprehend.Types.DocumentClassifierOutputDataConfig
import Amazonka.Comprehend.Types.DocumentClassifierProperties
import Amazonka.Comprehend.Types.DocumentClassifierSummary
import Amazonka.Comprehend.Types.DocumentLabel
import Amazonka.Comprehend.Types.DocumentMetadata
import Amazonka.Comprehend.Types.DocumentReaderConfig
import Amazonka.Comprehend.Types.DocumentTypeListItem
import Amazonka.Comprehend.Types.DominantLanguage
import Amazonka.Comprehend.Types.DominantLanguageDetectionJobFilter
import Amazonka.Comprehend.Types.DominantLanguageDetectionJobProperties
import Amazonka.Comprehend.Types.EndpointFilter
import Amazonka.Comprehend.Types.EndpointProperties
import Amazonka.Comprehend.Types.EntitiesDetectionJobFilter
import Amazonka.Comprehend.Types.EntitiesDetectionJobProperties
import Amazonka.Comprehend.Types.Entity
import Amazonka.Comprehend.Types.EntityLabel
import Amazonka.Comprehend.Types.EntityRecognitionConfig
import Amazonka.Comprehend.Types.EntityRecognizerAnnotations
import Amazonka.Comprehend.Types.EntityRecognizerDocuments
import Amazonka.Comprehend.Types.EntityRecognizerEntityList
import Amazonka.Comprehend.Types.EntityRecognizerEvaluationMetrics
import Amazonka.Comprehend.Types.EntityRecognizerFilter
import Amazonka.Comprehend.Types.EntityRecognizerInputDataConfig
import Amazonka.Comprehend.Types.EntityRecognizerMetadata
import Amazonka.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem
import Amazonka.Comprehend.Types.EntityRecognizerOutputDataConfig
import Amazonka.Comprehend.Types.EntityRecognizerProperties
import Amazonka.Comprehend.Types.EntityRecognizerSummary
import Amazonka.Comprehend.Types.EntityTypesEvaluationMetrics
import Amazonka.Comprehend.Types.EntityTypesListItem
import Amazonka.Comprehend.Types.ErrorsListItem
import Amazonka.Comprehend.Types.EventsDetectionJobFilter
import Amazonka.Comprehend.Types.EventsDetectionJobProperties
import Amazonka.Comprehend.Types.ExtractedCharactersListItem
import Amazonka.Comprehend.Types.FlywheelFilter
import Amazonka.Comprehend.Types.FlywheelIterationFilter
import Amazonka.Comprehend.Types.FlywheelIterationProperties
import Amazonka.Comprehend.Types.FlywheelModelEvaluationMetrics
import Amazonka.Comprehend.Types.FlywheelProperties
import Amazonka.Comprehend.Types.FlywheelSummary
import Amazonka.Comprehend.Types.Geometry
import Amazonka.Comprehend.Types.InputDataConfig
import Amazonka.Comprehend.Types.KeyPhrase
import Amazonka.Comprehend.Types.KeyPhrasesDetectionJobFilter
import Amazonka.Comprehend.Types.KeyPhrasesDetectionJobProperties
import Amazonka.Comprehend.Types.MentionSentiment
import Amazonka.Comprehend.Types.OutputDataConfig
import Amazonka.Comprehend.Types.PartOfSpeechTag
import Amazonka.Comprehend.Types.PiiEntitiesDetectionJobFilter
import Amazonka.Comprehend.Types.PiiEntitiesDetectionJobProperties
import Amazonka.Comprehend.Types.PiiEntity
import Amazonka.Comprehend.Types.PiiOutputDataConfig
import Amazonka.Comprehend.Types.Point
import Amazonka.Comprehend.Types.RedactionConfig
import Amazonka.Comprehend.Types.RelationshipsListItem
import Amazonka.Comprehend.Types.SentimentDetectionJobFilter
import Amazonka.Comprehend.Types.SentimentDetectionJobProperties
import Amazonka.Comprehend.Types.SentimentScore
import Amazonka.Comprehend.Types.SyntaxToken
import Amazonka.Comprehend.Types.Tag
import Amazonka.Comprehend.Types.TargetedSentimentDetectionJobFilter
import Amazonka.Comprehend.Types.TargetedSentimentDetectionJobProperties
import Amazonka.Comprehend.Types.TargetedSentimentEntity
import Amazonka.Comprehend.Types.TargetedSentimentMention
import Amazonka.Comprehend.Types.TaskConfig
import Amazonka.Comprehend.Types.TopicsDetectionJobFilter
import Amazonka.Comprehend.Types.TopicsDetectionJobProperties
import Amazonka.Comprehend.Types.UpdateDataSecurityConfig
import Amazonka.Comprehend.Types.VpcConfig
import Amazonka.Comprehend.Types.WarningsListItem
import Amazonka.Comprehend.UntagResource
import Amazonka.Comprehend.UpdateEndpoint
import Amazonka.Comprehend.UpdateFlywheel

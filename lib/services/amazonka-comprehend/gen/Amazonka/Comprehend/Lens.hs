{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Comprehend.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
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

    -- ** ClassifyDocument
    classifyDocument_text,
    classifyDocument_endpointArn,
    classifyDocumentResponse_classes,
    classifyDocumentResponse_labels,
    classifyDocumentResponse_httpStatus,

    -- ** ContainsPiiEntities
    containsPiiEntities_text,
    containsPiiEntities_languageCode,
    containsPiiEntitiesResponse_labels,
    containsPiiEntitiesResponse_httpStatus,

    -- ** CreateDocumentClassifier
    createDocumentClassifier_tags,
    createDocumentClassifier_outputDataConfig,
    createDocumentClassifier_modelKmsKeyId,
    createDocumentClassifier_clientRequestToken,
    createDocumentClassifier_vpcConfig,
    createDocumentClassifier_volumeKmsKeyId,
    createDocumentClassifier_versionName,
    createDocumentClassifier_mode,
    createDocumentClassifier_documentClassifierName,
    createDocumentClassifier_dataAccessRoleArn,
    createDocumentClassifier_inputDataConfig,
    createDocumentClassifier_languageCode,
    createDocumentClassifierResponse_documentClassifierArn,
    createDocumentClassifierResponse_httpStatus,

    -- ** CreateEndpoint
    createEndpoint_tags,
    createEndpoint_clientRequestToken,
    createEndpoint_dataAccessRoleArn,
    createEndpoint_endpointName,
    createEndpoint_modelArn,
    createEndpoint_desiredInferenceUnits,
    createEndpointResponse_endpointArn,
    createEndpointResponse_httpStatus,

    -- ** CreateEntityRecognizer
    createEntityRecognizer_tags,
    createEntityRecognizer_modelKmsKeyId,
    createEntityRecognizer_clientRequestToken,
    createEntityRecognizer_vpcConfig,
    createEntityRecognizer_volumeKmsKeyId,
    createEntityRecognizer_versionName,
    createEntityRecognizer_recognizerName,
    createEntityRecognizer_dataAccessRoleArn,
    createEntityRecognizer_inputDataConfig,
    createEntityRecognizer_languageCode,
    createEntityRecognizerResponse_entityRecognizerArn,
    createEntityRecognizerResponse_httpStatus,

    -- ** DeleteDocumentClassifier
    deleteDocumentClassifier_documentClassifierArn,
    deleteDocumentClassifierResponse_httpStatus,

    -- ** DeleteEndpoint
    deleteEndpoint_endpointArn,
    deleteEndpointResponse_httpStatus,

    -- ** DeleteEntityRecognizer
    deleteEntityRecognizer_entityRecognizerArn,
    deleteEntityRecognizerResponse_httpStatus,

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

    -- ** DescribeKeyPhrasesDetectionJob
    describeKeyPhrasesDetectionJob_jobId,
    describeKeyPhrasesDetectionJobResponse_keyPhrasesDetectionJobProperties,
    describeKeyPhrasesDetectionJobResponse_httpStatus,

    -- ** DescribePiiEntitiesDetectionJob
    describePiiEntitiesDetectionJob_jobId,
    describePiiEntitiesDetectionJobResponse_piiEntitiesDetectionJobProperties,
    describePiiEntitiesDetectionJobResponse_httpStatus,

    -- ** DescribeSentimentDetectionJob
    describeSentimentDetectionJob_jobId,
    describeSentimentDetectionJobResponse_sentimentDetectionJobProperties,
    describeSentimentDetectionJobResponse_httpStatus,

    -- ** DescribeTopicsDetectionJob
    describeTopicsDetectionJob_jobId,
    describeTopicsDetectionJobResponse_topicsDetectionJobProperties,
    describeTopicsDetectionJobResponse_httpStatus,

    -- ** DetectDominantLanguage
    detectDominantLanguage_text,
    detectDominantLanguageResponse_languages,
    detectDominantLanguageResponse_httpStatus,

    -- ** DetectEntities
    detectEntities_languageCode,
    detectEntities_endpointArn,
    detectEntities_text,
    detectEntitiesResponse_entities,
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
    detectSentimentResponse_sentimentScore,
    detectSentimentResponse_sentiment,
    detectSentimentResponse_httpStatus,

    -- ** DetectSyntax
    detectSyntax_text,
    detectSyntax_languageCode,
    detectSyntaxResponse_syntaxTokens,
    detectSyntaxResponse_httpStatus,

    -- ** ListDocumentClassificationJobs
    listDocumentClassificationJobs_nextToken,
    listDocumentClassificationJobs_filter,
    listDocumentClassificationJobs_maxResults,
    listDocumentClassificationJobsResponse_nextToken,
    listDocumentClassificationJobsResponse_documentClassificationJobPropertiesList,
    listDocumentClassificationJobsResponse_httpStatus,

    -- ** ListDocumentClassifierSummaries
    listDocumentClassifierSummaries_nextToken,
    listDocumentClassifierSummaries_maxResults,
    listDocumentClassifierSummariesResponse_nextToken,
    listDocumentClassifierSummariesResponse_documentClassifierSummariesList,
    listDocumentClassifierSummariesResponse_httpStatus,

    -- ** ListDocumentClassifiers
    listDocumentClassifiers_nextToken,
    listDocumentClassifiers_filter,
    listDocumentClassifiers_maxResults,
    listDocumentClassifiersResponse_nextToken,
    listDocumentClassifiersResponse_documentClassifierPropertiesList,
    listDocumentClassifiersResponse_httpStatus,

    -- ** ListDominantLanguageDetectionJobs
    listDominantLanguageDetectionJobs_nextToken,
    listDominantLanguageDetectionJobs_filter,
    listDominantLanguageDetectionJobs_maxResults,
    listDominantLanguageDetectionJobsResponse_nextToken,
    listDominantLanguageDetectionJobsResponse_dominantLanguageDetectionJobPropertiesList,
    listDominantLanguageDetectionJobsResponse_httpStatus,

    -- ** ListEndpoints
    listEndpoints_nextToken,
    listEndpoints_filter,
    listEndpoints_maxResults,
    listEndpointsResponse_nextToken,
    listEndpointsResponse_endpointPropertiesList,
    listEndpointsResponse_httpStatus,

    -- ** ListEntitiesDetectionJobs
    listEntitiesDetectionJobs_nextToken,
    listEntitiesDetectionJobs_filter,
    listEntitiesDetectionJobs_maxResults,
    listEntitiesDetectionJobsResponse_nextToken,
    listEntitiesDetectionJobsResponse_entitiesDetectionJobPropertiesList,
    listEntitiesDetectionJobsResponse_httpStatus,

    -- ** ListEntityRecognizerSummaries
    listEntityRecognizerSummaries_nextToken,
    listEntityRecognizerSummaries_maxResults,
    listEntityRecognizerSummariesResponse_nextToken,
    listEntityRecognizerSummariesResponse_entityRecognizerSummariesList,
    listEntityRecognizerSummariesResponse_httpStatus,

    -- ** ListEntityRecognizers
    listEntityRecognizers_nextToken,
    listEntityRecognizers_filter,
    listEntityRecognizers_maxResults,
    listEntityRecognizersResponse_nextToken,
    listEntityRecognizersResponse_entityRecognizerPropertiesList,
    listEntityRecognizersResponse_httpStatus,

    -- ** ListEventsDetectionJobs
    listEventsDetectionJobs_nextToken,
    listEventsDetectionJobs_filter,
    listEventsDetectionJobs_maxResults,
    listEventsDetectionJobsResponse_nextToken,
    listEventsDetectionJobsResponse_eventsDetectionJobPropertiesList,
    listEventsDetectionJobsResponse_httpStatus,

    -- ** ListKeyPhrasesDetectionJobs
    listKeyPhrasesDetectionJobs_nextToken,
    listKeyPhrasesDetectionJobs_filter,
    listKeyPhrasesDetectionJobs_maxResults,
    listKeyPhrasesDetectionJobsResponse_nextToken,
    listKeyPhrasesDetectionJobsResponse_keyPhrasesDetectionJobPropertiesList,
    listKeyPhrasesDetectionJobsResponse_httpStatus,

    -- ** ListPiiEntitiesDetectionJobs
    listPiiEntitiesDetectionJobs_nextToken,
    listPiiEntitiesDetectionJobs_filter,
    listPiiEntitiesDetectionJobs_maxResults,
    listPiiEntitiesDetectionJobsResponse_nextToken,
    listPiiEntitiesDetectionJobsResponse_piiEntitiesDetectionJobPropertiesList,
    listPiiEntitiesDetectionJobsResponse_httpStatus,

    -- ** ListSentimentDetectionJobs
    listSentimentDetectionJobs_nextToken,
    listSentimentDetectionJobs_filter,
    listSentimentDetectionJobs_maxResults,
    listSentimentDetectionJobsResponse_sentimentDetectionJobPropertiesList,
    listSentimentDetectionJobsResponse_nextToken,
    listSentimentDetectionJobsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTopicsDetectionJobs
    listTopicsDetectionJobs_nextToken,
    listTopicsDetectionJobs_filter,
    listTopicsDetectionJobs_maxResults,
    listTopicsDetectionJobsResponse_nextToken,
    listTopicsDetectionJobsResponse_topicsDetectionJobPropertiesList,
    listTopicsDetectionJobsResponse_httpStatus,

    -- ** StartDocumentClassificationJob
    startDocumentClassificationJob_tags,
    startDocumentClassificationJob_clientRequestToken,
    startDocumentClassificationJob_vpcConfig,
    startDocumentClassificationJob_jobName,
    startDocumentClassificationJob_volumeKmsKeyId,
    startDocumentClassificationJob_documentClassifierArn,
    startDocumentClassificationJob_inputDataConfig,
    startDocumentClassificationJob_outputDataConfig,
    startDocumentClassificationJob_dataAccessRoleArn,
    startDocumentClassificationJobResponse_jobStatus,
    startDocumentClassificationJobResponse_jobId,
    startDocumentClassificationJobResponse_jobArn,
    startDocumentClassificationJobResponse_httpStatus,

    -- ** StartDominantLanguageDetectionJob
    startDominantLanguageDetectionJob_tags,
    startDominantLanguageDetectionJob_clientRequestToken,
    startDominantLanguageDetectionJob_vpcConfig,
    startDominantLanguageDetectionJob_jobName,
    startDominantLanguageDetectionJob_volumeKmsKeyId,
    startDominantLanguageDetectionJob_inputDataConfig,
    startDominantLanguageDetectionJob_outputDataConfig,
    startDominantLanguageDetectionJob_dataAccessRoleArn,
    startDominantLanguageDetectionJobResponse_jobStatus,
    startDominantLanguageDetectionJobResponse_jobId,
    startDominantLanguageDetectionJobResponse_jobArn,
    startDominantLanguageDetectionJobResponse_httpStatus,

    -- ** StartEntitiesDetectionJob
    startEntitiesDetectionJob_tags,
    startEntitiesDetectionJob_clientRequestToken,
    startEntitiesDetectionJob_vpcConfig,
    startEntitiesDetectionJob_jobName,
    startEntitiesDetectionJob_entityRecognizerArn,
    startEntitiesDetectionJob_volumeKmsKeyId,
    startEntitiesDetectionJob_inputDataConfig,
    startEntitiesDetectionJob_outputDataConfig,
    startEntitiesDetectionJob_dataAccessRoleArn,
    startEntitiesDetectionJob_languageCode,
    startEntitiesDetectionJobResponse_jobStatus,
    startEntitiesDetectionJobResponse_jobId,
    startEntitiesDetectionJobResponse_jobArn,
    startEntitiesDetectionJobResponse_httpStatus,

    -- ** StartEventsDetectionJob
    startEventsDetectionJob_tags,
    startEventsDetectionJob_clientRequestToken,
    startEventsDetectionJob_jobName,
    startEventsDetectionJob_inputDataConfig,
    startEventsDetectionJob_outputDataConfig,
    startEventsDetectionJob_dataAccessRoleArn,
    startEventsDetectionJob_languageCode,
    startEventsDetectionJob_targetEventTypes,
    startEventsDetectionJobResponse_jobStatus,
    startEventsDetectionJobResponse_jobId,
    startEventsDetectionJobResponse_jobArn,
    startEventsDetectionJobResponse_httpStatus,

    -- ** StartKeyPhrasesDetectionJob
    startKeyPhrasesDetectionJob_tags,
    startKeyPhrasesDetectionJob_clientRequestToken,
    startKeyPhrasesDetectionJob_vpcConfig,
    startKeyPhrasesDetectionJob_jobName,
    startKeyPhrasesDetectionJob_volumeKmsKeyId,
    startKeyPhrasesDetectionJob_inputDataConfig,
    startKeyPhrasesDetectionJob_outputDataConfig,
    startKeyPhrasesDetectionJob_dataAccessRoleArn,
    startKeyPhrasesDetectionJob_languageCode,
    startKeyPhrasesDetectionJobResponse_jobStatus,
    startKeyPhrasesDetectionJobResponse_jobId,
    startKeyPhrasesDetectionJobResponse_jobArn,
    startKeyPhrasesDetectionJobResponse_httpStatus,

    -- ** StartPiiEntitiesDetectionJob
    startPiiEntitiesDetectionJob_tags,
    startPiiEntitiesDetectionJob_clientRequestToken,
    startPiiEntitiesDetectionJob_jobName,
    startPiiEntitiesDetectionJob_redactionConfig,
    startPiiEntitiesDetectionJob_inputDataConfig,
    startPiiEntitiesDetectionJob_outputDataConfig,
    startPiiEntitiesDetectionJob_mode,
    startPiiEntitiesDetectionJob_dataAccessRoleArn,
    startPiiEntitiesDetectionJob_languageCode,
    startPiiEntitiesDetectionJobResponse_jobStatus,
    startPiiEntitiesDetectionJobResponse_jobId,
    startPiiEntitiesDetectionJobResponse_jobArn,
    startPiiEntitiesDetectionJobResponse_httpStatus,

    -- ** StartSentimentDetectionJob
    startSentimentDetectionJob_tags,
    startSentimentDetectionJob_clientRequestToken,
    startSentimentDetectionJob_vpcConfig,
    startSentimentDetectionJob_jobName,
    startSentimentDetectionJob_volumeKmsKeyId,
    startSentimentDetectionJob_inputDataConfig,
    startSentimentDetectionJob_outputDataConfig,
    startSentimentDetectionJob_dataAccessRoleArn,
    startSentimentDetectionJob_languageCode,
    startSentimentDetectionJobResponse_jobStatus,
    startSentimentDetectionJobResponse_jobId,
    startSentimentDetectionJobResponse_jobArn,
    startSentimentDetectionJobResponse_httpStatus,

    -- ** StartTopicsDetectionJob
    startTopicsDetectionJob_tags,
    startTopicsDetectionJob_clientRequestToken,
    startTopicsDetectionJob_vpcConfig,
    startTopicsDetectionJob_jobName,
    startTopicsDetectionJob_numberOfTopics,
    startTopicsDetectionJob_volumeKmsKeyId,
    startTopicsDetectionJob_inputDataConfig,
    startTopicsDetectionJob_outputDataConfig,
    startTopicsDetectionJob_dataAccessRoleArn,
    startTopicsDetectionJobResponse_jobStatus,
    startTopicsDetectionJobResponse_jobId,
    startTopicsDetectionJobResponse_jobArn,
    startTopicsDetectionJobResponse_httpStatus,

    -- ** StopDominantLanguageDetectionJob
    stopDominantLanguageDetectionJob_jobId,
    stopDominantLanguageDetectionJobResponse_jobStatus,
    stopDominantLanguageDetectionJobResponse_jobId,
    stopDominantLanguageDetectionJobResponse_httpStatus,

    -- ** StopEntitiesDetectionJob
    stopEntitiesDetectionJob_jobId,
    stopEntitiesDetectionJobResponse_jobStatus,
    stopEntitiesDetectionJobResponse_jobId,
    stopEntitiesDetectionJobResponse_httpStatus,

    -- ** StopEventsDetectionJob
    stopEventsDetectionJob_jobId,
    stopEventsDetectionJobResponse_jobStatus,
    stopEventsDetectionJobResponse_jobId,
    stopEventsDetectionJobResponse_httpStatus,

    -- ** StopKeyPhrasesDetectionJob
    stopKeyPhrasesDetectionJob_jobId,
    stopKeyPhrasesDetectionJobResponse_jobStatus,
    stopKeyPhrasesDetectionJobResponse_jobId,
    stopKeyPhrasesDetectionJobResponse_httpStatus,

    -- ** StopPiiEntitiesDetectionJob
    stopPiiEntitiesDetectionJob_jobId,
    stopPiiEntitiesDetectionJobResponse_jobStatus,
    stopPiiEntitiesDetectionJobResponse_jobId,
    stopPiiEntitiesDetectionJobResponse_httpStatus,

    -- ** StopSentimentDetectionJob
    stopSentimentDetectionJob_jobId,
    stopSentimentDetectionJobResponse_jobStatus,
    stopSentimentDetectionJobResponse_jobId,
    stopSentimentDetectionJobResponse_httpStatus,

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
    updateEndpoint_desiredInferenceUnits,
    updateEndpoint_desiredDataAccessRoleArn,
    updateEndpoint_desiredModelArn,
    updateEndpoint_endpointArn,
    updateEndpointResponse_httpStatus,

    -- * Types

    -- ** AugmentedManifestsListItem
    augmentedManifestsListItem_documentType,
    augmentedManifestsListItem_annotationDataS3Uri,
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
    batchDetectSentimentItemResult_sentimentScore,
    batchDetectSentimentItemResult_sentiment,

    -- ** BatchDetectSyntaxItemResult
    batchDetectSyntaxItemResult_index,
    batchDetectSyntaxItemResult_syntaxTokens,

    -- ** BatchItemError
    batchItemError_errorMessage,
    batchItemError_index,
    batchItemError_errorCode,

    -- ** ClassifierEvaluationMetrics
    classifierEvaluationMetrics_microF1Score,
    classifierEvaluationMetrics_f1Score,
    classifierEvaluationMetrics_microPrecision,
    classifierEvaluationMetrics_accuracy,
    classifierEvaluationMetrics_hammingLoss,
    classifierEvaluationMetrics_recall,
    classifierEvaluationMetrics_precision,
    classifierEvaluationMetrics_microRecall,

    -- ** ClassifierMetadata
    classifierMetadata_evaluationMetrics,
    classifierMetadata_numberOfTestDocuments,
    classifierMetadata_numberOfLabels,
    classifierMetadata_numberOfTrainedDocuments,

    -- ** DocumentClass
    documentClass_name,
    documentClass_score,

    -- ** DocumentClassificationJobFilter
    documentClassificationJobFilter_jobStatus,
    documentClassificationJobFilter_jobName,
    documentClassificationJobFilter_submitTimeBefore,
    documentClassificationJobFilter_submitTimeAfter,

    -- ** DocumentClassificationJobProperties
    documentClassificationJobProperties_outputDataConfig,
    documentClassificationJobProperties_message,
    documentClassificationJobProperties_jobStatus,
    documentClassificationJobProperties_vpcConfig,
    documentClassificationJobProperties_jobName,
    documentClassificationJobProperties_submitTime,
    documentClassificationJobProperties_jobId,
    documentClassificationJobProperties_volumeKmsKeyId,
    documentClassificationJobProperties_dataAccessRoleArn,
    documentClassificationJobProperties_endTime,
    documentClassificationJobProperties_documentClassifierArn,
    documentClassificationJobProperties_jobArn,
    documentClassificationJobProperties_inputDataConfig,

    -- ** DocumentClassifierFilter
    documentClassifierFilter_status,
    documentClassifierFilter_submitTimeBefore,
    documentClassifierFilter_documentClassifierName,
    documentClassifierFilter_submitTimeAfter,

    -- ** DocumentClassifierInputDataConfig
    documentClassifierInputDataConfig_dataFormat,
    documentClassifierInputDataConfig_testS3Uri,
    documentClassifierInputDataConfig_s3Uri,
    documentClassifierInputDataConfig_augmentedManifests,
    documentClassifierInputDataConfig_labelDelimiter,

    -- ** DocumentClassifierOutputDataConfig
    documentClassifierOutputDataConfig_kmsKeyId,
    documentClassifierOutputDataConfig_s3Uri,

    -- ** DocumentClassifierProperties
    documentClassifierProperties_outputDataConfig,
    documentClassifierProperties_message,
    documentClassifierProperties_modelKmsKeyId,
    documentClassifierProperties_vpcConfig,
    documentClassifierProperties_submitTime,
    documentClassifierProperties_volumeKmsKeyId,
    documentClassifierProperties_trainingStartTime,
    documentClassifierProperties_dataAccessRoleArn,
    documentClassifierProperties_status,
    documentClassifierProperties_endTime,
    documentClassifierProperties_languageCode,
    documentClassifierProperties_versionName,
    documentClassifierProperties_mode,
    documentClassifierProperties_documentClassifierArn,
    documentClassifierProperties_inputDataConfig,
    documentClassifierProperties_trainingEndTime,
    documentClassifierProperties_classifierMetadata,

    -- ** DocumentClassifierSummary
    documentClassifierSummary_numberOfVersions,
    documentClassifierSummary_latestVersionName,
    documentClassifierSummary_latestVersionStatus,
    documentClassifierSummary_documentClassifierName,
    documentClassifierSummary_latestVersionCreatedAt,

    -- ** DocumentLabel
    documentLabel_name,
    documentLabel_score,

    -- ** DocumentReaderConfig
    documentReaderConfig_documentReadMode,
    documentReaderConfig_featureTypes,
    documentReaderConfig_documentReadAction,

    -- ** DominantLanguage
    dominantLanguage_score,
    dominantLanguage_languageCode,

    -- ** DominantLanguageDetectionJobFilter
    dominantLanguageDetectionJobFilter_jobStatus,
    dominantLanguageDetectionJobFilter_jobName,
    dominantLanguageDetectionJobFilter_submitTimeBefore,
    dominantLanguageDetectionJobFilter_submitTimeAfter,

    -- ** DominantLanguageDetectionJobProperties
    dominantLanguageDetectionJobProperties_outputDataConfig,
    dominantLanguageDetectionJobProperties_message,
    dominantLanguageDetectionJobProperties_jobStatus,
    dominantLanguageDetectionJobProperties_vpcConfig,
    dominantLanguageDetectionJobProperties_jobName,
    dominantLanguageDetectionJobProperties_submitTime,
    dominantLanguageDetectionJobProperties_jobId,
    dominantLanguageDetectionJobProperties_volumeKmsKeyId,
    dominantLanguageDetectionJobProperties_dataAccessRoleArn,
    dominantLanguageDetectionJobProperties_endTime,
    dominantLanguageDetectionJobProperties_jobArn,
    dominantLanguageDetectionJobProperties_inputDataConfig,

    -- ** EndpointFilter
    endpointFilter_status,
    endpointFilter_creationTimeBefore,
    endpointFilter_modelArn,
    endpointFilter_creationTimeAfter,

    -- ** EndpointProperties
    endpointProperties_desiredInferenceUnits,
    endpointProperties_message,
    endpointProperties_currentInferenceUnits,
    endpointProperties_desiredDataAccessRoleArn,
    endpointProperties_dataAccessRoleArn,
    endpointProperties_status,
    endpointProperties_desiredModelArn,
    endpointProperties_lastModifiedTime,
    endpointProperties_modelArn,
    endpointProperties_creationTime,
    endpointProperties_endpointArn,

    -- ** EntitiesDetectionJobFilter
    entitiesDetectionJobFilter_jobStatus,
    entitiesDetectionJobFilter_jobName,
    entitiesDetectionJobFilter_submitTimeBefore,
    entitiesDetectionJobFilter_submitTimeAfter,

    -- ** EntitiesDetectionJobProperties
    entitiesDetectionJobProperties_outputDataConfig,
    entitiesDetectionJobProperties_message,
    entitiesDetectionJobProperties_jobStatus,
    entitiesDetectionJobProperties_vpcConfig,
    entitiesDetectionJobProperties_jobName,
    entitiesDetectionJobProperties_submitTime,
    entitiesDetectionJobProperties_jobId,
    entitiesDetectionJobProperties_entityRecognizerArn,
    entitiesDetectionJobProperties_volumeKmsKeyId,
    entitiesDetectionJobProperties_dataAccessRoleArn,
    entitiesDetectionJobProperties_endTime,
    entitiesDetectionJobProperties_languageCode,
    entitiesDetectionJobProperties_jobArn,
    entitiesDetectionJobProperties_inputDataConfig,

    -- ** Entity
    entity_beginOffset,
    entity_type,
    entity_score,
    entity_endOffset,
    entity_text,

    -- ** EntityLabel
    entityLabel_name,
    entityLabel_score,

    -- ** EntityRecognizerAnnotations
    entityRecognizerAnnotations_testS3Uri,
    entityRecognizerAnnotations_s3Uri,

    -- ** EntityRecognizerDocuments
    entityRecognizerDocuments_testS3Uri,
    entityRecognizerDocuments_inputFormat,
    entityRecognizerDocuments_s3Uri,

    -- ** EntityRecognizerEntityList
    entityRecognizerEntityList_s3Uri,

    -- ** EntityRecognizerEvaluationMetrics
    entityRecognizerEvaluationMetrics_f1Score,
    entityRecognizerEvaluationMetrics_recall,
    entityRecognizerEvaluationMetrics_precision,

    -- ** EntityRecognizerFilter
    entityRecognizerFilter_recognizerName,
    entityRecognizerFilter_status,
    entityRecognizerFilter_submitTimeBefore,
    entityRecognizerFilter_submitTimeAfter,

    -- ** EntityRecognizerInputDataConfig
    entityRecognizerInputDataConfig_dataFormat,
    entityRecognizerInputDataConfig_entityList,
    entityRecognizerInputDataConfig_annotations,
    entityRecognizerInputDataConfig_documents,
    entityRecognizerInputDataConfig_augmentedManifests,
    entityRecognizerInputDataConfig_entityTypes,

    -- ** EntityRecognizerMetadata
    entityRecognizerMetadata_evaluationMetrics,
    entityRecognizerMetadata_numberOfTestDocuments,
    entityRecognizerMetadata_entityTypes,
    entityRecognizerMetadata_numberOfTrainedDocuments,

    -- ** EntityRecognizerMetadataEntityTypesListItem
    entityRecognizerMetadataEntityTypesListItem_evaluationMetrics,
    entityRecognizerMetadataEntityTypesListItem_type,
    entityRecognizerMetadataEntityTypesListItem_numberOfTrainMentions,

    -- ** EntityRecognizerProperties
    entityRecognizerProperties_message,
    entityRecognizerProperties_modelKmsKeyId,
    entityRecognizerProperties_recognizerMetadata,
    entityRecognizerProperties_vpcConfig,
    entityRecognizerProperties_submitTime,
    entityRecognizerProperties_entityRecognizerArn,
    entityRecognizerProperties_volumeKmsKeyId,
    entityRecognizerProperties_trainingStartTime,
    entityRecognizerProperties_dataAccessRoleArn,
    entityRecognizerProperties_status,
    entityRecognizerProperties_endTime,
    entityRecognizerProperties_languageCode,
    entityRecognizerProperties_versionName,
    entityRecognizerProperties_inputDataConfig,
    entityRecognizerProperties_trainingEndTime,

    -- ** EntityRecognizerSummary
    entityRecognizerSummary_numberOfVersions,
    entityRecognizerSummary_latestVersionName,
    entityRecognizerSummary_recognizerName,
    entityRecognizerSummary_latestVersionStatus,
    entityRecognizerSummary_latestVersionCreatedAt,

    -- ** EntityTypesEvaluationMetrics
    entityTypesEvaluationMetrics_f1Score,
    entityTypesEvaluationMetrics_recall,
    entityTypesEvaluationMetrics_precision,

    -- ** EntityTypesListItem
    entityTypesListItem_type,

    -- ** EventsDetectionJobFilter
    eventsDetectionJobFilter_jobStatus,
    eventsDetectionJobFilter_jobName,
    eventsDetectionJobFilter_submitTimeBefore,
    eventsDetectionJobFilter_submitTimeAfter,

    -- ** EventsDetectionJobProperties
    eventsDetectionJobProperties_outputDataConfig,
    eventsDetectionJobProperties_message,
    eventsDetectionJobProperties_jobStatus,
    eventsDetectionJobProperties_jobName,
    eventsDetectionJobProperties_submitTime,
    eventsDetectionJobProperties_jobId,
    eventsDetectionJobProperties_dataAccessRoleArn,
    eventsDetectionJobProperties_endTime,
    eventsDetectionJobProperties_targetEventTypes,
    eventsDetectionJobProperties_languageCode,
    eventsDetectionJobProperties_jobArn,
    eventsDetectionJobProperties_inputDataConfig,

    -- ** InputDataConfig
    inputDataConfig_documentReaderConfig,
    inputDataConfig_inputFormat,
    inputDataConfig_s3Uri,

    -- ** KeyPhrase
    keyPhrase_beginOffset,
    keyPhrase_score,
    keyPhrase_endOffset,
    keyPhrase_text,

    -- ** KeyPhrasesDetectionJobFilter
    keyPhrasesDetectionJobFilter_jobStatus,
    keyPhrasesDetectionJobFilter_jobName,
    keyPhrasesDetectionJobFilter_submitTimeBefore,
    keyPhrasesDetectionJobFilter_submitTimeAfter,

    -- ** KeyPhrasesDetectionJobProperties
    keyPhrasesDetectionJobProperties_outputDataConfig,
    keyPhrasesDetectionJobProperties_message,
    keyPhrasesDetectionJobProperties_jobStatus,
    keyPhrasesDetectionJobProperties_vpcConfig,
    keyPhrasesDetectionJobProperties_jobName,
    keyPhrasesDetectionJobProperties_submitTime,
    keyPhrasesDetectionJobProperties_jobId,
    keyPhrasesDetectionJobProperties_volumeKmsKeyId,
    keyPhrasesDetectionJobProperties_dataAccessRoleArn,
    keyPhrasesDetectionJobProperties_endTime,
    keyPhrasesDetectionJobProperties_languageCode,
    keyPhrasesDetectionJobProperties_jobArn,
    keyPhrasesDetectionJobProperties_inputDataConfig,

    -- ** OutputDataConfig
    outputDataConfig_kmsKeyId,
    outputDataConfig_s3Uri,

    -- ** PartOfSpeechTag
    partOfSpeechTag_tag,
    partOfSpeechTag_score,

    -- ** PiiEntitiesDetectionJobFilter
    piiEntitiesDetectionJobFilter_jobStatus,
    piiEntitiesDetectionJobFilter_jobName,
    piiEntitiesDetectionJobFilter_submitTimeBefore,
    piiEntitiesDetectionJobFilter_submitTimeAfter,

    -- ** PiiEntitiesDetectionJobProperties
    piiEntitiesDetectionJobProperties_outputDataConfig,
    piiEntitiesDetectionJobProperties_message,
    piiEntitiesDetectionJobProperties_jobStatus,
    piiEntitiesDetectionJobProperties_jobName,
    piiEntitiesDetectionJobProperties_submitTime,
    piiEntitiesDetectionJobProperties_jobId,
    piiEntitiesDetectionJobProperties_redactionConfig,
    piiEntitiesDetectionJobProperties_dataAccessRoleArn,
    piiEntitiesDetectionJobProperties_endTime,
    piiEntitiesDetectionJobProperties_languageCode,
    piiEntitiesDetectionJobProperties_mode,
    piiEntitiesDetectionJobProperties_jobArn,
    piiEntitiesDetectionJobProperties_inputDataConfig,

    -- ** PiiEntity
    piiEntity_beginOffset,
    piiEntity_type,
    piiEntity_score,
    piiEntity_endOffset,

    -- ** PiiOutputDataConfig
    piiOutputDataConfig_kmsKeyId,
    piiOutputDataConfig_s3Uri,

    -- ** RedactionConfig
    redactionConfig_maskMode,
    redactionConfig_piiEntityTypes,
    redactionConfig_maskCharacter,

    -- ** SentimentDetectionJobFilter
    sentimentDetectionJobFilter_jobStatus,
    sentimentDetectionJobFilter_jobName,
    sentimentDetectionJobFilter_submitTimeBefore,
    sentimentDetectionJobFilter_submitTimeAfter,

    -- ** SentimentDetectionJobProperties
    sentimentDetectionJobProperties_outputDataConfig,
    sentimentDetectionJobProperties_message,
    sentimentDetectionJobProperties_jobStatus,
    sentimentDetectionJobProperties_vpcConfig,
    sentimentDetectionJobProperties_jobName,
    sentimentDetectionJobProperties_submitTime,
    sentimentDetectionJobProperties_jobId,
    sentimentDetectionJobProperties_volumeKmsKeyId,
    sentimentDetectionJobProperties_dataAccessRoleArn,
    sentimentDetectionJobProperties_endTime,
    sentimentDetectionJobProperties_languageCode,
    sentimentDetectionJobProperties_jobArn,
    sentimentDetectionJobProperties_inputDataConfig,

    -- ** SentimentScore
    sentimentScore_positive,
    sentimentScore_negative,
    sentimentScore_neutral,
    sentimentScore_mixed,

    -- ** SyntaxToken
    syntaxToken_beginOffset,
    syntaxToken_partOfSpeech,
    syntaxToken_endOffset,
    syntaxToken_tokenId,
    syntaxToken_text,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TopicsDetectionJobFilter
    topicsDetectionJobFilter_jobStatus,
    topicsDetectionJobFilter_jobName,
    topicsDetectionJobFilter_submitTimeBefore,
    topicsDetectionJobFilter_submitTimeAfter,

    -- ** TopicsDetectionJobProperties
    topicsDetectionJobProperties_outputDataConfig,
    topicsDetectionJobProperties_message,
    topicsDetectionJobProperties_jobStatus,
    topicsDetectionJobProperties_vpcConfig,
    topicsDetectionJobProperties_jobName,
    topicsDetectionJobProperties_numberOfTopics,
    topicsDetectionJobProperties_submitTime,
    topicsDetectionJobProperties_jobId,
    topicsDetectionJobProperties_volumeKmsKeyId,
    topicsDetectionJobProperties_dataAccessRoleArn,
    topicsDetectionJobProperties_endTime,
    topicsDetectionJobProperties_jobArn,
    topicsDetectionJobProperties_inputDataConfig,

    -- ** VpcConfig
    vpcConfig_securityGroupIds,
    vpcConfig_subnets,
  )
where

import Amazonka.Comprehend.BatchDetectDominantLanguage
import Amazonka.Comprehend.BatchDetectEntities
import Amazonka.Comprehend.BatchDetectKeyPhrases
import Amazonka.Comprehend.BatchDetectSentiment
import Amazonka.Comprehend.BatchDetectSyntax
import Amazonka.Comprehend.ClassifyDocument
import Amazonka.Comprehend.ContainsPiiEntities
import Amazonka.Comprehend.CreateDocumentClassifier
import Amazonka.Comprehend.CreateEndpoint
import Amazonka.Comprehend.CreateEntityRecognizer
import Amazonka.Comprehend.DeleteDocumentClassifier
import Amazonka.Comprehend.DeleteEndpoint
import Amazonka.Comprehend.DeleteEntityRecognizer
import Amazonka.Comprehend.DescribeDocumentClassificationJob
import Amazonka.Comprehend.DescribeDocumentClassifier
import Amazonka.Comprehend.DescribeDominantLanguageDetectionJob
import Amazonka.Comprehend.DescribeEndpoint
import Amazonka.Comprehend.DescribeEntitiesDetectionJob
import Amazonka.Comprehend.DescribeEntityRecognizer
import Amazonka.Comprehend.DescribeEventsDetectionJob
import Amazonka.Comprehend.DescribeKeyPhrasesDetectionJob
import Amazonka.Comprehend.DescribePiiEntitiesDetectionJob
import Amazonka.Comprehend.DescribeSentimentDetectionJob
import Amazonka.Comprehend.DescribeTopicsDetectionJob
import Amazonka.Comprehend.DetectDominantLanguage
import Amazonka.Comprehend.DetectEntities
import Amazonka.Comprehend.DetectKeyPhrases
import Amazonka.Comprehend.DetectPiiEntities
import Amazonka.Comprehend.DetectSentiment
import Amazonka.Comprehend.DetectSyntax
import Amazonka.Comprehend.ListDocumentClassificationJobs
import Amazonka.Comprehend.ListDocumentClassifierSummaries
import Amazonka.Comprehend.ListDocumentClassifiers
import Amazonka.Comprehend.ListDominantLanguageDetectionJobs
import Amazonka.Comprehend.ListEndpoints
import Amazonka.Comprehend.ListEntitiesDetectionJobs
import Amazonka.Comprehend.ListEntityRecognizerSummaries
import Amazonka.Comprehend.ListEntityRecognizers
import Amazonka.Comprehend.ListEventsDetectionJobs
import Amazonka.Comprehend.ListKeyPhrasesDetectionJobs
import Amazonka.Comprehend.ListPiiEntitiesDetectionJobs
import Amazonka.Comprehend.ListSentimentDetectionJobs
import Amazonka.Comprehend.ListTagsForResource
import Amazonka.Comprehend.ListTopicsDetectionJobs
import Amazonka.Comprehend.StartDocumentClassificationJob
import Amazonka.Comprehend.StartDominantLanguageDetectionJob
import Amazonka.Comprehend.StartEntitiesDetectionJob
import Amazonka.Comprehend.StartEventsDetectionJob
import Amazonka.Comprehend.StartKeyPhrasesDetectionJob
import Amazonka.Comprehend.StartPiiEntitiesDetectionJob
import Amazonka.Comprehend.StartSentimentDetectionJob
import Amazonka.Comprehend.StartTopicsDetectionJob
import Amazonka.Comprehend.StopDominantLanguageDetectionJob
import Amazonka.Comprehend.StopEntitiesDetectionJob
import Amazonka.Comprehend.StopEventsDetectionJob
import Amazonka.Comprehend.StopKeyPhrasesDetectionJob
import Amazonka.Comprehend.StopPiiEntitiesDetectionJob
import Amazonka.Comprehend.StopSentimentDetectionJob
import Amazonka.Comprehend.StopTrainingDocumentClassifier
import Amazonka.Comprehend.StopTrainingEntityRecognizer
import Amazonka.Comprehend.TagResource
import Amazonka.Comprehend.Types.AugmentedManifestsListItem
import Amazonka.Comprehend.Types.BatchDetectDominantLanguageItemResult
import Amazonka.Comprehend.Types.BatchDetectEntitiesItemResult
import Amazonka.Comprehend.Types.BatchDetectKeyPhrasesItemResult
import Amazonka.Comprehend.Types.BatchDetectSentimentItemResult
import Amazonka.Comprehend.Types.BatchDetectSyntaxItemResult
import Amazonka.Comprehend.Types.BatchItemError
import Amazonka.Comprehend.Types.ClassifierEvaluationMetrics
import Amazonka.Comprehend.Types.ClassifierMetadata
import Amazonka.Comprehend.Types.DocumentClass
import Amazonka.Comprehend.Types.DocumentClassificationJobFilter
import Amazonka.Comprehend.Types.DocumentClassificationJobProperties
import Amazonka.Comprehend.Types.DocumentClassifierFilter
import Amazonka.Comprehend.Types.DocumentClassifierInputDataConfig
import Amazonka.Comprehend.Types.DocumentClassifierOutputDataConfig
import Amazonka.Comprehend.Types.DocumentClassifierProperties
import Amazonka.Comprehend.Types.DocumentClassifierSummary
import Amazonka.Comprehend.Types.DocumentLabel
import Amazonka.Comprehend.Types.DocumentReaderConfig
import Amazonka.Comprehend.Types.DominantLanguage
import Amazonka.Comprehend.Types.DominantLanguageDetectionJobFilter
import Amazonka.Comprehend.Types.DominantLanguageDetectionJobProperties
import Amazonka.Comprehend.Types.EndpointFilter
import Amazonka.Comprehend.Types.EndpointProperties
import Amazonka.Comprehend.Types.EntitiesDetectionJobFilter
import Amazonka.Comprehend.Types.EntitiesDetectionJobProperties
import Amazonka.Comprehend.Types.Entity
import Amazonka.Comprehend.Types.EntityLabel
import Amazonka.Comprehend.Types.EntityRecognizerAnnotations
import Amazonka.Comprehend.Types.EntityRecognizerDocuments
import Amazonka.Comprehend.Types.EntityRecognizerEntityList
import Amazonka.Comprehend.Types.EntityRecognizerEvaluationMetrics
import Amazonka.Comprehend.Types.EntityRecognizerFilter
import Amazonka.Comprehend.Types.EntityRecognizerInputDataConfig
import Amazonka.Comprehend.Types.EntityRecognizerMetadata
import Amazonka.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem
import Amazonka.Comprehend.Types.EntityRecognizerProperties
import Amazonka.Comprehend.Types.EntityRecognizerSummary
import Amazonka.Comprehend.Types.EntityTypesEvaluationMetrics
import Amazonka.Comprehend.Types.EntityTypesListItem
import Amazonka.Comprehend.Types.EventsDetectionJobFilter
import Amazonka.Comprehend.Types.EventsDetectionJobProperties
import Amazonka.Comprehend.Types.InputDataConfig
import Amazonka.Comprehend.Types.KeyPhrase
import Amazonka.Comprehend.Types.KeyPhrasesDetectionJobFilter
import Amazonka.Comprehend.Types.KeyPhrasesDetectionJobProperties
import Amazonka.Comprehend.Types.OutputDataConfig
import Amazonka.Comprehend.Types.PartOfSpeechTag
import Amazonka.Comprehend.Types.PiiEntitiesDetectionJobFilter
import Amazonka.Comprehend.Types.PiiEntitiesDetectionJobProperties
import Amazonka.Comprehend.Types.PiiEntity
import Amazonka.Comprehend.Types.PiiOutputDataConfig
import Amazonka.Comprehend.Types.RedactionConfig
import Amazonka.Comprehend.Types.SentimentDetectionJobFilter
import Amazonka.Comprehend.Types.SentimentDetectionJobProperties
import Amazonka.Comprehend.Types.SentimentScore
import Amazonka.Comprehend.Types.SyntaxToken
import Amazonka.Comprehend.Types.Tag
import Amazonka.Comprehend.Types.TopicsDetectionJobFilter
import Amazonka.Comprehend.Types.TopicsDetectionJobProperties
import Amazonka.Comprehend.Types.VpcConfig
import Amazonka.Comprehend.UntagResource
import Amazonka.Comprehend.UpdateEndpoint

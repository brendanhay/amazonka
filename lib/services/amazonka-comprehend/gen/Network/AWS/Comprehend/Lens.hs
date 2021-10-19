{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Lens
  ( -- * Operations

    -- ** BatchDetectSentiment
    batchDetectSentiment_textList,
    batchDetectSentiment_languageCode,
    batchDetectSentimentResponse_httpStatus,
    batchDetectSentimentResponse_resultList,
    batchDetectSentimentResponse_errorList,

    -- ** DeleteEntityRecognizer
    deleteEntityRecognizer_entityRecognizerArn,
    deleteEntityRecognizerResponse_httpStatus,

    -- ** DescribeKeyPhrasesDetectionJob
    describeKeyPhrasesDetectionJob_jobId,
    describeKeyPhrasesDetectionJobResponse_keyPhrasesDetectionJobProperties,
    describeKeyPhrasesDetectionJobResponse_httpStatus,

    -- ** ListEntitiesDetectionJobs
    listEntitiesDetectionJobs_nextToken,
    listEntitiesDetectionJobs_filter,
    listEntitiesDetectionJobs_maxResults,
    listEntitiesDetectionJobsResponse_entitiesDetectionJobPropertiesList,
    listEntitiesDetectionJobsResponse_nextToken,
    listEntitiesDetectionJobsResponse_httpStatus,

    -- ** CreateEndpoint
    createEndpoint_dataAccessRoleArn,
    createEndpoint_clientRequestToken,
    createEndpoint_tags,
    createEndpoint_endpointName,
    createEndpoint_modelArn,
    createEndpoint_desiredInferenceUnits,
    createEndpointResponse_endpointArn,
    createEndpointResponse_httpStatus,

    -- ** StopEventsDetectionJob
    stopEventsDetectionJob_jobId,
    stopEventsDetectionJobResponse_jobId,
    stopEventsDetectionJobResponse_jobStatus,
    stopEventsDetectionJobResponse_httpStatus,

    -- ** StartSentimentDetectionJob
    startSentimentDetectionJob_jobName,
    startSentimentDetectionJob_vpcConfig,
    startSentimentDetectionJob_volumeKmsKeyId,
    startSentimentDetectionJob_clientRequestToken,
    startSentimentDetectionJob_tags,
    startSentimentDetectionJob_inputDataConfig,
    startSentimentDetectionJob_outputDataConfig,
    startSentimentDetectionJob_dataAccessRoleArn,
    startSentimentDetectionJob_languageCode,
    startSentimentDetectionJobResponse_jobId,
    startSentimentDetectionJobResponse_jobArn,
    startSentimentDetectionJobResponse_jobStatus,
    startSentimentDetectionJobResponse_httpStatus,

    -- ** BatchDetectSyntax
    batchDetectSyntax_textList,
    batchDetectSyntax_languageCode,
    batchDetectSyntaxResponse_httpStatus,
    batchDetectSyntaxResponse_resultList,
    batchDetectSyntaxResponse_errorList,

    -- ** StartTopicsDetectionJob
    startTopicsDetectionJob_jobName,
    startTopicsDetectionJob_vpcConfig,
    startTopicsDetectionJob_volumeKmsKeyId,
    startTopicsDetectionJob_numberOfTopics,
    startTopicsDetectionJob_clientRequestToken,
    startTopicsDetectionJob_tags,
    startTopicsDetectionJob_inputDataConfig,
    startTopicsDetectionJob_outputDataConfig,
    startTopicsDetectionJob_dataAccessRoleArn,
    startTopicsDetectionJobResponse_jobId,
    startTopicsDetectionJobResponse_jobArn,
    startTopicsDetectionJobResponse_jobStatus,
    startTopicsDetectionJobResponse_httpStatus,

    -- ** DescribeEventsDetectionJob
    describeEventsDetectionJob_jobId,
    describeEventsDetectionJobResponse_eventsDetectionJobProperties,
    describeEventsDetectionJobResponse_httpStatus,

    -- ** DeleteEndpoint
    deleteEndpoint_endpointArn,
    deleteEndpointResponse_httpStatus,

    -- ** UpdateEndpoint
    updateEndpoint_desiredModelArn,
    updateEndpoint_desiredInferenceUnits,
    updateEndpoint_desiredDataAccessRoleArn,
    updateEndpoint_endpointArn,
    updateEndpointResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** BatchDetectKeyPhrases
    batchDetectKeyPhrases_textList,
    batchDetectKeyPhrases_languageCode,
    batchDetectKeyPhrasesResponse_httpStatus,
    batchDetectKeyPhrasesResponse_resultList,
    batchDetectKeyPhrasesResponse_errorList,

    -- ** DescribeSentimentDetectionJob
    describeSentimentDetectionJob_jobId,
    describeSentimentDetectionJobResponse_sentimentDetectionJobProperties,
    describeSentimentDetectionJobResponse_httpStatus,

    -- ** StartEntitiesDetectionJob
    startEntitiesDetectionJob_entityRecognizerArn,
    startEntitiesDetectionJob_jobName,
    startEntitiesDetectionJob_vpcConfig,
    startEntitiesDetectionJob_volumeKmsKeyId,
    startEntitiesDetectionJob_clientRequestToken,
    startEntitiesDetectionJob_tags,
    startEntitiesDetectionJob_inputDataConfig,
    startEntitiesDetectionJob_outputDataConfig,
    startEntitiesDetectionJob_dataAccessRoleArn,
    startEntitiesDetectionJob_languageCode,
    startEntitiesDetectionJobResponse_jobId,
    startEntitiesDetectionJobResponse_jobArn,
    startEntitiesDetectionJobResponse_jobStatus,
    startEntitiesDetectionJobResponse_httpStatus,

    -- ** StopPiiEntitiesDetectionJob
    stopPiiEntitiesDetectionJob_jobId,
    stopPiiEntitiesDetectionJobResponse_jobId,
    stopPiiEntitiesDetectionJobResponse_jobStatus,
    stopPiiEntitiesDetectionJobResponse_httpStatus,

    -- ** DescribeEntityRecognizer
    describeEntityRecognizer_entityRecognizerArn,
    describeEntityRecognizerResponse_entityRecognizerProperties,
    describeEntityRecognizerResponse_httpStatus,

    -- ** DetectSentiment
    detectSentiment_text,
    detectSentiment_languageCode,
    detectSentimentResponse_sentiment,
    detectSentimentResponse_sentimentScore,
    detectSentimentResponse_httpStatus,

    -- ** StartDominantLanguageDetectionJob
    startDominantLanguageDetectionJob_jobName,
    startDominantLanguageDetectionJob_vpcConfig,
    startDominantLanguageDetectionJob_volumeKmsKeyId,
    startDominantLanguageDetectionJob_clientRequestToken,
    startDominantLanguageDetectionJob_tags,
    startDominantLanguageDetectionJob_inputDataConfig,
    startDominantLanguageDetectionJob_outputDataConfig,
    startDominantLanguageDetectionJob_dataAccessRoleArn,
    startDominantLanguageDetectionJobResponse_jobId,
    startDominantLanguageDetectionJobResponse_jobArn,
    startDominantLanguageDetectionJobResponse_jobStatus,
    startDominantLanguageDetectionJobResponse_httpStatus,

    -- ** StopTrainingDocumentClassifier
    stopTrainingDocumentClassifier_documentClassifierArn,
    stopTrainingDocumentClassifierResponse_httpStatus,

    -- ** DescribeDocumentClassificationJob
    describeDocumentClassificationJob_jobId,
    describeDocumentClassificationJobResponse_documentClassificationJobProperties,
    describeDocumentClassificationJobResponse_httpStatus,

    -- ** ContainsPiiEntities
    containsPiiEntities_text,
    containsPiiEntities_languageCode,
    containsPiiEntitiesResponse_labels,
    containsPiiEntitiesResponse_httpStatus,

    -- ** ListEventsDetectionJobs
    listEventsDetectionJobs_nextToken,
    listEventsDetectionJobs_filter,
    listEventsDetectionJobs_maxResults,
    listEventsDetectionJobsResponse_eventsDetectionJobPropertiesList,
    listEventsDetectionJobsResponse_nextToken,
    listEventsDetectionJobsResponse_httpStatus,

    -- ** BatchDetectEntities
    batchDetectEntities_textList,
    batchDetectEntities_languageCode,
    batchDetectEntitiesResponse_httpStatus,
    batchDetectEntitiesResponse_resultList,
    batchDetectEntitiesResponse_errorList,

    -- ** CreateEntityRecognizer
    createEntityRecognizer_versionName,
    createEntityRecognizer_modelKmsKeyId,
    createEntityRecognizer_vpcConfig,
    createEntityRecognizer_volumeKmsKeyId,
    createEntityRecognizer_clientRequestToken,
    createEntityRecognizer_tags,
    createEntityRecognizer_recognizerName,
    createEntityRecognizer_dataAccessRoleArn,
    createEntityRecognizer_inputDataConfig,
    createEntityRecognizer_languageCode,
    createEntityRecognizerResponse_entityRecognizerArn,
    createEntityRecognizerResponse_httpStatus,

    -- ** StopKeyPhrasesDetectionJob
    stopKeyPhrasesDetectionJob_jobId,
    stopKeyPhrasesDetectionJobResponse_jobId,
    stopKeyPhrasesDetectionJobResponse_jobStatus,
    stopKeyPhrasesDetectionJobResponse_httpStatus,

    -- ** CreateDocumentClassifier
    createDocumentClassifier_versionName,
    createDocumentClassifier_modelKmsKeyId,
    createDocumentClassifier_mode,
    createDocumentClassifier_vpcConfig,
    createDocumentClassifier_volumeKmsKeyId,
    createDocumentClassifier_outputDataConfig,
    createDocumentClassifier_clientRequestToken,
    createDocumentClassifier_tags,
    createDocumentClassifier_documentClassifierName,
    createDocumentClassifier_dataAccessRoleArn,
    createDocumentClassifier_inputDataConfig,
    createDocumentClassifier_languageCode,
    createDocumentClassifierResponse_documentClassifierArn,
    createDocumentClassifierResponse_httpStatus,

    -- ** ListPiiEntitiesDetectionJobs
    listPiiEntitiesDetectionJobs_nextToken,
    listPiiEntitiesDetectionJobs_filter,
    listPiiEntitiesDetectionJobs_maxResults,
    listPiiEntitiesDetectionJobsResponse_nextToken,
    listPiiEntitiesDetectionJobsResponse_piiEntitiesDetectionJobPropertiesList,
    listPiiEntitiesDetectionJobsResponse_httpStatus,

    -- ** ListEntityRecognizers
    listEntityRecognizers_nextToken,
    listEntityRecognizers_filter,
    listEntityRecognizers_maxResults,
    listEntityRecognizersResponse_nextToken,
    listEntityRecognizersResponse_entityRecognizerPropertiesList,
    listEntityRecognizersResponse_httpStatus,

    -- ** StopSentimentDetectionJob
    stopSentimentDetectionJob_jobId,
    stopSentimentDetectionJobResponse_jobId,
    stopSentimentDetectionJobResponse_jobStatus,
    stopSentimentDetectionJobResponse_httpStatus,

    -- ** DetectDominantLanguage
    detectDominantLanguage_text,
    detectDominantLanguageResponse_languages,
    detectDominantLanguageResponse_httpStatus,

    -- ** ClassifyDocument
    classifyDocument_text,
    classifyDocument_endpointArn,
    classifyDocumentResponse_labels,
    classifyDocumentResponse_classes,
    classifyDocumentResponse_httpStatus,

    -- ** StartEventsDetectionJob
    startEventsDetectionJob_jobName,
    startEventsDetectionJob_clientRequestToken,
    startEventsDetectionJob_tags,
    startEventsDetectionJob_inputDataConfig,
    startEventsDetectionJob_outputDataConfig,
    startEventsDetectionJob_dataAccessRoleArn,
    startEventsDetectionJob_languageCode,
    startEventsDetectionJob_targetEventTypes,
    startEventsDetectionJobResponse_jobId,
    startEventsDetectionJobResponse_jobArn,
    startEventsDetectionJobResponse_jobStatus,
    startEventsDetectionJobResponse_httpStatus,

    -- ** DescribeTopicsDetectionJob
    describeTopicsDetectionJob_jobId,
    describeTopicsDetectionJobResponse_topicsDetectionJobProperties,
    describeTopicsDetectionJobResponse_httpStatus,

    -- ** ListDocumentClassificationJobs
    listDocumentClassificationJobs_nextToken,
    listDocumentClassificationJobs_filter,
    listDocumentClassificationJobs_maxResults,
    listDocumentClassificationJobsResponse_nextToken,
    listDocumentClassificationJobsResponse_documentClassificationJobPropertiesList,
    listDocumentClassificationJobsResponse_httpStatus,

    -- ** DetectPiiEntities
    detectPiiEntities_text,
    detectPiiEntities_languageCode,
    detectPiiEntitiesResponse_entities,
    detectPiiEntitiesResponse_httpStatus,

    -- ** ListEndpoints
    listEndpoints_nextToken,
    listEndpoints_filter,
    listEndpoints_maxResults,
    listEndpointsResponse_endpointPropertiesList,
    listEndpointsResponse_nextToken,
    listEndpointsResponse_httpStatus,

    -- ** DetectEntities
    detectEntities_languageCode,
    detectEntities_endpointArn,
    detectEntities_text,
    detectEntitiesResponse_entities,
    detectEntitiesResponse_httpStatus,

    -- ** DescribeDocumentClassifier
    describeDocumentClassifier_documentClassifierArn,
    describeDocumentClassifierResponse_documentClassifierProperties,
    describeDocumentClassifierResponse_httpStatus,

    -- ** DescribeDominantLanguageDetectionJob
    describeDominantLanguageDetectionJob_jobId,
    describeDominantLanguageDetectionJobResponse_dominantLanguageDetectionJobProperties,
    describeDominantLanguageDetectionJobResponse_httpStatus,

    -- ** ListEntityRecognizerSummaries
    listEntityRecognizerSummaries_nextToken,
    listEntityRecognizerSummaries_maxResults,
    listEntityRecognizerSummariesResponse_nextToken,
    listEntityRecognizerSummariesResponse_entityRecognizerSummariesList,
    listEntityRecognizerSummariesResponse_httpStatus,

    -- ** StopEntitiesDetectionJob
    stopEntitiesDetectionJob_jobId,
    stopEntitiesDetectionJobResponse_jobId,
    stopEntitiesDetectionJobResponse_jobStatus,
    stopEntitiesDetectionJobResponse_httpStatus,

    -- ** StopTrainingEntityRecognizer
    stopTrainingEntityRecognizer_entityRecognizerArn,
    stopTrainingEntityRecognizerResponse_httpStatus,

    -- ** StartPiiEntitiesDetectionJob
    startPiiEntitiesDetectionJob_jobName,
    startPiiEntitiesDetectionJob_redactionConfig,
    startPiiEntitiesDetectionJob_clientRequestToken,
    startPiiEntitiesDetectionJob_tags,
    startPiiEntitiesDetectionJob_inputDataConfig,
    startPiiEntitiesDetectionJob_outputDataConfig,
    startPiiEntitiesDetectionJob_mode,
    startPiiEntitiesDetectionJob_dataAccessRoleArn,
    startPiiEntitiesDetectionJob_languageCode,
    startPiiEntitiesDetectionJobResponse_jobId,
    startPiiEntitiesDetectionJobResponse_jobArn,
    startPiiEntitiesDetectionJobResponse_jobStatus,
    startPiiEntitiesDetectionJobResponse_httpStatus,

    -- ** ListKeyPhrasesDetectionJobs
    listKeyPhrasesDetectionJobs_nextToken,
    listKeyPhrasesDetectionJobs_filter,
    listKeyPhrasesDetectionJobs_maxResults,
    listKeyPhrasesDetectionJobsResponse_keyPhrasesDetectionJobPropertiesList,
    listKeyPhrasesDetectionJobsResponse_nextToken,
    listKeyPhrasesDetectionJobsResponse_httpStatus,

    -- ** DescribeEntitiesDetectionJob
    describeEntitiesDetectionJob_jobId,
    describeEntitiesDetectionJobResponse_entitiesDetectionJobProperties,
    describeEntitiesDetectionJobResponse_httpStatus,

    -- ** ListDocumentClassifierSummaries
    listDocumentClassifierSummaries_nextToken,
    listDocumentClassifierSummaries_maxResults,
    listDocumentClassifierSummariesResponse_nextToken,
    listDocumentClassifierSummariesResponse_documentClassifierSummariesList,
    listDocumentClassifierSummariesResponse_httpStatus,

    -- ** StopDominantLanguageDetectionJob
    stopDominantLanguageDetectionJob_jobId,
    stopDominantLanguageDetectionJobResponse_jobId,
    stopDominantLanguageDetectionJobResponse_jobStatus,
    stopDominantLanguageDetectionJobResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** DescribePiiEntitiesDetectionJob
    describePiiEntitiesDetectionJob_jobId,
    describePiiEntitiesDetectionJobResponse_piiEntitiesDetectionJobProperties,
    describePiiEntitiesDetectionJobResponse_httpStatus,

    -- ** ListTopicsDetectionJobs
    listTopicsDetectionJobs_nextToken,
    listTopicsDetectionJobs_filter,
    listTopicsDetectionJobs_maxResults,
    listTopicsDetectionJobsResponse_nextToken,
    listTopicsDetectionJobsResponse_topicsDetectionJobPropertiesList,
    listTopicsDetectionJobsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** BatchDetectDominantLanguage
    batchDetectDominantLanguage_textList,
    batchDetectDominantLanguageResponse_httpStatus,
    batchDetectDominantLanguageResponse_resultList,
    batchDetectDominantLanguageResponse_errorList,

    -- ** StartDocumentClassificationJob
    startDocumentClassificationJob_jobName,
    startDocumentClassificationJob_vpcConfig,
    startDocumentClassificationJob_volumeKmsKeyId,
    startDocumentClassificationJob_clientRequestToken,
    startDocumentClassificationJob_tags,
    startDocumentClassificationJob_documentClassifierArn,
    startDocumentClassificationJob_inputDataConfig,
    startDocumentClassificationJob_outputDataConfig,
    startDocumentClassificationJob_dataAccessRoleArn,
    startDocumentClassificationJobResponse_jobId,
    startDocumentClassificationJobResponse_jobArn,
    startDocumentClassificationJobResponse_jobStatus,
    startDocumentClassificationJobResponse_httpStatus,

    -- ** DetectKeyPhrases
    detectKeyPhrases_text,
    detectKeyPhrases_languageCode,
    detectKeyPhrasesResponse_keyPhrases,
    detectKeyPhrasesResponse_httpStatus,

    -- ** DetectSyntax
    detectSyntax_text,
    detectSyntax_languageCode,
    detectSyntaxResponse_syntaxTokens,
    detectSyntaxResponse_httpStatus,

    -- ** DescribeEndpoint
    describeEndpoint_endpointArn,
    describeEndpointResponse_endpointProperties,
    describeEndpointResponse_httpStatus,

    -- ** ListSentimentDetectionJobs
    listSentimentDetectionJobs_nextToken,
    listSentimentDetectionJobs_filter,
    listSentimentDetectionJobs_maxResults,
    listSentimentDetectionJobsResponse_nextToken,
    listSentimentDetectionJobsResponse_sentimentDetectionJobPropertiesList,
    listSentimentDetectionJobsResponse_httpStatus,

    -- ** DeleteDocumentClassifier
    deleteDocumentClassifier_documentClassifierArn,
    deleteDocumentClassifierResponse_httpStatus,

    -- ** ListDominantLanguageDetectionJobs
    listDominantLanguageDetectionJobs_nextToken,
    listDominantLanguageDetectionJobs_filter,
    listDominantLanguageDetectionJobs_maxResults,
    listDominantLanguageDetectionJobsResponse_nextToken,
    listDominantLanguageDetectionJobsResponse_dominantLanguageDetectionJobPropertiesList,
    listDominantLanguageDetectionJobsResponse_httpStatus,

    -- ** StartKeyPhrasesDetectionJob
    startKeyPhrasesDetectionJob_jobName,
    startKeyPhrasesDetectionJob_vpcConfig,
    startKeyPhrasesDetectionJob_volumeKmsKeyId,
    startKeyPhrasesDetectionJob_clientRequestToken,
    startKeyPhrasesDetectionJob_tags,
    startKeyPhrasesDetectionJob_inputDataConfig,
    startKeyPhrasesDetectionJob_outputDataConfig,
    startKeyPhrasesDetectionJob_dataAccessRoleArn,
    startKeyPhrasesDetectionJob_languageCode,
    startKeyPhrasesDetectionJobResponse_jobId,
    startKeyPhrasesDetectionJobResponse_jobArn,
    startKeyPhrasesDetectionJobResponse_jobStatus,
    startKeyPhrasesDetectionJobResponse_httpStatus,

    -- ** ListDocumentClassifiers
    listDocumentClassifiers_nextToken,
    listDocumentClassifiers_filter,
    listDocumentClassifiers_maxResults,
    listDocumentClassifiersResponse_nextToken,
    listDocumentClassifiersResponse_documentClassifierPropertiesList,
    listDocumentClassifiersResponse_httpStatus,

    -- * Types

    -- ** AugmentedManifestsListItem
    augmentedManifestsListItem_sourceDocumentsS3Uri,
    augmentedManifestsListItem_documentType,
    augmentedManifestsListItem_split,
    augmentedManifestsListItem_annotationDataS3Uri,
    augmentedManifestsListItem_s3Uri,
    augmentedManifestsListItem_attributeNames,

    -- ** BatchDetectDominantLanguageItemResult
    batchDetectDominantLanguageItemResult_languages,
    batchDetectDominantLanguageItemResult_index,

    -- ** BatchDetectEntitiesItemResult
    batchDetectEntitiesItemResult_entities,
    batchDetectEntitiesItemResult_index,

    -- ** BatchDetectKeyPhrasesItemResult
    batchDetectKeyPhrasesItemResult_index,
    batchDetectKeyPhrasesItemResult_keyPhrases,

    -- ** BatchDetectSentimentItemResult
    batchDetectSentimentItemResult_sentiment,
    batchDetectSentimentItemResult_sentimentScore,
    batchDetectSentimentItemResult_index,

    -- ** BatchDetectSyntaxItemResult
    batchDetectSyntaxItemResult_index,
    batchDetectSyntaxItemResult_syntaxTokens,

    -- ** BatchItemError
    batchItemError_errorCode,
    batchItemError_errorMessage,
    batchItemError_index,

    -- ** ClassifierEvaluationMetrics
    classifierEvaluationMetrics_microPrecision,
    classifierEvaluationMetrics_microF1Score,
    classifierEvaluationMetrics_recall,
    classifierEvaluationMetrics_precision,
    classifierEvaluationMetrics_microRecall,
    classifierEvaluationMetrics_f1Score,
    classifierEvaluationMetrics_hammingLoss,
    classifierEvaluationMetrics_accuracy,

    -- ** ClassifierMetadata
    classifierMetadata_numberOfLabels,
    classifierMetadata_evaluationMetrics,
    classifierMetadata_numberOfTrainedDocuments,
    classifierMetadata_numberOfTestDocuments,

    -- ** DocumentClass
    documentClass_score,
    documentClass_name,

    -- ** DocumentClassificationJobFilter
    documentClassificationJobFilter_submitTimeAfter,
    documentClassificationJobFilter_submitTimeBefore,
    documentClassificationJobFilter_jobName,
    documentClassificationJobFilter_jobStatus,

    -- ** DocumentClassificationJobProperties
    documentClassificationJobProperties_jobId,
    documentClassificationJobProperties_jobArn,
    documentClassificationJobProperties_documentClassifierArn,
    documentClassificationJobProperties_jobName,
    documentClassificationJobProperties_inputDataConfig,
    documentClassificationJobProperties_vpcConfig,
    documentClassificationJobProperties_volumeKmsKeyId,
    documentClassificationJobProperties_endTime,
    documentClassificationJobProperties_outputDataConfig,
    documentClassificationJobProperties_dataAccessRoleArn,
    documentClassificationJobProperties_jobStatus,
    documentClassificationJobProperties_message,
    documentClassificationJobProperties_submitTime,

    -- ** DocumentClassifierFilter
    documentClassifierFilter_status,
    documentClassifierFilter_submitTimeAfter,
    documentClassifierFilter_submitTimeBefore,
    documentClassifierFilter_documentClassifierName,

    -- ** DocumentClassifierInputDataConfig
    documentClassifierInputDataConfig_augmentedManifests,
    documentClassifierInputDataConfig_dataFormat,
    documentClassifierInputDataConfig_labelDelimiter,
    documentClassifierInputDataConfig_testS3Uri,
    documentClassifierInputDataConfig_s3Uri,

    -- ** DocumentClassifierOutputDataConfig
    documentClassifierOutputDataConfig_kmsKeyId,
    documentClassifierOutputDataConfig_s3Uri,

    -- ** DocumentClassifierProperties
    documentClassifierProperties_status,
    documentClassifierProperties_languageCode,
    documentClassifierProperties_classifierMetadata,
    documentClassifierProperties_trainingEndTime,
    documentClassifierProperties_documentClassifierArn,
    documentClassifierProperties_versionName,
    documentClassifierProperties_modelKmsKeyId,
    documentClassifierProperties_mode,
    documentClassifierProperties_inputDataConfig,
    documentClassifierProperties_vpcConfig,
    documentClassifierProperties_volumeKmsKeyId,
    documentClassifierProperties_endTime,
    documentClassifierProperties_outputDataConfig,
    documentClassifierProperties_trainingStartTime,
    documentClassifierProperties_dataAccessRoleArn,
    documentClassifierProperties_message,
    documentClassifierProperties_submitTime,

    -- ** DocumentClassifierSummary
    documentClassifierSummary_latestVersionCreatedAt,
    documentClassifierSummary_documentClassifierName,
    documentClassifierSummary_latestVersionStatus,
    documentClassifierSummary_numberOfVersions,
    documentClassifierSummary_latestVersionName,

    -- ** DocumentLabel
    documentLabel_score,
    documentLabel_name,

    -- ** DocumentReaderConfig
    documentReaderConfig_featureTypes,
    documentReaderConfig_documentReadMode,
    documentReaderConfig_documentReadAction,

    -- ** DominantLanguage
    dominantLanguage_languageCode,
    dominantLanguage_score,

    -- ** DominantLanguageDetectionJobFilter
    dominantLanguageDetectionJobFilter_submitTimeAfter,
    dominantLanguageDetectionJobFilter_submitTimeBefore,
    dominantLanguageDetectionJobFilter_jobName,
    dominantLanguageDetectionJobFilter_jobStatus,

    -- ** DominantLanguageDetectionJobProperties
    dominantLanguageDetectionJobProperties_jobId,
    dominantLanguageDetectionJobProperties_jobArn,
    dominantLanguageDetectionJobProperties_jobName,
    dominantLanguageDetectionJobProperties_inputDataConfig,
    dominantLanguageDetectionJobProperties_vpcConfig,
    dominantLanguageDetectionJobProperties_volumeKmsKeyId,
    dominantLanguageDetectionJobProperties_endTime,
    dominantLanguageDetectionJobProperties_outputDataConfig,
    dominantLanguageDetectionJobProperties_dataAccessRoleArn,
    dominantLanguageDetectionJobProperties_jobStatus,
    dominantLanguageDetectionJobProperties_message,
    dominantLanguageDetectionJobProperties_submitTime,

    -- ** EndpointFilter
    endpointFilter_status,
    endpointFilter_modelArn,
    endpointFilter_creationTimeAfter,
    endpointFilter_creationTimeBefore,

    -- ** EndpointProperties
    endpointProperties_creationTime,
    endpointProperties_status,
    endpointProperties_modelArn,
    endpointProperties_desiredModelArn,
    endpointProperties_lastModifiedTime,
    endpointProperties_desiredInferenceUnits,
    endpointProperties_currentInferenceUnits,
    endpointProperties_dataAccessRoleArn,
    endpointProperties_desiredDataAccessRoleArn,
    endpointProperties_message,
    endpointProperties_endpointArn,

    -- ** EntitiesDetectionJobFilter
    entitiesDetectionJobFilter_submitTimeAfter,
    entitiesDetectionJobFilter_submitTimeBefore,
    entitiesDetectionJobFilter_jobName,
    entitiesDetectionJobFilter_jobStatus,

    -- ** EntitiesDetectionJobProperties
    entitiesDetectionJobProperties_languageCode,
    entitiesDetectionJobProperties_jobId,
    entitiesDetectionJobProperties_jobArn,
    entitiesDetectionJobProperties_entityRecognizerArn,
    entitiesDetectionJobProperties_jobName,
    entitiesDetectionJobProperties_inputDataConfig,
    entitiesDetectionJobProperties_vpcConfig,
    entitiesDetectionJobProperties_volumeKmsKeyId,
    entitiesDetectionJobProperties_endTime,
    entitiesDetectionJobProperties_outputDataConfig,
    entitiesDetectionJobProperties_dataAccessRoleArn,
    entitiesDetectionJobProperties_jobStatus,
    entitiesDetectionJobProperties_message,
    entitiesDetectionJobProperties_submitTime,

    -- ** Entity
    entity_beginOffset,
    entity_text,
    entity_score,
    entity_endOffset,
    entity_type,

    -- ** EntityLabel
    entityLabel_score,
    entityLabel_name,

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
    entityRecognizerEvaluationMetrics_recall,
    entityRecognizerEvaluationMetrics_precision,
    entityRecognizerEvaluationMetrics_f1Score,

    -- ** EntityRecognizerFilter
    entityRecognizerFilter_status,
    entityRecognizerFilter_submitTimeAfter,
    entityRecognizerFilter_submitTimeBefore,
    entityRecognizerFilter_recognizerName,

    -- ** EntityRecognizerInputDataConfig
    entityRecognizerInputDataConfig_augmentedManifests,
    entityRecognizerInputDataConfig_annotations,
    entityRecognizerInputDataConfig_dataFormat,
    entityRecognizerInputDataConfig_documents,
    entityRecognizerInputDataConfig_entityList,
    entityRecognizerInputDataConfig_entityTypes,

    -- ** EntityRecognizerMetadata
    entityRecognizerMetadata_entityTypes,
    entityRecognizerMetadata_evaluationMetrics,
    entityRecognizerMetadata_numberOfTrainedDocuments,
    entityRecognizerMetadata_numberOfTestDocuments,

    -- ** EntityRecognizerMetadataEntityTypesListItem
    entityRecognizerMetadataEntityTypesListItem_evaluationMetrics,
    entityRecognizerMetadataEntityTypesListItem_type,
    entityRecognizerMetadataEntityTypesListItem_numberOfTrainMentions,

    -- ** EntityRecognizerProperties
    entityRecognizerProperties_status,
    entityRecognizerProperties_languageCode,
    entityRecognizerProperties_trainingEndTime,
    entityRecognizerProperties_versionName,
    entityRecognizerProperties_entityRecognizerArn,
    entityRecognizerProperties_modelKmsKeyId,
    entityRecognizerProperties_inputDataConfig,
    entityRecognizerProperties_vpcConfig,
    entityRecognizerProperties_volumeKmsKeyId,
    entityRecognizerProperties_endTime,
    entityRecognizerProperties_trainingStartTime,
    entityRecognizerProperties_dataAccessRoleArn,
    entityRecognizerProperties_recognizerMetadata,
    entityRecognizerProperties_message,
    entityRecognizerProperties_submitTime,

    -- ** EntityRecognizerSummary
    entityRecognizerSummary_latestVersionCreatedAt,
    entityRecognizerSummary_latestVersionStatus,
    entityRecognizerSummary_numberOfVersions,
    entityRecognizerSummary_latestVersionName,
    entityRecognizerSummary_recognizerName,

    -- ** EntityTypesEvaluationMetrics
    entityTypesEvaluationMetrics_recall,
    entityTypesEvaluationMetrics_precision,
    entityTypesEvaluationMetrics_f1Score,

    -- ** EntityTypesListItem
    entityTypesListItem_type,

    -- ** EventsDetectionJobFilter
    eventsDetectionJobFilter_submitTimeAfter,
    eventsDetectionJobFilter_submitTimeBefore,
    eventsDetectionJobFilter_jobName,
    eventsDetectionJobFilter_jobStatus,

    -- ** EventsDetectionJobProperties
    eventsDetectionJobProperties_languageCode,
    eventsDetectionJobProperties_jobId,
    eventsDetectionJobProperties_jobArn,
    eventsDetectionJobProperties_jobName,
    eventsDetectionJobProperties_targetEventTypes,
    eventsDetectionJobProperties_inputDataConfig,
    eventsDetectionJobProperties_endTime,
    eventsDetectionJobProperties_outputDataConfig,
    eventsDetectionJobProperties_dataAccessRoleArn,
    eventsDetectionJobProperties_jobStatus,
    eventsDetectionJobProperties_message,
    eventsDetectionJobProperties_submitTime,

    -- ** InputDataConfig
    inputDataConfig_documentReaderConfig,
    inputDataConfig_inputFormat,
    inputDataConfig_s3Uri,

    -- ** KeyPhrase
    keyPhrase_beginOffset,
    keyPhrase_text,
    keyPhrase_score,
    keyPhrase_endOffset,

    -- ** KeyPhrasesDetectionJobFilter
    keyPhrasesDetectionJobFilter_submitTimeAfter,
    keyPhrasesDetectionJobFilter_submitTimeBefore,
    keyPhrasesDetectionJobFilter_jobName,
    keyPhrasesDetectionJobFilter_jobStatus,

    -- ** KeyPhrasesDetectionJobProperties
    keyPhrasesDetectionJobProperties_languageCode,
    keyPhrasesDetectionJobProperties_jobId,
    keyPhrasesDetectionJobProperties_jobArn,
    keyPhrasesDetectionJobProperties_jobName,
    keyPhrasesDetectionJobProperties_inputDataConfig,
    keyPhrasesDetectionJobProperties_vpcConfig,
    keyPhrasesDetectionJobProperties_volumeKmsKeyId,
    keyPhrasesDetectionJobProperties_endTime,
    keyPhrasesDetectionJobProperties_outputDataConfig,
    keyPhrasesDetectionJobProperties_dataAccessRoleArn,
    keyPhrasesDetectionJobProperties_jobStatus,
    keyPhrasesDetectionJobProperties_message,
    keyPhrasesDetectionJobProperties_submitTime,

    -- ** OutputDataConfig
    outputDataConfig_kmsKeyId,
    outputDataConfig_s3Uri,

    -- ** PartOfSpeechTag
    partOfSpeechTag_tag,
    partOfSpeechTag_score,

    -- ** PiiEntitiesDetectionJobFilter
    piiEntitiesDetectionJobFilter_submitTimeAfter,
    piiEntitiesDetectionJobFilter_submitTimeBefore,
    piiEntitiesDetectionJobFilter_jobName,
    piiEntitiesDetectionJobFilter_jobStatus,

    -- ** PiiEntitiesDetectionJobProperties
    piiEntitiesDetectionJobProperties_languageCode,
    piiEntitiesDetectionJobProperties_jobId,
    piiEntitiesDetectionJobProperties_jobArn,
    piiEntitiesDetectionJobProperties_jobName,
    piiEntitiesDetectionJobProperties_mode,
    piiEntitiesDetectionJobProperties_inputDataConfig,
    piiEntitiesDetectionJobProperties_redactionConfig,
    piiEntitiesDetectionJobProperties_endTime,
    piiEntitiesDetectionJobProperties_outputDataConfig,
    piiEntitiesDetectionJobProperties_dataAccessRoleArn,
    piiEntitiesDetectionJobProperties_jobStatus,
    piiEntitiesDetectionJobProperties_message,
    piiEntitiesDetectionJobProperties_submitTime,

    -- ** PiiEntity
    piiEntity_beginOffset,
    piiEntity_score,
    piiEntity_endOffset,
    piiEntity_type,

    -- ** PiiOutputDataConfig
    piiOutputDataConfig_kmsKeyId,
    piiOutputDataConfig_s3Uri,

    -- ** RedactionConfig
    redactionConfig_maskCharacter,
    redactionConfig_maskMode,
    redactionConfig_piiEntityTypes,

    -- ** SentimentDetectionJobFilter
    sentimentDetectionJobFilter_submitTimeAfter,
    sentimentDetectionJobFilter_submitTimeBefore,
    sentimentDetectionJobFilter_jobName,
    sentimentDetectionJobFilter_jobStatus,

    -- ** SentimentDetectionJobProperties
    sentimentDetectionJobProperties_languageCode,
    sentimentDetectionJobProperties_jobId,
    sentimentDetectionJobProperties_jobArn,
    sentimentDetectionJobProperties_jobName,
    sentimentDetectionJobProperties_inputDataConfig,
    sentimentDetectionJobProperties_vpcConfig,
    sentimentDetectionJobProperties_volumeKmsKeyId,
    sentimentDetectionJobProperties_endTime,
    sentimentDetectionJobProperties_outputDataConfig,
    sentimentDetectionJobProperties_dataAccessRoleArn,
    sentimentDetectionJobProperties_jobStatus,
    sentimentDetectionJobProperties_message,
    sentimentDetectionJobProperties_submitTime,

    -- ** SentimentScore
    sentimentScore_mixed,
    sentimentScore_negative,
    sentimentScore_neutral,
    sentimentScore_positive,

    -- ** SyntaxToken
    syntaxToken_beginOffset,
    syntaxToken_text,
    syntaxToken_tokenId,
    syntaxToken_endOffset,
    syntaxToken_partOfSpeech,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TopicsDetectionJobFilter
    topicsDetectionJobFilter_submitTimeAfter,
    topicsDetectionJobFilter_submitTimeBefore,
    topicsDetectionJobFilter_jobName,
    topicsDetectionJobFilter_jobStatus,

    -- ** TopicsDetectionJobProperties
    topicsDetectionJobProperties_jobId,
    topicsDetectionJobProperties_jobArn,
    topicsDetectionJobProperties_jobName,
    topicsDetectionJobProperties_inputDataConfig,
    topicsDetectionJobProperties_vpcConfig,
    topicsDetectionJobProperties_volumeKmsKeyId,
    topicsDetectionJobProperties_endTime,
    topicsDetectionJobProperties_outputDataConfig,
    topicsDetectionJobProperties_dataAccessRoleArn,
    topicsDetectionJobProperties_numberOfTopics,
    topicsDetectionJobProperties_jobStatus,
    topicsDetectionJobProperties_message,
    topicsDetectionJobProperties_submitTime,

    -- ** VpcConfig
    vpcConfig_securityGroupIds,
    vpcConfig_subnets,
  )
where

import Network.AWS.Comprehend.BatchDetectDominantLanguage
import Network.AWS.Comprehend.BatchDetectEntities
import Network.AWS.Comprehend.BatchDetectKeyPhrases
import Network.AWS.Comprehend.BatchDetectSentiment
import Network.AWS.Comprehend.BatchDetectSyntax
import Network.AWS.Comprehend.ClassifyDocument
import Network.AWS.Comprehend.ContainsPiiEntities
import Network.AWS.Comprehend.CreateDocumentClassifier
import Network.AWS.Comprehend.CreateEndpoint
import Network.AWS.Comprehend.CreateEntityRecognizer
import Network.AWS.Comprehend.DeleteDocumentClassifier
import Network.AWS.Comprehend.DeleteEndpoint
import Network.AWS.Comprehend.DeleteEntityRecognizer
import Network.AWS.Comprehend.DescribeDocumentClassificationJob
import Network.AWS.Comprehend.DescribeDocumentClassifier
import Network.AWS.Comprehend.DescribeDominantLanguageDetectionJob
import Network.AWS.Comprehend.DescribeEndpoint
import Network.AWS.Comprehend.DescribeEntitiesDetectionJob
import Network.AWS.Comprehend.DescribeEntityRecognizer
import Network.AWS.Comprehend.DescribeEventsDetectionJob
import Network.AWS.Comprehend.DescribeKeyPhrasesDetectionJob
import Network.AWS.Comprehend.DescribePiiEntitiesDetectionJob
import Network.AWS.Comprehend.DescribeSentimentDetectionJob
import Network.AWS.Comprehend.DescribeTopicsDetectionJob
import Network.AWS.Comprehend.DetectDominantLanguage
import Network.AWS.Comprehend.DetectEntities
import Network.AWS.Comprehend.DetectKeyPhrases
import Network.AWS.Comprehend.DetectPiiEntities
import Network.AWS.Comprehend.DetectSentiment
import Network.AWS.Comprehend.DetectSyntax
import Network.AWS.Comprehend.ListDocumentClassificationJobs
import Network.AWS.Comprehend.ListDocumentClassifierSummaries
import Network.AWS.Comprehend.ListDocumentClassifiers
import Network.AWS.Comprehend.ListDominantLanguageDetectionJobs
import Network.AWS.Comprehend.ListEndpoints
import Network.AWS.Comprehend.ListEntitiesDetectionJobs
import Network.AWS.Comprehend.ListEntityRecognizerSummaries
import Network.AWS.Comprehend.ListEntityRecognizers
import Network.AWS.Comprehend.ListEventsDetectionJobs
import Network.AWS.Comprehend.ListKeyPhrasesDetectionJobs
import Network.AWS.Comprehend.ListPiiEntitiesDetectionJobs
import Network.AWS.Comprehend.ListSentimentDetectionJobs
import Network.AWS.Comprehend.ListTagsForResource
import Network.AWS.Comprehend.ListTopicsDetectionJobs
import Network.AWS.Comprehend.StartDocumentClassificationJob
import Network.AWS.Comprehend.StartDominantLanguageDetectionJob
import Network.AWS.Comprehend.StartEntitiesDetectionJob
import Network.AWS.Comprehend.StartEventsDetectionJob
import Network.AWS.Comprehend.StartKeyPhrasesDetectionJob
import Network.AWS.Comprehend.StartPiiEntitiesDetectionJob
import Network.AWS.Comprehend.StartSentimentDetectionJob
import Network.AWS.Comprehend.StartTopicsDetectionJob
import Network.AWS.Comprehend.StopDominantLanguageDetectionJob
import Network.AWS.Comprehend.StopEntitiesDetectionJob
import Network.AWS.Comprehend.StopEventsDetectionJob
import Network.AWS.Comprehend.StopKeyPhrasesDetectionJob
import Network.AWS.Comprehend.StopPiiEntitiesDetectionJob
import Network.AWS.Comprehend.StopSentimentDetectionJob
import Network.AWS.Comprehend.StopTrainingDocumentClassifier
import Network.AWS.Comprehend.StopTrainingEntityRecognizer
import Network.AWS.Comprehend.TagResource
import Network.AWS.Comprehend.Types.AugmentedManifestsListItem
import Network.AWS.Comprehend.Types.BatchDetectDominantLanguageItemResult
import Network.AWS.Comprehend.Types.BatchDetectEntitiesItemResult
import Network.AWS.Comprehend.Types.BatchDetectKeyPhrasesItemResult
import Network.AWS.Comprehend.Types.BatchDetectSentimentItemResult
import Network.AWS.Comprehend.Types.BatchDetectSyntaxItemResult
import Network.AWS.Comprehend.Types.BatchItemError
import Network.AWS.Comprehend.Types.ClassifierEvaluationMetrics
import Network.AWS.Comprehend.Types.ClassifierMetadata
import Network.AWS.Comprehend.Types.DocumentClass
import Network.AWS.Comprehend.Types.DocumentClassificationJobFilter
import Network.AWS.Comprehend.Types.DocumentClassificationJobProperties
import Network.AWS.Comprehend.Types.DocumentClassifierFilter
import Network.AWS.Comprehend.Types.DocumentClassifierInputDataConfig
import Network.AWS.Comprehend.Types.DocumentClassifierOutputDataConfig
import Network.AWS.Comprehend.Types.DocumentClassifierProperties
import Network.AWS.Comprehend.Types.DocumentClassifierSummary
import Network.AWS.Comprehend.Types.DocumentLabel
import Network.AWS.Comprehend.Types.DocumentReaderConfig
import Network.AWS.Comprehend.Types.DominantLanguage
import Network.AWS.Comprehend.Types.DominantLanguageDetectionJobFilter
import Network.AWS.Comprehend.Types.DominantLanguageDetectionJobProperties
import Network.AWS.Comprehend.Types.EndpointFilter
import Network.AWS.Comprehend.Types.EndpointProperties
import Network.AWS.Comprehend.Types.EntitiesDetectionJobFilter
import Network.AWS.Comprehend.Types.EntitiesDetectionJobProperties
import Network.AWS.Comprehend.Types.Entity
import Network.AWS.Comprehend.Types.EntityLabel
import Network.AWS.Comprehend.Types.EntityRecognizerAnnotations
import Network.AWS.Comprehend.Types.EntityRecognizerDocuments
import Network.AWS.Comprehend.Types.EntityRecognizerEntityList
import Network.AWS.Comprehend.Types.EntityRecognizerEvaluationMetrics
import Network.AWS.Comprehend.Types.EntityRecognizerFilter
import Network.AWS.Comprehend.Types.EntityRecognizerInputDataConfig
import Network.AWS.Comprehend.Types.EntityRecognizerMetadata
import Network.AWS.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem
import Network.AWS.Comprehend.Types.EntityRecognizerProperties
import Network.AWS.Comprehend.Types.EntityRecognizerSummary
import Network.AWS.Comprehend.Types.EntityTypesEvaluationMetrics
import Network.AWS.Comprehend.Types.EntityTypesListItem
import Network.AWS.Comprehend.Types.EventsDetectionJobFilter
import Network.AWS.Comprehend.Types.EventsDetectionJobProperties
import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.KeyPhrase
import Network.AWS.Comprehend.Types.KeyPhrasesDetectionJobFilter
import Network.AWS.Comprehend.Types.KeyPhrasesDetectionJobProperties
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Comprehend.Types.PartOfSpeechTag
import Network.AWS.Comprehend.Types.PiiEntitiesDetectionJobFilter
import Network.AWS.Comprehend.Types.PiiEntitiesDetectionJobProperties
import Network.AWS.Comprehend.Types.PiiEntity
import Network.AWS.Comprehend.Types.PiiOutputDataConfig
import Network.AWS.Comprehend.Types.RedactionConfig
import Network.AWS.Comprehend.Types.SentimentDetectionJobFilter
import Network.AWS.Comprehend.Types.SentimentDetectionJobProperties
import Network.AWS.Comprehend.Types.SentimentScore
import Network.AWS.Comprehend.Types.SyntaxToken
import Network.AWS.Comprehend.Types.Tag
import Network.AWS.Comprehend.Types.TopicsDetectionJobFilter
import Network.AWS.Comprehend.Types.TopicsDetectionJobProperties
import Network.AWS.Comprehend.Types.VpcConfig
import Network.AWS.Comprehend.UntagResource
import Network.AWS.Comprehend.UpdateEndpoint

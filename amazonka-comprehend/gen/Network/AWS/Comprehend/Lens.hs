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

    -- ** StopSentimentDetectionJob
    stopSentimentDetectionJob_jobId,
    stopSentimentDetectionJobResponse_jobStatus,
    stopSentimentDetectionJobResponse_jobId,
    stopSentimentDetectionJobResponse_httpStatus,

    -- ** StartEventsDetectionJob
    startEventsDetectionJob_clientRequestToken,
    startEventsDetectionJob_jobName,
    startEventsDetectionJob_inputDataConfig,
    startEventsDetectionJob_outputDataConfig,
    startEventsDetectionJob_dataAccessRoleArn,
    startEventsDetectionJob_languageCode,
    startEventsDetectionJob_targetEventTypes,
    startEventsDetectionJobResponse_jobStatus,
    startEventsDetectionJobResponse_jobId,
    startEventsDetectionJobResponse_httpStatus,

    -- ** CreateEndpoint
    createEndpoint_tags,
    createEndpoint_clientRequestToken,
    createEndpoint_endpointName,
    createEndpoint_modelArn,
    createEndpoint_desiredInferenceUnits,
    createEndpointResponse_endpointArn,
    createEndpointResponse_httpStatus,

    -- ** StartSentimentDetectionJob
    startSentimentDetectionJob_vpcConfig,
    startSentimentDetectionJob_volumeKmsKeyId,
    startSentimentDetectionJob_clientRequestToken,
    startSentimentDetectionJob_jobName,
    startSentimentDetectionJob_inputDataConfig,
    startSentimentDetectionJob_outputDataConfig,
    startSentimentDetectionJob_dataAccessRoleArn,
    startSentimentDetectionJob_languageCode,
    startSentimentDetectionJobResponse_jobStatus,
    startSentimentDetectionJobResponse_jobId,
    startSentimentDetectionJobResponse_httpStatus,

    -- ** StopEventsDetectionJob
    stopEventsDetectionJob_jobId,
    stopEventsDetectionJobResponse_jobStatus,
    stopEventsDetectionJobResponse_jobId,
    stopEventsDetectionJobResponse_httpStatus,

    -- ** ListEntityRecognizers
    listEntityRecognizers_nextToken,
    listEntityRecognizers_maxResults,
    listEntityRecognizers_filter,
    listEntityRecognizersResponse_nextToken,
    listEntityRecognizersResponse_entityRecognizerPropertiesList,
    listEntityRecognizersResponse_httpStatus,

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

    -- ** ListDominantLanguageDetectionJobs
    listDominantLanguageDetectionJobs_nextToken,
    listDominantLanguageDetectionJobs_maxResults,
    listDominantLanguageDetectionJobs_filter,
    listDominantLanguageDetectionJobsResponse_nextToken,
    listDominantLanguageDetectionJobsResponse_dominantLanguageDetectionJobPropertiesList,
    listDominantLanguageDetectionJobsResponse_httpStatus,

    -- ** StopKeyPhrasesDetectionJob
    stopKeyPhrasesDetectionJob_jobId,
    stopKeyPhrasesDetectionJobResponse_jobStatus,
    stopKeyPhrasesDetectionJobResponse_jobId,
    stopKeyPhrasesDetectionJobResponse_httpStatus,

    -- ** ListDocumentClassifiers
    listDocumentClassifiers_nextToken,
    listDocumentClassifiers_maxResults,
    listDocumentClassifiers_filter,
    listDocumentClassifiersResponse_nextToken,
    listDocumentClassifiersResponse_documentClassifierPropertiesList,
    listDocumentClassifiersResponse_httpStatus,

    -- ** CreateEntityRecognizer
    createEntityRecognizer_vpcConfig,
    createEntityRecognizer_volumeKmsKeyId,
    createEntityRecognizer_tags,
    createEntityRecognizer_clientRequestToken,
    createEntityRecognizer_recognizerName,
    createEntityRecognizer_dataAccessRoleArn,
    createEntityRecognizer_inputDataConfig,
    createEntityRecognizer_languageCode,
    createEntityRecognizerResponse_entityRecognizerArn,
    createEntityRecognizerResponse_httpStatus,

    -- ** StartKeyPhrasesDetectionJob
    startKeyPhrasesDetectionJob_vpcConfig,
    startKeyPhrasesDetectionJob_volumeKmsKeyId,
    startKeyPhrasesDetectionJob_clientRequestToken,
    startKeyPhrasesDetectionJob_jobName,
    startKeyPhrasesDetectionJob_inputDataConfig,
    startKeyPhrasesDetectionJob_outputDataConfig,
    startKeyPhrasesDetectionJob_dataAccessRoleArn,
    startKeyPhrasesDetectionJob_languageCode,
    startKeyPhrasesDetectionJobResponse_jobStatus,
    startKeyPhrasesDetectionJobResponse_jobId,
    startKeyPhrasesDetectionJobResponse_httpStatus,

    -- ** ListEventsDetectionJobs
    listEventsDetectionJobs_nextToken,
    listEventsDetectionJobs_maxResults,
    listEventsDetectionJobs_filter,
    listEventsDetectionJobsResponse_nextToken,
    listEventsDetectionJobsResponse_eventsDetectionJobPropertiesList,
    listEventsDetectionJobsResponse_httpStatus,

    -- ** ListSentimentDetectionJobs
    listSentimentDetectionJobs_nextToken,
    listSentimentDetectionJobs_maxResults,
    listSentimentDetectionJobs_filter,
    listSentimentDetectionJobsResponse_sentimentDetectionJobPropertiesList,
    listSentimentDetectionJobsResponse_nextToken,
    listSentimentDetectionJobsResponse_httpStatus,

    -- ** DetectSyntax
    detectSyntax_text,
    detectSyntax_languageCode,
    detectSyntaxResponse_syntaxTokens,
    detectSyntaxResponse_httpStatus,

    -- ** StartDocumentClassificationJob
    startDocumentClassificationJob_vpcConfig,
    startDocumentClassificationJob_volumeKmsKeyId,
    startDocumentClassificationJob_clientRequestToken,
    startDocumentClassificationJob_jobName,
    startDocumentClassificationJob_documentClassifierArn,
    startDocumentClassificationJob_inputDataConfig,
    startDocumentClassificationJob_outputDataConfig,
    startDocumentClassificationJob_dataAccessRoleArn,
    startDocumentClassificationJobResponse_jobStatus,
    startDocumentClassificationJobResponse_jobId,
    startDocumentClassificationJobResponse_httpStatus,

    -- ** DetectKeyPhrases
    detectKeyPhrases_text,
    detectKeyPhrases_languageCode,
    detectKeyPhrasesResponse_keyPhrases,
    detectKeyPhrasesResponse_httpStatus,

    -- ** DescribeEndpoint
    describeEndpoint_endpointArn,
    describeEndpointResponse_endpointProperties,
    describeEndpointResponse_httpStatus,

    -- ** BatchDetectEntities
    batchDetectEntities_textList,
    batchDetectEntities_languageCode,
    batchDetectEntitiesResponse_httpStatus,
    batchDetectEntitiesResponse_resultList,
    batchDetectEntitiesResponse_errorList,

    -- ** ListTopicsDetectionJobs
    listTopicsDetectionJobs_nextToken,
    listTopicsDetectionJobs_maxResults,
    listTopicsDetectionJobs_filter,
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

    -- ** StopTrainingDocumentClassifier
    stopTrainingDocumentClassifier_documentClassifierArn,
    stopTrainingDocumentClassifierResponse_httpStatus,

    -- ** DescribeEntityRecognizer
    describeEntityRecognizer_entityRecognizerArn,
    describeEntityRecognizerResponse_entityRecognizerProperties,
    describeEntityRecognizerResponse_httpStatus,

    -- ** DescribePiiEntitiesDetectionJob
    describePiiEntitiesDetectionJob_jobId,
    describePiiEntitiesDetectionJobResponse_piiEntitiesDetectionJobProperties,
    describePiiEntitiesDetectionJobResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListKeyPhrasesDetectionJobs
    listKeyPhrasesDetectionJobs_nextToken,
    listKeyPhrasesDetectionJobs_maxResults,
    listKeyPhrasesDetectionJobs_filter,
    listKeyPhrasesDetectionJobsResponse_nextToken,
    listKeyPhrasesDetectionJobsResponse_keyPhrasesDetectionJobPropertiesList,
    listKeyPhrasesDetectionJobsResponse_httpStatus,

    -- ** DescribeDominantLanguageDetectionJob
    describeDominantLanguageDetectionJob_jobId,
    describeDominantLanguageDetectionJobResponse_dominantLanguageDetectionJobProperties,
    describeDominantLanguageDetectionJobResponse_httpStatus,

    -- ** StopEntitiesDetectionJob
    stopEntitiesDetectionJob_jobId,
    stopEntitiesDetectionJobResponse_jobStatus,
    stopEntitiesDetectionJobResponse_jobId,
    stopEntitiesDetectionJobResponse_httpStatus,

    -- ** StopPiiEntitiesDetectionJob
    stopPiiEntitiesDetectionJob_jobId,
    stopPiiEntitiesDetectionJobResponse_jobStatus,
    stopPiiEntitiesDetectionJobResponse_jobId,
    stopPiiEntitiesDetectionJobResponse_httpStatus,

    -- ** DescribeDocumentClassifier
    describeDocumentClassifier_documentClassifierArn,
    describeDocumentClassifierResponse_documentClassifierProperties,
    describeDocumentClassifierResponse_httpStatus,

    -- ** StopTrainingEntityRecognizer
    stopTrainingEntityRecognizer_entityRecognizerArn,
    stopTrainingEntityRecognizerResponse_httpStatus,

    -- ** StartEntitiesDetectionJob
    startEntitiesDetectionJob_vpcConfig,
    startEntitiesDetectionJob_volumeKmsKeyId,
    startEntitiesDetectionJob_clientRequestToken,
    startEntitiesDetectionJob_entityRecognizerArn,
    startEntitiesDetectionJob_jobName,
    startEntitiesDetectionJob_inputDataConfig,
    startEntitiesDetectionJob_outputDataConfig,
    startEntitiesDetectionJob_dataAccessRoleArn,
    startEntitiesDetectionJob_languageCode,
    startEntitiesDetectionJobResponse_jobStatus,
    startEntitiesDetectionJobResponse_jobId,
    startEntitiesDetectionJobResponse_httpStatus,

    -- ** StartPiiEntitiesDetectionJob
    startPiiEntitiesDetectionJob_redactionConfig,
    startPiiEntitiesDetectionJob_clientRequestToken,
    startPiiEntitiesDetectionJob_jobName,
    startPiiEntitiesDetectionJob_inputDataConfig,
    startPiiEntitiesDetectionJob_outputDataConfig,
    startPiiEntitiesDetectionJob_mode,
    startPiiEntitiesDetectionJob_dataAccessRoleArn,
    startPiiEntitiesDetectionJob_languageCode,
    startPiiEntitiesDetectionJobResponse_jobStatus,
    startPiiEntitiesDetectionJobResponse_jobId,
    startPiiEntitiesDetectionJobResponse_httpStatus,

    -- ** ListEndpoints
    listEndpoints_nextToken,
    listEndpoints_maxResults,
    listEndpoints_filter,
    listEndpointsResponse_nextToken,
    listEndpointsResponse_endpointPropertiesList,
    listEndpointsResponse_httpStatus,

    -- ** DeleteEndpoint
    deleteEndpoint_endpointArn,
    deleteEndpointResponse_httpStatus,

    -- ** UpdateEndpoint
    updateEndpoint_endpointArn,
    updateEndpoint_desiredInferenceUnits,
    updateEndpointResponse_httpStatus,

    -- ** DescribeEventsDetectionJob
    describeEventsDetectionJob_jobId,
    describeEventsDetectionJobResponse_eventsDetectionJobProperties,
    describeEventsDetectionJobResponse_httpStatus,

    -- ** DetectPiiEntities
    detectPiiEntities_text,
    detectPiiEntities_languageCode,
    detectPiiEntitiesResponse_entities,
    detectPiiEntitiesResponse_httpStatus,

    -- ** ClassifyDocument
    classifyDocument_text,
    classifyDocument_endpointArn,
    classifyDocumentResponse_classes,
    classifyDocumentResponse_labels,
    classifyDocumentResponse_httpStatus,

    -- ** DetectDominantLanguage
    detectDominantLanguage_text,
    detectDominantLanguageResponse_languages,
    detectDominantLanguageResponse_httpStatus,

    -- ** ListDocumentClassificationJobs
    listDocumentClassificationJobs_nextToken,
    listDocumentClassificationJobs_maxResults,
    listDocumentClassificationJobs_filter,
    listDocumentClassificationJobsResponse_nextToken,
    listDocumentClassificationJobsResponse_documentClassificationJobPropertiesList,
    listDocumentClassificationJobsResponse_httpStatus,

    -- ** DescribeTopicsDetectionJob
    describeTopicsDetectionJob_jobId,
    describeTopicsDetectionJobResponse_topicsDetectionJobProperties,
    describeTopicsDetectionJobResponse_httpStatus,

    -- ** ListEntitiesDetectionJobs
    listEntitiesDetectionJobs_nextToken,
    listEntitiesDetectionJobs_maxResults,
    listEntitiesDetectionJobs_filter,
    listEntitiesDetectionJobsResponse_nextToken,
    listEntitiesDetectionJobsResponse_entitiesDetectionJobPropertiesList,
    listEntitiesDetectionJobsResponse_httpStatus,

    -- ** CreateDocumentClassifier
    createDocumentClassifier_vpcConfig,
    createDocumentClassifier_mode,
    createDocumentClassifier_outputDataConfig,
    createDocumentClassifier_volumeKmsKeyId,
    createDocumentClassifier_tags,
    createDocumentClassifier_clientRequestToken,
    createDocumentClassifier_documentClassifierName,
    createDocumentClassifier_dataAccessRoleArn,
    createDocumentClassifier_inputDataConfig,
    createDocumentClassifier_languageCode,
    createDocumentClassifierResponse_documentClassifierArn,
    createDocumentClassifierResponse_httpStatus,

    -- ** ListPiiEntitiesDetectionJobs
    listPiiEntitiesDetectionJobs_nextToken,
    listPiiEntitiesDetectionJobs_maxResults,
    listPiiEntitiesDetectionJobs_filter,
    listPiiEntitiesDetectionJobsResponse_nextToken,
    listPiiEntitiesDetectionJobsResponse_piiEntitiesDetectionJobPropertiesList,
    listPiiEntitiesDetectionJobsResponse_httpStatus,

    -- ** DeleteDocumentClassifier
    deleteDocumentClassifier_documentClassifierArn,
    deleteDocumentClassifierResponse_httpStatus,

    -- ** DescribeDocumentClassificationJob
    describeDocumentClassificationJob_jobId,
    describeDocumentClassificationJobResponse_documentClassificationJobProperties,
    describeDocumentClassificationJobResponse_httpStatus,

    -- ** StopDominantLanguageDetectionJob
    stopDominantLanguageDetectionJob_jobId,
    stopDominantLanguageDetectionJobResponse_jobStatus,
    stopDominantLanguageDetectionJobResponse_jobId,
    stopDominantLanguageDetectionJobResponse_httpStatus,

    -- ** DescribeEntitiesDetectionJob
    describeEntitiesDetectionJob_jobId,
    describeEntitiesDetectionJobResponse_entitiesDetectionJobProperties,
    describeEntitiesDetectionJobResponse_httpStatus,

    -- ** StartDominantLanguageDetectionJob
    startDominantLanguageDetectionJob_vpcConfig,
    startDominantLanguageDetectionJob_volumeKmsKeyId,
    startDominantLanguageDetectionJob_clientRequestToken,
    startDominantLanguageDetectionJob_jobName,
    startDominantLanguageDetectionJob_inputDataConfig,
    startDominantLanguageDetectionJob_outputDataConfig,
    startDominantLanguageDetectionJob_dataAccessRoleArn,
    startDominantLanguageDetectionJobResponse_jobStatus,
    startDominantLanguageDetectionJobResponse_jobId,
    startDominantLanguageDetectionJobResponse_httpStatus,

    -- ** DetectSentiment
    detectSentiment_text,
    detectSentiment_languageCode,
    detectSentimentResponse_sentimentScore,
    detectSentimentResponse_sentiment,
    detectSentimentResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DetectEntities
    detectEntities_languageCode,
    detectEntities_endpointArn,
    detectEntities_text,
    detectEntitiesResponse_entities,
    detectEntitiesResponse_httpStatus,

    -- ** DescribeSentimentDetectionJob
    describeSentimentDetectionJob_jobId,
    describeSentimentDetectionJobResponse_sentimentDetectionJobProperties,
    describeSentimentDetectionJobResponse_httpStatus,

    -- ** BatchDetectSyntax
    batchDetectSyntax_textList,
    batchDetectSyntax_languageCode,
    batchDetectSyntaxResponse_httpStatus,
    batchDetectSyntaxResponse_resultList,
    batchDetectSyntaxResponse_errorList,

    -- ** BatchDetectKeyPhrases
    batchDetectKeyPhrases_textList,
    batchDetectKeyPhrases_languageCode,
    batchDetectKeyPhrasesResponse_httpStatus,
    batchDetectKeyPhrasesResponse_resultList,
    batchDetectKeyPhrasesResponse_errorList,

    -- ** StartTopicsDetectionJob
    startTopicsDetectionJob_vpcConfig,
    startTopicsDetectionJob_volumeKmsKeyId,
    startTopicsDetectionJob_clientRequestToken,
    startTopicsDetectionJob_numberOfTopics,
    startTopicsDetectionJob_jobName,
    startTopicsDetectionJob_inputDataConfig,
    startTopicsDetectionJob_outputDataConfig,
    startTopicsDetectionJob_dataAccessRoleArn,
    startTopicsDetectionJobResponse_jobStatus,
    startTopicsDetectionJobResponse_jobId,
    startTopicsDetectionJobResponse_httpStatus,

    -- * Types

    -- ** AugmentedManifestsListItem
    augmentedManifestsListItem_s3Uri,
    augmentedManifestsListItem_attributeNames,

    -- ** BatchDetectDominantLanguageItemResult
    batchDetectDominantLanguageItemResult_languages,
    batchDetectDominantLanguageItemResult_index,

    -- ** BatchDetectEntitiesItemResult
    batchDetectEntitiesItemResult_index,
    batchDetectEntitiesItemResult_entities,

    -- ** BatchDetectKeyPhrasesItemResult
    batchDetectKeyPhrasesItemResult_keyPhrases,
    batchDetectKeyPhrasesItemResult_index,

    -- ** BatchDetectSentimentItemResult
    batchDetectSentimentItemResult_sentimentScore,
    batchDetectSentimentItemResult_sentiment,
    batchDetectSentimentItemResult_index,

    -- ** BatchDetectSyntaxItemResult
    batchDetectSyntaxItemResult_syntaxTokens,
    batchDetectSyntaxItemResult_index,

    -- ** BatchItemError
    batchItemError_index,
    batchItemError_errorMessage,
    batchItemError_errorCode,

    -- ** ClassifierEvaluationMetrics
    classifierEvaluationMetrics_microRecall,
    classifierEvaluationMetrics_f1Score,
    classifierEvaluationMetrics_microPrecision,
    classifierEvaluationMetrics_precision,
    classifierEvaluationMetrics_accuracy,
    classifierEvaluationMetrics_hammingLoss,
    classifierEvaluationMetrics_recall,
    classifierEvaluationMetrics_microF1Score,

    -- ** ClassifierMetadata
    classifierMetadata_numberOfLabels,
    classifierMetadata_numberOfTestDocuments,
    classifierMetadata_numberOfTrainedDocuments,
    classifierMetadata_evaluationMetrics,

    -- ** DocumentClass
    documentClass_name,
    documentClass_score,

    -- ** DocumentClassificationJobFilter
    documentClassificationJobFilter_jobStatus,
    documentClassificationJobFilter_submitTimeBefore,
    documentClassificationJobFilter_submitTimeAfter,
    documentClassificationJobFilter_jobName,

    -- ** DocumentClassificationJobProperties
    documentClassificationJobProperties_vpcConfig,
    documentClassificationJobProperties_inputDataConfig,
    documentClassificationJobProperties_message,
    documentClassificationJobProperties_jobStatus,
    documentClassificationJobProperties_outputDataConfig,
    documentClassificationJobProperties_documentClassifierArn,
    documentClassificationJobProperties_endTime,
    documentClassificationJobProperties_volumeKmsKeyId,
    documentClassificationJobProperties_submitTime,
    documentClassificationJobProperties_jobName,
    documentClassificationJobProperties_dataAccessRoleArn,
    documentClassificationJobProperties_jobId,

    -- ** DocumentClassifierFilter
    documentClassifierFilter_status,
    documentClassifierFilter_submitTimeBefore,
    documentClassifierFilter_submitTimeAfter,

    -- ** DocumentClassifierInputDataConfig
    documentClassifierInputDataConfig_augmentedManifests,
    documentClassifierInputDataConfig_dataFormat,
    documentClassifierInputDataConfig_labelDelimiter,
    documentClassifierInputDataConfig_s3Uri,

    -- ** DocumentClassifierOutputDataConfig
    documentClassifierOutputDataConfig_kmsKeyId,
    documentClassifierOutputDataConfig_s3Uri,

    -- ** DocumentClassifierProperties
    documentClassifierProperties_vpcConfig,
    documentClassifierProperties_languageCode,
    documentClassifierProperties_status,
    documentClassifierProperties_inputDataConfig,
    documentClassifierProperties_mode,
    documentClassifierProperties_message,
    documentClassifierProperties_outputDataConfig,
    documentClassifierProperties_documentClassifierArn,
    documentClassifierProperties_endTime,
    documentClassifierProperties_classifierMetadata,
    documentClassifierProperties_volumeKmsKeyId,
    documentClassifierProperties_submitTime,
    documentClassifierProperties_trainingStartTime,
    documentClassifierProperties_dataAccessRoleArn,
    documentClassifierProperties_trainingEndTime,

    -- ** DocumentLabel
    documentLabel_name,
    documentLabel_score,

    -- ** DominantLanguage
    dominantLanguage_languageCode,
    dominantLanguage_score,

    -- ** DominantLanguageDetectionJobFilter
    dominantLanguageDetectionJobFilter_jobStatus,
    dominantLanguageDetectionJobFilter_submitTimeBefore,
    dominantLanguageDetectionJobFilter_submitTimeAfter,
    dominantLanguageDetectionJobFilter_jobName,

    -- ** DominantLanguageDetectionJobProperties
    dominantLanguageDetectionJobProperties_vpcConfig,
    dominantLanguageDetectionJobProperties_inputDataConfig,
    dominantLanguageDetectionJobProperties_message,
    dominantLanguageDetectionJobProperties_jobStatus,
    dominantLanguageDetectionJobProperties_outputDataConfig,
    dominantLanguageDetectionJobProperties_endTime,
    dominantLanguageDetectionJobProperties_volumeKmsKeyId,
    dominantLanguageDetectionJobProperties_submitTime,
    dominantLanguageDetectionJobProperties_jobName,
    dominantLanguageDetectionJobProperties_dataAccessRoleArn,
    dominantLanguageDetectionJobProperties_jobId,

    -- ** EndpointFilter
    endpointFilter_status,
    endpointFilter_creationTimeBefore,
    endpointFilter_modelArn,
    endpointFilter_creationTimeAfter,

    -- ** EndpointProperties
    endpointProperties_currentInferenceUnits,
    endpointProperties_status,
    endpointProperties_creationTime,
    endpointProperties_desiredInferenceUnits,
    endpointProperties_message,
    endpointProperties_modelArn,
    endpointProperties_lastModifiedTime,
    endpointProperties_endpointArn,

    -- ** EntitiesDetectionJobFilter
    entitiesDetectionJobFilter_jobStatus,
    entitiesDetectionJobFilter_submitTimeBefore,
    entitiesDetectionJobFilter_submitTimeAfter,
    entitiesDetectionJobFilter_jobName,

    -- ** EntitiesDetectionJobProperties
    entitiesDetectionJobProperties_vpcConfig,
    entitiesDetectionJobProperties_languageCode,
    entitiesDetectionJobProperties_inputDataConfig,
    entitiesDetectionJobProperties_message,
    entitiesDetectionJobProperties_jobStatus,
    entitiesDetectionJobProperties_outputDataConfig,
    entitiesDetectionJobProperties_endTime,
    entitiesDetectionJobProperties_volumeKmsKeyId,
    entitiesDetectionJobProperties_submitTime,
    entitiesDetectionJobProperties_entityRecognizerArn,
    entitiesDetectionJobProperties_jobName,
    entitiesDetectionJobProperties_dataAccessRoleArn,
    entitiesDetectionJobProperties_jobId,

    -- ** Entity
    entity_endOffset,
    entity_type,
    entity_score,
    entity_text,
    entity_beginOffset,

    -- ** EntityRecognizerAnnotations
    entityRecognizerAnnotations_s3Uri,

    -- ** EntityRecognizerDocuments
    entityRecognizerDocuments_s3Uri,

    -- ** EntityRecognizerEntityList
    entityRecognizerEntityList_s3Uri,

    -- ** EntityRecognizerEvaluationMetrics
    entityRecognizerEvaluationMetrics_f1Score,
    entityRecognizerEvaluationMetrics_precision,
    entityRecognizerEvaluationMetrics_recall,

    -- ** EntityRecognizerFilter
    entityRecognizerFilter_status,
    entityRecognizerFilter_submitTimeBefore,
    entityRecognizerFilter_submitTimeAfter,

    -- ** EntityRecognizerInputDataConfig
    entityRecognizerInputDataConfig_documents,
    entityRecognizerInputDataConfig_augmentedManifests,
    entityRecognizerInputDataConfig_dataFormat,
    entityRecognizerInputDataConfig_annotations,
    entityRecognizerInputDataConfig_entityList,
    entityRecognizerInputDataConfig_entityTypes,

    -- ** EntityRecognizerMetadata
    entityRecognizerMetadata_numberOfTestDocuments,
    entityRecognizerMetadata_numberOfTrainedDocuments,
    entityRecognizerMetadata_evaluationMetrics,
    entityRecognizerMetadata_entityTypes,

    -- ** EntityRecognizerMetadataEntityTypesListItem
    entityRecognizerMetadataEntityTypesListItem_numberOfTrainMentions,
    entityRecognizerMetadataEntityTypesListItem_evaluationMetrics,
    entityRecognizerMetadataEntityTypesListItem_type,

    -- ** EntityRecognizerProperties
    entityRecognizerProperties_vpcConfig,
    entityRecognizerProperties_languageCode,
    entityRecognizerProperties_status,
    entityRecognizerProperties_inputDataConfig,
    entityRecognizerProperties_message,
    entityRecognizerProperties_endTime,
    entityRecognizerProperties_volumeKmsKeyId,
    entityRecognizerProperties_submitTime,
    entityRecognizerProperties_recognizerMetadata,
    entityRecognizerProperties_entityRecognizerArn,
    entityRecognizerProperties_trainingStartTime,
    entityRecognizerProperties_dataAccessRoleArn,
    entityRecognizerProperties_trainingEndTime,

    -- ** EntityTypesEvaluationMetrics
    entityTypesEvaluationMetrics_f1Score,
    entityTypesEvaluationMetrics_precision,
    entityTypesEvaluationMetrics_recall,

    -- ** EntityTypesListItem
    entityTypesListItem_type,

    -- ** EventsDetectionJobFilter
    eventsDetectionJobFilter_jobStatus,
    eventsDetectionJobFilter_submitTimeBefore,
    eventsDetectionJobFilter_submitTimeAfter,
    eventsDetectionJobFilter_jobName,

    -- ** EventsDetectionJobProperties
    eventsDetectionJobProperties_languageCode,
    eventsDetectionJobProperties_inputDataConfig,
    eventsDetectionJobProperties_message,
    eventsDetectionJobProperties_jobStatus,
    eventsDetectionJobProperties_outputDataConfig,
    eventsDetectionJobProperties_endTime,
    eventsDetectionJobProperties_submitTime,
    eventsDetectionJobProperties_targetEventTypes,
    eventsDetectionJobProperties_jobName,
    eventsDetectionJobProperties_dataAccessRoleArn,
    eventsDetectionJobProperties_jobId,

    -- ** InputDataConfig
    inputDataConfig_inputFormat,
    inputDataConfig_s3Uri,

    -- ** KeyPhrase
    keyPhrase_endOffset,
    keyPhrase_score,
    keyPhrase_text,
    keyPhrase_beginOffset,

    -- ** KeyPhrasesDetectionJobFilter
    keyPhrasesDetectionJobFilter_jobStatus,
    keyPhrasesDetectionJobFilter_submitTimeBefore,
    keyPhrasesDetectionJobFilter_submitTimeAfter,
    keyPhrasesDetectionJobFilter_jobName,

    -- ** KeyPhrasesDetectionJobProperties
    keyPhrasesDetectionJobProperties_vpcConfig,
    keyPhrasesDetectionJobProperties_languageCode,
    keyPhrasesDetectionJobProperties_inputDataConfig,
    keyPhrasesDetectionJobProperties_message,
    keyPhrasesDetectionJobProperties_jobStatus,
    keyPhrasesDetectionJobProperties_outputDataConfig,
    keyPhrasesDetectionJobProperties_endTime,
    keyPhrasesDetectionJobProperties_volumeKmsKeyId,
    keyPhrasesDetectionJobProperties_submitTime,
    keyPhrasesDetectionJobProperties_jobName,
    keyPhrasesDetectionJobProperties_dataAccessRoleArn,
    keyPhrasesDetectionJobProperties_jobId,

    -- ** OutputDataConfig
    outputDataConfig_kmsKeyId,
    outputDataConfig_s3Uri,

    -- ** PartOfSpeechTag
    partOfSpeechTag_score,
    partOfSpeechTag_tag,

    -- ** PiiEntitiesDetectionJobFilter
    piiEntitiesDetectionJobFilter_jobStatus,
    piiEntitiesDetectionJobFilter_submitTimeBefore,
    piiEntitiesDetectionJobFilter_submitTimeAfter,
    piiEntitiesDetectionJobFilter_jobName,

    -- ** PiiEntitiesDetectionJobProperties
    piiEntitiesDetectionJobProperties_redactionConfig,
    piiEntitiesDetectionJobProperties_languageCode,
    piiEntitiesDetectionJobProperties_inputDataConfig,
    piiEntitiesDetectionJobProperties_mode,
    piiEntitiesDetectionJobProperties_message,
    piiEntitiesDetectionJobProperties_jobStatus,
    piiEntitiesDetectionJobProperties_outputDataConfig,
    piiEntitiesDetectionJobProperties_endTime,
    piiEntitiesDetectionJobProperties_submitTime,
    piiEntitiesDetectionJobProperties_jobName,
    piiEntitiesDetectionJobProperties_dataAccessRoleArn,
    piiEntitiesDetectionJobProperties_jobId,

    -- ** PiiEntity
    piiEntity_endOffset,
    piiEntity_type,
    piiEntity_score,
    piiEntity_beginOffset,

    -- ** PiiOutputDataConfig
    piiOutputDataConfig_kmsKeyId,
    piiOutputDataConfig_s3Uri,

    -- ** RedactionConfig
    redactionConfig_maskCharacter,
    redactionConfig_maskMode,
    redactionConfig_piiEntityTypes,

    -- ** SentimentDetectionJobFilter
    sentimentDetectionJobFilter_jobStatus,
    sentimentDetectionJobFilter_submitTimeBefore,
    sentimentDetectionJobFilter_submitTimeAfter,
    sentimentDetectionJobFilter_jobName,

    -- ** SentimentDetectionJobProperties
    sentimentDetectionJobProperties_vpcConfig,
    sentimentDetectionJobProperties_languageCode,
    sentimentDetectionJobProperties_inputDataConfig,
    sentimentDetectionJobProperties_message,
    sentimentDetectionJobProperties_jobStatus,
    sentimentDetectionJobProperties_outputDataConfig,
    sentimentDetectionJobProperties_endTime,
    sentimentDetectionJobProperties_volumeKmsKeyId,
    sentimentDetectionJobProperties_submitTime,
    sentimentDetectionJobProperties_jobName,
    sentimentDetectionJobProperties_dataAccessRoleArn,
    sentimentDetectionJobProperties_jobId,

    -- ** SentimentScore
    sentimentScore_negative,
    sentimentScore_mixed,
    sentimentScore_positive,
    sentimentScore_neutral,

    -- ** SyntaxToken
    syntaxToken_tokenId,
    syntaxToken_partOfSpeech,
    syntaxToken_endOffset,
    syntaxToken_text,
    syntaxToken_beginOffset,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TopicsDetectionJobFilter
    topicsDetectionJobFilter_jobStatus,
    topicsDetectionJobFilter_submitTimeBefore,
    topicsDetectionJobFilter_submitTimeAfter,
    topicsDetectionJobFilter_jobName,

    -- ** TopicsDetectionJobProperties
    topicsDetectionJobProperties_vpcConfig,
    topicsDetectionJobProperties_inputDataConfig,
    topicsDetectionJobProperties_message,
    topicsDetectionJobProperties_jobStatus,
    topicsDetectionJobProperties_outputDataConfig,
    topicsDetectionJobProperties_endTime,
    topicsDetectionJobProperties_volumeKmsKeyId,
    topicsDetectionJobProperties_submitTime,
    topicsDetectionJobProperties_numberOfTopics,
    topicsDetectionJobProperties_jobName,
    topicsDetectionJobProperties_dataAccessRoleArn,
    topicsDetectionJobProperties_jobId,

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
import Network.AWS.Comprehend.ListDocumentClassifiers
import Network.AWS.Comprehend.ListDominantLanguageDetectionJobs
import Network.AWS.Comprehend.ListEndpoints
import Network.AWS.Comprehend.ListEntitiesDetectionJobs
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
import Network.AWS.Comprehend.Types.DocumentLabel
import Network.AWS.Comprehend.Types.DominantLanguage
import Network.AWS.Comprehend.Types.DominantLanguageDetectionJobFilter
import Network.AWS.Comprehend.Types.DominantLanguageDetectionJobProperties
import Network.AWS.Comprehend.Types.EndpointFilter
import Network.AWS.Comprehend.Types.EndpointProperties
import Network.AWS.Comprehend.Types.EntitiesDetectionJobFilter
import Network.AWS.Comprehend.Types.EntitiesDetectionJobProperties
import Network.AWS.Comprehend.Types.Entity
import Network.AWS.Comprehend.Types.EntityRecognizerAnnotations
import Network.AWS.Comprehend.Types.EntityRecognizerDocuments
import Network.AWS.Comprehend.Types.EntityRecognizerEntityList
import Network.AWS.Comprehend.Types.EntityRecognizerEvaluationMetrics
import Network.AWS.Comprehend.Types.EntityRecognizerFilter
import Network.AWS.Comprehend.Types.EntityRecognizerInputDataConfig
import Network.AWS.Comprehend.Types.EntityRecognizerMetadata
import Network.AWS.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem
import Network.AWS.Comprehend.Types.EntityRecognizerProperties
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

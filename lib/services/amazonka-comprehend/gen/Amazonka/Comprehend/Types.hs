{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Comprehend.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceUnavailableException,
    _ConcurrentModificationException,
    _InternalServerException,
    _TooManyTagsException,
    _TooManyTagKeysException,
    _UnsupportedLanguageException,
    _ResourceNotFoundException,
    _ResourceInUseException,
    _InvalidFilterException,
    _ResourceLimitExceededException,
    _TextSizeLimitExceededException,
    _BatchSizeLimitExceededException,
    _KmsKeyValidationException,
    _TooManyRequestsException,
    _InvalidRequestException,
    _JobNotFoundException,

    -- * AugmentedManifestsDocumentTypeFormat
    AugmentedManifestsDocumentTypeFormat (..),

    -- * DocumentClassifierDataFormat
    DocumentClassifierDataFormat (..),

    -- * DocumentClassifierMode
    DocumentClassifierMode (..),

    -- * DocumentReadAction
    DocumentReadAction (..),

    -- * DocumentReadFeatureTypes
    DocumentReadFeatureTypes (..),

    -- * DocumentReadMode
    DocumentReadMode (..),

    -- * EndpointStatus
    EndpointStatus (..),

    -- * EntityRecognizerDataFormat
    EntityRecognizerDataFormat (..),

    -- * EntityType
    EntityType (..),

    -- * InputFormat
    InputFormat (..),

    -- * JobStatus
    JobStatus (..),

    -- * LanguageCode
    LanguageCode (..),

    -- * ModelStatus
    ModelStatus (..),

    -- * PartOfSpeechTagType
    PartOfSpeechTagType (..),

    -- * PiiEntitiesDetectionMaskMode
    PiiEntitiesDetectionMaskMode (..),

    -- * PiiEntitiesDetectionMode
    PiiEntitiesDetectionMode (..),

    -- * PiiEntityType
    PiiEntityType (..),

    -- * SentimentType
    SentimentType (..),

    -- * Split
    Split (..),

    -- * SyntaxLanguageCode
    SyntaxLanguageCode (..),

    -- * TargetedSentimentEntityType
    TargetedSentimentEntityType (..),

    -- * AugmentedManifestsListItem
    AugmentedManifestsListItem (..),
    newAugmentedManifestsListItem,
    augmentedManifestsListItem_documentType,
    augmentedManifestsListItem_annotationDataS3Uri,
    augmentedManifestsListItem_sourceDocumentsS3Uri,
    augmentedManifestsListItem_split,
    augmentedManifestsListItem_s3Uri,
    augmentedManifestsListItem_attributeNames,

    -- * BatchDetectDominantLanguageItemResult
    BatchDetectDominantLanguageItemResult (..),
    newBatchDetectDominantLanguageItemResult,
    batchDetectDominantLanguageItemResult_index,
    batchDetectDominantLanguageItemResult_languages,

    -- * BatchDetectEntitiesItemResult
    BatchDetectEntitiesItemResult (..),
    newBatchDetectEntitiesItemResult,
    batchDetectEntitiesItemResult_entities,
    batchDetectEntitiesItemResult_index,

    -- * BatchDetectKeyPhrasesItemResult
    BatchDetectKeyPhrasesItemResult (..),
    newBatchDetectKeyPhrasesItemResult,
    batchDetectKeyPhrasesItemResult_index,
    batchDetectKeyPhrasesItemResult_keyPhrases,

    -- * BatchDetectSentimentItemResult
    BatchDetectSentimentItemResult (..),
    newBatchDetectSentimentItemResult,
    batchDetectSentimentItemResult_index,
    batchDetectSentimentItemResult_sentimentScore,
    batchDetectSentimentItemResult_sentiment,

    -- * BatchDetectSyntaxItemResult
    BatchDetectSyntaxItemResult (..),
    newBatchDetectSyntaxItemResult,
    batchDetectSyntaxItemResult_index,
    batchDetectSyntaxItemResult_syntaxTokens,

    -- * BatchDetectTargetedSentimentItemResult
    BatchDetectTargetedSentimentItemResult (..),
    newBatchDetectTargetedSentimentItemResult,
    batchDetectTargetedSentimentItemResult_entities,
    batchDetectTargetedSentimentItemResult_index,

    -- * BatchItemError
    BatchItemError (..),
    newBatchItemError,
    batchItemError_errorMessage,
    batchItemError_index,
    batchItemError_errorCode,

    -- * ClassifierEvaluationMetrics
    ClassifierEvaluationMetrics (..),
    newClassifierEvaluationMetrics,
    classifierEvaluationMetrics_microF1Score,
    classifierEvaluationMetrics_f1Score,
    classifierEvaluationMetrics_microPrecision,
    classifierEvaluationMetrics_accuracy,
    classifierEvaluationMetrics_hammingLoss,
    classifierEvaluationMetrics_recall,
    classifierEvaluationMetrics_precision,
    classifierEvaluationMetrics_microRecall,

    -- * ClassifierMetadata
    ClassifierMetadata (..),
    newClassifierMetadata,
    classifierMetadata_evaluationMetrics,
    classifierMetadata_numberOfTestDocuments,
    classifierMetadata_numberOfLabels,
    classifierMetadata_numberOfTrainedDocuments,

    -- * DocumentClass
    DocumentClass (..),
    newDocumentClass,
    documentClass_name,
    documentClass_score,

    -- * DocumentClassificationJobFilter
    DocumentClassificationJobFilter (..),
    newDocumentClassificationJobFilter,
    documentClassificationJobFilter_jobStatus,
    documentClassificationJobFilter_jobName,
    documentClassificationJobFilter_submitTimeBefore,
    documentClassificationJobFilter_submitTimeAfter,

    -- * DocumentClassificationJobProperties
    DocumentClassificationJobProperties (..),
    newDocumentClassificationJobProperties,
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

    -- * DocumentClassifierFilter
    DocumentClassifierFilter (..),
    newDocumentClassifierFilter,
    documentClassifierFilter_status,
    documentClassifierFilter_submitTimeBefore,
    documentClassifierFilter_documentClassifierName,
    documentClassifierFilter_submitTimeAfter,

    -- * DocumentClassifierInputDataConfig
    DocumentClassifierInputDataConfig (..),
    newDocumentClassifierInputDataConfig,
    documentClassifierInputDataConfig_dataFormat,
    documentClassifierInputDataConfig_testS3Uri,
    documentClassifierInputDataConfig_s3Uri,
    documentClassifierInputDataConfig_augmentedManifests,
    documentClassifierInputDataConfig_labelDelimiter,

    -- * DocumentClassifierOutputDataConfig
    DocumentClassifierOutputDataConfig (..),
    newDocumentClassifierOutputDataConfig,
    documentClassifierOutputDataConfig_kmsKeyId,
    documentClassifierOutputDataConfig_s3Uri,

    -- * DocumentClassifierProperties
    DocumentClassifierProperties (..),
    newDocumentClassifierProperties,
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
    documentClassifierProperties_sourceModelArn,
    documentClassifierProperties_inputDataConfig,
    documentClassifierProperties_trainingEndTime,
    documentClassifierProperties_classifierMetadata,

    -- * DocumentClassifierSummary
    DocumentClassifierSummary (..),
    newDocumentClassifierSummary,
    documentClassifierSummary_numberOfVersions,
    documentClassifierSummary_latestVersionName,
    documentClassifierSummary_latestVersionStatus,
    documentClassifierSummary_documentClassifierName,
    documentClassifierSummary_latestVersionCreatedAt,

    -- * DocumentLabel
    DocumentLabel (..),
    newDocumentLabel,
    documentLabel_name,
    documentLabel_score,

    -- * DocumentReaderConfig
    DocumentReaderConfig (..),
    newDocumentReaderConfig,
    documentReaderConfig_documentReadMode,
    documentReaderConfig_featureTypes,
    documentReaderConfig_documentReadAction,

    -- * DominantLanguage
    DominantLanguage (..),
    newDominantLanguage,
    dominantLanguage_score,
    dominantLanguage_languageCode,

    -- * DominantLanguageDetectionJobFilter
    DominantLanguageDetectionJobFilter (..),
    newDominantLanguageDetectionJobFilter,
    dominantLanguageDetectionJobFilter_jobStatus,
    dominantLanguageDetectionJobFilter_jobName,
    dominantLanguageDetectionJobFilter_submitTimeBefore,
    dominantLanguageDetectionJobFilter_submitTimeAfter,

    -- * DominantLanguageDetectionJobProperties
    DominantLanguageDetectionJobProperties (..),
    newDominantLanguageDetectionJobProperties,
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

    -- * EndpointFilter
    EndpointFilter (..),
    newEndpointFilter,
    endpointFilter_status,
    endpointFilter_creationTimeBefore,
    endpointFilter_modelArn,
    endpointFilter_creationTimeAfter,

    -- * EndpointProperties
    EndpointProperties (..),
    newEndpointProperties,
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

    -- * EntitiesDetectionJobFilter
    EntitiesDetectionJobFilter (..),
    newEntitiesDetectionJobFilter,
    entitiesDetectionJobFilter_jobStatus,
    entitiesDetectionJobFilter_jobName,
    entitiesDetectionJobFilter_submitTimeBefore,
    entitiesDetectionJobFilter_submitTimeAfter,

    -- * EntitiesDetectionJobProperties
    EntitiesDetectionJobProperties (..),
    newEntitiesDetectionJobProperties,
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

    -- * Entity
    Entity (..),
    newEntity,
    entity_beginOffset,
    entity_type,
    entity_score,
    entity_endOffset,
    entity_text,

    -- * EntityLabel
    EntityLabel (..),
    newEntityLabel,
    entityLabel_name,
    entityLabel_score,

    -- * EntityRecognizerAnnotations
    EntityRecognizerAnnotations (..),
    newEntityRecognizerAnnotations,
    entityRecognizerAnnotations_testS3Uri,
    entityRecognizerAnnotations_s3Uri,

    -- * EntityRecognizerDocuments
    EntityRecognizerDocuments (..),
    newEntityRecognizerDocuments,
    entityRecognizerDocuments_testS3Uri,
    entityRecognizerDocuments_inputFormat,
    entityRecognizerDocuments_s3Uri,

    -- * EntityRecognizerEntityList
    EntityRecognizerEntityList (..),
    newEntityRecognizerEntityList,
    entityRecognizerEntityList_s3Uri,

    -- * EntityRecognizerEvaluationMetrics
    EntityRecognizerEvaluationMetrics (..),
    newEntityRecognizerEvaluationMetrics,
    entityRecognizerEvaluationMetrics_f1Score,
    entityRecognizerEvaluationMetrics_recall,
    entityRecognizerEvaluationMetrics_precision,

    -- * EntityRecognizerFilter
    EntityRecognizerFilter (..),
    newEntityRecognizerFilter,
    entityRecognizerFilter_recognizerName,
    entityRecognizerFilter_status,
    entityRecognizerFilter_submitTimeBefore,
    entityRecognizerFilter_submitTimeAfter,

    -- * EntityRecognizerInputDataConfig
    EntityRecognizerInputDataConfig (..),
    newEntityRecognizerInputDataConfig,
    entityRecognizerInputDataConfig_dataFormat,
    entityRecognizerInputDataConfig_entityList,
    entityRecognizerInputDataConfig_annotations,
    entityRecognizerInputDataConfig_documents,
    entityRecognizerInputDataConfig_augmentedManifests,
    entityRecognizerInputDataConfig_entityTypes,

    -- * EntityRecognizerMetadata
    EntityRecognizerMetadata (..),
    newEntityRecognizerMetadata,
    entityRecognizerMetadata_evaluationMetrics,
    entityRecognizerMetadata_numberOfTestDocuments,
    entityRecognizerMetadata_entityTypes,
    entityRecognizerMetadata_numberOfTrainedDocuments,

    -- * EntityRecognizerMetadataEntityTypesListItem
    EntityRecognizerMetadataEntityTypesListItem (..),
    newEntityRecognizerMetadataEntityTypesListItem,
    entityRecognizerMetadataEntityTypesListItem_evaluationMetrics,
    entityRecognizerMetadataEntityTypesListItem_type,
    entityRecognizerMetadataEntityTypesListItem_numberOfTrainMentions,

    -- * EntityRecognizerProperties
    EntityRecognizerProperties (..),
    newEntityRecognizerProperties,
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
    entityRecognizerProperties_sourceModelArn,
    entityRecognizerProperties_inputDataConfig,
    entityRecognizerProperties_trainingEndTime,

    -- * EntityRecognizerSummary
    EntityRecognizerSummary (..),
    newEntityRecognizerSummary,
    entityRecognizerSummary_numberOfVersions,
    entityRecognizerSummary_latestVersionName,
    entityRecognizerSummary_recognizerName,
    entityRecognizerSummary_latestVersionStatus,
    entityRecognizerSummary_latestVersionCreatedAt,

    -- * EntityTypesEvaluationMetrics
    EntityTypesEvaluationMetrics (..),
    newEntityTypesEvaluationMetrics,
    entityTypesEvaluationMetrics_f1Score,
    entityTypesEvaluationMetrics_recall,
    entityTypesEvaluationMetrics_precision,

    -- * EntityTypesListItem
    EntityTypesListItem (..),
    newEntityTypesListItem,
    entityTypesListItem_type,

    -- * EventsDetectionJobFilter
    EventsDetectionJobFilter (..),
    newEventsDetectionJobFilter,
    eventsDetectionJobFilter_jobStatus,
    eventsDetectionJobFilter_jobName,
    eventsDetectionJobFilter_submitTimeBefore,
    eventsDetectionJobFilter_submitTimeAfter,

    -- * EventsDetectionJobProperties
    EventsDetectionJobProperties (..),
    newEventsDetectionJobProperties,
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

    -- * InputDataConfig
    InputDataConfig (..),
    newInputDataConfig,
    inputDataConfig_documentReaderConfig,
    inputDataConfig_inputFormat,
    inputDataConfig_s3Uri,

    -- * KeyPhrase
    KeyPhrase (..),
    newKeyPhrase,
    keyPhrase_beginOffset,
    keyPhrase_score,
    keyPhrase_endOffset,
    keyPhrase_text,

    -- * KeyPhrasesDetectionJobFilter
    KeyPhrasesDetectionJobFilter (..),
    newKeyPhrasesDetectionJobFilter,
    keyPhrasesDetectionJobFilter_jobStatus,
    keyPhrasesDetectionJobFilter_jobName,
    keyPhrasesDetectionJobFilter_submitTimeBefore,
    keyPhrasesDetectionJobFilter_submitTimeAfter,

    -- * KeyPhrasesDetectionJobProperties
    KeyPhrasesDetectionJobProperties (..),
    newKeyPhrasesDetectionJobProperties,
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

    -- * MentionSentiment
    MentionSentiment (..),
    newMentionSentiment,
    mentionSentiment_sentimentScore,
    mentionSentiment_sentiment,

    -- * OutputDataConfig
    OutputDataConfig (..),
    newOutputDataConfig,
    outputDataConfig_kmsKeyId,
    outputDataConfig_s3Uri,

    -- * PartOfSpeechTag
    PartOfSpeechTag (..),
    newPartOfSpeechTag,
    partOfSpeechTag_tag,
    partOfSpeechTag_score,

    -- * PiiEntitiesDetectionJobFilter
    PiiEntitiesDetectionJobFilter (..),
    newPiiEntitiesDetectionJobFilter,
    piiEntitiesDetectionJobFilter_jobStatus,
    piiEntitiesDetectionJobFilter_jobName,
    piiEntitiesDetectionJobFilter_submitTimeBefore,
    piiEntitiesDetectionJobFilter_submitTimeAfter,

    -- * PiiEntitiesDetectionJobProperties
    PiiEntitiesDetectionJobProperties (..),
    newPiiEntitiesDetectionJobProperties,
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

    -- * PiiEntity
    PiiEntity (..),
    newPiiEntity,
    piiEntity_beginOffset,
    piiEntity_type,
    piiEntity_score,
    piiEntity_endOffset,

    -- * PiiOutputDataConfig
    PiiOutputDataConfig (..),
    newPiiOutputDataConfig,
    piiOutputDataConfig_kmsKeyId,
    piiOutputDataConfig_s3Uri,

    -- * RedactionConfig
    RedactionConfig (..),
    newRedactionConfig,
    redactionConfig_maskMode,
    redactionConfig_piiEntityTypes,
    redactionConfig_maskCharacter,

    -- * SentimentDetectionJobFilter
    SentimentDetectionJobFilter (..),
    newSentimentDetectionJobFilter,
    sentimentDetectionJobFilter_jobStatus,
    sentimentDetectionJobFilter_jobName,
    sentimentDetectionJobFilter_submitTimeBefore,
    sentimentDetectionJobFilter_submitTimeAfter,

    -- * SentimentDetectionJobProperties
    SentimentDetectionJobProperties (..),
    newSentimentDetectionJobProperties,
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

    -- * SentimentScore
    SentimentScore (..),
    newSentimentScore,
    sentimentScore_positive,
    sentimentScore_negative,
    sentimentScore_neutral,
    sentimentScore_mixed,

    -- * SyntaxToken
    SyntaxToken (..),
    newSyntaxToken,
    syntaxToken_beginOffset,
    syntaxToken_partOfSpeech,
    syntaxToken_endOffset,
    syntaxToken_tokenId,
    syntaxToken_text,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * TargetedSentimentDetectionJobFilter
    TargetedSentimentDetectionJobFilter (..),
    newTargetedSentimentDetectionJobFilter,
    targetedSentimentDetectionJobFilter_jobStatus,
    targetedSentimentDetectionJobFilter_jobName,
    targetedSentimentDetectionJobFilter_submitTimeBefore,
    targetedSentimentDetectionJobFilter_submitTimeAfter,

    -- * TargetedSentimentDetectionJobProperties
    TargetedSentimentDetectionJobProperties (..),
    newTargetedSentimentDetectionJobProperties,
    targetedSentimentDetectionJobProperties_outputDataConfig,
    targetedSentimentDetectionJobProperties_message,
    targetedSentimentDetectionJobProperties_jobStatus,
    targetedSentimentDetectionJobProperties_vpcConfig,
    targetedSentimentDetectionJobProperties_jobName,
    targetedSentimentDetectionJobProperties_submitTime,
    targetedSentimentDetectionJobProperties_jobId,
    targetedSentimentDetectionJobProperties_volumeKmsKeyId,
    targetedSentimentDetectionJobProperties_dataAccessRoleArn,
    targetedSentimentDetectionJobProperties_endTime,
    targetedSentimentDetectionJobProperties_languageCode,
    targetedSentimentDetectionJobProperties_jobArn,
    targetedSentimentDetectionJobProperties_inputDataConfig,

    -- * TargetedSentimentEntity
    TargetedSentimentEntity (..),
    newTargetedSentimentEntity,
    targetedSentimentEntity_descriptiveMentionIndex,
    targetedSentimentEntity_mentions,

    -- * TargetedSentimentMention
    TargetedSentimentMention (..),
    newTargetedSentimentMention,
    targetedSentimentMention_beginOffset,
    targetedSentimentMention_type,
    targetedSentimentMention_score,
    targetedSentimentMention_endOffset,
    targetedSentimentMention_mentionSentiment,
    targetedSentimentMention_groupScore,
    targetedSentimentMention_text,

    -- * TopicsDetectionJobFilter
    TopicsDetectionJobFilter (..),
    newTopicsDetectionJobFilter,
    topicsDetectionJobFilter_jobStatus,
    topicsDetectionJobFilter_jobName,
    topicsDetectionJobFilter_submitTimeBefore,
    topicsDetectionJobFilter_submitTimeAfter,

    -- * TopicsDetectionJobProperties
    TopicsDetectionJobProperties (..),
    newTopicsDetectionJobProperties,
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

    -- * VpcConfig
    VpcConfig (..),
    newVpcConfig,
    vpcConfig_securityGroupIds,
    vpcConfig_subnets,
  )
where

import Amazonka.Comprehend.Types.AugmentedManifestsDocumentTypeFormat
import Amazonka.Comprehend.Types.AugmentedManifestsListItem
import Amazonka.Comprehend.Types.BatchDetectDominantLanguageItemResult
import Amazonka.Comprehend.Types.BatchDetectEntitiesItemResult
import Amazonka.Comprehend.Types.BatchDetectKeyPhrasesItemResult
import Amazonka.Comprehend.Types.BatchDetectSentimentItemResult
import Amazonka.Comprehend.Types.BatchDetectSyntaxItemResult
import Amazonka.Comprehend.Types.BatchDetectTargetedSentimentItemResult
import Amazonka.Comprehend.Types.BatchItemError
import Amazonka.Comprehend.Types.ClassifierEvaluationMetrics
import Amazonka.Comprehend.Types.ClassifierMetadata
import Amazonka.Comprehend.Types.DocumentClass
import Amazonka.Comprehend.Types.DocumentClassificationJobFilter
import Amazonka.Comprehend.Types.DocumentClassificationJobProperties
import Amazonka.Comprehend.Types.DocumentClassifierDataFormat
import Amazonka.Comprehend.Types.DocumentClassifierFilter
import Amazonka.Comprehend.Types.DocumentClassifierInputDataConfig
import Amazonka.Comprehend.Types.DocumentClassifierMode
import Amazonka.Comprehend.Types.DocumentClassifierOutputDataConfig
import Amazonka.Comprehend.Types.DocumentClassifierProperties
import Amazonka.Comprehend.Types.DocumentClassifierSummary
import Amazonka.Comprehend.Types.DocumentLabel
import Amazonka.Comprehend.Types.DocumentReadAction
import Amazonka.Comprehend.Types.DocumentReadFeatureTypes
import Amazonka.Comprehend.Types.DocumentReadMode
import Amazonka.Comprehend.Types.DocumentReaderConfig
import Amazonka.Comprehend.Types.DominantLanguage
import Amazonka.Comprehend.Types.DominantLanguageDetectionJobFilter
import Amazonka.Comprehend.Types.DominantLanguageDetectionJobProperties
import Amazonka.Comprehend.Types.EndpointFilter
import Amazonka.Comprehend.Types.EndpointProperties
import Amazonka.Comprehend.Types.EndpointStatus
import Amazonka.Comprehend.Types.EntitiesDetectionJobFilter
import Amazonka.Comprehend.Types.EntitiesDetectionJobProperties
import Amazonka.Comprehend.Types.Entity
import Amazonka.Comprehend.Types.EntityLabel
import Amazonka.Comprehend.Types.EntityRecognizerAnnotations
import Amazonka.Comprehend.Types.EntityRecognizerDataFormat
import Amazonka.Comprehend.Types.EntityRecognizerDocuments
import Amazonka.Comprehend.Types.EntityRecognizerEntityList
import Amazonka.Comprehend.Types.EntityRecognizerEvaluationMetrics
import Amazonka.Comprehend.Types.EntityRecognizerFilter
import Amazonka.Comprehend.Types.EntityRecognizerInputDataConfig
import Amazonka.Comprehend.Types.EntityRecognizerMetadata
import Amazonka.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem
import Amazonka.Comprehend.Types.EntityRecognizerProperties
import Amazonka.Comprehend.Types.EntityRecognizerSummary
import Amazonka.Comprehend.Types.EntityType
import Amazonka.Comprehend.Types.EntityTypesEvaluationMetrics
import Amazonka.Comprehend.Types.EntityTypesListItem
import Amazonka.Comprehend.Types.EventsDetectionJobFilter
import Amazonka.Comprehend.Types.EventsDetectionJobProperties
import Amazonka.Comprehend.Types.InputDataConfig
import Amazonka.Comprehend.Types.InputFormat
import Amazonka.Comprehend.Types.JobStatus
import Amazonka.Comprehend.Types.KeyPhrase
import Amazonka.Comprehend.Types.KeyPhrasesDetectionJobFilter
import Amazonka.Comprehend.Types.KeyPhrasesDetectionJobProperties
import Amazonka.Comprehend.Types.LanguageCode
import Amazonka.Comprehend.Types.MentionSentiment
import Amazonka.Comprehend.Types.ModelStatus
import Amazonka.Comprehend.Types.OutputDataConfig
import Amazonka.Comprehend.Types.PartOfSpeechTag
import Amazonka.Comprehend.Types.PartOfSpeechTagType
import Amazonka.Comprehend.Types.PiiEntitiesDetectionJobFilter
import Amazonka.Comprehend.Types.PiiEntitiesDetectionJobProperties
import Amazonka.Comprehend.Types.PiiEntitiesDetectionMaskMode
import Amazonka.Comprehend.Types.PiiEntitiesDetectionMode
import Amazonka.Comprehend.Types.PiiEntity
import Amazonka.Comprehend.Types.PiiEntityType
import Amazonka.Comprehend.Types.PiiOutputDataConfig
import Amazonka.Comprehend.Types.RedactionConfig
import Amazonka.Comprehend.Types.SentimentDetectionJobFilter
import Amazonka.Comprehend.Types.SentimentDetectionJobProperties
import Amazonka.Comprehend.Types.SentimentScore
import Amazonka.Comprehend.Types.SentimentType
import Amazonka.Comprehend.Types.Split
import Amazonka.Comprehend.Types.SyntaxLanguageCode
import Amazonka.Comprehend.Types.SyntaxToken
import Amazonka.Comprehend.Types.Tag
import Amazonka.Comprehend.Types.TargetedSentimentDetectionJobFilter
import Amazonka.Comprehend.Types.TargetedSentimentDetectionJobProperties
import Amazonka.Comprehend.Types.TargetedSentimentEntity
import Amazonka.Comprehend.Types.TargetedSentimentEntityType
import Amazonka.Comprehend.Types.TargetedSentimentMention
import Amazonka.Comprehend.Types.TopicsDetectionJobFilter
import Amazonka.Comprehend.Types.TopicsDetectionJobProperties
import Amazonka.Comprehend.Types.VpcConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-11-27@ of the Amazon Comprehend SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Comprehend",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "comprehend",
      Core.signingName = "comprehend",
      Core.version = "2017-11-27",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Comprehend",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified resource is not available. Check the resource and try your
-- request again.
_ResourceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ResourceUnavailableException"

-- | Concurrent modification of the tags associated with an Amazon Comprehend
-- resource is not supported.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | An internal server error occurred. Retry your request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The request contains more tags than can be associated with a resource
-- (50 tags per resource). The maximum number of tags includes both
-- existing tags and those included in your current request.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The request contains more tag keys than can be associated with a
-- resource (50 tag keys per resource).
_TooManyTagKeysException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagKeysException =
  Core._MatchServiceError
    defaultService
    "TooManyTagKeysException"

-- | Amazon Comprehend can\'t process the language of the input text. For
-- custom entity recognition APIs, only English, Spanish, French, Italian,
-- German, or Portuguese are accepted. For a list of supported languages,
-- <https://docs.aws.amazon.com/comprehend/latest/dg/supported-languages.html Supported languages>
-- in the Comprehend Developer Guide.
_UnsupportedLanguageException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedLanguageException =
  Core._MatchServiceError
    defaultService
    "UnsupportedLanguageException"

-- | The specified resource ARN was not found. Check the ARN and try your
-- request again.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The specified resource name is already in use. Use a different name and
-- try your request again.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The filter specified for the operation is invalid. Specify a different
-- filter.
_InvalidFilterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFilterException =
  Core._MatchServiceError
    defaultService
    "InvalidFilterException"

-- | The maximum number of resources per account has been exceeded. Review
-- the resources, and then try your request again.
_ResourceLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceededException"

-- | The size of the input text exceeds the limit. Use a smaller document.
_TextSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TextSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TextSizeLimitExceededException"

-- | The number of documents in the request exceeds the limit of 25. Try your
-- request again with fewer documents.
_BatchSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BatchSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "BatchSizeLimitExceededException"

-- | The KMS customer managed key (CMK) entered cannot be validated. Verify
-- the key and re-enter it.
_KmsKeyValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KmsKeyValidationException =
  Core._MatchServiceError
    defaultService
    "KmsKeyValidationException"

-- | The number of requests exceeds the limit. Resubmit your request later.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"

-- | The request is invalid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | The specified job was not found. Check the job ID and try again.
_JobNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_JobNotFoundException =
  Core._MatchServiceError
    defaultService
    "JobNotFoundException"

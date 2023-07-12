{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Comprehend.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _BatchSizeLimitExceededException,
    _ConcurrentModificationException,
    _InternalServerException,
    _InvalidFilterException,
    _InvalidRequestException,
    _JobNotFoundException,
    _KmsKeyValidationException,
    _ResourceInUseException,
    _ResourceLimitExceededException,
    _ResourceNotFoundException,
    _ResourceUnavailableException,
    _TextSizeLimitExceededException,
    _TooManyRequestsException,
    _TooManyTagKeysException,
    _TooManyTagsException,
    _UnsupportedLanguageException,

    -- * AugmentedManifestsDocumentTypeFormat
    AugmentedManifestsDocumentTypeFormat (..),

    -- * BlockType
    BlockType (..),

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

    -- * DocumentType
    DocumentType (..),

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

    -- * PageBasedErrorCode
    PageBasedErrorCode (..),

    -- * PartOfSpeechTagType
    PartOfSpeechTagType (..),

    -- * PiiEntitiesDetectionMaskMode
    PiiEntitiesDetectionMaskMode (..),

    -- * PiiEntitiesDetectionMode
    PiiEntitiesDetectionMode (..),

    -- * PiiEntityType
    PiiEntityType (..),

    -- * RelationshipType
    RelationshipType (..),

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
    augmentedManifestsListItem_annotationDataS3Uri,
    augmentedManifestsListItem_documentType,
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
    batchDetectSentimentItemResult_sentiment,
    batchDetectSentimentItemResult_sentimentScore,

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
    batchItemError_errorCode,
    batchItemError_errorMessage,
    batchItemError_index,

    -- * Block
    Block (..),
    newBlock,
    block_blockType,
    block_geometry,
    block_id,
    block_page,
    block_relationships,
    block_text,

    -- * BlockReference
    BlockReference (..),
    newBlockReference,
    blockReference_beginOffset,
    blockReference_blockId,
    blockReference_childBlocks,
    blockReference_endOffset,

    -- * BoundingBox
    BoundingBox (..),
    newBoundingBox,
    boundingBox_height,
    boundingBox_left,
    boundingBox_top,
    boundingBox_width,

    -- * ChildBlock
    ChildBlock (..),
    newChildBlock,
    childBlock_beginOffset,
    childBlock_childBlockId,
    childBlock_endOffset,

    -- * ClassifierEvaluationMetrics
    ClassifierEvaluationMetrics (..),
    newClassifierEvaluationMetrics,
    classifierEvaluationMetrics_accuracy,
    classifierEvaluationMetrics_f1Score,
    classifierEvaluationMetrics_hammingLoss,
    classifierEvaluationMetrics_microF1Score,
    classifierEvaluationMetrics_microPrecision,
    classifierEvaluationMetrics_microRecall,
    classifierEvaluationMetrics_precision,
    classifierEvaluationMetrics_recall,

    -- * ClassifierMetadata
    ClassifierMetadata (..),
    newClassifierMetadata,
    classifierMetadata_evaluationMetrics,
    classifierMetadata_numberOfLabels,
    classifierMetadata_numberOfTestDocuments,
    classifierMetadata_numberOfTrainedDocuments,

    -- * DocumentClass
    DocumentClass (..),
    newDocumentClass,
    documentClass_name,
    documentClass_page,
    documentClass_score,

    -- * DocumentClassificationJobFilter
    DocumentClassificationJobFilter (..),
    newDocumentClassificationJobFilter,
    documentClassificationJobFilter_jobName,
    documentClassificationJobFilter_jobStatus,
    documentClassificationJobFilter_submitTimeAfter,
    documentClassificationJobFilter_submitTimeBefore,

    -- * DocumentClassificationJobProperties
    DocumentClassificationJobProperties (..),
    newDocumentClassificationJobProperties,
    documentClassificationJobProperties_dataAccessRoleArn,
    documentClassificationJobProperties_documentClassifierArn,
    documentClassificationJobProperties_endTime,
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

    -- * DocumentClassifierFilter
    DocumentClassifierFilter (..),
    newDocumentClassifierFilter,
    documentClassifierFilter_documentClassifierName,
    documentClassifierFilter_status,
    documentClassifierFilter_submitTimeAfter,
    documentClassifierFilter_submitTimeBefore,

    -- * DocumentClassifierInputDataConfig
    DocumentClassifierInputDataConfig (..),
    newDocumentClassifierInputDataConfig,
    documentClassifierInputDataConfig_augmentedManifests,
    documentClassifierInputDataConfig_dataFormat,
    documentClassifierInputDataConfig_labelDelimiter,
    documentClassifierInputDataConfig_s3Uri,
    documentClassifierInputDataConfig_testS3Uri,

    -- * DocumentClassifierOutputDataConfig
    DocumentClassifierOutputDataConfig (..),
    newDocumentClassifierOutputDataConfig,
    documentClassifierOutputDataConfig_kmsKeyId,
    documentClassifierOutputDataConfig_s3Uri,

    -- * DocumentClassifierProperties
    DocumentClassifierProperties (..),
    newDocumentClassifierProperties,
    documentClassifierProperties_classifierMetadata,
    documentClassifierProperties_dataAccessRoleArn,
    documentClassifierProperties_documentClassifierArn,
    documentClassifierProperties_endTime,
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

    -- * DocumentClassifierSummary
    DocumentClassifierSummary (..),
    newDocumentClassifierSummary,
    documentClassifierSummary_documentClassifierName,
    documentClassifierSummary_latestVersionCreatedAt,
    documentClassifierSummary_latestVersionName,
    documentClassifierSummary_latestVersionStatus,
    documentClassifierSummary_numberOfVersions,

    -- * DocumentLabel
    DocumentLabel (..),
    newDocumentLabel,
    documentLabel_name,
    documentLabel_page,
    documentLabel_score,

    -- * DocumentMetadata
    DocumentMetadata (..),
    newDocumentMetadata,
    documentMetadata_extractedCharacters,
    documentMetadata_pages,

    -- * DocumentReaderConfig
    DocumentReaderConfig (..),
    newDocumentReaderConfig,
    documentReaderConfig_documentReadMode,
    documentReaderConfig_featureTypes,
    documentReaderConfig_documentReadAction,

    -- * DocumentTypeListItem
    DocumentTypeListItem (..),
    newDocumentTypeListItem,
    documentTypeListItem_page,
    documentTypeListItem_type,

    -- * DominantLanguage
    DominantLanguage (..),
    newDominantLanguage,
    dominantLanguage_languageCode,
    dominantLanguage_score,

    -- * DominantLanguageDetectionJobFilter
    DominantLanguageDetectionJobFilter (..),
    newDominantLanguageDetectionJobFilter,
    dominantLanguageDetectionJobFilter_jobName,
    dominantLanguageDetectionJobFilter_jobStatus,
    dominantLanguageDetectionJobFilter_submitTimeAfter,
    dominantLanguageDetectionJobFilter_submitTimeBefore,

    -- * DominantLanguageDetectionJobProperties
    DominantLanguageDetectionJobProperties (..),
    newDominantLanguageDetectionJobProperties,
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

    -- * EndpointFilter
    EndpointFilter (..),
    newEndpointFilter,
    endpointFilter_creationTimeAfter,
    endpointFilter_creationTimeBefore,
    endpointFilter_modelArn,
    endpointFilter_status,

    -- * EndpointProperties
    EndpointProperties (..),
    newEndpointProperties,
    endpointProperties_creationTime,
    endpointProperties_currentInferenceUnits,
    endpointProperties_dataAccessRoleArn,
    endpointProperties_desiredDataAccessRoleArn,
    endpointProperties_desiredInferenceUnits,
    endpointProperties_desiredModelArn,
    endpointProperties_endpointArn,
    endpointProperties_lastModifiedTime,
    endpointProperties_message,
    endpointProperties_modelArn,
    endpointProperties_status,

    -- * EntitiesDetectionJobFilter
    EntitiesDetectionJobFilter (..),
    newEntitiesDetectionJobFilter,
    entitiesDetectionJobFilter_jobName,
    entitiesDetectionJobFilter_jobStatus,
    entitiesDetectionJobFilter_submitTimeAfter,
    entitiesDetectionJobFilter_submitTimeBefore,

    -- * EntitiesDetectionJobProperties
    EntitiesDetectionJobProperties (..),
    newEntitiesDetectionJobProperties,
    entitiesDetectionJobProperties_dataAccessRoleArn,
    entitiesDetectionJobProperties_endTime,
    entitiesDetectionJobProperties_entityRecognizerArn,
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

    -- * Entity
    Entity (..),
    newEntity,
    entity_beginOffset,
    entity_blockReferences,
    entity_endOffset,
    entity_score,
    entity_text,
    entity_type,

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
    entityRecognizerDocuments_inputFormat,
    entityRecognizerDocuments_testS3Uri,
    entityRecognizerDocuments_s3Uri,

    -- * EntityRecognizerEntityList
    EntityRecognizerEntityList (..),
    newEntityRecognizerEntityList,
    entityRecognizerEntityList_s3Uri,

    -- * EntityRecognizerEvaluationMetrics
    EntityRecognizerEvaluationMetrics (..),
    newEntityRecognizerEvaluationMetrics,
    entityRecognizerEvaluationMetrics_f1Score,
    entityRecognizerEvaluationMetrics_precision,
    entityRecognizerEvaluationMetrics_recall,

    -- * EntityRecognizerFilter
    EntityRecognizerFilter (..),
    newEntityRecognizerFilter,
    entityRecognizerFilter_recognizerName,
    entityRecognizerFilter_status,
    entityRecognizerFilter_submitTimeAfter,
    entityRecognizerFilter_submitTimeBefore,

    -- * EntityRecognizerInputDataConfig
    EntityRecognizerInputDataConfig (..),
    newEntityRecognizerInputDataConfig,
    entityRecognizerInputDataConfig_annotations,
    entityRecognizerInputDataConfig_augmentedManifests,
    entityRecognizerInputDataConfig_dataFormat,
    entityRecognizerInputDataConfig_documents,
    entityRecognizerInputDataConfig_entityList,
    entityRecognizerInputDataConfig_entityTypes,

    -- * EntityRecognizerMetadata
    EntityRecognizerMetadata (..),
    newEntityRecognizerMetadata,
    entityRecognizerMetadata_entityTypes,
    entityRecognizerMetadata_evaluationMetrics,
    entityRecognizerMetadata_numberOfTestDocuments,
    entityRecognizerMetadata_numberOfTrainedDocuments,

    -- * EntityRecognizerMetadataEntityTypesListItem
    EntityRecognizerMetadataEntityTypesListItem (..),
    newEntityRecognizerMetadataEntityTypesListItem,
    entityRecognizerMetadataEntityTypesListItem_evaluationMetrics,
    entityRecognizerMetadataEntityTypesListItem_numberOfTrainMentions,
    entityRecognizerMetadataEntityTypesListItem_type,

    -- * EntityRecognizerProperties
    EntityRecognizerProperties (..),
    newEntityRecognizerProperties,
    entityRecognizerProperties_dataAccessRoleArn,
    entityRecognizerProperties_endTime,
    entityRecognizerProperties_entityRecognizerArn,
    entityRecognizerProperties_inputDataConfig,
    entityRecognizerProperties_languageCode,
    entityRecognizerProperties_message,
    entityRecognizerProperties_modelKmsKeyId,
    entityRecognizerProperties_recognizerMetadata,
    entityRecognizerProperties_sourceModelArn,
    entityRecognizerProperties_status,
    entityRecognizerProperties_submitTime,
    entityRecognizerProperties_trainingEndTime,
    entityRecognizerProperties_trainingStartTime,
    entityRecognizerProperties_versionName,
    entityRecognizerProperties_volumeKmsKeyId,
    entityRecognizerProperties_vpcConfig,

    -- * EntityRecognizerSummary
    EntityRecognizerSummary (..),
    newEntityRecognizerSummary,
    entityRecognizerSummary_latestVersionCreatedAt,
    entityRecognizerSummary_latestVersionName,
    entityRecognizerSummary_latestVersionStatus,
    entityRecognizerSummary_numberOfVersions,
    entityRecognizerSummary_recognizerName,

    -- * EntityTypesEvaluationMetrics
    EntityTypesEvaluationMetrics (..),
    newEntityTypesEvaluationMetrics,
    entityTypesEvaluationMetrics_f1Score,
    entityTypesEvaluationMetrics_precision,
    entityTypesEvaluationMetrics_recall,

    -- * EntityTypesListItem
    EntityTypesListItem (..),
    newEntityTypesListItem,
    entityTypesListItem_type,

    -- * ErrorsListItem
    ErrorsListItem (..),
    newErrorsListItem,
    errorsListItem_errorCode,
    errorsListItem_errorMessage,
    errorsListItem_page,

    -- * EventsDetectionJobFilter
    EventsDetectionJobFilter (..),
    newEventsDetectionJobFilter,
    eventsDetectionJobFilter_jobName,
    eventsDetectionJobFilter_jobStatus,
    eventsDetectionJobFilter_submitTimeAfter,
    eventsDetectionJobFilter_submitTimeBefore,

    -- * EventsDetectionJobProperties
    EventsDetectionJobProperties (..),
    newEventsDetectionJobProperties,
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

    -- * ExtractedCharactersListItem
    ExtractedCharactersListItem (..),
    newExtractedCharactersListItem,
    extractedCharactersListItem_count,
    extractedCharactersListItem_page,

    -- * Geometry
    Geometry (..),
    newGeometry,
    geometry_boundingBox,
    geometry_polygon,

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
    keyPhrase_endOffset,
    keyPhrase_score,
    keyPhrase_text,

    -- * KeyPhrasesDetectionJobFilter
    KeyPhrasesDetectionJobFilter (..),
    newKeyPhrasesDetectionJobFilter,
    keyPhrasesDetectionJobFilter_jobName,
    keyPhrasesDetectionJobFilter_jobStatus,
    keyPhrasesDetectionJobFilter_submitTimeAfter,
    keyPhrasesDetectionJobFilter_submitTimeBefore,

    -- * KeyPhrasesDetectionJobProperties
    KeyPhrasesDetectionJobProperties (..),
    newKeyPhrasesDetectionJobProperties,
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

    -- * MentionSentiment
    MentionSentiment (..),
    newMentionSentiment,
    mentionSentiment_sentiment,
    mentionSentiment_sentimentScore,

    -- * OutputDataConfig
    OutputDataConfig (..),
    newOutputDataConfig,
    outputDataConfig_kmsKeyId,
    outputDataConfig_s3Uri,

    -- * PartOfSpeechTag
    PartOfSpeechTag (..),
    newPartOfSpeechTag,
    partOfSpeechTag_score,
    partOfSpeechTag_tag,

    -- * PiiEntitiesDetectionJobFilter
    PiiEntitiesDetectionJobFilter (..),
    newPiiEntitiesDetectionJobFilter,
    piiEntitiesDetectionJobFilter_jobName,
    piiEntitiesDetectionJobFilter_jobStatus,
    piiEntitiesDetectionJobFilter_submitTimeAfter,
    piiEntitiesDetectionJobFilter_submitTimeBefore,

    -- * PiiEntitiesDetectionJobProperties
    PiiEntitiesDetectionJobProperties (..),
    newPiiEntitiesDetectionJobProperties,
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

    -- * PiiEntity
    PiiEntity (..),
    newPiiEntity,
    piiEntity_beginOffset,
    piiEntity_endOffset,
    piiEntity_score,
    piiEntity_type,

    -- * PiiOutputDataConfig
    PiiOutputDataConfig (..),
    newPiiOutputDataConfig,
    piiOutputDataConfig_kmsKeyId,
    piiOutputDataConfig_s3Uri,

    -- * Point
    Point (..),
    newPoint,
    point_x,
    point_y,

    -- * RedactionConfig
    RedactionConfig (..),
    newRedactionConfig,
    redactionConfig_maskCharacter,
    redactionConfig_maskMode,
    redactionConfig_piiEntityTypes,

    -- * RelationshipsListItem
    RelationshipsListItem (..),
    newRelationshipsListItem,
    relationshipsListItem_ids,
    relationshipsListItem_type,

    -- * SentimentDetectionJobFilter
    SentimentDetectionJobFilter (..),
    newSentimentDetectionJobFilter,
    sentimentDetectionJobFilter_jobName,
    sentimentDetectionJobFilter_jobStatus,
    sentimentDetectionJobFilter_submitTimeAfter,
    sentimentDetectionJobFilter_submitTimeBefore,

    -- * SentimentDetectionJobProperties
    SentimentDetectionJobProperties (..),
    newSentimentDetectionJobProperties,
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

    -- * SentimentScore
    SentimentScore (..),
    newSentimentScore,
    sentimentScore_mixed,
    sentimentScore_negative,
    sentimentScore_neutral,
    sentimentScore_positive,

    -- * SyntaxToken
    SyntaxToken (..),
    newSyntaxToken,
    syntaxToken_beginOffset,
    syntaxToken_endOffset,
    syntaxToken_partOfSpeech,
    syntaxToken_text,
    syntaxToken_tokenId,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * TargetedSentimentDetectionJobFilter
    TargetedSentimentDetectionJobFilter (..),
    newTargetedSentimentDetectionJobFilter,
    targetedSentimentDetectionJobFilter_jobName,
    targetedSentimentDetectionJobFilter_jobStatus,
    targetedSentimentDetectionJobFilter_submitTimeAfter,
    targetedSentimentDetectionJobFilter_submitTimeBefore,

    -- * TargetedSentimentDetectionJobProperties
    TargetedSentimentDetectionJobProperties (..),
    newTargetedSentimentDetectionJobProperties,
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

    -- * TargetedSentimentEntity
    TargetedSentimentEntity (..),
    newTargetedSentimentEntity,
    targetedSentimentEntity_descriptiveMentionIndex,
    targetedSentimentEntity_mentions,

    -- * TargetedSentimentMention
    TargetedSentimentMention (..),
    newTargetedSentimentMention,
    targetedSentimentMention_beginOffset,
    targetedSentimentMention_endOffset,
    targetedSentimentMention_groupScore,
    targetedSentimentMention_mentionSentiment,
    targetedSentimentMention_score,
    targetedSentimentMention_text,
    targetedSentimentMention_type,

    -- * TopicsDetectionJobFilter
    TopicsDetectionJobFilter (..),
    newTopicsDetectionJobFilter,
    topicsDetectionJobFilter_jobName,
    topicsDetectionJobFilter_jobStatus,
    topicsDetectionJobFilter_submitTimeAfter,
    topicsDetectionJobFilter_submitTimeBefore,

    -- * TopicsDetectionJobProperties
    TopicsDetectionJobProperties (..),
    newTopicsDetectionJobProperties,
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
import Amazonka.Comprehend.Types.Block
import Amazonka.Comprehend.Types.BlockReference
import Amazonka.Comprehend.Types.BlockType
import Amazonka.Comprehend.Types.BoundingBox
import Amazonka.Comprehend.Types.ChildBlock
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
import Amazonka.Comprehend.Types.DocumentMetadata
import Amazonka.Comprehend.Types.DocumentReadAction
import Amazonka.Comprehend.Types.DocumentReadFeatureTypes
import Amazonka.Comprehend.Types.DocumentReadMode
import Amazonka.Comprehend.Types.DocumentReaderConfig
import Amazonka.Comprehend.Types.DocumentType
import Amazonka.Comprehend.Types.DocumentTypeListItem
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
import Amazonka.Comprehend.Types.ErrorsListItem
import Amazonka.Comprehend.Types.EventsDetectionJobFilter
import Amazonka.Comprehend.Types.EventsDetectionJobProperties
import Amazonka.Comprehend.Types.ExtractedCharactersListItem
import Amazonka.Comprehend.Types.Geometry
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
import Amazonka.Comprehend.Types.PageBasedErrorCode
import Amazonka.Comprehend.Types.PartOfSpeechTag
import Amazonka.Comprehend.Types.PartOfSpeechTagType
import Amazonka.Comprehend.Types.PiiEntitiesDetectionJobFilter
import Amazonka.Comprehend.Types.PiiEntitiesDetectionJobProperties
import Amazonka.Comprehend.Types.PiiEntitiesDetectionMaskMode
import Amazonka.Comprehend.Types.PiiEntitiesDetectionMode
import Amazonka.Comprehend.Types.PiiEntity
import Amazonka.Comprehend.Types.PiiEntityType
import Amazonka.Comprehend.Types.PiiOutputDataConfig
import Amazonka.Comprehend.Types.Point
import Amazonka.Comprehend.Types.RedactionConfig
import Amazonka.Comprehend.Types.RelationshipType
import Amazonka.Comprehend.Types.RelationshipsListItem
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
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The number of documents in the request exceeds the limit of 25. Try your
-- request again with fewer documents.
_BatchSizeLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BatchSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "BatchSizeLimitExceededException"

-- | Concurrent modification of the tags associated with an Amazon Comprehend
-- resource is not supported.
_ConcurrentModificationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | An internal server error occurred. Retry your request.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The filter specified for the operation is invalid. Specify a different
-- filter.
_InvalidFilterException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidFilterException =
  Core._MatchServiceError
    defaultService
    "InvalidFilterException"

-- | The request is invalid.
_InvalidRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | The specified job was not found. Check the job ID and try again.
_JobNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_JobNotFoundException =
  Core._MatchServiceError
    defaultService
    "JobNotFoundException"

-- | The KMS customer managed key (CMK) entered cannot be validated. Verify
-- the key and re-enter it.
_KmsKeyValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KmsKeyValidationException =
  Core._MatchServiceError
    defaultService
    "KmsKeyValidationException"

-- | The specified resource name is already in use. Use a different name and
-- try your request again.
_ResourceInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The maximum number of resources per account has been exceeded. Review
-- the resources, and then try your request again.
_ResourceLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceededException"

-- | The specified resource ARN was not found. Check the ARN and try your
-- request again.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The specified resource is not available. Check the resource and try your
-- request again.
_ResourceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ResourceUnavailableException"

-- | The size of the input text exceeds the limit. Use a smaller document.
_TextSizeLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TextSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TextSizeLimitExceededException"

-- | The number of requests exceeds the limit. Resubmit your request later.
_TooManyRequestsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"

-- | The request contains more tag keys than can be associated with a
-- resource (50 tag keys per resource).
_TooManyTagKeysException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTagKeysException =
  Core._MatchServiceError
    defaultService
    "TooManyTagKeysException"

-- | The request contains more tags than can be associated with a resource
-- (50 tags per resource). The maximum number of tags includes both
-- existing tags and those included in your current request.
_TooManyTagsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | Amazon Comprehend can\'t process the language of the input text. For
-- custom entity recognition APIs, only English, Spanish, French, Italian,
-- German, or Portuguese are accepted. For a list of supported languages,
-- <https://docs.aws.amazon.com/comprehend/latest/dg/supported-languages.html Supported languages>
-- in the Comprehend Developer Guide.
_UnsupportedLanguageException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnsupportedLanguageException =
  Core._MatchServiceError
    defaultService
    "UnsupportedLanguageException"

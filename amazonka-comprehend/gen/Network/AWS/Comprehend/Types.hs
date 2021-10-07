{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceUnavailableException,
    _TooManyTagsException,
    _InvalidFilterException,
    _ResourceLimitExceededException,
    _BatchSizeLimitExceededException,
    _ConcurrentModificationException,
    _InvalidRequestException,
    _TextSizeLimitExceededException,
    _ResourceInUseException,
    _KmsKeyValidationException,
    _ResourceNotFoundException,
    _TooManyTagKeysException,
    _JobNotFoundException,
    _UnsupportedLanguageException,
    _InternalServerException,
    _TooManyRequestsException,

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

    -- * AugmentedManifestsListItem
    AugmentedManifestsListItem (..),
    newAugmentedManifestsListItem,
    augmentedManifestsListItem_documentType,
    augmentedManifestsListItem_sourceDocumentsS3Uri,
    augmentedManifestsListItem_annotationDataS3Uri,
    augmentedManifestsListItem_split,
    augmentedManifestsListItem_s3Uri,
    augmentedManifestsListItem_attributeNames,

    -- * BatchDetectDominantLanguageItemResult
    BatchDetectDominantLanguageItemResult (..),
    newBatchDetectDominantLanguageItemResult,
    batchDetectDominantLanguageItemResult_languages,
    batchDetectDominantLanguageItemResult_index,

    -- * BatchDetectEntitiesItemResult
    BatchDetectEntitiesItemResult (..),
    newBatchDetectEntitiesItemResult,
    batchDetectEntitiesItemResult_index,
    batchDetectEntitiesItemResult_entities,

    -- * BatchDetectKeyPhrasesItemResult
    BatchDetectKeyPhrasesItemResult (..),
    newBatchDetectKeyPhrasesItemResult,
    batchDetectKeyPhrasesItemResult_keyPhrases,
    batchDetectKeyPhrasesItemResult_index,

    -- * BatchDetectSentimentItemResult
    BatchDetectSentimentItemResult (..),
    newBatchDetectSentimentItemResult,
    batchDetectSentimentItemResult_sentiment,
    batchDetectSentimentItemResult_sentimentScore,
    batchDetectSentimentItemResult_index,

    -- * BatchDetectSyntaxItemResult
    BatchDetectSyntaxItemResult (..),
    newBatchDetectSyntaxItemResult,
    batchDetectSyntaxItemResult_syntaxTokens,
    batchDetectSyntaxItemResult_index,

    -- * BatchItemError
    BatchItemError (..),
    newBatchItemError,
    batchItemError_index,
    batchItemError_errorMessage,
    batchItemError_errorCode,

    -- * ClassifierEvaluationMetrics
    ClassifierEvaluationMetrics (..),
    newClassifierEvaluationMetrics,
    classifierEvaluationMetrics_f1Score,
    classifierEvaluationMetrics_microRecall,
    classifierEvaluationMetrics_microPrecision,
    classifierEvaluationMetrics_precision,
    classifierEvaluationMetrics_accuracy,
    classifierEvaluationMetrics_hammingLoss,
    classifierEvaluationMetrics_recall,
    classifierEvaluationMetrics_microF1Score,

    -- * ClassifierMetadata
    ClassifierMetadata (..),
    newClassifierMetadata,
    classifierMetadata_numberOfLabels,
    classifierMetadata_numberOfTestDocuments,
    classifierMetadata_numberOfTrainedDocuments,
    classifierMetadata_evaluationMetrics,

    -- * DocumentClass
    DocumentClass (..),
    newDocumentClass,
    documentClass_name,
    documentClass_score,

    -- * DocumentClassificationJobFilter
    DocumentClassificationJobFilter (..),
    newDocumentClassificationJobFilter,
    documentClassificationJobFilter_jobStatus,
    documentClassificationJobFilter_submitTimeBefore,
    documentClassificationJobFilter_submitTimeAfter,
    documentClassificationJobFilter_jobName,

    -- * DocumentClassificationJobProperties
    DocumentClassificationJobProperties (..),
    newDocumentClassificationJobProperties,
    documentClassificationJobProperties_vpcConfig,
    documentClassificationJobProperties_inputDataConfig,
    documentClassificationJobProperties_message,
    documentClassificationJobProperties_jobStatus,
    documentClassificationJobProperties_outputDataConfig,
    documentClassificationJobProperties_documentClassifierArn,
    documentClassificationJobProperties_endTime,
    documentClassificationJobProperties_jobArn,
    documentClassificationJobProperties_volumeKmsKeyId,
    documentClassificationJobProperties_submitTime,
    documentClassificationJobProperties_jobName,
    documentClassificationJobProperties_dataAccessRoleArn,
    documentClassificationJobProperties_jobId,

    -- * DocumentClassifierFilter
    DocumentClassifierFilter (..),
    newDocumentClassifierFilter,
    documentClassifierFilter_status,
    documentClassifierFilter_submitTimeBefore,
    documentClassifierFilter_submitTimeAfter,
    documentClassifierFilter_documentClassifierName,

    -- * DocumentClassifierInputDataConfig
    DocumentClassifierInputDataConfig (..),
    newDocumentClassifierInputDataConfig,
    documentClassifierInputDataConfig_augmentedManifests,
    documentClassifierInputDataConfig_dataFormat,
    documentClassifierInputDataConfig_testS3Uri,
    documentClassifierInputDataConfig_labelDelimiter,
    documentClassifierInputDataConfig_s3Uri,

    -- * DocumentClassifierOutputDataConfig
    DocumentClassifierOutputDataConfig (..),
    newDocumentClassifierOutputDataConfig,
    documentClassifierOutputDataConfig_kmsKeyId,
    documentClassifierOutputDataConfig_s3Uri,

    -- * DocumentClassifierProperties
    DocumentClassifierProperties (..),
    newDocumentClassifierProperties,
    documentClassifierProperties_languageCode,
    documentClassifierProperties_vpcConfig,
    documentClassifierProperties_status,
    documentClassifierProperties_inputDataConfig,
    documentClassifierProperties_message,
    documentClassifierProperties_mode,
    documentClassifierProperties_outputDataConfig,
    documentClassifierProperties_documentClassifierArn,
    documentClassifierProperties_versionName,
    documentClassifierProperties_endTime,
    documentClassifierProperties_classifierMetadata,
    documentClassifierProperties_volumeKmsKeyId,
    documentClassifierProperties_submitTime,
    documentClassifierProperties_modelKmsKeyId,
    documentClassifierProperties_trainingStartTime,
    documentClassifierProperties_dataAccessRoleArn,
    documentClassifierProperties_trainingEndTime,

    -- * DocumentClassifierSummary
    DocumentClassifierSummary (..),
    newDocumentClassifierSummary,
    documentClassifierSummary_latestVersionCreatedAt,
    documentClassifierSummary_documentClassifierName,
    documentClassifierSummary_latestVersionName,
    documentClassifierSummary_numberOfVersions,
    documentClassifierSummary_latestVersionStatus,

    -- * DocumentLabel
    DocumentLabel (..),
    newDocumentLabel,
    documentLabel_name,
    documentLabel_score,

    -- * DocumentReaderConfig
    DocumentReaderConfig (..),
    newDocumentReaderConfig,
    documentReaderConfig_featureTypes,
    documentReaderConfig_documentReadMode,
    documentReaderConfig_documentReadAction,

    -- * DominantLanguage
    DominantLanguage (..),
    newDominantLanguage,
    dominantLanguage_languageCode,
    dominantLanguage_score,

    -- * DominantLanguageDetectionJobFilter
    DominantLanguageDetectionJobFilter (..),
    newDominantLanguageDetectionJobFilter,
    dominantLanguageDetectionJobFilter_jobStatus,
    dominantLanguageDetectionJobFilter_submitTimeBefore,
    dominantLanguageDetectionJobFilter_submitTimeAfter,
    dominantLanguageDetectionJobFilter_jobName,

    -- * DominantLanguageDetectionJobProperties
    DominantLanguageDetectionJobProperties (..),
    newDominantLanguageDetectionJobProperties,
    dominantLanguageDetectionJobProperties_vpcConfig,
    dominantLanguageDetectionJobProperties_inputDataConfig,
    dominantLanguageDetectionJobProperties_message,
    dominantLanguageDetectionJobProperties_jobStatus,
    dominantLanguageDetectionJobProperties_outputDataConfig,
    dominantLanguageDetectionJobProperties_endTime,
    dominantLanguageDetectionJobProperties_jobArn,
    dominantLanguageDetectionJobProperties_volumeKmsKeyId,
    dominantLanguageDetectionJobProperties_submitTime,
    dominantLanguageDetectionJobProperties_jobName,
    dominantLanguageDetectionJobProperties_dataAccessRoleArn,
    dominantLanguageDetectionJobProperties_jobId,

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
    endpointProperties_currentInferenceUnits,
    endpointProperties_creationTime,
    endpointProperties_status,
    endpointProperties_desiredInferenceUnits,
    endpointProperties_message,
    endpointProperties_desiredDataAccessRoleArn,
    endpointProperties_modelArn,
    endpointProperties_lastModifiedTime,
    endpointProperties_endpointArn,
    endpointProperties_dataAccessRoleArn,
    endpointProperties_desiredModelArn,

    -- * EntitiesDetectionJobFilter
    EntitiesDetectionJobFilter (..),
    newEntitiesDetectionJobFilter,
    entitiesDetectionJobFilter_jobStatus,
    entitiesDetectionJobFilter_submitTimeBefore,
    entitiesDetectionJobFilter_submitTimeAfter,
    entitiesDetectionJobFilter_jobName,

    -- * EntitiesDetectionJobProperties
    EntitiesDetectionJobProperties (..),
    newEntitiesDetectionJobProperties,
    entitiesDetectionJobProperties_languageCode,
    entitiesDetectionJobProperties_vpcConfig,
    entitiesDetectionJobProperties_inputDataConfig,
    entitiesDetectionJobProperties_message,
    entitiesDetectionJobProperties_jobStatus,
    entitiesDetectionJobProperties_outputDataConfig,
    entitiesDetectionJobProperties_endTime,
    entitiesDetectionJobProperties_jobArn,
    entitiesDetectionJobProperties_volumeKmsKeyId,
    entitiesDetectionJobProperties_submitTime,
    entitiesDetectionJobProperties_entityRecognizerArn,
    entitiesDetectionJobProperties_jobName,
    entitiesDetectionJobProperties_dataAccessRoleArn,
    entitiesDetectionJobProperties_jobId,

    -- * Entity
    Entity (..),
    newEntity,
    entity_endOffset,
    entity_type,
    entity_score,
    entity_text,
    entity_beginOffset,

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
    entityRecognizerEvaluationMetrics_precision,
    entityRecognizerEvaluationMetrics_recall,

    -- * EntityRecognizerFilter
    EntityRecognizerFilter (..),
    newEntityRecognizerFilter,
    entityRecognizerFilter_status,
    entityRecognizerFilter_submitTimeBefore,
    entityRecognizerFilter_submitTimeAfter,
    entityRecognizerFilter_recognizerName,

    -- * EntityRecognizerInputDataConfig
    EntityRecognizerInputDataConfig (..),
    newEntityRecognizerInputDataConfig,
    entityRecognizerInputDataConfig_documents,
    entityRecognizerInputDataConfig_augmentedManifests,
    entityRecognizerInputDataConfig_dataFormat,
    entityRecognizerInputDataConfig_annotations,
    entityRecognizerInputDataConfig_entityList,
    entityRecognizerInputDataConfig_entityTypes,

    -- * EntityRecognizerMetadata
    EntityRecognizerMetadata (..),
    newEntityRecognizerMetadata,
    entityRecognizerMetadata_numberOfTestDocuments,
    entityRecognizerMetadata_numberOfTrainedDocuments,
    entityRecognizerMetadata_evaluationMetrics,
    entityRecognizerMetadata_entityTypes,

    -- * EntityRecognizerMetadataEntityTypesListItem
    EntityRecognizerMetadataEntityTypesListItem (..),
    newEntityRecognizerMetadataEntityTypesListItem,
    entityRecognizerMetadataEntityTypesListItem_numberOfTrainMentions,
    entityRecognizerMetadataEntityTypesListItem_evaluationMetrics,
    entityRecognizerMetadataEntityTypesListItem_type,

    -- * EntityRecognizerProperties
    EntityRecognizerProperties (..),
    newEntityRecognizerProperties,
    entityRecognizerProperties_languageCode,
    entityRecognizerProperties_vpcConfig,
    entityRecognizerProperties_status,
    entityRecognizerProperties_inputDataConfig,
    entityRecognizerProperties_message,
    entityRecognizerProperties_versionName,
    entityRecognizerProperties_endTime,
    entityRecognizerProperties_volumeKmsKeyId,
    entityRecognizerProperties_submitTime,
    entityRecognizerProperties_recognizerMetadata,
    entityRecognizerProperties_entityRecognizerArn,
    entityRecognizerProperties_modelKmsKeyId,
    entityRecognizerProperties_trainingStartTime,
    entityRecognizerProperties_dataAccessRoleArn,
    entityRecognizerProperties_trainingEndTime,

    -- * EntityRecognizerSummary
    EntityRecognizerSummary (..),
    newEntityRecognizerSummary,
    entityRecognizerSummary_latestVersionCreatedAt,
    entityRecognizerSummary_recognizerName,
    entityRecognizerSummary_latestVersionName,
    entityRecognizerSummary_numberOfVersions,
    entityRecognizerSummary_latestVersionStatus,

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

    -- * EventsDetectionJobFilter
    EventsDetectionJobFilter (..),
    newEventsDetectionJobFilter,
    eventsDetectionJobFilter_jobStatus,
    eventsDetectionJobFilter_submitTimeBefore,
    eventsDetectionJobFilter_submitTimeAfter,
    eventsDetectionJobFilter_jobName,

    -- * EventsDetectionJobProperties
    EventsDetectionJobProperties (..),
    newEventsDetectionJobProperties,
    eventsDetectionJobProperties_languageCode,
    eventsDetectionJobProperties_inputDataConfig,
    eventsDetectionJobProperties_message,
    eventsDetectionJobProperties_jobStatus,
    eventsDetectionJobProperties_outputDataConfig,
    eventsDetectionJobProperties_endTime,
    eventsDetectionJobProperties_jobArn,
    eventsDetectionJobProperties_submitTime,
    eventsDetectionJobProperties_targetEventTypes,
    eventsDetectionJobProperties_jobName,
    eventsDetectionJobProperties_dataAccessRoleArn,
    eventsDetectionJobProperties_jobId,

    -- * InputDataConfig
    InputDataConfig (..),
    newInputDataConfig,
    inputDataConfig_documentReaderConfig,
    inputDataConfig_inputFormat,
    inputDataConfig_s3Uri,

    -- * KeyPhrase
    KeyPhrase (..),
    newKeyPhrase,
    keyPhrase_endOffset,
    keyPhrase_score,
    keyPhrase_text,
    keyPhrase_beginOffset,

    -- * KeyPhrasesDetectionJobFilter
    KeyPhrasesDetectionJobFilter (..),
    newKeyPhrasesDetectionJobFilter,
    keyPhrasesDetectionJobFilter_jobStatus,
    keyPhrasesDetectionJobFilter_submitTimeBefore,
    keyPhrasesDetectionJobFilter_submitTimeAfter,
    keyPhrasesDetectionJobFilter_jobName,

    -- * KeyPhrasesDetectionJobProperties
    KeyPhrasesDetectionJobProperties (..),
    newKeyPhrasesDetectionJobProperties,
    keyPhrasesDetectionJobProperties_languageCode,
    keyPhrasesDetectionJobProperties_vpcConfig,
    keyPhrasesDetectionJobProperties_inputDataConfig,
    keyPhrasesDetectionJobProperties_message,
    keyPhrasesDetectionJobProperties_jobStatus,
    keyPhrasesDetectionJobProperties_outputDataConfig,
    keyPhrasesDetectionJobProperties_endTime,
    keyPhrasesDetectionJobProperties_jobArn,
    keyPhrasesDetectionJobProperties_volumeKmsKeyId,
    keyPhrasesDetectionJobProperties_submitTime,
    keyPhrasesDetectionJobProperties_jobName,
    keyPhrasesDetectionJobProperties_dataAccessRoleArn,
    keyPhrasesDetectionJobProperties_jobId,

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
    piiEntitiesDetectionJobFilter_jobStatus,
    piiEntitiesDetectionJobFilter_submitTimeBefore,
    piiEntitiesDetectionJobFilter_submitTimeAfter,
    piiEntitiesDetectionJobFilter_jobName,

    -- * PiiEntitiesDetectionJobProperties
    PiiEntitiesDetectionJobProperties (..),
    newPiiEntitiesDetectionJobProperties,
    piiEntitiesDetectionJobProperties_languageCode,
    piiEntitiesDetectionJobProperties_redactionConfig,
    piiEntitiesDetectionJobProperties_inputDataConfig,
    piiEntitiesDetectionJobProperties_message,
    piiEntitiesDetectionJobProperties_mode,
    piiEntitiesDetectionJobProperties_jobStatus,
    piiEntitiesDetectionJobProperties_outputDataConfig,
    piiEntitiesDetectionJobProperties_endTime,
    piiEntitiesDetectionJobProperties_jobArn,
    piiEntitiesDetectionJobProperties_submitTime,
    piiEntitiesDetectionJobProperties_jobName,
    piiEntitiesDetectionJobProperties_dataAccessRoleArn,
    piiEntitiesDetectionJobProperties_jobId,

    -- * PiiEntity
    PiiEntity (..),
    newPiiEntity,
    piiEntity_endOffset,
    piiEntity_type,
    piiEntity_score,
    piiEntity_beginOffset,

    -- * PiiOutputDataConfig
    PiiOutputDataConfig (..),
    newPiiOutputDataConfig,
    piiOutputDataConfig_kmsKeyId,
    piiOutputDataConfig_s3Uri,

    -- * RedactionConfig
    RedactionConfig (..),
    newRedactionConfig,
    redactionConfig_maskCharacter,
    redactionConfig_maskMode,
    redactionConfig_piiEntityTypes,

    -- * SentimentDetectionJobFilter
    SentimentDetectionJobFilter (..),
    newSentimentDetectionJobFilter,
    sentimentDetectionJobFilter_jobStatus,
    sentimentDetectionJobFilter_submitTimeBefore,
    sentimentDetectionJobFilter_submitTimeAfter,
    sentimentDetectionJobFilter_jobName,

    -- * SentimentDetectionJobProperties
    SentimentDetectionJobProperties (..),
    newSentimentDetectionJobProperties,
    sentimentDetectionJobProperties_languageCode,
    sentimentDetectionJobProperties_vpcConfig,
    sentimentDetectionJobProperties_inputDataConfig,
    sentimentDetectionJobProperties_message,
    sentimentDetectionJobProperties_jobStatus,
    sentimentDetectionJobProperties_outputDataConfig,
    sentimentDetectionJobProperties_endTime,
    sentimentDetectionJobProperties_jobArn,
    sentimentDetectionJobProperties_volumeKmsKeyId,
    sentimentDetectionJobProperties_submitTime,
    sentimentDetectionJobProperties_jobName,
    sentimentDetectionJobProperties_dataAccessRoleArn,
    sentimentDetectionJobProperties_jobId,

    -- * SentimentScore
    SentimentScore (..),
    newSentimentScore,
    sentimentScore_negative,
    sentimentScore_mixed,
    sentimentScore_positive,
    sentimentScore_neutral,

    -- * SyntaxToken
    SyntaxToken (..),
    newSyntaxToken,
    syntaxToken_tokenId,
    syntaxToken_partOfSpeech,
    syntaxToken_endOffset,
    syntaxToken_text,
    syntaxToken_beginOffset,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * TopicsDetectionJobFilter
    TopicsDetectionJobFilter (..),
    newTopicsDetectionJobFilter,
    topicsDetectionJobFilter_jobStatus,
    topicsDetectionJobFilter_submitTimeBefore,
    topicsDetectionJobFilter_submitTimeAfter,
    topicsDetectionJobFilter_jobName,

    -- * TopicsDetectionJobProperties
    TopicsDetectionJobProperties (..),
    newTopicsDetectionJobProperties,
    topicsDetectionJobProperties_vpcConfig,
    topicsDetectionJobProperties_inputDataConfig,
    topicsDetectionJobProperties_message,
    topicsDetectionJobProperties_jobStatus,
    topicsDetectionJobProperties_outputDataConfig,
    topicsDetectionJobProperties_endTime,
    topicsDetectionJobProperties_jobArn,
    topicsDetectionJobProperties_volumeKmsKeyId,
    topicsDetectionJobProperties_submitTime,
    topicsDetectionJobProperties_numberOfTopics,
    topicsDetectionJobProperties_jobName,
    topicsDetectionJobProperties_dataAccessRoleArn,
    topicsDetectionJobProperties_jobId,

    -- * VpcConfig
    VpcConfig (..),
    newVpcConfig,
    vpcConfig_securityGroupIds,
    vpcConfig_subnets,
  )
where

import Network.AWS.Comprehend.Types.AugmentedManifestsDocumentTypeFormat
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
import Network.AWS.Comprehend.Types.DocumentClassifierDataFormat
import Network.AWS.Comprehend.Types.DocumentClassifierFilter
import Network.AWS.Comprehend.Types.DocumentClassifierInputDataConfig
import Network.AWS.Comprehend.Types.DocumentClassifierMode
import Network.AWS.Comprehend.Types.DocumentClassifierOutputDataConfig
import Network.AWS.Comprehend.Types.DocumentClassifierProperties
import Network.AWS.Comprehend.Types.DocumentClassifierSummary
import Network.AWS.Comprehend.Types.DocumentLabel
import Network.AWS.Comprehend.Types.DocumentReadAction
import Network.AWS.Comprehend.Types.DocumentReadFeatureTypes
import Network.AWS.Comprehend.Types.DocumentReadMode
import Network.AWS.Comprehend.Types.DocumentReaderConfig
import Network.AWS.Comprehend.Types.DominantLanguage
import Network.AWS.Comprehend.Types.DominantLanguageDetectionJobFilter
import Network.AWS.Comprehend.Types.DominantLanguageDetectionJobProperties
import Network.AWS.Comprehend.Types.EndpointFilter
import Network.AWS.Comprehend.Types.EndpointProperties
import Network.AWS.Comprehend.Types.EndpointStatus
import Network.AWS.Comprehend.Types.EntitiesDetectionJobFilter
import Network.AWS.Comprehend.Types.EntitiesDetectionJobProperties
import Network.AWS.Comprehend.Types.Entity
import Network.AWS.Comprehend.Types.EntityLabel
import Network.AWS.Comprehend.Types.EntityRecognizerAnnotations
import Network.AWS.Comprehend.Types.EntityRecognizerDataFormat
import Network.AWS.Comprehend.Types.EntityRecognizerDocuments
import Network.AWS.Comprehend.Types.EntityRecognizerEntityList
import Network.AWS.Comprehend.Types.EntityRecognizerEvaluationMetrics
import Network.AWS.Comprehend.Types.EntityRecognizerFilter
import Network.AWS.Comprehend.Types.EntityRecognizerInputDataConfig
import Network.AWS.Comprehend.Types.EntityRecognizerMetadata
import Network.AWS.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem
import Network.AWS.Comprehend.Types.EntityRecognizerProperties
import Network.AWS.Comprehend.Types.EntityRecognizerSummary
import Network.AWS.Comprehend.Types.EntityType
import Network.AWS.Comprehend.Types.EntityTypesEvaluationMetrics
import Network.AWS.Comprehend.Types.EntityTypesListItem
import Network.AWS.Comprehend.Types.EventsDetectionJobFilter
import Network.AWS.Comprehend.Types.EventsDetectionJobProperties
import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.InputFormat
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.KeyPhrase
import Network.AWS.Comprehend.Types.KeyPhrasesDetectionJobFilter
import Network.AWS.Comprehend.Types.KeyPhrasesDetectionJobProperties
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.ModelStatus
import Network.AWS.Comprehend.Types.OutputDataConfig
import Network.AWS.Comprehend.Types.PartOfSpeechTag
import Network.AWS.Comprehend.Types.PartOfSpeechTagType
import Network.AWS.Comprehend.Types.PiiEntitiesDetectionJobFilter
import Network.AWS.Comprehend.Types.PiiEntitiesDetectionJobProperties
import Network.AWS.Comprehend.Types.PiiEntitiesDetectionMaskMode
import Network.AWS.Comprehend.Types.PiiEntitiesDetectionMode
import Network.AWS.Comprehend.Types.PiiEntity
import Network.AWS.Comprehend.Types.PiiEntityType
import Network.AWS.Comprehend.Types.PiiOutputDataConfig
import Network.AWS.Comprehend.Types.RedactionConfig
import Network.AWS.Comprehend.Types.SentimentDetectionJobFilter
import Network.AWS.Comprehend.Types.SentimentDetectionJobProperties
import Network.AWS.Comprehend.Types.SentimentScore
import Network.AWS.Comprehend.Types.SentimentType
import Network.AWS.Comprehend.Types.Split
import Network.AWS.Comprehend.Types.SyntaxLanguageCode
import Network.AWS.Comprehend.Types.SyntaxToken
import Network.AWS.Comprehend.Types.Tag
import Network.AWS.Comprehend.Types.TopicsDetectionJobFilter
import Network.AWS.Comprehend.Types.TopicsDetectionJobProperties
import Network.AWS.Comprehend.Types.VpcConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-11-27@ of the Amazon Comprehend SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Comprehend",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "comprehend",
      Core._serviceSigningName = "comprehend",
      Core._serviceVersion = "2017-11-27",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "Comprehend",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified resource is not available. Check the resource and try your
-- request again.
_ResourceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ResourceUnavailableException"

-- | The request contains more tags than can be associated with a resource
-- (50 tags per resource). The maximum number of tags includes both
-- existing tags and those included in your current request.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

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

-- | The number of documents in the request exceeds the limit of 25. Try your
-- request again with fewer documents.
_BatchSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BatchSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "BatchSizeLimitExceededException"

-- | Concurrent modification of the tags associated with an Amazon Comprehend
-- resource is not supported.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The request is invalid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | The size of the input text exceeds the limit. Use a smaller document.
_TextSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TextSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TextSizeLimitExceededException"

-- | The specified resource name is already in use. Use a different name and
-- try your request again.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The KMS customer managed key (CMK) entered cannot be validated. Verify
-- the key and re-enter it.
_KmsKeyValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KmsKeyValidationException =
  Core._MatchServiceError
    defaultService
    "KmsKeyValidationException"

-- | The specified resource ARN was not found. Check the ARN and try your
-- request again.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The request contains more tag keys than can be associated with a
-- resource (50 tag keys per resource).
_TooManyTagKeysException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagKeysException =
  Core._MatchServiceError
    defaultService
    "TooManyTagKeysException"

-- | The specified job was not found. Check the job ID and try again.
_JobNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_JobNotFoundException =
  Core._MatchServiceError
    defaultService
    "JobNotFoundException"

-- | Amazon Comprehend can\'t process the language of the input text. For
-- custom entity recognition APIs, only English, Spanish, French, Italian,
-- German, or Portuguese are accepted. For a list of supported languages,
-- see supported-languages.
_UnsupportedLanguageException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedLanguageException =
  Core._MatchServiceError
    defaultService
    "UnsupportedLanguageException"

-- | An internal server error occurred. Retry your request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The number of requests exceeds the limit. Resubmit your request later.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"

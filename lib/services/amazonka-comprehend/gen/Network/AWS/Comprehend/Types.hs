{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Comprehend.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceUnavailableException,
    _InvalidRequestException,
    _ResourceLimitExceededException,
    _TooManyTagsException,
    _TooManyRequestsException,
    _ConcurrentModificationException,
    _InternalServerException,
    _BatchSizeLimitExceededException,
    _UnsupportedLanguageException,
    _JobNotFoundException,
    _TooManyTagKeysException,
    _InvalidFilterException,
    _KmsKeyValidationException,
    _ResourceNotFoundException,
    _TextSizeLimitExceededException,
    _ResourceInUseException,

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
    augmentedManifestsListItem_sourceDocumentsS3Uri,
    augmentedManifestsListItem_documentType,
    augmentedManifestsListItem_split,
    augmentedManifestsListItem_annotationDataS3Uri,
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
    batchDetectSentimentItemResult_sentiment,
    batchDetectSentimentItemResult_sentimentScore,
    batchDetectSentimentItemResult_index,

    -- * BatchDetectSyntaxItemResult
    BatchDetectSyntaxItemResult (..),
    newBatchDetectSyntaxItemResult,
    batchDetectSyntaxItemResult_index,
    batchDetectSyntaxItemResult_syntaxTokens,

    -- * BatchItemError
    BatchItemError (..),
    newBatchItemError,
    batchItemError_errorCode,
    batchItemError_errorMessage,
    batchItemError_index,

    -- * ClassifierEvaluationMetrics
    ClassifierEvaluationMetrics (..),
    newClassifierEvaluationMetrics,
    classifierEvaluationMetrics_microPrecision,
    classifierEvaluationMetrics_microF1Score,
    classifierEvaluationMetrics_recall,
    classifierEvaluationMetrics_precision,
    classifierEvaluationMetrics_microRecall,
    classifierEvaluationMetrics_f1Score,
    classifierEvaluationMetrics_hammingLoss,
    classifierEvaluationMetrics_accuracy,

    -- * ClassifierMetadata
    ClassifierMetadata (..),
    newClassifierMetadata,
    classifierMetadata_numberOfLabels,
    classifierMetadata_evaluationMetrics,
    classifierMetadata_numberOfTrainedDocuments,
    classifierMetadata_numberOfTestDocuments,

    -- * DocumentClass
    DocumentClass (..),
    newDocumentClass,
    documentClass_score,
    documentClass_name,

    -- * DocumentClassificationJobFilter
    DocumentClassificationJobFilter (..),
    newDocumentClassificationJobFilter,
    documentClassificationJobFilter_submitTimeAfter,
    documentClassificationJobFilter_submitTimeBefore,
    documentClassificationJobFilter_jobName,
    documentClassificationJobFilter_jobStatus,

    -- * DocumentClassificationJobProperties
    DocumentClassificationJobProperties (..),
    newDocumentClassificationJobProperties,
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

    -- * DocumentClassifierFilter
    DocumentClassifierFilter (..),
    newDocumentClassifierFilter,
    documentClassifierFilter_status,
    documentClassifierFilter_submitTimeAfter,
    documentClassifierFilter_submitTimeBefore,
    documentClassifierFilter_documentClassifierName,

    -- * DocumentClassifierInputDataConfig
    DocumentClassifierInputDataConfig (..),
    newDocumentClassifierInputDataConfig,
    documentClassifierInputDataConfig_augmentedManifests,
    documentClassifierInputDataConfig_dataFormat,
    documentClassifierInputDataConfig_labelDelimiter,
    documentClassifierInputDataConfig_testS3Uri,
    documentClassifierInputDataConfig_s3Uri,

    -- * DocumentClassifierOutputDataConfig
    DocumentClassifierOutputDataConfig (..),
    newDocumentClassifierOutputDataConfig,
    documentClassifierOutputDataConfig_kmsKeyId,
    documentClassifierOutputDataConfig_s3Uri,

    -- * DocumentClassifierProperties
    DocumentClassifierProperties (..),
    newDocumentClassifierProperties,
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

    -- * DocumentClassifierSummary
    DocumentClassifierSummary (..),
    newDocumentClassifierSummary,
    documentClassifierSummary_latestVersionCreatedAt,
    documentClassifierSummary_documentClassifierName,
    documentClassifierSummary_latestVersionStatus,
    documentClassifierSummary_numberOfVersions,
    documentClassifierSummary_latestVersionName,

    -- * DocumentLabel
    DocumentLabel (..),
    newDocumentLabel,
    documentLabel_score,
    documentLabel_name,

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
    dominantLanguageDetectionJobFilter_submitTimeAfter,
    dominantLanguageDetectionJobFilter_submitTimeBefore,
    dominantLanguageDetectionJobFilter_jobName,
    dominantLanguageDetectionJobFilter_jobStatus,

    -- * DominantLanguageDetectionJobProperties
    DominantLanguageDetectionJobProperties (..),
    newDominantLanguageDetectionJobProperties,
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

    -- * EndpointFilter
    EndpointFilter (..),
    newEndpointFilter,
    endpointFilter_status,
    endpointFilter_modelArn,
    endpointFilter_creationTimeAfter,
    endpointFilter_creationTimeBefore,

    -- * EndpointProperties
    EndpointProperties (..),
    newEndpointProperties,
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

    -- * EntitiesDetectionJobFilter
    EntitiesDetectionJobFilter (..),
    newEntitiesDetectionJobFilter,
    entitiesDetectionJobFilter_submitTimeAfter,
    entitiesDetectionJobFilter_submitTimeBefore,
    entitiesDetectionJobFilter_jobName,
    entitiesDetectionJobFilter_jobStatus,

    -- * EntitiesDetectionJobProperties
    EntitiesDetectionJobProperties (..),
    newEntitiesDetectionJobProperties,
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

    -- * Entity
    Entity (..),
    newEntity,
    entity_beginOffset,
    entity_text,
    entity_score,
    entity_endOffset,
    entity_type,

    -- * EntityLabel
    EntityLabel (..),
    newEntityLabel,
    entityLabel_score,
    entityLabel_name,

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
    entityRecognizerEvaluationMetrics_recall,
    entityRecognizerEvaluationMetrics_precision,
    entityRecognizerEvaluationMetrics_f1Score,

    -- * EntityRecognizerFilter
    EntityRecognizerFilter (..),
    newEntityRecognizerFilter,
    entityRecognizerFilter_status,
    entityRecognizerFilter_submitTimeAfter,
    entityRecognizerFilter_submitTimeBefore,
    entityRecognizerFilter_recognizerName,

    -- * EntityRecognizerInputDataConfig
    EntityRecognizerInputDataConfig (..),
    newEntityRecognizerInputDataConfig,
    entityRecognizerInputDataConfig_augmentedManifests,
    entityRecognizerInputDataConfig_annotations,
    entityRecognizerInputDataConfig_dataFormat,
    entityRecognizerInputDataConfig_documents,
    entityRecognizerInputDataConfig_entityList,
    entityRecognizerInputDataConfig_entityTypes,

    -- * EntityRecognizerMetadata
    EntityRecognizerMetadata (..),
    newEntityRecognizerMetadata,
    entityRecognizerMetadata_entityTypes,
    entityRecognizerMetadata_evaluationMetrics,
    entityRecognizerMetadata_numberOfTrainedDocuments,
    entityRecognizerMetadata_numberOfTestDocuments,

    -- * EntityRecognizerMetadataEntityTypesListItem
    EntityRecognizerMetadataEntityTypesListItem (..),
    newEntityRecognizerMetadataEntityTypesListItem,
    entityRecognizerMetadataEntityTypesListItem_evaluationMetrics,
    entityRecognizerMetadataEntityTypesListItem_type,
    entityRecognizerMetadataEntityTypesListItem_numberOfTrainMentions,

    -- * EntityRecognizerProperties
    EntityRecognizerProperties (..),
    newEntityRecognizerProperties,
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

    -- * EntityRecognizerSummary
    EntityRecognizerSummary (..),
    newEntityRecognizerSummary,
    entityRecognizerSummary_latestVersionCreatedAt,
    entityRecognizerSummary_latestVersionStatus,
    entityRecognizerSummary_numberOfVersions,
    entityRecognizerSummary_latestVersionName,
    entityRecognizerSummary_recognizerName,

    -- * EntityTypesEvaluationMetrics
    EntityTypesEvaluationMetrics (..),
    newEntityTypesEvaluationMetrics,
    entityTypesEvaluationMetrics_recall,
    entityTypesEvaluationMetrics_precision,
    entityTypesEvaluationMetrics_f1Score,

    -- * EntityTypesListItem
    EntityTypesListItem (..),
    newEntityTypesListItem,
    entityTypesListItem_type,

    -- * EventsDetectionJobFilter
    EventsDetectionJobFilter (..),
    newEventsDetectionJobFilter,
    eventsDetectionJobFilter_submitTimeAfter,
    eventsDetectionJobFilter_submitTimeBefore,
    eventsDetectionJobFilter_jobName,
    eventsDetectionJobFilter_jobStatus,

    -- * EventsDetectionJobProperties
    EventsDetectionJobProperties (..),
    newEventsDetectionJobProperties,
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
    keyPhrase_text,
    keyPhrase_score,
    keyPhrase_endOffset,

    -- * KeyPhrasesDetectionJobFilter
    KeyPhrasesDetectionJobFilter (..),
    newKeyPhrasesDetectionJobFilter,
    keyPhrasesDetectionJobFilter_submitTimeAfter,
    keyPhrasesDetectionJobFilter_submitTimeBefore,
    keyPhrasesDetectionJobFilter_jobName,
    keyPhrasesDetectionJobFilter_jobStatus,

    -- * KeyPhrasesDetectionJobProperties
    KeyPhrasesDetectionJobProperties (..),
    newKeyPhrasesDetectionJobProperties,
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
    piiEntitiesDetectionJobFilter_submitTimeAfter,
    piiEntitiesDetectionJobFilter_submitTimeBefore,
    piiEntitiesDetectionJobFilter_jobName,
    piiEntitiesDetectionJobFilter_jobStatus,

    -- * PiiEntitiesDetectionJobProperties
    PiiEntitiesDetectionJobProperties (..),
    newPiiEntitiesDetectionJobProperties,
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

    -- * PiiEntity
    PiiEntity (..),
    newPiiEntity,
    piiEntity_beginOffset,
    piiEntity_score,
    piiEntity_endOffset,
    piiEntity_type,

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
    sentimentDetectionJobFilter_submitTimeAfter,
    sentimentDetectionJobFilter_submitTimeBefore,
    sentimentDetectionJobFilter_jobName,
    sentimentDetectionJobFilter_jobStatus,

    -- * SentimentDetectionJobProperties
    SentimentDetectionJobProperties (..),
    newSentimentDetectionJobProperties,
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
    syntaxToken_text,
    syntaxToken_tokenId,
    syntaxToken_endOffset,
    syntaxToken_partOfSpeech,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * TopicsDetectionJobFilter
    TopicsDetectionJobFilter (..),
    newTopicsDetectionJobFilter,
    topicsDetectionJobFilter_submitTimeAfter,
    topicsDetectionJobFilter_submitTimeBefore,
    topicsDetectionJobFilter_jobName,
    topicsDetectionJobFilter_jobStatus,

    -- * TopicsDetectionJobProperties
    TopicsDetectionJobProperties (..),
    newTopicsDetectionJobProperties,
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
import Amazonka.Comprehend.Types.TopicsDetectionJobFilter
import Amazonka.Comprehend.Types.TopicsDetectionJobProperties
import Amazonka.Comprehend.Types.VpcConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
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
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified resource is not available. Check the resource and try your
-- request again.
_ResourceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ResourceUnavailableException"

-- | The request is invalid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | The maximum number of resources per account has been exceeded. Review
-- the resources, and then try your request again.
_ResourceLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceededException"

-- | The request contains more tags than can be associated with a resource
-- (50 tags per resource). The maximum number of tags includes both
-- existing tags and those included in your current request.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The number of requests exceeds the limit. Resubmit your request later.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"

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

-- | The number of documents in the request exceeds the limit of 25. Try your
-- request again with fewer documents.
_BatchSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BatchSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "BatchSizeLimitExceededException"

-- | Amazon Comprehend can\'t process the language of the input text. For
-- custom entity recognition APIs, only English, Spanish, French, Italian,
-- German, or Portuguese are accepted. For a list of supported languages,
-- see supported-languages.
_UnsupportedLanguageException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedLanguageException =
  Core._MatchServiceError
    defaultService
    "UnsupportedLanguageException"

-- | The specified job was not found. Check the job ID and try again.
_JobNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_JobNotFoundException =
  Core._MatchServiceError
    defaultService
    "JobNotFoundException"

-- | The request contains more tag keys than can be associated with a
-- resource (50 tag keys per resource).
_TooManyTagKeysException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagKeysException =
  Core._MatchServiceError
    defaultService
    "TooManyTagKeysException"

-- | The filter specified for the operation is invalid. Specify a different
-- filter.
_InvalidFilterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFilterException =
  Core._MatchServiceError
    defaultService
    "InvalidFilterException"

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

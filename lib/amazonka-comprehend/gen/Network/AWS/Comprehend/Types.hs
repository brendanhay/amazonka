-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types
  ( -- * Service configuration
    comprehendService,

    -- * Errors

    -- * DocumentClassifierDataFormat
    DocumentClassifierDataFormat (..),

    -- * DocumentClassifierMode
    DocumentClassifierMode (..),

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

    -- * SyntaxLanguageCode
    SyntaxLanguageCode (..),

    -- * AugmentedManifestsListItem
    AugmentedManifestsListItem (..),
    mkAugmentedManifestsListItem,
    amliS3URI,
    amliAttributeNames,

    -- * BatchDetectDominantLanguageItemResult
    BatchDetectDominantLanguageItemResult (..),
    mkBatchDetectDominantLanguageItemResult,
    bddlirLanguages,
    bddlirIndex,

    -- * BatchDetectEntitiesItemResult
    BatchDetectEntitiesItemResult (..),
    mkBatchDetectEntitiesItemResult,
    bdeirEntities,
    bdeirIndex,

    -- * BatchDetectKeyPhrasesItemResult
    BatchDetectKeyPhrasesItemResult (..),
    mkBatchDetectKeyPhrasesItemResult,
    bdkpirIndex,
    bdkpirKeyPhrases,

    -- * BatchDetectSentimentItemResult
    BatchDetectSentimentItemResult (..),
    mkBatchDetectSentimentItemResult,
    bSentiment,
    bSentimentScore,
    bIndex,

    -- * BatchDetectSyntaxItemResult
    BatchDetectSyntaxItemResult (..),
    mkBatchDetectSyntaxItemResult,
    bdsirIndex,
    bdsirSyntaxTokens,

    -- * BatchItemError
    BatchItemError (..),
    mkBatchItemError,
    bieErrorCode,
    bieErrorMessage,
    bieIndex,

    -- * ClassifierEvaluationMetrics
    ClassifierEvaluationMetrics (..),
    mkClassifierEvaluationMetrics,
    cemMicroPrecision,
    cemMicroF1Score,
    cemRecall,
    cemPrecision,
    cemMicroRecall,
    cemF1Score,
    cemHammingLoss,
    cemAccuracy,

    -- * ClassifierMetadata
    ClassifierMetadata (..),
    mkClassifierMetadata,
    cmNumberOfLabels,
    cmEvaluationMetrics,
    cmNumberOfTrainedDocuments,
    cmNumberOfTestDocuments,

    -- * DocumentClass
    DocumentClass (..),
    mkDocumentClass,
    dcScore,
    dcName,

    -- * DocumentClassificationJobFilter
    DocumentClassificationJobFilter (..),
    mkDocumentClassificationJobFilter,
    dcjfSubmitTimeAfter,
    dcjfSubmitTimeBefore,
    dcjfJobName,
    dcjfJobStatus,

    -- * DocumentClassificationJobProperties
    DocumentClassificationJobProperties (..),
    mkDocumentClassificationJobProperties,
    dcjpJobId,
    dcjpDocumentClassifierARN,
    dcjpJobName,
    dcjpInputDataConfig,
    dcjpVPCConfig,
    dcjpVolumeKMSKeyId,
    dcjpEndTime,
    dcjpOutputDataConfig,
    dcjpDataAccessRoleARN,
    dcjpJobStatus,
    dcjpMessage,
    dcjpSubmitTime,

    -- * DocumentClassifierFilter
    DocumentClassifierFilter (..),
    mkDocumentClassifierFilter,
    dcfStatus,
    dcfSubmitTimeAfter,
    dcfSubmitTimeBefore,

    -- * DocumentClassifierInputDataConfig
    DocumentClassifierInputDataConfig (..),
    mkDocumentClassifierInputDataConfig,
    dcidcAugmentedManifests,
    dcidcDataFormat,
    dcidcLabelDelimiter,
    dcidcS3URI,

    -- * DocumentClassifierOutputDataConfig
    DocumentClassifierOutputDataConfig (..),
    mkDocumentClassifierOutputDataConfig,
    dcodcKMSKeyId,
    dcodcS3URI,

    -- * DocumentClassifierProperties
    DocumentClassifierProperties (..),
    mkDocumentClassifierProperties,
    dcpStatus,
    dcpLanguageCode,
    dcpClassifierMetadata,
    dcpTrainingEndTime,
    dcpDocumentClassifierARN,
    dcpMode,
    dcpInputDataConfig,
    dcpVPCConfig,
    dcpVolumeKMSKeyId,
    dcpEndTime,
    dcpOutputDataConfig,
    dcpTrainingStartTime,
    dcpDataAccessRoleARN,
    dcpMessage,
    dcpSubmitTime,

    -- * DocumentLabel
    DocumentLabel (..),
    mkDocumentLabel,
    dScore,
    dName,

    -- * DominantLanguage
    DominantLanguage (..),
    mkDominantLanguage,
    dlLanguageCode,
    dlScore,

    -- * DominantLanguageDetectionJobFilter
    DominantLanguageDetectionJobFilter (..),
    mkDominantLanguageDetectionJobFilter,
    dldjfSubmitTimeAfter,
    dldjfSubmitTimeBefore,
    dldjfJobName,
    dldjfJobStatus,

    -- * DominantLanguageDetectionJobProperties
    DominantLanguageDetectionJobProperties (..),
    mkDominantLanguageDetectionJobProperties,
    dldjpJobId,
    dldjpJobName,
    dldjpInputDataConfig,
    dldjpVPCConfig,
    dldjpVolumeKMSKeyId,
    dldjpEndTime,
    dldjpOutputDataConfig,
    dldjpDataAccessRoleARN,
    dldjpJobStatus,
    dldjpMessage,
    dldjpSubmitTime,

    -- * EndpointFilter
    EndpointFilter (..),
    mkEndpointFilter,
    efStatus,
    efModelARN,
    efCreationTimeAfter,
    efCreationTimeBefore,

    -- * EndpointProperties
    EndpointProperties (..),
    mkEndpointProperties,
    epCreationTime,
    epStatus,
    epModelARN,
    epLastModifiedTime,
    epDesiredInferenceUnits,
    epCurrentInferenceUnits,
    epMessage,
    epEndpointARN,

    -- * EntitiesDetectionJobFilter
    EntitiesDetectionJobFilter (..),
    mkEntitiesDetectionJobFilter,
    edjfSubmitTimeAfter,
    edjfSubmitTimeBefore,
    edjfJobName,
    edjfJobStatus,

    -- * EntitiesDetectionJobProperties
    EntitiesDetectionJobProperties (..),
    mkEntitiesDetectionJobProperties,
    edjpsLanguageCode,
    edjpsJobId,
    edjpsEntityRecognizerARN,
    edjpsJobName,
    edjpsInputDataConfig,
    edjpsVPCConfig,
    edjpsVolumeKMSKeyId,
    edjpsEndTime,
    edjpsOutputDataConfig,
    edjpsDataAccessRoleARN,
    edjpsJobStatus,
    edjpsMessage,
    edjpsSubmitTime,

    -- * Entity
    Entity (..),
    mkEntity,
    eBeginOffset,
    eText,
    eScore,
    eEndOffset,
    eType,

    -- * EntityRecognizerAnnotations
    EntityRecognizerAnnotations (..),
    mkEntityRecognizerAnnotations,
    eraS3URI,

    -- * EntityRecognizerDocuments
    EntityRecognizerDocuments (..),
    mkEntityRecognizerDocuments,
    erdS3URI,

    -- * EntityRecognizerEntityList
    EntityRecognizerEntityList (..),
    mkEntityRecognizerEntityList,
    erelS3URI,

    -- * EntityRecognizerEvaluationMetrics
    EntityRecognizerEvaluationMetrics (..),
    mkEntityRecognizerEvaluationMetrics,
    eremRecall,
    eremPrecision,
    eremF1Score,

    -- * EntityRecognizerFilter
    EntityRecognizerFilter (..),
    mkEntityRecognizerFilter,
    erfStatus,
    erfSubmitTimeAfter,
    erfSubmitTimeBefore,

    -- * EntityRecognizerInputDataConfig
    EntityRecognizerInputDataConfig (..),
    mkEntityRecognizerInputDataConfig,
    eridcAugmentedManifests,
    eridcAnnotations,
    eridcDataFormat,
    eridcDocuments,
    eridcEntityList,
    eridcEntityTypes,

    -- * EntityRecognizerMetadata
    EntityRecognizerMetadata (..),
    mkEntityRecognizerMetadata,
    ermEntityTypes,
    ermEvaluationMetrics,
    ermNumberOfTrainedDocuments,
    ermNumberOfTestDocuments,

    -- * EntityRecognizerMetadataEntityTypesListItem
    EntityRecognizerMetadataEntityTypesListItem (..),
    mkEntityRecognizerMetadataEntityTypesListItem,
    ermetliEvaluationMetrics,
    ermetliType,
    ermetliNumberOfTrainMentions,

    -- * EntityRecognizerProperties
    EntityRecognizerProperties (..),
    mkEntityRecognizerProperties,
    erpStatus,
    erpLanguageCode,
    erpTrainingEndTime,
    erpEntityRecognizerARN,
    erpInputDataConfig,
    erpVPCConfig,
    erpVolumeKMSKeyId,
    erpEndTime,
    erpTrainingStartTime,
    erpDataAccessRoleARN,
    erpRecognizerMetadata,
    erpMessage,
    erpSubmitTime,

    -- * EntityTypesEvaluationMetrics
    EntityTypesEvaluationMetrics (..),
    mkEntityTypesEvaluationMetrics,
    etemRecall,
    etemPrecision,
    etemF1Score,

    -- * EntityTypesListItem
    EntityTypesListItem (..),
    mkEntityTypesListItem,
    etliType,

    -- * EventsDetectionJobFilter
    EventsDetectionJobFilter (..),
    mkEventsDetectionJobFilter,
    eSubmitTimeAfter,
    eSubmitTimeBefore,
    eJobName,
    eJobStatus,

    -- * EventsDetectionJobProperties
    EventsDetectionJobProperties (..),
    mkEventsDetectionJobProperties,
    edjpLanguageCode,
    edjpJobId,
    edjpJobName,
    edjpTargetEventTypes,
    edjpInputDataConfig,
    edjpEndTime,
    edjpOutputDataConfig,
    edjpDataAccessRoleARN,
    edjpJobStatus,
    edjpMessage,
    edjpSubmitTime,

    -- * InputDataConfig
    InputDataConfig (..),
    mkInputDataConfig,
    idcInputFormat,
    idcS3URI,

    -- * KeyPhrase
    KeyPhrase (..),
    mkKeyPhrase,
    kpBeginOffset,
    kpText,
    kpScore,
    kpEndOffset,

    -- * KeyPhrasesDetectionJobFilter
    KeyPhrasesDetectionJobFilter (..),
    mkKeyPhrasesDetectionJobFilter,
    kpdjfSubmitTimeAfter,
    kpdjfSubmitTimeBefore,
    kpdjfJobName,
    kpdjfJobStatus,

    -- * KeyPhrasesDetectionJobProperties
    KeyPhrasesDetectionJobProperties (..),
    mkKeyPhrasesDetectionJobProperties,
    kpdjpLanguageCode,
    kpdjpJobId,
    kpdjpJobName,
    kpdjpInputDataConfig,
    kpdjpVPCConfig,
    kpdjpVolumeKMSKeyId,
    kpdjpEndTime,
    kpdjpOutputDataConfig,
    kpdjpDataAccessRoleARN,
    kpdjpJobStatus,
    kpdjpMessage,
    kpdjpSubmitTime,

    -- * OutputDataConfig
    OutputDataConfig (..),
    mkOutputDataConfig,
    odcKMSKeyId,
    odcS3URI,

    -- * PartOfSpeechTag
    PartOfSpeechTag (..),
    mkPartOfSpeechTag,
    postTag,
    postScore,

    -- * PiiEntitiesDetectionJobFilter
    PiiEntitiesDetectionJobFilter (..),
    mkPiiEntitiesDetectionJobFilter,
    pedjfSubmitTimeAfter,
    pedjfSubmitTimeBefore,
    pedjfJobName,
    pedjfJobStatus,

    -- * PiiEntitiesDetectionJobProperties
    PiiEntitiesDetectionJobProperties (..),
    mkPiiEntitiesDetectionJobProperties,
    pedjpLanguageCode,
    pedjpJobId,
    pedjpJobName,
    pedjpMode,
    pedjpInputDataConfig,
    pedjpRedactionConfig,
    pedjpEndTime,
    pedjpOutputDataConfig,
    pedjpDataAccessRoleARN,
    pedjpJobStatus,
    pedjpMessage,
    pedjpSubmitTime,

    -- * PiiEntity
    PiiEntity (..),
    mkPiiEntity,
    peBeginOffset,
    peScore,
    peEndOffset,
    peType,

    -- * PiiOutputDataConfig
    PiiOutputDataConfig (..),
    mkPiiOutputDataConfig,
    podcKMSKeyId,
    podcS3URI,

    -- * RedactionConfig
    RedactionConfig (..),
    mkRedactionConfig,
    rcMaskCharacter,
    rcMaskMode,
    rcPiiEntityTypes,

    -- * SentimentDetectionJobFilter
    SentimentDetectionJobFilter (..),
    mkSentimentDetectionJobFilter,
    sdjfSubmitTimeAfter,
    sdjfSubmitTimeBefore,
    sdjfJobName,
    sdjfJobStatus,

    -- * SentimentDetectionJobProperties
    SentimentDetectionJobProperties (..),
    mkSentimentDetectionJobProperties,
    sdjpLanguageCode,
    sdjpJobId,
    sdjpJobName,
    sdjpInputDataConfig,
    sdjpVPCConfig,
    sdjpVolumeKMSKeyId,
    sdjpEndTime,
    sdjpOutputDataConfig,
    sdjpDataAccessRoleARN,
    sdjpJobStatus,
    sdjpMessage,
    sdjpSubmitTime,

    -- * SentimentScore
    SentimentScore (..),
    mkSentimentScore,
    ssMixed,
    ssNegative,
    ssNeutral,
    ssPositive,

    -- * SyntaxToken
    SyntaxToken (..),
    mkSyntaxToken,
    stBeginOffset,
    stText,
    stTokenId,
    stEndOffset,
    stPartOfSpeech,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * TopicsDetectionJobFilter
    TopicsDetectionJobFilter (..),
    mkTopicsDetectionJobFilter,
    tdjfSubmitTimeAfter,
    tdjfSubmitTimeBefore,
    tdjfJobName,
    tdjfJobStatus,

    -- * TopicsDetectionJobProperties
    TopicsDetectionJobProperties (..),
    mkTopicsDetectionJobProperties,
    tdjpJobId,
    tdjpJobName,
    tdjpInputDataConfig,
    tdjpVPCConfig,
    tdjpVolumeKMSKeyId,
    tdjpEndTime,
    tdjpOutputDataConfig,
    tdjpDataAccessRoleARN,
    tdjpNumberOfTopics,
    tdjpJobStatus,
    tdjpMessage,
    tdjpSubmitTime,

    -- * VPCConfig
    VPCConfig (..),
    mkVPCConfig,
    vcSecurityGroupIds,
    vcSubnets,
  )
where

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
import Network.AWS.Comprehend.Types.DocumentLabel
import Network.AWS.Comprehend.Types.DominantLanguage
import Network.AWS.Comprehend.Types.DominantLanguageDetectionJobFilter
import Network.AWS.Comprehend.Types.DominantLanguageDetectionJobProperties
import Network.AWS.Comprehend.Types.EndpointFilter
import Network.AWS.Comprehend.Types.EndpointProperties
import Network.AWS.Comprehend.Types.EndpointStatus
import Network.AWS.Comprehend.Types.EntitiesDetectionJobFilter
import Network.AWS.Comprehend.Types.EntitiesDetectionJobProperties
import Network.AWS.Comprehend.Types.Entity
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
import Network.AWS.Comprehend.Types.SyntaxLanguageCode
import Network.AWS.Comprehend.Types.SyntaxToken
import Network.AWS.Comprehend.Types.Tag
import Network.AWS.Comprehend.Types.TopicsDetectionJobFilter
import Network.AWS.Comprehend.Types.TopicsDetectionJobProperties
import Network.AWS.Comprehend.Types.VPCConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-11-27@ of the Amazon Comprehend SDK configuration.
comprehendService :: Lude.Service
comprehendService =
  Lude.Service
    { Lude._svcAbbrev = "Comprehend",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "comprehend",
      Lude._svcVersion = "2017-11-27",
      Lude._svcEndpoint = Lude.defaultEndpoint comprehendService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Comprehend",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing

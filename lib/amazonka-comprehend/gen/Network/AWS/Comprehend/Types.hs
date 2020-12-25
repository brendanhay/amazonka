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
    mkServiceConfig,

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

    -- * EntityRecognizerEntityList
    EntityRecognizerEntityList (..),
    mkEntityRecognizerEntityList,
    erelS3Uri,

    -- * IamRoleArn
    IamRoleArn (..),

    -- * EntityRecognizerEvaluationMetrics
    EntityRecognizerEvaluationMetrics (..),
    mkEntityRecognizerEvaluationMetrics,
    eremF1Score,
    eremPrecision,
    eremRecall,

    -- * PartOfSpeechTag
    PartOfSpeechTag (..),
    mkPartOfSpeechTag,
    postScore,
    postTag,

    -- * EntityRecognizerProperties
    EntityRecognizerProperties (..),
    mkEntityRecognizerProperties,
    erpDataAccessRoleArn,
    erpEndTime,
    erpEntityRecognizerArn,
    erpInputDataConfig,
    erpLanguageCode,
    erpMessage,
    erpRecognizerMetadata,
    erpStatus,
    erpSubmitTime,
    erpTrainingEndTime,
    erpTrainingStartTime,
    erpVolumeKmsKeyId,
    erpVpcConfig,

    -- * PiiEntitiesDetectionJobProperties
    PiiEntitiesDetectionJobProperties (..),
    mkPiiEntitiesDetectionJobProperties,
    pedjpDataAccessRoleArn,
    pedjpEndTime,
    pedjpInputDataConfig,
    pedjpJobId,
    pedjpJobName,
    pedjpJobStatus,
    pedjpLanguageCode,
    pedjpMessage,
    pedjpMode,
    pedjpOutputDataConfig,
    pedjpRedactionConfig,
    pedjpSubmitTime,

    -- * EventsDetectionJobFilter
    EventsDetectionJobFilter (..),
    mkEventsDetectionJobFilter,
    edjfJobName,
    edjfJobStatus,
    edjfSubmitTimeAfter,
    edjfSubmitTimeBefore,

    -- * AnyLengthString
    AnyLengthString (..),

    -- * AttributeNamesListItem
    AttributeNamesListItem (..),

    -- * BatchDetectDominantLanguageItemResult
    BatchDetectDominantLanguageItemResult (..),
    mkBatchDetectDominantLanguageItemResult,
    bddlirIndex,
    bddlirLanguages,

    -- * CustomerInputString
    CustomerInputString (..),

    -- * LanguageCode
    LanguageCode (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * EndpointProperties
    EndpointProperties (..),
    mkEndpointProperties,
    epCreationTime,
    epCurrentInferenceUnits,
    epDesiredInferenceUnits,
    epEndpointArn,
    epLastModifiedTime,
    epMessage,
    epModelArn,
    epStatus,

    -- * KeyPhrase
    KeyPhrase (..),
    mkKeyPhrase,
    kpBeginOffset,
    kpEndOffset,
    kpScore,
    kpText,

    -- * SyntaxToken
    SyntaxToken (..),
    mkSyntaxToken,
    stBeginOffset,
    stEndOffset,
    stPartOfSpeech,
    stText,
    stTokenId,

    -- * BatchDetectEntitiesItemResult
    BatchDetectEntitiesItemResult (..),
    mkBatchDetectEntitiesItemResult,
    bdeirEntities,
    bdeirIndex,

    -- * ClassifierMetadata
    ClassifierMetadata (..),
    mkClassifierMetadata,
    cmEvaluationMetrics,
    cmNumberOfLabels,
    cmNumberOfTestDocuments,
    cmNumberOfTrainedDocuments,

    -- * JobId
    JobId (..),

    -- * DocumentClassifierArn
    DocumentClassifierArn (..),

    -- * MaskCharacter
    MaskCharacter (..),

    -- * String
    String (..),

    -- * EntityRecognizerFilter
    EntityRecognizerFilter (..),
    mkEntityRecognizerFilter,
    erfStatus,
    erfSubmitTimeAfter,
    erfSubmitTimeBefore,

    -- * PiiEntitiesDetectionJobFilter
    PiiEntitiesDetectionJobFilter (..),
    mkPiiEntitiesDetectionJobFilter,
    pedjfJobName,
    pedjfJobStatus,
    pedjfSubmitTimeAfter,
    pedjfSubmitTimeBefore,

    -- * PiiOutputDataConfig
    PiiOutputDataConfig (..),
    mkPiiOutputDataConfig,
    podcS3Uri,
    podcKmsKeyId,

    -- * ComprehendEndpointName
    ComprehendEndpointName (..),

    -- * DominantLanguage
    DominantLanguage (..),
    mkDominantLanguage,
    dlLanguageCode,
    dlScore,

    -- * EntityRecognizerArn
    EntityRecognizerArn (..),

    -- * EntityRecognizerMetadata
    EntityRecognizerMetadata (..),
    mkEntityRecognizerMetadata,
    ermEntityTypes,
    ermEvaluationMetrics,
    ermNumberOfTestDocuments,
    ermNumberOfTrainedDocuments,

    -- * JobName
    JobName (..),

    -- * KeyPhrasesDetectionJobProperties
    KeyPhrasesDetectionJobProperties (..),
    mkKeyPhrasesDetectionJobProperties,
    kpdjpDataAccessRoleArn,
    kpdjpEndTime,
    kpdjpInputDataConfig,
    kpdjpJobId,
    kpdjpJobName,
    kpdjpJobStatus,
    kpdjpLanguageCode,
    kpdjpMessage,
    kpdjpOutputDataConfig,
    kpdjpSubmitTime,
    kpdjpVolumeKmsKeyId,
    kpdjpVpcConfig,

    -- * EntityType
    EntityType (..),

    -- * DocumentClassifierMode
    DocumentClassifierMode (..),

    -- * ComprehendModelArn
    ComprehendModelArn (..),

    -- * PiiEntity
    PiiEntity (..),
    mkPiiEntity,
    peBeginOffset,
    peEndOffset,
    peScore,
    peType,

    -- * DocumentClassifierDataFormat
    DocumentClassifierDataFormat (..),

    -- * EndpointFilter
    EndpointFilter (..),
    mkEndpointFilter,
    efCreationTimeAfter,
    efCreationTimeBefore,
    efModelArn,
    efStatus,

    -- * BatchDetectSyntaxItemResult
    BatchDetectSyntaxItemResult (..),
    mkBatchDetectSyntaxItemResult,
    bdsirIndex,
    bdsirSyntaxTokens,

    -- * ClientRequestTokenString
    ClientRequestTokenString (..),

    -- * DocumentClassificationJobFilter
    DocumentClassificationJobFilter (..),
    mkDocumentClassificationJobFilter,
    dcjfJobName,
    dcjfJobStatus,
    dcjfSubmitTimeAfter,
    dcjfSubmitTimeBefore,

    -- * EntityRecognizerDataFormat
    EntityRecognizerDataFormat (..),

    -- * SyntaxLanguageCode
    SyntaxLanguageCode (..),

    -- * SentimentDetectionJobProperties
    SentimentDetectionJobProperties (..),
    mkSentimentDetectionJobProperties,
    sdjpDataAccessRoleArn,
    sdjpEndTime,
    sdjpInputDataConfig,
    sdjpJobId,
    sdjpJobName,
    sdjpJobStatus,
    sdjpLanguageCode,
    sdjpMessage,
    sdjpOutputDataConfig,
    sdjpSubmitTime,
    sdjpVolumeKmsKeyId,
    sdjpVpcConfig,

    -- * SubnetId
    SubnetId (..),

    -- * BatchDetectKeyPhrasesItemResult
    BatchDetectKeyPhrasesItemResult (..),
    mkBatchDetectKeyPhrasesItemResult,
    bdkpirIndex,
    bdkpirKeyPhrases,

    -- * EntityTypesEvaluationMetrics
    EntityTypesEvaluationMetrics (..),
    mkEntityTypesEvaluationMetrics,
    etemF1Score,
    etemPrecision,
    etemRecall,

    -- * PiiEntitiesDetectionMode
    PiiEntitiesDetectionMode (..),

    -- * BatchItemError
    BatchItemError (..),
    mkBatchItemError,
    bieErrorCode,
    bieErrorMessage,
    bieIndex,

    -- * KeyPhrasesDetectionJobFilter
    KeyPhrasesDetectionJobFilter (..),
    mkKeyPhrasesDetectionJobFilter,
    kpdjfJobName,
    kpdjfJobStatus,
    kpdjfSubmitTimeAfter,
    kpdjfSubmitTimeBefore,

    -- * DocumentClass
    DocumentClass (..),
    mkDocumentClass,
    dcName,
    dcScore,

    -- * EntityRecognizerAnnotations
    EntityRecognizerAnnotations (..),
    mkEntityRecognizerAnnotations,
    eraS3Uri,

    -- * DocumentLabel
    DocumentLabel (..),
    mkDocumentLabel,
    dName,
    dScore,

    -- * SecurityGroupId
    SecurityGroupId (..),

    -- * EntitiesDetectionJobProperties
    EntitiesDetectionJobProperties (..),
    mkEntitiesDetectionJobProperties,
    eDataAccessRoleArn,
    eEndTime,
    eEntityRecognizerArn,
    eInputDataConfig,
    eJobId,
    eJobName,
    eJobStatus,
    eLanguageCode,
    eMessage,
    eOutputDataConfig,
    eSubmitTime,
    eVolumeKmsKeyId,
    eVpcConfig,

    -- * ComprehendArn
    ComprehendArn (..),

    -- * PartOfSpeechTagType
    PartOfSpeechTagType (..),

    -- * EntityTypeName
    EntityTypeName (..),

    -- * InputDataConfig
    InputDataConfig (..),
    mkInputDataConfig,
    idcS3Uri,
    idcInputFormat,

    -- * DocumentClassifierInputDataConfig
    DocumentClassifierInputDataConfig (..),
    mkDocumentClassifierInputDataConfig,
    dcidcAugmentedManifests,
    dcidcDataFormat,
    dcidcLabelDelimiter,
    dcidcS3Uri,

    -- * VpcConfig
    VpcConfig (..),
    mkVpcConfig,
    vcSecurityGroupIds,
    vcSubnets,

    -- * EntityTypesListItem
    EntityTypesListItem (..),
    mkEntityTypesListItem,
    etliType,

    -- * RedactionConfig
    RedactionConfig (..),
    mkRedactionConfig,
    rcMaskCharacter,
    rcMaskMode,
    rcPiiEntityTypes,

    -- * SentimentType
    SentimentType (..),

    -- * KmsKeyId
    KmsKeyId (..),

    -- * EntityRecognizerMetadataEntityTypesListItem
    EntityRecognizerMetadataEntityTypesListItem (..),
    mkEntityRecognizerMetadataEntityTypesListItem,
    ermetliEvaluationMetrics,
    ermetliNumberOfTrainMentions,
    ermetliType,

    -- * ComprehendEndpointArn
    ComprehendEndpointArn (..),

    -- * SentimentDetectionJobFilter
    SentimentDetectionJobFilter (..),
    mkSentimentDetectionJobFilter,
    sdjfJobName,
    sdjfJobStatus,
    sdjfSubmitTimeAfter,
    sdjfSubmitTimeBefore,

    -- * SentimentScore
    SentimentScore (..),
    mkSentimentScore,
    ssMixed,
    ssNegative,
    ssNeutral,
    ssPositive,

    -- * DocumentClassificationJobProperties
    DocumentClassificationJobProperties (..),
    mkDocumentClassificationJobProperties,
    dcjpDataAccessRoleArn,
    dcjpDocumentClassifierArn,
    dcjpEndTime,
    dcjpInputDataConfig,
    dcjpJobId,
    dcjpJobName,
    dcjpJobStatus,
    dcjpMessage,
    dcjpOutputDataConfig,
    dcjpSubmitTime,
    dcjpVolumeKmsKeyId,
    dcjpVpcConfig,

    -- * EndpointStatus
    EndpointStatus (..),

    -- * TopicsDetectionJobFilter
    TopicsDetectionJobFilter (..),
    mkTopicsDetectionJobFilter,
    tdjfJobName,
    tdjfJobStatus,
    tdjfSubmitTimeAfter,
    tdjfSubmitTimeBefore,

    -- * InputFormat
    InputFormat (..),

    -- * EntitiesDetectionJobFilter
    EntitiesDetectionJobFilter (..),
    mkEntitiesDetectionJobFilter,
    edjffJobName,
    edjffJobStatus,
    edjffSubmitTimeAfter,
    edjffSubmitTimeBefore,

    -- * OutputDataConfig
    OutputDataConfig (..),
    mkOutputDataConfig,
    odcS3Uri,
    odcKmsKeyId,

    -- * PiiEntitiesDetectionMaskMode
    PiiEntitiesDetectionMaskMode (..),

    -- * LabelDelimiter
    LabelDelimiter (..),

    -- * DocumentClassifierOutputDataConfig
    DocumentClassifierOutputDataConfig (..),
    mkDocumentClassifierOutputDataConfig,
    dcodcKmsKeyId,
    dcodcS3Uri,

    -- * TagKey
    TagKey (..),

    -- * DocumentClassifierFilter
    DocumentClassifierFilter (..),
    mkDocumentClassifierFilter,
    dcfStatus,
    dcfSubmitTimeAfter,
    dcfSubmitTimeBefore,

    -- * DominantLanguageDetectionJobFilter
    DominantLanguageDetectionJobFilter (..),
    mkDominantLanguageDetectionJobFilter,
    dldjfJobName,
    dldjfJobStatus,
    dldjfSubmitTimeAfter,
    dldjfSubmitTimeBefore,

    -- * BatchDetectSentimentItemResult
    BatchDetectSentimentItemResult (..),
    mkBatchDetectSentimentItemResult,
    bIndex,
    bSentiment,
    bSentimentScore,

    -- * JobStatus
    JobStatus (..),

    -- * TopicsDetectionJobProperties
    TopicsDetectionJobProperties (..),
    mkTopicsDetectionJobProperties,
    tdjpDataAccessRoleArn,
    tdjpEndTime,
    tdjpInputDataConfig,
    tdjpJobId,
    tdjpJobName,
    tdjpJobStatus,
    tdjpMessage,
    tdjpNumberOfTopics,
    tdjpOutputDataConfig,
    tdjpSubmitTime,
    tdjpVolumeKmsKeyId,
    tdjpVpcConfig,

    -- * AugmentedManifestsListItem
    AugmentedManifestsListItem (..),
    mkAugmentedManifestsListItem,
    amliS3Uri,
    amliAttributeNames,

    -- * Entity
    Entity (..),
    mkEntity,
    eBeginOffset,
    eEndOffset,
    eScore,
    eText,
    eType,

    -- * PiiEntityType
    PiiEntityType (..),

    -- * ModelStatus
    ModelStatus (..),

    -- * S3Uri
    S3Uri (..),

    -- * EventsDetectionJobProperties
    EventsDetectionJobProperties (..),
    mkEventsDetectionJobProperties,
    edjpDataAccessRoleArn,
    edjpEndTime,
    edjpInputDataConfig,
    edjpJobId,
    edjpJobName,
    edjpJobStatus,
    edjpLanguageCode,
    edjpMessage,
    edjpOutputDataConfig,
    edjpSubmitTime,
    edjpTargetEventTypes,

    -- * ClassifierEvaluationMetrics
    ClassifierEvaluationMetrics (..),
    mkClassifierEvaluationMetrics,
    cemAccuracy,
    cemF1Score,
    cemHammingLoss,
    cemMicroF1Score,
    cemMicroPrecision,
    cemMicroRecall,
    cemPrecision,
    cemRecall,

    -- * EventTypeString
    EventTypeString (..),

    -- * DominantLanguageDetectionJobProperties
    DominantLanguageDetectionJobProperties (..),
    mkDominantLanguageDetectionJobProperties,
    dldjpDataAccessRoleArn,
    dldjpEndTime,
    dldjpInputDataConfig,
    dldjpJobId,
    dldjpJobName,
    dldjpJobStatus,
    dldjpMessage,
    dldjpOutputDataConfig,
    dldjpSubmitTime,
    dldjpVolumeKmsKeyId,
    dldjpVpcConfig,

    -- * EntityRecognizerInputDataConfig
    EntityRecognizerInputDataConfig (..),
    mkEntityRecognizerInputDataConfig,
    eridcEntityTypes,
    eridcAnnotations,
    eridcAugmentedManifests,
    eridcDataFormat,
    eridcDocuments,
    eridcEntityList,

    -- * EntityRecognizerDocuments
    EntityRecognizerDocuments (..),
    mkEntityRecognizerDocuments,
    erdS3Uri,

    -- * DocumentClassifierProperties
    DocumentClassifierProperties (..),
    mkDocumentClassifierProperties,
    dcpClassifierMetadata,
    dcpDataAccessRoleArn,
    dcpDocumentClassifierArn,
    dcpEndTime,
    dcpInputDataConfig,
    dcpLanguageCode,
    dcpMessage,
    dcpMode,
    dcpOutputDataConfig,
    dcpStatus,
    dcpSubmitTime,
    dcpTrainingEndTime,
    dcpTrainingStartTime,
    dcpVolumeKmsKeyId,
    dcpVpcConfig,

    -- * DataAccessRoleArn
    DataAccessRoleArn (..),

    -- * ClientRequestToken
    ClientRequestToken (..),

    -- * VolumeKmsKeyId
    VolumeKmsKeyId (..),

    -- * EndpointArn
    EndpointArn (..),

    -- * Message
    Message (..),

    -- * NextToken
    NextToken (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * ModelArn
    ModelArn (..),

    -- * Text
    Text (..),

    -- * DocumentClassifierName
    DocumentClassifierName (..),

    -- * RecognizerName
    RecognizerName (..),

    -- * ResourceArn
    ResourceArn (..),
  )
where

import Network.AWS.Comprehend.Types.AnyLengthString
import Network.AWS.Comprehend.Types.AttributeNamesListItem
import Network.AWS.Comprehend.Types.AugmentedManifestsListItem
import Network.AWS.Comprehend.Types.BatchDetectDominantLanguageItemResult
import Network.AWS.Comprehend.Types.BatchDetectEntitiesItemResult
import Network.AWS.Comprehend.Types.BatchDetectKeyPhrasesItemResult
import Network.AWS.Comprehend.Types.BatchDetectSentimentItemResult
import Network.AWS.Comprehend.Types.BatchDetectSyntaxItemResult
import Network.AWS.Comprehend.Types.BatchItemError
import Network.AWS.Comprehend.Types.ClassifierEvaluationMetrics
import Network.AWS.Comprehend.Types.ClassifierMetadata
import Network.AWS.Comprehend.Types.ClientRequestToken
import Network.AWS.Comprehend.Types.ClientRequestTokenString
import Network.AWS.Comprehend.Types.ComprehendArn
import Network.AWS.Comprehend.Types.ComprehendEndpointArn
import Network.AWS.Comprehend.Types.ComprehendEndpointName
import Network.AWS.Comprehend.Types.ComprehendModelArn
import Network.AWS.Comprehend.Types.CustomerInputString
import Network.AWS.Comprehend.Types.DataAccessRoleArn
import Network.AWS.Comprehend.Types.DocumentClass
import Network.AWS.Comprehend.Types.DocumentClassificationJobFilter
import Network.AWS.Comprehend.Types.DocumentClassificationJobProperties
import Network.AWS.Comprehend.Types.DocumentClassifierArn
import Network.AWS.Comprehend.Types.DocumentClassifierDataFormat
import Network.AWS.Comprehend.Types.DocumentClassifierFilter
import Network.AWS.Comprehend.Types.DocumentClassifierInputDataConfig
import Network.AWS.Comprehend.Types.DocumentClassifierMode
import Network.AWS.Comprehend.Types.DocumentClassifierName
import Network.AWS.Comprehend.Types.DocumentClassifierOutputDataConfig
import Network.AWS.Comprehend.Types.DocumentClassifierProperties
import Network.AWS.Comprehend.Types.DocumentLabel
import Network.AWS.Comprehend.Types.DominantLanguage
import Network.AWS.Comprehend.Types.DominantLanguageDetectionJobFilter
import Network.AWS.Comprehend.Types.DominantLanguageDetectionJobProperties
import Network.AWS.Comprehend.Types.EndpointArn
import Network.AWS.Comprehend.Types.EndpointFilter
import Network.AWS.Comprehend.Types.EndpointProperties
import Network.AWS.Comprehend.Types.EndpointStatus
import Network.AWS.Comprehend.Types.EntitiesDetectionJobFilter
import Network.AWS.Comprehend.Types.EntitiesDetectionJobProperties
import Network.AWS.Comprehend.Types.Entity
import Network.AWS.Comprehend.Types.EntityRecognizerAnnotations
import Network.AWS.Comprehend.Types.EntityRecognizerArn
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
import Network.AWS.Comprehend.Types.EntityTypeName
import Network.AWS.Comprehend.Types.EntityTypesEvaluationMetrics
import Network.AWS.Comprehend.Types.EntityTypesListItem
import Network.AWS.Comprehend.Types.EventTypeString
import Network.AWS.Comprehend.Types.EventsDetectionJobFilter
import Network.AWS.Comprehend.Types.EventsDetectionJobProperties
import Network.AWS.Comprehend.Types.IamRoleArn
import Network.AWS.Comprehend.Types.InputDataConfig
import Network.AWS.Comprehend.Types.InputFormat
import Network.AWS.Comprehend.Types.JobId
import Network.AWS.Comprehend.Types.JobName
import Network.AWS.Comprehend.Types.JobStatus
import Network.AWS.Comprehend.Types.Key
import Network.AWS.Comprehend.Types.KeyPhrase
import Network.AWS.Comprehend.Types.KeyPhrasesDetectionJobFilter
import Network.AWS.Comprehend.Types.KeyPhrasesDetectionJobProperties
import Network.AWS.Comprehend.Types.KmsKeyId
import Network.AWS.Comprehend.Types.LabelDelimiter
import Network.AWS.Comprehend.Types.LanguageCode
import Network.AWS.Comprehend.Types.MaskCharacter
import Network.AWS.Comprehend.Types.Message
import Network.AWS.Comprehend.Types.ModelArn
import Network.AWS.Comprehend.Types.ModelStatus
import Network.AWS.Comprehend.Types.NextToken
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
import Network.AWS.Comprehend.Types.RecognizerName
import Network.AWS.Comprehend.Types.RedactionConfig
import Network.AWS.Comprehend.Types.ResourceArn
import Network.AWS.Comprehend.Types.S3Uri
import Network.AWS.Comprehend.Types.SecurityGroupId
import Network.AWS.Comprehend.Types.SentimentDetectionJobFilter
import Network.AWS.Comprehend.Types.SentimentDetectionJobProperties
import Network.AWS.Comprehend.Types.SentimentScore
import Network.AWS.Comprehend.Types.SentimentType
import Network.AWS.Comprehend.Types.String
import Network.AWS.Comprehend.Types.SubnetId
import Network.AWS.Comprehend.Types.SyntaxLanguageCode
import Network.AWS.Comprehend.Types.SyntaxToken
import Network.AWS.Comprehend.Types.Tag
import Network.AWS.Comprehend.Types.TagKey
import Network.AWS.Comprehend.Types.Text
import Network.AWS.Comprehend.Types.TopicsDetectionJobFilter
import Network.AWS.Comprehend.Types.TopicsDetectionJobProperties
import Network.AWS.Comprehend.Types.Value
import Network.AWS.Comprehend.Types.VolumeKmsKeyId
import Network.AWS.Comprehend.Types.VpcConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-11-27@ of the Amazon Comprehend SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "Comprehend",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "comprehend",
      Core._svcVersion = "2017-11-27",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "Comprehend",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The specified resource is not available. Check the resource and try your request again.
_ResourceUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceUnavailableException"
{-# DEPRECATED _ResourceUnavailableException "Use generic-lens or generic-optics instead." #-}

-- | The request is invalid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError mkServiceConfig "InvalidRequestException"
{-# DEPRECATED _InvalidRequestException "Use generic-lens or generic-optics instead." #-}

-- | The maximum number of resources per account has been exceeded. Review the resources, and then try your request again.
_ResourceLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceLimitExceededException"
{-# DEPRECATED _ResourceLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | The request contains more tags than can be associated with a resource (50 tags per resource). The maximum number of tags includes both existing tags and those included in your current request.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError mkServiceConfig "TooManyTagsException"
{-# DEPRECATED _TooManyTagsException "Use generic-lens or generic-optics instead." #-}

-- | The number of requests exceeds the limit. Resubmit your request later.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    mkServiceConfig
    "TooManyRequestsException"
{-# DEPRECATED _TooManyRequestsException "Use generic-lens or generic-optics instead." #-}

-- | Concurrent modification of the tags associated with an Amazon Comprehend resource is not supported.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    mkServiceConfig
    "ConcurrentModificationException"
{-# DEPRECATED _ConcurrentModificationException "Use generic-lens or generic-optics instead." #-}

-- | An internal server error occurred. Retry your request.
_InternalServerException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError mkServiceConfig "InternalServerException"
{-# DEPRECATED _InternalServerException "Use generic-lens or generic-optics instead." #-}

-- | The number of documents in the request exceeds the limit of 25. Try your request again with fewer documents.
_BatchSizeLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BatchSizeLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "BatchSizeLimitExceededException"
{-# DEPRECATED _BatchSizeLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | Amazon Comprehend can't process the language of the input text. For all custom entity recognition APIs (such as @CreateEntityRecognizer@ ), only English, Spanish, French, Italian, German, or Portuguese are accepted. For most other APIs, such as those for Custom Classification, Amazon Comprehend accepts text in all supported languages. For a list of supported languages, see 'supported-languages' .
_UnsupportedLanguageException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedLanguageException =
  Core._MatchServiceError
    mkServiceConfig
    "UnsupportedLanguageException"
{-# DEPRECATED _UnsupportedLanguageException "Use generic-lens or generic-optics instead." #-}

-- | The specified job was not found. Check the job ID and try again.
_JobNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_JobNotFoundException =
  Core._MatchServiceError mkServiceConfig "JobNotFoundException"
{-# DEPRECATED _JobNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The request contains more tag keys than can be associated with a resource (50 tag keys per resource).
_TooManyTagKeysException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagKeysException =
  Core._MatchServiceError mkServiceConfig "TooManyTagKeysException"
{-# DEPRECATED _TooManyTagKeysException "Use generic-lens or generic-optics instead." #-}

-- | The filter specified for the operation is invalid. Specify a different filter.
_InvalidFilterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidFilterException =
  Core._MatchServiceError mkServiceConfig "InvalidFilterException"
{-# DEPRECATED _InvalidFilterException "Use generic-lens or generic-optics instead." #-}

-- | The KMS customer managed key (CMK) entered cannot be validated. Verify the key and re-enter it.
_KmsKeyValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_KmsKeyValidationException =
  Core._MatchServiceError
    mkServiceConfig
    "KmsKeyValidationException"
{-# DEPRECATED _KmsKeyValidationException "Use generic-lens or generic-optics instead." #-}

-- | The specified resource ARN was not found. Check the ARN and try your request again.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceNotFoundException"
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The size of the input text exceeds the limit. Use a smaller document.
_TextSizeLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TextSizeLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "TextSizeLimitExceededException"
{-# DEPRECATED _TextSizeLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | The specified resource name is already in use. Use a different name and try your request again.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError mkServiceConfig "ResourceInUseException"
{-# DEPRECATED _ResourceInUseException "Use generic-lens or generic-optics instead." #-}

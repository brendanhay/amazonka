{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Comprehend is an AWS service for gaining insight into the content of documents. Use these actions to determine the topics contained in your documents, the topics they discuss, the predominant sentiment expressed in them, the predominant language used, and more.
module Network.AWS.Comprehend
  ( -- * Service configuration
    comprehendService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchDetectSentiment
    module Network.AWS.Comprehend.BatchDetectSentiment,

    -- ** DeleteEntityRecognizer
    module Network.AWS.Comprehend.DeleteEntityRecognizer,

    -- ** DescribeKeyPhrasesDetectionJob
    module Network.AWS.Comprehend.DescribeKeyPhrasesDetectionJob,

    -- ** ListEntitiesDetectionJobs (Paginated)
    module Network.AWS.Comprehend.ListEntitiesDetectionJobs,

    -- ** CreateEndpoint
    module Network.AWS.Comprehend.CreateEndpoint,

    -- ** StopEventsDetectionJob
    module Network.AWS.Comprehend.StopEventsDetectionJob,

    -- ** StartSentimentDetectionJob
    module Network.AWS.Comprehend.StartSentimentDetectionJob,

    -- ** BatchDetectSyntax
    module Network.AWS.Comprehend.BatchDetectSyntax,

    -- ** StartTopicsDetectionJob
    module Network.AWS.Comprehend.StartTopicsDetectionJob,

    -- ** DescribeEventsDetectionJob
    module Network.AWS.Comprehend.DescribeEventsDetectionJob,

    -- ** DeleteEndpoint
    module Network.AWS.Comprehend.DeleteEndpoint,

    -- ** UpdateEndpoint
    module Network.AWS.Comprehend.UpdateEndpoint,

    -- ** ListTagsForResource
    module Network.AWS.Comprehend.ListTagsForResource,

    -- ** BatchDetectKeyPhrases
    module Network.AWS.Comprehend.BatchDetectKeyPhrases,

    -- ** DescribeSentimentDetectionJob
    module Network.AWS.Comprehend.DescribeSentimentDetectionJob,

    -- ** StartEntitiesDetectionJob
    module Network.AWS.Comprehend.StartEntitiesDetectionJob,

    -- ** StopPiiEntitiesDetectionJob
    module Network.AWS.Comprehend.StopPiiEntitiesDetectionJob,

    -- ** DescribeEntityRecognizer
    module Network.AWS.Comprehend.DescribeEntityRecognizer,

    -- ** DetectSentiment
    module Network.AWS.Comprehend.DetectSentiment,

    -- ** StartDominantLanguageDetectionJob
    module Network.AWS.Comprehend.StartDominantLanguageDetectionJob,

    -- ** StopTrainingDocumentClassifier
    module Network.AWS.Comprehend.StopTrainingDocumentClassifier,

    -- ** DescribeDocumentClassificationJob
    module Network.AWS.Comprehend.DescribeDocumentClassificationJob,

    -- ** ListEventsDetectionJobs
    module Network.AWS.Comprehend.ListEventsDetectionJobs,

    -- ** BatchDetectEntities
    module Network.AWS.Comprehend.BatchDetectEntities,

    -- ** CreateEntityRecognizer
    module Network.AWS.Comprehend.CreateEntityRecognizer,

    -- ** StopKeyPhrasesDetectionJob
    module Network.AWS.Comprehend.StopKeyPhrasesDetectionJob,

    -- ** CreateDocumentClassifier
    module Network.AWS.Comprehend.CreateDocumentClassifier,

    -- ** ListPiiEntitiesDetectionJobs
    module Network.AWS.Comprehend.ListPiiEntitiesDetectionJobs,

    -- ** ListEntityRecognizers (Paginated)
    module Network.AWS.Comprehend.ListEntityRecognizers,

    -- ** StopSentimentDetectionJob
    module Network.AWS.Comprehend.StopSentimentDetectionJob,

    -- ** DetectDominantLanguage
    module Network.AWS.Comprehend.DetectDominantLanguage,

    -- ** ClassifyDocument
    module Network.AWS.Comprehend.ClassifyDocument,

    -- ** StartEventsDetectionJob
    module Network.AWS.Comprehend.StartEventsDetectionJob,

    -- ** DescribeTopicsDetectionJob
    module Network.AWS.Comprehend.DescribeTopicsDetectionJob,

    -- ** ListDocumentClassificationJobs (Paginated)
    module Network.AWS.Comprehend.ListDocumentClassificationJobs,

    -- ** DetectPiiEntities
    module Network.AWS.Comprehend.DetectPiiEntities,

    -- ** ListEndpoints
    module Network.AWS.Comprehend.ListEndpoints,

    -- ** DetectEntities
    module Network.AWS.Comprehend.DetectEntities,

    -- ** DescribeDocumentClassifier
    module Network.AWS.Comprehend.DescribeDocumentClassifier,

    -- ** DescribeDominantLanguageDetectionJob
    module Network.AWS.Comprehend.DescribeDominantLanguageDetectionJob,

    -- ** StopEntitiesDetectionJob
    module Network.AWS.Comprehend.StopEntitiesDetectionJob,

    -- ** StopTrainingEntityRecognizer
    module Network.AWS.Comprehend.StopTrainingEntityRecognizer,

    -- ** StartPiiEntitiesDetectionJob
    module Network.AWS.Comprehend.StartPiiEntitiesDetectionJob,

    -- ** ListKeyPhrasesDetectionJobs (Paginated)
    module Network.AWS.Comprehend.ListKeyPhrasesDetectionJobs,

    -- ** DescribeEntitiesDetectionJob
    module Network.AWS.Comprehend.DescribeEntitiesDetectionJob,

    -- ** StopDominantLanguageDetectionJob
    module Network.AWS.Comprehend.StopDominantLanguageDetectionJob,

    -- ** TagResource
    module Network.AWS.Comprehend.TagResource,

    -- ** DescribePiiEntitiesDetectionJob
    module Network.AWS.Comprehend.DescribePiiEntitiesDetectionJob,

    -- ** ListTopicsDetectionJobs (Paginated)
    module Network.AWS.Comprehend.ListTopicsDetectionJobs,

    -- ** UntagResource
    module Network.AWS.Comprehend.UntagResource,

    -- ** BatchDetectDominantLanguage
    module Network.AWS.Comprehend.BatchDetectDominantLanguage,

    -- ** StartDocumentClassificationJob
    module Network.AWS.Comprehend.StartDocumentClassificationJob,

    -- ** DetectKeyPhrases
    module Network.AWS.Comprehend.DetectKeyPhrases,

    -- ** DetectSyntax
    module Network.AWS.Comprehend.DetectSyntax,

    -- ** DescribeEndpoint
    module Network.AWS.Comprehend.DescribeEndpoint,

    -- ** ListSentimentDetectionJobs (Paginated)
    module Network.AWS.Comprehend.ListSentimentDetectionJobs,

    -- ** DeleteDocumentClassifier
    module Network.AWS.Comprehend.DeleteDocumentClassifier,

    -- ** ListDominantLanguageDetectionJobs (Paginated)
    module Network.AWS.Comprehend.ListDominantLanguageDetectionJobs,

    -- ** StartKeyPhrasesDetectionJob
    module Network.AWS.Comprehend.StartKeyPhrasesDetectionJob,

    -- ** ListDocumentClassifiers (Paginated)
    module Network.AWS.Comprehend.ListDocumentClassifiers,

    -- * Types

    -- ** DocumentClassifierDataFormat
    DocumentClassifierDataFormat (..),

    -- ** DocumentClassifierMode
    DocumentClassifierMode (..),

    -- ** EndpointStatus
    EndpointStatus (..),

    -- ** EntityRecognizerDataFormat
    EntityRecognizerDataFormat (..),

    -- ** EntityType
    EntityType (..),

    -- ** InputFormat
    InputFormat (..),

    -- ** JobStatus
    JobStatus (..),

    -- ** LanguageCode
    LanguageCode (..),

    -- ** ModelStatus
    ModelStatus (..),

    -- ** PartOfSpeechTagType
    PartOfSpeechTagType (..),

    -- ** PiiEntitiesDetectionMaskMode
    PiiEntitiesDetectionMaskMode (..),

    -- ** PiiEntitiesDetectionMode
    PiiEntitiesDetectionMode (..),

    -- ** PiiEntityType
    PiiEntityType (..),

    -- ** SentimentType
    SentimentType (..),

    -- ** SyntaxLanguageCode
    SyntaxLanguageCode (..),

    -- ** AugmentedManifestsListItem
    AugmentedManifestsListItem (..),
    mkAugmentedManifestsListItem,
    amliAttributeNames,
    amliS3URI,

    -- ** BatchDetectDominantLanguageItemResult
    BatchDetectDominantLanguageItemResult (..),
    mkBatchDetectDominantLanguageItemResult,
    bddlirLanguages,
    bddlirIndex,

    -- ** BatchDetectEntitiesItemResult
    BatchDetectEntitiesItemResult (..),
    mkBatchDetectEntitiesItemResult,
    bdeirEntities,
    bdeirIndex,

    -- ** BatchDetectKeyPhrasesItemResult
    BatchDetectKeyPhrasesItemResult (..),
    mkBatchDetectKeyPhrasesItemResult,
    bdkpirIndex,
    bdkpirKeyPhrases,

    -- ** BatchDetectSentimentItemResult
    BatchDetectSentimentItemResult (..),
    mkBatchDetectSentimentItemResult,
    bSentiment,
    bSentimentScore,
    bIndex,

    -- ** BatchDetectSyntaxItemResult
    BatchDetectSyntaxItemResult (..),
    mkBatchDetectSyntaxItemResult,
    bdsirIndex,
    bdsirSyntaxTokens,

    -- ** BatchItemError
    BatchItemError (..),
    mkBatchItemError,
    bieErrorCode,
    bieErrorMessage,
    bieIndex,

    -- ** ClassifierEvaluationMetrics
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

    -- ** ClassifierMetadata
    ClassifierMetadata (..),
    mkClassifierMetadata,
    cmNumberOfLabels,
    cmEvaluationMetrics,
    cmNumberOfTrainedDocuments,
    cmNumberOfTestDocuments,

    -- ** DocumentClass
    DocumentClass (..),
    mkDocumentClass,
    dcScore,
    dcName,

    -- ** DocumentClassificationJobFilter
    DocumentClassificationJobFilter (..),
    mkDocumentClassificationJobFilter,
    dcjfSubmitTimeAfter,
    dcjfSubmitTimeBefore,
    dcjfJobName,
    dcjfJobStatus,

    -- ** DocumentClassificationJobProperties
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

    -- ** DocumentClassifierFilter
    DocumentClassifierFilter (..),
    mkDocumentClassifierFilter,
    dcfStatus,
    dcfSubmitTimeAfter,
    dcfSubmitTimeBefore,

    -- ** DocumentClassifierInputDataConfig
    DocumentClassifierInputDataConfig (..),
    mkDocumentClassifierInputDataConfig,
    dcidcAugmentedManifests,
    dcidcDataFormat,
    dcidcLabelDelimiter,
    dcidcS3URI,

    -- ** DocumentClassifierOutputDataConfig
    DocumentClassifierOutputDataConfig (..),
    mkDocumentClassifierOutputDataConfig,
    dcodcKMSKeyId,
    dcodcS3URI,

    -- ** DocumentClassifierProperties
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

    -- ** DocumentLabel
    DocumentLabel (..),
    mkDocumentLabel,
    dScore,
    dName,

    -- ** DominantLanguage
    DominantLanguage (..),
    mkDominantLanguage,
    dlLanguageCode,
    dlScore,

    -- ** DominantLanguageDetectionJobFilter
    DominantLanguageDetectionJobFilter (..),
    mkDominantLanguageDetectionJobFilter,
    dldjfSubmitTimeAfter,
    dldjfSubmitTimeBefore,
    dldjfJobName,
    dldjfJobStatus,

    -- ** DominantLanguageDetectionJobProperties
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

    -- ** EndpointFilter
    EndpointFilter (..),
    mkEndpointFilter,
    efStatus,
    efModelARN,
    efCreationTimeAfter,
    efCreationTimeBefore,

    -- ** EndpointProperties
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

    -- ** EntitiesDetectionJobFilter
    EntitiesDetectionJobFilter (..),
    mkEntitiesDetectionJobFilter,
    edjffSubmitTimeAfter,
    edjffSubmitTimeBefore,
    edjffJobName,
    edjffJobStatus,

    -- ** EntitiesDetectionJobProperties
    EntitiesDetectionJobProperties (..),
    mkEntitiesDetectionJobProperties,
    eLanguageCode,
    eJobId,
    eEntityRecognizerARN,
    eJobName,
    eInputDataConfig,
    eVPCConfig,
    eVolumeKMSKeyId,
    eEndTime,
    eOutputDataConfig,
    eDataAccessRoleARN,
    eJobStatus,
    eMessage,
    eSubmitTime,

    -- ** Entity
    Entity (..),
    mkEntity,
    eBeginOffset,
    eText,
    eScore,
    eEndOffset,
    eType,

    -- ** EntityRecognizerAnnotations
    EntityRecognizerAnnotations (..),
    mkEntityRecognizerAnnotations,
    eraS3URI,

    -- ** EntityRecognizerDocuments
    EntityRecognizerDocuments (..),
    mkEntityRecognizerDocuments,
    erdS3URI,

    -- ** EntityRecognizerEntityList
    EntityRecognizerEntityList (..),
    mkEntityRecognizerEntityList,
    erelS3URI,

    -- ** EntityRecognizerEvaluationMetrics
    EntityRecognizerEvaluationMetrics (..),
    mkEntityRecognizerEvaluationMetrics,
    eremRecall,
    eremPrecision,
    eremF1Score,

    -- ** EntityRecognizerFilter
    EntityRecognizerFilter (..),
    mkEntityRecognizerFilter,
    erfStatus,
    erfSubmitTimeAfter,
    erfSubmitTimeBefore,

    -- ** EntityRecognizerInputDataConfig
    EntityRecognizerInputDataConfig (..),
    mkEntityRecognizerInputDataConfig,
    eridcAugmentedManifests,
    eridcAnnotations,
    eridcEntityTypes,
    eridcDataFormat,
    eridcDocuments,
    eridcEntityList,

    -- ** EntityRecognizerMetadata
    EntityRecognizerMetadata (..),
    mkEntityRecognizerMetadata,
    ermEntityTypes,
    ermEvaluationMetrics,
    ermNumberOfTrainedDocuments,
    ermNumberOfTestDocuments,

    -- ** EntityRecognizerMetadataEntityTypesListItem
    EntityRecognizerMetadataEntityTypesListItem (..),
    mkEntityRecognizerMetadataEntityTypesListItem,
    ermetliEvaluationMetrics,
    ermetliType,
    ermetliNumberOfTrainMentions,

    -- ** EntityRecognizerProperties
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

    -- ** EntityTypesEvaluationMetrics
    EntityTypesEvaluationMetrics (..),
    mkEntityTypesEvaluationMetrics,
    etemRecall,
    etemPrecision,
    etemF1Score,

    -- ** EntityTypesListItem
    EntityTypesListItem (..),
    mkEntityTypesListItem,
    etliType,

    -- ** EventsDetectionJobFilter
    EventsDetectionJobFilter (..),
    mkEventsDetectionJobFilter,
    edjfSubmitTimeAfter,
    edjfSubmitTimeBefore,
    edjfJobName,
    edjfJobStatus,

    -- ** EventsDetectionJobProperties
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

    -- ** InputDataConfig
    InputDataConfig (..),
    mkInputDataConfig,
    idcInputFormat,
    idcS3URI,

    -- ** KeyPhrase
    KeyPhrase (..),
    mkKeyPhrase,
    kpBeginOffset,
    kpText,
    kpScore,
    kpEndOffset,

    -- ** KeyPhrasesDetectionJobFilter
    KeyPhrasesDetectionJobFilter (..),
    mkKeyPhrasesDetectionJobFilter,
    kpdjfSubmitTimeAfter,
    kpdjfSubmitTimeBefore,
    kpdjfJobName,
    kpdjfJobStatus,

    -- ** KeyPhrasesDetectionJobProperties
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

    -- ** OutputDataConfig
    OutputDataConfig (..),
    mkOutputDataConfig,
    odcKMSKeyId,
    odcS3URI,

    -- ** PartOfSpeechTag
    PartOfSpeechTag (..),
    mkPartOfSpeechTag,
    postTag,
    postScore,

    -- ** PiiEntitiesDetectionJobFilter
    PiiEntitiesDetectionJobFilter (..),
    mkPiiEntitiesDetectionJobFilter,
    pedjfSubmitTimeAfter,
    pedjfSubmitTimeBefore,
    pedjfJobName,
    pedjfJobStatus,

    -- ** PiiEntitiesDetectionJobProperties
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

    -- ** PiiEntity
    PiiEntity (..),
    mkPiiEntity,
    peBeginOffset,
    peScore,
    peEndOffset,
    peType,

    -- ** PiiOutputDataConfig
    PiiOutputDataConfig (..),
    mkPiiOutputDataConfig,
    podcKMSKeyId,
    podcS3URI,

    -- ** RedactionConfig
    RedactionConfig (..),
    mkRedactionConfig,
    rcMaskCharacter,
    rcMaskMode,
    rcPiiEntityTypes,

    -- ** SentimentDetectionJobFilter
    SentimentDetectionJobFilter (..),
    mkSentimentDetectionJobFilter,
    sdjfSubmitTimeAfter,
    sdjfSubmitTimeBefore,
    sdjfJobName,
    sdjfJobStatus,

    -- ** SentimentDetectionJobProperties
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

    -- ** SentimentScore
    SentimentScore (..),
    mkSentimentScore,
    ssMixed,
    ssNegative,
    ssNeutral,
    ssPositive,

    -- ** SyntaxToken
    SyntaxToken (..),
    mkSyntaxToken,
    stBeginOffset,
    stText,
    stTokenId,
    stEndOffset,
    stPartOfSpeech,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** TopicsDetectionJobFilter
    TopicsDetectionJobFilter (..),
    mkTopicsDetectionJobFilter,
    tdjfSubmitTimeAfter,
    tdjfSubmitTimeBefore,
    tdjfJobName,
    tdjfJobStatus,

    -- ** TopicsDetectionJobProperties
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

    -- ** VPCConfig
    VPCConfig (..),
    mkVPCConfig,
    vcSecurityGroupIds,
    vcSubnets,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
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
import Network.AWS.Comprehend.Types
import Network.AWS.Comprehend.UntagResource
import Network.AWS.Comprehend.UpdateEndpoint
import Network.AWS.Comprehend.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Comprehend'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

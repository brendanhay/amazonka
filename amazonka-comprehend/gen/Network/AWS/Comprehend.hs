{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Comprehend is an AWS service for gaining insight into the content
-- of documents. Use these actions to determine the topics contained in
-- your documents, the topics they discuss, the predominant sentiment
-- expressed in them, the predominant language used, and more.
module Network.AWS.Comprehend
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ResourceUnavailableException
    _ResourceUnavailableException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** InvalidFilterException
    _InvalidFilterException,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** BatchSizeLimitExceededException
    _BatchSizeLimitExceededException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** TextSizeLimitExceededException
    _TextSizeLimitExceededException,

    -- ** KmsKeyValidationException
    _KmsKeyValidationException,

    -- ** TooManyTagKeysException
    _TooManyTagKeysException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** JobNotFoundException
    _JobNotFoundException,

    -- ** UnsupportedLanguageException
    _UnsupportedLanguageException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** StopSentimentDetectionJob
    StopSentimentDetectionJob (StopSentimentDetectionJob'),
    newStopSentimentDetectionJob,
    StopSentimentDetectionJobResponse (StopSentimentDetectionJobResponse'),
    newStopSentimentDetectionJobResponse,

    -- ** StartEventsDetectionJob
    StartEventsDetectionJob (StartEventsDetectionJob'),
    newStartEventsDetectionJob,
    StartEventsDetectionJobResponse (StartEventsDetectionJobResponse'),
    newStartEventsDetectionJobResponse,

    -- ** CreateEndpoint
    CreateEndpoint (CreateEndpoint'),
    newCreateEndpoint,
    CreateEndpointResponse (CreateEndpointResponse'),
    newCreateEndpointResponse,

    -- ** StartSentimentDetectionJob
    StartSentimentDetectionJob (StartSentimentDetectionJob'),
    newStartSentimentDetectionJob,
    StartSentimentDetectionJobResponse (StartSentimentDetectionJobResponse'),
    newStartSentimentDetectionJobResponse,

    -- ** StopEventsDetectionJob
    StopEventsDetectionJob (StopEventsDetectionJob'),
    newStopEventsDetectionJob,
    StopEventsDetectionJobResponse (StopEventsDetectionJobResponse'),
    newStopEventsDetectionJobResponse,

    -- ** ListEntityRecognizers (Paginated)
    ListEntityRecognizers (ListEntityRecognizers'),
    newListEntityRecognizers,
    ListEntityRecognizersResponse (ListEntityRecognizersResponse'),
    newListEntityRecognizersResponse,

    -- ** BatchDetectSentiment
    BatchDetectSentiment (BatchDetectSentiment'),
    newBatchDetectSentiment,
    BatchDetectSentimentResponse (BatchDetectSentimentResponse'),
    newBatchDetectSentimentResponse,

    -- ** DeleteEntityRecognizer
    DeleteEntityRecognizer (DeleteEntityRecognizer'),
    newDeleteEntityRecognizer,
    DeleteEntityRecognizerResponse (DeleteEntityRecognizerResponse'),
    newDeleteEntityRecognizerResponse,

    -- ** DescribeKeyPhrasesDetectionJob
    DescribeKeyPhrasesDetectionJob (DescribeKeyPhrasesDetectionJob'),
    newDescribeKeyPhrasesDetectionJob,
    DescribeKeyPhrasesDetectionJobResponse (DescribeKeyPhrasesDetectionJobResponse'),
    newDescribeKeyPhrasesDetectionJobResponse,

    -- ** ListDominantLanguageDetectionJobs (Paginated)
    ListDominantLanguageDetectionJobs (ListDominantLanguageDetectionJobs'),
    newListDominantLanguageDetectionJobs,
    ListDominantLanguageDetectionJobsResponse (ListDominantLanguageDetectionJobsResponse'),
    newListDominantLanguageDetectionJobsResponse,

    -- ** StopKeyPhrasesDetectionJob
    StopKeyPhrasesDetectionJob (StopKeyPhrasesDetectionJob'),
    newStopKeyPhrasesDetectionJob,
    StopKeyPhrasesDetectionJobResponse (StopKeyPhrasesDetectionJobResponse'),
    newStopKeyPhrasesDetectionJobResponse,

    -- ** ListDocumentClassifiers (Paginated)
    ListDocumentClassifiers (ListDocumentClassifiers'),
    newListDocumentClassifiers,
    ListDocumentClassifiersResponse (ListDocumentClassifiersResponse'),
    newListDocumentClassifiersResponse,

    -- ** CreateEntityRecognizer
    CreateEntityRecognizer (CreateEntityRecognizer'),
    newCreateEntityRecognizer,
    CreateEntityRecognizerResponse (CreateEntityRecognizerResponse'),
    newCreateEntityRecognizerResponse,

    -- ** StartKeyPhrasesDetectionJob
    StartKeyPhrasesDetectionJob (StartKeyPhrasesDetectionJob'),
    newStartKeyPhrasesDetectionJob,
    StartKeyPhrasesDetectionJobResponse (StartKeyPhrasesDetectionJobResponse'),
    newStartKeyPhrasesDetectionJobResponse,

    -- ** ListEventsDetectionJobs
    ListEventsDetectionJobs (ListEventsDetectionJobs'),
    newListEventsDetectionJobs,
    ListEventsDetectionJobsResponse (ListEventsDetectionJobsResponse'),
    newListEventsDetectionJobsResponse,

    -- ** ListSentimentDetectionJobs (Paginated)
    ListSentimentDetectionJobs (ListSentimentDetectionJobs'),
    newListSentimentDetectionJobs,
    ListSentimentDetectionJobsResponse (ListSentimentDetectionJobsResponse'),
    newListSentimentDetectionJobsResponse,

    -- ** DetectSyntax
    DetectSyntax (DetectSyntax'),
    newDetectSyntax,
    DetectSyntaxResponse (DetectSyntaxResponse'),
    newDetectSyntaxResponse,

    -- ** StartDocumentClassificationJob
    StartDocumentClassificationJob (StartDocumentClassificationJob'),
    newStartDocumentClassificationJob,
    StartDocumentClassificationJobResponse (StartDocumentClassificationJobResponse'),
    newStartDocumentClassificationJobResponse,

    -- ** DetectKeyPhrases
    DetectKeyPhrases (DetectKeyPhrases'),
    newDetectKeyPhrases,
    DetectKeyPhrasesResponse (DetectKeyPhrasesResponse'),
    newDetectKeyPhrasesResponse,

    -- ** DescribeEndpoint
    DescribeEndpoint (DescribeEndpoint'),
    newDescribeEndpoint,
    DescribeEndpointResponse (DescribeEndpointResponse'),
    newDescribeEndpointResponse,

    -- ** BatchDetectEntities
    BatchDetectEntities (BatchDetectEntities'),
    newBatchDetectEntities,
    BatchDetectEntitiesResponse (BatchDetectEntitiesResponse'),
    newBatchDetectEntitiesResponse,

    -- ** ListTopicsDetectionJobs (Paginated)
    ListTopicsDetectionJobs (ListTopicsDetectionJobs'),
    newListTopicsDetectionJobs,
    ListTopicsDetectionJobsResponse (ListTopicsDetectionJobsResponse'),
    newListTopicsDetectionJobsResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** BatchDetectDominantLanguage
    BatchDetectDominantLanguage (BatchDetectDominantLanguage'),
    newBatchDetectDominantLanguage,
    BatchDetectDominantLanguageResponse (BatchDetectDominantLanguageResponse'),
    newBatchDetectDominantLanguageResponse,

    -- ** StopTrainingDocumentClassifier
    StopTrainingDocumentClassifier (StopTrainingDocumentClassifier'),
    newStopTrainingDocumentClassifier,
    StopTrainingDocumentClassifierResponse (StopTrainingDocumentClassifierResponse'),
    newStopTrainingDocumentClassifierResponse,

    -- ** DescribeEntityRecognizer
    DescribeEntityRecognizer (DescribeEntityRecognizer'),
    newDescribeEntityRecognizer,
    DescribeEntityRecognizerResponse (DescribeEntityRecognizerResponse'),
    newDescribeEntityRecognizerResponse,

    -- ** DescribePiiEntitiesDetectionJob
    DescribePiiEntitiesDetectionJob (DescribePiiEntitiesDetectionJob'),
    newDescribePiiEntitiesDetectionJob,
    DescribePiiEntitiesDetectionJobResponse (DescribePiiEntitiesDetectionJobResponse'),
    newDescribePiiEntitiesDetectionJobResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListKeyPhrasesDetectionJobs (Paginated)
    ListKeyPhrasesDetectionJobs (ListKeyPhrasesDetectionJobs'),
    newListKeyPhrasesDetectionJobs,
    ListKeyPhrasesDetectionJobsResponse (ListKeyPhrasesDetectionJobsResponse'),
    newListKeyPhrasesDetectionJobsResponse,

    -- ** DescribeDominantLanguageDetectionJob
    DescribeDominantLanguageDetectionJob (DescribeDominantLanguageDetectionJob'),
    newDescribeDominantLanguageDetectionJob,
    DescribeDominantLanguageDetectionJobResponse (DescribeDominantLanguageDetectionJobResponse'),
    newDescribeDominantLanguageDetectionJobResponse,

    -- ** StopEntitiesDetectionJob
    StopEntitiesDetectionJob (StopEntitiesDetectionJob'),
    newStopEntitiesDetectionJob,
    StopEntitiesDetectionJobResponse (StopEntitiesDetectionJobResponse'),
    newStopEntitiesDetectionJobResponse,

    -- ** StopPiiEntitiesDetectionJob
    StopPiiEntitiesDetectionJob (StopPiiEntitiesDetectionJob'),
    newStopPiiEntitiesDetectionJob,
    StopPiiEntitiesDetectionJobResponse (StopPiiEntitiesDetectionJobResponse'),
    newStopPiiEntitiesDetectionJobResponse,

    -- ** DescribeDocumentClassifier
    DescribeDocumentClassifier (DescribeDocumentClassifier'),
    newDescribeDocumentClassifier,
    DescribeDocumentClassifierResponse (DescribeDocumentClassifierResponse'),
    newDescribeDocumentClassifierResponse,

    -- ** StopTrainingEntityRecognizer
    StopTrainingEntityRecognizer (StopTrainingEntityRecognizer'),
    newStopTrainingEntityRecognizer,
    StopTrainingEntityRecognizerResponse (StopTrainingEntityRecognizerResponse'),
    newStopTrainingEntityRecognizerResponse,

    -- ** StartEntitiesDetectionJob
    StartEntitiesDetectionJob (StartEntitiesDetectionJob'),
    newStartEntitiesDetectionJob,
    StartEntitiesDetectionJobResponse (StartEntitiesDetectionJobResponse'),
    newStartEntitiesDetectionJobResponse,

    -- ** StartPiiEntitiesDetectionJob
    StartPiiEntitiesDetectionJob (StartPiiEntitiesDetectionJob'),
    newStartPiiEntitiesDetectionJob,
    StartPiiEntitiesDetectionJobResponse (StartPiiEntitiesDetectionJobResponse'),
    newStartPiiEntitiesDetectionJobResponse,

    -- ** ListEndpoints
    ListEndpoints (ListEndpoints'),
    newListEndpoints,
    ListEndpointsResponse (ListEndpointsResponse'),
    newListEndpointsResponse,

    -- ** DeleteEndpoint
    DeleteEndpoint (DeleteEndpoint'),
    newDeleteEndpoint,
    DeleteEndpointResponse (DeleteEndpointResponse'),
    newDeleteEndpointResponse,

    -- ** UpdateEndpoint
    UpdateEndpoint (UpdateEndpoint'),
    newUpdateEndpoint,
    UpdateEndpointResponse (UpdateEndpointResponse'),
    newUpdateEndpointResponse,

    -- ** DescribeEventsDetectionJob
    DescribeEventsDetectionJob (DescribeEventsDetectionJob'),
    newDescribeEventsDetectionJob,
    DescribeEventsDetectionJobResponse (DescribeEventsDetectionJobResponse'),
    newDescribeEventsDetectionJobResponse,

    -- ** DetectPiiEntities
    DetectPiiEntities (DetectPiiEntities'),
    newDetectPiiEntities,
    DetectPiiEntitiesResponse (DetectPiiEntitiesResponse'),
    newDetectPiiEntitiesResponse,

    -- ** ClassifyDocument
    ClassifyDocument (ClassifyDocument'),
    newClassifyDocument,
    ClassifyDocumentResponse (ClassifyDocumentResponse'),
    newClassifyDocumentResponse,

    -- ** DetectDominantLanguage
    DetectDominantLanguage (DetectDominantLanguage'),
    newDetectDominantLanguage,
    DetectDominantLanguageResponse (DetectDominantLanguageResponse'),
    newDetectDominantLanguageResponse,

    -- ** ListDocumentClassificationJobs (Paginated)
    ListDocumentClassificationJobs (ListDocumentClassificationJobs'),
    newListDocumentClassificationJobs,
    ListDocumentClassificationJobsResponse (ListDocumentClassificationJobsResponse'),
    newListDocumentClassificationJobsResponse,

    -- ** DescribeTopicsDetectionJob
    DescribeTopicsDetectionJob (DescribeTopicsDetectionJob'),
    newDescribeTopicsDetectionJob,
    DescribeTopicsDetectionJobResponse (DescribeTopicsDetectionJobResponse'),
    newDescribeTopicsDetectionJobResponse,

    -- ** ListEntitiesDetectionJobs (Paginated)
    ListEntitiesDetectionJobs (ListEntitiesDetectionJobs'),
    newListEntitiesDetectionJobs,
    ListEntitiesDetectionJobsResponse (ListEntitiesDetectionJobsResponse'),
    newListEntitiesDetectionJobsResponse,

    -- ** CreateDocumentClassifier
    CreateDocumentClassifier (CreateDocumentClassifier'),
    newCreateDocumentClassifier,
    CreateDocumentClassifierResponse (CreateDocumentClassifierResponse'),
    newCreateDocumentClassifierResponse,

    -- ** ListPiiEntitiesDetectionJobs
    ListPiiEntitiesDetectionJobs (ListPiiEntitiesDetectionJobs'),
    newListPiiEntitiesDetectionJobs,
    ListPiiEntitiesDetectionJobsResponse (ListPiiEntitiesDetectionJobsResponse'),
    newListPiiEntitiesDetectionJobsResponse,

    -- ** DeleteDocumentClassifier
    DeleteDocumentClassifier (DeleteDocumentClassifier'),
    newDeleteDocumentClassifier,
    DeleteDocumentClassifierResponse (DeleteDocumentClassifierResponse'),
    newDeleteDocumentClassifierResponse,

    -- ** DescribeDocumentClassificationJob
    DescribeDocumentClassificationJob (DescribeDocumentClassificationJob'),
    newDescribeDocumentClassificationJob,
    DescribeDocumentClassificationJobResponse (DescribeDocumentClassificationJobResponse'),
    newDescribeDocumentClassificationJobResponse,

    -- ** StopDominantLanguageDetectionJob
    StopDominantLanguageDetectionJob (StopDominantLanguageDetectionJob'),
    newStopDominantLanguageDetectionJob,
    StopDominantLanguageDetectionJobResponse (StopDominantLanguageDetectionJobResponse'),
    newStopDominantLanguageDetectionJobResponse,

    -- ** DescribeEntitiesDetectionJob
    DescribeEntitiesDetectionJob (DescribeEntitiesDetectionJob'),
    newDescribeEntitiesDetectionJob,
    DescribeEntitiesDetectionJobResponse (DescribeEntitiesDetectionJobResponse'),
    newDescribeEntitiesDetectionJobResponse,

    -- ** StartDominantLanguageDetectionJob
    StartDominantLanguageDetectionJob (StartDominantLanguageDetectionJob'),
    newStartDominantLanguageDetectionJob,
    StartDominantLanguageDetectionJobResponse (StartDominantLanguageDetectionJobResponse'),
    newStartDominantLanguageDetectionJobResponse,

    -- ** DetectSentiment
    DetectSentiment (DetectSentiment'),
    newDetectSentiment,
    DetectSentimentResponse (DetectSentimentResponse'),
    newDetectSentimentResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DetectEntities
    DetectEntities (DetectEntities'),
    newDetectEntities,
    DetectEntitiesResponse (DetectEntitiesResponse'),
    newDetectEntitiesResponse,

    -- ** DescribeSentimentDetectionJob
    DescribeSentimentDetectionJob (DescribeSentimentDetectionJob'),
    newDescribeSentimentDetectionJob,
    DescribeSentimentDetectionJobResponse (DescribeSentimentDetectionJobResponse'),
    newDescribeSentimentDetectionJobResponse,

    -- ** BatchDetectSyntax
    BatchDetectSyntax (BatchDetectSyntax'),
    newBatchDetectSyntax,
    BatchDetectSyntaxResponse (BatchDetectSyntaxResponse'),
    newBatchDetectSyntaxResponse,

    -- ** BatchDetectKeyPhrases
    BatchDetectKeyPhrases (BatchDetectKeyPhrases'),
    newBatchDetectKeyPhrases,
    BatchDetectKeyPhrasesResponse (BatchDetectKeyPhrasesResponse'),
    newBatchDetectKeyPhrasesResponse,

    -- ** StartTopicsDetectionJob
    StartTopicsDetectionJob (StartTopicsDetectionJob'),
    newStartTopicsDetectionJob,
    StartTopicsDetectionJobResponse (StartTopicsDetectionJobResponse'),
    newStartTopicsDetectionJobResponse,

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
    AugmentedManifestsListItem (AugmentedManifestsListItem'),
    newAugmentedManifestsListItem,

    -- ** BatchDetectDominantLanguageItemResult
    BatchDetectDominantLanguageItemResult (BatchDetectDominantLanguageItemResult'),
    newBatchDetectDominantLanguageItemResult,

    -- ** BatchDetectEntitiesItemResult
    BatchDetectEntitiesItemResult (BatchDetectEntitiesItemResult'),
    newBatchDetectEntitiesItemResult,

    -- ** BatchDetectKeyPhrasesItemResult
    BatchDetectKeyPhrasesItemResult (BatchDetectKeyPhrasesItemResult'),
    newBatchDetectKeyPhrasesItemResult,

    -- ** BatchDetectSentimentItemResult
    BatchDetectSentimentItemResult (BatchDetectSentimentItemResult'),
    newBatchDetectSentimentItemResult,

    -- ** BatchDetectSyntaxItemResult
    BatchDetectSyntaxItemResult (BatchDetectSyntaxItemResult'),
    newBatchDetectSyntaxItemResult,

    -- ** BatchItemError
    BatchItemError (BatchItemError'),
    newBatchItemError,

    -- ** ClassifierEvaluationMetrics
    ClassifierEvaluationMetrics (ClassifierEvaluationMetrics'),
    newClassifierEvaluationMetrics,

    -- ** ClassifierMetadata
    ClassifierMetadata (ClassifierMetadata'),
    newClassifierMetadata,

    -- ** DocumentClass
    DocumentClass (DocumentClass'),
    newDocumentClass,

    -- ** DocumentClassificationJobFilter
    DocumentClassificationJobFilter (DocumentClassificationJobFilter'),
    newDocumentClassificationJobFilter,

    -- ** DocumentClassificationJobProperties
    DocumentClassificationJobProperties (DocumentClassificationJobProperties'),
    newDocumentClassificationJobProperties,

    -- ** DocumentClassifierFilter
    DocumentClassifierFilter (DocumentClassifierFilter'),
    newDocumentClassifierFilter,

    -- ** DocumentClassifierInputDataConfig
    DocumentClassifierInputDataConfig (DocumentClassifierInputDataConfig'),
    newDocumentClassifierInputDataConfig,

    -- ** DocumentClassifierOutputDataConfig
    DocumentClassifierOutputDataConfig (DocumentClassifierOutputDataConfig'),
    newDocumentClassifierOutputDataConfig,

    -- ** DocumentClassifierProperties
    DocumentClassifierProperties (DocumentClassifierProperties'),
    newDocumentClassifierProperties,

    -- ** DocumentLabel
    DocumentLabel (DocumentLabel'),
    newDocumentLabel,

    -- ** DominantLanguage
    DominantLanguage (DominantLanguage'),
    newDominantLanguage,

    -- ** DominantLanguageDetectionJobFilter
    DominantLanguageDetectionJobFilter (DominantLanguageDetectionJobFilter'),
    newDominantLanguageDetectionJobFilter,

    -- ** DominantLanguageDetectionJobProperties
    DominantLanguageDetectionJobProperties (DominantLanguageDetectionJobProperties'),
    newDominantLanguageDetectionJobProperties,

    -- ** EndpointFilter
    EndpointFilter (EndpointFilter'),
    newEndpointFilter,

    -- ** EndpointProperties
    EndpointProperties (EndpointProperties'),
    newEndpointProperties,

    -- ** EntitiesDetectionJobFilter
    EntitiesDetectionJobFilter (EntitiesDetectionJobFilter'),
    newEntitiesDetectionJobFilter,

    -- ** EntitiesDetectionJobProperties
    EntitiesDetectionJobProperties (EntitiesDetectionJobProperties'),
    newEntitiesDetectionJobProperties,

    -- ** Entity
    Entity (Entity'),
    newEntity,

    -- ** EntityRecognizerAnnotations
    EntityRecognizerAnnotations (EntityRecognizerAnnotations'),
    newEntityRecognizerAnnotations,

    -- ** EntityRecognizerDocuments
    EntityRecognizerDocuments (EntityRecognizerDocuments'),
    newEntityRecognizerDocuments,

    -- ** EntityRecognizerEntityList
    EntityRecognizerEntityList (EntityRecognizerEntityList'),
    newEntityRecognizerEntityList,

    -- ** EntityRecognizerEvaluationMetrics
    EntityRecognizerEvaluationMetrics (EntityRecognizerEvaluationMetrics'),
    newEntityRecognizerEvaluationMetrics,

    -- ** EntityRecognizerFilter
    EntityRecognizerFilter (EntityRecognizerFilter'),
    newEntityRecognizerFilter,

    -- ** EntityRecognizerInputDataConfig
    EntityRecognizerInputDataConfig (EntityRecognizerInputDataConfig'),
    newEntityRecognizerInputDataConfig,

    -- ** EntityRecognizerMetadata
    EntityRecognizerMetadata (EntityRecognizerMetadata'),
    newEntityRecognizerMetadata,

    -- ** EntityRecognizerMetadataEntityTypesListItem
    EntityRecognizerMetadataEntityTypesListItem (EntityRecognizerMetadataEntityTypesListItem'),
    newEntityRecognizerMetadataEntityTypesListItem,

    -- ** EntityRecognizerProperties
    EntityRecognizerProperties (EntityRecognizerProperties'),
    newEntityRecognizerProperties,

    -- ** EntityTypesEvaluationMetrics
    EntityTypesEvaluationMetrics (EntityTypesEvaluationMetrics'),
    newEntityTypesEvaluationMetrics,

    -- ** EntityTypesListItem
    EntityTypesListItem (EntityTypesListItem'),
    newEntityTypesListItem,

    -- ** EventsDetectionJobFilter
    EventsDetectionJobFilter (EventsDetectionJobFilter'),
    newEventsDetectionJobFilter,

    -- ** EventsDetectionJobProperties
    EventsDetectionJobProperties (EventsDetectionJobProperties'),
    newEventsDetectionJobProperties,

    -- ** InputDataConfig
    InputDataConfig (InputDataConfig'),
    newInputDataConfig,

    -- ** KeyPhrase
    KeyPhrase (KeyPhrase'),
    newKeyPhrase,

    -- ** KeyPhrasesDetectionJobFilter
    KeyPhrasesDetectionJobFilter (KeyPhrasesDetectionJobFilter'),
    newKeyPhrasesDetectionJobFilter,

    -- ** KeyPhrasesDetectionJobProperties
    KeyPhrasesDetectionJobProperties (KeyPhrasesDetectionJobProperties'),
    newKeyPhrasesDetectionJobProperties,

    -- ** OutputDataConfig
    OutputDataConfig (OutputDataConfig'),
    newOutputDataConfig,

    -- ** PartOfSpeechTag
    PartOfSpeechTag (PartOfSpeechTag'),
    newPartOfSpeechTag,

    -- ** PiiEntitiesDetectionJobFilter
    PiiEntitiesDetectionJobFilter (PiiEntitiesDetectionJobFilter'),
    newPiiEntitiesDetectionJobFilter,

    -- ** PiiEntitiesDetectionJobProperties
    PiiEntitiesDetectionJobProperties (PiiEntitiesDetectionJobProperties'),
    newPiiEntitiesDetectionJobProperties,

    -- ** PiiEntity
    PiiEntity (PiiEntity'),
    newPiiEntity,

    -- ** PiiOutputDataConfig
    PiiOutputDataConfig (PiiOutputDataConfig'),
    newPiiOutputDataConfig,

    -- ** RedactionConfig
    RedactionConfig (RedactionConfig'),
    newRedactionConfig,

    -- ** SentimentDetectionJobFilter
    SentimentDetectionJobFilter (SentimentDetectionJobFilter'),
    newSentimentDetectionJobFilter,

    -- ** SentimentDetectionJobProperties
    SentimentDetectionJobProperties (SentimentDetectionJobProperties'),
    newSentimentDetectionJobProperties,

    -- ** SentimentScore
    SentimentScore (SentimentScore'),
    newSentimentScore,

    -- ** SyntaxToken
    SyntaxToken (SyntaxToken'),
    newSyntaxToken,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TopicsDetectionJobFilter
    TopicsDetectionJobFilter (TopicsDetectionJobFilter'),
    newTopicsDetectionJobFilter,

    -- ** TopicsDetectionJobProperties
    TopicsDetectionJobProperties (TopicsDetectionJobProperties'),
    newTopicsDetectionJobProperties,

    -- ** VpcConfig
    VpcConfig (VpcConfig'),
    newVpcConfig,
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
import Network.AWS.Comprehend.Lens
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

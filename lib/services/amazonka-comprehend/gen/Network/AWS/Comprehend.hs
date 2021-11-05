{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Comprehend
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-11-27@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Comprehend is an AWS service for gaining insight into the content
-- of documents. Use these actions to determine the topics contained in
-- your documents, the topics they discuss, the predominant sentiment
-- expressed in them, the predominant language used, and more.
module Amazonka.Comprehend
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ResourceUnavailableException
    _ResourceUnavailableException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** BatchSizeLimitExceededException
    _BatchSizeLimitExceededException,

    -- ** UnsupportedLanguageException
    _UnsupportedLanguageException,

    -- ** JobNotFoundException
    _JobNotFoundException,

    -- ** TooManyTagKeysException
    _TooManyTagKeysException,

    -- ** InvalidFilterException
    _InvalidFilterException,

    -- ** KmsKeyValidationException
    _KmsKeyValidationException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** TextSizeLimitExceededException
    _TextSizeLimitExceededException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

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

    -- ** ListEntitiesDetectionJobs (Paginated)
    ListEntitiesDetectionJobs (ListEntitiesDetectionJobs'),
    newListEntitiesDetectionJobs,
    ListEntitiesDetectionJobsResponse (ListEntitiesDetectionJobsResponse'),
    newListEntitiesDetectionJobsResponse,

    -- ** CreateEndpoint
    CreateEndpoint (CreateEndpoint'),
    newCreateEndpoint,
    CreateEndpointResponse (CreateEndpointResponse'),
    newCreateEndpointResponse,

    -- ** StopEventsDetectionJob
    StopEventsDetectionJob (StopEventsDetectionJob'),
    newStopEventsDetectionJob,
    StopEventsDetectionJobResponse (StopEventsDetectionJobResponse'),
    newStopEventsDetectionJobResponse,

    -- ** StartSentimentDetectionJob
    StartSentimentDetectionJob (StartSentimentDetectionJob'),
    newStartSentimentDetectionJob,
    StartSentimentDetectionJobResponse (StartSentimentDetectionJobResponse'),
    newStartSentimentDetectionJobResponse,

    -- ** BatchDetectSyntax
    BatchDetectSyntax (BatchDetectSyntax'),
    newBatchDetectSyntax,
    BatchDetectSyntaxResponse (BatchDetectSyntaxResponse'),
    newBatchDetectSyntaxResponse,

    -- ** StartTopicsDetectionJob
    StartTopicsDetectionJob (StartTopicsDetectionJob'),
    newStartTopicsDetectionJob,
    StartTopicsDetectionJobResponse (StartTopicsDetectionJobResponse'),
    newStartTopicsDetectionJobResponse,

    -- ** DescribeEventsDetectionJob
    DescribeEventsDetectionJob (DescribeEventsDetectionJob'),
    newDescribeEventsDetectionJob,
    DescribeEventsDetectionJobResponse (DescribeEventsDetectionJobResponse'),
    newDescribeEventsDetectionJobResponse,

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

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** BatchDetectKeyPhrases
    BatchDetectKeyPhrases (BatchDetectKeyPhrases'),
    newBatchDetectKeyPhrases,
    BatchDetectKeyPhrasesResponse (BatchDetectKeyPhrasesResponse'),
    newBatchDetectKeyPhrasesResponse,

    -- ** DescribeSentimentDetectionJob
    DescribeSentimentDetectionJob (DescribeSentimentDetectionJob'),
    newDescribeSentimentDetectionJob,
    DescribeSentimentDetectionJobResponse (DescribeSentimentDetectionJobResponse'),
    newDescribeSentimentDetectionJobResponse,

    -- ** StartEntitiesDetectionJob
    StartEntitiesDetectionJob (StartEntitiesDetectionJob'),
    newStartEntitiesDetectionJob,
    StartEntitiesDetectionJobResponse (StartEntitiesDetectionJobResponse'),
    newStartEntitiesDetectionJobResponse,

    -- ** StopPiiEntitiesDetectionJob
    StopPiiEntitiesDetectionJob (StopPiiEntitiesDetectionJob'),
    newStopPiiEntitiesDetectionJob,
    StopPiiEntitiesDetectionJobResponse (StopPiiEntitiesDetectionJobResponse'),
    newStopPiiEntitiesDetectionJobResponse,

    -- ** DescribeEntityRecognizer
    DescribeEntityRecognizer (DescribeEntityRecognizer'),
    newDescribeEntityRecognizer,
    DescribeEntityRecognizerResponse (DescribeEntityRecognizerResponse'),
    newDescribeEntityRecognizerResponse,

    -- ** DetectSentiment
    DetectSentiment (DetectSentiment'),
    newDetectSentiment,
    DetectSentimentResponse (DetectSentimentResponse'),
    newDetectSentimentResponse,

    -- ** StartDominantLanguageDetectionJob
    StartDominantLanguageDetectionJob (StartDominantLanguageDetectionJob'),
    newStartDominantLanguageDetectionJob,
    StartDominantLanguageDetectionJobResponse (StartDominantLanguageDetectionJobResponse'),
    newStartDominantLanguageDetectionJobResponse,

    -- ** StopTrainingDocumentClassifier
    StopTrainingDocumentClassifier (StopTrainingDocumentClassifier'),
    newStopTrainingDocumentClassifier,
    StopTrainingDocumentClassifierResponse (StopTrainingDocumentClassifierResponse'),
    newStopTrainingDocumentClassifierResponse,

    -- ** DescribeDocumentClassificationJob
    DescribeDocumentClassificationJob (DescribeDocumentClassificationJob'),
    newDescribeDocumentClassificationJob,
    DescribeDocumentClassificationJobResponse (DescribeDocumentClassificationJobResponse'),
    newDescribeDocumentClassificationJobResponse,

    -- ** ContainsPiiEntities
    ContainsPiiEntities (ContainsPiiEntities'),
    newContainsPiiEntities,
    ContainsPiiEntitiesResponse (ContainsPiiEntitiesResponse'),
    newContainsPiiEntitiesResponse,

    -- ** ListEventsDetectionJobs
    ListEventsDetectionJobs (ListEventsDetectionJobs'),
    newListEventsDetectionJobs,
    ListEventsDetectionJobsResponse (ListEventsDetectionJobsResponse'),
    newListEventsDetectionJobsResponse,

    -- ** BatchDetectEntities
    BatchDetectEntities (BatchDetectEntities'),
    newBatchDetectEntities,
    BatchDetectEntitiesResponse (BatchDetectEntitiesResponse'),
    newBatchDetectEntitiesResponse,

    -- ** CreateEntityRecognizer
    CreateEntityRecognizer (CreateEntityRecognizer'),
    newCreateEntityRecognizer,
    CreateEntityRecognizerResponse (CreateEntityRecognizerResponse'),
    newCreateEntityRecognizerResponse,

    -- ** StopKeyPhrasesDetectionJob
    StopKeyPhrasesDetectionJob (StopKeyPhrasesDetectionJob'),
    newStopKeyPhrasesDetectionJob,
    StopKeyPhrasesDetectionJobResponse (StopKeyPhrasesDetectionJobResponse'),
    newStopKeyPhrasesDetectionJobResponse,

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

    -- ** ListEntityRecognizers (Paginated)
    ListEntityRecognizers (ListEntityRecognizers'),
    newListEntityRecognizers,
    ListEntityRecognizersResponse (ListEntityRecognizersResponse'),
    newListEntityRecognizersResponse,

    -- ** StopSentimentDetectionJob
    StopSentimentDetectionJob (StopSentimentDetectionJob'),
    newStopSentimentDetectionJob,
    StopSentimentDetectionJobResponse (StopSentimentDetectionJobResponse'),
    newStopSentimentDetectionJobResponse,

    -- ** DetectDominantLanguage
    DetectDominantLanguage (DetectDominantLanguage'),
    newDetectDominantLanguage,
    DetectDominantLanguageResponse (DetectDominantLanguageResponse'),
    newDetectDominantLanguageResponse,

    -- ** ClassifyDocument
    ClassifyDocument (ClassifyDocument'),
    newClassifyDocument,
    ClassifyDocumentResponse (ClassifyDocumentResponse'),
    newClassifyDocumentResponse,

    -- ** StartEventsDetectionJob
    StartEventsDetectionJob (StartEventsDetectionJob'),
    newStartEventsDetectionJob,
    StartEventsDetectionJobResponse (StartEventsDetectionJobResponse'),
    newStartEventsDetectionJobResponse,

    -- ** DescribeTopicsDetectionJob
    DescribeTopicsDetectionJob (DescribeTopicsDetectionJob'),
    newDescribeTopicsDetectionJob,
    DescribeTopicsDetectionJobResponse (DescribeTopicsDetectionJobResponse'),
    newDescribeTopicsDetectionJobResponse,

    -- ** ListDocumentClassificationJobs (Paginated)
    ListDocumentClassificationJobs (ListDocumentClassificationJobs'),
    newListDocumentClassificationJobs,
    ListDocumentClassificationJobsResponse (ListDocumentClassificationJobsResponse'),
    newListDocumentClassificationJobsResponse,

    -- ** DetectPiiEntities
    DetectPiiEntities (DetectPiiEntities'),
    newDetectPiiEntities,
    DetectPiiEntitiesResponse (DetectPiiEntitiesResponse'),
    newDetectPiiEntitiesResponse,

    -- ** ListEndpoints
    ListEndpoints (ListEndpoints'),
    newListEndpoints,
    ListEndpointsResponse (ListEndpointsResponse'),
    newListEndpointsResponse,

    -- ** DetectEntities
    DetectEntities (DetectEntities'),
    newDetectEntities,
    DetectEntitiesResponse (DetectEntitiesResponse'),
    newDetectEntitiesResponse,

    -- ** DescribeDocumentClassifier
    DescribeDocumentClassifier (DescribeDocumentClassifier'),
    newDescribeDocumentClassifier,
    DescribeDocumentClassifierResponse (DescribeDocumentClassifierResponse'),
    newDescribeDocumentClassifierResponse,

    -- ** DescribeDominantLanguageDetectionJob
    DescribeDominantLanguageDetectionJob (DescribeDominantLanguageDetectionJob'),
    newDescribeDominantLanguageDetectionJob,
    DescribeDominantLanguageDetectionJobResponse (DescribeDominantLanguageDetectionJobResponse'),
    newDescribeDominantLanguageDetectionJobResponse,

    -- ** ListEntityRecognizerSummaries
    ListEntityRecognizerSummaries (ListEntityRecognizerSummaries'),
    newListEntityRecognizerSummaries,
    ListEntityRecognizerSummariesResponse (ListEntityRecognizerSummariesResponse'),
    newListEntityRecognizerSummariesResponse,

    -- ** StopEntitiesDetectionJob
    StopEntitiesDetectionJob (StopEntitiesDetectionJob'),
    newStopEntitiesDetectionJob,
    StopEntitiesDetectionJobResponse (StopEntitiesDetectionJobResponse'),
    newStopEntitiesDetectionJobResponse,

    -- ** StopTrainingEntityRecognizer
    StopTrainingEntityRecognizer (StopTrainingEntityRecognizer'),
    newStopTrainingEntityRecognizer,
    StopTrainingEntityRecognizerResponse (StopTrainingEntityRecognizerResponse'),
    newStopTrainingEntityRecognizerResponse,

    -- ** StartPiiEntitiesDetectionJob
    StartPiiEntitiesDetectionJob (StartPiiEntitiesDetectionJob'),
    newStartPiiEntitiesDetectionJob,
    StartPiiEntitiesDetectionJobResponse (StartPiiEntitiesDetectionJobResponse'),
    newStartPiiEntitiesDetectionJobResponse,

    -- ** ListKeyPhrasesDetectionJobs (Paginated)
    ListKeyPhrasesDetectionJobs (ListKeyPhrasesDetectionJobs'),
    newListKeyPhrasesDetectionJobs,
    ListKeyPhrasesDetectionJobsResponse (ListKeyPhrasesDetectionJobsResponse'),
    newListKeyPhrasesDetectionJobsResponse,

    -- ** DescribeEntitiesDetectionJob
    DescribeEntitiesDetectionJob (DescribeEntitiesDetectionJob'),
    newDescribeEntitiesDetectionJob,
    DescribeEntitiesDetectionJobResponse (DescribeEntitiesDetectionJobResponse'),
    newDescribeEntitiesDetectionJobResponse,

    -- ** ListDocumentClassifierSummaries
    ListDocumentClassifierSummaries (ListDocumentClassifierSummaries'),
    newListDocumentClassifierSummaries,
    ListDocumentClassifierSummariesResponse (ListDocumentClassifierSummariesResponse'),
    newListDocumentClassifierSummariesResponse,

    -- ** StopDominantLanguageDetectionJob
    StopDominantLanguageDetectionJob (StopDominantLanguageDetectionJob'),
    newStopDominantLanguageDetectionJob,
    StopDominantLanguageDetectionJobResponse (StopDominantLanguageDetectionJobResponse'),
    newStopDominantLanguageDetectionJobResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** DescribePiiEntitiesDetectionJob
    DescribePiiEntitiesDetectionJob (DescribePiiEntitiesDetectionJob'),
    newDescribePiiEntitiesDetectionJob,
    DescribePiiEntitiesDetectionJobResponse (DescribePiiEntitiesDetectionJobResponse'),
    newDescribePiiEntitiesDetectionJobResponse,

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

    -- ** DetectSyntax
    DetectSyntax (DetectSyntax'),
    newDetectSyntax,
    DetectSyntaxResponse (DetectSyntaxResponse'),
    newDetectSyntaxResponse,

    -- ** DescribeEndpoint
    DescribeEndpoint (DescribeEndpoint'),
    newDescribeEndpoint,
    DescribeEndpointResponse (DescribeEndpointResponse'),
    newDescribeEndpointResponse,

    -- ** ListSentimentDetectionJobs (Paginated)
    ListSentimentDetectionJobs (ListSentimentDetectionJobs'),
    newListSentimentDetectionJobs,
    ListSentimentDetectionJobsResponse (ListSentimentDetectionJobsResponse'),
    newListSentimentDetectionJobsResponse,

    -- ** DeleteDocumentClassifier
    DeleteDocumentClassifier (DeleteDocumentClassifier'),
    newDeleteDocumentClassifier,
    DeleteDocumentClassifierResponse (DeleteDocumentClassifierResponse'),
    newDeleteDocumentClassifierResponse,

    -- ** ListDominantLanguageDetectionJobs (Paginated)
    ListDominantLanguageDetectionJobs (ListDominantLanguageDetectionJobs'),
    newListDominantLanguageDetectionJobs,
    ListDominantLanguageDetectionJobsResponse (ListDominantLanguageDetectionJobsResponse'),
    newListDominantLanguageDetectionJobsResponse,

    -- ** StartKeyPhrasesDetectionJob
    StartKeyPhrasesDetectionJob (StartKeyPhrasesDetectionJob'),
    newStartKeyPhrasesDetectionJob,
    StartKeyPhrasesDetectionJobResponse (StartKeyPhrasesDetectionJobResponse'),
    newStartKeyPhrasesDetectionJobResponse,

    -- ** ListDocumentClassifiers (Paginated)
    ListDocumentClassifiers (ListDocumentClassifiers'),
    newListDocumentClassifiers,
    ListDocumentClassifiersResponse (ListDocumentClassifiersResponse'),
    newListDocumentClassifiersResponse,

    -- * Types

    -- ** AugmentedManifestsDocumentTypeFormat
    AugmentedManifestsDocumentTypeFormat (..),

    -- ** DocumentClassifierDataFormat
    DocumentClassifierDataFormat (..),

    -- ** DocumentClassifierMode
    DocumentClassifierMode (..),

    -- ** DocumentReadAction
    DocumentReadAction (..),

    -- ** DocumentReadFeatureTypes
    DocumentReadFeatureTypes (..),

    -- ** DocumentReadMode
    DocumentReadMode (..),

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

    -- ** Split
    Split (..),

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

    -- ** DocumentClassifierSummary
    DocumentClassifierSummary (DocumentClassifierSummary'),
    newDocumentClassifierSummary,

    -- ** DocumentLabel
    DocumentLabel (DocumentLabel'),
    newDocumentLabel,

    -- ** DocumentReaderConfig
    DocumentReaderConfig (DocumentReaderConfig'),
    newDocumentReaderConfig,

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

    -- ** EntityLabel
    EntityLabel (EntityLabel'),
    newEntityLabel,

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

    -- ** EntityRecognizerSummary
    EntityRecognizerSummary (EntityRecognizerSummary'),
    newEntityRecognizerSummary,

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

import Amazonka.Comprehend.BatchDetectDominantLanguage
import Amazonka.Comprehend.BatchDetectEntities
import Amazonka.Comprehend.BatchDetectKeyPhrases
import Amazonka.Comprehend.BatchDetectSentiment
import Amazonka.Comprehend.BatchDetectSyntax
import Amazonka.Comprehend.ClassifyDocument
import Amazonka.Comprehend.ContainsPiiEntities
import Amazonka.Comprehend.CreateDocumentClassifier
import Amazonka.Comprehend.CreateEndpoint
import Amazonka.Comprehend.CreateEntityRecognizer
import Amazonka.Comprehend.DeleteDocumentClassifier
import Amazonka.Comprehend.DeleteEndpoint
import Amazonka.Comprehend.DeleteEntityRecognizer
import Amazonka.Comprehend.DescribeDocumentClassificationJob
import Amazonka.Comprehend.DescribeDocumentClassifier
import Amazonka.Comprehend.DescribeDominantLanguageDetectionJob
import Amazonka.Comprehend.DescribeEndpoint
import Amazonka.Comprehend.DescribeEntitiesDetectionJob
import Amazonka.Comprehend.DescribeEntityRecognizer
import Amazonka.Comprehend.DescribeEventsDetectionJob
import Amazonka.Comprehend.DescribeKeyPhrasesDetectionJob
import Amazonka.Comprehend.DescribePiiEntitiesDetectionJob
import Amazonka.Comprehend.DescribeSentimentDetectionJob
import Amazonka.Comprehend.DescribeTopicsDetectionJob
import Amazonka.Comprehend.DetectDominantLanguage
import Amazonka.Comprehend.DetectEntities
import Amazonka.Comprehend.DetectKeyPhrases
import Amazonka.Comprehend.DetectPiiEntities
import Amazonka.Comprehend.DetectSentiment
import Amazonka.Comprehend.DetectSyntax
import Amazonka.Comprehend.Lens
import Amazonka.Comprehend.ListDocumentClassificationJobs
import Amazonka.Comprehend.ListDocumentClassifierSummaries
import Amazonka.Comprehend.ListDocumentClassifiers
import Amazonka.Comprehend.ListDominantLanguageDetectionJobs
import Amazonka.Comprehend.ListEndpoints
import Amazonka.Comprehend.ListEntitiesDetectionJobs
import Amazonka.Comprehend.ListEntityRecognizerSummaries
import Amazonka.Comprehend.ListEntityRecognizers
import Amazonka.Comprehend.ListEventsDetectionJobs
import Amazonka.Comprehend.ListKeyPhrasesDetectionJobs
import Amazonka.Comprehend.ListPiiEntitiesDetectionJobs
import Amazonka.Comprehend.ListSentimentDetectionJobs
import Amazonka.Comprehend.ListTagsForResource
import Amazonka.Comprehend.ListTopicsDetectionJobs
import Amazonka.Comprehend.StartDocumentClassificationJob
import Amazonka.Comprehend.StartDominantLanguageDetectionJob
import Amazonka.Comprehend.StartEntitiesDetectionJob
import Amazonka.Comprehend.StartEventsDetectionJob
import Amazonka.Comprehend.StartKeyPhrasesDetectionJob
import Amazonka.Comprehend.StartPiiEntitiesDetectionJob
import Amazonka.Comprehend.StartSentimentDetectionJob
import Amazonka.Comprehend.StartTopicsDetectionJob
import Amazonka.Comprehend.StopDominantLanguageDetectionJob
import Amazonka.Comprehend.StopEntitiesDetectionJob
import Amazonka.Comprehend.StopEventsDetectionJob
import Amazonka.Comprehend.StopKeyPhrasesDetectionJob
import Amazonka.Comprehend.StopPiiEntitiesDetectionJob
import Amazonka.Comprehend.StopSentimentDetectionJob
import Amazonka.Comprehend.StopTrainingDocumentClassifier
import Amazonka.Comprehend.StopTrainingEntityRecognizer
import Amazonka.Comprehend.TagResource
import Amazonka.Comprehend.Types
import Amazonka.Comprehend.UntagResource
import Amazonka.Comprehend.UpdateEndpoint
import Amazonka.Comprehend.Waiters

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

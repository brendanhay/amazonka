{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Comprehend
-- Copyright   : (c) 2013-2023 Brendan Hay
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

    -- ** BatchSizeLimitExceededException
    _BatchSizeLimitExceededException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** InvalidFilterException
    _InvalidFilterException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** JobNotFoundException
    _JobNotFoundException,

    -- ** KmsKeyValidationException
    _KmsKeyValidationException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourceUnavailableException
    _ResourceUnavailableException,

    -- ** TextSizeLimitExceededException
    _TextSizeLimitExceededException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** TooManyTagKeysException
    _TooManyTagKeysException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** UnsupportedLanguageException
    _UnsupportedLanguageException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchDetectDominantLanguage
    BatchDetectDominantLanguage (BatchDetectDominantLanguage'),
    newBatchDetectDominantLanguage,
    BatchDetectDominantLanguageResponse (BatchDetectDominantLanguageResponse'),
    newBatchDetectDominantLanguageResponse,

    -- ** BatchDetectEntities
    BatchDetectEntities (BatchDetectEntities'),
    newBatchDetectEntities,
    BatchDetectEntitiesResponse (BatchDetectEntitiesResponse'),
    newBatchDetectEntitiesResponse,

    -- ** BatchDetectKeyPhrases
    BatchDetectKeyPhrases (BatchDetectKeyPhrases'),
    newBatchDetectKeyPhrases,
    BatchDetectKeyPhrasesResponse (BatchDetectKeyPhrasesResponse'),
    newBatchDetectKeyPhrasesResponse,

    -- ** BatchDetectSentiment
    BatchDetectSentiment (BatchDetectSentiment'),
    newBatchDetectSentiment,
    BatchDetectSentimentResponse (BatchDetectSentimentResponse'),
    newBatchDetectSentimentResponse,

    -- ** BatchDetectSyntax
    BatchDetectSyntax (BatchDetectSyntax'),
    newBatchDetectSyntax,
    BatchDetectSyntaxResponse (BatchDetectSyntaxResponse'),
    newBatchDetectSyntaxResponse,

    -- ** BatchDetectTargetedSentiment
    BatchDetectTargetedSentiment (BatchDetectTargetedSentiment'),
    newBatchDetectTargetedSentiment,
    BatchDetectTargetedSentimentResponse (BatchDetectTargetedSentimentResponse'),
    newBatchDetectTargetedSentimentResponse,

    -- ** ClassifyDocument
    ClassifyDocument (ClassifyDocument'),
    newClassifyDocument,
    ClassifyDocumentResponse (ClassifyDocumentResponse'),
    newClassifyDocumentResponse,

    -- ** ContainsPiiEntities
    ContainsPiiEntities (ContainsPiiEntities'),
    newContainsPiiEntities,
    ContainsPiiEntitiesResponse (ContainsPiiEntitiesResponse'),
    newContainsPiiEntitiesResponse,

    -- ** CreateDocumentClassifier
    CreateDocumentClassifier (CreateDocumentClassifier'),
    newCreateDocumentClassifier,
    CreateDocumentClassifierResponse (CreateDocumentClassifierResponse'),
    newCreateDocumentClassifierResponse,

    -- ** CreateEndpoint
    CreateEndpoint (CreateEndpoint'),
    newCreateEndpoint,
    CreateEndpointResponse (CreateEndpointResponse'),
    newCreateEndpointResponse,

    -- ** CreateEntityRecognizer
    CreateEntityRecognizer (CreateEntityRecognizer'),
    newCreateEntityRecognizer,
    CreateEntityRecognizerResponse (CreateEntityRecognizerResponse'),
    newCreateEntityRecognizerResponse,

    -- ** DeleteDocumentClassifier
    DeleteDocumentClassifier (DeleteDocumentClassifier'),
    newDeleteDocumentClassifier,
    DeleteDocumentClassifierResponse (DeleteDocumentClassifierResponse'),
    newDeleteDocumentClassifierResponse,

    -- ** DeleteEndpoint
    DeleteEndpoint (DeleteEndpoint'),
    newDeleteEndpoint,
    DeleteEndpointResponse (DeleteEndpointResponse'),
    newDeleteEndpointResponse,

    -- ** DeleteEntityRecognizer
    DeleteEntityRecognizer (DeleteEntityRecognizer'),
    newDeleteEntityRecognizer,
    DeleteEntityRecognizerResponse (DeleteEntityRecognizerResponse'),
    newDeleteEntityRecognizerResponse,

    -- ** DeleteResourcePolicy
    DeleteResourcePolicy (DeleteResourcePolicy'),
    newDeleteResourcePolicy,
    DeleteResourcePolicyResponse (DeleteResourcePolicyResponse'),
    newDeleteResourcePolicyResponse,

    -- ** DescribeDocumentClassificationJob
    DescribeDocumentClassificationJob (DescribeDocumentClassificationJob'),
    newDescribeDocumentClassificationJob,
    DescribeDocumentClassificationJobResponse (DescribeDocumentClassificationJobResponse'),
    newDescribeDocumentClassificationJobResponse,

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

    -- ** DescribeEndpoint
    DescribeEndpoint (DescribeEndpoint'),
    newDescribeEndpoint,
    DescribeEndpointResponse (DescribeEndpointResponse'),
    newDescribeEndpointResponse,

    -- ** DescribeEntitiesDetectionJob
    DescribeEntitiesDetectionJob (DescribeEntitiesDetectionJob'),
    newDescribeEntitiesDetectionJob,
    DescribeEntitiesDetectionJobResponse (DescribeEntitiesDetectionJobResponse'),
    newDescribeEntitiesDetectionJobResponse,

    -- ** DescribeEntityRecognizer
    DescribeEntityRecognizer (DescribeEntityRecognizer'),
    newDescribeEntityRecognizer,
    DescribeEntityRecognizerResponse (DescribeEntityRecognizerResponse'),
    newDescribeEntityRecognizerResponse,

    -- ** DescribeEventsDetectionJob
    DescribeEventsDetectionJob (DescribeEventsDetectionJob'),
    newDescribeEventsDetectionJob,
    DescribeEventsDetectionJobResponse (DescribeEventsDetectionJobResponse'),
    newDescribeEventsDetectionJobResponse,

    -- ** DescribeKeyPhrasesDetectionJob
    DescribeKeyPhrasesDetectionJob (DescribeKeyPhrasesDetectionJob'),
    newDescribeKeyPhrasesDetectionJob,
    DescribeKeyPhrasesDetectionJobResponse (DescribeKeyPhrasesDetectionJobResponse'),
    newDescribeKeyPhrasesDetectionJobResponse,

    -- ** DescribePiiEntitiesDetectionJob
    DescribePiiEntitiesDetectionJob (DescribePiiEntitiesDetectionJob'),
    newDescribePiiEntitiesDetectionJob,
    DescribePiiEntitiesDetectionJobResponse (DescribePiiEntitiesDetectionJobResponse'),
    newDescribePiiEntitiesDetectionJobResponse,

    -- ** DescribeResourcePolicy
    DescribeResourcePolicy (DescribeResourcePolicy'),
    newDescribeResourcePolicy,
    DescribeResourcePolicyResponse (DescribeResourcePolicyResponse'),
    newDescribeResourcePolicyResponse,

    -- ** DescribeSentimentDetectionJob
    DescribeSentimentDetectionJob (DescribeSentimentDetectionJob'),
    newDescribeSentimentDetectionJob,
    DescribeSentimentDetectionJobResponse (DescribeSentimentDetectionJobResponse'),
    newDescribeSentimentDetectionJobResponse,

    -- ** DescribeTargetedSentimentDetectionJob
    DescribeTargetedSentimentDetectionJob (DescribeTargetedSentimentDetectionJob'),
    newDescribeTargetedSentimentDetectionJob,
    DescribeTargetedSentimentDetectionJobResponse (DescribeTargetedSentimentDetectionJobResponse'),
    newDescribeTargetedSentimentDetectionJobResponse,

    -- ** DescribeTopicsDetectionJob
    DescribeTopicsDetectionJob (DescribeTopicsDetectionJob'),
    newDescribeTopicsDetectionJob,
    DescribeTopicsDetectionJobResponse (DescribeTopicsDetectionJobResponse'),
    newDescribeTopicsDetectionJobResponse,

    -- ** DetectDominantLanguage
    DetectDominantLanguage (DetectDominantLanguage'),
    newDetectDominantLanguage,
    DetectDominantLanguageResponse (DetectDominantLanguageResponse'),
    newDetectDominantLanguageResponse,

    -- ** DetectEntities
    DetectEntities (DetectEntities'),
    newDetectEntities,
    DetectEntitiesResponse (DetectEntitiesResponse'),
    newDetectEntitiesResponse,

    -- ** DetectKeyPhrases
    DetectKeyPhrases (DetectKeyPhrases'),
    newDetectKeyPhrases,
    DetectKeyPhrasesResponse (DetectKeyPhrasesResponse'),
    newDetectKeyPhrasesResponse,

    -- ** DetectPiiEntities
    DetectPiiEntities (DetectPiiEntities'),
    newDetectPiiEntities,
    DetectPiiEntitiesResponse (DetectPiiEntitiesResponse'),
    newDetectPiiEntitiesResponse,

    -- ** DetectSentiment
    DetectSentiment (DetectSentiment'),
    newDetectSentiment,
    DetectSentimentResponse (DetectSentimentResponse'),
    newDetectSentimentResponse,

    -- ** DetectSyntax
    DetectSyntax (DetectSyntax'),
    newDetectSyntax,
    DetectSyntaxResponse (DetectSyntaxResponse'),
    newDetectSyntaxResponse,

    -- ** DetectTargetedSentiment
    DetectTargetedSentiment (DetectTargetedSentiment'),
    newDetectTargetedSentiment,
    DetectTargetedSentimentResponse (DetectTargetedSentimentResponse'),
    newDetectTargetedSentimentResponse,

    -- ** ImportModel
    ImportModel (ImportModel'),
    newImportModel,
    ImportModelResponse (ImportModelResponse'),
    newImportModelResponse,

    -- ** ListDocumentClassificationJobs (Paginated)
    ListDocumentClassificationJobs (ListDocumentClassificationJobs'),
    newListDocumentClassificationJobs,
    ListDocumentClassificationJobsResponse (ListDocumentClassificationJobsResponse'),
    newListDocumentClassificationJobsResponse,

    -- ** ListDocumentClassifierSummaries
    ListDocumentClassifierSummaries (ListDocumentClassifierSummaries'),
    newListDocumentClassifierSummaries,
    ListDocumentClassifierSummariesResponse (ListDocumentClassifierSummariesResponse'),
    newListDocumentClassifierSummariesResponse,

    -- ** ListDocumentClassifiers (Paginated)
    ListDocumentClassifiers (ListDocumentClassifiers'),
    newListDocumentClassifiers,
    ListDocumentClassifiersResponse (ListDocumentClassifiersResponse'),
    newListDocumentClassifiersResponse,

    -- ** ListDominantLanguageDetectionJobs (Paginated)
    ListDominantLanguageDetectionJobs (ListDominantLanguageDetectionJobs'),
    newListDominantLanguageDetectionJobs,
    ListDominantLanguageDetectionJobsResponse (ListDominantLanguageDetectionJobsResponse'),
    newListDominantLanguageDetectionJobsResponse,

    -- ** ListEndpoints (Paginated)
    ListEndpoints (ListEndpoints'),
    newListEndpoints,
    ListEndpointsResponse (ListEndpointsResponse'),
    newListEndpointsResponse,

    -- ** ListEntitiesDetectionJobs (Paginated)
    ListEntitiesDetectionJobs (ListEntitiesDetectionJobs'),
    newListEntitiesDetectionJobs,
    ListEntitiesDetectionJobsResponse (ListEntitiesDetectionJobsResponse'),
    newListEntitiesDetectionJobsResponse,

    -- ** ListEntityRecognizerSummaries
    ListEntityRecognizerSummaries (ListEntityRecognizerSummaries'),
    newListEntityRecognizerSummaries,
    ListEntityRecognizerSummariesResponse (ListEntityRecognizerSummariesResponse'),
    newListEntityRecognizerSummariesResponse,

    -- ** ListEntityRecognizers (Paginated)
    ListEntityRecognizers (ListEntityRecognizers'),
    newListEntityRecognizers,
    ListEntityRecognizersResponse (ListEntityRecognizersResponse'),
    newListEntityRecognizersResponse,

    -- ** ListEventsDetectionJobs
    ListEventsDetectionJobs (ListEventsDetectionJobs'),
    newListEventsDetectionJobs,
    ListEventsDetectionJobsResponse (ListEventsDetectionJobsResponse'),
    newListEventsDetectionJobsResponse,

    -- ** ListKeyPhrasesDetectionJobs (Paginated)
    ListKeyPhrasesDetectionJobs (ListKeyPhrasesDetectionJobs'),
    newListKeyPhrasesDetectionJobs,
    ListKeyPhrasesDetectionJobsResponse (ListKeyPhrasesDetectionJobsResponse'),
    newListKeyPhrasesDetectionJobsResponse,

    -- ** ListPiiEntitiesDetectionJobs (Paginated)
    ListPiiEntitiesDetectionJobs (ListPiiEntitiesDetectionJobs'),
    newListPiiEntitiesDetectionJobs,
    ListPiiEntitiesDetectionJobsResponse (ListPiiEntitiesDetectionJobsResponse'),
    newListPiiEntitiesDetectionJobsResponse,

    -- ** ListSentimentDetectionJobs (Paginated)
    ListSentimentDetectionJobs (ListSentimentDetectionJobs'),
    newListSentimentDetectionJobs,
    ListSentimentDetectionJobsResponse (ListSentimentDetectionJobsResponse'),
    newListSentimentDetectionJobsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTargetedSentimentDetectionJobs
    ListTargetedSentimentDetectionJobs (ListTargetedSentimentDetectionJobs'),
    newListTargetedSentimentDetectionJobs,
    ListTargetedSentimentDetectionJobsResponse (ListTargetedSentimentDetectionJobsResponse'),
    newListTargetedSentimentDetectionJobsResponse,

    -- ** ListTopicsDetectionJobs (Paginated)
    ListTopicsDetectionJobs (ListTopicsDetectionJobs'),
    newListTopicsDetectionJobs,
    ListTopicsDetectionJobsResponse (ListTopicsDetectionJobsResponse'),
    newListTopicsDetectionJobsResponse,

    -- ** PutResourcePolicy
    PutResourcePolicy (PutResourcePolicy'),
    newPutResourcePolicy,
    PutResourcePolicyResponse (PutResourcePolicyResponse'),
    newPutResourcePolicyResponse,

    -- ** StartDocumentClassificationJob
    StartDocumentClassificationJob (StartDocumentClassificationJob'),
    newStartDocumentClassificationJob,
    StartDocumentClassificationJobResponse (StartDocumentClassificationJobResponse'),
    newStartDocumentClassificationJobResponse,

    -- ** StartDominantLanguageDetectionJob
    StartDominantLanguageDetectionJob (StartDominantLanguageDetectionJob'),
    newStartDominantLanguageDetectionJob,
    StartDominantLanguageDetectionJobResponse (StartDominantLanguageDetectionJobResponse'),
    newStartDominantLanguageDetectionJobResponse,

    -- ** StartEntitiesDetectionJob
    StartEntitiesDetectionJob (StartEntitiesDetectionJob'),
    newStartEntitiesDetectionJob,
    StartEntitiesDetectionJobResponse (StartEntitiesDetectionJobResponse'),
    newStartEntitiesDetectionJobResponse,

    -- ** StartEventsDetectionJob
    StartEventsDetectionJob (StartEventsDetectionJob'),
    newStartEventsDetectionJob,
    StartEventsDetectionJobResponse (StartEventsDetectionJobResponse'),
    newStartEventsDetectionJobResponse,

    -- ** StartKeyPhrasesDetectionJob
    StartKeyPhrasesDetectionJob (StartKeyPhrasesDetectionJob'),
    newStartKeyPhrasesDetectionJob,
    StartKeyPhrasesDetectionJobResponse (StartKeyPhrasesDetectionJobResponse'),
    newStartKeyPhrasesDetectionJobResponse,

    -- ** StartPiiEntitiesDetectionJob
    StartPiiEntitiesDetectionJob (StartPiiEntitiesDetectionJob'),
    newStartPiiEntitiesDetectionJob,
    StartPiiEntitiesDetectionJobResponse (StartPiiEntitiesDetectionJobResponse'),
    newStartPiiEntitiesDetectionJobResponse,

    -- ** StartSentimentDetectionJob
    StartSentimentDetectionJob (StartSentimentDetectionJob'),
    newStartSentimentDetectionJob,
    StartSentimentDetectionJobResponse (StartSentimentDetectionJobResponse'),
    newStartSentimentDetectionJobResponse,

    -- ** StartTargetedSentimentDetectionJob
    StartTargetedSentimentDetectionJob (StartTargetedSentimentDetectionJob'),
    newStartTargetedSentimentDetectionJob,
    StartTargetedSentimentDetectionJobResponse (StartTargetedSentimentDetectionJobResponse'),
    newStartTargetedSentimentDetectionJobResponse,

    -- ** StartTopicsDetectionJob
    StartTopicsDetectionJob (StartTopicsDetectionJob'),
    newStartTopicsDetectionJob,
    StartTopicsDetectionJobResponse (StartTopicsDetectionJobResponse'),
    newStartTopicsDetectionJobResponse,

    -- ** StopDominantLanguageDetectionJob
    StopDominantLanguageDetectionJob (StopDominantLanguageDetectionJob'),
    newStopDominantLanguageDetectionJob,
    StopDominantLanguageDetectionJobResponse (StopDominantLanguageDetectionJobResponse'),
    newStopDominantLanguageDetectionJobResponse,

    -- ** StopEntitiesDetectionJob
    StopEntitiesDetectionJob (StopEntitiesDetectionJob'),
    newStopEntitiesDetectionJob,
    StopEntitiesDetectionJobResponse (StopEntitiesDetectionJobResponse'),
    newStopEntitiesDetectionJobResponse,

    -- ** StopEventsDetectionJob
    StopEventsDetectionJob (StopEventsDetectionJob'),
    newStopEventsDetectionJob,
    StopEventsDetectionJobResponse (StopEventsDetectionJobResponse'),
    newStopEventsDetectionJobResponse,

    -- ** StopKeyPhrasesDetectionJob
    StopKeyPhrasesDetectionJob (StopKeyPhrasesDetectionJob'),
    newStopKeyPhrasesDetectionJob,
    StopKeyPhrasesDetectionJobResponse (StopKeyPhrasesDetectionJobResponse'),
    newStopKeyPhrasesDetectionJobResponse,

    -- ** StopPiiEntitiesDetectionJob
    StopPiiEntitiesDetectionJob (StopPiiEntitiesDetectionJob'),
    newStopPiiEntitiesDetectionJob,
    StopPiiEntitiesDetectionJobResponse (StopPiiEntitiesDetectionJobResponse'),
    newStopPiiEntitiesDetectionJobResponse,

    -- ** StopSentimentDetectionJob
    StopSentimentDetectionJob (StopSentimentDetectionJob'),
    newStopSentimentDetectionJob,
    StopSentimentDetectionJobResponse (StopSentimentDetectionJobResponse'),
    newStopSentimentDetectionJobResponse,

    -- ** StopTargetedSentimentDetectionJob
    StopTargetedSentimentDetectionJob (StopTargetedSentimentDetectionJob'),
    newStopTargetedSentimentDetectionJob,
    StopTargetedSentimentDetectionJobResponse (StopTargetedSentimentDetectionJobResponse'),
    newStopTargetedSentimentDetectionJobResponse,

    -- ** StopTrainingDocumentClassifier
    StopTrainingDocumentClassifier (StopTrainingDocumentClassifier'),
    newStopTrainingDocumentClassifier,
    StopTrainingDocumentClassifierResponse (StopTrainingDocumentClassifierResponse'),
    newStopTrainingDocumentClassifierResponse,

    -- ** StopTrainingEntityRecognizer
    StopTrainingEntityRecognizer (StopTrainingEntityRecognizer'),
    newStopTrainingEntityRecognizer,
    StopTrainingEntityRecognizerResponse (StopTrainingEntityRecognizerResponse'),
    newStopTrainingEntityRecognizerResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateEndpoint
    UpdateEndpoint (UpdateEndpoint'),
    newUpdateEndpoint,
    UpdateEndpointResponse (UpdateEndpointResponse'),
    newUpdateEndpointResponse,

    -- * Types

    -- ** AugmentedManifestsDocumentTypeFormat
    AugmentedManifestsDocumentTypeFormat (..),

    -- ** BlockType
    BlockType (..),

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

    -- ** DocumentType
    DocumentType (..),

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

    -- ** PageBasedErrorCode
    PageBasedErrorCode (..),

    -- ** PartOfSpeechTagType
    PartOfSpeechTagType (..),

    -- ** PiiEntitiesDetectionMaskMode
    PiiEntitiesDetectionMaskMode (..),

    -- ** PiiEntitiesDetectionMode
    PiiEntitiesDetectionMode (..),

    -- ** PiiEntityType
    PiiEntityType (..),

    -- ** RelationshipType
    RelationshipType (..),

    -- ** SentimentType
    SentimentType (..),

    -- ** Split
    Split (..),

    -- ** SyntaxLanguageCode
    SyntaxLanguageCode (..),

    -- ** TargetedSentimentEntityType
    TargetedSentimentEntityType (..),

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

    -- ** BatchDetectTargetedSentimentItemResult
    BatchDetectTargetedSentimentItemResult (BatchDetectTargetedSentimentItemResult'),
    newBatchDetectTargetedSentimentItemResult,

    -- ** BatchItemError
    BatchItemError (BatchItemError'),
    newBatchItemError,

    -- ** Block
    Block (Block'),
    newBlock,

    -- ** BlockReference
    BlockReference (BlockReference'),
    newBlockReference,

    -- ** BoundingBox
    BoundingBox (BoundingBox'),
    newBoundingBox,

    -- ** ChildBlock
    ChildBlock (ChildBlock'),
    newChildBlock,

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

    -- ** DocumentMetadata
    DocumentMetadata (DocumentMetadata'),
    newDocumentMetadata,

    -- ** DocumentReaderConfig
    DocumentReaderConfig (DocumentReaderConfig'),
    newDocumentReaderConfig,

    -- ** DocumentTypeListItem
    DocumentTypeListItem (DocumentTypeListItem'),
    newDocumentTypeListItem,

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

    -- ** ErrorsListItem
    ErrorsListItem (ErrorsListItem'),
    newErrorsListItem,

    -- ** EventsDetectionJobFilter
    EventsDetectionJobFilter (EventsDetectionJobFilter'),
    newEventsDetectionJobFilter,

    -- ** EventsDetectionJobProperties
    EventsDetectionJobProperties (EventsDetectionJobProperties'),
    newEventsDetectionJobProperties,

    -- ** ExtractedCharactersListItem
    ExtractedCharactersListItem (ExtractedCharactersListItem'),
    newExtractedCharactersListItem,

    -- ** Geometry
    Geometry (Geometry'),
    newGeometry,

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

    -- ** MentionSentiment
    MentionSentiment (MentionSentiment'),
    newMentionSentiment,

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

    -- ** Point
    Point (Point'),
    newPoint,

    -- ** RedactionConfig
    RedactionConfig (RedactionConfig'),
    newRedactionConfig,

    -- ** RelationshipsListItem
    RelationshipsListItem (RelationshipsListItem'),
    newRelationshipsListItem,

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

    -- ** TargetedSentimentDetectionJobFilter
    TargetedSentimentDetectionJobFilter (TargetedSentimentDetectionJobFilter'),
    newTargetedSentimentDetectionJobFilter,

    -- ** TargetedSentimentDetectionJobProperties
    TargetedSentimentDetectionJobProperties (TargetedSentimentDetectionJobProperties'),
    newTargetedSentimentDetectionJobProperties,

    -- ** TargetedSentimentEntity
    TargetedSentimentEntity (TargetedSentimentEntity'),
    newTargetedSentimentEntity,

    -- ** TargetedSentimentMention
    TargetedSentimentMention (TargetedSentimentMention'),
    newTargetedSentimentMention,

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
import Amazonka.Comprehend.BatchDetectTargetedSentiment
import Amazonka.Comprehend.ClassifyDocument
import Amazonka.Comprehend.ContainsPiiEntities
import Amazonka.Comprehend.CreateDocumentClassifier
import Amazonka.Comprehend.CreateEndpoint
import Amazonka.Comprehend.CreateEntityRecognizer
import Amazonka.Comprehend.DeleteDocumentClassifier
import Amazonka.Comprehend.DeleteEndpoint
import Amazonka.Comprehend.DeleteEntityRecognizer
import Amazonka.Comprehend.DeleteResourcePolicy
import Amazonka.Comprehend.DescribeDocumentClassificationJob
import Amazonka.Comprehend.DescribeDocumentClassifier
import Amazonka.Comprehend.DescribeDominantLanguageDetectionJob
import Amazonka.Comprehend.DescribeEndpoint
import Amazonka.Comprehend.DescribeEntitiesDetectionJob
import Amazonka.Comprehend.DescribeEntityRecognizer
import Amazonka.Comprehend.DescribeEventsDetectionJob
import Amazonka.Comprehend.DescribeKeyPhrasesDetectionJob
import Amazonka.Comprehend.DescribePiiEntitiesDetectionJob
import Amazonka.Comprehend.DescribeResourcePolicy
import Amazonka.Comprehend.DescribeSentimentDetectionJob
import Amazonka.Comprehend.DescribeTargetedSentimentDetectionJob
import Amazonka.Comprehend.DescribeTopicsDetectionJob
import Amazonka.Comprehend.DetectDominantLanguage
import Amazonka.Comprehend.DetectEntities
import Amazonka.Comprehend.DetectKeyPhrases
import Amazonka.Comprehend.DetectPiiEntities
import Amazonka.Comprehend.DetectSentiment
import Amazonka.Comprehend.DetectSyntax
import Amazonka.Comprehend.DetectTargetedSentiment
import Amazonka.Comprehend.ImportModel
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
import Amazonka.Comprehend.ListTargetedSentimentDetectionJobs
import Amazonka.Comprehend.ListTopicsDetectionJobs
import Amazonka.Comprehend.PutResourcePolicy
import Amazonka.Comprehend.StartDocumentClassificationJob
import Amazonka.Comprehend.StartDominantLanguageDetectionJob
import Amazonka.Comprehend.StartEntitiesDetectionJob
import Amazonka.Comprehend.StartEventsDetectionJob
import Amazonka.Comprehend.StartKeyPhrasesDetectionJob
import Amazonka.Comprehend.StartPiiEntitiesDetectionJob
import Amazonka.Comprehend.StartSentimentDetectionJob
import Amazonka.Comprehend.StartTargetedSentimentDetectionJob
import Amazonka.Comprehend.StartTopicsDetectionJob
import Amazonka.Comprehend.StopDominantLanguageDetectionJob
import Amazonka.Comprehend.StopEntitiesDetectionJob
import Amazonka.Comprehend.StopEventsDetectionJob
import Amazonka.Comprehend.StopKeyPhrasesDetectionJob
import Amazonka.Comprehend.StopPiiEntitiesDetectionJob
import Amazonka.Comprehend.StopSentimentDetectionJob
import Amazonka.Comprehend.StopTargetedSentimentDetectionJob
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

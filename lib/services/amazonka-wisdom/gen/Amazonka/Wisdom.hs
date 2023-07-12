{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Wisdom
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-10-19@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Connect Wisdom delivers agents the information they need to solve
-- customer issues as they\'re actively speaking with customers. Agents can
-- search across connected repositories from within their agent desktop to
-- find answers quickly. Use the Amazon Connect Wisdom APIs to create an
-- assistant and a knowledge base, for example, or manage content by
-- uploading custom files.
module Amazonka.Wisdom
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** PreconditionFailedException
    _PreconditionFailedException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateAssistant
    CreateAssistant (CreateAssistant'),
    newCreateAssistant,
    CreateAssistantResponse (CreateAssistantResponse'),
    newCreateAssistantResponse,

    -- ** CreateAssistantAssociation
    CreateAssistantAssociation (CreateAssistantAssociation'),
    newCreateAssistantAssociation,
    CreateAssistantAssociationResponse (CreateAssistantAssociationResponse'),
    newCreateAssistantAssociationResponse,

    -- ** CreateContent
    CreateContent (CreateContent'),
    newCreateContent,
    CreateContentResponse (CreateContentResponse'),
    newCreateContentResponse,

    -- ** CreateKnowledgeBase
    CreateKnowledgeBase (CreateKnowledgeBase'),
    newCreateKnowledgeBase,
    CreateKnowledgeBaseResponse (CreateKnowledgeBaseResponse'),
    newCreateKnowledgeBaseResponse,

    -- ** CreateSession
    CreateSession (CreateSession'),
    newCreateSession,
    CreateSessionResponse (CreateSessionResponse'),
    newCreateSessionResponse,

    -- ** DeleteAssistant
    DeleteAssistant (DeleteAssistant'),
    newDeleteAssistant,
    DeleteAssistantResponse (DeleteAssistantResponse'),
    newDeleteAssistantResponse,

    -- ** DeleteAssistantAssociation
    DeleteAssistantAssociation (DeleteAssistantAssociation'),
    newDeleteAssistantAssociation,
    DeleteAssistantAssociationResponse (DeleteAssistantAssociationResponse'),
    newDeleteAssistantAssociationResponse,

    -- ** DeleteContent
    DeleteContent (DeleteContent'),
    newDeleteContent,
    DeleteContentResponse (DeleteContentResponse'),
    newDeleteContentResponse,

    -- ** DeleteKnowledgeBase
    DeleteKnowledgeBase (DeleteKnowledgeBase'),
    newDeleteKnowledgeBase,
    DeleteKnowledgeBaseResponse (DeleteKnowledgeBaseResponse'),
    newDeleteKnowledgeBaseResponse,

    -- ** GetAssistant
    GetAssistant (GetAssistant'),
    newGetAssistant,
    GetAssistantResponse (GetAssistantResponse'),
    newGetAssistantResponse,

    -- ** GetAssistantAssociation
    GetAssistantAssociation (GetAssistantAssociation'),
    newGetAssistantAssociation,
    GetAssistantAssociationResponse (GetAssistantAssociationResponse'),
    newGetAssistantAssociationResponse,

    -- ** GetContent
    GetContent (GetContent'),
    newGetContent,
    GetContentResponse (GetContentResponse'),
    newGetContentResponse,

    -- ** GetContentSummary
    GetContentSummary (GetContentSummary'),
    newGetContentSummary,
    GetContentSummaryResponse (GetContentSummaryResponse'),
    newGetContentSummaryResponse,

    -- ** GetKnowledgeBase
    GetKnowledgeBase (GetKnowledgeBase'),
    newGetKnowledgeBase,
    GetKnowledgeBaseResponse (GetKnowledgeBaseResponse'),
    newGetKnowledgeBaseResponse,

    -- ** GetRecommendations
    GetRecommendations (GetRecommendations'),
    newGetRecommendations,
    GetRecommendationsResponse (GetRecommendationsResponse'),
    newGetRecommendationsResponse,

    -- ** GetSession
    GetSession (GetSession'),
    newGetSession,
    GetSessionResponse (GetSessionResponse'),
    newGetSessionResponse,

    -- ** ListAssistantAssociations (Paginated)
    ListAssistantAssociations (ListAssistantAssociations'),
    newListAssistantAssociations,
    ListAssistantAssociationsResponse (ListAssistantAssociationsResponse'),
    newListAssistantAssociationsResponse,

    -- ** ListAssistants (Paginated)
    ListAssistants (ListAssistants'),
    newListAssistants,
    ListAssistantsResponse (ListAssistantsResponse'),
    newListAssistantsResponse,

    -- ** ListContents (Paginated)
    ListContents (ListContents'),
    newListContents,
    ListContentsResponse (ListContentsResponse'),
    newListContentsResponse,

    -- ** ListKnowledgeBases (Paginated)
    ListKnowledgeBases (ListKnowledgeBases'),
    newListKnowledgeBases,
    ListKnowledgeBasesResponse (ListKnowledgeBasesResponse'),
    newListKnowledgeBasesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** NotifyRecommendationsReceived
    NotifyRecommendationsReceived (NotifyRecommendationsReceived'),
    newNotifyRecommendationsReceived,
    NotifyRecommendationsReceivedResponse (NotifyRecommendationsReceivedResponse'),
    newNotifyRecommendationsReceivedResponse,

    -- ** QueryAssistant (Paginated)
    QueryAssistant (QueryAssistant'),
    newQueryAssistant,
    QueryAssistantResponse (QueryAssistantResponse'),
    newQueryAssistantResponse,

    -- ** RemoveKnowledgeBaseTemplateUri
    RemoveKnowledgeBaseTemplateUri (RemoveKnowledgeBaseTemplateUri'),
    newRemoveKnowledgeBaseTemplateUri,
    RemoveKnowledgeBaseTemplateUriResponse (RemoveKnowledgeBaseTemplateUriResponse'),
    newRemoveKnowledgeBaseTemplateUriResponse,

    -- ** SearchContent (Paginated)
    SearchContent (SearchContent'),
    newSearchContent,
    SearchContentResponse (SearchContentResponse'),
    newSearchContentResponse,

    -- ** SearchSessions (Paginated)
    SearchSessions (SearchSessions'),
    newSearchSessions,
    SearchSessionsResponse (SearchSessionsResponse'),
    newSearchSessionsResponse,

    -- ** StartContentUpload
    StartContentUpload (StartContentUpload'),
    newStartContentUpload,
    StartContentUploadResponse (StartContentUploadResponse'),
    newStartContentUploadResponse,

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

    -- ** UpdateContent
    UpdateContent (UpdateContent'),
    newUpdateContent,
    UpdateContentResponse (UpdateContentResponse'),
    newUpdateContentResponse,

    -- ** UpdateKnowledgeBaseTemplateUri
    UpdateKnowledgeBaseTemplateUri (UpdateKnowledgeBaseTemplateUri'),
    newUpdateKnowledgeBaseTemplateUri,
    UpdateKnowledgeBaseTemplateUriResponse (UpdateKnowledgeBaseTemplateUriResponse'),
    newUpdateKnowledgeBaseTemplateUriResponse,

    -- * Types

    -- ** AssistantStatus
    AssistantStatus (..),

    -- ** AssistantType
    AssistantType (..),

    -- ** AssociationType
    AssociationType (..),

    -- ** ContentStatus
    ContentStatus (..),

    -- ** FilterField
    FilterField (..),

    -- ** FilterOperator
    FilterOperator (..),

    -- ** KnowledgeBaseStatus
    KnowledgeBaseStatus (..),

    -- ** KnowledgeBaseType
    KnowledgeBaseType (..),

    -- ** RecommendationSourceType
    RecommendationSourceType (..),

    -- ** RecommendationTriggerType
    RecommendationTriggerType (..),

    -- ** RecommendationType
    RecommendationType (..),

    -- ** RelevanceLevel
    RelevanceLevel (..),

    -- ** AppIntegrationsConfiguration
    AppIntegrationsConfiguration (AppIntegrationsConfiguration'),
    newAppIntegrationsConfiguration,

    -- ** AssistantAssociationData
    AssistantAssociationData (AssistantAssociationData'),
    newAssistantAssociationData,

    -- ** AssistantAssociationInputData
    AssistantAssociationInputData (AssistantAssociationInputData'),
    newAssistantAssociationInputData,

    -- ** AssistantAssociationOutputData
    AssistantAssociationOutputData (AssistantAssociationOutputData'),
    newAssistantAssociationOutputData,

    -- ** AssistantAssociationSummary
    AssistantAssociationSummary (AssistantAssociationSummary'),
    newAssistantAssociationSummary,

    -- ** AssistantData
    AssistantData (AssistantData'),
    newAssistantData,

    -- ** AssistantSummary
    AssistantSummary (AssistantSummary'),
    newAssistantSummary,

    -- ** ContentData
    ContentData (ContentData'),
    newContentData,

    -- ** ContentReference
    ContentReference (ContentReference'),
    newContentReference,

    -- ** ContentSummary
    ContentSummary (ContentSummary'),
    newContentSummary,

    -- ** Document
    Document (Document'),
    newDocument,

    -- ** DocumentText
    DocumentText (DocumentText'),
    newDocumentText,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** Highlight
    Highlight (Highlight'),
    newHighlight,

    -- ** KnowledgeBaseAssociationData
    KnowledgeBaseAssociationData (KnowledgeBaseAssociationData'),
    newKnowledgeBaseAssociationData,

    -- ** KnowledgeBaseData
    KnowledgeBaseData (KnowledgeBaseData'),
    newKnowledgeBaseData,

    -- ** KnowledgeBaseSummary
    KnowledgeBaseSummary (KnowledgeBaseSummary'),
    newKnowledgeBaseSummary,

    -- ** NotifyRecommendationsReceivedError
    NotifyRecommendationsReceivedError (NotifyRecommendationsReceivedError'),
    newNotifyRecommendationsReceivedError,

    -- ** QueryRecommendationTriggerData
    QueryRecommendationTriggerData (QueryRecommendationTriggerData'),
    newQueryRecommendationTriggerData,

    -- ** RecommendationData
    RecommendationData (RecommendationData'),
    newRecommendationData,

    -- ** RecommendationTrigger
    RecommendationTrigger (RecommendationTrigger'),
    newRecommendationTrigger,

    -- ** RecommendationTriggerData
    RecommendationTriggerData (RecommendationTriggerData'),
    newRecommendationTriggerData,

    -- ** RenderingConfiguration
    RenderingConfiguration (RenderingConfiguration'),
    newRenderingConfiguration,

    -- ** ResultData
    ResultData (ResultData'),
    newResultData,

    -- ** SearchExpression
    SearchExpression (SearchExpression'),
    newSearchExpression,

    -- ** ServerSideEncryptionConfiguration
    ServerSideEncryptionConfiguration (ServerSideEncryptionConfiguration'),
    newServerSideEncryptionConfiguration,

    -- ** SessionData
    SessionData (SessionData'),
    newSessionData,

    -- ** SessionSummary
    SessionSummary (SessionSummary'),
    newSessionSummary,

    -- ** SourceConfiguration
    SourceConfiguration (SourceConfiguration'),
    newSourceConfiguration,
  )
where

import Amazonka.Wisdom.CreateAssistant
import Amazonka.Wisdom.CreateAssistantAssociation
import Amazonka.Wisdom.CreateContent
import Amazonka.Wisdom.CreateKnowledgeBase
import Amazonka.Wisdom.CreateSession
import Amazonka.Wisdom.DeleteAssistant
import Amazonka.Wisdom.DeleteAssistantAssociation
import Amazonka.Wisdom.DeleteContent
import Amazonka.Wisdom.DeleteKnowledgeBase
import Amazonka.Wisdom.GetAssistant
import Amazonka.Wisdom.GetAssistantAssociation
import Amazonka.Wisdom.GetContent
import Amazonka.Wisdom.GetContentSummary
import Amazonka.Wisdom.GetKnowledgeBase
import Amazonka.Wisdom.GetRecommendations
import Amazonka.Wisdom.GetSession
import Amazonka.Wisdom.Lens
import Amazonka.Wisdom.ListAssistantAssociations
import Amazonka.Wisdom.ListAssistants
import Amazonka.Wisdom.ListContents
import Amazonka.Wisdom.ListKnowledgeBases
import Amazonka.Wisdom.ListTagsForResource
import Amazonka.Wisdom.NotifyRecommendationsReceived
import Amazonka.Wisdom.QueryAssistant
import Amazonka.Wisdom.RemoveKnowledgeBaseTemplateUri
import Amazonka.Wisdom.SearchContent
import Amazonka.Wisdom.SearchSessions
import Amazonka.Wisdom.StartContentUpload
import Amazonka.Wisdom.TagResource
import Amazonka.Wisdom.Types
import Amazonka.Wisdom.UntagResource
import Amazonka.Wisdom.UpdateContent
import Amazonka.Wisdom.UpdateKnowledgeBaseTemplateUri
import Amazonka.Wisdom.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Wisdom'.

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

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Wisdom.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _PreconditionFailedException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _TooManyTagsException,
    _ValidationException,

    -- * AssistantStatus
    AssistantStatus (..),

    -- * AssistantType
    AssistantType (..),

    -- * AssociationType
    AssociationType (..),

    -- * ContentStatus
    ContentStatus (..),

    -- * FilterField
    FilterField (..),

    -- * FilterOperator
    FilterOperator (..),

    -- * KnowledgeBaseStatus
    KnowledgeBaseStatus (..),

    -- * KnowledgeBaseType
    KnowledgeBaseType (..),

    -- * RecommendationSourceType
    RecommendationSourceType (..),

    -- * RecommendationTriggerType
    RecommendationTriggerType (..),

    -- * RecommendationType
    RecommendationType (..),

    -- * RelevanceLevel
    RelevanceLevel (..),

    -- * AppIntegrationsConfiguration
    AppIntegrationsConfiguration (..),
    newAppIntegrationsConfiguration,
    appIntegrationsConfiguration_appIntegrationArn,
    appIntegrationsConfiguration_objectFields,

    -- * AssistantAssociationData
    AssistantAssociationData (..),
    newAssistantAssociationData,
    assistantAssociationData_tags,
    assistantAssociationData_assistantArn,
    assistantAssociationData_assistantAssociationArn,
    assistantAssociationData_assistantAssociationId,
    assistantAssociationData_assistantId,
    assistantAssociationData_associationData,
    assistantAssociationData_associationType,

    -- * AssistantAssociationInputData
    AssistantAssociationInputData (..),
    newAssistantAssociationInputData,
    assistantAssociationInputData_knowledgeBaseId,

    -- * AssistantAssociationOutputData
    AssistantAssociationOutputData (..),
    newAssistantAssociationOutputData,
    assistantAssociationOutputData_knowledgeBaseAssociation,

    -- * AssistantAssociationSummary
    AssistantAssociationSummary (..),
    newAssistantAssociationSummary,
    assistantAssociationSummary_tags,
    assistantAssociationSummary_assistantArn,
    assistantAssociationSummary_assistantAssociationArn,
    assistantAssociationSummary_assistantAssociationId,
    assistantAssociationSummary_assistantId,
    assistantAssociationSummary_associationData,
    assistantAssociationSummary_associationType,

    -- * AssistantData
    AssistantData (..),
    newAssistantData,
    assistantData_description,
    assistantData_serverSideEncryptionConfiguration,
    assistantData_tags,
    assistantData_assistantArn,
    assistantData_assistantId,
    assistantData_name,
    assistantData_status,
    assistantData_type,

    -- * AssistantSummary
    AssistantSummary (..),
    newAssistantSummary,
    assistantSummary_description,
    assistantSummary_serverSideEncryptionConfiguration,
    assistantSummary_tags,
    assistantSummary_assistantArn,
    assistantSummary_assistantId,
    assistantSummary_name,
    assistantSummary_status,
    assistantSummary_type,

    -- * ContentData
    ContentData (..),
    newContentData,
    contentData_linkOutUri,
    contentData_tags,
    contentData_contentArn,
    contentData_contentId,
    contentData_contentType,
    contentData_knowledgeBaseArn,
    contentData_knowledgeBaseId,
    contentData_metadata,
    contentData_name,
    contentData_revisionId,
    contentData_status,
    contentData_title,
    contentData_url,
    contentData_urlExpiry,

    -- * ContentReference
    ContentReference (..),
    newContentReference,
    contentReference_contentArn,
    contentReference_contentId,
    contentReference_knowledgeBaseArn,
    contentReference_knowledgeBaseId,

    -- * ContentSummary
    ContentSummary (..),
    newContentSummary,
    contentSummary_tags,
    contentSummary_contentArn,
    contentSummary_contentId,
    contentSummary_contentType,
    contentSummary_knowledgeBaseArn,
    contentSummary_knowledgeBaseId,
    contentSummary_metadata,
    contentSummary_name,
    contentSummary_revisionId,
    contentSummary_status,
    contentSummary_title,

    -- * Document
    Document (..),
    newDocument,
    document_excerpt,
    document_title,
    document_contentReference,

    -- * DocumentText
    DocumentText (..),
    newDocumentText,
    documentText_highlights,
    documentText_text,

    -- * Filter
    Filter (..),
    newFilter,
    filter_field,
    filter_operator,
    filter_value,

    -- * Highlight
    Highlight (..),
    newHighlight,
    highlight_beginOffsetInclusive,
    highlight_endOffsetExclusive,

    -- * KnowledgeBaseAssociationData
    KnowledgeBaseAssociationData (..),
    newKnowledgeBaseAssociationData,
    knowledgeBaseAssociationData_knowledgeBaseArn,
    knowledgeBaseAssociationData_knowledgeBaseId,

    -- * KnowledgeBaseData
    KnowledgeBaseData (..),
    newKnowledgeBaseData,
    knowledgeBaseData_description,
    knowledgeBaseData_lastContentModificationTime,
    knowledgeBaseData_renderingConfiguration,
    knowledgeBaseData_serverSideEncryptionConfiguration,
    knowledgeBaseData_sourceConfiguration,
    knowledgeBaseData_tags,
    knowledgeBaseData_knowledgeBaseArn,
    knowledgeBaseData_knowledgeBaseId,
    knowledgeBaseData_knowledgeBaseType,
    knowledgeBaseData_name,
    knowledgeBaseData_status,

    -- * KnowledgeBaseSummary
    KnowledgeBaseSummary (..),
    newKnowledgeBaseSummary,
    knowledgeBaseSummary_description,
    knowledgeBaseSummary_renderingConfiguration,
    knowledgeBaseSummary_serverSideEncryptionConfiguration,
    knowledgeBaseSummary_sourceConfiguration,
    knowledgeBaseSummary_tags,
    knowledgeBaseSummary_knowledgeBaseArn,
    knowledgeBaseSummary_knowledgeBaseId,
    knowledgeBaseSummary_knowledgeBaseType,
    knowledgeBaseSummary_name,
    knowledgeBaseSummary_status,

    -- * NotifyRecommendationsReceivedError
    NotifyRecommendationsReceivedError (..),
    newNotifyRecommendationsReceivedError,
    notifyRecommendationsReceivedError_message,
    notifyRecommendationsReceivedError_recommendationId,

    -- * QueryRecommendationTriggerData
    QueryRecommendationTriggerData (..),
    newQueryRecommendationTriggerData,
    queryRecommendationTriggerData_text,

    -- * RecommendationData
    RecommendationData (..),
    newRecommendationData,
    recommendationData_relevanceLevel,
    recommendationData_relevanceScore,
    recommendationData_type,
    recommendationData_document,
    recommendationData_recommendationId,

    -- * RecommendationTrigger
    RecommendationTrigger (..),
    newRecommendationTrigger,
    recommendationTrigger_data,
    recommendationTrigger_id,
    recommendationTrigger_recommendationIds,
    recommendationTrigger_source,
    recommendationTrigger_type,

    -- * RecommendationTriggerData
    RecommendationTriggerData (..),
    newRecommendationTriggerData,
    recommendationTriggerData_query,

    -- * RenderingConfiguration
    RenderingConfiguration (..),
    newRenderingConfiguration,
    renderingConfiguration_templateUri,

    -- * ResultData
    ResultData (..),
    newResultData,
    resultData_relevanceScore,
    resultData_document,
    resultData_resultId,

    -- * SearchExpression
    SearchExpression (..),
    newSearchExpression,
    searchExpression_filters,

    -- * ServerSideEncryptionConfiguration
    ServerSideEncryptionConfiguration (..),
    newServerSideEncryptionConfiguration,
    serverSideEncryptionConfiguration_kmsKeyId,

    -- * SessionData
    SessionData (..),
    newSessionData,
    sessionData_description,
    sessionData_tags,
    sessionData_name,
    sessionData_sessionArn,
    sessionData_sessionId,

    -- * SessionSummary
    SessionSummary (..),
    newSessionSummary,
    sessionSummary_assistantArn,
    sessionSummary_assistantId,
    sessionSummary_sessionArn,
    sessionSummary_sessionId,

    -- * SourceConfiguration
    SourceConfiguration (..),
    newSourceConfiguration,
    sourceConfiguration_appIntegrations,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.Wisdom.Types.AppIntegrationsConfiguration
import Amazonka.Wisdom.Types.AssistantAssociationData
import Amazonka.Wisdom.Types.AssistantAssociationInputData
import Amazonka.Wisdom.Types.AssistantAssociationOutputData
import Amazonka.Wisdom.Types.AssistantAssociationSummary
import Amazonka.Wisdom.Types.AssistantData
import Amazonka.Wisdom.Types.AssistantStatus
import Amazonka.Wisdom.Types.AssistantSummary
import Amazonka.Wisdom.Types.AssistantType
import Amazonka.Wisdom.Types.AssociationType
import Amazonka.Wisdom.Types.ContentData
import Amazonka.Wisdom.Types.ContentReference
import Amazonka.Wisdom.Types.ContentStatus
import Amazonka.Wisdom.Types.ContentSummary
import Amazonka.Wisdom.Types.Document
import Amazonka.Wisdom.Types.DocumentText
import Amazonka.Wisdom.Types.Filter
import Amazonka.Wisdom.Types.FilterField
import Amazonka.Wisdom.Types.FilterOperator
import Amazonka.Wisdom.Types.Highlight
import Amazonka.Wisdom.Types.KnowledgeBaseAssociationData
import Amazonka.Wisdom.Types.KnowledgeBaseData
import Amazonka.Wisdom.Types.KnowledgeBaseStatus
import Amazonka.Wisdom.Types.KnowledgeBaseSummary
import Amazonka.Wisdom.Types.KnowledgeBaseType
import Amazonka.Wisdom.Types.NotifyRecommendationsReceivedError
import Amazonka.Wisdom.Types.QueryRecommendationTriggerData
import Amazonka.Wisdom.Types.RecommendationData
import Amazonka.Wisdom.Types.RecommendationSourceType
import Amazonka.Wisdom.Types.RecommendationTrigger
import Amazonka.Wisdom.Types.RecommendationTriggerData
import Amazonka.Wisdom.Types.RecommendationTriggerType
import Amazonka.Wisdom.Types.RecommendationType
import Amazonka.Wisdom.Types.RelevanceLevel
import Amazonka.Wisdom.Types.RenderingConfiguration
import Amazonka.Wisdom.Types.ResultData
import Amazonka.Wisdom.Types.SearchExpression
import Amazonka.Wisdom.Types.ServerSideEncryptionConfiguration
import Amazonka.Wisdom.Types.SessionData
import Amazonka.Wisdom.Types.SessionSummary
import Amazonka.Wisdom.Types.SourceConfiguration

-- | API version @2020-10-19@ of the Amazon Connect Wisdom Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Wisdom",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "wisdom",
      Core.signingName = "wisdom",
      Core.version = "2020-10-19",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Wisdom",
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

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The request could not be processed because of conflict in the current
-- state of the resource. For example, if you\'re using a @Create@ API
-- (such as @CreateAssistant@) that accepts name, a conflicting resource
-- (usually with the same name) is being created or mutated.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The provided @revisionId@ does not match, indicating the content has
-- been modified since it was last read.
_PreconditionFailedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PreconditionFailedException =
  Core._MatchServiceError
    defaultService
    "PreconditionFailedException"
    Prelude.. Core.hasStatus 412

-- | The specified resource does not exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | You\'ve exceeded your service quota. To perform the requested action,
-- remove some of the relevant resources, or use service quotas to request
-- a service quota increase.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Amazon Connect Wisdom throws this exception if you have too many tags in
-- your tag set.
_TooManyTagsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"
    Prelude.. Core.hasStatus 400

-- | The input fails to satisfy the constraints specified by a service.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ConnectCases.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * CommentBodyTextType
    CommentBodyTextType (..),

    -- * DomainStatus
    DomainStatus (..),

    -- * FieldNamespace
    FieldNamespace (..),

    -- * FieldType
    FieldType (..),

    -- * Order
    Order (..),

    -- * RelatedItemType
    RelatedItemType (..),

    -- * TemplateStatus
    TemplateStatus (..),

    -- * BasicLayout
    BasicLayout (..),
    newBasicLayout,
    basicLayout_moreInfo,
    basicLayout_topPanel,

    -- * CaseEventIncludedData
    CaseEventIncludedData (..),
    newCaseEventIncludedData,
    caseEventIncludedData_fields,

    -- * CaseFilter
    CaseFilter (..),
    newCaseFilter,
    caseFilter_andAll,
    caseFilter_field,
    caseFilter_not,

    -- * CaseSummary
    CaseSummary (..),
    newCaseSummary,
    caseSummary_caseId,
    caseSummary_templateId,

    -- * CommentContent
    CommentContent (..),
    newCommentContent,
    commentContent_body,
    commentContent_contentType,

    -- * CommentFilter
    CommentFilter (..),
    newCommentFilter,

    -- * Contact
    Contact (..),
    newContact,
    contact_contactArn,

    -- * ContactContent
    ContactContent (..),
    newContactContent,
    contactContent_channel,
    contactContent_connectedToSystemTime,
    contactContent_contactArn,

    -- * ContactFilter
    ContactFilter (..),
    newContactFilter,
    contactFilter_channel,
    contactFilter_contactArn,

    -- * DomainSummary
    DomainSummary (..),
    newDomainSummary,
    domainSummary_domainArn,
    domainSummary_domainId,
    domainSummary_name,

    -- * EventBridgeConfiguration
    EventBridgeConfiguration (..),
    newEventBridgeConfiguration,
    eventBridgeConfiguration_includedData,
    eventBridgeConfiguration_enabled,

    -- * EventIncludedData
    EventIncludedData (..),
    newEventIncludedData,
    eventIncludedData_caseData,
    eventIncludedData_relatedItemData,

    -- * FieldError
    FieldError (..),
    newFieldError,
    fieldError_message,
    fieldError_errorCode,
    fieldError_id,

    -- * FieldFilter
    FieldFilter (..),
    newFieldFilter,
    fieldFilter_contains,
    fieldFilter_equalTo,
    fieldFilter_greaterThan,
    fieldFilter_greaterThanOrEqualTo,
    fieldFilter_lessThan,
    fieldFilter_lessThanOrEqualTo,

    -- * FieldGroup
    FieldGroup (..),
    newFieldGroup,
    fieldGroup_name,
    fieldGroup_fields,

    -- * FieldIdentifier
    FieldIdentifier (..),
    newFieldIdentifier,
    fieldIdentifier_id,

    -- * FieldItem
    FieldItem (..),
    newFieldItem,
    fieldItem_id,

    -- * FieldOption
    FieldOption (..),
    newFieldOption,
    fieldOption_active,
    fieldOption_name,
    fieldOption_value,

    -- * FieldOptionError
    FieldOptionError (..),
    newFieldOptionError,
    fieldOptionError_errorCode,
    fieldOptionError_message,
    fieldOptionError_value,

    -- * FieldSummary
    FieldSummary (..),
    newFieldSummary,
    fieldSummary_fieldArn,
    fieldSummary_fieldId,
    fieldSummary_name,
    fieldSummary_namespace,
    fieldSummary_type,

    -- * FieldValue
    FieldValue (..),
    newFieldValue,
    fieldValue_id,
    fieldValue_value,

    -- * FieldValueUnion
    FieldValueUnion (..),
    newFieldValueUnion,
    fieldValueUnion_booleanValue,
    fieldValueUnion_doubleValue,
    fieldValueUnion_stringValue,

    -- * GetFieldResponse
    GetFieldResponse (..),
    newGetFieldResponse,
    getFieldResponse_description,
    getFieldResponse_tags,
    getFieldResponse_fieldArn,
    getFieldResponse_fieldId,
    getFieldResponse_name,
    getFieldResponse_namespace,
    getFieldResponse_type,

    -- * LayoutConfiguration
    LayoutConfiguration (..),
    newLayoutConfiguration,
    layoutConfiguration_defaultLayout,

    -- * LayoutContent
    LayoutContent (..),
    newLayoutContent,
    layoutContent_basic,

    -- * LayoutSections
    LayoutSections (..),
    newLayoutSections,
    layoutSections_sections,

    -- * LayoutSummary
    LayoutSummary (..),
    newLayoutSummary,
    layoutSummary_layoutArn,
    layoutSummary_layoutId,
    layoutSummary_name,

    -- * RelatedItemContent
    RelatedItemContent (..),
    newRelatedItemContent,
    relatedItemContent_comment,
    relatedItemContent_contact,

    -- * RelatedItemEventIncludedData
    RelatedItemEventIncludedData (..),
    newRelatedItemEventIncludedData,
    relatedItemEventIncludedData_includeContent,

    -- * RelatedItemInputContent
    RelatedItemInputContent (..),
    newRelatedItemInputContent,
    relatedItemInputContent_comment,
    relatedItemInputContent_contact,

    -- * RelatedItemTypeFilter
    RelatedItemTypeFilter (..),
    newRelatedItemTypeFilter,
    relatedItemTypeFilter_comment,
    relatedItemTypeFilter_contact,

    -- * RequiredField
    RequiredField (..),
    newRequiredField,
    requiredField_fieldId,

    -- * SearchCasesResponseItem
    SearchCasesResponseItem (..),
    newSearchCasesResponseItem,
    searchCasesResponseItem_tags,
    searchCasesResponseItem_caseId,
    searchCasesResponseItem_fields,
    searchCasesResponseItem_templateId,

    -- * SearchRelatedItemsResponseItem
    SearchRelatedItemsResponseItem (..),
    newSearchRelatedItemsResponseItem,
    searchRelatedItemsResponseItem_tags,
    searchRelatedItemsResponseItem_associationTime,
    searchRelatedItemsResponseItem_content,
    searchRelatedItemsResponseItem_relatedItemId,
    searchRelatedItemsResponseItem_type,

    -- * Section
    Section (..),
    newSection,
    section_fieldGroup,

    -- * Sort
    Sort (..),
    newSort,
    sort_fieldId,
    sort_sortOrder,

    -- * TemplateSummary
    TemplateSummary (..),
    newTemplateSummary,
    templateSummary_name,
    templateSummary_status,
    templateSummary_templateArn,
    templateSummary_templateId,
  )
where

import Amazonka.ConnectCases.Types.BasicLayout
import Amazonka.ConnectCases.Types.CaseEventIncludedData
import Amazonka.ConnectCases.Types.CaseFilter
import Amazonka.ConnectCases.Types.CaseSummary
import Amazonka.ConnectCases.Types.CommentBodyTextType
import Amazonka.ConnectCases.Types.CommentContent
import Amazonka.ConnectCases.Types.CommentFilter
import Amazonka.ConnectCases.Types.Contact
import Amazonka.ConnectCases.Types.ContactContent
import Amazonka.ConnectCases.Types.ContactFilter
import Amazonka.ConnectCases.Types.DomainStatus
import Amazonka.ConnectCases.Types.DomainSummary
import Amazonka.ConnectCases.Types.EventBridgeConfiguration
import Amazonka.ConnectCases.Types.EventIncludedData
import Amazonka.ConnectCases.Types.FieldError
import Amazonka.ConnectCases.Types.FieldFilter
import Amazonka.ConnectCases.Types.FieldGroup
import Amazonka.ConnectCases.Types.FieldIdentifier
import Amazonka.ConnectCases.Types.FieldItem
import Amazonka.ConnectCases.Types.FieldNamespace
import Amazonka.ConnectCases.Types.FieldOption
import Amazonka.ConnectCases.Types.FieldOptionError
import Amazonka.ConnectCases.Types.FieldSummary
import Amazonka.ConnectCases.Types.FieldType
import Amazonka.ConnectCases.Types.FieldValue
import Amazonka.ConnectCases.Types.FieldValueUnion
import Amazonka.ConnectCases.Types.GetFieldResponse
import Amazonka.ConnectCases.Types.LayoutConfiguration
import Amazonka.ConnectCases.Types.LayoutContent
import Amazonka.ConnectCases.Types.LayoutSections
import Amazonka.ConnectCases.Types.LayoutSummary
import Amazonka.ConnectCases.Types.Order
import Amazonka.ConnectCases.Types.RelatedItemContent
import Amazonka.ConnectCases.Types.RelatedItemEventIncludedData
import Amazonka.ConnectCases.Types.RelatedItemInputContent
import Amazonka.ConnectCases.Types.RelatedItemType
import Amazonka.ConnectCases.Types.RelatedItemTypeFilter
import Amazonka.ConnectCases.Types.RequiredField
import Amazonka.ConnectCases.Types.SearchCasesResponseItem
import Amazonka.ConnectCases.Types.SearchRelatedItemsResponseItem
import Amazonka.ConnectCases.Types.Section
import Amazonka.ConnectCases.Types.Sort
import Amazonka.ConnectCases.Types.TemplateStatus
import Amazonka.ConnectCases.Types.TemplateSummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2022-10-03@ of the Amazon Connect Cases SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ConnectCases",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "cases",
      Core.signingName = "cases",
      Core.version = "2022-10-03",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "ConnectCases",
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
_AccessDeniedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The requested operation would cause a conflict with the current state of
-- a service resource associated with the request. Resolve the conflict
-- before retrying this request. See the accompanying error message for
-- details.
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | We couldn\'t process your request because of an issue with the server.
-- Try again later.
_InternalServerException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | We couldn\'t find the requested resource. Check that your resources
-- exists and were created in the same Amazon Web Services Region as your
-- request, and try your request again.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The service quota has been exceeded. For a list of service quotas, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html Amazon Connect Service Quotas>
-- in the /Amazon Connect Administrator Guide/.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The rate has been exceeded for this API. Please try again after a few
-- minutes.
_ThrottlingException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The request isn\'t valid. Check the syntax and try again.
_ValidationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

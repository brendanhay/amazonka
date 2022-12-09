{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ConnectCases
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2022-10-03@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Welcome to the Amazon Connect Cases API Reference. This guide provides
-- information about the Amazon Connect Cases API, which you can use to
-- create, update, get, and list Cases domains, fields, field options,
-- layouts, templates, cases, related items, and tags.
--
-- >  <p>For more information about Amazon Connect Cases, see <a href="https://docs.aws.amazon.com/connect/latest/adminguide/cases.html">Amazon Connect Cases</a> in the <i>Amazon Connect Administrator Guide</i>. </p>
module Amazonka.ConnectCases
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchGetField
    BatchGetField (BatchGetField'),
    newBatchGetField,
    BatchGetFieldResponse (BatchGetFieldResponse'),
    newBatchGetFieldResponse,

    -- ** BatchPutFieldOptions
    BatchPutFieldOptions (BatchPutFieldOptions'),
    newBatchPutFieldOptions,
    BatchPutFieldOptionsResponse (BatchPutFieldOptionsResponse'),
    newBatchPutFieldOptionsResponse,

    -- ** CreateCase
    CreateCase (CreateCase'),
    newCreateCase,
    CreateCaseResponse (CreateCaseResponse'),
    newCreateCaseResponse,

    -- ** CreateDomain
    CreateDomain (CreateDomain'),
    newCreateDomain,
    CreateDomainResponse (CreateDomainResponse'),
    newCreateDomainResponse,

    -- ** CreateField
    CreateField (CreateField'),
    newCreateField,
    CreateFieldResponse (CreateFieldResponse'),
    newCreateFieldResponse,

    -- ** CreateLayout
    CreateLayout (CreateLayout'),
    newCreateLayout,
    CreateLayoutResponse (CreateLayoutResponse'),
    newCreateLayoutResponse,

    -- ** CreateRelatedItem
    CreateRelatedItem (CreateRelatedItem'),
    newCreateRelatedItem,
    CreateRelatedItemResponse (CreateRelatedItemResponse'),
    newCreateRelatedItemResponse,

    -- ** CreateTemplate
    CreateTemplate (CreateTemplate'),
    newCreateTemplate,
    CreateTemplateResponse (CreateTemplateResponse'),
    newCreateTemplateResponse,

    -- ** GetCase
    GetCase (GetCase'),
    newGetCase,
    GetCaseResponse (GetCaseResponse'),
    newGetCaseResponse,

    -- ** GetCaseEventConfiguration
    GetCaseEventConfiguration (GetCaseEventConfiguration'),
    newGetCaseEventConfiguration,
    GetCaseEventConfigurationResponse (GetCaseEventConfigurationResponse'),
    newGetCaseEventConfigurationResponse,

    -- ** GetDomain
    GetDomain (GetDomain'),
    newGetDomain,
    GetDomainResponse (GetDomainResponse'),
    newGetDomainResponse,

    -- ** GetLayout
    GetLayout (GetLayout'),
    newGetLayout,
    GetLayoutResponse (GetLayoutResponse'),
    newGetLayoutResponse,

    -- ** GetTemplate
    GetTemplate (GetTemplate'),
    newGetTemplate,
    GetTemplateResponse (GetTemplateResponse'),
    newGetTemplateResponse,

    -- ** ListCasesForContact
    ListCasesForContact (ListCasesForContact'),
    newListCasesForContact,
    ListCasesForContactResponse (ListCasesForContactResponse'),
    newListCasesForContactResponse,

    -- ** ListDomains
    ListDomains (ListDomains'),
    newListDomains,
    ListDomainsResponse (ListDomainsResponse'),
    newListDomainsResponse,

    -- ** ListFieldOptions
    ListFieldOptions (ListFieldOptions'),
    newListFieldOptions,
    ListFieldOptionsResponse (ListFieldOptionsResponse'),
    newListFieldOptionsResponse,

    -- ** ListFields
    ListFields (ListFields'),
    newListFields,
    ListFieldsResponse (ListFieldsResponse'),
    newListFieldsResponse,

    -- ** ListLayouts
    ListLayouts (ListLayouts'),
    newListLayouts,
    ListLayoutsResponse (ListLayoutsResponse'),
    newListLayoutsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTemplates
    ListTemplates (ListTemplates'),
    newListTemplates,
    ListTemplatesResponse (ListTemplatesResponse'),
    newListTemplatesResponse,

    -- ** PutCaseEventConfiguration
    PutCaseEventConfiguration (PutCaseEventConfiguration'),
    newPutCaseEventConfiguration,
    PutCaseEventConfigurationResponse (PutCaseEventConfigurationResponse'),
    newPutCaseEventConfigurationResponse,

    -- ** SearchCases (Paginated)
    SearchCases (SearchCases'),
    newSearchCases,
    SearchCasesResponse (SearchCasesResponse'),
    newSearchCasesResponse,

    -- ** SearchRelatedItems (Paginated)
    SearchRelatedItems (SearchRelatedItems'),
    newSearchRelatedItems,
    SearchRelatedItemsResponse (SearchRelatedItemsResponse'),
    newSearchRelatedItemsResponse,

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

    -- ** UpdateCase
    UpdateCase (UpdateCase'),
    newUpdateCase,
    UpdateCaseResponse (UpdateCaseResponse'),
    newUpdateCaseResponse,

    -- ** UpdateField
    UpdateField (UpdateField'),
    newUpdateField,
    UpdateFieldResponse (UpdateFieldResponse'),
    newUpdateFieldResponse,

    -- ** UpdateLayout
    UpdateLayout (UpdateLayout'),
    newUpdateLayout,
    UpdateLayoutResponse (UpdateLayoutResponse'),
    newUpdateLayoutResponse,

    -- ** UpdateTemplate
    UpdateTemplate (UpdateTemplate'),
    newUpdateTemplate,
    UpdateTemplateResponse (UpdateTemplateResponse'),
    newUpdateTemplateResponse,

    -- * Types

    -- ** CommentBodyTextType
    CommentBodyTextType (..),

    -- ** DomainStatus
    DomainStatus (..),

    -- ** FieldNamespace
    FieldNamespace (..),

    -- ** FieldType
    FieldType (..),

    -- ** Order
    Order (..),

    -- ** RelatedItemType
    RelatedItemType (..),

    -- ** TemplateStatus
    TemplateStatus (..),

    -- ** BasicLayout
    BasicLayout (BasicLayout'),
    newBasicLayout,

    -- ** CaseEventIncludedData
    CaseEventIncludedData (CaseEventIncludedData'),
    newCaseEventIncludedData,

    -- ** CaseFilter
    CaseFilter (CaseFilter'),
    newCaseFilter,

    -- ** CaseSummary
    CaseSummary (CaseSummary'),
    newCaseSummary,

    -- ** CommentContent
    CommentContent (CommentContent'),
    newCommentContent,

    -- ** CommentFilter
    CommentFilter (CommentFilter'),
    newCommentFilter,

    -- ** Contact
    Contact (Contact'),
    newContact,

    -- ** ContactContent
    ContactContent (ContactContent'),
    newContactContent,

    -- ** ContactFilter
    ContactFilter (ContactFilter'),
    newContactFilter,

    -- ** DomainSummary
    DomainSummary (DomainSummary'),
    newDomainSummary,

    -- ** EventBridgeConfiguration
    EventBridgeConfiguration (EventBridgeConfiguration'),
    newEventBridgeConfiguration,

    -- ** EventIncludedData
    EventIncludedData (EventIncludedData'),
    newEventIncludedData,

    -- ** FieldError
    FieldError (FieldError'),
    newFieldError,

    -- ** FieldFilter
    FieldFilter (FieldFilter'),
    newFieldFilter,

    -- ** FieldGroup
    FieldGroup (FieldGroup'),
    newFieldGroup,

    -- ** FieldIdentifier
    FieldIdentifier (FieldIdentifier'),
    newFieldIdentifier,

    -- ** FieldItem
    FieldItem (FieldItem'),
    newFieldItem,

    -- ** FieldOption
    FieldOption (FieldOption'),
    newFieldOption,

    -- ** FieldOptionError
    FieldOptionError (FieldOptionError'),
    newFieldOptionError,

    -- ** FieldSummary
    FieldSummary (FieldSummary'),
    newFieldSummary,

    -- ** FieldValue
    FieldValue (FieldValue'),
    newFieldValue,

    -- ** FieldValueUnion
    FieldValueUnion (FieldValueUnion'),
    newFieldValueUnion,

    -- ** GetFieldResponse
    GetFieldResponse (GetFieldResponse'),
    newGetFieldResponse,

    -- ** LayoutConfiguration
    LayoutConfiguration (LayoutConfiguration'),
    newLayoutConfiguration,

    -- ** LayoutContent
    LayoutContent (LayoutContent'),
    newLayoutContent,

    -- ** LayoutSections
    LayoutSections (LayoutSections'),
    newLayoutSections,

    -- ** LayoutSummary
    LayoutSummary (LayoutSummary'),
    newLayoutSummary,

    -- ** RelatedItemContent
    RelatedItemContent (RelatedItemContent'),
    newRelatedItemContent,

    -- ** RelatedItemEventIncludedData
    RelatedItemEventIncludedData (RelatedItemEventIncludedData'),
    newRelatedItemEventIncludedData,

    -- ** RelatedItemInputContent
    RelatedItemInputContent (RelatedItemInputContent'),
    newRelatedItemInputContent,

    -- ** RelatedItemTypeFilter
    RelatedItemTypeFilter (RelatedItemTypeFilter'),
    newRelatedItemTypeFilter,

    -- ** RequiredField
    RequiredField (RequiredField'),
    newRequiredField,

    -- ** SearchCasesResponseItem
    SearchCasesResponseItem (SearchCasesResponseItem'),
    newSearchCasesResponseItem,

    -- ** SearchRelatedItemsResponseItem
    SearchRelatedItemsResponseItem (SearchRelatedItemsResponseItem'),
    newSearchRelatedItemsResponseItem,

    -- ** Section
    Section (Section'),
    newSection,

    -- ** Sort
    Sort (Sort'),
    newSort,

    -- ** TemplateSummary
    TemplateSummary (TemplateSummary'),
    newTemplateSummary,
  )
where

import Amazonka.ConnectCases.BatchGetField
import Amazonka.ConnectCases.BatchPutFieldOptions
import Amazonka.ConnectCases.CreateCase
import Amazonka.ConnectCases.CreateDomain
import Amazonka.ConnectCases.CreateField
import Amazonka.ConnectCases.CreateLayout
import Amazonka.ConnectCases.CreateRelatedItem
import Amazonka.ConnectCases.CreateTemplate
import Amazonka.ConnectCases.GetCase
import Amazonka.ConnectCases.GetCaseEventConfiguration
import Amazonka.ConnectCases.GetDomain
import Amazonka.ConnectCases.GetLayout
import Amazonka.ConnectCases.GetTemplate
import Amazonka.ConnectCases.Lens
import Amazonka.ConnectCases.ListCasesForContact
import Amazonka.ConnectCases.ListDomains
import Amazonka.ConnectCases.ListFieldOptions
import Amazonka.ConnectCases.ListFields
import Amazonka.ConnectCases.ListLayouts
import Amazonka.ConnectCases.ListTagsForResource
import Amazonka.ConnectCases.ListTemplates
import Amazonka.ConnectCases.PutCaseEventConfiguration
import Amazonka.ConnectCases.SearchCases
import Amazonka.ConnectCases.SearchRelatedItems
import Amazonka.ConnectCases.TagResource
import Amazonka.ConnectCases.Types
import Amazonka.ConnectCases.UntagResource
import Amazonka.ConnectCases.UpdateCase
import Amazonka.ConnectCases.UpdateField
import Amazonka.ConnectCases.UpdateLayout
import Amazonka.ConnectCases.UpdateTemplate
import Amazonka.ConnectCases.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ConnectCases'.

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

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ConnectCases.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Lens
  ( -- * Operations

    -- ** BatchGetField
    batchGetField_domainId,
    batchGetField_fields,
    batchGetFieldResponse_httpStatus,
    batchGetFieldResponse_errors,
    batchGetFieldResponse_fields,

    -- ** BatchPutFieldOptions
    batchPutFieldOptions_domainId,
    batchPutFieldOptions_fieldId,
    batchPutFieldOptions_options,
    batchPutFieldOptionsResponse_errors,
    batchPutFieldOptionsResponse_httpStatus,

    -- ** CreateCase
    createCase_clientToken,
    createCase_domainId,
    createCase_fields,
    createCase_templateId,
    createCaseResponse_httpStatus,
    createCaseResponse_caseArn,
    createCaseResponse_caseId,

    -- ** CreateDomain
    createDomain_name,
    createDomainResponse_httpStatus,
    createDomainResponse_domainArn,
    createDomainResponse_domainId,
    createDomainResponse_domainStatus,

    -- ** CreateField
    createField_description,
    createField_domainId,
    createField_name,
    createField_type,
    createFieldResponse_httpStatus,
    createFieldResponse_fieldArn,
    createFieldResponse_fieldId,

    -- ** CreateLayout
    createLayout_content,
    createLayout_domainId,
    createLayout_name,
    createLayoutResponse_httpStatus,
    createLayoutResponse_layoutArn,
    createLayoutResponse_layoutId,

    -- ** CreateRelatedItem
    createRelatedItem_caseId,
    createRelatedItem_content,
    createRelatedItem_domainId,
    createRelatedItem_type,
    createRelatedItemResponse_httpStatus,
    createRelatedItemResponse_relatedItemArn,
    createRelatedItemResponse_relatedItemId,

    -- ** CreateTemplate
    createTemplate_description,
    createTemplate_layoutConfiguration,
    createTemplate_requiredFields,
    createTemplate_status,
    createTemplate_domainId,
    createTemplate_name,
    createTemplateResponse_httpStatus,
    createTemplateResponse_templateArn,
    createTemplateResponse_templateId,

    -- ** GetCase
    getCase_nextToken,
    getCase_caseId,
    getCase_domainId,
    getCase_fields,
    getCaseResponse_nextToken,
    getCaseResponse_tags,
    getCaseResponse_httpStatus,
    getCaseResponse_fields,
    getCaseResponse_templateId,

    -- ** GetCaseEventConfiguration
    getCaseEventConfiguration_domainId,
    getCaseEventConfigurationResponse_httpStatus,
    getCaseEventConfigurationResponse_eventBridge,

    -- ** GetDomain
    getDomain_domainId,
    getDomainResponse_tags,
    getDomainResponse_httpStatus,
    getDomainResponse_createdTime,
    getDomainResponse_domainArn,
    getDomainResponse_domainId,
    getDomainResponse_domainStatus,
    getDomainResponse_name,

    -- ** GetLayout
    getLayout_domainId,
    getLayout_layoutId,
    getLayoutResponse_tags,
    getLayoutResponse_httpStatus,
    getLayoutResponse_content,
    getLayoutResponse_layoutArn,
    getLayoutResponse_layoutId,
    getLayoutResponse_name,

    -- ** GetTemplate
    getTemplate_domainId,
    getTemplate_templateId,
    getTemplateResponse_description,
    getTemplateResponse_layoutConfiguration,
    getTemplateResponse_requiredFields,
    getTemplateResponse_tags,
    getTemplateResponse_httpStatus,
    getTemplateResponse_name,
    getTemplateResponse_status,
    getTemplateResponse_templateArn,
    getTemplateResponse_templateId,

    -- ** ListCasesForContact
    listCasesForContact_maxResults,
    listCasesForContact_nextToken,
    listCasesForContact_contactArn,
    listCasesForContact_domainId,
    listCasesForContactResponse_nextToken,
    listCasesForContactResponse_httpStatus,
    listCasesForContactResponse_cases,

    -- ** ListDomains
    listDomains_maxResults,
    listDomains_nextToken,
    listDomainsResponse_nextToken,
    listDomainsResponse_httpStatus,
    listDomainsResponse_domains,

    -- ** ListFieldOptions
    listFieldOptions_maxResults,
    listFieldOptions_nextToken,
    listFieldOptions_values,
    listFieldOptions_domainId,
    listFieldOptions_fieldId,
    listFieldOptionsResponse_nextToken,
    listFieldOptionsResponse_httpStatus,
    listFieldOptionsResponse_options,

    -- ** ListFields
    listFields_maxResults,
    listFields_nextToken,
    listFields_domainId,
    listFieldsResponse_nextToken,
    listFieldsResponse_httpStatus,
    listFieldsResponse_fields,

    -- ** ListLayouts
    listLayouts_maxResults,
    listLayouts_nextToken,
    listLayouts_domainId,
    listLayoutsResponse_nextToken,
    listLayoutsResponse_httpStatus,
    listLayoutsResponse_layouts,

    -- ** ListTagsForResource
    listTagsForResource_arn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTemplates
    listTemplates_maxResults,
    listTemplates_nextToken,
    listTemplates_status,
    listTemplates_domainId,
    listTemplatesResponse_nextToken,
    listTemplatesResponse_httpStatus,
    listTemplatesResponse_templates,

    -- ** PutCaseEventConfiguration
    putCaseEventConfiguration_domainId,
    putCaseEventConfiguration_eventBridge,
    putCaseEventConfigurationResponse_httpStatus,

    -- ** SearchCases
    searchCases_fields,
    searchCases_filter,
    searchCases_maxResults,
    searchCases_nextToken,
    searchCases_searchTerm,
    searchCases_sorts,
    searchCases_domainId,
    searchCasesResponse_nextToken,
    searchCasesResponse_httpStatus,
    searchCasesResponse_cases,

    -- ** SearchRelatedItems
    searchRelatedItems_filters,
    searchRelatedItems_maxResults,
    searchRelatedItems_nextToken,
    searchRelatedItems_caseId,
    searchRelatedItems_domainId,
    searchRelatedItemsResponse_nextToken,
    searchRelatedItemsResponse_httpStatus,
    searchRelatedItemsResponse_relatedItems,

    -- ** TagResource
    tagResource_arn,
    tagResource_tags,

    -- ** UntagResource
    untagResource_arn,
    untagResource_tagKeys,

    -- ** UpdateCase
    updateCase_caseId,
    updateCase_domainId,
    updateCase_fields,
    updateCaseResponse_httpStatus,

    -- ** UpdateField
    updateField_description,
    updateField_name,
    updateField_domainId,
    updateField_fieldId,
    updateFieldResponse_httpStatus,

    -- ** UpdateLayout
    updateLayout_content,
    updateLayout_name,
    updateLayout_domainId,
    updateLayout_layoutId,
    updateLayoutResponse_httpStatus,

    -- ** UpdateTemplate
    updateTemplate_description,
    updateTemplate_layoutConfiguration,
    updateTemplate_name,
    updateTemplate_requiredFields,
    updateTemplate_status,
    updateTemplate_domainId,
    updateTemplate_templateId,
    updateTemplateResponse_httpStatus,

    -- * Types

    -- ** BasicLayout
    basicLayout_moreInfo,
    basicLayout_topPanel,

    -- ** CaseEventIncludedData
    caseEventIncludedData_fields,

    -- ** CaseFilter
    caseFilter_andAll,
    caseFilter_field,
    caseFilter_not,

    -- ** CaseSummary
    caseSummary_caseId,
    caseSummary_templateId,

    -- ** CommentContent
    commentContent_body,
    commentContent_contentType,

    -- ** CommentFilter

    -- ** Contact
    contact_contactArn,

    -- ** ContactContent
    contactContent_channel,
    contactContent_connectedToSystemTime,
    contactContent_contactArn,

    -- ** ContactFilter
    contactFilter_channel,
    contactFilter_contactArn,

    -- ** DomainSummary
    domainSummary_domainArn,
    domainSummary_domainId,
    domainSummary_name,

    -- ** EventBridgeConfiguration
    eventBridgeConfiguration_includedData,
    eventBridgeConfiguration_enabled,

    -- ** EventIncludedData
    eventIncludedData_caseData,
    eventIncludedData_relatedItemData,

    -- ** FieldError
    fieldError_message,
    fieldError_errorCode,
    fieldError_id,

    -- ** FieldFilter
    fieldFilter_contains,
    fieldFilter_equalTo,
    fieldFilter_greaterThan,
    fieldFilter_greaterThanOrEqualTo,
    fieldFilter_lessThan,
    fieldFilter_lessThanOrEqualTo,

    -- ** FieldGroup
    fieldGroup_name,
    fieldGroup_fields,

    -- ** FieldIdentifier
    fieldIdentifier_id,

    -- ** FieldItem
    fieldItem_id,

    -- ** FieldOption
    fieldOption_active,
    fieldOption_name,
    fieldOption_value,

    -- ** FieldOptionError
    fieldOptionError_errorCode,
    fieldOptionError_message,
    fieldOptionError_value,

    -- ** FieldSummary
    fieldSummary_fieldArn,
    fieldSummary_fieldId,
    fieldSummary_name,
    fieldSummary_namespace,
    fieldSummary_type,

    -- ** FieldValue
    fieldValue_id,
    fieldValue_value,

    -- ** FieldValueUnion
    fieldValueUnion_booleanValue,
    fieldValueUnion_doubleValue,
    fieldValueUnion_stringValue,

    -- ** GetFieldResponse
    getFieldResponse_description,
    getFieldResponse_tags,
    getFieldResponse_fieldArn,
    getFieldResponse_fieldId,
    getFieldResponse_name,
    getFieldResponse_namespace,
    getFieldResponse_type,

    -- ** LayoutConfiguration
    layoutConfiguration_defaultLayout,

    -- ** LayoutContent
    layoutContent_basic,

    -- ** LayoutSections
    layoutSections_sections,

    -- ** LayoutSummary
    layoutSummary_layoutArn,
    layoutSummary_layoutId,
    layoutSummary_name,

    -- ** RelatedItemContent
    relatedItemContent_comment,
    relatedItemContent_contact,

    -- ** RelatedItemEventIncludedData
    relatedItemEventIncludedData_includeContent,

    -- ** RelatedItemInputContent
    relatedItemInputContent_comment,
    relatedItemInputContent_contact,

    -- ** RelatedItemTypeFilter
    relatedItemTypeFilter_comment,
    relatedItemTypeFilter_contact,

    -- ** RequiredField
    requiredField_fieldId,

    -- ** SearchCasesResponseItem
    searchCasesResponseItem_tags,
    searchCasesResponseItem_caseId,
    searchCasesResponseItem_fields,
    searchCasesResponseItem_templateId,

    -- ** SearchRelatedItemsResponseItem
    searchRelatedItemsResponseItem_tags,
    searchRelatedItemsResponseItem_associationTime,
    searchRelatedItemsResponseItem_content,
    searchRelatedItemsResponseItem_relatedItemId,
    searchRelatedItemsResponseItem_type,

    -- ** Section
    section_fieldGroup,

    -- ** Sort
    sort_fieldId,
    sort_sortOrder,

    -- ** TemplateSummary
    templateSummary_name,
    templateSummary_status,
    templateSummary_templateArn,
    templateSummary_templateId,
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
import Amazonka.ConnectCases.Types.BasicLayout
import Amazonka.ConnectCases.Types.CaseEventIncludedData
import Amazonka.ConnectCases.Types.CaseFilter
import Amazonka.ConnectCases.Types.CaseSummary
import Amazonka.ConnectCases.Types.CommentContent
import Amazonka.ConnectCases.Types.CommentFilter
import Amazonka.ConnectCases.Types.Contact
import Amazonka.ConnectCases.Types.ContactContent
import Amazonka.ConnectCases.Types.ContactFilter
import Amazonka.ConnectCases.Types.DomainSummary
import Amazonka.ConnectCases.Types.EventBridgeConfiguration
import Amazonka.ConnectCases.Types.EventIncludedData
import Amazonka.ConnectCases.Types.FieldError
import Amazonka.ConnectCases.Types.FieldFilter
import Amazonka.ConnectCases.Types.FieldGroup
import Amazonka.ConnectCases.Types.FieldIdentifier
import Amazonka.ConnectCases.Types.FieldItem
import Amazonka.ConnectCases.Types.FieldOption
import Amazonka.ConnectCases.Types.FieldOptionError
import Amazonka.ConnectCases.Types.FieldSummary
import Amazonka.ConnectCases.Types.FieldValue
import Amazonka.ConnectCases.Types.FieldValueUnion
import Amazonka.ConnectCases.Types.GetFieldResponse
import Amazonka.ConnectCases.Types.LayoutConfiguration
import Amazonka.ConnectCases.Types.LayoutContent
import Amazonka.ConnectCases.Types.LayoutSections
import Amazonka.ConnectCases.Types.LayoutSummary
import Amazonka.ConnectCases.Types.RelatedItemContent
import Amazonka.ConnectCases.Types.RelatedItemEventIncludedData
import Amazonka.ConnectCases.Types.RelatedItemInputContent
import Amazonka.ConnectCases.Types.RelatedItemTypeFilter
import Amazonka.ConnectCases.Types.RequiredField
import Amazonka.ConnectCases.Types.SearchCasesResponseItem
import Amazonka.ConnectCases.Types.SearchRelatedItemsResponseItem
import Amazonka.ConnectCases.Types.Section
import Amazonka.ConnectCases.Types.Sort
import Amazonka.ConnectCases.Types.TemplateSummary
import Amazonka.ConnectCases.UntagResource
import Amazonka.ConnectCases.UpdateCase
import Amazonka.ConnectCases.UpdateField
import Amazonka.ConnectCases.UpdateLayout
import Amazonka.ConnectCases.UpdateTemplate

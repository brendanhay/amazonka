{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Lens
  ( -- * Operations

    -- ** RefreshTrustedAdvisorCheck
    refreshTrustedAdvisorCheck_checkId,
    refreshTrustedAdvisorCheckResponse_httpStatus,
    refreshTrustedAdvisorCheckResponse_status,

    -- ** DescribeCases
    describeCases_includeResolvedCases,
    describeCases_caseIdList,
    describeCases_afterTime,
    describeCases_beforeTime,
    describeCases_nextToken,
    describeCases_includeCommunications,
    describeCases_displayId,
    describeCases_language,
    describeCases_maxResults,
    describeCasesResponse_cases,
    describeCasesResponse_nextToken,
    describeCasesResponse_httpStatus,

    -- ** DescribeTrustedAdvisorCheckRefreshStatuses
    describeTrustedAdvisorCheckRefreshStatuses_checkIds,
    describeTrustedAdvisorCheckRefreshStatusesResponse_httpStatus,
    describeTrustedAdvisorCheckRefreshStatusesResponse_statuses,

    -- ** DescribeTrustedAdvisorCheckSummaries
    describeTrustedAdvisorCheckSummaries_checkIds,
    describeTrustedAdvisorCheckSummariesResponse_httpStatus,
    describeTrustedAdvisorCheckSummariesResponse_summaries,

    -- ** CreateCase
    createCase_severityCode,
    createCase_issueType,
    createCase_ccEmailAddresses,
    createCase_language,
    createCase_categoryCode,
    createCase_serviceCode,
    createCase_attachmentSetId,
    createCase_subject,
    createCase_communicationBody,
    createCaseResponse_caseId,
    createCaseResponse_httpStatus,

    -- ** ResolveCase
    resolveCase_caseId,
    resolveCaseResponse_initialCaseStatus,
    resolveCaseResponse_finalCaseStatus,
    resolveCaseResponse_httpStatus,

    -- ** DescribeSeverityLevels
    describeSeverityLevels_language,
    describeSeverityLevelsResponse_severityLevels,
    describeSeverityLevelsResponse_httpStatus,

    -- ** DescribeTrustedAdvisorChecks
    describeTrustedAdvisorChecks_language,
    describeTrustedAdvisorChecksResponse_httpStatus,
    describeTrustedAdvisorChecksResponse_checks,

    -- ** DescribeAttachment
    describeAttachment_attachmentId,
    describeAttachmentResponse_attachment,
    describeAttachmentResponse_httpStatus,

    -- ** AddAttachmentsToSet
    addAttachmentsToSet_attachmentSetId,
    addAttachmentsToSet_attachments,
    addAttachmentsToSetResponse_expiryTime,
    addAttachmentsToSetResponse_attachmentSetId,
    addAttachmentsToSetResponse_httpStatus,

    -- ** DescribeTrustedAdvisorCheckResult
    describeTrustedAdvisorCheckResult_language,
    describeTrustedAdvisorCheckResult_checkId,
    describeTrustedAdvisorCheckResultResponse_result,
    describeTrustedAdvisorCheckResultResponse_httpStatus,

    -- ** DescribeServices
    describeServices_serviceCodeList,
    describeServices_language,
    describeServicesResponse_services,
    describeServicesResponse_httpStatus,

    -- ** DescribeCommunications
    describeCommunications_afterTime,
    describeCommunications_beforeTime,
    describeCommunications_nextToken,
    describeCommunications_maxResults,
    describeCommunications_caseId,
    describeCommunicationsResponse_nextToken,
    describeCommunicationsResponse_communications,
    describeCommunicationsResponse_httpStatus,

    -- ** AddCommunicationToCase
    addCommunicationToCase_caseId,
    addCommunicationToCase_ccEmailAddresses,
    addCommunicationToCase_attachmentSetId,
    addCommunicationToCase_communicationBody,
    addCommunicationToCaseResponse_result,
    addCommunicationToCaseResponse_httpStatus,

    -- * Types

    -- ** Attachment
    attachment_data,
    attachment_fileName,

    -- ** AttachmentDetails
    attachmentDetails_attachmentId,
    attachmentDetails_fileName,

    -- ** CaseDetails
    caseDetails_subject,
    caseDetails_status,
    caseDetails_recentCommunications,
    caseDetails_severityCode,
    caseDetails_caseId,
    caseDetails_ccEmailAddresses,
    caseDetails_displayId,
    caseDetails_submittedBy,
    caseDetails_language,
    caseDetails_timeCreated,
    caseDetails_categoryCode,
    caseDetails_serviceCode,

    -- ** Category
    category_name,
    category_code,

    -- ** Communication
    communication_body,
    communication_caseId,
    communication_submittedBy,
    communication_timeCreated,
    communication_attachmentSet,

    -- ** RecentCaseCommunications
    recentCaseCommunications_nextToken,
    recentCaseCommunications_communications,

    -- ** SeverityLevel
    severityLevel_name,
    severityLevel_code,

    -- ** SupportService
    supportService_categories,
    supportService_name,
    supportService_code,

    -- ** TrustedAdvisorCategorySpecificSummary
    trustedAdvisorCategorySpecificSummary_costOptimizing,

    -- ** TrustedAdvisorCheckDescription
    trustedAdvisorCheckDescription_id,
    trustedAdvisorCheckDescription_name,
    trustedAdvisorCheckDescription_description,
    trustedAdvisorCheckDescription_category,
    trustedAdvisorCheckDescription_metadata,

    -- ** TrustedAdvisorCheckRefreshStatus
    trustedAdvisorCheckRefreshStatus_checkId,
    trustedAdvisorCheckRefreshStatus_status,
    trustedAdvisorCheckRefreshStatus_millisUntilNextRefreshable,

    -- ** TrustedAdvisorCheckResult
    trustedAdvisorCheckResult_checkId,
    trustedAdvisorCheckResult_timestamp,
    trustedAdvisorCheckResult_status,
    trustedAdvisorCheckResult_resourcesSummary,
    trustedAdvisorCheckResult_categorySpecificSummary,
    trustedAdvisorCheckResult_flaggedResources,

    -- ** TrustedAdvisorCheckSummary
    trustedAdvisorCheckSummary_hasFlaggedResources,
    trustedAdvisorCheckSummary_checkId,
    trustedAdvisorCheckSummary_timestamp,
    trustedAdvisorCheckSummary_status,
    trustedAdvisorCheckSummary_resourcesSummary,
    trustedAdvisorCheckSummary_categorySpecificSummary,

    -- ** TrustedAdvisorCostOptimizingSummary
    trustedAdvisorCostOptimizingSummary_estimatedMonthlySavings,
    trustedAdvisorCostOptimizingSummary_estimatedPercentMonthlySavings,

    -- ** TrustedAdvisorResourceDetail
    trustedAdvisorResourceDetail_isSuppressed,
    trustedAdvisorResourceDetail_region,
    trustedAdvisorResourceDetail_status,
    trustedAdvisorResourceDetail_resourceId,
    trustedAdvisorResourceDetail_metadata,

    -- ** TrustedAdvisorResourcesSummary
    trustedAdvisorResourcesSummary_resourcesProcessed,
    trustedAdvisorResourcesSummary_resourcesFlagged,
    trustedAdvisorResourcesSummary_resourcesIgnored,
    trustedAdvisorResourcesSummary_resourcesSuppressed,
  )
where

import Network.AWS.Support.AddAttachmentsToSet
import Network.AWS.Support.AddCommunicationToCase
import Network.AWS.Support.CreateCase
import Network.AWS.Support.DescribeAttachment
import Network.AWS.Support.DescribeCases
import Network.AWS.Support.DescribeCommunications
import Network.AWS.Support.DescribeServices
import Network.AWS.Support.DescribeSeverityLevels
import Network.AWS.Support.DescribeTrustedAdvisorCheckRefreshStatuses
import Network.AWS.Support.DescribeTrustedAdvisorCheckResult
import Network.AWS.Support.DescribeTrustedAdvisorCheckSummaries
import Network.AWS.Support.DescribeTrustedAdvisorChecks
import Network.AWS.Support.RefreshTrustedAdvisorCheck
import Network.AWS.Support.ResolveCase
import Network.AWS.Support.Types.Attachment
import Network.AWS.Support.Types.AttachmentDetails
import Network.AWS.Support.Types.CaseDetails
import Network.AWS.Support.Types.Category
import Network.AWS.Support.Types.Communication
import Network.AWS.Support.Types.RecentCaseCommunications
import Network.AWS.Support.Types.SeverityLevel
import Network.AWS.Support.Types.SupportService
import Network.AWS.Support.Types.TrustedAdvisorCategorySpecificSummary
import Network.AWS.Support.Types.TrustedAdvisorCheckDescription
import Network.AWS.Support.Types.TrustedAdvisorCheckRefreshStatus
import Network.AWS.Support.Types.TrustedAdvisorCheckResult
import Network.AWS.Support.Types.TrustedAdvisorCheckSummary
import Network.AWS.Support.Types.TrustedAdvisorCostOptimizingSummary
import Network.AWS.Support.Types.TrustedAdvisorResourceDetail
import Network.AWS.Support.Types.TrustedAdvisorResourcesSummary

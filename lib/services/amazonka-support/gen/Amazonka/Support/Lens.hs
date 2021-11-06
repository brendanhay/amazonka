{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Support.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Lens
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

import Amazonka.Support.AddAttachmentsToSet
import Amazonka.Support.AddCommunicationToCase
import Amazonka.Support.CreateCase
import Amazonka.Support.DescribeAttachment
import Amazonka.Support.DescribeCases
import Amazonka.Support.DescribeCommunications
import Amazonka.Support.DescribeServices
import Amazonka.Support.DescribeSeverityLevels
import Amazonka.Support.DescribeTrustedAdvisorCheckRefreshStatuses
import Amazonka.Support.DescribeTrustedAdvisorCheckResult
import Amazonka.Support.DescribeTrustedAdvisorCheckSummaries
import Amazonka.Support.DescribeTrustedAdvisorChecks
import Amazonka.Support.RefreshTrustedAdvisorCheck
import Amazonka.Support.ResolveCase
import Amazonka.Support.Types.Attachment
import Amazonka.Support.Types.AttachmentDetails
import Amazonka.Support.Types.CaseDetails
import Amazonka.Support.Types.Category
import Amazonka.Support.Types.Communication
import Amazonka.Support.Types.RecentCaseCommunications
import Amazonka.Support.Types.SeverityLevel
import Amazonka.Support.Types.SupportService
import Amazonka.Support.Types.TrustedAdvisorCategorySpecificSummary
import Amazonka.Support.Types.TrustedAdvisorCheckDescription
import Amazonka.Support.Types.TrustedAdvisorCheckRefreshStatus
import Amazonka.Support.Types.TrustedAdvisorCheckResult
import Amazonka.Support.Types.TrustedAdvisorCheckSummary
import Amazonka.Support.Types.TrustedAdvisorCostOptimizingSummary
import Amazonka.Support.Types.TrustedAdvisorResourceDetail
import Amazonka.Support.Types.TrustedAdvisorResourcesSummary

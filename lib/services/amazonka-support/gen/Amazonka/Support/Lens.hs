{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Support.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Lens
  ( -- * Operations

    -- ** AddAttachmentsToSet
    addAttachmentsToSet_attachmentSetId,
    addAttachmentsToSet_attachments,
    addAttachmentsToSetResponse_expiryTime,
    addAttachmentsToSetResponse_attachmentSetId,
    addAttachmentsToSetResponse_httpStatus,

    -- ** AddCommunicationToCase
    addCommunicationToCase_ccEmailAddresses,
    addCommunicationToCase_caseId,
    addCommunicationToCase_attachmentSetId,
    addCommunicationToCase_communicationBody,
    addCommunicationToCaseResponse_result,
    addCommunicationToCaseResponse_httpStatus,

    -- ** CreateCase
    createCase_ccEmailAddresses,
    createCase_issueType,
    createCase_categoryCode,
    createCase_serviceCode,
    createCase_attachmentSetId,
    createCase_language,
    createCase_severityCode,
    createCase_subject,
    createCase_communicationBody,
    createCaseResponse_caseId,
    createCaseResponse_httpStatus,

    -- ** DescribeAttachment
    describeAttachment_attachmentId,
    describeAttachmentResponse_attachment,
    describeAttachmentResponse_httpStatus,

    -- ** DescribeCases
    describeCases_nextToken,
    describeCases_caseIdList,
    describeCases_includeResolvedCases,
    describeCases_displayId,
    describeCases_afterTime,
    describeCases_maxResults,
    describeCases_includeCommunications,
    describeCases_beforeTime,
    describeCases_language,
    describeCasesResponse_nextToken,
    describeCasesResponse_cases,
    describeCasesResponse_httpStatus,

    -- ** DescribeCommunications
    describeCommunications_nextToken,
    describeCommunications_afterTime,
    describeCommunications_maxResults,
    describeCommunications_beforeTime,
    describeCommunications_caseId,
    describeCommunicationsResponse_nextToken,
    describeCommunicationsResponse_communications,
    describeCommunicationsResponse_httpStatus,

    -- ** DescribeServices
    describeServices_serviceCodeList,
    describeServices_language,
    describeServicesResponse_services,
    describeServicesResponse_httpStatus,

    -- ** DescribeSeverityLevels
    describeSeverityLevels_language,
    describeSeverityLevelsResponse_severityLevels,
    describeSeverityLevelsResponse_httpStatus,

    -- ** DescribeTrustedAdvisorCheckRefreshStatuses
    describeTrustedAdvisorCheckRefreshStatuses_checkIds,
    describeTrustedAdvisorCheckRefreshStatusesResponse_httpStatus,
    describeTrustedAdvisorCheckRefreshStatusesResponse_statuses,

    -- ** DescribeTrustedAdvisorCheckResult
    describeTrustedAdvisorCheckResult_language,
    describeTrustedAdvisorCheckResult_checkId,
    describeTrustedAdvisorCheckResultResponse_result,
    describeTrustedAdvisorCheckResultResponse_httpStatus,

    -- ** DescribeTrustedAdvisorCheckSummaries
    describeTrustedAdvisorCheckSummaries_checkIds,
    describeTrustedAdvisorCheckSummariesResponse_httpStatus,
    describeTrustedAdvisorCheckSummariesResponse_summaries,

    -- ** DescribeTrustedAdvisorChecks
    describeTrustedAdvisorChecks_language,
    describeTrustedAdvisorChecksResponse_httpStatus,
    describeTrustedAdvisorChecksResponse_checks,

    -- ** RefreshTrustedAdvisorCheck
    refreshTrustedAdvisorCheck_checkId,
    refreshTrustedAdvisorCheckResponse_httpStatus,
    refreshTrustedAdvisorCheckResponse_status,

    -- ** ResolveCase
    resolveCase_caseId,
    resolveCaseResponse_finalCaseStatus,
    resolveCaseResponse_initialCaseStatus,
    resolveCaseResponse_httpStatus,

    -- * Types

    -- ** Attachment
    attachment_fileName,
    attachment_data,

    -- ** AttachmentDetails
    attachmentDetails_fileName,
    attachmentDetails_attachmentId,

    -- ** CaseDetails
    caseDetails_ccEmailAddresses,
    caseDetails_caseId,
    caseDetails_displayId,
    caseDetails_timeCreated,
    caseDetails_categoryCode,
    caseDetails_serviceCode,
    caseDetails_status,
    caseDetails_submittedBy,
    caseDetails_recentCommunications,
    caseDetails_subject,
    caseDetails_language,
    caseDetails_severityCode,

    -- ** Category
    category_name,
    category_code,

    -- ** Communication
    communication_caseId,
    communication_body,
    communication_timeCreated,
    communication_submittedBy,
    communication_attachmentSet,

    -- ** RecentCaseCommunications
    recentCaseCommunications_nextToken,
    recentCaseCommunications_communications,

    -- ** SeverityLevel
    severityLevel_name,
    severityLevel_code,

    -- ** SupportService
    supportService_name,
    supportService_code,
    supportService_categories,

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

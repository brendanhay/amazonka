{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Support.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Lens
  ( -- * Operations

    -- ** AddAttachmentsToSet
    addAttachmentsToSet_attachmentSetId,
    addAttachmentsToSet_attachments,
    addAttachmentsToSetResponse_attachmentSetId,
    addAttachmentsToSetResponse_expiryTime,
    addAttachmentsToSetResponse_httpStatus,

    -- ** AddCommunicationToCase
    addCommunicationToCase_attachmentSetId,
    addCommunicationToCase_caseId,
    addCommunicationToCase_ccEmailAddresses,
    addCommunicationToCase_communicationBody,
    addCommunicationToCaseResponse_result,
    addCommunicationToCaseResponse_httpStatus,

    -- ** CreateCase
    createCase_attachmentSetId,
    createCase_categoryCode,
    createCase_ccEmailAddresses,
    createCase_issueType,
    createCase_language,
    createCase_serviceCode,
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
    describeCases_afterTime,
    describeCases_beforeTime,
    describeCases_caseIdList,
    describeCases_displayId,
    describeCases_includeCommunications,
    describeCases_includeResolvedCases,
    describeCases_language,
    describeCases_maxResults,
    describeCases_nextToken,
    describeCasesResponse_cases,
    describeCasesResponse_nextToken,
    describeCasesResponse_httpStatus,

    -- ** DescribeCommunications
    describeCommunications_afterTime,
    describeCommunications_beforeTime,
    describeCommunications_maxResults,
    describeCommunications_nextToken,
    describeCommunications_caseId,
    describeCommunicationsResponse_communications,
    describeCommunicationsResponse_nextToken,
    describeCommunicationsResponse_httpStatus,

    -- ** DescribeCreateCaseOptions
    describeCreateCaseOptions_issueType,
    describeCreateCaseOptions_serviceCode,
    describeCreateCaseOptions_language,
    describeCreateCaseOptions_categoryCode,
    describeCreateCaseOptionsResponse_communicationTypes,
    describeCreateCaseOptionsResponse_languageAvailability,
    describeCreateCaseOptionsResponse_httpStatus,

    -- ** DescribeServices
    describeServices_language,
    describeServices_serviceCodeList,
    describeServicesResponse_services,
    describeServicesResponse_httpStatus,

    -- ** DescribeSeverityLevels
    describeSeverityLevels_language,
    describeSeverityLevelsResponse_severityLevels,
    describeSeverityLevelsResponse_httpStatus,

    -- ** DescribeSupportedLanguages
    describeSupportedLanguages_issueType,
    describeSupportedLanguages_serviceCode,
    describeSupportedLanguages_categoryCode,
    describeSupportedLanguagesResponse_supportedLanguages,
    describeSupportedLanguagesResponse_httpStatus,

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
    attachment_data,
    attachment_fileName,

    -- ** AttachmentDetails
    attachmentDetails_attachmentId,
    attachmentDetails_fileName,

    -- ** CaseDetails
    caseDetails_caseId,
    caseDetails_categoryCode,
    caseDetails_ccEmailAddresses,
    caseDetails_displayId,
    caseDetails_language,
    caseDetails_recentCommunications,
    caseDetails_serviceCode,
    caseDetails_severityCode,
    caseDetails_status,
    caseDetails_subject,
    caseDetails_submittedBy,
    caseDetails_timeCreated,

    -- ** Category
    category_code,
    category_name,

    -- ** Communication
    communication_attachmentSet,
    communication_body,
    communication_caseId,
    communication_submittedBy,
    communication_timeCreated,

    -- ** CommunicationTypeOptions
    communicationTypeOptions_datesWithoutSupport,
    communicationTypeOptions_supportedHours,
    communicationTypeOptions_type,

    -- ** DateInterval
    dateInterval_endDateTime,
    dateInterval_startDateTime,

    -- ** RecentCaseCommunications
    recentCaseCommunications_communications,
    recentCaseCommunications_nextToken,

    -- ** SeverityLevel
    severityLevel_code,
    severityLevel_name,

    -- ** SupportService
    supportService_categories,
    supportService_code,
    supportService_name,

    -- ** SupportedHour
    supportedHour_endTime,
    supportedHour_startTime,

    -- ** SupportedLanguage
    supportedLanguage_code,
    supportedLanguage_display,
    supportedLanguage_language,

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
import Amazonka.Support.DescribeCreateCaseOptions
import Amazonka.Support.DescribeServices
import Amazonka.Support.DescribeSeverityLevels
import Amazonka.Support.DescribeSupportedLanguages
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
import Amazonka.Support.Types.CommunicationTypeOptions
import Amazonka.Support.Types.DateInterval
import Amazonka.Support.Types.RecentCaseCommunications
import Amazonka.Support.Types.SeverityLevel
import Amazonka.Support.Types.SupportService
import Amazonka.Support.Types.SupportedHour
import Amazonka.Support.Types.SupportedLanguage
import Amazonka.Support.Types.TrustedAdvisorCategorySpecificSummary
import Amazonka.Support.Types.TrustedAdvisorCheckDescription
import Amazonka.Support.Types.TrustedAdvisorCheckRefreshStatus
import Amazonka.Support.Types.TrustedAdvisorCheckResult
import Amazonka.Support.Types.TrustedAdvisorCheckSummary
import Amazonka.Support.Types.TrustedAdvisorCostOptimizingSummary
import Amazonka.Support.Types.TrustedAdvisorResourceDetail
import Amazonka.Support.Types.TrustedAdvisorResourcesSummary

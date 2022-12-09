{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Support
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2013-04-15@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Web Services Support
--
-- The /Amazon Web Services Support API Reference/ is intended for
-- programmers who need detailed information about the Amazon Web Services
-- Support operations and data types. You can use the API to manage your
-- support cases programmatically. The Amazon Web Services Support API uses
-- HTTP methods that return results in JSON format.
--
-- -   You must have a Business, Enterprise On-Ramp, or Enterprise Support
--     plan to use the Amazon Web Services Support API.
--
-- -   If you call the Amazon Web Services Support API from an account that
--     does not have a Business, Enterprise On-Ramp, or Enterprise Support
--     plan, the @SubscriptionRequiredException@ error message appears. For
--     information about changing your support plan, see
--     <http://aws.amazon.com/premiumsupport/ Amazon Web Services Support>.
--
-- The Amazon Web Services Support service also exposes a set of
-- <http://aws.amazon.com/premiumsupport/trustedadvisor/ Trusted Advisor>
-- features. You can retrieve a list of checks and their descriptions, get
-- check results, specify checks to refresh, and get the refresh status of
-- checks.
--
-- The following list describes the Amazon Web Services Support case
-- management operations:
--
-- -   Service names, issue categories, and available severity levels - The
--     DescribeServices and DescribeSeverityLevels operations return Amazon
--     Web Services service names, service codes, service categories, and
--     problem severity levels. You use these values when you call the
--     CreateCase operation.
--
-- -   Case creation, case details, and case resolution - The CreateCase,
--     DescribeCases, DescribeAttachment, and ResolveCase operations create
--     Amazon Web Services Support cases, retrieve information about cases,
--     and resolve cases.
--
-- -   Case communication - The DescribeCommunications,
--     AddCommunicationToCase, and AddAttachmentsToSet operations retrieve
--     and add communications and attachments to Amazon Web Services
--     Support cases.
--
-- The following list describes the operations available from the Amazon
-- Web Services Support service for Trusted Advisor:
--
-- -   DescribeTrustedAdvisorChecks returns the list of checks that run
--     against your Amazon Web Services resources.
--
-- -   Using the @checkId@ for a specific check returned by
--     DescribeTrustedAdvisorChecks, you can call
--     DescribeTrustedAdvisorCheckResult to obtain the results for the
--     check that you specified.
--
-- -   DescribeTrustedAdvisorCheckSummaries returns summarized results for
--     one or more Trusted Advisor checks.
--
-- -   RefreshTrustedAdvisorCheck requests that Trusted Advisor rerun a
--     specified check.
--
-- -   DescribeTrustedAdvisorCheckRefreshStatuses reports the refresh
--     status of one or more checks.
--
-- For authentication of requests, Amazon Web Services Support uses
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
--
-- See
-- <https://docs.aws.amazon.com/awssupport/latest/user/Welcome.html About the Amazon Web Services Support API>
-- in the /Amazon Web Services Support User Guide/ for information about
-- how to use this service to create and manage your support cases, and how
-- to call Trusted Advisor for results of checks on your resources.
module Amazonka.Support
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AttachmentIdNotFound
    _AttachmentIdNotFound,

    -- ** AttachmentLimitExceeded
    _AttachmentLimitExceeded,

    -- ** AttachmentSetExpired
    _AttachmentSetExpired,

    -- ** AttachmentSetIdNotFound
    _AttachmentSetIdNotFound,

    -- ** AttachmentSetSizeLimitExceeded
    _AttachmentSetSizeLimitExceeded,

    -- ** CaseCreationLimitExceeded
    _CaseCreationLimitExceeded,

    -- ** CaseIdNotFound
    _CaseIdNotFound,

    -- ** DescribeAttachmentLimitExceeded
    _DescribeAttachmentLimitExceeded,

    -- ** InternalServerError
    _InternalServerError,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AddAttachmentsToSet
    AddAttachmentsToSet (AddAttachmentsToSet'),
    newAddAttachmentsToSet,
    AddAttachmentsToSetResponse (AddAttachmentsToSetResponse'),
    newAddAttachmentsToSetResponse,

    -- ** AddCommunicationToCase
    AddCommunicationToCase (AddCommunicationToCase'),
    newAddCommunicationToCase,
    AddCommunicationToCaseResponse (AddCommunicationToCaseResponse'),
    newAddCommunicationToCaseResponse,

    -- ** CreateCase
    CreateCase (CreateCase'),
    newCreateCase,
    CreateCaseResponse (CreateCaseResponse'),
    newCreateCaseResponse,

    -- ** DescribeAttachment
    DescribeAttachment (DescribeAttachment'),
    newDescribeAttachment,
    DescribeAttachmentResponse (DescribeAttachmentResponse'),
    newDescribeAttachmentResponse,

    -- ** DescribeCases (Paginated)
    DescribeCases (DescribeCases'),
    newDescribeCases,
    DescribeCasesResponse (DescribeCasesResponse'),
    newDescribeCasesResponse,

    -- ** DescribeCommunications (Paginated)
    DescribeCommunications (DescribeCommunications'),
    newDescribeCommunications,
    DescribeCommunicationsResponse (DescribeCommunicationsResponse'),
    newDescribeCommunicationsResponse,

    -- ** DescribeServices
    DescribeServices (DescribeServices'),
    newDescribeServices,
    DescribeServicesResponse (DescribeServicesResponse'),
    newDescribeServicesResponse,

    -- ** DescribeSeverityLevels
    DescribeSeverityLevels (DescribeSeverityLevels'),
    newDescribeSeverityLevels,
    DescribeSeverityLevelsResponse (DescribeSeverityLevelsResponse'),
    newDescribeSeverityLevelsResponse,

    -- ** DescribeTrustedAdvisorCheckRefreshStatuses
    DescribeTrustedAdvisorCheckRefreshStatuses (DescribeTrustedAdvisorCheckRefreshStatuses'),
    newDescribeTrustedAdvisorCheckRefreshStatuses,
    DescribeTrustedAdvisorCheckRefreshStatusesResponse (DescribeTrustedAdvisorCheckRefreshStatusesResponse'),
    newDescribeTrustedAdvisorCheckRefreshStatusesResponse,

    -- ** DescribeTrustedAdvisorCheckResult
    DescribeTrustedAdvisorCheckResult (DescribeTrustedAdvisorCheckResult'),
    newDescribeTrustedAdvisorCheckResult,
    DescribeTrustedAdvisorCheckResultResponse (DescribeTrustedAdvisorCheckResultResponse'),
    newDescribeTrustedAdvisorCheckResultResponse,

    -- ** DescribeTrustedAdvisorCheckSummaries
    DescribeTrustedAdvisorCheckSummaries (DescribeTrustedAdvisorCheckSummaries'),
    newDescribeTrustedAdvisorCheckSummaries,
    DescribeTrustedAdvisorCheckSummariesResponse (DescribeTrustedAdvisorCheckSummariesResponse'),
    newDescribeTrustedAdvisorCheckSummariesResponse,

    -- ** DescribeTrustedAdvisorChecks
    DescribeTrustedAdvisorChecks (DescribeTrustedAdvisorChecks'),
    newDescribeTrustedAdvisorChecks,
    DescribeTrustedAdvisorChecksResponse (DescribeTrustedAdvisorChecksResponse'),
    newDescribeTrustedAdvisorChecksResponse,

    -- ** RefreshTrustedAdvisorCheck
    RefreshTrustedAdvisorCheck (RefreshTrustedAdvisorCheck'),
    newRefreshTrustedAdvisorCheck,
    RefreshTrustedAdvisorCheckResponse (RefreshTrustedAdvisorCheckResponse'),
    newRefreshTrustedAdvisorCheckResponse,

    -- ** ResolveCase
    ResolveCase (ResolveCase'),
    newResolveCase,
    ResolveCaseResponse (ResolveCaseResponse'),
    newResolveCaseResponse,

    -- * Types

    -- ** Attachment
    Attachment (Attachment'),
    newAttachment,

    -- ** AttachmentDetails
    AttachmentDetails (AttachmentDetails'),
    newAttachmentDetails,

    -- ** CaseDetails
    CaseDetails (CaseDetails'),
    newCaseDetails,

    -- ** Category
    Category (Category'),
    newCategory,

    -- ** Communication
    Communication (Communication'),
    newCommunication,

    -- ** RecentCaseCommunications
    RecentCaseCommunications (RecentCaseCommunications'),
    newRecentCaseCommunications,

    -- ** SeverityLevel
    SeverityLevel (SeverityLevel'),
    newSeverityLevel,

    -- ** SupportService
    SupportService (SupportService'),
    newSupportService,

    -- ** TrustedAdvisorCategorySpecificSummary
    TrustedAdvisorCategorySpecificSummary (TrustedAdvisorCategorySpecificSummary'),
    newTrustedAdvisorCategorySpecificSummary,

    -- ** TrustedAdvisorCheckDescription
    TrustedAdvisorCheckDescription (TrustedAdvisorCheckDescription'),
    newTrustedAdvisorCheckDescription,

    -- ** TrustedAdvisorCheckRefreshStatus
    TrustedAdvisorCheckRefreshStatus (TrustedAdvisorCheckRefreshStatus'),
    newTrustedAdvisorCheckRefreshStatus,

    -- ** TrustedAdvisorCheckResult
    TrustedAdvisorCheckResult (TrustedAdvisorCheckResult'),
    newTrustedAdvisorCheckResult,

    -- ** TrustedAdvisorCheckSummary
    TrustedAdvisorCheckSummary (TrustedAdvisorCheckSummary'),
    newTrustedAdvisorCheckSummary,

    -- ** TrustedAdvisorCostOptimizingSummary
    TrustedAdvisorCostOptimizingSummary (TrustedAdvisorCostOptimizingSummary'),
    newTrustedAdvisorCostOptimizingSummary,

    -- ** TrustedAdvisorResourceDetail
    TrustedAdvisorResourceDetail (TrustedAdvisorResourceDetail'),
    newTrustedAdvisorResourceDetail,

    -- ** TrustedAdvisorResourcesSummary
    TrustedAdvisorResourcesSummary (TrustedAdvisorResourcesSummary'),
    newTrustedAdvisorResourcesSummary,
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
import Amazonka.Support.Lens
import Amazonka.Support.RefreshTrustedAdvisorCheck
import Amazonka.Support.ResolveCase
import Amazonka.Support.Types
import Amazonka.Support.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Support'.

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

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Support__
--
-- The AWS Support API reference is intended for programmers who need detailed information about the AWS Support operations and data types. This service enables you to manage your AWS Support cases programmatically. It uses HTTP methods that return results in JSON format.
-- The AWS Support service also exposes a set of <http://aws.amazon.com/premiumsupport/trustedadvisor/ AWS Trusted Advisor> features. You can retrieve a list of checks and their descriptions, get check results, specify checks to refresh, and get the refresh status of checks.
-- The following list describes the AWS Support case management operations:
--
--     * __Service names, issue categories, and available severity levels. __ The 'DescribeServices' and 'DescribeSeverityLevels' operations return AWS service names, service codes, service categories, and problem severity levels. You use these values when you call the 'CreateCase' operation.
--
--
--     * __Case creation, case details, and case resolution.__ The 'CreateCase' , 'DescribeCases' , 'DescribeAttachment' , and 'ResolveCase' operations create AWS Support cases, retrieve information about cases, and resolve cases.
--
--
--     * __Case communication.__ The 'DescribeCommunications' , 'AddCommunicationToCase' , and 'AddAttachmentsToSet' operations retrieve and add communications and attachments to AWS Support cases.
--
--
-- The following list describes the operations available from the AWS Support service for Trusted Advisor:
--
--     * 'DescribeTrustedAdvisorChecks' returns the list of checks that run against your AWS resources.
--
--
--     * Using the @checkId@ for a specific check returned by 'DescribeTrustedAdvisorChecks' , you can call 'DescribeTrustedAdvisorCheckResult' to obtain the results for the check that you specified.
--
--
--     * 'DescribeTrustedAdvisorCheckSummaries' returns summarized results for one or more Trusted Advisor checks.
--
--
--     * 'RefreshTrustedAdvisorCheck' requests that Trusted Advisor rerun a specified check.
--
--
--     * 'DescribeTrustedAdvisorCheckRefreshStatuses' reports the refresh status of one or more checks.
--
--
-- For authentication of requests, AWS Support uses <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
-- See <https://docs.aws.amazon.com/awssupport/latest/user/Welcome.html About the AWS Support API> in the /AWS Support User Guide/ for information about how to use this service to create and manage your support cases, and how to call Trusted Advisor for results of checks on your resources.
module Network.AWS.Support
  ( -- * Service configuration
    supportService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** RefreshTrustedAdvisorCheck
    module Network.AWS.Support.RefreshTrustedAdvisorCheck,

    -- ** DescribeCases (Paginated)
    module Network.AWS.Support.DescribeCases,

    -- ** DescribeTrustedAdvisorCheckRefreshStatuses
    module Network.AWS.Support.DescribeTrustedAdvisorCheckRefreshStatuses,

    -- ** DescribeTrustedAdvisorCheckSummaries
    module Network.AWS.Support.DescribeTrustedAdvisorCheckSummaries,

    -- ** CreateCase
    module Network.AWS.Support.CreateCase,

    -- ** ResolveCase
    module Network.AWS.Support.ResolveCase,

    -- ** DescribeSeverityLevels
    module Network.AWS.Support.DescribeSeverityLevels,

    -- ** DescribeTrustedAdvisorChecks
    module Network.AWS.Support.DescribeTrustedAdvisorChecks,

    -- ** DescribeAttachment
    module Network.AWS.Support.DescribeAttachment,

    -- ** AddAttachmentsToSet
    module Network.AWS.Support.AddAttachmentsToSet,

    -- ** DescribeTrustedAdvisorCheckResult
    module Network.AWS.Support.DescribeTrustedAdvisorCheckResult,

    -- ** DescribeServices
    module Network.AWS.Support.DescribeServices,

    -- ** DescribeCommunications (Paginated)
    module Network.AWS.Support.DescribeCommunications,

    -- ** AddCommunicationToCase
    module Network.AWS.Support.AddCommunicationToCase,

    -- * Types

    -- ** Attachment
    Attachment (..),
    mkAttachment,
    aData,
    aFileName,

    -- ** AttachmentDetails
    AttachmentDetails (..),
    mkAttachmentDetails,
    adAttachmentId,
    adFileName,

    -- ** CaseDetails
    CaseDetails (..),
    mkCaseDetails,
    cdSubject,
    cdStatus,
    cdRecentCommunications,
    cdSeverityCode,
    cdCaseId,
    cdCcEmailAddresses,
    cdDisplayId,
    cdSubmittedBy,
    cdLanguage,
    cdTimeCreated,
    cdCategoryCode,
    cdServiceCode,

    -- ** Category
    Category (..),
    mkCategory,
    cName,
    cCode,

    -- ** Communication
    Communication (..),
    mkCommunication,
    cBody,
    cCaseId,
    cSubmittedBy,
    cTimeCreated,
    cAttachmentSet,

    -- ** RecentCaseCommunications
    RecentCaseCommunications (..),
    mkRecentCaseCommunications,
    rccNextToken,
    rccCommunications,

    -- ** SeverityLevel
    SeverityLevel (..),
    mkSeverityLevel,
    slName,
    slCode,

    -- ** SupportService
    SupportService (..),
    mkSupportService,
    ssCategories,
    ssName,
    ssCode,

    -- ** TrustedAdvisorCategorySpecificSummary
    TrustedAdvisorCategorySpecificSummary (..),
    mkTrustedAdvisorCategorySpecificSummary,
    tacssCostOptimizing,

    -- ** TrustedAdvisorCheckDescription
    TrustedAdvisorCheckDescription (..),
    mkTrustedAdvisorCheckDescription,
    tacdCategory,
    tacdName,
    tacdMetadata,
    tacdId,
    tacdDescription,

    -- ** TrustedAdvisorCheckRefreshStatus
    TrustedAdvisorCheckRefreshStatus (..),
    mkTrustedAdvisorCheckRefreshStatus,
    tacrsMillisUntilNextRefreshable,
    tacrsStatus,
    tacrsCheckId,

    -- ** TrustedAdvisorCheckResult
    TrustedAdvisorCheckResult (..),
    mkTrustedAdvisorCheckResult,
    tacrCategorySpecificSummary,
    tacrStatus,
    tacrCheckId,
    tacrFlaggedResources,
    tacrResourcesSummary,
    tacrTimestamp,

    -- ** TrustedAdvisorCheckSummary
    TrustedAdvisorCheckSummary (..),
    mkTrustedAdvisorCheckSummary,
    tacsCategorySpecificSummary,
    tacsStatus,
    tacsCheckId,
    tacsResourcesSummary,
    tacsTimestamp,
    tacsHasFlaggedResources,

    -- ** TrustedAdvisorCostOptimizingSummary
    TrustedAdvisorCostOptimizingSummary (..),
    mkTrustedAdvisorCostOptimizingSummary,
    tacosEstimatedMonthlySavings,
    tacosEstimatedPercentMonthlySavings,

    -- ** TrustedAdvisorResourceDetail
    TrustedAdvisorResourceDetail (..),
    mkTrustedAdvisorResourceDetail,
    tardStatus,
    tardResourceId,
    tardIsSuppressed,
    tardMetadata,
    tardRegion,

    -- ** TrustedAdvisorResourcesSummary
    TrustedAdvisorResourcesSummary (..),
    mkTrustedAdvisorResourcesSummary,
    tarsResourcesIgnored,
    tarsResourcesProcessed,
    tarsResourcesFlagged,
    tarsResourcesSuppressed,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import qualified Network.AWS.Prelude as Lude
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
import Network.AWS.Support.Types
import Network.AWS.Support.Waiters

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

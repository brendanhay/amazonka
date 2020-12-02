{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types
  ( -- * Service Configuration
    support,

    -- * Errors

    -- * Attachment
    Attachment,
    attachment,
    aData,
    aFileName,

    -- * AttachmentDetails
    AttachmentDetails,
    attachmentDetails,
    adAttachmentId,
    adFileName,

    -- * CaseDetails
    CaseDetails,
    caseDetails,
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

    -- * Category
    Category,
    category,
    cName,
    cCode,

    -- * Communication
    Communication,
    communication,
    cBody,
    cCaseId,
    cSubmittedBy,
    cTimeCreated,
    cAttachmentSet,

    -- * RecentCaseCommunications
    RecentCaseCommunications,
    recentCaseCommunications,
    rccNextToken,
    rccCommunications,

    -- * SeverityLevel
    SeverityLevel,
    severityLevel,
    slName,
    slCode,

    -- * SupportService
    SupportService,
    supportService,
    ssCategories,
    ssName,
    ssCode,

    -- * TrustedAdvisorCategorySpecificSummary
    TrustedAdvisorCategorySpecificSummary,
    trustedAdvisorCategorySpecificSummary,
    tacssCostOptimizing,

    -- * TrustedAdvisorCheckDescription
    TrustedAdvisorCheckDescription,
    trustedAdvisorCheckDescription,
    tacdId,
    tacdName,
    tacdDescription,
    tacdCategory,
    tacdMetadata,

    -- * TrustedAdvisorCheckRefreshStatus
    TrustedAdvisorCheckRefreshStatus,
    trustedAdvisorCheckRefreshStatus,
    tacrsCheckId,
    tacrsStatus,
    tacrsMillisUntilNextRefreshable,

    -- * TrustedAdvisorCheckResult
    TrustedAdvisorCheckResult,
    trustedAdvisorCheckResult,
    tacrCheckId,
    tacrTimestamp,
    tacrStatus,
    tacrResourcesSummary,
    tacrCategorySpecificSummary,
    tacrFlaggedResources,

    -- * TrustedAdvisorCheckSummary
    TrustedAdvisorCheckSummary,
    trustedAdvisorCheckSummary,
    tacsHasFlaggedResources,
    tacsCheckId,
    tacsTimestamp,
    tacsStatus,
    tacsResourcesSummary,
    tacsCategorySpecificSummary,

    -- * TrustedAdvisorCostOptimizingSummary
    TrustedAdvisorCostOptimizingSummary,
    trustedAdvisorCostOptimizingSummary,
    tacosEstimatedMonthlySavings,
    tacosEstimatedPercentMonthlySavings,

    -- * TrustedAdvisorResourceDetail
    TrustedAdvisorResourceDetail,
    trustedAdvisorResourceDetail,
    tardIsSuppressed,
    tardRegion,
    tardStatus,
    tardResourceId,
    tardMetadata,

    -- * TrustedAdvisorResourcesSummary
    TrustedAdvisorResourcesSummary,
    trustedAdvisorResourcesSummary,
    tarsResourcesProcessed,
    tarsResourcesFlagged,
    tarsResourcesIgnored,
    tarsResourcesSuppressed,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
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

-- | API version @2013-04-15@ of the Amazon Support SDK configuration.
support :: Service
support =
  Service
    { _svcAbbrev = "Support",
      _svcSigner = v4,
      _svcPrefix = "support",
      _svcVersion = "2013-04-15",
      _svcEndpoint = defaultEndpoint support,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Support",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

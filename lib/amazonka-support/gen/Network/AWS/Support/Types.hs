-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types
  ( -- * Service configuration
    supportService,

    -- * Errors

    -- * Attachment
    Attachment (..),
    mkAttachment,
    aData,
    aFileName,

    -- * AttachmentDetails
    AttachmentDetails (..),
    mkAttachmentDetails,
    adAttachmentId,
    adFileName,

    -- * CaseDetails
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

    -- * Category
    Category (..),
    mkCategory,
    cName,
    cCode,

    -- * Communication
    Communication (..),
    mkCommunication,
    cBody,
    cCaseId,
    cSubmittedBy,
    cTimeCreated,
    cAttachmentSet,

    -- * RecentCaseCommunications
    RecentCaseCommunications (..),
    mkRecentCaseCommunications,
    rccNextToken,
    rccCommunications,

    -- * SeverityLevel
    SeverityLevel (..),
    mkSeverityLevel,
    slName,
    slCode,

    -- * SupportService
    SupportService (..),
    mkSupportService,
    ssCategories,
    ssName,
    ssCode,

    -- * TrustedAdvisorCategorySpecificSummary
    TrustedAdvisorCategorySpecificSummary (..),
    mkTrustedAdvisorCategorySpecificSummary,
    tacssCostOptimizing,

    -- * TrustedAdvisorCheckDescription
    TrustedAdvisorCheckDescription (..),
    mkTrustedAdvisorCheckDescription,
    tacdId,
    tacdName,
    tacdDescription,
    tacdCategory,
    tacdMetadata,

    -- * TrustedAdvisorCheckRefreshStatus
    TrustedAdvisorCheckRefreshStatus (..),
    mkTrustedAdvisorCheckRefreshStatus,
    tacrsCheckId,
    tacrsStatus,
    tacrsMillisUntilNextRefreshable,

    -- * TrustedAdvisorCheckResult
    TrustedAdvisorCheckResult (..),
    mkTrustedAdvisorCheckResult,
    tacrCheckId,
    tacrTimestamp,
    tacrStatus,
    tacrResourcesSummary,
    tacrCategorySpecificSummary,
    tacrFlaggedResources,

    -- * TrustedAdvisorCheckSummary
    TrustedAdvisorCheckSummary (..),
    mkTrustedAdvisorCheckSummary,
    tacsHasFlaggedResources,
    tacsCheckId,
    tacsTimestamp,
    tacsStatus,
    tacsResourcesSummary,
    tacsCategorySpecificSummary,

    -- * TrustedAdvisorCostOptimizingSummary
    TrustedAdvisorCostOptimizingSummary (..),
    mkTrustedAdvisorCostOptimizingSummary,
    tacosEstimatedMonthlySavings,
    tacosEstimatedPercentMonthlySavings,

    -- * TrustedAdvisorResourceDetail
    TrustedAdvisorResourceDetail (..),
    mkTrustedAdvisorResourceDetail,
    tardIsSuppressed,
    tardRegion,
    tardStatus,
    tardResourceId,
    tardMetadata,

    -- * TrustedAdvisorResourcesSummary
    TrustedAdvisorResourcesSummary (..),
    mkTrustedAdvisorResourcesSummary,
    tarsResourcesProcessed,
    tarsResourcesFlagged,
    tarsResourcesIgnored,
    tarsResourcesSuppressed,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign
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
supportService :: Lude.Service
supportService =
  Lude.Service
    { Lude._svcAbbrev = "Support",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "support",
      Lude._svcVersion = "2013-04-15",
      Lude._svcEndpoint = Lude.defaultEndpoint supportService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Support",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing

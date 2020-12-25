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
    mkServiceConfig,

    -- * Errors
    _AttachmentSetExpired,
    _AttachmentLimitExceeded,
    _DescribeAttachmentLimitExceeded,
    _CaseIdNotFound,
    _AttachmentSetIdNotFound,
    _AttachmentSetSizeLimitExceeded,
    _AttachmentIdNotFound,
    _InternalServerError,
    _CaseCreationLimitExceeded,

    -- * Subject
    Subject (..),

    -- * TrustedAdvisorResourcesSummary
    TrustedAdvisorResourcesSummary (..),
    mkTrustedAdvisorResourcesSummary,
    tarsResourcesProcessed,
    tarsResourcesFlagged,
    tarsResourcesIgnored,
    tarsResourcesSuppressed,

    -- * Status
    Status (..),

    -- * SeverityLevelName
    SeverityLevelName (..),

    -- * CcEmailAddress
    CcEmailAddress (..),

    -- * ExpiryTime
    ExpiryTime (..),

    -- * String
    String (..),

    -- * SupportService
    SupportService (..),
    mkSupportService,
    ssCategories,
    ssCode,
    ssName,

    -- * TrustedAdvisorCategorySpecificSummary
    TrustedAdvisorCategorySpecificSummary (..),
    mkTrustedAdvisorCategorySpecificSummary,
    tacssCostOptimizing,

    -- * Communication
    Communication (..),
    mkCommunication,
    cAttachmentSet,
    cBody,
    cCaseId,
    cSubmittedBy,
    cTimeCreated,

    -- * CommunicationBody
    CommunicationBody (..),

    -- * Category
    Category (..),
    mkCategory,
    cCode,
    cName,

    -- * SeverityLevelCode
    SeverityLevelCode (..),

    -- * TrustedAdvisorCheckSummary
    TrustedAdvisorCheckSummary (..),
    mkTrustedAdvisorCheckSummary,
    tacsCheckId,
    tacsTimestamp,
    tacsStatus,
    tacsResourcesSummary,
    tacsCategorySpecificSummary,
    tacsHasFlaggedResources,

    -- * AttachmentDetails
    AttachmentDetails (..),
    mkAttachmentDetails,
    adAttachmentId,
    adFileName,

    -- * TrustedAdvisorCheckResult
    TrustedAdvisorCheckResult (..),
    mkTrustedAdvisorCheckResult,
    tacrCheckId,
    tacrTimestamp,
    tacrStatus,
    tacrResourcesSummary,
    tacrCategorySpecificSummary,
    tacrFlaggedResources,

    -- * SeverityCode
    SeverityCode (..),

    -- * CaseId
    CaseId (..),

    -- * TrustedAdvisorCheckDescription
    TrustedAdvisorCheckDescription (..),
    mkTrustedAdvisorCheckDescription,
    tacdId,
    tacdName,
    tacdDescription,
    tacdCategory,
    tacdMetadata,

    -- * IssueType
    IssueType (..),

    -- * AfterTime
    AfterTime (..),

    -- * Attachment
    Attachment (..),
    mkAttachment,
    aData,
    aFileName,

    -- * NextToken
    NextToken (..),

    -- * RecentCaseCommunications
    RecentCaseCommunications (..),
    mkRecentCaseCommunications,
    rccCommunications,
    rccNextToken,

    -- * AttachmentId
    AttachmentId (..),

    -- * BeforeTime
    BeforeTime (..),

    -- * DisplayId
    DisplayId (..),

    -- * Language
    Language (..),

    -- * TrustedAdvisorResourceDetail
    TrustedAdvisorResourceDetail (..),
    mkTrustedAdvisorResourceDetail,
    tardStatus,
    tardResourceId,
    tardMetadata,
    tardIsSuppressed,
    tardRegion,

    -- * TrustedAdvisorCostOptimizingSummary
    TrustedAdvisorCostOptimizingSummary (..),
    mkTrustedAdvisorCostOptimizingSummary,
    tacosEstimatedMonthlySavings,
    tacosEstimatedPercentMonthlySavings,

    -- * SubmittedBy
    SubmittedBy (..),

    -- * CategoryCode
    CategoryCode (..),

    -- * TimeCreated
    TimeCreated (..),

    -- * SeverityLevel
    SeverityLevel (..),
    mkSeverityLevel,
    slCode,
    slName,

    -- * ServiceCode
    ServiceCode (..),

    -- * CaseDetails
    CaseDetails (..),
    mkCaseDetails,
    cdCaseId,
    cdCategoryCode,
    cdCcEmailAddresses,
    cdDisplayId,
    cdLanguage,
    cdRecentCommunications,
    cdServiceCode,
    cdSeverityCode,
    cdStatus,
    cdSubject,
    cdSubmittedBy,
    cdTimeCreated,

    -- * AttachmentSetId
    AttachmentSetId (..),

    -- * TrustedAdvisorCheckRefreshStatus
    TrustedAdvisorCheckRefreshStatus (..),
    mkTrustedAdvisorCheckRefreshStatus,
    tacrsCheckId,
    tacrsStatus,
    tacrsMillisUntilNextRefreshable,

    -- * FileName
    FileName (..),

    -- * Code
    Code (..),

    -- * Name
    Name (..),

    -- * Body
    Body (..),

    -- * FinalCaseStatus
    FinalCaseStatus (..),

    -- * InitialCaseStatus
    InitialCaseStatus (..),
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.Support.Types.AfterTime
import Network.AWS.Support.Types.Attachment
import Network.AWS.Support.Types.AttachmentDetails
import Network.AWS.Support.Types.AttachmentId
import Network.AWS.Support.Types.AttachmentSetId
import Network.AWS.Support.Types.BeforeTime
import Network.AWS.Support.Types.Body
import Network.AWS.Support.Types.CaseDetails
import Network.AWS.Support.Types.CaseId
import Network.AWS.Support.Types.Category
import Network.AWS.Support.Types.CategoryCode
import Network.AWS.Support.Types.CcEmailAddress
import Network.AWS.Support.Types.Code
import Network.AWS.Support.Types.Communication
import Network.AWS.Support.Types.CommunicationBody
import Network.AWS.Support.Types.DisplayId
import Network.AWS.Support.Types.ExpiryTime
import Network.AWS.Support.Types.FileName
import Network.AWS.Support.Types.FinalCaseStatus
import Network.AWS.Support.Types.InitialCaseStatus
import Network.AWS.Support.Types.IssueType
import Network.AWS.Support.Types.Language
import Network.AWS.Support.Types.Name
import Network.AWS.Support.Types.NextToken
import Network.AWS.Support.Types.RecentCaseCommunications
import Network.AWS.Support.Types.ServiceCode
import Network.AWS.Support.Types.SeverityCode
import Network.AWS.Support.Types.SeverityLevel
import Network.AWS.Support.Types.SeverityLevelCode
import Network.AWS.Support.Types.SeverityLevelName
import Network.AWS.Support.Types.Status
import Network.AWS.Support.Types.String
import Network.AWS.Support.Types.Subject
import Network.AWS.Support.Types.SubmittedBy
import Network.AWS.Support.Types.SupportService
import Network.AWS.Support.Types.TimeCreated
import Network.AWS.Support.Types.TrustedAdvisorCategorySpecificSummary
import Network.AWS.Support.Types.TrustedAdvisorCheckDescription
import Network.AWS.Support.Types.TrustedAdvisorCheckRefreshStatus
import Network.AWS.Support.Types.TrustedAdvisorCheckResult
import Network.AWS.Support.Types.TrustedAdvisorCheckSummary
import Network.AWS.Support.Types.TrustedAdvisorCostOptimizingSummary
import Network.AWS.Support.Types.TrustedAdvisorResourceDetail
import Network.AWS.Support.Types.TrustedAdvisorResourcesSummary

-- | API version @2013-04-15@ of the Amazon Support SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "Support",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "support",
      Core._svcVersion = "2013-04-15",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "Support",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The expiration time of the attachment set has passed. The set expires 1 hour after it is created.
_AttachmentSetExpired :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AttachmentSetExpired =
  Core._MatchServiceError mkServiceConfig "AttachmentSetExpired"
{-# DEPRECATED _AttachmentSetExpired "Use generic-lens or generic-optics instead." #-}

-- | The limit for the number of attachment sets created in a short period of time has been exceeded.
_AttachmentLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AttachmentLimitExceeded =
  Core._MatchServiceError mkServiceConfig "AttachmentLimitExceeded"
{-# DEPRECATED _AttachmentLimitExceeded "Use generic-lens or generic-optics instead." #-}

-- | The limit for the number of 'DescribeAttachment' requests in a short period of time has been exceeded.
_DescribeAttachmentLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DescribeAttachmentLimitExceeded =
  Core._MatchServiceError
    mkServiceConfig
    "DescribeAttachmentLimitExceeded"
{-# DEPRECATED _DescribeAttachmentLimitExceeded "Use generic-lens or generic-optics instead." #-}

-- | The requested @caseId@ could not be located.
_CaseIdNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CaseIdNotFound =
  Core._MatchServiceError mkServiceConfig "CaseIdNotFound"
{-# DEPRECATED _CaseIdNotFound "Use generic-lens or generic-optics instead." #-}

-- | An attachment set with the specified ID could not be found.
_AttachmentSetIdNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AttachmentSetIdNotFound =
  Core._MatchServiceError mkServiceConfig "AttachmentSetIdNotFound"
{-# DEPRECATED _AttachmentSetIdNotFound "Use generic-lens or generic-optics instead." #-}

-- | A limit for the size of an attachment set has been exceeded. The limits are three attachments and 5 MB per attachment.
_AttachmentSetSizeLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AttachmentSetSizeLimitExceeded =
  Core._MatchServiceError
    mkServiceConfig
    "AttachmentSetSizeLimitExceeded"
{-# DEPRECATED _AttachmentSetSizeLimitExceeded "Use generic-lens or generic-optics instead." #-}

-- | An attachment with the specified ID could not be found.
_AttachmentIdNotFound :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AttachmentIdNotFound =
  Core._MatchServiceError mkServiceConfig "AttachmentIdNotFound"
{-# DEPRECATED _AttachmentIdNotFound "Use generic-lens or generic-optics instead." #-}

-- | An internal server error occurred.
_InternalServerError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError mkServiceConfig "InternalServerError"
{-# DEPRECATED _InternalServerError "Use generic-lens or generic-optics instead." #-}

-- | The case creation limit for the account has been exceeded.
_CaseCreationLimitExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CaseCreationLimitExceeded =
  Core._MatchServiceError
    mkServiceConfig
    "CaseCreationLimitExceeded"
{-# DEPRECATED _CaseCreationLimitExceeded "Use generic-lens or generic-optics instead." #-}

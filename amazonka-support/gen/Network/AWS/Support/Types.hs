{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AttachmentSetExpired,
    _CaseCreationLimitExceeded,
    _AttachmentSetSizeLimitExceeded,
    _AttachmentSetIdNotFound,
    _InternalServerError,
    _CaseIdNotFound,
    _AttachmentIdNotFound,
    _AttachmentLimitExceeded,
    _DescribeAttachmentLimitExceeded,

    -- * Attachment
    Attachment (..),
    newAttachment,
    attachment_data,
    attachment_fileName,

    -- * AttachmentDetails
    AttachmentDetails (..),
    newAttachmentDetails,
    attachmentDetails_attachmentId,
    attachmentDetails_fileName,

    -- * CaseDetails
    CaseDetails (..),
    newCaseDetails,
    caseDetails_displayId,
    caseDetails_status,
    caseDetails_caseId,
    caseDetails_recentCommunications,
    caseDetails_serviceCode,
    caseDetails_categoryCode,
    caseDetails_submittedBy,
    caseDetails_subject,
    caseDetails_ccEmailAddresses,
    caseDetails_severityCode,
    caseDetails_timeCreated,
    caseDetails_language,

    -- * Category
    Category (..),
    newCategory,
    category_code,
    category_name,

    -- * Communication
    Communication (..),
    newCommunication,
    communication_caseId,
    communication_attachmentSet,
    communication_body,
    communication_submittedBy,
    communication_timeCreated,

    -- * RecentCaseCommunications
    RecentCaseCommunications (..),
    newRecentCaseCommunications,
    recentCaseCommunications_nextToken,
    recentCaseCommunications_communications,

    -- * SeverityLevel
    SeverityLevel (..),
    newSeverityLevel,
    severityLevel_code,
    severityLevel_name,

    -- * SupportService
    SupportService (..),
    newSupportService,
    supportService_code,
    supportService_name,
    supportService_categories,

    -- * TrustedAdvisorCategorySpecificSummary
    TrustedAdvisorCategorySpecificSummary (..),
    newTrustedAdvisorCategorySpecificSummary,
    trustedAdvisorCategorySpecificSummary_costOptimizing,

    -- * TrustedAdvisorCheckDescription
    TrustedAdvisorCheckDescription (..),
    newTrustedAdvisorCheckDescription,
    trustedAdvisorCheckDescription_id,
    trustedAdvisorCheckDescription_name,
    trustedAdvisorCheckDescription_description,
    trustedAdvisorCheckDescription_category,
    trustedAdvisorCheckDescription_metadata,

    -- * TrustedAdvisorCheckRefreshStatus
    TrustedAdvisorCheckRefreshStatus (..),
    newTrustedAdvisorCheckRefreshStatus,
    trustedAdvisorCheckRefreshStatus_checkId,
    trustedAdvisorCheckRefreshStatus_status,
    trustedAdvisorCheckRefreshStatus_millisUntilNextRefreshable,

    -- * TrustedAdvisorCheckResult
    TrustedAdvisorCheckResult (..),
    newTrustedAdvisorCheckResult,
    trustedAdvisorCheckResult_checkId,
    trustedAdvisorCheckResult_timestamp,
    trustedAdvisorCheckResult_status,
    trustedAdvisorCheckResult_resourcesSummary,
    trustedAdvisorCheckResult_categorySpecificSummary,
    trustedAdvisorCheckResult_flaggedResources,

    -- * TrustedAdvisorCheckSummary
    TrustedAdvisorCheckSummary (..),
    newTrustedAdvisorCheckSummary,
    trustedAdvisorCheckSummary_hasFlaggedResources,
    trustedAdvisorCheckSummary_checkId,
    trustedAdvisorCheckSummary_timestamp,
    trustedAdvisorCheckSummary_status,
    trustedAdvisorCheckSummary_resourcesSummary,
    trustedAdvisorCheckSummary_categorySpecificSummary,

    -- * TrustedAdvisorCostOptimizingSummary
    TrustedAdvisorCostOptimizingSummary (..),
    newTrustedAdvisorCostOptimizingSummary,
    trustedAdvisorCostOptimizingSummary_estimatedMonthlySavings,
    trustedAdvisorCostOptimizingSummary_estimatedPercentMonthlySavings,

    -- * TrustedAdvisorResourceDetail
    TrustedAdvisorResourceDetail (..),
    newTrustedAdvisorResourceDetail,
    trustedAdvisorResourceDetail_isSuppressed,
    trustedAdvisorResourceDetail_region,
    trustedAdvisorResourceDetail_status,
    trustedAdvisorResourceDetail_resourceId,
    trustedAdvisorResourceDetail_metadata,

    -- * TrustedAdvisorResourcesSummary
    TrustedAdvisorResourcesSummary (..),
    newTrustedAdvisorResourcesSummary,
    trustedAdvisorResourcesSummary_resourcesProcessed,
    trustedAdvisorResourcesSummary_resourcesFlagged,
    trustedAdvisorResourcesSummary_resourcesIgnored,
    trustedAdvisorResourcesSummary_resourcesSuppressed,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "Support",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "support",
      Prelude._svcVersion = "2013-04-15",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseJSONError "Support",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The expiration time of the attachment set has passed. The set expires 1
-- hour after it is created.
_AttachmentSetExpired :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AttachmentSetExpired =
  Prelude._MatchServiceError
    defaultService
    "AttachmentSetExpired"

-- | The case creation limit for the account has been exceeded.
_CaseCreationLimitExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CaseCreationLimitExceeded =
  Prelude._MatchServiceError
    defaultService
    "CaseCreationLimitExceeded"

-- | A limit for the size of an attachment set has been exceeded. The limits
-- are three attachments and 5 MB per attachment.
_AttachmentSetSizeLimitExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AttachmentSetSizeLimitExceeded =
  Prelude._MatchServiceError
    defaultService
    "AttachmentSetSizeLimitExceeded"

-- | An attachment set with the specified ID could not be found.
_AttachmentSetIdNotFound :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AttachmentSetIdNotFound =
  Prelude._MatchServiceError
    defaultService
    "AttachmentSetIdNotFound"

-- | An internal server error occurred.
_InternalServerError :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalServerError =
  Prelude._MatchServiceError
    defaultService
    "InternalServerError"

-- | The requested @caseId@ could not be located.
_CaseIdNotFound :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CaseIdNotFound =
  Prelude._MatchServiceError
    defaultService
    "CaseIdNotFound"

-- | An attachment with the specified ID could not be found.
_AttachmentIdNotFound :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AttachmentIdNotFound =
  Prelude._MatchServiceError
    defaultService
    "AttachmentIdNotFound"

-- | The limit for the number of attachment sets created in a short period of
-- time has been exceeded.
_AttachmentLimitExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AttachmentLimitExceeded =
  Prelude._MatchServiceError
    defaultService
    "AttachmentLimitExceeded"

-- | The limit for the number of DescribeAttachment requests in a short
-- period of time has been exceeded.
_DescribeAttachmentLimitExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DescribeAttachmentLimitExceeded =
  Prelude._MatchServiceError
    defaultService
    "DescribeAttachmentLimitExceeded"

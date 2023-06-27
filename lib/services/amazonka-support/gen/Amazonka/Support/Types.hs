{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Support.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AttachmentIdNotFound,
    _AttachmentLimitExceeded,
    _AttachmentSetExpired,
    _AttachmentSetIdNotFound,
    _AttachmentSetSizeLimitExceeded,
    _CaseCreationLimitExceeded,
    _CaseIdNotFound,
    _DescribeAttachmentLimitExceeded,
    _InternalServerError,
    _ThrottlingException,

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

    -- * Category
    Category (..),
    newCategory,
    category_code,
    category_name,

    -- * Communication
    Communication (..),
    newCommunication,
    communication_attachmentSet,
    communication_body,
    communication_caseId,
    communication_submittedBy,
    communication_timeCreated,

    -- * CommunicationTypeOptions
    CommunicationTypeOptions (..),
    newCommunicationTypeOptions,
    communicationTypeOptions_datesWithoutSupport,
    communicationTypeOptions_supportedHours,
    communicationTypeOptions_type,

    -- * DateInterval
    DateInterval (..),
    newDateInterval,
    dateInterval_endDateTime,
    dateInterval_startDateTime,

    -- * RecentCaseCommunications
    RecentCaseCommunications (..),
    newRecentCaseCommunications,
    recentCaseCommunications_communications,
    recentCaseCommunications_nextToken,

    -- * SeverityLevel
    SeverityLevel (..),
    newSeverityLevel,
    severityLevel_code,
    severityLevel_name,

    -- * SupportService
    SupportService (..),
    newSupportService,
    supportService_categories,
    supportService_code,
    supportService_name,

    -- * SupportedHour
    SupportedHour (..),
    newSupportedHour,
    supportedHour_endTime,
    supportedHour_startTime,

    -- * SupportedLanguage
    SupportedLanguage (..),
    newSupportedLanguage,
    supportedLanguage_code,
    supportedLanguage_display,
    supportedLanguage_language,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
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

-- | API version @2013-04-15@ of the Amazon Support SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Support",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "support",
      Core.signingName = "support",
      Core.version = "2013-04-15",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Support",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | An attachment with the specified ID could not be found.
_AttachmentIdNotFound :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AttachmentIdNotFound =
  Core._MatchServiceError
    defaultService
    "AttachmentIdNotFound"

-- | The limit for the number of attachment sets created in a short period of
-- time has been exceeded.
_AttachmentLimitExceeded :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AttachmentLimitExceeded =
  Core._MatchServiceError
    defaultService
    "AttachmentLimitExceeded"

-- | The expiration time of the attachment set has passed. The set expires 1
-- hour after it is created.
_AttachmentSetExpired :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AttachmentSetExpired =
  Core._MatchServiceError
    defaultService
    "AttachmentSetExpired"

-- | An attachment set with the specified ID could not be found.
_AttachmentSetIdNotFound :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AttachmentSetIdNotFound =
  Core._MatchServiceError
    defaultService
    "AttachmentSetIdNotFound"

-- | A limit for the size of an attachment set has been exceeded. The limits
-- are three attachments and 5 MB per attachment.
_AttachmentSetSizeLimitExceeded :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AttachmentSetSizeLimitExceeded =
  Core._MatchServiceError
    defaultService
    "AttachmentSetSizeLimitExceeded"

-- | The case creation limit for the account has been exceeded.
_CaseCreationLimitExceeded :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CaseCreationLimitExceeded =
  Core._MatchServiceError
    defaultService
    "CaseCreationLimitExceeded"

-- | The requested @caseId@ couldn\'t be located.
_CaseIdNotFound :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CaseIdNotFound =
  Core._MatchServiceError
    defaultService
    "CaseIdNotFound"

-- | The limit for the number of DescribeAttachment requests in a short
-- period of time has been exceeded.
_DescribeAttachmentLimitExceeded :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DescribeAttachmentLimitExceeded =
  Core._MatchServiceError
    defaultService
    "DescribeAttachmentLimitExceeded"

-- | An internal server error occurred.
_InternalServerError :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerError =
  Core._MatchServiceError
    defaultService
    "InternalServerError"

-- | You have exceeded the maximum allowed TPS (Transactions Per Second) for
-- the operations.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

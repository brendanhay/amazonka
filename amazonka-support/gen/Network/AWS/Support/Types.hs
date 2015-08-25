{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Support.Types
    (
    -- * Service Configuration
      support

    -- * Errors
    , _AttachmentSetExpired
    , _AttachmentLimitExceeded
    , _DescribeAttachmentLimitExceeded
    , _CaseIdNotFound
    , _AttachmentSetIdNotFound
    , _AttachmentSetSizeLimitExceeded
    , _AttachmentIdNotFound
    , _InternalServerError
    , _CaseCreationLimitExceeded

    -- * Attachment
    , Attachment
    , attachment
    , aData
    , aFileName

    -- * AttachmentDetails
    , AttachmentDetails
    , attachmentDetails
    , adAttachmentId
    , adFileName

    -- * CaseDetails
    , CaseDetails
    , caseDetails
    , cdSubject
    , cdStatus
    , cdRecentCommunications
    , cdSeverityCode
    , cdCaseId
    , cdCcEmailAddresses
    , cdDisplayId
    , cdSubmittedBy
    , cdLanguage
    , cdCategoryCode
    , cdTimeCreated
    , cdServiceCode

    -- * Category
    , Category
    , category
    , cName
    , cCode

    -- * Communication
    , Communication
    , communication
    , cBody
    , cCaseId
    , cSubmittedBy
    , cTimeCreated
    , cAttachmentSet

    -- * RecentCaseCommunications
    , RecentCaseCommunications
    , recentCaseCommunications
    , rccNextToken
    , rccCommunications

    -- * SeverityLevel
    , SeverityLevel
    , severityLevel
    , slName
    , slCode

    -- * SupportService
    , SupportService
    , supportService
    , ssCategories
    , ssName
    , ssCode

    -- * TrustedAdvisorCategorySpecificSummary
    , TrustedAdvisorCategorySpecificSummary
    , trustedAdvisorCategorySpecificSummary
    , tacssCostOptimizing

    -- * TrustedAdvisorCheckDescription
    , TrustedAdvisorCheckDescription
    , trustedAdvisorCheckDescription
    , tacdId
    , tacdName
    , tacdDescription
    , tacdCategory
    , tacdMetadata

    -- * TrustedAdvisorCheckRefreshStatus
    , TrustedAdvisorCheckRefreshStatus
    , trustedAdvisorCheckRefreshStatus
    , tacrsCheckId
    , tacrsStatus
    , tacrsMillisUntilNextRefreshable

    -- * TrustedAdvisorCheckResult
    , TrustedAdvisorCheckResult
    , trustedAdvisorCheckResult
    , tacrCheckId
    , tacrTimestamp
    , tacrStatus
    , tacrResourcesSummary
    , tacrCategorySpecificSummary
    , tacrFlaggedResources

    -- * TrustedAdvisorCheckSummary
    , TrustedAdvisorCheckSummary
    , trustedAdvisorCheckSummary
    , tacsHasFlaggedResources
    , tacsCheckId
    , tacsTimestamp
    , tacsStatus
    , tacsResourcesSummary
    , tacsCategorySpecificSummary

    -- * TrustedAdvisorCostOptimizingSummary
    , TrustedAdvisorCostOptimizingSummary
    , trustedAdvisorCostOptimizingSummary
    , tacosEstimatedMonthlySavings
    , tacosEstimatedPercentMonthlySavings

    -- * TrustedAdvisorResourceDetail
    , TrustedAdvisorResourceDetail
    , trustedAdvisorResourceDetail
    , tardIsSuppressed
    , tardStatus
    , tardRegion
    , tardResourceId
    , tardMetadata

    -- * TrustedAdvisorResourcesSummary
    , TrustedAdvisorResourcesSummary
    , trustedAdvisorResourcesSummary
    , tarsResourcesProcessed
    , tarsResourcesFlagged
    , tarsResourcesIgnored
    , tarsResourcesSuppressed
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4
import           Network.AWS.Support.Types.Product
import           Network.AWS.Support.Types.Sum

-- | API version '2013-04-15' of the Amazon Support SDK configuration.
support :: Service
support =
    Service
    { _svcAbbrev = "Support"
    , _svcSigner = v4
    , _svcPrefix = "support"
    , _svcVersion = "2013-04-15"
    , _svcEndpoint = defaultEndpoint support
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError
    , _svcRetry = retry
    }
  where
    retry =
        Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | The expiration time of the attachment set has passed. The set expires 1
-- hour after it is created.
_AttachmentSetExpired :: AsError a => Getting (First ServiceError) a ServiceError
_AttachmentSetExpired = _ServiceError . hasCode "AttachmentSetExpired"

-- | The limit for the number of attachment sets created in a short period of
-- time has been exceeded.
_AttachmentLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_AttachmentLimitExceeded = _ServiceError . hasCode "AttachmentLimitExceeded"

-- | The limit for the number of DescribeAttachment requests in a short
-- period of time has been exceeded.
_DescribeAttachmentLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_DescribeAttachmentLimitExceeded =
    _ServiceError . hasCode "DescribeAttachmentLimitExceeded"

-- | The requested 'CaseId' could not be located.
_CaseIdNotFound :: AsError a => Getting (First ServiceError) a ServiceError
_CaseIdNotFound = _ServiceError . hasCode "CaseIdNotFound"

-- | An attachment set with the specified ID could not be found.
_AttachmentSetIdNotFound :: AsError a => Getting (First ServiceError) a ServiceError
_AttachmentSetIdNotFound = _ServiceError . hasCode "AttachmentSetIdNotFound"

-- | A limit for the size of an attachment set has been exceeded. The limits
-- are 3 attachments and 5 MB per attachment.
_AttachmentSetSizeLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_AttachmentSetSizeLimitExceeded =
    _ServiceError . hasCode "AttachmentSetSizeLimitExceeded"

-- | An attachment with the specified ID could not be found.
_AttachmentIdNotFound :: AsError a => Getting (First ServiceError) a ServiceError
_AttachmentIdNotFound = _ServiceError . hasCode "AttachmentIdNotFound"

-- | An internal server error occurred.
_InternalServerError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServerError = _ServiceError . hasCode "InternalServerError"

-- | The case creation limit for the account has been exceeded.
_CaseCreationLimitExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_CaseCreationLimitExceeded =
    _ServiceError . hasCode "CaseCreationLimitExceeded"

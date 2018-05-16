{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types
    (
    -- * Service Configuration
      fms

    -- * Errors
    , _InternalErrorException
    , _InvalidInputException
    , _InvalidOperationException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * PolicyComplianceStatusType
    , PolicyComplianceStatusType (..)

    -- * SecurityServiceType
    , SecurityServiceType (..)

    -- * ViolationReason
    , ViolationReason (..)

    -- * ComplianceViolator
    , ComplianceViolator
    , complianceViolator
    , cvResourceId
    , cvResourceType
    , cvViolationReason

    -- * EvaluationResult
    , EvaluationResult
    , evaluationResult
    , erViolatorCount
    , erComplianceStatus
    , erEvaluationLimitExceeded

    -- * Policy
    , Policy
    , policy
    , pPolicyId
    , pResourceTags
    , pPolicyUpdateToken
    , pPolicyName
    , pSecurityServicePolicyData
    , pResourceType
    , pExcludeResourceTags
    , pRemediationEnabled

    -- * PolicyComplianceDetail
    , PolicyComplianceDetail
    , policyComplianceDetail
    , pcdExpiredAt
    , pcdPolicyId
    , pcdViolators
    , pcdEvaluationLimitExceeded
    , pcdPolicyOwner
    , pcdMemberAccount

    -- * PolicyComplianceStatus
    , PolicyComplianceStatus
    , policyComplianceStatus
    , pcsEvaluationResults
    , pcsLastUpdated
    , pcsPolicyName
    , pcsPolicyId
    , pcsPolicyOwner
    , pcsMemberAccount

    -- * PolicySummary
    , PolicySummary
    , policySummary
    , psPolicyName
    , psRemediationEnabled
    , psResourceType
    , psPolicyId
    , psPolicyARN
    , psSecurityServiceType

    -- * ResourceTag
    , ResourceTag
    , resourceTag
    , rtValue
    , rtKey

    -- * SecurityServicePolicyData
    , SecurityServicePolicyData
    , securityServicePolicyData
    , sspdManagedServiceData
    , sspdType
    ) where

import Network.AWS.FMS.Types.Product
import Network.AWS.FMS.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2018-01-01@ of the Amazon Firewall Management Service SDK configuration.
fms :: Service
fms =
  Service
    { _svcAbbrev = "FMS"
    , _svcSigner = v4
    , _svcPrefix = "fms"
    , _svcVersion = "2018-01-01"
    , _svcEndpoint = defaultEndpoint fms
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "FMS"
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
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The operation failed because of a system problem, even though the request was valid. Retry your request.
--
--
_InternalErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalErrorException = _MatchServiceError fms "InternalErrorException"


-- | The parameters of the request were invalid.
--
--
_InvalidInputException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInputException = _MatchServiceError fms "InvalidInputException"


-- | The operation failed because there was nothing to do. For example, you might have submitted an @AssociateAdminAccount@ request, but the account ID that you submitted was already set as the AWS Firewall Manager administrator.
--
--
_InvalidOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOperationException = _MatchServiceError fms "InvalidOperationException"


-- | The specified resource was not found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException = _MatchServiceError fms "ResourceNotFoundException"


-- | The operation exceeds a resource limit, for example, the maximum number of @policy@ objects that you can create for an AWS account. For more information, see <http://docs.aws.amazon.com/waf/latest/developerguide/fms-limits.html Firewall Manager Limits> in the /AWS WAF Developer Guide/ .
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _MatchServiceError fms "LimitExceededException"


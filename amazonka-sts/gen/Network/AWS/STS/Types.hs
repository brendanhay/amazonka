{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.STS.Types
    (
    -- * Service Configuration
      sts

    -- * Errors
    , _MalformedPolicyDocumentException
    , _InvalidAuthorizationMessageException
    , _PackedPolicyTooLargeException
    , _RegionDisabledException
    , _IdPCommunicationErrorException
    , _InvalidIdentityTokenException
    , _ExpiredTokenException
    , _IdPRejectedClaimException

    -- * AssumedRoleUser
    , AssumedRoleUser
    , assumedRoleUser
    , aruAssumedRoleId
    , aruARN

    -- * FederatedUser
    , FederatedUser
    , federatedUser
    , fuFederatedUserId
    , fuARN
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.STS.Types.Product
import Network.AWS.STS.Types.Sum

-- | API version @2011-06-15@ of the Amazon Security Token Service SDK configuration.
sts :: Service
sts =
  Service
    { _svcAbbrev = "STS"
    , _svcSigner = v4
    , _svcPrefix = "sts"
    , _svcVersion = "2011-06-15"
    , _svcEndpoint = defaultEndpoint sts
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError "STS"
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


-- | The request was rejected because the policy document was malformed. The error message describes the specific error.
--
--
_MalformedPolicyDocumentException :: AsError a => Getting (First ServiceError) a ServiceError
_MalformedPolicyDocumentException =
  _MatchServiceError sts "MalformedPolicyDocument" . hasStatus 400


-- | The error returned if the message passed to @DecodeAuthorizationMessage@ was invalid. This can happen if the token contains invalid characters, such as linebreaks.
--
--
_InvalidAuthorizationMessageException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAuthorizationMessageException =
  _MatchServiceError sts "InvalidAuthorizationMessageException" . hasStatus 400


-- | The request was rejected because the policy document was too large. The error message describes how big the policy document is, in packed form, as a percentage of what the API allows.
--
--
_PackedPolicyTooLargeException :: AsError a => Getting (First ServiceError) a ServiceError
_PackedPolicyTooLargeException =
  _MatchServiceError sts "PackedPolicyTooLarge" . hasStatus 400


-- | STS is not activated in the requested region for the account that is being asked to generate credentials. The account administrator must use the IAM console to activate STS in that region. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating AWS STS in an AWS Region> in the /IAM User Guide/ .
--
--
_RegionDisabledException :: AsError a => Getting (First ServiceError) a ServiceError
_RegionDisabledException =
  _MatchServiceError sts "RegionDisabledException" . hasStatus 403


-- | The request could not be fulfilled because the non-AWS identity provider (IDP) that was asked to verify the incoming identity token could not be reached. This is often a transient error caused by network conditions. Retry the request a limited number of times so that you don't exceed the request rate. If the error persists, the non-AWS identity provider might be down or not responding.
--
--
_IdPCommunicationErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_IdPCommunicationErrorException =
  _MatchServiceError sts "IDPCommunicationError" . hasStatus 400


-- | The web identity token that was passed could not be validated by AWS. Get a new identity token from the identity provider and then retry the request.
--
--
_InvalidIdentityTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidIdentityTokenException =
  _MatchServiceError sts "InvalidIdentityToken" . hasStatus 400


-- | The web identity token that was passed is expired or is not valid. Get a new identity token from the identity provider and then retry the request.
--
--
_ExpiredTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_ExpiredTokenException =
  _MatchServiceError sts "ExpiredTokenException" . hasStatus 400


-- | The identity provider (IdP) reported that authentication failed. This might be because the claim is invalid.
--
--
-- If this error is returned for the @AssumeRoleWithWebIdentity@ operation, it can also mean that the claim has expired or has been explicitly revoked.
--
_IdPRejectedClaimException :: AsError a => Getting (First ServiceError) a ServiceError
_IdPRejectedClaimException =
  _MatchServiceError sts "IDPRejectedClaim" . hasStatus 403


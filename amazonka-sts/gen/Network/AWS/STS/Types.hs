{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.STS.Types
    (
    -- * Service
      STS

    -- * Errors
    , _MalformedPolicyDocumentException
    , _PackedPolicyTooLargeException
    , _InvalidAuthorizationMessageException
    , _IdPCommunicationErrorException
    , _ExpiredTokenException
    , _InvalidIdentityTokenException
    , _IdPRejectedClaimException

    -- * AssumedRoleUser
    , AssumedRoleUser
    , assumedRoleUser
    , aruAssumedRoleId
    , aruARN

    -- * Credentials
    , Credentials
    , credentials
    , cAccessKeyId
    , cSecretAccessKey
    , cSessionToken
    , cExpiration

    -- * FederatedUser
    , FederatedUser
    , federatedUser
    , fuFederatedUserId
    , fuARN
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4
import           Network.AWS.STS.Types.Product
import           Network.AWS.STS.Types.Sum

-- | Version @2011-06-15@ of the Amazon Security Token Service SDK.
data STS

instance AWSService STS where
    type Sg STS = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "STS"
            , _svcPrefix = "sts"
            , _svcVersion = "2011-06-15"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
            , _svcRetry = retry
            }
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

-- | The request was rejected because the policy document was malformed. The
-- error message describes the specific error.
_MalformedPolicyDocumentException :: AsError a => Getting (First ServiceError) a ServiceError
_MalformedPolicyDocumentException =
    _ServiceError . hasStatus 400 . hasCode "MalformedPolicyDocument"

-- | The request was rejected because the policy document was too large. The
-- error message describes how big the policy document is, in packed form,
-- as a percentage of what the API allows.
_PackedPolicyTooLargeException :: AsError a => Getting (First ServiceError) a ServiceError
_PackedPolicyTooLargeException =
    _ServiceError . hasStatus 400 . hasCode "PackedPolicyTooLarge"

-- | The error returned if the message passed to 'DecodeAuthorizationMessage'
-- was invalid. This can happen if the token contains invalid characters,
-- such as linebreaks.
_InvalidAuthorizationMessageException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAuthorizationMessageException =
    _ServiceError .
    hasStatus 400 . hasCode "InvalidAuthorizationMessageException"

-- | The request could not be fulfilled because the non-AWS identity provider
-- (IDP) that was asked to verify the incoming identity token could not be
-- reached. This is often a transient error caused by network conditions.
-- Retry the request a limited number of times so that you don\'t exceed
-- the request rate. If the error persists, the non-AWS identity provider
-- might be down or not responding.
_IdPCommunicationErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_IdPCommunicationErrorException =
    _ServiceError . hasStatus 400 . hasCode "IDPCommunicationError"

-- | The web identity token that was passed is expired or is not valid. Get a
-- new identity token from the identity provider and then retry the
-- request.
_ExpiredTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_ExpiredTokenException =
    _ServiceError . hasStatus 400 . hasCode "ExpiredTokenException"

-- | The web identity token that was passed could not be validated by AWS.
-- Get a new identity token from the identity provider and then retry the
-- request.
_InvalidIdentityTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidIdentityTokenException =
    _ServiceError . hasStatus 400 . hasCode "InvalidIdentityToken"

-- | The identity provider (IdP) reported that authentication failed. This
-- might be because the claim is invalid.
--
-- If this error is returned for the 'AssumeRoleWithWebIdentity' operation,
-- it can also mean that the claim has expired or has been explicitly
-- revoked.
_IdPRejectedClaimException :: AsError a => Getting (First ServiceError) a ServiceError
_IdPRejectedClaimException =
    _ServiceError . hasStatus 403 . hasCode "IDPRejectedClaim"

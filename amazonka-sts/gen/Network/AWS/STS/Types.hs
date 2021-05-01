{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.STS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _RegionDisabledException,
    _MalformedPolicyDocumentException,
    _PackedPolicyTooLargeException,
    _InvalidIdentityTokenException,
    _ExpiredTokenException,
    _InvalidAuthorizationMessageException,
    _IDPRejectedClaimException,
    _IDPCommunicationErrorException,

    -- * AssumedRoleUser
    AssumedRoleUser (..),
    newAssumedRoleUser,
    assumedRoleUser_assumedRoleId,
    assumedRoleUser_arn,

    -- * FederatedUser
    FederatedUser (..),
    newFederatedUser,
    federatedUser_federatedUserId,
    federatedUser_arn,

    -- * PolicyDescriptorType
    PolicyDescriptorType (..),
    newPolicyDescriptorType,
    policyDescriptorType_arn,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.STS.Types.AssumedRoleUser
import Network.AWS.STS.Types.FederatedUser
import Network.AWS.STS.Types.PolicyDescriptorType
import Network.AWS.STS.Types.Tag
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2011-06-15@ of the Amazon Security Token Service SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "STS",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "sts",
      Prelude._svcSigningName = "sts",
      Prelude._svcVersion = "2011-06-15",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseXMLError "STS",
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
          ( Prelude.hasCode "IDPCommunicationError"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "idp_unreachable_error"
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

-- | STS is not activated in the requested region for the account that is
-- being asked to generate credentials. The account administrator must use
-- the IAM console to activate STS in that region. For more information,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating AWS STS in an AWS Region>
-- in the /IAM User Guide/.
_RegionDisabledException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RegionDisabledException =
  Prelude._MatchServiceError
    defaultService
    "RegionDisabledException"
    Prelude.. Prelude.hasStatus 403

-- | The request was rejected because the policy document was malformed. The
-- error message describes the specific error.
_MalformedPolicyDocumentException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MalformedPolicyDocumentException =
  Prelude._MatchServiceError
    defaultService
    "MalformedPolicyDocument"
    Prelude.. Prelude.hasStatus 400

-- | The request was rejected because the total packed size of the session
-- policies and session tags combined was too large. An AWS conversion
-- compresses the session policy document, session policy ARNs, and session
-- tags into a packed binary format that has a separate limit. The error
-- message indicates by percentage how close the policies and tags are to
-- the upper size limit. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Passing Session Tags in STS>
-- in the /IAM User Guide/.
--
-- You could receive this error even though you meet other defined session
-- policy and session tag limits. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html IAM and STS Entity Character Limits>
-- in the /IAM User Guide/.
_PackedPolicyTooLargeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PackedPolicyTooLargeException =
  Prelude._MatchServiceError
    defaultService
    "PackedPolicyTooLarge"
    Prelude.. Prelude.hasStatus 400

-- | The web identity token that was passed could not be validated by AWS.
-- Get a new identity token from the identity provider and then retry the
-- request.
_InvalidIdentityTokenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidIdentityTokenException =
  Prelude._MatchServiceError
    defaultService
    "InvalidIdentityToken"
    Prelude.. Prelude.hasStatus 400

-- | The web identity token that was passed is expired or is not valid. Get a
-- new identity token from the identity provider and then retry the
-- request.
_ExpiredTokenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ExpiredTokenException =
  Prelude._MatchServiceError
    defaultService
    "ExpiredTokenException"
    Prelude.. Prelude.hasStatus 400

-- | The error returned if the message passed to @DecodeAuthorizationMessage@
-- was invalid. This can happen if the token contains invalid characters,
-- such as linebreaks.
_InvalidAuthorizationMessageException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidAuthorizationMessageException =
  Prelude._MatchServiceError
    defaultService
    "InvalidAuthorizationMessageException"
    Prelude.. Prelude.hasStatus 400

-- | The identity provider (IdP) reported that authentication failed. This
-- might be because the claim is invalid.
--
-- If this error is returned for the @AssumeRoleWithWebIdentity@ operation,
-- it can also mean that the claim has expired or has been explicitly
-- revoked.
_IDPRejectedClaimException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IDPRejectedClaimException =
  Prelude._MatchServiceError
    defaultService
    "IDPRejectedClaim"
    Prelude.. Prelude.hasStatus 403

-- | The request could not be fulfilled because the identity provider (IDP)
-- that was asked to verify the incoming identity token could not be
-- reached. This is often a transient error caused by network conditions.
-- Retry the request a limited number of times so that you don\'t exceed
-- the request rate. If the error persists, the identity provider might be
-- down or not responding.
_IDPCommunicationErrorException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IDPCommunicationErrorException =
  Prelude._MatchServiceError
    defaultService
    "IDPCommunicationError"
    Prelude.. Prelude.hasStatus 400

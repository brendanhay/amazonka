{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.STS.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.STS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ExpiredTokenException,
    _IDPCommunicationErrorException,
    _IDPRejectedClaimException,
    _InvalidAuthorizationMessageException,
    _InvalidIdentityTokenException,
    _MalformedPolicyDocumentException,
    _PackedPolicyTooLargeException,
    _RegionDisabledException,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.STS.Types.AssumedRoleUser
import Amazonka.STS.Types.FederatedUser
import Amazonka.STS.Types.PolicyDescriptorType
import Amazonka.STS.Types.Tag
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2011-06-15@ of the Amazon Security Token Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "STS",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "sts",
      Core.signingName = "sts",
      Core.version = "2011-06-15",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseXMLError "STS",
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
      | Lens.has
          ( Core.hasCode "IDPCommunicationError"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "idp_unreachable_error"
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

-- | The web identity token that was passed is expired or is not valid. Get a
-- new identity token from the identity provider and then retry the
-- request.
_ExpiredTokenException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ExpiredTokenException =
  Core._MatchServiceError
    defaultService
    "ExpiredTokenException"
    Prelude.. Core.hasStatus 400

-- | The request could not be fulfilled because the identity provider (IDP)
-- that was asked to verify the incoming identity token could not be
-- reached. This is often a transient error caused by network conditions.
-- Retry the request a limited number of times so that you don\'t exceed
-- the request rate. If the error persists, the identity provider might be
-- down or not responding.
_IDPCommunicationErrorException :: Core.AsError a => Lens.Fold a Core.ServiceError
_IDPCommunicationErrorException =
  Core._MatchServiceError
    defaultService
    "IDPCommunicationError"
    Prelude.. Core.hasStatus 400

-- | The identity provider (IdP) reported that authentication failed. This
-- might be because the claim is invalid.
--
-- If this error is returned for the @AssumeRoleWithWebIdentity@ operation,
-- it can also mean that the claim has expired or has been explicitly
-- revoked.
_IDPRejectedClaimException :: Core.AsError a => Lens.Fold a Core.ServiceError
_IDPRejectedClaimException =
  Core._MatchServiceError
    defaultService
    "IDPRejectedClaim"
    Prelude.. Core.hasStatus 403

-- | The error returned if the message passed to @DecodeAuthorizationMessage@
-- was invalid. This can happen if the token contains invalid characters,
-- such as linebreaks.
_InvalidAuthorizationMessageException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidAuthorizationMessageException =
  Core._MatchServiceError
    defaultService
    "InvalidAuthorizationMessageException"
    Prelude.. Core.hasStatus 400

-- | The web identity token that was passed could not be validated by Amazon
-- Web Services. Get a new identity token from the identity provider and
-- then retry the request.
_InvalidIdentityTokenException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidIdentityTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidIdentityToken"
    Prelude.. Core.hasStatus 400

-- | The request was rejected because the policy document was malformed. The
-- error message describes the specific error.
_MalformedPolicyDocumentException :: Core.AsError a => Lens.Fold a Core.ServiceError
_MalformedPolicyDocumentException =
  Core._MatchServiceError
    defaultService
    "MalformedPolicyDocument"
    Prelude.. Core.hasStatus 400

-- | The request was rejected because the total packed size of the session
-- policies and session tags combined was too large. An Amazon Web Services
-- conversion compresses the session policy document, session policy ARNs,
-- and session tags into a packed binary format that has a separate limit.
-- The error message indicates by percentage how close the policies and
-- tags are to the upper size limit. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Passing Session Tags in STS>
-- in the /IAM User Guide/.
--
-- You could receive this error even though you meet other defined session
-- policy and session tag limits. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html#reference_iam-limits-entity-length IAM and STS Entity Character Limits>
-- in the /IAM User Guide/.
_PackedPolicyTooLargeException :: Core.AsError a => Lens.Fold a Core.ServiceError
_PackedPolicyTooLargeException =
  Core._MatchServiceError
    defaultService
    "PackedPolicyTooLarge"
    Prelude.. Core.hasStatus 400

-- | STS is not activated in the requested region for the account that is
-- being asked to generate credentials. The account administrator must use
-- the IAM console to activate STS in that region. For more information,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating Amazon Web Services STS in an Amazon Web Services Region>
-- in the /IAM User Guide/.
_RegionDisabledException :: Core.AsError a => Lens.Fold a Core.ServiceError
_RegionDisabledException =
  Core._MatchServiceError
    defaultService
    "RegionDisabledException"
    Prelude.. Core.hasStatus 403

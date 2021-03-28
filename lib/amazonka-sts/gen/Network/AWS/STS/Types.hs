-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.STS.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _MalformedPolicyDocumentException
    , _InvalidAuthorizationMessageException
    , _PackedPolicyTooLargeException
    , _RegionDisabledException
    , _IDPCommunicationErrorException
    , _InvalidIdentityTokenException
    , _ExpiredTokenException
    , _IDPRejectedClaimException

    -- * Subject
    , Subject (..)

    -- * UserIdType
    , UserIdType (..)

    -- * Audience
    , Audience (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * WebIdentitySubjectType
    , WebIdentitySubjectType (..)

    -- * TagKeyType
    , TagKeyType (..)

    -- * ExternalIdType
    , ExternalIdType (..)

    -- * SerialNumberType
    , SerialNumberType (..)

    -- * DecodedMessageType
    , DecodedMessageType (..)

    -- * SAMLAssertionType
    , SAMLAssertionType (..)

    -- * SubjectType
    , SubjectType (..)

    -- * NameQualifier
    , NameQualifier (..)

    -- * FederatedUser
    , FederatedUser (..)
    , mkFederatedUser
    , fuFederatedUserId
    , fuArn

    -- * RoleSessionNameType
    , RoleSessionNameType (..)

    -- * ArnType
    , ArnType (..)

    -- * AssumedRoleUser
    , AssumedRoleUser (..)
    , mkAssumedRoleUser
    , aruAssumedRoleId
    , aruArn

    -- * PolicyDescriptorType
    , PolicyDescriptorType (..)
    , mkPolicyDescriptorType
    , pdtArn

    -- * Issuer
    , Issuer (..)

    -- * TokenCodeType
    , TokenCodeType (..)

    -- * SessionPolicyDocumentType
    , SessionPolicyDocumentType (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * AccessKeyId
    , AccessKeyId (..)

    -- * EncodedMessage
    , EncodedMessage (..)

    -- * RoleArn
    , RoleArn (..)

    -- * RoleSessionName
    , RoleSessionName (..)

    -- * WebIdentityToken
    , WebIdentityToken (..)

    -- * Policy
    , Policy (..)

    -- * ProviderId
    , ProviderId (..)

    -- * SecretAccessKey
    , SecretAccessKey (..)

    -- * SessionToken
    , SessionToken (..)

    -- * Account
    , Account (..)

    -- * Name
    , Name (..)

    -- * Provider
    , Provider (..)

    -- * TokenCode
    , TokenCode (..)

    -- * FederatedUserId
    , FederatedUserId (..)

    -- * Arn
    , Arn (..)

    -- * AssumedRoleId
    , AssumedRoleId (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.STS.Types.Subject
  
import Network.AWS.STS.Types.UserIdType
  
import Network.AWS.STS.Types.Audience
  
  
import Network.AWS.STS.Types.Tag
  
import Network.AWS.STS.Types.WebIdentitySubjectType
  
import Network.AWS.STS.Types.TagKeyType
  
import Network.AWS.STS.Types.ExternalIdType
  
import Network.AWS.STS.Types.SerialNumberType
  
import Network.AWS.STS.Types.DecodedMessageType
  
  
  
import Network.AWS.STS.Types.SAMLAssertionType
  
import Network.AWS.STS.Types.SubjectType
  
import Network.AWS.STS.Types.NameQualifier
  
  
  
import Network.AWS.STS.Types.FederatedUser
  
import Network.AWS.STS.Types.RoleSessionNameType
  
import Network.AWS.STS.Types.ArnType
  
import Network.AWS.STS.Types.AssumedRoleUser
  
import Network.AWS.STS.Types.PolicyDescriptorType
  
  
  
import Network.AWS.STS.Types.Issuer
  
  
import Network.AWS.STS.Types.TokenCodeType
  
import Network.AWS.STS.Types.SessionPolicyDocumentType
  
import Network.AWS.STS.Types.Key
  
import Network.AWS.STS.Types.Value
  
import Network.AWS.STS.Types.AccessKeyId
  
import Network.AWS.STS.Types.EncodedMessage
  
import Network.AWS.STS.Types.RoleArn
  
import Network.AWS.STS.Types.RoleSessionName
  
import Network.AWS.STS.Types.WebIdentityToken
  
import Network.AWS.STS.Types.Policy
  
import Network.AWS.STS.Types.ProviderId
  
import Network.AWS.STS.Types.SecretAccessKey
  
import Network.AWS.STS.Types.SessionToken
  
import Network.AWS.STS.Types.Account
  
import Network.AWS.STS.Types.Name
  
import Network.AWS.STS.Types.Provider
  
import Network.AWS.STS.Types.TokenCode
  
import Network.AWS.STS.Types.FederatedUserId
  
import Network.AWS.STS.Types.Arn
  
import Network.AWS.STS.Types.AssumedRoleId
  

-- | API version @2011-06-15@ of the Amazon Security Token Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "STS", Core._svcSigner = Sign.v4,
                 Core._svcPrefix = "sts", Core._svcVersion = "2011-06-15",
                 Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseXMLError "STS",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Lens.has
              (Core.hasCode "IDPCommunicationError" Core.. Core.hasStatus 400)
              e
            = Core.Just "idp_unreachable_error"
          | Core.otherwise = Core.Nothing

-- | The request was rejected because the policy document was malformed. The error message describes the specific error.
_MalformedPolicyDocumentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MalformedPolicyDocumentException
  = Core._MatchServiceError mkServiceConfig "MalformedPolicyDocument"
      Core.. Core.hasStatues 400
{-# INLINEABLE _MalformedPolicyDocumentException #-}
{-# DEPRECATED _MalformedPolicyDocumentException "Use generic-lens or generic-optics instead"  #-}

-- | The error returned if the message passed to @DecodeAuthorizationMessage@ was invalid. This can happen if the token contains invalid characters, such as linebreaks. 
_InvalidAuthorizationMessageException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAuthorizationMessageException
  = Core._MatchServiceError mkServiceConfig
      "InvalidAuthorizationMessageException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidAuthorizationMessageException #-}
{-# DEPRECATED _InvalidAuthorizationMessageException "Use generic-lens or generic-optics instead"  #-}

-- | The request was rejected because the total packed size of the session policies and session tags combined was too large. An AWS conversion compresses the session policy document, session policy ARNs, and session tags into a packed binary format that has a separate limit. The error message indicates by percentage how close the policies and tags are to the upper size limit. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_session-tags.html Passing Session Tags in STS> in the /IAM User Guide/ .
--
-- You could receive this error even though you meet other defined session policy and session tag limits. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html IAM and STS Entity Character Limits> in the /IAM User Guide/ .
_PackedPolicyTooLargeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PackedPolicyTooLargeException
  = Core._MatchServiceError mkServiceConfig "PackedPolicyTooLarge"
      Core.. Core.hasStatues 400
{-# INLINEABLE _PackedPolicyTooLargeException #-}
{-# DEPRECATED _PackedPolicyTooLargeException "Use generic-lens or generic-optics instead"  #-}

-- | STS is not activated in the requested region for the account that is being asked to generate credentials. The account administrator must use the IAM console to activate STS in that region. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_enable-regions.html Activating and Deactivating AWS STS in an AWS Region> in the /IAM User Guide/ .
_RegionDisabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RegionDisabledException
  = Core._MatchServiceError mkServiceConfig "RegionDisabledException"
      Core.. Core.hasStatues 403
{-# INLINEABLE _RegionDisabledException #-}
{-# DEPRECATED _RegionDisabledException "Use generic-lens or generic-optics instead"  #-}

-- | The request could not be fulfilled because the identity provider (IDP) that was asked to verify the incoming identity token could not be reached. This is often a transient error caused by network conditions. Retry the request a limited number of times so that you don't exceed the request rate. If the error persists, the identity provider might be down or not responding.
_IDPCommunicationErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IDPCommunicationErrorException
  = Core._MatchServiceError mkServiceConfig "IDPCommunicationError"
      Core.. Core.hasStatues 400
{-# INLINEABLE _IDPCommunicationErrorException #-}
{-# DEPRECATED _IDPCommunicationErrorException "Use generic-lens or generic-optics instead"  #-}

-- | The web identity token that was passed could not be validated by AWS. Get a new identity token from the identity provider and then retry the request.
_InvalidIdentityTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidIdentityTokenException
  = Core._MatchServiceError mkServiceConfig "InvalidIdentityToken"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidIdentityTokenException #-}
{-# DEPRECATED _InvalidIdentityTokenException "Use generic-lens or generic-optics instead"  #-}

-- | The web identity token that was passed is expired or is not valid. Get a new identity token from the identity provider and then retry the request.
_ExpiredTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ExpiredTokenException
  = Core._MatchServiceError mkServiceConfig "ExpiredTokenException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ExpiredTokenException #-}
{-# DEPRECATED _ExpiredTokenException "Use generic-lens or generic-optics instead"  #-}

-- | The identity provider (IdP) reported that authentication failed. This might be because the claim is invalid.
--
-- If this error is returned for the @AssumeRoleWithWebIdentity@ operation, it can also mean that the claim has expired or has been explicitly revoked. 
_IDPRejectedClaimException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IDPRejectedClaimException
  = Core._MatchServiceError mkServiceConfig "IDPRejectedClaim" Core..
      Core.hasStatues 403
{-# INLINEABLE _IDPRejectedClaimException #-}
{-# DEPRECATED _IDPRejectedClaimException "Use generic-lens or generic-optics instead"  #-}

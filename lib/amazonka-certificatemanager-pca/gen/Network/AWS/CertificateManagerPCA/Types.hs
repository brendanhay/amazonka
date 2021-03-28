-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManagerPCA.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _InvalidTagException
    , _InvalidRequestException
    , _PermissionAlreadyExistsException
    , _MalformedCSRException
    , _RequestAlreadyProcessedException
    , _MalformedCertificateException
    , _RequestFailedException
    , _CertificateMismatchException
    , _TooManyTagsException
    , _InvalidArgsException
    , _RequestInProgressException
    , _ConcurrentModificationException
    , _InvalidNextTokenException
    , _LockoutPreventedException
    , _InvalidArnException
    , _InvalidPolicyException
    , _ResourceNotFoundException
    , _InvalidStateException
    , _LimitExceededException

    -- * SigningAlgorithm
    , SigningAlgorithm (..)

    -- * IdempotencyToken
    , IdempotencyToken (..)

    -- * FailureReason
    , FailureReason (..)

    -- * CertificateAuthorityConfiguration
    , CertificateAuthorityConfiguration (..)
    , mkCertificateAuthorityConfiguration
    , cacKeyAlgorithm
    , cacSigningAlgorithm
    , cacSubject

    -- * CertificateAuthorityType
    , CertificateAuthorityType (..)

    -- * ValidityPeriodType
    , ValidityPeriodType (..)

    -- * S3Key
    , S3Key (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * CsrBody
    , CsrBody (..)

    -- * Arn
    , Arn (..)

    -- * CrlConfiguration
    , CrlConfiguration (..)
    , mkCrlConfiguration
    , ccEnabled
    , ccCustomCname
    , ccExpirationInDays
    , ccS3BucketName

    -- * String5
    , String5 (..)

    -- * String128
    , String128 (..)

    -- * ASN1Subject
    , ASN1Subject (..)
    , mkASN1Subject
    , asnsCommonName
    , asnsCountry
    , asnsDistinguishedNameQualifier
    , asnsGenerationQualifier
    , asnsGivenName
    , asnsInitials
    , asnsLocality
    , asnsOrganization
    , asnsOrganizationalUnit
    , asnsPseudonym
    , asnsSerialNumber
    , asnsState
    , asnsSurname
    , asnsTitle

    -- * AccountId
    , AccountId (..)

    -- * CertificateAuthority
    , CertificateAuthority (..)
    , mkCertificateAuthority
    , caArn
    , caCertificateAuthorityConfiguration
    , caCreatedAt
    , caFailureReason
    , caLastStateChangeAt
    , caNotAfter
    , caNotBefore
    , caOwnerAccount
    , caRestorableUntil
    , caRevocationConfiguration
    , caSerial
    , caStatus
    , caType

    -- * NextToken
    , NextToken (..)

    -- * RevocationReason
    , RevocationReason (..)

    -- * String3To255
    , String3To255 (..)

    -- * AuditReportStatus
    , AuditReportStatus (..)

    -- * Principal
    , Principal (..)

    -- * Validity
    , Validity (..)
    , mkValidity
    , vValue
    , vType

    -- * AWSPolicy
    , AWSPolicy (..)

    -- * CertificateAuthorityStatus
    , CertificateAuthorityStatus (..)

    -- * KeyAlgorithm
    , KeyAlgorithm (..)

    -- * CertificateChain
    , CertificateChain (..)

    -- * RevocationConfiguration
    , RevocationConfiguration (..)
    , mkRevocationConfiguration
    , rcCrlConfiguration

    -- * Permission
    , Permission (..)
    , mkPermission
    , pActions
    , pCertificateAuthorityArn
    , pCreatedAt
    , pPolicy
    , pPrincipal
    , pSourceAccount

    -- * ResourceOwner
    , ResourceOwner (..)

    -- * AuditReportResponseFormat
    , AuditReportResponseFormat (..)

    -- * AuditReportId
    , AuditReportId (..)

    -- * ActionType
    , ActionType (..)

    -- * S3BucketName
    , S3BucketName (..)

    -- * CertificateAuthorityArn
    , CertificateAuthorityArn (..)

    -- * CertificateSerial
    , CertificateSerial (..)

    -- * CertificateArn
    , CertificateArn (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * Policy
    , Policy (..)

    -- * CustomCname
    , CustomCname (..)

    -- * Certificate
    , Certificate (..)

    -- * CommonName
    , CommonName (..)

    -- * Country
    , Country (..)

    -- * DistinguishedNameQualifier
    , DistinguishedNameQualifier (..)

    -- * GenerationQualifier
    , GenerationQualifier (..)

    -- * GivenName
    , GivenName (..)

    -- * Organization
    , Organization (..)

    -- * OrganizationalUnit
    , OrganizationalUnit (..)

    -- * SerialNumber
    , SerialNumber (..)

    -- * Surname
    , Surname (..)

    -- * Title
    , Title (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
  
import Network.AWS.CertificateManagerPCA.Types.SigningAlgorithm
  
import Network.AWS.CertificateManagerPCA.Types.IdempotencyToken
  
import Network.AWS.CertificateManagerPCA.Types.FailureReason
  
import Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityConfiguration
  
import Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityType
  
import Network.AWS.CertificateManagerPCA.Types.ValidityPeriodType
  
  
import Network.AWS.CertificateManagerPCA.Types.S3Key
  
  
import Network.AWS.CertificateManagerPCA.Types.Tag
  
  
  
import Network.AWS.CertificateManagerPCA.Types.CsrBody
  
  
import Network.AWS.CertificateManagerPCA.Types.Arn
  
import Network.AWS.CertificateManagerPCA.Types.CrlConfiguration
  
  
import Network.AWS.CertificateManagerPCA.Types.String5
  
  
import Network.AWS.CertificateManagerPCA.Types.String128
  
  
import Network.AWS.CertificateManagerPCA.Types.ASN1Subject
  
  
import Network.AWS.CertificateManagerPCA.Types.AccountId
  
import Network.AWS.CertificateManagerPCA.Types.CertificateAuthority
  
import Network.AWS.CertificateManagerPCA.Types.NextToken
  
  
import Network.AWS.CertificateManagerPCA.Types.RevocationReason
  
import Network.AWS.CertificateManagerPCA.Types.String3To255
  
import Network.AWS.CertificateManagerPCA.Types.AuditReportStatus
  
import Network.AWS.CertificateManagerPCA.Types.Principal
  
  
import Network.AWS.CertificateManagerPCA.Types.Validity
  
  
  
import Network.AWS.CertificateManagerPCA.Types.AWSPolicy
  
import Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityStatus
  
import Network.AWS.CertificateManagerPCA.Types.KeyAlgorithm
  
import Network.AWS.CertificateManagerPCA.Types.CertificateChain
  
import Network.AWS.CertificateManagerPCA.Types.RevocationConfiguration
  
import Network.AWS.CertificateManagerPCA.Types.Permission
  
import Network.AWS.CertificateManagerPCA.Types.ResourceOwner
  
import Network.AWS.CertificateManagerPCA.Types.AuditReportResponseFormat
  
  
import Network.AWS.CertificateManagerPCA.Types.AuditReportId
  
import Network.AWS.CertificateManagerPCA.Types.ActionType
  
  
  
  
import Network.AWS.CertificateManagerPCA.Types.S3BucketName
  
  
import Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityArn
  
import Network.AWS.CertificateManagerPCA.Types.CertificateSerial
  
import Network.AWS.CertificateManagerPCA.Types.CertificateArn
  
import Network.AWS.CertificateManagerPCA.Types.Key
  
import Network.AWS.CertificateManagerPCA.Types.Value
  
import Network.AWS.CertificateManagerPCA.Types.Policy
  
import Network.AWS.CertificateManagerPCA.Types.CustomCname
  
import Network.AWS.CertificateManagerPCA.Types.Certificate
  
import Network.AWS.CertificateManagerPCA.Types.CommonName
  
import Network.AWS.CertificateManagerPCA.Types.Country
  
import Network.AWS.CertificateManagerPCA.Types.DistinguishedNameQualifier
  
import Network.AWS.CertificateManagerPCA.Types.GenerationQualifier
  
import Network.AWS.CertificateManagerPCA.Types.GivenName
  
import Network.AWS.CertificateManagerPCA.Types.Organization
  
import Network.AWS.CertificateManagerPCA.Types.OrganizationalUnit
  
import Network.AWS.CertificateManagerPCA.Types.SerialNumber
  
import Network.AWS.CertificateManagerPCA.Types.Surname
  
import Network.AWS.CertificateManagerPCA.Types.Title
  

-- | API version @2017-08-22@ of the Amazon Certificate Manager Private Certificate Authority SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "CertificateManagerPCA",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "acm-pca",
                 Core._svcVersion = "2017-08-22", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "CertificateManagerPCA",
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
          | Core.otherwise = Core.Nothing

-- | The tag associated with the CA is not valid. The invalid argument is contained in the message field.
_InvalidTagException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTagException
  = Core._MatchServiceError mkServiceConfig "InvalidTagException"
{-# INLINEABLE _InvalidTagException #-}
{-# DEPRECATED _InvalidTagException "Use generic-lens or generic-optics instead"  #-}

-- | The request action cannot be performed or is prohibited.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException
  = Core._MatchServiceError mkServiceConfig "InvalidRequestException"
{-# INLINEABLE _InvalidRequestException #-}
{-# DEPRECATED _InvalidRequestException "Use generic-lens or generic-optics instead"  #-}

-- | The designated permission has already been given to the user.
_PermissionAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PermissionAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "PermissionAlreadyExistsException"
{-# INLINEABLE _PermissionAlreadyExistsException #-}
{-# DEPRECATED _PermissionAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | The certificate signing request is invalid.
_MalformedCSRException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MalformedCSRException
  = Core._MatchServiceError mkServiceConfig "MalformedCSRException"
{-# INLINEABLE _MalformedCSRException #-}
{-# DEPRECATED _MalformedCSRException "Use generic-lens or generic-optics instead"  #-}

-- | Your request has already been completed.
_RequestAlreadyProcessedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestAlreadyProcessedException
  = Core._MatchServiceError mkServiceConfig
      "RequestAlreadyProcessedException"
{-# INLINEABLE _RequestAlreadyProcessedException #-}
{-# DEPRECATED _RequestAlreadyProcessedException "Use generic-lens or generic-optics instead"  #-}

-- | One or more fields in the certificate are invalid.
_MalformedCertificateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MalformedCertificateException
  = Core._MatchServiceError mkServiceConfig
      "MalformedCertificateException"
{-# INLINEABLE _MalformedCertificateException #-}
{-# DEPRECATED _MalformedCertificateException "Use generic-lens or generic-optics instead"  #-}

-- | The request has failed for an unspecified reason.
_RequestFailedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestFailedException
  = Core._MatchServiceError mkServiceConfig "RequestFailedException"
{-# INLINEABLE _RequestFailedException #-}
{-# DEPRECATED _RequestFailedException "Use generic-lens or generic-optics instead"  #-}

-- | The certificate authority certificate you are importing does not comply with conditions specified in the certificate that signed it.
_CertificateMismatchException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CertificateMismatchException
  = Core._MatchServiceError mkServiceConfig
      "CertificateMismatchException"
{-# INLINEABLE _CertificateMismatchException #-}
{-# DEPRECATED _CertificateMismatchException "Use generic-lens or generic-optics instead"  #-}

-- | You can associate up to 50 tags with a private CA. Exception information is contained in the exception message field.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException
  = Core._MatchServiceError mkServiceConfig "TooManyTagsException"
{-# INLINEABLE _TooManyTagsException #-}
{-# DEPRECATED _TooManyTagsException "Use generic-lens or generic-optics instead"  #-}

-- | One or more of the specified arguments was not valid.
_InvalidArgsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArgsException
  = Core._MatchServiceError mkServiceConfig "InvalidArgsException"
{-# INLINEABLE _InvalidArgsException #-}
{-# DEPRECATED _InvalidArgsException "Use generic-lens or generic-optics instead"  #-}

-- | Your request is already in progress.
_RequestInProgressException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestInProgressException
  = Core._MatchServiceError mkServiceConfig
      "RequestInProgressException"
{-# INLINEABLE _RequestInProgressException #-}
{-# DEPRECATED _RequestInProgressException "Use generic-lens or generic-optics instead"  #-}

-- | A previous update to your private CA is still ongoing.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException
  = Core._MatchServiceError mkServiceConfig
      "ConcurrentModificationException"
{-# INLINEABLE _ConcurrentModificationException #-}
{-# DEPRECATED _ConcurrentModificationException "Use generic-lens or generic-optics instead"  #-}

-- | The token specified in the @NextToken@ argument is not valid. Use the token returned from your previous call to <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities> .
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException
  = Core._MatchServiceError mkServiceConfig
      "InvalidNextTokenException"
{-# INLINEABLE _InvalidNextTokenException #-}
{-# DEPRECATED _InvalidNextTokenException "Use generic-lens or generic-optics instead"  #-}

-- | The current action was prevented because it would lock the caller out from performing subsequent actions. Verify that the specified parameters would not result in the caller being denied access to the resource. 
_LockoutPreventedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LockoutPreventedException
  = Core._MatchServiceError mkServiceConfig
      "LockoutPreventedException"
{-# INLINEABLE _LockoutPreventedException #-}
{-# DEPRECATED _LockoutPreventedException "Use generic-lens or generic-optics instead"  #-}

-- | The requested Amazon Resource Name (ARN) does not refer to an existing resource.
_InvalidArnException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArnException
  = Core._MatchServiceError mkServiceConfig "InvalidArnException"
{-# INLINEABLE _InvalidArnException #-}
{-# DEPRECATED _InvalidArnException "Use generic-lens or generic-optics instead"  #-}

-- | The resource policy is invalid or is missing a required statement. For general information about IAM policy and statement structure, see <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#access_policies-json Overview of JSON Policies> .
_InvalidPolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPolicyException
  = Core._MatchServiceError mkServiceConfig "InvalidPolicyException"
{-# INLINEABLE _InvalidPolicyException #-}
{-# DEPRECATED _InvalidPolicyException "Use generic-lens or generic-optics instead"  #-}

-- | A resource such as a private CA, S3 bucket, certificate, audit report, or policy cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The state of the private CA does not allow this action to occur.
_InvalidStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidStateException
  = Core._MatchServiceError mkServiceConfig "InvalidStateException"
{-# INLINEABLE _InvalidStateException #-}
{-# DEPRECATED _InvalidStateException "Use generic-lens or generic-optics instead"  #-}

-- | An ACM Private CA quota has been exceeded. See the exception message returned to determine the quota that was exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}

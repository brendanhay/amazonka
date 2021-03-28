-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManager.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _InvalidTagException
    , _InvalidParameterException
    , _InvalidDomainValidationOptionsException
    , _TooManyTagsException
    , _InvalidArgsException
    , _RequestInProgressException
    , _TagPolicyException
    , _InvalidArnException
    , _ResourceNotFoundException
    , _InvalidStateException
    , _LimitExceededException
    , _ResourceInUseException

    -- * DomainStatus
    , DomainStatus (..)

    -- * IdempotencyToken
    , IdempotencyToken (..)

    -- * FailureReason
    , FailureReason (..)

    -- * CertificateDetail
    , CertificateDetail (..)
    , mkCertificateDetail
    , cdCertificateArn
    , cdCertificateAuthorityArn
    , cdCreatedAt
    , cdDomainName
    , cdDomainValidationOptions
    , cdExtendedKeyUsages
    , cdFailureReason
    , cdImportedAt
    , cdInUseBy
    , cdIssuedAt
    , cdIssuer
    , cdKeyAlgorithm
    , cdKeyUsages
    , cdNotAfter
    , cdNotBefore
    , cdOptions
    , cdRenewalEligibility
    , cdRenewalSummary
    , cdRevocationReason
    , cdRevokedAt
    , cdSerial
    , cdSignatureAlgorithm
    , cdStatus
    , cdSubject
    , cdSubjectAlternativeNames
    , cdType

    -- * ValidationMethod
    , ValidationMethod (..)

    -- * ResourceRecord
    , ResourceRecord (..)
    , mkResourceRecord
    , rrName
    , rrType
    , rrValue

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * PrivateKey
    , PrivateKey (..)

    -- * ExtendedKeyUsageName
    , ExtendedKeyUsageName (..)

    -- * Arn
    , Arn (..)

    -- * CertificateType
    , CertificateType (..)

    -- * RecordType
    , RecordType (..)

    -- * KeyUsageName
    , KeyUsageName (..)

    -- * DomainValidationOption
    , DomainValidationOption (..)
    , mkDomainValidationOption
    , dvoDomainName
    , dvoValidationDomain

    -- * Filters
    , Filters (..)
    , mkFilters
    , fExtendedKeyUsage
    , fKeyTypes
    , fKeyUsage

    -- * RenewalEligibility
    , RenewalEligibility (..)

    -- * CertificateSummary
    , CertificateSummary (..)
    , mkCertificateSummary
    , csCertificateArn
    , csDomainName

    -- * NextToken
    , NextToken (..)

    -- * RevocationReason
    , RevocationReason (..)

    -- * RenewalStatus
    , RenewalStatus (..)

    -- * DomainNameString
    , DomainNameString (..)

    -- * RenewalSummary
    , RenewalSummary (..)
    , mkRenewalSummary
    , rsRenewalStatus
    , rsDomainValidationOptions
    , rsUpdatedAt
    , rsRenewalStatusReason

    -- * KeyUsage
    , KeyUsage (..)
    , mkKeyUsage
    , kuName

    -- * CertificateOptions
    , CertificateOptions (..)
    , mkCertificateOptions
    , coCertificateTransparencyLoggingPreference

    -- * ExtendedKeyUsage
    , ExtendedKeyUsage (..)
    , mkExtendedKeyUsage
    , ekuName
    , ekuOID

    -- * CertificateTransparencyLoggingPreference
    , CertificateTransparencyLoggingPreference (..)

    -- * KeyAlgorithm
    , KeyAlgorithm (..)

    -- * CertificateChain
    , CertificateChain (..)

    -- * DomainValidation
    , DomainValidation (..)
    , mkDomainValidation
    , dvDomainName
    , dvResourceRecord
    , dvValidationDomain
    , dvValidationEmails
    , dvValidationMethod
    , dvValidationStatus

    -- * CertificateStatus
    , CertificateStatus (..)

    -- * CertificateArn
    , CertificateArn (..)

    -- * CertificateAuthorityArn
    , CertificateAuthorityArn (..)

    -- * DomainName
    , DomainName (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * ValidationDomain
    , ValidationDomain (..)

    -- * Certificate
    , Certificate (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.CertificateManager.Types.DomainStatus
  
  
import Network.AWS.CertificateManager.Types.IdempotencyToken
  
import Network.AWS.CertificateManager.Types.FailureReason
  
import Network.AWS.CertificateManager.Types.CertificateDetail
  
import Network.AWS.CertificateManager.Types.ValidationMethod
  
  
import Network.AWS.CertificateManager.Types.ResourceRecord
  
import Network.AWS.CertificateManager.Types.Tag
  
import Network.AWS.CertificateManager.Types.PrivateKey
  
import Network.AWS.CertificateManager.Types.ExtendedKeyUsageName
  
import Network.AWS.CertificateManager.Types.Arn
  
import Network.AWS.CertificateManager.Types.CertificateType
  
import Network.AWS.CertificateManager.Types.RecordType
  
import Network.AWS.CertificateManager.Types.KeyUsageName
  
  
import Network.AWS.CertificateManager.Types.DomainValidationOption
  
import Network.AWS.CertificateManager.Types.Filters
  
import Network.AWS.CertificateManager.Types.RenewalEligibility
  
  
  
import Network.AWS.CertificateManager.Types.CertificateSummary
  
import Network.AWS.CertificateManager.Types.NextToken
  
  
import Network.AWS.CertificateManager.Types.RevocationReason
  
import Network.AWS.CertificateManager.Types.RenewalStatus
  
import Network.AWS.CertificateManager.Types.DomainNameString
  
import Network.AWS.CertificateManager.Types.RenewalSummary
  
import Network.AWS.CertificateManager.Types.KeyUsage
  
import Network.AWS.CertificateManager.Types.CertificateOptions
  
import Network.AWS.CertificateManager.Types.ExtendedKeyUsage
  
import Network.AWS.CertificateManager.Types.CertificateTransparencyLoggingPreference
  
  
import Network.AWS.CertificateManager.Types.KeyAlgorithm
  
import Network.AWS.CertificateManager.Types.CertificateChain
  
import Network.AWS.CertificateManager.Types.DomainValidation
  
  
  
import Network.AWS.CertificateManager.Types.CertificateStatus
  
  
  
  
import Network.AWS.CertificateManager.Types.CertificateArn
  
import Network.AWS.CertificateManager.Types.CertificateAuthorityArn
  
import Network.AWS.CertificateManager.Types.DomainName
  
import Network.AWS.CertificateManager.Types.Key
  
import Network.AWS.CertificateManager.Types.Value
  
import Network.AWS.CertificateManager.Types.ValidationDomain
  
import Network.AWS.CertificateManager.Types.Certificate
  

-- | API version @2015-12-08@ of the Amazon Certificate Manager SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "CertificateManager",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "acm",
                 Core._svcVersion = "2015-12-08", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "CertificateManager",
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

-- | One or both of the values that make up the key-value pair is not valid. For example, you cannot specify a tag value that begins with @aws:@ .
_InvalidTagException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTagException
  = Core._MatchServiceError mkServiceConfig "InvalidTagException"
{-# INLINEABLE _InvalidTagException #-}
{-# DEPRECATED _InvalidTagException "Use generic-lens or generic-optics instead"  #-}

-- | An input parameter was invalid.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException
  = Core._MatchServiceError mkServiceConfig
      "InvalidParameterException"
{-# INLINEABLE _InvalidParameterException #-}
{-# DEPRECATED _InvalidParameterException "Use generic-lens or generic-optics instead"  #-}

-- | One or more values in the 'DomainValidationOption' structure is incorrect.
_InvalidDomainValidationOptionsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDomainValidationOptionsException
  = Core._MatchServiceError mkServiceConfig
      "InvalidDomainValidationOptionsException"
{-# INLINEABLE _InvalidDomainValidationOptionsException #-}
{-# DEPRECATED _InvalidDomainValidationOptionsException "Use generic-lens or generic-optics instead"  #-}

-- | The request contains too many tags. Try the request again with fewer tags.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException
  = Core._MatchServiceError mkServiceConfig "TooManyTagsException"
{-# INLINEABLE _TooManyTagsException #-}
{-# DEPRECATED _TooManyTagsException "Use generic-lens or generic-optics instead"  #-}

-- | One or more of of request parameters specified is not valid.
_InvalidArgsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArgsException
  = Core._MatchServiceError mkServiceConfig "InvalidArgsException"
{-# INLINEABLE _InvalidArgsException #-}
{-# DEPRECATED _InvalidArgsException "Use generic-lens or generic-optics instead"  #-}

-- | The certificate request is in process and the certificate in your account has not yet been issued.
_RequestInProgressException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestInProgressException
  = Core._MatchServiceError mkServiceConfig
      "RequestInProgressException"
{-# INLINEABLE _RequestInProgressException #-}
{-# DEPRECATED _RequestInProgressException "Use generic-lens or generic-optics instead"  #-}

-- | A specified tag did not comply with an existing tag policy and was rejected.
_TagPolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagPolicyException
  = Core._MatchServiceError mkServiceConfig "TagPolicyException"
{-# INLINEABLE _TagPolicyException #-}
{-# DEPRECATED _TagPolicyException "Use generic-lens or generic-optics instead"  #-}

-- | The requested Amazon Resource Name (ARN) does not refer to an existing resource.
_InvalidArnException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArnException
  = Core._MatchServiceError mkServiceConfig "InvalidArnException"
{-# INLINEABLE _InvalidArnException #-}
{-# DEPRECATED _InvalidArnException "Use generic-lens or generic-optics instead"  #-}

-- | The specified certificate cannot be found in the caller's account or the caller's account cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | Processing has reached an invalid state.
_InvalidStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidStateException
  = Core._MatchServiceError mkServiceConfig "InvalidStateException"
{-# INLINEABLE _InvalidStateException #-}
{-# DEPRECATED _InvalidStateException "Use generic-lens or generic-optics instead"  #-}

-- | An ACM quota has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The certificate is in use by another AWS service in the caller's account. Remove the association and try again.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException
  = Core._MatchServiceError mkServiceConfig "ResourceInUseException"
{-# INLINEABLE _ResourceInUseException #-}
{-# DEPRECATED _ResourceInUseException "Use generic-lens or generic-optics instead"  #-}

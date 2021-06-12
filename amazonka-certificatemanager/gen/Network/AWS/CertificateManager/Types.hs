{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _TooManyTagsException,
    _InvalidStateException,
    _InvalidArgsException,
    _InvalidArnException,
    _TagPolicyException,
    _InvalidDomainValidationOptionsException,
    _ThrottlingException,
    _InvalidParameterException,
    _AccessDeniedException,
    _ValidationException,
    _InvalidTagException,
    _RequestInProgressException,
    _ResourceInUseException,
    _LimitExceededException,
    _ConflictException,
    _ResourceNotFoundException,

    -- * CertificateStatus
    CertificateStatus (..),

    -- * CertificateTransparencyLoggingPreference
    CertificateTransparencyLoggingPreference (..),

    -- * CertificateType
    CertificateType (..),

    -- * DomainStatus
    DomainStatus (..),

    -- * ExtendedKeyUsageName
    ExtendedKeyUsageName (..),

    -- * FailureReason
    FailureReason (..),

    -- * KeyAlgorithm
    KeyAlgorithm (..),

    -- * KeyUsageName
    KeyUsageName (..),

    -- * RecordType
    RecordType (..),

    -- * RenewalEligibility
    RenewalEligibility (..),

    -- * RenewalStatus
    RenewalStatus (..),

    -- * RevocationReason
    RevocationReason (..),

    -- * ValidationMethod
    ValidationMethod (..),

    -- * CertificateDetail
    CertificateDetail (..),
    newCertificateDetail,
    certificateDetail_status,
    certificateDetail_notBefore,
    certificateDetail_certificateAuthorityArn,
    certificateDetail_importedAt,
    certificateDetail_extendedKeyUsages,
    certificateDetail_domainValidationOptions,
    certificateDetail_renewalEligibility,
    certificateDetail_options,
    certificateDetail_serial,
    certificateDetail_certificateArn,
    certificateDetail_createdAt,
    certificateDetail_inUseBy,
    certificateDetail_subjectAlternativeNames,
    certificateDetail_domainName,
    certificateDetail_revocationReason,
    certificateDetail_subject,
    certificateDetail_failureReason,
    certificateDetail_keyUsages,
    certificateDetail_revokedAt,
    certificateDetail_notAfter,
    certificateDetail_signatureAlgorithm,
    certificateDetail_issuer,
    certificateDetail_type,
    certificateDetail_keyAlgorithm,
    certificateDetail_issuedAt,
    certificateDetail_renewalSummary,

    -- * CertificateOptions
    CertificateOptions (..),
    newCertificateOptions,
    certificateOptions_certificateTransparencyLoggingPreference,

    -- * CertificateSummary
    CertificateSummary (..),
    newCertificateSummary,
    certificateSummary_certificateArn,
    certificateSummary_domainName,

    -- * DomainValidation
    DomainValidation (..),
    newDomainValidation,
    domainValidation_resourceRecord,
    domainValidation_validationEmails,
    domainValidation_validationMethod,
    domainValidation_validationStatus,
    domainValidation_validationDomain,
    domainValidation_domainName,

    -- * DomainValidationOption
    DomainValidationOption (..),
    newDomainValidationOption,
    domainValidationOption_domainName,
    domainValidationOption_validationDomain,

    -- * ExpiryEventsConfiguration
    ExpiryEventsConfiguration (..),
    newExpiryEventsConfiguration,
    expiryEventsConfiguration_daysBeforeExpiry,

    -- * ExtendedKeyUsage
    ExtendedKeyUsage (..),
    newExtendedKeyUsage,
    extendedKeyUsage_name,
    extendedKeyUsage_oid,

    -- * Filters
    Filters (..),
    newFilters,
    filters_keyTypes,
    filters_extendedKeyUsage,
    filters_keyUsage,

    -- * KeyUsage
    KeyUsage (..),
    newKeyUsage,
    keyUsage_name,

    -- * RenewalSummary
    RenewalSummary (..),
    newRenewalSummary,
    renewalSummary_renewalStatusReason,
    renewalSummary_renewalStatus,
    renewalSummary_domainValidationOptions,
    renewalSummary_updatedAt,

    -- * ResourceRecord
    ResourceRecord (..),
    newResourceRecord,
    resourceRecord_name,
    resourceRecord_type,
    resourceRecord_value,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,
  )
where

import Network.AWS.CertificateManager.Types.CertificateDetail
import Network.AWS.CertificateManager.Types.CertificateOptions
import Network.AWS.CertificateManager.Types.CertificateStatus
import Network.AWS.CertificateManager.Types.CertificateSummary
import Network.AWS.CertificateManager.Types.CertificateTransparencyLoggingPreference
import Network.AWS.CertificateManager.Types.CertificateType
import Network.AWS.CertificateManager.Types.DomainStatus
import Network.AWS.CertificateManager.Types.DomainValidation
import Network.AWS.CertificateManager.Types.DomainValidationOption
import Network.AWS.CertificateManager.Types.ExpiryEventsConfiguration
import Network.AWS.CertificateManager.Types.ExtendedKeyUsage
import Network.AWS.CertificateManager.Types.ExtendedKeyUsageName
import Network.AWS.CertificateManager.Types.FailureReason
import Network.AWS.CertificateManager.Types.Filters
import Network.AWS.CertificateManager.Types.KeyAlgorithm
import Network.AWS.CertificateManager.Types.KeyUsage
import Network.AWS.CertificateManager.Types.KeyUsageName
import Network.AWS.CertificateManager.Types.RecordType
import Network.AWS.CertificateManager.Types.RenewalEligibility
import Network.AWS.CertificateManager.Types.RenewalStatus
import Network.AWS.CertificateManager.Types.RenewalSummary
import Network.AWS.CertificateManager.Types.ResourceRecord
import Network.AWS.CertificateManager.Types.RevocationReason
import Network.AWS.CertificateManager.Types.Tag
import Network.AWS.CertificateManager.Types.ValidationMethod
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-12-08@ of the Amazon Certificate Manager SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "CertificateManager",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "acm",
      Core._serviceSigningName = "acm",
      Core._serviceVersion = "2015-12-08",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "CertificateManager",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | The request contains too many tags. Try the request again with fewer
-- tags.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | Processing has reached an invalid state.
_InvalidStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidStateException =
  Core._MatchServiceError
    defaultService
    "InvalidStateException"

-- | One or more of of request parameters specified is not valid.
_InvalidArgsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArgsException =
  Core._MatchServiceError
    defaultService
    "InvalidArgsException"

-- | The requested Amazon Resource Name (ARN) does not refer to an existing
-- resource.
_InvalidArnException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArnException =
  Core._MatchServiceError
    defaultService
    "InvalidArnException"

-- | A specified tag did not comply with an existing tag policy and was
-- rejected.
_TagPolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagPolicyException =
  Core._MatchServiceError
    defaultService
    "TagPolicyException"

-- | One or more values in the DomainValidationOption structure is incorrect.
_InvalidDomainValidationOptionsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDomainValidationOptionsException =
  Core._MatchServiceError
    defaultService
    "InvalidDomainValidationOptionsException"

-- | The request was denied because it exceeded a quota.
_ThrottlingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | An input parameter was invalid.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | You do not have access required to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The supplied input failed to satisfy constraints of an AWS service.
_ValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | One or both of the values that make up the key-value pair is not valid.
-- For example, you cannot specify a tag value that begins with @aws:@.
_InvalidTagException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTagException =
  Core._MatchServiceError
    defaultService
    "InvalidTagException"

-- | The certificate request is in process and the certificate in your
-- account has not yet been issued.
_RequestInProgressException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestInProgressException =
  Core._MatchServiceError
    defaultService
    "RequestInProgressException"

-- | The certificate is in use by another AWS service in the caller\'s
-- account. Remove the association and try again.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | An ACM quota has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | You are trying to update a resource or configuration that is already
-- being created or updated. Wait for the previous operation to finish and
-- try again.
_ConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | The specified certificate cannot be found in the caller\'s account or
-- the caller\'s account cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

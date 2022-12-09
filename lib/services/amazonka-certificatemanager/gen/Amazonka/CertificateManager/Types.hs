{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CertificateManager.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManager.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InvalidArgsException,
    _InvalidArnException,
    _InvalidDomainValidationOptionsException,
    _InvalidParameterException,
    _InvalidStateException,
    _InvalidTagException,
    _LimitExceededException,
    _RequestInProgressException,
    _ResourceInUseException,
    _ResourceNotFoundException,
    _TagPolicyException,
    _ThrottlingException,
    _TooManyTagsException,
    _ValidationException,

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

    -- * SortBy
    SortBy (..),

    -- * SortOrder
    SortOrder (..),

    -- * ValidationMethod
    ValidationMethod (..),

    -- * CertificateDetail
    CertificateDetail (..),
    newCertificateDetail,
    certificateDetail_certificateArn,
    certificateDetail_certificateAuthorityArn,
    certificateDetail_createdAt,
    certificateDetail_domainName,
    certificateDetail_domainValidationOptions,
    certificateDetail_extendedKeyUsages,
    certificateDetail_failureReason,
    certificateDetail_importedAt,
    certificateDetail_inUseBy,
    certificateDetail_issuedAt,
    certificateDetail_issuer,
    certificateDetail_keyAlgorithm,
    certificateDetail_keyUsages,
    certificateDetail_notAfter,
    certificateDetail_notBefore,
    certificateDetail_options,
    certificateDetail_renewalEligibility,
    certificateDetail_renewalSummary,
    certificateDetail_revocationReason,
    certificateDetail_revokedAt,
    certificateDetail_serial,
    certificateDetail_signatureAlgorithm,
    certificateDetail_status,
    certificateDetail_subject,
    certificateDetail_subjectAlternativeNames,
    certificateDetail_type,

    -- * CertificateOptions
    CertificateOptions (..),
    newCertificateOptions,
    certificateOptions_certificateTransparencyLoggingPreference,

    -- * CertificateSummary
    CertificateSummary (..),
    newCertificateSummary,
    certificateSummary_certificateArn,
    certificateSummary_createdAt,
    certificateSummary_domainName,
    certificateSummary_exported,
    certificateSummary_extendedKeyUsages,
    certificateSummary_hasAdditionalSubjectAlternativeNames,
    certificateSummary_importedAt,
    certificateSummary_inUse,
    certificateSummary_issuedAt,
    certificateSummary_keyAlgorithm,
    certificateSummary_keyUsages,
    certificateSummary_notAfter,
    certificateSummary_notBefore,
    certificateSummary_renewalEligibility,
    certificateSummary_revokedAt,
    certificateSummary_status,
    certificateSummary_subjectAlternativeNameSummaries,
    certificateSummary_type,

    -- * DomainValidation
    DomainValidation (..),
    newDomainValidation,
    domainValidation_resourceRecord,
    domainValidation_validationDomain,
    domainValidation_validationEmails,
    domainValidation_validationMethod,
    domainValidation_validationStatus,
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
    filters_extendedKeyUsage,
    filters_keyTypes,
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

import Amazonka.CertificateManager.Types.CertificateDetail
import Amazonka.CertificateManager.Types.CertificateOptions
import Amazonka.CertificateManager.Types.CertificateStatus
import Amazonka.CertificateManager.Types.CertificateSummary
import Amazonka.CertificateManager.Types.CertificateTransparencyLoggingPreference
import Amazonka.CertificateManager.Types.CertificateType
import Amazonka.CertificateManager.Types.DomainStatus
import Amazonka.CertificateManager.Types.DomainValidation
import Amazonka.CertificateManager.Types.DomainValidationOption
import Amazonka.CertificateManager.Types.ExpiryEventsConfiguration
import Amazonka.CertificateManager.Types.ExtendedKeyUsage
import Amazonka.CertificateManager.Types.ExtendedKeyUsageName
import Amazonka.CertificateManager.Types.FailureReason
import Amazonka.CertificateManager.Types.Filters
import Amazonka.CertificateManager.Types.KeyAlgorithm
import Amazonka.CertificateManager.Types.KeyUsage
import Amazonka.CertificateManager.Types.KeyUsageName
import Amazonka.CertificateManager.Types.RecordType
import Amazonka.CertificateManager.Types.RenewalEligibility
import Amazonka.CertificateManager.Types.RenewalStatus
import Amazonka.CertificateManager.Types.RenewalSummary
import Amazonka.CertificateManager.Types.ResourceRecord
import Amazonka.CertificateManager.Types.RevocationReason
import Amazonka.CertificateManager.Types.SortBy
import Amazonka.CertificateManager.Types.SortOrder
import Amazonka.CertificateManager.Types.Tag
import Amazonka.CertificateManager.Types.ValidationMethod
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2015-12-08@ of the Amazon Certificate Manager SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CertificateManager",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "acm",
      Core.signingName = "acm",
      Core.version = "2015-12-08",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "CertificateManager",
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

-- | You do not have access required to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | You are trying to update a resource or configuration that is already
-- being created or updated. Wait for the previous operation to finish and
-- try again.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | One or more of of request parameters specified is not valid.
_InvalidArgsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArgsException =
  Core._MatchServiceError
    defaultService
    "InvalidArgsException"

-- | The requested Amazon Resource Name (ARN) does not refer to an existing
-- resource.
_InvalidArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArnException =
  Core._MatchServiceError
    defaultService
    "InvalidArnException"

-- | One or more values in the DomainValidationOption structure is incorrect.
_InvalidDomainValidationOptionsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDomainValidationOptionsException =
  Core._MatchServiceError
    defaultService
    "InvalidDomainValidationOptionsException"

-- | An input parameter was invalid.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | Processing has reached an invalid state.
_InvalidStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidStateException =
  Core._MatchServiceError
    defaultService
    "InvalidStateException"

-- | One or both of the values that make up the key-value pair is not valid.
-- For example, you cannot specify a tag value that begins with @aws:@.
_InvalidTagException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTagException =
  Core._MatchServiceError
    defaultService
    "InvalidTagException"

-- | An ACM quota has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The certificate request is in process and the certificate in your
-- account has not yet been issued.
_RequestInProgressException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RequestInProgressException =
  Core._MatchServiceError
    defaultService
    "RequestInProgressException"

-- | The certificate is in use by another Amazon Web Services service in the
-- caller\'s account. Remove the association and try again.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The specified certificate cannot be found in the caller\'s account or
-- the caller\'s account cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | A specified tag did not comply with an existing tag policy and was
-- rejected.
_TagPolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagPolicyException =
  Core._MatchServiceError
    defaultService
    "TagPolicyException"

-- | The request was denied because it exceeded a quota.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The request contains too many tags. Try the request again with fewer
-- tags.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The supplied input failed to satisfy constraints of an Amazon Web
-- Services service.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

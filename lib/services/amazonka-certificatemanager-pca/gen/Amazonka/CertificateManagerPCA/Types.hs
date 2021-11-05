{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CertificateManagerPCA.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidTagException,
    _InvalidRequestException,
    _PermissionAlreadyExistsException,
    _MalformedCSRException,
    _RequestAlreadyProcessedException,
    _MalformedCertificateException,
    _RequestFailedException,
    _CertificateMismatchException,
    _TooManyTagsException,
    _InvalidArgsException,
    _RequestInProgressException,
    _ConcurrentModificationException,
    _InvalidNextTokenException,
    _LockoutPreventedException,
    _InvalidArnException,
    _InvalidPolicyException,
    _ResourceNotFoundException,
    _InvalidStateException,
    _LimitExceededException,

    -- * AccessMethodType
    AccessMethodType (..),

    -- * ActionType
    ActionType (..),

    -- * AuditReportResponseFormat
    AuditReportResponseFormat (..),

    -- * AuditReportStatus
    AuditReportStatus (..),

    -- * CertificateAuthorityStatus
    CertificateAuthorityStatus (..),

    -- * CertificateAuthorityType
    CertificateAuthorityType (..),

    -- * ExtendedKeyUsageType
    ExtendedKeyUsageType (..),

    -- * FailureReason
    FailureReason (..),

    -- * KeyAlgorithm
    KeyAlgorithm (..),

    -- * KeyStorageSecurityStandard
    KeyStorageSecurityStandard (..),

    -- * PolicyQualifierId
    PolicyQualifierId (..),

    -- * ResourceOwner
    ResourceOwner (..),

    -- * RevocationReason
    RevocationReason (..),

    -- * S3ObjectAcl
    S3ObjectAcl (..),

    -- * SigningAlgorithm
    SigningAlgorithm (..),

    -- * ValidityPeriodType
    ValidityPeriodType (..),

    -- * ASN1Subject
    ASN1Subject (..),
    newASN1Subject,
    aSN1Subject_givenName,
    aSN1Subject_state,
    aSN1Subject_commonName,
    aSN1Subject_organizationalUnit,
    aSN1Subject_country,
    aSN1Subject_generationQualifier,
    aSN1Subject_locality,
    aSN1Subject_pseudonym,
    aSN1Subject_initials,
    aSN1Subject_title,
    aSN1Subject_organization,
    aSN1Subject_serialNumber,
    aSN1Subject_surname,
    aSN1Subject_distinguishedNameQualifier,

    -- * AccessDescription
    AccessDescription (..),
    newAccessDescription,
    accessDescription_accessMethod,
    accessDescription_accessLocation,

    -- * AccessMethod
    AccessMethod (..),
    newAccessMethod,
    accessMethod_accessMethodType,
    accessMethod_customObjectIdentifier,

    -- * ApiPassthrough
    ApiPassthrough (..),
    newApiPassthrough,
    apiPassthrough_subject,
    apiPassthrough_extensions,

    -- * CertificateAuthority
    CertificateAuthority (..),
    newCertificateAuthority,
    certificateAuthority_status,
    certificateAuthority_failureReason,
    certificateAuthority_certificateAuthorityConfiguration,
    certificateAuthority_arn,
    certificateAuthority_createdAt,
    certificateAuthority_serial,
    certificateAuthority_keyStorageSecurityStandard,
    certificateAuthority_notBefore,
    certificateAuthority_restorableUntil,
    certificateAuthority_type,
    certificateAuthority_ownerAccount,
    certificateAuthority_revocationConfiguration,
    certificateAuthority_lastStateChangeAt,
    certificateAuthority_notAfter,

    -- * CertificateAuthorityConfiguration
    CertificateAuthorityConfiguration (..),
    newCertificateAuthorityConfiguration,
    certificateAuthorityConfiguration_csrExtensions,
    certificateAuthorityConfiguration_keyAlgorithm,
    certificateAuthorityConfiguration_signingAlgorithm,
    certificateAuthorityConfiguration_subject,

    -- * CrlConfiguration
    CrlConfiguration (..),
    newCrlConfiguration,
    crlConfiguration_customCname,
    crlConfiguration_expirationInDays,
    crlConfiguration_s3ObjectAcl,
    crlConfiguration_s3BucketName,
    crlConfiguration_enabled,

    -- * CsrExtensions
    CsrExtensions (..),
    newCsrExtensions,
    csrExtensions_subjectInformationAccess,
    csrExtensions_keyUsage,

    -- * EdiPartyName
    EdiPartyName (..),
    newEdiPartyName,
    ediPartyName_nameAssigner,
    ediPartyName_partyName,

    -- * ExtendedKeyUsage
    ExtendedKeyUsage (..),
    newExtendedKeyUsage,
    extendedKeyUsage_extendedKeyUsageType,
    extendedKeyUsage_extendedKeyUsageObjectIdentifier,

    -- * Extensions
    Extensions (..),
    newExtensions,
    extensions_subjectAlternativeNames,
    extensions_keyUsage,
    extensions_extendedKeyUsage,
    extensions_certificatePolicies,

    -- * GeneralName
    GeneralName (..),
    newGeneralName,
    generalName_ipAddress,
    generalName_uniformResourceIdentifier,
    generalName_registeredId,
    generalName_ediPartyName,
    generalName_rfc822Name,
    generalName_otherName,
    generalName_dnsName,
    generalName_directoryName,

    -- * KeyUsage
    KeyUsage (..),
    newKeyUsage,
    keyUsage_dataEncipherment,
    keyUsage_encipherOnly,
    keyUsage_nonRepudiation,
    keyUsage_cRLSign,
    keyUsage_digitalSignature,
    keyUsage_keyCertSign,
    keyUsage_decipherOnly,
    keyUsage_keyEncipherment,
    keyUsage_keyAgreement,

    -- * OcspConfiguration
    OcspConfiguration (..),
    newOcspConfiguration,
    ocspConfiguration_ocspCustomCname,
    ocspConfiguration_enabled,

    -- * OtherName
    OtherName (..),
    newOtherName,
    otherName_typeId,
    otherName_value,

    -- * Permission
    Permission (..),
    newPermission,
    permission_sourceAccount,
    permission_actions,
    permission_createdAt,
    permission_principal,
    permission_policy,
    permission_certificateAuthorityArn,

    -- * PolicyInformation
    PolicyInformation (..),
    newPolicyInformation,
    policyInformation_policyQualifiers,
    policyInformation_certPolicyId,

    -- * PolicyQualifierInfo
    PolicyQualifierInfo (..),
    newPolicyQualifierInfo,
    policyQualifierInfo_policyQualifierId,
    policyQualifierInfo_qualifier,

    -- * Qualifier
    Qualifier (..),
    newQualifier,
    qualifier_cpsUri,

    -- * RevocationConfiguration
    RevocationConfiguration (..),
    newRevocationConfiguration,
    revocationConfiguration_crlConfiguration,
    revocationConfiguration_ocspConfiguration,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * Validity
    Validity (..),
    newValidity,
    validity_value,
    validity_type,
  )
where

import Amazonka.CertificateManagerPCA.Types.ASN1Subject
import Amazonka.CertificateManagerPCA.Types.AccessDescription
import Amazonka.CertificateManagerPCA.Types.AccessMethod
import Amazonka.CertificateManagerPCA.Types.AccessMethodType
import Amazonka.CertificateManagerPCA.Types.ActionType
import Amazonka.CertificateManagerPCA.Types.ApiPassthrough
import Amazonka.CertificateManagerPCA.Types.AuditReportResponseFormat
import Amazonka.CertificateManagerPCA.Types.AuditReportStatus
import Amazonka.CertificateManagerPCA.Types.CertificateAuthority
import Amazonka.CertificateManagerPCA.Types.CertificateAuthorityConfiguration
import Amazonka.CertificateManagerPCA.Types.CertificateAuthorityStatus
import Amazonka.CertificateManagerPCA.Types.CertificateAuthorityType
import Amazonka.CertificateManagerPCA.Types.CrlConfiguration
import Amazonka.CertificateManagerPCA.Types.CsrExtensions
import Amazonka.CertificateManagerPCA.Types.EdiPartyName
import Amazonka.CertificateManagerPCA.Types.ExtendedKeyUsage
import Amazonka.CertificateManagerPCA.Types.ExtendedKeyUsageType
import Amazonka.CertificateManagerPCA.Types.Extensions
import Amazonka.CertificateManagerPCA.Types.FailureReason
import Amazonka.CertificateManagerPCA.Types.GeneralName
import Amazonka.CertificateManagerPCA.Types.KeyAlgorithm
import Amazonka.CertificateManagerPCA.Types.KeyStorageSecurityStandard
import Amazonka.CertificateManagerPCA.Types.KeyUsage
import Amazonka.CertificateManagerPCA.Types.OcspConfiguration
import Amazonka.CertificateManagerPCA.Types.OtherName
import Amazonka.CertificateManagerPCA.Types.Permission
import Amazonka.CertificateManagerPCA.Types.PolicyInformation
import Amazonka.CertificateManagerPCA.Types.PolicyQualifierId
import Amazonka.CertificateManagerPCA.Types.PolicyQualifierInfo
import Amazonka.CertificateManagerPCA.Types.Qualifier
import Amazonka.CertificateManagerPCA.Types.ResourceOwner
import Amazonka.CertificateManagerPCA.Types.RevocationConfiguration
import Amazonka.CertificateManagerPCA.Types.RevocationReason
import Amazonka.CertificateManagerPCA.Types.S3ObjectAcl
import Amazonka.CertificateManagerPCA.Types.SigningAlgorithm
import Amazonka.CertificateManagerPCA.Types.Tag
import Amazonka.CertificateManagerPCA.Types.Validity
import Amazonka.CertificateManagerPCA.Types.ValidityPeriodType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-08-22@ of the Amazon Certificate Manager Private Certificate Authority SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "CertificateManagerPCA",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "acm-pca",
      Core._serviceSigningName = "acm-pca",
      Core._serviceVersion = "2017-08-22",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "CertificateManagerPCA",
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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The tag associated with the CA is not valid. The invalid argument is
-- contained in the message field.
_InvalidTagException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTagException =
  Core._MatchServiceError
    defaultService
    "InvalidTagException"

-- | The request action cannot be performed or is prohibited.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | The designated permission has already been given to the user.
_PermissionAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PermissionAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "PermissionAlreadyExistsException"

-- | The certificate signing request is invalid.
_MalformedCSRException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MalformedCSRException =
  Core._MatchServiceError
    defaultService
    "MalformedCSRException"

-- | Your request has already been completed.
_RequestAlreadyProcessedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RequestAlreadyProcessedException =
  Core._MatchServiceError
    defaultService
    "RequestAlreadyProcessedException"

-- | One or more fields in the certificate are invalid.
_MalformedCertificateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MalformedCertificateException =
  Core._MatchServiceError
    defaultService
    "MalformedCertificateException"

-- | The request has failed for an unspecified reason.
_RequestFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RequestFailedException =
  Core._MatchServiceError
    defaultService
    "RequestFailedException"

-- | The certificate authority certificate you are importing does not comply
-- with conditions specified in the certificate that signed it.
_CertificateMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CertificateMismatchException =
  Core._MatchServiceError
    defaultService
    "CertificateMismatchException"

-- | You can associate up to 50 tags with a private CA. Exception information
-- is contained in the exception message field.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | One or more of the specified arguments was not valid.
_InvalidArgsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArgsException =
  Core._MatchServiceError
    defaultService
    "InvalidArgsException"

-- | Your request is already in progress.
_RequestInProgressException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RequestInProgressException =
  Core._MatchServiceError
    defaultService
    "RequestInProgressException"

-- | A previous update to your private CA is still ongoing.
_ConcurrentModificationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | The token specified in the @NextToken@ argument is not valid. Use the
-- token returned from your previous call to
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities>.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | The current action was prevented because it would lock the caller out
-- from performing subsequent actions. Verify that the specified parameters
-- would not result in the caller being denied access to the resource.
_LockoutPreventedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LockoutPreventedException =
  Core._MatchServiceError
    defaultService
    "LockoutPreventedException"

-- | The requested Amazon Resource Name (ARN) does not refer to an existing
-- resource.
_InvalidArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArnException =
  Core._MatchServiceError
    defaultService
    "InvalidArnException"

-- | The resource policy is invalid or is missing a required statement. For
-- general information about IAM policy and statement structure, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#access_policies-json Overview of JSON Policies>.
_InvalidPolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPolicyException =
  Core._MatchServiceError
    defaultService
    "InvalidPolicyException"

-- | A resource such as a private CA, S3 bucket, certificate, audit report,
-- or policy cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The state of the private CA does not allow this action to occur.
_InvalidStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidStateException =
  Core._MatchServiceError
    defaultService
    "InvalidStateException"

-- | An ACM Private CA quota has been exceeded. See the exception message
-- returned to determine the quota that was exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

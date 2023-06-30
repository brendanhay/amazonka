{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CertificateManagerPCA.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _CertificateMismatchException,
    _ConcurrentModificationException,
    _InvalidArgsException,
    _InvalidArnException,
    _InvalidNextTokenException,
    _InvalidPolicyException,
    _InvalidRequestException,
    _InvalidStateException,
    _InvalidTagException,
    _LimitExceededException,
    _LockoutPreventedException,
    _MalformedCSRException,
    _MalformedCertificateException,
    _PermissionAlreadyExistsException,
    _RequestAlreadyProcessedException,
    _RequestFailedException,
    _RequestInProgressException,
    _ResourceNotFoundException,
    _TooManyTagsException,

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

    -- * CertificateAuthorityUsageMode
    CertificateAuthorityUsageMode (..),

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
    aSN1Subject_commonName,
    aSN1Subject_country,
    aSN1Subject_customAttributes,
    aSN1Subject_distinguishedNameQualifier,
    aSN1Subject_generationQualifier,
    aSN1Subject_givenName,
    aSN1Subject_initials,
    aSN1Subject_locality,
    aSN1Subject_organization,
    aSN1Subject_organizationalUnit,
    aSN1Subject_pseudonym,
    aSN1Subject_serialNumber,
    aSN1Subject_state,
    aSN1Subject_surname,
    aSN1Subject_title,

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
    apiPassthrough_extensions,
    apiPassthrough_subject,

    -- * CertificateAuthority
    CertificateAuthority (..),
    newCertificateAuthority,
    certificateAuthority_arn,
    certificateAuthority_certificateAuthorityConfiguration,
    certificateAuthority_createdAt,
    certificateAuthority_failureReason,
    certificateAuthority_keyStorageSecurityStandard,
    certificateAuthority_lastStateChangeAt,
    certificateAuthority_notAfter,
    certificateAuthority_notBefore,
    certificateAuthority_ownerAccount,
    certificateAuthority_restorableUntil,
    certificateAuthority_revocationConfiguration,
    certificateAuthority_serial,
    certificateAuthority_status,
    certificateAuthority_type,
    certificateAuthority_usageMode,

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
    crlConfiguration_s3BucketName,
    crlConfiguration_s3ObjectAcl,
    crlConfiguration_enabled,

    -- * CsrExtensions
    CsrExtensions (..),
    newCsrExtensions,
    csrExtensions_keyUsage,
    csrExtensions_subjectInformationAccess,

    -- * CustomAttribute
    CustomAttribute (..),
    newCustomAttribute,
    customAttribute_objectIdentifier,
    customAttribute_value,

    -- * CustomExtension
    CustomExtension (..),
    newCustomExtension,
    customExtension_critical,
    customExtension_objectIdentifier,
    customExtension_value,

    -- * EdiPartyName
    EdiPartyName (..),
    newEdiPartyName,
    ediPartyName_nameAssigner,
    ediPartyName_partyName,

    -- * ExtendedKeyUsage
    ExtendedKeyUsage (..),
    newExtendedKeyUsage,
    extendedKeyUsage_extendedKeyUsageObjectIdentifier,
    extendedKeyUsage_extendedKeyUsageType,

    -- * Extensions
    Extensions (..),
    newExtensions,
    extensions_certificatePolicies,
    extensions_customExtensions,
    extensions_extendedKeyUsage,
    extensions_keyUsage,
    extensions_subjectAlternativeNames,

    -- * GeneralName
    GeneralName (..),
    newGeneralName,
    generalName_directoryName,
    generalName_dnsName,
    generalName_ediPartyName,
    generalName_ipAddress,
    generalName_otherName,
    generalName_registeredId,
    generalName_rfc822Name,
    generalName_uniformResourceIdentifier,

    -- * KeyUsage
    KeyUsage (..),
    newKeyUsage,
    keyUsage_cRLSign,
    keyUsage_dataEncipherment,
    keyUsage_decipherOnly,
    keyUsage_digitalSignature,
    keyUsage_encipherOnly,
    keyUsage_keyAgreement,
    keyUsage_keyCertSign,
    keyUsage_keyEncipherment,
    keyUsage_nonRepudiation,

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
    permission_actions,
    permission_certificateAuthorityArn,
    permission_createdAt,
    permission_policy,
    permission_principal,
    permission_sourceAccount,

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
import Amazonka.CertificateManagerPCA.Types.CertificateAuthorityUsageMode
import Amazonka.CertificateManagerPCA.Types.CrlConfiguration
import Amazonka.CertificateManagerPCA.Types.CsrExtensions
import Amazonka.CertificateManagerPCA.Types.CustomAttribute
import Amazonka.CertificateManagerPCA.Types.CustomExtension
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-08-22@ of the Amazon Certificate Manager Private Certificate Authority SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CertificateManagerPCA",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "acm-pca",
      Core.signingName = "acm-pca",
      Core.version = "2017-08-22",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "CertificateManagerPCA",
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

-- | The certificate authority certificate you are importing does not comply
-- with conditions specified in the certificate that signed it.
_CertificateMismatchException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CertificateMismatchException =
  Core._MatchServiceError
    defaultService
    "CertificateMismatchException"

-- | A previous update to your private CA is still ongoing.
_ConcurrentModificationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConcurrentModificationException =
  Core._MatchServiceError
    defaultService
    "ConcurrentModificationException"

-- | One or more of the specified arguments was not valid.
_InvalidArgsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidArgsException =
  Core._MatchServiceError
    defaultService
    "InvalidArgsException"

-- | The requested Amazon Resource Name (ARN) does not refer to an existing
-- resource.
_InvalidArnException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidArnException =
  Core._MatchServiceError
    defaultService
    "InvalidArnException"

-- | The token specified in the @NextToken@ argument is not valid. Use the
-- token returned from your previous call to
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities>.
_InvalidNextTokenException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | The resource policy is invalid or is missing a required statement. For
-- general information about IAM policy and statement structure, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#access_policies-json Overview of JSON Policies>.
_InvalidPolicyException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidPolicyException =
  Core._MatchServiceError
    defaultService
    "InvalidPolicyException"

-- | The request action cannot be performed or is prohibited.
_InvalidRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | The state of the private CA does not allow this action to occur.
_InvalidStateException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidStateException =
  Core._MatchServiceError
    defaultService
    "InvalidStateException"

-- | The tag associated with the CA is not valid. The invalid argument is
-- contained in the message field.
_InvalidTagException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidTagException =
  Core._MatchServiceError
    defaultService
    "InvalidTagException"

-- | An Amazon Web Services Private CA quota has been exceeded. See the
-- exception message returned to determine the quota that was exceeded.
_LimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The current action was prevented because it would lock the caller out
-- from performing subsequent actions. Verify that the specified parameters
-- would not result in the caller being denied access to the resource.
_LockoutPreventedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_LockoutPreventedException =
  Core._MatchServiceError
    defaultService
    "LockoutPreventedException"

-- | The certificate signing request is invalid.
_MalformedCSRException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MalformedCSRException =
  Core._MatchServiceError
    defaultService
    "MalformedCSRException"

-- | One or more fields in the certificate are invalid.
_MalformedCertificateException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_MalformedCertificateException =
  Core._MatchServiceError
    defaultService
    "MalformedCertificateException"

-- | The designated permission has already been given to the user.
_PermissionAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_PermissionAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "PermissionAlreadyExistsException"

-- | Your request has already been completed.
_RequestAlreadyProcessedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RequestAlreadyProcessedException =
  Core._MatchServiceError
    defaultService
    "RequestAlreadyProcessedException"

-- | The request has failed for an unspecified reason.
_RequestFailedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RequestFailedException =
  Core._MatchServiceError
    defaultService
    "RequestFailedException"

-- | Your request is already in progress.
_RequestInProgressException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RequestInProgressException =
  Core._MatchServiceError
    defaultService
    "RequestInProgressException"

-- | A resource such as a private CA, S3 bucket, certificate, audit report,
-- or policy cannot be found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | You can associate up to 50 tags with a private CA. Exception information
-- is contained in the exception message field.
_TooManyTagsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

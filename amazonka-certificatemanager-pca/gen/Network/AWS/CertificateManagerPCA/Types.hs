{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _TooManyTagsException,
    _InvalidStateException,
    _InvalidArgsException,
    _InvalidPolicyException,
    _InvalidArnException,
    _MalformedCertificateException,
    _RequestAlreadyProcessedException,
    _ConcurrentModificationException,
    _InvalidNextTokenException,
    _PermissionAlreadyExistsException,
    _InvalidRequestException,
    _InvalidTagException,
    _RequestInProgressException,
    _LimitExceededException,
    _CertificateMismatchException,
    _ResourceNotFoundException,
    _RequestFailedException,
    _LockoutPreventedException,
    _MalformedCSRException,

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

    -- * PolicyQualifierId
    PolicyQualifierId (..),

    -- * ResourceOwner
    ResourceOwner (..),

    -- * RevocationReason
    RevocationReason (..),

    -- * SigningAlgorithm
    SigningAlgorithm (..),

    -- * ValidityPeriodType
    ValidityPeriodType (..),

    -- * ASN1Subject
    ASN1Subject (..),
    newASN1Subject,
    aSN1Subject_locality,
    aSN1Subject_generationQualifier,
    aSN1Subject_surname,
    aSN1Subject_title,
    aSN1Subject_organizationalUnit,
    aSN1Subject_initials,
    aSN1Subject_pseudonym,
    aSN1Subject_commonName,
    aSN1Subject_state,
    aSN1Subject_givenName,
    aSN1Subject_organization,
    aSN1Subject_distinguishedNameQualifier,
    aSN1Subject_serialNumber,
    aSN1Subject_country,

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
    certificateAuthority_status,
    certificateAuthority_notBefore,
    certificateAuthority_revocationConfiguration,
    certificateAuthority_serial,
    certificateAuthority_arn,
    certificateAuthority_createdAt,
    certificateAuthority_certificateAuthorityConfiguration,
    certificateAuthority_failureReason,
    certificateAuthority_notAfter,
    certificateAuthority_lastStateChangeAt,
    certificateAuthority_type,
    certificateAuthority_ownerAccount,
    certificateAuthority_restorableUntil,

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
    crlConfiguration_s3BucketName,
    crlConfiguration_expirationInDays,
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
    extendedKeyUsage_extendedKeyUsageObjectIdentifier,
    extendedKeyUsage_extendedKeyUsageType,

    -- * Extensions
    Extensions (..),
    newExtensions,
    extensions_certificatePolicies,
    extensions_extendedKeyUsage,
    extensions_subjectAlternativeNames,
    extensions_keyUsage,

    -- * GeneralName
    GeneralName (..),
    newGeneralName,
    generalName_ediPartyName,
    generalName_otherName,
    generalName_uniformResourceIdentifier,
    generalName_ipAddress,
    generalName_dnsName,
    generalName_directoryName,
    generalName_rfc822Name,
    generalName_registeredId,

    -- * KeyUsage
    KeyUsage (..),
    newKeyUsage,
    keyUsage_dataEncipherment,
    keyUsage_keyCertSign,
    keyUsage_cRLSign,
    keyUsage_keyEncipherment,
    keyUsage_encipherOnly,
    keyUsage_keyAgreement,
    keyUsage_digitalSignature,
    keyUsage_decipherOnly,
    keyUsage_nonRepudiation,

    -- * OtherName
    OtherName (..),
    newOtherName,
    otherName_typeId,
    otherName_value,

    -- * Permission
    Permission (..),
    newPermission,
    permission_certificateAuthorityArn,
    permission_createdAt,
    permission_actions,
    permission_principal,
    permission_sourceAccount,
    permission_policy,

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

import Network.AWS.CertificateManagerPCA.Types.ASN1Subject
import Network.AWS.CertificateManagerPCA.Types.AccessDescription
import Network.AWS.CertificateManagerPCA.Types.AccessMethod
import Network.AWS.CertificateManagerPCA.Types.AccessMethodType
import Network.AWS.CertificateManagerPCA.Types.ActionType
import Network.AWS.CertificateManagerPCA.Types.ApiPassthrough
import Network.AWS.CertificateManagerPCA.Types.AuditReportResponseFormat
import Network.AWS.CertificateManagerPCA.Types.AuditReportStatus
import Network.AWS.CertificateManagerPCA.Types.CertificateAuthority
import Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityConfiguration
import Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityStatus
import Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityType
import Network.AWS.CertificateManagerPCA.Types.CrlConfiguration
import Network.AWS.CertificateManagerPCA.Types.CsrExtensions
import Network.AWS.CertificateManagerPCA.Types.EdiPartyName
import Network.AWS.CertificateManagerPCA.Types.ExtendedKeyUsage
import Network.AWS.CertificateManagerPCA.Types.ExtendedKeyUsageType
import Network.AWS.CertificateManagerPCA.Types.Extensions
import Network.AWS.CertificateManagerPCA.Types.FailureReason
import Network.AWS.CertificateManagerPCA.Types.GeneralName
import Network.AWS.CertificateManagerPCA.Types.KeyAlgorithm
import Network.AWS.CertificateManagerPCA.Types.KeyUsage
import Network.AWS.CertificateManagerPCA.Types.OtherName
import Network.AWS.CertificateManagerPCA.Types.Permission
import Network.AWS.CertificateManagerPCA.Types.PolicyInformation
import Network.AWS.CertificateManagerPCA.Types.PolicyQualifierId
import Network.AWS.CertificateManagerPCA.Types.PolicyQualifierInfo
import Network.AWS.CertificateManagerPCA.Types.Qualifier
import Network.AWS.CertificateManagerPCA.Types.ResourceOwner
import Network.AWS.CertificateManagerPCA.Types.RevocationConfiguration
import Network.AWS.CertificateManagerPCA.Types.RevocationReason
import Network.AWS.CertificateManagerPCA.Types.SigningAlgorithm
import Network.AWS.CertificateManagerPCA.Types.Tag
import Network.AWS.CertificateManagerPCA.Types.Validity
import Network.AWS.CertificateManagerPCA.Types.ValidityPeriodType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
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
      | Prelude.otherwise = Prelude.Nothing

-- | You can associate up to 50 tags with a private CA. Exception information
-- is contained in the exception message field.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The state of the private CA does not allow this action to occur.
_InvalidStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidStateException =
  Core._MatchServiceError
    defaultService
    "InvalidStateException"

-- | One or more of the specified arguments was not valid.
_InvalidArgsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArgsException =
  Core._MatchServiceError
    defaultService
    "InvalidArgsException"

-- | The resource policy is invalid or is missing a required statement. For
-- general information about IAM policy and statement structure, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#access_policies-json Overview of JSON Policies>.
_InvalidPolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPolicyException =
  Core._MatchServiceError
    defaultService
    "InvalidPolicyException"

-- | The requested Amazon Resource Name (ARN) does not refer to an existing
-- resource.
_InvalidArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArnException =
  Core._MatchServiceError
    defaultService
    "InvalidArnException"

-- | One or more fields in the certificate are invalid.
_MalformedCertificateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MalformedCertificateException =
  Core._MatchServiceError
    defaultService
    "MalformedCertificateException"

-- | Your request has already been completed.
_RequestAlreadyProcessedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RequestAlreadyProcessedException =
  Core._MatchServiceError
    defaultService
    "RequestAlreadyProcessedException"

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

-- | The designated permission has already been given to the user.
_PermissionAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PermissionAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "PermissionAlreadyExistsException"

-- | The request action cannot be performed or is prohibited.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | The tag associated with the CA is not valid. The invalid argument is
-- contained in the message field.
_InvalidTagException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTagException =
  Core._MatchServiceError
    defaultService
    "InvalidTagException"

-- | Your request is already in progress.
_RequestInProgressException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RequestInProgressException =
  Core._MatchServiceError
    defaultService
    "RequestInProgressException"

-- | An ACM Private CA quota has been exceeded. See the exception message
-- returned to determine the quota that was exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The certificate authority certificate you are importing does not comply
-- with conditions specified in the certificate that signed it.
_CertificateMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CertificateMismatchException =
  Core._MatchServiceError
    defaultService
    "CertificateMismatchException"

-- | A resource such as a private CA, S3 bucket, certificate, audit report,
-- or policy cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The request has failed for an unspecified reason.
_RequestFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RequestFailedException =
  Core._MatchServiceError
    defaultService
    "RequestFailedException"

-- | The current action was prevented because it would lock the caller out
-- from performing subsequent actions. Verify that the specified parameters
-- would not result in the caller being denied access to the resource.
_LockoutPreventedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LockoutPreventedException =
  Core._MatchServiceError
    defaultService
    "LockoutPreventedException"

-- | The certificate signing request is invalid.
_MalformedCSRException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MalformedCSRException =
  Core._MatchServiceError
    defaultService
    "MalformedCSRException"

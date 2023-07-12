{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CertificateManagerPCA.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Lens
  ( -- * Operations

    -- ** CreateCertificateAuthority
    createCertificateAuthority_idempotencyToken,
    createCertificateAuthority_keyStorageSecurityStandard,
    createCertificateAuthority_revocationConfiguration,
    createCertificateAuthority_tags,
    createCertificateAuthority_usageMode,
    createCertificateAuthority_certificateAuthorityConfiguration,
    createCertificateAuthority_certificateAuthorityType,
    createCertificateAuthorityResponse_certificateAuthorityArn,
    createCertificateAuthorityResponse_httpStatus,

    -- ** CreateCertificateAuthorityAuditReport
    createCertificateAuthorityAuditReport_certificateAuthorityArn,
    createCertificateAuthorityAuditReport_s3BucketName,
    createCertificateAuthorityAuditReport_auditReportResponseFormat,
    createCertificateAuthorityAuditReportResponse_auditReportId,
    createCertificateAuthorityAuditReportResponse_s3Key,
    createCertificateAuthorityAuditReportResponse_httpStatus,

    -- ** CreatePermission
    createPermission_sourceAccount,
    createPermission_certificateAuthorityArn,
    createPermission_principal,
    createPermission_actions,

    -- ** DeleteCertificateAuthority
    deleteCertificateAuthority_permanentDeletionTimeInDays,
    deleteCertificateAuthority_certificateAuthorityArn,

    -- ** DeletePermission
    deletePermission_sourceAccount,
    deletePermission_certificateAuthorityArn,
    deletePermission_principal,

    -- ** DeletePolicy
    deletePolicy_resourceArn,

    -- ** DescribeCertificateAuthority
    describeCertificateAuthority_certificateAuthorityArn,
    describeCertificateAuthorityResponse_certificateAuthority,
    describeCertificateAuthorityResponse_httpStatus,

    -- ** DescribeCertificateAuthorityAuditReport
    describeCertificateAuthorityAuditReport_certificateAuthorityArn,
    describeCertificateAuthorityAuditReport_auditReportId,
    describeCertificateAuthorityAuditReportResponse_auditReportStatus,
    describeCertificateAuthorityAuditReportResponse_createdAt,
    describeCertificateAuthorityAuditReportResponse_s3BucketName,
    describeCertificateAuthorityAuditReportResponse_s3Key,
    describeCertificateAuthorityAuditReportResponse_httpStatus,

    -- ** GetCertificate
    getCertificate_certificateAuthorityArn,
    getCertificate_certificateArn,
    getCertificateResponse_certificate,
    getCertificateResponse_certificateChain,
    getCertificateResponse_httpStatus,

    -- ** GetCertificateAuthorityCertificate
    getCertificateAuthorityCertificate_certificateAuthorityArn,
    getCertificateAuthorityCertificateResponse_certificate,
    getCertificateAuthorityCertificateResponse_certificateChain,
    getCertificateAuthorityCertificateResponse_httpStatus,

    -- ** GetCertificateAuthorityCsr
    getCertificateAuthorityCsr_certificateAuthorityArn,
    getCertificateAuthorityCsrResponse_csr,
    getCertificateAuthorityCsrResponse_httpStatus,

    -- ** GetPolicy
    getPolicy_resourceArn,
    getPolicyResponse_policy,
    getPolicyResponse_httpStatus,

    -- ** ImportCertificateAuthorityCertificate
    importCertificateAuthorityCertificate_certificateChain,
    importCertificateAuthorityCertificate_certificateAuthorityArn,
    importCertificateAuthorityCertificate_certificate,

    -- ** IssueCertificate
    issueCertificate_apiPassthrough,
    issueCertificate_idempotencyToken,
    issueCertificate_templateArn,
    issueCertificate_validityNotBefore,
    issueCertificate_certificateAuthorityArn,
    issueCertificate_csr,
    issueCertificate_signingAlgorithm,
    issueCertificate_validity,
    issueCertificateResponse_certificateArn,
    issueCertificateResponse_httpStatus,

    -- ** ListCertificateAuthorities
    listCertificateAuthorities_maxResults,
    listCertificateAuthorities_nextToken,
    listCertificateAuthorities_resourceOwner,
    listCertificateAuthoritiesResponse_certificateAuthorities,
    listCertificateAuthoritiesResponse_nextToken,
    listCertificateAuthoritiesResponse_httpStatus,

    -- ** ListPermissions
    listPermissions_maxResults,
    listPermissions_nextToken,
    listPermissions_certificateAuthorityArn,
    listPermissionsResponse_nextToken,
    listPermissionsResponse_permissions,
    listPermissionsResponse_httpStatus,

    -- ** ListTags
    listTags_maxResults,
    listTags_nextToken,
    listTags_certificateAuthorityArn,
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** PutPolicy
    putPolicy_resourceArn,
    putPolicy_policy,

    -- ** RestoreCertificateAuthority
    restoreCertificateAuthority_certificateAuthorityArn,

    -- ** RevokeCertificate
    revokeCertificate_certificateAuthorityArn,
    revokeCertificate_certificateSerial,
    revokeCertificate_revocationReason,

    -- ** TagCertificateAuthority
    tagCertificateAuthority_certificateAuthorityArn,
    tagCertificateAuthority_tags,

    -- ** UntagCertificateAuthority
    untagCertificateAuthority_certificateAuthorityArn,
    untagCertificateAuthority_tags,

    -- ** UpdateCertificateAuthority
    updateCertificateAuthority_revocationConfiguration,
    updateCertificateAuthority_status,
    updateCertificateAuthority_certificateAuthorityArn,

    -- * Types

    -- ** ASN1Subject
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

    -- ** AccessDescription
    accessDescription_accessMethod,
    accessDescription_accessLocation,

    -- ** AccessMethod
    accessMethod_accessMethodType,
    accessMethod_customObjectIdentifier,

    -- ** ApiPassthrough
    apiPassthrough_extensions,
    apiPassthrough_subject,

    -- ** CertificateAuthority
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

    -- ** CertificateAuthorityConfiguration
    certificateAuthorityConfiguration_csrExtensions,
    certificateAuthorityConfiguration_keyAlgorithm,
    certificateAuthorityConfiguration_signingAlgorithm,
    certificateAuthorityConfiguration_subject,

    -- ** CrlConfiguration
    crlConfiguration_customCname,
    crlConfiguration_expirationInDays,
    crlConfiguration_s3BucketName,
    crlConfiguration_s3ObjectAcl,
    crlConfiguration_enabled,

    -- ** CsrExtensions
    csrExtensions_keyUsage,
    csrExtensions_subjectInformationAccess,

    -- ** CustomAttribute
    customAttribute_objectIdentifier,
    customAttribute_value,

    -- ** CustomExtension
    customExtension_critical,
    customExtension_objectIdentifier,
    customExtension_value,

    -- ** EdiPartyName
    ediPartyName_nameAssigner,
    ediPartyName_partyName,

    -- ** ExtendedKeyUsage
    extendedKeyUsage_extendedKeyUsageObjectIdentifier,
    extendedKeyUsage_extendedKeyUsageType,

    -- ** Extensions
    extensions_certificatePolicies,
    extensions_customExtensions,
    extensions_extendedKeyUsage,
    extensions_keyUsage,
    extensions_subjectAlternativeNames,

    -- ** GeneralName
    generalName_directoryName,
    generalName_dnsName,
    generalName_ediPartyName,
    generalName_ipAddress,
    generalName_otherName,
    generalName_registeredId,
    generalName_rfc822Name,
    generalName_uniformResourceIdentifier,

    -- ** KeyUsage
    keyUsage_cRLSign,
    keyUsage_dataEncipherment,
    keyUsage_decipherOnly,
    keyUsage_digitalSignature,
    keyUsage_encipherOnly,
    keyUsage_keyAgreement,
    keyUsage_keyCertSign,
    keyUsage_keyEncipherment,
    keyUsage_nonRepudiation,

    -- ** OcspConfiguration
    ocspConfiguration_ocspCustomCname,
    ocspConfiguration_enabled,

    -- ** OtherName
    otherName_typeId,
    otherName_value,

    -- ** Permission
    permission_actions,
    permission_certificateAuthorityArn,
    permission_createdAt,
    permission_policy,
    permission_principal,
    permission_sourceAccount,

    -- ** PolicyInformation
    policyInformation_policyQualifiers,
    policyInformation_certPolicyId,

    -- ** PolicyQualifierInfo
    policyQualifierInfo_policyQualifierId,
    policyQualifierInfo_qualifier,

    -- ** Qualifier
    qualifier_cpsUri,

    -- ** RevocationConfiguration
    revocationConfiguration_crlConfiguration,
    revocationConfiguration_ocspConfiguration,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** Validity
    validity_value,
    validity_type,
  )
where

import Amazonka.CertificateManagerPCA.CreateCertificateAuthority
import Amazonka.CertificateManagerPCA.CreateCertificateAuthorityAuditReport
import Amazonka.CertificateManagerPCA.CreatePermission
import Amazonka.CertificateManagerPCA.DeleteCertificateAuthority
import Amazonka.CertificateManagerPCA.DeletePermission
import Amazonka.CertificateManagerPCA.DeletePolicy
import Amazonka.CertificateManagerPCA.DescribeCertificateAuthority
import Amazonka.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport
import Amazonka.CertificateManagerPCA.GetCertificate
import Amazonka.CertificateManagerPCA.GetCertificateAuthorityCertificate
import Amazonka.CertificateManagerPCA.GetCertificateAuthorityCsr
import Amazonka.CertificateManagerPCA.GetPolicy
import Amazonka.CertificateManagerPCA.ImportCertificateAuthorityCertificate
import Amazonka.CertificateManagerPCA.IssueCertificate
import Amazonka.CertificateManagerPCA.ListCertificateAuthorities
import Amazonka.CertificateManagerPCA.ListPermissions
import Amazonka.CertificateManagerPCA.ListTags
import Amazonka.CertificateManagerPCA.PutPolicy
import Amazonka.CertificateManagerPCA.RestoreCertificateAuthority
import Amazonka.CertificateManagerPCA.RevokeCertificate
import Amazonka.CertificateManagerPCA.TagCertificateAuthority
import Amazonka.CertificateManagerPCA.Types.ASN1Subject
import Amazonka.CertificateManagerPCA.Types.AccessDescription
import Amazonka.CertificateManagerPCA.Types.AccessMethod
import Amazonka.CertificateManagerPCA.Types.ApiPassthrough
import Amazonka.CertificateManagerPCA.Types.CertificateAuthority
import Amazonka.CertificateManagerPCA.Types.CertificateAuthorityConfiguration
import Amazonka.CertificateManagerPCA.Types.CrlConfiguration
import Amazonka.CertificateManagerPCA.Types.CsrExtensions
import Amazonka.CertificateManagerPCA.Types.CustomAttribute
import Amazonka.CertificateManagerPCA.Types.CustomExtension
import Amazonka.CertificateManagerPCA.Types.EdiPartyName
import Amazonka.CertificateManagerPCA.Types.ExtendedKeyUsage
import Amazonka.CertificateManagerPCA.Types.Extensions
import Amazonka.CertificateManagerPCA.Types.GeneralName
import Amazonka.CertificateManagerPCA.Types.KeyUsage
import Amazonka.CertificateManagerPCA.Types.OcspConfiguration
import Amazonka.CertificateManagerPCA.Types.OtherName
import Amazonka.CertificateManagerPCA.Types.Permission
import Amazonka.CertificateManagerPCA.Types.PolicyInformation
import Amazonka.CertificateManagerPCA.Types.PolicyQualifierInfo
import Amazonka.CertificateManagerPCA.Types.Qualifier
import Amazonka.CertificateManagerPCA.Types.RevocationConfiguration
import Amazonka.CertificateManagerPCA.Types.Tag
import Amazonka.CertificateManagerPCA.Types.Validity
import Amazonka.CertificateManagerPCA.UntagCertificateAuthority
import Amazonka.CertificateManagerPCA.UpdateCertificateAuthority

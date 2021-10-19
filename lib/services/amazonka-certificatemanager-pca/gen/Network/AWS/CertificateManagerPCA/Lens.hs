{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Lens
  ( -- * Operations

    -- ** ImportCertificateAuthorityCertificate
    importCertificateAuthorityCertificate_certificateChain,
    importCertificateAuthorityCertificate_certificateAuthorityArn,
    importCertificateAuthorityCertificate_certificate,

    -- ** CreatePermission
    createPermission_sourceAccount,
    createPermission_certificateAuthorityArn,
    createPermission_principal,
    createPermission_actions,

    -- ** DescribeCertificateAuthorityAuditReport
    describeCertificateAuthorityAuditReport_certificateAuthorityArn,
    describeCertificateAuthorityAuditReport_auditReportId,
    describeCertificateAuthorityAuditReportResponse_s3Key,
    describeCertificateAuthorityAuditReportResponse_createdAt,
    describeCertificateAuthorityAuditReportResponse_auditReportStatus,
    describeCertificateAuthorityAuditReportResponse_s3BucketName,
    describeCertificateAuthorityAuditReportResponse_httpStatus,

    -- ** DeletePermission
    deletePermission_sourceAccount,
    deletePermission_certificateAuthorityArn,
    deletePermission_principal,

    -- ** RevokeCertificate
    revokeCertificate_certificateAuthorityArn,
    revokeCertificate_certificateSerial,
    revokeCertificate_revocationReason,

    -- ** UpdateCertificateAuthority
    updateCertificateAuthority_status,
    updateCertificateAuthority_revocationConfiguration,
    updateCertificateAuthority_certificateAuthorityArn,

    -- ** DeleteCertificateAuthority
    deleteCertificateAuthority_permanentDeletionTimeInDays,
    deleteCertificateAuthority_certificateAuthorityArn,

    -- ** GetCertificateAuthorityCsr
    getCertificateAuthorityCsr_certificateAuthorityArn,
    getCertificateAuthorityCsrResponse_csr,
    getCertificateAuthorityCsrResponse_httpStatus,

    -- ** CreateCertificateAuthority
    createCertificateAuthority_idempotencyToken,
    createCertificateAuthority_keyStorageSecurityStandard,
    createCertificateAuthority_revocationConfiguration,
    createCertificateAuthority_tags,
    createCertificateAuthority_certificateAuthorityConfiguration,
    createCertificateAuthority_certificateAuthorityType,
    createCertificateAuthorityResponse_certificateAuthorityArn,
    createCertificateAuthorityResponse_httpStatus,

    -- ** ListCertificateAuthorities
    listCertificateAuthorities_nextToken,
    listCertificateAuthorities_resourceOwner,
    listCertificateAuthorities_maxResults,
    listCertificateAuthoritiesResponse_certificateAuthorities,
    listCertificateAuthoritiesResponse_nextToken,
    listCertificateAuthoritiesResponse_httpStatus,

    -- ** GetCertificate
    getCertificate_certificateAuthorityArn,
    getCertificate_certificateArn,
    getCertificateResponse_certificate,
    getCertificateResponse_certificateChain,
    getCertificateResponse_httpStatus,

    -- ** TagCertificateAuthority
    tagCertificateAuthority_certificateAuthorityArn,
    tagCertificateAuthority_tags,

    -- ** PutPolicy
    putPolicy_resourceArn,
    putPolicy_policy,

    -- ** DeletePolicy
    deletePolicy_resourceArn,

    -- ** DescribeCertificateAuthority
    describeCertificateAuthority_certificateAuthorityArn,
    describeCertificateAuthorityResponse_certificateAuthority,
    describeCertificateAuthorityResponse_httpStatus,

    -- ** RestoreCertificateAuthority
    restoreCertificateAuthority_certificateAuthorityArn,

    -- ** IssueCertificate
    issueCertificate_idempotencyToken,
    issueCertificate_apiPassthrough,
    issueCertificate_templateArn,
    issueCertificate_validityNotBefore,
    issueCertificate_certificateAuthorityArn,
    issueCertificate_csr,
    issueCertificate_signingAlgorithm,
    issueCertificate_validity,
    issueCertificateResponse_certificateArn,
    issueCertificateResponse_httpStatus,

    -- ** GetCertificateAuthorityCertificate
    getCertificateAuthorityCertificate_certificateAuthorityArn,
    getCertificateAuthorityCertificateResponse_certificate,
    getCertificateAuthorityCertificateResponse_certificateChain,
    getCertificateAuthorityCertificateResponse_httpStatus,

    -- ** ListPermissions
    listPermissions_nextToken,
    listPermissions_maxResults,
    listPermissions_certificateAuthorityArn,
    listPermissionsResponse_nextToken,
    listPermissionsResponse_permissions,
    listPermissionsResponse_httpStatus,

    -- ** UntagCertificateAuthority
    untagCertificateAuthority_certificateAuthorityArn,
    untagCertificateAuthority_tags,

    -- ** CreateCertificateAuthorityAuditReport
    createCertificateAuthorityAuditReport_certificateAuthorityArn,
    createCertificateAuthorityAuditReport_s3BucketName,
    createCertificateAuthorityAuditReport_auditReportResponseFormat,
    createCertificateAuthorityAuditReportResponse_s3Key,
    createCertificateAuthorityAuditReportResponse_auditReportId,
    createCertificateAuthorityAuditReportResponse_httpStatus,

    -- ** ListTags
    listTags_nextToken,
    listTags_maxResults,
    listTags_certificateAuthorityArn,
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** GetPolicy
    getPolicy_resourceArn,
    getPolicyResponse_policy,
    getPolicyResponse_httpStatus,

    -- * Types

    -- ** ASN1Subject
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

    -- ** AccessDescription
    accessDescription_accessMethod,
    accessDescription_accessLocation,

    -- ** AccessMethod
    accessMethod_accessMethodType,
    accessMethod_customObjectIdentifier,

    -- ** ApiPassthrough
    apiPassthrough_subject,
    apiPassthrough_extensions,

    -- ** CertificateAuthority
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

    -- ** CertificateAuthorityConfiguration
    certificateAuthorityConfiguration_csrExtensions,
    certificateAuthorityConfiguration_keyAlgorithm,
    certificateAuthorityConfiguration_signingAlgorithm,
    certificateAuthorityConfiguration_subject,

    -- ** CrlConfiguration
    crlConfiguration_customCname,
    crlConfiguration_expirationInDays,
    crlConfiguration_s3ObjectAcl,
    crlConfiguration_s3BucketName,
    crlConfiguration_enabled,

    -- ** CsrExtensions
    csrExtensions_subjectInformationAccess,
    csrExtensions_keyUsage,

    -- ** EdiPartyName
    ediPartyName_nameAssigner,
    ediPartyName_partyName,

    -- ** ExtendedKeyUsage
    extendedKeyUsage_extendedKeyUsageType,
    extendedKeyUsage_extendedKeyUsageObjectIdentifier,

    -- ** Extensions
    extensions_subjectAlternativeNames,
    extensions_keyUsage,
    extensions_extendedKeyUsage,
    extensions_certificatePolicies,

    -- ** GeneralName
    generalName_ipAddress,
    generalName_uniformResourceIdentifier,
    generalName_registeredId,
    generalName_ediPartyName,
    generalName_rfc822Name,
    generalName_otherName,
    generalName_dnsName,
    generalName_directoryName,

    -- ** KeyUsage
    keyUsage_dataEncipherment,
    keyUsage_encipherOnly,
    keyUsage_nonRepudiation,
    keyUsage_cRLSign,
    keyUsage_digitalSignature,
    keyUsage_keyCertSign,
    keyUsage_decipherOnly,
    keyUsage_keyEncipherment,
    keyUsage_keyAgreement,

    -- ** OcspConfiguration
    ocspConfiguration_ocspCustomCname,
    ocspConfiguration_enabled,

    -- ** OtherName
    otherName_typeId,
    otherName_value,

    -- ** Permission
    permission_sourceAccount,
    permission_actions,
    permission_createdAt,
    permission_principal,
    permission_policy,
    permission_certificateAuthorityArn,

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

import Network.AWS.CertificateManagerPCA.CreateCertificateAuthority
import Network.AWS.CertificateManagerPCA.CreateCertificateAuthorityAuditReport
import Network.AWS.CertificateManagerPCA.CreatePermission
import Network.AWS.CertificateManagerPCA.DeleteCertificateAuthority
import Network.AWS.CertificateManagerPCA.DeletePermission
import Network.AWS.CertificateManagerPCA.DeletePolicy
import Network.AWS.CertificateManagerPCA.DescribeCertificateAuthority
import Network.AWS.CertificateManagerPCA.DescribeCertificateAuthorityAuditReport
import Network.AWS.CertificateManagerPCA.GetCertificate
import Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCertificate
import Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCsr
import Network.AWS.CertificateManagerPCA.GetPolicy
import Network.AWS.CertificateManagerPCA.ImportCertificateAuthorityCertificate
import Network.AWS.CertificateManagerPCA.IssueCertificate
import Network.AWS.CertificateManagerPCA.ListCertificateAuthorities
import Network.AWS.CertificateManagerPCA.ListPermissions
import Network.AWS.CertificateManagerPCA.ListTags
import Network.AWS.CertificateManagerPCA.PutPolicy
import Network.AWS.CertificateManagerPCA.RestoreCertificateAuthority
import Network.AWS.CertificateManagerPCA.RevokeCertificate
import Network.AWS.CertificateManagerPCA.TagCertificateAuthority
import Network.AWS.CertificateManagerPCA.Types.ASN1Subject
import Network.AWS.CertificateManagerPCA.Types.AccessDescription
import Network.AWS.CertificateManagerPCA.Types.AccessMethod
import Network.AWS.CertificateManagerPCA.Types.ApiPassthrough
import Network.AWS.CertificateManagerPCA.Types.CertificateAuthority
import Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityConfiguration
import Network.AWS.CertificateManagerPCA.Types.CrlConfiguration
import Network.AWS.CertificateManagerPCA.Types.CsrExtensions
import Network.AWS.CertificateManagerPCA.Types.EdiPartyName
import Network.AWS.CertificateManagerPCA.Types.ExtendedKeyUsage
import Network.AWS.CertificateManagerPCA.Types.Extensions
import Network.AWS.CertificateManagerPCA.Types.GeneralName
import Network.AWS.CertificateManagerPCA.Types.KeyUsage
import Network.AWS.CertificateManagerPCA.Types.OcspConfiguration
import Network.AWS.CertificateManagerPCA.Types.OtherName
import Network.AWS.CertificateManagerPCA.Types.Permission
import Network.AWS.CertificateManagerPCA.Types.PolicyInformation
import Network.AWS.CertificateManagerPCA.Types.PolicyQualifierInfo
import Network.AWS.CertificateManagerPCA.Types.Qualifier
import Network.AWS.CertificateManagerPCA.Types.RevocationConfiguration
import Network.AWS.CertificateManagerPCA.Types.Tag
import Network.AWS.CertificateManagerPCA.Types.Validity
import Network.AWS.CertificateManagerPCA.UntagCertificateAuthority
import Network.AWS.CertificateManagerPCA.UpdateCertificateAuthority

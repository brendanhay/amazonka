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

    -- ** CreatePermission
    createPermission_sourceAccount,
    createPermission_certificateAuthorityArn,
    createPermission_principal,
    createPermission_actions,

    -- ** RestoreCertificateAuthority
    restoreCertificateAuthority_certificateAuthorityArn,

    -- ** DeletePolicy
    deletePolicy_resourceArn,

    -- ** DescribeCertificateAuthority
    describeCertificateAuthority_certificateAuthorityArn,
    describeCertificateAuthorityResponse_certificateAuthority,
    describeCertificateAuthorityResponse_httpStatus,

    -- ** TagCertificateAuthority
    tagCertificateAuthority_certificateAuthorityArn,
    tagCertificateAuthority_tags,

    -- ** CreateCertificateAuthorityAuditReport
    createCertificateAuthorityAuditReport_certificateAuthorityArn,
    createCertificateAuthorityAuditReport_s3BucketName,
    createCertificateAuthorityAuditReport_auditReportResponseFormat,
    createCertificateAuthorityAuditReportResponse_s3Key,
    createCertificateAuthorityAuditReportResponse_auditReportId,
    createCertificateAuthorityAuditReportResponse_httpStatus,

    -- ** GetCertificate
    getCertificate_certificateAuthorityArn,
    getCertificate_certificateArn,
    getCertificateResponse_certificateChain,
    getCertificateResponse_certificate,
    getCertificateResponse_httpStatus,

    -- ** CreateCertificateAuthority
    createCertificateAuthority_idempotencyToken,
    createCertificateAuthority_revocationConfiguration,
    createCertificateAuthority_tags,
    createCertificateAuthority_certificateAuthorityConfiguration,
    createCertificateAuthority_certificateAuthorityType,
    createCertificateAuthorityResponse_certificateAuthorityArn,
    createCertificateAuthorityResponse_httpStatus,

    -- ** GetCertificateAuthorityCsr
    getCertificateAuthorityCsr_certificateAuthorityArn,
    getCertificateAuthorityCsrResponse_csr,
    getCertificateAuthorityCsrResponse_httpStatus,

    -- ** ListCertificateAuthorities
    listCertificateAuthorities_nextToken,
    listCertificateAuthorities_maxResults,
    listCertificateAuthorities_resourceOwner,
    listCertificateAuthoritiesResponse_nextToken,
    listCertificateAuthoritiesResponse_certificateAuthorities,
    listCertificateAuthoritiesResponse_httpStatus,

    -- ** RevokeCertificate
    revokeCertificate_certificateAuthorityArn,
    revokeCertificate_certificateSerial,
    revokeCertificate_revocationReason,

    -- ** DeletePermission
    deletePermission_sourceAccount,
    deletePermission_certificateAuthorityArn,
    deletePermission_principal,

    -- ** ListPermissions
    listPermissions_nextToken,
    listPermissions_maxResults,
    listPermissions_certificateAuthorityArn,
    listPermissionsResponse_nextToken,
    listPermissionsResponse_permissions,
    listPermissionsResponse_httpStatus,

    -- ** GetCertificateAuthorityCertificate
    getCertificateAuthorityCertificate_certificateAuthorityArn,
    getCertificateAuthorityCertificateResponse_certificateChain,
    getCertificateAuthorityCertificateResponse_certificate,
    getCertificateAuthorityCertificateResponse_httpStatus,

    -- ** IssueCertificate
    issueCertificate_idempotencyToken,
    issueCertificate_validityNotBefore,
    issueCertificate_templateArn,
    issueCertificate_apiPassthrough,
    issueCertificate_certificateAuthorityArn,
    issueCertificate_csr,
    issueCertificate_signingAlgorithm,
    issueCertificate_validity,
    issueCertificateResponse_certificateArn,
    issueCertificateResponse_httpStatus,

    -- ** ImportCertificateAuthorityCertificate
    importCertificateAuthorityCertificate_certificateChain,
    importCertificateAuthorityCertificate_certificateAuthorityArn,
    importCertificateAuthorityCertificate_certificate,

    -- ** PutPolicy
    putPolicy_resourceArn,
    putPolicy_policy,

    -- ** GetPolicy
    getPolicy_resourceArn,
    getPolicyResponse_policy,
    getPolicyResponse_httpStatus,

    -- ** ListTags
    listTags_nextToken,
    listTags_maxResults,
    listTags_certificateAuthorityArn,
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** DeleteCertificateAuthority
    deleteCertificateAuthority_permanentDeletionTimeInDays,
    deleteCertificateAuthority_certificateAuthorityArn,

    -- ** UpdateCertificateAuthority
    updateCertificateAuthority_status,
    updateCertificateAuthority_revocationConfiguration,
    updateCertificateAuthority_certificateAuthorityArn,

    -- ** UntagCertificateAuthority
    untagCertificateAuthority_certificateAuthorityArn,
    untagCertificateAuthority_tags,

    -- ** DescribeCertificateAuthorityAuditReport
    describeCertificateAuthorityAuditReport_certificateAuthorityArn,
    describeCertificateAuthorityAuditReport_auditReportId,
    describeCertificateAuthorityAuditReportResponse_auditReportStatus,
    describeCertificateAuthorityAuditReportResponse_createdAt,
    describeCertificateAuthorityAuditReportResponse_s3Key,
    describeCertificateAuthorityAuditReportResponse_s3BucketName,
    describeCertificateAuthorityAuditReportResponse_httpStatus,

    -- * Types

    -- ** ASN1Subject
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

    -- ** CertificateAuthorityConfiguration
    certificateAuthorityConfiguration_csrExtensions,
    certificateAuthorityConfiguration_keyAlgorithm,
    certificateAuthorityConfiguration_signingAlgorithm,
    certificateAuthorityConfiguration_subject,

    -- ** CrlConfiguration
    crlConfiguration_customCname,
    crlConfiguration_s3BucketName,
    crlConfiguration_expirationInDays,
    crlConfiguration_enabled,

    -- ** CsrExtensions
    csrExtensions_subjectInformationAccess,
    csrExtensions_keyUsage,

    -- ** EdiPartyName
    ediPartyName_nameAssigner,
    ediPartyName_partyName,

    -- ** ExtendedKeyUsage
    extendedKeyUsage_extendedKeyUsageObjectIdentifier,
    extendedKeyUsage_extendedKeyUsageType,

    -- ** Extensions
    extensions_certificatePolicies,
    extensions_extendedKeyUsage,
    extensions_subjectAlternativeNames,
    extensions_keyUsage,

    -- ** GeneralName
    generalName_ediPartyName,
    generalName_otherName,
    generalName_uniformResourceIdentifier,
    generalName_ipAddress,
    generalName_dnsName,
    generalName_directoryName,
    generalName_rfc822Name,
    generalName_registeredId,

    -- ** KeyUsage
    keyUsage_dataEncipherment,
    keyUsage_keyCertSign,
    keyUsage_cRLSign,
    keyUsage_keyEncipherment,
    keyUsage_encipherOnly,
    keyUsage_keyAgreement,
    keyUsage_digitalSignature,
    keyUsage_decipherOnly,
    keyUsage_nonRepudiation,

    -- ** OtherName
    otherName_typeId,
    otherName_value,

    -- ** Permission
    permission_certificateAuthorityArn,
    permission_createdAt,
    permission_actions,
    permission_principal,
    permission_sourceAccount,
    permission_policy,

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

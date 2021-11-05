{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CertificateManagerPCA.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Lens
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

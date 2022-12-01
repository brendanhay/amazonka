{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CertificateManager.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManager.Lens
  ( -- * Operations

    -- ** AddTagsToCertificate
    addTagsToCertificate_certificateArn,
    addTagsToCertificate_tags,

    -- ** DeleteCertificate
    deleteCertificate_certificateArn,

    -- ** DescribeCertificate
    describeCertificate_certificateArn,
    describeCertificateResponse_certificate,
    describeCertificateResponse_httpStatus,

    -- ** ExportCertificate
    exportCertificate_certificateArn,
    exportCertificate_passphrase,
    exportCertificateResponse_privateKey,
    exportCertificateResponse_certificate,
    exportCertificateResponse_certificateChain,
    exportCertificateResponse_httpStatus,

    -- ** GetAccountConfiguration
    getAccountConfigurationResponse_expiryEvents,
    getAccountConfigurationResponse_httpStatus,

    -- ** GetCertificate
    getCertificate_certificateArn,
    getCertificateResponse_certificate,
    getCertificateResponse_certificateChain,
    getCertificateResponse_httpStatus,

    -- ** ImportCertificate
    importCertificate_tags,
    importCertificate_certificateArn,
    importCertificate_certificateChain,
    importCertificate_certificate,
    importCertificate_privateKey,
    importCertificateResponse_certificateArn,
    importCertificateResponse_httpStatus,

    -- ** ListCertificates
    listCertificates_sortOrder,
    listCertificates_nextToken,
    listCertificates_maxItems,
    listCertificates_sortBy,
    listCertificates_includes,
    listCertificates_certificateStatuses,
    listCertificatesResponse_nextToken,
    listCertificatesResponse_certificateSummaryList,
    listCertificatesResponse_httpStatus,

    -- ** ListTagsForCertificate
    listTagsForCertificate_certificateArn,
    listTagsForCertificateResponse_tags,
    listTagsForCertificateResponse_httpStatus,

    -- ** PutAccountConfiguration
    putAccountConfiguration_expiryEvents,
    putAccountConfiguration_idempotencyToken,

    -- ** RemoveTagsFromCertificate
    removeTagsFromCertificate_certificateArn,
    removeTagsFromCertificate_tags,

    -- ** RenewCertificate
    renewCertificate_certificateArn,

    -- ** RequestCertificate
    requestCertificate_tags,
    requestCertificate_domainValidationOptions,
    requestCertificate_certificateAuthorityArn,
    requestCertificate_idempotencyToken,
    requestCertificate_keyAlgorithm,
    requestCertificate_options,
    requestCertificate_validationMethod,
    requestCertificate_subjectAlternativeNames,
    requestCertificate_domainName,
    requestCertificateResponse_certificateArn,
    requestCertificateResponse_httpStatus,

    -- ** ResendValidationEmail
    resendValidationEmail_certificateArn,
    resendValidationEmail_domain,
    resendValidationEmail_validationDomain,

    -- ** UpdateCertificateOptions
    updateCertificateOptions_certificateArn,
    updateCertificateOptions_options,

    -- * Types

    -- ** CertificateDetail
    certificateDetail_issuer,
    certificateDetail_domainValidationOptions,
    certificateDetail_type,
    certificateDetail_certificateAuthorityArn,
    certificateDetail_domainName,
    certificateDetail_serial,
    certificateDetail_keyUsages,
    certificateDetail_renewalSummary,
    certificateDetail_keyAlgorithm,
    certificateDetail_extendedKeyUsages,
    certificateDetail_inUseBy,
    certificateDetail_status,
    certificateDetail_options,
    certificateDetail_certificateArn,
    certificateDetail_importedAt,
    certificateDetail_notBefore,
    certificateDetail_revocationReason,
    certificateDetail_signatureAlgorithm,
    certificateDetail_revokedAt,
    certificateDetail_subject,
    certificateDetail_notAfter,
    certificateDetail_renewalEligibility,
    certificateDetail_createdAt,
    certificateDetail_subjectAlternativeNames,
    certificateDetail_failureReason,
    certificateDetail_issuedAt,

    -- ** CertificateOptions
    certificateOptions_certificateTransparencyLoggingPreference,

    -- ** CertificateSummary
    certificateSummary_subjectAlternativeNameSummaries,
    certificateSummary_type,
    certificateSummary_domainName,
    certificateSummary_exported,
    certificateSummary_keyUsages,
    certificateSummary_keyAlgorithm,
    certificateSummary_extendedKeyUsages,
    certificateSummary_status,
    certificateSummary_hasAdditionalSubjectAlternativeNames,
    certificateSummary_certificateArn,
    certificateSummary_importedAt,
    certificateSummary_notBefore,
    certificateSummary_inUse,
    certificateSummary_revokedAt,
    certificateSummary_notAfter,
    certificateSummary_renewalEligibility,
    certificateSummary_createdAt,
    certificateSummary_issuedAt,

    -- ** DomainValidation
    domainValidation_validationStatus,
    domainValidation_validationDomain,
    domainValidation_resourceRecord,
    domainValidation_validationEmails,
    domainValidation_validationMethod,
    domainValidation_domainName,

    -- ** DomainValidationOption
    domainValidationOption_domainName,
    domainValidationOption_validationDomain,

    -- ** ExpiryEventsConfiguration
    expiryEventsConfiguration_daysBeforeExpiry,

    -- ** ExtendedKeyUsage
    extendedKeyUsage_name,
    extendedKeyUsage_oid,

    -- ** Filters
    filters_extendedKeyUsage,
    filters_keyTypes,
    filters_keyUsage,

    -- ** KeyUsage
    keyUsage_name,

    -- ** RenewalSummary
    renewalSummary_renewalStatusReason,
    renewalSummary_renewalStatus,
    renewalSummary_domainValidationOptions,
    renewalSummary_updatedAt,

    -- ** ResourceRecord
    resourceRecord_name,
    resourceRecord_type,
    resourceRecord_value,

    -- ** Tag
    tag_value,
    tag_key,
  )
where

import Amazonka.CertificateManager.AddTagsToCertificate
import Amazonka.CertificateManager.DeleteCertificate
import Amazonka.CertificateManager.DescribeCertificate
import Amazonka.CertificateManager.ExportCertificate
import Amazonka.CertificateManager.GetAccountConfiguration
import Amazonka.CertificateManager.GetCertificate
import Amazonka.CertificateManager.ImportCertificate
import Amazonka.CertificateManager.ListCertificates
import Amazonka.CertificateManager.ListTagsForCertificate
import Amazonka.CertificateManager.PutAccountConfiguration
import Amazonka.CertificateManager.RemoveTagsFromCertificate
import Amazonka.CertificateManager.RenewCertificate
import Amazonka.CertificateManager.RequestCertificate
import Amazonka.CertificateManager.ResendValidationEmail
import Amazonka.CertificateManager.Types.CertificateDetail
import Amazonka.CertificateManager.Types.CertificateOptions
import Amazonka.CertificateManager.Types.CertificateSummary
import Amazonka.CertificateManager.Types.DomainValidation
import Amazonka.CertificateManager.Types.DomainValidationOption
import Amazonka.CertificateManager.Types.ExpiryEventsConfiguration
import Amazonka.CertificateManager.Types.ExtendedKeyUsage
import Amazonka.CertificateManager.Types.Filters
import Amazonka.CertificateManager.Types.KeyUsage
import Amazonka.CertificateManager.Types.RenewalSummary
import Amazonka.CertificateManager.Types.ResourceRecord
import Amazonka.CertificateManager.Types.Tag
import Amazonka.CertificateManager.UpdateCertificateOptions

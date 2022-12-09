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
    exportCertificateResponse_certificate,
    exportCertificateResponse_certificateChain,
    exportCertificateResponse_privateKey,
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
    importCertificate_certificateArn,
    importCertificate_certificateChain,
    importCertificate_tags,
    importCertificate_certificate,
    importCertificate_privateKey,
    importCertificateResponse_certificateArn,
    importCertificateResponse_httpStatus,

    -- ** ListCertificates
    listCertificates_certificateStatuses,
    listCertificates_includes,
    listCertificates_maxItems,
    listCertificates_nextToken,
    listCertificates_sortBy,
    listCertificates_sortOrder,
    listCertificatesResponse_certificateSummaryList,
    listCertificatesResponse_nextToken,
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
    requestCertificate_certificateAuthorityArn,
    requestCertificate_domainValidationOptions,
    requestCertificate_idempotencyToken,
    requestCertificate_keyAlgorithm,
    requestCertificate_options,
    requestCertificate_subjectAlternativeNames,
    requestCertificate_tags,
    requestCertificate_validationMethod,
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

    -- ** CertificateOptions
    certificateOptions_certificateTransparencyLoggingPreference,

    -- ** CertificateSummary
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

    -- ** DomainValidation
    domainValidation_resourceRecord,
    domainValidation_validationDomain,
    domainValidation_validationEmails,
    domainValidation_validationMethod,
    domainValidation_validationStatus,
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

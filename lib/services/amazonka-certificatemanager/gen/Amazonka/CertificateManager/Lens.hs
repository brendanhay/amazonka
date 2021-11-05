{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CertificateManager.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManager.Lens
  ( -- * Operations

    -- ** ResendValidationEmail
    resendValidationEmail_certificateArn,
    resendValidationEmail_domain,
    resendValidationEmail_validationDomain,

    -- ** UpdateCertificateOptions
    updateCertificateOptions_certificateArn,
    updateCertificateOptions_options,

    -- ** ListTagsForCertificate
    listTagsForCertificate_certificateArn,
    listTagsForCertificateResponse_tags,
    listTagsForCertificateResponse_httpStatus,

    -- ** GetCertificate
    getCertificate_certificateArn,
    getCertificateResponse_certificate,
    getCertificateResponse_certificateChain,
    getCertificateResponse_httpStatus,

    -- ** AddTagsToCertificate
    addTagsToCertificate_certificateArn,
    addTagsToCertificate_tags,

    -- ** RequestCertificate
    requestCertificate_idempotencyToken,
    requestCertificate_validationMethod,
    requestCertificate_subjectAlternativeNames,
    requestCertificate_options,
    requestCertificate_domainValidationOptions,
    requestCertificate_certificateAuthorityArn,
    requestCertificate_tags,
    requestCertificate_domainName,
    requestCertificateResponse_certificateArn,
    requestCertificateResponse_httpStatus,

    -- ** ListCertificates
    listCertificates_certificateStatuses,
    listCertificates_nextToken,
    listCertificates_includes,
    listCertificates_maxItems,
    listCertificatesResponse_certificateSummaryList,
    listCertificatesResponse_nextToken,
    listCertificatesResponse_httpStatus,

    -- ** DeleteCertificate
    deleteCertificate_certificateArn,

    -- ** RemoveTagsFromCertificate
    removeTagsFromCertificate_certificateArn,
    removeTagsFromCertificate_tags,

    -- ** GetAccountConfiguration
    getAccountConfigurationResponse_expiryEvents,
    getAccountConfigurationResponse_httpStatus,

    -- ** ImportCertificate
    importCertificate_certificateArn,
    importCertificate_certificateChain,
    importCertificate_tags,
    importCertificate_certificate,
    importCertificate_privateKey,
    importCertificateResponse_certificateArn,
    importCertificateResponse_httpStatus,

    -- ** PutAccountConfiguration
    putAccountConfiguration_expiryEvents,
    putAccountConfiguration_idempotencyToken,

    -- ** DescribeCertificate
    describeCertificate_certificateArn,
    describeCertificateResponse_certificate,
    describeCertificateResponse_httpStatus,

    -- ** RenewCertificate
    renewCertificate_certificateArn,

    -- ** ExportCertificate
    exportCertificate_certificateArn,
    exportCertificate_passphrase,
    exportCertificateResponse_privateKey,
    exportCertificateResponse_certificate,
    exportCertificateResponse_certificateChain,
    exportCertificateResponse_httpStatus,

    -- * Types

    -- ** CertificateDetail
    certificateDetail_subject,
    certificateDetail_status,
    certificateDetail_failureReason,
    certificateDetail_subjectAlternativeNames,
    certificateDetail_inUseBy,
    certificateDetail_createdAt,
    certificateDetail_certificateArn,
    certificateDetail_serial,
    certificateDetail_renewalEligibility,
    certificateDetail_extendedKeyUsages,
    certificateDetail_importedAt,
    certificateDetail_keyUsages,
    certificateDetail_revokedAt,
    certificateDetail_notBefore,
    certificateDetail_revocationReason,
    certificateDetail_domainName,
    certificateDetail_renewalSummary,
    certificateDetail_keyAlgorithm,
    certificateDetail_type,
    certificateDetail_options,
    certificateDetail_issuedAt,
    certificateDetail_signatureAlgorithm,
    certificateDetail_domainValidationOptions,
    certificateDetail_issuer,
    certificateDetail_notAfter,
    certificateDetail_certificateAuthorityArn,

    -- ** CertificateOptions
    certificateOptions_certificateTransparencyLoggingPreference,

    -- ** CertificateSummary
    certificateSummary_certificateArn,
    certificateSummary_domainName,

    -- ** DomainValidation
    domainValidation_validationEmails,
    domainValidation_validationMethod,
    domainValidation_resourceRecord,
    domainValidation_validationStatus,
    domainValidation_validationDomain,
    domainValidation_domainName,

    -- ** DomainValidationOption
    domainValidationOption_domainName,
    domainValidationOption_validationDomain,

    -- ** ExpiryEventsConfiguration
    expiryEventsConfiguration_daysBeforeExpiry,

    -- ** ExtendedKeyUsage
    extendedKeyUsage_oid,
    extendedKeyUsage_name,

    -- ** Filters
    filters_keyTypes,
    filters_keyUsage,
    filters_extendedKeyUsage,

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

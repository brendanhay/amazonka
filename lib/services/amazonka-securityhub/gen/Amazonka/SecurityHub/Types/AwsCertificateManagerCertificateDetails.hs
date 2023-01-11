{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateDomainValidationOption
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateExtendedKeyUsage
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateKeyUsage
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateOptions
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateRenewalSummary

-- | Provides details about an Certificate Manager certificate.
--
-- /See:/ 'newAwsCertificateManagerCertificateDetails' smart constructor.
data AwsCertificateManagerCertificateDetails = AwsCertificateManagerCertificateDetails'
  { -- | The ARN of the private certificate authority (CA) that will be used to
    -- issue the certificate.
    certificateAuthorityArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the certificate was requested.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | The fully qualified domain name (FQDN), such as www.example.com, that is
    -- secured by the certificate.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the initial validation of each domain name
    -- that occurs as a result of the @RequestCertificate@ request.
    --
    -- Only provided if the certificate type is @AMAZON_ISSUED@.
    domainValidationOptions :: Prelude.Maybe [AwsCertificateManagerCertificateDomainValidationOption],
    -- | Contains a list of Extended Key Usage X.509 v3 extension objects. Each
    -- object specifies a purpose for which the certificate public key can be
    -- used and consists of a name and an object identifier (OID).
    extendedKeyUsages :: Prelude.Maybe [AwsCertificateManagerCertificateExtendedKeyUsage],
    -- | For a failed certificate request, the reason for the failure.
    --
    -- Valid values: @NO_AVAILABLE_CONTACTS@ |
    -- @ADDITIONAL_VERIFICATION_REQUIRED@ | @DOMAIN_NOT_ALLOWED@ |
    -- @INVALID_PUBLIC_DOMAIN@ | @DOMAIN_VALIDATION_DENIED@ | @CAA_ERROR@ |
    -- @PCA_LIMIT_EXCEEDED@ | @PCA_INVALID_ARN@ | @PCA_INVALID_STATE@ |
    -- @PCA_REQUEST_FAILED@ | @PCA_NAME_CONSTRAINTS_VALIDATION@ |
    -- @PCA_RESOURCE_NOT_FOUND@ | @PCA_INVALID_ARGS@ | @PCA_INVALID_DURATION@ |
    -- @PCA_ACCESS_DENIED@ | @SLR_NOT_FOUND@ | @OTHER@
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the certificate was imported. Provided if the certificate
    -- type is @IMPORTED@.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    importedAt :: Prelude.Maybe Prelude.Text,
    -- | The list of ARNs for the Amazon Web Services resources that use the
    -- certificate.
    inUseBy :: Prelude.Maybe [Prelude.Text],
    -- | Indicates when the certificate was issued. Provided if the certificate
    -- type is @AMAZON_ISSUED@.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    issuedAt :: Prelude.Maybe Prelude.Text,
    -- | The name of the certificate authority that issued and signed the
    -- certificate.
    issuer :: Prelude.Maybe Prelude.Text,
    -- | The algorithm that was used to generate the public-private key pair.
    --
    -- Valid values: @RSA_2048@ | @RSA_1024@ |@ RSA_4096@ | @EC_prime256v1@ |
    -- @EC_secp384r1@ | @EC_secp521r1@
    keyAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | A list of key usage X.509 v3 extension objects.
    keyUsages :: Prelude.Maybe [AwsCertificateManagerCertificateKeyUsage],
    -- | The time after which the certificate becomes invalid.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    notAfter :: Prelude.Maybe Prelude.Text,
    -- | The time before which the certificate is not valid.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    notBefore :: Prelude.Maybe Prelude.Text,
    -- | Provides a value that specifies whether to add the certificate to a
    -- transparency log.
    options :: Prelude.Maybe AwsCertificateManagerCertificateOptions,
    -- | Whether the certificate is eligible for renewal.
    --
    -- Valid values: @ELIGIBLE@ | @INELIGIBLE@
    renewalEligibility :: Prelude.Maybe Prelude.Text,
    -- | Information about the status of the Certificate Manager managed renewal
    -- for the certificate. Provided only when the certificate type is
    -- @AMAZON_ISSUED@.
    renewalSummary :: Prelude.Maybe AwsCertificateManagerCertificateRenewalSummary,
    -- | The serial number of the certificate.
    serial :: Prelude.Maybe Prelude.Text,
    -- | The algorithm that was used to sign the certificate.
    signatureAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | The status of the certificate.
    --
    -- Valid values: @PENDING_VALIDATION@ | @ISSUED@ | @INACTIVE@ | @EXPIRED@ |
    -- @VALIDATION_TIMED_OUT@ | @REVOKED@ | @FAILED@
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the entity that is associated with the public key contained
    -- in the certificate.
    subject :: Prelude.Maybe Prelude.Text,
    -- | One or more domain names (subject alternative names) included in the
    -- certificate. This list contains the domain names that are bound to the
    -- public key that is contained in the certificate.
    --
    -- The subject alternative names include the canonical domain name (CN) of
    -- the certificate and additional domain names that can be used to connect
    -- to the website.
    subjectAlternativeNames :: Prelude.Maybe [Prelude.Text],
    -- | The source of the certificate. For certificates that Certificate Manager
    -- provides, @Type@ is @AMAZON_ISSUED@. For certificates that are imported
    -- with @ImportCertificate@, @Type@ is @IMPORTED@.
    --
    -- Valid values: @IMPORTED@ | @AMAZON_ISSUED@ | @PRIVATE@
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCertificateManagerCertificateDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityArn', 'awsCertificateManagerCertificateDetails_certificateAuthorityArn' - The ARN of the private certificate authority (CA) that will be used to
-- issue the certificate.
--
-- 'createdAt', 'awsCertificateManagerCertificateDetails_createdAt' - Indicates when the certificate was requested.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'domainName', 'awsCertificateManagerCertificateDetails_domainName' - The fully qualified domain name (FQDN), such as www.example.com, that is
-- secured by the certificate.
--
-- 'domainValidationOptions', 'awsCertificateManagerCertificateDetails_domainValidationOptions' - Contains information about the initial validation of each domain name
-- that occurs as a result of the @RequestCertificate@ request.
--
-- Only provided if the certificate type is @AMAZON_ISSUED@.
--
-- 'extendedKeyUsages', 'awsCertificateManagerCertificateDetails_extendedKeyUsages' - Contains a list of Extended Key Usage X.509 v3 extension objects. Each
-- object specifies a purpose for which the certificate public key can be
-- used and consists of a name and an object identifier (OID).
--
-- 'failureReason', 'awsCertificateManagerCertificateDetails_failureReason' - For a failed certificate request, the reason for the failure.
--
-- Valid values: @NO_AVAILABLE_CONTACTS@ |
-- @ADDITIONAL_VERIFICATION_REQUIRED@ | @DOMAIN_NOT_ALLOWED@ |
-- @INVALID_PUBLIC_DOMAIN@ | @DOMAIN_VALIDATION_DENIED@ | @CAA_ERROR@ |
-- @PCA_LIMIT_EXCEEDED@ | @PCA_INVALID_ARN@ | @PCA_INVALID_STATE@ |
-- @PCA_REQUEST_FAILED@ | @PCA_NAME_CONSTRAINTS_VALIDATION@ |
-- @PCA_RESOURCE_NOT_FOUND@ | @PCA_INVALID_ARGS@ | @PCA_INVALID_DURATION@ |
-- @PCA_ACCESS_DENIED@ | @SLR_NOT_FOUND@ | @OTHER@
--
-- 'importedAt', 'awsCertificateManagerCertificateDetails_importedAt' - Indicates when the certificate was imported. Provided if the certificate
-- type is @IMPORTED@.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'inUseBy', 'awsCertificateManagerCertificateDetails_inUseBy' - The list of ARNs for the Amazon Web Services resources that use the
-- certificate.
--
-- 'issuedAt', 'awsCertificateManagerCertificateDetails_issuedAt' - Indicates when the certificate was issued. Provided if the certificate
-- type is @AMAZON_ISSUED@.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'issuer', 'awsCertificateManagerCertificateDetails_issuer' - The name of the certificate authority that issued and signed the
-- certificate.
--
-- 'keyAlgorithm', 'awsCertificateManagerCertificateDetails_keyAlgorithm' - The algorithm that was used to generate the public-private key pair.
--
-- Valid values: @RSA_2048@ | @RSA_1024@ |@ RSA_4096@ | @EC_prime256v1@ |
-- @EC_secp384r1@ | @EC_secp521r1@
--
-- 'keyUsages', 'awsCertificateManagerCertificateDetails_keyUsages' - A list of key usage X.509 v3 extension objects.
--
-- 'notAfter', 'awsCertificateManagerCertificateDetails_notAfter' - The time after which the certificate becomes invalid.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'notBefore', 'awsCertificateManagerCertificateDetails_notBefore' - The time before which the certificate is not valid.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'options', 'awsCertificateManagerCertificateDetails_options' - Provides a value that specifies whether to add the certificate to a
-- transparency log.
--
-- 'renewalEligibility', 'awsCertificateManagerCertificateDetails_renewalEligibility' - Whether the certificate is eligible for renewal.
--
-- Valid values: @ELIGIBLE@ | @INELIGIBLE@
--
-- 'renewalSummary', 'awsCertificateManagerCertificateDetails_renewalSummary' - Information about the status of the Certificate Manager managed renewal
-- for the certificate. Provided only when the certificate type is
-- @AMAZON_ISSUED@.
--
-- 'serial', 'awsCertificateManagerCertificateDetails_serial' - The serial number of the certificate.
--
-- 'signatureAlgorithm', 'awsCertificateManagerCertificateDetails_signatureAlgorithm' - The algorithm that was used to sign the certificate.
--
-- 'status', 'awsCertificateManagerCertificateDetails_status' - The status of the certificate.
--
-- Valid values: @PENDING_VALIDATION@ | @ISSUED@ | @INACTIVE@ | @EXPIRED@ |
-- @VALIDATION_TIMED_OUT@ | @REVOKED@ | @FAILED@
--
-- 'subject', 'awsCertificateManagerCertificateDetails_subject' - The name of the entity that is associated with the public key contained
-- in the certificate.
--
-- 'subjectAlternativeNames', 'awsCertificateManagerCertificateDetails_subjectAlternativeNames' - One or more domain names (subject alternative names) included in the
-- certificate. This list contains the domain names that are bound to the
-- public key that is contained in the certificate.
--
-- The subject alternative names include the canonical domain name (CN) of
-- the certificate and additional domain names that can be used to connect
-- to the website.
--
-- 'type'', 'awsCertificateManagerCertificateDetails_type' - The source of the certificate. For certificates that Certificate Manager
-- provides, @Type@ is @AMAZON_ISSUED@. For certificates that are imported
-- with @ImportCertificate@, @Type@ is @IMPORTED@.
--
-- Valid values: @IMPORTED@ | @AMAZON_ISSUED@ | @PRIVATE@
newAwsCertificateManagerCertificateDetails ::
  AwsCertificateManagerCertificateDetails
newAwsCertificateManagerCertificateDetails =
  AwsCertificateManagerCertificateDetails'
    { certificateAuthorityArn =
        Prelude.Nothing,
      createdAt = Prelude.Nothing,
      domainName = Prelude.Nothing,
      domainValidationOptions =
        Prelude.Nothing,
      extendedKeyUsages =
        Prelude.Nothing,
      failureReason = Prelude.Nothing,
      importedAt = Prelude.Nothing,
      inUseBy = Prelude.Nothing,
      issuedAt = Prelude.Nothing,
      issuer = Prelude.Nothing,
      keyAlgorithm = Prelude.Nothing,
      keyUsages = Prelude.Nothing,
      notAfter = Prelude.Nothing,
      notBefore = Prelude.Nothing,
      options = Prelude.Nothing,
      renewalEligibility =
        Prelude.Nothing,
      renewalSummary = Prelude.Nothing,
      serial = Prelude.Nothing,
      signatureAlgorithm =
        Prelude.Nothing,
      status = Prelude.Nothing,
      subject = Prelude.Nothing,
      subjectAlternativeNames =
        Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The ARN of the private certificate authority (CA) that will be used to
-- issue the certificate.
awsCertificateManagerCertificateDetails_certificateAuthorityArn :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDetails_certificateAuthorityArn = Lens.lens (\AwsCertificateManagerCertificateDetails' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {certificateAuthorityArn = a} :: AwsCertificateManagerCertificateDetails)

-- | Indicates when the certificate was requested.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsCertificateManagerCertificateDetails_createdAt :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDetails_createdAt = Lens.lens (\AwsCertificateManagerCertificateDetails' {createdAt} -> createdAt) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {createdAt = a} :: AwsCertificateManagerCertificateDetails)

-- | The fully qualified domain name (FQDN), such as www.example.com, that is
-- secured by the certificate.
awsCertificateManagerCertificateDetails_domainName :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDetails_domainName = Lens.lens (\AwsCertificateManagerCertificateDetails' {domainName} -> domainName) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {domainName = a} :: AwsCertificateManagerCertificateDetails)

-- | Contains information about the initial validation of each domain name
-- that occurs as a result of the @RequestCertificate@ request.
--
-- Only provided if the certificate type is @AMAZON_ISSUED@.
awsCertificateManagerCertificateDetails_domainValidationOptions :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe [AwsCertificateManagerCertificateDomainValidationOption])
awsCertificateManagerCertificateDetails_domainValidationOptions = Lens.lens (\AwsCertificateManagerCertificateDetails' {domainValidationOptions} -> domainValidationOptions) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {domainValidationOptions = a} :: AwsCertificateManagerCertificateDetails) Prelude.. Lens.mapping Lens.coerced

-- | Contains a list of Extended Key Usage X.509 v3 extension objects. Each
-- object specifies a purpose for which the certificate public key can be
-- used and consists of a name and an object identifier (OID).
awsCertificateManagerCertificateDetails_extendedKeyUsages :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe [AwsCertificateManagerCertificateExtendedKeyUsage])
awsCertificateManagerCertificateDetails_extendedKeyUsages = Lens.lens (\AwsCertificateManagerCertificateDetails' {extendedKeyUsages} -> extendedKeyUsages) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {extendedKeyUsages = a} :: AwsCertificateManagerCertificateDetails) Prelude.. Lens.mapping Lens.coerced

-- | For a failed certificate request, the reason for the failure.
--
-- Valid values: @NO_AVAILABLE_CONTACTS@ |
-- @ADDITIONAL_VERIFICATION_REQUIRED@ | @DOMAIN_NOT_ALLOWED@ |
-- @INVALID_PUBLIC_DOMAIN@ | @DOMAIN_VALIDATION_DENIED@ | @CAA_ERROR@ |
-- @PCA_LIMIT_EXCEEDED@ | @PCA_INVALID_ARN@ | @PCA_INVALID_STATE@ |
-- @PCA_REQUEST_FAILED@ | @PCA_NAME_CONSTRAINTS_VALIDATION@ |
-- @PCA_RESOURCE_NOT_FOUND@ | @PCA_INVALID_ARGS@ | @PCA_INVALID_DURATION@ |
-- @PCA_ACCESS_DENIED@ | @SLR_NOT_FOUND@ | @OTHER@
awsCertificateManagerCertificateDetails_failureReason :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDetails_failureReason = Lens.lens (\AwsCertificateManagerCertificateDetails' {failureReason} -> failureReason) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {failureReason = a} :: AwsCertificateManagerCertificateDetails)

-- | Indicates when the certificate was imported. Provided if the certificate
-- type is @IMPORTED@.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsCertificateManagerCertificateDetails_importedAt :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDetails_importedAt = Lens.lens (\AwsCertificateManagerCertificateDetails' {importedAt} -> importedAt) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {importedAt = a} :: AwsCertificateManagerCertificateDetails)

-- | The list of ARNs for the Amazon Web Services resources that use the
-- certificate.
awsCertificateManagerCertificateDetails_inUseBy :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe [Prelude.Text])
awsCertificateManagerCertificateDetails_inUseBy = Lens.lens (\AwsCertificateManagerCertificateDetails' {inUseBy} -> inUseBy) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {inUseBy = a} :: AwsCertificateManagerCertificateDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates when the certificate was issued. Provided if the certificate
-- type is @AMAZON_ISSUED@.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsCertificateManagerCertificateDetails_issuedAt :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDetails_issuedAt = Lens.lens (\AwsCertificateManagerCertificateDetails' {issuedAt} -> issuedAt) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {issuedAt = a} :: AwsCertificateManagerCertificateDetails)

-- | The name of the certificate authority that issued and signed the
-- certificate.
awsCertificateManagerCertificateDetails_issuer :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDetails_issuer = Lens.lens (\AwsCertificateManagerCertificateDetails' {issuer} -> issuer) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {issuer = a} :: AwsCertificateManagerCertificateDetails)

-- | The algorithm that was used to generate the public-private key pair.
--
-- Valid values: @RSA_2048@ | @RSA_1024@ |@ RSA_4096@ | @EC_prime256v1@ |
-- @EC_secp384r1@ | @EC_secp521r1@
awsCertificateManagerCertificateDetails_keyAlgorithm :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDetails_keyAlgorithm = Lens.lens (\AwsCertificateManagerCertificateDetails' {keyAlgorithm} -> keyAlgorithm) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {keyAlgorithm = a} :: AwsCertificateManagerCertificateDetails)

-- | A list of key usage X.509 v3 extension objects.
awsCertificateManagerCertificateDetails_keyUsages :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe [AwsCertificateManagerCertificateKeyUsage])
awsCertificateManagerCertificateDetails_keyUsages = Lens.lens (\AwsCertificateManagerCertificateDetails' {keyUsages} -> keyUsages) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {keyUsages = a} :: AwsCertificateManagerCertificateDetails) Prelude.. Lens.mapping Lens.coerced

-- | The time after which the certificate becomes invalid.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsCertificateManagerCertificateDetails_notAfter :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDetails_notAfter = Lens.lens (\AwsCertificateManagerCertificateDetails' {notAfter} -> notAfter) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {notAfter = a} :: AwsCertificateManagerCertificateDetails)

-- | The time before which the certificate is not valid.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsCertificateManagerCertificateDetails_notBefore :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDetails_notBefore = Lens.lens (\AwsCertificateManagerCertificateDetails' {notBefore} -> notBefore) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {notBefore = a} :: AwsCertificateManagerCertificateDetails)

-- | Provides a value that specifies whether to add the certificate to a
-- transparency log.
awsCertificateManagerCertificateDetails_options :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe AwsCertificateManagerCertificateOptions)
awsCertificateManagerCertificateDetails_options = Lens.lens (\AwsCertificateManagerCertificateDetails' {options} -> options) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {options = a} :: AwsCertificateManagerCertificateDetails)

-- | Whether the certificate is eligible for renewal.
--
-- Valid values: @ELIGIBLE@ | @INELIGIBLE@
awsCertificateManagerCertificateDetails_renewalEligibility :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDetails_renewalEligibility = Lens.lens (\AwsCertificateManagerCertificateDetails' {renewalEligibility} -> renewalEligibility) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {renewalEligibility = a} :: AwsCertificateManagerCertificateDetails)

-- | Information about the status of the Certificate Manager managed renewal
-- for the certificate. Provided only when the certificate type is
-- @AMAZON_ISSUED@.
awsCertificateManagerCertificateDetails_renewalSummary :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe AwsCertificateManagerCertificateRenewalSummary)
awsCertificateManagerCertificateDetails_renewalSummary = Lens.lens (\AwsCertificateManagerCertificateDetails' {renewalSummary} -> renewalSummary) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {renewalSummary = a} :: AwsCertificateManagerCertificateDetails)

-- | The serial number of the certificate.
awsCertificateManagerCertificateDetails_serial :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDetails_serial = Lens.lens (\AwsCertificateManagerCertificateDetails' {serial} -> serial) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {serial = a} :: AwsCertificateManagerCertificateDetails)

-- | The algorithm that was used to sign the certificate.
awsCertificateManagerCertificateDetails_signatureAlgorithm :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDetails_signatureAlgorithm = Lens.lens (\AwsCertificateManagerCertificateDetails' {signatureAlgorithm} -> signatureAlgorithm) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {signatureAlgorithm = a} :: AwsCertificateManagerCertificateDetails)

-- | The status of the certificate.
--
-- Valid values: @PENDING_VALIDATION@ | @ISSUED@ | @INACTIVE@ | @EXPIRED@ |
-- @VALIDATION_TIMED_OUT@ | @REVOKED@ | @FAILED@
awsCertificateManagerCertificateDetails_status :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDetails_status = Lens.lens (\AwsCertificateManagerCertificateDetails' {status} -> status) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {status = a} :: AwsCertificateManagerCertificateDetails)

-- | The name of the entity that is associated with the public key contained
-- in the certificate.
awsCertificateManagerCertificateDetails_subject :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDetails_subject = Lens.lens (\AwsCertificateManagerCertificateDetails' {subject} -> subject) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {subject = a} :: AwsCertificateManagerCertificateDetails)

-- | One or more domain names (subject alternative names) included in the
-- certificate. This list contains the domain names that are bound to the
-- public key that is contained in the certificate.
--
-- The subject alternative names include the canonical domain name (CN) of
-- the certificate and additional domain names that can be used to connect
-- to the website.
awsCertificateManagerCertificateDetails_subjectAlternativeNames :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe [Prelude.Text])
awsCertificateManagerCertificateDetails_subjectAlternativeNames = Lens.lens (\AwsCertificateManagerCertificateDetails' {subjectAlternativeNames} -> subjectAlternativeNames) (\s@AwsCertificateManagerCertificateDetails' {} a -> s {subjectAlternativeNames = a} :: AwsCertificateManagerCertificateDetails) Prelude.. Lens.mapping Lens.coerced

-- | The source of the certificate. For certificates that Certificate Manager
-- provides, @Type@ is @AMAZON_ISSUED@. For certificates that are imported
-- with @ImportCertificate@, @Type@ is @IMPORTED@.
--
-- Valid values: @IMPORTED@ | @AMAZON_ISSUED@ | @PRIVATE@
awsCertificateManagerCertificateDetails_type :: Lens.Lens' AwsCertificateManagerCertificateDetails (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDetails_type = Lens.lens (\AwsCertificateManagerCertificateDetails' {type'} -> type') (\s@AwsCertificateManagerCertificateDetails' {} a -> s {type' = a} :: AwsCertificateManagerCertificateDetails)

instance
  Data.FromJSON
    AwsCertificateManagerCertificateDetails
  where
  parseJSON =
    Data.withObject
      "AwsCertificateManagerCertificateDetails"
      ( \x ->
          AwsCertificateManagerCertificateDetails'
            Prelude.<$> (x Data..:? "CertificateAuthorityArn")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "DomainName")
            Prelude.<*> ( x Data..:? "DomainValidationOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "ExtendedKeyUsages"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "ImportedAt")
            Prelude.<*> (x Data..:? "InUseBy" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "IssuedAt")
            Prelude.<*> (x Data..:? "Issuer")
            Prelude.<*> (x Data..:? "KeyAlgorithm")
            Prelude.<*> (x Data..:? "KeyUsages" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "NotAfter")
            Prelude.<*> (x Data..:? "NotBefore")
            Prelude.<*> (x Data..:? "Options")
            Prelude.<*> (x Data..:? "RenewalEligibility")
            Prelude.<*> (x Data..:? "RenewalSummary")
            Prelude.<*> (x Data..:? "Serial")
            Prelude.<*> (x Data..:? "SignatureAlgorithm")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Subject")
            Prelude.<*> ( x Data..:? "SubjectAlternativeNames"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    AwsCertificateManagerCertificateDetails
  where
  hashWithSalt
    _salt
    AwsCertificateManagerCertificateDetails' {..} =
      _salt
        `Prelude.hashWithSalt` certificateAuthorityArn
        `Prelude.hashWithSalt` createdAt
        `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` domainValidationOptions
        `Prelude.hashWithSalt` extendedKeyUsages
        `Prelude.hashWithSalt` failureReason
        `Prelude.hashWithSalt` importedAt
        `Prelude.hashWithSalt` inUseBy
        `Prelude.hashWithSalt` issuedAt
        `Prelude.hashWithSalt` issuer
        `Prelude.hashWithSalt` keyAlgorithm
        `Prelude.hashWithSalt` keyUsages
        `Prelude.hashWithSalt` notAfter
        `Prelude.hashWithSalt` notBefore
        `Prelude.hashWithSalt` options
        `Prelude.hashWithSalt` renewalEligibility
        `Prelude.hashWithSalt` renewalSummary
        `Prelude.hashWithSalt` serial
        `Prelude.hashWithSalt` signatureAlgorithm
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` subject
        `Prelude.hashWithSalt` subjectAlternativeNames
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    AwsCertificateManagerCertificateDetails
  where
  rnf AwsCertificateManagerCertificateDetails' {..} =
    Prelude.rnf certificateAuthorityArn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf domainValidationOptions
      `Prelude.seq` Prelude.rnf extendedKeyUsages
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf importedAt
      `Prelude.seq` Prelude.rnf inUseBy
      `Prelude.seq` Prelude.rnf issuedAt
      `Prelude.seq` Prelude.rnf issuer
      `Prelude.seq` Prelude.rnf keyAlgorithm
      `Prelude.seq` Prelude.rnf keyUsages
      `Prelude.seq` Prelude.rnf notAfter
      `Prelude.seq` Prelude.rnf notBefore
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf renewalEligibility
      `Prelude.seq` Prelude.rnf renewalSummary
      `Prelude.seq` Prelude.rnf serial
      `Prelude.seq` Prelude.rnf signatureAlgorithm
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf subject
      `Prelude.seq` Prelude.rnf
        subjectAlternativeNames
      `Prelude.seq` Prelude.rnf type'

instance
  Data.ToJSON
    AwsCertificateManagerCertificateDetails
  where
  toJSON AwsCertificateManagerCertificateDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CertificateAuthorityArn" Data..=)
              Prelude.<$> certificateAuthorityArn,
            ("CreatedAt" Data..=) Prelude.<$> createdAt,
            ("DomainName" Data..=) Prelude.<$> domainName,
            ("DomainValidationOptions" Data..=)
              Prelude.<$> domainValidationOptions,
            ("ExtendedKeyUsages" Data..=)
              Prelude.<$> extendedKeyUsages,
            ("FailureReason" Data..=) Prelude.<$> failureReason,
            ("ImportedAt" Data..=) Prelude.<$> importedAt,
            ("InUseBy" Data..=) Prelude.<$> inUseBy,
            ("IssuedAt" Data..=) Prelude.<$> issuedAt,
            ("Issuer" Data..=) Prelude.<$> issuer,
            ("KeyAlgorithm" Data..=) Prelude.<$> keyAlgorithm,
            ("KeyUsages" Data..=) Prelude.<$> keyUsages,
            ("NotAfter" Data..=) Prelude.<$> notAfter,
            ("NotBefore" Data..=) Prelude.<$> notBefore,
            ("Options" Data..=) Prelude.<$> options,
            ("RenewalEligibility" Data..=)
              Prelude.<$> renewalEligibility,
            ("RenewalSummary" Data..=)
              Prelude.<$> renewalSummary,
            ("Serial" Data..=) Prelude.<$> serial,
            ("SignatureAlgorithm" Data..=)
              Prelude.<$> signatureAlgorithm,
            ("Status" Data..=) Prelude.<$> status,
            ("Subject" Data..=) Prelude.<$> subject,
            ("SubjectAlternativeNames" Data..=)
              Prelude.<$> subjectAlternativeNames,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )

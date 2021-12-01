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
-- Module      : Amazonka.CertificateManager.Types.CertificateDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManager.Types.CertificateDetail where

import Amazonka.CertificateManager.Types.CertificateOptions
import Amazonka.CertificateManager.Types.CertificateStatus
import Amazonka.CertificateManager.Types.CertificateType
import Amazonka.CertificateManager.Types.DomainValidation
import Amazonka.CertificateManager.Types.ExtendedKeyUsage
import Amazonka.CertificateManager.Types.FailureReason
import Amazonka.CertificateManager.Types.KeyAlgorithm
import Amazonka.CertificateManager.Types.KeyUsage
import Amazonka.CertificateManager.Types.RenewalEligibility
import Amazonka.CertificateManager.Types.RenewalSummary
import Amazonka.CertificateManager.Types.RevocationReason
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains metadata about an ACM certificate. This structure is returned
-- in the response to a DescribeCertificate request.
--
-- /See:/ 'newCertificateDetail' smart constructor.
data CertificateDetail = CertificateDetail'
  { -- | The name of the entity that is associated with the public key contained
    -- in the certificate.
    subject :: Prelude.Maybe Prelude.Text,
    -- | The status of the certificate.
    status :: Prelude.Maybe CertificateStatus,
    -- | The reason the certificate request failed. This value exists only when
    -- the certificate status is @FAILED@. For more information, see
    -- <https://docs.aws.amazon.com/acm/latest/userguide/troubleshooting.html#troubleshooting-failed Certificate Request Failed>
    -- in the /Amazon Web Services Certificate Manager User Guide/.
    failureReason :: Prelude.Maybe FailureReason,
    -- | One or more domain names (subject alternative names) included in the
    -- certificate. This list contains the domain names that are bound to the
    -- public key that is contained in the certificate. The subject alternative
    -- names include the canonical domain name (CN) of the certificate and
    -- additional domain names that can be used to connect to the website.
    subjectAlternativeNames :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of ARNs for the Amazon Web Services resources that are using the
    -- certificate. A certificate can be used by multiple Amazon Web Services
    -- resources.
    inUseBy :: Prelude.Maybe [Prelude.Text],
    -- | The time at which the certificate was requested.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the certificate. For more information
    -- about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The serial number of the certificate.
    serial :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the certificate is eligible for renewal. At this time,
    -- only exported private certificates can be renewed with the
    -- RenewCertificate command.
    renewalEligibility :: Prelude.Maybe RenewalEligibility,
    -- | Contains a list of Extended Key Usage X.509 v3 extension objects. Each
    -- object specifies a purpose for which the certificate public key can be
    -- used and consists of a name and an object identifier (OID).
    extendedKeyUsages :: Prelude.Maybe [ExtendedKeyUsage],
    -- | The date and time at which the certificate was imported. This value
    -- exists only when the certificate type is @IMPORTED@.
    importedAt :: Prelude.Maybe Core.POSIX,
    -- | A list of Key Usage X.509 v3 extension objects. Each object is a string
    -- value that identifies the purpose of the public key contained in the
    -- certificate. Possible extension values include DIGITAL_SIGNATURE,
    -- KEY_ENCHIPHERMENT, NON_REPUDIATION, and more.
    keyUsages :: Prelude.Maybe [KeyUsage],
    -- | The time at which the certificate was revoked. This value exists only
    -- when the certificate status is @REVOKED@.
    revokedAt :: Prelude.Maybe Core.POSIX,
    -- | The time before which the certificate is not valid.
    notBefore :: Prelude.Maybe Core.POSIX,
    -- | The reason the certificate was revoked. This value exists only when the
    -- certificate status is @REVOKED@.
    revocationReason :: Prelude.Maybe RevocationReason,
    -- | The fully qualified domain name for the certificate, such as
    -- www.example.com or example.com.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the status of ACM\'s
    -- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>
    -- for the certificate. This field exists only when the certificate type is
    -- @AMAZON_ISSUED@.
    renewalSummary :: Prelude.Maybe RenewalSummary,
    -- | The algorithm that was used to generate the public-private key pair.
    keyAlgorithm :: Prelude.Maybe KeyAlgorithm,
    -- | The source of the certificate. For certificates provided by ACM, this
    -- value is @AMAZON_ISSUED@. For certificates that you imported with
    -- ImportCertificate, this value is @IMPORTED@. ACM does not provide
    -- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>
    -- for imported certificates. For more information about the differences
    -- between certificates that you import and those that ACM provides, see
    -- <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing Certificates>
    -- in the /Amazon Web Services Certificate Manager User Guide/.
    type' :: Prelude.Maybe CertificateType,
    -- | Value that specifies whether to add the certificate to a transparency
    -- log. Certificate transparency makes it possible to detect SSL
    -- certificates that have been mistakenly or maliciously issued. A browser
    -- might respond to certificate that has not been logged by showing an
    -- error message. The logs are cryptographically secure.
    options :: Prelude.Maybe CertificateOptions,
    -- | The time at which the certificate was issued. This value exists only
    -- when the certificate type is @AMAZON_ISSUED@.
    issuedAt :: Prelude.Maybe Core.POSIX,
    -- | The algorithm that was used to sign the certificate.
    signatureAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the initial validation of each domain name
    -- that occurs as a result of the RequestCertificate request. This field
    -- exists only when the certificate type is @AMAZON_ISSUED@.
    domainValidationOptions :: Prelude.Maybe (Prelude.NonEmpty DomainValidation),
    -- | The name of the certificate authority that issued and signed the
    -- certificate.
    issuer :: Prelude.Maybe Prelude.Text,
    -- | The time after which the certificate is not valid.
    notAfter :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the ACM PCA private certificate
    -- authority (CA) that issued the certificate. This has the following
    -- format:
    --
    -- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012@
    certificateAuthorityArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CertificateDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subject', 'certificateDetail_subject' - The name of the entity that is associated with the public key contained
-- in the certificate.
--
-- 'status', 'certificateDetail_status' - The status of the certificate.
--
-- 'failureReason', 'certificateDetail_failureReason' - The reason the certificate request failed. This value exists only when
-- the certificate status is @FAILED@. For more information, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/troubleshooting.html#troubleshooting-failed Certificate Request Failed>
-- in the /Amazon Web Services Certificate Manager User Guide/.
--
-- 'subjectAlternativeNames', 'certificateDetail_subjectAlternativeNames' - One or more domain names (subject alternative names) included in the
-- certificate. This list contains the domain names that are bound to the
-- public key that is contained in the certificate. The subject alternative
-- names include the canonical domain name (CN) of the certificate and
-- additional domain names that can be used to connect to the website.
--
-- 'inUseBy', 'certificateDetail_inUseBy' - A list of ARNs for the Amazon Web Services resources that are using the
-- certificate. A certificate can be used by multiple Amazon Web Services
-- resources.
--
-- 'createdAt', 'certificateDetail_createdAt' - The time at which the certificate was requested.
--
-- 'certificateArn', 'certificateDetail_certificateArn' - The Amazon Resource Name (ARN) of the certificate. For more information
-- about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'serial', 'certificateDetail_serial' - The serial number of the certificate.
--
-- 'renewalEligibility', 'certificateDetail_renewalEligibility' - Specifies whether the certificate is eligible for renewal. At this time,
-- only exported private certificates can be renewed with the
-- RenewCertificate command.
--
-- 'extendedKeyUsages', 'certificateDetail_extendedKeyUsages' - Contains a list of Extended Key Usage X.509 v3 extension objects. Each
-- object specifies a purpose for which the certificate public key can be
-- used and consists of a name and an object identifier (OID).
--
-- 'importedAt', 'certificateDetail_importedAt' - The date and time at which the certificate was imported. This value
-- exists only when the certificate type is @IMPORTED@.
--
-- 'keyUsages', 'certificateDetail_keyUsages' - A list of Key Usage X.509 v3 extension objects. Each object is a string
-- value that identifies the purpose of the public key contained in the
-- certificate. Possible extension values include DIGITAL_SIGNATURE,
-- KEY_ENCHIPHERMENT, NON_REPUDIATION, and more.
--
-- 'revokedAt', 'certificateDetail_revokedAt' - The time at which the certificate was revoked. This value exists only
-- when the certificate status is @REVOKED@.
--
-- 'notBefore', 'certificateDetail_notBefore' - The time before which the certificate is not valid.
--
-- 'revocationReason', 'certificateDetail_revocationReason' - The reason the certificate was revoked. This value exists only when the
-- certificate status is @REVOKED@.
--
-- 'domainName', 'certificateDetail_domainName' - The fully qualified domain name for the certificate, such as
-- www.example.com or example.com.
--
-- 'renewalSummary', 'certificateDetail_renewalSummary' - Contains information about the status of ACM\'s
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>
-- for the certificate. This field exists only when the certificate type is
-- @AMAZON_ISSUED@.
--
-- 'keyAlgorithm', 'certificateDetail_keyAlgorithm' - The algorithm that was used to generate the public-private key pair.
--
-- 'type'', 'certificateDetail_type' - The source of the certificate. For certificates provided by ACM, this
-- value is @AMAZON_ISSUED@. For certificates that you imported with
-- ImportCertificate, this value is @IMPORTED@. ACM does not provide
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>
-- for imported certificates. For more information about the differences
-- between certificates that you import and those that ACM provides, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing Certificates>
-- in the /Amazon Web Services Certificate Manager User Guide/.
--
-- 'options', 'certificateDetail_options' - Value that specifies whether to add the certificate to a transparency
-- log. Certificate transparency makes it possible to detect SSL
-- certificates that have been mistakenly or maliciously issued. A browser
-- might respond to certificate that has not been logged by showing an
-- error message. The logs are cryptographically secure.
--
-- 'issuedAt', 'certificateDetail_issuedAt' - The time at which the certificate was issued. This value exists only
-- when the certificate type is @AMAZON_ISSUED@.
--
-- 'signatureAlgorithm', 'certificateDetail_signatureAlgorithm' - The algorithm that was used to sign the certificate.
--
-- 'domainValidationOptions', 'certificateDetail_domainValidationOptions' - Contains information about the initial validation of each domain name
-- that occurs as a result of the RequestCertificate request. This field
-- exists only when the certificate type is @AMAZON_ISSUED@.
--
-- 'issuer', 'certificateDetail_issuer' - The name of the certificate authority that issued and signed the
-- certificate.
--
-- 'notAfter', 'certificateDetail_notAfter' - The time after which the certificate is not valid.
--
-- 'certificateAuthorityArn', 'certificateDetail_certificateAuthorityArn' - The Amazon Resource Name (ARN) of the ACM PCA private certificate
-- authority (CA) that issued the certificate. This has the following
-- format:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012@
newCertificateDetail ::
  CertificateDetail
newCertificateDetail =
  CertificateDetail'
    { subject = Prelude.Nothing,
      status = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      subjectAlternativeNames = Prelude.Nothing,
      inUseBy = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      serial = Prelude.Nothing,
      renewalEligibility = Prelude.Nothing,
      extendedKeyUsages = Prelude.Nothing,
      importedAt = Prelude.Nothing,
      keyUsages = Prelude.Nothing,
      revokedAt = Prelude.Nothing,
      notBefore = Prelude.Nothing,
      revocationReason = Prelude.Nothing,
      domainName = Prelude.Nothing,
      renewalSummary = Prelude.Nothing,
      keyAlgorithm = Prelude.Nothing,
      type' = Prelude.Nothing,
      options = Prelude.Nothing,
      issuedAt = Prelude.Nothing,
      signatureAlgorithm = Prelude.Nothing,
      domainValidationOptions = Prelude.Nothing,
      issuer = Prelude.Nothing,
      notAfter = Prelude.Nothing,
      certificateAuthorityArn = Prelude.Nothing
    }

-- | The name of the entity that is associated with the public key contained
-- in the certificate.
certificateDetail_subject :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.Text)
certificateDetail_subject = Lens.lens (\CertificateDetail' {subject} -> subject) (\s@CertificateDetail' {} a -> s {subject = a} :: CertificateDetail)

-- | The status of the certificate.
certificateDetail_status :: Lens.Lens' CertificateDetail (Prelude.Maybe CertificateStatus)
certificateDetail_status = Lens.lens (\CertificateDetail' {status} -> status) (\s@CertificateDetail' {} a -> s {status = a} :: CertificateDetail)

-- | The reason the certificate request failed. This value exists only when
-- the certificate status is @FAILED@. For more information, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/troubleshooting.html#troubleshooting-failed Certificate Request Failed>
-- in the /Amazon Web Services Certificate Manager User Guide/.
certificateDetail_failureReason :: Lens.Lens' CertificateDetail (Prelude.Maybe FailureReason)
certificateDetail_failureReason = Lens.lens (\CertificateDetail' {failureReason} -> failureReason) (\s@CertificateDetail' {} a -> s {failureReason = a} :: CertificateDetail)

-- | One or more domain names (subject alternative names) included in the
-- certificate. This list contains the domain names that are bound to the
-- public key that is contained in the certificate. The subject alternative
-- names include the canonical domain name (CN) of the certificate and
-- additional domain names that can be used to connect to the website.
certificateDetail_subjectAlternativeNames :: Lens.Lens' CertificateDetail (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
certificateDetail_subjectAlternativeNames = Lens.lens (\CertificateDetail' {subjectAlternativeNames} -> subjectAlternativeNames) (\s@CertificateDetail' {} a -> s {subjectAlternativeNames = a} :: CertificateDetail) Prelude.. Lens.mapping Lens.coerced

-- | A list of ARNs for the Amazon Web Services resources that are using the
-- certificate. A certificate can be used by multiple Amazon Web Services
-- resources.
certificateDetail_inUseBy :: Lens.Lens' CertificateDetail (Prelude.Maybe [Prelude.Text])
certificateDetail_inUseBy = Lens.lens (\CertificateDetail' {inUseBy} -> inUseBy) (\s@CertificateDetail' {} a -> s {inUseBy = a} :: CertificateDetail) Prelude.. Lens.mapping Lens.coerced

-- | The time at which the certificate was requested.
certificateDetail_createdAt :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.UTCTime)
certificateDetail_createdAt = Lens.lens (\CertificateDetail' {createdAt} -> createdAt) (\s@CertificateDetail' {} a -> s {createdAt = a} :: CertificateDetail) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the certificate. For more information
-- about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
certificateDetail_certificateArn :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.Text)
certificateDetail_certificateArn = Lens.lens (\CertificateDetail' {certificateArn} -> certificateArn) (\s@CertificateDetail' {} a -> s {certificateArn = a} :: CertificateDetail)

-- | The serial number of the certificate.
certificateDetail_serial :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.Text)
certificateDetail_serial = Lens.lens (\CertificateDetail' {serial} -> serial) (\s@CertificateDetail' {} a -> s {serial = a} :: CertificateDetail)

-- | Specifies whether the certificate is eligible for renewal. At this time,
-- only exported private certificates can be renewed with the
-- RenewCertificate command.
certificateDetail_renewalEligibility :: Lens.Lens' CertificateDetail (Prelude.Maybe RenewalEligibility)
certificateDetail_renewalEligibility = Lens.lens (\CertificateDetail' {renewalEligibility} -> renewalEligibility) (\s@CertificateDetail' {} a -> s {renewalEligibility = a} :: CertificateDetail)

-- | Contains a list of Extended Key Usage X.509 v3 extension objects. Each
-- object specifies a purpose for which the certificate public key can be
-- used and consists of a name and an object identifier (OID).
certificateDetail_extendedKeyUsages :: Lens.Lens' CertificateDetail (Prelude.Maybe [ExtendedKeyUsage])
certificateDetail_extendedKeyUsages = Lens.lens (\CertificateDetail' {extendedKeyUsages} -> extendedKeyUsages) (\s@CertificateDetail' {} a -> s {extendedKeyUsages = a} :: CertificateDetail) Prelude.. Lens.mapping Lens.coerced

-- | The date and time at which the certificate was imported. This value
-- exists only when the certificate type is @IMPORTED@.
certificateDetail_importedAt :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.UTCTime)
certificateDetail_importedAt = Lens.lens (\CertificateDetail' {importedAt} -> importedAt) (\s@CertificateDetail' {} a -> s {importedAt = a} :: CertificateDetail) Prelude.. Lens.mapping Core._Time

-- | A list of Key Usage X.509 v3 extension objects. Each object is a string
-- value that identifies the purpose of the public key contained in the
-- certificate. Possible extension values include DIGITAL_SIGNATURE,
-- KEY_ENCHIPHERMENT, NON_REPUDIATION, and more.
certificateDetail_keyUsages :: Lens.Lens' CertificateDetail (Prelude.Maybe [KeyUsage])
certificateDetail_keyUsages = Lens.lens (\CertificateDetail' {keyUsages} -> keyUsages) (\s@CertificateDetail' {} a -> s {keyUsages = a} :: CertificateDetail) Prelude.. Lens.mapping Lens.coerced

-- | The time at which the certificate was revoked. This value exists only
-- when the certificate status is @REVOKED@.
certificateDetail_revokedAt :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.UTCTime)
certificateDetail_revokedAt = Lens.lens (\CertificateDetail' {revokedAt} -> revokedAt) (\s@CertificateDetail' {} a -> s {revokedAt = a} :: CertificateDetail) Prelude.. Lens.mapping Core._Time

-- | The time before which the certificate is not valid.
certificateDetail_notBefore :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.UTCTime)
certificateDetail_notBefore = Lens.lens (\CertificateDetail' {notBefore} -> notBefore) (\s@CertificateDetail' {} a -> s {notBefore = a} :: CertificateDetail) Prelude.. Lens.mapping Core._Time

-- | The reason the certificate was revoked. This value exists only when the
-- certificate status is @REVOKED@.
certificateDetail_revocationReason :: Lens.Lens' CertificateDetail (Prelude.Maybe RevocationReason)
certificateDetail_revocationReason = Lens.lens (\CertificateDetail' {revocationReason} -> revocationReason) (\s@CertificateDetail' {} a -> s {revocationReason = a} :: CertificateDetail)

-- | The fully qualified domain name for the certificate, such as
-- www.example.com or example.com.
certificateDetail_domainName :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.Text)
certificateDetail_domainName = Lens.lens (\CertificateDetail' {domainName} -> domainName) (\s@CertificateDetail' {} a -> s {domainName = a} :: CertificateDetail)

-- | Contains information about the status of ACM\'s
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>
-- for the certificate. This field exists only when the certificate type is
-- @AMAZON_ISSUED@.
certificateDetail_renewalSummary :: Lens.Lens' CertificateDetail (Prelude.Maybe RenewalSummary)
certificateDetail_renewalSummary = Lens.lens (\CertificateDetail' {renewalSummary} -> renewalSummary) (\s@CertificateDetail' {} a -> s {renewalSummary = a} :: CertificateDetail)

-- | The algorithm that was used to generate the public-private key pair.
certificateDetail_keyAlgorithm :: Lens.Lens' CertificateDetail (Prelude.Maybe KeyAlgorithm)
certificateDetail_keyAlgorithm = Lens.lens (\CertificateDetail' {keyAlgorithm} -> keyAlgorithm) (\s@CertificateDetail' {} a -> s {keyAlgorithm = a} :: CertificateDetail)

-- | The source of the certificate. For certificates provided by ACM, this
-- value is @AMAZON_ISSUED@. For certificates that you imported with
-- ImportCertificate, this value is @IMPORTED@. ACM does not provide
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>
-- for imported certificates. For more information about the differences
-- between certificates that you import and those that ACM provides, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing Certificates>
-- in the /Amazon Web Services Certificate Manager User Guide/.
certificateDetail_type :: Lens.Lens' CertificateDetail (Prelude.Maybe CertificateType)
certificateDetail_type = Lens.lens (\CertificateDetail' {type'} -> type') (\s@CertificateDetail' {} a -> s {type' = a} :: CertificateDetail)

-- | Value that specifies whether to add the certificate to a transparency
-- log. Certificate transparency makes it possible to detect SSL
-- certificates that have been mistakenly or maliciously issued. A browser
-- might respond to certificate that has not been logged by showing an
-- error message. The logs are cryptographically secure.
certificateDetail_options :: Lens.Lens' CertificateDetail (Prelude.Maybe CertificateOptions)
certificateDetail_options = Lens.lens (\CertificateDetail' {options} -> options) (\s@CertificateDetail' {} a -> s {options = a} :: CertificateDetail)

-- | The time at which the certificate was issued. This value exists only
-- when the certificate type is @AMAZON_ISSUED@.
certificateDetail_issuedAt :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.UTCTime)
certificateDetail_issuedAt = Lens.lens (\CertificateDetail' {issuedAt} -> issuedAt) (\s@CertificateDetail' {} a -> s {issuedAt = a} :: CertificateDetail) Prelude.. Lens.mapping Core._Time

-- | The algorithm that was used to sign the certificate.
certificateDetail_signatureAlgorithm :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.Text)
certificateDetail_signatureAlgorithm = Lens.lens (\CertificateDetail' {signatureAlgorithm} -> signatureAlgorithm) (\s@CertificateDetail' {} a -> s {signatureAlgorithm = a} :: CertificateDetail)

-- | Contains information about the initial validation of each domain name
-- that occurs as a result of the RequestCertificate request. This field
-- exists only when the certificate type is @AMAZON_ISSUED@.
certificateDetail_domainValidationOptions :: Lens.Lens' CertificateDetail (Prelude.Maybe (Prelude.NonEmpty DomainValidation))
certificateDetail_domainValidationOptions = Lens.lens (\CertificateDetail' {domainValidationOptions} -> domainValidationOptions) (\s@CertificateDetail' {} a -> s {domainValidationOptions = a} :: CertificateDetail) Prelude.. Lens.mapping Lens.coerced

-- | The name of the certificate authority that issued and signed the
-- certificate.
certificateDetail_issuer :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.Text)
certificateDetail_issuer = Lens.lens (\CertificateDetail' {issuer} -> issuer) (\s@CertificateDetail' {} a -> s {issuer = a} :: CertificateDetail)

-- | The time after which the certificate is not valid.
certificateDetail_notAfter :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.UTCTime)
certificateDetail_notAfter = Lens.lens (\CertificateDetail' {notAfter} -> notAfter) (\s@CertificateDetail' {} a -> s {notAfter = a} :: CertificateDetail) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the ACM PCA private certificate
-- authority (CA) that issued the certificate. This has the following
-- format:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012@
certificateDetail_certificateAuthorityArn :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.Text)
certificateDetail_certificateAuthorityArn = Lens.lens (\CertificateDetail' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@CertificateDetail' {} a -> s {certificateAuthorityArn = a} :: CertificateDetail)

instance Core.FromJSON CertificateDetail where
  parseJSON =
    Core.withObject
      "CertificateDetail"
      ( \x ->
          CertificateDetail'
            Prelude.<$> (x Core..:? "Subject")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..:? "SubjectAlternativeNames")
            Prelude.<*> (x Core..:? "InUseBy" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "CertificateArn")
            Prelude.<*> (x Core..:? "Serial")
            Prelude.<*> (x Core..:? "RenewalEligibility")
            Prelude.<*> ( x Core..:? "ExtendedKeyUsages"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ImportedAt")
            Prelude.<*> (x Core..:? "KeyUsages" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "RevokedAt")
            Prelude.<*> (x Core..:? "NotBefore")
            Prelude.<*> (x Core..:? "RevocationReason")
            Prelude.<*> (x Core..:? "DomainName")
            Prelude.<*> (x Core..:? "RenewalSummary")
            Prelude.<*> (x Core..:? "KeyAlgorithm")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Options")
            Prelude.<*> (x Core..:? "IssuedAt")
            Prelude.<*> (x Core..:? "SignatureAlgorithm")
            Prelude.<*> (x Core..:? "DomainValidationOptions")
            Prelude.<*> (x Core..:? "Issuer")
            Prelude.<*> (x Core..:? "NotAfter")
            Prelude.<*> (x Core..:? "CertificateAuthorityArn")
      )

instance Prelude.Hashable CertificateDetail where
  hashWithSalt salt' CertificateDetail' {..} =
    salt'
      `Prelude.hashWithSalt` certificateAuthorityArn
      `Prelude.hashWithSalt` notAfter
      `Prelude.hashWithSalt` issuer
      `Prelude.hashWithSalt` domainValidationOptions
      `Prelude.hashWithSalt` signatureAlgorithm
      `Prelude.hashWithSalt` issuedAt
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` keyAlgorithm
      `Prelude.hashWithSalt` renewalSummary
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` revocationReason
      `Prelude.hashWithSalt` notBefore
      `Prelude.hashWithSalt` revokedAt
      `Prelude.hashWithSalt` keyUsages
      `Prelude.hashWithSalt` importedAt
      `Prelude.hashWithSalt` extendedKeyUsages
      `Prelude.hashWithSalt` renewalEligibility
      `Prelude.hashWithSalt` serial
      `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` inUseBy
      `Prelude.hashWithSalt` subjectAlternativeNames
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` subject

instance Prelude.NFData CertificateDetail where
  rnf CertificateDetail' {..} =
    Prelude.rnf subject
      `Prelude.seq` Prelude.rnf certificateAuthorityArn
      `Prelude.seq` Prelude.rnf notAfter
      `Prelude.seq` Prelude.rnf issuer
      `Prelude.seq` Prelude.rnf domainValidationOptions
      `Prelude.seq` Prelude.rnf signatureAlgorithm
      `Prelude.seq` Prelude.rnf issuedAt
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf keyAlgorithm
      `Prelude.seq` Prelude.rnf renewalSummary
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf revocationReason
      `Prelude.seq` Prelude.rnf notBefore
      `Prelude.seq` Prelude.rnf revokedAt
      `Prelude.seq` Prelude.rnf keyUsages
      `Prelude.seq` Prelude.rnf importedAt
      `Prelude.seq` Prelude.rnf extendedKeyUsages
      `Prelude.seq` Prelude.rnf renewalEligibility
      `Prelude.seq` Prelude.rnf serial
      `Prelude.seq` Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf inUseBy
      `Prelude.seq` Prelude.rnf subjectAlternativeNames
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf status

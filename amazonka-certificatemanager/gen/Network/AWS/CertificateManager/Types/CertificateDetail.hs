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
-- Module      : Network.AWS.CertificateManager.Types.CertificateDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.CertificateDetail where

import Network.AWS.CertificateManager.Types.CertificateOptions
import Network.AWS.CertificateManager.Types.CertificateStatus
import Network.AWS.CertificateManager.Types.CertificateType
import Network.AWS.CertificateManager.Types.DomainValidation
import Network.AWS.CertificateManager.Types.ExtendedKeyUsage
import Network.AWS.CertificateManager.Types.FailureReason
import Network.AWS.CertificateManager.Types.KeyAlgorithm
import Network.AWS.CertificateManager.Types.KeyUsage
import Network.AWS.CertificateManager.Types.RenewalEligibility
import Network.AWS.CertificateManager.Types.RenewalSummary
import Network.AWS.CertificateManager.Types.RevocationReason
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains metadata about an ACM certificate. This structure is returned
-- in the response to a DescribeCertificate request.
--
-- /See:/ 'newCertificateDetail' smart constructor.
data CertificateDetail = CertificateDetail'
  { -- | The status of the certificate.
    status :: Prelude.Maybe CertificateStatus,
    -- | The time before which the certificate is not valid.
    notBefore :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the ACM PCA private certificate
    -- authority (CA) that issued the certificate. This has the following
    -- format:
    --
    -- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012@
    certificateAuthorityArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which the certificate was imported. This value
    -- exists only when the certificate type is @IMPORTED@.
    importedAt :: Prelude.Maybe Core.POSIX,
    -- | Contains a list of Extended Key Usage X.509 v3 extension objects. Each
    -- object specifies a purpose for which the certificate public key can be
    -- used and consists of a name and an object identifier (OID).
    extendedKeyUsages :: Prelude.Maybe [ExtendedKeyUsage],
    -- | Contains information about the initial validation of each domain name
    -- that occurs as a result of the RequestCertificate request. This field
    -- exists only when the certificate type is @AMAZON_ISSUED@.
    domainValidationOptions :: Prelude.Maybe (Prelude.NonEmpty DomainValidation),
    -- | Specifies whether the certificate is eligible for renewal. At this time,
    -- only exported private certificates can be renewed with the
    -- RenewCertificate command.
    renewalEligibility :: Prelude.Maybe RenewalEligibility,
    -- | Value that specifies whether to add the certificate to a transparency
    -- log. Certificate transparency makes it possible to detect SSL
    -- certificates that have been mistakenly or maliciously issued. A browser
    -- might respond to certificate that has not been logged by showing an
    -- error message. The logs are cryptographically secure.
    options :: Prelude.Maybe CertificateOptions,
    -- | The serial number of the certificate.
    serial :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the certificate. For more information
    -- about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the certificate was requested.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | A list of ARNs for the Amazon Web Services resources that are using the
    -- certificate. A certificate can be used by multiple Amazon Web Services
    -- resources.
    inUseBy :: Prelude.Maybe [Prelude.Text],
    -- | The fully qualified domain name for the certificate, such as
    -- www.example.com or example.com.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | One or more domain names (subject alternative names) included in the
    -- certificate. This list contains the domain names that are bound to the
    -- public key that is contained in the certificate. The subject alternative
    -- names include the canonical domain name (CN) of the certificate and
    -- additional domain names that can be used to connect to the website.
    subjectAlternativeNames :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The reason the certificate was revoked. This value exists only when the
    -- certificate status is @REVOKED@.
    revocationReason :: Prelude.Maybe RevocationReason,
    -- | The reason the certificate request failed. This value exists only when
    -- the certificate status is @FAILED@. For more information, see
    -- <https://docs.aws.amazon.com/acm/latest/userguide/troubleshooting.html#troubleshooting-failed Certificate Request Failed>
    -- in the /Amazon Web Services Certificate Manager User Guide/.
    failureReason :: Prelude.Maybe FailureReason,
    -- | The name of the entity that is associated with the public key contained
    -- in the certificate.
    subject :: Prelude.Maybe Prelude.Text,
    -- | The time after which the certificate is not valid.
    notAfter :: Prelude.Maybe Core.POSIX,
    -- | The time at which the certificate was revoked. This value exists only
    -- when the certificate status is @REVOKED@.
    revokedAt :: Prelude.Maybe Core.POSIX,
    -- | A list of Key Usage X.509 v3 extension objects. Each object is a string
    -- value that identifies the purpose of the public key contained in the
    -- certificate. Possible extension values include DIGITAL_SIGNATURE,
    -- KEY_ENCHIPHERMENT, NON_REPUDIATION, and more.
    keyUsages :: Prelude.Maybe [KeyUsage],
    -- | The name of the certificate authority that issued and signed the
    -- certificate.
    issuer :: Prelude.Maybe Prelude.Text,
    -- | The algorithm that was used to sign the certificate.
    signatureAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | The time at which the certificate was issued. This value exists only
    -- when the certificate type is @AMAZON_ISSUED@.
    issuedAt :: Prelude.Maybe Core.POSIX,
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
    -- | Contains information about the status of ACM\'s
    -- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>
    -- for the certificate. This field exists only when the certificate type is
    -- @AMAZON_ISSUED@.
    renewalSummary :: Prelude.Maybe RenewalSummary
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
-- 'status', 'certificateDetail_status' - The status of the certificate.
--
-- 'notBefore', 'certificateDetail_notBefore' - The time before which the certificate is not valid.
--
-- 'certificateAuthorityArn', 'certificateDetail_certificateAuthorityArn' - The Amazon Resource Name (ARN) of the ACM PCA private certificate
-- authority (CA) that issued the certificate. This has the following
-- format:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012@
--
-- 'importedAt', 'certificateDetail_importedAt' - The date and time at which the certificate was imported. This value
-- exists only when the certificate type is @IMPORTED@.
--
-- 'extendedKeyUsages', 'certificateDetail_extendedKeyUsages' - Contains a list of Extended Key Usage X.509 v3 extension objects. Each
-- object specifies a purpose for which the certificate public key can be
-- used and consists of a name and an object identifier (OID).
--
-- 'domainValidationOptions', 'certificateDetail_domainValidationOptions' - Contains information about the initial validation of each domain name
-- that occurs as a result of the RequestCertificate request. This field
-- exists only when the certificate type is @AMAZON_ISSUED@.
--
-- 'renewalEligibility', 'certificateDetail_renewalEligibility' - Specifies whether the certificate is eligible for renewal. At this time,
-- only exported private certificates can be renewed with the
-- RenewCertificate command.
--
-- 'options', 'certificateDetail_options' - Value that specifies whether to add the certificate to a transparency
-- log. Certificate transparency makes it possible to detect SSL
-- certificates that have been mistakenly or maliciously issued. A browser
-- might respond to certificate that has not been logged by showing an
-- error message. The logs are cryptographically secure.
--
-- 'serial', 'certificateDetail_serial' - The serial number of the certificate.
--
-- 'certificateArn', 'certificateDetail_certificateArn' - The Amazon Resource Name (ARN) of the certificate. For more information
-- about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
--
-- 'createdAt', 'certificateDetail_createdAt' - The time at which the certificate was requested.
--
-- 'inUseBy', 'certificateDetail_inUseBy' - A list of ARNs for the Amazon Web Services resources that are using the
-- certificate. A certificate can be used by multiple Amazon Web Services
-- resources.
--
-- 'domainName', 'certificateDetail_domainName' - The fully qualified domain name for the certificate, such as
-- www.example.com or example.com.
--
-- 'subjectAlternativeNames', 'certificateDetail_subjectAlternativeNames' - One or more domain names (subject alternative names) included in the
-- certificate. This list contains the domain names that are bound to the
-- public key that is contained in the certificate. The subject alternative
-- names include the canonical domain name (CN) of the certificate and
-- additional domain names that can be used to connect to the website.
--
-- 'revocationReason', 'certificateDetail_revocationReason' - The reason the certificate was revoked. This value exists only when the
-- certificate status is @REVOKED@.
--
-- 'failureReason', 'certificateDetail_failureReason' - The reason the certificate request failed. This value exists only when
-- the certificate status is @FAILED@. For more information, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/troubleshooting.html#troubleshooting-failed Certificate Request Failed>
-- in the /Amazon Web Services Certificate Manager User Guide/.
--
-- 'subject', 'certificateDetail_subject' - The name of the entity that is associated with the public key contained
-- in the certificate.
--
-- 'notAfter', 'certificateDetail_notAfter' - The time after which the certificate is not valid.
--
-- 'revokedAt', 'certificateDetail_revokedAt' - The time at which the certificate was revoked. This value exists only
-- when the certificate status is @REVOKED@.
--
-- 'keyUsages', 'certificateDetail_keyUsages' - A list of Key Usage X.509 v3 extension objects. Each object is a string
-- value that identifies the purpose of the public key contained in the
-- certificate. Possible extension values include DIGITAL_SIGNATURE,
-- KEY_ENCHIPHERMENT, NON_REPUDIATION, and more.
--
-- 'issuer', 'certificateDetail_issuer' - The name of the certificate authority that issued and signed the
-- certificate.
--
-- 'signatureAlgorithm', 'certificateDetail_signatureAlgorithm' - The algorithm that was used to sign the certificate.
--
-- 'issuedAt', 'certificateDetail_issuedAt' - The time at which the certificate was issued. This value exists only
-- when the certificate type is @AMAZON_ISSUED@.
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
-- 'renewalSummary', 'certificateDetail_renewalSummary' - Contains information about the status of ACM\'s
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>
-- for the certificate. This field exists only when the certificate type is
-- @AMAZON_ISSUED@.
newCertificateDetail ::
  CertificateDetail
newCertificateDetail =
  CertificateDetail'
    { status = Prelude.Nothing,
      notBefore = Prelude.Nothing,
      certificateAuthorityArn = Prelude.Nothing,
      importedAt = Prelude.Nothing,
      extendedKeyUsages = Prelude.Nothing,
      domainValidationOptions = Prelude.Nothing,
      renewalEligibility = Prelude.Nothing,
      options = Prelude.Nothing,
      serial = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      inUseBy = Prelude.Nothing,
      domainName = Prelude.Nothing,
      subjectAlternativeNames = Prelude.Nothing,
      revocationReason = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      subject = Prelude.Nothing,
      notAfter = Prelude.Nothing,
      revokedAt = Prelude.Nothing,
      keyUsages = Prelude.Nothing,
      issuer = Prelude.Nothing,
      signatureAlgorithm = Prelude.Nothing,
      issuedAt = Prelude.Nothing,
      keyAlgorithm = Prelude.Nothing,
      type' = Prelude.Nothing,
      renewalSummary = Prelude.Nothing
    }

-- | The status of the certificate.
certificateDetail_status :: Lens.Lens' CertificateDetail (Prelude.Maybe CertificateStatus)
certificateDetail_status = Lens.lens (\CertificateDetail' {status} -> status) (\s@CertificateDetail' {} a -> s {status = a} :: CertificateDetail)

-- | The time before which the certificate is not valid.
certificateDetail_notBefore :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.UTCTime)
certificateDetail_notBefore = Lens.lens (\CertificateDetail' {notBefore} -> notBefore) (\s@CertificateDetail' {} a -> s {notBefore = a} :: CertificateDetail) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the ACM PCA private certificate
-- authority (CA) that issued the certificate. This has the following
-- format:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012@
certificateDetail_certificateAuthorityArn :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.Text)
certificateDetail_certificateAuthorityArn = Lens.lens (\CertificateDetail' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@CertificateDetail' {} a -> s {certificateAuthorityArn = a} :: CertificateDetail)

-- | The date and time at which the certificate was imported. This value
-- exists only when the certificate type is @IMPORTED@.
certificateDetail_importedAt :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.UTCTime)
certificateDetail_importedAt = Lens.lens (\CertificateDetail' {importedAt} -> importedAt) (\s@CertificateDetail' {} a -> s {importedAt = a} :: CertificateDetail) Prelude.. Lens.mapping Core._Time

-- | Contains a list of Extended Key Usage X.509 v3 extension objects. Each
-- object specifies a purpose for which the certificate public key can be
-- used and consists of a name and an object identifier (OID).
certificateDetail_extendedKeyUsages :: Lens.Lens' CertificateDetail (Prelude.Maybe [ExtendedKeyUsage])
certificateDetail_extendedKeyUsages = Lens.lens (\CertificateDetail' {extendedKeyUsages} -> extendedKeyUsages) (\s@CertificateDetail' {} a -> s {extendedKeyUsages = a} :: CertificateDetail) Prelude.. Lens.mapping Lens._Coerce

-- | Contains information about the initial validation of each domain name
-- that occurs as a result of the RequestCertificate request. This field
-- exists only when the certificate type is @AMAZON_ISSUED@.
certificateDetail_domainValidationOptions :: Lens.Lens' CertificateDetail (Prelude.Maybe (Prelude.NonEmpty DomainValidation))
certificateDetail_domainValidationOptions = Lens.lens (\CertificateDetail' {domainValidationOptions} -> domainValidationOptions) (\s@CertificateDetail' {} a -> s {domainValidationOptions = a} :: CertificateDetail) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies whether the certificate is eligible for renewal. At this time,
-- only exported private certificates can be renewed with the
-- RenewCertificate command.
certificateDetail_renewalEligibility :: Lens.Lens' CertificateDetail (Prelude.Maybe RenewalEligibility)
certificateDetail_renewalEligibility = Lens.lens (\CertificateDetail' {renewalEligibility} -> renewalEligibility) (\s@CertificateDetail' {} a -> s {renewalEligibility = a} :: CertificateDetail)

-- | Value that specifies whether to add the certificate to a transparency
-- log. Certificate transparency makes it possible to detect SSL
-- certificates that have been mistakenly or maliciously issued. A browser
-- might respond to certificate that has not been logged by showing an
-- error message. The logs are cryptographically secure.
certificateDetail_options :: Lens.Lens' CertificateDetail (Prelude.Maybe CertificateOptions)
certificateDetail_options = Lens.lens (\CertificateDetail' {options} -> options) (\s@CertificateDetail' {} a -> s {options = a} :: CertificateDetail)

-- | The serial number of the certificate.
certificateDetail_serial :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.Text)
certificateDetail_serial = Lens.lens (\CertificateDetail' {serial} -> serial) (\s@CertificateDetail' {} a -> s {serial = a} :: CertificateDetail)

-- | The Amazon Resource Name (ARN) of the certificate. For more information
-- about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
certificateDetail_certificateArn :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.Text)
certificateDetail_certificateArn = Lens.lens (\CertificateDetail' {certificateArn} -> certificateArn) (\s@CertificateDetail' {} a -> s {certificateArn = a} :: CertificateDetail)

-- | The time at which the certificate was requested.
certificateDetail_createdAt :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.UTCTime)
certificateDetail_createdAt = Lens.lens (\CertificateDetail' {createdAt} -> createdAt) (\s@CertificateDetail' {} a -> s {createdAt = a} :: CertificateDetail) Prelude.. Lens.mapping Core._Time

-- | A list of ARNs for the Amazon Web Services resources that are using the
-- certificate. A certificate can be used by multiple Amazon Web Services
-- resources.
certificateDetail_inUseBy :: Lens.Lens' CertificateDetail (Prelude.Maybe [Prelude.Text])
certificateDetail_inUseBy = Lens.lens (\CertificateDetail' {inUseBy} -> inUseBy) (\s@CertificateDetail' {} a -> s {inUseBy = a} :: CertificateDetail) Prelude.. Lens.mapping Lens._Coerce

-- | The fully qualified domain name for the certificate, such as
-- www.example.com or example.com.
certificateDetail_domainName :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.Text)
certificateDetail_domainName = Lens.lens (\CertificateDetail' {domainName} -> domainName) (\s@CertificateDetail' {} a -> s {domainName = a} :: CertificateDetail)

-- | One or more domain names (subject alternative names) included in the
-- certificate. This list contains the domain names that are bound to the
-- public key that is contained in the certificate. The subject alternative
-- names include the canonical domain name (CN) of the certificate and
-- additional domain names that can be used to connect to the website.
certificateDetail_subjectAlternativeNames :: Lens.Lens' CertificateDetail (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
certificateDetail_subjectAlternativeNames = Lens.lens (\CertificateDetail' {subjectAlternativeNames} -> subjectAlternativeNames) (\s@CertificateDetail' {} a -> s {subjectAlternativeNames = a} :: CertificateDetail) Prelude.. Lens.mapping Lens._Coerce

-- | The reason the certificate was revoked. This value exists only when the
-- certificate status is @REVOKED@.
certificateDetail_revocationReason :: Lens.Lens' CertificateDetail (Prelude.Maybe RevocationReason)
certificateDetail_revocationReason = Lens.lens (\CertificateDetail' {revocationReason} -> revocationReason) (\s@CertificateDetail' {} a -> s {revocationReason = a} :: CertificateDetail)

-- | The reason the certificate request failed. This value exists only when
-- the certificate status is @FAILED@. For more information, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/troubleshooting.html#troubleshooting-failed Certificate Request Failed>
-- in the /Amazon Web Services Certificate Manager User Guide/.
certificateDetail_failureReason :: Lens.Lens' CertificateDetail (Prelude.Maybe FailureReason)
certificateDetail_failureReason = Lens.lens (\CertificateDetail' {failureReason} -> failureReason) (\s@CertificateDetail' {} a -> s {failureReason = a} :: CertificateDetail)

-- | The name of the entity that is associated with the public key contained
-- in the certificate.
certificateDetail_subject :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.Text)
certificateDetail_subject = Lens.lens (\CertificateDetail' {subject} -> subject) (\s@CertificateDetail' {} a -> s {subject = a} :: CertificateDetail)

-- | The time after which the certificate is not valid.
certificateDetail_notAfter :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.UTCTime)
certificateDetail_notAfter = Lens.lens (\CertificateDetail' {notAfter} -> notAfter) (\s@CertificateDetail' {} a -> s {notAfter = a} :: CertificateDetail) Prelude.. Lens.mapping Core._Time

-- | The time at which the certificate was revoked. This value exists only
-- when the certificate status is @REVOKED@.
certificateDetail_revokedAt :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.UTCTime)
certificateDetail_revokedAt = Lens.lens (\CertificateDetail' {revokedAt} -> revokedAt) (\s@CertificateDetail' {} a -> s {revokedAt = a} :: CertificateDetail) Prelude.. Lens.mapping Core._Time

-- | A list of Key Usage X.509 v3 extension objects. Each object is a string
-- value that identifies the purpose of the public key contained in the
-- certificate. Possible extension values include DIGITAL_SIGNATURE,
-- KEY_ENCHIPHERMENT, NON_REPUDIATION, and more.
certificateDetail_keyUsages :: Lens.Lens' CertificateDetail (Prelude.Maybe [KeyUsage])
certificateDetail_keyUsages = Lens.lens (\CertificateDetail' {keyUsages} -> keyUsages) (\s@CertificateDetail' {} a -> s {keyUsages = a} :: CertificateDetail) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the certificate authority that issued and signed the
-- certificate.
certificateDetail_issuer :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.Text)
certificateDetail_issuer = Lens.lens (\CertificateDetail' {issuer} -> issuer) (\s@CertificateDetail' {} a -> s {issuer = a} :: CertificateDetail)

-- | The algorithm that was used to sign the certificate.
certificateDetail_signatureAlgorithm :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.Text)
certificateDetail_signatureAlgorithm = Lens.lens (\CertificateDetail' {signatureAlgorithm} -> signatureAlgorithm) (\s@CertificateDetail' {} a -> s {signatureAlgorithm = a} :: CertificateDetail)

-- | The time at which the certificate was issued. This value exists only
-- when the certificate type is @AMAZON_ISSUED@.
certificateDetail_issuedAt :: Lens.Lens' CertificateDetail (Prelude.Maybe Prelude.UTCTime)
certificateDetail_issuedAt = Lens.lens (\CertificateDetail' {issuedAt} -> issuedAt) (\s@CertificateDetail' {} a -> s {issuedAt = a} :: CertificateDetail) Prelude.. Lens.mapping Core._Time

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

-- | Contains information about the status of ACM\'s
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>
-- for the certificate. This field exists only when the certificate type is
-- @AMAZON_ISSUED@.
certificateDetail_renewalSummary :: Lens.Lens' CertificateDetail (Prelude.Maybe RenewalSummary)
certificateDetail_renewalSummary = Lens.lens (\CertificateDetail' {renewalSummary} -> renewalSummary) (\s@CertificateDetail' {} a -> s {renewalSummary = a} :: CertificateDetail)

instance Core.FromJSON CertificateDetail where
  parseJSON =
    Core.withObject
      "CertificateDetail"
      ( \x ->
          CertificateDetail'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "NotBefore")
            Prelude.<*> (x Core..:? "CertificateAuthorityArn")
            Prelude.<*> (x Core..:? "ImportedAt")
            Prelude.<*> ( x Core..:? "ExtendedKeyUsages"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "DomainValidationOptions")
            Prelude.<*> (x Core..:? "RenewalEligibility")
            Prelude.<*> (x Core..:? "Options")
            Prelude.<*> (x Core..:? "Serial")
            Prelude.<*> (x Core..:? "CertificateArn")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "InUseBy" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "DomainName")
            Prelude.<*> (x Core..:? "SubjectAlternativeNames")
            Prelude.<*> (x Core..:? "RevocationReason")
            Prelude.<*> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..:? "Subject")
            Prelude.<*> (x Core..:? "NotAfter")
            Prelude.<*> (x Core..:? "RevokedAt")
            Prelude.<*> (x Core..:? "KeyUsages" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Issuer")
            Prelude.<*> (x Core..:? "SignatureAlgorithm")
            Prelude.<*> (x Core..:? "IssuedAt")
            Prelude.<*> (x Core..:? "KeyAlgorithm")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "RenewalSummary")
      )

instance Prelude.Hashable CertificateDetail

instance Prelude.NFData CertificateDetail

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

-- | Contains metadata about an ACM certificate. This structure is returned
-- in the response to a DescribeCertificate request.
--
-- /See:/ 'newCertificateDetail' smart constructor.
data CertificateDetail = CertificateDetail'
  { -- | The status of the certificate.
    status :: Core.Maybe CertificateStatus,
    -- | The time before which the certificate is not valid.
    notBefore :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the ACM PCA private certificate
    -- authority (CA) that issued the certificate. This has the following
    -- format:
    --
    -- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012@
    certificateAuthorityArn :: Core.Maybe Core.Text,
    -- | The date and time at which the certificate was imported. This value
    -- exists only when the certificate type is @IMPORTED@.
    importedAt :: Core.Maybe Core.POSIX,
    -- | Contains a list of Extended Key Usage X.509 v3 extension objects. Each
    -- object specifies a purpose for which the certificate public key can be
    -- used and consists of a name and an object identifier (OID).
    extendedKeyUsages :: Core.Maybe [ExtendedKeyUsage],
    -- | Contains information about the initial validation of each domain name
    -- that occurs as a result of the RequestCertificate request. This field
    -- exists only when the certificate type is @AMAZON_ISSUED@.
    domainValidationOptions :: Core.Maybe (Core.NonEmpty DomainValidation),
    -- | Specifies whether the certificate is eligible for renewal. At this time,
    -- only exported private certificates can be renewed with the
    -- RenewCertificate command.
    renewalEligibility :: Core.Maybe RenewalEligibility,
    -- | Value that specifies whether to add the certificate to a transparency
    -- log. Certificate transparency makes it possible to detect SSL
    -- certificates that have been mistakenly or maliciously issued. A browser
    -- might respond to certificate that has not been logged by showing an
    -- error message. The logs are cryptographically secure.
    options :: Core.Maybe CertificateOptions,
    -- | The serial number of the certificate.
    serial :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the certificate. For more information
    -- about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    certificateArn :: Core.Maybe Core.Text,
    -- | The time at which the certificate was requested.
    createdAt :: Core.Maybe Core.POSIX,
    -- | A list of ARNs for the AWS resources that are using the certificate. A
    -- certificate can be used by multiple AWS resources.
    inUseBy :: Core.Maybe [Core.Text],
    -- | One or more domain names (subject alternative names) included in the
    -- certificate. This list contains the domain names that are bound to the
    -- public key that is contained in the certificate. The subject alternative
    -- names include the canonical domain name (CN) of the certificate and
    -- additional domain names that can be used to connect to the website.
    subjectAlternativeNames :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The fully qualified domain name for the certificate, such as
    -- www.example.com or example.com.
    domainName :: Core.Maybe Core.Text,
    -- | The reason the certificate was revoked. This value exists only when the
    -- certificate status is @REVOKED@.
    revocationReason :: Core.Maybe RevocationReason,
    -- | The name of the entity that is associated with the public key contained
    -- in the certificate.
    subject :: Core.Maybe Core.Text,
    -- | The reason the certificate request failed. This value exists only when
    -- the certificate status is @FAILED@. For more information, see
    -- <https://docs.aws.amazon.com/acm/latest/userguide/troubleshooting.html#troubleshooting-failed Certificate Request Failed>
    -- in the /AWS Certificate Manager User Guide/.
    failureReason :: Core.Maybe FailureReason,
    -- | A list of Key Usage X.509 v3 extension objects. Each object is a string
    -- value that identifies the purpose of the public key contained in the
    -- certificate. Possible extension values include DIGITAL_SIGNATURE,
    -- KEY_ENCHIPHERMENT, NON_REPUDIATION, and more.
    keyUsages :: Core.Maybe [KeyUsage],
    -- | The time at which the certificate was revoked. This value exists only
    -- when the certificate status is @REVOKED@.
    revokedAt :: Core.Maybe Core.POSIX,
    -- | The time after which the certificate is not valid.
    notAfter :: Core.Maybe Core.POSIX,
    -- | The algorithm that was used to sign the certificate.
    signatureAlgorithm :: Core.Maybe Core.Text,
    -- | The name of the certificate authority that issued and signed the
    -- certificate.
    issuer :: Core.Maybe Core.Text,
    -- | The source of the certificate. For certificates provided by ACM, this
    -- value is @AMAZON_ISSUED@. For certificates that you imported with
    -- ImportCertificate, this value is @IMPORTED@. ACM does not provide
    -- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>
    -- for imported certificates. For more information about the differences
    -- between certificates that you import and those that ACM provides, see
    -- <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing Certificates>
    -- in the /AWS Certificate Manager User Guide/.
    type' :: Core.Maybe CertificateType,
    -- | The algorithm that was used to generate the public-private key pair.
    keyAlgorithm :: Core.Maybe KeyAlgorithm,
    -- | The time at which the certificate was issued. This value exists only
    -- when the certificate type is @AMAZON_ISSUED@.
    issuedAt :: Core.Maybe Core.POSIX,
    -- | Contains information about the status of ACM\'s
    -- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>
    -- for the certificate. This field exists only when the certificate type is
    -- @AMAZON_ISSUED@.
    renewalSummary :: Core.Maybe RenewalSummary
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- in the /AWS General Reference/.
--
-- 'createdAt', 'certificateDetail_createdAt' - The time at which the certificate was requested.
--
-- 'inUseBy', 'certificateDetail_inUseBy' - A list of ARNs for the AWS resources that are using the certificate. A
-- certificate can be used by multiple AWS resources.
--
-- 'subjectAlternativeNames', 'certificateDetail_subjectAlternativeNames' - One or more domain names (subject alternative names) included in the
-- certificate. This list contains the domain names that are bound to the
-- public key that is contained in the certificate. The subject alternative
-- names include the canonical domain name (CN) of the certificate and
-- additional domain names that can be used to connect to the website.
--
-- 'domainName', 'certificateDetail_domainName' - The fully qualified domain name for the certificate, such as
-- www.example.com or example.com.
--
-- 'revocationReason', 'certificateDetail_revocationReason' - The reason the certificate was revoked. This value exists only when the
-- certificate status is @REVOKED@.
--
-- 'subject', 'certificateDetail_subject' - The name of the entity that is associated with the public key contained
-- in the certificate.
--
-- 'failureReason', 'certificateDetail_failureReason' - The reason the certificate request failed. This value exists only when
-- the certificate status is @FAILED@. For more information, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/troubleshooting.html#troubleshooting-failed Certificate Request Failed>
-- in the /AWS Certificate Manager User Guide/.
--
-- 'keyUsages', 'certificateDetail_keyUsages' - A list of Key Usage X.509 v3 extension objects. Each object is a string
-- value that identifies the purpose of the public key contained in the
-- certificate. Possible extension values include DIGITAL_SIGNATURE,
-- KEY_ENCHIPHERMENT, NON_REPUDIATION, and more.
--
-- 'revokedAt', 'certificateDetail_revokedAt' - The time at which the certificate was revoked. This value exists only
-- when the certificate status is @REVOKED@.
--
-- 'notAfter', 'certificateDetail_notAfter' - The time after which the certificate is not valid.
--
-- 'signatureAlgorithm', 'certificateDetail_signatureAlgorithm' - The algorithm that was used to sign the certificate.
--
-- 'issuer', 'certificateDetail_issuer' - The name of the certificate authority that issued and signed the
-- certificate.
--
-- 'type'', 'certificateDetail_type' - The source of the certificate. For certificates provided by ACM, this
-- value is @AMAZON_ISSUED@. For certificates that you imported with
-- ImportCertificate, this value is @IMPORTED@. ACM does not provide
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>
-- for imported certificates. For more information about the differences
-- between certificates that you import and those that ACM provides, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing Certificates>
-- in the /AWS Certificate Manager User Guide/.
--
-- 'keyAlgorithm', 'certificateDetail_keyAlgorithm' - The algorithm that was used to generate the public-private key pair.
--
-- 'issuedAt', 'certificateDetail_issuedAt' - The time at which the certificate was issued. This value exists only
-- when the certificate type is @AMAZON_ISSUED@.
--
-- 'renewalSummary', 'certificateDetail_renewalSummary' - Contains information about the status of ACM\'s
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>
-- for the certificate. This field exists only when the certificate type is
-- @AMAZON_ISSUED@.
newCertificateDetail ::
  CertificateDetail
newCertificateDetail =
  CertificateDetail'
    { status = Core.Nothing,
      notBefore = Core.Nothing,
      certificateAuthorityArn = Core.Nothing,
      importedAt = Core.Nothing,
      extendedKeyUsages = Core.Nothing,
      domainValidationOptions = Core.Nothing,
      renewalEligibility = Core.Nothing,
      options = Core.Nothing,
      serial = Core.Nothing,
      certificateArn = Core.Nothing,
      createdAt = Core.Nothing,
      inUseBy = Core.Nothing,
      subjectAlternativeNames = Core.Nothing,
      domainName = Core.Nothing,
      revocationReason = Core.Nothing,
      subject = Core.Nothing,
      failureReason = Core.Nothing,
      keyUsages = Core.Nothing,
      revokedAt = Core.Nothing,
      notAfter = Core.Nothing,
      signatureAlgorithm = Core.Nothing,
      issuer = Core.Nothing,
      type' = Core.Nothing,
      keyAlgorithm = Core.Nothing,
      issuedAt = Core.Nothing,
      renewalSummary = Core.Nothing
    }

-- | The status of the certificate.
certificateDetail_status :: Lens.Lens' CertificateDetail (Core.Maybe CertificateStatus)
certificateDetail_status = Lens.lens (\CertificateDetail' {status} -> status) (\s@CertificateDetail' {} a -> s {status = a} :: CertificateDetail)

-- | The time before which the certificate is not valid.
certificateDetail_notBefore :: Lens.Lens' CertificateDetail (Core.Maybe Core.UTCTime)
certificateDetail_notBefore = Lens.lens (\CertificateDetail' {notBefore} -> notBefore) (\s@CertificateDetail' {} a -> s {notBefore = a} :: CertificateDetail) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the ACM PCA private certificate
-- authority (CA) that issued the certificate. This has the following
-- format:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012@
certificateDetail_certificateAuthorityArn :: Lens.Lens' CertificateDetail (Core.Maybe Core.Text)
certificateDetail_certificateAuthorityArn = Lens.lens (\CertificateDetail' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@CertificateDetail' {} a -> s {certificateAuthorityArn = a} :: CertificateDetail)

-- | The date and time at which the certificate was imported. This value
-- exists only when the certificate type is @IMPORTED@.
certificateDetail_importedAt :: Lens.Lens' CertificateDetail (Core.Maybe Core.UTCTime)
certificateDetail_importedAt = Lens.lens (\CertificateDetail' {importedAt} -> importedAt) (\s@CertificateDetail' {} a -> s {importedAt = a} :: CertificateDetail) Core.. Lens.mapping Core._Time

-- | Contains a list of Extended Key Usage X.509 v3 extension objects. Each
-- object specifies a purpose for which the certificate public key can be
-- used and consists of a name and an object identifier (OID).
certificateDetail_extendedKeyUsages :: Lens.Lens' CertificateDetail (Core.Maybe [ExtendedKeyUsage])
certificateDetail_extendedKeyUsages = Lens.lens (\CertificateDetail' {extendedKeyUsages} -> extendedKeyUsages) (\s@CertificateDetail' {} a -> s {extendedKeyUsages = a} :: CertificateDetail) Core.. Lens.mapping Lens._Coerce

-- | Contains information about the initial validation of each domain name
-- that occurs as a result of the RequestCertificate request. This field
-- exists only when the certificate type is @AMAZON_ISSUED@.
certificateDetail_domainValidationOptions :: Lens.Lens' CertificateDetail (Core.Maybe (Core.NonEmpty DomainValidation))
certificateDetail_domainValidationOptions = Lens.lens (\CertificateDetail' {domainValidationOptions} -> domainValidationOptions) (\s@CertificateDetail' {} a -> s {domainValidationOptions = a} :: CertificateDetail) Core.. Lens.mapping Lens._Coerce

-- | Specifies whether the certificate is eligible for renewal. At this time,
-- only exported private certificates can be renewed with the
-- RenewCertificate command.
certificateDetail_renewalEligibility :: Lens.Lens' CertificateDetail (Core.Maybe RenewalEligibility)
certificateDetail_renewalEligibility = Lens.lens (\CertificateDetail' {renewalEligibility} -> renewalEligibility) (\s@CertificateDetail' {} a -> s {renewalEligibility = a} :: CertificateDetail)

-- | Value that specifies whether to add the certificate to a transparency
-- log. Certificate transparency makes it possible to detect SSL
-- certificates that have been mistakenly or maliciously issued. A browser
-- might respond to certificate that has not been logged by showing an
-- error message. The logs are cryptographically secure.
certificateDetail_options :: Lens.Lens' CertificateDetail (Core.Maybe CertificateOptions)
certificateDetail_options = Lens.lens (\CertificateDetail' {options} -> options) (\s@CertificateDetail' {} a -> s {options = a} :: CertificateDetail)

-- | The serial number of the certificate.
certificateDetail_serial :: Lens.Lens' CertificateDetail (Core.Maybe Core.Text)
certificateDetail_serial = Lens.lens (\CertificateDetail' {serial} -> serial) (\s@CertificateDetail' {} a -> s {serial = a} :: CertificateDetail)

-- | The Amazon Resource Name (ARN) of the certificate. For more information
-- about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
certificateDetail_certificateArn :: Lens.Lens' CertificateDetail (Core.Maybe Core.Text)
certificateDetail_certificateArn = Lens.lens (\CertificateDetail' {certificateArn} -> certificateArn) (\s@CertificateDetail' {} a -> s {certificateArn = a} :: CertificateDetail)

-- | The time at which the certificate was requested.
certificateDetail_createdAt :: Lens.Lens' CertificateDetail (Core.Maybe Core.UTCTime)
certificateDetail_createdAt = Lens.lens (\CertificateDetail' {createdAt} -> createdAt) (\s@CertificateDetail' {} a -> s {createdAt = a} :: CertificateDetail) Core.. Lens.mapping Core._Time

-- | A list of ARNs for the AWS resources that are using the certificate. A
-- certificate can be used by multiple AWS resources.
certificateDetail_inUseBy :: Lens.Lens' CertificateDetail (Core.Maybe [Core.Text])
certificateDetail_inUseBy = Lens.lens (\CertificateDetail' {inUseBy} -> inUseBy) (\s@CertificateDetail' {} a -> s {inUseBy = a} :: CertificateDetail) Core.. Lens.mapping Lens._Coerce

-- | One or more domain names (subject alternative names) included in the
-- certificate. This list contains the domain names that are bound to the
-- public key that is contained in the certificate. The subject alternative
-- names include the canonical domain name (CN) of the certificate and
-- additional domain names that can be used to connect to the website.
certificateDetail_subjectAlternativeNames :: Lens.Lens' CertificateDetail (Core.Maybe (Core.NonEmpty Core.Text))
certificateDetail_subjectAlternativeNames = Lens.lens (\CertificateDetail' {subjectAlternativeNames} -> subjectAlternativeNames) (\s@CertificateDetail' {} a -> s {subjectAlternativeNames = a} :: CertificateDetail) Core.. Lens.mapping Lens._Coerce

-- | The fully qualified domain name for the certificate, such as
-- www.example.com or example.com.
certificateDetail_domainName :: Lens.Lens' CertificateDetail (Core.Maybe Core.Text)
certificateDetail_domainName = Lens.lens (\CertificateDetail' {domainName} -> domainName) (\s@CertificateDetail' {} a -> s {domainName = a} :: CertificateDetail)

-- | The reason the certificate was revoked. This value exists only when the
-- certificate status is @REVOKED@.
certificateDetail_revocationReason :: Lens.Lens' CertificateDetail (Core.Maybe RevocationReason)
certificateDetail_revocationReason = Lens.lens (\CertificateDetail' {revocationReason} -> revocationReason) (\s@CertificateDetail' {} a -> s {revocationReason = a} :: CertificateDetail)

-- | The name of the entity that is associated with the public key contained
-- in the certificate.
certificateDetail_subject :: Lens.Lens' CertificateDetail (Core.Maybe Core.Text)
certificateDetail_subject = Lens.lens (\CertificateDetail' {subject} -> subject) (\s@CertificateDetail' {} a -> s {subject = a} :: CertificateDetail)

-- | The reason the certificate request failed. This value exists only when
-- the certificate status is @FAILED@. For more information, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/troubleshooting.html#troubleshooting-failed Certificate Request Failed>
-- in the /AWS Certificate Manager User Guide/.
certificateDetail_failureReason :: Lens.Lens' CertificateDetail (Core.Maybe FailureReason)
certificateDetail_failureReason = Lens.lens (\CertificateDetail' {failureReason} -> failureReason) (\s@CertificateDetail' {} a -> s {failureReason = a} :: CertificateDetail)

-- | A list of Key Usage X.509 v3 extension objects. Each object is a string
-- value that identifies the purpose of the public key contained in the
-- certificate. Possible extension values include DIGITAL_SIGNATURE,
-- KEY_ENCHIPHERMENT, NON_REPUDIATION, and more.
certificateDetail_keyUsages :: Lens.Lens' CertificateDetail (Core.Maybe [KeyUsage])
certificateDetail_keyUsages = Lens.lens (\CertificateDetail' {keyUsages} -> keyUsages) (\s@CertificateDetail' {} a -> s {keyUsages = a} :: CertificateDetail) Core.. Lens.mapping Lens._Coerce

-- | The time at which the certificate was revoked. This value exists only
-- when the certificate status is @REVOKED@.
certificateDetail_revokedAt :: Lens.Lens' CertificateDetail (Core.Maybe Core.UTCTime)
certificateDetail_revokedAt = Lens.lens (\CertificateDetail' {revokedAt} -> revokedAt) (\s@CertificateDetail' {} a -> s {revokedAt = a} :: CertificateDetail) Core.. Lens.mapping Core._Time

-- | The time after which the certificate is not valid.
certificateDetail_notAfter :: Lens.Lens' CertificateDetail (Core.Maybe Core.UTCTime)
certificateDetail_notAfter = Lens.lens (\CertificateDetail' {notAfter} -> notAfter) (\s@CertificateDetail' {} a -> s {notAfter = a} :: CertificateDetail) Core.. Lens.mapping Core._Time

-- | The algorithm that was used to sign the certificate.
certificateDetail_signatureAlgorithm :: Lens.Lens' CertificateDetail (Core.Maybe Core.Text)
certificateDetail_signatureAlgorithm = Lens.lens (\CertificateDetail' {signatureAlgorithm} -> signatureAlgorithm) (\s@CertificateDetail' {} a -> s {signatureAlgorithm = a} :: CertificateDetail)

-- | The name of the certificate authority that issued and signed the
-- certificate.
certificateDetail_issuer :: Lens.Lens' CertificateDetail (Core.Maybe Core.Text)
certificateDetail_issuer = Lens.lens (\CertificateDetail' {issuer} -> issuer) (\s@CertificateDetail' {} a -> s {issuer = a} :: CertificateDetail)

-- | The source of the certificate. For certificates provided by ACM, this
-- value is @AMAZON_ISSUED@. For certificates that you imported with
-- ImportCertificate, this value is @IMPORTED@. ACM does not provide
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>
-- for imported certificates. For more information about the differences
-- between certificates that you import and those that ACM provides, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing Certificates>
-- in the /AWS Certificate Manager User Guide/.
certificateDetail_type :: Lens.Lens' CertificateDetail (Core.Maybe CertificateType)
certificateDetail_type = Lens.lens (\CertificateDetail' {type'} -> type') (\s@CertificateDetail' {} a -> s {type' = a} :: CertificateDetail)

-- | The algorithm that was used to generate the public-private key pair.
certificateDetail_keyAlgorithm :: Lens.Lens' CertificateDetail (Core.Maybe KeyAlgorithm)
certificateDetail_keyAlgorithm = Lens.lens (\CertificateDetail' {keyAlgorithm} -> keyAlgorithm) (\s@CertificateDetail' {} a -> s {keyAlgorithm = a} :: CertificateDetail)

-- | The time at which the certificate was issued. This value exists only
-- when the certificate type is @AMAZON_ISSUED@.
certificateDetail_issuedAt :: Lens.Lens' CertificateDetail (Core.Maybe Core.UTCTime)
certificateDetail_issuedAt = Lens.lens (\CertificateDetail' {issuedAt} -> issuedAt) (\s@CertificateDetail' {} a -> s {issuedAt = a} :: CertificateDetail) Core.. Lens.mapping Core._Time

-- | Contains information about the status of ACM\'s
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>
-- for the certificate. This field exists only when the certificate type is
-- @AMAZON_ISSUED@.
certificateDetail_renewalSummary :: Lens.Lens' CertificateDetail (Core.Maybe RenewalSummary)
certificateDetail_renewalSummary = Lens.lens (\CertificateDetail' {renewalSummary} -> renewalSummary) (\s@CertificateDetail' {} a -> s {renewalSummary = a} :: CertificateDetail)

instance Core.FromJSON CertificateDetail where
  parseJSON =
    Core.withObject
      "CertificateDetail"
      ( \x ->
          CertificateDetail'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "NotBefore")
            Core.<*> (x Core..:? "CertificateAuthorityArn")
            Core.<*> (x Core..:? "ImportedAt")
            Core.<*> (x Core..:? "ExtendedKeyUsages" Core..!= Core.mempty)
            Core.<*> (x Core..:? "DomainValidationOptions")
            Core.<*> (x Core..:? "RenewalEligibility")
            Core.<*> (x Core..:? "Options")
            Core.<*> (x Core..:? "Serial")
            Core.<*> (x Core..:? "CertificateArn")
            Core.<*> (x Core..:? "CreatedAt")
            Core.<*> (x Core..:? "InUseBy" Core..!= Core.mempty)
            Core.<*> (x Core..:? "SubjectAlternativeNames")
            Core.<*> (x Core..:? "DomainName")
            Core.<*> (x Core..:? "RevocationReason")
            Core.<*> (x Core..:? "Subject")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "KeyUsages" Core..!= Core.mempty)
            Core.<*> (x Core..:? "RevokedAt")
            Core.<*> (x Core..:? "NotAfter")
            Core.<*> (x Core..:? "SignatureAlgorithm")
            Core.<*> (x Core..:? "Issuer")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "KeyAlgorithm")
            Core.<*> (x Core..:? "IssuedAt")
            Core.<*> (x Core..:? "RenewalSummary")
      )

instance Core.Hashable CertificateDetail

instance Core.NFData CertificateDetail

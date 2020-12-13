{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.CertificateDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.CertificateDetail
  ( CertificateDetail (..),

    -- * Smart constructor
    mkCertificateDetail,

    -- * Lenses
    cdSubject,
    cdStatus,
    cdFailureReason,
    cdSubjectAlternativeNames,
    cdInUseBy,
    cdCreatedAt,
    cdCertificateARN,
    cdSerial,
    cdRenewalEligibility,
    cdExtendedKeyUsages,
    cdImportedAt,
    cdKeyUsages,
    cdRevokedAt,
    cdNotBefore,
    cdRevocationReason,
    cdDomainName,
    cdRenewalSummary,
    cdKeyAlgorithm,
    cdType,
    cdOptions,
    cdIssuedAt,
    cdSignatureAlgorithm,
    cdDomainValidationOptions,
    cdIssuer,
    cdNotAfter,
    cdCertificateAuthorityARN,
  )
where

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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains metadata about an ACM certificate. This structure is returned in the response to a 'DescribeCertificate' request.
--
-- /See:/ 'mkCertificateDetail' smart constructor.
data CertificateDetail = CertificateDetail'
  { -- | The name of the entity that is associated with the public key contained in the certificate.
    subject :: Lude.Maybe Lude.Text,
    -- | The status of the certificate.
    status :: Lude.Maybe CertificateStatus,
    -- | The reason the certificate request failed. This value exists only when the certificate status is @FAILED@ . For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/troubleshooting.html#troubleshooting-failed Certificate Request Failed> in the /AWS Certificate Manager User Guide/ .
    failureReason :: Lude.Maybe FailureReason,
    -- | One or more domain names (subject alternative names) included in the certificate. This list contains the domain names that are bound to the public key that is contained in the certificate. The subject alternative names include the canonical domain name (CN) of the certificate and additional domain names that can be used to connect to the website.
    subjectAlternativeNames :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | A list of ARNs for the AWS resources that are using the certificate. A certificate can be used by multiple AWS resources.
    inUseBy :: Lude.Maybe [Lude.Text],
    -- | The time at which the certificate was requested. This value exists only when the certificate type is @AMAZON_ISSUED@ .
    createdAt :: Lude.Maybe Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of the certificate. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    certificateARN :: Lude.Maybe Lude.Text,
    -- | The serial number of the certificate.
    serial :: Lude.Maybe Lude.Text,
    -- | Specifies whether the certificate is eligible for renewal. At this time, only exported private certificates can be renewed with the 'RenewCertificate' command.
    renewalEligibility :: Lude.Maybe RenewalEligibility,
    -- | Contains a list of Extended Key Usage X.509 v3 extension objects. Each object specifies a purpose for which the certificate public key can be used and consists of a name and an object identifier (OID).
    extendedKeyUsages :: Lude.Maybe [ExtendedKeyUsage],
    -- | The date and time at which the certificate was imported. This value exists only when the certificate type is @IMPORTED@ .
    importedAt :: Lude.Maybe Lude.Timestamp,
    -- | A list of Key Usage X.509 v3 extension objects. Each object is a string value that identifies the purpose of the public key contained in the certificate. Possible extension values include DIGITAL_SIGNATURE, KEY_ENCHIPHERMENT, NON_REPUDIATION, and more.
    keyUsages :: Lude.Maybe [KeyUsage],
    -- | The time at which the certificate was revoked. This value exists only when the certificate status is @REVOKED@ .
    revokedAt :: Lude.Maybe Lude.Timestamp,
    -- | The time before which the certificate is not valid.
    notBefore :: Lude.Maybe Lude.Timestamp,
    -- | The reason the certificate was revoked. This value exists only when the certificate status is @REVOKED@ .
    revocationReason :: Lude.Maybe RevocationReason,
    -- | The fully qualified domain name for the certificate, such as www.example.com or example.com.
    domainName :: Lude.Maybe Lude.Text,
    -- | Contains information about the status of ACM's <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> for the certificate. This field exists only when the certificate type is @AMAZON_ISSUED@ .
    renewalSummary :: Lude.Maybe RenewalSummary,
    -- | The algorithm that was used to generate the public-private key pair.
    keyAlgorithm :: Lude.Maybe KeyAlgorithm,
    -- | The source of the certificate. For certificates provided by ACM, this value is @AMAZON_ISSUED@ . For certificates that you imported with 'ImportCertificate' , this value is @IMPORTED@ . ACM does not provide <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> for imported certificates. For more information about the differences between certificates that you import and those that ACM provides, see <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing Certificates> in the /AWS Certificate Manager User Guide/ .
    type' :: Lude.Maybe CertificateType,
    -- | Value that specifies whether to add the certificate to a transparency log. Certificate transparency makes it possible to detect SSL certificates that have been mistakenly or maliciously issued. A browser might respond to certificate that has not been logged by showing an error message. The logs are cryptographically secure.
    options :: Lude.Maybe CertificateOptions,
    -- | The time at which the certificate was issued. This value exists only when the certificate type is @AMAZON_ISSUED@ .
    issuedAt :: Lude.Maybe Lude.Timestamp,
    -- | The algorithm that was used to sign the certificate.
    signatureAlgorithm :: Lude.Maybe Lude.Text,
    -- | Contains information about the initial validation of each domain name that occurs as a result of the 'RequestCertificate' request. This field exists only when the certificate type is @AMAZON_ISSUED@ .
    domainValidationOptions :: Lude.Maybe (Lude.NonEmpty DomainValidation),
    -- | The name of the certificate authority that issued and signed the certificate.
    issuer :: Lude.Maybe Lude.Text,
    -- | The time after which the certificate is not valid.
    notAfter :: Lude.Maybe Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of the ACM PCA private certificate authority (CA) that issued the certificate. This has the following format:
    --
    -- @arn:aws:acm-pca:region:account:certificate-authority/12345678-1234-1234-1234-123456789012@
    certificateAuthorityARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CertificateDetail' with the minimum fields required to make a request.
--
-- * 'subject' - The name of the entity that is associated with the public key contained in the certificate.
-- * 'status' - The status of the certificate.
-- * 'failureReason' - The reason the certificate request failed. This value exists only when the certificate status is @FAILED@ . For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/troubleshooting.html#troubleshooting-failed Certificate Request Failed> in the /AWS Certificate Manager User Guide/ .
-- * 'subjectAlternativeNames' - One or more domain names (subject alternative names) included in the certificate. This list contains the domain names that are bound to the public key that is contained in the certificate. The subject alternative names include the canonical domain name (CN) of the certificate and additional domain names that can be used to connect to the website.
-- * 'inUseBy' - A list of ARNs for the AWS resources that are using the certificate. A certificate can be used by multiple AWS resources.
-- * 'createdAt' - The time at which the certificate was requested. This value exists only when the certificate type is @AMAZON_ISSUED@ .
-- * 'certificateARN' - The Amazon Resource Name (ARN) of the certificate. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
-- * 'serial' - The serial number of the certificate.
-- * 'renewalEligibility' - Specifies whether the certificate is eligible for renewal. At this time, only exported private certificates can be renewed with the 'RenewCertificate' command.
-- * 'extendedKeyUsages' - Contains a list of Extended Key Usage X.509 v3 extension objects. Each object specifies a purpose for which the certificate public key can be used and consists of a name and an object identifier (OID).
-- * 'importedAt' - The date and time at which the certificate was imported. This value exists only when the certificate type is @IMPORTED@ .
-- * 'keyUsages' - A list of Key Usage X.509 v3 extension objects. Each object is a string value that identifies the purpose of the public key contained in the certificate. Possible extension values include DIGITAL_SIGNATURE, KEY_ENCHIPHERMENT, NON_REPUDIATION, and more.
-- * 'revokedAt' - The time at which the certificate was revoked. This value exists only when the certificate status is @REVOKED@ .
-- * 'notBefore' - The time before which the certificate is not valid.
-- * 'revocationReason' - The reason the certificate was revoked. This value exists only when the certificate status is @REVOKED@ .
-- * 'domainName' - The fully qualified domain name for the certificate, such as www.example.com or example.com.
-- * 'renewalSummary' - Contains information about the status of ACM's <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> for the certificate. This field exists only when the certificate type is @AMAZON_ISSUED@ .
-- * 'keyAlgorithm' - The algorithm that was used to generate the public-private key pair.
-- * 'type'' - The source of the certificate. For certificates provided by ACM, this value is @AMAZON_ISSUED@ . For certificates that you imported with 'ImportCertificate' , this value is @IMPORTED@ . ACM does not provide <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> for imported certificates. For more information about the differences between certificates that you import and those that ACM provides, see <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing Certificates> in the /AWS Certificate Manager User Guide/ .
-- * 'options' - Value that specifies whether to add the certificate to a transparency log. Certificate transparency makes it possible to detect SSL certificates that have been mistakenly or maliciously issued. A browser might respond to certificate that has not been logged by showing an error message. The logs are cryptographically secure.
-- * 'issuedAt' - The time at which the certificate was issued. This value exists only when the certificate type is @AMAZON_ISSUED@ .
-- * 'signatureAlgorithm' - The algorithm that was used to sign the certificate.
-- * 'domainValidationOptions' - Contains information about the initial validation of each domain name that occurs as a result of the 'RequestCertificate' request. This field exists only when the certificate type is @AMAZON_ISSUED@ .
-- * 'issuer' - The name of the certificate authority that issued and signed the certificate.
-- * 'notAfter' - The time after which the certificate is not valid.
-- * 'certificateAuthorityARN' - The Amazon Resource Name (ARN) of the ACM PCA private certificate authority (CA) that issued the certificate. This has the following format:
--
-- @arn:aws:acm-pca:region:account:certificate-authority/12345678-1234-1234-1234-123456789012@
mkCertificateDetail ::
  CertificateDetail
mkCertificateDetail =
  CertificateDetail'
    { subject = Lude.Nothing,
      status = Lude.Nothing,
      failureReason = Lude.Nothing,
      subjectAlternativeNames = Lude.Nothing,
      inUseBy = Lude.Nothing,
      createdAt = Lude.Nothing,
      certificateARN = Lude.Nothing,
      serial = Lude.Nothing,
      renewalEligibility = Lude.Nothing,
      extendedKeyUsages = Lude.Nothing,
      importedAt = Lude.Nothing,
      keyUsages = Lude.Nothing,
      revokedAt = Lude.Nothing,
      notBefore = Lude.Nothing,
      revocationReason = Lude.Nothing,
      domainName = Lude.Nothing,
      renewalSummary = Lude.Nothing,
      keyAlgorithm = Lude.Nothing,
      type' = Lude.Nothing,
      options = Lude.Nothing,
      issuedAt = Lude.Nothing,
      signatureAlgorithm = Lude.Nothing,
      domainValidationOptions = Lude.Nothing,
      issuer = Lude.Nothing,
      notAfter = Lude.Nothing,
      certificateAuthorityARN = Lude.Nothing
    }

-- | The name of the entity that is associated with the public key contained in the certificate.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSubject :: Lens.Lens' CertificateDetail (Lude.Maybe Lude.Text)
cdSubject = Lens.lens (subject :: CertificateDetail -> Lude.Maybe Lude.Text) (\s a -> s {subject = a} :: CertificateDetail)
{-# DEPRECATED cdSubject "Use generic-lens or generic-optics with 'subject' instead." #-}

-- | The status of the certificate.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdStatus :: Lens.Lens' CertificateDetail (Lude.Maybe CertificateStatus)
cdStatus = Lens.lens (status :: CertificateDetail -> Lude.Maybe CertificateStatus) (\s a -> s {status = a} :: CertificateDetail)
{-# DEPRECATED cdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The reason the certificate request failed. This value exists only when the certificate status is @FAILED@ . For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/troubleshooting.html#troubleshooting-failed Certificate Request Failed> in the /AWS Certificate Manager User Guide/ .
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdFailureReason :: Lens.Lens' CertificateDetail (Lude.Maybe FailureReason)
cdFailureReason = Lens.lens (failureReason :: CertificateDetail -> Lude.Maybe FailureReason) (\s a -> s {failureReason = a} :: CertificateDetail)
{-# DEPRECATED cdFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | One or more domain names (subject alternative names) included in the certificate. This list contains the domain names that are bound to the public key that is contained in the certificate. The subject alternative names include the canonical domain name (CN) of the certificate and additional domain names that can be used to connect to the website.
--
-- /Note:/ Consider using 'subjectAlternativeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSubjectAlternativeNames :: Lens.Lens' CertificateDetail (Lude.Maybe (Lude.NonEmpty Lude.Text))
cdSubjectAlternativeNames = Lens.lens (subjectAlternativeNames :: CertificateDetail -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {subjectAlternativeNames = a} :: CertificateDetail)
{-# DEPRECATED cdSubjectAlternativeNames "Use generic-lens or generic-optics with 'subjectAlternativeNames' instead." #-}

-- | A list of ARNs for the AWS resources that are using the certificate. A certificate can be used by multiple AWS resources.
--
-- /Note:/ Consider using 'inUseBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdInUseBy :: Lens.Lens' CertificateDetail (Lude.Maybe [Lude.Text])
cdInUseBy = Lens.lens (inUseBy :: CertificateDetail -> Lude.Maybe [Lude.Text]) (\s a -> s {inUseBy = a} :: CertificateDetail)
{-# DEPRECATED cdInUseBy "Use generic-lens or generic-optics with 'inUseBy' instead." #-}

-- | The time at which the certificate was requested. This value exists only when the certificate type is @AMAZON_ISSUED@ .
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCreatedAt :: Lens.Lens' CertificateDetail (Lude.Maybe Lude.Timestamp)
cdCreatedAt = Lens.lens (createdAt :: CertificateDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: CertificateDetail)
{-# DEPRECATED cdCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The Amazon Resource Name (ARN) of the certificate. For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCertificateARN :: Lens.Lens' CertificateDetail (Lude.Maybe Lude.Text)
cdCertificateARN = Lens.lens (certificateARN :: CertificateDetail -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: CertificateDetail)
{-# DEPRECATED cdCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The serial number of the certificate.
--
-- /Note:/ Consider using 'serial' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSerial :: Lens.Lens' CertificateDetail (Lude.Maybe Lude.Text)
cdSerial = Lens.lens (serial :: CertificateDetail -> Lude.Maybe Lude.Text) (\s a -> s {serial = a} :: CertificateDetail)
{-# DEPRECATED cdSerial "Use generic-lens or generic-optics with 'serial' instead." #-}

-- | Specifies whether the certificate is eligible for renewal. At this time, only exported private certificates can be renewed with the 'RenewCertificate' command.
--
-- /Note:/ Consider using 'renewalEligibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdRenewalEligibility :: Lens.Lens' CertificateDetail (Lude.Maybe RenewalEligibility)
cdRenewalEligibility = Lens.lens (renewalEligibility :: CertificateDetail -> Lude.Maybe RenewalEligibility) (\s a -> s {renewalEligibility = a} :: CertificateDetail)
{-# DEPRECATED cdRenewalEligibility "Use generic-lens or generic-optics with 'renewalEligibility' instead." #-}

-- | Contains a list of Extended Key Usage X.509 v3 extension objects. Each object specifies a purpose for which the certificate public key can be used and consists of a name and an object identifier (OID).
--
-- /Note:/ Consider using 'extendedKeyUsages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdExtendedKeyUsages :: Lens.Lens' CertificateDetail (Lude.Maybe [ExtendedKeyUsage])
cdExtendedKeyUsages = Lens.lens (extendedKeyUsages :: CertificateDetail -> Lude.Maybe [ExtendedKeyUsage]) (\s a -> s {extendedKeyUsages = a} :: CertificateDetail)
{-# DEPRECATED cdExtendedKeyUsages "Use generic-lens or generic-optics with 'extendedKeyUsages' instead." #-}

-- | The date and time at which the certificate was imported. This value exists only when the certificate type is @IMPORTED@ .
--
-- /Note:/ Consider using 'importedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdImportedAt :: Lens.Lens' CertificateDetail (Lude.Maybe Lude.Timestamp)
cdImportedAt = Lens.lens (importedAt :: CertificateDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {importedAt = a} :: CertificateDetail)
{-# DEPRECATED cdImportedAt "Use generic-lens or generic-optics with 'importedAt' instead." #-}

-- | A list of Key Usage X.509 v3 extension objects. Each object is a string value that identifies the purpose of the public key contained in the certificate. Possible extension values include DIGITAL_SIGNATURE, KEY_ENCHIPHERMENT, NON_REPUDIATION, and more.
--
-- /Note:/ Consider using 'keyUsages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdKeyUsages :: Lens.Lens' CertificateDetail (Lude.Maybe [KeyUsage])
cdKeyUsages = Lens.lens (keyUsages :: CertificateDetail -> Lude.Maybe [KeyUsage]) (\s a -> s {keyUsages = a} :: CertificateDetail)
{-# DEPRECATED cdKeyUsages "Use generic-lens or generic-optics with 'keyUsages' instead." #-}

-- | The time at which the certificate was revoked. This value exists only when the certificate status is @REVOKED@ .
--
-- /Note:/ Consider using 'revokedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdRevokedAt :: Lens.Lens' CertificateDetail (Lude.Maybe Lude.Timestamp)
cdRevokedAt = Lens.lens (revokedAt :: CertificateDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {revokedAt = a} :: CertificateDetail)
{-# DEPRECATED cdRevokedAt "Use generic-lens or generic-optics with 'revokedAt' instead." #-}

-- | The time before which the certificate is not valid.
--
-- /Note:/ Consider using 'notBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdNotBefore :: Lens.Lens' CertificateDetail (Lude.Maybe Lude.Timestamp)
cdNotBefore = Lens.lens (notBefore :: CertificateDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {notBefore = a} :: CertificateDetail)
{-# DEPRECATED cdNotBefore "Use generic-lens or generic-optics with 'notBefore' instead." #-}

-- | The reason the certificate was revoked. This value exists only when the certificate status is @REVOKED@ .
--
-- /Note:/ Consider using 'revocationReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdRevocationReason :: Lens.Lens' CertificateDetail (Lude.Maybe RevocationReason)
cdRevocationReason = Lens.lens (revocationReason :: CertificateDetail -> Lude.Maybe RevocationReason) (\s a -> s {revocationReason = a} :: CertificateDetail)
{-# DEPRECATED cdRevocationReason "Use generic-lens or generic-optics with 'revocationReason' instead." #-}

-- | The fully qualified domain name for the certificate, such as www.example.com or example.com.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDomainName :: Lens.Lens' CertificateDetail (Lude.Maybe Lude.Text)
cdDomainName = Lens.lens (domainName :: CertificateDetail -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: CertificateDetail)
{-# DEPRECATED cdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Contains information about the status of ACM's <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> for the certificate. This field exists only when the certificate type is @AMAZON_ISSUED@ .
--
-- /Note:/ Consider using 'renewalSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdRenewalSummary :: Lens.Lens' CertificateDetail (Lude.Maybe RenewalSummary)
cdRenewalSummary = Lens.lens (renewalSummary :: CertificateDetail -> Lude.Maybe RenewalSummary) (\s a -> s {renewalSummary = a} :: CertificateDetail)
{-# DEPRECATED cdRenewalSummary "Use generic-lens or generic-optics with 'renewalSummary' instead." #-}

-- | The algorithm that was used to generate the public-private key pair.
--
-- /Note:/ Consider using 'keyAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdKeyAlgorithm :: Lens.Lens' CertificateDetail (Lude.Maybe KeyAlgorithm)
cdKeyAlgorithm = Lens.lens (keyAlgorithm :: CertificateDetail -> Lude.Maybe KeyAlgorithm) (\s a -> s {keyAlgorithm = a} :: CertificateDetail)
{-# DEPRECATED cdKeyAlgorithm "Use generic-lens or generic-optics with 'keyAlgorithm' instead." #-}

-- | The source of the certificate. For certificates provided by ACM, this value is @AMAZON_ISSUED@ . For certificates that you imported with 'ImportCertificate' , this value is @IMPORTED@ . ACM does not provide <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> for imported certificates. For more information about the differences between certificates that you import and those that ACM provides, see <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing Certificates> in the /AWS Certificate Manager User Guide/ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdType :: Lens.Lens' CertificateDetail (Lude.Maybe CertificateType)
cdType = Lens.lens (type' :: CertificateDetail -> Lude.Maybe CertificateType) (\s a -> s {type' = a} :: CertificateDetail)
{-# DEPRECATED cdType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Value that specifies whether to add the certificate to a transparency log. Certificate transparency makes it possible to detect SSL certificates that have been mistakenly or maliciously issued. A browser might respond to certificate that has not been logged by showing an error message. The logs are cryptographically secure.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdOptions :: Lens.Lens' CertificateDetail (Lude.Maybe CertificateOptions)
cdOptions = Lens.lens (options :: CertificateDetail -> Lude.Maybe CertificateOptions) (\s a -> s {options = a} :: CertificateDetail)
{-# DEPRECATED cdOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | The time at which the certificate was issued. This value exists only when the certificate type is @AMAZON_ISSUED@ .
--
-- /Note:/ Consider using 'issuedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdIssuedAt :: Lens.Lens' CertificateDetail (Lude.Maybe Lude.Timestamp)
cdIssuedAt = Lens.lens (issuedAt :: CertificateDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {issuedAt = a} :: CertificateDetail)
{-# DEPRECATED cdIssuedAt "Use generic-lens or generic-optics with 'issuedAt' instead." #-}

-- | The algorithm that was used to sign the certificate.
--
-- /Note:/ Consider using 'signatureAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdSignatureAlgorithm :: Lens.Lens' CertificateDetail (Lude.Maybe Lude.Text)
cdSignatureAlgorithm = Lens.lens (signatureAlgorithm :: CertificateDetail -> Lude.Maybe Lude.Text) (\s a -> s {signatureAlgorithm = a} :: CertificateDetail)
{-# DEPRECATED cdSignatureAlgorithm "Use generic-lens or generic-optics with 'signatureAlgorithm' instead." #-}

-- | Contains information about the initial validation of each domain name that occurs as a result of the 'RequestCertificate' request. This field exists only when the certificate type is @AMAZON_ISSUED@ .
--
-- /Note:/ Consider using 'domainValidationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdDomainValidationOptions :: Lens.Lens' CertificateDetail (Lude.Maybe (Lude.NonEmpty DomainValidation))
cdDomainValidationOptions = Lens.lens (domainValidationOptions :: CertificateDetail -> Lude.Maybe (Lude.NonEmpty DomainValidation)) (\s a -> s {domainValidationOptions = a} :: CertificateDetail)
{-# DEPRECATED cdDomainValidationOptions "Use generic-lens or generic-optics with 'domainValidationOptions' instead." #-}

-- | The name of the certificate authority that issued and signed the certificate.
--
-- /Note:/ Consider using 'issuer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdIssuer :: Lens.Lens' CertificateDetail (Lude.Maybe Lude.Text)
cdIssuer = Lens.lens (issuer :: CertificateDetail -> Lude.Maybe Lude.Text) (\s a -> s {issuer = a} :: CertificateDetail)
{-# DEPRECATED cdIssuer "Use generic-lens or generic-optics with 'issuer' instead." #-}

-- | The time after which the certificate is not valid.
--
-- /Note:/ Consider using 'notAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdNotAfter :: Lens.Lens' CertificateDetail (Lude.Maybe Lude.Timestamp)
cdNotAfter = Lens.lens (notAfter :: CertificateDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {notAfter = a} :: CertificateDetail)
{-# DEPRECATED cdNotAfter "Use generic-lens or generic-optics with 'notAfter' instead." #-}

-- | The Amazon Resource Name (ARN) of the ACM PCA private certificate authority (CA) that issued the certificate. This has the following format:
--
-- @arn:aws:acm-pca:region:account:certificate-authority/12345678-1234-1234-1234-123456789012@
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCertificateAuthorityARN :: Lens.Lens' CertificateDetail (Lude.Maybe Lude.Text)
cdCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: CertificateDetail -> Lude.Maybe Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: CertificateDetail)
{-# DEPRECATED cdCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

instance Lude.FromJSON CertificateDetail where
  parseJSON =
    Lude.withObject
      "CertificateDetail"
      ( \x ->
          CertificateDetail'
            Lude.<$> (x Lude..:? "Subject")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..:? "SubjectAlternativeNames")
            Lude.<*> (x Lude..:? "InUseBy" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "CertificateArn")
            Lude.<*> (x Lude..:? "Serial")
            Lude.<*> (x Lude..:? "RenewalEligibility")
            Lude.<*> (x Lude..:? "ExtendedKeyUsages" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ImportedAt")
            Lude.<*> (x Lude..:? "KeyUsages" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "RevokedAt")
            Lude.<*> (x Lude..:? "NotBefore")
            Lude.<*> (x Lude..:? "RevocationReason")
            Lude.<*> (x Lude..:? "DomainName")
            Lude.<*> (x Lude..:? "RenewalSummary")
            Lude.<*> (x Lude..:? "KeyAlgorithm")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "Options")
            Lude.<*> (x Lude..:? "IssuedAt")
            Lude.<*> (x Lude..:? "SignatureAlgorithm")
            Lude.<*> (x Lude..:? "DomainValidationOptions")
            Lude.<*> (x Lude..:? "Issuer")
            Lude.<*> (x Lude..:? "NotAfter")
            Lude.<*> (x Lude..:? "CertificateAuthorityArn")
      )

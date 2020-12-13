{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Certificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Certificate
  ( Certificate (..),

    -- * Smart constructor
    mkCertificate,

    -- * Lenses
    cfStatus,
    cfSubjectAlternativeNames,
    cfArn,
    cfCreatedAt,
    cfEligibleToRenew,
    cfRequestFailureReason,
    cfRevokedAt,
    cfNotBefore,
    cfRevocationReason,
    cfDomainName,
    cfName,
    cfRenewalSummary,
    cfSupportCode,
    cfDomainValidationRecords,
    cfInUseResourceCount,
    cfIssuedAt,
    cfKeyAlgorithm,
    cfSerialNumber,
    cfIssuerCA,
    cfTags,
    cfNotAfter,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.CertificateStatus
import Network.AWS.Lightsail.Types.DomainValidationRecord
import Network.AWS.Lightsail.Types.RenewalSummary
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Lude

-- | Describes the full details of an Amazon Lightsail SSL/TLS certificate.
--
-- /See:/ 'mkCertificate' smart constructor.
data Certificate = Certificate'
  { -- | The validation status of the certificate.
    status :: Lude.Maybe CertificateStatus,
    -- | An array of strings that specify the alternate domains (e.g., @example2.com@ ) and subdomains (e.g., @blog.example.com@ ) of the certificate.
    subjectAlternativeNames :: Lude.Maybe [Lude.Text],
    -- | The Amazon Resource Name (ARN) of the certificate.
    arn :: Lude.Maybe Lude.Text,
    -- | The timestamp when the certificate was created.
    createdAt :: Lude.Maybe Lude.Timestamp,
    -- | The renewal eligibility of the certificate.
    eligibleToRenew :: Lude.Maybe Lude.Text,
    -- | The validation failure reason, if any, of the certificate.
    --
    -- The following failure reasons are possible:
    --
    --     * __@NO_AVAILABLE_CONTACTS@ __ - This failure applies to email validation, which is not available for Lightsail certificates.
    --
    --
    --     * __@ADDITIONAL_VERIFICATION_REQUIRED@ __ - Lightsail requires additional information to process this certificate request. This can happen as a fraud-protection measure, such as when the domain ranks within the Alexa top 1000 websites. To provide the required information, use the <https://console.aws.amazon.com/support/home AWS Support Center> to contact AWS Support.
    --
    --
    --     * __@DOMAIN_NOT_ALLOWED@ __ - One or more of the domain names in the certificate request was reported as an unsafe domain by <https://www.virustotal.com/gui/home/url VirusTotal> . To correct the problem, search for your domain name on the <https://www.virustotal.com/gui/home/url VirusTotal> website. If your domain is reported as suspicious, see <https://www.google.com/webmasters/hacked/?hl=en Google Help for Hacked Websites> to learn what you can do.
    -- If you believe that the result is a false positive, notify the organization that is reporting the domain. VirusTotal is an aggregate of several antivirus and URL scanners and cannot remove your domain from a block list itself. After you correct the problem and the VirusTotal registry has been updated, request a new certificate.
    -- If you see this error and your domain is not included in the VirusTotal list, visit the <https://console.aws.amazon.com/support/home AWS Support Center> and create a case.
    --
    --
    --     * __@INVALID_PUBLIC_DOMAIN@ __ - One or more of the domain names in the certificate request is not valid. Typically, this is because a domain name in the request is not a valid top-level domain. Try to request a certificate again, correcting any spelling errors or typos that were in the failed request, and ensure that all domain names in the request are for valid top-level domains. For example, you cannot request a certificate for @example.invalidpublicdomain@ because @invalidpublicdomain@ is not a valid top-level domain.
    --
    --
    --     * __@OTHER@ __ - Typically, this failure occurs when there is a typographical error in one or more of the domain names in the certificate request. Try to request a certificate again, correcting any spelling errors or typos that were in the failed request.
    requestFailureReason :: Lude.Maybe Lude.Text,
    -- | The timestamp when the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
    revokedAt :: Lude.Maybe Lude.Timestamp,
    -- | The timestamp when the certificate is first valid.
    notBefore :: Lude.Maybe Lude.Timestamp,
    -- | The reason the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
    revocationReason :: Lude.Maybe Lude.Text,
    -- | The domain name of the certificate.
    domainName :: Lude.Maybe Lude.Text,
    -- | The name of the certificate (e.g., @my-certificate@ ).
    name :: Lude.Maybe Lude.Text,
    -- | An object that describes the status of the certificate renewal managed by Lightsail.
    renewalSummary :: Lude.Maybe RenewalSummary,
    -- | The support code. Include this code in your email to support when you have questions about your Lightsail certificate. This code enables our support team to look up your Lightsail information more easily.
    supportCode :: Lude.Maybe Lude.Text,
    -- | An array of objects that describe the domain validation records of the certificate.
    domainValidationRecords :: Lude.Maybe [DomainValidationRecord],
    -- | The number of Lightsail resources that the certificate is attached to.
    inUseResourceCount :: Lude.Maybe Lude.Int,
    -- | The timestamp when the certificate was issued.
    issuedAt :: Lude.Maybe Lude.Timestamp,
    -- | The algorithm used to generate the key pair (the public and private key) of the certificate.
    keyAlgorithm :: Lude.Maybe Lude.Text,
    -- | The serial number of the certificate.
    serialNumber :: Lude.Maybe Lude.Text,
    -- | The certificate authority that issued the certificate.
    issuerCA :: Lude.Maybe Lude.Text,
    -- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
    tags :: Lude.Maybe [Tag],
    -- | The timestamp when the certificate expires.
    notAfter :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Certificate' with the minimum fields required to make a request.
--
-- * 'status' - The validation status of the certificate.
-- * 'subjectAlternativeNames' - An array of strings that specify the alternate domains (e.g., @example2.com@ ) and subdomains (e.g., @blog.example.com@ ) of the certificate.
-- * 'arn' - The Amazon Resource Name (ARN) of the certificate.
-- * 'createdAt' - The timestamp when the certificate was created.
-- * 'eligibleToRenew' - The renewal eligibility of the certificate.
-- * 'requestFailureReason' - The validation failure reason, if any, of the certificate.
--
-- The following failure reasons are possible:
--
--     * __@NO_AVAILABLE_CONTACTS@ __ - This failure applies to email validation, which is not available for Lightsail certificates.
--
--
--     * __@ADDITIONAL_VERIFICATION_REQUIRED@ __ - Lightsail requires additional information to process this certificate request. This can happen as a fraud-protection measure, such as when the domain ranks within the Alexa top 1000 websites. To provide the required information, use the <https://console.aws.amazon.com/support/home AWS Support Center> to contact AWS Support.
--
--
--     * __@DOMAIN_NOT_ALLOWED@ __ - One or more of the domain names in the certificate request was reported as an unsafe domain by <https://www.virustotal.com/gui/home/url VirusTotal> . To correct the problem, search for your domain name on the <https://www.virustotal.com/gui/home/url VirusTotal> website. If your domain is reported as suspicious, see <https://www.google.com/webmasters/hacked/?hl=en Google Help for Hacked Websites> to learn what you can do.
-- If you believe that the result is a false positive, notify the organization that is reporting the domain. VirusTotal is an aggregate of several antivirus and URL scanners and cannot remove your domain from a block list itself. After you correct the problem and the VirusTotal registry has been updated, request a new certificate.
-- If you see this error and your domain is not included in the VirusTotal list, visit the <https://console.aws.amazon.com/support/home AWS Support Center> and create a case.
--
--
--     * __@INVALID_PUBLIC_DOMAIN@ __ - One or more of the domain names in the certificate request is not valid. Typically, this is because a domain name in the request is not a valid top-level domain. Try to request a certificate again, correcting any spelling errors or typos that were in the failed request, and ensure that all domain names in the request are for valid top-level domains. For example, you cannot request a certificate for @example.invalidpublicdomain@ because @invalidpublicdomain@ is not a valid top-level domain.
--
--
--     * __@OTHER@ __ - Typically, this failure occurs when there is a typographical error in one or more of the domain names in the certificate request. Try to request a certificate again, correcting any spelling errors or typos that were in the failed request.
--
--
-- * 'revokedAt' - The timestamp when the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
-- * 'notBefore' - The timestamp when the certificate is first valid.
-- * 'revocationReason' - The reason the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
-- * 'domainName' - The domain name of the certificate.
-- * 'name' - The name of the certificate (e.g., @my-certificate@ ).
-- * 'renewalSummary' - An object that describes the status of the certificate renewal managed by Lightsail.
-- * 'supportCode' - The support code. Include this code in your email to support when you have questions about your Lightsail certificate. This code enables our support team to look up your Lightsail information more easily.
-- * 'domainValidationRecords' - An array of objects that describe the domain validation records of the certificate.
-- * 'inUseResourceCount' - The number of Lightsail resources that the certificate is attached to.
-- * 'issuedAt' - The timestamp when the certificate was issued.
-- * 'keyAlgorithm' - The algorithm used to generate the key pair (the public and private key) of the certificate.
-- * 'serialNumber' - The serial number of the certificate.
-- * 'issuerCA' - The certificate authority that issued the certificate.
-- * 'tags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
-- * 'notAfter' - The timestamp when the certificate expires.
mkCertificate ::
  Certificate
mkCertificate =
  Certificate'
    { status = Lude.Nothing,
      subjectAlternativeNames = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      eligibleToRenew = Lude.Nothing,
      requestFailureReason = Lude.Nothing,
      revokedAt = Lude.Nothing,
      notBefore = Lude.Nothing,
      revocationReason = Lude.Nothing,
      domainName = Lude.Nothing,
      name = Lude.Nothing,
      renewalSummary = Lude.Nothing,
      supportCode = Lude.Nothing,
      domainValidationRecords = Lude.Nothing,
      inUseResourceCount = Lude.Nothing,
      issuedAt = Lude.Nothing,
      keyAlgorithm = Lude.Nothing,
      serialNumber = Lude.Nothing,
      issuerCA = Lude.Nothing,
      tags = Lude.Nothing,
      notAfter = Lude.Nothing
    }

-- | The validation status of the certificate.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfStatus :: Lens.Lens' Certificate (Lude.Maybe CertificateStatus)
cfStatus = Lens.lens (status :: Certificate -> Lude.Maybe CertificateStatus) (\s a -> s {status = a} :: Certificate)
{-# DEPRECATED cfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | An array of strings that specify the alternate domains (e.g., @example2.com@ ) and subdomains (e.g., @blog.example.com@ ) of the certificate.
--
-- /Note:/ Consider using 'subjectAlternativeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfSubjectAlternativeNames :: Lens.Lens' Certificate (Lude.Maybe [Lude.Text])
cfSubjectAlternativeNames = Lens.lens (subjectAlternativeNames :: Certificate -> Lude.Maybe [Lude.Text]) (\s a -> s {subjectAlternativeNames = a} :: Certificate)
{-# DEPRECATED cfSubjectAlternativeNames "Use generic-lens or generic-optics with 'subjectAlternativeNames' instead." #-}

-- | The Amazon Resource Name (ARN) of the certificate.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfArn :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cfArn = Lens.lens (arn :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Certificate)
{-# DEPRECATED cfArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The timestamp when the certificate was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfCreatedAt :: Lens.Lens' Certificate (Lude.Maybe Lude.Timestamp)
cfCreatedAt = Lens.lens (createdAt :: Certificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: Certificate)
{-# DEPRECATED cfCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The renewal eligibility of the certificate.
--
-- /Note:/ Consider using 'eligibleToRenew' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfEligibleToRenew :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cfEligibleToRenew = Lens.lens (eligibleToRenew :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {eligibleToRenew = a} :: Certificate)
{-# DEPRECATED cfEligibleToRenew "Use generic-lens or generic-optics with 'eligibleToRenew' instead." #-}

-- | The validation failure reason, if any, of the certificate.
--
-- The following failure reasons are possible:
--
--     * __@NO_AVAILABLE_CONTACTS@ __ - This failure applies to email validation, which is not available for Lightsail certificates.
--
--
--     * __@ADDITIONAL_VERIFICATION_REQUIRED@ __ - Lightsail requires additional information to process this certificate request. This can happen as a fraud-protection measure, such as when the domain ranks within the Alexa top 1000 websites. To provide the required information, use the <https://console.aws.amazon.com/support/home AWS Support Center> to contact AWS Support.
--
--
--     * __@DOMAIN_NOT_ALLOWED@ __ - One or more of the domain names in the certificate request was reported as an unsafe domain by <https://www.virustotal.com/gui/home/url VirusTotal> . To correct the problem, search for your domain name on the <https://www.virustotal.com/gui/home/url VirusTotal> website. If your domain is reported as suspicious, see <https://www.google.com/webmasters/hacked/?hl=en Google Help for Hacked Websites> to learn what you can do.
-- If you believe that the result is a false positive, notify the organization that is reporting the domain. VirusTotal is an aggregate of several antivirus and URL scanners and cannot remove your domain from a block list itself. After you correct the problem and the VirusTotal registry has been updated, request a new certificate.
-- If you see this error and your domain is not included in the VirusTotal list, visit the <https://console.aws.amazon.com/support/home AWS Support Center> and create a case.
--
--
--     * __@INVALID_PUBLIC_DOMAIN@ __ - One or more of the domain names in the certificate request is not valid. Typically, this is because a domain name in the request is not a valid top-level domain. Try to request a certificate again, correcting any spelling errors or typos that were in the failed request, and ensure that all domain names in the request are for valid top-level domains. For example, you cannot request a certificate for @example.invalidpublicdomain@ because @invalidpublicdomain@ is not a valid top-level domain.
--
--
--     * __@OTHER@ __ - Typically, this failure occurs when there is a typographical error in one or more of the domain names in the certificate request. Try to request a certificate again, correcting any spelling errors or typos that were in the failed request.
--
--
--
-- /Note:/ Consider using 'requestFailureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRequestFailureReason :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cfRequestFailureReason = Lens.lens (requestFailureReason :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {requestFailureReason = a} :: Certificate)
{-# DEPRECATED cfRequestFailureReason "Use generic-lens or generic-optics with 'requestFailureReason' instead." #-}

-- | The timestamp when the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
--
-- /Note:/ Consider using 'revokedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRevokedAt :: Lens.Lens' Certificate (Lude.Maybe Lude.Timestamp)
cfRevokedAt = Lens.lens (revokedAt :: Certificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {revokedAt = a} :: Certificate)
{-# DEPRECATED cfRevokedAt "Use generic-lens or generic-optics with 'revokedAt' instead." #-}

-- | The timestamp when the certificate is first valid.
--
-- /Note:/ Consider using 'notBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfNotBefore :: Lens.Lens' Certificate (Lude.Maybe Lude.Timestamp)
cfNotBefore = Lens.lens (notBefore :: Certificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {notBefore = a} :: Certificate)
{-# DEPRECATED cfNotBefore "Use generic-lens or generic-optics with 'notBefore' instead." #-}

-- | The reason the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
--
-- /Note:/ Consider using 'revocationReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRevocationReason :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cfRevocationReason = Lens.lens (revocationReason :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {revocationReason = a} :: Certificate)
{-# DEPRECATED cfRevocationReason "Use generic-lens or generic-optics with 'revocationReason' instead." #-}

-- | The domain name of the certificate.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDomainName :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cfDomainName = Lens.lens (domainName :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: Certificate)
{-# DEPRECATED cfDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The name of the certificate (e.g., @my-certificate@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfName :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cfName = Lens.lens (name :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Certificate)
{-# DEPRECATED cfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An object that describes the status of the certificate renewal managed by Lightsail.
--
-- /Note:/ Consider using 'renewalSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRenewalSummary :: Lens.Lens' Certificate (Lude.Maybe RenewalSummary)
cfRenewalSummary = Lens.lens (renewalSummary :: Certificate -> Lude.Maybe RenewalSummary) (\s a -> s {renewalSummary = a} :: Certificate)
{-# DEPRECATED cfRenewalSummary "Use generic-lens or generic-optics with 'renewalSummary' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about your Lightsail certificate. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfSupportCode :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cfSupportCode = Lens.lens (supportCode :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {supportCode = a} :: Certificate)
{-# DEPRECATED cfSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | An array of objects that describe the domain validation records of the certificate.
--
-- /Note:/ Consider using 'domainValidationRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDomainValidationRecords :: Lens.Lens' Certificate (Lude.Maybe [DomainValidationRecord])
cfDomainValidationRecords = Lens.lens (domainValidationRecords :: Certificate -> Lude.Maybe [DomainValidationRecord]) (\s a -> s {domainValidationRecords = a} :: Certificate)
{-# DEPRECATED cfDomainValidationRecords "Use generic-lens or generic-optics with 'domainValidationRecords' instead." #-}

-- | The number of Lightsail resources that the certificate is attached to.
--
-- /Note:/ Consider using 'inUseResourceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfInUseResourceCount :: Lens.Lens' Certificate (Lude.Maybe Lude.Int)
cfInUseResourceCount = Lens.lens (inUseResourceCount :: Certificate -> Lude.Maybe Lude.Int) (\s a -> s {inUseResourceCount = a} :: Certificate)
{-# DEPRECATED cfInUseResourceCount "Use generic-lens or generic-optics with 'inUseResourceCount' instead." #-}

-- | The timestamp when the certificate was issued.
--
-- /Note:/ Consider using 'issuedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfIssuedAt :: Lens.Lens' Certificate (Lude.Maybe Lude.Timestamp)
cfIssuedAt = Lens.lens (issuedAt :: Certificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {issuedAt = a} :: Certificate)
{-# DEPRECATED cfIssuedAt "Use generic-lens or generic-optics with 'issuedAt' instead." #-}

-- | The algorithm used to generate the key pair (the public and private key) of the certificate.
--
-- /Note:/ Consider using 'keyAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfKeyAlgorithm :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cfKeyAlgorithm = Lens.lens (keyAlgorithm :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {keyAlgorithm = a} :: Certificate)
{-# DEPRECATED cfKeyAlgorithm "Use generic-lens or generic-optics with 'keyAlgorithm' instead." #-}

-- | The serial number of the certificate.
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfSerialNumber :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cfSerialNumber = Lens.lens (serialNumber :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {serialNumber = a} :: Certificate)
{-# DEPRECATED cfSerialNumber "Use generic-lens or generic-optics with 'serialNumber' instead." #-}

-- | The certificate authority that issued the certificate.
--
-- /Note:/ Consider using 'issuerCA' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfIssuerCA :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cfIssuerCA = Lens.lens (issuerCA :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {issuerCA = a} :: Certificate)
{-# DEPRECATED cfIssuerCA "Use generic-lens or generic-optics with 'issuerCA' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTags :: Lens.Lens' Certificate (Lude.Maybe [Tag])
cfTags = Lens.lens (tags :: Certificate -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Certificate)
{-# DEPRECATED cfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The timestamp when the certificate expires.
--
-- /Note:/ Consider using 'notAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfNotAfter :: Lens.Lens' Certificate (Lude.Maybe Lude.Timestamp)
cfNotAfter = Lens.lens (notAfter :: Certificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {notAfter = a} :: Certificate)
{-# DEPRECATED cfNotAfter "Use generic-lens or generic-optics with 'notAfter' instead." #-}

instance Lude.FromJSON Certificate where
  parseJSON =
    Lude.withObject
      "Certificate"
      ( \x ->
          Certificate'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "subjectAlternativeNames" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "eligibleToRenew")
            Lude.<*> (x Lude..:? "requestFailureReason")
            Lude.<*> (x Lude..:? "revokedAt")
            Lude.<*> (x Lude..:? "notBefore")
            Lude.<*> (x Lude..:? "revocationReason")
            Lude.<*> (x Lude..:? "domainName")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "renewalSummary")
            Lude.<*> (x Lude..:? "supportCode")
            Lude.<*> (x Lude..:? "domainValidationRecords" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "inUseResourceCount")
            Lude.<*> (x Lude..:? "issuedAt")
            Lude.<*> (x Lude..:? "keyAlgorithm")
            Lude.<*> (x Lude..:? "serialNumber")
            Lude.<*> (x Lude..:? "issuerCA")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "notAfter")
      )

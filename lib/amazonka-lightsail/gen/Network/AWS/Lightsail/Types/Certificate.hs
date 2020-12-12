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
    cerStatus,
    cerSubjectAlternativeNames,
    cerArn,
    cerCreatedAt,
    cerEligibleToRenew,
    cerRequestFailureReason,
    cerRevokedAt,
    cerNotBefore,
    cerRevocationReason,
    cerDomainName,
    cerName,
    cerRenewalSummary,
    cerSupportCode,
    cerDomainValidationRecords,
    cerInUseResourceCount,
    cerIssuedAt,
    cerKeyAlgorithm,
    cerSerialNumber,
    cerIssuerCA,
    cerTags,
    cerNotAfter,
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
  { status ::
      Lude.Maybe CertificateStatus,
    subjectAlternativeNames :: Lude.Maybe [Lude.Text],
    arn :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    eligibleToRenew :: Lude.Maybe Lude.Text,
    requestFailureReason :: Lude.Maybe Lude.Text,
    revokedAt :: Lude.Maybe Lude.Timestamp,
    notBefore :: Lude.Maybe Lude.Timestamp,
    revocationReason :: Lude.Maybe Lude.Text,
    domainName :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    renewalSummary :: Lude.Maybe RenewalSummary,
    supportCode :: Lude.Maybe Lude.Text,
    domainValidationRecords :: Lude.Maybe [DomainValidationRecord],
    inUseResourceCount :: Lude.Maybe Lude.Int,
    issuedAt :: Lude.Maybe Lude.Timestamp,
    keyAlgorithm :: Lude.Maybe Lude.Text,
    serialNumber :: Lude.Maybe Lude.Text,
    issuerCA :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    notAfter :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Certificate' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the certificate.
-- * 'createdAt' - The timestamp when the certificate was created.
-- * 'domainName' - The domain name of the certificate.
-- * 'domainValidationRecords' - An array of objects that describe the domain validation records of the certificate.
-- * 'eligibleToRenew' - The renewal eligibility of the certificate.
-- * 'inUseResourceCount' - The number of Lightsail resources that the certificate is attached to.
-- * 'issuedAt' - The timestamp when the certificate was issued.
-- * 'issuerCA' - The certificate authority that issued the certificate.
-- * 'keyAlgorithm' - The algorithm used to generate the key pair (the public and private key) of the certificate.
-- * 'name' - The name of the certificate (e.g., @my-certificate@ ).
-- * 'notAfter' - The timestamp when the certificate expires.
-- * 'notBefore' - The timestamp when the certificate is first valid.
-- * 'renewalSummary' - An object that describes the status of the certificate renewal managed by Lightsail.
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
-- * 'revocationReason' - The reason the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
-- * 'revokedAt' - The timestamp when the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
-- * 'serialNumber' - The serial number of the certificate.
-- * 'status' - The validation status of the certificate.
-- * 'subjectAlternativeNames' - An array of strings that specify the alternate domains (e.g., @example2.com@ ) and subdomains (e.g., @blog.example.com@ ) of the certificate.
-- * 'supportCode' - The support code. Include this code in your email to support when you have questions about your Lightsail certificate. This code enables our support team to look up your Lightsail information more easily.
-- * 'tags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
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
cerStatus :: Lens.Lens' Certificate (Lude.Maybe CertificateStatus)
cerStatus = Lens.lens (status :: Certificate -> Lude.Maybe CertificateStatus) (\s a -> s {status = a} :: Certificate)
{-# DEPRECATED cerStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | An array of strings that specify the alternate domains (e.g., @example2.com@ ) and subdomains (e.g., @blog.example.com@ ) of the certificate.
--
-- /Note:/ Consider using 'subjectAlternativeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerSubjectAlternativeNames :: Lens.Lens' Certificate (Lude.Maybe [Lude.Text])
cerSubjectAlternativeNames = Lens.lens (subjectAlternativeNames :: Certificate -> Lude.Maybe [Lude.Text]) (\s a -> s {subjectAlternativeNames = a} :: Certificate)
{-# DEPRECATED cerSubjectAlternativeNames "Use generic-lens or generic-optics with 'subjectAlternativeNames' instead." #-}

-- | The Amazon Resource Name (ARN) of the certificate.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerArn :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cerArn = Lens.lens (arn :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Certificate)
{-# DEPRECATED cerArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The timestamp when the certificate was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerCreatedAt :: Lens.Lens' Certificate (Lude.Maybe Lude.Timestamp)
cerCreatedAt = Lens.lens (createdAt :: Certificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: Certificate)
{-# DEPRECATED cerCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The renewal eligibility of the certificate.
--
-- /Note:/ Consider using 'eligibleToRenew' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerEligibleToRenew :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cerEligibleToRenew = Lens.lens (eligibleToRenew :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {eligibleToRenew = a} :: Certificate)
{-# DEPRECATED cerEligibleToRenew "Use generic-lens or generic-optics with 'eligibleToRenew' instead." #-}

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
cerRequestFailureReason :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cerRequestFailureReason = Lens.lens (requestFailureReason :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {requestFailureReason = a} :: Certificate)
{-# DEPRECATED cerRequestFailureReason "Use generic-lens or generic-optics with 'requestFailureReason' instead." #-}

-- | The timestamp when the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
--
-- /Note:/ Consider using 'revokedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerRevokedAt :: Lens.Lens' Certificate (Lude.Maybe Lude.Timestamp)
cerRevokedAt = Lens.lens (revokedAt :: Certificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {revokedAt = a} :: Certificate)
{-# DEPRECATED cerRevokedAt "Use generic-lens or generic-optics with 'revokedAt' instead." #-}

-- | The timestamp when the certificate is first valid.
--
-- /Note:/ Consider using 'notBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerNotBefore :: Lens.Lens' Certificate (Lude.Maybe Lude.Timestamp)
cerNotBefore = Lens.lens (notBefore :: Certificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {notBefore = a} :: Certificate)
{-# DEPRECATED cerNotBefore "Use generic-lens or generic-optics with 'notBefore' instead." #-}

-- | The reason the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
--
-- /Note:/ Consider using 'revocationReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerRevocationReason :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cerRevocationReason = Lens.lens (revocationReason :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {revocationReason = a} :: Certificate)
{-# DEPRECATED cerRevocationReason "Use generic-lens or generic-optics with 'revocationReason' instead." #-}

-- | The domain name of the certificate.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerDomainName :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cerDomainName = Lens.lens (domainName :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: Certificate)
{-# DEPRECATED cerDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The name of the certificate (e.g., @my-certificate@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerName :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cerName = Lens.lens (name :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Certificate)
{-# DEPRECATED cerName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An object that describes the status of the certificate renewal managed by Lightsail.
--
-- /Note:/ Consider using 'renewalSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerRenewalSummary :: Lens.Lens' Certificate (Lude.Maybe RenewalSummary)
cerRenewalSummary = Lens.lens (renewalSummary :: Certificate -> Lude.Maybe RenewalSummary) (\s a -> s {renewalSummary = a} :: Certificate)
{-# DEPRECATED cerRenewalSummary "Use generic-lens or generic-optics with 'renewalSummary' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about your Lightsail certificate. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerSupportCode :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cerSupportCode = Lens.lens (supportCode :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {supportCode = a} :: Certificate)
{-# DEPRECATED cerSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | An array of objects that describe the domain validation records of the certificate.
--
-- /Note:/ Consider using 'domainValidationRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerDomainValidationRecords :: Lens.Lens' Certificate (Lude.Maybe [DomainValidationRecord])
cerDomainValidationRecords = Lens.lens (domainValidationRecords :: Certificate -> Lude.Maybe [DomainValidationRecord]) (\s a -> s {domainValidationRecords = a} :: Certificate)
{-# DEPRECATED cerDomainValidationRecords "Use generic-lens or generic-optics with 'domainValidationRecords' instead." #-}

-- | The number of Lightsail resources that the certificate is attached to.
--
-- /Note:/ Consider using 'inUseResourceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerInUseResourceCount :: Lens.Lens' Certificate (Lude.Maybe Lude.Int)
cerInUseResourceCount = Lens.lens (inUseResourceCount :: Certificate -> Lude.Maybe Lude.Int) (\s a -> s {inUseResourceCount = a} :: Certificate)
{-# DEPRECATED cerInUseResourceCount "Use generic-lens or generic-optics with 'inUseResourceCount' instead." #-}

-- | The timestamp when the certificate was issued.
--
-- /Note:/ Consider using 'issuedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerIssuedAt :: Lens.Lens' Certificate (Lude.Maybe Lude.Timestamp)
cerIssuedAt = Lens.lens (issuedAt :: Certificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {issuedAt = a} :: Certificate)
{-# DEPRECATED cerIssuedAt "Use generic-lens or generic-optics with 'issuedAt' instead." #-}

-- | The algorithm used to generate the key pair (the public and private key) of the certificate.
--
-- /Note:/ Consider using 'keyAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerKeyAlgorithm :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cerKeyAlgorithm = Lens.lens (keyAlgorithm :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {keyAlgorithm = a} :: Certificate)
{-# DEPRECATED cerKeyAlgorithm "Use generic-lens or generic-optics with 'keyAlgorithm' instead." #-}

-- | The serial number of the certificate.
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerSerialNumber :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cerSerialNumber = Lens.lens (serialNumber :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {serialNumber = a} :: Certificate)
{-# DEPRECATED cerSerialNumber "Use generic-lens or generic-optics with 'serialNumber' instead." #-}

-- | The certificate authority that issued the certificate.
--
-- /Note:/ Consider using 'issuerCA' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerIssuerCA :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cerIssuerCA = Lens.lens (issuerCA :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {issuerCA = a} :: Certificate)
{-# DEPRECATED cerIssuerCA "Use generic-lens or generic-optics with 'issuerCA' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerTags :: Lens.Lens' Certificate (Lude.Maybe [Tag])
cerTags = Lens.lens (tags :: Certificate -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Certificate)
{-# DEPRECATED cerTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The timestamp when the certificate expires.
--
-- /Note:/ Consider using 'notAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerNotAfter :: Lens.Lens' Certificate (Lude.Maybe Lude.Timestamp)
cerNotAfter = Lens.lens (notAfter :: Certificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {notAfter = a} :: Certificate)
{-# DEPRECATED cerNotAfter "Use generic-lens or generic-optics with 'notAfter' instead." #-}

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

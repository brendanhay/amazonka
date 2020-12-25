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
    cfArn,
    cfCreatedAt,
    cfDomainName,
    cfDomainValidationRecords,
    cfEligibleToRenew,
    cfInUseResourceCount,
    cfIssuedAt,
    cfIssuerCA,
    cfKeyAlgorithm,
    cfName,
    cfNotAfter,
    cfNotBefore,
    cfRenewalSummary,
    cfRequestFailureReason,
    cfRevocationReason,
    cfRevokedAt,
    cfSerialNumber,
    cfStatus,
    cfSubjectAlternativeNames,
    cfSupportCode,
    cfTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.CertificateName as Types
import qualified Network.AWS.Lightsail.Types.CertificateStatus as Types
import qualified Network.AWS.Lightsail.Types.DomainName as Types
import qualified Network.AWS.Lightsail.Types.DomainValidationRecord as Types
import qualified Network.AWS.Lightsail.Types.EligibleToRenew as Types
import qualified Network.AWS.Lightsail.Types.IssuerCA as Types
import qualified Network.AWS.Lightsail.Types.KeyAlgorithm as Types
import qualified Network.AWS.Lightsail.Types.NonEmptyString as Types
import qualified Network.AWS.Lightsail.Types.RenewalSummary as Types
import qualified Network.AWS.Lightsail.Types.RequestFailureReason as Types
import qualified Network.AWS.Lightsail.Types.RevocationReason as Types
import qualified Network.AWS.Lightsail.Types.SerialNumber as Types
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Lightsail.Types.Tag as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the full details of an Amazon Lightsail SSL/TLS certificate.
--
-- /See:/ 'mkCertificate' smart constructor.
data Certificate = Certificate'
  { -- | The Amazon Resource Name (ARN) of the certificate.
    arn :: Core.Maybe Types.NonEmptyString,
    -- | The timestamp when the certificate was created.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | The domain name of the certificate.
    domainName :: Core.Maybe Types.DomainName,
    -- | An array of objects that describe the domain validation records of the certificate.
    domainValidationRecords :: Core.Maybe [Types.DomainValidationRecord],
    -- | The renewal eligibility of the certificate.
    eligibleToRenew :: Core.Maybe Types.EligibleToRenew,
    -- | The number of Lightsail resources that the certificate is attached to.
    inUseResourceCount :: Core.Maybe Core.Int,
    -- | The timestamp when the certificate was issued.
    issuedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The certificate authority that issued the certificate.
    issuerCA :: Core.Maybe Types.IssuerCA,
    -- | The algorithm used to generate the key pair (the public and private key) of the certificate.
    keyAlgorithm :: Core.Maybe Types.KeyAlgorithm,
    -- | The name of the certificate (e.g., @my-certificate@ ).
    name :: Core.Maybe Types.CertificateName,
    -- | The timestamp when the certificate expires.
    notAfter :: Core.Maybe Core.NominalDiffTime,
    -- | The timestamp when the certificate is first valid.
    notBefore :: Core.Maybe Core.NominalDiffTime,
    -- | An object that describes the status of the certificate renewal managed by Lightsail.
    renewalSummary :: Core.Maybe Types.RenewalSummary,
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
    requestFailureReason :: Core.Maybe Types.RequestFailureReason,
    -- | The reason the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
    revocationReason :: Core.Maybe Types.RevocationReason,
    -- | The timestamp when the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
    revokedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The serial number of the certificate.
    serialNumber :: Core.Maybe Types.SerialNumber,
    -- | The validation status of the certificate.
    status :: Core.Maybe Types.CertificateStatus,
    -- | An array of strings that specify the alternate domains (e.g., @example2.com@ ) and subdomains (e.g., @blog.example.com@ ) of the certificate.
    subjectAlternativeNames :: Core.Maybe [Types.DomainName],
    -- | The support code. Include this code in your email to support when you have questions about your Lightsail certificate. This code enables our support team to look up your Lightsail information more easily.
    supportCode :: Core.Maybe Types.String,
    -- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Certificate' value with any optional fields omitted.
mkCertificate ::
  Certificate
mkCertificate =
  Certificate'
    { arn = Core.Nothing,
      createdAt = Core.Nothing,
      domainName = Core.Nothing,
      domainValidationRecords = Core.Nothing,
      eligibleToRenew = Core.Nothing,
      inUseResourceCount = Core.Nothing,
      issuedAt = Core.Nothing,
      issuerCA = Core.Nothing,
      keyAlgorithm = Core.Nothing,
      name = Core.Nothing,
      notAfter = Core.Nothing,
      notBefore = Core.Nothing,
      renewalSummary = Core.Nothing,
      requestFailureReason = Core.Nothing,
      revocationReason = Core.Nothing,
      revokedAt = Core.Nothing,
      serialNumber = Core.Nothing,
      status = Core.Nothing,
      subjectAlternativeNames = Core.Nothing,
      supportCode = Core.Nothing,
      tags = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the certificate.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfArn :: Lens.Lens' Certificate (Core.Maybe Types.NonEmptyString)
cfArn = Lens.field @"arn"
{-# DEPRECATED cfArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The timestamp when the certificate was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfCreatedAt :: Lens.Lens' Certificate (Core.Maybe Core.NominalDiffTime)
cfCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED cfCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The domain name of the certificate.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDomainName :: Lens.Lens' Certificate (Core.Maybe Types.DomainName)
cfDomainName = Lens.field @"domainName"
{-# DEPRECATED cfDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | An array of objects that describe the domain validation records of the certificate.
--
-- /Note:/ Consider using 'domainValidationRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfDomainValidationRecords :: Lens.Lens' Certificate (Core.Maybe [Types.DomainValidationRecord])
cfDomainValidationRecords = Lens.field @"domainValidationRecords"
{-# DEPRECATED cfDomainValidationRecords "Use generic-lens or generic-optics with 'domainValidationRecords' instead." #-}

-- | The renewal eligibility of the certificate.
--
-- /Note:/ Consider using 'eligibleToRenew' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfEligibleToRenew :: Lens.Lens' Certificate (Core.Maybe Types.EligibleToRenew)
cfEligibleToRenew = Lens.field @"eligibleToRenew"
{-# DEPRECATED cfEligibleToRenew "Use generic-lens or generic-optics with 'eligibleToRenew' instead." #-}

-- | The number of Lightsail resources that the certificate is attached to.
--
-- /Note:/ Consider using 'inUseResourceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfInUseResourceCount :: Lens.Lens' Certificate (Core.Maybe Core.Int)
cfInUseResourceCount = Lens.field @"inUseResourceCount"
{-# DEPRECATED cfInUseResourceCount "Use generic-lens or generic-optics with 'inUseResourceCount' instead." #-}

-- | The timestamp when the certificate was issued.
--
-- /Note:/ Consider using 'issuedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfIssuedAt :: Lens.Lens' Certificate (Core.Maybe Core.NominalDiffTime)
cfIssuedAt = Lens.field @"issuedAt"
{-# DEPRECATED cfIssuedAt "Use generic-lens or generic-optics with 'issuedAt' instead." #-}

-- | The certificate authority that issued the certificate.
--
-- /Note:/ Consider using 'issuerCA' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfIssuerCA :: Lens.Lens' Certificate (Core.Maybe Types.IssuerCA)
cfIssuerCA = Lens.field @"issuerCA"
{-# DEPRECATED cfIssuerCA "Use generic-lens or generic-optics with 'issuerCA' instead." #-}

-- | The algorithm used to generate the key pair (the public and private key) of the certificate.
--
-- /Note:/ Consider using 'keyAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfKeyAlgorithm :: Lens.Lens' Certificate (Core.Maybe Types.KeyAlgorithm)
cfKeyAlgorithm = Lens.field @"keyAlgorithm"
{-# DEPRECATED cfKeyAlgorithm "Use generic-lens or generic-optics with 'keyAlgorithm' instead." #-}

-- | The name of the certificate (e.g., @my-certificate@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfName :: Lens.Lens' Certificate (Core.Maybe Types.CertificateName)
cfName = Lens.field @"name"
{-# DEPRECATED cfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The timestamp when the certificate expires.
--
-- /Note:/ Consider using 'notAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfNotAfter :: Lens.Lens' Certificate (Core.Maybe Core.NominalDiffTime)
cfNotAfter = Lens.field @"notAfter"
{-# DEPRECATED cfNotAfter "Use generic-lens or generic-optics with 'notAfter' instead." #-}

-- | The timestamp when the certificate is first valid.
--
-- /Note:/ Consider using 'notBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfNotBefore :: Lens.Lens' Certificate (Core.Maybe Core.NominalDiffTime)
cfNotBefore = Lens.field @"notBefore"
{-# DEPRECATED cfNotBefore "Use generic-lens or generic-optics with 'notBefore' instead." #-}

-- | An object that describes the status of the certificate renewal managed by Lightsail.
--
-- /Note:/ Consider using 'renewalSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRenewalSummary :: Lens.Lens' Certificate (Core.Maybe Types.RenewalSummary)
cfRenewalSummary = Lens.field @"renewalSummary"
{-# DEPRECATED cfRenewalSummary "Use generic-lens or generic-optics with 'renewalSummary' instead." #-}

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
cfRequestFailureReason :: Lens.Lens' Certificate (Core.Maybe Types.RequestFailureReason)
cfRequestFailureReason = Lens.field @"requestFailureReason"
{-# DEPRECATED cfRequestFailureReason "Use generic-lens or generic-optics with 'requestFailureReason' instead." #-}

-- | The reason the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
--
-- /Note:/ Consider using 'revocationReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRevocationReason :: Lens.Lens' Certificate (Core.Maybe Types.RevocationReason)
cfRevocationReason = Lens.field @"revocationReason"
{-# DEPRECATED cfRevocationReason "Use generic-lens or generic-optics with 'revocationReason' instead." #-}

-- | The timestamp when the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
--
-- /Note:/ Consider using 'revokedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRevokedAt :: Lens.Lens' Certificate (Core.Maybe Core.NominalDiffTime)
cfRevokedAt = Lens.field @"revokedAt"
{-# DEPRECATED cfRevokedAt "Use generic-lens or generic-optics with 'revokedAt' instead." #-}

-- | The serial number of the certificate.
--
-- /Note:/ Consider using 'serialNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfSerialNumber :: Lens.Lens' Certificate (Core.Maybe Types.SerialNumber)
cfSerialNumber = Lens.field @"serialNumber"
{-# DEPRECATED cfSerialNumber "Use generic-lens or generic-optics with 'serialNumber' instead." #-}

-- | The validation status of the certificate.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfStatus :: Lens.Lens' Certificate (Core.Maybe Types.CertificateStatus)
cfStatus = Lens.field @"status"
{-# DEPRECATED cfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | An array of strings that specify the alternate domains (e.g., @example2.com@ ) and subdomains (e.g., @blog.example.com@ ) of the certificate.
--
-- /Note:/ Consider using 'subjectAlternativeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfSubjectAlternativeNames :: Lens.Lens' Certificate (Core.Maybe [Types.DomainName])
cfSubjectAlternativeNames = Lens.field @"subjectAlternativeNames"
{-# DEPRECATED cfSubjectAlternativeNames "Use generic-lens or generic-optics with 'subjectAlternativeNames' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about your Lightsail certificate. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfSupportCode :: Lens.Lens' Certificate (Core.Maybe Types.String)
cfSupportCode = Lens.field @"supportCode"
{-# DEPRECATED cfSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfTags :: Lens.Lens' Certificate (Core.Maybe [Types.Tag])
cfTags = Lens.field @"tags"
{-# DEPRECATED cfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON Certificate where
  parseJSON =
    Core.withObject "Certificate" Core.$
      \x ->
        Certificate'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "domainName")
          Core.<*> (x Core..:? "domainValidationRecords")
          Core.<*> (x Core..:? "eligibleToRenew")
          Core.<*> (x Core..:? "inUseResourceCount")
          Core.<*> (x Core..:? "issuedAt")
          Core.<*> (x Core..:? "issuerCA")
          Core.<*> (x Core..:? "keyAlgorithm")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "notAfter")
          Core.<*> (x Core..:? "notBefore")
          Core.<*> (x Core..:? "renewalSummary")
          Core.<*> (x Core..:? "requestFailureReason")
          Core.<*> (x Core..:? "revocationReason")
          Core.<*> (x Core..:? "revokedAt")
          Core.<*> (x Core..:? "serialNumber")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "subjectAlternativeNames")
          Core.<*> (x Core..:? "supportCode")
          Core.<*> (x Core..:? "tags")

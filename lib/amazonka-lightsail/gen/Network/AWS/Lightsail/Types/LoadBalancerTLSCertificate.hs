-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTLSCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificate
  ( LoadBalancerTLSCertificate (..),

    -- * Smart constructor
    mkLoadBalancerTLSCertificate,

    -- * Lenses
    lbtcFailureReason,
    lbtcSubject,
    lbtcStatus,
    lbtcSubjectAlternativeNames,
    lbtcResourceType,
    lbtcArn,
    lbtcCreatedAt,
    lbtcLocation,
    lbtcLoadBalancerName,
    lbtcSerial,
    lbtcIsAttached,
    lbtcRevokedAt,
    lbtcNotBefore,
    lbtcRevocationReason,
    lbtcDomainName,
    lbtcName,
    lbtcRenewalSummary,
    lbtcSupportCode,
    lbtcDomainValidationRecords,
    lbtcIssuedAt,
    lbtcKeyAlgorithm,
    lbtcSignatureAlgorithm,
    lbtcIssuer,
    lbtcTags,
    lbtcNotAfter,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainValidationRecord
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateFailureReason
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRenewalSummary
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRevocationReason
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateStatus
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Lude

-- | Describes a load balancer SSL/TLS certificate.
--
-- TLS is just an updated, more secure version of Secure Socket Layer (SSL).
--
-- /See:/ 'mkLoadBalancerTLSCertificate' smart constructor.
data LoadBalancerTLSCertificate = LoadBalancerTLSCertificate'
  { failureReason ::
      Lude.Maybe
        LoadBalancerTLSCertificateFailureReason,
    subject :: Lude.Maybe Lude.Text,
    status ::
      Lude.Maybe
        LoadBalancerTLSCertificateStatus,
    subjectAlternativeNames ::
      Lude.Maybe [Lude.Text],
    resourceType ::
      Lude.Maybe ResourceType,
    arn :: Lude.Maybe Lude.Text,
    createdAt ::
      Lude.Maybe Lude.Timestamp,
    location ::
      Lude.Maybe ResourceLocation,
    loadBalancerName ::
      Lude.Maybe Lude.Text,
    serial :: Lude.Maybe Lude.Text,
    isAttached :: Lude.Maybe Lude.Bool,
    revokedAt ::
      Lude.Maybe Lude.Timestamp,
    notBefore ::
      Lude.Maybe Lude.Timestamp,
    revocationReason ::
      Lude.Maybe
        LoadBalancerTLSCertificateRevocationReason,
    domainName :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    renewalSummary ::
      Lude.Maybe
        LoadBalancerTLSCertificateRenewalSummary,
    supportCode :: Lude.Maybe Lude.Text,
    domainValidationRecords ::
      Lude.Maybe
        [LoadBalancerTLSCertificateDomainValidationRecord],
    issuedAt :: Lude.Maybe Lude.Timestamp,
    keyAlgorithm :: Lude.Maybe Lude.Text,
    signatureAlgorithm ::
      Lude.Maybe Lude.Text,
    issuer :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'LoadBalancerTLSCertificate' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the SSL/TLS certificate.
-- * 'createdAt' - The time when you created your SSL/TLS certificate.
-- * 'domainName' - The domain name for your SSL/TLS certificate.
-- * 'domainValidationRecords' - An array of LoadBalancerTlsCertificateDomainValidationRecord objects describing the records.
-- * 'failureReason' - The validation failure reason, if any, of the certificate.
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
-- * 'isAttached' - When @true@ , the SSL/TLS certificate is attached to the Lightsail load balancer.
-- * 'issuedAt' - The time when the SSL/TLS certificate was issued.
-- * 'issuer' - The issuer of the certificate.
-- * 'keyAlgorithm' - The algorithm used to generate the key pair (the public and private key).
-- * 'loadBalancerName' - The load balancer name where your SSL/TLS certificate is attached.
-- * 'location' - The AWS Region and Availability Zone where you created your certificate.
-- * 'name' - The name of the SSL/TLS certificate (e.g., @my-certificate@ ).
-- * 'notAfter' - The timestamp when the SSL/TLS certificate expires.
-- * 'notBefore' - The timestamp when the SSL/TLS certificate is first valid.
-- * 'renewalSummary' - An object that describes the status of the certificate renewal managed by Lightsail.
-- * 'resourceType' - The resource type (e.g., @LoadBalancerTlsCertificate@ ).
--
--
--     * __@Instance@ __ - A Lightsail instance (a virtual private server)
--
--
--     * __@StaticIp@ __ - A static IP address
--
--
--     * __@KeyPair@ __ - The key pair used to connect to a Lightsail instance
--
--
--     * __@InstanceSnapshot@ __ - A Lightsail instance snapshot
--
--
--     * __@Domain@ __ - A DNS zone
--
--
--     * __@PeeredVpc@ __ - A peered VPC
--
--
--     * __@LoadBalancer@ __ - A Lightsail load balancer
--
--
--     * __@LoadBalancerTlsCertificate@ __ - An SSL/TLS certificate associated with a Lightsail load balancer
--
--
--     * __@Disk@ __ - A Lightsail block storage disk
--
--
--     * __@DiskSnapshot@ __ - A block storage disk snapshot
--
--
-- * 'revocationReason' - The reason the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
-- * 'revokedAt' - The timestamp when the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
-- * 'serial' - The serial number of the certificate.
-- * 'signatureAlgorithm' - The algorithm that was used to sign the certificate.
-- * 'status' - The validation status of the SSL/TLS certificate. Valid values are below.
-- * 'subject' - The name of the entity that is associated with the public key contained in the certificate.
-- * 'subjectAlternativeNames' - An array of strings that specify the alternate domains (e.g., @example2.com@ ) and subdomains (e.g., @blog.example.com@ ) for the certificate.
-- * 'supportCode' - The support code. Include this code in your email to support when you have questions about your Lightsail load balancer or SSL/TLS certificate. This code enables our support team to look up your Lightsail information more easily.
-- * 'tags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
mkLoadBalancerTLSCertificate ::
  LoadBalancerTLSCertificate
mkLoadBalancerTLSCertificate =
  LoadBalancerTLSCertificate'
    { failureReason = Lude.Nothing,
      subject = Lude.Nothing,
      status = Lude.Nothing,
      subjectAlternativeNames = Lude.Nothing,
      resourceType = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      location = Lude.Nothing,
      loadBalancerName = Lude.Nothing,
      serial = Lude.Nothing,
      isAttached = Lude.Nothing,
      revokedAt = Lude.Nothing,
      notBefore = Lude.Nothing,
      revocationReason = Lude.Nothing,
      domainName = Lude.Nothing,
      name = Lude.Nothing,
      renewalSummary = Lude.Nothing,
      supportCode = Lude.Nothing,
      domainValidationRecords = Lude.Nothing,
      issuedAt = Lude.Nothing,
      keyAlgorithm = Lude.Nothing,
      signatureAlgorithm = Lude.Nothing,
      issuer = Lude.Nothing,
      tags = Lude.Nothing,
      notAfter = Lude.Nothing
    }

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
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcFailureReason :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe LoadBalancerTLSCertificateFailureReason)
lbtcFailureReason = Lens.lens (failureReason :: LoadBalancerTLSCertificate -> Lude.Maybe LoadBalancerTLSCertificateFailureReason) (\s a -> s {failureReason = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The name of the entity that is associated with the public key contained in the certificate.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcSubject :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe Lude.Text)
lbtcSubject = Lens.lens (subject :: LoadBalancerTLSCertificate -> Lude.Maybe Lude.Text) (\s a -> s {subject = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcSubject "Use generic-lens or generic-optics with 'subject' instead." #-}

-- | The validation status of the SSL/TLS certificate. Valid values are below.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcStatus :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe LoadBalancerTLSCertificateStatus)
lbtcStatus = Lens.lens (status :: LoadBalancerTLSCertificate -> Lude.Maybe LoadBalancerTLSCertificateStatus) (\s a -> s {status = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | An array of strings that specify the alternate domains (e.g., @example2.com@ ) and subdomains (e.g., @blog.example.com@ ) for the certificate.
--
-- /Note:/ Consider using 'subjectAlternativeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcSubjectAlternativeNames :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe [Lude.Text])
lbtcSubjectAlternativeNames = Lens.lens (subjectAlternativeNames :: LoadBalancerTLSCertificate -> Lude.Maybe [Lude.Text]) (\s a -> s {subjectAlternativeNames = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcSubjectAlternativeNames "Use generic-lens or generic-optics with 'subjectAlternativeNames' instead." #-}

-- | The resource type (e.g., @LoadBalancerTlsCertificate@ ).
--
--
--     * __@Instance@ __ - A Lightsail instance (a virtual private server)
--
--
--     * __@StaticIp@ __ - A static IP address
--
--
--     * __@KeyPair@ __ - The key pair used to connect to a Lightsail instance
--
--
--     * __@InstanceSnapshot@ __ - A Lightsail instance snapshot
--
--
--     * __@Domain@ __ - A DNS zone
--
--
--     * __@PeeredVpc@ __ - A peered VPC
--
--
--     * __@LoadBalancer@ __ - A Lightsail load balancer
--
--
--     * __@LoadBalancerTlsCertificate@ __ - An SSL/TLS certificate associated with a Lightsail load balancer
--
--
--     * __@Disk@ __ - A Lightsail block storage disk
--
--
--     * __@DiskSnapshot@ __ - A block storage disk snapshot
--
--
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcResourceType :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe ResourceType)
lbtcResourceType = Lens.lens (resourceType :: LoadBalancerTLSCertificate -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The Amazon Resource Name (ARN) of the SSL/TLS certificate.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcArn :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe Lude.Text)
lbtcArn = Lens.lens (arn :: LoadBalancerTLSCertificate -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time when you created your SSL/TLS certificate.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcCreatedAt :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe Lude.Timestamp)
lbtcCreatedAt = Lens.lens (createdAt :: LoadBalancerTLSCertificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The AWS Region and Availability Zone where you created your certificate.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcLocation :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe ResourceLocation)
lbtcLocation = Lens.lens (location :: LoadBalancerTLSCertificate -> Lude.Maybe ResourceLocation) (\s a -> s {location = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The load balancer name where your SSL/TLS certificate is attached.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcLoadBalancerName :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe Lude.Text)
lbtcLoadBalancerName = Lens.lens (loadBalancerName :: LoadBalancerTLSCertificate -> Lude.Maybe Lude.Text) (\s a -> s {loadBalancerName = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The serial number of the certificate.
--
-- /Note:/ Consider using 'serial' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcSerial :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe Lude.Text)
lbtcSerial = Lens.lens (serial :: LoadBalancerTLSCertificate -> Lude.Maybe Lude.Text) (\s a -> s {serial = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcSerial "Use generic-lens or generic-optics with 'serial' instead." #-}

-- | When @true@ , the SSL/TLS certificate is attached to the Lightsail load balancer.
--
-- /Note:/ Consider using 'isAttached' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcIsAttached :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe Lude.Bool)
lbtcIsAttached = Lens.lens (isAttached :: LoadBalancerTLSCertificate -> Lude.Maybe Lude.Bool) (\s a -> s {isAttached = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcIsAttached "Use generic-lens or generic-optics with 'isAttached' instead." #-}

-- | The timestamp when the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
--
-- /Note:/ Consider using 'revokedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcRevokedAt :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe Lude.Timestamp)
lbtcRevokedAt = Lens.lens (revokedAt :: LoadBalancerTLSCertificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {revokedAt = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcRevokedAt "Use generic-lens or generic-optics with 'revokedAt' instead." #-}

-- | The timestamp when the SSL/TLS certificate is first valid.
--
-- /Note:/ Consider using 'notBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcNotBefore :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe Lude.Timestamp)
lbtcNotBefore = Lens.lens (notBefore :: LoadBalancerTLSCertificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {notBefore = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcNotBefore "Use generic-lens or generic-optics with 'notBefore' instead." #-}

-- | The reason the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
--
-- /Note:/ Consider using 'revocationReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcRevocationReason :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe LoadBalancerTLSCertificateRevocationReason)
lbtcRevocationReason = Lens.lens (revocationReason :: LoadBalancerTLSCertificate -> Lude.Maybe LoadBalancerTLSCertificateRevocationReason) (\s a -> s {revocationReason = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcRevocationReason "Use generic-lens or generic-optics with 'revocationReason' instead." #-}

-- | The domain name for your SSL/TLS certificate.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcDomainName :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe Lude.Text)
lbtcDomainName = Lens.lens (domainName :: LoadBalancerTLSCertificate -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The name of the SSL/TLS certificate (e.g., @my-certificate@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcName :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe Lude.Text)
lbtcName = Lens.lens (name :: LoadBalancerTLSCertificate -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An object that describes the status of the certificate renewal managed by Lightsail.
--
-- /Note:/ Consider using 'renewalSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcRenewalSummary :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe LoadBalancerTLSCertificateRenewalSummary)
lbtcRenewalSummary = Lens.lens (renewalSummary :: LoadBalancerTLSCertificate -> Lude.Maybe LoadBalancerTLSCertificateRenewalSummary) (\s a -> s {renewalSummary = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcRenewalSummary "Use generic-lens or generic-optics with 'renewalSummary' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about your Lightsail load balancer or SSL/TLS certificate. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcSupportCode :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe Lude.Text)
lbtcSupportCode = Lens.lens (supportCode :: LoadBalancerTLSCertificate -> Lude.Maybe Lude.Text) (\s a -> s {supportCode = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | An array of LoadBalancerTlsCertificateDomainValidationRecord objects describing the records.
--
-- /Note:/ Consider using 'domainValidationRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcDomainValidationRecords :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe [LoadBalancerTLSCertificateDomainValidationRecord])
lbtcDomainValidationRecords = Lens.lens (domainValidationRecords :: LoadBalancerTLSCertificate -> Lude.Maybe [LoadBalancerTLSCertificateDomainValidationRecord]) (\s a -> s {domainValidationRecords = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcDomainValidationRecords "Use generic-lens or generic-optics with 'domainValidationRecords' instead." #-}

-- | The time when the SSL/TLS certificate was issued.
--
-- /Note:/ Consider using 'issuedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcIssuedAt :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe Lude.Timestamp)
lbtcIssuedAt = Lens.lens (issuedAt :: LoadBalancerTLSCertificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {issuedAt = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcIssuedAt "Use generic-lens or generic-optics with 'issuedAt' instead." #-}

-- | The algorithm used to generate the key pair (the public and private key).
--
-- /Note:/ Consider using 'keyAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcKeyAlgorithm :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe Lude.Text)
lbtcKeyAlgorithm = Lens.lens (keyAlgorithm :: LoadBalancerTLSCertificate -> Lude.Maybe Lude.Text) (\s a -> s {keyAlgorithm = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcKeyAlgorithm "Use generic-lens or generic-optics with 'keyAlgorithm' instead." #-}

-- | The algorithm that was used to sign the certificate.
--
-- /Note:/ Consider using 'signatureAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcSignatureAlgorithm :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe Lude.Text)
lbtcSignatureAlgorithm = Lens.lens (signatureAlgorithm :: LoadBalancerTLSCertificate -> Lude.Maybe Lude.Text) (\s a -> s {signatureAlgorithm = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcSignatureAlgorithm "Use generic-lens or generic-optics with 'signatureAlgorithm' instead." #-}

-- | The issuer of the certificate.
--
-- /Note:/ Consider using 'issuer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcIssuer :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe Lude.Text)
lbtcIssuer = Lens.lens (issuer :: LoadBalancerTLSCertificate -> Lude.Maybe Lude.Text) (\s a -> s {issuer = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcIssuer "Use generic-lens or generic-optics with 'issuer' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcTags :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe [Tag])
lbtcTags = Lens.lens (tags :: LoadBalancerTLSCertificate -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The timestamp when the SSL/TLS certificate expires.
--
-- /Note:/ Consider using 'notAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcNotAfter :: Lens.Lens' LoadBalancerTLSCertificate (Lude.Maybe Lude.Timestamp)
lbtcNotAfter = Lens.lens (notAfter :: LoadBalancerTLSCertificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {notAfter = a} :: LoadBalancerTLSCertificate)
{-# DEPRECATED lbtcNotAfter "Use generic-lens or generic-optics with 'notAfter' instead." #-}

instance Lude.FromJSON LoadBalancerTLSCertificate where
  parseJSON =
    Lude.withObject
      "LoadBalancerTLSCertificate"
      ( \x ->
          LoadBalancerTLSCertificate'
            Lude.<$> (x Lude..:? "failureReason")
            Lude.<*> (x Lude..:? "subject")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "subjectAlternativeNames" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "loadBalancerName")
            Lude.<*> (x Lude..:? "serial")
            Lude.<*> (x Lude..:? "isAttached")
            Lude.<*> (x Lude..:? "revokedAt")
            Lude.<*> (x Lude..:? "notBefore")
            Lude.<*> (x Lude..:? "revocationReason")
            Lude.<*> (x Lude..:? "domainName")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "renewalSummary")
            Lude.<*> (x Lude..:? "supportCode")
            Lude.<*> (x Lude..:? "domainValidationRecords" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "issuedAt")
            Lude.<*> (x Lude..:? "keyAlgorithm")
            Lude.<*> (x Lude..:? "signatureAlgorithm")
            Lude.<*> (x Lude..:? "issuer")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "notAfter")
      )

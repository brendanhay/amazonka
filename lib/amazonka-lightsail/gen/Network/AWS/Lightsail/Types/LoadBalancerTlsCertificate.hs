{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTlsCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.LoadBalancerTlsCertificate
  ( LoadBalancerTlsCertificate (..)
  -- * Smart constructor
  , mkLoadBalancerTlsCertificate
  -- * Lenses
  , lbtcArn
  , lbtcCreatedAt
  , lbtcDomainName
  , lbtcDomainValidationRecords
  , lbtcFailureReason
  , lbtcIsAttached
  , lbtcIssuedAt
  , lbtcIssuer
  , lbtcKeyAlgorithm
  , lbtcLoadBalancerName
  , lbtcLocation
  , lbtcName
  , lbtcNotAfter
  , lbtcNotBefore
  , lbtcRenewalSummary
  , lbtcResourceType
  , lbtcRevocationReason
  , lbtcRevokedAt
  , lbtcSerial
  , lbtcSignatureAlgorithm
  , lbtcStatus
  , lbtcSubject
  , lbtcSubjectAlternativeNames
  , lbtcSupportCode
  , lbtcTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.Arn as Types
import qualified Network.AWS.Lightsail.Types.DomainName as Types
import qualified Network.AWS.Lightsail.Types.Issuer as Types
import qualified Network.AWS.Lightsail.Types.KeyAlgorithm as Types
import qualified Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationRecord as Types
import qualified Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateFailureReason as Types
import qualified Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateRenewalSummary as Types
import qualified Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateRevocationReason as Types
import qualified Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateStatus as Types
import qualified Network.AWS.Lightsail.Types.ResourceLocation as Types
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Lightsail.Types.ResourceType as Types
import qualified Network.AWS.Lightsail.Types.Serial as Types
import qualified Network.AWS.Lightsail.Types.SignatureAlgorithm as Types
import qualified Network.AWS.Lightsail.Types.Subject as Types
import qualified Network.AWS.Lightsail.Types.Tag as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a load balancer SSL/TLS certificate.
--
-- TLS is just an updated, more secure version of Secure Socket Layer (SSL).
--
-- /See:/ 'mkLoadBalancerTlsCertificate' smart constructor.
data LoadBalancerTlsCertificate = LoadBalancerTlsCertificate'
  { arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the SSL/TLS certificate.
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when you created your SSL/TLS certificate.
  , domainName :: Core.Maybe Types.DomainName
    -- ^ The domain name for your SSL/TLS certificate.
  , domainValidationRecords :: Core.Maybe [Types.LoadBalancerTlsCertificateDomainValidationRecord]
    -- ^ An array of LoadBalancerTlsCertificateDomainValidationRecord objects describing the records.
  , failureReason :: Core.Maybe Types.LoadBalancerTlsCertificateFailureReason
    -- ^ The validation failure reason, if any, of the certificate.
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
  , isAttached :: Core.Maybe Core.Bool
    -- ^ When @true@ , the SSL/TLS certificate is attached to the Lightsail load balancer.
  , issuedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when the SSL/TLS certificate was issued.
  , issuer :: Core.Maybe Types.Issuer
    -- ^ The issuer of the certificate.
  , keyAlgorithm :: Core.Maybe Types.KeyAlgorithm
    -- ^ The algorithm used to generate the key pair (the public and private key).
  , loadBalancerName :: Core.Maybe Types.ResourceName
    -- ^ The load balancer name where your SSL/TLS certificate is attached.
  , location :: Core.Maybe Types.ResourceLocation
    -- ^ The AWS Region and Availability Zone where you created your certificate.
  , name :: Core.Maybe Types.ResourceName
    -- ^ The name of the SSL/TLS certificate (e.g., @my-certificate@ ).
  , notAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the SSL/TLS certificate expires.
  , notBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the SSL/TLS certificate is first valid.
  , renewalSummary :: Core.Maybe Types.LoadBalancerTlsCertificateRenewalSummary
    -- ^ An object that describes the status of the certificate renewal managed by Lightsail.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The resource type (e.g., @LoadBalancerTlsCertificate@ ).
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
  , revocationReason :: Core.Maybe Types.LoadBalancerTlsCertificateRevocationReason
    -- ^ The reason the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
  , revokedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
  , serial :: Core.Maybe Types.Serial
    -- ^ The serial number of the certificate.
  , signatureAlgorithm :: Core.Maybe Types.SignatureAlgorithm
    -- ^ The algorithm that was used to sign the certificate.
  , status :: Core.Maybe Types.LoadBalancerTlsCertificateStatus
    -- ^ The validation status of the SSL/TLS certificate. Valid values are below.
  , subject :: Core.Maybe Types.Subject
    -- ^ The name of the entity that is associated with the public key contained in the certificate.
  , subjectAlternativeNames :: Core.Maybe [Core.Text]
    -- ^ An array of strings that specify the alternate domains (e.g., @example2.com@ ) and subdomains (e.g., @blog.example.com@ ) for the certificate.
  , supportCode :: Core.Maybe Core.Text
    -- ^ The support code. Include this code in your email to support when you have questions about your Lightsail load balancer or SSL/TLS certificate. This code enables our support team to look up your Lightsail information more easily.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'LoadBalancerTlsCertificate' value with any optional fields omitted.
mkLoadBalancerTlsCertificate
    :: LoadBalancerTlsCertificate
mkLoadBalancerTlsCertificate
  = LoadBalancerTlsCertificate'{arn = Core.Nothing,
                                createdAt = Core.Nothing, domainName = Core.Nothing,
                                domainValidationRecords = Core.Nothing,
                                failureReason = Core.Nothing, isAttached = Core.Nothing,
                                issuedAt = Core.Nothing, issuer = Core.Nothing,
                                keyAlgorithm = Core.Nothing, loadBalancerName = Core.Nothing,
                                location = Core.Nothing, name = Core.Nothing,
                                notAfter = Core.Nothing, notBefore = Core.Nothing,
                                renewalSummary = Core.Nothing, resourceType = Core.Nothing,
                                revocationReason = Core.Nothing, revokedAt = Core.Nothing,
                                serial = Core.Nothing, signatureAlgorithm = Core.Nothing,
                                status = Core.Nothing, subject = Core.Nothing,
                                subjectAlternativeNames = Core.Nothing, supportCode = Core.Nothing,
                                tags = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the SSL/TLS certificate.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcArn :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Types.Arn)
lbtcArn = Lens.field @"arn"
{-# INLINEABLE lbtcArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The time when you created your SSL/TLS certificate.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcCreatedAt :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.NominalDiffTime)
lbtcCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE lbtcCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The domain name for your SSL/TLS certificate.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcDomainName :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Types.DomainName)
lbtcDomainName = Lens.field @"domainName"
{-# INLINEABLE lbtcDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | An array of LoadBalancerTlsCertificateDomainValidationRecord objects describing the records.
--
-- /Note:/ Consider using 'domainValidationRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcDomainValidationRecords :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe [Types.LoadBalancerTlsCertificateDomainValidationRecord])
lbtcDomainValidationRecords = Lens.field @"domainValidationRecords"
{-# INLINEABLE lbtcDomainValidationRecords #-}
{-# DEPRECATED domainValidationRecords "Use generic-lens or generic-optics with 'domainValidationRecords' instead"  #-}

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
lbtcFailureReason :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Types.LoadBalancerTlsCertificateFailureReason)
lbtcFailureReason = Lens.field @"failureReason"
{-# INLINEABLE lbtcFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | When @true@ , the SSL/TLS certificate is attached to the Lightsail load balancer.
--
-- /Note:/ Consider using 'isAttached' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcIsAttached :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.Bool)
lbtcIsAttached = Lens.field @"isAttached"
{-# INLINEABLE lbtcIsAttached #-}
{-# DEPRECATED isAttached "Use generic-lens or generic-optics with 'isAttached' instead"  #-}

-- | The time when the SSL/TLS certificate was issued.
--
-- /Note:/ Consider using 'issuedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcIssuedAt :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.NominalDiffTime)
lbtcIssuedAt = Lens.field @"issuedAt"
{-# INLINEABLE lbtcIssuedAt #-}
{-# DEPRECATED issuedAt "Use generic-lens or generic-optics with 'issuedAt' instead"  #-}

-- | The issuer of the certificate.
--
-- /Note:/ Consider using 'issuer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcIssuer :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Types.Issuer)
lbtcIssuer = Lens.field @"issuer"
{-# INLINEABLE lbtcIssuer #-}
{-# DEPRECATED issuer "Use generic-lens or generic-optics with 'issuer' instead"  #-}

-- | The algorithm used to generate the key pair (the public and private key).
--
-- /Note:/ Consider using 'keyAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcKeyAlgorithm :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Types.KeyAlgorithm)
lbtcKeyAlgorithm = Lens.field @"keyAlgorithm"
{-# INLINEABLE lbtcKeyAlgorithm #-}
{-# DEPRECATED keyAlgorithm "Use generic-lens or generic-optics with 'keyAlgorithm' instead"  #-}

-- | The load balancer name where your SSL/TLS certificate is attached.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcLoadBalancerName :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Types.ResourceName)
lbtcLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE lbtcLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

-- | The AWS Region and Availability Zone where you created your certificate.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcLocation :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Types.ResourceLocation)
lbtcLocation = Lens.field @"location"
{-# INLINEABLE lbtcLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The name of the SSL/TLS certificate (e.g., @my-certificate@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcName :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Types.ResourceName)
lbtcName = Lens.field @"name"
{-# INLINEABLE lbtcName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The timestamp when the SSL/TLS certificate expires.
--
-- /Note:/ Consider using 'notAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcNotAfter :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.NominalDiffTime)
lbtcNotAfter = Lens.field @"notAfter"
{-# INLINEABLE lbtcNotAfter #-}
{-# DEPRECATED notAfter "Use generic-lens or generic-optics with 'notAfter' instead"  #-}

-- | The timestamp when the SSL/TLS certificate is first valid.
--
-- /Note:/ Consider using 'notBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcNotBefore :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.NominalDiffTime)
lbtcNotBefore = Lens.field @"notBefore"
{-# INLINEABLE lbtcNotBefore #-}
{-# DEPRECATED notBefore "Use generic-lens or generic-optics with 'notBefore' instead"  #-}

-- | An object that describes the status of the certificate renewal managed by Lightsail.
--
-- /Note:/ Consider using 'renewalSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcRenewalSummary :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Types.LoadBalancerTlsCertificateRenewalSummary)
lbtcRenewalSummary = Lens.field @"renewalSummary"
{-# INLINEABLE lbtcRenewalSummary #-}
{-# DEPRECATED renewalSummary "Use generic-lens or generic-optics with 'renewalSummary' instead"  #-}

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
lbtcResourceType :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Types.ResourceType)
lbtcResourceType = Lens.field @"resourceType"
{-# INLINEABLE lbtcResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The reason the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
--
-- /Note:/ Consider using 'revocationReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcRevocationReason :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Types.LoadBalancerTlsCertificateRevocationReason)
lbtcRevocationReason = Lens.field @"revocationReason"
{-# INLINEABLE lbtcRevocationReason #-}
{-# DEPRECATED revocationReason "Use generic-lens or generic-optics with 'revocationReason' instead"  #-}

-- | The timestamp when the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
--
-- /Note:/ Consider using 'revokedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcRevokedAt :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.NominalDiffTime)
lbtcRevokedAt = Lens.field @"revokedAt"
{-# INLINEABLE lbtcRevokedAt #-}
{-# DEPRECATED revokedAt "Use generic-lens or generic-optics with 'revokedAt' instead"  #-}

-- | The serial number of the certificate.
--
-- /Note:/ Consider using 'serial' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcSerial :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Types.Serial)
lbtcSerial = Lens.field @"serial"
{-# INLINEABLE lbtcSerial #-}
{-# DEPRECATED serial "Use generic-lens or generic-optics with 'serial' instead"  #-}

-- | The algorithm that was used to sign the certificate.
--
-- /Note:/ Consider using 'signatureAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcSignatureAlgorithm :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Types.SignatureAlgorithm)
lbtcSignatureAlgorithm = Lens.field @"signatureAlgorithm"
{-# INLINEABLE lbtcSignatureAlgorithm #-}
{-# DEPRECATED signatureAlgorithm "Use generic-lens or generic-optics with 'signatureAlgorithm' instead"  #-}

-- | The validation status of the SSL/TLS certificate. Valid values are below.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcStatus :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Types.LoadBalancerTlsCertificateStatus)
lbtcStatus = Lens.field @"status"
{-# INLINEABLE lbtcStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The name of the entity that is associated with the public key contained in the certificate.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcSubject :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Types.Subject)
lbtcSubject = Lens.field @"subject"
{-# INLINEABLE lbtcSubject #-}
{-# DEPRECATED subject "Use generic-lens or generic-optics with 'subject' instead"  #-}

-- | An array of strings that specify the alternate domains (e.g., @example2.com@ ) and subdomains (e.g., @blog.example.com@ ) for the certificate.
--
-- /Note:/ Consider using 'subjectAlternativeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcSubjectAlternativeNames :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe [Core.Text])
lbtcSubjectAlternativeNames = Lens.field @"subjectAlternativeNames"
{-# INLINEABLE lbtcSubjectAlternativeNames #-}
{-# DEPRECATED subjectAlternativeNames "Use generic-lens or generic-optics with 'subjectAlternativeNames' instead"  #-}

-- | The support code. Include this code in your email to support when you have questions about your Lightsail load balancer or SSL/TLS certificate. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcSupportCode :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.Text)
lbtcSupportCode = Lens.field @"supportCode"
{-# INLINEABLE lbtcSupportCode #-}
{-# DEPRECATED supportCode "Use generic-lens or generic-optics with 'supportCode' instead"  #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcTags :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe [Types.Tag])
lbtcTags = Lens.field @"tags"
{-# INLINEABLE lbtcTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON LoadBalancerTlsCertificate where
        parseJSON
          = Core.withObject "LoadBalancerTlsCertificate" Core.$
              \ x ->
                LoadBalancerTlsCertificate' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "createdAt" Core.<*>
                    x Core..:? "domainName"
                    Core.<*> x Core..:? "domainValidationRecords"
                    Core.<*> x Core..:? "failureReason"
                    Core.<*> x Core..:? "isAttached"
                    Core.<*> x Core..:? "issuedAt"
                    Core.<*> x Core..:? "issuer"
                    Core.<*> x Core..:? "keyAlgorithm"
                    Core.<*> x Core..:? "loadBalancerName"
                    Core.<*> x Core..:? "location"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "notAfter"
                    Core.<*> x Core..:? "notBefore"
                    Core.<*> x Core..:? "renewalSummary"
                    Core.<*> x Core..:? "resourceType"
                    Core.<*> x Core..:? "revocationReason"
                    Core.<*> x Core..:? "revokedAt"
                    Core.<*> x Core..:? "serial"
                    Core.<*> x Core..:? "signatureAlgorithm"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "subject"
                    Core.<*> x Core..:? "subjectAlternativeNames"
                    Core.<*> x Core..:? "supportCode"
                    Core.<*> x Core..:? "tags"

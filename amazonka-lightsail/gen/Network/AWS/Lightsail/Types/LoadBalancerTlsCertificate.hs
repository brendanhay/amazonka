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
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTlsCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTlsCertificate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationRecord
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateFailureReason
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateRenewalSummary
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateRevocationReason
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateStatus
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag

-- | Describes a load balancer SSL\/TLS certificate.
--
-- TLS is just an updated, more secure version of Secure Socket Layer
-- (SSL).
--
-- /See:/ 'newLoadBalancerTlsCertificate' smart constructor.
data LoadBalancerTlsCertificate = LoadBalancerTlsCertificate'
  { -- | The validation status of the SSL\/TLS certificate. Valid values are
    -- below.
    status :: Core.Maybe LoadBalancerTlsCertificateStatus,
    -- | The timestamp when the SSL\/TLS certificate is first valid.
    notBefore :: Core.Maybe Core.POSIX,
    -- | The serial number of the certificate.
    serial :: Core.Maybe Core.Text,
    -- | When @true@, the SSL\/TLS certificate is attached to the Lightsail load
    -- balancer.
    isAttached :: Core.Maybe Core.Bool,
    -- | The time when you created your SSL\/TLS certificate.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the SSL\/TLS certificate.
    arn :: Core.Maybe Core.Text,
    -- | The resource type (e.g., @LoadBalancerTlsCertificate@).
    --
    -- -   __@Instance@__ - A Lightsail instance (a virtual private server)
    --
    -- -   __@StaticIp@__ - A static IP address
    --
    -- -   __@KeyPair@__ - The key pair used to connect to a Lightsail instance
    --
    -- -   __@InstanceSnapshot@__ - A Lightsail instance snapshot
    --
    -- -   __@Domain@__ - A DNS zone
    --
    -- -   __@PeeredVpc@__ - A peered VPC
    --
    -- -   __@LoadBalancer@__ - A Lightsail load balancer
    --
    -- -   __@LoadBalancerTlsCertificate@__ - An SSL\/TLS certificate
    --     associated with a Lightsail load balancer
    --
    -- -   __@Disk@__ - A Lightsail block storage disk
    --
    -- -   __@DiskSnapshot@__ - A block storage disk snapshot
    resourceType :: Core.Maybe ResourceType,
    -- | The support code. Include this code in your email to support when you
    -- have questions about your Lightsail load balancer or SSL\/TLS
    -- certificate. This code enables our support team to look up your
    -- Lightsail information more easily.
    supportCode :: Core.Maybe Core.Text,
    -- | An array of strings that specify the alternate domains (e.g.,
    -- @example2.com@) and subdomains (e.g., @blog.example.com@) for the
    -- certificate.
    subjectAlternativeNames :: Core.Maybe [Core.Text],
    -- | The name of the SSL\/TLS certificate (e.g., @my-certificate@).
    name :: Core.Maybe Core.Text,
    -- | The domain name for your SSL\/TLS certificate.
    domainName :: Core.Maybe Core.Text,
    -- | The name of the entity that is associated with the public key contained
    -- in the certificate.
    subject :: Core.Maybe Core.Text,
    -- | The validation failure reason, if any, of the certificate.
    --
    -- The following failure reasons are possible:
    --
    -- -   __@NO_AVAILABLE_CONTACTS@__ - This failure applies to email
    --     validation, which is not available for Lightsail certificates.
    --
    -- -   __@ADDITIONAL_VERIFICATION_REQUIRED@__ - Lightsail requires
    --     additional information to process this certificate request. This can
    --     happen as a fraud-protection measure, such as when the domain ranks
    --     within the Alexa top 1000 websites. To provide the required
    --     information, use the
    --     <https://console.aws.amazon.com/support/home AWS Support Center> to
    --     contact AWS Support.
    --
    --     You cannot request a certificate for Amazon-owned domain names such
    --     as those ending in amazonaws.com, cloudfront.net, or
    --     elasticbeanstalk.com.
    --
    -- -   __@DOMAIN_NOT_ALLOWED@__ - One or more of the domain names in the
    --     certificate request was reported as an unsafe domain by
    --     <https://www.virustotal.com/gui/home/url VirusTotal>. To correct the
    --     problem, search for your domain name on the
    --     <https://www.virustotal.com/gui/home/url VirusTotal> website. If
    --     your domain is reported as suspicious, see
    --     <https://developers.google.com/web/fundamentals/security/hacked Google Help for Hacked Websites>
    --     to learn what you can do.
    --
    --     If you believe that the result is a false positive, notify the
    --     organization that is reporting the domain. VirusTotal is an
    --     aggregate of several antivirus and URL scanners and cannot remove
    --     your domain from a block list itself. After you correct the problem
    --     and the VirusTotal registry has been updated, request a new
    --     certificate.
    --
    --     If you see this error and your domain is not included in the
    --     VirusTotal list, visit the
    --     <https://console.aws.amazon.com/support/home AWS Support Center> and
    --     create a case.
    --
    -- -   __@INVALID_PUBLIC_DOMAIN@__ - One or more of the domain names in the
    --     certificate request is not valid. Typically, this is because a
    --     domain name in the request is not a valid top-level domain. Try to
    --     request a certificate again, correcting any spelling errors or typos
    --     that were in the failed request, and ensure that all domain names in
    --     the request are for valid top-level domains. For example, you cannot
    --     request a certificate for @example.invalidpublicdomain@ because
    --     @invalidpublicdomain@ is not a valid top-level domain.
    --
    -- -   __@OTHER@__ - Typically, this failure occurs when there is a
    --     typographical error in one or more of the domain names in the
    --     certificate request. Try to request a certificate again, correcting
    --     any spelling errors or typos that were in the failed request.
    failureReason :: Core.Maybe LoadBalancerTlsCertificateFailureReason,
    -- | The reason the certificate was revoked. This value is present only when
    -- the certificate status is @REVOKED@.
    revocationReason :: Core.Maybe LoadBalancerTlsCertificateRevocationReason,
    -- | The timestamp when the certificate was revoked. This value is present
    -- only when the certificate status is @REVOKED@.
    revokedAt :: Core.Maybe Core.POSIX,
    -- | The timestamp when the SSL\/TLS certificate expires.
    notAfter :: Core.Maybe Core.POSIX,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
    tags :: Core.Maybe [Tag],
    -- | The algorithm that was used to sign the certificate.
    signatureAlgorithm :: Core.Maybe Core.Text,
    -- | The issuer of the certificate.
    issuer :: Core.Maybe Core.Text,
    -- | The time when the SSL\/TLS certificate was issued.
    issuedAt :: Core.Maybe Core.POSIX,
    -- | The algorithm used to generate the key pair (the public and private
    -- key).
    keyAlgorithm :: Core.Maybe Core.Text,
    -- | An array of LoadBalancerTlsCertificateDomainValidationRecord objects
    -- describing the records.
    domainValidationRecords :: Core.Maybe [LoadBalancerTlsCertificateDomainValidationRecord],
    -- | The AWS Region and Availability Zone where you created your certificate.
    location :: Core.Maybe ResourceLocation,
    -- | The load balancer name where your SSL\/TLS certificate is attached.
    loadBalancerName :: Core.Maybe Core.Text,
    -- | An object that describes the status of the certificate renewal managed
    -- by Lightsail.
    renewalSummary :: Core.Maybe LoadBalancerTlsCertificateRenewalSummary
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LoadBalancerTlsCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'loadBalancerTlsCertificate_status' - The validation status of the SSL\/TLS certificate. Valid values are
-- below.
--
-- 'notBefore', 'loadBalancerTlsCertificate_notBefore' - The timestamp when the SSL\/TLS certificate is first valid.
--
-- 'serial', 'loadBalancerTlsCertificate_serial' - The serial number of the certificate.
--
-- 'isAttached', 'loadBalancerTlsCertificate_isAttached' - When @true@, the SSL\/TLS certificate is attached to the Lightsail load
-- balancer.
--
-- 'createdAt', 'loadBalancerTlsCertificate_createdAt' - The time when you created your SSL\/TLS certificate.
--
-- 'arn', 'loadBalancerTlsCertificate_arn' - The Amazon Resource Name (ARN) of the SSL\/TLS certificate.
--
-- 'resourceType', 'loadBalancerTlsCertificate_resourceType' - The resource type (e.g., @LoadBalancerTlsCertificate@).
--
-- -   __@Instance@__ - A Lightsail instance (a virtual private server)
--
-- -   __@StaticIp@__ - A static IP address
--
-- -   __@KeyPair@__ - The key pair used to connect to a Lightsail instance
--
-- -   __@InstanceSnapshot@__ - A Lightsail instance snapshot
--
-- -   __@Domain@__ - A DNS zone
--
-- -   __@PeeredVpc@__ - A peered VPC
--
-- -   __@LoadBalancer@__ - A Lightsail load balancer
--
-- -   __@LoadBalancerTlsCertificate@__ - An SSL\/TLS certificate
--     associated with a Lightsail load balancer
--
-- -   __@Disk@__ - A Lightsail block storage disk
--
-- -   __@DiskSnapshot@__ - A block storage disk snapshot
--
-- 'supportCode', 'loadBalancerTlsCertificate_supportCode' - The support code. Include this code in your email to support when you
-- have questions about your Lightsail load balancer or SSL\/TLS
-- certificate. This code enables our support team to look up your
-- Lightsail information more easily.
--
-- 'subjectAlternativeNames', 'loadBalancerTlsCertificate_subjectAlternativeNames' - An array of strings that specify the alternate domains (e.g.,
-- @example2.com@) and subdomains (e.g., @blog.example.com@) for the
-- certificate.
--
-- 'name', 'loadBalancerTlsCertificate_name' - The name of the SSL\/TLS certificate (e.g., @my-certificate@).
--
-- 'domainName', 'loadBalancerTlsCertificate_domainName' - The domain name for your SSL\/TLS certificate.
--
-- 'subject', 'loadBalancerTlsCertificate_subject' - The name of the entity that is associated with the public key contained
-- in the certificate.
--
-- 'failureReason', 'loadBalancerTlsCertificate_failureReason' - The validation failure reason, if any, of the certificate.
--
-- The following failure reasons are possible:
--
-- -   __@NO_AVAILABLE_CONTACTS@__ - This failure applies to email
--     validation, which is not available for Lightsail certificates.
--
-- -   __@ADDITIONAL_VERIFICATION_REQUIRED@__ - Lightsail requires
--     additional information to process this certificate request. This can
--     happen as a fraud-protection measure, such as when the domain ranks
--     within the Alexa top 1000 websites. To provide the required
--     information, use the
--     <https://console.aws.amazon.com/support/home AWS Support Center> to
--     contact AWS Support.
--
--     You cannot request a certificate for Amazon-owned domain names such
--     as those ending in amazonaws.com, cloudfront.net, or
--     elasticbeanstalk.com.
--
-- -   __@DOMAIN_NOT_ALLOWED@__ - One or more of the domain names in the
--     certificate request was reported as an unsafe domain by
--     <https://www.virustotal.com/gui/home/url VirusTotal>. To correct the
--     problem, search for your domain name on the
--     <https://www.virustotal.com/gui/home/url VirusTotal> website. If
--     your domain is reported as suspicious, see
--     <https://developers.google.com/web/fundamentals/security/hacked Google Help for Hacked Websites>
--     to learn what you can do.
--
--     If you believe that the result is a false positive, notify the
--     organization that is reporting the domain. VirusTotal is an
--     aggregate of several antivirus and URL scanners and cannot remove
--     your domain from a block list itself. After you correct the problem
--     and the VirusTotal registry has been updated, request a new
--     certificate.
--
--     If you see this error and your domain is not included in the
--     VirusTotal list, visit the
--     <https://console.aws.amazon.com/support/home AWS Support Center> and
--     create a case.
--
-- -   __@INVALID_PUBLIC_DOMAIN@__ - One or more of the domain names in the
--     certificate request is not valid. Typically, this is because a
--     domain name in the request is not a valid top-level domain. Try to
--     request a certificate again, correcting any spelling errors or typos
--     that were in the failed request, and ensure that all domain names in
--     the request are for valid top-level domains. For example, you cannot
--     request a certificate for @example.invalidpublicdomain@ because
--     @invalidpublicdomain@ is not a valid top-level domain.
--
-- -   __@OTHER@__ - Typically, this failure occurs when there is a
--     typographical error in one or more of the domain names in the
--     certificate request. Try to request a certificate again, correcting
--     any spelling errors or typos that were in the failed request.
--
-- 'revocationReason', 'loadBalancerTlsCertificate_revocationReason' - The reason the certificate was revoked. This value is present only when
-- the certificate status is @REVOKED@.
--
-- 'revokedAt', 'loadBalancerTlsCertificate_revokedAt' - The timestamp when the certificate was revoked. This value is present
-- only when the certificate status is @REVOKED@.
--
-- 'notAfter', 'loadBalancerTlsCertificate_notAfter' - The timestamp when the SSL\/TLS certificate expires.
--
-- 'tags', 'loadBalancerTlsCertificate_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
--
-- 'signatureAlgorithm', 'loadBalancerTlsCertificate_signatureAlgorithm' - The algorithm that was used to sign the certificate.
--
-- 'issuer', 'loadBalancerTlsCertificate_issuer' - The issuer of the certificate.
--
-- 'issuedAt', 'loadBalancerTlsCertificate_issuedAt' - The time when the SSL\/TLS certificate was issued.
--
-- 'keyAlgorithm', 'loadBalancerTlsCertificate_keyAlgorithm' - The algorithm used to generate the key pair (the public and private
-- key).
--
-- 'domainValidationRecords', 'loadBalancerTlsCertificate_domainValidationRecords' - An array of LoadBalancerTlsCertificateDomainValidationRecord objects
-- describing the records.
--
-- 'location', 'loadBalancerTlsCertificate_location' - The AWS Region and Availability Zone where you created your certificate.
--
-- 'loadBalancerName', 'loadBalancerTlsCertificate_loadBalancerName' - The load balancer name where your SSL\/TLS certificate is attached.
--
-- 'renewalSummary', 'loadBalancerTlsCertificate_renewalSummary' - An object that describes the status of the certificate renewal managed
-- by Lightsail.
newLoadBalancerTlsCertificate ::
  LoadBalancerTlsCertificate
newLoadBalancerTlsCertificate =
  LoadBalancerTlsCertificate'
    { status = Core.Nothing,
      notBefore = Core.Nothing,
      serial = Core.Nothing,
      isAttached = Core.Nothing,
      createdAt = Core.Nothing,
      arn = Core.Nothing,
      resourceType = Core.Nothing,
      supportCode = Core.Nothing,
      subjectAlternativeNames = Core.Nothing,
      name = Core.Nothing,
      domainName = Core.Nothing,
      subject = Core.Nothing,
      failureReason = Core.Nothing,
      revocationReason = Core.Nothing,
      revokedAt = Core.Nothing,
      notAfter = Core.Nothing,
      tags = Core.Nothing,
      signatureAlgorithm = Core.Nothing,
      issuer = Core.Nothing,
      issuedAt = Core.Nothing,
      keyAlgorithm = Core.Nothing,
      domainValidationRecords = Core.Nothing,
      location = Core.Nothing,
      loadBalancerName = Core.Nothing,
      renewalSummary = Core.Nothing
    }

-- | The validation status of the SSL\/TLS certificate. Valid values are
-- below.
loadBalancerTlsCertificate_status :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe LoadBalancerTlsCertificateStatus)
loadBalancerTlsCertificate_status = Lens.lens (\LoadBalancerTlsCertificate' {status} -> status) (\s@LoadBalancerTlsCertificate' {} a -> s {status = a} :: LoadBalancerTlsCertificate)

-- | The timestamp when the SSL\/TLS certificate is first valid.
loadBalancerTlsCertificate_notBefore :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.UTCTime)
loadBalancerTlsCertificate_notBefore = Lens.lens (\LoadBalancerTlsCertificate' {notBefore} -> notBefore) (\s@LoadBalancerTlsCertificate' {} a -> s {notBefore = a} :: LoadBalancerTlsCertificate) Core.. Lens.mapping Core._Time

-- | The serial number of the certificate.
loadBalancerTlsCertificate_serial :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.Text)
loadBalancerTlsCertificate_serial = Lens.lens (\LoadBalancerTlsCertificate' {serial} -> serial) (\s@LoadBalancerTlsCertificate' {} a -> s {serial = a} :: LoadBalancerTlsCertificate)

-- | When @true@, the SSL\/TLS certificate is attached to the Lightsail load
-- balancer.
loadBalancerTlsCertificate_isAttached :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.Bool)
loadBalancerTlsCertificate_isAttached = Lens.lens (\LoadBalancerTlsCertificate' {isAttached} -> isAttached) (\s@LoadBalancerTlsCertificate' {} a -> s {isAttached = a} :: LoadBalancerTlsCertificate)

-- | The time when you created your SSL\/TLS certificate.
loadBalancerTlsCertificate_createdAt :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.UTCTime)
loadBalancerTlsCertificate_createdAt = Lens.lens (\LoadBalancerTlsCertificate' {createdAt} -> createdAt) (\s@LoadBalancerTlsCertificate' {} a -> s {createdAt = a} :: LoadBalancerTlsCertificate) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the SSL\/TLS certificate.
loadBalancerTlsCertificate_arn :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.Text)
loadBalancerTlsCertificate_arn = Lens.lens (\LoadBalancerTlsCertificate' {arn} -> arn) (\s@LoadBalancerTlsCertificate' {} a -> s {arn = a} :: LoadBalancerTlsCertificate)

-- | The resource type (e.g., @LoadBalancerTlsCertificate@).
--
-- -   __@Instance@__ - A Lightsail instance (a virtual private server)
--
-- -   __@StaticIp@__ - A static IP address
--
-- -   __@KeyPair@__ - The key pair used to connect to a Lightsail instance
--
-- -   __@InstanceSnapshot@__ - A Lightsail instance snapshot
--
-- -   __@Domain@__ - A DNS zone
--
-- -   __@PeeredVpc@__ - A peered VPC
--
-- -   __@LoadBalancer@__ - A Lightsail load balancer
--
-- -   __@LoadBalancerTlsCertificate@__ - An SSL\/TLS certificate
--     associated with a Lightsail load balancer
--
-- -   __@Disk@__ - A Lightsail block storage disk
--
-- -   __@DiskSnapshot@__ - A block storage disk snapshot
loadBalancerTlsCertificate_resourceType :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe ResourceType)
loadBalancerTlsCertificate_resourceType = Lens.lens (\LoadBalancerTlsCertificate' {resourceType} -> resourceType) (\s@LoadBalancerTlsCertificate' {} a -> s {resourceType = a} :: LoadBalancerTlsCertificate)

-- | The support code. Include this code in your email to support when you
-- have questions about your Lightsail load balancer or SSL\/TLS
-- certificate. This code enables our support team to look up your
-- Lightsail information more easily.
loadBalancerTlsCertificate_supportCode :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.Text)
loadBalancerTlsCertificate_supportCode = Lens.lens (\LoadBalancerTlsCertificate' {supportCode} -> supportCode) (\s@LoadBalancerTlsCertificate' {} a -> s {supportCode = a} :: LoadBalancerTlsCertificate)

-- | An array of strings that specify the alternate domains (e.g.,
-- @example2.com@) and subdomains (e.g., @blog.example.com@) for the
-- certificate.
loadBalancerTlsCertificate_subjectAlternativeNames :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe [Core.Text])
loadBalancerTlsCertificate_subjectAlternativeNames = Lens.lens (\LoadBalancerTlsCertificate' {subjectAlternativeNames} -> subjectAlternativeNames) (\s@LoadBalancerTlsCertificate' {} a -> s {subjectAlternativeNames = a} :: LoadBalancerTlsCertificate) Core.. Lens.mapping Lens._Coerce

-- | The name of the SSL\/TLS certificate (e.g., @my-certificate@).
loadBalancerTlsCertificate_name :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.Text)
loadBalancerTlsCertificate_name = Lens.lens (\LoadBalancerTlsCertificate' {name} -> name) (\s@LoadBalancerTlsCertificate' {} a -> s {name = a} :: LoadBalancerTlsCertificate)

-- | The domain name for your SSL\/TLS certificate.
loadBalancerTlsCertificate_domainName :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.Text)
loadBalancerTlsCertificate_domainName = Lens.lens (\LoadBalancerTlsCertificate' {domainName} -> domainName) (\s@LoadBalancerTlsCertificate' {} a -> s {domainName = a} :: LoadBalancerTlsCertificate)

-- | The name of the entity that is associated with the public key contained
-- in the certificate.
loadBalancerTlsCertificate_subject :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.Text)
loadBalancerTlsCertificate_subject = Lens.lens (\LoadBalancerTlsCertificate' {subject} -> subject) (\s@LoadBalancerTlsCertificate' {} a -> s {subject = a} :: LoadBalancerTlsCertificate)

-- | The validation failure reason, if any, of the certificate.
--
-- The following failure reasons are possible:
--
-- -   __@NO_AVAILABLE_CONTACTS@__ - This failure applies to email
--     validation, which is not available for Lightsail certificates.
--
-- -   __@ADDITIONAL_VERIFICATION_REQUIRED@__ - Lightsail requires
--     additional information to process this certificate request. This can
--     happen as a fraud-protection measure, such as when the domain ranks
--     within the Alexa top 1000 websites. To provide the required
--     information, use the
--     <https://console.aws.amazon.com/support/home AWS Support Center> to
--     contact AWS Support.
--
--     You cannot request a certificate for Amazon-owned domain names such
--     as those ending in amazonaws.com, cloudfront.net, or
--     elasticbeanstalk.com.
--
-- -   __@DOMAIN_NOT_ALLOWED@__ - One or more of the domain names in the
--     certificate request was reported as an unsafe domain by
--     <https://www.virustotal.com/gui/home/url VirusTotal>. To correct the
--     problem, search for your domain name on the
--     <https://www.virustotal.com/gui/home/url VirusTotal> website. If
--     your domain is reported as suspicious, see
--     <https://developers.google.com/web/fundamentals/security/hacked Google Help for Hacked Websites>
--     to learn what you can do.
--
--     If you believe that the result is a false positive, notify the
--     organization that is reporting the domain. VirusTotal is an
--     aggregate of several antivirus and URL scanners and cannot remove
--     your domain from a block list itself. After you correct the problem
--     and the VirusTotal registry has been updated, request a new
--     certificate.
--
--     If you see this error and your domain is not included in the
--     VirusTotal list, visit the
--     <https://console.aws.amazon.com/support/home AWS Support Center> and
--     create a case.
--
-- -   __@INVALID_PUBLIC_DOMAIN@__ - One or more of the domain names in the
--     certificate request is not valid. Typically, this is because a
--     domain name in the request is not a valid top-level domain. Try to
--     request a certificate again, correcting any spelling errors or typos
--     that were in the failed request, and ensure that all domain names in
--     the request are for valid top-level domains. For example, you cannot
--     request a certificate for @example.invalidpublicdomain@ because
--     @invalidpublicdomain@ is not a valid top-level domain.
--
-- -   __@OTHER@__ - Typically, this failure occurs when there is a
--     typographical error in one or more of the domain names in the
--     certificate request. Try to request a certificate again, correcting
--     any spelling errors or typos that were in the failed request.
loadBalancerTlsCertificate_failureReason :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe LoadBalancerTlsCertificateFailureReason)
loadBalancerTlsCertificate_failureReason = Lens.lens (\LoadBalancerTlsCertificate' {failureReason} -> failureReason) (\s@LoadBalancerTlsCertificate' {} a -> s {failureReason = a} :: LoadBalancerTlsCertificate)

-- | The reason the certificate was revoked. This value is present only when
-- the certificate status is @REVOKED@.
loadBalancerTlsCertificate_revocationReason :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe LoadBalancerTlsCertificateRevocationReason)
loadBalancerTlsCertificate_revocationReason = Lens.lens (\LoadBalancerTlsCertificate' {revocationReason} -> revocationReason) (\s@LoadBalancerTlsCertificate' {} a -> s {revocationReason = a} :: LoadBalancerTlsCertificate)

-- | The timestamp when the certificate was revoked. This value is present
-- only when the certificate status is @REVOKED@.
loadBalancerTlsCertificate_revokedAt :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.UTCTime)
loadBalancerTlsCertificate_revokedAt = Lens.lens (\LoadBalancerTlsCertificate' {revokedAt} -> revokedAt) (\s@LoadBalancerTlsCertificate' {} a -> s {revokedAt = a} :: LoadBalancerTlsCertificate) Core.. Lens.mapping Core._Time

-- | The timestamp when the SSL\/TLS certificate expires.
loadBalancerTlsCertificate_notAfter :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.UTCTime)
loadBalancerTlsCertificate_notAfter = Lens.lens (\LoadBalancerTlsCertificate' {notAfter} -> notAfter) (\s@LoadBalancerTlsCertificate' {} a -> s {notAfter = a} :: LoadBalancerTlsCertificate) Core.. Lens.mapping Core._Time

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
loadBalancerTlsCertificate_tags :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe [Tag])
loadBalancerTlsCertificate_tags = Lens.lens (\LoadBalancerTlsCertificate' {tags} -> tags) (\s@LoadBalancerTlsCertificate' {} a -> s {tags = a} :: LoadBalancerTlsCertificate) Core.. Lens.mapping Lens._Coerce

-- | The algorithm that was used to sign the certificate.
loadBalancerTlsCertificate_signatureAlgorithm :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.Text)
loadBalancerTlsCertificate_signatureAlgorithm = Lens.lens (\LoadBalancerTlsCertificate' {signatureAlgorithm} -> signatureAlgorithm) (\s@LoadBalancerTlsCertificate' {} a -> s {signatureAlgorithm = a} :: LoadBalancerTlsCertificate)

-- | The issuer of the certificate.
loadBalancerTlsCertificate_issuer :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.Text)
loadBalancerTlsCertificate_issuer = Lens.lens (\LoadBalancerTlsCertificate' {issuer} -> issuer) (\s@LoadBalancerTlsCertificate' {} a -> s {issuer = a} :: LoadBalancerTlsCertificate)

-- | The time when the SSL\/TLS certificate was issued.
loadBalancerTlsCertificate_issuedAt :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.UTCTime)
loadBalancerTlsCertificate_issuedAt = Lens.lens (\LoadBalancerTlsCertificate' {issuedAt} -> issuedAt) (\s@LoadBalancerTlsCertificate' {} a -> s {issuedAt = a} :: LoadBalancerTlsCertificate) Core.. Lens.mapping Core._Time

-- | The algorithm used to generate the key pair (the public and private
-- key).
loadBalancerTlsCertificate_keyAlgorithm :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.Text)
loadBalancerTlsCertificate_keyAlgorithm = Lens.lens (\LoadBalancerTlsCertificate' {keyAlgorithm} -> keyAlgorithm) (\s@LoadBalancerTlsCertificate' {} a -> s {keyAlgorithm = a} :: LoadBalancerTlsCertificate)

-- | An array of LoadBalancerTlsCertificateDomainValidationRecord objects
-- describing the records.
loadBalancerTlsCertificate_domainValidationRecords :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe [LoadBalancerTlsCertificateDomainValidationRecord])
loadBalancerTlsCertificate_domainValidationRecords = Lens.lens (\LoadBalancerTlsCertificate' {domainValidationRecords} -> domainValidationRecords) (\s@LoadBalancerTlsCertificate' {} a -> s {domainValidationRecords = a} :: LoadBalancerTlsCertificate) Core.. Lens.mapping Lens._Coerce

-- | The AWS Region and Availability Zone where you created your certificate.
loadBalancerTlsCertificate_location :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe ResourceLocation)
loadBalancerTlsCertificate_location = Lens.lens (\LoadBalancerTlsCertificate' {location} -> location) (\s@LoadBalancerTlsCertificate' {} a -> s {location = a} :: LoadBalancerTlsCertificate)

-- | The load balancer name where your SSL\/TLS certificate is attached.
loadBalancerTlsCertificate_loadBalancerName :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe Core.Text)
loadBalancerTlsCertificate_loadBalancerName = Lens.lens (\LoadBalancerTlsCertificate' {loadBalancerName} -> loadBalancerName) (\s@LoadBalancerTlsCertificate' {} a -> s {loadBalancerName = a} :: LoadBalancerTlsCertificate)

-- | An object that describes the status of the certificate renewal managed
-- by Lightsail.
loadBalancerTlsCertificate_renewalSummary :: Lens.Lens' LoadBalancerTlsCertificate (Core.Maybe LoadBalancerTlsCertificateRenewalSummary)
loadBalancerTlsCertificate_renewalSummary = Lens.lens (\LoadBalancerTlsCertificate' {renewalSummary} -> renewalSummary) (\s@LoadBalancerTlsCertificate' {} a -> s {renewalSummary = a} :: LoadBalancerTlsCertificate)

instance Core.FromJSON LoadBalancerTlsCertificate where
  parseJSON =
    Core.withObject
      "LoadBalancerTlsCertificate"
      ( \x ->
          LoadBalancerTlsCertificate'
            Core.<$> (x Core..:? "status")
            Core.<*> (x Core..:? "notBefore")
            Core.<*> (x Core..:? "serial")
            Core.<*> (x Core..:? "isAttached")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "resourceType")
            Core.<*> (x Core..:? "supportCode")
            Core.<*> ( x Core..:? "subjectAlternativeNames"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "domainName")
            Core.<*> (x Core..:? "subject")
            Core.<*> (x Core..:? "failureReason")
            Core.<*> (x Core..:? "revocationReason")
            Core.<*> (x Core..:? "revokedAt")
            Core.<*> (x Core..:? "notAfter")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "signatureAlgorithm")
            Core.<*> (x Core..:? "issuer")
            Core.<*> (x Core..:? "issuedAt")
            Core.<*> (x Core..:? "keyAlgorithm")
            Core.<*> ( x Core..:? "domainValidationRecords"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "location")
            Core.<*> (x Core..:? "loadBalancerName")
            Core.<*> (x Core..:? "renewalSummary")
      )

instance Core.Hashable LoadBalancerTlsCertificate

instance Core.NFData LoadBalancerTlsCertificate

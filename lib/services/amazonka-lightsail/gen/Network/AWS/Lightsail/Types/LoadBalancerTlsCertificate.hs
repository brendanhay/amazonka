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
import qualified Network.AWS.Prelude as Prelude

-- | Describes a load balancer SSL\/TLS certificate.
--
-- TLS is just an updated, more secure version of Secure Socket Layer
-- (SSL).
--
-- /See:/ 'newLoadBalancerTlsCertificate' smart constructor.
data LoadBalancerTlsCertificate = LoadBalancerTlsCertificate'
  { -- | The timestamp when the SSL\/TLS certificate is first valid.
    notBefore :: Prelude.Maybe Core.POSIX,
    -- | The validation status of the SSL\/TLS certificate. Valid values are
    -- below.
    status :: Prelude.Maybe LoadBalancerTlsCertificateStatus,
    -- | The serial number of the certificate.
    serial :: Prelude.Maybe Prelude.Text,
    -- | When @true@, the SSL\/TLS certificate is attached to the Lightsail load
    -- balancer.
    isAttached :: Prelude.Maybe Prelude.Bool,
    -- | The time when you created your SSL\/TLS certificate.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the SSL\/TLS certificate.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The support code. Include this code in your email to support when you
    -- have questions about your Lightsail load balancer or SSL\/TLS
    -- certificate. This code enables our support team to look up your
    -- Lightsail information more easily.
    supportCode :: Prelude.Maybe Prelude.Text,
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
    resourceType :: Prelude.Maybe ResourceType,
    -- | The domain name for your SSL\/TLS certificate.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | An array of strings that specify the alternate domains (e.g.,
    -- @example2.com@) and subdomains (e.g., @blog.example.com@) for the
    -- certificate.
    subjectAlternativeNames :: Prelude.Maybe [Prelude.Text],
    -- | The name of the SSL\/TLS certificate (e.g., @my-certificate@).
    name :: Prelude.Maybe Prelude.Text,
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
    failureReason :: Prelude.Maybe LoadBalancerTlsCertificateFailureReason,
    -- | The name of the entity that is associated with the public key contained
    -- in the certificate.
    subject :: Prelude.Maybe Prelude.Text,
    -- | The reason the certificate was revoked. This value is present only when
    -- the certificate status is @REVOKED@.
    revocationReason :: Prelude.Maybe LoadBalancerTlsCertificateRevocationReason,
    -- | The timestamp when the SSL\/TLS certificate expires.
    notAfter :: Prelude.Maybe Core.POSIX,
    -- | The timestamp when the certificate was revoked. This value is present
    -- only when the certificate status is @REVOKED@.
    revokedAt :: Prelude.Maybe Core.POSIX,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | The issuer of the certificate.
    issuer :: Prelude.Maybe Prelude.Text,
    -- | The algorithm that was used to sign the certificate.
    signatureAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | The algorithm used to generate the key pair (the public and private
    -- key).
    keyAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | The time when the SSL\/TLS certificate was issued.
    issuedAt :: Prelude.Maybe Core.POSIX,
    -- | An array of LoadBalancerTlsCertificateDomainValidationRecord objects
    -- describing the records.
    domainValidationRecords :: Prelude.Maybe [LoadBalancerTlsCertificateDomainValidationRecord],
    -- | The load balancer name where your SSL\/TLS certificate is attached.
    loadBalancerName :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region and Availability Zone where you created your certificate.
    location :: Prelude.Maybe ResourceLocation,
    -- | An object that describes the status of the certificate renewal managed
    -- by Lightsail.
    renewalSummary :: Prelude.Maybe LoadBalancerTlsCertificateRenewalSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoadBalancerTlsCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notBefore', 'loadBalancerTlsCertificate_notBefore' - The timestamp when the SSL\/TLS certificate is first valid.
--
-- 'status', 'loadBalancerTlsCertificate_status' - The validation status of the SSL\/TLS certificate. Valid values are
-- below.
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
-- 'supportCode', 'loadBalancerTlsCertificate_supportCode' - The support code. Include this code in your email to support when you
-- have questions about your Lightsail load balancer or SSL\/TLS
-- certificate. This code enables our support team to look up your
-- Lightsail information more easily.
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
-- 'domainName', 'loadBalancerTlsCertificate_domainName' - The domain name for your SSL\/TLS certificate.
--
-- 'subjectAlternativeNames', 'loadBalancerTlsCertificate_subjectAlternativeNames' - An array of strings that specify the alternate domains (e.g.,
-- @example2.com@) and subdomains (e.g., @blog.example.com@) for the
-- certificate.
--
-- 'name', 'loadBalancerTlsCertificate_name' - The name of the SSL\/TLS certificate (e.g., @my-certificate@).
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
-- 'subject', 'loadBalancerTlsCertificate_subject' - The name of the entity that is associated with the public key contained
-- in the certificate.
--
-- 'revocationReason', 'loadBalancerTlsCertificate_revocationReason' - The reason the certificate was revoked. This value is present only when
-- the certificate status is @REVOKED@.
--
-- 'notAfter', 'loadBalancerTlsCertificate_notAfter' - The timestamp when the SSL\/TLS certificate expires.
--
-- 'revokedAt', 'loadBalancerTlsCertificate_revokedAt' - The timestamp when the certificate was revoked. This value is present
-- only when the certificate status is @REVOKED@.
--
-- 'tags', 'loadBalancerTlsCertificate_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
--
-- 'issuer', 'loadBalancerTlsCertificate_issuer' - The issuer of the certificate.
--
-- 'signatureAlgorithm', 'loadBalancerTlsCertificate_signatureAlgorithm' - The algorithm that was used to sign the certificate.
--
-- 'keyAlgorithm', 'loadBalancerTlsCertificate_keyAlgorithm' - The algorithm used to generate the key pair (the public and private
-- key).
--
-- 'issuedAt', 'loadBalancerTlsCertificate_issuedAt' - The time when the SSL\/TLS certificate was issued.
--
-- 'domainValidationRecords', 'loadBalancerTlsCertificate_domainValidationRecords' - An array of LoadBalancerTlsCertificateDomainValidationRecord objects
-- describing the records.
--
-- 'loadBalancerName', 'loadBalancerTlsCertificate_loadBalancerName' - The load balancer name where your SSL\/TLS certificate is attached.
--
-- 'location', 'loadBalancerTlsCertificate_location' - The AWS Region and Availability Zone where you created your certificate.
--
-- 'renewalSummary', 'loadBalancerTlsCertificate_renewalSummary' - An object that describes the status of the certificate renewal managed
-- by Lightsail.
newLoadBalancerTlsCertificate ::
  LoadBalancerTlsCertificate
newLoadBalancerTlsCertificate =
  LoadBalancerTlsCertificate'
    { notBefore =
        Prelude.Nothing,
      status = Prelude.Nothing,
      serial = Prelude.Nothing,
      isAttached = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      domainName = Prelude.Nothing,
      subjectAlternativeNames = Prelude.Nothing,
      name = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      subject = Prelude.Nothing,
      revocationReason = Prelude.Nothing,
      notAfter = Prelude.Nothing,
      revokedAt = Prelude.Nothing,
      tags = Prelude.Nothing,
      issuer = Prelude.Nothing,
      signatureAlgorithm = Prelude.Nothing,
      keyAlgorithm = Prelude.Nothing,
      issuedAt = Prelude.Nothing,
      domainValidationRecords = Prelude.Nothing,
      loadBalancerName = Prelude.Nothing,
      location = Prelude.Nothing,
      renewalSummary = Prelude.Nothing
    }

-- | The timestamp when the SSL\/TLS certificate is first valid.
loadBalancerTlsCertificate_notBefore :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.UTCTime)
loadBalancerTlsCertificate_notBefore = Lens.lens (\LoadBalancerTlsCertificate' {notBefore} -> notBefore) (\s@LoadBalancerTlsCertificate' {} a -> s {notBefore = a} :: LoadBalancerTlsCertificate) Prelude.. Lens.mapping Core._Time

-- | The validation status of the SSL\/TLS certificate. Valid values are
-- below.
loadBalancerTlsCertificate_status :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe LoadBalancerTlsCertificateStatus)
loadBalancerTlsCertificate_status = Lens.lens (\LoadBalancerTlsCertificate' {status} -> status) (\s@LoadBalancerTlsCertificate' {} a -> s {status = a} :: LoadBalancerTlsCertificate)

-- | The serial number of the certificate.
loadBalancerTlsCertificate_serial :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificate_serial = Lens.lens (\LoadBalancerTlsCertificate' {serial} -> serial) (\s@LoadBalancerTlsCertificate' {} a -> s {serial = a} :: LoadBalancerTlsCertificate)

-- | When @true@, the SSL\/TLS certificate is attached to the Lightsail load
-- balancer.
loadBalancerTlsCertificate_isAttached :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Bool)
loadBalancerTlsCertificate_isAttached = Lens.lens (\LoadBalancerTlsCertificate' {isAttached} -> isAttached) (\s@LoadBalancerTlsCertificate' {} a -> s {isAttached = a} :: LoadBalancerTlsCertificate)

-- | The time when you created your SSL\/TLS certificate.
loadBalancerTlsCertificate_createdAt :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.UTCTime)
loadBalancerTlsCertificate_createdAt = Lens.lens (\LoadBalancerTlsCertificate' {createdAt} -> createdAt) (\s@LoadBalancerTlsCertificate' {} a -> s {createdAt = a} :: LoadBalancerTlsCertificate) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the SSL\/TLS certificate.
loadBalancerTlsCertificate_arn :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificate_arn = Lens.lens (\LoadBalancerTlsCertificate' {arn} -> arn) (\s@LoadBalancerTlsCertificate' {} a -> s {arn = a} :: LoadBalancerTlsCertificate)

-- | The support code. Include this code in your email to support when you
-- have questions about your Lightsail load balancer or SSL\/TLS
-- certificate. This code enables our support team to look up your
-- Lightsail information more easily.
loadBalancerTlsCertificate_supportCode :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificate_supportCode = Lens.lens (\LoadBalancerTlsCertificate' {supportCode} -> supportCode) (\s@LoadBalancerTlsCertificate' {} a -> s {supportCode = a} :: LoadBalancerTlsCertificate)

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
loadBalancerTlsCertificate_resourceType :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe ResourceType)
loadBalancerTlsCertificate_resourceType = Lens.lens (\LoadBalancerTlsCertificate' {resourceType} -> resourceType) (\s@LoadBalancerTlsCertificate' {} a -> s {resourceType = a} :: LoadBalancerTlsCertificate)

-- | The domain name for your SSL\/TLS certificate.
loadBalancerTlsCertificate_domainName :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificate_domainName = Lens.lens (\LoadBalancerTlsCertificate' {domainName} -> domainName) (\s@LoadBalancerTlsCertificate' {} a -> s {domainName = a} :: LoadBalancerTlsCertificate)

-- | An array of strings that specify the alternate domains (e.g.,
-- @example2.com@) and subdomains (e.g., @blog.example.com@) for the
-- certificate.
loadBalancerTlsCertificate_subjectAlternativeNames :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe [Prelude.Text])
loadBalancerTlsCertificate_subjectAlternativeNames = Lens.lens (\LoadBalancerTlsCertificate' {subjectAlternativeNames} -> subjectAlternativeNames) (\s@LoadBalancerTlsCertificate' {} a -> s {subjectAlternativeNames = a} :: LoadBalancerTlsCertificate) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the SSL\/TLS certificate (e.g., @my-certificate@).
loadBalancerTlsCertificate_name :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificate_name = Lens.lens (\LoadBalancerTlsCertificate' {name} -> name) (\s@LoadBalancerTlsCertificate' {} a -> s {name = a} :: LoadBalancerTlsCertificate)

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
loadBalancerTlsCertificate_failureReason :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe LoadBalancerTlsCertificateFailureReason)
loadBalancerTlsCertificate_failureReason = Lens.lens (\LoadBalancerTlsCertificate' {failureReason} -> failureReason) (\s@LoadBalancerTlsCertificate' {} a -> s {failureReason = a} :: LoadBalancerTlsCertificate)

-- | The name of the entity that is associated with the public key contained
-- in the certificate.
loadBalancerTlsCertificate_subject :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificate_subject = Lens.lens (\LoadBalancerTlsCertificate' {subject} -> subject) (\s@LoadBalancerTlsCertificate' {} a -> s {subject = a} :: LoadBalancerTlsCertificate)

-- | The reason the certificate was revoked. This value is present only when
-- the certificate status is @REVOKED@.
loadBalancerTlsCertificate_revocationReason :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe LoadBalancerTlsCertificateRevocationReason)
loadBalancerTlsCertificate_revocationReason = Lens.lens (\LoadBalancerTlsCertificate' {revocationReason} -> revocationReason) (\s@LoadBalancerTlsCertificate' {} a -> s {revocationReason = a} :: LoadBalancerTlsCertificate)

-- | The timestamp when the SSL\/TLS certificate expires.
loadBalancerTlsCertificate_notAfter :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.UTCTime)
loadBalancerTlsCertificate_notAfter = Lens.lens (\LoadBalancerTlsCertificate' {notAfter} -> notAfter) (\s@LoadBalancerTlsCertificate' {} a -> s {notAfter = a} :: LoadBalancerTlsCertificate) Prelude.. Lens.mapping Core._Time

-- | The timestamp when the certificate was revoked. This value is present
-- only when the certificate status is @REVOKED@.
loadBalancerTlsCertificate_revokedAt :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.UTCTime)
loadBalancerTlsCertificate_revokedAt = Lens.lens (\LoadBalancerTlsCertificate' {revokedAt} -> revokedAt) (\s@LoadBalancerTlsCertificate' {} a -> s {revokedAt = a} :: LoadBalancerTlsCertificate) Prelude.. Lens.mapping Core._Time

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
loadBalancerTlsCertificate_tags :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe [Tag])
loadBalancerTlsCertificate_tags = Lens.lens (\LoadBalancerTlsCertificate' {tags} -> tags) (\s@LoadBalancerTlsCertificate' {} a -> s {tags = a} :: LoadBalancerTlsCertificate) Prelude.. Lens.mapping Lens._Coerce

-- | The issuer of the certificate.
loadBalancerTlsCertificate_issuer :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificate_issuer = Lens.lens (\LoadBalancerTlsCertificate' {issuer} -> issuer) (\s@LoadBalancerTlsCertificate' {} a -> s {issuer = a} :: LoadBalancerTlsCertificate)

-- | The algorithm that was used to sign the certificate.
loadBalancerTlsCertificate_signatureAlgorithm :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificate_signatureAlgorithm = Lens.lens (\LoadBalancerTlsCertificate' {signatureAlgorithm} -> signatureAlgorithm) (\s@LoadBalancerTlsCertificate' {} a -> s {signatureAlgorithm = a} :: LoadBalancerTlsCertificate)

-- | The algorithm used to generate the key pair (the public and private
-- key).
loadBalancerTlsCertificate_keyAlgorithm :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificate_keyAlgorithm = Lens.lens (\LoadBalancerTlsCertificate' {keyAlgorithm} -> keyAlgorithm) (\s@LoadBalancerTlsCertificate' {} a -> s {keyAlgorithm = a} :: LoadBalancerTlsCertificate)

-- | The time when the SSL\/TLS certificate was issued.
loadBalancerTlsCertificate_issuedAt :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.UTCTime)
loadBalancerTlsCertificate_issuedAt = Lens.lens (\LoadBalancerTlsCertificate' {issuedAt} -> issuedAt) (\s@LoadBalancerTlsCertificate' {} a -> s {issuedAt = a} :: LoadBalancerTlsCertificate) Prelude.. Lens.mapping Core._Time

-- | An array of LoadBalancerTlsCertificateDomainValidationRecord objects
-- describing the records.
loadBalancerTlsCertificate_domainValidationRecords :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe [LoadBalancerTlsCertificateDomainValidationRecord])
loadBalancerTlsCertificate_domainValidationRecords = Lens.lens (\LoadBalancerTlsCertificate' {domainValidationRecords} -> domainValidationRecords) (\s@LoadBalancerTlsCertificate' {} a -> s {domainValidationRecords = a} :: LoadBalancerTlsCertificate) Prelude.. Lens.mapping Lens._Coerce

-- | The load balancer name where your SSL\/TLS certificate is attached.
loadBalancerTlsCertificate_loadBalancerName :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificate_loadBalancerName = Lens.lens (\LoadBalancerTlsCertificate' {loadBalancerName} -> loadBalancerName) (\s@LoadBalancerTlsCertificate' {} a -> s {loadBalancerName = a} :: LoadBalancerTlsCertificate)

-- | The AWS Region and Availability Zone where you created your certificate.
loadBalancerTlsCertificate_location :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe ResourceLocation)
loadBalancerTlsCertificate_location = Lens.lens (\LoadBalancerTlsCertificate' {location} -> location) (\s@LoadBalancerTlsCertificate' {} a -> s {location = a} :: LoadBalancerTlsCertificate)

-- | An object that describes the status of the certificate renewal managed
-- by Lightsail.
loadBalancerTlsCertificate_renewalSummary :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe LoadBalancerTlsCertificateRenewalSummary)
loadBalancerTlsCertificate_renewalSummary = Lens.lens (\LoadBalancerTlsCertificate' {renewalSummary} -> renewalSummary) (\s@LoadBalancerTlsCertificate' {} a -> s {renewalSummary = a} :: LoadBalancerTlsCertificate)

instance Core.FromJSON LoadBalancerTlsCertificate where
  parseJSON =
    Core.withObject
      "LoadBalancerTlsCertificate"
      ( \x ->
          LoadBalancerTlsCertificate'
            Prelude.<$> (x Core..:? "notBefore")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "serial")
            Prelude.<*> (x Core..:? "isAttached")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "supportCode")
            Prelude.<*> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "domainName")
            Prelude.<*> ( x Core..:? "subjectAlternativeNames"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "failureReason")
            Prelude.<*> (x Core..:? "subject")
            Prelude.<*> (x Core..:? "revocationReason")
            Prelude.<*> (x Core..:? "notAfter")
            Prelude.<*> (x Core..:? "revokedAt")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "issuer")
            Prelude.<*> (x Core..:? "signatureAlgorithm")
            Prelude.<*> (x Core..:? "keyAlgorithm")
            Prelude.<*> (x Core..:? "issuedAt")
            Prelude.<*> ( x Core..:? "domainValidationRecords"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "loadBalancerName")
            Prelude.<*> (x Core..:? "location")
            Prelude.<*> (x Core..:? "renewalSummary")
      )

instance Prelude.Hashable LoadBalancerTlsCertificate

instance Prelude.NFData LoadBalancerTlsCertificate

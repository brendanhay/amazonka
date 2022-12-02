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
-- Module      : Amazonka.Lightsail.Types.LoadBalancerTlsCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.LoadBalancerTlsCertificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationRecord
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateFailureReason
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateRenewalSummary
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateRevocationReason
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateStatus
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceType
import Amazonka.Lightsail.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a load balancer SSL\/TLS certificate.
--
-- TLS is just an updated, more secure version of Secure Socket Layer
-- (SSL).
--
-- /See:/ 'newLoadBalancerTlsCertificate' smart constructor.
data LoadBalancerTlsCertificate = LoadBalancerTlsCertificate'
  { -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | The issuer of the certificate.
    issuer :: Prelude.Maybe Prelude.Text,
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
    -- | The name of the SSL\/TLS certificate (e.g., @my-certificate@).
    name :: Prelude.Maybe Prelude.Text,
    -- | The domain name for your SSL\/TLS certificate.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The load balancer name where your SSL\/TLS certificate is attached.
    loadBalancerName :: Prelude.Maybe Prelude.Text,
    -- | The serial number of the certificate.
    serial :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the SSL\/TLS certificate.
    arn :: Prelude.Maybe Prelude.Text,
    -- | An object that describes the status of the certificate renewal managed
    -- by Lightsail.
    renewalSummary :: Prelude.Maybe LoadBalancerTlsCertificateRenewalSummary,
    -- | The algorithm used to generate the key pair (the public and private
    -- key).
    keyAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | The validation status of the SSL\/TLS certificate. Valid values are
    -- below.
    status :: Prelude.Maybe LoadBalancerTlsCertificateStatus,
    -- | The Amazon Web Services Region and Availability Zone where you created
    -- your certificate.
    location :: Prelude.Maybe ResourceLocation,
    -- | The timestamp when the SSL\/TLS certificate is first valid.
    notBefore :: Prelude.Maybe Data.POSIX,
    -- | The reason the certificate was revoked. This value is present only when
    -- the certificate status is @REVOKED@.
    revocationReason :: Prelude.Maybe LoadBalancerTlsCertificateRevocationReason,
    -- | The algorithm that was used to sign the certificate.
    signatureAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | When @true@, the SSL\/TLS certificate is attached to the Lightsail load
    -- balancer.
    isAttached :: Prelude.Maybe Prelude.Bool,
    -- | An array of LoadBalancerTlsCertificateDomainValidationRecord objects
    -- describing the records.
    domainValidationRecords :: Prelude.Maybe [LoadBalancerTlsCertificateDomainValidationRecord],
    -- | The timestamp when the certificate was revoked. This value is present
    -- only when the certificate status is @REVOKED@.
    revokedAt :: Prelude.Maybe Data.POSIX,
    -- | The support code. Include this code in your email to support when you
    -- have questions about your Lightsail load balancer or SSL\/TLS
    -- certificate. This code enables our support team to look up your
    -- Lightsail information more easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the SSL\/TLS certificate expires.
    notAfter :: Prelude.Maybe Data.POSIX,
    -- | The name of the entity that is associated with the public key contained
    -- in the certificate.
    subject :: Prelude.Maybe Prelude.Text,
    -- | An array of strings that specify the alternate domains (e.g.,
    -- @example2.com@) and subdomains (e.g., @blog.example.com@) for the
    -- certificate.
    subjectAlternativeNames :: Prelude.Maybe [Prelude.Text],
    -- | The time when you created your SSL\/TLS certificate.
    createdAt :: Prelude.Maybe Data.POSIX,
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
    -- | The time when the SSL\/TLS certificate was issued.
    issuedAt :: Prelude.Maybe Data.POSIX
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
-- 'tags', 'loadBalancerTlsCertificate_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
--
-- 'issuer', 'loadBalancerTlsCertificate_issuer' - The issuer of the certificate.
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
-- 'name', 'loadBalancerTlsCertificate_name' - The name of the SSL\/TLS certificate (e.g., @my-certificate@).
--
-- 'domainName', 'loadBalancerTlsCertificate_domainName' - The domain name for your SSL\/TLS certificate.
--
-- 'loadBalancerName', 'loadBalancerTlsCertificate_loadBalancerName' - The load balancer name where your SSL\/TLS certificate is attached.
--
-- 'serial', 'loadBalancerTlsCertificate_serial' - The serial number of the certificate.
--
-- 'arn', 'loadBalancerTlsCertificate_arn' - The Amazon Resource Name (ARN) of the SSL\/TLS certificate.
--
-- 'renewalSummary', 'loadBalancerTlsCertificate_renewalSummary' - An object that describes the status of the certificate renewal managed
-- by Lightsail.
--
-- 'keyAlgorithm', 'loadBalancerTlsCertificate_keyAlgorithm' - The algorithm used to generate the key pair (the public and private
-- key).
--
-- 'status', 'loadBalancerTlsCertificate_status' - The validation status of the SSL\/TLS certificate. Valid values are
-- below.
--
-- 'location', 'loadBalancerTlsCertificate_location' - The Amazon Web Services Region and Availability Zone where you created
-- your certificate.
--
-- 'notBefore', 'loadBalancerTlsCertificate_notBefore' - The timestamp when the SSL\/TLS certificate is first valid.
--
-- 'revocationReason', 'loadBalancerTlsCertificate_revocationReason' - The reason the certificate was revoked. This value is present only when
-- the certificate status is @REVOKED@.
--
-- 'signatureAlgorithm', 'loadBalancerTlsCertificate_signatureAlgorithm' - The algorithm that was used to sign the certificate.
--
-- 'isAttached', 'loadBalancerTlsCertificate_isAttached' - When @true@, the SSL\/TLS certificate is attached to the Lightsail load
-- balancer.
--
-- 'domainValidationRecords', 'loadBalancerTlsCertificate_domainValidationRecords' - An array of LoadBalancerTlsCertificateDomainValidationRecord objects
-- describing the records.
--
-- 'revokedAt', 'loadBalancerTlsCertificate_revokedAt' - The timestamp when the certificate was revoked. This value is present
-- only when the certificate status is @REVOKED@.
--
-- 'supportCode', 'loadBalancerTlsCertificate_supportCode' - The support code. Include this code in your email to support when you
-- have questions about your Lightsail load balancer or SSL\/TLS
-- certificate. This code enables our support team to look up your
-- Lightsail information more easily.
--
-- 'notAfter', 'loadBalancerTlsCertificate_notAfter' - The timestamp when the SSL\/TLS certificate expires.
--
-- 'subject', 'loadBalancerTlsCertificate_subject' - The name of the entity that is associated with the public key contained
-- in the certificate.
--
-- 'subjectAlternativeNames', 'loadBalancerTlsCertificate_subjectAlternativeNames' - An array of strings that specify the alternate domains (e.g.,
-- @example2.com@) and subdomains (e.g., @blog.example.com@) for the
-- certificate.
--
-- 'createdAt', 'loadBalancerTlsCertificate_createdAt' - The time when you created your SSL\/TLS certificate.
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
-- 'issuedAt', 'loadBalancerTlsCertificate_issuedAt' - The time when the SSL\/TLS certificate was issued.
newLoadBalancerTlsCertificate ::
  LoadBalancerTlsCertificate
newLoadBalancerTlsCertificate =
  LoadBalancerTlsCertificate'
    { tags = Prelude.Nothing,
      issuer = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      name = Prelude.Nothing,
      domainName = Prelude.Nothing,
      loadBalancerName = Prelude.Nothing,
      serial = Prelude.Nothing,
      arn = Prelude.Nothing,
      renewalSummary = Prelude.Nothing,
      keyAlgorithm = Prelude.Nothing,
      status = Prelude.Nothing,
      location = Prelude.Nothing,
      notBefore = Prelude.Nothing,
      revocationReason = Prelude.Nothing,
      signatureAlgorithm = Prelude.Nothing,
      isAttached = Prelude.Nothing,
      domainValidationRecords = Prelude.Nothing,
      revokedAt = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      notAfter = Prelude.Nothing,
      subject = Prelude.Nothing,
      subjectAlternativeNames = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      issuedAt = Prelude.Nothing
    }

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
loadBalancerTlsCertificate_tags :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe [Tag])
loadBalancerTlsCertificate_tags = Lens.lens (\LoadBalancerTlsCertificate' {tags} -> tags) (\s@LoadBalancerTlsCertificate' {} a -> s {tags = a} :: LoadBalancerTlsCertificate) Prelude.. Lens.mapping Lens.coerced

-- | The issuer of the certificate.
loadBalancerTlsCertificate_issuer :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificate_issuer = Lens.lens (\LoadBalancerTlsCertificate' {issuer} -> issuer) (\s@LoadBalancerTlsCertificate' {} a -> s {issuer = a} :: LoadBalancerTlsCertificate)

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

-- | The name of the SSL\/TLS certificate (e.g., @my-certificate@).
loadBalancerTlsCertificate_name :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificate_name = Lens.lens (\LoadBalancerTlsCertificate' {name} -> name) (\s@LoadBalancerTlsCertificate' {} a -> s {name = a} :: LoadBalancerTlsCertificate)

-- | The domain name for your SSL\/TLS certificate.
loadBalancerTlsCertificate_domainName :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificate_domainName = Lens.lens (\LoadBalancerTlsCertificate' {domainName} -> domainName) (\s@LoadBalancerTlsCertificate' {} a -> s {domainName = a} :: LoadBalancerTlsCertificate)

-- | The load balancer name where your SSL\/TLS certificate is attached.
loadBalancerTlsCertificate_loadBalancerName :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificate_loadBalancerName = Lens.lens (\LoadBalancerTlsCertificate' {loadBalancerName} -> loadBalancerName) (\s@LoadBalancerTlsCertificate' {} a -> s {loadBalancerName = a} :: LoadBalancerTlsCertificate)

-- | The serial number of the certificate.
loadBalancerTlsCertificate_serial :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificate_serial = Lens.lens (\LoadBalancerTlsCertificate' {serial} -> serial) (\s@LoadBalancerTlsCertificate' {} a -> s {serial = a} :: LoadBalancerTlsCertificate)

-- | The Amazon Resource Name (ARN) of the SSL\/TLS certificate.
loadBalancerTlsCertificate_arn :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificate_arn = Lens.lens (\LoadBalancerTlsCertificate' {arn} -> arn) (\s@LoadBalancerTlsCertificate' {} a -> s {arn = a} :: LoadBalancerTlsCertificate)

-- | An object that describes the status of the certificate renewal managed
-- by Lightsail.
loadBalancerTlsCertificate_renewalSummary :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe LoadBalancerTlsCertificateRenewalSummary)
loadBalancerTlsCertificate_renewalSummary = Lens.lens (\LoadBalancerTlsCertificate' {renewalSummary} -> renewalSummary) (\s@LoadBalancerTlsCertificate' {} a -> s {renewalSummary = a} :: LoadBalancerTlsCertificate)

-- | The algorithm used to generate the key pair (the public and private
-- key).
loadBalancerTlsCertificate_keyAlgorithm :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificate_keyAlgorithm = Lens.lens (\LoadBalancerTlsCertificate' {keyAlgorithm} -> keyAlgorithm) (\s@LoadBalancerTlsCertificate' {} a -> s {keyAlgorithm = a} :: LoadBalancerTlsCertificate)

-- | The validation status of the SSL\/TLS certificate. Valid values are
-- below.
loadBalancerTlsCertificate_status :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe LoadBalancerTlsCertificateStatus)
loadBalancerTlsCertificate_status = Lens.lens (\LoadBalancerTlsCertificate' {status} -> status) (\s@LoadBalancerTlsCertificate' {} a -> s {status = a} :: LoadBalancerTlsCertificate)

-- | The Amazon Web Services Region and Availability Zone where you created
-- your certificate.
loadBalancerTlsCertificate_location :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe ResourceLocation)
loadBalancerTlsCertificate_location = Lens.lens (\LoadBalancerTlsCertificate' {location} -> location) (\s@LoadBalancerTlsCertificate' {} a -> s {location = a} :: LoadBalancerTlsCertificate)

-- | The timestamp when the SSL\/TLS certificate is first valid.
loadBalancerTlsCertificate_notBefore :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.UTCTime)
loadBalancerTlsCertificate_notBefore = Lens.lens (\LoadBalancerTlsCertificate' {notBefore} -> notBefore) (\s@LoadBalancerTlsCertificate' {} a -> s {notBefore = a} :: LoadBalancerTlsCertificate) Prelude.. Lens.mapping Data._Time

-- | The reason the certificate was revoked. This value is present only when
-- the certificate status is @REVOKED@.
loadBalancerTlsCertificate_revocationReason :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe LoadBalancerTlsCertificateRevocationReason)
loadBalancerTlsCertificate_revocationReason = Lens.lens (\LoadBalancerTlsCertificate' {revocationReason} -> revocationReason) (\s@LoadBalancerTlsCertificate' {} a -> s {revocationReason = a} :: LoadBalancerTlsCertificate)

-- | The algorithm that was used to sign the certificate.
loadBalancerTlsCertificate_signatureAlgorithm :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificate_signatureAlgorithm = Lens.lens (\LoadBalancerTlsCertificate' {signatureAlgorithm} -> signatureAlgorithm) (\s@LoadBalancerTlsCertificate' {} a -> s {signatureAlgorithm = a} :: LoadBalancerTlsCertificate)

-- | When @true@, the SSL\/TLS certificate is attached to the Lightsail load
-- balancer.
loadBalancerTlsCertificate_isAttached :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Bool)
loadBalancerTlsCertificate_isAttached = Lens.lens (\LoadBalancerTlsCertificate' {isAttached} -> isAttached) (\s@LoadBalancerTlsCertificate' {} a -> s {isAttached = a} :: LoadBalancerTlsCertificate)

-- | An array of LoadBalancerTlsCertificateDomainValidationRecord objects
-- describing the records.
loadBalancerTlsCertificate_domainValidationRecords :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe [LoadBalancerTlsCertificateDomainValidationRecord])
loadBalancerTlsCertificate_domainValidationRecords = Lens.lens (\LoadBalancerTlsCertificate' {domainValidationRecords} -> domainValidationRecords) (\s@LoadBalancerTlsCertificate' {} a -> s {domainValidationRecords = a} :: LoadBalancerTlsCertificate) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp when the certificate was revoked. This value is present
-- only when the certificate status is @REVOKED@.
loadBalancerTlsCertificate_revokedAt :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.UTCTime)
loadBalancerTlsCertificate_revokedAt = Lens.lens (\LoadBalancerTlsCertificate' {revokedAt} -> revokedAt) (\s@LoadBalancerTlsCertificate' {} a -> s {revokedAt = a} :: LoadBalancerTlsCertificate) Prelude.. Lens.mapping Data._Time

-- | The support code. Include this code in your email to support when you
-- have questions about your Lightsail load balancer or SSL\/TLS
-- certificate. This code enables our support team to look up your
-- Lightsail information more easily.
loadBalancerTlsCertificate_supportCode :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificate_supportCode = Lens.lens (\LoadBalancerTlsCertificate' {supportCode} -> supportCode) (\s@LoadBalancerTlsCertificate' {} a -> s {supportCode = a} :: LoadBalancerTlsCertificate)

-- | The timestamp when the SSL\/TLS certificate expires.
loadBalancerTlsCertificate_notAfter :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.UTCTime)
loadBalancerTlsCertificate_notAfter = Lens.lens (\LoadBalancerTlsCertificate' {notAfter} -> notAfter) (\s@LoadBalancerTlsCertificate' {} a -> s {notAfter = a} :: LoadBalancerTlsCertificate) Prelude.. Lens.mapping Data._Time

-- | The name of the entity that is associated with the public key contained
-- in the certificate.
loadBalancerTlsCertificate_subject :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificate_subject = Lens.lens (\LoadBalancerTlsCertificate' {subject} -> subject) (\s@LoadBalancerTlsCertificate' {} a -> s {subject = a} :: LoadBalancerTlsCertificate)

-- | An array of strings that specify the alternate domains (e.g.,
-- @example2.com@) and subdomains (e.g., @blog.example.com@) for the
-- certificate.
loadBalancerTlsCertificate_subjectAlternativeNames :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe [Prelude.Text])
loadBalancerTlsCertificate_subjectAlternativeNames = Lens.lens (\LoadBalancerTlsCertificate' {subjectAlternativeNames} -> subjectAlternativeNames) (\s@LoadBalancerTlsCertificate' {} a -> s {subjectAlternativeNames = a} :: LoadBalancerTlsCertificate) Prelude.. Lens.mapping Lens.coerced

-- | The time when you created your SSL\/TLS certificate.
loadBalancerTlsCertificate_createdAt :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.UTCTime)
loadBalancerTlsCertificate_createdAt = Lens.lens (\LoadBalancerTlsCertificate' {createdAt} -> createdAt) (\s@LoadBalancerTlsCertificate' {} a -> s {createdAt = a} :: LoadBalancerTlsCertificate) Prelude.. Lens.mapping Data._Time

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

-- | The time when the SSL\/TLS certificate was issued.
loadBalancerTlsCertificate_issuedAt :: Lens.Lens' LoadBalancerTlsCertificate (Prelude.Maybe Prelude.UTCTime)
loadBalancerTlsCertificate_issuedAt = Lens.lens (\LoadBalancerTlsCertificate' {issuedAt} -> issuedAt) (\s@LoadBalancerTlsCertificate' {} a -> s {issuedAt = a} :: LoadBalancerTlsCertificate) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON LoadBalancerTlsCertificate where
  parseJSON =
    Data.withObject
      "LoadBalancerTlsCertificate"
      ( \x ->
          LoadBalancerTlsCertificate'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "issuer")
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "domainName")
            Prelude.<*> (x Data..:? "loadBalancerName")
            Prelude.<*> (x Data..:? "serial")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "renewalSummary")
            Prelude.<*> (x Data..:? "keyAlgorithm")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "location")
            Prelude.<*> (x Data..:? "notBefore")
            Prelude.<*> (x Data..:? "revocationReason")
            Prelude.<*> (x Data..:? "signatureAlgorithm")
            Prelude.<*> (x Data..:? "isAttached")
            Prelude.<*> ( x Data..:? "domainValidationRecords"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "revokedAt")
            Prelude.<*> (x Data..:? "supportCode")
            Prelude.<*> (x Data..:? "notAfter")
            Prelude.<*> (x Data..:? "subject")
            Prelude.<*> ( x Data..:? "subjectAlternativeNames"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "failureReason")
            Prelude.<*> (x Data..:? "issuedAt")
      )

instance Prelude.Hashable LoadBalancerTlsCertificate where
  hashWithSalt _salt LoadBalancerTlsCertificate' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` issuer
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` loadBalancerName
      `Prelude.hashWithSalt` serial
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` renewalSummary
      `Prelude.hashWithSalt` keyAlgorithm
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` notBefore
      `Prelude.hashWithSalt` revocationReason
      `Prelude.hashWithSalt` signatureAlgorithm
      `Prelude.hashWithSalt` isAttached
      `Prelude.hashWithSalt` domainValidationRecords
      `Prelude.hashWithSalt` revokedAt
      `Prelude.hashWithSalt` supportCode
      `Prelude.hashWithSalt` notAfter
      `Prelude.hashWithSalt` subject
      `Prelude.hashWithSalt` subjectAlternativeNames
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` issuedAt

instance Prelude.NFData LoadBalancerTlsCertificate where
  rnf LoadBalancerTlsCertificate' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf issuer
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf loadBalancerName
      `Prelude.seq` Prelude.rnf serial
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf renewalSummary
      `Prelude.seq` Prelude.rnf keyAlgorithm
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf notBefore
      `Prelude.seq` Prelude.rnf revocationReason
      `Prelude.seq` Prelude.rnf signatureAlgorithm
      `Prelude.seq` Prelude.rnf isAttached
      `Prelude.seq` Prelude.rnf domainValidationRecords
      `Prelude.seq` Prelude.rnf revokedAt
      `Prelude.seq` Prelude.rnf supportCode
      `Prelude.seq` Prelude.rnf notAfter
      `Prelude.seq` Prelude.rnf subject
      `Prelude.seq` Prelude.rnf
        subjectAlternativeNames
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf
        failureReason
      `Prelude.seq` Prelude.rnf issuedAt

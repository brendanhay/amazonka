{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Lightsail.Types.Certificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Certificate where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.CertificateStatus
import Network.AWS.Lightsail.Types.DomainValidationRecord
import Network.AWS.Lightsail.Types.RenewalSummary
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Prelude

-- | Describes the full details of an Amazon Lightsail SSL\/TLS certificate.
--
-- To get a summary of a certificate, use the @GetCertificates@ action and
-- ommit @includeCertificateDetails@ from your request. The response will
-- include only the certificate Amazon Resource Name (ARN), certificate
-- name, domain name, and tags.
--
-- /See:/ 'newCertificate' smart constructor.
data Certificate = Certificate'
  { -- | The validation status of the certificate.
    status :: Prelude.Maybe CertificateStatus,
    -- | The timestamp when the certificate is first valid.
    notBefore :: Prelude.Maybe Prelude.POSIX,
    -- | The number of Lightsail resources that the certificate is attached to.
    inUseResourceCount :: Prelude.Maybe Prelude.Int,
    -- | The timestamp when the certificate was created.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of the certificate.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The renewal eligibility of the certificate.
    eligibleToRenew :: Prelude.Maybe Prelude.Text,
    -- | The support code. Include this code in your email to support when you
    -- have questions about your Lightsail certificate. This code enables our
    -- support team to look up your Lightsail information more easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | An array of strings that specify the alternate domains (e.g.,
    -- @example2.com@) and subdomains (e.g., @blog.example.com@) of the
    -- certificate.
    subjectAlternativeNames :: Prelude.Maybe [Prelude.Text],
    -- | The name of the certificate (e.g., @my-certificate@).
    name :: Prelude.Maybe Prelude.Text,
    -- | The domain name of the certificate.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The reason the certificate was revoked. This value is present only when
    -- the certificate status is @REVOKED@.
    revocationReason :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the certificate was revoked. This value is present
    -- only when the certificate status is @REVOKED@.
    revokedAt :: Prelude.Maybe Prelude.POSIX,
    -- | The certificate authority that issued the certificate.
    issuerCA :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the certificate expires.
    notAfter :: Prelude.Maybe Prelude.POSIX,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | The serial number of the certificate.
    serialNumber :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the certificate was issued.
    issuedAt :: Prelude.Maybe Prelude.POSIX,
    -- | The algorithm used to generate the key pair (the public and private key)
    -- of the certificate.
    keyAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe the domain validation records of the
    -- certificate.
    domainValidationRecords :: Prelude.Maybe [DomainValidationRecord],
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
    requestFailureReason :: Prelude.Maybe Prelude.Text,
    -- | An object that describes the status of the certificate renewal managed
    -- by Lightsail.
    renewalSummary :: Prelude.Maybe RenewalSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Certificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'certificate_status' - The validation status of the certificate.
--
-- 'notBefore', 'certificate_notBefore' - The timestamp when the certificate is first valid.
--
-- 'inUseResourceCount', 'certificate_inUseResourceCount' - The number of Lightsail resources that the certificate is attached to.
--
-- 'createdAt', 'certificate_createdAt' - The timestamp when the certificate was created.
--
-- 'arn', 'certificate_arn' - The Amazon Resource Name (ARN) of the certificate.
--
-- 'eligibleToRenew', 'certificate_eligibleToRenew' - The renewal eligibility of the certificate.
--
-- 'supportCode', 'certificate_supportCode' - The support code. Include this code in your email to support when you
-- have questions about your Lightsail certificate. This code enables our
-- support team to look up your Lightsail information more easily.
--
-- 'subjectAlternativeNames', 'certificate_subjectAlternativeNames' - An array of strings that specify the alternate domains (e.g.,
-- @example2.com@) and subdomains (e.g., @blog.example.com@) of the
-- certificate.
--
-- 'name', 'certificate_name' - The name of the certificate (e.g., @my-certificate@).
--
-- 'domainName', 'certificate_domainName' - The domain name of the certificate.
--
-- 'revocationReason', 'certificate_revocationReason' - The reason the certificate was revoked. This value is present only when
-- the certificate status is @REVOKED@.
--
-- 'revokedAt', 'certificate_revokedAt' - The timestamp when the certificate was revoked. This value is present
-- only when the certificate status is @REVOKED@.
--
-- 'issuerCA', 'certificate_issuerCA' - The certificate authority that issued the certificate.
--
-- 'notAfter', 'certificate_notAfter' - The timestamp when the certificate expires.
--
-- 'tags', 'certificate_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
--
-- 'serialNumber', 'certificate_serialNumber' - The serial number of the certificate.
--
-- 'issuedAt', 'certificate_issuedAt' - The timestamp when the certificate was issued.
--
-- 'keyAlgorithm', 'certificate_keyAlgorithm' - The algorithm used to generate the key pair (the public and private key)
-- of the certificate.
--
-- 'domainValidationRecords', 'certificate_domainValidationRecords' - An array of objects that describe the domain validation records of the
-- certificate.
--
-- 'requestFailureReason', 'certificate_requestFailureReason' - The validation failure reason, if any, of the certificate.
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
-- 'renewalSummary', 'certificate_renewalSummary' - An object that describes the status of the certificate renewal managed
-- by Lightsail.
newCertificate ::
  Certificate
newCertificate =
  Certificate'
    { status = Prelude.Nothing,
      notBefore = Prelude.Nothing,
      inUseResourceCount = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      eligibleToRenew = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      subjectAlternativeNames = Prelude.Nothing,
      name = Prelude.Nothing,
      domainName = Prelude.Nothing,
      revocationReason = Prelude.Nothing,
      revokedAt = Prelude.Nothing,
      issuerCA = Prelude.Nothing,
      notAfter = Prelude.Nothing,
      tags = Prelude.Nothing,
      serialNumber = Prelude.Nothing,
      issuedAt = Prelude.Nothing,
      keyAlgorithm = Prelude.Nothing,
      domainValidationRecords = Prelude.Nothing,
      requestFailureReason = Prelude.Nothing,
      renewalSummary = Prelude.Nothing
    }

-- | The validation status of the certificate.
certificate_status :: Lens.Lens' Certificate (Prelude.Maybe CertificateStatus)
certificate_status = Lens.lens (\Certificate' {status} -> status) (\s@Certificate' {} a -> s {status = a} :: Certificate)

-- | The timestamp when the certificate is first valid.
certificate_notBefore :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_notBefore = Lens.lens (\Certificate' {notBefore} -> notBefore) (\s@Certificate' {} a -> s {notBefore = a} :: Certificate) Prelude.. Lens.mapping Prelude._Time

-- | The number of Lightsail resources that the certificate is attached to.
certificate_inUseResourceCount :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Int)
certificate_inUseResourceCount = Lens.lens (\Certificate' {inUseResourceCount} -> inUseResourceCount) (\s@Certificate' {} a -> s {inUseResourceCount = a} :: Certificate)

-- | The timestamp when the certificate was created.
certificate_createdAt :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_createdAt = Lens.lens (\Certificate' {createdAt} -> createdAt) (\s@Certificate' {} a -> s {createdAt = a} :: Certificate) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the certificate.
certificate_arn :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_arn = Lens.lens (\Certificate' {arn} -> arn) (\s@Certificate' {} a -> s {arn = a} :: Certificate)

-- | The renewal eligibility of the certificate.
certificate_eligibleToRenew :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_eligibleToRenew = Lens.lens (\Certificate' {eligibleToRenew} -> eligibleToRenew) (\s@Certificate' {} a -> s {eligibleToRenew = a} :: Certificate)

-- | The support code. Include this code in your email to support when you
-- have questions about your Lightsail certificate. This code enables our
-- support team to look up your Lightsail information more easily.
certificate_supportCode :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_supportCode = Lens.lens (\Certificate' {supportCode} -> supportCode) (\s@Certificate' {} a -> s {supportCode = a} :: Certificate)

-- | An array of strings that specify the alternate domains (e.g.,
-- @example2.com@) and subdomains (e.g., @blog.example.com@) of the
-- certificate.
certificate_subjectAlternativeNames :: Lens.Lens' Certificate (Prelude.Maybe [Prelude.Text])
certificate_subjectAlternativeNames = Lens.lens (\Certificate' {subjectAlternativeNames} -> subjectAlternativeNames) (\s@Certificate' {} a -> s {subjectAlternativeNames = a} :: Certificate) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the certificate (e.g., @my-certificate@).
certificate_name :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_name = Lens.lens (\Certificate' {name} -> name) (\s@Certificate' {} a -> s {name = a} :: Certificate)

-- | The domain name of the certificate.
certificate_domainName :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_domainName = Lens.lens (\Certificate' {domainName} -> domainName) (\s@Certificate' {} a -> s {domainName = a} :: Certificate)

-- | The reason the certificate was revoked. This value is present only when
-- the certificate status is @REVOKED@.
certificate_revocationReason :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_revocationReason = Lens.lens (\Certificate' {revocationReason} -> revocationReason) (\s@Certificate' {} a -> s {revocationReason = a} :: Certificate)

-- | The timestamp when the certificate was revoked. This value is present
-- only when the certificate status is @REVOKED@.
certificate_revokedAt :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_revokedAt = Lens.lens (\Certificate' {revokedAt} -> revokedAt) (\s@Certificate' {} a -> s {revokedAt = a} :: Certificate) Prelude.. Lens.mapping Prelude._Time

-- | The certificate authority that issued the certificate.
certificate_issuerCA :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_issuerCA = Lens.lens (\Certificate' {issuerCA} -> issuerCA) (\s@Certificate' {} a -> s {issuerCA = a} :: Certificate)

-- | The timestamp when the certificate expires.
certificate_notAfter :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_notAfter = Lens.lens (\Certificate' {notAfter} -> notAfter) (\s@Certificate' {} a -> s {notAfter = a} :: Certificate) Prelude.. Lens.mapping Prelude._Time

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
certificate_tags :: Lens.Lens' Certificate (Prelude.Maybe [Tag])
certificate_tags = Lens.lens (\Certificate' {tags} -> tags) (\s@Certificate' {} a -> s {tags = a} :: Certificate) Prelude.. Lens.mapping Prelude._Coerce

-- | The serial number of the certificate.
certificate_serialNumber :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_serialNumber = Lens.lens (\Certificate' {serialNumber} -> serialNumber) (\s@Certificate' {} a -> s {serialNumber = a} :: Certificate)

-- | The timestamp when the certificate was issued.
certificate_issuedAt :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_issuedAt = Lens.lens (\Certificate' {issuedAt} -> issuedAt) (\s@Certificate' {} a -> s {issuedAt = a} :: Certificate) Prelude.. Lens.mapping Prelude._Time

-- | The algorithm used to generate the key pair (the public and private key)
-- of the certificate.
certificate_keyAlgorithm :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_keyAlgorithm = Lens.lens (\Certificate' {keyAlgorithm} -> keyAlgorithm) (\s@Certificate' {} a -> s {keyAlgorithm = a} :: Certificate)

-- | An array of objects that describe the domain validation records of the
-- certificate.
certificate_domainValidationRecords :: Lens.Lens' Certificate (Prelude.Maybe [DomainValidationRecord])
certificate_domainValidationRecords = Lens.lens (\Certificate' {domainValidationRecords} -> domainValidationRecords) (\s@Certificate' {} a -> s {domainValidationRecords = a} :: Certificate) Prelude.. Lens.mapping Prelude._Coerce

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
certificate_requestFailureReason :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_requestFailureReason = Lens.lens (\Certificate' {requestFailureReason} -> requestFailureReason) (\s@Certificate' {} a -> s {requestFailureReason = a} :: Certificate)

-- | An object that describes the status of the certificate renewal managed
-- by Lightsail.
certificate_renewalSummary :: Lens.Lens' Certificate (Prelude.Maybe RenewalSummary)
certificate_renewalSummary = Lens.lens (\Certificate' {renewalSummary} -> renewalSummary) (\s@Certificate' {} a -> s {renewalSummary = a} :: Certificate)

instance Prelude.FromJSON Certificate where
  parseJSON =
    Prelude.withObject
      "Certificate"
      ( \x ->
          Certificate'
            Prelude.<$> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "notBefore")
            Prelude.<*> (x Prelude..:? "inUseResourceCount")
            Prelude.<*> (x Prelude..:? "createdAt")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "eligibleToRenew")
            Prelude.<*> (x Prelude..:? "supportCode")
            Prelude.<*> ( x Prelude..:? "subjectAlternativeNames"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "domainName")
            Prelude.<*> (x Prelude..:? "revocationReason")
            Prelude.<*> (x Prelude..:? "revokedAt")
            Prelude.<*> (x Prelude..:? "issuerCA")
            Prelude.<*> (x Prelude..:? "notAfter")
            Prelude.<*> (x Prelude..:? "tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "serialNumber")
            Prelude.<*> (x Prelude..:? "issuedAt")
            Prelude.<*> (x Prelude..:? "keyAlgorithm")
            Prelude.<*> ( x Prelude..:? "domainValidationRecords"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "requestFailureReason")
            Prelude.<*> (x Prelude..:? "renewalSummary")
      )

instance Prelude.Hashable Certificate

instance Prelude.NFData Certificate

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Certificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Certificate where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.CertificateStatus
import Network.AWS.Lightsail.Types.DomainValidationRecord
import Network.AWS.Lightsail.Types.RenewalSummary
import Network.AWS.Lightsail.Types.Tag
import Network.AWS.Prelude

-- | Describes the full details of an Amazon Lightsail SSL/TLS certificate.
--
--
--
-- /See:/ 'certificate' smart constructor.
data Certificate = Certificate'
  { _cerStatus ::
      !(Maybe CertificateStatus),
    _cerSubjectAlternativeNames :: !(Maybe [Text]),
    _cerArn :: !(Maybe Text),
    _cerCreatedAt :: !(Maybe POSIX),
    _cerEligibleToRenew :: !(Maybe Text),
    _cerRequestFailureReason :: !(Maybe Text),
    _cerRevokedAt :: !(Maybe POSIX),
    _cerNotBefore :: !(Maybe POSIX),
    _cerRevocationReason :: !(Maybe Text),
    _cerDomainName :: !(Maybe Text),
    _cerName :: !(Maybe Text),
    _cerRenewalSummary :: !(Maybe RenewalSummary),
    _cerSupportCode :: !(Maybe Text),
    _cerDomainValidationRecords :: !(Maybe [DomainValidationRecord]),
    _cerInUseResourceCount :: !(Maybe Int),
    _cerIssuedAt :: !(Maybe POSIX),
    _cerKeyAlgorithm :: !(Maybe Text),
    _cerSerialNumber :: !(Maybe Text),
    _cerIssuerCA :: !(Maybe Text),
    _cerTags :: !(Maybe [Tag]),
    _cerNotAfter :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Certificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cerStatus' - The validation status of the certificate.
--
-- * 'cerSubjectAlternativeNames' - An array of strings that specify the alternate domains (e.g., @example2.com@ ) and subdomains (e.g., @blog.example.com@ ) of the certificate.
--
-- * 'cerArn' - The Amazon Resource Name (ARN) of the certificate.
--
-- * 'cerCreatedAt' - The timestamp when the certificate was created.
--
-- * 'cerEligibleToRenew' - The renewal eligibility of the certificate.
--
-- * 'cerRequestFailureReason' - The validation failure reason, if any, of the certificate. The following failure reasons are possible:     * __@NO_AVAILABLE_CONTACTS@ __ - This failure applies to email validation, which is not available for Lightsail certificates.     * __@ADDITIONAL_VERIFICATION_REQUIRED@ __ - Lightsail requires additional information to process this certificate request. This can happen as a fraud-protection measure, such as when the domain ranks within the Alexa top 1000 websites. To provide the required information, use the <https://console.aws.amazon.com/support/home AWS Support Center> to contact AWS Support.     * __@DOMAIN_NOT_ALLOWED@ __ - One or more of the domain names in the certificate request was reported as an unsafe domain by <https://www.virustotal.com/gui/home/url VirusTotal> . To correct the problem, search for your domain name on the <https://www.virustotal.com/gui/home/url VirusTotal> website. If your domain is reported as suspicious, see <https://www.google.com/webmasters/hacked/?hl=en Google Help for Hacked Websites> to learn what you can do. If you believe that the result is a false positive, notify the organization that is reporting the domain. VirusTotal is an aggregate of several antivirus and URL scanners and cannot remove your domain from a block list itself. After you correct the problem and the VirusTotal registry has been updated, request a new certificate. If you see this error and your domain is not included in the VirusTotal list, visit the <https://console.aws.amazon.com/support/home AWS Support Center> and create a case.     * __@INVALID_PUBLIC_DOMAIN@ __ - One or more of the domain names in the certificate request is not valid. Typically, this is because a domain name in the request is not a valid top-level domain. Try to request a certificate again, correcting any spelling errors or typos that were in the failed request, and ensure that all domain names in the request are for valid top-level domains. For example, you cannot request a certificate for @example.invalidpublicdomain@ because @invalidpublicdomain@ is not a valid top-level domain.     * __@OTHER@ __ - Typically, this failure occurs when there is a typographical error in one or more of the domain names in the certificate request. Try to request a certificate again, correcting any spelling errors or typos that were in the failed request.
--
-- * 'cerRevokedAt' - The timestamp when the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
--
-- * 'cerNotBefore' - The timestamp when the certificate is first valid.
--
-- * 'cerRevocationReason' - The reason the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
--
-- * 'cerDomainName' - The domain name of the certificate.
--
-- * 'cerName' - The name of the certificate (e.g., @my-certificate@ ).
--
-- * 'cerRenewalSummary' - An object that describes the status of the certificate renewal managed by Lightsail.
--
-- * 'cerSupportCode' - The support code. Include this code in your email to support when you have questions about your Lightsail certificate. This code enables our support team to look up your Lightsail information more easily.
--
-- * 'cerDomainValidationRecords' - An array of objects that describe the domain validation records of the certificate.
--
-- * 'cerInUseResourceCount' - The number of Lightsail resources that the certificate is attached to.
--
-- * 'cerIssuedAt' - The timestamp when the certificate was issued.
--
-- * 'cerKeyAlgorithm' - The algorithm used to generate the key pair (the public and private key) of the certificate.
--
-- * 'cerSerialNumber' - The serial number of the certificate.
--
-- * 'cerIssuerCA' - The certificate authority that issued the certificate.
--
-- * 'cerTags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- * 'cerNotAfter' - The timestamp when the certificate expires.
certificate ::
  Certificate
certificate =
  Certificate'
    { _cerStatus = Nothing,
      _cerSubjectAlternativeNames = Nothing,
      _cerArn = Nothing,
      _cerCreatedAt = Nothing,
      _cerEligibleToRenew = Nothing,
      _cerRequestFailureReason = Nothing,
      _cerRevokedAt = Nothing,
      _cerNotBefore = Nothing,
      _cerRevocationReason = Nothing,
      _cerDomainName = Nothing,
      _cerName = Nothing,
      _cerRenewalSummary = Nothing,
      _cerSupportCode = Nothing,
      _cerDomainValidationRecords = Nothing,
      _cerInUseResourceCount = Nothing,
      _cerIssuedAt = Nothing,
      _cerKeyAlgorithm = Nothing,
      _cerSerialNumber = Nothing,
      _cerIssuerCA = Nothing,
      _cerTags = Nothing,
      _cerNotAfter = Nothing
    }

-- | The validation status of the certificate.
cerStatus :: Lens' Certificate (Maybe CertificateStatus)
cerStatus = lens _cerStatus (\s a -> s {_cerStatus = a})

-- | An array of strings that specify the alternate domains (e.g., @example2.com@ ) and subdomains (e.g., @blog.example.com@ ) of the certificate.
cerSubjectAlternativeNames :: Lens' Certificate [Text]
cerSubjectAlternativeNames = lens _cerSubjectAlternativeNames (\s a -> s {_cerSubjectAlternativeNames = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the certificate.
cerArn :: Lens' Certificate (Maybe Text)
cerArn = lens _cerArn (\s a -> s {_cerArn = a})

-- | The timestamp when the certificate was created.
cerCreatedAt :: Lens' Certificate (Maybe UTCTime)
cerCreatedAt = lens _cerCreatedAt (\s a -> s {_cerCreatedAt = a}) . mapping _Time

-- | The renewal eligibility of the certificate.
cerEligibleToRenew :: Lens' Certificate (Maybe Text)
cerEligibleToRenew = lens _cerEligibleToRenew (\s a -> s {_cerEligibleToRenew = a})

-- | The validation failure reason, if any, of the certificate. The following failure reasons are possible:     * __@NO_AVAILABLE_CONTACTS@ __ - This failure applies to email validation, which is not available for Lightsail certificates.     * __@ADDITIONAL_VERIFICATION_REQUIRED@ __ - Lightsail requires additional information to process this certificate request. This can happen as a fraud-protection measure, such as when the domain ranks within the Alexa top 1000 websites. To provide the required information, use the <https://console.aws.amazon.com/support/home AWS Support Center> to contact AWS Support.     * __@DOMAIN_NOT_ALLOWED@ __ - One or more of the domain names in the certificate request was reported as an unsafe domain by <https://www.virustotal.com/gui/home/url VirusTotal> . To correct the problem, search for your domain name on the <https://www.virustotal.com/gui/home/url VirusTotal> website. If your domain is reported as suspicious, see <https://www.google.com/webmasters/hacked/?hl=en Google Help for Hacked Websites> to learn what you can do. If you believe that the result is a false positive, notify the organization that is reporting the domain. VirusTotal is an aggregate of several antivirus and URL scanners and cannot remove your domain from a block list itself. After you correct the problem and the VirusTotal registry has been updated, request a new certificate. If you see this error and your domain is not included in the VirusTotal list, visit the <https://console.aws.amazon.com/support/home AWS Support Center> and create a case.     * __@INVALID_PUBLIC_DOMAIN@ __ - One or more of the domain names in the certificate request is not valid. Typically, this is because a domain name in the request is not a valid top-level domain. Try to request a certificate again, correcting any spelling errors or typos that were in the failed request, and ensure that all domain names in the request are for valid top-level domains. For example, you cannot request a certificate for @example.invalidpublicdomain@ because @invalidpublicdomain@ is not a valid top-level domain.     * __@OTHER@ __ - Typically, this failure occurs when there is a typographical error in one or more of the domain names in the certificate request. Try to request a certificate again, correcting any spelling errors or typos that were in the failed request.
cerRequestFailureReason :: Lens' Certificate (Maybe Text)
cerRequestFailureReason = lens _cerRequestFailureReason (\s a -> s {_cerRequestFailureReason = a})

-- | The timestamp when the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
cerRevokedAt :: Lens' Certificate (Maybe UTCTime)
cerRevokedAt = lens _cerRevokedAt (\s a -> s {_cerRevokedAt = a}) . mapping _Time

-- | The timestamp when the certificate is first valid.
cerNotBefore :: Lens' Certificate (Maybe UTCTime)
cerNotBefore = lens _cerNotBefore (\s a -> s {_cerNotBefore = a}) . mapping _Time

-- | The reason the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
cerRevocationReason :: Lens' Certificate (Maybe Text)
cerRevocationReason = lens _cerRevocationReason (\s a -> s {_cerRevocationReason = a})

-- | The domain name of the certificate.
cerDomainName :: Lens' Certificate (Maybe Text)
cerDomainName = lens _cerDomainName (\s a -> s {_cerDomainName = a})

-- | The name of the certificate (e.g., @my-certificate@ ).
cerName :: Lens' Certificate (Maybe Text)
cerName = lens _cerName (\s a -> s {_cerName = a})

-- | An object that describes the status of the certificate renewal managed by Lightsail.
cerRenewalSummary :: Lens' Certificate (Maybe RenewalSummary)
cerRenewalSummary = lens _cerRenewalSummary (\s a -> s {_cerRenewalSummary = a})

-- | The support code. Include this code in your email to support when you have questions about your Lightsail certificate. This code enables our support team to look up your Lightsail information more easily.
cerSupportCode :: Lens' Certificate (Maybe Text)
cerSupportCode = lens _cerSupportCode (\s a -> s {_cerSupportCode = a})

-- | An array of objects that describe the domain validation records of the certificate.
cerDomainValidationRecords :: Lens' Certificate [DomainValidationRecord]
cerDomainValidationRecords = lens _cerDomainValidationRecords (\s a -> s {_cerDomainValidationRecords = a}) . _Default . _Coerce

-- | The number of Lightsail resources that the certificate is attached to.
cerInUseResourceCount :: Lens' Certificate (Maybe Int)
cerInUseResourceCount = lens _cerInUseResourceCount (\s a -> s {_cerInUseResourceCount = a})

-- | The timestamp when the certificate was issued.
cerIssuedAt :: Lens' Certificate (Maybe UTCTime)
cerIssuedAt = lens _cerIssuedAt (\s a -> s {_cerIssuedAt = a}) . mapping _Time

-- | The algorithm used to generate the key pair (the public and private key) of the certificate.
cerKeyAlgorithm :: Lens' Certificate (Maybe Text)
cerKeyAlgorithm = lens _cerKeyAlgorithm (\s a -> s {_cerKeyAlgorithm = a})

-- | The serial number of the certificate.
cerSerialNumber :: Lens' Certificate (Maybe Text)
cerSerialNumber = lens _cerSerialNumber (\s a -> s {_cerSerialNumber = a})

-- | The certificate authority that issued the certificate.
cerIssuerCA :: Lens' Certificate (Maybe Text)
cerIssuerCA = lens _cerIssuerCA (\s a -> s {_cerIssuerCA = a})

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
cerTags :: Lens' Certificate [Tag]
cerTags = lens _cerTags (\s a -> s {_cerTags = a}) . _Default . _Coerce

-- | The timestamp when the certificate expires.
cerNotAfter :: Lens' Certificate (Maybe UTCTime)
cerNotAfter = lens _cerNotAfter (\s a -> s {_cerNotAfter = a}) . mapping _Time

instance FromJSON Certificate where
  parseJSON =
    withObject
      "Certificate"
      ( \x ->
          Certificate'
            <$> (x .:? "status")
            <*> (x .:? "subjectAlternativeNames" .!= mempty)
            <*> (x .:? "arn")
            <*> (x .:? "createdAt")
            <*> (x .:? "eligibleToRenew")
            <*> (x .:? "requestFailureReason")
            <*> (x .:? "revokedAt")
            <*> (x .:? "notBefore")
            <*> (x .:? "revocationReason")
            <*> (x .:? "domainName")
            <*> (x .:? "name")
            <*> (x .:? "renewalSummary")
            <*> (x .:? "supportCode")
            <*> (x .:? "domainValidationRecords" .!= mempty)
            <*> (x .:? "inUseResourceCount")
            <*> (x .:? "issuedAt")
            <*> (x .:? "keyAlgorithm")
            <*> (x .:? "serialNumber")
            <*> (x .:? "issuerCA")
            <*> (x .:? "tags" .!= mempty)
            <*> (x .:? "notAfter")
      )

instance Hashable Certificate

instance NFData Certificate

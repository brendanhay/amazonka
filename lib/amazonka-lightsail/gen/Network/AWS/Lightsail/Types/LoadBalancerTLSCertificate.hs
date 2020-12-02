{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTLSCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificate where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainValidationRecord
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateFailureReason
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRenewalSummary
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRevocationReason
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateStatus
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import Network.AWS.Prelude

-- | Describes a load balancer SSL/TLS certificate.
--
--
-- TLS is just an updated, more secure version of Secure Socket Layer (SSL).
--
--
-- /See:/ 'loadBalancerTLSCertificate' smart constructor.
data LoadBalancerTLSCertificate = LoadBalancerTLSCertificate'
  { _lbtcFailureReason ::
      !( Maybe
           LoadBalancerTLSCertificateFailureReason
       ),
    _lbtcSubject :: !(Maybe Text),
    _lbtcStatus ::
      !( Maybe
           LoadBalancerTLSCertificateStatus
       ),
    _lbtcSubjectAlternativeNames ::
      !(Maybe [Text]),
    _lbtcResourceType ::
      !(Maybe ResourceType),
    _lbtcArn :: !(Maybe Text),
    _lbtcCreatedAt :: !(Maybe POSIX),
    _lbtcLocation ::
      !(Maybe ResourceLocation),
    _lbtcLoadBalancerName ::
      !(Maybe Text),
    _lbtcSerial :: !(Maybe Text),
    _lbtcIsAttached :: !(Maybe Bool),
    _lbtcRevokedAt :: !(Maybe POSIX),
    _lbtcNotBefore :: !(Maybe POSIX),
    _lbtcRevocationReason ::
      !( Maybe
           LoadBalancerTLSCertificateRevocationReason
       ),
    _lbtcDomainName :: !(Maybe Text),
    _lbtcName :: !(Maybe Text),
    _lbtcRenewalSummary ::
      !( Maybe
           LoadBalancerTLSCertificateRenewalSummary
       ),
    _lbtcSupportCode :: !(Maybe Text),
    _lbtcDomainValidationRecords ::
      !( Maybe
           [LoadBalancerTLSCertificateDomainValidationRecord]
       ),
    _lbtcIssuedAt :: !(Maybe POSIX),
    _lbtcKeyAlgorithm :: !(Maybe Text),
    _lbtcSignatureAlgorithm ::
      !(Maybe Text),
    _lbtcIssuer :: !(Maybe Text),
    _lbtcTags :: !(Maybe [Tag]),
    _lbtcNotAfter :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoadBalancerTLSCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbtcFailureReason' - The validation failure reason, if any, of the certificate. The following failure reasons are possible:     * __@NO_AVAILABLE_CONTACTS@ __ - This failure applies to email validation, which is not available for Lightsail certificates.     * __@ADDITIONAL_VERIFICATION_REQUIRED@ __ - Lightsail requires additional information to process this certificate request. This can happen as a fraud-protection measure, such as when the domain ranks within the Alexa top 1000 websites. To provide the required information, use the <https://console.aws.amazon.com/support/home AWS Support Center> to contact AWS Support.     * __@DOMAIN_NOT_ALLOWED@ __ - One or more of the domain names in the certificate request was reported as an unsafe domain by <https://www.virustotal.com/gui/home/url VirusTotal> . To correct the problem, search for your domain name on the <https://www.virustotal.com/gui/home/url VirusTotal> website. If your domain is reported as suspicious, see <https://www.google.com/webmasters/hacked/?hl=en Google Help for Hacked Websites> to learn what you can do. If you believe that the result is a false positive, notify the organization that is reporting the domain. VirusTotal is an aggregate of several antivirus and URL scanners and cannot remove your domain from a block list itself. After you correct the problem and the VirusTotal registry has been updated, request a new certificate. If you see this error and your domain is not included in the VirusTotal list, visit the <https://console.aws.amazon.com/support/home AWS Support Center> and create a case.     * __@INVALID_PUBLIC_DOMAIN@ __ - One or more of the domain names in the certificate request is not valid. Typically, this is because a domain name in the request is not a valid top-level domain. Try to request a certificate again, correcting any spelling errors or typos that were in the failed request, and ensure that all domain names in the request are for valid top-level domains. For example, you cannot request a certificate for @example.invalidpublicdomain@ because @invalidpublicdomain@ is not a valid top-level domain.     * __@OTHER@ __ - Typically, this failure occurs when there is a typographical error in one or more of the domain names in the certificate request. Try to request a certificate again, correcting any spelling errors or typos that were in the failed request.
--
-- * 'lbtcSubject' - The name of the entity that is associated with the public key contained in the certificate.
--
-- * 'lbtcStatus' - The validation status of the SSL/TLS certificate. Valid values are below.
--
-- * 'lbtcSubjectAlternativeNames' - An array of strings that specify the alternate domains (e.g., @example2.com@ ) and subdomains (e.g., @blog.example.com@ ) for the certificate.
--
-- * 'lbtcResourceType' - The resource type (e.g., @LoadBalancerTlsCertificate@ ).     * __@Instance@ __ - A Lightsail instance (a virtual private server)     * __@StaticIp@ __ - A static IP address     * __@KeyPair@ __ - The key pair used to connect to a Lightsail instance     * __@InstanceSnapshot@ __ - A Lightsail instance snapshot     * __@Domain@ __ - A DNS zone     * __@PeeredVpc@ __ - A peered VPC     * __@LoadBalancer@ __ - A Lightsail load balancer     * __@LoadBalancerTlsCertificate@ __ - An SSL/TLS certificate associated with a Lightsail load balancer     * __@Disk@ __ - A Lightsail block storage disk     * __@DiskSnapshot@ __ - A block storage disk snapshot
--
-- * 'lbtcArn' - The Amazon Resource Name (ARN) of the SSL/TLS certificate.
--
-- * 'lbtcCreatedAt' - The time when you created your SSL/TLS certificate.
--
-- * 'lbtcLocation' - The AWS Region and Availability Zone where you created your certificate.
--
-- * 'lbtcLoadBalancerName' - The load balancer name where your SSL/TLS certificate is attached.
--
-- * 'lbtcSerial' - The serial number of the certificate.
--
-- * 'lbtcIsAttached' - When @true@ , the SSL/TLS certificate is attached to the Lightsail load balancer.
--
-- * 'lbtcRevokedAt' - The timestamp when the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
--
-- * 'lbtcNotBefore' - The timestamp when the SSL/TLS certificate is first valid.
--
-- * 'lbtcRevocationReason' - The reason the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
--
-- * 'lbtcDomainName' - The domain name for your SSL/TLS certificate.
--
-- * 'lbtcName' - The name of the SSL/TLS certificate (e.g., @my-certificate@ ).
--
-- * 'lbtcRenewalSummary' - An object that describes the status of the certificate renewal managed by Lightsail.
--
-- * 'lbtcSupportCode' - The support code. Include this code in your email to support when you have questions about your Lightsail load balancer or SSL/TLS certificate. This code enables our support team to look up your Lightsail information more easily.
--
-- * 'lbtcDomainValidationRecords' - An array of LoadBalancerTlsCertificateDomainValidationRecord objects describing the records.
--
-- * 'lbtcIssuedAt' - The time when the SSL/TLS certificate was issued.
--
-- * 'lbtcKeyAlgorithm' - The algorithm used to generate the key pair (the public and private key).
--
-- * 'lbtcSignatureAlgorithm' - The algorithm that was used to sign the certificate.
--
-- * 'lbtcIssuer' - The issuer of the certificate.
--
-- * 'lbtcTags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- * 'lbtcNotAfter' - The timestamp when the SSL/TLS certificate expires.
loadBalancerTLSCertificate ::
  LoadBalancerTLSCertificate
loadBalancerTLSCertificate =
  LoadBalancerTLSCertificate'
    { _lbtcFailureReason = Nothing,
      _lbtcSubject = Nothing,
      _lbtcStatus = Nothing,
      _lbtcSubjectAlternativeNames = Nothing,
      _lbtcResourceType = Nothing,
      _lbtcArn = Nothing,
      _lbtcCreatedAt = Nothing,
      _lbtcLocation = Nothing,
      _lbtcLoadBalancerName = Nothing,
      _lbtcSerial = Nothing,
      _lbtcIsAttached = Nothing,
      _lbtcRevokedAt = Nothing,
      _lbtcNotBefore = Nothing,
      _lbtcRevocationReason = Nothing,
      _lbtcDomainName = Nothing,
      _lbtcName = Nothing,
      _lbtcRenewalSummary = Nothing,
      _lbtcSupportCode = Nothing,
      _lbtcDomainValidationRecords = Nothing,
      _lbtcIssuedAt = Nothing,
      _lbtcKeyAlgorithm = Nothing,
      _lbtcSignatureAlgorithm = Nothing,
      _lbtcIssuer = Nothing,
      _lbtcTags = Nothing,
      _lbtcNotAfter = Nothing
    }

-- | The validation failure reason, if any, of the certificate. The following failure reasons are possible:     * __@NO_AVAILABLE_CONTACTS@ __ - This failure applies to email validation, which is not available for Lightsail certificates.     * __@ADDITIONAL_VERIFICATION_REQUIRED@ __ - Lightsail requires additional information to process this certificate request. This can happen as a fraud-protection measure, such as when the domain ranks within the Alexa top 1000 websites. To provide the required information, use the <https://console.aws.amazon.com/support/home AWS Support Center> to contact AWS Support.     * __@DOMAIN_NOT_ALLOWED@ __ - One or more of the domain names in the certificate request was reported as an unsafe domain by <https://www.virustotal.com/gui/home/url VirusTotal> . To correct the problem, search for your domain name on the <https://www.virustotal.com/gui/home/url VirusTotal> website. If your domain is reported as suspicious, see <https://www.google.com/webmasters/hacked/?hl=en Google Help for Hacked Websites> to learn what you can do. If you believe that the result is a false positive, notify the organization that is reporting the domain. VirusTotal is an aggregate of several antivirus and URL scanners and cannot remove your domain from a block list itself. After you correct the problem and the VirusTotal registry has been updated, request a new certificate. If you see this error and your domain is not included in the VirusTotal list, visit the <https://console.aws.amazon.com/support/home AWS Support Center> and create a case.     * __@INVALID_PUBLIC_DOMAIN@ __ - One or more of the domain names in the certificate request is not valid. Typically, this is because a domain name in the request is not a valid top-level domain. Try to request a certificate again, correcting any spelling errors or typos that were in the failed request, and ensure that all domain names in the request are for valid top-level domains. For example, you cannot request a certificate for @example.invalidpublicdomain@ because @invalidpublicdomain@ is not a valid top-level domain.     * __@OTHER@ __ - Typically, this failure occurs when there is a typographical error in one or more of the domain names in the certificate request. Try to request a certificate again, correcting any spelling errors or typos that were in the failed request.
lbtcFailureReason :: Lens' LoadBalancerTLSCertificate (Maybe LoadBalancerTLSCertificateFailureReason)
lbtcFailureReason = lens _lbtcFailureReason (\s a -> s {_lbtcFailureReason = a})

-- | The name of the entity that is associated with the public key contained in the certificate.
lbtcSubject :: Lens' LoadBalancerTLSCertificate (Maybe Text)
lbtcSubject = lens _lbtcSubject (\s a -> s {_lbtcSubject = a})

-- | The validation status of the SSL/TLS certificate. Valid values are below.
lbtcStatus :: Lens' LoadBalancerTLSCertificate (Maybe LoadBalancerTLSCertificateStatus)
lbtcStatus = lens _lbtcStatus (\s a -> s {_lbtcStatus = a})

-- | An array of strings that specify the alternate domains (e.g., @example2.com@ ) and subdomains (e.g., @blog.example.com@ ) for the certificate.
lbtcSubjectAlternativeNames :: Lens' LoadBalancerTLSCertificate [Text]
lbtcSubjectAlternativeNames = lens _lbtcSubjectAlternativeNames (\s a -> s {_lbtcSubjectAlternativeNames = a}) . _Default . _Coerce

-- | The resource type (e.g., @LoadBalancerTlsCertificate@ ).     * __@Instance@ __ - A Lightsail instance (a virtual private server)     * __@StaticIp@ __ - A static IP address     * __@KeyPair@ __ - The key pair used to connect to a Lightsail instance     * __@InstanceSnapshot@ __ - A Lightsail instance snapshot     * __@Domain@ __ - A DNS zone     * __@PeeredVpc@ __ - A peered VPC     * __@LoadBalancer@ __ - A Lightsail load balancer     * __@LoadBalancerTlsCertificate@ __ - An SSL/TLS certificate associated with a Lightsail load balancer     * __@Disk@ __ - A Lightsail block storage disk     * __@DiskSnapshot@ __ - A block storage disk snapshot
lbtcResourceType :: Lens' LoadBalancerTLSCertificate (Maybe ResourceType)
lbtcResourceType = lens _lbtcResourceType (\s a -> s {_lbtcResourceType = a})

-- | The Amazon Resource Name (ARN) of the SSL/TLS certificate.
lbtcArn :: Lens' LoadBalancerTLSCertificate (Maybe Text)
lbtcArn = lens _lbtcArn (\s a -> s {_lbtcArn = a})

-- | The time when you created your SSL/TLS certificate.
lbtcCreatedAt :: Lens' LoadBalancerTLSCertificate (Maybe UTCTime)
lbtcCreatedAt = lens _lbtcCreatedAt (\s a -> s {_lbtcCreatedAt = a}) . mapping _Time

-- | The AWS Region and Availability Zone where you created your certificate.
lbtcLocation :: Lens' LoadBalancerTLSCertificate (Maybe ResourceLocation)
lbtcLocation = lens _lbtcLocation (\s a -> s {_lbtcLocation = a})

-- | The load balancer name where your SSL/TLS certificate is attached.
lbtcLoadBalancerName :: Lens' LoadBalancerTLSCertificate (Maybe Text)
lbtcLoadBalancerName = lens _lbtcLoadBalancerName (\s a -> s {_lbtcLoadBalancerName = a})

-- | The serial number of the certificate.
lbtcSerial :: Lens' LoadBalancerTLSCertificate (Maybe Text)
lbtcSerial = lens _lbtcSerial (\s a -> s {_lbtcSerial = a})

-- | When @true@ , the SSL/TLS certificate is attached to the Lightsail load balancer.
lbtcIsAttached :: Lens' LoadBalancerTLSCertificate (Maybe Bool)
lbtcIsAttached = lens _lbtcIsAttached (\s a -> s {_lbtcIsAttached = a})

-- | The timestamp when the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
lbtcRevokedAt :: Lens' LoadBalancerTLSCertificate (Maybe UTCTime)
lbtcRevokedAt = lens _lbtcRevokedAt (\s a -> s {_lbtcRevokedAt = a}) . mapping _Time

-- | The timestamp when the SSL/TLS certificate is first valid.
lbtcNotBefore :: Lens' LoadBalancerTLSCertificate (Maybe UTCTime)
lbtcNotBefore = lens _lbtcNotBefore (\s a -> s {_lbtcNotBefore = a}) . mapping _Time

-- | The reason the certificate was revoked. This value is present only when the certificate status is @REVOKED@ .
lbtcRevocationReason :: Lens' LoadBalancerTLSCertificate (Maybe LoadBalancerTLSCertificateRevocationReason)
lbtcRevocationReason = lens _lbtcRevocationReason (\s a -> s {_lbtcRevocationReason = a})

-- | The domain name for your SSL/TLS certificate.
lbtcDomainName :: Lens' LoadBalancerTLSCertificate (Maybe Text)
lbtcDomainName = lens _lbtcDomainName (\s a -> s {_lbtcDomainName = a})

-- | The name of the SSL/TLS certificate (e.g., @my-certificate@ ).
lbtcName :: Lens' LoadBalancerTLSCertificate (Maybe Text)
lbtcName = lens _lbtcName (\s a -> s {_lbtcName = a})

-- | An object that describes the status of the certificate renewal managed by Lightsail.
lbtcRenewalSummary :: Lens' LoadBalancerTLSCertificate (Maybe LoadBalancerTLSCertificateRenewalSummary)
lbtcRenewalSummary = lens _lbtcRenewalSummary (\s a -> s {_lbtcRenewalSummary = a})

-- | The support code. Include this code in your email to support when you have questions about your Lightsail load balancer or SSL/TLS certificate. This code enables our support team to look up your Lightsail information more easily.
lbtcSupportCode :: Lens' LoadBalancerTLSCertificate (Maybe Text)
lbtcSupportCode = lens _lbtcSupportCode (\s a -> s {_lbtcSupportCode = a})

-- | An array of LoadBalancerTlsCertificateDomainValidationRecord objects describing the records.
lbtcDomainValidationRecords :: Lens' LoadBalancerTLSCertificate [LoadBalancerTLSCertificateDomainValidationRecord]
lbtcDomainValidationRecords = lens _lbtcDomainValidationRecords (\s a -> s {_lbtcDomainValidationRecords = a}) . _Default . _Coerce

-- | The time when the SSL/TLS certificate was issued.
lbtcIssuedAt :: Lens' LoadBalancerTLSCertificate (Maybe UTCTime)
lbtcIssuedAt = lens _lbtcIssuedAt (\s a -> s {_lbtcIssuedAt = a}) . mapping _Time

-- | The algorithm used to generate the key pair (the public and private key).
lbtcKeyAlgorithm :: Lens' LoadBalancerTLSCertificate (Maybe Text)
lbtcKeyAlgorithm = lens _lbtcKeyAlgorithm (\s a -> s {_lbtcKeyAlgorithm = a})

-- | The algorithm that was used to sign the certificate.
lbtcSignatureAlgorithm :: Lens' LoadBalancerTLSCertificate (Maybe Text)
lbtcSignatureAlgorithm = lens _lbtcSignatureAlgorithm (\s a -> s {_lbtcSignatureAlgorithm = a})

-- | The issuer of the certificate.
lbtcIssuer :: Lens' LoadBalancerTLSCertificate (Maybe Text)
lbtcIssuer = lens _lbtcIssuer (\s a -> s {_lbtcIssuer = a})

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
lbtcTags :: Lens' LoadBalancerTLSCertificate [Tag]
lbtcTags = lens _lbtcTags (\s a -> s {_lbtcTags = a}) . _Default . _Coerce

-- | The timestamp when the SSL/TLS certificate expires.
lbtcNotAfter :: Lens' LoadBalancerTLSCertificate (Maybe UTCTime)
lbtcNotAfter = lens _lbtcNotAfter (\s a -> s {_lbtcNotAfter = a}) . mapping _Time

instance FromJSON LoadBalancerTLSCertificate where
  parseJSON =
    withObject
      "LoadBalancerTLSCertificate"
      ( \x ->
          LoadBalancerTLSCertificate'
            <$> (x .:? "failureReason")
            <*> (x .:? "subject")
            <*> (x .:? "status")
            <*> (x .:? "subjectAlternativeNames" .!= mempty)
            <*> (x .:? "resourceType")
            <*> (x .:? "arn")
            <*> (x .:? "createdAt")
            <*> (x .:? "location")
            <*> (x .:? "loadBalancerName")
            <*> (x .:? "serial")
            <*> (x .:? "isAttached")
            <*> (x .:? "revokedAt")
            <*> (x .:? "notBefore")
            <*> (x .:? "revocationReason")
            <*> (x .:? "domainName")
            <*> (x .:? "name")
            <*> (x .:? "renewalSummary")
            <*> (x .:? "supportCode")
            <*> (x .:? "domainValidationRecords" .!= mempty)
            <*> (x .:? "issuedAt")
            <*> (x .:? "keyAlgorithm")
            <*> (x .:? "signatureAlgorithm")
            <*> (x .:? "issuer")
            <*> (x .:? "tags" .!= mempty)
            <*> (x .:? "notAfter")
      )

instance Hashable LoadBalancerTLSCertificate

instance NFData LoadBalancerTLSCertificate

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManager.Types.Product where

import Network.AWS.CertificateManager.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains metadata about an ACM certificate. This structure is returned in the response to a 'DescribeCertificate' request.
--
--
--
-- /See:/ 'certificateDetail' smart constructor.
data CertificateDetail = CertificateDetail'
  { _cdSubject                 :: !(Maybe Text)
  , _cdStatus                  :: !(Maybe CertificateStatus)
  , _cdFailureReason           :: !(Maybe FailureReason)
  , _cdSubjectAlternativeNames :: !(Maybe (List1 Text))
  , _cdInUseBy                 :: !(Maybe [Text])
  , _cdCreatedAt               :: !(Maybe POSIX)
  , _cdCertificateARN          :: !(Maybe Text)
  , _cdSerial                  :: !(Maybe Text)
  , _cdRenewalEligibility      :: !(Maybe RenewalEligibility)
  , _cdExtendedKeyUsages       :: !(Maybe [ExtendedKeyUsage])
  , _cdImportedAt              :: !(Maybe POSIX)
  , _cdKeyUsages               :: !(Maybe [KeyUsage])
  , _cdRevokedAt               :: !(Maybe POSIX)
  , _cdNotBefore               :: !(Maybe POSIX)
  , _cdRevocationReason        :: !(Maybe RevocationReason)
  , _cdDomainName              :: !(Maybe Text)
  , _cdRenewalSummary          :: !(Maybe RenewalSummary)
  , _cdKeyAlgorithm            :: !(Maybe KeyAlgorithm)
  , _cdType                    :: !(Maybe CertificateType)
  , _cdOptions                 :: !(Maybe CertificateOptions)
  , _cdIssuedAt                :: !(Maybe POSIX)
  , _cdSignatureAlgorithm      :: !(Maybe Text)
  , _cdDomainValidationOptions :: !(Maybe (List1 DomainValidation))
  , _cdIssuer                  :: !(Maybe Text)
  , _cdNotAfter                :: !(Maybe POSIX)
  , _cdCertificateAuthorityARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CertificateDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdSubject' - The name of the entity that is associated with the public key contained in the certificate.
--
-- * 'cdStatus' - The status of the certificate.
--
-- * 'cdFailureReason' - The reason the certificate request failed. This value exists only when the certificate status is @FAILED@ . For more information, see <http://docs.aws.amazon.com/acm/latest/userguide/troubleshooting.html#troubleshooting-failed Certificate Request Failed> in the /AWS Certificate Manager User Guide/ .
--
-- * 'cdSubjectAlternativeNames' - One or more domain names (subject alternative names) included in the certificate. This list contains the domain names that are bound to the public key that is contained in the certificate. The subject alternative names include the canonical domain name (CN) of the certificate and additional domain names that can be used to connect to the website.
--
-- * 'cdInUseBy' - A list of ARNs for the AWS resources that are using the certificate. A certificate can be used by multiple AWS resources.
--
-- * 'cdCreatedAt' - The time at which the certificate was requested. This value exists only when the certificate type is @AMAZON_ISSUED@ .
--
-- * 'cdCertificateARN' - The Amazon Resource Name (ARN) of the certificate. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- * 'cdSerial' - The serial number of the certificate.
--
-- * 'cdRenewalEligibility' - Specifies whether the certificate is eligible for renewal.
--
-- * 'cdExtendedKeyUsages' - Contains a list of Extended Key Usage X.509 v3 extension objects. Each object specifies a purpose for which the certificate public key can be used and consists of a name and an object identifier (OID).
--
-- * 'cdImportedAt' - The date and time at which the certificate was imported. This value exists only when the certificate type is @IMPORTED@ .
--
-- * 'cdKeyUsages' - A list of Key Usage X.509 v3 extension objects. Each object is a string value that identifies the purpose of the public key contained in the certificate. Possible extension values include DIGITAL_SIGNATURE, KEY_ENCHIPHERMENT, NON_REPUDIATION, and more.
--
-- * 'cdRevokedAt' - The time at which the certificate was revoked. This value exists only when the certificate status is @REVOKED@ .
--
-- * 'cdNotBefore' - The time before which the certificate is not valid.
--
-- * 'cdRevocationReason' - The reason the certificate was revoked. This value exists only when the certificate status is @REVOKED@ .
--
-- * 'cdDomainName' - The fully qualified domain name for the certificate, such as www.example.com or example.com.
--
-- * 'cdRenewalSummary' - Contains information about the status of ACM's <http://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> for the certificate. This field exists only when the certificate type is @AMAZON_ISSUED@ .
--
-- * 'cdKeyAlgorithm' - The algorithm that was used to generate the public-private key pair.
--
-- * 'cdType' - The source of the certificate. For certificates provided by ACM, this value is @AMAZON_ISSUED@ . For certificates that you imported with 'ImportCertificate' , this value is @IMPORTED@ . ACM does not provide <http://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> for imported certificates. For more information about the differences between certificates that you import and those that ACM provides, see <http://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing Certificates> in the /AWS Certificate Manager User Guide/ .
--
-- * 'cdOptions' - Value that specifies whether to add the certificate to a transparency log. Certificate transparency makes it possible to detect SSL certificates that have been mistakenly or maliciously issued. A browser might respond to certificate that has not been logged by showing an error message. The logs are cryptographically secure.
--
-- * 'cdIssuedAt' - The time at which the certificate was issued. This value exists only when the certificate type is @AMAZON_ISSUED@ .
--
-- * 'cdSignatureAlgorithm' - The algorithm that was used to sign the certificate.
--
-- * 'cdDomainValidationOptions' - Contains information about the initial validation of each domain name that occurs as a result of the 'RequestCertificate' request. This field exists only when the certificate type is @AMAZON_ISSUED@ .
--
-- * 'cdIssuer' - The name of the certificate authority that issued and signed the certificate.
--
-- * 'cdNotAfter' - The time after which the certificate is not valid.
--
-- * 'cdCertificateAuthorityARN' - The Amazon Resource Name (ARN) of the ACM PCA private certificate authority (CA) that issued the certificate. This has the following format:  @arn:aws:acm-pca:region:account:certificate-authority/12345678-1234-1234-1234-123456789012@
certificateDetail
    :: CertificateDetail
certificateDetail =
  CertificateDetail'
    { _cdSubject = Nothing
    , _cdStatus = Nothing
    , _cdFailureReason = Nothing
    , _cdSubjectAlternativeNames = Nothing
    , _cdInUseBy = Nothing
    , _cdCreatedAt = Nothing
    , _cdCertificateARN = Nothing
    , _cdSerial = Nothing
    , _cdRenewalEligibility = Nothing
    , _cdExtendedKeyUsages = Nothing
    , _cdImportedAt = Nothing
    , _cdKeyUsages = Nothing
    , _cdRevokedAt = Nothing
    , _cdNotBefore = Nothing
    , _cdRevocationReason = Nothing
    , _cdDomainName = Nothing
    , _cdRenewalSummary = Nothing
    , _cdKeyAlgorithm = Nothing
    , _cdType = Nothing
    , _cdOptions = Nothing
    , _cdIssuedAt = Nothing
    , _cdSignatureAlgorithm = Nothing
    , _cdDomainValidationOptions = Nothing
    , _cdIssuer = Nothing
    , _cdNotAfter = Nothing
    , _cdCertificateAuthorityARN = Nothing
    }


-- | The name of the entity that is associated with the public key contained in the certificate.
cdSubject :: Lens' CertificateDetail (Maybe Text)
cdSubject = lens _cdSubject (\ s a -> s{_cdSubject = a})

-- | The status of the certificate.
cdStatus :: Lens' CertificateDetail (Maybe CertificateStatus)
cdStatus = lens _cdStatus (\ s a -> s{_cdStatus = a})

-- | The reason the certificate request failed. This value exists only when the certificate status is @FAILED@ . For more information, see <http://docs.aws.amazon.com/acm/latest/userguide/troubleshooting.html#troubleshooting-failed Certificate Request Failed> in the /AWS Certificate Manager User Guide/ .
cdFailureReason :: Lens' CertificateDetail (Maybe FailureReason)
cdFailureReason = lens _cdFailureReason (\ s a -> s{_cdFailureReason = a})

-- | One or more domain names (subject alternative names) included in the certificate. This list contains the domain names that are bound to the public key that is contained in the certificate. The subject alternative names include the canonical domain name (CN) of the certificate and additional domain names that can be used to connect to the website.
cdSubjectAlternativeNames :: Lens' CertificateDetail (Maybe (NonEmpty Text))
cdSubjectAlternativeNames = lens _cdSubjectAlternativeNames (\ s a -> s{_cdSubjectAlternativeNames = a}) . mapping _List1

-- | A list of ARNs for the AWS resources that are using the certificate. A certificate can be used by multiple AWS resources.
cdInUseBy :: Lens' CertificateDetail [Text]
cdInUseBy = lens _cdInUseBy (\ s a -> s{_cdInUseBy = a}) . _Default . _Coerce

-- | The time at which the certificate was requested. This value exists only when the certificate type is @AMAZON_ISSUED@ .
cdCreatedAt :: Lens' CertificateDetail (Maybe UTCTime)
cdCreatedAt = lens _cdCreatedAt (\ s a -> s{_cdCreatedAt = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the certificate. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
cdCertificateARN :: Lens' CertificateDetail (Maybe Text)
cdCertificateARN = lens _cdCertificateARN (\ s a -> s{_cdCertificateARN = a})

-- | The serial number of the certificate.
cdSerial :: Lens' CertificateDetail (Maybe Text)
cdSerial = lens _cdSerial (\ s a -> s{_cdSerial = a})

-- | Specifies whether the certificate is eligible for renewal.
cdRenewalEligibility :: Lens' CertificateDetail (Maybe RenewalEligibility)
cdRenewalEligibility = lens _cdRenewalEligibility (\ s a -> s{_cdRenewalEligibility = a})

-- | Contains a list of Extended Key Usage X.509 v3 extension objects. Each object specifies a purpose for which the certificate public key can be used and consists of a name and an object identifier (OID).
cdExtendedKeyUsages :: Lens' CertificateDetail [ExtendedKeyUsage]
cdExtendedKeyUsages = lens _cdExtendedKeyUsages (\ s a -> s{_cdExtendedKeyUsages = a}) . _Default . _Coerce

-- | The date and time at which the certificate was imported. This value exists only when the certificate type is @IMPORTED@ .
cdImportedAt :: Lens' CertificateDetail (Maybe UTCTime)
cdImportedAt = lens _cdImportedAt (\ s a -> s{_cdImportedAt = a}) . mapping _Time

-- | A list of Key Usage X.509 v3 extension objects. Each object is a string value that identifies the purpose of the public key contained in the certificate. Possible extension values include DIGITAL_SIGNATURE, KEY_ENCHIPHERMENT, NON_REPUDIATION, and more.
cdKeyUsages :: Lens' CertificateDetail [KeyUsage]
cdKeyUsages = lens _cdKeyUsages (\ s a -> s{_cdKeyUsages = a}) . _Default . _Coerce

-- | The time at which the certificate was revoked. This value exists only when the certificate status is @REVOKED@ .
cdRevokedAt :: Lens' CertificateDetail (Maybe UTCTime)
cdRevokedAt = lens _cdRevokedAt (\ s a -> s{_cdRevokedAt = a}) . mapping _Time

-- | The time before which the certificate is not valid.
cdNotBefore :: Lens' CertificateDetail (Maybe UTCTime)
cdNotBefore = lens _cdNotBefore (\ s a -> s{_cdNotBefore = a}) . mapping _Time

-- | The reason the certificate was revoked. This value exists only when the certificate status is @REVOKED@ .
cdRevocationReason :: Lens' CertificateDetail (Maybe RevocationReason)
cdRevocationReason = lens _cdRevocationReason (\ s a -> s{_cdRevocationReason = a})

-- | The fully qualified domain name for the certificate, such as www.example.com or example.com.
cdDomainName :: Lens' CertificateDetail (Maybe Text)
cdDomainName = lens _cdDomainName (\ s a -> s{_cdDomainName = a})

-- | Contains information about the status of ACM's <http://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> for the certificate. This field exists only when the certificate type is @AMAZON_ISSUED@ .
cdRenewalSummary :: Lens' CertificateDetail (Maybe RenewalSummary)
cdRenewalSummary = lens _cdRenewalSummary (\ s a -> s{_cdRenewalSummary = a})

-- | The algorithm that was used to generate the public-private key pair.
cdKeyAlgorithm :: Lens' CertificateDetail (Maybe KeyAlgorithm)
cdKeyAlgorithm = lens _cdKeyAlgorithm (\ s a -> s{_cdKeyAlgorithm = a})

-- | The source of the certificate. For certificates provided by ACM, this value is @AMAZON_ISSUED@ . For certificates that you imported with 'ImportCertificate' , this value is @IMPORTED@ . ACM does not provide <http://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> for imported certificates. For more information about the differences between certificates that you import and those that ACM provides, see <http://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing Certificates> in the /AWS Certificate Manager User Guide/ .
cdType :: Lens' CertificateDetail (Maybe CertificateType)
cdType = lens _cdType (\ s a -> s{_cdType = a})

-- | Value that specifies whether to add the certificate to a transparency log. Certificate transparency makes it possible to detect SSL certificates that have been mistakenly or maliciously issued. A browser might respond to certificate that has not been logged by showing an error message. The logs are cryptographically secure.
cdOptions :: Lens' CertificateDetail (Maybe CertificateOptions)
cdOptions = lens _cdOptions (\ s a -> s{_cdOptions = a})

-- | The time at which the certificate was issued. This value exists only when the certificate type is @AMAZON_ISSUED@ .
cdIssuedAt :: Lens' CertificateDetail (Maybe UTCTime)
cdIssuedAt = lens _cdIssuedAt (\ s a -> s{_cdIssuedAt = a}) . mapping _Time

-- | The algorithm that was used to sign the certificate.
cdSignatureAlgorithm :: Lens' CertificateDetail (Maybe Text)
cdSignatureAlgorithm = lens _cdSignatureAlgorithm (\ s a -> s{_cdSignatureAlgorithm = a})

-- | Contains information about the initial validation of each domain name that occurs as a result of the 'RequestCertificate' request. This field exists only when the certificate type is @AMAZON_ISSUED@ .
cdDomainValidationOptions :: Lens' CertificateDetail (Maybe (NonEmpty DomainValidation))
cdDomainValidationOptions = lens _cdDomainValidationOptions (\ s a -> s{_cdDomainValidationOptions = a}) . mapping _List1

-- | The name of the certificate authority that issued and signed the certificate.
cdIssuer :: Lens' CertificateDetail (Maybe Text)
cdIssuer = lens _cdIssuer (\ s a -> s{_cdIssuer = a})

-- | The time after which the certificate is not valid.
cdNotAfter :: Lens' CertificateDetail (Maybe UTCTime)
cdNotAfter = lens _cdNotAfter (\ s a -> s{_cdNotAfter = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the ACM PCA private certificate authority (CA) that issued the certificate. This has the following format:  @arn:aws:acm-pca:region:account:certificate-authority/12345678-1234-1234-1234-123456789012@
cdCertificateAuthorityARN :: Lens' CertificateDetail (Maybe Text)
cdCertificateAuthorityARN = lens _cdCertificateAuthorityARN (\ s a -> s{_cdCertificateAuthorityARN = a})

instance FromJSON CertificateDetail where
        parseJSON
          = withObject "CertificateDetail"
              (\ x ->
                 CertificateDetail' <$>
                   (x .:? "Subject") <*> (x .:? "Status") <*>
                     (x .:? "FailureReason")
                     <*> (x .:? "SubjectAlternativeNames")
                     <*> (x .:? "InUseBy" .!= mempty)
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "CertificateArn")
                     <*> (x .:? "Serial")
                     <*> (x .:? "RenewalEligibility")
                     <*> (x .:? "ExtendedKeyUsages" .!= mempty)
                     <*> (x .:? "ImportedAt")
                     <*> (x .:? "KeyUsages" .!= mempty)
                     <*> (x .:? "RevokedAt")
                     <*> (x .:? "NotBefore")
                     <*> (x .:? "RevocationReason")
                     <*> (x .:? "DomainName")
                     <*> (x .:? "RenewalSummary")
                     <*> (x .:? "KeyAlgorithm")
                     <*> (x .:? "Type")
                     <*> (x .:? "Options")
                     <*> (x .:? "IssuedAt")
                     <*> (x .:? "SignatureAlgorithm")
                     <*> (x .:? "DomainValidationOptions")
                     <*> (x .:? "Issuer")
                     <*> (x .:? "NotAfter")
                     <*> (x .:? "CertificateAuthorityArn"))

instance Hashable CertificateDetail where

instance NFData CertificateDetail where

-- | Structure that contains options for your certificate. Currently, you can use this only to specify whether to opt in to or out of certificate transparency logging. Some browsers require that public certificates issued for your domain be recorded in a log. Certificates that are not logged typically generate a browser error. Transparency makes it possible for you to detect SSL/TLS certificates that have been mistakenly or maliciously issued for your domain. For general information, see <http://docs.aws.amazon.com/acm/latest/userguide/acm-concepts.html#concept-transparency Certificate Transparency Logging> .
--
--
--
-- /See:/ 'certificateOptions' smart constructor.
newtype CertificateOptions = CertificateOptions'
  { _coCertificateTransparencyLoggingPreference :: Maybe CertificateTransparencyLoggingPreference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CertificateOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coCertificateTransparencyLoggingPreference' - You can opt out of certificate transparency logging by specifying the @DISABLED@ option. Opt in by specifying @ENABLED@ .
certificateOptions
    :: CertificateOptions
certificateOptions =
  CertificateOptions' {_coCertificateTransparencyLoggingPreference = Nothing}


-- | You can opt out of certificate transparency logging by specifying the @DISABLED@ option. Opt in by specifying @ENABLED@ .
coCertificateTransparencyLoggingPreference :: Lens' CertificateOptions (Maybe CertificateTransparencyLoggingPreference)
coCertificateTransparencyLoggingPreference = lens _coCertificateTransparencyLoggingPreference (\ s a -> s{_coCertificateTransparencyLoggingPreference = a})

instance FromJSON CertificateOptions where
        parseJSON
          = withObject "CertificateOptions"
              (\ x ->
                 CertificateOptions' <$>
                   (x .:? "CertificateTransparencyLoggingPreference"))

instance Hashable CertificateOptions where

instance NFData CertificateOptions where

instance ToJSON CertificateOptions where
        toJSON CertificateOptions'{..}
          = object
              (catMaybes
                 [("CertificateTransparencyLoggingPreference" .=) <$>
                    _coCertificateTransparencyLoggingPreference])

-- | This structure is returned in the response object of 'ListCertificates' action.
--
--
--
-- /See:/ 'certificateSummary' smart constructor.
data CertificateSummary = CertificateSummary'
  { _csCertificateARN :: !(Maybe Text)
  , _csDomainName     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CertificateSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csCertificateARN' - Amazon Resource Name (ARN) of the certificate. This is of the form: @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@  For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'csDomainName' - Fully qualified domain name (FQDN), such as www.example.com or example.com, for the certificate.
certificateSummary
    :: CertificateSummary
certificateSummary =
  CertificateSummary' {_csCertificateARN = Nothing, _csDomainName = Nothing}


-- | Amazon Resource Name (ARN) of the certificate. This is of the form: @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@  For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
csCertificateARN :: Lens' CertificateSummary (Maybe Text)
csCertificateARN = lens _csCertificateARN (\ s a -> s{_csCertificateARN = a})

-- | Fully qualified domain name (FQDN), such as www.example.com or example.com, for the certificate.
csDomainName :: Lens' CertificateSummary (Maybe Text)
csDomainName = lens _csDomainName (\ s a -> s{_csDomainName = a})

instance FromJSON CertificateSummary where
        parseJSON
          = withObject "CertificateSummary"
              (\ x ->
                 CertificateSummary' <$>
                   (x .:? "CertificateArn") <*> (x .:? "DomainName"))

instance Hashable CertificateSummary where

instance NFData CertificateSummary where

-- | Contains information about the validation of each domain name in the certificate.
--
--
--
-- /See:/ 'domainValidation' smart constructor.
data DomainValidation = DomainValidation'
  { _dvValidationEmails :: !(Maybe [Text])
  , _dvValidationMethod :: !(Maybe ValidationMethod)
  , _dvResourceRecord   :: !(Maybe ResourceRecord)
  , _dvValidationStatus :: !(Maybe DomainStatus)
  , _dvValidationDomain :: !(Maybe Text)
  , _dvDomainName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DomainValidation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvValidationEmails' - A list of email addresses that ACM used to send domain validation emails.
--
-- * 'dvValidationMethod' - Specifies the domain validation method.
--
-- * 'dvResourceRecord' - Contains the CNAME record that you add to your DNS database for domain validation. For more information, see <http://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-dns.html Use DNS to Validate Domain Ownership> .
--
-- * 'dvValidationStatus' - The validation status of the domain name. This can be one of the following values:     * @PENDING_VALIDATION@      * SUCCESS     * FAILED
--
-- * 'dvValidationDomain' - The domain name that ACM used to send domain validation emails.
--
-- * 'dvDomainName' - A fully qualified domain name (FQDN) in the certificate. For example, @www.example.com@ or @example.com@ .
domainValidation
    :: Text -- ^ 'dvDomainName'
    -> DomainValidation
domainValidation pDomainName_ =
  DomainValidation'
    { _dvValidationEmails = Nothing
    , _dvValidationMethod = Nothing
    , _dvResourceRecord = Nothing
    , _dvValidationStatus = Nothing
    , _dvValidationDomain = Nothing
    , _dvDomainName = pDomainName_
    }


-- | A list of email addresses that ACM used to send domain validation emails.
dvValidationEmails :: Lens' DomainValidation [Text]
dvValidationEmails = lens _dvValidationEmails (\ s a -> s{_dvValidationEmails = a}) . _Default . _Coerce

-- | Specifies the domain validation method.
dvValidationMethod :: Lens' DomainValidation (Maybe ValidationMethod)
dvValidationMethod = lens _dvValidationMethod (\ s a -> s{_dvValidationMethod = a})

-- | Contains the CNAME record that you add to your DNS database for domain validation. For more information, see <http://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-dns.html Use DNS to Validate Domain Ownership> .
dvResourceRecord :: Lens' DomainValidation (Maybe ResourceRecord)
dvResourceRecord = lens _dvResourceRecord (\ s a -> s{_dvResourceRecord = a})

-- | The validation status of the domain name. This can be one of the following values:     * @PENDING_VALIDATION@      * SUCCESS     * FAILED
dvValidationStatus :: Lens' DomainValidation (Maybe DomainStatus)
dvValidationStatus = lens _dvValidationStatus (\ s a -> s{_dvValidationStatus = a})

-- | The domain name that ACM used to send domain validation emails.
dvValidationDomain :: Lens' DomainValidation (Maybe Text)
dvValidationDomain = lens _dvValidationDomain (\ s a -> s{_dvValidationDomain = a})

-- | A fully qualified domain name (FQDN) in the certificate. For example, @www.example.com@ or @example.com@ .
dvDomainName :: Lens' DomainValidation Text
dvDomainName = lens _dvDomainName (\ s a -> s{_dvDomainName = a})

instance FromJSON DomainValidation where
        parseJSON
          = withObject "DomainValidation"
              (\ x ->
                 DomainValidation' <$>
                   (x .:? "ValidationEmails" .!= mempty) <*>
                     (x .:? "ValidationMethod")
                     <*> (x .:? "ResourceRecord")
                     <*> (x .:? "ValidationStatus")
                     <*> (x .:? "ValidationDomain")
                     <*> (x .: "DomainName"))

instance Hashable DomainValidation where

instance NFData DomainValidation where

-- | Contains information about the domain names that you want ACM to use to send you emails that enable you to validate domain ownership.
--
--
--
-- /See:/ 'domainValidationOption' smart constructor.
data DomainValidationOption = DomainValidationOption'
  { _dvoDomainName       :: !Text
  , _dvoValidationDomain :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DomainValidationOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvoDomainName' - A fully qualified domain name (FQDN) in the certificate request.
--
-- * 'dvoValidationDomain' - The domain name that you want ACM to use to send you validation emails. This domain name is the suffix of the email addresses that you want ACM to use. This must be the same as the @DomainName@ value or a superdomain of the @DomainName@ value. For example, if you request a certificate for @testing.example.com@ , you can specify @example.com@ for this value. In that case, ACM sends domain validation emails to the following five addresses:     * admin@example.com     * administrator@example.com     * hostmaster@example.com     * postmaster@example.com     * webmaster@example.com
domainValidationOption
    :: Text -- ^ 'dvoDomainName'
    -> Text -- ^ 'dvoValidationDomain'
    -> DomainValidationOption
domainValidationOption pDomainName_ pValidationDomain_ =
  DomainValidationOption'
    {_dvoDomainName = pDomainName_, _dvoValidationDomain = pValidationDomain_}


-- | A fully qualified domain name (FQDN) in the certificate request.
dvoDomainName :: Lens' DomainValidationOption Text
dvoDomainName = lens _dvoDomainName (\ s a -> s{_dvoDomainName = a})

-- | The domain name that you want ACM to use to send you validation emails. This domain name is the suffix of the email addresses that you want ACM to use. This must be the same as the @DomainName@ value or a superdomain of the @DomainName@ value. For example, if you request a certificate for @testing.example.com@ , you can specify @example.com@ for this value. In that case, ACM sends domain validation emails to the following five addresses:     * admin@example.com     * administrator@example.com     * hostmaster@example.com     * postmaster@example.com     * webmaster@example.com
dvoValidationDomain :: Lens' DomainValidationOption Text
dvoValidationDomain = lens _dvoValidationDomain (\ s a -> s{_dvoValidationDomain = a})

instance Hashable DomainValidationOption where

instance NFData DomainValidationOption where

instance ToJSON DomainValidationOption where
        toJSON DomainValidationOption'{..}
          = object
              (catMaybes
                 [Just ("DomainName" .= _dvoDomainName),
                  Just ("ValidationDomain" .= _dvoValidationDomain)])

-- | The Extended Key Usage X.509 v3 extension defines one or more purposes for which the public key can be used. This is in addition to or in place of the basic purposes specified by the Key Usage extension.
--
--
--
-- /See:/ 'extendedKeyUsage' smart constructor.
data ExtendedKeyUsage = ExtendedKeyUsage'
  { _ekuOId  :: !(Maybe Text)
  , _ekuName :: !(Maybe ExtendedKeyUsageName)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExtendedKeyUsage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ekuOId' - An object identifier (OID) for the extension value. OIDs are strings of numbers separated by periods. The following OIDs are defined in RFC 3280 and RFC 5280.      * @1.3.6.1.5.5.7.3.1 (TLS_WEB_SERVER_AUTHENTICATION)@      * @1.3.6.1.5.5.7.3.2 (TLS_WEB_CLIENT_AUTHENTICATION)@      * @1.3.6.1.5.5.7.3.3 (CODE_SIGNING)@      * @1.3.6.1.5.5.7.3.4 (EMAIL_PROTECTION)@      * @1.3.6.1.5.5.7.3.8 (TIME_STAMPING)@      * @1.3.6.1.5.5.7.3.9 (OCSP_SIGNING)@      * @1.3.6.1.5.5.7.3.5 (IPSEC_END_SYSTEM)@      * @1.3.6.1.5.5.7.3.6 (IPSEC_TUNNEL)@      * @1.3.6.1.5.5.7.3.7 (IPSEC_USER)@
--
-- * 'ekuName' - The name of an Extended Key Usage value.
extendedKeyUsage
    :: ExtendedKeyUsage
extendedKeyUsage = ExtendedKeyUsage' {_ekuOId = Nothing, _ekuName = Nothing}


-- | An object identifier (OID) for the extension value. OIDs are strings of numbers separated by periods. The following OIDs are defined in RFC 3280 and RFC 5280.      * @1.3.6.1.5.5.7.3.1 (TLS_WEB_SERVER_AUTHENTICATION)@      * @1.3.6.1.5.5.7.3.2 (TLS_WEB_CLIENT_AUTHENTICATION)@      * @1.3.6.1.5.5.7.3.3 (CODE_SIGNING)@      * @1.3.6.1.5.5.7.3.4 (EMAIL_PROTECTION)@      * @1.3.6.1.5.5.7.3.8 (TIME_STAMPING)@      * @1.3.6.1.5.5.7.3.9 (OCSP_SIGNING)@      * @1.3.6.1.5.5.7.3.5 (IPSEC_END_SYSTEM)@      * @1.3.6.1.5.5.7.3.6 (IPSEC_TUNNEL)@      * @1.3.6.1.5.5.7.3.7 (IPSEC_USER)@
ekuOId :: Lens' ExtendedKeyUsage (Maybe Text)
ekuOId = lens _ekuOId (\ s a -> s{_ekuOId = a})

-- | The name of an Extended Key Usage value.
ekuName :: Lens' ExtendedKeyUsage (Maybe ExtendedKeyUsageName)
ekuName = lens _ekuName (\ s a -> s{_ekuName = a})

instance FromJSON ExtendedKeyUsage where
        parseJSON
          = withObject "ExtendedKeyUsage"
              (\ x ->
                 ExtendedKeyUsage' <$>
                   (x .:? "OID") <*> (x .:? "Name"))

instance Hashable ExtendedKeyUsage where

instance NFData ExtendedKeyUsage where

-- | This structure can be used in the 'ListCertificates' action to filter the output of the certificate list.
--
--
--
-- /See:/ 'filters' smart constructor.
data Filters = Filters'
  { _fKeyTypes         :: !(Maybe [KeyAlgorithm])
  , _fKeyUsage         :: !(Maybe [KeyUsageName])
  , _fExtendedKeyUsage :: !(Maybe [ExtendedKeyUsageName])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Filters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fKeyTypes' - Specify one or more algorithms that can be used to generate key pairs.
--
-- * 'fKeyUsage' - Specify one or more 'KeyUsage' extension values.
--
-- * 'fExtendedKeyUsage' - Specify one or more 'ExtendedKeyUsage' extension values.
filters
    :: Filters
filters =
  Filters'
    {_fKeyTypes = Nothing, _fKeyUsage = Nothing, _fExtendedKeyUsage = Nothing}


-- | Specify one or more algorithms that can be used to generate key pairs.
fKeyTypes :: Lens' Filters [KeyAlgorithm]
fKeyTypes = lens _fKeyTypes (\ s a -> s{_fKeyTypes = a}) . _Default . _Coerce

-- | Specify one or more 'KeyUsage' extension values.
fKeyUsage :: Lens' Filters [KeyUsageName]
fKeyUsage = lens _fKeyUsage (\ s a -> s{_fKeyUsage = a}) . _Default . _Coerce

-- | Specify one or more 'ExtendedKeyUsage' extension values.
fExtendedKeyUsage :: Lens' Filters [ExtendedKeyUsageName]
fExtendedKeyUsage = lens _fExtendedKeyUsage (\ s a -> s{_fExtendedKeyUsage = a}) . _Default . _Coerce

instance Hashable Filters where

instance NFData Filters where

instance ToJSON Filters where
        toJSON Filters'{..}
          = object
              (catMaybes
                 [("keyTypes" .=) <$> _fKeyTypes,
                  ("keyUsage" .=) <$> _fKeyUsage,
                  ("extendedKeyUsage" .=) <$> _fExtendedKeyUsage])

-- | The Key Usage X.509 v3 extension defines the purpose of the public key contained in the certificate.
--
--
--
-- /See:/ 'keyUsage' smart constructor.
newtype KeyUsage = KeyUsage'
  { _kuName :: Maybe KeyUsageName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'KeyUsage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kuName' - A string value that contains a Key Usage extension name.
keyUsage
    :: KeyUsage
keyUsage = KeyUsage' {_kuName = Nothing}


-- | A string value that contains a Key Usage extension name.
kuName :: Lens' KeyUsage (Maybe KeyUsageName)
kuName = lens _kuName (\ s a -> s{_kuName = a})

instance FromJSON KeyUsage where
        parseJSON
          = withObject "KeyUsage"
              (\ x -> KeyUsage' <$> (x .:? "Name"))

instance Hashable KeyUsage where

instance NFData KeyUsage where

-- | Contains information about the status of ACM's <http://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> for the certificate. This structure exists only when the certificate type is @AMAZON_ISSUED@ .
--
--
--
-- /See:/ 'renewalSummary' smart constructor.
data RenewalSummary = RenewalSummary'
  { _rsRenewalStatus           :: !RenewalStatus
  , _rsDomainValidationOptions :: !(List1 DomainValidation)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RenewalSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsRenewalStatus' - The status of ACM's <http://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> of the certificate.
--
-- * 'rsDomainValidationOptions' - Contains information about the validation of each domain name in the certificate, as it pertains to ACM's <http://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> . This is different from the initial validation that occurs as a result of the 'RequestCertificate' request. This field exists only when the certificate type is @AMAZON_ISSUED@ .
renewalSummary
    :: RenewalStatus -- ^ 'rsRenewalStatus'
    -> NonEmpty DomainValidation -- ^ 'rsDomainValidationOptions'
    -> RenewalSummary
renewalSummary pRenewalStatus_ pDomainValidationOptions_ =
  RenewalSummary'
    { _rsRenewalStatus = pRenewalStatus_
    , _rsDomainValidationOptions = _List1 # pDomainValidationOptions_
    }


-- | The status of ACM's <http://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> of the certificate.
rsRenewalStatus :: Lens' RenewalSummary RenewalStatus
rsRenewalStatus = lens _rsRenewalStatus (\ s a -> s{_rsRenewalStatus = a})

-- | Contains information about the validation of each domain name in the certificate, as it pertains to ACM's <http://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> . This is different from the initial validation that occurs as a result of the 'RequestCertificate' request. This field exists only when the certificate type is @AMAZON_ISSUED@ .
rsDomainValidationOptions :: Lens' RenewalSummary (NonEmpty DomainValidation)
rsDomainValidationOptions = lens _rsDomainValidationOptions (\ s a -> s{_rsDomainValidationOptions = a}) . _List1

instance FromJSON RenewalSummary where
        parseJSON
          = withObject "RenewalSummary"
              (\ x ->
                 RenewalSummary' <$>
                   (x .: "RenewalStatus") <*>
                     (x .: "DomainValidationOptions"))

instance Hashable RenewalSummary where

instance NFData RenewalSummary where

-- | Contains a DNS record value that you can use to can use to validate ownership or control of a domain. This is used by the 'DescribeCertificate' action.
--
--
--
-- /See:/ 'resourceRecord' smart constructor.
data ResourceRecord = ResourceRecord'
  { _rrName  :: !Text
  , _rrType  :: !RecordType
  , _rrValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrName' - The name of the DNS record to create in your domain. This is supplied by ACM.
--
-- * 'rrType' - The type of DNS record. Currently this can be @CNAME@ .
--
-- * 'rrValue' - The value of the CNAME record to add to your DNS database. This is supplied by ACM.
resourceRecord
    :: Text -- ^ 'rrName'
    -> RecordType -- ^ 'rrType'
    -> Text -- ^ 'rrValue'
    -> ResourceRecord
resourceRecord pName_ pType_ pValue_ =
  ResourceRecord' {_rrName = pName_, _rrType = pType_, _rrValue = pValue_}


-- | The name of the DNS record to create in your domain. This is supplied by ACM.
rrName :: Lens' ResourceRecord Text
rrName = lens _rrName (\ s a -> s{_rrName = a})

-- | The type of DNS record. Currently this can be @CNAME@ .
rrType :: Lens' ResourceRecord RecordType
rrType = lens _rrType (\ s a -> s{_rrType = a})

-- | The value of the CNAME record to add to your DNS database. This is supplied by ACM.
rrValue :: Lens' ResourceRecord Text
rrValue = lens _rrValue (\ s a -> s{_rrValue = a})

instance FromJSON ResourceRecord where
        parseJSON
          = withObject "ResourceRecord"
              (\ x ->
                 ResourceRecord' <$>
                   (x .: "Name") <*> (x .: "Type") <*> (x .: "Value"))

instance Hashable ResourceRecord where

instance NFData ResourceRecord where

-- | A key-value pair that identifies or specifies metadata about an ACM resource.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - The value of the tag.
--
-- * 'tagKey' - The key of the tag.
tag
    :: Text -- ^ 'tagKey'
    -> Tag
tag pKey_ = Tag' {_tagValue = Nothing, _tagKey = pKey_}


-- | The value of the tag.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | The key of the tag.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "Value") <*> (x .: "Key"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _tagValue,
                  Just ("Key" .= _tagKey)])

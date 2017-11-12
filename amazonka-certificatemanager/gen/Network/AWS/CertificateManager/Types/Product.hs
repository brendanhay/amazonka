{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.Product
-- Copyright   : (c) 2013-2017 Brendan Hay
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
  , _cdImportedAt              :: !(Maybe POSIX)
  , _cdRevokedAt               :: !(Maybe POSIX)
  , _cdNotBefore               :: !(Maybe POSIX)
  , _cdRevocationReason        :: !(Maybe RevocationReason)
  , _cdDomainName              :: !(Maybe Text)
  , _cdRenewalSummary          :: !(Maybe RenewalSummary)
  , _cdKeyAlgorithm            :: !(Maybe KeyAlgorithm)
  , _cdType                    :: !(Maybe CertificateType)
  , _cdIssuedAt                :: !(Maybe POSIX)
  , _cdSignatureAlgorithm      :: !(Maybe Text)
  , _cdDomainValidationOptions :: !(Maybe (List1 DomainValidation))
  , _cdIssuer                  :: !(Maybe Text)
  , _cdNotAfter                :: !(Maybe POSIX)
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
-- * 'cdImportedAt' - The date and time at which the certificate was imported. This value exists only when the certificate type is @IMPORTED@ .
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
-- * 'cdKeyAlgorithm' - The algorithm that was used to generate the key pair (the public and private key).
--
-- * 'cdType' - The source of the certificate. For certificates provided by ACM, this value is @AMAZON_ISSUED@ . For certificates that you imported with 'ImportCertificate' , this value is @IMPORTED@ . ACM does not provide <http://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> for imported certificates. For more information about the differences between certificates that you import and those that ACM provides, see <http://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing Certificates> in the /AWS Certificate Manager User Guide/ .
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
  , _cdImportedAt = Nothing
  , _cdRevokedAt = Nothing
  , _cdNotBefore = Nothing
  , _cdRevocationReason = Nothing
  , _cdDomainName = Nothing
  , _cdRenewalSummary = Nothing
  , _cdKeyAlgorithm = Nothing
  , _cdType = Nothing
  , _cdIssuedAt = Nothing
  , _cdSignatureAlgorithm = Nothing
  , _cdDomainValidationOptions = Nothing
  , _cdIssuer = Nothing
  , _cdNotAfter = Nothing
  }


-- | The name of the entity that is associated with the public key contained in the certificate.
cdSubject :: Lens' CertificateDetail (Maybe Text)
cdSubject = lens _cdSubject (\ s a -> s{_cdSubject = a});

-- | The status of the certificate.
cdStatus :: Lens' CertificateDetail (Maybe CertificateStatus)
cdStatus = lens _cdStatus (\ s a -> s{_cdStatus = a});

-- | The reason the certificate request failed. This value exists only when the certificate status is @FAILED@ . For more information, see <http://docs.aws.amazon.com/acm/latest/userguide/troubleshooting.html#troubleshooting-failed Certificate Request Failed> in the /AWS Certificate Manager User Guide/ .
cdFailureReason :: Lens' CertificateDetail (Maybe FailureReason)
cdFailureReason = lens _cdFailureReason (\ s a -> s{_cdFailureReason = a});

-- | One or more domain names (subject alternative names) included in the certificate. This list contains the domain names that are bound to the public key that is contained in the certificate. The subject alternative names include the canonical domain name (CN) of the certificate and additional domain names that can be used to connect to the website.
cdSubjectAlternativeNames :: Lens' CertificateDetail (Maybe (NonEmpty Text))
cdSubjectAlternativeNames = lens _cdSubjectAlternativeNames (\ s a -> s{_cdSubjectAlternativeNames = a}) . mapping _List1;

-- | A list of ARNs for the AWS resources that are using the certificate. A certificate can be used by multiple AWS resources.
cdInUseBy :: Lens' CertificateDetail [Text]
cdInUseBy = lens _cdInUseBy (\ s a -> s{_cdInUseBy = a}) . _Default . _Coerce;

-- | The time at which the certificate was requested. This value exists only when the certificate type is @AMAZON_ISSUED@ .
cdCreatedAt :: Lens' CertificateDetail (Maybe UTCTime)
cdCreatedAt = lens _cdCreatedAt (\ s a -> s{_cdCreatedAt = a}) . mapping _Time;

-- | The Amazon Resource Name (ARN) of the certificate. For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
cdCertificateARN :: Lens' CertificateDetail (Maybe Text)
cdCertificateARN = lens _cdCertificateARN (\ s a -> s{_cdCertificateARN = a});

-- | The serial number of the certificate.
cdSerial :: Lens' CertificateDetail (Maybe Text)
cdSerial = lens _cdSerial (\ s a -> s{_cdSerial = a});

-- | The date and time at which the certificate was imported. This value exists only when the certificate type is @IMPORTED@ .
cdImportedAt :: Lens' CertificateDetail (Maybe UTCTime)
cdImportedAt = lens _cdImportedAt (\ s a -> s{_cdImportedAt = a}) . mapping _Time;

-- | The time at which the certificate was revoked. This value exists only when the certificate status is @REVOKED@ .
cdRevokedAt :: Lens' CertificateDetail (Maybe UTCTime)
cdRevokedAt = lens _cdRevokedAt (\ s a -> s{_cdRevokedAt = a}) . mapping _Time;

-- | The time before which the certificate is not valid.
cdNotBefore :: Lens' CertificateDetail (Maybe UTCTime)
cdNotBefore = lens _cdNotBefore (\ s a -> s{_cdNotBefore = a}) . mapping _Time;

-- | The reason the certificate was revoked. This value exists only when the certificate status is @REVOKED@ .
cdRevocationReason :: Lens' CertificateDetail (Maybe RevocationReason)
cdRevocationReason = lens _cdRevocationReason (\ s a -> s{_cdRevocationReason = a});

-- | The fully qualified domain name for the certificate, such as www.example.com or example.com.
cdDomainName :: Lens' CertificateDetail (Maybe Text)
cdDomainName = lens _cdDomainName (\ s a -> s{_cdDomainName = a});

-- | Contains information about the status of ACM's <http://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> for the certificate. This field exists only when the certificate type is @AMAZON_ISSUED@ .
cdRenewalSummary :: Lens' CertificateDetail (Maybe RenewalSummary)
cdRenewalSummary = lens _cdRenewalSummary (\ s a -> s{_cdRenewalSummary = a});

-- | The algorithm that was used to generate the key pair (the public and private key).
cdKeyAlgorithm :: Lens' CertificateDetail (Maybe KeyAlgorithm)
cdKeyAlgorithm = lens _cdKeyAlgorithm (\ s a -> s{_cdKeyAlgorithm = a});

-- | The source of the certificate. For certificates provided by ACM, this value is @AMAZON_ISSUED@ . For certificates that you imported with 'ImportCertificate' , this value is @IMPORTED@ . ACM does not provide <http://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> for imported certificates. For more information about the differences between certificates that you import and those that ACM provides, see <http://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing Certificates> in the /AWS Certificate Manager User Guide/ .
cdType :: Lens' CertificateDetail (Maybe CertificateType)
cdType = lens _cdType (\ s a -> s{_cdType = a});

-- | The time at which the certificate was issued. This value exists only when the certificate type is @AMAZON_ISSUED@ .
cdIssuedAt :: Lens' CertificateDetail (Maybe UTCTime)
cdIssuedAt = lens _cdIssuedAt (\ s a -> s{_cdIssuedAt = a}) . mapping _Time;

-- | The algorithm that was used to sign the certificate.
cdSignatureAlgorithm :: Lens' CertificateDetail (Maybe Text)
cdSignatureAlgorithm = lens _cdSignatureAlgorithm (\ s a -> s{_cdSignatureAlgorithm = a});

-- | Contains information about the initial validation of each domain name that occurs as a result of the 'RequestCertificate' request. This field exists only when the certificate type is @AMAZON_ISSUED@ .
cdDomainValidationOptions :: Lens' CertificateDetail (Maybe (NonEmpty DomainValidation))
cdDomainValidationOptions = lens _cdDomainValidationOptions (\ s a -> s{_cdDomainValidationOptions = a}) . mapping _List1;

-- | The name of the certificate authority that issued and signed the certificate.
cdIssuer :: Lens' CertificateDetail (Maybe Text)
cdIssuer = lens _cdIssuer (\ s a -> s{_cdIssuer = a});

-- | The time after which the certificate is not valid.
cdNotAfter :: Lens' CertificateDetail (Maybe UTCTime)
cdNotAfter = lens _cdNotAfter (\ s a -> s{_cdNotAfter = a}) . mapping _Time;

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
                     <*> (x .:? "ImportedAt")
                     <*> (x .:? "RevokedAt")
                     <*> (x .:? "NotBefore")
                     <*> (x .:? "RevocationReason")
                     <*> (x .:? "DomainName")
                     <*> (x .:? "RenewalSummary")
                     <*> (x .:? "KeyAlgorithm")
                     <*> (x .:? "Type")
                     <*> (x .:? "IssuedAt")
                     <*> (x .:? "SignatureAlgorithm")
                     <*> (x .:? "DomainValidationOptions")
                     <*> (x .:? "Issuer")
                     <*> (x .:? "NotAfter"))

instance Hashable CertificateDetail where

instance NFData CertificateDetail where

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
csCertificateARN = lens _csCertificateARN (\ s a -> s{_csCertificateARN = a});

-- | Fully qualified domain name (FQDN), such as www.example.com or example.com, for the certificate.
csDomainName :: Lens' CertificateSummary (Maybe Text)
csDomainName = lens _csDomainName (\ s a -> s{_csDomainName = a});

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
-- * 'dvValidationStatus' - The validation status of the domain name.
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
  , _dvValidationStatus = Nothing
  , _dvValidationDomain = Nothing
  , _dvDomainName = pDomainName_
  }


-- | A list of email addresses that ACM used to send domain validation emails.
dvValidationEmails :: Lens' DomainValidation [Text]
dvValidationEmails = lens _dvValidationEmails (\ s a -> s{_dvValidationEmails = a}) . _Default . _Coerce;

-- | The validation status of the domain name.
dvValidationStatus :: Lens' DomainValidation (Maybe DomainStatus)
dvValidationStatus = lens _dvValidationStatus (\ s a -> s{_dvValidationStatus = a});

-- | The domain name that ACM used to send domain validation emails.
dvValidationDomain :: Lens' DomainValidation (Maybe Text)
dvValidationDomain = lens _dvValidationDomain (\ s a -> s{_dvValidationDomain = a});

-- | A fully qualified domain name (FQDN) in the certificate. For example, @www.example.com@ or @example.com@ .
dvDomainName :: Lens' DomainValidation Text
dvDomainName = lens _dvDomainName (\ s a -> s{_dvDomainName = a});

instance FromJSON DomainValidation where
        parseJSON
          = withObject "DomainValidation"
              (\ x ->
                 DomainValidation' <$>
                   (x .:? "ValidationEmails" .!= mempty) <*>
                     (x .:? "ValidationStatus")
                     <*> (x .:? "ValidationDomain")
                     <*> (x .: "DomainName"))

instance Hashable DomainValidation where

instance NFData DomainValidation where

-- | Contains information about the domain names that you want ACM to use to send you emails to validate your ownership of the domain.
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
dvoDomainName = lens _dvoDomainName (\ s a -> s{_dvoDomainName = a});

-- | The domain name that you want ACM to use to send you validation emails. This domain name is the suffix of the email addresses that you want ACM to use. This must be the same as the @DomainName@ value or a superdomain of the @DomainName@ value. For example, if you request a certificate for @testing.example.com@ , you can specify @example.com@ for this value. In that case, ACM sends domain validation emails to the following five addresses:     * admin@example.com     * administrator@example.com     * hostmaster@example.com     * postmaster@example.com     * webmaster@example.com
dvoValidationDomain :: Lens' DomainValidationOption Text
dvoValidationDomain = lens _dvoValidationDomain (\ s a -> s{_dvoValidationDomain = a});

instance Hashable DomainValidationOption where

instance NFData DomainValidationOption where

instance ToJSON DomainValidationOption where
        toJSON DomainValidationOption'{..}
          = object
              (catMaybes
                 [Just ("DomainName" .= _dvoDomainName),
                  Just ("ValidationDomain" .= _dvoValidationDomain)])

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
rsRenewalStatus = lens _rsRenewalStatus (\ s a -> s{_rsRenewalStatus = a});

-- | Contains information about the validation of each domain name in the certificate, as it pertains to ACM's <http://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal> . This is different from the initial validation that occurs as a result of the 'RequestCertificate' request. This field exists only when the certificate type is @AMAZON_ISSUED@ .
rsDomainValidationOptions :: Lens' RenewalSummary (NonEmpty DomainValidation)
rsDomainValidationOptions = lens _rsDomainValidationOptions (\ s a -> s{_rsDomainValidationOptions = a}) . _List1;

instance FromJSON RenewalSummary where
        parseJSON
          = withObject "RenewalSummary"
              (\ x ->
                 RenewalSummary' <$>
                   (x .: "RenewalStatus") <*>
                     (x .: "DomainValidationOptions"))

instance Hashable RenewalSummary where

instance NFData RenewalSummary where

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
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | The key of the tag.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManager.Types.Product where

import           Network.AWS.CertificateManager.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | This structure is returned in the response object of the
-- < DescribeCertificate> action.
--
-- /See:/ 'certificateDetail' smart constructor.
data CertificateDetail = CertificateDetail'
    { _cdSubject                 :: !(Maybe Text)
    , _cdStatus                  :: !(Maybe CertificateStatus)
    , _cdSubjectAlternativeNames :: !(Maybe (List1 Text))
    , _cdInUseBy                 :: !(Maybe [Text])
    , _cdCreatedAt               :: !(Maybe POSIX)
    , _cdCertificateARN          :: !(Maybe Text)
    , _cdSerial                  :: !(Maybe Text)
    , _cdRevokedAt               :: !(Maybe POSIX)
    , _cdNotBefore               :: !(Maybe POSIX)
    , _cdRevocationReason        :: !(Maybe RevocationReason)
    , _cdDomainName              :: !(Maybe Text)
    , _cdKeyAlgorithm            :: !(Maybe KeyAlgorithm)
    , _cdIssuedAt                :: !(Maybe POSIX)
    , _cdSignatureAlgorithm      :: !(Maybe Text)
    , _cdDomainValidationOptions :: !(Maybe (List1 DomainValidation))
    , _cdIssuer                  :: !(Maybe Text)
    , _cdNotAfter                :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CertificateDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdSubject'
--
-- * 'cdStatus'
--
-- * 'cdSubjectAlternativeNames'
--
-- * 'cdInUseBy'
--
-- * 'cdCreatedAt'
--
-- * 'cdCertificateARN'
--
-- * 'cdSerial'
--
-- * 'cdRevokedAt'
--
-- * 'cdNotBefore'
--
-- * 'cdRevocationReason'
--
-- * 'cdDomainName'
--
-- * 'cdKeyAlgorithm'
--
-- * 'cdIssuedAt'
--
-- * 'cdSignatureAlgorithm'
--
-- * 'cdDomainValidationOptions'
--
-- * 'cdIssuer'
--
-- * 'cdNotAfter'
certificateDetail
    :: CertificateDetail
certificateDetail =
    CertificateDetail'
    { _cdSubject = Nothing
    , _cdStatus = Nothing
    , _cdSubjectAlternativeNames = Nothing
    , _cdInUseBy = Nothing
    , _cdCreatedAt = Nothing
    , _cdCertificateARN = Nothing
    , _cdSerial = Nothing
    , _cdRevokedAt = Nothing
    , _cdNotBefore = Nothing
    , _cdRevocationReason = Nothing
    , _cdDomainName = Nothing
    , _cdKeyAlgorithm = Nothing
    , _cdIssuedAt = Nothing
    , _cdSignatureAlgorithm = Nothing
    , _cdDomainValidationOptions = Nothing
    , _cdIssuer = Nothing
    , _cdNotAfter = Nothing
    }

-- | The X.500 distinguished name of the entity associated with the public
-- key contained in the certificate.
cdSubject :: Lens' CertificateDetail (Maybe Text)
cdSubject = lens _cdSubject (\ s a -> s{_cdSubject = a});

-- | A 'CertificateStatus' enumeration value that can contain one of the
-- following:
--
-- -   PENDING_VALIDATION
-- -   ISSUED
-- -   INACTIVE
-- -   EXPIRED
-- -   REVOKED
-- -   FAILED
-- -   VALIDATION_TIMED_OUT
cdStatus :: Lens' CertificateDetail (Maybe CertificateStatus)
cdStatus = lens _cdStatus (\ s a -> s{_cdStatus = a});

-- | One or more domain names (subject alternative names) included in the
-- certificate request. After the certificate is issued, this list includes
-- the domain names bound to the public key contained in the certificate.
-- The subject alternative names include the canonical domain name (CN) of
-- the certificate and additional domain names that can be used to connect
-- to the website.
cdSubjectAlternativeNames :: Lens' CertificateDetail (Maybe (NonEmpty Text))
cdSubjectAlternativeNames = lens _cdSubjectAlternativeNames (\ s a -> s{_cdSubjectAlternativeNames = a}) . mapping _List1;

-- | List that identifies ARNs that are using the certificate. A single ACM
-- Certificate can be used by multiple AWS resources.
cdInUseBy :: Lens' CertificateDetail [Text]
cdInUseBy = lens _cdInUseBy (\ s a -> s{_cdInUseBy = a}) . _Default . _Coerce;

-- | Time at which the certificate was requested.
cdCreatedAt :: Lens' CertificateDetail (Maybe UTCTime)
cdCreatedAt = lens _cdCreatedAt (\ s a -> s{_cdCreatedAt = a}) . mapping _Time;

-- | Amazon Resource Name (ARN) of the certificate. This is of the form:
--
-- 'arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012'
--
-- For more information about ARNs, see
-- <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
cdCertificateARN :: Lens' CertificateDetail (Maybe Text)
cdCertificateARN = lens _cdCertificateARN (\ s a -> s{_cdCertificateARN = a});

-- | String that contains the serial number of the certificate.
cdSerial :: Lens' CertificateDetail (Maybe Text)
cdSerial = lens _cdSerial (\ s a -> s{_cdSerial = a});

-- | The time, if any, at which the certificate was revoked. This value
-- exists only if the certificate has been revoked.
cdRevokedAt :: Lens' CertificateDetail (Maybe UTCTime)
cdRevokedAt = lens _cdRevokedAt (\ s a -> s{_cdRevokedAt = a}) . mapping _Time;

-- | Time before which the certificate is not valid.
cdNotBefore :: Lens' CertificateDetail (Maybe UTCTime)
cdNotBefore = lens _cdNotBefore (\ s a -> s{_cdNotBefore = a}) . mapping _Time;

-- | A 'RevocationReason' enumeration value that indicates why the
-- certificate was revoked. This value exists only if the certificate has
-- been revoked. This can be one of the following vales:
--
-- -   UNSPECIFIED
-- -   KEY_COMPROMISE
-- -   CA_COMPROMISE
-- -   AFFILIATION_CHANGED
-- -   SUPERCEDED
-- -   CESSATION_OF_OPERATION
-- -   CERTIFICATE_HOLD
-- -   REMOVE_FROM_CRL
-- -   PRIVILEGE_WITHDRAWN
-- -   A_A_COMPROMISE
cdRevocationReason :: Lens' CertificateDetail (Maybe RevocationReason)
cdRevocationReason = lens _cdRevocationReason (\ s a -> s{_cdRevocationReason = a});

-- | Fully qualified domain name (FQDN), such as www.example.com or
-- example.com, for the certificate.
cdDomainName :: Lens' CertificateDetail (Maybe Text)
cdDomainName = lens _cdDomainName (\ s a -> s{_cdDomainName = a});

-- | Asymmetric algorithm used to generate the public and private key pair.
-- Currently the only supported value is 'RSA_2048'.
cdKeyAlgorithm :: Lens' CertificateDetail (Maybe KeyAlgorithm)
cdKeyAlgorithm = lens _cdKeyAlgorithm (\ s a -> s{_cdKeyAlgorithm = a});

-- | Time at which the certificate was issued.
cdIssuedAt :: Lens' CertificateDetail (Maybe UTCTime)
cdIssuedAt = lens _cdIssuedAt (\ s a -> s{_cdIssuedAt = a}) . mapping _Time;

-- | Algorithm used to generate a signature. Currently the only supported
-- value is 'SHA256WITHRSA'.
cdSignatureAlgorithm :: Lens' CertificateDetail (Maybe Text)
cdSignatureAlgorithm = lens _cdSignatureAlgorithm (\ s a -> s{_cdSignatureAlgorithm = a});

-- | References a < DomainValidation> structure that contains the domain name
-- in the certificate and the email address that can be used for
-- validation.
cdDomainValidationOptions :: Lens' CertificateDetail (Maybe (NonEmpty DomainValidation))
cdDomainValidationOptions = lens _cdDomainValidationOptions (\ s a -> s{_cdDomainValidationOptions = a}) . mapping _List1;

-- | The X.500 distinguished name of the CA that issued and signed the
-- certificate.
cdIssuer :: Lens' CertificateDetail (Maybe Text)
cdIssuer = lens _cdIssuer (\ s a -> s{_cdIssuer = a});

-- | Time after which the certificate is not valid.
cdNotAfter :: Lens' CertificateDetail (Maybe UTCTime)
cdNotAfter = lens _cdNotAfter (\ s a -> s{_cdNotAfter = a}) . mapping _Time;

instance FromJSON CertificateDetail where
        parseJSON
          = withObject "CertificateDetail"
              (\ x ->
                 CertificateDetail' <$>
                   (x .:? "Subject") <*> (x .:? "Status") <*>
                     (x .:? "SubjectAlternativeNames")
                     <*> (x .:? "InUseBy" .!= mempty)
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "CertificateArn")
                     <*> (x .:? "Serial")
                     <*> (x .:? "RevokedAt")
                     <*> (x .:? "NotBefore")
                     <*> (x .:? "RevocationReason")
                     <*> (x .:? "DomainName")
                     <*> (x .:? "KeyAlgorithm")
                     <*> (x .:? "IssuedAt")
                     <*> (x .:? "SignatureAlgorithm")
                     <*> (x .:? "DomainValidationOptions")
                     <*> (x .:? "Issuer")
                     <*> (x .:? "NotAfter"))

instance Hashable CertificateDetail

-- | This structure is returned in the response object of < ListCertificates>
-- action.
--
-- /See:/ 'certificateSummary' smart constructor.
data CertificateSummary = CertificateSummary'
    { _csCertificateARN :: !(Maybe Text)
    , _csDomainName     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CertificateSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csCertificateARN'
--
-- * 'csDomainName'
certificateSummary
    :: CertificateSummary
certificateSummary =
    CertificateSummary'
    { _csCertificateARN = Nothing
    , _csDomainName = Nothing
    }

-- | Amazon Resource Name (ARN) of the certificate. This is of the form:
--
-- 'arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012'
--
-- For more information about ARNs, see
-- <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
csCertificateARN :: Lens' CertificateSummary (Maybe Text)
csCertificateARN = lens _csCertificateARN (\ s a -> s{_csCertificateARN = a});

-- | Fully qualified domain name (FQDN), such as www.example.com or
-- example.com, for the certificate.
csDomainName :: Lens' CertificateSummary (Maybe Text)
csDomainName = lens _csDomainName (\ s a -> s{_csDomainName = a});

instance FromJSON CertificateSummary where
        parseJSON
          = withObject "CertificateSummary"
              (\ x ->
                 CertificateSummary' <$>
                   (x .:? "CertificateArn") <*> (x .:? "DomainName"))

instance Hashable CertificateSummary

-- | Structure that contains the domain name, the base validation domain to
-- which validation email is sent, and the email addresses used to validate
-- the domain identity.
--
-- /See:/ 'domainValidation' smart constructor.
data DomainValidation = DomainValidation'
    { _dvValidationEmails :: !(Maybe [Text])
    , _dvValidationDomain :: !(Maybe Text)
    , _dvDomainName       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DomainValidation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvValidationEmails'
--
-- * 'dvValidationDomain'
--
-- * 'dvDomainName'
domainValidation
    :: Text -- ^ 'dvDomainName'
    -> DomainValidation
domainValidation pDomainName_ =
    DomainValidation'
    { _dvValidationEmails = Nothing
    , _dvValidationDomain = Nothing
    , _dvDomainName = pDomainName_
    }

-- | A list of contact address for the domain registrant.
dvValidationEmails :: Lens' DomainValidation [Text]
dvValidationEmails = lens _dvValidationEmails (\ s a -> s{_dvValidationEmails = a}) . _Default . _Coerce;

-- | The base validation domain that acts as the suffix of the email
-- addresses that are used to send the emails.
dvValidationDomain :: Lens' DomainValidation (Maybe Text)
dvValidationDomain = lens _dvValidationDomain (\ s a -> s{_dvValidationDomain = a});

-- | Fully Qualified Domain Name (FQDN) of the form
-- 'www.example.com or ''example.com'
dvDomainName :: Lens' DomainValidation Text
dvDomainName = lens _dvDomainName (\ s a -> s{_dvDomainName = a});

instance FromJSON DomainValidation where
        parseJSON
          = withObject "DomainValidation"
              (\ x ->
                 DomainValidation' <$>
                   (x .:? "ValidationEmails" .!= mempty) <*>
                     (x .:? "ValidationDomain")
                     <*> (x .: "DomainName"))

instance Hashable DomainValidation

-- | This structure is used in the request object of the
-- < RequestCertificate> action.
--
-- /See:/ 'domainValidationOption' smart constructor.
data DomainValidationOption = DomainValidationOption'
    { _dvoDomainName       :: !Text
    , _dvoValidationDomain :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DomainValidationOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvoDomainName'
--
-- * 'dvoValidationDomain'
domainValidationOption
    :: Text -- ^ 'dvoDomainName'
    -> Text -- ^ 'dvoValidationDomain'
    -> DomainValidationOption
domainValidationOption pDomainName_ pValidationDomain_ =
    DomainValidationOption'
    { _dvoDomainName = pDomainName_
    , _dvoValidationDomain = pValidationDomain_
    }

-- | Fully Qualified Domain Name (FQDN) of the certificate being requested.
dvoDomainName :: Lens' DomainValidationOption Text
dvoDomainName = lens _dvoDomainName (\ s a -> s{_dvoDomainName = a});

-- | The domain to which validation email is sent. This is the base
-- validation domain that will act as the suffix of the email addresses.
-- This must be the same as the 'DomainName' value or a superdomain of the
-- 'DomainName' value. For example, if you requested a certificate for
-- 'site.subdomain.example.com' and specify a __ValidationDomain__ of
-- 'subdomain.example.com', ACM sends email to the domain registrant,
-- technical contact, and administrative contact in WHOIS for the base
-- domain and the and the following five addresses:
--
-- -   admin\'subdomain.example.com
-- -   administrator\'subdomain.example.com
-- -   hostmaster\'subdomain.example.com
-- -   postmaster\'subdomain.example.com
-- -   webmaster\'subdomain.example.com
dvoValidationDomain :: Lens' DomainValidationOption Text
dvoValidationDomain = lens _dvoValidationDomain (\ s a -> s{_dvoValidationDomain = a});

instance Hashable DomainValidationOption

instance ToJSON DomainValidationOption where
        toJSON DomainValidationOption'{..}
          = object
              (catMaybes
                 [Just ("DomainName" .= _dvoDomainName),
                  Just ("ValidationDomain" .= _dvoValidationDomain)])

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManagerPCA.Types.Product where

import Network.AWS.CertificateManagerPCA.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the certificate subject. The certificate can be one issued by your private certificate authority (CA) or it can be your private CA certificate. The __Subject__ field in the certificate identifies the entity that owns or controls the public key in the certificate. The entity can be a user, computer, device, or service. The __Subject__ must contain an X.500 distinguished name (DN). A DN is a sequence of relative distinguished names (RDNs). The RDNs are separated by commas in the certificate. The DN must be unique for each for each entity, but your private CA can issue more than one certificate with the same DN to the same entity.
--
--
--
-- /See:/ 'asn1Subject' smart constructor.
data ASN1Subject = ASN1Subject'
  { _asGivenName                  :: !(Maybe Text)
  , _asState                      :: !(Maybe Text)
  , _asCommonName                 :: !(Maybe Text)
  , _asOrganizationalUnit         :: !(Maybe Text)
  , _asCountry                    :: !(Maybe Text)
  , _asGenerationQualifier        :: !(Maybe Text)
  , _asLocality                   :: !(Maybe Text)
  , _asPseudonym                  :: !(Maybe Text)
  , _asInitials                   :: !(Maybe Text)
  , _asTitle                      :: !(Maybe Text)
  , _asOrganization               :: !(Maybe Text)
  , _asSerialNumber               :: !(Maybe Text)
  , _asSurname                    :: !(Maybe Text)
  , _asDistinguishedNameQualifier :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ASN1Subject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asGivenName' - First name.
--
-- * 'asState' - State in which the subject of the certificate is located.
--
-- * 'asCommonName' - Fully qualified domain name (FQDN) associated with the certificate subject.
--
-- * 'asOrganizationalUnit' - A subdivision or unit of the organization (such as sales or finance) with which the certificate subject is affiliated.
--
-- * 'asCountry' - Two digit code that specifies the country in which the certificate subject located.
--
-- * 'asGenerationQualifier' - Typically a qualifier appended to the name of an individual. Examples include Jr. for junior, Sr. for senior, and III for third.
--
-- * 'asLocality' - The locality (such as a city or town) in which the certificate subject is located.
--
-- * 'asPseudonym' - Typically a shortened version of a longer __GivenName__ . For example, Jonathan is often shortened to John. Elizabeth is often shortened to Beth, Liz, or Eliza.
--
-- * 'asInitials' - Concatenation that typically contains the first letter of the __GivenName__ , the first letter of the middle name if one exists, and the first letter of the __SurName__ .
--
-- * 'asTitle' - A title such as Mr. or Ms. which is pre-pended to the name to refer formally to the certificate subject.
--
-- * 'asOrganization' - Legal name of the organization with which the certificate subject is affiliated.
--
-- * 'asSerialNumber' - The certificate serial number.
--
-- * 'asSurname' - Family name. In the US and the UK for example, the surname of an individual is ordered last. In Asian cultures the surname is typically ordered first.
--
-- * 'asDistinguishedNameQualifier' - Disambiguating information for the certificate subject.
asn1Subject
    :: ASN1Subject
asn1Subject =
  ASN1Subject'
    { _asGivenName = Nothing
    , _asState = Nothing
    , _asCommonName = Nothing
    , _asOrganizationalUnit = Nothing
    , _asCountry = Nothing
    , _asGenerationQualifier = Nothing
    , _asLocality = Nothing
    , _asPseudonym = Nothing
    , _asInitials = Nothing
    , _asTitle = Nothing
    , _asOrganization = Nothing
    , _asSerialNumber = Nothing
    , _asSurname = Nothing
    , _asDistinguishedNameQualifier = Nothing
    }


-- | First name.
asGivenName :: Lens' ASN1Subject (Maybe Text)
asGivenName = lens _asGivenName (\ s a -> s{_asGivenName = a})

-- | State in which the subject of the certificate is located.
asState :: Lens' ASN1Subject (Maybe Text)
asState = lens _asState (\ s a -> s{_asState = a})

-- | Fully qualified domain name (FQDN) associated with the certificate subject.
asCommonName :: Lens' ASN1Subject (Maybe Text)
asCommonName = lens _asCommonName (\ s a -> s{_asCommonName = a})

-- | A subdivision or unit of the organization (such as sales or finance) with which the certificate subject is affiliated.
asOrganizationalUnit :: Lens' ASN1Subject (Maybe Text)
asOrganizationalUnit = lens _asOrganizationalUnit (\ s a -> s{_asOrganizationalUnit = a})

-- | Two digit code that specifies the country in which the certificate subject located.
asCountry :: Lens' ASN1Subject (Maybe Text)
asCountry = lens _asCountry (\ s a -> s{_asCountry = a})

-- | Typically a qualifier appended to the name of an individual. Examples include Jr. for junior, Sr. for senior, and III for third.
asGenerationQualifier :: Lens' ASN1Subject (Maybe Text)
asGenerationQualifier = lens _asGenerationQualifier (\ s a -> s{_asGenerationQualifier = a})

-- | The locality (such as a city or town) in which the certificate subject is located.
asLocality :: Lens' ASN1Subject (Maybe Text)
asLocality = lens _asLocality (\ s a -> s{_asLocality = a})

-- | Typically a shortened version of a longer __GivenName__ . For example, Jonathan is often shortened to John. Elizabeth is often shortened to Beth, Liz, or Eliza.
asPseudonym :: Lens' ASN1Subject (Maybe Text)
asPseudonym = lens _asPseudonym (\ s a -> s{_asPseudonym = a})

-- | Concatenation that typically contains the first letter of the __GivenName__ , the first letter of the middle name if one exists, and the first letter of the __SurName__ .
asInitials :: Lens' ASN1Subject (Maybe Text)
asInitials = lens _asInitials (\ s a -> s{_asInitials = a})

-- | A title such as Mr. or Ms. which is pre-pended to the name to refer formally to the certificate subject.
asTitle :: Lens' ASN1Subject (Maybe Text)
asTitle = lens _asTitle (\ s a -> s{_asTitle = a})

-- | Legal name of the organization with which the certificate subject is affiliated.
asOrganization :: Lens' ASN1Subject (Maybe Text)
asOrganization = lens _asOrganization (\ s a -> s{_asOrganization = a})

-- | The certificate serial number.
asSerialNumber :: Lens' ASN1Subject (Maybe Text)
asSerialNumber = lens _asSerialNumber (\ s a -> s{_asSerialNumber = a})

-- | Family name. In the US and the UK for example, the surname of an individual is ordered last. In Asian cultures the surname is typically ordered first.
asSurname :: Lens' ASN1Subject (Maybe Text)
asSurname = lens _asSurname (\ s a -> s{_asSurname = a})

-- | Disambiguating information for the certificate subject.
asDistinguishedNameQualifier :: Lens' ASN1Subject (Maybe Text)
asDistinguishedNameQualifier = lens _asDistinguishedNameQualifier (\ s a -> s{_asDistinguishedNameQualifier = a})

instance FromJSON ASN1Subject where
        parseJSON
          = withObject "ASN1Subject"
              (\ x ->
                 ASN1Subject' <$>
                   (x .:? "GivenName") <*> (x .:? "State") <*>
                     (x .:? "CommonName")
                     <*> (x .:? "OrganizationalUnit")
                     <*> (x .:? "Country")
                     <*> (x .:? "GenerationQualifier")
                     <*> (x .:? "Locality")
                     <*> (x .:? "Pseudonym")
                     <*> (x .:? "Initials")
                     <*> (x .:? "Title")
                     <*> (x .:? "Organization")
                     <*> (x .:? "SerialNumber")
                     <*> (x .:? "Surname")
                     <*> (x .:? "DistinguishedNameQualifier"))

instance Hashable ASN1Subject where

instance NFData ASN1Subject where

instance ToJSON ASN1Subject where
        toJSON ASN1Subject'{..}
          = object
              (catMaybes
                 [("GivenName" .=) <$> _asGivenName,
                  ("State" .=) <$> _asState,
                  ("CommonName" .=) <$> _asCommonName,
                  ("OrganizationalUnit" .=) <$> _asOrganizationalUnit,
                  ("Country" .=) <$> _asCountry,
                  ("GenerationQualifier" .=) <$>
                    _asGenerationQualifier,
                  ("Locality" .=) <$> _asLocality,
                  ("Pseudonym" .=) <$> _asPseudonym,
                  ("Initials" .=) <$> _asInitials,
                  ("Title" .=) <$> _asTitle,
                  ("Organization" .=) <$> _asOrganization,
                  ("SerialNumber" .=) <$> _asSerialNumber,
                  ("Surname" .=) <$> _asSurname,
                  ("DistinguishedNameQualifier" .=) <$>
                    _asDistinguishedNameQualifier])

-- | Contains information about your private certificate authority (CA). Your private CA can issue and revoke X.509 digital certificates. Digital certificates verify that the entity named in the certificate __Subject__ field owns or controls the public key contained in the __Subject Public Key Info__ field. Call the 'CreateCertificateAuthority' function to create your private CA. You must then call the 'GetCertificateAuthorityCertificate' function to retrieve a private CA certificate signing request (CSR). Take the CSR to your on-premises CA and sign it with the root CA certificate or a subordinate certificate. Call the 'ImportCertificateAuthorityCertificate' function to import the signed certificate into AWS Certificate Manager (ACM).
--
--
--
-- /See:/ 'certificateAuthority' smart constructor.
data CertificateAuthority = CertificateAuthority'
  { _caStatus :: !(Maybe CertificateAuthorityStatus)
  , _caFailureReason :: !(Maybe FailureReason)
  , _caCertificateAuthorityConfiguration :: !(Maybe CertificateAuthorityConfiguration)
  , _caARN :: !(Maybe Text)
  , _caCreatedAt :: !(Maybe POSIX)
  , _caSerial :: !(Maybe Text)
  , _caNotBefore :: !(Maybe POSIX)
  , _caType :: !(Maybe CertificateAuthorityType)
  , _caRevocationConfiguration :: !(Maybe RevocationConfiguration)
  , _caLastStateChangeAt :: !(Maybe POSIX)
  , _caNotAfter :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CertificateAuthority' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caStatus' - Status of your private CA.
--
-- * 'caFailureReason' - Reason the request to create your private CA failed.
--
-- * 'caCertificateAuthorityConfiguration' - Your private CA configuration.
--
-- * 'caARN' - Amazon Resource Name (ARN) for your private certificate authority (CA). The format is @/12345678-1234-1234-1234-123456789012/ @ .
--
-- * 'caCreatedAt' - Date and time at which your private CA was created.
--
-- * 'caSerial' - Serial number of your private CA.
--
-- * 'caNotBefore' - Date and time before which your private CA certificate is not valid.
--
-- * 'caType' - Type of your private CA.
--
-- * 'caRevocationConfiguration' - Information about the certificate revocation list (CRL) created and maintained by your private CA.
--
-- * 'caLastStateChangeAt' - Date and time at which your private CA was last updated.
--
-- * 'caNotAfter' - Date and time after which your private CA certificate is not valid.
certificateAuthority
    :: CertificateAuthority
certificateAuthority =
  CertificateAuthority'
    { _caStatus = Nothing
    , _caFailureReason = Nothing
    , _caCertificateAuthorityConfiguration = Nothing
    , _caARN = Nothing
    , _caCreatedAt = Nothing
    , _caSerial = Nothing
    , _caNotBefore = Nothing
    , _caType = Nothing
    , _caRevocationConfiguration = Nothing
    , _caLastStateChangeAt = Nothing
    , _caNotAfter = Nothing
    }


-- | Status of your private CA.
caStatus :: Lens' CertificateAuthority (Maybe CertificateAuthorityStatus)
caStatus = lens _caStatus (\ s a -> s{_caStatus = a})

-- | Reason the request to create your private CA failed.
caFailureReason :: Lens' CertificateAuthority (Maybe FailureReason)
caFailureReason = lens _caFailureReason (\ s a -> s{_caFailureReason = a})

-- | Your private CA configuration.
caCertificateAuthorityConfiguration :: Lens' CertificateAuthority (Maybe CertificateAuthorityConfiguration)
caCertificateAuthorityConfiguration = lens _caCertificateAuthorityConfiguration (\ s a -> s{_caCertificateAuthorityConfiguration = a})

-- | Amazon Resource Name (ARN) for your private certificate authority (CA). The format is @/12345678-1234-1234-1234-123456789012/ @ .
caARN :: Lens' CertificateAuthority (Maybe Text)
caARN = lens _caARN (\ s a -> s{_caARN = a})

-- | Date and time at which your private CA was created.
caCreatedAt :: Lens' CertificateAuthority (Maybe UTCTime)
caCreatedAt = lens _caCreatedAt (\ s a -> s{_caCreatedAt = a}) . mapping _Time

-- | Serial number of your private CA.
caSerial :: Lens' CertificateAuthority (Maybe Text)
caSerial = lens _caSerial (\ s a -> s{_caSerial = a})

-- | Date and time before which your private CA certificate is not valid.
caNotBefore :: Lens' CertificateAuthority (Maybe UTCTime)
caNotBefore = lens _caNotBefore (\ s a -> s{_caNotBefore = a}) . mapping _Time

-- | Type of your private CA.
caType :: Lens' CertificateAuthority (Maybe CertificateAuthorityType)
caType = lens _caType (\ s a -> s{_caType = a})

-- | Information about the certificate revocation list (CRL) created and maintained by your private CA.
caRevocationConfiguration :: Lens' CertificateAuthority (Maybe RevocationConfiguration)
caRevocationConfiguration = lens _caRevocationConfiguration (\ s a -> s{_caRevocationConfiguration = a})

-- | Date and time at which your private CA was last updated.
caLastStateChangeAt :: Lens' CertificateAuthority (Maybe UTCTime)
caLastStateChangeAt = lens _caLastStateChangeAt (\ s a -> s{_caLastStateChangeAt = a}) . mapping _Time

-- | Date and time after which your private CA certificate is not valid.
caNotAfter :: Lens' CertificateAuthority (Maybe UTCTime)
caNotAfter = lens _caNotAfter (\ s a -> s{_caNotAfter = a}) . mapping _Time

instance FromJSON CertificateAuthority where
        parseJSON
          = withObject "CertificateAuthority"
              (\ x ->
                 CertificateAuthority' <$>
                   (x .:? "Status") <*> (x .:? "FailureReason") <*>
                     (x .:? "CertificateAuthorityConfiguration")
                     <*> (x .:? "Arn")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "Serial")
                     <*> (x .:? "NotBefore")
                     <*> (x .:? "Type")
                     <*> (x .:? "RevocationConfiguration")
                     <*> (x .:? "LastStateChangeAt")
                     <*> (x .:? "NotAfter"))

instance Hashable CertificateAuthority where

instance NFData CertificateAuthority where

-- | Contains configuration information for your private certificate authority (CA). This includes information about the class of public key algorithm and the key pair that your private CA creates when it issues a certificate, the signature algorithm it uses used when issuing certificates, and its X.500 distinguished name. You must specify this information when you call the 'CreateCertificateAuthority' function.
--
--
--
-- /See:/ 'certificateAuthorityConfiguration' smart constructor.
data CertificateAuthorityConfiguration = CertificateAuthorityConfiguration'
  { _cacKeyAlgorithm     :: !KeyAlgorithm
  , _cacSigningAlgorithm :: !SigningAlgorithm
  , _cacSubject          :: !ASN1Subject
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CertificateAuthorityConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cacKeyAlgorithm' - Type of the public key algorithm and size, in bits, of the key pair that your key pair creates when it issues a certificate.
--
-- * 'cacSigningAlgorithm' - Name of the algorithm your private CA uses to sign certificate requests.
--
-- * 'cacSubject' - Structure that contains X.500 distinguished name information for your private CA.
certificateAuthorityConfiguration
    :: KeyAlgorithm -- ^ 'cacKeyAlgorithm'
    -> SigningAlgorithm -- ^ 'cacSigningAlgorithm'
    -> ASN1Subject -- ^ 'cacSubject'
    -> CertificateAuthorityConfiguration
certificateAuthorityConfiguration pKeyAlgorithm_ pSigningAlgorithm_ pSubject_ =
  CertificateAuthorityConfiguration'
    { _cacKeyAlgorithm = pKeyAlgorithm_
    , _cacSigningAlgorithm = pSigningAlgorithm_
    , _cacSubject = pSubject_
    }


-- | Type of the public key algorithm and size, in bits, of the key pair that your key pair creates when it issues a certificate.
cacKeyAlgorithm :: Lens' CertificateAuthorityConfiguration KeyAlgorithm
cacKeyAlgorithm = lens _cacKeyAlgorithm (\ s a -> s{_cacKeyAlgorithm = a})

-- | Name of the algorithm your private CA uses to sign certificate requests.
cacSigningAlgorithm :: Lens' CertificateAuthorityConfiguration SigningAlgorithm
cacSigningAlgorithm = lens _cacSigningAlgorithm (\ s a -> s{_cacSigningAlgorithm = a})

-- | Structure that contains X.500 distinguished name information for your private CA.
cacSubject :: Lens' CertificateAuthorityConfiguration ASN1Subject
cacSubject = lens _cacSubject (\ s a -> s{_cacSubject = a})

instance FromJSON CertificateAuthorityConfiguration
         where
        parseJSON
          = withObject "CertificateAuthorityConfiguration"
              (\ x ->
                 CertificateAuthorityConfiguration' <$>
                   (x .: "KeyAlgorithm") <*> (x .: "SigningAlgorithm")
                     <*> (x .: "Subject"))

instance Hashable CertificateAuthorityConfiguration
         where

instance NFData CertificateAuthorityConfiguration
         where

instance ToJSON CertificateAuthorityConfiguration
         where
        toJSON CertificateAuthorityConfiguration'{..}
          = object
              (catMaybes
                 [Just ("KeyAlgorithm" .= _cacKeyAlgorithm),
                  Just ("SigningAlgorithm" .= _cacSigningAlgorithm),
                  Just ("Subject" .= _cacSubject)])

-- | Contains configuration information for a certificate revocation list (CRL). Your private certificate authority (CA) creates base CRLs. Delta CRLs are not supported. You can enable CRLs for your new or an existing private CA by setting the __Enabled__ parameter to @true@ . Your private CA writes CRLs to an S3 bucket that you specify in the __S3BucketName__ parameter. You can hide the name of your bucket by specifying a value for the __CustomCname__ parameter. Your private CA copies the CNAME or the S3 bucket name to the __CRL Distribution Points__ extension of each certificate it issues. Your S3 bucket policy must give write permission to ACM PCA.
--
--
-- Your private CA uses the value in the __ExpirationInDays__ parameter to calculate the __nextUpdate__ field in the CRL. The CRL is refreshed at 1/2 the age of next update or when a certificate is revoked. When a certificate is revoked, it is recorded in the next CRL that is generated and in the next audit report. Only time valid certificates are listed in the CRL. Expired certificates are not included.
--
-- CRLs contain the following fields:
--
--     * __Version__ : The current version number defined in RFC 5280 is V2. The integer value is 0x1.
--
--     * __Signature Algorithm__ : The name of the algorithm used to sign the CRL.
--
--     * __Issuer__ : The X.500 distinguished name of your private CA that issued the CRL.
--
--     * __Last Update__ : The issue date and time of this CRL.
--
--     * __Next Update__ : The day and time by which the next CRL will be issued.
--
--     * __Revoked Certificates__ : List of revoked certificates. Each list item contains the following information.
--
--     * __Serial Number__ : The serial number, in hexadecimal format, of the revoked certificate.
--
--     * __Revocation Date__ : Date and time the certificate was revoked.
--
--     * __CRL Entry Extensions__ : Optional extensions for the CRL entry.
--
--     * __X509v3 CRL Reason Code__ : Reason the certificate was revoked.
--
--
--
--
--
--     * __CRL Extensions__ : Optional extensions for the CRL.
--
--     * __X509v3 Authority Key Identifier__ : Identifies the public key associated with the private key used to sign the certificate.
--
--     * __X509v3 CRL Number:__ : Decimal sequence number for the CRL.
--
--
--
--     * __Signature Algorithm__ : Algorithm used by your private CA to sign the CRL.
--
--     * __Signature Value__ : Signature computed over the CRL.
--
--
--
-- Certificate revocation lists created by ACM PCA are DER-encoded. You can use the following OpenSSL command to list a CRL.
--
-- @openssl crl -inform DER -text -in /crl_path/ -noout@
--
--
-- /See:/ 'crlConfiguration' smart constructor.
data CrlConfiguration = CrlConfiguration'
  { _ccCustomCname      :: !(Maybe Text)
  , _ccExpirationInDays :: !(Maybe Nat)
  , _ccS3BucketName     :: !(Maybe Text)
  , _ccEnabled          :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CrlConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccCustomCname' - Name inserted into the certificate __CRL Distribution Points__ extension that enables the use of an alias for the CRL distribution point. Use this value if you don't want the name of your S3 bucket to be public.
--
-- * 'ccExpirationInDays' - Number of days until a certificate expires.
--
-- * 'ccS3BucketName' - Name of the S3 bucket that contains the CRL. If you do not provide a value for the __CustomCname__ argument, the name of your S3 bucket is placed into the __CRL Distribution Points__ extension of the issued certificate. You can change the name of your bucket by calling the 'UpdateCertificateAuthority' function. You must specify a bucket policy that allows ACM PCA to write the CRL to your bucket.
--
-- * 'ccEnabled' - Boolean value that specifies whether certificate revocation lists (CRLs) are enabled. You can use this value to enable certificate revocation for a new CA when you call the 'CreateCertificateAuthority' function or for an existing CA when you call the 'UpdateCertificateAuthority' function.
crlConfiguration
    :: Bool -- ^ 'ccEnabled'
    -> CrlConfiguration
crlConfiguration pEnabled_ =
  CrlConfiguration'
    { _ccCustomCname = Nothing
    , _ccExpirationInDays = Nothing
    , _ccS3BucketName = Nothing
    , _ccEnabled = pEnabled_
    }


-- | Name inserted into the certificate __CRL Distribution Points__ extension that enables the use of an alias for the CRL distribution point. Use this value if you don't want the name of your S3 bucket to be public.
ccCustomCname :: Lens' CrlConfiguration (Maybe Text)
ccCustomCname = lens _ccCustomCname (\ s a -> s{_ccCustomCname = a})

-- | Number of days until a certificate expires.
ccExpirationInDays :: Lens' CrlConfiguration (Maybe Natural)
ccExpirationInDays = lens _ccExpirationInDays (\ s a -> s{_ccExpirationInDays = a}) . mapping _Nat

-- | Name of the S3 bucket that contains the CRL. If you do not provide a value for the __CustomCname__ argument, the name of your S3 bucket is placed into the __CRL Distribution Points__ extension of the issued certificate. You can change the name of your bucket by calling the 'UpdateCertificateAuthority' function. You must specify a bucket policy that allows ACM PCA to write the CRL to your bucket.
ccS3BucketName :: Lens' CrlConfiguration (Maybe Text)
ccS3BucketName = lens _ccS3BucketName (\ s a -> s{_ccS3BucketName = a})

-- | Boolean value that specifies whether certificate revocation lists (CRLs) are enabled. You can use this value to enable certificate revocation for a new CA when you call the 'CreateCertificateAuthority' function or for an existing CA when you call the 'UpdateCertificateAuthority' function.
ccEnabled :: Lens' CrlConfiguration Bool
ccEnabled = lens _ccEnabled (\ s a -> s{_ccEnabled = a})

instance FromJSON CrlConfiguration where
        parseJSON
          = withObject "CrlConfiguration"
              (\ x ->
                 CrlConfiguration' <$>
                   (x .:? "CustomCname") <*> (x .:? "ExpirationInDays")
                     <*> (x .:? "S3BucketName")
                     <*> (x .: "Enabled"))

instance Hashable CrlConfiguration where

instance NFData CrlConfiguration where

instance ToJSON CrlConfiguration where
        toJSON CrlConfiguration'{..}
          = object
              (catMaybes
                 [("CustomCname" .=) <$> _ccCustomCname,
                  ("ExpirationInDays" .=) <$> _ccExpirationInDays,
                  ("S3BucketName" .=) <$> _ccS3BucketName,
                  Just ("Enabled" .= _ccEnabled)])

-- | Certificate revocation information used by the 'CreateCertificateAuthority' and 'UpdateCertificateAuthority' functions. Your private certificate authority (CA) can create and maintain a certificate revocation list (CRL). A CRL contains information about certificates revoked by your CA. For more information, see 'RevokeCertificate' .
--
--
--
-- /See:/ 'revocationConfiguration' smart constructor.
newtype RevocationConfiguration = RevocationConfiguration'
  { _rcCrlConfiguration :: Maybe CrlConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RevocationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcCrlConfiguration' - Configuration of the certificate revocation list (CRL), if any, maintained by your private CA.
revocationConfiguration
    :: RevocationConfiguration
revocationConfiguration =
  RevocationConfiguration' {_rcCrlConfiguration = Nothing}


-- | Configuration of the certificate revocation list (CRL), if any, maintained by your private CA.
rcCrlConfiguration :: Lens' RevocationConfiguration (Maybe CrlConfiguration)
rcCrlConfiguration = lens _rcCrlConfiguration (\ s a -> s{_rcCrlConfiguration = a})

instance FromJSON RevocationConfiguration where
        parseJSON
          = withObject "RevocationConfiguration"
              (\ x ->
                 RevocationConfiguration' <$>
                   (x .:? "CrlConfiguration"))

instance Hashable RevocationConfiguration where

instance NFData RevocationConfiguration where

instance ToJSON RevocationConfiguration where
        toJSON RevocationConfiguration'{..}
          = object
              (catMaybes
                 [("CrlConfiguration" .=) <$> _rcCrlConfiguration])

-- | Tags are labels that you can use to identify and organize your private CAs. Each tag consists of a key and an optional value. You can associate up to 50 tags with a private CA. To add one or more tags to a private CA, call the 'TagCertificateAuthority' function. To remove a tag, call the 'UntagCertificateAuthority' function.
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
-- * 'tagValue' - Value of the tag.
--
-- * 'tagKey' - Key (name) of the tag.
tag
    :: Text -- ^ 'tagKey'
    -> Tag
tag pKey_ = Tag' {_tagValue = Nothing, _tagKey = pKey_}


-- | Value of the tag.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | Key (name) of the tag.
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

-- | Length of time for which the certificate issued by your private certificate authority (CA), or by the private CA itself, is valid in days, months, or years. You can issue a certificate by calling the 'IssueCertificate' function.
--
--
--
-- /See:/ 'validity' smart constructor.
data Validity = Validity'
  { _vValue :: !Nat
  , _vType  :: !ValidityPeriodType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Validity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vValue' - Time period.
--
-- * 'vType' - Specifies whether the @Value@ parameter represents days, months, or years.
validity
    :: Natural -- ^ 'vValue'
    -> ValidityPeriodType -- ^ 'vType'
    -> Validity
validity pValue_ pType_ = Validity' {_vValue = _Nat # pValue_, _vType = pType_}


-- | Time period.
vValue :: Lens' Validity Natural
vValue = lens _vValue (\ s a -> s{_vValue = a}) . _Nat

-- | Specifies whether the @Value@ parameter represents days, months, or years.
vType :: Lens' Validity ValidityPeriodType
vType = lens _vType (\ s a -> s{_vType = a})

instance Hashable Validity where

instance NFData Validity where

instance ToJSON Validity where
        toJSON Validity'{..}
          = object
              (catMaybes
                 [Just ("Value" .= _vValue), Just ("Type" .= _vType)])

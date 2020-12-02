{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.ASN1Subject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.ASN1Subject where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the certificate subject. The certificate can be one issued by your private certificate authority (CA) or it can be your private CA certificate. The __Subject__ field in the certificate identifies the entity that owns or controls the public key in the certificate. The entity can be a user, computer, device, or service. The __Subject__ must contain an X.500 distinguished name (DN). A DN is a sequence of relative distinguished names (RDNs). The RDNs are separated by commas in the certificate. The DN must be unique for each entity, but your private CA can issue more than one certificate with the same DN to the same entity.
--
--
--
-- /See:/ 'asn1Subject' smart constructor.
data ASN1Subject = ASN1Subject'
  { _asGivenName :: !(Maybe Text),
    _asState :: !(Maybe Text),
    _asCommonName :: !(Maybe Text),
    _asOrganizationalUnit :: !(Maybe Text),
    _asCountry :: !(Maybe Text),
    _asGenerationQualifier :: !(Maybe Text),
    _asLocality :: !(Maybe Text),
    _asPseudonym :: !(Maybe Text),
    _asInitials :: !(Maybe Text),
    _asTitle :: !(Maybe Text),
    _asOrganization :: !(Maybe Text),
    _asSerialNumber :: !(Maybe Text),
    _asSurname :: !(Maybe Text),
    _asDistinguishedNameQualifier :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

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
-- * 'asCountry' - Two-digit code that specifies the country in which the certificate subject located.
--
-- * 'asGenerationQualifier' - Typically a qualifier appended to the name of an individual. Examples include Jr. for junior, Sr. for senior, and III for third.
--
-- * 'asLocality' - The locality (such as a city or town) in which the certificate subject is located.
--
-- * 'asPseudonym' - Typically a shortened version of a longer __GivenName__ . For example, Jonathan is often shortened to John. Elizabeth is often shortened to Beth, Liz, or Eliza.
--
-- * 'asInitials' - Concatenation that typically contains the first letter of the __GivenName__ , the first letter of the middle name if one exists, and the first letter of the __SurName__ .
--
-- * 'asTitle' - A title such as Mr. or Ms., which is pre-pended to the name to refer formally to the certificate subject.
--
-- * 'asOrganization' - Legal name of the organization with which the certificate subject is affiliated.
--
-- * 'asSerialNumber' - The certificate serial number.
--
-- * 'asSurname' - Family name. In the US and the UK, for example, the surname of an individual is ordered last. In Asian cultures the surname is typically ordered first.
--
-- * 'asDistinguishedNameQualifier' - Disambiguating information for the certificate subject.
asn1Subject ::
  ASN1Subject
asn1Subject =
  ASN1Subject'
    { _asGivenName = Nothing,
      _asState = Nothing,
      _asCommonName = Nothing,
      _asOrganizationalUnit = Nothing,
      _asCountry = Nothing,
      _asGenerationQualifier = Nothing,
      _asLocality = Nothing,
      _asPseudonym = Nothing,
      _asInitials = Nothing,
      _asTitle = Nothing,
      _asOrganization = Nothing,
      _asSerialNumber = Nothing,
      _asSurname = Nothing,
      _asDistinguishedNameQualifier = Nothing
    }

-- | First name.
asGivenName :: Lens' ASN1Subject (Maybe Text)
asGivenName = lens _asGivenName (\s a -> s {_asGivenName = a})

-- | State in which the subject of the certificate is located.
asState :: Lens' ASN1Subject (Maybe Text)
asState = lens _asState (\s a -> s {_asState = a})

-- | Fully qualified domain name (FQDN) associated with the certificate subject.
asCommonName :: Lens' ASN1Subject (Maybe Text)
asCommonName = lens _asCommonName (\s a -> s {_asCommonName = a})

-- | A subdivision or unit of the organization (such as sales or finance) with which the certificate subject is affiliated.
asOrganizationalUnit :: Lens' ASN1Subject (Maybe Text)
asOrganizationalUnit = lens _asOrganizationalUnit (\s a -> s {_asOrganizationalUnit = a})

-- | Two-digit code that specifies the country in which the certificate subject located.
asCountry :: Lens' ASN1Subject (Maybe Text)
asCountry = lens _asCountry (\s a -> s {_asCountry = a})

-- | Typically a qualifier appended to the name of an individual. Examples include Jr. for junior, Sr. for senior, and III for third.
asGenerationQualifier :: Lens' ASN1Subject (Maybe Text)
asGenerationQualifier = lens _asGenerationQualifier (\s a -> s {_asGenerationQualifier = a})

-- | The locality (such as a city or town) in which the certificate subject is located.
asLocality :: Lens' ASN1Subject (Maybe Text)
asLocality = lens _asLocality (\s a -> s {_asLocality = a})

-- | Typically a shortened version of a longer __GivenName__ . For example, Jonathan is often shortened to John. Elizabeth is often shortened to Beth, Liz, or Eliza.
asPseudonym :: Lens' ASN1Subject (Maybe Text)
asPseudonym = lens _asPseudonym (\s a -> s {_asPseudonym = a})

-- | Concatenation that typically contains the first letter of the __GivenName__ , the first letter of the middle name if one exists, and the first letter of the __SurName__ .
asInitials :: Lens' ASN1Subject (Maybe Text)
asInitials = lens _asInitials (\s a -> s {_asInitials = a})

-- | A title such as Mr. or Ms., which is pre-pended to the name to refer formally to the certificate subject.
asTitle :: Lens' ASN1Subject (Maybe Text)
asTitle = lens _asTitle (\s a -> s {_asTitle = a})

-- | Legal name of the organization with which the certificate subject is affiliated.
asOrganization :: Lens' ASN1Subject (Maybe Text)
asOrganization = lens _asOrganization (\s a -> s {_asOrganization = a})

-- | The certificate serial number.
asSerialNumber :: Lens' ASN1Subject (Maybe Text)
asSerialNumber = lens _asSerialNumber (\s a -> s {_asSerialNumber = a})

-- | Family name. In the US and the UK, for example, the surname of an individual is ordered last. In Asian cultures the surname is typically ordered first.
asSurname :: Lens' ASN1Subject (Maybe Text)
asSurname = lens _asSurname (\s a -> s {_asSurname = a})

-- | Disambiguating information for the certificate subject.
asDistinguishedNameQualifier :: Lens' ASN1Subject (Maybe Text)
asDistinguishedNameQualifier = lens _asDistinguishedNameQualifier (\s a -> s {_asDistinguishedNameQualifier = a})

instance FromJSON ASN1Subject where
  parseJSON =
    withObject
      "ASN1Subject"
      ( \x ->
          ASN1Subject'
            <$> (x .:? "GivenName")
            <*> (x .:? "State")
            <*> (x .:? "CommonName")
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
            <*> (x .:? "DistinguishedNameQualifier")
      )

instance Hashable ASN1Subject

instance NFData ASN1Subject

instance ToJSON ASN1Subject where
  toJSON ASN1Subject' {..} =
    object
      ( catMaybes
          [ ("GivenName" .=) <$> _asGivenName,
            ("State" .=) <$> _asState,
            ("CommonName" .=) <$> _asCommonName,
            ("OrganizationalUnit" .=) <$> _asOrganizationalUnit,
            ("Country" .=) <$> _asCountry,
            ("GenerationQualifier" .=) <$> _asGenerationQualifier,
            ("Locality" .=) <$> _asLocality,
            ("Pseudonym" .=) <$> _asPseudonym,
            ("Initials" .=) <$> _asInitials,
            ("Title" .=) <$> _asTitle,
            ("Organization" .=) <$> _asOrganization,
            ("SerialNumber" .=) <$> _asSerialNumber,
            ("Surname" .=) <$> _asSurname,
            ("DistinguishedNameQualifier" .=)
              <$> _asDistinguishedNameQualifier
          ]
      )

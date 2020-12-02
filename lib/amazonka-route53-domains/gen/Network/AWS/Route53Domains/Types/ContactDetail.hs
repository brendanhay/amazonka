{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.ContactDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.ContactDetail where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53Domains.Types.ContactType
import Network.AWS.Route53Domains.Types.CountryCode
import Network.AWS.Route53Domains.Types.ExtraParam

-- | ContactDetail includes the following elements.
--
--
--
-- /See:/ 'contactDetail' smart constructor.
data ContactDetail = ContactDetail'
  { _cdOrganizationName ::
      !(Maybe Text),
    _cdEmail :: !(Maybe Text),
    _cdState :: !(Maybe Text),
    _cdFax :: !(Maybe Text),
    _cdLastName :: !(Maybe Text),
    _cdExtraParams :: !(Maybe [ExtraParam]),
    _cdZipCode :: !(Maybe Text),
    _cdAddressLine1 :: !(Maybe Text),
    _cdCity :: !(Maybe Text),
    _cdPhoneNumber :: !(Maybe Text),
    _cdAddressLine2 :: !(Maybe Text),
    _cdFirstName :: !(Maybe Text),
    _cdCountryCode :: !(Maybe CountryCode),
    _cdContactType :: !(Maybe ContactType)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContactDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdOrganizationName' - Name of the organization for contact types other than @PERSON@ .
--
-- * 'cdEmail' - Email address of the contact.
--
-- * 'cdState' - The state or province of the contact's city.
--
-- * 'cdFax' - Fax number of the contact. Constraints: Phone number must be specified in the format "+[country dialing code].[number including any area code]". For example, a US phone number might appear as @"+1.1234567890"@ .
--
-- * 'cdLastName' - Last name of contact.
--
-- * 'cdExtraParams' - A list of name-value pairs for parameters required by certain top-level domains.
--
-- * 'cdZipCode' - The zip or postal code of the contact's address.
--
-- * 'cdAddressLine1' - First line of the contact's address.
--
-- * 'cdCity' - The city of the contact's address.
--
-- * 'cdPhoneNumber' - The phone number of the contact. Constraints: Phone number must be specified in the format "+[country dialing code].[number including any area code>]". For example, a US phone number might appear as @"+1.1234567890"@ .
--
-- * 'cdAddressLine2' - Second line of contact's address, if any.
--
-- * 'cdFirstName' - First name of contact.
--
-- * 'cdCountryCode' - Code for the country of the contact's address.
--
-- * 'cdContactType' - Indicates whether the contact is a person, company, association, or public organization. Note the following:     * If you specify a value other than @PERSON@ , you must also specify a value for @OrganizationName@ .     * For some TLDs, the privacy protection available depends on the value that you specify for @Contact Type@ . For the privacy protection settings for your TLD, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/      * For .es domains, if you specify @PERSON@ , you must specify @INDIVIDUAL@ for the value of @ES_LEGAL_FORM@ .
contactDetail ::
  ContactDetail
contactDetail =
  ContactDetail'
    { _cdOrganizationName = Nothing,
      _cdEmail = Nothing,
      _cdState = Nothing,
      _cdFax = Nothing,
      _cdLastName = Nothing,
      _cdExtraParams = Nothing,
      _cdZipCode = Nothing,
      _cdAddressLine1 = Nothing,
      _cdCity = Nothing,
      _cdPhoneNumber = Nothing,
      _cdAddressLine2 = Nothing,
      _cdFirstName = Nothing,
      _cdCountryCode = Nothing,
      _cdContactType = Nothing
    }

-- | Name of the organization for contact types other than @PERSON@ .
cdOrganizationName :: Lens' ContactDetail (Maybe Text)
cdOrganizationName = lens _cdOrganizationName (\s a -> s {_cdOrganizationName = a})

-- | Email address of the contact.
cdEmail :: Lens' ContactDetail (Maybe Text)
cdEmail = lens _cdEmail (\s a -> s {_cdEmail = a})

-- | The state or province of the contact's city.
cdState :: Lens' ContactDetail (Maybe Text)
cdState = lens _cdState (\s a -> s {_cdState = a})

-- | Fax number of the contact. Constraints: Phone number must be specified in the format "+[country dialing code].[number including any area code]". For example, a US phone number might appear as @"+1.1234567890"@ .
cdFax :: Lens' ContactDetail (Maybe Text)
cdFax = lens _cdFax (\s a -> s {_cdFax = a})

-- | Last name of contact.
cdLastName :: Lens' ContactDetail (Maybe Text)
cdLastName = lens _cdLastName (\s a -> s {_cdLastName = a})

-- | A list of name-value pairs for parameters required by certain top-level domains.
cdExtraParams :: Lens' ContactDetail [ExtraParam]
cdExtraParams = lens _cdExtraParams (\s a -> s {_cdExtraParams = a}) . _Default . _Coerce

-- | The zip or postal code of the contact's address.
cdZipCode :: Lens' ContactDetail (Maybe Text)
cdZipCode = lens _cdZipCode (\s a -> s {_cdZipCode = a})

-- | First line of the contact's address.
cdAddressLine1 :: Lens' ContactDetail (Maybe Text)
cdAddressLine1 = lens _cdAddressLine1 (\s a -> s {_cdAddressLine1 = a})

-- | The city of the contact's address.
cdCity :: Lens' ContactDetail (Maybe Text)
cdCity = lens _cdCity (\s a -> s {_cdCity = a})

-- | The phone number of the contact. Constraints: Phone number must be specified in the format "+[country dialing code].[number including any area code>]". For example, a US phone number might appear as @"+1.1234567890"@ .
cdPhoneNumber :: Lens' ContactDetail (Maybe Text)
cdPhoneNumber = lens _cdPhoneNumber (\s a -> s {_cdPhoneNumber = a})

-- | Second line of contact's address, if any.
cdAddressLine2 :: Lens' ContactDetail (Maybe Text)
cdAddressLine2 = lens _cdAddressLine2 (\s a -> s {_cdAddressLine2 = a})

-- | First name of contact.
cdFirstName :: Lens' ContactDetail (Maybe Text)
cdFirstName = lens _cdFirstName (\s a -> s {_cdFirstName = a})

-- | Code for the country of the contact's address.
cdCountryCode :: Lens' ContactDetail (Maybe CountryCode)
cdCountryCode = lens _cdCountryCode (\s a -> s {_cdCountryCode = a})

-- | Indicates whether the contact is a person, company, association, or public organization. Note the following:     * If you specify a value other than @PERSON@ , you must also specify a value for @OrganizationName@ .     * For some TLDs, the privacy protection available depends on the value that you specify for @Contact Type@ . For the privacy protection settings for your TLD, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/      * For .es domains, if you specify @PERSON@ , you must specify @INDIVIDUAL@ for the value of @ES_LEGAL_FORM@ .
cdContactType :: Lens' ContactDetail (Maybe ContactType)
cdContactType = lens _cdContactType (\s a -> s {_cdContactType = a})

instance FromJSON ContactDetail where
  parseJSON =
    withObject
      "ContactDetail"
      ( \x ->
          ContactDetail'
            <$> (x .:? "OrganizationName")
            <*> (x .:? "Email")
            <*> (x .:? "State")
            <*> (x .:? "Fax")
            <*> (x .:? "LastName")
            <*> (x .:? "ExtraParams" .!= mempty)
            <*> (x .:? "ZipCode")
            <*> (x .:? "AddressLine1")
            <*> (x .:? "City")
            <*> (x .:? "PhoneNumber")
            <*> (x .:? "AddressLine2")
            <*> (x .:? "FirstName")
            <*> (x .:? "CountryCode")
            <*> (x .:? "ContactType")
      )

instance Hashable ContactDetail

instance NFData ContactDetail

instance ToJSON ContactDetail where
  toJSON ContactDetail' {..} =
    object
      ( catMaybes
          [ ("OrganizationName" .=) <$> _cdOrganizationName,
            ("Email" .=) <$> _cdEmail,
            ("State" .=) <$> _cdState,
            ("Fax" .=) <$> _cdFax,
            ("LastName" .=) <$> _cdLastName,
            ("ExtraParams" .=) <$> _cdExtraParams,
            ("ZipCode" .=) <$> _cdZipCode,
            ("AddressLine1" .=) <$> _cdAddressLine1,
            ("City" .=) <$> _cdCity,
            ("PhoneNumber" .=) <$> _cdPhoneNumber,
            ("AddressLine2" .=) <$> _cdAddressLine2,
            ("FirstName" .=) <$> _cdFirstName,
            ("CountryCode" .=) <$> _cdCountryCode,
            ("ContactType" .=) <$> _cdContactType
          ]
      )

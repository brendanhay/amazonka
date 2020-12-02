{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Contact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Contact where

import Network.AWS.AlexaBusiness.Types.PhoneNumber
import Network.AWS.AlexaBusiness.Types.SipAddress
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A contact with attributes.
--
--
--
-- /See:/ 'contact' smart constructor.
data Contact = Contact'
  { _cLastName :: !(Maybe Text),
    _cContactARN :: !(Maybe Text),
    _cPhoneNumbers :: !(Maybe [PhoneNumber]),
    _cPhoneNumber :: !(Maybe (Sensitive Text)),
    _cSipAddresses :: !(Maybe [SipAddress]),
    _cFirstName :: !(Maybe Text),
    _cDisplayName :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'Contact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cLastName' - The last name of the contact, used to call the contact on the device.
--
-- * 'cContactARN' - The ARN of the contact.
--
-- * 'cPhoneNumbers' - The list of phone numbers for the contact.
--
-- * 'cPhoneNumber' - The phone number of the contact. The phone number type defaults to WORK. You can either specify PhoneNumber or PhoneNumbers. We recommend that you use PhoneNumbers, which lets you specify the phone number type and multiple numbers.
--
-- * 'cSipAddresses' - The list of SIP addresses for the contact.
--
-- * 'cFirstName' - The first name of the contact, used to call the contact on the device.
--
-- * 'cDisplayName' - The name of the contact to display on the console.
contact ::
  Contact
contact =
  Contact'
    { _cLastName = Nothing,
      _cContactARN = Nothing,
      _cPhoneNumbers = Nothing,
      _cPhoneNumber = Nothing,
      _cSipAddresses = Nothing,
      _cFirstName = Nothing,
      _cDisplayName = Nothing
    }

-- | The last name of the contact, used to call the contact on the device.
cLastName :: Lens' Contact (Maybe Text)
cLastName = lens _cLastName (\s a -> s {_cLastName = a})

-- | The ARN of the contact.
cContactARN :: Lens' Contact (Maybe Text)
cContactARN = lens _cContactARN (\s a -> s {_cContactARN = a})

-- | The list of phone numbers for the contact.
cPhoneNumbers :: Lens' Contact [PhoneNumber]
cPhoneNumbers = lens _cPhoneNumbers (\s a -> s {_cPhoneNumbers = a}) . _Default . _Coerce

-- | The phone number of the contact. The phone number type defaults to WORK. You can either specify PhoneNumber or PhoneNumbers. We recommend that you use PhoneNumbers, which lets you specify the phone number type and multiple numbers.
cPhoneNumber :: Lens' Contact (Maybe Text)
cPhoneNumber = lens _cPhoneNumber (\s a -> s {_cPhoneNumber = a}) . mapping _Sensitive

-- | The list of SIP addresses for the contact.
cSipAddresses :: Lens' Contact [SipAddress]
cSipAddresses = lens _cSipAddresses (\s a -> s {_cSipAddresses = a}) . _Default . _Coerce

-- | The first name of the contact, used to call the contact on the device.
cFirstName :: Lens' Contact (Maybe Text)
cFirstName = lens _cFirstName (\s a -> s {_cFirstName = a})

-- | The name of the contact to display on the console.
cDisplayName :: Lens' Contact (Maybe Text)
cDisplayName = lens _cDisplayName (\s a -> s {_cDisplayName = a})

instance FromJSON Contact where
  parseJSON =
    withObject
      "Contact"
      ( \x ->
          Contact'
            <$> (x .:? "LastName")
            <*> (x .:? "ContactArn")
            <*> (x .:? "PhoneNumbers" .!= mempty)
            <*> (x .:? "PhoneNumber")
            <*> (x .:? "SipAddresses" .!= mempty)
            <*> (x .:? "FirstName")
            <*> (x .:? "DisplayName")
      )

instance Hashable Contact

instance NFData Contact

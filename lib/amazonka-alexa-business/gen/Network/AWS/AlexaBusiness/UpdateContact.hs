{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateContact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the contact details by the contact ARN.
module Network.AWS.AlexaBusiness.UpdateContact
  ( -- * Creating a Request
    updateContact,
    UpdateContact,

    -- * Request Lenses
    ucLastName,
    ucPhoneNumbers,
    ucPhoneNumber,
    ucSipAddresses,
    ucFirstName,
    ucDisplayName,
    ucContactARN,

    -- * Destructuring the Response
    updateContactResponse,
    UpdateContactResponse,

    -- * Response Lenses
    ucrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateContact' smart constructor.
data UpdateContact = UpdateContact'
  { _ucLastName :: !(Maybe Text),
    _ucPhoneNumbers :: !(Maybe [PhoneNumber]),
    _ucPhoneNumber :: !(Maybe (Sensitive Text)),
    _ucSipAddresses :: !(Maybe [SipAddress]),
    _ucFirstName :: !(Maybe Text),
    _ucDisplayName :: !(Maybe Text),
    _ucContactARN :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateContact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucLastName' - The updated last name of the contact.
--
-- * 'ucPhoneNumbers' - The list of phone numbers for the contact.
--
-- * 'ucPhoneNumber' - The updated phone number of the contact. The phone number type defaults to WORK. You can either specify PhoneNumber or PhoneNumbers. We recommend that you use PhoneNumbers, which lets you specify the phone number type and multiple numbers.
--
-- * 'ucSipAddresses' - The list of SIP addresses for the contact.
--
-- * 'ucFirstName' - The updated first name of the contact.
--
-- * 'ucDisplayName' - The updated display name of the contact.
--
-- * 'ucContactARN' - The ARN of the contact to update.
updateContact ::
  -- | 'ucContactARN'
  Text ->
  UpdateContact
updateContact pContactARN_ =
  UpdateContact'
    { _ucLastName = Nothing,
      _ucPhoneNumbers = Nothing,
      _ucPhoneNumber = Nothing,
      _ucSipAddresses = Nothing,
      _ucFirstName = Nothing,
      _ucDisplayName = Nothing,
      _ucContactARN = pContactARN_
    }

-- | The updated last name of the contact.
ucLastName :: Lens' UpdateContact (Maybe Text)
ucLastName = lens _ucLastName (\s a -> s {_ucLastName = a})

-- | The list of phone numbers for the contact.
ucPhoneNumbers :: Lens' UpdateContact [PhoneNumber]
ucPhoneNumbers = lens _ucPhoneNumbers (\s a -> s {_ucPhoneNumbers = a}) . _Default . _Coerce

-- | The updated phone number of the contact. The phone number type defaults to WORK. You can either specify PhoneNumber or PhoneNumbers. We recommend that you use PhoneNumbers, which lets you specify the phone number type and multiple numbers.
ucPhoneNumber :: Lens' UpdateContact (Maybe Text)
ucPhoneNumber = lens _ucPhoneNumber (\s a -> s {_ucPhoneNumber = a}) . mapping _Sensitive

-- | The list of SIP addresses for the contact.
ucSipAddresses :: Lens' UpdateContact [SipAddress]
ucSipAddresses = lens _ucSipAddresses (\s a -> s {_ucSipAddresses = a}) . _Default . _Coerce

-- | The updated first name of the contact.
ucFirstName :: Lens' UpdateContact (Maybe Text)
ucFirstName = lens _ucFirstName (\s a -> s {_ucFirstName = a})

-- | The updated display name of the contact.
ucDisplayName :: Lens' UpdateContact (Maybe Text)
ucDisplayName = lens _ucDisplayName (\s a -> s {_ucDisplayName = a})

-- | The ARN of the contact to update.
ucContactARN :: Lens' UpdateContact Text
ucContactARN = lens _ucContactARN (\s a -> s {_ucContactARN = a})

instance AWSRequest UpdateContact where
  type Rs UpdateContact = UpdateContactResponse
  request = postJSON alexaBusiness
  response =
    receiveEmpty
      (\s h x -> UpdateContactResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateContact

instance NFData UpdateContact

instance ToHeaders UpdateContact where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AlexaForBusiness.UpdateContact" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateContact where
  toJSON UpdateContact' {..} =
    object
      ( catMaybes
          [ ("LastName" .=) <$> _ucLastName,
            ("PhoneNumbers" .=) <$> _ucPhoneNumbers,
            ("PhoneNumber" .=) <$> _ucPhoneNumber,
            ("SipAddresses" .=) <$> _ucSipAddresses,
            ("FirstName" .=) <$> _ucFirstName,
            ("DisplayName" .=) <$> _ucDisplayName,
            Just ("ContactArn" .= _ucContactARN)
          ]
      )

instance ToPath UpdateContact where
  toPath = const "/"

instance ToQuery UpdateContact where
  toQuery = const mempty

-- | /See:/ 'updateContactResponse' smart constructor.
newtype UpdateContactResponse = UpdateContactResponse'
  { _ucrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateContactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucrsResponseStatus' - -- | The response status code.
updateContactResponse ::
  -- | 'ucrsResponseStatus'
  Int ->
  UpdateContactResponse
updateContactResponse pResponseStatus_ =
  UpdateContactResponse' {_ucrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
ucrsResponseStatus :: Lens' UpdateContactResponse Int
ucrsResponseStatus = lens _ucrsResponseStatus (\s a -> s {_ucrsResponseStatus = a})

instance NFData UpdateContactResponse

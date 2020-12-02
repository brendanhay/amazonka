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
-- Module      : Network.AWS.AlexaBusiness.CreateContact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a contact with the specified details.
module Network.AWS.AlexaBusiness.CreateContact
  ( -- * Creating a Request
    createContact,
    CreateContact,

    -- * Request Lenses
    ccLastName,
    ccPhoneNumbers,
    ccPhoneNumber,
    ccSipAddresses,
    ccDisplayName,
    ccClientRequestToken,
    ccFirstName,

    -- * Destructuring the Response
    createContactResponse,
    CreateContactResponse,

    -- * Response Lenses
    ccrsContactARN,
    ccrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createContact' smart constructor.
data CreateContact = CreateContact'
  { _ccLastName :: !(Maybe Text),
    _ccPhoneNumbers :: !(Maybe [PhoneNumber]),
    _ccPhoneNumber :: !(Maybe (Sensitive Text)),
    _ccSipAddresses :: !(Maybe [SipAddress]),
    _ccDisplayName :: !(Maybe Text),
    _ccClientRequestToken :: !(Maybe Text),
    _ccFirstName :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateContact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccLastName' - The last name of the contact that is used to call the contact on the device.
--
-- * 'ccPhoneNumbers' - The list of phone numbers for the contact.
--
-- * 'ccPhoneNumber' - The phone number of the contact in E.164 format. The phone number type defaults to WORK. You can specify PhoneNumber or PhoneNumbers. We recommend that you use PhoneNumbers, which lets you specify the phone number type and multiple numbers.
--
-- * 'ccSipAddresses' - The list of SIP addresses for the contact.
--
-- * 'ccDisplayName' - The name of the contact to display on the console.
--
-- * 'ccClientRequestToken' - A unique, user-specified identifier for this request that ensures idempotency.
--
-- * 'ccFirstName' - The first name of the contact that is used to call the contact on the device.
createContact ::
  -- | 'ccFirstName'
  Text ->
  CreateContact
createContact pFirstName_ =
  CreateContact'
    { _ccLastName = Nothing,
      _ccPhoneNumbers = Nothing,
      _ccPhoneNumber = Nothing,
      _ccSipAddresses = Nothing,
      _ccDisplayName = Nothing,
      _ccClientRequestToken = Nothing,
      _ccFirstName = pFirstName_
    }

-- | The last name of the contact that is used to call the contact on the device.
ccLastName :: Lens' CreateContact (Maybe Text)
ccLastName = lens _ccLastName (\s a -> s {_ccLastName = a})

-- | The list of phone numbers for the contact.
ccPhoneNumbers :: Lens' CreateContact [PhoneNumber]
ccPhoneNumbers = lens _ccPhoneNumbers (\s a -> s {_ccPhoneNumbers = a}) . _Default . _Coerce

-- | The phone number of the contact in E.164 format. The phone number type defaults to WORK. You can specify PhoneNumber or PhoneNumbers. We recommend that you use PhoneNumbers, which lets you specify the phone number type and multiple numbers.
ccPhoneNumber :: Lens' CreateContact (Maybe Text)
ccPhoneNumber = lens _ccPhoneNumber (\s a -> s {_ccPhoneNumber = a}) . mapping _Sensitive

-- | The list of SIP addresses for the contact.
ccSipAddresses :: Lens' CreateContact [SipAddress]
ccSipAddresses = lens _ccSipAddresses (\s a -> s {_ccSipAddresses = a}) . _Default . _Coerce

-- | The name of the contact to display on the console.
ccDisplayName :: Lens' CreateContact (Maybe Text)
ccDisplayName = lens _ccDisplayName (\s a -> s {_ccDisplayName = a})

-- | A unique, user-specified identifier for this request that ensures idempotency.
ccClientRequestToken :: Lens' CreateContact (Maybe Text)
ccClientRequestToken = lens _ccClientRequestToken (\s a -> s {_ccClientRequestToken = a})

-- | The first name of the contact that is used to call the contact on the device.
ccFirstName :: Lens' CreateContact Text
ccFirstName = lens _ccFirstName (\s a -> s {_ccFirstName = a})

instance AWSRequest CreateContact where
  type Rs CreateContact = CreateContactResponse
  request = postJSON alexaBusiness
  response =
    receiveJSON
      ( \s h x ->
          CreateContactResponse'
            <$> (x .?> "ContactArn") <*> (pure (fromEnum s))
      )

instance Hashable CreateContact

instance NFData CreateContact

instance ToHeaders CreateContact where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AlexaForBusiness.CreateContact" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateContact where
  toJSON CreateContact' {..} =
    object
      ( catMaybes
          [ ("LastName" .=) <$> _ccLastName,
            ("PhoneNumbers" .=) <$> _ccPhoneNumbers,
            ("PhoneNumber" .=) <$> _ccPhoneNumber,
            ("SipAddresses" .=) <$> _ccSipAddresses,
            ("DisplayName" .=) <$> _ccDisplayName,
            ("ClientRequestToken" .=) <$> _ccClientRequestToken,
            Just ("FirstName" .= _ccFirstName)
          ]
      )

instance ToPath CreateContact where
  toPath = const "/"

instance ToQuery CreateContact where
  toQuery = const mempty

-- | /See:/ 'createContactResponse' smart constructor.
data CreateContactResponse = CreateContactResponse'
  { _ccrsContactARN ::
      !(Maybe Text),
    _ccrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateContactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsContactARN' - The ARN of the newly created address book.
--
-- * 'ccrsResponseStatus' - -- | The response status code.
createContactResponse ::
  -- | 'ccrsResponseStatus'
  Int ->
  CreateContactResponse
createContactResponse pResponseStatus_ =
  CreateContactResponse'
    { _ccrsContactARN = Nothing,
      _ccrsResponseStatus = pResponseStatus_
    }

-- | The ARN of the newly created address book.
ccrsContactARN :: Lens' CreateContactResponse (Maybe Text)
ccrsContactARN = lens _ccrsContactARN (\s a -> s {_ccrsContactARN = a})

-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateContactResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\s a -> s {_ccrsResponseStatus = a})

instance NFData CreateContactResponse

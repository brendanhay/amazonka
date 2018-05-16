{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateContact
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a contact with the specified details.
--
--
module Network.AWS.AlexaBusiness.CreateContact
    (
    -- * Creating a Request
      createContact
    , CreateContact
    -- * Request Lenses
    , ccLastName
    , ccDisplayName
    , ccClientRequestToken
    , ccFirstName
    , ccPhoneNumber

    -- * Destructuring the Response
    , createContactResponse
    , CreateContactResponse
    -- * Response Lenses
    , ccrsContactARN
    , ccrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createContact' smart constructor.
data CreateContact = CreateContact'
  { _ccLastName           :: !(Maybe Text)
  , _ccDisplayName        :: !(Maybe Text)
  , _ccClientRequestToken :: !(Maybe Text)
  , _ccFirstName          :: !Text
  , _ccPhoneNumber        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateContact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccLastName' - The last name of the contact that is used to call the contact on the device.
--
-- * 'ccDisplayName' - The name of the contact to display on the console.
--
-- * 'ccClientRequestToken' - A unique, user-specified identifier for this request that ensures idempotency.
--
-- * 'ccFirstName' - The first name of the contact that is used to call the contact on the device.
--
-- * 'ccPhoneNumber' - The phone number of the contact in E.164 format.
createContact
    :: Text -- ^ 'ccFirstName'
    -> Text -- ^ 'ccPhoneNumber'
    -> CreateContact
createContact pFirstName_ pPhoneNumber_ =
  CreateContact'
    { _ccLastName = Nothing
    , _ccDisplayName = Nothing
    , _ccClientRequestToken = Nothing
    , _ccFirstName = pFirstName_
    , _ccPhoneNumber = pPhoneNumber_
    }


-- | The last name of the contact that is used to call the contact on the device.
ccLastName :: Lens' CreateContact (Maybe Text)
ccLastName = lens _ccLastName (\ s a -> s{_ccLastName = a})

-- | The name of the contact to display on the console.
ccDisplayName :: Lens' CreateContact (Maybe Text)
ccDisplayName = lens _ccDisplayName (\ s a -> s{_ccDisplayName = a})

-- | A unique, user-specified identifier for this request that ensures idempotency.
ccClientRequestToken :: Lens' CreateContact (Maybe Text)
ccClientRequestToken = lens _ccClientRequestToken (\ s a -> s{_ccClientRequestToken = a})

-- | The first name of the contact that is used to call the contact on the device.
ccFirstName :: Lens' CreateContact Text
ccFirstName = lens _ccFirstName (\ s a -> s{_ccFirstName = a})

-- | The phone number of the contact in E.164 format.
ccPhoneNumber :: Lens' CreateContact Text
ccPhoneNumber = lens _ccPhoneNumber (\ s a -> s{_ccPhoneNumber = a})

instance AWSRequest CreateContact where
        type Rs CreateContact = CreateContactResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 CreateContactResponse' <$>
                   (x .?> "ContactArn") <*> (pure (fromEnum s)))

instance Hashable CreateContact where

instance NFData CreateContact where

instance ToHeaders CreateContact where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.CreateContact" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateContact where
        toJSON CreateContact'{..}
          = object
              (catMaybes
                 [("LastName" .=) <$> _ccLastName,
                  ("DisplayName" .=) <$> _ccDisplayName,
                  ("ClientRequestToken" .=) <$> _ccClientRequestToken,
                  Just ("FirstName" .= _ccFirstName),
                  Just ("PhoneNumber" .= _ccPhoneNumber)])

instance ToPath CreateContact where
        toPath = const "/"

instance ToQuery CreateContact where
        toQuery = const mempty

-- | /See:/ 'createContactResponse' smart constructor.
data CreateContactResponse = CreateContactResponse'
  { _ccrsContactARN     :: !(Maybe Text)
  , _ccrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateContactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsContactARN' - The ARN of the newly created address book.
--
-- * 'ccrsResponseStatus' - -- | The response status code.
createContactResponse
    :: Int -- ^ 'ccrsResponseStatus'
    -> CreateContactResponse
createContactResponse pResponseStatus_ =
  CreateContactResponse'
    {_ccrsContactARN = Nothing, _ccrsResponseStatus = pResponseStatus_}


-- | The ARN of the newly created address book.
ccrsContactARN :: Lens' CreateContactResponse (Maybe Text)
ccrsContactARN = lens _ccrsContactARN (\ s a -> s{_ccrsContactARN = a})

-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateContactResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\ s a -> s{_ccrsResponseStatus = a})

instance NFData CreateContactResponse where

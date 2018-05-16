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
-- Module      : Network.AWS.AlexaBusiness.UpdateContact
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the contact details by the contact ARN.
--
--
module Network.AWS.AlexaBusiness.UpdateContact
    (
    -- * Creating a Request
      updateContact
    , UpdateContact
    -- * Request Lenses
    , ucLastName
    , ucPhoneNumber
    , ucFirstName
    , ucDisplayName
    , ucContactARN

    -- * Destructuring the Response
    , updateContactResponse
    , UpdateContactResponse
    -- * Response Lenses
    , ucrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateContact' smart constructor.
data UpdateContact = UpdateContact'
  { _ucLastName    :: !(Maybe Text)
  , _ucPhoneNumber :: !(Maybe Text)
  , _ucFirstName   :: !(Maybe Text)
  , _ucDisplayName :: !(Maybe Text)
  , _ucContactARN  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateContact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucLastName' - The updated last name of the contact.
--
-- * 'ucPhoneNumber' - The updated phone number of the contact.
--
-- * 'ucFirstName' - The updated first name of the contact.
--
-- * 'ucDisplayName' - The updated display name of the contact.
--
-- * 'ucContactARN' - The ARN of the contact to update.
updateContact
    :: Text -- ^ 'ucContactARN'
    -> UpdateContact
updateContact pContactARN_ =
  UpdateContact'
    { _ucLastName = Nothing
    , _ucPhoneNumber = Nothing
    , _ucFirstName = Nothing
    , _ucDisplayName = Nothing
    , _ucContactARN = pContactARN_
    }


-- | The updated last name of the contact.
ucLastName :: Lens' UpdateContact (Maybe Text)
ucLastName = lens _ucLastName (\ s a -> s{_ucLastName = a})

-- | The updated phone number of the contact.
ucPhoneNumber :: Lens' UpdateContact (Maybe Text)
ucPhoneNumber = lens _ucPhoneNumber (\ s a -> s{_ucPhoneNumber = a})

-- | The updated first name of the contact.
ucFirstName :: Lens' UpdateContact (Maybe Text)
ucFirstName = lens _ucFirstName (\ s a -> s{_ucFirstName = a})

-- | The updated display name of the contact.
ucDisplayName :: Lens' UpdateContact (Maybe Text)
ucDisplayName = lens _ucDisplayName (\ s a -> s{_ucDisplayName = a})

-- | The ARN of the contact to update.
ucContactARN :: Lens' UpdateContact Text
ucContactARN = lens _ucContactARN (\ s a -> s{_ucContactARN = a})

instance AWSRequest UpdateContact where
        type Rs UpdateContact = UpdateContactResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateContactResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateContact where

instance NFData UpdateContact where

instance ToHeaders UpdateContact where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.UpdateContact" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateContact where
        toJSON UpdateContact'{..}
          = object
              (catMaybes
                 [("LastName" .=) <$> _ucLastName,
                  ("PhoneNumber" .=) <$> _ucPhoneNumber,
                  ("FirstName" .=) <$> _ucFirstName,
                  ("DisplayName" .=) <$> _ucDisplayName,
                  Just ("ContactArn" .= _ucContactARN)])

instance ToPath UpdateContact where
        toPath = const "/"

instance ToQuery UpdateContact where
        toQuery = const mempty

-- | /See:/ 'updateContactResponse' smart constructor.
newtype UpdateContactResponse = UpdateContactResponse'
  { _ucrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateContactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucrsResponseStatus' - -- | The response status code.
updateContactResponse
    :: Int -- ^ 'ucrsResponseStatus'
    -> UpdateContactResponse
updateContactResponse pResponseStatus_ =
  UpdateContactResponse' {_ucrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ucrsResponseStatus :: Lens' UpdateContactResponse Int
ucrsResponseStatus = lens _ucrsResponseStatus (\ s a -> s{_ucrsResponseStatus = a})

instance NFData UpdateContactResponse where

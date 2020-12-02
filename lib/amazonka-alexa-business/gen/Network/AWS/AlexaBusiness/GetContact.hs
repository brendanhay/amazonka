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
-- Module      : Network.AWS.AlexaBusiness.GetContact
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the contact details by the contact ARN.
--
--
module Network.AWS.AlexaBusiness.GetContact
    (
    -- * Creating a Request
      getContact
    , GetContact
    -- * Request Lenses
    , gcContactARN

    -- * Destructuring the Response
    , getContactResponse
    , GetContactResponse
    -- * Response Lenses
    , gcrsContact
    , gcrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getContact' smart constructor.
newtype GetContact = GetContact'
  { _gcContactARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetContact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcContactARN' - The ARN of the contact for which to request details.
getContact
    :: Text -- ^ 'gcContactARN'
    -> GetContact
getContact pContactARN_ = GetContact' {_gcContactARN = pContactARN_}


-- | The ARN of the contact for which to request details.
gcContactARN :: Lens' GetContact Text
gcContactARN = lens _gcContactARN (\ s a -> s{_gcContactARN = a})

instance AWSRequest GetContact where
        type Rs GetContact = GetContactResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 GetContactResponse' <$>
                   (x .?> "Contact") <*> (pure (fromEnum s)))

instance Hashable GetContact where

instance NFData GetContact where

instance ToHeaders GetContact where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.GetContact" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetContact where
        toJSON GetContact'{..}
          = object
              (catMaybes [Just ("ContactArn" .= _gcContactARN)])

instance ToPath GetContact where
        toPath = const "/"

instance ToQuery GetContact where
        toQuery = const mempty

-- | /See:/ 'getContactResponse' smart constructor.
data GetContactResponse = GetContactResponse'
  { _gcrsContact        :: !(Maybe Contact)
  , _gcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetContactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrsContact' - The details of the requested contact.
--
-- * 'gcrsResponseStatus' - -- | The response status code.
getContactResponse
    :: Int -- ^ 'gcrsResponseStatus'
    -> GetContactResponse
getContactResponse pResponseStatus_ =
  GetContactResponse'
    {_gcrsContact = Nothing, _gcrsResponseStatus = pResponseStatus_}


-- | The details of the requested contact.
gcrsContact :: Lens' GetContactResponse (Maybe Contact)
gcrsContact = lens _gcrsContact (\ s a -> s{_gcrsContact = a})

-- | -- | The response status code.
gcrsResponseStatus :: Lens' GetContactResponse Int
gcrsResponseStatus = lens _gcrsResponseStatus (\ s a -> s{_gcrsResponseStatus = a})

instance NFData GetContactResponse where

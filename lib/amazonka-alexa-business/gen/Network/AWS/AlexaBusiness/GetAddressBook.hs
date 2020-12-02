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
-- Module      : Network.AWS.AlexaBusiness.GetAddressBook
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets address the book details by the address book ARN.
--
--
module Network.AWS.AlexaBusiness.GetAddressBook
    (
    -- * Creating a Request
      getAddressBook
    , GetAddressBook
    -- * Request Lenses
    , gabAddressBookARN

    -- * Destructuring the Response
    , getAddressBookResponse
    , GetAddressBookResponse
    -- * Response Lenses
    , gabrsAddressBook
    , gabrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAddressBook' smart constructor.
newtype GetAddressBook = GetAddressBook'
  { _gabAddressBookARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAddressBook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gabAddressBookARN' - The ARN of the address book for which to request details.
getAddressBook
    :: Text -- ^ 'gabAddressBookARN'
    -> GetAddressBook
getAddressBook pAddressBookARN_ =
  GetAddressBook' {_gabAddressBookARN = pAddressBookARN_}


-- | The ARN of the address book for which to request details.
gabAddressBookARN :: Lens' GetAddressBook Text
gabAddressBookARN = lens _gabAddressBookARN (\ s a -> s{_gabAddressBookARN = a})

instance AWSRequest GetAddressBook where
        type Rs GetAddressBook = GetAddressBookResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 GetAddressBookResponse' <$>
                   (x .?> "AddressBook") <*> (pure (fromEnum s)))

instance Hashable GetAddressBook where

instance NFData GetAddressBook where

instance ToHeaders GetAddressBook where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.GetAddressBook" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetAddressBook where
        toJSON GetAddressBook'{..}
          = object
              (catMaybes
                 [Just ("AddressBookArn" .= _gabAddressBookARN)])

instance ToPath GetAddressBook where
        toPath = const "/"

instance ToQuery GetAddressBook where
        toQuery = const mempty

-- | /See:/ 'getAddressBookResponse' smart constructor.
data GetAddressBookResponse = GetAddressBookResponse'
  { _gabrsAddressBook    :: !(Maybe AddressBook)
  , _gabrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAddressBookResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gabrsAddressBook' - The details of the requested address book.
--
-- * 'gabrsResponseStatus' - -- | The response status code.
getAddressBookResponse
    :: Int -- ^ 'gabrsResponseStatus'
    -> GetAddressBookResponse
getAddressBookResponse pResponseStatus_ =
  GetAddressBookResponse'
    {_gabrsAddressBook = Nothing, _gabrsResponseStatus = pResponseStatus_}


-- | The details of the requested address book.
gabrsAddressBook :: Lens' GetAddressBookResponse (Maybe AddressBook)
gabrsAddressBook = lens _gabrsAddressBook (\ s a -> s{_gabrsAddressBook = a})

-- | -- | The response status code.
gabrsResponseStatus :: Lens' GetAddressBookResponse Int
gabrsResponseStatus = lens _gabrsResponseStatus (\ s a -> s{_gabrsResponseStatus = a})

instance NFData GetAddressBookResponse where

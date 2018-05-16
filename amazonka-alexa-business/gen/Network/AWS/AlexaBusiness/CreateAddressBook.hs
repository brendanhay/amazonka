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
-- Module      : Network.AWS.AlexaBusiness.CreateAddressBook
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an address book with the specified details.
--
--
module Network.AWS.AlexaBusiness.CreateAddressBook
    (
    -- * Creating a Request
      createAddressBook
    , CreateAddressBook
    -- * Request Lenses
    , cabClientRequestToken
    , cabDescription
    , cabName

    -- * Destructuring the Response
    , createAddressBookResponse
    , CreateAddressBookResponse
    -- * Response Lenses
    , cabrsAddressBookARN
    , cabrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createAddressBook' smart constructor.
data CreateAddressBook = CreateAddressBook'
  { _cabClientRequestToken :: !(Maybe Text)
  , _cabDescription        :: !(Maybe Text)
  , _cabName               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAddressBook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cabClientRequestToken' - A unique, user-specified identifier for the request that ensures idempotency.
--
-- * 'cabDescription' - The description of the address book.
--
-- * 'cabName' - The name of the address book.
createAddressBook
    :: Text -- ^ 'cabName'
    -> CreateAddressBook
createAddressBook pName_ =
  CreateAddressBook'
    { _cabClientRequestToken = Nothing
    , _cabDescription = Nothing
    , _cabName = pName_
    }


-- | A unique, user-specified identifier for the request that ensures idempotency.
cabClientRequestToken :: Lens' CreateAddressBook (Maybe Text)
cabClientRequestToken = lens _cabClientRequestToken (\ s a -> s{_cabClientRequestToken = a})

-- | The description of the address book.
cabDescription :: Lens' CreateAddressBook (Maybe Text)
cabDescription = lens _cabDescription (\ s a -> s{_cabDescription = a})

-- | The name of the address book.
cabName :: Lens' CreateAddressBook Text
cabName = lens _cabName (\ s a -> s{_cabName = a})

instance AWSRequest CreateAddressBook where
        type Rs CreateAddressBook = CreateAddressBookResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 CreateAddressBookResponse' <$>
                   (x .?> "AddressBookArn") <*> (pure (fromEnum s)))

instance Hashable CreateAddressBook where

instance NFData CreateAddressBook where

instance ToHeaders CreateAddressBook where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.CreateAddressBook" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateAddressBook where
        toJSON CreateAddressBook'{..}
          = object
              (catMaybes
                 [("ClientRequestToken" .=) <$>
                    _cabClientRequestToken,
                  ("Description" .=) <$> _cabDescription,
                  Just ("Name" .= _cabName)])

instance ToPath CreateAddressBook where
        toPath = const "/"

instance ToQuery CreateAddressBook where
        toQuery = const mempty

-- | /See:/ 'createAddressBookResponse' smart constructor.
data CreateAddressBookResponse = CreateAddressBookResponse'
  { _cabrsAddressBookARN :: !(Maybe Text)
  , _cabrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAddressBookResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cabrsAddressBookARN' - The ARN of the newly created address book.
--
-- * 'cabrsResponseStatus' - -- | The response status code.
createAddressBookResponse
    :: Int -- ^ 'cabrsResponseStatus'
    -> CreateAddressBookResponse
createAddressBookResponse pResponseStatus_ =
  CreateAddressBookResponse'
    {_cabrsAddressBookARN = Nothing, _cabrsResponseStatus = pResponseStatus_}


-- | The ARN of the newly created address book.
cabrsAddressBookARN :: Lens' CreateAddressBookResponse (Maybe Text)
cabrsAddressBookARN = lens _cabrsAddressBookARN (\ s a -> s{_cabrsAddressBookARN = a})

-- | -- | The response status code.
cabrsResponseStatus :: Lens' CreateAddressBookResponse Int
cabrsResponseStatus = lens _cabrsResponseStatus (\ s a -> s{_cabrsResponseStatus = a})

instance NFData CreateAddressBookResponse where

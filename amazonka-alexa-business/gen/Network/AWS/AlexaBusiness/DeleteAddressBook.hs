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
-- Module      : Network.AWS.AlexaBusiness.DeleteAddressBook
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an address book by the address book ARN.
--
--
module Network.AWS.AlexaBusiness.DeleteAddressBook
    (
    -- * Creating a Request
      deleteAddressBook
    , DeleteAddressBook
    -- * Request Lenses
    , dabAddressBookARN

    -- * Destructuring the Response
    , deleteAddressBookResponse
    , DeleteAddressBookResponse
    -- * Response Lenses
    , dabrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAddressBook' smart constructor.
newtype DeleteAddressBook = DeleteAddressBook'
  { _dabAddressBookARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAddressBook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dabAddressBookARN' - The ARN of the address book to delete.
deleteAddressBook
    :: Text -- ^ 'dabAddressBookARN'
    -> DeleteAddressBook
deleteAddressBook pAddressBookARN_ =
  DeleteAddressBook' {_dabAddressBookARN = pAddressBookARN_}


-- | The ARN of the address book to delete.
dabAddressBookARN :: Lens' DeleteAddressBook Text
dabAddressBookARN = lens _dabAddressBookARN (\ s a -> s{_dabAddressBookARN = a})

instance AWSRequest DeleteAddressBook where
        type Rs DeleteAddressBook = DeleteAddressBookResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteAddressBookResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteAddressBook where

instance NFData DeleteAddressBook where

instance ToHeaders DeleteAddressBook where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.DeleteAddressBook" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteAddressBook where
        toJSON DeleteAddressBook'{..}
          = object
              (catMaybes
                 [Just ("AddressBookArn" .= _dabAddressBookARN)])

instance ToPath DeleteAddressBook where
        toPath = const "/"

instance ToQuery DeleteAddressBook where
        toQuery = const mempty

-- | /See:/ 'deleteAddressBookResponse' smart constructor.
newtype DeleteAddressBookResponse = DeleteAddressBookResponse'
  { _dabrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAddressBookResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dabrsResponseStatus' - -- | The response status code.
deleteAddressBookResponse
    :: Int -- ^ 'dabrsResponseStatus'
    -> DeleteAddressBookResponse
deleteAddressBookResponse pResponseStatus_ =
  DeleteAddressBookResponse' {_dabrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dabrsResponseStatus :: Lens' DeleteAddressBookResponse Int
dabrsResponseStatus = lens _dabrsResponseStatus (\ s a -> s{_dabrsResponseStatus = a})

instance NFData DeleteAddressBookResponse where

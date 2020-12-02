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
-- Module      : Network.AWS.AlexaBusiness.UpdateAddressBook
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates address book details by the address book ARN.
--
--
module Network.AWS.AlexaBusiness.UpdateAddressBook
    (
    -- * Creating a Request
      updateAddressBook
    , UpdateAddressBook
    -- * Request Lenses
    , uabName
    , uabDescription
    , uabAddressBookARN

    -- * Destructuring the Response
    , updateAddressBookResponse
    , UpdateAddressBookResponse
    -- * Response Lenses
    , uabrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateAddressBook' smart constructor.
data UpdateAddressBook = UpdateAddressBook'
  { _uabName           :: !(Maybe Text)
  , _uabDescription    :: !(Maybe Text)
  , _uabAddressBookARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAddressBook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uabName' - The updated name of the room.
--
-- * 'uabDescription' - The updated description of the room.
--
-- * 'uabAddressBookARN' - The ARN of the room to update.
updateAddressBook
    :: Text -- ^ 'uabAddressBookARN'
    -> UpdateAddressBook
updateAddressBook pAddressBookARN_ =
  UpdateAddressBook'
    { _uabName = Nothing
    , _uabDescription = Nothing
    , _uabAddressBookARN = pAddressBookARN_
    }


-- | The updated name of the room.
uabName :: Lens' UpdateAddressBook (Maybe Text)
uabName = lens _uabName (\ s a -> s{_uabName = a})

-- | The updated description of the room.
uabDescription :: Lens' UpdateAddressBook (Maybe Text)
uabDescription = lens _uabDescription (\ s a -> s{_uabDescription = a})

-- | The ARN of the room to update.
uabAddressBookARN :: Lens' UpdateAddressBook Text
uabAddressBookARN = lens _uabAddressBookARN (\ s a -> s{_uabAddressBookARN = a})

instance AWSRequest UpdateAddressBook where
        type Rs UpdateAddressBook = UpdateAddressBookResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateAddressBookResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateAddressBook where

instance NFData UpdateAddressBook where

instance ToHeaders UpdateAddressBook where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.UpdateAddressBook" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateAddressBook where
        toJSON UpdateAddressBook'{..}
          = object
              (catMaybes
                 [("Name" .=) <$> _uabName,
                  ("Description" .=) <$> _uabDescription,
                  Just ("AddressBookArn" .= _uabAddressBookARN)])

instance ToPath UpdateAddressBook where
        toPath = const "/"

instance ToQuery UpdateAddressBook where
        toQuery = const mempty

-- | /See:/ 'updateAddressBookResponse' smart constructor.
newtype UpdateAddressBookResponse = UpdateAddressBookResponse'
  { _uabrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAddressBookResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uabrsResponseStatus' - -- | The response status code.
updateAddressBookResponse
    :: Int -- ^ 'uabrsResponseStatus'
    -> UpdateAddressBookResponse
updateAddressBookResponse pResponseStatus_ =
  UpdateAddressBookResponse' {_uabrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uabrsResponseStatus :: Lens' UpdateAddressBookResponse Int
uabrsResponseStatus = lens _uabrsResponseStatus (\ s a -> s{_uabrsResponseStatus = a})

instance NFData UpdateAddressBookResponse where

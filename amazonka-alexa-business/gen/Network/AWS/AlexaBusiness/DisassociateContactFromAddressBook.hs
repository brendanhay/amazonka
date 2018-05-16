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
-- Module      : Network.AWS.AlexaBusiness.DisassociateContactFromAddressBook
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a contact from a given address book.
--
--
module Network.AWS.AlexaBusiness.DisassociateContactFromAddressBook
    (
    -- * Creating a Request
      disassociateContactFromAddressBook
    , DisassociateContactFromAddressBook
    -- * Request Lenses
    , dcfabContactARN
    , dcfabAddressBookARN

    -- * Destructuring the Response
    , disassociateContactFromAddressBookResponse
    , DisassociateContactFromAddressBookResponse
    -- * Response Lenses
    , dcfabrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateContactFromAddressBook' smart constructor.
data DisassociateContactFromAddressBook = DisassociateContactFromAddressBook'
  { _dcfabContactARN     :: !Text
  , _dcfabAddressBookARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateContactFromAddressBook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcfabContactARN' - The ARN of the contact to disassociate from an address book.
--
-- * 'dcfabAddressBookARN' - The ARN of the address from which to disassociate the contact.
disassociateContactFromAddressBook
    :: Text -- ^ 'dcfabContactARN'
    -> Text -- ^ 'dcfabAddressBookARN'
    -> DisassociateContactFromAddressBook
disassociateContactFromAddressBook pContactARN_ pAddressBookARN_ =
  DisassociateContactFromAddressBook'
    {_dcfabContactARN = pContactARN_, _dcfabAddressBookARN = pAddressBookARN_}


-- | The ARN of the contact to disassociate from an address book.
dcfabContactARN :: Lens' DisassociateContactFromAddressBook Text
dcfabContactARN = lens _dcfabContactARN (\ s a -> s{_dcfabContactARN = a})

-- | The ARN of the address from which to disassociate the contact.
dcfabAddressBookARN :: Lens' DisassociateContactFromAddressBook Text
dcfabAddressBookARN = lens _dcfabAddressBookARN (\ s a -> s{_dcfabAddressBookARN = a})

instance AWSRequest
           DisassociateContactFromAddressBook
         where
        type Rs DisassociateContactFromAddressBook =
             DisassociateContactFromAddressBookResponse
        request = postJSON alexaBusiness
        response
          = receiveEmpty
              (\ s h x ->
                 DisassociateContactFromAddressBookResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DisassociateContactFromAddressBook
         where

instance NFData DisassociateContactFromAddressBook
         where

instance ToHeaders DisassociateContactFromAddressBook
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.DisassociateContactFromAddressBook"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateContactFromAddressBook
         where
        toJSON DisassociateContactFromAddressBook'{..}
          = object
              (catMaybes
                 [Just ("ContactArn" .= _dcfabContactARN),
                  Just ("AddressBookArn" .= _dcfabAddressBookARN)])

instance ToPath DisassociateContactFromAddressBook
         where
        toPath = const "/"

instance ToQuery DisassociateContactFromAddressBook
         where
        toQuery = const mempty

-- | /See:/ 'disassociateContactFromAddressBookResponse' smart constructor.
newtype DisassociateContactFromAddressBookResponse = DisassociateContactFromAddressBookResponse'
  { _dcfabrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateContactFromAddressBookResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcfabrsResponseStatus' - -- | The response status code.
disassociateContactFromAddressBookResponse
    :: Int -- ^ 'dcfabrsResponseStatus'
    -> DisassociateContactFromAddressBookResponse
disassociateContactFromAddressBookResponse pResponseStatus_ =
  DisassociateContactFromAddressBookResponse'
    {_dcfabrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dcfabrsResponseStatus :: Lens' DisassociateContactFromAddressBookResponse Int
dcfabrsResponseStatus = lens _dcfabrsResponseStatus (\ s a -> s{_dcfabrsResponseStatus = a})

instance NFData
           DisassociateContactFromAddressBookResponse
         where

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
-- Module      : Network.AWS.SSM.PutInventory
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Bulk update custom inventory items on one more instance. The request adds an inventory item, if it doesn't already exist, or updates an inventory item, if it does exist.
--
--
module Network.AWS.SSM.PutInventory
    (
    -- * Creating a Request
      putInventory
    , PutInventory
    -- * Request Lenses
    , piInstanceId
    , piItems

    -- * Destructuring the Response
    , putInventoryResponse
    , PutInventoryResponse
    -- * Response Lenses
    , pirsMessage
    , pirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'putInventory' smart constructor.
data PutInventory = PutInventory'
  { _piInstanceId :: !Text
  , _piItems      :: !(List1 InventoryItem)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutInventory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piInstanceId' - One or more instance IDs where you want to add or update inventory items.
--
-- * 'piItems' - The inventory items that you want to add or update on instances.
putInventory
    :: Text -- ^ 'piInstanceId'
    -> NonEmpty InventoryItem -- ^ 'piItems'
    -> PutInventory
putInventory pInstanceId_ pItems_ =
  PutInventory' {_piInstanceId = pInstanceId_, _piItems = _List1 # pItems_}


-- | One or more instance IDs where you want to add or update inventory items.
piInstanceId :: Lens' PutInventory Text
piInstanceId = lens _piInstanceId (\ s a -> s{_piInstanceId = a})

-- | The inventory items that you want to add or update on instances.
piItems :: Lens' PutInventory (NonEmpty InventoryItem)
piItems = lens _piItems (\ s a -> s{_piItems = a}) . _List1

instance AWSRequest PutInventory where
        type Rs PutInventory = PutInventoryResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 PutInventoryResponse' <$>
                   (x .?> "Message") <*> (pure (fromEnum s)))

instance Hashable PutInventory where

instance NFData PutInventory where

instance ToHeaders PutInventory where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.PutInventory" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutInventory where
        toJSON PutInventory'{..}
          = object
              (catMaybes
                 [Just ("InstanceId" .= _piInstanceId),
                  Just ("Items" .= _piItems)])

instance ToPath PutInventory where
        toPath = const "/"

instance ToQuery PutInventory where
        toQuery = const mempty

-- | /See:/ 'putInventoryResponse' smart constructor.
data PutInventoryResponse = PutInventoryResponse'
  { _pirsMessage        :: !(Maybe Text)
  , _pirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutInventoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pirsMessage' - Information about the request.
--
-- * 'pirsResponseStatus' - -- | The response status code.
putInventoryResponse
    :: Int -- ^ 'pirsResponseStatus'
    -> PutInventoryResponse
putInventoryResponse pResponseStatus_ =
  PutInventoryResponse'
    {_pirsMessage = Nothing, _pirsResponseStatus = pResponseStatus_}


-- | Information about the request.
pirsMessage :: Lens' PutInventoryResponse (Maybe Text)
pirsMessage = lens _pirsMessage (\ s a -> s{_pirsMessage = a})

-- | -- | The response status code.
pirsResponseStatus :: Lens' PutInventoryResponse Int
pirsResponseStatus = lens _pirsResponseStatus (\ s a -> s{_pirsResponseStatus = a})

instance NFData PutInventoryResponse where

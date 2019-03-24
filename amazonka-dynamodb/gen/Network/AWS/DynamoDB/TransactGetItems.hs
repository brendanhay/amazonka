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
-- Module      : Network.AWS.DynamoDB.TransactGetItems
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @TransactGetItems@ is a synchronous operation that atomically retrieves multiple items from one or more tables (but not from indexes) in a single account and region. A @TransactGetItems@ call can contain up to 10 @TransactGetItem@ objects, each of which contains a @Get@ structure that specifies an item to retrieve from a table in the account and region. A call to @TransactGetItems@ cannot retrieve items from tables in more than one AWS account or region.
--
--
-- DynamoDB rejects the entire @TransactGetItems@ request if any of the following is true:
--
--     * A conflicting operation is in the process of updating an item to be read.
--
--     * There is insufficient provisioned capacity for the transaction to be completed.
--
--     * There is a user error, such as an invalid data format.
--
--
--
module Network.AWS.DynamoDB.TransactGetItems
    (
    -- * Creating a Request
      transactGetItems
    , TransactGetItems
    -- * Request Lenses
    , tgiReturnConsumedCapacity
    , tgiTransactItems

    -- * Destructuring the Response
    , transactGetItemsResponse
    , TransactGetItemsResponse
    -- * Response Lenses
    , tgirsResponses
    , tgirsConsumedCapacity
    , tgirsResponseStatus
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'transactGetItems' smart constructor.
data TransactGetItems = TransactGetItems'
  { _tgiReturnConsumedCapacity :: !(Maybe ReturnConsumedCapacity)
  , _tgiTransactItems          :: !(List1 TransactGetItem)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TransactGetItems' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgiReturnConsumedCapacity' - A value of @TOTAL@ causes consumed capacity information to be returned, and a value of @NONE@ prevents that information from being returned. No other value is valid.
--
-- * 'tgiTransactItems' - An ordered array of up to 10 @TransactGetItem@ objects, each of which contains a @Get@ structure.
transactGetItems
    :: NonEmpty TransactGetItem -- ^ 'tgiTransactItems'
    -> TransactGetItems
transactGetItems pTransactItems_ =
  TransactGetItems'
    { _tgiReturnConsumedCapacity = Nothing
    , _tgiTransactItems = _List1 # pTransactItems_
    }


-- | A value of @TOTAL@ causes consumed capacity information to be returned, and a value of @NONE@ prevents that information from being returned. No other value is valid.
tgiReturnConsumedCapacity :: Lens' TransactGetItems (Maybe ReturnConsumedCapacity)
tgiReturnConsumedCapacity = lens _tgiReturnConsumedCapacity (\ s a -> s{_tgiReturnConsumedCapacity = a})

-- | An ordered array of up to 10 @TransactGetItem@ objects, each of which contains a @Get@ structure.
tgiTransactItems :: Lens' TransactGetItems (NonEmpty TransactGetItem)
tgiTransactItems = lens _tgiTransactItems (\ s a -> s{_tgiTransactItems = a}) . _List1

instance AWSRequest TransactGetItems where
        type Rs TransactGetItems = TransactGetItemsResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 TransactGetItemsResponse' <$>
                   (x .?> "Responses") <*>
                     (x .?> "ConsumedCapacity" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable TransactGetItems where

instance NFData TransactGetItems where

instance ToHeaders TransactGetItems where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.TransactGetItems" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON TransactGetItems where
        toJSON TransactGetItems'{..}
          = object
              (catMaybes
                 [("ReturnConsumedCapacity" .=) <$>
                    _tgiReturnConsumedCapacity,
                  Just ("TransactItems" .= _tgiTransactItems)])

instance ToPath TransactGetItems where
        toPath = const "/"

instance ToQuery TransactGetItems where
        toQuery = const mempty

-- | /See:/ 'transactGetItemsResponse' smart constructor.
data TransactGetItemsResponse = TransactGetItemsResponse'
  { _tgirsResponses        :: !(Maybe (List1 ItemResponse))
  , _tgirsConsumedCapacity :: !(Maybe [ConsumedCapacity])
  , _tgirsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TransactGetItemsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgirsResponses' - An ordered array of up to 10 @ItemResponse@ objects, each of which corresponds to the @TransactGetItem@ object in the same position in the /TransactItems/ array. Each @ItemResponse@ object contains a Map of the name-value pairs that are the projected attributes of the requested item. If a requested item could not be retrieved, the corresponding @ItemResponse@ object is Null, or if the requested item has no projected attributes, the corresponding @ItemResponse@ object is an empty Map.
--
-- * 'tgirsConsumedCapacity' - If the /ReturnConsumedCapacity/ value was @TOTAL@ , this is an array of @ConsumedCapacity@ objects, one for each table addressed by @TransactGetItem@ objects in the /TransactItems/ parameter. These @ConsumedCapacity@ objects report the read-capacity units consumed by the @TransactGetItems@ call in that table.
--
-- * 'tgirsResponseStatus' - -- | The response status code.
transactGetItemsResponse
    :: Int -- ^ 'tgirsResponseStatus'
    -> TransactGetItemsResponse
transactGetItemsResponse pResponseStatus_ =
  TransactGetItemsResponse'
    { _tgirsResponses = Nothing
    , _tgirsConsumedCapacity = Nothing
    , _tgirsResponseStatus = pResponseStatus_
    }


-- | An ordered array of up to 10 @ItemResponse@ objects, each of which corresponds to the @TransactGetItem@ object in the same position in the /TransactItems/ array. Each @ItemResponse@ object contains a Map of the name-value pairs that are the projected attributes of the requested item. If a requested item could not be retrieved, the corresponding @ItemResponse@ object is Null, or if the requested item has no projected attributes, the corresponding @ItemResponse@ object is an empty Map.
tgirsResponses :: Lens' TransactGetItemsResponse (Maybe (NonEmpty ItemResponse))
tgirsResponses = lens _tgirsResponses (\ s a -> s{_tgirsResponses = a}) . mapping _List1

-- | If the /ReturnConsumedCapacity/ value was @TOTAL@ , this is an array of @ConsumedCapacity@ objects, one for each table addressed by @TransactGetItem@ objects in the /TransactItems/ parameter. These @ConsumedCapacity@ objects report the read-capacity units consumed by the @TransactGetItems@ call in that table.
tgirsConsumedCapacity :: Lens' TransactGetItemsResponse [ConsumedCapacity]
tgirsConsumedCapacity = lens _tgirsConsumedCapacity (\ s a -> s{_tgirsConsumedCapacity = a}) . _Default . _Coerce

-- | -- | The response status code.
tgirsResponseStatus :: Lens' TransactGetItemsResponse Int
tgirsResponseStatus = lens _tgirsResponseStatus (\ s a -> s{_tgirsResponseStatus = a})

instance NFData TransactGetItemsResponse where

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.TransactWriteItems
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @TransactWriteItems@ is a synchronous write operation that groups up to 25 action requests. These actions can target items in different tables, but not in different AWS accounts or Regions, and no two actions can target the same item. For example, you cannot both @ConditionCheck@ and @Update@ the same item. The aggregate size of the items in the transaction cannot exceed 4 MB.
--
--
-- The actions are completed atomically so that either all of them succeed, or all of them fail. They are defined by the following objects:
--
--     * @Put@    Initiates a @PutItem@ operation to write a new item. This structure specifies the primary key of the item to be written, the name of the table to write it in, an optional condition expression that must be satisfied for the write to succeed, a list of the item's attributes, and a field indicating whether to retrieve the item's attributes if the condition is not met.
--
--     * @Update@    Initiates an @UpdateItem@ operation to update an existing item. This structure specifies the primary key of the item to be updated, the name of the table where it resides, an optional condition expression that must be satisfied for the update to succeed, an expression that defines one or more attributes to be updated, and a field indicating whether to retrieve the item's attributes if the condition is not met.
--
--     * @Delete@    Initiates a @DeleteItem@ operation to delete an existing item. This structure specifies the primary key of the item to be deleted, the name of the table where it resides, an optional condition expression that must be satisfied for the deletion to succeed, and a field indicating whether to retrieve the item's attributes if the condition is not met.
--
--     * @ConditionCheck@    Applies a condition to an item that is not being modified by the transaction. This structure specifies the primary key of the item to be checked, the name of the table where it resides, a condition expression that must be satisfied for the transaction to succeed, and a field indicating whether to retrieve the item's attributes if the condition is not met.
--
--
--
-- DynamoDB rejects the entire @TransactWriteItems@ request if any of the following is true:
--
--     * A condition in one of the condition expressions is not met.
--
--     * An ongoing operation is in the process of updating the same item.
--
--     * There is insufficient provisioned capacity for the transaction to be completed.
--
--     * An item size becomes too large (bigger than 400 KB), a local secondary index (LSI) becomes too large, or a similar validation error occurs because of changes made by the transaction.
--
--     * The aggregate size of the items in the transaction exceeds 4 MB.
--
--     * There is a user error, such as an invalid data format.
module Network.AWS.DynamoDB.TransactWriteItems
  ( -- * Creating a Request
    transactWriteItems,
    TransactWriteItems,

    -- * Request Lenses
    twiReturnConsumedCapacity,
    twiReturnItemCollectionMetrics,
    twiClientRequestToken,
    twiTransactItems,

    -- * Destructuring the Response
    transactWriteItemsResponse,
    TransactWriteItemsResponse,

    -- * Response Lenses
    twirsItemCollectionMetrics,
    twirsConsumedCapacity,
    twirsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'transactWriteItems' smart constructor.
data TransactWriteItems = TransactWriteItems'
  { _twiReturnConsumedCapacity ::
      !(Maybe ReturnConsumedCapacity),
    _twiReturnItemCollectionMetrics ::
      !(Maybe ReturnItemCollectionMetrics),
    _twiClientRequestToken :: !(Maybe Text),
    _twiTransactItems :: !(List1 TransactWriteItem)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransactWriteItems' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'twiReturnConsumedCapacity' - Undocumented member.
--
-- * 'twiReturnItemCollectionMetrics' - Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections (if any), that were modified during the operation and are returned in the response. If set to @NONE@ (the default), no statistics are returned.
--
-- * 'twiClientRequestToken' - Providing a @ClientRequestToken@ makes the call to @TransactWriteItems@ idempotent, meaning that multiple identical calls have the same effect as one single call. Although multiple identical calls using the same client request token produce the same result on the server (no side effects), the responses to the calls might not be the same. If the @ReturnConsumedCapacity>@ parameter is set, then the initial @TransactWriteItems@ call returns the amount of write capacity units consumed in making the changes. Subsequent @TransactWriteItems@ calls with the same client token return the number of read capacity units consumed in reading the item. A client request token is valid for 10 minutes after the first request that uses it is completed. After 10 minutes, any request with the same client token is treated as a new request. Do not resubmit the same request with the same client token for more than 10 minutes, or the result might not be idempotent. If you submit a request with the same client token but a change in other parameters within the 10-minute idempotency window, DynamoDB returns an @IdempotentParameterMismatch@ exception.
--
-- * 'twiTransactItems' - An ordered array of up to 25 @TransactWriteItem@ objects, each of which contains a @ConditionCheck@ , @Put@ , @Update@ , or @Delete@ object. These can operate on items in different tables, but the tables must reside in the same AWS account and Region, and no two of them can operate on the same item.
transactWriteItems ::
  -- | 'twiTransactItems'
  NonEmpty TransactWriteItem ->
  TransactWriteItems
transactWriteItems pTransactItems_ =
  TransactWriteItems'
    { _twiReturnConsumedCapacity = Nothing,
      _twiReturnItemCollectionMetrics = Nothing,
      _twiClientRequestToken = Nothing,
      _twiTransactItems = _List1 # pTransactItems_
    }

-- | Undocumented member.
twiReturnConsumedCapacity :: Lens' TransactWriteItems (Maybe ReturnConsumedCapacity)
twiReturnConsumedCapacity = lens _twiReturnConsumedCapacity (\s a -> s {_twiReturnConsumedCapacity = a})

-- | Determines whether item collection metrics are returned. If set to @SIZE@ , the response includes statistics about item collections (if any), that were modified during the operation and are returned in the response. If set to @NONE@ (the default), no statistics are returned.
twiReturnItemCollectionMetrics :: Lens' TransactWriteItems (Maybe ReturnItemCollectionMetrics)
twiReturnItemCollectionMetrics = lens _twiReturnItemCollectionMetrics (\s a -> s {_twiReturnItemCollectionMetrics = a})

-- | Providing a @ClientRequestToken@ makes the call to @TransactWriteItems@ idempotent, meaning that multiple identical calls have the same effect as one single call. Although multiple identical calls using the same client request token produce the same result on the server (no side effects), the responses to the calls might not be the same. If the @ReturnConsumedCapacity>@ parameter is set, then the initial @TransactWriteItems@ call returns the amount of write capacity units consumed in making the changes. Subsequent @TransactWriteItems@ calls with the same client token return the number of read capacity units consumed in reading the item. A client request token is valid for 10 minutes after the first request that uses it is completed. After 10 minutes, any request with the same client token is treated as a new request. Do not resubmit the same request with the same client token for more than 10 minutes, or the result might not be idempotent. If you submit a request with the same client token but a change in other parameters within the 10-minute idempotency window, DynamoDB returns an @IdempotentParameterMismatch@ exception.
twiClientRequestToken :: Lens' TransactWriteItems (Maybe Text)
twiClientRequestToken = lens _twiClientRequestToken (\s a -> s {_twiClientRequestToken = a})

-- | An ordered array of up to 25 @TransactWriteItem@ objects, each of which contains a @ConditionCheck@ , @Put@ , @Update@ , or @Delete@ object. These can operate on items in different tables, but the tables must reside in the same AWS account and Region, and no two of them can operate on the same item.
twiTransactItems :: Lens' TransactWriteItems (NonEmpty TransactWriteItem)
twiTransactItems = lens _twiTransactItems (\s a -> s {_twiTransactItems = a}) . _List1

instance AWSRequest TransactWriteItems where
  type Rs TransactWriteItems = TransactWriteItemsResponse
  request = postJSON dynamoDB
  response =
    receiveJSON
      ( \s h x ->
          TransactWriteItemsResponse'
            <$> (x .?> "ItemCollectionMetrics" .!@ mempty)
            <*> (x .?> "ConsumedCapacity" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable TransactWriteItems

instance NFData TransactWriteItems

instance ToHeaders TransactWriteItems where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DynamoDB_20120810.TransactWriteItems" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
          ]
      )

instance ToJSON TransactWriteItems where
  toJSON TransactWriteItems' {..} =
    object
      ( catMaybes
          [ ("ReturnConsumedCapacity" .=) <$> _twiReturnConsumedCapacity,
            ("ReturnItemCollectionMetrics" .=)
              <$> _twiReturnItemCollectionMetrics,
            ("ClientRequestToken" .=) <$> _twiClientRequestToken,
            Just ("TransactItems" .= _twiTransactItems)
          ]
      )

instance ToPath TransactWriteItems where
  toPath = const "/"

instance ToQuery TransactWriteItems where
  toQuery = const mempty

-- | /See:/ 'transactWriteItemsResponse' smart constructor.
data TransactWriteItemsResponse = TransactWriteItemsResponse'
  { _twirsItemCollectionMetrics ::
      !( Maybe
           ( Map
               Text
               ([ItemCollectionMetrics])
           )
       ),
    _twirsConsumedCapacity ::
      !(Maybe [ConsumedCapacity]),
    _twirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransactWriteItemsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'twirsItemCollectionMetrics' - A list of tables that were processed by @TransactWriteItems@ and, for each table, information about any item collections that were affected by individual @UpdateItem@ , @PutItem@ , or @DeleteItem@ operations.
--
-- * 'twirsConsumedCapacity' - The capacity units consumed by the entire @TransactWriteItems@ operation. The values of the list are ordered according to the ordering of the @TransactItems@ request parameter.
--
-- * 'twirsResponseStatus' - -- | The response status code.
transactWriteItemsResponse ::
  -- | 'twirsResponseStatus'
  Int ->
  TransactWriteItemsResponse
transactWriteItemsResponse pResponseStatus_ =
  TransactWriteItemsResponse'
    { _twirsItemCollectionMetrics =
        Nothing,
      _twirsConsumedCapacity = Nothing,
      _twirsResponseStatus = pResponseStatus_
    }

-- | A list of tables that were processed by @TransactWriteItems@ and, for each table, information about any item collections that were affected by individual @UpdateItem@ , @PutItem@ , or @DeleteItem@ operations.
twirsItemCollectionMetrics :: Lens' TransactWriteItemsResponse (HashMap Text ([ItemCollectionMetrics]))
twirsItemCollectionMetrics = lens _twirsItemCollectionMetrics (\s a -> s {_twirsItemCollectionMetrics = a}) . _Default . _Map

-- | The capacity units consumed by the entire @TransactWriteItems@ operation. The values of the list are ordered according to the ordering of the @TransactItems@ request parameter.
twirsConsumedCapacity :: Lens' TransactWriteItemsResponse [ConsumedCapacity]
twirsConsumedCapacity = lens _twirsConsumedCapacity (\s a -> s {_twirsConsumedCapacity = a}) . _Default . _Coerce

-- | -- | The response status code.
twirsResponseStatus :: Lens' TransactWriteItemsResponse Int
twirsResponseStatus = lens _twirsResponseStatus (\s a -> s {_twirsResponseStatus = a})

instance NFData TransactWriteItemsResponse

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
-- Module      : Network.AWS.DynamoDB.UpdateTable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the provisioned throughput settings, global secondary indexes, or DynamoDB Streams settings for a given table.
--
--
-- You can only perform one of the following operations at once:
--
--     * Modify the provisioned throughput settings of the table.
--
--     * Enable or disable Streams on the table.
--
--     * Remove a global secondary index from the table.
--
--     * Create a new global secondary index on the table. Once the index begins backfilling, you can use @UpdateTable@ to perform other operations.
--
--
--
-- @UpdateTable@ is an asynchronous operation; while it is executing, the table status changes from @ACTIVE@ to @UPDATING@ . While it is @UPDATING@ , you cannot issue another @UpdateTable@ request. When the table returns to the @ACTIVE@ state, the @UpdateTable@ operation is complete.
--
module Network.AWS.DynamoDB.UpdateTable
    (
    -- * Creating a Request
      updateTable
    , UpdateTable
    -- * Request Lenses
    , utAttributeDefinitions
    , utProvisionedThroughput
    , utGlobalSecondaryIndexUpdates
    , utStreamSpecification
    , utTableName

    -- * Destructuring the Response
    , updateTableResponse
    , UpdateTableResponse
    -- * Response Lenses
    , utrsTableDescription
    , utrsResponseStatus
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of an @UpdateTable@ operation.
--
--
--
-- /See:/ 'updateTable' smart constructor.
data UpdateTable = UpdateTable'
  { _utAttributeDefinitions        :: !(Maybe [AttributeDefinition])
  , _utProvisionedThroughput       :: !(Maybe ProvisionedThroughput)
  , _utGlobalSecondaryIndexUpdates :: !(Maybe [GlobalSecondaryIndexUpdate])
  , _utStreamSpecification         :: !(Maybe StreamSpecification)
  , _utTableName                   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utAttributeDefinitions' - An array of attributes that describe the key schema for the table and indexes. If you are adding a new global secondary index to the table, @AttributeDefinitions@ must include the key element(s) of the new index.
--
-- * 'utProvisionedThroughput' - The new provisioned throughput settings for the specified table or index.
--
-- * 'utGlobalSecondaryIndexUpdates' - An array of one or more global secondary indexes for the table. For each index in the array, you can request one action:     * @Create@ - add a new global secondary index to the table.     * @Update@ - modify the provisioned throughput settings of an existing global secondary index.     * @Delete@ - remove a global secondary index from the table. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GSI.OnlineOps.html Managing Global Secondary Indexes> in the /Amazon DynamoDB Developer Guide/ .
--
-- * 'utStreamSpecification' - Represents the DynamoDB Streams configuration for the table.
--
-- * 'utTableName' - The name of the table to be updated.
updateTable
    :: Text -- ^ 'utTableName'
    -> UpdateTable
updateTable pTableName_ =
  UpdateTable'
    { _utAttributeDefinitions = Nothing
    , _utProvisionedThroughput = Nothing
    , _utGlobalSecondaryIndexUpdates = Nothing
    , _utStreamSpecification = Nothing
    , _utTableName = pTableName_
    }


-- | An array of attributes that describe the key schema for the table and indexes. If you are adding a new global secondary index to the table, @AttributeDefinitions@ must include the key element(s) of the new index.
utAttributeDefinitions :: Lens' UpdateTable [AttributeDefinition]
utAttributeDefinitions = lens _utAttributeDefinitions (\ s a -> s{_utAttributeDefinitions = a}) . _Default . _Coerce

-- | The new provisioned throughput settings for the specified table or index.
utProvisionedThroughput :: Lens' UpdateTable (Maybe ProvisionedThroughput)
utProvisionedThroughput = lens _utProvisionedThroughput (\ s a -> s{_utProvisionedThroughput = a})

-- | An array of one or more global secondary indexes for the table. For each index in the array, you can request one action:     * @Create@ - add a new global secondary index to the table.     * @Update@ - modify the provisioned throughput settings of an existing global secondary index.     * @Delete@ - remove a global secondary index from the table. For more information, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GSI.OnlineOps.html Managing Global Secondary Indexes> in the /Amazon DynamoDB Developer Guide/ .
utGlobalSecondaryIndexUpdates :: Lens' UpdateTable [GlobalSecondaryIndexUpdate]
utGlobalSecondaryIndexUpdates = lens _utGlobalSecondaryIndexUpdates (\ s a -> s{_utGlobalSecondaryIndexUpdates = a}) . _Default . _Coerce

-- | Represents the DynamoDB Streams configuration for the table.
utStreamSpecification :: Lens' UpdateTable (Maybe StreamSpecification)
utStreamSpecification = lens _utStreamSpecification (\ s a -> s{_utStreamSpecification = a})

-- | The name of the table to be updated.
utTableName :: Lens' UpdateTable Text
utTableName = lens _utTableName (\ s a -> s{_utTableName = a})

instance AWSRequest UpdateTable where
        type Rs UpdateTable = UpdateTableResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 UpdateTableResponse' <$>
                   (x .?> "TableDescription") <*> (pure (fromEnum s)))

instance Hashable UpdateTable where

instance NFData UpdateTable where

instance ToHeaders UpdateTable where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.UpdateTable" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON UpdateTable where
        toJSON UpdateTable'{..}
          = object
              (catMaybes
                 [("AttributeDefinitions" .=) <$>
                    _utAttributeDefinitions,
                  ("ProvisionedThroughput" .=) <$>
                    _utProvisionedThroughput,
                  ("GlobalSecondaryIndexUpdates" .=) <$>
                    _utGlobalSecondaryIndexUpdates,
                  ("StreamSpecification" .=) <$>
                    _utStreamSpecification,
                  Just ("TableName" .= _utTableName)])

instance ToPath UpdateTable where
        toPath = const "/"

instance ToQuery UpdateTable where
        toQuery = const mempty

-- | Represents the output of an @UpdateTable@ operation.
--
--
--
-- /See:/ 'updateTableResponse' smart constructor.
data UpdateTableResponse = UpdateTableResponse'
  { _utrsTableDescription :: !(Maybe TableDescription)
  , _utrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateTableResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utrsTableDescription' - Represents the properties of the table.
--
-- * 'utrsResponseStatus' - -- | The response status code.
updateTableResponse
    :: Int -- ^ 'utrsResponseStatus'
    -> UpdateTableResponse
updateTableResponse pResponseStatus_ =
  UpdateTableResponse'
    {_utrsTableDescription = Nothing, _utrsResponseStatus = pResponseStatus_}


-- | Represents the properties of the table.
utrsTableDescription :: Lens' UpdateTableResponse (Maybe TableDescription)
utrsTableDescription = lens _utrsTableDescription (\ s a -> s{_utrsTableDescription = a})

-- | -- | The response status code.
utrsResponseStatus :: Lens' UpdateTableResponse Int
utrsResponseStatus = lens _utrsResponseStatus (\ s a -> s{_utrsResponseStatus = a})

instance NFData UpdateTableResponse where

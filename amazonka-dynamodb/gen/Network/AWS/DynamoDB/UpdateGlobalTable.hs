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
-- Module      : Network.AWS.DynamoDB.UpdateGlobalTable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or removes replicas in the specified global table. The global table must already exist to be able to use this operation. Any replica to be added must be empty, must have the same name as the global table, must have the same key schema, and must have DynamoDB Streams enabled and must have same provisioned and maximum write capacity units.
--
--
-- If global secondary indexes are specified, then the following conditions must also be met:
--
--     * The global secondary indexes must have the same name.
--
--     * The global secondary indexes must have the same hash key and sort key (if present).
--
--     * The global secondary indexes must have the same provisioned and maximum write capacity units.
--
--
--
module Network.AWS.DynamoDB.UpdateGlobalTable
    (
    -- * Creating a Request
      updateGlobalTable
    , UpdateGlobalTable
    -- * Request Lenses
    , ugtGlobalTableName
    , ugtReplicaUpdates

    -- * Destructuring the Response
    , updateGlobalTableResponse
    , UpdateGlobalTableResponse
    -- * Response Lenses
    , ugtrsGlobalTableDescription
    , ugtrsResponseStatus
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateGlobalTable' smart constructor.
data UpdateGlobalTable = UpdateGlobalTable'
  { _ugtGlobalTableName :: !Text
  , _ugtReplicaUpdates  :: ![ReplicaUpdate]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGlobalTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugtGlobalTableName' - The global table name.
--
-- * 'ugtReplicaUpdates' - A list of regions that should be added or removed from the global table.
updateGlobalTable
    :: Text -- ^ 'ugtGlobalTableName'
    -> UpdateGlobalTable
updateGlobalTable pGlobalTableName_ =
  UpdateGlobalTable'
    {_ugtGlobalTableName = pGlobalTableName_, _ugtReplicaUpdates = mempty}


-- | The global table name.
ugtGlobalTableName :: Lens' UpdateGlobalTable Text
ugtGlobalTableName = lens _ugtGlobalTableName (\ s a -> s{_ugtGlobalTableName = a})

-- | A list of regions that should be added or removed from the global table.
ugtReplicaUpdates :: Lens' UpdateGlobalTable [ReplicaUpdate]
ugtReplicaUpdates = lens _ugtReplicaUpdates (\ s a -> s{_ugtReplicaUpdates = a}) . _Coerce

instance AWSRequest UpdateGlobalTable where
        type Rs UpdateGlobalTable = UpdateGlobalTableResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 UpdateGlobalTableResponse' <$>
                   (x .?> "GlobalTableDescription") <*>
                     (pure (fromEnum s)))

instance Hashable UpdateGlobalTable where

instance NFData UpdateGlobalTable where

instance ToHeaders UpdateGlobalTable where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.UpdateGlobalTable" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON UpdateGlobalTable where
        toJSON UpdateGlobalTable'{..}
          = object
              (catMaybes
                 [Just ("GlobalTableName" .= _ugtGlobalTableName),
                  Just ("ReplicaUpdates" .= _ugtReplicaUpdates)])

instance ToPath UpdateGlobalTable where
        toPath = const "/"

instance ToQuery UpdateGlobalTable where
        toQuery = const mempty

-- | /See:/ 'updateGlobalTableResponse' smart constructor.
data UpdateGlobalTableResponse = UpdateGlobalTableResponse'
  { _ugtrsGlobalTableDescription :: !(Maybe GlobalTableDescription)
  , _ugtrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateGlobalTableResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugtrsGlobalTableDescription' - Contains the details of the global table.
--
-- * 'ugtrsResponseStatus' - -- | The response status code.
updateGlobalTableResponse
    :: Int -- ^ 'ugtrsResponseStatus'
    -> UpdateGlobalTableResponse
updateGlobalTableResponse pResponseStatus_ =
  UpdateGlobalTableResponse'
    { _ugtrsGlobalTableDescription = Nothing
    , _ugtrsResponseStatus = pResponseStatus_
    }


-- | Contains the details of the global table.
ugtrsGlobalTableDescription :: Lens' UpdateGlobalTableResponse (Maybe GlobalTableDescription)
ugtrsGlobalTableDescription = lens _ugtrsGlobalTableDescription (\ s a -> s{_ugtrsGlobalTableDescription = a})

-- | -- | The response status code.
ugtrsResponseStatus :: Lens' UpdateGlobalTableResponse Int
ugtrsResponseStatus = lens _ugtrsResponseStatus (\ s a -> s{_ugtrsResponseStatus = a})

instance NFData UpdateGlobalTableResponse where

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
-- Module      : Network.AWS.DynamoDB.CreateGlobalTable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a global table from an existing table. A global table creates a replication relationship between two or more DynamoDB tables with the same table name in the provided regions.
--
--
-- Tables can only be added as the replicas of a global table group under the following conditions:
--
--     * The tables must have the same name.
--
--     * The tables must contain no items.
--
--     * The tables must have the same hash key and sort key (if present).
--
--     * The tables must have DynamoDB Streams enabled (NEW_AND_OLD_IMAGES).
--
--     * The tables must have same provisioned and maximum write capacity units.
--
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
module Network.AWS.DynamoDB.CreateGlobalTable
    (
    -- * Creating a Request
      createGlobalTable
    , CreateGlobalTable
    -- * Request Lenses
    , cgtGlobalTableName
    , cgtReplicationGroup

    -- * Destructuring the Response
    , createGlobalTableResponse
    , CreateGlobalTableResponse
    -- * Response Lenses
    , cgtrsGlobalTableDescription
    , cgtrsResponseStatus
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createGlobalTable' smart constructor.
data CreateGlobalTable = CreateGlobalTable'
  { _cgtGlobalTableName  :: !Text
  , _cgtReplicationGroup :: ![Replica]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGlobalTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgtGlobalTableName' - The global table name.
--
-- * 'cgtReplicationGroup' - The regions where the global table needs to be created.
createGlobalTable
    :: Text -- ^ 'cgtGlobalTableName'
    -> CreateGlobalTable
createGlobalTable pGlobalTableName_ =
  CreateGlobalTable'
    {_cgtGlobalTableName = pGlobalTableName_, _cgtReplicationGroup = mempty}


-- | The global table name.
cgtGlobalTableName :: Lens' CreateGlobalTable Text
cgtGlobalTableName = lens _cgtGlobalTableName (\ s a -> s{_cgtGlobalTableName = a})

-- | The regions where the global table needs to be created.
cgtReplicationGroup :: Lens' CreateGlobalTable [Replica]
cgtReplicationGroup = lens _cgtReplicationGroup (\ s a -> s{_cgtReplicationGroup = a}) . _Coerce

instance AWSRequest CreateGlobalTable where
        type Rs CreateGlobalTable = CreateGlobalTableResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 CreateGlobalTableResponse' <$>
                   (x .?> "GlobalTableDescription") <*>
                     (pure (fromEnum s)))

instance Hashable CreateGlobalTable where

instance NFData CreateGlobalTable where

instance ToHeaders CreateGlobalTable where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.CreateGlobalTable" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON CreateGlobalTable where
        toJSON CreateGlobalTable'{..}
          = object
              (catMaybes
                 [Just ("GlobalTableName" .= _cgtGlobalTableName),
                  Just ("ReplicationGroup" .= _cgtReplicationGroup)])

instance ToPath CreateGlobalTable where
        toPath = const "/"

instance ToQuery CreateGlobalTable where
        toQuery = const mempty

-- | /See:/ 'createGlobalTableResponse' smart constructor.
data CreateGlobalTableResponse = CreateGlobalTableResponse'
  { _cgtrsGlobalTableDescription :: !(Maybe GlobalTableDescription)
  , _cgtrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGlobalTableResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgtrsGlobalTableDescription' - Contains the details of the global table.
--
-- * 'cgtrsResponseStatus' - -- | The response status code.
createGlobalTableResponse
    :: Int -- ^ 'cgtrsResponseStatus'
    -> CreateGlobalTableResponse
createGlobalTableResponse pResponseStatus_ =
  CreateGlobalTableResponse'
    { _cgtrsGlobalTableDescription = Nothing
    , _cgtrsResponseStatus = pResponseStatus_
    }


-- | Contains the details of the global table.
cgtrsGlobalTableDescription :: Lens' CreateGlobalTableResponse (Maybe GlobalTableDescription)
cgtrsGlobalTableDescription = lens _cgtrsGlobalTableDescription (\ s a -> s{_cgtrsGlobalTableDescription = a})

-- | -- | The response status code.
cgtrsResponseStatus :: Lens' CreateGlobalTableResponse Int
cgtrsResponseStatus = lens _cgtrsResponseStatus (\ s a -> s{_cgtrsResponseStatus = a})

instance NFData CreateGlobalTableResponse where

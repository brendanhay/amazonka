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
-- Module      : Network.AWS.ElastiCache.CreateSnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a copy of an entire cluster or replication group at a specific moment in time.
--
--
module Network.AWS.ElastiCache.CreateSnapshot
    (
    -- * Creating a Request
      createSnapshot
    , CreateSnapshot
    -- * Request Lenses
    , csCacheClusterId
    , csReplicationGroupId
    , csSnapshotName

    -- * Destructuring the Response
    , createSnapshotResponse
    , CreateSnapshotResponse
    -- * Response Lenses
    , crersSnapshot
    , crersResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @CreateSnapshot@ operation.
--
--
--
-- /See:/ 'createSnapshot' smart constructor.
data CreateSnapshot = CreateSnapshot'
  { _csCacheClusterId     :: !(Maybe Text)
  , _csReplicationGroupId :: !(Maybe Text)
  , _csSnapshotName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csCacheClusterId' - The identifier of an existing cluster. The snapshot is created from this cluster.
--
-- * 'csReplicationGroupId' - The identifier of an existing replication group. The snapshot is created from this replication group.
--
-- * 'csSnapshotName' - A name for the snapshot being created.
createSnapshot
    :: Text -- ^ 'csSnapshotName'
    -> CreateSnapshot
createSnapshot pSnapshotName_ =
  CreateSnapshot'
    { _csCacheClusterId = Nothing
    , _csReplicationGroupId = Nothing
    , _csSnapshotName = pSnapshotName_
    }


-- | The identifier of an existing cluster. The snapshot is created from this cluster.
csCacheClusterId :: Lens' CreateSnapshot (Maybe Text)
csCacheClusterId = lens _csCacheClusterId (\ s a -> s{_csCacheClusterId = a})

-- | The identifier of an existing replication group. The snapshot is created from this replication group.
csReplicationGroupId :: Lens' CreateSnapshot (Maybe Text)
csReplicationGroupId = lens _csReplicationGroupId (\ s a -> s{_csReplicationGroupId = a})

-- | A name for the snapshot being created.
csSnapshotName :: Lens' CreateSnapshot Text
csSnapshotName = lens _csSnapshotName (\ s a -> s{_csSnapshotName = a})

instance AWSRequest CreateSnapshot where
        type Rs CreateSnapshot = CreateSnapshotResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "CreateSnapshotResult"
              (\ s h x ->
                 CreateSnapshotResponse' <$>
                   (x .@? "Snapshot") <*> (pure (fromEnum s)))

instance Hashable CreateSnapshot where

instance NFData CreateSnapshot where

instance ToHeaders CreateSnapshot where
        toHeaders = const mempty

instance ToPath CreateSnapshot where
        toPath = const "/"

instance ToQuery CreateSnapshot where
        toQuery CreateSnapshot'{..}
          = mconcat
              ["Action" =: ("CreateSnapshot" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheClusterId" =: _csCacheClusterId,
               "ReplicationGroupId" =: _csReplicationGroupId,
               "SnapshotName" =: _csSnapshotName]

-- | /See:/ 'createSnapshotResponse' smart constructor.
data CreateSnapshotResponse = CreateSnapshotResponse'
  { _crersSnapshot       :: !(Maybe Snapshot)
  , _crersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crersSnapshot' - Undocumented member.
--
-- * 'crersResponseStatus' - -- | The response status code.
createSnapshotResponse
    :: Int -- ^ 'crersResponseStatus'
    -> CreateSnapshotResponse
createSnapshotResponse pResponseStatus_ =
  CreateSnapshotResponse'
    {_crersSnapshot = Nothing, _crersResponseStatus = pResponseStatus_}


-- | Undocumented member.
crersSnapshot :: Lens' CreateSnapshotResponse (Maybe Snapshot)
crersSnapshot = lens _crersSnapshot (\ s a -> s{_crersSnapshot = a})

-- | -- | The response status code.
crersResponseStatus :: Lens' CreateSnapshotResponse Int
crersResponseStatus = lens _crersResponseStatus (\ s a -> s{_crersResponseStatus = a})

instance NFData CreateSnapshotResponse where

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
-- Module      : Network.AWS.ElastiCache.DeleteReplicationGroup
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The /DeleteReplicationGroup/ action deletes an existing replication group. By default, this action deletes the entire replication group, including the primary cluster and all of the read replicas. You can optionally delete only the read replicas, while retaining the primary cluster.
--
-- When you receive a successful response from this action, Amazon ElastiCache immediately begins deleting the selected resources; you cannot cancel or revert this action.
module Network.AWS.ElastiCache.DeleteReplicationGroup
    (
    -- * Creating a Request
      deleteReplicationGroup
    , DeleteReplicationGroup
    -- * Request Lenses
    , drgFinalSnapshotIdentifier
    , drgRetainPrimaryCluster
    , drgReplicationGroupId

    -- * Destructuring the Response
    , deleteReplicationGroupResponse
    , DeleteReplicationGroupResponse
    -- * Response Lenses
    , delrsReplicationGroup
    , delrsResponseStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.ElastiCache.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DeleteReplicationGroup/ action.
--
-- /See:/ 'deleteReplicationGroup' smart constructor.
data DeleteReplicationGroup = DeleteReplicationGroup'
    { _drgFinalSnapshotIdentifier :: !(Maybe Text)
    , _drgRetainPrimaryCluster    :: !(Maybe Bool)
    , _drgReplicationGroupId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteReplicationGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drgFinalSnapshotIdentifier'
--
-- * 'drgRetainPrimaryCluster'
--
-- * 'drgReplicationGroupId'
deleteReplicationGroup
    :: Text -- ^ 'drgReplicationGroupId'
    -> DeleteReplicationGroup
deleteReplicationGroup pReplicationGroupId_ =
    DeleteReplicationGroup'
    { _drgFinalSnapshotIdentifier = Nothing
    , _drgRetainPrimaryCluster = Nothing
    , _drgReplicationGroupId = pReplicationGroupId_
    }

-- | The name of a final node group snapshot. ElastiCache creates the snapshot from the primary node in the cluster, rather than one of the replicas; this is to ensure that it captures the freshest data. After the final snapshot is taken, the cluster is immediately deleted.
drgFinalSnapshotIdentifier :: Lens' DeleteReplicationGroup (Maybe Text)
drgFinalSnapshotIdentifier = lens _drgFinalSnapshotIdentifier (\ s a -> s{_drgFinalSnapshotIdentifier = a});

-- | If set to /true/, all of the read replicas will be deleted, but the primary node will be retained.
drgRetainPrimaryCluster :: Lens' DeleteReplicationGroup (Maybe Bool)
drgRetainPrimaryCluster = lens _drgRetainPrimaryCluster (\ s a -> s{_drgRetainPrimaryCluster = a});

-- | The identifier for the cluster to be deleted. This parameter is not case sensitive.
drgReplicationGroupId :: Lens' DeleteReplicationGroup Text
drgReplicationGroupId = lens _drgReplicationGroupId (\ s a -> s{_drgReplicationGroupId = a});

instance AWSRequest DeleteReplicationGroup where
        type Rs DeleteReplicationGroup =
             DeleteReplicationGroupResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "DeleteReplicationGroupResult"
              (\ s h x ->
                 DeleteReplicationGroupResponse' <$>
                   (x .@? "ReplicationGroup") <*> (pure (fromEnum s)))

instance Hashable DeleteReplicationGroup

instance NFData DeleteReplicationGroup

instance ToHeaders DeleteReplicationGroup where
        toHeaders = const mempty

instance ToPath DeleteReplicationGroup where
        toPath = const "/"

instance ToQuery DeleteReplicationGroup where
        toQuery DeleteReplicationGroup'{..}
          = mconcat
              ["Action" =:
                 ("DeleteReplicationGroup" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "FinalSnapshotIdentifier" =:
                 _drgFinalSnapshotIdentifier,
               "RetainPrimaryCluster" =: _drgRetainPrimaryCluster,
               "ReplicationGroupId" =: _drgReplicationGroupId]

-- | /See:/ 'deleteReplicationGroupResponse' smart constructor.
data DeleteReplicationGroupResponse = DeleteReplicationGroupResponse'
    { _delrsReplicationGroup :: !(Maybe ReplicationGroup)
    , _delrsResponseStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteReplicationGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsReplicationGroup'
--
-- * 'delrsResponseStatus'
deleteReplicationGroupResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteReplicationGroupResponse
deleteReplicationGroupResponse pResponseStatus_ =
    DeleteReplicationGroupResponse'
    { _delrsReplicationGroup = Nothing
    , _delrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
delrsReplicationGroup :: Lens' DeleteReplicationGroupResponse (Maybe ReplicationGroup)
delrsReplicationGroup = lens _delrsReplicationGroup (\ s a -> s{_delrsReplicationGroup = a});

-- | The response status code.
delrsResponseStatus :: Lens' DeleteReplicationGroupResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a});

instance NFData DeleteReplicationGroupResponse

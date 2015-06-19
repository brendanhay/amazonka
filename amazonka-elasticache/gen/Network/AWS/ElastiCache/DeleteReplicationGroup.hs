{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElastiCache.DeleteReplicationGroup
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The /DeleteReplicationGroup/ action deletes an existing replication
-- group. By default, this action deletes the entire replication group,
-- including the primary cluster and all of the read replicas. You can
-- optionally delete only the read replicas, while retaining the primary
-- cluster.
--
-- When you receive a successful response from this action, Amazon
-- ElastiCache immediately begins deleting the selected resources; you
-- cannot cancel or revert this action.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DeleteReplicationGroup.html>
module Network.AWS.ElastiCache.DeleteReplicationGroup
    (
    -- * Request
      DeleteReplicationGroup
    -- ** Request constructor
    , deleteReplicationGroup
    -- ** Request lenses
    , drgFinalSnapshotIdentifier
    , drgRetainPrimaryCluster
    , drgReplicationGroupId

    -- * Response
    , DeleteReplicationGroupResponse
    -- ** Response constructor
    , deleteReplicationGroupResponse
    -- ** Response lenses
    , drgrReplicationGroup
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteReplicationGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drgFinalSnapshotIdentifier'
--
-- * 'drgRetainPrimaryCluster'
--
-- * 'drgReplicationGroupId'
data DeleteReplicationGroup = DeleteReplicationGroup'{_drgFinalSnapshotIdentifier :: Maybe Text, _drgRetainPrimaryCluster :: Maybe Bool, _drgReplicationGroupId :: Text} deriving (Eq, Read, Show)

-- | 'DeleteReplicationGroup' smart constructor.
deleteReplicationGroup :: Text -> DeleteReplicationGroup
deleteReplicationGroup pReplicationGroupId = DeleteReplicationGroup'{_drgFinalSnapshotIdentifier = Nothing, _drgRetainPrimaryCluster = Nothing, _drgReplicationGroupId = pReplicationGroupId};

-- | The name of a final node group snapshot. ElastiCache creates the
-- snapshot from the primary node in the cluster, rather than one of the
-- replicas; this is to ensure that it captures the freshest data. After
-- the final snapshot is taken, the cluster is immediately deleted.
drgFinalSnapshotIdentifier :: Lens' DeleteReplicationGroup (Maybe Text)
drgFinalSnapshotIdentifier = lens _drgFinalSnapshotIdentifier (\ s a -> s{_drgFinalSnapshotIdentifier = a});

-- | If set to /true/, all of the read replicas will be deleted, but the
-- primary node will be retained.
drgRetainPrimaryCluster :: Lens' DeleteReplicationGroup (Maybe Bool)
drgRetainPrimaryCluster = lens _drgRetainPrimaryCluster (\ s a -> s{_drgRetainPrimaryCluster = a});

-- | The identifier for the cluster to be deleted. This parameter is not case
-- sensitive.
drgReplicationGroupId :: Lens' DeleteReplicationGroup Text
drgReplicationGroupId = lens _drgReplicationGroupId (\ s a -> s{_drgReplicationGroupId = a});

instance AWSRequest DeleteReplicationGroup where
        type Sv DeleteReplicationGroup = ElastiCache
        type Rs DeleteReplicationGroup =
             DeleteReplicationGroupResponse
        request = post
        response
          = receiveXMLWrapper "DeleteReplicationGroupResult"
              (\ s h x ->
                 DeleteReplicationGroupResponse' <$>
                   (x .@? "ReplicationGroup"))

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drgrReplicationGroup'
newtype DeleteReplicationGroupResponse = DeleteReplicationGroupResponse'{_drgrReplicationGroup :: Maybe ReplicationGroup} deriving (Eq, Read, Show)

-- | 'DeleteReplicationGroupResponse' smart constructor.
deleteReplicationGroupResponse :: DeleteReplicationGroupResponse
deleteReplicationGroupResponse = DeleteReplicationGroupResponse'{_drgrReplicationGroup = Nothing};

-- | FIXME: Undocumented member.
drgrReplicationGroup :: Lens' DeleteReplicationGroupResponse (Maybe ReplicationGroup)
drgrReplicationGroup = lens _drgrReplicationGroup (\ s a -> s{_drgrReplicationGroup = a});

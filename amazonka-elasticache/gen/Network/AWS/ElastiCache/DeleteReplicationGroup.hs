{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DeleteReplicationGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The /DeleteReplicationGroup/ operation deletes an existing cluster. By default,
-- this operation deletes the entire cluster, including the primary node group
-- and all of the read replicas. You can optionally delete only the read
-- replicas, while retaining the primary node group.
--
-- When you receive a successful response from this operation, Amazon
-- ElastiCache immediately begins deleting the selected resources; you cannot
-- cancel or revert this operation.
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
    , drgReplicationGroupId
    , drgRetainPrimaryCluster

    -- * Response
    , DeleteReplicationGroupResponse
    -- ** Response constructor
    , deleteReplicationGroupResponse
    -- ** Response lenses
    , drgrReplicationGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data DeleteReplicationGroup = DeleteReplicationGroup
    { _drgFinalSnapshotIdentifier :: Maybe Text
    , _drgReplicationGroupId      :: Text
    , _drgRetainPrimaryCluster    :: Maybe Bool
    } deriving (Eq, Ord, Show)

-- | 'DeleteReplicationGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drgFinalSnapshotIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'drgReplicationGroupId' @::@ 'Text'
--
-- * 'drgRetainPrimaryCluster' @::@ 'Maybe' 'Bool'
--
deleteReplicationGroup :: Text -- ^ 'drgReplicationGroupId'
                       -> DeleteReplicationGroup
deleteReplicationGroup p1 = DeleteReplicationGroup
    { _drgReplicationGroupId      = p1
    , _drgRetainPrimaryCluster    = Nothing
    , _drgFinalSnapshotIdentifier = Nothing
    }

-- | The name of a final node group snapshot. ElastiCache creates the snapshot
-- from the primary node in the cluster, rather than one of the replicas; this
-- is to ensure that it captures the freshest data. After the final snapshot is
-- taken, the cluster is immediately deleted.
drgFinalSnapshotIdentifier :: Lens' DeleteReplicationGroup (Maybe Text)
drgFinalSnapshotIdentifier =
    lens _drgFinalSnapshotIdentifier
        (\s a -> s { _drgFinalSnapshotIdentifier = a })

-- | The identifier for the cluster to be deleted. This parameter is not case
-- sensitive.
drgReplicationGroupId :: Lens' DeleteReplicationGroup Text
drgReplicationGroupId =
    lens _drgReplicationGroupId (\s a -> s { _drgReplicationGroupId = a })

-- | If set to /true/, all of the read replicas will be deleted, but the primary
-- node will be retained.
drgRetainPrimaryCluster :: Lens' DeleteReplicationGroup (Maybe Bool)
drgRetainPrimaryCluster =
    lens _drgRetainPrimaryCluster (\s a -> s { _drgRetainPrimaryCluster = a })

newtype DeleteReplicationGroupResponse = DeleteReplicationGroupResponse
    { _drgrReplicationGroup :: Maybe ReplicationGroup
    } deriving (Eq, Show)

-- | 'DeleteReplicationGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drgrReplicationGroup' @::@ 'Maybe' 'ReplicationGroup'
--
deleteReplicationGroupResponse :: DeleteReplicationGroupResponse
deleteReplicationGroupResponse = DeleteReplicationGroupResponse
    { _drgrReplicationGroup = Nothing
    }

drgrReplicationGroup :: Lens' DeleteReplicationGroupResponse (Maybe ReplicationGroup)
drgrReplicationGroup =
    lens _drgrReplicationGroup (\s a -> s { _drgrReplicationGroup = a })

instance ToPath DeleteReplicationGroup where
    toPath = const "/"

instance ToQuery DeleteReplicationGroup where
    toQuery DeleteReplicationGroup{..} = mconcat
        [ "FinalSnapshotIdentifier" =? _drgFinalSnapshotIdentifier
        , "ReplicationGroupId"      =? _drgReplicationGroupId
        , "RetainPrimaryCluster"    =? _drgRetainPrimaryCluster
        ]

instance ToHeaders DeleteReplicationGroup

instance AWSRequest DeleteReplicationGroup where
    type Sv DeleteReplicationGroup = ElastiCache
    type Rs DeleteReplicationGroup = DeleteReplicationGroupResponse

    request  = post "DeleteReplicationGroup"
    response = xmlResponse

instance FromXML DeleteReplicationGroupResponse where
    parseXML = withElement "DeleteReplicationGroupResult" $ \x -> DeleteReplicationGroupResponse
        <$> x .@? "ReplicationGroup"

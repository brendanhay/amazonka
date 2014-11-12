{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ElastiCache.DeleteReplicationGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteReplicationGroup operation deletes an existing cluster. By
-- default, this operation deletes the entire cluster, including the primary
-- node group and all of the read replicas. You can optionally delete only the
-- read replicas, while retaining the primary node group. When you receive a
-- successful response from this operation, Amazon ElastiCache immediately
-- begins deleting the selected resources; you cannot cancel or revert this
-- operation.
module Network.AWS.ElastiCache.DeleteReplicationGroup
    (
    -- * Request
      DeleteReplicationGroupMessage
    -- ** Request constructor
    , deleteReplicationGroupMessage
    -- ** Request lenses
    , drgm1FinalSnapshotIdentifier
    , drgm1ReplicationGroupId
    , drgm1RetainPrimaryCluster

    -- * Response
    , DeleteReplicationGroupResult
    -- ** Response constructor
    , deleteReplicationGroupResult
    -- ** Response lenses
    , drgrReplicationGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data DeleteReplicationGroupMessage = DeleteReplicationGroupMessage
    { _drgm1FinalSnapshotIdentifier :: Maybe Text
    , _drgm1ReplicationGroupId      :: Text
    , _drgm1RetainPrimaryCluster    :: Maybe Bool
    } (Eq, Ord, Show, Generic)

-- | 'DeleteReplicationGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drgm1FinalSnapshotIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'drgm1ReplicationGroupId' @::@ 'Text'
--
-- * 'drgm1RetainPrimaryCluster' @::@ 'Maybe' 'Bool'
--
deleteReplicationGroupMessage :: Text -- ^ 'drgm1ReplicationGroupId'
                              -> DeleteReplicationGroupMessage
deleteReplicationGroupMessage p1 = DeleteReplicationGroupMessage
    { _drgm1ReplicationGroupId      = p1
    , _drgm1RetainPrimaryCluster    = Nothing
    , _drgm1FinalSnapshotIdentifier = Nothing
    }

-- | The name of a final node group snapshot. ElastiCache creates the snapshot
-- from the primary node in the cluster, rather than one of the replicas;
-- this is to ensure that it captures the freshest data. After the final
-- snapshot is taken, the cluster is immediately deleted.
drgm1FinalSnapshotIdentifier :: Lens' DeleteReplicationGroupMessage (Maybe Text)
drgm1FinalSnapshotIdentifier =
    lens _drgm1FinalSnapshotIdentifier
        (\s a -> s { _drgm1FinalSnapshotIdentifier = a })

-- | The identifier for the cluster to be deleted. This parameter is not case
-- sensitive.
drgm1ReplicationGroupId :: Lens' DeleteReplicationGroupMessage Text
drgm1ReplicationGroupId =
    lens _drgm1ReplicationGroupId (\s a -> s { _drgm1ReplicationGroupId = a })

-- | If set to true, all of the read replicas will be deleted, but the primary
-- node will be retained.
drgm1RetainPrimaryCluster :: Lens' DeleteReplicationGroupMessage (Maybe Bool)
drgm1RetainPrimaryCluster =
    lens _drgm1RetainPrimaryCluster
        (\s a -> s { _drgm1RetainPrimaryCluster = a })
instance ToQuery DeleteReplicationGroupMessage

instance ToPath DeleteReplicationGroupMessage where
    toPath = const "/"

newtype DeleteReplicationGroupResult = DeleteReplicationGroupResult
    { _drgrReplicationGroup :: Maybe ReplicationGroup
    } (Eq, Show, Generic)

-- | 'DeleteReplicationGroupResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drgrReplicationGroup' @::@ 'Maybe' 'ReplicationGroup'
--
deleteReplicationGroupResult :: DeleteReplicationGroupResult
deleteReplicationGroupResult = DeleteReplicationGroupResult
    { _drgrReplicationGroup = Nothing
    }

drgrReplicationGroup :: Lens' DeleteReplicationGroupResult (Maybe ReplicationGroup)
drgrReplicationGroup =
    lens _drgrReplicationGroup (\s a -> s { _drgrReplicationGroup = a })

instance FromXML DeleteReplicationGroupResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteReplicationGroupResult"

instance AWSRequest DeleteReplicationGroupMessage where
    type Sv DeleteReplicationGroupMessage = ElastiCache
    type Rs DeleteReplicationGroupMessage = DeleteReplicationGroupResult

    request  = post "DeleteReplicationGroup"
    response = xmlResponse $ \h x -> DeleteReplicationGroupResult
        <$> x %| "ReplicationGroup"

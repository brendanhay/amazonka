{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteReplicationGroup operation deletes an existing replication group.
-- By default, this operation deletes the entire replication group, including
-- the primary cache cluster and all of the read replicas. You can optionally
-- delete only the read replicas, while retaining the primary cache cluster.
-- When you receive a successful response from this operation, Amazon
-- ElastiCache immediately begins deleting the selected resources; you cannot
-- cancel or revert this operation.
-- https://elasticache.us-east-1.amazonaws.com/ ?Action=DeleteReplicationGroup
-- &RetainPrimaryCluster=false &FinalSnapshotIdentifier=my-final-snapshot
-- &ReplicationGroupId=my-repgroup &Version=2014-03-24 &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z &X-Amz-Credential=
-- my-redis-primary my-repgroup deleting My replication group
-- 93eb37db-b9d7-11e3-8a16-7978bb24ffdf.
module Network.AWS.ElastiCache
    (
    -- * Request
      DeleteReplicationGroup
    -- ** Request constructor
    , mkDeleteReplicationGroup
    -- ** Request lenses
    , drgReplicationGroupId
    , drgRetainPrimaryCluster
    , drgFinalSnapshotIdentifier

    -- * Response
    , DeleteReplicationGroupResponse
    -- ** Response constructor
    , mkDeleteReplicationGroupResponse
    -- ** Response lenses
    , drgrReplicationGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a DeleteReplicationGroup operation.
data DeleteReplicationGroup = DeleteReplicationGroup
    { _drgReplicationGroupId :: Text
    , _drgRetainPrimaryCluster :: Maybe Bool
    , _drgFinalSnapshotIdentifier :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteReplicationGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReplicationGroupId ::@ @Text@
--
-- * @RetainPrimaryCluster ::@ @Maybe Bool@
--
-- * @FinalSnapshotIdentifier ::@ @Maybe Text@
--
mkDeleteReplicationGroup :: Text -- ^ 'drgReplicationGroupId'
                         -> DeleteReplicationGroup
mkDeleteReplicationGroup p1 = DeleteReplicationGroup
    { _drgReplicationGroupId = p1
    , _drgRetainPrimaryCluster = Nothing
    , _drgFinalSnapshotIdentifier = Nothing
    }

-- | The identifier for the replication group to be deleted. This parameter is
-- not case sensitive.
drgReplicationGroupId :: Lens' DeleteReplicationGroup Text
drgReplicationGroupId =
    lens _drgReplicationGroupId (\s a -> s { _drgReplicationGroupId = a })

-- | If set to true, all of the read replicas will be deleted, but the primary
-- cache cluster will be retained.
drgRetainPrimaryCluster :: Lens' DeleteReplicationGroup (Maybe Bool)
drgRetainPrimaryCluster =
    lens _drgRetainPrimaryCluster
         (\s a -> s { _drgRetainPrimaryCluster = a })

-- | The name of a final cache cluster snapshot. ElastiCache creates the
-- snapshot from the primary cluster in the replication group, rather than one
-- of the replicas; this is to ensure that it captures the freshest data.
-- After the final snapshot is taken, the replication group is deleted
-- immediately afterward.
drgFinalSnapshotIdentifier :: Lens' DeleteReplicationGroup (Maybe Text)
drgFinalSnapshotIdentifier =
    lens _drgFinalSnapshotIdentifier
         (\s a -> s { _drgFinalSnapshotIdentifier = a })

instance ToQuery DeleteReplicationGroup where
    toQuery = genericQuery def

newtype DeleteReplicationGroupResponse = DeleteReplicationGroupResponse
    { _drgrReplicationGroup :: Maybe ReplicationGroup
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteReplicationGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReplicationGroup ::@ @Maybe ReplicationGroup@
--
mkDeleteReplicationGroupResponse :: DeleteReplicationGroupResponse
mkDeleteReplicationGroupResponse = DeleteReplicationGroupResponse
    { _drgrReplicationGroup = Nothing
    }

-- | Contains all of the attributes of a specific replication group.
drgrReplicationGroup :: Lens' DeleteReplicationGroupResponse (Maybe ReplicationGroup)
drgrReplicationGroup =
    lens _drgrReplicationGroup (\s a -> s { _drgrReplicationGroup = a })

instance FromXML DeleteReplicationGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteReplicationGroup where
    type Sv DeleteReplicationGroup = ElastiCache
    type Rs DeleteReplicationGroup = DeleteReplicationGroupResponse

    request = post "DeleteReplicationGroup"
    response _ = xmlResponse

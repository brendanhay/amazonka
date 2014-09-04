{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.DeleteReplicationGroup
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
module Network.AWS.ElastiCache.V2014_07_15.DeleteReplicationGroup
    (
    -- * Request
      DeleteReplicationGroup
    -- ** Request constructor
    , deleteReplicationGroup
    -- ** Request lenses
    , drgmReplicationGroupId
    , drgmRetainPrimaryCluster
    , drgmFinalSnapshotIdentifier

    -- * Response
    , DeleteReplicationGroupResponse
    -- ** Response lenses
    , rgxReplicationGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteReplicationGroup' request.
deleteReplicationGroup :: Text -- ^ 'drgmReplicationGroupId'
                       -> DeleteReplicationGroup
deleteReplicationGroup p1 = DeleteReplicationGroup
    { _drgmReplicationGroupId = p1
    , _drgmRetainPrimaryCluster = Nothing
    , _drgmFinalSnapshotIdentifier = Nothing
    }
{-# INLINE deleteReplicationGroup #-}

data DeleteReplicationGroup = DeleteReplicationGroup
    { _drgmReplicationGroupId :: Text
      -- ^ The identifier for the replication group to be deleted. This
      -- parameter is not case sensitive.
    , _drgmRetainPrimaryCluster :: Maybe Bool
      -- ^ If set to true, all of the read replicas will be deleted, but the
      -- primary cache cluster will be retained.
    , _drgmFinalSnapshotIdentifier :: Maybe Text
      -- ^ The name of a final cache cluster snapshot. ElastiCache creates
      -- the snapshot from the primary cluster in the replication group,
      -- rather than one of the replicas; this is to ensure that it
      -- captures the freshest data. After the final snapshot is taken,
      -- the replication group is deleted immediately afterward.
    } deriving (Show, Generic)

-- | The identifier for the replication group to be deleted. This parameter is
-- not case sensitive.
drgmReplicationGroupId :: Lens' DeleteReplicationGroup (Text)
drgmReplicationGroupId f x =
    f (_drgmReplicationGroupId x)
        <&> \y -> x { _drgmReplicationGroupId = y }
{-# INLINE drgmReplicationGroupId #-}

-- | If set to true, all of the read replicas will be deleted, but the primary
-- cache cluster will be retained.
drgmRetainPrimaryCluster :: Lens' DeleteReplicationGroup (Maybe Bool)
drgmRetainPrimaryCluster f x =
    f (_drgmRetainPrimaryCluster x)
        <&> \y -> x { _drgmRetainPrimaryCluster = y }
{-# INLINE drgmRetainPrimaryCluster #-}

-- | The name of a final cache cluster snapshot. ElastiCache creates the
-- snapshot from the primary cluster in the replication group, rather than one
-- of the replicas; this is to ensure that it captures the freshest data.
-- After the final snapshot is taken, the replication group is deleted
-- immediately afterward.
drgmFinalSnapshotIdentifier :: Lens' DeleteReplicationGroup (Maybe Text)
drgmFinalSnapshotIdentifier f x =
    f (_drgmFinalSnapshotIdentifier x)
        <&> \y -> x { _drgmFinalSnapshotIdentifier = y }
{-# INLINE drgmFinalSnapshotIdentifier #-}

instance ToQuery DeleteReplicationGroup where
    toQuery = genericQuery def

data DeleteReplicationGroupResponse = DeleteReplicationGroupResponse
    { _rgxReplicationGroup :: Maybe ReplicationGroup
      -- ^ Contains all of the attributes of a specific replication group.
    } deriving (Show, Generic)

-- | Contains all of the attributes of a specific replication group.
rgxReplicationGroup :: Lens' DeleteReplicationGroupResponse (Maybe ReplicationGroup)
rgxReplicationGroup f x =
    f (_rgxReplicationGroup x)
        <&> \y -> x { _rgxReplicationGroup = y }
{-# INLINE rgxReplicationGroup #-}

instance FromXML DeleteReplicationGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteReplicationGroup where
    type Sv DeleteReplicationGroup = ElastiCache
    type Rs DeleteReplicationGroup = DeleteReplicationGroupResponse

    request = post "DeleteReplicationGroup"
    response _ = xmlResponse

{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.DeleteReplicationGroup
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
module Network.AWS.ElastiCache.V2014_03_24.DeleteReplicationGroup where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.ElastiCache.V2014_03_24.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'DeleteReplicationGroup' request.
deleteReplicationGroup :: Text -- ^ '_drgmReplicationGroupId'
                       -> DeleteReplicationGroup
deleteReplicationGroup p1 = DeleteReplicationGroup
    { _drgmReplicationGroupId = p1
    , _drgmRetainPrimaryCluster = Nothing
    , _drgmFinalSnapshotIdentifier = Nothing
    }

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
    } deriving (Generic)

instance ToQuery DeleteReplicationGroup where
    toQuery = genericToQuery def

instance AWSRequest DeleteReplicationGroup where
    type Sv DeleteReplicationGroup = ElastiCache
    type Rs DeleteReplicationGroup = DeleteReplicationGroupResponse

    request = post "DeleteReplicationGroup"
    response _ = xmlResponse

data DeleteReplicationGroupResponse = DeleteReplicationGroupResponse
    { _rgxReplicationGroup :: Maybe ReplicationGroup
      -- ^ Contains all of the attributes of a specific replication group.
    } deriving (Generic)

instance FromXML DeleteReplicationGroupResponse where
    fromXMLOptions = xmlOptions

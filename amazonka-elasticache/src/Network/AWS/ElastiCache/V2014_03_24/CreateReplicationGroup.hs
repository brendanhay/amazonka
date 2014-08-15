{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.CreateReplicationGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateReplicationGroup operation creates a replication group. A
-- replication group is a collection of cache clusters, where one of the
-- clusters is a read/write primary and the other clusters are read-only
-- replicas. Writes to the primary are automatically propagated to the
-- replicas. When you create a replication group, you must specify an existing
-- cache cluster that is in the primary role. When the replication group has
-- been successfully created, you can add one or more read replica replicas to
-- it, up to a total of five read replicas.
module Network.AWS.ElastiCache.V2014_03_24.CreateReplicationGroup where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_03_24.Types
import Network.AWS.Prelude

data CreateReplicationGroup = CreateReplicationGroup
    { _crgmReplicationGroupDescription :: Text
      -- ^ A user-specified description for the replication group.
    , _crgmPrimaryClusterId :: Text
      -- ^ The identifier of the cache cluster that will serve as the
      -- primary for this replication group. This cache cluster must
      -- already exist and have a status of available.
    , _crgmReplicationGroupId :: Text
      -- ^ The replication group identifier. This parameter is stored as a
      -- lowercase string. Constraints: Must contain from 1 to 20
      -- alphanumeric characters or hyphens. First character must be a
      -- letter. Cannot end with a hyphen or contain two consecutive
      -- hyphens.
    } deriving (Show, Generic)

makeLenses ''CreateReplicationGroup

instance ToQuery CreateReplicationGroup where
    toQuery = genericQuery def

data CreateReplicationGroupResponse = CreateReplicationGroupResponse
    { _rgwReplicationGroup :: Maybe ReplicationGroup
      -- ^ Contains all of the attributes of a specific replication group.
    } deriving (Show, Generic)

makeLenses ''CreateReplicationGroupResponse

instance FromXML CreateReplicationGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateReplicationGroup where
    type Sv CreateReplicationGroup = ElastiCache
    type Rs CreateReplicationGroup = CreateReplicationGroupResponse

    request = post "CreateReplicationGroup"
    response _ = xmlResponse

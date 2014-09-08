{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.CreateReplicationGroup
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
-- https://elasticache.us-east-1.amazonaws.com/ ?Action=CreateReplicationGroup
-- ?ReplicationGroupDescription=My%20replication%20group
-- &ReplicationGroupId=my-repgroup &PrimaryClusterId=my-redis-primary
-- &Version=2014-03-24 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20140401T192317Z &X-Amz-Credential= my-redis-primary
-- my-redis-primary my-repgroup creating My replication group
-- f3b7b32d-b9d2-11e3-8a16-7978bb24ffdf.
module Network.AWS.ElastiCache.V2014_07_15.CreateReplicationGroup
    (
    -- * Request
      CreateReplicationGroup
    -- ** Request constructor
    , mkCreateReplicationGroup
    -- ** Request lenses
    , crgReplicationGroupId
    , crgPrimaryClusterId
    , crgReplicationGroupDescription

    -- * Response
    , CreateReplicationGroupResponse
    -- ** Response lenses
    , crgrReplicationGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Represents the input of a CreateReplicationGroup operation.
data CreateReplicationGroup = CreateReplicationGroup
    { _crgReplicationGroupId :: Text
    , _crgPrimaryClusterId :: Text
    , _crgReplicationGroupDescription :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateReplicationGroup' request.
mkCreateReplicationGroup :: Text -- ^ 'crgReplicationGroupId'
                         -> Text -- ^ 'crgPrimaryClusterId'
                         -> Text -- ^ 'crgReplicationGroupDescription'
                         -> CreateReplicationGroup
mkCreateReplicationGroup p1 p2 p3 = CreateReplicationGroup
    { _crgReplicationGroupId = p1
    , _crgPrimaryClusterId = p2
    , _crgReplicationGroupDescription = p3
    }

-- | The replication group identifier. This parameter is stored as a lowercase
-- string. Constraints: Must contain from 1 to 20 alphanumeric characters or
-- hyphens. First character must be a letter. Cannot end with a hyphen or
-- contain two consecutive hyphens.
crgReplicationGroupId :: Lens' CreateReplicationGroup Text
crgReplicationGroupId =
    lens _crgReplicationGroupId (\s a -> s { _crgReplicationGroupId = a })

-- | The identifier of the cache cluster that will serve as the primary for this
-- replication group. This cache cluster must already exist and have a status
-- of available.
crgPrimaryClusterId :: Lens' CreateReplicationGroup Text
crgPrimaryClusterId =
    lens _crgPrimaryClusterId (\s a -> s { _crgPrimaryClusterId = a })

-- | A user-specified description for the replication group.
crgReplicationGroupDescription :: Lens' CreateReplicationGroup Text
crgReplicationGroupDescription =
    lens _crgReplicationGroupDescription
         (\s a -> s { _crgReplicationGroupDescription = a })

instance ToQuery CreateReplicationGroup where
    toQuery = genericQuery def

newtype CreateReplicationGroupResponse = CreateReplicationGroupResponse
    { _crgrReplicationGroup :: Maybe ReplicationGroup
    } deriving (Show, Generic)

-- | Contains all of the attributes of a specific replication group.
crgrReplicationGroup :: Lens' CreateReplicationGroupResponse (Maybe ReplicationGroup)
crgrReplicationGroup =
    lens _crgrReplicationGroup (\s a -> s { _crgrReplicationGroup = a })

instance FromXML CreateReplicationGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateReplicationGroup where
    type Sv CreateReplicationGroup = ElastiCache
    type Rs CreateReplicationGroup = CreateReplicationGroupResponse

    request = post "CreateReplicationGroup"
    response _ = xmlResponse

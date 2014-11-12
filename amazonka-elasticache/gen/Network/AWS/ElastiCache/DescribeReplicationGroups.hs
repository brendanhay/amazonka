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

-- Module      : Network.AWS.ElastiCache.DescribeReplicationGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeReplicationGroups operation returns information about a
-- particular replication group. If no identifier is specified,
-- DescribeReplicationGroups returns information about all replication groups.
module Network.AWS.ElastiCache.DescribeReplicationGroups
    (
    -- * Request
      DescribeReplicationGroupsMessage
    -- ** Request constructor
    , describeReplicationGroupsMessage
    -- ** Request lenses
    , drgmMarker
    , drgmMaxRecords
    , drgmReplicationGroupId

    -- * Response
    , ReplicationGroupMessage
    -- ** Response constructor
    , replicationGroupMessage
    -- ** Response lenses
    , rgmMarker
    , rgmReplicationGroups
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data DescribeReplicationGroupsMessage = DescribeReplicationGroupsMessage
    { _drgmMarker             :: Maybe Text
    , _drgmMaxRecords         :: Maybe Int
    , _drgmReplicationGroupId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeReplicationGroupsMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drgmMarker' @::@ 'Maybe' 'Text'
--
-- * 'drgmMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'drgmReplicationGroupId' @::@ 'Maybe' 'Text'
--
describeReplicationGroupsMessage :: DescribeReplicationGroupsMessage
describeReplicationGroupsMessage = DescribeReplicationGroupsMessage
    { _drgmReplicationGroupId = Nothing
    , _drgmMaxRecords         = Nothing
    , _drgmMarker             = Nothing
    }

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
drgmMarker :: Lens' DescribeReplicationGroupsMessage (Maybe Text)
drgmMarker = lens _drgmMarker (\s a -> s { _drgmMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
drgmMaxRecords :: Lens' DescribeReplicationGroupsMessage (Maybe Int)
drgmMaxRecords = lens _drgmMaxRecords (\s a -> s { _drgmMaxRecords = a })

-- | The identifier for the replication group to be described. This parameter
-- is not case sensitive. If you do not specify this parameter, information
-- about all replication groups is returned.
drgmReplicationGroupId :: Lens' DescribeReplicationGroupsMessage (Maybe Text)
drgmReplicationGroupId =
    lens _drgmReplicationGroupId (\s a -> s { _drgmReplicationGroupId = a })

instance ToQuery DescribeReplicationGroupsMessage

instance ToPath DescribeReplicationGroupsMessage where
    toPath = const "/"

data ReplicationGroupMessage = ReplicationGroupMessage
    { _rgmMarker            :: Maybe Text
    , _rgmReplicationGroups :: [ReplicationGroup]
    } deriving (Eq, Show, Generic)

-- | 'ReplicationGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rgmMarker' @::@ 'Maybe' 'Text'
--
-- * 'rgmReplicationGroups' @::@ ['ReplicationGroup']
--
replicationGroupMessage :: ReplicationGroupMessage
replicationGroupMessage = ReplicationGroupMessage
    { _rgmMarker            = Nothing
    , _rgmReplicationGroups = mempty
    }

-- | Provides an identifier to allow retrieval of paginated results.
rgmMarker :: Lens' ReplicationGroupMessage (Maybe Text)
rgmMarker = lens _rgmMarker (\s a -> s { _rgmMarker = a })

-- | A list of replication groups. Each item in the list contains detailed
-- information about one replication group.
rgmReplicationGroups :: Lens' ReplicationGroupMessage [ReplicationGroup]
rgmReplicationGroups =
    lens _rgmReplicationGroups (\s a -> s { _rgmReplicationGroups = a })

instance FromXML ReplicationGroupMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReplicationGroupMessage"

instance AWSRequest DescribeReplicationGroupsMessage where
    type Sv DescribeReplicationGroupsMessage = ElastiCache
    type Rs DescribeReplicationGroupsMessage = ReplicationGroupMessage

    request  = post "DescribeReplicationGroups"
    response = xmlResponse $ \h x -> ReplicationGroupMessage
        <$> x %| "Marker"
        <*> x %| "ReplicationGroups"

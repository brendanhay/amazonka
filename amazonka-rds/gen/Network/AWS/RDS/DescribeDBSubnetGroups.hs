{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.RDS.DescribeDBSubnetGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of DBSubnetGroup descriptions. If a DBSubnetGroupName is
-- specified, the list will contain only the descriptions of the specified
-- DBSubnetGroup. For an overview of CIDR ranges, go to the Wikipedia
-- Tutorial.
module Network.AWS.RDS.DescribeDBSubnetGroups
    (
    -- * Request
      DescribeDBSubnetGroups
    -- ** Request constructor
    , describeDBSubnetGroups
    -- ** Request lenses
    , ddbsgDBSubnetGroupName
    , ddbsgFilters
    , ddbsgMarker
    , ddbsgMaxRecords

    -- * Response
    , DescribeDBSubnetGroupsResponse
    -- ** Response constructor
    , describeDBSubnetGroupsResponse
    -- ** Response lenses
    , ddbsgrDBSubnetGroups
    , ddbsgrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data DescribeDBSubnetGroups = DescribeDBSubnetGroups
    { _ddbsgDBSubnetGroupName :: Maybe Text
    , _ddbsgFilters           :: [Filter]
    , _ddbsgMarker            :: Maybe Text
    , _ddbsgMaxRecords        :: Maybe Int
    } deriving (Eq, Show, Generic)

-- | 'DescribeDBSubnetGroups' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbsgDBSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'ddbsgFilters' @::@ ['Filter']
--
-- * 'ddbsgMarker' @::@ 'Maybe' 'Text'
--
-- * 'ddbsgMaxRecords' @::@ 'Maybe' 'Int'
--
describeDBSubnetGroups :: DescribeDBSubnetGroups
describeDBSubnetGroups = DescribeDBSubnetGroups
    { _ddbsgDBSubnetGroupName = Nothing
    , _ddbsgFilters           = mempty
    , _ddbsgMaxRecords        = Nothing
    , _ddbsgMarker            = Nothing
    }

-- | The name of the DB subnet group to return details for.
ddbsgDBSubnetGroupName :: Lens' DescribeDBSubnetGroups (Maybe Text)
ddbsgDBSubnetGroupName =
    lens _ddbsgDBSubnetGroupName (\s a -> s { _ddbsgDBSubnetGroupName = a })

-- | This parameter is not currently supported.
ddbsgFilters :: Lens' DescribeDBSubnetGroups [Filter]
ddbsgFilters = lens _ddbsgFilters (\s a -> s { _ddbsgFilters = a })

-- | An optional pagination token provided by a previous
-- DescribeDBSubnetGroups request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
ddbsgMarker :: Lens' DescribeDBSubnetGroups (Maybe Text)
ddbsgMarker = lens _ddbsgMarker (\s a -> s { _ddbsgMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
ddbsgMaxRecords :: Lens' DescribeDBSubnetGroups (Maybe Int)
ddbsgMaxRecords = lens _ddbsgMaxRecords (\s a -> s { _ddbsgMaxRecords = a })

instance ToQuery DescribeDBSubnetGroups

instance ToPath DescribeDBSubnetGroups where
    toPath = const "/"

data DescribeDBSubnetGroupsResponse = DescribeDBSubnetGroupsResponse
    { _ddbsgrDBSubnetGroups :: [DBSubnetGroup]
    , _ddbsgrMarker         :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeDBSubnetGroupsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbsgrDBSubnetGroups' @::@ ['DBSubnetGroup']
--
-- * 'ddbsgrMarker' @::@ 'Maybe' 'Text'
--
describeDBSubnetGroupsResponse :: DescribeDBSubnetGroupsResponse
describeDBSubnetGroupsResponse = DescribeDBSubnetGroupsResponse
    { _ddbsgrMarker         = Nothing
    , _ddbsgrDBSubnetGroups = mempty
    }

-- | A list of DBSubnetGroup instances.
ddbsgrDBSubnetGroups :: Lens' DescribeDBSubnetGroupsResponse [DBSubnetGroup]
ddbsgrDBSubnetGroups =
    lens _ddbsgrDBSubnetGroups (\s a -> s { _ddbsgrDBSubnetGroups = a })

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
ddbsgrMarker :: Lens' DescribeDBSubnetGroupsResponse (Maybe Text)
ddbsgrMarker = lens _ddbsgrMarker (\s a -> s { _ddbsgrMarker = a })

instance AWSRequest DescribeDBSubnetGroups where
    type Sv DescribeDBSubnetGroups = RDS
    type Rs DescribeDBSubnetGroups = DescribeDBSubnetGroupsResponse

    request  = post "DescribeDBSubnetGroups"
    response = xmlResponse $ \h x -> DescribeDBSubnetGroupsResponse
        <$> x %| "DBSubnetGroups"
        <*> x %| "Marker"

instance AWSPager DescribeDBSubnetGroups where
    next rq rs = (\x -> rq & ddbsgMarker ?~ x)
        <$> (rs ^. ddbsgrMarker)

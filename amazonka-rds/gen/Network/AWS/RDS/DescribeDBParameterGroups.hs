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

-- Module      : Network.AWS.RDS.DescribeDBParameterGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of DBParameterGroup descriptions. If a DBParameterGroupName
-- is specified, the list will contain only the description of the specified
-- DB parameter group.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBParameterGroups.html>
module Network.AWS.RDS.DescribeDBParameterGroups
    (
    -- * Request
      DescribeDBParameterGroups
    -- ** Request constructor
    , describeDBParameterGroups
    -- ** Request lenses
    , ddbpgDBParameterGroupName
    , ddbpgFilters
    , ddbpgMarker
    , ddbpgMaxRecords

    -- * Response
    , DescribeDBParameterGroupsResponse
    -- ** Response constructor
    , describeDBParameterGroupsResponse
    -- ** Response lenses
    , ddbpgrDBParameterGroups
    , ddbpgrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data DescribeDBParameterGroups = DescribeDBParameterGroups
    { _ddbpgDBParameterGroupName :: Maybe Text
    , _ddbpgFilters              :: List "Filter" Filter
    , _ddbpgMarker               :: Maybe Text
    , _ddbpgMaxRecords           :: Maybe Int
    } deriving (Eq, Show)

-- | 'DescribeDBParameterGroups' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbpgDBParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'ddbpgFilters' @::@ ['Filter']
--
-- * 'ddbpgMarker' @::@ 'Maybe' 'Text'
--
-- * 'ddbpgMaxRecords' @::@ 'Maybe' 'Int'
--
describeDBParameterGroups :: DescribeDBParameterGroups
describeDBParameterGroups = DescribeDBParameterGroups
    { _ddbpgDBParameterGroupName = Nothing
    , _ddbpgFilters              = mempty
    , _ddbpgMaxRecords           = Nothing
    , _ddbpgMarker               = Nothing
    }

-- | The name of a specific DB parameter group to return details for.
-- Constraints: Must be 1 to 255 alphanumeric characters First character
-- must be a letter Cannot end with a hyphen or contain two consecutive
-- hyphens.
ddbpgDBParameterGroupName :: Lens' DescribeDBParameterGroups (Maybe Text)
ddbpgDBParameterGroupName =
    lens _ddbpgDBParameterGroupName
        (\s a -> s { _ddbpgDBParameterGroupName = a })

-- | This parameter is not currently supported.
ddbpgFilters :: Lens' DescribeDBParameterGroups [Filter]
ddbpgFilters = lens _ddbpgFilters (\s a -> s { _ddbpgFilters = a }) . _List

-- | An optional pagination token provided by a previous
-- DescribeDBParameterGroups request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
ddbpgMarker :: Lens' DescribeDBParameterGroups (Maybe Text)
ddbpgMarker = lens _ddbpgMarker (\s a -> s { _ddbpgMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a pagination token called a
-- marker is included in the response so that the remaining results may be
-- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
ddbpgMaxRecords :: Lens' DescribeDBParameterGroups (Maybe Int)
ddbpgMaxRecords = lens _ddbpgMaxRecords (\s a -> s { _ddbpgMaxRecords = a })

data DescribeDBParameterGroupsResponse = DescribeDBParameterGroupsResponse
    { _ddbpgrDBParameterGroups :: List "DBParameterGroup" DBParameterGroup
    , _ddbpgrMarker            :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeDBParameterGroupsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbpgrDBParameterGroups' @::@ ['DBParameterGroup']
--
-- * 'ddbpgrMarker' @::@ 'Maybe' 'Text'
--
describeDBParameterGroupsResponse :: DescribeDBParameterGroupsResponse
describeDBParameterGroupsResponse = DescribeDBParameterGroupsResponse
    { _ddbpgrMarker            = Nothing
    , _ddbpgrDBParameterGroups = mempty
    }

-- | A list of DBParameterGroup instances.
ddbpgrDBParameterGroups :: Lens' DescribeDBParameterGroupsResponse [DBParameterGroup]
ddbpgrDBParameterGroups =
    lens _ddbpgrDBParameterGroups (\s a -> s { _ddbpgrDBParameterGroups = a })
        . _List

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by MaxRecords.
ddbpgrMarker :: Lens' DescribeDBParameterGroupsResponse (Maybe Text)
ddbpgrMarker = lens _ddbpgrMarker (\s a -> s { _ddbpgrMarker = a })

instance ToPath DescribeDBParameterGroups where
    toPath = const "/"

instance ToQuery DescribeDBParameterGroups where
    toQuery DescribeDBParameterGroups{..} = mconcat
        [ "DBParameterGroupName" =? _ddbpgDBParameterGroupName
        , "Filters"              =? _ddbpgFilters
        , "Marker"               =? _ddbpgMarker
        , "MaxRecords"           =? _ddbpgMaxRecords
        ]

instance ToHeaders DescribeDBParameterGroups

instance AWSRequest DescribeDBParameterGroups where
    type Sv DescribeDBParameterGroups = RDS
    type Rs DescribeDBParameterGroups = DescribeDBParameterGroupsResponse

    request  = post "DescribeDBParameterGroups"
    response = xmlResponse

instance FromXML DescribeDBParameterGroupsResponse where
    parseXML = withElement "DescribeDBParameterGroupsResult" $ \x -> DescribeDBParameterGroupsResponse
        <$> x .@? "DBParameterGroups"
        <*> x .@? "Marker"

instance AWSPager DescribeDBParameterGroups where
    page rq rs
        | stop (rq ^. ddbpgMarker) = Nothing
        | otherwise = (\x -> rq & ddbpgMarker ?~ x)
            <$> (rs ^. ddbpgrMarker)

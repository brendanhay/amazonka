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

-- Module      : Network.AWS.RDS.DescribeDBSecurityGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a list of 'DBSecurityGroup' descriptions. If a 'DBSecurityGroupName' is
-- specified, the list will contain only the descriptions of the specified DB
-- security group.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBSecurityGroups.html>
module Network.AWS.RDS.DescribeDBSecurityGroups
    (
    -- * Request
      DescribeDBSecurityGroups
    -- ** Request constructor
    , describeDBSecurityGroups
    -- ** Request lenses
    , ddbsg1DBSecurityGroupName
    , ddbsg1Filters
    , ddbsg1Marker
    , ddbsg1MaxRecords

    -- * Response
    , DescribeDBSecurityGroupsResponse
    -- ** Response constructor
    , describeDBSecurityGroupsResponse
    -- ** Response lenses
    , ddbsgr1DBSecurityGroups
    , ddbsgr1Marker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data DescribeDBSecurityGroups = DescribeDBSecurityGroups
    { _ddbsg1DBSecurityGroupName :: Maybe Text
    , _ddbsg1Filters             :: List "Filter" Filter
    , _ddbsg1Marker              :: Maybe Text
    , _ddbsg1MaxRecords          :: Maybe Int
    } deriving (Eq, Show)

-- | 'DescribeDBSecurityGroups' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbsg1DBSecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'ddbsg1Filters' @::@ ['Filter']
--
-- * 'ddbsg1Marker' @::@ 'Maybe' 'Text'
--
-- * 'ddbsg1MaxRecords' @::@ 'Maybe' 'Int'
--
describeDBSecurityGroups :: DescribeDBSecurityGroups
describeDBSecurityGroups = DescribeDBSecurityGroups
    { _ddbsg1DBSecurityGroupName = Nothing
    , _ddbsg1Filters             = mempty
    , _ddbsg1MaxRecords          = Nothing
    , _ddbsg1Marker              = Nothing
    }

-- | The name of the DB security group to return details for.
ddbsg1DBSecurityGroupName :: Lens' DescribeDBSecurityGroups (Maybe Text)
ddbsg1DBSecurityGroupName =
    lens _ddbsg1DBSecurityGroupName
        (\s a -> s { _ddbsg1DBSecurityGroupName = a })

-- | This parameter is not currently supported.
ddbsg1Filters :: Lens' DescribeDBSecurityGroups [Filter]
ddbsg1Filters = lens _ddbsg1Filters (\s a -> s { _ddbsg1Filters = a }) . _List

-- | An optional pagination token provided by a previous DescribeDBSecurityGroups
-- request. If this parameter is specified, the response includes only records
-- beyond the marker, up to the value specified by 'MaxRecords'.
ddbsg1Marker :: Lens' DescribeDBSecurityGroups (Maybe Text)
ddbsg1Marker = lens _ddbsg1Marker (\s a -> s { _ddbsg1Marker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified 'MaxRecords' value, a pagination token called a marker
-- is included in the response so that the remaining results may be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
ddbsg1MaxRecords :: Lens' DescribeDBSecurityGroups (Maybe Int)
ddbsg1MaxRecords = lens _ddbsg1MaxRecords (\s a -> s { _ddbsg1MaxRecords = a })

data DescribeDBSecurityGroupsResponse = DescribeDBSecurityGroupsResponse
    { _ddbsgr1DBSecurityGroups :: List "DBSecurityGroup" DBSecurityGroup
    , _ddbsgr1Marker           :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeDBSecurityGroupsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbsgr1DBSecurityGroups' @::@ ['DBSecurityGroup']
--
-- * 'ddbsgr1Marker' @::@ 'Maybe' 'Text'
--
describeDBSecurityGroupsResponse :: DescribeDBSecurityGroupsResponse
describeDBSecurityGroupsResponse = DescribeDBSecurityGroupsResponse
    { _ddbsgr1Marker           = Nothing
    , _ddbsgr1DBSecurityGroups = mempty
    }

-- | A list of 'DBSecurityGroup' instances.
ddbsgr1DBSecurityGroups :: Lens' DescribeDBSecurityGroupsResponse [DBSecurityGroup]
ddbsgr1DBSecurityGroups =
    lens _ddbsgr1DBSecurityGroups (\s a -> s { _ddbsgr1DBSecurityGroups = a })
        . _List

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the marker,
-- up to the value specified by 'MaxRecords'.
ddbsgr1Marker :: Lens' DescribeDBSecurityGroupsResponse (Maybe Text)
ddbsgr1Marker = lens _ddbsgr1Marker (\s a -> s { _ddbsgr1Marker = a })

instance ToPath DescribeDBSecurityGroups where
    toPath = const "/"

instance ToQuery DescribeDBSecurityGroups where
    toQuery DescribeDBSecurityGroups{..} = mconcat
        [ "DBSecurityGroupName" =? _ddbsg1DBSecurityGroupName
        , "Filters"             =? _ddbsg1Filters
        , "Marker"              =? _ddbsg1Marker
        , "MaxRecords"          =? _ddbsg1MaxRecords
        ]

instance ToHeaders DescribeDBSecurityGroups

instance AWSRequest DescribeDBSecurityGroups where
    type Sv DescribeDBSecurityGroups = RDS
    type Rs DescribeDBSecurityGroups = DescribeDBSecurityGroupsResponse

    request  = post "DescribeDBSecurityGroups"
    response = xmlResponse

instance FromXML DescribeDBSecurityGroupsResponse where
    parseXML = withElement "DescribeDBSecurityGroupsResult" $ \x -> DescribeDBSecurityGroupsResponse
        <$> x .@  "DBSecurityGroups"
        <*> x .@? "Marker"

instance AWSPager DescribeDBSecurityGroups where
    page rq rs
        | stop (rq ^. ddbsg1Marker) = Nothing
        | otherwise = (\x -> rq & ddbsg1Marker ?~ x)
            <$> (rs ^. ddbsgr1Marker)

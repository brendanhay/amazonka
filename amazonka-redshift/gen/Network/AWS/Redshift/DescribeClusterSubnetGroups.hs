{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeClusterSubnetGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns one or more cluster subnet group objects, which contain metadata
-- about your cluster subnet groups. By default, this operation returns
-- information about all cluster subnet groups that are defined in you AWS
-- account.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeClusterSubnetGroups.html>
module Network.AWS.Redshift.DescribeClusterSubnetGroups
    (
    -- * Request
      DescribeClusterSubnetGroups
    -- ** Request constructor
    , describeClusterSubnetGroups
    -- ** Request lenses
    , dcsg1ClusterSubnetGroupName
    , dcsg1Marker
    , dcsg1MaxRecords

    -- * Response
    , DescribeClusterSubnetGroupsResponse
    -- ** Response constructor
    , describeClusterSubnetGroupsResponse
    -- ** Response lenses
    , dcsgrClusterSubnetGroups
    , dcsgrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data DescribeClusterSubnetGroups = DescribeClusterSubnetGroups
    { _dcsg1ClusterSubnetGroupName :: Maybe Text
    , _dcsg1Marker                 :: Maybe Text
    , _dcsg1MaxRecords             :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeClusterSubnetGroups' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsg1ClusterSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dcsg1Marker' @::@ 'Maybe' 'Text'
--
-- * 'dcsg1MaxRecords' @::@ 'Maybe' 'Int'
--
describeClusterSubnetGroups :: DescribeClusterSubnetGroups
describeClusterSubnetGroups = DescribeClusterSubnetGroups
    { _dcsg1ClusterSubnetGroupName = Nothing
    , _dcsg1MaxRecords             = Nothing
    , _dcsg1Marker                 = Nothing
    }

-- | The name of the cluster subnet group for which information is requested.
dcsg1ClusterSubnetGroupName :: Lens' DescribeClusterSubnetGroups (Maybe Text)
dcsg1ClusterSubnetGroupName =
    lens _dcsg1ClusterSubnetGroupName
        (\s a -> s { _dcsg1ClusterSubnetGroupName = a })

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterSubnetGroups
-- request exceed the value specified in MaxRecords, AWS returns a value in
-- the Marker field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the Marker
-- parameter and retrying the request.
dcsg1Marker :: Lens' DescribeClusterSubnetGroups (Maybe Text)
dcsg1Marker = lens _dcsg1Marker (\s a -> s { _dcsg1Marker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: 100 Constraints: minimum 20, maximum 100.
dcsg1MaxRecords :: Lens' DescribeClusterSubnetGroups (Maybe Int)
dcsg1MaxRecords = lens _dcsg1MaxRecords (\s a -> s { _dcsg1MaxRecords = a })

data DescribeClusterSubnetGroupsResponse = DescribeClusterSubnetGroupsResponse
    { _dcsgrClusterSubnetGroups :: [ClusterSubnetGroup]
    , _dcsgrMarker              :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeClusterSubnetGroupsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgrClusterSubnetGroups' @::@ ['ClusterSubnetGroup']
--
-- * 'dcsgrMarker' @::@ 'Maybe' 'Text'
--
describeClusterSubnetGroupsResponse :: DescribeClusterSubnetGroupsResponse
describeClusterSubnetGroupsResponse = DescribeClusterSubnetGroupsResponse
    { _dcsgrMarker              = Nothing
    , _dcsgrClusterSubnetGroups = mempty
    }

-- | A list of ClusterSubnetGroup instances.
dcsgrClusterSubnetGroups :: Lens' DescribeClusterSubnetGroupsResponse [ClusterSubnetGroup]
dcsgrClusterSubnetGroups =
    lens _dcsgrClusterSubnetGroups
        (\s a -> s { _dcsgrClusterSubnetGroups = a })

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the Marker parameter and retrying the command. If the
-- Marker field is empty, all response records have been retrieved for the
-- request.
dcsgrMarker :: Lens' DescribeClusterSubnetGroupsResponse (Maybe Text)
dcsgrMarker = lens _dcsgrMarker (\s a -> s { _dcsgrMarker = a })

instance ToPath DescribeClusterSubnetGroups where
    toPath = const "/"

instance ToQuery DescribeClusterSubnetGroups

instance ToHeaders DescribeClusterSubnetGroups

instance AWSRequest DescribeClusterSubnetGroups where
    type Sv DescribeClusterSubnetGroups = Redshift
    type Rs DescribeClusterSubnetGroups = DescribeClusterSubnetGroupsResponse

    request  = post "DescribeClusterSubnetGroups"
    response = xmlResponse

instance FromXML DescribeClusterSubnetGroupsResponse where
    parseXML x = DescribeClusterSubnetGroupsResponse
        <$> x .@ "ClusterSubnetGroups"
        <*> x .@? "Marker"

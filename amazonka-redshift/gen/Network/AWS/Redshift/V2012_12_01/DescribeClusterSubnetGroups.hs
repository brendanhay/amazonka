{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DescribeClusterSubnetGroups
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
-- account. https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeClusterSubnetGroups &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130130/us-east-1/redshift/aws4_request
-- &x-amz-date=20130130T153938Z
-- &x-amz-signedheaders=content-type;host;x-amz-date vpc-5d917a30 my subnet
-- group my-subnet-group Complete Active subnet-71c5091c us-east-1a Active
-- subnet-78de1215 us-east-1a 42024b68-6af3-11e2-a726-6368a468fa67.
module Network.AWS.Redshift.V2012_12_01.DescribeClusterSubnetGroups
    (
    -- * Request
      DescribeClusterSubnetGroups
    -- ** Request constructor
    , mkDescribeClusterSubnetGroups
    -- ** Request lenses
    , dcsg3ClusterSubnetGroupName
    , dcsg3MaxRecords
    , dcsg3Marker

    -- * Response
    , DescribeClusterSubnetGroupsResponse
    -- ** Response lenses
    , dcsgrsrsMarker
    , dcsgrsrsClusterSubnetGroups
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | 
data DescribeClusterSubnetGroups = DescribeClusterSubnetGroups
    { _dcsg3ClusterSubnetGroupName :: Maybe Text
    , _dcsg3MaxRecords :: Maybe Integer
    , _dcsg3Marker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeClusterSubnetGroups' request.
mkDescribeClusterSubnetGroups :: DescribeClusterSubnetGroups
mkDescribeClusterSubnetGroups = DescribeClusterSubnetGroups
    { _dcsg3ClusterSubnetGroupName = Nothing
    , _dcsg3MaxRecords = Nothing
    , _dcsg3Marker = Nothing
    }

-- | The name of the cluster subnet group for which information is requested.
dcsg3ClusterSubnetGroupName :: Lens' DescribeClusterSubnetGroups (Maybe Text)
dcsg3ClusterSubnetGroupName =
    lens _dcsg3ClusterSubnetGroupName
         (\s a -> s { _dcsg3ClusterSubnetGroupName = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the returned
-- marker value. Default: 100 Constraints: minimum 20, maximum 100.
dcsg3MaxRecords :: Lens' DescribeClusterSubnetGroups (Maybe Integer)
dcsg3MaxRecords = lens _dcsg3MaxRecords (\s a -> s { _dcsg3MaxRecords = a })

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a DescribeClusterSubnetGroups request
-- exceed the value specified in MaxRecords, AWS returns a value in the Marker
-- field of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the Marker parameter and retrying
-- the request.
dcsg3Marker :: Lens' DescribeClusterSubnetGroups (Maybe Text)
dcsg3Marker = lens _dcsg3Marker (\s a -> s { _dcsg3Marker = a })

instance ToQuery DescribeClusterSubnetGroups where
    toQuery = genericQuery def

-- | Contains the output from the DescribeClusterSubnetGroups action.
data DescribeClusterSubnetGroupsResponse = DescribeClusterSubnetGroupsResponse
    { _dcsgrsrsMarker :: Maybe Text
    , _dcsgrsrsClusterSubnetGroups :: [ClusterSubnetGroup]
    } deriving (Show, Generic)

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
dcsgrsrsMarker :: Lens' DescribeClusterSubnetGroupsResponse (Maybe Text)
dcsgrsrsMarker = lens _dcsgrsrsMarker (\s a -> s { _dcsgrsrsMarker = a })

-- | A list of ClusterSubnetGroup instances.
dcsgrsrsClusterSubnetGroups :: Lens' DescribeClusterSubnetGroupsResponse [ClusterSubnetGroup]
dcsgrsrsClusterSubnetGroups =
    lens _dcsgrsrsClusterSubnetGroups
         (\s a -> s { _dcsgrsrsClusterSubnetGroups = a })

instance FromXML DescribeClusterSubnetGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeClusterSubnetGroups where
    type Sv DescribeClusterSubnetGroups = Redshift
    type Rs DescribeClusterSubnetGroups = DescribeClusterSubnetGroupsResponse

    request = post "DescribeClusterSubnetGroups"
    response _ = xmlResponse

instance AWSPager DescribeClusterSubnetGroups where
    next rq rs = (\x -> rq & dcsg3Marker ?~ x) <$> (rs ^. dcsgrsrsMarker)


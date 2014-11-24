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

-- Module      : Network.AWS.Redshift.DescribeClusterSecurityGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about Amazon Redshift security groups. If the name of a
-- security group is specified, the response will contain only information
-- about only that security group. For information about managing security
-- groups, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html
-- Amazon Redshift Cluster Security Groups> in the /Amazon Redshift Cluster
-- Management Guide/. If you specify both tag keys and tag values in the same
-- request, Amazon Redshift returns all security groups that match any
-- combination of the specified keys and values. For example, if you have
-- owner and environment for tag keys, and admin and test for tag values, all
-- security groups that have any combination of those values are returned. If
-- both tag keys and values are omitted from the request, security groups are
-- returned regardless of whether they have tag keys or values associated with
-- them.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeClusterSecurityGroups.html>
module Network.AWS.Redshift.DescribeClusterSecurityGroups
    (
    -- * Request
      DescribeClusterSecurityGroups
    -- ** Request constructor
    , describeClusterSecurityGroups
    -- ** Request lenses
    , dcsgClusterSecurityGroupName
    , dcsgMarker
    , dcsgMaxRecords
    , dcsgTagKeys
    , dcsgTagValues

    -- * Response
    , DescribeClusterSecurityGroupsResponse
    -- ** Response constructor
    , describeClusterSecurityGroupsResponse
    -- ** Response lenses
    , dcsgr1ClusterSecurityGroups
    , dcsgr1Marker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data DescribeClusterSecurityGroups = DescribeClusterSecurityGroups
    { _dcsgClusterSecurityGroupName :: Maybe Text
    , _dcsgMarker                   :: Maybe Text
    , _dcsgMaxRecords               :: Maybe Int
    , _dcsgTagKeys                  :: List "TagKey" Text
    , _dcsgTagValues                :: List "TagValue" Text
    } deriving (Eq, Ord, Show)

-- | 'DescribeClusterSecurityGroups' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgClusterSecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dcsgMarker' @::@ 'Maybe' 'Text'
--
-- * 'dcsgMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dcsgTagKeys' @::@ ['Text']
--
-- * 'dcsgTagValues' @::@ ['Text']
--
describeClusterSecurityGroups :: DescribeClusterSecurityGroups
describeClusterSecurityGroups = DescribeClusterSecurityGroups
    { _dcsgClusterSecurityGroupName = Nothing
    , _dcsgMaxRecords               = Nothing
    , _dcsgMarker                   = Nothing
    , _dcsgTagKeys                  = mempty
    , _dcsgTagValues                = mempty
    }

-- | The name of a cluster security group for which you are requesting
-- details. You can specify either the Marker parameter or a
-- ClusterSecurityGroupName parameter, but not both. Example:
-- securitygroup1.
dcsgClusterSecurityGroupName :: Lens' DescribeClusterSecurityGroups (Maybe Text)
dcsgClusterSecurityGroupName =
    lens _dcsgClusterSecurityGroupName
        (\s a -> s { _dcsgClusterSecurityGroupName = a })

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeClusterSecurityGroups>
-- request exceed the value specified in MaxRecords, AWS returns a value in
-- the Marker field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the Marker
-- parameter and retrying the request. Constraints: You can specify either
-- the ClusterSecurityGroupName parameter or the Marker parameter, but not
-- both.
dcsgMarker :: Lens' DescribeClusterSecurityGroups (Maybe Text)
dcsgMarker = lens _dcsgMarker (\s a -> s { _dcsgMarker = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value. Default: 100 Constraints: minimum 20, maximum 100.
dcsgMaxRecords :: Lens' DescribeClusterSecurityGroups (Maybe Int)
dcsgMaxRecords = lens _dcsgMaxRecords (\s a -> s { _dcsgMaxRecords = a })

-- | A tag key or keys for which you want to return all matching cluster
-- security groups that are associated with the specified key or keys. For
-- example, suppose that you have security groups that are tagged with keys
-- called owner and environment. If you specify both of these tag keys in
-- the request, Amazon Redshift returns a response with the security groups
-- that have either or both of these tag keys associated with them.
dcsgTagKeys :: Lens' DescribeClusterSecurityGroups [Text]
dcsgTagKeys = lens _dcsgTagKeys (\s a -> s { _dcsgTagKeys = a }) . _List

-- | A tag value or values for which you want to return all matching cluster
-- security groups that are associated with the specified tag value or
-- values. For example, suppose that you have security groups that are
-- tagged with values called admin and test. If you specify both of these
-- tag values in the request, Amazon Redshift returns a response with the
-- security groups that have either or both of these tag values associated
-- with them.
dcsgTagValues :: Lens' DescribeClusterSecurityGroups [Text]
dcsgTagValues = lens _dcsgTagValues (\s a -> s { _dcsgTagValues = a }) . _List

data DescribeClusterSecurityGroupsResponse = DescribeClusterSecurityGroupsResponse
    { _dcsgr1ClusterSecurityGroups :: List "ClusterSecurityGroup" ClusterSecurityGroup
    , _dcsgr1Marker                :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeClusterSecurityGroupsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgr1ClusterSecurityGroups' @::@ ['ClusterSecurityGroup']
--
-- * 'dcsgr1Marker' @::@ 'Maybe' 'Text'
--
describeClusterSecurityGroupsResponse :: DescribeClusterSecurityGroupsResponse
describeClusterSecurityGroupsResponse = DescribeClusterSecurityGroupsResponse
    { _dcsgr1Marker                = Nothing
    , _dcsgr1ClusterSecurityGroups = mempty
    }

-- | A list of ClusterSecurityGroup> instances.
dcsgr1ClusterSecurityGroups :: Lens' DescribeClusterSecurityGroupsResponse [ClusterSecurityGroup]
dcsgr1ClusterSecurityGroups =
    lens _dcsgr1ClusterSecurityGroups
        (\s a -> s { _dcsgr1ClusterSecurityGroups = a })
            . _List

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the Marker parameter and retrying the command. If the
-- Marker field is empty, all response records have been retrieved for the
-- request.
dcsgr1Marker :: Lens' DescribeClusterSecurityGroupsResponse (Maybe Text)
dcsgr1Marker = lens _dcsgr1Marker (\s a -> s { _dcsgr1Marker = a })

instance ToPath DescribeClusterSecurityGroups where
    toPath = const "/"

instance ToQuery DescribeClusterSecurityGroups where
    toQuery DescribeClusterSecurityGroups{..} = mconcat
        [ "ClusterSecurityGroupName" =? _dcsgClusterSecurityGroupName
        , "Marker"                   =? _dcsgMarker
        , "MaxRecords"               =? _dcsgMaxRecords
        , "TagKeys"                  =? _dcsgTagKeys
        , "TagValues"                =? _dcsgTagValues
        ]

instance ToHeaders DescribeClusterSecurityGroups

instance AWSRequest DescribeClusterSecurityGroups where
    type Sv DescribeClusterSecurityGroups = Redshift
    type Rs DescribeClusterSecurityGroups = DescribeClusterSecurityGroupsResponse

    request  = post "DescribeClusterSecurityGroups"
    response = xmlResponse

instance FromXML DescribeClusterSecurityGroupsResponse where
    parseXML = withElement "DescribeClusterSecurityGroupsResult" $ \x -> DescribeClusterSecurityGroupsResponse
        <$> x .@  "ClusterSecurityGroups"
        <*> x .@? "Marker"

instance AWSPager DescribeClusterSecurityGroups where
    page rq rs
        | stop (rq ^. dcsgMarker) = Nothing
        | otherwise = (\x -> rq & dcsgMarker ?~ x)
            <$> (rs ^. dcsgr1Marker)

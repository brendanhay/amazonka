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

-- Module      : Network.AWS.Redshift.DescribeClusterVersions
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

-- | Returns descriptions of the available Amazon Redshift cluster versions. You
-- can call this operation even before creating any clusters to learn more about
-- the Amazon Redshift versions. For more information about managing clusters,
-- go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters> in the /Amazon Redshift Cluster Management Guide/
--
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeClusterVersions.html>
module Network.AWS.Redshift.DescribeClusterVersions
    (
    -- * Request
      DescribeClusterVersions
    -- ** Request constructor
    , describeClusterVersions
    -- ** Request lenses
    , dcvClusterParameterGroupFamily
    , dcvClusterVersion
    , dcvMarker
    , dcvMaxRecords

    -- * Response
    , DescribeClusterVersionsResponse
    -- ** Response constructor
    , describeClusterVersionsResponse
    -- ** Response lenses
    , dcvrClusterVersions
    , dcvrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data DescribeClusterVersions = DescribeClusterVersions
    { _dcvClusterParameterGroupFamily :: Maybe Text
    , _dcvClusterVersion              :: Maybe Text
    , _dcvMarker                      :: Maybe Text
    , _dcvMaxRecords                  :: Maybe Int
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeClusterVersions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcvClusterParameterGroupFamily' @::@ 'Maybe' 'Text'
--
-- * 'dcvClusterVersion' @::@ 'Maybe' 'Text'
--
-- * 'dcvMarker' @::@ 'Maybe' 'Text'
--
-- * 'dcvMaxRecords' @::@ 'Maybe' 'Int'
--
describeClusterVersions :: DescribeClusterVersions
describeClusterVersions = DescribeClusterVersions
    { _dcvClusterVersion              = Nothing
    , _dcvClusterParameterGroupFamily = Nothing
    , _dcvMaxRecords                  = Nothing
    , _dcvMarker                      = Nothing
    }

-- | The name of a specific cluster parameter group family to return details for.
--
-- Constraints:
--
-- Must be 1 to 255 alphanumeric characters First character must be a letter Cannot end with a hyphen or contain two consecutive hyphens
--
dcvClusterParameterGroupFamily :: Lens' DescribeClusterVersions (Maybe Text)
dcvClusterParameterGroupFamily =
    lens _dcvClusterParameterGroupFamily
        (\s a -> s { _dcvClusterParameterGroupFamily = a })

-- | The specific cluster version to return.
--
-- Example: '1.0'
dcvClusterVersion :: Lens' DescribeClusterVersions (Maybe Text)
dcvClusterVersion =
    lens _dcvClusterVersion (\s a -> s { _dcvClusterVersion = a })

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a 'DescribeClusterVersions' request
-- exceed the value specified in 'MaxRecords', AWS returns a value in the 'Marker'
-- field of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the 'Marker' parameter and retrying the
-- request.
dcvMarker :: Lens' DescribeClusterVersions (Maybe Text)
dcvMarker = lens _dcvMarker (\s a -> s { _dcvMarker = a })

-- | The maximum number of response records to return in each call. If the number
-- of remaining response records exceeds the specified 'MaxRecords' value, a value
-- is returned in a 'marker' field of the response. You can retrieve the next set
-- of records by retrying the command with the returned marker value.
--
-- Default: '100'
--
-- Constraints: minimum 20, maximum 100.
dcvMaxRecords :: Lens' DescribeClusterVersions (Maybe Int)
dcvMaxRecords = lens _dcvMaxRecords (\s a -> s { _dcvMaxRecords = a })

data DescribeClusterVersionsResponse = DescribeClusterVersionsResponse
    { _dcvrClusterVersions :: List "member" ClusterVersion
    , _dcvrMarker          :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DescribeClusterVersionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcvrClusterVersions' @::@ ['ClusterVersion']
--
-- * 'dcvrMarker' @::@ 'Maybe' 'Text'
--
describeClusterVersionsResponse :: DescribeClusterVersionsResponse
describeClusterVersionsResponse = DescribeClusterVersionsResponse
    { _dcvrMarker          = Nothing
    , _dcvrClusterVersions = mempty
    }

-- | A list of 'Version' elements.
dcvrClusterVersions :: Lens' DescribeClusterVersionsResponse [ClusterVersion]
dcvrClusterVersions =
    lens _dcvrClusterVersions (\s a -> s { _dcvrClusterVersions = a })
        . _List

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker value
-- in the 'Marker' parameter and retrying the command. If the 'Marker' field is
-- empty, all response records have been retrieved for the request.
dcvrMarker :: Lens' DescribeClusterVersionsResponse (Maybe Text)
dcvrMarker = lens _dcvrMarker (\s a -> s { _dcvrMarker = a })

instance ToPath DescribeClusterVersions where
    toPath = const "/"

instance ToQuery DescribeClusterVersions where
    toQuery DescribeClusterVersions{..} = mconcat
        [ "ClusterParameterGroupFamily" =? _dcvClusterParameterGroupFamily
        , "ClusterVersion"              =? _dcvClusterVersion
        , "Marker"                      =? _dcvMarker
        , "MaxRecords"                  =? _dcvMaxRecords
        ]

instance ToHeaders DescribeClusterVersions

instance AWSRequest DescribeClusterVersions where
    type Sv DescribeClusterVersions = Redshift
    type Rs DescribeClusterVersions = DescribeClusterVersionsResponse

    request  = post "DescribeClusterVersions"
    response = xmlResponse

instance FromXML DescribeClusterVersionsResponse where
    parseXML = withElement "DescribeClusterVersionsResult" $ \x -> DescribeClusterVersionsResponse
        <$> x .@? "ClusterVersions" .!@ mempty
        <*> x .@? "Marker"

instance AWSPager DescribeClusterVersions where
    page rq rs
        | stop (rs ^. dcvrMarker) = Nothing
        | otherwise = (\x -> rq & dcvMarker ?~ x)
            <$> (rs ^. dcvrMarker)

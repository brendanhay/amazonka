{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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

-- | Returns descriptions of the available Amazon Redshift cluster versions. You
-- can call this operation even before creating any clusters to learn more
-- about the Amazon Redshift versions. For more information about managing
-- clusters, go to Amazon Redshift Clusters in the Amazon Redshift Management
-- Guide https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeClusterVersions &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121207/us-east-1/redshift/aws4_request
-- &x-amz-date=20121207T230708Z
-- &x-amz-signedheaders=content-type;host;x-amz-date redshift-1.0 Initial
-- release of redshift 1.0 d39cd5e5-40c2-11e2-8a25-eb010998df4e.
module Network.AWS.Redshift.DescribeClusterVersions
    (
    -- * Request
      DescribeClusterVersions
    -- ** Request constructor
    , describeClusterVersions
    -- ** Request lenses
    , dcvClusterVersion
    , dcvClusterParameterGroupFamily
    , dcvMaxRecords
    , dcvMarker

    -- * Response
    , DescribeClusterVersionsResponse
    -- ** Response constructor
    , describeClusterVersionsResponse
    -- ** Response lenses
    , dcvrMarker
    , dcvrClusterVersions
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

data DescribeClusterVersions = DescribeClusterVersions
    { _dcvClusterVersion :: Maybe Text
    , _dcvClusterParameterGroupFamily :: Maybe Text
    , _dcvMaxRecords :: Maybe Integer
    , _dcvMarker :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeClusterVersions' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ClusterVersion ::@ @Maybe Text@
--
-- * @ClusterParameterGroupFamily ::@ @Maybe Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
describeClusterVersions :: DescribeClusterVersions
describeClusterVersions = DescribeClusterVersions
    { _dcvClusterVersion = Nothing
    , _dcvClusterParameterGroupFamily = Nothing
    , _dcvMaxRecords = Nothing
    , _dcvMarker = Nothing
    }

-- | The specific cluster version to return. Example: 1.0.
dcvClusterVersion :: Lens' DescribeClusterVersions (Maybe Text)
dcvClusterVersion =
    lens _dcvClusterVersion (\s a -> s { _dcvClusterVersion = a })

-- | The name of a specific cluster parameter group family to return details
-- for. Constraints: Must be 1 to 255 alphanumeric characters First character
-- must be a letter Cannot end with a hyphen or contain two consecutive
-- hyphens.
dcvClusterParameterGroupFamily :: Lens' DescribeClusterVersions (Maybe Text)
dcvClusterParameterGroupFamily =
    lens _dcvClusterParameterGroupFamily
         (\s a -> s { _dcvClusterParameterGroupFamily = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the returned
-- marker value. Default: 100 Constraints: minimum 20, maximum 100.
dcvMaxRecords :: Lens' DescribeClusterVersions (Maybe Integer)
dcvMaxRecords = lens _dcvMaxRecords (\s a -> s { _dcvMaxRecords = a })

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a DescribeClusterVersions request
-- exceed the value specified in MaxRecords, AWS returns a value in the Marker
-- field of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the Marker parameter and retrying
-- the request.
dcvMarker :: Lens' DescribeClusterVersions (Maybe Text)
dcvMarker = lens _dcvMarker (\s a -> s { _dcvMarker = a })

instance ToQuery DescribeClusterVersions where
    toQuery = genericQuery def

-- | Contains the output from the DescribeClusterVersions action.
data DescribeClusterVersionsResponse = DescribeClusterVersionsResponse
    { _dcvrMarker :: Maybe Text
    , _dcvrClusterVersions :: [ClusterVersion]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeClusterVersionsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Marker ::@ @Maybe Text@
--
-- * @ClusterVersions ::@ @[ClusterVersion]@
--
describeClusterVersionsResponse :: DescribeClusterVersionsResponse
describeClusterVersionsResponse = DescribeClusterVersionsResponse
    { _dcvrMarker = Nothing
    , _dcvrClusterVersions = mempty
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
dcvrMarker :: Lens' DescribeClusterVersionsResponse (Maybe Text)
dcvrMarker = lens _dcvrMarker (\s a -> s { _dcvrMarker = a })

-- | A list of Version elements.
dcvrClusterVersions :: Lens' DescribeClusterVersionsResponse [ClusterVersion]
dcvrClusterVersions =
    lens _dcvrClusterVersions (\s a -> s { _dcvrClusterVersions = a })

instance FromXML DescribeClusterVersionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeClusterVersions where
    type Sv DescribeClusterVersions = Redshift
    type Rs DescribeClusterVersions = DescribeClusterVersionsResponse

    request = post "DescribeClusterVersions"
    response _ = xmlResponse

instance AWSPager DescribeClusterVersions where
    next rq rs = (\x -> rq & dcvMarker ?~ x)
        <$> (rs ^. dcvrMarker)

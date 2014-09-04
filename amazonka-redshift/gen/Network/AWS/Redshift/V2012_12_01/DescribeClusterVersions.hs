{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DescribeClusterVersions
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
module Network.AWS.Redshift.V2012_12_01.DescribeClusterVersions
    (
    -- * Request
      DescribeClusterVersions
    -- ** Request constructor
    , mkDescribeClusterVersionsMessage
    -- ** Request lenses
    , dcvmClusterVersion
    , dcvmClusterParameterGroupFamily
    , dcvmMaxRecords
    , dcvmMarker

    -- * Response
    , DescribeClusterVersionsResponse
    -- ** Response lenses
    , cvmMarker
    , cvmClusterVersions
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeClusterVersions' request.
mkDescribeClusterVersionsMessage :: DescribeClusterVersions
mkDescribeClusterVersionsMessage = DescribeClusterVersions
    { _dcvmClusterVersion = Nothing
    , _dcvmClusterParameterGroupFamily = Nothing
    , _dcvmMaxRecords = Nothing
    , _dcvmMarker = Nothing
    }
{-# INLINE mkDescribeClusterVersionsMessage #-}

data DescribeClusterVersions = DescribeClusterVersions
    { _dcvmClusterVersion :: Maybe Text
      -- ^ The specific cluster version to return. Example: 1.0.
    , _dcvmClusterParameterGroupFamily :: Maybe Text
      -- ^ The name of a specific cluster parameter group family to return
      -- details for. Constraints: Must be 1 to 255 alphanumeric
      -- characters First character must be a letter Cannot end with a
      -- hyphen or contain two consecutive hyphens.
    , _dcvmMaxRecords :: Maybe Integer
      -- ^ The maximum number of response records to return in each call. If
      -- the number of remaining response records exceeds the specified
      -- MaxRecords value, a value is returned in a marker field of the
      -- response. You can retrieve the next set of records by retrying
      -- the command with the returned marker value. Default: 100
      -- Constraints: minimum 20, maximum 100.
    , _dcvmMarker :: Maybe Text
      -- ^ An optional parameter that specifies the starting point to return
      -- a set of response records. When the results of a
      -- DescribeClusterVersions request exceed the value specified in
      -- MaxRecords, AWS returns a value in the Marker field of the
      -- response. You can retrieve the next set of response records by
      -- providing the returned marker value in the Marker parameter and
      -- retrying the request.
    } deriving (Show, Generic)

-- | The specific cluster version to return. Example: 1.0.
dcvmClusterVersion :: Lens' DescribeClusterVersions (Maybe Text)
dcvmClusterVersion = lens _dcvmClusterVersion (\s a -> s { _dcvmClusterVersion = a })
{-# INLINE dcvmClusterVersion #-}

-- | The name of a specific cluster parameter group family to return details
-- for. Constraints: Must be 1 to 255 alphanumeric characters First character
-- must be a letter Cannot end with a hyphen or contain two consecutive
-- hyphens.
dcvmClusterParameterGroupFamily :: Lens' DescribeClusterVersions (Maybe Text)
dcvmClusterParameterGroupFamily = lens _dcvmClusterParameterGroupFamily (\s a -> s { _dcvmClusterParameterGroupFamily = a })
{-# INLINE dcvmClusterParameterGroupFamily #-}

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the returned
-- marker value. Default: 100 Constraints: minimum 20, maximum 100.
dcvmMaxRecords :: Lens' DescribeClusterVersions (Maybe Integer)
dcvmMaxRecords = lens _dcvmMaxRecords (\s a -> s { _dcvmMaxRecords = a })
{-# INLINE dcvmMaxRecords #-}

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a DescribeClusterVersions request
-- exceed the value specified in MaxRecords, AWS returns a value in the Marker
-- field of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the Marker parameter and retrying
-- the request.
dcvmMarker :: Lens' DescribeClusterVersions (Maybe Text)
dcvmMarker = lens _dcvmMarker (\s a -> s { _dcvmMarker = a })
{-# INLINE dcvmMarker #-}

instance ToQuery DescribeClusterVersions where
    toQuery = genericQuery def

data DescribeClusterVersionsResponse = DescribeClusterVersionsResponse
    { _cvmMarker :: Maybe Text
      -- ^ A value that indicates the starting point for the next set of
      -- response records in a subsequent request. If a value is returned
      -- in a response, you can retrieve the next set of records by
      -- providing this returned marker value in the Marker parameter and
      -- retrying the command. If the Marker field is empty, all response
      -- records have been retrieved for the request.
    , _cvmClusterVersions :: [ClusterVersion]
      -- ^ A list of Version elements.
    } deriving (Show, Generic)

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
cvmMarker :: Lens' DescribeClusterVersionsResponse (Maybe Text)
cvmMarker = lens _cvmMarker (\s a -> s { _cvmMarker = a })
{-# INLINE cvmMarker #-}

-- | A list of Version elements.
cvmClusterVersions :: Lens' DescribeClusterVersionsResponse ([ClusterVersion])
cvmClusterVersions = lens _cvmClusterVersions (\s a -> s { _cvmClusterVersions = a })
{-# INLINE cvmClusterVersions #-}

instance FromXML DescribeClusterVersionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeClusterVersions where
    type Sv DescribeClusterVersions = Redshift
    type Rs DescribeClusterVersions = DescribeClusterVersionsResponse

    request = post "DescribeClusterVersions"
    response _ = xmlResponse

instance AWSPager DescribeClusterVersions where
    next rq rs = (\x -> rq { _dcvmMarker = Just x })
        <$> (_cvmMarker rs)

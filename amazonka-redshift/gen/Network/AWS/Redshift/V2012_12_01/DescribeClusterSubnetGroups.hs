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
    , mkDescribeClusterSubnetGroupsMessage
    -- ** Request lenses
    , dcsgpClusterSubnetGroupName
    , dcsgpMaxRecords
    , dcsgpMarker

    -- * Response
    , DescribeClusterSubnetGroupsResponse
    -- ** Response lenses
    , csgpMarker
    , csgpClusterSubnetGroups
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeClusterSubnetGroups' request.
mkDescribeClusterSubnetGroupsMessage :: DescribeClusterSubnetGroups
mkDescribeClusterSubnetGroupsMessage = DescribeClusterSubnetGroups
    { _dcsgpClusterSubnetGroupName = Nothing
    , _dcsgpMaxRecords = Nothing
    , _dcsgpMarker = Nothing
    }
{-# INLINE mkDescribeClusterSubnetGroupsMessage #-}

data DescribeClusterSubnetGroups = DescribeClusterSubnetGroups
    { _dcsgpClusterSubnetGroupName :: Maybe Text
      -- ^ The name of the cluster subnet group for which information is
      -- requested.
    , _dcsgpMaxRecords :: Maybe Integer
      -- ^ The maximum number of response records to return in each call. If
      -- the number of remaining response records exceeds the specified
      -- MaxRecords value, a value is returned in a marker field of the
      -- response. You can retrieve the next set of records by retrying
      -- the command with the returned marker value. Default: 100
      -- Constraints: minimum 20, maximum 100.
    , _dcsgpMarker :: Maybe Text
      -- ^ An optional parameter that specifies the starting point to return
      -- a set of response records. When the results of a
      -- DescribeClusterSubnetGroups request exceed the value specified in
      -- MaxRecords, AWS returns a value in the Marker field of the
      -- response. You can retrieve the next set of response records by
      -- providing the returned marker value in the Marker parameter and
      -- retrying the request.
    } deriving (Show, Generic)

-- | The name of the cluster subnet group for which information is requested.
dcsgpClusterSubnetGroupName :: Lens' DescribeClusterSubnetGroups (Maybe Text)
dcsgpClusterSubnetGroupName = lens _dcsgpClusterSubnetGroupName (\s a -> s { _dcsgpClusterSubnetGroupName = a })
{-# INLINE dcsgpClusterSubnetGroupName #-}

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the returned
-- marker value. Default: 100 Constraints: minimum 20, maximum 100.
dcsgpMaxRecords :: Lens' DescribeClusterSubnetGroups (Maybe Integer)
dcsgpMaxRecords = lens _dcsgpMaxRecords (\s a -> s { _dcsgpMaxRecords = a })
{-# INLINE dcsgpMaxRecords #-}

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a DescribeClusterSubnetGroups request
-- exceed the value specified in MaxRecords, AWS returns a value in the Marker
-- field of the response. You can retrieve the next set of response records by
-- providing the returned marker value in the Marker parameter and retrying
-- the request.
dcsgpMarker :: Lens' DescribeClusterSubnetGroups (Maybe Text)
dcsgpMarker = lens _dcsgpMarker (\s a -> s { _dcsgpMarker = a })
{-# INLINE dcsgpMarker #-}

instance ToQuery DescribeClusterSubnetGroups where
    toQuery = genericQuery def

data DescribeClusterSubnetGroupsResponse = DescribeClusterSubnetGroupsResponse
    { _csgpMarker :: Maybe Text
      -- ^ A value that indicates the starting point for the next set of
      -- response records in a subsequent request. If a value is returned
      -- in a response, you can retrieve the next set of records by
      -- providing this returned marker value in the Marker parameter and
      -- retrying the command. If the Marker field is empty, all response
      -- records have been retrieved for the request.
    , _csgpClusterSubnetGroups :: [ClusterSubnetGroup]
      -- ^ A list of ClusterSubnetGroup instances.
    } deriving (Show, Generic)

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
csgpMarker :: Lens' DescribeClusterSubnetGroupsResponse (Maybe Text)
csgpMarker = lens _csgpMarker (\s a -> s { _csgpMarker = a })
{-# INLINE csgpMarker #-}

-- | A list of ClusterSubnetGroup instances.
csgpClusterSubnetGroups :: Lens' DescribeClusterSubnetGroupsResponse ([ClusterSubnetGroup])
csgpClusterSubnetGroups = lens _csgpClusterSubnetGroups (\s a -> s { _csgpClusterSubnetGroups = a })
{-# INLINE csgpClusterSubnetGroups #-}

instance FromXML DescribeClusterSubnetGroupsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeClusterSubnetGroups where
    type Sv DescribeClusterSubnetGroups = Redshift
    type Rs DescribeClusterSubnetGroups = DescribeClusterSubnetGroupsResponse

    request = post "DescribeClusterSubnetGroups"
    response _ = xmlResponse

instance AWSPager DescribeClusterSubnetGroups where
    next rq rs = (\x -> rq { _dcsgpMarker = Just x })
        <$> (_csgpMarker rs)

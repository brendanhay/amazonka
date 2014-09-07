{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DescribeOrderableClusterOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of orderable cluster options. Before you create a new
-- cluster you can use this operation to find what options are available, such
-- as the EC2 Availability Zones (AZ) in the specific AWS region that you can
-- specify, and the node types you can request. The node types differ by
-- available storage, memory, CPU and price. With the cost involved you might
-- want to obtain a list of cluster options in the specific region and specify
-- values when creating a cluster. For more information about managing
-- clusters, go to Amazon Redshift Clusters in the Amazon Redshift Management
-- Guide https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeOrderableClusterOptions &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121207/us-east-1/redshift/aws4_request
-- &x-amz-date=20121207T225314Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 1.0 multi-node
-- dw1.8xlarge us-east-1a us-east-1c us-east-1d 1.0 multi-node dw1.xlarge
-- us-east-1a us-east-1c us-east-1d 1.0 single-node dw1.xlarge us-east-1a
-- us-east-1c us-east-1d e37414cc-40c0-11e2-b6a0-df98b1a86860.
module Network.AWS.Redshift.V2012_12_01.DescribeOrderableClusterOptions
    (
    -- * Request
      DescribeOrderableClusterOptions
    -- ** Request constructor
    , mkDescribeOrderableClusterOptions
    -- ** Request lenses
    , docoClusterVersion
    , docoNodeType
    , docoMaxRecords
    , docoMarker

    -- * Response
    , DescribeOrderableClusterOptionsResponse
    -- ** Response lenses
    , docorsOrderableClusterOptions
    , docorsMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | 
data DescribeOrderableClusterOptions = DescribeOrderableClusterOptions
    { _docoClusterVersion :: Maybe Text
    , _docoNodeType :: Maybe Text
    , _docoMaxRecords :: Maybe Integer
    , _docoMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeOrderableClusterOptions' request.
mkDescribeOrderableClusterOptions :: DescribeOrderableClusterOptions
mkDescribeOrderableClusterOptions = DescribeOrderableClusterOptions
    { _docoClusterVersion = Nothing
    , _docoNodeType = Nothing
    , _docoMaxRecords = Nothing
    , _docoMarker = Nothing
    }

-- | The version filter value. Specify this parameter to show only the available
-- offerings matching the specified version. Default: All versions.
-- Constraints: Must be one of the version returned from
-- DescribeClusterVersions.
docoClusterVersion :: Lens' DescribeOrderableClusterOptions (Maybe Text)
docoClusterVersion =
    lens _docoClusterVersion (\s a -> s { _docoClusterVersion = a })

-- | The node type filter value. Specify this parameter to show only the
-- available offerings matching the specified node type.
docoNodeType :: Lens' DescribeOrderableClusterOptions (Maybe Text)
docoNodeType = lens _docoNodeType (\s a -> s { _docoNodeType = a })

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the returned
-- marker value. Default: 100 Constraints: minimum 20, maximum 100.
docoMaxRecords :: Lens' DescribeOrderableClusterOptions (Maybe Integer)
docoMaxRecords = lens _docoMaxRecords (\s a -> s { _docoMaxRecords = a })

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a DescribeOrderableClusterOptions
-- request exceed the value specified in MaxRecords, AWS returns a value in
-- the Marker field of the response. You can retrieve the next set of response
-- records by providing the returned marker value in the Marker parameter and
-- retrying the request.
docoMarker :: Lens' DescribeOrderableClusterOptions (Maybe Text)
docoMarker = lens _docoMarker (\s a -> s { _docoMarker = a })

instance ToQuery DescribeOrderableClusterOptions where
    toQuery = genericQuery def

-- | Contains the output from the DescribeOrderableClusterOptions action.
data DescribeOrderableClusterOptionsResponse = DescribeOrderableClusterOptionsResponse
    { _docorsOrderableClusterOptions :: [OrderableClusterOption]
    , _docorsMarker :: Maybe Text
    } deriving (Show, Generic)

-- | An OrderableClusterOption structure containing information about orderable
-- options for the Cluster.
docorsOrderableClusterOptions :: Lens' DescribeOrderableClusterOptionsResponse [OrderableClusterOption]
docorsOrderableClusterOptions =
    lens _docorsOrderableClusterOptions
         (\s a -> s { _docorsOrderableClusterOptions = a })

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
docorsMarker :: Lens' DescribeOrderableClusterOptionsResponse (Maybe Text)
docorsMarker = lens _docorsMarker (\s a -> s { _docorsMarker = a })

instance FromXML DescribeOrderableClusterOptionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeOrderableClusterOptions where
    type Sv DescribeOrderableClusterOptions = Redshift
    type Rs DescribeOrderableClusterOptions = DescribeOrderableClusterOptionsResponse

    request = post "DescribeOrderableClusterOptions"
    response _ = xmlResponse

instance AWSPager DescribeOrderableClusterOptions where
    next rq rs = (\x -> rq & docoMarker ?~ x) <$> (rs ^. docorsMarker)


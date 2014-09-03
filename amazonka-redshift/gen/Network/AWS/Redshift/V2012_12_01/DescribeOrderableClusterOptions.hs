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
    , describeOrderableClusterOptions
    -- ** Request lenses
    , docomMaxRecords
    , docomClusterVersion
    , docomNodeType
    , docomMarker

    -- * Response
    , DescribeOrderableClusterOptionsResponse
    -- ** Response lenses
    , ocomOrderableClusterOptions
    , ocomMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeOrderableClusterOptions' request.
describeOrderableClusterOptions :: DescribeOrderableClusterOptions
describeOrderableClusterOptions = DescribeOrderableClusterOptions
    { _docomMaxRecords = Nothing
    , _docomClusterVersion = Nothing
    , _docomNodeType = Nothing
    , _docomMarker = Nothing
    }

data DescribeOrderableClusterOptions = DescribeOrderableClusterOptions
    { _docomMaxRecords :: Maybe Integer
      -- ^ The maximum number of response records to return in each call. If
      -- the number of remaining response records exceeds the specified
      -- MaxRecords value, a value is returned in a marker field of the
      -- response. You can retrieve the next set of records by retrying
      -- the command with the returned marker value. Default: 100
      -- Constraints: minimum 20, maximum 100.
    , _docomClusterVersion :: Maybe Text
      -- ^ The version filter value. Specify this parameter to show only the
      -- available offerings matching the specified version. Default: All
      -- versions. Constraints: Must be one of the version returned from
      -- DescribeClusterVersions.
    , _docomNodeType :: Maybe Text
      -- ^ The node type filter value. Specify this parameter to show only
      -- the available offerings matching the specified node type.
    , _docomMarker :: Maybe Text
      -- ^ An optional parameter that specifies the starting point to return
      -- a set of response records. When the results of a
      -- DescribeOrderableClusterOptions request exceed the value
      -- specified in MaxRecords, AWS returns a value in the Marker field
      -- of the response. You can retrieve the next set of response
      -- records by providing the returned marker value in the Marker
      -- parameter and retrying the request.
    } deriving (Show, Generic)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified MaxRecords
-- value, a value is returned in a marker field of the response. You can
-- retrieve the next set of records by retrying the command with the returned
-- marker value. Default: 100 Constraints: minimum 20, maximum 100.
docomMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeOrderableClusterOptions
    -> f DescribeOrderableClusterOptions
docomMaxRecords f x =
    (\y -> x { _docomMaxRecords = y })
       <$> f (_docomMaxRecords x)
{-# INLINE docomMaxRecords #-}

-- | The version filter value. Specify this parameter to show only the available
-- offerings matching the specified version. Default: All versions.
-- Constraints: Must be one of the version returned from
-- DescribeClusterVersions.
docomClusterVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeOrderableClusterOptions
    -> f DescribeOrderableClusterOptions
docomClusterVersion f x =
    (\y -> x { _docomClusterVersion = y })
       <$> f (_docomClusterVersion x)
{-# INLINE docomClusterVersion #-}

-- | The node type filter value. Specify this parameter to show only the
-- available offerings matching the specified node type.
docomNodeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeOrderableClusterOptions
    -> f DescribeOrderableClusterOptions
docomNodeType f x =
    (\y -> x { _docomNodeType = y })
       <$> f (_docomNodeType x)
{-# INLINE docomNodeType #-}

-- | An optional parameter that specifies the starting point to return a set of
-- response records. When the results of a DescribeOrderableClusterOptions
-- request exceed the value specified in MaxRecords, AWS returns a value in
-- the Marker field of the response. You can retrieve the next set of response
-- records by providing the returned marker value in the Marker parameter and
-- retrying the request.
docomMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeOrderableClusterOptions
    -> f DescribeOrderableClusterOptions
docomMarker f x =
    (\y -> x { _docomMarker = y })
       <$> f (_docomMarker x)
{-# INLINE docomMarker #-}

instance ToQuery DescribeOrderableClusterOptions where
    toQuery = genericQuery def

data DescribeOrderableClusterOptionsResponse = DescribeOrderableClusterOptionsResponse
    { _ocomOrderableClusterOptions :: [OrderableClusterOption]
      -- ^ An OrderableClusterOption structure containing information about
      -- orderable options for the Cluster.
    , _ocomMarker :: Maybe Text
      -- ^ A value that indicates the starting point for the next set of
      -- response records in a subsequent request. If a value is returned
      -- in a response, you can retrieve the next set of records by
      -- providing this returned marker value in the Marker parameter and
      -- retrying the command. If the Marker field is empty, all response
      -- records have been retrieved for the request.
    } deriving (Show, Generic)

-- | An OrderableClusterOption structure containing information about orderable
-- options for the Cluster.
ocomOrderableClusterOptions
    :: Functor f
    => ([OrderableClusterOption]
    -> f ([OrderableClusterOption]))
    -> DescribeOrderableClusterOptionsResponse
    -> f DescribeOrderableClusterOptionsResponse
ocomOrderableClusterOptions f x =
    (\y -> x { _ocomOrderableClusterOptions = y })
       <$> f (_ocomOrderableClusterOptions x)
{-# INLINE ocomOrderableClusterOptions #-}

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response, you
-- can retrieve the next set of records by providing this returned marker
-- value in the Marker parameter and retrying the command. If the Marker field
-- is empty, all response records have been retrieved for the request.
ocomMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeOrderableClusterOptionsResponse
    -> f DescribeOrderableClusterOptionsResponse
ocomMarker f x =
    (\y -> x { _ocomMarker = y })
       <$> f (_ocomMarker x)
{-# INLINE ocomMarker #-}

instance FromXML DescribeOrderableClusterOptionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeOrderableClusterOptions where
    type Sv DescribeOrderableClusterOptions = Redshift
    type Rs DescribeOrderableClusterOptions = DescribeOrderableClusterOptionsResponse

    request = post "DescribeOrderableClusterOptions"
    response _ = xmlResponse

instance AWSPager DescribeOrderableClusterOptions where
    next rq rs = (\x -> rq { _docomMarker = Just x })
        <$> (_ocomMarker rs)

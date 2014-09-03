{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeRegions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more regions that are currently available to you. For a
-- list of the regions supported by Amazon EC2, see Regions and Endpoints.
-- Example 1 This example displays information about all regions.
-- https://ec2.amazonaws.com/?Action=DescribeRegions &amp;AUTHPARAMS Example 2
-- This example displays information about the specified regions only.
-- https://ec2.amazonaws.com/?Action=DescribeRegions
-- &amp;RegionName.1=us-east-1 &amp;RegionName.2=eu-west-1 &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE us-east-1 ec2.us-east-1.amazonaws.com
-- eu-west-1 ec2.eu-west-1amazonaws.com.
module Network.AWS.EC2.V2014_06_15.DescribeRegions
    (
    -- * Request
      DescribeRegions
    -- ** Request constructor
    , describeRegions
    -- ** Request lenses
    , drsFilters
    , drsRegionNames

    -- * Response
    , DescribeRegionsResponse
    -- ** Response lenses
    , drtRegions
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeRegions' request.
describeRegions :: DescribeRegions
describeRegions = DescribeRegions
    { _drsFilters = mempty
    , _drsRegionNames = mempty
    }

data DescribeRegions = DescribeRegions
    { _drsFilters :: [Filter]
      -- ^ One or more filters. endpoint - The endpoint of the region (for
      -- example, ec2.us-east-1.amazonaws.com). region-name - The name of
      -- the region (for example, us-east-1).
    , _drsRegionNames :: [Text]
      -- ^ The names of one or more regions.
    } deriving (Show, Generic)

-- | One or more filters. endpoint - The endpoint of the region (for example,
-- ec2.us-east-1.amazonaws.com). region-name - The name of the region (for
-- example, us-east-1).
drsFilters
    :: Functor f
    => ([Filter]
    -> f ([Filter]))
    -> DescribeRegions
    -> f DescribeRegions
drsFilters f x =
    (\y -> x { _drsFilters = y })
       <$> f (_drsFilters x)
{-# INLINE drsFilters #-}

-- | The names of one or more regions.
drsRegionNames
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeRegions
    -> f DescribeRegions
drsRegionNames f x =
    (\y -> x { _drsRegionNames = y })
       <$> f (_drsRegionNames x)
{-# INLINE drsRegionNames #-}

instance ToQuery DescribeRegions where
    toQuery = genericQuery def

data DescribeRegionsResponse = DescribeRegionsResponse
    { _drtRegions :: [Region]
      -- ^ Information about one or more regions.
    } deriving (Show, Generic)

-- | Information about one or more regions.
drtRegions
    :: Functor f
    => ([Region]
    -> f ([Region]))
    -> DescribeRegionsResponse
    -> f DescribeRegionsResponse
drtRegions f x =
    (\y -> x { _drtRegions = y })
       <$> f (_drtRegions x)
{-# INLINE drtRegions #-}

instance FromXML DescribeRegionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeRegions where
    type Sv DescribeRegions = EC2
    type Rs DescribeRegions = DescribeRegionsResponse

    request = post "DescribeRegions"
    response _ = xmlResponse

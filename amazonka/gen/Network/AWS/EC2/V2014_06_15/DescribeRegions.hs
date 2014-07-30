{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.EC2.V2014_06_15.DescribeRegions where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Region, Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_06_15.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'DescribeRegions' request.
describeRegions :: DescribeRegions
describeRegions = DescribeRegions
    { _drsDryRun = Nothing
    , _drsFilters = mempty
    , _drsRegionNames = mempty
    }

data DescribeRegions = DescribeRegions
    { _drsDryRun :: Maybe Bool
      -- ^ 
    , _drsFilters :: [Filter]
      -- ^ One or more filters. endpoint - The endpoint of the region (for
      -- example, ec2.us-east-1.amazonaws.com). region-name - The name of
      -- the region (for example, us-east-1).
    , _drsRegionNames :: [Text]
      -- ^ The names of one or more regions.
    } deriving (Generic)

instance ToQuery DescribeRegions where
    toQuery = genericToQuery def

instance AWSRequest DescribeRegions where
    type Sv DescribeRegions = EC2
    type Rs DescribeRegions = DescribeRegionsResponse

    request = post "DescribeRegions"
    response _ = xmlResponse

data DescribeRegionsResponse = DescribeRegionsResponse
    { _drtRegions :: [Region]
      -- ^ Information about one or more regions.
    } deriving (Generic)

instance FromXML DescribeRegionsResponse where
    fromXMLOptions = xmlOptions

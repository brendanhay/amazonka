{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeRegions
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
module Network.AWS.EC2.DescribeRegions
    (
    -- * Request
      DescribeRegions
    -- ** Request constructor
    , describeRegions
    -- ** Request lenses
    , dr1RegionName
    , dr1Filter

    -- * Response
    , DescribeRegionsResponse
    -- ** Response constructor
    , describeRegionsResponse
    -- ** Response lenses
    , drrItem
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DescribeRegions = DescribeRegions
    { _dr1RegionName :: [Text]
    , _dr1Filter :: [Filter]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeRegions' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RegionName ::@ @[Text]@
--
-- * @Filter ::@ @[Filter]@
--
describeRegions :: DescribeRegions
describeRegions = DescribeRegions
    { _dr1RegionName = mempty
    , _dr1Filter = mempty
    }

-- | The names of one or more regions.
dr1RegionName :: Lens' DescribeRegions [Text]
dr1RegionName = lens _dr1RegionName (\s a -> s { _dr1RegionName = a })

-- | One or more filters. endpoint - The endpoint of the region (for example,
-- ec2.us-east-1.amazonaws.com). region-name - The name of the region (for
-- example, us-east-1).
dr1Filter :: Lens' DescribeRegions [Filter]
dr1Filter = lens _dr1Filter (\s a -> s { _dr1Filter = a })

instance ToQuery DescribeRegions where
    toQuery = genericQuery def

newtype DescribeRegionsResponse = DescribeRegionsResponse
    { _drrItem :: [Region]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeRegionsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Item ::@ @[Region]@
--
describeRegionsResponse :: DescribeRegionsResponse
describeRegionsResponse = DescribeRegionsResponse
    { _drrItem = mempty
    }

-- | Information about one or more regions.
drrItem :: Lens' DescribeRegionsResponse [Region]
drrItem = lens _drrItem (\s a -> s { _drrItem = a })

instance FromXML DescribeRegionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeRegions where
    type Sv DescribeRegions = EC2
    type Rs DescribeRegions = DescribeRegionsResponse

    request = post "DescribeRegions"
    response _ = xmlResponse

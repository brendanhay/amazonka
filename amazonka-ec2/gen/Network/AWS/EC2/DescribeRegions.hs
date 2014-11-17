{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeRegions.html>
module Network.AWS.EC2.DescribeRegions
    (
    -- * Request
      DescribeRegions
    -- ** Request constructor
    , describeRegions
    -- ** Request lenses
    , dr1DryRun
    , dr1Filters
    , dr1RegionNames

    -- * Response
    , DescribeRegionsResponse
    -- ** Response constructor
    , describeRegionsResponse
    -- ** Response lenses
    , drrRegions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeRegions = DescribeRegions
    { _dr1DryRun      :: Maybe Bool
    , _dr1Filters     :: [Filter]
    , _dr1RegionNames :: [Text]
    } deriving (Eq, Show, Generic)

-- | 'DescribeRegions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dr1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dr1Filters' @::@ ['Filter']
--
-- * 'dr1RegionNames' @::@ ['Text']
--
describeRegions :: DescribeRegions
describeRegions = DescribeRegions
    { _dr1DryRun      = Nothing
    , _dr1RegionNames = mempty
    , _dr1Filters     = mempty
    }

dr1DryRun :: Lens' DescribeRegions (Maybe Bool)
dr1DryRun = lens _dr1DryRun (\s a -> s { _dr1DryRun = a })

-- | One or more filters. endpoint - The endpoint of the region (for example,
-- ec2.us-east-1.amazonaws.com). region-name - The name of the region (for
-- example, us-east-1).
dr1Filters :: Lens' DescribeRegions [Filter]
dr1Filters = lens _dr1Filters (\s a -> s { _dr1Filters = a })

-- | The names of one or more regions.
dr1RegionNames :: Lens' DescribeRegions [Text]
dr1RegionNames = lens _dr1RegionNames (\s a -> s { _dr1RegionNames = a })

newtype DescribeRegionsResponse = DescribeRegionsResponse
    { _drrRegions :: [Region]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeRegionsResponse where
    type Item DescribeRegionsResponse = Region

    fromList = DescribeRegionsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _drrRegions

-- | 'DescribeRegionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drrRegions' @::@ ['Region']
--
describeRegionsResponse :: DescribeRegionsResponse
describeRegionsResponse = DescribeRegionsResponse
    { _drrRegions = mempty
    }

-- | Information about one or more regions.
drrRegions :: Lens' DescribeRegionsResponse [Region]
drrRegions = lens _drrRegions (\s a -> s { _drrRegions = a })

instance AWSRequest DescribeRegions where
    type Sv DescribeRegions = EC2
    type Rs DescribeRegions = DescribeRegionsResponse

    request  = post "DescribeRegions"
    response = xmlResponse

instance FromXML DescribeRegionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeRegionsResponse"

instance ToPath DescribeRegions where
    toPath = const "/"

instance ToHeaders DescribeRegions

instance ToQuery DescribeRegions where
    toQuery DescribeRegions{..} = mconcat
        [ "dryRun"     =? _dr1DryRun
        , "RegionName" =? _dr1RegionNames
        , "Filter"     =? _dr1Filters
        ]

instance ToXML DescribeRegions where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "DescribeRegions"

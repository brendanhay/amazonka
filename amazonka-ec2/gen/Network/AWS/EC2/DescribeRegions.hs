{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.EC2.DescribeRegions
    (
    -- * Request
      DescribeRegions
    -- ** Request constructor
    , describeRegions
    -- ** Request lenses
    , drDryRun
    , drFilters
    , drRegionNames

    -- * Response
    , DescribeRegionsResult
    -- ** Response constructor
    , describeRegionsResponse
    -- ** Response lenses
    , drrRegions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DescribeRegions = DescribeRegions
    { _drDryRun      :: Maybe Bool
    , _drFilters     :: [Filter]
    , _drRegionNames :: [Text]
    } deriving (Eq, Show, Generic)

-- | 'DescribeRegions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'drFilters' @::@ ['Filter']
--
-- * 'drRegionNames' @::@ ['Text']
--
describeRegions :: DescribeRegions
describeRegions = DescribeRegions
    { _drDryRun      = Nothing
    , _drRegionNames = mempty
    , _drFilters     = mempty
    }

drDryRun :: Lens' DescribeRegions (Maybe Bool)
drDryRun = lens _drDryRun (\s a -> s { _drDryRun = a })

-- | One or more filters. endpoint - The endpoint of the region (for example,
-- ec2.us-east-1.amazonaws.com). region-name - The name of the region (for
-- example, us-east-1).
drFilters :: Lens' DescribeRegions [Filter]
drFilters = lens _drFilters (\s a -> s { _drFilters = a })

-- | The names of one or more regions.
drRegionNames :: Lens' DescribeRegions [Text]
drRegionNames = lens _drRegionNames (\s a -> s { _drRegionNames = a })

instance ToPath DescribeRegions where
    toPath = const "/"

instance ToQuery DescribeRegions

newtype DescribeRegionsResult = DescribeRegionsResult
    { _drrRegions :: [Region]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'DescribeRegionsResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drrRegions' @::@ ['Region']
--
describeRegionsResponse :: DescribeRegionsResult
describeRegionsResponse = DescribeRegionsResult
    { _drrRegions = mempty
    }

-- | Information about one or more regions.
drrRegions :: Lens' DescribeRegionsResult [Region]
drrRegions = lens _drrRegions (\s a -> s { _drrRegions = a })

instance AWSRequest DescribeRegions where
    type Sv DescribeRegions = EC2
    type Rs DescribeRegions = DescribeRegionsResult

    request  = post "DescribeRegions"
    response = xmlResponse $ \h x -> DescribeRegionsResult
        <$> x %| "regionInfo"

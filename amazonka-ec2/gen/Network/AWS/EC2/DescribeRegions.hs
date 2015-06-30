{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DescribeRegions
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes one or more regions that are currently available to you.
--
-- For a list of the regions supported by Amazon EC2, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html#ec2_region Regions and Endpoints>.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeRegions.html>
module Network.AWS.EC2.DescribeRegions
    (
    -- * Request
      DescribeRegions
    -- ** Request constructor
    , describeRegions
    -- ** Request lenses
    , dr1RegionNames
    , dr1Filters
    , dr1DryRun

    -- * Response
    , DescribeRegionsResponse
    -- ** Response constructor
    , describeRegionsResponse
    -- ** Response lenses
    , drrRegions
    , drrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeRegions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dr1RegionNames'
--
-- * 'dr1Filters'
--
-- * 'dr1DryRun'
data DescribeRegions = DescribeRegions'
    { _dr1RegionNames :: !(Maybe [Text])
    , _dr1Filters     :: !(Maybe [Filter])
    , _dr1DryRun      :: !(Maybe Bool)
    } deriving (Eq,Read,Show)

-- | 'DescribeRegions' smart constructor.
describeRegions :: DescribeRegions
describeRegions =
    DescribeRegions'
    { _dr1RegionNames = Nothing
    , _dr1Filters = Nothing
    , _dr1DryRun = Nothing
    }

-- | The names of one or more regions.
dr1RegionNames :: Lens' DescribeRegions [Text]
dr1RegionNames = lens _dr1RegionNames (\ s a -> s{_dr1RegionNames = a}) . _Default;

-- | One or more filters.
--
-- -   @endpoint@ - The endpoint of the region (for example,
--     @ec2.us-east-1.amazonaws.com@).
--
-- -   @region-name@ - The name of the region (for example, @us-east-1@).
--
dr1Filters :: Lens' DescribeRegions [Filter]
dr1Filters = lens _dr1Filters (\ s a -> s{_dr1Filters = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dr1DryRun :: Lens' DescribeRegions (Maybe Bool)
dr1DryRun = lens _dr1DryRun (\ s a -> s{_dr1DryRun = a});

instance AWSRequest DescribeRegions where
        type Sv DescribeRegions = EC2
        type Rs DescribeRegions = DescribeRegionsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeRegionsResponse' <$>
                   (x .@? "regionInfo" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeRegions where
        toHeaders = const mempty

instance ToPath DescribeRegions where
        toPath = const "/"

instance ToQuery DescribeRegions where
        toQuery DescribeRegions'{..}
          = mconcat
              ["Action" =: ("DescribeRegions" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery
                 (toQueryList "RegionName" <$> _dr1RegionNames),
               toQuery (toQueryList "Filter" <$> _dr1Filters),
               "DryRun" =: _dr1DryRun]

-- | /See:/ 'describeRegionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drrRegions'
--
-- * 'drrStatus'
data DescribeRegionsResponse = DescribeRegionsResponse'
    { _drrRegions :: !(Maybe [RegionInfo])
    , _drrStatus  :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeRegionsResponse' smart constructor.
describeRegionsResponse :: Int -> DescribeRegionsResponse
describeRegionsResponse pStatus =
    DescribeRegionsResponse'
    { _drrRegions = Nothing
    , _drrStatus = pStatus
    }

-- | Information about one or more regions.
drrRegions :: Lens' DescribeRegionsResponse [RegionInfo]
drrRegions = lens _drrRegions (\ s a -> s{_drrRegions = a}) . _Default;

-- | FIXME: Undocumented member.
drrStatus :: Lens' DescribeRegionsResponse Int
drrStatus = lens _drrStatus (\ s a -> s{_drrStatus = a});

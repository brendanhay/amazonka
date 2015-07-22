{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeRegions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more regions that are currently available to you.
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
    , drsrqRegionNames
    , drsrqFilters
    , drsrqDryRun

    -- * Response
    , DescribeRegionsResponse
    -- ** Response constructor
    , describeRegionsResponse
    -- ** Response lenses
    , drrsRegions
    , drrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeRegions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drsrqRegionNames'
--
-- * 'drsrqFilters'
--
-- * 'drsrqDryRun'
data DescribeRegions = DescribeRegions'
    { _drsrqRegionNames :: !(Maybe [Text])
    , _drsrqFilters     :: !(Maybe [Filter])
    , _drsrqDryRun      :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeRegions' smart constructor.
describeRegions :: DescribeRegions
describeRegions =
    DescribeRegions'
    { _drsrqRegionNames = Nothing
    , _drsrqFilters = Nothing
    , _drsrqDryRun = Nothing
    }

-- | The names of one or more regions.
drsrqRegionNames :: Lens' DescribeRegions [Text]
drsrqRegionNames = lens _drsrqRegionNames (\ s a -> s{_drsrqRegionNames = a}) . _Default;

-- | One or more filters.
--
-- -   @endpoint@ - The endpoint of the region (for example,
--     @ec2.us-east-1.amazonaws.com@).
--
-- -   @region-name@ - The name of the region (for example, @us-east-1@).
--
drsrqFilters :: Lens' DescribeRegions [Filter]
drsrqFilters = lens _drsrqFilters (\ s a -> s{_drsrqFilters = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
drsrqDryRun :: Lens' DescribeRegions (Maybe Bool)
drsrqDryRun = lens _drsrqDryRun (\ s a -> s{_drsrqDryRun = a});

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
                 (toQueryList "RegionName" <$> _drsrqRegionNames),
               toQuery (toQueryList "Filter" <$> _drsrqFilters),
               "DryRun" =: _drsrqDryRun]

-- | /See:/ 'describeRegionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drrsRegions'
--
-- * 'drrsStatus'
data DescribeRegionsResponse = DescribeRegionsResponse'
    { _drrsRegions :: !(Maybe [RegionInfo])
    , _drrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeRegionsResponse' smart constructor.
describeRegionsResponse :: Int -> DescribeRegionsResponse
describeRegionsResponse pStatus =
    DescribeRegionsResponse'
    { _drrsRegions = Nothing
    , _drrsStatus = pStatus
    }

-- | Information about one or more regions.
drrsRegions :: Lens' DescribeRegionsResponse [RegionInfo]
drrsRegions = lens _drrsRegions (\ s a -> s{_drrsRegions = a}) . _Default;

-- | FIXME: Undocumented member.
drrsStatus :: Lens' DescribeRegionsResponse Int
drrsStatus = lens _drrsStatus (\ s a -> s{_drrsStatus = a});

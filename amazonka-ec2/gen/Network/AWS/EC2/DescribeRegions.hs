{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeRegions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more regions that are currently available to you.
--
-- For a list of the regions supported by Amazon EC2, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html#ec2_region Regions and Endpoints>.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeRegions.html AWS API Reference> for DescribeRegions.
module Network.AWS.EC2.DescribeRegions
    (
    -- * Creating a Request
      DescribeRegions
    , describeRegions
    -- * Request Lenses
    , drsRegionNames
    , drsFilters
    , drsDryRun

    -- * Destructuring the Response
    , DescribeRegionsResponse
    , describeRegionsResponse
    -- * Response Lenses
    , drrsRegions
    , drrsStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeRegions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drsRegionNames'
--
-- * 'drsFilters'
--
-- * 'drsDryRun'
data DescribeRegions = DescribeRegions'
    { _drsRegionNames :: !(Maybe [Text])
    , _drsFilters :: !(Maybe [Filter])
    , _drsDryRun :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeRegions' smart constructor.
describeRegions :: DescribeRegions
describeRegions = 
    DescribeRegions'
    { _drsRegionNames = Nothing
    , _drsFilters = Nothing
    , _drsDryRun = Nothing
    }

-- | The names of one or more regions.
drsRegionNames :: Lens' DescribeRegions [Text]
drsRegionNames = lens _drsRegionNames (\ s a -> s{_drsRegionNames = a}) . _Default . _Coerce;

-- | One or more filters.
--
-- -   @endpoint@ - The endpoint of the region (for example,
--     @ec2.us-east-1.amazonaws.com@).
--
-- -   @region-name@ - The name of the region (for example, @us-east-1@).
--
drsFilters :: Lens' DescribeRegions [Filter]
drsFilters = lens _drsFilters (\ s a -> s{_drsFilters = a}) . _Default . _Coerce;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
drsDryRun :: Lens' DescribeRegions (Maybe Bool)
drsDryRun = lens _drsDryRun (\ s a -> s{_drsDryRun = a});

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
                 (toQueryList "RegionName" <$> _drsRegionNames),
               toQuery (toQueryList "Filter" <$> _drsFilters),
               "DryRun" =: _drsDryRun]

-- | /See:/ 'describeRegionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drrsRegions'
--
-- * 'drrsStatus'
data DescribeRegionsResponse = DescribeRegionsResponse'
    { _drrsRegions :: !(Maybe [RegionInfo])
    , _drrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeRegionsResponse' smart constructor.
describeRegionsResponse :: Int -> DescribeRegionsResponse
describeRegionsResponse pStatus_ = 
    DescribeRegionsResponse'
    { _drrsRegions = Nothing
    , _drrsStatus = pStatus_
    }

-- | Information about one or more regions.
drrsRegions :: Lens' DescribeRegionsResponse [RegionInfo]
drrsRegions = lens _drrsRegions (\ s a -> s{_drrsRegions = a}) . _Default . _Coerce;

-- | Undocumented member.
drrsStatus :: Lens' DescribeRegionsResponse Int
drrsStatus = lens _drrsStatus (\ s a -> s{_drrsStatus = a});

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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more regions that are currently available to you.
--
--
-- For a list of the regions supported by Amazon EC2, see <http://docs.aws.amazon.com/general/latest/gr/rande.html#ec2_region Regions and Endpoints> .
--
module Network.AWS.EC2.DescribeRegions
    (
    -- * Creating a Request
      describeRegions
    , DescribeRegions
    -- * Request Lenses
    , drsRegionNames
    , drsFilters
    , drsDryRun

    -- * Destructuring the Response
    , describeRegionsResponse
    , DescribeRegionsResponse
    -- * Response Lenses
    , drrsRegions
    , drrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeRegions.
--
--
--
-- /See:/ 'describeRegions' smart constructor.
data DescribeRegions = DescribeRegions'
  { _drsRegionNames :: !(Maybe [Text])
  , _drsFilters     :: !(Maybe [Filter])
  , _drsDryRun      :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRegions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsRegionNames' - The names of one or more regions.
--
-- * 'drsFilters' - One or more filters.     * @endpoint@ - The endpoint of the region (for example, @ec2.us-east-1.amazonaws.com@ ).     * @region-name@ - The name of the region (for example, @us-east-1@ ).
--
-- * 'drsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
describeRegions
    :: DescribeRegions
describeRegions =
  DescribeRegions'
    {_drsRegionNames = Nothing, _drsFilters = Nothing, _drsDryRun = Nothing}


-- | The names of one or more regions.
drsRegionNames :: Lens' DescribeRegions [Text]
drsRegionNames = lens _drsRegionNames (\ s a -> s{_drsRegionNames = a}) . _Default . _Coerce

-- | One or more filters.     * @endpoint@ - The endpoint of the region (for example, @ec2.us-east-1.amazonaws.com@ ).     * @region-name@ - The name of the region (for example, @us-east-1@ ).
drsFilters :: Lens' DescribeRegions [Filter]
drsFilters = lens _drsFilters (\ s a -> s{_drsFilters = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
drsDryRun :: Lens' DescribeRegions (Maybe Bool)
drsDryRun = lens _drsDryRun (\ s a -> s{_drsDryRun = a})

instance AWSRequest DescribeRegions where
        type Rs DescribeRegions = DescribeRegionsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeRegionsResponse' <$>
                   (x .@? "regionInfo" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeRegions where

instance NFData DescribeRegions where

instance ToHeaders DescribeRegions where
        toHeaders = const mempty

instance ToPath DescribeRegions where
        toPath = const "/"

instance ToQuery DescribeRegions where
        toQuery DescribeRegions'{..}
          = mconcat
              ["Action" =: ("DescribeRegions" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "RegionName" <$> _drsRegionNames),
               toQuery (toQueryList "Filter" <$> _drsFilters),
               "DryRun" =: _drsDryRun]

-- | Contains the output of DescribeRegions.
--
--
--
-- /See:/ 'describeRegionsResponse' smart constructor.
data DescribeRegionsResponse = DescribeRegionsResponse'
  { _drrsRegions        :: !(Maybe [RegionInfo])
  , _drrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRegionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrsRegions' - Information about one or more regions.
--
-- * 'drrsResponseStatus' - -- | The response status code.
describeRegionsResponse
    :: Int -- ^ 'drrsResponseStatus'
    -> DescribeRegionsResponse
describeRegionsResponse pResponseStatus_ =
  DescribeRegionsResponse'
    {_drrsRegions = Nothing, _drrsResponseStatus = pResponseStatus_}


-- | Information about one or more regions.
drrsRegions :: Lens' DescribeRegionsResponse [RegionInfo]
drrsRegions = lens _drrsRegions (\ s a -> s{_drrsRegions = a}) . _Default . _Coerce

-- | -- | The response status code.
drrsResponseStatus :: Lens' DescribeRegionsResponse Int
drrsResponseStatus = lens _drrsResponseStatus (\ s a -> s{_drrsResponseStatus = a})

instance NFData DescribeRegionsResponse where

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
-- Module      : Network.AWS.RDS.DescribeSourceRegions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the source AWS Regions where the current AWS Region can create a Read Replica or copy a DB snapshot from. This API action supports pagination.
--
--
module Network.AWS.RDS.DescribeSourceRegions
    (
    -- * Creating a Request
      describeSourceRegions
    , DescribeSourceRegions
    -- * Request Lenses
    , dsrRegionName
    , dsrFilters
    , dsrMarker
    , dsrMaxRecords

    -- * Destructuring the Response
    , describeSourceRegionsResponse
    , DescribeSourceRegionsResponse
    -- * Response Lenses
    , dsrrsMarker
    , dsrrsSourceRegions
    , dsrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeSourceRegions' smart constructor.
data DescribeSourceRegions = DescribeSourceRegions'
  { _dsrRegionName :: !(Maybe Text)
  , _dsrFilters    :: !(Maybe [Filter])
  , _dsrMarker     :: !(Maybe Text)
  , _dsrMaxRecords :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSourceRegions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrRegionName' - The source AWS Region name. For example, @us-east-1@ . Constraints:     * Must specify a valid AWS Region name.
--
-- * 'dsrFilters' - This parameter is not currently supported.
--
-- * 'dsrMarker' - An optional pagination token provided by a previous 'DescribeSourceRegions' request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dsrMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
describeSourceRegions
    :: DescribeSourceRegions
describeSourceRegions =
  DescribeSourceRegions'
    { _dsrRegionName = Nothing
    , _dsrFilters = Nothing
    , _dsrMarker = Nothing
    , _dsrMaxRecords = Nothing
    }


-- | The source AWS Region name. For example, @us-east-1@ . Constraints:     * Must specify a valid AWS Region name.
dsrRegionName :: Lens' DescribeSourceRegions (Maybe Text)
dsrRegionName = lens _dsrRegionName (\ s a -> s{_dsrRegionName = a})

-- | This parameter is not currently supported.
dsrFilters :: Lens' DescribeSourceRegions [Filter]
dsrFilters = lens _dsrFilters (\ s a -> s{_dsrFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous 'DescribeSourceRegions' request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dsrMarker :: Lens' DescribeSourceRegions (Maybe Text)
dsrMarker = lens _dsrMarker (\ s a -> s{_dsrMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
dsrMaxRecords :: Lens' DescribeSourceRegions (Maybe Int)
dsrMaxRecords = lens _dsrMaxRecords (\ s a -> s{_dsrMaxRecords = a})

instance AWSRequest DescribeSourceRegions where
        type Rs DescribeSourceRegions =
             DescribeSourceRegionsResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "DescribeSourceRegionsResult"
              (\ s h x ->
                 DescribeSourceRegionsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "SourceRegions" .!@ mempty >>=
                        may (parseXMLList "SourceRegion"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeSourceRegions where

instance NFData DescribeSourceRegions where

instance ToHeaders DescribeSourceRegions where
        toHeaders = const mempty

instance ToPath DescribeSourceRegions where
        toPath = const "/"

instance ToQuery DescribeSourceRegions where
        toQuery DescribeSourceRegions'{..}
          = mconcat
              ["Action" =: ("DescribeSourceRegions" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "RegionName" =: _dsrRegionName,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _dsrFilters),
               "Marker" =: _dsrMarker,
               "MaxRecords" =: _dsrMaxRecords]

-- | Contains the result of a successful invocation of the 'DescribeSourceRegions' action.
--
--
--
-- /See:/ 'describeSourceRegionsResponse' smart constructor.
data DescribeSourceRegionsResponse = DescribeSourceRegionsResponse'
  { _dsrrsMarker         :: !(Maybe Text)
  , _dsrrsSourceRegions  :: !(Maybe [SourceRegion])
  , _dsrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSourceRegionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrrsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dsrrsSourceRegions' - A list of SourceRegion instances that contains each source AWS Region that the current AWS Region can get a Read Replica or a DB snapshot from.
--
-- * 'dsrrsResponseStatus' - -- | The response status code.
describeSourceRegionsResponse
    :: Int -- ^ 'dsrrsResponseStatus'
    -> DescribeSourceRegionsResponse
describeSourceRegionsResponse pResponseStatus_ =
  DescribeSourceRegionsResponse'
    { _dsrrsMarker = Nothing
    , _dsrrsSourceRegions = Nothing
    , _dsrrsResponseStatus = pResponseStatus_
    }


-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dsrrsMarker :: Lens' DescribeSourceRegionsResponse (Maybe Text)
dsrrsMarker = lens _dsrrsMarker (\ s a -> s{_dsrrsMarker = a})

-- | A list of SourceRegion instances that contains each source AWS Region that the current AWS Region can get a Read Replica or a DB snapshot from.
dsrrsSourceRegions :: Lens' DescribeSourceRegionsResponse [SourceRegion]
dsrrsSourceRegions = lens _dsrrsSourceRegions (\ s a -> s{_dsrrsSourceRegions = a}) . _Default . _Coerce

-- | -- | The response status code.
dsrrsResponseStatus :: Lens' DescribeSourceRegionsResponse Int
dsrrsResponseStatus = lens _dsrrsResponseStatus (\ s a -> s{_dsrrsResponseStatus = a})

instance NFData DescribeSourceRegionsResponse where

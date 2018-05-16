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
-- Module      : Network.AWS.DMS.DescribeReplicationSubnetGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the replication subnet groups.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeReplicationSubnetGroups
    (
    -- * Creating a Request
      describeReplicationSubnetGroups
    , DescribeReplicationSubnetGroups
    -- * Request Lenses
    , drsgFilters
    , drsgMarker
    , drsgMaxRecords

    -- * Destructuring the Response
    , describeReplicationSubnetGroupsResponse
    , DescribeReplicationSubnetGroupsResponse
    -- * Response Lenses
    , drsgsrsMarker
    , drsgsrsReplicationSubnetGroups
    , drsgsrsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeReplicationSubnetGroups' smart constructor.
data DescribeReplicationSubnetGroups = DescribeReplicationSubnetGroups'
  { _drsgFilters    :: !(Maybe [Filter])
  , _drsgMarker     :: !(Maybe Text)
  , _drsgMaxRecords :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReplicationSubnetGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsgFilters' - Filters applied to the describe action.
--
-- * 'drsgMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'drsgMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
describeReplicationSubnetGroups
    :: DescribeReplicationSubnetGroups
describeReplicationSubnetGroups =
  DescribeReplicationSubnetGroups'
    {_drsgFilters = Nothing, _drsgMarker = Nothing, _drsgMaxRecords = Nothing}


-- | Filters applied to the describe action.
drsgFilters :: Lens' DescribeReplicationSubnetGroups [Filter]
drsgFilters = lens _drsgFilters (\ s a -> s{_drsgFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
drsgMarker :: Lens' DescribeReplicationSubnetGroups (Maybe Text)
drsgMarker = lens _drsgMarker (\ s a -> s{_drsgMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
drsgMaxRecords :: Lens' DescribeReplicationSubnetGroups (Maybe Int)
drsgMaxRecords = lens _drsgMaxRecords (\ s a -> s{_drsgMaxRecords = a})

instance AWSPager DescribeReplicationSubnetGroups
         where
        page rq rs
          | stop (rs ^. drsgsrsMarker) = Nothing
          | stop (rs ^. drsgsrsReplicationSubnetGroups) =
            Nothing
          | otherwise =
            Just $ rq & drsgMarker .~ rs ^. drsgsrsMarker

instance AWSRequest DescribeReplicationSubnetGroups
         where
        type Rs DescribeReplicationSubnetGroups =
             DescribeReplicationSubnetGroupsResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 DescribeReplicationSubnetGroupsResponse' <$>
                   (x .?> "Marker") <*>
                     (x .?> "ReplicationSubnetGroups" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeReplicationSubnetGroups
         where

instance NFData DescribeReplicationSubnetGroups where

instance ToHeaders DescribeReplicationSubnetGroups
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.DescribeReplicationSubnetGroups"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeReplicationSubnetGroups where
        toJSON DescribeReplicationSubnetGroups'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _drsgFilters,
                  ("Marker" .=) <$> _drsgMarker,
                  ("MaxRecords" .=) <$> _drsgMaxRecords])

instance ToPath DescribeReplicationSubnetGroups where
        toPath = const "/"

instance ToQuery DescribeReplicationSubnetGroups
         where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'describeReplicationSubnetGroupsResponse' smart constructor.
data DescribeReplicationSubnetGroupsResponse = DescribeReplicationSubnetGroupsResponse'
  { _drsgsrsMarker                  :: !(Maybe Text)
  , _drsgsrsReplicationSubnetGroups :: !(Maybe [ReplicationSubnetGroup])
  , _drsgsrsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeReplicationSubnetGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsgsrsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'drsgsrsReplicationSubnetGroups' - A description of the replication subnet groups.
--
-- * 'drsgsrsResponseStatus' - -- | The response status code.
describeReplicationSubnetGroupsResponse
    :: Int -- ^ 'drsgsrsResponseStatus'
    -> DescribeReplicationSubnetGroupsResponse
describeReplicationSubnetGroupsResponse pResponseStatus_ =
  DescribeReplicationSubnetGroupsResponse'
    { _drsgsrsMarker = Nothing
    , _drsgsrsReplicationSubnetGroups = Nothing
    , _drsgsrsResponseStatus = pResponseStatus_
    }


-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
drsgsrsMarker :: Lens' DescribeReplicationSubnetGroupsResponse (Maybe Text)
drsgsrsMarker = lens _drsgsrsMarker (\ s a -> s{_drsgsrsMarker = a})

-- | A description of the replication subnet groups.
drsgsrsReplicationSubnetGroups :: Lens' DescribeReplicationSubnetGroupsResponse [ReplicationSubnetGroup]
drsgsrsReplicationSubnetGroups = lens _drsgsrsReplicationSubnetGroups (\ s a -> s{_drsgsrsReplicationSubnetGroups = a}) . _Default . _Coerce

-- | -- | The response status code.
drsgsrsResponseStatus :: Lens' DescribeReplicationSubnetGroupsResponse Int
drsgsrsResponseStatus = lens _drsgsrsResponseStatus (\ s a -> s{_drsgsrsResponseStatus = a})

instance NFData
           DescribeReplicationSubnetGroupsResponse
         where

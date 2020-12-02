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
-- Module      : Network.AWS.RDS.DescribeDBParameterGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DBParameterGroup@ descriptions. If a @DBParameterGroupName@ is specified, the list will contain only the description of the specified DB parameter group.
--
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBParameterGroups
    (
    -- * Creating a Request
      describeDBParameterGroups
    , DescribeDBParameterGroups
    -- * Request Lenses
    , ddpgFilters
    , ddpgDBParameterGroupName
    , ddpgMarker
    , ddpgMaxRecords

    -- * Destructuring the Response
    , describeDBParameterGroupsResponse
    , DescribeDBParameterGroupsResponse
    -- * Response Lenses
    , ddpgrsMarker
    , ddpgrsDBParameterGroups
    , ddpgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeDBParameterGroups' smart constructor.
data DescribeDBParameterGroups = DescribeDBParameterGroups'
  { _ddpgFilters              :: !(Maybe [Filter])
  , _ddpgDBParameterGroupName :: !(Maybe Text)
  , _ddpgMarker               :: !(Maybe Text)
  , _ddpgMaxRecords           :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBParameterGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddpgFilters' - This parameter is not currently supported.
--
-- * 'ddpgDBParameterGroupName' - The name of a specific DB parameter group to return details for. Constraints:     * If supplied, must match the name of an existing DBClusterParameterGroup.
--
-- * 'ddpgMarker' - An optional pagination token provided by a previous @DescribeDBParameterGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddpgMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
describeDBParameterGroups
    :: DescribeDBParameterGroups
describeDBParameterGroups =
  DescribeDBParameterGroups'
    { _ddpgFilters = Nothing
    , _ddpgDBParameterGroupName = Nothing
    , _ddpgMarker = Nothing
    , _ddpgMaxRecords = Nothing
    }


-- | This parameter is not currently supported.
ddpgFilters :: Lens' DescribeDBParameterGroups [Filter]
ddpgFilters = lens _ddpgFilters (\ s a -> s{_ddpgFilters = a}) . _Default . _Coerce

-- | The name of a specific DB parameter group to return details for. Constraints:     * If supplied, must match the name of an existing DBClusterParameterGroup.
ddpgDBParameterGroupName :: Lens' DescribeDBParameterGroups (Maybe Text)
ddpgDBParameterGroupName = lens _ddpgDBParameterGroupName (\ s a -> s{_ddpgDBParameterGroupName = a})

-- | An optional pagination token provided by a previous @DescribeDBParameterGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddpgMarker :: Lens' DescribeDBParameterGroups (Maybe Text)
ddpgMarker = lens _ddpgMarker (\ s a -> s{_ddpgMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
ddpgMaxRecords :: Lens' DescribeDBParameterGroups (Maybe Int)
ddpgMaxRecords = lens _ddpgMaxRecords (\ s a -> s{_ddpgMaxRecords = a})

instance AWSPager DescribeDBParameterGroups where
        page rq rs
          | stop (rs ^. ddpgrsMarker) = Nothing
          | stop (rs ^. ddpgrsDBParameterGroups) = Nothing
          | otherwise =
            Just $ rq & ddpgMarker .~ rs ^. ddpgrsMarker

instance AWSRequest DescribeDBParameterGroups where
        type Rs DescribeDBParameterGroups =
             DescribeDBParameterGroupsResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "DescribeDBParameterGroupsResult"
              (\ s h x ->
                 DescribeDBParameterGroupsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "DBParameterGroups" .!@ mempty >>=
                        may (parseXMLList "DBParameterGroup"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeDBParameterGroups where

instance NFData DescribeDBParameterGroups where

instance ToHeaders DescribeDBParameterGroups where
        toHeaders = const mempty

instance ToPath DescribeDBParameterGroups where
        toPath = const "/"

instance ToQuery DescribeDBParameterGroups where
        toQuery DescribeDBParameterGroups'{..}
          = mconcat
              ["Action" =:
                 ("DescribeDBParameterGroups" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _ddpgFilters),
               "DBParameterGroupName" =: _ddpgDBParameterGroupName,
               "Marker" =: _ddpgMarker,
               "MaxRecords" =: _ddpgMaxRecords]

-- | Contains the result of a successful invocation of the 'DescribeDBParameterGroups' action.
--
--
--
-- /See:/ 'describeDBParameterGroupsResponse' smart constructor.
data DescribeDBParameterGroupsResponse = DescribeDBParameterGroupsResponse'
  { _ddpgrsMarker            :: !(Maybe Text)
  , _ddpgrsDBParameterGroups :: !(Maybe [DBParameterGroup])
  , _ddpgrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBParameterGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddpgrsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddpgrsDBParameterGroups' - A list of 'DBParameterGroup' instances.
--
-- * 'ddpgrsResponseStatus' - -- | The response status code.
describeDBParameterGroupsResponse
    :: Int -- ^ 'ddpgrsResponseStatus'
    -> DescribeDBParameterGroupsResponse
describeDBParameterGroupsResponse pResponseStatus_ =
  DescribeDBParameterGroupsResponse'
    { _ddpgrsMarker = Nothing
    , _ddpgrsDBParameterGroups = Nothing
    , _ddpgrsResponseStatus = pResponseStatus_
    }


-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddpgrsMarker :: Lens' DescribeDBParameterGroupsResponse (Maybe Text)
ddpgrsMarker = lens _ddpgrsMarker (\ s a -> s{_ddpgrsMarker = a})

-- | A list of 'DBParameterGroup' instances.
ddpgrsDBParameterGroups :: Lens' DescribeDBParameterGroupsResponse [DBParameterGroup]
ddpgrsDBParameterGroups = lens _ddpgrsDBParameterGroups (\ s a -> s{_ddpgrsDBParameterGroups = a}) . _Default . _Coerce

-- | -- | The response status code.
ddpgrsResponseStatus :: Lens' DescribeDBParameterGroupsResponse Int
ddpgrsResponseStatus = lens _ddpgrsResponseStatus (\ s a -> s{_ddpgrsResponseStatus = a})

instance NFData DescribeDBParameterGroupsResponse
         where

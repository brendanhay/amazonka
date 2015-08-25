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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of 'DBParameterGroup' descriptions. If a
-- 'DBParameterGroupName' is specified, the list will contain only the
-- description of the specified DB parameter group.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBParameterGroups.html AWS API Reference> for DescribeDBParameterGroups.
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
    , ddpgMaxRecords
    , ddpgMarker

    -- * Destructuring the Response
    , describeDBParameterGroupsResponse
    , DescribeDBParameterGroupsResponse
    -- * Response Lenses
    , ddpgrsMarker
    , ddpgrsDBParameterGroups
    , ddpgrsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeDBParameterGroups' smart constructor.
data DescribeDBParameterGroups = DescribeDBParameterGroups'
    { _ddpgFilters              :: !(Maybe [Filter])
    , _ddpgDBParameterGroupName :: !(Maybe Text)
    , _ddpgMaxRecords           :: !(Maybe Int)
    , _ddpgMarker               :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeDBParameterGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddpgFilters'
--
-- * 'ddpgDBParameterGroupName'
--
-- * 'ddpgMaxRecords'
--
-- * 'ddpgMarker'
describeDBParameterGroups
    :: DescribeDBParameterGroups
describeDBParameterGroups =
    DescribeDBParameterGroups'
    { _ddpgFilters = Nothing
    , _ddpgDBParameterGroupName = Nothing
    , _ddpgMaxRecords = Nothing
    , _ddpgMarker = Nothing
    }

-- | This parameter is not currently supported.
ddpgFilters :: Lens' DescribeDBParameterGroups [Filter]
ddpgFilters = lens _ddpgFilters (\ s a -> s{_ddpgFilters = a}) . _Default . _Coerce;

-- | The name of a specific DB parameter group to return details for.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
ddpgDBParameterGroupName :: Lens' DescribeDBParameterGroups (Maybe Text)
ddpgDBParameterGroupName = lens _ddpgDBParameterGroupName (\ s a -> s{_ddpgDBParameterGroupName = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified 'MaxRecords' value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
ddpgMaxRecords :: Lens' DescribeDBParameterGroups (Maybe Int)
ddpgMaxRecords = lens _ddpgMaxRecords (\ s a -> s{_ddpgMaxRecords = a});

-- | An optional pagination token provided by a previous
-- 'DescribeDBParameterGroups' request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by 'MaxRecords'.
ddpgMarker :: Lens' DescribeDBParameterGroups (Maybe Text)
ddpgMarker = lens _ddpgMarker (\ s a -> s{_ddpgMarker = a});

instance AWSPager DescribeDBParameterGroups where
        page rq rs
          | stop (rs ^. ddpgrsMarker) = Nothing
          | stop (rs ^. ddpgrsDBParameterGroups) = Nothing
          | otherwise =
            Just $ rq & ddpgMarker .~ rs ^. ddpgrsMarker

instance AWSRequest DescribeDBParameterGroups where
        type Rs DescribeDBParameterGroups =
             DescribeDBParameterGroupsResponse
        request = postQuery rDS
        response
          = receiveXMLWrapper "DescribeDBParameterGroupsResult"
              (\ s h x ->
                 DescribeDBParameterGroupsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "DBParameterGroups" .!@ mempty >>=
                        may (parseXMLList "DBParameterGroup"))
                     <*> (pure (fromEnum s)))

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
               "MaxRecords" =: _ddpgMaxRecords,
               "Marker" =: _ddpgMarker]

-- | Contains the result of a successful invocation of the
-- DescribeDBParameterGroups action.
--
-- /See:/ 'describeDBParameterGroupsResponse' smart constructor.
data DescribeDBParameterGroupsResponse = DescribeDBParameterGroupsResponse'
    { _ddpgrsMarker            :: !(Maybe Text)
    , _ddpgrsDBParameterGroups :: !(Maybe [DBParameterGroup])
    , _ddpgrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeDBParameterGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddpgrsMarker'
--
-- * 'ddpgrsDBParameterGroups'
--
-- * 'ddpgrsStatus'
describeDBParameterGroupsResponse
    :: Int -- ^ 'ddpgrsStatus'
    -> DescribeDBParameterGroupsResponse
describeDBParameterGroupsResponse pStatus_ =
    DescribeDBParameterGroupsResponse'
    { _ddpgrsMarker = Nothing
    , _ddpgrsDBParameterGroups = Nothing
    , _ddpgrsStatus = pStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by 'MaxRecords'.
ddpgrsMarker :: Lens' DescribeDBParameterGroupsResponse (Maybe Text)
ddpgrsMarker = lens _ddpgrsMarker (\ s a -> s{_ddpgrsMarker = a});

-- | A list of DBParameterGroup instances.
ddpgrsDBParameterGroups :: Lens' DescribeDBParameterGroupsResponse [DBParameterGroup]
ddpgrsDBParameterGroups = lens _ddpgrsDBParameterGroups (\ s a -> s{_ddpgrsDBParameterGroups = a}) . _Default . _Coerce;

-- | The response status code.
ddpgrsStatus :: Lens' DescribeDBParameterGroupsResponse Int
ddpgrsStatus = lens _ddpgrsStatus (\ s a -> s{_ddpgrsStatus = a});

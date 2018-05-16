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
-- Module      : Network.AWS.RDS.DescribeDBSecurityGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DBSecurityGroup@ descriptions. If a @DBSecurityGroupName@ is specified, the list will contain only the descriptions of the specified DB security group.
--
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBSecurityGroups
    (
    -- * Creating a Request
      describeDBSecurityGroups
    , DescribeDBSecurityGroups
    -- * Request Lenses
    , ddbsgFilters
    , ddbsgMarker
    , ddbsgMaxRecords
    , ddbsgDBSecurityGroupName

    -- * Destructuring the Response
    , describeDBSecurityGroupsResponse
    , DescribeDBSecurityGroupsResponse
    -- * Response Lenses
    , ddbsgrsDBSecurityGroups
    , ddbsgrsMarker
    , ddbsgrsResponseStatus
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
-- /See:/ 'describeDBSecurityGroups' smart constructor.
data DescribeDBSecurityGroups = DescribeDBSecurityGroups'
  { _ddbsgFilters             :: !(Maybe [Filter])
  , _ddbsgMarker              :: !(Maybe Text)
  , _ddbsgMaxRecords          :: !(Maybe Int)
  , _ddbsgDBSecurityGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBSecurityGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbsgFilters' - This parameter is not currently supported.
--
-- * 'ddbsgMarker' - An optional pagination token provided by a previous @DescribeDBSecurityGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddbsgMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
--
-- * 'ddbsgDBSecurityGroupName' - The name of the DB security group to return details for.
describeDBSecurityGroups
    :: DescribeDBSecurityGroups
describeDBSecurityGroups =
  DescribeDBSecurityGroups'
    { _ddbsgFilters = Nothing
    , _ddbsgMarker = Nothing
    , _ddbsgMaxRecords = Nothing
    , _ddbsgDBSecurityGroupName = Nothing
    }


-- | This parameter is not currently supported.
ddbsgFilters :: Lens' DescribeDBSecurityGroups [Filter]
ddbsgFilters = lens _ddbsgFilters (\ s a -> s{_ddbsgFilters = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous @DescribeDBSecurityGroups@ request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddbsgMarker :: Lens' DescribeDBSecurityGroups (Maybe Text)
ddbsgMarker = lens _ddbsgMarker (\ s a -> s{_ddbsgMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
ddbsgMaxRecords :: Lens' DescribeDBSecurityGroups (Maybe Int)
ddbsgMaxRecords = lens _ddbsgMaxRecords (\ s a -> s{_ddbsgMaxRecords = a})

-- | The name of the DB security group to return details for.
ddbsgDBSecurityGroupName :: Lens' DescribeDBSecurityGroups (Maybe Text)
ddbsgDBSecurityGroupName = lens _ddbsgDBSecurityGroupName (\ s a -> s{_ddbsgDBSecurityGroupName = a})

instance AWSPager DescribeDBSecurityGroups where
        page rq rs
          | stop (rs ^. ddbsgrsMarker) = Nothing
          | stop (rs ^. ddbsgrsDBSecurityGroups) = Nothing
          | otherwise =
            Just $ rq & ddbsgMarker .~ rs ^. ddbsgrsMarker

instance AWSRequest DescribeDBSecurityGroups where
        type Rs DescribeDBSecurityGroups =
             DescribeDBSecurityGroupsResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "DescribeDBSecurityGroupsResult"
              (\ s h x ->
                 DescribeDBSecurityGroupsResponse' <$>
                   (x .@? "DBSecurityGroups" .!@ mempty >>=
                      may (parseXMLList "DBSecurityGroup"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeDBSecurityGroups where

instance NFData DescribeDBSecurityGroups where

instance ToHeaders DescribeDBSecurityGroups where
        toHeaders = const mempty

instance ToPath DescribeDBSecurityGroups where
        toPath = const "/"

instance ToQuery DescribeDBSecurityGroups where
        toQuery DescribeDBSecurityGroups'{..}
          = mconcat
              ["Action" =:
                 ("DescribeDBSecurityGroups" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _ddbsgFilters),
               "Marker" =: _ddbsgMarker,
               "MaxRecords" =: _ddbsgMaxRecords,
               "DBSecurityGroupName" =: _ddbsgDBSecurityGroupName]

-- | Contains the result of a successful invocation of the 'DescribeDBSecurityGroups' action.
--
--
--
-- /See:/ 'describeDBSecurityGroupsResponse' smart constructor.
data DescribeDBSecurityGroupsResponse = DescribeDBSecurityGroupsResponse'
  { _ddbsgrsDBSecurityGroups :: !(Maybe [DBSecurityGroup])
  , _ddbsgrsMarker           :: !(Maybe Text)
  , _ddbsgrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDBSecurityGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbsgrsDBSecurityGroups' - A list of 'DBSecurityGroup' instances.
--
-- * 'ddbsgrsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'ddbsgrsResponseStatus' - -- | The response status code.
describeDBSecurityGroupsResponse
    :: Int -- ^ 'ddbsgrsResponseStatus'
    -> DescribeDBSecurityGroupsResponse
describeDBSecurityGroupsResponse pResponseStatus_ =
  DescribeDBSecurityGroupsResponse'
    { _ddbsgrsDBSecurityGroups = Nothing
    , _ddbsgrsMarker = Nothing
    , _ddbsgrsResponseStatus = pResponseStatus_
    }


-- | A list of 'DBSecurityGroup' instances.
ddbsgrsDBSecurityGroups :: Lens' DescribeDBSecurityGroupsResponse [DBSecurityGroup]
ddbsgrsDBSecurityGroups = lens _ddbsgrsDBSecurityGroups (\ s a -> s{_ddbsgrsDBSecurityGroups = a}) . _Default . _Coerce

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
ddbsgrsMarker :: Lens' DescribeDBSecurityGroupsResponse (Maybe Text)
ddbsgrsMarker = lens _ddbsgrsMarker (\ s a -> s{_ddbsgrsMarker = a})

-- | -- | The response status code.
ddbsgrsResponseStatus :: Lens' DescribeDBSecurityGroupsResponse Int
ddbsgrsResponseStatus = lens _ddbsgrsResponseStatus (\ s a -> s{_ddbsgrsResponseStatus = a})

instance NFData DescribeDBSecurityGroupsResponse
         where

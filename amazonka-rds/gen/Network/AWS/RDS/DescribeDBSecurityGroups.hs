{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.RDS.DescribeDBSecurityGroups
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

-- | Returns a list of @DBSecurityGroup@ descriptions. If a
-- @DBSecurityGroupName@ is specified, the list will contain only the
-- descriptions of the specified DB security group.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBSecurityGroups.html>
module Network.AWS.RDS.DescribeDBSecurityGroups
    (
    -- * Request
      DescribeDBSecurityGroups
    -- ** Request constructor
    , describeDBSecurityGroups
    -- ** Request lenses
    , ddbsgFilters
    , ddbsgMaxRecords
    , ddbsgMarker
    , ddbsgDBSecurityGroupName

    -- * Response
    , DescribeDBSecurityGroupsResponse
    -- ** Response constructor
    , describeDBSecurityGroupsResponse
    -- ** Response lenses
    , desDBSecurityGroups
    , desMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDBSecurityGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbsgFilters'
--
-- * 'ddbsgMaxRecords'
--
-- * 'ddbsgMarker'
--
-- * 'ddbsgDBSecurityGroupName'
data DescribeDBSecurityGroups = DescribeDBSecurityGroups'{_ddbsgFilters :: Maybe [Filter], _ddbsgMaxRecords :: Maybe Int, _ddbsgMarker :: Maybe Text, _ddbsgDBSecurityGroupName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeDBSecurityGroups' smart constructor.
describeDBSecurityGroups :: DescribeDBSecurityGroups
describeDBSecurityGroups = DescribeDBSecurityGroups'{_ddbsgFilters = Nothing, _ddbsgMaxRecords = Nothing, _ddbsgMarker = Nothing, _ddbsgDBSecurityGroupName = Nothing};

-- | This parameter is not currently supported.
ddbsgFilters :: Lens' DescribeDBSecurityGroups [Filter]
ddbsgFilters = lens _ddbsgFilters (\ s a -> s{_ddbsgFilters = a}) . _Default;

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results may be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
ddbsgMaxRecords :: Lens' DescribeDBSecurityGroups (Maybe Int)
ddbsgMaxRecords = lens _ddbsgMaxRecords (\ s a -> s{_ddbsgMaxRecords = a});

-- | An optional pagination token provided by a previous
-- DescribeDBSecurityGroups request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
ddbsgMarker :: Lens' DescribeDBSecurityGroups (Maybe Text)
ddbsgMarker = lens _ddbsgMarker (\ s a -> s{_ddbsgMarker = a});

-- | The name of the DB security group to return details for.
ddbsgDBSecurityGroupName :: Lens' DescribeDBSecurityGroups (Maybe Text)
ddbsgDBSecurityGroupName = lens _ddbsgDBSecurityGroupName (\ s a -> s{_ddbsgDBSecurityGroupName = a});

instance AWSRequest DescribeDBSecurityGroups where
        type Sv DescribeDBSecurityGroups = RDS
        type Rs DescribeDBSecurityGroups =
             DescribeDBSecurityGroupsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeDBSecurityGroupsResult"
              (\ s h x ->
                 DescribeDBSecurityGroupsResponse' <$>
                   (x .@? "DBSecurityGroups" .!@ mempty >>=
                      may (parseXMLList "DBSecurityGroup"))
                     <*> (x .@? "Marker"))

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
               "MaxRecords" =: _ddbsgMaxRecords,
               "Marker" =: _ddbsgMarker,
               "DBSecurityGroupName" =: _ddbsgDBSecurityGroupName]

-- | /See:/ 'describeDBSecurityGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desDBSecurityGroups'
--
-- * 'desMarker'
data DescribeDBSecurityGroupsResponse = DescribeDBSecurityGroupsResponse'{_desDBSecurityGroups :: Maybe [DBSecurityGroup], _desMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeDBSecurityGroupsResponse' smart constructor.
describeDBSecurityGroupsResponse :: DescribeDBSecurityGroupsResponse
describeDBSecurityGroupsResponse = DescribeDBSecurityGroupsResponse'{_desDBSecurityGroups = Nothing, _desMarker = Nothing};

-- | A list of DBSecurityGroup instances.
desDBSecurityGroups :: Lens' DescribeDBSecurityGroupsResponse [DBSecurityGroup]
desDBSecurityGroups = lens _desDBSecurityGroups (\ s a -> s{_desDBSecurityGroups = a}) . _Default;

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
desMarker :: Lens' DescribeDBSecurityGroupsResponse (Maybe Text)
desMarker = lens _desMarker (\ s a -> s{_desMarker = a});

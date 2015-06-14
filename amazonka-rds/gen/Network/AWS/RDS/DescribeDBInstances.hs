{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.RDS.DescribeDBInstances
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

-- | Returns information about provisioned RDS instances. This API supports
-- pagination.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBInstances.html>
module Network.AWS.RDS.DescribeDBInstances
    (
    -- * Request
      DescribeDBInstances
    -- ** Request constructor
    , describeDBInstances
    -- ** Request lenses
    , describeFilters
    , describeDBInstanceIdentifier
    , describeMaxRecords
    , describeMarker

    -- * Response
    , DescribeDBInstancesResponse
    -- ** Response constructor
    , describeDBInstancesResponse
    -- ** Response lenses
    , ddirDBInstances
    , ddirMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.RDS.Types

-- | /See:/ 'describeDBInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'describeFilters'
--
-- * 'describeDBInstanceIdentifier'
--
-- * 'describeMaxRecords'
--
-- * 'describeMarker'
data DescribeDBInstances = DescribeDBInstances'{_describeFilters :: Maybe [Filter], _describeDBInstanceIdentifier :: Maybe Text, _describeMaxRecords :: Maybe Int, _describeMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeDBInstances' smart constructor.
describeDBInstances :: DescribeDBInstances
describeDBInstances = DescribeDBInstances'{_describeFilters = Nothing, _describeDBInstanceIdentifier = Nothing, _describeMaxRecords = Nothing, _describeMarker = Nothing};

-- | This parameter is not currently supported.
describeFilters :: Lens' DescribeDBInstances (Maybe [Filter])
describeFilters = lens _describeFilters (\ s a -> s{_describeFilters = a});

-- | The user-supplied instance identifier. If this parameter is specified,
-- information from only the specific DB instance is returned. This
-- parameter isn\'t case sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
describeDBInstanceIdentifier :: Lens' DescribeDBInstances (Maybe Text)
describeDBInstanceIdentifier = lens _describeDBInstanceIdentifier (\ s a -> s{_describeDBInstanceIdentifier = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results may be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
describeMaxRecords :: Lens' DescribeDBInstances (Maybe Int)
describeMaxRecords = lens _describeMaxRecords (\ s a -> s{_describeMaxRecords = a});

-- | An optional pagination token provided by a previous DescribeDBInstances
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by @MaxRecords@ .
describeMarker :: Lens' DescribeDBInstances (Maybe Text)
describeMarker = lens _describeMarker (\ s a -> s{_describeMarker = a});

instance AWSRequest DescribeDBInstances where
        type Sv DescribeDBInstances = RDS
        type Rs DescribeDBInstances =
             DescribeDBInstancesResponse
        request = post
        response
          = receiveXMLWrapper "DescribeDBInstancesResult"
              (\ s h x ->
                 DescribeDBInstancesResponse' <$>
                   (x .@? "DBInstances" .!@ mempty >>=
                      parseXMLList "DBInstance")
                     <*> x .@? "Marker")

instance ToHeaders DescribeDBInstances where
        toHeaders = const mempty

instance ToPath DescribeDBInstances where
        toPath = const "/"

instance ToQuery DescribeDBInstances where
        toQuery DescribeDBInstances'{..}
          = mconcat
              ["Action" =: ("DescribeDBInstances" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Filters" =: "Filter" =: _describeFilters,
               "DBInstanceIdentifier" =:
                 _describeDBInstanceIdentifier,
               "MaxRecords" =: _describeMaxRecords,
               "Marker" =: _describeMarker]

-- | /See:/ 'describeDBInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddirDBInstances'
--
-- * 'ddirMarker'
data DescribeDBInstancesResponse = DescribeDBInstancesResponse'{_ddirDBInstances :: Maybe [DBInstance], _ddirMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeDBInstancesResponse' smart constructor.
describeDBInstancesResponse :: DescribeDBInstancesResponse
describeDBInstancesResponse = DescribeDBInstancesResponse'{_ddirDBInstances = Nothing, _ddirMarker = Nothing};

-- | A list of DBInstance instances.
ddirDBInstances :: Lens' DescribeDBInstancesResponse (Maybe [DBInstance])
ddirDBInstances = lens _ddirDBInstances (\ s a -> s{_ddirDBInstances = a});

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@ .
ddirMarker :: Lens' DescribeDBInstancesResponse (Maybe Text)
ddirMarker = lens _ddirMarker (\ s a -> s{_ddirMarker = a});

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBSubnetGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Returns a list of DBSubnetGroup descriptions. If a DBSubnetGroupName is
-- specified, the list will contain only the descriptions of the specified
-- DBSubnetGroup.
--
-- For an overview of CIDR ranges, go to the
-- <http://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Wikipedia Tutorial>.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBSubnetGroups.html>
module Network.AWS.RDS.DescribeDBSubnetGroups
    (
    -- * Request
      DescribeDBSubnetGroups
    -- ** Request constructor
    , describeDBSubnetGroups
    -- ** Request lenses
    , ddsgDBSubnetGroupName
    , ddsgFilters
    , ddsgMaxRecords
    , ddsgMarker

    -- * Response
    , DescribeDBSubnetGroupsResponse
    -- ** Response constructor
    , describeDBSubnetGroupsResponse
    -- ** Response lenses
    , ddsgrDBSubnetGroups
    , ddsgrMarker
    , ddsgrStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeDBSubnetGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddsgDBSubnetGroupName'
--
-- * 'ddsgFilters'
--
-- * 'ddsgMaxRecords'
--
-- * 'ddsgMarker'
data DescribeDBSubnetGroups = DescribeDBSubnetGroups'
    { _ddsgDBSubnetGroupName :: !(Maybe Text)
    , _ddsgFilters           :: !(Maybe [Filter])
    , _ddsgMaxRecords        :: !(Maybe Int)
    , _ddsgMarker            :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBSubnetGroups' smart constructor.
describeDBSubnetGroups :: DescribeDBSubnetGroups
describeDBSubnetGroups =
    DescribeDBSubnetGroups'
    { _ddsgDBSubnetGroupName = Nothing
    , _ddsgFilters = Nothing
    , _ddsgMaxRecords = Nothing
    , _ddsgMarker = Nothing
    }

-- | The name of the DB subnet group to return details for.
ddsgDBSubnetGroupName :: Lens' DescribeDBSubnetGroups (Maybe Text)
ddsgDBSubnetGroupName = lens _ddsgDBSubnetGroupName (\ s a -> s{_ddsgDBSubnetGroupName = a});

-- | This parameter is not currently supported.
ddsgFilters :: Lens' DescribeDBSubnetGroups [Filter]
ddsgFilters = lens _ddsgFilters (\ s a -> s{_ddsgFilters = a}) . _Default;

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results may be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
ddsgMaxRecords :: Lens' DescribeDBSubnetGroups (Maybe Int)
ddsgMaxRecords = lens _ddsgMaxRecords (\ s a -> s{_ddsgMaxRecords = a});

-- | An optional pagination token provided by a previous
-- DescribeDBSubnetGroups request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
ddsgMarker :: Lens' DescribeDBSubnetGroups (Maybe Text)
ddsgMarker = lens _ddsgMarker (\ s a -> s{_ddsgMarker = a});

instance AWSPager DescribeDBSubnetGroups where
        page rq rs
          | stop (rs ^. ddsgrMarker) = Nothing
          | stop (rs ^. ddsgrDBSubnetGroups) = Nothing
          | otherwise =
            Just $ rq & ddsgMarker .~ rs ^. ddsgrMarker

instance AWSRequest DescribeDBSubnetGroups where
        type Sv DescribeDBSubnetGroups = RDS
        type Rs DescribeDBSubnetGroups =
             DescribeDBSubnetGroupsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeDBSubnetGroupsResult"
              (\ s h x ->
                 DescribeDBSubnetGroupsResponse' <$>
                   (x .@? "DBSubnetGroups" .!@ mempty >>=
                      may (parseXMLList "DBSubnetGroup"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeDBSubnetGroups where
        toHeaders = const mempty

instance ToPath DescribeDBSubnetGroups where
        toPath = const "/"

instance ToQuery DescribeDBSubnetGroups where
        toQuery DescribeDBSubnetGroups'{..}
          = mconcat
              ["Action" =:
                 ("DescribeDBSubnetGroups" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBSubnetGroupName" =: _ddsgDBSubnetGroupName,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _ddsgFilters),
               "MaxRecords" =: _ddsgMaxRecords,
               "Marker" =: _ddsgMarker]

-- | Contains the result of a successful invocation of the
-- DescribeDBSubnetGroups action.
--
-- /See:/ 'describeDBSubnetGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddsgrDBSubnetGroups'
--
-- * 'ddsgrMarker'
--
-- * 'ddsgrStatus'
data DescribeDBSubnetGroupsResponse = DescribeDBSubnetGroupsResponse'
    { _ddsgrDBSubnetGroups :: !(Maybe [DBSubnetGroup])
    , _ddsgrMarker         :: !(Maybe Text)
    , _ddsgrStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBSubnetGroupsResponse' smart constructor.
describeDBSubnetGroupsResponse :: Int -> DescribeDBSubnetGroupsResponse
describeDBSubnetGroupsResponse pStatus =
    DescribeDBSubnetGroupsResponse'
    { _ddsgrDBSubnetGroups = Nothing
    , _ddsgrMarker = Nothing
    , _ddsgrStatus = pStatus
    }

-- | A list of DBSubnetGroup instances.
ddsgrDBSubnetGroups :: Lens' DescribeDBSubnetGroupsResponse [DBSubnetGroup]
ddsgrDBSubnetGroups = lens _ddsgrDBSubnetGroups (\ s a -> s{_ddsgrDBSubnetGroups = a}) . _Default;

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
ddsgrMarker :: Lens' DescribeDBSubnetGroupsResponse (Maybe Text)
ddsgrMarker = lens _ddsgrMarker (\ s a -> s{_ddsgrMarker = a});

-- | FIXME: Undocumented member.
ddsgrStatus :: Lens' DescribeDBSubnetGroupsResponse Int
ddsgrStatus = lens _ddsgrStatus (\ s a -> s{_ddsgrStatus = a});

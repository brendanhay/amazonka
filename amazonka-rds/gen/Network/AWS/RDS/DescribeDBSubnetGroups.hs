{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBSubnetGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of DBSubnetGroup descriptions. If a DBSubnetGroupName is
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
    , ddsgrqDBSubnetGroupName
    , ddsgrqFilters
    , ddsgrqMaxRecords
    , ddsgrqMarker

    -- * Response
    , DescribeDBSubnetGroupsResponse
    -- ** Response constructor
    , describeDBSubnetGroupsResponse
    -- ** Response lenses
    , ddsgrsDBSubnetGroups
    , ddsgrsMarker
    , ddsgrsStatus
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
-- * 'ddsgrqDBSubnetGroupName'
--
-- * 'ddsgrqFilters'
--
-- * 'ddsgrqMaxRecords'
--
-- * 'ddsgrqMarker'
data DescribeDBSubnetGroups = DescribeDBSubnetGroups'
    { _ddsgrqDBSubnetGroupName :: !(Maybe Text)
    , _ddsgrqFilters           :: !(Maybe [Filter])
    , _ddsgrqMaxRecords        :: !(Maybe Int)
    , _ddsgrqMarker            :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBSubnetGroups' smart constructor.
describeDBSubnetGroups :: DescribeDBSubnetGroups
describeDBSubnetGroups =
    DescribeDBSubnetGroups'
    { _ddsgrqDBSubnetGroupName = Nothing
    , _ddsgrqFilters = Nothing
    , _ddsgrqMaxRecords = Nothing
    , _ddsgrqMarker = Nothing
    }

-- | The name of the DB subnet group to return details for.
ddsgrqDBSubnetGroupName :: Lens' DescribeDBSubnetGroups (Maybe Text)
ddsgrqDBSubnetGroupName = lens _ddsgrqDBSubnetGroupName (\ s a -> s{_ddsgrqDBSubnetGroupName = a});

-- | This parameter is not currently supported.
ddsgrqFilters :: Lens' DescribeDBSubnetGroups [Filter]
ddsgrqFilters = lens _ddsgrqFilters (\ s a -> s{_ddsgrqFilters = a}) . _Default;

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results may be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
ddsgrqMaxRecords :: Lens' DescribeDBSubnetGroups (Maybe Int)
ddsgrqMaxRecords = lens _ddsgrqMaxRecords (\ s a -> s{_ddsgrqMaxRecords = a});

-- | An optional pagination token provided by a previous
-- DescribeDBSubnetGroups request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
ddsgrqMarker :: Lens' DescribeDBSubnetGroups (Maybe Text)
ddsgrqMarker = lens _ddsgrqMarker (\ s a -> s{_ddsgrqMarker = a});

instance AWSPager DescribeDBSubnetGroups where
        page rq rs
          | stop (rs ^. ddsgrsMarker) = Nothing
          | stop (rs ^. ddsgrsDBSubnetGroups) = Nothing
          | otherwise =
            Just $ rq & ddsgrqMarker .~ rs ^. ddsgrsMarker

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
               "DBSubnetGroupName" =: _ddsgrqDBSubnetGroupName,
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _ddsgrqFilters),
               "MaxRecords" =: _ddsgrqMaxRecords,
               "Marker" =: _ddsgrqMarker]

-- | Contains the result of a successful invocation of the
-- DescribeDBSubnetGroups action.
--
-- /See:/ 'describeDBSubnetGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddsgrsDBSubnetGroups'
--
-- * 'ddsgrsMarker'
--
-- * 'ddsgrsStatus'
data DescribeDBSubnetGroupsResponse = DescribeDBSubnetGroupsResponse'
    { _ddsgrsDBSubnetGroups :: !(Maybe [DBSubnetGroup])
    , _ddsgrsMarker         :: !(Maybe Text)
    , _ddsgrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBSubnetGroupsResponse' smart constructor.
describeDBSubnetGroupsResponse :: Int -> DescribeDBSubnetGroupsResponse
describeDBSubnetGroupsResponse pStatus_ =
    DescribeDBSubnetGroupsResponse'
    { _ddsgrsDBSubnetGroups = Nothing
    , _ddsgrsMarker = Nothing
    , _ddsgrsStatus = pStatus_
    }

-- | A list of DBSubnetGroup instances.
ddsgrsDBSubnetGroups :: Lens' DescribeDBSubnetGroupsResponse [DBSubnetGroup]
ddsgrsDBSubnetGroups = lens _ddsgrsDBSubnetGroups (\ s a -> s{_ddsgrsDBSubnetGroups = a}) . _Default;

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
ddsgrsMarker :: Lens' DescribeDBSubnetGroupsResponse (Maybe Text)
ddsgrsMarker = lens _ddsgrsMarker (\ s a -> s{_ddsgrsMarker = a});

-- | FIXME: Undocumented member.
ddsgrsStatus :: Lens' DescribeDBSubnetGroupsResponse Int
ddsgrsStatus = lens _ddsgrsStatus (\ s a -> s{_ddsgrsStatus = a});

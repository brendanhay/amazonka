{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBSecurityGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DBSecurityGroup@ descriptions. If a
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
    , ddbsgrqFilters
    , ddbsgrqMaxRecords
    , ddbsgrqMarker
    , ddbsgrqDBSecurityGroupName

    -- * Response
    , DescribeDBSecurityGroupsResponse
    -- ** Response constructor
    , describeDBSecurityGroupsResponse
    -- ** Response lenses
    , ddbsgrsDBSecurityGroups
    , ddbsgrsMarker
    , ddbsgrsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeDBSecurityGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbsgrqFilters'
--
-- * 'ddbsgrqMaxRecords'
--
-- * 'ddbsgrqMarker'
--
-- * 'ddbsgrqDBSecurityGroupName'
data DescribeDBSecurityGroups = DescribeDBSecurityGroups'
    { _ddbsgrqFilters             :: !(Maybe [Filter])
    , _ddbsgrqMaxRecords          :: !(Maybe Int)
    , _ddbsgrqMarker              :: !(Maybe Text)
    , _ddbsgrqDBSecurityGroupName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBSecurityGroups' smart constructor.
describeDBSecurityGroups :: DescribeDBSecurityGroups
describeDBSecurityGroups =
    DescribeDBSecurityGroups'
    { _ddbsgrqFilters = Nothing
    , _ddbsgrqMaxRecords = Nothing
    , _ddbsgrqMarker = Nothing
    , _ddbsgrqDBSecurityGroupName = Nothing
    }

-- | This parameter is not currently supported.
ddbsgrqFilters :: Lens' DescribeDBSecurityGroups [Filter]
ddbsgrqFilters = lens _ddbsgrqFilters (\ s a -> s{_ddbsgrqFilters = a}) . _Default;

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results may be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
ddbsgrqMaxRecords :: Lens' DescribeDBSecurityGroups (Maybe Int)
ddbsgrqMaxRecords = lens _ddbsgrqMaxRecords (\ s a -> s{_ddbsgrqMaxRecords = a});

-- | An optional pagination token provided by a previous
-- DescribeDBSecurityGroups request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
ddbsgrqMarker :: Lens' DescribeDBSecurityGroups (Maybe Text)
ddbsgrqMarker = lens _ddbsgrqMarker (\ s a -> s{_ddbsgrqMarker = a});

-- | The name of the DB security group to return details for.
ddbsgrqDBSecurityGroupName :: Lens' DescribeDBSecurityGroups (Maybe Text)
ddbsgrqDBSecurityGroupName = lens _ddbsgrqDBSecurityGroupName (\ s a -> s{_ddbsgrqDBSecurityGroupName = a});

instance AWSPager DescribeDBSecurityGroups where
        page rq rs
          | stop (rs ^. ddbsgrsMarker) = Nothing
          | stop (rs ^. ddbsgrsDBSecurityGroups) = Nothing
          | otherwise =
            Just $ rq & ddbsgrqMarker .~ rs ^. ddbsgrsMarker

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
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

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
                 toQuery (toQueryList "Filter" <$> _ddbsgrqFilters),
               "MaxRecords" =: _ddbsgrqMaxRecords,
               "Marker" =: _ddbsgrqMarker,
               "DBSecurityGroupName" =: _ddbsgrqDBSecurityGroupName]

-- | Contains the result of a successful invocation of the
-- DescribeDBSecurityGroups action.
--
-- /See:/ 'describeDBSecurityGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbsgrsDBSecurityGroups'
--
-- * 'ddbsgrsMarker'
--
-- * 'ddbsgrsStatus'
data DescribeDBSecurityGroupsResponse = DescribeDBSecurityGroupsResponse'
    { _ddbsgrsDBSecurityGroups :: !(Maybe [DBSecurityGroup])
    , _ddbsgrsMarker           :: !(Maybe Text)
    , _ddbsgrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBSecurityGroupsResponse' smart constructor.
describeDBSecurityGroupsResponse :: Int -> DescribeDBSecurityGroupsResponse
describeDBSecurityGroupsResponse pStatus =
    DescribeDBSecurityGroupsResponse'
    { _ddbsgrsDBSecurityGroups = Nothing
    , _ddbsgrsMarker = Nothing
    , _ddbsgrsStatus = pStatus
    }

-- | A list of DBSecurityGroup instances.
ddbsgrsDBSecurityGroups :: Lens' DescribeDBSecurityGroupsResponse [DBSecurityGroup]
ddbsgrsDBSecurityGroups = lens _ddbsgrsDBSecurityGroups (\ s a -> s{_ddbsgrsDBSecurityGroups = a}) . _Default;

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
ddbsgrsMarker :: Lens' DescribeDBSecurityGroupsResponse (Maybe Text)
ddbsgrsMarker = lens _ddbsgrsMarker (\ s a -> s{_ddbsgrsMarker = a});

-- | FIXME: Undocumented member.
ddbsgrsStatus :: Lens' DescribeDBSecurityGroupsResponse Int
ddbsgrsStatus = lens _ddbsgrsStatus (\ s a -> s{_ddbsgrsStatus = a});

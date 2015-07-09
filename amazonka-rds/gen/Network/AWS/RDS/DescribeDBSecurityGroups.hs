{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

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
    , ddbsgFilters
    , ddbsgMaxRecords
    , ddbsgMarker
    , ddbsgDBSecurityGroupName

    -- * Response
    , DescribeDBSecurityGroupsResponse
    -- ** Response constructor
    , describeDBSecurityGroupsResponse
    -- ** Response lenses
    , ddbsgrDBSecurityGroups
    , ddbsgrMarker
    , ddbsgrStatus
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
-- * 'ddbsgFilters'
--
-- * 'ddbsgMaxRecords'
--
-- * 'ddbsgMarker'
--
-- * 'ddbsgDBSecurityGroupName'
data DescribeDBSecurityGroups = DescribeDBSecurityGroups'
    { _ddbsgFilters             :: !(Maybe [Filter])
    , _ddbsgMaxRecords          :: !(Maybe Int)
    , _ddbsgMarker              :: !(Maybe Text)
    , _ddbsgDBSecurityGroupName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBSecurityGroups' smart constructor.
describeDBSecurityGroups :: DescribeDBSecurityGroups
describeDBSecurityGroups =
    DescribeDBSecurityGroups'
    { _ddbsgFilters = Nothing
    , _ddbsgMaxRecords = Nothing
    , _ddbsgMarker = Nothing
    , _ddbsgDBSecurityGroupName = Nothing
    }

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

instance AWSPager DescribeDBSecurityGroups where
        page rq rs
          | stop (rs ^. ddbsgrMarker) = Nothing
          | stop (rs ^. ddbsgrDBSecurityGroups) = Nothing
          | otherwise =
            Just $ rq & ddbsgMarker .~ rs ^. ddbsgrMarker

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
                 toQuery (toQueryList "Filter" <$> _ddbsgFilters),
               "MaxRecords" =: _ddbsgMaxRecords,
               "Marker" =: _ddbsgMarker,
               "DBSecurityGroupName" =: _ddbsgDBSecurityGroupName]

-- | Contains the result of a successful invocation of the
-- DescribeDBSecurityGroups action.
--
-- /See:/ 'describeDBSecurityGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbsgrDBSecurityGroups'
--
-- * 'ddbsgrMarker'
--
-- * 'ddbsgrStatus'
data DescribeDBSecurityGroupsResponse = DescribeDBSecurityGroupsResponse'
    { _ddbsgrDBSecurityGroups :: !(Maybe [DBSecurityGroup])
    , _ddbsgrMarker           :: !(Maybe Text)
    , _ddbsgrStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBSecurityGroupsResponse' smart constructor.
describeDBSecurityGroupsResponse :: Int -> DescribeDBSecurityGroupsResponse
describeDBSecurityGroupsResponse pStatus =
    DescribeDBSecurityGroupsResponse'
    { _ddbsgrDBSecurityGroups = Nothing
    , _ddbsgrMarker = Nothing
    , _ddbsgrStatus = pStatus
    }

-- | A list of DBSecurityGroup instances.
ddbsgrDBSecurityGroups :: Lens' DescribeDBSecurityGroupsResponse [DBSecurityGroup]
ddbsgrDBSecurityGroups = lens _ddbsgrDBSecurityGroups (\ s a -> s{_ddbsgrDBSecurityGroups = a}) . _Default;

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
ddbsgrMarker :: Lens' DescribeDBSecurityGroupsResponse (Maybe Text)
ddbsgrMarker = lens _ddbsgrMarker (\ s a -> s{_ddbsgrMarker = a});

-- | FIXME: Undocumented member.
ddbsgrStatus :: Lens' DescribeDBSecurityGroupsResponse Int
ddbsgrStatus = lens _ddbsgrStatus (\ s a -> s{_ddbsgrStatus = a});

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeDBParameterGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @DBParameterGroup@ descriptions. If a
-- @DBParameterGroupName@ is specified, the list will contain only the
-- description of the specified DB parameter group.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeDBParameterGroups.html>
module Network.AWS.RDS.DescribeDBParameterGroups
    (
    -- * Request
      DescribeDBParameterGroups
    -- ** Request constructor
    , describeDBParameterGroups
    -- ** Request lenses
    , ddpgrqFilters
    , ddpgrqDBParameterGroupName
    , ddpgrqMaxRecords
    , ddpgrqMarker

    -- * Response
    , DescribeDBParameterGroupsResponse
    -- ** Response constructor
    , describeDBParameterGroupsResponse
    -- ** Response lenses
    , ddpgrsMarker
    , ddpgrsDBParameterGroups
    , ddpgrsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeDBParameterGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddpgrqFilters'
--
-- * 'ddpgrqDBParameterGroupName'
--
-- * 'ddpgrqMaxRecords'
--
-- * 'ddpgrqMarker'
data DescribeDBParameterGroups = DescribeDBParameterGroups'
    { _ddpgrqFilters              :: !(Maybe [Filter])
    , _ddpgrqDBParameterGroupName :: !(Maybe Text)
    , _ddpgrqMaxRecords           :: !(Maybe Int)
    , _ddpgrqMarker               :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBParameterGroups' smart constructor.
describeDBParameterGroups :: DescribeDBParameterGroups
describeDBParameterGroups =
    DescribeDBParameterGroups'
    { _ddpgrqFilters = Nothing
    , _ddpgrqDBParameterGroupName = Nothing
    , _ddpgrqMaxRecords = Nothing
    , _ddpgrqMarker = Nothing
    }

-- | This parameter is not currently supported.
ddpgrqFilters :: Lens' DescribeDBParameterGroups [Filter]
ddpgrqFilters = lens _ddpgrqFilters (\ s a -> s{_ddpgrqFilters = a}) . _Default;

-- | The name of a specific DB parameter group to return details for.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
ddpgrqDBParameterGroupName :: Lens' DescribeDBParameterGroups (Maybe Text)
ddpgrqDBParameterGroupName = lens _ddpgrqDBParameterGroupName (\ s a -> s{_ddpgrqDBParameterGroupName = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results may be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
ddpgrqMaxRecords :: Lens' DescribeDBParameterGroups (Maybe Int)
ddpgrqMaxRecords = lens _ddpgrqMaxRecords (\ s a -> s{_ddpgrqMaxRecords = a});

-- | An optional pagination token provided by a previous
-- @DescribeDBParameterGroups@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
ddpgrqMarker :: Lens' DescribeDBParameterGroups (Maybe Text)
ddpgrqMarker = lens _ddpgrqMarker (\ s a -> s{_ddpgrqMarker = a});

instance AWSPager DescribeDBParameterGroups where
        page rq rs
          | stop (rs ^. ddpgrsMarker) = Nothing
          | stop (rs ^. ddpgrsDBParameterGroups) = Nothing
          | otherwise =
            Just $ rq & ddpgrqMarker .~ rs ^. ddpgrsMarker

instance AWSRequest DescribeDBParameterGroups where
        type Sv DescribeDBParameterGroups = RDS
        type Rs DescribeDBParameterGroups =
             DescribeDBParameterGroupsResponse
        request = post
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
                 toQuery (toQueryList "Filter" <$> _ddpgrqFilters),
               "DBParameterGroupName" =:
                 _ddpgrqDBParameterGroupName,
               "MaxRecords" =: _ddpgrqMaxRecords,
               "Marker" =: _ddpgrqMarker]

-- | Contains the result of a successful invocation of the
-- DescribeDBParameterGroups action.
--
-- /See:/ 'describeDBParameterGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddpgrsMarker'
--
-- * 'ddpgrsDBParameterGroups'
--
-- * 'ddpgrsStatus'
data DescribeDBParameterGroupsResponse = DescribeDBParameterGroupsResponse'
    { _ddpgrsMarker            :: !(Maybe Text)
    , _ddpgrsDBParameterGroups :: !(Maybe [DBParameterGroup])
    , _ddpgrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeDBParameterGroupsResponse' smart constructor.
describeDBParameterGroupsResponse :: Int -> DescribeDBParameterGroupsResponse
describeDBParameterGroupsResponse pStatus =
    DescribeDBParameterGroupsResponse'
    { _ddpgrsMarker = Nothing
    , _ddpgrsDBParameterGroups = Nothing
    , _ddpgrsStatus = pStatus
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
ddpgrsMarker :: Lens' DescribeDBParameterGroupsResponse (Maybe Text)
ddpgrsMarker = lens _ddpgrsMarker (\ s a -> s{_ddpgrsMarker = a});

-- | A list of DBParameterGroup instances.
ddpgrsDBParameterGroups :: Lens' DescribeDBParameterGroupsResponse [DBParameterGroup]
ddpgrsDBParameterGroups = lens _ddpgrsDBParameterGroups (\ s a -> s{_ddpgrsDBParameterGroups = a}) . _Default;

-- | FIXME: Undocumented member.
ddpgrsStatus :: Lens' DescribeDBParameterGroupsResponse Int
ddpgrsStatus = lens _ddpgrsStatus (\ s a -> s{_ddpgrsStatus = a});

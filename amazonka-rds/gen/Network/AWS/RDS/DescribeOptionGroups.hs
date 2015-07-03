{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.RDS.DescribeOptionGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes the available option groups.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeOptionGroups.html>
module Network.AWS.RDS.DescribeOptionGroups
    (
    -- * Request
      DescribeOptionGroups
    -- ** Request constructor
    , describeOptionGroups
    -- ** Request lenses
    , dogFilters
    , dogEngineName
    , dogMajorEngineVersion
    , dogMaxRecords
    , dogMarker
    , dogOptionGroupName

    -- * Response
    , DescribeOptionGroupsResponse
    -- ** Response constructor
    , describeOptionGroupsResponse
    -- ** Response lenses
    , dogrMarker
    , dogrOptionGroupsList
    , dogrStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeOptionGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dogFilters'
--
-- * 'dogEngineName'
--
-- * 'dogMajorEngineVersion'
--
-- * 'dogMaxRecords'
--
-- * 'dogMarker'
--
-- * 'dogOptionGroupName'
data DescribeOptionGroups = DescribeOptionGroups'
    { _dogFilters            :: !(Maybe [Filter])
    , _dogEngineName         :: !(Maybe Text)
    , _dogMajorEngineVersion :: !(Maybe Text)
    , _dogMaxRecords         :: !(Maybe Int)
    , _dogMarker             :: !(Maybe Text)
    , _dogOptionGroupName    :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'DescribeOptionGroups' smart constructor.
describeOptionGroups :: DescribeOptionGroups
describeOptionGroups =
    DescribeOptionGroups'
    { _dogFilters = Nothing
    , _dogEngineName = Nothing
    , _dogMajorEngineVersion = Nothing
    , _dogMaxRecords = Nothing
    , _dogMarker = Nothing
    , _dogOptionGroupName = Nothing
    }

-- | This parameter is not currently supported.
dogFilters :: Lens' DescribeOptionGroups [Filter]
dogFilters = lens _dogFilters (\ s a -> s{_dogFilters = a}) . _Default;

-- | Filters the list of option groups to only include groups associated with
-- a specific database engine.
dogEngineName :: Lens' DescribeOptionGroups (Maybe Text)
dogEngineName = lens _dogEngineName (\ s a -> s{_dogEngineName = a});

-- | Filters the list of option groups to only include groups associated with
-- a specific database engine version. If specified, then EngineName must
-- also be specified.
dogMajorEngineVersion :: Lens' DescribeOptionGroups (Maybe Text)
dogMajorEngineVersion = lens _dogMajorEngineVersion (\ s a -> s{_dogMajorEngineVersion = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
dogMaxRecords :: Lens' DescribeOptionGroups (Maybe Int)
dogMaxRecords = lens _dogMaxRecords (\ s a -> s{_dogMaxRecords = a});

-- | An optional pagination token provided by a previous DescribeOptionGroups
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by @MaxRecords@.
dogMarker :: Lens' DescribeOptionGroups (Maybe Text)
dogMarker = lens _dogMarker (\ s a -> s{_dogMarker = a});

-- | The name of the option group to describe. Cannot be supplied together
-- with EngineName or MajorEngineVersion.
dogOptionGroupName :: Lens' DescribeOptionGroups (Maybe Text)
dogOptionGroupName = lens _dogOptionGroupName (\ s a -> s{_dogOptionGroupName = a});

instance AWSPager DescribeOptionGroups where
        page rq rs
          | stop (rs ^. dogrMarker) = Nothing
          | stop (rs ^. dogrOptionGroupsList) = Nothing
          | otherwise =
            Just $ rq & dogMarker .~ rs ^. dogrMarker

instance AWSRequest DescribeOptionGroups where
        type Sv DescribeOptionGroups = RDS
        type Rs DescribeOptionGroups =
             DescribeOptionGroupsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeOptionGroupsResult"
              (\ s h x ->
                 DescribeOptionGroupsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "OptionGroupsList" .!@ mempty >>=
                        may (parseXMLList "OptionGroup"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeOptionGroups where
        toHeaders = const mempty

instance ToPath DescribeOptionGroups where
        toPath = const "/"

instance ToQuery DescribeOptionGroups where
        toQuery DescribeOptionGroups'{..}
          = mconcat
              ["Action" =: ("DescribeOptionGroups" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _dogFilters),
               "EngineName" =: _dogEngineName,
               "MajorEngineVersion" =: _dogMajorEngineVersion,
               "MaxRecords" =: _dogMaxRecords,
               "Marker" =: _dogMarker,
               "OptionGroupName" =: _dogOptionGroupName]

-- | List of option groups.
--
-- /See:/ 'describeOptionGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dogrMarker'
--
-- * 'dogrOptionGroupsList'
--
-- * 'dogrStatus'
data DescribeOptionGroupsResponse = DescribeOptionGroupsResponse'
    { _dogrMarker           :: !(Maybe Text)
    , _dogrOptionGroupsList :: !(Maybe [OptionGroup])
    , _dogrStatus           :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeOptionGroupsResponse' smart constructor.
describeOptionGroupsResponse :: Int -> DescribeOptionGroupsResponse
describeOptionGroupsResponse pStatus =
    DescribeOptionGroupsResponse'
    { _dogrMarker = Nothing
    , _dogrOptionGroupsList = Nothing
    , _dogrStatus = pStatus
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
dogrMarker :: Lens' DescribeOptionGroupsResponse (Maybe Text)
dogrMarker = lens _dogrMarker (\ s a -> s{_dogrMarker = a});

-- | List of option groups.
dogrOptionGroupsList :: Lens' DescribeOptionGroupsResponse [OptionGroup]
dogrOptionGroupsList = lens _dogrOptionGroupsList (\ s a -> s{_dogrOptionGroupsList = a}) . _Default;

-- | FIXME: Undocumented member.
dogrStatus :: Lens' DescribeOptionGroupsResponse Int
dogrStatus = lens _dogrStatus (\ s a -> s{_dogrStatus = a});

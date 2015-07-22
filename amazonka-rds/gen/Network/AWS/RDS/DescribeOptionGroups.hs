{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeOptionGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the available option groups.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeOptionGroups.html>
module Network.AWS.RDS.DescribeOptionGroups
    (
    -- * Request
      DescribeOptionGroups
    -- ** Request constructor
    , describeOptionGroups
    -- ** Request lenses
    , dogrqFilters
    , dogrqEngineName
    , dogrqMajorEngineVersion
    , dogrqMaxRecords
    , dogrqMarker
    , dogrqOptionGroupName

    -- * Response
    , DescribeOptionGroupsResponse
    -- ** Response constructor
    , describeOptionGroupsResponse
    -- ** Response lenses
    , dogrsMarker
    , dogrsOptionGroupsList
    , dogrsStatus
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
-- * 'dogrqFilters'
--
-- * 'dogrqEngineName'
--
-- * 'dogrqMajorEngineVersion'
--
-- * 'dogrqMaxRecords'
--
-- * 'dogrqMarker'
--
-- * 'dogrqOptionGroupName'
data DescribeOptionGroups = DescribeOptionGroups'
    { _dogrqFilters            :: !(Maybe [Filter])
    , _dogrqEngineName         :: !(Maybe Text)
    , _dogrqMajorEngineVersion :: !(Maybe Text)
    , _dogrqMaxRecords         :: !(Maybe Int)
    , _dogrqMarker             :: !(Maybe Text)
    , _dogrqOptionGroupName    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeOptionGroups' smart constructor.
describeOptionGroups :: DescribeOptionGroups
describeOptionGroups =
    DescribeOptionGroups'
    { _dogrqFilters = Nothing
    , _dogrqEngineName = Nothing
    , _dogrqMajorEngineVersion = Nothing
    , _dogrqMaxRecords = Nothing
    , _dogrqMarker = Nothing
    , _dogrqOptionGroupName = Nothing
    }

-- | This parameter is not currently supported.
dogrqFilters :: Lens' DescribeOptionGroups [Filter]
dogrqFilters = lens _dogrqFilters (\ s a -> s{_dogrqFilters = a}) . _Default;

-- | Filters the list of option groups to only include groups associated with
-- a specific database engine.
dogrqEngineName :: Lens' DescribeOptionGroups (Maybe Text)
dogrqEngineName = lens _dogrqEngineName (\ s a -> s{_dogrqEngineName = a});

-- | Filters the list of option groups to only include groups associated with
-- a specific database engine version. If specified, then EngineName must
-- also be specified.
dogrqMajorEngineVersion :: Lens' DescribeOptionGroups (Maybe Text)
dogrqMajorEngineVersion = lens _dogrqMajorEngineVersion (\ s a -> s{_dogrqMajorEngineVersion = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20, maximum 100
dogrqMaxRecords :: Lens' DescribeOptionGroups (Maybe Int)
dogrqMaxRecords = lens _dogrqMaxRecords (\ s a -> s{_dogrqMaxRecords = a});

-- | An optional pagination token provided by a previous DescribeOptionGroups
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by @MaxRecords@.
dogrqMarker :: Lens' DescribeOptionGroups (Maybe Text)
dogrqMarker = lens _dogrqMarker (\ s a -> s{_dogrqMarker = a});

-- | The name of the option group to describe. Cannot be supplied together
-- with EngineName or MajorEngineVersion.
dogrqOptionGroupName :: Lens' DescribeOptionGroups (Maybe Text)
dogrqOptionGroupName = lens _dogrqOptionGroupName (\ s a -> s{_dogrqOptionGroupName = a});

instance AWSPager DescribeOptionGroups where
        page rq rs
          | stop (rs ^. dogrsMarker) = Nothing
          | stop (rs ^. dogrsOptionGroupsList) = Nothing
          | otherwise =
            Just $ rq & dogrqMarker .~ rs ^. dogrsMarker

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
                 toQuery (toQueryList "Filter" <$> _dogrqFilters),
               "EngineName" =: _dogrqEngineName,
               "MajorEngineVersion" =: _dogrqMajorEngineVersion,
               "MaxRecords" =: _dogrqMaxRecords,
               "Marker" =: _dogrqMarker,
               "OptionGroupName" =: _dogrqOptionGroupName]

-- | List of option groups.
--
-- /See:/ 'describeOptionGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dogrsMarker'
--
-- * 'dogrsOptionGroupsList'
--
-- * 'dogrsStatus'
data DescribeOptionGroupsResponse = DescribeOptionGroupsResponse'
    { _dogrsMarker           :: !(Maybe Text)
    , _dogrsOptionGroupsList :: !(Maybe [OptionGroup])
    , _dogrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeOptionGroupsResponse' smart constructor.
describeOptionGroupsResponse :: Int -> DescribeOptionGroupsResponse
describeOptionGroupsResponse pStatus_ =
    DescribeOptionGroupsResponse'
    { _dogrsMarker = Nothing
    , _dogrsOptionGroupsList = Nothing
    , _dogrsStatus = pStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
dogrsMarker :: Lens' DescribeOptionGroupsResponse (Maybe Text)
dogrsMarker = lens _dogrsMarker (\ s a -> s{_dogrsMarker = a});

-- | List of option groups.
dogrsOptionGroupsList :: Lens' DescribeOptionGroupsResponse [OptionGroup]
dogrsOptionGroupsList = lens _dogrsOptionGroupsList (\ s a -> s{_dogrsOptionGroupsList = a}) . _Default;

-- | FIXME: Undocumented member.
dogrsStatus :: Lens' DescribeOptionGroupsResponse Int
dogrsStatus = lens _dogrsStatus (\ s a -> s{_dogrsStatus = a});

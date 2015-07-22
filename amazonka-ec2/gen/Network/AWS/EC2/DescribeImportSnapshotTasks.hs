{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeImportSnapshotTasks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes your import snapshot tasks.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeImportSnapshotTasks.html>
module Network.AWS.EC2.DescribeImportSnapshotTasks
    (
    -- * Request
      DescribeImportSnapshotTasks
    -- ** Request constructor
    , describeImportSnapshotTasks
    -- ** Request lenses
    , distrqFilters
    , distrqImportTaskIds
    , distrqNextToken
    , distrqDryRun
    , distrqMaxResults

    -- * Response
    , DescribeImportSnapshotTasksResponse
    -- ** Response constructor
    , describeImportSnapshotTasksResponse
    -- ** Response lenses
    , distrsNextToken
    , distrsImportSnapshotTasks
    , distrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeImportSnapshotTasks' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'distrqFilters'
--
-- * 'distrqImportTaskIds'
--
-- * 'distrqNextToken'
--
-- * 'distrqDryRun'
--
-- * 'distrqMaxResults'
data DescribeImportSnapshotTasks = DescribeImportSnapshotTasks'
    { _distrqFilters       :: !(Maybe [Filter])
    , _distrqImportTaskIds :: !(Maybe [Text])
    , _distrqNextToken     :: !(Maybe Text)
    , _distrqDryRun        :: !(Maybe Bool)
    , _distrqMaxResults    :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeImportSnapshotTasks' smart constructor.
describeImportSnapshotTasks :: DescribeImportSnapshotTasks
describeImportSnapshotTasks =
    DescribeImportSnapshotTasks'
    { _distrqFilters = Nothing
    , _distrqImportTaskIds = Nothing
    , _distrqNextToken = Nothing
    , _distrqDryRun = Nothing
    , _distrqMaxResults = Nothing
    }

-- | One or more filters.
distrqFilters :: Lens' DescribeImportSnapshotTasks [Filter]
distrqFilters = lens _distrqFilters (\ s a -> s{_distrqFilters = a}) . _Default;

-- | A list of import snapshot task IDs.
distrqImportTaskIds :: Lens' DescribeImportSnapshotTasks [Text]
distrqImportTaskIds = lens _distrqImportTaskIds (\ s a -> s{_distrqImportTaskIds = a}) . _Default;

-- | A token that indicates the next page of results.
distrqNextToken :: Lens' DescribeImportSnapshotTasks (Maybe Text)
distrqNextToken = lens _distrqNextToken (\ s a -> s{_distrqNextToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
distrqDryRun :: Lens' DescribeImportSnapshotTasks (Maybe Bool)
distrqDryRun = lens _distrqDryRun (\ s a -> s{_distrqDryRun = a});

-- | The maximum number of results to return in a single request.
distrqMaxResults :: Lens' DescribeImportSnapshotTasks (Maybe Int)
distrqMaxResults = lens _distrqMaxResults (\ s a -> s{_distrqMaxResults = a});

instance AWSRequest DescribeImportSnapshotTasks where
        type Sv DescribeImportSnapshotTasks = EC2
        type Rs DescribeImportSnapshotTasks =
             DescribeImportSnapshotTasksResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeImportSnapshotTasksResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "importSnapshotTaskSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeImportSnapshotTasks where
        toHeaders = const mempty

instance ToPath DescribeImportSnapshotTasks where
        toPath = const "/"

instance ToQuery DescribeImportSnapshotTasks where
        toQuery DescribeImportSnapshotTasks'{..}
          = mconcat
              ["Action" =:
                 ("DescribeImportSnapshotTasks" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _distrqFilters),
               toQuery
                 (toQueryList "ImportTaskId" <$>
                    _distrqImportTaskIds),
               "NextToken" =: _distrqNextToken,
               "DryRun" =: _distrqDryRun,
               "MaxResults" =: _distrqMaxResults]

-- | /See:/ 'describeImportSnapshotTasksResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'distrsNextToken'
--
-- * 'distrsImportSnapshotTasks'
--
-- * 'distrsStatus'
data DescribeImportSnapshotTasksResponse = DescribeImportSnapshotTasksResponse'
    { _distrsNextToken           :: !(Maybe Text)
    , _distrsImportSnapshotTasks :: !(Maybe [ImportSnapshotTask])
    , _distrsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeImportSnapshotTasksResponse' smart constructor.
describeImportSnapshotTasksResponse :: Int -> DescribeImportSnapshotTasksResponse
describeImportSnapshotTasksResponse pStatus =
    DescribeImportSnapshotTasksResponse'
    { _distrsNextToken = Nothing
    , _distrsImportSnapshotTasks = Nothing
    , _distrsStatus = pStatus
    }

-- | The token to use to get the next page of results. This value is @null@
-- when there are no more results to return.
distrsNextToken :: Lens' DescribeImportSnapshotTasksResponse (Maybe Text)
distrsNextToken = lens _distrsNextToken (\ s a -> s{_distrsNextToken = a});

-- | A list of zero or more import snapshot tasks that are currently active
-- or were completed or canceled in the previous 7 days.
distrsImportSnapshotTasks :: Lens' DescribeImportSnapshotTasksResponse [ImportSnapshotTask]
distrsImportSnapshotTasks = lens _distrsImportSnapshotTasks (\ s a -> s{_distrsImportSnapshotTasks = a}) . _Default;

-- | FIXME: Undocumented member.
distrsStatus :: Lens' DescribeImportSnapshotTasksResponse Int
distrsStatus = lens _distrsStatus (\ s a -> s{_distrsStatus = a});

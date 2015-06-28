{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DescribeImportSnapshotTasks
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

-- | Describes your import snapshot tasks.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeImportSnapshotTasks.html>
module Network.AWS.EC2.DescribeImportSnapshotTasks
    (
    -- * Request
      DescribeImportSnapshotTasks
    -- ** Request constructor
    , describeImportSnapshotTasks
    -- ** Request lenses
    , distFilters
    , distImportTaskIds
    , distNextToken
    , distDryRun
    , distMaxResults

    -- * Response
    , DescribeImportSnapshotTasksResponse
    -- ** Response constructor
    , describeImportSnapshotTasksResponse
    -- ** Response lenses
    , distrNextToken
    , distrImportSnapshotTasks
    , distrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeImportSnapshotTasks' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'distFilters'
--
-- * 'distImportTaskIds'
--
-- * 'distNextToken'
--
-- * 'distDryRun'
--
-- * 'distMaxResults'
data DescribeImportSnapshotTasks = DescribeImportSnapshotTasks'
    { _distFilters       :: !(Maybe [Filter])
    , _distImportTaskIds :: !(Maybe [Text])
    , _distNextToken     :: !(Maybe Text)
    , _distDryRun        :: !(Maybe Bool)
    , _distMaxResults    :: !(Maybe Int)
    } deriving (Eq,Read,Show)

-- | 'DescribeImportSnapshotTasks' smart constructor.
describeImportSnapshotTasks :: DescribeImportSnapshotTasks
describeImportSnapshotTasks =
    DescribeImportSnapshotTasks'
    { _distFilters = Nothing
    , _distImportTaskIds = Nothing
    , _distNextToken = Nothing
    , _distDryRun = Nothing
    , _distMaxResults = Nothing
    }

-- | One or more filters.
distFilters :: Lens' DescribeImportSnapshotTasks [Filter]
distFilters = lens _distFilters (\ s a -> s{_distFilters = a}) . _Default;

-- | A list of import snapshot task IDs.
distImportTaskIds :: Lens' DescribeImportSnapshotTasks [Text]
distImportTaskIds = lens _distImportTaskIds (\ s a -> s{_distImportTaskIds = a}) . _Default;

-- | A token that indicates the next page of results.
distNextToken :: Lens' DescribeImportSnapshotTasks (Maybe Text)
distNextToken = lens _distNextToken (\ s a -> s{_distNextToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
distDryRun :: Lens' DescribeImportSnapshotTasks (Maybe Bool)
distDryRun = lens _distDryRun (\ s a -> s{_distDryRun = a});

-- | The maximum number of results to return in a single request.
distMaxResults :: Lens' DescribeImportSnapshotTasks (Maybe Int)
distMaxResults = lens _distMaxResults (\ s a -> s{_distMaxResults = a});

instance AWSRequest DescribeImportSnapshotTasks where
        type Sv DescribeImportSnapshotTasks = EC2
        type Rs DescribeImportSnapshotTasks =
             DescribeImportSnapshotTasksResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeImportSnapshotTasksResponse' <$>
                   (x .@? "nextToken") <*> (may (parseXMLList "item") x)
                     <*> (pure s))

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
               toQuery (toQueryList "Filter" <$> _distFilters),
               toQuery
                 (toQueryList "ImportTaskId" <$> _distImportTaskIds),
               "NextToken" =: _distNextToken,
               "DryRun" =: _distDryRun,
               "MaxResults" =: _distMaxResults]

-- | /See:/ 'describeImportSnapshotTasksResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'distrNextToken'
--
-- * 'distrImportSnapshotTasks'
--
-- * 'distrStatus'
data DescribeImportSnapshotTasksResponse = DescribeImportSnapshotTasksResponse'
    { _distrNextToken           :: !(Maybe Text)
    , _distrImportSnapshotTasks :: !(Maybe [ImportSnapshotTask])
    , _distrStatus              :: !Status
    } deriving (Eq,Read,Show)

-- | 'DescribeImportSnapshotTasksResponse' smart constructor.
describeImportSnapshotTasksResponse :: Status -> DescribeImportSnapshotTasksResponse
describeImportSnapshotTasksResponse pStatus =
    DescribeImportSnapshotTasksResponse'
    { _distrNextToken = Nothing
    , _distrImportSnapshotTasks = Nothing
    , _distrStatus = pStatus
    }

-- | The token to use to get the next page of results. This value is @null@
-- when there are no more results to return.
distrNextToken :: Lens' DescribeImportSnapshotTasksResponse (Maybe Text)
distrNextToken = lens _distrNextToken (\ s a -> s{_distrNextToken = a});

-- | A list of zero or more import snapshot tasks that are currently active
-- or were completed or canceled in the previous 7 days.
distrImportSnapshotTasks :: Lens' DescribeImportSnapshotTasksResponse [ImportSnapshotTask]
distrImportSnapshotTasks = lens _distrImportSnapshotTasks (\ s a -> s{_distrImportSnapshotTasks = a}) . _Default;

-- | FIXME: Undocumented member.
distrStatus :: Lens' DescribeImportSnapshotTasksResponse Status
distrStatus = lens _distrStatus (\ s a -> s{_distrStatus = a});

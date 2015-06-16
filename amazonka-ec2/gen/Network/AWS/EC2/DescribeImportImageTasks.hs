{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.DescribeImportImageTasks
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

-- | Displays details about an import virtual machine or import snapshot
-- tasks that are already created.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeImportImageTasks.html>
module Network.AWS.EC2.DescribeImportImageTasks
    (
    -- * Request
      DescribeImportImageTasks
    -- ** Request constructor
    , describeImportImageTasks
    -- ** Request lenses
    , diitFilters
    , diitImportTaskIds
    , diitNextToken
    , diitDryRun
    , diitMaxResults

    -- * Response
    , DescribeImportImageTasksResponse
    -- ** Response constructor
    , describeImportImageTasksResponse
    -- ** Response lenses
    , diitrImportImageTasks
    , diitrNextToken
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'describeImportImageTasks' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diitFilters'
--
-- * 'diitImportTaskIds'
--
-- * 'diitNextToken'
--
-- * 'diitDryRun'
--
-- * 'diitMaxResults'
data DescribeImportImageTasks = DescribeImportImageTasks'{_diitFilters :: Maybe [Filter], _diitImportTaskIds :: Maybe [Text], _diitNextToken :: Maybe Text, _diitDryRun :: Maybe Bool, _diitMaxResults :: Maybe Int} deriving (Eq, Read, Show)

-- | 'DescribeImportImageTasks' smart constructor.
describeImportImageTasks :: DescribeImportImageTasks
describeImportImageTasks = DescribeImportImageTasks'{_diitFilters = Nothing, _diitImportTaskIds = Nothing, _diitNextToken = Nothing, _diitDryRun = Nothing, _diitMaxResults = Nothing};

-- | One or more filters.
diitFilters :: Lens' DescribeImportImageTasks [Filter]
diitFilters = lens _diitFilters (\ s a -> s{_diitFilters = a}) . _Default;

-- | A list of import image task IDs.
diitImportTaskIds :: Lens' DescribeImportImageTasks [Text]
diitImportTaskIds = lens _diitImportTaskIds (\ s a -> s{_diitImportTaskIds = a}) . _Default;

-- | A token that indicates the next page of results.
diitNextToken :: Lens' DescribeImportImageTasks (Maybe Text)
diitNextToken = lens _diitNextToken (\ s a -> s{_diitNextToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
diitDryRun :: Lens' DescribeImportImageTasks (Maybe Bool)
diitDryRun = lens _diitDryRun (\ s a -> s{_diitDryRun = a});

-- | The maximum number of results to return in a single request.
diitMaxResults :: Lens' DescribeImportImageTasks (Maybe Int)
diitMaxResults = lens _diitMaxResults (\ s a -> s{_diitMaxResults = a});

instance AWSRequest DescribeImportImageTasks where
        type Sv DescribeImportImageTasks = EC2
        type Rs DescribeImportImageTasks =
             DescribeImportImageTasksResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeImportImageTasksResponse' <$>
                   (may (parseXMLList "item") x) <*>
                     (x .@? "nextToken"))

instance ToHeaders DescribeImportImageTasks where
        toHeaders = const mempty

instance ToPath DescribeImportImageTasks where
        toPath = const "/"

instance ToQuery DescribeImportImageTasks where
        toQuery DescribeImportImageTasks'{..}
          = mconcat
              ["Action" =:
                 ("DescribeImportImageTasks" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _diitFilters),
               toQuery
                 (toQueryList "ImportTaskId" <$> _diitImportTaskIds),
               "NextToken" =: _diitNextToken,
               "DryRun" =: _diitDryRun,
               "MaxResults" =: _diitMaxResults]

-- | /See:/ 'describeImportImageTasksResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diitrImportImageTasks'
--
-- * 'diitrNextToken'
data DescribeImportImageTasksResponse = DescribeImportImageTasksResponse'{_diitrImportImageTasks :: Maybe [ImportImageTask], _diitrNextToken :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeImportImageTasksResponse' smart constructor.
describeImportImageTasksResponse :: DescribeImportImageTasksResponse
describeImportImageTasksResponse = DescribeImportImageTasksResponse'{_diitrImportImageTasks = Nothing, _diitrNextToken = Nothing};

-- | A list of zero or more import image tasks that are currently active or
-- were completed or canceled in the previous 7 days.
diitrImportImageTasks :: Lens' DescribeImportImageTasksResponse [ImportImageTask]
diitrImportImageTasks = lens _diitrImportImageTasks (\ s a -> s{_diitrImportImageTasks = a}) . _Default;

-- | The token to use to get the next page of results. This value is @null@
-- when there are no more results to return.
diitrNextToken :: Lens' DescribeImportImageTasksResponse (Maybe Text)
diitrNextToken = lens _diitrNextToken (\ s a -> s{_diitrNextToken = a});

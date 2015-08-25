{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeImportSnapshotTasks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your import snapshot tasks.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeImportSnapshotTasks.html AWS API Reference> for DescribeImportSnapshotTasks.
module Network.AWS.EC2.DescribeImportSnapshotTasks
    (
    -- * Creating a Request
      describeImportSnapshotTasks
    , DescribeImportSnapshotTasks
    -- * Request Lenses
    , distFilters
    , distImportTaskIds
    , distNextToken
    , distDryRun
    , distMaxResults

    -- * Destructuring the Response
    , describeImportSnapshotTasksResponse
    , DescribeImportSnapshotTasksResponse
    -- * Response Lenses
    , distrsNextToken
    , distrsImportSnapshotTasks
    , distrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeImportSnapshotTasks' smart constructor.
data DescribeImportSnapshotTasks = DescribeImportSnapshotTasks'
    { _distFilters       :: !(Maybe [Filter])
    , _distImportTaskIds :: !(Maybe [Text])
    , _distNextToken     :: !(Maybe Text)
    , _distDryRun        :: !(Maybe Bool)
    , _distMaxResults    :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeImportSnapshotTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
describeImportSnapshotTasks
    :: DescribeImportSnapshotTasks
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
distFilters = lens _distFilters (\ s a -> s{_distFilters = a}) . _Default . _Coerce;

-- | A list of import snapshot task IDs.
distImportTaskIds :: Lens' DescribeImportSnapshotTasks [Text]
distImportTaskIds = lens _distImportTaskIds (\ s a -> s{_distImportTaskIds = a}) . _Default . _Coerce;

-- | A token that indicates the next page of results.
distNextToken :: Lens' DescribeImportSnapshotTasks (Maybe Text)
distNextToken = lens _distNextToken (\ s a -> s{_distNextToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
distDryRun :: Lens' DescribeImportSnapshotTasks (Maybe Bool)
distDryRun = lens _distDryRun (\ s a -> s{_distDryRun = a});

-- | The maximum number of results to return in a single request.
distMaxResults :: Lens' DescribeImportSnapshotTasks (Maybe Int)
distMaxResults = lens _distMaxResults (\ s a -> s{_distMaxResults = a});

instance AWSRequest DescribeImportSnapshotTasks where
        type Rs DescribeImportSnapshotTasks =
             DescribeImportSnapshotTasksResponse
        request = postQuery eC2
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
               toQuery (toQueryList "Filters" <$> _distFilters),
               toQuery
                 (toQueryList "ImportTaskId" <$> _distImportTaskIds),
               "NextToken" =: _distNextToken,
               "DryRun" =: _distDryRun,
               "MaxResults" =: _distMaxResults]

-- | /See:/ 'describeImportSnapshotTasksResponse' smart constructor.
data DescribeImportSnapshotTasksResponse = DescribeImportSnapshotTasksResponse'
    { _distrsNextToken           :: !(Maybe Text)
    , _distrsImportSnapshotTasks :: !(Maybe [ImportSnapshotTask])
    , _distrsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeImportSnapshotTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'distrsNextToken'
--
-- * 'distrsImportSnapshotTasks'
--
-- * 'distrsStatus'
describeImportSnapshotTasksResponse
    :: Int -- ^ 'distrsStatus'
    -> DescribeImportSnapshotTasksResponse
describeImportSnapshotTasksResponse pStatus_ =
    DescribeImportSnapshotTasksResponse'
    { _distrsNextToken = Nothing
    , _distrsImportSnapshotTasks = Nothing
    , _distrsStatus = pStatus_
    }

-- | The token to use to get the next page of results. This value is 'null'
-- when there are no more results to return.
distrsNextToken :: Lens' DescribeImportSnapshotTasksResponse (Maybe Text)
distrsNextToken = lens _distrsNextToken (\ s a -> s{_distrsNextToken = a});

-- | A list of zero or more import snapshot tasks that are currently active
-- or were completed or canceled in the previous 7 days.
distrsImportSnapshotTasks :: Lens' DescribeImportSnapshotTasksResponse [ImportSnapshotTask]
distrsImportSnapshotTasks = lens _distrsImportSnapshotTasks (\ s a -> s{_distrsImportSnapshotTasks = a}) . _Default . _Coerce;

-- | The response status code.
distrsStatus :: Lens' DescribeImportSnapshotTasksResponse Int
distrsStatus = lens _distrsStatus (\ s a -> s{_distrsStatus = a});

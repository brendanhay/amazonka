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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your import snapshot tasks.
--
--
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
    , distrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeImportSnapshotTasks.
--
--
--
-- /See:/ 'describeImportSnapshotTasks' smart constructor.
data DescribeImportSnapshotTasks = DescribeImportSnapshotTasks'
  { _distFilters       :: !(Maybe [Filter])
  , _distImportTaskIds :: !(Maybe [Text])
  , _distNextToken     :: !(Maybe Text)
  , _distDryRun        :: !(Maybe Bool)
  , _distMaxResults    :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeImportSnapshotTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'distFilters' - One or more filters.
--
-- * 'distImportTaskIds' - A list of import snapshot task IDs.
--
-- * 'distNextToken' - A token that indicates the next page of results.
--
-- * 'distDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'distMaxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value.
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
distFilters = lens _distFilters (\ s a -> s{_distFilters = a}) . _Default . _Coerce

-- | A list of import snapshot task IDs.
distImportTaskIds :: Lens' DescribeImportSnapshotTasks [Text]
distImportTaskIds = lens _distImportTaskIds (\ s a -> s{_distImportTaskIds = a}) . _Default . _Coerce

-- | A token that indicates the next page of results.
distNextToken :: Lens' DescribeImportSnapshotTasks (Maybe Text)
distNextToken = lens _distNextToken (\ s a -> s{_distNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
distDryRun :: Lens' DescribeImportSnapshotTasks (Maybe Bool)
distDryRun = lens _distDryRun (\ s a -> s{_distDryRun = a})

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value.
distMaxResults :: Lens' DescribeImportSnapshotTasks (Maybe Int)
distMaxResults = lens _distMaxResults (\ s a -> s{_distMaxResults = a})

instance AWSRequest DescribeImportSnapshotTasks where
        type Rs DescribeImportSnapshotTasks =
             DescribeImportSnapshotTasksResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeImportSnapshotTasksResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "importSnapshotTaskSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeImportSnapshotTasks where

instance NFData DescribeImportSnapshotTasks where

instance ToHeaders DescribeImportSnapshotTasks where
        toHeaders = const mempty

instance ToPath DescribeImportSnapshotTasks where
        toPath = const "/"

instance ToQuery DescribeImportSnapshotTasks where
        toQuery DescribeImportSnapshotTasks'{..}
          = mconcat
              ["Action" =:
                 ("DescribeImportSnapshotTasks" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filters" <$> _distFilters),
               toQuery
                 (toQueryList "ImportTaskId" <$> _distImportTaskIds),
               "NextToken" =: _distNextToken,
               "DryRun" =: _distDryRun,
               "MaxResults" =: _distMaxResults]

-- | Contains the output for DescribeImportSnapshotTasks.
--
--
--
-- /See:/ 'describeImportSnapshotTasksResponse' smart constructor.
data DescribeImportSnapshotTasksResponse = DescribeImportSnapshotTasksResponse'
  { _distrsNextToken           :: !(Maybe Text)
  , _distrsImportSnapshotTasks :: !(Maybe [ImportSnapshotTask])
  , _distrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeImportSnapshotTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'distrsNextToken' - The token to use to get the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'distrsImportSnapshotTasks' - A list of zero or more import snapshot tasks that are currently active or were completed or canceled in the previous 7 days.
--
-- * 'distrsResponseStatus' - -- | The response status code.
describeImportSnapshotTasksResponse
    :: Int -- ^ 'distrsResponseStatus'
    -> DescribeImportSnapshotTasksResponse
describeImportSnapshotTasksResponse pResponseStatus_ =
  DescribeImportSnapshotTasksResponse'
    { _distrsNextToken = Nothing
    , _distrsImportSnapshotTasks = Nothing
    , _distrsResponseStatus = pResponseStatus_
    }


-- | The token to use to get the next page of results. This value is @null@ when there are no more results to return.
distrsNextToken :: Lens' DescribeImportSnapshotTasksResponse (Maybe Text)
distrsNextToken = lens _distrsNextToken (\ s a -> s{_distrsNextToken = a})

-- | A list of zero or more import snapshot tasks that are currently active or were completed or canceled in the previous 7 days.
distrsImportSnapshotTasks :: Lens' DescribeImportSnapshotTasksResponse [ImportSnapshotTask]
distrsImportSnapshotTasks = lens _distrsImportSnapshotTasks (\ s a -> s{_distrsImportSnapshotTasks = a}) . _Default . _Coerce

-- | -- | The response status code.
distrsResponseStatus :: Lens' DescribeImportSnapshotTasksResponse Int
distrsResponseStatus = lens _distrsResponseStatus (\ s a -> s{_distrsResponseStatus = a})

instance NFData DescribeImportSnapshotTasksResponse
         where

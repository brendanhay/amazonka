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
-- Module      : Network.AWS.CloudWatchLogs.DescribeExportTasks
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all the export tasks that are associated with the AWS account
-- making the request. The export tasks can be filtered based on 'TaskId'
-- or 'TaskStatus'.
--
-- By default, this operation returns up to 50 export tasks that satisfy
-- the specified filters. If there are more export tasks to list, the
-- response would contain a 'nextToken' value in the response body. You can
-- also limit the number of export tasks returned in the response by
-- specifying the 'limit' parameter in the request.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeExportTasks.html AWS API Reference> for DescribeExportTasks.
module Network.AWS.CloudWatchLogs.DescribeExportTasks
    (
    -- * Creating a Request
      describeExportTasks
    , DescribeExportTasks
    -- * Request Lenses
    , detTaskId
    , detNextToken
    , detLimit
    , detStatusCode

    -- * Destructuring the Response
    , describeExportTasksResponse
    , DescribeExportTasksResponse
    -- * Response Lenses
    , detrsNextToken
    , detrsExportTasks
    , detrsResponseStatus
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.CloudWatchLogs.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeExportTasks' smart constructor.
data DescribeExportTasks = DescribeExportTasks'
    { _detTaskId     :: !(Maybe Text)
    , _detNextToken  :: !(Maybe Text)
    , _detLimit      :: !(Maybe Nat)
    , _detStatusCode :: !(Maybe ExportTaskStatusCode)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeExportTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detTaskId'
--
-- * 'detNextToken'
--
-- * 'detLimit'
--
-- * 'detStatusCode'
describeExportTasks
    :: DescribeExportTasks
describeExportTasks =
    DescribeExportTasks'
    { _detTaskId = Nothing
    , _detNextToken = Nothing
    , _detLimit = Nothing
    , _detStatusCode = Nothing
    }

-- | Export task that matches the specified task Id will be returned. This
-- can result in zero or one export task.
detTaskId :: Lens' DescribeExportTasks (Maybe Text)
detTaskId = lens _detTaskId (\ s a -> s{_detTaskId = a});

-- | A string token used for pagination that points to the next page of
-- results. It must be a value obtained from the response of the previous
-- 'DescribeExportTasks' request.
detNextToken :: Lens' DescribeExportTasks (Maybe Text)
detNextToken = lens _detNextToken (\ s a -> s{_detNextToken = a});

-- | The maximum number of items returned in the response. If you don\'t
-- specify a value, the request would return up to 50 items.
detLimit :: Lens' DescribeExportTasks (Maybe Natural)
detLimit = lens _detLimit (\ s a -> s{_detLimit = a}) . mapping _Nat;

-- | All export tasks that matches the specified status code will be
-- returned. This can return zero or more export tasks.
detStatusCode :: Lens' DescribeExportTasks (Maybe ExportTaskStatusCode)
detStatusCode = lens _detStatusCode (\ s a -> s{_detStatusCode = a});

instance AWSRequest DescribeExportTasks where
        type Rs DescribeExportTasks =
             DescribeExportTasksResponse
        request = postJSON cloudWatchLogs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeExportTasksResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "exportTasks" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeExportTasks where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DescribeExportTasks" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeExportTasks where
        toJSON DescribeExportTasks'{..}
          = object
              (catMaybes
                 [("taskId" .=) <$> _detTaskId,
                  ("nextToken" .=) <$> _detNextToken,
                  ("limit" .=) <$> _detLimit,
                  ("statusCode" .=) <$> _detStatusCode])

instance ToPath DescribeExportTasks where
        toPath = const "/"

instance ToQuery DescribeExportTasks where
        toQuery = const mempty

-- | /See:/ 'describeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
    { _detrsNextToken      :: !(Maybe Text)
    , _detrsExportTasks    :: !(Maybe [ExportTask])
    , _detrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeExportTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detrsNextToken'
--
-- * 'detrsExportTasks'
--
-- * 'detrsResponseStatus'
describeExportTasksResponse
    :: Int -- ^ 'detrsResponseStatus'
    -> DescribeExportTasksResponse
describeExportTasksResponse pResponseStatus_ =
    DescribeExportTasksResponse'
    { _detrsNextToken = Nothing
    , _detrsExportTasks = Nothing
    , _detrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
detrsNextToken :: Lens' DescribeExportTasksResponse (Maybe Text)
detrsNextToken = lens _detrsNextToken (\ s a -> s{_detrsNextToken = a});

-- | Undocumented member.
detrsExportTasks :: Lens' DescribeExportTasksResponse [ExportTask]
detrsExportTasks = lens _detrsExportTasks (\ s a -> s{_detrsExportTasks = a}) . _Default . _Coerce;

-- | The response status code.
detrsResponseStatus :: Lens' DescribeExportTasksResponse Int
detrsResponseStatus = lens _detrsResponseStatus (\ s a -> s{_detrsResponseStatus = a});

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
-- Module      : Network.AWS.CloudWatchLogs.CreateExportTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an export task, which allows you to efficiently export data from a log group to an Amazon S3 bucket.
--
--
-- This is an asynchronous call. If all the required information is provided, this operation initiates an export task and responds with the ID of the task. After the task has started, you can use 'DescribeExportTasks' to get the status of the export task. Each account can only have one active (@RUNNING@ or @PENDING@ ) export task at a time. To cancel an export task, use 'CancelExportTask' .
--
-- You can export logs from multiple log groups or multiple time ranges to the same S3 bucket. To separate out log data for each export task, you can specify a prefix to be used as the Amazon S3 key prefix for all exported objects.
--
module Network.AWS.CloudWatchLogs.CreateExportTask
    (
    -- * Creating a Request
      createExportTask
    , CreateExportTask
    -- * Request Lenses
    , cetDestinationPrefix
    , cetTaskName
    , cetLogStreamNamePrefix
    , cetLogGroupName
    , cetFrom
    , cetTo
    , cetDestination

    -- * Destructuring the Response
    , createExportTaskResponse
    , CreateExportTaskResponse
    -- * Response Lenses
    , cetrsTaskId
    , cetrsResponseStatus
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createExportTask' smart constructor.
data CreateExportTask = CreateExportTask'
  { _cetDestinationPrefix   :: !(Maybe Text)
  , _cetTaskName            :: !(Maybe Text)
  , _cetLogStreamNamePrefix :: !(Maybe Text)
  , _cetLogGroupName        :: !Text
  , _cetFrom                :: !Nat
  , _cetTo                  :: !Nat
  , _cetDestination         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateExportTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cetDestinationPrefix' - The prefix used as the start of the key for every object exported. If you don't specify a value, the default is @exportedlogs@ .
--
-- * 'cetTaskName' - The name of the export task.
--
-- * 'cetLogStreamNamePrefix' - Export only log streams that match the provided prefix. If you don't specify a value, no prefix filter is applied.
--
-- * 'cetLogGroupName' - The name of the log group.
--
-- * 'cetFrom' - The start time of the range for the request, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a time stamp earlier than this time are not exported.
--
-- * 'cetTo' - The end time of the range for the request, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a time stamp later than this time are not exported.
--
-- * 'cetDestination' - The name of S3 bucket for the exported log data. The bucket must be in the same AWS region.
createExportTask
    :: Text -- ^ 'cetLogGroupName'
    -> Natural -- ^ 'cetFrom'
    -> Natural -- ^ 'cetTo'
    -> Text -- ^ 'cetDestination'
    -> CreateExportTask
createExportTask pLogGroupName_ pFrom_ pTo_ pDestination_ =
  CreateExportTask'
    { _cetDestinationPrefix = Nothing
    , _cetTaskName = Nothing
    , _cetLogStreamNamePrefix = Nothing
    , _cetLogGroupName = pLogGroupName_
    , _cetFrom = _Nat # pFrom_
    , _cetTo = _Nat # pTo_
    , _cetDestination = pDestination_
    }


-- | The prefix used as the start of the key for every object exported. If you don't specify a value, the default is @exportedlogs@ .
cetDestinationPrefix :: Lens' CreateExportTask (Maybe Text)
cetDestinationPrefix = lens _cetDestinationPrefix (\ s a -> s{_cetDestinationPrefix = a})

-- | The name of the export task.
cetTaskName :: Lens' CreateExportTask (Maybe Text)
cetTaskName = lens _cetTaskName (\ s a -> s{_cetTaskName = a})

-- | Export only log streams that match the provided prefix. If you don't specify a value, no prefix filter is applied.
cetLogStreamNamePrefix :: Lens' CreateExportTask (Maybe Text)
cetLogStreamNamePrefix = lens _cetLogStreamNamePrefix (\ s a -> s{_cetLogStreamNamePrefix = a})

-- | The name of the log group.
cetLogGroupName :: Lens' CreateExportTask Text
cetLogGroupName = lens _cetLogGroupName (\ s a -> s{_cetLogGroupName = a})

-- | The start time of the range for the request, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a time stamp earlier than this time are not exported.
cetFrom :: Lens' CreateExportTask Natural
cetFrom = lens _cetFrom (\ s a -> s{_cetFrom = a}) . _Nat

-- | The end time of the range for the request, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a time stamp later than this time are not exported.
cetTo :: Lens' CreateExportTask Natural
cetTo = lens _cetTo (\ s a -> s{_cetTo = a}) . _Nat

-- | The name of S3 bucket for the exported log data. The bucket must be in the same AWS region.
cetDestination :: Lens' CreateExportTask Text
cetDestination = lens _cetDestination (\ s a -> s{_cetDestination = a})

instance AWSRequest CreateExportTask where
        type Rs CreateExportTask = CreateExportTaskResponse
        request = postJSON cloudWatchLogs
        response
          = receiveJSON
              (\ s h x ->
                 CreateExportTaskResponse' <$>
                   (x .?> "taskId") <*> (pure (fromEnum s)))

instance Hashable CreateExportTask where

instance NFData CreateExportTask where

instance ToHeaders CreateExportTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.CreateExportTask" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateExportTask where
        toJSON CreateExportTask'{..}
          = object
              (catMaybes
                 [("destinationPrefix" .=) <$> _cetDestinationPrefix,
                  ("taskName" .=) <$> _cetTaskName,
                  ("logStreamNamePrefix" .=) <$>
                    _cetLogStreamNamePrefix,
                  Just ("logGroupName" .= _cetLogGroupName),
                  Just ("from" .= _cetFrom), Just ("to" .= _cetTo),
                  Just ("destination" .= _cetDestination)])

instance ToPath CreateExportTask where
        toPath = const "/"

instance ToQuery CreateExportTask where
        toQuery = const mempty

-- | /See:/ 'createExportTaskResponse' smart constructor.
data CreateExportTaskResponse = CreateExportTaskResponse'
  { _cetrsTaskId         :: !(Maybe Text)
  , _cetrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateExportTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cetrsTaskId' - The ID of the export task.
--
-- * 'cetrsResponseStatus' - -- | The response status code.
createExportTaskResponse
    :: Int -- ^ 'cetrsResponseStatus'
    -> CreateExportTaskResponse
createExportTaskResponse pResponseStatus_ =
  CreateExportTaskResponse'
    {_cetrsTaskId = Nothing, _cetrsResponseStatus = pResponseStatus_}


-- | The ID of the export task.
cetrsTaskId :: Lens' CreateExportTaskResponse (Maybe Text)
cetrsTaskId = lens _cetrsTaskId (\ s a -> s{_cetrsTaskId = a})

-- | -- | The response status code.
cetrsResponseStatus :: Lens' CreateExportTaskResponse Int
cetrsResponseStatus = lens _cetrsResponseStatus (\ s a -> s{_cetrsResponseStatus = a})

instance NFData CreateExportTaskResponse where

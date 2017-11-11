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
-- Module      : Network.AWS.StepFunctions.GetActivityTask
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by workers to retrieve a task (with the specified activity ARN) scheduled for execution by a running state machine. This initiates a long poll, where the service holds the HTTP connection open and responds as soon as a task becomes available (i.e. an execution of a task of this type is needed.) The maximum time the service holds on to the request before responding is 60 seconds. If no task is available within 60 seconds, the poll will return an empty result, that is, the @taskToken@ returned is an empty string.
--
--
-- /Important:/ Workers should set their client side socket timeout to at least 65 seconds (5 seconds higher than the maximum time the service may hold the poll request).
--
module Network.AWS.StepFunctions.GetActivityTask
    (
    -- * Creating a Request
      getActivityTask
    , GetActivityTask
    -- * Request Lenses
    , gatWorkerName
    , gatActivityARN

    -- * Destructuring the Response
    , getActivityTaskResponse
    , GetActivityTaskResponse
    -- * Response Lenses
    , gatrsInput
    , gatrsTaskToken
    , gatrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types
import Network.AWS.StepFunctions.Types.Product

-- | /See:/ 'getActivityTask' smart constructor.
data GetActivityTask = GetActivityTask'
  { _gatWorkerName  :: {-# NOUNPACK #-}!(Maybe Text)
  , _gatActivityARN :: {-# NOUNPACK #-}!Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetActivityTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gatWorkerName' - An arbitrary name may be provided in order to identify the worker that the task is assigned to. This name will be used when it is logged in the execution history.
--
-- * 'gatActivityARN' - The Amazon Resource Name (ARN) of the activity to retrieve tasks from.
getActivityTask
    :: Text -- ^ 'gatActivityARN'
    -> GetActivityTask
getActivityTask pActivityARN_ =
  GetActivityTask' {_gatWorkerName = Nothing, _gatActivityARN = pActivityARN_}


-- | An arbitrary name may be provided in order to identify the worker that the task is assigned to. This name will be used when it is logged in the execution history.
gatWorkerName :: Lens' GetActivityTask (Maybe Text)
gatWorkerName = lens _gatWorkerName (\ s a -> s{_gatWorkerName = a});

-- | The Amazon Resource Name (ARN) of the activity to retrieve tasks from.
gatActivityARN :: Lens' GetActivityTask Text
gatActivityARN = lens _gatActivityARN (\ s a -> s{_gatActivityARN = a});

instance AWSRequest GetActivityTask where
        type Rs GetActivityTask = GetActivityTaskResponse
        request = postJSON stepFunctions
        response
          = receiveJSON
              (\ s h x ->
                 GetActivityTaskResponse' <$>
                   (x .?> "input") <*> (x .?> "taskToken") <*>
                     (pure (fromEnum s)))

instance Hashable GetActivityTask where

instance NFData GetActivityTask where

instance ToHeaders GetActivityTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSStepFunctions.GetActivityTask" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON GetActivityTask where
        toJSON GetActivityTask'{..}
          = object
              (catMaybes
                 [("workerName" .=) <$> _gatWorkerName,
                  Just ("activityArn" .= _gatActivityARN)])

instance ToPath GetActivityTask where
        toPath = const "/"

instance ToQuery GetActivityTask where
        toQuery = const mempty

-- | /See:/ 'getActivityTaskResponse' smart constructor.
data GetActivityTaskResponse = GetActivityTaskResponse'
  { _gatrsInput          :: {-# NOUNPACK #-}!(Maybe Text)
  , _gatrsTaskToken      :: {-# NOUNPACK #-}!(Maybe Text)
  , _gatrsResponseStatus :: {-# NOUNPACK #-}!Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetActivityTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gatrsInput' - The JSON input data for the task.
--
-- * 'gatrsTaskToken' - A token that identifies the scheduled task. This token must be copied and included in subsequent calls to 'SendTaskHeartbeat' , 'SendTaskSuccess' or 'SendTaskFailure' in order to report the progress or completion of the task.
--
-- * 'gatrsResponseStatus' - -- | The response status code.
getActivityTaskResponse
    :: Int -- ^ 'gatrsResponseStatus'
    -> GetActivityTaskResponse
getActivityTaskResponse pResponseStatus_ =
  GetActivityTaskResponse'
  { _gatrsInput = Nothing
  , _gatrsTaskToken = Nothing
  , _gatrsResponseStatus = pResponseStatus_
  }


-- | The JSON input data for the task.
gatrsInput :: Lens' GetActivityTaskResponse (Maybe Text)
gatrsInput = lens _gatrsInput (\ s a -> s{_gatrsInput = a});

-- | A token that identifies the scheduled task. This token must be copied and included in subsequent calls to 'SendTaskHeartbeat' , 'SendTaskSuccess' or 'SendTaskFailure' in order to report the progress or completion of the task.
gatrsTaskToken :: Lens' GetActivityTaskResponse (Maybe Text)
gatrsTaskToken = lens _gatrsTaskToken (\ s a -> s{_gatrsTaskToken = a});

-- | -- | The response status code.
gatrsResponseStatus :: Lens' GetActivityTaskResponse Int
gatrsResponseStatus = lens _gatrsResponseStatus (\ s a -> s{_gatrsResponseStatus = a});

instance NFData GetActivityTaskResponse where

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
-- Module      : Network.AWS.DataPipeline.ReportTaskRunnerHeartbeat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @ReportTaskRunnerHeartbeat@ every 15 minutes to indicate that they are operational. If the AWS Data Pipeline Task Runner is launched on a resource managed by AWS Data Pipeline, the web service can use this call to detect when the task runner application has failed and restart a new instance.
--
--
module Network.AWS.DataPipeline.ReportTaskRunnerHeartbeat
    (
    -- * Creating a Request
      reportTaskRunnerHeartbeat
    , ReportTaskRunnerHeartbeat
    -- * Request Lenses
    , rtrhHostname
    , rtrhWorkerGroup
    , rtrhTaskrunnerId

    -- * Destructuring the Response
    , reportTaskRunnerHeartbeatResponse
    , ReportTaskRunnerHeartbeatResponse
    -- * Response Lenses
    , rtrhrsResponseStatus
    , rtrhrsTerminate
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.DataPipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ReportTaskRunnerHeartbeat.
--
--
--
-- /See:/ 'reportTaskRunnerHeartbeat' smart constructor.
data ReportTaskRunnerHeartbeat = ReportTaskRunnerHeartbeat'
  { _rtrhHostname     :: !(Maybe Text)
  , _rtrhWorkerGroup  :: !(Maybe Text)
  , _rtrhTaskrunnerId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReportTaskRunnerHeartbeat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtrhHostname' - The public DNS name of the task runner.
--
-- * 'rtrhWorkerGroup' - The type of task the task runner is configured to accept and process. The worker group is set as a field on objects in the pipeline when they are created. You can only specify a single value for @workerGroup@ . There are no wildcard values permitted in @workerGroup@ ; the string must be an exact, case-sensitive, match.
--
-- * 'rtrhTaskrunnerId' - The ID of the task runner. This value should be unique across your AWS account. In the case of AWS Data Pipeline Task Runner launched on a resource managed by AWS Data Pipeline, the web service provides a unique identifier when it launches the application. If you have written a custom task runner, you should assign a unique identifier for the task runner.
reportTaskRunnerHeartbeat
    :: Text -- ^ 'rtrhTaskrunnerId'
    -> ReportTaskRunnerHeartbeat
reportTaskRunnerHeartbeat pTaskrunnerId_ =
  ReportTaskRunnerHeartbeat'
    { _rtrhHostname = Nothing
    , _rtrhWorkerGroup = Nothing
    , _rtrhTaskrunnerId = pTaskrunnerId_
    }


-- | The public DNS name of the task runner.
rtrhHostname :: Lens' ReportTaskRunnerHeartbeat (Maybe Text)
rtrhHostname = lens _rtrhHostname (\ s a -> s{_rtrhHostname = a})

-- | The type of task the task runner is configured to accept and process. The worker group is set as a field on objects in the pipeline when they are created. You can only specify a single value for @workerGroup@ . There are no wildcard values permitted in @workerGroup@ ; the string must be an exact, case-sensitive, match.
rtrhWorkerGroup :: Lens' ReportTaskRunnerHeartbeat (Maybe Text)
rtrhWorkerGroup = lens _rtrhWorkerGroup (\ s a -> s{_rtrhWorkerGroup = a})

-- | The ID of the task runner. This value should be unique across your AWS account. In the case of AWS Data Pipeline Task Runner launched on a resource managed by AWS Data Pipeline, the web service provides a unique identifier when it launches the application. If you have written a custom task runner, you should assign a unique identifier for the task runner.
rtrhTaskrunnerId :: Lens' ReportTaskRunnerHeartbeat Text
rtrhTaskrunnerId = lens _rtrhTaskrunnerId (\ s a -> s{_rtrhTaskrunnerId = a})

instance AWSRequest ReportTaskRunnerHeartbeat where
        type Rs ReportTaskRunnerHeartbeat =
             ReportTaskRunnerHeartbeatResponse
        request = postJSON dataPipeline
        response
          = receiveJSON
              (\ s h x ->
                 ReportTaskRunnerHeartbeatResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "terminate"))

instance Hashable ReportTaskRunnerHeartbeat where

instance NFData ReportTaskRunnerHeartbeat where

instance ToHeaders ReportTaskRunnerHeartbeat where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DataPipeline.ReportTaskRunnerHeartbeat" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ReportTaskRunnerHeartbeat where
        toJSON ReportTaskRunnerHeartbeat'{..}
          = object
              (catMaybes
                 [("hostname" .=) <$> _rtrhHostname,
                  ("workerGroup" .=) <$> _rtrhWorkerGroup,
                  Just ("taskrunnerId" .= _rtrhTaskrunnerId)])

instance ToPath ReportTaskRunnerHeartbeat where
        toPath = const "/"

instance ToQuery ReportTaskRunnerHeartbeat where
        toQuery = const mempty

-- | Contains the output of ReportTaskRunnerHeartbeat.
--
--
--
-- /See:/ 'reportTaskRunnerHeartbeatResponse' smart constructor.
data ReportTaskRunnerHeartbeatResponse = ReportTaskRunnerHeartbeatResponse'
  { _rtrhrsResponseStatus :: !Int
  , _rtrhrsTerminate      :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReportTaskRunnerHeartbeatResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtrhrsResponseStatus' - -- | The response status code.
--
-- * 'rtrhrsTerminate' - Indicates whether the calling task runner should terminate.
reportTaskRunnerHeartbeatResponse
    :: Int -- ^ 'rtrhrsResponseStatus'
    -> Bool -- ^ 'rtrhrsTerminate'
    -> ReportTaskRunnerHeartbeatResponse
reportTaskRunnerHeartbeatResponse pResponseStatus_ pTerminate_ =
  ReportTaskRunnerHeartbeatResponse'
    {_rtrhrsResponseStatus = pResponseStatus_, _rtrhrsTerminate = pTerminate_}


-- | -- | The response status code.
rtrhrsResponseStatus :: Lens' ReportTaskRunnerHeartbeatResponse Int
rtrhrsResponseStatus = lens _rtrhrsResponseStatus (\ s a -> s{_rtrhrsResponseStatus = a})

-- | Indicates whether the calling task runner should terminate.
rtrhrsTerminate :: Lens' ReportTaskRunnerHeartbeatResponse Bool
rtrhrsTerminate = lens _rtrhrsTerminate (\ s a -> s{_rtrhrsTerminate = a})

instance NFData ReportTaskRunnerHeartbeatResponse
         where

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
-- Module      : Network.AWS.DataPipeline.ReportTaskProgress
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @ReportTaskProgress@ when assigned a task to acknowledge that it has the task. If the web service does not receive this acknowledgement within 2 minutes, it assigns the task in a subsequent 'PollForTask' call. After this initial acknowledgement, the task runner only needs to report progress every 15 minutes to maintain its ownership of the task. You can change this reporting time from 15 minutes by specifying a @reportProgressTimeout@ field in your pipeline.
--
--
-- If a task runner does not report its status after 5 minutes, AWS Data Pipeline assumes that the task runner is unable to process the task and reassigns the task in a subsequent response to 'PollForTask' . Task runners should call @ReportTaskProgress@ every 60 seconds.
--
module Network.AWS.DataPipeline.ReportTaskProgress
    (
    -- * Creating a Request
      reportTaskProgress
    , ReportTaskProgress
    -- * Request Lenses
    , rtpFields
    , rtpTaskId

    -- * Destructuring the Response
    , reportTaskProgressResponse
    , ReportTaskProgressResponse
    -- * Response Lenses
    , rtprsResponseStatus
    , rtprsCanceled
    ) where

import Network.AWS.DataPipeline.Types
import Network.AWS.DataPipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ReportTaskProgress.
--
--
--
-- /See:/ 'reportTaskProgress' smart constructor.
data ReportTaskProgress = ReportTaskProgress'
  { _rtpFields :: !(Maybe [Field])
  , _rtpTaskId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReportTaskProgress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtpFields' - Key-value pairs that define the properties of the ReportTaskProgressInput object.
--
-- * 'rtpTaskId' - The ID of the task assigned to the task runner. This value is provided in the response for 'PollForTask' .
reportTaskProgress
    :: Text -- ^ 'rtpTaskId'
    -> ReportTaskProgress
reportTaskProgress pTaskId_ =
  ReportTaskProgress' {_rtpFields = Nothing, _rtpTaskId = pTaskId_}


-- | Key-value pairs that define the properties of the ReportTaskProgressInput object.
rtpFields :: Lens' ReportTaskProgress [Field]
rtpFields = lens _rtpFields (\ s a -> s{_rtpFields = a}) . _Default . _Coerce

-- | The ID of the task assigned to the task runner. This value is provided in the response for 'PollForTask' .
rtpTaskId :: Lens' ReportTaskProgress Text
rtpTaskId = lens _rtpTaskId (\ s a -> s{_rtpTaskId = a})

instance AWSRequest ReportTaskProgress where
        type Rs ReportTaskProgress =
             ReportTaskProgressResponse
        request = postJSON dataPipeline
        response
          = receiveJSON
              (\ s h x ->
                 ReportTaskProgressResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "canceled"))

instance Hashable ReportTaskProgress where

instance NFData ReportTaskProgress where

instance ToHeaders ReportTaskProgress where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DataPipeline.ReportTaskProgress" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ReportTaskProgress where
        toJSON ReportTaskProgress'{..}
          = object
              (catMaybes
                 [("fields" .=) <$> _rtpFields,
                  Just ("taskId" .= _rtpTaskId)])

instance ToPath ReportTaskProgress where
        toPath = const "/"

instance ToQuery ReportTaskProgress where
        toQuery = const mempty

-- | Contains the output of ReportTaskProgress.
--
--
--
-- /See:/ 'reportTaskProgressResponse' smart constructor.
data ReportTaskProgressResponse = ReportTaskProgressResponse'
  { _rtprsResponseStatus :: !Int
  , _rtprsCanceled       :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReportTaskProgressResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtprsResponseStatus' - -- | The response status code.
--
-- * 'rtprsCanceled' - If true, the calling task runner should cancel processing of the task. The task runner does not need to call 'SetTaskStatus' for canceled tasks.
reportTaskProgressResponse
    :: Int -- ^ 'rtprsResponseStatus'
    -> Bool -- ^ 'rtprsCanceled'
    -> ReportTaskProgressResponse
reportTaskProgressResponse pResponseStatus_ pCanceled_ =
  ReportTaskProgressResponse'
    {_rtprsResponseStatus = pResponseStatus_, _rtprsCanceled = pCanceled_}


-- | -- | The response status code.
rtprsResponseStatus :: Lens' ReportTaskProgressResponse Int
rtprsResponseStatus = lens _rtprsResponseStatus (\ s a -> s{_rtprsResponseStatus = a})

-- | If true, the calling task runner should cancel processing of the task. The task runner does not need to call 'SetTaskStatus' for canceled tasks.
rtprsCanceled :: Lens' ReportTaskProgressResponse Bool
rtprsCanceled = lens _rtprsCanceled (\ s a -> s{_rtprsCanceled = a})

instance NFData ReportTaskProgressResponse where

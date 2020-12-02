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
-- Module      : Network.AWS.IoTAnalytics.StartPipelineReprocessing
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the reprocessing of raw message data through the pipeline.
--
--
module Network.AWS.IoTAnalytics.StartPipelineReprocessing
    (
    -- * Creating a Request
      startPipelineReprocessing
    , StartPipelineReprocessing
    -- * Request Lenses
    , sprStartTime
    , sprEndTime
    , sprPipelineName

    -- * Destructuring the Response
    , startPipelineReprocessingResponse
    , StartPipelineReprocessingResponse
    -- * Response Lenses
    , sprrsReprocessingId
    , sprrsResponseStatus
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startPipelineReprocessing' smart constructor.
data StartPipelineReprocessing = StartPipelineReprocessing'
  { _sprStartTime    :: !(Maybe POSIX)
  , _sprEndTime      :: !(Maybe POSIX)
  , _sprPipelineName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartPipelineReprocessing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sprStartTime' - The start time (inclusive) of raw message data that is reprocessed.
--
-- * 'sprEndTime' - The end time (exclusive) of raw message data that is reprocessed.
--
-- * 'sprPipelineName' - The name of the pipeline on which to start reprocessing.
startPipelineReprocessing
    :: Text -- ^ 'sprPipelineName'
    -> StartPipelineReprocessing
startPipelineReprocessing pPipelineName_ =
  StartPipelineReprocessing'
    { _sprStartTime = Nothing
    , _sprEndTime = Nothing
    , _sprPipelineName = pPipelineName_
    }


-- | The start time (inclusive) of raw message data that is reprocessed.
sprStartTime :: Lens' StartPipelineReprocessing (Maybe UTCTime)
sprStartTime = lens _sprStartTime (\ s a -> s{_sprStartTime = a}) . mapping _Time

-- | The end time (exclusive) of raw message data that is reprocessed.
sprEndTime :: Lens' StartPipelineReprocessing (Maybe UTCTime)
sprEndTime = lens _sprEndTime (\ s a -> s{_sprEndTime = a}) . mapping _Time

-- | The name of the pipeline on which to start reprocessing.
sprPipelineName :: Lens' StartPipelineReprocessing Text
sprPipelineName = lens _sprPipelineName (\ s a -> s{_sprPipelineName = a})

instance AWSRequest StartPipelineReprocessing where
        type Rs StartPipelineReprocessing =
             StartPipelineReprocessingResponse
        request = postJSON ioTAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 StartPipelineReprocessingResponse' <$>
                   (x .?> "reprocessingId") <*> (pure (fromEnum s)))

instance Hashable StartPipelineReprocessing where

instance NFData StartPipelineReprocessing where

instance ToHeaders StartPipelineReprocessing where
        toHeaders = const mempty

instance ToJSON StartPipelineReprocessing where
        toJSON StartPipelineReprocessing'{..}
          = object
              (catMaybes
                 [("startTime" .=) <$> _sprStartTime,
                  ("endTime" .=) <$> _sprEndTime])

instance ToPath StartPipelineReprocessing where
        toPath StartPipelineReprocessing'{..}
          = mconcat
              ["/pipelines/", toBS _sprPipelineName,
               "/reprocessing"]

instance ToQuery StartPipelineReprocessing where
        toQuery = const mempty

-- | /See:/ 'startPipelineReprocessingResponse' smart constructor.
data StartPipelineReprocessingResponse = StartPipelineReprocessingResponse'
  { _sprrsReprocessingId :: !(Maybe Text)
  , _sprrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartPipelineReprocessingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sprrsReprocessingId' - The ID of the pipeline reprocessing activity that was started.
--
-- * 'sprrsResponseStatus' - -- | The response status code.
startPipelineReprocessingResponse
    :: Int -- ^ 'sprrsResponseStatus'
    -> StartPipelineReprocessingResponse
startPipelineReprocessingResponse pResponseStatus_ =
  StartPipelineReprocessingResponse'
    {_sprrsReprocessingId = Nothing, _sprrsResponseStatus = pResponseStatus_}


-- | The ID of the pipeline reprocessing activity that was started.
sprrsReprocessingId :: Lens' StartPipelineReprocessingResponse (Maybe Text)
sprrsReprocessingId = lens _sprrsReprocessingId (\ s a -> s{_sprrsReprocessingId = a})

-- | -- | The response status code.
sprrsResponseStatus :: Lens' StartPipelineReprocessingResponse Int
sprrsResponseStatus = lens _sprrsResponseStatus (\ s a -> s{_sprrsResponseStatus = a})

instance NFData StartPipelineReprocessingResponse
         where

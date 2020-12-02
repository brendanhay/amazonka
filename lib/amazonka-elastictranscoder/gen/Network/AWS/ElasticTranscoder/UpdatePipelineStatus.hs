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
-- Module      : Network.AWS.ElasticTranscoder.UpdatePipelineStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The UpdatePipelineStatus operation pauses or reactivates a pipeline, so that the pipeline stops or restarts the processing of jobs.
--
--
-- Changing the pipeline status is useful if you want to cancel one or more jobs. You can't cancel jobs after Elastic Transcoder has started processing them; if you pause the pipeline to which you submitted the jobs, you have more time to get the job IDs for the jobs that you want to cancel, and to send a 'CancelJob' request.
--
module Network.AWS.ElasticTranscoder.UpdatePipelineStatus
    (
    -- * Creating a Request
      updatePipelineStatus
    , UpdatePipelineStatus
    -- * Request Lenses
    , upsId
    , upsStatus

    -- * Destructuring the Response
    , updatePipelineStatusResponse
    , UpdatePipelineStatusResponse
    -- * Response Lenses
    , upsrsPipeline
    , upsrsResponseStatus
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.ElasticTranscoder.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The @UpdatePipelineStatusRequest@ structure.
--
--
--
-- /See:/ 'updatePipelineStatus' smart constructor.
data UpdatePipelineStatus = UpdatePipelineStatus'
  { _upsId     :: !Text
  , _upsStatus :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdatePipelineStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upsId' - The identifier of the pipeline to update.
--
-- * 'upsStatus' - The desired status of the pipeline:     * @Active@ : The pipeline is processing jobs.     * @Paused@ : The pipeline is not currently processing jobs.
updatePipelineStatus
    :: Text -- ^ 'upsId'
    -> Text -- ^ 'upsStatus'
    -> UpdatePipelineStatus
updatePipelineStatus pId_ pStatus_ =
  UpdatePipelineStatus' {_upsId = pId_, _upsStatus = pStatus_}


-- | The identifier of the pipeline to update.
upsId :: Lens' UpdatePipelineStatus Text
upsId = lens _upsId (\ s a -> s{_upsId = a})

-- | The desired status of the pipeline:     * @Active@ : The pipeline is processing jobs.     * @Paused@ : The pipeline is not currently processing jobs.
upsStatus :: Lens' UpdatePipelineStatus Text
upsStatus = lens _upsStatus (\ s a -> s{_upsStatus = a})

instance AWSRequest UpdatePipelineStatus where
        type Rs UpdatePipelineStatus =
             UpdatePipelineStatusResponse
        request = postJSON elasticTranscoder
        response
          = receiveJSON
              (\ s h x ->
                 UpdatePipelineStatusResponse' <$>
                   (x .?> "Pipeline") <*> (pure (fromEnum s)))

instance Hashable UpdatePipelineStatus where

instance NFData UpdatePipelineStatus where

instance ToHeaders UpdatePipelineStatus where
        toHeaders = const mempty

instance ToJSON UpdatePipelineStatus where
        toJSON UpdatePipelineStatus'{..}
          = object (catMaybes [Just ("Status" .= _upsStatus)])

instance ToPath UpdatePipelineStatus where
        toPath UpdatePipelineStatus'{..}
          = mconcat
              ["/2012-09-25/pipelines/", toBS _upsId, "/status"]

instance ToQuery UpdatePipelineStatus where
        toQuery = const mempty

-- | When you update status for a pipeline, Elastic Transcoder returns the values that you specified in the request.
--
--
--
-- /See:/ 'updatePipelineStatusResponse' smart constructor.
data UpdatePipelineStatusResponse = UpdatePipelineStatusResponse'
  { _upsrsPipeline       :: !(Maybe Pipeline)
  , _upsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdatePipelineStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upsrsPipeline' - A section of the response body that provides information about the pipeline.
--
-- * 'upsrsResponseStatus' - -- | The response status code.
updatePipelineStatusResponse
    :: Int -- ^ 'upsrsResponseStatus'
    -> UpdatePipelineStatusResponse
updatePipelineStatusResponse pResponseStatus_ =
  UpdatePipelineStatusResponse'
    {_upsrsPipeline = Nothing, _upsrsResponseStatus = pResponseStatus_}


-- | A section of the response body that provides information about the pipeline.
upsrsPipeline :: Lens' UpdatePipelineStatusResponse (Maybe Pipeline)
upsrsPipeline = lens _upsrsPipeline (\ s a -> s{_upsrsPipeline = a})

-- | -- | The response status code.
upsrsResponseStatus :: Lens' UpdatePipelineStatusResponse Int
upsrsResponseStatus = lens _upsrsResponseStatus (\ s a -> s{_upsrsResponseStatus = a})

instance NFData UpdatePipelineStatusResponse where

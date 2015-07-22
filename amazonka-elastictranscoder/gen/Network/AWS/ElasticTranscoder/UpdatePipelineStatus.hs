{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.UpdatePipelineStatus
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The UpdatePipelineStatus operation pauses or reactivates a pipeline, so
-- that the pipeline stops or restarts the processing of jobs.
--
-- Changing the pipeline status is useful if you want to cancel one or more
-- jobs. You can\'t cancel jobs after Elastic Transcoder has started
-- processing them; if you pause the pipeline to which you submitted the
-- jobs, you have more time to get the job IDs for the jobs that you want
-- to cancel, and to send a CancelJob request.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/UpdatePipelineStatus.html>
module Network.AWS.ElasticTranscoder.UpdatePipelineStatus
    (
    -- * Request
      UpdatePipelineStatus
    -- ** Request constructor
    , updatePipelineStatus
    -- ** Request lenses
    , upsrqId
    , upsrqStatus

    -- * Response
    , UpdatePipelineStatusResponse
    -- ** Response constructor
    , updatePipelineStatusResponse
    -- ** Response lenses
    , upsrsPipeline
    , upsrsStatus
    ) where

import           Network.AWS.ElasticTranscoder.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The @UpdatePipelineStatusRequest@ structure.
--
-- /See:/ 'updatePipelineStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upsrqId'
--
-- * 'upsrqStatus'
data UpdatePipelineStatus = UpdatePipelineStatus'
    { _upsrqId     :: !Text
    , _upsrqStatus :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdatePipelineStatus' smart constructor.
updatePipelineStatus :: Text -> Text -> UpdatePipelineStatus
updatePipelineStatus pId_ pStatus_ =
    UpdatePipelineStatus'
    { _upsrqId = pId_
    , _upsrqStatus = pStatus_
    }

-- | The identifier of the pipeline to update.
upsrqId :: Lens' UpdatePipelineStatus Text
upsrqId = lens _upsrqId (\ s a -> s{_upsrqId = a});

-- | The desired status of the pipeline:
--
-- -   @Active@: The pipeline is processing jobs.
-- -   @Paused@: The pipeline is not currently processing jobs.
upsrqStatus :: Lens' UpdatePipelineStatus Text
upsrqStatus = lens _upsrqStatus (\ s a -> s{_upsrqStatus = a});

instance AWSRequest UpdatePipelineStatus where
        type Sv UpdatePipelineStatus = ElasticTranscoder
        type Rs UpdatePipelineStatus =
             UpdatePipelineStatusResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdatePipelineStatusResponse' <$>
                   (x .?> "Pipeline") <*> (pure (fromEnum s)))

instance ToHeaders UpdatePipelineStatus where
        toHeaders = const mempty

instance ToJSON UpdatePipelineStatus where
        toJSON UpdatePipelineStatus'{..}
          = object ["Status" .= _upsrqStatus]

instance ToPath UpdatePipelineStatus where
        toPath UpdatePipelineStatus'{..}
          = mconcat
              ["/2012-09-25/pipelines/", toText _upsrqId,
               "/status"]

instance ToQuery UpdatePipelineStatus where
        toQuery = const mempty

-- | When you update status for a pipeline, Elastic Transcoder returns the
-- values that you specified in the request.
--
-- /See:/ 'updatePipelineStatusResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upsrsPipeline'
--
-- * 'upsrsStatus'
data UpdatePipelineStatusResponse = UpdatePipelineStatusResponse'
    { _upsrsPipeline :: !(Maybe Pipeline)
    , _upsrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdatePipelineStatusResponse' smart constructor.
updatePipelineStatusResponse :: Int -> UpdatePipelineStatusResponse
updatePipelineStatusResponse pStatus_ =
    UpdatePipelineStatusResponse'
    { _upsrsPipeline = Nothing
    , _upsrsStatus = pStatus_
    }

-- | A section of the response body that provides information about the
-- pipeline.
upsrsPipeline :: Lens' UpdatePipelineStatusResponse (Maybe Pipeline)
upsrsPipeline = lens _upsrsPipeline (\ s a -> s{_upsrsPipeline = a});

-- | FIXME: Undocumented member.
upsrsStatus :: Lens' UpdatePipelineStatusResponse Int
upsrsStatus = lens _upsrsStatus (\ s a -> s{_upsrsStatus = a});

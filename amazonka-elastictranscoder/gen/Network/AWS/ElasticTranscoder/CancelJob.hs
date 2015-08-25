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
-- Module      : Network.AWS.ElasticTranscoder.CancelJob
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The CancelJob operation cancels an unfinished job.
--
-- You can only cancel a job that has a status of 'Submitted'. To prevent a
-- pipeline from starting to process a job while you\'re getting the job
-- identifier, use UpdatePipelineStatus to temporarily pause the pipeline.
--
-- /See:/ <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/CancelJob.html AWS API Reference> for CancelJob.
module Network.AWS.ElasticTranscoder.CancelJob
    (
    -- * Creating a Request
      cancelJob
    , CancelJob
    -- * Request Lenses
    , cjId

    -- * Destructuring the Response
    , cancelJobResponse
    , CancelJobResponse
    -- * Response Lenses
    , canrsStatus
    ) where

import           Network.AWS.ElasticTranscoder.Types
import           Network.AWS.ElasticTranscoder.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The 'CancelJobRequest' structure.
--
-- /See:/ 'cancelJob' smart constructor.
newtype CancelJob = CancelJob'
    { _cjId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CancelJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjId'
cancelJob
    :: Text -- ^ 'cjId'
    -> CancelJob
cancelJob pId_ =
    CancelJob'
    { _cjId = pId_
    }

-- | The identifier of the job that you want to cancel.
--
-- To get a list of the jobs (including their 'jobId') that have a status
-- of 'Submitted', use the ListJobsByStatus API action.
cjId :: Lens' CancelJob Text
cjId = lens _cjId (\ s a -> s{_cjId = a});

instance AWSRequest CancelJob where
        type Rs CancelJob = CancelJobResponse
        request = delete elasticTranscoder
        response
          = receiveEmpty
              (\ s h x ->
                 CancelJobResponse' <$> (pure (fromEnum s)))

instance ToHeaders CancelJob where
        toHeaders = const mempty

instance ToPath CancelJob where
        toPath CancelJob'{..}
          = mconcat ["/2012-09-25/jobs/", toBS _cjId]

instance ToQuery CancelJob where
        toQuery = const mempty

-- | The response body contains a JSON object. If the job is successfully
-- canceled, the value of 'Success' is 'true'.
--
-- /See:/ 'cancelJobResponse' smart constructor.
newtype CancelJobResponse = CancelJobResponse'
    { _canrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CancelJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'canrsStatus'
cancelJobResponse
    :: Int -- ^ 'canrsStatus'
    -> CancelJobResponse
cancelJobResponse pStatus_ =
    CancelJobResponse'
    { _canrsStatus = pStatus_
    }

-- | The response status code.
canrsStatus :: Lens' CancelJobResponse Int
canrsStatus = lens _canrsStatus (\ s a -> s{_canrsStatus = a});

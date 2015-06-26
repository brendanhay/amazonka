{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticTranscoder.CancelJob
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The CancelJob operation cancels an unfinished job.
--
-- You can only cancel a job that has a status of @Submitted@. To prevent a
-- pipeline from starting to process a job while you\'re getting the job
-- identifier, use UpdatePipelineStatus to temporarily pause the pipeline.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/CancelJob.html>
module Network.AWS.ElasticTranscoder.CancelJob
    (
    -- * Request
      CancelJob
    -- ** Request constructor
    , cancelJob
    -- ** Request lenses
    , cjId

    -- * Response
    , CancelJobResponse
    -- ** Response constructor
    , cancelJobResponse
    -- ** Response lenses
    , canStatusCode
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The @CancelJobRequest@ structure.
--
-- /See:/ 'cancelJob' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cjId'
newtype CancelJob = CancelJob'{_cjId :: Text} deriving (Eq, Read, Show)

-- | 'CancelJob' smart constructor.
cancelJob :: Text -> CancelJob
cancelJob pId = CancelJob'{_cjId = pId};

-- | The identifier of the job that you want to cancel.
--
-- To get a list of the jobs (including their @jobId@) that have a status
-- of @Submitted@, use the ListJobsByStatus API action.
cjId :: Lens' CancelJob Text
cjId = lens _cjId (\ s a -> s{_cjId = a});

instance AWSRequest CancelJob where
        type Sv CancelJob = ElasticTranscoder
        type Rs CancelJob = CancelJobResponse
        request = delete
        response
          = receiveJSON
              (\ s h x ->
                 CancelJobResponse' <$> (pure (fromEnum s)))

instance ToHeaders CancelJob where
        toHeaders = const mempty

instance ToPath CancelJob where
        toPath CancelJob'{..}
          = mconcat ["/2012-09-25/jobs/", toText _cjId]

instance ToQuery CancelJob where
        toQuery = const mempty

-- | The response body contains a JSON object. If the job is successfully
-- canceled, the value of @Success@ is @true@.
--
-- /See:/ 'cancelJobResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'canStatusCode'
newtype CancelJobResponse = CancelJobResponse'{_canStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'CancelJobResponse' smart constructor.
cancelJobResponse :: Int -> CancelJobResponse
cancelJobResponse pStatusCode = CancelJobResponse'{_canStatusCode = pStatusCode};

-- | FIXME: Undocumented member.
canStatusCode :: Lens' CancelJobResponse Int
canStatusCode = lens _canStatusCode (\ s a -> s{_canStatusCode = a});

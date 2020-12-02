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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The CancelJob operation cancels an unfinished job.
--
--
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
    , canrsResponseStatus
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.ElasticTranscoder.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The @CancelJobRequest@ structure.
--
--
--
-- /See:/ 'cancelJob' smart constructor.
newtype CancelJob = CancelJob'
  { _cjId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjId' - The identifier of the job that you want to cancel. To get a list of the jobs (including their @jobId@ ) that have a status of @Submitted@ , use the 'ListJobsByStatus' API action.
cancelJob
    :: Text -- ^ 'cjId'
    -> CancelJob
cancelJob pId_ = CancelJob' {_cjId = pId_}


-- | The identifier of the job that you want to cancel. To get a list of the jobs (including their @jobId@ ) that have a status of @Submitted@ , use the 'ListJobsByStatus' API action.
cjId :: Lens' CancelJob Text
cjId = lens _cjId (\ s a -> s{_cjId = a})

instance AWSRequest CancelJob where
        type Rs CancelJob = CancelJobResponse
        request = delete elasticTranscoder
        response
          = receiveEmpty
              (\ s h x ->
                 CancelJobResponse' <$> (pure (fromEnum s)))

instance Hashable CancelJob where

instance NFData CancelJob where

instance ToHeaders CancelJob where
        toHeaders = const mempty

instance ToPath CancelJob where
        toPath CancelJob'{..}
          = mconcat ["/2012-09-25/jobs/", toBS _cjId]

instance ToQuery CancelJob where
        toQuery = const mempty

-- | The response body contains a JSON object. If the job is successfully canceled, the value of @Success@ is @true@ .
--
--
--
-- /See:/ 'cancelJobResponse' smart constructor.
newtype CancelJobResponse = CancelJobResponse'
  { _canrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'canrsResponseStatus' - -- | The response status code.
cancelJobResponse
    :: Int -- ^ 'canrsResponseStatus'
    -> CancelJobResponse
cancelJobResponse pResponseStatus_ =
  CancelJobResponse' {_canrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
canrsResponseStatus :: Lens' CancelJobResponse Int
canrsResponseStatus = lens _canrsResponseStatus (\ s a -> s{_canrsResponseStatus = a})

instance NFData CancelJobResponse where

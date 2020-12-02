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
-- Module      : Network.AWS.CodePipeline.AcknowledgeJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified job and whether that job has been received by the job worker. Only used for custom actions.
--
--
module Network.AWS.CodePipeline.AcknowledgeJob
    (
    -- * Creating a Request
      acknowledgeJob
    , AcknowledgeJob
    -- * Request Lenses
    , ajJobId
    , ajNonce

    -- * Destructuring the Response
    , acknowledgeJobResponse
    , AcknowledgeJobResponse
    -- * Response Lenses
    , ajrsStatus
    , ajrsResponseStatus
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of an AcknowledgeJob action.
--
--
--
-- /See:/ 'acknowledgeJob' smart constructor.
data AcknowledgeJob = AcknowledgeJob'
  { _ajJobId :: !Text
  , _ajNonce :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcknowledgeJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ajJobId' - The unique system-generated ID of the job for which you want to confirm receipt.
--
-- * 'ajNonce' - A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Get this number from the response of the 'PollForJobs' request that returned this job.
acknowledgeJob
    :: Text -- ^ 'ajJobId'
    -> Text -- ^ 'ajNonce'
    -> AcknowledgeJob
acknowledgeJob pJobId_ pNonce_ =
  AcknowledgeJob' {_ajJobId = pJobId_, _ajNonce = pNonce_}


-- | The unique system-generated ID of the job for which you want to confirm receipt.
ajJobId :: Lens' AcknowledgeJob Text
ajJobId = lens _ajJobId (\ s a -> s{_ajJobId = a})

-- | A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Get this number from the response of the 'PollForJobs' request that returned this job.
ajNonce :: Lens' AcknowledgeJob Text
ajNonce = lens _ajNonce (\ s a -> s{_ajNonce = a})

instance AWSRequest AcknowledgeJob where
        type Rs AcknowledgeJob = AcknowledgeJobResponse
        request = postJSON codePipeline
        response
          = receiveJSON
              (\ s h x ->
                 AcknowledgeJobResponse' <$>
                   (x .?> "status") <*> (pure (fromEnum s)))

instance Hashable AcknowledgeJob where

instance NFData AcknowledgeJob where

instance ToHeaders AcknowledgeJob where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.AcknowledgeJob" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AcknowledgeJob where
        toJSON AcknowledgeJob'{..}
          = object
              (catMaybes
                 [Just ("jobId" .= _ajJobId),
                  Just ("nonce" .= _ajNonce)])

instance ToPath AcknowledgeJob where
        toPath = const "/"

instance ToQuery AcknowledgeJob where
        toQuery = const mempty

-- | Represents the output of an AcknowledgeJob action.
--
--
--
-- /See:/ 'acknowledgeJobResponse' smart constructor.
data AcknowledgeJobResponse = AcknowledgeJobResponse'
  { _ajrsStatus         :: !(Maybe JobStatus)
  , _ajrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AcknowledgeJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ajrsStatus' - Whether the job worker has received the specified job.
--
-- * 'ajrsResponseStatus' - -- | The response status code.
acknowledgeJobResponse
    :: Int -- ^ 'ajrsResponseStatus'
    -> AcknowledgeJobResponse
acknowledgeJobResponse pResponseStatus_ =
  AcknowledgeJobResponse'
    {_ajrsStatus = Nothing, _ajrsResponseStatus = pResponseStatus_}


-- | Whether the job worker has received the specified job.
ajrsStatus :: Lens' AcknowledgeJobResponse (Maybe JobStatus)
ajrsStatus = lens _ajrsStatus (\ s a -> s{_ajrsStatus = a})

-- | -- | The response status code.
ajrsResponseStatus :: Lens' AcknowledgeJobResponse Int
ajrsResponseStatus = lens _ajrsResponseStatus (\ s a -> s{_ajrsResponseStatus = a})

instance NFData AcknowledgeJobResponse where

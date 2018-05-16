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
-- Module      : Network.AWS.Batch.TerminateJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates a job in a job queue. Jobs that are in the @STARTING@ or @RUNNING@ state are terminated, which causes them to transition to @FAILED@ . Jobs that have not progressed to the @STARTING@ state are cancelled.
--
--
module Network.AWS.Batch.TerminateJob
    (
    -- * Creating a Request
      terminateJob
    , TerminateJob
    -- * Request Lenses
    , tjJobId
    , tjReason

    -- * Destructuring the Response
    , terminateJobResponse
    , TerminateJobResponse
    -- * Response Lenses
    , tjrsResponseStatus
    ) where

import Network.AWS.Batch.Types
import Network.AWS.Batch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'terminateJob' smart constructor.
data TerminateJob = TerminateJob'
  { _tjJobId  :: !Text
  , _tjReason :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TerminateJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tjJobId' - The AWS Batch job ID of the job to terminate.
--
-- * 'tjReason' - A message to attach to the job that explains the reason for canceling it. This message is returned by future 'DescribeJobs' operations on the job. This message is also recorded in the AWS Batch activity logs.
terminateJob
    :: Text -- ^ 'tjJobId'
    -> Text -- ^ 'tjReason'
    -> TerminateJob
terminateJob pJobId_ pReason_ =
  TerminateJob' {_tjJobId = pJobId_, _tjReason = pReason_}


-- | The AWS Batch job ID of the job to terminate.
tjJobId :: Lens' TerminateJob Text
tjJobId = lens _tjJobId (\ s a -> s{_tjJobId = a})

-- | A message to attach to the job that explains the reason for canceling it. This message is returned by future 'DescribeJobs' operations on the job. This message is also recorded in the AWS Batch activity logs.
tjReason :: Lens' TerminateJob Text
tjReason = lens _tjReason (\ s a -> s{_tjReason = a})

instance AWSRequest TerminateJob where
        type Rs TerminateJob = TerminateJobResponse
        request = postJSON batch
        response
          = receiveEmpty
              (\ s h x ->
                 TerminateJobResponse' <$> (pure (fromEnum s)))

instance Hashable TerminateJob where

instance NFData TerminateJob where

instance ToHeaders TerminateJob where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON TerminateJob where
        toJSON TerminateJob'{..}
          = object
              (catMaybes
                 [Just ("jobId" .= _tjJobId),
                  Just ("reason" .= _tjReason)])

instance ToPath TerminateJob where
        toPath = const "/v1/terminatejob"

instance ToQuery TerminateJob where
        toQuery = const mempty

-- | /See:/ 'terminateJobResponse' smart constructor.
newtype TerminateJobResponse = TerminateJobResponse'
  { _tjrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TerminateJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tjrsResponseStatus' - -- | The response status code.
terminateJobResponse
    :: Int -- ^ 'tjrsResponseStatus'
    -> TerminateJobResponse
terminateJobResponse pResponseStatus_ =
  TerminateJobResponse' {_tjrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
tjrsResponseStatus :: Lens' TerminateJobResponse Int
tjrsResponseStatus = lens _tjrsResponseStatus (\ s a -> s{_tjrsResponseStatus = a})

instance NFData TerminateJobResponse where

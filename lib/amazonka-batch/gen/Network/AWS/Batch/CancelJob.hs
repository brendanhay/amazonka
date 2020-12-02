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
-- Module      : Network.AWS.Batch.CancelJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a job in an AWS Batch job queue. Jobs that are in the @SUBMITTED@ , @PENDING@ , or @RUNNABLE@ state are cancelled. Jobs that have progressed to @STARTING@ or @RUNNING@ are not cancelled (but the API operation still succeeds, even if no job is cancelled); these jobs must be terminated with the 'TerminateJob' operation.
--
--
module Network.AWS.Batch.CancelJob
    (
    -- * Creating a Request
      cancelJob
    , CancelJob
    -- * Request Lenses
    , cjJobId
    , cjReason

    -- * Destructuring the Response
    , cancelJobResponse
    , CancelJobResponse
    -- * Response Lenses
    , cjrsResponseStatus
    ) where

import Network.AWS.Batch.Types
import Network.AWS.Batch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'cancelJob' smart constructor.
data CancelJob = CancelJob'
  { _cjJobId  :: !Text
  , _cjReason :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjJobId' - The AWS Batch job ID of the job to cancel.
--
-- * 'cjReason' - A message to attach to the job that explains the reason for canceling it. This message is returned by future 'DescribeJobs' operations on the job. This message is also recorded in the AWS Batch activity logs.
cancelJob
    :: Text -- ^ 'cjJobId'
    -> Text -- ^ 'cjReason'
    -> CancelJob
cancelJob pJobId_ pReason_ =
  CancelJob' {_cjJobId = pJobId_, _cjReason = pReason_}


-- | The AWS Batch job ID of the job to cancel.
cjJobId :: Lens' CancelJob Text
cjJobId = lens _cjJobId (\ s a -> s{_cjJobId = a})

-- | A message to attach to the job that explains the reason for canceling it. This message is returned by future 'DescribeJobs' operations on the job. This message is also recorded in the AWS Batch activity logs.
cjReason :: Lens' CancelJob Text
cjReason = lens _cjReason (\ s a -> s{_cjReason = a})

instance AWSRequest CancelJob where
        type Rs CancelJob = CancelJobResponse
        request = postJSON batch
        response
          = receiveEmpty
              (\ s h x ->
                 CancelJobResponse' <$> (pure (fromEnum s)))

instance Hashable CancelJob where

instance NFData CancelJob where

instance ToHeaders CancelJob where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CancelJob where
        toJSON CancelJob'{..}
          = object
              (catMaybes
                 [Just ("jobId" .= _cjJobId),
                  Just ("reason" .= _cjReason)])

instance ToPath CancelJob where
        toPath = const "/v1/canceljob"

instance ToQuery CancelJob where
        toQuery = const mempty

-- | /See:/ 'cancelJobResponse' smart constructor.
newtype CancelJobResponse = CancelJobResponse'
  { _cjrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjrsResponseStatus' - -- | The response status code.
cancelJobResponse
    :: Int -- ^ 'cjrsResponseStatus'
    -> CancelJobResponse
cancelJobResponse pResponseStatus_ =
  CancelJobResponse' {_cjrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
cjrsResponseStatus :: Lens' CancelJobResponse Int
cjrsResponseStatus = lens _cjrsResponseStatus (\ s a -> s{_cjrsResponseStatus = a})

instance NFData CancelJobResponse where

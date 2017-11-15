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
-- Module      : Network.AWS.Glue.BatchStopJobRun
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a batch of job runs for a given job.
--
--
module Network.AWS.Glue.BatchStopJobRun
    (
    -- * Creating a Request
      batchStopJobRun
    , BatchStopJobRun
    -- * Request Lenses
    , bsjrJobName
    , bsjrJobRunIds

    -- * Destructuring the Response
    , batchStopJobRunResponse
    , BatchStopJobRunResponse
    -- * Response Lenses
    , bsjrrsSuccessfulSubmissions
    , bsjrrsErrors
    , bsjrrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchStopJobRun' smart constructor.
data BatchStopJobRun = BatchStopJobRun'
  { _bsjrJobName   :: !Text
  , _bsjrJobRunIds :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchStopJobRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsjrJobName' - The name of the job whose job runs are to be stopped.
--
-- * 'bsjrJobRunIds' - A list of job run Ids of the given job to be stopped.
batchStopJobRun
    :: Text -- ^ 'bsjrJobName'
    -> NonEmpty Text -- ^ 'bsjrJobRunIds'
    -> BatchStopJobRun
batchStopJobRun pJobName_ pJobRunIds_ =
  BatchStopJobRun'
  {_bsjrJobName = pJobName_, _bsjrJobRunIds = _List1 # pJobRunIds_}


-- | The name of the job whose job runs are to be stopped.
bsjrJobName :: Lens' BatchStopJobRun Text
bsjrJobName = lens _bsjrJobName (\ s a -> s{_bsjrJobName = a});

-- | A list of job run Ids of the given job to be stopped.
bsjrJobRunIds :: Lens' BatchStopJobRun (NonEmpty Text)
bsjrJobRunIds = lens _bsjrJobRunIds (\ s a -> s{_bsjrJobRunIds = a}) . _List1;

instance AWSRequest BatchStopJobRun where
        type Rs BatchStopJobRun = BatchStopJobRunResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 BatchStopJobRunResponse' <$>
                   (x .?> "SuccessfulSubmissions" .!@ mempty) <*>
                     (x .?> "Errors" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable BatchStopJobRun where

instance NFData BatchStopJobRun where

instance ToHeaders BatchStopJobRun where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.BatchStopJobRun" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchStopJobRun where
        toJSON BatchStopJobRun'{..}
          = object
              (catMaybes
                 [Just ("JobName" .= _bsjrJobName),
                  Just ("JobRunIds" .= _bsjrJobRunIds)])

instance ToPath BatchStopJobRun where
        toPath = const "/"

instance ToQuery BatchStopJobRun where
        toQuery = const mempty

-- | /See:/ 'batchStopJobRunResponse' smart constructor.
data BatchStopJobRunResponse = BatchStopJobRunResponse'
  { _bsjrrsSuccessfulSubmissions :: !(Maybe [BatchStopJobRunSuccessfulSubmission])
  , _bsjrrsErrors :: !(Maybe [BatchStopJobRunError])
  , _bsjrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchStopJobRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bsjrrsSuccessfulSubmissions' - A list of job runs which are successfully submitted for stopping.
--
-- * 'bsjrrsErrors' - A list containing the job run Ids and details of the error that occurred for each job run while submitting to stop.
--
-- * 'bsjrrsResponseStatus' - -- | The response status code.
batchStopJobRunResponse
    :: Int -- ^ 'bsjrrsResponseStatus'
    -> BatchStopJobRunResponse
batchStopJobRunResponse pResponseStatus_ =
  BatchStopJobRunResponse'
  { _bsjrrsSuccessfulSubmissions = Nothing
  , _bsjrrsErrors = Nothing
  , _bsjrrsResponseStatus = pResponseStatus_
  }


-- | A list of job runs which are successfully submitted for stopping.
bsjrrsSuccessfulSubmissions :: Lens' BatchStopJobRunResponse [BatchStopJobRunSuccessfulSubmission]
bsjrrsSuccessfulSubmissions = lens _bsjrrsSuccessfulSubmissions (\ s a -> s{_bsjrrsSuccessfulSubmissions = a}) . _Default . _Coerce;

-- | A list containing the job run Ids and details of the error that occurred for each job run while submitting to stop.
bsjrrsErrors :: Lens' BatchStopJobRunResponse [BatchStopJobRunError]
bsjrrsErrors = lens _bsjrrsErrors (\ s a -> s{_bsjrrsErrors = a}) . _Default . _Coerce;

-- | -- | The response status code.
bsjrrsResponseStatus :: Lens' BatchStopJobRunResponse Int
bsjrrsResponseStatus = lens _bsjrrsResponseStatus (\ s a -> s{_bsjrrsResponseStatus = a});

instance NFData BatchStopJobRunResponse where

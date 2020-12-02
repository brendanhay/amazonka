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
-- Module      : Network.AWS.Glue.StartJobRun
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a job run using a job definition.
--
--
module Network.AWS.Glue.StartJobRun
    (
    -- * Creating a Request
      startJobRun
    , StartJobRun
    -- * Request Lenses
    , sjrArguments
    , sjrAllocatedCapacity
    , sjrTimeout
    , sjrJobRunId
    , sjrJobName

    -- * Destructuring the Response
    , startJobRunResponse
    , StartJobRunResponse
    -- * Response Lenses
    , sjrrsJobRunId
    , sjrrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startJobRun' smart constructor.
data StartJobRun = StartJobRun'
  { _sjrArguments         :: !(Maybe (Map Text Text))
  , _sjrAllocatedCapacity :: !(Maybe Int)
  , _sjrTimeout           :: !(Maybe Nat)
  , _sjrJobRunId          :: !(Maybe Text)
  , _sjrJobName           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartJobRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sjrArguments' - The job arguments specifically for this run. They override the equivalent default arguments set for in the job definition itself. You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes. For information about how to specify and consume your own Job arguments, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide. For information about the key-value pairs that AWS Glue consumes to set up your job, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
--
-- * 'sjrAllocatedCapacity' - The number of AWS Glue data processing units (DPUs) to allocate to this JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
--
-- * 'sjrTimeout' - The job run timeout in minutes. It overrides the timeout value of the job.
--
-- * 'sjrJobRunId' - The ID of a previous JobRun to retry.
--
-- * 'sjrJobName' - The name of the job definition to use.
startJobRun
    :: Text -- ^ 'sjrJobName'
    -> StartJobRun
startJobRun pJobName_ =
  StartJobRun'
    { _sjrArguments = Nothing
    , _sjrAllocatedCapacity = Nothing
    , _sjrTimeout = Nothing
    , _sjrJobRunId = Nothing
    , _sjrJobName = pJobName_
    }


-- | The job arguments specifically for this run. They override the equivalent default arguments set for in the job definition itself. You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes. For information about how to specify and consume your own Job arguments, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide. For information about the key-value pairs that AWS Glue consumes to set up your job, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
sjrArguments :: Lens' StartJobRun (HashMap Text Text)
sjrArguments = lens _sjrArguments (\ s a -> s{_sjrArguments = a}) . _Default . _Map

-- | The number of AWS Glue data processing units (DPUs) to allocate to this JobRun. From 2 to 100 DPUs can be allocated; the default is 10. A DPU is a relative measure of processing power that consists of 4 vCPUs of compute capacity and 16 GB of memory. For more information, see the <https://aws.amazon.com/glue/pricing/ AWS Glue pricing page> .
sjrAllocatedCapacity :: Lens' StartJobRun (Maybe Int)
sjrAllocatedCapacity = lens _sjrAllocatedCapacity (\ s a -> s{_sjrAllocatedCapacity = a})

-- | The job run timeout in minutes. It overrides the timeout value of the job.
sjrTimeout :: Lens' StartJobRun (Maybe Natural)
sjrTimeout = lens _sjrTimeout (\ s a -> s{_sjrTimeout = a}) . mapping _Nat

-- | The ID of a previous JobRun to retry.
sjrJobRunId :: Lens' StartJobRun (Maybe Text)
sjrJobRunId = lens _sjrJobRunId (\ s a -> s{_sjrJobRunId = a})

-- | The name of the job definition to use.
sjrJobName :: Lens' StartJobRun Text
sjrJobName = lens _sjrJobName (\ s a -> s{_sjrJobName = a})

instance AWSRequest StartJobRun where
        type Rs StartJobRun = StartJobRunResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 StartJobRunResponse' <$>
                   (x .?> "JobRunId") <*> (pure (fromEnum s)))

instance Hashable StartJobRun where

instance NFData StartJobRun where

instance ToHeaders StartJobRun where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.StartJobRun" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartJobRun where
        toJSON StartJobRun'{..}
          = object
              (catMaybes
                 [("Arguments" .=) <$> _sjrArguments,
                  ("AllocatedCapacity" .=) <$> _sjrAllocatedCapacity,
                  ("Timeout" .=) <$> _sjrTimeout,
                  ("JobRunId" .=) <$> _sjrJobRunId,
                  Just ("JobName" .= _sjrJobName)])

instance ToPath StartJobRun where
        toPath = const "/"

instance ToQuery StartJobRun where
        toQuery = const mempty

-- | /See:/ 'startJobRunResponse' smart constructor.
data StartJobRunResponse = StartJobRunResponse'
  { _sjrrsJobRunId       :: !(Maybe Text)
  , _sjrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartJobRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sjrrsJobRunId' - The ID assigned to this job run.
--
-- * 'sjrrsResponseStatus' - -- | The response status code.
startJobRunResponse
    :: Int -- ^ 'sjrrsResponseStatus'
    -> StartJobRunResponse
startJobRunResponse pResponseStatus_ =
  StartJobRunResponse'
    {_sjrrsJobRunId = Nothing, _sjrrsResponseStatus = pResponseStatus_}


-- | The ID assigned to this job run.
sjrrsJobRunId :: Lens' StartJobRunResponse (Maybe Text)
sjrrsJobRunId = lens _sjrrsJobRunId (\ s a -> s{_sjrrsJobRunId = a})

-- | -- | The response status code.
sjrrsResponseStatus :: Lens' StartJobRunResponse Int
sjrrsResponseStatus = lens _sjrrsResponseStatus (\ s a -> s{_sjrrsResponseStatus = a})

instance NFData StartJobRunResponse where

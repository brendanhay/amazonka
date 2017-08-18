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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs a job.
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
    , sjrJobRunId
    , sjrJobName

    -- * Destructuring the Response
    , startJobRunResponse
    , StartJobRunResponse
    -- * Response Lenses
    , sjrrsJobRunId
    , sjrrsResponseStatus
    ) where

import           Network.AWS.Glue.Types
import           Network.AWS.Glue.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'startJobRun' smart constructor.
data StartJobRun = StartJobRun'
    { _sjrArguments         :: !(Maybe (Map Text Text))
    , _sjrAllocatedCapacity :: !(Maybe Int)
    , _sjrJobRunId          :: !(Maybe Text)
    , _sjrJobName           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StartJobRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sjrArguments' - Specific arguments for this job run.
--
-- * 'sjrAllocatedCapacity' - The infrastructure capacity to allocate to this job.
--
-- * 'sjrJobRunId' - The ID of the job run to start.
--
-- * 'sjrJobName' - The name of the job to start.
startJobRun
    :: Text -- ^ 'sjrJobName'
    -> StartJobRun
startJobRun pJobName_ =
    StartJobRun'
    { _sjrArguments = Nothing
    , _sjrAllocatedCapacity = Nothing
    , _sjrJobRunId = Nothing
    , _sjrJobName = pJobName_
    }

-- | Specific arguments for this job run.
sjrArguments :: Lens' StartJobRun (HashMap Text Text)
sjrArguments = lens _sjrArguments (\ s a -> s{_sjrArguments = a}) . _Default . _Map;

-- | The infrastructure capacity to allocate to this job.
sjrAllocatedCapacity :: Lens' StartJobRun (Maybe Int)
sjrAllocatedCapacity = lens _sjrAllocatedCapacity (\ s a -> s{_sjrAllocatedCapacity = a});

-- | The ID of the job run to start.
sjrJobRunId :: Lens' StartJobRun (Maybe Text)
sjrJobRunId = lens _sjrJobRunId (\ s a -> s{_sjrJobRunId = a});

-- | The name of the job to start.
sjrJobName :: Lens' StartJobRun Text
sjrJobName = lens _sjrJobName (\ s a -> s{_sjrJobName = a});

instance AWSRequest StartJobRun where
        type Rs StartJobRun = StartJobRunResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 StartJobRunResponse' <$>
                   (x .?> "JobRunId") <*> (pure (fromEnum s)))

instance Hashable StartJobRun

instance NFData StartJobRun

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    { _sjrrsJobRunId = Nothing
    , _sjrrsResponseStatus = pResponseStatus_
    }

-- | The ID assigned to this job run.
sjrrsJobRunId :: Lens' StartJobRunResponse (Maybe Text)
sjrrsJobRunId = lens _sjrrsJobRunId (\ s a -> s{_sjrrsJobRunId = a});

-- | -- | The response status code.
sjrrsResponseStatus :: Lens' StartJobRunResponse Int
sjrrsResponseStatus = lens _sjrrsResponseStatus (\ s a -> s{_sjrrsResponseStatus = a});

instance NFData StartJobRunResponse

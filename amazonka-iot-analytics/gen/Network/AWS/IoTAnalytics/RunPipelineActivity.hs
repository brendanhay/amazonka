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
-- Module      : Network.AWS.IoTAnalytics.RunPipelineActivity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Simulates the results of running a pipeline activity on a message payload.
--
--
module Network.AWS.IoTAnalytics.RunPipelineActivity
    (
    -- * Creating a Request
      runPipelineActivity
    , RunPipelineActivity
    -- * Request Lenses
    , rpaPipelineActivity
    , rpaPayloads

    -- * Destructuring the Response
    , runPipelineActivityResponse
    , RunPipelineActivityResponse
    -- * Response Lenses
    , rparsLogResult
    , rparsPayloads
    , rparsResponseStatus
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'runPipelineActivity' smart constructor.
data RunPipelineActivity = RunPipelineActivity'
  { _rpaPipelineActivity :: !PipelineActivity
  , _rpaPayloads         :: !(List1 Base64)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RunPipelineActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpaPipelineActivity' - The pipeline activity that is run. This must not be a 'channel' activity or a 'datastore' activity because these activities are used in a pipeline only to load the original message and to store the (possibly) transformed message. If a 'lambda' activity is specified, only short-running Lambda functions (those with a timeout of less than 30 seconds or less) can be used.
--
-- * 'rpaPayloads' - The sample message payloads on which the pipeline activity is run.
runPipelineActivity
    :: PipelineActivity -- ^ 'rpaPipelineActivity'
    -> NonEmpty ByteString -- ^ 'rpaPayloads'
    -> RunPipelineActivity
runPipelineActivity pPipelineActivity_ pPayloads_ =
  RunPipelineActivity'
    { _rpaPipelineActivity = pPipelineActivity_
    , _rpaPayloads = _List1 # pPayloads_
    }


-- | The pipeline activity that is run. This must not be a 'channel' activity or a 'datastore' activity because these activities are used in a pipeline only to load the original message and to store the (possibly) transformed message. If a 'lambda' activity is specified, only short-running Lambda functions (those with a timeout of less than 30 seconds or less) can be used.
rpaPipelineActivity :: Lens' RunPipelineActivity PipelineActivity
rpaPipelineActivity = lens _rpaPipelineActivity (\ s a -> s{_rpaPipelineActivity = a})

-- | The sample message payloads on which the pipeline activity is run.
rpaPayloads :: Lens' RunPipelineActivity (NonEmpty ByteString)
rpaPayloads = lens _rpaPayloads (\ s a -> s{_rpaPayloads = a}) . _List1

instance AWSRequest RunPipelineActivity where
        type Rs RunPipelineActivity =
             RunPipelineActivityResponse
        request = postJSON ioTAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 RunPipelineActivityResponse' <$>
                   (x .?> "logResult") <*> (x .?> "payloads") <*>
                     (pure (fromEnum s)))

instance Hashable RunPipelineActivity where

instance NFData RunPipelineActivity where

instance ToHeaders RunPipelineActivity where
        toHeaders = const mempty

instance ToJSON RunPipelineActivity where
        toJSON RunPipelineActivity'{..}
          = object
              (catMaybes
                 [Just ("pipelineActivity" .= _rpaPipelineActivity),
                  Just ("payloads" .= _rpaPayloads)])

instance ToPath RunPipelineActivity where
        toPath = const "/pipelineactivities/run"

instance ToQuery RunPipelineActivity where
        toQuery = const mempty

-- | /See:/ 'runPipelineActivityResponse' smart constructor.
data RunPipelineActivityResponse = RunPipelineActivityResponse'
  { _rparsLogResult      :: !(Maybe Text)
  , _rparsPayloads       :: !(Maybe (List1 Base64))
  , _rparsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RunPipelineActivityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rparsLogResult' - In case the pipeline activity fails, the log message that is generated.
--
-- * 'rparsPayloads' - The enriched or transformed sample message payloads as base64-encoded strings. (The results of running the pipeline activity on each input sample message payload, encoded in base64.)
--
-- * 'rparsResponseStatus' - -- | The response status code.
runPipelineActivityResponse
    :: Int -- ^ 'rparsResponseStatus'
    -> RunPipelineActivityResponse
runPipelineActivityResponse pResponseStatus_ =
  RunPipelineActivityResponse'
    { _rparsLogResult = Nothing
    , _rparsPayloads = Nothing
    , _rparsResponseStatus = pResponseStatus_
    }


-- | In case the pipeline activity fails, the log message that is generated.
rparsLogResult :: Lens' RunPipelineActivityResponse (Maybe Text)
rparsLogResult = lens _rparsLogResult (\ s a -> s{_rparsLogResult = a})

-- | The enriched or transformed sample message payloads as base64-encoded strings. (The results of running the pipeline activity on each input sample message payload, encoded in base64.)
rparsPayloads :: Lens' RunPipelineActivityResponse (Maybe (NonEmpty ByteString))
rparsPayloads = lens _rparsPayloads (\ s a -> s{_rparsPayloads = a}) . mapping _List1

-- | -- | The response status code.
rparsResponseStatus :: Lens' RunPipelineActivityResponse Int
rparsResponseStatus = lens _rparsResponseStatus (\ s a -> s{_rparsResponseStatus = a})

instance NFData RunPipelineActivityResponse where

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
-- Module      : Network.AWS.CodePipeline.RetryStageExecution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resumes the pipeline execution by retrying the last failed actions in a stage.
--
--
module Network.AWS.CodePipeline.RetryStageExecution
    (
    -- * Creating a Request
      retryStageExecution
    , RetryStageExecution
    -- * Request Lenses
    , rsePipelineName
    , rseStageName
    , rsePipelineExecutionId
    , rseRetryMode

    -- * Destructuring the Response
    , retryStageExecutionResponse
    , RetryStageExecutionResponse
    -- * Response Lenses
    , rsersPipelineExecutionId
    , rsersResponseStatus
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a RetryStageExecution action.
--
--
--
-- /See:/ 'retryStageExecution' smart constructor.
data RetryStageExecution = RetryStageExecution'
  { _rsePipelineName        :: !Text
  , _rseStageName           :: !Text
  , _rsePipelineExecutionId :: !Text
  , _rseRetryMode           :: !StageRetryMode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RetryStageExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsePipelineName' - The name of the pipeline that contains the failed stage.
--
-- * 'rseStageName' - The name of the failed stage to be retried.
--
-- * 'rsePipelineExecutionId' - The ID of the pipeline execution in the failed stage to be retried. Use the 'GetPipelineState' action to retrieve the current pipelineExecutionId of the failed stage
--
-- * 'rseRetryMode' - The scope of the retry attempt. Currently, the only supported value is FAILED_ACTIONS.
retryStageExecution
    :: Text -- ^ 'rsePipelineName'
    -> Text -- ^ 'rseStageName'
    -> Text -- ^ 'rsePipelineExecutionId'
    -> StageRetryMode -- ^ 'rseRetryMode'
    -> RetryStageExecution
retryStageExecution pPipelineName_ pStageName_ pPipelineExecutionId_ pRetryMode_ =
  RetryStageExecution'
    { _rsePipelineName = pPipelineName_
    , _rseStageName = pStageName_
    , _rsePipelineExecutionId = pPipelineExecutionId_
    , _rseRetryMode = pRetryMode_
    }


-- | The name of the pipeline that contains the failed stage.
rsePipelineName :: Lens' RetryStageExecution Text
rsePipelineName = lens _rsePipelineName (\ s a -> s{_rsePipelineName = a})

-- | The name of the failed stage to be retried.
rseStageName :: Lens' RetryStageExecution Text
rseStageName = lens _rseStageName (\ s a -> s{_rseStageName = a})

-- | The ID of the pipeline execution in the failed stage to be retried. Use the 'GetPipelineState' action to retrieve the current pipelineExecutionId of the failed stage
rsePipelineExecutionId :: Lens' RetryStageExecution Text
rsePipelineExecutionId = lens _rsePipelineExecutionId (\ s a -> s{_rsePipelineExecutionId = a})

-- | The scope of the retry attempt. Currently, the only supported value is FAILED_ACTIONS.
rseRetryMode :: Lens' RetryStageExecution StageRetryMode
rseRetryMode = lens _rseRetryMode (\ s a -> s{_rseRetryMode = a})

instance AWSRequest RetryStageExecution where
        type Rs RetryStageExecution =
             RetryStageExecutionResponse
        request = postJSON codePipeline
        response
          = receiveJSON
              (\ s h x ->
                 RetryStageExecutionResponse' <$>
                   (x .?> "pipelineExecutionId") <*>
                     (pure (fromEnum s)))

instance Hashable RetryStageExecution where

instance NFData RetryStageExecution where

instance ToHeaders RetryStageExecution where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.RetryStageExecution" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RetryStageExecution where
        toJSON RetryStageExecution'{..}
          = object
              (catMaybes
                 [Just ("pipelineName" .= _rsePipelineName),
                  Just ("stageName" .= _rseStageName),
                  Just
                    ("pipelineExecutionId" .= _rsePipelineExecutionId),
                  Just ("retryMode" .= _rseRetryMode)])

instance ToPath RetryStageExecution where
        toPath = const "/"

instance ToQuery RetryStageExecution where
        toQuery = const mempty

-- | Represents the output of a RetryStageExecution action.
--
--
--
-- /See:/ 'retryStageExecutionResponse' smart constructor.
data RetryStageExecutionResponse = RetryStageExecutionResponse'
  { _rsersPipelineExecutionId :: !(Maybe Text)
  , _rsersResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RetryStageExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsersPipelineExecutionId' - The ID of the current workflow execution in the failed stage.
--
-- * 'rsersResponseStatus' - -- | The response status code.
retryStageExecutionResponse
    :: Int -- ^ 'rsersResponseStatus'
    -> RetryStageExecutionResponse
retryStageExecutionResponse pResponseStatus_ =
  RetryStageExecutionResponse'
    { _rsersPipelineExecutionId = Nothing
    , _rsersResponseStatus = pResponseStatus_
    }


-- | The ID of the current workflow execution in the failed stage.
rsersPipelineExecutionId :: Lens' RetryStageExecutionResponse (Maybe Text)
rsersPipelineExecutionId = lens _rsersPipelineExecutionId (\ s a -> s{_rsersPipelineExecutionId = a})

-- | -- | The response status code.
rsersResponseStatus :: Lens' RetryStageExecutionResponse Int
rsersResponseStatus = lens _rsersResponseStatus (\ s a -> s{_rsersResponseStatus = a})

instance NFData RetryStageExecutionResponse where

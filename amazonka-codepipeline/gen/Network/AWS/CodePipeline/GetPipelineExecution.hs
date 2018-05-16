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
-- Module      : Network.AWS.CodePipeline.GetPipelineExecution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an execution of a pipeline, including details about artifacts, the pipeline execution ID, and the name, version, and status of the pipeline.
--
--
module Network.AWS.CodePipeline.GetPipelineExecution
    (
    -- * Creating a Request
      getPipelineExecution
    , GetPipelineExecution
    -- * Request Lenses
    , gpePipelineName
    , gpePipelineExecutionId

    -- * Destructuring the Response
    , getPipelineExecutionResponse
    , GetPipelineExecutionResponse
    -- * Response Lenses
    , gpersPipelineExecution
    , gpersResponseStatus
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a GetPipelineExecution action.
--
--
--
-- /See:/ 'getPipelineExecution' smart constructor.
data GetPipelineExecution = GetPipelineExecution'
  { _gpePipelineName        :: !Text
  , _gpePipelineExecutionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPipelineExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpePipelineName' - The name of the pipeline about which you want to get execution details.
--
-- * 'gpePipelineExecutionId' - The ID of the pipeline execution about which you want to get execution details.
getPipelineExecution
    :: Text -- ^ 'gpePipelineName'
    -> Text -- ^ 'gpePipelineExecutionId'
    -> GetPipelineExecution
getPipelineExecution pPipelineName_ pPipelineExecutionId_ =
  GetPipelineExecution'
    { _gpePipelineName = pPipelineName_
    , _gpePipelineExecutionId = pPipelineExecutionId_
    }


-- | The name of the pipeline about which you want to get execution details.
gpePipelineName :: Lens' GetPipelineExecution Text
gpePipelineName = lens _gpePipelineName (\ s a -> s{_gpePipelineName = a})

-- | The ID of the pipeline execution about which you want to get execution details.
gpePipelineExecutionId :: Lens' GetPipelineExecution Text
gpePipelineExecutionId = lens _gpePipelineExecutionId (\ s a -> s{_gpePipelineExecutionId = a})

instance AWSRequest GetPipelineExecution where
        type Rs GetPipelineExecution =
             GetPipelineExecutionResponse
        request = postJSON codePipeline
        response
          = receiveJSON
              (\ s h x ->
                 GetPipelineExecutionResponse' <$>
                   (x .?> "pipelineExecution") <*> (pure (fromEnum s)))

instance Hashable GetPipelineExecution where

instance NFData GetPipelineExecution where

instance ToHeaders GetPipelineExecution where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.GetPipelineExecution" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetPipelineExecution where
        toJSON GetPipelineExecution'{..}
          = object
              (catMaybes
                 [Just ("pipelineName" .= _gpePipelineName),
                  Just
                    ("pipelineExecutionId" .= _gpePipelineExecutionId)])

instance ToPath GetPipelineExecution where
        toPath = const "/"

instance ToQuery GetPipelineExecution where
        toQuery = const mempty

-- | Represents the output of a GetPipelineExecution action.
--
--
--
-- /See:/ 'getPipelineExecutionResponse' smart constructor.
data GetPipelineExecutionResponse = GetPipelineExecutionResponse'
  { _gpersPipelineExecution :: !(Maybe PipelineExecution)
  , _gpersResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPipelineExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpersPipelineExecution' - Represents information about the execution of a pipeline.
--
-- * 'gpersResponseStatus' - -- | The response status code.
getPipelineExecutionResponse
    :: Int -- ^ 'gpersResponseStatus'
    -> GetPipelineExecutionResponse
getPipelineExecutionResponse pResponseStatus_ =
  GetPipelineExecutionResponse'
    {_gpersPipelineExecution = Nothing, _gpersResponseStatus = pResponseStatus_}


-- | Represents information about the execution of a pipeline.
gpersPipelineExecution :: Lens' GetPipelineExecutionResponse (Maybe PipelineExecution)
gpersPipelineExecution = lens _gpersPipelineExecution (\ s a -> s{_gpersPipelineExecution = a})

-- | -- | The response status code.
gpersResponseStatus :: Lens' GetPipelineExecutionResponse Int
gpersResponseStatus = lens _gpersResponseStatus (\ s a -> s{_gpersResponseStatus = a})

instance NFData GetPipelineExecutionResponse where

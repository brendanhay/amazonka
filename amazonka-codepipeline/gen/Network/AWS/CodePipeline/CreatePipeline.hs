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
-- Module      : Network.AWS.CodePipeline.CreatePipeline
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a pipeline.
--
--
module Network.AWS.CodePipeline.CreatePipeline
    (
    -- * Creating a Request
      createPipeline
    , CreatePipeline
    -- * Request Lenses
    , cpPipeline

    -- * Destructuring the Response
    , createPipelineResponse
    , CreatePipelineResponse
    -- * Response Lenses
    , cprsPipeline
    , cprsResponseStatus
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a CreatePipeline action.
--
--
--
-- /See:/ 'createPipeline' smart constructor.
newtype CreatePipeline = CreatePipeline'
  { _cpPipeline :: PipelineDeclaration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePipeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpPipeline' - Represents the structure of actions and stages to be performed in the pipeline.
createPipeline
    :: PipelineDeclaration -- ^ 'cpPipeline'
    -> CreatePipeline
createPipeline pPipeline_ = CreatePipeline' {_cpPipeline = pPipeline_}


-- | Represents the structure of actions and stages to be performed in the pipeline.
cpPipeline :: Lens' CreatePipeline PipelineDeclaration
cpPipeline = lens _cpPipeline (\ s a -> s{_cpPipeline = a})

instance AWSRequest CreatePipeline where
        type Rs CreatePipeline = CreatePipelineResponse
        request = postJSON codePipeline
        response
          = receiveJSON
              (\ s h x ->
                 CreatePipelineResponse' <$>
                   (x .?> "pipeline") <*> (pure (fromEnum s)))

instance Hashable CreatePipeline where

instance NFData CreatePipeline where

instance ToHeaders CreatePipeline where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.CreatePipeline" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreatePipeline where
        toJSON CreatePipeline'{..}
          = object
              (catMaybes [Just ("pipeline" .= _cpPipeline)])

instance ToPath CreatePipeline where
        toPath = const "/"

instance ToQuery CreatePipeline where
        toQuery = const mempty

-- | Represents the output of a CreatePipeline action.
--
--
--
-- /See:/ 'createPipelineResponse' smart constructor.
data CreatePipelineResponse = CreatePipelineResponse'
  { _cprsPipeline       :: !(Maybe PipelineDeclaration)
  , _cprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePipelineResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsPipeline' - Represents the structure of actions and stages to be performed in the pipeline.
--
-- * 'cprsResponseStatus' - -- | The response status code.
createPipelineResponse
    :: Int -- ^ 'cprsResponseStatus'
    -> CreatePipelineResponse
createPipelineResponse pResponseStatus_ =
  CreatePipelineResponse'
    {_cprsPipeline = Nothing, _cprsResponseStatus = pResponseStatus_}


-- | Represents the structure of actions and stages to be performed in the pipeline.
cprsPipeline :: Lens' CreatePipelineResponse (Maybe PipelineDeclaration)
cprsPipeline = lens _cprsPipeline (\ s a -> s{_cprsPipeline = a})

-- | -- | The response status code.
cprsResponseStatus :: Lens' CreatePipelineResponse Int
cprsResponseStatus = lens _cprsResponseStatus (\ s a -> s{_cprsResponseStatus = a})

instance NFData CreatePipelineResponse where

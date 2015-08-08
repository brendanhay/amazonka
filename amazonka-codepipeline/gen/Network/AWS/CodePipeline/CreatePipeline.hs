{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.CreatePipeline
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a pipeline.
--
-- /See:/ <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_CreatePipeline.html AWS API Reference> for CreatePipeline.
module Network.AWS.CodePipeline.CreatePipeline
    (
    -- * Creating a Request
      CreatePipeline
    , createPipeline
    -- * Request Lenses
    , cpPipeline

    -- * Destructuring the Response
    , CreatePipelineResponse
    , createPipelineResponse
    -- * Response Lenses
    , cprsPipeline
    , cprsStatus
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a create pipeline action.
--
-- /See:/ 'createPipeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpPipeline'
newtype CreatePipeline = CreatePipeline'
    { _cpPipeline :: PipelineDeclaration
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreatePipeline' smart constructor.
createPipeline :: PipelineDeclaration -> CreatePipeline
createPipeline pPipeline_ =
    CreatePipeline'
    { _cpPipeline = pPipeline_
    }

-- | Undocumented member.
cpPipeline :: Lens' CreatePipeline PipelineDeclaration
cpPipeline = lens _cpPipeline (\ s a -> s{_cpPipeline = a});

instance AWSRequest CreatePipeline where
        type Sv CreatePipeline = CodePipeline
        type Rs CreatePipeline = CreatePipelineResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreatePipelineResponse' <$>
                   (x .?> "pipeline") <*> (pure (fromEnum s)))

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
          = object ["pipeline" .= _cpPipeline]

instance ToPath CreatePipeline where
        toPath = const "/"

instance ToQuery CreatePipeline where
        toQuery = const mempty

-- | Represents the output of a create pipeline action.
--
-- /See:/ 'createPipelineResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cprsPipeline'
--
-- * 'cprsStatus'
data CreatePipelineResponse = CreatePipelineResponse'
    { _cprsPipeline :: !(Maybe PipelineDeclaration)
    , _cprsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreatePipelineResponse' smart constructor.
createPipelineResponse :: Int -> CreatePipelineResponse
createPipelineResponse pStatus_ =
    CreatePipelineResponse'
    { _cprsPipeline = Nothing
    , _cprsStatus = pStatus_
    }

-- | Undocumented member.
cprsPipeline :: Lens' CreatePipelineResponse (Maybe PipelineDeclaration)
cprsPipeline = lens _cprsPipeline (\ s a -> s{_cprsPipeline = a});

-- | Undocumented member.
cprsStatus :: Lens' CreatePipelineResponse Int
cprsStatus = lens _cprsStatus (\ s a -> s{_cprsStatus = a});

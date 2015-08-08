{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.UpdatePipeline
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified pipeline with edits or changes to its structure. Use
-- a JSON file with the pipeline structure in conjunction with
-- UpdatePipeline to provide the full structure of the pipeline. Updating
-- the pipeline increases the version number of the pipeline by 1.
--
-- /See:/ <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_UpdatePipeline.html AWS API Reference> for UpdatePipeline.
module Network.AWS.CodePipeline.UpdatePipeline
    (
    -- * Creating a Request
      UpdatePipeline
    , updatePipeline
    -- * Request Lenses
    , upPipeline

    -- * Destructuring the Response
    , UpdatePipelineResponse
    , updatePipelineResponse
    -- * Response Lenses
    , uprsPipeline
    , uprsStatus
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of an update pipeline action.
--
-- /See:/ 'updatePipeline' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upPipeline'
newtype UpdatePipeline = UpdatePipeline'
    { _upPipeline :: PipelineDeclaration
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdatePipeline' smart constructor.
updatePipeline :: PipelineDeclaration -> UpdatePipeline
updatePipeline pPipeline_ =
    UpdatePipeline'
    { _upPipeline = pPipeline_
    }

-- | The name of the pipeline to be updated.
upPipeline :: Lens' UpdatePipeline PipelineDeclaration
upPipeline = lens _upPipeline (\ s a -> s{_upPipeline = a});

instance AWSRequest UpdatePipeline where
        type Sv UpdatePipeline = CodePipeline
        type Rs UpdatePipeline = UpdatePipelineResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdatePipelineResponse' <$>
                   (x .?> "pipeline") <*> (pure (fromEnum s)))

instance ToHeaders UpdatePipeline where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.UpdatePipeline" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdatePipeline where
        toJSON UpdatePipeline'{..}
          = object ["pipeline" .= _upPipeline]

instance ToPath UpdatePipeline where
        toPath = const "/"

instance ToQuery UpdatePipeline where
        toQuery = const mempty

-- | Represents the output of an update pipeline action.
--
-- /See:/ 'updatePipelineResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uprsPipeline'
--
-- * 'uprsStatus'
data UpdatePipelineResponse = UpdatePipelineResponse'
    { _uprsPipeline :: !(Maybe PipelineDeclaration)
    , _uprsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdatePipelineResponse' smart constructor.
updatePipelineResponse :: Int -> UpdatePipelineResponse
updatePipelineResponse pStatus_ =
    UpdatePipelineResponse'
    { _uprsPipeline = Nothing
    , _uprsStatus = pStatus_
    }

-- | The structure of the updated pipeline.
uprsPipeline :: Lens' UpdatePipelineResponse (Maybe PipelineDeclaration)
uprsPipeline = lens _uprsPipeline (\ s a -> s{_uprsPipeline = a});

-- | Undocumented member.
uprsStatus :: Lens' UpdatePipelineResponse Int
uprsStatus = lens _uprsStatus (\ s a -> s{_uprsStatus = a});

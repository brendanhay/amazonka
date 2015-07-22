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
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified pipeline with edits or changes to its structure. Use
-- a JSON file with the pipeline structure in conjunction with
-- UpdatePipeline to provide the full structure of the pipeline. Updating
-- the pipeline increases the version number of the pipeline by 1.
--
-- <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_UpdatePipeline.html>
module Network.AWS.CodePipeline.UpdatePipeline
    (
    -- * Request
      UpdatePipeline
    -- ** Request constructor
    , updatePipeline
    -- ** Request lenses
    , uprqPipeline

    -- * Response
    , UpdatePipelineResponse
    -- ** Response constructor
    , updatePipelineResponse
    -- ** Response lenses
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
-- * 'uprqPipeline'
newtype UpdatePipeline = UpdatePipeline'
    { _uprqPipeline :: PipelineDeclaration
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdatePipeline' smart constructor.
updatePipeline :: PipelineDeclaration -> UpdatePipeline
updatePipeline pPipeline_ =
    UpdatePipeline'
    { _uprqPipeline = pPipeline_
    }

-- | The name of the pipeline to be updated.
uprqPipeline :: Lens' UpdatePipeline PipelineDeclaration
uprqPipeline = lens _uprqPipeline (\ s a -> s{_uprqPipeline = a});

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
          = object ["pipeline" .= _uprqPipeline]

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

-- | FIXME: Undocumented member.
uprsStatus :: Lens' UpdatePipelineResponse Int
uprsStatus = lens _uprsStatus (\ s a -> s{_uprsStatus = a});

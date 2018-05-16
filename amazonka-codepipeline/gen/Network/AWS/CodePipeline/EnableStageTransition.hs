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
-- Module      : Network.AWS.CodePipeline.EnableStageTransition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables artifacts in a pipeline to transition to a stage in a pipeline.
--
--
module Network.AWS.CodePipeline.EnableStageTransition
    (
    -- * Creating a Request
      enableStageTransition
    , EnableStageTransition
    -- * Request Lenses
    , estPipelineName
    , estStageName
    , estTransitionType

    -- * Destructuring the Response
    , enableStageTransitionResponse
    , EnableStageTransitionResponse
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of an EnableStageTransition action.
--
--
--
-- /See:/ 'enableStageTransition' smart constructor.
data EnableStageTransition = EnableStageTransition'
  { _estPipelineName   :: !Text
  , _estStageName      :: !Text
  , _estTransitionType :: !StageTransitionType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableStageTransition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'estPipelineName' - The name of the pipeline in which you want to enable the flow of artifacts from one stage to another.
--
-- * 'estStageName' - The name of the stage where you want to enable the transition of artifacts, either into the stage (inbound) or from that stage to the next stage (outbound).
--
-- * 'estTransitionType' - Specifies whether artifacts will be allowed to enter the stage and be processed by the actions in that stage (inbound) or whether already-processed artifacts will be allowed to transition to the next stage (outbound).
enableStageTransition
    :: Text -- ^ 'estPipelineName'
    -> Text -- ^ 'estStageName'
    -> StageTransitionType -- ^ 'estTransitionType'
    -> EnableStageTransition
enableStageTransition pPipelineName_ pStageName_ pTransitionType_ =
  EnableStageTransition'
    { _estPipelineName = pPipelineName_
    , _estStageName = pStageName_
    , _estTransitionType = pTransitionType_
    }


-- | The name of the pipeline in which you want to enable the flow of artifacts from one stage to another.
estPipelineName :: Lens' EnableStageTransition Text
estPipelineName = lens _estPipelineName (\ s a -> s{_estPipelineName = a})

-- | The name of the stage where you want to enable the transition of artifacts, either into the stage (inbound) or from that stage to the next stage (outbound).
estStageName :: Lens' EnableStageTransition Text
estStageName = lens _estStageName (\ s a -> s{_estStageName = a})

-- | Specifies whether artifacts will be allowed to enter the stage and be processed by the actions in that stage (inbound) or whether already-processed artifacts will be allowed to transition to the next stage (outbound).
estTransitionType :: Lens' EnableStageTransition StageTransitionType
estTransitionType = lens _estTransitionType (\ s a -> s{_estTransitionType = a})

instance AWSRequest EnableStageTransition where
        type Rs EnableStageTransition =
             EnableStageTransitionResponse
        request = postJSON codePipeline
        response = receiveNull EnableStageTransitionResponse'

instance Hashable EnableStageTransition where

instance NFData EnableStageTransition where

instance ToHeaders EnableStageTransition where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.EnableStageTransition" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON EnableStageTransition where
        toJSON EnableStageTransition'{..}
          = object
              (catMaybes
                 [Just ("pipelineName" .= _estPipelineName),
                  Just ("stageName" .= _estStageName),
                  Just ("transitionType" .= _estTransitionType)])

instance ToPath EnableStageTransition where
        toPath = const "/"

instance ToQuery EnableStageTransition where
        toQuery = const mempty

-- | /See:/ 'enableStageTransitionResponse' smart constructor.
data EnableStageTransitionResponse =
  EnableStageTransitionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableStageTransitionResponse' with the minimum fields required to make a request.
--
enableStageTransitionResponse
    :: EnableStageTransitionResponse
enableStageTransitionResponse = EnableStageTransitionResponse'


instance NFData EnableStageTransitionResponse where

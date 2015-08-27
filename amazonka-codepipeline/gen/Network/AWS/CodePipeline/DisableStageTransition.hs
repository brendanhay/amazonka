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
-- Module      : Network.AWS.CodePipeline.DisableStageTransition
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Prevents artifacts in a pipeline from transitioning to the next stage in
-- the pipeline.
--
-- /See:/ <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_DisableStageTransition.html AWS API Reference> for DisableStageTransition.
module Network.AWS.CodePipeline.DisableStageTransition
    (
    -- * Creating a Request
      disableStageTransition
    , DisableStageTransition
    -- * Request Lenses
    , dstPipelineName
    , dstStageName
    , dstTransitionType
    , dstReason

    -- * Destructuring the Response
    , disableStageTransitionResponse
    , DisableStageTransitionResponse
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.CodePipeline.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a disable stage transition input action.
--
-- /See:/ 'disableStageTransition' smart constructor.
data DisableStageTransition = DisableStageTransition'
    { _dstPipelineName   :: !Text
    , _dstStageName      :: !Text
    , _dstTransitionType :: !StageTransitionType
    , _dstReason         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DisableStageTransition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dstPipelineName'
--
-- * 'dstStageName'
--
-- * 'dstTransitionType'
--
-- * 'dstReason'
disableStageTransition
    :: Text -- ^ 'dstPipelineName'
    -> Text -- ^ 'dstStageName'
    -> StageTransitionType -- ^ 'dstTransitionType'
    -> Text -- ^ 'dstReason'
    -> DisableStageTransition
disableStageTransition pPipelineName_ pStageName_ pTransitionType_ pReason_ =
    DisableStageTransition'
    { _dstPipelineName = pPipelineName_
    , _dstStageName = pStageName_
    , _dstTransitionType = pTransitionType_
    , _dstReason = pReason_
    }

-- | The name of the pipeline in which you want to disable the flow of
-- artifacts from one stage to another.
dstPipelineName :: Lens' DisableStageTransition Text
dstPipelineName = lens _dstPipelineName (\ s a -> s{_dstPipelineName = a});

-- | The name of the stage where you want to disable the inbound or outbound
-- transition of artifacts.
dstStageName :: Lens' DisableStageTransition Text
dstStageName = lens _dstStageName (\ s a -> s{_dstStageName = a});

-- | Specifies whether artifacts will be prevented from transitioning into
-- the stage and being processed by the actions in that stage (inbound), or
-- prevented from transitioning from the stage after they have been
-- processed by the actions in that stage (outbound).
dstTransitionType :: Lens' DisableStageTransition StageTransitionType
dstTransitionType = lens _dstTransitionType (\ s a -> s{_dstTransitionType = a});

-- | The reason given to the user why a stage is disabled, such as waiting
-- for manual approval or manual tests. This message is displayed in the
-- pipeline console UI.
dstReason :: Lens' DisableStageTransition Text
dstReason = lens _dstReason (\ s a -> s{_dstReason = a});

instance AWSRequest DisableStageTransition where
        type Rs DisableStageTransition =
             DisableStageTransitionResponse
        request = postJSON codePipeline
        response
          = receiveNull DisableStageTransitionResponse'

instance ToHeaders DisableStageTransition where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.DisableStageTransition" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisableStageTransition where
        toJSON DisableStageTransition'{..}
          = object
              (catMaybes
                 [Just ("pipelineName" .= _dstPipelineName),
                  Just ("stageName" .= _dstStageName),
                  Just ("transitionType" .= _dstTransitionType),
                  Just ("reason" .= _dstReason)])

instance ToPath DisableStageTransition where
        toPath = const "/"

instance ToQuery DisableStageTransition where
        toQuery = const mempty

-- | /See:/ 'disableStageTransitionResponse' smart constructor.
data DisableStageTransitionResponse =
    DisableStageTransitionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DisableStageTransitionResponse' with the minimum fields required to make a request.
--
disableStageTransitionResponse
    :: DisableStageTransitionResponse
disableStageTransitionResponse = DisableStageTransitionResponse'

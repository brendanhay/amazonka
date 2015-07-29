{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.DisableStageTransition
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Prevents artifacts in a pipeline from transitioning to the next stage in
-- the pipeline.
--
-- <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_DisableStageTransition.html>
module Network.AWS.CodePipeline.DisableStageTransition
    (
    -- * Request
      DisableStageTransition
    -- ** Request constructor
    , disableStageTransition
    -- ** Request lenses
    , dstPipelineName
    , dstStageName
    , dstTransitionType
    , dstReason

    -- * Response
    , DisableStageTransitionResponse
    -- ** Response constructor
    , disableStageTransitionResponse
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a disable stage transition input action.
--
-- /See:/ 'disableStageTransition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dstPipelineName'
--
-- * 'dstStageName'
--
-- * 'dstTransitionType'
--
-- * 'dstReason'
data DisableStageTransition = DisableStageTransition'
    { _dstPipelineName   :: !Text
    , _dstStageName      :: !Text
    , _dstTransitionType :: !StageTransitionType
    , _dstReason         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableStageTransition' smart constructor.
disableStageTransition :: Text -> Text -> StageTransitionType -> Text -> DisableStageTransition
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
        type Sv DisableStageTransition = CodePipeline
        type Rs DisableStageTransition =
             DisableStageTransitionResponse
        request = postJSON
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
              ["pipelineName" .= _dstPipelineName,
               "stageName" .= _dstStageName,
               "transitionType" .= _dstTransitionType,
               "reason" .= _dstReason]

instance ToPath DisableStageTransition where
        toPath = const mempty

instance ToQuery DisableStageTransition where
        toQuery = const mempty

-- | /See:/ 'disableStageTransitionResponse' smart constructor.
data DisableStageTransitionResponse =
    DisableStageTransitionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableStageTransitionResponse' smart constructor.
disableStageTransitionResponse :: DisableStageTransitionResponse
disableStageTransitionResponse = DisableStageTransitionResponse'

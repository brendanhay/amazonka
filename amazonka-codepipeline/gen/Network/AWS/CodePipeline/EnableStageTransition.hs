{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.EnableStageTransition
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Enables artifacts in a pipeline to transition to a stage in a pipeline.
--
-- <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_EnableStageTransition.html>
module Network.AWS.CodePipeline.EnableStageTransition
    (
    -- * Request
      EnableStageTransition
    -- ** Request constructor
    , enableStageTransition
    -- ** Request lenses
    , estPipelineName
    , estStageName
    , estTransitionType

    -- * Response
    , EnableStageTransitionResponse
    -- ** Response constructor
    , enableStageTransitionResponse
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of an enable stage transition action.
--
-- /See:/ 'enableStageTransition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'estPipelineName'
--
-- * 'estStageName'
--
-- * 'estTransitionType'
data EnableStageTransition = EnableStageTransition'
    { _estPipelineName   :: !Text
    , _estStageName      :: !Text
    , _estTransitionType :: !StageTransitionType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableStageTransition' smart constructor.
enableStageTransition :: Text -> Text -> StageTransitionType -> EnableStageTransition
enableStageTransition pPipelineName pStageName pTransitionType =
    EnableStageTransition'
    { _estPipelineName = pPipelineName
    , _estStageName = pStageName
    , _estTransitionType = pTransitionType
    }

-- | The name of the pipeline in which you want to enable the flow of
-- artifacts from one stage to another.
estPipelineName :: Lens' EnableStageTransition Text
estPipelineName = lens _estPipelineName (\ s a -> s{_estPipelineName = a});

-- | The name of the stage where you want to enable the transition of
-- artifacts, either into the stage (inbound) or from that stage to the
-- next stage (outbound).
estStageName :: Lens' EnableStageTransition Text
estStageName = lens _estStageName (\ s a -> s{_estStageName = a});

-- | Specifies whether artifacts will be allowed to enter the stage and be
-- processed by the actions in that stage (inbound) or whether
-- already-processed artifacts will be allowed to transition to the next
-- stage (outbound).
estTransitionType :: Lens' EnableStageTransition StageTransitionType
estTransitionType = lens _estTransitionType (\ s a -> s{_estTransitionType = a});

instance AWSRequest EnableStageTransition where
        type Sv EnableStageTransition = CodePipeline
        type Rs EnableStageTransition =
             EnableStageTransitionResponse
        request = postJSON
        response = receiveNull EnableStageTransitionResponse'

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
              ["pipelineName" .= _estPipelineName,
               "stageName" .= _estStageName,
               "transitionType" .= _estTransitionType]

instance ToPath EnableStageTransition where
        toPath = const "/"

instance ToQuery EnableStageTransition where
        toQuery = const mempty

-- | /See:/ 'enableStageTransitionResponse' smart constructor.
data EnableStageTransitionResponse =
    EnableStageTransitionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableStageTransitionResponse' smart constructor.
enableStageTransitionResponse :: EnableStageTransitionResponse
enableStageTransitionResponse = EnableStageTransitionResponse'

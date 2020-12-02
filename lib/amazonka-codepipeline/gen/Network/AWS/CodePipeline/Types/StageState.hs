{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.StageState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.StageState where

import Network.AWS.CodePipeline.Types.ActionState
import Network.AWS.CodePipeline.Types.StageExecution
import Network.AWS.CodePipeline.Types.TransitionState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about the state of the stage.
--
--
--
-- /See:/ 'stageState' smart constructor.
data StageState = StageState'
  { _ssInboundExecution ::
      !(Maybe StageExecution),
    _ssInboundTransitionState :: !(Maybe TransitionState),
    _ssActionStates :: !(Maybe [ActionState]),
    _ssStageName :: !(Maybe Text),
    _ssLatestExecution :: !(Maybe StageExecution)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StageState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssInboundExecution' - Undocumented member.
--
-- * 'ssInboundTransitionState' - The state of the inbound transition, which is either enabled or disabled.
--
-- * 'ssActionStates' - The state of the stage.
--
-- * 'ssStageName' - The name of the stage.
--
-- * 'ssLatestExecution' - Information about the latest execution in the stage, including its ID and status.
stageState ::
  StageState
stageState =
  StageState'
    { _ssInboundExecution = Nothing,
      _ssInboundTransitionState = Nothing,
      _ssActionStates = Nothing,
      _ssStageName = Nothing,
      _ssLatestExecution = Nothing
    }

-- | Undocumented member.
ssInboundExecution :: Lens' StageState (Maybe StageExecution)
ssInboundExecution = lens _ssInboundExecution (\s a -> s {_ssInboundExecution = a})

-- | The state of the inbound transition, which is either enabled or disabled.
ssInboundTransitionState :: Lens' StageState (Maybe TransitionState)
ssInboundTransitionState = lens _ssInboundTransitionState (\s a -> s {_ssInboundTransitionState = a})

-- | The state of the stage.
ssActionStates :: Lens' StageState [ActionState]
ssActionStates = lens _ssActionStates (\s a -> s {_ssActionStates = a}) . _Default . _Coerce

-- | The name of the stage.
ssStageName :: Lens' StageState (Maybe Text)
ssStageName = lens _ssStageName (\s a -> s {_ssStageName = a})

-- | Information about the latest execution in the stage, including its ID and status.
ssLatestExecution :: Lens' StageState (Maybe StageExecution)
ssLatestExecution = lens _ssLatestExecution (\s a -> s {_ssLatestExecution = a})

instance FromJSON StageState where
  parseJSON =
    withObject
      "StageState"
      ( \x ->
          StageState'
            <$> (x .:? "inboundExecution")
            <*> (x .:? "inboundTransitionState")
            <*> (x .:? "actionStates" .!= mempty)
            <*> (x .:? "stageName")
            <*> (x .:? "latestExecution")
      )

instance Hashable StageState

instance NFData StageState

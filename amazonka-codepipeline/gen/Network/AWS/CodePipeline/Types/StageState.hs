{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.StageState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.StageState where

import Network.AWS.CodePipeline.Types.ActionState
import Network.AWS.CodePipeline.Types.StageExecution
import Network.AWS.CodePipeline.Types.TransitionState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents information about the state of the stage.
--
-- /See:/ 'newStageState' smart constructor.
data StageState = StageState'
  { inboundExecution :: Prelude.Maybe StageExecution,
    -- | Information about the latest execution in the stage, including its ID
    -- and status.
    latestExecution :: Prelude.Maybe StageExecution,
    -- | The name of the stage.
    stageName :: Prelude.Maybe Prelude.Text,
    -- | The state of the inbound transition, which is either enabled or
    -- disabled.
    inboundTransitionState :: Prelude.Maybe TransitionState,
    -- | The state of the stage.
    actionStates :: Prelude.Maybe [ActionState]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StageState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inboundExecution', 'stageState_inboundExecution' - Undocumented member.
--
-- 'latestExecution', 'stageState_latestExecution' - Information about the latest execution in the stage, including its ID
-- and status.
--
-- 'stageName', 'stageState_stageName' - The name of the stage.
--
-- 'inboundTransitionState', 'stageState_inboundTransitionState' - The state of the inbound transition, which is either enabled or
-- disabled.
--
-- 'actionStates', 'stageState_actionStates' - The state of the stage.
newStageState ::
  StageState
newStageState =
  StageState'
    { inboundExecution = Prelude.Nothing,
      latestExecution = Prelude.Nothing,
      stageName = Prelude.Nothing,
      inboundTransitionState = Prelude.Nothing,
      actionStates = Prelude.Nothing
    }

-- | Undocumented member.
stageState_inboundExecution :: Lens.Lens' StageState (Prelude.Maybe StageExecution)
stageState_inboundExecution = Lens.lens (\StageState' {inboundExecution} -> inboundExecution) (\s@StageState' {} a -> s {inboundExecution = a} :: StageState)

-- | Information about the latest execution in the stage, including its ID
-- and status.
stageState_latestExecution :: Lens.Lens' StageState (Prelude.Maybe StageExecution)
stageState_latestExecution = Lens.lens (\StageState' {latestExecution} -> latestExecution) (\s@StageState' {} a -> s {latestExecution = a} :: StageState)

-- | The name of the stage.
stageState_stageName :: Lens.Lens' StageState (Prelude.Maybe Prelude.Text)
stageState_stageName = Lens.lens (\StageState' {stageName} -> stageName) (\s@StageState' {} a -> s {stageName = a} :: StageState)

-- | The state of the inbound transition, which is either enabled or
-- disabled.
stageState_inboundTransitionState :: Lens.Lens' StageState (Prelude.Maybe TransitionState)
stageState_inboundTransitionState = Lens.lens (\StageState' {inboundTransitionState} -> inboundTransitionState) (\s@StageState' {} a -> s {inboundTransitionState = a} :: StageState)

-- | The state of the stage.
stageState_actionStates :: Lens.Lens' StageState (Prelude.Maybe [ActionState])
stageState_actionStates = Lens.lens (\StageState' {actionStates} -> actionStates) (\s@StageState' {} a -> s {actionStates = a} :: StageState) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON StageState where
  parseJSON =
    Prelude.withObject
      "StageState"
      ( \x ->
          StageState'
            Prelude.<$> (x Prelude..:? "inboundExecution")
            Prelude.<*> (x Prelude..:? "latestExecution")
            Prelude.<*> (x Prelude..:? "stageName")
            Prelude.<*> (x Prelude..:? "inboundTransitionState")
            Prelude.<*> ( x Prelude..:? "actionStates"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable StageState

instance Prelude.NFData StageState

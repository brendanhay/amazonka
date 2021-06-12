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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents information about the state of the stage.
--
-- /See:/ 'newStageState' smart constructor.
data StageState = StageState'
  { inboundExecution :: Core.Maybe StageExecution,
    -- | Information about the latest execution in the stage, including its ID
    -- and status.
    latestExecution :: Core.Maybe StageExecution,
    -- | The name of the stage.
    stageName :: Core.Maybe Core.Text,
    -- | The state of the inbound transition, which is either enabled or
    -- disabled.
    inboundTransitionState :: Core.Maybe TransitionState,
    -- | The state of the stage.
    actionStates :: Core.Maybe [ActionState]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { inboundExecution = Core.Nothing,
      latestExecution = Core.Nothing,
      stageName = Core.Nothing,
      inboundTransitionState = Core.Nothing,
      actionStates = Core.Nothing
    }

-- | Undocumented member.
stageState_inboundExecution :: Lens.Lens' StageState (Core.Maybe StageExecution)
stageState_inboundExecution = Lens.lens (\StageState' {inboundExecution} -> inboundExecution) (\s@StageState' {} a -> s {inboundExecution = a} :: StageState)

-- | Information about the latest execution in the stage, including its ID
-- and status.
stageState_latestExecution :: Lens.Lens' StageState (Core.Maybe StageExecution)
stageState_latestExecution = Lens.lens (\StageState' {latestExecution} -> latestExecution) (\s@StageState' {} a -> s {latestExecution = a} :: StageState)

-- | The name of the stage.
stageState_stageName :: Lens.Lens' StageState (Core.Maybe Core.Text)
stageState_stageName = Lens.lens (\StageState' {stageName} -> stageName) (\s@StageState' {} a -> s {stageName = a} :: StageState)

-- | The state of the inbound transition, which is either enabled or
-- disabled.
stageState_inboundTransitionState :: Lens.Lens' StageState (Core.Maybe TransitionState)
stageState_inboundTransitionState = Lens.lens (\StageState' {inboundTransitionState} -> inboundTransitionState) (\s@StageState' {} a -> s {inboundTransitionState = a} :: StageState)

-- | The state of the stage.
stageState_actionStates :: Lens.Lens' StageState (Core.Maybe [ActionState])
stageState_actionStates = Lens.lens (\StageState' {actionStates} -> actionStates) (\s@StageState' {} a -> s {actionStates = a} :: StageState) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON StageState where
  parseJSON =
    Core.withObject
      "StageState"
      ( \x ->
          StageState'
            Core.<$> (x Core..:? "inboundExecution")
            Core.<*> (x Core..:? "latestExecution")
            Core.<*> (x Core..:? "stageName")
            Core.<*> (x Core..:? "inboundTransitionState")
            Core.<*> (x Core..:? "actionStates" Core..!= Core.mempty)
      )

instance Core.Hashable StageState

instance Core.NFData StageState

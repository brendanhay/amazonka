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
-- Module      : Amazonka.CodePipeline.Types.StageState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.StageState where

import Amazonka.CodePipeline.Types.ActionState
import Amazonka.CodePipeline.Types.StageExecution
import Amazonka.CodePipeline.Types.TransitionState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about the state of the stage.
--
-- /See:/ 'newStageState' smart constructor.
data StageState = StageState'
  { -- | The state of the stage.
    actionStates :: Prelude.Maybe [ActionState],
    inboundExecution :: Prelude.Maybe StageExecution,
    -- | The state of the inbound transition, which is either enabled or
    -- disabled.
    inboundTransitionState :: Prelude.Maybe TransitionState,
    -- | Information about the latest execution in the stage, including its ID
    -- and status.
    latestExecution :: Prelude.Maybe StageExecution,
    -- | The name of the stage.
    stageName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StageState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionStates', 'stageState_actionStates' - The state of the stage.
--
-- 'inboundExecution', 'stageState_inboundExecution' - Undocumented member.
--
-- 'inboundTransitionState', 'stageState_inboundTransitionState' - The state of the inbound transition, which is either enabled or
-- disabled.
--
-- 'latestExecution', 'stageState_latestExecution' - Information about the latest execution in the stage, including its ID
-- and status.
--
-- 'stageName', 'stageState_stageName' - The name of the stage.
newStageState ::
  StageState
newStageState =
  StageState'
    { actionStates = Prelude.Nothing,
      inboundExecution = Prelude.Nothing,
      inboundTransitionState = Prelude.Nothing,
      latestExecution = Prelude.Nothing,
      stageName = Prelude.Nothing
    }

-- | The state of the stage.
stageState_actionStates :: Lens.Lens' StageState (Prelude.Maybe [ActionState])
stageState_actionStates = Lens.lens (\StageState' {actionStates} -> actionStates) (\s@StageState' {} a -> s {actionStates = a} :: StageState) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
stageState_inboundExecution :: Lens.Lens' StageState (Prelude.Maybe StageExecution)
stageState_inboundExecution = Lens.lens (\StageState' {inboundExecution} -> inboundExecution) (\s@StageState' {} a -> s {inboundExecution = a} :: StageState)

-- | The state of the inbound transition, which is either enabled or
-- disabled.
stageState_inboundTransitionState :: Lens.Lens' StageState (Prelude.Maybe TransitionState)
stageState_inboundTransitionState = Lens.lens (\StageState' {inboundTransitionState} -> inboundTransitionState) (\s@StageState' {} a -> s {inboundTransitionState = a} :: StageState)

-- | Information about the latest execution in the stage, including its ID
-- and status.
stageState_latestExecution :: Lens.Lens' StageState (Prelude.Maybe StageExecution)
stageState_latestExecution = Lens.lens (\StageState' {latestExecution} -> latestExecution) (\s@StageState' {} a -> s {latestExecution = a} :: StageState)

-- | The name of the stage.
stageState_stageName :: Lens.Lens' StageState (Prelude.Maybe Prelude.Text)
stageState_stageName = Lens.lens (\StageState' {stageName} -> stageName) (\s@StageState' {} a -> s {stageName = a} :: StageState)

instance Data.FromJSON StageState where
  parseJSON =
    Data.withObject
      "StageState"
      ( \x ->
          StageState'
            Prelude.<$> (x Data..:? "actionStates" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "inboundExecution")
            Prelude.<*> (x Data..:? "inboundTransitionState")
            Prelude.<*> (x Data..:? "latestExecution")
            Prelude.<*> (x Data..:? "stageName")
      )

instance Prelude.Hashable StageState where
  hashWithSalt _salt StageState' {..} =
    _salt `Prelude.hashWithSalt` actionStates
      `Prelude.hashWithSalt` inboundExecution
      `Prelude.hashWithSalt` inboundTransitionState
      `Prelude.hashWithSalt` latestExecution
      `Prelude.hashWithSalt` stageName

instance Prelude.NFData StageState where
  rnf StageState' {..} =
    Prelude.rnf actionStates
      `Prelude.seq` Prelude.rnf inboundExecution
      `Prelude.seq` Prelude.rnf inboundTransitionState
      `Prelude.seq` Prelude.rnf latestExecution
      `Prelude.seq` Prelude.rnf stageName

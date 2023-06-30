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
-- Module      : Amazonka.FIS.Types.ExperimentAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types.ExperimentActionState
import qualified Amazonka.Prelude as Prelude

-- | Describes the action for an experiment.
--
-- /See:/ 'newExperimentAction' smart constructor.
data ExperimentAction = ExperimentAction'
  { -- | The ID of the action.
    actionId :: Prelude.Maybe Prelude.Text,
    -- | The description for the action.
    description :: Prelude.Maybe Prelude.Text,
    -- | The time that the action ended.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The parameters for the action.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the action that must be completed before this action starts.
    startAfter :: Prelude.Maybe [Prelude.Text],
    -- | The time that the action started.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The state of the action.
    state :: Prelude.Maybe ExperimentActionState,
    -- | The targets for the action.
    targets :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionId', 'experimentAction_actionId' - The ID of the action.
--
-- 'description', 'experimentAction_description' - The description for the action.
--
-- 'endTime', 'experimentAction_endTime' - The time that the action ended.
--
-- 'parameters', 'experimentAction_parameters' - The parameters for the action.
--
-- 'startAfter', 'experimentAction_startAfter' - The name of the action that must be completed before this action starts.
--
-- 'startTime', 'experimentAction_startTime' - The time that the action started.
--
-- 'state', 'experimentAction_state' - The state of the action.
--
-- 'targets', 'experimentAction_targets' - The targets for the action.
newExperimentAction ::
  ExperimentAction
newExperimentAction =
  ExperimentAction'
    { actionId = Prelude.Nothing,
      description = Prelude.Nothing,
      endTime = Prelude.Nothing,
      parameters = Prelude.Nothing,
      startAfter = Prelude.Nothing,
      startTime = Prelude.Nothing,
      state = Prelude.Nothing,
      targets = Prelude.Nothing
    }

-- | The ID of the action.
experimentAction_actionId :: Lens.Lens' ExperimentAction (Prelude.Maybe Prelude.Text)
experimentAction_actionId = Lens.lens (\ExperimentAction' {actionId} -> actionId) (\s@ExperimentAction' {} a -> s {actionId = a} :: ExperimentAction)

-- | The description for the action.
experimentAction_description :: Lens.Lens' ExperimentAction (Prelude.Maybe Prelude.Text)
experimentAction_description = Lens.lens (\ExperimentAction' {description} -> description) (\s@ExperimentAction' {} a -> s {description = a} :: ExperimentAction)

-- | The time that the action ended.
experimentAction_endTime :: Lens.Lens' ExperimentAction (Prelude.Maybe Prelude.UTCTime)
experimentAction_endTime = Lens.lens (\ExperimentAction' {endTime} -> endTime) (\s@ExperimentAction' {} a -> s {endTime = a} :: ExperimentAction) Prelude.. Lens.mapping Data._Time

-- | The parameters for the action.
experimentAction_parameters :: Lens.Lens' ExperimentAction (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
experimentAction_parameters = Lens.lens (\ExperimentAction' {parameters} -> parameters) (\s@ExperimentAction' {} a -> s {parameters = a} :: ExperimentAction) Prelude.. Lens.mapping Lens.coerced

-- | The name of the action that must be completed before this action starts.
experimentAction_startAfter :: Lens.Lens' ExperimentAction (Prelude.Maybe [Prelude.Text])
experimentAction_startAfter = Lens.lens (\ExperimentAction' {startAfter} -> startAfter) (\s@ExperimentAction' {} a -> s {startAfter = a} :: ExperimentAction) Prelude.. Lens.mapping Lens.coerced

-- | The time that the action started.
experimentAction_startTime :: Lens.Lens' ExperimentAction (Prelude.Maybe Prelude.UTCTime)
experimentAction_startTime = Lens.lens (\ExperimentAction' {startTime} -> startTime) (\s@ExperimentAction' {} a -> s {startTime = a} :: ExperimentAction) Prelude.. Lens.mapping Data._Time

-- | The state of the action.
experimentAction_state :: Lens.Lens' ExperimentAction (Prelude.Maybe ExperimentActionState)
experimentAction_state = Lens.lens (\ExperimentAction' {state} -> state) (\s@ExperimentAction' {} a -> s {state = a} :: ExperimentAction)

-- | The targets for the action.
experimentAction_targets :: Lens.Lens' ExperimentAction (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
experimentAction_targets = Lens.lens (\ExperimentAction' {targets} -> targets) (\s@ExperimentAction' {} a -> s {targets = a} :: ExperimentAction) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ExperimentAction where
  parseJSON =
    Data.withObject
      "ExperimentAction"
      ( \x ->
          ExperimentAction'
            Prelude.<$> (x Data..:? "actionId")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "startAfter" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "targets" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ExperimentAction where
  hashWithSalt _salt ExperimentAction' {..} =
    _salt
      `Prelude.hashWithSalt` actionId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` startAfter
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` targets

instance Prelude.NFData ExperimentAction where
  rnf ExperimentAction' {..} =
    Prelude.rnf actionId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf startAfter
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf targets

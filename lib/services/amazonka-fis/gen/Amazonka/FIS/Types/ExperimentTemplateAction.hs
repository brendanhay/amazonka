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
-- Module      : Amazonka.FIS.Types.ExperimentTemplateAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentTemplateAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an action for an experiment template.
--
-- /See:/ 'newExperimentTemplateAction' smart constructor.
data ExperimentTemplateAction = ExperimentTemplateAction'
  { -- | The name of the action that must be completed before the current action
    -- starts.
    startAfter :: Prelude.Maybe [Prelude.Text],
    -- | The targets for the action.
    targets :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A description for the action.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the action.
    actionId :: Prelude.Maybe Prelude.Text,
    -- | The parameters for the action.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentTemplateAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startAfter', 'experimentTemplateAction_startAfter' - The name of the action that must be completed before the current action
-- starts.
--
-- 'targets', 'experimentTemplateAction_targets' - The targets for the action.
--
-- 'description', 'experimentTemplateAction_description' - A description for the action.
--
-- 'actionId', 'experimentTemplateAction_actionId' - The ID of the action.
--
-- 'parameters', 'experimentTemplateAction_parameters' - The parameters for the action.
newExperimentTemplateAction ::
  ExperimentTemplateAction
newExperimentTemplateAction =
  ExperimentTemplateAction'
    { startAfter =
        Prelude.Nothing,
      targets = Prelude.Nothing,
      description = Prelude.Nothing,
      actionId = Prelude.Nothing,
      parameters = Prelude.Nothing
    }

-- | The name of the action that must be completed before the current action
-- starts.
experimentTemplateAction_startAfter :: Lens.Lens' ExperimentTemplateAction (Prelude.Maybe [Prelude.Text])
experimentTemplateAction_startAfter = Lens.lens (\ExperimentTemplateAction' {startAfter} -> startAfter) (\s@ExperimentTemplateAction' {} a -> s {startAfter = a} :: ExperimentTemplateAction) Prelude.. Lens.mapping Lens.coerced

-- | The targets for the action.
experimentTemplateAction_targets :: Lens.Lens' ExperimentTemplateAction (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
experimentTemplateAction_targets = Lens.lens (\ExperimentTemplateAction' {targets} -> targets) (\s@ExperimentTemplateAction' {} a -> s {targets = a} :: ExperimentTemplateAction) Prelude.. Lens.mapping Lens.coerced

-- | A description for the action.
experimentTemplateAction_description :: Lens.Lens' ExperimentTemplateAction (Prelude.Maybe Prelude.Text)
experimentTemplateAction_description = Lens.lens (\ExperimentTemplateAction' {description} -> description) (\s@ExperimentTemplateAction' {} a -> s {description = a} :: ExperimentTemplateAction)

-- | The ID of the action.
experimentTemplateAction_actionId :: Lens.Lens' ExperimentTemplateAction (Prelude.Maybe Prelude.Text)
experimentTemplateAction_actionId = Lens.lens (\ExperimentTemplateAction' {actionId} -> actionId) (\s@ExperimentTemplateAction' {} a -> s {actionId = a} :: ExperimentTemplateAction)

-- | The parameters for the action.
experimentTemplateAction_parameters :: Lens.Lens' ExperimentTemplateAction (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
experimentTemplateAction_parameters = Lens.lens (\ExperimentTemplateAction' {parameters} -> parameters) (\s@ExperimentTemplateAction' {} a -> s {parameters = a} :: ExperimentTemplateAction) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ExperimentTemplateAction where
  parseJSON =
    Core.withObject
      "ExperimentTemplateAction"
      ( \x ->
          ExperimentTemplateAction'
            Prelude.<$> (x Core..:? "startAfter" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "targets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "actionId")
            Prelude.<*> (x Core..:? "parameters" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ExperimentTemplateAction where
  hashWithSalt _salt ExperimentTemplateAction' {..} =
    _salt `Prelude.hashWithSalt` startAfter
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` actionId
      `Prelude.hashWithSalt` parameters

instance Prelude.NFData ExperimentTemplateAction where
  rnf ExperimentTemplateAction' {..} =
    Prelude.rnf startAfter
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf actionId
      `Prelude.seq` Prelude.rnf parameters

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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an action for an experiment template.
--
-- /See:/ 'newExperimentTemplateAction' smart constructor.
data ExperimentTemplateAction = ExperimentTemplateAction'
  { -- | The ID of the action.
    actionId :: Prelude.Maybe Prelude.Text,
    -- | A description for the action.
    description :: Prelude.Maybe Prelude.Text,
    -- | The parameters for the action.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the action that must be completed before the current action
    -- starts.
    startAfter :: Prelude.Maybe [Prelude.Text],
    -- | The targets for the action.
    targets :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'actionId', 'experimentTemplateAction_actionId' - The ID of the action.
--
-- 'description', 'experimentTemplateAction_description' - A description for the action.
--
-- 'parameters', 'experimentTemplateAction_parameters' - The parameters for the action.
--
-- 'startAfter', 'experimentTemplateAction_startAfter' - The name of the action that must be completed before the current action
-- starts.
--
-- 'targets', 'experimentTemplateAction_targets' - The targets for the action.
newExperimentTemplateAction ::
  ExperimentTemplateAction
newExperimentTemplateAction =
  ExperimentTemplateAction'
    { actionId =
        Prelude.Nothing,
      description = Prelude.Nothing,
      parameters = Prelude.Nothing,
      startAfter = Prelude.Nothing,
      targets = Prelude.Nothing
    }

-- | The ID of the action.
experimentTemplateAction_actionId :: Lens.Lens' ExperimentTemplateAction (Prelude.Maybe Prelude.Text)
experimentTemplateAction_actionId = Lens.lens (\ExperimentTemplateAction' {actionId} -> actionId) (\s@ExperimentTemplateAction' {} a -> s {actionId = a} :: ExperimentTemplateAction)

-- | A description for the action.
experimentTemplateAction_description :: Lens.Lens' ExperimentTemplateAction (Prelude.Maybe Prelude.Text)
experimentTemplateAction_description = Lens.lens (\ExperimentTemplateAction' {description} -> description) (\s@ExperimentTemplateAction' {} a -> s {description = a} :: ExperimentTemplateAction)

-- | The parameters for the action.
experimentTemplateAction_parameters :: Lens.Lens' ExperimentTemplateAction (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
experimentTemplateAction_parameters = Lens.lens (\ExperimentTemplateAction' {parameters} -> parameters) (\s@ExperimentTemplateAction' {} a -> s {parameters = a} :: ExperimentTemplateAction) Prelude.. Lens.mapping Lens.coerced

-- | The name of the action that must be completed before the current action
-- starts.
experimentTemplateAction_startAfter :: Lens.Lens' ExperimentTemplateAction (Prelude.Maybe [Prelude.Text])
experimentTemplateAction_startAfter = Lens.lens (\ExperimentTemplateAction' {startAfter} -> startAfter) (\s@ExperimentTemplateAction' {} a -> s {startAfter = a} :: ExperimentTemplateAction) Prelude.. Lens.mapping Lens.coerced

-- | The targets for the action.
experimentTemplateAction_targets :: Lens.Lens' ExperimentTemplateAction (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
experimentTemplateAction_targets = Lens.lens (\ExperimentTemplateAction' {targets} -> targets) (\s@ExperimentTemplateAction' {} a -> s {targets = a} :: ExperimentTemplateAction) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ExperimentTemplateAction where
  parseJSON =
    Data.withObject
      "ExperimentTemplateAction"
      ( \x ->
          ExperimentTemplateAction'
            Prelude.<$> (x Data..:? "actionId")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "startAfter" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "targets" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ExperimentTemplateAction where
  hashWithSalt _salt ExperimentTemplateAction' {..} =
    _salt `Prelude.hashWithSalt` actionId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` startAfter
      `Prelude.hashWithSalt` targets

instance Prelude.NFData ExperimentTemplateAction where
  rnf ExperimentTemplateAction' {..} =
    Prelude.rnf actionId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf startAfter
      `Prelude.seq` Prelude.rnf targets

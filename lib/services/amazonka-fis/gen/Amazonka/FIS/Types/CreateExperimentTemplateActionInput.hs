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
-- Module      : Amazonka.FIS.Types.CreateExperimentTemplateActionInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.CreateExperimentTemplateActionInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies an action for an experiment template.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fis/latest/userguide/actions.html Actions>
-- in the /Fault Injection Simulator User Guide/.
--
-- /See:/ 'newCreateExperimentTemplateActionInput' smart constructor.
data CreateExperimentTemplateActionInput = CreateExperimentTemplateActionInput'
  { -- | A description for the action.
    description :: Prelude.Maybe Prelude.Text,
    -- | The parameters for the action, if applicable.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the action that must be completed before the current action
    -- starts. Omit this parameter to run the action at the start of the
    -- experiment.
    startAfter :: Prelude.Maybe [Prelude.Text],
    -- | The targets for the action.
    targets :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the action. The format of the action ID is:
    -- aws:/service-name/:/action-type/.
    actionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateExperimentTemplateActionInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createExperimentTemplateActionInput_description' - A description for the action.
--
-- 'parameters', 'createExperimentTemplateActionInput_parameters' - The parameters for the action, if applicable.
--
-- 'startAfter', 'createExperimentTemplateActionInput_startAfter' - The name of the action that must be completed before the current action
-- starts. Omit this parameter to run the action at the start of the
-- experiment.
--
-- 'targets', 'createExperimentTemplateActionInput_targets' - The targets for the action.
--
-- 'actionId', 'createExperimentTemplateActionInput_actionId' - The ID of the action. The format of the action ID is:
-- aws:/service-name/:/action-type/.
newCreateExperimentTemplateActionInput ::
  -- | 'actionId'
  Prelude.Text ->
  CreateExperimentTemplateActionInput
newCreateExperimentTemplateActionInput pActionId_ =
  CreateExperimentTemplateActionInput'
    { description =
        Prelude.Nothing,
      parameters = Prelude.Nothing,
      startAfter = Prelude.Nothing,
      targets = Prelude.Nothing,
      actionId = pActionId_
    }

-- | A description for the action.
createExperimentTemplateActionInput_description :: Lens.Lens' CreateExperimentTemplateActionInput (Prelude.Maybe Prelude.Text)
createExperimentTemplateActionInput_description = Lens.lens (\CreateExperimentTemplateActionInput' {description} -> description) (\s@CreateExperimentTemplateActionInput' {} a -> s {description = a} :: CreateExperimentTemplateActionInput)

-- | The parameters for the action, if applicable.
createExperimentTemplateActionInput_parameters :: Lens.Lens' CreateExperimentTemplateActionInput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createExperimentTemplateActionInput_parameters = Lens.lens (\CreateExperimentTemplateActionInput' {parameters} -> parameters) (\s@CreateExperimentTemplateActionInput' {} a -> s {parameters = a} :: CreateExperimentTemplateActionInput) Prelude.. Lens.mapping Lens.coerced

-- | The name of the action that must be completed before the current action
-- starts. Omit this parameter to run the action at the start of the
-- experiment.
createExperimentTemplateActionInput_startAfter :: Lens.Lens' CreateExperimentTemplateActionInput (Prelude.Maybe [Prelude.Text])
createExperimentTemplateActionInput_startAfter = Lens.lens (\CreateExperimentTemplateActionInput' {startAfter} -> startAfter) (\s@CreateExperimentTemplateActionInput' {} a -> s {startAfter = a} :: CreateExperimentTemplateActionInput) Prelude.. Lens.mapping Lens.coerced

-- | The targets for the action.
createExperimentTemplateActionInput_targets :: Lens.Lens' CreateExperimentTemplateActionInput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createExperimentTemplateActionInput_targets = Lens.lens (\CreateExperimentTemplateActionInput' {targets} -> targets) (\s@CreateExperimentTemplateActionInput' {} a -> s {targets = a} :: CreateExperimentTemplateActionInput) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the action. The format of the action ID is:
-- aws:/service-name/:/action-type/.
createExperimentTemplateActionInput_actionId :: Lens.Lens' CreateExperimentTemplateActionInput Prelude.Text
createExperimentTemplateActionInput_actionId = Lens.lens (\CreateExperimentTemplateActionInput' {actionId} -> actionId) (\s@CreateExperimentTemplateActionInput' {} a -> s {actionId = a} :: CreateExperimentTemplateActionInput)

instance
  Prelude.Hashable
    CreateExperimentTemplateActionInput
  where
  hashWithSalt
    _salt
    CreateExperimentTemplateActionInput' {..} =
      _salt `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` parameters
        `Prelude.hashWithSalt` startAfter
        `Prelude.hashWithSalt` targets
        `Prelude.hashWithSalt` actionId

instance
  Prelude.NFData
    CreateExperimentTemplateActionInput
  where
  rnf CreateExperimentTemplateActionInput' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf startAfter
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf actionId

instance
  Data.ToJSON
    CreateExperimentTemplateActionInput
  where
  toJSON CreateExperimentTemplateActionInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("parameters" Data..=) Prelude.<$> parameters,
            ("startAfter" Data..=) Prelude.<$> startAfter,
            ("targets" Data..=) Prelude.<$> targets,
            Prelude.Just ("actionId" Data..= actionId)
          ]
      )

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
-- Module      : Amazonka.MigrationHubOrchestrator.Types.WorkflowStepOutput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Types.WorkflowStepOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types.DataType
import Amazonka.MigrationHubOrchestrator.Types.WorkflowStepOutputUnion
import qualified Amazonka.Prelude as Prelude

-- | The output of a step.
--
-- /See:/ 'newWorkflowStepOutput' smart constructor.
data WorkflowStepOutput = WorkflowStepOutput'
  { -- | The data type of the output.
    dataType :: Prelude.Maybe DataType,
    -- | The name of the step.
    name :: Prelude.Maybe Prelude.Text,
    -- | Determine if an output is required from a step.
    required :: Prelude.Maybe Prelude.Bool,
    -- | The value of the output.
    value :: Prelude.Maybe WorkflowStepOutputUnion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowStepOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataType', 'workflowStepOutput_dataType' - The data type of the output.
--
-- 'name', 'workflowStepOutput_name' - The name of the step.
--
-- 'required', 'workflowStepOutput_required' - Determine if an output is required from a step.
--
-- 'value', 'workflowStepOutput_value' - The value of the output.
newWorkflowStepOutput ::
  WorkflowStepOutput
newWorkflowStepOutput =
  WorkflowStepOutput'
    { dataType = Prelude.Nothing,
      name = Prelude.Nothing,
      required = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The data type of the output.
workflowStepOutput_dataType :: Lens.Lens' WorkflowStepOutput (Prelude.Maybe DataType)
workflowStepOutput_dataType = Lens.lens (\WorkflowStepOutput' {dataType} -> dataType) (\s@WorkflowStepOutput' {} a -> s {dataType = a} :: WorkflowStepOutput)

-- | The name of the step.
workflowStepOutput_name :: Lens.Lens' WorkflowStepOutput (Prelude.Maybe Prelude.Text)
workflowStepOutput_name = Lens.lens (\WorkflowStepOutput' {name} -> name) (\s@WorkflowStepOutput' {} a -> s {name = a} :: WorkflowStepOutput)

-- | Determine if an output is required from a step.
workflowStepOutput_required :: Lens.Lens' WorkflowStepOutput (Prelude.Maybe Prelude.Bool)
workflowStepOutput_required = Lens.lens (\WorkflowStepOutput' {required} -> required) (\s@WorkflowStepOutput' {} a -> s {required = a} :: WorkflowStepOutput)

-- | The value of the output.
workflowStepOutput_value :: Lens.Lens' WorkflowStepOutput (Prelude.Maybe WorkflowStepOutputUnion)
workflowStepOutput_value = Lens.lens (\WorkflowStepOutput' {value} -> value) (\s@WorkflowStepOutput' {} a -> s {value = a} :: WorkflowStepOutput)

instance Data.FromJSON WorkflowStepOutput where
  parseJSON =
    Data.withObject
      "WorkflowStepOutput"
      ( \x ->
          WorkflowStepOutput'
            Prelude.<$> (x Data..:? "dataType")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "required")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable WorkflowStepOutput where
  hashWithSalt _salt WorkflowStepOutput' {..} =
    _salt `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` required
      `Prelude.hashWithSalt` value

instance Prelude.NFData WorkflowStepOutput where
  rnf WorkflowStepOutput' {..} =
    Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf required
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON WorkflowStepOutput where
  toJSON WorkflowStepOutput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dataType" Data..=) Prelude.<$> dataType,
            ("name" Data..=) Prelude.<$> name,
            ("required" Data..=) Prelude.<$> required,
            ("value" Data..=) Prelude.<$> value
          ]
      )

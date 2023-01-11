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
-- Module      : Amazonka.MigrationHubOrchestrator.Types.StepOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Types.StepOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types.DataType
import qualified Amazonka.Prelude as Prelude

-- | The output of the step.
--
-- /See:/ 'newStepOutput' smart constructor.
data StepOutput = StepOutput'
  { -- | The data type of the step output.
    dataType :: Prelude.Maybe DataType,
    -- | The name of the step.
    name :: Prelude.Maybe Prelude.Text,
    -- | Determine if an output is required from a step.
    required :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StepOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataType', 'stepOutput_dataType' - The data type of the step output.
--
-- 'name', 'stepOutput_name' - The name of the step.
--
-- 'required', 'stepOutput_required' - Determine if an output is required from a step.
newStepOutput ::
  StepOutput
newStepOutput =
  StepOutput'
    { dataType = Prelude.Nothing,
      name = Prelude.Nothing,
      required = Prelude.Nothing
    }

-- | The data type of the step output.
stepOutput_dataType :: Lens.Lens' StepOutput (Prelude.Maybe DataType)
stepOutput_dataType = Lens.lens (\StepOutput' {dataType} -> dataType) (\s@StepOutput' {} a -> s {dataType = a} :: StepOutput)

-- | The name of the step.
stepOutput_name :: Lens.Lens' StepOutput (Prelude.Maybe Prelude.Text)
stepOutput_name = Lens.lens (\StepOutput' {name} -> name) (\s@StepOutput' {} a -> s {name = a} :: StepOutput)

-- | Determine if an output is required from a step.
stepOutput_required :: Lens.Lens' StepOutput (Prelude.Maybe Prelude.Bool)
stepOutput_required = Lens.lens (\StepOutput' {required} -> required) (\s@StepOutput' {} a -> s {required = a} :: StepOutput)

instance Data.FromJSON StepOutput where
  parseJSON =
    Data.withObject
      "StepOutput"
      ( \x ->
          StepOutput'
            Prelude.<$> (x Data..:? "dataType")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "required")
      )

instance Prelude.Hashable StepOutput where
  hashWithSalt _salt StepOutput' {..} =
    _salt `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` required

instance Prelude.NFData StepOutput where
  rnf StepOutput' {..} =
    Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf required

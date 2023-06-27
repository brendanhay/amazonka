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
-- Module      : Amazonka.Transfer.Types.ExecutionStepResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.ExecutionStepResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.ExecutionError
import Amazonka.Transfer.Types.WorkflowStepType

-- | Specifies the following details for the step: error (if any), outputs
-- (if any), and the step type.
--
-- /See:/ 'newExecutionStepResult' smart constructor.
data ExecutionStepResult = ExecutionStepResult'
  { -- | Specifies the details for an error, if it occurred during execution of
    -- the specified workflow step.
    error :: Prelude.Maybe ExecutionError,
    -- | The values for the key\/value pair applied as a tag to the file. Only
    -- applicable if the step type is @TAG@.
    outputs :: Prelude.Maybe Prelude.Text,
    -- | One of the available step types.
    --
    -- -   __@COPY@__ - Copy the file to another location.
    --
    -- -   __@CUSTOM@__ - Perform a custom step with an Lambda function target.
    --
    -- -   __@DECRYPT@__ - Decrypt a file that was encrypted before it was
    --     uploaded.
    --
    -- -   __@DELETE@__ - Delete the file.
    --
    -- -   __@TAG@__ - Add a tag to the file.
    stepType :: Prelude.Maybe WorkflowStepType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutionStepResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'executionStepResult_error' - Specifies the details for an error, if it occurred during execution of
-- the specified workflow step.
--
-- 'outputs', 'executionStepResult_outputs' - The values for the key\/value pair applied as a tag to the file. Only
-- applicable if the step type is @TAG@.
--
-- 'stepType', 'executionStepResult_stepType' - One of the available step types.
--
-- -   __@COPY@__ - Copy the file to another location.
--
-- -   __@CUSTOM@__ - Perform a custom step with an Lambda function target.
--
-- -   __@DECRYPT@__ - Decrypt a file that was encrypted before it was
--     uploaded.
--
-- -   __@DELETE@__ - Delete the file.
--
-- -   __@TAG@__ - Add a tag to the file.
newExecutionStepResult ::
  ExecutionStepResult
newExecutionStepResult =
  ExecutionStepResult'
    { error = Prelude.Nothing,
      outputs = Prelude.Nothing,
      stepType = Prelude.Nothing
    }

-- | Specifies the details for an error, if it occurred during execution of
-- the specified workflow step.
executionStepResult_error :: Lens.Lens' ExecutionStepResult (Prelude.Maybe ExecutionError)
executionStepResult_error = Lens.lens (\ExecutionStepResult' {error} -> error) (\s@ExecutionStepResult' {} a -> s {error = a} :: ExecutionStepResult)

-- | The values for the key\/value pair applied as a tag to the file. Only
-- applicable if the step type is @TAG@.
executionStepResult_outputs :: Lens.Lens' ExecutionStepResult (Prelude.Maybe Prelude.Text)
executionStepResult_outputs = Lens.lens (\ExecutionStepResult' {outputs} -> outputs) (\s@ExecutionStepResult' {} a -> s {outputs = a} :: ExecutionStepResult)

-- | One of the available step types.
--
-- -   __@COPY@__ - Copy the file to another location.
--
-- -   __@CUSTOM@__ - Perform a custom step with an Lambda function target.
--
-- -   __@DECRYPT@__ - Decrypt a file that was encrypted before it was
--     uploaded.
--
-- -   __@DELETE@__ - Delete the file.
--
-- -   __@TAG@__ - Add a tag to the file.
executionStepResult_stepType :: Lens.Lens' ExecutionStepResult (Prelude.Maybe WorkflowStepType)
executionStepResult_stepType = Lens.lens (\ExecutionStepResult' {stepType} -> stepType) (\s@ExecutionStepResult' {} a -> s {stepType = a} :: ExecutionStepResult)

instance Data.FromJSON ExecutionStepResult where
  parseJSON =
    Data.withObject
      "ExecutionStepResult"
      ( \x ->
          ExecutionStepResult'
            Prelude.<$> (x Data..:? "Error")
            Prelude.<*> (x Data..:? "Outputs")
            Prelude.<*> (x Data..:? "StepType")
      )

instance Prelude.Hashable ExecutionStepResult where
  hashWithSalt _salt ExecutionStepResult' {..} =
    _salt
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` stepType

instance Prelude.NFData ExecutionStepResult where
  rnf ExecutionStepResult' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf stepType

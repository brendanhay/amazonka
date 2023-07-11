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
-- Module      : Amazonka.CodePipeline.Types.ActionExecutionOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionExecutionOutput where

import Amazonka.CodePipeline.Types.ActionExecutionResult
import Amazonka.CodePipeline.Types.ArtifactDetail
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Output details listed for an action execution, such as the action
-- execution result.
--
-- /See:/ 'newActionExecutionOutput' smart constructor.
data ActionExecutionOutput = ActionExecutionOutput'
  { -- | Execution result information listed in the output details for an action
    -- execution.
    executionResult :: Prelude.Maybe ActionExecutionResult,
    -- | Details of output artifacts of the action that correspond to the action
    -- execution.
    outputArtifacts :: Prelude.Maybe [ArtifactDetail],
    -- | The outputVariables field shows the key-value pairs that were output as
    -- part of that execution.
    outputVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionExecutionOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionResult', 'actionExecutionOutput_executionResult' - Execution result information listed in the output details for an action
-- execution.
--
-- 'outputArtifacts', 'actionExecutionOutput_outputArtifacts' - Details of output artifacts of the action that correspond to the action
-- execution.
--
-- 'outputVariables', 'actionExecutionOutput_outputVariables' - The outputVariables field shows the key-value pairs that were output as
-- part of that execution.
newActionExecutionOutput ::
  ActionExecutionOutput
newActionExecutionOutput =
  ActionExecutionOutput'
    { executionResult =
        Prelude.Nothing,
      outputArtifacts = Prelude.Nothing,
      outputVariables = Prelude.Nothing
    }

-- | Execution result information listed in the output details for an action
-- execution.
actionExecutionOutput_executionResult :: Lens.Lens' ActionExecutionOutput (Prelude.Maybe ActionExecutionResult)
actionExecutionOutput_executionResult = Lens.lens (\ActionExecutionOutput' {executionResult} -> executionResult) (\s@ActionExecutionOutput' {} a -> s {executionResult = a} :: ActionExecutionOutput)

-- | Details of output artifacts of the action that correspond to the action
-- execution.
actionExecutionOutput_outputArtifacts :: Lens.Lens' ActionExecutionOutput (Prelude.Maybe [ArtifactDetail])
actionExecutionOutput_outputArtifacts = Lens.lens (\ActionExecutionOutput' {outputArtifacts} -> outputArtifacts) (\s@ActionExecutionOutput' {} a -> s {outputArtifacts = a} :: ActionExecutionOutput) Prelude.. Lens.mapping Lens.coerced

-- | The outputVariables field shows the key-value pairs that were output as
-- part of that execution.
actionExecutionOutput_outputVariables :: Lens.Lens' ActionExecutionOutput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
actionExecutionOutput_outputVariables = Lens.lens (\ActionExecutionOutput' {outputVariables} -> outputVariables) (\s@ActionExecutionOutput' {} a -> s {outputVariables = a} :: ActionExecutionOutput) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ActionExecutionOutput where
  parseJSON =
    Data.withObject
      "ActionExecutionOutput"
      ( \x ->
          ActionExecutionOutput'
            Prelude.<$> (x Data..:? "executionResult")
            Prelude.<*> ( x
                            Data..:? "outputArtifacts"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "outputVariables"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ActionExecutionOutput where
  hashWithSalt _salt ActionExecutionOutput' {..} =
    _salt
      `Prelude.hashWithSalt` executionResult
      `Prelude.hashWithSalt` outputArtifacts
      `Prelude.hashWithSalt` outputVariables

instance Prelude.NFData ActionExecutionOutput where
  rnf ActionExecutionOutput' {..} =
    Prelude.rnf executionResult
      `Prelude.seq` Prelude.rnf outputArtifacts
      `Prelude.seq` Prelude.rnf outputVariables

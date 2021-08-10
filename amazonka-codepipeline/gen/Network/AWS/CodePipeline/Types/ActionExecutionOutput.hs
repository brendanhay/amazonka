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
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecutionOutput where

import Network.AWS.CodePipeline.Types.ActionExecutionResult
import Network.AWS.CodePipeline.Types.ArtifactDetail
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Output details listed for an action execution, such as the action
-- execution result.
--
-- /See:/ 'newActionExecutionOutput' smart constructor.
data ActionExecutionOutput = ActionExecutionOutput'
  { -- | Execution result information listed in the output details for an action
    -- execution.
    executionResult :: Prelude.Maybe ActionExecutionResult,
    -- | The outputVariables field shows the key-value pairs that were output as
    -- part of that execution.
    outputVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Details of output artifacts of the action that correspond to the action
    -- execution.
    outputArtifacts :: Prelude.Maybe [ArtifactDetail]
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
-- 'outputVariables', 'actionExecutionOutput_outputVariables' - The outputVariables field shows the key-value pairs that were output as
-- part of that execution.
--
-- 'outputArtifacts', 'actionExecutionOutput_outputArtifacts' - Details of output artifacts of the action that correspond to the action
-- execution.
newActionExecutionOutput ::
  ActionExecutionOutput
newActionExecutionOutput =
  ActionExecutionOutput'
    { executionResult =
        Prelude.Nothing,
      outputVariables = Prelude.Nothing,
      outputArtifacts = Prelude.Nothing
    }

-- | Execution result information listed in the output details for an action
-- execution.
actionExecutionOutput_executionResult :: Lens.Lens' ActionExecutionOutput (Prelude.Maybe ActionExecutionResult)
actionExecutionOutput_executionResult = Lens.lens (\ActionExecutionOutput' {executionResult} -> executionResult) (\s@ActionExecutionOutput' {} a -> s {executionResult = a} :: ActionExecutionOutput)

-- | The outputVariables field shows the key-value pairs that were output as
-- part of that execution.
actionExecutionOutput_outputVariables :: Lens.Lens' ActionExecutionOutput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
actionExecutionOutput_outputVariables = Lens.lens (\ActionExecutionOutput' {outputVariables} -> outputVariables) (\s@ActionExecutionOutput' {} a -> s {outputVariables = a} :: ActionExecutionOutput) Prelude.. Lens.mapping Lens._Coerce

-- | Details of output artifacts of the action that correspond to the action
-- execution.
actionExecutionOutput_outputArtifacts :: Lens.Lens' ActionExecutionOutput (Prelude.Maybe [ArtifactDetail])
actionExecutionOutput_outputArtifacts = Lens.lens (\ActionExecutionOutput' {outputArtifacts} -> outputArtifacts) (\s@ActionExecutionOutput' {} a -> s {outputArtifacts = a} :: ActionExecutionOutput) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON ActionExecutionOutput where
  parseJSON =
    Core.withObject
      "ActionExecutionOutput"
      ( \x ->
          ActionExecutionOutput'
            Prelude.<$> (x Core..:? "executionResult")
            Prelude.<*> ( x Core..:? "outputVariables"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "outputArtifacts"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ActionExecutionOutput

instance Prelude.NFData ActionExecutionOutput

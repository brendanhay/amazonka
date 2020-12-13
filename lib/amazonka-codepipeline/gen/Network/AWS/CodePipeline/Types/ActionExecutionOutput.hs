{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecutionOutput
  ( ActionExecutionOutput (..),

    -- * Smart constructor
    mkActionExecutionOutput,

    -- * Lenses
    aeoOutputVariables,
    aeoOutputArtifacts,
    aeoExecutionResult,
  )
where

import Network.AWS.CodePipeline.Types.ActionExecutionResult
import Network.AWS.CodePipeline.Types.ArtifactDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Output details listed for an action execution, such as the action execution result.
--
-- /See:/ 'mkActionExecutionOutput' smart constructor.
data ActionExecutionOutput = ActionExecutionOutput'
  { -- | The outputVariables field shows the key-value pairs that were output as part of that execution.
    outputVariables :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Details of output artifacts of the action that correspond to the action execution.
    outputArtifacts :: Lude.Maybe [ArtifactDetail],
    -- | Execution result information listed in the output details for an action execution.
    executionResult :: Lude.Maybe ActionExecutionResult
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActionExecutionOutput' with the minimum fields required to make a request.
--
-- * 'outputVariables' - The outputVariables field shows the key-value pairs that were output as part of that execution.
-- * 'outputArtifacts' - Details of output artifacts of the action that correspond to the action execution.
-- * 'executionResult' - Execution result information listed in the output details for an action execution.
mkActionExecutionOutput ::
  ActionExecutionOutput
mkActionExecutionOutput =
  ActionExecutionOutput'
    { outputVariables = Lude.Nothing,
      outputArtifacts = Lude.Nothing,
      executionResult = Lude.Nothing
    }

-- | The outputVariables field shows the key-value pairs that were output as part of that execution.
--
-- /Note:/ Consider using 'outputVariables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeoOutputVariables :: Lens.Lens' ActionExecutionOutput (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
aeoOutputVariables = Lens.lens (outputVariables :: ActionExecutionOutput -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {outputVariables = a} :: ActionExecutionOutput)
{-# DEPRECATED aeoOutputVariables "Use generic-lens or generic-optics with 'outputVariables' instead." #-}

-- | Details of output artifacts of the action that correspond to the action execution.
--
-- /Note:/ Consider using 'outputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeoOutputArtifacts :: Lens.Lens' ActionExecutionOutput (Lude.Maybe [ArtifactDetail])
aeoOutputArtifacts = Lens.lens (outputArtifacts :: ActionExecutionOutput -> Lude.Maybe [ArtifactDetail]) (\s a -> s {outputArtifacts = a} :: ActionExecutionOutput)
{-# DEPRECATED aeoOutputArtifacts "Use generic-lens or generic-optics with 'outputArtifacts' instead." #-}

-- | Execution result information listed in the output details for an action execution.
--
-- /Note:/ Consider using 'executionResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeoExecutionResult :: Lens.Lens' ActionExecutionOutput (Lude.Maybe ActionExecutionResult)
aeoExecutionResult = Lens.lens (executionResult :: ActionExecutionOutput -> Lude.Maybe ActionExecutionResult) (\s a -> s {executionResult = a} :: ActionExecutionOutput)
{-# DEPRECATED aeoExecutionResult "Use generic-lens or generic-optics with 'executionResult' instead." #-}

instance Lude.FromJSON ActionExecutionOutput where
  parseJSON =
    Lude.withObject
      "ActionExecutionOutput"
      ( \x ->
          ActionExecutionOutput'
            Lude.<$> (x Lude..:? "outputVariables" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "outputArtifacts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "executionResult")
      )

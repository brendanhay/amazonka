{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.ActionExecutionOutput
  ( ActionExecutionOutput (..)
  -- * Smart constructor
  , mkActionExecutionOutput
  -- * Lenses
  , aeoExecutionResult
  , aeoOutputArtifacts
  , aeoOutputVariables
  ) where

import qualified Network.AWS.CodePipeline.Types.ActionExecutionResult as Types
import qualified Network.AWS.CodePipeline.Types.ArtifactDetail as Types
import qualified Network.AWS.CodePipeline.Types.OutputVariablesKey as Types
import qualified Network.AWS.CodePipeline.Types.OutputVariablesValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Output details listed for an action execution, such as the action execution result.
--
-- /See:/ 'mkActionExecutionOutput' smart constructor.
data ActionExecutionOutput = ActionExecutionOutput'
  { executionResult :: Core.Maybe Types.ActionExecutionResult
    -- ^ Execution result information listed in the output details for an action execution.
  , outputArtifacts :: Core.Maybe [Types.ArtifactDetail]
    -- ^ Details of output artifacts of the action that correspond to the action execution.
  , outputVariables :: Core.Maybe (Core.HashMap Types.OutputVariablesKey Types.OutputVariablesValue)
    -- ^ The outputVariables field shows the key-value pairs that were output as part of that execution.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActionExecutionOutput' value with any optional fields omitted.
mkActionExecutionOutput
    :: ActionExecutionOutput
mkActionExecutionOutput
  = ActionExecutionOutput'{executionResult = Core.Nothing,
                           outputArtifacts = Core.Nothing, outputVariables = Core.Nothing}

-- | Execution result information listed in the output details for an action execution.
--
-- /Note:/ Consider using 'executionResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeoExecutionResult :: Lens.Lens' ActionExecutionOutput (Core.Maybe Types.ActionExecutionResult)
aeoExecutionResult = Lens.field @"executionResult"
{-# INLINEABLE aeoExecutionResult #-}
{-# DEPRECATED executionResult "Use generic-lens or generic-optics with 'executionResult' instead"  #-}

-- | Details of output artifacts of the action that correspond to the action execution.
--
-- /Note:/ Consider using 'outputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeoOutputArtifacts :: Lens.Lens' ActionExecutionOutput (Core.Maybe [Types.ArtifactDetail])
aeoOutputArtifacts = Lens.field @"outputArtifacts"
{-# INLINEABLE aeoOutputArtifacts #-}
{-# DEPRECATED outputArtifacts "Use generic-lens or generic-optics with 'outputArtifacts' instead"  #-}

-- | The outputVariables field shows the key-value pairs that were output as part of that execution.
--
-- /Note:/ Consider using 'outputVariables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeoOutputVariables :: Lens.Lens' ActionExecutionOutput (Core.Maybe (Core.HashMap Types.OutputVariablesKey Types.OutputVariablesValue))
aeoOutputVariables = Lens.field @"outputVariables"
{-# INLINEABLE aeoOutputVariables #-}
{-# DEPRECATED outputVariables "Use generic-lens or generic-optics with 'outputVariables' instead"  #-}

instance Core.FromJSON ActionExecutionOutput where
        parseJSON
          = Core.withObject "ActionExecutionOutput" Core.$
              \ x ->
                ActionExecutionOutput' Core.<$>
                  (x Core..:? "executionResult") Core.<*>
                    x Core..:? "outputArtifacts"
                    Core.<*> x Core..:? "outputVariables"

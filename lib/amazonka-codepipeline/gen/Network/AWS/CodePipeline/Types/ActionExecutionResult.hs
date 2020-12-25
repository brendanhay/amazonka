{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecutionResult
  ( ActionExecutionResult (..),

    -- * Smart constructor
    mkActionExecutionResult,

    -- * Lenses
    aerExternalExecutionId,
    aerExternalExecutionSummary,
    aerExternalExecutionUrl,
  )
where

import qualified Network.AWS.CodePipeline.Types.ExternalExecutionId as Types
import qualified Network.AWS.CodePipeline.Types.ExternalExecutionSummary as Types
import qualified Network.AWS.CodePipeline.Types.ExternalExecutionUrl as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Execution result information, such as the external execution ID.
--
-- /See:/ 'mkActionExecutionResult' smart constructor.
data ActionExecutionResult = ActionExecutionResult'
  { -- | The action provider's external ID for the action execution.
    externalExecutionId :: Core.Maybe Types.ExternalExecutionId,
    -- | The action provider's summary for the action execution.
    externalExecutionSummary :: Core.Maybe Types.ExternalExecutionSummary,
    -- | The deepest external link to the external resource (for example, a repository URL or deployment endpoint) that is used when running the action.
    externalExecutionUrl :: Core.Maybe Types.ExternalExecutionUrl
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActionExecutionResult' value with any optional fields omitted.
mkActionExecutionResult ::
  ActionExecutionResult
mkActionExecutionResult =
  ActionExecutionResult'
    { externalExecutionId = Core.Nothing,
      externalExecutionSummary = Core.Nothing,
      externalExecutionUrl = Core.Nothing
    }

-- | The action provider's external ID for the action execution.
--
-- /Note:/ Consider using 'externalExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aerExternalExecutionId :: Lens.Lens' ActionExecutionResult (Core.Maybe Types.ExternalExecutionId)
aerExternalExecutionId = Lens.field @"externalExecutionId"
{-# DEPRECATED aerExternalExecutionId "Use generic-lens or generic-optics with 'externalExecutionId' instead." #-}

-- | The action provider's summary for the action execution.
--
-- /Note:/ Consider using 'externalExecutionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aerExternalExecutionSummary :: Lens.Lens' ActionExecutionResult (Core.Maybe Types.ExternalExecutionSummary)
aerExternalExecutionSummary = Lens.field @"externalExecutionSummary"
{-# DEPRECATED aerExternalExecutionSummary "Use generic-lens or generic-optics with 'externalExecutionSummary' instead." #-}

-- | The deepest external link to the external resource (for example, a repository URL or deployment endpoint) that is used when running the action.
--
-- /Note:/ Consider using 'externalExecutionUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aerExternalExecutionUrl :: Lens.Lens' ActionExecutionResult (Core.Maybe Types.ExternalExecutionUrl)
aerExternalExecutionUrl = Lens.field @"externalExecutionUrl"
{-# DEPRECATED aerExternalExecutionUrl "Use generic-lens or generic-optics with 'externalExecutionUrl' instead." #-}

instance Core.FromJSON ActionExecutionResult where
  parseJSON =
    Core.withObject "ActionExecutionResult" Core.$
      \x ->
        ActionExecutionResult'
          Core.<$> (x Core..:? "externalExecutionId")
          Core.<*> (x Core..:? "externalExecutionSummary")
          Core.<*> (x Core..:? "externalExecutionUrl")

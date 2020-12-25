{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ExecutionDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ExecutionDetails
  ( ExecutionDetails (..),

    -- * Smart constructor
    mkExecutionDetails,

    -- * Lenses
    edExternalExecutionId,
    edPercentComplete,
    edSummary,
  )
where

import qualified Network.AWS.CodePipeline.Types.ExecutionId as Types
import qualified Network.AWS.CodePipeline.Types.Summary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of the actions taken and results produced on an artifact as it passes through stages in the pipeline.
--
-- /See:/ 'mkExecutionDetails' smart constructor.
data ExecutionDetails = ExecutionDetails'
  { -- | The system-generated unique ID of this action used to identify this job worker in any external systems, such as AWS CodeDeploy.
    externalExecutionId :: Core.Maybe Types.ExecutionId,
    -- | The percentage of work completed on the action, represented on a scale of 0 to 100 percent.
    percentComplete :: Core.Maybe Core.Natural,
    -- | The summary of the current status of the actions.
    summary :: Core.Maybe Types.Summary
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExecutionDetails' value with any optional fields omitted.
mkExecutionDetails ::
  ExecutionDetails
mkExecutionDetails =
  ExecutionDetails'
    { externalExecutionId = Core.Nothing,
      percentComplete = Core.Nothing,
      summary = Core.Nothing
    }

-- | The system-generated unique ID of this action used to identify this job worker in any external systems, such as AWS CodeDeploy.
--
-- /Note:/ Consider using 'externalExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edExternalExecutionId :: Lens.Lens' ExecutionDetails (Core.Maybe Types.ExecutionId)
edExternalExecutionId = Lens.field @"externalExecutionId"
{-# DEPRECATED edExternalExecutionId "Use generic-lens or generic-optics with 'externalExecutionId' instead." #-}

-- | The percentage of work completed on the action, represented on a scale of 0 to 100 percent.
--
-- /Note:/ Consider using 'percentComplete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edPercentComplete :: Lens.Lens' ExecutionDetails (Core.Maybe Core.Natural)
edPercentComplete = Lens.field @"percentComplete"
{-# DEPRECATED edPercentComplete "Use generic-lens or generic-optics with 'percentComplete' instead." #-}

-- | The summary of the current status of the actions.
--
-- /Note:/ Consider using 'summary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edSummary :: Lens.Lens' ExecutionDetails (Core.Maybe Types.Summary)
edSummary = Lens.field @"summary"
{-# DEPRECATED edSummary "Use generic-lens or generic-optics with 'summary' instead." #-}

instance Core.FromJSON ExecutionDetails where
  toJSON ExecutionDetails {..} =
    Core.object
      ( Core.catMaybes
          [ ("externalExecutionId" Core..=) Core.<$> externalExecutionId,
            ("percentComplete" Core..=) Core.<$> percentComplete,
            ("summary" Core..=) Core.<$> summary
          ]
      )

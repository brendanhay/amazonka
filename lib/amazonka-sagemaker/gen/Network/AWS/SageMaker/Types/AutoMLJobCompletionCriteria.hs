{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobCompletionCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobCompletionCriteria
  ( AutoMLJobCompletionCriteria (..),

    -- * Smart constructor
    mkAutoMLJobCompletionCriteria,

    -- * Lenses
    amljccMaxAutoMLJobRuntimeInSeconds,
    amljccMaxCandidates,
    amljccMaxRuntimePerTrainingJobInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | How long a job is allowed to run, or how many candidates a job is allowed to generate.
--
-- /See:/ 'mkAutoMLJobCompletionCriteria' smart constructor.
data AutoMLJobCompletionCriteria = AutoMLJobCompletionCriteria'
  { -- | The maximum time, in seconds, an AutoML job is allowed to wait for a trial to complete. It must be equal to or greater than MaxRuntimePerTrainingJobInSeconds.
    maxAutoMLJobRuntimeInSeconds :: Core.Maybe Core.Natural,
    -- | The maximum number of times a training job is allowed to run.
    maxCandidates :: Core.Maybe Core.Natural,
    -- | The maximum time, in seconds, a job is allowed to run.
    maxRuntimePerTrainingJobInSeconds :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoMLJobCompletionCriteria' value with any optional fields omitted.
mkAutoMLJobCompletionCriteria ::
  AutoMLJobCompletionCriteria
mkAutoMLJobCompletionCriteria =
  AutoMLJobCompletionCriteria'
    { maxAutoMLJobRuntimeInSeconds =
        Core.Nothing,
      maxCandidates = Core.Nothing,
      maxRuntimePerTrainingJobInSeconds = Core.Nothing
    }

-- | The maximum time, in seconds, an AutoML job is allowed to wait for a trial to complete. It must be equal to or greater than MaxRuntimePerTrainingJobInSeconds.
--
-- /Note:/ Consider using 'maxAutoMLJobRuntimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljccMaxAutoMLJobRuntimeInSeconds :: Lens.Lens' AutoMLJobCompletionCriteria (Core.Maybe Core.Natural)
amljccMaxAutoMLJobRuntimeInSeconds = Lens.field @"maxAutoMLJobRuntimeInSeconds"
{-# DEPRECATED amljccMaxAutoMLJobRuntimeInSeconds "Use generic-lens or generic-optics with 'maxAutoMLJobRuntimeInSeconds' instead." #-}

-- | The maximum number of times a training job is allowed to run.
--
-- /Note:/ Consider using 'maxCandidates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljccMaxCandidates :: Lens.Lens' AutoMLJobCompletionCriteria (Core.Maybe Core.Natural)
amljccMaxCandidates = Lens.field @"maxCandidates"
{-# DEPRECATED amljccMaxCandidates "Use generic-lens or generic-optics with 'maxCandidates' instead." #-}

-- | The maximum time, in seconds, a job is allowed to run.
--
-- /Note:/ Consider using 'maxRuntimePerTrainingJobInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljccMaxRuntimePerTrainingJobInSeconds :: Lens.Lens' AutoMLJobCompletionCriteria (Core.Maybe Core.Natural)
amljccMaxRuntimePerTrainingJobInSeconds = Lens.field @"maxRuntimePerTrainingJobInSeconds"
{-# DEPRECATED amljccMaxRuntimePerTrainingJobInSeconds "Use generic-lens or generic-optics with 'maxRuntimePerTrainingJobInSeconds' instead." #-}

instance Core.FromJSON AutoMLJobCompletionCriteria where
  toJSON AutoMLJobCompletionCriteria {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxAutoMLJobRuntimeInSeconds" Core..=)
              Core.<$> maxAutoMLJobRuntimeInSeconds,
            ("MaxCandidates" Core..=) Core.<$> maxCandidates,
            ("MaxRuntimePerTrainingJobInSeconds" Core..=)
              Core.<$> maxRuntimePerTrainingJobInSeconds
          ]
      )

instance Core.FromJSON AutoMLJobCompletionCriteria where
  parseJSON =
    Core.withObject "AutoMLJobCompletionCriteria" Core.$
      \x ->
        AutoMLJobCompletionCriteria'
          Core.<$> (x Core..:? "MaxAutoMLJobRuntimeInSeconds")
          Core.<*> (x Core..:? "MaxCandidates")
          Core.<*> (x Core..:? "MaxRuntimePerTrainingJobInSeconds")

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SearchRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SearchRecord
  ( SearchRecord (..),

    -- * Smart constructor
    mkSearchRecord,

    -- * Lenses
    srExperiment,
    srTrainingJob,
    srTrial,
    srTrialComponent,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.Experiment as Types
import qualified Network.AWS.SageMaker.Types.TrainingJob as Types
import qualified Network.AWS.SageMaker.Types.Trial as Types
import qualified Network.AWS.SageMaker.Types.TrialComponent as Types

-- | A single resource returned as part of the 'Search' API response.
--
-- /See:/ 'mkSearchRecord' smart constructor.
data SearchRecord = SearchRecord'
  { -- | The properties of an experiment.
    experiment :: Core.Maybe Types.Experiment,
    -- | The properties of a training job.
    trainingJob :: Core.Maybe Types.TrainingJob,
    -- | The properties of a trial.
    trial :: Core.Maybe Types.Trial,
    -- | The properties of a trial component.
    trialComponent :: Core.Maybe Types.TrialComponent
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SearchRecord' value with any optional fields omitted.
mkSearchRecord ::
  SearchRecord
mkSearchRecord =
  SearchRecord'
    { experiment = Core.Nothing,
      trainingJob = Core.Nothing,
      trial = Core.Nothing,
      trialComponent = Core.Nothing
    }

-- | The properties of an experiment.
--
-- /Note:/ Consider using 'experiment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srExperiment :: Lens.Lens' SearchRecord (Core.Maybe Types.Experiment)
srExperiment = Lens.field @"experiment"
{-# DEPRECATED srExperiment "Use generic-lens or generic-optics with 'experiment' instead." #-}

-- | The properties of a training job.
--
-- /Note:/ Consider using 'trainingJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srTrainingJob :: Lens.Lens' SearchRecord (Core.Maybe Types.TrainingJob)
srTrainingJob = Lens.field @"trainingJob"
{-# DEPRECATED srTrainingJob "Use generic-lens or generic-optics with 'trainingJob' instead." #-}

-- | The properties of a trial.
--
-- /Note:/ Consider using 'trial' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srTrial :: Lens.Lens' SearchRecord (Core.Maybe Types.Trial)
srTrial = Lens.field @"trial"
{-# DEPRECATED srTrial "Use generic-lens or generic-optics with 'trial' instead." #-}

-- | The properties of a trial component.
--
-- /Note:/ Consider using 'trialComponent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srTrialComponent :: Lens.Lens' SearchRecord (Core.Maybe Types.TrialComponent)
srTrialComponent = Lens.field @"trialComponent"
{-# DEPRECATED srTrialComponent "Use generic-lens or generic-optics with 'trialComponent' instead." #-}

instance Core.FromJSON SearchRecord where
  parseJSON =
    Core.withObject "SearchRecord" Core.$
      \x ->
        SearchRecord'
          Core.<$> (x Core..:? "Experiment")
          Core.<*> (x Core..:? "TrainingJob")
          Core.<*> (x Core..:? "Trial")
          Core.<*> (x Core..:? "TrialComponent")

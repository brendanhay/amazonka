{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ExperimentConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ExperimentConfig
  ( ExperimentConfig (..),

    -- * Smart constructor
    mkExperimentConfig,

    -- * Lenses
    ecExperimentName,
    ecTrialComponentDisplayName,
    ecTrialName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ExperimentEntityName as Types

-- | Associates a SageMaker job as a trial component with an experiment and trial. Specified when you call the following APIs:
--
--
--     * 'CreateProcessingJob'
--
--
--     * 'CreateTrainingJob'
--
--
--     * 'CreateTransformJob'
--
--
--
-- /See:/ 'mkExperimentConfig' smart constructor.
data ExperimentConfig = ExperimentConfig'
  { -- | The name of an existing experiment to associate the trial component with.
    experimentName :: Core.Maybe Types.ExperimentEntityName,
    -- | The display name for the trial component. If this key isn't specified, the display name is the trial component name.
    trialComponentDisplayName :: Core.Maybe Types.ExperimentEntityName,
    -- | The name of an existing trial to associate the trial component with. If not specified, a new trial is created.
    trialName :: Core.Maybe Types.ExperimentEntityName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExperimentConfig' value with any optional fields omitted.
mkExperimentConfig ::
  ExperimentConfig
mkExperimentConfig =
  ExperimentConfig'
    { experimentName = Core.Nothing,
      trialComponentDisplayName = Core.Nothing,
      trialName = Core.Nothing
    }

-- | The name of an existing experiment to associate the trial component with.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecExperimentName :: Lens.Lens' ExperimentConfig (Core.Maybe Types.ExperimentEntityName)
ecExperimentName = Lens.field @"experimentName"
{-# DEPRECATED ecExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

-- | The display name for the trial component. If this key isn't specified, the display name is the trial component name.
--
-- /Note:/ Consider using 'trialComponentDisplayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecTrialComponentDisplayName :: Lens.Lens' ExperimentConfig (Core.Maybe Types.ExperimentEntityName)
ecTrialComponentDisplayName = Lens.field @"trialComponentDisplayName"
{-# DEPRECATED ecTrialComponentDisplayName "Use generic-lens or generic-optics with 'trialComponentDisplayName' instead." #-}

-- | The name of an existing trial to associate the trial component with. If not specified, a new trial is created.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecTrialName :: Lens.Lens' ExperimentConfig (Core.Maybe Types.ExperimentEntityName)
ecTrialName = Lens.field @"trialName"
{-# DEPRECATED ecTrialName "Use generic-lens or generic-optics with 'trialName' instead." #-}

instance Core.FromJSON ExperimentConfig where
  toJSON ExperimentConfig {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExperimentName" Core..=) Core.<$> experimentName,
            ("TrialComponentDisplayName" Core..=)
              Core.<$> trialComponentDisplayName,
            ("TrialName" Core..=) Core.<$> trialName
          ]
      )

instance Core.FromJSON ExperimentConfig where
  parseJSON =
    Core.withObject "ExperimentConfig" Core.$
      \x ->
        ExperimentConfig'
          Core.<$> (x Core..:? "ExperimentName")
          Core.<*> (x Core..:? "TrialComponentDisplayName")
          Core.<*> (x Core..:? "TrialName")

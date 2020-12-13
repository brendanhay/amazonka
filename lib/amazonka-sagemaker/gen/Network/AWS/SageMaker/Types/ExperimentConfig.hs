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
    ecTrialComponentDisplayName,
    ecExperimentName,
    ecTrialName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { -- | The display name for the trial component. If this key isn't specified, the display name is the trial component name.
    trialComponentDisplayName :: Lude.Maybe Lude.Text,
    -- | The name of an existing experiment to associate the trial component with.
    experimentName :: Lude.Maybe Lude.Text,
    -- | The name of an existing trial to associate the trial component with. If not specified, a new trial is created.
    trialName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExperimentConfig' with the minimum fields required to make a request.
--
-- * 'trialComponentDisplayName' - The display name for the trial component. If this key isn't specified, the display name is the trial component name.
-- * 'experimentName' - The name of an existing experiment to associate the trial component with.
-- * 'trialName' - The name of an existing trial to associate the trial component with. If not specified, a new trial is created.
mkExperimentConfig ::
  ExperimentConfig
mkExperimentConfig =
  ExperimentConfig'
    { trialComponentDisplayName = Lude.Nothing,
      experimentName = Lude.Nothing,
      trialName = Lude.Nothing
    }

-- | The display name for the trial component. If this key isn't specified, the display name is the trial component name.
--
-- /Note:/ Consider using 'trialComponentDisplayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecTrialComponentDisplayName :: Lens.Lens' ExperimentConfig (Lude.Maybe Lude.Text)
ecTrialComponentDisplayName = Lens.lens (trialComponentDisplayName :: ExperimentConfig -> Lude.Maybe Lude.Text) (\s a -> s {trialComponentDisplayName = a} :: ExperimentConfig)
{-# DEPRECATED ecTrialComponentDisplayName "Use generic-lens or generic-optics with 'trialComponentDisplayName' instead." #-}

-- | The name of an existing experiment to associate the trial component with.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecExperimentName :: Lens.Lens' ExperimentConfig (Lude.Maybe Lude.Text)
ecExperimentName = Lens.lens (experimentName :: ExperimentConfig -> Lude.Maybe Lude.Text) (\s a -> s {experimentName = a} :: ExperimentConfig)
{-# DEPRECATED ecExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

-- | The name of an existing trial to associate the trial component with. If not specified, a new trial is created.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecTrialName :: Lens.Lens' ExperimentConfig (Lude.Maybe Lude.Text)
ecTrialName = Lens.lens (trialName :: ExperimentConfig -> Lude.Maybe Lude.Text) (\s a -> s {trialName = a} :: ExperimentConfig)
{-# DEPRECATED ecTrialName "Use generic-lens or generic-optics with 'trialName' instead." #-}

instance Lude.FromJSON ExperimentConfig where
  parseJSON =
    Lude.withObject
      "ExperimentConfig"
      ( \x ->
          ExperimentConfig'
            Lude.<$> (x Lude..:? "TrialComponentDisplayName")
            Lude.<*> (x Lude..:? "ExperimentName")
            Lude.<*> (x Lude..:? "TrialName")
      )

instance Lude.ToJSON ExperimentConfig where
  toJSON ExperimentConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TrialComponentDisplayName" Lude..=)
              Lude.<$> trialComponentDisplayName,
            ("ExperimentName" Lude..=) Lude.<$> experimentName,
            ("TrialName" Lude..=) Lude.<$> trialName
          ]
      )

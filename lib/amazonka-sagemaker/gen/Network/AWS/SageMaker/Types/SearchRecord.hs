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
    srTrainingJob,
    srTrial,
    srTrialComponent,
    srExperiment,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.Experiment
import Network.AWS.SageMaker.Types.TrainingJob
import Network.AWS.SageMaker.Types.Trial
import Network.AWS.SageMaker.Types.TrialComponent

-- | A single resource returned as part of the 'Search' API response.
--
-- /See:/ 'mkSearchRecord' smart constructor.
data SearchRecord = SearchRecord'
  { trainingJob ::
      Lude.Maybe TrainingJob,
    trial :: Lude.Maybe Trial,
    trialComponent :: Lude.Maybe TrialComponent,
    experiment :: Lude.Maybe Experiment
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchRecord' with the minimum fields required to make a request.
--
-- * 'experiment' - The properties of an experiment.
-- * 'trainingJob' - The properties of a training job.
-- * 'trial' - The properties of a trial.
-- * 'trialComponent' - The properties of a trial component.
mkSearchRecord ::
  SearchRecord
mkSearchRecord =
  SearchRecord'
    { trainingJob = Lude.Nothing,
      trial = Lude.Nothing,
      trialComponent = Lude.Nothing,
      experiment = Lude.Nothing
    }

-- | The properties of a training job.
--
-- /Note:/ Consider using 'trainingJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srTrainingJob :: Lens.Lens' SearchRecord (Lude.Maybe TrainingJob)
srTrainingJob = Lens.lens (trainingJob :: SearchRecord -> Lude.Maybe TrainingJob) (\s a -> s {trainingJob = a} :: SearchRecord)
{-# DEPRECATED srTrainingJob "Use generic-lens or generic-optics with 'trainingJob' instead." #-}

-- | The properties of a trial.
--
-- /Note:/ Consider using 'trial' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srTrial :: Lens.Lens' SearchRecord (Lude.Maybe Trial)
srTrial = Lens.lens (trial :: SearchRecord -> Lude.Maybe Trial) (\s a -> s {trial = a} :: SearchRecord)
{-# DEPRECATED srTrial "Use generic-lens or generic-optics with 'trial' instead." #-}

-- | The properties of a trial component.
--
-- /Note:/ Consider using 'trialComponent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srTrialComponent :: Lens.Lens' SearchRecord (Lude.Maybe TrialComponent)
srTrialComponent = Lens.lens (trialComponent :: SearchRecord -> Lude.Maybe TrialComponent) (\s a -> s {trialComponent = a} :: SearchRecord)
{-# DEPRECATED srTrialComponent "Use generic-lens or generic-optics with 'trialComponent' instead." #-}

-- | The properties of an experiment.
--
-- /Note:/ Consider using 'experiment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srExperiment :: Lens.Lens' SearchRecord (Lude.Maybe Experiment)
srExperiment = Lens.lens (experiment :: SearchRecord -> Lude.Maybe Experiment) (\s a -> s {experiment = a} :: SearchRecord)
{-# DEPRECATED srExperiment "Use generic-lens or generic-optics with 'experiment' instead." #-}

instance Lude.FromJSON SearchRecord where
  parseJSON =
    Lude.withObject
      "SearchRecord"
      ( \x ->
          SearchRecord'
            Lude.<$> (x Lude..:? "TrainingJob")
            Lude.<*> (x Lude..:? "Trial")
            Lude.<*> (x Lude..:? "TrialComponent")
            Lude.<*> (x Lude..:? "Experiment")
      )

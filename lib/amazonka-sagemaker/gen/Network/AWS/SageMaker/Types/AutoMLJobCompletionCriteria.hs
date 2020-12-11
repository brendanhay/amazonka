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
    amljccMaxCandidates,
    amljccMaxRuntimePerTrainingJobInSeconds,
    amljccMaxAutoMLJobRuntimeInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | How long a job is allowed to run, or how many candidates a job is allowed to generate.
--
-- /See:/ 'mkAutoMLJobCompletionCriteria' smart constructor.
data AutoMLJobCompletionCriteria = AutoMLJobCompletionCriteria'
  { maxCandidates ::
      Lude.Maybe Lude.Natural,
    maxRuntimePerTrainingJobInSeconds ::
      Lude.Maybe Lude.Natural,
    maxAutoMLJobRuntimeInSeconds ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoMLJobCompletionCriteria' with the minimum fields required to make a request.
--
-- * 'maxAutoMLJobRuntimeInSeconds' - The maximum time, in seconds, an AutoML job is allowed to wait for a trial to complete. It must be equal to or greater than MaxRuntimePerTrainingJobInSeconds.
-- * 'maxCandidates' - The maximum number of times a training job is allowed to run.
-- * 'maxRuntimePerTrainingJobInSeconds' - The maximum time, in seconds, a job is allowed to run.
mkAutoMLJobCompletionCriteria ::
  AutoMLJobCompletionCriteria
mkAutoMLJobCompletionCriteria =
  AutoMLJobCompletionCriteria'
    { maxCandidates = Lude.Nothing,
      maxRuntimePerTrainingJobInSeconds = Lude.Nothing,
      maxAutoMLJobRuntimeInSeconds = Lude.Nothing
    }

-- | The maximum number of times a training job is allowed to run.
--
-- /Note:/ Consider using 'maxCandidates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljccMaxCandidates :: Lens.Lens' AutoMLJobCompletionCriteria (Lude.Maybe Lude.Natural)
amljccMaxCandidates = Lens.lens (maxCandidates :: AutoMLJobCompletionCriteria -> Lude.Maybe Lude.Natural) (\s a -> s {maxCandidates = a} :: AutoMLJobCompletionCriteria)
{-# DEPRECATED amljccMaxCandidates "Use generic-lens or generic-optics with 'maxCandidates' instead." #-}

-- | The maximum time, in seconds, a job is allowed to run.
--
-- /Note:/ Consider using 'maxRuntimePerTrainingJobInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljccMaxRuntimePerTrainingJobInSeconds :: Lens.Lens' AutoMLJobCompletionCriteria (Lude.Maybe Lude.Natural)
amljccMaxRuntimePerTrainingJobInSeconds = Lens.lens (maxRuntimePerTrainingJobInSeconds :: AutoMLJobCompletionCriteria -> Lude.Maybe Lude.Natural) (\s a -> s {maxRuntimePerTrainingJobInSeconds = a} :: AutoMLJobCompletionCriteria)
{-# DEPRECATED amljccMaxRuntimePerTrainingJobInSeconds "Use generic-lens or generic-optics with 'maxRuntimePerTrainingJobInSeconds' instead." #-}

-- | The maximum time, in seconds, an AutoML job is allowed to wait for a trial to complete. It must be equal to or greater than MaxRuntimePerTrainingJobInSeconds.
--
-- /Note:/ Consider using 'maxAutoMLJobRuntimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljccMaxAutoMLJobRuntimeInSeconds :: Lens.Lens' AutoMLJobCompletionCriteria (Lude.Maybe Lude.Natural)
amljccMaxAutoMLJobRuntimeInSeconds = Lens.lens (maxAutoMLJobRuntimeInSeconds :: AutoMLJobCompletionCriteria -> Lude.Maybe Lude.Natural) (\s a -> s {maxAutoMLJobRuntimeInSeconds = a} :: AutoMLJobCompletionCriteria)
{-# DEPRECATED amljccMaxAutoMLJobRuntimeInSeconds "Use generic-lens or generic-optics with 'maxAutoMLJobRuntimeInSeconds' instead." #-}

instance Lude.FromJSON AutoMLJobCompletionCriteria where
  parseJSON =
    Lude.withObject
      "AutoMLJobCompletionCriteria"
      ( \x ->
          AutoMLJobCompletionCriteria'
            Lude.<$> (x Lude..:? "MaxCandidates")
            Lude.<*> (x Lude..:? "MaxRuntimePerTrainingJobInSeconds")
            Lude.<*> (x Lude..:? "MaxAutoMLJobRuntimeInSeconds")
      )

instance Lude.ToJSON AutoMLJobCompletionCriteria where
  toJSON AutoMLJobCompletionCriteria' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MaxCandidates" Lude..=) Lude.<$> maxCandidates,
            ("MaxRuntimePerTrainingJobInSeconds" Lude..=)
              Lude.<$> maxRuntimePerTrainingJobInSeconds,
            ("MaxAutoMLJobRuntimeInSeconds" Lude..=)
              Lude.<$> maxAutoMLJobRuntimeInSeconds
          ]
      )

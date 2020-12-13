{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Parent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Parent
  ( Parent (..),

    -- * Smart constructor
    mkParent,

    -- * Lenses
    pExperimentName,
    pTrialName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The trial that a trial component is associated with and the experiment the trial is part of. A component might not be associated with a trial. A component can be associated with multiple trials.
--
-- /See:/ 'mkParent' smart constructor.
data Parent = Parent'
  { -- | The name of the experiment.
    experimentName :: Lude.Maybe Lude.Text,
    -- | The name of the trial.
    trialName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Parent' with the minimum fields required to make a request.
--
-- * 'experimentName' - The name of the experiment.
-- * 'trialName' - The name of the trial.
mkParent ::
  Parent
mkParent =
  Parent' {experimentName = Lude.Nothing, trialName = Lude.Nothing}

-- | The name of the experiment.
--
-- /Note:/ Consider using 'experimentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pExperimentName :: Lens.Lens' Parent (Lude.Maybe Lude.Text)
pExperimentName = Lens.lens (experimentName :: Parent -> Lude.Maybe Lude.Text) (\s a -> s {experimentName = a} :: Parent)
{-# DEPRECATED pExperimentName "Use generic-lens or generic-optics with 'experimentName' instead." #-}

-- | The name of the trial.
--
-- /Note:/ Consider using 'trialName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTrialName :: Lens.Lens' Parent (Lude.Maybe Lude.Text)
pTrialName = Lens.lens (trialName :: Parent -> Lude.Maybe Lude.Text) (\s a -> s {trialName = a} :: Parent)
{-# DEPRECATED pTrialName "Use generic-lens or generic-optics with 'trialName' instead." #-}

instance Lude.FromJSON Parent where
  parseJSON =
    Lude.withObject
      "Parent"
      ( \x ->
          Parent'
            Lude.<$> (x Lude..:? "ExperimentName") Lude.<*> (x Lude..:? "TrialName")
      )

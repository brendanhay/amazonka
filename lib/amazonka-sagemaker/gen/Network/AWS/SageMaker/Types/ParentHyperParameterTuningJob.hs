{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ParentHyperParameterTuningJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ParentHyperParameterTuningJob
  ( ParentHyperParameterTuningJob (..),

    -- * Smart constructor
    mkParentHyperParameterTuningJob,

    -- * Lenses
    phptjHyperParameterTuningJobName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A previously completed or stopped hyperparameter tuning job to be used as a starting point for a new hyperparameter tuning job.
--
-- /See:/ 'mkParentHyperParameterTuningJob' smart constructor.
newtype ParentHyperParameterTuningJob = ParentHyperParameterTuningJob'
  { -- | The name of the hyperparameter tuning job to be used as a starting point for a new hyperparameter tuning job.
    hyperParameterTuningJobName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParentHyperParameterTuningJob' with the minimum fields required to make a request.
--
-- * 'hyperParameterTuningJobName' - The name of the hyperparameter tuning job to be used as a starting point for a new hyperparameter tuning job.
mkParentHyperParameterTuningJob ::
  ParentHyperParameterTuningJob
mkParentHyperParameterTuningJob =
  ParentHyperParameterTuningJob'
    { hyperParameterTuningJobName =
        Lude.Nothing
    }

-- | The name of the hyperparameter tuning job to be used as a starting point for a new hyperparameter tuning job.
--
-- /Note:/ Consider using 'hyperParameterTuningJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phptjHyperParameterTuningJobName :: Lens.Lens' ParentHyperParameterTuningJob (Lude.Maybe Lude.Text)
phptjHyperParameterTuningJobName = Lens.lens (hyperParameterTuningJobName :: ParentHyperParameterTuningJob -> Lude.Maybe Lude.Text) (\s a -> s {hyperParameterTuningJobName = a} :: ParentHyperParameterTuningJob)
{-# DEPRECATED phptjHyperParameterTuningJobName "Use generic-lens or generic-optics with 'hyperParameterTuningJobName' instead." #-}

instance Lude.FromJSON ParentHyperParameterTuningJob where
  parseJSON =
    Lude.withObject
      "ParentHyperParameterTuningJob"
      ( \x ->
          ParentHyperParameterTuningJob'
            Lude.<$> (x Lude..:? "HyperParameterTuningJobName")
      )

instance Lude.ToJSON ParentHyperParameterTuningJob where
  toJSON ParentHyperParameterTuningJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("HyperParameterTuningJobName" Lude..=)
              Lude.<$> hyperParameterTuningJobName
          ]
      )

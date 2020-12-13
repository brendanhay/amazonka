{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TuningJobCompletionCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TuningJobCompletionCriteria
  ( TuningJobCompletionCriteria (..),

    -- * Smart constructor
    mkTuningJobCompletionCriteria,

    -- * Lenses
    tjccTargetObjectiveMetricValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The job completion criteria.
--
-- /See:/ 'mkTuningJobCompletionCriteria' smart constructor.
newtype TuningJobCompletionCriteria = TuningJobCompletionCriteria'
  { -- | The value of the objective metric.
    targetObjectiveMetricValue :: Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TuningJobCompletionCriteria' with the minimum fields required to make a request.
--
-- * 'targetObjectiveMetricValue' - The value of the objective metric.
mkTuningJobCompletionCriteria ::
  -- | 'targetObjectiveMetricValue'
  Lude.Double ->
  TuningJobCompletionCriteria
mkTuningJobCompletionCriteria pTargetObjectiveMetricValue_ =
  TuningJobCompletionCriteria'
    { targetObjectiveMetricValue =
        pTargetObjectiveMetricValue_
    }

-- | The value of the objective metric.
--
-- /Note:/ Consider using 'targetObjectiveMetricValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjccTargetObjectiveMetricValue :: Lens.Lens' TuningJobCompletionCriteria Lude.Double
tjccTargetObjectiveMetricValue = Lens.lens (targetObjectiveMetricValue :: TuningJobCompletionCriteria -> Lude.Double) (\s a -> s {targetObjectiveMetricValue = a} :: TuningJobCompletionCriteria)
{-# DEPRECATED tjccTargetObjectiveMetricValue "Use generic-lens or generic-optics with 'targetObjectiveMetricValue' instead." #-}

instance Lude.FromJSON TuningJobCompletionCriteria where
  parseJSON =
    Lude.withObject
      "TuningJobCompletionCriteria"
      ( \x ->
          TuningJobCompletionCriteria'
            Lude.<$> (x Lude..: "TargetObjectiveMetricValue")
      )

instance Lude.ToJSON TuningJobCompletionCriteria where
  toJSON TuningJobCompletionCriteria' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("TargetObjectiveMetricValue" Lude..= targetObjectiveMetricValue)
          ]
      )

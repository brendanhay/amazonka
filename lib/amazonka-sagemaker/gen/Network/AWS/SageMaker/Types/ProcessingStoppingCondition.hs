-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingStoppingCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingStoppingCondition
  ( ProcessingStoppingCondition (..),

    -- * Smart constructor
    mkProcessingStoppingCondition,

    -- * Lenses
    pscMaxRuntimeInSeconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a time limit for how long the processing job is allowed to run.
--
-- /See:/ 'mkProcessingStoppingCondition' smart constructor.
newtype ProcessingStoppingCondition = ProcessingStoppingCondition'
  { maxRuntimeInSeconds ::
      Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProcessingStoppingCondition' with the minimum fields required to make a request.
--
-- * 'maxRuntimeInSeconds' - Specifies the maximum runtime in seconds.
mkProcessingStoppingCondition ::
  -- | 'maxRuntimeInSeconds'
  Lude.Natural ->
  ProcessingStoppingCondition
mkProcessingStoppingCondition pMaxRuntimeInSeconds_ =
  ProcessingStoppingCondition'
    { maxRuntimeInSeconds =
        pMaxRuntimeInSeconds_
    }

-- | Specifies the maximum runtime in seconds.
--
-- /Note:/ Consider using 'maxRuntimeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pscMaxRuntimeInSeconds :: Lens.Lens' ProcessingStoppingCondition Lude.Natural
pscMaxRuntimeInSeconds = Lens.lens (maxRuntimeInSeconds :: ProcessingStoppingCondition -> Lude.Natural) (\s a -> s {maxRuntimeInSeconds = a} :: ProcessingStoppingCondition)
{-# DEPRECATED pscMaxRuntimeInSeconds "Use generic-lens or generic-optics with 'maxRuntimeInSeconds' instead." #-}

instance Lude.FromJSON ProcessingStoppingCondition where
  parseJSON =
    Lude.withObject
      "ProcessingStoppingCondition"
      ( \x ->
          ProcessingStoppingCondition'
            Lude.<$> (x Lude..: "MaxRuntimeInSeconds")
      )

instance Lude.ToJSON ProcessingStoppingCondition where
  toJSON ProcessingStoppingCondition' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("MaxRuntimeInSeconds" Lude..= maxRuntimeInSeconds)]
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.EvaluationMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.EvaluationMetrics
  ( EvaluationMetrics (..),

    -- * Smart constructor
    mkEvaluationMetrics,

    -- * Lenses
    emTransformType,
    emFindMatchesMetrics,
  )
where

import qualified Network.AWS.Glue.Types.FindMatchesMetrics as Types
import qualified Network.AWS.Glue.Types.TransformType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Evaluation metrics provide an estimate of the quality of your machine learning transform.
--
-- /See:/ 'mkEvaluationMetrics' smart constructor.
data EvaluationMetrics = EvaluationMetrics'
  { -- | The type of machine learning transform.
    transformType :: Types.TransformType,
    -- | The evaluation metrics for the find matches algorithm.
    findMatchesMetrics :: Core.Maybe Types.FindMatchesMetrics
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EvaluationMetrics' value with any optional fields omitted.
mkEvaluationMetrics ::
  -- | 'transformType'
  Types.TransformType ->
  EvaluationMetrics
mkEvaluationMetrics transformType =
  EvaluationMetrics'
    { transformType,
      findMatchesMetrics = Core.Nothing
    }

-- | The type of machine learning transform.
--
-- /Note:/ Consider using 'transformType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emTransformType :: Lens.Lens' EvaluationMetrics Types.TransformType
emTransformType = Lens.field @"transformType"
{-# DEPRECATED emTransformType "Use generic-lens or generic-optics with 'transformType' instead." #-}

-- | The evaluation metrics for the find matches algorithm.
--
-- /Note:/ Consider using 'findMatchesMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emFindMatchesMetrics :: Lens.Lens' EvaluationMetrics (Core.Maybe Types.FindMatchesMetrics)
emFindMatchesMetrics = Lens.field @"findMatchesMetrics"
{-# DEPRECATED emFindMatchesMetrics "Use generic-lens or generic-optics with 'findMatchesMetrics' instead." #-}

instance Core.FromJSON EvaluationMetrics where
  parseJSON =
    Core.withObject "EvaluationMetrics" Core.$
      \x ->
        EvaluationMetrics'
          Core.<$> (x Core..: "TransformType")
          Core.<*> (x Core..:? "FindMatchesMetrics")

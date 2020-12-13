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
    emFindMatchesMetrics,
    emTransformType,
  )
where

import Network.AWS.Glue.Types.FindMatchesMetrics
import Network.AWS.Glue.Types.TransformType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Evaluation metrics provide an estimate of the quality of your machine learning transform.
--
-- /See:/ 'mkEvaluationMetrics' smart constructor.
data EvaluationMetrics = EvaluationMetrics'
  { -- | The evaluation metrics for the find matches algorithm.
    findMatchesMetrics :: Lude.Maybe FindMatchesMetrics,
    -- | The type of machine learning transform.
    transformType :: TransformType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EvaluationMetrics' with the minimum fields required to make a request.
--
-- * 'findMatchesMetrics' - The evaluation metrics for the find matches algorithm.
-- * 'transformType' - The type of machine learning transform.
mkEvaluationMetrics ::
  -- | 'transformType'
  TransformType ->
  EvaluationMetrics
mkEvaluationMetrics pTransformType_ =
  EvaluationMetrics'
    { findMatchesMetrics = Lude.Nothing,
      transformType = pTransformType_
    }

-- | The evaluation metrics for the find matches algorithm.
--
-- /Note:/ Consider using 'findMatchesMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emFindMatchesMetrics :: Lens.Lens' EvaluationMetrics (Lude.Maybe FindMatchesMetrics)
emFindMatchesMetrics = Lens.lens (findMatchesMetrics :: EvaluationMetrics -> Lude.Maybe FindMatchesMetrics) (\s a -> s {findMatchesMetrics = a} :: EvaluationMetrics)
{-# DEPRECATED emFindMatchesMetrics "Use generic-lens or generic-optics with 'findMatchesMetrics' instead." #-}

-- | The type of machine learning transform.
--
-- /Note:/ Consider using 'transformType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emTransformType :: Lens.Lens' EvaluationMetrics TransformType
emTransformType = Lens.lens (transformType :: EvaluationMetrics -> TransformType) (\s a -> s {transformType = a} :: EvaluationMetrics)
{-# DEPRECATED emTransformType "Use generic-lens or generic-optics with 'transformType' instead." #-}

instance Lude.FromJSON EvaluationMetrics where
  parseJSON =
    Lude.withObject
      "EvaluationMetrics"
      ( \x ->
          EvaluationMetrics'
            Lude.<$> (x Lude..:? "FindMatchesMetrics")
            Lude.<*> (x Lude..: "TransformType")
      )

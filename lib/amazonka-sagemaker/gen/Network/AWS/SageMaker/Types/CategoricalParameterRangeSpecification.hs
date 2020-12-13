{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CategoricalParameterRangeSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CategoricalParameterRangeSpecification
  ( CategoricalParameterRangeSpecification (..),

    -- * Smart constructor
    mkCategoricalParameterRangeSpecification,

    -- * Lenses
    cprsValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines the possible values for a categorical hyperparameter.
--
-- /See:/ 'mkCategoricalParameterRangeSpecification' smart constructor.
newtype CategoricalParameterRangeSpecification = CategoricalParameterRangeSpecification'
  { -- | The allowed categories for the hyperparameter.
    values :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CategoricalParameterRangeSpecification' with the minimum fields required to make a request.
--
-- * 'values' - The allowed categories for the hyperparameter.
mkCategoricalParameterRangeSpecification ::
  -- | 'values'
  Lude.NonEmpty Lude.Text ->
  CategoricalParameterRangeSpecification
mkCategoricalParameterRangeSpecification pValues_ =
  CategoricalParameterRangeSpecification' {values = pValues_}

-- | The allowed categories for the hyperparameter.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsValues :: Lens.Lens' CategoricalParameterRangeSpecification (Lude.NonEmpty Lude.Text)
cprsValues = Lens.lens (values :: CategoricalParameterRangeSpecification -> Lude.NonEmpty Lude.Text) (\s a -> s {values = a} :: CategoricalParameterRangeSpecification)
{-# DEPRECATED cprsValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Lude.FromJSON CategoricalParameterRangeSpecification where
  parseJSON =
    Lude.withObject
      "CategoricalParameterRangeSpecification"
      ( \x ->
          CategoricalParameterRangeSpecification'
            Lude.<$> (x Lude..: "Values")
      )

instance Lude.ToJSON CategoricalParameterRangeSpecification where
  toJSON CategoricalParameterRangeSpecification' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Values" Lude..= values)])

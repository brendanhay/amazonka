{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CategoricalParameterRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CategoricalParameterRange
  ( CategoricalParameterRange (..),

    -- * Smart constructor
    mkCategoricalParameterRange,

    -- * Lenses
    cprValues,
    cprName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of categorical hyperparameters to tune.
--
-- /See:/ 'mkCategoricalParameterRange' smart constructor.
data CategoricalParameterRange = CategoricalParameterRange'
  { -- | A list of the categories for the hyperparameter.
    values :: Lude.NonEmpty Lude.Text,
    -- | The name of the categorical hyperparameter to tune.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CategoricalParameterRange' with the minimum fields required to make a request.
--
-- * 'values' - A list of the categories for the hyperparameter.
-- * 'name' - The name of the categorical hyperparameter to tune.
mkCategoricalParameterRange ::
  -- | 'values'
  Lude.NonEmpty Lude.Text ->
  -- | 'name'
  Lude.Text ->
  CategoricalParameterRange
mkCategoricalParameterRange pValues_ pName_ =
  CategoricalParameterRange' {values = pValues_, name = pName_}

-- | A list of the categories for the hyperparameter.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprValues :: Lens.Lens' CategoricalParameterRange (Lude.NonEmpty Lude.Text)
cprValues = Lens.lens (values :: CategoricalParameterRange -> Lude.NonEmpty Lude.Text) (\s a -> s {values = a} :: CategoricalParameterRange)
{-# DEPRECATED cprValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The name of the categorical hyperparameter to tune.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprName :: Lens.Lens' CategoricalParameterRange Lude.Text
cprName = Lens.lens (name :: CategoricalParameterRange -> Lude.Text) (\s a -> s {name = a} :: CategoricalParameterRange)
{-# DEPRECATED cprName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON CategoricalParameterRange where
  parseJSON =
    Lude.withObject
      "CategoricalParameterRange"
      ( \x ->
          CategoricalParameterRange'
            Lude.<$> (x Lude..: "Values") Lude.<*> (x Lude..: "Name")
      )

instance Lude.ToJSON CategoricalParameterRange where
  toJSON CategoricalParameterRange' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Values" Lude..= values),
            Lude.Just ("Name" Lude..= name)
          ]
      )

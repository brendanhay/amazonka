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
    cprName,
    cprValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ParameterKey as Types
import qualified Network.AWS.SageMaker.Types.ParameterValue as Types

-- | A list of categorical hyperparameters to tune.
--
-- /See:/ 'mkCategoricalParameterRange' smart constructor.
data CategoricalParameterRange = CategoricalParameterRange'
  { -- | The name of the categorical hyperparameter to tune.
    name :: Types.ParameterKey,
    -- | A list of the categories for the hyperparameter.
    values :: Core.NonEmpty Types.ParameterValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CategoricalParameterRange' value with any optional fields omitted.
mkCategoricalParameterRange ::
  -- | 'name'
  Types.ParameterKey ->
  -- | 'values'
  Core.NonEmpty Types.ParameterValue ->
  CategoricalParameterRange
mkCategoricalParameterRange name values =
  CategoricalParameterRange' {name, values}

-- | The name of the categorical hyperparameter to tune.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprName :: Lens.Lens' CategoricalParameterRange Types.ParameterKey
cprName = Lens.field @"name"
{-# DEPRECATED cprName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of the categories for the hyperparameter.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprValues :: Lens.Lens' CategoricalParameterRange (Core.NonEmpty Types.ParameterValue)
cprValues = Lens.field @"values"
{-# DEPRECATED cprValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromJSON CategoricalParameterRange where
  toJSON CategoricalParameterRange {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Values" Core..= values)
          ]
      )

instance Core.FromJSON CategoricalParameterRange where
  parseJSON =
    Core.withObject "CategoricalParameterRange" Core.$
      \x ->
        CategoricalParameterRange'
          Core.<$> (x Core..: "Name") Core.<*> (x Core..: "Values")

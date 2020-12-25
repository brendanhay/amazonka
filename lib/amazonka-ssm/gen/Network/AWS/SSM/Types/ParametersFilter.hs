{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParametersFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ParametersFilter
  ( ParametersFilter (..),

    -- * Smart constructor
    mkParametersFilter,

    -- * Lenses
    pKey,
    pValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.ParametersFilterKey as Types
import qualified Network.AWS.SSM.Types.ParametersFilterValue as Types

-- | This data type is deprecated. Instead, use 'ParameterStringFilter' .
--
-- /See:/ 'mkParametersFilter' smart constructor.
data ParametersFilter = ParametersFilter'
  { -- | The name of the filter.
    key :: Types.ParametersFilterKey,
    -- | The filter values.
    values :: Core.NonEmpty Types.ParametersFilterValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParametersFilter' value with any optional fields omitted.
mkParametersFilter ::
  -- | 'key'
  Types.ParametersFilterKey ->
  -- | 'values'
  Core.NonEmpty Types.ParametersFilterValue ->
  ParametersFilter
mkParametersFilter key values = ParametersFilter' {key, values}

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pKey :: Lens.Lens' ParametersFilter Types.ParametersFilterKey
pKey = Lens.field @"key"
{-# DEPRECATED pKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The filter values.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pValues :: Lens.Lens' ParametersFilter (Core.NonEmpty Types.ParametersFilterValue)
pValues = Lens.field @"values"
{-# DEPRECATED pValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromJSON ParametersFilter where
  toJSON ParametersFilter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Key" Core..= key),
            Core.Just ("Values" Core..= values)
          ]
      )

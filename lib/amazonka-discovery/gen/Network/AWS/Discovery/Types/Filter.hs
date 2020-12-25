{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.Filter
  ( Filter (..),

    -- * Smart constructor
    mkFilter,

    -- * Lenses
    fName,
    fValues,
    fCondition,
  )
where

import qualified Network.AWS.Discovery.Types.Condition as Types
import qualified Network.AWS.Discovery.Types.FilterValue as Types
import qualified Network.AWS.Discovery.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A filter that can use conditional operators.
--
-- For more information about filters, see <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html Querying Discovered Configuration Items> in the /AWS Application Discovery Service User Guide/ .
--
-- /See:/ 'mkFilter' smart constructor.
data Filter = Filter'
  { -- | The name of the filter.
    name :: Types.String,
    -- | A string value on which to filter. For example, if you choose the @destinationServer.osVersion@ filter name, you could specify @Ubuntu@ for the value.
    values :: [Types.FilterValue],
    -- | A conditional operator. The following operators are valid: EQUALS, NOT_EQUALS, CONTAINS, NOT_CONTAINS. If you specify multiple filters, the system utilizes all filters as though concatenated by /AND/ . If you specify multiple values for a particular filter, the system differentiates the values using /OR/ . Calling either /DescribeConfigurations/ or /ListConfigurations/ returns attributes of matching configuration items.
    condition :: Types.Condition
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Filter' value with any optional fields omitted.
mkFilter ::
  -- | 'name'
  Types.String ->
  -- | 'condition'
  Types.Condition ->
  Filter
mkFilter name condition =
  Filter' {name, values = Core.mempty, condition}

-- | The name of the filter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fName :: Lens.Lens' Filter Types.String
fName = Lens.field @"name"
{-# DEPRECATED fName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A string value on which to filter. For example, if you choose the @destinationServer.osVersion@ filter name, you could specify @Ubuntu@ for the value.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fValues :: Lens.Lens' Filter [Types.FilterValue]
fValues = Lens.field @"values"
{-# DEPRECATED fValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | A conditional operator. The following operators are valid: EQUALS, NOT_EQUALS, CONTAINS, NOT_CONTAINS. If you specify multiple filters, the system utilizes all filters as though concatenated by /AND/ . If you specify multiple values for a particular filter, the system differentiates the values using /OR/ . Calling either /DescribeConfigurations/ or /ListConfigurations/ returns attributes of matching configuration items.
--
-- /Note:/ Consider using 'condition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fCondition :: Lens.Lens' Filter Types.Condition
fCondition = Lens.field @"condition"
{-# DEPRECATED fCondition "Use generic-lens or generic-optics with 'condition' instead." #-}

instance Core.FromJSON Filter where
  toJSON Filter {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("values" Core..= values),
            Core.Just ("condition" Core..= condition)
          ]
      )

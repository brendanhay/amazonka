{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NestedFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NestedFilters
  ( NestedFilters (..),

    -- * Smart constructor
    mkNestedFilters,

    -- * Lenses
    nfNestedPropertyName,
    nfFilters,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.Filter as Types
import qualified Network.AWS.SageMaker.Types.ResourcePropertyName as Types

-- | A list of nested 'Filter' objects. A resource must satisfy the conditions of all filters to be included in the results returned from the 'Search' API.
--
-- For example, to filter on a training job's @InputDataConfig@ property with a specific channel name and @S3Uri@ prefix, define the following filters:
--
--     * @'{Name:"InputDataConfig.ChannelName", "Operator":"Equals", "Value":"train"}',@
--
--
--     * @'{Name:"InputDataConfig.DataSource.S3DataSource.S3Uri", "Operator":"Contains", "Value":"mybucket/catdata"}'@
--
--
--
-- /See:/ 'mkNestedFilters' smart constructor.
data NestedFilters = NestedFilters'
  { -- | The name of the property to use in the nested filters. The value must match a listed property name, such as @InputDataConfig@ .
    nestedPropertyName :: Types.ResourcePropertyName,
    -- | A list of filters. Each filter acts on a property. Filters must contain at least one @Filters@ value. For example, a @NestedFilters@ call might include a filter on the @PropertyName@ parameter of the @InputDataConfig@ property: @InputDataConfig.DataSource.S3DataSource.S3Uri@ .
    filters :: Core.NonEmpty Types.Filter
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NestedFilters' value with any optional fields omitted.
mkNestedFilters ::
  -- | 'nestedPropertyName'
  Types.ResourcePropertyName ->
  -- | 'filters'
  Core.NonEmpty Types.Filter ->
  NestedFilters
mkNestedFilters nestedPropertyName filters =
  NestedFilters' {nestedPropertyName, filters}

-- | The name of the property to use in the nested filters. The value must match a listed property name, such as @InputDataConfig@ .
--
-- /Note:/ Consider using 'nestedPropertyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfNestedPropertyName :: Lens.Lens' NestedFilters Types.ResourcePropertyName
nfNestedPropertyName = Lens.field @"nestedPropertyName"
{-# DEPRECATED nfNestedPropertyName "Use generic-lens or generic-optics with 'nestedPropertyName' instead." #-}

-- | A list of filters. Each filter acts on a property. Filters must contain at least one @Filters@ value. For example, a @NestedFilters@ call might include a filter on the @PropertyName@ parameter of the @InputDataConfig@ property: @InputDataConfig.DataSource.S3DataSource.S3Uri@ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfFilters :: Lens.Lens' NestedFilters (Core.NonEmpty Types.Filter)
nfFilters = Lens.field @"filters"
{-# DEPRECATED nfFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

instance Core.FromJSON NestedFilters where
  toJSON NestedFilters {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("NestedPropertyName" Core..= nestedPropertyName),
            Core.Just ("Filters" Core..= filters)
          ]
      )

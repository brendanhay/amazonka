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
    nfFilters,
    nfNestedPropertyName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.Filter

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
  { -- | A list of filters. Each filter acts on a property. Filters must contain at least one @Filters@ value. For example, a @NestedFilters@ call might include a filter on the @PropertyName@ parameter of the @InputDataConfig@ property: @InputDataConfig.DataSource.S3DataSource.S3Uri@ .
    filters :: Lude.NonEmpty Filter,
    -- | The name of the property to use in the nested filters. The value must match a listed property name, such as @InputDataConfig@ .
    nestedPropertyName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NestedFilters' with the minimum fields required to make a request.
--
-- * 'filters' - A list of filters. Each filter acts on a property. Filters must contain at least one @Filters@ value. For example, a @NestedFilters@ call might include a filter on the @PropertyName@ parameter of the @InputDataConfig@ property: @InputDataConfig.DataSource.S3DataSource.S3Uri@ .
-- * 'nestedPropertyName' - The name of the property to use in the nested filters. The value must match a listed property name, such as @InputDataConfig@ .
mkNestedFilters ::
  -- | 'filters'
  Lude.NonEmpty Filter ->
  -- | 'nestedPropertyName'
  Lude.Text ->
  NestedFilters
mkNestedFilters pFilters_ pNestedPropertyName_ =
  NestedFilters'
    { filters = pFilters_,
      nestedPropertyName = pNestedPropertyName_
    }

-- | A list of filters. Each filter acts on a property. Filters must contain at least one @Filters@ value. For example, a @NestedFilters@ call might include a filter on the @PropertyName@ parameter of the @InputDataConfig@ property: @InputDataConfig.DataSource.S3DataSource.S3Uri@ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfFilters :: Lens.Lens' NestedFilters (Lude.NonEmpty Filter)
nfFilters = Lens.lens (filters :: NestedFilters -> Lude.NonEmpty Filter) (\s a -> s {filters = a} :: NestedFilters)
{-# DEPRECATED nfFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The name of the property to use in the nested filters. The value must match a listed property name, such as @InputDataConfig@ .
--
-- /Note:/ Consider using 'nestedPropertyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nfNestedPropertyName :: Lens.Lens' NestedFilters Lude.Text
nfNestedPropertyName = Lens.lens (nestedPropertyName :: NestedFilters -> Lude.Text) (\s a -> s {nestedPropertyName = a} :: NestedFilters)
{-# DEPRECATED nfNestedPropertyName "Use generic-lens or generic-optics with 'nestedPropertyName' instead." #-}

instance Lude.ToJSON NestedFilters where
  toJSON NestedFilters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Filters" Lude..= filters),
            Lude.Just ("NestedPropertyName" Lude..= nestedPropertyName)
          ]
      )

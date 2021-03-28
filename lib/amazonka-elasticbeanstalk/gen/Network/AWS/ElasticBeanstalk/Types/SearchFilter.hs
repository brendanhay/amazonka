{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.SearchFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.SearchFilter
  ( SearchFilter (..)
  -- * Smart constructor
  , mkSearchFilter
  -- * Lenses
  , sfAttribute
  , sfOperator
  , sfValues
  ) where

import qualified Network.AWS.ElasticBeanstalk.Types.Attribute as Types
import qualified Network.AWS.ElasticBeanstalk.Types.Operator as Types
import qualified Network.AWS.ElasticBeanstalk.Types.SearchFilterValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes criteria to restrict a list of results.
--
-- For operators that apply a single value to the attribute, the filter is evaluated as follows: @Attribute Operator Values[1]@ 
-- Some operators, e.g. @in@ , can apply multiple values. In this case, the filter is evaluated as a logical union (OR) of applications of the operator to the attribute with each one of the values: @(Attribute Operator Values[1]) OR (Attribute Operator Values[2]) OR ...@ 
-- The valid values for attributes of @SearchFilter@ depend on the API action. For valid values, see the reference page for the API action you're calling that takes a @SearchFilter@ parameter.
--
-- /See:/ 'mkSearchFilter' smart constructor.
data SearchFilter = SearchFilter'
  { attribute :: Core.Maybe Types.Attribute
    -- ^ The result attribute to which the filter values are applied. Valid values vary by API action.
  , operator :: Core.Maybe Types.Operator
    -- ^ The operator to apply to the @Attribute@ with each of the @Values@ . Valid values vary by @Attribute@ .
  , values :: Core.Maybe [Types.SearchFilterValue]
    -- ^ The list of values applied to the @Attribute@ and @Operator@ attributes. Number of values and valid values vary by @Attribute@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SearchFilter' value with any optional fields omitted.
mkSearchFilter
    :: SearchFilter
mkSearchFilter
  = SearchFilter'{attribute = Core.Nothing, operator = Core.Nothing,
                  values = Core.Nothing}

-- | The result attribute to which the filter values are applied. Valid values vary by API action.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfAttribute :: Lens.Lens' SearchFilter (Core.Maybe Types.Attribute)
sfAttribute = Lens.field @"attribute"
{-# INLINEABLE sfAttribute #-}
{-# DEPRECATED attribute "Use generic-lens or generic-optics with 'attribute' instead"  #-}

-- | The operator to apply to the @Attribute@ with each of the @Values@ . Valid values vary by @Attribute@ .
--
-- /Note:/ Consider using 'operator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfOperator :: Lens.Lens' SearchFilter (Core.Maybe Types.Operator)
sfOperator = Lens.field @"operator"
{-# INLINEABLE sfOperator #-}
{-# DEPRECATED operator "Use generic-lens or generic-optics with 'operator' instead"  #-}

-- | The list of values applied to the @Attribute@ and @Operator@ attributes. Number of values and valid values vary by @Attribute@ .
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfValues :: Lens.Lens' SearchFilter (Core.Maybe [Types.SearchFilterValue])
sfValues = Lens.field @"values"
{-# INLINEABLE sfValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.ToQuery SearchFilter where
        toQuery SearchFilter{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Attribute") attribute
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Operator") operator
              Core.<>
              Core.toQueryPair "Values"
                (Core.maybe Core.mempty (Core.toQueryList "member") values)

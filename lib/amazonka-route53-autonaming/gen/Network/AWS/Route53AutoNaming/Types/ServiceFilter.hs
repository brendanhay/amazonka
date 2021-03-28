{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.ServiceFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.ServiceFilter
  ( ServiceFilter (..)
  -- * Smart constructor
  , mkServiceFilter
  -- * Lenses
  , sfName
  , sfValues
  , sfCondition
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53AutoNaming.Types.FilterCondition as Types
import qualified Network.AWS.Route53AutoNaming.Types.FilterValue as Types
import qualified Network.AWS.Route53AutoNaming.Types.ServiceFilterName as Types

-- | A complex type that lets you specify the namespaces that you want to list services for.
--
-- /See:/ 'mkServiceFilter' smart constructor.
data ServiceFilter = ServiceFilter'
  { name :: Types.ServiceFilterName
    -- ^ Specify @NAMESPACE_ID@ .
  , values :: [Types.FilterValue]
    -- ^ The values that are applicable to the value that you specify for @Condition@ to filter the list of services.
  , condition :: Core.Maybe Types.FilterCondition
    -- ^ The operator that you want to use to determine whether a service is returned by @ListServices@ . Valid values for @Condition@ include the following:
--
--
--     * @EQ@ : When you specify @EQ@ , specify one namespace ID for @Values@ . @EQ@ is the default condition and can be omitted.
--
--
--     * @IN@ : When you specify @IN@ , specify a list of the IDs for the namespaces that you want @ListServices@ to return a list of services for.
--
--
--     * @BETWEEN@ : Not applicable.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServiceFilter' value with any optional fields omitted.
mkServiceFilter
    :: Types.ServiceFilterName -- ^ 'name'
    -> ServiceFilter
mkServiceFilter name
  = ServiceFilter'{name, values = Core.mempty,
                   condition = Core.Nothing}

-- | Specify @NAMESPACE_ID@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfName :: Lens.Lens' ServiceFilter Types.ServiceFilterName
sfName = Lens.field @"name"
{-# INLINEABLE sfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The values that are applicable to the value that you specify for @Condition@ to filter the list of services.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfValues :: Lens.Lens' ServiceFilter [Types.FilterValue]
sfValues = Lens.field @"values"
{-# INLINEABLE sfValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

-- | The operator that you want to use to determine whether a service is returned by @ListServices@ . Valid values for @Condition@ include the following:
--
--
--     * @EQ@ : When you specify @EQ@ , specify one namespace ID for @Values@ . @EQ@ is the default condition and can be omitted.
--
--
--     * @IN@ : When you specify @IN@ , specify a list of the IDs for the namespaces that you want @ListServices@ to return a list of services for.
--
--
--     * @BETWEEN@ : Not applicable.
--
--
--
-- /Note:/ Consider using 'condition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfCondition :: Lens.Lens' ServiceFilter (Core.Maybe Types.FilterCondition)
sfCondition = Lens.field @"condition"
{-# INLINEABLE sfCondition #-}
{-# DEPRECATED condition "Use generic-lens or generic-optics with 'condition' instead"  #-}

instance Core.FromJSON ServiceFilter where
        toJSON ServiceFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("Values" Core..= values),
                  ("Condition" Core..=) Core.<$> condition])

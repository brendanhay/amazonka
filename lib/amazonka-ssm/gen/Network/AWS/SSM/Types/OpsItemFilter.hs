{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItemFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.OpsItemFilter
  ( OpsItemFilter (..)
  -- * Smart constructor
  , mkOpsItemFilter
  -- * Lenses
  , oifKey
  , oifValues
  , oifOperator
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.OpsItemFilterKey as Types
import qualified Network.AWS.SSM.Types.OpsItemFilterOperator as Types
import qualified Network.AWS.SSM.Types.OpsItemFilterValue as Types

-- | Describes an OpsItem filter.
--
-- /See:/ 'mkOpsItemFilter' smart constructor.
data OpsItemFilter = OpsItemFilter'
  { key :: Types.OpsItemFilterKey
    -- ^ The name of the filter.
  , values :: [Types.OpsItemFilterValue]
    -- ^ The filter value.
  , operator :: Types.OpsItemFilterOperator
    -- ^ The operator used by the filter call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OpsItemFilter' value with any optional fields omitted.
mkOpsItemFilter
    :: Types.OpsItemFilterKey -- ^ 'key'
    -> Types.OpsItemFilterOperator -- ^ 'operator'
    -> OpsItemFilter
mkOpsItemFilter key operator
  = OpsItemFilter'{key, values = Core.mempty, operator}

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oifKey :: Lens.Lens' OpsItemFilter Types.OpsItemFilterKey
oifKey = Lens.field @"key"
{-# INLINEABLE oifKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The filter value.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oifValues :: Lens.Lens' OpsItemFilter [Types.OpsItemFilterValue]
oifValues = Lens.field @"values"
{-# INLINEABLE oifValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

-- | The operator used by the filter call.
--
-- /Note:/ Consider using 'operator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oifOperator :: Lens.Lens' OpsItemFilter Types.OpsItemFilterOperator
oifOperator = Lens.field @"operator"
{-# INLINEABLE oifOperator #-}
{-# DEPRECATED operator "Use generic-lens or generic-optics with 'operator' instead"  #-}

instance Core.FromJSON OpsItemFilter where
        toJSON OpsItemFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Key" Core..= key),
                  Core.Just ("Values" Core..= values),
                  Core.Just ("Operator" Core..= operator)])

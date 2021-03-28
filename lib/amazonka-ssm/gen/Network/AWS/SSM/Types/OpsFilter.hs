{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.OpsFilter
  ( OpsFilter (..)
  -- * Smart constructor
  , mkOpsFilter
  -- * Lenses
  , ofKey
  , ofValues
  , ofType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.OpsFilterKey as Types
import qualified Network.AWS.SSM.Types.OpsFilterOperatorType as Types
import qualified Network.AWS.SSM.Types.OpsFilterValue as Types

-- | A filter for viewing OpsItem summaries.
--
-- /See:/ 'mkOpsFilter' smart constructor.
data OpsFilter = OpsFilter'
  { key :: Types.OpsFilterKey
    -- ^ The name of the filter.
  , values :: Core.NonEmpty Types.OpsFilterValue
    -- ^ The filter value.
  , type' :: Core.Maybe Types.OpsFilterOperatorType
    -- ^ The type of filter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OpsFilter' value with any optional fields omitted.
mkOpsFilter
    :: Types.OpsFilterKey -- ^ 'key'
    -> Core.NonEmpty Types.OpsFilterValue -- ^ 'values'
    -> OpsFilter
mkOpsFilter key values
  = OpsFilter'{key, values, type' = Core.Nothing}

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofKey :: Lens.Lens' OpsFilter Types.OpsFilterKey
ofKey = Lens.field @"key"
{-# INLINEABLE ofKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The filter value.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofValues :: Lens.Lens' OpsFilter (Core.NonEmpty Types.OpsFilterValue)
ofValues = Lens.field @"values"
{-# INLINEABLE ofValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

-- | The type of filter.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofType :: Lens.Lens' OpsFilter (Core.Maybe Types.OpsFilterOperatorType)
ofType = Lens.field @"type'"
{-# INLINEABLE ofType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON OpsFilter where
        toJSON OpsFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Key" Core..= key),
                  Core.Just ("Values" Core..= values),
                  ("Type" Core..=) Core.<$> type'])

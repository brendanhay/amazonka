{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackInstanceFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.StackInstanceFilter
  ( StackInstanceFilter (..)
  -- * Smart constructor
  , mkStackInstanceFilter
  -- * Lenses
  , sifName
  , sifValues
  ) where

import qualified Network.AWS.CloudFormation.Types.StackInstanceFilterName as Types
import qualified Network.AWS.CloudFormation.Types.Values as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The status that stack instances are filtered by.
--
-- /See:/ 'mkStackInstanceFilter' smart constructor.
data StackInstanceFilter = StackInstanceFilter'
  { name :: Core.Maybe Types.StackInstanceFilterName
    -- ^ The type of filter to apply.
  , values :: Core.Maybe Types.Values
    -- ^ The status to filter by.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StackInstanceFilter' value with any optional fields omitted.
mkStackInstanceFilter
    :: StackInstanceFilter
mkStackInstanceFilter
  = StackInstanceFilter'{name = Core.Nothing, values = Core.Nothing}

-- | The type of filter to apply.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sifName :: Lens.Lens' StackInstanceFilter (Core.Maybe Types.StackInstanceFilterName)
sifName = Lens.field @"name"
{-# INLINEABLE sifName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The status to filter by.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sifValues :: Lens.Lens' StackInstanceFilter (Core.Maybe Types.Values)
sifValues = Lens.field @"values"
{-# INLINEABLE sifValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.ToQuery StackInstanceFilter where
        toQuery StackInstanceFilter{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Name") name Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Values") values

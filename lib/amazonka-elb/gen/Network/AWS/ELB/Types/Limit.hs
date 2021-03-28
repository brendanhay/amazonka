{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.Limit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELB.Types.Limit
  ( Limit (..)
  -- * Smart constructor
  , mkLimit
  -- * Lenses
  , lMax
  , lName
  ) where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.ELB.Types.Max as Types
import qualified Network.AWS.ELB.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an Elastic Load Balancing resource limit for your AWS account.
--
-- /See:/ 'mkLimit' smart constructor.
data Limit = Limit'
  { max :: Core.Maybe Types.Max
    -- ^ The maximum value of the limit.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the limit. The possible values are:
--
--
--     * classic-listeners
--
--
--     * classic-load-balancers
--
--
--     * classic-registered-instances
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Limit' value with any optional fields omitted.
mkLimit
    :: Limit
mkLimit = Limit'{max = Core.Nothing, name = Core.Nothing}

-- | The maximum value of the limit.
--
-- /Note:/ Consider using 'max' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMax :: Lens.Lens' Limit (Core.Maybe Types.Max)
lMax = Lens.field @"max"
{-# INLINEABLE lMax #-}
{-# DEPRECATED max "Use generic-lens or generic-optics with 'max' instead"  #-}

-- | The name of the limit. The possible values are:
--
--
--     * classic-listeners
--
--
--     * classic-load-balancers
--
--
--     * classic-registered-instances
--
--
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lName :: Lens.Lens' Limit (Core.Maybe Types.Name)
lName = Lens.field @"name"
{-# INLINEABLE lName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromXML Limit where
        parseXML x
          = Limit' Core.<$> (x Core..@? "Max") Core.<*> x Core..@? "Name"

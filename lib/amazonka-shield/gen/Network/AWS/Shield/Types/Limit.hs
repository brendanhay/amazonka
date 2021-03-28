{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.Limit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.Limit
  ( Limit (..)
  -- * Smart constructor
  , mkLimit
  -- * Lenses
  , lMax
  , lType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies how many protections of a given type you can create.
--
-- /See:/ 'mkLimit' smart constructor.
data Limit = Limit'
  { max :: Core.Maybe Core.Integer
    -- ^ The maximum number of protections that can be created for the specified @Type@ .
  , type' :: Core.Maybe Core.Text
    -- ^ The type of protection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Limit' value with any optional fields omitted.
mkLimit
    :: Limit
mkLimit = Limit'{max = Core.Nothing, type' = Core.Nothing}

-- | The maximum number of protections that can be created for the specified @Type@ .
--
-- /Note:/ Consider using 'max' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMax :: Lens.Lens' Limit (Core.Maybe Core.Integer)
lMax = Lens.field @"max"
{-# INLINEABLE lMax #-}
{-# DEPRECATED max "Use generic-lens or generic-optics with 'max' instead"  #-}

-- | The type of protection.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lType :: Lens.Lens' Limit (Core.Maybe Core.Text)
lType = Lens.field @"type'"
{-# INLINEABLE lType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON Limit where
        parseJSON
          = Core.withObject "Limit" Core.$
              \ x ->
                Limit' Core.<$> (x Core..:? "Max") Core.<*> x Core..:? "Type"

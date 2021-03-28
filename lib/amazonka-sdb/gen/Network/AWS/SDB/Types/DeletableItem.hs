{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.DeletableItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SDB.Types.DeletableItem
  ( DeletableItem (..)
  -- * Smart constructor
  , mkDeletableItem
  -- * Lenses
  , diName
  , diAttributes
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SDB.Types.Attribute as Types

-- | /See:/ 'mkDeletableItem' smart constructor.
data DeletableItem = DeletableItem'
  { name :: Core.Text
  , attributes :: Core.Maybe [Types.Attribute]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletableItem' value with any optional fields omitted.
mkDeletableItem
    :: Core.Text -- ^ 'name'
    -> DeletableItem
mkDeletableItem name
  = DeletableItem'{name, attributes = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diName :: Lens.Lens' DeletableItem Core.Text
diName = Lens.field @"name"
{-# INLINEABLE diName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diAttributes :: Lens.Lens' DeletableItem (Core.Maybe [Types.Attribute])
diAttributes = Lens.field @"attributes"
{-# INLINEABLE diAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

instance Core.ToQuery DeletableItem where
        toQuery DeletableItem{..}
          = Core.toQueryPair "ItemName" name Core.<>
              Core.maybe Core.mempty (Core.toQueryList "Attribute") attributes

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.Item
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SDB.Types.Item
  ( Item (..)
  -- * Smart constructor
  , mkItem
  -- * Lenses
  , iName
  , iAttributes
  , iAlternateNameEncoding
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SDB.Types.Attribute as Types

-- | 
--
-- /See:/ 'mkItem' smart constructor.
data Item = Item'
  { name :: Core.Text
    -- ^ The name of the item.
  , attributes :: [Types.Attribute]
    -- ^ A list of attributes.
  , alternateNameEncoding :: Core.Maybe Core.Text
    -- ^ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Item' value with any optional fields omitted.
mkItem
    :: Core.Text -- ^ 'name'
    -> Item
mkItem name
  = Item'{name, attributes = Core.mempty,
          alternateNameEncoding = Core.Nothing}

-- | The name of the item.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iName :: Lens.Lens' Item Core.Text
iName = Lens.field @"name"
{-# INLINEABLE iName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A list of attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAttributes :: Lens.Lens' Item [Types.Attribute]
iAttributes = Lens.field @"attributes"
{-# INLINEABLE iAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'alternateNameEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAlternateNameEncoding :: Lens.Lens' Item (Core.Maybe Core.Text)
iAlternateNameEncoding = Lens.field @"alternateNameEncoding"
{-# INLINEABLE iAlternateNameEncoding #-}
{-# DEPRECATED alternateNameEncoding "Use generic-lens or generic-optics with 'alternateNameEncoding' instead"  #-}

instance Core.FromXML Item where
        parseXML x
          = Item' Core.<$>
              (x Core..@ "Name") Core.<*>
                x Core..@ "Attribute" Core..@! Core.mempty
                Core.<*> x Core..@? "AlternateNameEncoding"

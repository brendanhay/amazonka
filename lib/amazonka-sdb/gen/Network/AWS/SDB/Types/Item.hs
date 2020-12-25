{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.Item
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.Item
  ( Item (..),

    -- * Smart constructor
    mkItem,

    -- * Lenses
    iName,
    iAttributes,
    iAlternateNameEncoding,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SDB.Types.Attribute as Types
import qualified Network.AWS.SDB.Types.String as Types

-- |
--
-- /See:/ 'mkItem' smart constructor.
data Item = Item'
  { -- | The name of the item.
    name :: Types.String,
    -- | A list of attributes.
    attributes :: [Types.Attribute],
    -- |
    alternateNameEncoding :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Item' value with any optional fields omitted.
mkItem ::
  -- | 'name'
  Types.String ->
  Item
mkItem name =
  Item'
    { name,
      attributes = Core.mempty,
      alternateNameEncoding = Core.Nothing
    }

-- | The name of the item.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iName :: Lens.Lens' Item Types.String
iName = Lens.field @"name"
{-# DEPRECATED iName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAttributes :: Lens.Lens' Item [Types.Attribute]
iAttributes = Lens.field @"attributes"
{-# DEPRECATED iAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- |
--
-- /Note:/ Consider using 'alternateNameEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAlternateNameEncoding :: Lens.Lens' Item (Core.Maybe Types.String)
iAlternateNameEncoding = Lens.field @"alternateNameEncoding"
{-# DEPRECATED iAlternateNameEncoding "Use generic-lens or generic-optics with 'alternateNameEncoding' instead." #-}

instance Core.FromXML Item where
  parseXML x =
    Item'
      Core.<$> (x Core..@ "Name")
      Core.<*> (x Core..@? "Attribute" Core..@! Core.mempty)
      Core.<*> (x Core..@? "AlternateNameEncoding")

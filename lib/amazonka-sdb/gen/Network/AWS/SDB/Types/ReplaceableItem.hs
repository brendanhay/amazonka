{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.ReplaceableItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.ReplaceableItem
  ( ReplaceableItem (..),

    -- * Smart constructor
    mkReplaceableItem,

    -- * Lenses
    riName,
    riAttributes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SDB.Types.ReplaceableAttribute as Types
import qualified Network.AWS.SDB.Types.String as Types

-- |
--
-- /See:/ 'mkReplaceableItem' smart constructor.
data ReplaceableItem = ReplaceableItem'
  { -- | The name of the replaceable item.
    name :: Types.String,
    -- | The list of attributes for a replaceable item.
    attributes :: [Types.ReplaceableAttribute]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplaceableItem' value with any optional fields omitted.
mkReplaceableItem ::
  -- | 'name'
  Types.String ->
  ReplaceableItem
mkReplaceableItem name =
  ReplaceableItem' {name, attributes = Core.mempty}

-- | The name of the replaceable item.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riName :: Lens.Lens' ReplaceableItem Types.String
riName = Lens.field @"name"
{-# DEPRECATED riName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The list of attributes for a replaceable item.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riAttributes :: Lens.Lens' ReplaceableItem [Types.ReplaceableAttribute]
riAttributes = Lens.field @"attributes"
{-# DEPRECATED riAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

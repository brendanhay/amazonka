{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Alias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.Alias
  ( Alias (..),

    -- * Smart constructor
    mkAlias,

    -- * Lenses
    aName,
    aNames,
    aType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.String as Types

-- | An alias for an edge.
--
-- /See:/ 'mkAlias' smart constructor.
data Alias = Alias'
  { -- | The canonical name of the alias.
    name :: Core.Maybe Types.String,
    -- | A list of names for the alias, including the canonical name.
    names :: Core.Maybe [Types.String],
    -- | The type of the alias.
    type' :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Alias' value with any optional fields omitted.
mkAlias ::
  Alias
mkAlias =
  Alias'
    { name = Core.Nothing,
      names = Core.Nothing,
      type' = Core.Nothing
    }

-- | The canonical name of the alias.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Alias (Core.Maybe Types.String)
aName = Lens.field @"name"
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of names for the alias, including the canonical name.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNames :: Lens.Lens' Alias (Core.Maybe [Types.String])
aNames = Lens.field @"names"
{-# DEPRECATED aNames "Use generic-lens or generic-optics with 'names' instead." #-}

-- | The type of the alias.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aType :: Lens.Lens' Alias (Core.Maybe Types.String)
aType = Lens.field @"type'"
{-# DEPRECATED aType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON Alias where
  parseJSON =
    Core.withObject "Alias" Core.$
      \x ->
        Alias'
          Core.<$> (x Core..:? "Name")
          Core.<*> (x Core..:? "Names")
          Core.<*> (x Core..:? "Type")

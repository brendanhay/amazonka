{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Alias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.Alias
  ( Alias (..)
  -- * Smart constructor
  , mkAlias
  -- * Lenses
  , aName
  , aNames
  , aType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An alias for an edge.
--
-- /See:/ 'mkAlias' smart constructor.
data Alias = Alias'
  { name :: Core.Maybe Core.Text
    -- ^ The canonical name of the alias.
  , names :: Core.Maybe [Core.Text]
    -- ^ A list of names for the alias, including the canonical name.
  , type' :: Core.Maybe Core.Text
    -- ^ The type of the alias.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Alias' value with any optional fields omitted.
mkAlias
    :: Alias
mkAlias
  = Alias'{name = Core.Nothing, names = Core.Nothing,
           type' = Core.Nothing}

-- | The canonical name of the alias.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Alias (Core.Maybe Core.Text)
aName = Lens.field @"name"
{-# INLINEABLE aName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A list of names for the alias, including the canonical name.
--
-- /Note:/ Consider using 'names' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNames :: Lens.Lens' Alias (Core.Maybe [Core.Text])
aNames = Lens.field @"names"
{-# INLINEABLE aNames #-}
{-# DEPRECATED names "Use generic-lens or generic-optics with 'names' instead"  #-}

-- | The type of the alias.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aType :: Lens.Lens' Alias (Core.Maybe Core.Text)
aType = Lens.field @"type'"
{-# INLINEABLE aType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON Alias where
        parseJSON
          = Core.withObject "Alias" Core.$
              \ x ->
                Alias' Core.<$>
                  (x Core..:? "Name") Core.<*> x Core..:? "Names" Core.<*>
                    x Core..:? "Type"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.Attribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.Attribute
  ( Attribute (..)
  -- * Smart constructor
  , mkAttribute
  -- * Lenses
  , aName
  , aValue
  ) where

import qualified Network.AWS.DirectoryService.Types.Name as Types
import qualified Network.AWS.DirectoryService.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a named directory attribute.
--
-- /See:/ 'mkAttribute' smart constructor.
data Attribute = Attribute'
  { name :: Core.Maybe Types.Name
    -- ^ The name of the attribute.
  , value :: Core.Maybe Types.Value
    -- ^ The value of the attribute.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Attribute' value with any optional fields omitted.
mkAttribute
    :: Attribute
mkAttribute = Attribute'{name = Core.Nothing, value = Core.Nothing}

-- | The name of the attribute.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Attribute (Core.Maybe Types.Name)
aName = Lens.field @"name"
{-# INLINEABLE aName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aValue :: Lens.Lens' Attribute (Core.Maybe Types.Value)
aValue = Lens.field @"value"
{-# INLINEABLE aValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON Attribute where
        toJSON Attribute{..}
          = Core.object
              (Core.catMaybes
                 [("Name" Core..=) Core.<$> name, ("Value" Core..=) Core.<$> value])

instance Core.FromJSON Attribute where
        parseJSON
          = Core.withObject "Attribute" Core.$
              \ x ->
                Attribute' Core.<$> (x Core..:? "Name") Core.<*> x Core..:? "Value"

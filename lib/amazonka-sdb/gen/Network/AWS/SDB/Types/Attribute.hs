{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.Attribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.Attribute
  ( Attribute (..),

    -- * Smart constructor
    mkAttribute,

    -- * Lenses
    aName,
    aValue,
    aAlternateNameEncoding,
    aAlternateValueEncoding,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SDB.Types.AlternateNameEncoding as Types
import qualified Network.AWS.SDB.Types.AlternateValueEncoding as Types
import qualified Network.AWS.SDB.Types.Name as Types
import qualified Network.AWS.SDB.Types.Value as Types

-- |
--
-- /See:/ 'mkAttribute' smart constructor.
data Attribute = Attribute'
  { -- | The name of the attribute.
    name :: Types.Name,
    -- | The value of the attribute.
    value :: Types.Value,
    -- |
    alternateNameEncoding :: Core.Maybe Types.AlternateNameEncoding,
    -- |
    alternateValueEncoding :: Core.Maybe Types.AlternateValueEncoding
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Attribute' value with any optional fields omitted.
mkAttribute ::
  -- | 'name'
  Types.Name ->
  -- | 'value'
  Types.Value ->
  Attribute
mkAttribute name value =
  Attribute'
    { name,
      value,
      alternateNameEncoding = Core.Nothing,
      alternateValueEncoding = Core.Nothing
    }

-- | The name of the attribute.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Attribute Types.Name
aName = Lens.field @"name"
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aValue :: Lens.Lens' Attribute Types.Value
aValue = Lens.field @"value"
{-# DEPRECATED aValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- |
--
-- /Note:/ Consider using 'alternateNameEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlternateNameEncoding :: Lens.Lens' Attribute (Core.Maybe Types.AlternateNameEncoding)
aAlternateNameEncoding = Lens.field @"alternateNameEncoding"
{-# DEPRECATED aAlternateNameEncoding "Use generic-lens or generic-optics with 'alternateNameEncoding' instead." #-}

-- |
--
-- /Note:/ Consider using 'alternateValueEncoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlternateValueEncoding :: Lens.Lens' Attribute (Core.Maybe Types.AlternateValueEncoding)
aAlternateValueEncoding = Lens.field @"alternateValueEncoding"
{-# DEPRECATED aAlternateValueEncoding "Use generic-lens or generic-optics with 'alternateValueEncoding' instead." #-}

instance Core.FromXML Attribute where
  parseXML x =
    Attribute'
      Core.<$> (x Core..@ "Name")
      Core.<*> (x Core..@ "Value")
      Core.<*> (x Core..@? "AlternateNameEncoding")
      Core.<*> (x Core..@? "AlternateValueEncoding")

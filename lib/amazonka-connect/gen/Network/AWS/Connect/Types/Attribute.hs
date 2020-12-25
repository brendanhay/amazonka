{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Attribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Attribute
  ( Attribute (..),

    -- * Smart constructor
    mkAttribute,

    -- * Lenses
    aAttributeType,
    aValue,
  )
where

import qualified Network.AWS.Connect.Types.InstanceAttributeType as Types
import qualified Network.AWS.Connect.Types.InstanceAttributeValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A toggle for an individual feature at the instance level.
--
-- /See:/ 'mkAttribute' smart constructor.
data Attribute = Attribute'
  { -- | The type of attribute.
    attributeType :: Core.Maybe Types.InstanceAttributeType,
    -- | The value of the attribute.
    value :: Core.Maybe Types.InstanceAttributeValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Attribute' value with any optional fields omitted.
mkAttribute ::
  Attribute
mkAttribute =
  Attribute' {attributeType = Core.Nothing, value = Core.Nothing}

-- | The type of attribute.
--
-- /Note:/ Consider using 'attributeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAttributeType :: Lens.Lens' Attribute (Core.Maybe Types.InstanceAttributeType)
aAttributeType = Lens.field @"attributeType"
{-# DEPRECATED aAttributeType "Use generic-lens or generic-optics with 'attributeType' instead." #-}

-- | The value of the attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aValue :: Lens.Lens' Attribute (Core.Maybe Types.InstanceAttributeValue)
aValue = Lens.field @"value"
{-# DEPRECATED aValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON Attribute where
  parseJSON =
    Core.withObject "Attribute" Core.$
      \x ->
        Attribute'
          Core.<$> (x Core..:? "AttributeType") Core.<*> (x Core..:? "Value")

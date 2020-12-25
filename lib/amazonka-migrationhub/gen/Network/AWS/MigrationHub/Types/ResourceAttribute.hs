{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.ResourceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.ResourceAttribute
  ( ResourceAttribute (..),

    -- * Smart constructor
    mkResourceAttribute,

    -- * Lenses
    raType,
    raValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types.ResourceAttributeType as Types
import qualified Network.AWS.MigrationHub.Types.Value as Types
import qualified Network.AWS.Prelude as Core

-- | Attribute associated with a resource.
--
-- Note the corresponding format required per type listed below:
--
--     * IPV4
--
--     * @x.x.x.x@
-- /where x is an integer in the range [0,255]/
--
--
--     * IPV6
--
--     * @y : y : y : y : y : y : y : y@
-- /where y is a hexadecimal between 0 and FFFF. [0, FFFF]/
--
--
--     * MAC_ADDRESS
--
--     * @^([0-9A-Fa-f]{2}[:-]){5}([0-9A-Fa-f]{2})$@
--
--
--     * FQDN
--
--     * @^[^<>{}\\\\/?,=\\p{Cntrl}]{1,256}$@
--
--
--
-- /See:/ 'mkResourceAttribute' smart constructor.
data ResourceAttribute = ResourceAttribute'
  { -- | Type of resource.
    type' :: Types.ResourceAttributeType,
    -- | Value of the resource type.
    value :: Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceAttribute' value with any optional fields omitted.
mkResourceAttribute ::
  -- | 'type\''
  Types.ResourceAttributeType ->
  -- | 'value'
  Types.Value ->
  ResourceAttribute
mkResourceAttribute type' value = ResourceAttribute' {type', value}

-- | Type of resource.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raType :: Lens.Lens' ResourceAttribute Types.ResourceAttributeType
raType = Lens.field @"type'"
{-# DEPRECATED raType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Value of the resource type.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raValue :: Lens.Lens' ResourceAttribute Types.Value
raValue = Lens.field @"value"
{-# DEPRECATED raValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON ResourceAttribute where
  toJSON ResourceAttribute {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Type" Core..= type'),
            Core.Just ("Value" Core..= value)
          ]
      )

instance Core.FromJSON ResourceAttribute where
  parseJSON =
    Core.withObject "ResourceAttribute" Core.$
      \x ->
        ResourceAttribute'
          Core.<$> (x Core..: "Type") Core.<*> (x Core..: "Value")

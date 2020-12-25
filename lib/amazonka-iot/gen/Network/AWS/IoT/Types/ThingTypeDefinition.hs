{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingTypeDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingTypeDefinition
  ( ThingTypeDefinition (..),

    -- * Smart constructor
    mkThingTypeDefinition,

    -- * Lenses
    ttdThingTypeArn,
    ttdThingTypeMetadata,
    ttdThingTypeName,
    ttdThingTypeProperties,
  )
where

import qualified Network.AWS.IoT.Types.ThingTypeArn as Types
import qualified Network.AWS.IoT.Types.ThingTypeMetadata as Types
import qualified Network.AWS.IoT.Types.ThingTypeName as Types
import qualified Network.AWS.IoT.Types.ThingTypeProperties as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The definition of the thing type, including thing type name and description.
--
-- /See:/ 'mkThingTypeDefinition' smart constructor.
data ThingTypeDefinition = ThingTypeDefinition'
  { -- | The thing type ARN.
    thingTypeArn :: Core.Maybe Types.ThingTypeArn,
    -- | The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
    thingTypeMetadata :: Core.Maybe Types.ThingTypeMetadata,
    -- | The name of the thing type.
    thingTypeName :: Core.Maybe Types.ThingTypeName,
    -- | The ThingTypeProperties for the thing type.
    thingTypeProperties :: Core.Maybe Types.ThingTypeProperties
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ThingTypeDefinition' value with any optional fields omitted.
mkThingTypeDefinition ::
  ThingTypeDefinition
mkThingTypeDefinition =
  ThingTypeDefinition'
    { thingTypeArn = Core.Nothing,
      thingTypeMetadata = Core.Nothing,
      thingTypeName = Core.Nothing,
      thingTypeProperties = Core.Nothing
    }

-- | The thing type ARN.
--
-- /Note:/ Consider using 'thingTypeArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttdThingTypeArn :: Lens.Lens' ThingTypeDefinition (Core.Maybe Types.ThingTypeArn)
ttdThingTypeArn = Lens.field @"thingTypeArn"
{-# DEPRECATED ttdThingTypeArn "Use generic-lens or generic-optics with 'thingTypeArn' instead." #-}

-- | The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
--
-- /Note:/ Consider using 'thingTypeMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttdThingTypeMetadata :: Lens.Lens' ThingTypeDefinition (Core.Maybe Types.ThingTypeMetadata)
ttdThingTypeMetadata = Lens.field @"thingTypeMetadata"
{-# DEPRECATED ttdThingTypeMetadata "Use generic-lens or generic-optics with 'thingTypeMetadata' instead." #-}

-- | The name of the thing type.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttdThingTypeName :: Lens.Lens' ThingTypeDefinition (Core.Maybe Types.ThingTypeName)
ttdThingTypeName = Lens.field @"thingTypeName"
{-# DEPRECATED ttdThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

-- | The ThingTypeProperties for the thing type.
--
-- /Note:/ Consider using 'thingTypeProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttdThingTypeProperties :: Lens.Lens' ThingTypeDefinition (Core.Maybe Types.ThingTypeProperties)
ttdThingTypeProperties = Lens.field @"thingTypeProperties"
{-# DEPRECATED ttdThingTypeProperties "Use generic-lens or generic-optics with 'thingTypeProperties' instead." #-}

instance Core.FromJSON ThingTypeDefinition where
  parseJSON =
    Core.withObject "ThingTypeDefinition" Core.$
      \x ->
        ThingTypeDefinition'
          Core.<$> (x Core..:? "thingTypeArn")
          Core.<*> (x Core..:? "thingTypeMetadata")
          Core.<*> (x Core..:? "thingTypeName")
          Core.<*> (x Core..:? "thingTypeProperties")

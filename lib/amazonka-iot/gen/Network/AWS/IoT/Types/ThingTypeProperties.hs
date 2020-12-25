{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingTypeProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingTypeProperties
  ( ThingTypeProperties (..),

    -- * Smart constructor
    mkThingTypeProperties,

    -- * Lenses
    ttpSearchableAttributes,
    ttpThingTypeDescription,
  )
where

import qualified Network.AWS.IoT.Types.AttributeName as Types
import qualified Network.AWS.IoT.Types.ThingTypeDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The ThingTypeProperties contains information about the thing type including: a thing type description, and a list of searchable thing attribute names.
--
-- /See:/ 'mkThingTypeProperties' smart constructor.
data ThingTypeProperties = ThingTypeProperties'
  { -- | A list of searchable thing attribute names.
    searchableAttributes :: Core.Maybe [Types.AttributeName],
    -- | The description of the thing type.
    thingTypeDescription :: Core.Maybe Types.ThingTypeDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ThingTypeProperties' value with any optional fields omitted.
mkThingTypeProperties ::
  ThingTypeProperties
mkThingTypeProperties =
  ThingTypeProperties'
    { searchableAttributes = Core.Nothing,
      thingTypeDescription = Core.Nothing
    }

-- | A list of searchable thing attribute names.
--
-- /Note:/ Consider using 'searchableAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttpSearchableAttributes :: Lens.Lens' ThingTypeProperties (Core.Maybe [Types.AttributeName])
ttpSearchableAttributes = Lens.field @"searchableAttributes"
{-# DEPRECATED ttpSearchableAttributes "Use generic-lens or generic-optics with 'searchableAttributes' instead." #-}

-- | The description of the thing type.
--
-- /Note:/ Consider using 'thingTypeDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttpThingTypeDescription :: Lens.Lens' ThingTypeProperties (Core.Maybe Types.ThingTypeDescription)
ttpThingTypeDescription = Lens.field @"thingTypeDescription"
{-# DEPRECATED ttpThingTypeDescription "Use generic-lens or generic-optics with 'thingTypeDescription' instead." #-}

instance Core.FromJSON ThingTypeProperties where
  toJSON ThingTypeProperties {..} =
    Core.object
      ( Core.catMaybes
          [ ("searchableAttributes" Core..=) Core.<$> searchableAttributes,
            ("thingTypeDescription" Core..=) Core.<$> thingTypeDescription
          ]
      )

instance Core.FromJSON ThingTypeProperties where
  parseJSON =
    Core.withObject "ThingTypeProperties" Core.$
      \x ->
        ThingTypeProperties'
          Core.<$> (x Core..:? "searchableAttributes")
          Core.<*> (x Core..:? "thingTypeDescription")

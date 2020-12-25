{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingGroupIndexingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingGroupIndexingConfiguration
  ( ThingGroupIndexingConfiguration (..),

    -- * Smart constructor
    mkThingGroupIndexingConfiguration,

    -- * Lenses
    tgicThingGroupIndexingMode,
    tgicCustomFields,
    tgicManagedFields,
  )
where

import qualified Network.AWS.IoT.Types.Field as Types
import qualified Network.AWS.IoT.Types.ThingGroupIndexingMode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Thing group indexing configuration.
--
-- /See:/ 'mkThingGroupIndexingConfiguration' smart constructor.
data ThingGroupIndexingConfiguration = ThingGroupIndexingConfiguration'
  { -- | Thing group indexing mode.
    thingGroupIndexingMode :: Types.ThingGroupIndexingMode,
    -- | A list of thing group fields to index. This list cannot contain any managed fields. Use the GetIndexingConfiguration API to get a list of managed fields.
    --
    -- Contains custom field names and their data type.
    customFields :: Core.Maybe [Types.Field],
    -- | Contains fields that are indexed and whose types are already known by the Fleet Indexing service.
    managedFields :: Core.Maybe [Types.Field]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ThingGroupIndexingConfiguration' value with any optional fields omitted.
mkThingGroupIndexingConfiguration ::
  -- | 'thingGroupIndexingMode'
  Types.ThingGroupIndexingMode ->
  ThingGroupIndexingConfiguration
mkThingGroupIndexingConfiguration thingGroupIndexingMode =
  ThingGroupIndexingConfiguration'
    { thingGroupIndexingMode,
      customFields = Core.Nothing,
      managedFields = Core.Nothing
    }

-- | Thing group indexing mode.
--
-- /Note:/ Consider using 'thingGroupIndexingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgicThingGroupIndexingMode :: Lens.Lens' ThingGroupIndexingConfiguration Types.ThingGroupIndexingMode
tgicThingGroupIndexingMode = Lens.field @"thingGroupIndexingMode"
{-# DEPRECATED tgicThingGroupIndexingMode "Use generic-lens or generic-optics with 'thingGroupIndexingMode' instead." #-}

-- | A list of thing group fields to index. This list cannot contain any managed fields. Use the GetIndexingConfiguration API to get a list of managed fields.
--
-- Contains custom field names and their data type.
--
-- /Note:/ Consider using 'customFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgicCustomFields :: Lens.Lens' ThingGroupIndexingConfiguration (Core.Maybe [Types.Field])
tgicCustomFields = Lens.field @"customFields"
{-# DEPRECATED tgicCustomFields "Use generic-lens or generic-optics with 'customFields' instead." #-}

-- | Contains fields that are indexed and whose types are already known by the Fleet Indexing service.
--
-- /Note:/ Consider using 'managedFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgicManagedFields :: Lens.Lens' ThingGroupIndexingConfiguration (Core.Maybe [Types.Field])
tgicManagedFields = Lens.field @"managedFields"
{-# DEPRECATED tgicManagedFields "Use generic-lens or generic-optics with 'managedFields' instead." #-}

instance Core.FromJSON ThingGroupIndexingConfiguration where
  toJSON ThingGroupIndexingConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("thingGroupIndexingMode" Core..= thingGroupIndexingMode),
            ("customFields" Core..=) Core.<$> customFields,
            ("managedFields" Core..=) Core.<$> managedFields
          ]
      )

instance Core.FromJSON ThingGroupIndexingConfiguration where
  parseJSON =
    Core.withObject "ThingGroupIndexingConfiguration" Core.$
      \x ->
        ThingGroupIndexingConfiguration'
          Core.<$> (x Core..: "thingGroupIndexingMode")
          Core.<*> (x Core..:? "customFields")
          Core.<*> (x Core..:? "managedFields")

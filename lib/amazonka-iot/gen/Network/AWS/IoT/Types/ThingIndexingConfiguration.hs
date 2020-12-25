{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingIndexingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingIndexingConfiguration
  ( ThingIndexingConfiguration (..),

    -- * Smart constructor
    mkThingIndexingConfiguration,

    -- * Lenses
    ticThingIndexingMode,
    ticCustomFields,
    ticManagedFields,
    ticThingConnectivityIndexingMode,
  )
where

import qualified Network.AWS.IoT.Types.Field as Types
import qualified Network.AWS.IoT.Types.ThingConnectivityIndexingMode as Types
import qualified Network.AWS.IoT.Types.ThingIndexingMode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The thing indexing configuration. For more information, see <https://docs.aws.amazon.com/iot/latest/developerguide/managing-index.html Managing Thing Indexing> .
--
-- /See:/ 'mkThingIndexingConfiguration' smart constructor.
data ThingIndexingConfiguration = ThingIndexingConfiguration'
  { -- | Thing indexing mode. Valid values are:
    --
    --
    --     * REGISTRY – Your thing index contains registry data only.
    --
    --
    --     * REGISTRY_AND_SHADOW - Your thing index contains registry and shadow data.
    --
    --
    --     * OFF - Thing indexing is disabled.
    thingIndexingMode :: Types.ThingIndexingMode,
    -- | Contains custom field names and their data type.
    customFields :: Core.Maybe [Types.Field],
    -- | Contains fields that are indexed and whose types are already known by the Fleet Indexing service.
    managedFields :: Core.Maybe [Types.Field],
    -- | Thing connectivity indexing mode. Valid values are:
    --
    --
    --     * STATUS – Your thing index contains connectivity status. To enable thing connectivity indexing, thingIndexMode must not be set to OFF.
    --
    --
    --     * OFF - Thing connectivity status indexing is disabled.
    thingConnectivityIndexingMode :: Core.Maybe Types.ThingConnectivityIndexingMode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ThingIndexingConfiguration' value with any optional fields omitted.
mkThingIndexingConfiguration ::
  -- | 'thingIndexingMode'
  Types.ThingIndexingMode ->
  ThingIndexingConfiguration
mkThingIndexingConfiguration thingIndexingMode =
  ThingIndexingConfiguration'
    { thingIndexingMode,
      customFields = Core.Nothing,
      managedFields = Core.Nothing,
      thingConnectivityIndexingMode = Core.Nothing
    }

-- | Thing indexing mode. Valid values are:
--
--
--     * REGISTRY – Your thing index contains registry data only.
--
--
--     * REGISTRY_AND_SHADOW - Your thing index contains registry and shadow data.
--
--
--     * OFF - Thing indexing is disabled.
--
--
--
-- /Note:/ Consider using 'thingIndexingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ticThingIndexingMode :: Lens.Lens' ThingIndexingConfiguration Types.ThingIndexingMode
ticThingIndexingMode = Lens.field @"thingIndexingMode"
{-# DEPRECATED ticThingIndexingMode "Use generic-lens or generic-optics with 'thingIndexingMode' instead." #-}

-- | Contains custom field names and their data type.
--
-- /Note:/ Consider using 'customFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ticCustomFields :: Lens.Lens' ThingIndexingConfiguration (Core.Maybe [Types.Field])
ticCustomFields = Lens.field @"customFields"
{-# DEPRECATED ticCustomFields "Use generic-lens or generic-optics with 'customFields' instead." #-}

-- | Contains fields that are indexed and whose types are already known by the Fleet Indexing service.
--
-- /Note:/ Consider using 'managedFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ticManagedFields :: Lens.Lens' ThingIndexingConfiguration (Core.Maybe [Types.Field])
ticManagedFields = Lens.field @"managedFields"
{-# DEPRECATED ticManagedFields "Use generic-lens or generic-optics with 'managedFields' instead." #-}

-- | Thing connectivity indexing mode. Valid values are:
--
--
--     * STATUS – Your thing index contains connectivity status. To enable thing connectivity indexing, thingIndexMode must not be set to OFF.
--
--
--     * OFF - Thing connectivity status indexing is disabled.
--
--
--
-- /Note:/ Consider using 'thingConnectivityIndexingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ticThingConnectivityIndexingMode :: Lens.Lens' ThingIndexingConfiguration (Core.Maybe Types.ThingConnectivityIndexingMode)
ticThingConnectivityIndexingMode = Lens.field @"thingConnectivityIndexingMode"
{-# DEPRECATED ticThingConnectivityIndexingMode "Use generic-lens or generic-optics with 'thingConnectivityIndexingMode' instead." #-}

instance Core.FromJSON ThingIndexingConfiguration where
  toJSON ThingIndexingConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("thingIndexingMode" Core..= thingIndexingMode),
            ("customFields" Core..=) Core.<$> customFields,
            ("managedFields" Core..=) Core.<$> managedFields,
            ("thingConnectivityIndexingMode" Core..=)
              Core.<$> thingConnectivityIndexingMode
          ]
      )

instance Core.FromJSON ThingIndexingConfiguration where
  parseJSON =
    Core.withObject "ThingIndexingConfiguration" Core.$
      \x ->
        ThingIndexingConfiguration'
          Core.<$> (x Core..: "thingIndexingMode")
          Core.<*> (x Core..:? "customFields")
          Core.<*> (x Core..:? "managedFields")
          Core.<*> (x Core..:? "thingConnectivityIndexingMode")

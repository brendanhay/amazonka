{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingDocument
  ( ThingDocument (..),

    -- * Smart constructor
    mkThingDocument,

    -- * Lenses
    tdAttributes,
    tdConnectivity,
    tdShadow,
    tdThingGroupNames,
    tdThingId,
    tdThingName,
    tdThingTypeName,
  )
where

import qualified Network.AWS.IoT.Types.AttributeName as Types
import qualified Network.AWS.IoT.Types.AttributeValue as Types
import qualified Network.AWS.IoT.Types.JsonDocument as Types
import qualified Network.AWS.IoT.Types.ThingConnectivity as Types
import qualified Network.AWS.IoT.Types.ThingGroupName as Types
import qualified Network.AWS.IoT.Types.ThingId as Types
import qualified Network.AWS.IoT.Types.ThingName as Types
import qualified Network.AWS.IoT.Types.ThingTypeName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The thing search index document.
--
-- /See:/ 'mkThingDocument' smart constructor.
data ThingDocument = ThingDocument'
  { -- | The attributes.
    attributes :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue),
    -- | Indicates whether the thing is connected to the AWS IoT service.
    connectivity :: Core.Maybe Types.ThingConnectivity,
    -- | The shadow.
    shadow :: Core.Maybe Types.JsonDocument,
    -- | Thing group names.
    thingGroupNames :: Core.Maybe [Types.ThingGroupName],
    -- | The thing ID.
    thingId :: Core.Maybe Types.ThingId,
    -- | The thing name.
    thingName :: Core.Maybe Types.ThingName,
    -- | The thing type name.
    thingTypeName :: Core.Maybe Types.ThingTypeName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ThingDocument' value with any optional fields omitted.
mkThingDocument ::
  ThingDocument
mkThingDocument =
  ThingDocument'
    { attributes = Core.Nothing,
      connectivity = Core.Nothing,
      shadow = Core.Nothing,
      thingGroupNames = Core.Nothing,
      thingId = Core.Nothing,
      thingName = Core.Nothing,
      thingTypeName = Core.Nothing
    }

-- | The attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdAttributes :: Lens.Lens' ThingDocument (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
tdAttributes = Lens.field @"attributes"
{-# DEPRECATED tdAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | Indicates whether the thing is connected to the AWS IoT service.
--
-- /Note:/ Consider using 'connectivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdConnectivity :: Lens.Lens' ThingDocument (Core.Maybe Types.ThingConnectivity)
tdConnectivity = Lens.field @"connectivity"
{-# DEPRECATED tdConnectivity "Use generic-lens or generic-optics with 'connectivity' instead." #-}

-- | The shadow.
--
-- /Note:/ Consider using 'shadow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdShadow :: Lens.Lens' ThingDocument (Core.Maybe Types.JsonDocument)
tdShadow = Lens.field @"shadow"
{-# DEPRECATED tdShadow "Use generic-lens or generic-optics with 'shadow' instead." #-}

-- | Thing group names.
--
-- /Note:/ Consider using 'thingGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdThingGroupNames :: Lens.Lens' ThingDocument (Core.Maybe [Types.ThingGroupName])
tdThingGroupNames = Lens.field @"thingGroupNames"
{-# DEPRECATED tdThingGroupNames "Use generic-lens or generic-optics with 'thingGroupNames' instead." #-}

-- | The thing ID.
--
-- /Note:/ Consider using 'thingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdThingId :: Lens.Lens' ThingDocument (Core.Maybe Types.ThingId)
tdThingId = Lens.field @"thingId"
{-# DEPRECATED tdThingId "Use generic-lens or generic-optics with 'thingId' instead." #-}

-- | The thing name.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdThingName :: Lens.Lens' ThingDocument (Core.Maybe Types.ThingName)
tdThingName = Lens.field @"thingName"
{-# DEPRECATED tdThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | The thing type name.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdThingTypeName :: Lens.Lens' ThingDocument (Core.Maybe Types.ThingTypeName)
tdThingTypeName = Lens.field @"thingTypeName"
{-# DEPRECATED tdThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

instance Core.FromJSON ThingDocument where
  parseJSON =
    Core.withObject "ThingDocument" Core.$
      \x ->
        ThingDocument'
          Core.<$> (x Core..:? "attributes")
          Core.<*> (x Core..:? "connectivity")
          Core.<*> (x Core..:? "shadow")
          Core.<*> (x Core..:? "thingGroupNames")
          Core.<*> (x Core..:? "thingId")
          Core.<*> (x Core..:? "thingName")
          Core.<*> (x Core..:? "thingTypeName")

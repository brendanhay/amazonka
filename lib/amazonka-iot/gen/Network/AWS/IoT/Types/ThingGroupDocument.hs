{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingGroupDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingGroupDocument
  ( ThingGroupDocument (..),

    -- * Smart constructor
    mkThingGroupDocument,

    -- * Lenses
    tgdAttributes,
    tgdParentGroupNames,
    tgdThingGroupDescription,
    tgdThingGroupId,
    tgdThingGroupName,
  )
where

import qualified Network.AWS.IoT.Types.AttributeName as Types
import qualified Network.AWS.IoT.Types.AttributeValue as Types
import qualified Network.AWS.IoT.Types.ThingGroupDescription as Types
import qualified Network.AWS.IoT.Types.ThingGroupId as Types
import qualified Network.AWS.IoT.Types.ThingGroupName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The thing group search index document.
--
-- /See:/ 'mkThingGroupDocument' smart constructor.
data ThingGroupDocument = ThingGroupDocument'
  { -- | The thing group attributes.
    attributes :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue),
    -- | Parent group names.
    parentGroupNames :: Core.Maybe [Types.ThingGroupName],
    -- | The thing group description.
    thingGroupDescription :: Core.Maybe Types.ThingGroupDescription,
    -- | The thing group ID.
    thingGroupId :: Core.Maybe Types.ThingGroupId,
    -- | The thing group name.
    thingGroupName :: Core.Maybe Types.ThingGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ThingGroupDocument' value with any optional fields omitted.
mkThingGroupDocument ::
  ThingGroupDocument
mkThingGroupDocument =
  ThingGroupDocument'
    { attributes = Core.Nothing,
      parentGroupNames = Core.Nothing,
      thingGroupDescription = Core.Nothing,
      thingGroupId = Core.Nothing,
      thingGroupName = Core.Nothing
    }

-- | The thing group attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgdAttributes :: Lens.Lens' ThingGroupDocument (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
tgdAttributes = Lens.field @"attributes"
{-# DEPRECATED tgdAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | Parent group names.
--
-- /Note:/ Consider using 'parentGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgdParentGroupNames :: Lens.Lens' ThingGroupDocument (Core.Maybe [Types.ThingGroupName])
tgdParentGroupNames = Lens.field @"parentGroupNames"
{-# DEPRECATED tgdParentGroupNames "Use generic-lens or generic-optics with 'parentGroupNames' instead." #-}

-- | The thing group description.
--
-- /Note:/ Consider using 'thingGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgdThingGroupDescription :: Lens.Lens' ThingGroupDocument (Core.Maybe Types.ThingGroupDescription)
tgdThingGroupDescription = Lens.field @"thingGroupDescription"
{-# DEPRECATED tgdThingGroupDescription "Use generic-lens or generic-optics with 'thingGroupDescription' instead." #-}

-- | The thing group ID.
--
-- /Note:/ Consider using 'thingGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgdThingGroupId :: Lens.Lens' ThingGroupDocument (Core.Maybe Types.ThingGroupId)
tgdThingGroupId = Lens.field @"thingGroupId"
{-# DEPRECATED tgdThingGroupId "Use generic-lens or generic-optics with 'thingGroupId' instead." #-}

-- | The thing group name.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgdThingGroupName :: Lens.Lens' ThingGroupDocument (Core.Maybe Types.ThingGroupName)
tgdThingGroupName = Lens.field @"thingGroupName"
{-# DEPRECATED tgdThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

instance Core.FromJSON ThingGroupDocument where
  parseJSON =
    Core.withObject "ThingGroupDocument" Core.$
      \x ->
        ThingGroupDocument'
          Core.<$> (x Core..:? "attributes")
          Core.<*> (x Core..:? "parentGroupNames")
          Core.<*> (x Core..:? "thingGroupDescription")
          Core.<*> (x Core..:? "thingGroupId")
          Core.<*> (x Core..:? "thingGroupName")

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingAttribute
  ( ThingAttribute (..),

    -- * Smart constructor
    mkThingAttribute,

    -- * Lenses
    taAttributes,
    taThingArn,
    taThingName,
    taThingTypeName,
    taVersion,
  )
where

import qualified Network.AWS.IoT.Types.AttributeName as Types
import qualified Network.AWS.IoT.Types.AttributeValue as Types
import qualified Network.AWS.IoT.Types.ThingArn as Types
import qualified Network.AWS.IoT.Types.ThingName as Types
import qualified Network.AWS.IoT.Types.ThingTypeName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The properties of the thing, including thing name, thing type name, and a list of thing attributes.
--
-- /See:/ 'mkThingAttribute' smart constructor.
data ThingAttribute = ThingAttribute'
  { -- | A list of thing attributes which are name-value pairs.
    attributes :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue),
    -- | The thing ARN.
    thingArn :: Core.Maybe Types.ThingArn,
    -- | The name of the thing.
    thingName :: Core.Maybe Types.ThingName,
    -- | The name of the thing type, if the thing has been associated with a type.
    thingTypeName :: Core.Maybe Types.ThingTypeName,
    -- | The version of the thing record in the registry.
    version :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ThingAttribute' value with any optional fields omitted.
mkThingAttribute ::
  ThingAttribute
mkThingAttribute =
  ThingAttribute'
    { attributes = Core.Nothing,
      thingArn = Core.Nothing,
      thingName = Core.Nothing,
      thingTypeName = Core.Nothing,
      version = Core.Nothing
    }

-- | A list of thing attributes which are name-value pairs.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taAttributes :: Lens.Lens' ThingAttribute (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
taAttributes = Lens.field @"attributes"
{-# DEPRECATED taAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The thing ARN.
--
-- /Note:/ Consider using 'thingArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taThingArn :: Lens.Lens' ThingAttribute (Core.Maybe Types.ThingArn)
taThingArn = Lens.field @"thingArn"
{-# DEPRECATED taThingArn "Use generic-lens or generic-optics with 'thingArn' instead." #-}

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taThingName :: Lens.Lens' ThingAttribute (Core.Maybe Types.ThingName)
taThingName = Lens.field @"thingName"
{-# DEPRECATED taThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | The name of the thing type, if the thing has been associated with a type.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taThingTypeName :: Lens.Lens' ThingAttribute (Core.Maybe Types.ThingTypeName)
taThingTypeName = Lens.field @"thingTypeName"
{-# DEPRECATED taThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

-- | The version of the thing record in the registry.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taVersion :: Lens.Lens' ThingAttribute (Core.Maybe Core.Integer)
taVersion = Lens.field @"version"
{-# DEPRECATED taVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON ThingAttribute where
  parseJSON =
    Core.withObject "ThingAttribute" Core.$
      \x ->
        ThingAttribute'
          Core.<$> (x Core..:? "attributes")
          Core.<*> (x Core..:? "thingArn")
          Core.<*> (x Core..:? "thingName")
          Core.<*> (x Core..:? "thingTypeName")
          Core.<*> (x Core..:? "version")

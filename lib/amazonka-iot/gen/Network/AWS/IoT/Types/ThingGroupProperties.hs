{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingGroupProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.ThingGroupProperties
  ( ThingGroupProperties (..)
  -- * Smart constructor
  , mkThingGroupProperties
  -- * Lenses
  , tgpAttributePayload
  , tgpThingGroupDescription
  ) where

import qualified Network.AWS.IoT.Types.AttributePayload as Types
import qualified Network.AWS.IoT.Types.ThingGroupDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Thing group properties.
--
-- /See:/ 'mkThingGroupProperties' smart constructor.
data ThingGroupProperties = ThingGroupProperties'
  { attributePayload :: Core.Maybe Types.AttributePayload
    -- ^ The thing group attributes in JSON format.
  , thingGroupDescription :: Core.Maybe Types.ThingGroupDescription
    -- ^ The thing group description.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ThingGroupProperties' value with any optional fields omitted.
mkThingGroupProperties
    :: ThingGroupProperties
mkThingGroupProperties
  = ThingGroupProperties'{attributePayload = Core.Nothing,
                          thingGroupDescription = Core.Nothing}

-- | The thing group attributes in JSON format.
--
-- /Note:/ Consider using 'attributePayload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpAttributePayload :: Lens.Lens' ThingGroupProperties (Core.Maybe Types.AttributePayload)
tgpAttributePayload = Lens.field @"attributePayload"
{-# INLINEABLE tgpAttributePayload #-}
{-# DEPRECATED attributePayload "Use generic-lens or generic-optics with 'attributePayload' instead"  #-}

-- | The thing group description.
--
-- /Note:/ Consider using 'thingGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpThingGroupDescription :: Lens.Lens' ThingGroupProperties (Core.Maybe Types.ThingGroupDescription)
tgpThingGroupDescription = Lens.field @"thingGroupDescription"
{-# INLINEABLE tgpThingGroupDescription #-}
{-# DEPRECATED thingGroupDescription "Use generic-lens or generic-optics with 'thingGroupDescription' instead"  #-}

instance Core.FromJSON ThingGroupProperties where
        toJSON ThingGroupProperties{..}
          = Core.object
              (Core.catMaybes
                 [("attributePayload" Core..=) Core.<$> attributePayload,
                  ("thingGroupDescription" Core..=) Core.<$> thingGroupDescription])

instance Core.FromJSON ThingGroupProperties where
        parseJSON
          = Core.withObject "ThingGroupProperties" Core.$
              \ x ->
                ThingGroupProperties' Core.<$>
                  (x Core..:? "attributePayload") Core.<*>
                    x Core..:? "thingGroupDescription"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DeviceShadowEnrichActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.DeviceShadowEnrichActivity
  ( DeviceShadowEnrichActivity (..)
  -- * Smart constructor
  , mkDeviceShadowEnrichActivity
  -- * Lenses
  , dseaName
  , dseaAttribute
  , dseaThingName
  , dseaRoleArn
  , dseaNext
  ) where

import qualified Network.AWS.IoTAnalytics.Types.ActivityName as Types
import qualified Network.AWS.IoTAnalytics.Types.Attribute as Types
import qualified Network.AWS.IoTAnalytics.Types.RoleArn as Types
import qualified Network.AWS.IoTAnalytics.Types.ThingName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An activity that adds information from the AWS IoT Device Shadow service to a message.
--
-- /See:/ 'mkDeviceShadowEnrichActivity' smart constructor.
data DeviceShadowEnrichActivity = DeviceShadowEnrichActivity'
  { name :: Types.ActivityName
    -- ^ The name of the @deviceShadowEnrich@ activity.
  , attribute :: Types.Attribute
    -- ^ The name of the attribute that is added to the message.
  , thingName :: Types.ThingName
    -- ^ The name of the IoT device whose shadow information is added to the message.
  , roleArn :: Types.RoleArn
    -- ^ The ARN of the role that allows access to the device's shadow.
  , next :: Core.Maybe Types.ActivityName
    -- ^ The next activity in the pipeline.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeviceShadowEnrichActivity' value with any optional fields omitted.
mkDeviceShadowEnrichActivity
    :: Types.ActivityName -- ^ 'name'
    -> Types.Attribute -- ^ 'attribute'
    -> Types.ThingName -- ^ 'thingName'
    -> Types.RoleArn -- ^ 'roleArn'
    -> DeviceShadowEnrichActivity
mkDeviceShadowEnrichActivity name attribute thingName roleArn
  = DeviceShadowEnrichActivity'{name, attribute, thingName, roleArn,
                                next = Core.Nothing}

-- | The name of the @deviceShadowEnrich@ activity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dseaName :: Lens.Lens' DeviceShadowEnrichActivity Types.ActivityName
dseaName = Lens.field @"name"
{-# INLINEABLE dseaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The name of the attribute that is added to the message.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dseaAttribute :: Lens.Lens' DeviceShadowEnrichActivity Types.Attribute
dseaAttribute = Lens.field @"attribute"
{-# INLINEABLE dseaAttribute #-}
{-# DEPRECATED attribute "Use generic-lens or generic-optics with 'attribute' instead"  #-}

-- | The name of the IoT device whose shadow information is added to the message.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dseaThingName :: Lens.Lens' DeviceShadowEnrichActivity Types.ThingName
dseaThingName = Lens.field @"thingName"
{-# INLINEABLE dseaThingName #-}
{-# DEPRECATED thingName "Use generic-lens or generic-optics with 'thingName' instead"  #-}

-- | The ARN of the role that allows access to the device's shadow.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dseaRoleArn :: Lens.Lens' DeviceShadowEnrichActivity Types.RoleArn
dseaRoleArn = Lens.field @"roleArn"
{-# INLINEABLE dseaRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The next activity in the pipeline.
--
-- /Note:/ Consider using 'next' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dseaNext :: Lens.Lens' DeviceShadowEnrichActivity (Core.Maybe Types.ActivityName)
dseaNext = Lens.field @"next"
{-# INLINEABLE dseaNext #-}
{-# DEPRECATED next "Use generic-lens or generic-optics with 'next' instead"  #-}

instance Core.FromJSON DeviceShadowEnrichActivity where
        toJSON DeviceShadowEnrichActivity{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("attribute" Core..= attribute),
                  Core.Just ("thingName" Core..= thingName),
                  Core.Just ("roleArn" Core..= roleArn),
                  ("next" Core..=) Core.<$> next])

instance Core.FromJSON DeviceShadowEnrichActivity where
        parseJSON
          = Core.withObject "DeviceShadowEnrichActivity" Core.$
              \ x ->
                DeviceShadowEnrichActivity' Core.<$>
                  (x Core..: "name") Core.<*> x Core..: "attribute" Core.<*>
                    x Core..: "thingName"
                    Core.<*> x Core..: "roleArn"
                    Core.<*> x Core..:? "next"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DeviceRegistryEnrichActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.DeviceRegistryEnrichActivity
  ( DeviceRegistryEnrichActivity (..)
  -- * Smart constructor
  , mkDeviceRegistryEnrichActivity
  -- * Lenses
  , dreaName
  , dreaAttribute
  , dreaThingName
  , dreaRoleArn
  , dreaNext
  ) where

import qualified Network.AWS.IoTAnalytics.Types.ActivityName as Types
import qualified Network.AWS.IoTAnalytics.Types.Attribute as Types
import qualified Network.AWS.IoTAnalytics.Types.RoleArn as Types
import qualified Network.AWS.IoTAnalytics.Types.ThingName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An activity that adds data from the AWS IoT device registry to your message.
--
-- /See:/ 'mkDeviceRegistryEnrichActivity' smart constructor.
data DeviceRegistryEnrichActivity = DeviceRegistryEnrichActivity'
  { name :: Types.ActivityName
    -- ^ The name of the @deviceRegistryEnrich@ activity.
  , attribute :: Types.Attribute
    -- ^ The name of the attribute that is added to the message.
  , thingName :: Types.ThingName
    -- ^ The name of the IoT device whose registry information is added to the message.
  , roleArn :: Types.RoleArn
    -- ^ The ARN of the role that allows access to the device's registry information.
  , next :: Core.Maybe Types.ActivityName
    -- ^ The next activity in the pipeline.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeviceRegistryEnrichActivity' value with any optional fields omitted.
mkDeviceRegistryEnrichActivity
    :: Types.ActivityName -- ^ 'name'
    -> Types.Attribute -- ^ 'attribute'
    -> Types.ThingName -- ^ 'thingName'
    -> Types.RoleArn -- ^ 'roleArn'
    -> DeviceRegistryEnrichActivity
mkDeviceRegistryEnrichActivity name attribute thingName roleArn
  = DeviceRegistryEnrichActivity'{name, attribute, thingName,
                                  roleArn, next = Core.Nothing}

-- | The name of the @deviceRegistryEnrich@ activity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreaName :: Lens.Lens' DeviceRegistryEnrichActivity Types.ActivityName
dreaName = Lens.field @"name"
{-# INLINEABLE dreaName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The name of the attribute that is added to the message.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreaAttribute :: Lens.Lens' DeviceRegistryEnrichActivity Types.Attribute
dreaAttribute = Lens.field @"attribute"
{-# INLINEABLE dreaAttribute #-}
{-# DEPRECATED attribute "Use generic-lens or generic-optics with 'attribute' instead"  #-}

-- | The name of the IoT device whose registry information is added to the message.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreaThingName :: Lens.Lens' DeviceRegistryEnrichActivity Types.ThingName
dreaThingName = Lens.field @"thingName"
{-# INLINEABLE dreaThingName #-}
{-# DEPRECATED thingName "Use generic-lens or generic-optics with 'thingName' instead"  #-}

-- | The ARN of the role that allows access to the device's registry information.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreaRoleArn :: Lens.Lens' DeviceRegistryEnrichActivity Types.RoleArn
dreaRoleArn = Lens.field @"roleArn"
{-# INLINEABLE dreaRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The next activity in the pipeline.
--
-- /Note:/ Consider using 'next' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dreaNext :: Lens.Lens' DeviceRegistryEnrichActivity (Core.Maybe Types.ActivityName)
dreaNext = Lens.field @"next"
{-# INLINEABLE dreaNext #-}
{-# DEPRECATED next "Use generic-lens or generic-optics with 'next' instead"  #-}

instance Core.FromJSON DeviceRegistryEnrichActivity where
        toJSON DeviceRegistryEnrichActivity{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("attribute" Core..= attribute),
                  Core.Just ("thingName" Core..= thingName),
                  Core.Just ("roleArn" Core..= roleArn),
                  ("next" Core..=) Core.<$> next])

instance Core.FromJSON DeviceRegistryEnrichActivity where
        parseJSON
          = Core.withObject "DeviceRegistryEnrichActivity" Core.$
              \ x ->
                DeviceRegistryEnrichActivity' Core.<$>
                  (x Core..: "name") Core.<*> x Core..: "attribute" Core.<*>
                    x Core..: "thingName"
                    Core.<*> x Core..: "roleArn"
                    Core.<*> x Core..:? "next"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DevicePool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.DevicePool
  ( DevicePool (..)
  -- * Smart constructor
  , mkDevicePool
  -- * Lenses
  , dpArn
  , dpDescription
  , dpMaxDevices
  , dpName
  , dpRules
  , dpType
  ) where

import qualified Network.AWS.DeviceFarm.Types.Arn as Types
import qualified Network.AWS.DeviceFarm.Types.Description as Types
import qualified Network.AWS.DeviceFarm.Types.DevicePoolType as Types
import qualified Network.AWS.DeviceFarm.Types.Name as Types
import qualified Network.AWS.DeviceFarm.Types.Rule as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a collection of device types.
--
-- /See:/ 'mkDevicePool' smart constructor.
data DevicePool = DevicePool'
  { arn :: Core.Maybe Types.Arn
    -- ^ The device pool's ARN.
  , description :: Core.Maybe Types.Description
    -- ^ The device pool's description.
  , maxDevices :: Core.Maybe Core.Int
    -- ^ The number of devices that Device Farm can add to your device pool. Device Farm adds devices that are available and meet the criteria that you assign for the @rules@ parameter. Depending on how many devices meet these constraints, your device pool might contain fewer devices than the value for this parameter.
--
-- By specifying the maximum number of devices, you can control the costs that you incur by running tests.
  , name :: Core.Maybe Types.Name
    -- ^ The device pool's name.
  , rules :: Core.Maybe [Types.Rule]
    -- ^ Information about the device pool's rules.
  , type' :: Core.Maybe Types.DevicePoolType
    -- ^ The device pool's type.
--
-- Allowed values include:
--
--     * CURATED: A device pool that is created and managed by AWS Device Farm.
--
--
--     * PRIVATE: A device pool that is created and managed by the device pool developer.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DevicePool' value with any optional fields omitted.
mkDevicePool
    :: DevicePool
mkDevicePool
  = DevicePool'{arn = Core.Nothing, description = Core.Nothing,
                maxDevices = Core.Nothing, name = Core.Nothing,
                rules = Core.Nothing, type' = Core.Nothing}

-- | The device pool's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpArn :: Lens.Lens' DevicePool (Core.Maybe Types.Arn)
dpArn = Lens.field @"arn"
{-# INLINEABLE dpArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The device pool's description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDescription :: Lens.Lens' DevicePool (Core.Maybe Types.Description)
dpDescription = Lens.field @"description"
{-# INLINEABLE dpDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The number of devices that Device Farm can add to your device pool. Device Farm adds devices that are available and meet the criteria that you assign for the @rules@ parameter. Depending on how many devices meet these constraints, your device pool might contain fewer devices than the value for this parameter.
--
-- By specifying the maximum number of devices, you can control the costs that you incur by running tests.
--
-- /Note:/ Consider using 'maxDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpMaxDevices :: Lens.Lens' DevicePool (Core.Maybe Core.Int)
dpMaxDevices = Lens.field @"maxDevices"
{-# INLINEABLE dpMaxDevices #-}
{-# DEPRECATED maxDevices "Use generic-lens or generic-optics with 'maxDevices' instead"  #-}

-- | The device pool's name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpName :: Lens.Lens' DevicePool (Core.Maybe Types.Name)
dpName = Lens.field @"name"
{-# INLINEABLE dpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Information about the device pool's rules.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpRules :: Lens.Lens' DevicePool (Core.Maybe [Types.Rule])
dpRules = Lens.field @"rules"
{-# INLINEABLE dpRules #-}
{-# DEPRECATED rules "Use generic-lens or generic-optics with 'rules' instead"  #-}

-- | The device pool's type.
--
-- Allowed values include:
--
--     * CURATED: A device pool that is created and managed by AWS Device Farm.
--
--
--     * PRIVATE: A device pool that is created and managed by the device pool developer.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpType :: Lens.Lens' DevicePool (Core.Maybe Types.DevicePoolType)
dpType = Lens.field @"type'"
{-# INLINEABLE dpType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON DevicePool where
        parseJSON
          = Core.withObject "DevicePool" Core.$
              \ x ->
                DevicePool' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "description" Core.<*>
                    x Core..:? "maxDevices"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "rules"
                    Core.<*> x Core..:? "type"

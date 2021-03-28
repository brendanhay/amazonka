{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.Device
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.Device
  ( Device (..)
  -- * Smart constructor
  , mkDevice
  -- * Lenses
  , dHostPath
  , dContainerPath
  , dPermissions
  ) where

import qualified Network.AWS.Batch.Types.DeviceCgroupPermission as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing a container instance host device.
--
-- /See:/ 'mkDevice' smart constructor.
data Device = Device'
  { hostPath :: Core.Text
    -- ^ The path for the device on the host container instance.
  , containerPath :: Core.Maybe Core.Text
    -- ^ The path inside the container at which to expose the host device. By default the @hostPath@ value is used.
  , permissions :: Core.Maybe [Types.DeviceCgroupPermission]
    -- ^ The explicit permissions to provide to the container for the device. By default, the container has permissions for @read@ , @write@ , and @mknod@ for the device.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Device' value with any optional fields omitted.
mkDevice
    :: Core.Text -- ^ 'hostPath'
    -> Device
mkDevice hostPath
  = Device'{hostPath, containerPath = Core.Nothing,
            permissions = Core.Nothing}

-- | The path for the device on the host container instance.
--
-- /Note:/ Consider using 'hostPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dHostPath :: Lens.Lens' Device Core.Text
dHostPath = Lens.field @"hostPath"
{-# INLINEABLE dHostPath #-}
{-# DEPRECATED hostPath "Use generic-lens or generic-optics with 'hostPath' instead"  #-}

-- | The path inside the container at which to expose the host device. By default the @hostPath@ value is used.
--
-- /Note:/ Consider using 'containerPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dContainerPath :: Lens.Lens' Device (Core.Maybe Core.Text)
dContainerPath = Lens.field @"containerPath"
{-# INLINEABLE dContainerPath #-}
{-# DEPRECATED containerPath "Use generic-lens or generic-optics with 'containerPath' instead"  #-}

-- | The explicit permissions to provide to the container for the device. By default, the container has permissions for @read@ , @write@ , and @mknod@ for the device.
--
-- /Note:/ Consider using 'permissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPermissions :: Lens.Lens' Device (Core.Maybe [Types.DeviceCgroupPermission])
dPermissions = Lens.field @"permissions"
{-# INLINEABLE dPermissions #-}
{-# DEPRECATED permissions "Use generic-lens or generic-optics with 'permissions' instead"  #-}

instance Core.FromJSON Device where
        toJSON Device{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("hostPath" Core..= hostPath),
                  ("containerPath" Core..=) Core.<$> containerPath,
                  ("permissions" Core..=) Core.<$> permissions])

instance Core.FromJSON Device where
        parseJSON
          = Core.withObject "Device" Core.$
              \ x ->
                Device' Core.<$>
                  (x Core..: "hostPath") Core.<*> x Core..:? "containerPath" Core.<*>
                    x Core..:? "permissions"

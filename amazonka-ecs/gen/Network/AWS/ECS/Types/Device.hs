{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Device
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Device where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types.DeviceCgroupPermission
import qualified Network.AWS.Lens as Lens

-- | An object representing a container instance host device.
--
-- /See:/ 'newDevice' smart constructor.
data Device = Device'
  { -- | The explicit permissions to provide to the container for the device. By
    -- default, the container has permissions for @read@, @write@, and @mknod@
    -- for the device.
    permissions :: Core.Maybe [DeviceCgroupPermission],
    -- | The path inside the container at which to expose the host device.
    containerPath :: Core.Maybe Core.Text,
    -- | The path for the device on the host container instance.
    hostPath :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Device' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissions', 'device_permissions' - The explicit permissions to provide to the container for the device. By
-- default, the container has permissions for @read@, @write@, and @mknod@
-- for the device.
--
-- 'containerPath', 'device_containerPath' - The path inside the container at which to expose the host device.
--
-- 'hostPath', 'device_hostPath' - The path for the device on the host container instance.
newDevice ::
  -- | 'hostPath'
  Core.Text ->
  Device
newDevice pHostPath_ =
  Device'
    { permissions = Core.Nothing,
      containerPath = Core.Nothing,
      hostPath = pHostPath_
    }

-- | The explicit permissions to provide to the container for the device. By
-- default, the container has permissions for @read@, @write@, and @mknod@
-- for the device.
device_permissions :: Lens.Lens' Device (Core.Maybe [DeviceCgroupPermission])
device_permissions = Lens.lens (\Device' {permissions} -> permissions) (\s@Device' {} a -> s {permissions = a} :: Device) Core.. Lens.mapping Lens._Coerce

-- | The path inside the container at which to expose the host device.
device_containerPath :: Lens.Lens' Device (Core.Maybe Core.Text)
device_containerPath = Lens.lens (\Device' {containerPath} -> containerPath) (\s@Device' {} a -> s {containerPath = a} :: Device)

-- | The path for the device on the host container instance.
device_hostPath :: Lens.Lens' Device Core.Text
device_hostPath = Lens.lens (\Device' {hostPath} -> hostPath) (\s@Device' {} a -> s {hostPath = a} :: Device)

instance Core.FromJSON Device where
  parseJSON =
    Core.withObject
      "Device"
      ( \x ->
          Device'
            Core.<$> (x Core..:? "permissions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "containerPath")
            Core.<*> (x Core..: "hostPath")
      )

instance Core.Hashable Device

instance Core.NFData Device

instance Core.ToJSON Device where
  toJSON Device' {..} =
    Core.object
      ( Core.catMaybes
          [ ("permissions" Core..=) Core.<$> permissions,
            ("containerPath" Core..=) Core.<$> containerPath,
            Core.Just ("hostPath" Core..= hostPath)
          ]
      )

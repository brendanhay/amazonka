{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ECS.Types.DeviceCgroupPermission
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing a container instance host device.
--
-- /See:/ 'newDevice' smart constructor.
data Device = Device'
  { -- | The explicit permissions to provide to the container for the device. By
    -- default, the container has permissions for @read@, @write@, and @mknod@
    -- for the device.
    permissions :: Prelude.Maybe [DeviceCgroupPermission],
    -- | The path inside the container at which to expose the host device.
    containerPath :: Prelude.Maybe Prelude.Text,
    -- | The path for the device on the host container instance.
    hostPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  Device
newDevice pHostPath_ =
  Device'
    { permissions = Prelude.Nothing,
      containerPath = Prelude.Nothing,
      hostPath = pHostPath_
    }

-- | The explicit permissions to provide to the container for the device. By
-- default, the container has permissions for @read@, @write@, and @mknod@
-- for the device.
device_permissions :: Lens.Lens' Device (Prelude.Maybe [DeviceCgroupPermission])
device_permissions = Lens.lens (\Device' {permissions} -> permissions) (\s@Device' {} a -> s {permissions = a} :: Device) Prelude.. Lens.mapping Prelude._Coerce

-- | The path inside the container at which to expose the host device.
device_containerPath :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_containerPath = Lens.lens (\Device' {containerPath} -> containerPath) (\s@Device' {} a -> s {containerPath = a} :: Device)

-- | The path for the device on the host container instance.
device_hostPath :: Lens.Lens' Device Prelude.Text
device_hostPath = Lens.lens (\Device' {hostPath} -> hostPath) (\s@Device' {} a -> s {hostPath = a} :: Device)

instance Prelude.FromJSON Device where
  parseJSON =
    Prelude.withObject
      "Device"
      ( \x ->
          Device'
            Prelude.<$> ( x Prelude..:? "permissions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "containerPath")
            Prelude.<*> (x Prelude..: "hostPath")
      )

instance Prelude.Hashable Device

instance Prelude.NFData Device

instance Prelude.ToJSON Device where
  toJSON Device' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("permissions" Prelude..=) Prelude.<$> permissions,
            ("containerPath" Prelude..=)
              Prelude.<$> containerPath,
            Prelude.Just ("hostPath" Prelude..= hostPath)
          ]
      )

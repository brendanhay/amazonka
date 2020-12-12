{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.Device
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.Device
  ( Device (..),

    -- * Smart constructor
    mkDevice,

    -- * Lenses
    dContainerPath,
    dPermissions,
    dHostPath,
  )
where

import Network.AWS.Batch.Types.DeviceCgroupPermission
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing a container instance host device.
--
-- /See:/ 'mkDevice' smart constructor.
data Device = Device'
  { containerPath :: Lude.Maybe Lude.Text,
    permissions :: Lude.Maybe [DeviceCgroupPermission],
    hostPath :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Device' with the minimum fields required to make a request.
--
-- * 'containerPath' - The path inside the container at which to expose the host device. By default the @hostPath@ value is used.
-- * 'hostPath' - The path for the device on the host container instance.
-- * 'permissions' - The explicit permissions to provide to the container for the device. By default, the container has permissions for @read@ , @write@ , and @mknod@ for the device.
mkDevice ::
  -- | 'hostPath'
  Lude.Text ->
  Device
mkDevice pHostPath_ =
  Device'
    { containerPath = Lude.Nothing,
      permissions = Lude.Nothing,
      hostPath = pHostPath_
    }

-- | The path inside the container at which to expose the host device. By default the @hostPath@ value is used.
--
-- /Note:/ Consider using 'containerPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dContainerPath :: Lens.Lens' Device (Lude.Maybe Lude.Text)
dContainerPath = Lens.lens (containerPath :: Device -> Lude.Maybe Lude.Text) (\s a -> s {containerPath = a} :: Device)
{-# DEPRECATED dContainerPath "Use generic-lens or generic-optics with 'containerPath' instead." #-}

-- | The explicit permissions to provide to the container for the device. By default, the container has permissions for @read@ , @write@ , and @mknod@ for the device.
--
-- /Note:/ Consider using 'permissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPermissions :: Lens.Lens' Device (Lude.Maybe [DeviceCgroupPermission])
dPermissions = Lens.lens (permissions :: Device -> Lude.Maybe [DeviceCgroupPermission]) (\s a -> s {permissions = a} :: Device)
{-# DEPRECATED dPermissions "Use generic-lens or generic-optics with 'permissions' instead." #-}

-- | The path for the device on the host container instance.
--
-- /Note:/ Consider using 'hostPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dHostPath :: Lens.Lens' Device Lude.Text
dHostPath = Lens.lens (hostPath :: Device -> Lude.Text) (\s a -> s {hostPath = a} :: Device)
{-# DEPRECATED dHostPath "Use generic-lens or generic-optics with 'hostPath' instead." #-}

instance Lude.FromJSON Device where
  parseJSON =
    Lude.withObject
      "Device"
      ( \x ->
          Device'
            Lude.<$> (x Lude..:? "containerPath")
            Lude.<*> (x Lude..:? "permissions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "hostPath")
      )

instance Lude.ToJSON Device where
  toJSON Device' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("containerPath" Lude..=) Lude.<$> containerPath,
            ("permissions" Lude..=) Lude.<$> permissions,
            Lude.Just ("hostPath" Lude..= hostPath)
          ]
      )

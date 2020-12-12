{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.KernelCapabilities
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.KernelCapabilities
  ( KernelCapabilities (..),

    -- * Smart constructor
    mkKernelCapabilities,

    -- * Lenses
    kcDrop,
    kcAdd,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Linux capabilities for the container that are added to or dropped from the default configuration provided by Docker. For more information on the default capabilities and the non-default available capabilities, see <https://docs.docker.com/engine/reference/run/#runtime-privilege-and-linux-capabilities Runtime privilege and Linux capabilities> in the /Docker run reference/ . For more detailed information on these Linux capabilities, see the <http://man7.org/linux/man-pages/man7/capabilities.7.html capabilities(7)> Linux manual page.
--
-- /See:/ 'mkKernelCapabilities' smart constructor.
data KernelCapabilities = KernelCapabilities'
  { drop ::
      Lude.Maybe [Lude.Text],
    add :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KernelCapabilities' with the minimum fields required to make a request.
--
-- * 'add' - The Linux capabilities for the container that have been added to the default configuration provided by Docker. This parameter maps to @CapAdd@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--cap-add@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- Valid values: @"ALL" | "AUDIT_CONTROL" | "AUDIT_WRITE" | "BLOCK_SUSPEND" | "CHOWN" | "DAC_OVERRIDE" | "DAC_READ_SEARCH" | "FOWNER" | "FSETID" | "IPC_LOCK" | "IPC_OWNER" | "KILL" | "LEASE" | "LINUX_IMMUTABLE" | "MAC_ADMIN" | "MAC_OVERRIDE" | "MKNOD" | "NET_ADMIN" | "NET_BIND_SERVICE" | "NET_BROADCAST" | "NET_RAW" | "SETFCAP" | "SETGID" | "SETPCAP" | "SETUID" | "SYS_ADMIN" | "SYS_BOOT" | "SYS_CHROOT" | "SYS_MODULE" | "SYS_NICE" | "SYS_PACCT" | "SYS_PTRACE" | "SYS_RAWIO" | "SYS_RESOURCE" | "SYS_TIME" | "SYS_TTY_CONFIG" | "SYSLOG" | "WAKE_ALARM"@
-- * 'drop' - The Linux capabilities for the container that have been removed from the default configuration provided by Docker. This parameter maps to @CapDrop@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--cap-drop@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- Valid values: @"ALL" | "AUDIT_CONTROL" | "AUDIT_WRITE" | "BLOCK_SUSPEND" | "CHOWN" | "DAC_OVERRIDE" | "DAC_READ_SEARCH" | "FOWNER" | "FSETID" | "IPC_LOCK" | "IPC_OWNER" | "KILL" | "LEASE" | "LINUX_IMMUTABLE" | "MAC_ADMIN" | "MAC_OVERRIDE" | "MKNOD" | "NET_ADMIN" | "NET_BIND_SERVICE" | "NET_BROADCAST" | "NET_RAW" | "SETFCAP" | "SETGID" | "SETPCAP" | "SETUID" | "SYS_ADMIN" | "SYS_BOOT" | "SYS_CHROOT" | "SYS_MODULE" | "SYS_NICE" | "SYS_PACCT" | "SYS_PTRACE" | "SYS_RAWIO" | "SYS_RESOURCE" | "SYS_TIME" | "SYS_TTY_CONFIG" | "SYSLOG" | "WAKE_ALARM"@
mkKernelCapabilities ::
  KernelCapabilities
mkKernelCapabilities =
  KernelCapabilities' {drop = Lude.Nothing, add = Lude.Nothing}

-- | The Linux capabilities for the container that have been removed from the default configuration provided by Docker. This parameter maps to @CapDrop@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--cap-drop@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- Valid values: @"ALL" | "AUDIT_CONTROL" | "AUDIT_WRITE" | "BLOCK_SUSPEND" | "CHOWN" | "DAC_OVERRIDE" | "DAC_READ_SEARCH" | "FOWNER" | "FSETID" | "IPC_LOCK" | "IPC_OWNER" | "KILL" | "LEASE" | "LINUX_IMMUTABLE" | "MAC_ADMIN" | "MAC_OVERRIDE" | "MKNOD" | "NET_ADMIN" | "NET_BIND_SERVICE" | "NET_BROADCAST" | "NET_RAW" | "SETFCAP" | "SETGID" | "SETPCAP" | "SETUID" | "SYS_ADMIN" | "SYS_BOOT" | "SYS_CHROOT" | "SYS_MODULE" | "SYS_NICE" | "SYS_PACCT" | "SYS_PTRACE" | "SYS_RAWIO" | "SYS_RESOURCE" | "SYS_TIME" | "SYS_TTY_CONFIG" | "SYSLOG" | "WAKE_ALARM"@
--
-- /Note:/ Consider using 'drop' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kcDrop :: Lens.Lens' KernelCapabilities (Lude.Maybe [Lude.Text])
kcDrop = Lens.lens (drop :: KernelCapabilities -> Lude.Maybe [Lude.Text]) (\s a -> s {drop = a} :: KernelCapabilities)
{-# DEPRECATED kcDrop "Use generic-lens or generic-optics with 'drop' instead." #-}

-- | The Linux capabilities for the container that have been added to the default configuration provided by Docker. This parameter maps to @CapAdd@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--cap-add@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- Valid values: @"ALL" | "AUDIT_CONTROL" | "AUDIT_WRITE" | "BLOCK_SUSPEND" | "CHOWN" | "DAC_OVERRIDE" | "DAC_READ_SEARCH" | "FOWNER" | "FSETID" | "IPC_LOCK" | "IPC_OWNER" | "KILL" | "LEASE" | "LINUX_IMMUTABLE" | "MAC_ADMIN" | "MAC_OVERRIDE" | "MKNOD" | "NET_ADMIN" | "NET_BIND_SERVICE" | "NET_BROADCAST" | "NET_RAW" | "SETFCAP" | "SETGID" | "SETPCAP" | "SETUID" | "SYS_ADMIN" | "SYS_BOOT" | "SYS_CHROOT" | "SYS_MODULE" | "SYS_NICE" | "SYS_PACCT" | "SYS_PTRACE" | "SYS_RAWIO" | "SYS_RESOURCE" | "SYS_TIME" | "SYS_TTY_CONFIG" | "SYSLOG" | "WAKE_ALARM"@
--
-- /Note:/ Consider using 'add' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kcAdd :: Lens.Lens' KernelCapabilities (Lude.Maybe [Lude.Text])
kcAdd = Lens.lens (add :: KernelCapabilities -> Lude.Maybe [Lude.Text]) (\s a -> s {add = a} :: KernelCapabilities)
{-# DEPRECATED kcAdd "Use generic-lens or generic-optics with 'add' instead." #-}

instance Lude.FromJSON KernelCapabilities where
  parseJSON =
    Lude.withObject
      "KernelCapabilities"
      ( \x ->
          KernelCapabilities'
            Lude.<$> (x Lude..:? "drop" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "add" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON KernelCapabilities where
  toJSON KernelCapabilities' {..} =
    Lude.object
      ( Lude.catMaybes
          [("drop" Lude..=) Lude.<$> drop, ("add" Lude..=) Lude.<$> add]
      )
